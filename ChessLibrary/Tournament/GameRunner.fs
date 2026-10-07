/// Plays a run's games on pooled engines, whatever the mode: borrows the pairing's two engines,
/// sets up the board, seeds the replay, plays, has the run's record take the game and gives the
/// engines back. ParallelExecution plays several games at once on it; one board is the case the
/// cup, swiss and ladder runners have.
module ChessLibrary.GameRunner

open System
open System.Threading
open System.Threading.Tasks
open System.Text
open System.Collections.Generic
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.Chess
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GamePersistence

let assignDeviceToConfig (config: EngineConfig) (gpu: int) =
    if String.IsNullOrEmpty config.DeviceOption || String.IsNullOrEmpty config.DeviceTemplate then
        config
    else
        let newOptions = Dictionary<string, obj>(config.Options, StringComparer.OrdinalIgnoreCase)
        let parts = config.DeviceTemplate.Split([|"{0}"|], StringSplitOptions.None)
        let pattern = String.Join(@"\d+", parts |> Array.map Regex.Escape)
        let value =
            match newOptions.TryGetValue(config.DeviceOption) with
            | true, existing ->
                let existingStr = string existing
                let mutable offset = 0
                Regex.Replace(existingStr, pattern, fun _ ->
                    let result = config.DeviceTemplate.Replace("{0}", string (gpu + offset))
                    offset <- offset + 1
                    result)
            | false, _ -> config.DeviceTemplate.Replace("{0}", string gpu)
        newOptions.[config.DeviceOption] <- box value
        { config with Options = newOptions }

/// Quit politely, then make sure the process is gone. Never throws: teardown must go on.
let stopEngine (eng: ChessEngine) : Task =
    task {
        try eng.Quit() with _ -> ()
        try do! eng.StopProcessAsync() with _ -> ()
    }

/// A game's engines could not be had: one failed to start (`Engine` names it), or the run's
/// engines were shut down (`Engine` is empty).
type EngineRefusedException(engine: string, cause: exn) =
    inherit Exception((if engine = "" then cause.Message else sprintf "Engine %s: %s" engine cause.Message), cause)
    member _.Engine = engine

/// Runs EngineMachine: the one owner of a run's engine instances. `start name slot` starts an
/// instance, `stop` stops one. A game requests its pair, is granted both at once, and releases
/// them when it ends. Generic so it is tested with strings.
type EngineAgent<'T>(policy: EngineMachine.Policy, start: string -> int -> Task<'T>, stop: 'T -> Task) =
    let grants = Dictionary<int, TaskCompletionSource<'T * 'T>>()
    let drained = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
    // the state after the last step, readable from outside: a wedged agent cannot answer for itself
    [<VolatileField>]
    let mutable published = EngineMachine.initial policy
    let take game =
        lock grants (fun () ->
            match grants.TryGetValue game with
            | true, tcs -> grants.Remove game |> ignore; Some tcs
            | _ -> None)

    let agent = MailboxProcessor<EngineMachine.Event<'T>>.Start(fun inbox ->
        let rec loop state = async {
            let! event = inbox.Receive()
            let state, effects =
                // a dead agent would hang every game: a step that throws refuses a request and
                // stops an engine that arrived, so neither a game nor a process is left behind
                try EngineMachine.step state event
                with ex ->
                    eprintfn "Engines: %A not handled: %s" event ex.Message
                    match event with
                    | EngineMachine.Request r -> state, [ EngineMachine.Refuse (r.Game, "", ex) ]
                    | EngineMachine.Started (id, item) -> state, [ EngineMachine.Stop (id, item) ]
                    | _ -> state, []
            published <- state
            for effect in effects do
                match effect with
                | EngineMachine.Start (id, name, slot) ->
                    // off the agent, and awaited rather than waited for: an engine start holds no thread
                    task {
                        let! outcome =
                            Task.Run<EngineMachine.Event<'T>>(Func<Task<EngineMachine.Event<'T>>>(fun () ->
                                task {
                                    try
                                        let! item = start name slot
                                        return EngineMachine.Started (id, item)
                                    with ex -> return EngineMachine.StartFailed (id, ex) }))
                        inbox.Post outcome } |> ignore
                | EngineMachine.Stop (id, item) ->
                    // Stopped always comes back: the capacity it frees may be what a waiting game needs
                    task {
                        try
                            try do! Task.Run(Func<Task>(fun () -> stop item))
                            with _ -> ()
                        finally inbox.Post (EngineMachine.Stopped id) } |> ignore
                | EngineMachine.Grant (game, white, black) ->
                    take game |> Option.iter (fun tcs -> tcs.TrySetResult((white, black)) |> ignore)
                | EngineMachine.Refuse (game, name, ex) ->
                    take game |> Option.iter (fun tcs -> tcs.TrySetException(EngineRefusedException(name, ex)) |> ignore)
                | EngineMachine.Drained -> drained.TrySetResult() |> ignore
            return! loop state }
        loop published)

    /// A game's two engines, granted together.
    member _.Request(game: int, white: string, black: string) : Task<'T * 'T> =
        let tcs = TaskCompletionSource<'T * 'T>(TaskCreationOptions.RunContinuationsAsynchronously)
        lock grants (fun () -> grants.[game] <- tcs)
        agent.Post (EngineMachine.Request { Game = game; White = white; Black = black })
        tcs.Task

    /// The game ended. `keep`: the engines needed soon (the others are stopped); None keeps all.
    member _.Release(game: int, keep: Set<string> option) = agent.Post (EngineMachine.Release (game, keep))

    /// Refuses whatever still waits and stops every engine; done when the last has stopped.
    member _.Shutdown() : Task =
        agent.Post EngineMachine.Shutdown
        drained.Task

    /// The engines as of the last step.
    member _.State = published

/// The board at the end of the opening: the FEN (or the start position), then the book moves
/// up to OpeningsPly. EPD books carry no moves. Sets the tournament's FRC flag from a FEN opening.
let boardAfterOpening (logger: ILogger) (tourny: Tournament) (epdBook: bool) (pair: Pairing) =
    let board = Board()
    let start = if String.IsNullOrEmpty pair.Opening.Fen then Chess.startPos else pair.Opening.Fen
    board.LoadFen start
    board.StartPosition <- start
    if not (String.IsNullOrEmpty pair.Opening.Fen) then tourny.IsChess960 <- board.IsFRC
    let openingMoves = pair.Opening.Mainline |> Seq.truncate tourny.Opening.OpeningsPly |> Seq.toArray
    if not epdBook then
        for m in openingMoves do board.PlayOpeningMove m.San
    if tourny.VerboseLogging then
        let line =
            openingMoves
            |> Seq.map (fun m -> if m.Color = "w" then sprintf "%d. %s" m.MoveNumber m.San else m.San)
            |> String.concat " "
        logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, line)
        logger.LogDebug("{position}", sprintf "position fen %s moves %s" board.StartPosition (String.concat " " board.UciMovesPlayed))
    board

/// What the games of a run share.
type GameContext =
    { Logger: ILogger
      Tourny: Tournament
      Callback: Update -> unit
      /// The live feed and the multi-board grid: updates stamped with the board ("" for the
      /// run's own); a no-op when nothing listens.
      Feed: string -> Update -> unit
      /// Whether anything listens on Feed: game updates are stamped only then.
      FeedAny: bool
      Cts: CancellationTokenSource
      /// The user's adjudication of the running game.
      Adjudicate: unit -> UserAdjudication option
      Record: RecordAgent.RecordAgent
      /// Games played at once: up to this many instances of each engine.
      Concurrency: int
      /// An EPD book carries no moves to play out.
      EpdBook: bool
      /// With several boards, after a game: whether one of the next games plays this engine;
      /// one that none does is stopped. One board needs no forecast: an engine stays until a
      /// game starts that does not play it.
      NeededSoon: string -> bool }

/// The engines of a run and the games played on them. Shutdown stops the engines at the end;
/// Dispose is the safety net for a run that ends without it.
type GameRunner(ctx: GameContext) =
    let tourny, logger, callback = ctx.Tourny, ctx.Logger, ctx.Callback
    let gpus = tourny.TestOptions.GPUs
    // Full per-game engine initialisation (MoveOverheadMs, restart checks, the GUI's opening
    // delay) is what the sequential runner did for the GUI; only the GUI with one board gets
    // it. Pooled engines in the console and in multi-board runs skip it but still get
    // ucinewgame + readyok before every game, which is cheap - see GameLoop.prepareEngines.
    let initPerGame = not tourny.ConsoleOnly && ctx.Concurrency = 1
    // Track all spawned engines for cleanup safety net
    let allEngines = ResizeArray<ChessEngine>()

    // Up to `parallelism` instances of each engine, started when a game first needs them, not up
    // front: a run starts as soon as the first game's two engines are ready instead of after
    // every instance of every engine has started and answered readyok - with several Ceres or
    // Lc0 engines that was minutes before the first move. Each engine is registered in
    // allEngines before init, so an init failure still gets it killed by Dispose.
    let spawnEngine (e: EngineConfig) (i: int) = task {
        let cfg =
            if gpus <> null && gpus.Length > 0 then
                let gpu = gpus.[i % gpus.Length]
                logger.LogInformation($"Engine pool {e.Name} instance {i}: assigning GPU {gpu}")
                assignDeviceToConfig e gpu
            else e
        let! eng = EngineHelper.createEngineAsync (cfg, Some logger)
        lock allEngines (fun () -> allEngines.Add(eng))
        callback (Update.EngineStarted(e.Name, eng.GetDefaultOptions() |> Seq.map (fun kv -> kv.Key, string kv.Value) |> Map.ofSeq))
        // Pooled engines that skip per-game init must be initialised here. When the game
        // initialises its own engines (GUI, one board) it must NOT happen here: the game
        // sends StartOfGame before it initialises, so the board shows the pairing while
        // Ceres or Lc0 spend their seconds on readyok - initialising at spawn moved that
        // wait in front of the first thing the page could show.
        // An engine whose init fails is stopped here: the machine forgets a failed start, so
        // nothing else would stop its process before the run ends.
        if not initPerGame then
            try do! EngineHelper.initEngineAsync 0 eng
            with ex ->
                do! stopEngine eng
                lock allEngines (fun () -> allEngines.Remove eng |> ignore)
                System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
        return eng }
    let configs = tourny.EngineSetup.Engines |> List.map (fun e -> e.Name, e) |> Map.ofList

    let forget (eng: ChessEngine) =
        // Stopped for good: the safety net has nothing to do for it, and a long run
        // with many engines would otherwise hold every wrapper it ever spawned.
        lock allEngines (fun () -> allEngines.Remove eng |> ignore)

    // An engine is not kept for nothing: keeping every engine of a ten-engine round robin
    // alive - ten networks on one GPU, times the number of boards - is not what anyone signed
    // up for. One board: before a game, the idle engines it does not play are stopped. Several:
    // after a game, an engine none of the next games needs (NeededSoon) is stopped - a
    // forecast, so a respawn now and then, never a hang. Two engines play every game and are
    // always kept.
    let keepAllEngines = tourny.EngineSetup.Engines.Length <= 2
    let engines =
        EngineAgent<ChessEngine>(
            // one board: the engine a game does not play is gone before the next one starts (one
            // network on the GPU at a time, as before); several boards start without waiting
            { Capacity = ctx.Concurrency; OneBoard = ctx.Concurrency = 1 && not keepAllEngines; StopBeforeStart = ctx.Concurrency = 1 },
            (fun name slot -> spawnEngine configs.[name] slot),
            (fun eng ->
                task {
                    do! stopEngine eng
                    forget eng }))
    let mutable nextGame = 0
    // two boards refused by the same engine report it once
    let mutable startFailureReported = 0

    // Helper function to check the status of each engine and restart if necessary
    let engineHealthy (engine:ChessEngine) = task {
        try
            // Check if engine has exited and try to restart
            if engine.HasExited() then
                logger.LogWarning($"Engine {engine.Name} has exited, attempting restart")
                try
                    // The same init as the pool's first start, warm-up included: pooled
                    // engines skip per-game init, so this is the only place the new
                    // process can load its network before a clock runs. Throws on failure.
                    do! EngineHelper.initEngineAsync 0 engine
                    logger.LogCritical($"Successfully restarted engine {engine.Name}")
                    return true
                with
                | ex ->
                    logger.LogCritical(ex, $"Exception restarting engine {engine.Name}")
                    return false
            else
                return true
        with
        | ex ->
            logger.LogCritical(ex, $"Failed to get engine {engine.Name} restarted")
            return false
    }

    // After a game: one board keeps its engines (the next request stops what it does not
    // play); several keep only the engines one of the next games needs.
    let keepAfterGame () =
        if ctx.Concurrency = 1 || keepAllEngines then None
        else Some (configs.Keys |> Seq.filter ctx.NeededSoon |> Set.ofSeq)

    // Engines start on demand, so a request can fail - a binary that is missing or dies at
    // start. That stops the run: an engine that cannot start ended the run before engines
    // started lazily, and a run that quietly plays on without it is worse. The pairing's other
    // engine is already free again. The logger is not visible in the console, so stdout.
    let requestEngines (game: int) (pair: Pairing) = task {
        try return! engines.Request(game, pair.White.Name, pair.Black.Name)
        with :? EngineRefusedException as ex when ex.Engine <> "" ->
            if Interlocked.Exchange(&startFailureReported, 1) = 0 then
                let cause = if isNull ex.InnerException then ex.Message else ex.InnerException.Message
                ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red
                    (sprintf "Engine %s failed to start: %s - stopping the tournament" ex.Engine cause)
                callback (Update.EngineStartFailed(ex.Engine, cause))
            ctx.Cts.Cancel()
            System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
            return Unchecked.defaultof<ChessEngine * ChessEngine> }

    // what the engine-failure log keeps of a game that ended in an exception
    let incident (white: ChessEngine) (black: ChessEngine) (pair: Pairing) (board: Board) : ChessLibrary.CustomException.CatchContext =
        { EngineName = white.Name
          OpponentName = black.Name
          GameNumber = pair.GameNr
          MoveNumber = board.MoveNumber()
          TimeControl = $"[{tourny.TimeControlTextForPlayer white.Config.TimeControlID}; {tourny.TimeControlTextForPlayer black.Config.TimeControlID}]"
          TimeRemaining = None
          PositionFen = board.FEN()
          LastCommand = None
          TimestampUtc = DateTime.UtcNow
          MoveHistory = board.GetMoveHistory() }

    let stopAll () =
        let running = lock allEngines (fun () -> allEngines |> Seq.filter (fun e -> try not (e.HasExited()) with _ -> false) |> Seq.toArray)
        if running.Length > 0 then
            try Task.WhenAll(running |> Array.map stopEngine).Wait() with _ -> ()

    /// Plays one pairing and has the record take it. `slot` is the board: the live-feed
    /// gameId, so the grid shows a fixed set of boards, reused as games finish and new ones
    /// start. Throws when the game could not be played at all (an engine that will not start
    /// or restart, a replay or record that fails); a game that ends in an engine failure is a
    /// result, recorded like any other.
    member _.Play(slot: int, pair: Pairing) : Task<Result * RecordAgent.Recorded> = task {
        // both engines at once: no borrow order to keep, no engine to give back
        let game = Interlocked.Increment &nextGame
        let! wEng, bEng = requestEngines game pair
        // the engines are released before the game's exception goes on
        let mutable failure : System.Runtime.ExceptionServices.ExceptionDispatchInfo = null
        let mutable outcome = Unchecked.defaultof<Result * RecordAgent.Recorded>
        try
            let! wOk = wEng |> engineHealthy
            let! bOk = bEng |> engineHealthy
            if wOk |> not || bOk |> not then
                logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                Exception("Unhealthy engine detected, potentially skipping game") |> raise
            let! res =
                async {
                    let currentBoard = boardAfterOpening logger tourny ctx.EpdBook pair
                    let sb = StringBuilder()
                    Update.RoundNr pair.RoundNr |> callback
                    ctx.Feed "" (Update.RoundNr pair.RoundNr)

                    // with prevention on, the game plays with copies the record seeds
                    let! replay = ctx.Record.Seed pair

                    // Per-game callback: stamp this game's events with its board for the live feed.
                    let gameCallback =
                        if ctx.FeedAny then
                            let gid = string slot
                            fun (u: Update) -> ctx.Feed gid u; callback u
                        else callback

                    // The game itself. Pooled engines skip per-game init (see initPerGame);
                    // with prevention on, each side is held to the moves in its replay copy.
                    let! result =
                        let gametimer = Stopwatch.GetTimestamp()
                        async {
                            try
                                return! Game.GameLoop.play (not initPerGame) replay sb ctx.Cts logger tourny currentBoard wEng bEng pair ctx.Adjudicate gameCallback
                            with
                            | ex ->
                                try ChessLibrary.CustomException.EngineFailures.log logger ex (incident wEng bEng pair currentBoard)
                                with logEx -> logger.LogError(logEx, "Engine failure in {Round} not logged", pair.RoundNr)
                                let! result = handleGameExceptionAsync logger ex ctx.Cts gametimer currentBoard wEng bEng pair
                                return { result with GameDeviations = Game.GameLoop.deviationsOf ex } }
                    let gameLabel = if tourny.TotalGames > 0 then sprintf "Game %d/%d: " pair.GameNr tourny.TotalGames else ""
                    logger.LogInformation("{GameLabel}{GameResult}", gameLabel, result.ToString())

                    // recorded, merged for replay and written to the PGN by the record, in the order games end
                    let! recorded =
                        ctx.Record.Finish
                            { Pairing = pair; Result = result; Plies = currentBoard.UciMovesPlayed.Count
                              Moves = ResizeArray(currentBoard.UciMovesPlayed); Movetext = sb.ToString()
                              Replay = replay; Cancelled = ctx.Cts.IsCancellationRequested }
                    // recorded: a listener that fails must not make it a game to play again
                    if recorded.Played then
                        try
                            callback (Update.GameFinished
                                { GameNr = pair.GameNr; RoundNr = pair.RoundNr; White = pair.White.Name; Black = pair.Black.Name
                                  OpeningHash = pair.OpeningHash; Result = result })
                        with ex -> logger.LogError(ex, "GameFinished for {Round} not handled", pair.RoundNr)
                    match recorded.Metadata with
                    | Some gameData when tourny.VerboseLogging -> logger.LogInformation(gameMetadataSummary gameData)
                    | _ -> ()
                    return result, recorded
                } |> Async.StartAsTask
            outcome <- res
        with ex -> failure <- System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture ex
        engines.Release(game, keepAfterGame ())
        if not (isNull failure) then failure.Throw()
        // the standings as they are now, not as they were when this game ended: a slower
        // game can never send an older table after a newer one
        let _, recorded = outcome
        if recorded.PeriodicDue then
            try
                let! results = ctx.Record.Results() |> Async.StartAsTask
                callback (Update.PeriodicResults (ResizeArray<Result>(results)))
            with ex -> logger.LogError(ex, "Standings after game {Round} not sent", pair.RoundNr)
        return outcome }

    /// Stops every engine, at the end of a run.
    member _.Shutdown() : Task = task {
        do! engines.Shutdown()
        for e in configs.Keys do printfn $"Engine {e} stopped" }

    interface IDisposable with
        /// Safety net: stops any engine process still running (a no-op after Shutdown). A
        /// Dispose cannot await; this is the end of the run, and they are stopped at once.
        member _.Dispose() = stopAll ()
