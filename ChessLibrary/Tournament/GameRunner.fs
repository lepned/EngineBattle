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

/// A pool of up to `capacity` instances created on demand by `spawn` (given the instance
/// index, which is never one a live instance holds). Returned instances are handed out again
/// before any new one is spawned. An agent runs PoolMachine; generic so it is tested with ints.
type LazyPool<'T when 'T: equality>(capacity: int, spawn: int -> Task<'T>) =
    let replies = Dictionary<int, TaskCompletionSource<'T>>()
    // the slot of each instance handed out, for Return and Evict
    let slots = Dictionary<'T, int>(HashIdentity.Structural)
    let mutable taken = 0
    let mutable nextId = 0
    let take id =
        lock replies (fun () ->
            match replies.TryGetValue id with
            | true, tcs -> replies.Remove id |> ignore; Some tcs
            | _ -> None)

    let agent = MailboxProcessor<PoolMachine.Event<'T> * AsyncReplyChannel<'T list> option>.Start(fun inbox ->
        let rec loop state = async {
            let! event, reply = inbox.Receive()
            let state, effects =
                // a dead agent would hang every borrower; drop the event instead, refusing a borrow
                try PoolMachine.step state event
                with ex ->
                    eprintfn "Engine pool: %A not handled: %s" event ex.Message
                    match event with
                    | PoolMachine.Borrow id -> state, [ PoolMachine.Refuse (id, ex) ]
                    | _ -> state, []
            // published before anyone is answered, so a caller sees it settled
            taken <- state.Taken.Count
            let mutable released = []
            for effect in effects do
                match effect with
                | PoolMachine.Spawn slot ->
                    // awaited, not waited for: an engine start holds no thread. Off the agent too:
                    // a spawn runs synchronously up to its first await (constructor, Process.Start)
                    task {
                        let! outcome =
                            Task.Run<PoolMachine.Event<'T>>(fun () ->
                                task {
                                    try
                                        let! item = spawn slot
                                        return PoolMachine.Spawned (slot, item)
                                    with ex -> return PoolMachine.SpawnFailed (slot, ex) })
                        inbox.Post (outcome, None) } |> ignore
                | PoolMachine.Give (id, slot, item) ->
                    lock slots (fun () -> slots.[item] <- slot)
                    take id |> Option.iter (fun tcs -> tcs.TrySetResult item |> ignore)
                | PoolMachine.Refuse (id, ex) ->
                    take id |> Option.iter (fun tcs -> tcs.TrySetException ex |> ignore)
                | PoolMachine.Release items ->
                    lock slots (fun () -> for item in items do slots.Remove item |> ignore)
                    released <- items
            reply |> Option.iter (fun r -> r.Reply released)
            return! loop state }
        loop (PoolMachine.initial capacity))

    let slotOf (item: 'T) =
        let slot =
            lock slots (fun () ->
                match slots.TryGetValue item with
                | true, slot -> Some slot
                | _ -> None)
        // its slot would stay taken, and a borrower of a full pool wait for it for ever
        if slot.IsNone then eprintfn "Engine pool: %A is not one of its instances" item
        slot

    member _.Borrow() : Task<'T> =
        let tcs = TaskCompletionSource<'T>(TaskCreationOptions.RunContinuationsAsynchronously)
        let id = Interlocked.Increment &nextId
        lock replies (fun () -> replies.[id] <- tcs)
        agent.Post (PoolMachine.Borrow id, None)
        tcs.Task

    member _.Return(item: 'T) =
        slotOf item |> Option.iter (fun slot -> agent.PostAndReply(fun r -> PoolMachine.Return (slot, item), Some r) |> ignore)

    /// The borrower is not returning this instance: it has been stopped. Frees its slot so a
    /// later borrow spawns a fresh one, and wakes a borrower waiting on the full pool.
    member _.Evict(item: 'T) =
        slotOf item |> Option.iter (fun slot ->
            lock slots (fun () -> slots.Remove item |> ignore)
            agent.PostAndReply(fun r -> PoolMachine.Evict slot, Some r) |> ignore)

    /// The returned instances, for the caller to stop: their slots are free again, so a later
    /// borrow spawns a fresh one.
    member _.Shed() : Task<'T[]> =
        task {
            let! released = agent.PostAndAsyncReply(fun r -> PoolMachine.Shed, Some r)
            return List.toArray released }

    /// How many instances exist right now (spawned, whether out on loan or returned).
    member _.Spawned = taken

    /// Every instance that was returned, for teardown. Never called while borrows are live.
    member _.Drain() : 'T[] = agent.PostAndReply(fun r -> PoolMachine.Drain, Some r) |> List.toArray

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

/// One board, before a game: the idle instances of every engine the game does not play are
/// released from their pools and stopped (`stop`), so a later game of theirs starts them afresh.
let shedIdleExcept (pools: Map<string, LazyPool<'T>>) (playing: string list) (stop: string -> 'T -> Task) : Task =
    task {
        for KeyValue(name, pool) in pools do
            if not (List.contains name playing) then
                let! idle = pool.Shed()
                for item in idle do
                    do! stop name item }

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
      /// Games played at once: the pools hold this many instances of each engine.
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

    // One engine pool per engine name, capacity = parallelism. Instances are spawned on the
    // first borrow, not up front: a run starts as soon as the first game's two engines are
    // ready instead of after every instance of every name has started and answered readyok -
    // with several Ceres or Lc0 engines that was minutes before the first move. Each engine is
    // registered in allEngines before init, so an init failure still gets it killed by Dispose.
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
        if not initPerGame then do! EngineHelper.initEngineAsync 0 eng
        return eng }
    let enginePools =
        tourny.EngineSetup.Engines
        |> List.map (fun e -> e.Name, LazyPool<ChessEngine>(ctx.Concurrency, spawnEngine e))
        |> Map.ofList

    let forget (eng: ChessEngine) =
        // Stopped for good: the safety net has nothing to do for it, and a long run
        // with many engines would otherwise hold every wrapper it ever spawned.
        lock allEngines (fun () -> allEngines.Remove eng |> ignore)

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

    // An engine is not kept for nothing: keeping every engine of a ten-engine round robin
    // alive - ten networks on one GPU, times the number of boards - is not what anyone signed
    // up for. One board: before a game, the idle engines it does not play are stopped. Several:
    // after a game, an engine none of the next games needs (NeededSoon) is stopped - a
    // forecast, so a respawn now and then, never a hang: an eviction wakes a borrower waiting
    // on the full pool. Two engines play every game and are always kept.
    let keepAllEngines = tourny.EngineSetup.Engines.Length <= 2
    let stopIdleExcept (pair: Pairing) : Task = task {
        if ctx.Concurrency = 1 && not keepAllEngines then
            do! shedIdleExcept enginePools [ pair.White.Name; pair.Black.Name ] (fun _ eng ->
                task {
                    do! stopEngine eng
                    forget eng }) }
    let settleEngine (name: string) (eng: ChessEngine) : Task = task {
        if ctx.Concurrency = 1 || keepAllEngines || ctx.NeededSoon name then enginePools.[name].Return eng
        else
            enginePools.[name].Evict eng
            do! stopEngine eng
            forget eng }

    // Pools spawn on borrow, so a borrow can fail - a binary that is missing or dies at
    // start - with the pairing's other engine already out. Give that one back (with one
    // board a leaked engine hangs every later game it is in) and stop the run: an engine
    // that cannot start ended the run before the pools were lazy, and a run that quietly
    // plays on without it is worse. The logger is not visible in the console, so stdout.
    let borrowEngine (name: string) (giveBack: unit -> unit) = task {
        try return! enginePools.[name].Borrow()
        with ex ->
            giveBack ()
            ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red
                (sprintf "Engine %s failed to start: %s - stopping the tournament" name ex.Message)
            callback (Update.EngineStartFailed(name, ex.Message))
            ctx.Cts.Cancel()
            System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
            return Unchecked.defaultof<ChessEngine> }

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
        do! stopIdleExcept pair
        // Borrow engines in sorted name order to prevent ABBA deadlock.
        // With openingsTwice, consecutive pairings swap colors (A-white/B-black then B-white/A-black).
        // Two workers borrowing in white-then-black order can deadlock when they cross.
        let firstName, secondName =
            if String.Compare(pair.White.Name, pair.Black.Name, StringComparison.Ordinal) <= 0
            then pair.White.Name, pair.Black.Name
            else pair.Black.Name, pair.White.Name
        let! firstEng = borrowEngine firstName ignore
        let! secondEng = borrowEngine secondName (fun () -> enginePools.[firstName].Return firstEng)
        let wEng, bEng =
            if firstName = pair.White.Name then firstEng, secondEng
            else secondEng, firstEng
        // the settles are awaited, which a finally cannot do: the game's exception waits for them
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
        do! settleEngine pair.White.Name wEng
        do! settleEngine pair.Black.Name bEng
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

    /// Stops every engine at once, at the end of a run (no borrows live).
    member _.Shutdown() : Task = task {
        let drained = [| for KeyValue(e, pool) in enginePools -> e, pool.Drain() |]
        do! Task.WhenAll [| for _, engines in drained do for eng in engines -> stopEngine eng |]
        for e, _ in drained do printfn $"Engine {e} stopped" }

    interface IDisposable with
        /// Safety net: stops any engine process still running (a no-op after Shutdown). A
        /// Dispose cannot await; this is the end of the run, and they are stopped at once.
        member _.Dispose() = stopAll ()
