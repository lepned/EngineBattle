module ChessLibrary.ParallelExecution

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Text
open System.Threading.Channels
open System.Collections.Generic
open System.Diagnostics
open Microsoft.Extensions.Logging
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.Chess
open ChessLibrary.ChessUtilities
open ChessLibrary.TournamentPairing
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.GameExecution
open ChessLibrary.GamePersistence
open ChessLibrary.TournamentRunners.TournamentUtils
open System.Text.RegularExpressions

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
let private stopEngine (eng: ChessEngine) =
    try eng.Quit() with _ -> ()
    try eng.StopProcess() with _ -> ()

/// A crashed or aborted game. It carries "1/2-1/2" purely as a placeholder (NotStarted is
/// Result.Empty from a cancellation race), so it must neither be scored, written to the PGN
/// nor seed replay state - a written game counts in standings/SPRT and makes Scheduler.Diff
/// treat the pair as played on resume.
let private notPlayed (r: Result) =
    r.Reason = MiscTypes.ResultReason.Cancel || r.Reason = MiscTypes.ResultReason.NotStarted

/// A pool of up to `capacity` instances created on demand by `spawn` (given the instance
/// index). Returned instances are handed out again before any new one is spawned. Generic so
/// the slot accounting can be tested without a process behind it; the runner uses it with
/// ChessEngine.
type LazyPool<'T>(capacity: int, spawn: int -> 'T) =
    let available = Channel.CreateUnbounded<'T>()
    let mutable spawned = 0
    let sync = obj()
    // Replaced whenever a slot is freed. A borrower captures it BEFORE it looks at the pool,
    // so a slot freed between its decision to wait and the wait itself still wakes it - the
    // same no-gap pattern as ReplayGate.released.
    let mutable slotFreed = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

    let freeSlot () =
        Interlocked.Decrement(&spawned) |> ignore
        let toWake =
            lock sync (fun () ->
                let t = slotFreed
                slotFreed <- TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
                t)
        toWake.TrySetResult() |> ignore

    // A failed spawn must not keep its slot, or at capacity 1 every later borrow of this
    // pool waits forever for a return that never comes. Plain try/with: inside the task
    // builder a handler cannot re-raise.
    let spawnSlot (slot: int) =
        try spawn (slot - 1)
        with _ ->
            freeSlot ()
            reraise ()

    /// A returned instance if there is one, else a fresh one while the pool is under capacity,
    /// else the next instance to be returned - or the next slot to be freed by an eviction,
    /// after which a fresh one is spawned into it.
    member this.Borrow() : Task<'T> = task {
        let freed = lock sync (fun () -> slotFreed.Task)
        match available.Reader.TryRead() with
        | true, item -> return item
        | _ ->
            // Claim a slot first; if that overshoots, give it back and wait.
            let slot = Interlocked.Increment(&spawned)
            if slot <= capacity then
                return spawnSlot slot
            else
                Interlocked.Decrement(&spawned) |> ignore
                let! _ = Task.WhenAny(available.Reader.WaitToReadAsync().AsTask(), freed)
                // Drain closes the channel; a borrow after that would otherwise spin here.
                if available.Reader.Completion.IsCompleted then invalidOp "LazyPool: borrow after Drain"
                return! this.Borrow()
    }

    member _.Return(item: 'T) = available.Writer.TryWrite item |> ignore

    /// The borrower is not returning this instance: it has been stopped. Frees its slot so a
    /// later borrow spawns a fresh one, and wakes a borrower waiting on the full pool.
    member _.Evict(_item: 'T) = freeSlot ()

    /// How many instances exist right now (spawned, whether out on loan or returned).
    member _.Spawned = spawned

    /// Every instance that was returned, for teardown. Never called while borrows are live.
    member _.Drain() : 'T[] =
        available.Writer.Complete()
        [| let mutable go = true
           while go do
               match available.Reader.TryRead() with
               | true, item -> yield item
               | _ -> go <- false |]

/// What a run plays, worked out once before any engine starts: the book sized for the mode,
/// the plan, and the plan diffed against the games already in the output PGN (a resume plays
/// only what is missing). Sets TotalGames and CurrentGameNr on the tournament as before.
type private Schedule =
    { /// The whole plan with its round labels, for the total the page shows.
      AllPairings: Pairing list
      GamesLeftToPlay: Pairing list
      GamesAlreadyPlayed: PgnGame[]
      /// PreventMoveDeviation's reference games, if a ReferencePGNPath is set.
      ReferenceGames: PgnGame[]
      /// An EPD book carries no moves to play out.
      EpdBook: bool
      StartInfo: StartOfTournamentInfo }

let private buildSchedule (logger: ILogger) (tourny: Tournament) : Schedule =
    let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
    let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    let isGauntlet = tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)

    // The new Scheduler expects the book to already be sized for the chosen
    // distribution. For Gauntlet + Spread (= RandomOpenings=true) with more
    // than one opponent, that means `rounds × numOpp` distinct openings; for
    // Gauntlet + Shared and all non-Gauntlet modes, just `rounds` openings.
    let numOpps = rest.Length
    let gauntletDistribution =
        if tourny.Opening.RandomOpenings then Scheduler.Spread else Scheduler.Shared
    let effectiveBookSize =
        if isGauntlet && gauntletDistribution = Scheduler.Spread && numOpps > 1 then
            tourny.Rounds * numOpps
        else
            tourny.Rounds

    let mutable epdBook = false
    let games =
        match tourny.Opening.OpeningsPath with
        |Some path ->
        if File.Exists path |> not then
            if tourny.VerboseLogging then
                logger.LogError($"Opening file {path} does not exist")
            [| for i = 1 to effectiveBookSize do yield PGNTypes.PgnGame.Empty i |]
        elif path.ToLower().Contains ".epd" then
            epdBook <- true
            let all = EPDExtractor.parseEPDFile path |> Seq.truncate effectiveBookSize |> Seq.toArray
            all
        else
            let all = ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.truncate effectiveBookSize |> Seq.toArray
            if tourny.VerboseLogging then
                logger.LogInformation $"Total number of openings in PGN = {all.Length}"
            all
        |_ ->
            [| for i = 1 to effectiveBookSize do yield PGNTypes.PgnGame.Empty i |]

    if isGauntlet && gauntletDistribution = Scheduler.Spread && numOpps > 1 && games.Length < effectiveBookSize then
        ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow
            (sprintf "Warning: Gauntlet with %d opponents and %d rounds needs %d openings to avoid wrap, but the book only has %d. Some openings will be reused and engines may play them more than expected."
                numOpps tourny.Rounds effectiveBookSize games.Length)

    let gamesAlreadyPlayed =
        let fileExists = File.Exists tourny.PgnOutPath
        if fileExists then
            // Shared with the sequential runners on purpose: this used to be a second copy that
            // overwrote the stored opening hash, which undoes the two-key matching in Diff.diff.
            GameHelpers.loadGamesAlreadyPlayed tourny.PgnOutPath
        else
            [||]

    let referencGamesPlayed =
        let fileExists = File.Exists tourny.ReferencePGNPath
        if fileExists then
            ChessLibrary.FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
        else
            [||]
    let gamesToPlay =
        let openings = games |> Seq.truncate effectiveBookSize |> Seq.toList
        if tourny.Opening.RandomOpenings then PairingHelper.shuffleOpeningsForTournament tourny.Opening openings
        else openings

    // Build one ScheduleConfig and reuse it for both total-plan and diff.
    let scheduleCfg : Scheduler.ScheduleConfig =
        if isGauntlet then
            { Mode = Scheduler.Gauntlet
              Challengers = challengers
              Opponents = rest
              Openings = gamesToPlay
              Rounds = tourny.Rounds
              OpeningsTwice = tourny.Opening.OpeningsTwice
              PreventDeviation = tourny.PreventMoveDeviation
              Distribution = gauntletDistribution }
        else
            { Mode = Scheduler.RoundRobin
              Challengers = tourny.EngineSetup.Engines
              Opponents = []
              Openings = gamesToPlay
              Rounds = tourny.Rounds
              OpeningsTwice = tourny.Opening.OpeningsTwice
              PreventDeviation = tourny.PreventMoveDeviation
              Distribution = Scheduler.Shared }
    let plan =
        if isGauntlet
        then ChessLibrary.Scheduler.Gauntlet.generate scheduleCfg
        else ChessLibrary.Scheduler.RoundRobin.generate scheduleCfg
    let gamesPerPair = if tourny.Opening.OpeningsTwice then 2 else 1
    let priorGames = gamesAlreadyPlayed.Length
    let allPairings =
        plan
        |> ChessLibrary.Scheduler.Diff.applyPairLabels 0 gamesPerPair
        |> ChessLibrary.Scheduler.Diff.toPairings
    let gamesLeftToPlay =
        let afterDiff = ChessLibrary.Scheduler.Diff.diff plan gamesAlreadyPlayed
        let afterLimits =
            if isGauntlet
            then ChessLibrary.Scheduler.Diff.enforceGameLimits challengers rest tourny.Rounds tourny.Opening.OpeningsTwice gamesAlreadyPlayed afterDiff
            else afterDiff
        afterLimits
        |> ChessLibrary.Scheduler.Diff.applyPairLabels priorGames gamesPerPair
        |> ChessLibrary.Scheduler.Diff.toPairings

    if tourny.VerboseLogging then
        PairingHelper.logOpeningPairs logger gamesLeftToPlay

    let totalGames = allPairings.Length
    tourny.TotalGames <- totalGames
    let numberOfGamesPlayed = gamesAlreadyPlayed.Length
    tourny.CurrentGameNr <- numberOfGamesPlayed

    let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
    let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
    { AllPairings = allPairings
      GamesLeftToPlay = gamesLeftToPlay
      GamesAlreadyPlayed = gamesAlreadyPlayed
      ReferenceGames = referencGamesPlayed
      EpdBook = epdBook
      StartInfo = startInfo }

/// Every outlet for gameId-stamped updates: the optional file recorder and HTTP sink from
/// tournament.json's LiveFeed section (an EB_LIVEFEED_* env var overrides the matching field;
/// nothing configured means no feed, the normal case), and the in-process sink the WebGUI
/// multi-board grid listens on. `Any` says whether per-game events need stamping at all.
type private LiveFeed =
    { Any: bool
      Emit: string -> Update -> unit
      Dispose: unit -> unit }

let private openLiveFeed (logger: ILogger) (tourny: Tournament) (taggedSink: (string -> Update -> unit) option) : LiveFeed =
    let liveFeedCfg = if obj.ReferenceEquals(box tourny.LiveFeed, null) then LiveFeedConfig.Empty else tourny.LiveFeed
    let pickFeed (envName: string) (cfgVal: string) =
        match Environment.GetEnvironmentVariable envName with
        | null | "" -> (if isNull cfgVal then "" else cfgVal)
        | v -> v
    let feedFile   = pickFeed "EB_LIVEFEED_FILE"   liveFeedCfg.File
    let feedUrl    = pickFeed "EB_LIVEFEED_URL"    liveFeedCfg.Url
    let feedSource = pickFeed "EB_LIVEFEED_SOURCE" liveFeedCfg.Source
    let feedToken  = pickFeed "EB_LIVEFEED_TOKEN"  liveFeedCfg.Token
    let liveFeedRecorder : LiveFeedRecorder option =
        if String.IsNullOrEmpty feedFile then None
        else
            try
                logger.LogInformation("Live feed recording to {path}", feedFile)
                Some (new LiveFeedRecorder(feedFile))
            with ex ->
                logger.LogError("Failed to open live feed file {path}: {msg}", feedFile, ex.Message)
                None
    let liveFeedHttpSink : LiveFeedHttpSink option =
        if String.IsNullOrEmpty feedUrl then None
        else
            try
                logger.LogInformation("Live feed posting to {url}", feedUrl)
                Some (new LiveFeedHttpSink(feedUrl, feedSource, feedToken))
            with ex ->
                logger.LogError("Failed to init live feed URL sink {url}: {msg}", feedUrl, ex.Message)
                None
    { Any = liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome || taggedSink.IsSome
      Emit =
        fun gid u ->
            // Serialise once, fan out to file and/or HTTP; the in-process sink gets the Update itself.
            if liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome then
                let line = LiveFeedWire.withGameId gid (LiveFeedWire.serializeUpdate u)
                liveFeedRecorder |> Option.iter (fun r -> r.RecordLine line)
                liveFeedHttpSink |> Option.iter (fun s -> s.Send line)
            taggedSink |> Option.iter (fun s -> s gid u)
      Dispose =
        fun () ->
            liveFeedRecorder |> Option.iter (fun r -> r.Dispose())
            liveFeedHttpSink |> Option.iter (fun s -> s.Dispose()) }

let parallelTournamentRun
  (logger: ILogger)
  (tourny: Tournament)
  (callback: Update -> unit)
  (taggedSink: (string -> Update -> unit) option)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  (cts: CancellationTokenSource)
  (externalPgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) =
  // Ladder, Cup, and Swiss manage their own pairings — dispatch directly
  let mode = if String.IsNullOrWhiteSpace tourny.TournamentMode then "" else tourny.TournamentMode.Trim().ToLowerInvariant()
  ChessLibrary.Engine.resetPrintedEngines()
  match mode with
  | "ladder" ->
      TournamentRunners.ladder logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | "cup" ->
      let seeding =
        match tourny.CupOptions.SeedingStrategy with
        | null -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
        | s when s.Equals("random", StringComparison.OrdinalIgnoreCase) -> TournamentPairing.PairingHelper.CupSeedingStrategy.Random
        | _ -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
      TournamentRunners.cup seeding tourny.CupOptions.UniquePerMatchOnly false logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | "swiss" ->
      TournamentRunners.swiss logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | _ ->
  async {

      logger.LogInformation("Tournament in parallel run about to start")

      let { AllPairings = allPairings; GamesLeftToPlay = gamesLeftToPlay
            GamesAlreadyPlayed = gamesAlreadyPlayed; ReferenceGames = referenceGames
            EpdBook = epdBook; StartInfo = startInfo } = buildSchedule logger tourny
      let feed = openLiveFeed logger tourny taggedSink
      // The pairing table and the total the page shows come from these two; the sequential
      // runner sent them and this path never did, so the page sat empty until the first game.
      callback (Update.TotalNumberOfPairs allPairings.Length)
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      callback (Update.StartOfTournament startInfo)
      feed.Emit "" (Update.StartOfTournament startInfo)

      // Verbose-only diagnostics go to stdout: the console host's logger is silent below Critical.
      let verbose (msg: string) =
          if tourny.VerboseLogging then
              ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkGray msg

      // Deviation prevention shares moves between games through replayDicts, one per engine. A
      // game seeds its own copies before it starts - saved games first, then what earlier games
      // of this run established - and merges them back only when it has finished, so a game
      // running alongside it never sees a partial line. Both under one lock.
      let replayList = ResizeArray<GameReplay>()
      let replayDicts =
          [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
      let replayLock = obj()
      let seedReplay (pair: Pairing) (white: ReferenceGameReplay) (black: ReferenceGameReplay) =
          lock replayLock (fun () ->
              let localDicts = [ pair.White.Name, white; pair.Black.Name, black ] |> Map.ofList
              prepareGameReplay pair localDicts replayList referenceGames gamesAlreadyPlayed
              for kvp in replayDicts.[pair.White.Name] do
                  if not (white.ContainsKey kvp.Key) then white.[kvp.Key] <- kvp.Value
              for kvp in replayDicts.[pair.Black.Name] do
                  if not (black.ContainsKey kvp.Key) then black.[kvp.Key] <- kvp.Value)
      let mergeReplay (pair: Pairing) (white: ReferenceGameReplay) (black: ReferenceGameReplay) (result: Result) (gameData: PGNTypes.GameMetadata) (moves: ResizeArray<string>) =
          lock replayLock (fun () ->
              for kvp in white do replayDicts.[pair.White.Name].[kvp.Key] <- kvp.Value
              for kvp in black do replayDicts.[pair.Black.Name].[kvp.Key] <- kvp.Value
              addToReplayList replayList tourny result gameData moves)

      // The board at the end of the opening: the FEN (or the start position), then the book
      // moves up to OpeningsPly. EPD books carry no moves. Sets the tournament's FRC flag from
      // a FEN opening, as before.
      let boardAfterOpening (pair: Pairing) =
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

      let metadataOf (pair: Pairing) (result: Result) : PGNTypes.GameMetadata =
          { OpeningHash = pair.OpeningHash
            Event = tourny.Description
            Site = tourny.Name
            Date = DateTime.Now.ToShortDateString()
            Round = pair.RoundNr
            White = result.Player1
            Black = result.Player2
            Result = result.Result
            Reason = result.Reason
            GameTime = result.GameTime
            Moves = result.Moves
            Fen = pair.Opening.Fen
            OpeningName = pair.Opening.GameMetaData.OpeningName
            Deviations = tourny.DeviationCounter
            StartEvals = result.OutOfOpeningEvals
            OtherTags = pair.Opening.GameMetaData.OtherTags }

      // The sequential runners reseed the deviation counter from PGN history before every
      // pairing (via searchAndPrepareReplay); the parallel path called the bare
      // prepareGameReplay and never did, so a resumed run restarted the count at zero and
      // wrote Deviations tags lower than games already in the same file. The seed value
      // comes from gamesAlreadyPlayed, which is fixed for the whole run, so it belongs here
      // once rather than inside the workers where it would race.
      let seededDeviations =
          gamesAlreadyPlayed |> Seq.tryLast |> Option.map (fun g -> g.GameMetaData.Deviations) |> Option.defaultValue 0
      if seededDeviations > tourny.DeviationCounter then
          tourny.DeviationCounter <- seededDeviations

      let gpus = tourny.TestOptions.GPUs
      let concurrency =
          let memBased =
              HardwareInfo.concurrencyLevel
                  tourny.EngineSetup.Engines
                  tourny.TestOptions.NumberOfGamesInParallel
          let gpuBased =
              if gpus <> null && gpus.Length > 1 then
                  max memBased gpus.Length
              else
                  memBased
          max 1 gpuBased

      // A user can adjudicate "the" running game only when there is exactly one; with several
      // boards the request has no single target, so it is answered with None.
      let adjudicate = if concurrency = 1 then tryGetUserAdjudication else (fun () -> None)
      // Standings refresh. Console: every 10 games as before - each refresh re-reads the PGN
      // and runs Ordo there, and the tuner's SPRT check hangs off it. GUI: every 2 games with
      // one board, what the sequential runner it replaces did, and every `concurrency` above.
      let periodicEvery = if tourny.ConsoleOnly then 10 else max 2 concurrency
      // Per-game engine initialisation (ucinewgame + readyok, and the GUI's opening delay) is
      // what the sequential runner this replaces did for the GUI. Pooled engines in the console
      // and in multi-board runs skip it on purpose - Ceres and Lc0 spend ~10 s on readyok, see
      // playGeneric - so the choice keeps every mode exactly as it was: only the GUI with one
      // board initialises per game.
      let initPerGame = not tourny.ConsoleOnly && concurrency = 1

      if gamesLeftToPlay.Length = 0 then
          feed.Dispose()
          return []
      else
          // 1) the pairing queue: plan order, except that with deviation prevention on a pairing
          //    whose replay key is held by a game still in flight waits for it - a repeat of an
          //    earlier game's opening and colours must see that game finished and merged, or it
          //    plays against a partial line and the two then race to define it. See ReplayGate.
          let gate = ReplayGate.ReplayGate(gamesLeftToPlay, tourny.PreventMoveDeviation, tourny.PreventMoveDeviationFor)

          // Track all spawned engines for cleanup safety net
          let allEngines = ResizeArray<ChessEngine>()

          try // safety net: ensure engine processes are killed even if async fails before teardown

          // 2) one engine pool per engine name, capacity = parallelism. Instances are spawned on
          //    the first borrow, not up front: a run starts as soon as the first game's two
          //    engines are ready instead of after every instance of every name has started and
          //    answered readyok - with several Ceres or Lc0 engines that was minutes before the
          //    first move, and the sequential runner this path replaced for the GUI only ever
          //    started the two engines about to play. Each engine is registered in allEngines
          //    before init, so an init failure still gets it killed by the finally below.
          let spawnEngine (e: EngineConfig) (i: int) =
              let cfg =
                  if gpus <> null && gpus.Length > 0 then
                      let gpu = gpus.[i % gpus.Length]
                      logger.LogInformation($"Engine pool {e.Name} instance {i}: assigning GPU {gpu}")
                      assignDeviceToConfig e gpu
                  else e
              let eng = EngineHelper.createEngine (cfg, Some logger)
              lock allEngines (fun () -> allEngines.Add(eng))
              // Pooled engines that skip per-game init must be initialised here. When the game
              // initialises its own engines (GUI, one board) it must NOT happen here: the game
              // sends StartOfGame before it initialises, so the board shows the pairing while
              // Ceres or Lc0 spend their seconds on readyok - initialising at spawn moved that
              // wait in front of the first thing the page could show.
              if not initPerGame then EngineHelper.initEngine 0 eng
              eng
          let enginePools =
              tourny.EngineSetup.Engines
              |> List.map (fun e -> e.Name, LazyPool<ChessEngine>(concurrency, spawnEngine e))
              |> Map.ofList

          // 3) PGN agent: use external if provided, else create local
          let pgnAgent, ownsAgent =
              match externalPgnAgent with
              | Some a -> a, false
              | None -> ChessLibrary.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
          // Dispose the owned agent (open FileStream on the output PGN) on every exit
          // path, not just the happy path.
          use _pgnGuard =
              { new IDisposable with
                  member _.Dispose() =
                      if ownsAgent then
                          try pgnAgent.Post(ChessLibrary.FullPGNParser.Dispose) with _ -> () }

          // a thread‐safe result collector
          let results = System.Collections.Concurrent.ConcurrentBag<Result>()

          // Helper function to check the status of each engine and restart if necessary
          let engineHealthy (engine:ChessEngine) = task {
              try
                  // Check if engine has exited and try to restart
                  if engine.HasExited() then
                      logger.LogWarning($"Engine {engine.Name} has exited, attempting restart")
                      try
                          engine.StartProcess()
                          let ok = engine.WaitForReadyOk() // Wait for "readyok" response
                          if ok then
                              logger.LogCritical($"Successfully restarted engine {engine.Name}")
                              return true
                          else
                              logger.LogCritical($"Not able to restart engine {engine.Name}")
                              return false
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


          // After a game: keep the engine in the pool, or stop it. An engine that none of the
          // next games needs is stopped rather than kept: the sequential runner this path
          // replaced spawned per pairing and killed after the game, and keeping every engine
          // of a ten-engine round robin alive - ten networks on one GPU, times the number of
          // boards - is not what anyone signed up for. "The next games" are the next
          // `concurrency` pairings in plan order: the one game to come on one board, roughly
          // what the boards pick next on several (under prevention the gate can pick
          // differently - a respawn, never a hang: an eviction wakes a borrower waiting on the
          // full pool). The colour-swapped twin costs no respawn. Two engines play every game
          // and are kept. (Not a closure inside playOne's finally: the task builder emits a
          // finally twice and the compiler rejects the duplicate.)
          let keepAllEngines = tourny.EngineSetup.Engines.Length <= 2
          let settleEngine (name: string) (eng: ChessEngine) =
              let neededSoon =
                  keepAllEngines ||
                  (gate.PeekNext concurrency |> List.exists (fun p -> p.White.Name = name || p.Black.Name = name))
              if neededSoon then enginePools.[name].Return eng
              else
                  enginePools.[name].Evict eng
                  stopEngine eng
                  // Stopped for good: the safety net has nothing to do for it, and a long run
                  // with many engines would otherwise hold every wrapper it ever spawned.
                  lock allEngines (fun () -> allEngines.Remove eng |> ignore)

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
                  cts.Cancel()
                  System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
                  return Unchecked.defaultof<ChessEngine> }

          // 4) play one pairing on borrowed engines. `slot` is the worker/board index - the
          //    live-feed gameId, so the grid shows a fixed set of boards (one tile per slot),
          //    reused as games finish and new ones start.
          let playOne (slot: int) (pair: Pairing) = task {
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
              try
                  let! wOk = wEng |> engineHealthy
                  let! bOk = bEng |> engineHealthy
                  if wOk |> not || bOk |> not then
                      logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                      Exception("Unhealthy engine detected, potentially skipping game") |> raise
                  let! res =
                      async {
                          tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
                          let currentBoard = boardAfterOpening pair
                          let sb = StringBuilder()
                          Update.RoundNr pair.RoundNr |> callback
                          feed.Emit "" (Update.RoundNr pair.RoundNr)

                          let localWhiteDict = ReferenceGameReplay()
                          let localBlackDict = ReferenceGameReplay()

                          // Per-game callback: stamp this game's events with its worker slot for the live feed.
                          let gameCallback =
                              if feed.Any then
                                  let gid = string slot
                                  fun (u: Update) -> feed.Emit gid u; callback u
                              else callback

                          // The game itself. Pooled engines skip per-game init (see initPerGame);
                          // with prevention on, each side is held to the moves in its replay copy.
                          let! result =
                              let gametimer = Stopwatch.GetTimestamp()
                              async {
                                  try
                                      let replayWhite, replayBlack =
                                          if tourny.PreventMoveDeviation then
                                              seedReplay pair localWhiteDict localBlackDict
                                              Some localWhiteDict, Some localBlackDict
                                          else None, None
                                      return! playGeneric (not initPerGame) replayWhite replayBlack sb cts logger tourny currentBoard wEng bEng pair adjudicate gameCallback
                                  with
                                  | ex -> return handleGameException logger ex cts gametimer currentBoard wEng bEng pair  }

                          let gameData = metadataOf pair result
                          if tourny.PreventMoveDeviation && not (notPlayed result) then
                              mergeReplay pair localWhiteDict localBlackDict result gameData (ResizeArray(currentBoard.UciMovesPlayed))
                          if not (notPlayed result) && not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                              pgnAgent.Post (ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, sb.ToString(), result))
                          if tourny.VerboseLogging then
                              logger.LogInformation(gameMetadataSummary gameData)
                          return result
                      } |> Async.StartAsTask
                  if not (notPlayed res) then
                      results.Add res

              finally
                  settleEngine pair.White.Name wEng
                  settleEngine pair.Black.Name bEng
              }

          // 5) worker loop: task CE with proper cancellation
          let mutable gameCounter = 0
          let worker i = task {
              try
                  let mutable keepGoing = true
                  while keepGoing do
                      // The channel this loop replaced threw out of WaitToReadAsync(cts.Token) the
                      // moment the run was cancelled; the gate has no token, so ask before taking
                      // the next pairing, or a cancelled run marches through the rest of the plan.
                      if cts.IsCancellationRequested then keepGoing <- false else
                      match gate.TryTake() with
                      | ReplayGate.Done -> keepGoing <- false
                      | ReplayGate.Wait released ->
                          // One line per wake-up explains why fewer boards are busy.
                          verbose (sprintf "Gate: worker %d waits - every pending game repeats one still in flight" i)
                          do! released.WaitAsync(cts.Token)
                      | ReplayGate.Start pair ->
                          try
                              try
                                  // The round label is the one id that is unique per game; the
                                  // console's own G-number is not reliable once games overlap.
                                  verbose (sprintf "Gate: worker %d starts round %s: %s vs %s" i pair.RoundNr pair.White.Name pair.Black.Name)
                                  logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                                  do! playOne i pair
                                  let gc = Interlocked.Increment(&gameCounter)
                                  if gc % periodicEvery = 0 then
                                      let res = ResizeArray<Result>(results)
                                      callback (Update.PeriodicResults res)
                              with ex ->
                                  logger.LogError(ex, "Worker {Worker} failed game {White} vs {Black}, continuing",
                                      i, pair.White.Name, pair.Black.Name)
                                  Interlocked.Increment(&gameCounter) |> ignore
                          finally
                              // Released whether the game was played, cancelled or failed: a key
                              // held by a dead game would stall every repeat of it for the run.
                              gate.Release pair
                              verbose (sprintf "Gate: worker %d finished round %s" i pair.RoundNr)
                          // The pause the sequential runner gave the GUI after every game: the final
                          // position stays on the board for DelayBetweenGames before the next game
                          // starts. initEngines also waits this long at the NEXT start, in parallel
                          // with readyok, which is what the old runner did too - but that wait is
                          // invisible, the board has already moved on. The console sets it to zero.
                          // After Release, so a repeat of this key is not held for the pause as well.
                          if tourny.DelayBetweenGames > TimeSpan.Zero && not cts.IsCancellationRequested then
                              do! Task.Delay(tourny.DelayBetweenGames, cts.Token)
              with
              | :? OperationCanceledException -> ()
              | :? AggregateException as ae
                  when ae.InnerExceptions |> Seq.exists (fun e -> e :? OperationCanceledException) -> ()
          }

          // 6) launch exactly p workers
          let! _ =
              [| for i in 1..concurrency -> worker i |]
              |> Task.WhenAll
              |> Async.AwaitTask

          // 7) teardown
          for KeyValue(e, pool) in enginePools do
              pool.Drain() |> Array.Parallel.iter stopEngine
              printfn $"Engine {e} stopped"

          // 8) collect results
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
          // Signal tournament completion over the live feed so a grid/feed viewer can show a
          // distinct "Completed" state (vs a silently dropped feed). The internal callback's own
          // EndOfTournament fires later in Tournament.fs — after these sinks are disposed — so we
          // tee it here while the recorder/HTTP sinks are still alive. No-op when no feed is set.
          feed.Emit "" (Update.EndOfTournament tourny)
          // Bounded like every other agent round-trip in the project: if the PGN agent has
          // died, an unbounded PostAndReply hangs the tournament permanently at the point
          // where all games are already played and only the ordered copy is left to write.
          let games =
            match pgnAgent.TryPostAndReply((fun reply -> ChessLibrary.FullPGNParser.GetPGNGamesWithRaw(reply)), 30000) with
            | Some games -> games
            | None ->
                logger.LogError "PGN agent did not answer within 30s; leaving the ordered PGN copy untouched"
                ResizeArray<PgnGame>()
          // writeRawPgnGamesAdjustedToFile deletes the target before writing, so handing it an
          // empty sequence would erase a good "_ordered" file from an earlier run — turning a
          // hang into data loss. Only rewrite it when we actually have the games.
          if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not && games.Count > 0 then
              let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
              let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn"
              let combined = Path.Combine(directory,path)
              ChessLibrary.PGNWriter.writeRawPgnGamesAdjustedToFile combined games
          return results |> Seq.toList
          finally
              feed.Dispose()
              // Safety net: stop any engine processes still running (no-op if teardown already stopped them)
              for eng in allEngines do
                  try
                      if not (eng.HasExited()) then stopEngine eng
                  with _ -> ()
  }
