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

/// A pool of up to `capacity` instances created on demand by `spawn` (given the instance
/// index). Returned instances are handed out again before any new one is spawned. Generic so
/// the slot accounting can be tested without a process behind it; the runner uses it with
/// ChessEngine.
type LazyPool<'T>(capacity: int, spawn: int -> 'T) =
    let available = Channel.CreateUnbounded<'T>()
    let mutable spawned = 0

    // A failed spawn must not keep its slot, or at capacity 1 every later borrow of this
    // pool waits forever for a return that never comes. Plain try/with: inside the task
    // builder a handler cannot re-raise.
    let spawnSlot (slot: int) =
        try spawn (slot - 1)
        with _ ->
            Interlocked.Decrement(&spawned) |> ignore
            reraise ()

    member _.Borrow() : Task<'T> = task {
        match available.Reader.TryRead() with
        | true, item -> return item
        | _ ->
            // Claim a slot first; if that overshoots, give it back and wait for a return.
            let slot = Interlocked.Increment(&spawned)
            if slot <= capacity then
                return spawnSlot slot
            else
                Interlocked.Decrement(&spawned) |> ignore
                return! available.Reader.ReadAsync()
    }

    member _.Return(item: 'T) = available.Writer.TryWrite item |> ignore

    /// The borrower is not returning this instance: it has been stopped. Frees its slot so a
    /// later borrow spawns a fresh one. Only while no other borrow is waiting: a waiter decided
    /// to wait when the pool was full and is not told that a slot opened, so it would wait for
    /// a return that never comes. The runner evicts only with a single worker.
    member _.Evict(_item: 'T) = Interlocked.Decrement(&spawned) |> ignore

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
      // Optional live feed recording (EB_LIVEFEED_FILE): tee gameId-stamped wire events for the
      // multi-game grid view to tail live. No-op unless the env var is set.
      // Live-feed settings come from tournament.json's LiveFeed section; an EB_LIVEFEED_* env var
      // overrides the matching field. Missing section + no env var => no feed (normal tournament).
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
      // Serialize once, fan out to file and/or HTTP sinks (no-op when neither is configured).
      let emitFeed (gid: string) (u: Update) =
          if liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome then
              let line = LiveFeedWire.withGameId gid (LiveFeedWire.serializeUpdate u)
              liveFeedRecorder |> Option.iter (fun r -> r.RecordLine line)
              liveFeedHttpSink |> Option.iter (fun s -> s.Send line)
      // Tagged fan-out to every sink, including the in-process one (WebGUI multi-board grid).
      let emitAll (gid: string) (u: Update) =
          emitFeed gid u
          taggedSink |> Option.iter (fun s -> s gid u)
      // The pairing table and the total the page shows come from these two; the sequential
      // runner sent them and this path never did, so the page sat empty until the first game.
      callback (Update.TotalNumberOfPairs allPairings.Length)
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      callback (Update.StartOfTournament startInfo)
      emitAll "" (Update.StartOfTournament startInfo)

      let replayList = ResizeArray<GameReplay>()
      let replayDicts =
          [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
      let replayLock = obj()

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


          // 4) helper to play one pairing using borrowed engines.
          // `slot` is the worker/board index — used as the live-feed gameId so the grid shows a
          // fixed set of boards (one tile per slot), reused as games finish and new ones start.
          // After a game: keep the engine in the pool, or stop it. With one board and more than
          // two engines, an engine that does not play the next game is stopped rather than kept:
          // the sequential runner this path replaced spawned per pairing and killed after the
          // game, and keeping every engine of a ten-engine round robin alive - ten networks on
          // one GPU - is not what anyone signed up for. The engine the next game needs stays, so
          // the colour-swapped twin costs no respawn. Multi-board runs keep the pool: they need
          // the instances. (Not a closure inside the finally below: the task builder emits a
          // finally twice and the compiler rejects the duplicate.)
          let keepAllEngines = concurrency > 1 || tourny.EngineSetup.Engines.Length <= 2
          let settleEngine (name: string) (eng: ChessEngine) =
              let playsNext =
                  keepAllEngines ||
                  (match gate.PeekNext() with
                   | Some p -> p.White.Name = name || p.Black.Name = name
                   | None -> false)
              if playsNext then enginePools.[name].Return eng
              else
                  enginePools.[name].Evict eng
                  try eng.Quit() with _ -> ()
                  try eng.StopProcess() with _ -> ()
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
                  let! (res, pairing) =
                      async {
                          let currentBoard = Board()
                          match pair with
                          |_ when String.IsNullOrEmpty pair.Opening.Fen |> not ->
                              currentBoard.LoadFen(pair.Opening.Fen)
                              currentBoard.StartPosition <- pair.Opening.Fen
                              tourny.IsChess960 <- currentBoard.IsFRC
                          |_ ->
                            currentBoard.LoadFen Chess.startPos
                          tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
                          let limit = tourny.Opening.OpeningsPly
                          let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
                          let completeGame =
                            openingMoves
                            |> Seq.mapi(fun i m ->
                                  if m.Color = "w" then
                                    sprintf "%d. %s" m.MoveNumber m.San
                                  else
                                    sprintf "%s" m.San)
                            |> String.concat " "

                          if tourny.VerboseLogging then
                              logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)

                          if pair.Opening.Fen = "" then
                              currentBoard.LoadFen Chess.startPos
                              currentBoard.StartPosition <- Chess.startPos
                          else
                              currentBoard.LoadFen pair.Opening.Fen
                              currentBoard.StartPosition <- pair.Opening.Fen
                              tourny.IsChess960 <- currentBoard.IsFRC
                          let mutable moveIndex = 0
                          if not epdBook then
                              for m in openingMoves do
                                  currentBoard.PlayOpeningMove m.San

                          let posWithMoves =
                              let fen = currentBoard.StartPosition
                              let start = $"position fen {fen} moves"
                              currentBoard.UciMovesPlayed
                              |> Seq.fold(fun state m -> sprintf "%s %s" state m) start

                          if tourny.VerboseLogging then
                              logger.LogDebug("{position}", posWithMoves)


                          let sb = StringBuilder()
                          Update.RoundNr pair.RoundNr |> callback
                          emitAll "" (Update.RoundNr pair.RoundNr)

                          let localWhiteDict = ReferenceGameReplay()
                          let localBlackDict = ReferenceGameReplay()

                          // Per-game callback: stamp this game's events with its worker slot for the live feed.
                          let gameCallback =
                              if liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome || taggedSink.IsSome then
                                  let gid = string slot
                                  fun (u: Update) -> emitAll gid u; callback u
                              else callback

                          let! result =
                              let gametimer = Stopwatch.GetTimestamp()
                              async {
                                  try
                                      if tourny.PreventMoveDeviation then
                                          lock replayLock (fun () ->
                                              let localDicts = [ pair.White.Name, localWhiteDict; pair.Black.Name, localBlackDict ] |> Map.ofList
                                              prepareGameReplay pair localDicts replayList referencGamesPlayed gamesAlreadyPlayed
                                              for kvp in replayDicts.[pair.White.Name] do
                                                  if not (localWhiteDict.ContainsKey kvp.Key) then localWhiteDict[kvp.Key] <- kvp.Value
                                              for kvp in replayDicts.[pair.Black.Name] do
                                                  if not (localBlackDict.ContainsKey kvp.Key) then localBlackDict[kvp.Key] <- kvp.Value)
                                          if initPerGame then
                                              return! playDoNotDeviate localWhiteDict localBlackDict sb cts logger tourny currentBoard wEng bEng pair adjudicate gameCallback
                                          else
                                              return! playConsoleDoNotDeviate localWhiteDict localBlackDict sb cts logger tourny currentBoard wEng bEng pair adjudicate gameCallback
                                      else
                                          if initPerGame then
                                              return! play sb cts logger tourny currentBoard wEng bEng pair adjudicate gameCallback
                                          else
                                              return! playConsole sb cts logger tourny currentBoard wEng bEng pair adjudicate gameCallback

                                  with
                                  | ex -> return handleGameException logger ex cts gametimer currentBoard wEng bEng pair  }

                          let gameData : PGNTypes.GameMetadata =
                              {
                                  OpeningHash = pair.OpeningHash
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
                                  OtherTags = pair.Opening.GameMetaData.OtherTags
                              }

                          // Cancel results (crashed/aborted games) must not seed replay state or
                          // reach the PGN — a written game counts in standings/SPRT and makes
                          // Scheduler.Diff treat the pair as played on resume.
                          // NotStarted is Result.Empty from a cancellation race — not a played game either.
                          let isCancelled =
                              result.Reason = MiscTypes.ResultReason.Cancel
                              || result.Reason = MiscTypes.ResultReason.NotStarted
                          if tourny.PreventMoveDeviation && not isCancelled then
                              lock replayLock (fun () ->
                                  for kvp in localWhiteDict do replayDicts.[pair.White.Name].[kvp.Key] <- kvp.Value
                                  for kvp in localBlackDict do replayDicts.[pair.Black.Name].[kvp.Key] <- kvp.Value
                                  addToReplayList replayList tourny result gameData (ResizeArray(currentBoard.UciMovesPlayed)))

                          let moveSection = sb.ToString()
                          if not isCancelled && not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                              pgnAgent.Post (ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
                          if tourny.VerboseLogging then
                              logger.LogInformation(gameMetadataSummary gameData)
                          return result, pair
                      } |> Async.StartAsTask
                  if res.Reason <> MiscTypes.ResultReason.Cancel then
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
                          // Verbose only, on stdout like the replay diagnostics: one line per wake-up
                          // explains why fewer boards are busy.
                          if tourny.VerboseLogging then
                              ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkGray
                                  (sprintf "Gate: worker %d waits - every pending game repeats one still in flight" i)
                          do! released.WaitAsync(cts.Token)
                      | ReplayGate.Start pair ->
                          try
                              try
                                  // The round label is the one id that is unique per game; the
                                  // console's own G-number is not reliable once games overlap.
                                  if tourny.VerboseLogging then
                                      ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkGray
                                          (sprintf "Gate: worker %d starts round %s: %s vs %s" i pair.RoundNr pair.White.Name pair.Black.Name)
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
                              if tourny.VerboseLogging then
                                  ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkGray
                                      (sprintf "Gate: worker %d finished round %s" i pair.RoundNr)
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
              pool.Drain()
              |> Array.Parallel.iter (fun eng ->
                  try eng.Quit() with _ -> ()
                  try eng.StopProcess() with _ -> ())
              printfn $"Engine {e} stopped"

          // 8) collect results
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
          // Signal tournament completion over the live feed so a grid/feed viewer can show a
          // distinct "Completed" state (vs a silently dropped feed). The internal callback's own
          // EndOfTournament fires later in Tournament.fs — after these sinks are disposed — so we
          // tee it here while the recorder/HTTP sinks are still alive. No-op when no feed is set.
          emitAll "" (Update.EndOfTournament tourny)
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
              liveFeedRecorder |> Option.iter (fun r -> r.Dispose())
              liveFeedHttpSink |> Option.iter (fun s -> s.Dispose())
              // Safety net: stop any engine processes still running (no-op if teardown already stopped them)
              for eng in allEngines do
                  try
                      if not (eng.HasExited()) then
                          try eng.Quit() with _ -> ()
                          try eng.StopProcess() with _ -> ()
                  with _ -> ()
  }
