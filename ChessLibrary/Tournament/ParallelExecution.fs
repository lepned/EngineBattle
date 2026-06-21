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

let parallelTournamentRun
  (logger: ILogger)
  (tourny: Tournament)
  (callback: Update -> unit)
  (cts: CancellationTokenSource)
  (externalPgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) =
  // Ladder, Cup, and Swiss manage their own pairings — dispatch directly
  let mode = if String.IsNullOrWhiteSpace tourny.TournamentMode then "" else tourny.TournamentMode.Trim().ToLowerInvariant()
  ChessLibrary.Engine.resetPrintedEngines()
  match mode with
  | "ladder" ->
      TournamentRunners.ladder logger tourny callback cts (fun () -> None) externalPgnAgent
  | "cup" ->
      let seeding =
        match tourny.CupOptions.SeedingStrategy with
        | null -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
        | s when s.Equals("random", StringComparison.OrdinalIgnoreCase) -> TournamentPairing.PairingHelper.CupSeedingStrategy.Random
        | _ -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
      TournamentRunners.cup seeding tourny.CupOptions.UniquePerMatchOnly false logger tourny callback cts (fun () -> None) externalPgnAgent
  | "swiss" ->
      TournamentRunners.swiss logger tourny callback cts (fun () -> None) externalPgnAgent
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
              let parsed = ChessLibrary.FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
              parsed |> Array.iter Hash.writeOpeningHashToPgnGame
              parsed
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
      let priorPairs = gamesAlreadyPlayed.Length / gamesPerPair
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
          |> ChessLibrary.Scheduler.Diff.applyPairLabels priorPairs gamesPerPair
          |> ChessLibrary.Scheduler.Diff.toPairings

      if tourny.VerboseLogging then
          PairingHelper.printAllOpeningPairs logger gamesLeftToPlay

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
      callback (Update.StartOfTournament startInfo)
      emitFeed "" (Update.StartOfTournament startInfo)

      let replayList = ResizeArray<GameReplay>()
      let replayDicts =
          [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
      let replayLock = obj()

      let searchReplayList (pairing : Pairing) =
          searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

      let gpus = tourny.TestOptions.GPUs
      let concurrency =
          let memBased =
              HardwareInfo.concurrencyLevel
                  tourny.EngineSetup.Engines
                  tourny.TestOptions.NumberOfGamesInParallelConsoleOnly
          let gpuBased =
              if gpus <> null && gpus.Length > 1 then
                  max memBased gpus.Length
              else
                  memBased
          max 1 gpuBased

      if gamesLeftToPlay.Length = 0 then
          return []
      else
          // 1) build pairing channel
          let pairingCh = Channel.CreateUnbounded<Pairing>()
          for pair in gamesLeftToPlay do
              pairingCh.Writer.TryWrite(pair) |> ignore
          pairingCh.Writer.Complete()

          // 2) build one engine‐pool channel per engine‐name, capacity = parallelism
          // Track all spawned engines for cleanup safety net
          let allEngines = ResizeArray<ChessEngine>()
          let enginePools =
              tourny.EngineSetup.Engines
              |> List.toArray
              |> Array.Parallel.map (fun e ->
                  let ch = Channel.CreateBounded<ChessEngine>(concurrency)
                  // pre-spawn p instances
                  let engines =
                      [| 0..concurrency - 1 |]
                      |> Array.Parallel.map (fun i ->
                          let cfg =
                            if gpus <> null && gpus.Length > 0 then
                              let gpu = gpus.[i % gpus.Length]
                              logger.LogInformation($"Engine pool {e.Name} instance {i}: assigning GPU {gpu}")
                              assignDeviceToConfig e gpu
                            else e
                          let eng = EngineHelper.createEngine (cfg, Some logger)
                          EngineHelper.initEngine 0 eng
                          lock allEngines (fun () -> allEngines.Add(eng))
                          ch.Writer.TryWrite(eng) |> ignore
                          eng.Name, ch )
                  engines )
              |> Array.concat
              |> Map.ofArray

          try // safety net: ensure engine processes are killed even if async fails before teardown

          // 3) PGN agent: use external if provided, else create local
          let pgnAgent, ownsAgent =
              match externalPgnAgent with
              | Some a -> a, false
              | None -> ChessLibrary.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true

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
          let playOne (slot: int) (pair: Pairing) = task {
              // Borrow engines in sorted name order to prevent ABBA deadlock.
              // With openingsTwice, consecutive pairings swap colors (A-white/B-black then B-white/A-black).
              // Two workers borrowing in white-then-black order can deadlock when they cross.
              let firstName, secondName =
                  if String.Compare(pair.White.Name, pair.Black.Name, StringComparison.Ordinal) <= 0
                  then pair.White.Name, pair.Black.Name
                  else pair.Black.Name, pair.White.Name
              let! firstEng = enginePools.[firstName].Reader.ReadAsync()
              let! secondEng = enginePools.[secondName].Reader.ReadAsync()
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

                          let localWhiteDict = ReferenceGameReplay()
                          let localBlackDict = ReferenceGameReplay()

                          // Per-game callback: stamp this game's events with its GameNr for the live feed.
                          let gameCallback =
                              if liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome then
                                  let gid = string slot
                                  fun (u: Update) -> emitFeed gid u; callback u
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
                                          return! playConsoleDoNotDeviate localWhiteDict localBlackDict sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) gameCallback
                                      else
                                          return! playConsole sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) gameCallback

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

                          if tourny.PreventMoveDeviation then
                              lock replayLock (fun () ->
                                  for kvp in localWhiteDict do replayDicts.[pair.White.Name].[kvp.Key] <- kvp.Value
                                  for kvp in localBlackDict do replayDicts.[pair.Black.Name].[kvp.Key] <- kvp.Value
                                  addToReplayList replayList tourny result gameData (ResizeArray(currentBoard.UciMovesPlayed)))

                          let moveSection = sb.ToString()
                          if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                              pgnAgent.Post (ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
                          if tourny.VerboseLogging then
                              logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
                          return result, pair
                      } |> Async.StartAsTask
                  results.Add res

              finally
                  if not (enginePools.[pair.White.Name].Writer.TryWrite(wEng)) then
                      logger.LogError("Failed to return engine {Engine} to pool", wEng.Name)
                  if not (enginePools.[pair.Black.Name].Writer.TryWrite(bEng)) then
                      logger.LogError("Failed to return engine {Engine} to pool", bEng.Name)
              }

          // 5) worker loop: task CE with proper cancellation
          let mutable gameCounter = 0
          let worker i = task {
              try
                  let mutable keepGoing = true
                  while keepGoing do
                      let! canRead = pairingCh.Reader.WaitToReadAsync(cts.Token)
                      if canRead then
                          match pairingCh.Reader.TryRead() with
                          | true, pair ->
                              try
                                  logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                                  do! playOne i pair
                                  let gc = Interlocked.Increment(&gameCounter)
                                  if gc % 10 = 0 then
                                      let res = ResizeArray<Result>(results)
                                      callback (Update.PeriodicResults res)
                              with ex ->
                                  logger.LogError(ex, "Worker {Worker} failed game {White} vs {Black}, continuing",
                                      i, pair.White.Name, pair.Black.Name)
                                  Interlocked.Increment(&gameCounter) |> ignore
                          | _ -> ()
                      else
                          keepGoing <- false
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
          for KeyValue(e, ch) in enginePools do
              ch.Writer.Complete()
              let _ =
                  [|1.. ch.Reader.Count|]
                  |> Array.Parallel.map (fun _ ->
                      let eng = enginePools.[e].Reader.ReadAsync().AsTask().Result
                      try eng.Quit() with _ -> ()
                      try eng.StopProcess() with _ -> ())
              printfn $"Engine {e} stopped"

          // 8) collect results
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
          let games = pgnAgent.PostAndReply(fun reply -> ChessLibrary.FullPGNParser.GetPGNGames(reply))
          if ownsAgent then
              pgnAgent.Post(ChessLibrary.FullPGNParser.Dispose)
          if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not then
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
