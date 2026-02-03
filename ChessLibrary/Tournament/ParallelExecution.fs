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
open ChessLibrary.TournamentRunners.TournamentUtils

let parallelTournamentRunBackup (logger: ILogger) (tourny: Tournament) callback (cts: CancellationTokenSource) = async {
  let mutable counter = 0
  let mutable gameNr = 0
  logger.LogInformation($"Tournament in parallel run about to start")
  let board = Board()
  board.LoadFen Chess.startPos
  let results = ResizeArray<Result>()
  let mutable epdBook = false
  let games =
    match tourny.Opening.OpeningsPath with
    |Some path ->
      if File.Exists path |> not then
        if tourny.VerboseLogging then
          logger.LogError($"Opening file {path} does not exist")
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
      elif path.ToLower().Contains ".epd" then
        epdBook <- true
        let all = EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
        all
      else
        let all = ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
        if tourny.VerboseLogging then
          logger.LogInformation $"Total number of openings in PGN = {all.Length}"
        all
    |_ ->
      [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]

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
  let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
  let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
  let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
  gameNr <- gamesAlreadyPlayed.Length

  let allPairings =
    if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) then
      if tourny.Opening.OpeningsTwice then
        PairingHelper.gauntletDoubleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
      else
        PairingHelper.gauntletSingleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
    else
      if tourny.Opening.OpeningsTwice then
        PairingHelper.generateAllRoundRobinDoubleRounds tourny.EngineSetup.Engines gamesToPlay
      else
        PairingHelper.generateAllRoundRobinSingleRounds tourny.EngineSetup.Engines gamesToPlay
  let playedSet = PairingHelper.playedSet gamesAlreadyPlayed
  let gamesLeftToPlay =
    [
      for p in allPairings do
      if PairingHelper.hasPlayedBefore p playedSet |> not then
        yield p
    ]

  if tourny.VerboseLogging then
    PairingHelper.printAllOpeningPairs logger gamesLeftToPlay
  let totalGames = allPairings.Length
  tourny.TotalGames <- totalGames

  let numberOfGamesPlayed = gamesAlreadyPlayed.Length
  if gamesLeftToPlay.Length = 0 then
    return results |> Seq.toList

  else
    let pgnGameWriterAgent = ChessLibrary.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
    callback (Update.TotalNumberOfPairs allPairings.Length)
    callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
    let lastGame = gamesAlreadyPlayed |> Seq.tryLast
    let pairings =
      match lastGame, allPairings.Length > numberOfGamesPlayed with
      |Some last, true ->
          if last.Mainline.Count > 0 then
            let lastMove = last.Mainline |> Seq.last
            if lastMove.Comment.Contains "cancelled" || lastMove.Comment.Contains "cancelled" then
              allPairings |> List.skip (numberOfGamesPlayed - 1)
            else
              allPairings |> List.skip numberOfGamesPlayed
          else
            allPairings
      |_ -> allPairings

    tourny.CurrentGameNr <- numberOfGamesPlayed
    let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny pairings
    let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
    callback (Update.StartOfTournament startInfo)

    let nGamesInParallel = max 1 tourny.TestOptions.NumberOfGamesInParallelConsoleOnly
    let replayList = ResizeArray<GameReplay>()
    let replayDicts =
      [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList

    let searchReplayList (pairing : Pairing) =
      searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

    let allEngines =
      tourny.EngineSetup.Engines |> List.map(fun e -> EngineHelper.createEngine (e, Some logger))

    let getPairsForTwoEnginesOnlyForParallelRun (engine1:string) (engine2 : string) =
        [
          yield! gamesLeftToPlay |> List.filter(fun p -> p.White.Name = engine1 && p.Black.Name = engine2 )
          yield! gamesLeftToPlay |> List.filter(fun p -> p.White.Name = engine2 && p.Black.Name = engine1 )
        ] |> List.sortBy(fun p -> p.Opening.GameNumber)

    let getAllPairsCombinationsFromPairings =
      [
        let mutable played = HashSet<string * string>()
        for engine1 in allEngines do
          for engine2 in allEngines do
            if engine1.Name <> engine2.Name then
              if played.Contains(engine1.Name, engine2.Name) |> not then
                let pairs = getPairsForTwoEnginesOnlyForParallelRun engine1.Name engine2.Name
                played.Add(engine1.Name, engine2.Name) |> ignore
                played.Add(engine2.Name, engine1.Name) |> ignore
                yield engine1, engine2, pairs
      ] |> List.filter(fun (_,_,p) -> p.Length > 0)

    let initializePairForParalellRun (e1:ChessEngine, e2:ChessEngine) (n:int) =
      [
        for _ = 1 to n do
          let eng1 = EngineHelper.createEngine (e1.Config, Some logger)
          let eng2 = EngineHelper.createEngine (e2.Config, Some logger)
          GameInitialization.initEngines 0 tourny eng1 eng2 logger
          yield eng1, eng2
      ]

    let calculateMemUsage (engine1:ChessEngine) (engine2:ChessEngine) =
      let configs = [engine1.Config; engine2.Config]
      let footPrintBoth = HardwareInfo.sumFootprints configs
      let totalAvail = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> uint64
      let maxMem = totalAvail / 2UL
      let concurrency = int (maxMem / footPrintBoth)
      concurrency

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

    let parallelTasks =
        let enginePairsToRun = getAllPairsCombinationsFromPairings
        seq {
              for (engine1, engine2, myPairs) in enginePairsToRun do
                let footPrintMemory = calculateMemUsage engine1 engine2
                let nGamesInParallel = min footPrintMemory nGamesInParallel
                let engines = initializePairForParalellRun (engine1, engine2) nGamesInParallel
                let chunks = myPairs |> List.chunkBySize nGamesInParallel

                for chunk in chunks do
                  yield
                    [
                    for idx, pair in chunk |> List.mapi(fun idx e -> idx, e) do
                      let currentBoard = Board()
                      let mutable engine1, engine2 = engines[idx]
                      if engine1.Name = pair.Black.Name then
                        let (eng1,eng2) = engine2, engine1
                        engine1 <- eng1
                        engine2 <- eng2
                      try
                          let wOk = engine1 |> engineHealthy |> Async.AwaitIAsyncResult |> Async.RunSynchronously
                          let bOk = engine2 |> engineHealthy |> Async.AwaitIAsyncResult |> Async.RunSynchronously
                          if wOk |> not || bOk |> not then
                              logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                              Exception("Unhealthy engine detected, potentially skipping game") |> raise
                      with
                      |ex ->
                          logger.LogCritical(ex, "Error checking engine health for {white} vs {black}", pair.White.Name, pair.Black.Name)

                      match pair with
                      |_ when String.IsNullOrEmpty pair.Opening.Fen |> not ->
                          currentBoard.LoadFen(pair.Opening.Fen)
                          currentBoard.StartPosition <- pair.Opening.Fen
                          tourny.IsChess960 <- currentBoard.IsFRC
                      |_ ->
                        currentBoard.LoadFen Chess.startPos
                      tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
                      if cts.IsCancellationRequested then
                        () //todo
                      else
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
                          board.StartPosition <- Chess.startPos
                        else
                          currentBoard.LoadFen pair.Opening.Fen
                          currentBoard.StartPosition <- pair.Opening.Fen
                          tourny.IsChess960 <- currentBoard.IsFRC

                        if not epdBook then
                          for m in openingMoves do
                            currentBoard.PlayOpeningMove m.San

                        let posWithMoves =
                          let fen = currentBoard.StartPosition
                          let start = $"position fen {fen} moves"
                          currentBoard.UciMovesPlayed |> Seq.fold(fun state m ->
                            sprintf "%s %s" state m) start
                        if tourny.VerboseLogging then
                          logger.LogDebug("{position}", posWithMoves)

                        async {
                            let sb = StringBuilder()
                            Interlocked.Increment(&gameNr) |> ignore
                            Update.RoundNr pair.RoundNr |> callback
                            let moreThanTwoPlayers = tourny.EngineSetup.EngineDefList.Length > 2
                            let! result =
                                let gametimer = Stopwatch.GetTimestamp()
                                async {
                                    try
                                      if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                          searchReplayList pair
                                          let whiteReplayDict = replayDicts.[pair.White.Name]
                                          let blackReplayDict = replayDicts.[pair.Black.Name]
                                          return! playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard engine1 engine2 pair (fun () -> None) callback
                                      else
                                          return! play sb cts logger tourny currentBoard engine1 engine2 pair (fun () -> None) callback

                                    with
                                    | ex -> return handleGameException logger ex cts gametimer board engine1 engine2 pair  }

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

                            let moveSection = sb.ToString()
                            if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                              pgnGameWriterAgent.Post (ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
                            if tourny.VerboseLogging then
                              logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
                            return result, pair
                            }
                    ]

                try
                  for (e1,e2) in engines do
                    e1.StopProcess()
                    e2.StopProcess()
                with e ->
                    logger.LogError(e, "Unexpected error when trying to shutdown the engines")
        }

    for task in parallelTasks do
      let! partialRes = Async.Parallel task
      for (res,p) in partialRes do
        counter <- counter + 1
        results.Add res
        if counter % (nGamesInParallel * 5) = 0 then
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)

    let res = ResizeArray<Result>(results)
    callback (Update.PeriodicResults res)
    let games = pgnGameWriterAgent.PostAndReply(fun reply -> ChessLibrary.FullPGNParser.GetPGNGames(reply))
    pgnGameWriterAgent.Post(ChessLibrary.FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
    if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not then
        let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
        let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn"
        let combined = Path.Combine(directory,path)
        ChessLibrary.PGNWriter.writeRawPgnGamesAdjustedToFile combined games
    return results |> Seq.toList
    }

let parallelTournamentRun
  (logger: ILogger)
  (tourny: Tournament)
  (callback: Update -> unit)
  (cts: CancellationTokenSource) = async {

      logger.LogInformation("Tournament in parallel run about to start")
      let mutable epdBook = false
      let games =
          match tourny.Opening.OpeningsPath with
          |Some path ->
          if File.Exists path |> not then
              if tourny.VerboseLogging then
                  logger.LogError($"Opening file {path} does not exist")
              [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
          elif path.ToLower().Contains ".epd" then
              epdBook <- true
              let all = EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
              all
          else
              let all = ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
              if tourny.VerboseLogging then
                  logger.LogInformation $"Total number of openings in PGN = {all.Length}"
              all
          |_ ->
              [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]

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
      let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
      let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
      let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)

      let allPairings =
          if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) then
              if tourny.Opening.OpeningsTwice then
                  PairingHelper.gauntletDoubleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
              else
                  PairingHelper.gauntletSingleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
          else
              if tourny.Opening.OpeningsTwice then
                  PairingHelper.generateAllRoundRobinDoubleRounds tourny.EngineSetup.Engines gamesToPlay
              else
                  PairingHelper.generateAllRoundRobinSingleRounds tourny.EngineSetup.Engines gamesToPlay
      let playedSet = PairingHelper.playedSet gamesAlreadyPlayed
      let gamesLeftToPlay =
          [
          for p in allPairings do
          if PairingHelper.hasPlayedBefore p playedSet |> not then
              yield p
          ]

      if tourny.VerboseLogging then
          PairingHelper.printAllOpeningPairs logger gamesLeftToPlay

      let totalGames = allPairings.Length
      tourny.TotalGames <- totalGames
      let numberOfGamesPlayed = gamesAlreadyPlayed.Length
      tourny.CurrentGameNr <- numberOfGamesPlayed

      let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
      let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
      callback (Update.StartOfTournament startInfo)

      let replayList = ResizeArray<GameReplay>()
      let replayDicts =
          [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList

      let searchReplayList (pairing : Pairing) =
          searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

      let concurrency =
          HardwareInfo.concurrencyLevel
              tourny.EngineSetup.Engines
              tourny.TestOptions.NumberOfGamesInParallelConsoleOnly

      if concurrency < 1 then
          printfn "Concurrency level is less than 1, using backup plan"
          let! results = parallelTournamentRunBackup logger tourny callback cts
          return results
      else
          // 1) build pairing channel
          let pairingCh = Channel.CreateUnbounded<Pairing>()
          for pair in gamesLeftToPlay do
              pairingCh.Writer.TryWrite(pair) |> ignore
          pairingCh.Writer.Complete()

          // 2) build one engine‐pool channel per engine‐name, capacity = parallelism
          let enginePools =
              tourny.EngineSetup.Engines
              |> List.toArray
              |> Array.Parallel.map (fun e ->
                  let ch = Channel.CreateBounded<ChessEngine>(concurrency)
                  // pre-spawn p instances
                  let engines =
                      [| 1..concurrency |]
                      |> Array.Parallel.map (fun _ ->
                          let eng = EngineHelper.createEngine (e, Some logger)
                          EngineHelper.initEngine 0 eng
                          ch.Writer.TryWrite(eng) |> ignore
                          eng.Name, ch )
                  engines )
              |> Array.concat
              |> Map.ofArray

          // 3) PGN writer agent stays the same
          let pgnAgent = ChessLibrary.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath

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


          // 4) helper to play one pairing using borrowed engines
          let playOne (pair: Pairing) = task {
              // borrow
              let! wEng = enginePools.[pair.White.Name].Reader.ReadAsync()
              let! bEng = enginePools.[pair.Black.Name].Reader.ReadAsync()
              let! wOk = wEng |> engineHealthy
              let! bOk = bEng |> engineHealthy
              if wOk |> not || bOk |> not then
                  logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                  Exception("Unhealthy engine detected, potentially skipping game") |> raise
              try
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
                          let moreThanTwoPlayers = tourny.EngineSetup.EngineDefList.Length > 2

                          let! result =
                              let gametimer = Stopwatch.GetTimestamp()
                              async {
                                  try
                                      if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                          searchReplayList pair
                                          let whiteReplayDict = replayDicts.[pair.White.Name]
                                          let blackReplayDict = replayDicts.[pair.Black.Name]
                                          return! playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) callback
                                      else
                                          return! play sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) callback

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

                          let moveSection = sb.ToString()
                          if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                              pgnAgent.Post (ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
                          if tourny.VerboseLogging then
                              logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
                          return result, pair
                      } |> Async.StartAsTask
                  results.Add res

              finally
                  enginePools.[pair.White.Name].Writer.WriteAsync(wEng) |> ignore
                  enginePools.[pair.Black.Name].Writer.WriteAsync(bEng) |> ignore
              }

          // 5) worker loop: pull pairings until done
          let mutable gameCounter = 0 // Counter to track the number of games processed
          let worker i = async {
              while pairingCh.Reader.WaitToReadAsync().Result do
                  match pairingCh.Reader.TryRead() with
                  | true, pair ->
                      logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                      do! playOne pair |> Async.AwaitTask
                      Interlocked.Increment(&gameCounter) |> ignore // Increment the counter atomically
                      if gameCounter % 10 = 0 then
                          let res = ResizeArray<Result>(results) // Collect results
                          callback (Update.PeriodicResults res) // Call the callback every 10 gam
                  | _ -> ()
              }

          // 6) launch exactly p workers
          let! _ =
              [| for i in 1..concurrency -> (worker i |> Async.StartAsTask) |]
              |> Task.WhenAll
              |> Async.AwaitTask

          // 7) teardown
          for KeyValue(e, ch) in enginePools do
              ch.Writer.Complete()
              let _ =
                  [|1.. ch.Reader.Count|]
                  |> Array.Parallel.map (fun _ ->
                      let eng = enginePools.[e].Reader.ReadAsync().AsTask().Result
                      //let! eng = enginePools.[e].Reader.ReadAsync().AsTask() |> Async.AwaitTask
                      eng.StopProcess())
              printfn $"Engine {e} stopped"

          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
          let games = pgnAgent.PostAndReply(fun reply -> ChessLibrary.FullPGNParser.GetPGNGames(reply))
          pgnAgent.Post(ChessLibrary.FullPGNParser.Dispose)
          pgnAgent.Dispose()
          if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not then
              let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
              let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn"
              let combined = Path.Combine(directory,path)
              ChessLibrary.PGNWriter.writeRawPgnGamesAdjustedToFile combined games
          // return immutable list of results
          return results |> Seq.toList
  }
