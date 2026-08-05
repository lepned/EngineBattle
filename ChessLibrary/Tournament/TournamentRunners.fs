module ChessLibrary.TournamentRunners

open System
open System.IO
open System.Threading
open System.Text
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.CupTypes
open ChessLibrary.SwissTypes
open ChessLibrary.LadderTypes
open ChessLibrary.MiscTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.Chess
open ChessLibrary.ChessUtilities
open ChessLibrary.TournamentPairing
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.GamePersistence
open ChessLibrary.GameSetup

/// Utility functions for tournament time estimation
module TournamentUtils =
  let estimateGameDuration (white: TimeConfig) (black:TimeConfig) (movesEst : int) =
    let wFixedTicks = if white.NodeLimit then 0L else white.Fixed.Ticks
    let wIncrTicks = if white.NodeLimit then 0L else white.Increment.Ticks
    let bFixedTicks = if black.NodeLimit then 0L else black.Fixed.Ticks
    let bIncrTicks = if black.NodeLimit then 0L else black.Increment.Ticks
    let fixedTs = TimeSpan.FromTicks (wFixedTicks + bFixedTicks)
    let incrTs = TimeSpan.FromTicks (wIncrTicks + bIncrTicks)
    let fixedTime = fixedTs.TotalSeconds
    let incrTime = incrTs.TotalSeconds
    let seconds = fixedTime + (incrTime * float movesEst)
    seconds

  let estimateTournamentAndGameTime (pairs:int) (tourny:Tournament) (pairings: Pairing seq) =
    let movesEst = tourny.Adjudication.DrawOption.MinDrawMove + tourny.Adjudication.DrawOption.DrawMoveLength + 15
    let delay = tourny.DelayBetweenGames.TotalSeconds
    let mutable secs = 0.0
    for p in pairings do
      let whiteTc = tourny.FindTimeControl p.White.TimeControlID
      let blackTc = tourny.FindTimeControl p.Black.TimeControlID
      let avgGameDurationSec = estimateGameDuration whiteTc blackTc movesEst
      secs <- secs + avgGameDurationSec + delay
    let avgGameDurationSec =
      if secs = 0.0 then
        0.0
      else
        secs / (pairings |> Seq.length |> float)
    TimeSpan.FromSeconds(secs), TimeSpan.FromSeconds(avgGameDurationSec)

open TournamentUtils

let gauntlet (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  let mutable gameNr = 0
  let sbDev = new StringBuilder()
  logger.LogInformation($"Gauntlet tournament about to start")
  let board = Board()
  board.LoadFen Chess.startPos
  let mutable results = List.empty<Result>

  // Load openings, games already played, and reference games using helpers.
  // The new Scheduler produces a correctly-sized plan for the chosen
  // distribution (Spread for RandomOpenings=true, Shared otherwise), so
  // the runner just needs to load enough openings to cover it.
  let challengers = tourny.EngineSetup.Engines |> List.take tourny.Challengers
  let opponents = tourny.EngineSetup.Engines |> List.skip tourny.Challengers
  let numOpps = opponents.Length
  let distribution =
    if tourny.Opening.RandomOpenings then Scheduler.Spread else Scheduler.Shared
  let effectiveBookSize =
    match distribution with
    | Scheduler.Spread when numOpps > 1 -> tourny.Rounds * numOpps
    | _ -> tourny.Rounds
  let (games, epdBook) = loadOpenings tourny.Opening.OpeningsPath effectiveBookSize
  if distribution = Scheduler.Spread && numOpps > 1 && games.Length < effectiveBookSize then
    ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow
      (sprintf "Warning: Gauntlet with %d opponents and %d rounds needs %d openings to avoid wrap, but the book only has %d. Some openings will be reused and engines may play them more than expected."
        numOpps tourny.Rounds effectiveBookSize games.Length)
  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referencGamesPlayed = loadReferenceGames tourny.ReferencePGNPath

  let schedulerOpenings =
    let openings = games |> Seq.truncate effectiveBookSize |> Seq.toList
    if tourny.Opening.RandomOpenings then PairingHelper.shuffleOpeningsForTournament tourny.Opening openings
    else openings
  let scheduleCfg : Scheduler.ScheduleConfig =
    { Mode = Scheduler.Gauntlet
      Challengers = challengers
      Opponents = opponents
      Openings = schedulerOpenings
      Rounds = tourny.Rounds
      OpeningsTwice = tourny.Opening.OpeningsTwice
      PreventDeviation = tourny.PreventMoveDeviation
      Distribution = distribution }
  let plan = ChessLibrary.Scheduler.Gauntlet.generate scheduleCfg
  let gamesPerPair = if tourny.Opening.OpeningsTwice then 2 else 1
  let priorGames = gamesAlreadyPlayed.Length
  let pairings =
    plan
    |> ChessLibrary.Scheduler.Diff.applyPairLabels 0 gamesPerPair
    |> ChessLibrary.Scheduler.Diff.toPairings
  let gamesLeftToPlay =
    ChessLibrary.Scheduler.Diff.diff plan gamesAlreadyPlayed
    |> ChessLibrary.Scheduler.Diff.enforceGameLimits challengers opponents tourny.Rounds tourny.Opening.OpeningsTwice gamesAlreadyPlayed
    |> ChessLibrary.Scheduler.Diff.applyPairLabels priorGames gamesPerPair
    |> ChessLibrary.Scheduler.Diff.toPairings

  PairingHelper.logOpeningPairs logger gamesLeftToPlay
  let totalGames = pairings.Length
  tourny.TotalGames <- totalGames
  let numberOfGamesPlayed = gamesAlreadyPlayed.Length

  if gamesLeftToPlay.Length = 0 then
    return results
  else
    callback (Update.TotalNumberOfPairs pairings.Length)
    callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
    let pgnGameWriterAgent, ownsAgent =
      match pgnAgent with
      | Some a -> a, false
      | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
    tourny.CurrentGameNr <- numberOfGamesPlayed
    let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
    let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
    callback (Update.StartOfTournament startInfo)
    let replayList = ResizeArray<GameReplay>()
    let replayDicts = createReplayDicts tourny.EngineSetup.Engines

    let searchReplayList (pairing : Pairing) =
      searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

    let sb = StringBuilder()

    for pair in gamesLeftToPlay do
      if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
        searchReplayList pair
      tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
      if cts.IsCancellationRequested then
        sbDev.Clear() |> ignore
      else
        // Round label was pre-computed by the Scheduler via `applyPairLabels`
        // (seeded from PGN history), so just use it as-is.
        let roundTxt = pair.RoundNr

        // Execute game with full setup using helper
        let result = executeGameWithSetup logger tourny board pair epdBook sb cts replayDicts replayList pgnGameWriterAgent tryGetUserAdjudication callback roundTxt
        results <- result :: results

        do! Async.Sleep(tourny.DelayBetweenGames.TotalMilliseconds |> int)
        board.ResetBoardState()
        gameNr <- gameNr + 1
        if gameNr % 2 = 0 then
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)

    let res = ResizeArray<Result>(results)
    callback (Update.PeriodicResults res)
    if ownsAgent then
      pgnGameWriterAgent.Post(FullPGNParser.Dispose)
      pgnGameWriterAgent.Dispose()
    return results
}

let roundRobin (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  let mutable gameNr = 0
  logger.LogInformation($"Round robin tournament about to start")
  let numberOfPlayers = tourny.EngineSetup.Engines.Length
  let board = Board()
  board.LoadFen Chess.startPos
  let mutable engine1 = Unchecked.defaultof<ChessEngine>
  let mutable engine2 = Unchecked.defaultof<ChessEngine>
  let mutable results = List.empty<Result>

  // Load openings, games already played, and reference games using helpers
  let (games, epdBook) = loadOpenings tourny.Opening.OpeningsPath tourny.Rounds
  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referencGamesPlayed = loadReferenceGames tourny.ReferencePGNPath

  let gamesToPlay =
    let openings = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
    if tourny.Opening.RandomOpenings then PairingHelper.shuffleOpeningsForTournament tourny.Opening openings
    else openings
  let scheduleCfg : Scheduler.ScheduleConfig =
    { Mode = Scheduler.RoundRobin
      Challengers = tourny.EngineSetup.Engines
      Opponents = []
      Openings = gamesToPlay
      Rounds = tourny.Rounds
      OpeningsTwice = tourny.Opening.OpeningsTwice
      PreventDeviation = tourny.PreventMoveDeviation
      Distribution = Scheduler.Shared }
  let plan = ChessLibrary.Scheduler.RoundRobin.generate scheduleCfg
  let gamesPerPair = if tourny.Opening.OpeningsTwice then 2 else 1
  let priorGames = gamesAlreadyPlayed.Length
  let pairings =
    plan
    |> ChessLibrary.Scheduler.Diff.applyPairLabels 0 gamesPerPair
    |> ChessLibrary.Scheduler.Diff.toPairings
  let gamesLeftToPlay =
    ChessLibrary.Scheduler.Diff.diff plan gamesAlreadyPlayed
    |> ChessLibrary.Scheduler.Diff.applyPairLabels priorGames gamesPerPair
    |> ChessLibrary.Scheduler.Diff.toPairings

  PairingHelper.logOpeningPairs logger gamesLeftToPlay
  let totalGames = pairings.Length
  tourny.TotalGames <- totalGames

  let numberOfGamesPlayed = gamesAlreadyPlayed.Length
  if gamesLeftToPlay.Length = 0 then
    return results
  else
    let pgnGameWriterAgent, ownsAgent =
      match pgnAgent with
      | Some a -> a, false
      | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
    callback (Update.TotalNumberOfPairs pairings.Length)
    callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
    tourny.CurrentGameNr <- numberOfGamesPlayed
    let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny pairings
    let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
    callback (Update.StartOfTournament startInfo)
    let replayList = ResizeArray<GameReplay>()
    let replayDicts = createReplayDicts tourny.EngineSetup.Engines
    let getReplayDictForPlayer (name:string) = replayDicts.[name]

    let searchReplayList (pairing : Pairing) =
      searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

    let sb = StringBuilder()

    try
      for pair in gamesLeftToPlay do
        if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
          searchReplayList pair
        tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
        if cts.IsCancellationRequested then
          sb.Clear() |> ignore
        else
          // Setup board and log opening info
          let openingMoves = setupBoardForGame board pair epdBook tourny.Opening.OpeningsPly (fun v -> tourny.IsChess960 <- v)
          logOpeningInfo logger pair openingMoves
          logPosition logger board

          // Engine creation with reuse logic
          if numberOfPlayers > 2 then
            engine1 <- EngineHelper.createEngine (pair.White, Some logger)
            engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
          if engine1 = Unchecked.defaultof<ChessEngine> || engine2 = Unchecked.defaultof<ChessEngine> then
            engine1 <- EngineHelper.createEngine (pair.White, Some logger)
            engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
          if engine1.Name = pair.Black.Name || engine2.Name = pair.White.Name then
            let (eng1,eng2) = engine2, engine1
            engine1 <- eng1
            engine2 <- eng2

          // Round label was pre-computed by the Scheduler via `applyPairLabels`
          // (seeded from PGN history), so just use it as-is.
          let roundTxt = pair.RoundNr
          callback (Update.RoundNr roundTxt)

          // Execute game with exception handling
          let replayDictWhite = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.White.Name) else None
          let replayDictBlack = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.Black.Name) else None
          let result = executeGame tourny replayDictWhite replayDictBlack sb cts logger board engine1 engine2 pair tryGetUserAdjudication callback

          let forceStopEngines = match result.Reason with | ResultReason.Disconnected _ -> true | _ -> false
          results <- result :: results

          // Process completed game: build metadata, add to replay, write PGN
          let gameData = buildGameMetadata tourny pair result roundTxt
          addToReplayList replayList tourny result gameData board.UciMovesPlayed
          let moveSection = sb.ToString()
          writeGameToPgnSimple pgnGameWriterAgent tourny gameData moveSection result cts
          if tourny.VerboseLogging then
            logger.LogInformation(gameMetadataSummary gameData)

          // Small delay to let pumps finish their cleanup
          do! Async.Sleep 200

          cleanupEnginesConditional engine1 engine2 forceStopEngines numberOfPlayers cts
          do! Async.Sleep(tourny.DelayBetweenGames.TotalMilliseconds |> int)
          board.ResetBoardState()
          gameNr <- gameNr + 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res)

      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      if ownsAgent then
        pgnGameWriterAgent.Post(FullPGNParser.Dispose)
        pgnGameWriterAgent.Dispose()
      return results
    finally
      if engine1 <> Unchecked.defaultof<ChessEngine> then
        cleanupEngines engine1 engine2
}

let cup (strategy: PairingHelper.CupSeedingStrategy) (uniquePerMatchOnly: bool) (resumeRequested: bool) (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  let isPowerOfTwo (n: int) = n > 0 && (n &&& (n - 1) = 0)
  let resolveCupBracketPath () =
    let configuredPath =
      if obj.ReferenceEquals(tourny.CupOptions, null) then "" else tourny.CupOptions.BracketPath
    let fullPath =
      if String.IsNullOrWhiteSpace configuredPath then
        let candidates =
          [ Path.Combine(Environment.CurrentDirectory, "wwwroot")
            Path.Combine(Environment.CurrentDirectory, "WebGUI", "wwwroot") ]
        let folder =
          candidates |> List.tryFind Directory.Exists
          |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "wwwroot"))
        Path.Combine(folder, "cup_bracket.json")
      elif Path.IsPathRooted configuredPath then
        configuredPath
      else
        Path.Combine(Environment.CurrentDirectory, configuredPath)
    let dir = Path.GetDirectoryName(fullPath)
    if String.IsNullOrWhiteSpace dir |> not then
      Directory.CreateDirectory(dir) |> ignore
    fullPath

  let writeCupBracket (agent: MailboxProcessor<CupBracketMessage>) (bracket: CupBracket) =
    let maxPersistedOpenings = 50
    let trimOrder (order: ResizeArray<int>) =
      if obj.ReferenceEquals(order, null) then
        order
      else
        ResizeArray<int>(order |> Seq.truncate maxPersistedOpenings)
    let trimmedRounds =
      bracket.Rounds
      |> Seq.map (fun r ->
          let trimmedMatches =
            r.Matches
            |> Seq.map (fun m ->
                { m with OpeningOrder = trimOrder m.OpeningOrder })
            |> ResizeArray
          { r with Matches = trimmedMatches })
      |> ResizeArray
    let trimmedBracket =
      { bracket with
          GlobalOpeningOrder = trimOrder bracket.GlobalOpeningOrder
          Rounds = trimmedRounds }
    agent.PostAndReply(fun reply -> WriteCupBracket(trimmedBracket, reply))
    callback Update.CupBracketUpdated

  let roundPairIncrements =
    if obj.ReferenceEquals(tourny.CupOptions, null) then [] else tourny.CupOptions.RoundPairIncrements
  let gamesPerMatch = 2
  let gamesPerMatchForRound roundNumber =
    PairingHelper.gamesPerMatchForRound gamesPerMatch roundPairIncrements roundNumber

  let mutable gameNr = 0
  logger.LogInformation("Cup tournament about to start")
  let numberOfPlayers = tourny.EngineSetup.Engines.Length
  if isPowerOfTwo numberOfPlayers |> not then
    logger.LogError("Cup tournaments require a power-of-two number of players, got {playerCount}", numberOfPlayers)
    failwith "Cup tournaments require a power-of-two number of players."

  let board = Board()
  board.LoadFen Chess.startPos
  let mutable engine1 = Unchecked.defaultof<ChessEngine>
  let mutable engine2 = Unchecked.defaultof<ChessEngine>
  let mutable results = List.empty<Result>

  // Load openings, games already played, and reference games using helpers
  let (games, epdBook) = loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds
  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referencGamesPlayed = loadReferenceGames tourny.ReferencePGNPath

  let openings = games |> Seq.toList
  if openings.IsEmpty then
    logger.LogError("No openings available for cup tournament")
    failwith "No openings available for cup tournament."
  let randomOpenings = if obj.ReferenceEquals(tourny.CupOptions, null) then false else tourny.CupOptions.RandomOpenings

  let liveGamesByOpening = Dictionary<string,int>()
  let pgnGameWriterAgent, ownsAgent =
    match pgnAgent with
    | Some a -> a, false
    | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
  let replayList = ResizeArray<GameReplay>()
  let replayDicts = createReplayDicts tourny.EngineSetup.Engines
  let getReplayDictForPlayer (name:string) = replayDicts.[name]
  let cupBracketPath = resolveCupBracketPath()
  let cupBracketAgent = TournamentState.startCupBracketReaderWriter cupBracketPath
  let mutable matchId = 1
  let mutable openingIndex = 0
  let loadBracket () =
    cupBracketAgent.PostAndReply(fun reply -> ReadCupBracket reply)

  let ensureMatchOpeningOrder (matchInfo: CupMatch) =
    if obj.ReferenceEquals(matchInfo.OpeningOrder, null) then
      matchInfo.OpeningOrder <- ResizeArray<int>()

  let ensureGlobalOpeningOrder (bracket: CupBracket) =
    if obj.ReferenceEquals(bracket.GlobalOpeningOrder, null) then
      bracket.GlobalOpeningOrder <- ResizeArray<int>()

  let mutable bracket =
    match loadBracket () with
    | Some loaded ->
        ensureGlobalOpeningOrder loaded
        for round in loaded.Rounds do
          for matchInfo in round.Matches do
            ensureMatchOpeningOrder matchInfo
        openingIndex <- loaded.NextOpeningIndex
        matchId <- loaded.Rounds |> Seq.collect (fun r -> r.Matches |> Seq.map (fun m -> m.MatchId)) |> Seq.append [0] |> Seq.max |> (+) 1
        loaded
    | None ->
        if resumeRequested then
          logger.LogError("Resume requested for cup tournament but no bracket data was found.")
          failwith "Resume requested but cup bracket data is missing."
        { TournamentName = tourny.Name
          Strategy = strategy.ToString()
          GamesPerMatch = gamesPerMatchForRound 1
          UniqueOpeningsGlobal = not uniquePerMatchOnly
          NextOpeningIndex = openingIndex
          GlobalOpeningOrder = ResizeArray<int>()
          Rounds = ResizeArray<CupRound>()
          UpdatedUtc = DateTime.UtcNow }

  let totalRounds =
    let mutable players = numberOfPlayers
    let mutable rounds = 0
    while players > 1 do
      players <- players / 2
      rounds <- rounds + 1
    rounds

  let seedBands = PairingHelper.autoSeedBands numberOfPlayers

  let seedPlayers (players: EngineConfig list) =
    match strategy with
    | PairingHelper.CupSeedingStrategy.Random ->
        let shuffled = players |> List.toArray
        Random.Shuffle(shuffled)
        shuffled |> Array.toList
      | PairingHelper.CupSeedingStrategy.ByRating ->
          PairingHelper.seedByBands players seedBands true

  let seedPairs (players: EngineConfig list) =
    players
    |> List.chunkBySize 2
    |> List.map (fun chunk ->
        match chunk with
        | [a; b] -> (a, b)
        | _ -> failwith "Cup bracket requires even number of players")

  let seededPlayers = seedPlayers tourny.EngineSetup.Engines

  let ensureRound (roundNumber: int) =
    if bracket.Rounds |> Seq.exists (fun r -> r.RoundNumber = roundNumber) then
      ()
    else
      let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
      let roundMatches = ResizeArray<CupMatch>()
      for _ in 1..matchCount do
        let matchInfo =
          { MatchId = matchId
            RoundNumber = roundNumber
            PlayerA = "TBD"
            PlayerB = "TBD"
            PlayerARating = 0
            PlayerBRating = 0
            ScoreA = 0.0
            ScoreB = 0.0
            Winner = None
            IsDecided = false
            Games = ResizeArray<CupGame>()
            OpeningOrder = ResizeArray<int>() }
        matchId <- matchId + 1
        roundMatches.Add matchInfo
      bracket.Rounds.Add { RoundNumber = roundNumber; Matches = roundMatches }
  if bracket.Rounds.Count = 0 then
    if resumeRequested then
      logger.LogError("Resume requested for cup tournament but bracket rounds are empty.")
      failwith "Resume requested but cup bracket is empty."
    for roundNumber in 1..totalRounds do
      let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
      let roundMatches = ResizeArray<CupMatch>()
      if roundNumber = 1 then
        let pairs = seedPairs seededPlayers
        for (a, b) in pairs do
          let matchInfo =
            { MatchId = matchId
              RoundNumber = roundNumber
              PlayerA = a.Name
              PlayerB = b.Name
              PlayerARating = a.Rating
              PlayerBRating = b.Rating
              ScoreA = 0.0
              ScoreB = 0.0
              Winner = None
              IsDecided = false
              Games = ResizeArray<CupGame>()
              OpeningOrder = ResizeArray<int>() }
          matchId <- matchId + 1
          roundMatches.Add matchInfo
      else
        for _ in 1..matchCount do
          let matchInfo =
            { MatchId = matchId
              RoundNumber = roundNumber
              PlayerA = "TBD"
              PlayerB = "TBD"
              PlayerARating = 0
              PlayerBRating = 0
              ScoreA = 0.0
              ScoreB = 0.0
              Winner = None
              IsDecided = false
              Games = ResizeArray<CupGame>()
              OpeningOrder = ResizeArray<int>() }
          matchId <- matchId + 1
          roundMatches.Add matchInfo
      let round = { RoundNumber = roundNumber; Matches = roundMatches }
      bracket.Rounds.Add round

    writeCupBracket cupBracketAgent bracket

  let minTotalGames =
    [ 1 .. totalRounds ]
    |> List.sumBy (fun roundNumber ->
        let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
        matchCount * gamesPerMatchForRound roundNumber)
  tourny.TotalGames <- minTotalGames

  // Adjust TotalGames for decided matches that played fewer/more games than scheduled
  // (early termination saves games, tiebreaks add games)
  for round in bracket.Rounds do
      let gpm = gamesPerMatchForRound round.RoundNumber
      for m in round.Matches do
          if m.IsDecided then
              let diff = m.Games.Count - gpm
              if diff <> 0 then
                  tourny.TotalGames <- tourny.TotalGames + diff

  callback (Update.TotalNumberOfPairs tourny.TotalGames)
  callback (Update.PairingList (ResizeArray<Pairing>()))
  let (tTime, gTime) = estimateTournamentAndGameTime minTotalGames tourny []
  let startInfo = { NumberOfGames = minTotalGames; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny }
  callback (Update.StartOfTournament startInfo)

  let ensureGlobalOpeningOrderFn () =
    if randomOpenings && openings.Length > 1 && not uniquePerMatchOnly then
      if bracket.GlobalOpeningOrder.Count = 0 || bracket.GlobalOpeningOrder.Count < openings.Length then
        let order = [0 .. openings.Length - 1]
        let shuffled = order |> List.toArray
        Random.Shuffle(shuffled)
        bracket.GlobalOpeningOrder <- ResizeArray<int>(shuffled)
        writeCupBracket cupBracketAgent bracket

  let globalOpenings =
    ensureGlobalOpeningOrderFn ()
    if bracket.GlobalOpeningOrder.Count > 0 then
      bracket.GlobalOpeningOrder
      |> Seq.map (fun idx -> openings.[idx % openings.Length])
      |> Seq.toList
    else
      openings

  let getNextOpening (openingsList: PGNTypes.PgnGame list) (localIndex: int) =
    if uniquePerMatchOnly then
      openingsList.[localIndex % openingsList.Length]
    else
      if openingIndex >= openingsList.Length then
        openingIndex <- 0
      let opening = openingsList.[openingIndex]
      openingIndex <- openingIndex + 1
      bracket.NextOpeningIndex <- openingIndex
      opening

  let searchReplayList (pairing : Pairing) =
    searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

  let sb = StringBuilder()
  let bracketGamesPlayed =
    bracket.Rounds
    |> Seq.collect (fun r -> r.Matches |> Seq.collect (fun m -> m.Games))
    |> Seq.length
  let gamesPlayedCount = if bracketGamesPlayed > 0 then bracketGamesPlayed else gamesAlreadyPlayed.Length
  tourny.CurrentGameNr <- gamesPlayedCount
  gameNr <- gamesPlayedCount

  let playPairing (pair: Pairing) = async {
    if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
      searchReplayList pair
    tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
    if cts.IsCancellationRequested then
      sb.Clear() |> ignore
      return Result.Empty
    else
      // Setup board and log opening info
      let openingMoves = setupBoardForGame board pair epdBook tourny.Opening.OpeningsPly (fun v -> tourny.IsChess960 <- v)
      logOpeningInfo logger pair openingMoves
      logPosition logger board

      // Engine creation with reuse logic
      if numberOfPlayers > 2 then
        engine1 <- EngineHelper.createEngine (pair.White, Some logger)
        engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
      if engine1 = Unchecked.defaultof<ChessEngine> || engine2 = Unchecked.defaultof<ChessEngine> then
        engine1 <- EngineHelper.createEngine (pair.White, Some logger)
        engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
      if engine1.Name = pair.Black.Name || engine2.Name = pair.White.Name then
        let (eng1,eng2) = engine2, engine1
        engine1 <- eng1
        engine2 <- eng2

      // Compute round text (cup uses liveGamesByOpening dictionary)
      let openingsAlreadyPlayed = countOpeningsAlreadyPlayed gamesAlreadyPlayed pair.OpeningHash
      let liveGamesPlayed = if liveGamesByOpening.ContainsKey pair.OpeningHash then liveGamesByOpening.[pair.OpeningHash] else 0
      let roundTxt = computeRoundTextFromPairing pair openingsAlreadyPlayed liveGamesPlayed
      callback (Update.RoundNr roundTxt)

      // Execute game with exception handling
      let replayDictWhite = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.White.Name) else None
      let replayDictBlack = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.Black.Name) else None
      let result = executeGame tourny replayDictWhite replayDictBlack sb cts logger board engine1 engine2 pair tryGetUserAdjudication callback

      let isCancelled = result.Reason = ResultReason.Cancel
      let forceStopEngines = match result.Reason with | ResultReason.Disconnected _ -> true | _ -> false
      if not isCancelled then
        results <- result :: results

      if not isCancelled then
        // Process completed game: build metadata, add to replay, write PGN
        let gameData = buildGameMetadata tourny pair result roundTxt
        addToReplayList replayList tourny result gameData board.UciMovesPlayed
        let moveSection = sb.ToString()
        writeGameToPgnSimple pgnGameWriterAgent tourny gameData moveSection result cts
        if tourny.VerboseLogging then
          logger.LogInformation(gameMetadataSummary gameData)

      do! Async.Sleep 200

      cleanupEnginesConditional engine1 engine2 forceStopEngines numberOfPlayers cts
      board.ResetBoardState()
      if not isCancelled then
        gameNr <- gameNr + 1
        if liveGamesByOpening.ContainsKey pair.OpeningHash then
          liveGamesByOpening.[pair.OpeningHash] <- liveGamesByOpening.[pair.OpeningHash] + 1
        else
          liveGamesByOpening.[pair.OpeningHash] <- 1
        if gameNr % 2 = 0 then
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
      return result
      }

  let getRoundPlayers (round: CupRound) =
    round.Matches
    |> Seq.collect (fun m -> [ m.PlayerA; m.PlayerB ])
    |> Seq.filter (fun name -> not (String.IsNullOrWhiteSpace name) && not (name.Equals("TBD", StringComparison.OrdinalIgnoreCase)))
    |> Seq.distinct
    |> Seq.toList

  let initialRound =
    bracket.Rounds
    |> Seq.sortBy (fun r -> r.RoundNumber)
    |> Seq.tryFind (fun r -> r.Matches |> Seq.exists (fun m -> not m.IsDecided))
    |> Option.defaultValue (bracket.Rounds |> Seq.sortByDescending (fun r -> r.RoundNumber) |> Seq.tryHead |> Option.defaultValue { RoundNumber = 1; Matches = ResizeArray() })

  let currentRoundPlayers =
    let players = getRoundPlayers initialRound
    if players.IsEmpty then
      seededPlayers |> List.map (fun e -> e.Name)
    else
      players

  let currentPlayersFromNames names =
    names
    |> List.choose (fun name -> tourny.EngineSetup.Engines |> List.tryFind (fun e -> e.Name = name))

  let mutable currentPlayers = currentPlayersFromNames currentRoundPlayers
  let mutable roundNumber = initialRound.RoundNumber
  try
  while currentPlayers.Length > 1 && not cts.IsCancellationRequested do
    let pairs =
      currentPlayers
      |> List.chunkBySize 2
      |> List.choose (function | [a; b] -> Some (a, b) | _ -> None)
    let round =
      bracket.Rounds
      |> Seq.find (fun r -> r.RoundNumber = roundNumber)
    for matchIndex in 0..(pairs.Length - 1) do
      let (a, b) = pairs.[matchIndex]
      let matchInfo = round.Matches.[matchIndex]
      let updated =
        { matchInfo with
            PlayerA = a.Name
            PlayerB = b.Name
            PlayerARating = a.Rating
            PlayerBRating = b.Rating }
      round.Matches.[matchIndex] <- updated

    writeCupBracket cupBracketAgent bracket

    let winners = ResizeArray<EngineConfig>()
    for matchIndex in 0..(pairs.Length - 1) do
      let pair = pairs.[matchIndex]
      let matchInfo = round.Matches.[matchIndex]
      let playerA, playerB = pair
      if matchInfo.IsDecided then
        ()
      else
        let playedInMatch = matchInfo.Games.Count
        let gamesPerMatch = gamesPerMatchForRound roundNumber
        let mutable gamesRemaining = Math.Max(0, gamesPerMatch - playedInMatch)
        let mutable localOpeningIndex = playedInMatch / 2
        let matchOpenings =
          if uniquePerMatchOnly then
            if randomOpenings && openings.Length > 1 then
              if matchInfo.OpeningOrder.Count = 0 || matchInfo.OpeningOrder.Count < openings.Length then
                let order = [0 .. openings.Length - 1]
                let shuffled = order |> List.toArray
                Random.Shuffle(shuffled)
                matchInfo.OpeningOrder <- ResizeArray<int>(shuffled)
                writeCupBracket cupBracketAgent bracket
              matchInfo.OpeningOrder
              |> Seq.map (fun idx -> openings.[idx % openings.Length])
              |> Seq.toList
            else
              openings
          else
            globalOpenings
        let tryGetOpeningByHash (hash: string) =
          match matchOpenings |> List.tryFind (fun o ->
            Hash.computeOpeningHashFromGame o = hash) with
          | Some opening -> opening
          | None ->
              logger.LogWarning("Cup resume: opening hash {Hash} not found in opening book — using next available opening.", hash)
              getNextOpening matchOpenings localOpeningIndex

        if not matchInfo.IsDecided then
          if matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining then
            matchInfo.IsDecided <- true
            matchInfo.Winner <- Some matchInfo.PlayerA
            writeCupBracket cupBracketAgent bracket
          elif matchInfo.ScoreB > matchInfo.ScoreA + float gamesRemaining then
            matchInfo.IsDecided <- true
            matchInfo.Winner <- Some matchInfo.PlayerB
            writeCupBracket cupBracketAgent bracket
          elif gamesRemaining = 0 then
            if matchInfo.ScoreA > matchInfo.ScoreB then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.PlayerA
              writeCupBracket cupBracketAgent bracket
            elif matchInfo.ScoreB > matchInfo.ScoreA then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.PlayerB
              writeCupBracket cupBracketAgent bracket

        while matchInfo.IsDecided |> not && not cts.IsCancellationRequested do
          let tieBreakRound = gamesRemaining = 0
          if gamesRemaining = 0 then
            gamesRemaining <- 2
            tourny.TotalGames <- tourny.TotalGames + 2
            callback (Update.TotalNumberOfPairs tourny.TotalGames)
          let hasOddGame = matchInfo.Games.Count % 2 = 1
          let opening =
            if hasOddGame then
              let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
              tryGetOpeningByHash lastGame.OpeningHash
            else
              let usedHashes =
                matchInfo.Games
                |> Seq.map (fun g -> g.OpeningHash)
                |> Set.ofSeq
              let openingIndexForMatch =
                if usedHashes.Count < matchOpenings.Length then
                  PairingHelper.nextUnusedOpeningIndex usedHashes matchOpenings localOpeningIndex
                else
                  localOpeningIndex % matchOpenings.Length
              localOpeningIndex <- openingIndexForMatch + 1
              matchOpenings.[openingIndexForMatch]
          let openingHash = Hash.computeOpeningHashFromGame opening
          let playOrder =
            if hasOddGame then
              let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
              if lastGame.White = playerA.Name then
                [ (playerB, playerA) ]
              else
                [ (playerA, playerB) ]
            else
              [ (playerA, playerB); (playerB, playerA) ]
          let plannedPairings =
            PairingHelper.buildRemainingCupPairings
              matchInfo
              playerA
              playerB
              matchOpenings
              opening
              playOrder
              gamesRemaining
              localOpeningIndex
          callback (Update.PairingList plannedPairings)
          for (white, black) in playOrder do
            if matchInfo.IsDecided || gamesRemaining = 0 || cts.IsCancellationRequested then
              ()
            else
              let pairing =
                { Opening = opening
                  White = white
                  Black = black
                  GameNr = 0
                  RoundNr = $"{matchInfo.RoundNumber}.{matchInfo.Games.Count + 1}"
                  OpeningHash = openingHash }
              let! result = playPairing pairing
              if result.Reason <> ResultReason.Cancel then
                let game : CupGame =
                  { GameNr = gameNr
                    White = white.Name
                    Black = black.Name
                    OpeningId = opening.GameNumber.ToString()
                    OpeningHash = openingHash
                    Result = result.Result }
                matchInfo.Games.Add game
                let scoreWhite =
                  match result.Result with
                  | "1-0" -> 1.0
                  | "0-1" -> 0.0
                  | "1/2-1/2" -> 0.5
                  | _ -> 0.0
                let scoreBlack =
                  match result.Result with
                  | "1-0" -> 0.0
                  | "0-1" -> 1.0
                  | "1/2-1/2" -> 0.5
                  | _ -> 0.0
                if white.Name = matchInfo.PlayerA then
                  matchInfo.ScoreA <- matchInfo.ScoreA + scoreWhite
                  matchInfo.ScoreB <- matchInfo.ScoreB + scoreBlack
                else
                  matchInfo.ScoreA <- matchInfo.ScoreA + scoreBlack
                  matchInfo.ScoreB <- matchInfo.ScoreB + scoreWhite
                gamesRemaining <- gamesRemaining - 1
                if matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining then
                  matchInfo.IsDecided <- true
                  matchInfo.Winner <- Some matchInfo.PlayerA
                elif matchInfo.ScoreB > matchInfo.ScoreA + float gamesRemaining then
                  matchInfo.IsDecided <- true
                  matchInfo.Winner <- Some matchInfo.PlayerB
                elif gamesRemaining = 0 then
                  if matchInfo.ScoreA > matchInfo.ScoreB then
                    matchInfo.IsDecided <- true
                    matchInfo.Winner <- Some matchInfo.PlayerA
                  elif matchInfo.ScoreB > matchInfo.ScoreA then
                    matchInfo.IsDecided <- true
                    matchInfo.Winner <- Some matchInfo.PlayerB
                // Adjust total for unplayed games when match decided early
                if matchInfo.IsDecided && gamesRemaining > 0 then
                  tourny.TotalGames <- tourny.TotalGames - gamesRemaining
                  callback (Update.TotalNumberOfPairs tourny.TotalGames)
                writeCupBracket cupBracketAgent bracket
      match matchInfo.Winner with
      | Some name when name = playerA.Name ->
          winners.Add playerA
          if roundNumber < totalRounds then
            ensureRound (roundNumber + 1)
            let nextRound = bracket.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber + 1)
            match nextRound with
            | Some next ->
                let nextIndex = matchIndex / 2
                let nextMatch = next.Matches.[nextIndex]
                let updated =
                  if matchIndex % 2 = 0 then
                    { nextMatch with PlayerA = playerA.Name; PlayerARating = playerA.Rating }
                  else
                    { nextMatch with PlayerB = playerA.Name; PlayerBRating = playerA.Rating }
                next.Matches.[nextIndex] <- updated
                writeCupBracket cupBracketAgent bracket
            | None -> ()
      | Some name when name = playerB.Name ->
          winners.Add playerB
          if roundNumber < totalRounds then
            ensureRound (roundNumber + 1)
            let nextRound = bracket.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber + 1)
            match nextRound with
            | Some next ->
                let nextIndex = matchIndex / 2
                let nextMatch = next.Matches.[nextIndex]
                let updated =
                  if matchIndex % 2 = 0 then
                    { nextMatch with PlayerA = playerB.Name; PlayerARating = playerB.Rating }
                  else
                    { nextMatch with PlayerB = playerB.Name; PlayerBRating = playerB.Rating }
                next.Matches.[nextIndex] <- updated
                writeCupBracket cupBracketAgent bracket
            | None -> ()
      | _ -> ()

    currentPlayers <- winners |> Seq.toList
    roundNumber <- roundNumber + 1

  let res = ResizeArray<Result>(results)
  callback (Update.PeriodicResults res)
  if ownsAgent then
    pgnGameWriterAgent.Post(FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
  cupBracketAgent.Post DisposeCupBracket
  return results
  finally
    if engine1 <> Unchecked.defaultof<ChessEngine> then
      cleanupEngines engine1 engine2
}

let swiss (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  let resolveSwissPath () =
    let configuredPath =
      if obj.ReferenceEquals(tourny.SwissOptions, null) then "" else tourny.SwissOptions.StatePath
    let fullPath =
      if String.IsNullOrWhiteSpace configuredPath then
        let candidates =
          [ Path.Combine(Environment.CurrentDirectory, "wwwroot")
            Path.Combine(Environment.CurrentDirectory, "WebGUI", "wwwroot") ]
        let folder =
          candidates |> List.tryFind Directory.Exists
          |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "wwwroot"))
        Path.Combine(folder, "swiss_state.json")
      elif Path.IsPathRooted configuredPath then
        configuredPath
      else
        Path.Combine(Environment.CurrentDirectory, configuredPath)
    let dir = Path.GetDirectoryName(fullPath)
    if String.IsNullOrWhiteSpace dir |> not then
      Directory.CreateDirectory(dir) |> ignore
    fullPath

  let writeSwissState (agent: MailboxProcessor<SwissStateMessage>) (state: SwissTypes.SwissState) =
    let maxPersistedOpenings = 50
    let trimOrder (order: ResizeArray<int>) =
      if obj.ReferenceEquals(order, null) then
        order
      else
        ResizeArray<int>(order |> Seq.truncate maxPersistedOpenings)
    let trimmedRounds =
      state.Rounds
      |> Seq.map (fun r ->
          let trimmedPairings =
            r.Pairings
            |> Seq.map (fun p ->
                { p with OpeningOrder = trimOrder p.OpeningOrder })
            |> ResizeArray
          { r with Pairings = trimmedPairings })
      |> ResizeArray
    let trimmedState =
      { state with
          GlobalOpeningOrder = trimOrder state.GlobalOpeningOrder
          Rounds = trimmedRounds }
    agent.PostAndReply(fun reply -> WriteSwissState(trimmedState, reply))
    callback SwissStateUpdated

  let gamesPerMatchForRound _ =
    let gamesPerMatch = tourny.SwissOptions.GamesPerMatch
    if gamesPerMatch < 2 then 2
    elif gamesPerMatch % 2 = 1 then gamesPerMatch + 1
    else gamesPerMatch

  let mutable gameNr = 0
  logger.LogInformation("Swiss tournament about to start")
  let numberOfPlayers = tourny.EngineSetup.Engines.Length
  if numberOfPlayers % 2 = 1 then
    logger.LogInformation("Swiss tournament has an odd number of players; a bye will be assigned each round.")

  let board = Board()
  board.LoadFen Chess.startPos
  let mutable results = List.empty<Result>

  // Load openings, games already played, and reference games using helpers
  let (games, epdBook) = loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds
  let openings = games |> Seq.toList
  if openings.IsEmpty then
    logger.LogError("No openings available for Swiss tournament")
    failwith "No openings available for Swiss tournament."

  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referencGamesPlayed = loadReferenceGames tourny.ReferencePGNPath

  let pgnGameWriterAgent, ownsAgent =
    match pgnAgent with
    | Some a -> a, false
    | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
  let replayList = ResizeArray<GameReplay>()
  let replayDicts = createReplayDicts tourny.EngineSetup.Engines
  let getReplayDictForPlayer (name:string) = replayDicts.[name]

  let swissPath = resolveSwissPath ()
  let swissAgent = TournamentState.startSwissStateReaderWriter swissPath
  let mutable openingIndex = 0
  let mutable pairId = 1

  let ensurePairingOpeningOrder (pairing: SwissTypes.SwissPairing) =
    if obj.ReferenceEquals(pairing.OpeningOrder, null) then
      pairing.OpeningOrder <- ResizeArray<int>()

  let ensureGlobalOpeningOrderFn (state: SwissTypes.SwissState) =
    if obj.ReferenceEquals(state.GlobalOpeningOrder, null) then
      state.GlobalOpeningOrder <- ResizeArray<int>()

  let mutable state : SwissTypes.SwissState =
    match swissAgent.PostAndReply(fun reply -> ReadSwissState reply) with
    | Some loaded ->
        ensureGlobalOpeningOrderFn loaded
        for round in loaded.Rounds do
          for pairing in round.Pairings do
            ensurePairingOpeningOrder pairing
        openingIndex <- loaded.NextOpeningIndex
        pairId <- loaded.Rounds |> Seq.collect (fun r -> r.Pairings |> Seq.map (fun p -> p.PairId)) |> Seq.append [0] |> Seq.max |> (+) 1
        loaded
    | None ->
        { TournamentName = tourny.Name
          SeedGroupCount = tourny.SwissOptions.SeedGroupCount
          GamesPerMatch = tourny.SwissOptions.GamesPerMatch
          UniqueOpeningsGlobal = not tourny.SwissOptions.UniquePerMatchOnly
          NextOpeningIndex = openingIndex
          GlobalOpeningOrder = ResizeArray<int>()
          Rounds = ResizeArray<SwissTypes.SwissRound>()
          UpdatedUtc = DateTime.UtcNow }

  let maxRounds = Math.Max(1, numberOfPlayers - 1)
  let configuredRounds = if tourny.SwissOptions.Rounds > 0 then tourny.SwissOptions.Rounds else tourny.Rounds
  let totalRounds = min configuredRounds maxRounds
  let maxPairs = (numberOfPlayers * (numberOfPlayers - 1)) / 2
  if configuredRounds > maxRounds then
    logger.LogInformation("Swiss rounds capped to {MaxRounds} based on {Players} players.", maxRounds, numberOfPlayers)
  let totalGames =
    [ 1 .. totalRounds ]
    |> List.sumBy (fun roundNumber ->
        let matchCount = numberOfPlayers / 2
        matchCount * gamesPerMatchForRound roundNumber)
  tourny.TotalGames <- totalGames
  callback (Update.TotalNumberOfPairs totalGames)
  let (tTime, gTime) = estimateTournamentAndGameTime totalGames tourny []
  let startInfo = { NumberOfGames = totalGames; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny }
  callback (Update.StartOfTournament startInfo)

  let seedOrder = PairingHelper.tcecSeedOrder tourny.EngineSetup.Engines tourny.SwissOptions.SeedGroupCount
  let seedMap =
    seedOrder
    |> List.mapi (fun idx p -> p.Name, idx + 1)
    |> Map.ofList

  let buildScores () =
    let scores = Dictionary<string, float>(StringComparer.OrdinalIgnoreCase)
    for p in tourny.EngineSetup.Engines do
      scores.[p.Name] <- 0.0
    for round in state.Rounds do
      for pairing in round.Pairings do
        if scores.ContainsKey pairing.PlayerA then
          scores.[pairing.PlayerA] <- scores.[pairing.PlayerA] + pairing.ScoreA
        if scores.ContainsKey pairing.PlayerB then
          scores.[pairing.PlayerB] <- scores.[pairing.PlayerB] + pairing.ScoreB
    scores |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

  let buildPriorPairs () =
    let pairs = ResizeArray<string>()
    for round in state.Rounds do
      for pairing in round.Pairings do
        if String.IsNullOrWhiteSpace pairing.PlayerA |> not
           && String.IsNullOrWhiteSpace pairing.PlayerB |> not
           && pairing.PlayerB <> "BYE" then
          pairs.Add(PairingHelper.swissPairKey pairing.PlayerA pairing.PlayerB)
    pairs |> Set.ofSeq

  let buildByeSet () =
    state.Rounds
    |> Seq.collect (fun r -> r.Pairings)
    |> Seq.filter (fun p -> p.PlayerB = "BYE")
    |> Seq.map (fun p -> p.PlayerA)
    |> Set.ofSeq

  let ensureGlobalOrder () =
    if tourny.SwissOptions.RandomOpenings && openings.Length > 1 && not tourny.SwissOptions.UniquePerMatchOnly then
      if state.GlobalOpeningOrder.Count = 0 || state.GlobalOpeningOrder.Count < openings.Length then
        let order = [0 .. openings.Length - 1]
        let shuffled = order |> List.toArray
        Random.Shuffle(shuffled)
        state.GlobalOpeningOrder <- ResizeArray<int>(shuffled)
        writeSwissState swissAgent state

  let globalOpenings =
    ensureGlobalOrder ()
    if state.GlobalOpeningOrder.Count > 0 then
      state.GlobalOpeningOrder
      |> Seq.map (fun idx -> openings.[idx % openings.Length])
      |> Seq.toList
    else
      openings

  let getNextOpening (openingsList: PGNTypes.PgnGame list) (localIndex: int) =
    if tourny.SwissOptions.UniquePerMatchOnly then
      openingsList.[localIndex % openingsList.Length]
    else
      if openingIndex >= openingsList.Length then
        openingIndex <- 0
      let opening = openingsList.[openingIndex]
      openingIndex <- openingIndex + 1
      state.NextOpeningIndex <- openingIndex
      opening

  let getOpeningHash (opening: PGNTypes.PgnGame) = Hash.computeOpeningHashFromGame opening

  let searchReplayList (pairing : Pairing) =
    searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

  let sb = StringBuilder()
  let stateGamesPlayed =
    state.Rounds
    |> Seq.collect (fun r -> r.Pairings |> Seq.collect (fun p -> p.Games))
    |> Seq.length
  let gamesPlayedCount = if stateGamesPlayed > 0 then stateGamesPlayed else gamesAlreadyPlayed.Length
  tourny.CurrentGameNr <- gamesPlayedCount
  gameNr <- gamesPlayedCount

  let playPairing (pair: Pairing) = async {
    if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
      searchReplayList pair
    tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
    if cts.IsCancellationRequested then
      sb.Clear() |> ignore
      return Result.Empty
    else
      // Compute round text
      let openingsAlreadyPlayed = countOpeningsAlreadyPlayed gamesAlreadyPlayed pair.OpeningHash
      let roundTxt = computeRoundTextFromPairing pair openingsAlreadyPlayed 0

      // Execute game with full setup using helper
      let result = executeGameWithSetup logger tourny board pair epdBook sb cts replayDicts replayList pgnGameWriterAgent tryGetUserAdjudication callback roundTxt
      results <- result :: results

      board.ResetBoardState()
      gameNr <- gameNr + 1
      if gameNr % 2 = 0 then
        let res = ResizeArray<Result>(results)
        callback (Update.PeriodicResults res)
      return result
  }

  let roundToStart =
    state.Rounds
    |> Seq.sortBy (fun r -> r.RoundNumber)
    |> Seq.tryFind (fun r -> r.Pairings |> Seq.exists (fun p -> not p.IsDecided))
    |> Option.map (fun r -> r.RoundNumber)
    |> Option.defaultValue (state.Rounds.Count + 1)

  let runRound (roundNumber: int) (roundPairs: (EngineConfig * EngineConfig) list) = async {
    let round : SwissTypes.SwissRound =
      match state.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber) with
      | Some existing -> existing
      | None ->
          let roundPairings = ResizeArray<SwissTypes.SwissPairing>()
          for (a, b) in roundPairs do
            let pairing : SwissTypes.SwissPairing =
              { PairId = pairId
                RoundNumber = roundNumber
                PlayerA = a.Name
                PlayerB = b.Name
                PlayerARating = a.Rating
                PlayerBRating = b.Rating
                ScoreA = if b.Name = "BYE" then 1.0 else 0.0
                ScoreB = 0.0
                IsDecided = b.Name = "BYE"
                Games = ResizeArray<SwissTypes.SwissGame>()
                OpeningOrder = ResizeArray<int>() }
            pairId <- pairId + 1
            roundPairings.Add pairing
          let newRound : SwissTypes.SwissRound =
            { RoundNumber = roundNumber
              Pairings = roundPairings }
          state.Rounds.Add newRound
          writeSwissState swissAgent state
          newRound

    let gamesPerMatch = gamesPerMatchForRound roundNumber
    let plannedPairings = ResizeArray<Pairing>()
    let mutable previewIndex = openingIndex
    for pairing in round.Pairings do
      if pairing.IsDecided || pairing.PlayerB = "BYE" then
        ()
      else
      ensurePairingOpeningOrder pairing
      let matchOpenings =
        if tourny.SwissOptions.UniquePerMatchOnly then
          if tourny.SwissOptions.RandomOpenings && openings.Length > 1 then
            if pairing.OpeningOrder.Count = 0 || pairing.OpeningOrder.Count < openings.Length then
              let order = [0 .. openings.Length - 1]
              let shuffled = order |> List.toArray
              Random.Shuffle(shuffled)
              pairing.OpeningOrder <- ResizeArray<int>(shuffled)
              writeSwissState swissAgent state
            pairing.OpeningOrder
            |> Seq.map (fun idx -> openings.[idx % openings.Length])
            |> Seq.toList
          else
            openings
        else
          globalOpenings
      let seedA = seedMap.[pairing.PlayerA]
      let seedB = seedMap.[pairing.PlayerB]
      let firstWhite, firstBlack =
        if seedA <= seedB then
          pairing.PlayerA, pairing.PlayerB
        else
          pairing.PlayerB, pairing.PlayerA
      let whiteFirst = tourny.EngineSetup.Engines |> List.find (fun e -> e.Name = firstWhite)
      let blackFirst = tourny.EngineSetup.Engines |> List.find (fun e -> e.Name = firstBlack)
      if tourny.SwissOptions.UniquePerMatchOnly then
        PairingHelper.addPlannedPairings plannedPairings whiteFirst blackFirst matchOpenings gamesPerMatch 0
        |> ignore
      else
        previewIndex <- PairingHelper.addPlannedPairings plannedPairings whiteFirst blackFirst matchOpenings gamesPerMatch previewIndex
    callback (Update.PairingList plannedPairings)

    for pairIndex in 0 .. round.Pairings.Count - 1 do
      let pairing = round.Pairings.[pairIndex]
      if pairing.PlayerB = "BYE" then
        if pairing.IsDecided |> not then
          pairing.ScoreA <- 1.0
          pairing.ScoreB <- 0.0
          pairing.IsDecided <- true
          writeSwissState swissAgent state
      else
      let pairingPlayers =
        tourny.EngineSetup.Engines
        |> List.filter (fun e -> e.Name = pairing.PlayerA || e.Name = pairing.PlayerB)
      if pairingPlayers.Length = 2 then
        let playerA = pairingPlayers |> List.find (fun e -> e.Name = pairing.PlayerA)
        let playerB = pairingPlayers |> List.find (fun e -> e.Name = pairing.PlayerB)
        let matchOpenings =
          if tourny.SwissOptions.UniquePerMatchOnly then
            if tourny.SwissOptions.RandomOpenings && openings.Length > 1 then
              if pairing.OpeningOrder.Count = 0 then
                let order = [0 .. openings.Length - 1]
                let shuffled = order |> List.toArray
                Random.Shuffle(shuffled)
                pairing.OpeningOrder <- ResizeArray<int>(shuffled)
                writeSwissState swissAgent state
              pairing.OpeningOrder
              |> Seq.map (fun idx -> openings.[idx % openings.Length])
              |> Seq.toList
            else
              openings
          else
            globalOpenings
        let seedA = seedMap.[pairing.PlayerA]
        let seedB = seedMap.[pairing.PlayerB]
        let firstWhite, firstBlack =
          if seedA <= seedB then
            playerA, playerB
          else
            playerB, playerA
        let tryGetOpeningByHash (hash: string) =
          match matchOpenings |> List.tryFind (fun o -> getOpeningHash o = hash) with
          | Some opening -> opening
          | None ->
              logger.LogWarning("Swiss resume: opening hash {Hash} not found in opening book — using next available opening.", hash)
              if tourny.SwissOptions.UniquePerMatchOnly then
                matchOpenings.[(pairing.Games.Count / 2) % matchOpenings.Length]
              else
                matchOpenings.[openingIndex % matchOpenings.Length]
        let nextOpening (localIndex: int ref) =
          if tourny.SwissOptions.UniquePerMatchOnly then
            let opening = matchOpenings.[(!localIndex) % matchOpenings.Length]
            localIndex := !localIndex + 1
            opening
          else
            if openingIndex >= matchOpenings.Length then
              openingIndex <- 0
            let opening = matchOpenings.[openingIndex]
            openingIndex <- openingIndex + 1
            state.NextOpeningIndex <- openingIndex
            opening
        let mutable gamesRemaining = Math.Max(0, gamesPerMatch - pairing.Games.Count)
        let localOpeningIndex = ref (pairing.Games.Count / 2)
        while gamesRemaining > 0 && not cts.IsCancellationRequested do
          let hasOddGame = pairing.Games.Count % 2 = 1
          let opening =
            if hasOddGame then
              let lastGame = pairing.Games.[pairing.Games.Count - 1]
              tryGetOpeningByHash lastGame.OpeningHash
            else
              nextOpening localOpeningIndex
          let openingHash = getOpeningHash opening
          let playOrder =
            if hasOddGame then
              let lastGame = pairing.Games.[pairing.Games.Count - 1]
              if lastGame.White = playerA.Name then
                [ (playerB, playerA) ]
              else
                [ (playerA, playerB) ]
            else
              [ (firstWhite, firstBlack); (firstBlack, firstWhite) ]
          for (white, black) in playOrder do
            if gamesRemaining = 0 || cts.IsCancellationRequested then
              ()
            else
              let roundGameNumber = (pairIndex * gamesPerMatch) + pairing.Games.Count + 1
              let pairingGame =
                { Opening = opening
                  White = white
                  Black = black
                  GameNr = 0
                  RoundNr = $"{pairing.RoundNumber}.{roundGameNumber}"
                  OpeningHash = openingHash }
              let! result = playPairing pairingGame
              if result.Reason <> ResultReason.Cancel then
                let game : SwissTypes.SwissGame =
                  { GameNr = gameNr
                    White = white.Name
                    Black = black.Name
                    OpeningId = opening.GameNumber.ToString()
                    OpeningHash = openingHash
                    Result = result.Result }
                pairing.Games.Add game
                let scoreWhite =
                  match result.Result with
                  | "1-0" -> 1.0
                  | "0-1" -> 0.0
                  | "1/2-1/2" -> 0.5
                  | _ -> 0.0
                let scoreBlack =
                  match result.Result with
                  | "1-0" -> 0.0
                  | "0-1" -> 1.0
                  | "1/2-1/2" -> 0.5
                  | _ -> 0.0
                if white.Name = pairing.PlayerA then
                  pairing.ScoreA <- pairing.ScoreA + scoreWhite
                  pairing.ScoreB <- pairing.ScoreB + scoreBlack
                else
                  pairing.ScoreA <- pairing.ScoreA + scoreBlack
                  pairing.ScoreB <- pairing.ScoreB + scoreWhite
                gamesRemaining <- gamesRemaining - 1
                if pairing.Games.Count >= gamesPerMatch then
                  pairing.IsDecided <- true
                  plannedPairings.RemoveAll(fun p ->
                    (p.White.Name = pairing.PlayerA || p.White.Name = pairing.PlayerB) &&
                    (p.Black.Name = pairing.PlayerA || p.Black.Name = pairing.PlayerB)) |> ignore
                  callback (Update.PairingList plannedPairings)
                writeSwissState swissAgent state
          if hasOddGame && tourny.SwissOptions.UniquePerMatchOnly then
            localOpeningIndex := !localOpeningIndex + 1
          if gamesRemaining > 0 && not cts.IsCancellationRequested then
            do! Async.Sleep(tourny.DelayBetweenGames.TotalMilliseconds |> int)
  }

  let mutable roundNumber = roundToStart
  let mutable continueRounds = true
  while continueRounds && roundNumber <= totalRounds && not cts.IsCancellationRequested do
    let scores = buildScores ()
    let priorPairs = buildPriorPairs ()
    if priorPairs.Count >= maxPairs then
      logger.LogInformation("Swiss tournament completed: all unique pairs have been played.")
      continueRounds <- false
    else
      let byeSet = buildByeSet ()
      let roundPairs : (EngineConfig * EngineConfig) list =
        PairingHelper.swissRoundPairings tourny.EngineSetup.Engines seedOrder scores priorPairs byeSet
      do! runRound roundNumber roundPairs
      roundNumber <- roundNumber + 1

  if tourny.SwissOptions.AllowExtraPairsOnTie && not cts.IsCancellationRequested then
    // Sonneborn-Berger tiebreak: for each player, sum over all opponents of
    // (points scored against that opponent) × (opponent's total score).
    let computeSBScores (scores: Map<string, float>) =
      let sb = Dictionary<string, float>(StringComparer.OrdinalIgnoreCase)
      for p in tourny.EngineSetup.Engines do
        sb.[p.Name] <- 0.0
      for round in state.Rounds do
        for pairing in round.Pairings do
          if pairing.PlayerB <> "BYE" then
            let oppAScore = scores |> Map.tryFind pairing.PlayerB |> Option.defaultValue 0.0
            let oppBScore = scores |> Map.tryFind pairing.PlayerA |> Option.defaultValue 0.0
            if sb.ContainsKey pairing.PlayerA then
              sb.[pairing.PlayerA] <- sb.[pairing.PlayerA] + pairing.ScoreA * oppAScore
            if sb.ContainsKey pairing.PlayerB then
              sb.[pairing.PlayerB] <- sb.[pairing.PlayerB] + pairing.ScoreB * oppBScore
      sb |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

    let rec resolveTieBreak tieRoundNumber = async {
      let scores = buildScores ()
      if scores.IsEmpty then
        return ()
      else
        let maxScore = scores |> Seq.maxBy (fun kvp -> kvp.Value) |> fun kvp -> kvp.Value
        let tied =
          scores
          |> Seq.filter (fun kvp -> kvp.Value = maxScore)
          |> Seq.map (fun kvp -> kvp.Key)
          |> Seq.toList
        if tied.Length <= 1 then
          return ()
        elif tied.Length > 2 then
          let sbScores = computeSBScores scores
          let ranked =
            tied
            |> List.sortByDescending (fun name -> sbScores |> Map.tryFind name |> Option.defaultValue 0.0)
          logger.LogInformation(
            "Swiss tiebreak: {Count} players tied at {Score}. Resolved by Sonneborn-Berger: {Ranking}",
            tied.Length, maxScore,
            ranked |> List.map (fun n -> sprintf "%s (SB=%.1f)" n (sbScores |> Map.tryFind n |> Option.defaultValue 0.0)) |> String.concat ", ")
          return ()
        else
          // Exactly 2 players tied — playoff: keep playing until one wins a match.
          let tiedPlayers =
            tourny.EngineSetup.Engines
            |> List.filter (fun p -> tied |> List.contains p.Name)
          logger.LogInformation("Swiss tiebreak: 2 players tied at {Score}, playing playoff match.", maxScore)
          let byeSet = buildByeSet ()
          let roundPairs : (EngineConfig * EngineConfig) list =
            PairingHelper.swissRoundPairings tiedPlayers seedOrder scores Set.empty byeSet
          let extraGames = roundPairs.Length * (gamesPerMatchForRound tieRoundNumber)
          tourny.TotalGames <- tourny.TotalGames + extraGames
          callback (Update.TotalNumberOfPairs tourny.TotalGames)
          do! runRound tieRoundNumber roundPairs
          return! resolveTieBreak (tieRoundNumber + 1)
    }
    do! resolveTieBreak roundNumber

  let res = ResizeArray<Result>(results)
  callback (Update.PeriodicResults res)
  if ownsAgent then
    pgnGameWriterAgent.Post(FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
  swissAgent.Post DisposeSwissState
  return results
}

let ladder (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  let resolveLadderPath () =
    let configuredPath =
      if obj.ReferenceEquals(tourny.LadderOptions, null) then "" else tourny.LadderOptions.StatePath
    let fullPath =
      if String.IsNullOrWhiteSpace configuredPath then
        let candidates =
          [ Path.Combine(Environment.CurrentDirectory, "wwwroot")
            Path.Combine(Environment.CurrentDirectory, "WebGUI", "wwwroot") ]
        let folder =
          candidates |> List.tryFind Directory.Exists
          |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "wwwroot"))
        Path.Combine(folder, "ladder_state.json")
      elif Path.IsPathRooted configuredPath then
        configuredPath
      else
        Path.Combine(Environment.CurrentDirectory, configuredPath)
    let dir = Path.GetDirectoryName(fullPath)
    if String.IsNullOrWhiteSpace dir |> not then
      Directory.CreateDirectory(dir) |> ignore
    fullPath

  let writeLadderState (agent: MailboxProcessor<TournamentTypes.LadderStateMessage>) (state: LadderState) =
    agent.PostAndReply(fun reply -> TournamentTypes.WriteLadderState(state, reply))
    callback Update.LadderStateUpdated

  let gamesPerMatch =
    let pairs = if obj.ReferenceEquals(tourny.LadderOptions, null) then 4 else tourny.LadderOptions.GamePairsPerMatch
    max 1 pairs * 2

  let mutable gameNr = 0
  logger.LogInformation("Ladder tournament about to start")
  let numberOfPlayers = tourny.EngineSetup.Engines.Length
  if numberOfPlayers < 2 then
    logger.LogError("Ladder tournaments require at least 2 engines, got {playerCount}", numberOfPlayers)
    failwith "Ladder tournaments require at least 2 engines."

  let board = Board()
  board.LoadFen Chess.startPos
  let mutable results = List.empty<Result>

  let (games, epdBook) = loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds
  let openings = games |> Seq.toList
  if openings.IsEmpty then
    logger.LogError("No openings available for Ladder tournament")
    failwith "No openings available for Ladder tournament."
  let randomOpenings = if obj.ReferenceEquals(tourny.LadderOptions, null) then false else tourny.LadderOptions.RandomOpenings

  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referencGamesPlayed = loadReferenceGames tourny.ReferencePGNPath

  let pgnGameWriterAgent, ownsAgent =
    match pgnAgent with
    | Some a -> a, false
    | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
  let replayList = ResizeArray<GameReplay>()
  let replayDicts = createReplayDicts tourny.EngineSetup.Engines

  let ladderPath = resolveLadderPath ()
  let ladderAgent = TournamentState.startLadderStateReaderWriter ladderPath
  let mutable openingIndex = 0
  let mutable matchId = 1

  let ensureGlobalOpeningOrder (state: LadderState) =
    if obj.ReferenceEquals(state.GlobalOpeningOrder, null) then
      state.GlobalOpeningOrder <- ResizeArray<int>()

  let mutable state : LadderState =
    match ladderAgent.PostAndReply(fun reply -> TournamentTypes.ReadLadderState reply) with
    | Some loaded ->
        ensureGlobalOpeningOrder loaded
        openingIndex <- loaded.NextOpeningIndex
        matchId <- (loaded.Matches |> Seq.map (fun m -> m.MatchId) |> Seq.append [0] |> Seq.max) + 1
        loaded
    | None ->
        let sorted =
          tourny.EngineSetup.Engines
          |> List.sortByDescending (fun e -> e.Rating)
          |> List.map (fun e -> e.Name)
        { TournamentName = tourny.Name
          GamePairsPerMatch = gamesPerMatch
          InitialRankings = ResizeArray<string>(sorted)
          SurvivingEngines = ResizeArray<string>(sorted)
          EliminatedEngines = ResizeArray<string>()
          CurrentClimbNumber = 1
          CurrentClimberIndex = sorted.Length - 1
          Matches = ResizeArray<LadderMatch>()
          NextOpeningIndex = openingIndex
          GlobalOpeningOrder = ResizeArray<int>()
          UpdatedUtc = DateTime.UtcNow }

  let totalMatches = numberOfPlayers - 1
  tourny.TotalGames <- totalMatches * gamesPerMatch

  // Adjust TotalGames for decided matches that played fewer/more games than scheduled
  // (early termination saves games, tiebreaks add games)
  for m in state.Matches do
      if m.IsDecided then
          let diff = m.Games.Count - gamesPerMatch
          if diff <> 0 then
              tourny.TotalGames <- tourny.TotalGames + diff

  callback (Update.TotalNumberOfPairs tourny.TotalGames)
  callback (Update.PairingList (ResizeArray<Pairing>()))
  let (tTime, gTime) = estimateTournamentAndGameTime tourny.TotalGames tourny []
  let startInfo = { NumberOfGames = tourny.TotalGames; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny }
  callback (Update.StartOfTournament startInfo)

  // Initialize global opening order for random openings
  if randomOpenings && openings.Length > 1 then
    if state.GlobalOpeningOrder.Count = 0 || state.GlobalOpeningOrder.Count < openings.Length then
      let order = [0 .. openings.Length - 1]
      let shuffled = order |> List.toArray
      Random.Shuffle(shuffled)
      state.GlobalOpeningOrder <- ResizeArray<int>(shuffled)
      writeLadderState ladderAgent state

  let globalOpenings =
    if state.GlobalOpeningOrder.Count > 0 then
      state.GlobalOpeningOrder
      |> Seq.map (fun idx -> openings.[idx % openings.Length])
      |> Seq.toList
    else
      openings

  let getNextOpening () =
    if openingIndex >= globalOpenings.Length then
      openingIndex <- 0
    let opening = globalOpenings.[openingIndex]
    openingIndex <- openingIndex + 1
    state.NextOpeningIndex <- openingIndex
    opening

  let searchReplayList (pairing : Pairing) =
    searchAndPrepareReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny

  let sb = StringBuilder()
  let stateGamesPlayed =
    state.Matches
    |> Seq.collect (fun m -> m.Games)
    |> Seq.length
  let gamesPlayedCount = if stateGamesPlayed > 0 then stateGamesPlayed else gamesAlreadyPlayed.Length
  tourny.CurrentGameNr <- gamesPlayedCount
  gameNr <- gamesPlayedCount

  let findEngine (name: string) =
    tourny.EngineSetup.Engines |> List.find (fun e -> e.Name = name)

  let playPairing (pair: Pairing) = async {
    if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
      searchReplayList pair
    tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
    if cts.IsCancellationRequested then
      sb.Clear() |> ignore
      return Result.Empty
    else
      let openingsAlreadyPlayed = countOpeningsAlreadyPlayed gamesAlreadyPlayed pair.OpeningHash
      let roundTxt = computeRoundTextFromPairing pair openingsAlreadyPlayed 0
      let result = executeGameWithSetup logger tourny board pair epdBook sb cts replayDicts replayList pgnGameWriterAgent tryGetUserAdjudication callback roundTxt
      results <- result :: results
      board.ResetBoardState()
      gameNr <- gameNr + 1
      if gameNr % 2 = 0 then
        let res = ResizeArray<Result>(results)
        callback (Update.PeriodicResults res)
      return result
  }

  let printLadderStandings (climbInfo: string) =
    printfn ""
    printfn "%s" climbInfo
    printfn "Current Ladder:"
    for i in 0 .. state.SurvivingEngines.Count - 1 do
      let name = state.SurvivingEngines.[i]
      let eng = findEngine name
      let marker =
        if i = state.CurrentClimberIndex && state.SurvivingEngines.Count > 1 then " <- climbing"
        else ""
      printfn "  %d. %s (Rating: %d)%s" (i + 1) name eng.Rating marker
    if state.EliminatedEngines.Count > 0 then
      printfn "Eliminated: %s" (state.EliminatedEngines |> Seq.rev |> String.concat ", ")
    printfn ""

  let playMiniMatch (matchInfo: LadderMatch) (challengerConfig: EngineConfig) (defenderConfig: EngineConfig) (startingGamesRemaining: int) = async {
    let mutable gamesRemaining = startingGamesRemaining
    while not matchInfo.IsDecided && not cts.IsCancellationRequested do
      if gamesRemaining = 0 then
        gamesRemaining <- 2
        tourny.TotalGames <- tourny.TotalGames + 2
        callback (Update.TotalNumberOfPairs tourny.TotalGames)
        printfn "  Tiebreak: scores tied %.1f-%.1f, playing 2 extra games" matchInfo.ScoreChallenger matchInfo.ScoreDefender
      let hasOddGame = matchInfo.Games.Count % 2 = 1
      let opening =
        if hasOddGame then
          let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
          match globalOpenings |> List.tryFind (fun o -> Hash.computeOpeningHashFromGame o = lastGame.OpeningHash) with
          | Some op -> op
          | None -> getNextOpening ()
        else
          getNextOpening ()
      let openingHash = Hash.computeOpeningHashFromGame opening
      let playOrder =
        if hasOddGame then
          let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
          if lastGame.White = challengerConfig.Name then
            [ (defenderConfig, challengerConfig) ]
          else
            [ (challengerConfig, defenderConfig) ]
        else
          [ (challengerConfig, defenderConfig); (defenderConfig, challengerConfig) ]
      // Build planned pairings for UI display
      let plannedPairings = ResizeArray<Pairing>()
      let mutable planIdx = 0
      let mutable planRemaining = gamesRemaining
      for (w, b) in playOrder do
        if planRemaining > 0 then
          plannedPairings.Add
            { Opening = opening
              White = w
              Black = b
              GameNr = 0
              RoundNr = $"{state.CurrentClimbNumber}.{matchInfo.Games.Count + planIdx + 1}"
              OpeningHash = openingHash }
          planIdx <- planIdx + 1
          planRemaining <- planRemaining - 1
      let mutable peekIndex = openingIndex
      while planRemaining > 0 do
        let peekOpening = globalOpenings.[peekIndex % globalOpenings.Length]
        peekIndex <- peekIndex + 1
        let peekHash = Hash.computeOpeningHashFromGame peekOpening
        for (w, b) in [ (challengerConfig, defenderConfig); (defenderConfig, challengerConfig) ] do
          if planRemaining > 0 then
            plannedPairings.Add
              { Opening = peekOpening
                White = w
                Black = b
                GameNr = 0
                RoundNr = $"{state.CurrentClimbNumber}.{matchInfo.Games.Count + planIdx + 1}"
                OpeningHash = peekHash }
            planIdx <- planIdx + 1
            planRemaining <- planRemaining - 1
      callback (Update.PairingList plannedPairings)
      for (white, black) in playOrder do
        if matchInfo.IsDecided || gamesRemaining = 0 || cts.IsCancellationRequested then
          ()
        else
          let pairing =
            { Opening = opening
              White = white
              Black = black
              GameNr = 0
              RoundNr = $"{state.CurrentClimbNumber}.{matchInfo.Games.Count + 1}"
              OpeningHash = openingHash }
          let! result = playPairing pairing
          if result.Reason <> ResultReason.Cancel then
            let game : LadderGame =
              { GameNr = gameNr
                White = white.Name
                Black = black.Name
                OpeningId = opening.GameNumber.ToString()
                OpeningHash = openingHash
                Result = result.Result }
            matchInfo.Games.Add game
            let scoreWhite =
              match result.Result with
              | "1-0" -> 1.0 | "0-1" -> 0.0 | "1/2-1/2" -> 0.5 | _ -> 0.0
            let scoreBlack =
              match result.Result with
              | "1-0" -> 0.0 | "0-1" -> 1.0 | "1/2-1/2" -> 0.5 | _ -> 0.0
            if white.Name = matchInfo.Challenger then
              matchInfo.ScoreChallenger <- matchInfo.ScoreChallenger + scoreWhite
              matchInfo.ScoreDefender <- matchInfo.ScoreDefender + scoreBlack
            else
              matchInfo.ScoreChallenger <- matchInfo.ScoreChallenger + scoreBlack
              matchInfo.ScoreDefender <- matchInfo.ScoreDefender + scoreWhite
            gamesRemaining <- gamesRemaining - 1
            // Early termination check
            if matchInfo.ScoreChallenger > matchInfo.ScoreDefender + float gamesRemaining then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.Challenger
            elif matchInfo.ScoreDefender > matchInfo.ScoreChallenger + float gamesRemaining then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.Defender
            elif gamesRemaining = 0 then
              if matchInfo.ScoreChallenger > matchInfo.ScoreDefender then
                matchInfo.IsDecided <- true
                matchInfo.Winner <- Some matchInfo.Challenger
              elif matchInfo.ScoreDefender > matchInfo.ScoreChallenger then
                matchInfo.IsDecided <- true
                matchInfo.Winner <- Some matchInfo.Defender
              // else: still tied, outer loop will add tiebreak pair
            // Adjust total for unplayed games when match decided early
            if matchInfo.IsDecided && gamesRemaining > 0 then
              tourny.TotalGames <- tourny.TotalGames - gamesRemaining
              callback (Update.TotalNumberOfPairs tourny.TotalGames)
            writeLadderState ladderAgent state
            // Delay between individual games within a match
            if gamesRemaining > 0 && not matchInfo.IsDecided && not cts.IsCancellationRequested then
              do! Async.Sleep(tourny.DelayBetweenGames.TotalMilliseconds |> int)
  }

  let processMatchResult (matchInfo: LadderMatch) =
    if matchInfo.IsDecided then
      let winnerName = matchInfo.Winner |> Option.defaultValue matchInfo.Defender
      let loserName = if winnerName = matchInfo.Challenger then matchInfo.Defender else matchInfo.Challenger
      state.EliminatedEngines.Add loserName
      state.SurvivingEngines.Remove loserName |> ignore
      let climbInfo = sprintf "=== Ladder Match %d (Climb %d) === [Challenger] %s vs [Defender] %s: %s wins %.1f-%.1f. %s eliminated."
                        matchInfo.MatchId state.CurrentClimbNumber matchInfo.Challenger matchInfo.Defender winnerName matchInfo.ScoreChallenger matchInfo.ScoreDefender loserName
      if winnerName = matchInfo.Challenger then
        // Challenger continues climbing — index shifts because defender was removed
        let idx = state.SurvivingEngines.IndexOf(winnerName)
        if idx < 0 then
          logger.LogWarning("Ladder: winner {Winner} not found in SurvivingEngines — restarting climb", winnerName)
          state.CurrentClimbNumber <- state.CurrentClimbNumber + 1
          state.CurrentClimberIndex <- state.SurvivingEngines.Count - 1
        elif idx = 0 then
          // Climber beat the #1 engine — start new climb from bottom
          state.CurrentClimbNumber <- state.CurrentClimbNumber + 1
          state.CurrentClimberIndex <- state.SurvivingEngines.Count - 1
        else
          state.CurrentClimberIndex <- idx
      else
        // Challenger eliminated, start new climb from bottom
        state.CurrentClimbNumber <- state.CurrentClimbNumber + 1
        state.CurrentClimberIndex <- state.SurvivingEngines.Count - 1
      writeLadderState ladderAgent state
      printLadderStandings climbInfo
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)

  // Resume: skip already-decided matches
  // Find the current match to resume or start fresh
  let currentUndecidedMatch =
    state.Matches |> Seq.tryFind (fun m -> not m.IsDecided)

  // Resume an undecided match if there is one
  match currentUndecidedMatch with
  | Some matchInfo ->
      let challengerConfig = findEngine matchInfo.Challenger
      let defenderConfig = findEngine matchInfo.Defender
      let remaining =
        let base' = gamesPerMatch - matchInfo.Games.Count
        if base' < 0 then
          // In tiebreak territory: finish the current pair if mid-pair
          if matchInfo.Games.Count % 2 = 1 then 1 else 0
        else
          base'
      do! playMiniMatch matchInfo challengerConfig defenderConfig remaining
      processMatchResult matchInfo
  | None -> ()

  // Main ladder loop
  while state.SurvivingEngines.Count > 1 && not cts.IsCancellationRequested do
    let climberIdx = state.CurrentClimberIndex
    let defenderIdx = climberIdx - 1
    if defenderIdx < 0 then
      // Climber is at the top — shouldn't happen, but handle gracefully
      state.CurrentClimbNumber <- state.CurrentClimbNumber + 1
      state.CurrentClimberIndex <- state.SurvivingEngines.Count - 1
      writeLadderState ladderAgent state
    else
      let challengerName = state.SurvivingEngines.[climberIdx]
      let defenderName = state.SurvivingEngines.[defenderIdx]
      let challengerConfig = findEngine challengerName
      let defenderConfig = findEngine defenderName

      let matchInfo : LadderMatch =
        { MatchId = matchId
          ClimbNumber = state.CurrentClimbNumber
          Challenger = challengerName
          Defender = defenderName
          ChallengerRating = challengerConfig.Rating
          DefenderRating = defenderConfig.Rating
          ScoreChallenger = 0.0
          ScoreDefender = 0.0
          Winner = None
          IsDecided = false
          Games = ResizeArray<LadderGame>() }
      matchId <- matchId + 1
      state.Matches.Add matchInfo
      writeLadderState ladderAgent state

      printfn ""
      printfn "=== Ladder Match %d (Climb %d) ===" matchInfo.MatchId state.CurrentClimbNumber
      printfn "[Challenger] %s (%d) vs [Defender] %s (%d)" challengerName challengerConfig.Rating defenderName defenderConfig.Rating

      do! playMiniMatch matchInfo challengerConfig defenderConfig gamesPerMatch
      if not cts.IsCancellationRequested then
        processMatchResult matchInfo

  // Final standings
  if state.SurvivingEngines.Count = 1 then
    let champion = state.SurvivingEngines.[0]
    printfn ""
    printfn "========================================="
    printfn "  LADDER CHAMPION: %s" champion
    printfn "========================================="
    printfn "Final standings:"
    printfn "  1. %s (Champion)" champion
    let mutable rank = 2
    for name in state.EliminatedEngines |> Seq.rev do
      printfn "  %d. %s (Eliminated)" rank name
      rank <- rank + 1
    printfn ""

  let res = ResizeArray<Result>(results)
  callback (Update.PeriodicResults res)
  if ownsAgent then
    pgnGameWriterAgent.Post(FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
  ladderAgent.Post TournamentTypes.DisposeLadderState
  return results
}
