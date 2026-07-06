module ChessLibrary.Tournament

open System
open System.IO
open System.Threading
open System.Threading.Channels
open System.Diagnostics
open Microsoft.Extensions.Logging
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PositionTypes
open ChessLibrary.MiscTypes
open ChessLibrary.Chess
open ChessLibrary.Configuration
open ChessLibrary.GameAnalysis
open ChessLibrary.ChessUtilities
open ChessLibrary.EngineProtocol
open ChessLibrary.TournamentPairing
open ChessLibrary.RuntimeUtilities
open ChessLibrary.TournamentTypes

module TournamentUtils =

  let validateEnginesInTournament (tourny : Tournament)  =
    async {
      Validation.validateTournamentInput tourny
      let mutable valid = tourny.EngineSetup.Engines.Length > 1
      for engConfig in tourny.EngineSetup.Engines do
        let engine = EngineHelper.createEngine (engConfig, None)
        if valid then
          valid <- engine.PassedValidation
          engine.PrintNonDefaultValues()
        engine.StopProcess()
        Async.Sleep(1000) |> ignore
      if valid then
        RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Green "\nTournament validation was successful"
      else
        RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red "\nTournament validation failed"
      return valid
    } |> Async.StartAsTask
  
module Manager =  

  let mutable cupResumeRequested = false
  let setCupResumeRequested value =
    cupResumeRequested <- value
  let consumeCupResumeRequested () =
    let value = cupResumeRequested
    cupResumeRequested <- false
    value
  let mutable cupBracketPathOverride : string option = None
  let setCupBracketPathOverride (path: string) =
    if String.IsNullOrWhiteSpace path then
      cupBracketPathOverride <- None
    else
      cupBracketPathOverride <- Some path
  let consumeCupBracketPathOverride () =
    let value = cupBracketPathOverride
    cupBracketPathOverride <- None
    value

  let mutable lastLoadError = ""

  let loadTournament () =
    lastLoadError <- ""
    try
        let path = DirectoryInfo(Environment.CurrentDirectory).FullName //.Parent.Parent.FullName
        let pathToTournamentJson = Path.Combine(path,"wwwroot","tournament.json")
        let tournyFromJson = JSON.readTournamentJson pathToTournamentJson
    
        let tournament = 
          match tournyFromJson with
          |Some tourny ->
            let tourny =           
              if tourny.EngineSetup.EngineDefList.Length > 0 then
                let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList            
                let isGauntlet = tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
                if isGauntlet && tourny.Challengers > 0 then
                  for engine in engineList |> List.truncate tourny.Challengers do
                    engine.IsChallenger <- true
                else
                  for engine in engineList do
                    engine.IsChallenger <- false
                let engineSetup = {tourny.EngineSetup with Engines = engineList}
                let cupDefaults = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
                let cupOptions = if obj.ReferenceEquals(tourny.CupOptions, null) then cupDefaults else tourny.CupOptions
                let tournamentMode =
                  if String.IsNullOrWhiteSpace tourny.TournamentMode then "RR" else tourny.TournamentMode
                let cupOptions =
                  match consumeCupBracketPathOverride () with
                  | Some overridePath -> { cupOptions with BracketPath = overridePath }
                  | None -> cupOptions
                let swissDefaults = { GamesPerMatch = 2; Rounds = tourny.Rounds; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
                let swissOptions =
                  let baseOptions = if obj.ReferenceEquals(tourny.SwissOptions, null) then swissDefaults else tourny.SwissOptions
                  let rounds = if baseOptions.Rounds > 0 then baseOptions.Rounds else tourny.Rounds
                  { baseOptions with Rounds = rounds }
                let ladderDefaults = { GamePairsPerMatch = 4; RandomOpenings = false; StatePath = "wwwroot/ladder_state.json" }
                let ladderOptions = if obj.ReferenceEquals(tourny.LadderOptions, null) then ladderDefaults else tourny.LadderOptions
                // Challengers is only used for Gauntlet mode; reset to 0 for other modes
                let challengers = if isGauntlet then tourny.Challengers else 0
                let updatedTourny = {tourny with EngineSetup = engineSetup; CupOptions = cupOptions; SwissOptions = swissOptions; LadderOptions = ladderOptions; TournamentMode = tournamentMode; Challengers = challengers }
                Validation.validateTournamentInput updatedTourny
                updatedTourny
              else 
                let cupDefaults = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
                let cupOptions = if obj.ReferenceEquals(tourny.CupOptions, null) then cupDefaults else tourny.CupOptions
                let tournamentMode =
                  if String.IsNullOrWhiteSpace tourny.TournamentMode then "RR" else tourny.TournamentMode
                let cupOptions =
                  match consumeCupBracketPathOverride () with
                  | Some overridePath -> { cupOptions with BracketPath = overridePath }
                  | None -> cupOptions
                let swissDefaults = { GamesPerMatch = 2; Rounds = tourny.Rounds; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
                let swissOptions =
                  let baseOptions = if obj.ReferenceEquals(tourny.SwissOptions, null) then swissDefaults else tourny.SwissOptions
                  let rounds = if baseOptions.Rounds > 0 then baseOptions.Rounds else tourny.Rounds
                  { baseOptions with Rounds = rounds }
                let ladderDefaults = { GamePairsPerMatch = 4; RandomOpenings = false; StatePath = "wwwroot/ladder_state.json" }
                let ladderOptions = if obj.ReferenceEquals(tourny.LadderOptions, null) then ladderDefaults else tourny.LadderOptions
                // Challengers is only used for Gauntlet mode; reset to 0 for other modes
                let isGauntlet = tournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
                let challengers = if isGauntlet then tourny.Challengers else 0
                { tourny with CupOptions = cupOptions; SwissOptions = swissOptions; LadderOptions = ladderOptions; TournamentMode = tournamentMode; Challengers = challengers }

            let openingPath = 
              match tourny.Opening.OpeningsPath with
              |Some path -> 
                if String.IsNullOrEmpty path then None else Some path
              |_ -> None
            let tourny = {tourny with Opening = {tourny.Opening with OpeningsPath = openingPath}}
            if tourny.MinMoveTimeInMS = 0 then
              { tourny with MinMoveTimeInMS = 300 }
            else
              tourny          
          |_ -> 
            ConsoleUtils.printInColor ConsoleColor.Red "Tournament json file not found!"
            failwith "Tournament json file not found!"
        tournament
    with exn ->
      let innerMsg = if exn.InnerException <> null then exn.InnerException.Message else exn.Message
      let msg = innerMsg
      ConsoleUtils.printInColor ConsoleColor.Red $"Error loading tournament.json: {msg}"
      lastLoadError <- msg
      Tournament.Empty
  
  let startTournament
    (cts:CancellationTokenSource)
    (tournament : Tournament)
    (logger:ILogger)
    sendResponse
    consoleMode
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) =
      logger.LogInformation (tournament.Summary())
      let timer = Stopwatch()
      timer.Start()
      let tourny =
        //let nodeLimit = tournament.EngineSetup.Engines |> List.map(fun e -> tournament.FindTimeControl e.TimeControlID) |> List.forall(fun e -> e.NodeLimit)
        if consoleMode then
          ParallelExecution.parallelTournamentRun logger tournament sendResponse cts pgnAgent
        else
          let mode =
            if String.IsNullOrWhiteSpace tournament.TournamentMode then "RR"
            else tournament.TournamentMode
          let modeNormalized = mode.Trim().ToLowerInvariant()
          let seeding =
            match tournament.CupOptions.SeedingStrategy with
            | null -> PairingHelper.CupSeedingStrategy.ByRating
            | s when s.Equals("random", StringComparison.OrdinalIgnoreCase) -> PairingHelper.CupSeedingStrategy.Random
            | _ -> PairingHelper.CupSeedingStrategy.ByRating
          match modeNormalized with
          | "gauntlet" ->
              TournamentRunners.gauntlet logger tournament sendResponse cts tryGetUserAdjudication pgnAgent
          | "cup" ->
              let resumeRequested = consumeCupResumeRequested ()
              TournamentRunners.cup seeding tournament.CupOptions.UniquePerMatchOnly resumeRequested logger tournament sendResponse cts tryGetUserAdjudication pgnAgent
          | "swiss" ->
              TournamentRunners.swiss logger tournament sendResponse cts tryGetUserAdjudication pgnAgent
          | "ladder" ->
              TournamentRunners.ladder logger tournament sendResponse cts tryGetUserAdjudication pgnAgent
          | "rr" | "roundrobin" | "round-robin" | _ ->
              TournamentRunners.roundRobin logger tournament sendResponse cts tryGetUserAdjudication pgnAgent            
      
      let mutable validationPassed = true
      //check for value head tests
      if tournament.TestOptions.ValueTest then
        //validate value tests
        for engineConfig in tournament.EngineSetup.Engines do
          let isLc0 = engineConfig.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
          match PuzzleEngineAnalysis.getPuzzleValueEngine engineConfig with
          |Some engine ->
            // Classify by the engine's self-declared UCI identity as well as path, so an
            // Lc0 at a path without "lc0" still gets its ValueOnly-enabled config swapped in.
            if isLc0 || engine.UciIdName.Contains("lc0", StringComparison.OrdinalIgnoreCase) then
                let conf = engine.Config
                tournament.EngineSetup.Engines <- tournament.EngineSetup.Engines |> List.map(fun e -> if e.Name = engineConfig.Name then conf else e)
            printfn "Value test engine found: %s" engine.Name
          |_ -> 
            validationPassed <- false //failwith "Value test engine not found"            
            if isLc0 then
              ConsoleUtils.printInColor ConsoleColor.Yellow $"The Lc0 binary used does not support value head testing: {engineConfig.Name}. Please make sure to use a binary that supports value head tests via an option called ValueOnly or by using command line argument valuehead for Lc0 rewrite"              
            else
              let msg = $"The engine {engineConfig.Name} does not support value head testing, only Lc0 and Ceres supports it currently"
              ConsoleUtils.printInColor ConsoleColor.Yellow msg
        if validationPassed then
          ConsoleUtils.printInColor ConsoleColor.Yellow "All engines passed validation for value head tests"
        else
          ConsoleUtils.printInColor ConsoleColor.Red "Validation failed for value head tests"
          
      if validationPassed then
        //run tournament
        // Do NOT pass cts.Token to RunSynchronously — the tuner's SPRT callback
        // uses cts.Cancel() to stop remaining games, but RunSynchronously must
        // still return normally with the completed results. Passing the token here
        // would throw OperationCanceledException and discard all results.
        let res = tourny |> Async.RunSynchronously
        sendResponse (EndOfTournament tournament)
        logger.LogInformation($"Elapsed tournament time in seconds: {(timer.ElapsedMilliseconds/1000L)}")
        res
      else
        logger.LogInformation("Tournament validation failed, please make sure that all engines in the tournament supports value head tests.")
        []
  
  type Runner (logger: ILogger, callback: Action<Update>, reloadTournament:bool, consoleOnly : bool) =
    let cts = new CancellationTokenSource()
    let userAdjudicationChannel = Channel.CreateUnbounded<UserAdjudication>()
    let mutable tournamentLoaded = reloadTournament
    let mutable tournament = if reloadTournament then loadTournament() else Tournament.Empty
    let mutable resultsFromPGN = ResizeArray<Result>()
    let mutable pgnReader = None
    let mutable consoleMode = consoleOnly
    let executablePath() = tournament.OrdoExePath

    // Serializes Ordo calls on a background thread with "latest wins" draining
    let ordoAgent = MailboxProcessor<PlayerResult[] * string * string>.Start(fun inbox ->
        let rec loop () = async {
            let! msg = inbox.Receive()
            // Drain queue to only process the latest request
            let mutable latest = msg
            while inbox.CurrentQueueLength > 0 do
                let! next = inbox.Receive()
                latest <- next
            let (capturedData, ordoPath, pgnPath) = latest
            try
                let cmd = OrdoHelper.createOrdoCommand ordoPath pgnPath ""
                Console.WriteLine($"\n Ordo command: {cmd.Arguments} \n")
                let! ordo = OrdoHelper.runCommandAsync cmd capturedData cts.Token |> Async.AwaitTask
                callback.Invoke (Update.GameSummary ordo)
            with e ->
                Console.WriteLine($"Ordo error: {e.Message}")
            return! loop()
        }
        loop()
    , cts.Token)

    let tryDequeueUserAdjudication () =
      let reader = userAdjudicationChannel.Reader
      let mutable last = None
      let mutable item = Unchecked.defaultof<UserAdjudication>
      while reader.TryRead(&item) do
        last <- Some item
      last
    
    member val TotalGames = 0 with get, set
    member val SuppressDashboard = false with get, set
    member x.PgnReader
        with get() = 
          if String.IsNullOrWhiteSpace tournament.PgnOutPath then
            failwith "PgnOutPath is not set in tournament.json"
          match pgnReader with
          |None ->
            let reader = ChessLibrary.FullPGNParser.startPgnGameReaderWriter tournament.PgnOutPath
            pgnReader <- Some reader
            reader
          |Some pgnReader -> pgnReader
        and set(value) = pgnReader <- Some value

    member _.DisposePgnReader() =
        match pgnReader with
        | Some reader ->
            reader.Post(ChessLibrary.FullPGNParser.Dispose)
            pgnReader <- None
        | None -> ()

    member x.SendResponse (update: Update) =
      // Raise the callback with a proper Update response
      match update with
      | PeriodicResults results ->
          try
              callback.Invoke update
              if not x.SuppressDashboard then
                  let pgnGames = x.GetPGNGames()
                  if pgnGames.Count > 0 then
                      let consoleResString, data, _, _= PGNCalculator.getEngineDataResults pgnGames
                      let ordoPath = executablePath()
                      if String.IsNullOrEmpty ordoPath |> not && tournament.ConsoleOnly then
                          let capturedData = data |> Seq.toArray
                          ordoAgent.Post (capturedData, ordoPath, tournament.PgnOutPath)
                      else
                          let gameUpdate = Update.GameSummary consoleResString
                          callback.Invoke gameUpdate
                  else
                    let pRes = x.GetPlayerResults results
                    let cross = x.GenerateStatsCrosstable results
                    let table = OrdoHelper.getResultsAndPairsInConsoleFormat pRes cross
                    callback.Invoke (Update.GameSummary table)
          with e ->
              let msg = $"Error generating periodic results: {e.Message}"
              printfn "%s" msg
      |_ -> callback.Invoke update
        
    member _.AddTournament tourny = tournament <- tourny 

    member _.AdjudicateGame(gameNr: int, result: string) =
      let isValid =
        match result with
        | "1-0" | "0-1" | "1/2-1/2" -> true
        | _ -> false

      if isValid then
        userAdjudicationChannel.Writer.TryWrite({ GameNr = gameNr; Result = result }) |> ignore
      else
        logger.LogWarning("Invalid user adjudication result string: {Result}", result)

    member x.Run() =
      try
        // Ensure external shutdowns are translated to our CTS cancellation
        Console.CancelKeyPress.Add(fun args ->
            cts.Cancel()
            args.Cancel <- true
        )
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
            try cts.Cancel() with _ -> ()
        )
        // Create shared PGN agent for the tournament so the callback's
        // GetPGNGames() uses the same agent workers write to.
        // Must be created BEFORE GetResults() so PgnReader returns this
        // agent instead of lazily creating a separate one that gets leaked.
        let agent =
            if String.IsNullOrWhiteSpace tournament.PgnOutPath |> not then
                let a = FullPGNParser.startPgnGameReaderWriter tournament.PgnOutPath
                pgnReader <- Some a
                Some a
            else None
        resultsFromPGN <- x.GetResults()
        startTournament cts tournament logger x.SendResponse consoleMode tryDequeueUserAdjudication agent
      with
      | :? OperationCanceledException ->
        logger.LogInformation("Tournament cancelled.")
        resultsFromPGN |> Seq.toList
      | e ->
        printfn "Error: %A" e
        logger.LogCritical ("failed to run tournament" + tournament.MinSummary())
        resultsFromPGN |> Seq.toList
        //raise e
    
    member _.LinkCancellation(token: CancellationToken) = token.Register(fun () -> cts.Cancel()) |> ignore
    member _.GetPlayerResults (results: ResizeArray<Result>) : ResizeArray<PlayerResult> =
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let isGauntlet = tournament.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
      PGNCalculator.getFullStat isGauntlet challengers players results

    member _.GetPlayerResultsFromPGN (results: ResizeArray<Result>) : seq<PlayerResult> =
      PGNCalculator.getFullStatFromResults results

    member _.Cancel() = cts.Cancel()

    member _.InvalidateTournament() =
      tournamentLoaded <- false

    member _.Tournament() =
      //check if tournament is empty and reload if necessary
      if not tournamentLoaded then
        tournament <- loadTournament ()
        tournamentLoaded <- true
      tournament

    member _.LayoutUpdated() =      
        let tourny = loadTournament ()
        tournament.LayoutOption <- tourny.LayoutOption
        tournament

    member val GetPiecesLeft = 0 with get, set

    member val Pairings  = ResizeArray<Pairing>() with get, set

    member x.GetGamesLeftToPlay() =
      let gamesAlreadyPlayed = x.GetPGNGames() |> Seq.toArray
      let playedSet = PairingHelper.playedSet gamesAlreadyPlayed
      let gamesLeftToPlay = 
        [
          for p in x.Pairings do
          if PairingHelper.hasPlayedBefore p playedSet |> not then
            yield p
        ]
      gamesLeftToPlay
    
    member x.GetAllPairings() =
      let gamesLeftToPlay = x.GetGamesLeftToPlay()
      if gamesLeftToPlay.Length = 0 then
        ResizeArray<_>()
      else        
        gamesLeftToPlay |> ResizeArray

    member x.GetLastestPairings() =      
      let gamesLeftToPlay = x.GetGamesLeftToPlay()
      if gamesLeftToPlay.Length = 0 then
        ResizeArray<_>()
      else
        x.TotalGames <- gamesLeftToPlay.Length + tournament.CurrentGameNr
        gamesLeftToPlay |> Seq.skip 1 |> Seq.truncate 20 |> ResizeArray

    member x.GetResults() : ResizeArray<Result> =
      let fileExists = File.Exists tournament.PgnOutPath
      if fileExists then
        match x.PgnReader.TryPostAndReply((fun reply -> ChessLibrary.FullPGNParser.GetResults reply), timeout = 30000) with
        | Some results -> results
        | None ->
            printfn "WARNING: GetResults timed out after 30s"
            ResizeArray<Result>()
      else
        ResizeArray<Result>()

    member x.GetPGNGames() : ResizeArray<PgnGame> =
      let fileExists = File.Exists tournament.PgnOutPath
      if fileExists then
        match x.PgnReader.TryPostAndReply((fun reply -> ChessLibrary.FullPGNParser.GetPGNGames reply), timeout = 30000) with
        | Some results ->
            // Always recompute opening hashes to keep resume logic compatible across versions.
            results |> Seq.iter Hash.writeOpeningHashToPgnGame
            results
        | None ->
            printfn "WARNING: GetPGNGames timed out after 30s"
            ResizeArray<PgnGame>()
      else
        ResizeArray<PgnGame>()
 
    member _.GenerateCrosstableEntries (results: ResizeArray<Result>) =
      PGNCalculator.generateCrosstableEntries results
 
    member _.GenerateStatsCrosstable (results: ResizeArray<Result>) = 
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      PGNCalculator.generateSmallStatCrossTable results challengers players

    member _.GetGauntletCrosstable (results: ResizeArray<Result>) = 
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let challengers = 
        if tournament.EngineSetup.Engines.Length = 2 then
          players |> List.take 1
        else          
          tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      PGNCalculator.generateBigStatCrossTable results challengers players
