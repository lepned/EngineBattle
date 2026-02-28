namespace ConsoleApp
open System
open System.IO
open System.Threading
open System.Diagnostics
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System.Linq
open Serilog
open ChessLibrary
open TypesDef
open TimeControlTypes
open PuzzleTypes
open Tournament
open Configuration
open GameAnalysis
open TournamentPairing
open RuntimeUtilities
open CliParser


module BlazorInterop =
    let mutable blazorProcess: Process option = None
    let mutable private targetPage = "/tournament"

    let startBlazorAppAsync (page: string) (port: int option) =
        async {
            // Store the page route for use when server starts
            targetPage <- "/" + page.TrimStart('/')

            let currentDir = DirectoryInfo(Environment.CurrentDirectory)
            let endWithConsole = currentDir.FullName.EndsWith("Console")
            let parent =
                if endWithConsole then
                    currentDir.Parent
                else
                    currentDir.Parent.Parent.Parent.Parent
            let path = Path.Combine(parent.FullName, "WebGUI")
            let blazorProjectPath = path
            let psi = ProcessStartInfo()
            psi.FileName <- "dotnet"
            psi.Arguments <-
                match port with
                | Some p -> $"run --urls http://localhost:{p}"
                | None -> "run"
            psi.WorkingDirectory <- blazorProjectPath
            psi.UseShellExecute <- false
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.CreateNoWindow <- true

            let proc = new Process()
            proc.StartInfo <- psi

            proc.OutputDataReceived.Add(fun args ->
                if not (isNull args.Data) then
                    Console.WriteLine(args.Data)
                    // Open browser as soon as the app is running
                    if args.Data.Contains("Now listening on:") then
                        let baseUrl =
                            let parts = args.Data.Split("Now listening on:")
                            if parts.Length > 1 then
                                parts.[1].Trim().TrimEnd('/')
                            else
                                ""
                        let fullUrl = baseUrl + targetPage
                        Console.WriteLine($"Opening browser at {fullUrl}")
                        try
                            let psi = ProcessStartInfo()
                            psi.FileName <- fullUrl
                            psi.UseShellExecute <- true
                            Process.Start(psi) |> ignore
                        with ex ->
                            Console.WriteLine($"Failed to open browser: {ex.Message}")
            )

            proc.ErrorDataReceived.Add(fun args ->
                if not (isNull args.Data) then
                    Console.Error.WriteLine(args.Data)
            )

            proc.EnableRaisingEvents <- true
            proc.Exited.Add(fun _ ->
                Console.WriteLine("Blazor app has exited.")
                proc.Dispose()
            )

            proc.Start() |> ignore
            proc.BeginOutputReadLine()
            proc.BeginErrorReadLine()

            // Store the process instance in the mutable variable
            blazorProcess <- Some proc
            // Keep the process running in the background
            do! Async.Sleep(-1)
        }

    let stopBlazorApp() =
        match blazorProcess with
        | Some proc when not proc.HasExited ->
            Console.WriteLine("Terminating Blazor app process...")
            try
                proc.Kill(true) // Kill entire process tree (dotnet run + child WebGUI process)
            with _ -> ()
            proc.Dispose()
        | _ -> ()


module TestPath =
  // Define various paths for PGN files and directories
  let startPos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let frcLichess = "C:/Dev/Chess/PGNs/Results/Lichess/lichess_db_chess960_rated_2024-02.pgn"
  let bigLichess = "C:/Dev/Chess/PGNs/Results/Lichess/lichess_db_standard_rated_2017-10.pgn"
  let tcecOpening = "C:/Dev/Chess/Openings/TCEC_16-25.pgn"
  let pgnCup = "C:/Dev/Chess/PGNs/CupTest.pgn"
  let pgnTest1 = "C:/Dev/Chess/PGNs/Sf--t3-ovUncBo_T_Test.pgn"
  let pgnTest2 = "C:/Dev/Chess/PGNs/test_UHO_02.pgn"
  let pgnTest3 = "C:/Dev/Chess/PGNs/Lichess_Fast_01.pgn"
  let pgnTest4 = "C:/Dev/Chess/PGNs/Results/match_TOURN_Ceres1_LC0_2_01.pgn"
  let pgnTest5 = "C:/Dev/Chess/PGNs/Results/BT4_Binary.pgn"
  let pgnTest6 = "C:/Dev/Chess/PGNs/Results/SimulatedSpeedTest.pgn"
  let pgnTest7 = "C:/Dev/Chess/PGNs/Deviations/feb25.pgn"
  let pgnTest8 = "C:/Dev/Chess/PGNs/Boosting.pgn"
  let pgnTest9 = "C:/Dev/Chess/PGNs/Results/T3_distilledTest.pgn"
  let pgnTest10 = "C:/Dev/Chess/PGNs/t3_Test_19.pgn"
  let pgnTest11 = "C:/Dev/Chess/PGNs/Results/TCEC_Season_24_-_Superfinal.pgn"
  let pgnTest12 = "C:/Dev/Chess/PGNs/Results/ccc22-rapid-semifinals.pgn"
  let pgnTest13 = "C:/Dev/Chess/PGNs/Results/TCEC-S25-Superfinal.pgn"
  let pgnTest14 = "C:/Dev/Chess/PGNs/Results/TCEC-everything.pgn"
  let pgnTest15 = "C:/Dev/Chess/PGNs/CeresValueTempTest_07.pgn"
  let pgnTest16 = "C:/Dev/Chess/PGNs/match_run2.pgn"
  let pgnTest17 = "C:/Dev/Chess/PGNs/UHO_4060_v4.pgn"
  let ccc = "C:/Dev/Chess/pgns/ccc22-rapid-semifinals.pgn"
  let selection = "C:/Dev/Chess/PGNs/Results/TCEC/Selection"
  let allTcec = "C:/Dev/Chess/PGNs/Results/TCEC"
  let tcec21 = "C:/Dev/Chess/PGNs/Results/TCEC/TCEC_Season_21/TCEC_Season_21_-_Superfinal.pgn"
  let cccSelection = "C:/Dev/Chess/PGNs/Results/CCC"
  let testPGN = "C:/Dev/Chess/PGNs/Results/test.pgn"
  let ceres1 = "C:/Dev/Chess/PGNs/Ceres/match_TOURN_Ceres1_Ceres2_638722631385989248.pgn"
  let ceres2 = "C:/Dev/Chess/PGNs/Ceres/match_TOURN_Ceres1_Ceres2_638722637902257039.pgn"
  let ceres3 = "C:/Dev/Chess/PGNs/Ceres/match_TOURN_Ceres1_Ceres2_638721916003216067.pgn"
  let ceresFolder = "C:/Dev/Chess/PGNs/Ceres"
  let navsLatestPGN = "C:/Dev/Chess/PGNs/ContemptTest.pgn"
  
  
  
  /// <summary>
  /// Determines the path to the tournament JSON file.
  /// </summary>
  let tournamentPath() =
      let dir = DirectoryInfo(Environment.CurrentDirectory).Parent.Parent.Parent.Parent.FullName
      let path = Path.Combine(dir, "WebGUI","wwwroot", "tournament.json")
      if FileInfo(path).Exists then
        path
      else
        let dir = DirectoryInfo(Environment.CurrentDirectory).Parent.FullName
        let path = Path.Combine(dir,"WebGUI","wwwroot","tournament.json")
        path
  
  /// <summary>
  /// Reads the tournament JSON file and initializes the tournament object.
  /// </summary>
  let tournament() = 
      //let currentDir = DirectoryInfo(Environment.CurrentDirectory).FullName
      //check if current directory ends with Console
      //let endsWithConsole = currentDir.EndsWith("Console")
      //printfn "Current directory: %s" currentDir
      //if endsWithConsole then
      //  printfn "Current directory ends with Console"
            
      let dir = DirectoryInfo(Environment.CurrentDirectory).Parent.Parent.Parent.Parent.FullName
      let tournamentJsonPath = Path.Combine(dir, "WebGUI","wwwroot", "tournament.json")
      match JSON.readTournamentJson tournamentJsonPath with
      |Some tourny ->
        let tourny = 
          if tourny.EngineSetup.EngineDefList.Length > 0 then
            let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
            if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) && tourny.Challengers > 0 then
              for engine in engineList |> List.truncate tourny.Challengers do
                engine.IsChallenger <- true
            else
              for engine in engineList do
                engine.IsChallenger <- false
            let engineSetup = {tourny.EngineSetup with Engines = engineList}
            {tourny with EngineSetup = engineSetup }
          else               
            let path = "C:\Dev\Chess\Networks\CeresLatest"
            let dir = DirectoryInfo(path)
            if dir.Exists then
              let challengers = List<string>.Empty //["Ceres C1-640-34_4.4bn"]
              let engineList = (EngineHelper.createEnginesFromFolder dir.FullName) |> Seq.toList
              for engine in engineList do
                engine.IsChallenger <- false
                if challengers |> List.exists(fun e -> e.Contains engine.Name) then
                    engine.IsChallenger <- true
              let engineSetup = {tourny.EngineSetup with Engines = engineList}
              {tourny with EngineSetup = engineSetup }
            else 
              tourny         
        {tourny with MinMoveTimeInMS = 0; ConsoleOnly = true; DelayBetweenGames = TimeOnly.MinValue }        
      |_ -> //backup plan
        let dir = DirectoryInfo(Environment.CurrentDirectory).Parent.Parent.Parent.Parent.FullName
        let path = Path.Combine(dir,"WebGUI", "Data","tournamentEmpty.json")
        let dest = Path.Combine(dir,"WebGUI","wwwroot","tournament.json")
        if FileInfo(path).Exists then
          File.Copy(path, dest, true) |> ignore
          let tourny = JSON.readTournamentJson dest
          match tourny with
          |Some tourny ->
            let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
            let engineSetup = {tourny.EngineSetup with Engines = engineList}
            {tourny with EngineSetup = engineSetup }
          |_ -> failwith "Tournament json file not found!"
        else 
          //try current directory 
          //let dir = DirectoryInfo(Environment.CurrentDirectory).FullName
          //printfn "Current directory: %s" dir
          let path = Path.Combine(dir,"tournament.json")
          if FileInfo(path).Exists then
              //printfn "Found tournament.json in current directory"
              //File.Copy(path, dest, true) |> ignore
              let tourny = JSON.readTournamentJson path
              match tourny with
              |Some tourny ->
                let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
                let engineSetup = {tourny.EngineSetup with Engines = engineList}
                {tourny with EngineSetup = engineSetup }
              |_ -> failwith "Tournament json file not found!"
          else 
            let dir = DirectoryInfo(Environment.CurrentDirectory).FullName
            printfn "Current directory: %s" dir
            failwith "Tournament json file not found after backup plans!"
  
  /// <summary>
  /// Generates engine configuration JSON files and a tournament configuration JSON file from a specified directory.
  /// </summary>
  let createTournamentJsonAndEngineJsonFromDirectory() =
    let networkFolder = "C:/Dev/Chess/Networks/CeresLatest"
    let folder = "C:/Dev/Chess/Networks/CeresLatest/output_EngineJson"
    JSON.getAllConfigFiles networkFolder
    let tournyCloned = JSON.createTournamentFile (tournamentPath()) folder
    JSON.writeTournamentJson tournyCloned folder

module Eret =
  
  let eretPath = "C:/Dev/Chess/Puzzles/ERET_VESELY203.epd"
  let eretPath2 = "C:/Dev/Chess/Puzzles/chad_tactics-100M.epd"  
  let timeConfig = UnionType.FixedTime (TimeOnly(0,0,5)) //10 seconds
  let timeConfig2 = UnionType.Nodes 1_000_000

  /// <summary>
  /// Processes the ERET update and prints relevant information.
  /// </summary>
  let processEret (data :EretConfig) (update : ERET) =
    match update with    
    | Start info -> printfn "\nERET test started: %s" info
    | Puzzle (epd, correct) -> 
        let avoidM, bMove = 
          (if epd.AvoidMove.IsSome then sprintf "\n\tAvoidMove: %s" epd.AvoidMove.Value else ""), 
          if epd.BestMove.IsSome then sprintf "\n\tBestMove: %s" epd.BestMove.Value else ""
        let epdText = sprintf "id: %A FEN: %s %s %s" epd.Id epd.FEN avoidM bMove
        printfn "Puzzle: %s, Correct/solved: %b" epdText correct
    | PlayerResult res ->
        printfn "\nPlayer results for %s: Correct: %d, Failed: %d, Accuracy: %s\n" res.PlayerName (Seq.length res.CorrectPuzzles) (Seq.length res.FailedPuzzles) (res.Accuracy.ToString("F2"))
    | AllResults eretResults -> 
        printfn "\nAll failed puzzles by engine below:" 
        printfn "--------------------------------------------------------------------"
        for res in eretResults do
            printfn "\nFailed puzzles by %s: Correct: %d, Failed: %d, Accuracy: %s\n" res.PlayerName (Seq.length res.CorrectPuzzles) (Seq.length res.FailedPuzzles) (res.Accuracy.ToString("F2"))
            for (puzzle,_) in res.FailedPuzzles do                
                printfn "%s" puzzle.RawInput
        printfn "\n--------------------------------------------------------------------"
        let escaped = JSONParser.escapeString data.FailedPuzzlesOutputFolder

        if Directory.Exists(escaped) then
            let datePart = DateTime.Now.ToString("yyyy-MM-dd_HH-mm", System.Globalization.CultureInfo.InvariantCulture)
            let fileName = Path.Combine(escaped, $"failedEretPuzzles_{datePart}.epd")
            let boardBm = Chess.Board()
            let boardAm = Chess.Board()
            try
              use sw = File.AppendText(fileName)
              PuzzleRunners.writeToFile data eretResults sw boardBm boardAm
              Console.WriteLine($"Failed Eret puzzles written to {fileName}")
            with 
            | ex ->
                Console.WriteLine($"Failed to write results to file: {ex.Message}")
       
    | ResultsInConsole table ->
        let escaped = JSONParser.escapeString data.FailedPuzzlesOutputFolder
        if Directory.Exists(escaped) then        
          let datePart = DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
          let outputPath = Path.Combine(escaped, $"EretSummary_{datePart}.txt")
          try          
            File.WriteAllText(outputPath, table);
            Console.WriteLine($"Console summary written to {outputPath}")
          with 
          | ex ->
              Console.WriteLine($"Failed to write results to file: {ex.Message}")        
        printfn "\n%s" table

 
module Program =
  open Configuration.JSONParser
  open System.Globalization
  open TournamentTypes

  /// <summary>
  /// Configures logging for the application.
  /// </summary>
  let configureLogging (builder: ILoggingBuilder) =
      builder
          .AddSerilog()
          .AddConsole()
          .SetMinimumLevel(LogLevel.Critical)
          |> ignore

  let logPath = Path.Combine("..", "logs", "log-{Date}.txt");
  
  /// <summary>
  /// Creates and configures the host for the application.
  /// </summary>
  let createHost() =
      Log.Logger <- LoggerConfiguration() // Create a Serilog logger configuration
        .MinimumLevel.Information() // Set the minimum log level
        //.WriteTo.Console() // Write to console
        .WriteTo.File(logPath, rollingInterval = Serilog.RollingInterval.Day) // Write to file
        .CreateLogger() // Create the logger
      Host.CreateDefaultBuilder()
          .ConfigureLogging(configureLogging)
          .Build()
        

  
  let runEretTest (path:string) =
    let normalizedPath = normalizePath path
    let data = loadEretConfig normalizedPath
    printfn "Processing ERET puzzle file: %s" path 
    let time = UnionType.FixedTime (TimeOnly(0,0,data.TimeInSeconds))
    let nodes = UnionType.Nodes data.Nodes
    let engineConfigs = 
        data.Engines 
        |> Seq.collect (mapToEngPuzzleConfig data.EngineFolder)
        |> ResizeArray   
    let timeControl = if data.RunWithNodeLimit then nodes else time
    PuzzleRunners.runEretTests
      timeControl
      engineConfigs
      data
      (Eret.processEret data)
      CancellationToken.None |> ignore

  let runPuzzles (path:string) =    
    let data = loadPuzzleConfig (normalizePath path)
    let normalizedPath = normalizePath data.PuzzleFile
    printfn "Processing Lichess puzzle file: %s" path   
    let engineConfigs = 
        data.Engines 
        |> Seq.collect (mapToEngPuzzleConfig data.EngineFolder)
        |> ResizeArray

    let puzzles = parsePuzzle normalizedPath false
    let formattedLength = puzzles.Length.ToString("N0")
    printfn $"Loaded {formattedLength} puzzles from {normalizedPath}"

    let puzzleInput =
        TypesDef.PuzzleInput.PuzzleInput.Create(
            puzzles,
            data.MaxRating,
            data.MinRating,
            data.RatingGroups,
            data.PuzzleFilter,
            engineConfigs,
            1,
            data.SampleSize,
            data.Nodes,
            data.Failed,
            data.Solved,
            data.Concurrency   )

    let update (res: Lichess) =
        match res with
        | PuzzleResult score ->
            let correct = score.Correct
            let total = score.TotalNumber
            let failed = total - correct
            let name = score.Engine
            printfn "Puzzle result for %s: Correct: %d, Failed: %d" name correct failed
        | Done msg -> printfn "Puzzle done: %s" msg
                  
    let types = 
        if String.IsNullOrEmpty(data.Type) || String.IsNullOrWhiteSpace (data.Type.Trim()) then            
            []
        else
            data.Type.ToLower().Split(",") 
            |> Seq.map (fun e -> e.Trim()) 
            |> Seq.toList
    
    let ct = CancellationToken.None
    let scores =
        match types with
        | [] ->
            printfn "No puzzle types specified, defaulting to both policy and value test"
            PuzzleRunners.runValueAndPolicyHeadTest(puzzleInput, update, ct)
        | _ ->
            printfn "Puzzle types specified: %s" data.Type
            let nodeList = PuzzleDataUtils.parseNodes puzzleInput.nodes
            let subTests =
                types
                |> List.collect (fun t ->
                    match t with
                    | "policy" -> [ PuzzleEngineAgent.SubTest.Policy ]
                    | "value"  -> [ PuzzleEngineAgent.SubTest.Value ]
                    | "search" -> nodeList |> Array.toList |> List.map PuzzleEngineAgent.SubTest.Search
                    | "solve"  -> nodeList |> Array.toList |> List.map PuzzleEngineAgent.SubTest.Solve
                    | other ->
                        printfn "Unknown puzzle type '%s', skipping" other
                        [])
            if subTests.IsEmpty then
                printfn "No valid puzzle types found, defaulting to policy and value test"
                PuzzleRunners.runValueAndPolicyHeadTest(puzzleInput, update, ct)
            else
                PuzzleEngineAgent.runTest puzzleInput (Action<Lichess>(update)) subTests ct

    let valueScores = 
        scores 
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Value"))
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)                         
        |> Seq.toList

    let policyScores = 
        scores 
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Policy") && e.Nodes = 1)
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
        |> Seq.toList

    let search =
        scores
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Search") && e.Nodes > 1)
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
        |> Seq.toList

    let solve =
        scores
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Solve") && e.Nodes > 1)
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
        |> Seq.toList

    let writeToFile (scores: Score seq) (sw:StreamWriter) (boardBm: Chess.Board) (boardAm: Chess.Board) =
        for item in scores do
            sw.WriteLine($"\n## Failed puzzles by {item.Engine} (nn: {item.NeuralNet}) - overall performance: {item.PlayerRecord.Rating:F0} - Type: {item.Type} - Theme: {item.Filter} - Nodes: {item.Nodes}\n")
            let sorted =
                item.FailedPuzzles
                |> Seq.map (fun (a,b) ->
                        let parts = b.Split(',')
                        match parts with
                        | [| value; _ |] ->
                            let v = Double.Parse(value.Trim(), CultureInfo.InvariantCulture)
                            (a, v, b)
                        | _ -> (a, float a.Rating, b)
                )
                |> Seq.sortBy (fun (_, value, _) -> value)
                //|> Seq.takeWhile (fun (_, value, _) -> value < 100.0)
            
            for (puzzle,_,policyStr) in sorted do
                for cmd in puzzle.Commands do
                    if not (String.IsNullOrWhiteSpace(cmd.MovePlayed)) && cmd.MovePlayed.Length >= 4 then
                        boardBm.PlayCommands(cmd.Command)
                        let fen = boardBm.FEN()
                        boardBm.PlayUciMove(cmd.CorrectMove)
                        let bm = boardBm.SanMovesPlayed |> Seq.tryLast |> Option.defaultValue null
                        boardAm.PlayCommands(cmd.Command)
                        boardAm.PlayUciMove(cmd.MovePlayed)
                        let aM = boardAm.SanMovesPlayed |> Seq.tryLast |> Option.defaultValue null
                        let policies = policyStr.Split(',')
                        let bmP, amP =
                            if policies.Length > 1 then
                                policies.[0].Trim(), policies.[1].Trim()
                            else
                                "", ""
                        let msg = $"{fen} bm {bm}; am {aM}; id \"Lichess id {puzzle.PuzzleId}, policy value for bestmove {bm}={bmP} and move played {aM}={amP}\"; other \"{cmd.CorrectMove},{cmd.MovePlayed}\""
                        sw.WriteLine(msg)
        
    let escaped = escapeString data.FailedPuzzlesOutputFolder
    let table = createCombinedScoresTable normalizedPath policyScores valueScores search solve
    printfn "%s" table

    if Directory.Exists(escaped) then
        let filenameFriendlyDate = DateTime.Now.ToString("yyyy-MM-dd_HH-mm", System.Globalization.CultureInfo.InvariantCulture)        
        //let datePart = DateTime.Now.ToString("yyyyMMdd_HHmmss", CultureInfo.InvariantCulture)
        let fileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.epd")
        let boardBm = Chess.Board()
        let boardAm = Chess.Board()

        use sw = File.AppendText(fileName)
        writeToFile policyScores sw boardBm boardAm
        writeToFile valueScores sw boardBm boardAm
        writeToFile search sw boardBm boardAm
        writeToFile solve sw boardBm boardAm

        let allScoresForCross = Seq.concat [ policyScores; valueScores; search; solve ]
        PuzzleCrossEngine.writeCrossEngineFiles escaped filenameFriendlyDate allScoresForCross

        let testTypeInfo = String.Join("-", types)
        let engineCount = engineConfigs.Count
        //write table to file with date and time
        let tableFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt")
        //let tableFileName = Path.Combine(escaped, $"LichessPuzzleScore_{datePart}.txt")
        use tableWriter = new StreamWriter(tableFileName)
        tableWriter.WriteLine(table)    
  
  let runTournament (tournament:Tournament.Tournament) (logger: Microsoft.Extensions.Logging.ILogger) =    
    let cts = new CancellationTokenSource()
    let exitEvent = new ManualResetEvent(false)
    Console.CancelKeyPress.Add(fun args ->
        printfn "Cancellation requested. Stopping..."
        cts.Cancel()
        args.Cancel <- false  // CHANGED: Allow the application to terminate
        exitEvent.Set() |> ignore
        Environment.Exit(0)  // Force exit
    )    
    
    /// Define the MailboxProcessor for handling updates asynchronously
    let createUpdateProcessor (verbose:bool) =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () =
                async {
                    // Wait for a message
                    let! update = inbox.Receive()
                    // Process the update
                    match update with
                    | GameStarted white -> printfn "Game started with white player %s" white
                    | EndOfGame result -> printfn "End of game result: %s" (result.ToString())
                    | BestMove (bm, status) -> 
                        if verbose then
                            printfn "Player %s: BestMove %s with eval %A" bm.Player bm.Move bm.Eval
                    | Info (player, info) -> 
                        if verbose then
                            printfn "Player %s with info data: %s" player info
                    | Eval (player, evalType) -> 
                        if verbose then
                            printfn "Player %s with eval %A" player evalType
                    | Status engineStatus -> 
                        if verbose then
                            printfn "\tPlayer %s with PV: %s" engineStatus.PlayerName engineStatus.PV
                    | PonderStatus engineStatus -> 
                        if verbose then
                            printfn "\tPlayer %s with Nodes: %d" engineStatus.PlayerName engineStatus.Nodes
                    | Time (player, time) -> 
                        if verbose then
                            printfn "\tPlayer %s with time left %A" player time
                    | NNSeq nnSeq -> 
                        if verbose then
                            printfn "NNSeq: %A" nnSeq
                    | StartOfGame startGameInfo -> printfn "%s" (startGameInfo.ToString())
                    | EndOfTournament info -> printfn "End Of Tournament:\n%s" (info.Summary())
                    | StartOfTournament info ->
                        printfn "\nStart of tournament:\nNumber of games to play: %d \nSummary:\n%s\n" info.NumberOfGames (info.Tournament.Value.MinSummary())
                    | MessagesFromEngine (player, message) -> 
                        if verbose then
                            printfn "MessagesFromEngine: Player - %s, Message - %s" player message
                    | PairingList pairings ->
                        let openings = PairingHelper.getAllOpeningPairs (pairings |> Seq.toList)
                        //if verbose then 
                        printfn "%s" openings
                    | PeriodicResults results ->
                        match results |> Seq.tryHead with
                        | Some _ -> printfn "Partial update after game %d" results.Count
                        | None -> printfn "Partial update after game (no games played yet)"                    
                    | GameSummary summary -> printfn "\nTournament Summary: \n%s" summary
                    | TotalNumberOfPairs totalPairs -> 
                        if verbose then
                            printfn "Total number of pairs in tournament: %d" totalPairs
                    | RoundNr roundNr -> 
                        if verbose then
                            printfn "Round number: %s" roundNr
                    | _ ->
                        if verbose then
                            printfn "Received update: %A" update

                    // Continue processing messages
                    return! loop ()
                }
            loop ()
        )
      
    let tourny = tournament
    //ConsoleHelper.displayTournament tourny      
    let updateProcessor = createUpdateProcessor tourny.VerboseLogging
    let printUpdate update = updateProcessor.Post(update)

    let runner = Manager.Runner(logger, printUpdate, false, true)
    runner.AddTournament tourny
    let start = Stopwatch.GetTimestamp()
    let formatHms (ts: TimeSpan) = 
        let hours = int ts.TotalHours 
        let minutes = ts.Minutes 
        let seconds = ts.Seconds 
        sprintf "%dh %dm %ds" hours minutes seconds

    try
        let results = runner.Run()
        let endTime = Stopwatch.GetElapsedTime start
        let gamesPlayed = results |> Seq.length
        let msg = $"Tournament completed - Duration: {formatHms endTime}, Games: {gamesPlayed}, Parallel: {tourny.TestOptions.NumberOfGamesInParallelConsoleOnly}"
        Console.WriteLine msg
        logger.LogInformation(msg)
        if File.Exists tourny.PgnOutPath then            
            let consoleRes, _, _, allResults = PGNCalculator.getEngineDataResults (runner.GetPGNGames())            
            Console.WriteLine consoleRes
            logger.LogInformation(consoleRes)
        else
            let results = results |> ResizeArray
            let scoreTable = runner.GetPlayerResults(results)
            let table = runner.GenerateStatsCrosstable(results)
            let consoleRes = OrdoHelper.getResultsAndPairsInConsoleFormat scoreTable table
            Console.WriteLine consoleRes
            logger.LogInformation(consoleRes)        
        
    with
    |ex -> printfn "Caught an exception: %s" ex.Message

  /// <summary>
  /// The main entry point for the application.
  /// </summary>
  [<EntryPoint>]
  let main argv = 
    ConsoleUtils.originalColor <- Console.ForegroundColor
    
    let test = false
    if test then      
      try 
        let start = Stopwatch.GetTimestamp()
        let parsedGames = ChessLibrary.FullPGNParser.parsePgnFile TestPath.pgnTest17
        let game = parsedGames |> Seq.item 0
        for m in game.Mainline do
            printfn "%s (%s) %s" m.San m.Color m.Comment
        for g in parsedGames do
            if g.GameNumber % 100000 = 0 then
                if String.IsNullOrEmpty g.Raw then
                    let raw = ChessLibrary.FullPGNParser.toPgnString g
                    printfn "\n%s" raw 
                else
                    printfn "\n%s" g.Raw                
       
        printfn "Total games parsed: %d" (parsedGames.Count())        
        //let games, total = Test.removeEPFensInPGNFile TestPath.pgnTest17
        //printfn "Total games processed: %d, removed: %d" total (total - games.Length)
        //let pgnFile = "C:/Dev/Chess/PGNs/Results/UHO_4060_v4_epRemoved.pgn"
        //use writer = new StreamWriter(pgnFile)
        //for game in games do            
        //    writer.Write(game.Raw)
        //Test.ParsingTests.testRemovePlayerFromPGN "Ceres"
        //let getMates = Test.ParsingTests.getAllMatesFromPGN Test.ParsingTests.queenOddsGames false
        //if getMates |> Seq.isEmpty then
        //  printfn "No mates found"
        //else
        //  let numberOfMatesFound = getMates |> Seq.length
        //  printfn "Number of mates found: %d" numberOfMatesFound
        //  let quickMates = getMates |> List.sortBy (fun (_,idx,_,_,_) -> idx)
        //  let top10 = quickMates |> List.truncate 10
        //  printfn "First 10 mates found:"
        //  for (fen, idx, _, _, _) in top10 do
        //    printfn "Mate after %d plies with move %s (%s): Fen: %s" idx fen.ShortSan (fen.Move.LongSan) fen.FenAfterMove

        //  let epdFile = "C:/Dev/Chess/Lichess/top10QueenOdds.epd"          
        //  let writeEPd () =
        //    use writer = new StreamWriter(epdFile)
        //    for (fen, idx, _, _, _) in top10 do
        //      let idInfo = sprintf "Mate after %d plies" idx
        //      let epd = sprintf "%s bm %s; id \"%s\";" fen.FenAfterMove fen.Move.LongSan idInfo
        //      writer.WriteLine(epd)
        //  writeEPd()

        //let eng1, eng2 = TestPath.tournament.EngineSetup.Engines.[0], TestPath.tournament.EngineSetup.Engines.[1]
        //let engine1, engine2 =
        //  let e1 = Engine.createEngine eng1
        //  let e2 = Engine.createEngine eng2
        //  Engine.initEngines 2 e1 e2          
        //  e1, e2
        //let playDemo = Engine.playMovesFromFen engine1 engine2 TestPath.startPos 5000 10   
        
        
        //Test.Deviations.deviationSummaryFromPGNs testPGN
        //ParsingTests.parsAllPGNgames bigLichess false
        //ParsingTests.gameAnalysisFromFolderAndSubFolder ceresFolder 2 4.0 (3.0, 0.5)       
        //let files = [pgnTest1; pgnTest2; pgnTest3; pgnTest4; pgnTest5 ; pgnTest6; pgnTest7; pgnTest8; pgnTest13 ]   
        //files |> List.iter (fun file -> Test.ParsingTests.parsAllPGNgames file false)        
        
        //Test.completeFRCPerftVerificationTest 5 960
        //Test.smallPerftTestSample 5        
        let mutable time = int64 (Stopwatch.GetElapsedTime(start).TotalMilliseconds)
        let ts = TimeSpan.FromMilliseconds(float time)
        printfn "\nTime: %A" ts

        0
      with
      |ex -> 
        printfn "Caught an exception: %s" ex.Message
        0

    else
        // Hook into the ProcessExit event
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> BlazorInterop.stopBlazorApp())
        //let path = DirectoryInfo(Environment.CurrentDirectory).FullName       
        //printfn "Current directory: %s" path        
        //let endsWithConsole = path.EndsWith("Console")
        //Async.Start(BlazorInterop.startBlazorAppAsync())
        
        Console.OutputEncoding <- System.Text.Encoding.UTF8
        let cliArgs = CustomParser.parse (System.Environment.GetCommandLineArgs())
        let mutable tournament = Tournament.Tournament.Empty
        match cliArgs with
        | [] -> 
            printfn "No arguments provided to console app"
        | _ -> 
            printfn "\nArguments provided to console app: %A" cliArgs
            for arg in cliArgs do
                match arg with
                | Verb (Perft (depth, sampleSize)) ->
                    printfn "Running Chess960 PERFT with depth: %d and sample size: %d" depth sampleSize
                    Test.completeFRCPerftVerificationTestFast depth sampleSize
                | Verb (Analyze fenOrFile) ->
                    let board = Chess.Board()
                    board.LoadFen fenOrFile
                    let update (engineUpdate: ChessLibrary.EngineTypes.EngineUpdate)  =
                        match engineUpdate with
                        | EngineTypes.EngineUpdate.Info (p, info) ->
                            printfn "Info: %s" info
                        | EngineTypes.EngineUpdate.BestMove bestMove ->
                            printfn "Best move: %s" bestMove.MoveHistory
                        | EngineTypes.EngineUpdate.Eval (p,eval) ->
                            printfn "Eval: %A" eval
                        | _ -> ()
                    printfn "Todo - Analyzing FEN not implemented: %s" fenOrFile
                    //let engine = Engine.createAltEngine( update, (TestPath.tournament()).EngineSetup.Engines.[0])
                    //engine.SendUCICommand(TypesDef.Engine.UCICommand.UciNewGame)
                    //engine.SendUCICommand(TypesDef.Engine.UCICommand.Position fenOrFile)
                    //engine.SendUCICommand(TypesDef.Engine.UCICommand.GoNodes 1000000)                    
                    //Engine.playMovesFromFen engine engine fenOrFile 5000 1
                    //engine.StopProcess()
                | Verb (PuzzleJson path) -> 
                    runPuzzles path          
                | Verb (Eret path) ->                     
                    runEretTest path
                | Verb (Tournament configFile) ->
                    let normalizedPath = normalizePath configFile                
                    let tournamentConfig = JSON.readTournamentJson normalizedPath
                    match tournamentConfig with
                    | Some tourny ->
                        let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
                        if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) && tourny.Challengers > 0 then
                          for engine in engineList |> List.truncate tourny.Challengers do
                            engine.IsChallenger <- true
                        else
                          for engine in engineList do
                            engine.IsChallenger <- false
                        let engineSetup = {tourny.EngineSetup with Engines = engineList}
                        tournament <- 
                            {tourny with 
                                EngineSetup = engineSetup
                                PreventMoveDeviation = tournament.TestOptions.NumberOfGamesInParallelConsoleOnly <= 1 
                                MinMoveTimeInMS = 0
                                ConsoleOnly = true
                                DelayBetweenGames = TimeOnly.MinValue
                            }
                        if tournament.TournamentMode.Equals("Ladder", StringComparison.OrdinalIgnoreCase) then
                          printfn "Ladder mode: %d engines, %d game pairs per match" engineList.Length (if obj.ReferenceEquals(tournament.LadderOptions, null) then 4 else tournament.LadderOptions.GamePairsPerMatch)
                        printfn "Running tournament with config file: %s" configFile
                        use host = createHost()
                        host.Start()    
                        let loggerFactory = host.Services.GetService(typeof<ILoggerFactory>) :?> ILoggerFactory
                        let logger = loggerFactory.CreateLogger("EngineBattle Console logger") // Using a general category name
                        runTournament tournament logger
                        host.StopAsync().Wait()                    
                    | None -> 
                        printfn "Tournamentjson config file not found..."                        
                | Verb (Benchmark path) ->
                    BenchmarkRunner.runBenchmark path
                | Verb (Tune path) ->
                    Console.CancelKeyPress.Add(fun args ->
                        args.Cancel <- true
                        printfn "\nTuner cancelled by user. Exiting..."
                        Environment.Exit(0)
                    )
                    BayesianOptimizer.runTuneWithDispatch path
                | Verb (Redash path) ->
                    BayesianOptimizer.regenerateDashboard path
                | Verb (GUI (page, port)) ->
                    let portStr = match port with Some p -> $" on port {p}" | None -> ""
                    printfn "Starting WebGUI%s with page: /%s" portStr page
                    printfn "Press Ctrl+C or Enter to stop the server..."
                    // Handle Ctrl+C to properly shutdown WebGUI
                    Console.CancelKeyPress.Add(fun args ->
                        args.Cancel <- true // Prevent immediate termination
                        BlazorInterop.stopBlazorApp()
                        Environment.Exit(0)
                    )
                    Async.Start(BlazorInterop.startBlazorAppAsync page port)
                    // Keep the console app running until terminated
                    Console.ReadLine() |> ignore
                    BlazorInterop.stopBlazorApp()
                | Help ->
                    printfn "Help: Available commands are:"
                    printfn "  - perft <depth> <sampleSize>"
                    printfn "  - puzzlejson <path>"
                    printfn "  - eretjson <path>"
                    printfn "  - tournamentjson <configFile>"
                    printfn "  - benchmark <configFile>"
                    printfn "  - tune <configFile>"
                    printfn "  - redash <configFile>  - Regenerate BO dashboard from saved state"
                    printfn "  - gui [page] [port]  - Launch WebGUI (default: tournament page, port 5018)"
                    printfn "    Examples: gui, gui singleEngineAnalysis, gui 5020, gui help 5020"
                | _ ->
                    printfn "Unhandled argument: %A" arg
        0

