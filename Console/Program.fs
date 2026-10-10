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
open MathNet.Numerics.LinearAlgebra


module BlazorInterop =
    let mutable blazorProcess: Process option = None
    let mutable private targetPage = "/tournament"

    /// The WebGUI project `gui` runs with `dotnet run`: found from the Console folder or its build
    /// output in a source checkout. None in a published package, which has no project to run.
    let tryWebGuiProject () =
        let currentDir = DirectoryInfo(Environment.CurrentDirectory)
        let parent =
            if currentDir.FullName.EndsWith("Console") then currentDir.Parent
            else
                let mutable dir = currentDir
                for _ in 1 .. 4 do
                    if dir <> null && dir.Parent <> null then dir <- dir.Parent
                dir
        if isNull parent then None
        else
            let path = Path.Combine(parent.FullName, "WebGUI")
            if File.Exists(Path.Combine(path, "WebGUI.csproj")) then Some path else None

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
                    // bin/Release/netX.0 layout: four levels up to the solution root.
                    // Guard the chain — from a shallow cwd (e.g. C:\EB) Parent is null.
                    let mutable dir = currentDir
                    for _ in 1 .. 4 do
                        if dir <> null && dir.Parent <> null then dir <- dir.Parent
                    dir
            if isNull parent then
                failwithf "Cannot locate the WebGUI project from '%s' — run the gui command from the repo (Console folder or build output)." currentDir.FullName
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
  let bigLichess = "C:/Dev/Chess/PGNs/Results/Lichess/lichess_db_standard_rated_2017-10.pgn"
  let pgnTest1 = "C:/Dev/Chess/PGNs/Sf--t3-ovUncBo_T_Test.pgn"
  let pgnTest2 = "C:/Dev/Chess/PGNs/test_UHO_02.pgn"
  let pgnTest3 = "C:/Dev/Chess/PGNs/Lichess_Fast_01.pgn"
  let pgnTest4 = "C:/Dev/Chess/PGNs/Results/match_TOURN_Ceres1_LC0_2_01.pgn"
  let pgnTest5 = "C:/Dev/Chess/PGNs/Results/BT4_Binary.pgn"
  let pgnTest6 = "C:/Dev/Chess/PGNs/Results/SimulatedSpeedTest.pgn"
  let pgnTest7 = "C:/Dev/Chess/PGNs/Deviations/feb25.pgn"
  let pgnTest8 = "C:/Dev/Chess/PGNs/Boosting.pgn"
  let pgnTest13 = "C:/Dev/Chess/PGNs/Results/TCEC-S25-Superfinal.pgn"
  let pgnTest17 = "C:/Dev/Chess/PGNs/UHO_4060_v4.pgn"
  let selection = "C:/Dev/Chess/PGNs/Results/TCEC/Selection"
  let ceresFolder = "C:/Dev/Chess/PGNs/Ceres"
  
  
  
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
            // No engine defs listed: use the tournament exactly as the file describes it. This
            // used to fall back to a folder of Ceres networks on one particular Windows machine,
            // which on any other machine - and on every Linux one - silently did nothing.
            tourny
        {tourny with MinMoveTimeInMS = 0; ConsoleOnly = true; DelayBetweenGames = TimeSpan.Zero }        
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
  
module RunOutput =

  /// Prepares a run's output folder and says what happened, returning the folder the
  /// run should write into ("" when it must write nothing).
  ///
  /// Runs before any engine is created. A run whose folder could not be prepared still
  /// completes — it just produces no files, and now says so instead of looking like a
  /// normal run that happened to write nothing.
  let report (label: string) (configured: string) : string =
      match PuzzleDataUtils.ensureOutputFolder configured with
      | PuzzleDataUtils.Ready(path, created) ->
          let suffix = if created then " (created)" else ""
          ConsoleUtils.greenConsole $"{label} -> {path}{suffix}"
          path
      | PuzzleDataUtils.NotConfigured ->
          ConsoleUtils.yellowConsole
              "No FailedPuzzlesOutputFolder set - no result files will be written."
          ""
      | PuzzleDataUtils.Failed(path, message) ->
          // exception messages already end in a period, so do not add another
          let reason = message.TrimEnd('.')
          ConsoleUtils.redConsole
              $"Cannot use output folder {path}: {reason}. The run continues, but no result files will be written."
          ""

module Eret =

  let timeConfig = UnionType.FixedTime (TimeSpan(0,0,5)) //10 seconds

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
        let escaped = data.FailedPuzzlesOutputFolder

        if escaped <> "" then
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
        let escaped = data.FailedPuzzlesOutputFolder
        if escaped <> "" then
          let datePart = DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
          let outputPath = Path.Combine(escaped, $"EretSummary_{datePart}.txt")
          try
            File.WriteAllText(outputPath, table);
            Console.WriteLine($"Console summary written to {outputPath}")
          with
          | ex ->
              Console.WriteLine($"Failed to write results to file: {ex.Message}")
        printfn "\n%s" table
    | EretError msg ->
        RuntimeUtilities.ConsoleUtils.redConsole $"\nERET Error: {msg}"

 
/// The processes attached to this console: 1 when the program has a console of its own (started
/// from Explorer rather than from a shell).
module ConsoleWindow =
  [<System.Runtime.InteropServices.DllImport("kernel32.dll")>]
  extern uint32 GetConsoleProcessList(uint32[] processList, uint32 processCount)
  [<System.Runtime.InteropServices.DllImport("kernel32.dll")>]
  extern nativeint GetConsoleWindow()
  [<System.Runtime.InteropServices.DllImport("user32.dll")>]
  extern bool IsWindowVisible(nativeint hWnd)

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

  // logs/eb-cli-<date>.log in the working directory, a new file each day and past 10 MB
  let logPath = Path.Combine(Environment.CurrentDirectory, "logs", "eb-cli-.log");
  
  /// <summary>
  /// Creates and configures the host for the application.
  /// </summary>
  let createHost() =
      Log.Logger <- LoggerConfiguration() // Create a Serilog logger configuration
        .MinimumLevel.Information() // Set the minimum log level
        //.WriteTo.Console() // Write to console
        .WriteTo.File(logPath, rollingInterval = Serilog.RollingInterval.Day, rollOnFileSizeLimit = true, fileSizeLimitBytes = 10_000_000L)
        .CreateLogger() // Create the logger
      Host.CreateDefaultBuilder()
          .ConfigureLogging(configureLogging)
          .Build()
        

  
  let runEretTest (path:string) =
    let normalizedPath = normalizePath path
    let loaded = loadEretConfig normalizedPath
    printfn "Processing ERET puzzle file: %s" path
    // Resolved once, before any engine loads, and carried on the config the writers
    // read. Empty means the run writes no files - the handler skips on that.
    let data =
      { loaded with
          FailedPuzzlesOutputFolder = RunOutput.report "ERET results" loaded.FailedPuzzlesOutputFolder }
    let time = UnionType.FixedTime (TimeSpan(0,0,data.TimeInSeconds))
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

  type PositionResult = {
      Depth: int; SDepth: int; Eval: ChessLibrary.MiscTypes.EvalType
      Nodes: int64; Nps: int64; Eps: float; Time: TimeSpan
      TBHits: int64; WDL: ChessLibrary.EngineTypes.WDL option
      Bestmove: string; PV: string; SanPV: string
  }

  /// An engine argument as a file: a path as given, or a bare def name from engineDefsFolder in
  /// eb-cli.json (CliSettings.resolveEngine).
  let private resolveEnginePath (enginePath: string) =
      CliSettings.resolveEngine File.Exists (normalizePath enginePath) (fun () -> CliSettings.current.Force())

  /// Resolves an engine path (JSON config or bare exe) to an EngineConfig, applying UCI option overrides.
  let private resolveEngineConfig (enginePath: string) (uciOptions: (string * string) list) =
      // Result.Ok/Error: an opened engine module has an Ok case of its own
      let normalizedEngine =
          match resolveEnginePath enginePath with
          | Result.Ok p -> p
          | Result.Error msg -> failwith msg
      let config =
          if normalizedEngine.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
              Configuration.JSON.readSingleEngineConfig normalizedEngine
          else
              TypesDef.CoreTypes.EngineConfig.EmptyWithPath normalizedEngine
      let options = System.Collections.Generic.Dictionary<string, obj>(config.Options)
      for (key, value) in uciOptions do
          options.[key] <- box value
      { config with Options = options }


  let runPuzzles (path:string) (jsonOut: string option) =
    let startedUtc = DateTime.UtcNow
    let runStopwatch = System.Diagnostics.Stopwatch.StartNew()
    let data = loadPuzzleConfig (normalizePath path)
    let normalizedPath = normalizePath data.PuzzleFile
    printfn "Processing Lichess puzzle file: %s" path
    // Resolved before any engine loads, so an unusable path costs seconds instead of a
    // whole run. Empty means "write nothing", and every writer below reads this binding.
    let outputFolder = RunOutput.report "Puzzle results" data.FailedPuzzlesOutputFolder
    let engineConfigs = 
        data.Engines 
        |> Seq.collect (mapToEngPuzzleConfig data.EngineFolder)
        |> ResizeArray

    // The record is the only holder of the database, so the runner can release it once the
    // samples are drawn (see JSONParser.loadPuzzleInput for why it is not built here).
    let puzzleInput =
        loadPuzzleInput(
            normalizedPath,
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
            data.Concurrency,
            data.IncludeFailedPuzzles,
            data.ScoreAllPositions )
    // Only the count outlives the run: the runner empties puzzleData once it has its samples.
    let puzzleDbCount = puzzleInput.puzzleData.Length
    printfn $"Loaded {puzzleDbCount:N0} puzzles from {normalizedPath}"

    // Engine failures (refused net, crash mid-sweep) arrive here as LichessError; counted so
    // the exit code below can say the run is incomplete.
    let engineErrors = ref 0
    let update (res: Lichess) =
        match res with
        | PuzzleResult score ->
            let correct = score.Correct
            let total = score.TotalNumber
            let failed = total - correct
            let name = score.Engine
            printfn "Puzzle result for %s: Correct: %d, Failed: %d" name correct failed
        | Done msg -> printfn "Puzzle done: %s" msg
        | Progress (processed, total, label) ->
            printf "\r  %s: %d / %d" label processed total
        | LichessError msg ->
            engineErrors.Value <- engineErrors.Value + 1
            RuntimeUtilities.ConsoleUtils.redConsole $"\nPuzzle Error: {msg}"
                  
    let ct = CancellationToken.None
    let scores =
        // One parser with the GUI (PuzzleRunners.parseSubTests). An unknown token refuses
        // the run: silently running the tokens that did parse produced a narrower run that
        // finished green under the config's own label.
        // A malformed Nodes value is a config error whatever Type says; refuse it with the
        // offending text instead of an unhandled FormatException (it used to be read lazily,
        // so an empty Type never touched it).
        let nodeList =
            try PuzzleDataUtils.parseNodes puzzleInput.nodes
            with ex ->
                RuntimeUtilities.ConsoleUtils.redConsole $"Puzzle Error: {ex.Message}"
                exit 1
        match PuzzleRunners.parseSubTests data.Type nodeList with
        | Result.Error unknown ->
            RuntimeUtilities.ConsoleUtils.redConsole $"
Puzzle Error: {PuzzleRunners.unknownSubTestsMessage unknown}"
            exit 1
        | Result.Ok [] ->
            printfn "No puzzle types specified, defaulting to both policy and value test"
            PuzzleRunners.runValueAndPolicyHeadTest(puzzleInput, update, ct)
        | Result.Ok subTests ->
            printfn "Puzzle types specified: %s" data.Type
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
        |> Seq.filter (fun e -> e.TotalNumber > 0 && (e.Type.Contains("Policy") || e.Type.StartsWith("pTop")))
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
        |> Seq.toList

    // No `Nodes > 1` clause: a search at one node is typed "Policy" by PuzzleEngineAgent
    // and lands in policyScores, so the clause never excluded a search - it only dropped
    // legitimate one-node Solve results, which the GUI kept. One rule for both paths:
    // a row belongs here if it produced results.
    let search =
        scores
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Search"))
        |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                         .ThenByDescending(fun e -> e.RatingAvg)
                         .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
        |> Seq.toList

    let solve =
        scores
        |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Solve"))
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
                        // the bm half needs the same guard as the am half: PlayUciMove
                        // no-ops on an unmatched move and the last SAN is then the
                        // opponent's setup move, written out as a legal-looking wrong bm
                        let bm = PuzzleDataUtils.sanOfMovePlayed boardBm cmd.Command cmd.CorrectMove
                        let aM = PuzzleDataUtils.sanOfMovePlayed boardAm cmd.Command cmd.MovePlayed
                        let policies = policyStr.Split(',')
                        let bmP, amP =
                            if policies.Length > 1 then
                                policies.[0].Trim(), policies.[1].Trim()
                            else
                                "", ""
                        let msg = $"{fen} bm {bm}; am {aM}; id \"Lichess id {puzzle.PuzzleId}, policy value for bestmove {bm}={bmP} and move played {aM}={amP}\"; other \"{cmd.CorrectMove},{cmd.MovePlayed}\""
                        sw.WriteLine(msg)
        
    let writeFailedPuzzlesToCsv (allScores: Score list list) (csvPath: string) =
        let allFailed =
            allScores
            |> Seq.concat
            |> Seq.collect (fun score -> score.FailedPuzzles |> Seq.map fst)
            |> Seq.distinctBy (fun p -> p.PuzzleId)
            |> Seq.toArray
        if allFailed.Length > 0 then
            use sw = new StreamWriter(csvPath)
            sw.WriteLine("PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags")
            for p in allFailed do
                sw.WriteLine($"{p.PuzzleId},{p.Fen},{p.Moves},{int p.Rating},{int p.RatingDeviation},{p.Popularity},{p.NbPlays},{p.Themes},{p.GameUrl},{p.OpeningTags}")
            printfn "  Failed puzzles CSV (%d unique): %s" allFailed.Length csvPath

    // Worst-case candidates nominated by the 1-node EstNodes formula, worst first.
    // Written in lichess CSV format so the file can feed a `search`-type config
    // directly for targeted real-search verification of the predicted hardest puzzles.
    let writeHardestByEstNodesCsv (allScores: Score list list) (csvPath: string) =
        let formatEstNodes (n: float) =
            if n < 10.0 then n.ToString("F2", CultureInfo.InvariantCulture)
            elif n < 1000.0 then n.ToString("F0", CultureInfo.InvariantCulture)
            elif n < 1_000_000.0 then (n / 1000.0).ToString("F1", CultureInfo.InvariantCulture) + "k"
            else (n / 1_000_000.0).ToString("F2", CultureInfo.InvariantCulture) + "M"
        let hardest =
            allScores
            |> Seq.concat
            |> Seq.collect (fun score -> score.HardestByEstNodes)
            |> Seq.sortByDescending snd
            |> Seq.distinctBy (fun (p, _) -> p.PuzzleId)
            |> Seq.truncate 50
            |> Seq.toArray
        if hardest.Length > 0 then
            use sw = new StreamWriter(csvPath)
            sw.WriteLine("PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags")
            for (p, _) in hardest do
                sw.WriteLine($"{p.PuzzleId},{p.Fen},{p.Moves},{int p.Rating},{int p.RatingDeviation},{p.Popularity},{p.NbPlays},{p.Themes},{p.GameUrl},{p.OpeningTags}")
            printfn "  Hardest-by-EstNodes CSV (top %d, worst first): %s" hardest.Length csvPath
            printfn "  Top 10 worst-case candidates by estimated nodes to find the correct move:"
            for (p, estN) in hardest |> Seq.truncate 10 do
                printfn "    %-12O  rating %4d  est %-7s  %s" p.PuzzleId (int p.Rating) (formatEstNodes estN) p.GameUrl

    // One order for the theme tables, the paired table and both JSON writes: A must
    // mean the same net in all of them or the same comparison reads with its sign
    // flipped in one table and not the other.
    let engineNamesInConfigOrder =
        engineConfigs
        |> Seq.map (fun (cfg: ChessLibrary.TypesDef.CoreTypes.EngineConfig, _) -> cfg.Name)
        |> Seq.toList

    // Paired (McNemar) stats. Same slices as the theme tables, but at slice level the
    // solved/failed sets survive, so the discordant counts - and with them the tight test -
    // are available. Empty for single-net runs.
    //
    // Computed ONCE, here: the text summary renders it and both JSON writes embed it. It
    // sits outside the output-folder guard on purpose - a run configured to write no files
    // still has a result worth printing, and only the "full table" pointer needs a folder.
    let allScoresForCross = Seq.concat [ policyScores; valueScores; search; solve ]
    // An empty result is normal (a single-net run has nothing to compare); a failure is
    // not. The outcome carries both so neither the JSON nor the console can confuse them.
    let pairedOutcome =
        try PuzzlePaired.outcomeOf (PuzzlePaired.computeOrdered engineNamesInConfigOrder allScoresForCross)
        with ex ->
            RuntimeUtilities.ConsoleUtils.redConsole $"Paired comparison failed: {ex.Message}"
            PuzzlePaired.failedOutcome
    let pairedComparisons = pairedOutcome.Comparisons

    // One console line, same rule as the theme headline: point at the file, do not reprint
    // it. Thin rows are excluded from the headline, not from the table: z = 2.0 off four
    // disagreements is exactly the kind of number that must not lead a run report.
    match
        pairedComparisons
        |> List.filter (fun c -> c.Discordant >= PuzzlePaired.ThinDiscordance)
        |> List.sortByDescending (fun c -> abs c.Z) with
    | strongest :: _ when abs strongest.Z >= 2.0 ->
        printfn ""
        printfn "--- Paired comparison (McNemar) ---"
        // same naming rule as the table: a cross-engine pair shares a net name, and
        // "netX vs netX" in the headline reads as a bug in the tool
        let nameA, nameB =
            PuzzlePaired.sideNames strongest.NetA strongest.NetB strongest.EngineA strongest.EngineB
        printfn "  Strongest: %s vs %s, %s @ rating %d: %+.1f pp, z=%+.2f (%d discordant of %d)"
            nameA nameB strongest.Type strongest.RatingGroup
            strongest.DeltaPp strongest.Z strongest.Discordant strongest.N
    | _ -> ()

    // One reading for both JSON writes: taking Elapsed twice reported two different
    // durations for one run in the two files.
    let elapsedSeconds = runStopwatch.Elapsed.TotalSeconds

    // Prepared at run start; empty when nothing should be written.
    let escaped = outputFolder
    let table = createCombinedScoresTable normalizedPath policyScores valueScores search solve
    printfn "%s" table

    if escaped <> "" then
        let filenameFriendlyDate = DateTime.Now.ToString("yyyy-MM-dd_HH-mm", System.Globalization.CultureInfo.InvariantCulture)        
        // The folder is prepared at run start, but it can still disappear during a run
        // that takes hours - a dropped share, a cleanup job. Every writer is wrapped so
        // one failure costs its own file and nothing else: not the remaining files, not
        // the --json output an external tool asked for, and not the process.
        let mutable writeFailures = 0
        let tryWrite (what: string) (action: unit -> unit) =
            try action ()
            with ex ->
                writeFailures <- writeFailures + 1
                RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write {what}: {ex.Message}"

        //let datePart = DateTime.Now.ToString("yyyyMMdd_HHmmss", CultureInfo.InvariantCulture)
        let fileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.epd")
        tryWrite fileName (fun () ->
            let boardBm = Chess.Board()
            let boardAm = Chess.Board()
            use sw = File.AppendText(fileName)
            writeToFile policyScores sw boardBm boardAm
            writeToFile valueScores sw boardBm boardAm
            writeToFile search sw boardBm boardAm
            writeToFile solve sw boardBm boardAm)

        let csvFileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.csv")
        tryWrite csvFileName (fun () ->
            writeFailedPuzzlesToCsv [policyScores; valueScores; search; solve] csvFileName)

        let estCsvFileName = Path.Combine(escaped, $"estNodesHardest_{filenameFriendlyDate}.csv")
        tryWrite estCsvFileName (fun () ->
            writeHardestByEstNodesCsv [policyScores] estCsvFileName)

        tryWrite "cross-engine files" (fun () ->
            PuzzleCrossEngine.writeCrossEngineFiles escaped filenameFriendlyDate allScoresForCross)

        // Per-theme breakdown, shared with the GUI so both runs produce the same files.
        // Returns the summary appended to LichessSummary_<stamp>.txt below.
        let themeOutput =
            try
                PuzzleThemes.writeThemeFiles
                    escaped
                    filenameFriendlyDate
                    engineNamesInConfigOrder
                    allScoresForCross
            with ex ->
                writeFailures <- writeFailures + 1
                RuntimeUtilities.ConsoleUtils.redConsole $"Per-theme comparison failed: {ex.Message}"
                { Summary = ""; CsvPath = ""; Headline = "" }
        // the tables go to the summary file - dumping a dozen of them on someone who
        // ran a puzzle test buries the result they came for
        if themeOutput.Summary <> "" then
            printfn "
--- Per-Theme Comparison ---"
            printfn "%s" themeOutput.Headline
            printfn "  Full tables: %s" (Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt"))
            printfn "  All themes:  %s" themeOutput.CsvPath
        let themeSummary = themeOutput.Summary

        let pairedSummary =
            // counted like every other writer: the summary file loses a section, and the
            // green "Result files written" line below must not claim otherwise. A silent
            // `with _ -> ""` here hid the loss entirely.
            if pairedOutcome.Failed then
                writeFailures <- writeFailures + 1
                ""
            else
                try PuzzlePaired.render pairedComparisons
                with ex ->
                    writeFailures <- writeFailures + 1
                    RuntimeUtilities.ConsoleUtils.redConsole $"Paired table rendering failed: {ex.Message}"
                    ""
        if pairedSummary <> "" then
            printfn "  Full paired table: %s" (Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt"))

        let testTypeInfo =
            let tokens = if String.IsNullOrWhiteSpace data.Type then [||] else data.Type.ToLower().Split(',') |> Array.map (fun t -> t.Trim())
            String.Join("-", tokens)
        let engineCount = engineConfigs.Count
        //write table to file with date and time
        let tableFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt")
        //let tableFileName = Path.Combine(escaped, $"LichessPuzzleScore_{datePart}.txt")
        tryWrite tableFileName (fun () ->
            use tableWriter = new StreamWriter(tableFileName)
            tableWriter.WriteLine(table)
            if pairedSummary <> "" then
                tableWriter.WriteLine(pairedSummary)
            if themeSummary <> "" then
                tableWriter.WriteLine(themeSummary))

        // Always write the machine-readable twin beside the text summary, same basename.
        // Trend tooling reads these; nothing should have to parse the human-formatted table,
        // whose column layout has drifted across many releases. See Console/PuzzleJsonSchema.md.
        let pairedJsonFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.json")
        try
            PuzzleJsonOutput.buildResultWithPaired
                pairedOutcome
                normalizedPath
                puzzleDbCount
                data.SampleSize
                data.MinRating
                data.MaxRating
                data.PuzzleFilter
                data.RatingGroups
                startedUtc
                elapsedSeconds
                scores
            |> PuzzleJsonOutput.writeToFile pairedJsonFileName
        with ex ->
            writeFailures <- writeFailures + 1
            RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write JSON summary to {pairedJsonFileName}: {ex.Message}"

        // Naming the folder at the end makes a mistyped path obvious instead of leaving a
        // run that looks finished. It must never claim success over a red line above it -
        // the whole point is that a run which wrote nothing does not read as a normal one.
        if writeFailures = 0 then
            RuntimeUtilities.ConsoleUtils.greenConsole $"Result files written to {escaped}"
        else
            RuntimeUtilities.ConsoleUtils.yellowConsole
                $"{writeFailures} output file(s) failed to write (see above). Folder: {escaped}"

    // Optional structured JSON output for external tooling (e.g. Python tuner).
    // See Console/PuzzleJsonSchema.md for the public schema contract.
    match jsonOut with
    | Some out ->
        try
            let result =
                PuzzleJsonOutput.buildResultWithPaired
                    pairedOutcome
                    normalizedPath
                    puzzleDbCount
                    data.SampleSize
                    data.MinRating
                    data.MaxRating
                    data.PuzzleFilter
                    data.RatingGroups
                    startedUtc
                    elapsedSeconds
                    scores
            PuzzleJsonOutput.writeToFile out result
            printfn "  JSON results written: %s" out
            // the file is still written and still useful, but a consumer reading `paired`
            // must be told it is empty for the wrong reason
            if pairedOutcome.Failed then
                RuntimeUtilities.ConsoleUtils.yellowConsole
                    "  (paired comparison failed - that section of the JSON is empty)"
        with ex ->
            RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write JSON results to {out}: {ex.Message}"
    | None -> ()
    // Exit code. A run in which an engine failed (refused network, crash mid-sweep) used to
    // end with 0 like a finished one, so a script could not tell them apart without parsing
    // the log. The files for the groups that did run are written above; the code says the
    // run is incomplete. No scores at all is the same failure even without an error line.
    let scored = scores |> Seq.filter (fun sc -> sc.TotalNumber > 0) |> Seq.length
    if engineErrors.Value > 0 || scored = 0 then
        RuntimeUtilities.ConsoleUtils.redConsole
            $"Puzzle run incomplete: {engineErrors.Value} engine error(s), {scored} score(s) recorded (exit code 1)"
        exit 1

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
    
    /// Compact result-reason labels for console output.
    let shortReason (reason: ChessLibrary.MiscTypes.ResultReason) : string =
        match reason with
        | ChessLibrary.MiscTypes.Checkmate -> "mate"
        | ChessLibrary.MiscTypes.Stalemate -> "stale"
        | ChessLibrary.MiscTypes.AdjudicateTB -> "tb-known"
        | ChessLibrary.MiscTypes.AdjudicateMaterial -> "insuf-mat"
        | ChessLibrary.MiscTypes.ExcessiveMoves -> "50-moves"
        | ChessLibrary.MiscTypes.Repetition -> "rep-draw"
        | ChessLibrary.MiscTypes.AdjudicatedEvaluation -> "eval-agree"
        | ChessLibrary.MiscTypes.ForfeitLimits -> "time-limit"
        | ChessLibrary.MiscTypes.Cancel -> "cancelled"
        | ChessLibrary.MiscTypes.Illegal -> "illegal"
        | ChessLibrary.MiscTypes.NotStarted -> "not-started"
        | ChessLibrary.MiscTypes.AdjudicatedByUser -> "adj-user"
        | ChessLibrary.MiscTypes.Resignation -> "resigns"
        | ChessLibrary.MiscTypes.Disconnected _ -> "disconn"
        | ChessLibrary.MiscTypes.Stalled _ -> "stalled"

    /// Define the MailboxProcessor for handling updates asynchronously
    let createUpdateProcessor (verbose:bool) =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () =
                async {
                    // Wait for a message
                    let! update = inbox.Receive()
                    // Process the update
                    match update with
                    | GameStarted _ -> ()  // white-player info already shown in StartOfGame line
                    | EndOfGame result ->
                        let time = float result.GameTime / 1000.0
                        // Named, not numbered: the result carries no game number, and the last STARTED number is wrong once games overlap.
                        printfn "\u25C0 %s vs %s  %-7s  %-12s  %dm %.1fs"
                            result.Player1 result.Player2 result.Result (shortReason result.Reason) result.Moves time
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
                    | StartOfGame startGameInfo ->
                        printfn "\u25B6 G%d  %s vs %s"
                            startGameInfo.CurrentGameNr
                            startGameInfo.WhitePlayer.Name
                            startGameInfo.BlackPlayer.Name
                    | EndOfTournament info -> printfn "End Of Tournament:\n%s" (info.Summary())
                    | StartOfTournament info ->
                        printfn "\nStart of tournament:\nNumber of games to play: %d \nSummary:\n%s\n" info.NumberOfGames (info.Tournament.Value.MinSummary())
                    | MessagesFromEngine (player, message) -> 
                        if verbose then
                            printfn "MessagesFromEngine: Player - %s, Message - %s" player message
                    | PairingList pairings ->
                        printfn "%s" (PairingHelper.getOpeningPairs (pairings |> Seq.toList))
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

    let runner = Manager.Runner(logger, printUpdate, false)
    runner.AddTournament tourny
    // true when the run went through: tournamentjson's exit code
    let mutable failed = false
    let start = Stopwatch.GetTimestamp()
    let formatHms (ts: TimeSpan) = 
        let hours = int ts.TotalHours 
        let minutes = ts.Minutes 
        let seconds = ts.Seconds 
        sprintf "%dh %dm %ds" hours minutes seconds

    try
        let results = runner.Run()
        // a run that stopped with an error has said why; no "completed" summary after it
        failed <- runner.Failure.IsSome
        if not failed then
          let endTime = Stopwatch.GetElapsedTime start
          let gamesPlayed = results |> Seq.length
          let msg = $"Tournament completed - Duration: {formatHms endTime}, Games: {gamesPlayed}, Parallel: {tourny.TestOptions.NumberOfGamesInParallel}"
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
    |ex ->
        printfn "Caught an exception: %s" ex.Message
        failed <- true
    not failed

  // ---------- Batch piece-value analysis over a folder of nets ----------
  // For each net: self-play (RR) at the template's time control - nodes=1 in the usual template -
  // then outcome + pgneval endgame regressions.
  // Reuses runTournament and the pvfit helpers. Resumable (skips a net whose PGN is
  // already complete) and fault-tolerant (a failing net is logged and skipped).

  let private pvCountGames (pgn: string) : int =
      if not (File.Exists pgn) then 0
      else File.ReadLines pgn |> Seq.filter (fun l -> l.StartsWith("[Result ")) |> Seq.length

  /// Endgame (<=16 pieces) implied values (N,B,R,Q with pawn=1) from outcome regression.
  let private pvOutcomeEndgame (pgn: string) : (float * float * float * float) option =
      let rows = Commands.Values.pvCollectOutcomeRows pgn 400000 |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
      if rows.Length < 50 then None
      else
          let b = Commands.Values.pvFitTanh (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
          if abs b.[0] < 1e-9 then None
          else Some (b.[1] / b.[0], b.[2] / b.[0], b.[3] / b.[0], b.[4] / b.[0])

  /// Endgame implied values (N,B,R,Q with pawn=1) from PGN-eval regression.
  let private pvPgnEvalEndgame (pgn: string) : (float * float * float * float) option =
      let rows = Commands.Values.pvCollectPgnEvalRows pgn 400000 None |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
      if rows.Length < 50 then None
      else
          let b = Commands.Values.pvFitL1 (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
          if abs b.[1] < 1e-9 then None
          else Some (b.[2] / b.[1], b.[3] / b.[1], b.[4] / b.[1], b.[5] / b.[1])

  let runPvBatch (p: CliParser.PvBatchParams) =
    try
        let templatePath = normalizePath p.Template
        if not (File.Exists templatePath) then failwithf "Template tournament not found: %s" templatePath
        let netDir = normalizePath p.NetFolder
        if not (Directory.Exists netDir) then failwithf "Net folder not found: %s" netDir
        let rounds = p.Rounds |> Option.defaultValue 1000
        let outDir = (p.Out |> Option.defaultValue (Path.Combine(netDir, "pv_batch"))) |> normalizePath
        Directory.CreateDirectory(outDir) |> ignore

        match JSON.readTournamentJson templatePath with
        | None -> printfn "Could not read template tournament: %s" templatePath
        | Some templateTourny ->
            let templateEngines = JSON.readEngineDefs templateTourny.EngineSetup.EngineDefFolder templateTourny.EngineSetup.EngineDefList
            match templateEngines with
            | [] -> printfn "Template tournament has no engine def to use as a template."
            | templateEngine :: _ ->
                let nets = Directory.GetFiles(netDir, "*.onnx") |> Array.sort
                printfn "Found %d nets in %s\nOutput: %s\n" nets.Length netDir outDir
                let summaryPath = Path.Combine(outDir, "summary.csv")
                if not (File.Exists summaryPath) then
                    File.WriteAllText(summaryPath, "net,mode,knight,bishop,rook,queen\n")

                use host = createHost()
                host.Start()
                let loggerFactory = host.Services.GetService(typeof<ILoggerFactory>) :?> ILoggerFactory
                let logger = loggerFactory.CreateLogger("EngineBattle pvbatch")

                let cloneEngine (name: string) (netPath: string) =
                    let opts = System.Collections.Generic.Dictionary<string, obj>(templateEngine.Options)
                    opts.["Network"] <- box netPath
                    { templateEngine with Name = name; IsChallenger = false; NetworkPath = netDir; Options = opts }

                let fmt (v: float) = sprintf "%.2f" v
                for net in nets do
                    let baseName = Path.GetFileNameWithoutExtension net
                    try
                        let netPath = net.Replace("\\", "/")
                        let pgn = Path.Combine(outDir, baseName + ".pgn").Replace("\\", "/")
                        printfn "\n==== %s ====" baseName
                        let have = pvCountGames pgn
                        if have >= rounds then
                            printfn "  PGN already has %d games — skipping tournament." have
                        else
                            let engA = cloneEngine (sprintf "PVB-%s-a" baseName) netPath
                            let engB = cloneEngine (sprintf "PVB-%s-b" baseName) netPath
                            let tourny =
                                { templateTourny with
                                    EngineSetup = { templateTourny.EngineSetup with Engines = [ engA; engB ] }
                                    TournamentMode = "RR"
                                    Rounds = rounds
                                    PgnOutPath = pgn
                                    ConsoleOnly = true
                                    MinMoveTimeInMS = 0
                                    DelayBetweenGames = TimeSpan.Zero
                                    // Self-play loads two copies of the net per concurrency level, so concurrency 2
                                    // means 4 simultaneous TensorRT engines — big nets OOM the GPU. Clamp to 1
                                    // regardless of the template; this also enables PreventMoveDeviation.
                                    TestOptions = { templateTourny.TestOptions with NumberOfGamesInParallel = 1 }
                                    PreventMoveDeviation = true }
                            // the template's time control, which the batch does not override
                            printfn "  running %d-game self-play at %s..." rounds (tourny.TimeControlTextForPlayer engA.TimeControlID)
                            runTournament tourny logger |> ignore

                        match pvOutcomeEndgame pgn with
                        | Some (n, b, r, q) ->
                            printfn "  outcome endgame:  P 1.00  N %s  B %s  R %s  Q %s" (fmt n) (fmt b) (fmt r) (fmt q)
                            File.AppendAllText(summaryPath, sprintf "%s,outcome,%.2f,%.2f,%.2f,%.2f\n" baseName n b r q)
                        | None -> printfn "  outcome endgame:  (insufficient data)"
                        match pvPgnEvalEndgame pgn with
                        | Some (n, b, r, q) ->
                            printfn "  pgneval endgame:  P 1.00  N %s  B %s  R %s  Q %s" (fmt n) (fmt b) (fmt r) (fmt q)
                            File.AppendAllText(summaryPath, sprintf "%s,pgneval,%.2f,%.2f,%.2f,%.2f\n" baseName n b r q)
                        | None -> printfn "  pgneval endgame:  (insufficient data)"
                    with ex ->
                        printfn "  ERROR on %s: %s" baseName ex.Message

                host.StopAsync().Wait()
                printfn "\n==== BATCH DONE ====\nSummary: %s\n" summaryPath
                if File.Exists summaryPath then printfn "%s" (File.ReadAllText summaryPath)
    with
    | :? CustomException.EngineStartupException as ex ->
        // Refused or dead engine: the same exit code cmp and the puzzle command give.
        ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Aborted (pvbatch): %s" ex.Message)
        exit 1
    | ex ->
        printfn "Error during pvbatch: %s" ex.Message

  /// Book Evaluation from the command line: the GUI page's run (ChessLibrary BookEvaluation), its
  /// output file beside the input under BookEvals/ unless --out says otherwise. Exit 1 when an
  /// engine stopped the run.
  let runBookEval (p: CliParser.BookEvalParams) =
    let input = Path.GetFullPath(normalizePath p.Input)
    if not (File.Exists input) then
        printfn "File not found: %s" input
        exit 1
    let ext = Path.GetExtension(input).ToLowerInvariant()
    if ext <> ".pgn" && ext <> ".epd" then
        printfn "bookeval reads .pgn or .epd files, not %s" ext
        exit 1
    let limitOf (e: CliParser.BookEvalEngine) =
        match e.Nodes, e.MoveTimeMs, p.DefaultNodes, p.DefaultMoveTimeMs with
        | Some n, _, _, _ -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit n
        | _, Some ms, _, _ -> TimeControlTypes.TimeControlCommands.SearchLimit.TimeLimit ms
        | _, _, Some n, _ -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit n
        | _, _, _, Some ms -> TimeControlTypes.TimeControlCommands.SearchLimit.TimeLimit ms
        | _ -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit 10000
    let engines =
        try p.Engines |> List.map (fun e -> ({ Config = resolveEngineConfig e.Engine []; Limit = limitOf e } : ChessLibrary.BookEvaluation.BookEngine))
        with ex -> printfn "%s" ex.Message; exit 1
    let filter =
        try ChessLibrary.BookEvaluation.parseFilter p.MinEval p.MaxEval p.MaxDiff engines.Length
        with :? FormatException -> printfn "--min, --max and --maxdiff take numbers"; exit 1
    let label = ChessLibrary.BookEvaluation.engineLabel engines
    let outPath =
        match p.Out with
        | Some o -> Path.GetFullPath(normalizePath o)
        | None -> Path.Combine(Path.GetDirectoryName input, "BookEvals", sprintf "BookEval_%s_%s%s" (Path.GetFileName input) label ext)
    let take (xs: seq<'a>) = match p.Count with Some n -> Seq.truncate n xs | None -> xs
    printfn "Book evaluation: %s" input
    printfn "Engines: %s" label
    printfn "Filter: |eval| %g-%g cp%s" filter.MinEval filter.MaxEval (if engines.Length > 1 then sprintf ", spread below %g cp" filter.MaxDiff else "")
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable lastShown = 0
    let progress = Action<int, int>(fun done' total ->
        // one line per 5 % (and the last), not one per position
        if done' = total || done' * 20 / total > lastShown * 20 / total then
            lastShown <- done'
            printf "
  %d / %d positions (%.0fs)   " done' total sw.Elapsed.TotalSeconds)
    use cts = new Threading.CancellationTokenSource()
    // the first Ctrl+C stops and writes what passed; a second one ends the program at once
    Console.CancelKeyPress.Add(fun a -> a.Cancel <- not cts.IsCancellationRequested; cts.Cancel())
    let passed, evaluated, removed, skipped, failure, cancelled, total =
        if ext = ".pgn" then
            let games = ChessLibrary.FullPGNParser.parsePgnFileWithRaw input |> take |> Seq.toList
            let o = ChessLibrary.BookEvaluation.evaluatePgns engines filter games progress cts.Token
            ChessLibrary.BookEvaluation.writePgns outPath o.Results
            o.Results.Count, o.Evaluated, o.Removed, o.Skipped, o.Failure, o.Cancelled, games.Length
        else
            let epds = ChessLibrary.EPDExtractor.readEPDs input |> take |> Seq.toList
            let o = ChessLibrary.BookEvaluation.evaluateEpds engines filter epds progress cts.Token
            ChessLibrary.BookEvaluation.writeEpds outPath o.Results
            o.Results.Count, o.Evaluated, o.Removed, o.Skipped, o.Failure, o.Cancelled, epds.Length
    // the page's Results table, row for row (BookEvaluation.summaryRows)
    let rows =
        ChessLibrary.BookEvaluation.summaryRows (Path.GetFileName input) total passed evaluated removed skipped
            failure cancelled engines p.MinEval p.MaxEval p.MaxDiff sw.Elapsed outPath
    printfn ""
    printfn ""
    printfn "Results"
    for label, value in rows do
        let line = sprintf "  %-31s %s" label value
        if label = "Status" then ConsoleUtils.printInColor (if failure.IsSome then ConsoleColor.Red else ConsoleColor.Yellow) line
        else printfn "%s" line
    if failure.IsSome then exit 1

  /// The console's command list: `help`, and what a run without arguments prints.
  let printConsoleHelp () =
    printfn ""
    printfn "EngineBattle - Chess engine tournament, analysis, and puzzle testing"
    printfn ""
    printfn "Usage: eb-cli <command> [arguments]"
    printfn ""
    printfn "Commands:"
    printfn "  match [options]                          An engine match from one command line (match -help)"
    printfn "  tournamentjson, tournament, t <config> [--append]  Run a tournament from JSON config (--append: a new cup/Swiss/ladder may start in a PGN that has games)"
    printfn "  puzzlejson, puzzle, p <config> [--json <path>]"
    printfn "                                          Run puzzle evaluation from JSON config"
    printfn "                                          --json <path>: write structured results JSON for tooling"
    printfn "  eretjson, eret <config>                 Run ERET evaluation from JSON config"
    printfn "  mkdef, md <engine.exe> [options]        Write an engine def from what the engine answers to 'uci'"
    printfn "                                          --out <folder> (default: current), --net <file> (into the engine's"
    printfn "                                          network option + NetworkPath), --tb <folder>, --base <def.json>,"
    printfn "                                          --uci <name> <value> (repeatable), --print, --force, --timeout <s>"
    printfn "  gendefs, gd <template.json> [--net F|--nets D] [--out D] [--dry-run] [--force]"
    printfn "                                          Defs for training checkpoints, an existing def as the template"
    printfn "  puzzletrend, pt [<folder>] [--arm S] [--type S] [--rg N] [--min-steps N] [--csv F]"
    printfn "                                          Per-arm step curves from many puzzle runs"
    printfn "  pvcombo <games.pgn>                     Material-imbalance report by material signature"
    printfn "  analyze, a <engine> [fen] [options]      Analyze a position with an engine"
    printfn "  compare, cmp <e1> <e2> [options]         Compare two engines side-by-side"
    printfn "  piecevalues, pv, values <engine> [fen] [options]"
    printfn "                                          Contextual piece values via leave-one-out net eval"
    printfn "                                          (shares --fen/--moves/--nodes/--uci; defaults to nodes=1)"
    printfn "  piecevaluefit, pvfit <engine> <positions.epd|.pgn> [options]"
    printfn "                                          Net's implied piece values via regression over many"
    printfn "                                          positions (--sample N, --nodes N, --by-phase, --uci K V)"
    printfn "                                          --outcome: DeepMind-style fit on PGN game results"
    printfn "                                                     (tanh model, no engine eval)"
    printfn "                                          --pgneval: fit on PGN embedded eval (wv=, pawns,"
    printfn "                                                     no engine; coefficients in pawns)"
    printfn "  pvbatch <templateTournament.json> <netFolder> [--rounds N] [--out DIR]"
    printfn "                                          Per net in the folder: self-play at the template's"
    printfn "                                          time control + outcome/pgneval regressions -> summary.csv"
    printfn "  benchmark, bench, b <config>            Run engine benchmark"
    printfn "  tune <config>                           Run Bayesian parameter tuner"
    printfn "  redash <config>                         Regenerate BO dashboard from saved state"
    printfn "  pgnsummary, pgn, ps <pgnFile>           Analyze PGN game terminations"
    printfn "  pgncheck, pc <pgnFile>                  Parser health check: games, plies, throughput"
    printfn "  pgnstrip, strip <pgnFile> [--out F] [--force] [--skip-empty] [--skip-invalid]  Write the games without comments, variations or NAGs (default <name>_stripped.pgn)"
    printfn "  pgnvalidate, pgnv <pgnFile> [--csv F] [--threads N]  Replay every game: illegal/ambiguous moves (errors), non-standard SAN (warnings)"
    printfn "  bookeval, be <book.pgn|epd> --engine <def|exe> [--nodes N|--movetime MS] [--engine ...]"
    printfn "                                          Keep the openings every engine scores within --min/--max cp"
    printfn "                                          (default 80-100) and agree on within --maxdiff (40); a limit"
    printfn "                                          after an --engine is its own, before the first everyone's;"
    printfn "                                          --count N openings, --out F (default BookEvals/ beside the book)"
    printfn "  deviations, dev <pgnFile>               Self-consistency and position deviations from PGN"
    printfn "  query, q <fen|startpos> [options]       Position query as JSON (status, legal moves, attackers,"
    printfn "                                          pins, insights, SEE); --epd <file> for a batch"
    printfn "  tb, tablebase <fen> [--tb <folder>] [--json] [--ignore-clock]"
    printfn "                                          Every move from the Syzygy tables: win, draw, loss with DTZ"
    printfn "                                          (folder: --tb, EB_SYZYGY_PATH or eb-cli.json; clock counts unless --ignore-clock)"
    printfn "  elo, e <pgnFile>                        Show ELO ratings and results from PGN"
    printfn "  speed, sp <pgnFile>                     Show speed statistics from PGN"
    printfn "  validate, v <config>                    Validate a tournament config without running"
    printfn "  perft <depth> [sampleSize]              Run perft move generation test"
    printfn "  gui [page] [port]                       Launch WebGUI (default: tournament, port 5018)"
    printfn "  help, h                                 Show this help message"
    printfn ""
    printfn "Settings: eb-cli.json next to this program (optional): engineDefsFolder (a bare engine name,"
    printfn "  e.g. 'a Lc0_BT4_332', is looked up there), syzygyPath (tb), puzzleResultsFolder (puzzletrend)."
    printfn ""
    printfn "Diagnostics:"
    printfn "  enginecheck, ec <engine> [options]      Check that an engine follows the UCI protocol and behaves in"
    printfn "                                          the analysis pages; --only <groups> (startup,options,positions,"
    printfn "                                          limits,info,stop,ponder,edge,quit,analysis), --nodes N |"
    printfn "                                          --movetime MS, --rounds R, --delay MS, --stop-delay MS,"
    printfn "                                          --moves, --uci K V"
    printfn ""
    printfn "Analyze options:"
    printfn "  --fen S        Set position (quoted FEN string)"
    printfn "  --moves M...   Append moves to position (e.g. --moves d2d4 d7d5 c2c4)"
    printfn "  --nodes N      Search N nodes (default: 1000000)"
    printfn "  --movetime N   Search for N milliseconds"
    printfn "  --depth N      Search to depth N"
    printfn "  --args S       Override engine command-line arguments (e.g. dag-preview)"
    printfn "  --uci K V      Set any UCI option (repeatable, e.g. --uci Backend onnx-trt)"
    printfn "  --options       Show all UCI options supported by the engine and exit"
    printfn ""
    printfn "Compare options:"
    printfn "  --fen S         Set position (quoted FEN string)"
    printfn "  --positions F   EPD file with multiple positions"
    printfn "  --nodes N       Search N nodes (default: 1000000)"
    printfn "  --movetime N    Search for N milliseconds"
    printfn "  --depth N       Search to depth N"
    printfn "  --threshold CP  Only show positions with eval diff >= CP"
    printfn "  --uci1 K V      Set UCI option for engine 1 (repeatable)"
    printfn "  --uci2 K V      Set UCI option for engine 2 (repeatable)"
    printfn ""
    printfn "Examples:"
    printfn "  t C:/path/to/tournament.json"
    printfn "  v C:/path/to/tournament.json"
    printfn "  p C:/path/to/puzzle.json"
    printfn "  pgn C:/path/to/games.pgn"
    printfn "  elo C:/path/to/games.pgn"
    printfn "  sp C:/path/to/games.pgn"
    printfn "  a engine.json startpos --nodes 100000"
    printfn "  a C:/path/to/engine.exe startpos --depth 15"
    printfn "  a engine.json \"fen string\" --movetime 5000 --uci Threads 2"
    printfn "  cmp engine1.json engine2.json --nodes 100000"
    printfn "  cmp engine1.json engine2.json --positions test.epd --depth 20"
    printfn "  cmp engine1.exe engine2.exe --positions test.epd --threshold 0.5"
    printfn "  gui"
    printfn "  gui singleEngineAnalysis"
    printfn "  gui 5020"
    printfn ""

  /// Started from Explorer (a double-click): the console window is the program's own and closes
  /// with it, so wait for Enter - otherwise the text is gone before it can be read.
  /// Only a window someone can see: a program that starts this one with a hidden console
  /// (CreateNoWindow) also leaves it alone on its console, and nobody could press Enter there.
  let waitIfOwnConsole () =
    if OperatingSystem.IsWindows() && not Console.IsInputRedirected then
      try
        let window = ConsoleWindow.GetConsoleWindow()
        if ConsoleWindow.GetConsoleProcessList(Array.zeroCreate 4, 4u) <= 1u
           && window <> 0n && ConsoleWindow.IsWindowVisible window then
          printfn "Press Enter to close."
          Console.ReadLine() |> ignore
      with _ -> ()

  /// <summary>
  /// The main entry point for the application.
  /// </summary>
  [<EntryPoint>]
  let main argv =
    // Invariant culture before anything formats or parses a number — see WebGUI/Program.cs
    // for the reproduction. This side writes the PGN files, where a comma decimal would not
    // just look wrong but corrupt what every other tool reads back.
    Globalization.CultureInfo.DefaultThreadCurrentCulture <- Globalization.CultureInfo.InvariantCulture
    Globalization.CultureInfo.DefaultThreadCurrentUICulture <- Globalization.CultureInfo.InvariantCulture
    Threading.Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture
    Threading.Thread.CurrentThread.CurrentUICulture <- Globalization.CultureInfo.InvariantCulture

    // `match`, or a command line that starts with an option (so a tool that runs matches can run
    // this program; no other verb starts with '-', and a mistyped option then gets match's error
    // and suggestion): MatchMode owns stdout, so it goes before anything prints.
    match List.ofArray argv with
    | first :: rest when first = "match" || (first.StartsWith "-" && not (List.contains first [ "-h"; "--help"; "--version" ])) ->
        Console.OutputEncoding <- System.Text.Encoding.UTF8
        let viaVerb = first = "match"
        exit (MatchMode.run (if viaVerb then rest else List.ofArray argv) viaVerb)
    | _ -> ()

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
        // The old verbs, while their commands move to Cli one group at a time (docs/EbCliRewrite.md):
        // a command not moved yet hands its arguments to the verb it replaces.
        let runLegacy (tokens: string list) =
            let cliArgs = CustomParser.parse (Array.ofList ("eb-cli" :: tokens))
            for arg in cliArgs do
                match arg with
                | Verb (PvBatch p) ->
                    runPvBatch p
                | Verb (PuzzleJson (path, jsonOut)) ->
                    runPuzzles path jsonOut
                | Verb (Eret path) ->                     
                    runEretTest path
                | Verb (Tournament (configFile, append)) ->
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
                        let tournament =
                            {tourny with
                                EngineSetup = engineSetup
                                MinMoveTimeInMS = 0
                                ConsoleOnly = true
                                DelayBetweenGames = TimeSpan.Zero
                            }
                        tournament.AppendToPgn <- append
                        if tournament.TournamentMode.Equals("Ladder", StringComparison.OrdinalIgnoreCase) then
                          printfn "Ladder mode: %d engines, %d game pairs per match" engineList.Length (if obj.ReferenceEquals(tournament.LadderOptions, null) then 4 else tournament.LadderOptions.GamePairsPerMatch)
                        for warning in Validation.duplicateOpeningWarnings tournament do
                          ConsoleUtils.printInColor ConsoleColor.Yellow warning
                        // AllowPondering: every engine started once first, to see which can ponder
                        let enginesOk = not tournament.AllowPondering || (let ok, _, _ = Tournament.TournamentUtils.checkPonderingEngines tournament in ok)
                        if enginesOk then
                          printfn "Running tournament with config file: %s" configFile
                          use host = createHost()
                          host.Start()
                          let loggerFactory = host.Services.GetService(typeof<ILoggerFactory>) :?> ILoggerFactory
                          let logger = loggerFactory.CreateLogger("EngineBattle Console logger") // Using a general category name
                          let ok = runTournament tournament logger
                          host.StopAsync().Wait()
                          // a run that stopped with an error is not a success to a script
                          if not ok then exit 1
                    | None -> 
                        printfn "Tournamentjson config file not found..."                        
                | Verb (Tune path) ->
                    Console.CancelKeyPress.Add(fun args ->
                        args.Cancel <- true
                        printfn "\nTuner cancelled by user. Exiting..."
                        Environment.Exit(0)
                    )
                    BayesianOptimizer.runTuneWithDispatch path
                | Verb (Validate configFile) ->
                    let normalizedPath = normalizePath configFile
                    let tournamentConfig = JSON.readTournamentJson normalizedPath
                    match tournamentConfig with
                    | Some tourny ->
                        let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
                        let engineSetup = {tourny.EngineSetup with Engines = engineList}
                        let t = {tourny with EngineSetup = engineSetup}
                        Validation.validateTournamentInput t
                        printfn "Validation complete."
                    | None ->
                        printfn "Config file not found: %s" normalizedPath
                | Verb (PuzzleTrend (folder, armFilter, typeFilter, rgFilter, csvOut, minSteps)) ->
                    // Consolidates many puzzle runs into step curves per training arm, which is
                    // otherwise assembled by hand from one summary table per run.
                    let folder =
                        match folder with
                        | Some f -> f
                        | None ->
                            match CliSettings.current.Force() with
                            | Result.Ok { PuzzleResultsFolder = Some f } -> f
                            | Result.Ok _ ->
                                eprintfn "Missing folder for puzzletrend: pass <folder> or put { \"puzzleResultsFolder\": \"<folder>\" } in %s" (CliSettings.path ())
                                exit 1
                            | Result.Error e ->
                                eprintfn "puzzletrend: cannot read %s" e
                                exit 1
                    let normalizedFolder = normalizePath folder
                    if not (Directory.Exists normalizedFolder) then
                        printfn "Folder not found: %s" normalizedFolder
                    else
                        let all = ChessLibrary.PuzzleTrend.loadFolder normalizedFolder
                        if List.isEmpty all then
                            printfn "No puzzle result JSON found in %s" normalizedFolder
                            printfn "(runs write LichessSummary_<stamp>.json beside the text summary)"
                        else
                            let matches (needle: string option) (haystack: string) =
                                match needle with
                                | Some n -> haystack.Contains(n, StringComparison.OrdinalIgnoreCase)
                                | None -> true
                            let filtered =
                                all
                                |> List.filter (fun p ->
                                    matches armFilter p.Arm
                                    && matches typeFilter p.Type
                                    && (match rgFilter with Some rg -> p.RatingGroup = rg | None -> true))
                                |> ChessLibrary.PuzzleTrend.dedupe
                            let filtered, droppedSeries = ChessLibrary.PuzzleTrend.filterMinSteps minSteps filtered
                            if droppedSeries > 0 then
                                printfn "Hiding %d series with fewer than %d steps (--min-steps 1 to show all)." droppedSeries minSteps
                            if List.isEmpty filtered then
                                printfn "No rows matched the given filters (%d checkpoint rows scanned)." all.Length
                            else
                                let arms = filtered |> List.map (fun p -> p.Arm) |> List.distinct
                                printfn "%d checkpoint rows across %d arm(s) from %s"
                                    filtered.Length arms.Length normalizedFolder
                                printfn "%s" (ChessLibrary.PuzzleTrend.render filtered)
                                match csvOut with
                                | Some out ->
                                    let outPath = normalizePath out
                                    File.WriteAllText(outPath, ChessLibrary.PuzzleTrend.toCsv filtered)
                                    printfn "CSV written: %s" outPath
                                | None -> ()
                | Verb (BookEval p) -> runBookEval p
                | Verb (Redash path) ->
                    BayesianOptimizer.regenerateDashboard path
                | Verb (GUI (_, _)) when (BlazorInterop.tryWebGuiProject ()).IsNone ->
                    // A published console has no WebGUI project to `dotnet run`; say so rather
                    // than announce a server that never starts.
                    printfn "gui runs the WebGUI of a source checkout, and there is none here."
                    printfn "The WebGUI is EngineBattle, next to eb-cli in the EngineBattle download."
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
                    printConsoleHelp ()
                | _ ->
                    printfn "Unhandled argument: %A" arg
            Cli.Output.ExitOk
        let tokens = List.ofArray argv
        let code = Cli.App.run (Registry.all runLegacy) tokens
        // double-clicked: keep the window open long enough to read the command list
        if tokens.IsEmpty then waitIfOwnConsole ()
        code

