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
  let timeConfig = UnionType.FixedTime (TimeSpan(0,0,5)) //10 seconds
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
    | EretError msg ->
        RuntimeUtilities.ConsoleUtils.redConsole $"\nERET Error: {msg}"

 
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
      Nodes: int64; Nps: int64; Time: TimeSpan
      TBHits: int64; WDL: ChessLibrary.EngineTypes.WDL option
      Bestmove: string; PV: string; SanPV: string
  }

  /// Resolves an engine path (JSON config or bare exe) to an EngineConfig, applying UCI option overrides.
  let private resolveEngineConfig (enginePath: string) (uciOptions: (string * string) list) =
      let normalizedEngine = normalizePath enginePath
      if not (File.Exists normalizedEngine) then
          failwithf "Engine file not found: %s" normalizedEngine
      let config =
          if normalizedEngine.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
              Configuration.JSON.readSingleEngineConfig normalizedEngine
          else
              TypesDef.CoreTypes.EngineConfig.EmptyWithPath normalizedEngine
      let options = System.Collections.Generic.Dictionary<string, obj>(config.Options)
      for (key, value) in uciOptions do
          options.[key] <- box value
      { config with Options = options }

  /// Runs engine analysis on a single position using an already-created engine.
  /// Sends ucinewgame + isready before the search. Caller is responsible for engine lifecycle.
  let private analyzePosition (engine: Engine.ChessEngine) (fen: string) (moves: string list) (searchDepth: int option) (searchMovetime: int option) (searchNodes: int option) (verbose: bool) =
      let isStartpos = fen.Equals("startpos", StringComparison.OrdinalIgnoreCase)
      let actualFen =
          if isStartpos then "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          else fen
      let board = ChessLibrary.Chess.Board()
      board.LoadFen actualFen
      for m in moves do
          board.PlayUciMove m
      let isWhite = board.Position.STM = 0uy

      engine.UciNewGame()
      let ok = engine.WaitForReadyOk(900000)
      if not ok then failwith "Engine did not respond to isready"

      let movesStr = if moves.Length > 0 then " moves " + String.concat " " moves else ""
      if isStartpos then
          engine.Position("position startpos" + movesStr)
      else
          engine.Position(sprintf "position fen %s%s" actualFen movesStr)

      match searchDepth, searchMovetime, searchNodes with
      | Some d, _, _ -> engine.Write(sprintf "go depth %d" d)
      | _, Some ms, _ -> engine.Go ms
      | _, _, Some n -> engine.GoNodes n
      | _ -> engine.GoNodes 1_000_000

      let sw = Diagnostics.Stopwatch.StartNew()
      let mutable lastDepth = 0
      let mutable lastEval = ChessLibrary.MiscTypes.EvalType.NA
      let mutable lastNodes = 0L
      let mutable lastNps = 0L
      let mutable lastPV = ""
      let mutable lastTBHits = 0L
      let mutable lastWDL: ChessLibrary.EngineTypes.WDL option = None
      let mutable lastSDepth = 0
      let mutable bestmove = ""
      let mutable running = true

      while running do
          let line = engine.ReadLine()
          if isNull line then
              running <- false
          else
              let trimmed = line.TrimStart()
              if trimmed.StartsWith("info", StringComparison.OrdinalIgnoreCase) then
                  if verbose then
                      if trimmed.StartsWith("info string", StringComparison.OrdinalIgnoreCase) then
                          printfn "%s" trimmed
                      elif trimmed.Contains("depth") && not (trimmed.Contains("currmove")) then
                          printfn "%s" trimmed
                  match ChessLibrary.EngineProtocol.Regex.getEssentialDataWithEPS trimmed isWhite with
                  | Some (depth, eval, nodes, nps, _eps, pv, tbhits, wdl, sDepth, _mpv) ->
                      lastDepth <- depth
                      lastEval <- eval
                      lastNodes <- nodes
                      lastNps <- nps
                      lastPV <- pv
                      lastTBHits <- tbhits
                      lastWDL <- wdl
                      lastSDepth <- sDepth
                  | None -> ()
              elif trimmed.StartsWith("bestmove", StringComparison.OrdinalIgnoreCase) then
                  if verbose then printfn "%s" trimmed
                  let parts = trimmed.Split(' ')
                  if parts.Length >= 2 then bestmove <- parts.[1]
                  running <- false

      sw.Stop()
      let sanPV =
          if not (String.IsNullOrWhiteSpace lastPV) then
              let moveList = Array.init 256 (fun _ -> Unchecked.defaultof<MoveTypes.TMove>)
              ChessLibrary.BoardUtils.getShortSanPVFromLongSanPVFast moveList &board lastPV
          else ""
      { Depth = lastDepth; SDepth = lastSDepth; Eval = lastEval
        Nodes = lastNodes; Nps = lastNps; Time = sw.Elapsed
        TBHits = lastTBHits; WDL = lastWDL; Bestmove = bestmove
        PV = lastPV; SanPV = sanPV }

  let runAnalyze (p: CliParser.AnalyzeParams) =
    let normalizedEngine = normalizePath p.Engine
    if not (File.Exists normalizedEngine) then
        printfn "Engine file not found: %s" normalizedEngine
    else
    try
        // Resolve engine config: JSON file or bare exe
        let config =
            if normalizedEngine.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
                Configuration.JSON.readSingleEngineConfig normalizedEngine
            else
                TypesDef.CoreTypes.EngineConfig.EmptyWithPath normalizedEngine

        // Apply UCI option overrides
        let options = System.Collections.Generic.Dictionary<string, obj>(config.Options)
        for (key, value) in p.UciOptions do
            options.[key] <- box value
        let config = { config with Options = options }
        let config =
            match p.Args with
            | Some a -> { config with Args = a }
            | None -> config

        // Validate FEN
        let isStartpos = p.Fen.Equals("startpos", StringComparison.OrdinalIgnoreCase)
        let fen =
            if isStartpos then
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            else p.Fen
        let board = ChessLibrary.Chess.Board()
        board.LoadFen fen

        // Play moves on the board so STM/ply are correct for SAN PV
        for m in p.Moves do
            board.PlayUciMove m

        let isWhite = board.Position.STM = 0uy

        printfn ""
        printfn "Engine: %s" config.Name
        printfn "FEN:    %s" fen
        if p.Moves.Length > 0 then
            printfn "Moves:  %s" (String.concat " " p.Moves)

        // Create synchronous engine
        let engine = ChessLibrary.EngineHelper.createEngine(config, None)
        try
            if p.ShowOptions then
                printfn "\nUCI options for %s:\n" config.Name
                let defaults = engine.GetDefaultOptions()
                for opt in defaults do
                    printfn "  %-30s  %s" opt.Key (opt.Value.ToString())
            else

            engine.UciNewGame()
            let ok = engine.WaitForReadyOk(900000)
            if not ok then failwith "Engine did not respond to isready"

            // Send position
            let movesStr = if p.Moves.Length > 0 then " moves " + String.concat " " p.Moves else ""
            if isStartpos then
                engine.Position("position startpos" + movesStr)
            else
                engine.Position(sprintf "position fen %s%s" fen movesStr)

            // Send go command
            let searchDesc =
                match p.Depth, p.MoveTime, p.Nodes with
                | Some d, _, _ ->
                    engine.Write(sprintf "go depth %d" d)
                    sprintf "depth %d" d
                | _, Some ms, _ ->
                    engine.Go ms
                    sprintf "movetime %dms" ms
                | _, _, Some n ->
                    engine.GoNodes n
                    sprintf "nodes %s" (n.ToString("N0"))
                | _ ->
                    engine.GoNodes 1_000_000
                    "nodes 1,000,000"
            printfn "Search: %s" searchDesc
            printfn ""

            let sw = Diagnostics.Stopwatch.StartNew()
            // Read loop — print raw info lines and track last parsed stats
            let mutable lastDepth = 0
            let mutable lastEval = ChessLibrary.MiscTypes.EvalType.NA
            let mutable lastNodes = 0L
            let mutable lastNps = 0L
            let mutable lastEps = 0L
            let mutable lastPV = ""
            let mutable lastTBHits = 0L
            let mutable lastWDL: ChessLibrary.EngineTypes.WDL option = None
            let mutable lastSDepth = 0
            let mutable bestmove = ""
            let mutable running = true

            while running do
                let line = engine.ReadLine()
                if isNull line then
                    running <- false
                else
                    let trimmed = line.TrimStart()
                    if trimmed.StartsWith("info", StringComparison.OrdinalIgnoreCase) then
                        // Print info lines with depth (skip currmove etc.) and info string (LogLiveStats)
                        if trimmed.StartsWith("info string", StringComparison.OrdinalIgnoreCase) then
                            printfn "%s" trimmed
                        elif trimmed.Contains("depth") && not (trimmed.Contains("currmove")) then
                            printfn "%s" trimmed
                        match ChessLibrary.EngineProtocol.Regex.getEssentialDataWithEPS trimmed isWhite with
                        | Some (depth, eval, nodes, nps, eps, pv, tbhits, wdl, sDepth, _mpv) ->
                            lastDepth <- depth
                            lastEval <- eval
                            lastNodes <- nodes
                            lastNps <- nps
                            lastEps <- eps
                            lastPV <- pv
                            lastTBHits <- tbhits
                            lastWDL <- wdl
                            lastSDepth <- sDepth
                        | None -> ()
                    elif trimmed.StartsWith("bestmove", StringComparison.OrdinalIgnoreCase) then
                        printfn "%s" trimmed
                        let parts = trimmed.Split(' ')
                        if parts.Length >= 2 then bestmove <- parts.[1]
                        running <- false

            // Print summary
            sw.Stop()
            printfn ""
            printfn "--- Summary ---"
            if lastDepth > 0 then
                let formattedNps = GameAnalysis.Formatting.formatNPS (float lastNps)
                let formattedEps = if lastEps > 0L then sprintf " (%s)" (GameAnalysis.Formatting.formatEPS (float lastEps)) else ""
                let wdlStr =
                    match lastWDL with
                    | Some wdl -> sprintf "WDL: %d-%d-%d" (int wdl.Win) (int wdl.Draw) (int wdl.Loss)
                    | None -> "WDL: N/A"
                let elapsed = sw.Elapsed
                let timeStr =
                    if elapsed.TotalSeconds < 1.0 then sprintf "%dms" elapsed.Milliseconds
                    elif elapsed.TotalMinutes < 1.0 then sprintf "%.1fs" elapsed.TotalSeconds
                    else sprintf "%dm %02ds" (int elapsed.TotalMinutes) elapsed.Seconds
                printfn "Depth:    %d (SD: %d)" lastDepth lastSDepth
                printfn "Eval:     %s" (lastEval.ToString())
                printfn "Nodes:    %s" (lastNodes.ToString("N0"))
                printfn "NPS:      %s%s" formattedNps formattedEps
                printfn "Time:     %s" timeStr
                printfn "TBHits:   %d" lastTBHits
                printfn "%s" wdlStr
                printfn "Bestmove: %s" bestmove
                if not (String.IsNullOrWhiteSpace lastPV) then
                    let pv = if lastPV.Length > 80 then lastPV.Substring(0, 77) + "..." else lastPV
                    printfn "PV:       %s" pv
                    let moveList = Array.init 256 (fun _ -> Unchecked.defaultof<MoveTypes.TMove>)
                    let sanPV = ChessLibrary.BoardUtils.getShortSanPVFromLongSanPVFast moveList &board lastPV
                    printfn "PV (SAN): %s" sanPV
            else
                printfn "No search info received from engine."
        finally
            engine.StopProcess()
    with ex ->
        printfn "Error during analysis: %s" ex.Message

  /// Position-query verb: machine-readable JSON for validator/tooling use (python-chess
  /// differential testing etc.). Data goes to stdout only; errors to stderr, exit 1.
  let runQuery (fen: string) (square: string option) (epdPath: string option) (pv: string option) =
    // Short FENs validate and normalize; the raw parser needs the default-filled form.
    let fen = ChessLibrary.BoardUtils.normalizeFen fen
    let jsonOpts indented =
        System.Text.Json.JsonSerializerOptions(
            PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.CamelCase,
            WriteIndented = indented)
    let sanPvOf (fen: string) (uciMoves: string) =
        let board = ChessLibrary.Chess.Board()
        board.LoadFen fen
        let buffer = Array.init 256 (fun _ -> Unchecked.defaultof<MoveTypes.TMove>)
        ChessLibrary.BoardUtils.getShortSanPVFromLongSanPVFast buffer &board uciMoves
    try
        match epdPath with
        | Some path ->
            // Batch mode: NDJSON per EPD position — insights + status + full legal move
            // list (the movegen/SAN differential-testing payload). Invalid FENs are
            // skipped with a stderr warning so one bad line can't kill the stream.
            let opts = jsonOpts false
            for e in ChessLibrary.EPDExtractor.readEPDs path do
                let validation = ChessLibrary.BoardUtils.validateFen e.FEN
                if not validation.IsValid then
                    eprintfn "skipping invalid FEN '%s': %s" e.FEN (String.concat "; " validation.Errors)
                else
                    let status = ChessLibrary.BoardUtils.getPositionStatus e.FEN
                    let record =
                        {| Fen = e.FEN
                           Status = status.Status
                           InsufficientMaterial = status.InsufficientMaterial
                           LegalMoves = ChessLibrary.BoardUtils.legalMovesOf e.FEN
                           Insights = ChessLibrary.BoardUtils.getPositionInsights e.FEN |}
                    printfn "%s" (System.Text.Json.JsonSerializer.Serialize(record, opts))
        | None ->
            let validation = ChessLibrary.BoardUtils.validateFen fen
            if not validation.IsValid then
                eprintfn "invalid FEN '%s': %s" fen (String.concat "; " validation.Errors)
                exit 1
            let insights = ChessLibrary.BoardUtils.getPositionInsights fen
            let status = ChessLibrary.BoardUtils.getPositionStatus fen
            let legalMoves = ChessLibrary.BoardUtils.legalMovesOf fen
            let sanPv = pv |> Option.map (sanPvOf fen)
            let parts = fen.Split(' ')
            let stm = if parts.Length > 1 then parts.[1] else "w"
            match square with
            | Some sq ->
                let atts = ChessLibrary.BoardUtils.attackersOf fen sq
                let record =
                    {| Fen = fen
                       SideToMove = stm
                       Status = status.Status
                       InsufficientMaterial = status.InsufficientMaterial
                       LegalMoves = legalMoves
                       Insights = insights
                       Pv = Option.toObj pv
                       SanPv = Option.toObj sanPv
                       Square = sq
                       AttackersWhite = atts.White
                       AttackersBlack = atts.Black
                       Attacks = ChessLibrary.BoardUtils.attacksFrom fen sq
                       IsPinned = ChessLibrary.BoardUtils.isPinned fen sq
                       PinRay = ChessLibrary.BoardUtils.pinRay fen sq
                       SafeDestinations = ChessLibrary.BoardUtils.getSafeDestinations fen sq |}
                printfn "%s" (System.Text.Json.JsonSerializer.Serialize(record, jsonOpts true))
            | None ->
                let record =
                    {| Fen = fen
                       SideToMove = stm
                       Status = status.Status
                       InsufficientMaterial = status.InsufficientMaterial
                       LegalMoves = legalMoves
                       Insights = insights
                       Pv = Option.toObj pv
                       SanPv = Option.toObj sanPv |}
                printfn "%s" (System.Text.Json.JsonSerializer.Serialize(record, jsonOpts true))
    with ex ->
        eprintfn "query error: %s" ex.Message
        exit 1

  let runCompare (p: CliParser.CompareParams) =
    try
        let config1 = resolveEngineConfig p.Engine1 p.UciOptions1
        let config2 = resolveEngineConfig p.Engine2 p.UciOptions2

        // Build position list: either from EPD file or single FEN
        let positions =
            match p.PositionsFile with
            | Some path ->
                let normalizedPath = normalizePath path
                if not (File.Exists normalizedPath) then
                    failwithf "Positions file not found: %s" normalizedPath
                EPDExtractor.readEPDs normalizedPath
                |> Seq.map (fun epd -> epd.FEN, epd.Id |> Option.defaultValue "")
                |> Seq.toArray
            | None ->
                let fen =
                    if p.Fen.Equals("startpos", StringComparison.OrdinalIgnoreCase) then
                        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                    else p.Fen
                [| fen, "" |]

        let searchDesc =
            match p.Depth, p.MoveTime, p.Nodes with
            | Some d, _, _ -> sprintf "depth %d" d
            | _, Some ms, _ -> sprintf "movetime %dms" ms
            | _, _, Some n -> sprintf "nodes %s" (n.ToString("N0"))
            | _ -> "nodes 1,000,000"

        printfn ""
        printfn "Engine 1: %s" config1.Name
        printfn "Engine 2: %s" config2.Name
        printfn "Positions: %d" positions.Length
        printfn "Search:   %s" searchDesc
        match p.Threshold with
        | Some t -> printfn "Threshold: %.2f cp" t
        | None -> ()
        printfn ""

        // Create engines once, reuse across all positions. Engine1's process must be
        // stopped even when engine2's creation throws (bad path, failed start).
        let engine1 = ChessLibrary.EngineHelper.createEngine(config1, None)
        let engine2 =
            try ChessLibrary.EngineHelper.createEngine(config2, None)
            with ex ->
                try engine1.StopProcess() with _ -> ()
                raise ex
        try

        let mutable totalAgreements = 0
        let mutable totalPositions = 0
        let mutable totalEvalDiff = 0.0
        let mutable maxEvalDiff = 0.0
        let mutable maxEvalDiffIdx = 0
        let mutable npsSum1 = 0.0
        let mutable npsSum2 = 0.0

        // Engine legend
        printfn "E1 = %s" config1.Name
        printfn "E2 = %s" config2.Name
        printfn ""

        // Header
        let fenW = positions |> Array.map (fun (fen, _) -> fen.Length) |> Array.max |> max 3
        printfn "%-4s  %-*s  %8s  %8s  %8s  %10s  %10s  %6s  %s"
            "#" fenW "FEN" "Eval E1" "Eval E2" "Diff" "NPS E1" "NPS E2" "Ratio" "Move"
        printfn "%s" (String.replicate (4 + 2 + fenW + 2 + 8 + 2 + 8 + 2 + 8 + 2 + 10 + 2 + 10 + 2 + 6 + 2 + 10) "-")

        for i in 0 .. positions.Length - 1 do
            let fen, _id = positions.[i]
            try
                let r1 = analyzePosition engine1 fen [] p.Depth p.MoveTime p.Nodes false
                let r2 = analyzePosition engine2 fen [] p.Depth p.MoveTime p.Nodes false

                let eval1Cp =
                    match r1.Eval with
                    | ChessLibrary.MiscTypes.EvalType.CP cp -> Some cp
                    | ChessLibrary.MiscTypes.EvalType.Mate m -> Some (float (sign m) * 999.99)
                    | _ -> None
                let eval2Cp =
                    match r2.Eval with
                    | ChessLibrary.MiscTypes.EvalType.CP cp -> Some cp
                    | ChessLibrary.MiscTypes.EvalType.Mate m -> Some (float (sign m) * 999.99)
                    | _ -> None

                let diff =
                    match eval1Cp, eval2Cp with
                    | Some e1, Some e2 -> Some (abs (e1 - e2))
                    | _ -> None

                let moveAgree = r1.Bestmove = r2.Bestmove
                totalPositions <- totalPositions + 1
                if moveAgree then totalAgreements <- totalAgreements + 1

                match diff with
                | Some d ->
                    totalEvalDiff <- totalEvalDiff + d
                    if d > maxEvalDiff then
                        maxEvalDiff <- d
                        maxEvalDiffIdx <- i + 1
                | None -> ()

                if r1.Nps > 0L then npsSum1 <- npsSum1 + float r1.Nps
                if r2.Nps > 0L then npsSum2 <- npsSum2 + float r2.Nps

                let shouldPrint =
                    match p.Threshold, diff with
                    | Some t, Some d -> d >= t
                    | _ -> true

                if shouldPrint then
                    let diffStr =
                        match diff with
                        | Some d -> sprintf "%+.2f" d
                        | None -> "N/A"
                    let moveStr =
                        if moveAgree then r1.Bestmove
                        else sprintf "%s/%s" r1.Bestmove r2.Bestmove
                    let npsRatio =
                        if r1.Nps > 0L && r2.Nps > 0L then sprintf "%.1fx" (float r1.Nps / float r2.Nps)
                        else "N/A"
                    printfn "%-4d  %-*s  %8s  %8s  %8s  %10s  %10s  %6s  %s"
                        (i + 1) fenW fen
                        (r1.Eval.ToString())
                        (r2.Eval.ToString())
                        diffStr
                        (GameAnalysis.Formatting.formatNPS (float r1.Nps))
                        (GameAnalysis.Formatting.formatNPS (float r2.Nps))
                        npsRatio
                        moveStr
            with ex ->
                printfn "%-4d  %-*s  ERROR: %s" (i + 1) fenW fen ex.Message

        // Summary
        if totalPositions > 0 then
            printfn ""
            printfn "--- Summary (%d positions) ---" totalPositions
            let avgDiff = if totalPositions > 0 then totalEvalDiff / float totalPositions else 0.0
            let agreePct = 100.0 * float totalAgreements / float totalPositions
            printfn "Move agreement: %d/%d (%.1f%%)" totalAgreements totalPositions agreePct
            printfn "Avg eval diff:  %.2f cp" avgDiff
            printfn "Max eval diff:  %.2f cp (position #%d)" maxEvalDiff maxEvalDiffIdx
            if npsSum1 > 0.0 && npsSum2 > 0.0 then
                let avgNps1 = npsSum1 / float totalPositions
                let avgNps2 = npsSum2 / float totalPositions
                let ratio = avgNps1 / avgNps2
                printfn "Avg NPS:        %s vs %s (ratio: %.2fx)"
                    (GameAnalysis.Formatting.formatNPS avgNps1)
                    (GameAnalysis.Formatting.formatNPS avgNps2)
                    ratio

        finally
            engine1.StopProcess()
            engine2.StopProcess()
    with ex ->
        printfn "Error during comparison: %s" ex.Message

  // ---------- Contextual piece valuation (leave-one-out net eval) ----------
  // "What is a piece worth to the net in THIS position" = how much the eval drops
  // when the piece is removed, framed from the piece owner's perspective.
  // All board manipulation is done on the FEN string (representation-agnostic);
  // square index convention: 0 = a1, 7 = h1, 56 = a8, 63 = h8.

  type private PieceContribution =
      { Sq: int
        Piece: char
        IsWhite: bool
        Illegal: bool
        DeltaLogit: float option    // owner perspective, logit(score) — the de-saturated metric
        DeltaCp: float option       // owner perspective, centipawns (engine; saturates near rails)
        DeltaScore: float option }  // owner perspective, White-expected-score % points (saturates)

  let private pvSquareName (sq: int) =
      sprintf "%c%d" (char (int 'a' + sq % 8)) (sq / 8 + 1)

  let private pvFields (fen: string) =
      fen.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

  /// Parse the FEN board field into 64 chars ('.' = empty), indexed a1=0 .. h8=63.
  let private pvParseFenBoard (fen: string) : char[] =
      let boardField = (pvFields fen).[0]
      let squares = Array.create 64 '.'
      let mutable rank = 7
      let mutable file = 0
      for c in boardField do
          if c = '/' then rank <- rank - 1; file <- 0
          elif Char.IsDigit c then file <- file + (int c - int '0')
          else
              if rank >= 0 && rank < 8 && file < 8 then squares.[rank * 8 + file] <- c
              file <- file + 1
      squares

  /// Serialize a 64-char board array back into a FEN board field.
  let private pvBoardToField (squares: char[]) : string =
      let sb = System.Text.StringBuilder()
      for rank in 7 .. -1 .. 0 do
          let mutable empty = 0
          for file in 0 .. 7 do
              let c = squares.[rank * 8 + file]
              if c = '.' then empty <- empty + 1
              else
                  if empty > 0 then sb.Append(string empty) |> ignore; empty <- 0
                  sb.Append(c) |> ignore
          if empty > 0 then sb.Append(string empty) |> ignore
          if rank > 0 then sb.Append('/') |> ignore
      sb.ToString()

  /// Build a FEN with the piece on `sq` removed. Clears en passant and strips
  /// standard castling rights tied to a removed home-square rook. Keeps side to move.
  let private pvFenWithoutPiece (fen: string) (sq: int) : string =
      let fields = pvFields fen
      let squares = pvParseFenBoard fen
      let removed = squares.[sq]
      squares.[sq] <- '.'
      let boardField = pvBoardToField squares
      let stm = if fields.Length > 1 then fields.[1] else "w"
      let castling =
          let c = if fields.Length > 2 then fields.[2] else "-"
          if c = "-" then "-"
          else
              let dropped =
                  match sq, removed with
                  | 0, 'R' -> "Q" | 7, 'R' -> "K"
                  | 56, 'r' -> "q" | 63, 'r' -> "k"
                  | _ -> ""
              let filtered = String(c.ToCharArray() |> Array.filter (fun ch -> dropped.IndexOf(ch) < 0))
              if filtered = "" then "-" else filtered
      let half = if fields.Length > 4 then fields.[4] else "0"
      let full = if fields.Length > 5 then fields.[5] else "1"
      sprintf "%s %s %s - %s %s" boardField stm castling half full

  /// True if the side NOT to move is in check (i.e. the position is illegal /
  /// off-distribution for net eval — usually because a removed piece exposed a check).
  let private pvOpponentInCheck (fen: string) : bool =
      let fields = pvFields fen
      let stm = if fields.Length > 1 then fields.[1] else "w"
      let swapped = if stm = "w" then "b" else "w"
      let castling = if fields.Length > 2 then fields.[2] else "-"
      let half = if fields.Length > 4 then fields.[4] else "0"
      let full = if fields.Length > 5 then fields.[5] else "1"
      let swappedFen = sprintf "%s %s %s - %s %s" fields.[0] swapped castling half full
      let mutable pos = RuntimeUtilities.BoardHelper.getPosFromFen (Some swappedFen)
      MoveGeneration.InCheck &pos <> 0UL

  /// analyzePosition already returns the eval in White's perspective; convert to cp.
  let private pvEvalToWhiteCp (e: ChessLibrary.MiscTypes.EvalType) : float option =
      match e with
      | ChessLibrary.MiscTypes.EvalType.CP p -> Some (p * 100.0)
      | ChessLibrary.MiscTypes.EvalType.Mate m -> Some (float (sign m) * 30000.0)
      | _ -> None

  /// WDL is reported from the side-to-move's perspective; convert to White expected score %.
  let private pvWdlToWhiteScore (stmWhite: bool) (wdl: ChessLibrary.EngineTypes.WDL option) : float option =
      match wdl with
      | Some w ->
          let total = w.Win + w.Draw + w.Loss
          if total <= 0.0 then None
          else
              let win, draw = if stmWhite then w.Win, w.Draw else w.Loss, w.Draw
              Some ((win + 0.5 * draw) / total * 100.0)
      | None -> None

  /// White expected-score in logit (log-odds) space. Win% saturates at the rails;
  /// the logit stays finite and monotone (the value head emits soft WDL), which
  /// restores resolution between, e.g., removing a queen vs a rook.
  let private pvWdlToWhiteLogit (stmWhite: bool) (wdl: ChessLibrary.EngineTypes.WDL option) : float option =
      match pvWdlToWhiteScore stmWhite wdl with
      | Some pct ->
          let eps = 1e-4
          let s = max eps (min (1.0 - eps) (pct / 100.0))
          Some (log (s / (1.0 - s)))
      | None -> None

  let runPieceValues (p: CliParser.PieceValuesParams) =
    try
        let config = resolveEngineConfig p.Engine p.UciOptions

        // Resolve the effective FEN (apply any moves so values reflect the post-move position)
        let baseFen =
            if p.Fen.Equals("startpos", StringComparison.OrdinalIgnoreCase) then
                ChessLibrary.MiscTypes.startPosition
            else p.Fen
        let board = ChessLibrary.Chess.Board()
        board.LoadFen baseFen
        for m in p.Moves do board.PlayUciMove m
        let fen = RuntimeUtilities.BoardHelper.posToFen board.Position

        // Default to a pure net eval (nodes=1) unless the user overrides the search.
        let depth, movetime, nodes =
            match p.Depth, p.MoveTime, p.Nodes with
            | None, None, None -> None, None, Some 1
            | d, m, n -> d, m, n
        let searchDesc =
            match depth, movetime, nodes with
            | Some d, _, _ -> sprintf "depth %d" d
            | _, Some ms, _ -> sprintf "movetime %dms" ms
            | _, _, Some n -> sprintf "nodes %d" n
            | _ -> "nodes 1"

        let fields = pvFields fen
        let stmWhite = fields.Length < 2 || fields.[1] = "w"

        printfn ""
        printfn "Engine:   %s" config.Name
        printfn "FEN:      %s" fen
        printfn "Side:     %s to move" (if stmWhite then "White" else "Black")
        printfn "Search:   %s" searchDesc
        printfn ""
        printfn "Contextual piece value = how much the eval drops when each piece is removed,"
        printfn "framed from the piece owner's perspective. Higher = more valuable in THIS position."
        printfn ""

        let engine = ChessLibrary.EngineHelper.createEngine(config, None)
        try
            // Baseline eval
            let baseR = analyzePosition engine fen [] depth movetime nodes false
            let baseCp = pvEvalToWhiteCp baseR.Eval
            let baseScore = pvWdlToWhiteScore stmWhite baseR.WDL
            let baseLogit = pvWdlToWhiteLogit stmWhite baseR.WDL
            printfn "Baseline eval (White's view): %s%s"
                (match baseCp with Some c -> sprintf "%+.0f cp" c | None -> "n/a")
                (match baseScore with Some s -> sprintf "  |  White score %.1f%%" s | None -> "")

            // Decided-position guard: per-piece values are ill-posed once the game
            // is effectively over (any change saturates), so warn the user.
            match baseLogit with
            | Some l when abs l > 2.0 ->
                printfn "⚠️  Position is already decided (logit %+.1f ≈ score %.0f%%) — individual"
                    l (match baseScore with Some s -> s | None -> 0.0)
                printfn "    piece values are ill-posed here; use a position with |eval| < ~2 pawns."
            | _ -> ()

            let squares = pvParseFenBoard fen
            let pieces =
                [ for sq in 0 .. 63 do
                    let c = squares.[sq]
                    if c <> '.' && c <> 'K' && c <> 'k' then yield sq, c ]

            printf "Evaluating %d pieces " pieces.Length
            let results =
                [ for (sq, c) in pieces do
                    let isWhite = Char.IsUpper c
                    let modFen = pvFenWithoutPiece fen sq
                    if pvOpponentInCheck modFen then
                        printf "."
                        yield { Sq = sq; Piece = c; IsWhite = isWhite; Illegal = true
                                DeltaLogit = None; DeltaCp = None; DeltaScore = None }
                    else
                        let r = analyzePosition engine modFen [] depth movetime nodes false
                        let remCp = pvEvalToWhiteCp r.Eval
                        let remScore = pvWdlToWhiteScore stmWhite r.WDL
                        let remLogit = pvWdlToWhiteLogit stmWhite r.WDL
                        let sgn = if isWhite then 1.0 else -1.0
                        let delta baseV remV =
                            match baseV, remV with
                            | Some b, Some rm -> Some (sgn * (b - rm))
                            | _ -> None
                        printf "."
                        yield { Sq = sq; Piece = c; IsWhite = isWhite; Illegal = false
                                DeltaLogit = delta baseLogit remLogit
                                DeltaCp = delta baseCp remCp
                                DeltaScore = delta baseScore remScore } ]
            printfn " done.\n"

            let pieceWord (c: char) =
                let name =
                    match Char.ToUpper c with
                    | 'P' -> "pawn" | 'N' -> "knight" | 'B' -> "bishop"
                    | 'R' -> "rook" | 'Q' -> "queen" | _ -> "?"
                sprintf "%s %s" (if Char.IsUpper c then "White" else "Black") name

            // ---- Ranked table (Δlogit is the de-saturated headline metric) ----
            printfn "%-6s  %-14s  %9s  %10s  %10s  %s" "Square" "Piece" "Δlogit" "Δcp" "Δscore%" "Note"
            printfn "%s" (String.replicate 80 "-")
            results
            |> List.sortByDescending (fun r -> match r.DeltaLogit with Some v -> v | None -> System.Double.NegativeInfinity)
            |> List.iter (fun r ->
                let lgStr = match r.DeltaLogit with Some v -> sprintf "%+.2f" v | None -> "—"
                let cpStr = match r.DeltaCp with Some v -> sprintf "%+.0f" v | None -> "—"
                let scStr = match r.DeltaScore with Some v -> sprintf "%+.1f" v | None -> "—"
                let note = if r.Illegal then "off-distribution (removal exposes check)" else ""
                printfn "%-6s  %-14s  %9s  %10s  %10s  %s"
                    (pvSquareName r.Sq) (pieceWord r.Piece) lgStr cpStr scStr note)
            printfn ""

            // ---- Heatmap board (value in pawns, owner's perspective) ----
            let bySq = results |> List.map (fun r -> r.Sq, r) |> Map.ofList
            let cell (sq: int) =
                let c = squares.[sq]
                if c = '.' then "      "
                elif c = 'K' || c = 'k' then sprintf "%c  .  " c
                else
                    match Map.tryFind sq bySq with
                    | Some r when r.Illegal -> sprintf "%c  x  " c
                    | Some { DeltaLogit = Some v } ->
                        let lg = max -9.9 (min 9.9 v)
                        sprintf "%c%+5.1f" c lg
                    | _ -> sprintf "%c  ?  " c
            printfn "Board (piece + contextual value as Δlogit, owner's perspective):"
            printfn ""
            for rank in 7 .. -1 .. 0 do
                let cells = [ for file in 0 .. 7 -> cell (rank * 8 + file) ]
                printfn " %d | %s" (rank + 1) (String.concat " " cells)
            printfn "     %s" (String.concat " " [ for f in 0 .. 7 -> sprintf "%-6c" (char (int 'a' + f)) ])
            printfn ""
            printfn "  (K/k = king, 'x' = removal exposes check, '?' = no eval)"
            printfn ""

            // ---- Per-type summary (average Δlogit by piece type) ----
            printfn "Average contextual value by piece type (Δlogit), static order P<N≈B<R<Q:"
            printfn "%-8s  %10s  %5s" "Type" "Avg Δlogit" "Count"
            printfn "%s" (String.replicate 28 "-")
            for t in [ 'P'; 'N'; 'B'; 'R'; 'Q' ] do
                let vals =
                    results
                    |> List.filter (fun r -> not r.Illegal && Char.ToUpper r.Piece = t)
                    |> List.choose (fun r -> r.DeltaLogit)
                if not vals.IsEmpty then
                    let avg = List.average vals
                    let label = match t with 'P' -> "Pawn" | 'N' -> "Knight" | 'B' -> "Bishop" | 'R' -> "Rook" | 'Q' -> "Queen" | _ -> "?"
                    printfn "%-8s  %10.2f  %5d" label avg vals.Length
            printfn ""
        finally
            engine.StopProcess()
    with ex ->
        printfn "Error during piece valuation: %s" ex.Message

  // ---------- Multi-position piece-value regression (AlphaZero-style) ----------
  // Regress the net's eval (logit of White expected score) against material imbalance
  // across many positions. The coefficients are the net's implied piece values.
  // Uses L1 (IRLS) instead of L2 — the PNAS "Acquisition of Chess Knowledge" paper
  // found L2 systematically underestimates piece weights. Small ridge keeps the
  // 7x7 normal-equation solve stable when a material feature has little variance.

  /// White-minus-black piece counts: [ΔP; ΔN; ΔB; ΔR; ΔQ].
  let private pvMaterialDiff (squares: char[]) : float[] =
      let cnt c = squares |> Array.sumBy (fun x -> if x = c then 1.0 else 0.0)
      [| cnt 'P' - cnt 'p'; cnt 'N' - cnt 'n'; cnt 'B' - cnt 'b'; cnt 'R' - cnt 'r'; cnt 'Q' - cnt 'q' |]

  let private pvTotalPieces (squares: char[]) : int =
      squares |> Array.sumBy (fun x -> if x = '.' then 0 else 1)

  /// Weighted least squares via ridge-stabilised normal equations (feature count is tiny).
  let private pvWls (xRows: float[][]) (y: float[]) (wts: float[]) (ridge: float) : float[] =
      let p = xRows.[0].Length
      let n = xRows.Length
      let xtx = Array2D.zeroCreate p p
      let xty = Array.zeroCreate p
      for i in 0 .. n - 1 do
          let wi = wts.[i]
          let xi = xRows.[i]
          for a in 0 .. p - 1 do
              xty.[a] <- xty.[a] + wi * xi.[a] * y.[i]
              for b in 0 .. p - 1 do
                  xtx.[a, b] <- xtx.[a, b] + wi * xi.[a] * xi.[b]
      for a in 0 .. p - 1 do xtx.[a, a] <- xtx.[a, a] + ridge
      let m = DenseMatrix.init p p (fun a b -> xtx.[a, b])
      let v = DenseVector.init p (fun a -> xty.[a])
      (m.Solve(v)).ToArray()

  /// L1 regression by iteratively reweighted least squares (robust to outliers/saturation).
  let private pvFitL1 (xRows: float[][]) (y: float[]) : float[] =
      let n = xRows.Length
      let ridge = 1e-6
      let predict (beta: float[]) (x: float[]) = Array.fold2 (fun acc b xi -> acc + b * xi) 0.0 beta x
      let mutable beta = pvWls xRows y (Array.create n 1.0) ridge
      for _iter in 1 .. 25 do
          let wts = Array.init n (fun i -> 1.0 / max (abs (y.[i] - predict beta xRows.[i])) 1e-3)
          beta <- pvWls xRows y wts ridge
      beta

  /// DeepMind's model: fit g(d) = tanh(β·d) to game outcomes z ∈ {-1,0,+1} by
  /// Gauss-Newton least squares (Levenberg-Marquardt damped). Returns β.
  let private pvFitTanh (xRows: float[][]) (z: float[]) : float[] =
      let p = xRows.[0].Length
      let n = xRows.Length
      let lambda = 1e-3
      let beta = Array.zeroCreate p
      let dot (b: float[]) (x: float[]) = Array.fold2 (fun a bi xi -> a + bi * xi) 0.0 b x
      for _iter in 1 .. 100 do
          let jtj = Array2D.zeroCreate p p
          let rhs = Array.zeroCreate p     // = Σ s_i x_i r_i  (the −Jᵀr of GN)
          for i in 0 .. n - 1 do
              let xi = xRows.[i]
              let t = tanh (dot beta xi)
              let s = 1.0 - t * t          // tanh'(η) = sech²(η)
              let ri = z.[i] - t
              for a in 0 .. p - 1 do
                  rhs.[a] <- rhs.[a] + s * xi.[a] * ri
                  for b in 0 .. p - 1 do
                      jtj.[a, b] <- jtj.[a, b] + (s * xi.[a]) * (s * xi.[b])
          for a in 0 .. p - 1 do jtj.[a, a] <- jtj.[a, a] + lambda
          let m = DenseMatrix.init p p (fun a b -> jtj.[a, b])
          let v = DenseVector.init p (fun a -> rhs.[a])
          let step = (m.Solve(v)).ToArray()
          for a in 0 .. p - 1 do beta.[a] <- beta.[a] + step.[a]
      beta

  /// Game-clustered (block) bootstrap of the four normalised piece values [N;B;R;Q].
  /// Resamples whole GAMES with replacement — positions within a game are highly
  /// correlated (consecutive plies share nearly the same material), so resampling
  /// individual positions would treat them as independent and understate the intervals.
  /// `estimate` refits on a resampled row set and returns [|N;B;R;Q|] (pawn=1), or None
  /// if that draw is degenerate. Returns the (2.5%, 97.5%) percentile band per piece.
  /// Returns per-piece CIs [|N;B;R;Q|] plus the CI of the Bishop−Knight gap. The gap CI is
  /// computed per resample (B and N share the pawn-normalisation and the same games, so they
  /// co-move — the paired difference is far tighter than the overlap of the marginals implies).
  let private pvBootstrapCI
        (rows: (float[] * float * int * int)[])
        (estimate: (float[] * float * int * int)[] -> float[] option)
        (b: int) : ((float * float)[] * (float * float * float)) option =
      let games = rows |> Array.groupBy (fun (_, _, _, gid) -> gid) |> Array.map snd
      let ng = games.Length
      if ng < 2 then None
      else
          let rng = System.Random(12345)   // fixed seed → reproducible intervals
          let samples = ResizeArray<float[]>()
          for _ in 1 .. b do
              let draw = Array.init ng (fun _ -> games.[rng.Next ng]) |> Array.concat
              match estimate draw with
              | Some v -> samples.Add v
              | None -> ()
          if samples.Count < max 10 (b / 2) then None
          else
              let pct (q: float) (xs: float[]) =
                  let s = Array.sort xs
                  s.[max 0 (min (s.Length - 1) (int (q * float (s.Length - 1) + 0.5)))]
              let perPiece =
                  [| for i in 0 .. 3 ->
                       let col = samples |> Seq.map (fun v -> v.[i]) |> Array.ofSeq
                       (pct 0.025 col, pct 0.975 col) |]
              let gap = samples |> Seq.map (fun v -> v.[1] - v.[0]) |> Array.ofSeq   // Bishop − Knight
              Some (perPiece, (pct 0.025 gap, pct 0.5 gap, pct 0.975 gap))

  /// Outcome-regression rows: every position past the opening, labelled with its
  /// game's final result from White's perspective (+1 / 0 / -1). No engine needed.
  let private pvCollectOutcomeRows (path: string) (sample: int) : (float[] * float * int * int)[] =
      let rows = ResizeArray<float[] * float * int * int>()
      for gi, g in ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.indexed do
          let zWhite =
              match g.GameMetaData.Result with
              | "1-0" -> Some 1.0
              | "0-1" -> Some -1.0
              | "1/2-1/2" -> Some 0.0
              | _ -> None
          match zWhite with
          | None -> ()
          | Some z ->
              try
                  let board = ChessLibrary.Chess.Board()
                  let startFen =
                      if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                      else g.GameMetaData.Fen
                  board.LoadFen startFen
                  let mutable ply = 0
                  for pm in g.Mainline do
                      board.PlaySanMove pm.San
                      ply <- ply + 1
                      if ply >= 16 then
                          let f = board.FEN()
                          let squares = pvParseFenBoard f
                          let md = pvMaterialDiff squares
                          // DeepMind model: features are material imbalance only — NO intercept,
                          // NO tempo. Balanced positions (d=0) then drop out of the fit naturally.
                          rows.Add([| md.[0]; md.[1]; md.[2]; md.[3]; md.[4] |], z, pvTotalPieces squares, gi)
              with _ -> ()
      let arr = rows.ToArray()
      if arr.Length <= sample then arr
      else
          let stride = float arr.Length / float sample
          [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

  /// Read the engine's eval (wv=, in pawns) from a PGN move comment using EB's
  /// own annotation parser. Returns None when the comment has no wv (e.g. book moves).
  let private pvParseWv (comment: string) : float option =
      if String.IsNullOrWhiteSpace comment || not (comment.Contains "wv=") then None
      else
          let stat = ChessLibrary.EngineTypes.Annotation.getEngineStatData "" false comment
          Some stat.wv

  /// Rows from PGN embedded evals: for each position past the opening, the engine's
  /// in-game eval (wv, flipped to White's perspective, |wv|<4 to stay linear) vs
  /// material imbalance. No engine process needed — full-search evals straight from the PGN.
  let private pvCollectPgnEvalRows (path: string) (sample: int) (player: string option) : (float[] * float * int * int)[] =
      let rows = ResizeArray<float[] * float * int * int>()
      for gi, g in ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.indexed do
          try
              let white = g.GameMetaData.White
              let black = g.GameMetaData.Black
              let board = ChessLibrary.Chess.Board()
              let startFen =
                  if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                  else g.GameMetaData.Fen
              board.LoadFen startFen
              let mutable ply = 0
              for pm in g.Mainline do
                  // The wv on move m is the engine's eval of the position BEFORE m,
                  // from the mover's perspective → flip to White and pair with that position.
                  if ply >= 16 then
                      match pvParseWv pm.Comment with
                      // wv is already stored White-relative and in pawns (EB writes it via
                      // getEssentialDataWithEPS, which normalises to White) — do NOT flip.
                      // Continuous pawn target needs an intercept + tempo term, so include them.
                      // Keep decisive positions too (that's where material→eval is cleanest);
                      // only exclude mate sentinels (wv = ±200 from the parser).
                      | Some wv when abs wv < 20.0 ->
                          // Odd total piece count ⟹ side counts differ by an odd number ⟹
                          // guaranteed material imbalance. Use the board's native count.
                          let mutable pos = board.Position
                          let total = PositionTypes.PositionOps.numberOfPieces &pos
                          // --player filter: keep only plies where the named net was on move,
                          // so every wv is that single net's own eval (single-net assessment).
                          let onMove = if pos.STM = 0uy then white else black
                          let playerOk =
                              match player with
                              | None -> true
                              | Some pn -> onMove.IndexOf(pn, StringComparison.OrdinalIgnoreCase) >= 0
                          if total % 2 = 1 && playerOk then
                              let squares = pvParseFenBoard (board.FEN())
                              let md = pvMaterialDiff squares
                              let stm = if pos.STM = 0uy then 1.0 else -1.0
                              rows.Add([| 1.0; md.[0]; md.[1]; md.[2]; md.[3]; md.[4]; stm |], wv, total, gi)
                      | _ -> ()
                  board.PlaySanMove pm.San
                  ply <- ply + 1
          with _ -> ()
      let arr = rows.ToArray()
      if arr.Length <= sample then arr
      else
          let stride = float arr.Length / float sample
          [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

  // ---- Combination / contextual material-imbalance measurement ----
  // Matches positions by ABSOLUTE per-side material (not just the difference), so e.g.
  // BN-vs-BB (bishop pair) is distinguished from bare B-vs-N. A signature gives an exact
  // count for each INVOLVED piece type on each side; UNINVOLVED types must be equal across
  // sides (which forces pawns level by construction). Type order: [P;N;B;R;Q].
  let private pvSideCounts (squares: char[]) : int[] * int[] =
      let w = Array.zeroCreate 5
      let b = Array.zeroCreate 5
      let idx c = match c with 'P' -> 0 | 'N' -> 1 | 'B' -> 2 | 'R' -> 3 | 'Q' -> 4 | _ -> -1
      for c in squares do
          match c with
          | 'P' | 'N' | 'B' | 'R' | 'Q' -> let i = idx c in w.[i] <- w.[i] + 1
          | 'p' | 'n' | 'b' | 'r' | 'q' -> let i = idx (System.Char.ToUpper c) in b.[i] <- b.[i] + 1
          | _ -> ()
      w, b

  // Rows: (whiteCounts[5], blackCounts[5], z White-relative, wv option)
  let private pvComboRows (path: string) : (int[] * int[] * float * float option)[] =
      let rows = ResizeArray<int[] * int[] * float * float option>()
      for g in ChessLibrary.FullPGNParser.parsePgnFile path do
          let z = match g.GameMetaData.Result with "1-0" -> Some 1.0 | "0-1" -> Some -1.0 | "1/2-1/2" -> Some 0.0 | _ -> None
          match z with
          | None -> ()
          | Some zz ->
              try
                  let board = ChessLibrary.Chess.Board()
                  let startFen =
                      if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                      else g.GameMetaData.Fen
                  board.LoadFen startFen
                  let mutable ply = 0
                  for pm in g.Mainline do
                      board.PlaySanMove pm.San
                      ply <- ply + 1
                      if ply >= 16 then
                          let w, b = pvSideCounts (pvParseFenBoard (board.FEN()))
                          rows.Add(w, b, zz, pvParseWv pm.Comment)
              with _ -> ()
      rows.ToArray()

  // Does (w,b) match the signature with the A-side as White? Some = exact, None/None = equal.
  let private pvSigMatch (wr: int option[]) (br: int option[]) (w: int[]) (b: int[]) : bool =
      let mutable good = true
      for t in 0 .. 4 do
          match wr.[t], br.[t] with
          | Some wv, Some bv -> if w.[t] <> wv || b.[t] <> bv then good <- false
          | None, None -> if w.[t] <> b.[t] then good <- false
          | _ -> good <- false
      good

  // +1 = White is the A-side, -1 = Black is the A-side (mirror), 0 = no match
  let private pvComboOrient (wr: int option[]) (br: int option[]) (w: int[]) (b: int[]) : int =
      if pvSigMatch wr br w b then 1
      elif pvSigMatch wr br b w then -1
      else 0

  let private runPvCombo (pgn: string) =
    try
        let path = normalizePath pgn
        if not (File.Exists path) then failwithf "PGN not found: %s" path
        printfn "\nContextual material imbalances on:\n  %s\n" path
        printf "Collecting positions ..."
        let rows = pvComboRows path
        printfn " %d positions past ply 16.\n" rows.Length

        // build signature from involved (typeIndex, whiteCount, blackCount); P=0 N=1 B=2 R=3 Q=4
        let sigOf (involved: (int * int * int) list) =
            let wr = Array.create 5 None
            let br = Array.create 5 None
            for (t, wv, bv) in involved do wr.[t] <- Some wv; br.[t] <- Some bv
            wr, br
        let combos =
            [ "B vs N",     sigOf [ (2,1,0); (1,0,1) ]                 // bare, context-free baseline
              "R vs N",     sigOf [ (3,1,0); (1,0,1) ]
              "R vs B",     sigOf [ (3,1,0); (2,0,1) ]
              "BN vs BB",   sigOf [ (2,1,2); (1,1,0) ]                 // bishop pair: A=B+N, B=B+B
              "BB vs NN",   sigOf [ (2,2,0); (1,0,2) ]                 // pair vs two knights
              "BR vs BN",   sigOf [ (2,1,1); (3,1,0); (1,0,1) ]       // R vs N with a bishop each
              "RN vs RB",   sigOf [ (3,1,1); (1,1,0); (2,0,1) ]       // N vs B with a rook each
              "QN vs QB",   sigOf [ (4,1,1); (1,1,0); (2,0,1) ] ]     // N vs B with a queen each

        printfn "  %-12s %8s %10s %9s" "matchup" "n" "outcome" "eval(pw)"
        printfn "  %-12s %8s %10s %9s" "(A vs B)" "" "(+=A)" "(+=A)"
        for (label, (wr, br)) in combos do
            let acc = ResizeArray<float * float option>()
            for (w, b, z, wv) in rows do
                match pvComboOrient wr br w b with
                | 1 -> acc.Add(z, wv)
                | -1 -> acc.Add(-z, wv |> Option.map (fun v -> -v))
                | _ -> ()
            if acc.Count >= 20 then
                let mz = acc |> Seq.averageBy fst
                let evs = acc |> Seq.choose snd |> Array.ofSeq
                let me = if evs.Length > 0 then Array.average evs else nan
                printfn "  %-12s %8d %+10.3f %+9.2f" label acc.Count mz me
            else
                printfn "  %-12s %8d %10s %9s" label acc.Count "(few)" ""
        printfn ""
    with ex ->
        printfn "Error during pvcombo: %s" ex.Message

  /// Side to move is in check (a tactical position — exclude from a quiet fit).
  let private pvInCheck (fen: string) : bool =
      let mutable pos = RuntimeUtilities.BoardHelper.getPosFromFen (Some fen)
      MoveGeneration.InCheck &pos <> 0UL

  let private pvSquareIndexOf (s: string) : int =
      let file = int (Char.ToLower s.[0]) - int 'a'
      let rank = int s.[1] - int '1'
      if file >= 0 && file < 8 && rank >= 0 && rank < 8 then rank * 8 + file else -1

  /// Best move lands on an occupied square → a capture is on the board (tactical proxy).
  /// Misses en passant, which is rare enough to ignore for a quiet filter.
  let private pvBestIsCapture (squares: char[]) (bestmove: string) : bool =
      if bestmove.Length >= 4 then
          let toSq = pvSquareIndexOf (bestmove.Substring(2, 2))
          toSq >= 0 && squares.[toSq] <> '.'
      else false

  /// Replay each game's mainline SAN, collecting the FEN after every ply past the
  /// opening (ply ≥ 16), then stride-sample for spread across games and phases.
  let private pvCollectFensFromPgn (path: string) (sample: int) : string[] =
      let candidates = ResizeArray<string>()
      for g in ChessLibrary.FullPGNParser.parsePgnFile path do
          try
              let board = ChessLibrary.Chess.Board()
              let startFen =
                  if String.IsNullOrWhiteSpace g.GameMetaData.Fen then ChessLibrary.MiscTypes.startPosition
                  else g.GameMetaData.Fen
              board.LoadFen startFen
              let mutable ply = 0
              for pm in g.Mainline do
                  board.PlaySanMove pm.San
                  ply <- ply + 1
                  if ply >= 16 then candidates.Add(board.FEN())
          with _ -> ()
      let arr = candidates.ToArray()
      if arr.Length <= sample then arr
      else
          let stride = float arr.Length / float sample
          [| for i in 0 .. sample - 1 -> arr.[int (float i * stride)] |]

  let private runPieceValueFitEval (p: CliParser.PieceValueFitParams) =
    try
        let config = resolveEngineConfig p.Engine p.UciOptions
        let nodes = p.Nodes |> Option.defaultValue 1
        let path = normalizePath p.Positions
        if not (File.Exists path) then failwithf "Positions file not found: %s" path
        let isPgn = path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)
        let sampleN = p.Sample |> Option.defaultValue (if isPgn then 500 else Int32.MaxValue)
        let fens =
            if isPgn then
                pvCollectFensFromPgn path sampleN
            else
                EPDExtractor.readEPDs path
                |> Seq.map (fun e -> e.FEN)
                |> Seq.filter (fun f -> not (String.IsNullOrWhiteSpace f))
                |> Seq.truncate sampleN
                |> Seq.toArray

        printfn ""
        printfn "Engine:    %s" config.Name
        printfn "Positions: %s  (%s)" path (if isPgn then "PGN, quiet-filtered" else "EPD")
        printfn "Sampled:   %d   Search: nodes %d   Target: logit(White score)" fens.Length nodes
        printfn ""

        let engine = ChessLibrary.EngineHelper.createEngine(config, None)
        try
            printf "Evaluating %d positions " fens.Length
            let mutable nCheck = 0
            let mutable nCapture = 0
            let mutable nSat = 0
            let data =
                [| for idx in 0 .. fens.Length - 1 do
                     let fen = fens.[idx]
                     try
                         // Normalise short EPD FENs to 6 fields for the engine.
                         let fs = pvFields fen
                         let fen = if fs.Length = 4 then fen + " 0 1" else fen
                         let squares = pvParseFenBoard fen
                         let stmWhite = fs.Length < 2 || fs.[1] = "w"
                         if (idx + 1) % 20 = 0 then printf "."
                         if pvInCheck fen then
                             nCheck <- nCheck + 1   // quiet filter: side to move in check
                         else
                             let r = analyzePosition engine fen [] None None (Some nodes) false
                             match pvWdlToWhiteLogit stmWhite r.WDL with
                             | Some y when abs y < 4.0 ->   // keep the linear (unsaturated) regime
                                 if pvBestIsCapture squares r.Bestmove then
                                     nCapture <- nCapture + 1   // quiet filter: best move is a capture
                                 else
                                     let md = pvMaterialDiff squares
                                     let stm = if stmWhite then 1.0 else -1.0
                                     let feat = [| 1.0; md.[0]; md.[1]; md.[2]; md.[3]; md.[4]; stm |]
                                     yield feat, y, pvTotalPieces squares
                             | Some _ -> nSat <- nSat + 1    // saturated (|logit| ≥ 4)
                             | None -> ()
                     with _ -> () |]
            printfn " %d usable  (dropped: %d in-check, %d capture-best, %d saturated).\n"
                data.Length nCheck nCapture nSat

            let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
            let fitAndReport (label: string) (rows: (float[] * float * int)[]) =
                if rows.Length < 12 then
                    printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
                else
                    let X = rows |> Array.map (fun (f, _, _) -> f)
                    let y = rows |> Array.map (fun (_, v, _) -> v)
                    let variances =
                        [| for c in 1 .. 5 ->
                             let col = X |> Array.map (fun f -> f.[c])
                             let mean = Array.average col
                             col |> Array.averageBy (fun v -> (v - mean) * (v - mean)) |]
                    let beta = pvFitL1 X y
                    let bp = beta.[1]
                    let preds = X |> Array.map (fun f -> Array.fold2 (fun a b x -> a + b * x) 0.0 beta f)
                    let resid = Array.map2 (fun yy pp -> yy - pp) y preds
                    let mad = resid |> Array.averageBy abs
                    let ybar = Array.average y
                    let ssTot = y |> Array.sumBy (fun v -> (v - ybar) * (v - ybar))
                    let ssRes = resid |> Array.sumBy (fun v -> v * v)
                    let r2 = if ssTot > 1e-9 then 1.0 - ssRes / ssTot else 0.0
                    let valStr i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
                    printfn "── %s (n=%d) ──" label rows.Length
                    printfn "  Material variance: %s"
                        (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
                    if abs bp < 1e-6 || (variances |> Array.exists (fun v -> v < 0.02)) then
                        printfn "  ⚠️  Near-zero material variance — these positions are ~balanced, so the"
                        printfn "      coefficients below are unreliable. Use a material-imbalanced position set."
                    printfn "  βP: %.3f logits/pawn   tempo: %+.2f pawns   R²: %.3f   MAD: %.3f"
                        bp (if abs bp > 1e-9 then beta.[6] / bp else 0.0) r2 mad
                    printfn "  Implied values (pawn = 1.00):"
                    printfn "    Pawn 1.00   Knight %s   Bishop %s   Rook %s   Queen %s"
                        (valStr 2) (valStr 3) (valStr 4) (valStr 5)
                    printfn ""

            fitAndReport "All positions" data
            if p.ByPhase then
                fitAndReport "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp) -> tp > 16))
                fitAndReport "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp) -> tp <= 16))
        finally
            engine.StopProcess()
    with ex ->
        printfn "Error during piece-value fit: %s" ex.Message

  /// DeepMind-style fit: regress game OUTCOMES (not engine eval) on material imbalance
  /// through a tanh link. No engine needed — the target is the PGN result.
  let private runPieceValueFitOutcome (p: CliParser.PieceValueFitParams) =
    try
        let path = normalizePath p.Positions
        if not (File.Exists path) then failwithf "Positions file not found: %s" path
        if not (path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)) then
            failwith "--outcome requires a PGN file (it uses game results as the regression target)"
        let sampleN = p.Sample |> Option.defaultValue 20000
        printfn ""
        printfn "Positions: %s  (game-outcome regression — DeepMind tanh model)" path
        printfn "Target:    game result z in {-1,0,+1}, White's perspective   (engine arg ignored)"
        printfn ""
        printf "Collecting positions from games ..."
        let data = pvCollectOutcomeRows path sampleN
        printfn " %d positions.\n" data.Length

        let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
        let fit (label: string) (rows: (float[] * float * int * int)[]) =
            if rows.Length < 50 then
                printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
            else
                let X = rows |> Array.map (fun (f, _, _, _) -> f)
                let z = rows |> Array.map (fun (_, v, _, _) -> v)
                // features are [ΔP;ΔN;ΔB;ΔR;ΔQ] (cols 0..4), no intercept
                let variances =
                    [| for c in 0 .. 4 ->
                         let col = X |> Array.map (fun f -> f.[c])
                         let m = Array.average col
                         col |> Array.averageBy (fun v -> (v - m) * (v - m)) |]
                let beta = pvFitTanh X z
                let bp = beta.[0]
                let preds = X |> Array.map (fun f -> tanh (Array.fold2 (fun a b x -> a + b * x) 0.0 beta f))
                let mse = Array.map2 (fun zz pp -> (zz - pp) * (zz - pp)) z preds |> Array.average
                let imbalanced = Array.zip z preds |> Array.filter (fun (zz, _) -> zz <> 0.0)
                let signAcc =
                    if imbalanced.Length = 0 then 0.0
                    else imbalanced |> Array.averageBy (fun (zz, pp) -> if sign zz = sign pp then 1.0 else 0.0)
                let valStr i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
                printfn "── %s (n=%d) ──" label rows.Length
                printfn "  Material variance: %s"
                    (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
                if abs bp < 1e-9 || (variances |> Array.exists (fun v -> v < 0.02)) then
                    printfn "  ⚠️  Near-zero material variance — coefficients unreliable for this split."
                printfn "  βP: %.3f (tanh units/pawn)   MSE: %.3f   sign-acc on decisive: %.1f%%"
                    bp mse (signAcc * 100.0)
                printfn "  Implied values (pawn = 1.00):"
                printfn "    Pawn 1.00   Knight %s   Bishop %s   Rook %s   Queen %s"
                    (valStr 1) (valStr 2) (valStr 3) (valStr 4)
                match p.Bootstrap with
                | Some b when b > 0 ->
                    let estimate (rs: (float[] * float * int * int)[]) =
                        let bx = pvFitTanh (rs |> Array.map (fun (f, _, _, _) -> f)) (rs |> Array.map (fun (_, v, _, _) -> v))
                        if abs bx.[0] < 1e-9 then None
                        else Some [| bx.[1] / bx.[0]; bx.[2] / bx.[0]; bx.[3] / bx.[0]; bx.[4] / bx.[0] |]
                    match pvBootstrapCI rows estimate b with
                    | Some (ci, (glo, gmed, ghi)) ->
                        let s (lo, hi) = sprintf "[%.2f, %.2f]" lo hi
                        printfn "  95%% CI (game-clustered bootstrap, B=%d):" b
                        printfn "    Knight %s   Bishop %s   Rook %s   Queen %s"
                            (s ci.[0]) (s ci.[1]) (s ci.[2]) (s ci.[3])
                        printfn "    Bishop−Knight gap: %+.2f  95%% CI [%+.2f, %+.2f]  (%s zero)"
                            gmed glo ghi (if glo > 0.0 then "excludes" else "includes")
                    | None -> printfn "  (bootstrap: too few games to resample)"
                | _ -> ()
                printfn ""
        fit "All positions" data
        if p.ByPhase then
            fit "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp, _) -> tp > 16))
            fit "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp, _) -> tp <= 16))
    with ex ->
        printfn "Error during outcome fit: %s" ex.Message

  /// Regress the PGN's embedded eval (wv, in pawns) on material imbalance — linear,
  /// material-only (no intercept), L1. Coefficients are piece values in pawns directly.
  /// A continuous bounded target avoids the queen blow-up the tanh-on-outcome model had.
  let private runPieceValueFitPgnEval (p: CliParser.PieceValueFitParams) =
    try
        let path = normalizePath p.Positions
        if not (File.Exists path) then failwithf "Positions file not found: %s" path
        if not (path.EndsWith(".pgn", StringComparison.OrdinalIgnoreCase)) then
            failwith "--pgneval requires a PGN file with wv= eval annotations"
        let sampleN = p.Sample |> Option.defaultValue 20000
        printfn ""
        printfn "Positions: %s  (regression on embedded engine eval wv=, in pawns)" path
        printfn "Target:    in-game eval, White's perspective   (engine arg ignored)"
        match p.Player with
        | Some pn -> printfn "Player:    only plies where '%s' was on move (single-net eval values)" pn
        | None -> printfn "Player:    all moves (blended across both engines)"
        printfn ""
        printf "Collecting positions with evals from games ..."
        let data = pvCollectPgnEvalRows path sampleN p.Player
        printfn " %d positions.\n" data.Length

        let names = [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen" |]
        let fit (label: string) (rows: (float[] * float * int * int)[]) =
            if rows.Length < 50 then
                printfn "── %s ──  only %d positions — too few to fit.\n" label rows.Length
            else
                let X = rows |> Array.map (fun (f, _, _, _) -> f)
                let y = rows |> Array.map (fun (_, v, _, _) -> v)
                // features: [intercept; ΔP; ΔN; ΔB; ΔR; ΔQ; stm] — material in cols 1..5
                let variances =
                    [| for c in 1 .. 5 ->
                         let col = X |> Array.map (fun f -> f.[c])
                         let m = Array.average col
                         col |> Array.averageBy (fun v -> (v - m) * (v - m)) |]
                let beta = pvFitL1 X y      // linear L1; material coefficients are pawn values
                let bp = beta.[1]
                let preds = X |> Array.map (fun f -> Array.fold2 (fun a b x -> a + b * x) 0.0 beta f)
                let resid = Array.map2 (fun yy pp -> yy - pp) y preds
                let mad = resid |> Array.averageBy abs
                let ybar = Array.average y
                let ssTot = y |> Array.sumBy (fun v -> (v - ybar) * (v - ybar))
                let ssRes = resid |> Array.sumBy (fun v -> v * v)
                let r2 = if ssTot > 1e-9 then 1.0 - ssRes / ssTot else 0.0
                let nrm i = if abs bp > 1e-9 then sprintf "%.2f" (beta.[i] / bp) else "n/a"
                printfn "── %s (n=%d) ──" label rows.Length
                printfn "  Material variance: %s"
                    (String.concat "  " (Array.map2 (fun (nm: string) v -> sprintf "%s %.2f" nm v) names variances))
                if variances |> Array.exists (fun v -> v < 0.02) then
                    printfn "  ⚠️  Near-zero material variance — coefficients unreliable for this split."
                printfn "  Raw coefficients (eval-pawns per piece):"
                printfn "    Pawn %.2f   Knight %.2f   Bishop %.2f   Rook %.2f   Queen %.2f"
                    beta.[1] beta.[2] beta.[3] beta.[4] beta.[5]
                printfn "  Normalised (pawn = 1.00):  Knight %s   Bishop %s   Rook %s   Queen %s"
                    (nrm 2) (nrm 3) (nrm 4) (nrm 5)
                printfn "  intercept: %+.2f   tempo: %+.2f   βP: %.2f   R²: %.3f   MAD: %.3f pawns"
                    beta.[0] beta.[6] bp r2 mad
                match p.Bootstrap with
                | Some b when b > 0 ->
                    let estimate (rs: (float[] * float * int * int)[]) =
                        let bx = pvFitL1 (rs |> Array.map (fun (f, _, _, _) -> f)) (rs |> Array.map (fun (_, v, _, _) -> v))
                        if abs bx.[1] < 1e-9 then None
                        else Some [| bx.[2] / bx.[1]; bx.[3] / bx.[1]; bx.[4] / bx.[1]; bx.[5] / bx.[1] |]
                    match pvBootstrapCI rows estimate b with
                    | Some (ci, (glo, gmed, ghi)) ->
                        let s (lo, hi) = sprintf "[%.2f, %.2f]" lo hi
                        printfn "  95%% CI (game-clustered bootstrap, B=%d):" b
                        printfn "    Knight %s   Bishop %s   Rook %s   Queen %s"
                            (s ci.[0]) (s ci.[1]) (s ci.[2]) (s ci.[3])
                        printfn "    Bishop−Knight gap: %+.2f  95%% CI [%+.2f, %+.2f]  (%s zero)"
                            gmed glo ghi (if glo > 0.0 then "excludes" else "includes")
                    | None -> printfn "  (bootstrap: too few games to resample)"
                | _ -> ()
                printfn ""
        fit "All positions" data
        if p.ByPhase then
            fit "Middlegame (>16 pieces)" (data |> Array.filter (fun (_, _, tp, _) -> tp > 16))
            fit "Endgame (<=16 pieces)"   (data |> Array.filter (fun (_, _, tp, _) -> tp <= 16))
    with ex ->
        printfn "Error during PGN-eval fit: %s" ex.Message

  let runPieceValueFit (p: CliParser.PieceValueFitParams) =
    if p.PgnEval then runPieceValueFitPgnEval p
    elif p.Outcome then runPieceValueFitOutcome p
    else runPieceValueFitEval p

  let runPuzzles (path:string) (jsonOut: string option) =
    let startedUtc = DateTime.UtcNow
    let runStopwatch = System.Diagnostics.Stopwatch.StartNew()
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
            data.Concurrency,
            data.IncludeFailedPuzzles )

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
            RuntimeUtilities.ConsoleUtils.redConsole $"\nPuzzle Error: {msg}"
                  
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
                    | "policyvalue" | "dual" -> [ PuzzleEngineAgent.SubTest.PolicyValue ]
                    | "search" -> nodeList |> Array.toList |> List.map PuzzleEngineAgent.SubTest.Search
                    | "solve"  -> nodeList |> Array.toList |> List.map PuzzleEngineAgent.SubTest.Solve
                    | t when t.StartsWith("policytop") ->
                        match System.Int32.TryParse(t.Substring("policytop".Length)) with
                        | true, n when n >= 1 -> [ PuzzleEngineAgent.SubTest.PolicyTopN n ]
                        | _ ->
                            printfn "Invalid policytop value '%s', expected e.g. policytop3" t
                            []
                    | t when t.StartsWith("policy") && t.Length > 6 ->
                        match System.Int32.TryParse(t.Substring("policy".Length)) with
                        | true, n when n >= 1 -> [ PuzzleEngineAgent.SubTest.PolicyTopN n ]
                        | _ ->
                            printfn "Invalid policy value '%s', expected e.g. policy3" t
                            []
                    | t when t.StartsWith("valuetop") ->
                        match System.Int32.TryParse(t.Substring("valuetop".Length)) with
                        | true, n when n >= 1 -> [ PuzzleEngineAgent.SubTest.ValueTopN n ]
                        | _ ->
                            printfn "Invalid valuetop value '%s', expected e.g. valuetop3" t
                            []
                    | t when t.StartsWith("value") && t.Length > 5 ->
                        match System.Int32.TryParse(t.Substring("value".Length)) with
                        | true, n when n >= 1 -> [ PuzzleEngineAgent.SubTest.ValueTopN n ]
                        | _ ->
                            printfn "Invalid value value '%s', expected e.g. value3" t
                            []
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
        |> Seq.filter (fun e -> e.TotalNumber > 0 && (e.Type.Contains("Value") || e.Type.StartsWith("vTop")))
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

        let csvFileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.csv")
        writeFailedPuzzlesToCsv [policyScores; valueScores; search; solve] csvFileName

        let estCsvFileName = Path.Combine(escaped, $"estNodesHardest_{filenameFriendlyDate}.csv")
        writeHardestByEstNodesCsv [policyScores] estCsvFileName

        let allScoresForCross = Seq.concat [ policyScores; valueScores; search; solve ]
        PuzzleCrossEngine.writeCrossEngineFiles escaped filenameFriendlyDate allScoresForCross

        let testTypeInfo = String.Join("-", types)
        let engineCount = engineConfigs.Count
        //write table to file with date and time
        let tableFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt")
        //let tableFileName = Path.Combine(escaped, $"LichessPuzzleScore_{datePart}.txt")
        use tableWriter = new StreamWriter(tableFileName)
        tableWriter.WriteLine(table)

    // Optional structured JSON output for external tooling (e.g. Python tuner).
    // See Console/PuzzleJsonSchema.md for the public schema contract.
    match jsonOut with
    | Some out ->
        try
            let result =
                PuzzleJsonOutput.buildResult
                    normalizedPath
                    puzzles.Length
                    data.SampleSize
                    data.MinRating
                    data.MaxRating
                    data.PuzzleFilter
                    data.RatingGroups
                    startedUtc
                    runStopwatch.Elapsed.TotalSeconds
                    scores
            PuzzleJsonOutput.writeToFile out result
            printfn "  JSON results written: %s" out
        with ex ->
            RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write JSON results to {out}: {ex.Message}"
    | None -> ()

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
        match reason.Explanation with
        | "Checkmate" -> "mate"
        | "Stalemate" -> "stale"
        | "Tablebase known result" -> "tb-known"
        | "Insufficient material" -> "insuf-mat"
        | "Too many moves" -> "too-many"
        | "Repetition draw" -> "rep-draw"
        | "Evaluation agreement" -> "eval-agree"
        | "Time/node limit forfeit" -> "time-limit"
        | "Game was cancelled" -> "cancelled"
        | "Illegal move" -> "illegal"
        | "Not started" -> "not-started"
        | "Adjudicated by user" -> "adj-user"
        | s when s.EndsWith(" Disconnected") -> "disconn"
        | other -> other

    /// Define the MailboxProcessor for handling updates asynchronously
    let createUpdateProcessor (verbose:bool) =
        let mutable currentGameNr = 0
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
                        printfn "\u25C0 G%d  %-7s  %-12s  %dm %.1fs"
                            currentGameNr result.Result (shortReason result.Reason) result.Moves time
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
                        currentGameNr <- startGameInfo.CurrentGameNr
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
    |ex -> printfn "Caught an exception: %s" ex.Message

  // ---------- Batch piece-value analysis over a folder of nets ----------
  // For each net: nodes=1 self-play (RR), then outcome + pgneval endgame regressions.
  // Reuses runTournament and the pvfit helpers. Resumable (skips a net whose PGN is
  // already complete) and fault-tolerant (a failing net is logged and skipped).

  let private pvCountGames (pgn: string) : int =
      if not (File.Exists pgn) then 0
      else File.ReadLines pgn |> Seq.filter (fun l -> l.StartsWith("[Result ")) |> Seq.length

  /// Endgame (<=16 pieces) implied values (N,B,R,Q with pawn=1) from outcome regression.
  let private pvOutcomeEndgame (pgn: string) : (float * float * float * float) option =
      let rows = pvCollectOutcomeRows pgn 400000 |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
      if rows.Length < 50 then None
      else
          let b = pvFitTanh (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
          if abs b.[0] < 1e-9 then None
          else Some (b.[1] / b.[0], b.[2] / b.[0], b.[3] / b.[0], b.[4] / b.[0])

  /// Endgame implied values (N,B,R,Q with pawn=1) from PGN-eval regression.
  let private pvPgnEvalEndgame (pgn: string) : (float * float * float * float) option =
      let rows = pvCollectPgnEvalRows pgn 400000 None |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
      if rows.Length < 50 then None
      else
          let b = pvFitL1 (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
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
                            printfn "  running %d-game nodes=1 self-play..." rounds
                            runTournament tourny logger

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
    with ex ->
        printfn "Error during pvbatch: %s" ex.Message

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
            // stderr, not stdout: scriptable verbs (query) must own stdout for their JSON
            eprintfn "\nArguments provided to console app: %A" cliArgs
            for arg in cliArgs do
                match arg with
                | Verb (Perft (depth, sampleSize)) ->
                    printfn "Running Chess960 PERFT with depth: %d and sample size: %d" depth sampleSize
                    Test.completeFRCPerftVerificationTestFast depth sampleSize
                | Verb (Analyze p) ->
                    runAnalyze p
                | Verb (Compare p) ->
                    runCompare p
                | Verb (Query (fen, square, epd, pv)) ->
                    runQuery fen square epd pv
                | Verb (PieceValues p) ->
                    runPieceValues p
                | Verb (PieceValueFit p) ->
                    runPieceValueFit p
                | Verb (PvBatch p) ->
                    runPvBatch p
                | Verb (PvCombo pgn) ->
                    runPvCombo pgn
                | Verb (PuzzleJson (path, jsonOut)) ->
                    runPuzzles path jsonOut
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
                                PreventMoveDeviation = tourny.TestOptions.NumberOfGamesInParallel <= 1
                                MinMoveTimeInMS = 0
                                ConsoleOnly = true
                                DelayBetweenGames = TimeSpan.Zero
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
                    try
                        BenchmarkRunner.runBenchmark path
                    with ex ->
                        eprintfn "Benchmark failed: %s" ex.Message
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
                | Verb (PgnSummary path) ->
                    let normalizedPath = normalizePath path
                    if not (File.Exists normalizedPath) then
                        printfn "PGN file not found: %s" normalizedPath
                    else
                        try
                            ChessLibrary.Test.ParsingTests.pgnTerminationSummary normalizedPath 2 4.0 (3.0, 0.5) |> ignore
                        with ex ->
                            printfn "Error processing PGN file: %s" ex.Message
                | Verb (Elo path) ->
                    let normalizedPath = normalizePath path
                    if not (File.Exists normalizedPath) then
                        printfn "PGN file not found: %s" normalizedPath
                    else
                        try
                            let games = FullPGNParser.parsePgnFile normalizedPath
                            let consoleRes, _, _, _ = PGNCalculator.getEngineDataResults games
                            printfn "%s" consoleRes
                        with ex ->
                            printfn "Error processing PGN file: %s" ex.Message
                | Verb (Speed path) ->
                    let normalizedPath = normalizePath path
                    if not (File.Exists normalizedPath) then
                        printfn "PGN file not found: %s" normalizedPath
                    else
                        try
                            let games = FullPGNParser.parsePgnFile normalizedPath
                            let stats =
                                PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile(games, 0)
                                |> Array.filter _.Median
                                |> Array.sortByDescending _.AvgNPS
                            let table = ConsoleHelper.writeSummaryEngineStatsToConsole stats
                            printfn "%s" table
                        with ex ->
                            printfn "Error processing PGN file: %s" ex.Message
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
                    printfn ""
                    printfn "EngineBattle - Chess engine tournament, analysis, and puzzle testing"
                    printfn ""
                    printfn "Usage: EngineBattle <command> [arguments]"
                    printfn ""
                    printfn "Commands:"
                    printfn "  tournamentjson, tournament, t <config>  Run a tournament from JSON config"
                    printfn "  puzzlejson, puzzle, p <config> [--json <path>]"
                    printfn "                                          Run puzzle evaluation from JSON config"
                    printfn "                                          --json <path>: write structured results JSON for tooling"
                    printfn "  eretjson, eret <config>                 Run ERET evaluation from JSON config"
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
                    printfn "                                          Per net in the folder: nodes=1 self-play +"
                    printfn "                                          outcome/pgneval regressions -> summary.csv"
                    printfn "  benchmark, bench, b <config>            Run engine benchmark"
                    printfn "  tune <config>                           Run Bayesian parameter tuner"
                    printfn "  redash <config>                         Regenerate BO dashboard from saved state"
                    printfn "  pgnsummary, pgn, ps <pgnFile>           Analyze PGN game terminations"
                    printfn "  elo, e <pgnFile>                        Show ELO ratings and results from PGN"
                    printfn "  speed, sp <pgnFile>                     Show speed statistics from PGN"
                    printfn "  validate, v <config>                    Validate a tournament config without running"
                    printfn "  perft <depth> [sampleSize]              Run perft move generation test"
                    printfn "  gui [page] [port]                       Launch WebGUI (default: tournament, port 5018)"
                    printfn "  help, h                                 Show this help message"
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
                | _ ->
                    printfn "Unhandled argument: %A" arg
        0

