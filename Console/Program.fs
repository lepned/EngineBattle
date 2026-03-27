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
      let ok = engine.WaitForReadyOk(60000)
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
            let ok = engine.WaitForReadyOk(60000)
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

        // Create engines once, reuse across all positions
        let engine1 = ChessLibrary.EngineHelper.createEngine(config1, None)
        let engine2 = ChessLibrary.EngineHelper.createEngine(config2, None)
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
                | Verb (Analyze p) ->
                    runAnalyze p
                | Verb (Compare p) ->
                    runCompare p
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
                    printfn "  puzzlejson, puzzle, p <config>          Run puzzle evaluation from JSON config"
                    printfn "  eretjson, eret <config>                 Run ERET evaluation from JSON config"
                    printfn "  analyze, a <engine> [fen] [options]      Analyze a position with an engine"
                    printfn "  compare, cmp <e1> <e2> [options]         Compare two engines side-by-side"
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

