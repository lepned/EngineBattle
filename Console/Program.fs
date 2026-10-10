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

