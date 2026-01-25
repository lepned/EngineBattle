module WinboardProbeTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.Extensions.Logging
open Xunit
open ChessLibrary.WinboardIntegration
open ChessLibrary.WinboardProtocol
open ChessLibrary.Chess

type TestLogger() =
    interface ILogger with
        member _.BeginScope(_) = { new IDisposable with member _.Dispose() = () }
        member _.IsEnabled(_) = false
        member _.Log(_, _, _, _, _) = ()

let private isWinboardProbeEnabled () =
    false // Set to true to enable these tests.

let private startEngine (path: string) =
    let proc = new Process()
    proc.StartInfo.FileName <- path
    proc.StartInfo.WorkingDirectory <- Path.GetDirectoryName(path)
    if Path.GetFileName(path).ToLowerInvariant().StartsWith("crafty") then
        proc.StartInfo.Arguments <- "--xboard"
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardInput <- true
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.RedirectStandardError <- true
    proc.StartInfo.CreateNoWindow <- true
    if not (proc.Start()) then
        failwithf "Failed to start %s" path
    proc.StandardInput.NewLine <- "\n"
    proc.StandardInput.AutoFlush <- true
    proc

let private readLinesWithTimeout (queue: System.Collections.Concurrent.ConcurrentQueue<string>) (timeoutMs: int) =
    let lines = ResizeArray<string>()
    let sw = System.Diagnostics.Stopwatch.StartNew()
    while sw.ElapsedMilliseconds < int64 timeoutMs do
        let mutable line = null
        while queue.TryDequeue(&line) do
            lines.Add(line)
        Thread.Sleep(25)
    lines |> Seq.toList

let private readOptionLines (lines: string list) =
    lines
    |> List.filter (fun line -> line.TrimStart().StartsWith("option ", StringComparison.OrdinalIgnoreCase))

let private pumpOutput (proc: Process) (handler: WinboardHandler) (cts: CancellationTokenSource) =
    Task.Run(fun () ->
        while not cts.IsCancellationRequested && not proc.HasExited do
            let line = proc.StandardOutput.ReadLine()
            if isNull line then
                ()
            else
                handler.ProcessOutput(line) |> ignore
    )

type ExpectedFeatures = {
    Ping: bool option
    SetBoard: bool option
    UserMove: bool option
    Analyze: bool option
    TimeCmd: bool option
    OTimeCmd: bool option
    ForceCmd: bool option
    WhiteCmd: bool option
    BlackCmd: bool option
    PostCmd: bool option
    MoveNowCmd: bool option
    ExitCmd: bool option
    EasyCmd: bool option
    HardCmd: bool option
}

type ExpectedInit = {
    RequiresInit: bool
}

type ProbeResult = {
    Engine: string
    Path: string
    InitOk: bool
    IsInitialized: bool
    IsV1Fallback: bool
    Ping: bool
    SetBoard: bool
    UserMove: bool
    Analyze: bool
    TimeCmd: bool
    OTimeCmd: bool
    ForceCmd: bool
    WhiteCmd: bool
    BlackCmd: bool
    PostCmd: bool
    MoveNowCmd: bool
    ExitCmd: bool
    EasyCmd: bool
    HardCmd: bool
}

let private assertExpected (label: string) (expected: bool option) (actual: bool) =
    match expected with
    | Some value -> Assert.Equal(value, actual)
    | None -> ()

let private probeEngineResult (flipped:bool) (engineName: string) (path: string) : ProbeResult =
    use proc = startEngine path
    let logger = TestLogger() :> ILogger
    let handler = WinboardHandler(logger, engineName, flipped)
    use cts = new CancellationTokenSource()
    let _pump = pumpOutput proc handler cts

    let ok = initializeWinboard proc handler (Some logger) engineName 10000 |> Async.RunSynchronously

    if ok && not handler.IsV1Fallback then
        let probes = handler.GetProbePlan(false)
        for probe in probes do
            let probeAsync = handler.BeginProbe probe
            match probe.Kind with
            | ProbeKind.Ping ->
                match handler.GetProbePingCommand() with
                | Some pingCmd -> proc.StandardInput.WriteLine(pingCmd)
                | None -> ()
            | _ ->
                for cmd in probe.Commands do
                    proc.StandardInput.WriteLine(cmd)
            probeAsync |> Async.RunSynchronously |> ignore
            for cmd in probe.CleanupCommands do
                proc.StandardInput.WriteLine(cmd)

    let result = {
        Engine = engineName
        Path = path
        InitOk = ok
        IsInitialized = handler.IsInitialized
        IsV1Fallback = handler.IsV1Fallback
        Ping = handler.Features.Ping
        SetBoard = handler.Features.SetBoard
        UserMove = handler.Features.UserMove
        Analyze = handler.Features.Analyze
        TimeCmd = handler.Features.TimeCmd
        OTimeCmd = handler.Features.OTimeCmd
        ForceCmd = handler.Features.ForceCmd
        WhiteCmd = handler.Features.WhiteCmd
        BlackCmd = handler.Features.BlackCmd
        PostCmd = handler.Features.PostCmd
        MoveNowCmd = handler.Features.MoveNowCmd
        ExitCmd = handler.Features.ExitCmd
        EasyCmd = handler.Features.EasyCmd
        HardCmd = handler.Features.HardCmd
    }

    try
        proc.StandardInput.WriteLine("quit")
    with _ -> ()
    cts.Cancel()
    proc.WaitForExit(1000) |> ignore
    result

let private probeEngine (flipped:bool)  (engineName: string) (path: string) (expectedInit: ExpectedInit) (expected: ExpectedFeatures) =
    if not (isWinboardProbeEnabled()) then
        ()
    elif not (File.Exists(path)) then
        // Skip if engine isn't present on this machine.
        ()
    else
        let result = probeEngineResult flipped engineName path
        if expectedInit.RequiresInit then
            Assert.True(result.InitOk, $"Winboard init failed for {engineName}")
            Assert.True(result.IsInitialized, $"Handler not initialized for {engineName}")
        else
            Assert.True(not result.InitOk || not result.IsInitialized, $"{engineName} expected to fail init")

        if result.InitOk then
            assertExpected $"{engineName}.Ping" expected.Ping result.Ping
            assertExpected $"{engineName}.SetBoard" expected.SetBoard result.SetBoard
            assertExpected $"{engineName}.UserMove" expected.UserMove result.UserMove
            assertExpected $"{engineName}.Analyze" expected.Analyze result.Analyze
            assertExpected $"{engineName}.TimeCmd" expected.TimeCmd result.TimeCmd
            assertExpected $"{engineName}.OTimeCmd" expected.OTimeCmd result.OTimeCmd
            assertExpected $"{engineName}.ForceCmd" expected.ForceCmd result.ForceCmd
            assertExpected $"{engineName}.WhiteCmd" expected.WhiteCmd result.WhiteCmd
            assertExpected $"{engineName}.BlackCmd" expected.BlackCmd result.BlackCmd
            assertExpected $"{engineName}.PostCmd" expected.PostCmd result.PostCmd
            assertExpected $"{engineName}.MoveNowCmd" expected.MoveNowCmd result.MoveNowCmd
            assertExpected $"{engineName}.ExitCmd" expected.ExitCmd result.ExitCmd
            assertExpected $"{engineName}.EasyCmd" expected.EasyCmd result.EasyCmd
            assertExpected $"{engineName}.HardCmd" expected.HardCmd result.HardCmd

[<Fact>]
let ``Winboard probing detects capabilities for ant`` () =
    probeEngine false "ant" @"C:\Dev\Chess\Engines\WB_Natives\ant.exe"
        { RequiresInit = true }
        { Ping = Some false; SetBoard = Some true; UserMove = Some true; Analyze = Some false; TimeCmd = None; OTimeCmd = None; ForceCmd = None; WhiteCmd = None; BlackCmd = None; PostCmd = None; MoveNowCmd = None; ExitCmd = None; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for Comet_B68`` () =
    probeEngine false "Comet_B68" @"C:\Dev\Chess\Engines\WB_Natives\Comet_B68.exe"
        { RequiresInit = true }
        { Ping = Some false; SetBoard = Some true; UserMove = Some false; Analyze = Some true; TimeCmd = Some true; OTimeCmd = Some true; ForceCmd = Some true; WhiteCmd = None; BlackCmd = None; PostCmd = Some true; MoveNowCmd = Some false; ExitCmd = Some true; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for TheTurk`` () =
    probeEngine false "TheTurk" @"C:\Dev\Chess\Engines\WB_Natives\TheTurk.exe"
        { RequiresInit = true }
        { Ping = Some false; SetBoard = Some true; UserMove = Some true; Analyze = Some false; TimeCmd = Some true; OTimeCmd = Some true; ForceCmd = Some true; WhiteCmd = Some true; BlackCmd = Some true; PostCmd = Some true; MoveNowCmd = Some true; ExitCmd = Some true; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for Jonny400`` () =
    probeEngine false "Jonny400" @"C:\Dev\Chess\Engines\WB_Natives\Jonny400.exe"
        { RequiresInit = true }
        { Ping = Some false; SetBoard = Some false; UserMove = Some false; Analyze = Some false; TimeCmd = Some true; OTimeCmd = Some true; ForceCmd = Some true; WhiteCmd = Some true; BlackCmd = Some true; PostCmd = Some true; MoveNowCmd = Some true; ExitCmd = Some true; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for Thinker5.4DUCI`` () =
    probeEngine false "Thinker5.4DUCI" @"C:\Dev\Chess\Engines\WB_Natives\Thinker5.4DUCI.exe"
        { RequiresInit = true }
        { Ping = Some false; SetBoard = Some true; UserMove = Some true; Analyze = Some true; TimeCmd = Some true; OTimeCmd = Some true; ForceCmd = Some true; WhiteCmd = Some true; BlackCmd = Some true; PostCmd = Some true; MoveNowCmd = Some true; ExitCmd = Some true; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for xchenard64`` () =
    probeEngine false "xchenard64" @"C:\Dev\Chess\Engines\WB_Natives\xchenard64.exe"
        { RequiresInit = true }
        { Ping = Some true; SetBoard = Some true; UserMove = Some true; Analyze = Some false; TimeCmd = Some true; OTimeCmd = Some true; ForceCmd = Some true; WhiteCmd = Some false; BlackCmd = Some false; PostCmd = Some true; MoveNowCmd = Some false; ExitCmd = Some false; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard probing detects capabilities for TheKing350x64`` () =
    probeEngine false "TheKing350x64" @"C:\Dev\Chess\Engines\WB_Natives\TheKing350x64.exe"
        { RequiresInit = true }
        { Ping = None; SetBoard = None; UserMove = None; Analyze = None; TimeCmd = None; OTimeCmd = None; ForceCmd = None; WhiteCmd = None; BlackCmd = None; PostCmd = None; MoveNowCmd = None; ExitCmd = None; EasyCmd = None; HardCmd = None }

[<Fact>]
let ``Winboard move output formats parse`` () =
    let startBoard = Board()

    let withFen fen =
        let b = Board()
        b.LoadFen(fen)
        b

    let assertBestmove expected line board =
        let parsed = tryParseMoveOutput board line
        Assert.Equal(Some expected, parsed)

    assertBestmove "bestmove e2e4" "move e2e4" startBoard
    assertBestmove "bestmove e2e4" "e2e4" startBoard
    assertBestmove "bestmove g6h7" "1. ... g6h7" startBoard
    assertBestmove "bestmove e2e4" "e2-e4" startBoard
    assertBestmove "bestmove g6h7" "g6-h7" startBoard

    // SAN without move number
    let bishopBoard = withFen "8/8/8/8/8/8/2B5/4K3 w - - 0 1"
    assertBestmove "bestmove c2h7" "move Bh7" bishopBoard

    // SAN with move number and ellipsis
    let blackPawnBoard = withFen "8/8/8/3p4/8/8/8/4K3 b - - 0 1"
    assertBestmove "bestmove d5d4" "1. ... d4" blackPawnBoard

[<Fact>]
let ``Winboard PV formats normalize`` () =
    let withFen fen =
        let b = Board()
        b.LoadFen(fen)
        b

    let assertPvContains expectedTokens infoLine board =
        match parseThinkingOutput false board "test" infoLine with
        | Some uci ->
            Assert.Contains(" pv ", uci)
            for token in expectedTokens do
                Assert.Contains(token, uci)
        | None ->
            Assert.True(false, $"Expected pv in '{infoLine}'")

    // Coordinate PV with move number and ellipsis + hyphenated coords
    let fen1 = "rn1qkbnr/pp2ppp1/2p3bp/3pP2P/3P2P1/5P2/PPP5/RNBQKBNR b KQkq - 0 7"
    let board1 = withFen fen1
    assertPvContains [ "g6h7" ] "1 -13 0 234 1. ... g6-h7 b1c3 e7e6" board1

    // SAN PV with move number and ellipsis
    let board2 = withFen fen1
    assertPvContains [ "g6h7" ] "1 -13 0 234 1. ... Bh7 Nc3 e6" board2

    // SAN PV without move number
    let board3 = withFen fen1
    assertPvContains [ "g6h7" ] "1 -13 0 234 Bh7 Nc3 e6" board3

    // SAN PV with move number only
    let board4 = withFen "8/8/8/3p4/8/8/8/4K3 b - - 0 1"
    assertPvContains [ "d5d4" ] "1 -13 0 234 1. d4" board4

[<Fact>]
let ``Crafty analysis mode emits analysis and exits with go move`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty\crafty-23.5-64bit.exe"
    if not (isWinboardProbeEnabled()) then
        ()
    elif not (File.Exists(path)) then
        ()
    else
        use proc = startEngine path
        let outQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let errQueue = System.Collections.Concurrent.ConcurrentQueue<string>()

        let _outTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardOutput.ReadLine()
                    if isNull line then () else outQueue.Enqueue(line))
        let _errTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardError.ReadLine()
                    if isNull line then () else errQueue.Enqueue(line))

        proc.StandardInput.WriteLine("xboard")
        proc.StandardInput.WriteLine("protover 2")
        readLinesWithTimeout outQueue 1500 |> ignore

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        proc.StandardInput.WriteLine("post")
        proc.StandardInput.WriteLine("force")
        proc.StandardInput.WriteLine($"setboard {fen}")
        proc.StandardInput.WriteLine("analyze")

        let analysisLines = readLinesWithTimeout outQueue 3000
        let analysisRegex = System.Text.RegularExpressions.Regex(@"^\s*\d+\s+[+-]?\d+\s+\d+\s+\d+")
        let hasAnalysis = analysisLines |> List.exists (fun l -> analysisRegex.IsMatch(l))
        let analysisJoined = String.Join(" | ", analysisLines)
        Assert.True(hasAnalysis, $"Crafty produced no analysis lines. Output: {analysisJoined}")

        let moveRegex = System.Text.RegularExpressions.Regex(@"^(move\s+)?[a-h][1-8][a-h][1-8][qrbn]?$", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        let isMoveLine (line: string) =
            let t = line.Trim()
            t.StartsWith("move ", StringComparison.OrdinalIgnoreCase)
            || moveRegex.IsMatch(t)
            || t.StartsWith("hint:", StringComparison.OrdinalIgnoreCase)

        // Try "?" in analyze mode (move-now).
        proc.StandardInput.WriteLine("?")
        let moveLines0 = readLinesWithTimeout outQueue 5000
        let hasMove0 = moveLines0 |> List.exists isMoveLine

        // If no move, exit analysis and force idle.
        if not hasMove0 then
            proc.StandardInput.WriteLine("exit")
            proc.StandardInput.WriteLine("force")
        let moveLines1 = if hasMove0 then [] else readLinesWithTimeout outQueue 3000
        let hasMove1 = hasMove0 || (moveLines1 |> List.exists isMoveLine)

        // If still no move, start a fresh game in play mode and ask for a move.
        if not hasMove1 then
            proc.StandardInput.WriteLine("new")
            proc.StandardInput.WriteLine("black")
            proc.StandardInput.WriteLine("go")
        let moveLines2 = if hasMove1 then [] else readLinesWithTimeout outQueue 3000
        let hasMove2 = hasMove1 || (moveLines2 |> List.exists isMoveLine)

        // If still no move, send "?" to force a move.
        if not hasMove2 then
            proc.StandardInput.WriteLine("?")
        let moveLines3 = if hasMove2 then [] else readLinesWithTimeout outQueue 5000
        let hasMove3 = hasMove2 || (moveLines3 |> List.exists isMoveLine)

        let moveJoined = String.Join(" | ", moveLines0 @ moveLines1 @ moveLines2 @ moveLines3)
        let hasMove = hasMove3

        Assert.True(hasMove, $"Crafty produced no move after ?, exit, and go. Output: {moveJoined}")

        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore

[<Fact>]
let ``Crafty analyze stops after stop sequence`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty\crafty-23.5-64bit.exe"
    if not (isWinboardProbeEnabled()) then
        ()
    elif not (File.Exists(path)) then
        ()
    else
        use proc = startEngine path
        let outQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let _outTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardOutput.ReadLine()
                    if isNull line then () else outQueue.Enqueue(line))

        proc.StandardInput.WriteLine("xboard")
        proc.StandardInput.WriteLine("protover 2")
        readLinesWithTimeout outQueue 1500 |> ignore

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        proc.StandardInput.WriteLine("post")
        proc.StandardInput.WriteLine("force")
        proc.StandardInput.WriteLine($"setboard {fen}")
        proc.StandardInput.WriteLine("analyze")

        let analysisLines = readLinesWithTimeout outQueue 3000
        let analysisRegex = System.Text.RegularExpressions.Regex(@"^\s*\d+\s+[+-]?\d+\s+\d+\s+\d+")
        let hasAnalysis = analysisLines |> List.exists (fun l -> analysisRegex.IsMatch(l))
        let analysisJoined = String.Join(" | ", analysisLines)
        Assert.True(hasAnalysis, $"Crafty produced no analysis lines. Output: {analysisJoined}")

        // Simulate stop mapping in analysis mode: ? + exit + force
        proc.StandardInput.WriteLine("?")
        proc.StandardInput.WriteLine("exit")
        proc.StandardInput.WriteLine("force")

        let afterStopLines = readLinesWithTimeout outQueue 3000
        let hasAnalysisAfterStop = afterStopLines |> List.exists (fun l -> analysisRegex.IsMatch(l))
        let afterStopJoined = String.Join(" | ", afterStopLines)
        Assert.False(hasAnalysisAfterStop, $"Crafty continued analysis after stop. Output: {afterStopJoined}")

        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore

[<Fact>]
let ``Winboard probing discovery report`` () =
    if not (isWinboardProbeEnabled()) then
        ()
    else
        let engines = [
            ("ant", @"C:\Dev\Chess\Engines\WB_Natives\ant.exe")
            ("Comet_B68", @"C:\Dev\Chess\Engines\WB_Natives\Comet_B68.exe")
            ("Jonny400", @"C:\Dev\Chess\Engines\WB_Natives\Jonny400.exe")
            ("TheTurk", @"C:\Dev\Chess\Engines\WB_Natives\TheTurk.exe")
            ("Thinker5.4DUCI", @"C:\Dev\Chess\Engines\WB_Natives\Thinker5.4DUCI.exe")
            ("xchenard64", @"C:\Dev\Chess\Engines\WB_Natives\xchenard64.exe")
            ("TheKing350x64", @"C:\Dev\Chess\Engines\WB_Natives\TheKing350x64.exe")
            ("Crafty", @"C:\Dev\Chess\Engines\Crafty\crafty-23.5-64bit.exe")
        ]

        let results =
            engines
            |> List.choose (fun (name, path) ->
                if File.Exists(path) then
                    Some (probeEngineResult false name path)
                else
                    None)

        let outDir = Path.Combine(Directory.GetCurrentDirectory(), "TestResults")
        Directory.CreateDirectory(outDir) |> ignore
        let outPath = Path.Combine(outDir, "wb-probe.json")

        let options = JsonSerializerOptions(WriteIndented = true)
        options.Converters.Add(JsonStringEnumConverter())
        let json = JsonSerializer.Serialize(results, options)
        File.WriteAllText(outPath, json)

        Assert.True(results.Length > 0, "No engines were found to probe.")

[<Fact>]
let ``Crafty reports CECP options`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty\crafty-23.5-64bit.exe"
    if not (isWinboardProbeEnabled()) then
        ()
    elif not (File.Exists(path)) then
        ()
    else
        use proc = startEngine path
        let outQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let errQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let _outTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardOutput.ReadLine()
                    if isNull line then () else outQueue.Enqueue(line))
        let _errTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardError.ReadLine()
                    if isNull line then () else errQueue.Enqueue(line))

        proc.StandardInput.WriteLine("xboard")
        proc.StandardInput.WriteLine("protover 2")
        let outLines = readLinesWithTimeout outQueue 5000
        let errLines = readLinesWithTimeout errQueue 5000
        let optionLines = readOptionLines (outLines @ errLines)

        let outDir = Path.Combine(Directory.GetCurrentDirectory(), "TestResults")
        Directory.CreateDirectory(outDir) |> ignore
        let outPath = Path.Combine(outDir, "crafty-options.txt")
        if optionLines.Length > 0 then
            File.WriteAllLines(outPath, optionLines)
        else
            File.WriteAllLines(outPath, [ "No CECP option lines detected." ])

        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore
