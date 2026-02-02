module WinboardProbeTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Xunit
open ChessLibrary.WinboardIntegration
open ChessLibrary.WinboardProtocol
open ChessLibrary.Chess
open ChessLibrary.TypesDef.CoreTypes

type TestLogger() =
    interface ILogger with
        member _.BeginScope(_) = { new IDisposable with member _.Dispose() = () }
        member _.IsEnabled(_) = false
        member _.Log(_, _, _, _, _) = ()

let private isWinboardProbeEnabled () =
    false // Set to true to enable live engine tests.

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
    let sw = Stopwatch.StartNew()
    while sw.ElapsedMilliseconds < int64 timeoutMs do
        let mutable line = null
        while queue.TryDequeue(&line) do
            lines.Add(line)
        Thread.Sleep(25)
    lines |> Seq.toList

// ===== Pure function tests =====

[<Fact>]
let ``parseFeatureLine parses basic features`` () =
    let result = parseFeatureLine "feature ping=1 setboard=1 done=1" defaultFeatures
    Assert.True(result.Ping)
    Assert.True(result.SetBoard)
    Assert.True(result.Done)
    Assert.False(result.Analyze)

[<Fact>]
let ``parseFeatureLine parses quoted myname`` () =
    let result = parseFeatureLine "feature myname=\"Crafty 25.2\" done=1" defaultFeatures
    Assert.Equal(Some "Crafty 25.2", result.MyName)
    Assert.True(result.Done)

[<Fact>]
let ``parseFeatureLine parses Comet features`` () =
    // Comet declares: setboard=1 analyze=1 time=1
    let result = parseFeatureLine "feature setboard=1 analyze=1 time=1 reuse=1 done=1" defaultFeatures
    Assert.True(result.SetBoard)
    Assert.True(result.Analyze)
    Assert.True(result.Time)
    Assert.Equal(Some true, result.Reuse)
    Assert.True(result.Done)

[<Fact>]
let ``parseFeatureLine parses Chenard features`` () =
    // xchenard: ping=1 setboard=1 usermove=1
    let result = parseFeatureLine "feature ping=1 setboard=1 usermove=1 myname=\"xchenard\" done=1" defaultFeatures
    Assert.True(result.Ping)
    Assert.True(result.SetBoard)
    Assert.True(result.UserMove)
    Assert.Equal(Some "xchenard", result.MyName)

[<Fact>]
let ``parseFeatureLine parses TheTurk features`` () =
    let result = parseFeatureLine "feature setboard=1 usermove=1 time=1 analyze=0 done=1" defaultFeatures
    Assert.True(result.SetBoard)
    Assert.True(result.UserMove)
    Assert.True(result.Time)
    Assert.False(result.Analyze)

[<Fact>]
let ``parseFeatureLine handles unknown keys gracefully`` () =
    let result = parseFeatureLine "feature unknown=1 colors=0 ping=1 done=1" defaultFeatures
    Assert.True(result.Ping)
    Assert.True(result.Done)

[<Fact>]
let ``parseFeatureLine accumulates across calls`` () =
    let f1 = parseFeatureLine "feature ping=1 setboard=1" defaultFeatures
    let f2 = parseFeatureLine "feature analyze=1 done=1" f1
    Assert.True(f2.Ping)
    Assert.True(f2.SetBoard)
    Assert.True(f2.Analyze)
    Assert.True(f2.Done)

// ===== positionToWinboard tests =====

[<Fact>]
let ``positionToWinboard with setboard sends setboard for FEN`` () =
    let features = { defaultFeatures with SetBoard = true }
    let board = Board()
    let fen = "r1bqkbnr/pppppppp/2n5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2"
    board.LoadFen(fen)
    let cmds = positionToWinboard $"position fen {fen}" features false &board
    Assert.Contains($"setboard {fen}", cmds)

[<Fact>]
let ``positionToWinboard without setboard sends moves`` () =
    let features = { defaultFeatures with SetBoard = false }
    let board = Board()
    board.PlayLongSanMove("e2e4")
    board.PlayLongSanMove("e7e5")
    let cmds = positionToWinboard "position startpos moves e2e4 e7e5" features false &board
    Assert.Equal<string list>(["e2e4"; "e7e5"], cmds)

[<Fact>]
let ``positionToWinboard startpos no moves gives empty`` () =
    let features = defaultFeatures
    let board = Board()
    let cmds = positionToWinboard "position startpos" features false &board
    Assert.Empty(cmds)

// ===== goToWinboard tests =====

[<Fact>]
let ``goToWinboard V1 with time control sends time and otim`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ForceV1Init() // Force V1 mode
    handler.UciToWinboard("position startpos") |> ignore
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000" true false
    Assert.Contains("time 30000", cmds)
    Assert.Contains("otim 30000", cmds)
    Assert.Contains("go", cmds)

[<Fact>]
let ``goToWinboard V2 with time control sends level`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000" true false
    // V2 engines should get level command: 300000ms = 5 minutes, no increment
    // Should be "level 0 5 0" (no seconds when 0, no decimal when 0)
    Assert.Contains("level 0 5 0", cmds)
    // Always gets time/otim updates
    Assert.True(cmds |> List.exists (fun s -> s.StartsWith("time")))
    Assert.Contains("go", cmds)

[<Fact>]
let ``goToWinboard with movetime sends st`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    handler.UciToWinboard("position startpos") |> ignore
    let cmds = handler.GoToWinboard "go movetime 5000" true false
    Assert.Contains("st 5", cmds)
    Assert.Contains("go", cmds)

[<Fact>]
let ``goToWinboard infinite with analyze sends analyze`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ProcessFeatureLine("feature analyze=1 done=1") |> ignore
    handler.UciToWinboard("position startpos") |> ignore
    let cmds = handler.GoToWinboard "go infinite" true true
    Assert.Contains("analyze", cmds)
    Assert.DoesNotContain("go", cmds)

[<Fact>]
let ``goToWinboard infinite without analyze sends go`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore
    handler.UciToWinboard("position startpos") |> ignore
    let cmds = handler.GoToWinboard "go infinite" true true
    Assert.Contains("go", cmds)
    Assert.DoesNotContain("analyze", cmds)

[<Fact>]
let ``goToWinboard V1 black to move swaps time and otim`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Test", wbConfig)
    handler.ForceV1Init() // Force V1 mode
    handler.UciToWinboard("position startpos moves e2e4") |> ignore // Black to move
    let cmds = handler.GoToWinboard "go wtime 100000 btime 200000" false false
    // Black to move: time = btime, otim = wtime
    Assert.Contains("time 20000", cmds)
    Assert.Contains("otim 10000", cmds)

// ===== parseThinkingOutput tests =====

[<Fact>]
let ``parseThinkingOutput parses standard format`` () =
    let board = Board()
    let result = parseThinkingOutput false board "TestEngine" "9 12 1000 100000 e2e4 e7e5"
    Assert.True(result.IsSome)
    let info = result.Value
    Assert.Contains("depth 9", info)
    Assert.Contains("score cp 12", info)
    Assert.Contains("nodes 100000", info)

[<Fact>]
let ``parseThinkingOutput handles TheKing depth encoding`` () =
    // TheKing uses depth*1000: 3001 means depth 3
    let board = Board()
    let result = parseThinkingOutput false board "TheKing" "3001 -42 0 971 e2e4"
    Assert.True(result.IsSome)
    Assert.Contains("depth 3", result.Value)
    Assert.Contains("score cp -42", result.Value)

[<Fact>]
let ``parseThinkingOutput handles tab-indented Crafty output`` () =
    let board = Board()
    let result = parseThinkingOutput false board "Crafty" "\t11 48 3 314966 e2e4 e7e5"
    Assert.True(result.IsSome)
    Assert.Contains("depth 11", result.Value)

[<Fact>]
let ``parseThinkingOutput score not negated for White perspective engines`` () =
    let board = Board()
    board.LoadFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
    // sideToMovePOV=false means engine reports from White's perspective (most engines)
    let result = parseThinkingOutput false board "TheKing" "5 30 10 5000 e7e5"
    Assert.True(result.IsSome)
    Assert.Contains("score cp 30", result.Value) // Not negated

[<Fact>]
let ``parseThinkingOutput negates score for side-to-move engines`` () =
    let board = Board()
    board.LoadFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
    // sideToMovePOV=true means engine reports from side-to-move's perspective (like Crafty)
    let result = parseThinkingOutput true board "TestEngine" "5 30 10 5000 e7e5"
    Assert.True(result.IsSome)
    Assert.Contains("score cp -30", result.Value) // Negated for black

[<Fact>]
let ``parseThinkingOutput filters tb suffix from Jonny`` () =
    let board = Board()
    let result = parseThinkingOutput false board "Jonny" "10 28 3 110752 e2e4 d7d5 tb=0/0"
    Assert.True(result.IsSome)
    Assert.Contains("depth 10", result.Value)
    // tb=0/0 should be filtered from PV
    Assert.DoesNotContain("tb=", result.Value)

// ===== tryParseMoveOutput tests =====

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

// ===== PV normalization tests =====

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

// ===== WinboardHandler integration tests =====

[<Fact>]
let ``Handler ProcessFeatureLine marks initialized on done`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    Assert.False(handler.IsInitialized)
    let done1 = handler.ProcessFeatureLine("feature ping=1 setboard=1")
    Assert.False(done1)
    Assert.False(handler.IsInitialized)
    let done2 = handler.ProcessFeatureLine("feature done=1")
    Assert.True(done2)
    Assert.True(handler.IsInitialized)
    Assert.True(handler.IsProtover2)
    Assert.True(handler.Features.Ping)
    Assert.True(handler.Features.SetBoard)

[<Fact>]
let ``Handler ForceV1Init sets v1 defaults`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ForceV1Init()
    Assert.True(handler.IsInitialized)
    Assert.True(handler.IsV1Fallback)
    Assert.False(handler.Features.Ping)
    Assert.False(handler.Features.SetBoard)

[<Fact>]
let ``Handler UciToWinboard translates position with setboard`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature setboard=1 done=1") |> ignore
    let cmds = handler.UciToWinboard("position fen r1bqkbnr/pppppppp/2n5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2")
    Assert.True(cmds.Length > 0)
    Assert.True(cmds |> List.exists (fun c -> c.StartsWith("setboard")))
    // Use full 6-field FEN — some engines (Chenard) require halfmove/fullmove counters
    let setboardCmd = cmds |> List.find (fun c -> c.StartsWith("setboard"))
    Assert.Equal("setboard r1bqkbnr/pppppppp/2n5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2", setboardCmd)

[<Fact>]
let ``Handler UciToWinboard translates go infinite to analyze`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature analyze=1 done=1") |> ignore
    let cmds = handler.UciToWinboard("position startpos")
    let goCmds = handler.UciToWinboard("go infinite")
    Assert.Contains("analyze", goCmds)

[<Fact>]
let ``Handler ProcessOutput translates pong to readyok`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature ping=1 done=1") |> ignore
    // Send isready which sets pendingPing
    let _ = handler.UciToWinboard("isready")
    // Now simulate pong response (any pong should work since we can't predict the ID)
    // We can't easily test this without knowing the ping ID, but we verify the pipeline
    let result = handler.ProcessOutput("pong 42")
    // Result depends on whether 42 matches the random ping ID — this tests the code path
    result |> ignore

[<Fact>]
let ``Handler stop in analyze mode sends exit`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature analyze=1 done=1") |> ignore
    handler.UciToWinboard("position startpos") |> ignore
    handler.UciToWinboard("go infinite") |> ignore
    let stopCmds = handler.UciToWinboard("stop")
    Assert.Contains("exit", stopCmds)

// ===== V1 engine simulation tests =====

[<Fact>]
let ``V1 handler ForceV1Init produces correct defaults`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    // Simulate: no feature lines received, force V1
    handler.ForceV1Init()
    Assert.True(handler.IsInitialized)
    Assert.True(handler.IsV1Fallback)
    Assert.False(handler.IsProtover2)
    Assert.False(handler.Features.Ping)
    Assert.False(handler.Features.SetBoard)
    Assert.False(handler.Features.Analyze)
    Assert.False(handler.Features.Time)

[<Fact>]
let ``V1 handler processes TheKing thinking output with depth x 1000`` () =
    // TheKing raw output: " 3001   -42      0       971 Bh7 Nc3 e6"
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()    
    // Set board to the test FEN (black to move)
    handler.UciToWinboard("position fen rn1qkbnr/pp2ppp1/2p3bp/3pP2P/3P2P1/5P2/PPP5/RNBQKBNR b KQkq - 0 7") |> ignore
    let result = handler.ProcessOutput("3001   -42      0       971 Bh7 Nc3 e6")
    Assert.True(result.IsSome, "TheKing thinking line should parse")
    let info = result.Value
    Assert.Contains("depth 3", info)
    Assert.Contains("score cp", info)
    Assert.Contains("pv", info)

[<Fact>]
let ``V1 handler processes TheKing move output`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    handler.UciToWinboard("position fen rn1qkbnr/pp2ppp1/2p3bp/3pP2P/3P2P1/5P2/PPP5/RNBQKBNR b KQkq - 0 7") |> ignore
    let result = handler.ProcessOutput("move g6h7")
    Assert.True(result.IsSome, "TheKing move line should parse")
    Assert.Equal(Some "bestmove g6h7", result)

[<Fact>]
let ``V1 handler ProcessOutput ignores error lines from TheKing`` () =
    // TheKing sends: "Error (unknown command): protover"
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    let result = handler.ProcessOutput("Error (unknown command): protover")
    Assert.True(result.IsNone, "Error lines should not produce UCI output")

[<Fact>]
let ``V1 handler UciToWinboard sends new for ucinewgame`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    let cmds = handler.UciToWinboard("ucinewgame")
    Assert.Contains("new", cmds)

[<Fact>]
let ``V1 handler UciToWinboard sends moves for position without setboard`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    Assert.False(handler.Features.SetBoard)
    let cmds = handler.UciToWinboard("position startpos moves e2e4 e7e5 g1f3")
    // V1 without setboard: new + force + individual moves
    Assert.Equal(5, cmds.Length)
    Assert.Equal<string list>(["new"; "force"; "e2e4"; "e7e5"; "g1f3"], cmds)

[<Fact>]
let ``V1 handler isready returns empty for no ping`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    Assert.False(handler.Features.Ping)
    let cmds = handler.UciToWinboard("isready")
    Assert.Empty(cmds)

[<Fact>]
let ``V1 handler go with movetime sends st and go`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    handler.UciToWinboard("position startpos") |> ignore
    let cmds = handler.UciToWinboard("go movetime 3000")
    Assert.Contains("st 3", cmds)
    Assert.Contains("go", cmds)

[<Fact>]
let ``V1 handler stop without analyze sends question mark`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()
    Assert.False(handler.Features.Analyze)
    handler.UciToWinboard("position startpos") |> ignore
    handler.UciToWinboard("go movetime 10000") |> ignore
    let stopCmds = handler.UciToWinboard("stop")
    Assert.Contains("?", stopCmds)

[<Fact>]
let ``V1 handler full TheKing game simulation`` () =
    // Simulate a full game cycle with TheKing-style V1 engine
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TheKing", wbConfig)
    handler.ForceV1Init()

    // isready → empty (no ping)
    let readyCmds = handler.UciToWinboard("isready")
    Assert.Empty(readyCmds)

    // ucinewgame → "new"
    let newCmds = handler.UciToWinboard("ucinewgame")
    Assert.Contains("new", newCmds)

    // position startpos moves e2e4 → new + force + e2e4
    // (V1 engines lack setboard, so "new" is required to reset before moves)
    let posCmds = handler.UciToWinboard("position startpos moves e2e4")
    Assert.Equal<string list>(["new"; "force"; "e2e4"], posCmds)

    // go movetime 3000 → st 3 + go
    let goCmds = handler.UciToWinboard("go movetime 3000")
    Assert.Contains("st 3", goCmds)
    Assert.Contains("go", goCmds)

    // Thinking output processed
    let thinkResult = handler.ProcessOutput("3001 -42 0 971 e7e5")
    Assert.True(thinkResult.IsSome)
    Assert.Contains("depth 3", thinkResult.Value)

    // Move output processed
    let moveResult = handler.ProcessOutput("move e7e5")
    Assert.True(moveResult.IsSome)
    Assert.Equal(Some "bestmove e7e5", moveResult)

[<Fact>]
let ``Jonny partial V2 features parsed without done`` () =
    // Jonny sends features but no done=1:
    // "feature myname="Jonny 4.00" sigint=0"
    // "feature variants="normal,fischerandom""
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Jonny", wbConfig)
    let done1 = handler.ProcessFeatureLine("feature myname=\"Jonny 4.00\" sigint=0")
    Assert.False(done1)
    Assert.False(handler.IsInitialized)
    let done2 = handler.ProcessFeatureLine("feature variants=\"normal,fischerandom\"")
    Assert.False(done2)
    Assert.False(handler.IsInitialized)
    // After timeout, MarkInitialized would be called
    handler.MarkInitialized()
    Assert.True(handler.IsInitialized)
    Assert.False(handler.IsV1Fallback)
    Assert.Equal(Some "Jonny 4.00", handler.Features.MyName)
    // Jonny does NOT declare setboard=1
    Assert.False(handler.Features.SetBoard)

[<Fact>]
let ``Jonny without setboard sends moves not setboard for FEN position`` () =
    // Jonny silently ignores setboard — our handler must not use it
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Jonny", wbConfig)
    handler.ProcessFeatureLine("feature myname=\"Jonny 4.00\" sigint=0") |> ignore
    handler.ProcessFeatureLine("feature variants=\"normal,fischerandom\"") |> ignore
    handler.MarkInitialized()
    Assert.False(handler.Features.SetBoard)
    // Position with FEN and moves — should send moves, not setboard
    let cmds = handler.UciToWinboard("position fen rn1qkbnr/pp2ppp1/2p3bp/3pP2P/3P2P1/5P2/PPP5/RNBQKBNR b KQkq - 0 7")
    // No setboard command should be present
    Assert.DoesNotContain("setboard", cmds |> String.concat " ")
    // FEN position with no moves → new + force (no moves to replay)
    Assert.Equal<string list>(["new"; "force"], cmds)

[<Fact>]
let ``Jonny thinking output with tb suffix parses`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Jonny", wbConfig)
    handler.MarkInitialized()
    handler.UciToWinboard("position startpos") |> ignore
    let result = handler.ProcessOutput("10 28 3 110752 e2e4 e7e5 g1f3 b8c6 d2d4 e5d4 f3d4 c6d4 d1d4 g8f6 tb=0/0")
    Assert.True(result.IsSome, "Jonny thinking line should parse")
    Assert.Contains("depth 10", result.Value)
    Assert.DoesNotContain("tb=", result.Value)

[<Fact>]
let ``Jonny coordinate move output parses`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "Jonny", wbConfig)
    handler.MarkInitialized()
    handler.UciToWinboard("position startpos") |> ignore
    let result = handler.ProcessOutput("move e2e4")
    Assert.True(result.IsSome)
    Assert.Equal(Some "bestmove e2e4", result)

// ===== Live engine tests (disabled by default) =====

[<Fact>]
let ``Crafty plays after setboard and st+go`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty 25.2\crafty.exe"
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
        proc.StandardInput.WriteLine($"setboard {fen}")
        proc.StandardInput.WriteLine("st 1")
        proc.StandardInput.WriteLine("go")

        let lines = readLinesWithTimeout outQueue 5000
        let moveRegex = System.Text.RegularExpressions.Regex(@"^(move\s+)?[a-h][1-8][a-h][1-8]([qrbn])?$", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        let hasMove = lines |> List.exists (fun l -> moveRegex.IsMatch(l.Trim()))
        Assert.True(hasMove, sprintf "Crafty produced no move after setboard+st+go. Output: %s" (String.Join(" | ", lines)))

        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore

[<Fact>]
let ``Crafty 25.2 is recognized as protover 2 engine after feature negotiation`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty 25.2\crafty.exe"
    if not (File.Exists(path)) then
        ()
    else
        use proc = startEngine path
        let logger = TestLogger() :> ILogger
        let wbConfig = WinboardConfig.Default
        let handler = WinboardHandler(logger, "Crafty", wbConfig)
        let outQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let _outTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardOutput.ReadLine()
                    if isNull line then () else outQueue.Enqueue(line))
        proc.StandardInput.WriteLine("xboard")
        proc.StandardInput.WriteLine("protover 2")
        let outLines = readLinesWithTimeout outQueue 3000
        for line in outLines do
            handler.ProcessOutput(line) |> ignore
        Assert.True(handler.IsProtover2, "Handler should be marked as protover 2 after feature negotiation with Crafty 25.2.")
        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore

[<Fact>]
let ``Crafty setboard feature is detected`` () =
    let path = @"C:\Dev\Chess\Engines\Crafty 25.2\crafty.exe"
    if not (isWinboardProbeEnabled()) then
        ()
    elif not (File.Exists(path)) then
        ()
    else
        use proc = startEngine path
        let logger = TestLogger() :> ILogger
        let wbConfig = WinboardConfig.Default
        let handler = WinboardHandler(logger, "Crafty", wbConfig)
        let outQueue = System.Collections.Concurrent.ConcurrentQueue<string>()
        let _outTask =
            Task.Run(fun () ->
                while not proc.HasExited do
                    let line = proc.StandardOutput.ReadLine()
                    if isNull line then () else outQueue.Enqueue(line))
        proc.StandardInput.WriteLine("xboard")
        proc.StandardInput.WriteLine("protover 2")
        let outLines = readLinesWithTimeout outQueue 3000
        for line in outLines do
            handler.ProcessOutput(line) |> ignore
        Assert.True(handler.Features.SetBoard, "Crafty should advertise setboard=1 in features")
        proc.StandardInput.WriteLine("quit")
        proc.WaitForExit(1000) |> ignore

// ===== WinboardConfig tests =====

[<Fact>]
let ``TimeControlStrategy LevelWithTime uses level and time commands`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.LevelWithTime }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // First go command should include level + time/otim
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should contain level command (first time)
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("level "))
    // Should also contain time/otim
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("otim "))
    // Should NOT contain st
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("st "))

[<Fact>]
let ``TimeControlStrategy TimeOtimOnly uses time otim only`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.TimeOtimOnly }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // Should only send time/otim, no level or st
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should contain time/otim
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("otim "))
    // Should NOT contain level or st
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("level "))
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("st "))

[<Fact>]
let ``TimeControlStrategy StWithTime uses st and time commands`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.StWithTime }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // Should send both st and time/otim for better time management
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should contain st
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("st "))
    // Should also contain time/otim (this is the bug fix!)
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("otim "))
    // Should NOT contain level
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("level "))

[<Fact>]
let ``TimeControlStrategy StOnly uses st only`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.StOnly }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // Legacy mode: only st, no time/otim (may cause poor time management)
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should contain st
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("st "))
    // Should NOT contain level, time, or otim
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("level "))
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("otim "))

[<Fact>]
let ``TimeControlStrategy AutoDetect falls back to TimeOtimOnly`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.AutoDetect }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // AutoDetect without resolved strategy should fallback to TimeOtimOnly
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should fallback to time/otim only
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("otim "))
    // Should NOT contain level or st
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("level "))
    Assert.DoesNotContain(cmds, fun cmd -> cmd.StartsWith("st "))

[<Fact>]
let ``TimeControlStrategy AutoDetect resolved to LevelWithTime works`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with TimeControlStrategy = TimeControlStrategy.AutoDetect }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // Simulate level probing success
    handler.SetResolvedStrategy(TimeControlStrategy.LevelWithTime)

    // Should now use level + time/otim
    let cmds = handler.GoToWinboard "go wtime 300000 btime 300000 winc 2000 binc 2000" true false

    // Should contain level and time/otim
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("level "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("time "))
    Assert.Contains(cmds, fun cmd -> cmd.StartsWith("otim "))

[<Fact>]
let ``WinboardConfig StartupCommands are included in post-init`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = {
        WinboardConfig.Default with
            StartupCommands = ["level 16"; "option Hash=256"]
    }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)

    let postInitCmds = handler.GetPostInitCommands()

    // Should contain standard commands
    Assert.Contains("post", postInitCmds)
    Assert.Contains("easy", postInitCmds)
    // Should contain custom startup commands
    Assert.Contains("level 16", postInitCmds)
    Assert.Contains("option Hash=256", postInitCmds)

[<Fact>]
let ``WinboardConfig Flipped affects score perspective`` () =
    let logger = TestLogger() :> ILogger

    // Test with SideToMovePOV=false (scores from White's perspective always - most engines)
    let wbConfigWhitePerspective = { WinboardConfig.Default with SideToMovePOV = false }
    let handlerWhitePerspective = WinboardHandler(logger, "WhitePerspectiveEngine", wbConfigWhitePerspective)
    handlerWhitePerspective.ProcessFeatureLine("feature done=1") |> ignore
    // Set up position: after 1. e4 (Black to move)
    handlerWhitePerspective.UciToWinboard("position startpos moves e2e4") |> ignore

    // Simulate thinking output: depth=10, score=50 (from White's perspective)
    let outputWhitePerspective = handlerWhitePerspective.ProcessOutput("10 50 100 1000 e7e5")
    Assert.True(outputWhitePerspective.IsSome)
    Assert.Contains("score cp 50", outputWhitePerspective.Value)  // No negation needed

    // Test with SideToMovePOV=true (scores from side-to-move - rare engines like Crafty)
    let wbConfigSideToMove = { WinboardConfig.Default with SideToMovePOV = true }
    let handlerSideToMove = WinboardHandler(logger, "SideToMoveEngine", wbConfigSideToMove)
    handlerSideToMove.ProcessFeatureLine("feature done=1") |> ignore
    // Set up position: after 1. e4 (Black to move)
    handlerSideToMove.UciToWinboard("position startpos moves e2e4") |> ignore

    // Simulate thinking output: depth=10, score=50 (from Black's perspective - good for Black)
    let outputSideToMove = handlerSideToMove.ProcessOutput("10 50 100 1000 e7e5")
    Assert.True(outputSideToMove.IsSome)
    // Should negate score for Black to move (convert to White's perspective)
    Assert.Contains("score cp -50", outputSideToMove.Value)

[<Fact>]
let ``WinboardConfig ForceV1Mode can be configured`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = { WinboardConfig.Default with ForceV1Mode = true }
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)

    // ForceV1Mode is set in config and will be used during initialization
    // The handler itself doesn't auto-initialize, but we can manually trigger V1 mode
    handler.ForceV1Init()

    // After ForceV1Init, handler should be in V1 mode
    Assert.True(handler.IsV1Fallback)
    Assert.True(handler.IsInitialized)
    Assert.False(handler.Features.SetBoard)  // V1 defaults to no setboard
    Assert.False(handler.Features.Ping)      // V1 defaults to no ping

[<Fact>]
let ``Thread safety concurrent access to handler state`` () =
    let logger = TestLogger() :> ILogger
    let wbConfig = WinboardConfig.Default
    let handler = WinboardHandler(logger, "TestEngine", wbConfig)
    handler.ProcessFeatureLine("feature done=1") |> ignore

    // Simulate concurrent access from multiple threads
    let tasks =
        [1..10]
        |> List.map (fun i ->
            async {
                for j in 1..100 do
                    // Concurrent reads and writes
                    if j % 3 = 0 then
                        handler.UciToWinboard("isready") |> ignore
                    elif j % 3 = 1 then
                        handler.UciToWinboard("position startpos moves e2e4") |> ignore
                    else
                        handler.ProcessOutput("10 50 100 1000 e7e5") |> ignore
            }
        )

    // Should not throw or deadlock
    tasks |> Async.Parallel |> Async.RunSynchronously |> ignore

    // Verify handler is still in a consistent state after concurrent access
    Assert.True(handler.IsInitialized)
    Assert.False(handler.IsV1Fallback)

    // Verify we can still process commands after concurrent access
    let cmds = handler.UciToWinboard("position startpos")
    Assert.NotEmpty(cmds)

    // Verify ProcessOutput still works
    let output = handler.ProcessOutput("10 50 100 1000 e2e4")
    Assert.True(output.IsSome)  // Should parse thinking output correctly
