/// Characterisation tests for ChessLibrary/Engine/Engine.fs, written against the code as it stood
/// before the rewrite (branch rewrite/engine-fs, 2026-09-28). They pin down what the two engine
/// classes SEND to an engine and what they REPORT back, so the rewrite can be held to the same
/// behaviour. Where a test pins a quirk rather than a design, the comment says so - those are the
/// places to decide deliberately, not to change by accident.
///
/// Every test runs FakeUciEngine (a sibling project, copied beside this assembly) as a real child
/// process: pipes, threads and timing are the real ones. The fake engine logs each line it
/// receives, which is how the tests see what EngineBattle wrote.
module EngineCharacterizationTests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Xunit
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.Engine

// ── Harness ─────────────────────────────────────────────────────────────────────────────────────

let private fakeExeName = if OperatingSystem.IsWindows() then "FakeUciEngine.exe" else "FakeUciEngine"
let private fakePath = Path.Combine(AppContext.BaseDirectory, fakeExeName)

let private newLogPath () =
    Path.Combine(Path.GetTempPath(), sprintf "fakeuci_%s.log" (Guid.NewGuid().ToString("N")))

/// Everything the fake engine received, including its "#args" header and any "#sync" markers.
let private sent (logPath: string) =
    if not (File.Exists logPath) then [||]
    else
        use fs = new FileStream(logPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
        use sr = new StreamReader(fs)
        sr.ReadToEnd().Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun l -> l.TrimEnd('\r'))

let private argsLine logPath = sent logPath |> Array.tryFind (fun l -> l.StartsWith "#args") |> Option.defaultValue ""
let private commands logPath =
    sent logPath |> Array.filter (fun l -> not (l.StartsWith "#") && not (l.StartsWith "option Sync="))

let private waitUntil (timeoutMs: int) (cond: unit -> bool) =
    let sw = Stopwatch.StartNew()
    while not (cond ()) && sw.ElapsedMilliseconds < int64 timeoutMs do
        Thread.Sleep 10
    cond ()

/// Waits until the fake engine has received `line` (or the timeout passes).
let private waitForSent logPath (line: string) =
    waitUntil 5000 (fun () -> commands logPath |> Array.contains line) |> ignore

/// What the engine has received so far, read only once it has received everything sent before
/// this call: a unique "#sync" line goes through the same pipe and is waited for. The tournament
/// engine writes without waiting for an answer, so a plain read races the pipe - it passed on
/// Windows and failed on Linux. The fake engine ignores the marker; `commands` drops it.
let private synced logPath (write: string -> unit) =
    let marker = "#sync " + Guid.NewGuid().ToString("N")
    write marker
    if not (waitUntil 5000 (fun () -> sent logPath |> Array.contains marker)) then
        failwithf "the fake engine never received %s" marker
    commands logPath

/// The Winboard form of `synced`: "#sync" has no xboard translation and would never be sent, so
/// the marker travels as a setoption, which the handler turns into "option Sync=<id>".
let private syncedWb logPath (write: string -> unit) =
    let id = Guid.NewGuid().ToString("N")
    write (sprintf "setoption name Sync value %s" id)
    let marker = "option Sync=" + id
    if not (waitUntil 5000 (fun () -> sent logPath |> Array.contains marker)) then
        failwithf "the fake engine never received %s" marker
    commands logPath

let private config (logPath: string) (extraArgs: string) (options: (string * obj) list) =
    let opts = Dictionary<string, obj>()
    for (k, v) in options do opts.[k] <- v
    { EngineConfig.Empty with
        Name = "Fake"
        Path = fakePath
        Args = (sprintf "--log \"%s\" %s" logPath extraArgs).Trim()
        Options = opts }

let private initCommands (cfg: EngineConfig) = EngineHelper.createInitialUCICommands cfg |> Seq.toList

/// The analysis-page engine, with its updates collected.
let private startAnalysis (cfg: EngineConfig) =
    let updates = ConcurrentQueue<EngineUpdate>()
    let eng = new ChessEngineWithUCIProcessing(updates.Enqueue, cfg, initCommands cfg, NullLogger.Instance, false)
    eng, updates

let private startTournament (cfg: EngineConfig) =
    new ChessEngine(cfg, initCommands cfg, Some (NullLogger.Instance :> ILogger))

let private quitAnalysis (eng: ChessEngineWithUCIProcessing) =
    try eng.SendUCICommand UCICommand.Quit with _ -> ()

/// Clean-up only: quit first so StopProcess does not sit out its three-second grace.
let private stopTournament (eng: ChessEngine) =
    try
        if not (eng.HasExited()) then
            eng.Quit()
            eng.StopProcess()
    with _ -> ()

let private statuses (updates: ConcurrentQueue<EngineUpdate>) =
    updates.ToArray() |> Array.choose (function Status s -> Some s | _ -> None)

let private bestMoves (updates: ConcurrentQueue<EngineUpdate>) =
    updates.ToArray() |> Array.choose (function BestMove b -> Some b | _ -> None)

let private dones (updates: ConcurrentQueue<EngineUpdate>) =
    updates.ToArray() |> Array.choose (function Done p -> Some p | _ -> None)

let private hasDone updates = dones updates |> Array.isEmpty |> not

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
/// The form every real caller sends (Board.PositionWithMovesFromGraph): the start position
/// spelled out as a FEN. See the startpos quirk test for why it matters.
let private fromStart (moves: string) =
    if moves = "" then "position fen " + startFen else sprintf "position fen %s moves %s" startFen moves

// ── ChessEngineWithUCIProcessing: start-up ──────────────────────────────────────────────────────

[<Fact>]
let ``Analysis engine start-up sends uci, the config options, MoveOverheadMs 0, ucinewgame and isready`` () =
    let log = newLogPath ()
    let cfg = config log "" [ "Threads", box 2; "Ponder", box true ]
    let eng, updates = startAnalysis cfg
    try
        Assert.Equal<string[]>(
            [| "uci"
               "setoption name Threads value 2"
               "setoption name Ponder value true"
               "setoption name MoveOverheadMs value 0"
               "ucinewgame"
               "isready" |],
            (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))))
        // Ready is reported once, at the end of start-up; the fake engine has no LogLiveStats.
        let ready = updates.ToArray() |> Array.choose (function Ready (p, live) -> Some (p, live) | _ -> None)
        Assert.Equal<(string * bool)[]>([| ("Fake", false) |], ready)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis engine reports HasLiveStat when the engine advertises LogLiveStats`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "--live-stats" [])
    try
        let ready = updates.ToArray() |> Array.choose (function Ready (_, live) -> Some live | _ -> None)
        Assert.Equal<bool[]>([| true |], ready)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis engine writes an option the engine does not have, but keeps it out of its option dictionaries`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [ "NoSuchOption", box 5; "Threads", box 3 ])
    try
        Assert.Contains("setoption name NoSuchOption value 5", (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))))
        Assert.False(eng.GetAllDefaultOptions().ContainsKey "NoSuchOption")
        Assert.Equal("3", string (eng.GetAllDefaultOptions().["Threads"]))
        Assert.True(eng.GetNoneDefaultSetOptions().ContainsKey "Threads")
        // Options never set keep the engine's default.
        Assert.Equal("16", string (eng.GetAllDefaultOptions().["Hash"]))
    finally quitAnalysis eng

[<Fact>]
let ``Analysis engine exposes the engine's options and its id name`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        let opts = eng.GetUCICommands()
        Assert.True(opts.ContainsKey "hash")        // case-insensitive
        Assert.True(opts.ContainsKey "Clear Hash")
        Assert.Equal("FakeUciEngine 1.0", eng.UciIdName)
        Assert.Equal("Fake", eng.Name)
        Assert.False(eng.IsLc0)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis engine takes the network name from WeightsFile without its last extension`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [ "WeightsFile", box "C:/nets/my-net.pb.gz" ])
    try
        // A quirk worth knowing: only ".gz" goes, so a .pb.gz net is called "my-net.pb".
        Assert.Equal("my-net.pb", eng.Network)
        Assert.Equal("Fake with net: my-net.pb", eng.FullName)
    finally quitAnalysis eng

// ── ChessEngineWithUCIProcessing: search output ─────────────────────────────────────────────────

[<Fact>]
let ``Analysis search reports each info line as Status and Info, then Done before BestMove`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart "e2e4"))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> bestMoves updates |> Array.isEmpty |> not))
        let st = statuses updates
        Assert.Equal<int[]>([| 1; 2; 3 |], st |> Array.map (fun s -> s.Depth))
        // Black to move: the engine's cp is turned to White's point of view.
        Assert.Equal<MiscTypes.EvalType[]>(
            [| MiscTypes.EvalType.CP -0.20; MiscTypes.EvalType.CP -0.21; MiscTypes.EvalType.CP -0.22 |],
            st |> Array.map (fun s -> s.Eval))
        Assert.All(st, fun s -> Assert.Equal("Fake", s.PlayerName))
        Assert.Equal(3000L, st.[2].Nodes)
        Assert.Equal(150000.0, st.[2].NPS)
        Assert.Equal(1, st.[2].MultiPV)
        Assert.Equal("e7e5 g1f3 b8c6", st.[2].PVLongSAN)
        Assert.Equal("1.... e5 2.Nf3 Nc6", st.[2].PV)
        Assert.Equal(WDLType.HasValue { Win = 400.0; Draw = 450.0; Loss = 150.0 }, st.[2].WDL)
        // The raw line travels alongside each parsed status.
        let infos = updates.ToArray() |> Array.choose (function Info (_, l) -> Some l | _ -> None)
        Assert.Equal(3, infos.Length)
        Assert.StartsWith("info depth 3 seldepth 5 score cp 22", infos.[2])
        // Done comes before BestMove.
        let order = updates.ToArray() |> Array.choose (function Done _ -> Some "done" | BestMove _ -> Some "best" | _ -> None)
        Assert.Equal<string[]>([| "done"; "best" |], order)
        let bm = (bestMoves updates).[0]
        Assert.Equal("e7e5", bm.Move)
        Assert.Equal("g1f3", bm.Ponder)
        Assert.Equal("e5", bm.MoveAndFen.ShortSan)
        // A quirk: FEN and MoveAndFen.FenAfterMove hold the position BEFORE the move - the board is
        // read without playing it - despite the field's name.
        Assert.Equal("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", bm.FEN)
        Assert.Equal(bm.FEN, bm.MoveAndFen.FenAfterMove)
        Assert.Equal(MiscTypes.EvalType.CP -0.22, bm.Eval)
        Assert.Equal(3000L, bm.Nodes)
        Assert.Equal("1.... e5 2.Nf3 Nc6", bm.PV)
        Assert.Equal(32, bm.PiecesLeft)
        Assert.Contains("go nodes 100", commands log)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis PositionWithMoves ignores the moves of a startpos command internally`` () =
    // A quirk: the wrapper's own board understands only "position fen ... moves ...". Given
    // "position startpos moves e2e4" it stays at the start position, so the engine's reply to
    // e2e4 is parsed from White's side and bestmove e7e5 is taken for an illegal move - no
    // BestMove is reported. Every real caller sends the fen form, so this never shows today.
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves "position startpos moves e2e4")
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> hasDone updates))
        Thread.Sleep 200
        Assert.Contains("position startpos moves e2e4", commands log)
        Assert.Equal(MiscTypes.EvalType.CP 0.20, (statuses updates).[0].Eval)
        Assert.Equal("1.Nf3 Nc6", (statuses updates).[0].PV)
        Assert.Empty(bestMoves updates)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis search keeps the last full PV when a bound line arrives`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [ "FakeBoundLine", box true ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> hasDone updates))
        let st = statuses updates
        Assert.Equal(4, st.Length)
        let bound = st.[3]
        Assert.Equal(4, bound.Depth)
        Assert.Equal(MiscTypes.EvalType.CP 0.99, bound.Eval)
        // The bound line's own PV is cut to the root move; the published PV is the previous one.
        Assert.Equal(st.[2].PV, bound.PV)
        Assert.Equal("e2e4 e7e5 g1f3", bound.PVLongSAN)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis search with MultiPV reports a status per line`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [ "MultiPV", box 2; "FakeInfoCount", box 1 ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> hasDone updates))
        Assert.Equal<int[]>([| 1; 2 |], statuses updates |> Array.map (fun s -> s.MultiPV))
    finally quitAnalysis eng

[<Fact>]
let ``Analysis bestmove without any pv line falls back to the numbered move`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [ "FakeNoPv", box true ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> bestMoves updates |> Array.isEmpty |> not))
        Assert.Equal("1.e4", (bestMoves updates).[0].PV)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis bestmove (none) reports Done and no BestMove`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [ "FakeBestMoveNone", box true ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> hasDone updates))
        Thread.Sleep 200
        Assert.Empty(bestMoves updates)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis move stats become one NNSeq when the node line arrives`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (config log "" [ "FakeMoveStats", box true; "FakeInfoCount", box 1 ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 100)
        Assert.True(waitUntil 5000 (fun () -> hasDone updates))
        let seqs = updates.ToArray() |> Array.choose (function NNSeq l -> Some (List.ofSeq l) | _ -> None)
        Assert.Equal(1, seqs.Length)
        let moves = seqs.[0]
        // The closing "node" line is part of the sequence too, as a pseudo-move named "node".
        Assert.Equal<string list>([ "e2e4"; "d2d4"; "node" ], moves |> List.map (fun n -> n.LANMove))
        Assert.Equal<string list>([ "e4"; "d4" ], moves |> List.truncate 2 |> List.map (fun n -> n.SANMove))
        Assert.Equal(900L, moves.[0].Nodes)
        Assert.Equal(61.0, moves.[0].P)
    finally quitAnalysis eng

// ── ChessEngineWithUCIProcessing: commands ──────────────────────────────────────────────────────

[<Fact>]
let ``Analysis commands are written in UCI form`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [ "FakeInfinite", box true ])
    try
        let before = (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.length
        eng.SendUCICommand(UCICommand.PositionWithMoves "position startpos moves e2e4")
        eng.SetSearchMoves [ "e7e5"; "c7c5" ]
        eng.SendUCICommand(UCICommand.GoNodes 5)
        eng.SendUCICommand UCICommand.Stop
        eng.ClearSearchMoves()
        eng.SendUCICommand(UCICommand.GoMoveTime 250)
        eng.SendUCICommand UCICommand.Stop
        eng.SendUCICommand UCICommand.GoInfinite
        eng.SendUCICommand UCICommand.Stop
        eng.SendUCICommand(UCICommand.GoTimeControl (UnionType.WithIncrement (TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 1.0), TimeSpan.FromSeconds 30.0, TimeSpan.FromSeconds 20.0))
        eng.SendUCICommand UCICommand.Stop
        eng.SendUCICommand(UCICommand.GoTimeControl (UnionType.FixedTime (TimeSpan.FromSeconds 60.0), TimeSpan.FromSeconds 30.0, TimeSpan.FromSeconds 20.0))
        eng.SendUCICommand UCICommand.Stop
        eng.SendUCICommand UCICommand.UciNewGame
        eng.SendUCICommand(UCICommand.RawCommand "debug off")
        eng.SendUCICommand(UCICommand.SetOption (EngineOption.Create "Hash" "32"))
        eng.SendUCICommand(UCICommand.SetOptions [ EngineOption.Create "Threads" "2"; EngineOption.Create "Style" "Risky" ])
        waitForSent log "setoption name Style value Risky"
        Assert.Equal<string[]>(
            [| "position startpos moves e2e4"
               "go nodes 5 searchmoves e7e5 c7c5"
               "stop"
               "go movetime 250"
               "stop"
               "go infinite"
               "stop"
               "go wtime 30000 btime 20000 winc 1000 binc 1000"
               "stop"
               "go wtime 30000 btime 20000"
               "stop"
               "ucinewgame"
               "debug off"
               "setoption name Hash value 32"
               "setoption name Threads value 2"
               "setoption name Style value Risky" |],
            (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.skip before)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis position commands switch UCI_Chess960 on for an FRC position and off again`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        let before = (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.length
        let frc = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
        eng.SendUCICommand(UCICommand.Position frc)
        eng.SendUCICommand(UCICommand.Position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        // A 4-field FEN gets its counters on the way to the engine.
        eng.SendUCICommand(UCICommand.Position "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3")
        waitForSent log "position fen rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        Assert.Equal<string[]>(
            [| "setoption name UCI_Chess960 value true"
               "position fen " + frc
               "setoption name UCI_Chess960 value false"
               "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
               "position fen rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" |],
            (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.skip before)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis SetMoveOverhead sends only a value inside the option's range`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        let before = (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.length
        eng.SendUCICommand(UCICommand.SetMoveOverhead ("MoveOverheadMs", 50))
        eng.SendUCICommand(UCICommand.SetMoveOverhead ("MoveOverheadMs", 99999))
        eng.SendUCICommand(UCICommand.SetMoveOverhead ("NoSuchOption", 10))
        eng.SendUCICommand(UCICommand.RawCommand "marker")
        waitForSent log "marker"
        Assert.Equal<string[]>([| "setoption name MoveOverheadMs value 50"; "marker" |], (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.skip before)
    finally quitAnalysis eng

[<Fact>]
let ``Analysis SetAllOptions writes booleans in lower case`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        let before = (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.length
        let d = Dictionary<string, obj>()
        d.["Ponder"] <- box true
        d.["Hash"] <- box 32
        eng.SetAllOptions d
        waitForSent log "setoption name Hash value 32"
        Assert.Equal<string[]>([| "setoption name Ponder value true"; "setoption name Hash value 32" |], (synced log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))) |> Array.skip before)
        Assert.Equal("32", string (eng.GetAllDefaultOptions().["Hash"]))
    finally quitAnalysis eng

[<Fact>]
let ``Analysis CurrentPositionCommand reflects the last position sent`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart "e2e4 e7e5"))
        Assert.Equal(fromStart "e2e4 e7e5", eng.CurrentPositionCommand())
    finally quitAnalysis eng

// ── ChessEngineWithUCIProcessing: readiness, stderr, exit ───────────────────────────────────────

[<Fact>]
let ``Analysis WaitForReadyOk is true for a ready engine and false at once for a fatal line or an exit`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    try
        Assert.True(eng.WaitForReadyOk())
        eng.SendUCICommand(UCICommand.SetOption (EngineOption.Create "FakeFatalOnReady" "true"))
        let sw = Stopwatch.StartNew()
        Assert.False(eng.WaitForReadyOk())
        Assert.True(sw.ElapsedMilliseconds < 5000L)
        eng.SendUCICommand(UCICommand.SetOption (EngineOption.Create "FakeFatalOnReady" "false"))
        eng.SendUCICommand(UCICommand.SetOption (EngineOption.Create "FakeExitOnReady" "true"))
        sw.Restart()
        Assert.False(eng.WaitForReadyOk())
        Assert.True(sw.ElapsedMilliseconds < 5000L)
        Assert.True(waitUntil 5000 (fun () -> eng.HasExited))
    finally quitAnalysis eng

[<Fact>]
let ``Analysis keeps stderr without colour codes and reports it in the diagnostics`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "--ansi" [ "FakeStderrOnReady", box 3 ])
    try
        Assert.True(waitUntil 5000 (fun () -> Seq.length eng.ErrorOutput = 3))
        Assert.Equal<string[]>([| "stderr line 1"; "stderr line 2"; "stderr line 3" |], eng.ErrorOutput |> Seq.toArray)
        Assert.StartsWith("Engine: Fake | ExitCode: N/A | Stderr lines: 3", eng.GetDiagnostics())
    finally quitAnalysis eng

[<Fact>]
let ``Analysis stderr buffer keeps the newest lines and counts the dropped ones`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [ "FakeStderrOnReady", box 700 ])
    try
        // Trimmed in blocks: at 601 lines it drops back to 500, then grows again.
        Assert.True(waitUntil 10000 (fun () -> eng.GetDiagnostics().Contains "stderr line 700"))
        Assert.Equal(599, Seq.length eng.ErrorOutput)
        Assert.Equal("stderr line 102", Seq.head eng.ErrorOutput)
        Assert.Contains("(101 older lines dropped)", eng.GetDiagnostics())
    finally quitAnalysis eng

[<Fact>]
let ``Analysis Quit ends the engine, and kills one that ignores quit`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [])
    let quick = Stopwatch.StartNew()
    eng.SendUCICommand UCICommand.Quit
    Assert.Contains("quit", commands log)
    // An engine that honours quit is gone well inside the one-second grace. The process object is
    // disposed right after, so LastExitCode is usually never set (the Exited event is lost).
    Assert.True(quick.ElapsedMilliseconds < 900L)

    let log2 = newLogPath ()
    let stubborn, _ = startAnalysis (config log2 "" [ "FakeIgnoreQuit", box true ])
    let sw = Stopwatch.StartNew()
    stubborn.SendUCICommand UCICommand.Quit
    // Waits one second for the exit, then kills.
    Assert.True(sw.ElapsedMilliseconds >= 900L)
    Assert.Contains("quit", commands log2)

[<Fact>]
let ``Analysis records the exit code of an engine that dies`` () =
    let log = newLogPath ()
    let eng, _ = startAnalysis (config log "" [ "FakeCrashOnGo", box true ])
    try
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand(UCICommand.GoNodes 1)
        Assert.True(waitUntil 5000 (fun () -> eng.LastExitCode = Some 3))
        Assert.True(eng.HasExited)
        Assert.Contains("fake crash in search", eng.ErrorOutput)
    finally quitAnalysis eng

// ── ChessEngine (tournament): start-up ──────────────────────────────────────────────────────────

[<Fact>]
let ``Tournament engine start-up sends uci and the options in the engine's own spelling, and nothing else`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "threads", box 2; "hash", box 64 ])
    try
        Assert.True(eng.PassedValidation)
        Assert.Equal<string[]>([| "uci"; "setoption name Threads value 2"; "setoption name Hash value 64" |], (synced log eng.Write))
        Assert.Equal<string[]>([| "setoption name Threads value 2"; "setoption name Hash value 64" |], eng.Commands.ToArray())
        Assert.Equal<string list>([ "setoption name Threads value 2"; "setoption name Hash value 64" ], eng.GetVerifiedCommands())
        Assert.Equal("FakeUciEngine 1.0", eng.UciIdName)
        Assert.True(eng.CanReuseWinboard)
        Assert.False(eng.HasExited())
    finally stopTournament eng

[<Fact>]
let ``Tournament engine fails validation for an out-of-range value or an unknown option, and still sends them`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "Threads", box 999 ])
    try
        Assert.False(eng.PassedValidation)
        Assert.Contains("setoption name Threads value 999", (synced log eng.Write))
    finally stopTournament eng
    let log2 = newLogPath ()
    let eng2 = startTournament (config log2 "" [ "NoSuchOption", box 1 ])
    try
        Assert.False(eng2.PassedValidation)
        Assert.Contains("setoption name NoSuchOption value 1", (synced log2 eng2.Write))
    finally stopTournament eng2

[<Fact>]
let ``Tournament engine created without validation still validates at creation`` () =
    // A quirk: createEngineWithoutValidation turns validation off AFTER the constructor has run
    // it, so an invalid option still fails. Pinned here so the rewrite decides it on purpose.
    let log = newLogPath ()
    let eng = EngineHelper.createEngineWithoutValidation (config log "" [ "Threads", box 999 ], None)
    try Assert.False(eng.PassedValidation)
    finally stopTournament eng

[<Fact>]
let ``Tournament engine that is not a UCI engine fails at creation and leaves no process`` () =
    let log = newLogPath ()
    let ex = Assert.ThrowsAny<exn>(fun () -> startTournament (config log "--exit-on-uci" []) |> ignore)
    Assert.Contains("did not respond to the uci command", ex.Message)

[<Fact>]
let ``Tournament engine takes the network name from WeightsFile or Network`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "WeightsFile", box "C:/nets/bt4.pb.gz" ])
    try
        Assert.Equal("bt4.pb", eng.Network)
        Assert.Equal("Fake with net: bt4.pb", eng.FullName)
    finally stopTournament eng

// ── ChessEngine: readiness, warm-up, new game ───────────────────────────────────────────────────

[<Fact>]
let ``Tournament WaitForReadyOk is true for a ready engine and fails at once for a fatal line or an exit`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        Assert.True(eng.WaitForReadyOk())
        Assert.Equal("", eng.ReadyFailure)
        eng.AddSetOption(EngineOption.Create "FakeFatalOnReady" "true")
        let sw = Stopwatch.StartNew()
        Assert.False(eng.WaitForReadyOk())
        Assert.True(sw.ElapsedMilliseconds < 5000L)
        Assert.Equal("reported a fatal initialization error: info string Cannot initialize engine: fake failure", eng.ReadyFailure)
    finally stopTournament eng
    let log2 = newLogPath ()
    let eng2 = startTournament (config log2 "" [ "FakeExitOnReady", box true ])
    try
        let sw = Stopwatch.StartNew()
        Assert.False(eng2.WaitForReadyOk())
        Assert.True(sw.ElapsedMilliseconds < 5000L)
        // The exit is named, with its code, even when stdout closes before the process is seen to
        // exit (the rewrite waits for the exit instead of reporting "output closed").
        Assert.Equal("exited (code 2) while waiting for readyok", eng2.ReadyFailure)
    finally stopTournament eng2

[<Fact>]
let ``Tournament PrepareNewGame warms up once per process, then sends ucinewgame and isready`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = (synced log eng.Write) |> Array.length
        Assert.True(eng.PrepareNewGame())
        Assert.True(eng.PrepareNewGame())
        Assert.Equal<string[]>(
            [| "position startpos"; "go nodes 1"; "ucinewgame"; "isready"; "ucinewgame"; "isready" |],
            (synced log eng.Write) |> Array.skip before)
        // A restarted engine is a new process and warms up again.
        eng.StopProcess()
        eng.StartProcess()
        let restartedAt = (synced log eng.Write) |> Array.length
        Assert.True(eng.PrepareNewGame())
        let after = (synced log eng.Write) |> Array.skip restartedAt
        Assert.Equal<string[]>([| "position startpos"; "go nodes 1"; "ucinewgame"; "isready" |], after)
    finally stopTournament eng

[<Fact>]
let ``Tournament WarmUp that gets no bestmove stops the search and drains its bestmove`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "FakeInfinite", box true ])
    try
        let before = (synced log eng.Write) |> Array.length
        Assert.False(eng.WarmUp 300)
        Assert.Equal<string[]>([| "position startpos"; "go nodes 1"; "stop" |], (synced log eng.Write) |> Array.skip before)
        // A timeout is not a fatal failure: ReadyFailure stays empty and the next isready works.
        Assert.Equal("", eng.ReadyFailure)
        Assert.True(eng.WaitForReadyOk())
    finally stopTournament eng

[<Fact>]
let ``Tournament WarmUp on an engine that dies reports the exit`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "FakeCrashOnGo", box true ])
    try
        Assert.False(eng.WarmUp 5000)
        // Always a reason, with the exit code. Before the rewrite, a closed stdout seen ahead of
        // the exit counted as a timeout and left ReadyFailure empty (seen on Linux).
        Assert.Equal("exited (code 3) during the warm-up search", eng.ReadyFailure)
        // Either way the next game cannot start.
        Assert.False(eng.PrepareNewGame())
    finally stopTournament eng

// ── ChessEngine: the async forms ────────────────────────────────────────────────────────────────

/// A context that swallows whatever is posted to it: a continuation sent here never runs, as one
/// sent to a Blazor dispatcher blocked in a synchronous call never would.
type private SwallowingContext() =
    inherit SynchronizationContext()
    override _.Post(_, _) = ()
    override _.Send(_, _) = ()

[<Fact>]
let ``Tournament sync readiness members do not need the caller's context to finish`` () =
    let log = newLogPath ()
    // The delay makes every readyok wait really asynchronous, so a continuation that wanted the
    // caller's context would be posted to it and lost.
    let eng = startTournament (config log "" [ "FakeReadyDelayMs", box 200 ])
    try
        let mutable results = [||]
        let worker =
            Thread(fun () ->
                SynchronizationContext.SetSynchronizationContext(SwallowingContext())
                results <- [| eng.PrepareNewGame(); eng.WaitForReadyOk(); eng.WarmUp 1000 |])
        worker.IsBackground <- true
        worker.Start()
        Assert.True(worker.Join 20000, "a synchronous member hung waiting for its caller's context")
        Assert.Equal<bool[]>([| true; true; true |], results)
    finally stopTournament eng

[<Fact>]
let ``Tournament WaitForReadyOkAsync ends on cancellation, records no failure, and the engine is still usable`` () =
    let log = newLogPath ()
    // Silent after isready: only the token can end the wait.
    let eng = startTournament (config log "" [ "FakeNoReadyOk", box true ])
    try
        use cts = new CancellationTokenSource(300)
        let sw = Stopwatch.StartNew()
        let wait = eng.WaitForReadyOkAsync(600000, cts.Token)
        let ex = Record.Exception(fun () -> wait.GetAwaiter().GetResult() |> ignore)
        Assert.IsAssignableFrom<OperationCanceledException>(ex) |> ignore
        Assert.True(sw.ElapsedMilliseconds < 5000L, sprintf "took %d ms" sw.ElapsedMilliseconds)
        Assert.Equal("", eng.ReadyFailure)
        eng.AddSetOption(EngineOption.Create "FakeNoReadyOk" "false")
        Assert.True(eng.WaitForReadyOk())
    finally stopTournament eng

[<Fact>]
let ``Tournament WarmUpAsync on cancellation stops the search, and the next isready drains it`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "FakeInfinite", box true ])
    try
        let before = (synced log eng.Write) |> Array.length
        use cts = new CancellationTokenSource(300)
        let ex = Record.Exception(fun () -> eng.WarmUpAsync(600000, cts.Token).GetAwaiter().GetResult() |> ignore)
        Assert.IsAssignableFrom<OperationCanceledException>(ex) |> ignore
        Assert.Equal<string[]>([| "position startpos"; "go nodes 1"; "stop" |], (synced log eng.Write) |> Array.skip before)
        Assert.Equal("", eng.ReadyFailure)
        // The stopped search's bestmove is skipped on the way to readyok.
        Assert.True(eng.WaitForReadyOk())
    finally stopTournament eng

[<Fact>]
let ``Tournament PrepareNewGameAsync sends what PrepareNewGame sends`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = (synced log eng.Write) |> Array.length
        Assert.True(eng.PrepareNewGameAsync().GetAwaiter().GetResult())
        Assert.True(eng.PrepareNewGameAsync(60000, CancellationToken.None).GetAwaiter().GetResult())
        Assert.Equal<string[]>(
            [| "position startpos"; "go nodes 1"; "ucinewgame"; "isready"; "ucinewgame"; "isready" |],
            (synced log eng.Write) |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``The async factories return at once and hand over a started engine when it is ready`` () =
    // 1.5 s to readyok: the analysis constructor waits for it, so a caller that made the engine
    // itself would be held that long.
    let log = newLogPath ()
    let cfg = config log "" [ "FakeReadyDelayMs", box 1500 ]
    let updates = ConcurrentQueue<EngineUpdate>()
    let sw = Stopwatch.StartNew()
    let pending = EngineHelper.createAltEngineAsync(updates.Enqueue, cfg, NullLogger.Instance, false)
    let returnedAfter = sw.ElapsedMilliseconds
    Assert.True(returnedAfter < 500L, sprintf "the call held its caller for %d ms" returnedAfter)
    Assert.False(pending.IsCompleted)
    let eng = pending.GetAwaiter().GetResult()
    try
        Assert.True(sw.ElapsedMilliseconds >= 1400L)
        Assert.True(eng.WaitForReadyOk(10000))
    finally quitAnalysis eng
    let log2 = newLogPath ()
    let tournament = EngineHelper.createEngineAsync(config log2 "" [], Some (NullLogger.Instance :> ILogger)).GetAwaiter().GetResult()
    try Assert.True(tournament.WaitForReadyOk())
    finally stopTournament tournament

// ── ChessEngine: options ────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``Tournament SetMoveOverhead is sent once per process and not at all when the def sets it`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = (synced log eng.Write) |> Array.length
        eng.SetMoveOverhead("MoveOverheadMs", 50)
        eng.SetMoveOverhead("MoveOverheadMs", 50)
        eng.SetMoveOverhead("MoveOverheadMs", 60)
        eng.SetMoveOverhead("moveoverhead", 99999)   // out of range
        eng.Write "marker"
        waitForSent log "marker"
        Assert.Equal<string[]>(
            [| "setoption name MoveOverheadMs value 50"; "setoption name MoveOverheadMs value 60"; "marker" |],
            (synced log eng.Write) |> Array.skip before)
        eng.StopProcess()
        eng.StartProcess()
        let restartedAt = (synced log eng.Write) |> Array.length
        eng.SetMoveOverhead("MoveOverheadMs", 60)
        eng.Write "marker2"
        waitForSent log "marker2"
        Assert.Equal<string[]>([| "setoption name MoveOverheadMs value 60"; "marker2" |], (synced log eng.Write) |> Array.skip restartedAt)
    finally stopTournament eng
    let log2 = newLogPath ()
    let eng2 = startTournament (config log2 "" [ "MoveOverheadMs", box 30 ])
    try
        let before = (synced log2 eng2.Write) |> Array.length
        eng2.SetMoveOverhead("MoveOverheadMs", 50)
        eng2.Write "marker"
        waitForSent log2 "marker"
        Assert.Equal<string[]>([| "marker" |], (synced log2 eng2.Write) |> Array.skip before)
    finally stopTournament eng2

[<Fact>]
let ``Tournament AddSetOption stops first, uses the engine's spelling and updates the config`` () =
    let log = newLogPath ()
    let cfg = config log "" []
    let eng = startTournament cfg
    try
        let before = (synced log eng.Write) |> Array.length
        eng.AddSetOption(EngineOption.Create "hash" "32")
        eng.AddSetOption(EngineOption.Create "hash" "32")
        eng.AddSetOption(EngineOption.Create "NoSuchOption" "1")
        eng.Write "marker"
        waitForSent log "marker"
        Assert.Equal<string[]>(
            [| "stop"; "setoption name Hash value 32"; "stop"; "setoption name Hash value 32"; "marker" |],
            (synced log eng.Write) |> Array.skip before)
        Assert.Equal(box "32", cfg.Options.["hash"])
        // Commands keeps one copy of a repeated setoption.
        Assert.Equal(1, eng.Commands |> Seq.filter ((=) "setoption name Hash value 32") |> Seq.length)
    finally stopTournament eng

[<Fact>]
let ``Tournament TryToUpdateOption picks the last option whose name contains the text`` () =
    // A quirk: "hash" matches both Hash and "Clear Hash", and the later one in the engine's list
    // wins. Pinned so the rewrite changes it deliberately if at all.
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = (synced log eng.Write) |> Array.length
        eng.TryToUpdateOption "hash" "64"
        eng.Write "marker"
        waitForSent log "marker"
        Assert.Equal<string[]>([| "stop"; "setoption name Clear Hash value 64"; "marker" |], (synced log eng.Write) |> Array.skip before)
    finally stopTournament eng

// ── ChessEngine: commands and reading ───────────────────────────────────────────────────────────

[<Fact>]
let ``Tournament commands are written in UCI form`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [ "FakeInfinite", box true ])
    try
        let before = (synced log eng.Write) |> Array.length
        eng.UciNewGame()
        eng.Position "position startpos moves e2e4"
        eng.PositionGoFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3"
        eng.Go 100
        eng.Stop()
        eng.Go(UnionType.WithIncrement (TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 2.0), TimeSpan.FromSeconds 50.0, TimeSpan.FromSeconds 40.0)
        eng.Stop()
        eng.GoNodes 1000
        eng.Stop()
        eng.GoValue()
        eng.Stop()
        eng.GoPonder "go ponder wtime 1000 btime 1000"
        eng.PonderHit()
        eng.Stop()
        eng.IsReady()
        eng.Uci()
        Assert.Equal<string[]>(
            [| "ucinewgame"
               "position startpos moves e2e4"
               "position fen rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
               "go movetime 100"
               "stop"
               "go wtime 50000 btime 40000 winc 2000 binc 2000"
               "stop"
               "go nodes 1000"
               "stop"
               "go value"
               "stop"
               "go ponder wtime 1000 btime 1000"
               "ponderhit"
               "stop"
               "isready"
               "uci" |],
            (synced log eng.Write) |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``Tournament ReadLineAsyncWithTimeout returns lines and null when cancelled`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        eng.IsReady()
        use cts = new CancellationTokenSource(5000)
        let line = eng.ReadLineAsyncWithTimeout(cts.Token).Result
        Assert.Equal("readyok", line)
        use quick = new CancellationTokenSource(100)
        Assert.Null(eng.ReadLineAsyncWithTimeout(quick.Token).Result)
    finally stopTournament eng

[<Fact>]
let ``Tournament StopProcess ends the engine and HasExited turns true`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    eng.StopProcess()
    Assert.True(eng.HasExited())
    // StopProcess waits for the engine to leave on its own (up to 3 s) and kills it after;
    // it does not send quit.
    Assert.DoesNotContain("quit", commands log)

// ── Lc0 detection by path ───────────────────────────────────────────────────────────────────────

/// A copy of the fake engine inside a folder whose path contains "lc0", which is how both
/// classes decide they are talking to Lc0.
let private lc0Copy () =
    let dir = Path.Combine(Path.GetTempPath(), sprintf "lc0-fake-%s" (Guid.NewGuid().ToString("N")))
    Directory.CreateDirectory dir |> ignore
    for f in Directory.GetFiles(AppContext.BaseDirectory, "FakeUciEngine*") do
        File.Copy(f, Path.Combine(dir, Path.GetFileName f))
    Path.Combine(dir, fakeExeName)

[<Fact>]
let ``Lc0 by path gets --show-hidden: appended by the tournament engine, only without args by the analysis engine`` () =
    let path = lc0Copy ()
    let log = newLogPath ()
    let tour = startTournament { config log "" [] with Path = path }
    try
        Assert.True(tour.IsLc0)
        Assert.EndsWith("--show-hidden", argsLine log)
    finally stopTournament tour

    let log2 = newLogPath ()
    let ana, _ = startAnalysis { config log2 "" [] with Path = path }
    try
        Assert.True(ana.IsLc0)
        Assert.DoesNotContain("--show-hidden", argsLine log2)
    finally quitAnalysis ana

    // With no args at all the analysis engine adds it too. The log path goes by environment.
    let log3 = newLogPath ()
    Environment.SetEnvironmentVariable("FAKEUCI_LOG", log3)
    try
        let ana2, _ = startAnalysis { config log3 "" [] with Path = path; Args = "" }
        try Assert.Equal("#args --show-hidden", argsLine log3)
        finally quitAnalysis ana2
    finally Environment.SetEnvironmentVariable("FAKEUCI_LOG", null)

// ── EngineHelper ────────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``createInitialUCICommands lower-cases booleans and resolves a relative WeightsFile against NetworkPath`` () =
    let opts = Dictionary<string, obj>()
    opts.["Ponder"] <- box "True"
    opts.["Threads"] <- box 4
    opts.["WeightsFile"] <- box "net.pb.gz"
    let cfg = { EngineConfig.Empty with Path = fakePath; NetworkPath = "C:/nets"; Options = opts }
    Assert.Equal<string list>(
        [ "setoption name Ponder value true"
          "setoption name Threads value 4"
          "setoption name WeightsFile value " + Path.Combine("C:/nets", "net.pb.gz") ],
        EngineHelper.createInitialUCICommands cfg |> Seq.toList)

[<Fact>]
let ``createEngine refuses a config whose engine path does not exist`` () =
    let cfg = { EngineConfig.Empty with Name = "Missing"; Path = Path.Combine(Path.GetTempPath(), "no-such-engine.exe") }
    let ex = Assert.ThrowsAny<exn>(fun () -> EngineHelper.createEngine (cfg, None) |> ignore)
    Assert.Equal("Engine could not be created", ex.Message)

[<Fact>]
let ``initEngine waits for readyok, then warms up and starts a new game`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = (synced log eng.Write) |> Array.length
        EngineHelper.initEngine 0 eng
        Assert.Equal<string[]>(
            [| "isready"; "position startpos"; "go nodes 1"; "ucinewgame"; "isready" |],
            (synced log eng.Write) |> Array.skip before)
    finally stopTournament eng

// ── HardwareInfo ────────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``getThreads reads Threads as int, string or JSON, defaults to 1 and caps at the cores left`` () =
    let cfgWith (v: obj option) =
        let opts = Dictionary<string, obj>()
        v |> Option.iter (fun v -> opts.["Threads"] <- v)
        { EngineConfig.Empty with Options = opts }
    let cap = Environment.ProcessorCount - 1
    Assert.Equal(min 2 cap, HardwareInfo.getThreads (cfgWith (Some (box 2))))
    Assert.Equal(min 3 cap, HardwareInfo.getThreads (cfgWith (Some (box "3"))))
    Assert.Equal(1, HardwareInfo.getThreads (cfgWith (Some (box "x"))))
    Assert.Equal(1, HardwareInfo.getThreads (cfgWith None))
    Assert.Equal(min 4 cap, HardwareInfo.getThreads (cfgWith (Some (box (JsonDocument.Parse("4").RootElement)))))
    Assert.Equal(min 5 cap, HardwareInfo.getThreads (cfgWith (Some (box (JsonDocument.Parse("\"5\"").RootElement)))))
    Assert.Equal(cap, HardwareInfo.getThreads (cfgWith (Some (box 100000))))

// ── Winboard / xboard ───────────────────────────────────────────────────────────────────────────
// Both classes speak Winboard through WinboardHandler: UCI-shaped commands go out translated to
// CECP, and the engine's replies come back translated to UCI-shaped lines. These tests pin what
// reaches a Winboard engine and what the caller reads back.

let private wbConfig (logPath: string) (extraArgs: string) (options: (string * obj) list) (wbc: WinboardConfig option) =
    { config logPath (("--xboard " + extraArgs).Trim()) options with Protocol = "Winboard"; WinboardConfig = wbc }

let private wbDefaults = WinboardConfig.Default

/// Reads translated lines from the tournament engine until one starts with `until` (or 5 s pass).
let private readUntil (eng: ChessEngine) (until: string) =
    let lines = ResizeArray<string>()
    use cts = new CancellationTokenSource(5000)
    let mutable fin = false
    while not fin do
        let l = eng.ReadLineAsyncWithTimeout(cts.Token).Result
        if isNull l then fin <- true
        else
            lines.Add l
            if l.StartsWith until then fin <- true
    lines.ToArray()

[<Fact>]
let ``Winboard tournament start-up negotiates features, then sends post and easy`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] None)
    try
        Assert.Equal<string[]>([| "xboard"; "protover 2"; "post"; "easy" |], syncedWb log eng.Write)
        Assert.True(eng.PassedValidation)
        Assert.True(eng.CanReuseWinboard)
        Assert.Equal("", eng.UciIdName)
    finally stopTournament eng

[<Fact>]
let ``Winboard options are sent as option commands but fail validation`` () =
    // A quirk: a Winboard engine has no UCI option list, so every configured option fails the
    // UCI validation (PassedValidation false) although it is sent, translated.
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [ "Hash", box 64 ] None)
    try
        Assert.Contains("option Hash=64", syncedWb log eng.Write)
        Assert.False(eng.PassedValidation)
    finally stopTournament eng

[<Fact>]
let ``Winboard v1 engine that rejects protover is probed for setboard`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "--wb-v1-error" [] None)
    try
        Assert.Equal<string[]>(
            [| "xboard"; "protover 2"
               "new"; "force"; "setboard rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
               "post"; "easy" |],
            syncedWb log eng.Write)
        // No setboard: positions go as moves after new.
        eng.Position "position startpos moves e2e4"
        Assert.Equal<string[]>([| "new"; "force"; "e2e4" |], syncedWb log eng.Write |> Array.skip 7)
    finally stopTournament eng

[<Fact>]
let ``Winboard ForceV1Mode skips protover`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] (Some { wbDefaults with ForceV1Mode = true }))
    try Assert.Equal<string[]>([| "xboard"; "post"; "easy" |], syncedWb log eng.Write)
    finally stopTournament eng

[<Fact>]
let ``Winboard AutoDetect probes the level command at start-up`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] (Some { wbDefaults with TimeControlStrategy = AutoDetect }))
    try
        Assert.Equal<string[]>(
            [| "xboard"; "protover 2"; "new"; "force"; "level 0 1 0"; "post"; "easy" |],
            syncedWb log eng.Write)
    finally stopTournament eng

[<Fact>]
let ``Winboard reuse=0 is reported by CanReuseWinboard`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "--wb-features \"ping=1 reuse=0 done=1\"" [] None)
    try Assert.False(eng.CanReuseWinboard)
    finally stopTournament eng

[<Fact>]
let ``Winboard WaitForReadyOk pings when it can, and assumes ready when it cannot`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] None)
    try
        Assert.True(eng.WaitForReadyOk())
        let pings = syncedWb log eng.Write |> Array.filter (fun l -> l.StartsWith "ping ")
        Assert.Equal(1, pings.Length)
    finally stopTournament eng
    // A ping that never gets its pong: one second, then ready anyway.
    let log2 = newLogPath ()
    let eng2 = startTournament (wbConfig log2 "--wb-no-pong" [] None)
    try
        let sw = Stopwatch.StartNew()
        Assert.True(eng2.WaitForReadyOk())
        Assert.InRange(sw.ElapsedMilliseconds, 800L, 5000L)
    finally stopTournament eng2
    // No ping feature: ready at once, nothing sent.
    let log3 = newLogPath ()
    let eng3 = startTournament (wbConfig log3 "--wb-features \"setboard=1 done=1\"" [] None)
    try
        let before = syncedWb log3 eng3.Write |> Array.length
        Assert.True(eng3.WaitForReadyOk())
        Assert.Equal(before, syncedWb log3 eng3.Write |> Array.length)
    finally stopTournament eng3

[<Fact>]
let ``Winboard PrepareNewGame, WarmUp and option updates send nothing`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] None)
    try
        let before = syncedWb log eng.Write |> Array.length
        Assert.True(eng.WarmUp 1000)
        Assert.True(eng.PrepareNewGame())
        eng.AddSetOption(EngineOption.Create "Hash" "32")      // no UCI option list: not found
        eng.SetMoveOverhead("MoveOverheadMs", 50)
        Assert.Equal(before, syncedWb log eng.Write |> Array.length)
    finally stopTournament eng

[<Fact>]
let ``Winboard game: new, force + setboard, level + time/otim + go, and move read back as bestmove`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [ "FakeBestMove", box "e7e5" ] None)
    try
        let before = syncedWb log eng.Write |> Array.length
        eng.UciNewGame()
        eng.Position "position startpos moves e2e4"
        eng.Go(UnionType.WithIncrement (TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 2.0), TimeSpan.FromSeconds 50.0, TimeSpan.FromSeconds 40.0)
        let replies = readUntil eng "bestmove"
        Assert.Equal<string[]>(
            [| "new"
               "force"
               "setboard rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
               "level 0 0:50 2"
               "time 4000"
               "otim 5000"
               "go" |],
            syncedWb log eng.Write |> Array.skip before)
        Assert.Equal("bestmove e7e5", Array.last replies)
        // The next position in the same game: no second "new".
        let mid = syncedWb log eng.Write |> Array.length
        eng.Position "position startpos moves e2e4 e7e5"
        eng.Stop()
        Assert.Equal<string[]>(
            [| "force"; "setboard rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"; "?" |],
            syncedWb log eng.Write |> Array.skip mid)
    finally stopTournament eng

[<Fact>]
let ``Winboard thinking output comes back as UCI info with the PV in coordinates`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] None)
    try
        eng.UciNewGame()
        eng.Position "position startpos"
        eng.Go 100
        let replies = readUntil eng "bestmove"
        Assert.Equal<string[]>(
            [| "info depth 1 score cp 21 time 10 nodes 1000 nps 100000 pv e2e4 e7e5 g1f3"
               "info depth 2 score cp 22 time 20 nodes 2000 nps 100000 pv e2e4 e7e5 g1f3"
               "info depth 3 score cp 23 time 30 nodes 3000 nps 100000 pv e2e4 e7e5 g1f3"
               "bestmove e2e4" |],
            replies)
    finally stopTournament eng

[<Fact>]
let ``Winboard go commands: movetime becomes st, nodes has no equivalent`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [ "FakeInfinite", box true ] None)
    try
        let before = syncedWb log eng.Write |> Array.length
        eng.Go 100
        eng.Stop()
        eng.GoNodes 1000
        eng.Stop()
        Assert.Equal<string[]>([| "st 1"; "go"; "?"; "go"; "?" |], syncedWb log eng.Write |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``Winboard engine without setboard gets moves, with usermove when it asked for it`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "--wb-features \"ping=1 usermove=1 done=1\"" [] None)
    try
        let before = syncedWb log eng.Write |> Array.length
        eng.Position "position startpos moves e2e4 e7e5"
        Assert.Equal<string[]>([| "new"; "force"; "usermove e2e4"; "usermove e7e5" |], syncedWb log eng.Write |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``Winboard Use4FieldFen sends setboard without the counters`` () =
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [] (Some { wbDefaults with Use4FieldFen = true }))
    try
        let before = syncedWb log eng.Write |> Array.length
        eng.Position "position startpos moves e2e4"
        Assert.Equal<string[]>(
            [| "new"; "force"; "setboard rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq -" |],
            syncedWb log eng.Write |> Array.skip before)
    finally stopTournament eng

// Winboard through the analysis engine.

[<Fact>]
let ``Winboard analysis start-up: features, post and easy, the options, then new`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (wbConfig log "" [] None)
    try
        Assert.Equal<string[]>(
            [| "xboard"; "protover 2"; "post"; "easy"; "option MoveOverheadMs=0"; "new" |],
            syncedWb log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m)))
        let ready = updates.ToArray() |> Array.choose (function Ready (p, live) -> Some (p, live) | _ -> None)
        Assert.Equal<(string * bool)[]>([| ("Fake", false) |], ready)
        Assert.True(eng.WaitForReadyOk())
    finally quitAnalysis eng

[<Fact>]
let ``Winboard analysis: infinite search is analyze, stop is exit, a timed search reports its move`` () =
    let log = newLogPath ()
    let eng, updates = startAnalysis (wbConfig log "" [] None)
    let sync () = syncedWb log (fun m -> eng.SendUCICommand(UCICommand.RawCommand m))
    try
        let before = sync () |> Array.length
        eng.SendUCICommand(UCICommand.PositionWithMoves (fromStart ""))
        eng.SendUCICommand UCICommand.GoInfinite
        Assert.True(waitUntil 5000 (fun () -> statuses updates |> Array.length >= 3))
        eng.SendUCICommand UCICommand.Stop
        Assert.Equal<string[]>(
            [| "force"; "setboard " + startFen; "easy"; "analyze"; "exit" |],
            sync () |> Array.skip before)
        let st = statuses updates
        Assert.Equal<int[]>([| 1; 2; 3 |], st |> Array.map (fun s -> s.Depth))
        Assert.Equal("1.e4 e5 2.Nf3", st.[2].PV)
        Assert.Empty(bestMoves updates)
        // A timed search: easy + go, and the move comes back as BestMove.
        eng.SendUCICommand(UCICommand.GoNodes 5)
        Assert.True(waitUntil 5000 (fun () -> bestMoves updates |> Array.isEmpty |> not))
        Assert.Equal("e2e4", (bestMoves updates).[0].Move)
    finally quitAnalysis eng

// ── Pondering ───────────────────────────────────────────────────────────────────────────────────
// GameExecution.playWithPondering drives these calls; the hit/miss decision is its own. What
// Engine.fs owns is the traffic: the pondered position, "go ... ponder", then either "ponderhit"
// (hit) or "stop" (miss), and the lines the caller reads back in between.

let private ponderGo = "go wtime 60000 btime 60000 winc 1000 binc 1000 ponder"

/// Lines readable within `ms` - the caller's view of what the engine sent meanwhile.
let private readFor (eng: ChessEngine) (ms: int) =
    let lines = ResizeArray<string>()
    use cts = new CancellationTokenSource(ms)
    let mutable fin = false
    while not fin do
        let l = eng.ReadLineAsyncWithTimeout(cts.Token).Result
        if isNull l then fin <- true else lines.Add l
    lines.ToArray()

[<Fact>]
let ``Ponder hit: no bestmove before ponderhit, then the search answers`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = synced log eng.Write |> Array.length
        // White pondered on 1...e5 after its 1.e4: the position with the expected reply played.
        eng.Position "position startpos moves e2e4 e7e5"
        eng.GoPonder ponderGo
        let whilePondering = readFor eng 400
        Assert.Equal(3, whilePondering |> Array.filter (fun l -> l.StartsWith "info depth") |> Array.length)
        Assert.DoesNotContain(whilePondering, fun l -> l.StartsWith "bestmove")
        eng.PonderHit()
        let after = readUntil eng "bestmove"
        Assert.Equal("bestmove g1f3 ponder b8c6", Array.last after)
        Assert.Equal<string[]>(
            [| "position startpos moves e2e4 e7e5"; ponderGo; "ponderhit" |],
            synced log eng.Write |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``Ponder miss: stop brings one stale bestmove, which the caller reads before the real search`` () =
    let log = newLogPath ()
    let eng = startTournament (config log "" [])
    try
        let before = synced log eng.Write |> Array.length
        eng.Position "position startpos moves e2e4 e7e5"
        eng.GoPonder ponderGo
        readFor eng 300 |> ignore
        // The opponent played 1...c5 instead: stop pondering, then search the real position.
        eng.Stop()
        let stale = readUntil eng "bestmove"
        Assert.Equal("bestmove g1f3 ponder b8c6", Array.last stale)
        eng.Position "position startpos moves e2e4"
        eng.Go(UnionType.WithIncrement (TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 1.0), TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 60.0)
        let real = readUntil eng "bestmove"
        Assert.Equal("bestmove e7e5 ponder g1f3", Array.last real)
        Assert.Equal<string[]>(
            [| "position startpos moves e2e4 e7e5"; ponderGo; "stop"
               "position startpos moves e2e4"; "go wtime 60000 btime 60000 winc 1000 binc 1000" |],
            synced log eng.Write |> Array.skip before)
    finally stopTournament eng

[<Fact>]
let ``Winboard has no ponder: go ponder becomes a plain go and ponderhit is dropped`` () =
    // A quirk worth knowing before anyone turns AllowPondering on with a Winboard engine: the
    // "ponder" is lost, so the engine searches (and moves) for the side to move on its board,
    // and ponderhit never reaches it.
    let log = newLogPath ()
    let eng = startTournament (wbConfig log "" [ "FakeInfinite", box true ] None)
    try
        let before = syncedWb log eng.Write |> Array.length
        eng.GoPonder ponderGo
        eng.PonderHit()
        eng.Stop()
        Assert.Equal<string[]>(
            [| "level 0 1 1"; "time 6000"; "otim 6000"; "go"; "?" |],
            syncedWb log eng.Write |> Array.skip before)
    finally stopTournament eng
