/// End-to-end measurements of ChessLibrary/Engine/Engine.fs, for the rewrite on rewrite/engine-fs.
///
/// Every scenario runs FakeUciEngine (copied beside this program) as a real child process, so the
/// numbers include the pipes, the reader threads and the parsing - the whole path an info line or a
/// bestmove takes. BenchmarkDotNet is not used: what matters here crosses threads and processes,
/// and its allocation counter only sees the benchmark thread. Instead each scenario reports:
///   - wall time per run (median and p95 over the runs)
///   - CPU time of THIS process per run, which separates EngineBattle's own work from the fake
///     engine's (the fake engine is a separate process and is not counted)
///   - bytes allocated by the whole process per run (GC.GetTotalAllocatedBytes, all threads)
///   - GC collections per run
/// Run `engine` for the table and `engine --json FILE` to also write the numbers, so a baseline
/// taken before the rewrite can be compared with the rewrite on the same machine.
module EngineBenchmarks

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.Engine

let private fakeExe = if OperatingSystem.IsWindows() then "FakeUciEngine.exe" else "FakeUciEngine"
let private fakePath = Path.Combine(AppContext.BaseDirectory, fakeExe)
let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

type Result =
    { Name: string
      Runs: int
      Units: int            // lines or round trips per run
      UnitName: string
      MedianMs: float
      P95Ms: float
      CpuMsPerRun: float
      AllocBytesPerRun: float
      Gen0PerRun: float
      Gen1PerRun: float
      Gen2PerRun: float }

let private percentile (p: float) (xs: float[]) =
    let s = Array.sort xs
    s.[min (s.Length - 1) (int (Math.Ceiling(p * float s.Length)) - 1 |> max 0)]

/// Runs `run` `warmup + runs` times and measures the last `runs`. `run` returns the wall time it
/// wants counted (so set-up inside a run, like sending the position, can be left out).
let private measure name units unitName warmup runs (run: unit -> TimeSpan) =
    for _ in 1 .. warmup do run () |> ignore
    GC.Collect(); GC.WaitForPendingFinalizers(); GC.Collect()
    let proc = Process.GetCurrentProcess()
    let cpu0 = proc.TotalProcessorTime
    let alloc0 = GC.GetTotalAllocatedBytes(true)
    let g0, g1, g2 = GC.CollectionCount 0, GC.CollectionCount 1, GC.CollectionCount 2
    let times = [| for _ in 1 .. runs -> (run ()).TotalMilliseconds |]
    proc.Refresh()
    let cpu = (proc.TotalProcessorTime - cpu0).TotalMilliseconds
    let alloc = float (GC.GetTotalAllocatedBytes(true) - alloc0)
    let per x = x / float runs
    { Name = name; Runs = runs; Units = units; UnitName = unitName
      MedianMs = percentile 0.5 times; P95Ms = percentile 0.95 times
      CpuMsPerRun = per cpu; AllocBytesPerRun = per alloc
      Gen0PerRun = per (float (GC.CollectionCount 0 - g0))
      Gen1PerRun = per (float (GC.CollectionCount 1 - g1))
      Gen2PerRun = per (float (GC.CollectionCount 2 - g2)) }

let private config (extraArgs: string) (options: (string * obj) list) =
    let opts = Dictionary<string, obj>()
    for (k, v) in options do opts.[k] <- v
    { EngineConfig.Empty with Name = "Fake"; Path = fakePath; Args = extraArgs; Options = opts }

let private initCommands cfg = EngineHelper.createInitialUCICommands cfg |> Seq.toList

/// The analysis engine with a callback that signals Done; console output from the engine's
/// start-up is swallowed so the table stays readable.
let private startAnalysisWith (logToFile: bool) cfg =
    let doneSignal = new AutoResetEvent(false)
    let mutable lines = 0
    let callback (u: EngineUpdate) =
        match u with
        | Status _ | NNSeq _ -> Interlocked.Increment(&lines) |> ignore
        | Done _ -> doneSignal.Set() |> ignore
        | _ -> ()
    let out = Console.Out
    Console.SetOut TextWriter.Null
    let eng =
        try
            let eng = new AnalysisEngine(callback, cfg, initCommands cfg, NullLogger.Instance, false, logToFile = logToFile)
            if not (eng.WaitUntilStarted 60000) then
                eng.Quit()
                failwith "the analysis engine did not start"
            eng
        finally Console.SetOut out
    eng, doneSignal

let private startAnalysis cfg = startAnalysisWith false cfg

let private startTournament cfg =
    let out = Console.Out
    Console.SetOut TextWriter.Null
    try new ChessEngine(cfg, initCommands cfg, Some (NullLogger.Instance :> ILogger))
    finally Console.SetOut out

let private quietly f =
    let out = Console.Out
    Console.SetOut TextWriter.Null
    try f () finally Console.SetOut out

let private stopTournament (eng: ChessEngine) = quietly (fun () -> (try eng.Quit(); eng.StopProcess() with _ -> ()))
let private quitAnalysis (eng: AnalysisEngine) = quietly (fun () -> (try eng.Quit() with _ -> ()))

let private readUntilBestmove (eng: ChessEngine) =
    use cts = new CancellationTokenSource(60000)
    let mutable fin = false
    let mutable n = 0
    while not fin do
        let l = eng.ReadLineAsyncWithTimeout(cts.Token).Result
        if isNull l || l.StartsWith "bestmove" then fin <- true
        else n <- n + 1
    n

// ── Scenarios ───────────────────────────────────────────────────────────────────────────────────

let private analysisFloodWith (logToFile: bool) name (lines: int) (multiPv: int) (moveStats: bool) runs =
    let infoCount = if moveStats then 1 else lines / multiPv
    let opts =
        [ "FakeInfoCount", box infoCount; "MultiPV", box multiPv; "FakePvLength", box 10
          "FakeMoveStats", box moveStats; "FakeMoveStatsRepeat", box (if moveStats then lines / 3 else 1) ]
    let eng, doneSignal = startAnalysisWith logToFile (config "" opts)
    try
        measure name lines "lines" 2 runs (fun () ->
            let sw = Stopwatch.StartNew()
            eng.Analyse("position fen " + startFen, "go nodes 1000")
            if not (doneSignal.WaitOne 120000) then failwithf "%s: no Done" name
            sw.Elapsed)
    finally quitAnalysis eng

let private analysisFlood name lines multiPv moveStats runs = analysisFloodWith false name lines multiPv moveStats runs

/// As the analysis pages run it: createAltEngine turns on the per-engine I/O log, one line per
/// line read. The log goes under a temporary working directory, not beside the benchmarks.
let private analysisFloodLogged name lines runs =
    let dir = Path.Combine(Path.GetTempPath(), "eb-engine-bench-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    let cwd = Environment.CurrentDirectory
    Environment.CurrentDirectory <- dir
    try analysisFloodWith true name lines 1 false runs
    finally
        Environment.CurrentDirectory <- cwd
        try Directory.Delete(dir, true) with _ -> ()

let private tournamentReadFlood name (lines: int) runs =
    let eng = startTournament (config "" [ "FakeInfoCount", box lines; "FakePvLength", box 10 ])
    try
        measure name lines "lines" 2 runs (fun () ->
            eng.Position "position startpos"
            let sw = Stopwatch.StartNew()
            eng.GoNodes 1000
            let n = readUntilBestmove eng
            if n < lines then failwithf "%s: read %d of %d lines" name n lines
            sw.Elapsed)
    finally stopTournament eng

let private winboardReadFlood name (lines: int) runs =
    let cfg =
        { config "--xboard" [ "FakeInfoCount", box lines; "FakePvLength", box 10 ] with
            Protocol = "Winboard"
            WinboardConfig = Some { WinboardConfig.Default with PreGoDelayMs = 0 } }
    let eng = startTournament cfg
    try
        measure name lines "lines" 2 runs (fun () ->
            eng.UciNewGame()
            eng.Position "position startpos"
            let sw = Stopwatch.StartNew()
            eng.Go 1000
            let n = readUntilBestmove eng
            if n < lines then failwithf "%s: read %d of %d lines" name n lines
            sw.Elapsed)
    finally stopTournament eng

let private readyRoundTripTournament name trips runs =
    let eng = startTournament (config "" [])
    try
        measure name trips "round trips" 1 runs (fun () ->
            let sw = Stopwatch.StartNew()
            for _ in 1 .. trips do
                if not (eng.WaitForReadyOk()) then failwith "readyok failed"
            sw.Elapsed)
    finally stopTournament eng

let private bestmoveRoundTripTournament name trips runs =
    let eng = startTournament (config "" [ "FakeInfoCount", box 0 ])
    try
        measure name trips "round trips" 1 runs (fun () ->
            let sw = Stopwatch.StartNew()
            for _ in 1 .. trips do
                eng.Position "position startpos"
                eng.GoNodes 1
                readUntilBestmove eng |> ignore
            sw.Elapsed)
    finally stopTournament eng

let private bestmoveRoundTripAnalysis name trips runs =
    let eng, _ = startAnalysis (config "" [ "FakeInfoCount", box 0 ])
    try
        measure name trips "round trips" 1 runs (fun () ->
            let sw = Stopwatch.StartNew()
            // each search is isready + go + bestmove, as the analysis pages run it
            for _ in 1 .. trips do
                match eng.Search("position fen " + startFen, "go nodes 1") |> Async.RunSynchronously with
                | Completed _ -> ()
                | other -> failwithf "search ended %A" other
            sw.Elapsed)
    finally quitAnalysis eng

// ── Runner ──────────────────────────────────────────────────────────────────────────────────────

let private print (r: Result) =
    let perUnitUs = r.MedianMs * 1000.0 / float r.Units
    printfn "%-34s %8.1f %8.1f %10.2f %9.1f %12.0f %8.1f %6.2f %5.2f"
        r.Name r.MedianMs r.P95Ms perUnitUs r.CpuMsPerRun (r.AllocBytesPerRun / float r.Units) r.Gen0PerRun r.Gen1PerRun r.Gen2PerRun

let run (args: string[]) =
    if not (File.Exists fakePath) then failwithf "FakeUciEngine not found beside the benchmarks: %s" fakePath
    let jsonOut =
        match args |> Array.tryFindIndex ((=) "--json") with
        | Some i when i + 1 < args.Length -> Some args.[i + 1]
        | _ -> None
    printfn "Engine.fs end-to-end benchmarks (FakeUciEngine as a child process)"
    printfn "%s, %s, %d cores" (Runtime.InteropServices.RuntimeInformation.OSDescription) (Runtime.InteropServices.RuntimeInformation.FrameworkDescription) Environment.ProcessorCount
    printfn ""
    printfn "%-34s %8s %8s %10s %9s %12s %8s %6s %5s" "scenario" "med ms" "p95 ms" "us/unit" "cpu ms" "bytes/unit" "gen0" "gen1" "gen2"
    printfn "%s" (String.replicate 110 "-")
    let results =
        [ analysisFlood "analysis flood mpv1 (20k lines)" 20000 1 false 10
          analysisFlood "analysis flood mpv4 (20k lines)" 20000 4 false 10
          analysisFlood "analysis move stats (20k lines)" 20000 1 true 10
          analysisFloodLogged "analysis flood + I/O log (20k)" 20000 10
          tournamentReadFlood "tournament read (20k lines)" 20000 10
          winboardReadFlood "winboard read (20k lines)" 20000 10
          readyRoundTripTournament "isready tournament (x200)" 200 5
          bestmoveRoundTripTournament "go->bestmove tournament (x200)" 200 5
          bestmoveRoundTripAnalysis "go->bestmove analysis (x200)" 200 5 ]
        |> List.map (fun r -> print r; r)
    printfn ""
    printfn "us/unit = median wall time per line or round trip; cpu ms = this process only, per run;"
    printfn "bytes/unit = whole-process allocations per line or round trip."
    match jsonOut with
    | Some path ->
        let json = JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(path, json)
        printfn "Written: %s" path
    | None -> ()
    0
