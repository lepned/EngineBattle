/// Measurements of `type Board` (ChessLibrary/Chess/Board.fs), for the rewrite on rewrite/board-fs.
/// The same custom harness as EngineBenchmarks (median/p95 wall time, this process's CPU, whole-
/// process allocations, GC counts), over the ways EB drives a board: a tournament game (MakeMove
/// and the repetition check per move), the GUI (PlayUciMove), PGN replays (PlaySanMove, short and
/// long, as the analysis tools and Game Review do), a game with variations loaded and printed,
/// stepping back and forth, and the position command. The games are random but seeded, so every
/// run replays the same moves.
module BoardBenchmarks

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open ChessLibrary
open ChessLibrary.Chess
open ChessLibrary.BoardUtils

type Result =
    { Name: string
      Runs: int
      Units: int
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

let private measure name units unitName warmup runs (run: unit -> unit) =
    for _ in 1 .. warmup do run ()
    GC.Collect(); GC.WaitForPendingFinalizers(); GC.Collect()
    let proc = Process.GetCurrentProcess()
    let cpu0 = proc.TotalProcessorTime
    let alloc0 = GC.GetTotalAllocatedBytes(true)
    let g0, g1, g2 = GC.CollectionCount 0, GC.CollectionCount 1, GC.CollectionCount 2
    let times =
        [| for _ in 1 .. runs ->
            let sw = Stopwatch.StartNew()
            run ()
            sw.Elapsed.TotalMilliseconds |]
    proc.Refresh()
    let per x = x / float runs
    { Name = name; Runs = runs; Units = units; UnitName = unitName
      MedianMs = percentile 0.5 times; P95Ms = percentile 0.95 times
      CpuMsPerRun = per (proc.TotalProcessorTime - cpu0).TotalMilliseconds
      AllocBytesPerRun = per (float (GC.GetTotalAllocatedBytes(true) - alloc0))
      Gen0PerRun = per (float (GC.CollectionCount 0 - g0))
      Gen1PerRun = per (float (GC.CollectionCount 1 - g1))
      Gen2PerRun = per (float (GC.CollectionCount 2 - g2)) }

// ── Games ───────────────────────────────────────────────────────────────────────────────────────

/// A random legal game of up to `plies` plies as (uci, san) pairs, the same every run.
let private randomGame (seed: int) (plies: int) =
    let rnd = Random seed
    let b = Board()
    b.ResetBoardState()
    let moves = ResizeArray<string * string>()
    while moves.Count < plies && b.AnyLegalMove() do
        let legal = b.GetLegalMoves() |> Seq.toArray
        let uci, san = legal.[rnd.Next legal.Length]
        b.PlayUciMove uci
        moves.Add((uci, san))
    moves.ToArray()

/// PGN text of a game with a `varLen`-move variation after every `every`th ply: what Game Review
/// builds for a reviewed game, and what a study looks like.
let private pgnWithVariations (seed: int) (plies: int) (every: int) (varLen: int) =
    let rnd = Random seed
    let main = randomGame seed plies
    let b = Board()
    b.ResetBoardState()
    let sb = StringBuilder("[Event \"bench\"]\n[Result \"*\"]\n\n")
    let number ply = if ply % 2 = 0 then sprintf "%d. " (ply / 2 + 1) else sprintf "%d... " (ply / 2 + 1)
    for ply in 0 .. main.Length - 1 do
        let uci, san = main.[ply]
        let fenBefore = b.FEN()
        sb.Append(number ply).Append(san).Append(' ') |> ignore
        if ply % every = every - 1 then
            // a variation replacing this move
            let v = Board()
            v.ResetBoardStateFromFen fenBefore
            let line = ResizeArray<string>()
            let mutable i = 0
            while i < varLen && v.AnyLegalMove() do
                let legal = v.GetLegalMoves() |> Seq.toArray
                let vu, vs = legal.[rnd.Next legal.Length]
                line.Add((if i = 0 then number ply else if (ply + i) % 2 = 0 then number (ply + i) else "") + vs)
                v.PlayUciMove vu
                i <- i + 1
            sb.Append("(").Append(String.Join(" ", line)).Append(") ") |> ignore
        b.PlayUciMove uci
    sb.Append("*").ToString()

// ── Scenarios ───────────────────────────────────────────────────────────────────────────────────

/// A tournament game as the game loop plays it: UciMovesPlayed by hand, MakeMove, and the
/// repetition count and claim after every move.
let private tournamentGame name (game: (string * string)[]) runs =
    measure name game.Length "plies" 20 runs (fun () ->
        let board = Board()
        board.ResetBoardState()
        board.LoadFen startPos
        board.StartPosition <- startPos
        for uci, _ in game do
            match tryGetMoveAndSanFromUci &board uci with
            | Some (tmove, _) ->
                let mutable m = tmove
                board.UciMovesPlayed.Add uci
                board.MakeMove &m
                board.RepetitionNr() |> ignore
                board.ClaimThreeFoldRep() |> ignore
            | None -> failwith "illegal")

let private playUci name (game: (string * string)[]) runs =
    measure name game.Length "plies" 20 runs (fun () ->
        let board = Board()
        board.ResetBoardState()
        for uci, _ in game do board.PlayUciMove uci)

let private playSan name (game: (string * string)[]) runs =
    measure name game.Length "plies" 20 runs (fun () ->
        let board = Board()
        board.ResetBoardState()
        for _, san in game do board.PlaySanMove san)

let private loadWithVariations name (pgn: PGNTypes.PgnGame) plies runs =
    measure name plies "plies" 5 runs (fun () ->
        let board = Board()
        board.ResetBoardState()
        board.LoadPGNGameWithVariations pgn)

let private printWithVariations name (pgn: PGNTypes.PgnGame) plies runs =
    let board = Board()
    board.ResetBoardState()
    board.LoadPGNGameWithVariations pgn
    measure name plies "plies" 5 runs (fun () ->
        board.GetMoveHistoryWithVariations() |> ignore
        board.InlineTokensFromGraph() |> ignore)

/// The GUI's back and forward buttons: TryGetPrevious/Next with the FEN shown, then LoadFen.
let private stepBackAndForth name (game: (string * string)[]) runs =
    let board = Board()
    board.ResetBoardState()
    for uci, _ in game do board.PlayUciMove uci
    measure name (2 * game.Length) "steps" 5 runs (fun () ->
        for _ in 1 .. game.Length do
            match board.TryGetPreviousMoveAndFen(board.FEN()) with
            | Some m -> board.LoadFen m.FenAfterMove
            | None -> ()
        for _ in 1 .. game.Length do
            match board.TryGetNextMoveAndFen(board.FEN()) with
            | Some m -> board.LoadFen m.FenAfterMove
            | None -> ())

let private positionCommand name (game: (string * string)[]) calls runs =
    let board = Board()
    board.ResetBoardState()
    for uci, _ in game do board.PlayUciMove uci
    measure name calls "calls" 5 runs (fun () ->
        for _ in 1 .. calls do board.PositionWithMoves() |> ignore)

// ── Runner ──────────────────────────────────────────────────────────────────────────────────────

let private print (r: Result) =
    let perUnitUs = r.MedianMs * 1000.0 / float r.Units
    printfn "%-40s %8.2f %8.2f %9.2f %9.1f %11.0f %7.2f %6.2f %5.2f"
        r.Name r.MedianMs r.P95Ms perUnitUs r.CpuMsPerRun (r.AllocBytesPerRun / float r.Units) r.Gen0PerRun r.Gen1PerRun r.Gen2PerRun

let run (args: string[]) =
    let jsonOut =
        match args |> Array.tryFindIndex ((=) "--json") with
        | Some i when i + 1 < args.Length -> Some args.[i + 1]
        | _ -> None
    let g120 = randomGame 11 120
    let g300 = randomGame 7 300
    let study = FullPGNParser.parseFullPgnGame (pgnWithVariations 5 120 4 8)
    let studyPlies = 120 + (120 / 4) * 8
    printfn "Board benchmarks (%d- and %d-ply seeded games; a %d-ply game with a variation every 4th ply)" g120.Length g300.Length studyPlies
    printfn "%s, %s" Runtime.InteropServices.RuntimeInformation.OSDescription Runtime.InteropServices.RuntimeInformation.FrameworkDescription
    printfn ""
    printfn "%-40s %8s %8s %9s %9s %11s %7s %6s %5s" "scenario" "med ms" "p95 ms" "us/unit" "cpu ms" "bytes/unit" "gen0" "gen1" "gen2"
    printfn "%s" (String.replicate 112 "-")
    let results =
        [ tournamentGame "tournament game, 120 plies" g120 200
          playUci "GUI PlayUciMove, 120 plies" g120 200
          playSan "PGN replay PlaySanMove, 120 plies" g120 200
          playSan "PGN replay PlaySanMove, 300 plies" g300 100
          loadWithVariations "load PGN with variations" study studyPlies 50
          printWithVariations "print game with variations" study studyPlies 100
          stepBackAndForth "step back and forth, 120 plies" g120 50
          positionCommand "PositionWithMoves at ply 120 (x100)" g120 100 50 ]
        |> List.map (fun r -> print r; r)
    printfn ""
    printfn "us/unit = median wall time per ply, step or call; cpu ms = this process, per run;"
    printfn "bytes/unit = whole-process allocations per ply, step or call."
    match jsonOut with
    | Some path ->
        File.WriteAllText(path, JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented = true)))
        printfn "Written: %s" path
    | None -> ()
    0
