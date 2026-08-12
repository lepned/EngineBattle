namespace Benchmarks

open System
open System.Diagnostics
open BenchmarkDotNet.Attributes
open ChessLibrary
open ChessLibrary.MoveTypes
open ChessLibrary.PositionTypes
open ChessLibrary.MoveGeneration
open ChessLibrary.RuntimeUtilities
open ChessLibrary.Chess

/// Phase 0 baseline benchmarks for the movegen/legality optimization plan
/// (docs/MoveGenOptimizationPlan.md). Measures pseudo-legal generation,
/// per-move BoardHelper.Illegal filtering, perft throughput, and the
/// PV SAN conversion hot paths, so the LegalityContext work has a baseline.
module MoveGenBenchmarks =

    let startposFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let kiwipeteFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    let endgameFen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
    let frc1Fen = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"

    /// Some BENCHMARK_POS entries carry a " moves ..." suffix the FEN parser does not understand
    let cleanFen (fen: string) =
        let idx = fen.IndexOf(" moves")
        if idx >= 0 then fen.Substring(0, idx) else fen

    let loadBenchmarkPositions () =
        Perft.StandardPositions.BENCHMARK_POS
        |> Array.map (fun (_, fen) -> BoardHelper.getPosFromFen (Some (cleanFen fen)))

    /// Pseudo-legal generation only, over all positions. Returns total move count.
    let genOnly (positions: Position[]) (buffer: TMove[]) =
        let mutable total = 0
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let mutable index = 0
            let span = buffer.AsSpan()
            generateCaptures span &index &pos
            generateQuiets span &index &pos false
            total <- total + index
        total

    /// Pseudo-legal generation + per-move Illegal filter. Returns total legal move count.
    let genAndFilter (positions: Position[]) (buffer: TMove[]) =
        let mutable legal = 0
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let mutable index = 0
            let span = buffer.AsSpan()
            generateCaptures span &index &pos
            generateQuiets span &index &pos false
            for j = 0 to index - 1 do
                let mutable move = buffer.[j]
                if not (BoardHelper.Illegal &move &pos) then
                    legal <- legal + 1
        legal

    /// Illegal filter only, over pregenerated pseudo-legal move lists.
    let filterOnly (positions: Position[]) (pregen: TMove[][]) =
        let mutable legal = 0
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let moves = pregen.[i]
            for j = 0 to moves.Length - 1 do
                let mutable move = moves.[j]
                if not (BoardHelper.Illegal &move &pos) then
                    legal <- legal + 1
        legal

    /// Full new pipeline: context + legal-only generation (no filtering needed).
    let genLegal (positions: Position[]) (buffer: TMove[]) =
        let mutable total = 0
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let ctx = createLegalityContext &pos
            let mutable index = 0
            let span = buffer.AsSpan()
            generateLegalCaptures span &index &pos &ctx
            generateLegalQuiets span &index &pos false &ctx
            total <- total + index
        total

    /// LegalityContext creation only, one per position.
    let ctxCreateOnly (positions: Position[]) =
        let mutable acc = 0UL
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let ctx = createLegalityContext &pos
            acc <- acc ^^^ ctx.KingDanger
        acc

    /// Context-based filter (create context once per position + mask tests per move).
    let ctxFilterOnly (positions: Position[]) (pregen: TMove[][]) =
        let mutable legal = 0
        for i = 0 to positions.Length - 1 do
            let mutable pos = positions.[i]
            let ctx = createLegalityContext &pos
            let moves = pregen.[i]
            for j = 0 to moves.Length - 1 do
                let mutable move = moves.[j]
                if not (isIllegalCtx &ctx &move &pos) then
                    legal <- legal + 1
        legal

    /// OLD-pipeline perft (pseudo-legal generation + per-move Illegal filter) calling
    /// Illegal (1 + extra) times per candidate. Illegal is pure, so node counts are
    /// unchanged; comparing runtimes against extra=0 isolates the share of Illegal in
    /// old-pipeline perft cost. Generates via the pseudo-legal generators directly —
    /// Board.GenerateMovesToBuffer is legal-only since the Phase 3 rework.
    let perftExtraIllegal (board: Board) depth (extra: int) =
        let maxDepth = max depth 10
        let buffer = Array.zeroCreate<TMove> (256 * maxDepth)
        let sink = Array.zeroCreate<int> 1
        let isFRC = board.IsFRC

        let rec search depth offset =
            if depth = 0 then 1L
            else
                let mutable nodes = 0L
                let mutable pos = board.Position
                let bufferSpan = buffer.AsSpan(offset, 256)
                let mutable count = 0
                generateCaptures bufferSpan &count &pos
                generateQuiets bufferSpan &count &pos isFRC
                for i = 0 to count - 1 do
                    let mutable move = buffer.[offset + i]
                    for _ = 1 to extra do
                        if BoardHelper.Illegal &move &pos then sink.[0] <- sink.[0] + 1
                    if not (BoardHelper.Illegal &move &pos) then
                        if depth = 1 then
                            nodes <- nodes + 1L
                        else
                            board.MakeMoveNoHash(&move)
                            nodes <- nodes + search (depth - 1) (offset + 256)
                            board.UndoMove()
                nodes
        search depth 0

    // ------------------------------------------------------------------
    // BenchmarkDotNet suites
    // ------------------------------------------------------------------

    /// perft throughput on fixed positions (standard, tactical, endgame, FRC)
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type PerftBenchmarks() =

        let startposBoard = Board()
        let kiwipeteBoard = Board()
        let endgameBoard = Board()
        let frcBoard = Board()

        [<GlobalSetup>]
        member _.Setup() =
            startposBoard.LoadFen(startposFen)
            kiwipeteBoard.LoadFen(kiwipeteFen)
            endgameBoard.LoadFen(endgameFen)
            frcBoard.LoadFen(frc1Fen)

        [<Benchmark(Baseline = true, Description = "perft startpos d5")>]
        member _.PerftStartposD5() = Perft.perftFast startposBoard 5

        [<Benchmark(Description = "perft kiwipete d4")>]
        member _.PerftKiwipeteD4() = Perft.perftFast kiwipeteBoard 4

        [<Benchmark(Description = "perft endgame d5")>]
        member _.PerftEndgameD5() = Perft.perftFast endgameBoard 5

        [<Benchmark(Description = "perft frc1 d4")>]
        member _.PerftFrc1D4() = Perft.perftFast frcBoard 4

    /// Generation vs legality filtering over the 34 standard benchmark positions.
    /// FilterOnly isolates BoardHelper.Illegal — the target of the LegalityContext work.
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type LegalityFilterBenchmarks() =

        let mutable positions : Position[] = [||]
        let mutable pregen : TMove[][] = [||]
        let buffer = Array.zeroCreate<TMove> 256

        [<GlobalSetup>]
        member _.Setup() =
            positions <- loadBenchmarkPositions ()
            pregen <-
                positions
                |> Array.map (fun p ->
                    let mutable pos = p
                    let mutable index = 0
                    let buf = Array.zeroCreate<TMove> 256
                    generateCaptures (buf.AsSpan()) &index &pos
                    generateQuiets (buf.AsSpan()) &index &pos false
                    buf.[0 .. index - 1])

        [<Benchmark(Baseline = true, Description = "generate pseudo-legal (34 pos)")>]
        member _.GeneratePseudoLegal() = genOnly positions buffer

        [<Benchmark(Description = "generate + Illegal filter (34 pos)")>]
        member _.GenerateAndFilter() = genAndFilter positions buffer

        [<Benchmark(Description = "Illegal filter only (34 pos)")>]
        member _.FilterOnly() = filterOnly positions pregen

        [<Benchmark(Description = "LegalityContext create only (34 pos)")>]
        member _.CtxCreateOnly() = ctxCreateOnly positions

        [<Benchmark(Description = "LegalityContext filter (34 pos)")>]
        member _.CtxFilterOnly() = ctxFilterOnly positions pregen

        [<Benchmark(Description = "legal-only generation (34 pos)")>]
        member _.GenerateLegal() = genLegal positions buffer

    /// PV SAN conversion hot paths (per engine info line during tournaments/analysis)
    [<MemoryDiagnoser>]
    [<RankColumn>]
    type PvConversionBenchmarks() =

        let board = Board()
        let moveBuf = Array.zeroCreate<TMove> 256
        // 20-ply Ruy Lopez PV, castling included on both sides
        let uciPv = "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1 f8e7 f1e1 b7b5 a4b3 d7d6 c2c3 e8g8 h2h3 c6a5 b3c2 c7c5"
        let sanPv = [ "e4"; "e5"; "Nf3"; "Nc6"; "Bb5"; "a6"; "Ba4"; "Nf6"; "O-O"; "Be7"; "Re1"; "b5"; "Bb3"; "d6"; "c3"; "O-O"; "h3"; "Na5"; "Bc2"; "c5" ]

        [<GlobalSetup>]
        member _.Setup() =
            board.LoadFen(startposFen)

        [<Benchmark(Description = "UCI PV -> numbered SAN (20 plies)")>]
        member _.UciPvToSan() = BoardUtils.getShortSanPVFromLongSanPVFast moveBuf &board uciPv

        [<Benchmark(Description = "SAN PV -> UCI (20 plies)")>]
        member _.SanPvToUci() = BoardUtils.getLongSanPVFromShortSanPV moveBuf &board sanPv

    // ------------------------------------------------------------------
    // Quick mode (no BenchmarkDotNet — instant numbers for the plan doc)
    // ------------------------------------------------------------------

    let private timeBestOf (runs: int) (f: unit -> 'a) =
        f () |> ignore // warmup
        let mutable best = Double.MaxValue
        let mutable result = Unchecked.defaultof<'a>
        for _ = 1 to runs do
            let sw = Stopwatch.StartNew()
            result <- f ()
            sw.Stop()
            best <- min best sw.Elapsed.TotalMilliseconds
        result, best

    let private timePerIter (label: string) (iters: int) (f: unit -> 'a) =
        f () |> ignore // warmup
        let sw = Stopwatch.StartNew()
        for _ = 1 to iters do
            f () |> ignore
        sw.Stop()
        let usPerIter = sw.Elapsed.TotalMilliseconds * 1000.0 / float iters
        printfn "  %-42s %10.2f us/iter  (%d iters, %.0f ms total)" label usPerIter iters sw.Elapsed.TotalMilliseconds
        usPerIter

    let runMoveGenQuick () =
        printfn "MoveGen Quick Benchmarks"
        printfn "========================"

        // 1) perft throughput
        printfn "\n[1] perft throughput (perftFast, bulk leaf counting, best of 3):"
        let perftCases =
            [ "startpos d5", startposFen, 5, Some 4_865_609L
              "kiwipete d4", kiwipeteFen, 4, Some 4_085_603L
              "endgame  d5", endgameFen, 5, Some 674_624L
              "frc1     d4", frc1Fen, 4, None ]
        for (label, fen, depth, expected) in perftCases do
            let board = Board()
            board.LoadFen(fen)
            let nodes, ms = timeBestOf 3 (fun () -> Perft.perftFast board depth)
            let nps = float nodes / (ms / 1000.0)
            let check =
                match expected with
                | Some e when e <> nodes -> sprintf "  MISMATCH! expected %d" e
                | Some _ -> "  (count verified)"
                | None -> ""
            printfn "  %-12s %12s nodes  %8.1f ms  %14s NPS%s" label (nodes.ToString("N0")) ms ((int64 nps).ToString("N0")) check

        // 2) share of Illegal in perft, via extra pure Illegal calls per candidate
        printfn "\n[2] share of BoardHelper.Illegal in perft (extra-call method):"
        printfn "    t(extra=2) vs t(extra=0): Illegal cost = (t2-t0)/2, share = cost/t0"
        for (label, fen, depth) in [ "startpos d5", startposFen, 5; "kiwipete d4", kiwipeteFen, 4 ] do
            let board = Board()
            board.LoadFen(fen)
            let _, t0 = timeBestOf 3 (fun () -> perftExtraIllegal board depth 0)
            let _, t2 = timeBestOf 3 (fun () -> perftExtraIllegal board depth 2)
            let illegalMs = (t2 - t0) / 2.0
            printfn "  %-12s t0=%8.1f ms  t2=%8.1f ms  -> Illegal ~%6.1f ms  (~%.0f%% of perft)"
                label t0 t2 illegalMs (100.0 * illegalMs / t0)

        // 3) static generation/filter microbench over the 34 standard positions
        printfn "\n[3] generation vs legality filter (34 standard positions per iter):"
        let positions = loadBenchmarkPositions ()
        let buffer = Array.zeroCreate<TMove> 256
        let pregen =
            positions
            |> Array.map (fun p ->
                let mutable pos = p
                let mutable index = 0
                let buf = Array.zeroCreate<TMove> 256
                generateCaptures (buf.AsSpan()) &index &pos
                generateQuiets (buf.AsSpan()) &index &pos false
                buf.[0 .. index - 1])
        let totalMoves = pregen |> Array.sumBy Array.length
        printfn "  (%d pseudo-legal moves across %d positions)" totalMoves positions.Length
        let iters = 20_000
        let tGen = timePerIter "generate pseudo-legal" iters (fun () -> genOnly positions buffer)
        let tGenFilter = timePerIter "generate + Illegal filter (old pipeline)" iters (fun () -> genAndFilter positions buffer)
        let tGenLegal = timePerIter "legal-only generation (new pipeline)" iters (fun () -> genLegal positions buffer)
        let tFilter = timePerIter "Illegal filter only (pregenerated)" iters (fun () -> filterOnly positions pregen)
        let tCtxCreate = timePerIter "LegalityContext create only" iters (fun () -> ctxCreateOnly positions)
        let tCtxFilter = timePerIter "LegalityContext filter (pregenerated)" iters (fun () -> ctxFilterOnly positions pregen)
        printfn "  -> Illegal filter is ~%.0f%% of generate+filter; ~%.2f us per position, ~%.0f ns per move"
            (100.0 * tFilter / tGenFilter)
            (tFilter / float positions.Length)
            (tFilter * 1000.0 / float totalMoves)
        printfn "  -> ctx filter vs Illegal filter: %.2fx; ctx creation ~%.0f ns per position (%.0f%% of ctx filter)"
            (tFilter / tCtxFilter)
            (tCtxCreate * 1000.0 / float positions.Length)
            (100.0 * tCtxCreate / tCtxFilter)
        printfn "  -> new pipeline (legal-only gen) vs old pipeline (gen + filter): %.2fx" (tGenFilter / tGenLegal)

        // 4) PV SAN conversion hot paths
        printfn "\n[4] PV SAN conversion (20-ply PV from startpos):"
        let board = Board()
        board.LoadFen(startposFen)
        let moveBuf = Array.zeroCreate<TMove> 256
        let uciPv = "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1 f8e7 f1e1 b7b5 a4b3 d7d6 c2c3 e8g8 h2h3 c6a5 b3c2 c7c5"
        let sanPv = [ "e4"; "e5"; "Nf3"; "Nc6"; "Bb5"; "a6"; "Ba4"; "Nf6"; "O-O"; "Be7"; "Re1"; "b5"; "Bb3"; "d6"; "c3"; "O-O"; "h3"; "Na5"; "Bc2"; "c5" ]
        // sanity: both directions must round-trip the full PV
        let sanOut = BoardUtils.getShortSanPVFromLongSanPVFast moveBuf &board uciPv
        let uciOut = BoardUtils.getLongSanPVFromShortSanPV moveBuf &board sanPv
        if sanOut.Split(' ').Length <> 20 then printfn "  WARNING: UCI->SAN returned %d tokens: %s" (sanOut.Split(' ').Length) sanOut
        if uciOut.Split(' ').Length <> 20 then printfn "  WARNING: SAN->UCI returned %d tokens: %s" (uciOut.Split(' ').Length) uciOut
        let tUciSan = timePerIter "UCI PV -> numbered SAN" 5_000 (fun () -> BoardUtils.getShortSanPVFromLongSanPVFast moveBuf &board uciPv)
        let tSanUci = timePerIter "SAN PV -> UCI" 5_000 (fun () -> BoardUtils.getLongSanPVFromShortSanPV moveBuf &board sanPv)
        printfn "  -> %.0f UCI->SAN and %.0f SAN->UCI PV conversions/sec"
            (1_000_000.0 / tUciSan) (1_000_000.0 / tSanUci)

        printfn "\nDone. Full BenchmarkDotNet suites: dotnet run -c Release -- movegen"
