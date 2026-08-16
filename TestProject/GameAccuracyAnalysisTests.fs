module GameAccuracyAnalysisTests

open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.GameAccuracyAnalysis

// ──────────────────────────────────────────────────────────────
// evalToWinProb tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``evalToWinProb returns 0.5 for equal position`` () =
    let wp = evalToWinProb (CP 0.0) 0
    Assert.Equal(0.5, wp, 3)

[<Fact>]
let ``evalToWinProb returns 1.0 for positive mate`` () =
    let wp = evalToWinProb (Mate 5) 0
    Assert.Equal(1.0, wp, 3)

[<Fact>]
let ``evalToWinProb returns 0.0 for negative mate`` () =
    let wp = evalToWinProb (Mate -3) 0
    Assert.Equal(0.0, wp, 3)

[<Fact>]
let ``evalToWinProb returns 0.5 for NA`` () =
    let wp = evalToWinProb NA 0
    Assert.Equal(0.5, wp, 3)

[<Fact>]
let ``evalToWinProb returns higher WP for positive eval`` () =
    let wp = evalToWinProb (CP 2.0) 0  // +200cp
    Assert.True(wp > 0.55, sprintf "Expected WP > 0.55 but got %f" wp)
    Assert.True(wp < 1.0, sprintf "Expected WP < 1.0 but got %f" wp)

[<Fact>]
let ``evalToWinProb returns lower WP for negative eval`` () =
    let wp = evalToWinProb (CP -2.0) 0  // -200cp
    Assert.True(wp < 0.45, sprintf "Expected WP < 0.45 but got %f" wp)
    Assert.True(wp > 0.0, sprintf "Expected WP > 0.0 but got %f" wp)

[<Fact>]
let ``evalToWinProb is symmetric around 0`` () =
    let wpPos = evalToWinProb (CP 1.5) 20
    let wpNeg = evalToWinProb (CP -1.5) 20
    Assert.Equal(wpPos, 1.0 - wpNeg, 6)

[<Fact>]
let ``evalToWinProb same CP has more impact at higher ply`` () =
    // K decreases with ply, so same CP has more impact in endgame
    let wpEarly = evalToWinProb (CP 1.0) 0    // K = 345
    let wpLate = evalToWinProb (CP 1.0) 60     // K = 261
    Assert.True(wpLate > wpEarly, sprintf "Expected late WP (%f) > early WP (%f)" wpLate wpEarly)

[<Fact>]
let ``evalToWinProb Mate 0 returns 0.5`` () =
    // Mate in 0 is a degenerate case
    let wp = evalToWinProb (Mate 0) 0
    Assert.Equal(0.5, wp, 3)

// ──────────────────────────────────────────────────────────────
// flipEval tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``flipEval negates CP`` () =
    Assert.Equal(CP -1.5, flipEval (CP 1.5))
    Assert.Equal(CP 2.0, flipEval (CP -2.0))

[<Fact>]
let ``flipEval negates Mate`` () =
    Assert.Equal(Mate -5, flipEval (Mate 5))
    Assert.Equal(Mate 3, flipEval (Mate -3))

[<Fact>]
let ``flipEval preserves NA`` () =
    Assert.Equal(NA, flipEval NA)

// ──────────────────────────────────────────────────────────────
// calculateMoveAccuracy tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``calculateMoveAccuracy returns 100 for 0 WP loss`` () =
    let acc = calculateMoveAccuracy 0.065 0.0
    Assert.Equal(100.0, acc, 0)

[<Fact>]
let ``calculateMoveAccuracy returns near 0 for large WP loss`` () =
    let acc = calculateMoveAccuracy 0.065 0.50  // 50% WP loss
    Assert.True(acc < 15.0, sprintf "Expected acc < 15 but got %f" acc)

[<Fact>]
let ``calculateMoveAccuracy is monotonically decreasing`` () =
    let losses = [| 0.0; 0.02; 0.05; 0.10; 0.20; 0.30; 0.50 |]
    let accuracies = losses |> Array.map (calculateMoveAccuracy 0.065)
    for i in 1 .. accuracies.Length - 1 do
        Assert.True(accuracies.[i] <= accuracies.[i-1],
            sprintf "Expected accuracy to decrease: %f <= %f at index %d" accuracies.[i] accuracies.[i-1] i)

[<Fact>]
let ``calculateMoveAccuracy is clamped between 0 and 100`` () =
    Assert.True(calculateMoveAccuracy 0.065 0.0 <= 100.0)
    Assert.True(calculateMoveAccuracy 0.065 0.0 >= 0.0)
    Assert.True(calculateMoveAccuracy 0.065 1.0 >= 0.0)
    Assert.True(calculateMoveAccuracy 0.065 1.0 <= 100.0)

[<Fact>]
let ``higher decay produces lower accuracy for same WP loss`` () =
    let accLichess = calculateMoveAccuracy 0.04354 0.10
    let accSteep = calculateMoveAccuracy 0.065 0.10
    Assert.True(accSteep < accLichess, sprintf "Steep %f should be < Lichess %f" accSteep accLichess)

// ──────────────────────────────────────────────────────────────
// Micro-loss tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``micro-loss gives penalty for PV1 match in easy position`` () =
    // pvGap = 0 (all moves equally good) → microLoss = base = 0.02
    let microLoss = max 0.0 (0.02 - 0.0 * 0.20)
    let acc = calculateMoveAccuracy 0.065 microLoss
    Assert.True(acc < 100.0, sprintf "Expected acc < 100 for easy PV1 match but got %f" acc)
    Assert.True(acc > 80.0, sprintf "Expected acc > 80 but got %f" acc)

[<Fact>]
let ``micro-loss is zero for hard positions with large PV gap`` () =
    // pvGap = 0.15 → microLoss = max(0, 0.02 - 0.15 * 0.20) = max(0, -0.01) = 0
    let microLoss = max 0.0 (0.02 - 0.15 * 0.20)
    Assert.Equal(0.0, microLoss, 10)

[<Fact>]
let ``micro-loss scales linearly with PV gap`` () =
    let loss0 = max 0.0 (0.02 - 0.0 * 0.20)   // gap=0 → 0.02
    let loss5 = max 0.0 (0.02 - 0.05 * 0.20)  // gap=5% → 0.01
    let loss10 = max 0.0 (0.02 - 0.10 * 0.20) // gap=10% → 0.0
    Assert.True(loss0 > loss5, "Gap=0 should have higher micro-loss than gap=5%")
    Assert.True(loss5 > loss10, "Gap=5% should have higher micro-loss than gap=10%")

// ──────────────────────────────────────────────────────────────
// classifyMove tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``classifyMove returns Forced when isForced`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.30 false true None false
    Assert.Equal(Forced, cls)

[<Fact>]
let ``classifyMove returns Excellent for best move without significant gap`` () =
    // PV1 match but no PV gap data → easy position → Excellent, not Best
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false None false
    Assert.Equal(Excellent, cls)

[<Fact>]
let ``classifyMove returns Best for best move with sufficient gap`` () =
    // PV1 match with gap >= BestMinPVGap (0.05) → genuinely hard find → Best
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false (Some 0.06) false
    Assert.Equal(Best, cls)

[<Fact>]
let ``classifyMove returns Great for best move with 12 pct gap`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false (Some 0.12) false
    Assert.Equal(Great, cls)

[<Fact>]
let ``classifyMove returns Brilliant for best move with 16 pct gap and sacrifice`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false (Some 0.16) true
    Assert.Equal(Brilliant, cls)

[<Fact>]
let ``classifyMove returns Great not Brilliant without sacrifice`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false (Some 0.16) false
    Assert.Equal(Great, cls)

[<Fact>]
let ``classifyMove returns Excellent for < 2 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.01 false false None false
    Assert.Equal(Excellent, cls)

[<Fact>]
let ``classifyMove returns Good for 2-3 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.025 false false None false
    Assert.Equal(Good, cls)

[<Fact>]
let ``classifyMove returns Inaccuracy for 3-5 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.04 false false None false
    Assert.Equal(Inaccuracy, cls)

[<Fact>]
let ``classifyMove returns Mistake for 5-10 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.07 false false None false
    Assert.Equal(Mistake, cls)

[<Fact>]
let ``classifyMove returns Blunder for >= 10 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.15 false false None false
    Assert.Equal(Blunder, cls)

// ──────────────────────────────────────────────────────────────
// Classification boundary tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``classifyMove at exact boundary 0.02 returns Good not Excellent`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.02 false false None false
    Assert.Equal(Good, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.03 returns Inaccuracy not Good`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.03 false false None false
    Assert.Equal(Inaccuracy, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.05 returns Mistake not Inaccuracy`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.05 false false None false
    Assert.Equal(Mistake, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.10 returns Blunder not Mistake`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.10 false false None false
    Assert.Equal(Blunder, cls)

// ──────────────────────────────────────────────────────────────
// computePlayerStats tests
// ──────────────────────────────────────────────────────────────

let private mkMove ply moveNum color san cls wpLoss cpLoss =
    { Ply = ply; MoveNumber = moveNum; Color = color; San = san; UciMove = ""
      Classification = cls; EvalBefore = CP 0.0; BestMove = ""; BestMoveSan = ""
      BestEval = CP 0.0; SecondBestEval = None
      WinProbBefore = 0.5; WinProbAfter = 0.5 - wpLoss
      WinProbLoss = wpLoss; CentipawnLoss = cpLoss
      MoveAccuracy = calculateMoveAccuracy 0.065 wpLoss
      HasEvalGap = false
      Depth = 20; Nodes = 100000L; PV = "" }

/// Neutral WP array (50% for all moves) for tests where volatility is irrelevant
let private neutralWP n = Array.create n 0.5

[<Fact>]
let ``computePlayerStats calculates accuracy excluding Book and Forced`` () =
    let moves = [|
        mkMove 0 1 "w" "e4" Book 0.0 0.0
        mkMove 2 2 "w" "Nf3" Best 0.0 0.0
        mkMove 4 3 "w" "Bb5" Excellent 0.01 5.0
        mkMove 6 4 "w" "Kh1" Forced 0.0 0.0
    |]
    let stats = computePlayerStats (neutralWP moves.Length) "White" moves "w"
    // Only Best and Excellent are classifiable (not Book/Forced)
    Assert.Equal(4, stats.MoveCount)
    Assert.True(stats.Accuracy > 90.0, sprintf "Expected accuracy > 90 but got %f" stats.Accuracy)

[<Fact>]
let ``computePlayerStats returns empty stats for no moves`` () =
    let stats = computePlayerStats [||] "White" [||] "w"
    Assert.Equal(0, stats.MoveCount)
    Assert.Equal(0.0, stats.Accuracy)
    Assert.Equal(0.0, stats.ACPL)

[<Fact>]
let ``computePlayerStats correctly filters by color`` () =
    let moves = [|
        mkMove 0 1 "w" "e4" Best 0.0 0.0
        mkMove 1 1 "b" "e5" Blunder 0.25 100.0
    |]
    let whiteStats = computePlayerStats (neutralWP moves.Length) "White" moves "w"
    let blackStats = computePlayerStats (neutralWP moves.Length) "Black" moves "b"
    Assert.Equal(1, whiteStats.MoveCount)
    Assert.Equal(1, blackStats.MoveCount)
    Assert.True(whiteStats.Accuracy > blackStats.Accuracy)

[<Fact>]
let ``computePlayerStats counts classifications`` () =
    let moves = [|
        mkMove 0 1 "w" "e4" Best 0.0 0.0
        mkMove 2 2 "w" "d4" Best 0.0 0.0
        mkMove 4 3 "w" "Nf3" Inaccuracy 0.07 30.0
        mkMove 6 4 "w" "Be2" Blunder 0.25 100.0
    |]
    let stats = computePlayerStats (neutralWP moves.Length) "White" moves "w"
    Assert.Equal(2, stats.Classifications |> Map.find Best)
    Assert.Equal(1, stats.Classifications |> Map.find Inaccuracy)
    Assert.Equal(1, stats.Classifications |> Map.find Blunder)

[<Fact>]
let ``computePlayerStats phase breakdown uses move numbers`` () =
    let moves = [|
        mkMove 0 5 "w" "e4" Best 0.0 0.0        // Opening (1-15)
        mkMove 2 10 "w" "d4" Best 0.0 0.0       // Opening
        mkMove 4 20 "w" "Nf3" Mistake 0.15 60.0 // Middlegame (16-40)
        mkMove 6 45 "w" "Kh1" Blunder 0.25 100.0 // Endgame (41+)
    |]
    let stats = computePlayerStats (neutralWP moves.Length) "White" moves "w"
    Assert.True(stats.OpeningAccuracy > stats.MiddlegameAccuracy)
    Assert.True(stats.MiddlegameAccuracy > stats.EndgameAccuracy)

// ──────────────────────────────────────────────────────────────
// Harmonic mean + volatility aggregation tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``harmonic mean penalizes bad moves more than arithmetic mean`` () =
    // Mix of perfect and terrible moves
    let moves = [|
        mkMove 0 1 "w" "e4" Best 0.0 0.0        // ~100%
        mkMove 2 2 "w" "d4" Best 0.0 0.0        // ~100%
        mkMove 4 3 "w" "Nf3" Best 0.0 0.0       // ~100%
        mkMove 6 4 "w" "Be2" Blunder 0.30 120.0 // very low
    |]
    let stats = computePlayerStats (neutralWP moves.Length) "White" moves "w"
    // Arithmetic mean would be ~82. Harmonic mean drags it down further.
    Assert.True(stats.Accuracy < 80.0, sprintf "Expected accuracy < 80 with harmonic mean but got %f" stats.Accuracy)

[<Fact>]
let ``volatility weighting emphasizes sharp positions`` () =
    // All white moves: 2 good in quiet, 1 bad in sharp
    let moves = [|
        mkMove 0 1 "w" "e4" Best 0.0 0.0
        mkMove 2 2 "w" "d4" Best 0.0 0.0
        mkMove 4 3 "w" "Nf3" Mistake 0.15 60.0
    |]
    // Quiet WP (stable) — mistake should have less volatility weight
    let quietWP = [| 0.55; 0.55; 0.55 |]
    let statsQuiet = computePlayerStats quietWP "White" moves "w"
    // Volatile WP (swinging) — mistake gets more volatility weight
    let volatileWP = [| 0.3; 0.7; 0.3 |]
    let statsVolatile = computePlayerStats volatileWP "White" moves "w"
    // In volatile game, the bad move is weighted more → lower accuracy
    Assert.True(statsVolatile.Accuracy <= statsQuiet.Accuracy,
        sprintf "Volatile %f should be <= quiet %f" statsVolatile.Accuracy statsQuiet.Accuracy)

// ──────────────────────────────────────────────────────────────
// countMaterial tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``countMaterial correct for starting position`` () =
    let w, b = countMaterial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.Equal(w, b)  // Equal material
    // Q=9 + 2R=10 + 2B=6 + 2N=6 + 8P=8 = 39
    Assert.Equal(39, w)

[<Fact>]
let ``countMaterial detects material imbalance`` () =
    // White missing a queen
    let w, b = countMaterial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w KQkq - 0 1"
    Assert.Equal(30, w)  // 39 - 9
    Assert.Equal(39, b)

// ──────────────────────────────────────────────────────────────
// MoveClassification.ToString tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``MoveClassification ToString returns expected symbols`` () =
    Assert.Equal("!!", Brilliant.ToString())
    Assert.Equal("!", Great.ToString())
    Assert.Equal("Best", Best.ToString())
    Assert.Equal("?!", Inaccuracy.ToString())
    Assert.Equal("?", Mistake.ToString())
    Assert.Equal("??", Blunder.ToString())

// ──────────────────────────────────────────────────────────────
// analyzeGameFromAnnotations tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``analyzeGameFromAnnotations returns None for empty game`` () =
    let game = ChessLibrary.PGNTypes.PgnGame.Empty 1
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default 0.065 game
    Assert.True(result.IsNone)

[<Fact>]
let ``analyzeGameFromAnnotations returns None for game with no eval annotations`` () =
    let game = ChessLibrary.PGNTypes.PgnGame.Empty 1
    game.Mainline.Add({ Ply = 0; MoveNumber = 1; Color = "w"; San = "e4"; Comment = ""; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 1; MoveNumber = 1; Color = "b"; San = "e5"; Comment = ""; Nags = []; Variations = ResizeArray() })
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default 0.065 game
    Assert.True(result.IsNone)

[<Fact>]
let ``analyzeGameFromAnnotations processes game with eval annotations`` () =
    let meta = { ChessLibrary.PGNTypes.GameMetadata.Empty with White = "Player1"; Black = "Player2"; Result = "1-0" }
    let game = { ChessLibrary.PGNTypes.PgnGame.Empty 1 with GameMetaData = meta }
    game.Mainline.Add({ Ply = 0; MoveNumber = 1; Color = "w"; San = "e4"; Comment = "wv=0.20, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 1; MoveNumber = 1; Color = "b"; San = "e5"; Comment = "wv=0.15, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 2; MoveNumber = 2; Color = "w"; San = "Nf3"; Comment = "wv=0.30, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default 0.065 game
    Assert.True(result.IsSome)
    let r = result.Value
    Assert.Equal(3, r.Moves.Length)
    Assert.Equal("Player1", r.WhitePlayer)
    Assert.Equal("Player2", r.BlackPlayer)
    Assert.Equal("PGN Annotations", r.AnalysisEngine)
    // All moves should have non-negative WP loss
    for m in r.Moves do
        Assert.True(m.WinProbLoss >= 0.0, sprintf "Move %s WP loss should be >= 0 but got %f" m.San m.WinProbLoss)
        Assert.True(m.MoveAccuracy >= 0.0 && m.MoveAccuracy <= 100.0)

[<Fact>]
let ``analyzeGameFromAnnotations reads lichess eval commands`` () =
    // A lichess-annotated game used to look identical to a game with no evals at all, so
    // Quick Analysis refused it while the move list was already showing its evals.
    let pgn =
        "[Event \"t\"]\n[White \"W\"]\n[Black \"B\"]\n[Result \"*\"]\n\n\
         1. e4 { [%eval 0.28] } 1... e5 { [%eval -1.5] } \
         2. Nf3 { [%eval 0.9] } 2... Nc6 { [%eval #3] } *\n"
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".pgn")
    try
        System.IO.File.WriteAllText(path, pgn)
        let game = ChessLibrary.FullPGNParser.parsePgnFile path |> Seq.head
        match analyzeGameFromAnnotations ClassificationThresholds.Default 0.065 game with
        | None -> failwith "expected the lichess evals to be found"
        | Some result -> Assert.Equal(4, result.Moves.Length)
    finally
        System.IO.File.Delete path
