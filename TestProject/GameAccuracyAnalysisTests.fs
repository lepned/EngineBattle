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
    let acc = calculateMoveAccuracy 0.0
    Assert.Equal(100.0, acc, 0)

[<Fact>]
let ``calculateMoveAccuracy returns near 0 for large WP loss`` () =
    let acc = calculateMoveAccuracy 0.50  // 50% WP loss
    Assert.True(acc < 15.0, sprintf "Expected acc < 15 but got %f" acc)

[<Fact>]
let ``calculateMoveAccuracy is monotonically decreasing`` () =
    let losses = [| 0.0; 0.02; 0.05; 0.10; 0.20; 0.30; 0.50 |]
    let accuracies = losses |> Array.map calculateMoveAccuracy
    for i in 1 .. accuracies.Length - 1 do
        Assert.True(accuracies.[i] <= accuracies.[i-1],
            sprintf "Expected accuracy to decrease: %f <= %f at index %d" accuracies.[i] accuracies.[i-1] i)

[<Fact>]
let ``calculateMoveAccuracy is clamped between 0 and 100`` () =
    Assert.True(calculateMoveAccuracy 0.0 <= 100.0)
    Assert.True(calculateMoveAccuracy 0.0 >= 0.0)
    Assert.True(calculateMoveAccuracy 1.0 >= 0.0)
    Assert.True(calculateMoveAccuracy 1.0 <= 100.0)

// ──────────────────────────────────────────────────────────────
// classifyMove tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``classifyMove returns Forced when isForced`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.30 false true None false
    Assert.Equal(Forced, cls)

[<Fact>]
let ``classifyMove returns Best for best move without gap`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.0 true false None false
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
let ``classifyMove returns Good for 2-5 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.03 false false None false
    Assert.Equal(Good, cls)

[<Fact>]
let ``classifyMove returns Inaccuracy for 5-10 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.07 false false None false
    Assert.Equal(Inaccuracy, cls)

[<Fact>]
let ``classifyMove returns Mistake for 10-20 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.15 false false None false
    Assert.Equal(Mistake, cls)

[<Fact>]
let ``classifyMove returns Blunder for > 20 pct WP loss`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.25 false false None false
    Assert.Equal(Blunder, cls)

// ──────────────────────────────────────────────────────────────
// Classification boundary tests
// ──────────────────────────────────────────────────────────────

[<Fact>]
let ``classifyMove at exact boundary 0.02 returns Good not Excellent`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.02 false false None false
    Assert.Equal(Good, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.05 returns Inaccuracy not Good`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.05 false false None false
    Assert.Equal(Inaccuracy, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.10 returns Mistake not Inaccuracy`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.10 false false None false
    Assert.Equal(Mistake, cls)

[<Fact>]
let ``classifyMove at exact boundary 0.20 returns Blunder not Mistake`` () =
    let cls = classifyMove ClassificationThresholds.Default 0.20 false false None false
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
      MoveAccuracy = calculateMoveAccuracy wpLoss
      Depth = 20; Nodes = 100000L; PV = "" }

[<Fact>]
let ``computePlayerStats calculates accuracy excluding Book and Forced`` () =
    let moves = [|
        mkMove 0 1 "w" "e4" Book 0.0 0.0
        mkMove 2 2 "w" "Nf3" Best 0.0 0.0
        mkMove 4 3 "w" "Bb5" Excellent 0.01 5.0
        mkMove 6 4 "w" "Kh1" Forced 0.0 0.0
    |]
    let stats = computePlayerStats "White" moves "w"
    // Only Best and Excellent are classifiable (not Book/Forced)
    Assert.Equal(4, stats.MoveCount)
    Assert.True(stats.Accuracy > 95.0, sprintf "Expected accuracy > 95 but got %f" stats.Accuracy)

[<Fact>]
let ``computePlayerStats returns empty stats for no moves`` () =
    let stats = computePlayerStats "White" [||] "w"
    Assert.Equal(0, stats.MoveCount)
    Assert.Equal(0.0, stats.Accuracy)
    Assert.Equal(0.0, stats.ACPL)

[<Fact>]
let ``computePlayerStats correctly filters by color`` () =
    let moves = [|
        mkMove 0 1 "w" "e4" Best 0.0 0.0
        mkMove 1 1 "b" "e5" Blunder 0.25 100.0
    |]
    let whiteStats = computePlayerStats "White" moves "w"
    let blackStats = computePlayerStats "Black" moves "b"
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
    let stats = computePlayerStats "White" moves "w"
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
    let stats = computePlayerStats "White" moves "w"
    Assert.True(stats.OpeningAccuracy > stats.MiddlegameAccuracy)
    Assert.True(stats.MiddlegameAccuracy > stats.EndgameAccuracy)

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
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default game
    Assert.True(result.IsNone)

[<Fact>]
let ``analyzeGameFromAnnotations returns None for game with no eval annotations`` () =
    let game = ChessLibrary.PGNTypes.PgnGame.Empty 1
    game.Mainline.Add({ Ply = 0; MoveNumber = 1; Color = "w"; San = "e4"; Comment = ""; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 1; MoveNumber = 1; Color = "b"; San = "e5"; Comment = ""; Nags = []; Variations = ResizeArray() })
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default game
    Assert.True(result.IsNone)

[<Fact>]
let ``analyzeGameFromAnnotations processes game with eval annotations`` () =
    let meta = { ChessLibrary.PGNTypes.GameMetadata.Empty with White = "Player1"; Black = "Player2"; Result = "1-0" }
    let game = { ChessLibrary.PGNTypes.PgnGame.Empty 1 with GameMetaData = meta }
    game.Mainline.Add({ Ply = 0; MoveNumber = 1; Color = "w"; San = "e4"; Comment = "wv=0.20, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 1; MoveNumber = 1; Color = "b"; San = "e5"; Comment = "wv=0.15, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    game.Mainline.Add({ Ply = 2; MoveNumber = 2; Color = "w"; San = "Nf3"; Comment = "wv=0.30, d=20, n=1000000"; Nags = []; Variations = ResizeArray() })
    let result = analyzeGameFromAnnotations ClassificationThresholds.Default game
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
