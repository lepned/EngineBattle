module PuzzleLossMetricTests

open System
open Xunit
open ChessLibrary.PuzzleEngineAgent
open ChessLibrary.EngineTypes

// ---------------------------------------------------------------------------
// Helper: build NNValues with just LANMove, P, Q
// ---------------------------------------------------------------------------

let private mkNN (lanMove: string) (p: float) (q: float) : NNValues =
    { NNValues.Empty with LANMove = lanMove; P = p; Q = q }

// ---------------------------------------------------------------------------
// findEngineRank
// ---------------------------------------------------------------------------

[<Fact>]
let ``findEngineRank returns 1 for top move`` () =
    let nn = [ mkNN "e2e4" 60.0 0.5; mkNN "d2d4" 30.0 0.4; mkNN "c2c4" 10.0 0.3 ]
    Assert.Equal(1, findEngineRank nn "e2e4")

[<Fact>]
let ``findEngineRank returns correct rank for lower move`` () =
    let nn = [ mkNN "e2e4" 60.0 0.5; mkNN "d2d4" 30.0 0.4; mkNN "c2c4" 10.0 0.3 ]
    Assert.Equal(3, findEngineRank nn "c2c4")

[<Fact>]
let ``findEngineRank returns 0 when move not found`` () =
    let nn = [ mkNN "e2e4" 60.0 0.5; mkNN "d2d4" 30.0 0.4 ]
    Assert.Equal(0, findEngineRank nn "g1f3")

[<Fact>]
let ``findEngineRank returns 0 for empty list`` () =
    Assert.Equal(0, findEngineRank [] "e2e4")

// ---------------------------------------------------------------------------
// computeRankWeightedKld
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeRankWeightedKld weights by 1 over rank`` () =
    // rank 1: kld=0.5, weight=1.0 → 0.5
    // rank 2: kld=1.0, weight=0.5 → 0.5
    // weighted avg = (0.5 + 0.5) / (1.0 + 0.5) = 1.0 / 1.5 = 0.6667
    let items = [ (0.5, 1); (1.0, 2) ]
    let result = computeRankWeightedKld items
    Assert.Equal(0.6667, result, 3)

[<Fact>]
let ``computeRankWeightedKld includes rank 0 as beyond-N`` () =
    // rank 0 is treated as effective rank 30 (beyond top-N).
    // rank 1: kld=0.5, weight=1.0     → 0.5
    // rank 0: kld=9.0, weight=1/30    → 0.3
    // rank 2: kld=1.0, weight=0.5     → 0.5
    // weighted avg = (0.5 + 0.3 + 0.5) / (1.0 + 1.0/30 + 0.5)
    //              = 1.3 / (1.5 + 0.0333)
    //              = 1.3 / 1.5333 ≈ 0.8478
    let items = [ (0.5, 1); (9.0, 0); (1.0, 2) ]
    let result = computeRankWeightedKld items
    Assert.Equal(0.8478, result, 3)

[<Fact>]
let ``computeRankWeightedKld rank 0 alone uses beyond-N weight`` () =
    // Only a rank-0 puzzle: weighted avg = kld (self), regardless of weight
    let items = [ (5.0, 0) ]
    let result = computeRankWeightedKld items
    Assert.Equal(5.0, result, 6)

[<Fact>]
let ``computeRankWeightedKld returns 0 for empty`` () =
    Assert.Equal(0.0, computeRankWeightedKld [])

// ---------------------------------------------------------------------------
// computeFrontierWeightedKld
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeFrontierWeightedKld peaks at rank 2`` () =
    // rank 1: kld=0.5, weight=0.3 → 0.15
    // rank 2: kld=2.0, weight=1.0 → 2.00
    // weighted avg = 2.15 / 1.3 = 1.6538
    let items = [ (0.5, 1); (2.0, 2) ]
    let result = computeFrontierWeightedKld items
    Assert.Equal(1.6538, result, 3)

[<Fact>]
let ``computeFrontierWeightedKld downweights deep ranks`` () =
    // rank 1: kld=0.5, weight=0.3 → 0.15
    // rank 15: kld=5.0, weight=0.02 → 0.10
    // weighted avg = 0.25 / 0.32 = 0.78125
    let items = [ (0.5, 1); (5.0, 15) ]
    let result = computeFrontierWeightedKld items
    Assert.Equal(0.7813, result, 3)

[<Fact>]
let ``computeFrontierWeightedKld includes rank 0 with deep-rank weight`` () =
    // rank 0 is treated the same as rank 11+ (weight 0.02).
    // rank 2: kld=0.5, weight=1.0 → 0.5
    // rank 0: kld=9.0, weight=0.02 → 0.18
    // weighted avg = 0.68 / 1.02 = 0.6667
    let items = [ (0.5, 2); (9.0, 0) ]
    let result = computeFrontierWeightedKld items
    Assert.Equal(0.6667, result, 3)

[<Fact>]
let ``computeFrontierWeightedKld rank 0 alone uses deep-rank weight`` () =
    // Only a rank-0 puzzle: weighted avg = kld (self), regardless of weight
    let items = [ (9.0, 0) ]
    let result = computeFrontierWeightedKld items
    Assert.Equal(9.0, result, 6)

[<Fact>]
let ``computeFrontierWeightedKld returns 0 for empty`` () =
    Assert.Equal(0.0, computeFrontierWeightedKld [])

// ---------------------------------------------------------------------------
// computeWeightedMarginLoss
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeWeightedMarginLoss all solved degenerates to uniform mean`` () =
    // All solved: w=1 for all, equivalent to plain average.
    // mean([0.1; 0.2; 0.3]) = 0.2
    let items = [ (0.1, true); (0.2, true); (0.3, true) ]
    let result = computeWeightedMarginLoss items 2.0
    Assert.Equal(0.2, result, 6)

[<Fact>]
let ``computeWeightedMarginLoss all unsolved degenerates to uniform mean`` () =
    // All unsolved: w=alpha cancels out in num/den, equivalent to plain average.
    // mean([1.0; 1.5; 2.0]) = 1.5
    let items = [ (1.0, false); (1.5, false); (2.0, false) ]
    let result = computeWeightedMarginLoss items 2.0
    Assert.Equal(1.5, result, 6)

[<Fact>]
let ``computeWeightedMarginLoss mixed applies 2x weight to unsolved`` () =
    // 3 solved (w=1) at margin 0.2, 2 unsolved (w=2) at margin 1.5.
    // num = 3*1*0.2 + 2*2*1.5 = 0.6 + 6.0 = 6.6
    // den = 3*1 + 2*2 = 3 + 4 = 7
    // weighted_margin = 6.6 / 7 = 0.9428571...
    let items = [
        (0.2, true); (0.2, true); (0.2, true)
        (1.5, false); (1.5, false)
    ]
    let result = computeWeightedMarginLoss items 2.0
    Assert.Equal(0.9429, result, 4)

[<Fact>]
let ``computeWeightedMarginLoss empty returns 0`` () =
    Assert.Equal(0.0, computeWeightedMarginLoss [] 2.0)

[<Fact>]
let ``computeWeightedMarginLoss alpha 1 equals uniform mean`` () =
    // alpha=1 reduces to uniform average regardless of solved status.
    // mean([0.2; 1.5; 0.5; 1.0]) = 0.8
    let items = [ (0.2, true); (1.5, false); (0.5, true); (1.0, false) ]
    let result = computeWeightedMarginLoss items 1.0
    Assert.Equal(0.8, result, 6)

// ---------------------------------------------------------------------------
// computeMarginLoss
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeMarginLoss returns near 0 when correct dominates`` () =
    // P_correct=90, P_competitor=5 → -log(90/(90+5)) = -log(0.9474) ≈ 0.054
    let nn = [ mkNN "e2e4" 90.0 0.0; mkNN "d2d4" 5.0 0.0; mkNN "c2c4" 5.0 0.0 ]
    let result = computeMarginLoss nn "e2e4"
    Assert.True(result < 0.1, sprintf "Expected < 0.1, got %f" result)

[<Fact>]
let ``computeMarginLoss returns log2 when equal`` () =
    // P_correct=40, P_competitor=40 → -log(40/(40+40)) = -log(0.5) = log(2) ≈ 0.693
    let nn = [ mkNN "e2e4" 40.0 0.0; mkNN "d2d4" 40.0 0.0 ]
    let result = computeMarginLoss nn "d2d4"
    Assert.Equal(log 2.0, result, 3)

[<Fact>]
let ``computeMarginLoss returns large value when competitor dominates`` () =
    // P_correct=5, P_competitor=80 → -log(5/(5+80)) = -log(0.0588) ≈ 2.833
    let nn = [ mkNN "e2e4" 80.0 0.0; mkNN "d2d4" 15.0 0.0; mkNN "c2c4" 5.0 0.0 ]
    let result = computeMarginLoss nn "c2c4"
    Assert.True(result > 2.0, sprintf "Expected > 2.0, got %f" result)

[<Fact>]
let ``computeMarginLoss returns 0 for single move`` () =
    let nn = [ mkNN "e2e4" 100.0 0.0 ]
    Assert.Equal(0.0, computeMarginLoss nn "e2e4")

// ---------------------------------------------------------------------------
// computeEstNodesToFind
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeEstNodesToFind returns 0 for rank 1`` () =
    let nn = [ mkNN "e2e4" 60.0 0.0; mkNN "d2d4" 30.0 0.0; mkNN "c2c4" 10.0 0.0 ]
    Assert.Equal(0.0, computeEstNodesToFind nn "e2e4")

[<Fact>]
let ``computeEstNodesToFind returns 0 for empty list`` () =
    Assert.Equal(0.0, computeEstNodesToFind [] "e2e4")

[<Fact>]
let ``computeEstNodesToFind matches hand-computed value`` () =
    // Correct move c2c4 at rank 3: P=10% → 0.10, sumHigher=90% → 0.9
    // N = (0.98416 * sqrt(0.9) / (2.897 * 0.10))² ≈ 10.3867
    let nn = [ mkNN "e2e4" 60.0 0.0; mkNN "d2d4" 30.0 0.0; mkNN "c2c4" 10.0 0.0 ]
    let result = computeEstNodesToFind nn "c2c4"
    Assert.Equal(10.3867, result, 4)

[<Fact>]
let ``computeEstNodesToFind converts percent to fraction`` () =
    // Correct move at rank 2 with equal split: P=0.5, sumHigher=0.5
    // N = (0.98416 * sqrt(0.5) / (2.897 * 0.5))² ≈ 0.2308 — found near-immediately
    let nn = [ mkNN "e2e4" 50.0 0.0; mkNN "d2d4" 50.0 0.0 ]
    let result = computeEstNodesToFind nn "d2d4"
    Assert.Equal(0.2308, result, 4)

[<Fact>]
let ``computeEstNodesToFind uses floor when correct move missing`` () =
    // Missing move → P floored at 0.01% with all listed mass counted as higher:
    // N = (0.98416 * sqrt(1.0) / (2.897 * 0.0001))² ≈ 1.154e7
    let nn = [ mkNN "e2e4" 60.0 0.0; mkNN "d2d4" 40.0 0.0 ]
    let result = computeEstNodesToFind nn "g1f3"
    Assert.True(result > 1.1e7 && result < 1.2e7, sprintf "Expected ~1.15e7, got %f" result)

[<Fact>]
let ``computeEstNodesToFind treats zero P as missing`` () =
    let nnZero = [ mkNN "e2e4" 99.0 0.0; mkNN "c2c4" 0.0 0.0 ]
    let nnMissing = [ mkNN "e2e4" 99.0 0.0 ]
    // P=0 takes the same floor path; both count all listed mass (99%) as higher.
    Assert.Equal(computeEstNodesToFind nnMissing "c2c4", computeEstNodesToFind nnZero "c2c4", 6)

[<Fact>]
let ``computeEstNodesToFind grows as correct move sinks in the ranking`` () =
    let rank2 = [ mkNN "e2e4" 50.0 0.0; mkNN "d2d4" 25.0 0.0; mkNN "c2c4" 25.0 0.0 ]
    let rank3 = [ mkNN "e2e4" 50.0 0.0; mkNN "c2c4" 25.0 0.0; mkNN "d2d4" 25.0 0.0 ]
    Assert.True(computeEstNodesToFind rank3 "d2d4" > computeEstNodesToFind rank2 "d2d4")

// ---------------------------------------------------------------------------
// normalizeCastlingAliases
// ---------------------------------------------------------------------------

[<Fact>]
let ``normalizeCastlingAliases rewrites king-takes-rook to standard`` () =
    // Lc0 verbose stats say e1h1; lichess solution says e1g1.
    let nn = [ mkNN "e1h1" 27.8 0.0; mkNN "f3f7" 10.0 0.0 ]
    let result = normalizeCastlingAliases "e1g1" nn
    Assert.Equal("e1g1", result.[0].LANMove)
    Assert.Equal(27.8, result.[0].P, 6)

[<Fact>]
let ``normalizeCastlingAliases no-op when standard notation present`` () =
    // Rook move e1g1 genuinely in the list — alias must not fire on e1h1-less lists
    let nn = [ mkNN "e1g1" 5.0 0.0; mkNN "d2d4" 40.0 0.0 ]
    let result = normalizeCastlingAliases "e1g1" nn
    Assert.Equal<NNValues list>(nn, result)

[<Fact>]
let ``normalizeCastlingAliases no-op when both notations present`` () =
    // Pathological: rook on e1 can move to g1 AND castling shown as e1h1.
    // Standard notation exists in the list, so nothing is rewritten.
    let nn = [ mkNN "e1g1" 5.0 0.0; mkNN "e1h1" 20.0 0.0 ]
    let result = normalizeCastlingAliases "e1g1" nn
    Assert.Equal<NNValues list>(nn, result)

[<Fact>]
let ``normalizeCastlingAliases no-op for non-castling correct move`` () =
    let nn = [ mkNN "e1h1" 20.0 0.0; mkNN "d2d4" 40.0 0.0 ]
    let result = normalizeCastlingAliases "d2d4" nn
    Assert.Equal<NNValues list>(nn, result)

[<Fact>]
let ``normalizeCastlingAliases handles black queenside`` () =
    let nn = [ mkNN "e8a8" 15.0 0.0; mkNN "g8f6" 30.0 0.0 ]
    let result = normalizeCastlingAliases "e8c8" nn
    Assert.Equal("e8c8", result.[0].LANMove)

// ---------------------------------------------------------------------------
// fractionAtOrBelow
// ---------------------------------------------------------------------------

[<Fact>]
let ``fractionAtOrBelow returns 0 for empty sample`` () =
    Assert.Equal(0.0, fractionAtOrBelow 1000.0 [||])

[<Fact>]
let ``fractionAtOrBelow counts inclusive threshold`` () =
    // 0, 500, 1000 are <= 1000; 5000 is not → 3/4
    Assert.Equal(0.75, fractionAtOrBelow 1000.0 [| 0.0; 500.0; 1000.0; 5000.0 |], 6)

[<Fact>]
let ``fractionAtOrBelow returns 1 when all below`` () =
    Assert.Equal(1.0, fractionAtOrBelow 1000.0 [| 0.0; 1.0; 999.9 |], 6)

// ---------------------------------------------------------------------------
// percentile (nearest-rank)
// ---------------------------------------------------------------------------

[<Fact>]
let ``percentile returns 0 for empty sample`` () =
    Assert.Equal(0.0, percentile 95.0 [||])

[<Fact>]
let ``percentile returns max for p100`` () =
    Assert.Equal(9.0, percentile 100.0 [| 3.0; 9.0; 1.0 |])

[<Fact>]
let ``percentile nearest-rank on 10 values`` () =
    // Nearest-rank: p95 of n=10 → ceil(0.95*10)=10th value (the max);
    // p50 → ceil(0.5*10)=5th value.
    let values = [| 1.0 .. 10.0 |]
    Assert.Equal(10.0, percentile 95.0 values)
    Assert.Equal(5.0, percentile 50.0 values)

[<Fact>]
let ``percentile handles unsorted input`` () =
    let values = [| 50.0; 10.0; 40.0; 20.0; 30.0 |]
    // p60 of n=5 → ceil(0.6*5)=3rd sorted value = 30
    Assert.Equal(30.0, percentile 60.0 values)

[<Fact>]
let ``percentile p99 picks tail of larger sample`` () =
    // n=100 → ceil(0.99*100)=99th sorted value
    let values = [| 1.0 .. 100.0 |]
    Assert.Equal(99.0, percentile 99.0 values)

// ---------------------------------------------------------------------------
// computeValueLoss
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeValueLoss penalizes low Q on mate`` () =
    // Q=0.7, threshold=0.95 → max(0, 0.95-0.7)² = 0.0625
    let nn = [ mkNN "e2e4" 60.0 0.7 ]
    let result = computeValueLoss nn "e2e4" "mateIn2 middlegame" true
    Assert.Equal(0.0625, result, 6)

[<Fact>]
let ``computeValueLoss zero for high Q on mate`` () =
    // Q=0.97, threshold=0.95 → max(0, 0.95-0.97)² = 0
    let nn = [ mkNN "e2e4" 60.0 0.97 ]
    Assert.Equal(0.0, computeValueLoss nn "e2e4" "mate" true)

[<Fact>]
let ``computeValueLoss penalizes low Q on crushing`` () =
    // Q=0.3, threshold=0.85 → max(0, 0.85-0.3)² = 0.3025
    let nn = [ mkNN "e2e4" 60.0 0.3 ]
    let result = computeValueLoss nn "e2e4" "crushing hangingPiece" true
    Assert.Equal(0.3025, result, 6)

[<Fact>]
let ``computeValueLoss zero for high Q on crushing`` () =
    // Q=0.9, threshold=0.85 → max(0, 0.85-0.9)² = 0
    let nn = [ mkNN "e2e4" 60.0 0.9 ]
    Assert.Equal(0.0, computeValueLoss nn "e2e4" "crushing" true)

[<Fact>]
let ``computeValueLoss penalizes high Q on equality`` () =
    // Q=0.6, equality → max(0, |0.6|-0.3)² = 0.09
    let nn = [ mkNN "e2e4" 60.0 0.6 ]
    let result = computeValueLoss nn "e2e4" "defensiveMove equality" true
    Assert.Equal(0.09, result, 6)

[<Fact>]
let ``computeValueLoss zero for near-zero Q on equality`` () =
    // Q=0.1, equality → max(0, |0.1|-0.3)² = 0 (within ±0.3 zone)
    let nn = [ mkNN "e2e4" 60.0 0.1 ]
    Assert.Equal(0.0, computeValueLoss nn "e2e4" "equality" true)

[<Fact>]
let ``computeValueLoss zero for high Q on advantage`` () =
    // Q=0.9, threshold=0.85 → max(0, 0.85-0.9)² = 0 (above threshold)
    let nn = [ mkNN "e2e4" 60.0 0.9 ]
    Assert.Equal(0.0, computeValueLoss nn "e2e4" "advantage endgame" true)

[<Fact>]
let ``computeValueLoss penalizes low Q on advantage`` () =
    // Q=0.5, threshold=0.85 → max(0, 0.85-0.5)² = 0.1225
    let nn = [ mkNN "e2e4" 60.0 0.5 ]
    let result = computeValueLoss nn "e2e4" "advantage endgame" true
    Assert.Equal(0.1225, result, 6)

[<Fact>]
let ``computeValueLoss treats untagged as winning threshold 0.85`` () =
    // Q=0.7, no winning/equality theme → threshold=0.85 → max(0, 0.85-0.7)² = 0.0225
    let nn = [ mkNN "e2e4" 60.0 0.7 ]
    let result = computeValueLoss nn "e2e4" "middlegame" true
    Assert.Equal(0.0225, result, 6)

[<Fact>]
let ``computeValueLoss returns sentinel for unsolved`` () =
    let nn = [ mkNN "e2e4" 60.0 0.8 ]
    Assert.Equal(-1.0, computeValueLoss nn "e2e4" "crushing" false)

[<Fact>]
let ``computeValueLoss returns sentinel for empty list`` () =
    Assert.Equal(-1.0, computeValueLoss [] "e2e4" "crushing" true)

[<Fact>]
let ``computeValueLoss quadratic scaling for big miss`` () =
    // Q=0.1, mate threshold=0.95 → max(0, 0.95-0.1)² = 0.7225 (big penalty)
    let nn = [ mkNN "e2e4" 60.0 0.1 ]
    let result = computeValueLoss nn "e2e4" "mate" true
    Assert.Equal(0.7225, result, 6)

