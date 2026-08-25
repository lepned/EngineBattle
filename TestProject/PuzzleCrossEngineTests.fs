module PuzzleCrossEngineTests

open Xunit
open ChessLibrary
open ChessLibrary.PuzzleTypes

// ---------------------------------------------------------------------------
// Regression cover for the grouping key. analyzeCrossEngine used to group on
// (Type, Nodes, Filter) only, while every other part of a run also slices by
// rating group. Consequences, both observed in real output:
//   * an engine appeared once per rating group in Engines, so every EPD section
//     was written twice, verbatim;
//   * solvedSets/failedSets are Map.ofList over those names, so the last rating
//     group silently overwrote the others and only one was ever analysed.
// ---------------------------------------------------------------------------

let private mkPuzzle (id: int) : CsvPuzzleData =
    CsvPuzzleData.Create(
        id, "8/8/8/8/8/8/8/K6k w - - 0 1", "a1a2", 2300.0, 80.0, 90, 100,
        "endgame", "https://lichess.org/x", "", null, null, null, 0)

let private mkScore (engine: string) (ratingAvg: float) (solved: int list) (failed: int list) : Score =
    { Engine = engine
      NeuralNet = engine + "-net"
      TotalNumber = solved.Length + failed.Length
      Correct = solved.Length
      Wrong = failed.Length
      RatingAvg = ratingAvg
      Filter = "none"
      PlayerRecord = { Rating = 2000.0; Deviation = 50.0; Volatility = 0.06 }
      FailedPuzzles = ResizeArray<CsvPuzzleData * string>(failed |> List.map (fun i -> mkPuzzle i, "e2e4,e7e5"))
      CorrectPuzzles = ResizeArray<CsvPuzzleData>(solved |> List.map mkPuzzle)
      Nodes = 1
      WithHistory = false
      Type = "Policy"
      AvgKLD = 0.0
      AvgRankWeightedKld = 0.0
      AvgFrontierKld = 0.0
      AvgMarginLoss = 0.0
      AvgValueLoss = 0.0
      AvgEstNodesLog10 = 0.0
      EstNodesP95 = 0.0
      EstNodesP99 = 0.0
      EstNodesCdf100 = 0.0
      HardestByEstNodes = ResizeArray<CsvPuzzleData * float>() }

[<Fact>]
let ``two rating groups produce two groups, not one merged group`` () =
    let scores =
        [ mkScore "A" 2299.0 [ 1; 2 ] [ 3 ]
          mkScore "B" 2299.0 [ 1; 3 ] [ 2 ]
          mkScore "A" 2498.0 [ 4 ] [ 5 ]
          mkScore "B" 2498.0 [ 5 ] [ 4 ] ]
    let results = PuzzleCrossEngine.analyzeCrossEngine scores
    Assert.Equal(2, results.Length)
    Assert.Equal<int list>(
        [ 2300; 2500 ],
        results |> List.map (fun r -> r.Group.RatingGroup) |> List.sort)

[<Fact>]
let ``each engine appears once per group, so sections are not duplicated`` () =
    let scores =
        [ mkScore "A" 2299.0 [ 1; 2 ] [ 3 ]
          mkScore "B" 2299.0 [ 1; 3 ] [ 2 ]
          mkScore "A" 2498.0 [ 4 ] [ 5 ]
          mkScore "B" 2498.0 [ 5 ] [ 4 ] ]
    for r in PuzzleCrossEngine.analyzeCrossEngine scores do
        Assert.Equal<string list>([ "A"; "B" ], r.Engines |> List.sort)

[<Fact>]
let ``a rating group keeps its own solved and failed sets`` () =
    // Under the old key the 2500 slice overwrote the 2300 one and this returned
    // the 2500 puzzles for both groups.
    let scores =
        [ mkScore "A" 2299.0 [ 1 ] [ 2 ]
          mkScore "B" 2299.0 [ 2 ] [ 1 ]
          mkScore "A" 2498.0 [ 8 ] [ 9 ]
          mkScore "B" 2498.0 [ 9 ] [ 8 ] ]
    let results = PuzzleCrossEngine.analyzeCrossEngine scores
    let g2300 = results |> List.find (fun r -> r.Group.RatingGroup = 2300)
    let ids =
        g2300.UniquelySolved
        |> Map.toList
        |> List.collect (fun (_, ps) -> ps |> List.map (fun p -> p.PuzzleId))
        |> List.sort
    Assert.Equal<int list>([ 1; 2 ], ids)

[<Fact>]
let ``a second row for one engine does not empty the unique-puzzle files`` () =
    // What `Type: "policy, policyvalue"` produces: two Scores for engine A in one
    // slice. engineNames was ["A"; "A"; "B"], so `filter (fun e -> e <> eng)` dropped
    // BOTH copies of A, othersAll came back empty, and every uniquely-solved and
    // uniquely-failed list was silently empty for the whole run.
    let scores =
        [ mkScore "A" 2299.0 [ 1; 2 ] [ 3 ]
          mkScore "A" 2299.0 [ 1; 3 ] [ 2 ]     // same engine, second test, same slice
          mkScore "B" 2299.0 [ 1 ] [ 2; 3 ] ]
    let r = PuzzleCrossEngine.analyzeCrossEngine scores |> List.exactlyOne
    Assert.Equal<string list>([ "A"; "B" ], r.Engines |> List.sort)
    let uniquelySolvedByA =
        r.UniquelySolved.["A"] |> List.map (fun p -> p.PuzzleId) |> List.sort
    Assert.Equal<int list>([ 2 ], uniquelySolvedByA)

[<Fact>]
let ``a single engine measured twice is still not a cross-engine group`` () =
    // Both rows are the same net, so there is nothing to compare - and the old code
    // passed the `engines.Length < 2` gate here and wrote every section twice.
    let scores =
        [ mkScore "A" 2299.0 [ 1; 2 ] [ 3 ]
          mkScore "A" 2299.0 [ 1; 3 ] [ 2 ] ]
    Assert.Empty(PuzzleCrossEngine.analyzeCrossEngine scores)

[<Fact>]
let ``a single engine in a group yields no cross-engine result`` () =
    Assert.Empty(PuzzleCrossEngine.analyzeCrossEngine [ mkScore "A" 2299.0 [ 1 ] [ 2 ] ])
