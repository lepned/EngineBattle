module PuzzleJsonOutputTests

open System
open System.IO
open System.Text.Json
open Xunit
open ChessLibrary.PuzzleTypes
open PuzzleJsonOutput

// ---------------------------------------------------------------------------
// Helpers for building hand-rolled Score values without spinning up an engine.
// ---------------------------------------------------------------------------

let private mkPlayer rating deviation volatility : PlayerRecord =
    { Rating = rating; Deviation = deviation; Volatility = volatility }

let private mkScore
    (engine: string) (net: string) (typ: string) (nodes: int)
    (correct: int) (total: int) (avgKLD: float) (playerRating: float) : Score =
    { Engine = engine
      NeuralNet = net
      TotalNumber = total
      Correct = correct
      Wrong = total - correct
      RatingAvg = 1900.0
      Filter = ""
      PlayerRecord = mkPlayer playerRating 50.0 0.06
      FailedPuzzles = ResizeArray<CsvPuzzleData * string>()
      CorrectPuzzles = ResizeArray<CsvPuzzleData>()
      Nodes = nodes
      WithHistory = false
      Type = typ
      AvgKLD = avgKLD
      AvgRankWeightedKld = 0.0
      AvgFrontierKld = 0.0
      AvgMarginLoss = 0.0
      AvgValueLoss = 0.0
      AvgEstNodesLog10 = 0.0
      EstNodesP95 = 0.0
      EstNodesP99 = 0.0
      EstNodesCdf100 = 0.0
      HardestByEstNodes = ResizeArray<CsvPuzzleData * float>() }

/// Helper that lets a test set both AvgKLD and AvgRankWeightedKld.
let private mkScoreWithRankWtKld
    (engine: string) (net: string) (typ: string) (nodes: int)
    (correct: int) (total: int) (avgKLD: float) (avgRankWtKld: float)
    (playerRating: float) : Score =
    { mkScore engine net typ nodes correct total avgKLD playerRating with
        AvgRankWeightedKld = avgRankWtKld }

let private fixedStartedUtc = DateTime(2026, 4, 8, 19, 23, 45, DateTimeKind.Utc)

// ---------------------------------------------------------------------------
// buildResult — pure transformation
// ---------------------------------------------------------------------------

[<Fact>]
let ``buildResult sets schemaVersion to 1`` () =
    let result =
        buildResult "puzzles.csv" 1000 500 0 3500 "" "" fixedStartedUtc 12.34 Seq.empty
    Assert.Equal(1, result.SchemaVersion)

[<Fact>]
let ``buildResult passes through metadata fields`` () =
    let result =
        buildResult "C:/p.csv" 9999 500 100 3500 "fork" "1500,2000" fixedStartedUtc 7.5 Seq.empty
    Assert.Equal("C:/p.csv", result.PuzzleFile)
    Assert.Equal(9999, result.TotalPuzzlesLoaded)
    Assert.Equal(500, result.SampleSize)
    Assert.Equal(100, result.MinRating)
    Assert.Equal(3500, result.MaxRating)
    Assert.Equal("fork", result.Filter)
    Assert.Equal("1500,2000", result.RatingGroups)
    Assert.Equal(7.5, result.ElapsedSeconds)
    Assert.Equal("2026-04-08T19:23:45.000Z", result.StartedUtc)

[<Fact>]
let ``buildResult with empty scores produces empty array`` () =
    let result =
        buildResult "p.csv" 0 0 0 0 "" "" fixedStartedUtc 0.0 Seq.empty
    Assert.NotNull(result.Scores)
    Assert.Empty(result.Scores)

[<Fact>]
let ``buildResult maps Score fields and computes accuracy`` () =
    let s = mkScore "Ceres" "C3-384" "Policy" 1 372 500 0.4127 2104.2
    let result =
        buildResult "p.csv" 1000 500 0 3500 "" "" fixedStartedUtc 1.0 [ s ]
    Assert.Single(result.Scores) |> ignore
    let entry = result.Scores.[0]
    Assert.Equal("Ceres", entry.Engine)
    Assert.Equal("C3-384", entry.NeuralNet)
    Assert.Equal("Policy", entry.Type)
    Assert.Equal(1, entry.Nodes)
    Assert.Equal(500, entry.TotalNumber)
    Assert.Equal(372, entry.Correct)
    Assert.Equal(128, entry.Wrong)
    Assert.Equal(0.744, entry.Accuracy, 6)
    Assert.Equal(2104.2, entry.PlayerRating, 6)
    Assert.Equal(0.4127, entry.AvgKLD, 6)

[<Fact>]
let ``buildResult treats zero TotalNumber as accuracy zero (no NaN)`` () =
    let s = mkScore "Eng" "" "Policy" 1 0 0 0.0 0.0
    let result =
        buildResult "p.csv" 0 0 0 0 "" "" fixedStartedUtc 0.0 [ s ]
    Assert.Equal(0.0, result.Scores.[0].Accuracy)
    Assert.False(Double.IsNaN result.Scores.[0].Accuracy)

[<Fact>]
let ``buildResult clamps NaN and Infinity floats to zero`` () =
    let s =
        { mkScore "Eng" "" "Policy" 1 1 1 0.0 0.0 with
            AvgKLD = Double.NaN
            AvgRankWeightedKld = Double.PositiveInfinity
            RatingAvg = Double.PositiveInfinity
            PlayerRecord = mkPlayer Double.NegativeInfinity Double.NaN Double.PositiveInfinity }
    let result =
        buildResult "p.csv" 0 0 0 0 "" "" fixedStartedUtc Double.NaN [ s ]
    let entry = result.Scores.[0]
    Assert.Equal(0.0, entry.AvgKLD)
    Assert.Equal(0.0, entry.AvgRankWeightedKld)
    Assert.Equal(0.0, entry.RatingAvg)
    Assert.Equal(0.0, entry.PlayerRating)
    Assert.Equal(0.0, entry.PlayerDeviation)
    Assert.Equal(0.0, entry.PlayerVolatility)
    Assert.Equal(0.0, result.ElapsedSeconds)

[<Fact>]
let ``buildResult preserves AvgRankWeightedKld through to entry`` () =
    let s =
        mkScoreWithRankWtKld "Ceres" "C3-384" "Policy" 1 372 500
            (* avgKLD *) 0.4127
            (* avgRankWtKld *) 0.3415
            (* playerRating *) 2104.2
    let result =
        buildResult "p.csv" 1000 500 0 3500 "" "" fixedStartedUtc 1.0 [ s ]
    let entry = result.Scores.[0]
    Assert.Equal(0.4127, entry.AvgKLD, 6)
    Assert.Equal(0.3415, entry.AvgRankWeightedKld, 6)

[<Fact>]
let ``serialize emits avgRankWeightedKld in camelCase JSON`` () =
    let s =
        mkScoreWithRankWtKld "Eng" "Net" "Policy" 1 4 5 0.123 0.087 2000.0
    let result =
        buildResult "p.csv" 100 5 0 3500 "" "" fixedStartedUtc 1.0 [ s ]
    let json = serialize result
    use doc = JsonDocument.Parse(json)
    let entry =
        doc.RootElement
            .GetProperty("scores").[0]
    Assert.Equal(0.123, entry.GetProperty("avgKLD").GetDouble(), 6)
    Assert.Equal(0.087, entry.GetProperty("avgRankWeightedKld").GetDouble(), 6)

[<Fact>]
let ``buildResult normalizes null strings to empty`` () =
    // Construct Score with null string fields. Score is a record so we use
    // a record-update over an empty Score.
    let s =
        { Score.empty with
            Engine = null
            NeuralNet = null
            Type = null
            Filter = null
            TotalNumber = 5
            Correct = 3 }
    let result =
        buildResult null 0 0 0 0 null null fixedStartedUtc 0.0 [ s ]
    Assert.Equal("", result.PuzzleFile)
    Assert.Equal("", result.Filter)
    Assert.Equal("", result.RatingGroups)
    let entry = result.Scores.[0]
    Assert.Equal("", entry.Engine)
    Assert.Equal("", entry.NeuralNet)
    Assert.Equal("", entry.Type)
    Assert.Equal("", entry.Filter)

[<Fact>]
let ``buildResult converts non-UTC startedUtc to UTC`` () =
    let local =
        DateTime(2026, 4, 8, 12, 0, 0, DateTimeKind.Local)
    let result =
        buildResult "p.csv" 0 0 0 0 "" "" local 0.0 Seq.empty
    Assert.EndsWith("Z", result.StartedUtc)

// ---------------------------------------------------------------------------
// serialize — JSON output shape
// ---------------------------------------------------------------------------

[<Fact>]
let ``serialize produces parseable JSON with camelCase keys`` () =
    let s = mkScore "Ceres" "C3" "Policy" 1 4 5 0.123 2000.0
    let result =
        buildResult "p.csv" 100 5 0 3500 "" "" fixedStartedUtc 1.0 [ s ]
    let json = serialize result
    use doc = JsonDocument.Parse(json)
    let root = doc.RootElement
    // Top-level keys
    Assert.Equal(1, root.GetProperty("schemaVersion").GetInt32())
    Assert.Equal("p.csv", root.GetProperty("puzzleFile").GetString())
    Assert.Equal(100, root.GetProperty("totalPuzzlesLoaded").GetInt32())
    // scores[0] keys (camelCase)
    let scores = root.GetProperty("scores")
    Assert.Equal(JsonValueKind.Array, scores.ValueKind)
    Assert.Equal(1, scores.GetArrayLength())
    let entry = scores.[0]
    Assert.Equal("Ceres", entry.GetProperty("engine").GetString())
    Assert.Equal("C3", entry.GetProperty("neuralNet").GetString())
    Assert.Equal(5, entry.GetProperty("totalNumber").GetInt32())
    Assert.Equal(4, entry.GetProperty("correct").GetInt32())
    Assert.Equal(1, entry.GetProperty("wrong").GetInt32())
    Assert.Equal(0.8, entry.GetProperty("accuracy").GetDouble(), 6)
    Assert.Equal(0.123, entry.GetProperty("avgKLD").GetDouble(), 6)

[<Fact>]
let ``serialize never throws on NaN or Infinity inputs`` () =
    let s =
        { Score.empty with
            Engine = "X"
            NeuralNet = ""
            Type = "Policy"
            TotalNumber = 1
            Correct = 1
            AvgKLD = Double.NaN
            RatingAvg = Double.PositiveInfinity
            PlayerRecord = mkPlayer Double.NaN Double.NaN Double.NegativeInfinity }
    let result =
        buildResult "p.csv" 0 0 0 0 "" "" fixedStartedUtc Double.PositiveInfinity [ s ]
    // The serializer would throw on raw NaN/Infinity; this should NOT throw.
    let json = serialize result
    Assert.False(String.IsNullOrEmpty json)
    use doc = JsonDocument.Parse(json)
    let avgKld =
        doc.RootElement
            .GetProperty("scores").[0]
            .GetProperty("avgKLD").GetDouble()
    Assert.Equal(0.0, avgKld)

// ---------------------------------------------------------------------------
// writeToFile — round trip
// ---------------------------------------------------------------------------

[<Fact>]
let ``writeToFile writes a parseable JSON file`` () =
    let s = mkScore "Eng" "Net" "Policy" 1 8 10 0.05 1800.0
    let result =
        buildResult "p.csv" 100 10 0 3500 "" "" fixedStartedUtc 0.5 [ s ]
    let path =
        Path.Combine(
            Path.GetTempPath(),
            sprintf "eb-puzzlejson-test-%s.json" (Guid.NewGuid().ToString("N")))
    try
        writeToFile path result
        Assert.True(File.Exists path)
        let text = File.ReadAllText path
        use doc = JsonDocument.Parse(text)
        let root = doc.RootElement
        Assert.Equal(1, root.GetProperty("schemaVersion").GetInt32())
        Assert.Equal(
            "Eng",
            root.GetProperty("scores").[0].GetProperty("engine").GetString())
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``writeToFile creates parent directory if missing`` () =
    let dir =
        Path.Combine(
            Path.GetTempPath(),
            sprintf "eb-puzzlejson-test-%s" (Guid.NewGuid().ToString("N")))
    let path = Path.Combine(dir, "nested", "out.json")
    try
        let result =
            buildResult "p.csv" 0 0 0 0 "" "" fixedStartedUtc 0.0 Seq.empty
        writeToFile path result
        Assert.True(File.Exists path)
    finally
        if Directory.Exists dir then Directory.Delete(dir, recursive = true)
