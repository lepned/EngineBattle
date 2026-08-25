module PuzzleReportTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.PuzzleTypes

// ---------------------------------------------------------------------------
// The report reader must stay in sync with the writer, so the round-trip tests
// build documents with PuzzleJsonOutput itself rather than hand-written JSON.
// ---------------------------------------------------------------------------

let private mkPlayer rating : PlayerRecord =
    { Rating = rating; Deviation = 50.0; Volatility = 0.06 }

let private mkScore (engine: string) (net: string) (typ: string) (correct: int) (total: int) : Score =
    { Engine = engine
      NeuralNet = net
      TotalNumber = total
      Correct = correct
      Wrong = total - correct
      RatingAvg = 2600.0
      Filter = ""
      PlayerRecord = mkPlayer 2650.0
      FailedPuzzles = ResizeArray<CsvPuzzleData * string>()
      CorrectPuzzles = ResizeArray<CsvPuzzleData>()
      Nodes = 1
      WithHistory = false
      Type = typ
      AvgKLD = 0.9
      AvgRankWeightedKld = 0.0
      AvgFrontierKld = 0.0
      AvgMarginLoss = 0.0
      AvgValueLoss = 0.0
      AvgEstNodesLog10 = 0.0
      EstNodesP95 = 0.0
      EstNodesP99 = 0.0
      EstNodesCdf100 = 0.0
      HardestByEstNodes = ResizeArray<CsvPuzzleData * float>() }

let private startedUtc = DateTime(2026, 8, 23, 18, 0, 0, DateTimeKind.Utc)

let private writeTempSummary (scores: Score seq) =
    let result =
        PuzzleJsonOutput.buildResult "puzzles.csv" 1000 500 0 3900 "" "2600" startedUtc 60.0 scores
    let path = Path.Combine(Path.GetTempPath(), sprintf "LichessSummary_test_%s.json" (Guid.NewGuid().ToString "N"))
    File.WriteAllText(path, PuzzleJsonOutput.serialize result)
    path

// ---------------------------------------------------------------------------
// Paired rows. The discordant counts cannot be recovered from the themes CSV,
// so if the reader loses them the report silently falls back to the wide sigma.
// ---------------------------------------------------------------------------

let private mkPuzzleForPair (id: int) : CsvPuzzleData =
    CsvPuzzleData.Create(
        id, "8/8/8/8/8/8/8/K6k w - - 0 1", "a1a2", 2600.0, 80.0, 90, 100,
        "endgame", "https://lichess.org/x", "", null, null, null, 0)

let private withPuzzles (solved: int list) (failed: int list) (s: Score) =
    { s with
        Correct = solved.Length
        Wrong = failed.Length
        TotalNumber = solved.Length + failed.Length
        CorrectPuzzles = ResizeArray<CsvPuzzleData>(solved |> List.map mkPuzzleForPair)
        FailedPuzzles =
            ResizeArray<CsvPuzzleData * string>(failed |> List.map (fun i -> mkPuzzleForPair i, "wrong")) }

[<Fact>]
let ``loadSummary round-trips the paired rows`` () =
    let a = mkScore "Eng A" "netA" "Policy" 0 0 |> withPuzzles [ 1; 2; 3 ] [ 4; 5 ]
    let b = mkScore "Eng B" "netB" "Policy" 0 0 |> withPuzzles [ 1; 2; 4; 5 ] [ 3 ]
    let path = writeTempSummary [ a; b ]
    try
        let doc = PuzzleReport.loadSummary path
        let row = doc.Paired |> Array.exactlyOne
        Assert.Equal("netA", row.NetA)
        Assert.Equal("netB", row.NetB)
        Assert.Equal(2600, row.RatingGroup)
        Assert.Equal(5, row.N)
        Assert.Equal(1, row.OnlyA)     // 3
        Assert.Equal(2, row.OnlyB)     // 4, 5
        Assert.Equal(3, row.Discordant)
        Assert.Equal(20.0, row.DeltaPp, 6)
        Assert.Equal(1.0 / sqrt 3.0, row.Z, 6)
        Assert.True(row.P > 0.0 && row.P <= 1.0)
    finally
        File.Delete path

[<Fact>]
let ``loadSummary yields no paired rows for a single-net run`` () =
    let path = writeTempSummary [ mkScore "Eng A" "netA" "Policy" 300 500 ]
    try Assert.Empty((PuzzleReport.loadSummary path).Paired)
    finally File.Delete path

[<Fact>]
let ``loadSummary treats a summary without a paired key as having none`` () =
    // Every file written before the field existed looks like this; the reader must
    // not throw, and the report must simply hide the section.
    let path = Path.Combine(Path.GetTempPath(), sprintf "LichessSummary_old_%s.json" (Guid.NewGuid().ToString "N"))
    File.WriteAllText(path, """{"schemaVersion":1,"puzzleFile":"p.csv","sampleSize":10,"scores":[]}""")
    try Assert.Empty((PuzzleReport.loadSummary path).Paired)
    finally File.Delete path

[<Fact>]
let ``loadSummary round-trips what PuzzleJsonOutput writes, estNodes included`` () =
    let hardest = ResizeArray<CsvPuzzleData * float>()
    hardest.Add(Unchecked.defaultof<CsvPuzzleData>, 6200.0)
    let score =
        { mkScore "Eng A" "netA" "Policy" 300 500 with
            AvgEstNodesLog10 = 1.2
            EstNodesP95 = 19.0
            EstNodesP99 = 227.0
            EstNodesCdf100 = 0.983
            HardestByEstNodes = hardest }
    let path = writeTempSummary [ score ]
    try
        let doc = PuzzleReport.loadSummary path
        Assert.Equal(500, doc.SampleSize)
        Assert.Equal("2600", doc.RatingGroups)
        let row = Assert.Single doc.Scores
        Assert.Equal("Eng A", row.Engine)
        Assert.Equal("netA", row.NeuralNet)
        Assert.Equal(0.6, row.Accuracy, 6)
        Assert.Equal(2650.0, row.PlayerRating, 6)
        Assert.Equal(19.0, row.EstNodesP95, 6)
        Assert.Equal(227.0, row.EstNodesP99, 6)
        Assert.Equal(6200.0, row.EstNodesMax, 6)
        Assert.Equal(0.983, row.EstNodesCdf100, 6)
    finally
        File.Delete path

[<Fact>]
let ``loadSummary reads pre-estNodes documents as zeros`` () =
    // A JSON without the estNodes properties at all — the shape older runs have.
    let path = Path.Combine(Path.GetTempPath(), sprintf "LichessSummary_old_%s.json" (Guid.NewGuid().ToString "N"))
    File.WriteAllText(path, """{"sampleSize":10,"ratingGroups":"2300","scores":[
        {"engine":"E","neuralNet":"n","type":"Policy","nodes":1,"totalNumber":10,
         "accuracy":0.5,"ratingAvg":2300.0,"playerRating":2300.0,"avgKLD":1.0}]}""")
    try
        let row = Assert.Single (PuzzleReport.loadSummary path).Scores
        Assert.Equal(0.0, row.EstNodesP95, 6)
        Assert.Equal(0.0, row.EstNodesMax, 6)
    finally
        File.Delete path

// ---------------------------------------------------------------------------
// themesPathFor
// ---------------------------------------------------------------------------

[<Fact>]
let ``themesPathFor pairs the CSV by stamp in the same folder`` () =
    let p = PuzzleReport.themesPathFor (Path.Combine("C:", "runs", "LichessSummary_2026-08-23_18-14.json"))
    Assert.Equal(Path.Combine("C:", "runs", "puzzleThemes_2026-08-23_18-14.csv"), p)

// ---------------------------------------------------------------------------
// loadThemes
// ---------------------------------------------------------------------------

let private writeTempCsv (lines: string list) =
    let path = Path.Combine(Path.GetTempPath(), sprintf "puzzleThemes_test_%s.csv" (Guid.NewGuid().ToString "N"))
    File.WriteAllLines(path, "type,rating_group,net_a,net_b,theme,n,accuracy_a_pct,accuracy_b_pct,delta_pp,sigma" :: lines)
    path

[<Fact>]
let ``loadThemes reads comparison rows`` () =
    let path = writeTempCsv [ "Policy,2600,netA,netB,fork,424,51.50,56.60,5.10,0.77" ]
    try
        let row = Assert.Single (PuzzleReport.loadThemes path)
        Assert.Equal("Policy", row.Type)
        Assert.Equal("2600", row.RatingGroup)
        Assert.Equal("netA", row.NetA)
        Assert.Equal("netB", row.NetB)
        Assert.Equal("fork", row.Theme)
        Assert.Equal(424, row.N)
        Assert.Equal(51.5, row.AccA, 6)
        Assert.Equal(56.6, row.AccB, 6)
    finally
        File.Delete path

[<Fact>]
let ``loadThemes tolerates single-net rows with blank comparison columns`` () =
    // The single-net writer leaves net_a / accuracy_a_pct / delta / sigma empty.
    let path = writeTempCsv [ "Policy,2600,,netB,fork,424,,56.60,," ]
    try
        let row = Assert.Single (PuzzleReport.loadThemes path)
        Assert.Equal("", row.NetA)
        Assert.True(Double.IsNaN row.AccA)
        Assert.Equal("netB", row.NetB)
        Assert.Equal(56.6, row.AccB, 6)
    finally
        File.Delete path

[<Fact>]
let ``loadThemes skips malformed lines instead of failing`` () =
    let path = writeTempCsv [ "garbage"; "Policy,2600,netA,netB,pin,notanumber,1,2,3,4"; "Policy,2600,netA,netB,pin,488,47.00,52.00,5.00,1.0" ]
    try
        let row = Assert.Single (PuzzleReport.loadThemes path)
        Assert.Equal("pin", row.Theme)
    finally
        File.Delete path

// ---------------------------------------------------------------------------
// dropNonTactical
// ---------------------------------------------------------------------------

let private themeRow typ rg theme n : PuzzleReport.ThemeRow =
    { Type = typ; RatingGroup = rg; Filter = ""; EngineA = "engA"; EngineB = "engB"; Nodes = Some 1
      NetA = "a"; NetB = "b"; Theme = theme; N = n; AccA = 50.0; AccB = 50.0 }

[<Fact>]
let ``dropNonTactical removes descriptive tags`` () =
    let rows = [| themeRow "Policy" "2600" "short" 500
                  themeRow "Policy" "2600" "crushing" 500
                  themeRow "Policy" "2600" "master" 500
                  themeRow "Policy" "2600" "fork" 500 |]
    let kept = PuzzleReport.dropNonTactical rows
    Assert.Equal<string[]>([| "fork" |], kept |> Array.map (fun r -> r.Theme))

// ---------------------------------------------------------------------------
// buildHeatView + buildProfiles, on a hand-computed fixture
// ---------------------------------------------------------------------------

let private cmpRow theme n accA accB : PuzzleReport.ThemeRow =
    { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = "engA"; EngineB = "engB"; Nodes = Some 1
      NetA = "netA"; NetB = "netB"; Theme = theme; N = n; AccA = accA; AccB = accB }

[<Fact>]
let ``buildHeatView computes raw and strength-removed residuals`` () =
    // netB is uniformly +10 on one theme and +2 on the other, so its offset is +3
    // ((5 + 1) / 2) and the spec residuals split symmetrically around it.
    let rows = [| cmpRow "fork" 500 40.0 50.0; cmpRow "pin" 500 60.0 62.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    Assert.Equal<string[]>([| "netA"; "netB" |], view.Nets)
    // columns easiest-first: pin (mean 61) before fork (mean 45)
    Assert.Equal<string[]>([| "pin"; "fork" |], view.Cols |> Array.map (fun c -> c.Theme))
    Assert.Equal(61.0, view.Cols.[0].Mean, 6)
    Assert.Equal(45.0, view.Cols.[1].Mean, 6)
    // raw: netA pin −1, fork −5; netB pin +1, fork +5
    Assert.Equal(-1.0, view.Raw.[0].[0], 6)
    Assert.Equal(-5.0, view.Raw.[0].[1], 6)
    Assert.Equal(1.0, view.Raw.[1].[0], 6)
    Assert.Equal(5.0, view.Raw.[1].[1], 6)
    // offsets −3 / +3, so spec: netA pin +2, fork −2 (mirror for netB)
    Assert.Equal(-3.0, view.Offsets.[0], 6)
    Assert.Equal(3.0, view.Offsets.[1], 6)
    Assert.Equal(2.0, view.Spec.[0].[0], 6)
    Assert.Equal(-2.0, view.Spec.[0].[1], 6)

[<Fact>]
let ``buildHeatView drops themes below the sample threshold`` () =
    let rows = [| cmpRow "fork" 500 40.0 50.0; cmpRow "skewer" 60 40.0 50.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    Assert.Equal<string[]>([| "fork" |], view.Cols |> Array.map (fun c -> c.Theme))

[<Fact>]
let ``buildHeatView single-net baseline is the net's own mean across themes`` () =
    let single theme n acc : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = "engB"; Nodes = Some 1
          NetA = ""; NetB = "netB"; Theme = theme; N = n; AccA = Double.NaN; AccB = acc }
    let rows = [| single "fork" 500 40.0; single "pin" 500 60.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netB" |]).Value
    let net = Assert.Single view.Nets
    Assert.Equal("netB", net)
    // own mean is 50, so pin +10 / fork −10, and raw = spec (offset 0)
    Assert.Equal(10.0, view.Raw.[0].[0], 6)
    Assert.Equal(-10.0, view.Raw.[0].[1], 6)
    Assert.Equal(0.0, view.Offsets.[0], 6)
    Assert.Equal(view.Raw.[0].[0], view.Spec.[0].[0], 6)

[<Fact>]
let ``buildProfiles picks signed extremes with binomial sigma`` () =
    let rows = [| cmpRow "fork" 400 40.0 50.0; cmpRow "pin" 400 60.0 62.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    let profs = PuzzleReport.buildProfiles view 400
    let a = profs |> Array.find (fun p -> p.Net = "netA")
    // netA spec: pin +2, fork −2 → best pin, worst fork
    Assert.Equal("pin", a.Best.Value.Theme)
    Assert.Equal(2.0, a.Best.Value.DeltaPp, 6)
    Assert.Equal("fork", a.Worst.Value.Theme)
    // sigma comes from PuzzleThemes.sigmaOf, which fixes p at 0.5:
    // |-2| / (sqrt(2*0.25/400)*100) = 2 / 3.5355 = 0.5657
    Assert.Equal(0.5657, a.Worst.Value.Sigma, 3)

[<Fact>]
let ``buildProfiles respects the callout threshold`` () =
    // pin is the extreme but sits under the callout n, so fork carries both sides
    let rows = [| cmpRow "fork" 500 40.0 50.0; cmpRow "pin" 300 10.0 90.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    let profs = PuzzleReport.buildProfiles view 400
    for p in profs do
        for e in [ p.Best; p.Worst ] do
            match e with
            | Some entry -> Assert.Equal("fork", entry.Theme)
            | None -> ()

[<Fact>]
let ``sigma does not blow up for a net at 0 or 100 percent on a theme`` () =
    // The per-net-accuracy SE this replaced collapsed to zero here, so a 2 pp
    // residual reported ~20000 sigma and cleared any guard. 503 such rows exist
    // in the real theme CSVs, mostly from ablation arms.
    let rows = [| cmpRow "fork" 500 50.0 0.0; cmpRow "pin" 500 50.0 100.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    let profs = PuzzleReport.buildProfiles view 400
    for p in profs do
        for e in [ p.Best; p.Worst ] do
            match e with
            | Some entry ->
                Assert.True(entry.Sigma < 100.0, $"sigma {entry.Sigma} is not a screening number")
                Assert.False(Double.IsNaN entry.Sigma || Double.IsInfinity entry.Sigma)
            | None -> ()

[<Fact>]
let ``sigma is independent of how strong the net is`` () =
    // Same residual, wildly different accuracy levels, must score the same.
    let weak = [| cmpRow "fork" 500 4.0 0.0; cmpRow "pin" 500 0.0 4.0 |]
    let strong = [| cmpRow "fork" 500 54.0 50.0; cmpRow "pin" 500 50.0 54.0 |]
    let sigmaOf rows =
        let v = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
        (PuzzleReport.buildProfiles v 400).[0].Best.Value.Sigma
    Assert.Equal(sigmaOf strong, sigmaOf weak, 6)

[<Fact>]
let ``loadThemes honours the quoting the writer emits for net names`` () =
    // PuzzleThemes.csvField quotes any field containing a comma, because net names
    // come from user-authored engine defs. A plain split shifted every later column
    // and dropped the row, silently emptying the whole run's theme data.
    let path = writeTempCsv [ "Policy,2600,\"C1-640-34, ema\",netB,fork,424,51.50,56.60,5.10,0.77" ]
    try
        let row = Assert.Single (PuzzleReport.loadThemes path)
        Assert.Equal("C1-640-34, ema", row.NetA)
        Assert.Equal("netB", row.NetB)
        Assert.Equal("fork", row.Theme)
        Assert.Equal(424, row.N)
    finally
        File.Delete path

[<Fact>]
let ``loadThemes reads a doubled quote inside a quoted field`` () =
    let path = writeTempCsv [ "Policy,2600,\"net \"\"A\"\"\",netB,fork,424,51.50,56.60,5.10,0.77" ]
    try
        let row = Assert.Single (PuzzleReport.loadThemes path)
        Assert.Equal("net \"A\"", row.NetA)
    finally
        File.Delete path

[<Fact>]
let ``loadThemes returns empty for a truncated or header-only file`` () =
    // Array.skip 1 threw here, and because the page loads summary and themes in one
    // task the throw took the leaderboard down with it.
    let empty = Path.Combine(Path.GetTempPath(), sprintf "puzzleThemes_empty_%s.csv" (Guid.NewGuid().ToString "N"))
    File.WriteAllText(empty, "")
    let headerOnly = writeTempCsv []
    try
        Assert.Empty(PuzzleReport.loadThemes empty)
        Assert.Empty(PuzzleReport.loadThemes headerOnly)
    finally
        File.Delete empty
        File.Delete headerOnly

// ---------------------------------------------------------------------------
// Nested subtypes: a child column earns its place only by diverging from its
// parent, and never contributes to a net's overall level either way.
// ---------------------------------------------------------------------------

let private famRow theme n accA accB : PuzzleReport.ThemeRow =
    { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = "engA"; EngineB = "engB"; Nodes = Some 1
      NetA = "netA"; NetB = "netB"; Theme = theme; N = n; AccA = accA; AccB = accB }

[<Fact>]
let ``dropNonTactical keeps nested subtypes, dropping only descriptive tags`` () =
    let rows = [| themeRow "Policy" "2600" "endgame" 3224
                  themeRow "Policy" "2600" "rookEndgame" 421
                  themeRow "Policy" "2600" "short" 500 |]
    Assert.Equal<string[]>([| "endgame"; "rookEndgame" |],
                           PuzzleReport.dropNonTactical rows |> Array.map (fun r -> r.Theme) |> Array.sort)

[<Fact>]
let ``a subtype that mirrors its parent is hidden`` () =
    // rookEndgame tracks endgame exactly, so it is redundant on screen.
    let rows = [| famRow "endgame" 3000 40.0 50.0
                  famRow "rookEndgame" 500 40.0 50.0
                  famRow "fork" 500 40.0 50.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    Assert.DoesNotContain("rookEndgame", view.Cols |> Array.map (fun c -> c.Theme))

[<Fact>]
let ``a subtype that pulls away from its parent is shown, next to it`` () =
    // netB is level with the field in endgames generally but far worse in rook endings.
    let rows = [| famRow "endgame" 3000 45.0 45.0
                  famRow "rookEndgame" 500 60.0 30.0
                  famRow "fork" 500 40.0 40.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    let themes = view.Cols |> Array.map (fun c -> c.Theme)
    Assert.Contains("rookEndgame", themes)
    // and it sits directly after its parent, marked as nested
    let pi = Array.findIndex ((=) "endgame") themes
    Assert.Equal("rookEndgame", themes.[pi + 1])
    Assert.Equal(Some "endgame", view.Cols.[pi + 1].Parent)
    Assert.Equal(None, view.Cols.[pi].Parent)

[<Fact>]
let ``a displayed subtype does not change any net's overall level`` () =
    // The regression that started this: showing parent and child together used to
    // double-count the family in the offset. Same data, one run where the subtype
    // diverges enough to be shown and one where it does not - offsets must match.
    let withChild =
        [| famRow "endgame" 3000 45.0 45.0
           famRow "rookEndgame" 500 60.0 30.0
           famRow "fork" 500 40.0 60.0 |]
    let withoutChild =
        [| famRow "endgame" 3000 45.0 45.0
           famRow "fork" 500 40.0 60.0 |]
    let offsets rows =
        (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value.Offsets
    let a = offsets withChild
    let b = offsets withoutChild
    Assert.Contains("rookEndgame",
                    (PuzzleReport.buildHeatView withChild "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value.Cols
                    |> Array.map (fun c -> c.Theme))
    Assert.Equal(b.[0], a.[0], 6)
    Assert.Equal(b.[1], a.[1], 6)

[<Fact>]
let ``a subtype whose parent is absent stands on its own`` () =
    let rows = [| famRow "pawnEndgame" 500 40.0 50.0; famRow "fork" 500 40.0 50.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    let col = view.Cols |> Array.find (fun c -> c.Theme = "pawnEndgame")
    Assert.Equal(None, col.Parent)

[<Fact>]
let ``theme rows that name the same net on both sides are dropped, not merged`` () =
    // A result file written before the themes CSV carried engine names: a cross-engine
    // run's two sides are indistinguishable, so the row cannot be attributed and
    // reporting either side would present two engines as one.
    let ambiguous : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = ""; Nodes = Some 1
          NetA = "sameNet"; NetB = "sameNet"; Theme = "fork"; N = 500; AccA = 40.0; AccB = 60.0 }
    let rows = [| ambiguous; famRow "pin" 500 40.0 50.0 |]
    match PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |] with
    | Some view -> Assert.DoesNotContain("fork", view.Cols |> Array.map (fun c -> c.Theme))
    | None -> ()

[<Fact>]
let ``a theme measured with different n keeps the smallest`` () =
    // PuzzleThemes.diff records the per-pair minimum, so three nets can disagree;
    // last-write-wins made the gates depend on read order.
    let r1 = famRow "fork" 500 40.0 50.0
    let r2 = { famRow "fork" 300 40.0 45.0 with NetB = "netC" }
    let view = (PuzzleReport.buildHeatView [| r1; r2 |] "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB"; "netC" |]).Value
    Assert.Equal(300, (view.Cols |> Array.find (fun c -> c.Theme = "fork")).N)

[<Fact>]
let ``a hidden subtype does not shift a single-net baseline`` () =
    // The single-net baseline used to average every kept theme, including columns the
    // viewer never sees and an endgame subtype counted twice beside its parent.
    let single theme n acc : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = "engB"; Nodes = Some 1
          NetA = ""; NetB = "solo"; Theme = theme; N = n; AccA = Double.NaN; AccB = acc }
    let rows = [| single "endgame" 3000 40.0; single "pawnEndgame" 600 41.0; single "fork" 500 45.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "solo" |]).Value
    // pawnEndgame tracks its parent, so it is hidden...
    Assert.Equal<string[]>([| "fork"; "endgame" |] |> Array.sort,
                           view.Cols |> Array.map (fun c -> c.Theme) |> Array.sort)
    // ...and the baseline is the mean of the two visible columns (42.5), not of all three
    let fork = view.Cols |> Array.findIndex (fun c -> c.Theme = "fork")
    Assert.Equal(42.5, view.Cols.[fork].Mean, 6)
    Assert.Equal(2.5, view.Raw.[0].[fork], 6)

[<Fact>]
let ``named mates nest under mate just as mateIn does`` () =
    let rows = [| famRow "mate" 900 40.0 40.0
                  famRow "backRankMate" 400 40.0 40.0
                  famRow "fork" 500 40.0 50.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    // backRankMate mirrors its parent here, so it is hidden rather than double-counted
    Assert.DoesNotContain("backRankMate", view.Cols |> Array.map (fun c -> c.Theme))

[<Fact>]
let ``a subtype column is not offered as a callout candidate`` () =
    // It is on screen because some net was extreme there; ranking it against columns that
    // were not pre-selected would hand every card the same pre-chosen theme.
    let rows = [| famRow "endgame" 3000 45.0 45.0
                  famRow "pawnEndgame" 600 60.0 30.0
                  famRow "fork" 500 44.0 46.0 |]
    let view = (PuzzleReport.buildHeatView rows "Policy" "2600" "" (Some 1) 250 [| "netA"; "netB" |]).Value
    Assert.Contains("pawnEndgame", view.Cols |> Array.map (fun c -> c.Theme))
    let profs = PuzzleReport.buildProfiles view 400
    for prof in profs do
        for entry in [ prof.Best; prof.Worst ] do
            match entry with
            | Some e -> Assert.NotEqual<string>("pawnEndgame", e.Theme)
            | None -> ()

[<Fact>]
let ``unattributable themes are counted, not silently absent`` () =
    let ambiguous : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = ""; Nodes = Some 1
          NetA = "sameNet"; NetB = "sameNet"; Theme = "fork"; N = 500; AccA = 40.0; AccB = 60.0 }
    Assert.Equal(1, PuzzleReport.unattributableThemes [| ambiguous |] "Policy" "2600" "" (Some 1))
    Assert.Equal(0, PuzzleReport.unattributableThemes [| famRow "fork" 500 40.0 50.0 |] "Policy" "2600" "" (Some 1))

[<Fact>]
let ``one net under two engines is two columns, not an unattributable row`` () =
    // The same input as above, but from a file that HAS the engine columns: this is a
    // normal cross-engine comparison and must render, not be dropped.
    let crossEngine : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = "Ceres"; EngineB = "Lc0"; Nodes = Some 1
          NetA = "sameNet"; NetB = "sameNet"; Theme = "fork"; N = 500; AccA = 40.0; AccB = 60.0 }
    let other = { crossEngine with Theme = "pin"; AccA = 45.0; AccB = 55.0 }
    Assert.Equal(0, PuzzleReport.unattributableThemes [| crossEngine; other |] "Policy" "2600" "" (Some 1))
    let view = (PuzzleReport.buildHeatView [| crossEngine; other |] "Policy" "2600" "" (Some 1) 250 [| "sameNet" |]).Value
    Assert.Equal(2, view.Nets.Length)
    // and the engine is what tells the two columns apart
    Assert.Contains("Ceres", view.Nets.[0] + view.Nets.[1])
    Assert.Contains("Lc0", view.Nets.[0] + view.Nets.[1])

[<Fact>]
let ``a themes file without the filter column still renders`` () =
    // 50 of 56 result files on disk predate the puzzle_filter column, and an unfiltered
    // run's summary says "none" - so comparing the absent column against it dropped every
    // row and the heat view and profiles vanished for almost every existing run.
    let legacy theme accA accB : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = ""; Nodes = Some 1
          NetA = "netA"; NetB = "netB"; Theme = theme; N = 500; AccA = accA; AccB = accB }
    let rows = [| legacy "fork" 40.0 50.0; legacy "pin" 42.0 52.0 |]
    let view = PuzzleReport.buildHeatView rows "Policy" "2600" "none" (Some 1) 250 [| "netA"; "netB" |]
    Assert.True(view.IsSome, "a pre-column themes file must still render")
    Assert.Equal(2, view.Value.Cols.Length)
    Assert.Equal(0, PuzzleReport.unattributableThemes rows "Policy" "2600" "none" (Some 1))

[<Fact>]
let ``two node budgets are separate slices, not a contradiction`` () =
    // `"Type": "search", "Nodes": "10, 100"` writes two row sets with the same type and
    // rating group. Without the nodes column they had identical keys and different
    // accuracies, so EVERY theme was flagged unattributable and the view disappeared.
    let row nodes theme accA accB : PuzzleReport.ThemeRow =
        { Type = "Search"; RatingGroup = "2600"; Filter = "none"; EngineA = "engA"
          EngineB = "engB"; Nodes = Some nodes; NetA = "netA"; NetB = "netB"
          Theme = theme; N = 500; AccA = accA; AccB = accB }
    let rows =
        [| row 10 "fork" 40.0 50.0; row 10 "pin" 42.0 52.0
           row 100 "fork" 55.0 65.0; row 100 "pin" 57.0 67.0 |]
    Assert.Equal(0, PuzzleReport.unattributableThemes rows "Search" "2600" "none" (Some 10))
    let low = (PuzzleReport.buildHeatView rows "Search" "2600" "none" (Some 10) 250 [| "netA"; "netB" |]).Value
    let high = (PuzzleReport.buildHeatView rows "Search" "2600" "none" (Some 100) 250 [| "netA"; "netB" |]).Value
    Assert.Equal(2, low.Cols.Length)
    Assert.Equal(2, high.Cols.Length)
    Assert.NotEqual(low.Acc.[0].[0], high.Acc.[0].[0], 6)

[<Fact>]
let ``a themes file without the nodes column still renders`` () =
    // Nodes = 0 means the column was absent. Comparing it against a real budget would
    // repeat exactly the mistake the filter column made.
    let legacy theme accA accB : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = ""; EngineA = ""; EngineB = ""
          Nodes = None; NetA = "netA"; NetB = "netB"; Theme = theme; N = 500
          AccA = accA; AccB = accB }
    let rows = [| legacy "fork" 40.0 50.0; legacy "pin" 42.0 52.0 |]
    let view = PuzzleReport.buildHeatView rows "Policy" "2600" "none" (Some 1) 250 [| "netA"; "netB" |]
    Assert.True(view.IsSome, "a pre-column themes file must still render")
    Assert.Equal(2, view.Value.Cols.Length)

[<Fact>]
let ``puzzle filters are separate slices in the heat view`` () =
    // "fork" and "pin" runs both carry the shared Lichess themes; folding them together
    // made every shared theme look self-contradictory and took the whole view down.
    let row filter theme accA accB : PuzzleReport.ThemeRow =
        { Type = "Policy"; RatingGroup = "2600"; Filter = filter; EngineA = "engA"; EngineB = "engB"; Nodes = Some 1
          NetA = "netA"; NetB = "netB"; Theme = theme; N = 500; AccA = accA; AccB = accB }
    let rows =
        [| row "fork" "middlegame" 40.0 50.0; row "fork" "crushing" 42.0 52.0
           row "pin" "middlegame" 30.0 60.0; row "pin" "crushing" 32.0 62.0 |]
    Assert.Equal(0, PuzzleReport.unattributableThemes rows "Policy" "2600" "fork" (Some 1))
    let forkView = (PuzzleReport.buildHeatView rows "Policy" "2600" "fork" (Some 1) 250 [| "netA"; "netB" |]).Value
    let pinView = (PuzzleReport.buildHeatView rows "Policy" "2600" "pin" (Some 1) 250 [| "netA"; "netB" |]).Value
    // same themes, different numbers - the two slices must not have been folded
    Assert.Equal(2, forkView.Cols.Length)
    Assert.Equal(2, pinView.Cols.Length)
    Assert.NotEqual(forkView.Acc.[0].[0], pinView.Acc.[0].[0], 6)
