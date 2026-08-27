module PuzzlePairedTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.PuzzleTypes

// ---------------------------------------------------------------------------
// Fixtures — puzzles are identified by PuzzleId alone here, so the board fields
// can stay constant.
// ---------------------------------------------------------------------------

let private mkPuzzle (id: int) : CsvPuzzleData =
    CsvPuzzleData.Create(
        string id, "8/8/8/8/8/8/8/K6k w - - 0 1", "a1a2", 2300.0, 80.0, 90, 100,
        "endgame", "https://lichess.org/x", "", null, null, null, 0)

let private mkScore (engine: string) (net: string) (typ: string) (ratingAvg: float)
                    (solved: int list) (failed: int list) : Score =
    { Engine = engine
      NeuralNet = net
      TotalNumber = solved.Length + failed.Length
      Correct = solved.Length
      Wrong = failed.Length
      RatingAvg = ratingAvg
      Filter = "none"
      PlayerRecord = { Rating = 2000.0; Deviation = 50.0; Volatility = 0.06 }
      FailedPuzzles = ResizeArray<CsvPuzzleData * string>(failed |> List.map (fun i -> mkPuzzle i, "wrong"))
      CorrectPuzzles = ResizeArray<CsvPuzzleData>(solved |> List.map mkPuzzle)
      Nodes = 1
      WithHistory = false
      Type = typ
      AvgKLD = 0.0
      AvgRankWeightedKld = 0.0
      AvgFrontierKld = 0.0
      AvgMarginLoss = 0.0
      AvgValueLoss = 0.0
      AvgEstNodesLog10 = 0.0
      EstNodesP95 = 0.0
      EstNodesP99 = 0.0
      EstNodesCdf100 = 0.0
      HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
      PositionsCorrect = 0
      PositionsScored = 0
      FirstMoveCorrect = 0
      FirstMoveScored = 0
      FirstMoveCorrectIds = System.Collections.Generic.HashSet<string>() }

// ---------------------------------------------------------------------------
// Discordant counts — the whole point of the paired test
// ---------------------------------------------------------------------------

[<Fact>]
let ``compute counts only the puzzles the two nets disagree on`` () =
    // 1,2 solved by both; 3,4 only by A; 5 only by B; 6 failed by both.
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2; 3; 4 ] [ 5; 6 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 1; 2; 5 ] [ 3; 4; 6 ]
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(6, c.N)
    Assert.Equal(2, c.OnlyA)
    Assert.Equal(1, c.OnlyB)
    Assert.Equal(3, c.Discordant)

[<Fact>]
let ``delta is B minus A, matching the theme tables`` () =
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2 ] [ 3; 4 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 1; 2; 3 ] [ 4 ]
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(50.0, c.AccuracyAPct, 6)
    Assert.Equal(75.0, c.AccuracyBPct, 6)
    Assert.Equal(25.0, c.DeltaPp, 6)
    Assert.True(c.Z > 0.0, "positive z must favour B")

[<Fact>]
let ``z is (onlyB - onlyA) over sqrt of the discordant count`` () =
    // 9 flips to B, 1 flip to A: z = 8 / sqrt 10
    let solvedByBoth = [ 100 .. 149 ]
    let flippedToB = [ 1 .. 9 ]
    let flippedToA = [ 10 ]
    let a = mkScore "e" "A" "Policy" 2300.0 (solvedByBoth @ flippedToA) flippedToB
    let b = mkScore "e" "B" "Policy" 2300.0 (solvedByBoth @ flippedToB) flippedToA
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(9, c.OnlyB)
    Assert.Equal(1, c.OnlyA)
    Assert.Equal(8.0 / sqrt 10.0, c.Z, 6)

[<Fact>]
let ``the paired z is tighter than the unpaired sigma on the same delta`` () =
    // The reason the module exists: same numbers, both tests, paired must win
    // whenever the nets agree on most puzzles.
    let agreed = [ 1000 .. 1799 ]      // 800 solved by both
    let flippedToB = [ 1 .. 60 ]
    let flippedToA = [ 100 .. 139 ]
    let a = mkScore "e" "A" "Policy" 2300.0 (agreed @ flippedToA) flippedToB
    let b = mkScore "e" "B" "Policy" 2300.0 (agreed @ flippedToB) flippedToA
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    let unpaired = PuzzleThemes.sigmaOf c.N c.DeltaPp
    Assert.True(abs c.Z > unpaired,
                sprintf "paired z %.2f should exceed unpaired sigma %.2f" c.Z unpaired)

[<Fact>]
let ``nets that never disagree carry no test`` () =
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2 ] [ 3 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 1; 2 ] [ 3 ]
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(0, c.Discordant)
    Assert.Equal(0.0, c.Z, 6)
    Assert.Equal(0.0, c.DeltaPp, 6)

[<Fact>]
let ``an equal split of flips is a zero z, not a missing row`` () =
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2; 3 ] [ 4; 5; 6 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 4; 5; 6 ] [ 1; 2; 3 ]
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(6, c.Discordant)
    Assert.Equal(0.0, c.Z, 6)

// ---------------------------------------------------------------------------
// Slicing — the bug that produced this module was a missing rating group
// ---------------------------------------------------------------------------

[<Fact>]
let ``rating groups are separate comparisons`` () =
    let a2300 = mkScore "e" "A" "Policy" 2299.0 [ 1; 2 ] [ 3 ]
    let b2300 = mkScore "e" "B" "Policy" 2299.0 [ 1; 2; 3 ] []
    let a2500 = mkScore "e" "A" "Policy" 2498.0 [ 4 ] [ 5; 6 ]
    let b2500 = mkScore "e" "B" "Policy" 2498.0 [ 4; 5 ] [ 6 ]
    let cs = PuzzlePaired.compute [ a2300; b2300; a2500; b2500 ]
    Assert.Equal(2, cs.Length)
    Assert.Equal<int list>([ 2300; 2500 ], cs |> List.map (fun c -> c.RatingGroup) |> List.sort)

[<Fact>]
let ``test types are separate comparisons`` () =
    let ap = mkScore "e" "A" "Policy" 2300.0 [ 1 ] [ 2 ]
    let bp = mkScore "e" "B" "Policy" 2300.0 [ 1; 2 ] []
    let av = mkScore "e" "A" "Value" 2300.0 [ 1 ] [ 2 ]
    let bv = mkScore "e" "B" "Value" 2300.0 [ 1; 2 ] []
    let cs = PuzzlePaired.compute [ ap; bp; av; bv ]
    Assert.Equal(2, cs.Length)
    Assert.Equal<string list>([ "Policy"; "Value" ], cs |> List.map (fun c -> c.Type) |> List.sort)

[<Fact>]
let ``three nets produce every pair, not just against the first`` () =
    let mk net solved failed = mkScore "e" net "Policy" 2300.0 solved failed
    let cs = PuzzlePaired.compute [ mk "A" [ 1 ] [ 2; 3 ]; mk "B" [ 1; 2 ] [ 3 ]; mk "C" [ 1; 2; 3 ] [] ]
    Assert.Equal(3, cs.Length)
    let pairs = cs |> List.map (fun c -> c.NetA + "-" + c.NetB) |> List.sort
    Assert.Equal<string list>([ "A-B"; "A-C"; "B-C" ], pairs)

[<Fact>]
let ``a single net produces no comparison`` () =
    Assert.Empty(PuzzlePaired.compute [ mkScore "e" "A" "Policy" 2300.0 [ 1 ] [ 2 ] ])

[<Fact>]
let ``only the puzzles both nets scored are counted`` () =
    // B died after puzzle 3; the pair must be judged on 1-3, not on A's full set.
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2; 4; 5 ] [ 3 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 1; 3 ] [ 2 ]
    let c = PuzzlePaired.compute [ a; b ] |> List.exactlyOne
    Assert.Equal(3, c.N)
    Assert.Equal(1, c.OnlyA)   // 2
    Assert.Equal(1, c.OnlyB)   // 3

// ---------------------------------------------------------------------------
// Orientation. This is the reason computeOrdered exists: scores do NOT arrive in
// config order, and if A and B swap, every DeltaPp and Z ships with the opposite
// sign from the theme tables sitting beside them in the same file.
// ---------------------------------------------------------------------------

[<Fact>]
let ``computeOrdered puts the config's first engine in the A slot`` () =
    // arrival order is deliberately the reverse of the config order
    let second = mkScore "second" "netSecond" "Policy" 2300.0 [ 1; 2; 3 ] [ 4 ]
    let first = mkScore "first" "netFirst" "Policy" 2300.0 [ 1 ] [ 2; 3; 4 ]
    let c = PuzzlePaired.computeOrdered [ "first"; "second" ] [ second; first ] |> List.exactlyOne
    Assert.Equal("netFirst", c.NetA)
    Assert.Equal("netSecond", c.NetB)
    Assert.Equal(25.0, c.AccuracyAPct, 6)
    Assert.Equal(75.0, c.AccuracyBPct, 6)
    Assert.Equal(50.0, c.DeltaPp, 6)
    Assert.True(c.Z > 0.0, "B is the stronger net here, so z must be positive")

[<Fact>]
let ``reversing the config order reverses the pair and the sign`` () =
    let a = mkScore "first" "netFirst" "Policy" 2300.0 [ 1 ] [ 2; 3; 4 ]
    let b = mkScore "second" "netSecond" "Policy" 2300.0 [ 1; 2; 3 ] [ 4 ]
    let fwd = PuzzlePaired.computeOrdered [ "first"; "second" ] [ a; b ] |> List.exactlyOne
    let rev = PuzzlePaired.computeOrdered [ "second"; "first" ] [ a; b ] |> List.exactlyOne
    Assert.Equal("netFirst", fwd.NetA)
    Assert.Equal("netSecond", rev.NetA)
    Assert.Equal(fwd.DeltaPp, -rev.DeltaPp, 6)
    Assert.Equal(fwd.Z, -rev.Z, 6)
    Assert.Equal(fwd.OnlyA, rev.OnlyB)

[<Fact>]
let ``the paired orientation matches the theme tables for the same scores`` () =
    // The two tables land in one file, so they must agree on which net is A.
    // PuzzleThemes takes the same config order and makes its first entry the baseline.
    // MinThemePuzzles is 25, so the theme tables need a slice big enough to survive it
    let a = mkScore "first" "netFirst" "Value" 2300.0 [ 1 .. 10 ] [ 11 .. 40 ]
    let b = mkScore "second" "netSecond" "Value" 2300.0 [ 1 .. 30 ] [ 31 .. 40 ]
    let order = [ "first"; "second" ]
    let paired = PuzzlePaired.computeOrdered order [ b; a ] |> List.exactlyOne
    let themes = PuzzleThemes.writeThemeFiles "" "" order [ b; a ]
    Assert.Equal("netFirst", paired.NetA)
    // renderDiff prints "A = <net>"; the baseline must be the same net
    Assert.Contains("A = netFirst", themes.Summary)
    Assert.Contains("B = netSecond", themes.Summary)

[<Fact>]
let ``an engine missing from the config order keeps its arrival position`` () =
    let known = mkScore "known" "netKnown" "Policy" 2300.0 [ 1 ] [ 2 ]
    let stranger = mkScore "stranger" "netStranger" "Policy" 2300.0 [ 1; 2 ] []
    let c = PuzzlePaired.computeOrdered [ "known" ] [ known; stranger ] |> List.exactlyOne
    Assert.Equal("netKnown", c.NetA)
    Assert.Equal("netStranger", c.NetB)

// ---------------------------------------------------------------------------
// Slice and net identity. Two tests can share a Type label ("policy, policyvalue",
// or a search at nodes <= 1), and a PuzzleFilter list produces one Score per theme.
// ---------------------------------------------------------------------------

[<Fact>]
let ``two measurements of one net never become a comparison`` () =
    // what `Type: "policy, policyvalue"` produces: same engine, same net, same slice
    let viaTopN = mkScore "e" "netX" "Policy" 2300.0 [ 1; 2 ] [ 3; 4 ]
    let viaCombo = mkScore "e" "netX" "Policy" 2300.0 [ 1; 3 ] [ 2; 4 ]
    Assert.Empty(PuzzlePaired.compute [ viaTopN; viaCombo ])

[<Fact>]
let ``a duplicated net does not double the pairs against a real opponent`` () =
    let viaTopN = mkScore "e" "netX" "Policy" 2300.0 [ 1; 2 ] [ 3; 4 ]
    let viaCombo = mkScore "e" "netX" "Policy" 2300.0 [ 1; 3 ] [ 2; 4 ]
    let other = mkScore "f" "netY" "Policy" 2300.0 [ 1; 2; 3 ] [ 4 ]
    let c = PuzzlePaired.compute [ viaTopN; viaCombo; other ] |> List.exactlyOne
    Assert.Equal("netX", c.NetA)
    Assert.Equal("netY", c.NetB)
    // the FIRST measurement in config order survives, not whichever finished last
    Assert.Equal(50.0, c.AccuracyAPct, 6)

[<Fact>]
let ``the same net under two engines is still a real comparison`` () =
    // cross-engine runs (one ONNX under Ceres and Lc0) must NOT be deduplicated
    let ceres = mkScore "Ceres" "netX" "Policy" 2300.0 [ 1; 2 ] [ 3 ]
    let lc0 = mkScore "Lc0" "netX" "Policy" 2300.0 [ 1; 2; 3 ] []
    let c = PuzzlePaired.compute [ ceres; lc0 ] |> List.exactlyOne
    Assert.Equal("Ceres", c.EngineA)
    Assert.Equal("Lc0", c.EngineB)

[<Fact>]
let ``puzzle filters are separate slices`` () =
    let fork a b = { mkScore "e" a "Policy" 2300.0 b [] with Filter = "fork" }
    let pin a b = { mkScore "e" a "Policy" 2300.0 b [] with Filter = "pin" }
    let cs =
        PuzzlePaired.compute
            [ fork "netA" [ 1; 2 ]; fork "netB" [ 1; 2 ]
              pin "netA" [ 3 ]; pin "netB" [ 3 ] ]
    Assert.Equal(2, cs.Length)
    Assert.Equal<string list>([ "fork"; "pin" ], cs |> List.map (fun c -> c.Filter) |> List.sort)

[<Fact>]
let ``node budgets are separate slices`` () =
    let at nodes net = { mkScore "e" net "Search" 2300.0 [ 1; 2 ] [ 3 ] with Nodes = nodes }
    let cs = PuzzlePaired.compute [ at 1 "netA"; at 1 "netB"; at 100 "netA"; at 100 "netB" ]
    Assert.Equal(2, cs.Length)
    Assert.Equal<int list>([ 1; 100 ], cs |> List.map (fun c -> c.Nodes) |> List.sort)

// ---------------------------------------------------------------------------
// p-value approximation
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData(0.0, 1.0)>]
[<InlineData(1.96, 0.05)>]
[<InlineData(2.576, 0.01)>]
[<InlineData(3.0, 0.0027)>]
let ``pValueOf matches the standard normal two-sided tail`` (z: float, expected: float) =
    Assert.Equal(expected, PuzzlePaired.pValueOf z, 3)

[<Fact>]
let ``all four bucketing rules agree`` () =
    // PuzzleTrend's copy is the one the report page uses to match a run's scores against
    // PairedRow.RatingGroup, so a drift would break that join silently.
    for ratingAvg in [ 0.0; 949.0; 950.0; 2299.0; 2300.0; 2498.0; 2600.4; 3899.0 ] do
        Assert.Equal(PuzzlePaired.ratingGroupOf ratingAvg, PuzzleTrend.ratingGroupOf ratingAvg)

[<Fact>]
let ``pValueOf is symmetric in the sign of z`` () =
    Assert.Equal(PuzzlePaired.pValueOf 2.4, PuzzlePaired.pValueOf -2.4, 10)

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

[<Fact>]
let ``render names the slice only when a run has more than one`` () =
    let one =
        PuzzlePaired.compute
            [ mkScore "e" "netA" "Policy" 2300.0 [ 1 .. 60 ] [ 61 .. 100 ]
              mkScore "e" "netB" "Policy" 2300.0 [ 1 .. 80 ] [ 81 .. 100 ] ]
    let single = PuzzlePaired.render one
    // "theme" alone would match the explanatory prose above the table ("per-theme sigma"),
    // so the nodes column is the honest marker for the slice header being present
    Assert.DoesNotContain("nodes", single)

    // two puzzle filters: the rows now differ ONLY in a column that used to be missing
    let withFilter f net solved failed =
        { mkScore "e" net "Policy" 2300.0 solved failed with Filter = f }
    let many =
        PuzzlePaired.compute
            [ withFilter "fork" "netA" [ 1 .. 60 ] [ 61 .. 100 ]
              withFilter "fork" "netB" [ 1 .. 80 ] [ 81 .. 100 ]
              withFilter "pin" "netA" [ 1 .. 50 ] [ 51 .. 100 ]
              withFilter "pin" "netB" [ 1 .. 70 ] [ 71 .. 100 ] ]
    let rendered = PuzzlePaired.render many
    Assert.Contains("nodes", rendered)
    Assert.Contains("fork", rendered)
    Assert.Contains("pin", rendered)

[<Fact>]
let ``render disambiguates one net measured under two engines`` () =
    let ceres = mkScore "Ceres" "netX" "Policy" 2300.0 [ 1 .. 60 ] [ 61 .. 100 ]
    let lc0 = mkScore "Lc0" "netX" "Policy" 2300.0 [ 1 .. 80 ] [ 81 .. 100 ]
    let rendered = PuzzlePaired.render (PuzzlePaired.compute [ ceres; lc0 ])
    // without the engine the row reads "netX vs netX", which looks like a bug in the tool
    Assert.Contains("(Ceres)", rendered)
    Assert.Contains("(Lc0)", rendered)

[<Fact>]
let ``a cross-engine row shows two DIFFERENT labels at real name lengths`` () =
    // The regression this guards: the finished "net (engine)" string was shortened from
    // the left, which cut the net away and left both sides ending in the same engine
    // text - byte-identical A and B columns.
    let net = "a4000-21bn11_srv_576_16_t91ab_ctrl_800026624"
    let ceres = mkScore "Ceres srv 576x16 t91ab ctrl 800M" net "Policy" 2300.0 [ 1 .. 60 ] [ 61 .. 100 ]
    let lc0 = mkScore "Lc0 srv 576x16 t91ab ctrl 800M" net "Policy" 2300.0 [ 1 .. 80 ] [ 81 .. 100 ]
    let rendered = PuzzlePaired.render (PuzzlePaired.compute [ ceres; lc0 ])
    let row =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.find (fun l -> l.Contains "Policy" && l.Contains "(")
    Assert.Contains("Ceres", row)
    Assert.Contains("Lc0", row)
    // the step number is what identifies the net, so it must survive too
    Assert.Contains("800026624", row)

[<Fact>]
let ``a long theme name does not shift the following columns`` () =
    let withFilter f net solved failed =
        { mkScore "e" net "Policy" 2300.0 solved failed with Filter = f }
    let comparisons =
        PuzzlePaired.compute
            [ withFilter "exposedKing" "netA" [ 1 .. 60 ] [ 61 .. 100 ]
              withFilter "exposedKing" "netB" [ 1 .. 80 ] [ 81 .. 100 ]
              withFilter "pin" "netA" [ 1 .. 50 ] [ 51 .. 100 ]
              withFilter "pin" "netB" [ 1 .. 70 ] [ 71 .. 100 ] ]
    let lines =
        (PuzzlePaired.render comparisons).Split([| Environment.NewLine |], StringSplitOptions.None)
    let dataLines = lines |> Array.filter (fun l -> l.Contains "Policy" && l.Contains "netA")
    Assert.Equal(2, dataLines.Length)
    // every data row must put its columns at the same offsets
    let netAColumn (l: string) = l.IndexOf "netA"
    Assert.Equal(netAColumn dataLines.[0], netAColumn dataLines.[1])

[<Fact>]
let ``an unfiltered run gets no theme column`` () =
    // Score.Filter is the literal "none" for an unfiltered run, not "" - a column that
    // reads "none" on every row is noise.
    let mk net solved failed = { mkScore "e" net "Policy" 2300.0 solved failed with Filter = "none" }
    let rendered =
        PuzzlePaired.render (PuzzlePaired.compute [ mk "netA" [ 1 .. 60 ] [ 61 .. 100 ]
                                                    mk "netB" [ 1 .. 80 ] [ 81 .. 100 ] ])
    Assert.DoesNotContain("nodes", rendered)

[<Fact>]
let ``engines differing only at the end still render distinctly`` () =
    // "Ceres-800M-gpu0" vs "-gpu1": cutting the head leaves both sides identical, which
    // is the very failure the cross-engine label exists to prevent.
    let a, b =
        PuzzlePaired.fittedSideNames
            "a4000-21bn11_srv_576_16_t91ab_ctrl_800026624"
            "a4000-21bn11_srv_576_16_t91ab_ctrl_800026624"
            "Ceres-800M-gpu0" "Ceres-800M-gpu1"
    Assert.NotEqual<string>(a, b)

[<Fact>]
let ``a composed label keeps the net's step number`` () =
    // renderDiffFor shortens what it is given; handing it a composed string used to cut
    // the net away entirely and leave two near-identical lines.
    let a, b =
        PuzzlePaired.fittedSideNames
            "a4000-21bn11_srv_576_16_t91ab_ctrl_800026624"
            "a4000-21bn11_srv_576_16_t91ab_ctrl_800026624"
            "Ceres srv 576x16 ctrl 800M" "Lc0 srv 576x16 ctrl 800M"
    Assert.NotEqual<string>(a, b)
    Assert.Contains("800026624", a)
    Assert.Contains("800026624", b)

[<Fact>]
let ``sideNames adds the engine only when the nets collide`` () =
    // The console headline and the per-theme tables use this; the fitted table labels use
    // the same condition, so the three views must agree on when an engine is needed.
    Assert.Equal(("netX (Ceres)", "netX (Lc0)"), PuzzlePaired.sideNames "netX" "netX" "Ceres" "Lc0")
    Assert.Equal(("netA", "netB"), PuzzlePaired.sideNames "netA" "netB" "Ceres" "Lc0")
    // one engine measuring one net twice is not a comparison; nothing to disambiguate
    Assert.Equal(("netX", "netX"), PuzzlePaired.sideNames "netX" "netX" "Ceres" "Ceres")

[<Fact>]
let ``render does not name the engine when the nets already differ`` () =
    let a = mkScore "Ceres" "netA" "Policy" 2300.0 [ 1 .. 60 ] [ 61 .. 100 ]
    let b = mkScore "Lc0" "netB" "Policy" 2300.0 [ 1 .. 80 ] [ 81 .. 100 ]
    let rendered = PuzzlePaired.render (PuzzlePaired.compute [ a; b ])
    Assert.DoesNotContain("(Ceres)", rendered)

[<Fact>]
let ``render truncates a large run and says so`` () =
    // 12 nets = 66 pairs, past MaxRenderedRows
    let nets =
        [ for i in 1 .. 12 ->
            mkScore "e" (sprintf "net%02d" i) "Policy" 2300.0 [ 1 .. 50 + i ] [ 51 + i .. 200 ] ]
    let comparisons = PuzzlePaired.compute nets
    Assert.True(comparisons.Length > PuzzlePaired.MaxRenderedRows)
    let rendered = PuzzlePaired.render comparisons
    let dataLines =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.filter (fun l -> l.Contains " net" && l.Contains "Policy")
    Assert.Equal(PuzzlePaired.MaxRenderedRows, dataLines.Length)
    // never a silent cap
    Assert.Contains(sprintf "%d of %d rows" PuzzlePaired.MaxRenderedRows comparisons.Length, rendered)

[<Fact>]
let ``a truncated table keeps the strongest evidence`` () =
    let nets =
        [ for i in 1 .. 12 ->
            mkScore "e" (sprintf "net%02d" i) "Policy" 2300.0 [ 1 .. 50 + i ] [ 51 + i .. 200 ] ]
    let comparisons = PuzzlePaired.compute nets
    let strongest = comparisons |> List.maxBy (fun c -> abs c.Z)
    let rendered = PuzzlePaired.render comparisons
    Assert.Contains(strongest.NetA, rendered)
    Assert.Contains(strongest.NetB, rendered)

[<Fact>]
let ``render returns empty for nothing to compare`` () =
    Assert.Equal("", PuzzlePaired.render [])

[<Fact>]
let ``render flags a thin discordance instead of hiding the row`` () =
    let a = mkScore "e" "A" "Policy" 2300.0 [ 1; 2 ] [ 3 ]
    let b = mkScore "e" "B" "Policy" 2300.0 [ 1; 2; 3 ] []
    let text = PuzzlePaired.render (PuzzlePaired.compute [ a; b ])
    Assert.Contains("(thin)", text)
    Assert.Contains("Policy", text)

[<Fact>]
let ``render does not flag a healthy discordance`` () =
    let agreed = [ 1000 .. 1099 ]
    let flippedToB = [ 1 .. 40 ]
    let a = mkScore "e" "A" "Policy" 2300.0 agreed flippedToB
    let b = mkScore "e" "B" "Policy" 2300.0 (agreed @ flippedToB) []
    let text = PuzzlePaired.render (PuzzlePaired.compute [ a; b ])
    Assert.DoesNotContain("(thin)", text)
