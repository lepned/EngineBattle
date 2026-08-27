module PuzzleThemeTests

open System.Collections.Generic
open Xunit
open ChessLibrary
open ChessLibrary.PuzzleTypes

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

let private mkPuzzle (themes: string) : CsvPuzzleData =
    CsvPuzzleData.Create(
        "1", "8/8/8/8/8/8/8/K6k w - - 0 1", "a1a2", 2300.0, 80.0, 90, 100,
        themes, "https://lichess.org/x", "", null, null, null, 0)

/// ThemeStat is small enough that a positional helper keeps the tests readable
let private stat theme total correct : PuzzleThemes.ThemeStat =
    { Theme = theme; Total = total; Correct = correct }

let private mkScore (solved: string list) (failed: string list) : Score =
    { Engine = "e"
      NeuralNet = "net"
      TotalNumber = solved.Length + failed.Length
      Correct = solved.Length
      Wrong = failed.Length
      RatingAvg = 2300.0
      Filter = "none"
      PlayerRecord = { Rating = 2000.0; Deviation = 50.0; Volatility = 0.06 }
      FailedPuzzles = ResizeArray<CsvPuzzleData * string>(failed |> List.map (fun t -> mkPuzzle t, "wrong"))
      CorrectPuzzles = ResizeArray<CsvPuzzleData>(solved |> List.map mkPuzzle)
      Nodes = 1
      WithHistory = false
      Type = "Value"
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

let private statFor theme (stats: PuzzleThemes.ThemeStat list) =
    stats |> List.find (fun s -> s.Theme = theme)

// ---------------------------------------------------------------------------

[<Fact>]
let ``themes are split on whitespace and deduplicated`` () =
    let parsed = PuzzleThemes.parseThemes "advantage long middlegame"
    Assert.Equal<string list>([ "advantage"; "long"; "middlegame" ], parsed)
    Assert.Equal<string list>([ "fork" ], PuzzleThemes.parseThemes "fork fork")
    Assert.Empty(PuzzleThemes.parseThemes "")
    Assert.Empty(PuzzleThemes.parseThemes "   ")

[<Fact>]
let ``a multi-theme puzzle counts once under each theme`` () =
    // theme totals deliberately sum to more than the sample size
    let score = mkScore [ "fork pin" ] []
    let stats = PuzzleThemes.breakdown score
    Assert.Equal(2, stats.Length)
    Assert.Equal(1, (statFor "fork" stats).Total)
    Assert.Equal(1, (statFor "pin" stats).Total)

[<Fact>]
let ``breakdown counts solved and failed separately`` () =
    let score = mkScore [ "endgame"; "endgame"; "endgame" ] [ "endgame"; "fork" ]
    let stats = PuzzleThemes.breakdown score
    let endgame = statFor "endgame" stats
    Assert.Equal(4, endgame.Total)
    Assert.Equal(3, endgame.Correct)
    Assert.Equal(0.75, PuzzleThemes.accuracyOf endgame, 3)
    let fork = statFor "fork" stats
    Assert.Equal(1, fork.Total)
    Assert.Equal(0, fork.Correct)

[<Fact>]
let ``breakdown tolerates a score with no puzzle lists`` () =
    let score = { mkScore [] [] with CorrectPuzzles = null; FailedPuzzles = null }
    Assert.Empty(PuzzleThemes.breakdown score)

[<Fact>]
let ``diff reports B minus A in percentage points`` () =
    let a = [ stat "endgame" 100 60 ]
    let b = [ stat "endgame" 100 40 ]
    let diffs, dropped = PuzzleThemes.diff 25 a b
    Assert.Equal(0, dropped)
    Assert.Single(diffs) |> ignore
    let d = List.head diffs
    Assert.Equal(60.0, d.AccuracyA * 100.0, 1)
    Assert.Equal(40.0, d.AccuracyB * 100.0, 1)
    Assert.Equal(-20.0, d.DeltaPp, 1)

[<Fact>]
let ``diff drops thin themes and counts them`` () =
    // a theme seen 5 times swings 20 pp on a single puzzle, so it must not rank
    let a =
        [ stat "endgame" 100 60
          stat "rare" 5 5 ]
    let b =
        [ stat "endgame" 100 55
          stat "rare" 5 0 ]
    let diffs, dropped = PuzzleThemes.diff 25 a b
    Assert.Equal(1, dropped)
    Assert.Single(diffs) |> ignore
    Assert.Equal<string>("endgame", (List.head diffs).Theme)

[<Fact>]
let ``diff keeps only themes present for both nets`` () =
    let a =
        [ stat "shared" 50 25
          stat "onlyA" 50 25 ]
    let b = [ stat "shared" 50 30 ]
    let diffs, _ = PuzzleThemes.diff 25 a b
    Assert.Single(diffs) |> ignore
    Assert.Equal<string>("shared", (List.head diffs).Theme)

[<Fact>]
let ``diff sorts worst-for-B first`` () =
    let a =
        [ stat "worse" 100 50
          stat "better" 100 50 ]
    let b =
        [ stat "worse" 100 20
          stat "better" 100 80 ]
    let diffs, _ = PuzzleThemes.diff 25 a b
    Assert.Equal<string>("worse", diffs.Head.Theme)
    Assert.Equal<string>("better", (List.last diffs).Theme)

[<Fact>]
let ``equal deltas are ordered by theme name, not by hash order`` () =
    // bishopEndgame and zugzwang really did tie at +8.7 with n=69 in a live run, and
    // swapped places between two runs of the same data until the tie-break was added
    let a = [ stat "zugzwang" 69 59; stat "bishopEndgame" 69 61 ]
    let b = [ stat "zugzwang" 69 65; stat "bishopEndgame" 69 67 ]
    let diffs, _ = PuzzleThemes.diff 25 a b
    let deltas = diffs |> List.map (fun d -> d.DeltaPp)
    Assert.Equal(deltas.[0], deltas.[1], 6)
    Assert.Equal<string>("bishopEndgame", diffs.Head.Theme)

[<Fact>]
let ``breakdown orders equal-sized themes deterministically`` () =
    let score = mkScore [ "zulu alpha"; "zulu alpha" ] [ "zulu alpha" ]
    let stats = PuzzleThemes.breakdown score
    Assert.Equal<string>("alpha", stats.Head.Theme)

[<Fact>]
let ``sigma scales with sample size, not with the delta alone`` () =
    // the same 10 pp delta is weak on 40 puzzles and strong on 1000 - this is the
    // number that was missing when a delta-sorted table was read as "samples too small"
    let small = PuzzleThemes.sigmaOf 40 10.0
    let large = PuzzleThemes.sigmaOf 1000 10.0
    Assert.True(small < 1.5, $"expected a weak sigma on n=40, got {small}")
    Assert.True(large > 4.0, $"expected a strong sigma on n=1000, got {large}")
    Assert.Equal(0.0, PuzzleThemes.sigmaOf 0 10.0, 6)
    Assert.Equal(PuzzleThemes.sigmaOf 500 -8.0, PuzzleThemes.sigmaOf 500 8.0, 6)

[<Fact>]
let ``the largest samples are shown even when the sort hides them`` () =
    // a delta-sorted list puts the SMALLEST samples at both ends, so the rows worth
    // trusting land in the elided middle
    let ends =
        [ for i in 1 .. 6 -> stat (sprintf "tiny_neg_%d" i) 30 (30 - i) ]
        @ [ for i in 1 .. 6 -> stat (sprintf "tiny_pos_%d" i) 30 i ]
    let a = stat "bigTheme" 1000 500 :: ends
    let b =
        stat "bigTheme" 1000 400
        :: [ for i in 1 .. 6 -> stat (sprintf "tiny_neg_%d" i) 30 0 ]
        @ [ for i in 1 .. 6 -> stat (sprintf "tiny_pos_%d" i) 30 30 ]
    let diffs, _ = PuzzleThemes.diff 25 a b
    let rendered = PuzzleThemes.renderDiffFor "A" "B" "engA" "engB" 6 diffs
    Assert.Contains("largest samples:", rendered)
    Assert.Contains("bigTheme", rendered)

// ---------------------------------------------------------------------------
// First-move theme scoring. A puzzle's tags describe the move it exists for, so a
// puzzle failed three moves deep must not be charged against those tags.
// ---------------------------------------------------------------------------

[<Fact>]
let ``a puzzle failed after a correct first move counts as solved for its themes`` () =
    // one solved puzzle, one failed LATE (first move right), one failed on the first move
    let solvedP = mkPuzzle "fork"
    let lateFail = { mkPuzzle "fork" with PuzzleId = "2" }
    let earlyFail = { mkPuzzle "fork" with PuzzleId = "3" }
    let score =
        { mkScore [] [] with
            CorrectPuzzles = ResizeArray<CsvPuzzleData>([ solvedP ])
            FailedPuzzles = ResizeArray<CsvPuzzleData * string>([ lateFail, "x"; earlyFail, "x" ])
            // FirstMoveScored is what declares the capability; the id set alone is data
            FirstMoveScored = 3
            FirstMoveCorrect = 2
            FirstMoveCorrectIds = System.Collections.Generic.HashSet<string>([ "1"; "2" ]) }
    let stat = PuzzleThemes.breakdown score |> List.find (fun s -> s.Theme = "fork")
    Assert.Equal(3, stat.Total)
    // solved + late failure both count: only the early failure missed the thematic move
    Assert.Equal(2, stat.Correct)

[<Fact>]
let ``an id set without the capability flag does not silently change the rule`` () =
    // The capability is FirstMoveScored, not the id set: a Score whose ids survived but
    // whose counters did not must fall back rather than half-apply the new rule.
    let solvedP = mkPuzzle "fork"
    let lateFail = { mkPuzzle "fork" with PuzzleId = "2" }
    let score =
        { mkScore [] [] with
            CorrectPuzzles = ResizeArray<CsvPuzzleData>([ solvedP ])
            FailedPuzzles = ResizeArray<CsvPuzzleData * string>([ lateFail, "x" ])
            FirstMoveScored = 0
            FirstMoveCorrectIds = System.Collections.Generic.HashSet<string>([ "1"; "2" ]) }
    let stat = PuzzleThemes.breakdown score |> List.find (fun s -> s.Theme = "fork")
    Assert.Equal(2, stat.Total)
    Assert.Equal(1, stat.Correct)   // whole-line, despite the ids being present

[<Fact>]
let ``a test that does not track first moves keeps the whole-line verdict`` () =
    let solvedP = mkPuzzle "fork"
    let failedP = { mkPuzzle "fork" with PuzzleId = "2" }
    let score =
        { mkScore [] [] with
            CorrectPuzzles = ResizeArray<CsvPuzzleData>([ solvedP ])
            FailedPuzzles = ResizeArray<CsvPuzzleData * string>([ failedP, "x" ])
            // no first-move tracking at all: FirstMoveScored = 0 is the capability signal
            FirstMoveCorrectIds = System.Collections.Generic.HashSet<string>() }
    let stat = PuzzleThemes.breakdown score |> List.find (fun s -> s.Theme = "fork")
    Assert.Equal(2, stat.Total)
    Assert.Equal(1, stat.Correct)

// ---------------------------------------------------------------------------
// Slice key. writeThemeFiles used to group on (Type, ratingGroup) alone, while
// PuzzlePaired and PuzzleCrossEngine also key on Nodes and Filter.
// ---------------------------------------------------------------------------

[<Fact>]
let ``a multi-filter run does not compare a net against itself`` () =
    // `PuzzleFilter: "fork,pin"` gives one Score per filter theme per net. Merged into
    // one group, `baseline :: others` made the first comparison netA-on-fork against
    // netA-on-pin - one net against itself, on two different puzzle sets.
    let withFilter f net (score: Score) = { score with Filter = f; NeuralNet = net; Engine = net }
    let many n theme = List.replicate n theme
    let scores =
        [ mkScore (many 30 "fork") (many 10 "fork") |> withFilter "fork" "netA"
          mkScore (many 20 "fork") (many 20 "fork") |> withFilter "fork" "netB"
          mkScore (many 30 "pin") (many 10 "pin") |> withFilter "pin" "netA"
          mkScore (many 20 "pin") (many 20 "pin") |> withFilter "pin" "netB" ]
    let out = PuzzleThemes.writeThemeFiles "" "" [ "netA"; "netB" ] scores
    // netA is the baseline in both slices and netB the comparison, so netA must never
    // appear in the B slot - that is what a net compared against itself looked like
    Assert.DoesNotContain("B = netA", out.Summary)
    Assert.Contains("A = netA", out.Summary)
    Assert.Contains("B = netB", out.Summary)
    // and both slices are labelled, so two tables no longer share one title
    Assert.Contains("theme fork", out.Summary)
    Assert.Contains("theme pin", out.Summary)

// ---------------------------------------------------------------------------
// Null-bestmove sentinel. Board.PlayUciMove no-ops on a move it cannot parse, so
// "0000" used to leave the previous SAN in place and the EPD recorded the
// OPPONENT's setup move as the avoid-move.
// ---------------------------------------------------------------------------

[<Fact>]
let ``sanOfMovePlayed renders the sentinel as unknown, not as a move`` () =
    let board = Chess.Board()
    let san = PuzzleDataUtils.sanOfMovePlayed board "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4" PuzzleDataUtils.NullBestmove
    Assert.Equal("?", san)

[<Fact>]
let ``sanOfMovePlayed renders a real move as SAN`` () =
    let board = Chess.Board()
    let san = PuzzleDataUtils.sanOfMovePlayed board "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4" "e7e5"
    Assert.Equal("e5", san)

[<Fact>]
let ``an unplayable move yields unknown, not the previous move`` () =
    // PlayUciMove no-ops on ANYTHING it cannot match, so the sentinel was only one member
    // of the class: an illegal move, a stale echo, or a non-UCI token long enough to pass
    // the callers' `Length >= 4` guard all left the opponent's setup move as the answer.
    let board = Chess.Board()
    let command = "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4"
    for unplayable in [ "h8h1"; "(none)"; "zzzz" ] do
        Assert.Equal("?", PuzzleDataUtils.sanOfMovePlayed board command unplayable)

[<Fact>]
let ``the sentinel never yields the move already on the board`` () =
    // the exact failure: PlayCommands pushes e4, PlayUciMove "0000" does nothing, and
    // the last SAN is still the opponent's setup move
    let board = Chess.Board()
    let command = "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4"
    let real = PuzzleDataUtils.sanOfMovePlayed board command "e7e5"
    let sentinel = PuzzleDataUtils.sanOfMovePlayed board command PuzzleDataUtils.NullBestmove
    Assert.NotEqual<string>("e4", sentinel)
    Assert.NotEqual<string>(real, sentinel)
