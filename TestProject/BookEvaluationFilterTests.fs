/// The Book Evaluation filter (BookEvaluation.bookPositionPasses): a position passes when
/// every engine's eval lies in the size window and the engines agree, the spread taken of the
/// evals with their signs - engines that disagree on who is better do not agree.
module BookEvaluationFilterTests

open Xunit
open ChessLibrary.BookEvaluation

// the page's defaults: min 80, max 100, max diff 40
let private passes = bookPositionPasses 80.0 100.0 40.0

[<Fact>]
let ``engines that agree in the window pass`` () =
    Assert.True(passes [| 90.0; 85.0 |])
    Assert.True(passes [| -90.0; -85.0 |])   // Black better, and both say so

[<Fact>]
let ``engines that disagree on who is better do not pass`` () =
    // the sizes are equal, and they used to be all that was compared
    Assert.False(passes [| 90.0; -90.0 |])
    Assert.Equal(180.0, bookEvalSpread [| 90.0; -90.0 |])

[<Fact>]
let ``an eval outside the window fails, on either side`` () =
    Assert.False(passes [| 90.0; 70.0 |])     // too drawish
    Assert.False(passes [| 90.0; 110.0 |])    // too one-sided
    Assert.False(passes [| -90.0; -110.0 |])

[<Fact>]
let ``the window's edges belong to it, the spread's does not`` () =
    Assert.True(passes [| 80.0; 100.0 |])            // spread 20
    Assert.False(bookPositionPasses 80.0 100.0 20.0 [| 80.0; 100.0 |])   // spread must be below the limit

[<Fact>]
let ``one engine has no spread to check`` () =
    Assert.True(bookPositionPasses 80.0 100.0 10000.0 [| -95.0 |])
    Assert.Equal(0.0, bookEvalSpread [| -95.0 |])

[<Fact>]
let ``empty fields fall back, and one engine has no diff limit`` () =
    let f = parseFilter "" " " "" 2
    Assert.Equal((0.0, 1000.0, 10000.0), (f.MinEval, f.MaxEval, f.MaxDiff))
    Assert.Equal(10000.0, (parseFilter "80" "100" "40" 1).MaxDiff)
    Assert.Equal(40.0, (parseFilter "80" "100" "40" 2).MaxDiff)

[<Fact>]
let ``a book evaluated before gets this run's MaxEval, not a second one`` () =
    let raw = "[Event \"x\"]\n[MaxEval \"80 by Old, e2e4\"]\n[White \"a\"]\n\n1. e4 e5 *"
    let game = { ChessLibrary.FullPGNParser.parseFullPgnGame raw with Raw = raw }
    let r = ChessLibrary.PGNTypes.PgnEvaluationResult.Create(game, 95.0, 0.0, "g1f3", "New", "")
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "eb_bookeval_%s.pgn" (System.Guid.NewGuid().ToString("N")))
    try
        writePgns path [ r ]
        let text = System.IO.File.ReadAllText path
        Assert.DoesNotContain("by Old", text)
        Assert.Contains("[MaxEval \"95 by New, g1f3\"]", text)
        Assert.StartsWith("[Event \"x\"]", text)
        Assert.Contains("1. e4 e5 *", text)
    finally System.IO.File.Delete path

[<Fact>]
let ``a wrapped comment that starts a line with a bracket stays in the movetext`` () =
    let raw = "[Event \"x\"]\n[White \"a\"]\n\n1. e4 { book move,\n[%clk 0:03:00] } e5 2. Nf3 ; rest of line\nNc6 *"
    let game = { ChessLibrary.FullPGNParser.parseFullPgnGame raw with Raw = raw }
    let r = ChessLibrary.PGNTypes.PgnEvaluationResult.Create(game, 95.0, 0.0, "g1f3", "New", "")
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "eb_bookeval_%s.pgn" (System.Guid.NewGuid().ToString("N")))
    try
        writePgns path [ r ]
        let back = ChessLibrary.FullPGNParser.parseFullPgnGame (System.IO.File.ReadAllText path)
        Assert.Equal<string list>([ "e4"; "e5"; "Nf3"; "Nc6" ], back.Mainline |> Seq.map (fun m -> m.San) |> List.ofSeq)
        Assert.Equal("a", back.GameMetaData.White)
    finally System.IO.File.Delete path
