/// The Book Evaluation filter (PuzzleEngineAnalysis.bookPositionPasses): a position passes when
/// every engine's eval lies in the size window and the engines agree, the spread taken of the
/// evals with their signs - engines that disagree on who is better do not agree.
module BookEvaluationFilterTests

open Xunit
open ChessLibrary.PuzzleEngineAnalysis

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
