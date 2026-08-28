module EmptyInputGuardTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes

// ---------------------------------------------------------------------------
// Two reporting paths divided by a count that can legitimately be zero: an empty
// PGN, and a player listed in the results who was never paired. 0/0 is NaN, and
// both values are printed - one of them through an int32 conversion, which turns
// NaN into a number that looks real in a table rather than an obvious blank.
//
// Pure functions, no engine and no files.
// ---------------------------------------------------------------------------

[<Fact>]
let ``an empty PGN gives a deviation fraction of zero, not NaN`` () =
    let _, _, _, _, _, fraction =
        DeviationAnalysis.analyzeDeviations Seq.empty
    Assert.False(Double.IsNaN fraction, "0 games must not divide by zero")
    Assert.Equal(0.0, fraction, 10)

[<Fact>]
let ``the deviation fraction is still a real ratio when there are games`` () =
    // Guarding the zero case must not flatten the normal one. With no games there is
    // nothing to deviate from either, so the honest check is that the guard leaves a
    // finite value on real input rather than short-circuiting everything to 0.
    let games = [ PgnGame.Empty 1; PgnGame.Empty 2 ]
    let _, _, _, _, _, fraction =
        DeviationAnalysis.analyzeDeviations games
    Assert.False(Double.IsNaN fraction)
    Assert.True(fraction >= 0.0 && fraction <= 1.0, sprintf "fraction out of range: %f" fraction)
