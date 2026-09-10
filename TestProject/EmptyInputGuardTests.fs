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
let ``the deviation fraction is a real ratio when games deviate`` () =
    // Guarding the zero case must not flatten the normal one. Two games between the same
    // players in the same opening, White choosing a different second move: at least one
    // deviation, so the guarded division must give devs/games, not the zero short-circuit.
    // (Two empty games could not tell the two apart: 0/2 and the short-circuit are both 0.)
    let pgn moves = sprintf "[Event \"t\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"1/2-1/2\"]\n\n%s 1/2-1/2\n" moves
    let g1 = FullPGNParser.parseFullPgnGame (pgn "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")
    let g2 = FullPGNParser.parseFullPgnGame (pgn "1. e4 e5 2. Nc3 Nc6 3. Bc4 Nf6")
    g1.GameMetaData.OpeningHash <- "same-opening"
    g2.GameMetaData.OpeningHash <- "same-opening"
    let _, _, _, _, _, fraction =
        DeviationAnalysis.analyzeDeviations [ g1; g2 ]
    Assert.False(Double.IsNaN fraction)
    Assert.True(fraction > 0.0, sprintf "expected a positive devs/games ratio, got %f" fraction)
