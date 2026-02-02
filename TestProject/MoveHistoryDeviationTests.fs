module MoveHistoryDeviationTests

open System
open Xunit
open ChessLibrary

[<Fact>]
let ``FirstDeviationIndex ignores move numbers and opening and finds mismatch`` () =
  // Opening: 1. d4 d5 2. c4 e6 3. Nf3 c6 4. Bg5 f6 5. Bf4 f5 6. e3 Nf6 7. Nc3 Be7 8. Bd3 O-O
  let opening =
    [| "d4"; "d5"; "c4"; "e6"; "Nf3"; "c6"; "Bg5"; "f6"; "Bf4"; "f5"; "e3"; "Nf6"; "Nc3"; "Be7"; "Bd3"; "O-O" |]

  // Previous (first) game moves (SAN only, includes opening).
  let referenceSans =
    Array.append opening [| "h4"; "a6"; "a3" |]

  // Second game moveHistory starts after opening, includes move numbers.
  let moveHistory = "9. h3 a6 10. a3"
  let idx = GameAnalysis.MoveHistoryDeviation.firstDeviationIndex moveHistory opening referenceSans
  Assert.True(idx.HasValue)
  Assert.Equal(0, idx.Value)

[<Fact>]
let ``FirstDeviationIndex finds later deviation`` () =
  let opening = [| "e4"; "e5"; "Nf3"; "Nc6" |]
  let referenceSans = Array.append opening [| "Bb5"; "a6"; "Ba4" |]
  let moveHistory = "3. Bb5 a6 4. Bc4"
  let idx = GameAnalysis.MoveHistoryDeviation.firstDeviationIndex moveHistory opening referenceSans
  Assert.True(idx.HasValue)
  Assert.Equal(2, idx.Value)

[<Fact>]
let ``FirstDeviationIndex treats 9... as move number token`` () =
  // Opening ends on white's move, so moveHistory begins with black ("...") move.
  let opening = [| "d4" |]
  let referenceSans = Array.append opening [| "d5"; "c4"; "e6" |]
  let moveHistory = "1... d5 2. Nf3"
  let idx = GameAnalysis.MoveHistoryDeviation.firstDeviationIndex moveHistory opening referenceSans
  Assert.True(idx.HasValue)
  Assert.Equal(1, idx.Value)

[<Fact>]
let ``FirstDeviationIndex returns null when reference missing`` () =
  let opening = [| |]
  let moveHistory = "1. e4 e5 2. Nf3"
  let idx = GameAnalysis.MoveHistoryDeviation.firstDeviationIndex moveHistory opening Array.empty
  Assert.False(idx.HasValue)

[<Fact>]
let ``FirstDeviationIndex returns next-move index when current is longer but prefix matches`` () =
  let opening = [| "e4"; "e5" |]
  let referenceSans = Array.append opening [| "Nf3"; "Nc6" |]
  let moveHistory = "2. Nf3 Nc6 3. Bb5"
  let idx = GameAnalysis.MoveHistoryDeviation.firstDeviationIndex moveHistory opening referenceSans
  Assert.True(idx.HasValue)
  Assert.Equal(2, idx.Value)
