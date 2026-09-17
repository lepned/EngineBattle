module OpeningDetectionTests

open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes

/// Builds a game whose plies carry the given comments. Only the comments matter here - the
/// opening rule never looks at the moves themselves.
let private gameWith (comments: string list) =
  let game = PgnGame.Empty 1
  comments
  |> List.iteri (fun i c ->
       game.Mainline.Add(
         { Ply = i
           MoveNumber = i / 2 + 1
           Color = (if i % 2 = 0 then "w" else "b")
           San = "e4"
           Comment = c
           Nags = []
           Variations = ResizeArray() }))
  game

let private search = "wv=0.31, d=24, mt=2100, tl=54000, n=900000"
let private book = "book, mb=+0+0+0+0+0,"

[<Fact>]
let ``search data decides: leading plies without it are the opening`` () =
  let g = gameWith [ book; book; book; book; search; search ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(4, plies)
  Assert.Equal(ChessUtilities.Opening.FromSearchData, source)

[<Fact>]
let ``the first ply carrying a tournament header instead of the book marker is still opening`` () =
  // EngineBattle attaches the pre-game tournament comment to ply 0, so its book marker is
  // missing even though it came from the book. Search data on later plies settles it.
  let tournamentHeader = "TournamentOptions: Rounds=500; Book=TCECbook_90_120.pgn;"
  let g = gameWith [ tournamentHeader; book; book; search; search ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(3, plies)
  Assert.Equal(ChessUtilities.Opening.FromSearchData, source)

[<Fact>]
let ``a later comment mentioning book cannot override search data`` () =
  // "out of book theory" at move 4 must not reclassify everything before it as opening.
  let g = gameWith [ search; search; search; "out of book theory here"; search ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(0, plies)
  Assert.Equal(ChessUtilities.Opening.FromSearchData, source)

[<Fact>]
let ``book exit marks the first choice, not the last book move`` () =
  // TCEC writes "{ Book exit }" on the ply AFTER the book. Treating it as a book move would
  // drop the first real choice of every game in the archive.
  let g = gameWith [ "Book exit"; ""; ""; "" ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(0, plies)
  Assert.Equal(ChessUtilities.Opening.FromBookMarker, source)

[<Fact>]
let ``book exit part way in leaves the plies before it as opening`` () =
  let g = gameWith [ ""; ""; ""; ""; "Book exit"; "" ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(4, plies)
  Assert.Equal(ChessUtilities.Opening.FromBookMarker, source)

[<Fact>]
let ``book moves without search data still end the opening at the leading run`` () =
  let g = gameWith [ book; book; ""; ""; "" ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(2, plies)
  Assert.Equal(ChessUtilities.Opening.FromBookMarker, source)

[<Fact>]
let ``a book mention far from the start does not swallow the opening`` () =
  // No search data anywhere, and the only "book" is at ply 4. Taking the last marked ply would
  // hide every choice before it; the leading run is empty, so nothing is excluded.
  let g = gameWith [ ""; ""; ""; ""; "book"; "" ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(0, plies)
  Assert.Equal(ChessUtilities.Opening.Unknown, source)

[<Fact>]
let ``no search data and no marker is reported as unknown, not as zero opening`` () =
  let g = gameWith [ ""; ""; "" ]
  let plies, source = ChessUtilities.Opening.plyCount g
  Assert.Equal(0, plies)
  Assert.Equal(ChessUtilities.Opening.Unknown, source)

[<Fact>]
let ``an abandoned game with no moves is not an unidentifiable opening`` () =
  let plies, source = ChessUtilities.Opening.plyCount (gameWith [])
  Assert.Equal(0, plies)
  Assert.Equal(ChessUtilities.Opening.NoMoves, source)
