module BoardTests

open Xunit
open ChessLibrary.Chess

[<Fact>]
let ``ResetBoardState should reset board to initial state`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    board.ResetBoardState()
    Assert.Equal(0, board.PlyCount)
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", board.FEN())

[<Fact>]
let ``LoadFen should set position from FEN string`` () =
    let board = Board()
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    board.LoadFen(fen)
    Assert.Equal(fen, board.FEN())

[<Fact>]
let ``PlaySimpleShortSan should make a legal move and update FEN`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    Assert.Equal(1, board.PlyCount)
    Assert.Contains("e2e4", board.LongSANMovesPlayed)

[<Fact>]
let ``UnMakeMove should revert the last move`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    let fenAfterMove = board.FEN()
    board.UnMakeMove()   
    Assert.Equal(0, board.PlyCount)
    Assert.NotEqual<string>(fenAfterMove, board.FEN())

[<Fact>]
let ``GenerateMoves should return legal moves for initial position`` () =
    let board = Board()
    let moves = board.GenerateMoves()
    Assert.True(moves.Length > 0)

[<Fact>]
let ``IsMate should return false for initial position`` () =
    let board = Board()
    Assert.False(board.IsMate())

[<Fact>]
let ``ClaimThreeFoldRep should return false for new game`` () =
    let board = Board()
    Assert.False(board.ClaimThreeFoldRep())

[<Fact>]
let ``PlayLongSanMove should add move to LongSANMovesPlayed and update PlyCount`` () =
    let board = Board()
    board.PlayLongSanMove "e2e4"
    Assert.Equal(1, board.PlyCount)
    Assert.Contains("e2e4", board.LongSANMovesPlayed)

[<Fact>]
let ``TryGetNextMoveAndFen returns None for empty move list`` () =
    let board = Board()
    let fen = board.FEN()
    Assert.True(board.TryGetNextMoveAndFen(fen).IsNone)

[<Fact>]
let ``TryGetPreviousMoveAndFen returns None for empty move list`` () =
    let board = Board()
    let fen = board.FEN()
    Assert.True(board.TryGetPreviousMoveAndFen(fen).IsNone)