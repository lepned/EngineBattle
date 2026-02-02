
module MoveGenerationTests

open Xunit
open System
open ChessLibrary
open ChessLibrary.MoveGeneration
open ChessLibrary.MoveTypes
open ChessLibrary.PositionTypes
open ChessLibrary.QBBOperations
open ChessLibrary.LowLevelUtilities.BoardHelper
open ChessLibrary.LowLevelUtilities

// Test for getPosFromFen
[<Fact>]
let ``getPosFromFen should parse FEN string into position`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let position = getPosFromFen (Some fen)
    Assert.Equal(PositionOps.WHITE, position.STM)
    Assert.Equal(0uy, position.Count50)
    Assert.Equal(0us, position.Ply)
  //--- Knight & King dispatchers ----------------------------------------------------
[<Theory>]
[<InlineData(0)>] // A1
[<InlineData(18)>] // C3
[<InlineData(63)>] // H8
let ``BBDestinations forwards KNIGHT and KING to their lookup tables`` (sq:int) =
  Assert.Equal(KnightDest.[sq], BBDestinations(TPieceType.KNIGHT, sq, 0UL))
  Assert.Equal(KingDest.[sq],   BBDestinations(TPieceType.KING,   sq, 0UL))

[<Fact>]
let ``BBPieces returns exactly the bitboard from PositionOps for each piece type`` () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let mutable pos = getPosFromFen (Some fen)
  let allPawns   = PositionOps.pawns &pos
  let allKnights = PositionOps.knights &pos
  Assert.Equal(allPawns,   BBPieces(TPieceType.PAWN,   &pos))
  Assert.Equal(allKnights, BBPieces(TPieceType.KNIGHT, &pos))

[<Fact>]
let ``FenParsing_ShouldReturnOriginalFenAfterRoundTrip`` () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let mutable pos = getPosFromFen (Some fen)
  let fenRoundTrip = posToFen pos
  Assert.Equal(fenRoundTrip, fen)

[<Fact>]
let ``FenParsing_Chess960_ShouldReturnOriginalFenAfterRoundTrip`` () =
  let fen = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
  let mutable pos = getPosFromFen (Some fen)
  let fenRoundTrip = posToFen pos
  Assert.Equal(fenRoundTrip, fen)

// ============================================================================
// Move Generation Tests
// ============================================================================

[<Fact>]
let ``generateCaptures should return zero captures from starting position`` () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let mutable pos = getPosFromFen (Some fen)
  let moves = Array.zeroCreate<TMove> 256
  let mutable index = 0
  generateCaptures (moves.AsSpan()) &index &pos
  Assert.Equal(0, index) // No captures possible from starting position

[<Fact>]
let ``generateQuiets should return 20 quiet moves from starting position`` () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let mutable pos = getPosFromFen (Some fen)
  let moves = Array.zeroCreate<TMove> 256
  let mutable index = 0
  generateQuiets (moves.AsSpan()) &index &pos false
  Assert.Equal(20, index) // 16 pawn moves + 4 knight moves

[<Fact>]
let ``generateCaptures should find captures in position with captures available`` () =
  // Position where white has captures available
  let fen = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
  let mutable pos = getPosFromFen (Some fen)
  let moves = Array.zeroCreate<TMove> 256
  let mutable index = 0
  generateCaptures (moves.AsSpan()) &index &pos
  Assert.True(index > 0, "Should find at least one capture (exd5)")

[<Fact>]
let ``generateQuiets and generateCaptures combined should produce all pseudo-legal moves`` () =
  let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  let mutable pos = getPosFromFen (Some fen)
  let moves = Array.zeroCreate<TMove> 256
  let mutable index = 0
  generateCaptures (moves.AsSpan()) &index &pos
  generateQuiets (moves.AsSpan()) &index &pos false
  // This is a well-known position (Kiwipete) - should have 48 pseudo-legal moves
  Assert.Equal(48, index)

// ============================================================================
// Perft Sanity Tests (quick verification of move generation correctness)
// ============================================================================

let private perftCount (board: Chess.Board) depth =
  let rec search depth =
    if depth = 0 then 1L
    else
      let mutable nodes = 0L
      let pos = board.Position
      let moves = board.GenerateMoves()
      for move in moves do
        let mutable m = move
        if not (BoardHelper.Illegal &m &pos) then
          board.MakeMove(&m)
          nodes <- nodes + search (depth - 1)
          board.UnMakeMove()
      nodes
  search depth

[<Fact>]
let ``Perft depth 1 from starting position should be 20`` () =
  let board = Chess.Board()
  board.LoadFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  let nodes = perftCount board 1
  Assert.Equal(20L, nodes)

[<Fact>]
let ``Perft depth 2 from starting position should be 400`` () =
  let board = Chess.Board()
  board.LoadFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  let nodes = perftCount board 2
  Assert.Equal(400L, nodes)

[<Fact>]
let ``Perft depth 3 from starting position should be 8902`` () =
  let board = Chess.Board()
  board.LoadFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  let nodes = perftCount board 3
  Assert.Equal(8902L, nodes)

[<Fact>]
let ``Perft depth 3 from Kiwipete position should be 97862`` () =
  // Kiwipete - a well-known perft test position
  let board = Chess.Board()
  board.LoadFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  let nodes = perftCount board 3
  Assert.Equal(97862L, nodes)

[<Fact>]
let ``Perft depth 3 from Chess960 position should be correct`` () =
  let board = Chess.Board()
  board.LoadFen("bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9")
  let nodes = perftCount board 3
  // Known correct value from Chess960TestPositions.fs: depth 3 = 12189
  Assert.Equal(12189L, nodes)


