
module MoveGenerationTests

open Xunit
open System
open ChessLibrary
open ChessLibrary.MoveGeneration
open ChessLibrary.TypesDef.TMove
open ChessLibrary.TypesDef.Position
open ChessLibrary.TypesDef.Position.PositionOps
open ChessLibrary.QBBOperations
open ChessLibrary.LowLevelUtilities.BoardHelper

// Test for getPosFromFen
[<Fact>]
let ``getPosFromFen should parse FEN string into position`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let position = getPosFromFen (Some fen)
    Assert.Equal(PositionOps.WHITE, position.STM)
    Assert.Equal(0uy, position.Count50)
    Assert.Equal(0uy, position.Ply)
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


