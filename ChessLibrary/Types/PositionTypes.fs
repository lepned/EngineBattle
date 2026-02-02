namespace ChessLibrary

open System
open QBBOperations

/// Position and bitboard operations for chess positions.
/// Contains the Position struct and PositionOps module.
module PositionTypes =

    [<Struct>]
    type Position =
      { mutable PM: uint64
        mutable P0: uint64
        mutable P1: uint64
        mutable P2: uint64
        mutable CastleFlags: byte
        mutable EnPassant: byte
        mutable Count50: byte
        mutable Rep: byte
        mutable STM: byte
        mutable Ply: uint16
        mutable RookInfo: RookPlacementInfo }
      with
        static member Default =
          { PM = 0UL; P0 = 0UL; P1 = 0UL; P2 = 0UL;
            CastleFlags = 0uy; EnPassant = 8uy; Count50 = 0uy; Rep = 0uy;
            STM = 0uy; Ply = 0us; RookInfo = RookPlacementInfo() }

    module PositionOps =
      let copy (board: Position inref) : Position =
        { PM = board.PM; P0 = board.P0; P1 = board.P1; P2 = board.P2;
          CastleFlags = board.CastleFlags; EnPassant = board.EnPassant;
          Count50 = board.Count50; Rep = board.Rep; STM = board.STM;
          Ply = board.Ply; RookInfo = board.RookInfo }
      let createEmptyTBoard () =
        { PM = 0UL; P0 = 0UL; P1 = 0UL; P2 = 0UL;
          CastleFlags = 0uy; EnPassant = 0uy; Count50 = 0uy; Rep = 0uy;
          STM = 0uy; Ply = 0us; RookInfo = RookPlacementInfo() }

      // (Additional inline functions for castling, occupancy, etc.)
      let WHITE = 0uy
      let BLACK = 8uy

      let inline CanCastleSM (position: Position inref) = (position.CastleFlags &&& 0x02uy) <> 0uy
      let inline CanCastleLM(position: Position inref) = (position.CastleFlags &&& 0x01uy) <> 0uy
      let inline CanCastleSO (position: Position inref) = (position.CastleFlags &&& 0x20uy) <> 0uy
      let inline CanCastleLO(position: Position inref) = (position.CastleFlags &&& 0x10uy) <> 0uy
      let inline ResetCastleSM(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xFDuy
      let inline ResetCastleLM(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xFEuy
      let inline ResetCastleSO(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xDFuy
      let inline ResetCastleLO(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xEFuy

       //these planes are used to calculate the bitboard of a particular kind of piece
       //      P2 P1 P0
       //       0  0  0    empty
       //       0  0  1    pawn
       //       0  1  0    knight
       //       0  1  1    bishop
       //       1  0  0    rook
       //       1  0  1    queen
       //       1  1  0    king

      let inline occupation (pos:Position inref) = pos.P0 ||| pos.P1 ||| pos.P2 // board occupation
      let inline pawns (pos:Position inref) = pos.P0 &&& ~~~pos.P1 &&& ~~~pos.P2 // all the pawns on the board
      let inline knights (pos:Position inref) = ~~~ pos.P0 &&& pos.P1 &&& ~~~ pos.P2
      let inline bishops (pos:Position inref) = pos.P0 &&& pos.P1
      let inline rooks (pos:Position inref) = ~~~ pos.P0 &&& ~~~ pos.P1 &&& pos.P2
      let inline queens (pos:Position inref) = pos.P0 &&& pos.P2
      let inline queenOrRooks (pos:Position inref) = ~~~ pos.P1 &&& pos.P2
      let inline queenOrBishops (pos:Position inref) = pos.P0 &&& (pos.P2 ||| pos.P1)
      let inline kings (pos:Position inref) = pos.P1 &&& pos.P2 // a bitboard with the 2 kings
      let inline sideToMove (pos:Position inref) = pos.PM
      let inline enPass (pos:Position inref) = pos.EnPassant
      let inline opposing (pos:Position inref) = pos.PM ^^^ (pos.P0 ||| pos.P1 ||| pos.P2)
      let inline numberOfPieces (pos:Position inref) = QBBOperations.Pop (occupation &pos) |> int32
      let inline changeSide (pos: Position outref) =
        pos.PM <- pos.PM ^^^ (occupation &pos) // update the side to move pieces
        pos.PM <- QBBOperations.RevBB pos.PM
        pos.P0 <- QBBOperations.RevBB pos.P0
        pos.P1 <- QBBOperations.RevBB pos.P1
        pos.P2 <- QBBOperations.RevBB pos.P2
        //let tempCastleFlags = pos.CastleFlags
        pos.CastleFlags <- byte ((pos.CastleFlags >>> 4) ||| (pos.CastleFlags <<< 4)) // roll the castle rights
        pos.STM <- pos.STM ^^^ BLACK // change the side to move

      let inline rooksM (pos:Position inref) = rooks &pos &&& pos.PM &&& firstRank //Rooks for player to move
      let inline rooksO (pos:Position inref) = rooks &pos &&& ~~~pos.PM &&& lastRank //Rooks for opponent

      let isStandardChessPosition (pos: Position inref) =
        let kings = kings &pos
        let myKsq = QBBOperations.MSB (kings &&& pos.PM)
        let oppKsq = QBBOperations.MSB (kings &&& ~~~pos.PM)
        let weCanCastle = CanCastleSM &pos || CanCastleLM &pos
        let opponentCanCastle = CanCastleSO &pos || CanCastleLO &pos
        let canCastle = weCanCastle || opponentCanCastle
        if not canCastle then
            true
        else
               // Check standard king positions
            let isNormalMyKingPosition = myKsq = 4UL
            let isNormalOppkKingPosition = oppKsq = 60UL

            // Initialize standardChess to true
            let mutable standardChess = true

            if pos.STM = WHITE then
              if CanCastleSM &pos then
                standardChess <- standardChess && pos.RookInfo.WhiteKRInitPlacement = 7uy && isNormalMyKingPosition

              // Check if White can castle queenside and if the rook is in the initial position
              if CanCastleLM &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteQRInitPlacement = 0uy && isNormalMyKingPosition

              // Check if black can castle kingside and if the rook is in the initial position
              if CanCastleSO &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackKRInitPlacement = 7uy && isNormalOppkKingPosition

              // Check if Black can castle queenside and if the rook is in the initial position
              if CanCastleLO &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackQRInitPlacement = 0uy && isNormalOppkKingPosition

            else
              if CanCastleSM &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackKRInitPlacement = 7uy && isNormalMyKingPosition

              // Check if black can castle queenside and if the rook is in the initial position
              if CanCastleLM &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackQRInitPlacement = 0uy && isNormalMyKingPosition

              // Check if White can castle kingside and if the rook is in the initial position
              if CanCastleSO &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteKRInitPlacement = 7uy && isNormalOppkKingPosition

              // Check if Black can castle queenside and if the rook is in the initial position
              if CanCastleLO &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteQRInitPlacement = 0uy && isNormalOppkKingPosition

            // Return true if all conditions for standard chess are met, otherwise false
            standardChess

      let isFRC (pos: Position inref) = isStandardChessPosition &pos |> not

      //get the rook index from the bitboard where 0 is a1 , 7 is h1, 56 is a8 and 63 is h8
      let getRookPositionsForCastling (pos:Position inref) =
        let mKingSq = QBBOperations.MSB (kings &pos &&& pos.PM &&& firstRank)
        let oKingSq = QBBOperations.MSB (kings &pos &&& ~~~pos.PM &&& lastRank)
        let rookM = rooksM &pos
        let rookO = rooksO &pos
        let ksM = QBBOperations.MSB rookM
        let qsM = QBBOperations.LSB rookM
        let ksO = QBBOperations.MSB rookO
        let qsO = QBBOperations.LSB rookO
        let nRooksM = QBBOperations.Pop rookM
        let nRooksO = QBBOperations.Pop rookO
        let mKr, mQr =
            if nRooksM > 1UL then
                (int ksM, int qsM)
            elif nRooksM = 1UL && mKingSq < ksM then
                (int ksM, 15)
            elif nRooksM = 1UL && mKingSq > ksM then
                (15, int qsM)
            else
                (15, 15)
        let oKr, oQr =
            if nRooksO > 1UL then
                (int ksO, int qsO)
            elif nRooksO = 1UL && oKingSq < ksO then
                (int ksO, 15)
            elif nRooksO = 1UL && oKingSq > ksO then
                (15, int qsO)
            else
                (15, 15)
        if pos.STM = WHITE then
          (mKr,mQr),(oKr,oQr)
        else
          (oKr,oQr),(mKr,mQr)



      let printBits debugMsg (value: uint64) =
        let binaryString = Convert.ToString(int64 value, 2)
        printfn "%s %d -> %s" debugMsg value binaryString
