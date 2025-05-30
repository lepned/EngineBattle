
module ChessLibrary.MoveGeneration

open System
open System.Text
open ChessLibrary.QBBOperations
open TypesDef.Position
open TypesDef.TMove

//reverse a binary string
let binaryReversed (binary:string) = System.String(Array.rev (binary.ToCharArray()))

let printBinaryView label (number : uint64) = 
  let binary = Convert.ToString(int64 number, 2) |> binaryReversed 
  printfn "%s\n%s" label binary

let inline GenRook(sq:int, occupation:uint64) =
  let occupation = occupation ^^^ (1UL <<< sq) // remove the selected piece from the occupation */
  ((0x8080808080808080UL >>> (63 ^^^ int (LSB((0x0101010101010101UL <<< sq) &&& (occupation ||| 0xFF00000000000000UL))))) &&& 
    (0x0101010101010101UL <<< int (MSB((0x8080808080808080UL >>> (63 ^^^ sq)) &&&  (occupation ||| 0x00000000000000FFUL))))) |||
   ((0xFF00000000000000UL >>> (63 ^^^ int (LSB((0x00000000000000FFUL <<< sq) &&&  (occupation ||| 0x8080808080808080UL))))) &&&
    (0x00000000000000FFUL <<< int (MSB((0xFF00000000000000UL >>> (63 ^^^ sq)) &&&  (occupation ||| 0x0101010101010101UL)))))
  
let inline GenBishop(sq:int, occupation:uint64) =
  let occupation = occupation ^^^ (1UL <<< sq)
  (((0x8040201008040201UL >>> (63 ^^^ int (LSB((0x8040201008040201UL <<< sq) &&& (occupation ||| 0xFF80808080808080UL))))) &&& 
    (0x8040201008040201UL <<< int (MSB((0x8040201008040201UL >>> (63 ^^^ sq)) &&& (occupation ||| 0x01010101010101FFUL))))) |||
    ((0x8102040810204081UL >>> (63 ^^^ int (LSB((0x8102040810204081UL <<< sq) &&& (occupation ||| 0xFF01010101010101UL))))) &&&
    (0x8102040810204081UL <<< int (MSB((0x8102040810204081UL >>> (63 ^^^ sq)) &&& (occupation ||| 0x80808080808080FFUL))))))

let BBPieces (piece: TPieceType, position: Position inref) =
    match piece with
    | TPieceType.PAWN -> PositionOps.pawns &position
    | TPieceType.KNIGHT -> PositionOps.knights &position
    | TPieceType.BISHOP -> PositionOps.bishops &position
    | TPieceType.ROOK -> PositionOps.rooks &position
    | TPieceType.QUEEN -> PositionOps.queens &position
    | TPieceType.KING -> PositionOps.kings &position
    | _ -> 0UL

let BBDestinations (piece: TPieceType, sq: int, occupation: uint64) =
    match piece with
    | TPieceType.KNIGHT -> KnightDest.[sq]
    | TPieceType.BISHOP -> GenBishop(sq, occupation)
    | TPieceType.ROOK -> GenRook(sq, occupation)
    | TPieceType.QUEEN -> (GenRook(sq, occupation) ||| GenBishop(sq, occupation))
    | TPieceType.KING -> KingDest.[sq]
    | _ -> 0UL

let InCheck(position: Position inref) =
    let occupations = PositionOps.occupation &position
    let opposing = position.PM ^^^ occupations
    let king = PositionOps.kings &position &&& position.PM
    let kingsq = int (LSB king)
    (((KnightDest.[kingsq] &&& PositionOps.knights &position) |||
    (GenRook(kingsq, occupations) &&& (PositionOps.rooks &position ||| PositionOps.queens &position)) |||
    (GenBishop(kingsq, occupations) &&& (PositionOps.bishops &position ||| PositionOps.queens &position)) |||
    ((((king <<< 9) &&& 0xFEFEFEFEFEFEFEFEUL) ||| ((king <<< 7) &&& 0x7F7F7F7F7F7F7F7FUL)) &&& PositionOps.pawns &position) |||
    (KingDest[kingsq] &&& PositionOps.kings &position)) &&& opposing)


let squareNumbersReversed =
    [
      [56; 57; 58; 59; 60; 61; 62; 63]
      [48; 49; 50; 51; 52; 53; 54; 55]
      [40; 41; 42; 43; 44; 45; 46; 47]
      [32; 33; 34; 35; 36; 37; 38; 39]
      [24; 25; 26; 27; 28; 29; 30; 31]
      [16; 17; 18; 19; 20; 21; 22; 23]
      [8; 9; 10; 11; 12; 13; 14; 15]
      [0; 1; 2; 3; 4; 5; 6; 7]
    ]

let inline isBitSet (number: uint64) (position: int) =
  (number &&& (1UL <<< position)) <> 0UL


let getPieceAndColorOnSquare(position: Position inref, sq : string) =   
    let mutable pos = position
    let opposing = PositionOps.opposing &pos
    let n = if pos.STM = 0uy then squareNameToNumberDictWhite[sq] else squareNameToNumberDictBlack[sq]
    let piece = TPieceType.Piece(n, &pos)      
    if piece <> 0UL then
        let black = 8uy    
        let color = if isBitSet opposing n then pos.STM ^^^ black else pos.STM
        let symbol = TPieceType.symbolFromPieceType piece pos.STM     
        if color = 0uy then
            symbol, "w"
        else
            symbol, "b"
    else
        Char.MinValue, ""
          

let PositionOpsToString(label: string, position: Position inref) =
    let sb = StringBuilder()
    let black = 8uy
    let add (msg:string) = sb.Append msg |> ignore
    sprintf "\n%s" label |> add
    sprintf "\n   board state" |> add
    let mutable pos = position
    let opposing = PositionOps.opposing &pos
    let q = if pos.STM = 0uy then "\n q" else "\n Q"
    let k = if pos.STM = 0uy then "  castling   k" else "  castling   K"
    if (pos.CastleFlags &&& 0x10uy) <> 0uy then add q  
    if (pos.CastleFlags &&& 0x20uy) <> 0uy then 
      if pos.CastleFlags &&& 0x10uy <> 0uy then
        add k
      else
        add ("\n  " + k)

    let chess960 = PositionOps.isFRC &pos
    for list in squareNumbersReversed do
      //add empty rows between ranks      
      sprintf "\n" |> add
      for i in list do
        let piece = TPieceType.Piece(i, &pos)      
        if piece <> 0UL then
          let color = if isBitSet opposing i then pos.STM ^^^ black else pos.STM
          let symbol = TPieceType.symbolFromPieceType piece color
          sprintf " %c" symbol |> add
        else
          sprintf " ." |> add
    let q = if pos.STM = 0uy then "\n Q" else "\n q"
    let k = if pos.STM = 0uy then "  castling   K" else "  castling   k"    
    if (pos.CastleFlags &&& 0x01uy) <> 0uy then add q
    if (pos.CastleFlags &&& 0x02uy) <> 0uy then 
      if pos.CastleFlags &&& 0x01uy <> 0uy then
        add k
      else
        add ("\n  " + k) 
        
    let epChar = char (byte 'a' + pos.EnPassant) |> string
    let epStr = if pos.STM = PositionOps.WHITE then (epChar + string 6) else (epChar + string 3)    
    add "\n EP: "
    add (" " + (if pos.EnPassant = 8uy then "-" else epStr))
    let moveNr = pos.Ply / 2uy + 1uy
    add "\n move count: "
    add (string pos.Count50 + " " + moveNr.ToString())
    let incheck = InCheck &position <> 0UL
    if incheck then
      let checkMsg = if pos.STM = 0uy then "\nwhite king in check" else "\nblack king in check"
      add checkMsg
    add (sprintf "\nIs FRC PositionOps: %b" chess960)
    sb.ToString()

let inline canKingReachLongRook (pos:Position inref) =    
    let kingSq = LSB (PositionOps.kings &pos &&& pos.PM) |> int    
    let rookLong = LSB (PositionOps.rooksM &pos) |> int // Long castling rook
    let occRow1WithoutKingAndRook = PositionOps.occupation &pos &&& QBBOperations.firstRank &&& (~~~((1UL <<< kingSq) ||| (1UL <<< rookLong)))    
    (LongRookKingMask[rookLong][kingSq] &&& occRow1WithoutKingAndRook) = 0UL

let inline canKingReachShortRook (pos:Position inref) =    
    let kingSq = LSB (PositionOps.kings &pos &&& pos.PM) |> int    
    let rookShort = MSB (PositionOps.rooksM &pos) |> int // short castling rook
    let occRow1WithoutKingAndRook = PositionOps.occupation &pos &&& QBBOperations.firstRank &&& (~~~((1UL <<< kingSq) ||| (1UL <<< rookShort)))
    ShortRookKingMask[rookShort][kingSq] &&& occRow1WithoutKingAndRook = 0UL

let generateQuiets (moves: TMove array inref) (index: int outref) (position : _ byref) isFRC =        
    let occupation = PositionOps.occupation &position
    
    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
      
      //generate moves for every piece of the same type of the side to move      
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB(pieces));
        
        // for every destinations on a free square generate a move
        let mutable destinations = ~~~occupation &&& BBDestinations(piece, square, occupation);
        while (destinations <> 0UL) do
            moves.[index] <-  { MoveType = piece; From = byte square; To = byte (LSB(destinations)); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

  //* one pawns push */
    let onePush = (((PositionOps.pawns &position &&& PositionOps.sideToMove &position) <<< 8) &&& ~~~occupation) &&& 0x00FFFFFFFFFFFFFFUL
    let mutable push1 = onePush
    while (push1 <> 0UL) do
      let square = int (LSB push1)
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 8); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push1 <- ClearLSB push1

      //* two pawns push */
    let mutable push2 = (onePush <<< 8) &&& ~~~occupation &&& 0x00000000FF000000UL    
    while (push2 <> 0UL) do
      let square = int (LSB push2);
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 16); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push2 <- ClearLSB push2

    // check if long castling is possible */
    if (PositionOps.CanCastleLM &position && canKingReachLongRook &position) then
        let longRookSq = 
          if position.STM = PositionOps.WHITE then
            position.RookInfo.WhiteQRInitPlacement
          else
            position.RookInfo.BlackQRInitPlacement
        let kingSq = LSB(PositionOps.kings &position &&& position.PM) |> int

        let opposing = PositionOps.opposing &position      
        let rookOcc = getRookMaskWithOccupancy kingSq 2 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSq 2 occupation
        
        let knightAttacks = (getKnightMask kingSq 2) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSq 2) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSq 2) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then          
          if isFRC then
            moves.[index] <-  { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = byte longRookSq; Promotion = TPieceType.EMPTY }
            index <- index + 1
          else
            moves.[index] <-  { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 2uy; Promotion = TPieceType.EMPTY }
            index <- index + 1       
          
    if (PositionOps.CanCastleSM &position && canKingReachShortRook &position) then
        let shortRookSq = MSB (PositionOps.rooksM &position) |> int
        let kingSq = LSB(PositionOps.kings &position &&& position.PM) |> int      
        let opposing = PositionOps.opposing &position
        let rookOcc = getRookMaskWithOccupancy kingSq 6 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSq 6 occupation      
        let knightAttacks = (getKnightMask kingSq 6) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSq 6) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSq 6) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then 
          if isFRC then
            moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = byte shortRookSq; Promotion = TPieceType.EMPTY }
            index <- index + 1            
          else
            moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 6uy; Promotion = TPieceType.EMPTY }
            index <- index + 1      


let generateQuietsInSpan (moves: TMove Span) (index: int outref) (position : _ inref) isFRC =            
    let occupation = PositionOps.occupation &position
    
    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
      
      //generate moves for every piece of the same type of the side to move      
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB(pieces));
        
        // for every destinations on a free square generate a move
        let mutable destinations = ~~~occupation &&& BBDestinations(piece, square, occupation);
        while (destinations <> 0UL) do
            moves.[index] <-  { MoveType = piece; From = byte square; To = byte (LSB(destinations)); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

  //* one pawns push */
    let onePush = (((PositionOps.pawns &position &&& PositionOps.sideToMove &position) <<< 8) &&& ~~~occupation) &&& 0x00FFFFFFFFFFFFFFUL
    let mutable push1 = onePush
    while (push1 <> 0UL) do
      let square = int (LSB push1)
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 8); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push1 <- ClearLSB push1

      //* two pawns push */
    let mutable push2 = (onePush <<< 8) &&& ~~~occupation &&& 0x00000000FF000000UL    
    while (push2 <> 0UL) do
      let square = int (LSB push2);
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 16); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push2 <- ClearLSB push2

    // check if long castling is possible */
    if (PositionOps.CanCastleLM &position && canKingReachLongRook &position) then        
        let longRookSq = 
          if position.STM = PositionOps.WHITE then
            position.RookInfo.WhiteQRInitPlacement
          else
            position.RookInfo.BlackQRInitPlacement
        let kingSq = LSB(PositionOps.kings &position &&& position.PM) |> int

        let opposing = PositionOps.opposing &position      
        let rookOcc = getRookMaskWithOccupancy kingSq 2 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSq 2 occupation
        
        let knightAttacks = (getKnightMask kingSq 2) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSq 2) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSq 2) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then          
          if isFRC then
            moves.[index] <-  { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = byte longRookSq; Promotion = TPieceType.EMPTY }
            index <- index + 1
            //moves.[index] <-  { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 2uy; Promotion = TPieceType.EMPTY }
            //index <- index + 1  
          else
            moves.[index] <-  { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 2uy; Promotion = TPieceType.EMPTY }
            index <- index + 1       
          
    if (PositionOps.CanCastleSM &position && canKingReachShortRook &position) then
        let shortRookSq = 
          if position.STM = PositionOps.WHITE then
            position.RookInfo.WhiteKRInitPlacement
          else
            position.RookInfo.BlackKRInitPlacement
        let kingSq = LSB(PositionOps.kings &position &&& position.PM) |> int      
        let opposing = PositionOps.opposing &position
        let rookOcc = getRookMaskWithOccupancy kingSq 6 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSq 6 occupation      
        let knightAttacks = (getKnightMask kingSq 6) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSq 6) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSq 6) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then 
          if isFRC then
            moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = byte shortRookSq; Promotion = TPieceType.EMPTY }
            index <- index + 1  
            //moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 6uy; Promotion = TPieceType.EMPTY }
            //index <- index + 1
          else
            moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 6uy; Promotion = TPieceType.EMPTY }
            index <- index + 1

let generateCaptures (moves: TMove array inref) (index: int outref) (position : _ byref) = 
    let mutable occupation = PositionOps.occupation &position
    let opposing = PositionOps.opposing &position
    
    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
    // generate moves for every piece of the same type of the side to move
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB pieces);
        // for every destinations on a free square generate a move
        let mutable destinations = opposing &&& BBDestinations(piece, square, occupation)
        while (destinations <> 0UL) do
            moves.[index] <-  { MoveType = piece ||| TPieceType.CAPTURE; From = byte square; To = byte (LSB destinations); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

    //right pawn captures (rpc)
    let pawns = PositionOps.pawns &position &&& PositionOps.sideToMove &position
    let mutable rpc = (pawns <<< 9) &&& 0x00FEFEFEFEFEFEFEUL &&& opposing
    while (rpc <> 0UL) do
      let square = int (LSB rpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 9); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      rpc <- ClearLSB rpc

    //left pawn captures (lpc)    
    let mutable lpc = (pawns <<< 7) &&& 0x007F7F7F7F7F7F7FUL &&& opposing
    while (lpc <> 0UL) do
      let square = int (LSB lpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      lpc <- ClearLSB lpc

    // Generate pawn promotions
    if (pawns &&& 0x00FF000000000000UL) <> 0UL then
      //left promo captures
      let mutable promoL = (pawns <<< 9) &&& 0xFE00000000000000UL &&& opposing
      while (promoL <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = LSB promoL
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 9UL); To = byte square; Promotion = piece }
          index <- index + 1
        promoL <- ClearLSB promoL
  
      //right promo captures
      let mutable promoR = (pawns <<< 7) &&& 0x7F00000000000000UL  &&& opposing
      while (promoR <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = int (LSB promoR)
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = piece }
          index <- index + 1
        promoR <- ClearLSB promoR
    
      // no capture promotions
      let mutable promoNoC = (pawns <<< 8) &&& ~~~occupation &&& 0xFF00000000000000UL
      while (promoNoC <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = int (LSB promoNoC)
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO; From = byte (square - 8); To = byte square; Promotion = piece }
          index <- index + 1
        promoNoC <- ClearLSB promoNoC

    if (position.EnPassant <> 8uy) then
      let mutable ep = pawns &&& EnPassant[int position.EnPassant]
      while (ep <> 0UL) do
        let square = int (LSB ep)        
        moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.EP ||| TPieceType.CAPTURE; From = byte square; To = (40uy + PositionOps.enPass(&position)); Promotion = TPieceType.EMPTY }
        index <- index + 1
        ep <- ClearLSB ep


let generateCapturesInSpan (moves: TMove Span) (index: int outref) (position : Position inref) =     
    //let moves = moves.Span
    let mutable occupation = PositionOps.occupation &position
    let opposing = PositionOps.opposing &position
    
    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
    // generate moves for every piece of the same type of the side to move
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB pieces);
        // for every destinations on a free square generate a move
        let mutable destinations = opposing &&& BBDestinations(piece, square, occupation)
        while (destinations <> 0UL) do
            moves.[index] <-  { MoveType = piece ||| TPieceType.CAPTURE; From = byte square; To = byte (LSB destinations); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

    //right pawn captures (rpc)
    let pawns = PositionOps.pawns &position &&& PositionOps.sideToMove &position
    let mutable rpc = (pawns <<< 9) &&& 0x00FEFEFEFEFEFEFEUL &&& opposing
    while (rpc <> 0UL) do
      let square = int (LSB rpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 9); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      rpc <- ClearLSB rpc

    //left pawn captures (lpc)    
    let mutable lpc = (pawns <<< 7) &&& 0x007F7F7F7F7F7F7FUL &&& opposing
    while (lpc <> 0UL) do
      let square = int (LSB lpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      lpc <- ClearLSB lpc

    // Generate pawn promotions
    if (pawns &&& 0x00FF000000000000UL) <> 0UL then
      //left promo captures
      let mutable promoL = (pawns <<< 9) &&& 0xFE00000000000000UL &&& opposing
      while (promoL <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = LSB promoL
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 9UL); To = byte square; Promotion = piece }
          index <- index + 1
        promoL <- ClearLSB promoL
  
      //right promo captures
      let mutable promoR = (pawns <<< 7) &&& 0x7F00000000000000UL  &&& opposing
      while (promoR <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = int (LSB promoR)
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = piece }
          index <- index + 1
        promoR <- ClearLSB promoR
    
      // no capture promotions
      let mutable promoNoC = (pawns <<< 8) &&& ~~~occupation &&& 0xFF00000000000000UL
      while (promoNoC <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = int (LSB promoNoC)
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO; From = byte (square - 8); To = byte square; Promotion = piece }
          index <- index + 1
        promoNoC <- ClearLSB promoNoC

    if (position.EnPassant <> 8uy) then
      let mutable ep = pawns &&& EnPassant[int position.EnPassant]
      while (ep <> 0UL) do
        let square = int (LSB ep)        
        moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.EP ||| TPieceType.CAPTURE; From = byte square; To = (40uy + PositionOps.enPass(&position)); Promotion = TPieceType.EMPTY }
        index <- index + 1
        ep <- ClearLSB ep

let makeMove (move : TMove inref) (position : Position byref) =
    let rookM = PositionOps.rooksM &position
    let rookO = PositionOps.rooksO &position
    let part = 1UL <<< int move.From
    let dest = 1UL <<< int move.To
    match (move.MoveType &&& TPieceType.PIECE_MASK) with
    | TPieceType.PAWN ->
        if (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then
            // EnPassant
            position.PM <- position.PM ^^^ (part ||| dest)
            position.P0 <- position.P0 ^^^ (part ||| dest)
            position.P0 <- position.P0 ^^^ (dest >>> 8) // delete the captured pawn
            position.EnPassant <- 8uy
        else
            let isCapture = (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
            if isCapture then
                // Delete the captured piece
                position.P0 <- position.P0 &&& ~~~dest
                position.P1 <- position.P1 &&& ~~~dest
                position.P2 <- position.P2 &&& ~~~dest
            if (move.MoveType &&& TPieceType.PROMO) <> TPieceType.EMPTY then
                position.PM <- position.PM ^^^ (part ||| dest)
                position.P0 <- position.P0 ^^^ part
                position.P0 <- position.P0 ||| (uint64 (int move.Promotion &&& 1) <<< int move.To)
                position.P1 <- position.P1 ||| (uint64 ((int move.Promotion >>> 1) &&& 1) <<< int move.To)
                position.P2 <- position.P2 ||| (uint64 (int move.Promotion >>> 2) <<< int move.To)
                position.EnPassant <- 8uy// clear enpassant
                if isCapture then
                    let shortOppRooks = 
                      if position.STM = PositionOps.WHITE then
                        position.RookInfo.BlackKRInitPlacement + 56uy
                      else
                        position.RookInfo.WhiteKRInitPlacement + 56uy
                    let longOppRooks = 
                      if position.STM = PositionOps.WHITE then
                        position.RookInfo.BlackQRInitPlacement + 56uy
                      else
                        position.RookInfo.WhiteQRInitPlacement + 56uy

                    if move.To = shortOppRooks then 
                      PositionOps.ResetCastleSO &position // captured the opponent king side rook
                    if move.To = longOppRooks then 
                      PositionOps.ResetCastleLO &position // captured the opponent queen side rook
            else // capture or push
                position.PM <- position.PM ^^^ (part ||| dest)
                position.P0 <- position.P0 ^^^ (part ||| dest)
                position.EnPassant <- 8uy // clear enpassant
                if move.To = move.From + 16uy && (EnPassantM.[int move.To &&& 0x07] &&& PositionOps.pawns &position &&& PositionOps.opposing &position) <> 0UL then
                    position.EnPassant <- byte (int move.To &&& 0x07) // save enpassant column
        position.Count50 <- 0uy
        PositionOps.changeSide &position
    | TPieceType.KNIGHT 
    | TPieceType.BISHOP 
    | TPieceType.ROOK 
    | TPieceType.QUEEN ->
        let isCapture = (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
        if isCapture then
            // Delete the captured piece
            position.P0 <- position.P0 &&& ~~~dest
            position.P1 <- position.P1 &&& ~~~dest
            position.P2 <- position.P2 &&& ~~~dest
        position.PM <- position.PM ^^^ (part ||| dest)
            //TODO: handle N, B, R & Q separately?
        position.P0 <- 
            if ((int move.MoveType &&& 1) <> 0) then 
              position.P0 ^^^ (part ||| dest) 
            else 
              position.P0 ^^^ 0UL
        position.P1 <- 
            if ((int move.MoveType &&& 2) <> 0) then 
              position.P1 ^^^ (part ||| dest) 
            else 
              position.P1 ^^^ 0UL
        position.P2 <- 
            if ((int move.MoveType &&& 4) <> 0) then 
              position.P2 ^^^ (part ||| dest) 
            else 
              position.P2 ^^^ 0UL
        position.EnPassant <- 8uy

        if (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.ROOK then                    
          let shortRooksM = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.WhiteKRInitPlacement 
              else
                position.RookInfo.BlackKRInitPlacement
          let longRooksM = 
            if position.STM = PositionOps.WHITE then
              position.RookInfo.WhiteQRInitPlacement 
            else
              position.RookInfo.BlackQRInitPlacement

          if (move.From = shortRooksM) then
              PositionOps.ResetCastleSM  &position; //king side rook moved
          if (move.From = longRooksM) then
              PositionOps.ResetCastleLM  &position; // queen side rook moved                    

        if (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
            let shortOppRooks = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.BlackKRInitPlacement + 56uy
              else
                position.RookInfo.WhiteKRInitPlacement + 56uy
            let longOppRooks = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.BlackQRInitPlacement + 56uy
              else
                position.RookInfo.WhiteQRInitPlacement + 56uy
            
            if move.To = shortOppRooks then 
              PositionOps.ResetCastleSO &position // captured the opponent king side rook
            if move.To = longOppRooks then 
              PositionOps.ResetCastleLO &position // captured the opponent queen side rook
        if (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
          position.Count50 <- 0uy
        else
          position.Count50 <- position.Count50 + 1uy
        PositionOps.changeSide(&position)

    | TPieceType.KING ->
        position.Count50 <- position.Count50 + 1uy
        if (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
            // Delete the captured piece
            position.P0 <- position.P0 &&& ~~~dest
            position.P1 <- position.P1 &&& ~~~dest
            position.P2 <- position.P2 &&& ~~~dest
    
        position.PM <- position.PM ^^^ (part ||| dest)
        position.P1 <- position.P1 ^^^ (part ||| dest)
        position.P2 <- position.P2 ^^^ (part ||| dest)
        //PositionOpsToString("Error", &PositionOps) |> printfn "Inside castling %s"
        let canCastleShort = PositionOps.CanCastleSM &position
        let canCastleLong = PositionOps.CanCastleLM &position
        PositionOps.ResetCastleSM &position // update the castle rights
        PositionOps.ResetCastleLM &position
        position.EnPassant <- 8uy

        if (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
            let shortOppRooks = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.BlackKRInitPlacement + 56uy
              else
                position.RookInfo.WhiteKRInitPlacement + 56uy
            let longOppRooks = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.BlackQRInitPlacement + 56uy
              else
                position.RookInfo.WhiteQRInitPlacement + 56uy

            if move.To = shortOppRooks && (position.CastleFlags &&& 0x20uy) <> 0uy then 
                PositionOps.ResetCastleSO &position // captured the opponent king side rook
            if move.To = longOppRooks && (position.CastleFlags &&& 0x10uy) <> 0uy then 
                PositionOps.ResetCastleLO &position // captured the opponent queen side rook
            position.Count50 <- 0uy

        elif (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
            let rookShortSq = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.WhiteKRInitPlacement |> int
              else
                position.RookInfo.BlackKRInitPlacement |> int
            let rookLongSq = 
              if position.STM = PositionOps.WHITE then
                position.RookInfo.WhiteQRInitPlacement |> int
              else
                position.RookInfo.BlackQRInitPlacement |> int

            if move.To = 6uy &&  move.To > move.From && canCastleShort then //short castling
                let maskRook = (1UL <<< rookShortSq) ||| (1UL <<< 5) // short castling                
                if rookShortSq <> 5 then
                  position.PM <- (position.PM ^^^ maskRook)
                  position.P2 <- (position.P2 ^^^ maskRook)
            elif move.To = 2uy &&  move.From > move.To then
                let maskRook = (1UL <<< (int)rookLongSq) ||| (1UL <<< 3)  // long castling
                if rookLongSq <> 3 then
                  position.PM <- (position.PM ^^^ maskRook)
                  position.P2 <- (position.P2 ^^^ maskRook)
            else
              //we are now in chess960 territory              
              let short = 0x0000000000000040UL
              let long =  0x000000000000004UL
              
              if move.To = (byte)rookShortSq && canCastleShort then // castle short
                position.PM <- position.PM ^^^ (dest ||| short)
                position.P1 <- position.P1 ^^^ (dest ||| short)
                position.P2 <- position.P2 ^^^ (dest ||| short)
                if rookShortSq <> 5 then
                  let maskRook = (1UL <<< rookShortSq) ||| (1UL <<< 5)
                  position.PM <- (position.PM ^^^ maskRook)
                  position.P2 <- (position.P2 ^^^ maskRook)
              elif move.To = (byte)rookLongSq && canCastleLong then
                position.PM <- position.PM ^^^ (dest ||| long)
                position.P1 <- position.P1 ^^^ (dest ||| long)
                position.P2 <- position.P2 ^^^ (dest ||| long)
                if rookLongSq <> 3 then
                  let maskRook = (1UL <<< rookLongSq) ||| (1UL <<< 3)
                  position.PM <- (position.PM ^^^ maskRook)
                  position.P2 <- (position.P2 ^^^ maskRook) 
              else                
                PositionOpsToString("Error", &position) |> printfn "Not a legal castle move %s"                 
                failwith $"Not a legal castle move {move} in this PositionOps {position}"
        PositionOps.changeSide(&position)

    |_ -> failwith $"Not a legal move {move} in this PositionOps {position}"


