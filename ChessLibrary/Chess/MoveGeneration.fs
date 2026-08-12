
module ChessLibrary.MoveGeneration

open System
open System.Text
open ChessLibrary.QBBOperations
open PositionTypes
open MoveTypes

//reverse a binary string
let binaryReversed (binary:string) = System.String(Array.rev (binary.ToCharArray()))

let printBinaryView label (number : uint64) = 
  let binary = Convert.ToString(int64 number, 2) |> binaryReversed 
  printfn "%s\n%s" label binary

// GenRook/GenBishop return standard attack-set semantics: the origin square is NOT
// included. (The raw QBB span construction keeps the origin bit; it is masked out here
// so attack-map uses — e.g. the LegalityContext king-danger map — need no per-call
// compensation. All other consumers mask the origin anyway via ~occupation/opposing.)
let inline GenRook(sq:int, occupation:uint64) =
  let occupation = occupation ^^^ (1UL <<< sq) // remove the selected piece from the occupation */
  (((0x8080808080808080UL >>> (63 ^^^ int (LSB((0x0101010101010101UL <<< sq) &&& (occupation ||| 0xFF00000000000000UL))))) &&&
    (0x0101010101010101UL <<< int (MSB((0x8080808080808080UL >>> (63 ^^^ sq)) &&&  (occupation ||| 0x00000000000000FFUL))))) |||
   ((0xFF00000000000000UL >>> (63 ^^^ int (LSB((0x00000000000000FFUL <<< sq) &&&  (occupation ||| 0x8080808080808080UL))))) &&&
    (0x00000000000000FFUL <<< int (MSB((0xFF00000000000000UL >>> (63 ^^^ sq)) &&&  (occupation ||| 0x0101010101010101UL)))))) &&& ~~~(1UL <<< sq)

let inline GenBishop(sq:int, occupation:uint64) =
  let occupation = occupation ^^^ (1UL <<< sq)
  ((((0x8040201008040201UL >>> (63 ^^^ int (LSB((0x8040201008040201UL <<< sq) &&& (occupation ||| 0xFF80808080808080UL))))) &&&
    (0x8040201008040201UL <<< int (MSB((0x8040201008040201UL >>> (63 ^^^ sq)) &&& (occupation ||| 0x01010101010101FFUL))))) |||
    ((0x8102040810204081UL >>> (63 ^^^ int (LSB((0x8102040810204081UL <<< sq) &&& (occupation ||| 0xFF01010101010101UL))))) &&&
    (0x8102040810204081UL <<< int (MSB((0x8102040810204081UL >>> (63 ^^^ sq)) &&& (occupation ||| 0x80808080808080FFUL))))))) &&& ~~~(1UL <<< sq)

let inline BBPieces (piece: TPieceType, position: Position inref) =
    match piece with
    | TPieceType.PAWN -> PositionOps.pawns &position
    | TPieceType.KNIGHT -> PositionOps.knights &position
    | TPieceType.BISHOP -> PositionOps.bishops &position
    | TPieceType.ROOK -> PositionOps.rooks &position
    | TPieceType.QUEEN -> PositionOps.queens &position
    | TPieceType.KING -> PositionOps.kings &position
    | _ -> 0UL

let inline BBDestinations (piece: TPieceType, sq: int, occupation: uint64) =
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

/// Full rook rays from each square on an empty board (excludes the square itself)
let rookRays = Array.init 64 (fun sq -> GenRook(sq, 1UL <<< sq))
/// Full bishop rays from each square on an empty board (excludes the square itself)
let bishopRays = Array.init 64 (fun sq -> GenBishop(sq, 1UL <<< sq))

/// betweenBB.[from * 64 + to] = squares strictly between two aligned squares (0 if not aligned).
/// lineBB.[from * 64 + to] = the full rank/file/diagonal through both squares incl. endpoints (0 if not aligned).
let betweenBB, lineBB =
    let between = Array.zeroCreate<uint64> 4096
    let line = Array.zeroCreate<uint64> 4096
    let dirs = [| (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1) |]
    for sq1 in 0 .. 63 do
        let f1, r1 = sq1 % 8, sq1 / 8
        for (df, dr) in dirs do
            let mutable f = f1 + df
            let mutable r = r1 + dr
            let mutable path = 0UL
            while f >= 0 && f < 8 && r >= 0 && r < 8 do
                between.[sq1 * 64 + (r * 8 + f)] <- path
                path <- path ||| (1UL <<< (r * 8 + f))
                f <- f + df
                r <- r + dr
    let axes = [| (1, 0); (0, 1); (1, 1); (1, -1) |]
    for sq1 in 0 .. 63 do
        let f1, r1 = sq1 % 8, sq1 / 8
        for (df, dr) in axes do
            let mutable lineMask = 1UL <<< sq1
            for s in [| 1; -1 |] do
                let mutable f = f1 + df * s
                let mutable r = r1 + dr * s
                while f >= 0 && f < 8 && r >= 0 && r < 8 do
                    lineMask <- lineMask ||| (1UL <<< (r * 8 + f))
                    f <- f + df * s
                    r <- r + dr * s
            let mutable rest = lineMask &&& ~~~(1UL <<< sq1)
            while rest <> 0UL do
                line.[sq1 * 64 + int (LSB rest)] <- lineMask
                rest <- ClearLSB rest
    between, line

/// Full line through a square in one direction pair (both ways, incl. the square itself)
let private buildLineDir (df: int) (dr: int) =
    Array.init 64 (fun sq ->
        let f0, r0 = sq % 8, sq / 8
        let mutable mask = 1UL <<< sq
        for s in [| 1; -1 |] do
            let mutable f = f0 + df * s
            let mutable r = r0 + dr * s
            while f >= 0 && f < 8 && r >= 0 && r < 8 do
                mask <- mask ||| (1UL <<< (r * 8 + f))
                f <- f + df * s
                r <- r + dr * s
        mask)

/// Diagonal through each square in the +9 direction (up-right in the relative frame)
let diagLinesNE = buildLineDir 1 1
/// Diagonal through each square in the +7 direction (up-left in the relative frame)
let diagLinesNW = buildLineDir -1 1

/// Reference per-move legality test (moved verbatim from BoardHelper.Illegal, which now
/// delegates here). Materializes the post-move occupancy and probes all attacks to the king.
/// Kept as the exact reference: isIllegalCtx falls back to it for EP and castle moves and is
/// differential-tested against it.
let Illegal (move: TMove inref) (position: Position inref) =
    let from = 1UL <<< int move.From
    let mutable toSq = 1UL <<< int move.To
    let mutable king = 0UL
    let mutable kingsq = 0
    let mutable newoccupation = ((PositionOps.occupation &position) ^^^ from) ||| toSq
    let mutable newopposing = (PositionOps.opposing &position) &&& ~~~toSq
    let mutable legal = true

    if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
       let kings = PositionOps.kings &position
       let kingExt = kings &&& PositionOps.sideToMove &position
       king <- toSq
       kingsq <- int move.To
       // castle rooks come from RookInfo, not from scanning back-rank rooks (a second own
       // rook on the back rank would otherwise be mistaken for the castle rook)
       let shortSq =
         if position.STM = PositionOps.WHITE then int position.RookInfo.WhiteKRInitPlacement
         else int position.RookInfo.BlackKRInitPlacement
       let longSq =
         if position.STM = PositionOps.WHITE then int position.RookInfo.WhiteQRInitPlacement
         else int position.RookInfo.BlackQRInitPlacement
       if longSq <= 7 && int move.To = longSq then //chess960 long castling: king lands on c1
         let rookBit = 1UL <<< longSq
         //remove the king and rook from the old occupation
         newoccupation <- newoccupation &&& ~~~kingExt &&& ~~~rookBit
         king <- 1UL <<< 2
         kingsq <- 2
         //add the king to the new occupation
         newoccupation <- newoccupation ||| king
       elif shortSq <= 7 && int move.To = shortSq then //chess960 short castling: king lands on g1
         let rookBit = 1UL <<< shortSq
         //remove the king and rook from the old occupation
         newoccupation <- newoccupation &&& ~~~kingExt &&& ~~~rookBit
         king <- 1UL <<< 6
         kingsq <- 6
         //add the king to the new occupation
         newoccupation <- newoccupation ||| king

    elif legal && (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.KING then
        king <- toSq
        kingsq <- int move.To
        if (kingsq > 63) then
          legal <- false

    else
        king <- PositionOps.kings &position &&& PositionOps.sideToMove &position
        kingsq <- int (LSB king)
        if (kingsq > 63) then
          legal <- false
        if legal && (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then
            let epmask = toSq >>> 8
            newopposing <- newopposing ^^^ epmask
            newoccupation <- newoccupation ^^^ epmask
    if legal then
      let mutable mask = KnightDest.[kingsq] &&& newopposing
      if legal && mask <> 0UL && (mask &&& PositionOps.knights &position) > 0UL then
          legal <- false

      mask <- (((king <<< 9) &&& 0xFEFEFEFEFEFEFEFEUL) ||| ((king <<< 7) &&& 0x7F7F7F7F7F7F7F7FUL)) &&& newopposing
      if legal && mask <> 0UL && (mask &&& PositionOps.pawns &position) > 0UL then
          legal <- false

      mask <- PositionOps.queenOrBishops &position &&& newopposing
      if legal && mask <> 0UL && (mask &&& GenBishop(kingsq, newoccupation)) > 0UL then
          legal <- false

      mask <- PositionOps.queenOrRooks &position &&& newopposing
      if legal && mask <> 0UL && (mask &&& GenRook(kingsq, newoccupation)) > 0UL then
          legal <- false

      if legal then
        mask <- newopposing &&& KingDest.[kingsq]
        mask <> 0UL && (mask &&& PositionOps.kings &position) > 0UL
      else
        true
    else
      true

/// Per-position legality data computed once, replacing per-move Illegal probes when a whole
/// move list is filtered. Checkers/pins/danger are all in the QBB side-to-move-relative frame.
[<Struct>]
type LegalityContext =
    { KingSq: int
      Checkers: uint64
      CheckMask: uint64
      Pinned: uint64
      KingDanger: uint64 }

let createLegalityContext (position: Position inref) =
    let occ = PositionOps.occupation &position
    let own = PositionOps.sideToMove &position
    let opposing = PositionOps.opposing &position
    let king = PositionOps.kings &position &&& own
    if king = 0UL then
        // malformed position (no own king): everything is illegal, matching Illegal's kingsq guard
        { KingSq = 64; Checkers = 0UL; CheckMask = 0UL; Pinned = 0UL; KingDanger = 0xFFFFFFFFFFFFFFFFUL }
    else
        let kingSq = int (LSB king)
        let checkers =
            ((KnightDest.[kingSq] &&& PositionOps.knights &position) |||
             (GenRook(kingSq, occ) &&& PositionOps.queenOrRooks &position) |||
             (GenBishop(kingSq, occ) &&& PositionOps.queenOrBishops &position) |||
             ((((king <<< 9) &&& 0xFEFEFEFEFEFEFEFEUL) ||| ((king <<< 7) &&& 0x7F7F7F7F7F7F7F7FUL)) &&& PositionOps.pawns &position)) &&& opposing
        // Non-king moves must land inside this mask: anywhere when not in check, on the
        // checker or a blocking square in single check, nowhere in double check.
        let checkMask =
            if checkers = 0UL then 0xFFFFFFFFFFFFFFFFUL
            elif ClearLSB checkers = 0UL then betweenBB.[kingSq * 64 + int (LSB checkers)] ||| checkers
            else 0UL
        let enemyRQ = PositionOps.queenOrRooks &position &&& opposing
        let enemyBQ = PositionOps.queenOrBishops &position &&& opposing
        // pinned = own pieces that are the single blocker between the king and an enemy slider
        let mutable pinned = 0UL
        let mutable snipers = (rookRays.[kingSq] &&& enemyRQ) ||| (bishopRays.[kingSq] &&& enemyBQ)
        while snipers <> 0UL do
            let sniperSq = int (LSB snipers)
            let btw = betweenBB.[kingSq * 64 + sniperSq] &&& occ
            if btw <> 0UL && ClearLSB btw = 0UL && (btw &&& own) <> 0UL then
                pinned <- pinned ||| btw
            snipers <- ClearLSB snipers
        // enemy attack map with our king removed from occupancy, so a king retreating
        // along a slider's check ray is still seen as attacked
        let occNoKing = occ ^^^ king
        let enemyPawns = PositionOps.pawns &position &&& opposing
        let mutable danger =
            ((enemyPawns >>> 9) &&& 0x7F7F7F7F7F7F7F7FUL) ||| ((enemyPawns >>> 7) &&& 0xFEFEFEFEFEFEFEFEUL)
        let enemyKing = PositionOps.kings &position &&& opposing
        if enemyKing <> 0UL then
            danger <- danger ||| KingDest.[int (LSB enemyKing)]
        let mutable enemyKnights = PositionOps.knights &position &&& opposing
        while enemyKnights <> 0UL do
            danger <- danger ||| KnightDest.[int (LSB enemyKnights)]
            enemyKnights <- ClearLSB enemyKnights
        let mutable rq = enemyRQ
        while rq <> 0UL do
            danger <- danger ||| GenRook(int (LSB rq), occNoKing)
            rq <- ClearLSB rq
        let mutable bq = enemyBQ
        while bq <> 0UL do
            danger <- danger ||| GenBishop(int (LSB bq), occNoKing)
            bq <- ClearLSB bq
        { KingSq = kingSq; Checkers = checkers; CheckMask = checkMask; Pinned = pinned; KingDanger = danger }

/// Context-based legality test. Must agree with Illegal for every move produced by
/// generateCaptures/generateQuiets (differential-tested in MoveGenLegalityTests).
/// EP and castle moves delegate to the reference test: both are rare and carry the
/// tricky edge cases (horizontal EP pin, FRC castling occupancy).
let isIllegalCtx (ctx: LegalityContext inref) (move: TMove inref) (position: Position inref) =
    if (move.MoveType &&& (TPieceType.CASTLE ||| TPieceType.EP)) <> TPieceType.EMPTY then
        Illegal &move &position
    elif (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.KING then
        int move.To > 63 || (ctx.KingDanger &&& (1UL <<< int move.To)) <> 0UL
    else
        let toBit = 1UL <<< int move.To
        if (ctx.Pinned &&& (1UL <<< int move.From)) <> 0UL then
            (lineBB.[ctx.KingSq * 64 + int move.From] &&& toBit &&& ctx.CheckMask) = 0UL
        else
            (toBit &&& ctx.CheckMask) = 0UL


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
    let moveNr = pos.Ply / 2us + 1us
    add "\n move count: "
    add (string pos.Count50 + " " + moveNr.ToString())
    let incheck = InCheck &position <> 0UL
    if incheck then
      let checkMsg = if pos.STM = 0uy then "\nwhite king in check" else "\nblack king in check"
      add checkMsg
    add (sprintf "\nIs FRC PositionOps: %b" chess960)
    sb.ToString()

// The castle rook comes from RookInfo (its recorded initial placement), NOT from
// MSB/LSB over all own back-rank rooks: a second own rook maneuvered to the back rank
// outside the castle rook would otherwise be mistaken for it and suppress a legal castle.
let inline canKingReachLongRook (pos:Position inref) =
    let kingSq = LSB (PositionOps.kings &pos &&& pos.PM) |> int
    let rookLong =
      if pos.STM = PositionOps.WHITE then int pos.RookInfo.WhiteQRInitPlacement
      else int pos.RookInfo.BlackQRInitPlacement
    if rookLong > 7 || kingSq > 7 then false
    else
      let occRow1WithoutKingAndRook = PositionOps.occupation &pos &&& QBBOperations.firstRank &&& (~~~((1UL <<< kingSq) ||| (1UL <<< rookLong)))
      (LongRookKingMask[rookLong][kingSq] &&& occRow1WithoutKingAndRook) = 0UL

let inline canKingReachShortRook (pos:Position inref) =
    let kingSq = LSB (PositionOps.kings &pos &&& pos.PM) |> int
    let rookShort =
      if pos.STM = PositionOps.WHITE then int pos.RookInfo.WhiteKRInitPlacement
      else int pos.RookInfo.BlackKRInitPlacement
    if rookShort > 7 || kingSq > 7 then false
    else
      let occRow1WithoutKingAndRook = PositionOps.occupation &pos &&& QBBOperations.firstRank &&& (~~~((1UL <<< kingSq) ||| (1UL <<< rookShort)))
      ShortRookKingMask[rookShort][kingSq] &&& occRow1WithoutKingAndRook = 0UL

let generateQuiets (moves: TMove Span) (index: int outref) (position : _ inref) isFRC =
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
          else
            moves.[index] <- { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSq; To = 6uy; Promotion = TPieceType.EMPTY }
            index <- index + 1

let generateCaptures (moves: TMove Span) (index: int outref) (position : Position inref) =
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

/// Legal-only variant of generateQuiets: destinations are masked with the LegalityContext
/// during generation (checkMask for unpinned pieces, pin line for pinned ones, ~KingDanger
/// for the king), so no post-generation legality filtering is needed. Castling and EP are
/// still routed through the reference Illegal test (rare, bit-exact parity).
/// Output must equal the old pseudo-legal list filtered by Illegal, in the same order —
/// differential-tested in MoveGenLegalityTests.
let generateLegalQuiets (moves: TMove Span) (index: int outref) (position: Position inref) isFRC (ctx: LegalityContext inref) =
    let occupation = PositionOps.occupation &position
    let checkMask = ctx.CheckMask
    let pinned = ctx.Pinned
    let kingSq = ctx.KingSq &&& 63

    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB(pieces))
        let legalMask =
          if piece = TPieceType.KING then ~~~ctx.KingDanger
          elif (pinned &&& (1UL <<< square)) <> 0UL then checkMask &&& lineBB.[kingSq * 64 + square]
          else checkMask
        let mutable destinations = ~~~occupation &&& BBDestinations(piece, square, occupation) &&& legalMask
        while (destinations <> 0UL) do
            moves.[index] <- { MoveType = piece; From = byte square; To = byte (LSB(destinations)); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

    // pawn pushes: unpinned pawns batched; a pinned pawn can only push when pinned along
    // the king's file, so those fold into the same batch (their push stays on the pin line
    // by construction) and emission order matches the pseudo-legal generator
    let ownPawns = PositionOps.pawns &position &&& PositionOps.sideToMove &position
    let kingFileMask = 0x0101010101010101UL <<< (kingSq &&& 7)
    let pushablePawns = (ownPawns &&& ~~~pinned) ||| (ownPawns &&& pinned &&& kingFileMask)
    // the single-push set stays unmasked for deriving double pushes: the intermediate
    // square need not be inside the check mask
    let onePush = ((pushablePawns <<< 8) &&& ~~~occupation) &&& 0x00FFFFFFFFFFFFFFUL
    let mutable push1 = onePush &&& checkMask
    while (push1 <> 0UL) do
      let square = int (LSB push1)
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 8); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push1 <- ClearLSB push1

    let mutable push2 = (onePush <<< 8) &&& ~~~occupation &&& 0x00000000FF000000UL &&& checkMask
    while (push2 <> 0UL) do
      let square = int (LSB push2)
      moves.[index] <-  { MoveType = TPieceType.PAWN; From = byte (square - 16); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      push2 <- ClearLSB push2

    // castling: same generation-time attack checks as generateQuiets, then the emitted
    // move must additionally pass the reference Illegal test (bit-exact with old pipeline)
    if (PositionOps.CanCastleLM &position && canKingReachLongRook &position) then
        let longRookSq =
          if position.STM = PositionOps.WHITE then
            position.RookInfo.WhiteQRInitPlacement
          else
            position.RookInfo.BlackQRInitPlacement
        let kingSqC = LSB(PositionOps.kings &position &&& position.PM) |> int
        let opposing = PositionOps.opposing &position
        let rookOcc = getRookMaskWithOccupancy kingSqC 2 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSqC 2 occupation
        let knightAttacks = (getKnightMask kingSqC 2) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSqC 2) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSqC 2) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then
          let mutable mv =
            if isFRC then
              { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSqC; To = byte longRookSq; Promotion = TPieceType.EMPTY }
            else
              { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSqC; To = 2uy; Promotion = TPieceType.EMPTY }
          if not (Illegal &mv &position) then
            moves.[index] <- mv
            index <- index + 1

    if (PositionOps.CanCastleSM &position && canKingReachShortRook &position) then
        let shortRookSq =
          if position.STM = PositionOps.WHITE then
            position.RookInfo.WhiteKRInitPlacement
          else
            position.RookInfo.BlackKRInitPlacement
        let kingSqC = LSB(PositionOps.kings &position &&& position.PM) |> int
        let opposing = PositionOps.opposing &position
        let rookOcc = getRookMaskWithOccupancy kingSqC 6 occupation
        let bishopOcc = getBishopMaskWithOccupancy kingSqC 6 occupation
        let knightAttacks = (getKnightMask kingSqC 6) &&& PositionOps.knights &position
        let pawnAttacks = (getPawnMask kingSqC 6) &&& PositionOps.pawns &position
        let kingAttacks = (getKingMask kingSqC 6) &&& PositionOps.kings &position
        let rookBishop = (rookOcc &&& PositionOps.queenOrRooks &position) ||| (bishopOcc &&& PositionOps.queenOrBishops &position)
        let attacks = (rookBishop ||| knightAttacks ||| pawnAttacks ||| kingAttacks) &&& opposing
        if attacks = 0UL then
          let mutable mv =
            if isFRC then
              { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSqC; To = byte shortRookSq; Promotion = TPieceType.EMPTY }
            else
              { MoveType = TPieceType.KING ||| TPieceType.CASTLE; From = byte kingSqC; To = 6uy; Promotion = TPieceType.EMPTY }
          if not (Illegal &mv &position) then
            moves.[index] <- mv
            index <- index + 1

/// Legal-only variant of generateCaptures (see generateLegalQuiets for the masking rules).
/// Pinned pawns fold into the batched capture shifts only when their pin line matches the
/// capture direction (+9 → NE diagonal through the king, +7 → NW diagonal).
let generateLegalCaptures (moves: TMove Span) (index: int outref) (position: Position inref) (ctx: LegalityContext inref) =
    let occupation = PositionOps.occupation &position
    let opposing = PositionOps.opposing &position
    let checkMask = ctx.CheckMask
    let pinned = ctx.Pinned
    let kingSq = ctx.KingSq &&& 63

    // generate moves from king to knight
    for piece in TPieceType.pieceTraversal do
      let mutable pieces = BBPieces(piece, &position) &&& PositionOps.sideToMove &position
      while (pieces <> 0UL) do
        let square = int (LSB pieces)
        let legalMask =
          if piece = TPieceType.KING then ~~~ctx.KingDanger
          elif (pinned &&& (1UL <<< square)) <> 0UL then checkMask &&& lineBB.[kingSq * 64 + square]
          else checkMask
        let mutable destinations = opposing &&& BBDestinations(piece, square, occupation) &&& legalMask
        while (destinations <> 0UL) do
            moves.[index] <-  { MoveType = piece ||| TPieceType.CAPTURE; From = byte square; To = byte (LSB destinations); Promotion = TPieceType.EMPTY }
            index <- index + 1
            destinations <- ClearLSB destinations
        pieces <- ClearLSB pieces

    let pawns = PositionOps.pawns &position &&& PositionOps.sideToMove &position
    let freePawns = pawns &&& ~~~pinned
    let neCapturers = freePawns ||| (pawns &&& pinned &&& diagLinesNE.[kingSq])
    let nwCapturers = freePawns ||| (pawns &&& pinned &&& diagLinesNW.[kingSq])

    //right pawn captures (rpc)
    let mutable rpc = (neCapturers <<< 9) &&& 0x00FEFEFEFEFEFEFEUL &&& opposing &&& checkMask
    while (rpc <> 0UL) do
      let square = int (LSB rpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 9); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      rpc <- ClearLSB rpc

    //left pawn captures (lpc)
    let mutable lpc = (nwCapturers <<< 7) &&& 0x007F7F7F7F7F7F7FUL &&& opposing &&& checkMask
    while (lpc <> 0UL) do
      let square = int (LSB lpc)
      moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = TPieceType.EMPTY }
      index <- index + 1
      lpc <- ClearLSB lpc

    // Generate pawn promotions
    if (pawns &&& 0x00FF000000000000UL) <> 0UL then
      //left promo captures
      let mutable promoL = (neCapturers <<< 9) &&& 0xFE00000000000000UL &&& opposing &&& checkMask
      while (promoL <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = LSB promoL
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 9UL); To = byte square; Promotion = piece }
          index <- index + 1
        promoL <- ClearLSB promoL

      //right promo captures
      let mutable promoR = (nwCapturers <<< 7) &&& 0x7F00000000000000UL  &&& opposing &&& checkMask
      while (promoR <> 0UL) do
        for piece in TPieceType.piecePromoTraversal do
          let square = int (LSB promoR)
          moves.[index] <-  { MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE; From = byte (square - 7); To = byte square; Promotion = piece }
          index <- index + 1
        promoR <- ClearLSB promoR

      // no capture promotions (file-pinned pawns fold in like regular pushes)
      let kingFileMask = 0x0101010101010101UL <<< (kingSq &&& 7)
      let promoPushSrc = freePawns ||| (pawns &&& pinned &&& kingFileMask)
      let mutable promoNoC = (promoPushSrc <<< 8) &&& ~~~occupation &&& 0xFF00000000000000UL &&& checkMask
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
        let mutable mv = { MoveType = TPieceType.PAWN ||| TPieceType.EP ||| TPieceType.CAPTURE; From = byte square; To = (40uy + PositionOps.enPass(&position)); Promotion = TPieceType.EMPTY }
        if not (Illegal &mv &position) then
          moves.[index] <- mv
          index <- index + 1
        ep <- ClearLSB ep

let makeMove (move : TMove inref) (position : Position byref) =
    let part = 1UL <<< int move.From
    let dest = 1UL <<< int move.To
    position.Ply <- position.Ply + 1us
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
            position.Count50 <- 0uy //reset the 50 move rule counter
            // Delete the captured piece
            position.P0 <- position.P0 &&& ~~~dest
            position.P1 <- position.P1 &&& ~~~dest
            position.P2 <- position.P2 &&& ~~~dest
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
    
        position.PM <- position.PM ^^^ (part ||| dest)
        position.P1 <- position.P1 ^^^ (part ||| dest)
        position.P2 <- position.P2 ^^^ (part ||| dest)
        let canCastleShort = PositionOps.CanCastleSM &position
        let canCastleLong = PositionOps.CanCastleLM &position
        PositionOps.ResetCastleSM &position // update the castle rights
        PositionOps.ResetCastleLM &position
        position.EnPassant <- 8uy
               
        if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
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


