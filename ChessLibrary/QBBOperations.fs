module ChessLibrary.QBBOperations

let [<Literal>]  firstRank = 0x0000000000000000FFUL
let [<Literal>]  lastRank = 0xff00000000000000UL

/// <summary>
/// The keys are strings representing the square names in standard algebraic notation (e.g., "a1", "b1", etc.).
/// The values are integers representing the indices of the squares (e.g., 0 for "a1", 1 for "b1", etc.).
/// </summary>
let squareNameToNumberDictWhite =
  [ "a1", 0; "b1", 1; "c1", 2; "d1", 3; "e1", 4; "f1", 5; "g1", 6; "h1", 7; "a2", 8; "b2", 9; "c2", 10; "d2", 11; 
    "e2", 12; "f2", 13; "g2", 14; "h2", 15; "a3", 16; "b3", 17; "c3", 18; "d3", 19; "e3", 20; "f3", 21; "g3", 22; 
    "h3", 23; "a4", 24; "b4", 25; "c4", 26; "d4", 27; "e4", 28; "f4", 29; "g4", 30; "h4", 31; "a5", 32; "b5", 33; 
    "c5", 34; "d5", 35; "e5", 36; "f5", 37; "g5", 38; "h5", 39; "a6", 40; "b6", 41; "c6", 42; "d6", 43; "e6", 44; 
    "f6", 45; "g6", 46; "h6", 47; "a7", 48; "b7", 49; "c7", 50; "d7", 51; "e7", 52; "f7", 53; "g7", 54; "h7", 55; 
    "a8", 56; "b8", 57; "c8", 58; "d8", 59; "e8", 60; "f8", 61; "g8", 62; "h8", 63 ] |> dict

//the same as above but start with "a8",0 for black pieces
let squareNameToNumberDictBlack =
  [ "a8", 0; "b8", 1; "c8", 2; "d8", 3; "e8", 4; "f8", 5; "g8", 6; "h8", 7; "a7", 8; "b7", 9; "c7", 10; "d7", 11; 
    "e7", 12; "f7", 13; "g7", 14; "h7", 15; "a6", 16; "b6", 17; "c6", 18; "d6", 19; "e6", 20; "f6", 21; "g6", 22; 
    "h6", 23; "a5", 24; "b5", 25; "c5", 26; "d5", 27; "e5", 28; "f5", 29; "g5", 30; "h5", 31; "a4", 32; "b4", 33; 
    "c4", 34; "d4", 35; "e4", 36; "f4", 37; "g4", 38; "h4", 39; "a3", 40; "b3", 41; "c3", 42; "d3", 43; "e3", 44; 
    "f3", 45; "g3", 46; "h3", 47; "a2", 48; "b2", 49; "c2", 50; "d2", 51; "e2", 52; "f2", 53; "g2", 54; "h2", 55; 
    "a1", 56; "b1", 57; "c1", 58; "d1", 59; "e1", 60; "f1", 61; "g1", 62; "h1", 63 ] |> dict
         

//key as square number and value as square name
let squareNumberToNameDictWhite =  
  [ 0, "a1"; 1, "b1"; 2, "c1"; 3, "d1"; 4, "e1"; 5, "f1"; 6, "g1"; 7, "h1"; 8, "a2"; 9, "b2"; 10, "c2"; 11, "d2"; 
    12, "e2"; 13, "f2"; 14, "g2"; 15, "h2"; 16, "a3"; 17, "b3"; 18, "c3"; 19, "d3"; 20, "e3"; 21, "f3"; 22, "g3"; 
    23, "h3"; 24, "a4"; 25, "b4"; 26, "c4"; 27, "d4"; 28, "e4"; 29, "f4"; 30, "g4"; 31, "h4"; 32, "a5"; 33, "b5"; 
    34, "c5"; 35, "d5"; 36, "e5"; 37, "f5"; 38, "g5"; 39, "h5"; 40, "a6"; 41, "b6"; 42, "c6"; 43, "d6"; 44, "e6"; 
    45, "f6"; 46, "g6"; 47, "h6"; 48, "a7"; 49, "b7"; 50, "c7"; 51, "d7"; 52, "e7"; 53, "f7"; 54, "g7"; 55, "h7"; 
    56, "a8"; 57, "b8"; 58, "c8"; 59, "d8"; 60, "e8"; 61, "f8"; 62, "g8"; 63, "h8" ] |> dict

//the same as above but start with a8,0 - for black pieces
let squareNumberToNameDictBlack =
  [ 0,"a8"; 1,"b8"; 2,"c8"; 3,"d8"; 4,"e8"; 5,"f8"; 6,"g8"; 7,"h8"; 8,"a7"; 9,"b7"; 10,"c7"; 11,"d7"; 
    12,"e7"; 13,"f7"; 14,"g7"; 15,"h7"; 16,"a6"; 17,"b6"; 18,"c6"; 19,"d6"; 20,"e6"; 21,"f6"; 22,"g6"; 
    23,"h6"; 24,"a5"; 25,"b5"; 26,"c5"; 27,"d5"; 28,"e5"; 29,"f5"; 30,"g5"; 31,"h5"; 32,"a4"; 33,"b4"; 
    34,"c4"; 35,"d4"; 36,"e4"; 37,"f4"; 38,"g4"; 39,"h4"; 40,"a3"; 41,"b3"; 42,"c3"; 43,"d3"; 44,"e3"; 
    45,"f3"; 46,"g3"; 47,"h3"; 48,"a2"; 49,"b2"; 50,"c2"; 51,"d2"; 52,"e2"; 53,"f2"; 54,"g2"; 55,"h2"; 
    56,"a1"; 57,"b1"; 58,"c1"; 59,"d1"; 60,"e1"; 61,"f1"; 62,"g1"; 63,"h1" ] |> dict

// Define each file mask as a constant
let [<Literal>] AFile = 0x101010101010100UL
let [<Literal>] BFile = 0x202020202020200UL
let [<Literal>] CFile = 0x404040404040400UL
let [<Literal>] DFile = 0x808080808080800UL
let [<Literal>] EFile = 0x1010101010101000UL
let [<Literal>] FFile = 0x2020202020202000UL
let [<Literal>] GFile = 0x4040404040404000UL
let [<Literal>] HFile = 0x8080808080808000UL

// Define the array of file masks
let FileMasks = [| AFile; BFile; CFile; DFile; EFile; FFile; GFile; HFile |]

//create diagonal mask for each square (withouth the first rank bit set)
let diagonalMasks =
  [|
    9241421688590303744UL
    36099303471055872UL
    141012904183808UL
    550831656960UL
    2151686144UL
    8404992UL
    32768UL
    0UL
  |]

//create anti diagonal mask for each square (withouth the first rank bit set)
let antiDiagonalMask =
  [|
    0UL
    256UL
    66048UL
    16909312UL
    4328785920UL
    1108169199616UL
    283691315109888UL
    72624976668147712UL
  |]

let LongRookKingMask =
  [
    [0UL; 15UL; 15UL; 15UL; 31UL; 63UL; 127UL; 0UL]
    [0UL; 0UL; 14UL; 14UL; 30UL; 62UL; 126UL; 0UL]
    [0UL; 0UL; 0UL; 12UL; 28UL; 60UL; 124UL; 0UL]
    [0UL; 0UL; 0UL; 0UL; 28UL; 60UL; 124UL; 0UL]
    [0UL; 0UL; 0UL; 0UL; 0UL; 60UL; 124UL; 0UL]
    [0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 124UL; 0UL]
    [0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL]
    [0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL]
  ]

let ShortRookKingMask =
  [|
    [|0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL|]
    [|0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL|]
    [|0UL; 126UL; 0UL; 0UL; 0UL; 0UL; 0UL; 0UL|]
    [|0UL; 126UL; 124UL; 0UL; 0UL; 0UL; 0UL; 0UL|]
    [|0UL; 126UL; 124UL; 120UL; 0UL; 0UL; 0UL; 0UL|]
    [|0UL; 126UL; 124UL; 120UL; 112UL; 0UL; 0UL; 0UL|]
    [|0UL; 126UL; 124UL; 120UL; 112UL; 96UL; 0UL; 0UL|]
    [|0UL; 254UL; 252UL; 248UL; 240UL; 224UL; 224UL; 0UL|]
  |]

    //array of bitboards that contains all the knight destination for every square
let KnightDest = [|
    0x0000000000020400UL;0x0000000000050800UL;0x00000000000a1100UL;0x0000000000142200UL;
    0x0000000000284400UL;0x0000000000508800UL;0x0000000000a01000UL;0x0000000000402000UL;
    0x0000000002040004UL;0x0000000005080008UL;0x000000000a110011UL;0x0000000014220022UL;
    0x0000000028440044UL;0x0000000050880088UL;0x00000000a0100010UL;0x0000000040200020UL;
    0x0000000204000402UL;0x0000000508000805UL;0x0000000a1100110aUL;0x0000001422002214UL;
    0x0000002844004428UL;0x0000005088008850UL;0x000000a0100010a0UL;0x0000004020002040UL;
    0x0000020400040200UL;0x0000050800080500UL;0x00000a1100110a00UL;0x0000142200221400UL;
    0x0000284400442800UL;0x0000508800885000UL;0x0000a0100010a000UL;0x0000402000204000UL;
    0x0002040004020000UL;0x0005080008050000UL;0x000a1100110a0000UL;0x0014220022140000UL;
    0x0028440044280000UL;0x0050880088500000UL;0x00a0100010a00000UL;0x0040200020400000UL;
    0x0204000402000000UL;0x0508000805000000UL;0x0a1100110a000000UL;0x1422002214000000UL;
    0x2844004428000000UL;0x5088008850000000UL;0xa0100010a0000000UL;0x4020002040000000UL;
    0x0400040200000000UL;0x0800080500000000UL;0x1100110a00000000UL;0x2200221400000000UL;
    0x4400442800000000UL;0x8800885000000000UL;0x100010a000000000UL;0x2000204000000000UL;
    0x0004020000000000UL;0x0008050000000000UL;0x00110a0000000000UL;0x0022140000000000UL;
    0x0044280000000000UL;0x0088500000000000UL;0x0010a00000000000UL;0x0020400000000000UL 
    |]

let KingDest = [|
    0x0000000000000302UL;0x0000000000000705UL;0x0000000000000e0aUL;0x0000000000001c14UL;
    0x0000000000003828UL;0x0000000000007050UL;0x000000000000e0a0UL;0x000000000000c040UL;
    0x0000000000030203UL;0x0000000000070507UL;0x00000000000e0a0eUL;0x00000000001c141cUL;
    0x0000000000382838UL;0x0000000000705070UL;0x0000000000e0a0e0UL;0x0000000000c040c0UL;
    0x0000000003020300UL;0x0000000007050700UL;0x000000000e0a0e00UL;0x000000001c141c00UL;
    0x0000000038283800UL;0x0000000070507000UL;0x00000000e0a0e000UL;0x00000000c040c000UL;
    0x0000000302030000UL;0x0000000705070000UL;0x0000000e0a0e0000UL;0x0000001c141c0000UL;
    0x0000003828380000UL;0x0000007050700000UL;0x000000e0a0e00000UL;0x000000c040c00000UL;
    0x0000030203000000UL;0x0000070507000000UL;0x00000e0a0e000000UL;0x00001c141c000000UL;
    0x0000382838000000UL;0x0000705070000000UL;0x0000e0a0e0000000UL;0x0000c040c0000000UL;
    0x0003020300000000UL;0x0007050700000000UL;0x000e0a0e00000000UL;0x001c141c00000000UL;
    0x0038283800000000UL;0x0070507000000000UL;0x00e0a0e000000000UL;0x00c040c000000000UL;
    0x0302030000000000UL;0x0705070000000000UL;0x0e0a0e0000000000UL;0x1c141c0000000000UL;
    0x3828380000000000UL;0x7050700000000000UL;0xe0a0e00000000000UL;0xc040c00000000000UL;
    0x0203000000000000UL;0x0507000000000000UL;0x0a0e000000000000UL;0x141c000000000000UL;
    0x2838000000000000UL;0x5070000000000000UL;0xa0e0000000000000UL;0x40c0000000000000UL
    |]

        //masks for finding the pawns that can capture with an enpassant (in move generation) */
let EnPassant = [|
    0x0000000200000000UL;0x0000000500000000UL;0x0000000A00000000UL;0x0000001400000000UL;
    0x0000002800000000UL;0x0000005000000000UL;0x000000A000000000UL;0x0000004000000000UL
    |]

// masks for finding the pawns that can capture with an enpassant (in make move) */
let EnPassantM = [|
    0x0000000002000000UL;0x0000000005000000UL;0x000000000A000000UL;0x0000000014000000UL;
    0x0000000028000000UL;0x0000000050000000UL;0x00000000A0000000UL;0x0000000040000000UL
    |]
    
let inline RevBB (bb:uint64) = System.Buffers.Binary.BinaryPrimitives.ReverseEndianness(bb)

let inline MSB(bb:uint64) = 63UL ^^^ (uint64)(System.Numerics.BitOperations.LeadingZeroCount(bb))  //63 ^ Lzcnt.X64.LeadingZeroCount(bb);

let inline LSB(bb:uint64) = (uint64)(System.Numerics.BitOperations.TrailingZeroCount(bb))

let inline ExtractLSB(bb:uint64) = bb &&& (uint64 (-(int64 bb))) //Bmi1.X64.ExtractLowestSetBit(bb)

let inline ClearLSB(bb:uint64) = bb &&& (bb - 1UL) //Bmi1.X64.ResetLowestSetBit(bb)

let inline Pop(bb:uint64) = (uint64)(System.Numerics.BitOperations.PopCount(bb))

    
let inline OppSq(sp:int) = sp ^^^ 56
let inline AbsSq(sq:int, sideToMove:int) = if sideToMove = 0 then sq else OppSq(sq)

// Define functions for each file
let getAFileKingAttack occupation = ExtractLSB(FileMasks.[0] &&& occupation)
let getBFileKingAttack occupation = ExtractLSB(FileMasks.[1] &&& occupation)
let getCFileKingAttack occupation = ExtractLSB(FileMasks.[2] &&& occupation)
let getDFileKingAttack occupation = ExtractLSB(FileMasks.[3] &&& occupation)
let getEFileKingAttack occupation = ExtractLSB(FileMasks.[4] &&& occupation)
let getFFileKingAttack occupation = ExtractLSB(FileMasks.[5] &&& occupation)
let getGFileKingAttack occupation = ExtractLSB(FileMasks.[6] &&& occupation)
let getHFileKingAttack occupation = ExtractLSB(FileMasks.[7] &&& occupation)

let createKnightMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <- mask ||| KnightDest.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| KnightDest.[currentSq]
  mask

//pre compute a 2D array with Knight masks where fromSq is one index and toSq the second index
let KnightMasks = Array2D.init 7 7 (fun fromSq toSq -> createKnightMask fromSq toSq)

//create a function that returns the Knight mask for a given fromSq and toSq
let getKnightMask fromSq toSq = KnightMasks.[fromSq, toSq]

//bishop attack from a1 square
let BishopAttacks = 
  [|0x8040201008040200UL;0x0080402010080500UL;0x0000804020110A00UL;0x0000008041221400UL;
    0x0000000182442800UL;0x0000010204885000UL;0x000102040810A000UL;0x102040810204000UL  |]

let extractLSBsFromFiles bitboard = 
  FileMasks
  |> Array.map (fun fileMask -> ExtractLSB (bitboard &&& fileMask)) 
  |> Array.fold (fun acc lsb -> acc ||| lsb) 0UL

let extractLSBsFromDiagonals bitboard = 
  diagonalMasks
  |> Array.map (fun diagonal -> ExtractLSB (bitboard &&& diagonal)) 
  |> Array.fold (fun acc lsb -> acc ||| lsb) 0UL

let extractLSBsFromAntiDiagonals bitboard = 
  antiDiagonalMask
  |> Array.map (fun diagonal -> ExtractLSB (bitboard &&& diagonal)) 
  |> Array.fold (fun acc lsb -> acc ||| lsb) 0UL
 
//create bishop masks for each square
let createBishopMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <- mask ||| BishopAttacks.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| BishopAttacks.[currentSq]
  mask

let createDiagonalBishopMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <- mask ||| diagonalMasks.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| diagonalMasks.[currentSq]
  mask

let createAntiDiagonalBishopMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <- mask ||| antiDiagonalMask.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| antiDiagonalMask.[currentSq]  
  mask

//pre compute a 2D array with bishop masks where fromSq is one index and toSq the second index
let BishopMasks = Array2D.init 8 8 (fun fromSq toSq -> createBishopMask fromSq toSq) 

let BishopDiagonalMasks = Array2D.init 8 8 (fun fromSq toSq -> createDiagonalBishopMask fromSq toSq) 

let BishopAntiDiagonalMasks = Array2D.init 8 8 (fun fromSq toSq -> createAntiDiagonalBishopMask fromSq toSq) 

//create a function that returns the Bishop mask for a given fromSq and toSq
let getBishopMask fromSq toSq = BishopMasks.[fromSq, toSq]

let getBishopMaskWithOccupancy fromSq toSq occupancy = 
  ((BishopDiagonalMasks.[fromSq, toSq] &&& occupancy) |> extractLSBsFromDiagonals)
  |||
  ((BishopAntiDiagonalMasks.[fromSq, toSq] &&& occupancy) |> extractLSBsFromAntiDiagonals)

let setBitsInRange (fromSquare: int) (toSquare: int) : uint64 =
    let lowerSquare = min fromSquare toSquare
    let upperSquare = max fromSquare toSquare
    let mask = Seq.fold (fun acc bit -> acc ||| (1UL <<< bit)) 0UL [lowerSquare..upperSquare]
    mask

//create rook masks for each square
let createRookMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    mask <-  mask ||| FileMasks.[currentSq]
    currentSq <- currentSq + dir
  mask <- mask ||| FileMasks.[currentSq]
  let dir = if startSq > finalSq then 1 else -1
  currentSq <- startSq + dir
  if dir = 1 then
    mask <- mask ||| setBitsInRange currentSq 7
  else
    mask <- mask ||| setBitsInRange 0 currentSq
  mask

//pre compute a 2D array with rook masks where fromSq is one index and toSq the second index
let RookMasks = Array2D.init 8 8 (fun fromSq toSq -> createRookMask fromSq toSq)

let getRookMask fromSq toSq = RookMasks.[fromSq, toSq]

let getLSBRowOneMask fromSq toSq occupancy = 
  if fromSq > toSq then //long castling
    (ExtractLSB ((setBitsInRange (fromSq + 1) 7) &&& occupancy))
     ||| (1UL <<< (MSB ((setBitsInRange (toSq - 1) 0) &&& (occupancy ||| 1UL)) |> int))
  else  //this needs to be checked
    1UL <<< (MSB ((setBitsInRange (fromSq - 1) 0) &&& (occupancy ||| 1UL)) |> int)

//create a function that returns the Rook mask for a given fromSq and toSq
let getRookMaskWithOccupancy fromSq toSq occupancy = 
  RookMasks.[fromSq, toSq] &&& occupancy |> extractLSBsFromFiles
  ||| getLSBRowOneMask fromSq toSq occupancy  

//create a function that returns the Rook and the bishop mask for a given fromSq and toSq
let getRookBishopMask fromSq toSq = getRookMask fromSq toSq ||| getBishopMask fromSq toSq

let getRookBishopMaskWithOccupancy fromSq toSq occupancy = getRookMaskWithOccupancy fromSq toSq occupancy ||| getBishopMaskWithOccupancy fromSq toSq occupancy

//create pawn masks for the second and seventh rank
let createPawnMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    if currentSq > 0 then
      mask <- mask ||| (1UL <<< currentSq + 7)
    if currentSq < 7 then
      mask <- mask ||| (1UL <<< currentSq + 9)
    currentSq <- currentSq + dir
  mask <- mask ||| (1UL <<< currentSq + 7)
  mask <- mask ||| (1UL <<< currentSq + 9)
  mask

//pre compute a 2D array with pawn masks where fromSq is one index and toSq the second index
let PawnMasks = Array2D.init 8 8 (fun fromSq toSq -> createPawnMask fromSq toSq)

//create a function that returns the Pawn mask for a given fromSq and toSq
let getPawnMask fromSq toSq = PawnMasks.[fromSq, toSq]

//create king masks for the second and seventh rank
let createKingMask startSq finalSq =
  let mutable mask = 0UL
  let mutable currentSq = startSq
  let dir = if startSq > finalSq then -1 else 1
  while currentSq <> finalSq do
    if currentSq < 5 && dir = 1 then
      mask <- mask ||| (1UL <<< currentSq + 10)
    if currentSq > 1 && dir = -1 then
      mask <- mask ||| (1UL <<< currentSq + 6)
    currentSq <- currentSq + dir  
  mask

//pre compute a 2D array with king masks where fromSq is one index and toSq the second index
let KingMasks = Array2D.init 7 7 (fun fromSq toSq -> createKingMask fromSq toSq)

//create a function that returns the King mask for a given fromSq and toSq
let getKingMask fromSq toSq = KingMasks.[fromSq, toSq]

/// Efficiently represents initial rook placement information used in Chess960.
/// Each of the 4 fields supports values in the range [0..15].
/// Initialized such that values returned by default object will return as 15.
[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Pack = 1)>]
type RookPlacementInfo =
    val mutable private rookInitPlacementBits: uint16

    new() = { rookInitPlacementBits = 0xFFFFus }      

    static member private EncodeValue(value: byte) : byte =
        byte (((int value + 1) &&& 0x0F) % 16)

    static member private DecodeValue(value: byte) : byte =
        byte (((int value - 1) &&& 0x0F) % 16)

    member this.WhiteKRInitPlacement
        with get() : byte =
            RookPlacementInfo.DecodeValue(byte ((this.rookInitPlacementBits >>> 12) &&& 0x0Fus))
        and set(value: byte) =
            this.rookInitPlacementBits <- (this.rookInitPlacementBits &&& ~~~(0x0Fus <<< 12)) ||| (uint16 (RookPlacementInfo.EncodeValue(value)) <<< 12)

    member this.WhiteQRInitPlacement
        with get() : byte =
            RookPlacementInfo.DecodeValue(byte ((this.rookInitPlacementBits >>> 8) &&& 0x0Fus))
        and set(value: byte) =
            this.rookInitPlacementBits <- (this.rookInitPlacementBits &&& ~~~(0x0Fus <<< 8)) ||| (uint16 (RookPlacementInfo.EncodeValue(value)) <<< 8)

    member this.BlackKRInitPlacement
        with get() : byte =
            RookPlacementInfo.DecodeValue(byte ((this.rookInitPlacementBits >>> 4) &&& 0x0Fus))
        and set(value: byte) =
            this.rookInitPlacementBits <- (this.rookInitPlacementBits &&& ~~~(0x0Fus <<< 4)) ||| (uint16 (RookPlacementInfo.EncodeValue(value)) <<< 4)

    member this.BlackQRInitPlacement
        with get() : byte =
            RookPlacementInfo.DecodeValue(byte (this.rookInitPlacementBits &&& 0x0Fus))
        and set(value: byte) =
            this.rookInitPlacementBits <- (this.rookInitPlacementBits &&& ~~~0x0Fus) ||| uint16 (RookPlacementInfo.EncodeValue(value))
