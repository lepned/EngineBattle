module ChessLibrary.ChessUtilities

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Security.Cryptography

open PGNTypes
open PositionTypes

module FEN =

  let parseFENandMoves (fenMoves: string) =
      if String.IsNullOrWhiteSpace(fenMoves) then
          ("", [||])
      else
          let parts = fenMoves.Split([| "moves" |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
          let fen = if parts.Length > 0 then parts.[0].Trim() else ""
          let moves =
              if parts.Length > 1 then
                  parts.[1].Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
              else
                  [||]
          (fen, moves)

  let extractFEN (inputString: string) =
      let prefix = "position fen "
      let idx = inputString.IndexOf(prefix, StringComparison.Ordinal)
      if idx = -1 then
          None
      else
          let startIndex = idx + prefix.Length
          if startIndex < inputString.Length then
              Some(inputString.Substring(startIndex))
          else
              None


module ZobrishHash =

    /// ZobristPiece[pieceType][square], pieceType in [0..11], square in [0..63]
  let ZobristPiece = Array2D.zeroCreate<uint64> 12 64

  /// ZobristCastling[16] if you track 4 bits of castling rights in a single nibble
  let ZobristCastling = Array.zeroCreate<uint64> 16

  /// ZobristEnPassant[8]
  let ZobristEnPassant = Array.zeroCreate<uint64> 8

  /// ZobristSide: 0 => White to move, 1 => Black to move
  let ZobristSide = Array.zeroCreate<uint64> 2

  let initializeZobristTables () =
    let rnd = System.Random(153)
    let getRand64 () =
      let bytes = Array.zeroCreate<byte> 8
      rnd.NextBytes(bytes)
      BitConverter.ToUInt64(bytes, 0)
    for i in 0..11 do
        for j in 0..63 do
            ZobristPiece.[i, j] <- getRand64 ()
    for i in 0..15 do
        ZobristCastling.[i] <- getRand64 ()
    for i in 0..7 do
        ZobristEnPassant.[i] <- getRand64 ()
    for i in 0..1 do
        ZobristSide.[i] <- getRand64 ()

  // Populate the tables exactly once, in the module's static initializer: the CLR runs
  // it (under its own lock) before any thread can read a table, so no Board needs to
  // call initializeZobristTables and no hash can ever be computed against empty or
  // half-written tables.
  do initializeZobristTables ()

  /// Which piece type (0..5) do P2,P1,P0 represent, or -1 if empty?
  let getPieceCode (pos: Position) (s: int) =
    let occ = (1UL <<< s)
    // If not occupied => empty
    if (PositionOps.occupation &pos &&& occ) = 0UL then
        -1
    else
        // figure out the 3-bit code
        let p2 = if (pos.P2 &&& occ) <> 0UL then 1 else 0
        let p1 = if (pos.P1 &&& occ) <> 0UL then 1 else 0
        let p0 = if (pos.P0 &&& occ) <> 0UL then 1 else 0
        match (p2 <<< 2) ||| (p1 <<< 1) ||| p0 with
        | 0 -> -1  // empty
        | 1 ->  0  // pawn
        | 2 ->  1  // knight
        | 3 ->  2  // bishop
        | 4 ->  3  // rook
        | 5 ->  4  // queen
        | 6 ->  5  // king
        | _ -> -1  // shouldn't happen with your design

  let isSquareSideToMove (pos: Position) (s: int) =
    ((pos.PM &&& (1UL <<< s)) <> 0UL)

  // sideToMove for White or Black
  // pos.STM = 0uy => White, 8uy => Black
  let isWhiteSTM (pos: Position) = (pos.STM = 0uy)

  // Convert (pieceCode, color) => index in [0..11]
  let getZobristIndex (pieceCode: int) (isWhite: bool) =
    match pieceCode with
    | 0 -> if isWhite then 0 else 6   // Pawn
    | 1 -> if isWhite then 1 else 7   // Knight
    | 2 -> if isWhite then 2 else 8   // Bishop
    | 3 -> if isWhite then 3 else 9   // Rook
    | 4 -> if isWhite then 4 else 10  // Queen
    | 5 -> if isWhite then 5 else 11  // King
    | _ -> -1

  // We want a nibble: [LM, SM, LO, SO], each 1 bit => 1 in that position
  let getCastleIndex (pos: Position) =
    let mutable castleIndex = 0
    if PositionOps.CanCastleLM &pos then castleIndex <- castleIndex ||| 0x1
    if PositionOps.CanCastleSM &pos then castleIndex <- castleIndex ||| 0x2
    if PositionOps.CanCastleLO &pos then castleIndex <- castleIndex ||| 0x4
    if PositionOps.CanCastleSO &pos then castleIndex <- castleIndex ||| 0x8
    castleIndex

  let computeBoardHash (pos: Position) =
    let mutable key = 0UL
    for s in 0..63 do
        let pc = getPieceCode pos s
        if pc >= 0 then
            let squareIsSideToMove = isSquareSideToMove pos s
            // is it White or Black occupant?
            // If 'pos.STM=0uy' (White) and 'squareIsSideToMove=true', that occupant is White
            // If 'pos.STM=8uy' (Black) and 'squareIsSideToMove=true', that occupant is Black
            // otherwise it's the opposite color
            let occupantIsWhite =
                if squareIsSideToMove then
                    isWhiteSTM pos
                else
                    not (isWhiteSTM pos)

            let zobIndex = getZobristIndex pc occupantIsWhite
            if zobIndex >= 0 then
                key <- key ^^^ ZobristPiece.[zobIndex, s]
    key

  /// Compute a fresh Zobrist hash from the entire Position
  let computeZobrist (pos: Position) =
    //let boardStr = positionToString("zobrist", &pos)
    let mutable key = 0UL

    // (1) Board occupancy
    key <- key ^^^ (computeBoardHash pos)

    // (2) Side to move
    if not (isWhiteSTM pos) then
        key <- key ^^^ ZobristSide.[1]
    else
        key <- key ^^^ ZobristSide.[0]

    // (3) Castling flags
    let ci = getCastleIndex pos
    key <- key ^^^ ZobristCastling.[ci]

    // (4) En passant
    let epFile = int (PositionOps.enPass &pos)
    if epFile < 8 then
        key <- key ^^^ ZobristEnPassant.[epFile]
    key


module Hash =

  let computeOpeningHash (input: string) =
    use sha256 = SHA256.Create()
    let bytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = sha256.ComputeHash(bytes)
    BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

  let private openingHashText (game: PgnGame) =
      let sb = new StringBuilder()
      let fen =
        if String.IsNullOrWhiteSpace game.GameMetaData.Fen then game.Fen else game.GameMetaData.Fen
      if String.IsNullOrEmpty fen |> not then
        sb.AppendLine(sprintf "[Fen \"%s\"]" fen ) |> ignore
      let sanMoves =
        let moves = game.Mainline |> Seq.takeWhile (fun m -> m.Comment.Contains "book") |> Seq.toList
        if moves.Length = 0 then
          // take while comment is empty
          game.Mainline |> Seq.takeWhile (fun m -> m.Comment = "") |> Seq.toList
        else
          moves
      for m in sanMoves do
          if m.Color = "w" then
            sb.Append(sprintf "%d.%s " m.MoveNumber m.San) |> ignore
          elif m.Color = "b" then
            sb.Append(sprintf "%s " m.San) |> ignore
      sb.AppendLine() |> ignore
      sb.ToString()

  let computeOpeningHashFromGame (game: PgnGame) =
      let openingText = openingHashText game
      if String.IsNullOrWhiteSpace openingText then
        computeOpeningHash (game.GameNumber.ToString())
      else
        computeOpeningHash openingText

  let hashBoard board =
      let zobrist = ZobrishHash.computeZobrist board
      zobrist

  let deviationHash board =
      ZobrishHash.computeZobrist board + (uint64 (hash board.Ply))

  let getOpeningInfo (game:PgnGame) =
    let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening" )
    let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation" )
    let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO" )
    match opening, variation, eco with
    |Some op, Some v, Some eco -> sprintf "Opening: %s - %s, ECO: %s" op.Value v.Value eco.Value
    |Some h,Some v, None -> sprintf "Opening: %s - %s" h.Value v.Value
    |Some h,_ , Some eco -> sprintf "Opening: %s, ECO: %s" h.Value eco.Value
    |None, None, Some eco -> sprintf "ECO: %s" eco.Value
    |Some op, None, None -> sprintf "%s" op.Value
    |_ ->
      if opening.IsSome then
        sprintf "Nr %i: %s" game.GameNumber opening.Value.Value
      elif String.IsNullOrEmpty game.Fen then
        sprintf "Nr %i: No opening name" game.GameNumber
      else
        sprintf "Nr %i: %s" game.GameNumber game.Fen

  let writeOpeningHashToPgnGame (game: PgnGame) =
      game.GameMetaData.OpeningHash <- computeOpeningHashFromGame game


module Random =
  let rnd = Random()

  let ShuffleSpan(values: Span<'a>) =
      let n = values.Length

      for i in 0 .. n - 2 do
          let j = rnd.Next(i, n)

          if j <> i then
              let temp = values.[i]
              values.[i] <- values.[j]
              values.[j] <- temp

  let Shuffle(values: 'a array) =
      if values = null then
          ArgumentNullException("values") |> raise

      ShuffleSpan(values.AsSpan())


module Chess960 =

  let generateAllChess960Positions () =
    let positions =
      [ for whiteBishop in 0 .. 2 .. 7 do
          for blackBishop in 1 .. 2 .. 7 do
            let remainingSquares = [ for i in 0 .. 7 do if i <> whiteBishop && i <> blackBishop then yield i ]
            for knights in Seq.choose (fun (a, b) -> if a < b then Some (a, b) else None) (Seq.allPairs remainingSquares remainingSquares) do
              let squaresForRooksKing = List.filter (fun i -> not (List.contains i [fst knights; snd knights])) remainingSquares
              for rook1 in squaresForRooksKing do
                for rook2 in squaresForRooksKing do
                  if rook1 < rook2 then
                    for king in squaresForRooksKing do
                      if rook1 < king && king < rook2 then
                        let mutable position = Array.create 8 ""
                        position.[whiteBishop] <- "B"
                        position.[blackBishop] <- "B"
                        position.[fst knights] <- "N"
                        position.[snd knights] <- "N"
                        position.[rook1] <- "R"
                        position.[rook2] <- "R"
                        position.[king] <- "K"
                        let queenPosition = position |> Array.findIndex (fun x -> x = "")
                        position.[queenPosition] <- "Q"
                        yield String.concat "" position ]
    positions

  // Function to validate the position
  let validatePosition (candidate: string) =
      if candidate.Length <> 8 then
          failwith "Invalid length"
      let validPieces = dict [('R', 2); ('N', 2); ('B', 2); ('Q', 1); ('K', 1)]
      let allChars = candidate.ToCharArray()
      let setOfCandidateChars = Set allChars
      let validChars = Set validPieces.Keys
      if validChars <> setOfCandidateChars then
          failwith "Contains invalid pieces"
      allChars
      |> Array.mapi (fun i e -> i, e = 'B')
      |> Array.filter (fun (_,e) -> e)
      |> (fun bishops -> if (fst bishops.[0]) % 2 = (fst bishops.[1]) % 2 then failwith "Both bishops on same color")

  // Function to calculate Chess960 PID
  let calcPositionId (startPos: string) =
      validatePosition startPos
      let subsetStep1 = startPos.ToCharArray() |> Array.filter (fun c -> not (c = 'Q' || c = 'B'))
      let knightPositions = subsetStep1 |> Array.mapi (fun i c -> if c = 'N' then Some i else None) |> Array.choose id
      let knightsTable = dict [
        (0, 1), 0
        (0, 2), 1
        (0, 3), 2
        (0, 4), 3
        (1, 2), 4
        (1, 3), 5
        (1, 4), 6
        (2, 3), 7
        (2, 4), 8
        (3, 4), 9 ]

      let N = knightsTable.[(knightPositions.[0], knightPositions.[1])]
      let subsetStep2 = startPos.ToCharArray() |> Array.filter (fun c -> c <> 'B')
      let Q = Array.findIndex (fun c -> c = 'Q') subsetStep2
      let darkSquares = startPos.ToCharArray() |> Array.mapi (fun i c -> if i % 2 = 0 then Some c else None) |> Array.choose id
      let lightSquares = startPos.ToCharArray() |> Array.mapi (fun i c -> if i % 2 <> 0 then Some c else None) |> Array.choose id
      let D = Array.findIndex (fun c -> c = 'B') darkSquares
      let L = Array.findIndex (fun c -> c = 'B') lightSquares
      4 * (4 * (6 * N + Q) + D) + L


  let chess960ToFen (startingPosition: string) =
    // Mirroring the starting position for black (lowercase)
    let whitePosition = startingPosition.ToLower()
    // Assuming both kingside and queenside castling are available initially
    let castlingAvailability = "KQkq"
    // Constructing the FEN string
    sprintf "%s/pppppppp/8/8/8/8/PPPPPPPP/%s w %s - 0 1" whitePosition startingPosition castlingAvailability

  let chess960DoubleToFen (white: string) (black: string) =
    // Mirroring the starting position for black (lowercase)
    let whitePosition = white.ToLower()
    // Assuming both kingside and queenside castling are available initially
    let castlingAvailability = "KQkq"
    // Constructing the FEN string
    sprintf "%s/pppppppp/8/8/8/8/PPPPPPPP/%s w %s - 0 1" whitePosition black castlingAvailability

  let getAllChess960PositionsWithPid () =
    generateAllChess960Positions()
    |> List.map (fun pos -> pos, calcPositionId pos)
    |> List.sortBy (fun (_, id) -> id)
    |> List.distinct
    |> List.map fst

  let makeDictionaryOfChess960Positions () =
    let positions = getAllChess960PositionsWithPid ()
    let dict = Dictionary<int, string>()
    let mutable pid = 0
    for pos in positions do
      let fen = chess960ToFen pos
      pid <- pid + 1
      dict.Add(pid, fen)
    dict

  let makeRandomChess960DoublePositions n =
    let positions = getAllChess960PositionsWithPid () |> List.toArray
    let rnd = Random.Shared
    let next () = rnd.Next(0, positions.Length)
    [|
      for _ = 0 to n do
        let wid, bid = next(), next()
        let white = positions[wid]
        let black = positions[bid]
        let fen = chess960DoubleToFen white black
        (wid, bid, fen)
    |] |> Array.distinctBy (fun (_, _, fen) -> fen)

  let drawChess960Positions n =
    let allPos = [| for KeyValue(pid, pos) in makeDictionaryOfChess960Positions() -> (pid, pos) |]
    Random.Shuffle(allPos)
    let n = min n allPos.Length
    allPos |> Seq.truncate n |> Seq.toList

  let writeChess960PositionsToFile path n =
    let positions = drawChess960Positions n
    let fileName = sprintf "%s/Chess960_%d.epd" path n
    use file = new StreamWriter(fileName)
    for (pid, pos) in positions do
      file.WriteLine(sprintf "%s ; %s;" pos (sprintf "id \"Chess960 - Position nr: %d\"" pid))
    printfn "File %s written to path" fileName

  let writeChess960DoublePositionsToFile path n =
    let positions = makeRandomChess960DoublePositions n
    let fileName = sprintf "%s/Chess960Double_%d.epd" path n
    use file = new StreamWriter(fileName)
    for (wid, bid, pos) in positions do
      file.WriteLine(sprintf "%s ; %s;" pos (sprintf "id \"DFRC - Position nr: %d vs %d\"" wid bid))
    printfn "File %s written to path" fileName

  /// Checks if a FEN represents a Chess960 starting position and returns the position ID (0-959)
  let tryGetPositionIdFromFen (fen: string) : int option =
    if String.IsNullOrWhiteSpace(fen) then None
    else
      let parts = fen.Split(' ')
      if parts.Length < 1 then None
      else
        let boardPart = parts.[0]
        let ranks = boardPart.Split('/')
        if ranks.Length <> 8 then None
        else
          // Check that it's a starting position:
          // - Rank 8 (index 0): black pieces (lowercase)
          // - Rank 7 (index 1): 8 black pawns "pppppppp"
          // - Ranks 6-3 (indices 2-5): empty "8"
          // - Rank 2 (index 6): 8 white pawns "PPPPPPPP"
          // - Rank 1 (index 7): white pieces (uppercase)
          let isValidStarting =
            ranks.[1] = "pppppppp" &&
            ranks.[2] = "8" &&
            ranks.[3] = "8" &&
            ranks.[4] = "8" &&
            ranks.[5] = "8" &&
            ranks.[6] = "PPPPPPPP"
          if not isValidStarting then None
          else
            // Extract white back rank (rank 1, index 7 in array)
            let whiteBackRank = ranks.[7].ToUpper()
            // Verify it's 8 characters (all pieces, no digits)
            if whiteBackRank.Length <> 8 then None
            elif whiteBackRank |> Seq.exists System.Char.IsDigit then None
            else
              try
                let posId = calcPositionId whiteBackRank
                Some posId
              with _ -> None

  /// Gets the FEN for a Chess960 position ID (0-959)
  let tryGetFenFromPositionId (positionId: int) : string option =
    if positionId < 0 || positionId > 959 then None
    else
      // Generate all positions sorted by their calculated position ID
      let positions =
        generateAllChess960Positions()
        |> List.map (fun pos -> calcPositionId pos, pos)
        |> List.sortBy fst
        |> List.distinctBy fst
      // Find the position with matching ID
      positions
      |> List.tryFind (fun (id, _) -> id = positionId)
      |> Option.map (fun (_, pos) -> chess960ToFen pos)
