namespace ChessLibrary

open System
open System.Text
open QBBOperations
open PositionTypes

/// Move types and operations for chess moves.
/// Contains TPieceType, TMove, and TMoveOps module.
module MoveTypes =
    [<Flags>]
    type TPieceType =
        | EMPTY = 0uy
        | PAWN = 1uy
        | KNIGHT = 2uy
        | BISHOP = 3uy
        | ROOK = 4uy
        | QUEEN = 5uy
        | KING = 6uy
        | PIECE_MASK = 0x07uy
        | CASTLE = 0x40uy
        | PROMO = 0x20uy
        | EP = 0x10uy
        | CAPTURE = 0x08uy

    module TPieceType =
      let pieceTraversal = [| TPieceType.KING; TPieceType.QUEEN; TPieceType.ROOK; TPieceType.BISHOP; TPieceType.KNIGHT |]
      let piecePromoTraversal = [| TPieceType.QUEEN; TPieceType.ROOK; TPieceType.BISHOP; TPieceType.KNIGHT |]
      let symbolFromPieceType (pieceType: uint64) color =
        let piece = byte pieceType |> LanguagePrimitives.EnumOfValue
        match piece with
        | TPieceType.EMPTY -> '.'
        | TPieceType.PAWN -> if color = 8uy then 'p' else 'P'
        | TPieceType.KNIGHT -> if color = 8uy then 'n' else 'N'
        | TPieceType.BISHOP -> if color = 8uy then 'b' else 'B'
        | TPieceType.ROOK -> if color = 8uy then 'r' else 'R'
        | TPieceType.QUEEN -> if color = 8uy then 'q' else 'Q'
        | TPieceType.KING -> if color = 8uy then 'k' else 'K'
        | _ -> failwith "Unknown piece type"
      let Piece (sq: int, position: Position inref) =
        ((position.P2 >>> sq &&& 1UL) <<< 2) |||
        ((position.P1 >>> sq &&& 1UL) <<< 1) |||
        (position.P0 >>> sq &&& 1UL)

    [<Struct>]
    type TMove =
      { MoveType: TPieceType
        From: byte
        To: byte
        Promotion: TPieceType }

    module TMoveOps =
      let inline getPiece (move: TMove) = move.MoveType &&& TPieceType.PIECE_MASK
      let inline isCaptureMove (move: TMove) = (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
      let inline isCastlingMove (move: TMove) = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
      let inline isPromotionMove (move: TMove) = move.Promotion <> TPieceType.EMPTY
      let inline isEnPassantMove (move: TMove) = (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
      let inline isEmptyMove (move: TMove) = move.MoveType = TPieceType.EMPTY
      let inline isKingMove (move: TMove) = getPiece move = TPieceType.KING
      let inline isQueenMove (move: TMove) = getPiece move = TPieceType.QUEEN
      let inline isRookMove (move: TMove) = getPiece move = TPieceType.ROOK
      let inline isBishopMove (move: TMove) = getPiece move = TPieceType.BISHOP
      let inline isKnightMove (move: TMove) = getPiece move = TPieceType.KNIGHT
      let inline isPawnMove (move: TMove) = getPiece move = TPieceType.PAWN
      let inline isPawnCaptureMove (move: TMove) = isCaptureMove move && isPawnMove move
      let inline isKingCastleMove (move: TMove) = isCastlingMove move && isKingMove move
      let inline getPromoPiece (move: TMove) = move.Promotion &&& TPieceType.PIECE_MASK
      //get piece on square
      let inline getPieceOnSquare (pos:Position inref) (sq:byte) =
        let sq = int sq
        let piece =
          ((pos.P2 >>> sq &&& 1UL) <<< 2) |||
          ((pos.P1 >>> sq &&& 1UL) <<< 1) |||
          (pos.P0 >>> sq &&& 1UL)
        piece

      let inline removeSpecialEndSymbols (input: string) = input.TrimEnd([|'+'; '#';'!';'?'|])

      let pieceSymbolFromPieceType (piece:TPieceType) =
        match piece with
        | TPieceType.EMPTY -> ""
        | TPieceType.PAWN -> ""
        | TPieceType.KNIGHT -> "N"
        | TPieceType.BISHOP -> "B"
        | TPieceType.ROOK -> "R"
        | TPieceType.QUEEN -> "Q"
        | TPieceType.KING -> "K"
        | _ -> failwith "Unknown piece type"

      //the inverse of the above function

      let pieceTypeFromSymbol (symbol:char) =
        match symbol with
        | 'N' -> TPieceType.KNIGHT
        | 'B' -> TPieceType.BISHOP
        | 'R' -> TPieceType.ROOK
        | 'Q' -> TPieceType.QUEEN
        | 'K' -> TPieceType.KING
        | 'a' -> TPieceType.PAWN
        | 'b' -> TPieceType.PAWN
        | 'c' -> TPieceType.PAWN
        | 'd' -> TPieceType.PAWN
        | 'e' -> TPieceType.PAWN
        | 'f' -> TPieceType.PAWN
        | 'g' -> TPieceType.PAWN
        | 'h' -> TPieceType.PAWN
        |_ -> TPieceType.EMPTY

      let createMove (moveType:TPieceType) (from:byte) (to_:byte) (promotion:TPieceType) =
        {
          MoveType = moveType
          From = from
          To = to_
          Promotion = promotion
        }

      let createEmptyMove () =
        {
          MoveType = TPieceType.EMPTY
          From = 0uy
          To = 0uy
          Promotion = TPieceType.EMPTY
        }

      //Since promotion value (TPieceType) Empty and Pawn are not allowed during promotion, we keep empty values for them
      let promo = [|' '; ' '; 'n'; 'b'; 'r'; 'q'|]

      let charSet = Set.ofList ['n'; 'b'; 'r'; 'q'; 'N'; 'B'; 'R'; 'Q'; '=']

      let empty = { MoveType = TPieceType.EMPTY; From=0uy; To=0uy; Promotion=TPieceType.EMPTY }

      let moveToStr (move: TMove inref) (sideToMove: byte) =
        let result = new StringBuilder(6)
        let from = AbsSq(int move.From, int sideToMove)
        let to_ = AbsSq(int move.To, int sideToMove)
        let promoIdx = int move.Promotion
        let promoChar = if promoIdx >= 0 && promoIdx < promo.Length then promo.[promoIdx] else ' '
        result.Append(char(int('a') + from % 8)) |> ignore
        result.Append(char(int('1') + from / 8)) |> ignore
        result.Append(char(int('a') + to_ % 8)) |> ignore
        result.Append(char(int('1') + to_ / 8)) |> ignore
        result.Append(promoChar) |> ignore
        result.ToString().TrimEnd()

      let dictNameToNumber (stm: inref<byte>) = if stm = 0uy then QBBOperations.squareNameToNumberDictWhite else QBBOperations.squareNameToNumberDictBlack
      let dictNumberToName (stm: inref<byte>) = if stm = 0uy then QBBOperations.squareNumberToNameDictWhite else QBBOperations.squareNumberToNameDictBlack

      //get TMove from SAN string
      let getTMoveFromShortSan (sanShort: string) (moves : TMove array) stm checkIsLegal =
        if String.IsNullOrWhiteSpace sanShort then None else
        let sanShort = sanShort.Trim()
        if sanShort.Length = 0 then None else
        let piece = pieceTypeFromSymbol sanShort.[0]
        let isCapture = sanShort.Contains("x")
        let isCastling = sanShort.Contains("0-0") || sanShort.Contains("O-O") || sanShort.Contains("0-0-0") || sanShort.Contains("O-O-O")
        let adjustedSan = removeSpecialEndSymbols sanShort
        if not isCastling && adjustedSan.Length < 2 then
          None
        else
          let lastChar = if adjustedSan.Length > 0 then adjustedSan.[adjustedSan.Length - 1] else ' '
          let isPromotion = charSet.Contains lastChar
          let inline trySlice start len =
            if start >= 0 && adjustedSan.Length >= start + len then
              Some (adjustedSan.Substring(start, len))
            else None

          if isCastling then
            if adjustedSan = "0-0" || adjustedSan = "O-O" then
              let castlingShort = moves |> Array.tryFind(fun m -> isCastlingMove m && m.To > m.From)
              castlingShort
            elif adjustedSan = "0-0-0" || adjustedSan = "O-O-O" then
              //let filter = moves |> Array.filter(fun m -> isCastlingMove m && m.To < m.From)
              let castlingShort = moves |> Array.tryFind(fun m -> isCastlingMove m && m.To < m.From)
              castlingShort
            else
              None

          else
            let nameToSqNumber = dictNameToNumber &stm
            let tryGetDest (sq: string) =
              let key = sq.ToLowerInvariant()
              match nameToSqNumber.TryGetValue key with
              | true, v -> Some (byte v)
              | _ -> None

            let moveType, promo, dest =
              if isPromotion && isCapture then
                match trySlice 2 2 |> Option.bind tryGetDest with
                | Some d -> TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE, pieceTypeFromSymbol adjustedSan.[adjustedSan.Length - 1], d
                | None -> TPieceType.EMPTY, TPieceType.EMPTY, 0uy
              elif isPromotion then
                match trySlice 0 2 |> Option.bind tryGetDest with
                | Some d -> TPieceType.PAWN ||| TPieceType.PROMO, pieceTypeFromSymbol adjustedSan.[adjustedSan.Length - 1], d
                | None -> TPieceType.EMPTY, TPieceType.EMPTY, 0uy
              elif isCapture then
                match trySlice (adjustedSan.Length - 2) 2 |> Option.bind tryGetDest with
                | Some d -> piece ||| TPieceType.CAPTURE, TPieceType.EMPTY, d
                | None -> TPieceType.EMPTY, TPieceType.EMPTY, 0uy
              else
                match trySlice (adjustedSan.Length - 2) 2 |> Option.bind tryGetDest with
                | Some d -> piece, TPieceType.EMPTY, d
                | None -> TPieceType.EMPTY, TPieceType.EMPTY, 0uy

            if moveType = TPieceType.EMPTY then
              None
            else
              let start = moves |> Array.filter(fun m -> m.To = dest && getPiece m = piece)

              if start.Length = 1 then
                Some start.[0]
              else

                let start1 = start |> Array.filter (fun m -> m.MoveType = moveType && m.Promotion = promo && checkIsLegal m)
                if start1.Length = 1 then
                  Some start1[0]
                else
                  //disambiguate by file or rank
                  let getDisambig (san:string) =
                    match piece with
                    | TPieceType.PAWN when san.Length >= 1 -> Some san.[0]
                    | _ when san.Length >= 2 -> Some san.[1]
                    | _ -> None

                  let isUniqueFile move san =
                    match getDisambig san with
                    | Some c -> (moveToStr &move stm).[0] = c
                    | None -> false
                  let start2 = start1 |> Array.filter(fun m -> isUniqueFile m adjustedSan && checkIsLegal m )
                  if start2.Length = 1 then
                    start2.[0] |> Some
                  else //check rank
                    let isUniqueRank move san =
                      match getDisambig san with
                      | Some c -> (moveToStr &move stm).[1] = c
                      | None -> false
                    let start3 = start1 |> Array.filter(fun m -> isUniqueRank m adjustedSan && checkIsLegal m )
                    let trank = start |> Array.filter(fun m -> isUniqueRank m adjustedSan && checkIsLegal m )
                    let tfile = start |> Array.filter(fun m -> isUniqueFile m adjustedSan && checkIsLegal m )
                    if start3.Length = 1 then
                      start3.[0] |> Some
                    elif tfile.Length = 1 then
                      tfile.[0] |> Some
                    elif trank.Length = 1 then
                      trank.[0] |> Some
                    else
                      let f sq = (dictNumberToName &stm).Item (int sq)
                      let fromSq = start |> Array.tryFind (fun m -> adjustedSan.Contains(f m.From) && checkIsLegal m)
                      let toSq = start |> Array.tryFind (fun m -> adjustedSan.Contains(f m.To) && checkIsLegal m)
                      let maybe =
                        match fromSq, toSq with
                        | Some f, Some t ->
                          if f.To = t.To then
                            //printfn "Ambiguous move: %s" adjustedSan
                            Some f
                          else
                            None

                        //| Some f, None ->
                        //  //found only from
                        //  Some f
                        //| None, Some t ->
                        //  //found only to
                        //  Some t
                        |_ -> None
                      if maybe.IsSome then
                        maybe

                      else
                        // In ambiguous cases, bail out without emitting debug prints (tests may run under a closed TextWriter).
                        start |> Array.tryFind(fun m -> checkIsLegal m )

        ///The order of standard SAN string is:
        ///1. piece symbol (if not pawn) - if pawn then use the file
        ///2. capture symbol (if capture)
        ///3. destination square (always) in algebraic notation
        ///4. promotion symbol (if promotion)
      let getShortSan fileOrRank (move:TMove) stm =
        let isCapture = isCaptureMove move
        let numberToName = dictNumberToName &stm  // if stm = 0uy then QBBOperations.squareNumberToNameDictWhite else QBBOperations.squareNumberToNameDictBlack
        let shortMoveStart = numberToName.[int move.From]
        let shortMoveDest = numberToName.[int move.To]
        let piece = getPiece move
        let mutable san = ""
        if isCastlingMove move then
          if move.From < move.To then
            san <- "0-0" //"O-O"
          else
            san <- "0-0-0" //"O-O-O"
        elif piece = TPieceType.PAWN then
          if isCapture || isEnPassantMove move  then
            san <- sprintf "%cx%s" shortMoveStart[0] shortMoveDest
          else
            san <- sprintf "%s" shortMoveDest
        else
          let pieceSymbol = pieceSymbolFromPieceType piece
          if isCapture then
            san <- sprintf "%s%sx%s" pieceSymbol fileOrRank shortMoveDest
          else
            san <- sprintf "%s%s%s" pieceSymbol fileOrRank shortMoveDest

        if isPromotionMove move then
          let promoPiece = getPromoPiece move
          let promoSymbol = pieceSymbolFromPieceType promoPiece
          san <- sprintf "%s=%s" san promoSymbol
        san


      // get sanLong from Tmove
      let getUciNotation (move: TMove) side = moveToStr &move side

      let pieceOnSquare p sq = getPieceOnSquare &p sq

      let foundRookOnSquare p (moves : TMove array) =
          let piece = TPieceType.ROOK |> uint64
          moves
          |> Array.tryFind (fun m -> (pieceOnSquare p m.To) = piece)

      /// Numeric UCI-token matcher over the first `count` entries of `moves`: parses
      /// "e2e4"/"e7e8q" once into relative-frame squares and compares TMove fields
      /// directly — no per-candidate string building. Match semantics are identical to
      /// exact string comparison against moveToStr output (lowercase only, 4 or 5 chars).
      let tryFindMoveByUciNotation (moves: TMove array) (count: int) (stm: byte) (uci: string) =
        if uci.Length < 4 || uci.Length > 5 then None
        else
          let f0 = int uci.[0] - int 'a'
          let r0 = int uci.[1] - int '1'
          let f1 = int uci.[2] - int 'a'
          let r1 = int uci.[3] - int '1'
          if f0 < 0 || f0 > 7 || r0 < 0 || r0 > 7 || f1 < 0 || f1 > 7 || r1 < 0 || r1 > 7 then None
          else
            let fromSq = byte (AbsSq(r0 * 8 + f0, int stm))
            let toSq = byte (AbsSq(r1 * 8 + f1, int stm))
            let promoChar = if uci.Length = 5 then uci.[4] else ' '
            let mutable result = None
            let mutable i = 0
            while result.IsNone && i < count do
              let m = moves.[i]
              if m.From = fromSq && m.To = toSq then
                let promoIdx = int m.Promotion
                let pChar = if promoIdx >= 0 && promoIdx < promo.Length then promo.[promoIdx] else ' '
                if pChar = promoChar then result <- Some m
              i <- i + 1
            result

      /// Count-aware SAN conversion over the first `count` entries of `moves` —
      /// single pass, no intermediate array allocations. `moves` must be the legal
      /// move list of the position (disambiguation counts alternatives from it).
      let getShortSanMoveFromTmoveN (moves : TMove array) (count: int) (move: TMove) (pos : Position) =
        if isCastlingMove move then
          if move.To < move.From then
            "0-0-0"
          elif move.To > move.From then
            "0-0"
          else
            failwith "Invalid castling move"
        else
          let movedPiece = getPiece move
          let promoPiece = getPromoPiece move
          let castling = isCastlingMove move
          let numberToSq = dictNumberToName &pos.STM
          let fromName = numberToSq.[int move.From]
          // one pass: count same-destination alternatives and their file/rank collisions
          let mutable sameSquare = 0
          let mutable sameFile = 0
          let mutable sameRank = 0
          for i in 0 .. count - 1 do
            let m = moves.[i]
            if m.To = move.To && getPiece m = movedPiece && promoPiece = m.Promotion && castling = isCastlingMove m then
              sameSquare <- sameSquare + 1
              let name = numberToSq.[int m.From]
              if name.[0] = fromName.[0] then sameFile <- sameFile + 1
              if name.[1] = fromName.[1] then sameRank <- sameRank + 1

          if sameSquare > 1 then
            //file is ambiguous when another same-destination move shares the from-file
            if sameFile > 1 then
              if sameRank > 1 then
                //both file and rank are ambiguous - we need to use both
                getShortSan fromName move pos.STM
              else
                getShortSan (fromName.[1].ToString()) move pos.STM
            else //we need to use file
              getShortSan (fromName.[0].ToString()) move pos.STM
          else
            getShortSan "" move pos.STM

      let getShortSanMoveFromTmove (moves : TMove array) (move: TMove) (pos : Position) =
        getShortSanMoveFromTmoveN moves moves.Length move pos

      // (Additional functions for converting moves to SAN strings, disambiguation, etc.)
