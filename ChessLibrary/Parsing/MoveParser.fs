module ChessLibrary.MoveParser

open System
open MoveTypes
open EPDTypes

module ConversionHelper =

  /// List of piece characters.
  let pieceChars = ['K';'Q';'R';'B';'N']

  /// Gets the piece symbol from a move.
  /// <param name="move">The move.</param>
  /// <returns>The piece symbol.</returns>
  let pieceSymbol (move:TMove inref) =
    match move.MoveType &&& TPieceType.PIECE_MASK with
      | TPieceType.EMPTY -> ""
      | TPieceType.PAWN -> ""
      | TPieceType.KNIGHT -> "N"
      | TPieceType.BISHOP -> "B"
      | TPieceType.ROOK -> "R"
      | TPieceType.QUEEN -> "Q"
      | TPieceType.KING -> "K"
      | _ -> failwith "Unknown piece type"

  /// Gets the promotion piece symbol.
  /// <param name="pieceType">The piece type.</param>
  /// <returns>The promotion piece symbol.</returns>
  let promoPiece (pieceType:TPieceType) =
    match pieceType with
      | TPieceType.KNIGHT -> "n"
      | TPieceType.BISHOP -> "b"
      | TPieceType.ROOK -> "r"
      | TPieceType.QUEEN -> "q"
      | _ -> failwith "Unknown piece type"

module ConvertTo =
  open ConversionHelper

  /// Converts a long SAN move to a standard SAN move.
  /// <param name="longSan">The long SAN move.</param>
  /// <param name="move">The move.</param>
  /// <param name="moves">The array of possible moves.</param>
  /// <param name="side">The side to move.</param>
  /// <returns>The standard SAN move.</returns>
  let standardSAN(longSan: string, move: TMove, moves: TMove array, side) =
      // Fail loudly on malformed input: returning a placeholder string here would flow
      // onward as if it were a real SAN move (move lists, PVs, written PGNs).
      if (longSan.Length < 4 || longSan.Length > 5) then
         failwithf "standardSAN: malformed long SAN '%s'" longSan

      elif (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
        if move.To = 6uy then
          //printfn "From: %d To: %d \n%A" move.From move.To move
          "0-0"
        else
          //printfn "From: %d To: %d \n%A" move.From move.To move
          "0-0-0"
      else
        let isCapture = TMoveOps.isCaptureMove move // (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
        let isEnpass = TMoveOps.isEnPassantMove move //(move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
        let isPromotion = TMoveOps.isPromotionMove move //(move.MoveType &&& TPieceType.PROMO) <> TPieceType.EMPTY
        let symbol = if (isCapture || isEnpass ) then "x" else ""
        let piece = pieceSymbol &move
        let candidates = moves |> Array.filter(fun m -> m.To = move.To && move.MoveType = m.MoveType)
        let len = candidates.Length
        let start =
          if isPromotion then  //promotion here - may be replaced with longSan.length > 4
            if isCapture then longSan.[0].ToString() else ""
          elif isEnpass then
            longSan.[0].ToString()
          elif String.IsNullOrEmpty(piece) && isCapture then
            longSan.[0].ToString()
          elif len > 1 then
            let rest = candidates |> Array.map (fun m -> TMoveOps.moveToStr &m side)
            let letter = rest |> Array.map(fun moveStr -> moveStr.[0]) |> Array.distinct
            let numbers = rest |> Array.map(fun moveStr -> moveStr.[1]) |> Array.distinct
            if letter.Length >= numbers.Length then
              //use letter now
              if move.Promotion <> TPieceType.EMPTY then
                piece + longSan.[2].ToString()
              else
                piece + longSan.[0].ToString()
            else
              //use number now
              piece + longSan.[1].ToString()
          else
            piece
        let ends =
          if isPromotion then
            let piece = promoPiece move.Promotion
            "=" + piece.ToUpper()
          //elif isCheck longSan
          else
            ""
        // Use only one letter for each square name
        let finalSan = start + symbol + longSan.[2].ToString() + longSan.[3].ToString() + ends
        finalSan.Trim()


//todo - move these to a module
type FinalResult =
    | WhiteWins
    | Draw
    | BlackWins
    | Unknown

let parseFinalResult (input: string) =
    match input with
    | "1" -> WhiteWins
    | "0" -> Draw
    | "-1" -> BlackWins
    | _ -> Unknown

/// Parses one EPD/FEN line. The FEN prefix is delimited by FIELD COUNTING — placement,
/// side, castling, ep, plus the two counters when present and numeric — so opcode order
/// no longer matters (the old parser assumed bm/am came first and corrupted the FEN for
/// lines like "<fen> id \"x\"; dm 5;"). Opcodes are then scanned in a single quote-aware
/// pass split on ';' (no regex, no Split allocations); bm/am/id/other are looked up by
/// name with quotes stripped. Plain FEN-only lines (openings files) pass through with
/// the whole line as FEN, and Id falls back to the FEN as before.
let parseLine (line: string) : EPDEntry option =
    if System.String.IsNullOrWhiteSpace(line) then
        None
    else
        let len = line.Length
        let inline isWs (c: char) = c = ' ' || c = '\t'
        // The FEN always precedes the opcodes, so the first ';' (even one glued to the
        // last counter, "... 0 1; id ...") is a hard upper bound for FEN tokens — the
        // old parser's cut-at-semicolon behavior, kept alongside the field counting.
        let fenLimit =
            let i = line.IndexOf ';'
            if i < 0 then len else i
        // One whitespace-separated token from index i within the FEN region:
        // struct (start, endExclusive), or (-1, -1) past the limit.
        let nextToken (i: int) =
            let mutable s = i
            while s < fenLimit && isWs line.[s] do s <- s + 1
            if s >= fenLimit then struct (-1, -1)
            else
                let mutable e = s
                while e < fenLimit && not (isWs line.[e]) do e <- e + 1
                struct (s, e)
        let isDigits (s: int) (e: int) =
            let mutable ok = e > s
            for i in s .. e - 1 do
                if not (System.Char.IsDigit line.[i]) then ok <- false
            ok

        let struct (s1, e1) = nextToken 0
        let struct (_, e2) = if e1 < 0 then struct (-1, -1) else nextToken e1
        let struct (_, e3) = if e2 < 0 then struct (-1, -1) else nextToken e2
        let struct (_, e4) = if e3 < 0 then struct (-1, -1) else nextToken e3
        if e4 < 0 then
            // Fewer than four fields: not EPD-shaped — old fallback of treating the
            // whole line as a FEN (plain short lines; junk tolerated as before).
            let fen = line.Trim()
            Some { RawInput = line; FEN = fen; BestMove = None; AvoidMove = None
                   Id = (if fen = "" then None else Some fen); Other = None }
        else
            let mutable fenEnd = e4
            let struct (s5, e5) = nextToken fenEnd
            if s5 >= 0 && isDigits s5 e5 then
                fenEnd <- e5
                let struct (s6, e6) = nextToken fenEnd
                if s6 >= 0 && isDigits s6 e6 then fenEnd <- e6
            let fen = line.Substring(s1, fenEnd - s1)

            // Opcode region: split on ';' outside quotes; each segment = name + value.
            let ops = System.Collections.Generic.List<struct (string * string)>()
            let addSegment (s: int) (e: int) =
                let mutable a = s
                let mutable b = e
                while a < b && isWs line.[a] do a <- a + 1
                while b > a && isWs line.[b - 1] do b <- b - 1
                if b > a then
                    let mutable n = a
                    while n < b && not (isWs line.[n]) do n <- n + 1
                    let name = line.Substring(a, n - a)
                    let mutable va = n
                    while va < b && isWs line.[va] do va <- va + 1
                    let value =
                        if va >= b then ""
                        elif line.[va] = '"' && line.[b - 1] = '"' && b - va >= 2 then
                            line.Substring(va + 1, b - va - 2)
                        else line.Substring(va, b - va)
                    ops.Add(struct (name, value))
            // Quote-aware split; an unbalanced quote would swallow every later ';', so
            // that case degrades to a plain split (the old parser's tolerance level).
            let scan (respectQuotes: bool) =
                let mutable segStart = fenEnd
                let mutable i = fenEnd
                let mutable inQuotes = false
                while i < len do
                    let c = line.[i]
                    if respectQuotes && c = '"' then inQuotes <- not inQuotes
                    elif c = ';' && not inQuotes then
                        addSegment segStart i
                        segStart <- i + 1
                    i <- i + 1
                addSegment segStart len   // trailing segment without ';'
                inQuotes
            if scan true then
                ops.Clear()
                scan false |> ignore

            let find (key: string) =
                let mutable res = None
                for struct (n, v) in ops do
                    if res.IsNone && n = key then res <- Some v
                res
            let idOpt =
                match find "id" with
                | Some _ as id -> id
                | None -> if fen = "" then None else Some fen
            Some { RawInput = line; FEN = fen; BestMove = find "bm"; AvoidMove = find "am"
                   Id = idOpt; Other = find "other" }
