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

/// Parses a single line of input according to our simple rules.
let parseLine (line: string) : EPDEntry option =
    if System.String.IsNullOrWhiteSpace(line) then
        None
    else
        // Look for the marker " am " or " bm "
        let iAm = line.IndexOf(" am ")
        let iBm = line.IndexOf(" bm ")
        let markerIndex, marker =
            if iAm >= 0 && (iBm < 0 || iAm < iBm) then (iAm, "am")
            elif iBm >= 0 then (iBm, "bm")
            else
                // If neither marker is found, use the semicolon (if any)
                let iSemi = line.IndexOf(';')
                ((if iSemi >= 0 then iSemi else line.Length), "")

        // The FEN is everything before the marker (or semicolon).
        let fen = line.Substring(0, markerIndex).Trim()

        // Look for an " id " field anywhere in the line.
        let idOpt =
            let idKey = " id "
            let idIdx = line.IndexOf(idKey)
            if idIdx >= 0 then
                // Everything after " id ".
                let afterId = line.Substring(idIdx + idKey.Length).Trim()
                // If the id is quoted, remove the quotes.
                if afterId.StartsWith("\"") then
                    let closingQuote = afterId.IndexOf('"', 1)
                    if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                    else Some afterId
                else Some afterId
            elif String.IsNullOrEmpty fen |> not then
                Some fen
            else None

        // Look for an " bm " field anywhere in the line.
        let bmOpt =
            let idKey = " bm "
            let idIdx = line.IndexOf(idKey)
            if idIdx >= 0 then
                // Everything after " bm ".
                let afterId = line.Substring(idIdx + idKey.Length).Trim()
                // If the bm is quoted, remove the quotes.
                if afterId.StartsWith("\"") then
                    let closingQuote = afterId.IndexOf('"', 1)
                    if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                    else Some afterId
                else
                  let closingQuote = afterId.IndexOf(';', 1)
                  if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                  else Some afterId

            else None

        // Look for an " am " field anywhere in the line.
        let amOpt =
            let idKey = " am "
            let idIdx = line.IndexOf(idKey)
            if idIdx >= 0 then
                // Everything after " am ".
                let afterId = line.Substring(idIdx + idKey.Length).Trim()
                // If the am is quoted, remove the quotes.
                if afterId.StartsWith("\"") then
                    let closingQuote = afterId.IndexOf('"', 1)
                    if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                    else Some afterId
                else
                  let closingQuote = afterId.IndexOf(';', 1)
                  if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                  else Some afterId
            else None

        let other =
            let idKey = " other "
            let idIdx = line.IndexOf(idKey)
            if idIdx >= 0 then
                // Everything after " other ".
                let afterId = line.Substring(idIdx + idKey.Length).Trim()
                // If the other is quoted, remove the quotes.
                if afterId.StartsWith("\"") then
                    let closingQuote = afterId.IndexOf('"', 1)
                    if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                    else Some afterId
                else
                  let closingQuote = afterId.IndexOf(';', 1)
                  if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                  else Some afterId
            else None

        Some {RawInput = line; FEN = fen; BestMove = bmOpt; AvoidMove = amOpt; Id = idOpt; Other = other }
