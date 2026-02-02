namespace ChessLibrary

open System

/// Miscellaneous types used throughout the chess library.
/// Contains EvalType, ResultReason, and related utilities.
module MiscTypes =
    /// The starting position in FEN notation.
    let startPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    type EvalType =
      | CP of Info: float
      | Mate of Info: int
      | NA
      with
        override this.ToString() =
          match this with
          | CP info -> if Math.Abs(info) < 0.005 then "0.00" else sprintf "%.2f" info
          | Mate info -> if info > 0 then sprintf "M%d" info else sprintf "-M%d" (abs info)
          | NA -> "None"
        member x.Value =
          match x with
          | CP cp -> cp
          | Mate m -> float m
          | _ -> failwith "EvalType is NA"
        member x.ValueStr =
          match x with
          | CP cp -> cp.ToString()
          | Mate m -> sprintf "M%d" m
          | _ -> failwith "EvalType is NA"
        member x.WinAdj v =
          match x with
          | CP cp -> abs cp > v
          | Mate _ -> true
          | _ -> failwith "EvalType is NA"
        member x.DrawAdj v =
          match x with
          | CP cp -> abs cp < v
          | Mate _ -> true
          | _ -> failwith "EvalType is NA"

    type ResultReason =
        | Checkmate
        | Stalemate
        | AdjudicateTB
        | AdjudicateMaterial
        | ExcessiveMoves
        | Repetition
        | AdjudicatedEvaluation
        | ForfeitLimits
        | Cancel
        | Illegal
        | Disconnected of string
        | NotStarted
        | AdjudicatedByUser

        override this.ToString() =
            match this with
            | Checkmate -> "CM"
            | Stalemate -> "SM"
            | AdjudicateTB -> "TB"
            | AdjudicateMaterial -> "AM"
            | ExcessiveMoves -> "50m"
            | Repetition -> "R3"
            | AdjudicatedEvaluation -> "AE"
            | ForfeitLimits -> "FL"
            | Cancel -> "XX"
            | Illegal -> "IM"
            | Disconnected p -> "DC"
            | NotStarted -> "NS"
            | AdjudicatedByUser -> "AU"

        member this.Explanation =
            match this with
            | Checkmate -> "Checkmate"
            | Stalemate -> "Stalemate"
            | AdjudicateTB -> "Tablebase known result"
            | AdjudicateMaterial -> "Insufficient material"
            | ExcessiveMoves -> "Too many moves"
            | Repetition -> "Repetition draw"
            | AdjudicatedEvaluation -> "Evaluation agreement"
            | ForfeitLimits -> "Time/node limit forfeit"
            | Cancel -> "Game was cancelled"
            | Illegal -> "Illegal move"
            | Disconnected p -> sprintf "%s Disconnected" p
            | NotStarted -> "Not started"
            | AdjudicatedByUser -> "Adjudicated by user"

    let stringToResultReason (str: string): ResultReason =
        match str with
        | "CM" -> Checkmate
        | "SM" -> Stalemate
        | "TB" -> AdjudicateTB
        | "AM" -> AdjudicateMaterial
        | "50m" -> ExcessiveMoves
        | "R3" -> Repetition
        | "AE" -> AdjudicatedEvaluation
        | "FL" -> ForfeitLimits
        | "XX" -> Cancel
        | "IM" -> Illegal
        | "DC" -> Disconnected ""
        | "NS" -> NotStarted
        | "AU" -> AdjudicatedByUser
        | _ -> failwith "Invalid ResultReason string"
