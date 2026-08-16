module ChessLibrary.GameAdjudication

open System
open System.IO
open System.Diagnostics
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.Chess
open ChessLibrary.PositionTypes
open ChessLibrary.TablebaseProbe

// Alias for backward compatibility with existing code that calls Formatting.createResultWithEval
module Formatting = ChessLibrary.TypesDef.CoreTypes

/// Abstracted function to count evaluations based on a condition
let countEvalsBasedOnCondition (evaluations: EvalType list) conditionFunc =
    evaluations
    |> Seq.takeWhile conditionFunc
    |> Seq.length

/// Function to check for sufficient high evaluations
let hasSufficientHighEvals (evaluations: EvalType list) minHighEvalSize minScoreThreshold =
    let isHighEval = function
        | CP score -> abs score >= minScoreThreshold
        | Mate _ -> true
        | _ -> false
    countEvalsBasedOnCondition (List.truncate minHighEvalSize evaluations) isHighEval >= minHighEvalSize

/// Function to check if two evaluations agree on which side is winning
let evalsAgreeOnWinner (eval1: EvalType) (eval2: EvalType) =
    match eval1, eval2 with
    | CP e1, CP e2 -> (e1 > 0.0 && e2 > 0.0) || (e1 < 0.0 && e2 < 0.0)
    | Mate m1, Mate m2 -> (m1 > 0 && m2 > 0) || (m1 < 0 && m2 < 0)
    | _ ->
        false  // If either is NA, don't agree

/// Function to check for sufficient low evaluations
let consecutiveNumberOfLowEvalsLeft (evaluations: EvalType list) minLowEvalSize maxDrawScore =
    let isLowEval = function
        | CP score -> abs score <= maxDrawScore
        | Mate _ -> false
        | _ -> false
    let consecutiveLows = countEvalsBasedOnCondition (List.truncate minLowEvalSize evaluations) isLowEval
    minLowEvalSize - consecutiveLows

let movesLeftBeforeDrawAdjudication (eval:EvalType) (evals: EvalType list) minMoveNumber drawPlies maxDrawScore =
    match eval with
    |Mate _ -> drawPlies
    |CP ev when abs ev > maxDrawScore -> drawPlies
    |_ when evals.Length >= (minMoveNumber * 2) ->
        consecutiveNumberOfLowEvalsLeft evals drawPlies maxDrawScore
    |_ -> drawPlies

/// A function to check if a list of evaluations is too low for a draw
let isConsecutiveLowEvalSufficient (evals: EvalType list) drawPlies maxDrawScore =
    let res = consecutiveNumberOfLowEvalsLeft evals drawPlies maxDrawScore
    res <= 0

/// A function to check if the tablebase adjudication should be applied
let shouldAdjudicateTB (evals: EvalType list) (piecesLeft: int) tbMen =
    if piecesLeft <= tbMen && evals.Length > 1 then
        match evals.[0], evals.[1] with
        |fst, snd when fst.WinAdj 5 && snd.WinAdj 5 -> true //draw
        |fst, snd when fst.DrawAdj 1 && snd.DrawAdj 1 -> true //win
        |_ -> false
    else
        false

/// A function to determine the winner and the result by evaluation agreement
let adjudicateByEval
    (logger: ILogger)
    (board:Board)
    (evals: EvalType list)
    (tourny: Tournament)
    (player1: string)
    (player2: string)
    (playedLastMove: string)
    gametimer
    gameMoveList
    moves =
    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let drawPlyLength = tourny.Adjudication.DrawOption.DrawMoveLength * 2
    let winPlyLength = tourny.Adjudication.WinOption.WinMoveLength * 2
    let tooHighEvals () = hasSufficientHighEvals evals winPlyLength tourny.Adjudication.WinOption.MinWinScore
    let tooLowEvals () = isConsecutiveLowEvalSufficient evals drawPlyLength tourny.Adjudication.DrawOption.MaxDrawScore
    let mutable posToCheck = board.Position
    let piecesLeft = PositionOps.numberOfPieces &posToCheck
    let withTBadjudicationMen =
        if tourny.Adjudication.TBAdj.UseTBAdjudication then
            tourny.Adjudication.TBAdj.TBMen
        else
            2
    let firstTwoEvals () =
        match evals |> List.rev with
        |[] -> []
        |[x] -> [x]
        |x::y::_ -> [x;y]

    // The tablebase branch answers only for positions it can probe. When the probe
    // yields nothing it must not swallow the position: repetition, insufficient
    // material, the 50-move rule, checkmate and stalemate all live in the chain below,
    // and an endgame small enough for a tablebase is exactly where they matter most.
    let tbAdjudication () =
      if piecesLeft <= withTBadjudicationMen then
          let firstTwoEvals = firstTwoEvals ()
          let tryProbe =
              try
                  let dir = tourny.Adjudication.TBAdj.TablebaseDirectory
                  if String.IsNullOrEmpty(dir) |> not && Directory.Exists dir then
                      let fen = board.FEN()
                      match runFathomSafe dir fen 3000 with
                      | Some tableRes ->
                          let tb = parse tableRes
                          match tb.Wdl with
                          | Some "Win" ->
                              let res = if board.Position.STM = 0uy then "1-0" else "0-1"
                              Formatting.createResultWithEval player1 player2 gameMoveList res ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                          | Some "Draw" ->
                              Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                          | Some "Loss" ->
                              let res = if board.Position.STM = 0uy then "0-1" else "1-0"
                              Formatting.createResultWithEval player1 player2 gameMoveList res ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                          | _ -> None
                      | None -> None
                  else None
              with ex ->
                  logger.LogWarning(ex, "TB adjudication probe failed; continuing without TB")
                  None

          if tryProbe.IsSome then
              tryProbe

          elif tryProbe.IsNone && shouldAdjudicateTB evals piecesLeft withTBadjudicationMen then
              try
                  match evals.[0] with
                  | EvalType.CP ev when ev > 5.0 ->
                      Formatting.createResultWithEval player1 player2 gameMoveList "1-0" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.CP ev when ev < -5.0 ->
                      Formatting.createResultWithEval player1 player2 gameMoveList "0-1" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.CP _ ->
                      Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.Mate m when m > 0 ->
                      Formatting.createResultWithEval player1 player2 gameMoveList "1-0" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.Mate m when m < 0 ->
                      Formatting.createResultWithEval player1 player2 gameMoveList "0-1" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.Mate m -> // mate 0 or mate -0
                      if m = -0 then
                          Formatting.createResultWithEval player1 player2 gameMoveList "0-1" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                      else
                          Formatting.createResultWithEval player1 player2 gameMoveList "1-0" ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | EvalType.NA ->
                      logger.LogCritical("TB adjudication fallback skipped: NA eval")
                      None
              with ex ->
                  logger.LogCritical(ex, "Error during TB adjudication fallback")
                  None
          else None
      // Too many pieces for a tablebase — nothing for this branch to say.
      else None

    let otherAdjudication () =
      if moves >= (tourny.Adjudication.WinOption.MinWinMove * 2 + winPlyLength) && tooHighEvals() then
          let result =
              match evals.[0] with
              |EvalType.CP ev when ev >= tourny.Adjudication.WinOption.MinWinScore -> "1-0" |> Some
              |EvalType.CP ev when ev <= -tourny.Adjudication.WinOption.MinWinScore -> "0-1" |> Some
              |EvalType.Mate m when m > 0 -> "1-0" |> Some
              |EvalType.Mate m when m < 0 -> "0-1" |> Some
              |EvalType.Mate m -> //mate 0 or mate -0
                  if m = -0 then "0-1" |> Some else "1-0" |> Some
              |EvalType.CP cp ->
                  logger.LogCritical("High evals but not reaching MinWinScore: {Eval}", cp)
                  None
              |NA ->
                  logger.LogCritical("High evals but NA eval found")
                  None
          match result with
          |None -> None
          |Some result ->
              // Check if engines agree on winner before adjudicating
              let pos = board.Position
              let ismate = board.AnyLegalMove() |> not && (ChessLibrary.MoveGeneration.InCheck &pos <> 0UL)
              if ismate then
                  let gameRes = if pos.STM = 0uy then "0-1" else "1-0"
                  let res = Formatting.createResultWithEval player1 player2 gameMoveList gameRes ResultReason.AdjudicatedEvaluation dur (firstTwoEvals())
                  Some res

              elif evals.Length >= 2 && not (evalsAgreeOnWinner evals.[0] evals.[1]) then
                  logger.LogDebug("High evals but engines disagree on winner (signs differ) - not adjudicating")
                  None
              else
                  let res = Formatting.createResultWithEval player1 player2 gameMoveList result ResultReason.AdjudicatedEvaluation dur (firstTwoEvals())
                  Some res

      elif moves >= (tourny.Adjudication.DrawOption.MinDrawMove * 2 + drawPlyLength) && tooLowEvals() then
          let res = Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.AdjudicatedEvaluation dur (firstTwoEvals())
          Some res

      elif board.InsufficientMaterial() then
          let res = Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.AdjudicateMaterial dur (firstTwoEvals())
          Some res

      elif board.ClaimThreeFoldRep () then
          let res = Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.Repetition dur (firstTwoEvals())
          Some res

      elif board.AnyLegalMove() |> not then
          let mutable mypos = board.Position
          let check = ChessLibrary.MoveGeneration.InCheck &mypos <> 0UL
          if check then
              if playedLastMove = player1 then
                  let res = Formatting.createResultWithEval player1 player2 gameMoveList "1-0" ResultReason.Checkmate dur  (firstTwoEvals())
                  Some res
              else
                  let res = Formatting.createResultWithEval player1 player2 gameMoveList "0-1" ResultReason.Checkmate dur (firstTwoEvals())
                  Some res
          else
              let res = Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.Stalemate dur (firstTwoEvals())
              Some res

      elif board.Position.Count50 >= 100uy then
          let res = Formatting.createResultWithEval player1 player2 gameMoveList "1/2-1/2" ResultReason.ExcessiveMoves dur (firstTwoEvals())
          Some res
      else
          None

    match tbAdjudication () with
    | Some res -> Some res
    | None -> otherAdjudication ()
