module ChessLibrary.PuzzleEngineAgent

open System
open System.Threading
open System.Threading.Channels
open System.Collections.Concurrent
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PuzzleTypes
open ChessLibrary.Chess
open ChessLibrary.Statistics
open ChessLibrary.TypesDef.PuzzleInput
open ChessLibrary.PuzzleEngineAnalysis

//Start the engine agent for value head tests (single consumer of UCI calls)
let startValueEngineAgent (engineCfg:EngineConfig) =
    MailboxProcessor.Start(fun inbox ->
      // Spin up one engine instance
      match getPuzzleValueEngine engineCfg with
      | Some engine ->
          engine.Name <- engine.Name

          let rec loop() = async {
            let! msg = inbox.Receive()
            match msg with
            | Quit reply ->
                engine.Quit()
                do! Async.Sleep 1000
                reply.Reply()
            | _ ->
                try
                    match msg with
                    | Ok reply ->
                        reply.Reply(true)
                    | NewGame reply ->
                        engine.UciNewGame()
                        engine.WaitForReadyOk() |> ignore
                        reply.Reply()
                    | BestMove (cmd, reply) ->
                        let mv = bestQPuzzleValueOnly engine cmd
                        reply.Reply (mv,0.0)
                    | BestMoveWithPolicy (cmd, correctMove, reply) ->
                        let mv = bestQPuzzleValueOnly engine cmd
                        reply.Reply (mv,String.Empty)
                    | BestMoveWithAllPolicies (_, reply) ->
                        reply.Reply ("", [])
                    | EvalAllMovesValue (_, reply) ->
                        reply.Reply []
                    | ValueTopNEval (_, reply) ->
                        reply.Reply ("", [])
                    | BestMoveValueHead (cmd, reply) ->
                        let mv = bestQPuzzleValueOnly engine cmd
                        reply.Reply mv
                    | SolvePuzzle (_, reply) ->
                        reply.Reply ("", "", ResizeArray())
                    | Network reply ->
                        reply.Reply engine.Network
                    | Quit _ -> ()
                with ex ->
                    eprintfn "PuzzleEngineAgent (value) error: %s" ex.Message
                    match msg with
                    | BestMove (_, reply) -> reply.Reply ("", 0.0)
                    | BestMoveWithPolicy (_, _, reply) -> reply.Reply ("", String.Empty)
                    | BestMoveWithAllPolicies (_, reply) -> reply.Reply ("", [])
                    | EvalAllMovesValue (_, reply) -> reply.Reply []
                    | ValueTopNEval (_, reply) -> reply.Reply ("", [])
                    | BestMoveValueHead (_, reply) -> reply.Reply ""
                    | SolvePuzzle (_, reply) -> reply.Reply ("", "", ResizeArray())
                    | NewGame reply -> reply.Reply()
                    | Ok reply -> reply.Reply(false)
                    | Network reply -> reply.Reply ""
                    | Quit _ -> ()
                return! loop()
          }
          loop()
      | None ->
          let rec loop() = async {
            let! msg = inbox.Receive()
            match msg with
            | Ok reply ->
                reply.Reply(false)
                return! loop()
            | NewGame reply ->
                reply.Reply()
                return! loop()
            | SolvePuzzle (_, reply) ->
                reply.Reply ("", "", ResizeArray())
                return! loop()
            | Quit reply ->
                reply.Reply()
            | _ ->
                return! loop()
          }
          loop()

    )

//Start the engine agent for policy head tests (single consumer of UCI calls)
let startPolicyEngineAgent (engineCfg:EngineConfig) nodes =
    MailboxProcessor.Start(fun inbox ->
      // Spin up one engine instance
      let engine = getPuzzlePolicyEngine (engineCfg,None)
      engine.Name <- engine.Name

      let rec loop() = async {
        let! msg = inbox.Receive()
        match msg with
        | Quit reply ->
            engine.StopProcess()
            reply.Reply()
        | _ ->
            try
                match msg with
                | Ok reply ->
                     reply.Reply(true)
                | NewGame reply ->
                    engine.UciNewGame()
                    engine.WaitForReadyOk() |> ignore
                    reply.Reply()
                | BestMove (cmd, reply) ->
                    let mv, nnValue = bestPolicyMove nodes engine cmd.Command
                    reply.Reply (mv,(if nnValue.IsSome then 0.0 else 0.0))
                | BestMoveWithPolicy (cmd, correctMove, reply) ->
                    let mv, nnValue = bestPolicyMoveWithPolicy correctMove nodes engine cmd.Command
                    if nnValue.Length = 0 then
                      reply.Reply (mv, String.Empty)
                    elif nnValue.Length = 1 then
                      reply.Reply (mv, sprintf "%.2f" nnValue.Head.P)
                    else
                      let nnValueString = nnValue |> List.map (fun v -> sprintf "%.2f" v.P) |> String.concat ", "
                      reply.Reply (mv, nnValueString)
                | BestMoveWithAllPolicies (cmd, reply) ->
                    let mv, allNNValues = bestPolicyMoveAllPolicies nodes engine cmd.Command
                    reply.Reply (mv, allNNValues)
                | EvalAllMovesValue (cmd, reply) ->
                    let moveVals = evaluateAllMovesV nodes engine cmd.Command
                    reply.Reply moveVals
                | ValueTopNEval (cmd, reply) ->
                    let mv, allNNValues = bestMoveAllPoliciesWithLegalMoveNodes engine cmd.Command
                    reply.Reply (mv, allNNValues)
                | BestMoveValueHead (cmd, reply) ->
                    let mv = bestQPuzzleValueOnly engine cmd
                    reply.Reply mv
                | SolvePuzzle (cmd, reply) ->
                    let bm, pv, nnValues = solvePuzzleSearch nodes engine cmd
                    reply.Reply (bm, pv, nnValues)
                | Network reply ->
                    reply.Reply engine.Network
                | Quit _ -> ()
            with ex ->
                eprintfn "PuzzleEngineAgent (policy) error: %s" ex.Message
                match msg with
                | BestMove (_, reply) -> reply.Reply ("", 0.0)
                | BestMoveWithPolicy (_, _, reply) -> reply.Reply ("", String.Empty)
                | BestMoveWithAllPolicies (_, reply) -> reply.Reply ("", [])
                | EvalAllMovesValue (_, reply) -> reply.Reply []
                | ValueTopNEval (_, reply) -> reply.Reply ("", [])
                | BestMoveValueHead (_, reply) -> reply.Reply ""
                | SolvePuzzle (_, reply) -> reply.Reply ("", "", ResizeArray())
                | NewGame reply -> reply.Reply()
                | Ok reply -> reply.Reply(false)
                | Network reply -> reply.Reply ""
                | Quit _ -> ()
            return! loop()
      }
      loop()
    )


//Per-puzzle async workflow
/// `scoreAllPositions` keeps querying after the first mistake so every position of a
/// multi-move puzzle is scored. Unlike the policy paths this costs real engine time -
/// positions after a miss are skipped today - which is why it is opt-in.
let runPuzzleViaAgentEx (agent:MailboxProcessor<EngineMsg>) (valueHead : bool) (scoreAllPositions: bool) (puzzle:CsvPuzzleData)  = async {
    // Reset engine state between puzzles
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    // 3b) Fresh board per puzzle
    let board = Board()
    let mutable correct    = true
    let mutable movePlayed = ""
    let mutable failedMove = ""
    let mutable policy = String.Empty
    let mutable posCorrect = 0
    let mutable posScored = 0
    let mutable firstMoveCorrect = 0
    let mutable firstMoveScored = 0

    for cmd in puzzle.Commands do
      // Without the flag this stops at the first mistake, as it always has.
      if correct || scoreAllPositions then
        // Ask engine for its candidate move
        let! (mv,p) = agent.PostAndAsyncReply(fun ch -> BestMoveWithPolicy(cmd, cmd.CorrectMove, ch))
        // Only the FIRST failure is reported downstream (EPD writer, visualizer), so
        // later moves must not overwrite what the failing position played.
        if correct then movePlayed <- mv

        // Compare to correct move, with mate-in-one fallback
        let mutable solved = cmd.CorrectMove = mv
        if not solved then
          // IsMate, not AnyLegalMove: the latter is also true for STALEMATE, which would
          // credit a move that throws the win away as if it solved the puzzle.
          board.PlayCommands cmd.Command
          board.PlayUciMove mv
          solved <- board.IsMate()
        // Position counters ONLY when the flag is on. Without it the loop stops at the
        // first mistake, so counting anyway would report a censored denominator while the
        // field docs promise 0 means "not measured" - and the ratio would move the wrong
        // way, since a net that fails LATER adds correct prefix positions.
        if scoreAllPositions then
            if solved then posCorrect <- posCorrect + 1
            posScored <- posScored + 1
        // Commands[0] is the move the puzzle exists for; its themes describe THIS move.
        // Tracked either way: that position is always queried.
        if firstMoveScored = 0 then
            firstMoveScored <- 1
            if solved then firstMoveCorrect <- 1
        if correct then
          if not solved then
            failedMove <- cmd.CorrectMove
            policy <- p
          correct <- solved

    // Sentinel: when the engine returned an empty bestmove for a failed puzzle
    // (e.g. the agent's exception handler fell back to ""), stamp "0000" so the
    // EPD writer and visualizer can still identify the failing command instead
    // of silently dropping the puzzle.
    let stampedMovePlayed =
        if not correct && System.String.IsNullOrEmpty movePlayed then PuzzleDataUtils.NullBestmove
        else movePlayed
    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = stampedMovePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }

    //Return minimal puzzle result
    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = valueHead
        Policy = policy
        PositionsCorrect = posCorrect
        PositionsScored = posScored
        FirstMoveCorrect = firstMoveCorrect
        FirstMoveScored = firstMoveScored
        KLD = 0.0
        EngineRank = 0
        MarginLoss = 0.0
        ValueLoss = 0.0
      }
  }

/// Engine's rank (1-indexed) of the correct move in allNNValues (sorted desc by P).
/// Returns 0 if the move is not found.
let findEngineRank (allNNValues: EngineTypes.NNValues list) (correctMove: string) =
    allNNValues
    |> List.tryFindIndex (fun v -> v.LANMove = correctMove)
    |> Option.map (fun i -> i + 1)
    |> Option.defaultValue 0

/// Weighted average of KLD values using 1/rank as weight.
/// rank <= 0 (correct move not in the returned policy list) is folded in at
/// effective rank 30 so the worst puzzles still contribute a gradient signal
/// without dominating the mean.
let computeRankWeightedKld (items: seq<float * int>) =
    let beyondTopNRank = 30
    let mutable wSum = 0.0
    let mutable wKldSum = 0.0
    for (kld, rank) in items do
        let effectiveRank = if rank > 0 then rank else beyondTopNRank
        let w = 1.0 / float effectiveRank
        wSum <- wSum + w
        wKldSum <- wKldSum + w * kld
    if wSum > 0.0 then wKldSum / wSum else 0.0

/// Frontier-weighted KLD: weights peak at rank 2-3 (near-misses), low at rank 1
/// (already solved) and rank 6+ (too far to flip). Targets the accuracy/Elo
/// frontier where small policy improvements translate to move-selection changes.
/// rank <= 0 (correct move not in the returned list) gets the same 0.02 floor
/// as rank 11+, so worst-case puzzles contribute a small but nonzero weight.
let computeFrontierWeightedKld (items: seq<float * int>) =
    let frontierWeight rank =
        match rank with
        | r when r <= 0 -> 0.02  // correct move beyond top-N, same floor as deep ranks
        | 1 -> 0.3
        | 2 -> 1.0
        | 3 -> 0.8
        | 4 -> 0.5
        | 5 -> 0.3
        | r when r <= 10 -> 0.1
        | _ -> 0.02
    let mutable wSum = 0.0
    let mutable wKldSum = 0.0
    for (kld, rank) in items do
        let w = frontierWeight rank
        wSum <- wSum + w
        wKldSum <- wKldSum + w * kld
    if wSum > 0.0 then wKldSum / wSum else 0.0

/// Margin loss: -log(P_correct / (P_correct + P_best_competitor)).
/// Measures how decisively the engine prefers the correct move over its best alternative.
/// Returns 0.0 when P_correct >> P_competitor (confident and right), log(2)≈0.693 when
/// they're equal (coin flip), and large values when the competitor dominates.
/// Naturally frontier-weighted: steepest gradient at the decision boundary.
/// Assumes allNNValues is sorted descending by P (as returned by BestMoveWithAllPolicies).
let computeMarginLoss (allNNValues: EngineTypes.NNValues list) (correctMove: string) =
    if allNNValues.Length < 2 then 0.0
    else
        let correctP =
            match allNNValues |> List.tryFind (fun v -> v.LANMove = correctMove) with
            | Some v when v.P > 0.0 -> v.P
            | _ -> 0.01  // floor at 0.01%
        let competitorP =
            match allNNValues |> List.tryFind (fun v -> v.LANMove <> correctMove) with
            | Some v when v.P > 0.0 -> v.P
            | _ -> 0.01
        -log(correctP / (correctP + competitorP))

/// Aggregated margin loss, weighted by solved status. Unsolved puzzles get
/// `unsolvedWeight` x the weight of solved ones (default 2.0), amplifying the
/// SPSA signal from puzzles the engine currently gets wrong. Degenerates to
/// uniform mean when all puzzles share the same status. Replaced the old
/// uniform average on 2026-04-11.
let computeWeightedMarginLoss (items: seq<float * bool>) (unsolvedWeight: float) =
    let mutable num = 0.0
    let mutable den = 0.0
    for (margin, solved) in items do
        let w = if solved then 1.0 else unsolvedWeight
        num <- num + w * margin
        den <- den + w
    if den > 0.0 then num / den else 0.0

/// Value head loss: one-sided quadratic penalty when Q is below the theme
/// threshold (mate 0.95, winning 0.85). No penalty for Q above threshold —
/// more confidence on winning positions is always fine. Equality is two-sided
/// with a ±0.3 dead zone around zero. Solved puzzles only (returns -1.0 sentinel
/// when unsolved, caller filters these out). Thresholds calibrated 2026-04-11
/// against manual Ceres testing at low node counts.
let computeValueLoss (allNNValues: EngineTypes.NNValues list) (correctMove: string) (themes: string) (wasSolved: bool) =
    if not wasSolved || allNNValues.IsEmpty then -1.0  // sentinel: exclude from aggregation
    else
        let q =
            match allNNValues |> List.tryFind (fun v -> v.LANMove = correctMove) with
            | Some v -> v.Q
            | None -> 0.0
        let t = themes.ToLowerInvariant()
        if t.Contains("equality") then
            // Equality: Q should be near zero. Penalize |Q| > 0.3 in either direction.
            let gap = max 0.0 (abs q - 0.3)
            gap * gap
        else
            // Winning puzzles: one-sided, only penalize Q below threshold.
            // Zero loss once Q >= threshold regardless of how much higher.
            let threshold =
                if t.Contains("mate") then 0.95
                elif t.Contains("crushing") then 0.85
                elif t.Contains("advantage") then 0.85
                else 0.85  // untagged — treat as completely winning
            let gap = max 0.0 (threshold - q)
            gap * gap

// Fixed constants matching modern Lc0 match-play tuning (validated 2026-07-27
// against verbose-move-stats visit patterns of a BT4 engine def). Deliberately
// NOT read from the engine's actual options so the metric stays comparable
// across engines with different tuned search parameters.
let estNodesCPuct = 2.897
let estNodesFpu = 0.98416

/// Estimated parent visits a PUCT search needs before first exploring the correct
/// move, from the FPU-reduction first-visit condition (suggested by Kovax):
///   FpuValue * sqrt(sum_of_higher_policies) <= P_correct * CPuct * sqrt(N)
/// solved for N. Moves ranked above the correct move get visited first and their
/// accumulated policy mass drives the FPU dock on unvisited children. Rank 1
/// returns 0 (explored immediately). Correct move missing or P=0 uses the same
/// 0.01% floor as computeKLD with all listed policy mass counted as "higher".
/// Order-of-magnitude heuristic: real engines grow CPuct with N and treat FPU at
/// root specially. Assumes allNNValues is sorted descending by P.
let computeEstNodesToFind (allNNValues: EngineTypes.NNValues list) (correctMove: string) =
    if allNNValues.IsEmpty then 0.0  // no policy data (e.g. classical engine)
    else
        // P values are in percent; convert to fractions.
        let pCorrect, sumHigherPct =
            match allNNValues |> List.tryFind (fun v -> v.LANMove = correctMove) with
            | Some v when v.P > 0.0 ->
                let higher = allNNValues |> List.takeWhile (fun v -> v.LANMove <> correctMove)
                v.P / 100.0, (higher |> List.sumBy (fun v -> max 0.0 v.P))
            | _ ->
                0.01 / 100.0, (allNNValues |> List.sumBy (fun v -> max 0.0 v.P))
        let sumHigher = sumHigherPct / 100.0
        if sumHigher <= 0.0 then 0.0  // correct move ranked first
        else (estNodesFpu * sqrt sumHigher / (estNodesCPuct * pCorrect)) ** 2.0

/// Lc0/Ceres verbose move stats print castling as king-takes-rook (e1h1) even in
/// standard chess, while lichess puzzle solutions use king-destination (e1g1).
/// Rewrite the alias in the parsed list so downstream string comparisons match.
/// Fires only when the list lacks the standard notation but contains the
/// king-takes-rook string, so genuine piece moves like Re1-g1 and Chess960
/// positions are unaffected.
let normalizeCastlingAliases (correctMove: string) (allNNValues: EngineTypes.NNValues list) =
    let alias =
        match correctMove with
        | "e1g1" -> Some "e1h1"
        | "e1c1" -> Some "e1a1"
        | "e8g8" -> Some "e8h8"
        | "e8c8" -> Some "e8a8"
        | _ -> None
    match alias with
    | Some kxr when not (allNNValues |> List.exists (fun v -> v.LANMove = correctMove))
                    && (allNNValues |> List.exists (fun v -> v.LANMove = kxr)) ->
        allNNValues |> List.map (fun v -> if v.LANMove = kxr then { v with LANMove = correctMove } else v)
    | _ -> allNNValues

/// Fraction of values at or below the threshold (0..1). 0.0 for an empty sample.
let fractionAtOrBelow (threshold: float) (values: float[]) =
    if values.Length = 0 then 0.0
    else float (values |> Array.sumBy (fun v -> if v <= threshold then 1 else 0)) / float values.Length

/// Nearest-rank percentile: smallest value with at least q% of the sample at or
/// below it. q in (0, 100]. Returns 0.0 for an empty sample.
let percentile (q: float) (values: float[]) =
    if values.Length = 0 then 0.0
    else
        let sorted = Array.sort values
        let idx = int (ceil (q / 100.0 * float sorted.Length)) - 1
        sorted.[max 0 (min (sorted.Length - 1) idx)]

/// Compute KLD for one move: -log(P_correct / 100).
/// When correct move not found or P=0, returns -log(0.01/100) ≈ 9.21 (floor at 0.01%).
let computeKLD (allNNValues: EngineTypes.NNValues list) (correctMove: string) =
    if allNNValues.IsEmpty then 0.0  // no policy data (e.g. classical engine)
    else
        match allNNValues |> List.tryFind (fun v -> v.LANMove = correctMove) with
        | Some v when v.P > 0.0 -> -log(v.P / 100.0)
        | _ -> -log(0.01 / 100.0)  // floor: treat as 0.01% policy

/// Softmax with numerical stability (subtract max before exp).
let softmax (values: float list) =
    if values.IsEmpty then []
    else
        let maxV = values |> List.max
        let exps = values |> List.map (fun v -> exp(v - maxV))
        let sumExps = exps |> List.sum
        exps |> List.map (fun e -> e / sumExps)

/// Compute Value KLD from per-move V evaluations. V values should already be negated
/// (from parent's perspective: higher = better for us).
let computeValueKLD (moveVals: (string * float) list) (correctMove: string) =
    if moveVals.IsEmpty then 0.0
    else
        let probs = softmax (moveVals |> List.map snd)
        let moveProbs = List.zip (moveVals |> List.map fst) probs
        match moveProbs |> List.tryFind (fun (m, _) -> m = correctMove) with
        | Some (_, p) when p > 0.0 -> -log(p)
        | _ -> -log(0.01 / 100.0)

/// Per-puzzle multi-topN workflow: one engine call, check all thresholds at once.
/// Returns (puzzle, maxKLD, maxMarginLoss, engineRankAtMaxKld, avgValueLoss, maxEstNodes, correctPerTopN) where:
///   * maxKLD = max log-loss across the puzzle's commands
///   * engineRankAtMaxKld = engine's rank (1-indexed) of the correct move AT the
///     command that produced maxKLD; 0 if no rank could be determined.
///   * maxEstNodes = max computeEstNodesToFind across the puzzle's commands.
///   * correctPerTopN maps each topN to whether the puzzle was solved at that threshold.
let runPuzzleViaAgentMultiTopN (agent:MailboxProcessor<EngineMsg>) (topNs:int list) (scoreAllPositions: bool) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let maxTopN = topNs |> List.max
    let mutable maxKLD = 0.0
    let mutable maxMarginLoss = 0.0
    let mutable maxEstNodes = 0.0
    let mutable engineRankAtMaxKld = 0
    let valueLosses = ResizeArray<float>()
    // Track correctness per topN threshold
    let correct = System.Collections.Generic.Dictionary<int, bool>()
    for n in topNs do correct.[n] <- true
    // Per-POSITION tally, alongside the all-or-nothing per-puzzle flag above. The engine
    // is already asked about every position here, so this costs nothing extra - what was
    // missing is that positions after the first mistake were never scored, making a
    // 3-of-4 puzzle indistinguishable from a 0-of-4 one.
    let posCorrect = System.Collections.Generic.Dictionary<int, int>()
    for n in topNs do posCorrect.[n] <- 0
    let mutable posScored = 0
    // Commands[0] only: the thematic move. Costs nothing - that position is always queried.
    let firstMoveCorrect = System.Collections.Generic.Dictionary<int, int>()
    for n in topNs do firstMoveCorrect.[n] <- 0
    let mutable firstMoveScored = 0
    // Position index is tracked separately from posScored, which is now flag-gated and
    // would otherwise stop identifying the first position when the flag is off.
    let mutable positionIndex = 0
    let mutable isFirstPosition = false
    // Capture the first top-1 failure so EPD writer and UI visualizer can show
    // the engine's wrong move (MovePlayed) on the correct board.
    let mutable firstFailedCorrectMove = ""
    let mutable firstFailedEngineMove = ""

    for cmd in puzzle.Commands do
        let! (mv, allNNValuesRaw) = agent.PostAndAsyncReply(fun ch -> BestMoveWithAllPolicies(cmd, ch))
        let allNNValues = normalizeCastlingAliases cmd.CorrectMove allNNValuesRaw
        let kld = computeKLD allNNValues cmd.CorrectMove
        let ml = computeMarginLoss allNNValues cmd.CorrectMove
        if ml > maxMarginLoss then maxMarginLoss <- ml
        // Hardest position of the puzzle's move sequence, consistent with maxKLD.
        let estN = computeEstNodesToFind allNNValues cmd.CorrectMove
        if estN > maxEstNodes then maxEstNodes <- estN
        if kld > maxKLD then
            maxKLD <- kld
            engineRankAtMaxKld <- findEngineRank allNNValues cmd.CorrectMove
        // Value loss: only for commands where the engine played the correct move (top-1)
        let wasSolvedThisCmd = allNNValues.Length > 0 && allNNValues.[0].LANMove = cmd.CorrectMove
        let vl = computeValueLoss allNNValues cmd.CorrectMove puzzle.Themes wasSolvedThisCmd
        if vl >= 0.0 then valueLosses.Add(vl)

        // Same rule as the value path: position counters are gated on the flag, the
        // first-move counter is not. This path queries every position either way, so the
        // flag changes only what is REPORTED, never what is measured.
        if scoreAllPositions then posScored <- posScored + 1
        isFirstPosition <- positionIndex = 0
        positionIndex <- positionIndex + 1
        if isFirstPosition then firstMoveScored <- 1
        // Check each threshold. The per-position tally is taken for EVERY position; the
        // per-puzzle flag still latches at the first mistake and is never revived.
        for n in topNs do
            let topNMoves = allNNValues |> List.truncate n |> List.map (fun v -> v.LANMove)
            let mutable solved = topNMoves |> List.contains cmd.CorrectMove
            if not solved then
              // Mate fallback. IsMate, not AnyLegalMove - the latter also fires on
              // STALEMATE and would credit a move that throws the win away.
              if n = maxTopN then
                let board = Board()
                board.PlayCommands cmd.Command
                board.PlayUciMove mv
                solved <- board.IsMate()
              // If mate fallback passed for maxTopN, it passes for all
            if solved then
              if scoreAllPositions then posCorrect.[n] <- posCorrect.[n] + 1
              if isFirstPosition then firstMoveCorrect.[n] <- firstMoveCorrect.[n] + 1
            if correct.[n] then
              correct.[n] <- solved
              // Guard on CorrectMove (always non-empty) so we capture the genuine FIRST top-1
              // failure even when the engine returned an empty bestmove (mv = "").
              if n = 1 && not solved && firstFailedCorrectMove = "" then
                  firstFailedCorrectMove <- cmd.CorrectMove
                  firstFailedEngineMove <- mv

    let avgValueLoss = if valueLosses.Count > 0 then valueLosses |> Seq.average else -1.0
    let updatedPuzzle =
        if firstFailedCorrectMove = "" then puzzle
        else
            // Sentinel "0000" when the engine returned an empty bestmove so the
            // failing command is still identifiable downstream.
            let stamped =
                if System.String.IsNullOrEmpty firstFailedEngineMove then PuzzleDataUtils.NullBestmove
                else firstFailedEngineMove
            let cmds =
                puzzle.Commands
                |> Seq.map (fun el ->
                    if el.CorrectMove = firstFailedCorrectMove then { el with MovePlayed = stamped }
                    else el)
                |> Seq.toList
            { puzzle with Commands = cmds }
    return (updatedPuzzle, maxKLD, maxMarginLoss, engineRankAtMaxKld, avgValueLoss, maxEstNodes,
            correct |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
            posCorrect |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
            posScored,
            firstMoveCorrect |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
            firstMoveScored)
  }

/// Per-puzzle multi-topN value workflow: one per-child evaluation, check all thresholds.
/// Returns (puzzle, correctPerTopN: Map<int, bool>)
let runPuzzleViaAgentValueMultiTopN (agent:MailboxProcessor<EngineMsg>) (topNs:int list) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let maxTopN = topNs |> List.max
    let correct = System.Collections.Generic.Dictionary<int, bool>()
    for n in topNs do correct.[n] <- true
    // The puzzle's FIRST solver move, which is the one its themes describe. Tracked here
    // so this builder produces the same theme-scoring rule as the others - which builder
    // happens to win a slice must not decide how themes are scored.
    let firstMoveCorrect = System.Collections.Generic.Dictionary<int, int>()
    for n in topNs do firstMoveCorrect.[n] <- 0
    let mutable positionIndex = 0

    for cmd in puzzle.Commands do
        let! moveVals = agent.PostAndAsyncReply(fun ch -> EvalAllMovesValue(cmd, ch))
        // Sort by V ascending (lower V from opponent = better for us)
        let sortedByV = moveVals |> List.sortBy snd
        let isFirstPosition = positionIndex = 0
        positionIndex <- positionIndex + 1

        for n in topNs do
            let topNMoves = sortedByV |> List.truncate n |> List.map fst
            let mutable solved = topNMoves |> List.contains cmd.CorrectMove
            if not solved && n = maxTopN then
              // Mate fallback. IsMate, not AnyLegalMove - the latter also fires on stalemate.
              let bestMove = if sortedByV.IsEmpty then "" else sortedByV.Head |> fst
              let board = Board()
              board.PlayCommands cmd.Command
              board.PlayUciMove bestMove
              solved <- board.IsMate()
            if solved && isFirstPosition then firstMoveCorrect.[n] <- firstMoveCorrect.[n] + 1
            if correct.[n] then correct.[n] <- solved

    return (puzzle,
            correct |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
            firstMoveCorrect |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
            (if positionIndex > 0 then 1 else 0))
  }

/// Per-puzzle Value TopN workflow: evaluate all legal moves with go nodes 1 per child,
/// check if correct move is in top N by value head (V).
let runPuzzleViaAgentValueTopN (agent:MailboxProcessor<EngineMsg>) (topN:int) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let board = Board()
    let mutable correct    = true
    let mutable movePlayed = ""
    let mutable failedMove = ""
    let mutable policy = String.Empty

    for cmd in puzzle.Commands do
        let! moveVals = agent.PostAndAsyncReply(fun ch -> EvalAllMovesValue(cmd, ch))
        // Sort by V ascending (V is from child/opponent perspective, so lower V = better for us)
        let sortedByV = moveVals |> List.sortBy snd
        let topNMoves = sortedByV |> List.truncate topN |> List.map fst

        if correct then
          // Best move by value head = lowest V (best for us)
          movePlayed <- if sortedByV.IsEmpty then "" else sortedByV.Head |> fst
          let mutable solved = topNMoves |> List.contains cmd.CorrectMove

          if not solved then
            failedMove <- cmd.CorrectMove
            let rank =
                sortedByV
                |> List.tryFindIndex (fun (m, _) -> m = cmd.CorrectMove)
                |> Option.map (fun i -> i + 1)
            let correctV = moveVals |> List.tryFind (fun (m, _) -> m = cmd.CorrectMove)
            policy <-
                match rank, correctV with
                | Some r, Some (_, v) -> sprintf "V=%.4f, rank #%d of top-%d" v r topN
                | None, _ -> sprintf "not evaluated, top-%d" topN
                | _ -> ""

            // Mate fallback
            board.PlayCommands cmd.Command
            board.PlayUciMove movePlayed
            solved <- not (board.AnyLegalMove())

          correct <- solved

    // Sentinel: when the engine returned an empty bestmove for a failed puzzle
    // (e.g. the agent's exception handler fell back to ""), stamp "0000" so the
    // EPD writer and visualizer can still identify the failing command instead
    // of silently dropping the puzzle.
    let stampedMovePlayed =
        if not correct && System.String.IsNullOrEmpty movePlayed then PuzzleDataUtils.NullBestmove
        else movePlayed
    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = stampedMovePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0}

    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = false
        Policy = policy
        PositionsCorrect = 0
        PositionsScored = 0
        FirstMoveCorrect = 0
        FirstMoveScored = 0
        KLD = 0.0
        EngineRank = 0
        MarginLoss = 0.0
        ValueLoss = 0.0
      }
  }

/// Per-puzzle solve workflow: search from first position, verify full PV
let runSolvePuzzleViaAgent (agent:MailboxProcessor<EngineMsg>) (puzzle:CsvPuzzleData) = async {
    // Reset engine state between puzzles
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let commands = puzzle.Commands |> Seq.toArray
    if commands.Length = 0 then
        return
          { PuzzleData = puzzle
            WasCorrect = false
            MovePlayed = ""
            FailedMove = ""
            ValueHead = false
            Policy = ""
            PositionsCorrect = 0
            PositionsScored = 0
            FirstMoveCorrect = 0
            FirstMoveScored = 0
            KLD = 0.0
            EngineRank = 0
            MarginLoss = 0.0
            ValueLoss = 0.0 }
    else
        // Send SolvePuzzle with the first position
        let! (bestmove, pvString, nnValues) = agent.PostAndAsyncReply(fun ch -> SolvePuzzle(commands.[0].Command, ch))
        // Ensure PV starts with bestmove. If not, the PV is from a stale
        // search iteration with a different first move — discard it.
        let pvMoves =
            let raw = if String.IsNullOrWhiteSpace pvString then [||] else pvString.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            if raw.Length > 0 && raw.[0] = bestmove then raw
            elif bestmove.Length >= 4 then [|bestmove|]
            else raw

        // Track the actual PV position on a board so mate checks use the real
        // position (opponent responses in the PV may differ from the puzzle path).
        let board = Board()
        board.PlayCommands commands.[0].Command // set up the initial puzzle position

        // Extract our moves from PV at indices 0, 2, 4...
        // We need pvMoves.Length >= 2 * commands.Length - 1 to cover all our moves
        let mutable correct = true
        let mutable movePlayed = bestmove
        let mutable failedMove = ""
        let mutable failedAtIndex = -1
        // Track how many PV moves we've replayed on the board (for advancing position)
        let mutable pvMovesPlayed = 0
        // When the opponent's PV response diverges from the puzzle path, subsequent
        // PV moves are from a different game line. The engine's own moves were all
        // correct — count as solved and stop checking further commands.
        let mutable pvDiverged = false

        // Puzzle moves: [setup, solver1, opponent1, solver2, opponent2, ...]
        // PV moves:     [solver1, opponent1_pv, solver2_pv, ...]
        // Compare opponent moves pairwise: puzzleMoves[2*i] vs pvMoves[2*i - 1]
        let puzzleMoves =
            if String.IsNullOrWhiteSpace puzzle.Moves then [||]
            else puzzle.Moves.Split(' ', StringSplitOptions.RemoveEmptyEntries)

        let opponentDiverged i =
            if i > 0 then
                let pvOpponentIdx = i * 2 - 1       // odd indices in PV: 1, 3, 5, ...
                let puzzleOpponentIdx = i * 2       // indices 2, 4, 6, ... in full puzzle moves
                if pvOpponentIdx < pvMoves.Length && puzzleOpponentIdx < puzzleMoves.Length then
                    pvMoves.[pvOpponentIdx] <> puzzleMoves.[puzzleOpponentIdx]
                else false
            else false

        for i in 0 .. commands.Length - 1 do
            if correct && not pvDiverged then
                // Check opponent divergence BEFORE comparing the engine's move.
                // If the opponent played a different response in the PV than the
                // puzzle expects, the board position is different — even if the
                // engine's move text coincidentally matches, it's a different line.
                // All prior engine moves were correct; accept as solved.
                if opponentDiverged i then
                    pvDiverged <- true
                else
                    let pvIndex = i * 2 // our moves are at even indices in PV
                    if pvIndex >= pvMoves.Length then
                        // PV too short to cover this move
                        if i > 0 then
                            // All engine moves verified so far were correct. The PV
                            // doesn't cover remaining moves (truncated or diverged).
                            pvDiverged <- true
                        else
                            correct <- false
                            failedMove <- commands.[i].CorrectMove
                            movePlayed <- bestmove
                            failedAtIndex <- i
                    else
                        let pvMove = pvMoves.[pvIndex]

                        let mutable solved = commands.[i].CorrectMove = pvMove
                        if not solved then
                            // Mate fallback: if this PV move delivers checkmate, accept it.
                            // A mate is a mate — the end position can have more than one
                            // mating move, so accept any move that checkmates.
                            // Use the actual PV-tracked board position (not the puzzle's
                            // expected path) since opponent responses may differ in the PV.
                            try
                                board.PlayUciMove pvMove
                                solved <- not (board.AnyLegalMove())
                                if solved then
                                    pvMovesPlayed <- pvIndex + 1
                                else
                                    board.UndoMove()
                            with _ -> ()
                        if solved then
                            // Advance the board through PV moves we haven't replayed yet
                            // (our move at pvIndex, plus opponent response at pvIndex+1)
                            while pvMovesPlayed <= pvIndex && pvMovesPlayed < pvMoves.Length do
                                try board.PlayUciMove pvMoves.[pvMovesPlayed] with _ -> ()
                                pvMovesPlayed <- pvMovesPlayed + 1
                            // Also play opponent's response if available
                            if pvMovesPlayed = pvIndex + 1 && pvIndex + 1 < pvMoves.Length then
                                try board.PlayUciMove pvMoves.[pvIndex + 1] with _ -> ()
                                pvMovesPlayed <- pvIndex + 2
                        else
                            correct <- false
                            failedMove <- commands.[i].CorrectMove
                            movePlayed <- pvMove
                            failedAtIndex <- i

        // Sentinel: when the engine returned an empty bestmove for a failed puzzle
        // (e.g. the agent's exception handler fell back to ""), stamp "0000" so the
        // EPD writer and visualizer can still identify the failing command instead
        // of silently dropping the puzzle.
        let stampedMovePlayed =
            if not correct && System.String.IsNullOrEmpty movePlayed then PuzzleDataUtils.NullBestmove
            else movePlayed
        let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = stampedMovePlayed} else el) |> Seq.toList
        let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }

        // When the failure is at a later position (i > 0), the initial NNValues
        // are from the root and won't contain the failed move. Do a follow-up
        // search at the failed position to get its NNValues.
        let! policyNNValues =
            if not correct && failedAtIndex > 0 && failedAtIndex < commands.Length then
                async {
                    let! (_, _, laterNN) = agent.PostAndAsyncReply(fun ch -> SolvePuzzle(commands.[failedAtIndex].Command, ch))
                    return laterNN
                }
            else async { return nnValues }

        // Build policy string: for failures with NNValues, show P% and rank;
        // for solved puzzles or engines without NNValues, keep the PV string.
        let policyString =
            if not correct && policyNNValues.Count > 0 then
                let sorted = policyNNValues |> Seq.sortByDescending (fun v -> v.P) |> Seq.toArray
                let correctP = sorted |> Array.tryFind (fun v -> v.LANMove = failedMove)
                let playedP  = sorted |> Array.tryFind (fun v -> v.LANMove = movePlayed)
                match correctP, playedP with
                | Some cp, Some pp ->
                    let rank = (sorted |> Array.findIndex (fun v -> v.LANMove = failedMove)) + 1
                    sprintf "%.2f (#%d), %.2f Q=%.2f" cp.P rank pp.P pp.Q
                | Some cp, None ->
                    let rank = (sorted |> Array.findIndex (fun v -> v.LANMove = failedMove)) + 1
                    sprintf "%.2f (#%d), " cp.P rank
                | None, Some pp ->
                    sprintf ", %.2f Q=%.2f" pp.P pp.Q
                | None, None -> ""
            else pvString

        return
          { PuzzleData = puzzleWithMove
            WasCorrect = correct
            MovePlayed = movePlayed
            FailedMove = failedMove
            ValueHead = false
            Policy = policyString
            PositionsCorrect = 0
            PositionsScored = 0
            FirstMoveCorrect = 0
            FirstMoveScored = 0
            KLD = 0.0
            EngineRank = 0
            MarginLoss = 0.0
            ValueLoss = 0.0 }
  }

/// Shutdown agents safely, swallowing any exceptions
let private shutdownAgents (agents: MailboxProcessor<EngineMsg>[]) =
    for agent in agents do
        try
            agent.PostAndAsyncReply(fun ch -> Quit ch)
            |> fun a -> Async.RunSynchronously(a, timeout = 10000)
        with _ -> ()

//Main test runner
let performValueNetworkTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency: int)
  (scoreAllPositions: bool)
  (onProgress: int -> unit)
  (ct: CancellationToken) =

    let concurrencyLevel = max 1 concurrency

    let agents =
        [| for _ in 1 .. concurrencyLevel do
            startValueEngineAgent engineCfg |]

    try
        let ok = agents |> Array.map (fun a -> a.PostAndAsyncReply(fun ch -> Ok ch)) |> Async.Parallel |> Async.RunSynchronously
        if ok |> Array.exists (fun x -> not x) then
            Score.empty
        else
            printfn "Starting value network test with %d concurrent agents..." concurrencyLevel

            // Channel-based work distribution for natural load balancing
            let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
            for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
            puzzleCh.Writer.Complete()

            let mutable processedCount = 0
            let total = puzzles.Length
            let resultsBag = ConcurrentBag<PuzzleResult>()
            let worker (agent: MailboxProcessor<EngineMsg>) = async {
                let mutable keepGoing = true
                while keepGoing && not ct.IsCancellationRequested do
                    let ok, puzzle = puzzleCh.Reader.TryRead()
                    if ok then
                        let! result = runPuzzleViaAgentEx agent true scoreAllPositions puzzle
                        resultsBag.Add(result)
                        let count = Interlocked.Increment(&processedCount)
                        if count % 10 = 0 || count = total then onProgress count
                    else
                        keepGoing <- false
            }

            [| for agent in agents -> worker agent |]
            |> Async.Parallel
            |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
            |> ignore

            let results = resultsBag.ToArray()

            let networkName =
                let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
                if not (String.IsNullOrEmpty engineNet) then engineNet
                elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
                else ""

            //Aggregate results
            let correct = results |> Array.filter (fun r -> r.WasCorrect)
            let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
            let w, d, l = correct.Length, 0, failed.Length

            let diffElo = EloCalculator.eloDiffWDL w d l
            let error   = EloCalculator.calculateEloError w d l
            let avg     =
              if results.Length = 0 then 0.0
              else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
            let perf    = avg + diffElo
            let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
            printfn "\nValue network rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s" perf avg diffElo theme
            let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}
            // Per-position totals are 0 unless ScoreAllPositions was set; the first-move
            // totals are always real, since that position is queried either way.
            let posCorrectTotal = results |> Array.sumBy (fun r -> r.PositionsCorrect)
            let posScoredTotal = results |> Array.sumBy (fun r -> r.PositionsScored)
            let firstMoveCorrectTotal = results |> Array.sumBy (fun r -> r.FirstMoveCorrect)
            let firstMoveScoredTotal = results |> Array.sumBy (fun r -> r.FirstMoveScored)
            let firstMoveIds =
                Collections.Generic.HashSet<int>(
                    results |> Seq.filter (fun r -> r.FirstMoveCorrect > 0)
                            |> Seq.map (fun r -> r.PuzzleData.PuzzleId))

            //Build and return the final score record
            {
              Engine = engineCfg.Name
              NeuralNet = networkName
              TotalNumber = results.Length
              Correct = w
              Wrong = l
              RatingAvg = avg
              Filter = if theme.Trim() = "" then "none" else theme.Trim()
              PlayerRecord = pRating
              FailedPuzzles = ResizeArray (failed  |> Array.map (fun r -> r.PuzzleData, r.Policy))
              CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
              Nodes = nodes
              WithHistory = false
              Type = "Value"
              AvgKLD = 0.0
              AvgRankWeightedKld = 0.0
              AvgFrontierKld = 0.0
              AvgMarginLoss = 0.0
              AvgValueLoss = 0.0
              AvgEstNodesLog10 = 0.0
              EstNodesP95 = 0.0
              EstNodesP99 = 0.0
              EstNodesCdf100 = 0.0
              HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
              PositionsCorrect = posCorrectTotal
              PositionsScored = posScoredTotal
              FirstMoveCorrect = firstMoveCorrectTotal
              FirstMoveScored = firstMoveScoredTotal
              FirstMoveCorrectIds = firstMoveIds
            }
    finally
        shutdownAgents agents

let performPolicyOrSearchTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (onProgress: int -> unit)
  (ct: CancellationToken) =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg nodes |]

    try
        printfn "Starting policy/search test with %d concurrent agents..." concurrency

        // Channel-based work distribution for natural load balancing
        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<PuzzleResult>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgentEx agent false false puzzle
                    resultsBag.Add(result)
                    let count = Interlocked.Increment(&processedCount)
                    if count % 10 = 0 || count = total then onProgress count
                else
                    keepGoing <- false
        }

        [| for agent in agents -> worker agent |]
        |> Async.Parallel
        |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
        |> ignore

        let results = resultsBag.ToArray()

        let networkName =
            let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
            if not (String.IsNullOrEmpty engineNet) then engineNet
            elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else ""

        //Aggregate results
        let correct = results |> Array.filter (fun r -> r.WasCorrect)
        let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
        let w, d, l = correct.Length, 0, failed.Length

        let diffElo = EloCalculator.eloDiffWDL w d l
        let error   = EloCalculator.calculateEloError w d l
        let avg     =
          if results.Length = 0 then 0.0
          else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
        let perf    = avg + diffElo
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
        if nodes > 1 then
          printfn "\nSearch rating performance: %.0f (avg %.0f + Δ%.0f) Nodes %d Theme: %s" perf avg diffElo nodes theme
        else
          printfn "\nPolicy network rating performance: %.0f (avg %.0f + Δ%.0f) Nodes %d Theme: %s" perf avg diffElo nodes theme
        let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}

        // Build and return the final score record
        {
          Engine = engineCfg.Name
          NeuralNet = networkName
          TotalNumber = results.Length
          Correct = w
          Wrong = l
          RatingAvg = avg
          Filter = if theme.Trim() = "" then "none" else theme.Trim()
          PlayerRecord = pRating
          FailedPuzzles = ResizeArray (failed  |> Array.map (fun r -> r.PuzzleData, r.Policy))
          CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
          Nodes = nodes
          WithHistory = false
          Type = if nodes > 1 then "Search" else "Policy"
          AvgKLD = 0.0
          AvgRankWeightedKld = 0.0
          AvgFrontierKld = 0.0
          AvgMarginLoss = 0.0
          AvgValueLoss = 0.0
          AvgEstNodesLog10 = 0.0
          EstNodesP95 = 0.0
          EstNodesP99 = 0.0
          EstNodesCdf100 = 0.0
          HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
          PositionsCorrect = results |> Array.sumBy (fun r -> r.PositionsCorrect)
          PositionsScored = results |> Array.sumBy (fun r -> r.PositionsScored)
          FirstMoveCorrect = results |> Array.sumBy (fun r -> r.FirstMoveCorrect)
          FirstMoveScored = results |> Array.sumBy (fun r -> r.FirstMoveScored)
          FirstMoveCorrectIds =
              Collections.Generic.HashSet<int>(
                  results |> Seq.filter (fun r -> r.FirstMoveCorrect > 0)
                          |> Seq.map (fun r -> r.PuzzleData.PuzzleId))
        }
    finally
        shutdownAgents agents


let performSolveTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (onProgress: int -> unit)
  (ct: CancellationToken) =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg nodes |]

    try
        printfn "Starting solve test with %d concurrent agents..." concurrency

        // Channel-based work distribution for natural load balancing
        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<PuzzleResult>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runSolvePuzzleViaAgent agent puzzle
                    resultsBag.Add(result)
                    let count = Interlocked.Increment(&processedCount)
                    if count % 10 = 0 || count = total then onProgress count
                else
                    keepGoing <- false
        }

        [| for agent in agents -> worker agent |]
        |> Async.Parallel
        |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
        |> ignore

        let results = resultsBag.ToArray()

        let networkName =
            let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
            if not (String.IsNullOrEmpty engineNet) then engineNet
            elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else ""

        //Aggregate results
        let correct = results |> Array.filter (fun r -> r.WasCorrect)
        let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
        let w, d, l = correct.Length, 0, failed.Length

        let diffElo = EloCalculator.eloDiffWDL w d l
        let error   = EloCalculator.calculateEloError w d l
        let avg     =
          if results.Length = 0 then 0.0
          else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
        let perf    = avg + diffElo
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
        printfn "\nSolve rating performance: %.0f (avg %.0f + Δ%.0f) Nodes %d Theme: %s" perf avg diffElo nodes theme
        let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}

        // Build and return the final score record
        {
          Engine = engineCfg.Name
          NeuralNet = networkName
          TotalNumber = results.Length
          Correct = w
          Wrong = l
          RatingAvg = avg
          Filter = if theme.Trim() = "" then "none" else theme.Trim()
          PlayerRecord = pRating
          FailedPuzzles = ResizeArray (failed  |> Array.map (fun r -> r.PuzzleData, r.Policy))
          CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
          Nodes = nodes
          WithHistory = false
          Type = "Solve"
          AvgKLD = 0.0
          AvgRankWeightedKld = 0.0
          AvgFrontierKld = 0.0
          AvgMarginLoss = 0.0
          AvgValueLoss = 0.0
          AvgEstNodesLog10 = 0.0
          EstNodesP95 = 0.0
          EstNodesP99 = 0.0
          EstNodesCdf100 = 0.0
          HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
          PositionsCorrect = results |> Array.sumBy (fun r -> r.PositionsCorrect)
          PositionsScored = results |> Array.sumBy (fun r -> r.PositionsScored)
          FirstMoveCorrect = results |> Array.sumBy (fun r -> r.FirstMoveCorrect)
          FirstMoveScored = results |> Array.sumBy (fun r -> r.FirstMoveScored)
          FirstMoveCorrectIds =
              Collections.Generic.HashSet<int>(
                  results |> Seq.filter (fun r -> r.FirstMoveCorrect > 0)
                          |> Seq.map (fun r -> r.PuzzleData.PuzzleId))
        }
    finally
        shutdownAgents agents


let performPolicyMultiTopNTest
  (topNs:int list)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (includeFailedPuzzles : bool)
  (scoreAllPositions : bool)
  (onProgress: int -> unit)
  (ct: CancellationToken) : Score list =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg 1 |]

    try
        let topNsStr = topNs |> List.map string |> String.concat ","
        printfn "Starting policy multi-topN [%s] test with %d concurrent agents..." topNsStr concurrency

        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<CsvPuzzleData * float * float * int * float * float * Map<int, bool> * Map<int, int> * int * Map<int, int> * int>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgentMultiTopN agent topNs scoreAllPositions puzzle
                    resultsBag.Add(result)
                    let count = Interlocked.Increment(&processedCount)
                    if count % 10 = 0 || count = total then onProgress count
                else
                    keepGoing <- false
        }

        [| for agent in agents -> worker agent |]
        |> Async.Parallel
        |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
        |> ignore

        let allResults = resultsBag.ToArray()

        let networkName =
            let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
            if not (String.IsNullOrEmpty engineNet) then engineNet
            elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else ""

        let avgRating =
          if allResults.Length = 0 then 0.0
          else allResults |> Array.averageBy (fun (p, _, _, _, _, _, _, _, _, _, _) -> p.Rating)
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme

        // Tail of the per-puzzle estimated nodes-to-find distribution (all puzzles,
        // raw node units). Independent of the topN threshold.
        let estNodesAll = allResults |> Array.map (fun (_, _, _, _, _, estN, _, _, _, _, _) -> estN)
        let estNodesP95 = percentile 95.0 estNodesAll
        let estNodesP99 = percentile 99.0 estNodesAll
        let estNodesCdf100 = fractionAtOrBelow 100.0 estNodesAll
        // Worst-case candidates by estimate, for targeted real-search verification.
        let hardestByEstNodes =
            allResults
            |> Array.map (fun (p, _, _, _, _, estN, _, _, _, _, _) -> p, estN)
            |> Array.sortByDescending snd
            |> Array.truncate 50

        // Produce one Score per topN threshold
        topNs |> List.map (fun topN ->
            let correct = allResults |> Array.filter (fun (_, _, _, _, _, _, m, _, _, _, _) -> m.[topN])
            let failed  = allResults |> Array.filter (fun (_, _, _, _, _, _, m, _, _, _, _) -> not m.[topN])
            let kldSource = if includeFailedPuzzles then allResults else correct
            let avgKLD =
              if kldSource.Length = 0 then 0.0
              else kldSource |> Array.averageBy (fun (_, kld, _, _, _, _, _, _, _, _, _) -> kld)
            let avgRankWeightedKld =
              computeRankWeightedKld (kldSource |> Seq.map (fun (_, kld, _, rank, _, _, _, _, _, _, _) -> kld, rank))
            // Frontier-weighted uses ALL puzzles (solved + failed) regardless of
            // includeFailedPuzzles — the frontier is defined by rank, not solved status.
            let avgFrontierKld =
              computeFrontierWeightedKld (allResults |> Seq.map (fun (_, kld, _, rank, _, _, _, _, _, _, _) -> kld, rank))
            // Margin loss uses all puzzles (solved + failed), weighted 2x on
            // unsolved. Solved status is per-puzzle top-1 correctness.
            let avgMarginLoss =
              computeWeightedMarginLoss
                (allResults |> Seq.map (fun (_, _, ml, _, _, _, correctPerTopN, _, _, _, _) ->
                    ml, (correctPerTopN |> Map.tryFind 1 |> Option.defaultValue false)))
                2.0
            // Positions this topN got right, over every position of every puzzle. Reported
            // alongside the per-puzzle Correct/TotalNumber, never instead of it.
            let posCorrectTotal =
                allResults |> Array.sumBy (fun (_, _, _, _, _, _, _, pc, _, _, _) ->
                    pc |> Map.tryFind topN |> Option.defaultValue 0)
            let posScoredTotal = allResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, ps, _, _) -> ps)
            // Themes describe the puzzle's FIRST solver move, so the theme breakdown wants
            // this rather than the whole-line verdict.
            let firstMoveCorrectTotal =
                allResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, _, fc, _) ->
                    fc |> Map.tryFind topN |> Option.defaultValue 0)
            let firstMoveScoredTotal = allResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, _, _, fs) -> fs)
            let firstMoveIds =
                Collections.Generic.HashSet<int>(
                    allResults
                    |> Seq.filter (fun (_, _, _, _, _, _, _, _, _, fc, _) ->
                        (fc |> Map.tryFind topN |> Option.defaultValue 0) > 0)
                    |> Seq.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p.PuzzleId))
            // Value loss: |Q - expected_Q| from puzzle themes, solved puzzles only (vl >= 0).
            let validValueLosses = allResults |> Array.choose (fun (_, _, _, _, vl, _, _, _, _, _, _) -> if vl >= 0.0 then Some vl else None)
            let avgValueLoss =
              if validValueLosses.Length = 0 then 0.0
              else validValueLosses |> Array.average
            // Estimated nodes-to-find aggregated in log space (raw N spans 0..millions).
            // ALWAYS uses all puzzles regardless of includeFailedPuzzles (like FrontierKLD
            // and MarginLoss): solved-at-top-1 puzzles are 0 by construction, so the
            // signal lives almost entirely in the failed ones.
            let avgEstNodesLog10 =
              if allResults.Length = 0 then 0.0
              else allResults |> Array.averageBy (fun (_, _, _, _, _, estN, _, _, _, _, _) -> log10 (1.0 + estN))
            let w, d, l = correct.Length, 0, failed.Length

            let diffElo = EloCalculator.eloDiffWDL w d l
            let error   = EloCalculator.calculateEloError w d l
            let perf    = avgRating + diffElo
            let typeLabel = if topN = 1 then "Policy" else sprintf "pTop%d" topN
            printfn "\n%s rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s  AvgKLD: %.4f  RankWtKLD: %.4f  FrontierKLD: %.4f" typeLabel perf avgRating diffElo theme avgKLD avgRankWeightedKld avgFrontierKld
            let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}

            {
              Engine = engineCfg.Name
              NeuralNet = networkName
              TotalNumber = allResults.Length
              Correct = w
              Wrong = l
              RatingAvg = avgRating
              Filter = if theme.Trim() = "" then "none" else theme.Trim()
              PlayerRecord = pRating
              FailedPuzzles = ResizeArray (failed |> Array.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p, ""))
              CorrectPuzzles = ResizeArray (correct |> Array.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p))
              Nodes = 1
              WithHistory = false
              Type = typeLabel
              AvgKLD = avgKLD
              AvgRankWeightedKld = avgRankWeightedKld
              AvgFrontierKld = avgFrontierKld
              AvgMarginLoss = avgMarginLoss
              AvgValueLoss = avgValueLoss
              AvgEstNodesLog10 = avgEstNodesLog10
              EstNodesP95 = estNodesP95
              EstNodesP99 = estNodesP99
              EstNodesCdf100 = estNodesCdf100
              HardestByEstNodes = ResizeArray hardestByEstNodes
              PositionsCorrect = posCorrectTotal
              PositionsScored = posScoredTotal
              FirstMoveCorrect = firstMoveCorrectTotal
              FirstMoveScored = firstMoveScoredTotal
              FirstMoveCorrectIds = firstMoveIds
            }
        )
    finally
        shutdownAgents agents


let performValueMultiTopNTest
  (topNs:int list)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (onProgress: int -> unit)
  (ct: CancellationToken) : Score list =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg 1 |]

    try
        let topNsStr = topNs |> List.map string |> String.concat ","
        printfn "Starting value multi-topN [%s] test with %d concurrent agents..." topNsStr concurrency

        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<CsvPuzzleData * Map<int, bool> * Map<int, int> * int>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgentValueMultiTopN agent topNs puzzle
                    resultsBag.Add(result)
                    let count = Interlocked.Increment(&processedCount)
                    if count % 10 = 0 || count = total then onProgress count
                else
                    keepGoing <- false
        }

        [| for agent in agents -> worker agent |]
        |> Async.Parallel
        |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
        |> ignore

        let allResults = resultsBag.ToArray()

        let networkName =
            let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
            if not (String.IsNullOrEmpty engineNet) then engineNet
            elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else ""

        let avgRating =
          if allResults.Length = 0 then 0.0
          else allResults |> Array.averageBy (fun (p, _, _, _) -> p.Rating)
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme

        topNs |> List.map (fun topN ->
            let correct = allResults |> Array.filter (fun (_, m, _, _) -> m.[topN])
            let failed  = allResults |> Array.filter (fun (_, m, _, _) -> not m.[topN])
            let w, d, l = correct.Length, 0, failed.Length

            let diffElo = EloCalculator.eloDiffWDL w d l
            let error   = EloCalculator.calculateEloError w d l
            let perf    = avgRating + diffElo
            let typeLabel = if topN = 1 then "Value" else sprintf "vTop%d" topN
            printfn "\n%s rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s" typeLabel perf avgRating diffElo theme
            let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}

            {
              Engine = engineCfg.Name
              NeuralNet = networkName
              TotalNumber = allResults.Length
              Correct = w
              Wrong = l
              RatingAvg = avgRating
              Filter = if theme.Trim() = "" then "none" else theme.Trim()
              PlayerRecord = pRating
              FailedPuzzles = ResizeArray (failed |> Array.map (fun (p, _, _, _) -> p, ""))
              CorrectPuzzles = ResizeArray (correct |> Array.map (fun (p, _, _, _) -> p))
              Nodes = 0
              WithHistory = false
              Type = typeLabel
              AvgKLD = 0.0
              AvgRankWeightedKld = 0.0
              AvgFrontierKld = 0.0
              AvgMarginLoss = 0.0
              AvgValueLoss = 0.0
              AvgEstNodesLog10 = 0.0
              EstNodesP95 = 0.0
              EstNodesP99 = 0.0
              EstNodesCdf100 = 0.0
              HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
              // this path evaluates every position by construction, so the position
              // counters would be meaningless here; only the first move is tracked
              PositionsCorrect = 0
              PositionsScored = 0
              FirstMoveCorrect = allResults |> Array.sumBy (fun (_, _, fc, _) -> fc |> Map.tryFind topN |> Option.defaultValue 0)
              FirstMoveScored = allResults |> Array.sumBy (fun (_, _, _, fs) -> fs)
              FirstMoveCorrectIds =
                  Collections.Generic.HashSet<int>(
                      allResults
                      |> Seq.filter (fun (_, _, fc, _) -> (fc |> Map.tryFind topN |> Option.defaultValue 0) > 0)
                      |> Seq.map (fun (p, _, _, _) -> p.PuzzleId))
            }
        )
    finally
        shutdownAgents agents


let performValueTopNTest
  (topN:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (onProgress: int -> unit)
  (ct: CancellationToken) =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg 1 |]

    try
        printfn "Starting value top-%d test (nodes=legal moves) with %d concurrent agents..." topN concurrency

        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<PuzzleResult>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgentValueTopN agent topN puzzle
                    resultsBag.Add(result)
                    let count = Interlocked.Increment(&processedCount)
                    if count % 10 = 0 || count = total then onProgress count
                else
                    keepGoing <- false
        }

        [| for agent in agents -> worker agent |]
        |> Async.Parallel
        |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
        |> ignore

        let results = resultsBag.ToArray()

        let networkName =
            let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
            if not (String.IsNullOrEmpty engineNet) then engineNet
            elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else ""

        let correct = results |> Array.filter (fun r -> r.WasCorrect)
        let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
        let w, d, l = correct.Length, 0, failed.Length

        let diffElo = EloCalculator.eloDiffWDL w d l
        let error   = EloCalculator.calculateEloError w d l
        let avg =
          if results.Length = 0 then 0.0
          else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
        let perf = avg + diffElo
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
        let typeLabel = if topN = 1 then "Value" else sprintf "vTop%d" topN
        printfn "\nValue top-%d rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s" topN perf avg diffElo theme
        let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}

        {
          Engine = engineCfg.Name
          NeuralNet = networkName
          TotalNumber = results.Length
          Correct = w
          Wrong = l
          RatingAvg = avg
          Filter = if theme.Trim() = "" then "none" else theme.Trim()
          PlayerRecord = pRating
          FailedPuzzles = ResizeArray (failed |> Array.map (fun r -> r.PuzzleData, r.Policy))
          CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
          Nodes = 0
          WithHistory = false
          Type = typeLabel
          AvgKLD = 0.0
          AvgRankWeightedKld = 0.0
          AvgFrontierKld = 0.0
          AvgMarginLoss = 0.0
          AvgValueLoss = 0.0
          AvgEstNodesLog10 = 0.0
          EstNodesP95 = 0.0
          EstNodesP99 = 0.0
          EstNodesCdf100 = 0.0
          HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
          PositionsCorrect = results |> Array.sumBy (fun r -> r.PositionsCorrect)
          PositionsScored = results |> Array.sumBy (fun r -> r.PositionsScored)
          FirstMoveCorrect = results |> Array.sumBy (fun r -> r.FirstMoveCorrect)
          FirstMoveScored = results |> Array.sumBy (fun r -> r.FirstMoveScored)
          FirstMoveCorrectIds =
              Collections.Generic.HashSet<int>(
                  results |> Seq.filter (fun r -> r.FirstMoveCorrect > 0)
                          |> Seq.map (fun r -> r.PuzzleData.PuzzleId))
        }
    finally
        shutdownAgents agents


// Per-puzzle value-head workflow using BestMoveValueHead (for combo agent)
let private runPuzzleViaAgentValueHead (agent:MailboxProcessor<EngineMsg>) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)
    let board = Board()
    let mutable correct    = true
    let mutable movePlayed = ""
    let mutable failedMove = ""

    for cmd in puzzle.Commands do
      if correct then
        let! mv = agent.PostAndAsyncReply(fun ch -> BestMoveValueHead(cmd, ch))
        movePlayed <- mv
        let mutable solved = cmd.CorrectMove = mv
        if not solved then
          // IsMate, not AnyLegalMove: the latter also fires on stalemate.
          board.PlayCommands cmd.Command
          board.PlayUciMove mv
          solved <- board.IsMate()
        // After the fallback, so a position the fallback RESCUED is not recorded as the
        // failure. Matches runPuzzleViaAgentEx; these two loops used to disagree.
        if not solved then failedMove <- cmd.CorrectMove
        correct <- solved

    // Sentinel: when the engine returned an empty bestmove for a failed puzzle
    // (e.g. the agent's exception handler fell back to ""), stamp "0000" so the
    // EPD writer and visualizer can still identify the failing command instead
    // of silently dropping the puzzle.
    let stampedMovePlayed =
        if not correct && System.String.IsNullOrEmpty movePlayed then PuzzleDataUtils.NullBestmove
        else movePlayed
    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = stampedMovePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }
    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = true
        Policy = ""
        PositionsCorrect = 0
        PositionsScored = 0
        FirstMoveCorrect = 0
        FirstMoveScored = 0
        KLD = 0.0
        EngineRank = 0
        MarginLoss = 0.0
        ValueLoss = 0.0
      }
  }

// Combo test: runs policy AND value evaluation on the SAME engine instance.
// Creates one set of policy agents, runs policy evaluation first, then value
// evaluation second, and shuts down only once. Saves engine init overhead
// compared to running performPolicyMultiTopNTest + performValueNetworkTest
// which each create separate engine instances.
let performPolicyValueTest
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
  (includeFailedPuzzles : bool)
  (scoreAllPositions : bool)
  (onProgress: int -> unit)
  (ct: CancellationToken) : Score list =

    let concurrency = max 1 concurrency
    let agents =
        [| for _ in 1 .. concurrency do
            startPolicyEngineAgent engineCfg 1 |]

    try
        let ok = agents |> Array.map (fun a -> a.PostAndAsyncReply(fun ch -> Ok ch)) |> Async.Parallel |> Async.RunSynchronously
        if ok |> Array.exists (fun x -> not x) then
            []
        else
            let networkName =
                let engineNet = agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
                if not (String.IsNullOrEmpty engineNet) then engineNet
                elif not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
                else ""

            let total = puzzles.Length
            let theme = if String.IsNullOrWhiteSpace theme then "none" else theme

            // ---- Pass 1: Policy evaluation ----
            printfn "Starting combo policy+value test with %d concurrent agents..." concurrency
            printfn "  Pass 1/2: Policy evaluation..."
            let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
            for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
            puzzleCh.Writer.Complete()

            let mutable processedCount = 0
            let policyResultsBag = ConcurrentBag<CsvPuzzleData * float * float * int * float * float * Map<int, bool> * Map<int, int> * int * Map<int, int> * int>()
            let policyWorker (agent: MailboxProcessor<EngineMsg>) = async {
                let mutable keepGoing = true
                while keepGoing && not ct.IsCancellationRequested do
                    let ok, puzzle = puzzleCh.Reader.TryRead()
                    if ok then
                        let! result = runPuzzleViaAgentMultiTopN agent [1] scoreAllPositions puzzle
                        policyResultsBag.Add(result)
                        let count = Interlocked.Increment(&processedCount)
                        if count % 10 = 0 || count = total then onProgress count
                    else
                        keepGoing <- false
            }

            [| for agent in agents -> policyWorker agent |]
            |> Async.Parallel
            |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
            |> ignore

            let policyResults = policyResultsBag.ToArray()

            // Build Policy Score
            let avgRating =
              if policyResults.Length = 0 then 0.0
              else policyResults |> Array.averageBy (fun (p, _, _, _, _, _, _, _, _, _, _) -> p.Rating)

            let correct = policyResults |> Array.filter (fun (_, _, _, _, _, _, m, _, _, _, _) -> m.[1])
            let failed  = policyResults |> Array.filter (fun (_, _, _, _, _, _, m, _, _, _, _) -> not m.[1])
            let kldSource = if includeFailedPuzzles then policyResults else correct
            let avgKLD =
              if kldSource.Length = 0 then 0.0
              else kldSource |> Array.averageBy (fun (_, kld, _, _, _, _, _, _, _, _, _) -> kld)
            let avgRankWeightedKld =
              computeRankWeightedKld (kldSource |> Seq.map (fun (_, kld, _, rank, _, _, _, _, _, _, _) -> kld, rank))
            let avgFrontierKld =
              computeFrontierWeightedKld (policyResults |> Seq.map (fun (_, kld, _, rank, _, _, _, _, _, _, _) -> kld, rank))
            let avgMarginLoss =
              computeWeightedMarginLoss
                (policyResults |> Seq.map (fun (_, _, ml, _, _, _, correctPerTopN, _, _, _, _) ->
                    ml, (correctPerTopN |> Map.tryFind 1 |> Option.defaultValue false)))
                2.0
            // Per-position tally; this path is top-1 only, so topN is 1.
            let posCorrectTotal =
                policyResults |> Array.sumBy (fun (_, _, _, _, _, _, _, pc, _, _, _) ->
                    pc |> Map.tryFind 1 |> Option.defaultValue 0)
            let posScoredTotal = policyResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, ps, _, _) -> ps)
            let firstMoveCorrectTotal =
                policyResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, _, fc, _) ->
                    fc |> Map.tryFind 1 |> Option.defaultValue 0)
            let firstMoveScoredTotal = policyResults |> Array.sumBy (fun (_, _, _, _, _, _, _, _, _, _, fs) -> fs)
            let firstMoveIds =
                Collections.Generic.HashSet<int>(
                    policyResults
                    |> Seq.filter (fun (_, _, _, _, _, _, _, _, _, fc, _) ->
                        (fc |> Map.tryFind 1 |> Option.defaultValue 0) > 0)
                    |> Seq.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p.PuzzleId))
            let validValueLosses = policyResults |> Array.choose (fun (_, _, _, _, vl, _, _, _, _, _, _) -> if vl >= 0.0 then Some vl else None)
            let avgValueLoss =
              if validValueLosses.Length = 0 then 0.0
              else validValueLosses |> Array.average
            // All puzzles, like FrontierKLD/MarginLoss — see performPolicyMultiTopNTest.
            let avgEstNodesLog10 =
              if policyResults.Length = 0 then 0.0
              else policyResults |> Array.averageBy (fun (_, _, _, _, _, estN, _, _, _, _, _) -> log10 (1.0 + estN))
            let estNodesAll = policyResults |> Array.map (fun (_, _, _, _, _, estN, _, _, _, _, _) -> estN)
            let estNodesP95 = percentile 95.0 estNodesAll
            let estNodesP99 = percentile 99.0 estNodesAll
            let estNodesCdf100 = fractionAtOrBelow 100.0 estNodesAll
            let hardestByEstNodes =
                policyResults
                |> Array.map (fun (p, _, _, _, _, estN, _, _, _, _, _) -> p, estN)
                |> Array.sortByDescending snd
                |> Array.truncate 50
            let pw, pd, pl = correct.Length, 0, failed.Length
            let diffElo = EloCalculator.eloDiffWDL pw pd pl
            let error   = EloCalculator.calculateEloError pw pd pl
            let perf    = avgRating + diffElo
            printfn "\nPolicy rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s  AvgKLD: %.4f" perf avgRating diffElo theme avgKLD
            let policyScore =
                {
                  Engine = engineCfg.Name
                  NeuralNet = networkName
                  TotalNumber = policyResults.Length
                  Correct = pw
                  Wrong = pl
                  RatingAvg = avgRating
                  Filter = if theme.Trim() = "" then "none" else theme.Trim()
                  PlayerRecord = {Rating = perf; Deviation = error; Volatility = 0.0}
                  FailedPuzzles = ResizeArray (failed |> Array.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p, ""))
                  CorrectPuzzles = ResizeArray (correct |> Array.map (fun (p, _, _, _, _, _, _, _, _, _, _) -> p))
                  Nodes = 1
                  WithHistory = false
                  Type = "Policy"
                  AvgKLD = avgKLD
                  AvgRankWeightedKld = avgRankWeightedKld
                  AvgFrontierKld = avgFrontierKld
                  AvgMarginLoss = avgMarginLoss
                  AvgValueLoss = avgValueLoss
                  AvgEstNodesLog10 = avgEstNodesLog10
                  EstNodesP95 = estNodesP95
                  EstNodesP99 = estNodesP99
                  EstNodesCdf100 = estNodesCdf100
                  HardestByEstNodes = ResizeArray hardestByEstNodes
                  PositionsCorrect = posCorrectTotal
                  PositionsScored = posScoredTotal
                  FirstMoveCorrect = firstMoveCorrectTotal
                  FirstMoveScored = firstMoveScoredTotal
                  FirstMoveCorrectIds = firstMoveIds
                }

            // ---- Pass 2: Value evaluation (same agents, same engine) ----
            if ct.IsCancellationRequested then [policyScore]
            else
                printfn "  Pass 2/2: Value evaluation..."
                // Reset agents for value pass
                for agent in agents do
                    agent.PostAndAsyncReply(fun ch -> NewGame ch) |> Async.RunSynchronously

                let puzzleCh2 = Channel.CreateUnbounded<CsvPuzzleData>()
                for p in puzzles do puzzleCh2.Writer.TryWrite(p) |> ignore
                puzzleCh2.Writer.Complete()

                processedCount <- 0
                let valueResultsBag = ConcurrentBag<PuzzleResult>()
                let valueWorker (agent: MailboxProcessor<EngineMsg>) = async {
                    let mutable keepGoing = true
                    while keepGoing && not ct.IsCancellationRequested do
                        let ok, puzzle = puzzleCh2.Reader.TryRead()
                        if ok then
                            let! result = runPuzzleViaAgentValueHead agent puzzle
                            valueResultsBag.Add(result)
                            let count = Interlocked.Increment(&processedCount)
                            if count % 10 = 0 || count = total then onProgress count
                        else
                            keepGoing <- false
                }

                [| for agent in agents -> valueWorker agent |]
                |> Async.Parallel
                |> fun a -> Async.RunSynchronously(a, cancellationToken = ct)
                |> ignore

                let valueResults = valueResultsBag.ToArray()
                let vCorrect = valueResults |> Array.filter (fun r -> r.WasCorrect)
                let vFailed  = valueResults |> Array.filter (fun r -> not r.WasCorrect)
                let vw, vd, vl = vCorrect.Length, 0, vFailed.Length
                let vDiffElo = EloCalculator.eloDiffWDL vw vd vl
                let vError   = EloCalculator.calculateEloError vw vd vl
                let vAvg =
                  if valueResults.Length = 0 then 0.0
                  else valueResults |> Array.averageBy (fun r -> r.PuzzleData.Rating)
                let vPerf = vAvg + vDiffElo
                printfn "\nValue rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s" vPerf vAvg vDiffElo theme
                let valueScore =
                    {
                      Engine = engineCfg.Name
                      NeuralNet = networkName
                      TotalNumber = valueResults.Length
                      Correct = vw
                      Wrong = vl
                      RatingAvg = vAvg
                      Filter = if theme.Trim() = "" then "none" else theme.Trim()
                      PlayerRecord = {Rating = vPerf; Deviation = vError; Volatility = 0.0}
                      FailedPuzzles = ResizeArray (vFailed |> Array.map (fun r -> r.PuzzleData, r.Policy))
                      CorrectPuzzles = ResizeArray (vCorrect |> Array.map (fun r -> r.PuzzleData))
                      Nodes = 1
                      WithHistory = false
                      Type = "Value"
                      AvgKLD = 0.0
                      AvgRankWeightedKld = 0.0
                      AvgFrontierKld = 0.0
                      AvgMarginLoss = 0.0
                      AvgValueLoss = 0.0
                      AvgEstNodesLog10 = 0.0
                      EstNodesP95 = 0.0
                      EstNodesP99 = 0.0
                      EstNodesCdf100 = 0.0
                      HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
                      PositionsCorrect = 0
                      PositionsScored = 0
                      FirstMoveCorrect = 0
                      FirstMoveScored = 0
                      FirstMoveCorrectIds = Collections.Generic.HashSet<int>()
                    }

                [policyScore; valueScore]
    finally
        shutdownAgents agents


type SubTest =
    | Value
    | Policy
    | PolicyValue
    | PolicyTopN of n:int
    | ValueTopN of n:int
    | Search of node:int
    | Solve of node:int

let runTest
      (input     : PuzzleInput)
      (callback  : Action<Lichess>)
      (toRun     : SubTest list)
      (ct        : CancellationToken)
    : ResizeArray<Score> =

    // common setup
    let enginesOnly = input.engines |> Seq.map fst
    ChessLibrary.Configuration.Validation.validateAllEnginesAndSomeSettings enginesOnly
    let ratings = PuzzleDataUtils.parseRatingGroups input.ratingGroups input.maxRating
    let themes  = PuzzleDataUtils.parseThemes input.puzzleFilter

    let sendU = callback.Invoke
    let results = ResizeArray<Score>()
    let start   = System.Diagnostics.Stopwatch.GetTimestamp()
    //check if toRun contains Search or Solve
    let hasSearch = toRun |> List.exists (function Search _ -> true | _ -> false)
    let hasSolve  = toRun |> List.exists (function Solve _  -> true | _ -> false)
    // Collect all policy topN values for merged execution
    let policyTopNs =
        toRun |> List.collect (function
            | Policy -> [1]
            | PolicyTopN n -> [n]
            | _ -> [])
        |> List.distinct |> List.sort
    // Collect all value topN values for merged execution
    let valueTopNs =
        toRun |> List.collect (function
            | ValueTopN n -> [n]
            | _ -> [])
        |> List.distinct |> List.sort

    for engine, nodes in input.engines do
      if ct.IsCancellationRequested then () else
      let hasLiveStats =
        engine.Options.ContainsKey("LogLiveStats")
        || engine.Options.ContainsKey("VerboseMoveStats")

      for theme in themes do
        if ct.IsCancellationRequested then () else
        let themeLabel = if System.String.IsNullOrWhiteSpace theme then "none" else theme

        for rating in ratings do
          if ct.IsCancellationRequested then () else
          // load & log
          let puzzles = PuzzleDataUtils.sortPuzzleData theme rating input
          if puzzles.Length = 0 then
              ChessLibrary.RuntimeUtilities.ConsoleUtils.yellowConsole $"\nSkipping tests: No puzzles found matching theme '{theme}' and rating {rating}"
          else
            let avg = puzzles |> Array.averageBy (fun p -> p.Rating)
            printfn "\nRating group %d (avg %.0f), theme \"%s\"" rating avg themeLabel

            let total = puzzles.Length
            let mkProgress testType processed = sendU (Progress(processed, total, $"{engine.Name} — {testType}"))

            try
              if hasSearch && nodes > 0 then
                let score = performPolicyOrSearchTest nodes engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress "Search") ct
                sendU (PuzzleResult score)
                results.Add score
              if hasSolve && nodes > 0 then
                let score = performSolveTest nodes engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress "Solve") ct
                sendU (PuzzleResult score)
                results.Add score
              // Run merged policy topN tests (single pass over puzzles)
              if policyTopNs.Length > 0 && hasLiveStats && not ct.IsCancellationRequested then
                let topNLabel = policyTopNs |> List.map string |> String.concat "," |> sprintf "Policy [%s]"
                let scores = performPolicyMultiTopNTest policyTopNs engine puzzles theme input.NumberOfPuzzlesInParallel input.IncludeFailedPuzzles input.ScoreAllPositions (mkProgress topNLabel) ct
                for score in scores do
                  sendU (PuzzleResult score)
                  results.Add score
              elif policyTopNs.Length > 0 && not hasLiveStats then
                RuntimeUtilities.ConsoleUtils.yellowConsole $"\nSkipping policy TopN tests: engine '{engine.Name}' does not support LogLiveStats (requires Lc0/Ceres)"
              // Run merged value topN tests (single pass over puzzles)
              if valueTopNs.Length > 0 && hasLiveStats && not ct.IsCancellationRequested then
                let vTopNLabel = valueTopNs |> List.map string |> String.concat "," |> sprintf "Value [%s]"
                let scores = performValueMultiTopNTest valueTopNs engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress vTopNLabel) ct
                for score in scores do
                  sendU (PuzzleResult score)
                  results.Add score
              // Run combo policy+value test (single engine init)
              let hasPolicyValue = toRun |> List.exists (function PolicyValue -> true | _ -> false)
              if hasPolicyValue && hasLiveStats && not ct.IsCancellationRequested then
                let scores = performPolicyValueTest engine puzzles theme input.NumberOfPuzzlesInParallel input.IncludeFailedPuzzles input.ScoreAllPositions (mkProgress "PolicyValue") ct
                for score in scores do
                  sendU (PuzzleResult score)
                  results.Add score
              elif hasPolicyValue && not hasLiveStats then
                RuntimeUtilities.ConsoleUtils.yellowConsole $"\nSkipping PolicyValue combo test: engine '{engine.Name}' does not support LogLiveStats (requires Lc0/Ceres)"
              // run each remaining sub-test (skip Policy/PolicyTopN/ValueTopN/PolicyValue, already handled)
              for test in toRun do
                if ct.IsCancellationRequested then () else
                match test with
                | Value when hasLiveStats ->
                    let score = performValueNetworkTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel input.ScoreAllPositions (mkProgress "Value") ct
                    sendU (PuzzleResult score)
                    results.Add score

                | Policy -> ()       // already handled in merged multi-topN
                | PolicyTopN _ -> () // already handled in merged multi-topN
                | ValueTopN _ -> ()  // already handled in merged multi-topN
                | PolicyValue -> ()  // already handled above

                | Search node when node > 1 ->
                    let score = performPolicyOrSearchTest node engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress $"Search {node}n") ct
                    sendU (PuzzleResult score)
                    results.Add score

                | Search node when node <= 1 ->
                    printfn "  → Skipping Search test with %d nodes (requires node count higher than 1)" node

                | Solve node when node > 1 ->
                    let score = performSolveTest node engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress $"Solve {node}n") ct
                    sendU (PuzzleResult score)
                    results.Add score

                | Solve node when node <= 1 ->
                    printfn "  → Skipping Solve test with %d nodes (requires node count higher than 1)" node

                | _ -> ()  // skip if node = empty/zero or Value if not lc0/ceres
            with ex ->
              sendU (LichessError (sprintf "Engine '%s' failed: %s" engine.Name ex.Message))

            // partial timing
            let elapsed = System.Diagnostics.Stopwatch.GetElapsedTime start
            let total   = results |> Seq.sumBy (fun s -> s.TotalNumber)
            printfn "  → %d puzzles in %.0f s (theme: %s)" total elapsed.TotalSeconds themeLabel

    // wrap up
    sendU (Done "Finished!")
    let totalPretty = System.Diagnostics.Stopwatch.GetElapsedTime start |> ChessLibrary.GameAnalysis.Time.prettyPrintTimeSpan
    ChessLibrary.RuntimeUtilities.ConsoleUtils.greenConsole $"\nTotal time: {totalPretty}"

    results
