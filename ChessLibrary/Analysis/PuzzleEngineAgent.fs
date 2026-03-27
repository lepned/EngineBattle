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
let runPuzzleViaAgent (agent:MailboxProcessor<EngineMsg>) (valueHead : bool) (puzzle:CsvPuzzleData)  = async {
    // Reset engine state between puzzles (policy/search only)
    if not valueHead then
        do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    // 3b) Fresh board per puzzle
    let board = Board()
    let mutable correct    = true
    let mutable movePlayed = ""
    let mutable failedMove = ""
    let mutable policy = String.Empty

    for cmd in puzzle.Commands do
      if correct then
        // Ask engine for its candidate move
        let! (mv,p) = agent.PostAndAsyncReply(fun ch -> BestMoveWithPolicy(cmd, cmd.CorrectMove, ch))
        movePlayed <- mv

        // Compare to correct move, with mate-in-one fallback
        let mutable solved = cmd.CorrectMove = mv
        if not solved then
          failedMove <- cmd.CorrectMove
          policy <- p
          board.PlayCommands cmd.Command
          board.PlayUciMove mv
          solved <- not (board.AnyLegalMove())
        correct <- solved

    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }

    //Return minimal puzzle result
    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = valueHead
        Policy = policy
        KLD = 0.0
      }
  }

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
/// Returns (maxKLD, correctPerTopN: Map<int, bool>) where correctPerTopN maps each topN to whether puzzle was solved.
let runPuzzleViaAgentMultiTopN (agent:MailboxProcessor<EngineMsg>) (topNs:int list) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let maxTopN = topNs |> List.max
    let mutable maxKLD = 0.0
    // Track correctness per topN threshold
    let correct = System.Collections.Generic.Dictionary<int, bool>()
    for n in topNs do correct.[n] <- true

    for cmd in puzzle.Commands do
        let! (mv, allNNValues) = agent.PostAndAsyncReply(fun ch -> BestMoveWithAllPolicies(cmd, ch))
        let kld = computeKLD allNNValues cmd.CorrectMove
        if kld > maxKLD then maxKLD <- kld

        // Check each threshold
        for n in topNs do
          if correct.[n] then
            let topNMoves = allNNValues |> List.truncate n |> List.map (fun v -> v.LANMove)
            let mutable solved = topNMoves |> List.contains cmd.CorrectMove
            if not solved then
              // Mate fallback (only need to check once for the max topN)
              if n = maxTopN then
                let board = Board()
                board.PlayCommands cmd.Command
                board.PlayUciMove mv
                solved <- not (board.AnyLegalMove())
              // If mate fallback passed for maxTopN, it passes for all
            correct.[n] <- solved

    return (puzzle, maxKLD, correct |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
  }

/// Per-puzzle async workflow with top-N policy check and KLD computation
let runPuzzleViaAgentTopN (agent:MailboxProcessor<EngineMsg>) (topN:int) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let board = Board()
    let mutable correct    = true
    let mutable movePlayed = ""
    let mutable failedMove = ""
    let mutable policy = String.Empty
    let mutable maxKLD = 0.0

    for cmd in puzzle.Commands do
        // Always compute KLD for every move (independent of correctness)
        let! (mv, allNNValues) = agent.PostAndAsyncReply(fun ch -> BestMoveWithAllPolicies(cmd, ch))
        let kld = computeKLD allNNValues cmd.CorrectMove
        if kld > maxKLD then maxKLD <- kld

        // Only check correctness if still on track
        if correct then
          movePlayed <- mv

          let topNMoves = allNNValues |> List.truncate topN |> List.map (fun v -> v.LANMove)
          let mutable solved = topNMoves |> List.contains cmd.CorrectMove

          if not solved then
            failedMove <- cmd.CorrectMove
            let rank =
                allNNValues
                |> List.tryFindIndex (fun v -> v.LANMove = cmd.CorrectMove)
                |> Option.map (fun i -> i + 1)
            let correctP = allNNValues |> List.tryFind (fun v -> v.LANMove = cmd.CorrectMove)
            policy <-
                match rank, correctP with
                | Some r, Some cp -> sprintf "%.2f, rank #%d of top-%d" cp.P r topN
                | None, _ -> sprintf "not in policy output, top-%d" topN
                | _ -> ""

            // Mate fallback: check if bestmove delivers mate
            board.PlayCommands cmd.Command
            board.PlayUciMove mv
            solved <- not (board.AnyLegalMove())

          correct <- solved

    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0}

    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = false
        Policy = policy
        KLD = maxKLD
      }
  }

/// Per-puzzle multi-topN value workflow: one per-child evaluation, check all thresholds.
/// Returns (puzzle, correctPerTopN: Map<int, bool>)
let runPuzzleViaAgentValueMultiTopN (agent:MailboxProcessor<EngineMsg>) (topNs:int list) (puzzle:CsvPuzzleData) = async {
    do! agent.PostAndAsyncReply(fun ch -> NewGame ch)

    let maxTopN = topNs |> List.max
    let correct = System.Collections.Generic.Dictionary<int, bool>()
    for n in topNs do correct.[n] <- true

    for cmd in puzzle.Commands do
        let! moveVals = agent.PostAndAsyncReply(fun ch -> EvalAllMovesValue(cmd, ch))
        // Sort by V ascending (lower V from opponent = better for us)
        let sortedByV = moveVals |> List.sortBy snd

        for n in topNs do
          if correct.[n] then
            let topNMoves = sortedByV |> List.truncate n |> List.map fst
            let mutable solved = topNMoves |> List.contains cmd.CorrectMove
            if not solved && n = maxTopN then
              // Mate fallback
              let bestMove = if sortedByV.IsEmpty then "" else sortedByV.Head |> fst
              let board = Board()
              board.PlayCommands cmd.Command
              board.PlayUciMove bestMove
              solved <- not (board.AnyLegalMove())
            correct.[n] <- solved

    return (puzzle, correct |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
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

    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el) |> Seq.toList
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0}

    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = false
        Policy = policy
        KLD = 0.0
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
            KLD = 0.0 }
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

        let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el) |> Seq.toList
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
            KLD = 0.0 }
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
                        let! result = runPuzzleViaAgent agent true puzzle
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
                if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
                else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

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
                    let! result = runPuzzleViaAgent agent false puzzle
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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

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
        }
    finally
        shutdownAgents agents


let performPolicyMultiTopNTest
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
        printfn "Starting policy multi-topN [%s] test with %d concurrent agents..." topNsStr concurrency

        let puzzleCh = Channel.CreateUnbounded<CsvPuzzleData>()
        for p in puzzles do puzzleCh.Writer.TryWrite(p) |> ignore
        puzzleCh.Writer.Complete()

        let mutable processedCount = 0
        let total = puzzles.Length
        let resultsBag = ConcurrentBag<CsvPuzzleData * float * Map<int, bool>>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgentMultiTopN agent topNs puzzle
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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

        let avgRating =
          if allResults.Length = 0 then 0.0
          else allResults |> Array.averageBy (fun (p, _, _) -> p.Rating)
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme

        // Produce one Score per topN threshold
        topNs |> List.map (fun topN ->
            let correct = allResults |> Array.filter (fun (_, _, m) -> m.[topN])
            let failed  = allResults |> Array.filter (fun (_, _, m) -> not m.[topN])
            let avgKLD =
              if correct.Length = 0 then 0.0
              else correct |> Array.averageBy (fun (_, kld, _) -> kld)
            let w, d, l = correct.Length, 0, failed.Length

            let diffElo = EloCalculator.eloDiffWDL w d l
            let error   = EloCalculator.calculateEloError w d l
            let perf    = avgRating + diffElo
            let typeLabel = if topN = 1 then "Policy" else sprintf "pTop%d" topN
            printfn "\n%s rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s  AvgKLD: %.4f" typeLabel perf avgRating diffElo theme avgKLD
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
              FailedPuzzles = ResizeArray (failed |> Array.map (fun (p, _, _) -> p, ""))
              CorrectPuzzles = ResizeArray (correct |> Array.map (fun (p, _, _) -> p))
              Nodes = 1
              WithHistory = false
              Type = typeLabel
              AvgKLD = avgKLD
            }
        )
    finally
        shutdownAgents agents


let performPolicyTopNTest
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
        printfn "Starting policy top-%d test with %d concurrent agents..." topN concurrency

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
                    let! result = runPuzzleViaAgentTopN agent topN puzzle
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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

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
        let avgKLD =
          if correct.Length = 0 then 0.0
          else correct |> Array.averageBy (fun r -> r.KLD)
        let typeLabel = if topN = 1 then "Policy" else sprintf "pTop%d" topN
        if topN = 1 then
          printfn "\nPolicy network rating performance: %.0f (avg %.0f + Δ%.0f) Nodes 1 Theme: %s  AvgKLD: %.4f" perf avg diffElo theme avgKLD
        else
          printfn "\nPolicy top-%d rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s  AvgKLD: %.4f" topN perf avg diffElo theme avgKLD
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
          Nodes = 1
          WithHistory = false
          Type = typeLabel
          AvgKLD = avgKLD
        }
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
        let resultsBag = ConcurrentBag<CsvPuzzleData * Map<int, bool>>()
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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

        let avgRating =
          if allResults.Length = 0 then 0.0
          else allResults |> Array.averageBy (fun (p, _) -> p.Rating)
        let theme = if String.IsNullOrWhiteSpace theme then "none" else theme

        topNs |> List.map (fun topN ->
            let correct = allResults |> Array.filter (fun (_, m) -> m.[topN])
            let failed  = allResults |> Array.filter (fun (_, m) -> not m.[topN])
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
              FailedPuzzles = ResizeArray (failed |> Array.map (fun (p, _) -> p, ""))
              CorrectPuzzles = ResizeArray (correct |> Array.map (fun (p, _) -> p))
              Nodes = 0
              WithHistory = false
              Type = typeLabel
              AvgKLD = 0.0
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
            if not (String.IsNullOrEmpty engineCfg.NetworkPath) then engineCfg.NetworkPath
            else agents.[0].PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

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
        }
    finally
        shutdownAgents agents


type SubTest =
    | Value
    | Policy
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
                let scores = performPolicyMultiTopNTest policyTopNs engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress topNLabel) ct
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
              // run each remaining sub-test (skip Policy/PolicyTopN/ValueTopN, already handled)
              for test in toRun do
                if ct.IsCancellationRequested then () else
                match test with
                | Value when hasLiveStats ->
                    let score = performValueNetworkTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel (mkProgress "Value") ct
                    sendU (PuzzleResult score)
                    results.Add score

                | Policy -> ()       // already handled in merged multi-topN
                | PolicyTopN _ -> () // already handled in merged multi-topN
                | ValueTopN _ -> ()  // already handled in merged multi-topN

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
