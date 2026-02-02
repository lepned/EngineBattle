module ChessLibrary.PuzzleEngineAgent

open System
open System.Threading
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
            | Ok reply ->
                reply.Reply(true)
                return! loop()
            | BestMove (cmd, reply) ->
                let mv = bestQPuzzleValueOnly engine cmd
                reply.Reply (mv,0.0)
                return! loop()
            | BestMoveWithPolicy (cmd, correctMove, reply) ->
                let mv = bestQPuzzleValueOnly engine cmd
                reply.Reply (mv,String.Empty)
                return! loop()

            | Quit reply ->
                engine.Quit()
                do! Async.Sleep 1000
                reply.Reply()
                // exit loop
            | Network reply ->
                reply.Reply engine.Network
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
        | Ok reply ->
             reply.Reply(true)
             return! loop()
        | BestMove (cmd, reply) ->
            let mv, nnValue = bestPolicyMove nodes engine cmd.Command
            reply.Reply (mv,(if nnValue.IsSome then 0.0 else 0.0))
            return! loop()
        | BestMoveWithPolicy (cmd, correctMove, reply) ->
            let mv, nnValue = bestPolicyMoveWithPolicy correctMove nodes engine cmd.Command
            if nnValue.Length = 0 then
              reply.Reply (mv, String.Empty)
            elif nnValue.Length = 1 then
              reply.Reply (mv, sprintf "%.2f" nnValue.Head.P)
            else
              let nnValueString = nnValue |> List.map (fun v -> sprintf "%.2f" v.P) |> String.concat ", "
              reply.Reply (mv, nnValueString)
            return! loop()

        | Quit reply ->
            engine.StopProcess()
            reply.Reply()
            // exit loop
        | Network reply ->
            reply.Reply engine.Network
            return! loop()
      }
      loop()
    )


//Per-puzzle async workflow
let runPuzzleViaAgent (agent:MailboxProcessor<EngineMsg>) (valueHead : bool) (puzzle:CsvPuzzleData)  = async {
    //Reset engine state
    //do! agent.PostAndAsyncReply(fun ch -> NewGame ch) |> Async.Ignore

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
          board.PlayLongSanMove mv
          solved <- not (board.AnyLegalMove())
        correct <- solved

    let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el)
    let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }

    //Return minimal puzzle result
    return
      { PuzzleData = puzzleWithMove
        WasCorrect = correct
        MovePlayed = movePlayed
        FailedMove = failedMove
        ValueHead = valueHead
        Policy = policy
      }
  }

/// A little wrapper around MailboxProcessor<'Msg> that
/// blocks PostAndAsyncReply once you've got 250 in flight.
type BoundedAgent<'Msg>(inner: MailboxProcessor<'Msg>, capacity: int) =
    let sem = new SemaphoreSlim(capacity, capacity)

    /// Expose the underlying MailboxProcessor
    member _.Inner = inner

    /// Exactly the same signature as MailboxProcessor.PostAndAsyncReply
    member _.PostAndAsyncReply(build: AsyncReplyChannel<'T> -> 'Msg) : Async<'T> =
      async {
        // wait until one "slot" is free
        do! Async.AwaitTask (sem.WaitAsync())
        try
          // send the message and await reply
          let! res = inner.PostAndAsyncReply(build)
          return res
        finally
          // release the slot once the reply comes back
          sem.Release() |> ignore
      }


//Main test runner
let performValueNetworkTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency: int)  =

    let concurrencyLevel = max 1 concurrency

    let agents =
        [| for i in 1 .. concurrencyLevel do
            let mb = startValueEngineAgent engineCfg
            yield BoundedAgent<EngineMsg>(mb,250)|]

    let ok = agents |> Array.map (fun a -> a.PostAndAsyncReply(fun ch -> Ok ch)) |> Async.Parallel |> Async.RunSynchronously
    if ok |> Array.exists (fun x -> not x) then
          Score.empty
    else
        printfn "Starting value network test with %d concurrent agents..." concurrencyLevel
        // Partition the puzzles among agents
        let puzzleChunks =
            puzzles
            |> Array.indexed
            |> Array.groupBy (fun (idx, _) -> idx % concurrencyLevel)
            |> Array.map (fun (_, items) -> items |> Array.map snd)

      // Map each agent to its workload and collect async work items
        let agentJobs =
            Array.zip agents puzzleChunks
            |> Array.map (fun (agent, agentPuzzles) ->
                agentPuzzles |> Array.map (runPuzzleViaAgent agent.Inner true))

      // Execute all jobs in parallel and collect results
        let results =
            agentJobs
            |> Array.map Async.Parallel
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.collect id

        let firstAgent = agents.[0]
        let networkName = firstAgent.PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

        //Shutdown the engines
        let _ =
            agents
            |> Array.map (fun agent -> agent.PostAndAsyncReply(fun ch -> Quit ch))
            |> Async.Parallel
            |> Async.RunSynchronously

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
        let score =
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
            }

        score

let performPolicyOrSearchTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)  =

    let concurrency = max 1 concurrency
    let agents =
        [| for i in 1 .. concurrency do
            let mb = startPolicyEngineAgent engineCfg nodes
            yield BoundedAgent<EngineMsg>(mb,250)
        |]

     // Partition the puzzles among agents
    let puzzleChunks =
        puzzles
        |> Array.indexed
        |> Array.groupBy (fun (idx, _) -> idx % concurrency)
        |> Array.map (fun (_, items) -> items |> Array.map snd)

    printfn "Starting policy/search test with %d concurrent agents..." concurrency
    // Map each agent to its workload and collect async work items
    let agentJobs =
        Array.zip agents puzzleChunks
        |> Array.map (fun (agent, agentPuzzles) ->
            agentPuzzles |> Array.map (runPuzzleViaAgent agent.Inner false))

  // Execute all jobs in parallel and collect results
    let results =
        agentJobs
        |> Array.map Async.Parallel
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.collect id

    let firstAgent = agents.[0]
    let networkName = firstAgent.PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously

    //Shutdown the engine
    let _ =
        agents
        |> Array.map (fun agent -> agent.PostAndAsyncReply(fun ch -> Quit ch))
        |> Async.Parallel
        |> Async.RunSynchronously

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
    let score =
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
        }

    score


type SubTest =
    | Value
    | Policy
    | Search of node:int

let runTest
      (input     : PuzzleInput)
      (callback  : Action<Lichess>)
      (toRun     : SubTest list)
    : ResizeArray<Score> =

    // common setup
    let enginesOnly = input.engines |> Seq.map fst
    ChessLibrary.Configuration.Validation.validateAllEnginesAndSomeSettings enginesOnly
    let ratings = PuzzleDataUtils.parseRatingGroups input.ratingGroups input.maxRating
    let themes  = PuzzleDataUtils.parseThemes input.puzzleFilter

    let sendU = callback.Invoke
    let results = ResizeArray<Score>()
    let start   = System.Diagnostics.Stopwatch.GetTimestamp()
    //check if toRun contains Search
    let hasSearch = toRun |> List.exists (function Search _ -> true | _ -> false)

    for engine, nodes in input.engines do
      let isCeresOrLc0 =
        engine.Path.ToLower().Contains("lc0")
        || engine.Name.ToLower().Contains("ceres")

      for theme in themes do
        let themeLabel = if System.String.IsNullOrWhiteSpace theme then "none" else theme

        for rating in ratings do
          // load & log
          let puzzles = PuzzleDataUtils.sortPuzzleData theme rating input
          if puzzles.Length = 0 then
              ChessLibrary.LowLevelUtilities.ConsoleUtils.yellowConsole $"\nSkipping tests: No puzzles found matching theme '{theme}' and rating {rating}"
          else
            let avg = puzzles |> Array.averageBy (fun p -> p.Rating)
            printfn "\nRating group %d (avg %.0f), theme \"%s\"" rating avg themeLabel

            if hasSearch && nodes > 0 then
              let score = performPolicyOrSearchTest nodes engine puzzles theme input.NumberOfPuzzlesInParallel
              sendU (PuzzleResult score)
              results.Add score
            // run each requested sub-test
            for test in toRun do
              match test with
              | Value when isCeresOrLc0 ->
                  let score = performValueNetworkTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel
                  sendU (PuzzleResult score)
                  results.Add score

              | Policy ->
                  let score = performPolicyOrSearchTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel
                  sendU (PuzzleResult score)
                  results.Add score

              | Search node when node > 1 ->
                  let score = performPolicyOrSearchTest node engine puzzles theme input.NumberOfPuzzlesInParallel
                  sendU (PuzzleResult score)
                  results.Add score

              | Search node when node <= 1 ->
                  printfn "  → Skipping Search test with %d nodes (requires node count higher than 1)" node

              | _ -> ()  // skip if node = empty/zero or Value if not lc0/ceres

            // partial timing
            let elapsed = System.Diagnostics.Stopwatch.GetElapsedTime start
            let total   = results |> Seq.sumBy (fun s -> s.TotalNumber)
            printfn "  → %d puzzles in %.0f s (theme: %s)" total elapsed.TotalSeconds themeLabel

    // wrap up
    sendU (Done "Finished!")
    let totalPretty = System.Diagnostics.Stopwatch.GetElapsedTime start |> ChessLibrary.GameAnalysis.Time.prettyPrintTimeSpan
    ChessLibrary.LowLevelUtilities.ConsoleUtils.greenConsole $"\nTotal time: {totalPretty}"

    results
