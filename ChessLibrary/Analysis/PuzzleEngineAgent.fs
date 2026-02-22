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
                    | BestMove (cmd, reply) ->
                        let mv = bestQPuzzleValueOnly engine cmd
                        reply.Reply (mv,0.0)
                    | BestMoveWithPolicy (cmd, correctMove, reply) ->
                        let mv = bestQPuzzleValueOnly engine cmd
                        reply.Reply (mv,String.Empty)
                    | Network reply ->
                        reply.Reply engine.Network
                    | Quit _ -> ()
                with ex ->
                    eprintfn "PuzzleEngineAgent (value) error: %s" ex.Message
                    match msg with
                    | BestMove (_, reply) -> reply.Reply ("", 0.0)
                    | BestMoveWithPolicy (_, _, reply) -> reply.Reply ("", String.Empty)
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
                | Network reply ->
                    reply.Reply engine.Network
                | Quit _ -> ()
            with ex ->
                eprintfn "PuzzleEngineAgent (policy) error: %s" ex.Message
                match msg with
                | BestMove (_, reply) -> reply.Reply ("", 0.0)
                | BestMoveWithPolicy (_, _, reply) -> reply.Reply ("", String.Empty)
                | Ok reply -> reply.Reply(false)
                | Network reply -> reply.Reply ""
                | Quit _ -> ()
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
          board.PlayUciMove mv
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

            let resultsBag = ConcurrentBag<PuzzleResult>()
            let worker (agent: MailboxProcessor<EngineMsg>) = async {
                let mutable keepGoing = true
                while keepGoing && not ct.IsCancellationRequested do
                    let ok, puzzle = puzzleCh.Reader.TryRead()
                    if ok then
                        let! result = runPuzzleViaAgent agent true puzzle
                        resultsBag.Add(result)
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
            }
    finally
        shutdownAgents agents

let performPolicyOrSearchTest
  (nodes:int)
  (engineCfg:EngineConfig)
  (puzzles:CsvPuzzleData[])
  (theme:string)
  (concurrency : int)
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

        let resultsBag = ConcurrentBag<PuzzleResult>()
        let worker (agent: MailboxProcessor<EngineMsg>) = async {
            let mutable keepGoing = true
            while keepGoing && not ct.IsCancellationRequested do
                let ok, puzzle = puzzleCh.Reader.TryRead()
                if ok then
                    let! result = runPuzzleViaAgent agent false puzzle
                    resultsBag.Add(result)
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
        }
    finally
        shutdownAgents agents


type SubTest =
    | Value
    | Policy
    | Search of node:int

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
    //check if toRun contains Search
    let hasSearch = toRun |> List.exists (function Search _ -> true | _ -> false)

    for engine, nodes in input.engines do
      if ct.IsCancellationRequested then () else
      let isCeresOrLc0 =
        engine.Path.ToLower().Contains("lc0")
        || engine.Name.ToLower().Contains("ceres")

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

            if hasSearch && nodes > 0 then
              let score = performPolicyOrSearchTest nodes engine puzzles theme input.NumberOfPuzzlesInParallel ct
              sendU (PuzzleResult score)
              results.Add score
            // run each requested sub-test
            for test in toRun do
              if ct.IsCancellationRequested then () else
              match test with
              | Value when isCeresOrLc0 ->
                  let score = performValueNetworkTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel ct
                  sendU (PuzzleResult score)
                  results.Add score

              | Policy ->
                  let score = performPolicyOrSearchTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel ct
                  sendU (PuzzleResult score)
                  results.Add score

              | Search node when node > 1 ->
                  let score = performPolicyOrSearchTest node engine puzzles theme input.NumberOfPuzzlesInParallel ct
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
    ChessLibrary.RuntimeUtilities.ConsoleUtils.greenConsole $"\nTotal time: {totalPretty}"

    results
