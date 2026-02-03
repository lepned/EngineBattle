module ChessLibrary.PuzzleRunners

open System
open System.IO
open System.Text
open System.Diagnostics
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EPDTypes
open ChessLibrary.PuzzleTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.Chess
open ChessLibrary.TypesDef.PuzzleInput
open ChessLibrary.PuzzleEngineAnalysis
open ChessLibrary.PuzzleEngineAgent
open ChessLibrary.PuzzleDataUtils

/// Only value-head (LC0/Ceres only)
let runValueHeadTest
    (input    : PuzzleInput,
     callback : Action<Lichess>)
  =
  runTest input callback [ Value ]

/// Only policy-head
let runPolicyHeadTest
    (input    : PuzzleInput,
     callback : Action<Lichess>)
  =
  runTest input callback [ Policy ]

/// Only "search" runs at each node setting
let runSearchTests
    (input    : PuzzleInput,
     callback : Action<Lichess>)
  =
  let searches =
    parseNodes input.nodes
    |> Array.toList
    |> List.map Search
  runTest input callback searches

/// Value + Policy
let runValueAndPolicyHeadTest
    (input      : PuzzleInput,
     callback   : Action<Lichess>)
  =
  let testTypes = [ Value; Policy ]
  runTest input callback testTypes

/// Value + Search
let runValueAndSearchTest
    (input    : PuzzleInput,
     callback : Action<Lichess>)
  =
  let searches =
    parseNodes input.nodes
    |> Array.toList
    |> List.map Search
  runTest input callback (Value :: searches)

/// Policy + Search
let runPolicyAndSearchTests
    (input    : PuzzleInput,
     callback : Action<Lichess>)
  =
  let searches =
    parseNodes input.nodes
    |> Array.toList
    |> List.map Search
  runTest input callback (Policy :: searches)

/// All three: Value, Policy, and (if requested) Search
let runAllTests
    (input      : PuzzleInput,
     callback   : Action<Lichess>)
  =
  let baseTests = [ Value; Policy ]
  let allTests =
      baseTests @
      (parseNodes input.nodes |> Array.toList |> List.map Search)
  runTest input callback allTests

let writeToFile (data:EretConfig) (scores: ERETResults seq) (sw:StreamWriter) (boardBm: Chess.Board) (boardAm: Chess.Board) =
    for item in scores do
        let tc = if data.RunWithNodeLimit then $"Nodes: {data.Nodes}" else $"TimeMs: {data.TimeInSeconds * 1000}"
        sw.WriteLine($"\n## Failed puzzles by {item.PlayerName} - overall performance: {item.Accuracy:P0} {tc}\n")
        for (puzzle, movePlayed) in item.FailedPuzzles do
            boardBm.LoadFen(puzzle.FEN)
            let bestMoveInLsan =
              match puzzle.BestMove with
              | Some bm ->
                boardBm.PlaySanMove(bm)
                match boardBm.MovesAndFenPlayed |> Seq.tryLast with
                | Some m -> m.Move.LongSan
                | None -> ""
              | None -> ""
            boardAm.LoadFen(puzzle.FEN)
            let avoidMoveInLsan =
              match puzzle.AvoidMove with
              | Some am ->
                boardAm.PlaySanMove(am)
                match boardAm.MovesAndFenPlayed |> Seq.tryLast with
                | Some m -> m.Move.LongSan
                | None -> ""
              | None -> ""
            let msg =
              let puzzleId = match puzzle.Id with | Some id -> id | None -> "No ID"
              if puzzle.BestMove.IsSome then
                  $"{puzzle.FEN} bm {puzzle.BestMove.Value}; am {movePlayed}; id \"{puzzleId}, player {item.PlayerName}\"; other \"{bestMoveInLsan},{movePlayed}\""
              elif puzzle.AvoidMove.IsSome then
                  $"{puzzle.FEN} am {puzzle.AvoidMove.Value}; id \"{puzzleId}, player {item.PlayerName}\"; other \"{avoidMoveInLsan},{movePlayed}\""
              else
                  $"{puzzle.FEN} am {puzzle.AvoidMove.Value}; id \"{puzzleId}, player {item.PlayerName} - No best move or avoid move found\""
            sw.WriteLine(msg)

let runEretTests (timeConfig: UnionType) (engineConfigs: (EngineConfig * int) seq) (data: EretConfig) (callback: Action<ERET>) =
  let sampleSize = data.SampleSize
  let epdPath = data.PuzzleFile
  let isnodes, nodes, time =
    match timeConfig with
    | UnionType.FixedTime t -> false, -1, t.ToTimeSpan().TotalMilliseconds |> int
    | UnionType.Nodes n -> true, n, 0
    |_ -> false, -1, 0

  let sendUpdate (update: ERET) = callback.Invoke(update)

  if isnodes then
    Start(sprintf "Running %d Eret positions with node limit: %A" sampleSize nodes) |> sendUpdate
  else
    Start(sprintf "Running %d Eret positions with time limit in ms: %A" sampleSize time) |> sendUpdate
  let puzzles = EPDExtractor.readEPDs epdPath |> Seq.truncate sampleSize |> Seq.toArray

  let watch = Stopwatch.StartNew()

  let runSingleEngine (engineConfig: (EngineConfig * int)) =
      let board = Board()
      let engineConfigOnly, _ = engineConfig
      let puzzlePolicyEngine = getPuzzlePolicyEngine (engineConfigOnly, None)
      puzzlePolicyEngine.Name <- puzzlePolicyEngine.Name
      let failedPuzzles = ResizeArray<EPDEntry * string>()
      let correctPuzzles = ResizeArray<EPDEntry>()
      let sbPuzzle = StringBuilder()
      sbPuzzle.AppendLine(sprintf "\nRun Date: %s" (DateTime.Now.ToLongDateString())) |> ignore
      sbPuzzle.AppendLine(sprintf "\tEngine: %s" puzzlePolicyEngine.Name) |> ignore
      sbPuzzle.AppendLine(sprintf "\t\tNetwork: %s" puzzlePolicyEngine.Network) |> ignore
      sbPuzzle.AppendLine(sprintf "\t\tExecPath: %s" puzzlePolicyEngine.Path) |> ignore
      for opt in puzzlePolicyEngine.Commands do
          sbPuzzle.AppendLine(sprintf "\t\t%s" opt) |> ignore
      let mutable puzzleNr = 1

      for puzzle : EPDEntry in puzzles do
          board.ResetBoardState()
          board.LoadFen puzzle.FEN
          let fen = board.FEN()
          let position = "position fen " + fen
          puzzleNr <- puzzleNr + 1
          let move, nnValue =
              if isnodes then
                  bestPolicyMove nodes puzzlePolicyEngine position
              else
                  bestMoveWithTime time puzzlePolicyEngine position
          board.PlayUciMove move
          let shortSanMove =
              match board.SanMovesPlayed |> Seq.tryLast with
              | Some move -> move
              | None ->
                  failwithf "No moves played for puzzle with FEN: %s" puzzle.FEN

          let correct =
              match puzzle.BestMove with
              | Some bm -> bm.Contains shortSanMove
              | None ->
                  match puzzle.AvoidMove with
                  | Some am -> am.Contains shortSanMove |> not
                  | None -> false

          puzzlePolicyEngine.UciNewGame()
          if correct then
              correctPuzzles.Add puzzle
          else
              failedPuzzles.Add (puzzle,move)
          Puzzle(puzzle, correct) |> sendUpdate

      puzzlePolicyEngine.Quit()
      let accuracy = (float)correctPuzzles.Count / (float)(correctPuzzles.Count + failedPuzzles.Count)
      let desc = if isnodes then sprintf "Nodes: %d" nodes else sprintf "Time: %d ms" time
      let eretResult = { PlayerName = puzzlePolicyEngine.Name; CorrectPuzzles = correctPuzzles; FailedPuzzles = failedPuzzles; Accuracy = accuracy; Desc=desc }
      PlayerResult(eretResult) |> sendUpdate
      eretResult

  let engineConfigsOnly = engineConfigs |> Seq.map fst
  let concurrency =
      if isnodes then
          HardwareInfo.concurrencyLevel engineConfigsOnly (engineConfigs |> Seq.length)
      else
          1
  printfn "Concurrency level: %d" concurrency

  let resultsNew =
      if concurrency > 1 then
          engineConfigs
          |> Seq.toArray
          |> Array.Parallel.map runSingleEngine
          |> Seq.toList
      else
          engineConfigs
          |> Seq.map runSingleEngine
          |> Seq.toList

  watch.Stop()
  resultsNew |> List.sortByDescending (fun eret -> eret.Accuracy) |> AllResults |> sendUpdate

  let isnodes, nodes, time =
    match timeConfig with
    | UnionType.FixedTime t -> false, -1, t.ToTimeSpan().TotalMilliseconds |> int
    | UnionType.Nodes n -> true, n, 0
    |_ -> false, -1, 0

  let eretSummary =
      resultsNew
      |> List.sortByDescending (fun eret -> eret.Accuracy)
      |> List.map (fun eret ->
          let correctCount = Seq.length eret.CorrectPuzzles
          let failedCount = Seq.length eret.FailedPuzzles
          let accStr = eret.Accuracy.ToString("P1")
          (eret.PlayerName, accStr, correctCount, failedCount)
      )

  //get the max length of the engine name for coumn width
  let maxEngineNameLength = eretSummary |> List.map (fun (engine, _, _, _) -> engine.Length) |> List.max
  let colWidths = [maxEngineNameLength + 2; 11; 10; 10]
  let eretFileName = Path.GetFileName epdPath
  let elapsed = watch.Elapsed

  let sb = StringBuilder()

  // Add timing information
  let timeMsg = if isnodes then sprintf "Nodes: %d" nodes else sprintf "Time: %d ms" time

  // Append opening markdown for Discord
  sb.AppendLine("\n```") |> ignore

  // Append ERET Test file name and details
  sb.AppendFormat("ERET Test file name: {0}, number of positions: {1}, {2}\n\n", eretFileName, sampleSize, timeMsg) |> ignore

  // Print header with proper alignment
  let header = sprintf "%-*s %-*s %-*s %-*s" colWidths.[0] "Engine" colWidths.[1] "Accuracy" colWidths.[2] "Correct" colWidths.[3] "Failed"
  sb.AppendLine(header) |> ignore
  sb.AppendLine(String.replicate (List.sum colWidths + 3) "-") |> ignore // Divider line

  // Append each row with proper alignment
  for (engine, acc, correct, failed) in eretSummary do
      sb.AppendFormat("{0,-" + colWidths.[0].ToString() + "} {1,-" + colWidths.[1].ToString() + "} {2,-" + colWidths.[2].ToString() + "} {3,-" + colWidths.[3].ToString() + "}\n", engine, acc, correct, failed) |> ignore

  sb.AppendLine(String.replicate (List.sum colWidths + 3) "-") |> ignore // Divider line
  sb.AppendLine("```") |> ignore // Closing markdown for Discord

  // Retrieve the accumulated string
  let outputString = sb.ToString()
  ResultsInConsole(outputString) |> sendUpdate

  let timing = sprintf "Time taken to run Eret tests: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
  ResizeArray(resultsNew)
