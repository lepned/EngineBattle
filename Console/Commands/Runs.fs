/// The long runs: puzzle run / eret / trend, book eval, tournament run / validate, tune run /
/// dashboard, piece-values batch. Ctrl+C: the first press stops the run and keeps what it has, the second
/// stops at once; either way the exit code is 130 (Cli.Output.onCtrlC).
module Commands.Runs

open System
open System.IO
open System.Text
open System.Threading
open System.Diagnostics
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System.Linq
open Serilog
open ChessLibrary
open TypesDef
open TimeControlTypes
open PuzzleTypes
open Tournament
open Configuration
open GameAnalysis
open TournamentPairing
open RuntimeUtilities
open MathNet.Numerics.LinearAlgebra
open Configuration.JSONParser
open System.Globalization
open TournamentTypes

let createCombinedScoresTable
    (fileName: string)
    (policyScores: ChessLibrary.PuzzleTypes.Score list)
    (valueScores: ChessLibrary.PuzzleTypes.Score list)
    (searchScores: ChessLibrary.PuzzleTypes.Score list)
    (solveScores: ChessLibrary.PuzzleTypes.Score list)=

    let sb = StringBuilder()
    sb.AppendLine("\n```\n") |> ignore
    sb.AppendLine(sprintf "Puzzle file name: %s\n" fileName) |> ignore

    // Combine all sets of scores
    let allScores = policyScores @ valueScores @ searchScores @ solveScores

    // Engine column auto-show: only when it disambiguates — a row has no net name to
    // identify it, or the same net appears under different engine configs (e.g. two
    // configs differing only in options). Otherwise Neural net alone identifies every
    // row and the column is redundant width.
    let showEngineColumn =
        allScores |> List.exists (fun s -> System.String.IsNullOrEmpty s.NeuralNet)
        || (allScores
            |> List.groupBy (fun s -> s.NeuralNet)
            |> List.exists (fun (_, group) ->
                group |> List.map (fun s -> s.Engine) |> List.distinct |> List.length > 1))

    // Helper to get max of header length vs. data lengths
    let maxOf (header:string) (lengths :int list) =
        let length = if lengths.Length > 0 then (lengths |> List.max) else 0
        max (header.Length) length

    let maxEngineWidth =
        maxOf "Engine" (allScores |> List.map (fun s -> s.Engine.Length))
    let maxNeuralNetWidth =
        maxOf "Neural net" (allScores |> List.map (fun s -> s.NeuralNet.Length))
    let maxPerfWidth =
        maxOf "Perf"
          (allScores
           |> List.map (fun s -> s.PlayerRecord.Rating.ToString("F0").Length))
    let maxAccuracyWidth =
        maxOf "Accuracy"
          (allScores
           |> List.map (fun s ->
               let pct = decimal s.Correct / decimal s.TotalNumber
               pct.ToString("P1").Length))
    let maxTotalWidth =
        maxOf "Total" (allScores |> List.map (fun s -> s.TotalNumber.ToString().Length))
    let maxAvgRatingWidth =
        maxOf "AvgR"
          (allScores
           |> List.map (fun s -> s.RatingAvg.ToString("F0").Length))
    let maxThemeWidth =
        maxOf "Theme"
          (allScores |> List.map (fun s -> s.Filter.Length + 2))
    let maxNodesWidth =
        maxOf "Nodes" (allScores |> List.map (fun s -> s.Nodes.ToString().Length))
    let maxTypeWidth =
        maxOf "Type" (allScores |> List.map (fun s -> s.Type.Length))
    let maxKLDWidth =
        maxOf "KLD" (policyScores |> List.map (fun s -> if s.AvgKLD > 0.0 then s.AvgKLD.ToString("F4").Length else 1))
    let formatKLD (s: ChessLibrary.PuzzleTypes.Score) =
        if s.AvgKLD > 0.0 then s.AvgKLD.ToString("F4") else "—"
    // Compact node-count formatter shared by the P95/P99/Max columns. Values can sit
    // below 10 (rank-1 puzzles contribute 0), so keep decimals at the low end or the
    // column collapses to "0".
    let formatNodeCount (n: float) =
        if n < 10.0 then n.ToString("F2")
        elif n < 1000.0 then n.ToString("F0")
        elif n < 1_000_000.0 then (n / 1000.0).ToString("F1") + "k"
        else (n / 1_000_000.0).ToString("F2") + "M"
    // AvgEstNodesLog10 = 0 means the estimated-nodes metric is absent (no policy data),
    // which blanks the percentile and CDF columns.
    let formatEstPercentile (value: float) (s: ChessLibrary.PuzzleTypes.Score) =
        if s.AvgEstNodesLog10 > 0.0 then formatNodeCount value else "—"
    let maxEstP95Width =
        maxOf "P95" (policyScores |> List.map (fun s -> (formatEstPercentile s.EstNodesP95 s).Length))
    let maxEstP99Width =
        maxOf "P99" (policyScores |> List.map (fun s -> (formatEstPercentile s.EstNodesP99 s).Length))
    // CDF at a 100-node budget: "a 100-node search reaches the correct move on X% of positions".
    let formatEstCdf100 (s: ChessLibrary.PuzzleTypes.Score) =
        if s.AvgEstNodesLog10 > 0.0 then (s.EstNodesCdf100 * 100.0).ToString("F1") + "%" else "—"
    let maxEstCdfWidth =
        maxOf "≤100" (policyScores |> List.map (fun s -> (formatEstCdf100 s).Length))
    // Worst case of the whole set by the formula: HardestByEstNodes is sorted desc,
    // so its head is the max per-puzzle estimate. First-visit estimate only — real
    // nodes-to-solve can be far higher when the value head also misjudges the child.
    let formatEstMax (s: ChessLibrary.PuzzleTypes.Score) =
        if s.AvgEstNodesLog10 > 0.0 && s.HardestByEstNodes.Count > 0
        then formatNodeCount (snd s.HardestByEstNodes.[0])
        else "—"
    let maxEstMaxWidth =
        maxOf "Max" (policyScores |> List.map (fun s -> (formatEstMax s).Length))

    let sep = "  "
    let engCol (value: string) =
        if showEngineColumn then value.PadRight maxEngineWidth + sep else ""
    // Base columns shared by all sections
    let baseHeader =
        engCol "Engine"
        + ("Neural net".PadRight maxNeuralNetWidth) + sep
        + ("Perf".PadRight maxPerfWidth) + sep
        + ("Accuracy".PadRight maxAccuracyWidth) + sep
        + ("Total".PadRight maxTotalWidth) + sep
        + ("AvgR".PadRight maxAvgRatingWidth) + sep
        + ("Theme".PadRight maxThemeWidth)
    let baseRow (s: ChessLibrary.PuzzleTypes.Score) =
        let perf = s.PlayerRecord.Rating.ToString("F0")
        let accuracy = (decimal s.Correct / decimal s.TotalNumber).ToString("P1")
        engCol s.Engine
        + (s.NeuralNet.PadRight maxNeuralNetWidth) + sep
        + (perf.PadRight maxPerfWidth) + sep
        + (accuracy.PadRight maxAccuracyWidth) + sep
        + (s.TotalNumber.ToString().PadRight maxTotalWidth) + sep
        + (s.RatingAvg.ToString("F0").PadRight maxAvgRatingWidth) + sep
        + (s.Filter.PadRight maxThemeWidth)

    // Policy: base + Type + KLD + P95 + P99 + Max + ≤100 (no Nodes)
    let policyHeaderLine =
        baseHeader + sep + ("Type".PadRight maxTypeWidth) + sep + ("KLD".PadRight maxKLDWidth)
        + sep + ("P95".PadRight maxEstP95Width) + sep + ("P99".PadRight maxEstP99Width)
        + sep + ("Max".PadRight maxEstMaxWidth) + sep + ("≤100".PadRight maxEstCdfWidth)
    // Value: base + Type (no Nodes, no KLD)
    let valueHeaderLine = baseHeader + sep + ("Type".PadRight maxTypeWidth)
    // Search/Solve: base + Nodes (no Type, no KLD)
    let searchHeaderLine = baseHeader + sep + ("Nodes".PadRight maxNodesWidth)

    let mutable startGroup =
        match policyScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0


    let mutable widestText = ""

    // Sum up all column widths and add a buffer for tab spacing
    let approximateWidth =
        let sepWidth = 2
        let columns = if showEngineColumn then 8 else 7
        (if showEngineColumn then maxEngineWidth else 0)
        + maxNeuralNetWidth + maxPerfWidth + maxAccuracyWidth +
        maxTotalWidth + maxAvgRatingWidth + maxThemeWidth + maxNodesWidth +
        (columns - 1) * sepWidth
        //20 // Buffer for spacing between columns

    let separatorLine = String.replicate approximateWidth "-"

    // Append policy Tests
    if policyScores.Length > 0 then
        sb.AppendLine("Policy Head Tests\n") |> ignore
        sb.AppendLine(policyHeaderLine) |> ignore
    policyScores
    |> List.iter (fun s ->
        let line =
            baseRow s + sep + (s.Type.PadRight maxTypeWidth) + sep + ((formatKLD s).PadRight maxKLDWidth)
            + sep + ((formatEstPercentile s.EstNodesP95 s).PadRight maxEstP95Width)
            + sep + ((formatEstPercentile s.EstNodesP99 s).PadRight maxEstP99Width)
            + sep + ((formatEstMax s).PadRight maxEstMaxWidth)
            + sep + ((formatEstCdf100 s).PadRight maxEstCdfWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append Value Head Tests
    if valueScores.Length > 0 then
        sb.AppendLine("Value Head Tests\n") |> ignore
        sb.AppendLine(valueHeaderLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match valueScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0
    // Append each value score row
    valueScores
    |> List.iter (fun s ->
        let line = baseRow s + sep + (s.Type.PadRight maxTypeWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append search Tests
    if searchScores.Length > 0 then
        sb.AppendLine("Search Tests\n") |> ignore
        sb.AppendLine(searchHeaderLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match searchScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0

    // Append each search score row
    searchScores
    |> List.iter (fun s ->
        let line = baseRow s + sep + (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append solve Tests
    if solveScores.Length > 0 then
        sb.AppendLine("Solve Tests\n") |> ignore
        sb.AppendLine(searchHeaderLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match solveScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0

    // Append each solve score row
    solveScores
    |> List.iter (fun s ->
        let line = baseRow s + sep + (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.Append("\n```\n") |> ignore
    sb.ToString()

module RunOutput =

  /// Prepares a run's output folder and says what happened, returning the folder the
  /// run should write into ("" when it must write nothing).
  ///
  /// Runs before any engine is created. A run whose folder could not be prepared still
  /// completes — it just produces no files, and now says so instead of looking like a
  /// normal run that happened to write nothing.
  let report (label: string) (configured: string) : string =
      match PuzzleDataUtils.ensureOutputFolder configured with
      | PuzzleDataUtils.Ready(path, created) ->
          let suffix = if created then " (created)" else ""
          ConsoleUtils.greenConsole $"{label} -> {path}{suffix}"
          path
      | PuzzleDataUtils.NotConfigured ->
          ConsoleUtils.yellowConsole
              "No FailedPuzzlesOutputFolder set - no result files will be written."
          ""
      | PuzzleDataUtils.Failed(path, message) ->
          // exception messages already end in a period, so do not add another
          let reason = message.TrimEnd('.')
          ConsoleUtils.redConsole
              $"Cannot use output folder {path}: {reason}. The run continues, but no result files will be written."
          ""

module Eret =

  let timeConfig = UnionType.FixedTime (TimeSpan(0,0,5)) //10 seconds

  /// <summary>
  /// Processes the ERET update and prints relevant information.
  /// </summary>
  let processEret (data :EretConfig) (update : ERET) =
    match update with    
    | Start info -> printfn "\nERET test started: %s" info
    | Puzzle (epd, correct) -> 
        let avoidM, bMove = 
          (if epd.AvoidMove.IsSome then sprintf "\n\tAvoidMove: %s" epd.AvoidMove.Value else ""), 
          if epd.BestMove.IsSome then sprintf "\n\tBestMove: %s" epd.BestMove.Value else ""
        let epdText = sprintf "id: %A FEN: %s %s %s" epd.Id epd.FEN avoidM bMove
        printfn "Puzzle: %s, Correct/solved: %b" epdText correct
    | PlayerResult res ->
        printfn "\nPlayer results for %s: Correct: %d, Failed: %d, Accuracy: %s\n" res.PlayerName (Seq.length res.CorrectPuzzles) (Seq.length res.FailedPuzzles) (res.Accuracy.ToString("F2"))
    | AllResults eretResults -> 
        printfn "\nAll failed puzzles by engine below:" 
        printfn "--------------------------------------------------------------------"
        for res in eretResults do
            printfn "\nFailed puzzles by %s: Correct: %d, Failed: %d, Accuracy: %s\n" res.PlayerName (Seq.length res.CorrectPuzzles) (Seq.length res.FailedPuzzles) (res.Accuracy.ToString("F2"))
            for (puzzle,_) in res.FailedPuzzles do                
                printfn "%s" puzzle.RawInput
        printfn "\n--------------------------------------------------------------------"
        let escaped = data.FailedPuzzlesOutputFolder

        if escaped <> "" then
            let datePart = DateTime.Now.ToString("yyyy-MM-dd_HH-mm", System.Globalization.CultureInfo.InvariantCulture)
            let fileName = Path.Combine(escaped, $"failedEretPuzzles_{datePart}.epd")
            let boardBm = Chess.Board()
            let boardAm = Chess.Board()
            try
              use sw = File.AppendText(fileName)
              PuzzleRunners.writeToFile data eretResults sw boardBm boardAm
              Console.WriteLine($"Failed Eret puzzles written to {fileName}")
            with 
            | ex ->
                Console.WriteLine($"Failed to write results to file: {ex.Message}")
       
    | ResultsInConsole table ->
        let escaped = data.FailedPuzzlesOutputFolder
        if escaped <> "" then
          let datePart = DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
          let outputPath = Path.Combine(escaped, $"EretSummary_{datePart}.txt")
          try
            File.WriteAllText(outputPath, table);
            Console.WriteLine($"Console summary written to {outputPath}")
          with
          | ex ->
              Console.WriteLine($"Failed to write results to file: {ex.Message}")
        printfn "\n%s" table
    | EretError msg ->
        RuntimeUtilities.ConsoleUtils.redConsole $"\nERET Error: {msg}"

 


/// <summary>
/// Configures logging for the application.
/// </summary>
let configureLogging (builder: ILoggingBuilder) =
    builder
        .AddSerilog()
        .AddConsole()
        .SetMinimumLevel(LogLevel.Critical)
        |> ignore

// logs/eb-cli-<date>.log in the working directory, a new file each day and past 10 MB
let logPath = Path.Combine(Environment.CurrentDirectory, "logs", "eb-cli-.log");

/// <summary>
/// Creates and configures the host for the application.
/// </summary>
let createHost() =
    Log.Logger <- LoggerConfiguration() // Create a Serilog logger configuration
      .MinimumLevel.Information() // Set the minimum log level
      //.WriteTo.Console() // Write to console
      .WriteTo.File(logPath, rollingInterval = Serilog.RollingInterval.Day, rollOnFileSizeLimit = true, fileSizeLimitBytes = 10_000_000L)
      .CreateLogger() // Create the logger
    Host.CreateDefaultBuilder()
        .ConfigureLogging(configureLogging)
        .Build()
      


/// An ERET run from its config (a path that exists); exit 1 when an engine failed.
let runEretTest (path: string) (json: bool) (ct: CancellationToken) =
  let loaded =
    try loadEretConfig path
    with ex -> raise (Cli.Parse.Usage $"not an ERET config: {path}: {ex.Message}")
  if obj.ReferenceEquals(loaded, null) then raise (Cli.Parse.Usage $"not an ERET config: {path}")
  printfn "Processing ERET puzzle file: %s" path
  // Resolved once, before any engine loads, and carried on the config the writers
  // read. Empty means the run writes no files - the handler skips on that.
  let data =
    { loaded with
        FailedPuzzlesOutputFolder = RunOutput.report "ERET results" loaded.FailedPuzzlesOutputFolder }
  let time = UnionType.FixedTime (TimeSpan(0,0,data.TimeInSeconds))
  let nodes = UnionType.Nodes data.Nodes
  let engineConfigs = 
      data.Engines 
      |> Seq.collect (mapToEngPuzzleConfig data.EngineFolder)
      |> ResizeArray   
  let timeControl = if data.RunWithNodeLimit then nodes else time
  // an engine that fails ends the run with its exception (exit 1); the parallel run's names each one
  // every engine's result, for --json
  let mutable final = []
  let onUpdate (update: ERET) =
    match update with
    | AllResults results -> final <- List.ofSeq results
    | _ -> ()
    Eret.processEret data update
  try PuzzleRunners.runEretTests timeControl engineConfigs data (Action<ERET>(onUpdate)) ct |> ignore
  with :? AggregateException as ae ->
    failwith (String.Join("; ", ae.Flatten().InnerExceptions |> Seq.map (fun e -> e.Message) |> Seq.distinct))
  if ct.IsCancellationRequested then
    Cli.Output.warn "stopped with Ctrl+C: the summary counts only the positions that were done"
  if json then
    let engines =
      [ for r in final ->
          {| Engine = r.PlayerName; Correct = r.CorrectPuzzles.Count; Failed = r.FailedPuzzles.Count; Accuracy = r.Accuracy
             FailedPuzzles = [ for epd, played in r.FailedPuzzles -> {| Id = Option.toObj epd.Id; Fen = epd.FEN; Played = played |} ] |} ]
    let errors =
      if ct.IsCancellationRequested then
        [ ({ Code = "interrupted"; Message = "stopped with Ctrl+C: only the positions that were done are counted" } : Cli.Output.JsonError) ]
      else []
    Cli.Output.printJson (Cli.Output.envelope "puzzle eret" (not ct.IsCancellationRequested) (box {| Config = path; Engines = engines |}) errors)
  Cli.Output.ExitOk

/// A puzzle run from its config (a path that exists). With json the result (PuzzleJsonSchema.md)
/// is printed as the envelope's result. Exit 1 when an engine failed or nothing was scored.
let runPuzzles (path: string) (json: bool) (ct: CancellationToken) =
  let startedUtc = DateTime.UtcNow
  let runStopwatch = System.Diagnostics.Stopwatch.StartNew()
  let data =
    try loadPuzzleConfig path
    with ex -> raise (Cli.Parse.Usage $"not a puzzle config: {path}: {ex.Message}")
  if obj.ReferenceEquals(data, null) then raise (Cli.Parse.Usage $"not a puzzle config: {path}")
  let normalizedPath = Cli.Groups.inputFile "puzzle file (PuzzleFile in the config)" data.PuzzleFile
  printfn "Processing Lichess puzzle file: %s" path
  // Resolved before any engine loads, so an unusable path costs seconds instead of a
  // whole run. Empty means "write nothing", and every writer below reads this binding.
  let outputFolder = RunOutput.report "Puzzle results" data.FailedPuzzlesOutputFolder
  let engineConfigs = 
      data.Engines 
      |> Seq.collect (mapToEngPuzzleConfig data.EngineFolder)
      |> ResizeArray

  // The record is the only holder of the database, so the runner can release it once the
  // samples are drawn (see JSONParser.loadPuzzleInput for why it is not built here).
  let puzzleInput =
      loadPuzzleInput(
          normalizedPath,
          data.MaxRating,
          data.MinRating,
          data.RatingGroups,
          data.PuzzleFilter,
          engineConfigs,
          1,
          data.SampleSize,
          data.Nodes,
          data.Failed,
          data.Solved,
          data.Concurrency,
          data.IncludeFailedPuzzles,
          data.ScoreAllPositions )
  // Only the count outlives the run: the runner empties puzzleData once it has its samples.
  let puzzleDbCount = puzzleInput.puzzleData.Length
  printfn $"Loaded {puzzleDbCount:N0} puzzles from {normalizedPath}"

  // Engine failures (refused net, crash mid-sweep) arrive here as LichessError; counted so
  // the exit code below can say the run is incomplete.
  let engineErrors = ref 0
  let update (res: Lichess) =
      match res with
      | PuzzleResult score ->
          let correct = score.Correct
          let total = score.TotalNumber
          let failed = total - correct
          let name = score.Engine
          printfn "Puzzle result for %s: Correct: %d, Failed: %d" name correct failed
      | Done msg -> printfn "Puzzle done: %s" msg
      | Progress (processed, total, label) ->
          eprintf "\r  %s: %d / %d" label processed total
      | LichessError msg ->
          engineErrors.Value <- engineErrors.Value + 1
          RuntimeUtilities.ConsoleUtils.redConsole $"\nPuzzle Error: {msg}"
                
  let scores =
      // One parser with the GUI (PuzzleRunners.parseSubTests). An unknown token refuses
      // the run: silently running the tokens that did parse produced a narrower run that
      // finished green under the config's own label.
      // A malformed Nodes value is a config error whatever Type says; refuse it with the
      // offending text instead of an unhandled FormatException (it used to be read lazily,
      // so an empty Type never touched it).
      let nodeList =
          try PuzzleDataUtils.parseNodes puzzleInput.nodes
          with ex -> raise (Cli.Parse.Usage $"Nodes in the config: {ex.Message}")
      match PuzzleRunners.parseSubTests data.Type nodeList with
      | Result.Error unknown -> raise (Cli.Parse.Usage $"Type in the config: {PuzzleRunners.unknownSubTestsMessage unknown}")
      | Result.Ok [] ->
          printfn "No puzzle types specified, defaulting to both policy and value test"
          PuzzleRunners.runValueAndPolicyHeadTest(puzzleInput, update, ct)
      | Result.Ok subTests ->
          printfn "Puzzle types specified: %s" data.Type
          PuzzleEngineAgent.runTest puzzleInput (Action<Lichess>(update)) subTests ct

  let valueScores = 
      scores 
      |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Value"))
      |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                       .ThenByDescending(fun e -> e.RatingAvg)
                       .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)                         
      |> Seq.toList

  let policyScores = 
      scores 
      |> Seq.filter (fun e -> e.TotalNumber > 0 && (e.Type.Contains("Policy") || e.Type.StartsWith("pTop")))
      |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                       .ThenByDescending(fun e -> e.RatingAvg)
                       .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
      |> Seq.toList

  // No `Nodes > 1` clause: a search at one node is typed "Policy" by PuzzleEngineAgent
  // and lands in policyScores, so the clause never excluded a search - it only dropped
  // legitimate one-node Solve results, which the GUI kept. One rule for both paths:
  // a row belongs here if it produced results.
  let search =
      scores
      |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Search"))
      |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                       .ThenByDescending(fun e -> e.RatingAvg)
                       .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
      |> Seq.toList

  let solve =
      scores
      |> Seq.filter (fun e -> e.TotalNumber > 0 && e.Type.Contains("Solve"))
      |> fun seq -> seq.OrderBy(fun e -> e.Filter)
                       .ThenByDescending(fun e -> e.RatingAvg)
                       .ThenByDescending(fun e -> decimal e.Correct / decimal e.TotalNumber)
      |> Seq.toList

  let writeToFile (scores: Score seq) (sw:StreamWriter) (boardBm: Chess.Board) (boardAm: Chess.Board) =
      for item in scores do
          sw.WriteLine($"\n## Failed puzzles by {item.Engine} (nn: {item.NeuralNet}) - overall performance: {item.PlayerRecord.Rating:F0} - Type: {item.Type} - Theme: {item.Filter} - Nodes: {item.Nodes}\n")
          let sorted =
              item.FailedPuzzles
              |> Seq.map (fun (a,b) ->
                      let parts = b.Split(',')
                      match parts with
                      | [| value; _ |] ->
                          let v = Double.Parse(value.Trim(), CultureInfo.InvariantCulture)
                          (a, v, b)
                      | _ -> (a, float a.Rating, b)
              )
              |> Seq.sortBy (fun (_, value, _) -> value)
              //|> Seq.takeWhile (fun (_, value, _) -> value < 100.0)
          
          for (puzzle,_,policyStr) in sorted do
              for cmd in puzzle.Commands do
                  if not (String.IsNullOrWhiteSpace(cmd.MovePlayed)) && cmd.MovePlayed.Length >= 4 then
                      boardBm.PlayCommands(cmd.Command)
                      let fen = boardBm.FEN()
                      // the bm half needs the same guard as the am half: PlayUciMove
                      // no-ops on an unmatched move and the last SAN is then the
                      // opponent's setup move, written out as a legal-looking wrong bm
                      let bm = PuzzleDataUtils.sanOfMovePlayed boardBm cmd.Command cmd.CorrectMove
                      let aM = PuzzleDataUtils.sanOfMovePlayed boardAm cmd.Command cmd.MovePlayed
                      let policies = policyStr.Split(',')
                      let bmP, amP =
                          if policies.Length > 1 then
                              policies.[0].Trim(), policies.[1].Trim()
                          else
                              "", ""
                      let msg = $"{fen} bm {bm}; am {aM}; id \"Lichess id {puzzle.PuzzleId}, policy value for bestmove {bm}={bmP} and move played {aM}={amP}\"; other \"{cmd.CorrectMove},{cmd.MovePlayed}\""
                      sw.WriteLine(msg)
      
  let writeFailedPuzzlesToCsv (allScores: Score list list) (csvPath: string) =
      let allFailed =
          allScores
          |> Seq.concat
          |> Seq.collect (fun score -> score.FailedPuzzles |> Seq.map fst)
          |> Seq.distinctBy (fun p -> p.PuzzleId)
          |> Seq.toArray
      if allFailed.Length > 0 then
          use sw = new StreamWriter(csvPath)
          sw.WriteLine("PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags")
          for p in allFailed do
              sw.WriteLine($"{p.PuzzleId},{p.Fen},{p.Moves},{int p.Rating},{int p.RatingDeviation},{p.Popularity},{p.NbPlays},{p.Themes},{p.GameUrl},{p.OpeningTags}")
          printfn "  Failed puzzles CSV (%d unique): %s" allFailed.Length csvPath

  // Worst-case candidates nominated by the 1-node EstNodes formula, worst first.
  // Written in lichess CSV format so the file can feed a `search`-type config
  // directly for targeted real-search verification of the predicted hardest puzzles.
  let writeHardestByEstNodesCsv (allScores: Score list list) (csvPath: string) =
      let formatEstNodes (n: float) =
          if n < 10.0 then n.ToString("F2", CultureInfo.InvariantCulture)
          elif n < 1000.0 then n.ToString("F0", CultureInfo.InvariantCulture)
          elif n < 1_000_000.0 then (n / 1000.0).ToString("F1", CultureInfo.InvariantCulture) + "k"
          else (n / 1_000_000.0).ToString("F2", CultureInfo.InvariantCulture) + "M"
      let hardest =
          allScores
          |> Seq.concat
          |> Seq.collect (fun score -> score.HardestByEstNodes)
          |> Seq.sortByDescending snd
          |> Seq.distinctBy (fun (p, _) -> p.PuzzleId)
          |> Seq.truncate 50
          |> Seq.toArray
      if hardest.Length > 0 then
          use sw = new StreamWriter(csvPath)
          sw.WriteLine("PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags")
          for (p, _) in hardest do
              sw.WriteLine($"{p.PuzzleId},{p.Fen},{p.Moves},{int p.Rating},{int p.RatingDeviation},{p.Popularity},{p.NbPlays},{p.Themes},{p.GameUrl},{p.OpeningTags}")
          printfn "  Hardest-by-EstNodes CSV (top %d, worst first): %s" hardest.Length csvPath
          printfn "  Top 10 worst-case candidates by estimated nodes to find the correct move:"
          for (p, estN) in hardest |> Seq.truncate 10 do
              printfn "    %-12O  rating %4d  est %-7s  %s" p.PuzzleId (int p.Rating) (formatEstNodes estN) p.GameUrl

  // One order for the theme tables, the paired table and both JSON writes: A must
  // mean the same net in all of them or the same comparison reads with its sign
  // flipped in one table and not the other.
  let engineNamesInConfigOrder =
      engineConfigs
      |> Seq.map (fun (cfg: ChessLibrary.TypesDef.CoreTypes.EngineConfig, _) -> cfg.Name)
      |> Seq.toList

  // Paired (McNemar) stats. Same slices as the theme tables, but at slice level the
  // solved/failed sets survive, so the discordant counts - and with them the tight test -
  // are available. Empty for single-net runs.
  //
  // Computed ONCE, here: the text summary renders it and both JSON writes embed it. It
  // sits outside the output-folder guard on purpose - a run configured to write no files
  // still has a result worth printing, and only the "full table" pointer needs a folder.
  let allScoresForCross = Seq.concat [ policyScores; valueScores; search; solve ]
  // An empty result is normal (a single-net run has nothing to compare); a failure is
  // not. The outcome carries both so neither the JSON nor the console can confuse them.
  let pairedOutcome =
      try PuzzlePaired.outcomeOf (PuzzlePaired.computeOrdered engineNamesInConfigOrder allScoresForCross)
      with ex ->
          RuntimeUtilities.ConsoleUtils.redConsole $"Paired comparison failed: {ex.Message}"
          PuzzlePaired.failedOutcome
  let pairedComparisons = pairedOutcome.Comparisons

  // One console line, same rule as the theme headline: point at the file, do not reprint
  // it. Thin rows are excluded from the headline, not from the table: z = 2.0 off four
  // disagreements is exactly the kind of number that must not lead a run report.
  match
      pairedComparisons
      |> List.filter (fun c -> c.Discordant >= PuzzlePaired.ThinDiscordance)
      |> List.sortByDescending (fun c -> abs c.Z) with
  | strongest :: _ when abs strongest.Z >= 2.0 ->
      printfn ""
      printfn "--- Paired comparison (McNemar) ---"
      // same naming rule as the table: a cross-engine pair shares a net name, and
      // "netX vs netX" in the headline reads as a bug in the tool
      let nameA, nameB =
          PuzzlePaired.sideNames strongest.NetA strongest.NetB strongest.EngineA strongest.EngineB
      printfn "  Strongest: %s vs %s, %s @ rating %d: %+.1f pp, z=%+.2f (%d discordant of %d)"
          nameA nameB strongest.Type strongest.RatingGroup
          strongest.DeltaPp strongest.Z strongest.Discordant strongest.N
  | _ -> ()

  // One reading for both JSON writes: taking Elapsed twice reported two different
  // durations for one run in the two files.
  let elapsedSeconds = runStopwatch.Elapsed.TotalSeconds

  // Prepared at run start; empty when nothing should be written.
  let escaped = outputFolder
  let table = createCombinedScoresTable normalizedPath policyScores valueScores search solve
  printfn "%s" table

  if escaped <> "" then
      let filenameFriendlyDate = DateTime.Now.ToString("yyyy-MM-dd_HH-mm", System.Globalization.CultureInfo.InvariantCulture)        
      // The folder is prepared at run start, but it can still disappear during a run
      // that takes hours - a dropped share, a cleanup job. Every writer is wrapped so
      // one failure costs its own file and nothing else: not the remaining files, not
      // the --json output an external tool asked for, and not the process.
      let mutable writeFailures = 0
      let tryWrite (what: string) (action: unit -> unit) =
          try action ()
          with ex ->
              writeFailures <- writeFailures + 1
              RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write {what}: {ex.Message}"

      //let datePart = DateTime.Now.ToString("yyyyMMdd_HHmmss", CultureInfo.InvariantCulture)
      let fileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.epd")
      tryWrite fileName (fun () ->
          let boardBm = Chess.Board()
          let boardAm = Chess.Board()
          use sw = File.AppendText(fileName)
          writeToFile policyScores sw boardBm boardAm
          writeToFile valueScores sw boardBm boardAm
          writeToFile search sw boardBm boardAm
          writeToFile solve sw boardBm boardAm)

      let csvFileName = Path.Combine(escaped, $"failedLichessPuzzles_{filenameFriendlyDate}.csv")
      tryWrite csvFileName (fun () ->
          writeFailedPuzzlesToCsv [policyScores; valueScores; search; solve] csvFileName)

      let estCsvFileName = Path.Combine(escaped, $"estNodesHardest_{filenameFriendlyDate}.csv")
      tryWrite estCsvFileName (fun () ->
          writeHardestByEstNodesCsv [policyScores] estCsvFileName)

      tryWrite "cross-engine files" (fun () ->
          PuzzleCrossEngine.writeCrossEngineFiles escaped filenameFriendlyDate allScoresForCross)

      // Per-theme breakdown, shared with the GUI so both runs produce the same files.
      // Returns the summary appended to LichessSummary_<stamp>.txt below.
      let themeOutput =
          try
              PuzzleThemes.writeThemeFiles
                  escaped
                  filenameFriendlyDate
                  engineNamesInConfigOrder
                  allScoresForCross
          with ex ->
              writeFailures <- writeFailures + 1
              RuntimeUtilities.ConsoleUtils.redConsole $"Per-theme comparison failed: {ex.Message}"
              { Summary = ""; CsvPath = ""; Headline = "" }
      // the tables go to the summary file - dumping a dozen of them on someone who
      // ran a puzzle test buries the result they came for
      if themeOutput.Summary <> "" then
          printfn "
--- Per-Theme Comparison ---"
          printfn "%s" themeOutput.Headline
          printfn "  Full tables: %s" (Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt"))
          printfn "  All themes:  %s" themeOutput.CsvPath
      let themeSummary = themeOutput.Summary

      let pairedSummary =
          // counted like every other writer: the summary file loses a section, and the
          // green "Result files written" line below must not claim otherwise. A silent
          // `with _ -> ""` here hid the loss entirely.
          if pairedOutcome.Failed then
              writeFailures <- writeFailures + 1
              ""
          else
              try PuzzlePaired.render pairedComparisons
              with ex ->
                  writeFailures <- writeFailures + 1
                  RuntimeUtilities.ConsoleUtils.redConsole $"Paired table rendering failed: {ex.Message}"
                  ""
      if pairedSummary <> "" then
          printfn "  Full paired table: %s" (Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt"))

      let testTypeInfo =
          let tokens = if String.IsNullOrWhiteSpace data.Type then [||] else data.Type.ToLower().Split(',') |> Array.map (fun t -> t.Trim())
          String.Join("-", tokens)
      let engineCount = engineConfigs.Count
      //write table to file with date and time
      let tableFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.txt")
      //let tableFileName = Path.Combine(escaped, $"LichessPuzzleScore_{datePart}.txt")
      tryWrite tableFileName (fun () ->
          use tableWriter = new StreamWriter(tableFileName)
          if ct.IsCancellationRequested then
              tableWriter.WriteLine("STOPPED WITH Ctrl+C: the rating group in progress is not counted")
          tableWriter.WriteLine(table)
          if pairedSummary <> "" then
              tableWriter.WriteLine(pairedSummary)
          if themeSummary <> "" then
              tableWriter.WriteLine(themeSummary))

      // Always write the machine-readable twin beside the text summary, same basename.
      // Trend tooling reads these; nothing should have to parse the human-formatted table,
      // whose column layout has drifted across many releases. See PuzzleJsonSchema.md.
      let pairedJsonFileName = Path.Combine(escaped, $"LichessSummary_{filenameFriendlyDate}.json")
      // a run stopped with Ctrl+C has no JSON twin: puzzle trend and the run report would read
      // its partial numbers as a finished checkpoint
      if ct.IsCancellationRequested then
          Cli.Output.warn $"stopped with Ctrl+C: no {Path.GetFileName pairedJsonFileName} (the text summary has what finished)"
      else
          try
              PuzzleJsonOutput.buildResultWithPaired
                  pairedOutcome
                  normalizedPath
                  puzzleDbCount
                  data.SampleSize
                  data.MinRating
                  data.MaxRating
                  data.PuzzleFilter
                  data.RatingGroups
                  startedUtc
                  elapsedSeconds
                  scores
              |> PuzzleJsonOutput.writeToFile pairedJsonFileName
          with ex ->
              writeFailures <- writeFailures + 1
              RuntimeUtilities.ConsoleUtils.redConsole $"Failed to write JSON summary to {pairedJsonFileName}: {ex.Message}"

      // Naming the folder at the end makes a mistyped path obvious instead of leaving a
      // run that looks finished. It must never claim success over a red line above it -
      // the whole point is that a run which wrote nothing does not read as a normal one.
      if writeFailures = 0 then
          RuntimeUtilities.ConsoleUtils.greenConsole $"Result files written to {escaped}"
      else
          RuntimeUtilities.ConsoleUtils.yellowConsole
              $"{writeFailures} output file(s) failed to write (see above). Folder: {escaped}"

  // The result, in the schema of PuzzleJsonSchema.md (the same document as the
  // LichessSummary_<stamp>.json above), as the result of the --json envelope.
  let scored = scores |> Seq.filter (fun sc -> sc.TotalNumber > 0) |> Seq.length
  let ok = engineErrors.Value = 0 && scored > 0
  if json then
      let result =
          PuzzleJsonOutput.buildResultWithPaired
              pairedOutcome
              normalizedPath
              puzzleDbCount
              data.SampleSize
              data.MinRating
              data.MaxRating
              data.PuzzleFilter
              data.RatingGroups
              startedUtc
              elapsedSeconds
              scores
      let errors =
          [ if ct.IsCancellationRequested then
              { Cli.Output.Code = "interrupted"; Cli.Output.Message = "stopped with Ctrl+C: the rating group in progress is not counted" }
            if not ok then
              { Cli.Output.Code = "incomplete"; Cli.Output.Message = $"{engineErrors.Value} engine error(s), {scored} score(s) recorded" }
            if pairedOutcome.Failed then
              { Cli.Output.Code = "paired-failed"; Cli.Output.Message = "the paired comparison failed; result.paired is empty" } ]
      let node = System.Text.Json.Nodes.JsonNode.Parse(PuzzleJsonOutput.serialize result)
      Cli.Output.printJson (Cli.Output.envelope "puzzle run" (ok && not ct.IsCancellationRequested) (box node) errors)
  // A run in which an engine failed (refused network, crash mid-sweep) is not a finished one;
  // the files for the groups that did run are written above.
  // stopped with Ctrl+C: the exit code is 130 whatever was recorded
  if ct.IsCancellationRequested then Cli.Output.ExitOk
  elif not ok then
      Cli.Output.error $"puzzle run incomplete: {engineErrors.Value} engine error(s), {scored} score(s) recorded"
      Cli.Output.ExitFailed
  else Cli.Output.ExitOk
/// A tournament to its end, or until `ct` is cancelled (Ctrl+C: the games in progress stop, the
/// finished ones are in the PGN). True when it went through.
/// What a console tournament run gives back: whether it went through, why not, the games of this
/// run, how long it took, and the standings it printed (players, pairs).
type TournamentOutcome =
  { Succeeded: bool
    Failure: string option
    Results: TypesDef.CoreTypes.Result list
    Duration: TimeSpan
    Standings: (TypesDef.CoreTypes.PlayerResult seq * TypesDef.CoreTypes.CrossTableEntry seq * TypesDef.CoreTypes.Result seq) option }

let runTournamentOutcome (tournament:Tournament.Tournament) (logger: Microsoft.Extensions.Logging.ILogger) (ct: CancellationToken) =
  
  /// Compact result-reason labels for console output.
  let shortReason (reason: ChessLibrary.MiscTypes.ResultReason) : string =
      match reason with
      | ChessLibrary.MiscTypes.Checkmate -> "mate"
      | ChessLibrary.MiscTypes.Stalemate -> "stale"
      | ChessLibrary.MiscTypes.AdjudicateTB -> "tb-known"
      | ChessLibrary.MiscTypes.AdjudicateMaterial -> "insuf-mat"
      | ChessLibrary.MiscTypes.ExcessiveMoves -> "50-moves"
      | ChessLibrary.MiscTypes.Repetition -> "rep-draw"
      | ChessLibrary.MiscTypes.AdjudicatedEvaluation -> "eval-agree"
      | ChessLibrary.MiscTypes.ForfeitLimits -> "time-limit"
      | ChessLibrary.MiscTypes.Cancel -> "cancelled"
      | ChessLibrary.MiscTypes.Illegal -> "illegal"
      | ChessLibrary.MiscTypes.NotStarted -> "not-started"
      | ChessLibrary.MiscTypes.AdjudicatedByUser -> "adj-user"
      | ChessLibrary.MiscTypes.Resignation -> "resigns"
      | ChessLibrary.MiscTypes.Disconnected _ -> "disconn"
      | ChessLibrary.MiscTypes.Stalled _ -> "stalled"

  /// Define the MailboxProcessor for handling updates asynchronously
  let createUpdateProcessor (verbose:bool) =
      MailboxProcessor.Start(fun inbox ->
          let rec loop () =
              async {
                  // Wait for a message
                  let! update = inbox.Receive()
                  // Process the update
                  match update with
                  | GameStarted _ -> ()  // white-player info already shown in StartOfGame line
                  | EndOfGame result ->
                      let time = float result.GameTime / 1000.0
                      // Named, not numbered: the result carries no game number, and the last STARTED number is wrong once games overlap.
                      printfn "\u25C0 %s vs %s  %-7s  %-12s  %dm %.1fs"
                          result.Player1 result.Player2 result.Result (shortReason result.Reason) result.Moves time
                  | BestMove (bm, status) -> 
                      if verbose then
                          printfn "Player %s: BestMove %s with eval %A" bm.Player bm.Move bm.Eval
                  | Info (player, info) -> 
                      if verbose then
                          printfn "Player %s with info data: %s" player info
                  | Eval (player, evalType) -> 
                      if verbose then
                          printfn "Player %s with eval %A" player evalType
                  | Status engineStatus -> 
                      if verbose then
                          printfn "\tPlayer %s with PV: %s" engineStatus.PlayerName engineStatus.PV
                  | PonderStatus engineStatus -> 
                      if verbose then
                          printfn "\tPlayer %s with Nodes: %d" engineStatus.PlayerName engineStatus.Nodes
                  | Time (player, time) -> 
                      if verbose then
                          printfn "\tPlayer %s with time left %A" player time
                  | NNSeq nnSeq -> 
                      if verbose then
                          printfn "NNSeq: %A" nnSeq
                  | StartOfGame startGameInfo ->
                      printfn "\u25B6 G%d  %s vs %s"
                          startGameInfo.CurrentGameNr
                          startGameInfo.WhitePlayer.Name
                          startGameInfo.BlackPlayer.Name
                  | EndOfTournament info -> printfn "End Of Tournament:\n%s" (info.Summary())
                  | StartOfTournament info ->
                      printfn "\nStart of tournament:\nNumber of games to play: %d \nSummary:\n%s\n" info.NumberOfGames (info.Tournament.Value.MinSummary())
                  | MessagesFromEngine (player, message) -> 
                      if verbose then
                          printfn "MessagesFromEngine: Player - %s, Message - %s" player message
                  | PairingList pairings ->
                      printfn "%s" (PairingHelper.getOpeningPairs (pairings |> Seq.toList))
                  | PeriodicResults results ->
                      match results |> Seq.tryHead with
                      | Some _ -> printfn "Partial update after game %d" results.Count
                      | None -> printfn "Partial update after game (no games played yet)"                    
                  | GameSummary summary -> printfn "\nTournament Summary: \n%s" summary
                  | TotalNumberOfPairs totalPairs -> 
                      if verbose then
                          printfn "Total number of pairs in tournament: %d" totalPairs
                  | RoundNr roundNr -> 
                      if verbose then
                          printfn "Round number: %s" roundNr
                  | _ ->
                      if verbose then
                          printfn "Received update: %A" update

                  // Continue processing messages
                  return! loop ()
              }
          loop ()
      )
    
  let tourny = tournament
  //ConsoleHelper.displayTournament tourny      
  let updateProcessor = createUpdateProcessor tourny.VerboseLogging
  // the games this run finished, as they finish: Run's own list is the PGN's when a run fails
  let finished = ResizeArray<TypesDef.CoreTypes.Result>()
  let printUpdate update =
      match update with
      // a game cut off by Ctrl+C is not in the PGN
      | EndOfGame result when result.Reason <> ChessLibrary.MiscTypes.Cancel && result.Reason <> ChessLibrary.MiscTypes.NotStarted ->
          lock finished (fun () -> finished.Add result)
      | _ -> ()
      updateProcessor.Post(update)

  let runner = Manager.Runner(logger, printUpdate, false)
  runner.AddTournament tourny
  runner.LinkCancellation ct
  // true when the run went through: tournamentjson's exit code
  let mutable failed = false
  let mutable failure = None
  let mutable standings = None
  let start = Stopwatch.GetTimestamp()
  let formatHms (ts: TimeSpan) = 
      let hours = int ts.TotalHours 
      let minutes = ts.Minutes 
      let seconds = ts.Seconds 
      sprintf "%dh %dm %ds" hours minutes seconds

  try
      let results = runner.Run()
      // a run that stopped with an error has said why; no "completed" summary after it
      failed <- runner.Failure.IsSome
      failure <- runner.Failure |> Option.map string
      if not failed then
        let endTime = Stopwatch.GetElapsedTime start
        let gamesPlayed = results |> Seq.length
        let how = if ct.IsCancellationRequested then "stopped (Ctrl+C)" else "completed"
        let msg = $"Tournament {how} - Duration: {formatHms endTime}, Games: {gamesPlayed}, Parallel: {tourny.NumberOfGamesInParallel}"
        Console.WriteLine msg
        logger.LogInformation(msg)
        if File.Exists tourny.PgnOutPath then
            let consoleRes, players, cross, allResults = PGNCalculator.getEngineDataResults (runner.GetPGNGames())
            standings <- Some (Seq.ofList players, Seq.ofArray (cross.ToArray()), Seq.ofArray allResults)
            Console.WriteLine consoleRes
            logger.LogInformation(consoleRes)
        else
            let results = results |> ResizeArray
            let scoreTable = runner.GetPlayerResults(results)
            let table = runner.GenerateStatsCrosstable(results)
            standings <- Some (Seq.ofArray (scoreTable.ToArray()), Seq.ofArray (table.ToArray()), Seq.ofArray (results.ToArray()))
            let consoleRes = StandingsTable.getResultsAndPairsInConsoleFormat scoreTable table
            Console.WriteLine consoleRes
            logger.LogInformation(consoleRes)
      
  with
  |ex ->
      printfn "Caught an exception: %s" ex.Message
      failed <- true
      failure <- Some ex.Message
  { Succeeded = not failed; Failure = failure; Results = lock finished (fun () -> List.ofSeq finished); Duration = Stopwatch.GetElapsedTime start; Standings = standings }

/// A console tournament run: true when it went through.
let runTournament tournament logger ct = (runTournamentOutcome tournament logger ct).Succeeded

// ---------- Batch piece-value analysis over a folder of nets ----------
// For each net: self-play (RR) at the template's time control - nodes=1 in the usual template -
// then outcome + pgneval endgame regressions.
// Reuses runTournament and the pvfit helpers. Resumable (skips a net whose PGN is
// already complete) and fault-tolerant (a failing net is logged and skipped).

let pvCountGames (pgn: string) : int =
    if not (File.Exists pgn) then 0
    else File.ReadLines pgn |> Seq.filter (fun l -> l.StartsWith("[Result ")) |> Seq.length

/// Endgame (<=16 pieces) implied values (N,B,R,Q with pawn=1) from outcome regression.
let pvOutcomeEndgame (pgn: string) : (float * float * float * float) option =
    let rows = Commands.Values.pvCollectOutcomeRows pgn 400000 |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
    if rows.Length < 50 then None
    else
        let b = Commands.Values.pvFitTanh (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
        if abs b.[0] < 1e-9 then None
        else Some (b.[1] / b.[0], b.[2] / b.[0], b.[3] / b.[0], b.[4] / b.[0])

/// Endgame implied values (N,B,R,Q with pawn=1) from PGN-eval regression.
let pvPgnEvalEndgame (pgn: string) : (float * float * float * float) option =
    let rows = Commands.Values.pvCollectPgnEvalRows pgn 400000 None |> Array.filter (fun (_, _, tp, _) -> tp <= 16)
    if rows.Length < 50 then None
    else
        let b = Commands.Values.pvFitL1 (rows |> Array.map (fun (f, _, _, _) -> f)) (rows |> Array.map (fun (_, v, _, _) -> v))
        if abs b.[1] < 1e-9 then None
        else Some (b.[2] / b.[1], b.[3] / b.[1], b.[4] / b.[1], b.[5] / b.[1])

/// `piece-values batch`: what it works on - a template tournament and a folder of nets, both existing.
type BatchParams =
    { Template: string
      NetFolder: string
      Rounds: int
      OutDir: string }

/// For each net: self-play at the template's time control, then the outcome and pgn-eval endgame
/// fits; a net whose PGN already has the rounds is not played again (the batch resumes). The summary
/// keeps the rows of nets this run did not reach and replaces those it did. Ctrl+C stops the net
/// being played without fitting it; the next run plays it on from its PGN.
let runPvBatch (p: BatchParams) (ct: CancellationToken) =
    match JSON.readTournamentJson p.Template |> Option.map (JSON.withAppTablebaseFolder Cli.Groups.appSyzygyFolder) with
    | None -> raise (Cli.Parse.Usage $"cannot read the template tournament: {p.Template}")
    | Some templateTourny ->
        let templateEngines = JSON.readEngineDefs templateTourny.EngineSetup.EngineDefFolder templateTourny.EngineSetup.EngineDefList
        match templateEngines with
        | [] -> raise (Cli.Parse.Usage "the template tournament has no engine def to use as a template")
        | templateEngine :: _ ->
            Directory.CreateDirectory(p.OutDir) |> ignore
            let nets = Directory.GetFiles(p.NetFolder, "*.onnx") |> Array.sort
            printfn "Found %d nets in %s\nOutput: %s\n" nets.Length p.NetFolder p.OutDir
            let summaryPath = Path.Combine(p.OutDir, "summary.csv")
            // net -> its rows; the rows of an earlier run stay until this run has the net's own
            let rows = Collections.Generic.SortedDictionary<string, string list>(StringComparer.Ordinal)
            if File.Exists summaryPath then
                for line in File.ReadAllLines summaryPath |> Array.skip 1 do
                    match line.Split(',') with
                    | parts when parts.Length > 1 && line.Trim() <> "" ->
                        rows.[parts.[0]] <- (match rows.TryGetValue parts.[0] with | true, l -> l | _ -> []) @ [ line + "\n" ]
                    | _ -> ()
            let writeSummary () =
                File.WriteAllText(summaryPath, "net,mode,knight,bishop,rook,queen\n" + String.Join("", rows.Values |> Seq.concat))
            use host = createHost()
            host.Start()
            let loggerFactory = host.Services.GetService(typeof<ILoggerFactory>) :?> ILoggerFactory
            let logger = loggerFactory.CreateLogger("EngineBattle piece-values batch")
            let cloneEngine (name: string) (netPath: string) =
                let opts = System.Collections.Generic.Dictionary<string, obj>(templateEngine.Options)
                opts.["Network"] <- box netPath
                { templateEngine with Name = name; IsChallenger = false; NetworkPath = p.NetFolder; Options = opts }
            let fmt (v: float) = sprintf "%.2f" v
            let mutable failures = 0
            for net in nets do
                if not ct.IsCancellationRequested then
                    let baseName = Path.GetFileNameWithoutExtension net
                    try
                        let netPath = net.Replace("\\", "/")
                        let pgn = Path.Combine(p.OutDir, baseName + ".pgn").Replace("\\", "/")
                        printfn "\n==== %s ====" baseName
                        let have = pvCountGames pgn
                        let played =
                          if have >= p.Rounds then
                            printfn "  the PGN already has %d games: not played again" have
                            true
                          else
                            let engA = cloneEngine (sprintf "PVB-%s-a" baseName) netPath
                            let engB = cloneEngine (sprintf "PVB-%s-b" baseName) netPath
                            let tourny =
                                { templateTourny with
                                    EngineSetup = { templateTourny.EngineSetup with Engines = [ engA; engB ] }
                                    TournamentMode = "RR"
                                    Rounds = p.Rounds
                                    PgnOutPath = pgn
                                    ConsoleOnly = true
                                    MinMoveTimeInMS = 0
                                    DelayBetweenGames = TimeSpan.Zero
                                    // Self-play loads two copies of the net per concurrency level, so concurrency 2
                                    // means 4 simultaneous TensorRT engines - big nets OOM the GPU. Clamp to 1
                                    // regardless of the template; this also enables PreventMoveDeviation.
                                    NumberOfGamesInParallel = 1
                                    PreventMoveDeviation = true }
                            printfn "  running %d-game self-play at %s..." p.Rounds (tourny.TimeControlTextForPlayer engA.TimeControlID)
                            runTournament tourny logger ct
                        if ct.IsCancellationRequested then
                            printfn "  stopped: not fitted; the next run plays it on from its PGN"
                        elif not played then
                            failures <- failures + 1
                            Cli.Output.error $"{baseName}: the self-play did not run through"
                        else
                        let netRows = Collections.Generic.List<string>()
                        match pvOutcomeEndgame pgn with
                        | Some (n, b, r, q) ->
                            printfn "  outcome endgame:  P 1.00  N %s  B %s  R %s  Q %s" (fmt n) (fmt b) (fmt r) (fmt q)
                            netRows.Add(sprintf "%s,outcome,%.2f,%.2f,%.2f,%.2f\n" baseName n b r q)
                        | None -> printfn "  outcome endgame:  (insufficient data)"
                        match pvPgnEvalEndgame pgn with
                        | Some (n, b, r, q) ->
                            printfn "  pgneval endgame:  P 1.00  N %s  B %s  R %s  Q %s" (fmt n) (fmt b) (fmt r) (fmt q)
                            netRows.Add(sprintf "%s,pgneval,%.2f,%.2f,%.2f,%.2f\n" baseName n b r q)
                        | None -> printfn "  pgneval endgame:  (insufficient data)"
                        rows.[baseName] <- List.ofSeq netRows
                        writeSummary ()
                    with ex ->
                        failures <- failures + 1
                        Cli.Output.error $"{baseName}: {ex.Message}"
            host.StopAsync().Wait()
            writeSummary ()
            printfn "\nSummary: %s\n%s" summaryPath (File.ReadAllText summaryPath)
            if failures = 0 then Cli.Output.ExitOk else Cli.Output.ExitFailed

/// `book eval`: one engine with its own limit, or none for the run's default.
type BookEngineArg =
    { Config: TypesDef.CoreTypes.EngineConfig
      Limit: TimeControlTypes.TimeControlCommands.SearchLimit option }

type BookParams =
    { Input: string
      Engines: BookEngineArg list
      DefaultLimit: TimeControlTypes.TimeControlCommands.SearchLimit
      MinEval: string
      MaxEval: string
      MaxDiff: string
      Count: int option
      Out: string }

/// Book Evaluation from the command line: the GUI page's run (ChessLibrary BookEvaluation). The
/// first Ctrl+C stops and writes what passed. Exit 1 when an engine stopped the run.
let runBookEval (p: BookParams) (ct: CancellationToken) =
    let ext = Path.GetExtension(p.Input).ToLowerInvariant()
    let engines =
        p.Engines |> List.map (fun e -> ({ Config = e.Config; Limit = defaultArg e.Limit p.DefaultLimit } : ChessLibrary.BookEvaluation.BookEngine))
    let filter =
        try ChessLibrary.BookEvaluation.parseFilter p.MinEval p.MaxEval p.MaxDiff engines.Length
        with :? FormatException as e -> raise (Cli.Parse.Usage $"--min-eval, --max-eval, --max-diff: {e.Message}")
    let label = ChessLibrary.BookEvaluation.engineLabel engines
    let take (xs: seq<'a>) = match p.Count with Some n -> Seq.truncate n xs | None -> xs
    printfn "Book evaluation: %s" p.Input
    printfn "Engines: %s" label
    printfn "Filter: |eval| %g-%g pawns%s" (filter.MinEval / 100.0) (filter.MaxEval / 100.0) (if engines.Length > 1 then sprintf ", spread below %g pawns" (filter.MaxDiff / 100.0) else "")
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable lastShown = 0
    let progress = Action<int, int>(fun done' total ->
        // one line per 5 % (and the last), not one per position
        if done' = total || done' * 20 / total > lastShown * 20 / total then
            lastShown <- done'
            eprintf "\r%d / %d positions (%.0fs)   " done' total sw.Elapsed.TotalSeconds)
    let passed, evaluated, removed, skipped, failure, cancelled, total =
        if ext = ".pgn" then
            let games = ChessLibrary.FullPGNParser.parsePgnFileWithRaw p.Input |> take |> Seq.toList
            let o = ChessLibrary.BookEvaluation.evaluatePgns engines filter games progress ct
            ChessLibrary.BookEvaluation.writePgns p.Out o.Results
            o.Results.Count, o.Evaluated, o.Removed, o.Skipped, o.Failure, o.Cancelled, games.Length
        else
            let epds = ChessLibrary.EPDExtractor.readEPDs p.Input |> take |> Seq.toList
            let o = ChessLibrary.BookEvaluation.evaluateEpds engines filter epds progress ct
            ChessLibrary.BookEvaluation.writeEpds p.Out o.Results
            o.Results.Count, o.Evaluated, o.Removed, o.Skipped, o.Failure, o.Cancelled, epds.Length
    eprintfn ""
    // the page's Results table, row for row (BookEvaluation.summaryRows)
    let rows =
        ChessLibrary.BookEvaluation.summaryRows (Path.GetFileName p.Input) total passed evaluated removed skipped
            failure cancelled engines p.MinEval p.MaxEval p.MaxDiff sw.Elapsed p.Out
    for label, value in rows do
        printfn "  %-31s %s" label value
    match failure with
    | Some f ->
        Cli.Output.error $"{f}"
        Cli.Output.ExitFailed
    | None -> Cli.Output.ExitOk
// ---------------------------------------------------------------- commands

open Cli.Spec
open Cli.Parse
open Cli.App

[<Literal>]
let Group = "Puzzles, books, tournaments and tuning"

let private configArg what = arg "config" what

let puzzleRunSpec =
    { command [ "puzzle"; "run" ] Group "Run a Lichess puzzle test from a config" with
        Description =
          Some "Runs the Lichess puzzle test of a puzzle config (PuzzleConfig.md): the tables on stdout, the result files in its FailedPuzzlesOutputFolder (LichessSummary_<stamp>.txt and .json, failed puzzles, per-theme tables). --json prints the result document of PuzzleJsonSchema.md as the envelope's result. The first Ctrl+C keeps the rating groups that finished and writes no .json summary, so puzzle trend never reads a partial run. Exit 1 when an engine failed or nothing was scored."
        Args = [ configArg "a puzzle config (JSON)" ]
        Options = [ json ]
        Examples =
          [ "eb-cli puzzle run PuzzleConfig.json", "The test, with the tables"
            "eb-cli puzzle run PuzzleConfig.json --json > result.json", "The result for a script" ] }

let puzzleRun (p: Parsed) =
    let path = Cli.Groups.inputFile "puzzle config" (argOf p "config").Value
    runPuzzles path (has p "json") (Cli.Output.onCtrlC ())

let puzzleEretSpec =
    { command [ "puzzle"; "eret" ] Group "Run an ERET test suite (EPD best-move positions) from a config" with
        Description = Some "Runs the ERET test of an ERET config (EretPuzzles.md); the failed puzzles and the summary go to its FailedPuzzlesOutputFolder. Exit 1 when an engine failed."
        Args = [ configArg "an ERET config (JSON)" ]
        Options = [ json ]
        Examples = [ "eb-cli puzzle eret EretConfig.json", "The test" ] }

let puzzleEret (p: Parsed) =
    runEretTest (Cli.Groups.inputFile "ERET config" (argOf p "config").Value) (has p "json") (Cli.Output.onCtrlC ())

let puzzleTrendSpec =
    { command [ "puzzle"; "trend" ] Group "Puzzle scores across a net's training checkpoints, from saved runs" with
        Description =
          Some "Reads the LichessSummary_<stamp>.json of every puzzle run in the folder and lines each arm's checkpoints up by training step. Series with fewer steps than --min-steps are hidden (most nets are one-offs). The folder: the argument, else puzzleResultsFolder in eb-cli.json."
        Args = [ optionalArg "folder" "the folder of puzzle results" ]
        Options =
          [ value "arm" "TEXT" "Only arms whose name contains this"
            value "type" "TEXT" "Only test types that contain this (policy, value)"
            value "rating-group" "N" "Only this rating group"
            value "min-steps" "N" "Hide series with fewer steps" |> withDefault "3"
            out "Also write the rows as CSV"
            force
            json ]
        Examples =
          [ "eb-cli puzzle trend C:/Dev/Chess/EB --arm deg4", "One arm's curve"
            "eb-cli puzzle trend --type policy -o trend.csv", "Every arm's policy rows, to a CSV" ] }

let puzzleTrend (p: Parsed) =
    let folder =
        match argOf p "folder" with
        | Some f -> f
        | None ->
            match CliSettings.current.Force() with
            | Result.Ok { PuzzleResultsFolder = Some f } -> f
            | Result.Ok _ -> raise (Usage $"""give the folder, or put {{ "puzzleResultsFolder": "<folder>" }} in {CliSettings.path ()}""")
            | Result.Error e -> raise (Usage $"cannot read {e}")
    let full = Cli.Groups.fullPath folder
    if not (Directory.Exists full) then raise (Usage $"folder not found: {folder}")
    let csv = valueOf p "out" |> Option.map (Cli.Groups.outputFile p)
    let minSteps = defaultArg (intOf p "min-steps") 3
    let all = ChessLibrary.PuzzleTrend.loadFolder full
    let contains (needle: string option) (haystack: string) =
        match needle with
        | Some n -> haystack.Contains(n, StringComparison.OrdinalIgnoreCase)
        | None -> true
    let rg = intOf p "rating-group"
    let filtered, dropped =
        all
        |> List.filter (fun r -> contains (valueOf p "arm") r.Arm && contains (valueOf p "type") r.Type && (match rg with Some g -> r.RatingGroup = g | None -> true))
        |> ChessLibrary.PuzzleTrend.dedupe
        |> ChessLibrary.PuzzleTrend.filterMinSteps minSteps
    if dropped > 0 then Cli.Output.info $"Hiding {dropped} series with fewer than {minSteps} steps (--min-steps 1 shows all)"
    csv |> Option.iter (fun path -> File.WriteAllText(path, ChessLibrary.PuzzleTrend.toCsv filtered); Cli.Output.info $"CSV written: {path}")
    if has p "json" then
        Cli.Output.printJson (Cli.Output.envelope (name p.Command) true (box filtered) [])
    elif List.isEmpty all then
        Cli.Output.warn $"no puzzle result JSON in {full} (runs write LichessSummary_<stamp>.json beside the text summary)"
    elif List.isEmpty filtered then
        Cli.Output.warn $"no rows matched ({all.Length} checkpoint rows read)"
    else
        let arms = filtered |> List.map (fun r -> r.Arm) |> List.distinct
        printfn "%d checkpoint rows across %d arm(s) from %s" filtered.Length arms.Length full
        printfn "%s" (ChessLibrary.PuzzleTrend.render filtered)
    Cli.Output.ExitOk

let bookEvalSpec =
    { command [ "book"; "eval" ] Group "Filter an opening book down to balanced openings, checked by engines" with
        Description =
          Some "Every engine searches the end position of each opening; an opening is kept when every eval is inside --min-eval..--max-eval (pawns, whichever side is better; the output is from White's view) and the engines are within --max-diff of each other. The kept openings are written as PGN or EPD, like the input. The first Ctrl+C stops and writes what passed. Exit 1 when an engine stopped the run."
        Args = [ arg "book" "an opening book (.pgn or .epd)" ]
        Options =
          [ value "engine" "ENGINE[,nodes=N|,movetime=MS]" "An engine, with its own limit after a comma if it has one; at least one" |> repeatable
            value "nodes" "N" "The limit of every engine without its own" |> withDefault "10000"
            value "movetime" "MS" "A time limit for every engine without its own, instead of nodes"
            value "min-eval" "PAWNS" "The smallest |eval| kept" |> withDefault "0.8"
            value "max-eval" "PAWNS" "The largest |eval| kept" |> withDefault "1.0"
            value "max-diff" "PAWNS" "The largest spread between the engines" |> withDefault "0.4"
            value "count" "N" "Only the first N openings"
            out "The output (default BookEvals/BookEval_<book>_<engines>.<ext> next to the book)"
            force ]
        OneOf = [ [ "nodes"; "movetime" ] ]
        Examples =
          [ "eb-cli book eval book.pgn --engine sf.json --engine lc0.json,nodes=2000", "Two engines, lc0 with its own limit"
            "eb-cli book eval book.epd --engine sf.json --movetime 300 --min-eval 0.3 --max-eval 1.2 -o balanced.epd", "A wider window, to a file of its own" ] }

let bookEval (p: Parsed) =
    let input = Cli.Groups.inputFile "book" (argOf p "book").Value
    let ext = Path.GetExtension(input).ToLowerInvariant()
    if ext <> ".pgn" && ext <> ".epd" then raise (Usage $"a book is .pgn or .epd, not {ext}")
    let limitText (text: string) =
        match text.Split('=') with
        | [| "nodes"; n |] -> (match Int32.TryParse n with | true, v when v > 0 -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit v | _ -> raise (Usage $"--engine: bad nodes '{n}'"))
        | [| "movetime"; ms |] -> (match Int32.TryParse ms with | true, v when v > 0 -> TimeControlTypes.TimeControlCommands.SearchLimit.TimeLimit v | _ -> raise (Usage $"--engine: bad movetime '{ms}'"))
        | _ -> raise (Usage $"--engine: '{text}' is not nodes=N or movetime=MS")
    let engines =
        match valuesOf p "engine" with
        | [] -> raise (Usage "give at least one --engine")
        | given ->
            given
            |> List.map (fun e ->
                let name, limit =
                    match e.LastIndexOf ',' with
                    | i when i > 0 && (e.Substring(i + 1).StartsWith "nodes=" || e.Substring(i + 1).StartsWith "movetime=") -> e.Substring(0, i), Some(limitText (e.Substring(i + 1)))
                    | _ -> e, None
                { Config = Cli.Groups.engineFrom p name "uci"; Limit = limit })
    let defaultLimit =
        match intOf p "movetime", intOf p "nodes" with
        | Some ms, _ -> TimeControlTypes.TimeControlCommands.SearchLimit.TimeLimit ms
        | None, Some n -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit n
        | None, None -> TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit 10000
    let label = ChessLibrary.BookEvaluation.engineLabel (engines |> List.map (fun e -> ({ Config = e.Config; Limit = defaultArg e.Limit defaultLimit } : ChessLibrary.BookEvaluation.BookEngine)))
    let out =
        match valueOf p "out" with
        | Some o -> Cli.Groups.outputFile p o
        | None -> Cli.Groups.outputFile p (Path.Combine(Path.GetDirectoryName input, "BookEvals", sprintf "BookEval_%s_%s%s" (Path.GetFileName input) label ext))
    runBookEval
        { Input = input
          Engines = engines
          DefaultLimit = defaultLimit
          MinEval = defaultArg (valueOf p "min-eval") "0.8"
          MaxEval = defaultArg (valueOf p "max-eval") "1.0"
          MaxDiff = defaultArg (valueOf p "max-diff") "0.4"
          Count = intOf p "count"
          Out = out }
        (Cli.Output.onCtrlC ())

/// The tournament of a config, with its engines read: None when the file is not a tournament.
let private loadTournament (path: string) =
    // the parse error is the usage error's message (readTournamentJson printed it on stdout)
    match JSON.tryReadTournamentJson path with
    | Result.Error message -> raise (Usage message)
    | Result.Ok tourny ->
        // tablebase adjudication without a folder: eb-cli's own (syzygyPath, EB_SYZYGY_PATH)
        let tourny = JSON.withAppTablebaseFolder Cli.Groups.appSyzygyFolder tourny
        let engineList = JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList
        Some(tourny, engineList)

let tournamentRunSpec =
    { command [ "tournament"; "run" ] Group "Run a tournament from a config" with
        Description =
          Some "Plays the tournament of a tournament config (TournamentConfig.md): round robin, gauntlet, Swiss, cup or ladder, one line per game, the table at the end; the games go to its PgnOutPath. A tournament resumes from the games already in its PGN. The first Ctrl+C stops the games in progress. A config with errors (what tournament validate reports) is refused with exit 2, as the WebGUI refuses to start it. Exit 1 when the run failed. --json: at the end, the games of this run (players, result, how it ended, moves, time; the moves are in the PGN) and the standings of the whole PGN, in the shape of pgn elo --json."
        Args = [ configArg "a tournament config (JSON)" ]
        Options =
          [ json ]
        Examples =
          [ "eb-cli tournament run tournament.json", "Play it (or go on with it)"
            "eb-cli tournament run tournament.json --json > result.json", "The games and the standings for a script" ] }

let tournamentRun (p: Parsed) =
    let path = Cli.Groups.inputFile "tournament config" (argOf p "config").Value
    match loadTournament path with
    | None -> raise (Usage $"not a tournament config: {path}")
    | Some(tourny, engineList) ->
        if not (String.IsNullOrWhiteSpace tourny.OrdoExePath) then
            Cli.Output.info "OrdoExePath is not used any more: the ratings are EngineBattle's own"
        if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) && tourny.Challengers > 0 then
            for engine in engineList |> List.truncate tourny.Challengers do engine.IsChallenger <- true
        else
            for engine in engineList do engine.IsChallenger <- false
        let tournament =
            { tourny with
                EngineSetup = { tourny.EngineSetup with Engines = engineList }
                MinMoveTimeInMS = 0
                ConsoleOnly = true
                DelayBetweenGames = TimeSpan.Zero }
        // the GUI refuses to start a config with errors; so does the console
        let checkable = { tournament with Challengers = (if tournament.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) then tournament.Challengers else 0) }
        match Validation.validateTournament checkable with
        | Validation.Errors messages ->
            // with --json the errors are in the envelope, not only on stderr
            if has p "json" then raise (Usage ("the config has errors: " + String.Join("; ", messages)))
            for m in messages do Cli.Output.error m
            raise (Usage "the config has errors (above); 'eb-cli tournament validate' checks it without running")
        | _ -> ()
        if tournament.TournamentMode.Equals("Ladder", StringComparison.OrdinalIgnoreCase) then
            printfn "Ladder mode: %d engines, %d game pairs per match" engineList.Length (if obj.ReferenceEquals(tournament.LadderOptions, null) then 4 else tournament.LadderOptions.GamePairsPerMatch)
        for warning in Validation.duplicateOpeningWarnings tournament do
            Cli.Output.warn warning
        // before the pondering check, which starts engines too
        let ct = Cli.Output.onCtrlC ()
        // AllowPondering: every engine started once first, to see which can ponder
        let enginesOk = not tournament.AllowPondering || (let ok, _, _ = Tournament.TournamentUtils.checkPonderingEngines tournament in ok)
        if not enginesOk then
            let reason = "AllowPondering is set, but not every engine can ponder"
            if has p "json" then
                Cli.Output.printJson (Cli.Output.envelope (name p.Command) false null [ ({ Code = "pondering"; Message = reason } : Cli.Output.JsonError) ])
            Cli.Output.error reason
            Cli.Output.ExitFailed
        else
            use host = createHost ()
            host.Start()
            let loggerFactory = host.Services.GetService(typeof<ILoggerFactory>) :?> ILoggerFactory
            let outcome = runTournamentOutcome tournament (loggerFactory.CreateLogger "EngineBattle Console logger") ct
            host.StopAsync().Wait()
            if has p "json" then
                let games =
                    [ for i, r in List.indexed outcome.Results ->
                        {| Game = i + 1; White = r.Player1; Black = r.Player2; Result = r.Result; Ending = r.Ending
                           Moves = r.Moves; Seconds = Math.Round(float r.GameTime / 1000.0, 1) |} ]
                let result =
                    {| Name = tournament.Name; Mode = tournament.ModeLabel(); Config = path; Pgn = tournament.PgnOutPath
                       Seconds = Math.Round(outcome.Duration.TotalSeconds, 1)
                       Parallel = tournament.NumberOfGamesInParallel
                       Standings = (match outcome.Standings with Some (players, cross, results) -> box (Commands.Pgn.standingsJsonOf None results players cross) | None -> null)
                       Games = games |}
                let errors =
                    [ match outcome.Failure with
                      | Some reason -> ({ Code = "failed"; Message = reason } : Cli.Output.JsonError)
                      | None -> ()
                      if ct.IsCancellationRequested then
                          ({ Code = "interrupted"; Message = "stopped with Ctrl+C: the games in progress are not counted" } : Cli.Output.JsonError) ]
                Cli.Output.printJson (Cli.Output.envelope (name p.Command) (outcome.Succeeded && not ct.IsCancellationRequested) (box result) errors)
            if outcome.Succeeded then Cli.Output.ExitOk else Cli.Output.ExitFailed

let tournamentValidateSpec =
    { command [ "tournament"; "validate" ] Group "Check a tournament config without running it" with
        Description = Some "Reads the config and its engine defs and checks the key settings; warnings for settings that will not do what they seem to. Exit 1 when the config has errors."
        Args = [ configArg "a tournament config (JSON)" ]
        Options = [ json ]
        Examples = [ "eb-cli tournament validate tournament.json", "Is it ready to run?" ] }

let tournamentValidate (p: Parsed) =
    let path = Cli.Groups.inputFile "tournament config" (argOf p "config").Value
    match loadTournament path with
    | None -> raise (Usage $"not a tournament config: {path}")
    | Some(tourny, engineList) ->
        // checked as eb-cli runs it: the WebGUI's own hints (the delay between games) are not eb-cli's
        let t = { tourny with EngineSetup = { tourny.EngineSetup with Engines = engineList }; ConsoleOnly = true }
        let errors = match Validation.validateTournament t with Validation.Errors e -> e | _ -> []
        if has p "json" then
            let result = {| Config = path; Valid = errors.IsEmpty; Warnings = Validation.tournamentWarnings t |}
            let listed = [ for e in errors -> ({ Code = "invalid-config"; Message = e } : Cli.Output.JsonError) ]
            Cli.Output.printJson (Cli.Output.envelope (name p.Command) errors.IsEmpty (box result) listed)
        else Validation.validateTournamentInput t
        if errors.IsEmpty then Cli.Output.ExitOk else Cli.Output.ExitFailed

let tuneRunSpec =
    { command [ "tune"; "run" ] Group "Tune engine parameters (Bayesian optimisation)" with
        Description = Some "Runs the tuner config (ConsoleTuner.md): Bayesian optimisation with SPRT matches, phase by phase; the state, history, best options and the dashboard go to its OutputDir. \"Resume\": true in the config goes on from the saved state. Ctrl+C stops at once."
        Args = [ configArg "a tuner config (JSON)" ]
        Examples = [ "eb-cli tune run tuner-config.json", "Tune" ] }

let tuneRun (p: Parsed) =
    let path = Cli.Groups.inputFile "tuner config" (argOf p "config").Value
    // the tuner has no point to stop at between its matches: Ctrl+C ends it at once
    Console.CancelKeyPress.Add(fun e ->
        e.Cancel <- true
        Cli.Output.warn "tuning stopped; \"Resume\": true in the config goes on from the saved state"
        exit Cli.Output.ExitInterrupted)
    ConsoleApp.BayesianOptimizer.runTuneWithDispatch path
    Cli.Output.ExitOk

let tuneDashboardSpec =
    { command [ "tune"; "dashboard" ] Group "Write the tuning dashboard again from the saved state" with
        Description = Some "Writes bo-dashboard.html in the config's OutputDir again from tune-state.json, without running anything."
        Args = [ configArg "the tuner config of the run" ]
        Examples = [ "eb-cli tune dashboard tuner-config.json", "The dashboard again" ] }

let tuneDashboard (p: Parsed) =
    ConsoleApp.BayesianOptimizer.regenerateDashboard (Cli.Groups.inputFile "tuner config" (argOf p "config").Value)
    Cli.Output.ExitOk

let valuesBatchSpec =
    { command [ "piece-values"; "batch" ] Commands.Positions.Group "Piece values for a folder of nets" with
        Description =
          Some "For every .onnx in the folder: self-play at the tournament's time control with the tournament's first engine and that net, then the outcome and pgn-eval endgame fits; summary.csv lists them. A net whose PGN already has the rounds is not played again. The summary keeps the rows of nets the run did not reach. The first Ctrl+C stops the net being played without fitting it; the next run plays it on from its PGN."
        Args = [ arg "tournament" "a tournament config: its first engine plays with each net, at its time control"; arg "nets" "a folder of .onnx networks" ]
        Options =
          [ value "rounds" "N" "Self-play games per net" |> withDefault "1000"
            value "out-dir" "DIR" "Where the PGNs and summary.csv go (default <nets>/pv_batch)" ]
        Examples = [ "eb-cli piece-values batch tournament.json C:/Nets/arm --rounds 400", "400 games per net" ] }

let valuesBatch (p: Parsed) =
    let template = Cli.Groups.inputFile "tournament config" (argOf p "tournament").Value
    let netsGiven = (argOf p "nets").Value
    let nets = Cli.Groups.fullPath netsGiven
    if not (Directory.Exists nets) then raise (Usage $"net folder not found: {netsGiven}")
    let rounds =
        match intOf p "rounds" with
        | Some r when r < 1 -> raise (Usage $"--rounds must be at least 1, got {r}")
        | Some r -> r
        | None -> 1000
    let outDir = valueOf p "out-dir" |> Option.map Cli.Groups.fullPath |> Option.defaultValue (Path.Combine(nets, "pv_batch"))
    runPvBatch { Template = template; NetFolder = nets; Rounds = rounds; OutDir = outDir } (Cli.Output.onCtrlC ())

let entries: Entry list =
    [ { Spec = puzzleRunSpec; Run = puzzleRun }
      { Spec = puzzleEretSpec; Run = puzzleEret }
      { Spec = puzzleTrendSpec; Run = puzzleTrend }
      { Spec = bookEvalSpec; Run = bookEval }
      { Spec = tournamentRunSpec; Run = tournamentRun }
      { Spec = tournamentValidateSpec; Run = tournamentValidate }
      { Spec = tuneRunSpec; Run = tuneRun }
      { Spec = tuneDashboardSpec; Run = tuneDashboard }
      { Spec = valuesBatchSpec; Run = valuesBatch } ]