module CliParser

open System
open System.Text

// --- CLI definition using a discriminated union ---
type AnalyzeParams =
    { Engine: string
      Fen: string
      Nodes: int option
      MoveTime: int option
      Depth: int option
      Args: string option
      Moves: string list
      UciOptions: (string * string) list
      ShowOptions: bool }

type CompareParams =
    { Engine1: string
      Engine2: string
      Fen: string
      PositionsFile: string option
      Nodes: int option
      MoveTime: int option
      Depth: int option
      Threshold: float option
      UciOptions1: (string * string) list
      UciOptions2: (string * string) list }

type VerbResult =
    | Perft of depth:int * sampleSize:int
    | Analyze of AnalyzeParams
    | Compare of CompareParams
    | PuzzleJson of path:string
    | Tournament of configFile:string
    | Eret of configFile: string
    | Benchmark of configFile:string
    | Tune of configFile:string
    | Redash of configFile:string
    | GUI of page: string * port: int option
    | PgnSummary of path:string
    | Validate of configFile:string
    | Elo of path:string
    | Speed of path:string


type CLIArguments =
    | Help
    | Games of int
    | Rounds of int
    | Verb of VerbResult


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
        maxOf "Avg rating"
          (allScores
           |> List.map (fun s -> s.RatingAvg.ToString("F0").Length))
    let maxThemeWidth =
        maxOf "Theme"
          (allScores |> List.map (fun s -> s.Filter.Length + 2))
    let maxNodesWidth =
        maxOf "Nodes" (allScores |> List.map (fun s -> s.Nodes.ToString().Length))

    // Build a header line formatter
    let headerLine =
        sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
          ("Engine".PadRight maxEngineWidth)
          ("Neural net".PadRight maxNeuralNetWidth)
          ("Perf".PadRight maxPerfWidth)
          ("Accuracy".PadRight maxAccuracyWidth)
          ("Total".PadRight maxTotalWidth)
          ("Avg rating".PadRight maxAvgRatingWidth)
          ("Theme".PadRight maxThemeWidth)
          ("Nodes".PadRight maxNodesWidth)

    let mutable startGroup =
        match policyScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0


    let mutable widestText = ""

    // Sum up all column widths and add a buffer for tab spacing
    let approximateWidth =
        let tabWidth = 6
        let columns = 8
        maxEngineWidth + maxNeuralNetWidth + maxPerfWidth + maxAccuracyWidth +
        maxTotalWidth + maxAvgRatingWidth + maxThemeWidth + maxNodesWidth +
        (columns - 1) * tabWidth
        //20 // Buffer for spacing between columns

    let separatorLine = String.replicate approximateWidth "-"

    // Append policy Tests
    if policyScores.Length > 0 then
        sb.AppendLine("Policy Head Tests\n") |> ignore
        sb.AppendLine(headerLine) |> ignore
    policyScores
    |> List.iter (fun s ->
        let perf = s.PlayerRecord.Rating.ToString("F0")
        let accuracy = (decimal s.Correct / decimal s.TotalNumber).ToString("P1")
        let line =
            sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
                (s.Engine.PadRight maxEngineWidth)
                (s.NeuralNet.PadRight maxNeuralNetWidth)
                (perf.PadRight maxPerfWidth)
                (accuracy.PadRight maxAccuracyWidth)
                (s.TotalNumber.ToString().PadRight maxTotalWidth)
                (s.RatingAvg.ToString("F0").PadRight maxAvgRatingWidth)
                (s.Filter.PadRight maxThemeWidth)
                (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append Value Head Tests
    if valueScores.Length > 0 then
        sb.AppendLine("Value Head Tests\n") |> ignore
        sb.AppendLine(headerLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match valueScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0
    // Append each value score row
    valueScores
    |> List.iter (fun s ->
        let perf = s.PlayerRecord.Rating.ToString("F0")
        let accuracy = (decimal s.Correct / decimal s.TotalNumber).ToString("P1")
        let line =
            sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
                (s.Engine.PadRight maxEngineWidth)
                (s.NeuralNet.PadRight maxNeuralNetWidth)
                (perf.PadRight maxPerfWidth)
                (accuracy.PadRight maxAccuracyWidth)
                (s.TotalNumber.ToString().PadRight maxTotalWidth)
                (s.RatingAvg.ToString("F0").PadRight maxAvgRatingWidth)
                (s.Filter.PadRight maxThemeWidth)
                (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append search Tests
    if searchScores.Length > 0 then
        sb.AppendLine("Search Tests\n") |> ignore
        sb.AppendLine(headerLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match searchScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0

    // Append each search score row
    searchScores
    |> List.iter (fun s ->
        let perf = s.PlayerRecord.Rating.ToString("F0")
        let accuracy = (decimal s.Correct / decimal s.TotalNumber).ToString("P1")
        let line =
            sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
                (s.Engine.PadRight maxEngineWidth)
                (s.NeuralNet.PadRight maxNeuralNetWidth)
                (perf.PadRight maxPerfWidth)
                (accuracy.PadRight maxAccuracyWidth)
                (s.TotalNumber.ToString().PadRight maxTotalWidth)
                (s.RatingAvg.ToString("F0").PadRight maxAvgRatingWidth)
                (s.Filter.PadRight maxThemeWidth)
                (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.AppendLine() |> ignore

    // Append solve Tests
    if solveScores.Length > 0 then
        sb.AppendLine("Solve Tests\n") |> ignore
        sb.AppendLine(headerLine) |> ignore

    widestText <- ""
    let mutable startGroup =
        match solveScores |> List.tryHead with
        | Some s -> s.RatingAvg
        | None -> 0

    // Append each solve score row
    solveScores
    |> List.iter (fun s ->
        let perf = s.PlayerRecord.Rating.ToString("F0")
        let accuracy = (decimal s.Correct / decimal s.TotalNumber).ToString("P1")
        let line =
            sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
                (s.Engine.PadRight maxEngineWidth)
                (s.NeuralNet.PadRight maxNeuralNetWidth)
                (perf.PadRight maxPerfWidth)
                (accuracy.PadRight maxAccuracyWidth)
                (s.TotalNumber.ToString().PadRight maxTotalWidth)
                (s.RatingAvg.ToString("F0").PadRight maxAvgRatingWidth)
                (s.Filter.PadRight maxThemeWidth)
                (s.Nodes.ToString().PadRight maxNodesWidth)
        widestText <- if widestText.Length < line.Length then line else widestText
        if s.RatingAvg <> startGroup then
            startGroup <- s.RatingAvg
            sb.AppendLine(separatorLine) |> ignore
        sb.AppendLine(line) |> ignore)
    sb.Append("\n```\n") |> ignore
    sb.ToString()


module CustomParser =
    
    let private parseInt (s : string) =
        match Int32.TryParse s with
        | (true, value) -> value
        | _ -> failwithf "Unable to parse integer from '%s'" s

    let rec private parseArgs (args: string[]) index (acc: CLIArguments list) =
        if index >= args.Length then List.rev acc
        else
            let arg = args.[index].ToLower()
            match arg with
            | "h" | "help" ->
                parseArgs args (index + 1) (Help :: acc)
            | "perft" -> // Handle the PERFT verb
                if index + 1 < args.Length then
                    let mutable nextIndex = index + 2
                    let depth = parseInt args.[index + 1]
                    let sampleSize =
                        if index + 2 < args.Length then
                            nextIndex <- index + 3
                            parseInt args.[index + 2]
                        else
                            printfn "Missing sample size for PERFT. Using default value of 10."
                            10
                    parseArgs args (nextIndex) (Verb (Perft (depth, sampleSize)) :: acc)
                else failwith "Missing parameter for PERFT"
            | "analyze" | "a" -> // Handle the Analyze verb
                if index + 1 < args.Length then
                    let engine = args.[index + 1]
                    // FEN is optional — if next arg is missing or starts with --, default to startpos
                    let mutable fen, startIdx =
                        if index + 2 < args.Length && not (args.[index + 2].StartsWith("--")) then
                            args.[index + 2], index + 3
                        else
                            "startpos", index + 2
                    let mutable i = startIdx
                    let mutable nodes = None
                    let mutable movetime = None
                    let mutable depth = None
                    let mutable engineArgs = None
                    let mutable moves = []
                    let mutable uciOptions = []
                    let mutable showOptions = false
                    while i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--nodes" -> nodes <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--movetime" -> movetime <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--depth" -> depth <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--fen" -> fen <- args.[i + 1]; i <- i + 2
                        | "--args" -> engineArgs <- Some args.[i + 1]; i <- i + 2
                        | "--moves" ->
                            i <- i + 1
                            while i < args.Length && not (args.[i].StartsWith("--")) do
                                moves <- args.[i] :: moves; i <- i + 1
                            moves <- List.rev moves
                        | "--uci" -> uciOptions <- (args.[i + 1], args.[i + 2]) :: uciOptions; i <- i + 3
                        | "--options" -> showOptions <- true; i <- i + 1
                        | unknown -> failwithf "Unknown analyze option: %s" unknown
                    let p = { Engine = engine; Fen = fen; Nodes = nodes; MoveTime = movetime
                              Depth = depth; Args = engineArgs; Moves = moves
                              UciOptions = List.rev uciOptions; ShowOptions = showOptions }
                    parseArgs args i (Verb (Analyze p) :: acc)
                else failwith "Missing parameter for Analyze (requires: <engine>)"
            | "compare" | "cmp" -> // Handle the Compare verb
                if index + 2 < args.Length then
                    let engine1 = args.[index + 1]
                    let engine2 = args.[index + 2]
                    let mutable i = index + 3
                    let mutable fen = "startpos"
                    let mutable positions = None
                    let mutable nodes = None
                    let mutable movetime = None
                    let mutable depth = None
                    let mutable threshold = None
                    let mutable uci1 = []
                    let mutable uci2 = []
                    while i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--fen" -> fen <- args.[i + 1]; i <- i + 2
                        | "--positions" -> positions <- Some args.[i + 1]; i <- i + 2
                        | "--nodes" -> nodes <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--movetime" -> movetime <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--depth" -> depth <- Some (parseInt args.[i + 1]); i <- i + 2
                        | "--threshold" ->
                            threshold <- Some (Double.Parse(args.[i + 1], System.Globalization.CultureInfo.InvariantCulture))
                            i <- i + 2
                        | "--uci1" -> uci1 <- (args.[i + 1], args.[i + 2]) :: uci1; i <- i + 3
                        | "--uci2" -> uci2 <- (args.[i + 1], args.[i + 2]) :: uci2; i <- i + 3
                        | unknown -> failwithf "Unknown compare option: %s" unknown
                    let p = { Engine1 = engine1; Engine2 = engine2; Fen = fen
                              PositionsFile = positions; Nodes = nodes; MoveTime = movetime
                              Depth = depth; Threshold = threshold
                              UciOptions1 = List.rev uci1; UciOptions2 = List.rev uci2 }
                    parseArgs args i (Verb (Compare p) :: acc)
                else failwith "Missing parameters for Compare (requires: <engine1> <engine2>)"
            | "puzzlejson" | "puzzle" | "p" -> // Handle the PuzzleFile verb
                if index + 1 < args.Length then
                    let puzzleFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (PuzzleJson puzzleFile) :: acc)
                else failwith "Missing parameter for Puzzlejson"
            | "eretjson" | "eret" -> // Handle the eretjson verb
                if index + 1 < args.Length then
                    let eretFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Eret eretFile) :: acc)
                else failwith "Missing parameter for Eretjson"
            | "tournamentjson" | "tournament" | "t" -> // Handle the Tournament verb
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Tournament configFile) :: acc)
                else failwith "Missing parameter for Tournament" 
            | "benchmark" | "bench" | "b" -> // Handle the Benchmark verb
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Benchmark configFile) :: acc)
                else failwith "Missing parameter for Benchmark"
            | "tune" -> // Handle the Tune verb
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Tune configFile) :: acc)
                else failwith "Missing parameter for Tune"
            | "redash" -> // Handle the Redash verb
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Redash configFile) :: acc)
                else failwith "Missing parameter for Redash"
            | "pgnsummary" | "pgn" | "ps" ->
                if index + 1 < args.Length then
                    let path = args.[index + 1]
                    parseArgs args (index + 2) (Verb (PgnSummary path) :: acc)
                else failwith "Missing parameter for pgnsummary"
            | "validate" | "v" ->
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Validate configFile) :: acc)
                else failwith "Missing parameter for Validate"
            | "elo" | "e" ->
                if index + 1 < args.Length then
                    let path = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Elo path) :: acc)
                else failwith "Missing parameter for Elo"
            | "speed" | "sp" ->
                if index + 1 < args.Length then
                    let path = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Speed path) :: acc)
                else failwith "Missing parameter for Speed"
            | "gui" ->
                // Accept: gui <page> <port> | gui <port> | gui <page>
                let nextArg idx = if idx < args.Length then Some args.[idx] else None
                match nextArg (index + 1), nextArg (index + 2) with
                | Some a, Some b when System.Int32.TryParse(b) |> fst ->
                    // gui <page> <port>
                    let page = a
                    let port = parseInt b
                    parseArgs args (index + 3) (Verb (GUI (page, Some port)) :: acc)
                | Some a, _ when System.Int32.TryParse(a) |> fst ->
                    // gui <port>
                    let port = parseInt a
                    parseArgs args (index + 2) (Verb (GUI ("tournament", Some port)) :: acc)
                | Some a, _ ->
                    // gui <page>
                    parseArgs args (index + 2) (Verb (GUI (a, None)) :: acc)
                | None, _ ->
                    // gui
                    parseArgs args (index + 1) (Verb (GUI ("tournament", None)) :: acc)
            | "--games" -> // Handle the --games flag
                if index + 1 < args.Length then
                    let games = parseInt args.[index + 1]
                    parseArgs args (index + 2) (Games games :: acc)
                else failwith "Missing parameter for --games"
            | "--rounds" -> // Handle the --rounds flag
                if index + 1 < args.Length then
                    let rounds = parseInt args.[index + 1]
                    parseArgs args (index + 2) (Rounds rounds :: acc)
                else failwith "Missing parameter for --rounds"
            | unknown -> // Handle unknown arguments
                printfn "Unknown argument: %s" unknown
                parseArgs args (index + 1) acc


    let parse (args: string[]) : CLIArguments list =
        parseArgs args[1..] 0 []

