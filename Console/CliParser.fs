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

type PieceValuesParams =
    { Engine: string
      Fen: string
      Moves: string list
      Nodes: int option
      MoveTime: int option
      Depth: int option
      UciOptions: (string * string) list }

type PieceValueFitParams =
    { Engine: string
      Positions: string
      Sample: int option
      Nodes: int option
      ByPhase: bool
      Outcome: bool
      PgnEval: bool
      Player: string option
      Bootstrap: int option
      UciOptions: (string * string) list }

type PvBatchParams =
    { Template: string
      NetFolder: string
      Rounds: int option
      Out: string option }

type VerbResult =
    | Perft of depth:int * sampleSize:int
    | Analyze of AnalyzeParams
    | Compare of CompareParams
    | PieceValues of PieceValuesParams
    | PieceValueFit of PieceValueFitParams
    | PvBatch of PvBatchParams
    | PvCombo of pgn:string
    | PuzzleJson of path:string * jsonOut:string option
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
    // edits are ordered (op, arg) pairs applied to the FEN before querying:
    // ("set","e4=Q") | ("remove","d2") | ("stm","b") | ("castling","KQ") | ("ep","e3")
    // emitEpd prints an EPD line (with the epdOps "name=value" opcodes) instead of JSON;
    // svgPath additionally writes an insights-overlay board SVG to the given file
    | Query of fen:string * square:string option * epd:string option * pv:string option * edits:(string * string) list * emitEpd:bool * epdOps:(string * string) list * svgPath:string option


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


module CustomParser =
    
    let private parseInt (s : string) =
        match Int32.TryParse s with
        | (true, value) -> value
        | _ -> failwithf "Unable to parse integer from '%s'" s

    /// Bounds-checked flag-value access: "a engine --nodes" prints a usage error
    /// instead of IndexOutOfRangeException
    let private valueOf (args: string[]) i =
        if i + 1 < args.Length then args.[i + 1]
        else failwithf "Missing value for option %s" args.[i]

    let private twoValuesOf (args: string[]) i =
        if i + 2 < args.Length then args.[i + 1], args.[i + 2]
        else failwithf "Missing name/value pair for option %s" args.[i]

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
            | "query" | "q" -> // Position-query verb: JSON insights/attacks for a FEN (validator use)
                if index + 1 < args.Length then
                    if args.[index + 1] = "--epd" then
                        if index + 2 < args.Length then
                            parseArgs args (index + 3) (Verb (Query ("", None, Some args.[index + 2], None, [], false, [], None)) :: acc)
                        else failwith "Missing EPD file after query --epd"
                    else
                        let fen =
                            if args.[index + 1].Equals("startpos", StringComparison.OrdinalIgnoreCase) then
                                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                            else args.[index + 1]
                        let square, afterSquare =
                            if index + 2 < args.Length && args.[index + 2].Length = 2 && not (args.[index + 2].StartsWith("--")) then
                                Some args.[index + 2], index + 3
                            else None, index + 2
                        // Flag loop: --pv, the board-editing flags and the EPD-writer
                        // flags, any order; edits and --op repeatable.
                        let mutable i = afterSquare
                        let mutable pv = None
                        let mutable emitEpd = false
                        let mutable svgPath = None
                        let edits = ResizeArray<string * string>()
                        let epdOps = ResizeArray<string * string>()
                        let mutable parsing = true
                        while parsing && i < args.Length do
                            let flag = args.[i]
                            let editOp =
                                match flag with
                                | "--setpiece" -> Some "set"
                                | "--remove" -> Some "remove"
                                | "--stm" -> Some "stm"
                                | "--castling" -> Some "castling"
                                | "--ep" -> Some "ep"
                                | _ -> None
                            if flag = "--emit-epd" then
                                emitEpd <- true
                                i <- i + 1
                            elif flag = "--pv" || flag = "--op" || flag = "--svg" || editOp.IsSome then
                                if i + 1 >= args.Length then failwithf "Missing value after %s" flag
                                match editOp with
                                | Some op -> edits.Add(op, args.[i + 1])
                                | None when flag = "--op" ->
                                    let v = args.[i + 1]
                                    let eq = v.IndexOf '='
                                    if eq <= 0 then failwithf "bad --op '%s' (expected name=value)" v
                                    epdOps.Add(v.Substring(0, eq), v.Substring(eq + 1))
                                | None when flag = "--svg" -> svgPath <- Some args.[i + 1]
                                | None -> pv <- Some args.[i + 1]
                                i <- i + 2
                            else parsing <- false
                        if epdOps.Count > 0 && not emitEpd then failwith "--op requires --emit-epd"
                        parseArgs args i (Verb (Query (fen, square, None, pv, List.ofSeq edits, emitEpd, List.ofSeq epdOps, svgPath)) :: acc)
                else failwith "Missing parameter for query: <fen|startpos> [square] [--pv \"<san|uci>\"] [--setpiece sq=P] [--remove sq] [--stm w|b] [--castling s] [--ep sq] [--emit-epd [--op name=value]...] [--svg out.svg] | --epd <file>"
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
                        | "--nodes" -> nodes <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--movetime" -> movetime <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--depth" -> depth <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--fen" -> fen <- valueOf args i; i <- i + 2
                        | "--args" -> engineArgs <- Some (valueOf args i); i <- i + 2
                        | "--moves" ->
                            i <- i + 1
                            while i < args.Length && not (args.[i].StartsWith("--")) do
                                moves <- args.[i] :: moves; i <- i + 1
                            moves <- List.rev moves
                        | "--uci" -> uciOptions <- (twoValuesOf args i) :: uciOptions; i <- i + 3
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
                        | "--fen" -> fen <- valueOf args i; i <- i + 2
                        | "--positions" -> positions <- Some (valueOf args i); i <- i + 2
                        | "--nodes" -> nodes <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--movetime" -> movetime <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--depth" -> depth <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--threshold" ->
                            threshold <- Some (Double.Parse(valueOf args i, System.Globalization.CultureInfo.InvariantCulture))
                            i <- i + 2
                        | "--uci1" -> uci1 <- (twoValuesOf args i) :: uci1; i <- i + 3
                        | "--uci2" -> uci2 <- (twoValuesOf args i) :: uci2; i <- i + 3
                        | unknown -> failwithf "Unknown compare option: %s" unknown
                    let p = { Engine1 = engine1; Engine2 = engine2; Fen = fen
                              PositionsFile = positions; Nodes = nodes; MoveTime = movetime
                              Depth = depth; Threshold = threshold
                              UciOptions1 = List.rev uci1; UciOptions2 = List.rev uci2 }
                    parseArgs args i (Verb (Compare p) :: acc)
                else failwith "Missing parameters for Compare (requires: <engine1> <engine2>)"
            | "piecevalues" | "pv" | "values" -> // Handle the PieceValues verb
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
                    let mutable moves = []
                    let mutable uciOptions = []
                    while i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--nodes" -> nodes <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--movetime" -> movetime <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--depth" -> depth <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--fen" -> fen <- valueOf args i; i <- i + 2
                        | "--moves" ->
                            i <- i + 1
                            while i < args.Length && not (args.[i].StartsWith("--")) do
                                moves <- args.[i] :: moves; i <- i + 1
                            moves <- List.rev moves
                        | "--uci" -> uciOptions <- (twoValuesOf args i) :: uciOptions; i <- i + 3
                        | unknown -> failwithf "Unknown piecevalues option: %s" unknown
                    let p = { Engine = engine; Fen = fen; Moves = moves
                              Nodes = nodes; MoveTime = movetime; Depth = depth
                              UciOptions = List.rev uciOptions }
                    parseArgs args i (Verb (PieceValues p) :: acc)
                else failwith "Missing parameter for PieceValues (requires: <engine>)"
            | "piecevaluefit" | "pvfit" -> // Handle the PieceValueFit verb (regression)
                if index + 2 < args.Length then
                    let engine = args.[index + 1]
                    let positions = args.[index + 2]
                    let mutable i = index + 3
                    let mutable sample = None
                    let mutable nodes = None
                    let mutable byPhase = false
                    let mutable outcome = false
                    let mutable pgnEval = false
                    let mutable player = None
                    let mutable bootstrap = None
                    let mutable uciOptions = []
                    while i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--sample" -> sample <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--nodes" -> nodes <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--by-phase" | "--byphase" -> byPhase <- true; i <- i + 1
                        | "--outcome" -> outcome <- true; i <- i + 1
                        | "--pgneval" -> pgnEval <- true; i <- i + 1
                        | "--player" -> player <- Some (valueOf args i); i <- i + 2
                        | "--bootstrap" | "--ci" -> bootstrap <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--uci" -> uciOptions <- (twoValuesOf args i) :: uciOptions; i <- i + 3
                        | unknown -> failwithf "Unknown pvfit option: %s" unknown
                    let p = { Engine = engine; Positions = positions; Sample = sample
                              Nodes = nodes; ByPhase = byPhase; Outcome = outcome; PgnEval = pgnEval
                              Player = player; Bootstrap = bootstrap; UciOptions = List.rev uciOptions }
                    parseArgs args i (Verb (PieceValueFit p) :: acc)
                else failwith "Missing parameters for PieceValueFit (requires: <engine> <positions.epd>)"
            | "pvbatch" -> // Batch piece-value analysis over a folder of nets
                if index + 2 < args.Length then
                    let template = args.[index + 1]
                    let netFolder = args.[index + 2]
                    let mutable i = index + 3
                    let mutable rounds = None
                    let mutable out = None
                    while i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--rounds" -> rounds <- Some (parseInt (valueOf args i)); i <- i + 2
                        | "--out" -> out <- Some (valueOf args i); i <- i + 2
                        | unknown -> failwithf "Unknown pvbatch option: %s" unknown
                    parseArgs args i (Verb (PvBatch { Template = template; NetFolder = netFolder; Rounds = rounds; Out = out }) :: acc)
                else failwith "Missing parameters for pvbatch (requires: <templateTournament.json> <netFolder>)"
            | "pvcombo" -> // Contextual (absolute-signature) material-imbalance report
                if index + 1 < args.Length then
                    parseArgs args (index + 2) (Verb (PvCombo args.[index + 1]) :: acc)
                else failwith "Missing parameter for pvcombo (requires: <games.pgn>)"
            | "puzzlejson" | "puzzle" | "p" -> // Handle the PuzzleFile verb
                if index + 1 < args.Length then
                    let puzzleFile = args.[index + 1]
                    // Optional flags after the config arg. Currently: --json <path>
                    // Stops at the first unrecognized flag and lets the outer
                    // parser handle whatever comes next, preserving existing
                    // lenient behavior for unknown args.
                    let mutable i = index + 2
                    let mutable jsonOut = None
                    let mutable scanning = true
                    while scanning && i < args.Length && args.[i].StartsWith("--") do
                        match args.[i].ToLower() with
                        | "--json" when i + 1 < args.Length ->
                            jsonOut <- Some args.[i + 1]
                            i <- i + 2
                        | _ -> scanning <- false
                    parseArgs args i (Verb (PuzzleJson (puzzleFile, jsonOut)) :: acc)
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

