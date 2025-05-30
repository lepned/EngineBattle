module CliParser

open System
open System.IO
open System.Text

// --- CLI definition using a discriminated union ---
type VerbResult =
    | Perft of depth:int * sampleSize:int
    | Analyze of fenOrFile:string
    | PuzzleJson of path:string
    | Tournament of configFile:string
    | Eret of configFile: string
    | GUI of page: string * port: int option


type CLIArguments =
    | Help 
    | Games of int
    | Rounds of int
    | Verb of VerbResult    


let createCombinedScoresTable
    (fileName: string)
    (policyScores: ChessLibrary.TypesDef.Puzzle.Score list)
    (valueScores: ChessLibrary.TypesDef.Puzzle.Score list) 
    (searchScores: ChessLibrary.TypesDef.Puzzle.Score list)=
    
    let sb = StringBuilder()
    sb.AppendLine("\n```\n") |> ignore
    sb.AppendLine(sprintf "Puzzle file name: %s\n" fileName) |> ignore

    // Combine all sets of scores
    let allScores = policyScores @ valueScores @ searchScores

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
            | "analyze" -> // Handle the Analyze verb
                if index + 1 < args.Length then
                    let fenOrFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Analyze fenOrFile) :: acc)
                else failwith "Missing parameter for Analyze"
            | "puzzlejson" -> // Handle the PuzzleFile verb
                if index + 1 < args.Length then
                    let puzzleFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (PuzzleJson puzzleFile) :: acc)
                else failwith "Missing parameter for Puzzlejson"
            | "eretjson" -> // Handle the eretjson verb
                if index + 1 < args.Length then
                    let eretFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Eret eretFile) :: acc)
                else failwith "Missing parameter for Eretjson"
            | "tournamentjson" -> // Handle the Tournament verb
                if index + 1 < args.Length then
                    let configFile = args.[index + 1]
                    parseArgs args (index + 2) (Verb (Tournament configFile) :: acc)
                else failwith "Missing parameter for Tournament" 
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

