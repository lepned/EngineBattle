namespace ConsoleApp

open System
open System.IO
open System.Text
open System.Text.Json
open System.Collections.Generic
open System.Linq
open ChessLibrary.Engine
open ChessLibrary.Utilities.Formatting
open ChessLibrary.Utilities.Regex
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Misc

module BenchmarkRunner =

  [<CLIMutable>]
  type BenchmarkPosition =
    { mutable Name: string
      mutable Fen: string }

  [<CLIMutable>]
  type BenchmarkOptionSet =
    { mutable OptionKey: string
      mutable Values: JsonElement[] }

  [<CLIMutable>]
  type BenchmarkConfig =
    { mutable EngineConfigPath: string
      mutable DurationSeconds: int
      mutable Positions: BenchmarkPosition[]
      mutable OptionSets: BenchmarkOptionSet[]
      mutable SummaryOutputPath: string }

  type SearchStats =
    { Depth: int
      Eval: EvalType
      Nodes: int64
      Nps: int64
      Eps: int64
      PV: string
      TBHits: int64
      WDL: WDL option
      SDepth: int
      MPV: int }

  type ComboResult =
    { ComboDesc: string
      PositionName: string
      Fen: string
      Time: TimeSpan
      Stats: SearchStats }

  type CombinationSummary =
    { ComboDesc: string
      PositionCount: int
      AvgEps: double
      AvgNps: double
      Time : TimeSpan }

  let private defaultDuration = 20

  let private resolvePath (path: string) fieldName =
    if String.IsNullOrWhiteSpace path then
      failwithf "%s path is required" fieldName
    let full = Path.GetFullPath(path)
    if File.Exists full then full
    else failwithf "%s not found: %s" fieldName full

  let private loadConfig (path: string) : BenchmarkConfig =
    let normalized = resolvePath path "Benchmark config"
    let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true, AllowTrailingCommas = true)
    let config = JsonSerializer.Deserialize<BenchmarkConfig>(File.ReadAllText(normalized), options)
    if obj.ReferenceEquals(config, null) then failwith "Unable to parse benchmark config"
    config

  let private safePositions (positions: BenchmarkPosition[]) =
    if isNull positions || positions.Length = 0 then
      [| { Name = "start"; Fen = startPosition} |]
    else
      positions

  let private safeOptionSets (optionSets: BenchmarkOptionSet[]) =
    if isNull optionSets then [||]
    else optionSets

  let private optionValueToObj (element: JsonElement) =
    match element.ValueKind with
    | JsonValueKind.Number ->
        match element.TryGetInt64() with
        | true, value -> box value
        | _ -> box (element.GetDouble())
    | JsonValueKind.True -> box true
    | JsonValueKind.False -> box false
    | JsonValueKind.String -> box (element.GetString())
    | JsonValueKind.Array ->
        element
        |> fun e -> e.EnumerateArray()
        |> Seq.map (fun json -> json.GetRawText())
        |> Seq.toArray
        |> box
    | JsonValueKind.Null -> null
    | _ -> box (element.GetRawText())

  let private cloneEngineConfig (baseConfig: EngineConfig) =
    let newOptions = Dictionary<string, obj>(baseConfig.Options, StringComparer.OrdinalIgnoreCase)
    { baseConfig with Options = newOptions }

  let private applyCombination (options: Dictionary<string, obj>) (combo: Map<string, obj>) =
    for Key, value in combo |> Map.toSeq do
      if options.ContainsKey(Key) then
        options.[Key] <- if isNull value then null else value
      else
        options.Add(Key, if isNull value then null else value)

  let private safeOptionSetsForCombos optionSets =
    optionSets
    |> Array.filter (fun set -> not (String.IsNullOrWhiteSpace set.OptionKey) && not (isNull set.Values) && set.Values.Length > 0)

  let private describeValue (value: obj) =
    match value with
    | null -> "null"
    | :? string as text -> text
    | :? bool as b -> b.ToString().ToLowerInvariant()
    | :? int64 as value64 -> value64.ToString()
    | :? int as value32 -> value32.ToString()
    | :? double as dbl -> dbl.ToString("G")
    | _ -> value.ToString()

  let private describeCombination (combo: Map<string, obj>) =
    if combo.IsEmpty then "baseline"
    else
      combo
      |> Map.toList
      |> List.map (fun (k, v) -> sprintf "%s=%s" k (describeValue v))
      |> String.concat ", "

  let private buildCombinations (optionSets: BenchmarkOptionSet[]) =
    let sets = safeOptionSetsForCombos optionSets
    if sets.Length = 0 then
      [ Map.empty ]
    else
      let rec loop idx acc =
        if idx = sets.Length then
          [ acc ]
        else
          [ for value in sets.[idx].Values do
              let objValue = optionValueToObj value
              let updated = acc |> Map.add sets.[idx].OptionKey objValue
              yield! loop (idx + 1) updated ]
      loop 0 Map.empty

  let private ensureDirectoryExists (path: string) =
    if String.IsNullOrWhiteSpace path then
      ()
    else
      let dir = Path.GetDirectoryName(path)
      if not (String.IsNullOrWhiteSpace dir) then
        Directory.CreateDirectory(dir) |> ignore

  let private resolveSummaryPath (config: BenchmarkConfig) =
    let desired = config.SummaryOutputPath
    let target =
      if String.IsNullOrWhiteSpace desired then
        let logDir = Path.Combine("logs")
        Directory.CreateDirectory(logDir) |> ignore
        Path.Combine(logDir, sprintf "benchmark-summary-%s.txt" (DateTime.Now.ToString("yyyyMMdd_HHmmss")))
      else
        Path.GetFullPath(desired)
    ensureDirectoryExists target
    target

  let private summarizeResults (results: ComboResult seq) =
    results
    |> Seq.groupBy (fun r -> r.ComboDesc)
    |> Seq.map (fun (combo, entries) ->
      let avgEps = entries |> Seq.averageBy (fun e -> float e.Stats.Eps)
      let avgNps = entries |> Seq.averageBy (fun e -> float e.Stats.Nps)
      let time  = entries |> Seq.averageBy (fun e -> e.Time.TotalSeconds)
      { ComboDesc = combo; PositionCount = Seq.length entries; AvgEps = avgEps; AvgNps = avgNps; Time = TimeSpan.FromSeconds time })

  let private pickBestCombination (summaries: seq<CombinationSummary>) =
    summaries
    |> Seq.sortWith (fun a b ->
      match compare b.AvgEps a.AvgEps with
      | 0 -> compare b.AvgNps a.AvgNps
      | x -> x)
    |> Seq.tryHead

  let private formatSummaryText (summaries: CombinationSummary list) (best: CombinationSummary option) =
    let sb = StringBuilder()
    sb.AppendLine("Benchmark summary") |> ignore
    sb.AppendLine("-----------------") |> ignore
    for summary in summaries do
      sb.AppendLine(sprintf "%s | Positions: %d | Avg EPS: %.1f | Avg NPS: %.1f | Duration(s): %.1f"
        summary.ComboDesc summary.PositionCount summary.AvgEps summary.AvgNps summary.Time.TotalSeconds) |> ignore
    match best with
    | Some best ->
      sb.AppendLine() |> ignore
      sb.AppendLine(sprintf "Best combination by EPS/NPS: %s (Avg EPS: %.1f, Avg NPS: %.1f)"
        best.ComboDesc best.AvgEps best.AvgNps) |> ignore
    | None ->
      sb.AppendLine() |> ignore
      sb.AppendLine("No valid search results recorded.") |> ignore
    sb.ToString()

  let private readSearchStats (engine: ChessEngine) (isWhite: bool) =
    let rec loop last =
      let line = engine.ReadLine()
      if isNull line then
        last
      else
        let trimmed = line.TrimStart()
        if trimmed.StartsWith("info", StringComparison.OrdinalIgnoreCase) then
          match getEssentialDataWithEPS trimmed isWhite with
          | Some (depth, eval, nodes, nps, eps, pv, tbhits, wdl, sDepth, mpv) ->
            loop (Some { Depth = depth
                         Eval = eval
                         Nodes = nodes
                         Nps = nps
                         Eps = eps
                         PV = pv
                         TBHits = tbhits
                         WDL = wdl
                         SDepth = sDepth
                         MPV = mpv })
          | None -> loop last
        elif trimmed.StartsWith("bestmove", StringComparison.OrdinalIgnoreCase) then
          last
        else
          loop last
    loop None

  let private logStats stats durationSeconds (dur: TimeSpan) =
    match stats with
    | None ->
      printfn "    [!] No info available after %ds search" durationSeconds
    | Some data ->
      let formattedNps = formatNPS (float data.Nps)
      let formattedEps = formatEPS (float data.Eps)
      let durMsg =
        if dur.TotalMinutes >= 1.0 then
          sprintf "%dm %ds" dur.Minutes dur.Seconds
        else
          sprintf "%.1fs" dur.TotalSeconds
      let formattedWdl =
        match data.WDL with
        | Some wdl -> sprintf "WDL=%d-%d-%d" (int wdl.Win) (int wdl.Draw) (int wdl.Loss)
        | None -> "WDL=N/A"
      
      printfn "    Time: %s | Depth: %d (SD: %d) | Eval: %s | TBHits: %d | %s" durMsg data.Depth data.SDepth (data.Eval.ToString()) data.TBHits formattedWdl
      printfn "    Nodes: %-10d | NPS: %-9s | EPS: %-9s" data.Nodes formattedNps formattedEps
      
      if not (String.IsNullOrWhiteSpace data.PV) && data.PV.Length > 0 then
        let pv = if data.PV.Length > 70 then data.PV.Substring(0, 67) + "..." else data.PV
        printfn "    PV: %s" pv

  let private runPosition (engine: ChessEngine) (position: BenchmarkPosition) durationSeconds comboDesc index totalPositions =
    let fen = if String.IsNullOrWhiteSpace position.Fen then startPosition else position.Fen.Trim()
    let board = ChessLibrary.Chess.Board()
    board.LoadFen fen
    let positionName = if String.IsNullOrWhiteSpace position.Name then sprintf "Position %d" (index + 1) else position.Name.Trim()
    let isWhite = board.Position.STM = 0uy
    
    printfn ""
    printfn "  [%d/%d] %s" (index + 1) totalPositions positionName
    printfn "    FEN: %s" fen
    
    engine.UciNewGame()
    let ok = engine.WaitForReadyOk(6000000)
    if not ok then
       failwith "Engine did not respond to readyok during benchmark"
    if fen.Equals("startpos", StringComparison.OrdinalIgnoreCase) then
      engine.Position("position startpos")
    else
      engine.PositionGoFen fen
    let sw = System.Diagnostics.Stopwatch.StartNew()
    sw.Start()
    let ms = durationSeconds * 1000    
    engine.Go ms
    let stats = readSearchStats engine isWhite
    sw.Stop()
    logStats stats durationSeconds sw.Elapsed
    match stats with
    | Some stats ->
      printfn "    Result: OK"
      Some { ComboDesc = comboDesc; PositionName = positionName; Fen = fen; Time = sw.Elapsed; Stats = stats }
    | None -> 
        printfn "    Result: FAILED - No valid search stats obtained"
        None

  let runBenchmark (configPath: string) =
    Console.OutputEncoding <- Encoding.UTF8
    let config = loadConfig configPath
    let durationSeconds = if config.DurationSeconds <= 0 then defaultDuration else config.DurationSeconds
    let positions = safePositions config.Positions
    let optionSets = safeOptionSets config.OptionSets
    let combos = buildCombinations optionSets
    let totalCombos = combos.Length
    let engineConfigPath = resolvePath config.EngineConfigPath "Engine config"
    let baselineConfig = ChessLibrary.Utilities.JSON.readSingleEngineConfig engineConfigPath    
    let engine = ChessLibrary.EngineHelper.createEngine(baselineConfig, None)
    engine.TryToUpdateOption "smartpruningfactor" "0"
    engine.TryToUpdateOption "moveoverhead" "0"
    
    printfn ""
    printfn "================================================================"
    printfn "                 BENCHMARK CONFIGURATION                        "
    printfn "================================================================"
    printfn "  Combinations: %d | Positions: %d | Duration: %ds per position" totalCombos positions.Length durationSeconds
    printfn "================================================================"
    
    let accumulatedResults = ResizeArray<ComboResult>()
    combos
    |> List.iteri (fun idx combo ->
      let comboDesc = describeCombination combo
      let headerText = sprintf "[%d/%d] Configuration: %s" (idx + 1) totalCombos comboDesc
      let separatorLength = max 64 (headerText.Length)
      let separator = String('-', separatorLength)
      
      printfn ""
      printfn "%s" separator
      printfn "%s" headerText
      printfn "%s" separator
      
      let engineConfig = cloneEngineConfig baselineConfig
      applyCombination engineConfig.Options combo      
      let options = engineConfig.Options |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))      
      for (k, v) in options do
        let value = v.ToString()
        if not (k.ToLower().Contains "smartpruningfactor") && not (k.ToLower().Contains "moveoverhead") then
            let opt = EngineOption.Create k value
            engine.AddSetOption(opt)
     
      try
        engine.UciNewGame()
        if not (engine.WaitForReadyOk(6000000)) then
          failwith "Engine did not respond to readyok before benchmark"        
        positions
        |> Array.iteri (fun posIdx position ->
            match runPosition engine position durationSeconds comboDesc posIdx positions.Length with
            | Some result -> accumulatedResults.Add result
            | None -> ()
        )
        
      finally
        printfn ""
        printfn "  Configuration complete."
        printfn "" )

    let summaries =
        query {
            for s in (accumulatedResults |> summarizeResults) do
            sortByDescending s.AvgEps
            thenByDescending s.AvgNps
            select s
        } |> Seq.toList

    engine.StopProcess()
    let best = pickBestCombination summaries
    let summaryText = formatSummaryText summaries best
    let summaryPath = resolveSummaryPath config
    File.WriteAllText(summaryPath, summaryText, Encoding.UTF8)
    printfn ""
    printfn "%s" summaryText
    printfn "Summary written to: %s" summaryPath
    printfn ""
