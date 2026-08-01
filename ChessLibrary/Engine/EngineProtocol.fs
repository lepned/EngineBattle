module ChessLibrary.EngineProtocol

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

open TypesDef.CoreTypes
open MiscTypes
open EngineTypes


module UciOptions =

  let uciToCommand (uciParameter: string) (value: string) : string option =
    let mapping =
        dict [
            "WeightsFile", "--weights"
            "Backend", "--backend"
            "BackendOptions", "--backend-opts"
            "Threads", "--threads"
            "NNCacheSize", "--nncache"
            "MinibatchSize", "--minibatch-size"
            "CPuct", "--cpuct"
            "CPuctExponent", "--cpuct-exponent"
            "CPuctExponentAtRoot", "--cpuct-exponent-at-root"
            "CPuctBase", "--cpuct-base"
            "CPuctFactor", "--cpuct-factor"
            "TwoFoldDraws", "--two-fold-draws"
            "VerboseMoveStats", "--verbose-move-stats"
            "FpuStrategy", "--fpu-strategy"
            "FpuValue", "--fpu-value"
            "CacheHistoryLength", "--cache-history-length"
            "PolicyTemperature", "--policy-softmax-temp"
            "MaxCollisionEvents", "--max-collision-events"
            "MaxCollisionVisits", "--max-collision-visits"
            "MaxCollisionVisitsScalingStart", "--max-collision-visits-scaling-start"
            "MaxCollisionVisitsScalingEnd", "--max-collision-visits-scaling-end"
            "MaxCollisionVisitsScalingPower", "--max-collision-visits-scaling-power"
            "OutOfOrderEval", "--out-of-order-eval"
            "MaxOutOfOrderEvalsFactor", "--max-out-of-order-evals-factor"
            "StickyEndgames", "--sticky-endgames"
            "SyzygyFastPlay", "--syzygy-fast-play"
            "MultiPV", "--multipv"
            "PerPVCounters", "--per-pv-counters"
            "ScoreType", "--score-type"
            "HistoryFill", "--history-fill"
            "MovesLeftMaxEffect", "--moves-left-max-effect"
            "MovesLeftThreshold", "--moves-left-threshold"
            "MovesLeftSlope", "--moves-left-slope"
            "MovesLeftConstantFactor", "--moves-left-constant-factor"
            "MovesLeftScaledFactor", "--moves-left-scaled-factor"
            "MovesLeftQuadraticFactor", "--moves-left-quadratic-factor"
            "MaxConcurrentSearchers", "--max-concurrent-searchers"
            "DrawScore", "--draw-score"
            "ContemptMode", "--contempt-mode"
            "Contempt", "--contempt"
            "WDLCalibrationElo", "--wdl-calibration-elo"
            "WDLEvalObjectivity", "--wdl-eval-objectivity"
            "WDLDrawRateReference", "--wdl-draw-rate-reference"
            "NodesPerSecondLimit", "--nps-limit"
            "TaskWorkers", "--task-workers"
            "MinimumProcessingWork", "--minimum-processing-work"
            "MinimumPickingWork", "--minimum-picking-work"
            "MinimumRemainingPickingWork", "--minimum-remaining-picking-work"
            "MinimumPerTaskProcessing", "--minimum-per-task-processing"
            "IdlingMinimumWork", "--idling-minimum-work"
            "ThreadIdlingThreshold", "--thread-idling-threshold"
            "CpuctUtilityStdevPrior", "--cpuct-utility-stdev-prior"
            "CpuctUtilityStdevScale", "--cpuct-utility-stdev-scale"
            "CpuctUtilityStdevPriorWeight", "--cpuct-utility-stdev-prior-weight"
            "UseVarianceScaling", "--use-variance-scaling"
            "MoveRuleBucketing", "--move-rule-bucketing"
            "ReportedNodes", "--reported-nodes"
            "UncertaintyWeightingCap", "--uncertainty-weighting-cap"
            "UncertaintyWeightingCoefficient", "--uncertainty-weighting-coefficient"
            "UncertaintyWeightingExponent", "--uncertainty-weighting-exponent"
            "UseUncertaintyWeighting", "--use-uncertainty-weighting"
            "EasyEvalWeightDecay", "--easy-eval-weight-decay"
            "CpuctUncertaintyMinFactor", "--cpuct-uncertainty-min-factor"
            "CpuctUncertaintyMaxFactor", "--cpuct-uncertainty-max-factor"
            "CpuctUncertaintyMinUncertainty", "--cpuct-uncertainty-min-uncertainty"
            "CpuctUncertaintyMaxUncertainty", "--cpuct-uncertainty-max-uncertainty"
            "UseJustFpuUncertainty", "--use-just-fpu-uncertainty"
            "UseCpuctUncertainty", "--use-cpuct-uncertainty"
            "DesperationMultiplier", "--desperation-multiplier"
            "DesperationLow", "--desperation-low"
            "DesperationHigh", "--desperation-high"
            "DesperationPriorWeight", "--desperation-prior-weight"
            "UseDesperation", "--use-desperation"
            "TopPolicyBoost", "--top-policy-boost"
            "TopPolicyNumBoost", "--top-policy-num-boost"
            "SearchSpinBackoff", "--search-spin-backoff"
            "ConfigFile", "--config"
            "SyzygyPath", "--syzygy-paths"
            "UCI_Chess960", "--chess960"
            "UCI_ShowWDL", "--show-wdl"
            "UCI_ShowMovesLeft", "--show-movesleft"
            "SmartPruningFactor", "--smart-pruning-factor"
            "SmartPruningMinimumBatches", "--smart-pruning-minimum-batches"
            "RamLimitMb", "--ramlimit-mb"
            "MoveOverheadMs", "--move-overhead"
            "TimeManager", "--time-manager"
            "LogFile", "--logfile"
        ]

    match mapping.TryGetValue uciParameter with
    | true, flag ->
        match Boolean.TryParse value with
        | true, v ->
              Some(sprintf "%s=%b" flag v)
        | false, _ ->
            if uciParameter.Contains "BackendOptions" then
              Some(sprintf "%s=\"%s\"" flag value)
            else
              Some(sprintf "%s=%s" flag value)
    | false, _ -> None


  let createCommandsFromConfig (config: EngineConfig) =
    let sb = StringBuilder()
    let append (s:string) = sb.Append (s + " ") |> ignore
    for option in config.Options do
      let mutable value = option.Value.ToString()
      let (ok,v) = Boolean.TryParse value
      if ok then
        value <- sprintf "%b" v
      match uciToCommand option.Key value with
      | Some k -> append k
      | None -> ()
    sb.ToString()


module Engine =

  let calcTopNn (nnValues : NNValues seq) =
    if nnValues |> Seq.length < 3 then
      None
    else
      let arr = nnValues |> Seq.toArray |> Array.rev |> Array.skip 1
      let nodes = arr |> Array.sortBy(fun e -> -e.Nodes)
      let qs = arr |> Array.sortBy(fun e -> -e.Q)
      let ps = arr |> Array.sortBy(fun e -> -e.P)
      (nodes[0].Nodes, nodes[1].Nodes, nodes[0].Q, qs[0].Q, nodes[0].P, ps[0].P) |> Some


  let createLC0BenchmarkString (config: EngineConfig) =
    let sb = StringBuilder()
    let append (s:string) = sb.Append (s + " ") |> ignore
    //append "& '"
    append config.Path
    //append "'"
    //append " benchmark"
    let options = UciOptions.createCommandsFromConfig config
    append options
    //append " --num-positions=1 --movetime=10000"
    sb.ToString()


module UCI =

  // Regular expression pattern to capture the option name and its default value
  let optionRegex = new Regex(@"option name (.*?) type.*?default (\S+)?", RegexOptions.Compiled)

  let extractOptionDefaults (uciOutputs: ResizeArray<string>) =
    let dict = new Dictionary<string, string>()

    uciOutputs //|> Seq.toList
    |> Seq.filter (fun s -> s.StartsWith("option"))
    |> Seq.iter (fun s ->
        let ismatch = optionRegex.Match(s)
        if ismatch.Success then
            let optionName = ismatch.Groups.[1].Value
            if ismatch.Groups.[2].Success then
                let value = ismatch.Groups.[2].Value  // This should be the second group.
                if not (String.IsNullOrWhiteSpace(value)) then
                    dict.Add(optionName, value)
        else ()
    )
    dict


  let createDefaultSetOptionCommandForName (dict: Dictionary<string, string>) (name: string) =
    let matchedKey =
        dict.Keys
        |> Seq.tryFind (fun key -> key.ToLower().Contains(name.ToLower()))

    match matchedKey with
    | Some key ->
        match dict.TryGetValue(key) with
        | (true, value) when not (String.IsNullOrWhiteSpace(value)) -> Some (sprintf "setoption name %s value %s" key value)
        | _ -> None
    | None -> None

  let getUCIOptionsAsync exePath = async {
    // Create a new process start info
    let psi = ProcessStartInfo()
    psi.FileName <- exePath
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    // Create the process and start it
    let proc = new Process()
    proc.StartInfo <- psi
    proc.Start() |> ignore

    // Send the 'uci' command
    let sw = proc.StandardInput
    sw.WriteLine("uci\n")

    // Read the output
    let output = ResizeArray<string>()
    let sr = proc.StandardOutput

    let rec readLinesAsync () = async {
        let! line = sr.ReadLineAsync() |> Async.AwaitTask
        if line <> "uciok" then
            if not (String.IsNullOrEmpty(line)) then
              output.Add(line)
            return! readLinesAsync ()
    }
    do! readLinesAsync()

    // Close the process
    proc.Kill()
    proc.Close()
    return output
    }

/// UCI info string parsing with compiled regex patterns
module Regex =
  //"info string c1h6  (69  ) N:       6 (+ 0) (P:  0.41%) (WL: -0.99587) (D: 0.003) (M: 60.0) (Q: -0.99587) (U: 1.12920) (S:  0.09888) (V: -0.9982) "

  let mPvRegex = new Regex(@"\bmultipv\s+(\d+)\b", RegexOptions.Compiled)
  let depthRegex = new Regex(@"depth\s(\d+)", RegexOptions.Compiled)
  let sDepthRegex = new Regex(@"seldepth\s(\d+)", RegexOptions.Compiled)
  let nodesRegex = new Regex(@"nodes\s+(\d+)", RegexOptions.Compiled)
  let npsRegex = new Regex(@"nps\s+(\d+)", RegexOptions.Compiled)
  let epsRegex = new Regex(@"eps\s+(\d+)", RegexOptions.Compiled)
  let pvRegex = new Regex(@"score.*pv\s(.*)", RegexOptions.Compiled)  //@"pv\s(.*)")
  let tbhitsRegex = new Regex("tbhits\s+(\d+)", RegexOptions.Compiled)
  let evalRegex = new Regex(@"score\s+(cp|mate)\s+(-?\d+)", RegexOptions.Compiled)
  let wdlRegex = new Regex(@"wdl\s+(\d+)\s+(\d+)\s+(\d+)", RegexOptions.Compiled)  //wdl 160 385 454
  let ponderRegex = new Regex(@"pd=([a-zA-Z0-9+#=-]+)", RegexOptions.Compiled)
  let evalWvRegex = new Regex(@"wv=([+-]?M?-?\d+(\.\d+)?)", RegexOptions.Compiled)
  let evalRegexAlt = new Regex(@"([+-]?\d+\.\d+)", RegexOptions.Compiled)

  let parseEvalRegexOption line isblack =
    let test = evalWvRegex.Match(line)
    if test.Success then
      let eval = test.Groups.[1].Value
      if eval.StartsWith("M") then
        // Parse the mate score as an integer
        let mateScore = System.Int32.Parse(eval.TrimStart('M'))
        let highMateScore = if mateScore > 0 then 999.0 else -999.0
        Some highMateScore
      elif eval.StartsWith("-M") then
        //let mateScore = System.Int32.Parse(eval.TrimStart('-').TrimStart('M'))
        Some -999.0
      else
        // Parse the regular score as a float
        Some(float eval)
    else
      let test2 = evalRegexAlt.Match(line)
      if test2.Success then
        let eval = test2.Groups.[1].Value
        if eval.StartsWith("M") then
          // Parse the mate score as an integer
          let mateScore = System.Int32.Parse(eval.TrimStart('M'))
          let maxScore = if mateScore > 0 then 999.0 else -999.0
          Some maxScore
        elif eval.StartsWith("-M") then
          Some -999.0
        else
          // Parse the regular score as a float
          let score = float eval
          if isblack && score <> 0.00 then
            Some (score * -1.0)
          else
            Some score
      else
        None

  let parsePonderMove line =
    let test = ponderRegex.Match(line)
    if test.Success then
      let ponder = test.Groups.[1].Value
      Some ponder
    else
      None

  let parseRegex myDefault format line (regex : Regex)  =
    let test = regex.Match(line)
    if test.Success then
      test.Groups[1].Value |> format
    else
      myDefault

  let parseWDL line =
    let test = wdlRegex.Match(line)
    if test.Success then
      let w = test.Groups[1].Value
      let d = test.Groups[2].Value
      let l = test.Groups[3].Value
      Some {Win=float w; Draw= float d; Loss= float l}
    else None

  let parseEvalRegex line =
    let test = evalRegex.Match(line)
    if test.Success then
      if test.Groups[1].Value.Contains("mate") then
        int test.Groups[2].Value |> Mate
      else
        float test.Groups[2].Value |> CP
    else
      NA

  let floatParser line regex = parseRegex 0.0 (fun x -> float (x.Replace(',', '.'))) line regex
  let evalParser line = parseEvalRegex line
  let intParser line regex = parseRegex 0 (fun x -> int x) line regex
  let int64Parser line regex = parseRegex 0L (fun x -> int64 x) line regex
  let stringParser line regex = parseRegex "" (fun x -> x.TrimEnd() ) line regex
  let wdlParser line = parseWDL line

  let move = new Regex("info string\s+(\w+)", RegexOptions.Compiled)
  let nodes = new Regex("N:\s+(\d+)", RegexOptions.Compiled)
  let p = new Regex(@"P:\s+(-?\d+[.,]\d+)", RegexOptions.Compiled)
  let q = new Regex(@"Q:\s+(-?\d+[.,]\d+)", RegexOptions.Compiled)
  let v = new Regex(@"V:\s+(-?\d+[.,]\d+)", RegexOptions.Compiled)
  let e = new Regex(@"E:\s+(\d+[.,]\d+)", RegexOptions.Compiled)

  /// True for an aspiration-window fail-high/fail-low line. Such a line reports a partial
  /// search: its score is a real bound ("at least"/"at most"), but its PV is typically cut
  /// to the single root move, because no PV is collected below a beta cutoff. Engines only
  /// print them on large searches, and if the search stops on one, that truncated PV is
  /// the last thing a GUI (or a PGN comment) would see.
  let isBoundLine (line: string) =
    line.Contains "lowerbound" || line.Contains "upperbound"

  let getEssentialDataWithEPS (line:string) isWhite =
    if line.StartsWith "info" then
      let eval =
        match evalParser line with
          |NA -> NA
          |CP eval ->
            let eval = if eval = -0.0 then 0.0 else eval
            (if isWhite then eval/100.0 else - eval / 100.0) |> CP
          |Mate m -> (if isWhite then m else -m) |> Mate
      if eval = NA then
        None
      else
        (intParser line depthRegex,
        eval,
        int64Parser line nodesRegex,
        int64Parser line npsRegex,
        int64Parser line epsRegex,
        stringParser line pvRegex,
        int64Parser line tbhitsRegex,
        wdlParser line,
        intParser line sDepthRegex,
        intParser line mPvRegex  ) |> Some
    else
      None

  let getEssentialData (line:string) isWhite =
    if line.StartsWith "info" then
      let eval =
        match evalParser line with
          |NA -> NA
          |CP eval ->
            let eval = if eval = -0.0 then 0.0 else eval
            (if isWhite then eval/100.0 else - eval / 100.0) |> CP
          |Mate m -> (if isWhite then m else -m) |> Mate
      if eval = NA then
        None
      else
        (intParser line depthRegex,
        eval,
        int64Parser line nodesRegex,
        int64Parser line npsRegex,
        stringParser line pvRegex,
        int64Parser line tbhitsRegex,
        wdlParser line,
        intParser line sDepthRegex,
        intParser line mPvRegex  ) |> Some
    else
      None

  //info string f8c5  (139 ) N:      19 (+ 0) (P:  0.75%) (WGT:      19.000) (WL: -0.99998)
  //(D: 0.000) (M: 63.2) (STD: 0.00000) (STDF: 1.00000) (VS: 0.99996) (E: 0.00388) (Q: -0.99998) (U: 0.57593) (S: -0.42405) (V:  -.----)

  let getInfoStringData player (line:string) =
    {
      Player = player
      LANMove = stringParser line move
      SANMove = String.Empty
      Nodes = int64Parser line nodes
      P = floatParser line p
      Q = floatParser line q
      V = floatParser line v
      E = floatParser line e
      Raw = line
    }
