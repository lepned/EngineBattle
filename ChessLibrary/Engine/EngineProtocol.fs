module ChessLibrary.EngineProtocol

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

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

  // The regex implementations. Kept for two jobs: they are the fallback for any line the fast
  // parser below declines, and they are the reference the fast parser is tested against
  // (EngineProtocolParserTests, which runs both over a corpus of real engine output).
  let legacyGetEssentialDataWithEPS (line:string) isWhite =
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

  let legacyGetEssentialData (line:string) isWhite =
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

  // ── Fast info-line parser ──────────────────────────────────────────────────────────────────
  // One pass of IndexOf and digit reads instead of eleven regex matches: the analysis pages see
  // tens of thousands of these lines a minute from Lc0 and Ceres, and the regex version cost
  // ~1.5 us and ~4.5 KB per line - nearly all the allocation on that path.
  //
  // It reproduces the regexes exactly, quirks included, because callers have been living with
  // them: each field is the FIRST place its pattern matches (so `depth` can land in a `seldepth`
  // that comes before it), the pv is everything after the LAST "pv<space>" following the first
  // "score" (so a line with no pv but a multipv yields the text after multipv), multipv needs a
  // word boundary on both sides. Anything it is not sure it reads the same way - a non-ASCII
  // character, a line break, a number long enough to overflow - goes to the regex version,
  // which then answers (or throws) exactly as before.

  exception private Decline

  /// .NET's \s for ASCII.
  let inline private isWs (c: char) = c = ' ' || (c >= '\t' && c <= '\r')
  let inline private isDigit (c: char) = c >= '0' && c <= '9'
  /// .NET's \w for ASCII.
  let inline private isWordChar (c: char) =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c || c = '_'

  /// Digits at `i` as int64, with the index after them; Decline for a run long enough to
  /// overflow (the regex path then parses, or throws, as it always did).
  let private readDigits (line: string) (i: int) (maxDigits: int) =
    let mutable j = i
    let mutable v = 0L
    while j < line.Length && isDigit line.[j] do
      if j - i >= maxDigits then raise Decline
      v <- v * 10L + int64 (int line.[j] - int '0')
      j <- j + 1
    v, j

  /// First match of `key\s+(\d+)` (or `key\s(\d+)` when exactlyOneSpace): the value and true, or
  /// 0 and false when the pattern matches nowhere.
  let private findNumber (line: string) (key: string) (exactlyOneSpace: bool) (maxDigits: int) =
    let mutable from = 0
    let mutable result = ValueNone
    while result.IsNone && from <= line.Length - key.Length do
      let k = line.IndexOf(key, from, StringComparison.Ordinal)
      if k < 0 then from <- line.Length
      else
        let mutable i = k + key.Length
        let wsStart = i
        if exactlyOneSpace then
          if i < line.Length && isWs line.[i] then i <- i + 1
        else
          while i < line.Length && isWs line.[i] do i <- i + 1
        if i > wsStart && i < line.Length && isDigit line.[i] then
          let v, _ = readDigits line i maxDigits
          result <- ValueSome v
        else
          from <- k + 1
    match result with
    | ValueSome v -> v, true
    | ValueNone -> 0L, false

  /// `\bmultipv\s+(\d+)\b`
  let private findMultiPv (line: string) =
    let key = "multipv"
    let mutable from = 0
    let mutable result = ValueNone
    while result.IsNone && from <= line.Length - key.Length do
      let k = line.IndexOf(key, from, StringComparison.Ordinal)
      if k < 0 then from <- line.Length
      else
        let boundaryBefore = k = 0 || not (isWordChar line.[k - 1])
        let mutable i = k + key.Length
        let wsStart = i
        while i < line.Length && isWs line.[i] do i <- i + 1
        if boundaryBefore && i > wsStart && i < line.Length && isDigit line.[i] then
          let v, j = readDigits line i 9
          // \b after a maximal digit run: the next character must not be a word character.
          if j = line.Length || not (isWordChar line.[j]) then result <- ValueSome v
          else from <- k + 1
        else
          from <- k + 1
    match result with
    | ValueSome v -> int v
    | ValueNone -> 0

  /// `score\s+(cp|mate)\s+(-?\d+)`: the raw value as the regex path would parse it.
  let private findScore (line: string) =
    let mutable from = 0
    let mutable result = NA
    let mutable fin = false
    while not fin && from <= line.Length - 5 do
      let k = line.IndexOf("score", from, StringComparison.Ordinal)
      if k < 0 then fin <- true
      else
        let mutable i = k + 5
        let ws1 = i
        while i < line.Length && isWs line.[i] do i <- i + 1
        let kind =
          if i = ws1 || i >= line.Length then 0
          elif String.CompareOrdinal(line, i, "cp", 0, 2) = 0 then 2
          elif String.CompareOrdinal(line, i, "mate", 0, 4) = 0 then 4
          else 0
        if kind = 0 then from <- k + 1
        else
          i <- i + kind
          let ws2 = i
          while i < line.Length && isWs line.[i] do i <- i + 1
          let neg = i < line.Length && line.[i] = '-' && i + 1 < line.Length && isDigit line.[i + 1]
          let d = if neg then i + 1 else i
          if i > ws2 && d < line.Length && isDigit line.[d] then
            if kind = 2 then
              let v, _ = readDigits line d 15
              // float of the matched text: "-0" is -0.0, as Double.Parse makes it.
              result <- CP (if neg then -(float v) else float v)
            else
              let v, _ = readDigits line d 9
              result <- Mate (if neg then -(int v) else int v)
            fin <- true
          else
            from <- k + 1
    result

  /// `score.*pv\s(.*)` trimmed at the end: after the LAST "pv<whitespace>" that follows the
  /// first "score".
  let private findPv (line: string) =
    let s = line.IndexOf("score", StringComparison.Ordinal)
    if s < 0 then ""
    else
      let mutable i = line.LastIndexOf("pv", StringComparison.Ordinal)
      let mutable result = null
      while isNull result && i >= s + 5 do
        if i + 2 < line.Length && isWs line.[i + 2] then result <- line.Substring(i + 3).TrimEnd()
        elif i = 0 then i <- -1
        else i <- line.LastIndexOf("pv", i - 1, StringComparison.Ordinal)
      if isNull result then "" else result

  /// `wdl\s+(\d+)\s+(\d+)\s+(\d+)`
  let private findWdl (line: string) =
    let mutable from = 0
    let mutable result = None
    let mutable fin = false
    while not fin && from <= line.Length - 3 do
      let k = line.IndexOf("wdl", from, StringComparison.Ordinal)
      if k < 0 then fin <- true
      else
        let mutable i = k + 3
        let values = Array.zeroCreate<int64> 3
        let mutable ok = true
        let mutable n = 0
        while ok && n < 3 do
          let ws = i
          while i < line.Length && isWs line.[i] do i <- i + 1
          if i > ws && i < line.Length && isDigit line.[i] then
            let v, j = readDigits line i 15
            values.[n] <- v
            i <- j
            n <- n + 1
          else ok <- false
        if ok then
          result <- Some { Win = float values.[0]; Draw = float values.[1]; Loss = float values.[2] }
          fin <- true
        else from <- k + 1
    result

  /// The fast path can read the line exactly as the regexes do.
  let private isPlainAscii (line: string) =
    let mutable ok = true
    let mutable i = 0
    while ok && i < line.Length do
      let c = line.[i]
      if c > '\u007f' || c = '\n' then ok <- false
      i <- i + 1
    ok

  let private essential (line: string) isWhite =
    let eval =
      match findScore line with
      | NA -> NA
      | CP eval ->
        let eval = if eval = -0.0 then 0.0 else eval
        (if isWhite then eval/100.0 else - eval / 100.0) |> CP
      | Mate m -> (if isWhite then m else -m) |> Mate
    if eval = NA then ValueNone
    else
      let depth, _ = findNumber line "depth" true 9
      let nodes, _ = findNumber line "nodes" false 18
      let nps, _ = findNumber line "nps" false 18
      let eps, _ = findNumber line "eps" false 18
      let tbhits, _ = findNumber line "tbhits" false 18
      let seldepth, _ = findNumber line "seldepth" true 9
      ValueSome (int depth, eval, nodes, nps, eps, findPv line, tbhits, findWdl line, int seldepth, findMultiPv line)

  /// Depth, eval (White's view), nodes, nps, eps, pv, tbhits, wdl, seldepth and multipv of an
  /// "info" line; None when the line has no score. Same answers as the regex version.
  let getEssentialDataWithEPS (line:string) isWhite =
    if not (line.StartsWith("info", StringComparison.Ordinal)) || not (isPlainAscii line) then
      legacyGetEssentialDataWithEPS line isWhite
    else
      try
        match essential line isWhite with
        | ValueSome r -> Some r
        | ValueNone -> None
      with Decline -> legacyGetEssentialDataWithEPS line isWhite

  /// As getEssentialDataWithEPS, without eps.
  let getEssentialData (line:string) isWhite =
    if not (line.StartsWith("info", StringComparison.Ordinal)) || not (isPlainAscii line) then
      legacyGetEssentialData line isWhite
    else
      try
        match essential line isWhite with
        | ValueSome (d, eval, nodes, nps, _, pv, tb, wdl, sd, mpv) -> Some (d, eval, nodes, nps, pv, tb, wdl, sd, mpv)
        | ValueNone -> None
      with Decline -> legacyGetEssentialData line isWhite

  //info string f8c5  (139 ) N:      19 (+ 0) (P:  0.75%) (WGT:      19.000) (WL: -0.99998)
  //(D: 0.000) (M: 63.2) (STD: 0.00000) (STDF: 1.00000) (VS: 0.99996) (E: 0.00388) (Q: -0.99998) (U: 0.57593) (S: -0.42405) (V:  -.----)

  let legacyGetInfoStringData player (line:string) =
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

  /// `info string\s+(\w+)`: the move a verbose-move-stats line is about.
  let private findStatMove (line: string) =
    let key = "info string"
    let mutable from = 0
    let mutable result = null
    while isNull result && from <= line.Length - key.Length do
      let k = line.IndexOf(key, from, StringComparison.Ordinal)
      if k < 0 then from <- line.Length
      else
        let mutable i = k + key.Length
        let ws = i
        while i < line.Length && isWs line.[i] do i <- i + 1
        let w = i
        while i < line.Length && isWordChar line.[i] do i <- i + 1
        if w > ws && i > w then result <- line.Substring(w, i - w)
        else from <- k + 1
    if isNull result then "" else result

  /// First match of `key\s+(-?\d+[.,]\d+)` (without the minus when not allowed), parsed as the
  /// regex path parses it: the matched text, comma turned to point, through `float`.
  let private findStatDecimal (line: string) (key: string) (allowMinus: bool) =
    let mutable from = 0
    let mutable result = ValueNone
    while result.IsNone && from <= line.Length - key.Length do
      let k = line.IndexOf(key, from, StringComparison.Ordinal)
      if k < 0 then from <- line.Length
      else
        let mutable i = k + key.Length
        let ws = i
        while i < line.Length && isWs line.[i] do i <- i + 1
        let start = i
        if allowMinus && i + 1 < line.Length && line.[i] = '-' && isDigit line.[i + 1] then i <- i + 1
        let intStart = i
        while i < line.Length && isDigit line.[i] do i <- i + 1
        if start > ws && i > intStart && i + 1 < line.Length && (line.[i] = '.' || line.[i] = ',') && isDigit line.[i + 1] then
          i <- i + 1
          while i < line.Length && isDigit line.[i] do i <- i + 1
          result <- ValueSome (float (line.Substring(start, i - start).Replace(',', '.')))
        else
          from <- k + 1
    match result with
    | ValueSome v -> v
    | ValueNone -> 0.0

  /// One verbose-move-stats line (Lc0 and Ceres "info string <move> ... N: ... P: ... Q: ...").
  /// Same answers as the regex version, which serves any line this one declines.
  let getInfoStringData player (line:string) =
    if not (isPlainAscii line) then legacyGetInfoStringData player line
    else
      try
        let nodes, _ = findNumber line "N:" false 18
        {
          Player = player
          LANMove = findStatMove line
          SANMove = String.Empty
          Nodes = nodes
          P = findStatDecimal line "P:" true
          Q = findStatDecimal line "Q:" true
          V = findStatDecimal line "V:" true
          E = findStatDecimal line "E:" false
          Raw = line
        }
      with Decline -> legacyGetInfoStringData player line
