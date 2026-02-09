namespace ConsoleApp

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary.Configuration
open ChessLibrary.MiscTypes
open ChessLibrary.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TimeControlTypes
open ChessLibrary.Statistics
open ChessLibrary.PGNTypes
open ChessLibrary.PuzzleTypes
open ChessLibrary.PuzzleRunners
open ChessLibrary.TypesDef.PuzzleInput
open ChessLibrary.TimeControlTypes

module TunerRunner =

  [<CLIMutable>]
  type TuneParameterDef =
    { mutable Name: string
      mutable Option: string
      mutable Min: float
      mutable Max: float
      mutable Step: float
      mutable Scale: string }

  [<CLIMutable>]
  type TunePhase =
    { mutable Name: string
      mutable Parameters: string[]
      mutable Iterations: int }

  [<CLIMutable>]
  type SprtConfig =
    { mutable Elo0: float
      mutable Elo1: float
      mutable Alpha: float
      mutable Beta: float
      mutable MinGames: int
      mutable MaxGames: int }

  [<CLIMutable>]
  type TuneConfig =
    { mutable EngineConfigPath: string
      mutable OpponentConfigPath: string
      mutable OpponentTargetNodes: int
      mutable BaseTournamentConfigPath: string
      mutable OutputDir: string
      mutable TargetNodes: int
      mutable ParallelGames: int
      mutable OpeningsPath: string
      mutable OpeningsPly: int
      mutable OpeningsTwice: bool
      mutable Seed: int
      mutable Resume: bool
      mutable MaxWallHours: float
      mutable MaxCandidates: int
      mutable PerturbationSize: float
      mutable Optimizer: string
      mutable InitialDesignSize: int
      mutable HypUpdateInterval: int
      mutable EvalMode: string
      mutable EvalConfigPath: string
      mutable Sprt: SprtConfig
      mutable GPUs: int[]
      mutable InitialDesignRadius: float
      mutable PreventOpponentDeviation: bool
      mutable MaxReferencePgnGames: int
      mutable UseOpponentForValidation: bool
      mutable Phases: TunePhase[]
      mutable Parameters: TuneParameterDef[] }

  [<CLIMutable>]
  type TuneState =
    { mutable PhaseIndex: int
      mutable IterationInPhase: int
      mutable GlobalIteration: int
      mutable CandidateCount: int
      mutable RandomCallsConsumed: int
      mutable X: float[]
      mutable BestX: float[]
      mutable StartedUtc: string
      mutable LastUpdatedUtc: string
      mutable ObservationsX: float[][]
      mutable ObservationsY: float[]
      mutable FinalValidationCompleted: bool }

  [<CLIMutable>]
  type MatchStats =
    { mutable Wins: int
      mutable Draws: int
      mutable Losses: int
      mutable Games: int
      mutable Llr: float
      mutable Elo: float
      mutable Decision: string
      mutable StoppedEarly: bool }

  [<CLIMutable>]
  type HistoryEntry =
    { mutable TimestampUtc: string
      mutable Phase: string
      mutable Iteration: int
      mutable Candidate: int
      mutable Ak: float
      mutable Ck: float
      mutable Winner: string
      mutable MatchDecision: string
      mutable Games: int
      mutable Wins: int
      mutable Draws: int
      mutable Losses: int
      mutable Llr: float
      mutable Elo: float
      mutable PredictedMean: float
      mutable PredictedStd: float
      mutable AcquisitionValue: float
      mutable Accuracy: float }

  type internal ResolvedParameter =
    { Def: TuneParameterDef
      OptionKey: string
      InitialValue: float
      ScaleIsLog: bool
      EmbeddedKey: string option }

  let private jsonOptionsIndented = JsonSerializerOptions(WriteIndented = true)
  let private jsonOptionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true, AllowTrailingCommas = true)

  let private nowUtc() = DateTime.UtcNow.ToString("O")

  let private failf fmt = Printf.kprintf failwith fmt

  let private resolvePath (path: string) fieldName =
    if String.IsNullOrWhiteSpace path then failf "%s is required" fieldName
    let full = Path.GetFullPath(path)
    if File.Exists full then full else failf "%s not found: %s" fieldName full

  let private ensureDir (path: string) =
    if String.IsNullOrWhiteSpace path then failwith "OutputDir is required"
    Directory.CreateDirectory(path) |> ignore

  let private readTuneConfig path =
    let normalized = resolvePath path "Tune config"
    let json = File.ReadAllText(normalized)
    let cfg = JsonSerializer.Deserialize<TuneConfig>(json, jsonOptionsRead)
    if obj.ReferenceEquals(cfg, null) then failwith "Unable to parse tune config"
    normalized, cfg

  let private tryToFloat (value: obj) : float option =
    match value with
    | null -> None
    | :? float as f -> Some f
    | :? decimal as m -> Some (float m)
    | :? int as i -> Some (float i)
    | :? int64 as i -> Some (float i)
    | :? uint32 as i -> Some (float i)
    | :? uint64 as i -> Some (float i)
    | :? string as s ->
        match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, n -> Some n
        | _ -> None
    | :? JsonElement as je ->
        match je.ValueKind with
        | JsonValueKind.Number ->
            match je.TryGetDouble() with
            | true, n -> Some n
            | _ -> None
        | JsonValueKind.String ->
            let s = je.GetString()
            match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, n -> Some n
            | _ -> None
        | _ -> None
    | _ -> None

  let private tryFindOptionKey (options: IDictionary<string,obj>) (name: string) =
    options.Keys
    |> Seq.tryFind (fun k -> k.Equals(name, StringComparison.OrdinalIgnoreCase))

  let parseEmbeddedValue (optionValue: string) (key: string) : float option =
    if String.IsNullOrEmpty optionValue then None
    else
      optionValue.Split([| '|'; ';' |])
      |> Array.tryPick (fun segment ->
        let idx = segment.IndexOf('=')
        if idx > 0 && segment.Substring(0, idx).Equals(key, StringComparison.OrdinalIgnoreCase) then
          match Double.TryParse(segment.Substring(idx + 1), Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
          | true, v -> Some v
          | _ -> None
        else None)

  let replaceEmbeddedValue (optionValue: string) (key: string) (newValue: float) : string =
    let formatted = newValue.ToString("G", Globalization.CultureInfo.InvariantCulture)
    // Use regex to split while preserving delimiters
    let pattern = @"([|;])"
    let parts = Text.RegularExpressions.Regex.Split(optionValue, pattern)
    // parts array alternates: segment, delimiter, segment, delimiter, ...
    // Example: "a|b;c" -> ["a", "|", "b", ";", "c"]
    let replaced =
      parts
      |> Array.map (fun part ->
        // Skip delimiters (single char that is | or ;)
        if part.Length = 1 && (part = "|" || part = ";") then part
        else
          let idx = part.IndexOf('=')
          if idx > 0 && part.Substring(0, idx).Equals(key, StringComparison.OrdinalIgnoreCase) then
            sprintf "%s=%s" (part.Substring(0, idx)) formatted
          else part)
    String.Concat(replaced)

  let private normalizeScale (s: string) =
    if String.IsNullOrWhiteSpace s then "linear"
    else s.Trim().ToLowerInvariant()

  let normalizeEvalMode (s: string) =
    if String.IsNullOrWhiteSpace s then "sprt"
    else s.Trim().ToLowerInvariant()

  let private validateConfig (cfg: TuneConfig) (engineCfg: EngineConfig) =
    if obj.ReferenceEquals(cfg.Sprt, null) then failwith "sprt block is required"
    if isNull cfg.Parameters || cfg.Parameters.Length = 0 then failwith "parameters must not be empty"
    if isNull cfg.Phases || cfg.Phases.Length = 0 then failwith "phases must not be empty"
    if cfg.TargetNodes <= 0 then failwith "targetNodes must be > 0"
    if cfg.ParallelGames <= 0 then failwith "parallelGames must be > 0"
    if cfg.MaxWallHours <= 0.0 then failwith "maxWallHours must be > 0"
    if cfg.MaxCandidates <= 0 then failwith "maxCandidates must be > 0"
    let evalMode = normalizeEvalMode cfg.EvalMode
    if evalMode <> "sprt" && evalMode <> "puzzle" && evalMode <> "eret" then
      failf "evalMode must be sprt|puzzle|eret, got: %s" evalMode
    if evalMode = "puzzle" || evalMode = "eret" then
      if String.IsNullOrWhiteSpace cfg.EvalConfigPath then
        failf "evalConfigPath is required when evalMode=%s" evalMode
      let full = Path.GetFullPath cfg.EvalConfigPath
      if not (File.Exists full) then
        failf "evalConfigPath not found: %s" full
    if cfg.Sprt.Alpha <= 0.0 || cfg.Sprt.Alpha >= 1.0 then failwith "sprt.alpha must be in (0,1)"
    if cfg.Sprt.Beta <= 0.0 || cfg.Sprt.Beta >= 1.0 then failwith "sprt.beta must be in (0,1)"
    if cfg.Sprt.MinGames < 1 then failwith "sprt.minGames must be >= 1"
    if cfg.Sprt.MaxGames < cfg.Sprt.MinGames then failwith "sprt.maxGames must be >= sprt.minGames"

    let nameSet = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    for p in cfg.Parameters do
      if String.IsNullOrWhiteSpace p.Name then failwith "parameter name is required"
      if not (nameSet.Add(p.Name)) then failf "duplicate parameter name: %s" p.Name
      if p.Min >= p.Max then failf "parameter %s: min must be < max" p.Name
      if p.Step <= 0.0 then failf "parameter %s: step must be > 0" p.Name
      let scale = normalizeScale p.Scale
      if scale <> "linear" && scale <> "log" then failf "parameter %s: scale must be linear|log" p.Name
      if scale = "log" && (p.Min <= 0.0 || p.Max <= 0.0) then failf "parameter %s: log scale requires min/max > 0" p.Name
      if not (String.IsNullOrEmpty p.Option) then
        match tryFindOptionKey engineCfg.Options p.Option with
        | None -> failf "parameter %s: option '%s' is missing in engine options" p.Name p.Option
        | Some k ->
            let v = string engineCfg.Options.[k]
            if parseEmbeddedValue v p.Name |> Option.isNone then
              failf "parameter %s: option '%s' does not contain embedded key '%s='" p.Name p.Option p.Name
      else
        if tryFindOptionKey engineCfg.Options p.Name |> Option.isNone then
          failf "parameter %s is missing in engine options" p.Name

    for ph in cfg.Phases do
      if String.IsNullOrWhiteSpace ph.Name then failwith "phase name is required"
      if ph.Iterations <= 0 then failf "phase %s: iterations must be > 0" ph.Name
      if isNull ph.Parameters || ph.Parameters.Length = 0 then failf "phase %s: parameters must not be empty" ph.Name
      for n in ph.Parameters do
        if not (nameSet.Contains n) then failf "phase %s references unknown parameter %s" ph.Name n

  let internal resolveParameters (cfg: TuneConfig) (engineCfg: EngineConfig) =
    cfg.Parameters
    |> Array.map (fun p ->
      if not (String.IsNullOrEmpty p.Option) then
        let key =
          match tryFindOptionKey engineCfg.Options p.Option with
          | Some k -> k
          | None -> failf "Missing option key for %s (option: %s)" p.Name p.Option
        let optValue = string engineCfg.Options.[key]
        let init =
          match parseEmbeddedValue optValue p.Name with
          | Some v -> v
          | None -> failf "Option %s does not contain embedded key '%s='" key p.Name
        { Def = p
          OptionKey = key
          InitialValue = init
          ScaleIsLog = normalizeScale p.Scale = "log"
          EmbeddedKey = Some p.Name }
      else
        let key =
          match tryFindOptionKey engineCfg.Options p.Name with
          | Some k -> k
          | None -> failf "Missing option key for %s" p.Name
        let init =
          match tryToFloat engineCfg.Options.[key] with
          | Some v -> v
          | None -> failf "Option %s is not numeric and cannot be tuned" key
        { Def = p
          OptionKey = key
          InitialValue = init
          ScaleIsLog = normalizeScale p.Scale = "log"
          EmbeddedKey = None })

  let internal clamp lo hi x = max lo (min hi x)

  let internal toNorm (p: ResolvedParameter) value =
    let v = clamp p.Def.Min p.Def.Max value
    if p.ScaleIsLog then
      let lo = log p.Def.Min
      let hi = log p.Def.Max
      let t = (log v - lo) / (hi - lo)
      clamp -1.0 1.0 (2.0 * t - 1.0)
    else
      let t = (v - p.Def.Min) / (p.Def.Max - p.Def.Min)
      clamp -1.0 1.0 (2.0 * t - 1.0)

  let private quantize (step: float) (value: float) =
    let n = Math.Round(value / step)
    let result = n * step
    // Round to step-implied decimal places to avoid IEEE 754 noise
    // e.g. step=0.05 → 2 decimals, step=0.1 → 1, step=1.0 → 0
    let decimals = max 0 (int (ceil (-log10 step)))
    Math.Round(result, decimals)

  let internal fromNorm (p: ResolvedParameter) n =
    let clamped = clamp -1.0 1.0 n
    let t = (clamped + 1.0) / 2.0
    let raw =
      if p.ScaleIsLog then
        let lo = log p.Def.Min
        let hi = log p.Def.Max
        exp (lo + t * (hi - lo))
      else
        p.Def.Min + t * (p.Def.Max - p.Def.Min)
    let q = quantize p.Def.Step raw
    let fixedV = clamp p.Def.Min p.Def.Max q
    if Double.IsFinite fixedV then fixedV else p.InitialValue

  let private boxValue (p: ResolvedParameter) (value: float) : obj =
    let rounded = Math.Round(value)
    if p.Def.Step >= 1.0 && abs (value - rounded) < 1e-9 then box (int rounded)
    else box value

  let internal optionsFromVector (baseOptions: IDictionary<string,obj>) (parameters: ResolvedParameter[]) (x: float[]) =
    let options = Dictionary<string,obj>(baseOptions, StringComparer.OrdinalIgnoreCase)
    for i in 0 .. parameters.Length - 1 do
      let p = parameters.[i]
      let v = fromNorm p x.[i]
      match p.EmbeddedKey with
      | Some key ->
          let current = string options.[p.OptionKey]
          options.[p.OptionKey] <- box (replaceEmbeddedValue current key v)
      | None ->
          options.[p.OptionKey] <- boxValue p v
    options

  let internal tunedValuesFromVector (baseOptions: IDictionary<string,obj>) (parameters: ResolvedParameter[]) (x: float[]) =
    // Build a working copy so multiple embedded params on the same option key accumulate
    let working = Dictionary<string,obj>(baseOptions, StringComparer.OrdinalIgnoreCase)
    for i in 0 .. parameters.Length - 1 do
      let p = parameters.[i]
      let v = fromNorm p x.[i]
      match p.EmbeddedKey with
      | Some key ->
          let current = string working.[p.OptionKey]
          working.[p.OptionKey] <- box (replaceEmbeddedValue current key v)
      | None ->
          working.[p.OptionKey] <- boxValue p v
    [|
      for i in 0 .. parameters.Length - 1 do
        let p = parameters.[i]
        match p.EmbeddedKey with
        | Some _ ->
            yield KeyValuePair<string,obj>(p.OptionKey, working.[p.OptionKey])
        | None ->
            let v = fromNorm p x.[i]
            yield KeyValuePair<string,obj>(p.OptionKey, boxValue p v)
    |]

  let private pFromElo elo =
    1.0 / (1.0 + Math.Pow(10.0, -elo / 400.0))

  let private llrFromWdl elo0 elo1 wins draws losses =
    let n = wins + draws + losses
    if n <= 0 then 0.0
    else
      let score = float wins + 0.5 * float draws
      let eps = 1e-9
      let p0 = clamp eps (1.0 - eps) (pFromElo elo0)
      let p1 = clamp eps (1.0 - eps) (pFromElo elo1)
      let ll p = score * log p + (float n - score) * log (1.0 - p)
      clamp -1000.0 1000.0 (ll p1 - ll p0)

  let private statsDecision (sprt: SprtConfig) wins draws losses =
    let games = wins + draws + losses
    let llr = llrFromWdl sprt.Elo0 sprt.Elo1 wins draws losses
    let elo = EloCalculator.eloDiffWDL (float wins) (float draws) (float losses)
    let a = log ((1.0 - sprt.Beta) / sprt.Alpha)
    let b = log (sprt.Beta / (1.0 - sprt.Alpha))
    if games >= sprt.MinGames && llr >= a then "accept_h1", llr, elo
    elif games >= sprt.MinGames && llr <= b then "accept_h0", llr, elo
    else "undecided", llr, elo

  let private pentanomialScores = [| 0.0; 0.5; 1.0; 1.5; 2.0 |]

  /// Tilts the observed pentanomial pdf so that its mean equals targetMean,
  /// using the exponential family: q(i) = pdf(i) * exp(λ * s_i) / Z(λ).
  /// Newton's method finds λ; derivative of mean w.r.t. λ is the variance.
  let private tiltDistribution (pdf: float[]) (targetMean: float) =
    let mutable lam = 0.0
    let mutable converged = false
    let mutable iter = 0
    while not converged && iter < 50 do
      iter <- iter + 1
      let weights = Array.init 5 (fun i -> pdf.[i] * exp(lam * pentanomialScores.[i]))
      let z = Array.sum weights
      let mean = (Array.init 5 (fun i -> pentanomialScores.[i] * weights.[i]) |> Array.sum) / z
      if abs (targetMean - mean) < 1e-9 then
        converged <- true
      else
        let meanSq = (Array.init 5 (fun i -> pentanomialScores.[i] * pentanomialScores.[i] * weights.[i]) |> Array.sum) / z
        let variance = meanSq - mean * mean
        if abs variance < 1e-12 then
          converged <- true
        else
          lam <- lam + (targetMean - mean) / variance
    let weights = Array.init 5 (fun i -> pdf.[i] * exp(lam * pentanomialScores.[i]))
    let z = Array.sum weights
    Array.init 5 (fun i -> weights.[i] / z)

  /// Pentanomial LLR using exponential tilting of the observed distribution.
  /// For each hypothesis (elo0, elo1), tilts the regularized observed frequencies
  /// to have the expected pair-score mean, then computes the multinomial LLR.
  let private pentanomialLlrInternal (elo0: float) (elo1: float) (n: float[]) =
    let total = Array.sum n
    if total < 1.0 then 0.0
    else
      let epsilon = 1e-3
      let regTotal = total + 5.0 * epsilon
      let pdf = Array.init 5 (fun i -> (n.[i] + epsilon) / regTotal)

      let mu0 = 2.0 * pFromElo elo0
      let mu1 = 2.0 * pFromElo elo1

      let q0 = tiltDistribution pdf mu0
      let q1 = tiltDistribution pdf mu1

      let mutable llr = 0.0
      for i in 0 .. 4 do
        if n.[i] > 0.0 then
          let ratio = q1.[i] / q0.[i]
          if ratio > 0.0 && Double.IsFinite(ratio) then
            llr <- llr + n.[i] * log(ratio)
      // Clamp to prevent Infinity from extreme one-sided results
      clamp -1000.0 1000.0 llr

  let private pentanomialForMatchup (left: string) (right: string) (games: ResizeArray<PgnGame>) =
    let swapped = StringComparer.OrdinalIgnoreCase.Compare(left, right) > 0
    let a, b =
      if not swapped then left, right
      else right, left
    games
    |> ChessLibrary.Statistics.Pentanomial.calculateAllMatchups
    |> List.tryPick (fun ((e1, e2), c) ->
      if String.Equals(e1, a, StringComparison.OrdinalIgnoreCase) && String.Equals(e2, b, StringComparison.OrdinalIgnoreCase) then
        Some (if swapped then { c with W2 = c.L2; L2 = c.W2; W15 = c.L15; L15 = c.W15 } else c)
      else
        None)

  let private statsDecisionFromPentanomial (sprt: SprtConfig) (counts: ChessLibrary.Statistics.Pentanomial.Counts) =
    let pairs = counts.CompletedPairs
    let games = pairs * 2
    if games <= 0 then "undecided", 0.0, 0.0, games
    else
      let score =
        0.5 * float counts.L15 +
        1.0 * float counts.D +
        1.5 * float counts.W15 +
        2.0 * float counts.W2
      let scoreFrac = score / float games
      let elo = EloCalculator.eloDifferenceFromScore scoreFrac
      let n = [| float counts.L2; float counts.L15; float counts.D; float counts.W15; float counts.W2 |]
      let llr = pentanomialLlrInternal sprt.Elo0 sprt.Elo1 n
      let a = log ((1.0 - sprt.Beta) / sprt.Alpha)
      let b = log (sprt.Beta / (1.0 - sprt.Alpha))
      if games >= sprt.MinGames && llr >= a then "accept_h1", llr, elo, games
      elif games >= sprt.MinGames && llr <= b then "accept_h0", llr, elo, games
      else "undecided", llr, elo, games

  let internal cloneEngineWithOptions name (baseCfg: EngineConfig) timeControlId (options: IDictionary<string,obj>) =
    let copied = Dictionary<string,obj>(options, StringComparer.OrdinalIgnoreCase)
    { baseCfg with Name = name; TimeControlID = timeControlId; Options = copied }

  let internal prepareWritablePgnPath (requestedPath: string) =
    let dir = Path.GetDirectoryName(requestedPath)
    if not (String.IsNullOrWhiteSpace dir) then
      Directory.CreateDirectory(dir) |> ignore

    if File.Exists requestedPath then
      try
        File.Delete(requestedPath)
        requestedPath
      with
      | _ ->
          // If another process still holds the file, rotate to a unique sibling path.
          let name = Path.GetFileNameWithoutExtension(requestedPath)
          let ext = Path.GetExtension(requestedPath)
          let stamp = DateTime.UtcNow.ToString("yyyyMMdd_HHmmss_fff")
          let suffix = Guid.NewGuid().ToString("N").Substring(0, 6)
          let rotated = Path.Combine(dir, sprintf "%s_%s_%s%s" name stamp suffix ext)
          rotated
    else
      requestedPath

  let private appendMatchPgnToCumulative (matchPgnPath: string) (cumulativePgnPath: string) (maxGames: int) =
    if not (String.IsNullOrWhiteSpace cumulativePgnPath) && File.Exists matchPgnPath then
      // Skip appending if cumulative PGN already has enough reference games
      let alreadyCapped =
        if maxGames > 0 && File.Exists cumulativePgnPath then
          use countFs = new FileStream(cumulativePgnPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
          use countSr = new StreamReader(countFs)
          let mutable count = 0
          let mutable line = countSr.ReadLine()
          while line <> null && count < maxGames do
            if line.StartsWith("[Result ") then count <- count + 1
            line <- countSr.ReadLine()
          count >= maxGames
        else false
      if not alreadyCapped then
        use readFs = new FileStream(matchPgnPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use sr = new StreamReader(readFs)
        let content = sr.ReadToEnd()
        if not (String.IsNullOrWhiteSpace content) then
          use writeFs = new FileStream(cumulativePgnPath, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)
          use sw = new StreamWriter(writeFs)
          sw.Write(content)

  let private buildNodeTimeControl targetNodes (tc: TimeControl) =
    let configs =
      if tc.TimeConfigs |> List.isEmpty then
        [ { Id = 1; Fixed = TimeOnly(0,0,1); Increment = TimeOnly.MinValue; NodeLimit = true; Nodes = targetNodes } ]
      else
        tc.TimeConfigs
        |> List.map (fun c -> { c with NodeLimit = true; Nodes = targetNodes; Fixed = TimeOnly(0,0,1); Increment = TimeOnly.MinValue })
    { tc with TimeConfigs = configs; WmovesToGo = 0; BmovesToGo = 0 }

  let internal buildMatchTournament (cfg: TuneConfig) (baseTournament: Tournament) (engineA: EngineConfig) (engineB: EngineConfig) (opponentNodes: int) pgnPath (referencePgnPath: string) (preventDeviationFor: string[]) =
    let oppNodes = if opponentNodes > 0 then opponentNodes else cfg.TargetNodes
    let tc, tcIdA, tcIdB =
      if oppNodes = cfg.TargetNodes then
        let tc = buildNodeTimeControl cfg.TargetNodes baseTournament.TimeControl
        let tcId = tc.TimeConfigs |> List.head |> fun t -> t.Id
        tc, tcId, tcId
      else
        let baseConf = { Id = 1; Fixed = TimeOnly(0,0,1); Increment = TimeOnly.MinValue; NodeLimit = true; Nodes = cfg.TargetNodes }
        let oppConf  = { Id = 2; Fixed = TimeOnly(0,0,1); Increment = TimeOnly.MinValue; NodeLimit = true; Nodes = oppNodes }
        let tc = { baseTournament.TimeControl with TimeConfigs = [ baseConf; oppConf ]; WmovesToGo = 0; BmovesToGo = 0 }
        tc, 1, 2
    let roundsNeeded =
      if cfg.OpeningsTwice then int (Math.Ceiling(float cfg.Sprt.MaxGames / 2.0))
      else cfg.Sprt.MaxGames

    let openingPath =
      if String.IsNullOrWhiteSpace cfg.OpeningsPath then baseTournament.Opening.OpeningsPath
      else Some (Path.GetFullPath cfg.OpeningsPath)

    let opening =
      { baseTournament.Opening with
          OpeningsPath = openingPath
          OpeningsPly = cfg.OpeningsPly
          OpeningsTwice = cfg.OpeningsTwice }

    let hasDeviation = preventDeviationFor <> null && preventDeviationFor.Length > 0
    let testOptions =
      { baseTournament.TestOptions with
          PolicyTest = false
          ValueTest = false
          WriteToConsole = false
          NumberOfGamesInParallelConsoleOnly = cfg.ParallelGames
          GPUs = if cfg.GPUs <> null then cfg.GPUs else baseTournament.TestOptions.GPUs }

    let eA = { engineA with TimeControlID = tcIdA }
    let eB = { engineB with TimeControlID = tcIdB }
    let setup =
      { EngineDefFolder = ""
        EngineDefList = []
        Engines = [ eA; eB ] }

    { baseTournament with
        Name = sprintf "%s_tune_match" baseTournament.Name
        Description = "EngineBattle tuner SPRT match"
        TournamentMode = "RR"
        ConsoleOnly = true
        PreventMoveDeviation = hasDeviation
        PreventMoveDeviationFor = if preventDeviationFor <> null then preventDeviationFor else [||]
        Rounds = roundsNeeded
        Challengers = 0
        DelayBetweenGames = TimeOnly.MinValue
        MinMoveTimeInMS = 0
        OrdoExePath = ""
        PgnOutPath = pgnPath
        ReferencePGNPath = if String.IsNullOrWhiteSpace referencePgnPath then "" else referencePgnPath
        TestOptions = testOptions
        Opening = opening
        TimeControl = tc
        EngineSetup = setup }

  let private runTournamentDirect (tournament: Tournament) (candidateName: string) (baselineName: string) (sprt: SprtConfig) =
    use logFactory =
      LoggerFactory.Create(fun builder ->
        builder.SetMinimumLevel(LogLevel.Warning) |> ignore
        builder.AddSimpleConsole(fun o ->
          o.SingleLine <- true
          o.TimestampFormat <- "HH:mm:ss "
          o.IncludeScopes <- false) |> ignore)
    let logger = logFactory.CreateLogger("EngineBattle.Tuner")
    let mutable runnerOpt : Manager.Runner option = None
    let mutable lastReportedGames = 0
    let callback =
      Action<Update>(fun update ->
        match update with
        | PeriodicResults results ->
            let gamesNow = results.Count
            if gamesNow >= max 2 sprt.MinGames && gamesNow > lastReportedGames then
              lastReportedGames <- gamesNow
              match runnerOpt with
              | Some runner ->
                  let pgnGames = runner.GetPGNGames()
                  match pentanomialForMatchup candidateName baselineName pgnGames with
                  | Some c ->
                      let decision, llr, elo, usedGames = statsDecisionFromPentanomial sprt c
                      printfn "  SPRT(penta) g=%d pairs=%d [L2=%d L1.5=%d D=%d W1.5=%d W2=%d] llr=%.3f elo=%.2f %s"
                        usedGames c.CompletedPairs c.L2 c.L15 c.D c.W15 c.W2 llr elo decision
                      if decision = "accept_h1" || decision = "accept_h0" then
                        runner.Cancel()
                  | None -> ()
              | None -> ()
        | _ -> ())
    let runner = Manager.Runner(logger, callback, false, true)
    runnerOpt <- Some runner
    runner.AddTournament tournament
    let results = runner.Run() |> Seq.toArray
    let pgnGames = runner.GetPGNGames()
    results, pgnGames

  let private scoreFromResult (candidateName: string) (result: Result) =
    let isCandidate (name: string) = name.Equals(candidateName, StringComparison.OrdinalIgnoreCase)
    match result.Result with
    | "1-0" -> if isCandidate result.Player1 then 1 else 0
    | "0-1" -> if isCandidate result.Player2 then 1 else 0
    | "1/2-1/2" -> 2
    | _ -> 2

  let internal runMatchBetween
      (cfg: TuneConfig)
      (baseTournament: Tournament)
      (engineA: EngineConfig)
      (engineB: EngineConfig)
      (opponentNodes: int)
      (pgnPath: string)
      (referencePgnPath: string)
      (preventDeviationFor: string[]) : MatchStats =

    let candidateName = engineA.Name
    let baselineName = engineB.Name
    let matchPgnPath = prepareWritablePgnPath pgnPath
    let tournament = buildMatchTournament cfg baseTournament engineA engineB opponentNodes matchPgnPath referencePgnPath preventDeviationFor

    let results, pgnGames = runTournamentDirect tournament candidateName baselineName cfg.Sprt
    appendMatchPgnToCumulative matchPgnPath referencePgnPath cfg.MaxReferencePgnGames

    let completedResults =
      results
      |> Array.filter (fun r -> r.Reason <> ResultReason.Cancel)

    let mutable wins = 0
    let mutable draws = 0
    let mutable losses = 0
    for game in completedResults do
      match scoreFromResult candidateName game with
      | 1 -> wins <- wins + 1
      | 0 -> losses <- losses + 1
      | _ -> draws <- draws + 1

    let games = wins + draws + losses
    if games = 0 then
      failwithf "No completed games in SPRT match (%s vs %s). All %d games were cancelled. Check engine startup/options. Match PGN: %s"
        candidateName baselineName results.Length matchPgnPath

    let decision, llr, elo =
      match pentanomialForMatchup candidateName baselineName pgnGames with
      | Some c ->
          let d, l, e, _ = statsDecisionFromPentanomial cfg.Sprt c
          d, l, e
      | None ->
          statsDecision cfg.Sprt wins draws losses

    let finalDecision =
      if decision = "undecided" then
        if elo >= 0.0 then "fallback_h1" else "fallback_h0"
      else
        decision

    { Wins = wins
      Draws = draws
      Losses = losses
      Games = games
      Llr = llr
      Elo = elo
      Decision = finalDecision
      StoppedEarly = games < cfg.Sprt.MaxGames }

  let internal runSprtMatch
      (cfg: TuneConfig)
      (baseTournament: Tournament)
      (baseEngine: EngineConfig)
      (candidateName: string)
      (candidateOptions: IDictionary<string,obj>)
      (baselineName: string)
      (baselineOptions: IDictionary<string,obj>)
      (pgnPath: string)
      (referencePgnPath: string)
      (preventDeviationFor: string[]) =

    let candidateEngine = cloneEngineWithOptions candidateName baseEngine 1 candidateOptions
    let baselineEngine = cloneEngineWithOptions baselineName baseEngine 1 baselineOptions
    runMatchBetween cfg baseTournament candidateEngine baselineEngine 0 pgnPath referencePgnPath preventDeviationFor

  let internal saveState (path: string) (state: TuneState) =
    let json = JsonSerializer.Serialize(state, jsonOptionsIndented)
    File.WriteAllText(path, json)

  let private loadState (path: string) =
    let json = File.ReadAllText(path)
    JsonSerializer.Deserialize<TuneState>(json, jsonOptionsRead)

  let internal appendHistory (path: string) (entry: HistoryEntry) =
    let line = JsonSerializer.Serialize(entry)
    File.AppendAllText(path, line + Environment.NewLine)

  let internal writeBestOptions (path: string) (pairs: KeyValuePair<string,obj>[]) =
    let d = Dictionary<string,obj>(StringComparer.OrdinalIgnoreCase)
    for kv in pairs do d.[kv.Key] <- kv.Value
    let json = JsonSerializer.Serialize(d, jsonOptionsIndented)
    File.WriteAllText(path, json)

  let internal summaryText
      (cfg: TuneConfig)
      (parameters: ResolvedParameter[])
      (bestX: float[])
      (finalStats: MatchStats)
      (candidateCount: int)
      (elapsed: TimeSpan)
      (evalMode: string)
      (finalAccuracyTuned: float option)
      (finalAccuracyInitial: float option) =
    let sb = StringBuilder()
    sb.AppendLine("EngineBattle tuner summary") |> ignore
    sb.AppendLine("-------------------------") |> ignore
    sb.AppendLine(sprintf "Eval mode: %s" evalMode) |> ignore
    sb.AppendLine(sprintf "Target nodes: %d" cfg.TargetNodes) |> ignore
    sb.AppendLine(sprintf "Parallel games: %d" cfg.ParallelGames) |> ignore
    sb.AppendLine(sprintf "Elapsed: %s" (elapsed.ToString())) |> ignore
    sb.AppendLine(sprintf "Candidate comparisons: %d" candidateCount) |> ignore
    match evalMode with
    | "puzzle" | "eret" ->
      match finalAccuracyTuned, finalAccuracyInitial with
      | Some tunedAcc, Some initAcc ->
        sb.AppendLine(sprintf "Final validation: tuned=%.4f vs initial=%.4f (delta=%+.4f)" tunedAcc initAcc (tunedAcc - initAcc)) |> ignore
        sb.AppendLine(sprintf "Final validation winner: %s" (if tunedAcc >= initAcc then "tuned" else "initial")) |> ignore
      | _ -> ()
    | _ ->
      sb.AppendLine(sprintf "Final validation decision: %s" finalStats.Decision) |> ignore
      sb.AppendLine(sprintf "Final validation WDL: %d/%d/%d (%d games)" finalStats.Wins finalStats.Draws finalStats.Losses finalStats.Games) |> ignore
      sb.AppendLine(sprintf "Final validation Elo: %.2f" finalStats.Elo) |> ignore
      sb.AppendLine(sprintf "Final validation LLR: %.4f" finalStats.Llr) |> ignore
    sb.AppendLine() |> ignore
    sb.AppendLine("Final tuned options:") |> ignore
    for i in 0 .. parameters.Length - 1 do
      let p = parameters.[i]
      let v = fromNorm p bestX.[i]
      match p.EmbeddedKey with
      | Some key -> sb.AppendLine(sprintf "- %s|%s = %g" p.OptionKey key v) |> ignore
      | None -> sb.AppendLine(sprintf "- %s = %g" p.OptionKey v) |> ignore
    sb.ToString()

  let private sampleRademacher (rng: Random) (active: bool[]) =
    let mutable calls = 0
    let delta =
      [|
        for i in 0 .. active.Length - 1 do
          if active.[i] then
            calls <- calls + 1
            if rng.Next(0, 2) = 0 then -1.0 else 1.0
          else 0.0
      |]
    delta, calls

  let internal shouldStop (started: DateTime) (cfg: TuneConfig) (candidateCount: int) =
    let wallExceeded = DateTime.UtcNow - started > TimeSpan.FromHours(cfg.MaxWallHours)
    wallExceeded || candidateCount >= cfg.MaxCandidates

  let private restoreRandomState seed callsConsumed =
    let rng = Random(seed)
    for _ in 1 .. callsConsumed do
      rng.Next(0, 2) |> ignore
    rng

  let normalizeParameterValue (parameter: TuneParameterDef) (value: float) =
    let p =
      { Def = parameter
        OptionKey = parameter.Name
        InitialValue = value
        ScaleIsLog = normalizeScale parameter.Scale = "log"
        EmbeddedKey = None }
    toNorm p value

  let denormalizeParameterValue (parameter: TuneParameterDef) (normValue: float) =
    let p =
      { Def = parameter
        OptionKey = parameter.Name
        InitialValue = parameter.Min
        ScaleIsLog = normalizeScale parameter.Scale = "log"
        EmbeddedKey = None }
    fromNorm p normValue

  let internal defaultPerturbation (v: float) = if v > 0.0 then v else 0.1

  let spsaGains phaseIterations globalIteration =
    let alpha = 0.602
    let gamma = 0.167
    let a = 0.5
    let c = 0.1
    let A = max 1.0 (0.1 * float phaseIterations)
    let k = globalIteration + 1
    let ak = a / Math.Pow(A + float k, alpha)
    let ck = c / Math.Pow(float k, gamma)
    ak, ck

  let sprtDecisionFromWdl (sprt: SprtConfig) wins draws losses =
    statsDecision sprt wins draws losses

  let pentanomialLlr (elo0: float) (elo1: float) (l2: int) (l15: int) (d: int) (w15: int) (w2: int) =
    pentanomialLlrInternal elo0 elo1 [| float l2; float l15; float d; float w15; float w2 |]

  /// Standalone SPSA optimization loop for testing convergence without engine dependencies.
  /// The core arithmetic mirrors runTune with identical constants.
  /// `compare` receives (xPlus, xMinus) normalized vectors and returns a score fraction
  /// (0.0 to 1.0) representing the score of xPlus. 0.5 = equal, >0.5 = xPlus is better.
  let runSpsaLoop
      (startX: float[])
      (active: bool[])
      (iterations: int)
      (seed: int)
      (compare: float[] -> float[] -> float)
      : float[] =
    let n = startX.Length
    let x = Array.copy startX
    let rng = Random(seed)
    let alpha = 0.602
    let gamma = 0.167
    let a = 0.5
    let c = 0.1
    let bigA = max 1.0 (0.1 * float iterations)
    for iter in 0 .. iterations - 1 do
      let k = iter + 1
      let ak = a / Math.Pow(bigA + float k, alpha)
      let ck = c / Math.Pow(float k, gamma)
      let delta = [| for i in 0 .. n - 1 do if active.[i] then (if rng.Next(0, 2) = 0 then -1.0 else 1.0) else 0.0 |]
      let xPlus = Array.copy x
      let xMinus = Array.copy x
      for i in 0 .. n - 1 do
        if active.[i] then
          xPlus.[i] <- x.[i] + ck * delta.[i]
          xMinus.[i] <- x.[i] - ck * delta.[i]
      for i in 0 .. n - 1 do xPlus.[i] <- clamp -1.0 1.0 xPlus.[i]
      for i in 0 .. n - 1 do xMinus.[i] <- clamp -1.0 1.0 xMinus.[i]
      let scoreFrac = compare xPlus xMinus
      let yDiff = 2.0 * (scoreFrac - 0.5)
      for i in 0 .. n - 1 do
        if active.[i] then
          let g = yDiff / (2.0 * ck * delta.[i])
          let step = clamp -0.25 0.25 (ak * g)
          x.[i] <- x.[i] + step
      for i in 0 .. n - 1 do x.[i] <- clamp -1.0 1.0 x.[i]
    x

  type internal TuneSetupData =
    { Config: TuneConfig
      BaseEngine: EngineConfig
      OpponentEngine: EngineConfig option
      Resolved: ResolvedParameter[]
      BaseTournament: Tournament
      StartX: float[]
      State: TuneState
      OutputDir: string
      StatePath: string
      HistoryPath: string
      BestOptionsPath: string
      SummaryPath: string
      PgnDir: string
      PgnBaseName: string
      PgnExt: string
      StartedUtc: DateTime
      MakeMatchPgnPath: string -> int -> string
      Optimizer: string
      EvalMode: string
      PuzzleData: CsvPuzzleData[] option
      PuzzleConfig: PuzzleConfig option
      EretConfig: EretConfig option
      CumulativePgnPath: string }

  let internal prepareTune path : TuneSetupData =
    let configPath, cfg = readTuneConfig path
    ensureDir cfg.OutputDir
    let outputDir = Path.GetFullPath cfg.OutputDir

    let enginePath = resolvePath cfg.EngineConfigPath "engineConfigPath"
    let tournamentPath = resolvePath cfg.BaseTournamentConfigPath "baseTournamentConfigPath"
    if not (String.IsNullOrWhiteSpace cfg.OpeningsPath) then
      ignore (resolvePath cfg.OpeningsPath "openingsPath")

    let baseEngine = JSON.readSingleEngineConfig enginePath

    let opponentEngine =
      if not (String.IsNullOrWhiteSpace cfg.OpponentConfigPath) then
        let oppPath = resolvePath cfg.OpponentConfigPath "opponentConfigPath"
        Some (JSON.readSingleEngineConfig oppPath)
      else
        None

    validateConfig cfg baseEngine
    let resolved = resolveParameters cfg baseEngine

    let baseTournament =
      match JSON.readTournamentJson tournamentPath with
      | Some t -> t
      | None -> failwithf "Unable to load tournament config: %s" tournamentPath

    let statePath = Path.Combine(outputDir, "tune-state.json")
    let historyPath = Path.Combine(outputDir, "tune-history.jsonl")
    let bestOptionsPath = Path.Combine(outputDir, "best-engine-options.json")
    let summaryPath = Path.Combine(outputDir, "tune-summary.txt")

    let startX = resolved |> Array.map (fun p -> toNorm p p.InitialValue)

    let state =
      if cfg.Resume && File.Exists(statePath) then
        let loaded = loadState statePath
        if obj.ReferenceEquals(loaded, null) then failwith "Failed to parse tune-state.json"
        if loaded.X.Length <> resolved.Length then failwith "Checkpoint dimensionality does not match parameters"
        loaded
      else
        { PhaseIndex = 0
          IterationInPhase = 0
          GlobalIteration = 0
          CandidateCount = 0
          RandomCallsConsumed = 0
          X = Array.copy startX
          BestX = Array.copy startX
          StartedUtc = nowUtc()
          LastUpdatedUtc = nowUtc()
          ObservationsX = null
          ObservationsY = null
          FinalValidationCompleted = false }

    let startedUtc =
      match DateTime.TryParse(state.StartedUtc, null, DateTimeStyles.RoundtripKind) with
      | true, dt -> dt
      | _ -> DateTime.UtcNow

    let configuredPgnPath =
      if String.IsNullOrWhiteSpace baseTournament.PgnOutPath then
        Path.Combine(outputDir, "tuner_matches.pgn")
      else
        Path.GetFullPath(baseTournament.PgnOutPath)

    let pgnDir =
      let d = Path.GetDirectoryName(configuredPgnPath)
      if String.IsNullOrWhiteSpace d then outputDir else d
    Directory.CreateDirectory(pgnDir) |> ignore

    let pgnBaseName =
      let n = Path.GetFileNameWithoutExtension(configuredPgnPath)
      if String.IsNullOrWhiteSpace n then "tuner_matches" else n

    let pgnExt =
      let e = Path.GetExtension(configuredPgnPath)
      if String.IsNullOrWhiteSpace e then ".pgn" else e

    let makeMatchPgnPath prefix number =
      Path.Combine(pgnDir, sprintf "%s_%s_%06d%s" pgnBaseName prefix number pgnExt)

    let cumulativePgnPath =
      if cfg.PreventOpponentDeviation then
        Path.Combine(pgnDir, sprintf "%s_cumulative%s" pgnBaseName pgnExt)
      else ""

    let optimizer =
      if String.IsNullOrWhiteSpace cfg.Optimizer then "spsa"
      else cfg.Optimizer.Trim().ToLowerInvariant()

    let evalMode = normalizeEvalMode cfg.EvalMode

    let puzzleConfig, puzzleData =
      if evalMode = "puzzle" then
        let pc = JSONParser.loadPuzzleConfig (Path.GetFullPath cfg.EvalConfigPath)
        let pd = JSONParser.parsePuzzle pc.PuzzleFile true
        printfn "Loaded %d puzzles from %s" pd.Length pc.PuzzleFile
        Some pc, Some pd
      else
        None, None

    let eretConfig =
      if evalMode = "eret" then
        Some (JSONParser.loadEretConfig (Path.GetFullPath cfg.EvalConfigPath))
      else
        None

    { Config = cfg
      BaseEngine = baseEngine
      OpponentEngine = opponentEngine
      Resolved = resolved
      BaseTournament = baseTournament
      StartX = startX
      State = state
      OutputDir = outputDir
      StatePath = statePath
      HistoryPath = historyPath
      BestOptionsPath = bestOptionsPath
      SummaryPath = summaryPath
      PgnDir = pgnDir
      PgnBaseName = pgnBaseName
      PgnExt = pgnExt
      StartedUtc = startedUtc
      MakeMatchPgnPath = makeMatchPgnPath
      Optimizer = optimizer
      EvalMode = evalMode
      PuzzleData = puzzleData
      PuzzleConfig = puzzleConfig
      EretConfig = eretConfig
      CumulativePgnPath = cumulativePgnPath }

  let private runPuzzlesByType (puzzleType: string) (input: PuzzleInput) : ResizeArray<Score> =
    let noop = Action<Lichess>(fun _ -> ())
    let t = if String.IsNullOrWhiteSpace puzzleType then "" else puzzleType.ToLower().Trim()
    match t with
    | "policy" -> runPolicyHeadTest(input, noop)
    | "value"  -> runValueHeadTest(input, noop)
    | "search" -> runSearchTests(input, noop)
    | _        -> runValueAndPolicyHeadTest(input, noop)

  let internal evaluateAccuracy
      (setup: TuneSetupData)
      (candidateOptions: IDictionary<string,obj>)
      (label: string)
      : float =
    let engineCfg = cloneEngineWithOptions label setup.BaseEngine 1 candidateOptions
    match setup.EvalMode with
    | "puzzle" ->
        let pc = setup.PuzzleConfig |> Option.defaultWith (fun () -> failwith "PuzzleConfig not loaded for puzzle evalMode")
        let puzzles = setup.PuzzleData |> Option.defaultWith (fun () -> failwith "PuzzleData not loaded for puzzle evalMode")
        let engines = ResizeArray([ (engineCfg, 0) ])
        let puzzleInput =
          PuzzleInput.Create(
            puzzles, pc.MaxRating, pc.MinRating, pc.RatingGroups, pc.PuzzleFilter,
            engines, 1, pc.SampleSize, pc.Nodes, 0, 0, max 1 pc.Concurrency)
        let scores = runPuzzlesByType pc.Type puzzleInput
        let totalCorrect = scores |> Seq.sumBy (fun s -> s.Correct)
        let totalNumber = scores |> Seq.sumBy (fun s -> s.TotalNumber)
        let acc = if totalNumber > 0 then float totalCorrect / float totalNumber else 0.0
        printfn "  %s accuracy: %.4f (%d/%d)" label acc totalCorrect totalNumber
        acc
    | "eret" ->
        let eretCfg = setup.EretConfig |> Option.defaultWith (fun () -> failwith "EretConfig not loaded for eret evalMode")
        let time = UnionType.FixedTime (TimeOnly(0,0,eretCfg.TimeInSeconds))
        let nodes = UnionType.Nodes eretCfg.Nodes
        let timeControl = if eretCfg.RunWithNodeLimit then nodes else time
        let engines = seq { (engineCfg, 0) }
        let results = runEretTests timeControl engines eretCfg (Action<_>(ignore))
        let acc = if results.Count > 0 then results.[0].Accuracy else 0.0
        printfn "  %s accuracy: %.4f" label acc
        acc
    | _ -> failwith "evaluateAccuracy called with non-puzzle/eret evalMode"

  let internal runComparisonByAccuracy
      (setup: TuneSetupData)
      (nameA: string) (optionsA: IDictionary<string,obj>)
      (nameB: string) (optionsB: IDictionary<string,obj>)
      : bool * float * float =
    let accA = evaluateAccuracy setup optionsA nameA
    let accB = evaluateAccuracy setup optionsB nameB
    let aBeatB = accA >= accB
    printfn "  %s=%.4f vs %s=%.4f → winner: %s" nameA accA nameB accB (if aBeatB then nameA else nameB)
    (aBeatB, accA, accB)

  let internal runOpponentComparison
      (setup: TuneSetupData)
      (nameA: string) (optionsA: IDictionary<string,obj>)
      (nameB: string) (optionsB: IDictionary<string,obj>)
      (pgnPathA: string) (pgnPathB: string) : bool =
    let cfg = setup.Config
    let baseEngine = setup.BaseEngine
    let opponent = setup.OpponentEngine.Value
    let oppNodes = if cfg.OpponentTargetNodes > 0 then cfg.OpponentTargetNodes else cfg.TargetNodes
    let opponentDeviationFor =
      if cfg.PreventOpponentDeviation then [| opponent.Name |]
      else null
    let cumulativePgnPath = setup.CumulativePgnPath

    let engineA = cloneEngineWithOptions nameA baseEngine 1 optionsA
    let statsA = runMatchBetween cfg setup.BaseTournament engineA opponent oppNodes pgnPathA cumulativePgnPath opponentDeviationFor
    let scoreA = (float statsA.Wins + 0.5 * float statsA.Draws) / float (max 1 statsA.Games)

    let engineB = cloneEngineWithOptions nameB baseEngine 1 optionsB
    let statsB = runMatchBetween cfg setup.BaseTournament engineB opponent oppNodes pgnPathB cumulativePgnPath opponentDeviationFor
    let scoreB = (float statsB.Wins + 0.5 * float statsB.Draws) / float (max 1 statsB.Games)

    printfn "  %s vs %s: scoreA=%.4f (%d/%d/%d) scoreB=%.4f (%d/%d/%d) → winner: %s"
      nameA nameB scoreA statsA.Wins statsA.Draws statsA.Losses scoreB statsB.Wins statsB.Draws statsB.Losses
      (if scoreA >= scoreB then nameA else nameB)
    scoreA >= scoreB

  let internal printTuneHeader (setup: TuneSetupData) =
    let cfg = setup.Config
    let resolved = setup.Resolved
    let x = setup.State.X
    let phaseIndex = setup.State.PhaseIndex
    let iterationInPhase = setup.State.IterationInPhase
    let candidateCount = setup.State.CandidateCount

    printfn ""
    printfn "EngineBattle Tuner"
    printfn "=================="
    printfn "Engine:          %s" setup.BaseEngine.Name
    match setup.OpponentEngine with
    | Some opp ->
        printfn "Opponent:        %s" opp.Name
        let oppNodes = if cfg.OpponentTargetNodes > 0 then cfg.OpponentTargetNodes else cfg.TargetNodes
        printfn "Opponent nodes:  %d" oppNodes
        if cfg.UseOpponentForValidation then
          printfn "Opp validation:  true"
    | None -> ()
    printfn "Optimizer:       %s" setup.Optimizer
    printfn "Eval mode:       %s" setup.EvalMode
    printfn "Target nodes:    %d" cfg.TargetNodes
    printfn "Parallel games:  %d" cfg.ParallelGames
    printfn "Max wall hours:  %.1f" cfg.MaxWallHours
    printfn "Max candidates:  %d" cfg.MaxCandidates
    if setup.Optimizer = "spsa" then
      printfn "Perturbation c:  %.3f" (defaultPerturbation cfg.PerturbationSize)
    if setup.EvalMode <> "sprt" then
      printfn "Eval config:     %s" cfg.EvalConfigPath
      match setup.PuzzleData with
      | Some pd -> printfn "Puzzles loaded:  %d" pd.Length
      | None -> ()
    printfn ""
    if setup.EvalMode = "sprt" then
      printfn "SPRT: elo0=%.1f  elo1=%.1f  alpha=%.3f  beta=%.3f  games=[%d..%d]"
        cfg.Sprt.Elo0 cfg.Sprt.Elo1 cfg.Sprt.Alpha cfg.Sprt.Beta cfg.Sprt.MinGames cfg.Sprt.MaxGames
    else
      printfn "SPRT config (used for maxGames/iteration budget): games=[%d..%d]"
        cfg.Sprt.MinGames cfg.Sprt.MaxGames
    printfn ""
    printfn "Parameters (%d):" resolved.Length
    for i in 0 .. resolved.Length - 1 do
      let p = resolved.[i]
      let v = fromNorm p x.[i]
      let label = match p.EmbeddedKey with Some key -> sprintf "%s|%s" p.OptionKey key | None -> p.OptionKey
      printfn "  %-20s = %-8g  [%g .. %g]  step=%g  %s"
        label v p.Def.Min p.Def.Max p.Def.Step (normalizeScale p.Def.Scale)
    printfn ""
    printfn "Phases (%d):" cfg.Phases.Length
    for ph in cfg.Phases do
      printfn "  %s: %d iterations, params=[%s]" ph.Name ph.Iterations (String.Join(", ", ph.Parameters))
    if cfg.Resume && (phaseIndex > 0 || iterationInPhase > 0 || candidateCount > 0) then
      printfn ""
      printfn "Resumed from checkpoint (phase=%d, iteration=%d, candidates=%d)" phaseIndex iterationInPhase candidateCount
    printfn ""
    printfn "Output: %s" setup.OutputDir
    printfn ""

  let internal runSpsaTune (setup: TuneSetupData) =
    let cfg = setup.Config
    let baseEngine = setup.BaseEngine
    let resolved = setup.Resolved
    let baseTournament = setup.BaseTournament
    let startX = setup.StartX
    let state = setup.State
    let outputDir = setup.OutputDir
    let statePath = setup.StatePath
    let historyPath = setup.HistoryPath
    let bestOptionsPath = setup.BestOptionsPath
    let summaryPath = setup.SummaryPath
    let pgnDir = setup.PgnDir
    let pgnBaseName = setup.PgnBaseName
    let pgnExt = setup.PgnExt
    let startedUtc = setup.StartedUtc
    let makeMatchPgnPath = setup.MakeMatchPgnPath

    let rng = restoreRandomState cfg.Seed state.RandomCallsConsumed

    let paramIndex =
      let d = Dictionary<string,int>(StringComparer.OrdinalIgnoreCase)
      resolved |> Array.iteri (fun i p -> d.[p.Def.Name] <- i)
      d

    let mutable x = Array.copy state.X
    let mutable bestX = Array.copy state.BestX
    let mutable phaseIndex = state.PhaseIndex
    let mutable iterationInPhase = state.IterationInPhase
    let mutable globalIteration = state.GlobalIteration
    let mutable candidateCount = state.CandidateCount
    let mutable randomCalls = state.RandomCallsConsumed

    let clampNormVector (v: float[]) =
      for i in 0 .. v.Length - 1 do
        v.[i] <- clamp -1.0 1.0 v.[i]

    let printCurrentValues label (vec: float[]) =
      printfn "  %s:" label
      for i in 0 .. resolved.Length - 1 do
        let p = resolved.[i]
        let v = fromNorm p vec.[i]
        let name = match p.EmbeddedKey with Some key -> sprintf "%s|%s" p.OptionKey key | None -> p.OptionKey
        printfn "    %-20s = %g" name v

    while phaseIndex < cfg.Phases.Length && not (shouldStop startedUtc cfg candidateCount) do
      let phase = cfg.Phases.[phaseIndex]
      let phaseStartX = Array.copy x
      let active = Array.create resolved.Length false
      for pname in phase.Parameters do
        let idx = paramIndex.[pname]
        active.[idx] <- true

      let alpha = 0.602
      let gamma = 0.167
      let a = 0.5
      let c = defaultPerturbation cfg.PerturbationSize
      let A = max 1.0 (0.1 * float phase.Iterations)

      while iterationInPhase < phase.Iterations && not (shouldStop startedUtc cfg candidateCount) do
        let k = globalIteration + 1
        let ak = a / Math.Pow(A + float k, alpha)
        let ck = c / Math.Pow(float k, gamma)

        let delta, calls = sampleRademacher rng active
        randomCalls <- randomCalls + calls

        let xPlus = Array.copy x
        let xMinus = Array.copy x
        for i in 0 .. x.Length - 1 do
          if active.[i] then
            xPlus.[i] <- x.[i] + ck * delta.[i]
            xMinus.[i] <- x.[i] - ck * delta.[i]
        clampNormVector xPlus
        clampNormVector xMinus

        let plusOptions = optionsFromVector baseEngine.Options resolved xPlus
        let minusOptions = optionsFromVector baseEngine.Options resolved xMinus

        let plusName = sprintf "%s[+]" baseEngine.Name
        let minusName = sprintf "%s[-]" baseEngine.Name

        printfn ""
        printfn "--- SPSA iteration %d/%d (phase: %s, candidate #%d)  ak=%.4f  ck=%.4f ---"
          (iterationInPhase + 1) phase.Iterations phase.Name (candidateCount + 1) ak ck
        for i in 0 .. resolved.Length - 1 do
          if active.[i] then
            let p = resolved.[i]
            let vPlus = fromNorm p xPlus.[i]
            let vMinus = fromNorm p xMinus.[i]
            let label = match p.EmbeddedKey with Some key -> sprintf "%s|%s" p.OptionKey key | None -> p.OptionKey
            printfn "  %-20s  [+] = %-8g  [-] = %g" label vPlus vMinus

        candidateCount <- candidateCount + 1

        let scoreFrac, stats, winner, accuracy =
          match setup.EvalMode with
          | "puzzle" | "eret" ->
            let accPlus = evaluateAccuracy setup plusOptions plusName
            let accMinus = evaluateAccuracy setup minusOptions minusName
            let sf = 0.5 + (accPlus - accMinus) / 2.0
            let w = if accPlus >= accMinus then plusName else minusName
            let syntheticStats =
              { Wins = 0; Draws = 0; Losses = 0; Games = 0
                Llr = 0.0; Elo = 0.0
                Decision = sprintf "%s_wins" (if accPlus >= accMinus then "plus" else "minus")
                StoppedEarly = false }
            sf, syntheticStats, w, (accPlus + accMinus) / 2.0
          | _ ->
            let pgnPath = makeMatchPgnPath "cmp" candidateCount
            let s = runSprtMatch cfg baseTournament baseEngine plusName plusOptions minusName minusOptions pgnPath "" null
            let w =
              match s.Decision with
              | "accept_h1" | "fallback_h1" -> plusName
              | _ -> minusName
            let sf = (float s.Wins + 0.5 * float s.Draws) / float s.Games
            sf, s, w, 0.0

        let yDiff = 2.0 * (scoreFrac - 0.5) // continuous signal in [-1, 1]
        for i in 0 .. x.Length - 1 do
          if active.[i] then
            let g = yDiff / (2.0 * ck * delta.[i])
            let step = clamp -0.25 0.25 (ak * g)
            x.[i] <- x.[i] + step
        clampNormVector x

        match setup.EvalMode with
        | "puzzle" | "eret" ->
          printfn "  Winner: %s (accuracy mode, scoreFrac=%.4f)" winner scoreFrac
        | _ ->
          printfn "  Winner: %s (%s, %d/%d/%d, elo=%.2f)" winner stats.Decision stats.Wins stats.Draws stats.Losses stats.Elo
        printCurrentValues "Current x" x

        globalIteration <- globalIteration + 1
        iterationInPhase <- iterationInPhase + 1

        let history =
          { TimestampUtc = nowUtc()
            Phase = phase.Name
            Iteration = iterationInPhase
            Candidate = candidateCount
            Ak = ak
            Ck = ck
            Winner = winner
            MatchDecision = stats.Decision
            Games = stats.Games
            Wins = stats.Wins
            Draws = stats.Draws
            Losses = stats.Losses
            Llr = stats.Llr
            Elo = stats.Elo
            PredictedMean = 0.0
            PredictedStd = 0.0
            AcquisitionValue = 0.0
            Accuracy = accuracy }
        appendHistory historyPath history

        let checkpoint =
          { PhaseIndex = phaseIndex
            IterationInPhase = iterationInPhase
            GlobalIteration = globalIteration
            CandidateCount = candidateCount
            RandomCallsConsumed = randomCalls
            X = Array.copy x
            BestX = Array.copy bestX
            StartedUtc = state.StartedUtc
            LastUpdatedUtc = nowUtc()
            ObservationsX = null
            ObservationsY = null
            FinalValidationCompleted = false }
        saveState statePath checkpoint

        ()

      // Phase confirmation: current phase result must beat the phase start.
      if not (shouldStop startedUtc cfg candidateCount) then
        printfn "\n--- Phase confirmation: %s[current] vs %s[phase-start] ---" baseEngine.Name baseEngine.Name
        candidateCount <- candidateCount + 1
        let curName = sprintf "%s[current]" baseEngine.Name
        let phaseStartName = sprintf "%s[phase-start]" baseEngine.Name
        let curOptions = optionsFromVector baseEngine.Options resolved x
        let startOptions = optionsFromVector baseEngine.Options resolved phaseStartX
        let keepCurrent =
          match setup.EvalMode with
          | "puzzle" | "eret" ->
            let (aBeatB, _, _) = runComparisonByAccuracy setup curName curOptions phaseStartName startOptions
            aBeatB
          | _ when cfg.UseOpponentForValidation && setup.OpponentEngine.IsSome ->
            let pgnA = makeMatchPgnPath "phase_a" candidateCount
            let pgnB = makeMatchPgnPath "phase_b" candidateCount
            runOpponentComparison setup curName curOptions phaseStartName startOptions pgnA pgnB
          | _ ->
            let pgnPath = makeMatchPgnPath "phase" candidateCount
            let confirm = runSprtMatch cfg baseTournament baseEngine curName curOptions phaseStartName startOptions pgnPath "" null
            match confirm.Decision with
            | "accept_h1" | "fallback_h1" -> true
            | _ -> false
        if not keepCurrent then
          printfn "  Phase confirmation failed — reverting to phase start"
          x <- Array.copy phaseStartX
        else
          printfn "  Phase confirmation passed"
        printCurrentValues "Current x" x

        // Compare against incumbent best and keep best.
        printfn "\n--- Best comparison: %s[current] vs %s[best] ---" baseEngine.Name baseEngine.Name
        candidateCount <- candidateCount + 1
        let curName2 = sprintf "%s[current]" baseEngine.Name
        let bestName = sprintf "%s[best]" baseEngine.Name
        let curOptions2 = optionsFromVector baseEngine.Options resolved x
        let bestOptions = optionsFromVector baseEngine.Options resolved bestX
        let currentBeatsBest =
          match setup.EvalMode with
          | "puzzle" | "eret" ->
            let (aBeatB, _, _) = runComparisonByAccuracy setup curName2 curOptions2 bestName bestOptions
            aBeatB
          | _ when cfg.UseOpponentForValidation && setup.OpponentEngine.IsSome ->
            let pgnA = makeMatchPgnPath "best_a" candidateCount
            let pgnB = makeMatchPgnPath "best_b" candidateCount
            runOpponentComparison setup curName2 curOptions2 bestName bestOptions pgnA pgnB
          | _ ->
            let pgnPath2 = makeMatchPgnPath "best" candidateCount
            let cmpBest = runSprtMatch cfg baseTournament baseEngine curName2 curOptions2 bestName bestOptions pgnPath2 "" null
            match cmpBest.Decision with
            | "accept_h1" | "fallback_h1" -> true
            | _ -> false
        if currentBeatsBest then
          printfn "  New best found"
          bestX <- Array.copy x
        else
          printfn "  Keeping previous best"
          x <- Array.copy bestX
        printCurrentValues "Best x" bestX

      phaseIndex <- phaseIndex + 1
      iterationInPhase <- 0

      let checkpoint =
        { PhaseIndex = phaseIndex
          IterationInPhase = iterationInPhase
          GlobalIteration = globalIteration
          CandidateCount = candidateCount
          RandomCallsConsumed = randomCalls
          X = Array.copy x
          BestX = Array.copy bestX
          StartedUtc = state.StartedUtc
          LastUpdatedUtc = nowUtc()
          ObservationsX = null
          ObservationsY = null
          FinalValidationCompleted = false }
      saveState statePath checkpoint

    // Final validation: best vs initial.
    if not state.FinalValidationCompleted then
      printfn "\n--- Final validation: %s[tuned] vs %s[initial] ---" baseEngine.Name baseEngine.Name
      let tunedName = sprintf "%s[tuned]" baseEngine.Name
      let initialName = sprintf "%s[initial]" baseEngine.Name
      let finalCandidateOptions = optionsFromVector baseEngine.Options resolved bestX
      let initialOptions = optionsFromVector baseEngine.Options resolved startX

      let finalStats, finalAccTuned, finalAccInitial =
        match setup.EvalMode with
        | "puzzle" | "eret" ->
          let (_, accT, accI) = runComparisonByAccuracy setup tunedName finalCandidateOptions initialName initialOptions
          let synth = { Wins = 0; Draws = 0; Losses = 0; Games = 0; Llr = 0.0; Elo = 0.0
                        Decision = (if accT >= accI then "tuned_wins" else "initial_wins"); StoppedEarly = false }
          synth, Some accT, Some accI
        | _ when cfg.UseOpponentForValidation && setup.OpponentEngine.IsSome ->
          let pgnA = Path.Combine(pgnDir, sprintf "%s_final_validation_tuned%s" pgnBaseName pgnExt)
          let pgnB = Path.Combine(pgnDir, sprintf "%s_final_validation_initial%s" pgnBaseName pgnExt)
          let tunedWins = runOpponentComparison setup tunedName finalCandidateOptions initialName initialOptions pgnA pgnB
          let synth = { Wins = 0; Draws = 0; Losses = 0; Games = 0; Llr = 0.0; Elo = 0.0
                        Decision = (if tunedWins then "tuned_wins" else "initial_wins"); StoppedEarly = false }
          synth, None, None
        | _ ->
          let finalPgnPath = Path.Combine(pgnDir, sprintf "%s_final_validation%s" pgnBaseName pgnExt)
          let s = runSprtMatch cfg baseTournament baseEngine tunedName finalCandidateOptions initialName initialOptions finalPgnPath "" null
          s, None, None

      let tunedPairs = tunedValuesFromVector baseEngine.Options resolved bestX
      writeBestOptions bestOptionsPath tunedPairs

      let elapsed = DateTime.UtcNow - startedUtc
      let summary = summaryText cfg resolved bestX finalStats candidateCount elapsed setup.EvalMode finalAccTuned finalAccInitial
      File.WriteAllText(summaryPath, summary)

      let finalState =
        { PhaseIndex = phaseIndex
          IterationInPhase = iterationInPhase
          GlobalIteration = globalIteration
          CandidateCount = candidateCount
          RandomCallsConsumed = randomCalls
          X = Array.copy bestX
          BestX = Array.copy bestX
          StartedUtc = state.StartedUtc
          LastUpdatedUtc = nowUtc()
          ObservationsX = null
          ObservationsY = null
          FinalValidationCompleted = true }
      saveState statePath finalState
    else
      printfn "\n--- Skipping final validation (already completed on previous run) ---"

    printfn "\nTuning completed."
    printfn "- state: %s" statePath
    printfn "- history: %s" historyPath
    printfn "- best options: %s" bestOptionsPath
    printfn "- summary: %s" summaryPath

  let runTune path =
    let setup = prepareTune path
    printTuneHeader setup
    runSpsaTune setup
