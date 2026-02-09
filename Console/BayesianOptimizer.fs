namespace ConsoleApp

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Collections.Generic
open Microsoft.Extensions.Logging
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open ChessLibrary.Configuration
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.Statistics

module BayesianOptimizer =

  // ──────────────────────────────────────────────────────────────────
  // Types
  // ──────────────────────────────────────────────────────────────────

  type GPHyperparameters =
    { SignalVariance: float     // σ_f²
      LengthScales: float[]    // l_i per dimension (ARD)
      NoiseVariance: float }   // σ_n²

  type GPModel =
    { Hyp: GPHyperparameters
      X: float[][]             // training inputs (n×d)
      Y: float[]               // training targets (n)
      L: Matrix<float>         // Cholesky factor of K + σ_n²I
      Alpha: Vector<float> }   // K⁻¹ y

  type BOObservation =
    { X: float[]
      Y: float }

  type BOResult =
    { BestX: float[]
      BestY: float
      Observations: BOObservation[] }

  // ──────────────────────────────────────────────────────────────────
  // GP Kernel
  // ──────────────────────────────────────────────────────────────────

  /// Squared Exponential kernel with Automatic Relevance Determination:
  /// k(x,x') = σ_f² * exp(-0.5 * Σ((x_i - x'_i)² / l_i²))
  let seArdKernel (hyp: GPHyperparameters) (x: float[]) (x': float[]) =
    let mutable sum = 0.0
    for i in 0 .. x.Length - 1 do
      let d = x.[i] - x'.[i]
      let l = hyp.LengthScales.[i]
      sum <- sum + d * d / (l * l)
    hyp.SignalVariance * exp(-0.5 * sum)

  /// Build the covariance matrix K + σ_n²I
  let buildCovarianceMatrix (hyp: GPHyperparameters) (xs: float[][]) =
    let n = xs.Length
    let k = DenseMatrix.create n n 0.0
    for i in 0 .. n - 1 do
      for j in i .. n - 1 do
        let v = seArdKernel hyp xs.[i] xs.[j]
        k.[i, j] <- v
        k.[j, i] <- v
      // Add noise to diagonal
      k.[i, i] <- k.[i, i] + hyp.NoiseVariance
    k

  // ──────────────────────────────────────────────────────────────────
  // GP Regression
  // ──────────────────────────────────────────────────────────────────

  /// Attempt Cholesky with increasing jitter on failure
  let private choleskyWithJitter (k: Matrix<float>) =
    let jitters = [| 0.0; 1e-6; 1e-5; 1e-4; 1e-3 |]
    let mutable result = None
    let mutable idx = 0
    while result.IsNone && idx < jitters.Length do
      try
        let mat =
          if jitters.[idx] = 0.0 then k
          else
            let copy = k.Clone()
            let n = copy.RowCount
            for i in 0 .. n - 1 do
              copy.[i, i] <- copy.[i, i] + jitters.[idx]
            copy
        result <- Some (mat.Cholesky().Factor)
      with _ ->
        idx <- idx + 1
    match result with
    | Some l -> l
    | None -> failwith "Cholesky decomposition failed even with jitter 1e-3"

  /// Fit a GP model to training data
  let fitGP (hyp: GPHyperparameters) (xs: float[][]) (ys: float[]) : GPModel =
    let n = xs.Length
    let k = buildCovarianceMatrix hyp xs
    let l = choleskyWithJitter k
    let yVec = DenseVector.ofArray ys
    // Solve L * z = y, then L^T * alpha = z  =>  alpha = K⁻¹ y
    let z = l.Solve(yVec)
    let alpha = l.Transpose().Solve(z)
    { Hyp = hyp; X = xs; Y = ys; L = l; Alpha = alpha }

  /// Predict posterior mean and variance at a new point
  let predict (gp: GPModel) (xStar: float[]) : float * float =
    let n = gp.X.Length
    let kStar = DenseVector.init n (fun i -> seArdKernel gp.Hyp gp.X.[i] xStar)
    let mu = kStar.DotProduct(gp.Alpha)
    let v = gp.L.Solve(kStar)
    let kss = seArdKernel gp.Hyp xStar xStar + gp.Hyp.NoiseVariance
    let variance = max 1e-10 (kss - v.DotProduct(v))
    mu, variance

  // ──────────────────────────────────────────────────────────────────
  // Hyperparameter Optimization
  // ──────────────────────────────────────────────────────────────────

  /// Log marginal likelihood: -0.5 * y^T α - Σ log(diag(L)) - n/2 * log(2π)
  let logMarginalLikelihood (xs: float[][]) (ys: float[]) (hyp: GPHyperparameters) =
    try
      let n = xs.Length
      let k = buildCovarianceMatrix hyp xs
      let l = choleskyWithJitter k
      let yVec = DenseVector.ofArray ys
      let z = l.Solve(yVec)
      let alpha = l.Transpose().Solve(z)
      let dataFit = -0.5 * yVec.DotProduct(alpha)
      let mutable logDet = 0.0
      for i in 0 .. n - 1 do
        logDet <- logDet + log (abs l.[i, i])
      let complexity = -logDet
      let constant = -0.5 * float n * log(2.0 * Math.PI)
      dataFit + complexity + constant
    with _ -> -infinity

  /// Grid search over hyperparameter candidates
  let optimizeHyperparameters (xs: float[][]) (ys: float[]) (d: int) : GPHyperparameters =
    let yStd =
      let mean = Array.average ys
      let variance = ys |> Array.sumBy (fun y -> (y - mean) ** 2.0) |> fun s -> s / float (max 1 (ys.Length - 1))
      max 0.01 (sqrt variance)

    let sigFCandidates = [| 0.1 * yStd * yStd; 0.5 * yStd * yStd; yStd * yStd; 2.0 * yStd * yStd |]
    let lenCandidates = [| 0.1; 0.3; 0.5; 1.0; 2.0 |]
    let noiseCandidates = [| 0.001; 0.01; 0.05; 0.1 |]

    let mutable bestLml = -infinity
    let mutable bestHyp =
      { SignalVariance = yStd * yStd
        LengthScales = Array.create d 0.5
        NoiseVariance = 0.05 }

    for sf in sigFCandidates do
      for len in lenCandidates do
        for noise in noiseCandidates do
          let hyp =
            { SignalVariance = sf
              LengthScales = Array.create d len
              NoiseVariance = noise }
          let lml = logMarginalLikelihood xs ys hyp
          if lml > bestLml then
            bestLml <- lml
            bestHyp <- hyp

    // ── Phase 2: coordinate-descent ARD refinement ──
    // Sweep each dimension's length scale independently to produce true per-dimension ARD.
    // Finer grid than the isotropic search to distinguish per-dimension sensitivity.
    // Only refine when we have enough data to distinguish dimensions.
    if xs.Length >= d + 2 then
      let ardCandidates = [| 0.05; 0.1; 0.15; 0.2; 0.3; 0.4; 0.5; 0.7; 1.0; 1.5; 2.0; 3.0 |]
      let ls = Array.copy bestHyp.LengthScales
      let mutable improved = true
      let mutable passes = 0
      while improved && passes < 3 do
        improved <- false
        passes <- passes + 1
        for dim in 0 .. d - 1 do
          let origLen = ls.[dim]
          for candidate in ardCandidates do
            if candidate <> origLen then
              ls.[dim] <- candidate
              let hyp = { bestHyp with LengthScales = Array.copy ls }
              let lml = logMarginalLikelihood xs ys hyp
              if lml > bestLml then
                bestLml <- lml
                bestHyp <- hyp
                improved <- true
              else
                ls.[dim] <- bestHyp.LengthScales.[dim]
          ls.[dim] <- bestHyp.LengthScales.[dim]

    bestHyp

  // ──────────────────────────────────────────────────────────────────
  // Acquisition Function
  // ──────────────────────────────────────────────────────────────────

  /// Expected Improvement: EI(x) = (μ - f_best) * Φ(z) + σ * φ(z)
  /// where z = (μ - f_best) / σ
  let expectedImprovement (gp: GPModel) (fBest: float) (x: float[]) : float =
    let mu, variance = predict gp x
    let sigma = sqrt variance
    if sigma < 1e-10 then 0.0
    else
      let z = (mu - fBest) / sigma
      let normal = Normal(0.0, 1.0)
      let ei = (mu - fBest) * normal.CumulativeDistribution(z) + sigma * normal.Density(z)
      max 0.0 ei

  // ──────────────────────────────────────────────────────────────────
  // Acquisition Optimization
  // ──────────────────────────────────────────────────────────────────

  let private clamp lo hi x = max lo (min hi x)

  /// Optimize acquisition by multi-start random + coordinate-wise refinement
  let optimizeAcquisition (gp: GPModel) (fBest: float) (d: int) (active: bool[]) (rng: Random) : float[] =
    let nRandom = 1000
    let nRefine = 20
    let stepSize = 0.05

    // Phase 1: random candidates
    let mutable bestX = Array.create d 0.0
    let mutable bestEi = -infinity

    for _ in 1 .. nRandom do
      let candidate = Array.init d (fun i ->
        if active.[i] then rng.NextDouble() * 2.0 - 1.0
        else 0.0)
      let ei = expectedImprovement gp fBest candidate
      if ei > bestEi then
        bestEi <- ei
        bestX <- candidate

    // Phase 2: coordinate-wise refinement around best
    let x = Array.copy bestX
    for _ in 1 .. nRefine do
      for i in 0 .. d - 1 do
        if active.[i] then
          let eiCurrent = expectedImprovement gp fBest x
          let saved = x.[i]
          // Try moving up
          x.[i] <- clamp -1.0 1.0 (saved + stepSize)
          let eiUp = expectedImprovement gp fBest x
          // Try moving down
          x.[i] <- clamp -1.0 1.0 (saved - stepSize)
          let eiDown = expectedImprovement gp fBest x
          // Keep the best direction
          if eiUp >= eiDown && eiUp > eiCurrent then
            x.[i] <- clamp -1.0 1.0 (saved + stepSize)
          elif eiDown > eiCurrent then
            x.[i] <- clamp -1.0 1.0 (saved - stepSize)
          else
            x.[i] <- saved

    x

  // ──────────────────────────────────────────────────────────────────
  // Latin Hypercube Sampling
  // ──────────────────────────────────────────────────────────────────

  /// Generate n points via Latin Hypercube Sampling in [-1, 1]^d
  /// Inactive dimensions are set to 0.0
  let latinHypercubeSample (n: int) (d: int) (active: bool[]) (rng: Random) : float[][] =
    // For each active dimension, create a random permutation of n strata
    let samples = Array.init n (fun _ -> Array.create d 0.0)
    for dim in 0 .. d - 1 do
      if active.[dim] then
        // Generate permutation of 0..n-1
        let perm = [| 0 .. n - 1 |]
        for i in n - 1 .. -1 .. 1 do
          let j = rng.Next(0, i + 1)
          let tmp = perm.[i]
          perm.[i] <- perm.[j]
          perm.[j] <- tmp
        for i in 0 .. n - 1 do
          let u = (float perm.[i] + rng.NextDouble()) / float n
          samples.[i].[dim] <- 2.0 * u - 1.0  // map [0,1] to [-1,1]
    samples

  let centeredLatinHypercubeSample
      (n: int) (d: int) (active: bool[]) (center: float[]) (radius: float) (rng: Random) : float[][] =
    let samples = latinHypercubeSample n d active rng
    for sample in samples do
      for dim in 0 .. d - 1 do
        if active.[dim] then
          sample.[dim] <- clamp -1.0 1.0 (center.[dim] + radius * sample.[dim])
    samples

  // ──────────────────────────────────────────────────────────────────
  // Testable BO Loop (engine-free)
  // ──────────────────────────────────────────────────────────────────

  /// Run Bayesian optimization loop using an arbitrary evaluate function.
  /// `evaluate x` returns the objective value (higher = better).
  /// Works in normalized [-1,1]^d space.
  let runBayesianLoop
      (startX: float[])
      (active: bool[])
      (iterations: int)
      (initialDesignSize: int)
      (seed: int)
      (evaluate: float[] -> float)
      : BOResult =
    let d = startX.Length
    let rng = Random(seed)
    let hypUpdateInterval = 5

    let observations = ResizeArray<BOObservation>()

    // Phase 1: initial design via LHS
    let designSize = if initialDesignSize > 0 then initialDesignSize else max 3 (2 * (active |> Array.filter id |> Array.length))
    let lhsSamples = latinHypercubeSample designSize d active rng
    for sample in lhsSamples do
      // Copy inactive dims from startX
      for i in 0 .. d - 1 do
        if not active.[i] then sample.[i] <- startX.[i]
      let y = evaluate sample
      observations.Add({ X = Array.copy sample; Y = y })

    // Also evaluate start point
    let y0 = evaluate startX
    observations.Add({ X = Array.copy startX; Y = y0 })

    let mutable bestObs = observations |> Seq.maxBy (fun o -> o.Y)
    let mutable currentHyp =
      { SignalVariance = 1.0
        LengthScales = Array.create d 0.5
        NoiseVariance = 0.05 }

    // Phase 2: BO iterations
    for iter in 0 .. iterations - 1 do
      let xs = observations |> Seq.map (fun o -> o.X) |> Seq.toArray
      let ys = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray

      // Re-optimize hyperparameters periodically
      if iter % hypUpdateInterval = 0 then
        currentHyp <- optimizeHyperparameters xs ys d

      let gp = fitGP currentHyp xs ys
      let fBest = bestObs.Y

      let xNew = optimizeAcquisition gp fBest d active rng
      // Copy inactive dims from startX
      for i in 0 .. d - 1 do
        if not active.[i] then xNew.[i] <- startX.[i]

      let yNew = evaluate xNew
      observations.Add({ X = Array.copy xNew; Y = yNew })

      if yNew > bestObs.Y then
        bestObs <- { X = Array.copy xNew; Y = yNew }

    { BestX = bestObs.X
      BestY = bestObs.Y
      Observations = observations.ToArray() }

  // ──────────────────────────────────────────────────────────────────
  // Dashboard
  // ──────────────────────────────────────────────────────────────────

  type internal DashboardHeader =
    { EngineName: string
      EvalMode: string
      Phase: string
      PhaseIndex: int
      TotalPhases: int
      Iteration: int
      TotalIterations: int
      Observations: int
      BestY: float
      ElapsedMinutes: float }

  type internal DashboardParamInfo =
    { Name: string
      Min: float
      Max: float
      Step: float
      IsLog: bool
      IsActive: bool
      BestValue: float }

  type internal DashboardSlice1D =
    { ParamName: string
      GridReal: float[]
      Mu: float[]
      Sigma: float[]
      Ei: float[]
      ObsReal: float[]
      ObsY: float[]
      BestReal: float }

  type internal DashboardContour2D =
    { ParamI: string
      ParamJ: string
      GridI: float[]
      GridJ: float[]
      Mu: float[][]
      ObsI: float[]
      ObsJ: float[]
      BestI: float
      BestJ: float }

  type internal DashboardHyperparams =
    { SignalVar: float
      LengthScales: float[]
      NoiseVar: float
      ParamNames: string[] }

  type internal DashboardHistoryRow =
    { Phase: string
      Iteration: int
      Candidate: int
      Wins: int
      Draws: int
      Losses: int
      Elo: float
      PredMu: float
      PredStd: float
      Ei: float
      Accuracy: float }

  type internal DashboardData =
    { Header: DashboardHeader
      Params: DashboardParamInfo[]
      Slices1D: DashboardSlice1D[]
      Contours2D: DashboardContour2D[]
      Hyperparams: DashboardHyperparams option
      ConvergenceY: float[]
      ConvergenceBest: float[]
      PredActualPairs: float[][]
      History: DashboardHistoryRow[]
      ExplorationParams: string[]
      ExplorationLines: float[][] }

  let private paramDisplayName (p: TunerRunner.ResolvedParameter) =
    match p.EmbeddedKey with
    | Some key -> sprintf "%s|%s" p.OptionKey key
    | None -> p.OptionKey

  let private sanitizeFloat (v: float) =
    if Double.IsFinite v then v else 0.0

  let private compute1DSlice
      (gp: GPModel) (fBest: float) (bestX: float[])
      (resolved: TunerRunner.ResolvedParameter[])
      (paramIdx: int) : DashboardSlice1D =
    let p = resolved.[paramIdx]
    let nGrid = 100
    let gridNorm = Array.init nGrid (fun i -> -1.0 + 2.0 * float i / float (nGrid - 1))
    let gridReal = Array.init nGrid (fun i -> TunerRunner.fromNorm p gridNorm.[i])
    let mu = Array.zeroCreate nGrid
    let sigma = Array.zeroCreate nGrid
    let ei = Array.zeroCreate nGrid
    for i in 0 .. nGrid - 1 do
      let xProbe = Array.copy bestX
      xProbe.[paramIdx] <- gridNorm.[i]
      let m, v = predict gp xProbe
      mu.[i] <- sanitizeFloat m
      sigma.[i] <- sanitizeFloat (sqrt (max 1e-10 v))
      ei.[i] <- sanitizeFloat (expectedImprovement gp fBest xProbe)
    // Project observations onto this axis
    let obs = gp.X
    let obsReal = Array.init obs.Length (fun i -> TunerRunner.fromNorm p obs.[i].[paramIdx])
    let obsY = Array.init obs.Length (fun i -> sanitizeFloat gp.Y.[i])
    let bestReal = TunerRunner.fromNorm p bestX.[paramIdx]
    { ParamName = paramDisplayName p
      GridReal = gridReal; Mu = mu; Sigma = sigma; Ei = ei
      ObsReal = obsReal; ObsY = obsY; BestReal = bestReal }

  let private compute2DContour
      (gp: GPModel) (bestX: float[])
      (resolved: TunerRunner.ResolvedParameter[])
      (idxI: int) (idxJ: int) : DashboardContour2D =
    let pI = resolved.[idxI]
    let pJ = resolved.[idxJ]
    let nGrid = 40
    let gridNormI = Array.init nGrid (fun i -> -1.0 + 2.0 * float i / float (nGrid - 1))
    let gridNormJ = Array.init nGrid (fun j -> -1.0 + 2.0 * float j / float (nGrid - 1))
    let gridRealI = Array.init nGrid (fun i -> TunerRunner.fromNorm pI gridNormI.[i])
    let gridRealJ = Array.init nGrid (fun j -> TunerRunner.fromNorm pJ gridNormJ.[j])
    let muGrid = Array.init nGrid (fun i ->
      Array.init nGrid (fun j ->
        let xProbe = Array.copy bestX
        xProbe.[idxI] <- gridNormI.[i]
        xProbe.[idxJ] <- gridNormJ.[j]
        let m, _ = predict gp xProbe
        sanitizeFloat m))
    let obs = gp.X
    let obsI = Array.init obs.Length (fun k -> TunerRunner.fromNorm pI obs.[k].[idxI])
    let obsJ = Array.init obs.Length (fun k -> TunerRunner.fromNorm pJ obs.[k].[idxJ])
    let bestI = TunerRunner.fromNorm pI bestX.[idxI]
    let bestJ = TunerRunner.fromNorm pJ bestX.[idxJ]
    { ParamI = paramDisplayName pI; ParamJ = paramDisplayName pJ
      GridI = gridRealI; GridJ = gridRealJ; Mu = muGrid
      ObsI = obsI; ObsJ = obsJ; BestI = bestI; BestJ = bestJ }

  let private computeConvergence (observations: ResizeArray<BOObservation>) =
    let ys = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray
    let bestSoFar = Array.zeroCreate ys.Length
    let mutable best = -infinity
    for i in 0 .. ys.Length - 1 do
      if ys.[i] > best then best <- ys.[i]
      bestSoFar.[i] <- best
    ys, bestSoFar

  let private readPredictionAccuracy (historyPath: string) =
    try
      if not (File.Exists historyPath) then [||]
      else
        File.ReadAllLines historyPath
        |> Array.choose (fun line ->
          try
            let entry = JsonSerializer.Deserialize<TunerRunner.HistoryEntry>(line)
            if entry.PredictedStd > 0.0 then
              let games = entry.Wins + entry.Draws + entry.Losses
              let actualScoreFrac =
                if games > 0 then (float entry.Wins + 0.5 * float entry.Draws) / float games
                else 0.5
              Some [| sanitizeFloat entry.PredictedMean; sanitizeFloat actualScoreFrac |]
            else None
          with _ -> None)
    with _ -> [||]

  let private readHistoryRows (historyPath: string) (maxRows: int) =
    try
      if not (File.Exists historyPath) then [||]
      else
        let lines = File.ReadAllLines historyPath
        let start = max 0 (lines.Length - maxRows)
        lines.[start..]
        |> Array.choose (fun line ->
          try
            let e = JsonSerializer.Deserialize<TunerRunner.HistoryEntry>(line)
            Some { Phase = e.Phase; Iteration = e.Iteration; Candidate = e.Candidate
                   Wins = e.Wins; Draws = e.Draws; Losses = e.Losses
                   Elo = sanitizeFloat e.Elo; PredMu = sanitizeFloat e.PredictedMean
                   PredStd = sanitizeFloat e.PredictedStd; Ei = sanitizeFloat e.AcquisitionValue
                   Accuracy = sanitizeFloat e.Accuracy }
          with _ -> None)
    with _ -> [||]

  let private buildDashboardData
      (gp: GPModel option) (currentHyp: GPHyperparameters)
      (observations: ResizeArray<BOObservation>) (bestX: float[]) (bestY: float)
      (resolved: TunerRunner.ResolvedParameter[]) (active: bool[])
      (setup: TunerRunner.TuneSetupData)
      (phase: TunerRunner.TunePhase) (phaseIndex: int) (iterationInPhase: int)
      (candidateCount: int) (startedUtc: DateTime) (historyPath: string) : DashboardData =
    let elapsed = (DateTime.UtcNow - startedUtc).TotalMinutes
    let header =
      { EngineName = setup.BaseEngine.Name
        EvalMode = setup.EvalMode
        Phase = phase.Name
        PhaseIndex = phaseIndex + 1
        TotalPhases = setup.Config.Phases.Length
        Iteration = iterationInPhase
        TotalIterations = phase.Iterations
        Observations = observations.Count
        BestY = sanitizeFloat bestY
        ElapsedMinutes = Math.Round(elapsed, 1) }

    let paramInfos = resolved |> Array.mapi (fun i p ->
      { Name = paramDisplayName p; Min = p.Def.Min; Max = p.Def.Max
        Step = p.Def.Step; IsLog = p.ScaleIsLog; IsActive = active.[i]
        BestValue = TunerRunner.fromNorm p bestX.[i] })

    let slices =
      match gp with
      | None -> [||]
      | Some g ->
        let fBest = bestY
        resolved |> Array.mapi (fun i _ -> compute1DSlice g fBest bestX resolved i)

    let contours =
      match gp with
      | None -> [||]
      | Some g ->
        let activeIndices = [| for i in 0 .. resolved.Length - 1 do if active.[i] then yield i |]
        if activeIndices.Length < 2 then [||]
        else
          // Build pairs sorted by inverse length scale (most important first)
          let pairs =
            [| for a in 0 .. activeIndices.Length - 2 do
                 for b in a + 1 .. activeIndices.Length - 1 do
                   let iA = activeIndices.[a]
                   let iB = activeIndices.[b]
                   let importance = 1.0 / g.Hyp.LengthScales.[iA] + 1.0 / g.Hyp.LengthScales.[iB]
                   yield (iA, iB, importance) |]
            |> Array.sortByDescending (fun (_, _, imp) -> imp)
            |> Array.truncate 15
          pairs |> Array.map (fun (iA, iB, _) -> compute2DContour g bestX resolved iA iB)

    let convergenceY, convergenceBest = computeConvergence observations

    let hyperparams =
      match gp with
      | None -> None
      | Some g ->
        Some { SignalVar = sanitizeFloat g.Hyp.SignalVariance
               LengthScales = g.Hyp.LengthScales |> Array.map sanitizeFloat
               NoiseVar = sanitizeFloat g.Hyp.NoiseVariance
               ParamNames = resolved |> Array.map paramDisplayName }

    let predAccuracy = readPredictionAccuracy historyPath
    let historyRows = readHistoryRows historyPath 50

    let explorationParams = resolved |> Array.map paramDisplayName
    let explorationLines =
      observations |> Seq.map (fun o ->
        let vals = Array.init resolved.Length (fun i -> TunerRunner.fromNorm resolved.[i] o.X.[i])
        Array.append vals [| sanitizeFloat o.Y |]) |> Seq.toArray

    { Header = header; Params = paramInfos
      Slices1D = slices; Contours2D = contours; Hyperparams = hyperparams
      ConvergenceY = convergenceY |> Array.map sanitizeFloat
      ConvergenceBest = convergenceBest |> Array.map sanitizeFloat
      PredActualPairs = predAccuracy; History = historyRows
      ExplorationParams = explorationParams; ExplorationLines = explorationLines }

  let private dashboardDataToJson (data: DashboardData) : string =
    let roundFloat (v: float) = Math.Round(v, 6)
    let jf (v: float) = JsonValue.Create(roundFloat v) :> JsonNode
    let ji (v: int) = JsonValue.Create(v) :> JsonNode
    let js (v: string) = if isNull v then JsonValue.Create("") :> JsonNode else JsonValue.Create(v) :> JsonNode
    let jb (v: bool) = JsonValue.Create(v) :> JsonNode
    let fa (arr: float[]) = JsonArray(arr |> Array.map jf)
    let sa (arr: string[]) = JsonArray(arr |> Array.map js)
    let fa2 (arr: float[][]) = JsonArray(arr |> Array.map (fun row -> fa row :> JsonNode))

    let header = JsonObject()
    header["engineName"] <- data.Header.EngineName |> js
    header["evalMode"] <- data.Header.EvalMode |> js
    header["phase"] <- data.Header.Phase |> js
    header["phaseIndex"] <- data.Header.PhaseIndex |> ji
    header["totalPhases"] <- data.Header.TotalPhases |> ji
    header["iteration"] <- data.Header.Iteration |> ji
    header["totalIterations"] <- data.Header.TotalIterations |> ji
    header["observations"] <- data.Header.Observations |> ji
    header["bestY"] <- data.Header.BestY |> jf
    header["elapsedMinutes"] <- data.Header.ElapsedMinutes |> jf

    let paramArr = JsonArray(data.Params |> Array.map (fun p ->
      let o = JsonObject()
      o["name"] <- p.Name |> js
      o["min"] <- p.Min |> jf
      o["max"] <- p.Max |> jf
      o["step"] <- p.Step |> jf
      o["isLog"] <- p.IsLog |> jb
      o["isActive"] <- p.IsActive |> jb
      o["bestValue"] <- p.BestValue |> jf
      o :> JsonNode))

    let slicesArr = JsonArray(data.Slices1D |> Array.map (fun s ->
      let o = JsonObject()
      o["paramName"] <- s.ParamName |> js
      o["gridReal"] <- fa s.GridReal
      o["mu"] <- fa s.Mu
      o["sigma"] <- fa s.Sigma
      o["ei"] <- fa s.Ei
      o["obsReal"] <- fa s.ObsReal
      o["obsY"] <- fa s.ObsY
      o["bestReal"] <- s.BestReal |> jf
      o :> JsonNode))

    let contoursArr = JsonArray(data.Contours2D |> Array.map (fun c ->
      let o = JsonObject()
      o["paramI"] <- c.ParamI |> js
      o["paramJ"] <- c.ParamJ |> js
      o["gridI"] <- fa c.GridI
      o["gridJ"] <- fa c.GridJ
      o["mu"] <- fa2 c.Mu
      o["obsI"] <- fa c.ObsI
      o["obsJ"] <- fa c.ObsJ
      o["bestI"] <- c.BestI |> jf
      o["bestJ"] <- c.BestJ |> jf
      o :> JsonNode))

    let historyArr = JsonArray(data.History |> Array.map (fun h ->
      let o = JsonObject()
      o["phase"] <- h.Phase |> js
      o["iteration"] <- h.Iteration |> ji
      o["candidate"] <- h.Candidate |> ji
      o["wins"] <- h.Wins |> ji
      o["draws"] <- h.Draws |> ji
      o["losses"] <- h.Losses |> ji
      o["elo"] <- h.Elo |> jf
      o["predMu"] <- h.PredMu |> jf
      o["predStd"] <- h.PredStd |> jf
      o["ei"] <- h.Ei |> jf
      o["accuracy"] <- h.Accuracy |> jf
      o :> JsonNode))

    let root = JsonObject()
    root["header"] <- header
    root["params"] <- paramArr
    root["slices1D"] <- slicesArr
    root["contours2D"] <- contoursArr
    root["convergenceY"] <- fa data.ConvergenceY
    root["convergenceBest"] <- fa data.ConvergenceBest
    root["predActualPairs"] <- fa2 data.PredActualPairs
    root["history"] <- historyArr
    root["explorationParams"] <- sa data.ExplorationParams
    root["explorationLines"] <- fa2 data.ExplorationLines

    match data.Hyperparams with
    | None -> ()
    | Some hp ->
      let hObj = JsonObject()
      hObj["signalVar"] <- hp.SignalVar |> jf
      hObj["lengthScales"] <- fa hp.LengthScales
      hObj["noiseVar"] <- hp.NoiseVar |> jf
      hObj["paramNames"] <- sa hp.ParamNames
      root["hyperparams"] <- hObj

    root.ToJsonString(JsonSerializerOptions())

  let private renderDashboardHtml (data: DashboardData) : string =
    let sb = System.Text.StringBuilder(65536)
    let inline w (s: string) = sb.Append(s) |> ignore
    let inline wl (s: string) = sb.AppendLine(s) |> ignore

    let dataJson = dashboardDataToJson data

    wl "<!DOCTYPE html>"
    wl "<html lang='en'>"
    wl "<head>"
    wl "<meta charset='UTF-8'>"
    wl "<meta http-equiv='refresh' content='120'>"
    wl "<meta name='viewport' content='width=device-width, initial-scale=1.0'>"
    w "<title>BO Dashboard — "; w data.Header.EngineName; wl "</title>"
    wl "<script src='https://cdn.plot.ly/plotly-2.35.2.min.js'></script>"
    wl "<style>"
    wl "* { box-sizing: border-box; margin: 0; padding: 0; }"
    wl "body { background: #1a1a2e; color: #c1c1c4; font-family: 'Segoe UI', system-ui, sans-serif; padding: 16px; }"
    wl ".header { background: #16213e; border-radius: 8px; padding: 16px 24px; margin-bottom: 16px; display: flex; flex-wrap: wrap; gap: 24px; align-items: center; }"
    wl ".header h1 { font-size: 1.3em; color: #e94560; margin-right: auto; }"
    wl ".header .stat { text-align: center; }"
    wl ".header .stat .label { font-size: 0.75em; color: #888; text-transform: uppercase; }"
    wl ".header .stat .value { font-size: 1.2em; font-weight: 600; color: #e2e2e2; }"
    wl ".grid { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-bottom: 16px; }"
    wl ".grid .full { grid-column: 1 / -1; }"
    wl ".card { background: #16213e; border-radius: 8px; padding: 12px; }"
    wl ".card h3 { font-size: 0.9em; color: #7fb3e0; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.5px; }"
    wl "table { width: 100%; border-collapse: collapse; font-size: 0.8em; }"
    wl "th { background: #0f3460; color: #e2e2e2; padding: 6px 8px; text-align: left; }"
    wl "td { padding: 4px 8px; border-bottom: 1px solid #1a1a2e; }"
    wl "tr:nth-child(even) td { background: #1a2240; }"
    wl "tr:hover td { background: #1a1a2e; }"
    wl ".refresh-note { text-align: center; color: #555; font-size: 0.75em; padding: 8px; }"
    wl ".card .desc { font-size: 0.75em; color: #666; margin-bottom: 6px; }"
    wl "</style>"
    wl "</head>"
    wl "<body>"

    // Header
    wl "<div class='header'>"
    w "<h1>"; w data.Header.EngineName; w " — Bayesian Optimizer"; wl "</h1>"
    w "<div class='stat'><div class='label'>Mode</div><div class='value'>"; w data.Header.EvalMode; wl "</div></div>"
    w "<div class='stat'><div class='label'>Phase</div><div class='value'>"; w data.Header.Phase
    w " ("; w (string data.Header.PhaseIndex); w "/"; w (string data.Header.TotalPhases); wl ")</div></div>"
    w "<div class='stat'><div class='label'>Iteration</div><div class='value'>"; w (string data.Header.Iteration)
    w "/"; w (string data.Header.TotalIterations); wl "</div></div>"
    w "<div class='stat'><div class='label'>Observations</div><div class='value'>"; w (string data.Header.Observations); wl "</div></div>"
    w "<div class='stat'><div class='label'>Best Y</div><div class='value'>"; w (sprintf "%.4f" data.Header.BestY); wl "</div></div>"
    w "<div class='stat'><div class='label'>Elapsed</div><div class='value'>"; w (sprintf "%.0f min" data.Header.ElapsedMinutes); wl "</div></div>"
    wl "</div>"

    // Best Parameters summary card
    if data.Params.Length > 0 then
      wl "<div class='card' style='margin-bottom: 16px; display: flex; flex-wrap: wrap; gap: 12px 24px; align-items: center;'>"
      wl "<h3 style='width: 100%; margin-bottom: 4px;'>Best Parameters</h3>"
      for p in data.Params do
        w "<span style='font-size: 0.85em;'><span style='color: #888;'>"; w p.Name
        w "</span> = <span style='color: #e2e2e2; font-weight: 600;'>"; w (sprintf "%.4g" p.BestValue); wl "</span></span>"
      wl "</div>"

    wl "<div class='grid'>"

    // Convergence plot (full width)
    wl "<div class='card full'><h3>Convergence</h3><div class='desc'>Score per evaluation (dots) and best-so-far (line).</div><div id='convergence'></div></div>"

    // 1D slices
    if data.Slices1D.Length > 0 then
      wl "<div class='card full'><div class='desc'>1D GP slices &mdash; predicted mean, &plusmn;2&sigma; band, observations, EI, and best value (green dashed).</div></div>"
    for i in 0 .. data.Slices1D.Length - 1 do
      w "<div class='card'><div id='slice"; w (string i); wl "'></div></div>"

    // Length scales (if GP fitted)
    if data.Hyperparams.IsSome then
      wl "<div class='card'><h3>Parameter Importance</h3><div class='desc'>Relative sensitivity of objective to each parameter (taller = more important).</div><div id='lengthscales'></div></div>"

    // Prediction accuracy
    if data.PredActualPairs.Length > 0 then
      wl "<div class='card'><h3>Prediction Accuracy</h3><div class='desc'>GP predicted score vs actual match result. Points near the diagonal mean the model is predicting well. Scattered points suggest the model needs more data.</div><div id='predaccuracy'></div></div>"

    // 2D contours (full width each)
    if data.Contours2D.Length > 0 then
      wl "<div class='card full'><div class='desc'>2D contour maps &mdash; GP predicted objective for parameter pairs (most important first).</div></div>"
    for i in 0 .. data.Contours2D.Length - 1 do
      w "<div class='card full'><div id='contour"; w (string i); wl "'></div></div>"

    // Parallel coordinates / exploration (full width)
    if data.ExplorationLines.Length > 0 then
      wl "<div class='card full'><h3>Exploration Map</h3><div class='desc'>Parallel coordinates showing all evaluated parameter combinations. Each line is one candidate. Color indicates the objective score (brighter = better). Clusters of bright lines reveal promising parameter regions.</div><div id='exploration'></div></div>"

    // History table (full width)
    if data.History.Length > 0 then
      wl "<div class='card full'><h3>Recent Evaluations</h3>"
      wl "<table><tr><th>Phase</th><th>Iter</th><th>#</th><th>W</th><th>D</th><th>L</th><th>Elo</th><th>Pred&mu;</th><th>Pred&sigma;</th><th>EI</th><th>Accuracy</th></tr>"
      for row in data.History do
        wl "<tr>"
        w "<td>"; w row.Phase; wl "</td>"
        w "<td>"; w (string row.Iteration); wl "</td>"
        w "<td>"; w (string row.Candidate); wl "</td>"
        w "<td>"; w (string row.Wins); wl "</td>"
        w "<td>"; w (string row.Draws); wl "</td>"
        w "<td>"; w (string row.Losses); wl "</td>"
        w "<td>"; w (sprintf "%+.1f" row.Elo); wl "</td>"
        w "<td>"; w (sprintf "%.4f" row.PredMu); wl "</td>"
        w "<td>"; w (sprintf "%.4f" row.PredStd); wl "</td>"
        w "<td>"; w (sprintf "%.6f" row.Ei); wl "</td>"
        w "<td>"; w (if row.Accuracy > 0.0 then sprintf "%.4f" row.Accuracy else "-"); wl "</td>"
        wl "</tr>"
      wl "</table></div>"

    wl "</div>" // close grid

    wl "<div class='refresh-note'>Auto-refreshes every 2 minutes</div>"

    // JavaScript
    wl "<script>"
    w "const DATA = "; w dataJson; wl ";"
    wl ""
    wl "const LAYOUT_COMMON = {"
    wl "  paper_bgcolor: '#16213e', plot_bgcolor: '#16213e',"
    wl "  font: { color: '#c1c1c4', size: 11 },"
    wl "  margin: { l: 50, r: 30, t: 30, b: 40 },"
    wl "  xaxis: { gridcolor: '#2a2a4a', zerolinecolor: '#2a2a4a', automargin: true },"
    wl "  yaxis: { gridcolor: '#2a2a4a', zerolinecolor: '#2a2a4a', automargin: true }"
    wl "};"
    wl "const CONFIG = { responsive: true, displayModeBar: false };"
    wl ""
    // Convergence
    wl "if (DATA.convergenceY && DATA.convergenceY.length > 0) {"
    wl "  const idx = DATA.convergenceY.map((_, i) => i + 1);"
    wl "  Plotly.newPlot('convergence', ["
    wl "    { x: idx, y: DATA.convergenceY, mode: 'markers', name: 'Score',"
    wl "      marker: { color: '#e94560', size: 6, opacity: 0.6, line: { color: 'white', width: 0.5 } } },"
    wl "    { x: idx, y: DATA.convergenceBest, mode: 'lines', name: 'Best so far',"
    wl "      line: { color: '#00d2ff', width: 2 } }"
    wl "  ], { ...LAYOUT_COMMON, title: 'Convergence', xaxis: { ...LAYOUT_COMMON.xaxis, title: 'Evaluation #' },"
    wl "       yaxis: { ...LAYOUT_COMMON.yaxis, title: 'Score Fraction' } }, CONFIG);"
    wl "}"
    wl ""
    // 1D slices
    wl "DATA.slices1D.forEach((s, i) => {"
    wl "  const muUpper = s.mu.map((m, k) => m + 2 * s.sigma[k]);"
    wl "  const muLower = s.mu.map((m, k) => m - 2 * s.sigma[k]);"
    wl "  const traces = ["
    wl "    { x: s.gridReal, y: muUpper, mode: 'lines', line: { width: 0 }, showlegend: false, hoverinfo: 'skip' },"
    wl "    { x: s.gridReal, y: muLower, mode: 'lines', line: { width: 0 }, fill: 'tonexty',"
    wl "      fillcolor: 'rgba(0,210,255,0.22)', showlegend: false, hoverinfo: 'skip' },"
    wl "    { x: s.gridReal, y: s.mu, mode: 'lines', name: 'GP Mean',"
    wl "      line: { color: '#00d2ff', width: 2 } },"
    wl "    { x: s.obsReal, y: s.obsY, mode: 'markers', name: 'Observations',"
    wl "      marker: { color: '#e94560', size: 5, opacity: 0.7 } },"
    wl "    { x: s.gridReal, y: s.ei, mode: 'lines', name: 'EI', yaxis: 'y2',"
    wl "      line: { color: '#e9c46a', width: 1, dash: 'dot' } }"
    wl "  ];"
    wl "  const pInfo = DATA.params.find(p => p.name === s.paramName);"
    wl "  const bestValStr = pInfo ? pInfo.bestValue.toPrecision(4) : '';"
    wl "  const sliceTitle = (pInfo && !pInfo.isActive) ? s.paramName + ' = ' + bestValStr + ' (frozen)' : s.paramName + ' = ' + bestValStr;"
    wl "  const layout = { ...LAYOUT_COMMON, title: sliceTitle,"
    wl "    xaxis: { ...LAYOUT_COMMON.xaxis, title: s.paramName, tickformat: '.2g' },"
    wl "    yaxis: { ...LAYOUT_COMMON.yaxis, title: 'Score Fraction' },"
    wl "    yaxis2: { overlaying: 'y', side: 'right', showgrid: false, title: 'EI', titlefont: { color: '#e9c46a' }, tickfont: { color: '#e9c46a' } },"
    wl "    shapes: [{ type: 'line', x0: s.bestReal, x1: s.bestReal, y0: 0, y1: 1, yref: 'paper',"
    wl "              line: { color: '#2ecc71', width: 1, dash: 'dash' } }],"
    wl "    annotations: [],"
    wl "    showlegend: i === 0, height: 280 };"
    // Set log axis if param is log scale — remove tickformat so Plotly uses clean log labels
    wl "  if (pInfo && pInfo.isLog) { layout.xaxis.type = 'log'; layout.xaxis.dtick = 'D2'; layout.xaxis.tickformat = '.2g'; }"
    wl "  Plotly.newPlot('slice' + i, traces, layout, CONFIG);"
    wl "});"
    wl ""
    // Length scales
    wl "if (DATA.hyperparams) {"
    wl "  const hp = DATA.hyperparams;"
    wl "  const invLS = hp.lengthScales.map(l => l > 0 ? 1.0 / l : 0);"
    wl "  const maxInv = Math.max(...invLS, 1e-12);"
    wl "  const importance = invLS.map(v => v / maxInv);"
    wl "  Plotly.newPlot('lengthscales', [{ x: hp.paramNames, y: importance, type: 'bar',"
    wl "    marker: { color: importance.map(v => v > 0.7 ? '#e94560' : v > 0.3 ? '#e9c46a' : '#2ecc71') } }],"
    wl "  { ...LAYOUT_COMMON, height: 250, yaxis: { ...LAYOUT_COMMON.yaxis, title: 'Importance (normalized)', range: [0, 1.05] } }, CONFIG);"
    wl "}"
    wl ""
    // Prediction accuracy
    wl "if (DATA.predActualPairs && DATA.predActualPairs.length > 0) {"
    wl "  const predMu = DATA.predActualPairs.map(p => p[0]);"
    wl "  const actualY = DATA.predActualPairs.map(p => p[1]);"
    wl "  const allVals = predMu.concat(actualY);"
    wl "  const lo = Math.min(...allVals), hi = Math.max(...allVals);"
    wl "  Plotly.newPlot('predaccuracy', ["
    wl "    { x: predMu, y: actualY, mode: 'markers', marker: { color: '#00d2ff', size: 5, opacity: 0.7 } },"
    wl "    { x: [lo, hi], y: [lo, hi], mode: 'lines', line: { color: '#555', dash: 'dash' }, showlegend: false }"
    wl "  ], { paper_bgcolor: '#16213e', plot_bgcolor: '#16213e',"
    wl "       font: { color: '#c1c1c4', size: 11 },"
    wl "       margin: { l: 50, r: 30, t: 30, b: 40 }, height: 280, showlegend: false,"
    wl "       xaxis: { gridcolor: '#2a2a4a', zerolinecolor: '#2a2a4a', automargin: true, title: 'Predicted Mean', tickformat: '.3f' },"
    wl "       yaxis: { gridcolor: '#2a2a4a', zerolinecolor: '#2a2a4a', automargin: true, title: 'Actual Score', tickformat: '.3f' }"
    wl "  }, CONFIG);"
    wl "}"
    wl ""
    // 2D contours
    wl "DATA.contours2D.forEach((c, i) => {"
    wl "  const traces = ["
    wl "    { x: c.gridI, y: c.gridJ, z: c.mu, type: 'contour',"
    wl "      colorscale: 'Viridis', colorbar: { title: 'Mean', tickfont: { color: '#c1c1c4' } },"
    wl "      contours: { coloring: 'heatmap' } },"
    wl "    { x: c.obsI, y: c.obsJ, mode: 'markers', name: 'Obs',"
    wl "      marker: { color: '#e94560', size: 5, line: { color: 'white', width: 0.5 } } },"
    wl "    { x: [c.bestI], y: [c.bestJ], mode: 'markers', name: 'Best',"
    wl "      marker: { color: '#2ecc71', size: 12, symbol: 'star' } }"
    wl "  ];"
    wl "  const layout = { ...LAYOUT_COMMON, title: c.paramI + ' vs ' + c.paramJ,"
    wl "    xaxis: { ...LAYOUT_COMMON.xaxis, title: c.paramI, tickformat: '.2g' },"
    wl "    yaxis: { ...LAYOUT_COMMON.yaxis, title: c.paramJ, tickformat: '.2g' },"
    wl "    height: 400, showlegend: false };"
    wl "  const piI = DATA.params.find(p => p.name === c.paramI);"
    wl "  const piJ = DATA.params.find(p => p.name === c.paramJ);"
    wl "  if (piI && piI.isLog) { layout.xaxis.type = 'log'; layout.xaxis.dtick = 'D2'; layout.xaxis.tickformat = '.2g'; }"
    wl "  if (piJ && piJ.isLog) { layout.yaxis.type = 'log'; layout.yaxis.dtick = 'D2'; layout.yaxis.tickformat = '.2g'; }"
    wl "  Plotly.newPlot('contour' + i, traces, layout, CONFIG);"
    wl "});"
    wl ""
    // Exploration (parallel coordinates)
    wl "if (DATA.explorationLines && DATA.explorationLines.length > 1) {"
    wl "  const nParams = DATA.explorationParams.length;"
    wl "  const colorVals = DATA.explorationLines.map(l => l[nParams]);"  // last element is Y
    wl "  const dims = DATA.explorationParams.map((name, k) => ({"
    wl "    label: name, values: DATA.explorationLines.map(l => l[k]), tickformat: '.2g'"
    wl "  }));"
    wl "  Plotly.newPlot('exploration', [{ type: 'parcoords',"
    wl "    line: { color: colorVals, colorscale: 'Viridis', showscale: true, colorbar: { title: 'Score Fraction', tickfont: { color: '#c1c1c4' } } },"
    wl "    dimensions: dims }],"
    wl "  { ...LAYOUT_COMMON, height: 400, margin: { ...LAYOUT_COMMON.margin, t: 60 } }, CONFIG);"
    wl "}"
    wl ""
    wl "</script>"
    wl "</body></html>"
    sb.ToString()

  let private writeDashboard
      (path: string) (gp: GPModel option) (currentHyp: GPHyperparameters)
      (observations: ResizeArray<BOObservation>) (bestX: float[]) (bestY: float)
      (resolved: TunerRunner.ResolvedParameter[]) (active: bool[])
      (setup: TunerRunner.TuneSetupData)
      (phase: TunerRunner.TunePhase) (phaseIndex: int) (iterationInPhase: int)
      (candidateCount: int) (startedUtc: DateTime) (historyPath: string) =
    try
      let data = buildDashboardData gp currentHyp observations bestX bestY
                    resolved active setup phase phaseIndex iterationInPhase
                    candidateCount startedUtc historyPath
      let html = renderDashboardHtml data
      File.WriteAllText(path, html)
    with ex ->
      printfn "  Warning: dashboard write failed: %s" ex.Message

  // ──────────────────────────────────────────────────────────────────
  // Dashboard Regeneration
  // ──────────────────────────────────────────────────────────────────

  /// Regenerate the BO dashboard from saved state without re-running tuning.
  let internal regenerateDashboard (configPath: string) =
    let setup = TunerRunner.prepareTune configPath
    let resolved = setup.Resolved
    let state = setup.State
    let d = resolved.Length
    let dashboardPath = Path.Combine(setup.OutputDir, "bo-dashboard.html")

    // Rebuild observations from state
    let observations = ResizeArray<BOObservation>()
    if not (isNull state.ObservationsX) && not (isNull state.ObservationsY) then
      for i in 0 .. state.ObservationsY.Length - 1 do
        observations.Add({ X = state.ObservationsX.[i]; Y = state.ObservationsY.[i] })

    let bestX = Array.copy state.BestX
    let bestY =
      if observations.Count > 0 then (observations |> Seq.maxBy (fun o -> o.Y)).Y
      else 0.5

    // Determine active params from last/current phase
    let phaseIdx = min state.PhaseIndex (setup.Config.Phases.Length - 1)
    let phase = setup.Config.Phases.[phaseIdx]
    let paramIndex =
      let d = Dictionary<string,int>(StringComparer.OrdinalIgnoreCase)
      resolved |> Array.iteri (fun i p -> d.[p.Def.Name] <- i)
      d
    let active = Array.create d false
    for pname in phase.Parameters do
      active.[paramIndex.[pname]] <- true

    // Fit GP if we have enough observations
    let gpModel, hyp =
      if observations.Count >= 2 then
        let xs = observations |> Seq.map (fun o -> o.X) |> Seq.toArray
        let ys = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray
        let h = optimizeHyperparameters xs ys d
        Some (fitGP h xs ys), h
      else
        None, { SignalVariance = 1.0; LengthScales = Array.create d 0.5; NoiseVariance = 0.05 }

    let startedUtc =
      match DateTime.TryParse(state.StartedUtc) with
      | true, dt -> dt
      | _ -> DateTime.UtcNow

    let data = buildDashboardData gpModel hyp observations bestX bestY
                  resolved active setup phase phaseIdx state.IterationInPhase
                  state.CandidateCount startedUtc setup.HistoryPath
    let html = renderDashboardHtml data
    File.WriteAllText(dashboardPath, html)
    printfn "Dashboard regenerated: %s" dashboardPath

  // ──────────────────────────────────────────────────────────────────
  // Integration Entry Point
  // ──────────────────────────────────────────────────────────────────

  /// Run Bayesian optimization tuning with engine matches.
  /// Called from runTuneWithDispatch when optimizer = "bayesian".
  let internal runBayesianTune (setup: TunerRunner.TuneSetupData) =
    let cfg = setup.Config
    let baseEngine = setup.BaseEngine
    let resolved = setup.Resolved
    let baseTournament = setup.BaseTournament
    let startX = setup.StartX
    let state = setup.State
    let statePath = setup.StatePath
    let historyPath = setup.HistoryPath
    let bestOptionsPath = setup.BestOptionsPath
    let summaryPath = setup.SummaryPath
    let makeMatchPgnPath = setup.MakeMatchPgnPath
    let pgnDir = setup.PgnDir
    let pgnBaseName = setup.PgnBaseName
    let pgnExt = setup.PgnExt
    let startedUtc = setup.StartedUtc
    let dashboardPath = Path.Combine(setup.OutputDir, "bo-dashboard.html")

    let opponentDeviationFor =
      if cfg.PreventOpponentDeviation && setup.OpponentEngine.IsSome then [| setup.OpponentEngine.Value.Name |]
      else null
    let cumulativePgnPath = setup.CumulativePgnPath

    let d = resolved.Length
    let rng = Random(cfg.Seed + 7919)  // different seed from SPSA

    let mutable candidateCount = state.CandidateCount
    let mutable phaseIndex = state.PhaseIndex
    let mutable iterationInPhase = state.IterationInPhase
    let mutable globalIteration = state.GlobalIteration
    let mutable bestX = Array.copy state.BestX

    // Restore observations from state if resuming
    let observations = ResizeArray<BOObservation>()
    if not (isNull state.ObservationsX) && not (isNull state.ObservationsY) then
      for i in 0 .. state.ObservationsY.Length - 1 do
        observations.Add({ X = state.ObservationsX.[i]; Y = state.ObservationsY.[i] })

    let paramIndex =
      let d = Dictionary<string,int>(StringComparer.OrdinalIgnoreCase)
      resolved |> Array.iteri (fun i p -> d.[p.Def.Name] <- i)
      d

    /// Snap a normalized vector to the quantization grid (fromNorm → toNorm round-trip).
    /// This ensures the GP observes the actual quantized point that was evaluated.
    let snapToGrid (vec: float[]) =
      for i in 0 .. d - 1 do
        vec.[i] <- TunerRunner.toNorm resolved.[i] (TunerRunner.fromNorm resolved.[i] vec.[i])

    /// Check if a (snapped) vector already exists in observations.
    let isDuplicate (vec: float[]) =
      observations |> Seq.exists (fun o ->
        let mutable same = true
        for i in 0 .. d - 1 do
          if abs(o.X.[i] - vec.[i]) > 1e-12 then same <- false
        same)

    let saveCheckpoint () =
      let checkpoint: TunerRunner.TuneState =
        { PhaseIndex = phaseIndex
          IterationInPhase = iterationInPhase
          GlobalIteration = globalIteration
          CandidateCount = candidateCount
          RandomCallsConsumed = 0
          X = Array.copy bestX
          BestX = Array.copy bestX
          StartedUtc = state.StartedUtc
          LastUpdatedUtc = DateTime.UtcNow.ToString("O")
          ObservationsX = observations |> Seq.map (fun o -> o.X) |> Seq.toArray
          ObservationsY = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray
          FinalValidationCompleted = false }
      TunerRunner.saveState statePath checkpoint

    let shouldStop () =
      TunerRunner.shouldStop startedUtc cfg candidateCount

    let printCurrentValues label (vec: float[]) =
      printfn "  %s:" label
      for i in 0 .. resolved.Length - 1 do
        let p = resolved.[i]
        let v = TunerRunner.fromNorm p vec.[i]
        let name = match p.EmbeddedKey with Some key -> sprintf "%s|%s" p.OptionKey key | None -> p.OptionKey
        printfn "    %-20s = %g" name v

    // GP prediction context — set before each BO iteration call, read in evaluateCandidate
    let mutable lastPredMu = 0.0
    let mutable lastPredStd = 0.0
    let mutable lastEI = 0.0

    /// Evaluate a candidate vector via SPRT match against baseline or accuracy test
    let evaluateCandidate (xCandidate: float[]) : float =
      candidateCount <- candidateCount + 1
      let candidateName = sprintf "%s[bo-%d]" baseEngine.Name candidateCount
      let candidateOptions = TunerRunner.optionsFromVector baseEngine.Options resolved xCandidate

      printfn "  BO evaluation #%d" candidateCount
      for i in 0 .. resolved.Length - 1 do
        let p = resolved.[i]
        let v = TunerRunner.fromNorm p xCandidate.[i]
        let label = match p.EmbeddedKey with Some key -> sprintf "%s|%s" p.OptionKey key | None -> p.OptionKey
        printfn "    %-20s = %g" label v

      let scoreFrac, stats, accuracy, opponentName =
        match setup.EvalMode with
        | "puzzle" | "eret" ->
          let acc = TunerRunner.evaluateAccuracy setup candidateOptions candidateName
          let synth: TunerRunner.MatchStats =
            { Wins = 0; Draws = 0; Losses = 0; Games = 0; Llr = 0.0; Elo = 0.0
              Decision = sprintf "accuracy=%.4f" acc; StoppedEarly = false }
          acc, synth, acc, ""
        | _ when setup.OpponentEngine.IsSome ->
          let opponent = setup.OpponentEngine.Value
          let candidateEngine = TunerRunner.cloneEngineWithOptions candidateName baseEngine 1 candidateOptions
          let pgnPath = makeMatchPgnPath "bo" candidateCount
          let s = TunerRunner.runMatchBetween cfg baseTournament candidateEngine opponent cfg.OpponentTargetNodes pgnPath cumulativePgnPath opponentDeviationFor
          let sf = (float s.Wins + 0.5 * float s.Draws) / float (max 1 s.Games)
          sf, s, 0.0, opponent.Name
        | _ ->
          let baselineName = sprintf "%s[initial]" baseEngine.Name
          let baselineOptions = TunerRunner.optionsFromVector baseEngine.Options resolved startX
          let pgnPath = makeMatchPgnPath "bo" candidateCount
          let s = TunerRunner.runSprtMatch cfg baseTournament baseEngine candidateName candidateOptions baselineName baselineOptions pgnPath "" null
          let sf = (float s.Wins + 0.5 * float s.Draws) / float (max 1 s.Games)
          sf, s, 0.0, baselineName

      match setup.EvalMode with
      | "puzzle" | "eret" ->
        printfn "  Result: accuracy=%.4f" accuracy
      | _ ->
        let winner = if stats.Elo >= 0.0 then candidateName else opponentName
        printfn "  Result: %s vs %s → %d/%d/%d elo=%+.2f winner=%s [%s]"
          candidateName opponentName stats.Wins stats.Draws stats.Losses stats.Elo winner stats.Decision

      let actualWinner =
        match setup.EvalMode with
        | "puzzle" | "eret" -> candidateName
        | _ -> if stats.Elo >= 0.0 then candidateName else opponentName

      let history: TunerRunner.HistoryEntry =
        { TimestampUtc = DateTime.UtcNow.ToString("O")
          Phase = if phaseIndex < cfg.Phases.Length then cfg.Phases.[phaseIndex].Name else "final"
          Iteration = iterationInPhase
          Candidate = candidateCount
          Ak = 0.0
          Ck = 0.0
          Winner = actualWinner
          MatchDecision = stats.Decision
          Games = stats.Games
          Wins = stats.Wins
          Draws = stats.Draws
          Losses = stats.Losses
          Llr = stats.Llr
          Elo = stats.Elo
          PredictedMean = lastPredMu
          PredictedStd = lastPredStd
          AcquisitionValue = lastEI
          Accuracy = accuracy }
      TunerRunner.appendHistory historyPath history

      scoreFrac

    let mutable currentHyp =
      { SignalVariance = 1.0
        LengthScales = Array.create d 0.5
        NoiseVariance = 0.05 }

    let hypUpdateInterval =
      if cfg.HypUpdateInterval > 0 then cfg.HypUpdateInterval else 5

    let isSprtEval = setup.EvalMode <> "puzzle" && setup.EvalMode <> "eret"

    /// Temporarily scale SPRT game budget based on iteration progress (half → full).
    /// Returns a restore function to reset original values.
    let scaleGameBudget (iteration: int) (totalIterations: int) : (unit -> unit) =
      if not isSprtEval then (fun () -> ())
      else
        let origMax = cfg.Sprt.MaxGames
        let origMin = cfg.Sprt.MinGames
        let progress = float iteration / float (max 1 (totalIterations - 1))
        let scaledMax = int (Math.Round(float origMax * (0.5 + 0.5 * progress)))
        let scaledMin = min origMin scaledMax
        cfg.Sprt.MaxGames <- max scaledMin scaledMax
        cfg.Sprt.MinGames <- scaledMin
        printfn "  [budget scaled: games=%d..%d (%.0f%% of full)]" cfg.Sprt.MinGames cfg.Sprt.MaxGames (100.0 * (0.5 + 0.5 * progress))
        fun () ->
          cfg.Sprt.MaxGames <- origMax
          cfg.Sprt.MinGames <- origMin

    while phaseIndex < cfg.Phases.Length && not (shouldStop()) do
      let phase = cfg.Phases.[phaseIndex]
      let phaseEntryBestX = Array.copy bestX  // save pre-phase best for confirmation
      let active = Array.create d false
      for pname in phase.Parameters do
        let idx = paramIndex.[pname]
        active.[idx] <- true

      let activeCount = active |> Array.filter id |> Array.length
      let initialDesignSize =
        if cfg.InitialDesignSize > 0 then cfg.InitialDesignSize
        else max 3 (2 * activeCount)

      printfn ""
      printfn "=== BO Phase: %s (design=%d, iterations=%d) ===" phase.Name initialDesignSize phase.Iterations

      // Clear observations only when entering a new phase (not resuming mid-phase)
      if iterationInPhase = 0 then
        observations.Clear()

      // Phase 1: initial design via LHS (skip if resuming with existing observations)
      if observations.Count = 0 then
        let designRadius =
          if phaseIndex = 0 then 1.0
          elif cfg.InitialDesignRadius > 0.0 then cfg.InitialDesignRadius
          else 0.5
        printfn "\n--- Initial design (%d points, radius=%.2f) ---" initialDesignSize designRadius
        let lhsSamples =
          if designRadius >= 1.0 then latinHypercubeSample initialDesignSize d active rng
          else centeredLatinHypercubeSample initialDesignSize d active bestX designRadius rng
        // Initial design uses minimum budget (iteration 0 of phase)
        let restoreDesign = scaleGameBudget 0 phase.Iterations
        for sample in lhsSamples do
          if shouldStop() then () else
          // Copy inactive dims from current bestX
          for i in 0 .. d - 1 do
            if not active.[i] then sample.[i] <- bestX.[i]
          snapToGrid sample
          let y = evaluateCandidate sample
          observations.Add({ X = Array.copy sample; Y = y })
          saveCheckpoint()

        // Also evaluate current best
        if not (shouldStop()) then
          let y0 = evaluateCandidate bestX
          observations.Add({ X = Array.copy bestX; Y = y0 })
          saveCheckpoint()

        restoreDesign()

        // Write dashboard after initial design (no GP model yet)
        writeDashboard dashboardPath None currentHyp observations bestX 0.5
          resolved active setup phase phaseIndex iterationInPhase
          candidateCount startedUtc historyPath
        printfn "  Dashboard: %s" dashboardPath

      // Find current best from observations
      let mutable bestObs =
        if observations.Count > 0 then observations |> Seq.maxBy (fun o -> o.Y)
        else { X = Array.copy bestX; Y = 0.5 }

      // Phase 2: BO iterations
      printfn "\n--- BO iterations (%d) ---" phase.Iterations
      while iterationInPhase < phase.Iterations && not (shouldStop()) do
        let xs = observations |> Seq.map (fun o -> o.X) |> Seq.toArray
        let ys = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray

        // Re-optimize hyperparameters periodically
        if iterationInPhase % hypUpdateInterval = 0 then
          currentHyp <- optimizeHyperparameters xs ys d

        let gp = fitGP currentHyp xs ys
        let fBest = bestObs.Y

        let xNew = optimizeAcquisition gp fBest d active rng
        // Copy inactive dims
        for i in 0 .. d - 1 do
          if not active.[i] then xNew.[i] <- bestX.[i]

        // Snap to quantization grid so GP observes the actual evaluated point
        snapToGrid xNew

        // If this quantized point was already evaluated, sample a random replacement
        if isDuplicate xNew then
          printfn "  Duplicate detected after quantization — sampling random candidate"
          let mutable found = false
          for _ in 1 .. 100 do
            if not found then
              for i in 0 .. d - 1 do
                if active.[i] then xNew.[i] <- rng.NextDouble() * 2.0 - 1.0
                else xNew.[i] <- bestX.[i]
              snapToGrid xNew
              if not (isDuplicate xNew) then found <- true
          if not found then
            printfn "  Warning: could not find non-duplicate after 100 random attempts"

        // Log GP prediction for the new point and store for history
        let predMu, predVar = predict gp xNew
        let predStd = sqrt predVar
        let eiVal = expectedImprovement gp fBest xNew
        lastPredMu <- predMu
        lastPredStd <- predStd
        lastEI <- eiVal
        printfn "\n--- BO iteration %d/%d (candidate #%d) predMu=%.4f predStd=%.4f EI=%.6f ---"
          (iterationInPhase + 1) phase.Iterations (candidateCount + 1) predMu predStd eiVal

        let restoreIter = scaleGameBudget iterationInPhase phase.Iterations
        let yNew = evaluateCandidate xNew
        restoreIter()
        observations.Add({ X = Array.copy xNew; Y = yNew })

        if yNew > bestObs.Y then
          bestObs <- { X = Array.copy xNew; Y = yNew }
          bestX <- Array.copy xNew
          printfn "  New best found! scoreFrac=%.4f" yNew

        iterationInPhase <- iterationInPhase + 1
        globalIteration <- globalIteration + 1
        saveCheckpoint()

        // Update dashboard with current GP model
        writeDashboard dashboardPath (Some gp) currentHyp observations bestX bestObs.Y
          resolved active setup phase phaseIndex iterationInPhase
          candidateCount startedUtc historyPath

      // Phase confirmation: best found in this phase vs pre-phase best
      if not (shouldStop()) then
        printfn "\n--- Phase confirmation: %s[current] vs %s[phase-start] ---" baseEngine.Name baseEngine.Name
        candidateCount <- candidateCount + 1
        let curName = sprintf "%s[current]" baseEngine.Name
        let phaseStartName = sprintf "%s[phase-start]" baseEngine.Name
        let curOptions = TunerRunner.optionsFromVector baseEngine.Options resolved bestObs.X
        let phaseStartOptions = TunerRunner.optionsFromVector baseEngine.Options resolved phaseEntryBestX
        let keepCurrent =
          match setup.EvalMode with
          | "puzzle" | "eret" ->
            let (aBeatB, _, _) = TunerRunner.runComparisonByAccuracy setup curName curOptions phaseStartName phaseStartOptions
            aBeatB
          | _ when cfg.UseOpponentForValidation && setup.OpponentEngine.IsSome ->
            let pgnA = makeMatchPgnPath "phase_a" candidateCount
            let pgnB = makeMatchPgnPath "phase_b" candidateCount
            TunerRunner.runOpponentComparison setup curName curOptions phaseStartName phaseStartOptions pgnA pgnB
          | _ ->
            let pgnPath = makeMatchPgnPath "phase" candidateCount
            let confirm = TunerRunner.runSprtMatch cfg baseTournament baseEngine curName curOptions phaseStartName phaseStartOptions pgnPath "" null
            match confirm.Decision with
            | "accept_h1" | "fallback_h1" -> true
            | _ -> false
        if keepCurrent then
          printfn "  Phase confirmation passed — updating best"
          bestX <- Array.copy bestObs.X
        else
          printfn "  Phase confirmation failed — reverting to pre-phase best"
          bestX <- Array.copy phaseEntryBestX
        printCurrentValues "Best x" bestX
        saveCheckpoint()

      phaseIndex <- phaseIndex + 1
      iterationInPhase <- 0
      saveCheckpoint()

    // Final validation: best vs initial
    if not state.FinalValidationCompleted then
      printfn "\n--- Final validation: %s[tuned] vs %s[initial] ---" baseEngine.Name baseEngine.Name
      let tunedName = sprintf "%s[tuned]" baseEngine.Name
      let initialName = sprintf "%s[initial]" baseEngine.Name
      let finalCandidateOptions = TunerRunner.optionsFromVector baseEngine.Options resolved bestX
      let initialOptions = TunerRunner.optionsFromVector baseEngine.Options resolved startX

      let finalStats, finalAccTuned, finalAccInitial =
        match setup.EvalMode with
        | "puzzle" | "eret" ->
          let (_, accT, accI) = TunerRunner.runComparisonByAccuracy setup tunedName finalCandidateOptions initialName initialOptions
          let synth: TunerRunner.MatchStats =
            { Wins = 0; Draws = 0; Losses = 0; Games = 0; Llr = 0.0; Elo = 0.0
              Decision = (if accT >= accI then "tuned_wins" else "initial_wins"); StoppedEarly = false }
          synth, Some accT, Some accI
        | _ when cfg.UseOpponentForValidation && setup.OpponentEngine.IsSome ->
          let pgnA = Path.Combine(pgnDir, sprintf "%s_final_validation_tuned%s" pgnBaseName pgnExt)
          let pgnB = Path.Combine(pgnDir, sprintf "%s_final_validation_initial%s" pgnBaseName pgnExt)
          let tunedWins = TunerRunner.runOpponentComparison setup tunedName finalCandidateOptions initialName initialOptions pgnA pgnB
          let synth: TunerRunner.MatchStats =
            { Wins = 0; Draws = 0; Losses = 0; Games = 0; Llr = 0.0; Elo = 0.0
              Decision = (if tunedWins then "tuned_wins" else "initial_wins"); StoppedEarly = false }
          synth, None, None
        | _ ->
          let finalPgnPath = Path.Combine(pgnDir, sprintf "%s_final_validation%s" pgnBaseName pgnExt)
          let s = TunerRunner.runSprtMatch cfg baseTournament baseEngine tunedName finalCandidateOptions initialName initialOptions finalPgnPath "" null
          s, None, None

      let tunedPairs = TunerRunner.tunedValuesFromVector baseEngine.Options resolved bestX
      TunerRunner.writeBestOptions bestOptionsPath tunedPairs

      let elapsed = DateTime.UtcNow - startedUtc
      let summary = TunerRunner.summaryText cfg resolved bestX finalStats candidateCount elapsed setup.EvalMode finalAccTuned finalAccInitial
      File.WriteAllText(summaryPath, summary)

      let finalState: TunerRunner.TuneState =
        { PhaseIndex = phaseIndex
          IterationInPhase = iterationInPhase
          GlobalIteration = globalIteration
          CandidateCount = candidateCount
          RandomCallsConsumed = 0
          X = Array.copy bestX
          BestX = Array.copy bestX
          StartedUtc = state.StartedUtc
          LastUpdatedUtc = DateTime.UtcNow.ToString("O")
          ObservationsX = observations |> Seq.map (fun o -> o.X) |> Seq.toArray
          ObservationsY = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray
          FinalValidationCompleted = true }
      TunerRunner.saveState statePath finalState
    else
      printfn "\n--- Skipping final validation (already completed on previous run) ---"

    printfn "\nBayesian optimization tuning completed."
    printfn "- state: %s" statePath
    printfn "- history: %s" historyPath
    printfn "- best options: %s" bestOptionsPath
    printfn "- summary: %s" summaryPath

  /// Entry point that dispatches to either SPSA or Bayesian optimizer.
  /// Called from Program.fs instead of TunerRunner.runTune.
  let runTuneWithDispatch path =
    let setup = TunerRunner.prepareTune path
    TunerRunner.printTuneHeader setup
    match setup.Optimizer with
    | "bayesian" | "bo" -> runBayesianTune setup
    | _ -> TunerRunner.runSpsaTune setup
