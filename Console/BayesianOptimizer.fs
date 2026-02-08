namespace ConsoleApp

open System
open System.IO
open System.Text.Json
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
          ObservationsY = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray }
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
          let s = TunerRunner.runMatchBetween cfg baseTournament candidateEngine opponent cfg.OpponentTargetNodes pgnPath
          let sf = (float s.Wins + 0.5 * float s.Draws) / float (max 1 s.Games)
          sf, s, 0.0, opponent.Name
        | _ ->
          let baselineName = sprintf "%s[initial]" baseEngine.Name
          let baselineOptions = TunerRunner.optionsFromVector baseEngine.Options resolved startX
          let pgnPath = makeMatchPgnPath "bo" candidateCount
          let s = TunerRunner.runSprtMatch cfg baseTournament baseEngine candidateName candidateOptions baselineName baselineOptions pgnPath
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
        printfn "\n--- Initial design (%d points) ---" initialDesignSize
        let lhsSamples = latinHypercubeSample initialDesignSize d active rng
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
          | _ ->
            let pgnPath = makeMatchPgnPath "phase" candidateCount
            let confirm = TunerRunner.runSprtMatch cfg baseTournament baseEngine curName curOptions phaseStartName phaseStartOptions pgnPath
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
      | _ ->
        let finalPgnPath = Path.Combine(pgnDir, sprintf "%s_final_validation%s" pgnBaseName pgnExt)
        let s = TunerRunner.runSprtMatch cfg baseTournament baseEngine tunedName finalCandidateOptions initialName initialOptions finalPgnPath
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
        ObservationsY = observations |> Seq.map (fun o -> o.Y) |> Seq.toArray }
    TunerRunner.saveState statePath finalState

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
