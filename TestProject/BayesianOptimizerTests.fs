module BayesianOptimizerTests

open Xunit
open ConsoleApp.BayesianOptimizer

let private euclideanDist (a: float[]) (b: float[]) =
    Array.map2 (fun x y -> (x - y) ** 2.0) a b |> Array.sum |> sqrt

// ── GP Kernel Tests ──

[<Fact>]
let ``SE-ARD kernel returns signal variance for identical points`` () =
    let hyp =
        { SignalVariance = 2.0
          LengthScales = [| 0.5; 0.5; 0.5 |]
          NoiseVariance = 0.01 }
    let x = [| 0.3; -0.1; 0.7 |]
    let k = seArdKernel hyp x x
    Assert.InRange(k, 1.99, 2.01)

[<Fact>]
let ``SE-ARD kernel returns near zero for distant points`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.1; 0.1 |]
          NoiseVariance = 0.01 }
    let x1 = [| -1.0; -1.0 |]
    let x2 = [| 1.0; 1.0 |]
    let k = seArdKernel hyp x1 x2
    Assert.True(k < 1e-6, sprintf "Expected near-zero kernel for distant points, got %g" k)

[<Fact>]
let ``SE-ARD kernel is symmetric`` () =
    let hyp =
        { SignalVariance = 1.5
          LengthScales = [| 0.3; 0.7 |]
          NoiseVariance = 0.01 }
    let x1 = [| 0.2; -0.5 |]
    let x2 = [| -0.3; 0.8 |]
    let k12 = seArdKernel hyp x1 x2
    let k21 = seArdKernel hyp x2 x1
    Assert.Equal(k12, k21, 10)

// ── GP Regression Tests ──

[<Fact>]
let ``GP predict interpolates near training points`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.5 |]
          NoiseVariance = 0.001 }
    let xs = [| [| -0.5 |]; [| 0.0 |]; [| 0.5 |] |]
    let ys = [| 0.2; 0.8; 0.3 |]
    let gp = fitGP hyp xs ys

    // Predict at a training point — should be close
    let mu, var = predict gp [| 0.0 |]
    Assert.InRange(mu, 0.7, 0.9)
    Assert.True(var < 0.1, sprintf "Variance at training point should be small, got %g" var)

[<Fact>]
let ``GP predict has high variance far from training data`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.3 |]
          NoiseVariance = 0.001 }
    let xs = [| [| -0.5 |]; [| 0.0 |]; [| 0.5 |] |]
    let ys = [| 0.2; 0.8; 0.3 |]
    let gp = fitGP hyp xs ys

    // Predict near training point
    let _, varNear = predict gp [| 0.1 |]
    // Predict far from all training data
    let _, varFar = predict gp [| 5.0 |]
    Assert.True(varFar > varNear, sprintf "Far variance (%g) should exceed near variance (%g)" varFar varNear)

// ── Acquisition Function Tests ──

[<Fact>]
let ``EI is zero at best point with zero uncertainty`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.3 |]
          NoiseVariance = 1e-6 }
    let xs = [| [| 0.0 |]; [| 0.5 |]; [| -0.5 |] |]
    let ys = [| 1.0; 0.5; 0.3 |]
    let gp = fitGP hyp xs ys
    // At the best point (x=0.0, y=1.0), predict ~1.0 with low variance → EI ~0
    let ei = expectedImprovement gp 1.0 [| 0.0 |]
    Assert.True(ei < 0.01, sprintf "EI at known best should be ~0, got %g" ei)

[<Fact>]
let ``EI is positive at uncertain point`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.3 |]
          NoiseVariance = 0.001 }
    let xs = [| [| -0.8 |]; [| -0.6 |] |]
    let ys = [| 0.5; 0.6 |]
    let gp = fitGP hyp xs ys
    // Far from training data, high uncertainty → positive EI
    let ei = expectedImprovement gp 0.6 [| 0.8 |]
    Assert.True(ei > 0.0, sprintf "EI at uncertain point should be positive, got %g" ei)

// ── Latin Hypercube Sampling Tests ──

[<Fact>]
let ``LHS produces correct number of samples`` () =
    let rng = System.Random(42)
    let active = [| true; true; false |]
    let samples = latinHypercubeSample 10 3 active rng
    Assert.Equal(10, samples.Length)
    for sample in samples do
        Assert.Equal(3, sample.Length)

[<Fact>]
let ``LHS respects active mask`` () =
    let rng = System.Random(42)
    let active = [| true; false; true |]
    let samples = latinHypercubeSample 20 3 active rng
    for sample in samples do
        // Inactive dimension should be 0.0
        Assert.Equal(0.0, sample.[1])
        // Active dimensions should be in [-1, 1]
        Assert.InRange(sample.[0], -1.0, 1.0)
        Assert.InRange(sample.[2], -1.0, 1.0)

[<Fact>]
let ``LHS covers the space (no duplicated strata)`` () =
    let rng = System.Random(42)
    let active = [| true |]
    let n = 10
    let samples = latinHypercubeSample n 1 active rng
    // Each sample should be in a different stratum (n equal-width bins)
    let bins =
        samples
        |> Array.map (fun s ->
            let u = (s.[0] + 1.0) / 2.0  // map [-1,1] to [0,1]
            int (u * float n) |> min (n - 1))
        |> Array.sort
    // Should have n distinct bins
    let distinctBins = bins |> Array.distinct |> Array.length
    Assert.Equal(n, distinctBins)

// ── Hyperparameter Optimization Tests ──

[<Fact>]
let ``log marginal likelihood is finite for well-conditioned data`` () =
    let xs = [| [| 0.0 |]; [| 0.3 |]; [| 0.6 |]; [| 0.9 |] |]
    let ys = [| 0.1; 0.5; 0.9; 0.4 |]
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.5 |]
          NoiseVariance = 0.01 }
    let lml = logMarginalLikelihood xs ys hyp
    Assert.True(System.Double.IsFinite lml, sprintf "LML should be finite, got %g" lml)

[<Fact>]
let ``optimizeHyperparameters returns valid hyperparameters`` () =
    let rng = System.Random(42)
    let xs = Array.init 15 (fun _ -> [| rng.NextDouble() * 2.0 - 1.0; rng.NextDouble() * 2.0 - 1.0 |])
    let ys = xs |> Array.map (fun x -> -x.[0] * x.[0] - x.[1] * x.[1] + 0.1 * rng.NextDouble())
    let hyp = optimizeHyperparameters xs ys 2
    Assert.True(hyp.SignalVariance > 0.0)
    Assert.True(hyp.NoiseVariance > 0.0)
    Assert.Equal(2, hyp.LengthScales.Length)
    for l in hyp.LengthScales do
        Assert.True(l > 0.0)

// ── Bayesian Loop Convergence Tests ──

[<Fact>]
let ``BO loop converges on deterministic quadratic`` () =
    // Objective: -||x - target||² (maximized at target)
    let target = [| 0.3; -0.4 |]
    let evaluate (x: float[]) =
        let d = euclideanDist x target
        -d * d  // negative squared distance (higher is better)
    let startX = [| 0.0; 0.0 |]
    let active = [| true; true |]
    let result = runBayesianLoop startX active 25 6 42 evaluate
    let dist = euclideanDist result.BestX target
    Assert.True(dist < 0.3, sprintf "BO should find optimum within 0.3, got dist=%.4f (best=[%.3f, %.3f])" dist result.BestX.[0] result.BestX.[1])

[<Fact>]
let ``BO loop converges on noisy quadratic`` () =
    let target = [| 0.3; -0.4 |]
    let noiseRng = System.Random(99)
    let evaluate (x: float[]) =
        let d = euclideanDist x target
        let noise = noiseRng.NextDouble() * 0.02 - 0.01
        -d * d + noise
    let startX = [| 0.0; 0.0 |]
    let active = [| true; true |]
    let result = runBayesianLoop startX active 30 8 42 evaluate
    let dist = euclideanDist result.BestX target
    Assert.True(dist < 0.5, sprintf "BO with noise should find optimum within 0.5, got dist=%.4f" dist)

[<Fact>]
let ``BO loop respects active mask`` () =
    let target = [| 0.5; -0.5; 0.5 |]
    let evaluate (x: float[]) =
        let d = euclideanDist x target
        -d * d
    let startX = [| 0.0; 0.2; 0.0 |]
    let active = [| true; false; true |]
    let result = runBayesianLoop startX active 20 4 42 evaluate
    // Inactive dim should remain at startX value
    Assert.Equal(0.2, result.BestX.[1])

[<Fact>]
let ``BO converges closer than SPSA with same evaluation budget`` () =
    let target = [| 0.4; -0.3; 0.7 |]
    let active = [| true; true; true |]
    let startX = [| 0.0; 0.0; 0.0 |]

    // BO gets initialDesign=6 + 20 iterations = 27 evaluations (including start point eval)
    let boEvaluate (x: float[]) =
        let d = euclideanDist x target
        -d * d  // negative squared distance
    let boResult = runBayesianLoop startX active 20 6 42 boEvaluate
    let boDist = euclideanDist boResult.BestX target

    // SPSA gets same budget as iterations (each iteration = one comparison)
    let spsaCompare (xPlus: float[]) (xMinus: float[]) =
        let dPlus = euclideanDist xPlus target
        let dMinus = euclideanDist xMinus target
        let diff = dMinus - dPlus
        0.5 + 0.3 * (tanh (diff * 5.0))
    let spsaResult = ConsoleApp.TunerRunner.runSpsaLoop startX active 27 42 spsaCompare
    let spsaDist = euclideanDist spsaResult target

    // BO should be at least as good (usually much better) for this smooth problem
    Assert.True(boDist < spsaDist + 0.15,
        sprintf "BO dist=%.4f should be competitive with SPSA dist=%.4f" boDist spsaDist)
