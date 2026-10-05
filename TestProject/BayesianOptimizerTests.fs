module BayesianOptimizerTests

open Xunit
open ConsoleApp.BayesianOptimizer

// ── GP Kernel Tests ──

[<Fact>]
let ``Matern52-ARD kernel returns signal variance for identical points`` () =
    let hyp =
        { SignalVariance = 2.0
          LengthScales = [| 0.5; 0.5; 0.5 |]
          NoiseVariance = 0.01 }
    let x = [| 0.3; -0.1; 0.7 |]
    let k = matern52ArdKernel hyp x x
    Assert.InRange(k, 1.99, 2.01)

[<Fact>]
let ``Matern52-ARD kernel returns near zero for distant points`` () =
    let hyp =
        { SignalVariance = 1.0
          LengthScales = [| 0.1; 0.1 |]
          NoiseVariance = 0.01 }
    let x1 = [| -1.0; -1.0 |]
    let x2 = [| 1.0; 1.0 |]
    let k = matern52ArdKernel hyp x1 x2
    Assert.True(k < 1e-6, sprintf "Expected near-zero kernel for distant points, got %g" k)

[<Fact>]
let ``Matern52-ARD kernel is symmetric`` () =
    let hyp =
        { SignalVariance = 1.5
          LengthScales = [| 0.3; 0.7 |]
          NoiseVariance = 0.01 }
    let x1 = [| 0.2; -0.5 |]
    let x2 = [| -0.3; 0.8 |]
    let k12 = matern52ArdKernel hyp x1 x2
    let k21 = matern52ArdKernel hyp x2 x1
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
    let gp = fitGP hyp xs ys 0.0 1.0 None

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
    let gp = fitGP hyp xs ys 0.0 1.0 None

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
    let gp = fitGP hyp xs ys 0.0 1.0 None
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
    let gp = fitGP hyp xs ys 0.0 1.0 None
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
    let lml = logMarginalLikelihood xs ys hyp None
    Assert.True(System.Double.IsFinite lml, sprintf "LML should be finite, got %g" lml)

[<Fact>]
let ``optimizeHyperparameters returns valid hyperparameters`` () =
    let rng = System.Random(42)
    let xs = Array.init 15 (fun _ -> [| rng.NextDouble() * 2.0 - 1.0; rng.NextDouble() * 2.0 - 1.0 |])
    let ys = xs |> Array.map (fun x -> -x.[0] * x.[0] - x.[1] * x.[1] + 0.1 * rng.NextDouble())
    let hyp = optimizeHyperparameters xs ys 2 None
    Assert.True(hyp.SignalVariance > 0.0)
    Assert.True(hyp.NoiseVariance > 0.0)
    Assert.Equal(2, hyp.LengthScales.Length)
    for l in hyp.LengthScales do
        Assert.True(l > 0.0)

// ── Centered Latin Hypercube Sampling Tests ──

[<Fact>]
let ``Centered LHS respects radius bounds`` () =
    let rng = System.Random(42)
    let active = [| true; true |]
    let center = [| 0.5; -0.3 |]
    let radius = 0.3
    let samples = centeredLatinHypercubeSample 20 2 active center radius rng
    for sample in samples do
        Assert.InRange(sample.[0], max -1.0 (0.5 - 0.3), min 1.0 (0.5 + 0.3))
        Assert.InRange(sample.[1], max -1.0 (-0.3 - 0.3), min 1.0 (-0.3 + 0.3))

[<Fact>]
let ``Centered LHS preserves stratification`` () =
    let rng = System.Random(42)
    let active = [| true |]
    let center = [| 0.0 |]
    let radius = 0.5
    let n = 10
    let samples = centeredLatinHypercubeSample n 1 active center radius rng
    // Map back to [0, 1] relative to center-radius..center+radius
    let bins =
        samples
        |> Array.map (fun s ->
            let u = (s.[0] - (center.[0] - radius)) / (2.0 * radius)
            int (u * float n) |> max 0 |> min (n - 1))
        |> Array.sort
    let distinctBins = bins |> Array.distinct |> Array.length
    Assert.Equal(n, distinctBins)

[<Fact>]
let ``Centered LHS clamps to valid range at boundary`` () =
    let rng = System.Random(42)
    let active = [| true |]
    let center = [| 0.9 |]
    let radius = 0.5
    let samples = centeredLatinHypercubeSample 20 1 active center radius rng
    for sample in samples do
        Assert.InRange(sample.[0], -1.0, 1.0)
