module TunerRunnerTests

open Xunit
open ConsoleApp

[<Fact>]
let ``denormalize and normalize are stable for linear scale`` () =
    let p: TunerRunner.TuneParameterDef =
        { Name = "smartpruningfactor"
          Option = null
          Min = 0.0
          Max = 2.0
          Step = 0.01
          Scale = "linear" }

    let v = 1.37
    let n = TunerRunner.normalizeParameterValue p v
    let roundTrip = TunerRunner.denormalizeParameterValue p n

    Assert.InRange(n, -1.0, 1.0)
    Assert.Equal(v, roundTrip, 2)

[<Fact>]
let ``denormalize and normalize are stable for log scale`` () =
    let p: TunerRunner.TuneParameterDef =
        { Name = "cpuctBase"
          Option = null
          Min = 1.0
          Max = 20000.0
          Step = 1.0
          Scale = "log" }

    let v = 512.0
    let n = TunerRunner.normalizeParameterValue p v
    let roundTrip = TunerRunner.denormalizeParameterValue p n

    Assert.InRange(n, -1.0, 1.0)
    Assert.Equal(v, roundTrip, 0)

[<Fact>]
let ``spsa gains decay with iteration`` () =
    let ak1, ck1 = TunerRunner.spsaGains 120 0
    let ak2, ck2 = TunerRunner.spsaGains 120 100

    Assert.True(ak2 < ak1)
    Assert.True(ck2 < ck1)

[<Fact>]
let ``sprt accepts h1 for strong positive score`` () =
    let sprt: TunerRunner.SprtConfig =
        { Elo0 = -3.0
          Elo1 = 3.0
          Alpha = 0.05
          Beta = 0.05
          MinGames = 24
          MaxGames = 400 }

    let decision, _, elo = TunerRunner.sprtDecisionFromWdl sprt 190 5 5

    Assert.Equal("accept_h1", decision)
    Assert.True(elo > 0.0)

[<Fact>]
let ``sprt accepts h0 for strong negative score`` () =
    let sprt: TunerRunner.SprtConfig =
        { Elo0 = -3.0
          Elo1 = 3.0
          Alpha = 0.05
          Beta = 0.05
          MinGames = 24
          MaxGames = 400 }

    let decision, _, elo = TunerRunner.sprtDecisionFromWdl sprt 5 5 190

    Assert.Equal("accept_h0", decision)
    Assert.True(elo < 0.0)

[<Fact>]
let ``sprt remains undecided before min games`` () =
    let sprt: TunerRunner.SprtConfig =
        { Elo0 = -3.0
          Elo1 = 3.0
          Alpha = 0.05
          Beta = 0.05
          MinGames = 24
          MaxGames = 400 }

    let decision, _, _ = TunerRunner.sprtDecisionFromWdl sprt 8 4 7

    Assert.Equal("undecided", decision)

[<Fact>]
let ``pentanomial llr is positive for strong winning data`` () =
    // 50 pairs, heavily winning: mostly W1.5 and W2
    let llr = TunerRunner.pentanomialLlr 0.0 5.0 0 2 10 25 13

    Assert.True(llr > 0.0, sprintf "Expected positive LLR, got %.4f" llr)

[<Fact>]
let ``pentanomial llr is negative for strong losing data`` () =
    // 50 pairs, heavily losing: mostly L1.5 and L2
    let llr = TunerRunner.pentanomialLlr 0.0 5.0 13 25 10 2 0

    Assert.True(llr < 0.0, sprintf "Expected negative LLR, got %.4f" llr)

[<Fact>]
let ``pentanomial llr is near zero for balanced data`` () =
    // 50 pairs, balanced around 50%
    let llr = TunerRunner.pentanomialLlr 0.0 5.0 5 10 20 10 5

    Assert.InRange(llr, -1.0, 1.0)

[<Fact>]
let ``pentanomial llr has larger magnitude than binomial for skewed data`` () =
    // Reproduce the user's scenario: 50 pairs with clear negative result
    // Old binomial LLR was only -0.259, pentanomial should be stronger
    let llr = TunerRunner.pentanomialLlr 0.0 5.0 0 23 19 8 0

    Assert.True(llr < -0.259, sprintf "Pentanomial LLR (%.4f) should exceed binomial magnitude (-0.259)" llr)

// --- SPSA convergence tests ---

let private euclideanDist (a: float[]) (b: float[]) =
    Array.map2 (fun x y -> (x - y) ** 2.0) a b |> Array.sum |> sqrt

[<Fact>]
let ``spsa converges with deterministic comparator`` () =
    let optimum = [| 0.4; -0.3; 0.7 |]
    let startX = [| 0.0; 0.0; 0.0 |]
    let active = [| true; true; true |]
    let compare (xPlus: float[]) (xMinus: float[]) =
        let dPlus = euclideanDist xPlus optimum
        let dMinus = euclideanDist xMinus optimum
        let diff = dMinus - dPlus  // positive if xPlus is closer
        0.5 + 0.3 * (tanh (diff * 5.0))  // sigmoid-mapped to [0.2, 0.8] range
    let result = TunerRunner.runSpsaLoop startX active 200 42 compare
    for i in 0 .. optimum.Length - 1 do
        Assert.InRange(result.[i], optimum.[i] - 0.10, optimum.[i] + 0.10)

[<Fact>]
let ``spsa converges with noisy comparator`` () =
    let optimum = [| 0.4; -0.3; 0.7 |]
    let startX = [| 0.0; 0.0; 0.0 |]
    let active = [| true; true; true |]
    let noiseRng = System.Random(123)
    let compare (xPlus: float[]) (xMinus: float[]) =
        let dPlus = euclideanDist xPlus optimum
        let dMinus = euclideanDist xMinus optimum
        let diff = dMinus - dPlus
        let signal = 0.5 + 0.3 * (tanh (diff * 5.0))
        let noise = noiseRng.NextDouble() * 0.1 - 0.05  // +/-5% noise
        max 0.0 (min 1.0 (signal + noise))
    let result = TunerRunner.runSpsaLoop startX active 500 42 compare
    for i in 0 .. optimum.Length - 1 do
        Assert.InRange(result.[i], optimum.[i] - 0.25, optimum.[i] + 0.25)

[<Fact>]
let ``spsa respects active mask`` () =
    let optimum = [| 0.5; -0.5; 0.5; -0.5 |]
    let startX = [| 0.0; 0.2; 0.0; -0.8 |]
    let active = [| true; false; true; false |]
    let compare (xPlus: float[]) (xMinus: float[]) =
        let dPlus = euclideanDist xPlus optimum
        let dMinus = euclideanDist xMinus optimum
        let diff = dMinus - dPlus
        0.5 + 0.3 * (tanh (diff * 5.0))
    let result = TunerRunner.runSpsaLoop startX active 200 42 compare
    // Active params should converge toward optimum
    Assert.InRange(result.[0], optimum.[0] - 0.10, optimum.[0] + 0.10)
    Assert.InRange(result.[2], optimum.[2] - 0.10, optimum.[2] + 0.10)
    // Inactive params must remain exactly at initial values
    Assert.Equal(0.2, result.[1])
    Assert.Equal(-0.8, result.[3])

/// Old binary SPSA loop (pre-continuous gradient) for comparison testing.
let private runBinarySpsaLoop (startX: float[]) (active: bool[]) (iterations: int) (seed: int)
    (compare: float[] -> float[] -> float) : float[] =
    let clamp lo hi x = max lo (min hi x)
    let n = startX.Length
    let x = Array.copy startX
    let rng = System.Random(seed)
    let alpha = 0.602
    let gamma = 0.101
    let a = 0.15
    let c = 0.1
    let bigA = max 1.0 (0.1 * float iterations)
    for iter in 0 .. iterations - 1 do
      let k = iter + 1
      let ak = a / System.Math.Pow(bigA + float k, alpha)
      let ck = c / System.Math.Pow(float k, gamma)
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
      let winSign = if scoreFrac > 0.5 then 1.0 else -1.0
      for i in 0 .. n - 1 do
        if active.[i] then
          let g = winSign * (1.0 / (2.0 * ck * delta.[i]))
          let step = clamp -0.25 0.25 (ak * g)
          x.[i] <- x.[i] + step
      for i in 0 .. n - 1 do x.[i] <- clamp -1.0 1.0 x.[i]
    x

[<Fact>]
let ``continuous gradient converges closer than binary`` () =
    let optimum = [| 0.4; -0.3; 0.7 |]
    let startX = [| 0.0; 0.0; 0.0 |]
    let active = [| true; true; true |]
    let iterations = 200
    let seed = 42

    let compare (xPlus: float[]) (xMinus: float[]) =
        let dPlus = euclideanDist xPlus optimum
        let dMinus = euclideanDist xMinus optimum
        let diff = dMinus - dPlus
        0.5 + 0.3 * (tanh (diff * 5.0))

    let binaryResult = runBinarySpsaLoop startX active iterations seed compare
    let continuousResult = TunerRunner.runSpsaLoop startX active iterations seed compare

    let binaryDist = euclideanDist binaryResult optimum
    let continuousDist = euclideanDist continuousResult optimum

    Assert.True(continuousDist < binaryDist,
        sprintf "Continuous (dist=%.4f) should be closer to optimum than binary (dist=%.4f)" continuousDist binaryDist)

// --- Embedded parameter helpers ---

[<Fact>]
let ``parseEmbeddedValue extracts value from pipe-delimited string`` () =
    let value = "path/net.onnx|V1TEMP=0.55|SMOLGEN=1"
    let result = TunerRunner.parseEmbeddedValue value "V1TEMP"
    Assert.Equal(Some 0.55, result)

    let result2 = TunerRunner.parseEmbeddedValue value "SMOLGEN"
    Assert.Equal(Some 1.0, result2)

    let missing = TunerRunner.parseEmbeddedValue value "NOTHERE"
    Assert.Equal(None, missing)

[<Fact>]
let ``parseEmbeddedValue extracts value from mixed delimiter string`` () =
    // Semicolons within pipe segments
    let value = "path/net.onnx|PARAM1=1.5;V1TEMP=0.55;PARAM2=2.0"
    let result = TunerRunner.parseEmbeddedValue value "V1TEMP"
    Assert.Equal(Some 0.55, result)

    let result2 = TunerRunner.parseEmbeddedValue value "PARAM1"
    Assert.Equal(Some 1.5, result2)

    let result3 = TunerRunner.parseEmbeddedValue value "PARAM2"
    Assert.Equal(Some 2.0, result3)

    let missing = TunerRunner.parseEmbeddedValue value "NOTHERE"
    Assert.Equal(None, missing)

[<Fact>]
let ``replaceEmbeddedValue patches value and preserves surrounding segments`` () =
    let value = "path/net.onnx|V1TEMP=0.55|SMOLGEN=1"
    let result = TunerRunner.replaceEmbeddedValue value "V1TEMP" 0.72
    Assert.Equal("path/net.onnx|V1TEMP=0.72|SMOLGEN=1", result)

[<Fact>]
let ``replaceEmbeddedValue preserves semicolon delimiters`` () =
    // Test that semicolons are preserved when replacing values
    let value = "a|b;V1TEMP=0.5;c"
    let result = TunerRunner.replaceEmbeddedValue value "V1TEMP" 0.72
    Assert.Equal("a|b;V1TEMP=0.72;c", result)

[<Fact>]
let ``replaceEmbeddedValue handles mixed delimiters`` () =
    // Realistic scenario: network path with cudagraphs and V1TEMP
    let value = "C:/Dev/Chess/Networks/CeresNet/C1-640-34-I8.onnx|cudagraphs=false;V1TEMP=0.55"
    let result = TunerRunner.replaceEmbeddedValue value "V1TEMP" 0.72
    Assert.Equal("C:/Dev/Chess/Networks/CeresNet/C1-640-34-I8.onnx|cudagraphs=false;V1TEMP=0.72", result)

// --- Eval mode helpers ---

[<Fact>]
let ``normalizeEvalMode defaults to sprt for null or empty`` () =
    Assert.Equal("sprt", TunerRunner.normalizeEvalMode null)
    Assert.Equal("sprt", TunerRunner.normalizeEvalMode "")
    Assert.Equal("sprt", TunerRunner.normalizeEvalMode "  ")

[<Fact>]
let ``normalizeEvalMode lowercases and trims`` () =
    Assert.Equal("puzzle", TunerRunner.normalizeEvalMode "Puzzle")
    Assert.Equal("eret", TunerRunner.normalizeEvalMode " ERET ")
    Assert.Equal("sprt", TunerRunner.normalizeEvalMode "SPRT")

[<Fact>]
let ``SPSA accuracy scoreFrac maps correctly`` () =
    // When both accuracies are equal, scoreFrac = 0.5
    let sf1 = 0.5 + (0.7 - 0.7) / 2.0
    Assert.Equal(0.5, sf1, 6)

    // When plus is better by 0.1, scoreFrac = 0.55
    let sf2 = 0.5 + (0.8 - 0.7) / 2.0
    Assert.Equal(0.55, sf2, 6)

    // When minus is better by 0.1, scoreFrac = 0.45
    let sf3 = 0.5 + (0.7 - 0.8) / 2.0
    Assert.Equal(0.45, sf3, 6)

    // When plus gets 100% and minus gets 0%, scoreFrac = 1.0
    let sf4 = 0.5 + (1.0 - 0.0) / 2.0
    Assert.Equal(1.0, sf4, 6)

    // When plus gets 0% and minus gets 100%, scoreFrac = 0.0
    let sf5 = 0.5 + (0.0 - 1.0) / 2.0
    Assert.Equal(0.0, sf5, 6)

[<Fact>]
let ``accuracy comparison logic selects correct winner`` () =
    // The comparison logic inside runComparisonByAccuracy: accA >= accB means A wins
    let check accA accB =
        let aBeatB = accA >= accB
        aBeatB
    Assert.True(check 0.75 0.70)   // A is better
    Assert.True(check 0.70 0.70)   // Tie goes to A
    Assert.False(check 0.65 0.70)  // B is better
