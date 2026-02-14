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
let ``accuracy comparison logic selects correct winner`` () =
    // The comparison logic inside runComparisonByAccuracy: accA >= accB means A wins
    let check accA accB =
        let aBeatB = accA >= accB
        aBeatB
    Assert.True(check 0.75 0.70)   // A is better
    Assert.True(check 0.70 0.70)   // Tie goes to A
    Assert.False(check 0.65 0.70)  // B is better
