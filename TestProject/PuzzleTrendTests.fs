module PuzzleTrendTests

open Xunit
open ChessLibrary

/// The net-name parser is the load-bearing part of `puzzletrend`: everything downstream
/// groups on the (arm, step) it produces. These cases are all real names taken from
/// C:/Dev/Chess/Networks/CeresNet/lepned/ and its Server/ subfolder.

[<Fact>]
let ``baseline arm parses to arm and step`` () =
    match PuzzleTrend.parseNetName "a4000-21bn11_srv_256_10_smol_t91_1B_700012544" with
    | Some id ->
        Assert.Equal("a4000-21bn11_srv_256_10_smol_t91_1B", id.Arm)
        Assert.Equal(700, id.StepM)
        Assert.False(id.IsEma)
    | None -> failwith "expected a checkpoint"

[<Fact>]
let ``branch arm keeps its suffix in the arm name`` () =
    // r600 forked off the baseline at 600M; it must NOT collapse into the baseline arm
    match PuzzleTrend.parseNetName "a4000-21bn11_srv_256_10_smol_t91_1B_r600_700014592" with
    | Some id ->
        Assert.Equal("a4000-21bn11_srv_256_10_smol_t91_1B_r600", id.Arm)
        Assert.Equal(700, id.StepM)
    | None -> failwith "expected a checkpoint"

[<Fact>]
let ``ema is a variant of the same step, not a separate step`` () =
    let raw = PuzzleTrend.parseNetName "a4000-21bn11_srv_512_16_t91_1B_700026880"
    let ema = PuzzleTrend.parseNetName "a4000-21bn11_srv_512_16_t91_1B_700026880ema"
    match raw, ema with
    | Some r, Some e ->
        Assert.Equal(r.Arm, e.Arm)
        Assert.Equal(r.StepM, e.StepM)
        Assert.False(r.IsEma)
        Assert.True(e.IsEma)
    | _ -> failwith "expected both to parse"

[<Fact>]
let ``arms with different batch sizes land on the same milestone`` () =
    // 256x10 counts 100001792 where 512x16 counts 100003840 - both are the 100M milestone,
    // and the step curves are only comparable if they bucket together
    let small = PuzzleTrend.parseNetName "a4000-21bn11_srv_256_10_smol_t91_1B_100001792"
    let big = PuzzleTrend.parseNetName "a4000-21bn11_srv_512_16_t91_1B_100003840"
    match small, big with
    | Some s, Some b ->
        Assert.Equal(100, s.StepM)
        Assert.Equal(100, b.StepM)
        Assert.NotEqual<string>(s.Arm, b.Arm)
    | _ -> failwith "expected both to parse"

[<Fact>]
let ``net name ending in a digit is not a checkpoint`` () =
    // "I8" ends in 8; without a magnitude guard this would become a bogus 0M checkpoint
    Assert.True((PuzzleTrend.parseNetName "C1-640-34-I8").IsNone)
    Assert.True((PuzzleTrend.parseNetName "C3-768-30-pre2-I8").IsNone)

[<Fact>]
let ``blank and extension-only names are rejected`` () =
    Assert.True((PuzzleTrend.parseNetName "").IsNone)
    Assert.True((PuzzleTrend.parseNetName "   ").IsNone)

[<Fact>]
let ``full paths and onnx extensions are stripped`` () =
    match PuzzleTrend.parseNetName "C:/Dev/Chess/Networks/CeresNet/lepned/Server/a4000-21bn11_srv_512_16_t91_1B_600023040ema.onnx" with
    | Some id ->
        Assert.Equal("a4000-21bn11_srv_512_16_t91_1B", id.Arm)
        Assert.Equal(600, id.StepM)
        Assert.True(id.IsEma)
    | None -> failwith "expected a checkpoint"

[<Fact>]
let ``step label embedded in the arm name is not mistaken for the counter`` () =
    // the trailing counter wins over the "300M" label earlier in the name
    match PuzzleTrend.parseNetName "lepned_256_10_cfT80_300M_300000256ema" with
    | Some id ->
        Assert.Equal("lepned_256_10_cfT80_300M", id.Arm)
        Assert.Equal(300, id.StepM)
        Assert.True(id.IsEma)
    | None -> failwith "expected a checkpoint"

[<Fact>]
let ``rating group snaps the sampled average back to the configured group`` () =
    Assert.Equal(2500, PuzzleTrend.ratingGroupOf 2499.0)
    Assert.Equal(2300, PuzzleTrend.ratingGroupOf 2300.0)
    Assert.Equal(2700, PuzzleTrend.ratingGroupOf 2693.0)
    Assert.Equal(2700, PuzzleTrend.ratingGroupOf 2699.0)

let private cell started total acc : PuzzleTrend.Point =
    { Arm = "arm"; StepM = 700; IsEma = false; Type = "Value"; RatingGroup = 2500
      Accuracy = acc; Kld = 0.0; Perf = 0.0; Total = total
      StartedUtc = started; SourceFile = started + ".json" }

[<Fact>]
let ``dedupe keeps the most recent reading at equal sample size`` () =
    let points = [ cell "2026-08-20T09:00:00Z" 2000 0.40; cell "2026-08-20T12:00:00Z" 2000 0.45 ]
    let deduped = PuzzleTrend.dedupe points
    Assert.Single(deduped) |> ignore
    Assert.Equal(0.45, (List.head deduped).Accuracy, 3)

[<Fact>]
let ``filterMinSteps drops short series and reports how many`` () =
    let at arm step : PuzzleTrend.Point =
        { Arm = arm; StepM = step; IsEma = false; Type = "Value"; RatingGroup = 2300
          Accuracy = 0.5; Kld = 0.0; Perf = 0.0; Total = 2000
          StartedUtc = "2026-08-20T09:00:00Z"; SourceFile = "s.json" }
    let points =
        [ at "curve" 100; at "curve" 200; at "curve" 300   // a real run
          at "oneoff" 500                                   // a single measured net
          at "pair" 100; at "pair" 200 ]                    // two points, still not a curve
    let kept, dropped = PuzzleTrend.filterMinSteps 3 points
    Assert.Equal(2, dropped)
    Assert.Equal(3, kept.Length)
    Assert.All(kept, fun p -> Assert.Equal<string>("curve", p.Arm))

[<Fact>]
let ``filterMinSteps of one keeps everything`` () =
    let p : PuzzleTrend.Point =
        { Arm = "a"; StepM = 100; IsEma = false; Type = "Value"; RatingGroup = 2300
          Accuracy = 0.5; Kld = 0.0; Perf = 0.0; Total = 2000
          StartedUtc = ""; SourceFile = "" }
    let kept, dropped = PuzzleTrend.filterMinSteps 1 [ p ]
    Assert.Equal(0, dropped)
    Assert.Single(kept) |> ignore

[<Fact>]
let ``raw and ema of one arm are separate series for the step count`` () =
    let mk ema step : PuzzleTrend.Point =
        { Arm = "arm"; StepM = step; IsEma = ema; Type = "Value"; RatingGroup = 2300
          Accuracy = 0.5; Kld = 0.0; Perf = 0.0; Total = 2000
          StartedUtc = ""; SourceFile = "" }
    // raw has 3 steps and survives; ema has only 2 and does not
    let points = [ mk false 100; mk false 200; mk false 300; mk true 200; mk true 300 ]
    let kept, dropped = PuzzleTrend.filterMinSteps 3 points
    Assert.Equal(1, dropped)
    Assert.Equal(3, kept.Length)
    Assert.All(kept, fun p -> Assert.False p.IsEma)

[<Fact>]
let ``a small later run does not displace a large earlier one`` () =
    // a 50-puzzle smoke test must not overwrite a 2000-puzzle measurement of the same checkpoint
    let points = [ cell "2026-08-20T09:00:00Z" 2000 0.426; cell "2026-08-20T13:48:00Z" 50 0.380 ]
    let deduped = PuzzleTrend.dedupe points
    Assert.Single(deduped) |> ignore
    Assert.Equal(0.426, (List.head deduped).Accuracy, 3)
    Assert.Equal(2000, (List.head deduped).Total)
