module Lc0ContemptTests

open System
open System.Globalization
open System.Threading
open Xunit
open ChessLibrary.Lc0Contempt

// ---------------------------------------------------------------------------
// The translation from settings to Lc0's options is the whole contract of the page:
// the lines it shows are the lines it sends.
// ---------------------------------------------------------------------------

[<Fact>]
let ``every option is sent, in Lc0's spelling and order`` () =
    let lines = toUciLines ContemptSettings.Lc0Defaults
    Assert.Equal<string list>(
        [ "setoption name ContemptMode value play"
          "setoption name Contempt value 0"
          "setoption name WDLCalibrationElo value 0"
          "setoption name WDLContemptAttenuation value 1.0"
          "setoption name WDLEvalObjectivity value 1.0"
          "setoption name ContemptMaxValue value 420"
          "setoption name WDLMaxS value 1.4"
          "setoption name WDLDrawRateTarget value 0.0"
          "setoption name WDLDrawRateReference value 0.5" ],
        lines)
    Assert.Equal<string list>(optionNames, toOptions ContemptSettings.Lc0Defaults |> List.map (fun o -> o.Name))

[<Fact>]
let ``floats use a decimal point whatever the thread culture`` () =
    let culture = Thread.CurrentThread.CurrentCulture
    try
        Thread.CurrentThread.CurrentCulture <- CultureInfo("nb-NO")
        let s = { ContemptSettings.Lc0Defaults with Attenuation = 0.6; DrawRateReference = 0.65 }
        let value name = toOptions s |> List.find (fun o -> o.Name = name) |> fun o -> o.Value
        Assert.Equal("0.6", value "WDLContemptAttenuation")
        Assert.Equal("0.65", value "WDLDrawRateReference")
    finally
        Thread.CurrentThread.CurrentCulture <- culture

[<Fact>]
let ``sparring is Lc0's advantage over the opponent, calibrated at its own strength, attenuated for play`` () =
    let r = (tryPreset "sparring").Value.Apply 2400 2000
    Assert.Equal((2400, 2000), (r.PlaysAt, r.Opponent))
    let s = r.Settings
    Assert.Equal(Play, s.Mode)
    Assert.Equal(400, s.Contempt)
    Assert.Equal(2400, s.CalibrationElo)
    Assert.Equal(0.6, s.Attenuation)
    Assert.Equal(1.0, s.Objectivity)   // a game shows the objective eval

[<Fact>]
let ``objective disables contempt and keeps the ratings`` () =
    let r = (tryPreset "objective").Value.Apply 2400 2000
    Assert.Equal(Disable, r.Settings.Mode)
    Assert.Equal((2400, 2000), (r.PlaysAt, r.Opponent))

[<Fact>]
let ``hold swaps the ratings when Lc0's is the higher one, so the contempt shown is the gap on screen`` () =
    let swapped = (tryPreset "hold").Value.Apply 2400 2000
    Assert.Equal((2000, 2400), (swapped.PlaysAt, swapped.Opponent))
    Assert.Equal(-400, swapped.Settings.Contempt)
    Assert.Equal(2000, swapped.Settings.CalibrationElo)
    let already = (tryPreset "hold").Value.Apply 2000 2400
    Assert.Equal((2000, 2400), (already.PlaysAt, already.Opponent))
    Assert.Equal(-400, already.Settings.Contempt)
    let equal = (tryPreset "hold").Value.Apply 2200 2200
    Assert.Equal((2100, 2200), (equal.PlaysAt, equal.Opponent))
    Assert.Equal(-100, equal.Settings.Contempt)

[<Fact>]
let ``play for the win keeps Lc0's strength and puts the opponent a cap below it, so the gap on screen is the cap`` () =
    let r = (tryPreset "win").Value.Apply 2000 2400
    Assert.Equal((2000, 1580), (r.PlaysAt, r.Opponent))
    Assert.Equal(r.Settings.MaxValue, r.Settings.Contempt)
    Assert.Equal(2000, r.Settings.CalibrationElo)

[<Fact>]
let ``analysis takes the chosen side and shows what that side plays by`` () =
    let w = forAnalysis true 2800 2500
    Assert.Equal(WhiteSideAnalysis, w.Mode)
    Assert.Equal(300, w.Contempt)
    Assert.Equal(0.0, w.Objectivity)
    Assert.Equal(1.0, w.Attenuation)
    let b = forAnalysis false 2500 2800
    Assert.Equal(BlackSideAnalysis, b.Mode)
    Assert.Equal(-300, b.Contempt)

[<Fact>]
let ``support is read from the engine's option list, case-insensitively, and the hidden four never count as missing`` () =
    // What Lc0 lists without --show-hidden.
    let lc0 = [ "ScoreType"; "contemptmode"; "Contempt"; "WDLCalibrationElo"; "WDLEvalObjectivity"; "WDLDrawRateReference"; "UCI_ShowWDL" ]
    let s = supportOf lc0
    Assert.True s.HasContempt
    Assert.Empty s.Missing
    Assert.Equal(5, s.Visible.Length)
    let stockfish = [ "Hash"; "Threads"; "UCI_ShowWDL" ]
    let n = supportOf stockfish
    Assert.False n.HasContempt
    // An Lc0 too old for the WDL options is reported, not silently half-configured.
    let old = [ "ContemptMode"; "Contempt" ]
    Assert.Equal<string list>([ "WDLCalibrationElo"; "WDLEvalObjectivity"; "WDLDrawRateReference" ], (supportOf old).Missing)

[<Fact>]
let ``presets start from Lc0's recommended draw-rate reference, not its default`` () =
    Assert.Equal(0.5, ContemptSettings.Lc0Defaults.DrawRateReference)
    Assert.Equal(0.58, ContemptSettings.Recommended.DrawRateReference)
    for p in presets @ analysisPresets do
        Assert.Equal(0.58, (p.Apply 2400 2000).Settings.DrawRateReference)

[<Fact>]
let ``calibration Elo follows Lc0's 50-per-doubling rule from a 10 minute reference`` () =
    Assert.Equal(0, timeControlAdjustment (Some 10.0))
    Assert.Equal(-90, timeControlAdjustment (Some 3.0))
    Assert.Equal(-50, timeControlAdjustment (Some 5.0))
    Assert.Equal(30, timeControlAdjustment (Some 15.0))
    Assert.Equal(80, timeControlAdjustment (Some 30.0))
    Assert.Equal(160, timeControlAdjustment (Some 90.0))
    Assert.Equal(0, timeControlAdjustment None)          // nodes or infinite: no clock, no rule
    Assert.Equal(0, timeControlAdjustment (Some 0.0))

[<Fact>]
let ``analysis presets are Lc0's kibitz and opening-prep setups`` () =
    let k = (tryAnalysisPreset "kibitz").Value.Apply 2800 2500
    Assert.Equal((0.0, 1.0), (k.Settings.Objectivity, k.Settings.Attenuation))
    Assert.Equal(300, k.Settings.Contempt)
    let p = (tryAnalysisPreset "prep").Value.Apply 2800 2500
    Assert.Equal((1.0, 0.5), (p.Settings.Objectivity, p.Settings.Attenuation))
