/// Lc0's contempt, as a page can offer it: what the engine assumes about the game - the Elo gap
/// to the opponent, whose side it looks at, how sharp its own play is calibrated - and the UCI
/// options that express it. Nothing here talks to an engine; the page and the tests share one
/// translation, and "Sent to Lc0" on the page is exactly `toUciLines`.
///
/// Lc0's own descriptions (lc0 --show-hidden --help, v0.31-v0.33):
/// - ContemptMode: 'play' for matches, 'white_side_analysis' / 'black_side_analysis' for
///   analysis, 'disable' to deactivate.
/// - Contempt: the simulated Elo advantage for the WDL conversion.
/// - WDLCalibrationElo: Elo of the active side; 0 keeps the raw WDL, no sharpening/softening.
/// - WDLContemptAttenuation: 1.0 for realistic analysis, 0.5-0.6 for optimal match performance.
/// - WDLEvalObjectivity: 0.0 reports the WDL Lc0 plays by, 1.0 attempts an objective eval.
/// - ContemptMaxValue: contempt above this is capped. WDLMaxS: raise for DFRC or piece odds.
/// - WDLDrawRateTarget: ignored while WDLCalibrationElo is set. WDLDrawRateReference: the net's
///   draw rate at default settings.
module ChessLibrary.Lc0Contempt

open System
open System.Collections.Generic
open System.Globalization
open ChessLibrary.EngineTypes

type ContemptMode =
    | Play
    | WhiteSideAnalysis
    | BlackSideAnalysis
    | Disable

    member this.UciValue =
        match this with
        | Play -> "play"
        | WhiteSideAnalysis -> "white_side_analysis"
        | BlackSideAnalysis -> "black_side_analysis"
        | Disable -> "disable"

/// One field per Lc0 option. `Contempt` is Lc0's assumed Elo advantage over the opponent:
/// positive, it plays for the win; negative, it plays to hold.
type ContemptSettings =
    { Mode: ContemptMode
      Contempt: int
      CalibrationElo: int
      Attenuation: float
      Objectivity: float
      MaxValue: int
      MaxS: float
      DrawRateTarget: float
      DrawRateReference: float }

    /// What Lc0 starts with when nothing is set.
    static member Lc0Defaults =
        { Mode = Play
          Contempt = 0
          CalibrationElo = 0
          Attenuation = 1.0
          Objectivity = 1.0
          MaxValue = 420
          MaxS = 1.4
          DrawRateTarget = 0.0
          DrawRateReference = 0.5 }

    /// Lc0's defaults with the one value its authors say to change for today's nets: a draw
    /// rate reference of 0.58 ("for recent strong networks", the v0.30 contempt post). Every
    /// preset starts from this.
    static member Recommended = { ContemptSettings.Lc0Defaults with DrawRateReference = 0.58 }

/// The option names as Lc0 spells them, in the order they are sent.
let optionNames =
    [ "ContemptMode"; "Contempt"; "WDLCalibrationElo"; "WDLContemptAttenuation"; "WDLEvalObjectivity"
      "ContemptMaxValue"; "WDLMaxS"; "WDLDrawRateTarget"; "WDLDrawRateReference" ]

/// Not listed by `uci` unless Lc0 runs with --show-hidden, but accepted by setoption all the
/// same (verified on v0.31.2, v0.32.0 and a v0.33 master build; an unknown name makes Lc0 print
/// "error Unknown option: <name>", so a typo is not silent).
let hiddenOptionNames =
    set [ "ContemptMaxValue"; "WDLContemptAttenuation"; "WDLMaxS"; "WDLDrawRateTarget" ]

// Lc0 parses these as floats; the decimal point must not follow the user's culture.
let private num (v: float) = v.ToString("0.0##", CultureInfo.InvariantCulture)

let toOptions (s: ContemptSettings) : EngineOption list =
    [ EngineOption.Create "ContemptMode" s.Mode.UciValue
      EngineOption.Create "Contempt" (string s.Contempt)
      EngineOption.Create "WDLCalibrationElo" (string s.CalibrationElo)
      EngineOption.Create "WDLContemptAttenuation" (num s.Attenuation)
      EngineOption.Create "WDLEvalObjectivity" (num s.Objectivity)
      EngineOption.Create "ContemptMaxValue" (string s.MaxValue)
      EngineOption.Create "WDLMaxS" (num s.MaxS)
      EngineOption.Create "WDLDrawRateTarget" (num s.DrawRateTarget)
      EngineOption.Create "WDLDrawRateReference" (num s.DrawRateReference) ]

/// The lines a user could paste into any UCI console to get the same Lc0.
let toUciLines (s: ContemptSettings) =
    toOptions s |> List.map (fun o -> sprintf "setoption name %s value %s" o.Name o.Value)

/// Lc0's contempt is its own advantage: the strength it plays at minus the opponent it
/// prepares for.
let contemptBetween (playsAt: int) (opponent: int) = playsAt - opponent

/// What a preset makes of the two ratings on the panel: the ratings it wants shown (a preset
/// may rearrange them, never invent a contempt the ratings do not explain) and the settings.
type PresetApplied =
    { PlaysAt: int
      Opponent: int
      Settings: ContemptSettings }

/// A named starting point. `Apply` takes the strength Lc0 plays at and the opponent's rating.
type Preset =
    { Id: string
      Name: string
      Description: string
      Apply: int -> int -> PresetApplied }

let private defaults = ContemptSettings.Recommended

/// The ratings a user knows are blitz/rapid ratings. Lc0's rule of thumb for the calibration
/// Elo: add 50 per doubling of thinking time, rapid being the reference - here 10 minutes,
/// and the same rule run backwards for faster games. Rounded to tens; 0 when there is no
/// clock (nodes, infinite). 3+2 -> -90, 5+3 -> -50, 15+10 -> +30, 30+0 -> +80, 90+30 -> +160.
let timeControlAdjustment (baseMinutes: float option) : int =
    match baseMinutes with
    | Some m when m > 0.0 -> int (Math.Round(50.0 * Math.Log2(m / 10.0) / 10.0)) * 10
    | _ -> 0

/// Named time controls for a page that has no game clock to read.
let timeControlChoices : (string * float) list =
    [ "Bullet 1+0", 1.0
      "Blitz 3+2", 3.0
      "Blitz 5+3", 5.0
      "Rapid 10+0", 10.0
      "Rapid 15+10", 15.0
      "Classical 30+0", 30.0
      "Classical 90+30", 90.0 ]

/// Contempt is always the gap between the ratings on screen; a preset that wants a particular
/// contempt moves a rating, so the panel never shows a number the ratings do not explain.
let private fromRatings (playsAt: int) (opponent: int) (settings: ContemptSettings) =
    { PlaysAt = playsAt
      Opponent = opponent
      Settings = { settings with Contempt = contemptBetween playsAt opponent; CalibrationElo = playsAt } }

/// For playing against Lc0.
let presets : Preset list =
    [ { Id = "objective"
        Name = "Objective"
        Description = "Contempt off: Lc0 plays and evaluates as if nobody in particular were at the board."
        Apply = fun playsAt opponent -> fromRatings playsAt opponent { defaults with Mode = Disable } }
      { Id = "sparring"
        Name = "Club sparring"
        Description =
            "Lc0 plays like a player of the chosen strength who knows your rating: takes risks where you are weaker and avoids the drawish lines. Attenuation 0.6 is Lc0's own recommendation for match play."
        Apply = fun playsAt opponent -> fromRatings playsAt opponent { defaults with Mode = Play; Attenuation = 0.6 } }
      { Id = "win"
        Name = "Play for the win"
        Description =
            "Lc0 prepares for an opponent as far below its own strength as the cap allows, whoever is at the board: it assumes it is far stronger and keeps the game alive - the opponent that never takes a draw."
        Apply = fun playsAt _ ->
            fromRatings playsAt (playsAt - defaults.MaxValue) { defaults with Mode = Play; Attenuation = 0.6 } }
      { Id = "hold"
        Name = "Hold vs stronger"
        Description =
            "Lc0 is the weaker player and steers for solid, drawish play - practise converting an edge against an opponent that only wants to hold. If Lc0's rating is the higher one, the two ratings are swapped."
        Apply = fun playsAt opponent ->
            let lc0, you =
                if playsAt > opponent then opponent, playsAt
                elif playsAt = opponent then opponent - 100, opponent
                else playsAt, opponent
            fromRatings lc0 you { defaults with Mode = Play; Attenuation = 0.6 } } ]

let tryPreset (id: string) = presets |> List.tryFind (fun p -> p.Id = id)

/// `p.Apply playsAt opponent` for callers that cannot invoke a curried F# function (C#).
let applyPreset (p: Preset) (playsAt: int) (opponent: int) = p.Apply playsAt opponent

/// Analysis from one side's point of view: that side plays at `playsAt` against an opponent of
/// `opponent`. Objectivity 0 so the evals shown are the ones that side plays by; the page pairs
/// them with an objective search.
let forAnalysis (whiteSide: bool) (playsAt: int) (opponent: int) : ContemptSettings =
    { defaults with
        Mode = (if whiteSide then WhiteSideAnalysis else BlackSideAnalysis)
        Contempt = contemptBetween playsAt opponent
        CalibrationElo = playsAt
        Attenuation = 1.0
        Objectivity = 0.0 }

/// For analysis from one side's view; the side itself is chosen separately, so these carry
/// WhiteSideAnalysis as a placeholder. Both are Lc0's own suggested setups (v0.30 post).
let analysisPresets : Preset list =
    [ { Id = "kibitz"
        Name = "Kibitz"
        Description =
            "What this side sees: evals and lines as the player of that strength experiences them against this opponent. Lc0's setup for following a game."
        Apply = fun playsAt opponent -> fromRatings playsAt opponent { defaults with Mode = WhiteSideAnalysis; Attenuation = 1.0; Objectivity = 0.0 } }
      { Id = "prep"
        Name = "Opening prep"
        Description =
            "Lines chosen for this side against this opponent, evals kept objective so you can still judge them. Lc0's setup for opening preparation: objectivity 1.0, attenuation 0.5."
        Apply = fun playsAt opponent -> fromRatings playsAt opponent { defaults with Mode = WhiteSideAnalysis; Attenuation = 0.5; Objectivity = 1.0 } } ]

let tryAnalysisPreset (id: string) = analysisPresets |> List.tryFind (fun p -> p.Id = id)

/// Contempt off, everything else as the presets have it - the objective half of a comparison
/// must share the draw-rate reference with the side view, or the two differ in more than contempt.
let objective = { defaults with Mode = Disable }

/// What an engine's option list (its `uci` reply) says about contempt.
type ContemptSupport =
    { /// The engine answers to ContemptMode at all - only Lc0 does.
      HasContempt: bool
      /// The contempt options it lists.
      Visible: string list
      /// Options it should list and does not - an older Lc0. The hidden four never count as missing.
      Missing: string list }

let supportOf (engineOptionNames: seq<string>) : ContemptSupport =
    let names = HashSet<string>(engineOptionNames, StringComparer.OrdinalIgnoreCase)
    { HasContempt = names.Contains "ContemptMode"
      Visible = optionNames |> List.filter names.Contains
      Missing = optionNames |> List.filter (fun n -> not (names.Contains n) && not (hiddenOptionNames.Contains n)) }
