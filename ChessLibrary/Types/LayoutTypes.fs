namespace ChessLibrary

open System
open System.Text.Json
open System.Text.Json.Serialization

module LayoutTypes =
  type Fonts =
    { StandingsFont: int
      PairingsFont: int
      LatestGamesFont: int
      CrossTableFont: int
      CupBracketFont: int
      SwissOverviewFont: int
      LadderOverviewFont: int
      MoveListFont: int
      InfoBannerFont: int
      TournamentDescFont: int
      EnginesPanelFont: int
      PVLabelFont: int }
  type Sizes =
    { LiveChartHeight: int
      MoveChartHeight: int
      PVboardSize: string
      LogoSize: string
      /// Ceiling for the main (tournament) logo in the middle of the engine panel - the image
      /// MainLogoFileName points at. Same "WxH" / "N" format as LogoSize; empty means 240x130.
      MainLogoSize: string }

  /// Parses LogoSize string. Returns Some (width, height) or None if empty/invalid.
  /// Formats: "WxH" (e.g., "150x100") or "N" for square (e.g., "150" -> 150x150)
  let parseLogoSize (logoSize: string) : (int * int) option =
    if String.IsNullOrWhiteSpace(logoSize) then None
    elif logoSize.Contains("x") then
      let parts = logoSize.Split('x')
      if parts.Length = 2 then
        match Int32.TryParse(parts.[0]), Int32.TryParse(parts.[1]) with
        | (true, w), (true, h) when w > 0 && h > 0 -> Some (w, h)
        | _ -> None
      else None
    else
      match Int32.TryParse(logoSize) with
      | true, size when size > 0 -> Some (size, size)
      | _ -> None
  type Charts =
    { ShowNPS: bool
      ShowEval: bool
      ShowNodes: bool
      ShowTime: bool
      NumberOfLines: int
      Qdiff: float }
    with
      /// What a fresh installation had been shipping in tournamentEmpty.json for a long time,
      /// moved here (2026-09-21) so the file can stop carrying it. The set that lived here
      /// before (time chart, 3 lines, Q 0.5) had never been seen by a new user.
      static member Default =
        { ShowNPS = true
          ShowEval = true
          ShowNodes = false
          ShowTime = false
          NumberOfLines = 5
          Qdiff = 1.0 }
  /// Where the crosstable goes in the left column of the tournament page. One setting with
  /// three values, replacing two flags (OnlyShowStandings, ShowCrosstableBelowStandings) that
  /// encoded the same choice and could contradict each other. "cycle", "below" and "none" in
  /// tournament.json; the old flags are still read and mapped, see Normalize.
  [<JsonConverter(typeof<CrosstableWithStandingsConverter>)>]
  type CrosstableWithStandings =
    /// Standings and crosstable take turns in the same box.
    | Cycle
    /// The crosstable gets a box of its own under the standings.
    | Below
    /// Standings only.
    | Hidden
  and CrosstableWithStandingsConverter() =
    inherit JsonConverter<CrosstableWithStandings>()
    override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: Type, _options: JsonSerializerOptions) =
      let value = reader.GetString()
      match (if isNull value then "" else value.Trim().ToLowerInvariant()) with
      | "cycle" -> Cycle
      | "below" -> Below
      | "none" | "hidden" | "off" -> Hidden
      | other -> failwith $"Unknown CrosstableWithStandings: {other} (use cycle, below or none)"
    override _.Write(writer: Utf8JsonWriter, value: CrosstableWithStandings, _options: JsonSerializerOptions) =
      writer.WriteStringValue(match value with Cycle -> "cycle" | Below -> "below" | Hidden -> "none")

  type LayoutOption =
    { Fonts: Fonts
      Sizes: Sizes
      Charts: Charts
      ShowPVBoard: bool
      UseNPM: bool
      BestMoveWithPolicy: bool
      /// Read for older files only; superseded by CrosstableWithStandings.
      OnlyShowStandings: bool
      ShowCrosstableBetweenGames: bool
      /// Read for older files only; superseded by CrosstableWithStandings.
      ShowCrosstableBelowStandings: bool
      /// Null when the file does not have it; Normalize derives it from the two old flags then.
      CrosstableWithStandings: CrosstableWithStandings
      AutoCycleTimeInSec: int }
    with
      /// What the tournament page renders when tournament.json says nothing about sizes -
      /// which is every fresh installation, since Data/tournamentEmpty.json carries no Fonts or
      /// Sizes any more, and every older config that leaves the fields out.
      ///
      /// The twelve font sizes are the ones a fresh installation had been shipping in
      /// tournamentEmpty.json for a long time, moved here unchanged (agreed 2026-09-21) so that
      /// dropping the fields from the file changed nothing about the text. An older, smaller set
      /// (12-16) lived here before and had never actually been seen by a new user. The Sizes
      /// were decided at the same time, and two of them deliberately differ from what that file
      /// shipped: charts 200 rather than 230 (the better height on an ordinary screen) and PV
      /// boards small rather than medium, shown by default. Tuned for 1080p; a bigger screen is
      /// one A+ and a "save" away, per screen, in the GUI.
      static member Default =
        { Fonts =
            { StandingsFont = 18
              PairingsFont = 16
              LatestGamesFont = 16
              CrossTableFont = 18
              CupBracketFont = 18
              SwissOverviewFont = 18
              LadderOverviewFont = 18
              MoveListFont = 16
              InfoBannerFont = 18
              TournamentDescFont = 16
              EnginesPanelFont = 18
              PVLabelFont = 17 }
          Sizes =
            { LiveChartHeight = 200
              MoveChartHeight = 200
              PVboardSize = "small"
              LogoSize = ""
              MainLogoSize = "" }
          Charts = Charts.Default
          ShowPVBoard = true
          UseNPM = false
          BestMoveWithPolicy = false
          // The two flags below are also what tournamentEmpty.json had shipped: standings only
          // on the left, and the crosstable shown between games.
          OnlyShowStandings = true
          ShowCrosstableBetweenGames = true
          ShowCrosstableBelowStandings = false
          CrosstableWithStandings = Hidden
          AutoCycleTimeInSec = 30 }

      /// The block as the page may use it: a missing block is Default, and a block that leaves
      /// out Fonts, Sizes or Charts gets those parts from Default. Every one of them is optional
      /// in tournament.json now, and System.Text.Json hands a left-out record back as null - a
      /// page that read layoutOptions.Sizes.LogoSize died on exactly that.
      static member Normalize (lo: LayoutOption) =
        if isNull (box lo) then LayoutOption.Default
        else
          { lo with
              Fonts = (if isNull (box lo.Fonts) then LayoutOption.Default.Fonts else lo.Fonts)
              Sizes = (if isNull (box lo.Sizes) then LayoutOption.Default.Sizes else lo.Sizes)
              Charts = (if isNull (box lo.Charts) then Charts.Default else lo.Charts)
              // A file from before the setting existed says it with the two old flags.
              CrosstableWithStandings =
                (if not (isNull (box lo.CrosstableWithStandings)) then lo.CrosstableWithStandings
                 elif lo.OnlyShowStandings then Hidden
                 elif lo.ShowCrosstableBelowStandings then Below
                 else Cycle) }
