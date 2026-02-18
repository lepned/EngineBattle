namespace ChessLibrary

open System

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
      LogoSize: string }

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
      static member Default =
        { ShowNPS = false
          ShowEval = true
          ShowNodes = false
          ShowTime = true
          NumberOfLines = 3
          Qdiff = 0.5 }
  type LayoutOption =
    { Fonts: Fonts
      Sizes: Sizes
      Charts: Charts
      ShowPVBoard: bool
      UseNPM: bool
      BestMoveWithPolicy: bool
      OnlyShowStandings: bool
      ShowCrosstableBetweenGames: bool
      ShowCrosstableBelowStandings: bool
      AutoCycleTimeInSec: int }
    with
      static member Default =
        { Fonts =
            { StandingsFont = 12
              PairingsFont = 12
              LatestGamesFont = 12
              CrossTableFont = 13
              CupBracketFont = 16
              SwissOverviewFont = 16
              LadderOverviewFont = 16
              MoveListFont = 15
              InfoBannerFont = 14
              TournamentDescFont = 12
              EnginesPanelFont = 14
              PVLabelFont = 12 }
          Sizes =
            { LiveChartHeight = 200
              MoveChartHeight = 200
              PVboardSize = "medium"
              LogoSize = "" }
          Charts = Charts.Default
          ShowPVBoard = false
          UseNPM = false
          BestMoveWithPolicy = false
          OnlyShowStandings = false
          ShowCrosstableBetweenGames = false
          ShowCrosstableBelowStandings = false
          AutoCycleTimeInSec = 30 }
