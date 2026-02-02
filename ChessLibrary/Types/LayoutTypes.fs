namespace ChessLibrary

module LayoutTypes =
  type Fonts =
    { StandingsFont: int
      PairingsFont: int
      LatestGamesFont: int
      CrossTableFont: int
      CupBracketFont: int
      SwissOverviewFont: int
      MoveListFont: int
      InfoBannerFont: int
      TournamentDescFont: int
      EnginesPanelFont: int
      PVLabelFont: int }
  type Sizes =
    { LiveChartHeight: int
      MoveChartHeight: int
      PVboardSize: string }
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
              MoveListFont = 15
              InfoBannerFont = 14
              TournamentDescFont = 12
              EnginesPanelFont = 14
              PVLabelFont = 12 }
          Sizes =
            { LiveChartHeight = 200
              MoveChartHeight = 200
              PVboardSize = "medium" }
          Charts = Charts.Default
          ShowPVBoard = false
          UseNPM = false
          BestMoveWithPolicy = false
          OnlyShowStandings = false
          ShowCrosstableBetweenGames = false
          ShowCrosstableBelowStandings = false
          AutoCycleTimeInSec = 30 }
