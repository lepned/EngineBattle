# Tournament Configuration

This document provides an overview of the `tournament.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for running a chess engine tournament.

## Configuration Fields

### General Information

- **Name**: The name of the tournament.
- **Description**: A brief description of the tournament.
- **OS**: The operating system used for the tournament.
- **CPU**: The CPU specifications.
- **RAM**: The amount of RAM available.
- **GPU**: The GPU specifications.
- **LogoFileName**: The filename of the tournament logo.
- **VerboseLogging**: Enable or disable verbose logging.
- **VerboseMoveAnnotation**: Enable or disable detailed move annotations in the output (such as extra engine analysis for each move).
- **MinMoveTimeInMS**: Minimum move time in milliseconds.
- **Gauntlet**: Indicates if the tournament is a gauntlet.
- **PreventMoveDeviation**: Prevent move deviation option.
- **Challengers**: Number of challengers in the tournament.
- **Rounds**: Number of rounds in the tournament.
- **PauseAfterRound**: Pause duration after each round in seconds.
- **DelayBetweenGames**: Delay between games in HH:MM:SS:MMM format.
- **MoveOverhead**: Move overhead time in HH:MM:SS:MMM format.

### Adjudication Options

- **DrawOption**:
  - **DrawMoveLength**: Number of moves to consider for a draw.
  - **MaxDrawScore**: Maximum score for a draw.
  - **MinDrawMove**: Minimum number of moves for a draw.
- **WinOption**:
  - **MinWinScore**: Minimum score for a win.
  - **WinMoveLength**: Number of moves to consider for a win.
  - **MinWinMove**: Minimum number of moves for a win.
- **TBAdj**:
  - **TablebaseDirectory**: Directory for tablebases.
  - **UseTBAdjudication**: Enable or disable tablebase adjudication.
  - **TBMen**: Number of men in tablebase adjudication.

### Test Options

- **PolicyTest**: Enable or disable policy tests.
- **ValueTest**: Enable or disable value tests - WIP.
- **WriteToConsole**: Enable or disable writing to console - WIP.
- **NumberOfGamesInParallelConsoleOnly**: Number of games to run in parallel in console mode.

### Opening Options

- **OpeningsPath**: Path to the openings file, either a PGN-file or an EPD-file.
- **OpeningsPly**: Number of plies for openings.
- **OpeningsTwice**: Use openings twice.

### Output Paths

- **PgnOutPath**: Path to the output PGN file.
- **ReferencePGNPath**: Path to the reference PGN file.

### Engine Setup

- **EngineDefFolder**: Folder containing engine definitions.
- **EngineDefList**: List of engine definition files.

### Layout Options

- **Fonts**:
  - **InfoBannerFont**: Font size for the info banner - top row.
  - **TournamentDescFont**: Font size for the tournament description.
  - **EnginesPanelFont**: Font size for the engines panel that shows search and time info for each engine.
  - **MoveListFont**: Font size for the move list below the chessboard.
  - **PVLabelFont**: Font size for the PV label below the chessboard.
  - **StandingsFont**: Font size for the standings table.
  - **CrossTableFont**: Font size for the cross table.
  - **PairingsFont**: Font size for the pairings table.
  - **LatestGamesFont**: Font size for the latest games.
- **Sizes**:
  - **LiveChartHeight**: Height of the live chart, which is MCTS charts (typically Lc0 and Ceres) for Top N visited moves and Top N Q-values (eval).
  - **MoveChartHeight**: Height of the move chart, which is regular Eval, NPS, NPM and Time charts.
  - **PVboardSize**: Size of the PV board.
- **Charts**:
  - **ShowEval**: Show evaluation chart.
  - **ShowNPS**: Show nodes per second chart.
  - **ShowTime**: Show time usage chart.
  - **ShowNodes**: Show nodes per move chart.
  - **NumberOfLines**: Number of lines in the chart.
  - **Qdiff**: Q difference for the chart, used to filter moves in the live chart.
- **ShowPVBoard**: Principal Variation (PV) Boards below each player.
- **UseNPM**: Use nodes per move in standings table instead of NPS.
- **BestMoveWithPolicy**: Show best move with policy - WIP.
- **OnlyShowStandings**: Only show standings.
- **ShowCrosstableBetweenGames**: Show cross table between games.
- **ShowCrosstableBelowStandings**: Show cross table below standings.
- **AutoCycleTimeInSec**: Auto cycle time in seconds, used to recycle tournament tables.

### Time Control

- **TimeConfigs**: List of time control configurations, which can be used in engineDef.json.
  - **Id**: Time control ID.
  - **Fixed**: Fixed time control in HH:MM:SS:MMM format.
  - **Increment**: Increment time control in HH:MM:SS:MMM format.
  - **NodeLimit**: Enable or disable node limit.
  - **Nodes**: Number of nodes for the time control.
- **WmovesToGo**: Number of white moves to go.
- **BmovesToGo**: Number of black moves to go.


## Tournament.json example - copy this as template

```
{
  "Name": "Demo tournament",
  "Description": "Testing features",
  "OS": "Win11 x64",
  "CPU": "i9-13900K CPU 32 Threads",
  "RAM": "32 Gb RAM",
  "GPU": "1x RTX 3080",
  "LogoFileName": "MainLogoEB.png",
  "VerboseLogging": false,
  "VerboseMoveAnnotation": false,
  "MinMoveTimeInMS": 400,
  "Gauntlet": true,
  "PreventMoveDeviation": false,
  "Challengers": 1,
  "Rounds": 50,
  "PauseAfterRound": 0,
  "DelayBetweenGames": "00:00:20.000",
  "MoveOverhead": "00:00:00.100",
  "Adjudication": {
    "DrawOption": {
      "DrawMoveLength": 5,
      "MaxDrawScore": 0.30,
      "MinDrawMove": 20
    },

    "WinOption": {
      "MinWinScore": 5.0,
      "WinMoveLength": 5,
      "MinWinMove": 0
    },

    "TBAdj": {
      "TablebaseDirectory": "C:/Dev/Chess/TableBases",
      "UseTBAdjudication": false,
      "TBMen": 6
    }
  },

  "TestOptions": {
    "PolicyTest": false,
    "ValueTest": false,
    "WriteToConsole": false,
    "NumberOfGamesInParallelConsoleOnly": 2
  },

  "Opening": {
    "OpeningsPath": "C:/Dev/Chess/Openings/sufi25.pgn",
    "OpeningsPly": 100,
    "OpeningsTwice": true
  },

  "PgnOutPath": "C:/Dev/Chess/PGNs/quickTest001.pgn",
  "ReferencePGNPath": "",
  "EngineSetup": {
    "EngineDefFolder": "C:/Dev/Chess/Engines/EngineDefs",
    "EngineDefList": [
      "SFDef.json",
      "DragonDef.json",
      "Lc0Def.json"
    ]
  },

  "LayoutOption": {
    "Fonts": {
      "InfoBannerFont": 21,
      "TournamentDescFont": 20,
      "EnginesPanelFont": 22,
      "MoveListFont": 20,
      "PVLabelFont": 21,
      "StandingsFont": 20,
      "CrossTableFont": 22,
      "PairingsFont": 16,
      "LatestGamesFont": 16
    },

    "Sizes": {      
      "LiveChartHeight": 240,
      "MoveChartHeight": 250,
      "PVboardSize": "medium"
    },

    "Charts": {
      "ShowEval": true,
      "ShowNPS": true,
      "ShowTime": false,
      "ShowNodes": false,
      "NumberOfLines": 5,
      "Qdiff": 1.0
    },

    "ShowPVBoard": true,
    "UseNPM": false,
    "BestMoveWithPolicy": false,
    "OnlyShowStandings": true,
    "ShowCrosstableBetweenGames": true,
    "ShowCrosstableBelowStandings": false,
    "AutoCycleTimeInSec": 30
  },

  "TimeControl": {
    "TimeConfigs": [
      {
        "Id": 1,
        "Fixed": "00:01:00.000",
        "Increment": "00:00:01.000",
        "NodeLimit": false,
        "Nodes": 1
      },
      {
        "Id": 2,
        "Fixed": "00:03:00.000",
        "Increment": "00:00:02.000",
        "NodeLimit": false,
        "Nodes": 1
      },
      {
        "Id": 3,
        "Fixed": "00:05:00.000",
        "Increment": "00:00:03.000",
        "NodeLimit": false,
        "Nodes": 100
      }
    ],
    "WmovesToGo": 0,
    "BmovesToGo": 0
  }

}