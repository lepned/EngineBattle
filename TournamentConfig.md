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
- **MainLogoFileName**: The filename of the tournament logo.
- **VerboseLogging**: Enable or disable verbose logging.
- **MoveAnnotation**: Move annotation detail level in PGN output. Values: `"None"` (no annotation), `"Minimal"` (eval, depth, move time), `"Standard"` (11 fields), `"Full"` (all 22 fields). Backward compatible: `true` → Full, `false` → Standard.
- **MinMoveTimeInMS**: Minimum move time in milliseconds.
- **TournamentMode**: Tournament mode (RR, Cup, Swiss, Gauntlet, Ladder).
- **AllowPondering**: Allow engines to ponder during the opponent’s time.
- **EngineStartupTimeoutInSec**: Engine startup timeout in seconds.
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
- **NumberOfGamesInParallelConsoleOnly**: Number of games to run in parallel in console mode. Applies to Round Robin and Gauntlet; Cup, Swiss and Ladder always run sequentially. Parallel play disables PreventMoveDeviation.

### LiveFeed (optional)

Broadcasts every game to a WebGUI for live viewing — with parallel console games this gives one live board per concurrent game on the `/tournament-grid` page. The section is a no-op when absent. Each field can be overridden at runtime by the matching `EB_LIVEFEED_*` environment variable.

- **Url**: WebGUI ingest endpoint to POST wire events to, e.g. `http://localhost:5018/api/livefeed`. Blank = off.
- **Source**: Server label shown in the grid; blank lets the WebGUI use the remote IP.
- **File**: Also record the feed to this NDJSON file (for later replay). Blank = off.
- **Token**: Authentication token, required only when the WebGUI host is started with the `EB_LIVEFEED_TOKEN` environment variable set (the values must match).

See `LiveFeedContract.md` for the wire protocol and the README section *Watching Parallel Games Live in the WebGUI* for a walkthrough.

### Opening Options

- **OpeningsPath**: Path to the openings file, either a PGN-file or an EPD-file.
- **OpeningsPly**: Number of plies for openings.
- **OpeningsTwice**: Use openings twice.
- **RandomOpenings**: True to randomize opening order for Round Robin and Gauntlet modes. Cup and Swiss modes have their own RandomOpenings in CupOptions/SwissOptions.
- **Seed**: Integer seed for the opening shuffle. Has effect only when `RandomOpenings = true`. **Default: 0** (what you get if you omit the field). The seed does not depend on engine names or the PGN path, so the shuffle is stable across engine-list changes and reproducible across machines for the same book. Set `Seed` to any integer you like to pick a different shuffle for a particular tournament. The effective seed is logged at tournament start.

### Cup Options

- **RoundPairIncrements**: Optional list of pairs per round (each pair is two games). If empty, defaults to one pair (2 games).
- **SeedingStrategy**: "ByRating" or "Random".
- **UniquePerMatchOnly**: True to reuse openings across matches, false to enforce global uniqueness.
- **BracketPath**: Path for the generated cup bracket JSON file.
- **RandomOpenings**: True to randomize openings instead of using the list order.

### Swiss Options

- **GamesPerMatch**: Number of games per match (even numbers recommended).
- **Rounds**: Number of rounds for swiss (defaults to global Rounds if 0).
- **SeedGroupCount**: Number of seed groups for TCEC-style seeding.
- **UniquePerMatchOnly**: True to reuse openings across matches, false to enforce global uniqueness.
- **RandomOpenings**: True to randomize openings instead of using the list order.
- **AllowExtraPairsOnTie**: True to play extra pairs if the top score is tied after scheduled rounds.
- **StatePath**: Path for the generated swiss state JSON file.

### Ladder Options

- **GamePairsPerMatch**: Number of game pairs per mini-match (each pair is 2 games with reversed colors). Default: 4.
- **RandomOpenings**: True to randomize openings instead of using the list order.
- **StatePath**: Path for the generated ladder state JSON file.

Ladder mode is an elimination-style climbing tournament. Engines are ranked by rating (highest = rank 1). The lowest-ranked surviving engine challenges the one above it. The loser is eliminated, the winner continues climbing. When a climber loses, a new climb starts from the new bottom engine. Tied matches play extra game pairs until one engine leads. The tournament ends when only 1 engine remains.

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
  - **CupBracketFont**: Font size for the cup bracket overview.
  - **SwissOverviewFont**: Font size for the swiss overview.
  - **LadderOverviewFont**: Font size for the ladder overview.
  - **PairingsFont**: Font size for the pairings table.
  - **LatestGamesFont**: Font size for the latest games.
- **Sizes**:
  - **LiveChartHeight**: Height of the live chart, which is MCTS charts (typically Lc0 and Ceres) for Top N visited moves and Top N Q-values (eval).
  - **MoveChartHeight**: Height of the move chart, which is regular Eval, NPS, NPM and Time charts.
  - **PVboardSize**: Size of the PV board.
  - **LogoSize**: Maximum size for engine logos. Format: "WxH" (e.g., "150x100") for width x height, or "N" for square (e.g., "120" for 120x120). Empty string or omitted uses default sizing. These values act as upper bounds; logos still shrink on narrow screens.
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
- **WmovesToGo** / **BmovesToGo**: Moves per repeating time-control period (e.g. `40` = "40 moves, then more time"). `0` (default) = sudden-death/increment with no `movestogo`. When `> 0`, EB sends a counting-down UCI `movestogo` each move and tops up the base time (`Fixed`) at every period boundary, with the **same base repeating** each period. Symmetric only (both sides use `max(W,B)`); multi-stage controls such as `40/90 + 30/30` are not supported.


## Tournament.json example - copy this as template

```
{
  "Name": "Demo tournament",
  "Description": "Testing features",
  "OS": "Win11 x64",
  "CPU": "i9-13900K CPU 32 Threads",
  "RAM": "32 Gb RAM",
  "GPU": "1x RTX 3080",
  "MainLogoFileName": "MainLogoEB.png",
  "VerboseLogging": false,
  "MoveAnnotation": "Standard",
  "MinMoveTimeInMS": 400,
  "TournamentMode": "RR",
  "AllowPondering": false,
  "EngineStartupTimeoutInSec": 900,
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
    "OpeningsTwice": true,
    "RandomOpenings": false,
    "Seed": 12345
  },
  "CupOptions": {
    "RoundPairIncrements": [1,2,3],
    "SeedingStrategy": "ByRating",
    "UniquePerMatchOnly": true,
    "BracketPath": "wwwroot/cup_bracket.json",
    "RandomOpenings": true
  },
  "SwissOptions": {
    "GamesPerMatch": 2,
    "Rounds": 0,
    "SeedGroupCount": 1,
    "UniquePerMatchOnly": true,
    "RandomOpenings": true,
    "AllowExtraPairsOnTie": true,
    "StatePath": "wwwroot/swiss_state.json"
  },
  "LadderOptions": {
    "GamePairsPerMatch": 4,
    "RandomOpenings": true,
    "StatePath": "wwwroot/ladder_state.json"
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
      "CupBracketFont": 20,
      "SwissOverviewFont": 20,
      "LadderOverviewFont": 20,
      "PairingsFont": 16,
      "LatestGamesFont": 16
    },

    "Sizes": {
      "LiveChartHeight": 240,
      "MoveChartHeight": 250,
      "PVboardSize": "medium",
      "LogoSize": ""
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
