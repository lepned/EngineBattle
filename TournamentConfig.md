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
- **MoveAnnotation**: Move annotation detail level in PGN output. Values: `"None"` (no annotation), `"Minimal"` (eval, nodes, speed, move time), `"Standard"` (11 fields), `"Full"` (18 fields). Backward compatible: `true` → Full, `false` → Standard.
- **MinMoveTimeInMS**: Minimum move time in milliseconds.
- **TournamentMode**: Tournament mode (RR, Cup, Swiss, Gauntlet, Ladder).
- **AllowPondering**: Allow engines to ponder during the opponent’s time, in every tournament mode. EngineBattle sets each engine's UCI `Ponder` option itself (a `Ponder` in an engine def is overridden), so nothing has to be added to the defs: `true` for an engine that will be asked to ponder, `false` otherwise. Pondering needs a clock: there is none on a time per move (`St`) or a node limit, nor with `PreventMoveDeviation` or the value/policy tests. When it is on, every engine is started once before the tournament, and all of them must be able to ponder (a UCI engine with a `Ponder` option), so that all play on equal terms: one that cannot, or a Winboard engine, stops the tournament with a message saying which. An engine that fails to start stops it too.
- **EngineStartupTimeoutInSec**: Engine startup timeout in seconds.
- **PreventMoveDeviation**: Prevent move deviation option.
- **Challengers**: Number of challengers in the tournament.
- **Rounds**: Number of rounds in the tournament. In round robin, and in a gauntlet with `RandomOpenings: false`, each round uses the next opening of the book, and a book shorter than `Rounds` starts over from its first opening. A gauntlet with `RandomOpenings: true` gives each opponent openings of its own instead - see `RandomOpenings`.
- **PauseAfterRound**: A round number; the WebGUI stops the tournament once a round beyond it starts (0 = never). Console runs ignore it.
- **DelayBetweenGames**: Delay between games, e.g. `00:00:20`.
- **MoveOverhead**: Move overhead time, e.g. `00:00:00.100`. Sent to the engines as their move overhead and allowed as a margin before a loss on time. It is sent only to an engine with a spin option whose name contains "overhead" (e.g. `Move Overhead`, `MoveOverheadMs`), only when the value lies within that option's range, and not when the engine def sets that option itself. For an engine on a time per move (`MoveTime`) it is a margin only - not sent - and never less than 50 ms.
- **OrdoExePath**: Path to the Ordo executable, used for the standings of console runs: the periodic summaries run Ordo on the output PGN, and a final pass with draw-rate calibration runs at the end. Empty = EngineBattle's own standings table. The GUI's Tournament creator fills it from the Ordo path in Settings.
- **ConsoleOnly**: Internal. Set automatically to `true` when the tournament runs from the console; there is no need to write it.

> **Time format**: `hh:mm:ss` with an optional `.fff` for milliseconds. No field has an upper
> limit, so `00:00:90` is ninety seconds, `00:60:00` is an hour and `30:00:00` is thirty hours.
> All three fields are required — `10:00` is rejected rather than guessed at, since it reads as
> ten hours to a parser and ten minutes to a person. Days are not part of the format: write long
> controls in hours. Values are normalised when EngineBattle saves the file, so `00:00:90`
> becomes `00:01:30` and the file always states what the value actually is.


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
- **NumberOfGamesInParallel**: Number of games to run in parallel (console and WebGUI). Applies to Round Robin and Gauntlet; Cup, Swiss and Ladder always run sequentially. The number is lowered automatically when one copy of each engine, times that number, does not fit in 70% of the RAM (each engine is started once before play to measure it; a console `match` prints a line saying so when it lowers the number). A `GPUs` list with more than one entry raises it to at least the number of GPUs. PreventMoveDeviation works with parallel play: a game that repeats an earlier game's opening and colours waits until that game has finished and merged its moves, so the parallelism you actually get is bounded by how many distinct opening/engine/colour combinations are ready - a gauntlet with `RandomOpenings: false`, where the challenger plays each opening with the same colour against every opponent, degrades toward sequential. Replay from a reference PGN or a resumed PGN is unaffected (the file is complete before play starts), which is how the tuner uses it. In the WebGUI, starting a tournament with a value above 1 opens the multi-board grid (`/tournament-grid`); user adjudication of the running game is available when this is 1. Engines are started when a game first needs them and stopped after a game unless one of the next few games (as many as run in parallel) needs them again, so a large field never has every engine running at once; a two-engine match keeps both. The old name `NumberOfGamesInParallelConsoleOnly` is still accepted when loading.
- **GPUs** (optional): List of GPU device ids for parallel games, e.g. `[0, 1]`. Each engine runs up to one instance per parallel game, and instance *i* (counting from 0) of every engine gets id `GPUs[i % GPUs.Length]`, so with two GPUs the first copy of each engine uses GPU 0 and the second GPU 1. The id is written into the engine's device option as named by `DeviceOption` and `DeviceTemplate` in its engine def (see `EngineDefConfig.md`); an engine def without them starts unchanged. Omitted, `null` or empty = no device assignment.

### LiveFeed (optional)

Broadcasts every game to a WebGUI for live viewing — with parallel console games this gives one live board per concurrent game on the `/tournament-grid` page. The section is a no-op when absent. Each field can be overridden at runtime by the matching `EB_LIVEFEED_*` environment variable.

- **Url**: WebGUI ingest endpoint to POST wire events to, e.g. `http://localhost:5018/api/livefeed`. Blank = off.
- **Source**: Server label shown in the grid; blank lets the WebGUI use the remote IP.
- **File**: Also record the feed to this NDJSON file (for later replay). Blank = off.
- **Token**: Authentication token, required only when the WebGUI host is started with the `EB_LIVEFEED_TOKEN` environment variable set (the values must match).

See `LiveFeedContract.md` for the wire protocol and the README section *Watching Parallel Console Games Live in the WebGUI* for a walkthrough.

### Opening Options

- **OpeningsPath**: Path to the openings file, either a PGN-file or an EPD-file.
- **OpeningsPly**: Number of plies for openings.
- **OpeningsTwice**: Use openings twice. With `true` every pair plays each opening with both colours. With `false` each opening is played once, and the colours swap from one opening (round robin) or round (gauntlet) to the next, so every engine gets White about as often as Black.

> **Changed in version 1.9 - read before resuming an older tournament.** Before 1.9, a round robin with `OpeningsTwice: false` gave every pair the same colours in every round (with two engines the same one was White in every game), and a gauntlet gave the challenger White in every game. A round robin also stopped at the end of the book when `Rounds` was larger. A tournament of either kind started with an older version and resumed with 1.9 is planned the new way, and the games already played are matched against that plan: expect some openings to be played again with the colours swapped, and a finished round robin with `Rounds` larger than its book to show games left. Start such a tournament over, with a new `PgnOutPath`, rather than resuming it. Tournaments with `OpeningsTwice: true` and a book at least as long as `Rounds` are not affected.
- **RandomOpenings**: True to randomize opening order for Round Robin and Gauntlet modes. Cup and Swiss modes have their own RandomOpenings in CupOptions/SwissOptions. In a gauntlet it also decides how the openings are shared between the opponents:
  - `false` (shared): the first `Rounds` openings of the book, in order, and every opponent plays the same opening in a round.
  - `true` (spread): with more than one opponent, the first `Rounds` x opponents openings of the book are shuffled and each opponent gets its own openings, none played against another opponent - in round *r* opponent *i* plays opening *r* x opponents + *i* of the shuffled list. A book shorter than that starts over from its first opening, so some openings are played against more than one opponent, and a warning is printed at the start.
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

Everything about how the tournament page LOOKS is set **in the app**, not here. On the
tournament page, the control in the bottom-right corner (it appears when the pointer comes for
it) has A-/A+ for the text, off/S/M/L for the PV boards, -/+ for the chart heights, toggles for
which charts are shown, **save** to make the sizes on screen the baseline for this screen, and
**reset** to go back to the defaults. Appearance has the rest under *Tournament page*: each
region of text on its own, chart lines and Q difference, nodes per move in the standings,
where the crosstable goes (cycling with the standings, below them, or hidden), the crosstable between games, and the cycle time.
Everything is remembered per screen, so a laptop and the monitor it docks to each keep their
own numbers.

The whole `LayoutOption` block is therefore **optional**, and a fresh installation does not
have one. So is every field inside it: a block that sets nothing but a logo size leaves
everything else at the default, and a `Charts` or `Sizes` block may name just the one value it
wants. They are still read for a screen that has nothing saved yet -
an older config keeps rendering exactly as it did - and ignored from the first "save" or slider
on that screen. Without them the defaults are: standings, crosstable, brackets, banner and
engine panel 18 px; pairings, latest games, move list and description 16 px; PV lines 17 px;
charts 200 px; PV boards on, small.

- **Fonts** (optional): a size in px per region, used until the screen has sizes of its own.
  `InfoBannerFont`, `TournamentDescFont`, `EnginesPanelFont`, `MoveListFont`, `PVLabelFont`,
  `StandingsFont`, `CrossTableFont`, `PairingsFont`, `LatestGamesFont`, and the three bracket
  views `CupBracketFont`, `SwissOverviewFont`, `LadderOverviewFont` (one region on screen: the
  largest of the three applies).
- **Sizes**:
  - **LiveChartHeight** (optional): Height of the live chart, which is MCTS charts (typically Lc0 and Ceres) for Top N visited moves and Top N Q-values (eval). Used until the screen has a height of its own.
  - **MoveChartHeight** (optional): Height of the move chart, which is regular Eval, NPS, NPM and Time charts. Same rule.
  - **PVboardSize** (optional): Size of the PV board, `small`, `medium` or `large`. The corner control's choice wins once made.
  - **LogoSize**: Maximum size for engine logos. Format: "WxH" (e.g., "150x100") for width x height, or "N" for square (e.g., "120" for 120x120). Empty string or omitted uses default sizing. These values act as upper bounds; logos still shrink on narrow screens.
  - **MainLogoSize**: Maximum size for the main logo in the middle of the engine panel (the image `MainLogoFileName` points at). Same format as LogoSize. Empty string or omitted means 240x130. An upper bound, like LogoSize: the logo still shrinks with the panel on narrow screens, it just never grows past this.
- **Charts** (optional, every field; the corner control and Appearance override them per screen):
  - **ShowEval**: Show evaluation chart.
  - **ShowNPS**: Show nodes per second chart.
  - **ShowTime**: Show time usage chart.
  - **ShowNodes**: Show nodes per move chart.
  - **NumberOfLines**: Number of lines in the chart.
  - **Qdiff**: Q difference for the chart, used to filter moves in the live chart.
- **ShowPVBoard**: Principal Variation (PV) Boards below each player.
- **UseNPM**: Use nodes per move in standings table instead of NPS.
- **BestMoveWithPolicy**: Show best move with policy - WIP.
- **CrosstableWithStandings**: Where the crosstable goes in the left column: `"cycle"` (standings and crosstable take turns in the same box), `"below"` (a box of its own under the standings) or `"none"` (standings only, the default). Replaces the older `OnlyShowStandings` and `ShowCrosstableBelowStandings`, which are still read when this field is absent.
- **ShowCrosstableBetweenGames**: Show the crosstable (the bracket in cup and ladder) in a dialog between games.
- **AutoCycleTimeInSec**: Seconds between the tables the standings box cycles through - the crosstable in `cycle` mode, and the progress tables in cup and ladder.

### Time Control

`TimeControl.TimeConfigs` lists the time settings of the tournament. Each engine plays the
setting whose `Id` its engine def names in `TimeControlID`, so two engines in one tournament
can play different time controls (a handicap match, a node-limited net against a timed one).

Write a setting in the short form - the same notation as `match`'s `tc=` and `st=`:

```json
"TimeControl": {
  "TimeConfigs": [
    { "Id": 1, "Tc": "60+1" },
    { "Id": 2, "Tc": "2:30+2" },
    { "Id": 3, "Tc": "40/5:00+2" },
    { "Id": 4, "St": 1 },
    { "Id": 5, "Nodes": 800 }
  ]
}
```

| Setting | Means | Examples |
|---|---|---|
| `"Tc": "base+inc"` | a clock: base time, and an increment added after every move | `"60+1"` 60 s + 1 s, `"10+0.1"`, `"5:00"` 5 min, no increment |
| base as `m:ss` | minutes and seconds | `"2:30+2"` 2 min 30 s + 2 s, `"90:00+30"` 90 min + 30 s |
| `"Tc": "moves/base+inc"` | a repeating period: after that many moves the base is added again | `"40/5:00+2"` 5 min per 40 moves + 2 s, `"40/300"` |
| `"St": seconds` | a fixed time per move, no clock (`go movetime`) | `1`, `0.5`, `"0.5"` |
| `"Nodes": n` | a node limit per move, no clock (`go nodes`) | `800`, `150000` |

- Seconds are the unit everywhere: the base (or `m:ss`), the increment and `St`. Decimals are fine
  (`"10+0.1"`, `"St": 0.25`); a trailing `s` is allowed (`"60s+1"`). There is no `m` unit - write
  minutes as `m:ss`.
- A setting gives its time one way. `Tc` or `St` together with `Fixed`, `Increment`, `MoveTime`
  or `MovesToGo` is an error, and so is a `Tc` that cannot be read - the file is refused with a
  message naming the setting, rather than a guess.
- Each setting has its own period: one engine can play `"40/5:00"` while another plays
  `"60/10:00"`.

How each kind plays:

- **Clock (`Tc`)**: the engine gets `go wtime .. btime .. winc .. binc ..` (and `movestogo` with a
  period). It loses on time when its clock goes below zero by more than `MoveOverhead`.
- **Time per move (`St`)**: the engine gets `go movetime`. It loses on time when a move takes
  longer than `St` plus a margin: `MoveOverhead`, and at least 50 ms. The margin is EngineBattle's
  own and is not sent to the engine (an engine answers a millisecond or three after its movetime;
  a loss over that says nothing about the engine). The clock in the GUI shows `St` plus the margin
  at every move and counts down while the engine thinks.
- **Node limit (`Nodes`)**: the engine gets `go nodes`. There is no clock and no loss on time; the
  GUI shows the limit (`800 nodes`) where a clock would be.

The time settings in the GUI's banner and the PGN are written the same way: `1' + 1''`,
`40/5' + 2''`, `1'' / move`, `800 nodes`.

#### The long form

The fields the short form stands for. A file in this form reads as it always did, and the
Tournament creator writes it:

```json
{ "Id": 1, "Fixed": "00:01:00", "Increment": "00:00:01", "NodeLimit": false, "Nodes": 0 }
```

- **Id**: the time setting's ID, named by `TimeControlID` in an engine def.
- **Fixed**: the base time, `hh:mm:ss` with optional `.fff`.
- **Increment**: added after every move, same format.
- **MovesToGo**: moves per repeating period, `0` or left out = none.
- **MoveTime**: a fixed time per move instead of a clock (`"00:00:01"`); left out = off.
- **NodeLimit** / **Nodes**: `true` and a node count for a node limit. `Nodes` alone, with no time
  and no `NodeLimit`, is a node limit too.

A field a setting leaves out is zero or off, so `{ "Id": 3, "MoveTime": "00:00:02" }` is a whole
setting.

- **WmovesToGo** / **BmovesToGo** (next to `TimeConfigs`, optional): a period for every setting
  without its own `MovesToGo`, as older files set it for the whole tournament. Left out = none.
  When a period is set, EB sends a counting-down UCI `movestogo` each move and adds the base time
  (`Fixed`) again at every period boundary, the **same base** each period; multi-stage controls
  such as `40/90 + 30/30` are not supported.

> **Tournament creator**: the GUI's creator writes its own time settings in the long form and does
> not read a file's settings first - saving a tournament from it replaces settings written by hand
> (`St`, a period, sub-second increments). Keep a copy of a hand-written file.

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
    "NumberOfGamesInParallel": 2
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
    "CrosstableWithStandings": "none",
    "ShowCrosstableBetweenGames": true,
    "AutoCycleTimeInSec": 30
  },

  "TimeControl": {
    "TimeConfigs": [
      { "Id": 1, "Tc": "60+1" },
      { "Id": 2, "Tc": "3:00+2" },
      { "Id": 3, "Tc": "5:00+3" }
    ]
  }

}
```
