# Console Tools

This document provides a comprehensive guide to using the EngineBattle Console CLI tools for running chess engine tournaments, puzzle tests, and benchmarks.

## Overview

The Console project is the command-line interface for EngineBattle. It provides tools for running tournaments, testing engines against puzzles, benchmarking UCI options, verifying move generation, and launching the WebGUI.

### Building and Running

```bash
# Open the Console folder
cd Console

# Build the Console project
dotnet build -c Release

# Run with a command
dotnet run -c Release -- <command> [arguments]
```

### General Syntax

```
dotnet run -c Release -- <command> <path-or-arguments>
```

### Command Summary

| Command | Aliases | Description |
|---------|---------|-------------|
| `tournamentjson` | `tournament`, `t` | Run a tournament from JSON config |
| `puzzlejson` | `puzzle`, `p` | Run puzzle evaluation from JSON config |
| `eretjson` | `eret` | Run ERET evaluation from JSON config |
| `analyze` | `a` | Analyze a position with an engine |
| `compare` | `cmp` | Compare two engines side-by-side |
| `benchmark` | `bench`, `b` | Run engine benchmark |
| `tune` | | Run Bayesian parameter tuner |
| `redash` | | Regenerate BO dashboard from saved state |
| `pgnsummary` | `pgn`, `ps` | Analyze PGN game terminations |
| `pgncheck` | `pc` | Parser health check on a PGN file (no analysis) |
| `elo` | `e` | Show Elo ratings and results from PGN |
| `speed` | `sp` | Show speed statistics from PGN |
| `validate` | `v` | Validate a tournament config without running |
| `perft` | | Run perft move generation test |
| `gui` | | Launch WebGUI |
| `help` | `h` | Show help message |

---

## Commands

### tournamentjson

Runs a chess engine tournament using a JSON configuration file.

**Syntax:**
```bash
dotnet run -c Release -- tournamentjson <path-to-tournament.json>
```

**Example:**
```bash
dotnet run -c Release -- tournamentjson C:/Dev/Chess/Tournaments/my_tournament.json
```

**Description:**
- Loads tournament configuration from the specified JSON file
- Initializes engines from the `EngineDefFolder` using files listed in `EngineDefList`
- Runs games according to the tournament mode (RR, Swiss, Cup, or Gauntlet)
- Outputs PGN to the configured `PgnOutPath`
- Displays live standings and results in the console

**Configuration:** See [TournamentConfig.md](TournamentConfig.md) for full configuration reference.

---

### puzzlejson

Runs Lichess puzzle tests against one or more chess engines.

**Syntax:**
```bash
dotnet run -c Release -- puzzlejson <path-to-puzzle-config.json>
```

**Example:**
```bash
dotnet run -c Release -- puzzlejson C:/Dev/Chess/Puzzles/PuzzleConfig.json
```

**Description:**
- Tests engines against puzzles from the Lichess puzzle database (CSV format)
- Supports three test types: `policy`, `value`, and `search`
- Groups results by rating ranges and puzzle themes
- Calculates performance ratings and accuracy percentages
- Saves failed puzzles and summary to the configured output folder

**Output:**
- Console table showing engine performance by puzzle type and rating group
- EPD file of failed puzzles (for further analysis)
- Summary text file with detailed statistics

**Configuration:** See [PuzzleConfig.md](PuzzleConfig.md) for full configuration reference.

---

### eretjson

Runs ERET (Engine Rapid Evaluation Tests) using EPD puzzle files.

**Syntax:**
```bash
dotnet run -c Release -- eretjson <path-to-eret-config.json>
```

**Example:**
```bash
dotnet run -c Release -- eretjson C:/Dev/Chess/ERET/EretConfig.json
```

**Description:**
- Tests engines against tactical puzzles in EPD format
- Supports both time-limited and node-limited searches
- Reports per-engine accuracy and failed puzzle details
- Saves failed puzzles to the configured output folder

**Output:**
- Per-puzzle result (correct/incorrect)
- Per-engine summary (correct count, failed count, accuracy)
- EPD file containing all failed puzzles
- Summary text file

**Configuration:** See [EretConfig.md](EretConfig.md) for full configuration reference.

---

### gui

Launches the WebGUI (Blazor Server) from the console.

**Syntax:**
```bash
dotnet run -c Release -- gui [page] [port]
```

**Examples:**
```bash
# Launch with default page (tournament) on default port (5018)
dotnet run -c Release -- gui

# Launch with specific page
dotnet run -c Release -- gui analysis/single

# Launch on custom port
dotnet run -c Release -- gui 5020

# Launch with specific page and port
dotnet run -c Release -- gui help 5020
```

**Arguments:**
- `page` (optional): The page route to open in the browser. Default: `tournament`
- `port` (optional): The port to run the server on. Default: `5018`

**Available Pages:**
- `/tournament` - Tournament runner and results
- `/analysis/single` - Single engine analysis
- `/analysis/dual` - Dual engine comparison
- `/analysis/game-review` - Game review and accuracy analysis
- `/EngineDef` - Engine definition overview
- `/tournamentSetup` - Tournament setup
- `/play-vs-computer` - Play against engine
- `/LichessPuzzles` - Lichess puzzle tests
- `/EretPuzzleTest` - ERET puzzle tests
- `/tools/pgn-tools` - PGN and EPD tools
- `/tools/book-evaluation` - Out-of-book position evaluation
- `/help` - Help and documentation
- `/speed` - Speed calculator
- `/ordo` - Ordo rating results
- `/deviationFinder` - Move deviation finder
- `/settings` - Global settings

**Description:**
- Starts the WebGUI Blazor Server application
- Opens the default browser at the specified page
- Press Ctrl+C or Enter to stop the server
- The server and all child processes are properly terminated on exit

**Running Multiple Instances:**

When one instance is running, `dotnet run` will fail to build due to locked DLLs. Use one of these workarounds:

```bash
# Option 1: Run from the built executable (faster)
.\bin\release\net10.0\EngineBattle.Console.exe gui analysis/single 5021

# Option 2: Skip the build step
dotnet run -c Release --no-build -- gui analysis/single 5021
```

---

### perft

Verifies Chess960 move generation correctness using PERFT (performance test).

**Syntax:**
```bash
dotnet run -c Release -- perft <depth> <sample-size>
```

**Example:**
```bash
# Test 10 random Chess960 positions at depth 5
dotnet run -c Release -- perft 5 10
```

**Arguments:**
- `depth`: Search depth (number of plies to explore)
- `sample-size`: Number of random Chess960 starting positions to test

**Description:**
- Generates random Chess960 starting positions
- Counts all legal move sequences to the specified depth
- Compares against known-correct node counts
- Reports any discrepancies (useful for debugging move generators)

---

### analyze

Analyzes a single position with a chess engine. Accepts engine JSON configs or bare exe paths.

**Aliases:** `a`

**Syntax:**
```bash
dotnet run -c Release -- analyze <engine> [fen] [options]
```

**Arguments:**
- `engine`: Path to an engine definition JSON file or a bare engine executable
- `fen` (optional): FEN string or `startpos` (default: `startpos`)

**Options:**
- `--fen S` — Set position (quoted FEN string)
- `--moves M...` — Append UCI moves to position (e.g. `--moves d2d4 d7d5 c2c4`). Consumes all subsequent non-`--` arguments.
- `--nodes N` — Search N nodes (default: 1000000)
- `--movetime N` — Search for N milliseconds
- `--depth N` — Search to depth N
- `--args S` — Override engine command-line arguments (e.g. `dag-preview`)
- `--uci K V` — Set any UCI option (repeatable, e.g. `--uci Backend onnx-trt`)
- `--options` — Show all UCI options supported by the engine and exit

**Examples:**
```bash
# Analyze with engine JSON config
dotnet run -c Release -- a engine.json startpos --nodes 100000

# Analyze with bare exe
dotnet run -c Release -- a C:/path/to/engine.exe startpos --depth 15

# Analyze specific position with UCI overrides
dotnet run -c Release -- a engine.json "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1" --nodes 10000

# Analyze with moves from startpos
dotnet run -c Release -- a engine.json --moves d2d4 d7d5 c2c4 --depth 20

# Show engine's UCI options
dotnet run -c Release -- a engine.json --options
```

**Output:**
- Raw UCI `info` lines with depth, eval, nodes, NPS, WDL, PV
- `info string` lines (e.g. Lc0's LogLiveStats)
- Raw `bestmove` line
- Summary: depth, eval, nodes, NPS, time, TBHits, WDL, bestmove, PV (UCI + SAN notation)

---

### compare

Side-by-side comparison of two engines across one or more positions. Creates both engines once and reuses them via `ucinewgame` between positions.

**Aliases:** `cmp`

**Syntax:**
```bash
dotnet run -c Release -- compare <engine1> <engine2> [options]
```

**Arguments:**
- `engine1`, `engine2`: Path to engine definition JSON files or bare engine executables

**Options:**
- `--fen S` — Set position (quoted FEN string)
- `--positions F` — EPD file with multiple positions
- `--nodes N` — Search N nodes (default: 1000000)
- `--movetime N` — Search for N milliseconds
- `--depth N` — Search to depth N
- `--threshold CP` — Only display positions where eval diff >= CP (summary still counts all)
- `--uci1 K V` — Set UCI option for engine 1 (repeatable)
- `--uci2 K V` — Set UCI option for engine 2 (repeatable)

**Examples:**
```bash
# Compare two engines on startpos
dotnet run -c Release -- cmp engine1.json engine2.json --nodes 100000

# Compare across multiple positions
dotnet run -c Release -- cmp engine1.json engine2.json --positions test.epd --depth 20

# Only show positions with large disagreements
dotnet run -c Release -- cmp engine1.exe engine2.exe --positions test.epd --threshold 0.5
```

**Output:**
- Table with FEN, per-engine eval and NPS, bestmove (agreed = single move, disagreed = `move1/move2`)
- Summary: move agreement %, avg/max eval diff with position #, NPS ratio

---

### benchmark

Runs engine benchmarks from a JSON configuration file.

**Aliases:** `bench`, `b`

**Syntax:**
```bash
dotnet run -c Release -- benchmark <path-to-config.json>
```

---

### tune

Runs the Bayesian parameter tuner.

**Syntax:**
```bash
dotnet run -c Release -- tune <path-to-tuner-config.json>
```

**Description:**
- Bayesian optimization with GP surrogate (Matern 5/2 ARD kernel)
- SPRT-based evaluation for each candidate
- Multi-phase tuning with parameter subsets
- Press Ctrl+C to gracefully stop

**Configuration:** See [Console/ConsoleTuner.md](Console/ConsoleTuner.md) for full configuration reference.

---

### redash

Regenerates the Bayesian optimizer HTML dashboard from a saved tuner state.

**Syntax:**
```bash
dotnet run -c Release -- redash <path-to-tuner-config.json>
```

---

### pgnsummary

Analyzes PGN game terminations and displays a summary.

**Aliases:** `pgn`, `ps`

**Syntax:**
```bash
dotnet run -c Release -- pgnsummary <path-to-pgn-file>
```

---

### pgncheck

Streams a PGN file through the parser and reports what it found: game and ply counts,
the longest game, games without moves, games without a result tag, games carrying a FEN
tag, throughput and peak memory. Deliberately does **no** analysis — `pgnsummary`, `elo`
and `speed` all replay boards or build statistics, so none of them tells you how the
parser itself behaves on a large file. Exits with code 1 if any game fails to parse.

Useful for validating a downloaded database before working with it, and as a regression
check after changes to the PGN parser.

**Aliases:** `pc`

**Syntax:**
```bash
dotnet run -c Release -- pgncheck <path-to-pgn-file>
```

---

### elo

Shows Elo ratings, results, and standings from a PGN file.

**Aliases:** `e`

**Syntax:**
```bash
dotnet run -c Release -- elo <path-to-pgn-file>
```

---

### speed

Shows speed statistics (NPS, nodes per move) from a PGN file.

**Aliases:** `sp`

**Syntax:**
```bash
dotnet run -c Release -- speed <path-to-pgn-file>
```

---

### validate

Validates a tournament configuration file without running the tournament.

**Aliases:** `v`

**Syntax:**
```bash
dotnet run -c Release -- validate <path-to-tournament.json>
```

---

### help

Displays available commands and their usage.

**Aliases:** `h`

**Syntax:**
```bash
dotnet run -c Release -- help
```

---

## Configuration File Examples

### Minimal Tournament Configuration

```json
{
  "Name": "Quick Test",
  "TournamentMode": "RR",
  "Rounds": 2,
  "Opening": {
    "OpeningsPath": "C:/Dev/Chess/Openings/openings.pgn",
    "OpeningsPly": 20,
    "OpeningsTwice": true
  },
  "PgnOutPath": "C:/Dev/Chess/Results/test.pgn",
  "EngineSetup": {
    "EngineDefFolder": "C:/Dev/Chess/Engines/EngineDefs",
    "EngineDefList": ["SFDef.json", "Lc0Def.json"]
  },
  "TimeControl": {
    "TimeConfigs": [{
      "Id": 1,
      "Fixed": "00:01:00.000",
      "Increment": "00:00:01.000"
    }]
  }
}
```

### Minimal Puzzle Configuration

```json
{
  "PuzzleFile": "C:/Dev/Chess/Puzzles/lichess_puzzles.csv",
  "Type": "policy, value",
  "MaxRating": 2500,
  "MinRating": 1500,
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    { "Engine": { "ConfigName": "SFDef.json" } }
  ],
  "SampleSize": 500,
  "Concurrency": 1,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Puzzles/Results"
}
```

### Minimal ERET Configuration

```json
{
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    { "Engine": { "ConfigName": "SFDef.json" } }
  ],
  "PuzzleFile": "C:/Dev/Chess/Puzzles/ERET_VESELY203.epd",
  "SampleSize": 50,
  "TimeInSeconds": 5,
  "RunWithNodeLimit": false,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Results"
}
```

---

## Related Documentation

- [TournamentConfig.md](TournamentConfig.md) - Tournament configuration reference
- [EngineDefConfig.md](EngineDefConfig.md) - Engine definition configuration
- [PuzzleConfig.md](PuzzleConfig.md) - Lichess puzzle test configuration
- [EretConfig.md](EretConfig.md) - ERET puzzle test configuration
- [SwissMode.md](SwissMode.md) - Swiss tournament mode details
- [CupMode.md](CupMode.md) - Knockout/Cup tournament mode details
- [LadderMode.md](LadderMode.md) - Ladder tournament mode details
- [Console/ConsoleTuner.md](Console/ConsoleTuner.md) - Bayesian tuner configuration
