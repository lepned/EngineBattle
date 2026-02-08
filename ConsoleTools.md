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

**Configuration:** See [Tournament.json.md](Tournament.json.md) for full configuration reference.

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
- `/EngineDef` - Engine definition overview
- `/tournamentSetup` - Tournament setup
- `/play-vs-computer` - Play against engine
- `/LichessPuzzles` - Lichess puzzle tests
- `/EretPuzzleTest` - ERET puzzle tests
- `/help` - Help and documentation
- `/speed` - Speed calculator
- `/ordo` - Ordo rating results
- `/deviationFinder` - Move deviation finder

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

### help

Displays available commands and their usage.

**Syntax:**
```bash
dotnet run -c Release -- help
```

**Output:**
```
Help: Available commands are:
  - perft <depth> <sampleSize>
  - puzzlejson <path>
  - eretjson <path>
  - tournamentjson <configFile>
  - gui [page] [port]  - Launch WebGUI (default: tournament page, port 5018)
    Examples: gui, gui analysis/single, gui 5020, gui help 5020
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

- [Tournament.json.md](Tournament.json.md) - Tournament configuration reference
- [EngineDef.json.md](EngineDef.json.md) - Engine definition configuration
- [PuzzleConfig.md](PuzzleConfig.md) - Lichess puzzle test configuration
- [EretConfig.md](EretConfig.md) - ERET puzzle test configuration
- [SwissMode.md](SwissMode.md) - Swiss tournament mode details
- [CupMode.md](CupMode.md) - Knockout/Cup tournament mode details
