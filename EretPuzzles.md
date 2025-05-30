# How to Run ERET Tests in EngineBattle (Console Mode)

EngineBattle supports ERET (Engine Regression and Evaluation Testing) mode, allowing you to benchmark engine versions, networks, or settings across a suite of test positions. The `EretConfig.json` configuration file defines which engines to test, which ERET suite to use, and how the evaluation should be performed.

---

## Prerequisites

- **.NET 9.0 or later** installed ([Download here](https://dotnet.microsoft.com/download))
- **EngineBattle** cloned from [GitHub](https://github.com/lepned/EngineBattle)
- One or more UCI-compatible chess engines (e.g., Stockfish, Lc0, Ceres)
- An ERET test suite file (e.g., EPD or FEN list)
- Engine definition files (see below)

---

## Step 1: Prepare Your EretConfig.json

EngineBattle uses `EretConfig.json` to define the engines, test suite, and evaluation parameters for ERET runs.

Here’s a sample `EretConfig.json`:

```
{
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    {
      "Engine": {
        "ConfigName": "SFDef.json"
      }
    },
    {
      "Engine": {
        "ConfigName": "ObsidianDef.json"
      }
    },
    {
      "EngineWithNets": {
        "ConfigName": "CeresDef.json",
        "ListOfNetsWithPaths": [
          "C:/Dev/Chess/Networks/CeresNet/C1-640-34.onnx",
          "C:/Dev/Chess/Networks/CeresNet/C1-512-25.onnx"
        ]
      }
    }
  ],
  "PuzzleFile": "C:/Dev/Chess/Puzzles/ERET_VESELY203.epd",
  "SampleSize": 50,
  "TimeInSeconds": 5,
  "Nodes": 10000,
  "RunWithNodeLimit": false,
  "FailedPuzzlesOutputFolder":"C:/Dev/Chess/EB"
}
```

**Key fields explained:**

- `EngineFolder`: Path to your engine definition files.
- `Engines`: List of engines to test. Each can be:
  - `Engine`: Uses a single engine config file.
  - `EngineWithNets`: Uses a config file and a list of neural network files.
- `PuzzleFile`: Path to your ERET test suite (EPD or FEN file).
- `SampleSize`: Number of positions to sample from the ERET suite.
- `TimeInSeconds`: Time limit for each engine to evaluate a position.
- `Nodes`: Node limit per position (max nodes each engine will search).
- `RunWithNodeLimit`: true/false to run with node limit or time limit.
- `FailedPuzzlesOutputFolder`: Where to save puzzles that engines fail.
---

## Step 2: Organize Your Engine Definitions

Each engine requires a definition file (e.g., `SFDef.json`) in your `EngineDefs` folder. Example structure:

```
#folder structure example

Engines/
├── EngineDefs/
│   ├── SFDef.json
│   ├── Lc0Def.json
│   └── CeresDef.json
├── LC0/
│   └── Lc0.exe
├── SF/
│   └── SF.exe
└── Ceres/
    └── Ceres.exe
```

## Step 3: Run ERET Mode from Console

Open a terminal and navigate to the `Console` folder inside your EngineBattle project. Then run:

`dotnet run -c release eretjson c:/Dev/Chess/EB/eretconfig.json`

Replace `c:/Dev/Chess/EB/eretconfig.json` with the path to your `EretConfig.json`.

- `-c release` runs the app in Release mode for best performance.
- `eretjson` tells EngineBattle to use ERET mode.
- The last argument is the full path to your `EretConfig.json`.

---

## Step 4: Review Results

Example of console output:

```
ERET Test file name: ERET_VESELY203.epd, number of positions: 25, Time: 2000 ms

Engine            Accuracy    Correct    Failed
---------------------------------------------------
Stockfish 17      80.0%       20         5
Obsidian 15.14    88.0%       22         3
Ceres C1-640-34   80.0%       20         5
Ceres C1-640-25   80.0%       20         5
---------------------------------------------------
```

- **Progress and results** will be shown in the console and summarized as shown above.
- **Failed puzzles** (where engines did not find the correct move) are saved in the folder specified by `FailedPuzzlesOutputFolder`. You can analyze these failed positions to identify engine weaknesses or compare performance across engines.
- To visualize the results, open the EPD file generated during the puzzle run in the EngineBattle GUI. Navigate to the Tools menu, select Test Canvas, and load your output file (EPD-file).
- The visualization displays each failed puzzle on a small chessboard, highlighting both the correct move and the move actually played by the engine using colored arrows. 

---

## Advanced Options

- **Multiple Engines/Networks**: Add more entries to the `Engines` list for broader comparisons.
- **Node Limit**: Adjust `Nodes` for deeper or faster testing.

---

## Summary

EngineBattle’s ERET mode makes it easy to run regression and evaluation tests on chess engines using a flexible JSON configuration. By customizing your `EretConfig.json`, you can benchmark engine improvements, test new networks, and ensure consistent performance across updates.

**Ready to get started?**
Clone EngineBattle, set up your engines and ERET suite, and run your first ERET test today!

---

For more details, see the official [EngineBattle documentation](https://github.com/lepned/EngineBattle) and the included ERET resources.
