# PuzzleConfig.json Configuration

This document provides an overview of the `PuzzleConfig.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for running general puzzle-solving tests with multiple chess engines.

## Configuration Fields

### General Information

- **PuzzleFile**: The file path to the puzzle file (CSV format).
- **Type**: Comma-separated type(s) of puzzles to test. Available types:
  - `policy` — Top-1 policy accuracy + KLD. Tests if the engine's best policy move matches the puzzle solution.
  - `policy2`, `policy3`, `policy5` (or `policytop2`, etc.) — Top-N policy accuracy + KLD. Tests if the correct move is within the top N policy moves. Multiple policy types are merged into a single pass (e.g., `"policy, policy3, policy5"` evaluates each puzzle once).
  - `value` — Value head test (Lc0: ValueOnly + `go nodes 1`, Ceres: `go value`). Tests if the value head's best move matches the puzzle solution.
  - `value2`, `value3`, `value5` (or `valuetop2`, etc.) — Top-N value head accuracy. Evaluates every legal move's child position with `go nodes 1` and checks if the correct move ranks in the top N by value. Multiple value types are merged into a single pass. Slower than policy tests (~30x) due to per-child evaluation. Lc0/Ceres only.
  - `search` — Search accuracy at N nodes (uses the `Nodes` setting).
  - `solve` — Solve from first position, verify full PV (uses the `Nodes` setting).

  **KLD (Kullback-Leibler Divergence)**: Reported for all policy types. Measures `-log(P_correct / 100)` — how much probability the policy assigns to the correct move. Lower = better. KLD is identical across all policy TopN thresholds since it depends on the raw probability, not the threshold.
- **MaxRating**: The maximum puzzle rating to include.
- **MinRating**: The minimum puzzle rating to include.
- **RatingGroups**: Comma-separated rating groups for analysis (e.g., "2500, 2700").
- **PuzzleFilter**: Optional comma-separated filter for puzzles, like i.e. matein3, exposedKing, doubleCheck, deflection and many more.
- **EngineFolder**: The file system path to the folder containing engine definition JSON files.
- **Engines**: List of engines to test. Each engine references a config file in your `EngineDefs` folder.
  - **Engine**: Standard UCI engine configuration.
    - `ConfigName`: Name of the engine definition file (e.g., "SFDef.json")
    - `Nodes`: Number of nodes to search per puzzle position (optional, 0 means this engine will use the global settings only) and can be specified differently for each engine.
  - **EngineWithNets**: Uses an engine definition file with multiple neural networks
    - `ConfigName`: Name of the engine definition file (e.g., "CeresDef.json")
    - `Nodes`: Number of nodes to search per puzzle position (optional, 0 means this engine will use the global settings only) and can be specified differently for each engine.
    - `ListOfNetsWithPaths`: Array of full paths to different neural network files to test. Each path replaces only the network path portion of the engine's `Network` (or `WeightsFile`) option — any backend prefix (e.g., `ONNX_TRT:`) and embedded parameters (e.g., `|cudagraphs=true;V1TEMP=0.55`) from the engine definition are preserved automatically.

### Test Parameters

- **SampleSize**: The number of puzzles to sample from the puzzle file.
- **Nodes**: Global comma-separated node limit per puzzle (eg., "10, 100" will run 10 and 100 nodes search). This applies to all engines in addition to individual engine `Nodes` settings. Default empty.
- **Concurrency**: The number of concurrent engine instances to use for testing.
- **Failed**: Number of failed puzzles to display in results, ordered by rating. Default 0.
- **Solved**: Number of solved puzzles to display in results, ordered by rating. Default 0.
- **FailedPuzzlesOutputFolder**: The folder where failed puzzles and summary will be saved.

## PuzzleConfig.json Example - Copy This as Template

```
{
  "PuzzleFile": "C:/Dev/Chess/Puzzles/lichess_db_April_2025.csv",
  "Type": "policy, policy3, value",
  "MaxRating": 3500,
  "MinRating": 0,
  "RatingGroups": "2500, 2700",
  "PuzzleFilter": "",
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    {
      "Engine": {
        "ConfigName": "SFDef.json"
        "Nodes": 10000
      }
    },
    {
      "Engine": {
        "ConfigName": "Lc0Def.json",
        "Nodes": 100
      }
    },
    {
      "EngineWithNets": {
        "ConfigName": "CeresDef.json",
        "Nodes": 100,
        "ListOfNetsWithPaths": [
          "C:/Dev/Chess/Networks/CeresNet/C1-640-34.onnx",
          "C:/Dev/Chess/Networks/CeresNet/C1-512-25.onnx"
        ]
      }
    }
  ],
  "SampleSize": 1000,
  "Nodes": "",
  "Concurrency" : 1,
  "Failed": 5,
  "Solved": 5,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Puzzles"
}
```
