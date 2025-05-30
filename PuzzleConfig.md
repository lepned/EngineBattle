# PuzzleConfig.json Configuration

This document provides an overview of the `PuzzleConfig.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for running general puzzle-solving tests with multiple chess engines.

## Configuration Fields

### General Information

- **PuzzleFile**: The file path to the puzzle file (CSV format).
- **Type**: Comma-separated type(s) of puzzles to test (e.g., "policy, value, search").
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
    - `ListOfNetsWithPaths`: Array of full paths to different neural network files to test

### Test Parameters

- **SampleSize**: The number of puzzles to sample from the puzzle file.
- **Nodes**: Global comma-separated node limit per puzzle (eg., "10, 100" will run 10 and 100 nodes search). This applies to all engines in addition to individual engine `Nodes` settings. Default empty.
- **Concurrency**: The number of concurrent engine instances to use for testing.
- **FailedPuzzlesOutputFolder**: The folder where failed puzzles and summary will be saved.

## PuzzleConfig.json Example - Copy This as Template

```
{
  "PuzzleFile": "C:/Dev/Chess/Puzzles/lichess_db_April_2025.csv",
  "Type": "policy, search",
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
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Puzzles"
}
```
