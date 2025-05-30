# EretConfig.json Configuration

This document provides an overview of the `EretConfig.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for running Engine Rapid Evaluation Tests (ERET) with multiple chess engines and a set of puzzles.

## Configuration Fields

### General Information

- **EngineFolder**: The file system path to the folder containing engine definition JSON files.
- **Engines**: A list of engines to use for the test. Each entry can be:
  - **Engine**: Uses a single engine definition file (`ConfigName`).
  - **EngineWithNets**: Uses an engine definition file with a list of neural network files (`ListOfNetsWithPaths`).
- **PuzzleFile**: The file path to the EPD or puzzle file to be used for testing.

### Test Parameters

- **SampleSize**: The number of puzzles to sample from the puzzle file.
- **TimeInSeconds**: The time limit per puzzle (used if `RunWithNodeLimit` is `false`).
- **Nodes**: The node limit per puzzle (used if `RunWithNodeLimit` is `true`).
- **RunWithNodeLimit**: If `true`, engines are limited by node count; if `false`, by time.
- **FailedPuzzlesOutputFolder**: Where to save puzzles that engines fail.

## EretConfig.json Example - Copy This as Template

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
  "SampleSize": 10,
  "TimeInSeconds": 5,
  "Nodes": 10000,
  "RunWithNodeLimit": false,
  "FailedPuzzlesOutputFolder":"C:/Dev/Chess/EB"
}
```
