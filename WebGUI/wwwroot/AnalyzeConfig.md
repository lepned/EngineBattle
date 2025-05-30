# AnalyzeConfig.json Configuration

This document provides an overview of the `AnalyzeConfig.json` configuration file used in the EngineBattle application. This file defines the engines, settings, and parameters to use in single or dual analysis mode.


## Analysis Modes
- **Single Analysis**: Uses only the first engine listed in the configuration for analysis
- **Dual Analysis**: Uses the first two engines simultaneously, enabling comparative analysis of their outputs

This flexibility makes the configuration suitable for a variety of analysis scenarios, from simple evaluations to in-depth engine comparisons.

## File Naming Convention

**Important**: For automatic loading and persistence, name your configuration file exactly `AnalyzeConfig.json`. This eliminate the need to manually reload the configuration file through the GUI each time you start the application. If you make changes to the config you need to reload the file in the GUI for the changes to take effect. 

## Configuration Fields

### General Information

- **EngineFolder**: The file system path to the folder containing engine definition JSON files.
- **Engines**: List of engines to use in the test. Each engine references a config file in your `EngineDefs` folder.
   - **Engine**: Standard UCI engine configuration.
    - `ConfigName`: Name of the engine definition file (e.g., "SFDef.json")
    - `Nodes`: The number of nodes each engine will search when analyzing a position. The setting can be adjusted dynamically through the GUI interface.
  - **EngineWithNets**: Uses an engine definition file with multiple neural networks
    - `ConfigName`: Name of the engine definition file (e.g., "CeresDef.json")
    - `Nodes`: The number of nodes each engine will search when analyzing a position. The setting can be adjusted dynamically through the GUI interface.
    - `ListOfNetsWithPaths`: Array of full paths to different neural network files to test

### Analysis Parameters

- **ChartLines**: The number of lines to display in the analysis chart, maximum is 10. This determines how many variations or moves are shown in the search chart.


## Usage and modes

- **DualAnalysis**: Enables the use of two engines simultaneously to analyze a position. This feature is particularly beneficial for testers and developers who want to compare engine features against a baseline.
- **SingleAnalysis**: Perform analysis using a single engine. This mode is ideal for general analysis tasks or scenarios where only one engine is required.
- **ConsoleOutput**: Provides detailed real-time UCI output during the analysis. This feature is useful for debugging and monitoring engine behavior.

## AnalyzeConfig.json Example - Copy This as Template

```
{
    "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
    "Engines": [
        {
            "Engine": {
                "ConfigName": "SFDef.json",
                "Nodes": 1000000
            }
        },        
        {
            "EngineWithNets": {
                "ConfigName": "CeresDef.json",
                "Nodes": 1000,
                "ListOfNetsWithPaths": [
                    "C:/Dev/Chess/Networks/CeresNet/C1-640-34.onnx"
                ]
            }
        }
    ],
    "ChartLines": 8
}
```

---

