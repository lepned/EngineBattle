# How to Run Lichess Puzzles in EngineBattle (Console Mode)

EngineBattle is a powerful tool for chess engine developers and enthusiasts, offering advanced features for engine testing, tournaments, and puzzle-based benchmarking. One of its standout capabilities is the ability to run large sets of Lichess puzzles against multiple engines or networks, providing deep insights into engine performance on tactical and strategic positions.

> **Note:**  
> Only neural network-based engines such as **Ceres** and **Lc0** support value head and policy head tests (i.e., `"value"` and `"policy"` types in your configuration).  
> All other UCI engines (such as Stockfish) can participate in `"search"` tests, but do not provide value or policy head outputs.

In this post, you’ll learn how to set up and run Lichess puzzle tests in EngineBattle using the console mode.

---

## Prerequisites

- **.NET 9.0 or later** installed ([Download here](https://dotnet.microsoft.com/download))
- **EngineBattle** cloned from [GitHub](https://github.com/lepned/EngineBattle)
- One or more UCI-compatible chess engines (e.g., Stockfish, Lc0, Ceres)
- A Lichess puzzle file (CSV format). You can download the latest Lichess puzzle database here: [Download Lichess Puzzle CSV](https://database.lichess.org/lichess_db_puzzle.csv.zst).

---

## Step 1: Prepare Your PuzzleConfig.json

EngineBattle uses a configuration file called `PuzzleConfig.json` to define which engines to test, which puzzles to use, and how the test should be run.

Here’s a sample `PuzzleConfig.json` for Lichess puzzles:

```
{
  "PuzzleFile": "C:/Dev/Chess/Puzzles/lichess_db_April_2025.csv",
  "Type": "policy, value, search",
  "MaxRating": 3500,
  "MinRating": 0,
  "RatingGroups": "2500, 2700",
  "PuzzleFilter": "",
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    {
      "Engine": {
        "ConfigName": "SFDef.json",
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

**Key fields explained:**

- `PuzzleFile`: Path to your Lichess puzzle CSV file.
- `Type`: Types of tests (e.g., `"policy"`, `"value"`, `"search"`).
- **Engines**: List of engines to test. Each engine references a config file in your `EngineDefs` folder.
  - **Engine**: Standard UCI engine configuration.
    - `ConfigName`: Name of the engine definition file (e.g., "SFDef.json")
    - `Nodes`: Number of nodes to search per puzzle position (optional, 0 means this engine will use the global settings only) and can be specified differently for each engine.
  - **EngineWithNets**: Neural network engine with multiple networks
    - `ConfigName`: Name of the engine definition file (e.g., "CeresDef.json")
    - `Nodes`: Number of nodes to search per puzzle position (can be specified differently for each engine)
    - `ListOfNetsWithPaths`: Array of full paths to different neural network files to test

- `SampleSize`: Number of puzzles to sample from the file.
- `Nodes`: Global comma-separated node limit per puzzle (eg., "10, 100" will run 10 and 100 nodes search). This applies to all engines in addition to individual engine `Nodes` settings. Default empty.
- `Concurrency`: Number of engine instances to run in parallel. More than 1 is usually not recommended unless you have a powerful computer.
- `FailedPuzzlesOutputFolder`: The folder where failed puzzles and summary will be saved.

For more details, see the [PuzzleConfig.md](PuzzleConfig.md) documentation.

---

## Step 2: Organize Your Engine Definitions

Each engine you want to test needs a definition file (e.g., `SFDef.json` for Stockfish) in your `EngineDefs` folder. Example structure:
---

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

## Step 3: Run the Puzzle Test from Console

Open a terminal and navigate to the `Console` folder inside your EngineBattle project. Then run:

`dotnet run -c release puzzlejson c:/Dev/Chess/EB/puzzleconfig.json `

Replace `c:/Dev/Chess/EB/puzzleconfig.json` with the path to your `PuzzleConfig.json`.

- `-c release` runs the app in Release mode for best performance.
- `puzzlejson` tells EngineBattle to use puzzle testing mode.
- The last argument is the full path to your `PuzzleConfig.json`.

---

## Step 4: Review Results

Example of console output:

```
Puzzle file: C:/Dev/Chess/Puzzles/lichess_db_April_2025.csv

Policy Head Tests

Engine          Neural net      Perf    Accuracy        Total   Avg rating      Theme   Nodes
Ceres 640-34    C1-640-34       2771    60.1%           1000    2700            none    1
LC0 BT4-332.pb  BT4-332.pb      2751    57.3%           1000    2700            none    1

Value Head Tests

Engine          Neural net      Perf    Accuracy        Total   Avg rating      Theme   Nodes
Ceres 640-34    C1-640-34       2909    76.9%           1000    2700            none    1
LC0 BT4-332.pb  BT4-332.pb      2895    75.5%           1000    2700            none    1

```

- **Progress and results** will be shown in the console and summarized as shown above.
- **Failed puzzles** (where engines did not find the correct move) are saved in the folder specified by `FailedPuzzlesOutputFolder`.
- You can analyze these failed positions to identify engine weaknesses or compare performance across engines.
- To visualize the results, open the EPD file generated during the puzzle run in the EngineBattle GUI. Navigate to the Tools menu, select Test Canvas, and load your output file (EPD-file).
- The visualization displays each failed puzzle on a small chessboard, highlighting both the correct move and the move actually played by the engine using colored arrows. 
  Additionally, the policy values for both the correct move and the move played are shown, allowing you to see not only what the engine chose, but also how strongly it preferred each option. This makes it easy to spot tactical oversights, policy head weaknesses, or cases where the engine was uncertain between candidate moves.

---

## Advanced Options

- **Filter puzzles** by type, rating, or motif using `Type`, `RatingGroups` and `PuzzleFilter`.
  You can find a list of available puzzle themes (motifs) to use in the `PuzzleFilter` field here: [Lichess Puzzle Themes](https://github.com/ornicar/lila/blob/master/translation/source/puzzleTheme.xml)
- **Test multiple engines or networks** by adding more entries to the `Engines` list.
- **Increase concurrency** for faster testing if your hardware allows.

---

## Summary

EngineBattle’s console mode makes it easy to benchmark chess engines on thousands of Lichess puzzles with just a single command. By customizing your `PuzzleConfig.json`, you can tailor tests to your needs—whether you’re evaluating tactical strength, policy head accuracy, or overall puzzle-solving ability. 
> **Note:** Puzzles can also be run in the GUI mode (in a very similar way), but this is not covered in this post.

**Ready to get started?**  
Clone EngineBattle, set up your engines and puzzles, and run your first test today! 

---

For more details, see the official [EngineBattle documentation](https://github.com/lepned/EngineBattle) and the included `PuzzleConfig.md` file.
