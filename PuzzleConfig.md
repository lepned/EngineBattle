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

  **Estimated nodes to find (P95, P99, Max, ≤100)**: Reported for all policy types. Per position, estimates how many nodes a PUCT search (Lc0/Ceres-style) would need before it *first explores* the correct move, from the FPU-reduction first-visit condition `FpuValue * sqrt(Σ P_higher) <= P_correct * CPuct * sqrt(N)` solved for N, where `Σ P_higher` is the policy mass of the moves ranked above the correct move. Uses fixed constants matching modern Lc0 match-play tuning (CPuct 2.897, FpuValue 0.98416) regardless of engine settings, so values are comparable across engines. A rank-1 correct move gives 0 (explored immediately); a correct move missing from the policy output is floored at P = 0.01% (~11.5M nodes). Per puzzle the hardest position of the move sequence counts, and statistics cover ALL puzzles, solved and failed, regardless of `IncludeFailedPuzzles` — solved-at-top-1 puzzles contribute 0 by construction, so the signal lives mostly in the failed ones. Three columns summarize the distribution: **P95** and **P99** are the 95th/99th percentile (nearest-rank) in raw node units — how many nodes the search needs in the worst 5%/1% of positions; they characterize the tail where the policy is bad, but are noisy at small sample sizes (P99 on 500 puzzles is literally the 5th-worst puzzle). **Max** is the single worst puzzle of the set by this estimate (the head of the hardest-by-estimate list, also written to `estNodesHardest_*.csv` worst-first for targeted follow-up). **≤100** is the CDF at a fixed 100-node budget: the percentage of positions where the correct move is first explored (visited at least once) within an estimated 100 nodes; the 100-node budget sits between P95 and P99 for current strong nets — where their distributions actually differ — while staying informative for weak nets too. Note that all these columns estimate the *first visit* only — a necessary but not sufficient condition for solving. Whether the search then keeps investing in the move depends on the value head evaluating the resulting position well; when policy AND value are both wrong (e.g. sacrifices), actual nodes-to-solve can exceed these estimates by orders of magnitude. Comparing ≤100 against the actual solve rate of a `search` test at 100 nodes exposes that value-head gap. This is an order-of-magnitude heuristic — real engines grow CPuct with N and treat FPU at root specially.
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
- **ScoreAllPositions**: `true` also scores every position of a multi-move puzzle, not only
  up to the first mistake. Default `false`. Additive: the per-puzzle numbers are the same
  whether the flag is on or off — this only adds `positionsCorrect`/`positionsScored`/
  `positionAccuracy` to the result JSON, which are `0` when the flag is off so a consumer can
  tell "not measured" from "measured as zero". (Comparability across BUILDS is a separate
  matter — see the stalemate discontinuity noted in `Console/PuzzleJsonSchema.md`.) For the `value` test it also costs engine time, since positions after a
  mistake are queried where they would otherwise be skipped; the policy tests already query
  every position, so there the flag only changes what is reported.

  Note this does NOT fix theme attribution — every position of a puzzle carries the puzzle's
  tags. For per-theme numbers use `firstMoveAccuracy`, which scores the puzzle's first solver
  move (the one its themes describe) and is measured in every run regardless of this flag.

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
  "ScoreAllPositions" : false,
  "Failed": 5,
  "Solved": 5,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Puzzles"
}
```
