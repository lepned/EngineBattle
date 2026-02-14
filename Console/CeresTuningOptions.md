# Ceres UCI Options for Tuning

Reference for all Ceres UCI options relevant to parameter tuning with the Bayesian optimizer.

## PUCT Exploration Parameters

The PUCT formula controls the exploration/exploitation tradeoff in MCTS:

```
U(s,a) = Q(s,a) + C(s) * P(s,a) * sqrt(N_parent) / (1 + N_child)
```

where `C(s) = CPUCT + CPUCTFactor * log((N_parent + CPUCTBase + 1) / CPUCTBase)`

| Option | Default | Scale | Description |
|--------|---------|-------|-------------|
| **CPUCT** | 1.745 | log | Base exploration constant. Higher = more exploration (follow policy less, try alternatives more). At low node counts the search tree is shallow, so this dominates behavior. |
| **CPUCTAtRoot** | 1.745 | log | Same as CPUCT but only at the root node. At very low node counts (e.g. 500), root is where most of the decision happens. Often tuned separately from CPUCT since root needs different exploration than deeper nodes. |
| **CPUCTFactor** | 3.894 | log | Scales the logarithmic growth of exploration with visit count. As a node gets more visits, exploration increases by `CPUCTFactor * log(...)`. Higher = more exploration in well-visited subtrees. |
| **CPUCTFactorAtRoot** | 3.894 | log | Same as CPUCTFactor but only at root. |
| **CPUCTBase** | 38739 | log | Controls when logarithmic scaling kicks in. Larger values delay the onset (exploration stays closer to base CPUCT for longer). The log term is `log((N + CPUCTBase + 1) / CPUCTBase)`, so at `N << CPUCTBase` the log term is ~0 and CPUCT alone drives exploration. |
| **CPUCTBaseAtRoot** | 38739 | log | Same as CPUCTBase but only at root. |

**Tuning notes:**
- At low node counts (< 1000), the tree is shallow and root parameters (`*AtRoot`) matter more than tree parameters.
- CPUCT and CPUCTFactor interact: CPUCT sets the floor, CPUCTFactor controls growth. Tune together or in separate phases.
- CPUCTBase is less sensitive at low node counts because `N << CPUCTBase` makes the log term negligible. More impactful at high node counts.
- Suggested ranges: CPUCT/CPUCTAtRoot [0.5, 4.0], CPUCTFactor [1.0, 8.0], CPUCTBase [5000, 80000].

## Policy and Value Temperature

| Option | Default | Scale | Description |
|--------|---------|-------|-------------|
| **PolicyTemperature** | 1.359 | log | Applied to raw policy logits before softmax. `T > 1` flattens the distribution (considers more moves), `T < 1` sharpens it (focuses on top moves). Interacts with CPUCT: both affect how much the search follows the policy prior vs exploring alternatives. |
| **ValueTemperature** | 1.0 | log | Scales the value head output. `T > 1` compresses Q values toward 0.5 (less decisive evaluations), `T < 1` makes them more extreme. Multiplier centered at 1.0, so log scale keeps 0.5 and 2.0 equidistant from neutral. |

**Tuning notes:**
- PolicyTemperature > 1 compensates for overconfident policies. With large nets that have strong policy heads, values above 1.0 are common.
- ValueTemperature is often left at 1.0 but can help if the value head is miscalibrated. Suggested range [0.5, 2.0] with linear scale.

## First Play Urgency (FPU)

| Option | Default | Scale | Description |
|--------|---------|-------|-------------|
| **FPU** | 0.33 | linear | Reduction applied to unvisited children: `Q_unvisited = Q_parent - FPU`. Higher values make the engine more reluctant to try unexplored moves (pessimistic about unknowns). Lower values encourage broader exploration. |
| **FPUAtRoot** | 1.0 | linear | Same as FPU but at root. Default of 1.0 means unvisited root moves get `Q_parent - 1.0`, which is very pessimistic (strongly favors the policy prior at root). |

**Tuning notes:**
- FPU is additive (a reduction), so linear scale is appropriate.
- FPU and FPUAtRoot serve different purposes: tree FPU affects depth-first vs breadth-first search character, while root FPU affects how quickly the engine considers moves outside the top policy choice.
- Suggested ranges: FPU [0.1, 0.7], FPUAtRoot [0.3, 1.5].
- FPUAtRoot = 1.0 is very aggressive; lower values may help at low node counts where the policy prior is more reliable.

## Search Pruning and Limits

| Option | Default | Scale | Description |
|--------|---------|-------|-------------|
| **SmartPruningFactor** | 1.33 | log | Controls futility pruning of moves that can't catch the best move given remaining search budget. Higher = more aggressive pruning (faster but may miss moves). `0` disables. Multiplier centered near 1.0, so log scale is appropriate. At low node counts, aggressive pruning risks cutting good moves before they're properly evaluated. |
| **SearchLimitMultiplier** | 1.00 | linear | Multiplier on the search limit (nodes/time). Values > 1 give the engine more search, < 1 give less. Useful for testing strength at different compute levels without changing the external limit. |
| **MaxTreeVisits** | (none) | - | Hard cap on total tree visits. |
| **MaxTreeNodes** | (none) | - | Hard cap on tree node count. |

## Search Algorithm

| Option | Default | Description |
|--------|---------|-------------|
| **MCGS** | true | Monte Carlo Graph Search (transposition-aware). When true, the search recognizes transpositions and shares evaluations. When false, uses standard MCTS tree search. MCGS is generally stronger but has different characteristics. |
| **PathMode** | Position | `Position` = identify nodes by board position only. `PositionAndHistory` = include move history (relevant for draw by repetition detection and history-dependent nets). |
| **EnableSiblingEval** | false | When true, evaluates sibling nodes during backup. Can improve search efficiency by getting more information per node evaluation. |
| **EnableUncertaintyBoosting** | false | Adds exploration bonus to moves with high value uncertainty. Encourages the search to resolve uncertain positions. Interacts with CPUCT parameters. |

**Tuning notes:**
- MCGS, EnableSiblingEval, and EnableUncertaintyBoosting are boolean options. In the tuner, use `"min": 0, "max": 1, "step": 1` to tune them as on/off switches.
- EnableUncertaintyBoosting changes the exploration dynamics significantly; if enabled, CPUCT values may need retuning.

## Network Configuration

| Option | Default | Description |
|--------|---------|-------------|
| **Network** | (none) | Network file path. Supports embedded parameters via `\|` separator: `path.onnx\|key=value;key=value`. Known embedded keys include **V1TEMP** (value head temperature applied at the network level, before search-level ValueTemperature). |
| **WeightsFile** | (none) | Alternative network specification. |

**Tuning notes:**
- **V1TEMP** (embedded in Network option) is a common tuning target. It adjusts value head calibration at the network level. In the tuner config, specify it as an embedded parameter:
  ```json
  { "name": "V1TEMP", "optionKey": "Network", "embeddedKey": "V1TEMP", "min": 0.3, "max": 1.5, "step": 0.01, "scale": "linear" }
  ```
- V1TEMP and ValueTemperature both affect value scaling but at different stages. Don't tune both simultaneously without understanding the interaction.

## Non-Tuning Options (Reference)

These options are set in the engine config but are not meaningful tuning targets:

| Option | Default | Purpose |
|--------|---------|---------|
| Device | (none) | GPU device assignment (e.g. `GPU:0#TensorRTNative`) |
| SyzygyPath | (none) | Tablebase path for endgame adjudication |
| LogFile | (none) | Debug log output path |
| SearchLogFile | (none) | Search-specific log path |
| MultiPV | 1 | Number of principal variations to report |
| VerboseMoveStats | false | Detailed per-move statistics in output |
| LogLiveStats | false | Live search statistics (enables chart display in WebGUI) |
| UCI_Chess960 | false | Fischer Random Chess support |
| PerPVCounters | false | Per-PV node counters |
| ScoreType | centipawn | Score display format (centipawn / Q / W-L) |
| UCI_ShowWDL | false | Show Win/Draw/Loss probabilities |
| MoveOverheadMs | 250 | Time reserved for communication overhead |
| RamLimitMb | 0 | Memory limit (0 = unlimited) |
| ReducedMemoryMode | false | Lower memory footprint at cost of speed |
| CUDAGraphSizes | [14,28,44,64] | Batch sizes for CUDA graph optimization |
| LimitsManagerName | (none) | Custom time management strategy |

## Example Tuner Parameter Blocks

### Minimal (3 params, quick tuning)
```json
"parameters": [
  { "name": "CPUCT", "min": 0.5, "max": 4.0, "step": 0.01, "scale": "log" },
  { "name": "PolicyTemperature", "min": 0.4, "max": 2.2, "step": 0.01, "scale": "log" },
  { "name": "FPU", "min": 0.1, "max": 0.7, "step": 0.01, "scale": "linear" }
]
```

### Comprehensive (root + tree, multi-phase)
```json
"phases": [
  { "name": "root", "parameters": ["CPUCTAtRoot", "CPUCTFactorAtRoot", "FPUAtRoot"], "iterations": 80 },
  { "name": "tree", "parameters": ["CPUCT", "CPUCTFactor", "FPU"], "iterations": 80 },
  { "name": "policy", "parameters": ["PolicyTemperature", "SmartPruningFactor"], "iterations": 60 }
],
"parameters": [
  { "name": "CPUCTAtRoot", "min": 0.5, "max": 4.0, "step": 0.01, "scale": "log" },
  { "name": "CPUCTFactorAtRoot", "min": 1.0, "max": 8.0, "step": 0.01, "scale": "log" },
  { "name": "FPUAtRoot", "min": 0.3, "max": 1.5, "step": 0.01, "scale": "linear" },
  { "name": "CPUCT", "min": 0.5, "max": 4.0, "step": 0.01, "scale": "log" },
  { "name": "CPUCTFactor", "min": 1.0, "max": 8.0, "step": 0.01, "scale": "log" },
  { "name": "FPU", "min": 0.1, "max": 0.7, "step": 0.01, "scale": "linear" },
  { "name": "PolicyTemperature", "min": 0.4, "max": 2.2, "step": 0.01, "scale": "log" },
  { "name": "SmartPruningFactor", "min": 0.5, "max": 3.0, "step": 0.01, "scale": "log" }
]
```

### With Embedded Network Parameter
```json
"parameters": [
  { "name": "V1TEMP", "optionKey": "Network", "embeddedKey": "V1TEMP", "min": 0.3, "max": 1.2, "step": 0.01, "scale": "linear" },
  { "name": "PolicyTemperature", "min": 0.4, "max": 2.2, "step": 0.01, "scale": "log" }
]
```
