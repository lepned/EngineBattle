# Console Tuner (Bayesian Optimizer + SPRT / Puzzle / ERET)

Run from the `Console` folder:

```bash
dotnet run -c Release -- tune tuner-config.json
```

The tuner optimizes UCI `setoption` values at a fixed node budget using a **Bayesian Optimizer** — Gaussian Process surrogate with Expected Improvement acquisition, more sample-efficient for typical parameter counts (5-10 parameters).

Three evaluation modes are available via `"evalMode"`:

- **`"sprt"`** (default) — head-to-head SPRT matches between candidates. Optimizes playing strength.
- **`"puzzle"`** — Lichess puzzle accuracy tests. Optimizes positional evaluation quality.
- **`"eret"`** — EPD position test accuracy. Optimizes tactical/positional evaluation.

SPRT match execution details:
- Uses the in-process tournament runner (`Manager.Runner`) for each comparison.
- Does not spawn `tournamentjson` subprocesses per candidate.
- Writes match PGNs under `outputDir/matches`.
- Computes SPRT decision after each bounded mini-match from final WDL.

## How the Optimizer Works

### Bayesian Optimizer

The tuner uses **Gaussian Process (GP) regression** with **Expected Improvement (EI)** acquisition to select parameter candidates.

1. **Initial design**: Latin Hypercube Sampling (LHS) generates space-filling initial points (default `2 × activeParams`, configurable via `"initialDesignSize"`). Each point is evaluated by running an SPRT match against the **fixed baseline** (initial parameters).

2. **GP surrogate**: A GP with a Matern 5/2 ARD kernel is fitted to all observations `(x, scoreFraction)`. The pipeline applies logit transform then standardization before GP fitting. Hyperparameters (signal variance, length scales, noise) are optimized via grid search over log marginal likelihood every `"hypUpdateInterval"` iterations (default 5). Per-point heteroscedastic noise is computed from game counts via the delta method on logit.

3. **Acquisition optimization**: Expected Improvement is maximized via multi-start random sampling (1000 candidates) followed by coordinate-wise refinement to select the next evaluation point.

4. **Evaluation**: The selected candidate is matched against the baseline via SPRT, producing a score fraction. Game budget scales from 50% to 100% over BO iterations.

5. **Repeat**: Steps 2-4 continue for the phase's configured iterations.

6. **Dashboard**: An HTML dashboard (`bo-dashboard.html`) is generated after each iteration with GP visualizations, convergence plots, and parameter importance.

### Parameter Normalization

All parameters are mapped to a **[-1, 1] normalized space** for optimization, then mapped back to their actual ranges:

- **Linear scale** (`"scale": "linear"`):
  - `toNorm(v)` = 2 × (v - min) / (max - min) - 1
  - `fromNorm(n)` = (n + 1) / 2 × (max - min) + min

- **Log scale** (`"scale": "log"`):
  - `toNorm(v)` = 2 × (log(v) - log(min)) / (log(max) - log(min)) - 1
  - `fromNorm(n)` = exp((n + 1) / 2 × (log(max) - log(min)) + log(min))

After mapping from normalized space, values are **quantized to the parameter's step size** to ensure only valid discrete values are used.

### Pentanomial SPRT

The tuner uses **pentanomial SPRT** (Sequential Probability Ratio Test on pentanomial WDL-pair distributions) to decide matches:

1. **Pentanomial distribution**: Each game pair produces 5 possible outcomes:
   - (WW=2, WD=1, WL/DD=0, LD=-1, LL=-2)

2. **Hypotheses**:
   - **H₀**: Elo difference = `elo0` (null hypothesis, e.g., -3 Elo)
   - **H₁**: Elo difference = `elo1` (alternative hypothesis, e.g., +3 Elo)

3. **Exponential tilting**: The algorithm uses Newton's method to find tilted probability distributions for H₀ and H₁ that match the target Elo means. `tiltDistribution(p, targetMean)` finds the exponential tilt parameter *λ* such that the tilted distribution has the desired mean.

4. **Log-Likelihood Ratio (LLR)**: After observing pentanomial counts (n₀, n₁, n₂, n₃, n₄):
   - LLR = Σᵢ nᵢ × log(p₁[i] / p₀[i])
   - where p₁ is the H₁ tilted distribution and p₀ is the H₀ tilted distribution

5. **Decision boundaries**:
   - If LLR ≥ log((1-β)/α), **reject H₀** (accept candidate as winner)
   - If LLR ≤ log(β/(1-α)), **reject H₁** (accept baseline as winner)
   - Otherwise, continue testing (up to `sprt.maxGames`)

6. **Fallback**: If no decision by `maxGames`, use Elo-sign decision based on the match score.

The pentanomial approach is more sample-efficient than simple win-rate SPRT because it uses the full information from game pairs.

## Evaluation Modes

### SPRT Mode (default)

The default `"evalMode": "sprt"` evaluates candidates by running head-to-head SPRT matches. This is the standard approach for optimizing playing strength.

### Puzzle Mode

Set `"evalMode": "puzzle"` with `"evalConfigPath"` pointing to a Lichess puzzle config JSON (same format as `puzzlejson` command). The tuner evaluates each candidate by running puzzle accuracy tests instead of SPRT matches.

Each candidate is evaluated for accuracy directly, and the accuracy value is used as the objective function (higher = better).

**Comparisons** (phase confirmation, best-of, final validation): Both sides are evaluated for accuracy; the higher accuracy wins.

Example config:
```json
{
  "evalMode": "puzzle",
  "evalConfigPath": "C:/Dev/Chess/EB/puzzle-config.json",
  "engineConfigPath": "...",
  "targetNodes": 250,
  ...
}
```

The puzzle config file should specify `puzzleFile`, `sampleSize`, `concurrency`, etc. The engine definitions inside the puzzle config are ignored — the tuner uses `engineConfigPath` and applies tuned parameters.

### ERET Mode

Set `"evalMode": "eret"` with `"evalConfigPath"` pointing to an ERET config JSON (same format as `eretjson` command). The tuner evaluates each candidate by running EPD position accuracy tests.

Example config:
```json
{
  "evalMode": "eret",
  "evalConfigPath": "C:/Dev/Chess/EB/eret-config.json",
  "engineConfigPath": "...",
  "targetNodes": 1000,
  ...
}
```

### Eval Mode Config Fields

```json
{
  "evalMode": "puzzle",
  "evalConfigPath": "C:/Dev/Chess/EB/puzzle-config.json"
}
```

- `"evalMode"`: `"sprt"` (default), `"puzzle"`, or `"eret"`.
- `"evalConfigPath"`: Path to the puzzle or ERET config JSON file. Required when evalMode is `"puzzle"` or `"eret"`.

The `"sprt"` block is still required even in puzzle/eret modes — `maxGames` is used as the iteration budget reference.

### Phase Structure

Tuning is organized into **phases**, each focusing on a subset of parameters:

1. **Phase execution**: Each phase runs for a fixed number of BO iterations (`"iterations"`), optimizing only the parameters listed in that phase's `"parameters"` array.

2. **Phase confirmation**: After completing a phase's iterations:
   - Run a confirmation match between the **current best** parameters and the **phase start** parameters.
   - If **current** wins, accept the phase; otherwise, reject and revert to phase start.

3. **Engine naming in confirmations**:
   - `[bo-N]`: BO candidate #N during optimization
   - `[current]`: Parameters after phase iterations
   - `[phase-start]`: Parameters at the start of the phase
   - `[tuned]`: Final tuned parameters
   - `[initial]`: Original starting parameters

### Convergence and Output

- **Checkpointing**: State is saved to `tune-state.json` after each iteration, allowing resume with `"resume": true`.
- **History**: Each candidate evaluation is logged to `tune-history.jsonl` with parameters, SPRT results, GP predictions, and LLR traces.
- **Dashboard**: An HTML dashboard (`bo-dashboard.html`) with GP visualizations, convergence plots, and parameter importance is updated after each iteration.
- **Final validation**: After all phases, a validation match compares **tuned** vs **initial** to measure total improvement.
- **Output**: Best parameters written to `best-engine-options.json` in UCI setoption format.

### Time and Resource Limits

- **maxWallHours**: Total wall-clock time limit across all phases.
- **maxCandidates**: Maximum number of candidate evaluations across all phases.
- **parallelGames**: Number of concurrent games per match (typically 1 for deterministic engines).

## Config Schema

```json
{
  "engineConfigPath": "C:/Dev/Chess/Engines/EngineDefs/Ceres 640_34_TRT_Base.json",
  "opponentConfigPath": "",
  "opponentTargetNodes": 0,
  "baseTournamentConfigPath": "C:/Dev/Chess/EB/tournament.json",
  "outputDir": "logs/tuner",
  "targetNodes": 250,
  "parallelGames": 1,
  "openingsPath": "C:/Dev/Chess/Openings/TCECbook_90_120.pgn",
  "openingsPly": 100,
  "openingsTwice": true,
  "seed": 12345,
  "resume": false,
  "maxWallHours": 24,
  "maxCandidates": 1000,
  "initialDesignSize": 0,
  "hypUpdateInterval": 5,
  "preventOpponentDeviation": false,
  "maxReferencePgnGames": 0,
  "useOpponentForValidation": false,
  "gpus": [0, 1],
  "deviceOption": "Device",
  "deviceTemplate": "GPU:{0}#TensorRTNative",
  "opponentDeviceOption": "",
  "opponentDeviceTemplate": "",
  "evalMode": "sprt",
  "evalConfigPath": "",
  "sprt": {
    "elo0": -3.0,
    "elo1": 3.0,
    "alpha": 0.05,
    "beta": 0.05,
    "minGames": 2,
    "maxGames": 8
  },
  "phases": [
    { "name": "core", "parameters": ["PolicyTemperature","CPUCT", "CPUCTAtRoot", "CPUCTFactor", "FPU", "V1TEMP"], "iterations": 120 },
    { "name": "joint_refine", "parameters": ["CPUCT", "CPUCTAtRoot", "CPUCTFactor", "PolicyTemperature", "FPU"], "iterations": 200 }
  ],
 "parameters": [
    { "name": "CPUCT", "min": 0.1, "max": 10.0, "step": 0.05, "scale": "log" },
    { "name": "CPUCTAtRoot", "min": 0.1, "max": 10.0, "step": 0.05, "scale": "log" },
    { "name": "CPUCTFactor", "min": 0.1, "max": 10.0, "step": 0.05, "scale": "log" },
    { "name": "PolicyTemperature", "min": 0.1, "max": 2.2, "step": 0.05, "scale": "linear" },
    { "name": "FPU", "min": 0.0, "max": 1.0, "step": 0.05, "scale": "linear" },
    { "name": "V1TEMP", "option": "Network", "min": 0.05, "max": 1.0, "step": 0.01, "scale": "linear" }
  ]
  }
```

## Output Files

- `tune-state.json`: checkpoint state for resume.
- `tune-history.jsonl`: append-only per-candidate history.
- `best-engine-options.json`: final tuned option key/value set.
- `tune-summary.txt`: final validation summary.
- `bo-dashboard.html`: interactive HTML dashboard with GP visualizations.

## Notes

- Tuning assumes all configured parameters are present as numeric UCI options in `engineConfigPath`.
- All matches run with node-limit time controls set to `targetNodes`.
- If SPRT does not reach a boundary by `sprt.maxGames`, fallback is Elo-sign decision.
- `initialDesignSize` (optional, default `2 × activeParams`) — number of Latin Hypercube Sampling points per phase before BO iterations begin. Set to 0 for auto.
- `hypUpdateInterval` (optional, default `5`) — refit GP hyperparameters every N iterations.
- The Bayesian optimizer uses no external dependencies beyond MathNet.Numerics (already in ChessLibrary).
- `opponentConfigPath` (optional) — path to a separate engine JSON to use as the baseline opponent during optimization. When set, BO candidates play against this fixed reference engine instead of the initial parameters of the tuned engine. Phase confirmations and final validation use self-play by default but can be switched to use the opponent via `useOpponentForValidation`. Omit or set to `""` to use the default behavior (candidate vs initial self-play).
- `opponentTargetNodes` (optional, default `0`) — node limit for the opponent engine when `opponentConfigPath` is set. When `0`, uses the same `targetNodes` as the candidate engine. When set to a positive value, the opponent searches at `opponentTargetNodes` while the candidate searches at `targetNodes`.
- `preventOpponentDeviation` (optional, default `false`) — when `true` and `opponentConfigPath` is set, constrains the opponent engine to replay its previous moves via a cumulative reference PGN. Forces sequential play (`parallelGames` = 1) when active.
- `maxReferencePgnGames` (optional, default `0`) — maximum number of games stored in the cumulative reference PGN used by `preventOpponentDeviation`. Once the cap is reached, no more games are appended. This prevents the reference PGN from growing indefinitely during long tuning runs, avoiding increasing parse times per iteration. `0` means no cap.
- `useOpponentForValidation` (optional, default `false`) — when `true` and `opponentConfigPath` is set, phase confirmations and final validation run each candidate against the opponent engine instead of self-play. Each comparison runs two matches (candidate A vs opponent, candidate B vs opponent) and compares score fractions. Only applies to SPRT eval mode; puzzle/ERET comparisons are unchanged. When `false` or when no opponent is configured, validation falls back to the default self-play SPRT match.
- `gpus` (optional) — array of GPU indices to assign across parallel games (e.g., `[0, 1]`). Requires `deviceOption` and `deviceTemplate` to be set.
- `deviceOption` (optional) — UCI option name used for GPU device assignment on the **tuned engine** (e.g., `"Device"`). Combined with `deviceTemplate` to set the device per game.
- `deviceTemplate` (optional) — template string for the device value on the **tuned engine** (e.g., `"GPU:{0}#TensorRTNative"`). `{0}` is replaced with the GPU index.
- `opponentDeviceOption` (optional) — UCI option name for GPU device assignment on the **opponent engine**. Only needed when the opponent also requires GPU assignment (e.g., another Ceres instance). Leave empty or omit when the opponent doesn't support a device option (e.g., Stockfish).
- `opponentDeviceTemplate` (optional) — template string for the device value on the **opponent engine**. Same format as `deviceTemplate`. Leave empty or omit when not needed.
- Old config files with `"optimizer"` or `"perturbationSize"` fields are silently ignored — the Bayesian optimizer always runs.

## Embedded Parameters (pipe-delimited option sub-values)

Some engine options contain embedded `KEY=value` parameters after a `|` delimiter. For example, a `Network` option might look like:

```
"Network": "C:/Dev/Chess/Networks/CeresNet/C1-640-34-I8.onnx|V1TEMP=0.55|SMOLGEN=1"
```

To tune an embedded sub-value, set `option` to the UCI option name that contains it. The `name` field then identifies the `KEY=value` within the pipe-delimited string:

```json
{
  "parameters": [
    { "name": "V1TEMP", "option": "Network", "min": 0.05, "max": 1.0, "step": 0.01, "scale": "linear" },
    { "name": "SMOLGEN", "option": "Network", "min": 0, "max": 2, "step": 1, "scale": "linear" }
  ]
}
```

When `option` is omitted (or null), the parameter behaves as before — `name` is the UCI option key and the entire option value is replaced.
