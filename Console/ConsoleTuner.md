# Console Tuner (SPSA / Bayesian + SPRT / Puzzle / ERET)

Run from the `Console` folder:

```bash
dotnet run -c Release -- tune tuner-config.json
```

The tuner optimizes UCI `setoption` values at a fixed node budget. Two optimizer backends are available:

- **SPSA** (default) — gradient-free stochastic approximation, good for noisy high-dimensional problems.
- **Bayesian** — Gaussian Process surrogate with Expected Improvement acquisition, more sample-efficient for 5-10 parameters.

Select the optimizer via the `"optimizer"` config field (`"spsa"` or `"bayesian"`).

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

The tuner implements **SPSA (Simultaneous Perturbation Stochastic Approximation)** with **pentanomial SPRT** testing for parameter optimization.

### SPSA Algorithm Overview

SPSA is a gradient-free optimization algorithm that estimates gradients through simultaneous perturbation of all parameters:

1. **Perturbation**: At iteration *k*, perturb the current parameter vector **x** by a random direction **Δ** to create two candidates:
   - **x⁺** = **x** + *c_k* **Δ**
   - **x⁻** = **x** - *c_k* **Δ**

2. **Evaluation**: Run an SPRT match between the two candidates to determine which is stronger.

3. **Update**: Move **x** in the winning direction:
   - If **x⁺** wins: **x** ← **x** + *a_k* **Δ**
   - If **x⁻** wins: **x** ← **x** - *a_k* **Δ**
   - If inconclusive: use Elo-sign fallback or no update

4. **Repeat**: Continue for the configured number of iterations per phase.

### Perturbation Details

- **Direction vector Δ**: Each component is drawn from a Rademacher distribution (±1 with equal probability).
- **Perturbation magnitude**: *c_k* = `perturbationSize` (default 0.1) in normalized parameter space.
- **Update step size**: *a_k* is typically smaller than *c_k* to allow convergence.
- **Engine naming**: During SPRT matches, engines are named `EngineName[+]` (for **x⁺**) and `EngineName[-]` (for **x⁻**).

### Parameter Normalization

All parameters are mapped to a **[-1, 1] normalized space** for SPSA operations, then mapped back to their actual ranges:

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
   - **H₁**: Elo difference = `elo1` (alternative hypothesis, e.g., +3 Lo)

3. **Exponential tilting**: The algorithm uses Newton's method to find tilted probability distributions for H₀ and H₁ that match the target Elo means. `tiltDistribution(p, targetMean)` finds the exponential tilt parameter *λ* such that the tilted distribution has the desired mean.

4. **Log-Likelihood Ratio (LLR)**: After observing pentanomial counts (n₀, n₁, n₂, n₃, n₄):
   - LLR = Σᵢ nᵢ × log(p₁[i] / p₀[i])
   - where p₁ is the H₁ tilted distribution and p₀ is the H₀ tilted distribution

5. **Decision boundaries**:
   - If LLR ≥ log((1-β)/α), **reject H₀** (accept **x⁺** or **x⁻** as winner)
   - If LLR ≤ log(β/(1-α)), **reject H₁** (accept the loser as not significantly worse)
   - Otherwise, continue testing (up to `sprt.maxGames`)

6. **Fallback**: If no decision by `maxGames`, use Elo-sign decision based on the match score.

The pentanomial approach is more sample-efficient than simple win-rate SPRT because it uses the full information from game pairs.

## Bayesian Optimizer

When `"optimizer": "bayesian"` is set, the tuner uses **Gaussian Process (GP) regression** with **Expected Improvement (EI)** acquisition instead of SPSA gradient estimation.

### How It Works

1. **Initial design**: Latin Hypercube Sampling (LHS) generates space-filling initial points (default `2 × activeParams`, configurable via `"initialDesignSize"`). Each point is evaluated by running an SPRT match against the **fixed baseline** (initial parameters).

2. **GP surrogate**: A GP with a Squared Exponential ARD kernel is fitted to all observations `(x, scoreFraction)`. Hyperparameters (signal variance, length scales, noise) are optimized via grid search over log marginal likelihood every `"hypUpdateInterval"` iterations (default 5).

3. **Acquisition optimization**: Expected Improvement is maximized via multi-start random sampling (1000 candidates) followed by coordinate-wise refinement to select the next evaluation point.

4. **Evaluation**: The selected candidate is matched against the baseline via SPRT, producing a score fraction.

5. **Repeat**: Steps 2-4 continue for the phase's configured iterations.

### Key Differences from SPSA

| | SPSA | Bayesian |
|---|---|---|
| **History usage** | Discards — each iteration uses only current pair | Cumulative — GP models all past evaluations |
| **Evaluation strategy** | Compare x⁺ vs x⁻ (relative) | Compare candidate vs fixed baseline (absolute) |
| **Best for** | High-dimensional (10+ params), noisy | Low-dimensional (5-10 params), expensive evaluations |
| **Sample efficiency** | Lower — needs many iterations | Higher — informed point selection |

### Bayesian-Specific Config Fields

```json
{
  "optimizer": "bayesian",
  "initialDesignSize": 12,
  "hypUpdateInterval": 5
}
```

- `"optimizer"`: `"bayesian"` or `"bo"` to enable. Default: `"spsa"`.
- `"initialDesignSize"`: Number of LHS points per phase before BO iterations. Default: `2 × activeParams` (minimum 3). Set to 0 for auto.
- `"hypUpdateInterval"`: Refit GP hyperparameters every N iterations. Default: 5.

## Evaluation Modes

### SPRT Mode (default)

The default `"evalMode": "sprt"` evaluates candidates by running head-to-head SPRT matches. This is the standard approach for optimizing playing strength.

### Puzzle Mode

Set `"evalMode": "puzzle"` with `"evalConfigPath"` pointing to a Lichess puzzle config JSON (same format as `puzzlejson` command). The tuner evaluates each candidate by running puzzle accuracy tests instead of SPRT matches.

**How it works with SPSA**: Each iteration evaluates accuracy(x+) and accuracy(x-) independently, then computes `scoreFrac = 0.5 + (acc_plus - acc_minus) / 2.0` to map the accuracy difference to a [0,1] range for gradient estimation.

**How it works with Bayesian**: Each candidate is evaluated for accuracy directly, and the accuracy value is used as the objective function (higher = better).

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

1. **Phase execution**: Each phase runs for a fixed number of SPSA iterations (`"iterations"`), perturbing only the parameters listed in that phase's `"parameters"` array.

2. **Phase confirmation**: After completing a phase's iterations:
   - Run a confirmation match between the **current** parameters (after all iterations) and the **phase start** parameters.
   - If **current** wins, accept the phase; otherwise, reject and revert to phase start.

3. **Best-of tracking**: The tuner maintains an **incumbent best** parameter set:
   - After each successful phase, run a match between **current** and **best**.
   - If **current** wins, it becomes the new **best**.

4. **Engine naming in confirmations**:
   - `[current]`: Parameters after phase iterations
   - `[best]`: Current incumbent best
   - `[tuned]`: Final tuned parameters
   - `[initial]`: Original starting parameters

### Convergence and Output

- **Checkpointing**: State is saved to `tune-state.json` after each iteration, allowing resume with `"resume": true`.
- **History**: Each candidate evaluation is logged to `tune-history.jsonl` with parameters, SPRT results, and LLR traces.
- **Final validation**: After all phases, a validation match compares **tuned** vs **initial** to measure total improvement.
- **Output**: Best parameters written to `best-engine-options.json` in UCI setoption format.

### Time and Resource Limits

- **maxWallHours**: Total wall-clock time limit across all phases.
- **maxCandidates**: Maximum number of SPSA iterations (candidate evaluations) across all phases.
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
  "optimizer": "spsa",
  "perturbationSize": 0.1,
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

## Notes

- Tuning assumes all configured parameters are present as numeric UCI options in `engineConfigPath`.
- All matches run with node-limit time controls set to `targetNodes`.
- If SPRT does not reach a boundary by `sprt.maxGames`, fallback is Elo-sign decision.
- `perturbationSize` (optional, default `0.1`) controls the SPSA `c` constant — the initial perturbation magnitude in normalized space. Larger values create bigger differences between `[+]` and `[-]` candidates, giving SPRT more signal to reach a decision. Smaller values improve convergence precision but may produce more undecided matches.
- `optimizer` (optional, default `"spsa"`) selects the optimization backend. Use `"bayesian"` or `"bo"` for Gaussian Process optimization. SPSA-only fields (`perturbationSize`) are ignored when using Bayesian; Bayesian-only fields (`initialDesignSize`, `hypUpdateInterval`) are ignored when using SPSA.
- The Bayesian optimizer uses no external dependencies beyond MathNet.Numerics (already in ChessLibrary).
- `opponentConfigPath` (optional) — path to a separate engine JSON to use as the baseline opponent during Bayesian optimization main evaluations. When set, BO candidates play against this fixed reference engine instead of the initial parameters of the tuned engine. SPSA iterations are unaffected (they always use self-play). Phase confirmations, best-of comparisons, and final validation use self-play by default but can be switched to use the opponent via `useOpponentForValidation`. Omit or set to `""` to use the default behavior (candidate vs initial self-play).
- `opponentTargetNodes` (optional, default `0`) — node limit for the opponent engine when `opponentConfigPath` is set. When `0`, uses the same `targetNodes` as the candidate engine. When set to a positive value, the opponent searches at `opponentTargetNodes` while the candidate searches at `targetNodes`.
- `preventOpponentDeviation` (optional, default `false`) — when `true` and `opponentConfigPath` is set, constrains the opponent engine to replay its previous moves via a cumulative reference PGN. Only applies to Bayesian optimizer main evaluations; SPSA and self-play matches are unaffected. Forces sequential play (`parallelGames` = 1) when active.
- `maxReferencePgnGames` (optional, default `0`) — maximum number of games stored in the cumulative reference PGN used by `preventOpponentDeviation`. Once the cap is reached, no more games are appended. This prevents the reference PGN from growing indefinitely during long tuning runs, avoiding increasing parse times per iteration. `0` means no cap.
- `useOpponentForValidation` (optional, default `false`) — when `true` and `opponentConfigPath` is set, phase confirmations, best-of comparisons, and final validation run each candidate against the opponent engine instead of self-play. Each comparison runs two matches (candidate A vs opponent, candidate B vs opponent) and compares score fractions. Only applies to SPRT eval mode; puzzle/ERET comparisons are unchanged. When `false` or when no opponent is configured, validation falls back to the default self-play SPRT match.
- `gpus` (optional) — array of GPU indices to assign across parallel games (e.g., `[0, 1]`). Requires `deviceOption` and `deviceTemplate` to be set.
- `deviceOption` (optional) — UCI option name used for GPU device assignment on the **tuned engine** (e.g., `"Device"`). Combined with `deviceTemplate` to set the device per game.
- `deviceTemplate` (optional) — template string for the device value on the **tuned engine** (e.g., `"GPU:{0}#TensorRTNative"`). `{0}` is replaced with the GPU index.
- `opponentDeviceOption` (optional) — UCI option name for GPU device assignment on the **opponent engine**. Only needed when the opponent also requires GPU assignment (e.g., another Ceres instance). Leave empty or omit when the opponent doesn't support a device option (e.g., Stockfish).
- `opponentDeviceTemplate` (optional) — template string for the device value on the **opponent engine**. Same format as `deviceTemplate`. Leave empty or omit when not needed.

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
