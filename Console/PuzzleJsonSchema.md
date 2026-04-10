# `puzzlejson --json` output schema

This document describes the JSON file written by EngineBattle's `puzzlejson` CLI command when invoked with the `--json <path>` flag. The schema is the **stable public contract** for external tooling that consumes EngineBattle puzzle results without parsing stdout (e.g. the Python SPSA tuner).

## Invocation

```bash
dotnet run --project Console -c Release -- puzzlejson <config.json> --json <output.json>
```

The `--json` flag is **optional** and **does not change** the existing stdout/file output. If omitted, behavior is byte-identical to previous EngineBattle releases.

## File format

A single top-level JSON object, UTF-8 encoded, indented with 2 spaces, camelCase field names. The output file is overwritten on each invocation.

## Stability rules

- `schemaVersion` is incremented **only** on a *breaking* change — meaning a renamed field, a removed field, or a changed type for an existing field.
- New optional fields can be added at any time without bumping the version. **Consumers MUST ignore unknown fields.**
- The current value is `1`. Consumers should accept `schemaVersion >= 1` and reject lower values.
- Numeric fields are JSON numbers, never quoted. Non-finite floats (NaN, ±Infinity) are clamped to `0.0` before serialization to keep the file parseable by every standard JSON library.
- String fields are never `null`; missing values are emitted as the empty string `""` **except** for `filter` on score rows — see the row schema below.

## Numeric type representation

JSON has no distinction between integer and floating-point numbers — they are both just "number". `System.Text.Json` (the serializer EngineBattle uses) emits a "float" field with an exactly-integer value as a bare integer literal: e.g. `2700` instead of `2700.0`, `0` instead of `0.0`. This is standard and correct, but it means consumers using a strictly-typed parser (e.g. JSON Schema with `"type": "number"` vs `"type": "integer"`) may see surprises.

**Recommendation for consumers:** treat all `*Rating`, `accuracy`, `elapsedSeconds`, `avgKLD`, `playerVolatility`, and `ratingAvg` fields as **numeric** (`int OR float`), and coerce to `float` if your downstream math depends on it. Python's stdlib `json` will yield `int` for integer-valued numbers and `float` otherwise; `float(value)` works for both.

The fields **always** emitted as integers (no float ambiguity) are: `schemaVersion`, `totalPuzzlesLoaded`, `sampleSize`, `minRating`, `maxRating`, `nodes`, `totalNumber`, `correct`, `wrong`.

## Top-level fields

| Field | Type | Description |
|---|---|---|
| `schemaVersion` | int | Schema version. Currently `1`. |
| `engineBattleVersion` | string | EngineBattle assembly version (e.g. `"1.4.0.0"`). Informational only. |
| `puzzleFile` | string | Absolute path to the puzzle CSV that was loaded. |
| `totalPuzzlesLoaded` | int | Number of puzzles read from the CSV before sampling/filtering. |
| `sampleSize` | int | The `SampleSize` value from the puzzle config. |
| `minRating` | int | Minimum puzzle rating filter. |
| `maxRating` | int | Maximum puzzle rating filter. |
| `filter` | string | The `PuzzleFilter` themes string from the puzzle config (may be empty). |
| `ratingGroups` | string | The `RatingGroups` string from the puzzle config (may be empty). |
| `startedUtc` | string | ISO-8601 UTC timestamp when the run started, e.g. `"2026-04-08T19:23:45.123Z"`. |
| `elapsedSeconds` | float | Wall-clock duration of the puzzle run, in seconds. |
| `scores` | array | One entry per `(engine, neuralNet, type, nodes)` result. See below. |

## `scores[]` entry fields

| Field | Type | Description |
|---|---|---|
| `engine` | string | Engine name (from the engine config). |
| `neuralNet` | string | Neural net name or path used for this result (empty for non-NN engines). |
| `type` | string | Puzzle test type, e.g. `"Policy"`, `"Value"`, `"pTop3"`, `"vTop5"`, `"Search"`, `"Solve"`. |
| `nodes` | int | Node limit used for this result (relevant for `Search`/`Solve` types). |
| `filter` | string | Theme filter for this result row. **The literal string `"none"` is the no-filter sentinel** (not the empty string) — this matches EngineBattle's internal `Score.Filter` default. Consumers wanting a "no theme" check should compare against `"none"`, not `""`. |
| `totalNumber` | int | Total puzzles evaluated in this row. |
| `correct` | int | Number of correctly solved puzzles. |
| `wrong` | int | `totalNumber - correct`. |
| `accuracy` | float | `correct / totalNumber`, in `[0, 1]`. Defined as `0.0` when `totalNumber == 0`. |
| `ratingAvg` | float | Average rating of the puzzles attempted in this row. |
| `playerRating` | float | Glicko-derived rating of the engine on this row's puzzles. |
| `playerDeviation` | float | Glicko rating deviation. |
| `playerVolatility` | float | Glicko volatility. |
| `avgKLD` | float | Cross-entropy of the engine's policy distribution against the puzzle's one-hot correct-move target, averaged over solved puzzles only. The per-puzzle metric is `-log(P_engine(correct_move))` (despite the historical "KLD" name, it's a cross-entropy on a one-hot target — the information content comes from the engine's distribution being shaped by softmax over all legal moves). **Lower is better.** Only meaningful for policy-type rows (`Policy`, `pTopN`); `0.0` otherwise. |
| `avgRankWeightedKld` | float | Rank-weighted aggregate of per-puzzle cross-entropy using `1/rank` weights. Respects the `IncludeFailedPuzzles` config flag (solved-only by default, all puzzles when true). **Lower is better.** Only meaningful for policy-type rows; `0.0` otherwise. |
| `withHistory` | bool | Whether the engine was given prior moves as history (Lc0/Ceres only). |

## Recommended scalar signal for optimizers

For black-box optimization (SPSA, BO, etc.) using policy-type results, **prefer `avgKLD` or `avgRankWeightedKld` over `accuracy`** as the per-iteration scalar signal. Both metrics are continuous in the network's policy distribution and produce measurable per-step deltas even for tiny weight perturbations; top-1 `accuracy` is a step function and frequently produces zero gradient signal for small changes.

Choosing between `avgKLD` and `avgRankWeightedKld`:

- **`avgKLD`** is the historical metric. Averages the per-puzzle cross-entropy over solved puzzles only, with no rank weighting. Treats all solved puzzles equally regardless of how confidently the engine ranked the correct move.
- **`avgRankWeightedKld`** is the search-relevance-aware variant. Includes all puzzles, weights by `1/rank`. Prioritizes optimization on puzzles where the engine already places the correct move at high rank (the moves search will actually visit). Improvements on these puzzles translate more directly to play strength than improvements on puzzles where the correct move is buried at rank 15+ (which search would never visit anyway). Recommended for policy SPSA when the goal is Elo gain rather than overall puzzle accuracy.

## Example output

```json
{
  "schemaVersion": 1,
  "engineBattleVersion": "1.4.0.0",
  "puzzleFile": "C:/Dev/Chess/Puzzles/lichess_db_April_2025.csv",
  "totalPuzzlesLoaded": 1234567,
  "sampleSize": 500,
  "minRating": 0,
  "maxRating": 3500,
  "filter": "",
  "ratingGroups": "2500, 2700",
  "startedUtc": "2026-04-08T19:23:45.123Z",
  "elapsedSeconds": 12.34,
  "scores": [
    {
      "engine": "Ceres",
      "neuralNet": "C3-384-12-I8-refit.engine",
      "type": "Policy",
      "nodes": 1,
      "filter": "none",
      "totalNumber": 500,
      "correct": 372,
      "wrong": 128,
      "accuracy": 0.744,
      "ratingAvg": 1923.4,
      "playerRating": 2104.2,
      "playerDeviation": 51.7,
      "playerVolatility": 0.06,
      "avgKLD": 0.4127,
      "avgRankWeightedKld": 0.3415,
      "withHistory": false
    }
  ]
}
```

## Python parser sketch

```python
import json
from pathlib import Path

def load_puzzle_results(path: Path) -> dict:
    data = json.loads(path.read_text(encoding="utf-8"))
    if data.get("schemaVersion", 0) < 1:
        raise ValueError(f"Unsupported puzzle schema version: {data.get('schemaVersion')!r}")
    return data

def primary_signal(data: dict, score_type: str = "Policy") -> dict:
    """Reduce a puzzle JSON to a single optimizer signal."""
    rows = [s for s in data["scores"] if s["type"] == score_type and s["totalNumber"] > 0]
    if not rows:
        raise ValueError(f"No rows of type {score_type!r} found")
    total   = sum(r["totalNumber"] for r in rows)
    correct = sum(r["correct"]     for r in rows)
    avg_kld = sum(r["avgKLD"] * r["totalNumber"] for r in rows) / total
    # avgRankWeightedKld is additive — fall back to 0.0 for older puzzlejson outputs
    avg_rank_wt_kld = sum(
        r.get("avgRankWeightedKld", 0.0) * r["totalNumber"] for r in rows
    ) / total
    return {
        "accuracy":         correct / total,
        "kld":              avg_kld,
        "rank_weighted_kld": avg_rank_wt_kld,
        "elo":              rows[0]["playerRating"],
        "n":                total,
    }
```

## Excluded from this schema (deliberately)

The following fields exist on the in-memory `Score` record but are **not** part of the JSON contract because they are large, debugging-only, or change frequently:

- `failedPuzzles` — full per-puzzle failure list, potentially huge
- `correctPuzzles` — full per-puzzle success list, potentially huge

If a future need requires per-puzzle detail in the JSON, it will go behind a separate `--json-include-puzzles` flag rather than expanding the default schema.

## Compatibility notes

- The `puzzlejson` command without `--json` is unchanged from prior EngineBattle releases. The flag is purely additive.
- The flag is recognized in any position after the config arg (e.g. `puzzlejson cfg.json --json out.json` works; future flags can also appear).
- Aliases `puzzle` and `p` accept the flag identically.
