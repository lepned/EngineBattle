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
| `paired` | array | One entry per net PAIR per slice, with the discordant counts and McNemar's z. Empty for single-net runs and absent in files written before this field existed. See below. |
| `pairedFailed` | bool | `true` only when computing the paired stats threw. Read it before concluding anything from an empty `paired` — see below. |

### Three units, and which to use

A Lichess puzzle is a SEQUENCE of moves, and `accuracy` scores the whole sequence: one wrong move anywhere and the puzzle counts as failed. That remains the definition.

> **Discontinuity, 2026-08-25.** Puzzles are credited when the engine's move differs from the recorded solution but still delivers checkmate. That check used to accept ANY position with no legal replies, which also matches **stalemate** — so a move that threw a won game away was scored as a solve. It now requires the opponent to be in check. `accuracy` therefore drops slightly from this build onward, and a step curve joining runs across the boundary (`puzzletrend` reads `accuracy`) will show it. The direction is a correction, not a regression: the old numbers were too high.

The problem it creates is attribution. A puzzle's THEMES describe the move it exists for — its first solver move — while later moves are usually forced follow-up carrying the same tags without being about them. Measured on a 1500-puzzle sample, **38–43% of failed puzzles had the thematic move right** and went wrong later, so `accuracy` charges nearly four in ten failures to a theme that had nothing to do with them.

| field | unit | use it for |
|---|---|---|
| `accuracy` | whole puzzle | overall strength; comparable with every historical run |
| `firstMoveAccuracy` | the thematic move | **anything per-theme** |
| `positionAccuracy` | every position | how much of a line a net gets right; opt-in |

`positionAccuracy` does NOT fix the attribution problem — every position of the puzzle still inherits the puzzle's tags.

### What `scores[]` and `paired[]` each cover

`scores[]` is the **complete record of the run**: every result row, including slices that produced no puzzles at all (`totalNumber: 0`). Nothing is filtered out of it, because this file is the machine-readable archive of the run and a consumer can always filter, while it cannot recover what was dropped.

`paired[]` covers only rows that can be **compared**, which means it excludes the empty (`totalNumber: 0`) slices — there is nothing to pair. The human-readable `LichessSummary_<stamp>.txt` beside it applies the same rule and lists the same comparisons, though it caps its table at 40 rows (stating the count when it does); `paired[]` is never capped, so the JSON is the complete list.

**An empty `paired[]` is normal.** Measuring one net at a time and comparing it against nets measured in earlier runs is a routine workflow, and such a run has nothing to pair. Do **not** treat empty as an error: check `pairedFailed` first. It is `true` only when the computation threw, which is the one case where `paired` is empty for a bad reason.

Practical consequence: joining `paired[]` to `scores[]` on `(type, ratingGroup, nodes, filter)` can find a score row with no paired counterpart. That is expected. The reverse never happens.


## `scores[]` entry fields

| Field | Type | Description |
|---|---|---|
| `engine` | string | Engine name (from the engine config). |
| `neuralNet` | string | Neural net name or path used for this result (empty for non-NN engines). |
| `type` | string | Puzzle test type, e.g. `"Policy"`, `"Value"`, `"pTop3"`, `"Search"`, `"Solve"`. |
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
| `avgFrontierKld` | float | Frontier-weighted cross-entropy: peaks where the correct move sits at rank 2-3 and falls away at rank 1 and rank 6+, so it emphasises the moves a search is most likely to flip. **Lower is better.** Only meaningful for policy-type rows; `0.0` otherwise. |
| `avgMarginLoss` | float | Pairwise margin between the correct move's probability and the best competing move's. **Lower is better.** Only meaningful for policy-type rows; `0.0` otherwise. |
| `avgValueLoss` | float | Value-head loss, `|Q - expected_Q|` derived from the puzzle's themes, over solved puzzles only. **Lower is better.** `0.0` when not measured. |
| `avgEstNodesLog10` | float | Mean of `log10(1 + N_est)`, where `N_est` estimates the parent visits a PUCT search needs before it first explores the correct move. Smooth aggregate, suited to tuning signals. **Lower is better.** `0.0` for non-policy rows — consumers use that as the "metric present" gate. |
| `estNodesP95` | float | 95th percentile of the per-puzzle `N_est` distribution, in raw node units: in the worst 5% of positions the search needs about this many nodes before it even tries the correct move. **Lower is better.** `0.0` for non-policy rows. |
| `estNodesP99` | float | As above at the 99th percentile. |
| `estNodesMax` | float | Worst single puzzle in the set by `N_est`. `0.0` when unavailable. |
| `estNodesCdf100` | float | Fraction (`0..1`) of puzzles whose `N_est` is at most 100 nodes. **Higher is better.** `0.0` for non-policy rows. |
| `firstMoveCorrect` | int | Puzzles whose FIRST solver move was right. |
| `firstMoveScored` | int | Puzzles that had a first solver move to score. `0` means the test does not track it — not that nothing was correct. `solve` is such a test. |
| `firstMoveAccuracy` | float | `firstMoveCorrect / firstMoveScored`, or `0.0` when `firstMoveScored` is 0. **Use this, not `accuracy`, when attributing a result to a theme** — see below. |
| `positionsCorrect` | int | Positions scored correctly across all puzzles. `0` unless the run set `ScoreAllPositions`. |
| `positionsScored` | int | Positions scored. `0` unless the run set `ScoreAllPositions`. Always `0` for `solve`, which runs one search from the puzzle's start position and so has no per-position verdicts. |
| `positionAccuracy` | float | `positionsCorrect / positionsScored`, or `0.0` when nothing was scored. |
| `withHistory` | bool | Whether the engine was given prior moves as history (Lc0/Ceres only). |

## `paired[]` entry fields

Every net in a run is scored on the **same** puzzles, so puzzle difficulty is common to both members of a pair and cancels. Only the positions where the two nets *disagree* carry information about which is better, and McNemar's test uses exactly those.

This matters in practice. On a real 4000-puzzle sweep, comparing two adjacent training checkpoints on the **same whole-slice delta**:

| metric | delta | unpaired sigma | paired z |
|---|---|---|---|
| policy top-1 | +2.6 pp | 2.3 | **3.74** |
| value | +2.1 pp | 1.9 | **2.82** |

A comparison that reads as marginal unpaired is often clearly significant paired.

**Do not compare a `paired[]` z against a per-theme sigma from `puzzleThemes_<stamp>.csv`.** They are different statistics on different data: `z` is computed on the whole slice, while the theme sigma is computed on that theme's own (always smaller) count, and on a different delta. The ratio between them is dominated by `sqrt(n_slice / n_theme)`, which says nothing about pairing. A significant `z` here does **not** confirm a theme-level difference.

| Field | Type | Description |
|---|---|---|
| `type` | string | Test type of the slice: `"Policy"`, `"pTop3"`, `"Value"`, … |
| `ratingGroup` | int | Rating group, bucketed to the nearest 100 from the slice's average puzzle rating. |
| `nodes` | int | Node count the slice was run at. |
| `filter` | string | Theme filter of the slice (may be empty). |
| `engineA` / `engineB` | string | Engine (def) names. A is the net listed **first in the config**, matching the theme tables. |
| `netA` / `netB` | string | Network names for A and B. |
| `n` | int | Puzzles **both** nets scored. Normally the full sample; smaller if one net's slice was cut short. |
| `onlyA` | int | Solved by A, failed by B. |
| `onlyB` | int | Solved by B, failed by A. |
| `discordant` | int | `onlyA + onlyB`. Below ~25 the normal approximation behind `z` is optimistic — prefer an exact binomial test on `onlyA`/`onlyB` there. |
| `accuracyAPct` / `accuracyBPct` | float | Accuracy over `n`, in percent. |
| `deltaPp` | float | `accuracyBPct - accuracyAPct`. Positive means B is better. |
| `z` | float | `(onlyB - onlyA) / sqrt(discordant)`. Signed so positive favours B. `0.0` when the nets never disagree. |
| `p` | float | Two-sided p for `z`, from a normal approximation. Convenience only; nothing in EngineBattle branches on it. |

All pairs are emitted, not just baseline-vs-rest: a three-net step curve wants the adjacent-step pairs too. A run of `k` nets therefore produces `k*(k-1)/2` rows per slice.

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
      "avgFrontierKld": 0.5218,
      "avgMarginLoss": 0.8298,
      "avgValueLoss": 0.3687,
      "avgEstNodesLog10": 0.6391,
      "estNodesP95": 19.0,
      "estNodesP99": 227.0,
      "estNodesMax": 6200.0,
      "estNodesCdf100": 0.983,
      "withHistory": false
    }
  ],
  "paired": [
    {
      "type": "Policy",
      "ratingGroup": 1900,
      "nodes": 1,
      "filter": "",
      "engineA": "Ceres net A",
      "engineB": "Ceres net B",
      "netA": "netA.onnx",
      "netB": "netB.onnx",
      "n": 500,
      "onlyA": 41,
      "onlyB": 62,
      "discordant": 103,
      "accuracyAPct": 74.4,
      "accuracyBPct": 78.6,
      "deltaPp": 4.2,
      "z": 2.0692,
      "p": 0.0385
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

The `paired[]` array is the exception that proves the rule: it is the one summary of those two lists that cannot be recomputed from anything else in the file, so it is carried even though the lists themselves are not.

If a future need requires per-puzzle detail in the JSON, it will go behind a separate `--json-include-puzzles` flag rather than expanding the default schema.

## Compatibility notes

- The `puzzlejson` command without `--json` is unchanged from prior EngineBattle releases. The flag is purely additive.
- The flag is recognized in any position after the config arg (e.g. `puzzlejson cfg.json --json out.json` works; future flags can also appear).
- Aliases `puzzle` and `p` accept the flag identically.
- `paired` was added without a `schemaVersion` bump, per the stability rules: it is a new optional field, and a consumer that does not know it ignores it. Files written before it exist simply have no `paired` key — read it defensively.
