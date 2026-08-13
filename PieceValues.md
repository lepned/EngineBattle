# Piece Values from a Neural Net

Tools for asking *"what is each piece worth to this network?"* — both contextually (one
position) and globally (the net's implied piece values). Two console commands:

- `piecevalues` / `pv` — **contextual** values in a single position (leave-one-out).
- `piecevaluefit` / `pvfit` — **global** values via regression over many positions.
- `pvbatch` — **batch** the global fit over a whole folder of nets (self-play + regress each).

There are three distinct notions of "a net's piece value", each with its own tool:

| Notion | Question it answers | Command |
|---|---|---|
| **Contextual** | What is *this* piece doing *here*? | `piecevalues` |
| **Eval-function** | What does the net's *evaluation* encode? | `pvfit … --pgneval` (or default eval mode) |
| **Revealed-preference** | What do the net's *results* imply? | `pvfit … --outcome` |

---

## 1. Contextual values — `piecevalues`

```
piecevalues <engine> [fen] [--fen S] [--moves m1 m2 …] [--nodes N] [--uci K V]
```

For every non-king piece: evaluate the position, remove that piece, re-evaluate, and
report the drop from the **owner's** perspective. Defaults to `nodes 1` (pure net eval).

**Key detail — measure in logit space, not win%.** Win% saturates at the rails: in a
balanced position, removing *any* officer is decisive, so every officer reads ~+50%. We
report **Δlogit** = `logit(W + ½D)` (from the soft WDL), which stays finite and monotone
and restores resolution (queen vs rook separate again). Δcp and Δscore% are shown too,
but Δlogit is the headline. Output includes a ranked table, an ASCII heat-map board, and a
per-type summary.

**Limitation.** Single-position leave-one-out resolves the **pawn tier and type ordering**
well, but officer magnitudes stay compressed because removal is the maximal perturbation.
A decided-position guard warns when `|baseline logit| > 2` (values are ill-posed once the
game is effectively over).

---

## 2. Global values — `pvfit`

```
pvfit <engine> <positions.epd|.pgn> [--sample N] [--nodes N] [--by-phase]
                                    [--outcome] [--pgneval] [--player NAME]
                                    [--bootstrap N] [--uci K V]
```

All modes regress on a **material-imbalance feature vector** `d = [ΔP, ΔN, ΔB, ΔR, ΔQ]`
(White-minus-Black piece counts, read from the FEN). Values are normalised so pawn = 1.

### Default (eval-regression)
Re-evaluates each position with the engine at `nodes 1` and regresses
`logit(W + ½D)` (White's view) on `d` (with intercept + a side-to-move/tempo term, L1
loss). A **quiet filter** drops in-check positions and positions whose best move is a
capture (a cheap quiescence proxy).

### `--outcome` — DeepMind model (PGN only)
The method behind AlphaZero's published piece values
([Tomašev et al. 2020](https://arxiv.org/abs/2009.04374)): fit `g(d) = tanh(wᵀd)` to the
**game result** `z ∈ {-1, 0, +1}` (White's perspective), by Gauss-Newton least squares.
**No intercept** — a balanced position (`d = 0`) gives `tanh(0)=0` and drops out of the
fit automatically, so only materially-imbalanced positions drive the estimate. This is the
**revealed-preference** view. (Uses `L1`/robust fitting elsewhere because the PNAS follow-up
found `L2` underestimates piece weights.)

### `--pgneval` — embedded eval (PGN only)
Regresses the engine's **in-game eval** (`wv=`, already stored White-relative and in pawns
by EB's annotation writer; parsed via `EngineTypes.Annotation.getEngineStatData`) on `d`.
A continuous, bounded pawn target avoids the queen blow-up that the `tanh`-on-outcome model
suffers under quasi-perfect separation. Keeps only **odd-total-piece-count** positions
(odd total ⟹ side counts differ by an odd number ⟹ guaranteed material imbalance; counted
via `PositionOps.numberOfPieces` on the live board). `--player NAME` keeps only plies where
the named engine was on move, so every `wv` is that single net's eval — letting you isolate
**one net's eval-function values from a mixed-engine PGN**.

`--by-phase` additionally splits into middlegame (>16 pieces) and endgame (≤16 pieces).

### `--bootstrap N` — confidence intervals (game-clustered)
Adds a 95% percentile CI per piece from `N` bootstrap resamples (`--ci` is an alias). The
resampling unit is the **whole game, not the position**: consecutive plies of one game share
nearly the same material, so resampling individual positions would treat ~20k correlated rows
as independent and report falsely tight intervals. Resampling games (≈1000 clusters) gives
honest width. The point estimate uses the same fit (`tanh` for `--outcome`, `L1` for
`--pgneval`); only the spread is bootstrapped. Fixed seed → reproducible.

**The marginal intervals look wide** — at 1000 games the per-net per-piece CIs are around
±0.8 (minors), ±1.1 (rook), ±3 (queen, skewed up from separation). Example — BT4-332 endgame
`--outcome`:

| Piece | Point | 95% CI |
|---|---|---|
| Knight | 2.78 | [1.94, 3.53] |
| Bishop | 2.95 | [2.13, 3.87] |
| Rook | 4.82 | [3.80, 6.06] |
| Queen | 9.12 | [6.69, 12.98] |

The CI is just a **precision** readout — how tightly this net's number is pinned down. More
self-play games tighten it; that's the lever when an interval is too loose.

One thing to get right when reading it: if you care about the **Bishop−Knight difference**,
look at the *paired gap CI* the tool prints — not whether the two marginal bars overlap.
The bishop and knight estimates are correlated across resamples (shared pawn-normalisation,
same games), so the gap is better resolved than the marginals suggest:

| Split | Gap (B−N) | 95% CI |
|---|---|---|
| All positions | +0.33 | [−0.10, +0.80] |
| Endgame | +0.17 | [−0.29, +0.76] |

The point estimate already shows bishop > knight for the net; the gap CI just says how
precisely. The **queen** is the loosest column (±3 at 1000 games) and benefits most from more
games.

---

## The data requirement (the crux)

The regression can only learn a piece's value from positions where that piece's count is
**imbalanced *and uncompensated***. This is where the data matters more than the method:

- **Balanced openings** (UHO etc.) → no material variance → nothing to fit.
- **Tactical puzzles** → imbalance confounded with the tactic.
- **Strong-engine games** → imbalances are rare and *compensated* (a good engine only
  sheds material for play), so only the **queen** registers; pawn/minor/rook wash to ~0.

The fix is data with **uncompensated** imbalances — e.g. **`nodes=1` self-play**: raw
policy moves (no search) are weak, so the net hangs pawns/minors/rooks without compensation
and games are decisive. This is cheap to generate and is closest in spirit to AlphaZero's
self-play (their signal came from self-play *diversity*, not strength). Generate a corpus
with a normal EB tournament at `NodeLimit=true, Nodes=1` over a varied opening book, then
point `pvfit` at the resulting PGN.

`pvfit` prints per-feature **material variance** and warns when it is near zero, so a
degenerate corpus is obvious rather than silently producing garbage.

---

## Worked example — net `C3-384-12-I8`

Corpus: 500 `nodes=1` self-play games (UHO openings, 68% decisive).

| Method | Pawn | Knight | Bishop | Rook | Queen |
|---|---|---|---|---|---|
| `--outcome`, all positions | 1.00 | 3.06 | 3.47 | 4.74 | 10.0 |
| `--outcome`, endgame | 1.00 | 2.83 | 3.17 | 4.20 | 8.54 |
| `--pgneval`, endgame | 1.00 | 2.94 | 3.19 | 4.69 | 6.36 |
| *classical theory* | *1* | *~3* | *~3.25* | *~5* | *~9* |

The revealed-preference fit lands on textbook values, and **bishop > knight** is recovered
by every method — the net has learned the bishop's slight edge. Cross-method agreement on
N/B/R is strong; the **queen is the least stable** (rare + separation-prone — more games
tighten it).

### Reading the diagnostics
- **sign-acc** (outcome): fraction of decisive positions whose result the material model
  predicts. Tracks where material matters — ~75% in endgames, ~52% in midgame.
- **R²** (eval modes): stays low (~0) because material is only one of several eval drivers
  (king safety, passers, activity). Trust the **coefficients** (averaged over many
  positions), not point-prediction.
- **Midgame alone is noisy** — material barely varies there; the endgame split is the
  load-bearing one.

---

## 3. Batch over a folder of nets — `pvbatch`

```
pvbatch <templateTournament.json> <netFolder> [--rounds N] [--out DIR]
```

Runs the global fit end-to-end for **every `*.onnx` in `<netFolder>`**, in one process. For
each net it: clones the template engine def (overriding `Network`), plays an `nodes=1`
self-play RR (`--rounds`, default 1000), then computes the **endgame (≤16 pieces)** `outcome`
and `pgneval` regressions and appends a row to `summary.csv`
(`net,mode,knight,bishop,rook,queen`). It is **resumable** (skips any net whose PGN already
has `rounds` games) and **fault-tolerant** (a failing net is logged and the batch continues).

> **Memory caveat (big nets).** Self-play loads **two** copies of the net per concurrency
> level, so the template's `NumberOfGamesInParallel` must be **1** for large nets —
> at 2 you get 4 simultaneous TensorRT engines and the GPU OOMs. The batch sets
> `PreventMoveDeviation` automatically when parallel ≤ 1.

### Cross-net results — 10 official Ceres nets

Corpus: 1000 `nodes=1` self-play games per net (UHO_4060_v4 openings), **endgame (≤16 pieces)**
regressions. Pawn ≡ 1.

**`--outcome` (revealed-preference — directly comparable to AlphaZero's published values):**

| Net | Knight | Bishop | Rook | Queen | B−N |
|---|---|---|---|---|---|
| C1-256-10 | 2.35 | 2.59 | 3.74 | 9.59 | +0.24 |
| C1-384-12 | 2.71 | 2.96 | 4.53 | 9.62 | +0.25 |
| C1-512-15 | 2.94 | 2.71 | 4.41 | 10.19 | −0.23 |
| C1-512-25 | 2.58 | 2.45 | 3.89 | 9.23 | −0.13 |
| C1-640-25 | 2.47 | 2.73 | 4.43 | 9.08 | +0.26 |
| C1-640-34 | 2.15 | 2.83 | 4.10 | 7.84 | +0.68 |
| C1-768-15 | 3.04 | 3.04 | 5.45 | 10.24 | 0.00 |
| C3-384-12-I8 | 2.69 | 3.07 | 4.66 | 9.23 | +0.38 |
| C3-512-34-pre8-I8 | 2.93 | 2.94 | 4.95 | 9.64 | +0.01 |
| C3-768-30-pre8-I8 | 2.23 | 2.52 | 4.04 | 8.65 | +0.29 |
| **BT4-332 (Lc0)** | **2.78** | **2.95** | **4.82** | **9.12** | **+0.17** |
| **AlphaZero (2020)** | **3.05** | **3.33** | **5.63** | **9.50** | **+0.28** |

**`--pgneval` (eval-function view, L1 on the in-game eval):**

| Net | Knight | Bishop | Rook | Queen |
|---|---|---|---|---|
| C1-256-10 | 2.21 | 2.16 | 3.69 | 5.99 |
| C1-384-12 | 3.71 | 3.86 | 5.76 | 15.22 |
| C1-512-15 | 2.35 | 2.10 | 3.66 | 7.12 |
| C1-512-25 | 2.31 | 2.41 | 3.01 | 5.39 |
| C1-640-25 | 2.03 | 1.96 | 2.81 | 6.08 |
| C1-640-34 | 1.65 | 2.37 | 4.02 | 7.40 |
| C1-768-15 | 2.26 | 2.81 | 3.35 | 6.92 |
| C3-384-12-I8 | 2.51 | 2.51 | 4.12 | 7.70 |
| C3-512-34-pre8-I8 | 3.27 | 3.61 | 5.40 | 10.70 |
| C3-768-30-pre8-I8 | 2.83 | 2.93 | 4.66 | 9.13 |
| **BT4-332 (Lc0)** | **3.14** | **3.10** | **4.32** | **7.67** |

(BT4-332 is a Leela BT4 net — different engine/architecture — run through the identical
`nodes=1` self-play + endgame-regression pipeline as a cross-family reference point.)

**Patterns.**
- **Bishop ≥ knight in 8 of 10 nets** (outcome) — same direction as AlphaZero (B 3.33 >
  N 3.05); the largest net **C1-640-34 has the strongest bishop preference (+0.68)**. Only
  C1-512-15 / C1-512-25 mildly invert.
- **Values are compressed vs AlphaZero**, especially knight and rook (rooks ~3.7–5.5 vs
  5.63). This is the expected signature of **`nodes=1` searchless + endgame-only**, not a
  defect — AZ's 2020 numbers came from *searched* (1s/move) games. Queen sits closest to
  the benchmark (~9).
- **C1-768-15 is the most "AlphaZero-like"** overall (N 3.04, B 3.04, R 5.45, Q 10.24).
- **`outcome` is far more stable than `pgneval`** here: the eval fit throws outliers
  (C1-384-12 queen **15.22**) where outcome stays sane (9.62). **Lead with `outcome`**;
  treat `pgneval` as a cross-check.
- **Cross-family check — BT4-332 (Lc0) lands mid-pack among the Ceres nets** despite being a
  different engine and architecture (outcome N 2.78 / B 2.95 / R 4.82 / Q 9.12, queen closest
  to AZ's 9.5; B > N preserved). That the scale and ordering match the Ceres family suggests
  the compression is driven by the **method** (searchless + endgame) far more than by net
  architecture or playing strength.

---

## References

- Tomašev, Paquet, Hassabis, Kramnik — *Assessing Game Balance with AlphaZero* (2020),
  [arXiv:2009.04374](https://arxiv.org/abs/2009.04374) — the `tanh(wᵀd)`-on-outcome model.
- McGrath et al. — *Acquisition of Chess Knowledge in AlphaZero*, PNAS 2022,
  [arXiv:2111.09259](https://arxiv.org/abs/2111.09259) — concept probing; L1-over-L2 finding.
