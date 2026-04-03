# Game Review

Lichess-style game review with move-by-move accuracy analysis. Analyze individual games or batch-review entire PGN files to get per-player accuracy scores, move classifications, and win probability charts.

## Page Location

WebGUI: **Analysis → Game Review** (`/analysis/game-review`)

## Setup

### Engine

The left panel loads a chess engine for analysis. Configure a default engine in **Global Settings** to have it pre-loaded. Any UCI engine works — stronger engines and higher search limits produce more accurate reviews.

### Search Settings

| Setting | Default | Description |
|---------|---------|-------------|
| **Search mode** | Time | `Time` (ms per move), `Nodes`, or `Depth` |
| **Time per move** | 1000 ms | Milliseconds per position when using Time mode |
| **Nodes** | 5000 | Nodes per position when using Nodes mode |
| **Depth** | 18 | Depth per position when using Depth mode |
| **MultiPV** | 5 | Number of principal variations to evaluate (used for move classification) |

## Review Modes

### Full Review

Click **Review** to analyze the current game with the loaded engine. Each position is evaluated with MultiPV to classify moves. A progress bar shows the current move being analyzed. Already-analyzed positions are skipped on re-review.

### Review All

For multi-game PGN files, click **Review All** to batch-analyze every game. Results are stored per-game and preserved when navigating between games.

### Quick Review

Click **Quick Review** to instantly classify moves using existing PGN annotations (eval comments like `wv=` fields) without running an engine. This works on games that were previously exported with annotations from EngineBattle or other tools that embed evaluation data.

## Results

### Accuracy Scores

Each player gets an overall accuracy score (0–100) displayed as a ring chart. The score uses a Lichess-style formula: exponential decay based on win probability loss per move, with harmonic mean weighting.

Phase-by-phase accuracy is also shown:
- **Opening** (moves 1–15)
- **Middlegame** (moves 16–40)
- **Endgame** (moves 41+)

**ACPL** (Average Centipawn Loss) is shown alongside the phase breakdown.

### Move Classifications

Each move is classified based on MultiPV analysis:

| Classification | Symbol | Meaning |
|---------------|--------|---------|
| **Brilliant** | !! | Played move matches PV1, large gap to PV2 |
| **Great** | ! | Played move matches PV1, notable gap to PV2 |
| **Best** | Best | Played move matches PV1 |
| **Excellent** | Excellent | Very small win probability loss |
| **Good** | Good | Small win probability loss |
| **Inaccuracy** | ?! | Moderate win probability loss |
| **Mistake** | ? | Significant win probability loss |
| **Blunder** | ?? | Large win probability loss |
| **Book** | Book | Opening book move (skipped from accuracy) |
| **Forced** | Forced | Only one legal move |

Classification thresholds are configurable in **Global Settings**.

### Win Probability Chart

A chart shows win probability over the course of the game, with a vertical indicator that follows board navigation. The chart updates as you click through moves.

### Critical Moves

A panel lists the most impactful moves (inaccuracies, mistakes, and blunders) with:
- Classification badge and symbol
- The move played and the engine's best move
- Per-move accuracy percentage

Click any critical move to jump to that position on the board.

### Move List

The annotated move list shows:
- Colored classification symbols next to each move
- Engine evaluation inline
- Click any move to navigate to that position

## Export

1. Click **Export Folder** to select a destination
2. Click **Export PGN** to save the annotated game(s)

The exported PGN includes:
- Engine evaluation annotations per move
- Move classification comments
- Best move variations (engine's preferred line when the played move differs)

Exported PGN files can be reloaded later and used with **Quick Review** for instant re-analysis without an engine.

## Multi-Game Navigation

When a PGN file contains multiple games:
- Use the **◀ / ▶** arrows or the game counter button to navigate
- A game list panel on the right shows all games (up to 100)
- Click any game to select it
- Per-game analysis results are preserved when switching between games

## Configurable Thresholds

In **Global Settings → Game Review**, you can adjust:

| Parameter | Default | Description |
|-----------|---------|-------------|
| **BrilliantPVGap** | 0.15 | Min WP gap between PV1 and PV2 for Brilliant |
| **GreatPVGap** | 0.10 | Min WP gap for Great |
| **BestMinPVGap** | 0.05 | Min WP gap for Best (vs just Excellent) |
| **ExcellentMaxWPLoss** | 0.02 | Max WP loss for Excellent |
| **GoodMaxWPLoss** | 0.03 | Max WP loss for Good |
| **InaccuracyMaxWPLoss** | 0.05 | Max WP loss for Inaccuracy |
| **MistakeMaxWPLoss** | 0.10 | Max WP loss for Mistake (above = Blunder) |
| **AccuracyDecay** | 0.085 | Exponential decay rate for accuracy formula |
| **MicroLossBase** | 0.037 | Base WP penalty for PV1 matches in easy positions |
| **MicroLossScale** | 0.20 | How fast micro-loss shrinks with increasing PV gap |
