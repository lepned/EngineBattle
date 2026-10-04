# Game Review

Lichess-style game review with move-by-move accuracy analysis. Analyze individual games or batch-review entire PGN files to get per-player accuracy scores, move classifications, and win probability charts.

## Page Location

WebGUI: **Play & Analysis → Game review** (`/analysis/game-review`)

## Setup

### Engine

The left panel picks the engine (its **Engine** button). Configure a default engine in **Global Settings** to have it pre-loaded. Any UCI engine works — stronger engines and higher search limits produce more accurate reviews.

The review runs on its own instance of that engine, started at the first review and reused for every review after it (and every game of **Review All**). Loading another engine in the panel replaces it; a review running at that moment stops.

While a review runs, the panel shows the review's search: the position being searched, its lines and charts. Between reviews it is an ordinary analysis panel - **Start** searches the position on the board.

### Search Settings

The search limit is the panel's own, read when you click **Review**: **Time** (ms per position) or **Nodes** (per position). The page opens with the defaults from **Global Settings → Game Review**:

| Setting | Default | Description |
|---------|---------|-------------|
| **Search mode** | Time | Which limit the panel starts with: `Time` or `Nodes` |
| **Time per move** | 1000 ms | Milliseconds per position in Time mode |
| **Nodes** | 5000 | Nodes per position in Nodes mode |
| **MultiPV** | 5 | Principal variations per position; the gaps between them drive the classification. Sent to the engine at every review, also when it is 1 |

## Review Modes

### Full Review

Click **Review** to analyze the current game. The engine searches the position before every move, then the final position, each with MultiPV, and the moves are classified from what it found. The board and the panel follow the position being searched, and a progress bar counts the positions. Each review searches every position again; nothing is carried over from an earlier review.

- **Book moves** (a PGN comment containing "book") and positions with no legal move are not searched. A final checkmate or stalemate is scored by the rules: a win for the side that mated, a draw for stalemate.
- **During a review** Load PGN, the game list and the game arrows are locked, so a result always belongs to the game it was started on.
- **Cancel** stops the search at once and discards the review: nothing is saved or annotated, and the game shows what it showed before (an earlier review's result, or the evals from its PGN).
- **If the engine fails** (it exits or stops answering), the review stops with a message saying why; the next review starts the engine again. A bestmove that is not legal in its position does not stop the review: that position is scored without a best move.

### Review All

For multi-game PGN files, click **Review All** to review every game in turn, the same way as **Review**. The line above the progress bar says which game is being reviewed. Each finished game keeps its result, shown again straight away when you return to it with the arrows or the game list.

**Cancel** stops the game being reviewed (discarded, as above) and starts no further games; games already finished keep their results. A failed review stops the run as well.

### Quick Review

Click **Quick Review** to instantly classify moves using existing PGN annotations (eval comments like `wv=` fields) without running an engine. This works on games that were previously exported with annotations from EngineBattle or other tools that embed evaluation data.

## Results

### Accuracy Scores

Each player gets an overall accuracy score (0–100) displayed as a ring chart. The score uses a Lichess-style formula: exponential decay based on win probability loss per move; the game score is the average of a volatility-weighted mean and the harmonic mean of the per-move accuracies (phase accuracies use the harmonic mean alone).

Phase-by-phase accuracy is also shown:
- **Opening** (moves 1–15)
- **Middlegame** (moves 16–40)
- **Endgame** (moves 41+)

**ACPL** (Average Centipawn Loss) is shown alongside the phase breakdown.

### Move Classifications

Each move is classified based on MultiPV analysis:

| Classification | Symbol | Meaning |
|---------------|--------|---------|
| **Brilliant** | !! | Played move matches PV1, large gap to PV2, and the move is a sacrifice |
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
2. Click **Export PGN** to save the annotated game shown on the board

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
