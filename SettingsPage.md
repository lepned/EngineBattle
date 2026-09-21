# Global Settings Page

The Settings page (`/settings`) configures paths, defaults, and preferences used across EngineBattle. Setting these up once saves time on every subsequent session — engines auto-load, file browsers open in the right folders, and analysis pages start with your preferred limits.

## Sections

### General

| Setting | Purpose |
|---------|---------|
| **Startup Page** | Which page opens when you launch the app. Set this to your most-used page (e.g., Single Analysis, Tournament). |
| **Default Engine** | Auto-loaded in Single Analysis, Game Review, and Play vs Computer. Avoids re-browsing for an engine every session. |
| **Secondary Engine** | Auto-loaded as the second engine in Dual Analysis. |

### Paths & Tools

| Setting | Purpose |
|---------|---------|
| **Analysis Games** | PGN file where analysis games are appended when you click "Save game" on analysis pages. |
| **Ordo Executable** | Full path to the Ordo rating tool, used by the Ordo Results page to compute ratings from PGN files. |

### Analysis

Defaults applied when opening analysis pages. You can still override them per session.

| Setting | Purpose |
|---------|---------|
| **Mode** | Default search mode: Time or Nodes. |
| **Time (ms)** | Default time limit for engine analysis. |
| **Nodes** | Default node limit for engine analysis. |
| **MultiPV Lines** | Default number of principal variation lines the engine reports. Higher values show more candidate moves. |
| **Policy Distribution Filter** | Min/max probability range for policy distribution charts (e.g., `0.4,0.6`). |
| **Min Policy Threshold** | Minimum probability (0-1) for a move to appear in the policy overlay on the chessboard. |
| **Combine White & Black Moves** | Merge white and black move data in policy distribution charts. |

### Game Review

Defaults for the Game Review page's analysis engine and the thresholds behind its move classification. The meaning of each threshold is explained in [GameReview.md](GameReview.md).

| Setting | Default | Purpose |
|---------|---------|---------|
| **Mode** | Time | Search mode: Time, Nodes, or Depth. |
| **Time (ms)** | 1000 | Milliseconds per move when using Time mode. |
| **Nodes** | 5000 | Node count per move when using Nodes mode. |
| **Depth** | 18 | Search depth per move when using Depth mode. |
| **MultiPV** | 5 | Principal variations evaluated per position; the gaps between them drive the classification. |
| **Accuracy Decay** | 0.085 | Exponential decay rate of the accuracy formula. |
| **Micro-Loss Base** / **Scale** | 0.037 / 0.20 | Penalty for best moves in easy positions, and how fast it shrinks with the PV gap. |
| **Brilliant / Great / Best min PV gap** | 0.15 / 0.10 / 0.05 | Win-probability gap between PV1 and PV2 that a played PV1 move needs for each label. |
| **Excellent / Good / Inaccuracy / Mistake** | 0.02 / 0.03 / 0.05 / 0.10 | Maximum win-probability loss for each label; above Mistake is a Blunder. |

### Folders

Directory paths that serve two purposes:
1. **FileBrowserDialog bookmarks** — configured folders appear as quick-access bookmarks when browsing for files.
2. **Initial browse directory** — when a page opens a file browser, it starts in the relevant folder instead of a generic location.

| Folder | Used by |
|--------|---------|
| **Engine Definitions** | Engine panels, analysis pages, Play vs Computer, the Engine creator |
| **Openings** | Tournament creator, opening book tools |
| **PGN Output** | Tournament results, analysis game saving, Ordo page |
| **Tournament Configs** | Tournament page, tournament creator |
| **Tablebase** | Tournament creator (Syzygy adjudication) and the Engine creator (`SyzygyPath` in new defs) |
| **Neural Networks** | Engine panel weights browser, the Engine creator (`NetworkPath` in new defs) |
| **Puzzle Configs** | Lichess Puzzles and ERET puzzle pages |
| **Puzzle Results** | Where puzzle runs write their summaries and failed-puzzle files; the Run report and Puzzle trend pages read from here |

**Show Engine Column in Puzzle Results** — when unchecked, puzzle result tables show only the neural net column.

### Board Appearance

Everything visual has its own page, **Appearance** (`/appearance`), with a live preview and instant apply; the Settings page only links to it. It holds:

- **Text & menu** — the app's base text size and the navigation menu.
- **Text per region** and **Charts & panels** — the tournament page: a text size for each region (standings, crosstable, pairings, engine panel, ...), chart heights, PV board size, which charts and panels to show, where the crosstable goes. Remembered **per screen**, so a laptop and the monitor it docks to each keep their own values; the corner control on the tournament page writes the same settings.
- **Theme**, **Pieces**, **Coordinates**, **PV Arrows**, **Policy Overlay**, **Highlight**, **Eval Bar** — the board and what is drawn on it.
- **Tournament header** and **Interaction & Effects** - the selection ring, move animation, **Show Legal Moves** (dots on the squares a picked-up piece may go to, a ring on a capture; rules only, no evaluation) and **Premoves** (queue your next move while the computer thinks on the play pages; it is played the moment your turn comes if it is still legal, shown on its two squares until then, and a click on the board cancels it).

## Persistence

Settings are saved to `Data/globalSettings.json` in the app directory. Click **Save** to persist changes. **Reset to Defaults** restores all values to their defaults (click Save again to persist the reset).

Settings are loaded on app startup and shared across all pages in the session.
