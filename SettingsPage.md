# Global Settings Page

The Settings page (`/settings`) configures paths, defaults, and preferences used across EngineBattle. Setting these up once saves time on every subsequent session — engines auto-load, file browsers start in the right folders, and analysis pages use your preferred search parameters.

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
| **Analysis Games** | PGN file where analysis games are appended when you click "Save PGN" on analysis pages. |
| **Ordo Executable** | Full path to the Ordo rating tool, used by the Ordo Results page to compute ratings from PGN files. |

### Folders

Directory paths that serve two purposes:
1. **FileBrowserDialog bookmarks** — configured folders appear as quick-access bookmarks when browsing for files.
2. **Initial browse directory** — when a page opens a file browser, it starts in the relevant folder instead of a generic location.

| Folder | Used by |
|--------|---------|
| **Engine Definitions** | Engine panels, analysis pages, Play vs Computer |
| **Openings** | Tournament creator, opening book tools |
| **PGN Output** | Tournament results, analysis game saving, Ordo page |
| **Tournament Configs** | Tournament page, tournament creator |
| **Tablebase** | Tournament creator (Syzygy adjudication) |
| **Neural Networks** | Engine panel weights browser |
| **Puzzle Configs** | Lichess Puzzles and ERET puzzle pages |

### Analysis

Defaults applied when opening analysis pages. You can still override them per-session.

| Setting | Purpose |
|---------|---------|
| **Search Time (ms)** | Default time limit for engine analysis. |
| **Search Nodes** | Default node limit for engine analysis. |
| **Chart Lines** | Number of lines shown in the CP evaluation chart. |
| **MultiPV Lines** | Default number of principal variation lines the engine reports. Higher values show more candidate moves. |
| **Policy Distribution Filter** | Min/max probability range for policy distribution charts (e.g., `0.4,0.6`). |
| **Min Policy Threshold** | Minimum probability (0-1) for a move to appear in the policy overlay on the chessboard. |
| **Combine White & Black Moves** | Merge white and black move data in policy distribution charts. |

### Game Review

Defaults for the Game Review page's analysis engine. These control how deeply each move is analyzed.

| Setting | Purpose |
|---------|---------|
| **Mode** | Search mode: Time, Nodes, or Depth. |
| **Time (ms)** | Milliseconds per move when using Time mode. |
| **Nodes** | Node count per move when using Nodes mode. |
| **Depth** | Search depth per move when using Depth mode. |

### Tournament

Defaults applied when creating tournament configurations.

| Setting | Purpose |
|---------|---------|
| **Delay Between Games (sec)** | Pause between games to let engines cool down (GPU thermal management). |
| **Move Overhead (ms)** | Time buffer subtracted from the engine's clock to account for communication delay. |

## Persistence

Settings are saved to `Data/globalSettings.json` in the app directory. Click **Save** to persist changes. **Reset to Defaults** restores all values to their defaults (click Save again to persist the reset).

Settings are loaded on app startup and shared across all pages in the session.
