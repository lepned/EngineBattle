# Lichess Broadcast Kibitzer

Watch live human tournaments relayed by lichess with **your own engine** analyzing the games — policy overlays, multi-PV lines and an eval bar, all computed on your hardware. No broadcast site shows MCTS internals; EngineBattle does.

Open it from the navigation menu: **Tournament → Lichess broadcast** (`/broadcast`).

## Quick start

1. The page opens as a **lobby**: tournaments that are live right now appear as cards, everything else as a list with start times. Click one to start watching — you land on the tournament's current round (the live round if one is running, otherwise the latest finished round). Power users can paste any lichess broadcast round URL or id in the field at the bottom.
2. All games of the round appear as tiles; the first game becomes the focus board. Click a tile to focus a different game. The **header line** above the board holds everything else: **←** back to the lobby (the stream and engine keep running), the round switcher pill, a LIVE badge, a stream-health dot (green = connected, amber = reconnecting; hover for details), the Follow button and **✕** to stop.
3. Press **▶** in the kibitzer panel (top right of the move list) to start engine analysis. Analysis follows whatever position is on the board — live moves, browsing, or your own exploration.

## The kibitzer panel

A compact engine widget above the move list:

- **Engine picker** — lists every engine definition (`*.json`) in your *Engine defs folder* (Settings). The default engine is preselected.
- **▶ / ■** — start/stop. While running, analysis automatically restarts on every position change.
- **Time limit** — search time per position: 5s (default), 10s, 30s or ∞ (analyze until the position changes).
- **Lines** — number of PV lines shown (1/3/5, MultiPV).
- **overlay** — policy overlay on the board (only shown for engines with `LogLiveStats`, i.e. Lc0/Ceres).

Evals are displayed from **white's perspective** everywhere (panel, bar, chart), matching lichess convention.

If the engine process crashes, the panel shows *"engine exited — press ▶ to restart"* — one click recreates it.

## Eval bar, candidate moves and eval chart

- The **vertical bar** beside the board tracks the engine's main line, using the same win-probability scaling lichess uses. It flips with the board.
- Below the board, engines with `LogLiveStats` (Lc0/Ceres) get a **candidate-moves panel**: the top 5 root moves by visits, each with a visit-share bar, an amber tick marking the policy prior (so you see where search and net disagree), node count, Q and a win/draw/loss micro-bar — all white-perspective. A small **Moves / Eval history** toggle switches between this panel and the chart.
- The **eval chart** plots eval per move: your engine (colored line and dots) against lichess's server evaluation (gray line, present when lichess has analyzed the game). The engine line fills in as positions get analyzed — the live tip while following, plus any position you browse to. Click anywhere in the chart to jump the board to that move. Engines without live stats always show the chart.

## Following vs exploring

- **Following live** (default): the board tracks the newest move of the focused game.
- Browsing with **← / → / Home / End**, clicking moves, or making your own moves on the board switches to **exploring** — the game keeps updating in the background (tiles, move list), but the board stays put and the engine analyzes what *you* are looking at.
- Stepping forward onto the latest move — or pressing **Follow live** — resumes following. Your exploration lines are discarded on reload.

## Round switcher

When the current round belongs to a tournament from the official list, the round pill in the header is a dropdown — this is where you move between rounds (live and upcoming rounds are marked). Pasted rounds that are not in the official list have no switcher.

## Finished games

Finished games show the result as score chips in the player bars ("1" / "0" / "½") and stop counting clocks. Tiles order live games first, strongest pairings on top.

## How it works / endpoints

- `GET https://lichess.org/api/broadcast` — official broadcast list (picker + round switcher).
- `GET https://lichess.org/api/stream/broadcast/round/{id}.pgn` — streaming endpoint: on connect it replays every game's full PGN (mid-join works), then pushes a game's complete PGN again on every update, sub-second latency, no auth for public broadcasts.
- Clocks come from `[%clk]` PGN comments, lichess server evals from `[%eval]` comments; both are stripped from the move list after extraction.
- The PGN of each update is parsed with EngineBattle's own parser and diffed against known state, so reconnects are seamless.

## Tips

- Prefer Ceres/Lc0 as the kibitzer engine to get the policy overlay — seeing the network's candidate moves on a live grandmaster game is the point of this page.
- The ∞ time limit gives the deepest analysis but keeps your GPU busy; 5–10s per position is a good balance for following a live round.
