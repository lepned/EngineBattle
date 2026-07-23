# Plan: Lichess Broadcast Kibitzer (live human games + local engine insight)

**Status: PLANNING ONLY — discussed 2026-07-22, no implementation decision yet.**

Show live human tournament games (World Championship, Candidates, any lichess
broadcast) inside EB with the full engine-analysis experience: a local Ceres/Lc0
(and/or Stockfish) analyzing the live position on the *user's own hardware*, with
EB's visualization — policy overlay, N/Q search plots, Q-vs-N convergence, WDL,
eval history, PV boards — the way tournament mode presents engine-vs-engine games
today.

## Why it's worth it (assessment)

- **Genuinely differentiated.** Every broadcast site shows a server-Stockfish eval
  bar and a best line. Nobody shows *MCTS internals* — policy priors vs search
  visits on the live position, convergence charts, net-vs-search disagreement.
  That is EB's home turf and is unique kibitzing content.
- **Zero server cost, scales with the user.** The engine runs on the viewer's GPU;
  EB is just the window. A 4090 owner gets world-class analysis of a WC game for free.
- **Deep reuse.** Almost every building block exists: streaming HTTP client pattern
  (`CeresStreamClient` with reconnect/backoff), full PGN parser (F#), EbChessboard,
  EnginePanel with auto-search + PVState + charts, policy overlay, eval-history
  charts, small board tiles. The new work is mostly glue plus one new page.
- **Audience fit.** EB users are exactly the people who own GPUs and care about
  engine internals. This turns EB from "engine testing tool" into something they
  open on WC days.

Verdict: high value / moderate cost — one of the best remaining feature ideas.

## Data source (verified 2026-07-22)

- `GET /api/stream/broadcast/round/{broadcastRoundId}.pgn` — streams the round;
  whenever a game updates, its PGN is (re)sent. Designed for sub-second-latency
  consumers. Public broadcasts need no auth.
- `GET /api/broadcast` — official broadcast list (for a picker); users can also
  paste a broadcast round URL/id directly.
- PGN carries players, Elo, result, and `[%clk]` comments for clocks.
- Fallback for non-lichess sources later: poll a plain live-PGN URL — but most
  major events relay through lichess broadcasts anyway, so v1 is lichess-only.

## Architecture sketch

1. **`LichessBroadcastClient`** (WebGUI service, C#) — modeled directly on
   `CeresStreamClient`: long-lived HTTP stream, line reader, fixed-backoff
   reconnect. Emits raw PGN chunks per game update.
2. **`BroadcastState`** (service) — parses incoming PGN (ChessLibrary parser),
   keyed by game (White/Black/Round tags): move list, clocks, result, and a
   per-game "new move" event. Handles the stream's resend-full-PGN model by
   diffing against known moves (also gives mid-join catch-up for free).
3. **`/broadcast` page** —
   - Picker: paste round URL or choose from the official list.
   - Focus game: EbChessboard + player/rating/clock header + move list
     (tournament-live look), stepping back through moves allowed (analysis
     follows whatever position is shown; "Follow live" toggle like PVBoardLive).
   - Other games: small tiles (PVtileBoard-style) to switch focus; results fill
     in as games end.
4. **Engine attachment** — 1–2 `EnginePanel`s exactly as in Single/DualAnalysis:
   auto-search restarts on position change (pattern exists: Focus+Auto). New
   live move arrives → stop, position, go. Human move cadence (minutes) means
   deep, stable searches — much friendlier than engine-game speeds. Ceres/Lc0
   give the policy/N/Q content; a second engine enables disagreement views
   (Compare PV exists).
5. **Game-long eval chart** — append focused-game evals per move like the
   tournament eval chart, so joining mid-game still shows the story so far
   (evaluate-past-moves lazily or only from join point in v1).

## Phases

1. **Ingest** — client + PGN diffing + BroadcastState with a console/log proof. (~1 day)
2. **Page core** — picker, focus board, move list, clocks, tiles. (~1–1.5 days)
3. **Engine glue** — EnginePanel(s) + auto-follow + charts + policy overlay. (~1–1.5 days)
4. **Polish** — reconnect/mid-join UX, results, round switching, docs. (~1 day)

Total ≈ 4–6 focused days. Phase 1 is the only real unknown (exact stream framing;
verify whether connect replays all current games — expected yes).

## Open questions / risks

- Stream framing details (game separators, whether tags always resent) — resolve
  in phase 1 with a live broadcast or a recorded sample.
- Rate limits: one streaming connection per viewer is the designed use; the
  official-list endpoint is low-traffic. Fine.
- Multi-round events: v1 = one round at a time; round switcher later.
- Analyzing *all* games concurrently (one engine rotating, or N engines) is a
  natural v2; v1 analyzes the focused game only.
- Clock display needs `[%clk]` parsing; some broadcasts lack clocks — degrade
  gracefully.
