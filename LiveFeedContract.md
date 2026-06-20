# Live Feed Contract (External Tournament Runner → EngineBattle)

**Status:** Draft v0.1 — wire-schema specification only. No implementation yet.

This document specifies the JSON contract an **external (proprietary) tournament runner** must
produce so EngineBattle (EB) can drive its live game visualization without running the engines
itself. A future EB page (a feed-driven clone of `Tournaments.razor`) consumes this stream.

> The contract is a JSON serialization of EB's internal `Update` event stream
> (`ChessLibrary/Tournament/TournamentTypes.fs:14`). The internal runner is just one producer of
> that stream; an external runner becomes a second producer. The wire format below is deliberately
> **decoupled** from the internal F# types (explicit `type` tags, camelCase, no F# DU
> auto-serialization) so internal refactors don't break external producers.

---

## 1. Transport model

The feed is an ordered **stream of events**. Each event is a single JSON object with a `type`
discriminator. EB applies events in the order received; ordering matters (a `BestMove` before its
`StartOfGame` is undefined behavior).

Transport is out of scope for this contract, but any of these can carry the stream unchanged:

- **NDJSON** (newline-delimited JSON) — one event object per line. Recommended for file tail / replay.
- **WebSocket / SignalR** — one event object per message. Recommended for live, low-latency.
- **HTTP POST** — one event object (or a JSON array of events) per request.

A reference implementation should accept NDJSON first (trivial to capture, replay, and diff against
a real internal game — see §6).

### Envelope

```jsonc
{ "type": "<EventType>", "gameId": "<stream key>", /* ...event-specific fields... */ }
```

Unknown `type` values and unknown fields MUST be ignored by EB (forward compatibility).

**`gameId` (reserved; single-game in v0.1).** Identifies which concurrent game a *per-game* event
belongs to, so a runner emitting many games in parallel can drive multiple independent views. See
§8 (Parallel games). For v0.1, producers may omit `gameId` or send `"0"`; EB treats a missing
`gameId` as the single active game. **Per-game** events carry `gameId`; **tournament-level** events
(`StartOfTournament`, `PairingList`, `TotalNumberOfPairs`, `RoundNr`, `PeriodicResults`,
`EndOfTournament`) are global and omit it.

---

## 2. Shared value types

These objects appear inside multiple events.

### 2.1 `eval` — evaluation (maps to `EvalType`)

```jsonc
{ "kind": "cp",   "value": 0.35 }   // centipawn score, in PAWNS (float). +0.35
{ "kind": "mate", "value": 5 }      // mate in N plies/moves (int). negative = getting mated
{ "kind": "na" }                    // not available
```

- `value` for `cp` is in **pawns** (e.g. `0.35`, not `35`), matching `EvalType.CP of float`.
- Orientation: score for the side that just searched (UCI side-to-move convention), positive = good
  for that side. *(Confirm against EB's chart sign convention during implementation.)*

### 2.2 `wdl` — win/draw/loss (maps to `WDLType`)

```jsonc
{ "win": 450, "draw": 500, "loss": 50 }   // per-mille (0..1000), should sum ~1000
null                                       // omit or null ⇒ NotFound (rendered as 0/0/0)
```

### 2.3 `time` fields (map to `TimeOnly`)

String, format `"HH:mm:ss"` or `"HH:mm:ss.fff"`. Examples: `"00:05:00"`, `"00:00:14.250"`.

### 2.4 `moveAndFen` (maps to `MoveAndFen` / `MoveDetail`) — for board animation

```jsonc
{
  "shortSan": "Nf3",                 // SAN for the move list
  "fenAfterMove": "rnbqkb1r/...",    // full FEN AFTER this move
  "move": {                          // detail used for square highlighting
    "longSan": "g1f3",               // long algebraic (from+to)
    "fromSq": "g1",
    "toSq": "f3",
    "color": "w",                    // "w" | "b"
    "isCastling": false,
    "comments": ""                   // optional
  }
}
```

### 2.5 `player` — engine identity & config (maps to `EngineConfig`)

Only the fields EB renders are required; everything else falls back to `EngineConfig.Empty` defaults.

| Field        | Type              | Req | Drives |
|--------------|-------------------|-----|--------|
| `name`       | string            | ✓   | Player name everywhere |
| `version`    | string            |     | Version label under logo |
| `dev`        | string            |     | Developer attribution / dev counter |
| `logoPath`   | string            |     | Engine logo image (URL or `Img/…`); blank ⇒ default logo |
| `options`    | object<string,string> | | **Hover-config tooltip** (UCI options) |
| `alias`      | string            |     | Short name in tables |
| `rating`     | int               |     | Elo seed / display |

```jsonc
{ "name": "Engine A", "version": "1.2", "dev": "Jane Doe", "logoPath": "Img/a.png",
  "options": { "Hash": "256", "Threads": "8", "SyzygyPath": "/tb" } }
```

### 2.6 `result` (maps to `Result`)

```jsonc
{
  "player1": "Engine A",     // White
  "player2": "Engine B",     // Black
  "result": "1-0",           // "1-0" | "0-1" | "1/2-1/2"
  "reason": "CM",            // ResultReason code, see §2.7
  "moves": 42,               // full-move count
  "gameTime": 450000,        // total game time, milliseconds (int64)
  "outOfOpeningEvals": []    // optional: eval[] captured leaving book
}
```

### 2.7 `reason` codes (maps to `ResultReason`)

| Code   | Meaning |
|--------|---------|
| `CM`   | Checkmate |
| `SM`   | Stalemate |
| `TB`   | Tablebase known result |
| `AM`   | Insufficient material |
| `50m`  | Too many moves (move limit) |
| `R3`   | Repetition draw |
| `AE`   | Evaluation agreement (adjudicated) |
| `FL`   | Time/node limit forfeit |
| `XX`   | Cancelled |
| `IM`   | Illegal move |
| `DC`   | Disconnected |
| `NS`   | Not started |
| `AU`   | Adjudicated by user |

---

## 3. Events

Legend: **Req** = required for full visualization · **Rec** = recommended · **Opt** = optional.

### 3.1 `StartOfTournament` — **Req**

Initializes tournament-wide state (info banner, standings scaffolding, charts).

```jsonc
{
  "type": "StartOfTournament",
  "numberOfGames": 100,
  "gameDurationSec": "00:02:30",       // TimeSpan estimate per game
  "tournamentDurationSec": "04:10:00", // TimeSpan estimate total
  "tournament": { /* Tournament config object — see note */ }
}
```

> `tournament` uses the same schema as `tournament.json` (see `TournamentConfig.md`). EB needs at
> least the participant list and `TournamentMode` so it can compute standings/crosstable from the
> `result`s you send later. Minimal viable subset: `Name`, `TournamentMode`, the engine/player list,
> `Rounds`. Sending the full object is preferred.

### 3.2 `TotalNumberOfPairs` — **Rec**

```jsonc
{ "type": "TotalNumberOfPairs", "count": 100 }
```

### 3.3 `PairingList` — **Rec**

Upcoming matchups shown in the pairings/cycling table.

```jsonc
{
  "type": "PairingList",
  "pairings": [
    { "white": { /* player */ }, "black": { /* player */ },
      "gameNr": 1, "roundNr": "1", "openingName": "Sicilian", "openingHash": "ab12" }
  ]
}
```

> `Pairing.Opening` is a full `PgnGame` internally; for the feed, `openingName`/`openingHash` are
> enough for display. The adapter fills the rest with empty defaults.

### 3.4 `RoundNr` — **Rec**

```jsonc
{ "type": "RoundNr", "round": "5/10" }
```

### 3.5 `StartOfGame` — **Req**

Initializes a new game: players, logos, hover-config, clocks, opening moves, starting position.

```jsonc
{
  "type": "StartOfGame",
  "whitePlayer": { /* player, §2.5 */ },
  "blackPlayer": { /* player, §2.5 */ },
  "startPos": "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  "openingMovesAndFen": [ /* moveAndFen[], §2.4 — book moves to replay */ ],
  "whiteTime": "00:05:00",
  "blackTime": "00:05:00",
  "whiteToMove": true,
  "openingName": "Sicilian Defense",
  "currentGameNr": 42,
  "openingHash": "ab12cd"
}
```

`openingMovesAndFen` is animated onto the board before live play begins; the last entry's
`fenAfterMove` becomes the live starting FEN. Send an empty array for games starting from `startPos`.

### 3.6 `GameStarted` — **Req**

Fires immediately after `StartOfGame`; starts the clocks and clears prior-game PV/eval/chart state.

```jsonc
{ "type": "GameStarted", "player": "Engine A" }
```

### 3.7 `Status` — **Req** (live "thinking")

Emit during search. Updates the engine panel, PV board, and PV-agreement display. Maps to
`EngineStatus`.

**Update cadence (important):** `Status` is **best-effort and cosmetic**, not authoritative —
everything correctness-critical (move, final eval, FEN, move list, chart points) arrives in
`BestMove`. A late, coarse, or dropped `Status` only makes the thinking panel refresh less often;
nothing breaks. Guidance:

- **Coalesce to the latest snapshot; drop stale ones.** Whether the runner coalesces before sending,
  or relies on EB keeping only the newest queued `Status` per player, never let a backlog build — a
  backlog makes the panel visibly lag behind the clock. This matters more than the exact interval.
- **Recommended trigger:** emit on each **new depth (iteration completion)**, with a **min interval
  ~500 ms–1 s** (don't spam shallow iterations) and a **heartbeat floor of ~2 s** (so long iterations
  still refresh nodes/NPS). Depth changes are the informative moments — that's when eval/PV jump.
- **Acceptable baseline:** a plain **~2 s coalesced** tick is fine because `Status` is
  non-authoritative; just expect PV/eval to lag the engine's true best line by up to the interval.
- **Do NOT emit one `Status` per UCI info line** — engines produce dozens per second; coalesce to the
  chosen cadence. The final search state is already carried in `BestMove.status`, so no separate
  "last Status" at move time is needed.
- The internal EB runner throttles `Status` to ~500 ms for reference; ~2 s is calmer and ~4× cheaper.

`Status` cadence does **not** affect clock smoothness — EB's local display timer renders the clocks
independently (see `Time`, §3.10).

```jsonc
{
  "type": "Status",
  "playerName": "Engine A",
  "eval": { "kind": "cp", "value": 0.28 },
  "depth": 27,
  "sd": 32,                 // selective depth
  "nodes": 40000000,        // int64
  "nps": 2900000,           // float
  "eps": 0,                 // NN evals/sec (Lc0-style); 0 if N/A
  "tbhits": 0,              // int64
  "wdl": { "win": 450, "draw": 500, "loss": 50 },
  "pv": "c5 Nf3 d6",        // short SAN PV (display)
  "pvLongSAN": "c7c5 g1f3 d7d6 d2d4", // long PV (PV board playback)
  "multiPV": 1
}
```

### 3.8 `PonderStatus` — **Opt**

Lighter update while pondering between moves. Maps to `EnginePonderStatus` (no PV/EPS/MultiPV).

```jsonc
{
  "type": "PonderStatus",
  "playerName": "Engine A",
  "eval": { "kind": "cp", "value": 0.30 },
  "depth": 25, "sd": 30, "nodes": 35000000, "nps": 2800000, "tbhits": 0,
  "wdl": { "win": 440, "draw": 510, "loss": 50 }
}
```

### 3.9 `BestMove` — **Req** (commit a move)

The hot path. Commits one move to the board, updates clocks, and appends to all charts. Carries both
the move (`info`, maps to `BestMoveInfo`) and the final search snapshot (`status`, maps to
`EngineStatus`, same shape as §3.7).

```jsonc
{
  "type": "BestMove",
  "info": {
    "player": "Engine A",
    "move": "e2e4",                 // long algebraic move played
    "ponder": "c7c5",               // optional predicted reply
    "eval": { "kind": "cp", "value": 0.35 },
    "timeLeft": "00:04:45",         // clock remaining AFTER the move
    "moveTime": "00:00:15",         // time spent on this move
    "nodes": 45000000,
    "nps": 3000000,
    "fen": "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
    "pv": "c5 Nf3 d6",
    "longPV": "c7c5 g1f3 d7d6",
    "moveAndFen": { /* moveAndFen, §2.4 */ },
    "moveHistory": "1. e4",         // full running move list (SAN)
    "move50": 0,                    // halfmove clock (50-move rule)
    "r3": 0,                        // repetition count
    "piecesLeft": 32,               // total pieces on board (TB-men display)
    "adjDrawML": 0                  // adjudication draw-move counter
  },
  "status": { /* EngineStatus, same shape as §3.7 */ }
}
```

Notes:
- `moveAndFen` drives the board animation + square highlight; `fen` and `moveHistory` keep the page's
  position and move list authoritative. Send all three consistently.
- Charts (`eval`, `nodes`, `nps`, `eps`, time-per-move) are built **inside EB** from these fields —
  the runner does not send chart series.

### 3.10 `Time` — **Opt**

Fast clock tick outside the per-move cadence. EB also runs its own 100/500 ms display timer, so this
is only needed if you want server-authoritative clocks.

```jsonc
{ "type": "Time", "player": "Engine A", "time": "00:04:30" }
```

### 3.11 `NNSeq` — **Opt** (Lc0/Ceres NN search data) — *requires full legal-move set*

Feeds the live search-info / policy plot and the Q-vs-N convergence chart. Maps to
`ResizeArray<NNValues>`.

```jsonc
{
  "type": "NNSeq",
  "gameId": "0",
  "moves": [
    { "player": "Engine A", "sanMove": "e4", "lanMove": "e2e4",
      "nodes": 1200000, "p": 0.34, "q": 0.18, "v": 0.20, "e": 0.0, "raw": "" },
    { "player": "Engine A", "sanMove": "d4", "lanMove": "d2d4",
      "nodes": 900000,  "p": 0.28, "q": 0.16, "v": 0.18, "e": 0.0, "raw": "" }
    /* … one entry per LEGAL move in the position … */
  ]
}
```

Per-move fields: `p` policy prior, `q` action value, `v` raw value, `nodes` visits, `e` extra/raw
eval, `raw` original engine line.

**Completeness requirement.** `moves` MUST contain **every legal move** in the current position
(or the event MUST be omitted entirely). The policy-distribution and Q-vs-N convergence
visualizations integrate over the whole move set; a truncated top-k silently distorts the
distribution and the convergence curves. Do not send a partial list.

**Engine-class-specific & optional.** Only Lc0/Ceres-style neural engines expose this (verbose
per-move stats — e.g. Lc0 `--show-hidden`/`VerboseMoveStats`, Ceres movestats). Plain UCI engines
cannot produce it, so `NNSeq` is optional and the view degrades gracefully: when no `NNSeq` arrives,
EB hides the NN charts (same gating as the internal `runWithLogLiveStats` path). Producers SHOULD
coalesce — emit at a sensible cadence (e.g. per depth/iteration), never one event per UCI info line,
since full legal-move arrays are large.

### 3.12 `Info` / `MessagesFromEngine` — **Opt**

```jsonc
{ "type": "Info", "player": "Engine A", "info": "string info from engine" }
{ "type": "MessagesFromEngine", "player": "Engine A", "message": "loaded net …" }
```

### 3.13 `PeriodicResults` — **Opt**

Cumulative results pushed periodically so standings refresh mid-tournament without waiting for each
`EndOfGame`.

```jsonc
{ "type": "PeriodicResults", "results": [ /* result[], §2.6 */ ] }
```

### 3.14 `EndOfGame` — **Req**

Records the outcome; EB recomputes standings + crosstable from the accumulated results.

```jsonc
{ "type": "EndOfGame", "result": { /* result, §2.6 */ } }
```

### 3.15 `EndOfTournament` — **Req**

Finalizes standings and opens the final results view.

```jsonc
{ "type": "EndOfTournament", "tournament": { /* Tournament config object */ } }
```

### 3.16 Bracket events — **Out of scope (v0.1)**

`CupBracketUpdated`, `SwissStateUpdated`, `LadderStateUpdated` drive the cup/swiss/ladder dialogs and
require richer state objects (`CupBracket`, `SwissState`, `LadderState`). Deferred to a later version;
round-robin / gauntlet visualization is fully covered without them.

---

## 4. Minimal viable stream

The smallest sequence that produces a working live single-game view:

```
StartOfTournament   (with tournament config)
StartOfGame         (players, opening, clocks, start FEN)
GameStarted
  Status …          (repeated during white's search)
BestMove            (white move committed)
  Status …          (repeated during black's search)
BestMove            (black move committed)
  … repeat …
EndOfGame           (result → standings update)
… next game …
EndOfTournament
```

Add `TotalNumberOfPairs` / `PairingList` / `RoundNr` for the pairings & round displays, and
`Status`/`PonderStatus`/`NNSeq` richness as available.

---

## 5. What EB derives (do NOT send)

The runner sends primitive per-move/per-game facts; EB computes the rest:

- **Standings & crosstable** — from the accumulated `result`s + tournament config.
- **All charts** (eval trend, nodes, NPS, EPS, time-per-move) — from `Status`/`BestMove` fields.
- **PV-agreement / deviation highlighting** — from the two engines' PVs.
- **Opening ECO/name lookup** — `openingName` is used directly; further lookup is internal.
- **Clock display ticking** — EB's own timer animates between your updates.

---

## 6. Validation strategy

Acceptance test for any implementation: **record-and-replay parity.**

1. Add a tap in EB's internal `Update` pipeline that serializes each `Update` to this wire format,
   one event per line (NDJSON), during a normal internal tournament.
2. Feed that NDJSON file back through the external-feed adapter into the new page.
3. The replayed visualization must match the original internal run (board, clocks, charts, standings).

This proves the contract is complete and the adapter is faithful before any real external runner is
involved.

---

## 7. Open questions (resolve during implementation)

- **Eval sign orientation** — confirm whether EB's eval charts expect side-to-move or White-relative
  scores, and document the chosen convention here.
- **Standings without a live runner** — `EndOfGame` currently calls `runner.GetPlayerResults` /
  `runner.GenerateStatsCrosstable`. In feed mode these must be reachable without a live tournament
  runner (expose as pure functions over `result[]` + tournament config).
- **Tournament config minimality** — pin down the smallest `tournament` subset that yields correct
  standings for each `TournamentMode`.
- **Time format** — lock `"HH:mm:ss"` vs `"HH:mm:ss.fff"` (recommend accepting both).

---

## 8. Parallel games (multi-view)

A runner may play **many games concurrently** and stream them all at once, so a spectator can watch
several boards in parallel or focus a single game. This is a v0.2 milestone; v0.1 stays single-game.
The wire format reserves what's needed now so it never has to break.

**Wire (already reserved):** every *per-game* event carries `gameId` (envelope, §1) — a stable
stream key chosen by the runner (e.g. the game number, or a board/worker id). Tournament-level events
stay global. v0.1 producers may omit `gameId`; EB treats absence as one active game.

- A `gameId` identifies a **board/stream**, not a pairing — a given `gameId` runs one game to
  completion (`StartOfGame … EndOfGame`), then may be **reused** for the next game on that board.
  Treat `StartOfGame` as "(re)initialize the view for this `gameId`."
- Per-game events: `StartOfGame`, `GameStarted`, `Status`, `PonderStatus`, `BestMove`, `Time`,
  `NNSeq`, `EndOfGame`.

**Consumer model (v0.2):**

- `JsonFeedService` **demultiplexes by `gameId`** into a registry of per-game view-models, each owning
  its board, clocks, engine panels, PVs, and charts.
- The UI offers a **board grid** of active games plus a **focus selector**; focusing a game routes
  that `gameId`'s stream into the existing single-game render path (the one selectable-source view
  agreed for the internal test), so no rendering logic is duplicated for multi-view.
- Tournament-level events (standings, pairings, rounds) update shared global state regardless of which
  game is focused.

**Open questions for v0.2:**

- Lifecycle/GC of finished `gameId` views (keep last result visible vs. evict on `EndOfGame`).
- Grid scaling and update throttling when many games stream high-frequency `Status`/`NNSeq` at once
  (per-game coalescing + only rendering visible/focused boards live).
- Whether `gameId` should also tag `PeriodicResults` rows for incremental per-board standings.

---

## 9. Transport (external runner)

Everything funnels through one chokepoint — `JsonFeedService.Ingest(jsonLine)` → parse → demux →
views — so a transport is just a thin shim that delivers wire lines to `Ingest`. Implemented sinks:
in-process bridge (internal runner), file record/replay/tail, and **HTTP POST** (below).

### 9.1 HTTP ingest endpoint — `POST /api/livefeed`

The WebGUI hosts `POST /api/livefeed`. The body is **NDJSON** (one wire event per line; batches
allowed). The handler reads each line and calls `JsonFeedService.Ingest`.

- **Batch + flush.** Producers should buffer and flush every ~200 ms (or N events), not one request
  per event — the `Status`/`NNSeq` stream is high-frequency.
- **Auth (optional).** If `EB_LIVEFEED_TOKEN` is set on the WebGUI, requests must send a matching
  `X-Feed-Token` header (open on a trusted LAN; required otherwise).
- **Returns** `{ "ingested": N }`.

### 9.2 Source tagging (multi-server)

The endpoint tags each event with a **source** so games from different servers stay distinct without
the runner having to bake the host into `gameId`:

- Source = `X-Feed-Source` header if the runner sets one (e.g. `rig-A`), else the remote IP
  (`192.168.1.57`). The header wins (survives NAT/proxy).
- EB stamps `source` into the envelope and **namespaces the demux key as `source/gameId`**, so two
  servers both using local slot `1` become distinct tiles. The grid labels each tile `live from
  <source>`. Runners therefore use **local** `gameId`s and need no awareness of other servers.
- Many runners POST to one WebGUI → one unified grid across all servers.

### 9.3 EB Console as a network producer

The internal parallel runner (`ParallelExecution`) emits the feed when env vars are set (no-op
otherwise):

- `EB_LIVEFEED_FILE=<path>` — write NDJSON to a file (record / local tail).
- `EB_LIVEFEED_URL=<url>` — POST NDJSON batches to a WebGUI's `/api/livefeed` (`LiveFeedHttpSink`).
- `EB_LIVEFEED_SOURCE=<name>` — friendly source name sent as `X-Feed-Source` (else the WebGUI uses the IP).
- `EB_LIVEFEED_TOKEN=<token>` — sent as `X-Feed-Token`.

Both sinks can be active at once. This makes EB Console the first external producer over the wire:
run a Console tournament on machine A with `EB_LIVEFEED_URL` pointing at the WebGUI on machine B, open
`/tournament-grid` on B, and the games render live, labeled by source.

### 9.4 WebSocket — optional (later)

For lowest latency, a `ws://host/livefeed` endpoint can stream events (`receive → Ingest`), same
demux/views. Add if HTTP batch latency proves insufficient.

### 9.5 Late-join / catch-up — the main gap

HTTP/WS are live streams with no history, so a grid opened mid-tournament misses earlier events. The
fix is a **server-side state cache in `JsonFeedService`**: keep, per `gameId`, the last `StartOfGame`
+ latest `Status`/board + accumulated results + `StartOfTournament`, and replay that snapshot to a new
subscriber before live events. This is the key robustness item for real multi-server monitoring (and
makes the file-tail's "read from start" unnecessary). Not yet implemented.
