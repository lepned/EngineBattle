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
{ "type": "<EventType>", /* ...event-specific fields... */ }
```

Unknown `type` values and unknown fields MUST be ignored by EB (forward compatibility).

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

### 3.11 `NNSeq` — **Opt** (Lc0-style per-move NN stats)

Feeds the live search-info / Q-value plot. Only relevant for engines that report per-move policy/Q.

```jsonc
{
  "type": "NNSeq",
  "moves": [
    { "player": "Engine A", "sanMove": "e4", "lanMove": "e2e4",
      "nodes": 1200000, "p": 0.34, "q": 0.18, "v": 0.20, "e": 0.0, "raw": "" }
  ]
}
```

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
