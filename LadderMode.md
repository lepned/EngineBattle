# Ladder Mode

Ladder mode is an elimination-style climbing tournament. Engines are ranked by rating and the lowest-ranked survivor repeatedly challenges the engine above it. The loser of each mini-match is eliminated. The tournament ends when only one engine remains.

## Quick Start

1. Set `TournamentMode` to `Ladder`
2. Add at least 2 engines (no upper limit)
3. Configure `LadderOptions` as needed

## Configuration (tournament.json)

```json
"LadderOptions": {
  "GamePairsPerMatch": 4,
  "RandomOpenings": true,
  "StatePath": "wwwroot/ladder_state.json"
}
```

### Field Summary

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `GamePairsPerMatch` | int | 4 | Game pairs per mini-match. Each pair is 2 games with reversed colors, so total games per match = value x 2. Minimum 1. |
| `RandomOpenings` | bool | false | Randomize opening order. The shuffled order is created once and persisted for resume consistency. |
| `StatePath` | string | `wwwroot/ladder_state.json` | Path for state persistence. Relative or absolute. Parent directories are created if missing. |

## How It Works

### Initial Ranking

Engines are sorted by rating (highest = rank 1, lowest = last). This determines the initial ladder order.

### The Climb

1. The lowest-ranked surviving engine becomes the **challenger**.
2. The challenger plays a mini-match against the engine directly above it (the **defender**).
3. The loser is eliminated and the winner stays on the ladder.
4. If the challenger wins, it moves up and challenges the next engine above.
5. If the challenger loses, it is eliminated and a new climb starts from the new bottom engine.
6. This continues until only one engine remains — the **champion**.

### Mini-Match Structure

Each mini-match consists of `GamePairsPerMatch` pairs of games:

- Each pair uses the same opening played twice with colors swapped.
- The challenger gets White in game 1 of each pair.
- Total games per match = `GamePairsPerMatch x 2` (default: 8 games).

### Early Termination

A mini-match ends early when the outcome is mathematically decided. After each game, if one engine's score exceeds the opponent's score plus the number of remaining games, the match is called. For example, in a 4-pair match (8 games), if the challenger leads 5-1 after 6 games, the remaining 2 games are skipped.

### Tie-Breaking

If the mini-match score is level after all scheduled games, **extra game pairs** are played until one engine leads. Each tiebreak round adds 2 games (one pair with reversed colors) using a fresh opening. This continues until the tie is broken — there is no limit on tiebreak rounds.

### Climbs

A "climb" is one full attempt by a bottom engine to ascend the ladder. A new climb starts when:

- The current climber loses (eliminated) — the new bottom engine starts climbing.
- The climber reaches rank 1 and beats the top engine — the climber is champion, but if other engines remain, a new climb starts from the new bottom.

The climb number increments with each new attempt. Match identifiers include the climb number (e.g., `2.3` = climb 2, game 3).

## Opening Selection

- Each game pair plays the same opening twice with colors swapped.
- When `RandomOpenings` is true, the opening order is shuffled once at tournament start and persisted.
- When `RandomOpenings` is false, openings are used sequentially from the book.
- If the opening book is exhausted, it wraps around to the beginning.

## State Persistence

Ladder state is saved to `StatePath` (JSON) after every game, including:

- Initial rankings and current surviving/eliminated engines
- Current climb number and climber position
- Full match history with individual game results
- Opening index and global opening order (for resume consistency)

### Resume Behavior

When starting a ladder tournament and a state file exists, a dialog is shown with:

- **Resume** — continue from where the tournament left off (disabled if the ladder is completed or the state file is unreadable).
- **Start New** — back up the existing state file (`.bak`) and start a fresh ladder.
- **Cancel** — abort without starting.

The dialog includes a preview of the current ladder standings (ranked table of surviving and eliminated engines).

If no state file exists, the tournament starts fresh with no dialog.

When resuming:

1. The state file is the source of truth for progress.
2. Completed matches are skipped; an in-progress match continues from the last completed game.
3. Opening order and index are restored so no openings are repeated or skipped.

## Tournament Length

For N engines, the tournament plays exactly **N - 1** mini-matches (each match eliminates one engine). The minimum games per match is `GamePairsPerMatch x 2`, though tiebreaks can extend individual matches. Early termination typically reduces total games played.

| Engines | Mini-Matches | Max Games (4 pairs) |
|---------|-------------|---------------------|
| 4 | 3 | 24 |
| 8 | 7 | 56 |
| 16 | 15 | 120 |
| 32 | 31 | 248 |

## Final Standings

The champion is the last engine standing. Remaining placements are determined by elimination order — the last engine eliminated places 2nd, second-to-last places 3rd, and so on.

## Example

With 5 engines ranked by rating: A (3800), B (3700), C (3600), D (3500), E (3400):

```
Climb 1: E challenges D
  → E wins  → D eliminated (5th place)
  → E challenges C
    → C wins → E eliminated (4th place)

Climb 2: C challenges B (new bottom after E eliminated is C)
  Wait — after E is eliminated, surviving = [A, B, C]
  New climber = C (bottom)
  → C challenges B
    → C wins → B eliminated (3rd place)
    → C challenges A
      → A wins → C eliminated (2nd place)
      → A is champion (1st place)
```

## State File Format (ladder_state.json)

```json
{
  "TournamentName": "My Ladder",
  "GamePairsPerMatch": 8,
  "InitialRankings": ["EngineA", "EngineB", "EngineC"],
  "SurvivingEngines": ["EngineA", "EngineB"],
  "EliminatedEngines": ["EngineC"],
  "CurrentClimbNumber": 2,
  "CurrentClimberIndex": 1,
  "Matches": [
    {
      "MatchId": 1,
      "ClimbNumber": 1,
      "Challenger": "EngineC",
      "Defender": "EngineB",
      "ChallengerRating": 3600,
      "DefenderRating": 3700,
      "ScoreChallenger": 2.5,
      "ScoreDefender": 5.5,
      "Winner": "EngineB",
      "IsDecided": true,
      "Games": [
        {
          "GameNr": 1,
          "White": "EngineC",
          "Black": "EngineB",
          "OpeningId": "42",
          "OpeningHash": "abc123...",
          "Result": "0-1"
        }
      ]
    }
  ],
  "NextOpeningIndex": 5,
  "GlobalOpeningOrder": [3, 0, 7, 1, 5, 2, 6, 4],
  "UpdatedUtc": "2026-02-18T12:00:00Z"
}
```

### Key Fields

| Field | Description |
|-------|-------------|
| `InitialRankings` | Original engine order by rating (immutable). |
| `SurvivingEngines` | Engines still in the tournament, ordered by rank (index 0 = top). |
| `EliminatedEngines` | Engines in order of elimination (first eliminated = index 0). |
| `CurrentClimbNumber` | Which climb attempt is in progress. |
| `CurrentClimberIndex` | Index into `SurvivingEngines` of the current climber. |
| `NextOpeningIndex` | Position in the opening sequence for the next game pair. |
| `GlobalOpeningOrder` | Shuffled opening indices (empty if `RandomOpenings` is false). |

## UI Integration

- Ladder progress table showing rank, engine, rating, status, climb info, and match scores
- Live updates during matches via `LadderStateUpdated` events
- Cycling view alternates between ladder progress and standings tables
- Current players (white/black) are highlighted in the progress table
- Between-games overlay shows the ladder standings dialog (same behavior as Cup and Swiss modes, controlled by `ShowCrosstableBetweenGames` in layout options)
- `LadderOverviewFont` in layout `Fonts` controls the font size of the ladder overview dialog
- Cross-table is not shown for Ladder mode (not applicable to elimination format)
