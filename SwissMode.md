# Swiss Mode

Swiss mode runs a fixed number of rounds where players with similar scores are paired together. Each pairing is played as a two-game match (same opening, colors swapped).

## Quick Start

1. Set `TournamentMode` to `Swiss`
2. Ensure the number of rounds is less than the number of players (to avoid forced rematches)
3. Configure `SwissOptions` as needed

## Configuration (tournament.json)

```json
"SwissOptions": {
  "GamesPerMatch": 2,
  "Rounds": 5,
  "SeedGroupCount": 1,
  "UniquePerMatchOnly": true,
  "RandomOpenings": true,
  "AllowExtraPairsOnTie": true,
  "StatePath": "wwwroot/swiss_state.json"
}
```

### Field Summary

| Field | Description |
|-------|-------------|
| `GamesPerMatch` | Games per pairing (must be even; each pair is two games with colors swapped). |
| `Rounds` | Total Swiss rounds. Uses global `Rounds` when set to 0. Must be less than player count to avoid forced rematches. |
| `SeedGroupCount` | Number of seeding groups for TCEC-style interleaving (see below). |
| `UniquePerMatchOnly` | When true, openings can repeat across matches but not within a match. |
| `RandomOpenings` | Randomize opening order; the shuffled order is persisted for resume. |
| `AllowExtraPairsOnTie` | Play extra pairs to break ties at the top after scheduled rounds. |
| `StatePath` | JSON file for state persistence and GUI updates. |

## Seeding

Default seeding uses TCEC-style grouping and interleaving:

1. Sort engines by rating (descending).
2. Split into N groups of roughly equal size.
3. Interleave by rank: A1, B1, C1, ... then A2, B2, C2, ...

### SeedGroupCount Examples

| Players | Recommended | Result |
|---------|-------------|--------|
| 8 | 1 | A1, A2, A3, A4, A5, A6, A7, A8 |
| 12 | 2 | A1, B1, A2, B2, A3, B3, ... |
| 24 | 4 | A1, B1, C1, D1, A2, B2, C2, D2, ... |

Use fewer groups for conservative seeding, more groups to spread top seeds further apart.

## Pairing Algorithm

The pairing algorithm follows standard Swiss principles with two code paths:

### Main Path (Grouped Pairing)

1. Group players by current score (highest to lowest).
2. Sort within each group by seed (strongest first).
3. Split each group into top-half and bottom-half.
4. Pair top-half vs bottom-half (1st vs middle, 2nd vs middle+1, etc.).
5. If a group has odd count, float one player down to the next score group.
6. Backtrack if a pairing would cause a rematch.

### Fallback Path

When the main path fails due to complex prior-pairing constraints (common in later rounds), a fallback algorithm attempts global pairing:

1. Order all players by score (descending), then by seed.
2. Greedily pair each player with the closest-scoring available opponent.
3. Backtrack if needed to avoid rematches.

Both paths produce pairings sorted so that **weakest score groups play first** and strongest last.

## Bye Selection

When there is an odd number of players, one player receives a bye (worth 1 point). The bye recipient is selected as follows:

1. **Lowest score first**: Players with fewer points are prioritized.
2. **Weakest among ties**: If multiple players share the lowest score, the one with the highest seed number (lowest rating) receives the bye.
3. **No repeat byes**: Players who have already received a bye are skipped when possible.

This ensures byes go to trailing engines, and among equals, to the lower-rated one.

## Game Execution Order

Each round executes pairings from weakest to strongest score groups. This follows standard Swiss convention:
- Weaker matchups typically finish faster
- Top games are saved for last (more exciting for spectators)
- Reduces overall round duration

## Opening Selection

- Each match uses one opening played twice (colors swapped for the second game).
- When `RandomOpenings` is true, the opening order is shuffled once and persisted.
- When `UniquePerMatchOnly` is true, openings can repeat across different matches but not within a single match.

## Tie-Breakers (TCEC-style)

Winner determination:
- Points only. If tied after scheduled rounds, play extra pairs (if `AllowExtraPairsOnTie` is true).
- For two tied engines: play additional pairs until one wins a pair.
- For three or more tied: use tie-break criteria.

Tie-break order for non-winner placements:
1. Sonneborn-Berger score
2. Number of wins
3. Direct encounter result
4. If still tied, share places (unless it affects promotion/relegation)

## State Persistence

Swiss state is saved to `StatePath` (JSON) after each game, including:
- Current round number and global opening index
- All pairings per round with results
- Standings snapshot per round
- Opening order (for resume consistency)

To resume a tournament, ensure the state file exists at the configured path.

## UI Integration

- Standings table with live score updates
- Current round pairings list
- Swiss overview dialog between games
