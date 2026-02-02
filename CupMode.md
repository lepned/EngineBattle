# Cup Mode

Cup mode is a single-elimination knockout tournament where players advance by winning head-to-head matches. Each match consists of one or more pairs, where a pair is two games with the same opening (colors swapped).

## Quick Start

1. Set `TournamentMode` to `Cup`
2. Ensure the number of engines is a power of two (4, 8, 16, 32, ...)
3. Configure `CupOptions` as needed

## Configuration (tournament.json)

```json
"CupOptions": {
  "RoundPairIncrements": [1, 2, 3],
  "SeedingStrategy": "ByRating",
  "UniquePerMatchOnly": true,
  "BracketPath": "wwwroot/cup_bracket.json",
  "RandomOpenings": true
}
```

### Field Summary

| Field | Description |
|-------|-------------|
| `RoundPairIncrements` | Pairs per round. Each pair = 2 games. Example: `[1,2,3]` means Round 1 has 2 games, Round 2 has 4, Round 3 has 6. Defaults to `[1]` if empty. |
| `SeedingStrategy` | `ByRating` (seeded bracket) or `Random` (shuffled bracket). |
| `UniquePerMatchOnly` | When true, openings can repeat across matches but not within a match. |
| `BracketPath` | JSON file for bracket state persistence and GUI updates. |
| `RandomOpenings` | Randomize opening order; the shuffled order is persisted for resume. |

## Bracket Structure

### Player Count Requirement

The number of players must be a power of two (4, 8, 16, 32, ...). First-round byes are not currently supported.

### Round Calculation

For N players, there are log2(N) rounds:
- 4 players: 2 rounds (Semifinal, Final)
- 8 players: 3 rounds (Quarterfinal, Semifinal, Final)
- 16 players: 4 rounds

### Match Indexing

Matches within each round are indexed 0 to (matchCount-1). Winners advance as follows:
- Match indices 0 and 1 → next round match 0
- Match indices 2 and 3 → next round match 1
- General formula: `nextMatchIndex = currentMatchIndex / 2`

The winner of an even-indexed match becomes PlayerA in the next round; odd-indexed becomes PlayerB.

## Seeding Strategies

### ByRating (Default)

Uses band-based seeding to create a fair bracket:

1. Sort engines by rating (descending).
2. Divide into seeding bands based on bracket size.
3. Place seeds so that top seeds meet only in later rounds.

For an 8-player bracket:
- Seed 1 vs Seed 8, Seed 4 vs Seed 5 (one half)
- Seed 2 vs Seed 7, Seed 3 vs Seed 6 (other half)

This ensures Seed 1 and Seed 2 can only meet in the final.

### Random

Engines are shuffled randomly. The shuffled order is persisted for resume consistency.

## Match Play

### Pairs and Games

- Each match consists of one or more pairs.
- Each pair uses the same opening twice (colors swapped).
- The higher-seeded player gets White in game 1 of each pair.

### Early Termination

A match ends early if a winner is mathematically decided before all scheduled pairs are played. For example, in a 3-pair match (6 games), if one player leads 4-0 after 4 games, the remaining games are skipped.

### Tiebreaks

When a match is tied after scheduled pairs, additional tiebreak pairs are played until a winner is determined. Tiebreak pairs use new openings when available.

## Opening Selection

- Each pair plays the same opening twice with colors swapped.
- When `RandomOpenings` is true, the global opening order is shuffled once and persisted.
- When `UniquePerMatchOnly` is true:
  - Openings can repeat across different matches
  - Openings cannot repeat within the same match
- When `UniquePerMatchOnly` is false:
  - Each opening is used only once across the entire tournament

## State Persistence

Bracket state is saved to `BracketPath` (JSON) after each game, including:
- Tournament name and settings
- All rounds with match details
- Per-match scores, winner, and game results
- Global opening index and order (for resume consistency)

### Resume Behavior

To resume a cup tournament:
1. Ensure the bracket file exists at `BracketPath`
2. The bracket state is the source of truth
3. Completed matches are skipped; in-progress matches continue from where they left off

## Files Written

| File | Content |
|------|---------|
| `cup_bracket.json` | Current bracket state, scores, and match results |
| PGN file | Game records (configured separately) |

## UI Integration

- Visual bracket display showing advancement
- Live score updates during matches
- Match details with individual game results
