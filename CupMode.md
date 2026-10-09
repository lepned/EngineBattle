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
  "RandomOpenings": true
}
```

### Field Summary

| Field | Description |
|-------|-------------|
| `RoundPairIncrements` | Pairs per round. Each pair = 2 games. Example: `[1,2,3]` means Round 1 has 2 games, Round 2 has 4, Round 3 has 6. Rounds beyond the list reuse its last value, and an entry below 1 counts as 1. Defaults to `[1]` if empty. |
| `SeedingStrategy` | `ByRating` (seeded bracket) or `Random` (shuffled bracket). The shuffle comes from `Opening.Seed`, so the same config gives the same draw. |
| `UniquePerMatchOnly` | When true, openings can repeat across matches but not within a match. |
| `BracketPath` | The bracket state file (resume and GUI). Optional; leave it out. By default the file sits next to the tournament's PGN, named after it (`MyCup.pgn` → `MyCup_cup_bracket.json`), so every tournament keeps its own. Set it only to keep the file somewhere else. The old shared default `wwwroot/cup_bracket.json` counts as not set. A tournament without a PGN keeps its state in `wwwroot/cup_bracket.json`. |
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

Seeds are placed in bands - seed 1, seed 2, seeds 3-4, seeds 5-8, and so on - with the order inside each band shuffled (from `Opening.Seed`, so it repeats for the same config). For an 8-player bracket seed 1 and seed 2 sit at opposite ends and meet at the earliest in the final, one of seeds 3-4 lands in each half, and each top seed meets a random one of seeds 5-8 in round 1.

This ensures Seed 1 and Seed 2 can only meet in the final.

### Random

Engines are shuffled randomly. The shuffled order is persisted for resume consistency.

## Match Play

### Pairs and Games

- Each match consists of one or more pairs.
- Each pair uses the same opening twice (colors swapped).
- The player listed first in the match gets White in game 1 of each pair: the higher seed in round 1, and from round 2 the winner arriving from the upper of the two feeding matches.

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
  - Each opening is used only once across the entire tournament: every pair takes the next opening of the tournament's order (the book order, or the shuffled order with `RandomOpenings`), tiebreak pairs included
  - When all openings have been used, the book starts again from its first opening (logged)

## State Persistence

Bracket state is saved to the bracket file (next to the PGN unless `BracketPath` is set) after each game, including:
- Tournament name and settings
- All rounds with match details
- Per-match scores, winner, and game results
- Global opening index and order (for resume consistency)

### Resume Behavior

To resume a cup tournament:
1. Run the same tournament again: its bracket file (next to the PGN unless `BracketPath` is set) is found
2. The bracket state is the source of truth
3. Completed matches are skipped; in-progress matches continue from where they left off

Delete the PGN to start the tournament over: a state file that records games its PGN does not have is set aside as `.bak`. A tournament paused in the old shared file under `wwwroot` is taken over from it once, when it holds this tournament (same name, its engines); the old file is then renamed `.migrated`, so a later Restart is not undone by it.

A cup, Swiss or ladder with no state file to resume from will not start in a PGN that already has games, since that would put two tournaments into one file: the GUI asks first (add to the PGN, or cancel and set another PgnOutPath), the console stops and says so (`tournamentjson <file> --append` adds to the PGN anyway). A state file that cannot be read - cut short by a crash, say - is set aside as `.corrupt` rather than overwritten, and the same question follows.

## Files Written

| File | Content |
|------|---------|
| `<PGN name>_cup_bracket.json` | Current bracket state, scores, and match results |
| PGN file | Game records (configured separately). Each game's `Round` is `round.n`, numbered in the order played across the round's matches, tiebreak games included: if match 1 takes games 1.1-1.4, match 2 starts at 1.5. |

## UI Integration

- Visual bracket display showing advancement
- Live score updates during matches
- Match details with individual game results
