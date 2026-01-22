# Swiss Mode

Swiss mode runs a fixed number of rounds and supports byes for odd player counts. Each pairing is played as a two-game pair (colors swapped).

## Configuration (tournament.json)

```
"SwissOptions": {
  "GamesPerMatch": 2,
  "Rounds": 0,
  "SeedGroupCount": 1,
  "UniquePerMatchOnly": true,
  "RandomOpenings": true,
  "AllowExtraPairsOnTie": true,
  "StatePath": "wwwroot/swiss_state.json"
}
```

Notes:

- `Rounds` uses the global tournament `Rounds` value when set to `0`. Repeat pairings are not allowed, so rounds must be less than the number of players.
- If there is an odd number of players, one player receives a bye each round (worth 1 point); byes are not repeated when possible.
- `GamesPerMatch` should be even; each pair is two games (white/black).
- `SeedGroupCount` controls TCEC-style seeding groups (A/B/C/...).

SwissOptions field summary:

- `GamesPerMatch`: Number of games per pairing (use even numbers; each pair is two games).
- `Rounds`: Total number of swiss rounds to play (must be less than number of players).
- `SeedGroupCount`: Number of seeding groups for TCEC-style interleaving.
- `UniquePerMatchOnly`: Allow reusing openings across matches; still unique within a match.
- `RandomOpenings`: Randomize opening order; persisted for resume.
- `AllowExtraPairsOnTie`: If the top score is tied after scheduled rounds, play extra pairs until resolved.
- `StatePath`: JSON state file used for resume and GUI updates.

## SeedGroupCount examples

These examples assume engines are sorted by rating and then split into groups.

- 8 players (recommend `SeedGroupCount = 1`):
  - Order: A1, A2, A3, A4, A5, A6, A7, A8.
  - Round 1 pairings (top-half vs bottom-half): A1–A5, A2–A6, A3–A7, A4–A8.
- 12 players (recommend `SeedGroupCount = 2`):
  - Groups of 6 (A1..A6, B1..B6), seeding order: A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6.
- 24 players (recommend `SeedGroupCount = 4`):
  - Groups of 6 (A1..A6, B1..B6, C1..C6, D1..D6), seeding order: A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3, A4, B4, C4, D4, A5, B5, C5, D5, A6, B6, C6, D6.

### Recommended SeedGroupCount

Use fewer groups for more conservative seeding, more groups to spread top seeds further.

- Fewer than 12 players: recommend `SeedGroupCount = 1`.
- Fewer than 24 players: recommend `SeedGroupCount = 2`.
- 24 players or more: recommend `SeedGroupCount = 4`.

## Seeding

Default seeding uses TCEC-style grouping and interleaving:

- Sort engines by rating (desc).
- Split into N groups of roughly equal size.
- Interleave by rank: A1, B1, C1, ... then A2, B2, C2, ...

## Pairing rules

- Group players by score (highest to lowest).
- Sort within each group by rating (desc).
- Pair top-half vs bottom-half within the same score group.
- If a group has an odd count, float one player down (lowest-rated in the group).
- No repeat pairings. If a repeat occurs, swap within the group; if unresolved, float a different player.
- If repeats cannot be resolved inside a group, allow a single cross-group pairing.
- Generate the full round pairing list at round start and publish it to the GUI.
- Each round starts with the weakest pairs first.

## Color in pair (game 1)

Follow TCEC: higher seed gets White in game 1 of the pair, then colors flip for game 2.

## Openings

Same opening strategy as cup mode:

- Each pair uses one opening twice (colors swapped).
- Unique-per-match behavior matches cup rules.
- Randomization controlled by `RandomOpenings`.

## Tie-breakers (TCEC-style)

- Winner is decided by points only. If needed, play extra pairs.
- Controlled by `SwissOptions.AllowExtraPairsOnTie`.
- If two engines are tied, play additional pairs until a decisive pair occurs.
- If three or more are tied, tiebreaks are used to resolve the winner.
- For non-winner placements, use tie-break order:
  1) Sonneborn-Berger
  2) Number of wins
  3) Direct encounter
  4) If still tied, share places unless it affects promotion/relegation.

## State persistence

Swiss state is persisted in a JSON file (`StatePath`), including:

- Current round number
- Pairings per round
- Results per game
- Standings snapshot per round

## UI

- Standings table similar to round-robin standings.
- Pairings list for the current round.
- Swiss overview dialog between games.
