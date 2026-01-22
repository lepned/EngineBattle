# Cup Mode

Cup mode is a knockout tournament where players advance by winning head-to-head matches.
Each match is played as a set of pairs: a pair is two games, one with each color.

## Quick start

1) Set `TournamentMode` to `Cup`
2) Ensure the number of engines is a power of two (4, 8, 16, 32, ...)
3) Configure `CupOptions` as needed

## Key behavior

- No byes are allowed.
- A match ends early if a winner is mathematically decided.
- When a match is tied after its scheduled pairs, tiebreak pairs are played until a winner is found.
- Openings can be sequential or randomized and can be unique globally or per match.

## CupOptions

```
"CupOptions": {
  "RoundPairIncrements": [1, 2, 3],
  "SeedingStrategy": "ByRating",
  "UniquePerMatchOnly": true,
  "BracketPath": "wwwroot/cup_bracket.json",
  "RandomOpenings": true
}
```

Field summary:

- `RoundPairIncrements`: Pairs per round. Each pair is two games. Example: `[1,2,3]` means 2,4,6 games. If empty, defaults to one pair (2 games).
- `SeedingStrategy`: `ByRating` or `Random`.
- `UniquePerMatchOnly`: `true` means openings can repeat across matches, but not within a match.
- `BracketPath`: Where the bracket JSON is written/read.
- `RandomOpenings`: Randomize opening order and persist the order for resume.

## Openings and fairness

- A pair always plays the same opening twice, once with each color.
- For tiebreaks, new openings are chosen when available.

## Resume behavior

If a bracket file exists at `BracketPath`, you can resume a cup run.
The bracket state is the source of truth for resume.

## Files written during cup mode

- `cup_bracket.json`: Current bracket state and scores.
