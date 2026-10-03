# Round Robin Mode

In a round robin every engine plays every other engine. A round is one opening: in each round
every pair of engines plays that opening, so the tournament plays `Rounds` openings in all.

## Quick Start

1. Set `TournamentMode` to `RR`
2. List the engines in `EngineSetup.EngineDefList`
3. Set `Rounds` (the number of openings) and point `Opening.OpeningsPath` at a book

## Configuration (tournament.json)

```json
"TournamentMode": "RR",
"Rounds": 50,
"Opening": {
  "OpeningsPath": "C:/Books/book.pgn",
  "OpeningsTwice": true,
  "RandomOpenings": false,
  "Seed": 0
}
```

| Field | Effect in a round robin |
|-------|-------------------------|
| `Rounds` | The number of openings; every pair plays each of them. |
| `OpeningsTwice` | `true`: every pair plays each opening with both colours, the two games one after the other. `false`: each opening once per pair (see *Colours*). |
| `RandomOpenings` | Plays the first `Rounds` openings of the book in a shuffled order, the same order for every pair. `Seed` picks the shuffle. |
| `NumberOfGamesInParallel` | Games played at the same time (see [Tournament configuration](TournamentConfig.md)). |

## How the pairings are made

The pairings follow the Berger system: one engine stays in place and the others rotate, so with N
engines every engine meets every other once in N - 1 rotations. With an odd number of engines one
engine sits out each rotation (a bye), and every engine sits out once.

All rotations are played for each opening before the next opening starts.

The number of games is `Rounds` x N x (N - 1) / 2, twice that with `OpeningsTwice`. For example, 6
engines, 50 rounds and `OpeningsTwice: true` give 50 x 15 x 2 = 1500 games.

## Openings

Round *r* uses the *r*-th opening of the book. A book shorter than `Rounds` starts over from its
first opening; of a longer book only the first `Rounds` openings are used.

## Colours

- With `OpeningsTwice: true` every pair plays each opening once with each colour.
- With `OpeningsTwice: false` the Berger rotation decides who has White, and every other opening
  the colours are swapped, so every engine gets White about as often as Black.

## Resuming

A tournament whose `PgnOutPath` already holds games continues where it stopped: the games already
in the file are matched by opening and colours and not played again.

## See Also

- [Gauntlet](GauntletMode.md) - challengers against a field
- [Tournament configuration](TournamentConfig.md) - every field in tournament.json
