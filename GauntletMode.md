# Gauntlet Mode

In a gauntlet one or more challengers play a field of opponents. Each challenger plays every
opponent; the challengers do not play each other, and neither do the opponents. It is the usual
way to test a new engine or network against a set of references.

## Quick Start

1. Set `TournamentMode` to `Gauntlet`
2. Set `Challengers` to the number of challengers, and list them first in
   `EngineSetup.EngineDefList` - the first `Challengers` engines are the challengers, the rest
   the opponents
3. Set `Rounds` and point `Opening.OpeningsPath` at a book

## Configuration (tournament.json)

```json
"TournamentMode": "Gauntlet",
"Challengers": 1,
"Rounds": 50,
"Opening": {
  "OpeningsPath": "C:/Books/book.pgn",
  "OpeningsTwice": true,
  "RandomOpenings": false,
  "Seed": 0
}
```

| Field | Effect in a gauntlet |
|-------|----------------------|
| `Challengers` | How many engines, from the top of `EngineDefList`, are challengers. |
| `Rounds` | Rounds; in each, every challenger plays every opponent once (twice with `OpeningsTwice`). |
| `OpeningsTwice` | `true`: each opening with both colours, the two games one after the other. `false`: each opening once (see *Colours*). |
| `RandomOpenings` | Decides how the openings are shared between the opponents (see *Openings*). `Seed` picks the shuffle. |
| `NumberOfGamesInParallel` | Games played at the same time (see [Tournament configuration](TournamentConfig.md)). |

The number of games is `Rounds` x challengers x opponents, twice that with `OpeningsTwice`. For
example, 1 challenger, 8 opponents, 50 rounds and `OpeningsTwice: true` give 800 games.

## Openings

- `RandomOpenings: false` (shared): round *r* uses the *r*-th opening of the book, and every
  opponent plays the same opening in a round. The challenger meets the whole field on the same
  openings, which makes the opponents' results easy to compare.
- `RandomOpenings: true` (spread): with more than one opponent, the first `Rounds` x opponents
  openings of the book are shuffled and each opponent gets openings of its own, none of them
  played against another opponent. A book shorter than that starts over from its first opening,
  so some openings are played against more than one opponent, and a warning is printed at the
  start.

A book shorter than `Rounds` (shared) starts over from its first opening.

## Colours

- With `OpeningsTwice: true` the challenger plays each opening once with each colour.
- With `OpeningsTwice: false` the colours swap from one round to the next, so the challenger has
  White in half the games.

## PreventMoveDeviation

With `PreventMoveDeviation` the order of the opponents rotates from round to round. Combined with
parallel games and shared openings, where the challenger plays each opening with the same colour
against every opponent, the games wait for each other and the run gets close to sequential (see
[Tournament configuration](TournamentConfig.md)).

## Resuming

A tournament whose `PgnOutPath` already holds games continues where it stopped: the games already
in the file are matched by opening and colours and not played again.

## See Also

- [Round robin](RoundRobinMode.md) - every engine against every other
- [Tournament configuration](TournamentConfig.md) - every field in tournament.json
