# match - engine matches from the command line

`match` plays a match between UCI engines from a single command line and reports it as it goes:
a line per game, rating reports at an interval, an SPRT that can stop the match, a final summary
and an exit code. It takes the fastchess command line and writes fastchess or cutechess output,
so a script or a testing tool written for those can run EngineBattle instead.

The games are played by EngineBattle's tournament runner - the same one behind the WebGUI's
tournaments - with EngineBattle's scheduling, clock and adjudication, and written to
EngineBattle's PGN.

## Where it is

`eb-cli` (`eb-cli.exe` on Windows) is EngineBattle's console, and it is in every EngineBattle
download next to the WebGUI (`EngineBattle`). In a source checkout it is
`dotnet run --project Console -c Release -- match ...`.

## Quick start

```bash
# a head-to-head: 100 rounds of two games (colours swapped), 10+0.1, four at a time
eb-cli match -engine cmd=./sf-dev name=dev -engine cmd=./sf-base name=base \
    -each tc=10+0.1 option.Threads=1 option.Hash=16 \
    -rounds 100 -concurrency 4 -openings file=UHO.epd order=random -pgnout file=dev-vs-base.pgn

# the same, stopping as soon as an SPRT decides
... -rounds 20000 -sprt elo0=0 elo1=5 alpha=0.05 beta=0.05

# a round robin of three engines at 800 nodes, cutechess-style output
eb-cli match -engine cmd=a.exe -engine cmd=b.exe -engine cmd=c.exe \
    -each nodes=800 -rounds 50 -output format=cutechess
```

`match` can be left out: a command line whose first argument starts with `-` is a match
(`eb-cli -engine ... -each ...`), since no other console command starts with one.

`eb-cli match -help` lists the options (a few rarely used ones are accepted but not listed);
`-version` prints EngineBattle's version and the commit it was built from (`EngineBattle dev
build (abc1234)` from a source build).

## What you see

**stdout** carries the match's report lines only:

```
Started game 1 of 200 (dev vs base)
Finished game 1 (dev vs base): 1-0 {White mates}
...
--------------------------------------------------
Results of dev vs base (10+0.1, 1t, 16MB, UHO.epd):
Elo: 12.30 +/- 20.11, nElo: 20.15 +/- 32.93
LOS: 88.54 %, DrawRatio: 48.00 %, PairsRatio: 1.30
Games: 100, Wins: 32, Losses: 28, Draws: 40, Points: 52.0 (52.00 %)
Ptnml(0-2): [2, 10, 24, 12, 2], WL/DD Ratio: 1.40
LLR: 0.41 (13.9%) (-2.94, 2.94) [0.00, 5.00]
--------------------------------------------------
```

Everything else - EngineBattle's own messages, engine output, warm-up lines - goes to the
`-log file=` file, or nowhere without one. Lines end with the platform's line ending (CRLF on
Windows).

- `-output format=cutechess` gives cutechess's `Score of ...` / `Elo difference: ...` lines.
- With more than two engines the report is a ranking table.
- `-report penta=false` reports W/D/L instead of pentanomial statistics; pentanomial needs
  `-games 2`.
- The statistics - Elo, nElo, LOS, pentanomial, and the SPRT's LLR in the normalized, logistic and
  bayesian models - follow the published formulas digit for digit.

## Stopping and resuming

- **SPRT:** when H0 or H1 is accepted the match stops, the games still running are not written,
  and the exit code is 0. A one-sided result still takes a while with the normalized model (about
  150 game pairs for `elo0=0 elo1=5`) - that is the test, not a slow run.
- **CTRL-C:** the games running are not written; `config.json` is saved and the command to resume
  is printed. Exit code 1.
- **Resume:** `eb-cli match -config file=config.json`, with any flags after it to
  change settings (`-rounds 200` to play on). The games already in the PGN are not played again,
  and the statistics carry on from them. `config.json` is also written at the end and every
  `-autosaveinterval` games (default 20).
- **A new match needs a new PGN file.** EngineBattle resumes from the games already in the
  `-pgnout` file, whatever `append=` says - pointing a new match at an old file continues the old
  one.

## What EngineBattle does its own way

The command line is taken as written; the match is EngineBattle's:

- **Schedule.** Every pair plays the same openings; with `-games 2` both colours of an opening are
  a pair (the pentanomial pairs are matched by opening, whatever order the games finish in).
  With one game per opening the colours alternate. A book shorter than `-rounds` starts over.
  `-srand` gives a reproducible order, not the same order as other tools.
- **Adjudication.** `-draw` and `-resign` set EngineBattle's evaluation adjudication (scores
  converted from centipawns to pawns); its counting rules are its own. `-tb` takes one tablebase
  folder or several (separated by `;`), probed in process (EngineBattle's C# port of Fathom).
- **Clock.** EngineBattle's clock decides time losses. A time loss reports how far the clock went
  below zero: `{White loses on time (37ms overrun)}`. Moves per period (`tc=40/60+0.6`) are each
  engine's own, as in the reference.
- **Time per move.** `st=` sends `go movetime` and runs no clock; a move loses on time when it
  takes longer than `st` plus `timemargin=`, in whole milliseconds from the `go`, as the reference
  counts it. The margin is at least 50 ms, which the reference's is not (its default is 0): an
  engine answers a millisecond or three after its movetime, and a loss on time over that says
  nothing about the engine. EngineBattle has one margin for all engines (its move overhead, not
  sent to an engine playing `st=`): a larger `timemargin=` applies when every engine plays `st=`,
  and the largest wins.
- **PGN.** EngineBattle's format, with its move comments (eval, depth, time, nodes and more); the
  PGN options of `-pgnout` other than `file=` are accepted and not needed. Without `-pgnout` the
  games go to `EngineBattle/match_<yyyyMMdd_HHmmss>.pgn` in the temp folder (`%TEMP%` on
  Windows), and the `-log` file says where.
- **Pondering.** `ponder`, as cutechess-cli writes it (`-each ponder`), lets the engines ponder on
  the opponent's time; EngineBattle sets their UCI `Ponder` option itself. It is all engines or
  none: given to some only, the match does not start. Every engine is started once before the
  match, and one that cannot ponder (no `Ponder` option) stops it with an error, so that all play
  on equal terms. Pondering doubles the search threads in the warning below.
- **Gauntlet.** `-seeds` N engines are the challengers; they do not play each other.
- **Engines** start in their own folder and are reused from game to game. `-startup-ms` is how
  long an engine has to start (default 10000, rounded up to whole seconds).
- **Concurrency.** Fewer games run at once when one copy of each engine, times `-concurrency`,
  does not fit in 70% of the memory (measured on one copy before the match - Lc0 and Ceres grow
  with their network): `Info: Adjusted concurrency to N, as many as fit in 70% of the memory.` More
  search threads than the machine has - games times the largest `option.Threads` - gives a
  warning and the match runs as asked.

Accepted but **not applied yet** (noted in the `-log` file): `timemargin=` with a clock or
nodes or when not every engine plays `st=`, `st=` together with `nodes=` (nodes limit the search), `restart=on`,
`depth=` together with another limit, `tc=` together with `nodes=` (nodes limit the search),
`-maxmoves`, `-openings start=`, `-noswap`, `-reverse`, `-tbignore50`, `-tbadjudicate`, more
than one `-tb` folder, `-epdout`, and the affinity/latency/strict options.

**Refused** with an error, since the games would not be the games asked for: `depth=` as the
only limit.

**cutechess-cli command lines** run as far as the reference (the tool whose command line `match`
takes) runs them, no further. Its command line is written in cutechess-cli's style
(`-engine cmd= name=`, `-each tc= proto=uci ponder`, `-rounds`, `-games`, `-repeat`,
`-openings file= format= order= plies=`, `-draw`, `-resign`, `-sprt`, `-concurrency`, `-event`,
`-site`, `-srand`, `-wait`, `-recover`, `-tournament`), and `-output format=cutechess` prints
cutechess's report lines. cutechess-cli's own forms are errors there and here:
`-pgnout games.pgn` (write `-pgnout file=games.pgn`), `-engine conf=` (an `engines.json`),
`-bookmode`, `-resultformat`.

## Exit codes

| code | when |
|---|---|
| 0 | the match completed, or the SPRT decided; `-help`, `-version` |
| 1 | an error on the command line, an engine that cannot start, CTRL-C |

## How it was checked

- The statistics and SPRT against the reference implementation's own test values, and against
  its code compiled and run on the same counts (equal to 12 significant digits, the printed text
  identical).
- The command line against the reference: 89 error cases print the same text, and 243 of 248
  mistyped options get the same suggestion (the rest are ties broken differently).
- The output: the reference's own games replayed through EngineBattle's report give the same
  text, line for line, in both formats.
- Whole matches: thirteen scenarios run with the same flags through the reference and through
  `match` on the same Stockfish at fixed nodes. The head-to-heads play the same games and print
  the same text - the SPRT runs stop on the same game; the others (adjudication, three engines,
  gauntlet) print the same lines in the same order.
- On Linux as on Windows: the thirteen scenarios again with Linux Stockfish, the head-to-heads
  byte for byte identical; the test suite green three times; CTRL-C (SIGINT) interrupts, saves
  and resumes. A match started in the background of a shell script inherits an ignored SIGINT,
  as any program does - start it with `env --default-signal=INT` to be able to interrupt it.

## For maintainers

- `ChessLibrary/Match/` - `MatchArgs` (the command line), `MatchMapping` (onto an EngineBattle
  tournament), `MatchStats`, `MatchSprt`, `MatchScoreboard`, `MatchOutput`, `MatchConfigJson`,
  `MatchFormat`; `THIRD-PARTY-NOTICE.txt` names the reference and carries its licence.
- `Console/MatchMode.fs` - the console command: stdout, the log, CTRL-C, resume.
- The runner sends `GameFinished` (number, pair label, opening hash, result) and
  `EngineStartFailed` updates for it.
