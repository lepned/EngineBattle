# Console Tools (eb-cli)

`eb-cli` is EngineBattle on the command line: engine analysis and comparison, tablebase lookups,
PGN tools, puzzle tests, book evaluation, tournaments, tuning, and engine matches.

## Getting it

### In the download

Every EngineBattle download has the console next to the WebGUI, as `eb-cli` (`eb-cli.exe` on
Windows). Run without arguments, it lists the commands. `gui` needs a source checkout; in the
download, start `EngineBattle` instead.

### From source

Needs the [.NET 10.0 SDK](https://dotnet.microsoft.com/download/dotnet/10.0) or later.

```bash
git clone https://github.com/lepned/EngineBattle.git
cd EngineBattle
dotnet run --project Console -c Release -- <command> [arguments]
```

### Build eb-cli yourself

One executable, as in the download, from the root of the clone:

```bash
dotnet publish Console -c Release -r win-x64 --no-self-contained -p:PublishSingleFile=true -p:DebugType=none -o publish/eb-cli
mv publish/eb-cli/EngineBattle.Console.exe publish/eb-cli/eb-cli.exe
# Linux/macOS: mv publish/eb-cli/EngineBattle.Console publish/eb-cli/eb-cli
```

- `-r` is required for a single file: `win-x64`, `win-arm64`, `linux-x64`, `osx-x64` or `osx-arm64`.
- `--no-self-contained` gives a file of about 13 MB that needs the .NET 10 runtime;
  `--self-contained` bundles the runtime in a much larger file.
- The file is all there is: tablebases are probed in process, so nothing has to sit next to it.

## How the commands work

The same rules hold for every command (`match` excepted - see below).

- **Shape:** `eb-cli <command> [<subcommand>] <subject> [options]`. The subject - the engine, the
  position, the PGN, the config - is a positional argument; everything else is a named option.
  Commands are grouped: `pgn parse`, `engine new`, `puzzle run`.
- **Options** are `--kebab-case` and case-sensitive; only `-h` (help) and `-o` (output) have a
  short form. `--name value` and `--name=value` are the same, as are `-o FILE` and `-o=FILE`.
  `--` ends the options.
- **Mistakes** are said, with a suggestion, and exit 2: `eb-cli analyse` gives
  `'analyse' is not a command (did you mean 'analyze'?)`.
- **Help:** `eb-cli` or `eb-cli help` lists the commands; `eb-cli help <command>` or
  `<command> --help` shows one command's arguments, options with their defaults, and examples.
  The help is made from the same specification the parser reads, so it is always what the
  program does; the reference below is that help. `eb-cli version` (or `--version`) names the
  build, `eb-cli version --json` the same as JSON.
- **Engines:** an engine definition (`.json`, with its network, backend, arguments and options), an engine
  executable, or the name of a def in `engineDefsFolder` ([eb-cli.json](#eb-clijson)).
  `--uci NAME=VALUE` sets a UCI option (repeatable), `--args "..."` replaces the def's arguments.
- **Positions:** a FEN - quoted, or its six fields as they come - or `startpos`.
  `--moves "e4 e5 Nf3"` plays moves first, in SAN or UCI.
- **Search limits:** at most one of `--nodes N`, `--movetime MS`, `--depth N`; each command shows
  its default.
- **Evals** are in pawns from White's view (0.35), as engines report them, unless an option says
  centipawns.
- **Files written:** `-o, --out FILE` is always a file and `--out-dir DIR` always a folder. A file
  that exists is not written over without `--force`.
- **Output:** the result goes to stdout; progress, warnings and errors go to stderr. `--json`
  prints the result as one JSON document on stdout, and nothing else goes there:

  ```json
  { "command": "pgn validate", "ok": true, "ebVersion": "1.10.1", "result": { },
    "errors": [ { "code": "invalid-games", "message": "..." } ] }
  ```

  `result` has each command's own fields; `puzzle run`'s is the document of
  [Console/PuzzleJsonSchema.md](Console/PuzzleJsonSchema.md).
- **Exit codes:**

  | Code | Meaning |
  |---|---|
  | 0 | success |
  | 1 | the command ran and failed, or found problems (a check failed, no tablebase answer, invalid games, an engine died) |
  | 2 | usage: an unknown command or option, a bad value, a missing input file, an invalid config |
  | 130 | interrupted with Ctrl+C |

- **Long runs** (tournament, puzzle and ERET tests, book evaluation, piece-values batch): the first
  Ctrl+C stops the run and keeps what it has (finished games, positions that passed); a second
  stops at once. A tournament goes on from the games already in its PGN.
- **EB_DEBUG=1** prints the stack trace of an unexpected failure.

### eb-cli.json

Optional settings next to the executable (next to `eb-cli.exe`; `Console/bin/Release/net10.0/`
from source). They save typing; none of them changes what a command computes.

```json
{
  "engineDefsFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "syzygyPath": "D:/syzygy",
  "puzzleResultsFolder": "C:/Dev/Chess/EB"
}
```

| Key | Used by | What it does |
|---|---|---|
| `engineDefsFolder` | every command that takes an engine | a bare name (`eb-cli analyze Lc0_BT4_332`) is looked up here as `<name>.json`, then `<name>` |
| `syzygyPath` | `tablebase` | the tables when neither `--syzygy` nor `EB_SYZYGY_PATH` is given |
| `puzzleResultsFolder` | `puzzle trend` | the folder when none is given |

Relative folders are relative to the file; keys may be in any case; an unknown key is reported.

### match

`eb-cli match ...` (or a first argument that starts with `-`) plays an engine match with
fastchess's command line - `-engine cmd=... name=...`, `-each tc=10+0.1`, `-rounds`, `-sprt` - and
its output. It keeps fastchess's own conventions and its `config.json`; see
[MatchMode.md](MatchMode.md) and `eb-cli match -help`.

## Commands

```text
Usage: eb-cli <command> [arguments] [options]

Engines and positions
  analyze <engine>                Analyze a position with an engine
  compare <engine1> <engine2>     Compare two engines on one or more positions
  position                        Facts about a position: status, legal moves, attacks, pins, insights
  tablebase <fen>                 Every move of a position from the Syzygy tables
  piece-values position <engine>  Piece values from an engine's evaluations in one position
  piece-values fit <positions>    Piece values fitted over many positions
  piece-values batch <tournament> <nets>
                                  Piece values for a folder of nets

Engine setup and testing
  engine check <engine>           Check that an engine follows the UCI protocol
  engine new <engine>             Create an engine definition (.json) from an executable, or copy one with another net
  engine bench <config>           Measure engine speed (NPS) across option sets, from a benchmark config

PGN files
  pgn parse <pgn>                 Parse every game: counts, empty games, parse speed
  pgn validate <pgn>              Replay every game: illegal or ambiguous moves, non-standard SAN
  pgn strip <pgn>                 Write the games without comments, variations or NAGs
  pgn endings <pgn>               How the games ended: missed wins, repetition, 50-move rule, stalemate...
  pgn elo <pgn>                   Ratings and results
  pgn speed <pgn>                 Speed statistics per engine
  pgn deviations <pgn>            Positions reached in several games, and the moves each engine chose there
  pgn imbalance <pgn>             Results by material imbalance

Puzzles, books, tournaments and tuning
  book eval <book>                Filter an opening book down to balanced openings, checked by engines
  puzzle run <config>             Run a Lichess puzzle test from a config
  puzzle eret <config>            Run an ERET test suite (EPD best-move positions) from a config
  puzzle trend                    Puzzle scores across a net's training checkpoints, from saved runs
  tournament run <config>         Run a tournament from a config
  tournament validate <config>    Check a tournament config without running it
  tune run <config>               Tune engine parameters (Bayesian optimisation)
  tune dashboard <config>         Write the tuning dashboard again from the saved state

Other
  perft <depth>                   Count the move tree on the 960 Chess960 test positions
  gui                             Start the WebGUI of a source checkout
```
## Command reference

### analyze

```text
Analyze a position with an engine

Usage: eb-cli analyze <engine> [<fen>] [options]

Searches the position once: the engine's info lines as they come on stderr, the summary on stdout (eval and WDL from White's view). A checkmate or stalemate is reported without a search. With --json, only the result, on stdout.

Arguments:
  <engine>  an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)
  [<fen>]   a FEN (quoted, or its six fields as they come) or startpos (default startpos)

Options:
      --moves "MOVES"   Moves played from the position first, SAN or UCI ("e4 e5 Nf3" or "e2e4 e7e5")
      --nodes N         Search this many nodes (default 1000000)
      --movetime MS     Search this many milliseconds
      --depth N         Search to this depth
      --uci NAME=VALUE  Set a UCI option of the engine (repeatable)
      --args "..."      The engine's command-line arguments, instead of the def's
      --list-options    List the engine's UCI options and their defaults instead of searching
      --json            Print the result as JSON on stdout (everything else goes to stderr)
  At most one of --nodes, --movetime, --depth.

Examples:
  eb-cli analyze Stockfish19
      Search the start position, 1,000,000 nodes
  eb-cli analyze lc0.json "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1" --movetime 2000
      Two seconds on a position
  eb-cli analyze sf.exe --moves "e4 e5 Nf3" --depth 20 --uci Threads=8
      After three moves, to depth 20, with 8 threads
  eb-cli analyze lc0.json --list-options
      The engine's UCI options
```
### compare

```text
Compare two engines on one or more positions

Usage: eb-cli compare <engine1> <engine2> [<fen>] [options]

Both engines search each position with the same limit; a table of evals, speed and best moves, then a summary. The position is the start position, a FEN (with --moves played from it), or every position of --positions.

Arguments:
  <engine1>  an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)
  <engine2>  an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)
  [<fen>]    a FEN (quoted, or its six fields as they come) or startpos (default startpos)

Options:
      --moves "MOVES"    Moves played from the position first, SAN or UCI ("e4 e5 Nf3" or "e2e4 e7e5")
      --positions FILE   An EPD file: every position in it, instead of one
      --nodes N          Search this many nodes (default 1000000)
      --movetime MS      Search this many milliseconds
      --depth N          Search to this depth
      --uci1 NAME=VALUE  Set a UCI option of engine 1 (repeatable)
      --uci2 NAME=VALUE  Set a UCI option of engine 2 (repeatable)
      --min-diff PAWNS   Show only positions where the evals differ by at least this many pawns, e.g. 0.5 (the summary counts all)
      --json             Print the result as JSON on stdout (everything else goes to stderr)
  At most one of --nodes, --movetime, --depth.

Examples:
  eb-cli compare lc0_a.json lc0_b.json --nodes 1000
      Speed and eval on the start position
  eb-cli compare sf.json lc0.json "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1" --movetime 500
      One position
  eb-cli compare sf.json lc0.json --positions suite.epd --movetime 500 --min-diff 0.5
      Only the positions where they disagree by half a pawn or more
```
### position

```text
Facts about a position: status, legal moves, attacks, pins, insights

Usage: eb-cli position [<fen>] [options]

Prints the position's status and legal moves; --json gives everything (attackers, pins, hanging pieces, forks and the other insights) for tools and validators. Edits change the position first, in the order set-piece, remove, side, castling, ep.

Arguments:
  [<fen>]  a FEN (quoted, or its six fields as they come) or startpos

Options:
      --moves "MOVES"    Moves played from the position first, SAN or UCI ("e4 e5 Nf3" or "e2e4 e7e5")
      --square SQ        Also the attackers, attacks, pin ray and safe squares of this square
      --pv "LINE"        Write this line (SAN or UCI) in SAN
      --set-piece SQ=P   Put a piece on a square (e4=Q, e4=n) (repeatable)
      --remove SQ        Take the piece off a square (repeatable)
      --side w|b         Set the side to move
      --castling RIGHTS  Set the castling rights (KQkq, -)
      --ep SQ            Set the en passant square
      --epd              Print the position as an EPD line instead
      --op NAME=VALUE    An EPD operation for --epd (bm=e4, id="test 1") (repeatable)
      --svg FILE         Also draw the position with its insights as an SVG
      --force            Write over a file that exists
      --positions FILE   Every position of an EPD file instead of one FEN
      --json             Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli position startpos
      Status and legal moves
  eb-cli position "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" --square e1 --json
      Everything, with the king's square
  eb-cli position startpos --set-piece e4=Q --remove d2 --epd --op id=edited
      An edited position as an EPD line
  eb-cli position --positions suite.epd --json
      Every position of a file
```
### tablebase

```text
Every move of a position from the Syzygy tables

Usage: eb-cli tablebase <fen> [options]

Wins, cursed wins, draws, blessed losses and losses, best first, with DTZ counted from the position. The halfmove clock counts unless --ignore-clock. The tables: --syzygy, else EB_SYZYGY_PATH, else syzygyPath in eb-cli.json.

Arguments:
  <fen>  a FEN (quoted, or its six fields as they come) or startpos

Options:
      --syzygy DIRS   Syzygy folders (several separated by ';' on Windows, ':' elsewhere)
      --ignore-clock  Read the halfmove clock as 0: what the position is worth if the clock starts now
      --json          Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli tablebase 8/8/8/8/8/2k5/2Q5/K7 b - - 0 1 --syzygy D:/syzygy
      Black to move: the one draw first, then the losses
  eb-cli tablebase "8/8/8/8/8/2k5/8/K6Q w - - 99 1" --json
      A cursed win, as JSON
```
### piece-values position

```text
Piece values from an engine's evaluations in one position

Usage: eb-cli piece-values position <engine> [<fen>] [options]

Removes each piece in turn and measures how much the eval drops, from the piece owner's view: a ranked table, the board, and the average per piece type. The default search is one node, a pure network eval.

Arguments:
  <engine>  an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)
  [<fen>]   a FEN (quoted, or its six fields as they come) or startpos (default startpos)

Options:
      --moves "MOVES"   Moves played from the position first, SAN or UCI ("e4 e5 Nf3" or "e2e4 e7e5")
      --nodes N         Search this many nodes (default 1)
      --movetime MS     Search this many milliseconds
      --depth N         Search to this depth
      --uci NAME=VALUE  Set a UCI option of the engine (repeatable)
      --args "..."      The engine's command-line arguments, instead of the def's
  At most one of --nodes, --movetime, --depth.

Examples:
  eb-cli piece-values position lc0.json "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
      What each piece is worth to the net
  eb-cli piece-values position ceres.json --moves "e4 e5 Nf3 Nc6" --nodes 100
      After four moves, with a 100-node search
```
### piece-values fit

```text
Piece values fitted over many positions

Usage: eb-cli piece-values fit <positions> [options]

Regresses a target against the material imbalance of many positions; the coefficients are the implied piece values. --from eval fits the engine's evaluations (needs --engine), outcome the games' results, pgn-eval the evals in the PGN's comments. Exit 1 when no group had enough positions to fit.

Arguments:
  <positions>  an EPD or PGN file

Options:
      --from eval|outcome|pgn-eval
                                  What is fitted (default eval)
      --engine ENGINE             an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)
      --sample N                  Use this many positions (default: all of an EPD, 500 of a PGN for eval, 20000 for outcome and pgn-eval)
      --nodes N                   Nodes per position for --from eval (default 1)
      --by-phase                  Also fit middlegame and endgame separately
      --player NAME               Only the games of this player (pgn-eval)
      --bootstrap N               Confidence intervals from N bootstrap samples (outcome, pgn-eval)
      --uci NAME=VALUE            Set a UCI option of the engine (repeatable)
      --args "..."                The engine's command-line arguments, instead of the def's

Examples:
  eb-cli piece-values fit quiet.epd --engine lc0.json
      The net's piece values over an EPD
  eb-cli piece-values fit games.pgn --from outcome --by-phase
      Values implied by the results
```
### piece-values batch

```text
Piece values for a folder of nets

Usage: eb-cli piece-values batch <tournament> <nets> [options]

For every .onnx in the folder: self-play at the tournament's time control with the tournament's first engine and that net, then the outcome and pgn-eval endgame fits; summary.csv lists them. A net whose PGN already has the rounds is not played again. The summary keeps the rows of nets the run did not reach. The first Ctrl+C stops the net being played without fitting it; the next run plays it on from its PGN.

Arguments:
  <tournament>  a tournament config: its first engine plays with each net, at its time control
  <nets>        a folder of .onnx networks

Options:
      --rounds N     Self-play games per net (default 1000)
      --out-dir DIR  Where the PGNs and summary.csv go (default <nets>/pv_batch)

Examples:
  eb-cli piece-values batch tournament.json C:/Nets/arm --rounds 400
      400 games per net
```
### engine check

```text
Check that an engine follows the UCI protocol

Usage: eb-cli engine check <engine> [options]

Start-up, options, positions, search limits, info lines, stop and isready, ponder, edge cases and quit, and how it behaves in EngineBattle's analysis pages. PASS, WARN, FAIL or SKIP per check, then a summary. Exit 1 when a check fails. Groups: startup, options, positions, limits, info, stop, ponder, edge, quit, analysis.

Arguments:
  <engine>  an engine definition (.json), an engine executable, or the name of a definition in engineDefsFolder (eb-cli.json)

Options:
      --nodes N         Nodes for the search checks (default 2000)
      --movetime MS     Milliseconds for the search checks, instead of nodes
      --only GROUPS     Run only these groups, comma-separated (startup,positions,stop)
      --delay MS        Wait between commands to the engine (default 30)
      --stop-delay MS   Wait before each stop (default 0)
      --repeat N        Repeat the stop and isready checks N times (default 3)
      --moves "MOVES"   The game the position checks play (SAN or UCI; default a 30-ply Ruy Lopez)
      --uci NAME=VALUE  Set a UCI option of the engine (repeatable)
      --args "..."      The engine's command-line arguments, instead of the def's
  At most one of --nodes, --movetime.

Examples:
  eb-cli engine check myengine.exe
      Every check
  eb-cli engine check lc0.json --only startup,positions,limits,stop
      Only these groups
  eb-cli engine check ceres.json --delay 50 --stop-delay 50
      A slower engine
```
### engine new

```text
Create an engine definition (.json) from an executable, or copy one with another net

Usage: eb-cli engine new <engine> [options]

An executable: starts the engine, reads its id and options, and writes <name>.json (the name without spaces) with every option at its default, overridden by --uci, --net and --syzygy - the GUI's Engine creator. A def (.json): writes a copy with the network --net (the same engine and settings), named after the net, or with the next step for a training checkpoint's def (<prefix>_<step>M[_ema].json); --name names it otherwise. A def that exists is not written over without --force.

Arguments:
  <engine>  the engine executable, or an existing def (.json)

Options:
      --out-dir DIR     The folder to write the def into (default: the working directory; a def's copy: the def's folder)
      --net FILE        The network file, set in the engine's own network option (with a def: required)
      --name NAME       The copy's name (with a def)
      --syzygy DIRS     Syzygy folders (several separated by ';' on Windows, ':' elsewhere)
      --base DEF        An existing def whose settings the new one starts from
      --uci NAME=VALUE  Set a UCI option of the engine (repeatable)
      --dry-run         Print the def, write nothing
      --force           Write over a file that exists
      --timeout MS      How long the engine has to answer (default 15000)

Examples:
  eb-cli engine new C:/Engines/sf/stockfish.exe --uci Threads=8 --uci Hash=4096 --out-dir defs
      A Stockfish def with 8 threads
  eb-cli engine new lc0.exe --net BT4.onnx --dry-run
      Show the def without writing it
  eb-cli engine new Lc0_BT4.json --net C:/Nets/BT5.pb.gz
      The same Lc0 with another net: BT5.json
  eb-cli engine new Ceres_arm_400M.json --net C:/Nets/arm_800003840.onnx
      A training checkpoint's def: Ceres_arm_800M.json
```
### engine bench

```text
Measure engine speed (NPS) across option sets, from a benchmark config

Usage: eb-cli engine bench <config>

Runs the benchmark config (engine, option sets, positions, duration; see Console/ConsoleBenchmark.md) and writes the summary to its SummaryOutputPath, or logs/benchmark-summary-<time>.txt.

Arguments:
  <config>  a benchmark config (JSON)

Examples:
  eb-cli engine bench benchmark-options.json
      Run the benchmark
```
### pgn parse

```text
Parse every game: counts, empty games, parse speed

Usage: eb-cli pgn parse <pgn> [options]

A health check of the parser on a file: games, plies, games without moves or a result, speed and memory. It replays no moves (pgn validate does). Exit 1 when a game could not be read.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn parse big.pgn
      How many games, and how fast they parse
```
### pgn validate

```text
Replay every game: illegal or ambiguous moves, non-standard SAN

Usage: eb-cli pgn validate <pgn> [options]

Every move is matched against the legal moves of its position. A move that fits none (or more than one) is an error and ends that game's check; a legal move written otherwise than standard SAN is a warning. The first 20 findings are shown; --out writes them all to a CSV. Exit 1 when there are errors.

Arguments:
  <pgn>  a PGN file

Options:
  -o, --out FILE   Write every finding to this CSV file
      --force      Write over a file that exists
      --threads N  Check games on N threads (up to the core count) (default 1)
      --json       Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn validate games.pgn
      Errors and warnings, with a summary
  eb-cli pgn validate big.pgn -o findings.csv --threads 8
      Every finding to a CSV, on 8 threads
```
### pgn strip

```text
Write the games without comments, variations or NAGs

Usage: eb-cli pgn strip <pgn> [options]

Every game written again: its tags as they were, the main line numbered and wrapped at 80 characters, the result. The output goes to a temporary file first, so a failed run leaves no half file; the input is never written over.

Arguments:
  <pgn>  a PGN file

Options:
  -o, --out FILE      The output PGN (default <name>_stripped.pgn next to the input)
      --force         Write over a file that exists
      --skip-empty    Leave out games without moves
      --skip-invalid  Leave out games with an illegal or ambiguous move or a bad start FEN
      --json          Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn strip games.pgn
      games_stripped.pgn next to it
  eb-cli pgn strip games.pgn -o clean.pgn --skip-empty --skip-invalid --force
      Only playable games, over clean.pgn
```
### pgn endings

```text
How the games ended: missed wins, repetition, 50-move rule, stalemate...

Usage: eb-cli pgn endings <pgn> [options]

From the games with engine evals: the missed wins (a drawn game in which an engine stood above +4) per engine and how they ended, and how many games ended by repetition, the 50-move rule, stalemate or insufficient material.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn endings games.pgn
      How the games ended
```
### pgn elo

```text
Ratings and results

Usage: eb-cli pgn elo <pgn> [options]

Elo, score and results per engine pair from the games.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn elo match.pgn
      The rating table
```
### pgn speed

```text
Speed statistics per engine

Usage: eb-cli pgn speed <pgn> [options]

Nodes per second (median and average) and other speed figures per engine, from the PGN's comments.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn speed match.pgn
      NPS per engine
```
### pgn deviations

```text
Positions reached in several games, and the moves each engine chose there

Usage: eb-cli pgn deviations <pgn> [options]

Positions reached in more than one game where the moves differ: which engine played what there, and engines that chose differently in the same position themselves (the deviation page's position view), with how the openings were told apart from the engines' own moves.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn deviations gauntlet.pgn
      Self and cross deviations
```
### pgn imbalance

```text
Results by material imbalance

Usage: eb-cli pgn imbalance <pgn> [options]

How the games went when one side had, for instance, a bishop for a knight, the bishop pair, or a rook for a minor piece: the score and the PGN evals per matchup, from the positions past ply 16.

Arguments:
  <pgn>  a PGN file

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli pgn imbalance games.pgn
      The matchups and how they scored
```
### book eval

```text
Filter an opening book down to balanced openings, checked by engines

Usage: eb-cli book eval <book> [options]

Every engine searches the end position of each opening; an opening is kept when every eval is inside --min-eval..--max-eval (pawns, either side) and the engines are within --max-diff of each other. The kept openings are written as PGN or EPD, like the input. The first Ctrl+C stops and writes what passed. Exit 1 when an engine stopped the run.

Arguments:
  <book>  an opening book (.pgn or .epd)

Options:
      --engine ENGINE[,nodes=N|,movetime=MS]
                                  An engine, with its own limit after a comma if it has one; at least one (repeatable)
      --nodes N                   The limit of every engine without its own (default 10000)
      --movetime MS               A time limit for every engine without its own, instead of nodes
      --min-eval PAWNS            The smallest |eval| kept (default 0.8)
      --max-eval PAWNS            The largest |eval| kept (default 1.0)
      --max-diff PAWNS            The largest spread between the engines (default 0.4)
      --count N                   Only the first N openings
  -o, --out FILE                  The output (default BookEvals/BookEval_<book>_<engines>.<ext> next to the book)
      --force                     Write over a file that exists
  At most one of --nodes, --movetime.

Examples:
  eb-cli book eval book.pgn --engine sf.json --engine lc0.json,nodes=2000
      Two engines, lc0 with its own limit
  eb-cli book eval book.epd --engine sf.json --movetime 300 --min-eval 0.3 --max-eval 1.2 -o balanced.epd
      A wider window, to a file of its own
```
### puzzle run

```text
Run a Lichess puzzle test from a config

Usage: eb-cli puzzle run <config> [options]

Runs the Lichess puzzle test of a puzzle config (PuzzleConfig.md): the tables on stdout, the result files in its FailedPuzzlesOutputFolder (LichessSummary_<stamp>.txt and .json, failed puzzles, per-theme tables). --json prints the result document of Console/PuzzleJsonSchema.md as the envelope's result. The first Ctrl+C keeps the rating groups that finished and writes no .json summary, so puzzle trend never reads a partial run. Exit 1 when an engine failed or nothing was scored.

Arguments:
  <config>  a puzzle config (JSON)

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli puzzle run PuzzleConfig.json
      The test, with the tables
  eb-cli puzzle run PuzzleConfig.json --json > result.json
      The result for a script
```
### puzzle eret

```text
Run an ERET test suite (EPD best-move positions) from a config

Usage: eb-cli puzzle eret <config> [options]

Runs the ERET test of an ERET config (EretPuzzles.md); the failed puzzles and the summary go to its FailedPuzzlesOutputFolder. Exit 1 when an engine failed.

Arguments:
  <config>  an ERET config (JSON)

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli puzzle eret EretConfig.json
      The test
```
### puzzle trend

```text
Puzzle scores across a net's training checkpoints, from saved runs

Usage: eb-cli puzzle trend [<folder>] [options]

Reads the LichessSummary_<stamp>.json of every puzzle run in the folder and lines each arm's checkpoints up by training step. Series with fewer steps than --min-steps are hidden (most nets are one-offs). The folder: the argument, else puzzleResultsFolder in eb-cli.json.

Arguments:
  [<folder>]  the folder of puzzle results

Options:
      --arm TEXT        Only arms whose name contains this
      --type TEXT       Only test types that contain this (policy, value)
      --rating-group N  Only this rating group
      --min-steps N     Hide series with fewer steps (default 3)
  -o, --out FILE        Also write the rows as CSV
      --force           Write over a file that exists
      --json            Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli puzzle trend C:/Dev/Chess/EB --arm deg4
      One arm's curve
  eb-cli puzzle trend --type policy -o trend.csv
      Every arm's policy rows, to a CSV
```
### tournament run

```text
Run a tournament from a config

Usage: eb-cli tournament run <config> [options]

Plays the tournament of a tournament config (TournamentConfig.md): round robin, gauntlet, Swiss, cup or ladder, one line per game, the table at the end; the games go to its PgnOutPath. A tournament resumes from the games already in its PGN. The first Ctrl+C stops the games in progress. A config with errors (what tournament validate reports) is refused with exit 2, as the WebGUI refuses to start it. Exit 1 when the run failed. --json: at the end, the games of this run (players, result, how it ended, moves, time; the moves are in the PGN) and the standings of the whole PGN, in the shape of pgn elo --json.

Arguments:
  <config>  a tournament config (JSON)

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli tournament run tournament.json
      Play it (or go on with it)
  eb-cli tournament run tournament.json --json > result.json
      The games and the standings for a script
```
### tournament validate

```text
Check a tournament config without running it

Usage: eb-cli tournament validate <config> [options]

Reads the config and its engine defs and checks the key settings; warnings for settings that will not do what they seem to. Exit 1 when the config has errors.

Arguments:
  <config>  a tournament config (JSON)

Options:
      --json  Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli tournament validate tournament.json
      Is it ready to run?
```
### tune run

```text
Tune engine parameters (Bayesian optimisation)

Usage: eb-cli tune run <config>

Runs the tuner config (Console/ConsoleTuner.md): Bayesian optimisation with SPRT matches, phase by phase; the state, history, best options and the dashboard go to its OutputDir. "Resume": true in the config goes on from the saved state. Ctrl+C stops at once.

Arguments:
  <config>  a tuner config (JSON)

Examples:
  eb-cli tune run tuner-config.json
      Tune
```
### tune dashboard

```text
Write the tuning dashboard again from the saved state

Usage: eb-cli tune dashboard <config>

Writes bo-dashboard.html in the config's OutputDir again from tune-state.json, without running anything.

Arguments:
  <config>  the tuner config of the run

Examples:
  eb-cli tune dashboard tuner-config.json
      The dashboard again
```
### perft

```text
Count the move tree on the 960 Chess960 test positions

Usage: eb-cli perft <depth> [options]

Counts every legal move sequence to the depth and compares it with the known count for each position (Chess960 start positions with castling rights, from the published perft table). Exit 1 when a count is wrong.

Arguments:
  <depth>  1 to 6

Options:
      --count N  Test the first N positions (1-960) (default 10)
      --json     Print the result as JSON on stdout (everything else goes to stderr)

Examples:
  eb-cli perft 5
      Depth 5 on the first 10 positions
  eb-cli perft 5 --count 960
      All of them (about a minute)
```
### gui

```text
Start the WebGUI of a source checkout

Usage: eb-cli gui [<page>] [options]

Runs the WebGUI project with `dotnet run` and opens the browser on the page. Enter or Ctrl+C stops it. A download has no project to run: start EngineBattle instead.

Arguments:
  [<page>]  the page to open (default tournament)

Options:
      --port N  Listen on this port (default the WebGUI's own, 5018)

Examples:
  eb-cli gui
      The tournament page
  eb-cli gui analysis/single --port 5100
      Single-engine analysis on port 5100
```
## Configuration File Examples

### Minimal Tournament Configuration

```json
{
  "Name": "Quick Test",
  "TournamentMode": "RR",
  "Rounds": 2,
  "Opening": {
    "OpeningsPath": "C:/Dev/Chess/Openings/openings.pgn",
    "OpeningsPly": 20,
    "OpeningsTwice": true
  },
  "PgnOutPath": "C:/Dev/Chess/Results/test.pgn",
  "EngineSetup": {
    "EngineDefFolder": "C:/Dev/Chess/Engines/EngineDefs",
    "EngineDefList": ["SFDef.json", "Lc0Def.json"]
  },
  "TimeControl": {
    "TimeConfigs": [ { "Id": 1, "Tc": "60+1" } ]
  }
}
```

### Minimal Puzzle Configuration

```json
{
  "PuzzleFile": "C:/Dev/Chess/Puzzles/lichess_puzzles.csv",
  "Type": "policy, value",
  "MaxRating": 2500,
  "MinRating": 1500,
  "RatingGroups": "2500",
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    { "Engine": { "ConfigName": "Lc0Def.json" } }
  ],
  "SampleSize": 500,
  "Concurrency": 1,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Puzzles/Results"
}
```

`policy` and `value` (and `policy3`, ...) read the network's own output and need Lc0 or Ceres. An
engine without a network, like Stockfish, uses `"search"` with `"Nodes": "10000"`. See
[PuzzleConfig.md](PuzzleConfig.md) for every type.

### Minimal ERET Configuration

```json
{
  "EngineFolder": "C:/Dev/Chess/Engines/EngineDefs",
  "Engines": [
    { "Engine": { "ConfigName": "SFDef.json" } }
  ],
  "PuzzleFile": "C:/Dev/Chess/Puzzles/ERET_VESELY203.epd",
  "SampleSize": 50,
  "TimeInSeconds": 5,
  "RunWithNodeLimit": false,
  "FailedPuzzlesOutputFolder": "C:/Dev/Chess/Results"
}
```

---

## Related Documentation

- [TournamentConfig.md](TournamentConfig.md) - Tournament configuration reference
- [EngineDefConfig.md](EngineDefConfig.md) - Engine definition configuration
- [PuzzleConfig.md](PuzzleConfig.md) - Lichess puzzle test configuration
- [EretConfig.md](EretConfig.md) - ERET puzzle test configuration
- [SwissMode.md](SwissMode.md) - Swiss tournament mode details
- [CupMode.md](CupMode.md) - Knockout/Cup tournament mode details
- [LadderMode.md](LadderMode.md) - Ladder tournament mode details
- [Console/ConsoleTuner.md](Console/ConsoleTuner.md) - Bayesian tuner configuration

