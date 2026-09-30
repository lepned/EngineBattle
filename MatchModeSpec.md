# match: the reference's observable behaviour (maintainer specification)

What `match` follows, written down from the reference implementation's source at commit `60d7a7a` (named, with its licence, in `ChessLibrary/Match/THIRD-PARTY-NOTICE.txt`; version string `alpha 1.8.2 `). All paths below are relative to `app/src/` unless they start with `app/tests` or `man.md`. Line numbers were checked against the source.

**Output streams.** Almost everything goes to **stdout**. That includes errors and warnings: `Logger::print` writes to `std::cout` (`core/logger/logger.hpp:77-91`). Only a few lines go to stderr (see §9).

---

## 1. CLI parsing (`cli/cli.hpp`, `cli/cli.cpp`, `cli/sanitize.cpp`)

### 1.1 Tokenisation (`cli.hpp:203-272`)
- The reference does no quoting itself. Each argv element is one token, so `args="--a --b"` reaches the reference as the single token `args=--a --b`.
- Walk argv from index 1. The token must exactly match a registered flag. Flags are stored with a leading `-`. `-version`, `-v`, `-help` are also registered in double-dash form (`--version`, `--v`, `--help`).
- **Unknown flag:**
  - Message: `Unrecognized option: {arg} parsing failed.`
  - Suggestion: take the registered flag with the smallest Levenshtein distance. If that distance is ≤ 2, append `: Did you mean "{flag}"?`.
  - Snapshot: `Unrecognized option: -engne parsing failed.: Did you mean "-engine"?`
  - On ties, the winner is the first minimum in `unordered_map` iteration order (unspecified).
- **Parameters:** consume every following token until one starts with `-`. Exception: a token of the form `-<digit>...` is a negative number and is consumed.
- **Error wrapping** (`cli.hpp:252-257`): any exception thrown by a handler is re-thrown as
  `Error while reading option "{flag}" with value "{args[i]}"` + `\n` + `Reason: {what}`.
  - `args[i]` is the **last consumed token**, or the flag itself if nothing was consumed.
  - Snapshots:
    - `Error while reading option "-concurrency" with value "-concurrency"\nReason: Option "-concurrency" expects exactly one value.`
    - `Error while reading option "-log" with value "level"\nReason: Option "-log" expects key=value pairs, got "level".`
    - `Error while reading option "-recover" with value "true"\nReason: Option "-recover" does not accept parameters.`
    - `Error while reading option "-engine" with value "name=Alexandria-EA649FED"\nReason: Invalid parameter (must be either "on" or "off"): true` — the reported value is the last token of that `-engine` group, not the bad token.
- **Parameter styles** (`cli.hpp:274-345`):
  - **None**: any parameter → `Option "{flag}" does not accept parameters.`
  - **Single**: anything other than exactly 1 parameter → `Option "{flag}" expects exactly one value.`
  - **KeyValue**:
    - Zero parameters → `Option "{flag}" expects key=value parameters.`
    - Each parameter is split at the **first** `=`.
    - A missing `=`, an empty key or an empty value → `Option "{flag}" expects key=value pairs, got "{param}".`
  - **KeyValueOptional** (`-pgnout`, `-epdout`): same rules, but zero parameters is allowed.
  - **Free** (`-event`, `-site`, `-repeat`, `-use-affinity`): raw token list.
- **Unknown keys:** `Unrecognized {name} option "{key}" with value "{value}".` (`cli.hpp:107-109`). The `{name}` values used are:
  - `engine`, `pgnout`, `pgnout notation`, `epdout`, `openings`, `openings format`, `openings order`
  - `sprt`, `draw`, `resign`, `log`, `log level`, `config`, `report`, `output`, `crc`, `quick`
- **Boolean sub-keys** (`append`, `nodes`, …) only match when the value is exactly `true` or `false`. Any other value falls through to "Unrecognized … option".
- **Number parsing** (`cli.cpp:24-63`):
  - Any whitespace, trailing junk, overflow, or NaN/inf gives `Invalid numeric value: "{str}"`.
  - Unsigned types reject a leading `-` (for example `-srand -1` is an error).
  - A boolean scalar that is neither `true` nor `false` gives `Expected boolean value (true/false), got: {str}`.
- **No arguments at all** (`argc==1`): print the man page and exit 0 (`cli.cpp:758`).

### 1.2 `-engine` / `-each` merge semantics
- `-engine k=v ...` appends a new `EngineConfiguration` and applies the pairs in order; the last duplicate key wins (`cli.cpp:226-232`).
- `-each` is **Deferred** (`cli.cpp:777`, `cli.hpp:244-248, 261-271`):
  - All `-each` parameters from every occurrence are concatenated.
  - They are applied **after the whole command line has been parsed**, to **every** engine, whatever the argv position. That includes engines created by `-quick` or loaded by `-config`.
  - So `-each` **overrides** `-engine` for scalar keys even if `-each` comes first.
  - For `option.X`, `-each` **appends** another `(X, value)` pair. The engine then holds both entries, and setoptions are sent in list order (Threads first, see §8), so the `-each` value is sent last and takes effect.
  - Error text: `Error while reading option "-each" with value "{all params joined by ' '}"`.
- Engine keys (`cli.cpp:186-224`):

| key | effect / validation |
|---|---|
| `cmd` | `cmd` |
| `name` | `name` |
| `tc` | `parseTc` (below) |
| `st` | seconds, decimals allowed, optional `s` suffix → `fixed_time` ms (rounded) |
| `timemargin` | int64 ms; `<0` → `The value for timemargin cannot be a negative number.` |
| `nodes` | int64 |
| `plies`, `depth` | int64 → `limit.plies` |
| `dir` | working dir; command becomes `dir/cmd` |
| `args` | raw string. Windows: command line is `"{dir/cmd} {args}"`, unquoted, with a trailing space when args is empty. POSIX: `argv_split` |
| `restart` | `on`/`off`, else `Invalid parameter (must be either "on" or "off"): {v}` |
| `option.NAME` | pushes `(NAME, value)`; the text before the first `.` is stripped |
| `proto` | must be `uci`, else `Unsupported protocol.` |

- **`parseTc`** (`cli.cpp:142-182`):
  - Contains `hg` → `Hourglass time control not supported.`
  - `inf` or `infinite` → all zero.
  - Empty → `Invalid time control: empty value`.
  - Grammar: `[moves/]time[+inc]`, where `time` is `sec` or `min:sec`.
  - `moves` may be `inf`/`infinite` (→ 0); otherwise it must be > 0 (`Time control move count must be positive`).
  - A separator must appear exactly once with non-empty sides, else `Invalid time control: "{v}"`.
  - Seconds parts (not minutes) may end in `s`.
  - Decimals are rounded to ms; negative values → `Invalid time control duration: "{v}"`.
  - Examples: `2+0.02s` → time 2000, inc 20. `40/1:9.65+0.1` → moves 40, time 69650, inc 100.

### 1.3 All options and defaults (`types/*.hpp`, `types/tournament.hpp:24-84`; non-`USE_CUTE` build)

| option | style | default / behaviour |
|---|---|---|
| `-concurrency N` | single int | 1. `≤0` → `hw_threads - |N|`, printing `Info: Adjusted concurrency to {} based on number of available hardware threads.`. `> hw` without force → `Error: Concurrency exceeds number of CPUs. Use -force-concurrency to override.`. Win64 + not Win11 + >63 → WARN `A concurrency setting of more than 63 is currently not supported on Windows.\nIf this affects your system, please open an issue or get in touch with the maintainers.`, set to 63 (`sanitize.cpp:41-79`) |
| `-force-concurrency` | none | false |
| `-rounds N` | single | 2 |
| `-games N` | single | 2 |
| `-repeat [N]` | free | exactly one all-digit parameter → games=N; otherwise games=2 (`cli.cpp:627-633`) |
| `-sprt elo0= elo1= alpha= beta= model=` | kv | enabled=true; alpha=beta=elo0=elo1=0; model `normalized`. If `rounds==0` **at that moment** → 500000. The default is 2, so this rarely fires (`cli.cpp:347-369`) |
| `-draw movenumber= movecount= score=` | kv | enabled; 0 / 1 / 0; score<0 → `Score cannot be negative.` |
| `-resign movecount= score= twosided=` | kv | enabled; 1 / 0 / false; score<0 → same error |
| `-maxmoves N` | single | enabled, move_count=N (default 1 when unset) |
| `-tb PATHS` | single | enabled, syzygy_dirs |
| `-tbpieces N` | single | 0 (= no limit) |
| `-tbignore50` | none | false |
| `-tbadjudicate WIN_LOSS/DRAW/BOTH` | single | BOTH; else `Invalid tb adjudication type: {}` |
| `-autosaveinterval N` | single | 20 |
| `-log file= level= append= compress= realtime= engine=` | kv | level warn; append true; compress false; realtime true; engine false. Levels: `trace`, `info`, `warn`, `err`, `fatal` |
| `-config file= outname= discard=true stats=` | kv | see §1.5 |
| `-report penta=bool` | kv | true |
| `-output format=fastchess/cutechess` | kv | `format=fastchess` (the default report format); any other value is an unrecognized-option error |
| `-crc32 pgn=bool` | kv | false |
| `-event ...` / `-site ...` | free | `The reference Tournament` / `?`. Parameters are **concatenated with no separator** (`-event My Event` → `MyEvent`). No parameters → empty string, and the PGN header is then omitted |
| `-wait N` | single | 0 ms, slept after each game in that worker |
| `-noswap`, `-reverse`, `-recover` | none | false |
| `-ratinginterval N` | single | 10; 0 → INT_MAX (disabled) |
| `-scoreinterval N` | single | 1; 0 → INT_MAX |
| `-srand N` | single uint64 | random (`random_device`) |
| `-seeds N` | single | 1 (gauntlet seeds) |
| `-variant standard/fischerandom` | single | standard; else `Unknown variant.`. FRC without a book → `Error: Please specify a Chess960 opening book` |
| `-tournament roundrobin/gauntlet` | single | roundrobin; else `Unsupported tournament format. Only supports roundrobin and gauntlet.` |
| `-openings file= format= order= plies= start= policy=round` | kv | format NONE, order sequential, plies -1 (all), start 1. See details after the table |
| `-pgnout [kv...]` | kv-opt | see §6 |
| `-epdout [file= append=]` | kv-opt | file default `the reference_%Y%m%d_%H%M%S.epd`, append true |
| `-use-affinity [list]` | free | `3,5,7-9` → `[3,5,7,8,9]`; bad list → `Bad cpu list.` |
| `-check-mate-pvs`, `-show-latency`, `-testEnv`, `-strict` | none | false |
| `-startup-ms`, `-ucinewgame-ms`, `-ping-ms` | single uint64 | 10000 / 60000 / 60000 |
| `-debug` | none | always errors: `The 'debug' option does not exist in the reference. Use the 'log' option instead to write all engine input and output into a text file.` |
| `-version`, `--version`, `-v`, `--v` | none | print `OptionsParser::Version` and `exit(0)` immediately |
| `-help`, `--help` | none | print the man page, `exit(0)` |

`-openings` details:
- `file` with extension `.epd`/`.pgn` sets the format.
- A missing file → `Opening file does not exist: {}`.
- `format` must be `epd`/`pgn`; the last key wins.
- `start < 1` → `Starting offset must be at least 1!`.
- `policy` other than `round` → `Unsupported opening book policy.`.

Version string (`cli.hpp:57-102`):
- Format: `the reference alpha 1.8.2 ` (note the trailing space), then `YYMMDD` (or `GIT_DATE`), then `-{GIT_SHA}`.
- The date/sha part is only present in non-RELEASE builds. `" (assertions)"` is appended in debug builds.
- Snapshot: `the reference alpha 1.8.2 <build> (assertions)`.

### 1.4 Post-parse sanitising (order matters; `cli.cpp:755-773`, `sanitize.cpp`)
1. Copy `variant` into every engine config.
2. **`fixConfig`** (`sanitize.cpp:20-34`):
   - If `games > 2`, swap games and rounds. If games is still > 2 → `Error: Exceeded -game limit! Must be less than 2`. So `-games 4` alone gives games=2, rounds=4.
   - `report_penta=false` if output is cutechess.
   - `report_penta=false` if `games != 2`.
3. **`setDefaults`**: ratinginterval/scoreinterval 0 → INT_MAX.
4. **`adjustConcurrency`** (above).
5. **`validateConfig`** (`sanitize.cpp:81-109`):
   - SPRT validity checks, in this order:
     - `Error; SPRT: elo0 must be less than elo1!` (elo0 ≥ elo1)
     - `Error; SPRT: alpha must be a decimal number between 0 and 1!`
     - `Error; SPRT: beta must be a decimal number between 0 and 1!`
     - `Error; SPRT: sum of alpha and beta must be less than 1!`
     - `Error; SPRT: invalid SPRT model!`
   - Bayesian model with penta → WARN `Warning; Bayesian SPRT model not available with pentanomial statistics. Disabling pentanomial reports...`, and penta is set to false (`sprt.cpp:52-71`).
   - FRC book check.
   - Without an opening file, prints WARN `Warning: No opening book specified! Consider using one, otherwise all games will be played from the starting position.`
   - If format is neither EPD nor PGN (including "no book at all"), **also** prints `Warning: Unknown opening format, 2. All games will be played from the starting position.` (the integer is the enum value).
   - TB enabled with empty dirs → `Error: Must provide a ;-separated list of Syzygy tablebase directories.`
6. **Engine checks** (`sanitize.cpp:111-173`):
   - Fewer than 2 engines → `Error: Need at least two engines to start!`
   - Windows: append `.exe` if `cmd` contains **no `.` anywhere** (so `./eng` is not changed).
   - All of tc, st, nodes and plies are zero → `Error; no TimeControl specified!`
   - tc together with st → `Error; cannot use tc and st together!`
   - If `dir` is set or the path is absolute, the binary must exist: `Engine binary does not exist: {path}`.
   - Empty name → stem of `cmd`. Still empty → `Error; please specify a name for each engine!`.
   - Duplicate names: the 2nd occurrence becomes `name_2`, the 3rd `name_3`, and so on.

Strict/log settings are applied only **after** parsing (`tournament_manager.cpp`), so warnings printed during parsing never trigger `-strict`.

### 1.5 `-quick` (`cli.cpp:656-715`)
- Each `cmd=X` creates an engine with cmd=name=X and tc 10+0.1 (time 10000, inc 100).
- `book=F` sets the opening file and order=random. The format comes from the `.pgn`/`.epd` extension, else error `Please include the .pgn or .epd file extension for the opening book.`.
- Validation:
  - Not exactly 2 `cmd` → `Option "-quick" requires exactly two cmd entries.`
  - No book → `Option "-quick" requires a book=FILE entry.`
- If both names are equal, they become `name1` and `name2`.
- It then sets:
  - games=2, rounds=25000
  - concurrency = max(1, hw-2)
  - recover
  - draw adjudication movenumber 30, movecount 8, score 8
  - output=CUTECHESS (so penta becomes false)

### 1.6 `-config` / resume JSON (`cli.cpp:484-538`, `tournament/base/tournament.cpp:128-172`)
- **`file=F`**:
  - Prints `Loading config file: {F}` to stdout; a missing file → `File not found: {F}`.
  - Saves the current tournament/engines as "old".
  - **Replaces** the whole tournament config and engine list, and loads `stats`.
  - Options after `-config` on the command line still apply (`-each` always applies).
- **`outname=N`**: config_name (default `config.json`).
- **`discard=true`**: prints `Discarding config file`, restores the old config/engines and clears stats. `discard=false` hits the error branch (`Unrecognized config option "discard" with value "false".`).
- **`stats=false`**: drop loaded stats.
- More than 2 engines → **stderr** `Warning: Stats will be dropped for more than 2 engines.` and the stats are cleared.
- **The file is always written** at tournament destruction, to `config_name` (default `./config.json`, even for normal runs). It is also written by autosave (§7).
- Format: `nlohmann::ordered_json`, `std::setw(4)` (4-space indent), then `std::endl`.
- Top-level key order (macro order, `tournament.hpp:85-88`):
  - `resign{move_count,score,twosided,enabled}`
  - `draw{move_number,move_count,score,enabled}`
  - `maxmoves{move_count,enabled}`
  - `tb_adjudication{syzygy_dirs,max_pieces,ignore_50_move_rule,enabled}` (**result_type is not saved**)
  - `opening{file,format,order,plies,start}`
  - `pgn{additional_lines_rgx,event_name,site,file,notation,append_file,track_nodes,track_seldepth,track_nps,track_hashfull,track_tbhits,track_timeleft,track_latency,track_pv,min,crc}`
  - `epd{file,append_file}`
  - `sprt{alpha,beta,elo0,elo1,model,enabled}`
  - `config_name`, `output`, `seed`, `variant`, `type`, `gauntlet_seeds`
  - `ratinginterval`, `scoreinterval`, `wait`, `autosaveinterval`
  - `games`, `rounds`, `concurrency`, `force_concurrency`
  - `recover`, `noswap`, `reverse`, `report_penta`, `affinity`, `check_mate_pvs`, `show_latency`
  - `log{file,level,append_file,compress,realtime,engine_coms}`
  - then `engines`, then `stats`
- Each engine entry: `{name,dir,cmd,args,restart,options:[[k,v],...],limit:{tc:{increment,fixed_time,time,moves,timemargin},nodes,plies},variant}`.
- Enums are written as integers:
  - Output: default format 0, cutechess 1
  - Notation: SAN 0, LAN 1, UCI 2
  - Order: RANDOM 0, SEQUENTIAL 1
  - Format: EPD 0, PGN 1, NONE 2
  - Variant: STANDARD 0, FRC 1
  - Tournament type: RR 0, GAUNTLET 1
  - Log level: ALL 0, TRACE 1, INFO 2, WARN 3, ERR 4, FATAL 5
- Values stored after sanitising (INT_MAX for disabled intervals, adjusted concurrency).
- Not saved: test_env, strict, startup/ucinewgame/ping ms, affinity_cpus.
- `stats` (`tournament.cpp:133-162`, `scoreboard.hpp:45-64`):
  - For each unordered engine pair (i<j in config order) there is one key `"{A} vs {B}"`.
  - Its value is combined stats from A's perspective: `{wins,losses,draws,penta_WW,penta_WD,penta_WL,penta_DD,penta_LD,penta_LL}`.
  - Object key order follows `unordered_map` iteration (unspecified).
  - On load, the key is split at the first `" vs "`.
- **Resume behaviour:**
  - `total = Σ(wins+losses+draws)` over the loaded stats.
  - `match_count_ = total`.
  - The scheduler starts with `game_counter=total`, `pair_counter=total/games`, `round=total/games+1`, and player indices reset to (0,1).
  - The book offset gains `+ total/games` (§7).
  - If pgn/epd `append=false` and total>0 → prints `Resuming from {total} games, ignoring {-pgnout|-epdout} append=false.` and appends anyway.

### 1.7 `-testEnv`, `-strict`
- **`-testEnv`**: the multi-part PV/mate warnings (§2.5) use `" :: "` as separator instead of `"\n"`.
- **`-strict`**: every `Logger::print` at WARN or higher (and every `LOG_WARN`/ERR/FATAL that passes the log level filter; default filter is WARN) sets `stop=true` and `abnormal_termination=true`. The tournament then stops (running games are interrupted) and exits 1 with the "interrupted" message (§2.6).

---

## 2. Console output

### 2.1 Startup (stdout)
- No banner is printed. The version is only `LOG_INFO`'d to the log file, and `Setting seed to: {seed}` only goes to the log.
- Possible parse-time lines, in the order they occur:
  - `Loading config file: …`
  - `Info: Adjusted concurrency…`
  - the Bayesian warning
  - the two opening warnings
- `Indexing opening suite...` is printed when order=random, during tournament construction (`game/book/opening_book.cpp:41`).
- `Resuming from…` lines (§1.6).

### 2.2 Per game (`matchmaking/tournament/roundrobin/roundrobin.cpp:78-141`; formats in `output/output_the reference.hpp:90-102` and `output/output_cutechess.hpp:99-111`, identical in both formats)
- **Game start**, printed in the worker thread, under `output_mutex_`, before engine processes are (re)started:
  `Started game {game_id} of {final_matchcount} ({white} vs {black})`
  - `game_id` is the 1-based generation counter from the scheduler.
  - `final_matchcount` = scheduler total:
    - Round robin: `n(n-1)/2 * rounds * games` (`tournament/roundrobin/scheduler.hpp:19-22`).
    - Gauntlet: `(s*n - s(s+1)/2) * rounds * games`, with `s = min(seeds, n-1)` (`tournament/gauntlet/scheduler.hpp:20-23`).
- **Game end** (`finish` callback, under the same mutex):
  `Finished game {game_id} ({white} vs {black}): {res} {{reason}}`
  - `res` comes from the white-perspective Stats: `1-0`, `0-1`, `1/2-1/2`, `*` (`output/output.hpp:61-72`).
  - Literal braces surround the reason, e.g. `Finished game 1 (A vs B): 1-0 {Black makes an illegal move}`.
- Order inside `finish` (`roundrobin.cpp:101-135`):
  1. `endGame`
  2. scoreboard update (`updatePair` if report_penta, else `updateNonPair`)
  3. `stats = getStats(first.name, second.name)`, where first/second = scheduler player1/player2 (config order), **not** white/black
  4. If `(match_count_+1) % scoreinterval == 0` **or** all matches played → `printResult(stats, first, second)`
  5. If (`shouldPrintRatingInterval` **and** `isPairCompleted(pairing_id)`) **or** all matches played → `printInterval(...)`
  6. `updateSprtStatus` (§5)
  7. `match_count_++`
- "All matches played" means `match_count_ + 1 == final_matchcount_` (`roundrobin.hpp:40`). So the final summary is printed **once** at the last game, via `printResult` + `printInterval`. It is printed only once even if it also coincides with an interval.
- **Rating interval** (`roundrobin.cpp:167-174`):
  - index = penta ? `pairing_id+1` : `match_count_+1`. Print when `index % ratinginterval == 0`.
  - `match_count_` includes games loaded on resume.
  - `isPairCompleted` is true for non-penta runs too (the cache is never filled), and in penta mode only after the pair's second game finishes.
  - e2e (`app/tests/e2e/rating_interval.sh`): rounds 5, games 2, ratinginterval 2 → penta=false gives 5 reports (games 2,4,6,8,10); penta=true gives 3 reports (pairs 2 and 4, plus the final).
- **Score interval**: index `match_count_+1`. It only has a visible effect in cutechess format; the reference `printResult` is a no-op.

### 2.3 The reference format
`printInterval` (`output_the reference.hpp:20-28`) prints:
```
--------------------------------------------------      (50 '-')
{printElo}
{printSprt}
--------------------------------------------------
```
**Two engines: H2H block** (`output_the reference.hpp:115-136`):
```
Results of {first} vs {second} ({tc}, {threads}, {hash}{, bookname}):
Elo: {elo.getElo()}, nElo: {elo.nElo()}
LOS: {elo.los()}, DrawRatio: {dr:.2f} %{, PairsRatio: {pr:.2f}}
Games: {W+L+D}, Wins: {W}, Losses: {L}, Draws: {D}, Points: {pts:.1f} ({ptsRatio:.2f} %)
[penta only] Ptnml(0-2): [{LL}, {LD}, {WL+DD}, {WD}, {WW}], WL/DD Ratio: {WL/DD:.2f}
```
- Elo object: `EloPentanomial` if penta, else `EloWDL`.
- DrawRatio: `drawRatioPenta` if penta, else `drawRatio`.
- `, PairsRatio:` is present only when penta.
- `tc` (`output_the reference.hpp:138-158`), per engine; if both engines' strings are equal it is printed once, else `"{a} - {b}"`:
  - If time+inc > 0: `[{moves}/]{time_s}[+{inc_s:.2g}]`. `time_s` uses fmt `{}` of a double (shortest form: `10`, `0.5`, `60`). The increment uses `{:.2g}` (`0.1`, `0.02`, `1`; `100` → `1e+02`).
  - Else if fixed time: `{fixed_s}/move`.
  - Else if plies: `{plies} plies`.
  - Else if nodes: `{nodes} nodes`.
  - Else empty.
- `threads`: `{value}t` from the engine's UCI option `Threads` (current value, i.e. engine default or the value set), or `NULL` if the engine has no such option. Printed once if equal, else `"a - b"`.
- `hash`: `{value}MB` or `NULL`, same rule.
- `bookname`: the opening file path after the last `/` or `\`. When empty, the `, bookname` part is omitted.
- The engines used for tc/threads/hash are the two UciEngine objects of the game that just finished, matched by name.
- Snapshot (`app/tests/snapshots/cli/actual_game_black_to_move.txt`): `Results of black_winner vs white_loser (1 nodes, NULL, 16MB, black-to-move.epd):`

**More than 2 engines: ranking table** (`output_the reference.hpp:34-77`):
- `W = max(25, longest name)`.
- Header: `fmt("{:<4} {:<{W}} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>20}\n", "Rank","Name","Elo","+/-","nElo","+/-","Games","Score","Draw","Ptnml(0-2)")`
- Row: `fmt("{:>4} {:<{W}} {:>10.2f} {:>10.2f} {:>10.2f} {:>10.2f} {:>10} {:>9.1f}% {:>9.1f}% {:>20}\n", rank, name, diff, error, nEloDiff, nEloError, games, pointsRatio, penta?drawRatioPenta:drawRatio, penta?"[a, b, c, d, e]":"")`
- Per-engine stats = sum over all pairings (inverted when the engine is the second key) (`scoreboard.hpp:157-171`).
- Sorted by `diff` descending with the comparator `(!isnan(a)&&isnan(b)) || a>b`, using unstable `std::sort`.

`printSprt` (`output_the reference.hpp:79-88`), only if SPRT is enabled:
`LLR: {llr:.2f} ({getFraction(llr)*100:.1f}%) {getBounds()} {getElo()}`, e.g. `LLR: 1.23 (41.8%) (-2.94, 2.94) [0.00, 5.00]`.
- For negative LLR the fraction is `-llr/lower`, which is **negative** (lower<0). Example: `(-34.0%)`.

### 2.4 Cutechess format (`output/output_cutechess.hpp`)
- `printResult`: `Score of {first} vs {second}: {W} - {L} - {D}  [{wdlScore:.3f}] {W+L+D}` (two spaces before `[`).
- `printInterval` has no dash lines.
  - Two engines: `Elo difference: {EloWDL.getElo()}, LOS: {los}, DrawRatio: {drawRatio:.2f} %`
  - More than 2: the same table as the reference, but without the Ptnml column (header has 9 columns, row ends at `{:>9.1f}%`), always WDL.
- `printSprt` (`output_cutechess.hpp:78-97`): `SPRT: llr {llr:.2f} ({pct:.1f}%), lbound {lower:.2f}, ubound {upper:.2f}{result}`
  - LLR is always trinomial.
  - `pct = llr<0 ? llr/lower*100 : llr/upper*100`.
  - `result` = ` - H1 was accepted` if `llr>=upper`, ` - H0 was accepted` if `llr<lower` (strict `<`), else empty.
- `endTournament`: always `Tournament finished`.

### 2.5 Warnings printed during games (stdout; `Warning; …` with a semicolon)
- `Warning; Illegal move {move|<none>} played by {name}` (`matchmaking/match/match.cpp:684`). Preceded, when the move string is not UCI-shaped, by `Warning; Move does not match uci move format, lowercase and 4/5 chars. Move {m} played by {name}` (`match.cpp:679-681`).
- `Warning; No info line available to extract score from engine {name}` and `Warning; Could not extract score from engine {name}: {err}` (`match.cpp:290, 297`). Error texts:
  - `Element 'score' not found`
  - `Element 'score' has no type or value`
  - `Unexpected score type: {line}`
  - `Invalid score value: {tok}`
- **PV checks.** Every info line containing `info` and ` score ` (excluding `info string`) is checked. Output is `"{1}{0}{2}{0}{3}{0}{4}"` with `sep = "\n"` (`" :: "` under -testEnv) (`match.cpp:687-742`):
  - Part 1, message:
    - `Warning; Illegal PV move - move {m} from {name}`
    - `Warning; PV continues after threefold repetition - move {m} from {name}`
    - `Warning; PV continues after fifty-move rule - move {m} from {name}`
    - `Warning; PV continues after checkmate - move {m} from {name}`
    - `Warning; PV continues after stalemate - move {m} from {name}`
    - `Warning; Incomplete mating PV - from {name}`, `Warning; Too long mating PV - from {name}`, `Warning; Mating PV does not end with checkmate - from {name}` (these three only with `-check-mate-pvs`)
    - `Warning; Bestmove does not match beginning of last PV - move {bestmove} from {name}`
  - Part 2: `Info; {info line}`
  - Part 3: `Position; {startpos | fen FEN}`
  - Part 4: `Moves; {uci moves joined by space}` (a trailing space when there are none)
  - Snapshot:
    ```
    Warning; Illegal PV move - move a1a1 from white_loser
    Info; info depth 1 pv a1a1 score cp 0
    Position; fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1
    Moves;
    ```
- **Mate sign mismatch** (`match.cpp:101-146`): `Warning; Sign mismatch in mate scores {themMate} and {usMate} from {themName} ({themColor}) and {usName} ({usColor})`, then `Infos; {lastInfo them} ; {lastInfo us}`, `Position; …`, `Moves; …`, with the same separator.
- Engine/UCI warnings (`engine/uci_engine.cpp`):
  - `Warning; Engine {name} is not responsive` (isready timeout, unless stopping)
  - `Warning; Engine {name} didn't respond to uci.`
  - `Warning; {name} doesn't have option {opt}`
  - `Warning; Invalid value for option {opt}; {value}`
  - `Warning; Failed to set option {k} with value {v} for engine {name}: {e}`
  - `Warning; Failed to set UCI_Chess960 option for engine {name}: {e}`
  - `Warning; No output from {name}`
  - `Warning; No bestmove found from {name} because: {reason}. Last line was: {line}` (only when the bestmove read status was OK)
  - `Warning; Failed to set CPU affinity for the engine process to {cpus}. Please restart.`
  - `Warning; Failed to set CPU affinity for the tournament thread.`
- Fatal lines:
  - `Fatal; {name} engine startup failure: "{err}"` (`match.cpp:342-353`). The error is `Couldn't write uci to engine`, `Engine didn't respond to uciok after startup`, or a process error. Effect: stop, abnormal, the game is not finished/recorded.
  - `Failed to set position from opening book, invalid FEN or EPD: {escaped}`
  - `Match failed with exception: {e}`
  - `Error while creating match: {e}`
- With `-crc32 pgn=true`, after every PGN write: `File {file} has CRC32: {crc:#x}` (lowercase, `0x` prefix, no padding) (`core/filesystem/file_writer.hpp:25-34`).
- `-show-latency` only logs to the log file (`LOG_INFO_THREAD`), nothing on stdout.

### 2.6 End of tournament / special stops
- **Normal completion**, without SPRT:
  - The final summary comes from the last `finish` (§2.2).
  - **No `Tournament finished` line is printed.**
  - The destructor (`tournament/base/tournament.cpp:61-87`), the reference output only, prints for each tracked player (unordered order):
    ```

    Player: {name}
      Timeouts: {n}
      Crashed: {n}

    ```
    The blank lines appear only if the tracker is non-empty. A player is tracked only after at least one timeout or disconnect/stall loss (`tournament.cpp:286-299`).
  - Then `config.json` is saved.
  - Then `main` prints `Finished match` and `Total Time: {h:02}:{m:02}:{s:02} (hours:minutes:seconds)` followed by an extra `\n` (a blank line) (`main.cpp:59-66`). Exit 0.
- **SPRT stop:** see §5. Exit 0.
- **Engine crash/stall without `-recover`** (`tournament.cpp:233-249`):
  - Prints `Game {id} stalled / disconnected and no recover option set for engine, stopping tournament.`
  - Calls `finish` (so the `Finished game …` line, intervals and SPRT run) but **writes no PGN/EPD** for that game.
  - Sets abnormal.
- **With `-recover`:** the game is recorded normally. Engines failing `isready` are recreated.
- **CTRL-C** (SIGINT; Windows: C/Break/Close/Logoff/Shutdown) (`core/globals/globals.cpp:63-68`):
  - Sets stop + abnormal and prints nothing itself.
  - Running games end as INTERRUPT: no Finished line, no PGN.
  - Then the player table, the save, and `main` prints `Tournament was interrupted. To resume the tournament, run: {argv[0]} -config file={config_name}`, then `Finished match`, then `Total Time…`. Exit 1.
- The same "interrupted" text appears for any abnormal termination: `-strict`, startup failure, the non-recover crash.
- **Uncaught errors** (`main.cpp:42-55`):
  - A `the reference_exception` prints `{what}` (stdout). Exit 1.
  - Any other `std::exception` prints `PLEASE submit a bug report to https://github.com/Disservin/the reference/issues/ and include command line parameters and possibly the stdout/log of the reference.` then `{what}`. Exit 1.
- TB load failure: `Error: Failed to load Syzygy tablebases from the following directories: {dirs}`.

---

## 3. Game-end annotation (`match.hpp:233-246`; used as the `Finished game` `{…}` text and the PGN last comment)

| Termination | reason text | PGN `Termination` |
|---|---|---|
| checkmate | `{Winner} mates` (`White`/`Black`; color = **not** side to move) | `normal` |
| stalemate | `Draw by stalemate` | normal |
| insufficient material | `Draw by insufficient mating material` | normal |
| 3-fold repetition | `Draw by 3-fold repetition` | normal |
| 50-move rule | `Draw by fifty moves rule` | normal |
| resign adjudication | `{Winner} wins by adjudication` | `adjudication` |
| draw adj. / maxmoves | `Draw by adjudication` | adjudication |
| TB win | `{Winner} wins by adjudication: SyzygyTB` | adjudication |
| TB draw | `Draw by adjudication: SyzygyTB` | adjudication |
| time loss | `{Loser} loses on time ({overrun}ms overrun)` | `time forfeit` |
| illegal move | `{Loser} makes an illegal move` | `illegal move` |
| crash/disconnect | `{SideToMove} disconnects` | `abandoned` |
| stall (isready timeout) | `{SideToMove}'s connection stalls` | `abandoned` |
| interrupted | `Game interrupted` (never printed) | `unterminated` |

Natural game-over order (`third_party/chess.hpp:2592-2604`, called at the start of each turn, `match.cpp:428-446`):
1. halfmove clock ≥ 100 → checkmate if mated, else fifty-move (a stalemate at hmvc ≥ 100 is reported as fifty-move)
2. insufficient material:
   - only kings
   - 3 pieces with a bishop or knight
   - 4 pieces: one bishop per side on the same square colour, or one side has 2 same-coloured bishops
   - K+N+N vs K is **not** insufficient
3. repetition: current position hash seen twice before, stepping back 2 plies at a time within the halfmove window
4. no legal moves → mate or stalemate

Time-loss details:
- `overrun = max(0, -time_left_after_subtract)`.
- A start-of-turn check `time_left <= 0` produces `(0ms overrun)` (`match.cpp:448-451`).
- On time loss the engine gets `stop` and the reference waits up to 10 s for `bestmove`.

Disconnect colour quirk:
- The colour is always the side to move at detection time.
- If Black fails `refreshUci` before the first move, the text is still `White disconnects` (`match.cpp:363-369, 607-625`).
- If `stop` is already set, the crash becomes INTERRUPT instead.

---

## 4. Statistics (`matchmaking/elo/*`, `matchmaking/stats.hpp`)
Constants: `z = 1.959963984540054`; `eloDiff(s) = -400*log10(1/s - 1)`.

**WDL** (`elo/elo_wdl.cpp:12-53`):
- `n = W+L+D`
- `s = (W + 0.5D)/n`
- `var = W/n*(1-s)² + D/n*(0.5-s)² + L/n*s²`
- `vpg = var/n`
- `ub/lb = s ± z*sqrt(vpg)`
- `diff = eloDiff(s)`
- `error = (eloDiff(ub) - eloDiff(lb))/2`
- `nElo(x) = (x-0.5)/sqrt(var) * 800/ln10`
- `nEloDiff = nElo(s)`, `nEloErr = (nElo(ub) - nElo(lb))/2`, using the same `var`
- `LOS = (1 - erf(-(s-0.5)/sqrt(2*vpg)))/2`

**Pentanomial** (`elo/elo_pentanomial.cpp:9-58`):
- `P = total pairs`
- `s = WW + 0.75WD + 0.5(WL+DD) + 0.25LD` (as fractions of P)
- `var = Σ p_i (a_i - s)²` with a = {1, .75, .5, .25, 0}
- `vpp = var/P`
- Bounds use `vpp`
- `nElo(x) = (x-0.5)/sqrt(2*var) * 800/ln10`
- `LOS` uses `vpp`

**String formats:**
- `getElo()` = `"{diff:.2f} +/- {error:.2f}"` (`elo_wdl.cpp:10`)
- `nElo()` = `"{:.2f} +/- {:.2f}"`
- `los()` = `"{los*100:.2f} %"`

**Ratios** (`stats.hpp:68-84`):
- `drawRatio = 100*D/n`
- `drawRatioPenta = (WL+DD)/P*100`
- `pairsRatio = (WW+WD)/(LD+LL)`
- `WL/DD = WL/DD`
- `points = W + 0.5D`
- `pointsRatio = points/n*100`
- Division by zero is not guarded:
  - fmt prints `inf`, `nan` or `-nan` (x86 `0.0/0.0` yields negative NaN, so `-nan` is typical).
  - An exact score of 0.5 gives `diff = -0.0`, printed as **`-0.00`**.

**Game-level Stats:** from the white perspective: W if white won, L if white lost, D if drawn.

**Pentanomial pairing** (`scoreboard.hpp:118-145`):
- The key is the scheduler `pairing_id`, i.e. the pair of games sharing one opening (games per round = 2).
- First finished game: `cache = ~stats`, i.e. the stats inverted (W↔L, WW↔LL, WD↔LD). Nothing is committed yet.
- Second game:
  - `cache += stats`, then increment exactly one penta bucket from the combined W/L/D:
    - WW if wins==2
    - WD if one win and one draw
    - WL if one win and one loss
    - DD if draws==2
    - LD if one loss and one draw
    - LL if losses==2
  - Commit the whole cache (W/L/D **and** penta) under key `(white2, black2)`.
- Consequence: in penta mode, W/L/D counts only include completed pairs.
- Quirk: the inversion assumes colours swap between the two games. With `-noswap` (games 2, penta), the first game's result is wrongly flipped.
- `getStats(a,b) = results[(a,b)] + ~results[(b,a)]` (`scoreboard.hpp:148-155`).

Unit-test reference values (`app/tests/elo_test.cpp`):
- WDL (136 W, 96 L, 111 D): diff 40.70, error 30.43, nElo 49.77 ± 36.77, LOS `99.60 %`.
- Penta `Stats(ll=34, ld=54, wl=31, dd=32, wd=64, ww=75)`: diff 55.58 ± 27.65, nElo 57.94 ± 28.28, LOS `100.00 %`.

---

## 5. SPRT (`matchmaking/sprt/sprt.cpp`)
- **Bounds:** `lower = ln(beta/(1-alpha))`, `upper = ln((1-beta)/alpha)` (`sprt.cpp:18-32`).
- **Result** (`sprt.cpp:313-321`): `llr >= upper` → H1; `llr <= lower` → H0; else continue.
- **Regularisation:** a count of 0 becomes 1e-3, both in the probabilities and in `total`.
- **Trinomial** (`getLLR(win, draw, loss)`, `sprt.cpp:93-119`): scores `{0, .5, 1}`, probs `{L,D,W}/total`.
  - `normalized`: `t0,t1 = elo/(800/ln10)`; LLR via `getLLR_normalized(total, scores, probs, t0, t1)`.
  - `bayesian`:
    - If raw win==0 or loss==0 → 0.
    - `drawelo = 200*log10((1-L)/L*(1-W)/W)` from the probs.
    - `score_i = pw + 0.5*(1-pw-pl)` with `pw = 1/(1+10^((-elo+drawelo)/400))`, `pl = 1/(1+10^((elo+drawelo)/400))`.
    - Then `getLLR_logistic`.
  - `logistic` (anything else): `s = 1/(1+10^(-elo/400))`, then `getLLR_logistic`.
- **Pentanomial** (`sprt.cpp:121-143`): probs `{LL, LD, WL+DD, WD, WW}`, scores `{0, .25, .5, .75, 1}`.
  - `normalized`: `t = sqrt(2)*elo/(800/ln10)`.
  - Otherwise (including bayesian): logistic.
- **`getLLR_logistic`** (`sprt.cpp:201-242`):
  - For s: `θ = itp(f, -1/(a_max - s), -1/(a_min - s), +INF, -INF, k1=0.1, k2=2.0, n0=0.99, eps=1e-3)`, where `f(x) = Σ p̂_i (a_i-s)/(1+x(a_i-s))`.
  - `p_i = p̂_i/(1+θ(a_i-s))`.
  - `LLR = total * Σ p̂_i (ln p1_i - ln p0_i)`.
- **`getLLR_normalized`** (`sprt.cpp:244-311`):
  - `mle(mu_ref=0.5, t)`: start with uniform p. Up to 10 iterations:
    - `(mu, var)` of p.
    - `φ_i = a_i - 0.5 - 0.5*t*σ*(1 + ((a_i-mu)/σ)²)`.
    - `θ = itp(g, -1/max φ, -1/min φ, +INF, -INF, 0.1, 2.0, 0.99, 1e-7)`, where `g(x) = Σ p̂_i φ_i/(1+xφ_i)`.
    - `newp = p̂/(1+θφ)`.
    - Break if max |Δp| < 1e-4.
  - LLR as above.
- **ITP** (`sprt.cpp:163-199`), reproduce verbatim:
  - If `f_a > 0`, swap a/b and f_a/f_b.
  - `n_half = ceil(log2(|b-a|/(2ε)))`, `n_max = n_half + n0`.
  - Loop while `|b-a| > 2ε`, with i = 0, 1, …:
    - `x_half = (a+b)/2`
    - `r = ε*2^(n_max - i) - (b-a)/2`
    - `δ = k1*(b-a)^k2`
    - `x_f = (f_b*a - f_a*b)/(f_b - f_a)`
    - `σ = (x_half - x_f)/|x_half - x_f|`
    - `x_t = δ <= |x_half - x_f| ? x_f + σδ : x_half`
    - `x_itp = |x_t - x_half| <= r ? x_t : x_half - σr`
    - Evaluate f; if exactly 0 → a = b = x; if negative → a = x; else b = x.
  - Return `(a+b)/2`.
  - Note: the initial f values are ±INF, so the first `x_f` is NaN and x_half is used.
- **Strings:** `getBounds()` = `({lower:.2f}, {upper:.2f})`; `getElo()` = `[{elo0:.2f}, {elo1:.2f}]`. `getFraction` is described in §2.3.
- **When the test is evaluated** (`roundrobin.cpp:143-165`): after every finished game, on `getStats(first, second)`, with penta per `report_penta`.
- **Stop condition:** `result != CONTINUE`. The `|| match_count_ == final_matchcount_` term never fires, because the count is incremented afterwards. On stop:
  1. set `stop`
  2. log `SPRT ({getElo()}) completed - {H0|H1} was accepted` (log file only)
  3. `printResult` (cutechess: another `Score of` line)
  4. `printInterval` (a full report again; this can duplicate the interval just printed)
  5. `endTournament(msg)`:
     - the reference prints the message, e.g. `SPRT ([0.00, 5.00]) completed - H1 was accepted`
     - cutechess prints `Tournament finished`
  - Other in-flight games are interrupted and not reported. Not abnormal: exit 0, no "interrupted" line; then `Finished match` / `Total Time`.
- If the rounds run out before a decision: the normal final summary, no SPRT message.

---

## 6. PGN / EPD output (`game/pgn/pgn_builder.cpp`, `game/pgn/pgn_gen.hpp`)
- **`-pgnout` keys:**
  - `file` (default `the reference_%Y%m%d_%H%M%S.pgn`, local time when parsed)
  - `append` (true)
  - `nodes`, `seldepth`, `nps`, `hashfull`, `tbhits`, `timeleft`, `latency`, `pv`, `min` (all false)
  - `notation=san|lan|uci` (san)
  - `match_line=REGEX` (repeatable)
- **When written:** one write per game, in finish order. The file is opened binary, so newlines are `\n`. Nothing is written for interrupted games, or for a non-recover crash (`tournament.cpp:264-280`).
- **Headers**, in this order (`pgn_builder.cpp:28-86`). Headers with empty values are skipped:
  1. `Event`, `Site`
  2. `Date` (`%Y.%m.%d`, local, taken at Match construction)
  3. `Round` = **pairing_id+1** (pair number; both games of a pair share it; not the tournament round)
  4. `White`, `Black`, `Result`
  5. Unless `min`: `EngineWhiteName`, `EngineWhiteAuthor`, `EngineBlackName`, `EngineBlackAuthor` (from UCI `id name`/`id author`; each only if non-empty)
  6. `SetUp "1"` + `FEN` if fen ≠ standard startpos (`rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`) or the variant is FRC. FEN = the opening position as `getFen()` with counters, **before** book moves. An EPD line without counters gets ` 0 1`.
  7. `Variant "Chess960"` for FRC
  8. Unless `min`:
     - `GameDuration` (`HH:MM:SS`)
     - `GameStartTime`, `GameEndTime` (`YYYY-MM-DDTHH:MM:SS +HHMM`)
     - `PlyCount` (= number of recorded moves, including book moves and the illegal move)
     - `Termination` (§3)
     - `TimeControl`, or `WhiteTimeControl` + `BlackTimeControl` if the Limits differ (timemargin included in the comparison)
  9. Unless `min` and only when found: `ECO`, `Opening`. This is the last match in the Lichess table on `getFen(false)` along the move list; there is none for FRC.
- **Header line format:** `[{key} "{value}"]\n`, then a blank line.
- **TimeControl string** (`game/timecontrol/timecontrol.cpp:65-82`):
  - Fixed time → `{sec:.8g}/move`.
  - All of moves, time and inc zero → `-`.
  - Otherwise `[{moves}/]{time_s:.6g}[+{inc_s:.6g}]`; the time part only if time+inc > 0, the `+inc` part only if inc > 0.
  - Examples: `40/60+0.6`, `0.001+0.005`, `0+0.005`, `1/move`, `0.2/move`.
- **Movetext** (`pgn_gen.hpp:57-124`):
  - Move numbers: `N. move` for White. For Black: `N... move` only as the very first move; otherwise the bare move.
  - Start number from the FEN fullmove counter, e.g. `17. e4`, `4... O-O`.
  - Comment: ` {…}` appended to the move.
  - Tokens are joined by single spaces. Wrap before a token if `curLen>0 && curLen + len + 1 > 80`. The result terminator is appended as the last token.
  - After the result the file gets `\n\n`.
- **Per-move comment** (`pgn_builder.cpp:162-196`):
  - Book moves: `book` only. The final reason is **not** added even if the book move is the last move.
  - Engine moves: `"{score}/{depth} {t}"`, then these parts, each prefixed by `, ` and only when enabled/non-empty:
    - `tl={timeleft}`, `latency={latency}` (both `{:.3f}s`)
    - `n={nodes}`, `sd={seldepth}`, `nps={nps}`, `hashfull={hf}`, `tbhits={tb}`
    - `pv="{pv}"`
    - the `line="…"` entries (joined by `, `)
    - on the last move only, the game reason
  - `score`: cp → `+1.23` / `-0.50` / `0.00` (no sign for 0). Mate → `+M{2m-1}` for m>0, `-M{-2m}` otherwise. No score → empty, so the comment starts `/15 …`.
  - `t` = `{elapsed/1000:.3f}s`.
  - `min=true` → no comments at all.
  - Example: `1. e4 {+1.00/15 1.321s} e5 {+1.23/15 0.430s} 2. Nf3 {+1.45/16 0.310s}\nNf6 {+10.15/18 1.821s, engine2 got checkmated} 1-0`
- **Illegal move handling:**
  - The move before the illegal one gets `, {reason}: {illegalMove}` and movetext stops there. Example: `18. Nf3 {+1.45/16 0.310s, Black makes an illegal move: a1a1} 1-0`.
  - If the first move is illegal, the movetext is just `{White makes an illegal move: a1a1} 0-1`.
- **Result:** `1-0`, `0-1`, `1/2-1/2`, `*` from the white player's result.
- **Notation:** SAN (`moveToSan`), LAN (`moveToLan`), or raw UCI.
- **Per-move data** comes from the last stdout info line that contains `info` and ` score `, is not `info string`, and is multipv 1 (or has no multipv); an exact score is preferred over a bound (`uci_engine.cpp:389-418`). The PV is the tokens after `pv` while they are UCI-shaped. `timeleft` = remaining clock after the move; `latency` = measured ms − the last `info … time` value. If the engine printed ≤ 1 stdout line, only move and time are stored.
- **CRC** (`-crc32 pgn=true`): the standard CRC-32 (poly 0xEDB88320) is seeded from the existing file content when appending, updated per write, and printed as in §2.5.
- **EPD out** (`game/epd/epd_builder.hpp:17-31`): one line per finished game: the final position after all legal moves, `getFen(false) + " hmvc {h}; fmvn {f};"` + `\n`. The ep square is only present when an ep capture is legal. Same write rules as PGN.

---

## 7. Scheduling semantics (`tournament/schedule/base_scheduler.hpp:36-78`, `roundrobin.cpp`)
- **Generation order**, per round, with `games` consecutive games per pair:
  - Round robin: pairs (0,1), (0,2), …, (0,n-1), (1,2), …
  - Gauntlet: same loop, but player1 < seeds, so seeds also play each other once.
- Each Pairing carries:
  - `game_id` = ++counter (1-based, global)
  - `pairing_id` = pair counter (0-based; increments after each `games` block)
  - `round_id`
  - `opening_id`: a new `fetchId()` when the first game of a pair is generated
- **Colours** (`roundrobin.cpp:85-91`): white = player1. If `game_id` is even and not `-noswap`, swap. Then if `-reverse`, swap again. Parity comes from the global game_id; with games=1 the colours alternate across successive games.
- **Openings** (`game/book/opening_book.cpp`):
  1. Load EPD (non-empty lines; a line with `;` goes through `setEpd`, otherwise `setFen`) or PGN (SAN moves up to `plies` if ≠ -1; stops before a game-ending move or at a SAN error; honours the FEN tag). `.gz` needs zlib.
  2. If order=random: print `Indexing opening suite...`, then shuffle with `std::mt19937_64` seeded with `seed`: `for i in 0 .. size-2: j = i + (rng() % (size-i)); swap(v[i], v[j])` (`opening_book.hpp:20-27`).
  3. Rotate left by `offset = (start-1) + resumedGames/games`, modulo size.
  4. **Truncate to `rounds` entries** if larger.
  5. `fetchId` = `idx++ % size`. With more than 2 engines, openings advance per pair encounter, not per round, and wrap within the truncated book.
  6. No book → startpos with no moves.
- **Concurrency:**
  - A thread pool of `concurrency` workers. Initially `concurrency` games are enqueued; each finished game enqueues the next.
  - Output lines are atomic (mutex), but the Started/Finished order interleaves.
  - Engines are cached by name and reused across games, unless `restart=on`, which uses a new process per game.
  - A global semaphore allows at most 16 simultaneous engine starts.
- **Autosave** (`roundrobin.cpp:32-52`): the main thread polls once per second. When `match_count_ >= initial + k*autosaveinterval`, it saves config.json and advances the target by one interval.
- **`-recover`**: see §2.6. The stall/crash is counted in the player table (`Crashed`), timeouts in `Timeouts`.

---

## 8. Adjudication, time and engine I/O
- **Per-turn order** in `playMove` (`match.cpp:428-580`):
  1. natural game over
  2. clock ≤ 0 → time loss (0 ms)
  3. `adjudicate` (TB → resign → draw → maxmoves)
  4. `isready`
  5. `position`
  6. `isready`
  7. `go`
  8. read until `bestmove`, with timeout threshold
  9. `isready`
  10. parse the move; then legality, then the time check
  11. make the move; update the trackers from the mover's `lastScore`
- `isready` uses `ping-ms`; a timeout is a stall, any other failure a disconnect.
- **Draw adjudication** (`match.hpp:40-75`, `match.cpp:798`):
  - After each engine move (book moves excluded): if the new hmvc == 0, reset the counter to 0.
  - Then, if movecount > 0: increment when `|cp| <= score` and the score is cp, else reset to 0.
  - If there is no score, reset.
  - Adjudicated at the start of a turn when `fullMoveNumber()-1 >= movenumber && counter >= movecount*2`. fullMoveNumber includes the FEN counter.
- **Resign adjudication** (`match.hpp:77-131`, `match.cpp:784-796`):
  - Non-twosided: per mover colour, count consecutive own moves with `cp <= -score` or a negative mate. Resignable when either colour's count ≥ movecount.
  - Twosided: count consecutive plies with `|cp| >= score` or any mate. Resignable when ≥ movecount*2.
  - Adjudicated only if the last mover's current `lastScore().value < 0`; that mover loses. Text: `{side-to-move colour} wins by adjudication`.
  - A missing score resets that colour's counter (or the shared counter when twosided).
- **Maxmoves** (`match.hpp:133-146`): counts engine plies only (not book). Draw when ≥ N*2.
- **TB** (`matchmaking/syzygy.cpp:21-72`, `match.cpp:747-782`):
  - Probed only when the position has hmvc == 0, no castling rights, pieces ≤ TB_LARGEST, and (tbpieces==0 or pieces ≤ tbpieces).
  - The WDL result is from the side to move. Cursed win / blessed loss count as a draw unless `-tbignore50`.
  - `-tbadjudicate` filters wins/losses vs draws.
- **Time control** (`timecontrol.cpp:16-63`, `matchmaking/player.hpp:16-23`):
  - Initial clock = `time + increment`, so the first `go` already includes the increment. With `st`, the clock is `fixed_time`.
  - After a move:
    - Decrement movestogo (at 1: reset to `moves` and credit `time`).
    - If untimed, done.
    - `left -= elapsed`. If `left < -timemargin`, it is a **loss** and `overrun = -left`.
    - Clamp at 0, add `increment`, add `time` if the period completed, and with st reset to `fixed_time`.
  - Read timeout = `left + timemargin + 100` ms. It is infinite when nodes or plies are set, even together with tc.
  - No time control (nodes/depth only) → no time losses.
- **`go` string** (`uci_engine.cpp:122-165`):
  - `go`, then ` nodes N` and ` depth N` when set.
  - With st: ` movetime {fixed}` and nothing else.
  - Otherwise, if our tc is timed or has increment: ` wtime {w} btime {b}` (each only if that side is timed/inc), ` winc`, ` binc` (if >0).
  - ` movestogo {left}` if moves are set.
- **`position`**: `position startpos` or `position fen {fen}`, then ` moves …` with all moves including book moves.
- **Engine lifecycle:**
  - Spawn, `uci`, wait for `uciok` within `startup-ms`. This happens only once per cached engine.
  - Each game: `setoption name X value V` for each option, with `Threads` first (stable partition). Buttons only for value `true`, as `setoption name X`. Unknown/invalid options produce warnings. FRC adds `UCI_Chess960=true`.
  - Then `ucinewgame` + `isready` within `ucinewgame-ms`.
  - At shutdown: `stop`, `quit`.

---

## 9. Exit codes and stderr
- **Exit 0:**
  - normal finish
  - SPRT decision
  - `-help` / `-version`
  - no arguments (help)
- **Exit 1 (`EXIT_FAILURE`):**
  - any CLI or config error (message on **stdout**)
  - runtime exception
  - CTRL-C
  - `-strict` warning
  - engine startup failure
  - non-recover stall/crash
  - `Error while creating match`
  - invalid book FEN
- `--compliance ENGINE [ARGS]` returns `!compliant`.
- **stderr only:**
  - `Warning: Stats will be dropped for more than 2 engines.`
  - `Failed to open log file.`
  - Windows console setup errors (`Error: UTF-8 code page (65001) is not valid on this system.`, `Failed to set console output code page. Error code: {}`, and similar)
  - `GetLogicalProcessorInformationEx failed.`
  - `perror` for signal setup
- **Log file format** (only with `-log file=`):
  - `[{LABEL:<6}] [{HH:MM:SS.micro:>15}] <{tid:>3 on Windows, 20 elsewhere}> the reference --- {msg}`
  - With `engine=true`, engine traffic: `[Engine] [time] <tid> {name} <--- {cmd}` and `… {<stderr> }{name} ---> {line}`.
  - Messages from `Logger::print` are also logged, with a double newline.

**Platform-dependent or uncertain points to confirm on your target:**
- `nan` vs `-nan` in fmt output depends on CPU NaN sign (x86 usually gives `-nan`).
- Unordered-map iteration order affects:
  - the Player timeout/crash table order
  - the `stats` key order in the JSON
  - the tie-breaking of the "Did you mean" suggestion
