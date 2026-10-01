# Winboard/XBoard Protocol Guide

This document explains how EngineBattle supports the Winboard/XBoard protocol, including feature negotiation, common issues, and troubleshooting.

## Protocol Overview

### UCI vs Winboard

| Aspect | UCI | Winboard/XBoard |
|--------|-----|-----------------|
| **Origin** | Modern (2000+) | Legacy (1991+) |
| **Standardization** | Well-defined spec | CECP spec with many engine variations |
| **Position setup** | `position fen ... moves ...` | `setboard` or replay all moves |
| **Thinking output** | `info depth X score cp Y nodes Z pv ...` | `depth score time nodes pv...` (varies) |
| **Time control** | `go wtime X btime Y winc Z binc W` | `level`, `time`, `otim`, `st` commands |
| **Move format** | Coordinate notation (`e2e4`) | Varies (coordinate, SAN, with/without `usermove`) |

### Protocol Versions

**Winboard V1** (original):
- No feature negotiation
- Conservative defaults assumed
- May not support `setboard`

**Winboard V2** (protover 2):
- Feature negotiation via `feature` command
- Engine declares capabilities (setboard, usermove, san, etc.)
- Ends with `done=1`

EngineBattle auto-detects the protocol version and falls back to V1 if needed.

## Feature Negotiation

When EngineBattle starts a Winboard engine, it sends:
```
xboard
protover 2
```

The engine should respond with feature lines, for example:
```
feature ping=1 setboard=1 playother=1 san=0 usermove=1 time=1 done=1
```

### Supported Features

| Feature | Description | Impact if Missing |
|---------|-------------|-------------------|
| `setboard=1` | Engine supports `setboard <FEN>` | Must replay all moves from start |
| `usermove=1` | Moves prefixed with `usermove` | Send bare coordinate moves |
| `san=1` | Engine wants moves sent in SAN | Rejected: EngineBattle sends coordinate moves, and the `rejected san` reply tells the engine so |
| `ping=1` | Supports `ping`/`pong` sync | Use timing-based sync |
| `time=1` | Engine manages its own clock | Send time updates anyway |
| `playother=1` | Supports `playother` command | Parsed only; EngineBattle always uses `force` + move |
| `analyze=1` | Supports analysis mode | Analysis may not work |
| `myname="X"` | Engine's display name | Use config name |
| `done=1` | Feature negotiation complete | Timeout: V1 fallback if no features arrived, otherwise the partial features are accepted |
| `done=0` | Engine needs longer to start | EngineBattle waits up to 10 minutes for `done=1` |

Every feature is answered, as the protocol asks: `accepted ping`, `accepted setboard`, and so on.
A feature EngineBattle does not know, and `san=1`, get `rejected`. An engine that answers a reply
with `Error (unknown command)` is harmless; it goes to the log as a warning.

### V1 Fallback

If no `done=1` is received within 8 seconds (an engine that sent `done=0` gets 10 minutes):
1. If no `feature` line arrived at all, the engine is assumed to be V1; if some did, they are accepted as-is and the engine is treated as V2
2. Conservative defaults are used
3. `setboard` support is probed by sending a test position

## Time Control Strategies

Winboard time control is complex because engines interpret commands differently.

### Available Strategies

Configure via `WinboardConfig.TimeControlStrategy` in engine config:

| Strategy | Commands Sent | When to Use |
|----------|---------------|-------------|
| `LevelWithTime` | `level` once + `time`/`otim` per move | Modern engines (default) |
| `TimeOtimOnly` | `time`/`otim` only | Engines with broken `level` (Comet) |
| `StWithTime` | `st` + `time`/`otim` | Engines that ignore `time`/`otim` (TheTurk) |
| `StOnly` | `st` only | Very old engines (may cause poor time use) |
| `AutoDetect` | Probe `level`, fallback if error | Unknown engines |

### Time Command Details

**`level MPS BASE INC`** - Set time control:
- `level 0 5 0` = 5 minutes per game, no increment
- `level 40 5 0` = 40 moves in 5 minutes, then repeat
- `level 0 0:30 1` = 30 seconds + 1 second increment

**`time N`** - Engine's remaining time in centiseconds
**`otim N`** - Opponent's remaining time in centiseconds
**`st N`** - Think for exactly N seconds

## What EngineBattle Reads From the Engine

**Its move**, in the two forms the protocol defines and no other:
- `move e2e4`
- `1. ... e2e4` (or `1...e2e4`) - the older form (Comet). The `...` is required even when the
  engine plays White; `12. e4` without it is not a move.

The move may be in coordinates (`e2e4`, `e7e8q`, `e7e8=q`; a pawn's `e7e8` without the piece is
a queen) or SAN (`Nf3`, `O-O`, `0-0`). A move that is not legal in the position the engine was
given is ignored with a warning - it is left over from an earlier search, and the engine's real
move follows. Other lines are not taken for moves, even with a move in them (`Hint: e7e5`, a
printed variation). A line that looks like a move in another form (`12. e4`, `My move is: e2e4`,
a move alone on its line) gets one warning in the log, so an engine that announces its moves that
way is seen for what it is.

**Its thinking lines** (`depth score time nodes pv`): the PV may be in SAN or coordinates, with
move numbers. Marks on the moves are dropped - Comet's `g1f3?` and `b1c3!`, check signs, Crafty's
`<HT>`, `ep` - castling with zeros (`0-0`, TheTurk) is read as `O-O`, and a promotion without its
piece (`a7a8`, Comet) as a queen. The PV ends at the first move that does not fit the position.

**A resignation**: `resign`, or a claim of its own loss made on its move (`0-1 {White resigns}`
from White). The game ends as a resignation (reason `RS`). Before, EngineBattle ignored it and the
engine lost on time once its clock ran out. A win or a draw the engine claims (`1-0 {White
mates}`) is ignored: EngineBattle judges the position itself.

**An error**: `Illegal move ...` (however it is spelled - `illegal move`, with or without the
colon) or `Error ...` during a game goes to the log as a warning naming
the position sent last, once per message. An engine that refuses its position or a time command
then usually sits silent until it loses on time, and this line says why.

## Common Issues and Solutions

### Engine doesn't start thinking

**Symptoms:** Engine accepts position but never returns a move.

**Possible causes:**
1. The engine refused a command - look in the log for `rejected a command`, which names the
   position it was sent
2. Time control not understood - try `TimeOtimOnly` strategy
3. Missing `go` equivalent - check if engine needs specific command
4. Engine waiting for `level` - set `RequiresLevelForThinkingOutput: true`

### Engine returns invalid moves

**Symptoms:** Engine returns moves in wrong format.

**Possible causes:**
1. The engine announces its move in a form EngineBattle does not read (see
   [What EngineBattle Reads From the Engine](#what-enginebattle-reads-from-the-engine)) - look in
   the log for `looks like a move but is not in a form the Winboard protocol defines`

### Engine crashes on position setup

**Symptoms:** Engine exits when receiving `setboard`.

**Possible causes:**
1. FEN format incompatible - try `Use4FieldFen: true` for old engines
2. Engine doesn't support `setboard` - feature negotiation failed

### Evaluation scores are inverted

**Symptoms:** Winning positions show as losing.

**Possible causes:**
1. Engine reports from White's point of view (Crafty does) rather than the side to move - set `SideToMovePOV: true`

### Engine ignores time pressure

**Symptoms:** Engine uses same time regardless of clock.

**Possible causes:**
1. `level` command broken - use `TimeOtimOnly` strategy
2. Engine needs `st` command - use `StWithTime` strategy

### Engine loses on time when the opponent has much more time left

**Symptoms:** The engine thinks too long when it is behind on the clock and loses on time.

**Cause:** Some engines budget their move from `otim` (the opponent's clock) rather than from
their own `time` (Comet). EngineBattle sends both clocks as they are. Give such an engine a
longer time control.

## Diagnostic Tools

### Manual Testing

To manually test a Winboard engine:

```
# Start engine
xboard
protover 2

# Wait for features, then:
new
force
setboard rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1

# Set time and start thinking
time 30000
otim 30000
go
```

### Reading Engine Output

Standard Winboard thinking output format:
```
depth score time nodes pv
  8    45   123  50000 e4 e5 Nf3 Nc6
```

Where:
- `depth` = search depth (plies)
- `score` = centipawns (from White's perspective usually)
- `time` = centiseconds
- `nodes` = nodes searched
- `pv` = principal variation (may be SAN or coordinate)

## Configuration Examples

### The WinboardConfig fields

Everything the `WinboardConfig` block of an engine def can hold; the examples below show
the ones that matter for each engine. Full descriptions in [EngineDefConfig.md](EngineDefConfig.md).

- **TimeControlStrategy**: `LevelWithTime` (default), `AutoDetect`, `TimeOtimOnly`, `StWithTime`, `StOnly` - see *Time Control Strategies* above.
- **SideToMovePOV**: `true` when the engine reports its scores from White's point of view, as Crafty does, instead of the side to move that the protocol expects (default `false`). The name says the opposite of what it does; it is kept so existing defs keep working.
- **RequiresLevelForThinkingOutput**: `true` for engines that print no thinking lines until a `level` command has been sent (default `false`).
- **Use4FieldFen**: `true` for engines whose `setboard` rejects the half-move and full-move fields (default `false`).
- **ForceV1Mode**: skip `protover 2` negotiation for engines that predate it (default `false`).
- **StartupCommands**: extra commands sent once after `post` and `easy`, e.g. `["level 16"]` (default `[]`).
- **PreGoDelayMs**: pause between the time commands and `go` for engines without `ping` support (default `100`; `0` = none). An engine with `ping` gets `go` at once; one that still needs a pause gets it from `CommandDelayMs`.
- **MinLevelIncrement**: the least increment, in whole seconds, the `level` command states (default `0`). `level` takes whole seconds and the increment is rounded down - 0.1 s becomes 0 - so an engine that only searches with an increment of at least 1 gets `1` here (Jonny plays instantly otherwise). A game without any increment (10+0) is sent as it is.
- **CommandDelayMs**: pause in milliseconds before each line after the first when EngineBattle sends several at once - `force`, `setboard`, `st`, `time`, `otim` (default `0`). For an engine that fails when commands arrive together (TheTurk). The clock starts after `go`, so the pause costs the engine no time.


The examples show only the fields that matter for each engine; a def that loads also needs the required fields of every engine def (`TimeControlID`, `Version`, `Rating`, `LogoPath`, `NetworkPath`, `Options`), see [EngineDefConfig.md](EngineDefConfig.md).

### Standard Engine (Crafty)
```json
{
  "Name": "Crafty",
  "Protocol": "Winboard",
  "Path": "C:/Engines/Crafty.exe"
}
```

### Engine with Broken Level (Comet)
```json
{
  "Name": "Comet",
  "Protocol": "Winboard",
  "Path": "C:/Engines/Comet.exe",
  "WinboardConfig": {
    "TimeControlStrategy": "TimeOtimOnly",
    "RequiresLevelForThinkingOutput": true
  }
}
```

Comet (B.68) never answers in under about a quarter of a second, whatever its clock says: told it
has 0.1 s left, it still searches for 0.26 s (in the middlegame up to a second). At blitz it loses
on time - at 20+0.2 about half its games, the same with `LevelWithTime` and with no pause before
`go`; at 60+1 none. Give it 60+1 or longer. It announces its move as `1. ... e2e4` rather than
`move e2e4`, which EngineBattle reads.

### Very Old Engine (TheTurk)
```json
{
  "Name": "TheTurk",
  "Protocol": "Winboard",
  "Path": "C:/Engines/TheTurk.exe",
  "WinboardConfig": {
    "TimeControlStrategy": "StWithTime",
    "Use4FieldFen": true,
    "ForceV1Mode": true,
    "CommandDelayMs": 20
  }
}
```

TheTurk ignores `time`/`otim` and misreads `level` with minutes and seconds, so `st` (whole
seconds a move) is the only control it keeps; with `TimeOtimOnly` it thinks 10 s a move whatever
the clock. EngineBattle works the `st` out from the time left and the increment, rounded down so
the engine never gets more than it has, and never more than half the time left: 30+0.5 gives
`st 1`, 60+1 `st 2`. Under a second a move -
10+0.1, or a short clock late in a game - it is `st 0`, a depth-1 move, so give it 30+0.5 or
longer.

TheTurk also crashes now and then (a `NullReferenceException` in its own command queue) when the
position and time commands arrive in one burst - about 3 to 7 game starts in 100. `CommandDelayMs:
20` spaces them out; with it, none crashed in 400 starts.

### Engine that reports from White's point of view
```json
{
  "Name": "CustomEngine",
  "Protocol": "Winboard",
  "Path": "C:/Engines/Custom.exe",
  "WinboardConfig": {
    "SideToMovePOV": true
  }
}
```

### Engine that needs a whole-second increment (Jonny)
```json
{
  "Name": "Jonny",
  "Protocol": "Winboard",
  "Path": "C:/Engines/Jonny.exe",
  "WinboardConfig": {
    "MinLevelIncrement": 1
  }
}
```

### Engines too slow for blitz

Some engines have a floor under their move time, whatever the clock says, and lose on time at
fast controls: Comet (about 0.25 s, see above) and TheKing (about 0.43 s; it lost 4 of 4 at
10+0.1 and none at 30+0.5). Give them 30+0.5 or longer.

## See Also

- [EngineDefConfig.md](EngineDefConfig.md) - Complete engine configuration reference
- [TournamentConfig.md](TournamentConfig.md) - Tournament configuration
- [CECP Specification](https://www.gnu.org/software/xboard/engine-intf.html) - Official protocol spec
