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
| `san=1` | Engine uses SAN notation | Use coordinate notation |
| `ping=1` | Supports `ping`/`pong` sync | Use timing-based sync |
| `time=1` | Engine manages its own clock | Send time updates anyway |
| `playother=1` | Supports `playother` command | Use `force` + move instead |
| `analyze=1` | Supports analysis mode | Analysis may not work |
| `myname="X"` | Engine's display name | Use config name |
| `done=1` | Feature negotiation complete | Timeout triggers V1 fallback |

### V1 Fallback

If no `done=1` is received within 2 seconds:
1. Engine is assumed to be V1
2. Conservative defaults are used
3. `setboard` support is probed by sending a test position

## Time Control Strategies

Winboard time control is complex because engines interpret commands differently.

### Available Strategies

Configure via `WinboardConfig.TimeControlStrategy` in engine config:

| Strategy | Commands Sent | When to Use |
|----------|---------------|-------------|
| `LevelWithTime` | `level` once + `time`/`otim` per move | Modern engines (default) |
| `TimeOtimOnly` | `time`/`otim` only | Engines with broken `level` (Comet, TheTurk) |
| `StWithTime` | `st` + `time`/`otim` | Safety mode for problematic engines |
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

## Common Issues and Solutions

### Engine doesn't start thinking

**Symptoms:** Engine accepts position but never returns a move.

**Possible causes:**
1. Time control not understood - try `TimeOtimOnly` strategy
2. Missing `go` equivalent - check if engine needs specific command
3. Engine waiting for `level` - set `RequiresLevelForThinkingOutput: true`

### Engine returns invalid moves

**Symptoms:** Engine returns moves in wrong format.

**Possible causes:**
1. Engine uses SAN but `san=0` was negotiated
2. Move notation includes unexpected prefixes (e.g., `1. e4`)

### Engine crashes on position setup

**Symptoms:** Engine exits when receiving `setboard`.

**Possible causes:**
1. FEN format incompatible - try `Use4FieldFen: true` for old engines
2. Engine doesn't support `setboard` - feature negotiation failed

### Evaluation scores are inverted

**Symptoms:** Winning positions show as losing.

**Possible causes:**
1. Engine reports from side-to-move perspective - set `SideToMovePOV: true`

### Engine ignores time pressure

**Symptoms:** Engine uses same time regardless of clock.

**Possible causes:**
1. `level` command broken - use `TimeOtimOnly` strategy
2. Engine needs `st` command - use `StWithTime` strategy

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

### Very Old Engine (TheTurk)
```json
{
  "Name": "TheTurk",
  "Protocol": "Winboard",
  "Path": "C:/Engines/TheTurk.exe",
  "WinboardConfig": {
    "TimeControlStrategy": "TimeOtimOnly",
    "Use4FieldFen": true,
    "ForceV1Mode": true
  }
}
```

### Engine with Side-to-Move Eval
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

## See Also

- [EngineDefConfig.md](EngineDefConfig.md) - Complete engine configuration reference
- [TournamentConfig.md](TournamentConfig.md) - Tournament configuration
- [CECP Specification](https://www.gnu.org/software/xboard/engine-intf.html) - Official protocol spec
