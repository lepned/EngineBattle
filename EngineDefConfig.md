# EngineDef.json Configuration

This document provides an overview of the `EngineDef.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for configuring a chess engine.

You rarely write one by hand. **Tools > Engine creator** in the GUI, and `mkdef` in the console,
both ask the engine itself: they start it, read what it answers to `uci` (name, author, every
option with its default) and write the def from that. The two share one implementation.

```bash
cd Console
dotnet run -c release -- mkdef C:/Engines/stockfish.exe --out C:/Chess/EngineDefs --tb D:/syzygy --uci Threads 8 --uci Hash 4096
dotnet run -c release -- mkdef C:/Engines/lc0.exe --net C:/Networks/BT4-332.pb.gz --out C:/Chess/EngineDefs
```

`--net` goes into the engine's own network option (Lc0's `WeightsFile`, Ceres' `Network` - found
on the option, not the engine's name) and its folder into `NetworkPath`; an engine without a
network option refuses it. `--tb` fills `SyzygyPath` when the engine has one and names no folder
yet. `--uci name value` overrides a default and must name an option the engine reported. `--base
def.json` carries the option values and the display fields of an existing def over onto a fresh
probe. `--print` shows the def instead of writing it; an existing file is never overwritten
without `--force`. The file is named after the engine without spaces (`Stockfish19.json`).

## Configuration Fields

### General Information

- **Name**: The full name of the chess engine.
- **Alias**: A short name or nickname for the engine.
- **Version**: The engine's version or additional descriptive information.
- **TimeControlID**: Identifier for the associated time control configuration defined in tournament.json.
- **Rating**: The engine's rating, typically represented by an Elo value.
- **Dev**: Information about the engine's developer(s).
- **LogoPath**: The file path for the engine's logo image.
- **Protocol**: The communication protocol (e.g., UCI, Winboard, XBoard) used by the engine.
- **DeviceOption** / **DeviceTemplate** (optional): for tournaments that run games in parallel on several GPUs. `DeviceOption` names the UCI option that selects the device and `DeviceTemplate` is its value with `{0}` standing for the GPU index - e.g. `"DeviceOption": "Device", "DeviceTemplate": "GPU:{0}#TensorRTNative"` for Ceres, or `"DeviceOption": "BackendOptions", "DeviceTemplate": "gpu={0}"` for Lc0. Each parallel worker gets the next id from `TestOptions.GPUs` in tournament.json; a value already present in `Options` has its numbers replaced in place. Left empty (the default), every game sends the option exactly as written.
- **IsChallenger** is not written in the file: EngineBattle sets it at run time for the challengers of a gauntlet.

### Winboard-Specific Configuration

For engines using the Winboard/XBoard protocol, you can optionally specify `WinboardConfig` to control protocol-specific behavior:

- **WinboardConfig** (optional): Configuration object for Winboard engines. If omitted, default values are used.
  - **SideToMovePOV** (bool): If `true`, the engine reports evaluation scores from side-to-move's perspective (requires conversion when Black to move). If `false` (default), the engine reports from White's perspective always. Default: `false`
  - **TimeControlStrategy** (string): Time control strategy for this engine. Default: `"LevelWithTime"`
    - `"LevelWithTime"`: Standard V2 approach (send `level` once per game + `time`/`otim` every move)
    - `"TimeOtimOnly"`: For V1 engines or engines with broken `level` command (send `time`/`otim` only) - Use for Comet, TheTurk
    - `"StWithTime"`: Safety mode (send `st` + `time`/`otim` for better time management)
    - `"StOnly"`: Legacy mode (send `st` only, may cause poor time management)
    - `"AutoDetect"`: Probe `level` command at runtime, fallback to `TimeOtimOnly` on error
  - **StartupCommands** (array of strings): Commands to send to the engine after initialization (after `post` and `easy`). Useful for engines requiring specific setup commands. Default: `[]`
  - **PreGoDelayMs** (int): pause between the time commands (`time`/`otim`, or `st`) and `go`, so an engine without `ping` support has read its clock before it starts thinking. Default: `100`; `0` sends `go` at once.
  - **ForceV1Mode** (bool): If `true`, forces Winboard V1 protocol mode (skips protover 2 negotiation, uses conservative defaults). Use this for very old engines that don't understand protover 2 at all. Default: `false`
  - **RequiresLevelForThinkingOutput** (bool): If `true`, sends a dummy `level 40 5 0` command at startup to enable standard thinking output format. Use this for engines that need `level` to enable output but have broken level-based time management (like Comet). **Note:** This is compatible with `TimeOtimOnly` strategy - the dummy level is sent only at startup, while time/otim are used during games. Default: `false`
  - **Use4FieldFen** (bool): If `true`, sends 4-field FEN format (position, side, castling, en passant) instead of full 6-field FEN (which includes halfmove clock and fullmove number) in `setboard` commands. Use this for very old engines that crash or hang on 6-field FEN (like TheTurk). Default: `false`

### Contempt Settings

- **ContemptEnabled**: Option to enable or disable contempt settings for the engine.
- **NegativeContemptAllowed**: Option to allow negative contempt values.

### Paths and Execution

- **Path**: The file system path to the engine's executable.
- **NetworkPath**: The directory where the engine neural network files reside.
- **Args**: Additional command-line arguments for the engine (optional).

### Options

- **Threads**: Number of threads the engine should use.
- **Hash**: Size of the hash memory in megabytes.
- **SyzygyPath**: The directory path for Syzygy tablebases.
- **Ponder**: Enables or disables the engine's pondering feature.
- **UCI_ShowWDL**: Determines whether the engine displays Windows/Draws/Loss statistics in UCI mode.
- Any other UCI option you want to pass to the engine.

## EngineDef.json Examples

### UCI Engine Example

```json
{
  "Name": "Stockfish 17",
  "Alias": "SF 17",
  "Version": "NNUE with 2 cores",
  "TimeControlID": 1,
  "Rating": 3600,
  "Dev": "by The Stockfish developers.",
  "LogoPath": "Img/SF-TCEC.png",
  "Protocol": "UCI",
  "ContemptEnabled": false,
  "NegativeContemptAllowed": false,
  "Path": "C:/Dev/Chess/Engines/SF/sf17.exe",
  "NetworkPath": "C:/Dev/Chess/Networks",
  "Args": null,
  "Options": {
    "Threads": 2,
    "Hash": 2048,
    "SyzygyPath": "C:/Dev/Chess/TableBases/syzygy",
    "Ponder": false,
    "UCI_ShowWDL": true
  }
}
```

### Winboard Engine Examples

#### Standard Winboard Engine (Crafty)

```json
{
  "Name": "Crafty",
  "Alias": "Crafty",
  "Version": "25.2",
  "TimeControlID": 1,
  "Rating": 2500,
  "Dev": "Robert Hyatt",
  "LogoPath": "Img/imgEB/Crafty.png",
  "Protocol": "Winboard",
  "ContemptEnabled": false,
  "NegativeContemptAllowed": false,
  "Path": "C:/Dev/Chess/Engines/Crafty.exe",
  "NetworkPath": "",
  "Args": "--xboard",
  "Options": {}
}
```

#### Winboard Engine with Broken Level Support (Comet)

Comet has broken `level` command for time management but requires it to enable standard thinking output. Use `TimeOtimOnly` strategy for time control + `RequiresLevelForThinkingOutput` flag.

```json
{
  "Name": "Comet",
  "Alias": "Comet",
  "Version": "B.68",
  "TimeControlID": 1,
  "Rating": 2200,
  "Dev": "N/A",
  "LogoPath": "Img/imgEB/Comet.png",
  "Protocol": "Winboard",
  "ContemptEnabled": false,
  "NegativeContemptAllowed": false,
  "Path": "C:/Dev/Chess/Engines/Comet_B68.exe",
  "NetworkPath": "",
  "Args": "",
  "Options": {},
  "WinboardConfig": {
    "SideToMovePOV": false,
    "TimeControlStrategy": "TimeOtimOnly",
    "RequiresLevelForThinkingOutput": true
  }
}
```

#### Winboard Engine with Startup Commands

```json
{
  "Name": "CustomEngine",
  "Alias": "Custom",
  "Version": "1.0",
  "TimeControlID": 1,
  "Rating": 2400,
  "Dev": "Developer Name",
  "LogoPath": "Img/engine.png",
  "Protocol": "Winboard",
  "ContemptEnabled": false,
  "NegativeContemptAllowed": false,
  "Path": "C:/Dev/Chess/Engines/Custom.exe",
  "NetworkPath": "",
  "Args": "",
  "Options": {},
  "WinboardConfig": {
    "SideToMovePOV": true,
    "TimeControlStrategy": "LevelWithTime",
    "StartupCommands": ["level 16"]
  }
}
```



