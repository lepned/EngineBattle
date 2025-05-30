# EngineDef.json Configuration

This document provides an overview of the `EngineDef.json` configuration file used in the EngineBattle application. This file defines the settings and parameters for configuring a chess engine.

## Configuration Fields

### General Information

- **Name**: The full name of the chess engine.
- **Alias**: A short name or nickname for the engine.
- **Version**: The engine's version or additional descriptive information.
- **TimeControlID**: Identifier for the associated time control configuration defined in tournament.json.
- **Rating**: The engine's rating, typically represented by an Elo value.
- **Dev**: Information about the engine's developer(s).
- **LogoPath**: The file path for the engine's logo image.
- **Protocol**: The communication protocol (e.g., UCI) used by the engine.

### Contempt Settings

- **ContemptEnabled**: Option to enable or disable contempt settings for the engine.
- **NegativeContemptAllowed**: Option to allow negative contempt values.

### Paths and Execution

- **Path**: The file system path to the engine's executable.
- **NetworkPath**: The directory where the engine�s neural network files reside.
- **Args**: Additional command-line arguments for the engine (optional).

### Options

- **Threads**: Number of threads the engine should use.
- **Hash**: Size of the hash memory in megabytes.
- **SyzygyPath**: The directory path for Syzygy tablebases.
- **Ponder**: Enables or disables the engine's pondering feature.
- **UCI_ShowWDL**: Determines whether the engine displays Windows/Draws/Loss statistics in UCI mode.
- Any other UCI option you want to pass to the engine.

## EngineDef.json Example - Copy This as Template

```
{
  "Name": "Stockfish 17",
  "Alias": "SF 17",
  "Version": "NNUE with 2 cores",
  "TimeControlID": 1,
  "Rating": 3600,
  "Dev": "by The Stockfish developers.",
  "LogoPath": "img/SF-TCEC.png",
  "Protocol": "UCI",
  "ContemptEnabled": false,
  "NegativeContemptAllowed": false,
  "Path": "C:/Dev/Chess/Engines/SF/sf17.exe",
  "NetworkPath": "C:/Dev/Chess/Networks",
  "Args": null,
  "Options": {
    "Threads": 2,
    "Hash": 2048,
    "SyzygyPath": "C:/Dev/Chess/TableBases/sygyzy",
    "Ponder": false,
    "UCI_ShowWDL": true
  }
}

```

