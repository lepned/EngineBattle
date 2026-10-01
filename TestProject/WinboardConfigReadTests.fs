/// An engine def's WinboardConfig block is laid over WinboardConfig.Default
/// (Configuration.JSON.WinboardConfigConverter): what the block leaves out keeps its default.
module WinboardConfigReadTests

open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes

let private readDef (winboardBlock: string) =
    let json =
        sprintf """{ "Name": "X", "TimeControlID": 1, "Version": "1", "Rating": 1, "LogoPath": "", "Protocol": "Winboard",
                     "Path": "x.exe", "NetworkPath": "", "Options": {}, "WinboardConfig": %s }""" winboardBlock
    let path = Path.GetTempFileName()
    File.WriteAllText(path, json)
    try
        match (Configuration.JSON.readSingleEngineConfig path).WinboardConfig with
        | Some w -> w
        | None -> failwith "expected a WinboardConfig"
    finally File.Delete path

[<Fact>]
let ``a block that sets one field keeps every other default`` () =
    // the Winboard.md example for an engine with a side-to-move eval
    let w = readDef """{ "SideToMovePOV": true }"""
    Assert.Equal({ WinboardConfig.Default with SideToMovePOV = true }, w)
    Assert.Equal(100, w.PreGoDelayMs)
    Assert.Equal(LevelWithTime, w.TimeControlStrategy)
    Assert.NotNull(box w.StartupCommands)

[<Fact>]
let ``fields that are given win over the defaults`` () =
    let w = readDef """{ "TimeControlStrategy": "TimeOtimOnly", "Use4FieldFen": true, "ForceV1Mode": true,
                         "StartupCommands": [ "level 16" ], "PreGoDelayMs": 0 }"""
    Assert.Equal(TimeOtimOnly, w.TimeControlStrategy)
    Assert.True(w.Use4FieldFen && w.ForceV1Mode)
    Assert.Equal<string list>([ "level 16" ], w.StartupCommands)
    Assert.Equal(0, w.PreGoDelayMs)   // an explicit 0 is kept, not taken for "missing"

[<Fact>]
let ``an empty block is the default`` () =
    Assert.Equal(WinboardConfig.Default, readDef "{}")

[<Fact>]
let ``the per-engine workarounds are read, and written whole so they read back the same`` () =
    // Jonny's and TheTurk's settings (Winboard.md)
    let w = readDef """{ "MinLevelIncrement": 1, "CommandDelayMs": 20 }"""
    Assert.Equal({ WinboardConfig.Default with MinLevelIncrement = 1; CommandDelayMs = 20 }, w)
    let options = System.Text.Json.JsonSerializerOptions()
    options.Converters.Add(Configuration.JSON.WinboardConfigConverter())
    options.Converters.Add(TimeControlStrategyConverter())
    let written = System.Text.Json.JsonSerializer.Serialize(w, options)
    Assert.Contains("\"CommandDelayMs\":20", written)
    Assert.Equal(w, readDef written)
