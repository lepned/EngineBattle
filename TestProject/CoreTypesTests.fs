module CoreTypesTests

open Xunit
open System
open ChessLibrary.TypesDef.TimeControl
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Misc
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef

// Test for EvalType
[<Fact>]
let ``EvalType ToString returns correct string`` () =
    Assert.Equal("0.00", (CP 0.0).ToString())
    Assert.Equal("M3", (Mate 3).ToString())
    Assert.Equal("-M3", (Mate -3).ToString())
    Assert.Equal("None", NA.ToString())

[<Fact>]
let ``EvalType Value returns correct value`` () =
    Assert.Equal(0.0, (CP 0.0).Value)
    Assert.Equal(3.0, (Mate 3).Value)
    Assert.Throws<System.Exception>(fun () -> NA.Value |> ignore)

[<Fact>]
let ``EvalType WinAdj and DrawAdj work correctly`` () =
    Assert.True((CP 0.6).WinAdj 0.5)
    Assert.False((CP 0.4).WinAdj 0.5)
    Assert.True((Mate 3).WinAdj 0.5)
    Assert.True((CP 0.4).DrawAdj 0.5)
    Assert.False((CP 0.6).DrawAdj 0.5)

// Test for EngineConfig
[<Fact>]
let ``EngineConfig Empty initializes correctly`` () =
    let config = EngineConfig.Empty
    Assert.Equal("", config.Name)
    Assert.Equal("UCI", config.Protocol)
    Assert.False(config.IsChallenger)

[<Fact>]
let ``EngineConfig Information returns correct string`` () =
    let config = { EngineConfig.Empty with Name = "TestEngine"; Protocol = "UCI" }
    let info = config.Information 100.0
    Assert.Contains("Protocol=UCI", info)

// Test for ResultReason
[<Fact>]
let ``ResultReason ToString returns correct abbreviation`` () =
    Assert.Equal("CM", Checkmate.ToString())
    Assert.Equal("SM", Stalemate.ToString())

[<Fact>]
let ``ResultReason Explanation returns correct description`` () =
    Assert.Equal("Checkmate", Checkmate.Explanation)
    Assert.Equal("Stalemate", Stalemate.Explanation)

// Test for stringToResultReason
[<Fact>]
let ``stringToResultReason parses valid strings`` () =
    Assert.Equal(Checkmate, stringToResultReason "CM")
    Assert.Equal(Stalemate, stringToResultReason "SM")

[<Fact>]
let ``stringToResultReason throws on invalid string`` () =
    Assert.Throws<System.Exception>(fun () -> stringToResultReason "INVALID" |> ignore)

// Test for GameMetadata
[<Fact>]
let ``GameMetadata Empty initializes correctly`` () =
    let metadata = GameMetadata.Empty
    Assert.Equal("", metadata.Event)
    Assert.Equal(Misc.ResultReason.NotStarted, metadata.Reason)

[<Fact>]
let ``GameMetadata Opening returns correct value`` () =
    let metadata = { GameMetadata.Empty with OtherTags = [{ Key = "Opening"; Value = "Sicilian Defense" }] }
    Assert.Equal("Sicilian Defense", metadata.Opening)

// Test for PgnGame
[<Fact>]
let ``PgnGame Empty initializes correctly`` () =
    let game = PgnGame.Empty 1
    Assert.Equal(1, game.GameNumber)
    Assert.Equal("", game.Fen)

// Test for WDLType
[<Fact>]
let ``WDLType Value returns correct WDL`` () =
    let wdl = HasValue { Win = 1.0; Draw = 0.5; Loss = 0.0 }
    Assert.Equal(1.0, wdl.Value().Win)
    Assert.Equal(WDL.Empty, NotFound.Value())

// Test for TimeControl
[<Fact>]
let ``TimeControl GetTimeConfig returns correct config`` () =
    let timeControl = { TimeConfigs = [{ Id = 1; Fixed = TimeOnly.MinValue; Increment = TimeOnly.MinValue; NodeLimit = false; Nodes = 0 }]; WmovesToGo = 0; BmovesToGo = 0 }
    let config = timeControl.GetTimeConfig 1
    Assert.Equal(1, config.Id)

[<Fact>]
let ``TimeControl ToString formats correctly`` () =
    let config1 = { Id = 1; Fixed = TimeOnly.FromTimeSpan(System.TimeSpan.FromSeconds(60.0)); Increment = TimeOnly.FromTimeSpan(System.TimeSpan.FromSeconds(1.0)); NodeLimit = false; Nodes = 0 }
    Assert.Equal("1' + 1''", config1.ToString())
    let config2 = { Id = 2; Fixed = TimeOnly.FromTimeSpan(System.TimeSpan.FromSeconds(90.0)); Increment = TimeOnly.FromTimeSpan(System.TimeSpan.FromSeconds(10.0)); NodeLimit = false; Nodes = 0 }
    Assert.Equal("1.5' + 10''", config2.ToString())
    let config3 = { Id = 2; Fixed = TimeOnly.FromTimeSpan(System.TimeSpan.FromMinutes(90.0)); Increment = TimeOnly.FromTimeSpan(System.TimeSpan.FromSeconds(30.0)); NodeLimit = false; Nodes = 0 }
    Assert.Equal("90' + 30''", config3.ToString())
