/// A tournament config needs only what has no default: what it leaves out is read from
/// Tournament.ConfigDefaults (Configuration.JSON.tryReadTournamentJson).
module TournamentConfigDefaultsTests

open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.Tournament

let private read (body: string) =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, body)
    try
        match Configuration.JSON.tryReadTournamentJson path with
        | Ok t -> t
        | Error msg -> failwithf "expected a parse, got: %s" msg
    finally File.Delete path

let private minimal (extra: string) =
    sprintf """{ "Name": "t", "TournamentMode": "RR", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] },
               "TimeControl": { "TimeConfigs": [ { "Id": 1, "Tc": "60+1" } ] }%s }""" extra

[<Fact>]
let ``a config without adjudication or mode sections reads, with adjudication off`` () =
    let t = read (minimal "")
    Assert.Equal(DrawOption.Off, t.Adjudication.DrawOption)
    Assert.Equal(WinOption.Off, t.Adjudication.WinOption)
    Assert.False t.Adjudication.TBAdj.UseTBAdjudication
    Assert.Equal(4, t.LadderOptions.GamePairsPerMatch)
    Assert.Equal(2, t.SwissOptions.GamesPerMatch)
    Assert.Equal(1, t.NumberOfGamesInParallel)
    Assert.Null t.GPUs
    Assert.Equal("", t.Site)

[<Fact>]
let ``a section that is given is read as written, field by field`` () =
    let t =
        read (minimal """, "Adjudication": { "DrawOption": { "MaxDrawScore": 0.2, "DrawMoveLength": 5 } },
                          "SwissOptions": { "Rounds": 3 }, "Site": "Oslo" """)
    // a draw section without MinDrawMove adjudicates from the first move, as it always has
    Assert.Equal({ Enabled = true; MinDrawMove = 0; MaxDrawScore = 0.2; DrawMoveLength = 5 }, t.Adjudication.DrawOption)
    // the sections it leaves out of Adjudication are off
    Assert.Equal(WinOption.Off, t.Adjudication.WinOption)
    Assert.False t.Adjudication.TBAdj.UseTBAdjudication
    // a mode section's missing fields are the documented defaults
    Assert.Equal((3, 2, 4), (t.SwissOptions.Rounds, t.SwissOptions.GamesPerMatch, t.SwissOptions.SeedGroupCount))
    Assert.Equal("Oslo", t.Site)

[<Fact>]
let ``an old TestOptions block reads as the top-level settings`` () =
    // both of the parallel setting's old names and the GPUs
    let t = read (minimal """, "TestOptions": { "NumberOfGamesInParallelConsoleOnly": 3, "GPUs": [0, 1], "PolicyTest": false, "ValueTest": false, "WriteToConsole": true }""")
    Assert.Equal(3, t.NumberOfGamesInParallel)
    Assert.Equal<int[]>([| 0; 1 |], t.GPUs)
    Assert.Equal(2, (read (minimal """, "TestOptions": { "NumberOfGamesInParallel": 2 }""")).NumberOfGamesInParallel)
    // a top-level key wins over the block
    Assert.Equal(4, (read (minimal """, "NumberOfGamesInParallel": 4, "TestOptions": { "NumberOfGamesInParallel": 2 }""")).NumberOfGamesInParallel)

[<Fact>]
let ``policy- and value-head play are time controls; an old file that asks for them is told so`` () =
    let told (extra: string) = (Assert.ThrowsAny<exn>(fun () -> read (minimal extra) |> ignore)).Message
    Assert.Contains("\"Nodes\": 1", told """, "TestOptions": { "PolicyTest": true }""")
    Assert.Contains("\"ValueHead\": true", told """, "TestOptions": { "ValueTest": true }""")

[<Fact>]
let ``a ValueHead time control has no clock and plays the value head`` () =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, """{ "Name": "t", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] },
                                 "TimeControl": { "TimeConfigs": [ { "Id": 1, "Tc": "60+1" }, { "Id": 2, "ValueHead": true } ] } }""")
    let t = try (match Configuration.JSON.tryReadTournamentJson path with Ok t -> t | Error e -> failwith e) finally File.Delete path
    let value = t.TimeControl.GetTimeConfig 2
    Assert.True value.ValueHead
    Assert.True value.NodeLimit
    Assert.Equal("value head", string value)
    Assert.Equal("-", value.PgnText 0)
    Assert.False (t.TimeControl.GetTimeConfig 1).ValueHead
    // no clock: a node limit's
    let side = ChessLibrary.Game.Clock.create t.TimeControl value System.TimeSpan.Zero
    Assert.True(match side.Limit with ChessLibrary.Game.Clock.Nodes _ -> true | _ -> false)
    Assert.Equal(ChessLibrary.Game.GoCommand.Value, ChessLibrary.Game.GoCommand.forMove false t.TimeControl value 0 side side)
    Assert.Equal(ChessLibrary.Game.GoCommand.NodeCount 2, ChessLibrary.Game.GoCommand.forMove true t.TimeControl value 0 side side)

[<Fact>]
let ``a null in the file is the default`` () =
    let t = read (minimal """, "LadderOptions": null, "ReferencePGNPath": null""")
    Assert.Equal(4, t.LadderOptions.GamePairsPerMatch)
    Assert.Equal("", t.ReferencePGNPath)

[<Fact>]
let ``draw and win adjudication have a switch, on when a section leaves it out`` () =
    let t = read (minimal """, "Adjudication": { "DrawOption": { "Enabled": false, "MinDrawMove": 20, "MaxDrawScore": 0.2, "DrawMoveLength": 5 },
                                                 "WinOption": { "MinWinMove": 0, "MinWinScore": 5.0, "WinMoveLength": 5 } }""")
    Assert.False t.Adjudication.DrawOption.Enabled
    Assert.True t.Adjudication.WinOption.Enabled

[<Fact>]
let ``tablebase adjudication is on when it names a folder, unless it says otherwise`` () =
    let tb (section: string) = (read (minimal (sprintf """, "Adjudication": { "TBAdj": %s }""" section))).Adjudication.TBAdj
    Assert.True (tb """{ "TablebaseDirectory": "D:/syzygy" }""").UseTBAdjudication
    Assert.False (tb """{ "TablebaseDirectory": "D:/syzygy", "UseTBAdjudication": false }""").UseTBAdjudication
    Assert.False (tb """{ "TablebaseDirectory": "" }""").UseTBAdjudication
    // TBMen left out: as many as the tables hold
    Assert.Equal(0, (tb """{ "TablebaseDirectory": "D:/syzygy" }""").TBMen)

let private errorsOf (t: Tournament) =
    match Configuration.Validation.validateTournament t with
    | Configuration.Validation.Errors messages -> messages
    | _ -> []

[<Fact>]
let ``a config without a time control is told so, not a NullReference`` () =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, """{ "Name": "t", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] } }""")
    let t = try (match Configuration.JSON.tryReadTournamentJson path with Ok t -> t | Error e -> failwith e) finally File.Delete path
    let errors = errorsOf t
    Assert.Contains(errors, fun m -> m.StartsWith "TimeControl is missing")
    Assert.Contains(errors, fun m -> m.Contains "EngineSetup.EngineDefList")
    // with engines, each of which names a time control the file does not have
    let engine name = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name; TimeControlID = 1 }
    let withEngines = { t with EngineSetup = { t.EngineSetup with Engines = [ engine "A"; engine "B" ] } }
    Assert.Contains(errorsOf withEngines, fun m -> m.StartsWith "TimeControl is missing")

[<Fact>]
let ``a short delay between games is a hint for the WebGUI, and nothing for the console`` () =
    let t = { read (minimal "") with DelayBetweenGames = System.TimeSpan.Zero }
    Assert.DoesNotContain(errorsOf t, fun m -> m.Contains "Delay")
    Assert.NotEmpty(Configuration.Validation.delayWarning t)
    Assert.Empty(Configuration.Validation.delayWarning { t with ConsoleOnly = true })

[<Fact>]
let ``an older shape: a top-level null is no value, a flag that is not a bool is an error`` () =
    let t = read (minimal """, "NumberOfGamesInParallel": null, "TestOptions": { "NumberOfGamesInParallel": 3 }""")
    Assert.Equal(3, t.NumberOfGamesInParallel)
    let ex = Assert.ThrowsAny<exn>(fun () -> read (minimal """, "TestOptions": { "ValueTest": "true" }""") |> ignore)
    Assert.Contains("TestOptions.ValueTest must be true or false", ex.Message)

[<Fact>]
let ``a cup of three is refused by the validation, as the cup runner refuses it`` () =
    let engine name = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name; TimeControlID = 1 }
    let cup names =
        let t = { read (minimal "") with TournamentMode = "Cup" }
        { t with EngineSetup = { t.EngineSetup with Engines = [ for n in names -> engine n ] } }
    Assert.Contains(errorsOf (cup [ "A"; "B"; "C" ]), fun m -> m.Contains "power-of-two number of players" && m.Contains "has 3")
    Assert.DoesNotContain(errorsOf (cup [ "A"; "B"; "C"; "D" ]), fun m -> m.Contains "power-of-two")

[<Fact>]
let ``a gauntlet is a gauntlet in any case`` () =
    let engine name = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name; TimeControlID = 1 }
    let t = { read (minimal "") with TournamentMode = "gauntlet"; GauntletOptions = { GauntletOptions.Default with Challengers = 2 } }
    let t = { t with EngineSetup = { t.EngineSetup with Engines = [ engine "A"; engine "B" ] } }
    Assert.Contains(errorsOf t, fun m -> m.Contains "challengers")

let private gauntlet (extra: string) =
    sprintf """{ "Name": "t", "TournamentMode": "Gauntlet", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] },
               "TimeControl": { "TimeConfigs": [ { "Id": 1, "Tc": "60+1" } ] }%s }""" extra

[<Fact>]
let ``a gauntlet without GauntletOptions is one challenger, shared openings in the book's order`` () =
    let t = read (gauntlet "")
    Assert.Equal(GauntletOptions.Default, t.GauntletOptions)
    Assert.False t.GauntletOptions.Spread

[<Fact>]
let ``GauntletOptions is read as written`` () =
    let t = read (gauntlet """, "GauntletOptions": { "Challengers": 2, "RandomOpenings": true, "Distribution": "Spread" } """)
    Assert.Equal(2, t.GauntletOptions.Challengers)
    Assert.True t.GauntletOptions.RandomOpenings
    Assert.True t.GauntletOptions.Spread
    Assert.True t.RandomOpeningsForMode
    // in a gauntlet Opening.RandomOpenings is not read any more
    let t = read (gauntlet """, "GauntletOptions": { "Challengers": 1 }, "Opening": { "RandomOpenings": true } """)
    Assert.False t.GauntletOptions.RandomOpenings
    Assert.False t.RandomOpeningsForMode

/// The older shape: a top-level Challengers, and Opening.RandomOpenings meaning both the random
/// draw and the spread distribution.
[<Fact>]
let ``an older gauntlet file's Challengers and RandomOpenings fill GauntletOptions`` () =
    let t = read (gauntlet """, "Challengers": 3, "Opening": { "RandomOpenings": true } """)
    Assert.Equal(3, t.GauntletOptions.Challengers)
    Assert.True t.GauntletOptions.RandomOpenings
    Assert.True t.GauntletOptions.Spread
    let t = read (gauntlet """, "Challengers": 2, "Opening": { "RandomOpenings": false } """)
    Assert.Equal(2, t.GauntletOptions.Challengers)
    Assert.False t.GauntletOptions.RandomOpenings
    Assert.False t.GauntletOptions.Spread
    // a given block wins over the old fields
    let t = read (gauntlet """, "Challengers": 3, "GauntletOptions": { "Challengers": 1 } """)
    Assert.Equal(1, t.GauntletOptions.Challengers)
    // a round robin's Opening.RandomOpenings stays its own and does not touch the gauntlet block
    let t = read (minimal """, "Challengers": 2, "Opening": { "RandomOpenings": true } """)
    Assert.True t.Opening.RandomOpenings
    Assert.True t.RandomOpeningsForMode
    Assert.False t.GauntletOptions.RandomOpenings

[<Fact>]
let ``a gauntlet refuses no challengers and an unknown distribution`` () =
    let engine name = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name; TimeControlID = 1 }
    let withEngines t = { t with EngineSetup = { t.EngineSetup with Engines = [ engine "A"; engine "B"; engine "C" ] } }
    let t = withEngines (read (gauntlet """, "GauntletOptions": { "Challengers": 0 } """))
    Assert.Contains(errorsOf t, fun m -> m.Contains "Challengers must be at least 1")
    let t = withEngines (read (gauntlet """, "GauntletOptions": { "Distribution": "random" } """))
    Assert.Contains(errorsOf t, fun m -> m.Contains "Distribution must be")

[<Fact>]
let ``GauntletOptions.ChallengerColor: Black first when not given, white or black as written, anything else refused`` () =
    let engine name = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name; TimeControlID = 1 }
    let withEngines t = { t with EngineSetup = { t.EngineSetup with Engines = [ engine "A"; engine "B"; engine "C" ] } }
    Assert.False (read (gauntlet "")).GauntletOptions.ChallengerWhite
    Assert.False (read (gauntlet """, "GauntletOptions": { "Challengers": 1 } """)).GauntletOptions.ChallengerWhite
    Assert.True (read (gauntlet """, "GauntletOptions": { "ChallengerColor": "White" } """)).GauntletOptions.ChallengerWhite
    let black = withEngines (read (gauntlet """, "GauntletOptions": { "ChallengerColor": "black" } """))
    Assert.False black.GauntletOptions.ChallengerWhite
    Assert.DoesNotContain(errorsOf black, fun m -> m.Contains "ChallengerColor")
    let t = withEngines (read (gauntlet """, "GauntletOptions": { "ChallengerColor": "random" } """))
    Assert.Contains(errorsOf t, fun m -> m.Contains "ChallengerColor must be")

[<Fact>]
let ``engine defs that are not there are named together, with the folder they were looked for in`` () =
    let folder = Path.Combine(Path.GetTempPath(), "eb_defs_" + System.Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory folder |> ignore
    try
        let ex = Assert.ThrowsAny<exn>(fun () -> Configuration.JSON.readEngineDefs folder [ "NoSuch.json"; "AlsoMissing.json" ] |> ignore)
        Assert.Contains("engine def not found: NoSuch.json", ex.Message)
        Assert.Contains("engine def not found: AlsoMissing.json", ex.Message)
        Assert.Contains(folder, ex.Message)
        Assert.DoesNotContain("Complete error message", ex.Message)
    finally Directory.Delete(folder, true)
