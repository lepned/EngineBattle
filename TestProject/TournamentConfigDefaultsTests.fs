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
    Assert.Equal(1, t.TestOptions.NumberOfGamesInParallel)
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
let ``TestOptions keep the old name of the parallel setting`` () =
    let t = read (minimal """, "TestOptions": { "NumberOfGamesInParallelConsoleOnly": 3 }""")
    Assert.Equal(3, t.TestOptions.NumberOfGamesInParallel)

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
