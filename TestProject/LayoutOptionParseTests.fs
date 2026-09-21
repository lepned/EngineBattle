/// tournament.json's LayoutOption block is optional, and so is every field in it: what a
/// file leaves out is the default, the same as leaving the whole block out. These tests pin
/// that contract at the parser, because System.Text.Json on its own gives an absent field
/// the type's zero (false, 0, null) - which once switched the PV boards off for any file
/// that set nothing but a logo size.
module LayoutOptionParseTests

open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef

let private minimal = """"Name": "t", "TournamentMode": "RR", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] }"""

/// Writes a file with the given LayoutOption block (None = no block) and reads it back the
/// way the GUI and the console do.
let private parse (layoutBlock: string option) =
    let body =
        match layoutBlock with
        | Some block -> sprintf "{ %s, \"LayoutOption\": %s }" minimal block
        | None -> sprintf "{ %s }" minimal
    let path = Path.GetTempFileName()
    File.WriteAllText(path, body)
    try Configuration.JSON.tryReadTournamentJson path
    finally File.Delete path

let private layout block =
    match parse block with
    | Ok t -> LayoutTypes.LayoutOption.Normalize t.LayoutOption
    | Error msg -> failwithf "expected a parse, got: %s" msg

[<Fact>]
let ``no LayoutOption block is the default`` () =
    Assert.Equal(LayoutTypes.LayoutOption.Default, layout None)

[<Fact>]
let ``a block with only a logo size keeps every other default`` () =
    let lo = layout (Some """{ "Sizes": { "MainLogoSize": "300x160" } }""")
    Assert.Equal("300x160", lo.Sizes.MainLogoSize)
    Assert.Equal(LayoutTypes.LayoutOption.Default.Sizes.LogoSize, lo.Sizes.LogoSize)
    Assert.Equal("small", lo.Sizes.PVboardSize)
    Assert.Equal(200, lo.Sizes.LiveChartHeight)
    Assert.True(lo.ShowPVBoard, "the PV boards must stay on")
    Assert.True(lo.ShowCrosstableBetweenGames)
    Assert.Equal(30, lo.AutoCycleTimeInSec)
    Assert.Equal(LayoutTypes.LayoutOption.Default.Fonts, lo.Fonts)
    Assert.Equal(LayoutTypes.Charts.Default, lo.Charts)
    Assert.Equal(LayoutTypes.CrosstableWithStandings.Hidden, lo.CrosstableWithStandings)

[<Fact>]
let ``a partial Charts block fills the rest from the default`` () =
    let lo = layout (Some """{ "Charts": { "ShowTime": true } }""")
    Assert.True(lo.Charts.ShowTime)
    Assert.True(lo.Charts.ShowEval)
    Assert.Equal(5, lo.Charts.NumberOfLines)

[<Fact>]
let ``a partial Fonts block keeps its zeros, so a region not given still reads as not set`` () =
    // The GUI falls back per region on 0 and labels it "(default)" - that only works if the
    // parser does not paint the default numbers into the file's block.
    match parse (Some """{ "Fonts": { "StandingsFont": 30 } }""") with
    | Ok t ->
        Assert.Equal(30, t.LayoutOption.Fonts.StandingsFont)
        Assert.Equal(0, t.LayoutOption.Fonts.PairingsFont)
    | Error msg -> failwithf "expected a parse, got: %s" msg

[<Fact>]
let ``a field written explicitly wins over the default`` () =
    let lo = layout (Some """{ "ShowPVBoard": false, "AutoCycleTimeInSec": 7 }""")
    Assert.False(lo.ShowPVBoard)
    Assert.Equal(7, lo.AutoCycleTimeInSec)

[<Fact>]
let ``the two old crosstable flags still decide when the new field is absent`` () =
    let ct block = (layout (Some block)).CrosstableWithStandings
    Assert.Equal(LayoutTypes.CrosstableWithStandings.Below, ct """{ "ShowCrosstableBelowStandings": true }""")
    Assert.Equal(LayoutTypes.CrosstableWithStandings.Hidden, ct """{ "OnlyShowStandings": true, "ShowCrosstableBelowStandings": true }""")
    Assert.Equal(LayoutTypes.CrosstableWithStandings.Cycle, ct """{ "OnlyShowStandings": false, "ShowCrosstableBelowStandings": false }""")

[<Fact>]
let ``the new crosstable field wins over the old flags`` () =
    let lo = layout (Some """{ "OnlyShowStandings": true, "CrosstableWithStandings": "cycle" }""")
    Assert.Equal(LayoutTypes.CrosstableWithStandings.Cycle, lo.CrosstableWithStandings)

[<Fact>]
let ``an unknown crosstable word is a parse error, not a silent default`` () =
    match parse (Some """{ "CrosstableWithStandings": "sideways" }""") with
    | Error msg -> Assert.Contains("could not be parsed", msg)
    | Ok _ -> failwith "expected an error"
