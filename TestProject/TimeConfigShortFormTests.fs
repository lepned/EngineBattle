/// tournament.json's short forms for a time setting ("Tc", "St", a bare "Nodes"), written the
/// way match takes them. They are expanded to the long fields before the file is read, so these
/// tests read a file the way the GUI and the console do and look at the TimeConfig that results.
module TimeConfigShortFormTests

open System
open System.IO
open Xunit
open ChessLibrary

let private minimal = """"Name": "t", "TournamentMode": "RR", "EngineSetup": { "EngineDefFolder": "", "EngineDefList": [] }"""

let private read (timeConfigs: string) (extra: string) =
    let body = sprintf "{ %s, \"TimeControl\": { \"TimeConfigs\": [ %s ]%s } }" minimal timeConfigs extra
    let path = Path.GetTempFileName()
    File.WriteAllText(path, body)
    try Configuration.JSON.tryReadTournamentJson path
    finally File.Delete path

let private configs timeConfigs =
    match read timeConfigs "" with
    | Ok t -> t.TimeControl
    | Error msg -> failwithf "expected a parse, got: %s" msg

let private error timeConfigs extra =
    match read timeConfigs extra with
    | Error msg -> msg
    | Ok _ -> failwith "expected an error"

[<Fact>]
let ``Tc is base+increment in seconds, as match takes it`` () =
    let tc = configs """{ "Id": 1, "Tc": "60+1" }, { "Id": 2, "Tc": "1:30+0.5" }, { "Id": 3, "Tc": "90:00+30" }, { "Id": 4, "Tc": "60s+1" }"""
    let c1, c2 = tc.GetTimeConfig 1, tc.GetTimeConfig 2
    Assert.Equal((TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 1.0, false), (c1.Fixed, c1.Increment, c1.NodeLimit))
    Assert.Equal((TimeSpan.FromSeconds 90.0, TimeSpan.FromSeconds 0.5), (c2.Fixed, c2.Increment))
    Assert.Equal((TimeSpan.FromMinutes 90.0, TimeSpan.FromSeconds 30.0), ((tc.GetTimeConfig 3).Fixed, (tc.GetTimeConfig 3).Increment))
    Assert.Equal((TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 1.0), ((tc.GetTimeConfig 4).Fixed, (tc.GetTimeConfig 4).Increment))
    Assert.Equal((0, 0), (tc.WmovesToGo, tc.BmovesToGo))

[<Fact>]
let ``moves per period belong to the setting, and settings may differ`` () =
    let tc = configs """{ "Id": 1, "Tc": "40/300+2" }, { "Id": 2, "Tc": "60/600" }, { "Id": 3, "Tc": "5:00+3" }"""
    let c1, c2, c3 = tc.GetTimeConfig 1, tc.GetTimeConfig 2, tc.GetTimeConfig 3
    Assert.Equal((TimeSpan.FromSeconds 300.0, TimeSpan.FromSeconds 2.0, 40), (c1.Fixed, c1.Increment, c1.MovesToGo))
    Assert.Equal((TimeSpan.FromSeconds 600.0, 60), (c2.Fixed, c2.MovesToGo))
    Assert.Equal((TimeSpan.FromMinutes 5.0, TimeSpan.FromSeconds 3.0, 0), (c3.Fixed, c3.Increment, c3.MovesToGo))
    Assert.Equal((0, 0), (tc.WmovesToGo, tc.BmovesToGo))
    Assert.Equal("40/5' + 2''", c1.ToString())

[<Fact>]
let ``St is seconds per move, a number or a text`` () =
    let tc = configs """{ "Id": 1, "St": 1 }, { "Id": 2, "St": "0.25s" }"""
    let c1, c2 = tc.GetTimeConfig 1, tc.GetTimeConfig 2
    Assert.True(c1.IsMoveTime)
    Assert.Equal(TimeSpan.FromSeconds 1.0, c1.MoveTime)
    Assert.Equal(TimeSpan.FromMilliseconds 250.0, c2.MoveTime)
    Assert.Equal("1'' / move", c1.ToString())

[<Fact>]
let ``Nodes alone is a node limit, but not next to a time`` () =
    let tc = configs """{ "Id": 1, "Nodes": 150000 }, { "Id": 2, "Fixed": "00:01:00", "Increment": "00:00:01", "Nodes": 1000 }"""
    let c1, c2 = tc.GetTimeConfig 1, tc.GetTimeConfig 2
    Assert.True(c1.NodeLimit)
    Assert.Equal(150000, c1.Nodes)
    Assert.False(c2.NodeLimit)

[<Fact>]
let ``the long form reads as before`` () =
    let tc = configs """{ "Id": 1, "Fixed": "00:00:60.000", "Increment": "00:00:01.000", "NodeLimit": false, "Nodes": 1000 }, { "Id": 2, "Fixed": "00:02:05", "Increment": "00:00:02", "NodeLimit": true, "Nodes": 150000 }"""
    let c1, c2 = tc.GetTimeConfig 1, tc.GetTimeConfig 2
    Assert.Equal((TimeSpan.FromSeconds 60.0, TimeSpan.FromSeconds 1.0, false, false), (c1.Fixed, c1.Increment, c1.NodeLimit, c1.IsMoveTime))
    Assert.True(c2.NodeLimit)

[<Fact>]
let ``what is wrong is said, not guessed`` () =
    Assert.Contains("give the time one way, not Tc and Fixed", error """{ "Id": 1, "Tc": "60+1", "Fixed": "00:01:00" }""" "")
    Assert.Contains("Tc \"abc\"", error """{ "Id": 1, "Tc": "abc" }""" "")
    Assert.Contains("gives no time", error """{ "Id": 1, "Tc": "inf" }""" "")
    Assert.Contains("St is the seconds per move", error """{ "Id": 1, "St": 0 }""" "")
    Assert.Contains("give the time one way, not Tc and MovesToGo", error """{ "Id": 1, "Tc": "40/300", "MovesToGo": 40 }""" "")
