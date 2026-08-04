module CoreTypesTests

open Xunit
open System
open System.Text.Json
open ChessLibrary.TimeControlTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TypesDef.Tournament

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

// Backward compat: a tournament.json with NO LiveFeed section must still deserialize.
[<Fact>]
let ``Tournament JSON without LiveFeed still deserializes (backward compat)`` () =
    let json = """{ "Name":"old config", "TournamentMode":"RR", "Rounds":5 }"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let t = JsonSerializer.Deserialize<Tournament>(json, opts)
    Assert.Equal("old config", t.Name)
    // missing LiveFeed => null; the runner treats null as "no feed" (normal tournament)
    Assert.True(obj.ReferenceEquals(box t.LiveFeed, null))

[<Fact>]
let ``Tournament JSON with LiveFeed deserializes the section`` () =
    let json = """{ "Name":"feed config", "TournamentMode":"RR",
                    "LiveFeed": { "Url":"http://localhost:5018/api/livefeed", "Source":"rig-A", "File":"", "Token":"" } }"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let t = JsonSerializer.Deserialize<Tournament>(json, opts)
    Assert.False(obj.ReferenceEquals(box t.LiveFeed, null))
    Assert.Equal("http://localhost:5018/api/livefeed", t.LiveFeed.Url)
    Assert.Equal("rig-A", t.LiveFeed.Source)

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
    Assert.Equal(ResultReason.NotStarted, metadata.Reason)

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
    let timeControl = { TimeConfigs = [{ Id = 1; Fixed = TimeSpan.Zero; Increment = TimeSpan.Zero; NodeLimit = false; Nodes = 0 }]; WmovesToGo = 0; BmovesToGo = 0 }
    let config = timeControl.GetTimeConfig 1
    Assert.Equal(1, config.Id)

[<Fact>]
let ``TimeControl ToString formats correctly`` () =
    let config1 = { Id = 1; Fixed = TimeSpan.FromSeconds(60.0); Increment = TimeSpan.FromSeconds(1.0); NodeLimit = false; Nodes = 0 }
    Assert.Equal("1' + 1''", config1.ToString())
    let config2 = { Id = 2; Fixed = TimeSpan.FromSeconds(90.0); Increment = TimeSpan.FromSeconds(10.0); NodeLimit = false; Nodes = 0 }
    Assert.Equal("1.5' + 10''", config2.ToString())
    let config3 = { Id = 2; Fixed = TimeSpan.FromMinutes(90.0); Increment = TimeSpan.FromSeconds(30.0); NodeLimit = false; Nodes = 0 }
    Assert.Equal("90' + 30''", config3.ToString())

[<Fact>]
let ``Tournament Empty cup options default to non-random openings`` () =
    let tourny = Tournament.Empty
    Assert.False(tourny.CupOptions.RandomOpenings)

// ============================================================================
// MoveAnnotation tests
// ============================================================================

let private roundTripMoveAnnotation (level: MoveAnnotation) =
    let opts = JsonSerializerOptions()
    opts.Converters.Add(MoveAnnotationConverter())
    let json = JsonSerializer.Serialize(level, opts)
    let result = JsonSerializer.Deserialize<MoveAnnotation>(json, opts)
    result

[<Fact>]
let ``MoveAnnotation JSON round-trip None`` () =
    Assert.Equal(MoveAnnotation.Off, roundTripMoveAnnotation MoveAnnotation.Off)

[<Fact>]
let ``MoveAnnotation JSON round-trip Minimal`` () =
    Assert.Equal(MoveAnnotation.Minimal, roundTripMoveAnnotation MoveAnnotation.Minimal)

[<Fact>]
let ``MoveAnnotation JSON round-trip Standard`` () =
    Assert.Equal(MoveAnnotation.Standard, roundTripMoveAnnotation MoveAnnotation.Standard)

[<Fact>]
let ``MoveAnnotation JSON round-trip Full`` () =
    Assert.Equal(MoveAnnotation.Full, roundTripMoveAnnotation MoveAnnotation.Full)

[<Fact>]
let ``MoveAnnotation backward compat true parses as Full`` () =
    let opts = JsonSerializerOptions()
    opts.Converters.Add(MoveAnnotationConverter())
    let result = JsonSerializer.Deserialize<MoveAnnotation>("true", opts)
    Assert.Equal(MoveAnnotation.Full, result)

[<Fact>]
let ``MoveAnnotation backward compat false parses as Standard`` () =
    let opts = JsonSerializerOptions()
    opts.Converters.Add(MoveAnnotationConverter())
    let result = JsonSerializer.Deserialize<MoveAnnotation>("false", opts)
    Assert.Equal(MoveAnnotation.Standard, result)

[<Fact>]
let ``Tournament with MoveAnnotation string deserializes correctly`` () =
    let json = """{"MoveAnnotation": "Minimal"}"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let tourny = JsonSerializer.Deserialize<Tournament>(json, opts)
    Assert.Equal(MoveAnnotation.Minimal, tourny.MoveAnnotation)

[<Fact>]
let ``Tournament with old VerboseMoveAnnotation field name is ignored gracefully`` () =
    // Old field name doesn't match new field name, so the value is not set.
    // Users must rename VerboseMoveAnnotation -> MoveAnnotation in their JSON.
    let json = """{"VerboseMoveAnnotation": true, "MoveAnnotation": "Full"}"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let tourny = JsonSerializer.Deserialize<Tournament>(json, opts)
    // The new MoveAnnotation field is read; old VerboseMoveAnnotation is silently ignored
    Assert.Equal(MoveAnnotation.Full, tourny.MoveAnnotation)

[<Fact>]
let ``Tournament with MoveAnnotation bool true deserializes as Full`` () =
    let json = """{"MoveAnnotation": true}"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let tourny = JsonSerializer.Deserialize<Tournament>(json, opts)
    Assert.Equal(MoveAnnotation.Full, tourny.MoveAnnotation)

[<Fact>]
let ``Tournament with MoveAnnotation bool false deserializes as Standard`` () =
    let json = """{"MoveAnnotation": false}"""
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let tourny = JsonSerializer.Deserialize<Tournament>(json, opts)
    Assert.Equal(MoveAnnotation.Standard, tourny.MoveAnnotation)

[<Fact>]
let ``Tournament Empty defaults to Standard MoveAnnotation`` () =
    let tourny = Tournament.Empty
    Assert.Equal(MoveAnnotation.Standard, tourny.MoveAnnotation)

// ============================================================================
// ChessMoveInfo annotation output tests
// ============================================================================

let private testMoveInfo =
    { ChessMoveInfo.Empty with wv = CP 1.23; d = 15; sd = 12; mt = 500L; tl = 59500L;
                                s = 1000000L; eps = 50000L; n = 500000L; pcs = 28uy;
                                pd = ""; pv = "e2e4"; n1 = 100L; n2 = 50L;
                                q1 = 0.55; q2 = 0.45; p1 = 30.0; pt = 25.0; tb = 0L }

[<Fact>]
let ``MinimalAnnotation includes only wv n s mt`` () =
    let result = testMoveInfo.MinimalAnnotation
    Assert.Contains("wv=", result)
    Assert.Contains("n=500000", result)
    Assert.Contains("s=1000000", result)
    Assert.Contains("mt=500", result)
    Assert.DoesNotContain("d=", result)
    Assert.DoesNotContain("pv=", result)
    Assert.DoesNotContain("eps=", result)

[<Fact>]
let ``StandardAnnotation includes 11 fields`` () =
    let result = testMoveInfo.StandardAnnotation
    Assert.Contains("wv=", result)
    Assert.Contains("mt=500", result)
    Assert.Contains("s=1000000", result)
    Assert.Contains("eps=50000", result)
    Assert.Contains("n=500000", result)
    Assert.Contains("d=15", result)
    Assert.Contains("sd=12", result)
    Assert.Contains("tl=59500", result)
    Assert.Contains("tb=0", result)
    Assert.Contains("pv=e2e4", result)
    Assert.DoesNotContain("n1=", result)
    Assert.DoesNotContain("q1=", result)
    Assert.DoesNotContain("pcs=", result)

[<Fact>]
let ``FullAnnotation includes all fields`` () =
    let result = testMoveInfo.FullAnnotation
    Assert.Contains("wv=", result)
    Assert.Contains("pv=", result)
    Assert.Contains("n1=", result)
    Assert.Contains("n2=", result)
    Assert.Contains("q1=", result)
    Assert.Contains("q2=", result)
    Assert.Contains("p1=", result)
    Assert.Contains("pt=", result)

// ============================================================================
// Duration parsing — hh:mm:ss[.fff] with no ceiling on any field
// ============================================================================

[<Theory>]
// The three ceilings users kept hitting, none of which apply to a duration.
[<InlineData("00:00:60", 60_000.0)>]
[<InlineData("00:60:00", 3_600_000.0)>]
[<InlineData("30:00:00", 108_000_000.0)>]
// Ordinary values must be unchanged.
[<InlineData("00:03:00.000", 180_000.0)>]
[<InlineData("00:00:20.000", 20_000.0)>]
[<InlineData("00:00:00.100", 100.0)>]
[<InlineData("00:00:00", 0.0)>]
// Overflow in more than one field at once, and a fraction alongside it.
[<InlineData("00:00:90.500", 90_500.0)>]
[<InlineData("01:90:90", 9_090_000.0)>]
let ``parseDuration reads every field without an upper bound`` (text: string) (expectedMs: float) =
    Assert.Equal(expectedMs, (parseDuration text).TotalMilliseconds, 3)

[<Theory>]
// Two fields are ambiguous: .NET reads "10:00" as ten hours, a user means ten minutes.
[<InlineData("10:00")>]
[<InlineData("90")>]
// Days are not part of the format, so TimeSpan's own long form is refused rather than
// silently accepted with a different meaning for the dot.
[<InlineData("1.06:00:00")>]
[<InlineData("")>]
[<InlineData("abc")>]
[<InlineData("00:0a:00")>]
[<InlineData("-00:01:00")>]
let ``parseDuration refuses input it would have to guess at`` (text: string) =
    Assert.ThrowsAny<exn>(fun () -> parseDuration text |> ignore)

[<Theory>]
// Normalised on the way out: the file states what the value became.
[<InlineData("00:00:90", "00:01:30.000")>]
[<InlineData("00:60:00", "01:00:00.000")>]
// Hours carry everything above a day rather than growing a day field.
[<InlineData("30:00:00", "30:00:00.000")>]
[<InlineData("00:00:00.100", "00:00:00.100")>]
[<InlineData("00:03:00.000", "00:03:00.000")>]
let ``formatDuration writes the canonical shape`` (input: string) (expected: string) =
    Assert.Equal(expected, formatDuration (parseDuration input))

[<Fact>]
let ``formatDuration round-trips through parseDuration`` () =
    for text in [ "00:00:00"; "00:00:00.001"; "00:03:00.000"; "30:00:00"; "00:00:90.500" ] do
        let once = parseDuration text
        Assert.Equal(once, parseDuration (formatDuration once))

[<Fact>]
let ``DurationConverter reads existing config values and normalises on write`` () =
    let options = JsonSerializerOptions()
    options.Converters.Add(DurationConverter())
    // Exactly the strings in the shipped tournament template.
    let json = """{"DelayBetweenGames":"00:00:20.000","MoveOverhead":"00:00:00.100"}"""
    let doc = JsonSerializer.Deserialize<Map<string, TimeSpan>>(json, options)
    Assert.Equal(20_000.0, doc.["DelayBetweenGames"].TotalMilliseconds, 3)
    Assert.Equal(100.0, doc.["MoveOverhead"].TotalMilliseconds, 3)
    let back = JsonSerializer.Serialize(doc, options)
    Assert.Contains("00:00:20.000", back)
    Assert.Contains("00:00:00.100", back)

[<Fact>]
let ``a duration past a day survives the JSON round trip`` () =
    let options = JsonSerializerOptions()
    options.Converters.Add(DurationConverter())
    let json = """{"Fixed":"30:00:00"}"""
    let doc = JsonSerializer.Deserialize<Map<string, TimeSpan>>(json, options)
    Assert.Equal(30.0, doc.["Fixed"].TotalHours, 6)
    Assert.Contains("30:00:00.000", JsonSerializer.Serialize(doc, options))
