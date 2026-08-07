module EngineProtocolTests

open System.Collections.Generic
open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.TypesDef.CoreTypes
module EP = ChessLibrary.EngineProtocol

[<Fact>]
let ``getEssentialDataWithEPS parses cp wdl multipv and eps`` () =
    let line =
        "info depth 22 seldepth 40 multipv 2 score cp -34 nodes 12345 nps 54321 eps 222 tbhits 7 wdl 160 385 454 pv e2e4 e7e5"

    let parsed = EP.Regex.getEssentialDataWithEPS line true
    Assert.True(parsed.IsSome)

    let (depth, eval, nodes, nps, eps, pv, tbhits, wdl, sd, mpv) = parsed.Value
    Assert.Equal(22, depth)
    match eval with
    | CP v -> Assert.Equal(-0.34, v, 3)
    | _ -> failwith "Expected CP evaluation"
    Assert.Equal(12345L, nodes)
    Assert.Equal(54321L, nps)
    Assert.Equal(222L, eps)
    Assert.Equal("e2e4 e7e5", pv)
    Assert.Equal(7L, tbhits)
    Assert.Equal(40, sd)
    Assert.Equal(2, mpv)
    Assert.True(wdl.IsSome)
    Assert.Equal(160.0, wdl.Value.Win, 6)
    Assert.Equal(385.0, wdl.Value.Draw, 6)
    Assert.Equal(454.0, wdl.Value.Loss, 6)

[<Fact>]
let ``getEssentialData parses mate score with side-to-move perspective`` () =
    let line = "info depth 10 score mate 3 nodes 1 nps 1 pv e2e4"

    let parsedWhite = EP.Regex.getEssentialData line true
    let parsedBlack = EP.Regex.getEssentialData line false

    Assert.True(parsedWhite.IsSome)
    Assert.True(parsedBlack.IsSome)

    let (_, evalWhite, _, _, _, _, _, _, _) = parsedWhite.Value
    let (_, evalBlack, _, _, _, _, _, _, _) = parsedBlack.Value

    match evalWhite with
    | Mate m -> Assert.Equal(3, m)
    | _ -> failwith "Expected Mate for white"

    match evalBlack with
    | Mate m -> Assert.Equal(-3, m)
    | _ -> failwith "Expected Mate for black"

[<Fact>]
let ``getEssentialData returns None for non-info or missing score`` () =
    let notInfo = "bestmove e2e4"
    let infoWithoutScore = "info depth 14 nodes 1234 nps 5000 pv e2e4"

    Assert.True((EP.Regex.getEssentialData notInfo true).IsNone)
    Assert.True((EP.Regex.getEssentialData infoWithoutScore true).IsNone)

[<Fact>]
let ``uciToCommand maps known options and preserves BackendOptions quoting`` () =
    let threads = EP.UciOptions.uciToCommand "Threads" "8"
    let verbose = EP.UciOptions.uciToCommand "VerboseMoveStats" "TRUE"
    let backendOpts = EP.UciOptions.uciToCommand "BackendOptions" "gpu=1 policy=head"
    let unknown = EP.UciOptions.uciToCommand "DoesNotExist" "x"

    Assert.Equal(Some "--threads=8", threads)
    Assert.Equal(Some "--verbose-move-stats=true", verbose)
    Assert.Equal(Some "--backend-opts=\"gpu=1 policy=head\"", backendOpts)
    Assert.True(unknown.IsNone)

[<Fact>]
let ``createCommandsFromConfig builds CLI flags from config options`` () =
    let options = Dictionary<string, obj>()
    options["Threads"] <- box 16
    options["VerboseMoveStats"] <- box true
    options["BackendOptions"] <- box "gpu=2 policy=head"
    options["UnknownX"] <- box "ignored"

    let cfg = { EngineConfig.Empty with Options = options }
    let cmd = EP.UciOptions.createCommandsFromConfig cfg

    Assert.Contains("--threads=16", cmd)
    Assert.Contains("--verbose-move-stats=true", cmd)
    Assert.Contains("--backend-opts=\"gpu=2 policy=head\"", cmd)
    Assert.DoesNotContain("UnknownX", cmd)

[<Fact>]
let ``extractOptionDefaults and createDefaultSetOptionCommandForName resolve defaults`` () =
    let lines = ResizeArray<string>()
    lines.Add("id name Fake")
    lines.Add("option name Hash type spin default 16 min 1 max 1024")
    lines.Add("option name Ponder type check default false")
    lines.Add("option name Threads type spin min 1 max 1024")

    let defaults = EP.UCI.extractOptionDefaults lines
    Assert.Equal(2, defaults.Count)
    Assert.Equal("16", defaults["Hash"])
    Assert.Equal("false", defaults["Ponder"])

    let cmdHash = EP.UCI.createDefaultSetOptionCommandForName defaults "hash"
    let cmdPonder = EP.UCI.createDefaultSetOptionCommandForName defaults "PONDER"
    let cmdMissing = EP.UCI.createDefaultSetOptionCommandForName defaults "threads"

    Assert.Equal(Some "setoption name Hash value 16", cmdHash)
    Assert.Equal(Some "setoption name Ponder value false", cmdPonder)
    Assert.True(cmdMissing.IsNone)

module UciOpt = ChessLibrary.UciOption

[<Fact>]
let ``combo option with single-word values parses each var`` () =
    let line = "option name Analysis Contempt type combo default Both var Off var White var Black var Both"
    match UciOpt.parseOption line with
    | Some opt ->
        Assert.Equal("Analysis Contempt", opt.Name)
        match opt.OptionType with
        | UciOpt.Combo (values, def) ->
            Assert.Equal<string list>([ "Off"; "White"; "Black"; "Both" ], values)
            Assert.Equal("Both", def)
        | _ -> failwith "Expected combo option"
    | None -> failwith "Option did not parse"

[<Fact>]
let ``combo option with multi-word values parses each var run`` () =
    let line = "option name Style type combo default Very Solid var Very Solid var Risky var Normal"
    match UciOpt.parseOption line with
    | Some opt ->
        match opt.OptionType with
        | UciOpt.Combo (values, def) ->
            // The old parse produced ["Very";"Solid";"var";"Risky";"var";"Normal"] with default "Very"
            Assert.Equal<string list>([ "Very Solid"; "Risky"; "Normal" ], values)
            Assert.Equal("Very Solid", def)
        | _ -> failwith "Expected combo option"
    | None -> failwith "Option did not parse"
