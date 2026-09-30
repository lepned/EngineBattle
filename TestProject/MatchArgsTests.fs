/// The reference's command line (ChessLibrary/Match/MatchArgs.fs) held to the reference's own tests:
/// app/tests/cli_test.cpp and the CLI snapshots in app/tests/snapshots/cli at 60d7a7a, same
/// arguments, same expected settings and error texts. The machine is faked (16 threads, POSIX,
/// every engine path a file, a fixed clock), so the results do not depend on where they run.
module MatchArgsTests

open System
open Xunit
open ChessLibrary.Match
open ChessLibrary.Match.MatchArgs

let private books = set [ "./app/tests/data/test.epd"; "app/tests/data/test.epd" ]

let private env =
    { defaultEnv () with
        HardwareThreads = 16
        IsWindows = false
        PathExists = books.Contains
        IsFile = fun _ -> true
        Now = fun () -> DateTime(2026, 9, 29, 13, 5, 9)
        RandomSeed = fun () -> 42UL
        Version = "EngineBattle 1.8.2 (abc1234)" }

let private dummy = "cmd=app/tests/mock/engine/dummy_engine"

let private baseArgs extras =
    [ "-engine"; dummy; "name=Alpha"; "tc=10/1+0"; "-engine"; dummy; "name=Beta"; "tc=10/1+0" ] @ extras

let private ok args =
    match parse env args with
    | Run p -> p
    | Failed(_, e) -> failwithf "expected a run, got the error: %s" e
    | Exit(_, t) -> failwithf "expected a run, got an exit: %s" t

let private error args =
    match parse env args with
    | Failed(_, e) -> e
    | other -> failwithf "expected an error, got %A" other

let private throwsWith (expected: string) args = Assert.Equal(expected, error args)

let private tcOf (tc: string) =
    (ok [ "-engine"; dummy; "name=Alpha"; "tc=" + tc; "-engine"; dummy; "name=Beta"; "tc=10/1+0" ]).Engines.Head.Tc

// ---- cli_test.cpp: errors ----

[<Fact>]
let ``tc and st not usable together`` () =
    throwsWith "Error; cannot use tc and st together!"
        [ "-engine"; "dir=./"; dummy; "tc=10/9.64"; "st=5"; "name=Alexandria-EA649FED"
          "-engine"; "dir=./"; dummy; "tc=40/1:9.65+0.1" ]

[<Fact>]
let ``no time control specified`` () =
    throwsWith "Error; no TimeControl specified!"
        [ "-engine"; "dir=./"; dummy; "name=Alexandria-EA649FED"; "-engine"; "dir=./"; dummy; "name=Alexandria-27E42728" ]

[<Fact>]
let ``too much concurrency`` () =
    throwsWith "Error: Concurrency exceeds number of CPUs. Use -force-concurrency to override." [ "-concurrency"; "20000" ]

[<Fact>]
let ``too many games`` () =
    throwsWith "Error: Exceeded -game limit! Must be less than 2" [ "-games"; "3"; "-rounds"; "25000" ]

[<Theory>]
[<InlineData("0.05", "0.05", "5", "-1.5", "bayesian", "Error; SPRT: elo0 must be less than elo1!")>]
[<InlineData("0.55", "0.55", "4", "5", "bayesian", "Error; SPRT: sum of alpha and beta must be less than 1!")>]
[<InlineData("0.05", "0.05", "4", "5", "dsadsa", "Error; SPRT: invalid SPRT model!")>]
[<InlineData("1.05", "0.05", "4", "5", "logistic", "Error; SPRT: alpha must be a decimal number between 0 and 1!")>]
[<InlineData("0.05", "1.05", "4", "5", "logistic", "Error; SPRT: beta must be a decimal number between 0 and 1!")>]
let ``invalid sprt configs`` (alpha: string, beta: string, elo0: string, elo1: string, model: string, expected: string) =
    throwsWith expected [ "-sprt"; "alpha=" + alpha; "beta=" + beta; "elo0=" + elo0; "elo1=" + elo1; "model=" + model ]

[<Fact>]
let ``no chess960 opening book`` () =
    throwsWith "Error: Please specify a Chess960 opening book" [ "-variant"; "fischerandom" ]

[<Fact>]
let ``not enough engines`` () =
    throwsWith "Error: Need at least two engines to start!" [ "-engine"; "dir=./"; dummy; "depth=5" ]

[<Fact>]
let ``zero time control counts as none`` () =
    throwsWith "Error; no TimeControl specified!"
        [ "-engine"; "dir=./"; dummy; "tc=10/0+0"; "name=Alexandria-EA649FED"; "-engine"; "dir=./"; dummy; "tc=10/0+0" ]

[<Theory>]
[<InlineData("40/")>]
[<InlineData("10+")>]
[<InlineData("1:")>]
[<InlineData("1:2:3")>]
[<InlineData("10/1junk")>]
[<InlineData("10/1ss")>]
[<InlineData("10/-1")>]
[<InlineData("0/1")>]
let ``malformed time controls throw`` (tc: string) =
    error [ "-engine"; dummy; "name=Alpha"; "tc=" + tc; "-engine"; dummy; "name=Beta"; "tc=10/1+0" ] |> ignore

[<Fact>]
let ``time control texts`` () =
    let reason tc =
        let e = error [ "-engine"; dummy; "name=Alpha"; "tc=" + tc; "-engine"; dummy; "name=Beta"; "tc=10/1+0" ]
        e.Substring(e.IndexOf "Reason: " + 8)
    Assert.Equal("Invalid time control: \"40/\"", reason "40/")
    Assert.Equal("Invalid time control: \"1:2:3\"", reason "1:2:3")
    Assert.Equal("Invalid numeric value: \"1junk\"", reason "10/1junk")
    Assert.Equal("Invalid time control duration: \"-1\"", reason "10/-1")
    Assert.Equal("Time control move count must be positive", reason "0/1")
    Assert.Equal("Hourglass time control not supported.", reason "hg10")

[<Fact>]
let ``time controls accept a seconds suffix and round to milliseconds`` () =
    Assert.Equal(20L, (tcOf "0.02s").Time)
    let tc = tcOf "0.2+0.002s"
    Assert.Equal((200L, 2L), (tc.Time, tc.Increment))
    let tc = tcOf "60+0.6"
    Assert.Equal((60000L, 600L), (tc.Time, tc.Increment))
    let tc = tcOf "2+0.02s"
    Assert.Equal((2000L, 20L), (tc.Time, tc.Increment))
    let tc = tcOf "40/1:9.65+0.1"
    Assert.Equal((40L, 69650L, 100L), (tc.Moves, tc.Time, tc.Increment))
    // a half millisecond rounds away from zero (std::round); decimal parsing makes 0.0015 an exact tie
    let tc = tcOf "1+0.0015"
    Assert.Equal(2L, tc.Increment)
    let tc = tcOf "inf/10"
    Assert.Equal((0L, 10000L), (tc.Moves, tc.Time))

[<Fact>]
let ``numeric options reject trailing characters and invalid ranges`` () =
    error (baseArgs [ "-rounds"; "10junk" ]) |> ignore
    error (baseArgs [ "-srand"; "-1" ]) |> ignore
    error (baseArgs [ "-rounds"; "999999999999999999999" ]) |> ignore
    error (baseArgs [ "-sprt"; "alpha=nan"; "beta=0.05"; "elo0=0"; "elo1=1" ]) |> ignore
    Assert.Equal(
        "Error while reading option \"-rounds\" with value \"10junk\"\nReason: Invalid numeric value: \"10junk\"",
        error (baseArgs [ "-rounds"; "10junk" ]))

[<Fact>]
let ``engine with invalid restart names the last token of its group`` () =
    throwsWith
        "Error while reading option \"-engine\" with value \"name=Alexandria-EA649FED\"\nReason: Invalid parameter (must be either \"on\" or \"off\"): true"
        [ "-engine"; "dir=./"; dummy; "tc=10/1+0"; "restart=true"; "name=Alexandria-EA649FED"
          "-engine"; "dir=./"; dummy; "name=Alexandria-27E42728"; "tc=10/1+0" ]

[<Fact>]
let ``empty TB paths`` () =
    throwsWith "Error while reading option \"-tb\" with value \"-tb\"\nReason: Option \"-tb\" expects exactly one value." [ "-tb" ]

// ---- cli_test.cpp: settings ----

[<Fact>]
let ``general config parsing`` () =
    let p =
        ok [ "-engine"; "dir=./"; dummy; "depth=5"; "st=5"; "nodes=5000"; "option.Threads=1"; "option.Hash=16"; "name=Alexandria-EA649FED"
             "-engine"; "dir=./"; dummy; "tc=40/1:9.65+0.1"; "timemargin=243"; "plies=7"; "option.Threads=1"; "option.Hash=32"; "name=Alexandria-27E42728"
             "-openings"; "file=./app/tests/data/test.epd"; "format=epd"; "order=random"; "plies=16"
             "-rounds"; "50"; "-games"; "2"; "-pgnout"; "file=PGNs/Alexandria-EA649FED_vs_Alexandria-27E42728"; "-use-affinity"; "0-1" ]
    Assert.Equal<int list>([ 0; 1 ], p.Tournament.AffinityCpus)
    match p.Engines with
    | [ e0; e1 ] ->
        Assert.Equal("Alexandria-EA649FED", e0.Name)
        Assert.Equal({ TcLimits.Zero with FixedTime = 5000L }, e0.Tc)
        Assert.Equal((5000L, 5L), (e0.Nodes, e0.Plies))
        Assert.Equal<(string * string) list>([ "Threads", "1"; "Hash", "16" ], e0.Options)
        Assert.Equal("Alexandria-27E42728", e1.Name)
        Assert.Equal({ TcLimits.Zero with Moves = 40L; Time = 69650L; Increment = 100L; TimeMargin = 243L }, e1.Tc)
        Assert.Equal((0L, 7L), (e1.Nodes, e1.Plies))
        Assert.Equal<(string * string) list>([ "Threads", "1"; "Hash", "32" ], e1.Options)
    | es -> failwithf "expected two engines, got %d" es.Length

[<Fact>]
let ``general config parsing 2`` () =
    let t =
        (ok [ "-engine"; "dir=./"; dummy; "name=Alexandria-EA649FED"; "tc=10/9.64"
              "-engine"; "dir=./"; dummy; "name=Alexandria-27E42728"; "tc=10/9.64"
              "-recover"; "-concurrency"; "1"; "-ratinginterval"; "2"; "-scoreinterval"; "3"; "-autosaveinterval"; "4"
              "-rounds"; "256"; "-draw"; "movenumber=40"; "movecount=3"; "score=15"
              "-resign"; "movecount=5"; "score=600"; "twosided=true"; "-maxmoves"; "150"; "-games"; "1"
              "-sprt"; "alpha=0.05"; "beta=0.05"; "elo0=-1.5"; "elo1=5"; "model=bayesian"
              "-openings"; "file=./app/tests/data/test.epd"; "format=epd"; "order=sequential"; "plies=16"; "start=4"
              "-variant"; "fischerandom"; "-output"; "format=cutechess"; "-srand"; "1234"; "-report"; "penta=false"
              "-use-affinity"; "-srand"; "1234"; "-epdout"; "file=EPDs/Alexandria-EA649FED_vs_Alexandria-27E42728"
              "-pgnout"; "file=PGNs/Alexandria-EA649FED_vs_Alexandria-27E42728"; "nodes=true"; "nps=true"; "seldepth=true"
              "hashfull=true"; "tbhits=true"; "min=true" ]).Tournament
    Assert.True t.Recover
    Assert.Equal((1, 2, 3, 4), (t.Concurrency, t.RatingInterval, t.ScoreInterval, t.AutoSaveInterval))
    Assert.Equal((1, 256), (t.Games, t.Rounds))
    Assert.Equal(Frc, t.Variant)
    Assert.Equal({ MoveNumber = 40; MoveCount = 3; Score = 15; Enabled = true }, t.Draw)
    Assert.Equal({ MoveCount = 5; Score = 600; TwoSided = true; Enabled = true }, t.Resign)
    Assert.Equal({ MoveCount = 150; Enabled = true }, t.MaxMoves)
    Assert.Equal(Cutechess, t.Output)
    Assert.True t.Affinity
    Assert.Equal(1234UL, t.Seed)
    Assert.False t.ReportPenta
    Assert.Equal({ Enabled = true; Alpha = 0.05; Beta = 0.05; Elo0 = -1.5; Elo1 = 5.0; Model = "bayesian" }, t.Sprt)
    Assert.Equal("PGNs/Alexandria-EA649FED_vs_Alexandria-27E42728", t.Pgn.File)
    Assert.True(t.Pgn.TrackNodes && t.Pgn.TrackSeldepth && t.Pgn.TrackNps && t.Pgn.TrackHashfull && t.Pgn.TrackTbhits && t.Pgn.Min)
    Assert.Equal("EPDs/Alexandria-EA649FED_vs_Alexandria-27E42728", t.Epd.File)
    Assert.Equal({ File = "./app/tests/data/test.epd"; Format = Epd; Order = Sequential; Plies = 16; Start = 4 }, t.Opening)

[<Fact>]
let ``each propagates to every engine`` () =
    let p = ok (baseArgs [ "-each"; "option.Hash=128" ])
    for e in p.Engines do Assert.Contains(("Hash", "128"), e.Options)

[<Fact>]
let ``each applies after parsing, whatever its position`` () =
    let p = ok ([ "-each"; "tc=5+0.05"; "option.Hash=64" ] @ baseArgs [ "-engine"; dummy; "name=Gamma"; "tc=1+0"; "option.Hash=8" ])
    for e in p.Engines do Assert.Equal((5000L, 50L), (e.Tc.Time, e.Tc.Increment))
    Assert.Equal<(string * string) list>([ "Hash", "8"; "Hash", "64" ], (List.last p.Engines).Options)

[<Fact>]
let ``each errors show all its parameters`` () =
    throwsWith "Error while reading option \"-each\" with value \"tc=1+0 bogus=1\"\nReason: Unrecognized engine option \"bogus\" with value \"1\"."
        (baseArgs [ "-each"; "tc=1+0"; "-each"; "bogus=1" ])

[<Fact>]
let ``default file names for pgnout and epdout`` () =
    let t =
        (ok [ "-engine"; "dir=./"; dummy; "depth=5"; "st=5"; "name=Alexandria-EA649FED"
              "-engine"; "dir=./"; dummy; "tc=40/1:9.65+0.1"; "name=Alexandria-27E42728"
              "-openings"; "file=./app/tests/data/test.epd"; "-rounds"; "50"; "-games"; "2"; "-pgnout"; "-epdout" ]).Tournament
    Assert.Equal("match_20260929_130509.pgn", t.Pgn.File)
    Assert.Equal("match_20260929_130509.epd", t.Epd.File)

[<Fact>]
let ``scalar options update the tournament`` () =
    let t =
        (ok (baseArgs [ "-concurrency"; "2"; "-force-concurrency"; "-ratinginterval"; "5"; "-scoreinterval"; "4"
                        "-autosaveinterval"; "7"; "-srand"; "123"; "-seeds"; "3"; "-wait"; "250"; "-noswap"; "-reverse" ])).Tournament
    Assert.Equal((2, 5, 4, 7), (t.Concurrency, t.RatingInterval, t.ScoreInterval, t.AutoSaveInterval))
    Assert.Equal((123UL, 3, 250), (t.Seed, t.GauntletSeeds, t.Wait))
    Assert.True(t.ForceConcurrency && t.NoSwap && t.Reverse)

[<Fact>]
let ``key value options populate outputs and tablebases`` () =
    let t =
        (ok (baseArgs [ "-pgnout"; "file=games.pgn"; "nodes=true"; "pv=true"; "tbhits=true"; "timeleft=true"; "latency=true"
                        "match_line=.*"; "notation=lan"; "-output"; "format=cutechess"; "-tb"; "app/tests/data/syzygy_wdl3"
                        "-tbignore50"; "-crc32"; "pgn=true"; "-site"; "Test"; "Arena" ])).Tournament
    Assert.Equal(Cutechess, t.Output)
    Assert.Equal("games.pgn", t.Pgn.File)
    Assert.True(t.Pgn.TrackNodes && t.Pgn.TrackPv && t.Pgn.TrackTbhits && t.Pgn.TrackTimeleft && t.Pgn.TrackLatency && t.Pgn.Crc)
    Assert.Equal<string list>([ ".*" ], t.Pgn.AdditionalLinesRgx)
    Assert.Equal(Lan, t.Pgn.Notation)
    Assert.Equal("TestArena", t.Pgn.Site)
    Assert.Equal({ SyzygyDirs = "app/tests/data/syzygy_wdl3"; MaxPieces = 0; ResultType = Both; Ignore50MoveRule = true; Enabled = true }, t.TbAdjudication)

[<Theory>]
[<InlineData("DRAW")>]
[<InlineData("WIN_LOSS")>]
[<InlineData("BOTH")>]
let ``tablebase adjudication parameters`` (kind: string) =
    let t = (ok (baseArgs [ "-tb"; "app/tests/data/syzygy_wdl3"; "-tbpieces"; "6"; "-tbadjudicate"; kind ])).Tournament
    Assert.Equal(6, t.TbAdjudication.MaxPieces)
    Assert.Equal((match kind with "DRAW" -> DrawOnly | "WIN_LOSS" -> WinLoss | _ -> Both), t.TbAdjudication.ResultType)

[<Fact>]
let ``logging options`` () =
    let l = (ok (baseArgs [ "-log"; "file=fast.log"; "level=trace"; "append=false"; "compress=true"; "realtime=true"; "engine=true" ])).Tournament.Log
    Assert.Equal({ File = "fast.log"; Level = Trace; AppendFile = false; Compress = true; Realtime = true; EngineComs = true }, l)

[<Fact>]
let ``report and output toggles`` () =
    Assert.False (ok (baseArgs [ "-report"; "penta=false" ])).Tournament.ReportPenta
    Assert.True (ok (baseArgs [ "-report"; "penta=true" ])).Tournament.ReportPenta
    Assert.Equal(Default, (ok (baseArgs [ "-output"; "format=fastchess" ])).Tournament.Output)
    Assert.False (ok (baseArgs [ "-crc32"; "pgn=false" ])).Tournament.Pgn.Crc

[<Fact>]
let ``engine names are auto-assigned and suffixed`` () =
    let p =
        ok [ "-engine"; "dir=./"; dummy; "depth=5"; "-engine"; "dir=./"; dummy; "tc=40/1:9.65+0.1"
             "-engine"; "dir=./"; dummy; "tc=40/1:9.65+0.1" ]
    Assert.Equal<string list>([ "dummy_engine"; "dummy_engine_2"; "dummy_engine_3" ], p.Engines |> List.map (fun e -> e.Name))

[<Fact>]
let ``quick creates two engines and presets the tournament`` () =
    let p = ok [ "-quick"; dummy; dummy; "book=app/tests/data/test.epd" ]
    let t = p.Tournament
    Assert.Equal<string list>([ "app/tests/mock/engine/dummy_engine1"; "app/tests/mock/engine/dummy_engine2" ], p.Engines |> List.map (fun e -> e.Name))
    Assert.Equal((2, 25000, 14), (t.Games, t.Rounds, t.Concurrency))
    Assert.Equal({ File = "app/tests/data/test.epd"; Format = Epd; Order = Random; Plies = -1; Start = 1 }, t.Opening)
    Assert.True t.Recover
    Assert.Equal({ MoveNumber = 30; MoveCount = 8; Score = 8; Enabled = true }, t.Draw)
    Assert.Equal(Cutechess, t.Output)
    Assert.False t.ReportPenta
    for e in p.Engines do Assert.Equal((10000L, 100L), (e.Tc.Time, e.Tc.Increment))

[<Fact>]
let ``quick errors`` () =
    Assert.EndsWith("Reason: Option \"-quick\" requires exactly two cmd entries.", error [ "-quick"; dummy; "book=a.epd" ])
    Assert.EndsWith("Reason: Option \"-quick\" requires a book=FILE entry.", error [ "-quick"; dummy; dummy ])
    Assert.EndsWith("Reason: Please include the .pgn or .epd file extension for the opening book.", error [ "-quick"; dummy; dummy; "book=a.txt" ])

[<Fact>]
let ``repeat resets games to a pair`` () =
    Assert.Equal(2, (ok (baseArgs [ "-games"; "4"; "-repeat" ])).Tournament.Games)
    // -games 4 alone is swapped into rounds
    let t = (ok (baseArgs [ "-games"; "4" ])).Tournament
    Assert.Equal((2, 4), (t.Games, t.Rounds))

[<Fact>]
let ``wait and event`` () =
    let t = (ok (baseArgs [ "-wait"; "150"; "-event"; "My"; "Event" ])).Tournament
    Assert.Equal(150, t.Wait)
    Assert.Equal("MyEvent", t.Pgn.EventName)

[<Fact>]
let ``gauntlet with seeds`` () =
    let t = (ok (baseArgs [ "-tournament"; "gauntlet"; "-seeds"; "2" ])).Tournament
    Assert.Equal((Gauntlet, 2), (t.Type, t.GauntletSeeds))

[<Fact>]
let ``show latency and test environment`` () =
    let t = (ok (baseArgs [ "-show-latency"; "-testEnv" ])).Tournament
    Assert.True(t.ShowLatency && t.TestEnv)

[<Fact>]
let ``debug option reports a helpful error`` () =
    throwsWith
        "Error while reading option \"-debug\" with value \"-debug\"\nReason: The 'debug' option does not exist. Use the 'log' option instead to write all engine input and output into a text file."
        (baseArgs [ "-debug" ])

[<Fact>]
let ``sprt models, missing parameters and proto`` () =
    Assert.Equal("logistic", (ok (baseArgs [ "-sprt"; "alpha=0.05"; "beta=0.05"; "elo0=-1"; "elo1=2"; "model=logistic" ])).Tournament.Sprt.Model)
    Assert.Equal("normalized", (ok (baseArgs [ "-sprt"; "alpha=0.05"; "beta=0.05"; "elo0=-1"; "elo1=2"; "model=normalized" ])).Tournament.Sprt.Model)
    error (baseArgs [ "-sprt"; "alpha=0.05"; "elo0=0" ]) |> ignore
    Assert.Equal("--debug --fast", (ok (baseArgs [ "-each"; "proto=uci"; "args=--debug --fast" ])).Engines.Head.Args)
    Assert.EndsWith("Reason: Unsupported protocol.", error (baseArgs [ "-each"; "proto=xboard" ]))

[<Fact>]
let ``bayesian with pentanomial warns and turns pentanomial off`` () =
    let p = ok (baseArgs [ "-sprt"; "alpha=0.05"; "beta=0.05"; "elo0=0"; "elo1=5"; "model=bayesian" ])
    Assert.False p.Tournament.ReportPenta
    Assert.Contains(Stdout "Warning; Bayesian SPRT model not available with pentanomial statistics. Disabling pentanomial reports...", p.Messages)

[<Fact>]
let ``restart on is accepted`` () =
    let p = ok (baseArgs [ "-engine"; "restart=on"; "name=foobar"; "tc=10/1+0"; dummy ])
    Assert.True p.Engines.[2].Restart

[<Fact>]
let ``invalid log level throws`` () =
    Assert.EndsWith("Reason: Unrecognized log level option \"level\" with value \"verbose\".", error (baseArgs [ "-log"; "level=verbose" ]))

[<Fact>]
let ``affinity lists`` () =
    Assert.Equal<int list>([ 3; 5; 7; 8; 9 ], (ok (baseArgs [ "-use-affinity"; "3,5,7-9" ])).Tournament.AffinityCpus)
    Assert.True (ok (baseArgs [ "-use-affinity" ])).Tournament.Affinity
    for bad in [ "3,"; "9-7"; "x"; "3;4" ] do
        Assert.EndsWith("Reason: Bad cpu list.", error (baseArgs [ "-use-affinity"; bad ]))

[<Fact>]
let ``compliance mode is not an option`` () =
    error [ "--compliance"; "app/tests/mock/engine/dummy_engine" ] |> ignore

// ---- snapshots/cli ----

[<Fact>]
let ``snapshot unknown option`` () =
    throwsWith "Unrecognized option: -engne parsing failed.: Did you mean \"-engine\"?" [ "-engne" ]
    throwsWith "Unrecognized option: -xyzzy parsing failed." [ "-xyzzy" ]
    // distance 2 still suggests (both checked against the reference itself)
    throwsWith "Unrecognized option: -rnds parsing failed.: Did you mean \"-rounds\"?" [ "-rnds" ]
    throwsWith "Unrecognized option: -rnd parsing failed.: Did you mean \"-srand\"?" [ "-rnd" ]

[<Fact>]
let ``snapshot missing value`` () =
    throwsWith "Error while reading option \"-concurrency\" with value \"-concurrency\"\nReason: Option \"-concurrency\" expects exactly one value." [ "-concurrency" ]

[<Fact>]
let ``snapshot unexpected value`` () =
    throwsWith "Error while reading option \"-recover\" with value \"true\"\nReason: Option \"-recover\" does not accept parameters." [ "-recover"; "true" ]

[<Fact>]
let ``snapshot invalid key value`` () =
    throwsWith "Error while reading option \"-log\" with value \"level\"\nReason: Option \"-log\" expects key=value pairs, got \"level\"." [ "-log"; "level" ]

[<Fact>]
let ``snapshot version`` () =
    for flag in [ "-version"; "--version"; "-v"; "--v" ] do
        match parse env (baseArgs [ flag; "-bogus" ]) with
        | Exit(_, text) -> Assert.Equal("EngineBattle 1.8.2 (abc1234)\n", text)
        | other -> failwithf "%s: expected an exit, got %A" flag other

[<Fact>]
let ``help is match's own text, for no arguments, -help and --help`` () =
    for args in [ []; [ "-help" ]; [ "--help" ] ] do
        match parse env args with
        | Exit(_, text) ->
            Assert.StartsWith("EngineBattle match - play a match between UCI engines from the command line\n", text)
            for flag in [ "-engine"; "-each"; "-concurrency"; "-sprt"; "-openings"; "-pgnout"; "-config"; "-quick"; "-log" ] do
                Assert.Contains(flag, text)
            Assert.DoesNotContain("\r", text)
        | other -> failwithf "expected an exit, got %A" other

// ---- sanitising beyond cli_test.cpp ----

[<Fact>]
let ``negative concurrency counts back from the hardware threads`` () =
    let p = ok (baseArgs [ "-concurrency"; "-2" ])
    Assert.Equal(14, p.Tournament.Concurrency)
    Assert.Equal(Stdout "Info: Adjusted concurrency to 14 based on number of available hardware threads.", p.Messages.Head)

[<Fact>]
let ``book warnings and interval defaults`` () =
    let p = ok (baseArgs [ "-ratinginterval"; "0"; "-scoreinterval"; "0" ])
    Assert.Equal((Int32.MaxValue, Int32.MaxValue), (p.Tournament.RatingInterval, p.Tournament.ScoreInterval))
    Assert.Equal<Message list>(
        [ Stdout "Warning: No opening book specified! Consider using one, otherwise all games will be played from the starting position."
          Stdout "Warning: Unknown opening format, 2. All games will be played from the starting position." ],
        p.Messages)
    Assert.Equal(42UL, p.Tournament.Seed)
    Assert.True p.Tournament.ReportPenta

[<Fact>]
let ``penta is off for cutechess output and single games`` () =
    Assert.False (ok (baseArgs [ "-output"; "format=cutechess" ])).Tournament.ReportPenta
    Assert.False (ok (baseArgs [ "-games"; "1" ])).Tournament.ReportPenta

[<Fact>]
let ``windows appends exe to a command without a dot`` () =
    let win = { env with IsWindows = true }
    match parse win (baseArgs [ "-engine"; "cmd=./eng"; "tc=1+0"; "-engine"; "cmd=sf"; "tc=1+0" ]) with
    | Run p -> Assert.Equal<string list>([ "app/tests/mock/engine/dummy_engine.exe"; "app/tests/mock/engine/dummy_engine.exe"; "./eng"; "sf.exe" ], p.Engines |> List.map (fun e -> e.Cmd))
    | other -> failwithf "%A" other

[<Fact>]
let ``missing binary and opening file`` () =
    let noFiles = { env with IsFile = fun _ -> false }
    match parse noFiles [ "-engine"; "dir=eng"; "cmd=sf"; "tc=1+0"; "-engine"; "cmd=sf"; "tc=1+0" ] with
    | Failed(_, e) -> Assert.Equal("Engine binary does not exist: " + IO.Path.Combine("eng", "sf"), e)
    | other -> failwithf "%A" other
    Assert.EndsWith("Reason: Opening file does not exist: nope.epd", error (baseArgs [ "-openings"; "file=nope.epd" ]))

[<Fact>]
let ``tc after st replaces the whole limit`` () =
    let tc = (ok [ "-engine"; dummy; "st=5"; "timemargin=10"; "tc=1+0"; "-engine"; dummy; "tc=1+0" ]).Engines.Head.Tc
    Assert.Equal({ TcLimits.Zero with Time = 1000L }, tc)

[<Fact>]
let ``negative numbers are values, not options`` () =
    // "-3" after -concurrency is its value (then counted back from 16 threads); "-x" would be an option
    Assert.Equal(13, (ok (baseArgs [ "-concurrency"; "-3" ])).Tournament.Concurrency)
    Assert.Equal("-1-2", (ok (baseArgs [ "-event"; "-1"; "-2"; "-recover" ])).Tournament.Pgn.EventName)

[<Fact>]
let ``engine names are the command's stem, as std::filesystem has it`` () =
    // checked against the reference: cmd=./.hidden is named ".hidden"
    let names = (ok [ "-engine"; "cmd=./.hidden"; "tc=1+0"; "-engine"; "cmd=eng/sf.v2.bin"; "tc=1+0"; "-engine"; "cmd=lc0"; "tc=1+0" ]).Engines |> List.map (fun e -> e.Name)
    Assert.Equal<string list>([ ".hidden"; "sf.v2"; "lc0" ], names)
    Assert.Equal("..", stem "a/..")

[<Fact>]
let ``discard before any file restores a fresh default`` () =
    // The reference restores its default-constructed configs: no engines are left
    Assert.Equal("Error: Need at least two engines to start!", error (baseArgs [ "-config"; "discard=true" ]))

[<Fact>]
let ``absurd values get the reference's texts, not .NET's`` () =
    // past decimal's range: "too large" as a duration, like any other too long time
    Assert.EndsWith("Reason: Invalid time control duration: \"1e26\"", error [ "-engine"; dummy; "tc=1e26"; "-engine"; dummy; "tc=1+0" ])
    Assert.EndsWith("Reason: Invalid time control duration: \"1e25\"", error [ "-engine"; dummy; "tc=1e25:0"; "-engine"; dummy; "tc=1+0" ])
    // abs Int32.MinValue used to throw; counted back in int64 it is just very negative
    let p = ok (baseArgs [ "-concurrency"; "-2147483648" ])
    Assert.Equal(-2147483632, p.Tournament.Concurrency)   // 16 threads in this env, minus 2^31
