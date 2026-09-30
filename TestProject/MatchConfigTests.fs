/// The reference's config.json (ChessLibrary/Match/MatchConfigJson.fs): the file the reference
/// 60d7a7a wrote after run B of the output tests (cutechess format, SPRT, four rounds) is read
/// and written back character for character, and the same command line parsed by EngineBattle
/// writes the same file. Plus resuming through -config file=.
module MatchConfigTests

open System
open System.IO
open Xunit
open ChessLibrary.Match
open ChessLibrary.Match.MatchStats

let private referenceFileText = """{
    "resign": {
        "move_count": 1,
        "score": 0,
        "twosided": false,
        "enabled": false
    },
    "draw": {
        "move_number": 0,
        "move_count": 1,
        "score": 0,
        "enabled": false
    },
    "maxmoves": {
        "move_count": 1,
        "enabled": false
    },
    "tb_adjudication": {
        "syzygy_dirs": "",
        "max_pieces": 0,
        "ignore_50_move_rule": false,
        "enabled": false
    },
    "opening": {
        "file": "/tmp/fcbuild/book.epd",
        "format": 0,
        "order": 1,
        "plies": -1,
        "start": 1
    },
    "pgn": {
        "additional_lines_rgx": [],
        "event_name": "EngineBattle Match",
        "site": "?",
        "file": "",
        "notation": 0,
        "append_file": true,
        "track_nodes": false,
        "track_seldepth": false,
        "track_nps": false,
        "track_hashfull": false,
        "track_tbhits": false,
        "track_timeleft": false,
        "track_latency": false,
        "track_pv": false,
        "min": false,
        "crc": false
    },
    "epd": {
        "file": "",
        "append_file": true
    },
    "sprt": {
        "alpha": 0.05,
        "beta": 0.05,
        "elo0": 0.0,
        "elo1": 5.0,
        "model": "normalized",
        "enabled": true
    },
    "config_name": "config.json",
    "output": 1,
    "seed": 4193720206659742432,
    "variant": 0,
    "type": 0,
    "gauntlet_seeds": 1,
    "ratinginterval": 3,
    "scoreinterval": 1,
    "wait": 0,
    "autosaveinterval": 20,
    "games": 2,
    "rounds": 4,
    "concurrency": 1,
    "force_concurrency": false,
    "recover": false,
    "noswap": false,
    "reverse": false,
    "report_penta": false,
    "affinity": false,
    "check_mate_pvs": false,
    "show_latency": false,
    "log": {
        "file": "",
        "level": 3,
        "append_file": true,
        "compress": false,
        "realtime": true,
        "engine_coms": false
    },
    "engines": [
        {
            "name": "SF-a",
            "dir": "",
            "cmd": "/mnt/c/Dev/Chess/Engines/stockfish/stockfish-windows-x86-64-universal.exe",
            "args": "",
            "restart": false,
            "options": [
                [
                    "Threads",
                    "1"
                ],
                [
                    "Hash",
                    "16"
                ]
            ],
            "limit": {
                "tc": {
                    "increment": 0,
                    "fixed_time": 0,
                    "time": 0,
                    "moves": 0,
                    "timemargin": 0
                },
                "nodes": 4000,
                "plies": 0
            },
            "variant": 0
        },
        {
            "name": "SF-b",
            "dir": "",
            "cmd": "/mnt/c/Dev/Chess/Engines/stockfish/stockfish-windows-x86-64-universal.exe",
            "args": "",
            "restart": false,
            "options": [
                [
                    "Threads",
                    "1"
                ],
                [
                    "Hash",
                    "16"
                ]
            ],
            "limit": {
                "tc": {
                    "increment": 0,
                    "fixed_time": 0,
                    "time": 0,
                    "moves": 0,
                    "timemargin": 0
                },
                "nodes": 3000,
                "plies": 0
            },
            "variant": 0
        }
    ],
    "stats": {
        "SF-a vs SF-b": {
            "wins": 2,
            "losses": 5,
            "draws": 1,
            "penta_WW": 0,
            "penta_WD": 0,
            "penta_WL": 0,
            "penta_DD": 0,
            "penta_LD": 0,
            "penta_LL": 0
        }
    }
}
"""
// The writer's line ends are LF, as the reference's; a checkout with core.autocrlf gives this
// literal CRLF, so it is compared with LF line ends whatever the working tree has.
let private referenceFile = referenceFileText.Replace("\r\n", "\n")

let private env =
    { MatchArgs.defaultEnv () with
        HardwareThreads = 24; IsWindows = false; PathExists = (fun _ -> true); IsFile = (fun _ -> true)
        LoadConfig = MatchConfigJson.load }

let private sf = "/mnt/c/Dev/Chess/Engines/stockfish/stockfish-windows-x86-64-universal.exe"

let private runB =
    [ "-engine"; "cmd=" + sf; "name=SF-a"; "nodes=4000"; "-engine"; "cmd=" + sf; "name=SF-b"; "nodes=3000"
      "-each"; "option.Threads=1"; "option.Hash=16"; "-concurrency"; "1"
      "-openings"; "file=/tmp/fcbuild/book.epd"; "format=epd"; "order=sequential"
      "-rounds"; "4"; "-games"; "2"; "-ratinginterval"; "3"; "-scoreinterval"; "1"; "-output"; "format=cutechess"
      "-sprt"; "elo0=0"; "elo1=5"; "alpha=0.05"; "beta=0.05"; "-srand"; "4193720206659742432" ]

[<Fact>]
let ``the reference's own config.json reads and writes back unchanged`` () =
    let c = MatchConfigJson.parse referenceFile
    Assert.Equal(referenceFile, MatchConfigJson.write c.Tournament c.Engines c.Stats)

[<Fact>]
let ``the same command line writes the same file`` () =
    match MatchArgs.parse env runB with
    | MatchArgs.Run p ->
        let stats = [ "SF-a vs SF-b", Stats.OfWld(2, 5, 1) ]
        Assert.Equal(referenceFile, MatchConfigJson.write p.Tournament p.Engines stats)
    | other -> failwithf "%A" other

[<Fact>]
let ``what the file holds`` () =
    let c = MatchConfigJson.parse referenceFile
    Assert.Equal((MatchArgs.Cutechess, 3, 4, 2), (c.Tournament.Output, c.Tournament.RatingInterval, c.Tournament.Rounds, c.Tournament.Games))
    Assert.Equal(4193720206659742432UL, c.Tournament.Seed)
    let expected : MatchArgs.SprtConfig = { Enabled = true; Alpha = 0.05; Beta = 0.05; Elo0 = 0.0; Elo1 = 5.0; Model = "normalized" }
    Assert.Equal(expected, c.Tournament.Sprt)
    Assert.Equal<string list>([ "SF-a"; "SF-b" ], c.Engines |> List.map (fun e -> e.Name))
    Assert.Equal<(string * string) list>([ "Threads", "1"; "Hash", "16" ], c.Engines.Head.Options)
    Assert.Equal((4000L, 3000L), (c.Engines.[0].Nodes, c.Engines.[1].Nodes))
    Assert.Equal<(string * Stats) list>([ "SF-a vs SF-b", Stats.OfWld(2, 5, 1) ], c.Stats)

[<Fact>]
let ``-config file= resumes the settings; later flags still apply; a missing file is the reference's error`` () =
    let path = Path.Combine(Path.GetTempPath(), $"eb-config-{Guid.NewGuid():N}.json")
    try
        File.WriteAllText(path, referenceFile)
        match MatchArgs.parse env [ "-config"; "file=" + path; "-ratinginterval"; "7" ] with
        | MatchArgs.Run p ->
            Assert.Equal((4, 7), (p.Tournament.Rounds, p.Tournament.RatingInterval))
            Assert.Equal(MatchArgs.Stdout $"Loading config file: {path}", p.Messages.Head)
            Assert.Equal(1, p.Stats.Length)
        | other -> failwithf "%A" other
    finally
        File.Delete path
    match MatchArgs.parse env [ "-config"; "file=nowhere.json" ] with
    | MatchArgs.Failed(_, e) ->
        Assert.Equal("Error while reading option \"-config\" with value \"file=nowhere.json\"\nReason: File not found: nowhere.json", e)
    | other -> failwithf "%A" other

[<Fact>]
let ``save replaces the file whole`` () =
    let path = Path.Combine(Path.GetTempPath(), $"eb-config-{Guid.NewGuid():N}.json")
    try
        let c = MatchConfigJson.parse referenceFile
        File.WriteAllText(path, "old")
        MatchConfigJson.save path c.Tournament c.Engines c.Stats
        Assert.Equal(referenceFile, File.ReadAllText path)
        Assert.False(File.Exists(path + ".tmp"))
    finally
        File.Delete path

[<Fact>]
let ``stats for the file come from the scoreboard, pair by pair in command-line order`` () =
    let engines =
        [ for n in [ "A"; "B"; "C" ] -> { MatchArgs.EngineConfig.Empty with Name = n } ]
    let board = MatchScoreboard.Scoreboard([ "A"; "B"; "C" ], false)
    board.Add("B", "A", "1-0", "h") |> ignore
    board.Add("C", "B", "1/2-1/2", "h") |> ignore
    Assert.Equal<(string * Stats) list>(
        [ "A vs B", Stats.OfWld(0, 1, 0); "A vs C", Stats.Empty; "B vs C", Stats.OfWld(0, 0, 1) ],
        MatchConfigJson.statsOf engines board)

[<Fact>]
let ``a negative round count round-trips as the reference's size_t`` () =
    let c = MatchConfigJson.parse referenceFile
    let t = { c.Tournament with Rounds = -1 }
    let text = MatchConfigJson.write t c.Engines c.Stats
    Assert.Contains("\"rounds\": 18446744073709551615,", text)
    Assert.Equal(-1, (MatchConfigJson.parse text).Tournament.Rounds)

[<Fact>]
let ``a save that cannot be written throws and leaves no temporary file`` () =
    let dir = Path.Combine(Path.GetTempPath(), $"eb-nodir-{Guid.NewGuid():N}")
    let path = Path.Combine(dir, "cfg.json")
    let c = MatchConfigJson.parse referenceFile
    Assert.ThrowsAny<IOException>(fun () -> MatchConfigJson.save path c.Tournament c.Engines c.Stats) |> ignore
    Assert.False(Directory.Exists dir)
