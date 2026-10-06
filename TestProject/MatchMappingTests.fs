/// A match command line as an EngineBattle tournament (ChessLibrary/Match/MatchMapping.fs):
/// each flag lands on the EngineBattle setting that plays it, and what EngineBattle cannot do yet
/// is an error (a different match) or a note (a detail), never silently dropped.
module MatchMappingTests

open System
open System.IO
open Xunit
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.Match

let private env =
    { MatchArgs.defaultEnv () with
        HardwareThreads = 16
        IsWindows = false
        PathExists = fun _ -> true
        IsFile = fun _ -> true
        RandomSeed = fun () -> 42UL }

let private sf = [ "-engine"; "cmd=engines/sf"; "name=SF"; "tc=10+0.1"; "option.Hash=16"; "option.Threads=2" ]
let private lc0 = [ "-engine"; "cmd=engines/lc0"; "name=Lc0"; "tc=10+0.1" ]

let private mapped args =
    match MatchArgs.parse env args with
    | MatchArgs.Run p ->
        match MatchMapping.map p with
        | Ok m -> m
        | Error e -> failwithf "expected a mapping, got: %s" e
    | other -> failwithf "expected a run, got %A" other

let private mapError args =
    match MatchArgs.parse env args with
    | MatchArgs.Run p ->
        match MatchMapping.map p with
        | Error e -> e
        | Ok _ -> failwith "expected a mapping error"
    | other -> failwithf "expected a run, got %A" other

let private hasNote (fragment: string) (m: MatchMapping.Mapped) =
    Assert.True(m.Notes |> List.exists (fun n -> n.Contains fragment), sprintf "no note with '%s' in %A" fragment m.Notes)

[<Fact>]
let ``a head-to-head plays pairs over the rounds`` () =
    let m = mapped (sf @ lc0 @ [ "-rounds"; "50"; "-openings"; "file=book.epd"; "order=random"; "-srand"; "7"; "-pgnout"; "file=out.pgn" ])
    let t = m.Tournament
    Assert.Equal("RR", t.TournamentMode)
    Assert.Equal(50, t.Rounds)
    Assert.True t.Opening.OpeningsTwice
    Assert.Equal(Some "book.epd", t.Opening.OpeningsPath)
    Assert.True t.Opening.RandomOpenings
    Assert.Equal(7, t.Opening.Seed)
    Assert.Equal(Int32.MaxValue, t.Opening.OpeningsPly)
    Assert.Equal("out.pgn", t.PgnOutPath)
    Assert.True t.ConsoleOnly
    Assert.Equal<string list>([ "SF"; "Lc0" ], t.EngineSetup.Engines |> List.map (fun e -> e.Name))
    Assert.Equal(Path.GetFullPath "engines/sf", t.EngineSetup.Engines.Head.Path)

[<Fact>]
let ``one game per round is one game per opening`` () =
    Assert.False (mapped (sf @ lc0 @ [ "-games"; "1" ])).Tournament.Opening.OpeningsTwice

[<Fact>]
let ``engine limits become time settings, one per distinct limit`` () =
    let m = mapped (sf @ [ "-engine"; "cmd=b"; "name=B"; "tc=40/60+0" ] @ [ "-engine"; "cmd=c"; "name=C"; "tc=10+0.1" ])
    let t = m.Tournament
    let tcs = t.TimeControl.TimeConfigs
    Assert.Equal(2, tcs.Length)
    Assert.Equal<int list>([ 1; 2 ], tcs |> List.map (fun c -> c.Id))
    Assert.Equal((TimeSpan.FromSeconds 10.0, TimeSpan.FromMilliseconds 100.0), (tcs.[0].Fixed, tcs.[0].Increment))
    Assert.Equal(TimeSpan.FromSeconds 60.0, tcs.[1].Fixed)
    Assert.Equal<int list>([ 1; 2; 1 ], t.EngineSetup.Engines |> List.map (fun e -> e.TimeControlID))
    // moves per period are each engine's own, as the reference keeps them
    Assert.Equal<int list>([ 0; 40 ], tcs |> List.map (fun c -> c.MovesToGo))
    Assert.Equal((0, 0), (t.TimeControl.WmovesToGo, t.TimeControl.BmovesToGo))
    Assert.False(m.Notes |> List.exists (fun n -> n.Contains "moves-to-go"), sprintf "%A" m.Notes)

[<Fact>]
let ``nodes limit the search, and win over a clock`` () =
    let m = mapped [ "-engine"; "cmd=a"; "name=A"; "nodes=800"; "-engine"; "cmd=b"; "name=B"; "nodes=800"; "tc=10+0.1" ]
    let tcs = m.Tournament.TimeControl.TimeConfigs
    Assert.True(tcs |> List.forall (fun c -> c.NodeLimit && c.Nodes = 800))
    Assert.Equal(1, tcs.Length)
    hasNote "tc= is not applied together with nodes=" m

[<Fact>]
let ``st is a time per move with no clock, and timemargin is its allowance`` () =
    let m = mapped [ "-engine"; "cmd=a"; "name=A"; "-engine"; "cmd=b"; "name=B"; "-each"; "st=0.5"; "timemargin=40" ]
    let tcs = m.Tournament.TimeControl.TimeConfigs
    Assert.Equal(1, tcs.Length)
    let c = tcs.Head
    Assert.True(c.IsMoveTime)
    Assert.Equal(TimeSpan.FromMilliseconds 500.0, c.MoveTime)
    Assert.Equal((TimeSpan.Zero, TimeSpan.Zero, false), (c.Fixed, c.Increment, c.NodeLimit))
    // timemargin is the tournament's MoveOverhead, which st uses as its margin only
    Assert.Equal(TimeSpan.FromMilliseconds 40.0, m.Tournament.MoveOverhead)
    Assert.False(m.Notes |> List.exists (fun n -> n.Contains "timemargin"), sprintf "%A" m.Notes)

[<Fact>]
let ``timemargin with st is one margin for all, and only when all play st`` () =
    let mixed = mapped [ "-engine"; "cmd=a"; "name=A"; "st=1"; "timemargin=40"; "-engine"; "cmd=b"; "name=B"; "tc=10+0.1" ]
    Assert.Equal(TimeSpan.Zero, mixed.Tournament.MoveOverhead)
    hasNote "only when all play st=" mixed
    let differ = mapped [ "-engine"; "cmd=a"; "name=A"; "st=1"; "timemargin=40"; "-engine"; "cmd=b"; "name=B"; "st=1"; "timemargin=90" ]
    Assert.Equal(TimeSpan.FromMilliseconds 90.0, differ.Tournament.MoveOverhead)
    hasNote "different timemargin=" differ

[<Fact>]
let ``nodes win over st, and say so`` () =
    let m = mapped [ "-engine"; "cmd=a"; "name=A"; "-engine"; "cmd=b"; "name=B"; "-each"; "st=1"; "nodes=800" ]
    let c = m.Tournament.TimeControl.TimeConfigs.Head
    Assert.True(c.NodeLimit && not c.IsMoveTime)
    hasNote "st= is not applied together with nodes=" m

[<Fact>]
let ``what would be a different match is an error`` () =
    Assert.Equal("Error: A: a depth limit (depth=/plies=) is not supported by EngineBattle yet.",
                 mapError [ "-engine"; "cmd=a"; "name=A"; "depth=10"; "-engine"; "cmd=b"; "name=B"; "depth=10" ])
    Assert.StartsWith("Error: A: nodes=3000000000 is larger",
                      mapError [ "-engine"; "cmd=a"; "name=A"; "nodes=3000000000"; "-engine"; "cmd=b"; "name=B"; "nodes=1" ])

[<Fact>]
let ``options go Threads first, a repeated one keeps its place and takes its last value`` () =
    let m = mapped (sf @ lc0 @ [ "-each"; "option.Hash=64" ])
    let opts = m.Tournament.EngineSetup.Engines.Head.Options
    Assert.Equal<string list>([ "Threads"; "Hash" ], opts.Keys |> List.ofSeq)
    Assert.Equal(box "64", opts.["Hash"])

[<Fact>]
let ``adjudication in pawns, and off when the reference has it off`` () =
    let t = (mapped (sf @ lc0 @ [ "-draw"; "movenumber=40"; "movecount=8"; "score=10"; "-resign"; "movecount=3"; "score=600" ])).Tournament
    Assert.Equal({ MinDrawMove = 40; DrawMoveLength = 8; MaxDrawScore = 0.10 }, t.Adjudication.DrawOption)
    Assert.Equal({ MinWinMove = 0; WinMoveLength = 3; MinWinScore = 6.0 }, t.Adjudication.WinOption)
    let off = (mapped (sf @ lc0)).Tournament.Adjudication
    Assert.True(off.DrawOption.MinDrawMove > 5949 && off.WinOption.MinWinMove > 5949)
    Assert.False off.TBAdj.UseTBAdjudication

[<Fact>]
let ``tablebases`` () =
    let m = mapped (sf @ lc0 @ [ "-tb"; "C:/tb/wdl;C:/tb/dtz"; "-tbignore50" ])
    let tb = m.Tournament.Adjudication.TBAdj
    // every folder, joined as Fathom expects them here
    Assert.Equal({ TablebaseDirectory = $"C:/tb/wdl{IO.Path.PathSeparator}C:/tb/dtz"; UseTBAdjudication = true; TBMen = 7 }, tb)
    hasNote "-tbignore50" m
    Assert.Equal(5, (mapped (sf @ lc0 @ [ "-tb"; "tb"; "-tbpieces"; "5" ])).Tournament.Adjudication.TBAdj.TBMen)

[<Fact>]
let ``gauntlet seeds are the challengers`` () =
    let m = mapped (sf @ lc0 @ [ "-engine"; "cmd=c"; "name=C"; "tc=1+0"; "-tournament"; "gauntlet"; "-seeds"; "2" ])
    let t = m.Tournament
    Assert.Equal(("Gauntlet", 2), (t.TournamentMode, t.Challengers))
    Assert.Equal<bool list>([ true; true; false ], t.EngineSetup.Engines |> List.map (fun e -> e.IsChallenger))
    hasNote "does not pair the seeds" m

[<Fact>]
let ``event, site, wait and concurrency`` () =
    let t = (mapped (sf @ lc0 @ [ "-event"; "Test"; "-site"; "Home"; "-wait"; "250"; "-concurrency"; "4" ])).Tournament
    Assert.Equal(("Test", "Home"), (t.Description, t.Name))
    Assert.Equal(TimeSpan.FromMilliseconds 250.0, t.DelayBetweenGames)
    Assert.Equal(4, t.TestOptions.NumberOfGamesInParallel)
    let d = (mapped (sf @ lc0)).Tournament
    Assert.Equal(("EngineBattle Match", "?"), (d.Description, d.Name))

[<Fact>]
let ``without -pgnout the games go to a temporary file`` () =
    let m = mapped (sf @ lc0)
    Assert.StartsWith(Path.Combine(Path.GetTempPath(), "EngineBattle", "match_"), m.Tournament.PgnOutPath)
    hasNote "no -pgnout" m

[<Fact>]
let ``accepted settings that are not acted on are noted`` () =
    let m =
        mapped ([ "-engine"; "cmd=a"; "name=A"; "tc=10+0.1"; "timemargin=50"; "restart=on"; "depth=12" ] @ lc0
                @ [ "-maxmoves"; "200"; "-noswap"; "-openings"; "file=b.pgn"; "start=5"; "-epdout"; "file=x.epd"
                    "-sprt"; "elo0=0"; "elo1=5"; "alpha=0.05"; "beta=0.05" ])
    for fragment in [ "timemargin=50"; "restart=on"; "depth=12"; "-maxmoves"; "-noswap"; "start=5"; "-epdout" ] do
        hasNote fragment m
    // the SPRT is applied (it stops the match), so it is not a note
    Assert.False(m.Notes |> List.exists (fun n -> n.Contains "-sprt"))

[<Fact>]
let ``a bare command is left for the PATH`` () =
    let t = (mapped [ "-engine"; "cmd=stockfish"; "tc=1+0"; "-engine"; "cmd=./lc0"; "tc=1+0" ]).Tournament
    Assert.Equal("stockfish", t.EngineSetup.Engines.Head.Path)
    Assert.Equal(Path.GetFullPath "./lc0", t.EngineSetup.Engines.[1].Path)

[<Fact>]
let ``ponder, as cutechess-cli writes it, lets every engine ponder - all or none`` () =
    Assert.True((mapped (sf @ lc0 @ [ "-each"; "ponder" ])).Tournament.AllowPondering)
    Assert.False((mapped (sf @ lc0)).Tournament.AllowPondering)
    // pondering is the whole match's in EngineBattle, and a field where some ponder is not equal
    Assert.Contains("some engines only", mapError (sf @ [ "ponder" ] @ lc0))
    // ponder=on / ponder=off as well; off is off (it used to turn pondering on)
    Assert.True((mapped (sf @ lc0 @ [ "-each"; "ponder=on" ])).Tournament.AllowPondering)
    Assert.False((mapped (sf @ lc0 @ [ "-each"; "ponder=off" ])).Tournament.AllowPondering)
    match MatchArgs.parse env (sf @ lc0 @ [ "-each"; "ponder=maybe" ]) with
    | MatchArgs.Run _ -> Assert.Fail "ponder=maybe was accepted"
    | _ -> ()
