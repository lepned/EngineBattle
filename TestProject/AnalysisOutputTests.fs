/// AnalysisOutput.step - what the analysis wrapper makes of an engine's output - run directly over
/// whole searches recorded from real engines (TestData/AnalysisOutput: Lc0 BT4 with verbose move
/// stats and one of our own nets, White and Black to move), with no process involved. The
/// end-to-end behaviour of the wrapper is pinned by EngineCharacterizationTests; these hold the
/// state machine to real output, line by line.
module AnalysisOutputTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.EngineTypes
open ChessLibrary.EngineProtocol
open ChessLibrary.MoveTypes

let private dataDir = Path.Combine(AppContext.BaseDirectory, "TestData", "AnalysisOutput")

/// Whole searches whose output all belongs to the position before it.
let private samples =
    Directory.GetFiles(dataDir, "*.txt")
    |> Array.filter (fun f -> not (Path.GetFileName(f).StartsWith "stale"))
    |> Array.sort

let private positionOf (positionCommand: string) =
    let board = Chess.Board()
    board.PlayCommands positionCommand
    let moveList = Array.init 256 (fun _ -> Unchecked.defaultof<TMove>)
    let white = board.Position.STM = 0uy
    let mutable b = board
    let pos =
        AnalysisOutput.boardPosition board (obj ()) (fun () -> white)
            (fun _ lan -> BoardUtils.getShortSanPVFromLongSanPVFast moveList &b lan)
    pos, white, board

/// Runs every line through step, as the wrapper does after the handshake.
let private run (pos: AnalysisOutput.IPosition) (lines: string seq) =
    let effects = ResizeArray<AnalysisOutput.Effect>()
    let mutable state = AnalysisOutput.State.Initial
    for line in lines do
        let next, produced = AnalysisOutput.step "Eng" pos state line
        state <- next
        effects.AddRange produced
    state, List.ofSeq effects

let private updates effects = effects |> List.choose (function AnalysisOutput.Update u -> Some u | _ -> None)

[<Fact>]
let ``There are recorded searches to run`` () =
    Assert.True(samples.Length >= 5, sprintf "found %d" samples.Length)

[<Fact>]
let ``Every recorded search yields a status per scored line, a stats set per node line and one bestmove`` () =
    for file in samples do
        let lines = File.ReadAllLines file
        let positionCommand, output = lines.[0], lines.[1..] |> Array.filter (fun l -> l <> "")
        let pos, white, board = positionOf positionCommand
        let _, effects = run pos output
        let ups = updates effects
        let name = Path.GetFileName file
        // Nothing printed: every move the engine named was legal where it named it.
        Assert.True((effects |> List.forall (function AnalysisOutput.Print _ -> false | _ -> true)), name)
        // One Status and one Info per scored info line.
        let scored = output |> Array.filter (fun l -> (Regex.legacyGetEssentialDataWithEPS l white).IsSome)
        let statuses = ups |> List.choose (function Status s -> Some s | _ -> None)
        Assert.Equal(scored.Length, statuses.Length)
        Assert.Equal(scored.Length, ups |> List.filter (function Info _ -> true | _ -> false) |> List.length)
        // The eval is White's view: the line's score as the parser reads it for the side to move.
        let firstParsed = Regex.legacyGetEssentialDataWithEPS scored.[0] white
        match firstParsed with
        | Some (_, eval, _, _, _, _, _, _, _, _) -> Assert.Equal(eval, statuses.[0].Eval)
        | None -> Assert.Fail name
        // One stats set per "info string node" line, as many moves as stat lines since the last one.
        let expectedSets =
            let sets = ResizeArray<int>()
            let mutable n = 0
            for l in output do
                if l.StartsWith("info string", StringComparison.Ordinal) && l.Contains "N:" then
                    if l.StartsWith("info string node", StringComparison.Ordinal) then
                        // the node line closes the set and is in it (unless it opens an empty one)
                        sets.Add(if n = 0 then 0 else n + 1)
                        n <- 0
                    else n <- n + 1
            List.ofSeq sets
        let sets = ups |> List.choose (function NNSeq l -> Some l | _ -> None)
        Assert.Equal<int list>(expectedSets, sets |> List.map (fun s -> s.Count))
        for set in sets do
            for m in set do
                if m.LANMove <> "node" then Assert.False(String.IsNullOrEmpty m.SANMove, sprintf "%s: no SAN for %s" name m.LANMove)
        // The engine's move as BestMove (legal, with a PV and the pre-move FEN), then Done.
        let finals = ups |> List.filter (function Done _ | BestMove _ -> true | _ -> false)
        match finals with
        | [ BestMove bm; Done "Eng" ] ->
            let expected = (Array.last output).Split(' ').[1]
            Assert.Equal(expected, bm.Move)
            Assert.False(String.IsNullOrEmpty bm.PV, name)
            Assert.Equal(board.FEN(), bm.FEN)
        | other -> Assert.Fail(sprintf "%s: expected BestMove then Done, got %A" name other)

[<Fact>]
let ``A new position drops the previous variation before the next line`` () =
    let pos, _, _ = positionOf "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let state, _ = run pos [ "info depth 3 score cp 20 nodes 100 pv e2e4 e7e5" ]
    Assert.Equal("e2e4 e7e5", state.PvLong)
    let cleared = AnalysisOutput.withoutPv state
    Assert.Equal("", cleared.Pv)
    Assert.Equal("", cleared.PvLong)
    // A bestmove with no pv line since: the numbered move, not the old variation.
    let _, fromCleared = AnalysisOutput.step "Eng" pos { cleared with Mode = AnalysisOutput.Idle } "bestmove e2e4"
    match fromCleared |> List.choose (function AnalysisOutput.Update (BestMove b) -> Some b.PV | _ -> None) with
    | [ pv ] -> Assert.Equal("1.e4", pv)
    | other -> Assert.Fail(sprintf "%A" other)

[<Fact>]
let ``Options are collected until uciok and reported once`` () =
    let pos, _, _ = positionOf "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let state, effects = run pos [ "option name Hash type spin default 16 min 1 max 64"; "option name Ponder type check default false"; "uciok" ]
    Assert.Equal(AnalysisOutput.Idle, state.Mode)
    match updates effects with
    | [ UCIInfo lines ] -> Assert.Equal<string list>([ "option name Hash type spin default 16 min 1 max 64"; "option name Ponder type check default false" ], List.ofSeq lines)
    | other -> Assert.Fail(sprintf "%A" other)

[<Fact>]
let ``Output of a stopped search that arrives after the next position is read against the new one`` () =
    // QUIRK, recorded from the analysis page with one of our nets: "stop" and the next position went
    // out together, and the stopped search's last lines and its bestmove came in after them. They
    // are parsed against the NEW position: its PVs do not convert and its bestmove is illegal
    // there, so no BestMove is reported - only Done and the illegal-move message. In that log 47
    // of 80 searches saw some of this. Not changed by the rewrite; a fix (skip output until the
    // stopped search's bestmove) is its own decision.
    let lines = File.ReadAllLines(Path.Combine(dataDir, "stale_after_stop.txt")) |> Array.filter (fun l -> l <> "")
    let pos, _, _ = positionOf lines.[0]
    let _, effects = run pos lines.[1..]
    let ups = updates effects
    Assert.Equal(1, ups |> List.filter (function Done _ -> true | _ -> false) |> List.length)
    Assert.Empty(ups |> List.filter (function BestMove _ -> true | _ -> false))
    let printed = effects |> List.choose (function AnalysisOutput.Print t -> Some t | _ -> None)
    Assert.StartsWith("Eng played an illegal move here: bestmove h1g1", printed.[0])

[<Fact>]
let ``One node in a policy test: the earliest top-P move gets the node's Q`` () =
    let pos, _, _ = positionOf "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let _, effects =
        run pos
            [ "info string e2e4  (322 ) N:       0 (+ 0) (P: 40.00%) (Q:  0.10000) (V:  0.0900)"
              "info string d2d4  (293 ) N:       0 (+ 0) (P: 40.00%) (Q:  0.20000) (V:  0.0400)"
              "info string node  (  20) N:       1 (+ 0) (P: 100.0%) (Q:  0.55000) (V:  0.0800)" ]
    match updates effects with
    | [ NNSeq set ] ->
        Assert.Equal<(string * float) list>([ "e2e4", 0.55; "d2d4", 0.20; "node", 0.55 ], [ for m in set -> m.LANMove, m.Q ])
    | other -> Assert.Fail(sprintf "%A" other)

[<Fact>]
let ``No move ends the search quietly in a finished position, and is an illegal move elsewhere`` () =
    let printed (effects: AnalysisOutput.Effect list) =
        effects |> List.exists (function AnalysisOutput.Print t -> t.Contains "illegal move" | _ -> false)
    let noBestMove (effects: AnalysisOutput.Effect list) =
        effects |> List.forall (function AnalysisOutput.Update (BestMove _) -> false | _ -> true)
    let mated, _, _ = positionOf "position fen rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
    let start, _, _ = positionOf "position startpos"
    for line in [ "bestmove (none)"; "bestmove 0000" ] do
        let _, effects = AnalysisOutput.step "Eng" mated AnalysisOutput.State.Initial line
        Assert.Contains(AnalysisOutput.Update (Done "Eng"), effects)
        Assert.False(printed effects, line)
        Assert.True(noBestMove effects, line)
    for line in [ "bestmove (none)"; "bestmove 0000"; "bestmove" ] do
        let _, effects = AnalysisOutput.step "Eng" start AnalysisOutput.State.Initial line
        Assert.Contains(AnalysisOutput.Update (Done "Eng"), effects)
        Assert.True(printed effects, line)
        Assert.True(noBestMove effects, line)

[<Fact>]
let ``A line outside the protocol (a dump command's answer, an error) is shown and changes nothing`` () =
    let pos, _, _ = positionOf "position startpos"
    let state = { AnalysisOutput.State.Initial with Mode = AnalysisOutput.Search }
    let next, effects = AnalysisOutput.step "Ceres" pos state "error Unknown command: dump-uci"
    Assert.Equal<AnalysisOutput.Effect list>([ AnalysisOutput.Print "Ceres: error Unknown command: dump-uci" ], effects)
    Assert.Equal(state, next)
    // a message as info string too; move stats stay stats
    let _, effects = AnalysisOutput.step "Ceres" pos state "info string No search manager created"
    Assert.Equal<AnalysisOutput.Effect list>([ AnalysisOutput.Print "Ceres: info string No search manager created" ], effects)
    // protocol lines are not echoed
    let _, effects = AnalysisOutput.step "Ceres" pos state "info depth 1 score cp 10 nodes 1 nps 1 time 1 pv e2e4"
    Assert.DoesNotContain(effects, fun e -> match e with AnalysisOutput.Print _ -> true | _ -> false)
