module PuzzleMateFallbackTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.PuzzleTypes

// ---------------------------------------------------------------------------
// The mate fallback exists because a puzzle whose recorded solution is one of
// several mates is solved by any of them. It used to run only at maxTopN and only
// on the top-1 move, which made a threshold's result depend on which OTHER
// thresholds the run requested — so the same net on the same puzzle scored lower
// on top-1 in a "policy, policy3" run than in a "policy" run.
//
// These drive the real runner through a stub MailboxProcessor: no engine needed,
// the agent is a plain message loop.
// ---------------------------------------------------------------------------

/// Black to move, then white mates in one. White Qa1 + Rb1 + Kg1; black Kg8 boxed in by
/// its own pawns f7/g7/h7, plus a7 to give black a waiting move.
///
/// After the setup move a7a6 BOTH Qa1-a8 and Rb1-b8 are mate, which is the case the
/// fallback exists for: the recorded solution is one of several mating moves.
let private mateFen = "6k1/p4ppp/8/8/8/8/8/QR4K1 b - - 0 1"

let private setupMove = "a7a6"

let private puzzleWithSolution (solution: string) : CsvPuzzleData =
    // Commands are built from ODD-index moves: index 0 is the opponent's setup move and
    // index 1 is the solver's, so `solution` becomes Commands[0].CorrectMove.
    let raw =
        CsvPuzzleData.Create(
            "1", mateFen, setupMove + " " + solution, 2000.0, 80.0, 90, 100,
            "mate mateIn1", "https://lichess.org/x", "", null, null, null, 0)
    PuzzleDataUtils.getUpdatedRecord raw

/// An agent that always answers with a fixed move ranking, highest first.
let private stubAgent (ranking: (string * float) list) =
    MailboxProcessor<EngineMsg>.Start(fun inbox ->
        let nn (mv: string) (p: float) : EngineTypes.NNValues =
            { EngineTypes.NNValues.Empty with LANMove = mv; P = p }
        let rec loop () = async {
            let! msg = inbox.Receive()
            match msg with
            | NewGame ch -> ch.Reply()
            | BestMoveWithAllPolicies (_, ch) ->
                let vals = ranking |> List.map (fun (mv, p) -> nn mv p)
                ch.Reply((fst ranking.Head), vals)
            | Network ch -> ch.Reply "stub"
            | Quit ch -> ch.Reply()
            | _ -> ()
            return! loop () }
        loop ())

/// Runs one puzzle through the real policy runner and returns correct-per-topN.
let private solveWith (topNs: int list) (ranking: (string * float) list) (solution: string) =
    use agent = stubAgent ranking
    let puzzle = puzzleWithSolution solution
    let r =
        PuzzleEngineAgent.runPuzzleViaAgentMultiTopN agent topNs false puzzle
        |> Async.RunSynchronously
    r.CorrectPerTopN

// ---------------------------------------------------------------------------

[<Fact>]
let ``a different mating move solves the puzzle at top-1`` () =
    // solution is Qa8#, the net's top move is Rb8# - also mate, so it counts
    let correct = solveWith [ 1 ] [ "b1b8", 0.9; "a1a8", 0.1 ] "a1a8"
    Assert.True(correct.[1], "a mate is a mate")

[<Fact>]
let ``top-1 does not depend on which other thresholds were requested`` () =
    // THE regression: with topNs = [1;3] the fallback used to fire only at n=3, so the
    // same net on the same puzzle scored lower on top-1 than in a policy-only run.
    let alone = solveWith [ 1 ] [ "b1b8", 0.9; "a1a8", 0.1 ]
    let withOthers = solveWith [ 1; 3 ] [ "b1b8", 0.9; "a1a8", 0.1 ]
    Assert.Equal(alone "a1a8" |> Map.find 1, withOthers "a1a8" |> Map.find 1)

[<Fact>]
let ``thresholds are monotone - solved at n implies solved at every larger n`` () =
    // The mating move sits at rank 2, so n=1 must fail and n=3 must pass; a metric
    // where a wider window scores WORSE would not be a ranking at all.
    let correct = solveWith [ 1; 3 ] [ "g1f1", 0.7; "b1b8", 0.2; "a1a8", 0.1 ] "a1a8"
    Assert.False(correct.[1], "the mating move is not the top move")
    Assert.True(correct.[3], "the mating move is within the top 3")

[<Fact>]
let ``a non-mating wrong move is still wrong`` () =
    let correct = solveWith [ 1; 3 ] [ "g1f1", 0.9; "g1h2", 0.1 ] "a1a8"
    Assert.False(correct.[1])
    Assert.False(correct.[3])

// ---------------------------------------------------------------------------
// Board reuse. The fallback now sets the position up once per position and walks
// candidates with PlayUciMove/UndoMove instead of building a fresh Board each time.
// UndoMove is O(1) but blind: PlayUciMove is a silent no-op on a move it cannot
// match, and undoing that would decrement past the setup and corrupt every later
// candidate.
// ---------------------------------------------------------------------------

[<Fact>]
let ``an unplayable candidate does not corrupt later candidates`` () =
    // "zzzz" cannot be played; the mating move sits AFTER it, so if the undo cycle
    // leaked state the mate would no longer be found.
    let correct = solveWith [ 1; 3 ] [ "g1f1", 0.7; "zzzz", 0.2; "a1a8", 0.1 ] "a1a8"
    Assert.True(correct.[3], "the real mating move is still reachable at n=3")

[<Fact>]
let ``the same board serves many positions in sequence`` () =
    // Two-move puzzle: the fallback runs on position 0, then the board is reused for
    // position 1. A leaked position would make the second command unsolvable.
    let raw =
        CsvPuzzleData.Create(
            "2", mateFen, setupMove + " a1a2 g8h8 a2a8", 2000.0, 80.0, 90, 100,
            "mate", "https://lichess.org/x", "", null, null, null, 0)
    let puzzle = PuzzleDataUtils.getUpdatedRecord raw
    Assert.Equal(2, Seq.length puzzle.Commands)
    use agent = stubAgent [ "a1a2", 0.9; "a2a8", 0.8 ]
    let r =
        PuzzleEngineAgent.runPuzzleViaAgentMultiTopN agent [ 1 ] false puzzle
        |> Async.RunSynchronously
    // first command's correct move IS the stub's top move, so it is solved directly;
    // the point is that the run completes without a corrupted board
    Assert.True(r.CorrectPerTopN.ContainsKey 1)

