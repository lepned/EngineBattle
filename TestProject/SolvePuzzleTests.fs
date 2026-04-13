module SolvePuzzleTests

open Xunit
open System
open ChessLibrary.PuzzleTypes
open ChessLibrary.PuzzleEngineAgent

/// Create a mock agent that responds to NewGame and SolvePuzzle with a
/// predetermined bestmove + PV string.  All other messages get safe defaults.
let private mkMockSolveAgent (bestmove: string) (pv: string) =
    MailboxProcessor<EngineMsg>.Start(fun inbox ->
        let rec loop () = async {
            let! msg = inbox.Receive()
            match msg with
            | NewGame reply          -> reply.Reply()
            | SolvePuzzle (_, reply) -> reply.Reply(bestmove, pv, ResizeArray())
            | Ok reply               -> reply.Reply(true)
            | Network reply          -> reply.Reply("mock-net")
            | Quit reply             -> reply.Reply()
            | BestMove (_, reply)    -> reply.Reply("", 0.0)
            | BestMoveWithPolicy (_, _, reply) -> reply.Reply("", "")
            | BestMoveWithAllPolicies (_, reply) -> reply.Reply("", [])
            | EvalAllMovesValue (_, reply) -> reply.Reply([])
            | ValueTopNEval (_, reply) -> reply.Reply("", [])
            | BestMoveValueHead (_, reply) -> reply.Reply("")
            return! loop()
        }
        loop()
    )

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let private mkPosition cmd correct =
    { Command = cmd; CorrectMove = correct; MovePlayed = "" }

let private mkPuzzle (commands: Position list) (fen: string) (moves: string) =
    CsvPuzzleData.Create(
        1, fen, moves, 1500.0, 100.0, 90, 1000, "", "", "", "",
        (commands :> Position seq),
        (Seq.empty<string>),
        0)

// ---------------------------------------------------------------------------
// Single-move tests
// ---------------------------------------------------------------------------

/// Position: 6k1/8/6K1/8/8/8/8/RR6 w - - 0 1
/// White: Kg6, Ra1, Rb1.  Black: Kg8.
/// Both a1a8 (Ra8#) and b1b8 (Rb8#) are checkmate.
let private singleMoveFen = "6k1/8/6K1/8/8/8/8/RR6 w - - 0 1"
let private singleMoveCmd = "position fen " + singleMoveFen
// Single-move: Moves = "setup a1a8" (setup is a dummy, solver move is a1a8)
let private singleMoveMoves = "e1e1 a1a8"

[<Fact>]
let ``Solve — exact PV match is correct`` () =
    let puzzle = mkPuzzle [ mkPosition singleMoveCmd "a1a8" ] singleMoveFen singleMoveMoves
    let agent  = mkMockSolveAgent "a1a8" "a1a8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "Exact PV match should be correct")

[<Fact>]
let ``Solve — different mating move accepted as correct`` () =
    // Puzzle expects Ra8# (a1a8) but engine plays Rb8# (b1b8) — also mate.
    let puzzle = mkPuzzle [ mkPosition singleMoveCmd "a1a8" ] singleMoveFen singleMoveMoves
    let agent  = mkMockSolveAgent "b1b8" "b1b8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "A different mating move should be accepted — a mate is a mate")

[<Fact>]
let ``Solve — non-mating wrong move is rejected`` () =
    // Engine plays Ra2 (a1a2) which is NOT mate.
    let puzzle = mkPuzzle [ mkPosition singleMoveCmd "a1a8" ] singleMoveFen singleMoveMoves
    let agent  = mkMockSolveAgent "a1a2" "a1a2"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.False(result.WasCorrect, "A non-mating wrong move must be rejected")
    Assert.Equal("a1a8", result.FailedMove)
    Assert.Equal("a1a2", result.MovePlayed)

// ---------------------------------------------------------------------------
// Multi-move tests
// ---------------------------------------------------------------------------

/// Position: 6k1/8/5K2/8/8/8/8/RR6 w - - 0 1
/// White: Kf6, Ra1, Rb1.  Black: Kg8.
/// Move 1: Kg6 (f6g6), opponent: Kh8 (g8h8).
/// After f6g6 g8h8: 7k/8/6K1/8/8/8/8/RR6 w
/// Both a1a8 and b1b8 are checkmate.
let private multiMoveFen = "6k1/8/5K2/8/8/8/8/RR6 w - - 0 1"
let private multiMoveCmd0 = "position fen " + multiMoveFen
let private multiMoveCmd1 = "position fen " + multiMoveFen + " moves f6g6 g8h8"
// Multi-move puzzle Moves: [setup, solver1, opponent1, solver2]
// Puzzle sequence: setup f6g6 g8h8 a1a8
let private multiMoveMoves = "e1e1 f6g6 g8h8 a1a8"

[<Fact>]
let ``Solve multi-move — last move differs but is mate, accepted`` () =
    // Puzzle expects a1a8 as the mating move, engine PV has b1b8 (also mate).
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"   // our move 1
        mkPosition multiMoveCmd1 "a1a8"   // our move 2 (expected mate)
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV: f6g6 g8h8 b1b8 — first move matches, last move differs but mates
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8h8 b1b8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "Last move that delivers mate should be accepted even if it differs from the puzzle solution")

[<Fact>]
let ``Solve multi-move — exact PV match is correct`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8h8 a1a8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "Exact multi-move PV match should be correct")

[<Fact>]
let ``Solve multi-move — non-mating last move is rejected when opponent matches`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV: f6g6 g8h8 a1a2 — first move matches, opponent matches (g8h8), but our
    // second move (a1a2) is wrong and not mate → puzzle fails.
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8h8 a1a2"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.False(result.WasCorrect, "Wrong move on the same puzzle line must be rejected")
    Assert.Equal("a1a8", result.FailedMove)
    Assert.Equal("a1a2", result.MovePlayed)

[<Fact>]
let ``Solve multi-move — first move wrong fails immediately`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV first move is wrong (a1a2 instead of f6g6)
    let agent  = mkMockSolveAgent "a1a2" "a1a2 g8h8 a1a8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.False(result.WasCorrect, "Wrong first move must fail the puzzle")
    Assert.Equal("f6g6", result.FailedMove)

// ---------------------------------------------------------------------------
// Opponent PV divergence
// ---------------------------------------------------------------------------

[<Fact>]
let ``Solve multi-move — opponent diverges in PV, accepted as solved`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV: f6g6 g8f8 a1a2 — first move correct, opponent plays g8f8 instead
    // of expected g8h8. Engine's follow-up (a1a2) is for the diverged line.
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8f8 a1a2"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "Opponent PV divergence should be accepted — engine found correct first move")

[<Fact>]
let ``Solve multi-move — opponent diverges but engine move coincidentally matches, accepted`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV: f6g6 g8f8 a1a8 — first move correct, opponent diverges (g8f8 vs g8h8),
    // engine's second move (a1a8) happens to match the puzzle's expected move text.
    // But the position is different due to opponent divergence — must still accept.
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8f8 a1a8"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "Opponent divergence should be detected before move comparison — accept as solved")

[<Fact>]
let ``Solve multi-move — stale PV with matching opponent, wrong engine move fails`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV: f6g6 g8h8 a1a2 — first move correct, opponent MATCHES (g8h8),
    // but engine's second move (a1a2) is wrong on the same line → must fail.
    let agent  = mkMockSolveAgent "f6g6" "f6g6 g8h8 a1a2"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.False(result.WasCorrect, "Wrong move on matching puzzle line must fail")

// ---------------------------------------------------------------------------
// PV too short
// ---------------------------------------------------------------------------

[<Fact>]
let ``Solve — PV too short but verified moves correct counts as solved`` () =
    let commands = [
        mkPosition multiMoveCmd0 "f6g6"
        mkPosition multiMoveCmd1 "a1a8"
    ]
    let puzzle = mkPuzzle commands multiMoveFen multiMoveMoves
    // PV only has the first move (correct), missing the second.
    // Engine found the right move — PV truncation shouldn't penalize.
    let agent  = mkMockSolveAgent "f6g6" "f6g6"
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.True(result.WasCorrect, "PV too short but first move correct should count as solved")

[<Fact>]
let ``Solve — empty PV fails`` () =
    let puzzle = mkPuzzle [ mkPosition singleMoveCmd "a1a8" ] singleMoveFen singleMoveMoves
    let agent  = mkMockSolveAgent "" ""
    let result = runSolvePuzzleViaAgent agent puzzle |> Async.RunSynchronously
    Assert.False(result.WasCorrect, "Empty PV should fail")
