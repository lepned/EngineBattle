/// Regression tests for the puzzle-pipeline review of 2026-09-04: the shared Type parser,
/// puzzle-id normalisation, ScoreAllPositions on the value half of policyvalue, and the
/// culture-independence of the JSON timestamp.
module PuzzlePipelineFixesTests

open System
open System.IO
open System.Globalization
open Xunit
open ChessLibrary
open ChessLibrary.Configuration
open ChessLibrary.PuzzleTypes
open ChessLibrary.PuzzleEngineAgent
open ChessLibrary.PuzzleRunners

// ---------------------------------------------------------------------------
// parseSubTests - one parser for the console and the GUI
// ---------------------------------------------------------------------------

[<Fact>]
let ``parseSubTests reads every valid token and expands search and solve over the node list`` () =
    match parseSubTests "Policy, policy3, PolicyTop5, value, policyvalue, dual, search, solve" [| 10; 100 |] with
    | Result.Ok l ->
        Assert.Equal<SubTest list>(
            [ Policy; PolicyTopN 3; PolicyTopN 5; Value; PolicyValue; PolicyValue
              Search 10; Search 100; Solve 10; Solve 100 ], l)
    | Result.Error e -> failwithf "unexpected error %A" e

[<Fact>]
let ``parseSubTests refuses the whole spec when one token is unknown`` () =
    // value3 has never been a test (no engine ranks moves by value head), but the parser
    // once accepted the token; running the policy half alone under that label is exactly
    // what must not happen.
    match parseSubTests "policy, value3" [||] with
    | Result.Error unknown -> Assert.Equal<string list>([ "value3" ], unknown)
    | Result.Ok l -> failwithf "value3 must not parse; got %A" l

[<Fact>]
let ``parseSubTests lists every unknown token and treats a bad policy suffix as unknown`` () =
    match parseSubTests "polcy, policy0, valuetop3" [||] with
    | Result.Error unknown -> Assert.Equal<string list>([ "polcy"; "policy0"; "valuetop3" ], unknown)
    | Result.Ok l -> failwithf "expected an error; got %A" l

[<Fact>]
let ``parseSubTests returns an empty list for an empty spec so the caller picks the default`` () =
    for spec in [ ""; "  ,  "; null ] do
        match parseSubTests spec [||] with
        | Result.Ok l -> Assert.Empty l
        | Result.Error e -> failwithf "empty spec %A must not error: %A" spec e

[<Fact>]
let ``unknownSubTestsMessage names the tokens and the valid set`` () =
    let msg = unknownSubTestsMessage [ "value3" ]
    Assert.StartsWith("Unknown puzzle type:", msg)
    Assert.Contains("'value3'", msg)
    Assert.Contains("policy<N>", msg)
    Assert.StartsWith("Unknown puzzle types:", unknownSubTestsMessage [ "a"; "b" ])

// ---------------------------------------------------------------------------
// Puzzle ids - non-blank and unique, or theme credit and McNemar sets collapse
// ---------------------------------------------------------------------------

[<Fact>]
let ``parsePuzzle makes blank and repeated ids unique and stable across loads`` () =
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_ids_%s.csv" (Guid.NewGuid().ToString "N"))
    let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    let line id = sprintf "%s,%s,f1b5 a7a6,1500,80,90,1000,opening short,https://lichess.org/x,Ruy" id fen
    File.WriteAllLines(
        path,
        [| "PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags"
           line "abc"; line ""; line "abc"; line "  "; line "xyz" |])
    try
        let ids = JSONParser.parsePuzzle path false |> Array.map (fun p -> p.PuzzleId)
        // row<N> is the line number in the FILE (header is line 1), so it survives a re-run
        Assert.Equal<string[]>([| "abc"; "row3"; "abc#2"; "row5"; "xyz" |], ids)
        Assert.Equal(ids.Length, (ids |> Array.distinct).Length)
        let again = JSONParser.parsePuzzle path false |> Array.map (fun p -> p.PuzzleId)
        Assert.Equal<string[]>(ids, again)
    finally
        File.Delete path

[<Fact>]
let ``ensureUniquePuzzleIds leaves a well-formed file untouched`` () =
    let mk id =
        CsvPuzzleData.Create(id, "8/8/8/8/8/8/8/K6k w - - 0 1", "", 1000.0, 50.0, 90, 10, "", "", "", "", Seq.empty, Seq.empty, 0)
    let records = [| mk "a"; mk "b"; mk "c" |]
    let out = JSONParser.ensureUniquePuzzleIds records
    Assert.Equal<string[]>([| "a"; "b"; "c" |], out |> Array.map (fun p -> p.PuzzleId))

// ---------------------------------------------------------------------------
// ScoreAllPositions on the value half of policyvalue
// ---------------------------------------------------------------------------

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let private mkPos (moves: string) (correct: string) : Position =
    { Command = sprintf "position fen %s moves %s" startFen moves; CorrectMove = correct; MovePlayed = "" }

/// Answers the correct move everywhere except the position whose command ends with
/// `failAt`, where it plays `wrongMove` (legal, not mate). Counts value-head queries.
let private mkValueAgent (failAt: string) (wrongMove: string) (queries: int ref) =
    MailboxProcessor<EngineMsg>.Start(fun inbox ->
        let rec loop () = async {
            let! msg = inbox.Receive()
            match msg with
            | NewGame reply -> reply.Reply()
            | BestMoveValueHead (cmd, reply) ->
                queries.Value <- queries.Value + 1
                reply.Reply(if cmd.Command.EndsWith failAt then wrongMove else cmd.CorrectMove)
            | Ok reply -> reply.Reply(true)
            | Network reply -> reply.Reply("mock")
            | Quit reply -> reply.Reply()
            | SolvePuzzle (_, reply) -> reply.Reply("", "", ResizeArray())
            | BestMoveWithPolicy (_, _, reply) -> reply.Reply("", "")
            | BestMoveWithAllPolicies (_, reply) -> reply.Reply("", [])
            return! loop ()
        }
        loop ())

/// Three positions; the mock fails the SECOND one.
let private threeMovePuzzle () =
    CsvPuzzleData.Create(
        "p1", startFen, "", 1500.0, 80.0, 90, 100, "opening", "", "", "",
        ([ mkPos "e2e4 e7e5" "g1f3"
           mkPos "e2e4 e7e5 g1f3 b8c6" "f1b5"
           mkPos "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6" "b5a4" ] :> Position seq),
        Seq.empty, 0)

[<Fact>]
let ``value-head runner scores every position when ScoreAllPositions is on`` () =
    let queries = ref 0
    let agent = mkValueAgent "b8c6" "f1c4" queries
    let result = runPuzzleViaAgentValueHead agent true (threeMovePuzzle ()) |> Async.RunSynchronously
    Assert.False result.WasCorrect
    Assert.Equal("f1b5", result.FailedMove)
    Assert.Equal("f1c4", result.MovePlayed)
    Assert.Equal(3, result.PositionsScored)
    Assert.Equal(2, result.PositionsCorrect)
    Assert.Equal(1, result.FirstMoveCorrect)
    Assert.Equal(1, result.FirstMoveScored)
    Assert.Equal(3, queries.Value)

[<Fact>]
let ``value-head runner stops at the first mistake and reports no positions without the flag`` () =
    let queries = ref 0
    let agent = mkValueAgent "b8c6" "f1c4" queries
    let result = runPuzzleViaAgentValueHead agent false (threeMovePuzzle ()) |> Async.RunSynchronously
    Assert.False result.WasCorrect
    Assert.Equal("f1b5", result.FailedMove)
    // 0 means "not measured", which is what the schema promises when the flag is off
    Assert.Equal(0, result.PositionsScored)
    Assert.Equal(0, result.PositionsCorrect)
    Assert.Equal(1, result.FirstMoveCorrect)
    Assert.Equal(2, queries.Value)

[<Fact>]
let ``value-head runner with the flag agrees with runPuzzleViaAgentEx on a fully solved puzzle`` () =
    let queries = ref 0
    let agent = mkValueAgent "never" "0000" queries
    let result = runPuzzleViaAgentValueHead agent true (threeMovePuzzle ()) |> Async.RunSynchronously
    Assert.True result.WasCorrect
    Assert.Equal(3, result.PositionsScored)
    Assert.Equal(3, result.PositionsCorrect)
    Assert.Equal("b5a4", result.MovePlayed)

// ---------------------------------------------------------------------------
// startedUtc is ISO-8601 whatever the host culture
// ---------------------------------------------------------------------------

[<Fact>]
let ``startedUtc keeps its colons under a culture whose time separator is a dot`` () =
    let culture = CultureInfo "fi-FI"
    // If the culture data on this machine does not use '.', the test would pass for the
    // wrong reason - say so instead.
    Assert.Equal(".", culture.DateTimeFormat.TimeSeparator)
    let saved = CultureInfo.CurrentCulture
    try
        CultureInfo.CurrentCulture <- culture
        let started = DateTime(2026, 9, 4, 12, 30, 45, 123, DateTimeKind.Utc)
        let result = PuzzleJsonOutput.buildResult "p.csv" 1 1 0 3500 "" "" started 1.0 []
        Assert.Equal("2026-09-04T12:30:45.123Z", result.StartedUtc)
    finally
        CultureInfo.CurrentCulture <- saved

// ---------------------------------------------------------------------------
// Dead agents (2026-09-10): an engine that cannot start must surface as Ok=false and an
// agentDeath reason within seconds, not as an agent that never answers.
// ---------------------------------------------------------------------------

[<Fact>]
let ``a policy agent whose engine cannot start answers Ok=false instead of hanging`` () =
    let cfg = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = "ghost"; Path = @"C:\definitely\not\here\ghost.exe"; Protocol = "UCI" }
    let agent = startPolicyEngineAgent cfg 1
    let ok = agent.PostAndAsyncReply((fun ch -> EngineMsg.Ok ch), timeout = 30000) |> Async.RunSynchronously
    Assert.False ok
    Assert.True((agentDeath agent).IsSome)
    agent.PostAndAsyncReply((fun ch -> EngineMsg.Quit ch), timeout = 30000) |> Async.RunSynchronously

[<Fact>]
let ``a value agent whose engine cannot start answers Ok=false instead of hanging`` () =
    let cfg = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = "ghost"; Path = @"C:\definitely\not\here\ceres.exe"; Protocol = "UCI" }
    let agent = startValueEngineAgent cfg
    let ok = agent.PostAndAsyncReply((fun ch -> EngineMsg.Ok ch), timeout = 30000) |> Async.RunSynchronously
    Assert.False ok
    Assert.True((agentDeath agent).IsSome)
    agent.PostAndAsyncReply((fun ch -> EngineMsg.Quit ch), timeout = 30000) |> Async.RunSynchronously
