/// The Board rewrite held to the board it replaces. TestProject/Legacy/LegacyBoard.fs is a frozen
/// copy of `type Board` from before the rewrite (module ChessLibrary.LegacyChess); both boards run
/// the same random sequences of operations - moves in UCI, SAN and coordinate form, FEN loads that
/// navigate back as the GUI does, variations, promotions and removals, PGN and history loads,
/// takebacks, opening moves, position commands - and after every step everything the public API
/// can observe must be the same: FEN, position, the move lists, hash keys, the position history,
/// the move graph as text, repetition, legal moves and the return value or exception of the step.
///
/// Both boards are driven through reflection by member name, so they are treated identically and
/// the harness does not care which one is which. Where the rewrite changes behaviour on purpose,
/// `expectedDivergence` below says so, and a targeted test in BoardCharacterizationTests pins the
/// new behaviour. The legacy copy leaves the repo once the rewrite is merged and verified.
module BoardDifferentialTests

open System
open System.IO
open System.Reflection
open System.Text
open System.Collections
open Microsoft.FSharp.Reflection
open Xunit
open ChessLibrary
open ChessLibrary.PositionTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.RuntimeUtilities

// ── Driving a board by member name ──────────────────────────────────────────────────────────────

let private flags = BindingFlags.Public ||| BindingFlags.Instance

let private call (board: obj) (name: string) (args: obj[]) =
    let t = board.GetType()
    let m =
        t.GetMethods(flags)
        |> Array.find (fun m -> m.Name = name && m.GetParameters().Length = args.Length)
    try Ok (m.Invoke(board, args))
    with :? TargetInvocationException as ex -> Error (ex.InnerException.GetType().Name)

let private get (board: obj) (name: string) =
    try Ok (board.GetType().GetProperty(name, flags).GetValue(board))
    with :? TargetInvocationException as ex -> Error (ex.InnerException.GetType().Name)

let private some (x: 'a) = box (Some x)

// ── Turning what a board shows into text ────────────────────────────────────────────────────────

let private describePosition (p: Position) =
    // Interpolation, not sprintf: this runs for 400 history entries per snapshot.
    $"{p.PM:x}/{p.P0:x}/{p.P1:x}/{p.P2:x} c{p.CastleFlags} e{p.EnPassant} h{p.Count50} r{p.Rep} s{p.STM} p{p.Ply} rook{p.RookInfo.WhiteKRInitPlacement}.{p.RookInfo.WhiteQRInitPlacement}.{p.RookInfo.BlackKRInitPlacement}.{p.RookInfo.BlackQRInitPlacement}"

let rec private describe (v: obj) : string =
    match v with
    | null -> "null"
    | :? string as s -> "\"" + s + "\""
    | :? Position as p -> describePosition p
    | :? IEnumerable as xs ->
        let sb = StringBuilder("[")
        for x in xs do sb.Append(describe x).Append("; ") |> ignore
        sb.Append("]").ToString()
    | _ when FSharpType.IsUnion(v.GetType()) || FSharpType.IsRecord(v.GetType()) || FSharpType.IsTuple(v.GetType()) ->
        // Fields one by one, so nothing is cut short the way %A cuts long collections.
        let t = v.GetType()
        if FSharpType.IsRecord t then
            FSharpType.GetRecordFields t
            |> Array.map (fun f -> f.Name + "=" + describe (f.GetValue v))
            |> String.concat ", " |> sprintf "{%s}"
        elif FSharpType.IsTuple t then
            FSharpValue.GetTupleFields v |> Array.map describe |> String.concat ", " |> sprintf "(%s)"
        else
            let case, fields = FSharpValue.GetUnionFields(v, t)
            if fields.Length = 0 then case.Name
            else sprintf "%s(%s)" case.Name (fields |> Array.map describe |> String.concat ", ")
    | _ -> sprintf "%A" v

let private describeResult = function
    | Ok v -> describe v
    | Error e -> "EXCEPTION " + e

/// The move graph as the public graph members show it: from the root down, each node's children
/// in their order, with every edge field (order, mainline flag, comments, the node reached). The
/// positions the edges reach are in the InlineTokensFromGraph view.
let private describeGraph (board: obj) =
    let sb = StringBuilder()
    let root = match get board "MoveGraphRootId" with Ok r -> unbox<int> r | _ -> -1
    let rec walk (node: int) depth =
        if depth < 400 then
            match call board "MoveGraphChildren" [| box node |] with
            | Ok (:? IEnumerable as children) ->
                for e in children |> Seq.cast<GameGraphTypes.MoveEdge> do
                    sb.Append(describe (box e)).AppendLine() |> ignore
                    walk e.To (depth + 1)
            | other -> sb.Append(describeResult other) |> ignore
    walk root 0
    sb.ToString()

/// Everything the public API shows, one entry per view. Views that throw say so.
let private snapshot (board: obj) =
    let q name = name, describeResult (call board name [||])
    let p name = name, describeResult (get board name)
    let game =
        match get board "Game" with
        | Ok (:? (Position[]) as g) ->
            // The whole history array would be 1,000 mostly-default positions; the first 400 hold
            // every position a sequence here reaches.
            g |> Array.truncate 400 |> Array.map describePosition |> String.concat "|"
        | other -> describeResult other
    [ q "FEN"; p "Position"; p "PlyCount"; p "StartPosition"; p "CurrentFEN"; p "IsFRC"
      p "MovesAndFenPlayed"; p "UciMovesPlayed"; p "SanMovesPlayed"; p "OpeningMovesPlayed"
      p "HashKeys"; "Game", game
      q "PositionWithMoves"; q "GetMoveHistory"; q "GetSanMoveHistory"
      q "GetMoveHistoryWithVariations"; q "InlineTokensFromGraph"; "Graph", describeGraph board
      "MoveLinesFromGraph(true)", describeResult (call board "MoveLinesFromGraph" [| box true |])
      "MoveLinesFromGraph(false)", describeResult (call board "MoveLinesFromGraph" [| box false |])
      p "MoveGraphRootId"; q "GetCurrentEdgeComment"
      q "RepetitionNr"; q "ClaimThreeFoldRep"; q "InsufficientMaterial"; q "IsMate"; q "AnyLegalMove"
      q "GetLegalMoves"; q "MoveNumber"; q "NextMoveNumber"; q "PositionHash"; q "DeviationHash" ]

// ── Positions to start from ─────────────────────────────────────────────────────────────────────

let private fenPool =
    [| "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
       "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
       "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"          // en passant
       "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"                     // castling both ways
       "8/P6k/8/8/8/8/8/4K3 w - - 0 1"                                           // promotion
       "8/8/8/4k3/8/8/4K3/8 w - - 0 1"                                           // dead position
       "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"          // checkmated
       "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"                                          // stalemate
       "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"      // Chess960
       "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3"                // 4-field EPD
       "r1bq1rk1/ppp2ppp/2np1n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQ1RK1 w - - 0 7" |]
// Not in the pool: a FEN with a high move number, which the old board could not load
// (BoardCharacterizationTests covers it) - here it would only end sequences early.

let private pgnPool =
    lazy (
        let file name = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "TestData", name))
        let fromFile name = FullPGNParser.parsePgnString (file name) |> Seq.truncate 6 |> Seq.toArray
        Array.concat
            [ fromFile "lichess_shapes_study.pgn"
              fromFile "SF_vs_Ceres_and_Lc0.pgn"
              [| FullPGNParser.parseFullPgnGame (file "lichess_pgn_LeelaPieceOddsFRC.pgn") |] ])

// ── Random operations ───────────────────────────────────────────────────────────────────────────

/// One step: its description and what to do to a board. The step is chosen from the REFERENCE
/// board's state (its legal moves, its graph), so both boards get exactly the same call.
type private Step = { Name: string; Run: obj -> Result<obj, string> }

/// Every move of the graph as (SAN, FEN it leads to), from the inline token stream.
let private graphEdges (board: obj) =
    match call board "InlineTokensFromGraph" [||] with
    | Ok (:? IEnumerable as tokens) ->
        tokens
        |> Seq.cast<GameGraphTypes.InlineMoveToken>
        |> Seq.filter (fun t -> not t.IsBracket)
        |> Seq.map (fun t -> t.Text, t.Fen)
        |> Seq.toArray
    | _ -> [||]

let private legalMoves (board: obj) =
    match call board "GetLegalMoves" [||] with
    | Ok (:? IEnumerable as xs) -> xs |> Seq.cast<string * string> |> Seq.toArray
    | _ -> [||]

let private nextStep (rnd: Random) (reference: obj) (visited: ResizeArray<string>) : Step =
    let pick (xs: 'a[]) = xs.[rnd.Next xs.Length]
    let legal = legalMoves reference
    let fenNow = match call reference "FEN" [||] with Ok f -> string f | _ -> ""
    let step name run = { Name = name; Run = run }
    let roll = rnd.Next 100
    if roll < 34 && legal.Length > 0 then
        let uci, _ = pick legal
        step (sprintf "PlayUciMove %s" uci) (fun b -> call b "PlayUciMove" [| box uci |])
    elif roll < 42 && legal.Length > 0 then
        let _, san = pick legal
        step (sprintf "PlaySanMove %s" san) (fun b -> call b "PlaySanMove" [| box san |])
    elif roll < 45 && legal.Length > 0 then
        let uci, _ = pick legal
        step (sprintf "PlaySanMove %s (coordinates)" uci) (fun b -> call b "PlaySanMove" [| box uci |])
    elif roll < 48 && legal.Length > 0 then
        let _, san = pick legal
        let comment = pick [| "good"; "[%eval 0.31]"; "?!" |]
        step (sprintf "PlaySanMoveWithComments %s {%s}" san comment) (fun b -> call b "PlaySanMoveWithComments" [| box san; box comment |])
    elif roll < 49 then
        step "PlayUciMove a1a1 (illegal)" (fun b -> call b "PlayUciMove" [| box "a1a1" |])
    elif roll < 58 && visited.Count > 0 then
        // Navigating back (or forth) to a position seen before, as the GUI does.
        let fen = pick (visited.ToArray())
        step (sprintf "LoadFen %s (visited)" fen) (fun b -> call b "LoadFen" [| some fen |])
    elif roll < 61 then
        let fen = pick fenPool
        step (sprintf "LoadFen %s" fen) (fun b -> call b "LoadFen" [| some fen |])
    elif roll < 63 then
        step "ResetBoardState" (fun b -> call b "ResetBoardState" [||])
    elif roll < 66 then
        let fen = pick fenPool
        step (sprintf "ResetBoardStateFromFen %s" fen) (fun b -> call b "ResetBoardStateFromFen" [| box fen |])
    elif roll < 70 then
        let arg = if rnd.Next 2 = 0 then fenNow else ""
        step (sprintf "TryGetPreviousMoveAndFen '%s'" arg) (fun b -> call b "TryGetPreviousMoveAndFen" [| box arg |])
    elif roll < 74 then
        let arg = if rnd.Next 2 = 0 then fenNow else ""
        step (sprintf "TryGetNextMoveAndFen '%s'" arg) (fun b -> call b "TryGetNextMoveAndFen" [| box arg |])
    elif roll < 81 then
        let edges = graphEdges reference
        if edges.Length = 0 then step "EndCurrentVariation" (fun b -> call b "EndCurrentVariation" [||])
        else
            let san, fen = pick edges
            let san = if rnd.Next 5 = 0 then "" else san
            match rnd.Next 4 with
            | 0 -> step (sprintf "RemoveVariationNode %s %s entire" san fen) (fun b -> call b "RemoveVariationNode" [| box san; box fen; box true |])
            | 1 -> step (sprintf "RemoveVariationNode %s %s" san fen) (fun b -> call b "RemoveVariationNode" [| box san; box fen; box false |])
            | 2 -> step (sprintf "RemoveVariationTail %s %s" san fen) (fun b -> call b "RemoveVariationTail" [| box san; box fen |])
            | _ -> step (sprintf "PromoteVariationToMainline %s %s" san fen) (fun b -> call b "PromoteVariationToMainline" [| box san; box fen |])
    elif roll < 83 then
        let comment = pick [| "note"; "[%clk 0:01:00]"; "" |]
        step (sprintf "SetCommentOnCurrentEdge '%s'" comment) (fun b -> call b "SetCommentOnCurrentEdge" [| box comment |])
    elif roll < 84 then
        step "EndCurrentVariation" (fun b -> call b "EndCurrentVariation" [||])
    elif roll < 86 && legal.Length > 0 then
        // The puzzle runner's probe: play a move, look, take it back.
        let uci, _ = pick legal
        step (sprintf "PlayUciMove %s then UndoMove" uci) (fun b ->
            call b "PlayUciMove" [| box uci |] |> ignore
            call b "UndoMove" [||])
    elif roll < 88 && legal.Length > 0 then
        let _, san = pick legal
        step (sprintf "PlayOpeningMove %s" san) (fun b -> call b "PlayOpeningMove" [| box san |])
    elif roll < 90 then
        let fen = pick fenPool
        step (sprintf "PlayCommands from %s" fen) (fun b -> call b "PlayCommands" [| box (sprintf "position fen %s" fen) |])
    elif roll < 91 && legal.Length > 0 then
        let uci, _ = pick legal
        step (sprintf "PlayFenWithMoves %s %s" fenNow uci) (fun b -> call b "PlayFenWithMoves" [| box (sprintf "%s moves %s" fenNow uci) |])
    elif roll < 92 && legal.Length > 0 then
        let uci, _ = pick legal
        step (sprintf "PlayPVLine %s from %s" uci fenNow) (fun b -> call b "PlayPVLine" [| box (Seq.singleton uci); box fenNow |])
    elif roll < 94 then
        let history = match call reference "GetMoveHistoryWithVariations" [||] with Ok h -> string h | _ -> ""
        step (sprintf "LoadMoveHistoryWithVariations %s" history) (fun b -> call b "LoadMoveHistoryWithVariations" [| box history |])
    elif roll < 96 then
        let games = pgnPool.Force()
        let i = rnd.Next games.Length
        step (sprintf "LoadPGNGameWithVariations pgn #%d" i) (fun b -> call b "LoadPGNGameWithVariations" [| box games.[i] |])
    elif roll < 97 then
        step "PositionWithMovesFromGraph" (fun b -> call b "PositionWithMovesFromGraph" [||])
    elif roll < 98 && visited.Count > 0 then
        let fen = pick (visited.ToArray())
        step (sprintf "GetMoveHistoryToCurrentFen %s" fen) (fun b -> call b "GetMoveHistoryToCurrentFen" [| box fen |])
    else
        // No bare UndoMove: at the start it breaks the old board's history (see `run`), which
        // ended a fifth of the sequences early; the probe above covers UndoMove.
        step "EndCurrentVariation" (fun b -> call b "EndCurrentVariation" [||])

// ── Deliberate differences ──────────────────────────────────────────────────────────────────────

/// A step the reference board threw on is logged with this mark.
let private threw = " [threw]"

/// Steps that start the game afresh: hash keys and the repetition start are reset on both boards.
/// Not when the step threw - a reset can fail halfway, the old repetition start still in place.
let private resetsGame (step: string) =
    not (step.EndsWith(threw, StringComparison.Ordinal))
    && ([ "ResetBoardState"; "PlayCommands"; "PlayFenWithMoves"; "PlayPVLine"; "LoadPGNGameWithVariations"; "LoadMoveHistoryWithVariations" ]
        |> List.exists (fun p -> step.StartsWith(p, StringComparison.Ordinal)))

/// Steps that move the graph cursor to another position (the two loaders do it for every
/// variation).
let private movesCursor (step: string) =
    [ "LoadFen"; "TryGetPreviousMoveAndFen"; "TryGetNextMoveAndFen"; "RemoveVariation"
      "LoadPGNGameWithVariations"; "LoadMoveHistoryWithVariations" ]
    |> List.exists (fun p -> step.StartsWith(p, StringComparison.Ordinal))

/// Views the rewrite is allowed to show differently after a given step history, and why.
///
/// The hash keys and what is counted from them. The old board kept the hash of every position
/// ever reached, so a line taken back (the GUI loads the earlier position) still counted towards
/// threefold. The new board's hash keys follow the line to the cursor when the cursor moves, as
/// MovesAndFenPlayed always did - so after a cursor move since the game last started afresh they
/// differ, by design. BoardCharacterizationTests pins the new behaviour.
let private expectedDivergence (history: string list) (view: string) =
    (view = "HashKeys" || view = "RepetitionNr" || view = "ClaimThreeFoldRep")
    && (// From the last fresh start on (a loader is both: it starts afresh, then moves the cursor).
        let sinceStart =
            match List.tryFindIndexBack resetsGame history with
            | Some i -> List.skip i history
            | None -> history
        sinceStart |> List.exists movesCursor)

// ── The comparison ──────────────────────────────────────────────────────────────────────────────

let private run (makeReference: unit -> obj) (makeCandidate: unit -> obj) (seed: int) (steps: int) =
    let rnd = Random seed
    let reference = makeReference ()
    let candidate = makeCandidate ()
    let visited = ResizeArray<string>()
    let history = ResizeArray<string>()
    let compare (stepName: string) (r1: string) (r2: string) =
        let fail view a b =
            let log = history |> Seq.mapi (fun i h -> sprintf "  %3d %s" i h) |> String.concat "\n"
            Assert.Fail(sprintf "seed %d, step %d (%s): %s differs\nreference: %s\ncandidate: %s\nsteps:\n%s"
                            seed (history.Count - 1) stepName view a b log)
        if r1 <> r2 && not (expectedDivergence (List.ofSeq history) "result") then fail "the step's result" r1 r2
        let s1 = snapshot reference
        let s2 = snapshot candidate
        for (view, a), (_, b) in List.zip s1 s2 do
            if a <> b && not (expectedDivergence (List.ofSeq history) view) then fail view a b
    compare "start" "" ""
    let mutable stopped = false
    for _ in 1 .. steps do
      if not stopped then
        let step = nextStep rnd reference visited
        history.Add step.Name
        let r1 = describeResult (step.Run reference)
        let r2 = describeResult (step.Run candidate)
        if r1.StartsWith("EXCEPTION", StringComparison.Ordinal) then history.[history.Count - 1] <- step.Name + threw
        // An IndexOutOfRangeException from the old board is its position history broken: the
        // fixed 1,000 entries overflowed (a FEN with a high move number, a long game with
        // variations - the new board grows instead), or an UndoMove went past the start, after
        // which every move throws on both boards, each leaving different half-made updates.
        // Nothing after it is worth comparing, so the sequence ends there.
        if r1 = "EXCEPTION IndexOutOfRangeException" then stopped <- true
        else compare step.Name r1 r2
        match call reference "FEN" [||] with
        | Ok f -> visited.Add(string f)
        | _ -> ()

let private legacy () = box (ChessLibrary.LegacyChess.Board())
let private current () = box (ChessLibrary.Chess.Board())

/// 15 sequences in a normal test run (a few seconds); set BOARD_DIFF_SEEDS for a long run, as the
/// rewrite does before each commit.
let private seeds =
    match Environment.GetEnvironmentVariable "BOARD_DIFF_SEEDS" with
    | null | "" -> 15
    | n -> int n

[<Fact>]
let ``The board shows what the legacy board shows over random operation sequences`` () =
    for seed in 1 .. seeds do
        run legacy current seed 100

/// The harness itself: two legacy boards must agree, and a board that is driven differently must
/// not. If this fails, the comparison above proves nothing.
[<Fact>]
let ``The differential harness sees a difference when there is one`` () =
    run legacy legacy 7 60
    let differs =
        try
            // Same seed, but the candidate gets an extra move before the run starts.
            run legacy (fun () -> let b = legacy () in call b "PlayUciMove" [| box "e2e4" |] |> ignore; b) 7 5
            false
        with :? Xunit.Sdk.FailException -> true
    Assert.True(differs, "a board one move ahead was reported as equal")
