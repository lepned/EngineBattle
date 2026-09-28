/// Characterisation tests for `type Board` (ChessLibrary/Chess/Board.fs), written against the board
/// as it stood before the rewrite (branch rewrite/board-fs, 2026-09-28). BoardDifferentialTests
/// holds the rewrite to the old board over random operation sequences; these pin, readably, the
/// flows other code is built on - how tournaments, deviation analysis and the GUI drive the board -
/// and the known bugs. A test marked BUG pins what the old board does wrong; the rewrite fixes it and
/// turns the test round in the same commit. QUIRK marks behaviour kept on purpose.
module BoardCharacterizationTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.Chess
open ChessLibrary.BoardUtils
open ChessLibrary.ChessUtilities
open ChessLibrary.RuntimeUtilities

let private start = startPos

/// How a tournament game drives the board (GameSetup + GameExecution): reset, load the start, book
/// moves through PlayOpeningMove, the opening kept aside and the list cleared, then every engine
/// move added to UciMovesPlayed by hand and made with MakeMove - PlayUciMove is not used.
let private tournamentBoard (book: string list) (moves: string list) =
    let board = Board()
    board.ResetBoardState()
    board.LoadFen start
    board.StartPosition <- start
    for san in book do board.PlayOpeningMove san
    board.MovesAndFenPlayed.Clear()
    for uci in moves do
        match tryGetMoveAndSanFromUci &board uci with
        | Some (tmove, _) ->
            let mutable m = tmove
            board.UciMovesPlayed.Add uci
            board.MakeMove &m
        | None -> failwithf "illegal test move %s" uci
    board

[<Fact>]
let ``Tournament: book and engine moves make the position command, the book stays out of the graph`` () =
    let board = tournamentBoard [ "e4"; "e5" ] [ "g1f3"; "b8c6"; "f1b5" ]
    Assert.Equal(sprintf "position fen %s moves e2e4 e7e5 g1f3 b8c6 f1b5" start, board.PositionWithMoves())
    Assert.Equal("r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3", board.FEN())
    Assert.Equal<string list>([ "e4"; "e5" ], List.ofSeq board.OpeningMovesPlayed)
    Assert.Equal(5, board.HashKeys.Count)
    Assert.Empty(board.MoveGraphChildren board.MoveGraphRootId)

[<Fact>]
let ``Tournament: threefold is claimed on the third occurrence, the start position included`` () =
    let shuffle = [ "g1f3"; "g8f6"; "f3g1"; "f6g8" ]
    let twice = tournamentBoard [] shuffle
    Assert.Equal(2, twice.RepetitionNr())
    Assert.False(twice.ClaimThreeFoldRep())
    let thrice = tournamentBoard [] (shuffle @ shuffle)
    Assert.Equal(3, thrice.RepetitionNr())
    Assert.True(thrice.ClaimThreeFoldRep())

[<Fact>]
let ``Deviation analysis: HashKeys, MovesAndFenPlayed and Game are indexed by move from the start`` () =
    // DeviationAnalysis reads HashKeys[i], MovesAndFenPlayed[i] and Game[i].STM for move i of a
    // game replayed with LoadFen + PlaySanMove.
    let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    let board = Board()
    board.LoadFen fen
    let before = ResizeArray<PositionTypes.Position>()
    let hashesAfter = ResizeArray<uint64>()
    let fensAfter = ResizeArray<string>()
    for san in [ "e5"; "Nf3"; "Nc6" ] do
        before.Add board.Position
        board.PlaySanMove san
        hashesAfter.Add(board.PositionHash())
        fensAfter.Add(board.FEN())
    Assert.Equal<uint64 list>(List.ofSeq hashesAfter, List.ofSeq board.HashKeys)
    Assert.Equal<string list>(List.ofSeq fensAfter, board.MovesAndFenPlayed |> Seq.map (fun m -> m.FenAfterMove) |> List.ofSeq)
    for i in 0 .. 2 do
        Assert.Equal(before.[i].STM, board.Game.[i].STM)
        Assert.Equal(before.[i].PM, board.Game.[i].PM)

[<Fact>]
let ``GUI: going back and playing another move makes a variation`` () =
    let board = Board()
    board.ResetBoardState()
    for uci in [ "e2e4"; "e7e5"; "g1f3" ] do board.PlayUciMove uci
    let afterE5 = board.MovesAndFenPlayed.[1].FenAfterMove
    board.LoadFen afterE5
    board.PlayUciMove "b1c3"
    Assert.Equal("1. e4 e5 2. Nf3 (2. Nc3)", board.GetMoveHistoryWithVariations())
    Assert.Equal<string list>([ "e2e4"; "e7e5"; "b1c3" ], List.ofSeq board.UciMovesPlayed)
    Assert.Equal<string list list>([ [ "e4"; "e5"; "Nf3" ]; [ "e4"; "e5"; "Nc3" ] ], board.MoveLinesFromGraph false)
    // The mainline continues with Nf3, not with the variation just played.
    Assert.Equal(Some "Nf3", board.TryGetNextMoveAndFen afterE5 |> Option.map (fun m -> m.ShortSan))

[<Fact>]
let ``QUIRK: TryGetPreviousMoveAndFen moves the graph cursor, not the board`` () =
    // The GUI loads the returned FEN itself.
    let board = Board()
    board.ResetBoardState()
    for uci in [ "e2e4"; "e7e5"; "g1f3" ] do board.PlayUciMove uci
    let fenBefore = board.FEN()
    let prev = board.TryGetPreviousMoveAndFen fenBefore
    Assert.Equal(Some ("Nf3", "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
                 prev |> Option.map (fun m -> m.ShortSan, m.FenAfterMove))
    Assert.Equal(fenBefore, board.FEN())

[<Fact>]
let ``QUIRK: UndoMove rewinds the position only`` () =
    // The puzzle runner and perft pair it with a move they look at and take back; the move lists
    // and hash keys keep the move.
    let board = Board()
    board.ResetBoardState()
    board.PlayUciMove "e2e4"
    board.UndoMove()
    Assert.Equal(start, board.FEN())
    Assert.Equal<string list>([ "e2e4" ], List.ofSeq board.UciMovesPlayed)
    Assert.Equal<string list>([ "e4" ], List.ofSeq board.SanMovesPlayed)
    Assert.Equal(1, board.HashKeys.Count)

[<Fact>]
let ``QUIRK: PositionWithMoves starts from StartPosition, which LoadFen does not set`` () =
    let board = Board()
    board.LoadFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    board.PlaySanMove "e5"
    Assert.Equal(sprintf "position fen %s moves e7e5" start, board.PositionWithMoves())

[<Fact>]
let ``QUIRK: PlayUciMove after PlayOpeningMove rebuilds the move lists from the graph, without the book`` () =
    // No caller mixes the two (tournaments use MakeMove after the book), but it is what happens.
    let board = Board()
    board.ResetBoardState()
    board.LoadFen start
    board.StartPosition <- start
    board.PlayOpeningMove "e4"
    board.PlayOpeningMove "e5"
    board.PlayUciMove "g1f3"
    Assert.Equal<string list>([ "g1f3" ], List.ofSeq board.UciMovesPlayed)
    Assert.Equal<string list>([ "Nf3" ], board.MovesAndFenPlayed |> Seq.map (fun m -> m.ShortSan) |> List.ofSeq)
    Assert.Equal(sprintf "position fen %s moves g1f3" start, board.PositionWithMoves())
    Assert.Equal(3, board.HashKeys.Count)

// ── Bugs the rewrite fixed (pinned as they were in ea09382, turned round with the fix) ─────────

[<Fact>]
let ``Plies across FEN reloads no longer overflow the history`` () =
    // Game Review rebuilds a reviewed game with LoadPGNGameWithVariations, which reloads a FEN
    // before every variation; once mainline and variations passed 1,000 plies together the old
    // board threw IndexOutOfRangeException. The history now grows.
    let board = Board()
    board.ResetBoardState()
    for _ in 1 .. 1500 do
        board.LoadFen start
        board.PlayUciMove "e2e4"
    Assert.Equal("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", board.FEN())

[<Fact>]
let ``A FEN with a high move number loads`` () =
    let board = Board()
    board.LoadFen "8/8/8/4k3/8/8/4K3/7R w - - 0 600"
    Assert.Equal("8/8/8/4k3/8/8/4K3/7R w - - 0 600", board.FEN())
    board.PlayUciMove "h1h5"
    Assert.Equal("8/8/8/4k2R/8/8/4K3/8 b - - 1 600", board.FEN())

[<Fact>]
let ``A taken-back line no longer counts towards threefold`` () =
    // Play vs Engine takes a move back by loading the earlier FEN; the old board kept counting
    // the positions of the line taken back (3 here, and a draw claimed on a twofold).
    let board = Board()
    board.ResetBoardState()
    for uci in [ "g1f3"; "g8f6" ] do board.PlayUciMove uci
    board.LoadFen start
    for uci in [ "g1f3"; "g8f6"; "f3g1"; "f6g8"; "g1f3"; "g8f6" ] do board.PlayUciMove uci
    Assert.Equal(2, board.RepetitionNr())
    Assert.False(board.ClaimThreeFoldRep())
    // Played on to the third occurrence, it is claimed.
    for uci in [ "f3g1"; "f6g8"; "g1f3"; "g8f6" ] do board.PlayUciMove uci
    Assert.Equal(3, board.RepetitionNr())
    Assert.True(board.ClaimThreeFoldRep())

[<Fact>]
let ``The hash keys follow the line when the GUI steps back and forth`` () =
    // The GUI's back button: TryGetPreviousMoveAndFen, then LoadFen of the position it returns.
    let board = Board()
    board.ResetBoardState()
    for uci in [ "e2e4"; "e7e5"; "g1f3"; "b8c6" ] do board.PlayUciMove uci
    let hashesOfLine () = board.MovesAndFenPlayed |> Seq.map (fun m -> Hash.hashBoard (BoardHelper.getPosFromFen (Some m.FenAfterMove))) |> List.ofSeq
    for _ in 1 .. 2 do
        match board.TryGetPreviousMoveAndFen(board.FEN()) with
        | Some m -> board.LoadFen m.FenAfterMove
        | None -> ()
    Assert.Equal(2, board.HashKeys.Count)
    Assert.Equal<uint64 list>(hashesOfLine (), List.ofSeq board.HashKeys)
    match board.TryGetNextMoveAndFen(board.FEN()) with
    | Some m -> board.LoadFen m.FenAfterMove
    | None -> ()
    Assert.Equal(3, board.HashKeys.Count)
    Assert.Equal<uint64 list>(hashesOfLine (), List.ofSeq board.HashKeys)
    // A new move from here is a variation, and the keys follow it.
    board.PlayUciMove "f8c5"
    Assert.Equal(4, board.HashKeys.Count)
    Assert.Equal<uint64 list>(hashesOfLine (), List.ofSeq board.HashKeys)

[<Fact>]
let ``Hash keys added outside the graph are left alone`` () =
    // A tournament's moves (MakeMove) and a puzzle probe (UndoMove) are not in the graph; loading
    // a position afterwards does not touch the keys, as before the rewrite.
    let board = tournamentBoard [ "e4"; "e5" ] [ "g1f3"; "b8c6" ]
    let keys = List.ofSeq board.HashKeys
    board.LoadFen start
    Assert.Equal<uint64 list>(keys, List.ofSeq board.HashKeys)
