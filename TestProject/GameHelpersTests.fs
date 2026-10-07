module GameHelpersTests

open System
open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TimeControlTypes

// ============================================================================
// Helper functions for creating test data
// ============================================================================

let private mkPlyMove moveNumber color san =
    { Ply = 0
      MoveNumber = moveNumber
      Color = color
      San = san
      Comment = ""
      Nags = []
      Variations = ResizeArray() }

let private mkEngine name =
    { EngineConfig.Empty with Name = name }

// ============================================================================
// GameRunner.boardAfterOpening: the board a game starts from
// ============================================================================

open ChessLibrary.Chess
open ChessLibrary.MiscTypes
open Microsoft.Extensions.Logging.Abstractions

let private startPosFen = startPosition // "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let private mkPgnGameWithFenAndMoves gameNr fen (moves: PlyMove list) =
    { PgnGame.Empty gameNr with
        Fen = fen
        GameMetaData = { GameMetadata.Empty with Fen = fen }
        Mainline = ResizeArray<PlyMove>(moves) }

let private mkPairingWithOpening white black opening =
    { White = mkEngine white
      Black = mkEngine black
      Opening = opening
      OpeningHash = ""
      GameNr = 1
      RoundNr = "" }

let private tournamentWithPly ply =
    { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPly = ply } }

let private boardFor (tourny: Tournament) epdBook pair =
    ChessLibrary.GameRunner.boardAfterOpening NullLogger.Instance tourny epdBook pair

[<Fact>]
let ``with an empty FEN the board starts from the start position`` () =
    let tourny = tournamentWithPly 10
    tourny.IsChess960 <- true
    let board = boardFor tourny false (mkPairingWithOpening "White" "Black" (mkPgnGameWithFenAndMoves 1 "" []))
    Assert.Equal(startPosFen, board.StartPosition)
    Assert.Equal(0, board.PlyCount)
    Assert.True(tourny.IsChess960) // left alone without a FEN

[<Fact>]
let ``a FEN opening starts from that position and sets the FRC flag from it`` () =
    let customFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let tourny = tournamentWithPly 10
    tourny.IsChess960 <- true
    let board = boardFor tourny false (mkPairingWithOpening "White" "Black" (mkPgnGameWithFenAndMoves 1 customFen []))
    Assert.Equal(customFen, board.StartPosition)
    Assert.False(tourny.IsChess960) // standard chess

[<Fact>]
let ``a PGN book's opening moves are played`` () =
    let moves = [ mkPlyMove 1 "w" "e4"; mkPlyMove 1 "b" "e5"; mkPlyMove 2 "w" "Nf3" ]
    let board = boardFor (tournamentWithPly 10) false (mkPairingWithOpening "White" "Black" (mkPgnGameWithFenAndMoves 1 "" moves))
    Assert.Equal<string list>([ "e2e4"; "e7e5"; "g1f3" ], List.ofSeq board.UciMovesPlayed)

[<Fact>]
let ``opening moves stop at OpeningsPly`` () =
    let moves = [ mkPlyMove 1 "w" "e4"; mkPlyMove 1 "b" "e5"; mkPlyMove 2 "w" "Nf3"; mkPlyMove 2 "b" "Nc6" ]
    let board = boardFor (tournamentWithPly 2) false (mkPairingWithOpening "White" "Black" (mkPgnGameWithFenAndMoves 1 "" moves))
    Assert.Equal<string list>([ "e2e4"; "e7e5" ], List.ofSeq board.UciMovesPlayed)

[<Fact>]
let ``an EPD book plays no moves`` () =
    let customFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let opening = mkPgnGameWithFenAndMoves 1 customFen [ mkPlyMove 1 "b" "e5" ]
    let board = boardFor (tournamentWithPly 10) true (mkPairingWithOpening "White" "Black" opening)
    Assert.Equal(customFen, board.StartPosition)
    Assert.Equal(0, board.UciMovesPlayed.Count)

[<Fact>]
let ``a PGN book with a FEN plays its moves from the FEN`` () =
    let fenAfterE4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let opening = mkPgnGameWithFenAndMoves 1 fenAfterE4 [ mkPlyMove 1 "b" "e5" ]
    let board = boardFor (tournamentWithPly 10) false (mkPairingWithOpening "White" "Black" opening)
    Assert.Equal(fenAfterE4, board.StartPosition)
    Assert.Equal<string list>([ "e7e5" ], List.ofSeq board.UciMovesPlayed)

// ============================================================================
// Moves-to-go (repeating time control) tests
// ============================================================================

let private mkTc periodW periodB fixedSec incSec : TimeControl =
    let cfg = { Id = 1
                Fixed = TimeSpan.FromSeconds(float fixedSec)
                Increment = TimeSpan.FromSeconds(float incSec)
                NodeLimit = false
                Nodes = 0; MoveTime = TimeSpan.Zero; MovesToGo = 0 }
    { TimeConfigs = [cfg]; WmovesToGo = periodW; BmovesToGo = periodB }

[<Fact>]
let ``MovesToGoPeriod is max of W and B`` () =
    Assert.Equal(40, (mkTc 40 30 300 0).MovesToGoPeriod)
    Assert.Equal(0, (mkTc 0 0 300 0).MovesToGoPeriod)

[<Fact>]
let ``GetTimeForMove counts moves-to-go down within a period`` () =
    let tc = mkTc 20 20 30 0
    let cfg = tc.GetTimeConfig 1
    let mtgOf movesDone =
        match tc.GetTimeForMove cfg movesDone with
        | UnionType.WithMoves(_, _, w, _) -> w
        | other -> failwithf "expected WithMoves, got %A" other
    Assert.Equal(20, mtgOf 0)    // first move of a period
    Assert.Equal(12, mtgOf 8)    // 8 completed -> 12 to go
    Assert.Equal(1, mtgOf 19)    // last move of the period
    Assert.Equal(20, mtgOf 20)   // wraps to a fresh period

[<Fact>]
let ``a setting's own period wins over the tournament's, which is the default`` () =
    let own = { (mkTc 0 0 300 0).GetTimeConfig 1 with Id = 2; MovesToGo = 40 }
    let tc = { mkTc 20 20 30 0 with TimeConfigs = [ (mkTc 0 0 30 0).GetTimeConfig 1; own ] }
    Assert.Equal(20, tc.PeriodFor(tc.GetTimeConfig 1))   // none of its own: the tournament's
    Assert.Equal(40, tc.PeriodFor(tc.GetTimeConfig 2))
    let mtgOf id movesDone =
        match tc.GetTimeForMove (tc.GetTimeConfig id) movesDone with
        | UnionType.WithMoves(_, _, w, _) -> w
        | other -> failwithf "expected WithMoves, got %A" other
    Assert.Equal(12, mtgOf 1 8)
    Assert.Equal(32, mtgOf 2 8)
    Assert.Equal(40, mtgOf 2 40)
    // and in a tournament without one, a setting with a period still has it
    let alone = { mkTc 0 0 30 0 with TimeConfigs = [ own ] }
    Assert.Equal(40, alone.PeriodFor own)
    Assert.Equal("40/5' + 0''", own.ToString())

[<Fact>]
let ``moves-to-go go-command emits a single valid movestogo`` () =
    let tc = mkTc 20 20 30 0
    let cfg = tc.GetTimeConfig 1
    let union = tc.GetTimeForMove cfg 8
    let cmd = TimeControlCommands.uciTimeCommand union (TimeSpan(0, 0, 30)) (TimeSpan(0, 0, 30))
    Assert.Contains("movestogo 12", cmd)
    Assert.DoesNotContain("movestogo 12 12", cmd)   // not the old invalid two-number form

[<Fact>]
let ``no moves-to-go produces a command without movestogo`` () =
    let tc = mkTc 0 0 30 1
    let cfg = tc.GetTimeConfig 1
    let union = tc.GetTimeForMove cfg 5
    let cmd = TimeControlCommands.uciTimeCommand union (TimeSpan(0, 0, 30)) (TimeSpan(0, 0, 30))
    Assert.DoesNotContain("movestogo", cmd)

// ============================================================================
// Long time controls — the ceiling TimeOnly imposed at 24 hours
// ============================================================================

[<Fact>]
let ``a time control past 24 hours survives into the UCI go command`` () =
    // 30h + 30s. The old type could not hold the fixed time at all, and the Ceres bridge
    // capped anything above 23.999h on the way in.
    let cfg = { Id = 1
                Fixed = TimeSpan.FromHours 30.0
                Increment = TimeSpan.FromSeconds 30.0
                NodeLimit = false
                Nodes = 0; MoveTime = TimeSpan.Zero; MovesToGo = 0 }
    let tc = { TimeConfigs = [cfg]; WmovesToGo = 0; BmovesToGo = 0 }
    let union = tc.GetTime cfg
    let cmd = TimeControlCommands.uciTimeCommand union cfg.Fixed cfg.Fixed
    // 30h = 108_000_000 ms, and UCI carries plain milliseconds.
    Assert.Contains("wtime 108000000", cmd)
    Assert.Contains("winc 30000", cmd)
