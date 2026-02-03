module GameHelpersTests

open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameSetup
open ChessLibrary.GameReplay

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

let private mkPgnGameWithHash gameNr hash =
    { PgnGame.Empty gameNr with
        GameMetaData = { GameMetadata.Empty with OpeningHash = hash } }

let private mkPairing roundNr openingGameNr =
    let opening = PgnGame.Empty openingGameNr
    { White = mkEngine "White"
      Black = mkEngine "Black"
      Opening = opening
      OpeningHash = ""
      GameNr = 1
      RoundNr = roundNr }

// ============================================================================
// formatOpeningMoves tests
// ============================================================================

[<Fact>]
let ``formatOpeningMoves with empty sequence returns empty string`` () =
    let result = formatOpeningMoves Seq.empty
    Assert.Equal("", result)

[<Fact>]
let ``formatOpeningMoves with single white move formats correctly`` () =
    let moves = [ mkPlyMove 1 "w" "e4" ]
    let result = formatOpeningMoves moves
    Assert.Equal("1. e4", result)

[<Fact>]
let ``formatOpeningMoves with single black move formats without move number`` () =
    let moves = [ mkPlyMove 1 "b" "e5" ]
    let result = formatOpeningMoves moves
    Assert.Equal("e5", result)

[<Fact>]
let ``formatOpeningMoves with full move pair formats correctly`` () =
    let moves = [ mkPlyMove 1 "w" "e4"; mkPlyMove 1 "b" "e5" ]
    let result = formatOpeningMoves moves
    Assert.Equal("1. e4 e5", result)

[<Fact>]
let ``formatOpeningMoves with multiple moves formats correctly`` () =
    let moves = [
        mkPlyMove 1 "w" "e4"
        mkPlyMove 1 "b" "e5"
        mkPlyMove 2 "w" "Nf3"
        mkPlyMove 2 "b" "Nc6"
    ]
    let result = formatOpeningMoves moves
    Assert.Equal("1. e4 e5 2. Nf3 Nc6", result)

[<Fact>]
let ``formatOpeningMoves with three moves formats correctly`` () =
    let moves = [
        mkPlyMove 1 "w" "d4"
        mkPlyMove 1 "b" "d5"
        mkPlyMove 2 "w" "c4"
    ]
    let result = formatOpeningMoves moves
    Assert.Equal("1. d4 d5 2. c4", result)

// ============================================================================
// computeRoundText tests
// ============================================================================

[<Fact>]
let ``computeRoundText first game of opening 1 returns 1.1`` () =
    let result = computeRoundText 1 0 0
    Assert.Equal("1.1", result)

[<Fact>]
let ``computeRoundText second game already played returns 1.2`` () =
    let result = computeRoundText 1 1 0
    Assert.Equal("1.2", result)

[<Fact>]
let ``computeRoundText with live game played returns 1.2`` () =
    let result = computeRoundText 1 0 1
    Assert.Equal("1.2", result)

[<Fact>]
let ``computeRoundText combines already played and live games`` () =
    let result = computeRoundText 1 2 3
    Assert.Equal("1.6", result)

[<Fact>]
let ``computeRoundText with different opening number`` () =
    let result = computeRoundText 5 3 2
    Assert.Equal("5.6", result)

[<Fact>]
let ``computeRoundText opening 10 game 1 returns 10.1`` () =
    let result = computeRoundText 10 0 0
    Assert.Equal("10.1", result)

// ============================================================================
// computeRoundTextFromPairing tests
// ============================================================================

[<Fact>]
let ``computeRoundTextFromPairing uses pairing RoundNr when set`` () =
    let pair = mkPairing "R1.G3" 1
    let result = computeRoundTextFromPairing pair 5 2
    Assert.Equal("R1.G3", result)

[<Fact>]
let ``computeRoundTextFromPairing computes when RoundNr is empty`` () =
    let pair = mkPairing "" 3
    let result = computeRoundTextFromPairing pair 2 1
    Assert.Equal("3.4", result)

[<Fact>]
let ``computeRoundTextFromPairing computes when RoundNr is whitespace`` () =
    let pair = mkPairing "   " 2
    let result = computeRoundTextFromPairing pair 0 0
    Assert.Equal("2.1", result)

[<Fact>]
let ``computeRoundTextFromPairing preserves custom round format`` () =
    let pair = mkPairing "Finals-Game1" 1
    let result = computeRoundTextFromPairing pair 10 10
    Assert.Equal("Finals-Game1", result)

// ============================================================================
// countOpeningsAlreadyPlayed tests
// ============================================================================

[<Fact>]
let ``countOpeningsAlreadyPlayed with empty array returns 0`` () =
    let games = [||]
    let result = countOpeningsAlreadyPlayed games "abc123"
    Assert.Equal(0, result)

[<Fact>]
let ``countOpeningsAlreadyPlayed with no matching hash returns 0`` () =
    let games = [| mkPgnGameWithHash 1 "xyz789" |]
    let result = countOpeningsAlreadyPlayed games "abc123"
    Assert.Equal(0, result)

[<Fact>]
let ``countOpeningsAlreadyPlayed with one matching hash returns 1`` () =
    let games = [| mkPgnGameWithHash 1 "abc123" |]
    let result = countOpeningsAlreadyPlayed games "abc123"
    Assert.Equal(1, result)

[<Fact>]
let ``countOpeningsAlreadyPlayed with multiple matching hashes returns count`` () =
    let games = [|
        mkPgnGameWithHash 1 "abc123"
        mkPgnGameWithHash 2 "abc123"
        mkPgnGameWithHash 3 "xyz789"
        mkPgnGameWithHash 4 "abc123"
    |]
    let result = countOpeningsAlreadyPlayed games "abc123"
    Assert.Equal(3, result)

[<Fact>]
let ``countOpeningsAlreadyPlayed with mixed hashes counts correctly`` () =
    let games = [|
        mkPgnGameWithHash 1 "hash1"
        mkPgnGameWithHash 2 "hash2"
        mkPgnGameWithHash 3 "hash1"
        mkPgnGameWithHash 4 "hash3"
        mkPgnGameWithHash 5 "hash2"
    |]
    Assert.Equal(2, countOpeningsAlreadyPlayed games "hash1")
    Assert.Equal(2, countOpeningsAlreadyPlayed games "hash2")
    Assert.Equal(1, countOpeningsAlreadyPlayed games "hash3")
    Assert.Equal(0, countOpeningsAlreadyPlayed games "hash4")

// ============================================================================
// createReplayDicts tests
// ============================================================================

[<Fact>]
let ``createReplayDicts with empty list returns empty map`` () =
    let result = createReplayDicts []
    Assert.True(result.IsEmpty)

[<Fact>]
let ``createReplayDicts with single engine returns map with one entry`` () =
    let engines = [ mkEngine "Stockfish" ]
    let result = createReplayDicts engines
    Assert.Equal(1, result.Count)
    Assert.True(result.ContainsKey "Stockfish")

[<Fact>]
let ``createReplayDicts with multiple engines returns map with all entries`` () =
    let engines = [ mkEngine "Stockfish"; mkEngine "Leela"; mkEngine "Komodo" ]
    let result = createReplayDicts engines
    Assert.Equal(3, result.Count)
    Assert.True(result.ContainsKey "Stockfish")
    Assert.True(result.ContainsKey "Leela")
    Assert.True(result.ContainsKey "Komodo")

[<Fact>]
let ``createReplayDicts creates empty dictionaries for each engine`` () =
    let engines = [ mkEngine "Engine1"; mkEngine "Engine2" ]
    let result = createReplayDicts engines
    Assert.Equal(0, result.["Engine1"].Count)
    Assert.Equal(0, result.["Engine2"].Count)

[<Fact>]
let ``createReplayDicts dictionaries are independent instances`` () =
    let engines = [ mkEngine "Engine1"; mkEngine "Engine2" ]
    let result = createReplayDicts engines
    // Add to one dictionary
    result.["Engine1"].[12345UL] <- { Engine = "Test"; Move = "e4"; TimeLeftInMs = 0L; Hash = "" }
    // Other dictionary should be unaffected
    Assert.Equal(1, result.["Engine1"].Count)
    Assert.Equal(0, result.["Engine2"].Count)

// ============================================================================
// setupBoardForGame tests (Medium Priority - Require Board Instance)
// ============================================================================

open ChessLibrary.Chess
open ChessLibrary.MiscTypes

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

[<Fact>]
let ``setupBoardForGame with empty FEN loads start position`` () =
    let board = Board()
    let opening = mkPgnGameWithFenAndMoves 1 "" []
    let pair = mkPairingWithOpening "White" "Black" opening
    let mutable chess960Called = false

    setupBoardForGame board pair false 10 (fun _ -> chess960Called <- true) |> ignore

    Assert.Equal(startPosFen, board.StartPosition)
    Assert.Equal(0, board.PlyCount)
    Assert.False(chess960Called) // Not called for empty FEN

[<Fact>]
let ``setupBoardForGame with custom FEN loads that position`` () =
    let board = Board()
    let customFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let opening = mkPgnGameWithFenAndMoves 1 customFen []
    let pair = mkPairingWithOpening "White" "Black" opening
    let mutable chess960Value = None

    setupBoardForGame board pair false 10 (fun v -> chess960Value <- Some v) |> ignore

    Assert.Equal(customFen, board.StartPosition)
    Assert.True(chess960Value.IsSome) // Callback was called
    Assert.False(chess960Value.Value) // Standard chess, not Chess960

[<Fact>]
let ``setupBoardForGame plays opening moves for PGN book`` () =
    let board = Board()
    let moves = [
        mkPlyMove 1 "w" "e4"
        mkPlyMove 1 "b" "e5"
        mkPlyMove 2 "w" "Nf3"
    ]
    let opening = mkPgnGameWithFenAndMoves 1 "" moves
    let pair = mkPairingWithOpening "White" "Black" opening

    setupBoardForGame board pair false 10 ignore |> ignore

    Assert.Equal(3, board.PlyCount)
    Assert.Contains("e2e4", board.UciMovesPlayed)
    Assert.Contains("e7e5", board.UciMovesPlayed)
    Assert.Contains("g1f3", board.UciMovesPlayed)

[<Fact>]
let ``setupBoardForGame truncates moves to openingsPly`` () =
    let board = Board()
    let moves = [
        mkPlyMove 1 "w" "e4"
        mkPlyMove 1 "b" "e5"
        mkPlyMove 2 "w" "Nf3"
        mkPlyMove 2 "b" "Nc6"
    ]
    let opening = mkPgnGameWithFenAndMoves 1 "" moves
    let pair = mkPairingWithOpening "White" "Black" opening

    setupBoardForGame board pair false 2 ignore |> ignore // Only 2 plies

    Assert.Equal(2, board.PlyCount)
    Assert.Contains("e2e4", board.UciMovesPlayed)
    Assert.Contains("e7e5", board.UciMovesPlayed)
    Assert.DoesNotContain("g1f3", board.UciMovesPlayed)

[<Fact>]
let ``setupBoardForGame for EPD book does not play moves`` () =
    let board = Board()
    let customFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let moves = [ mkPlyMove 1 "b" "e5" ] // EPD books have FEN, moves should be ignored
    let opening = mkPgnGameWithFenAndMoves 1 customFen moves
    let pair = mkPairingWithOpening "White" "Black" opening

    setupBoardForGame board pair true 10 ignore |> ignore // epdBook = true

    Assert.Equal(customFen, board.StartPosition)
    Assert.Equal(0, board.UciMovesPlayed.Count) // No moves played for EPD

[<Fact>]
let ``setupBoardForGame returns truncated opening moves`` () =
    let moves = [
        mkPlyMove 1 "w" "d4"
        mkPlyMove 1 "b" "d5"
        mkPlyMove 2 "w" "c4"
        mkPlyMove 2 "b" "e6"
    ]
    let opening = mkPgnGameWithFenAndMoves 1 "" moves
    let pair = mkPairingWithOpening "White" "Black" opening
    let board = Board()

    let returnedMoves = setupBoardForGame board pair false 3 ignore |> Seq.toList

    Assert.Equal(3, returnedMoves.Length)
    Assert.Equal("d4", returnedMoves.[0].San)
    Assert.Equal("d5", returnedMoves.[1].San)
    Assert.Equal("c4", returnedMoves.[2].San)

[<Fact>]
let ``setupBoardForGame with FEN and PGN book plays moves from FEN`` () =
    let board = Board()
    // Start from a position where e4 has been played
    let fenAfterE4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let moves = [ mkPlyMove 1 "b" "e5" ]
    let opening = mkPgnGameWithFenAndMoves 1 fenAfterE4 moves
    let pair = mkPairingWithOpening "White" "Black" opening

    setupBoardForGame board pair false 10 ignore |> ignore

    Assert.Equal(fenAfterE4, board.StartPosition)
    Assert.Equal(1, board.UciMovesPlayed.Count) // e5 was played from the FEN position
    Assert.Contains("e7e5", board.UciMovesPlayed)

// ============================================================================
// getPositionWithMoves tests (Medium Priority)
// ============================================================================

[<Fact>]
let ``getPositionWithMoves with start position and no moves`` () =
    let board = Board()
    board.LoadFen startPosFen
    board.StartPosition <- startPosFen

    let result = getPositionWithMoves board

    Assert.Equal($"position fen {startPosFen} moves", result)

[<Fact>]
let ``getPositionWithMoves with one move played`` () =
    let board = Board()
    board.LoadFen startPosFen
    board.StartPosition <- startPosFen
    board.PlayUciMove "e2e4"

    let result = getPositionWithMoves board

    Assert.Equal($"position fen {startPosFen} moves e2e4", result)

[<Fact>]
let ``getPositionWithMoves with multiple moves played`` () =
    let board = Board()
    board.LoadFen startPosFen
    board.StartPosition <- startPosFen
    board.PlayUciMove "e2e4"
    board.PlayUciMove "e7e5"
    board.PlayUciMove "g1f3"

    let result = getPositionWithMoves board

    Assert.Equal($"position fen {startPosFen} moves e2e4 e7e5 g1f3", result)

[<Fact>]
let ``getPositionWithMoves with custom FEN start position`` () =
    let board = Board()
    let customFen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    board.LoadFen customFen
    board.StartPosition <- customFen
    board.PlayUciMove "f1b5"

    let result = getPositionWithMoves board

    Assert.Equal($"position fen {customFen} moves f1b5", result)
