module ChessLibrary.GameSetup

open System
open System.Text
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.Engine
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.GameExecution
open ChessLibrary.GamePersistence
open ChessLibrary.CustomException

// ============================================================================
// Board Setup for Game
// ============================================================================

/// Setup board from pairing opening, handling both PGN and EPD formats
let setupBoardForGame
    (board: Board)
    (pair: Pairing)
    (epdBook: bool)
    (openingsPly: int)
    (setIsChess960: bool -> unit)
    : PlyMove seq =
    let openingMoves = pair.Opening.Mainline |> Seq.truncate openingsPly
    board.ResetBoardState()
    if pair.Opening.Fen = "" then
        board.LoadFen Chess.startPos
        board.StartPosition <- Chess.startPos
    else
        board.LoadFen pair.Opening.Fen
        board.StartPosition <- pair.Opening.Fen
        setIsChess960 board.IsFRC

    if not epdBook then
        for m in openingMoves do
            board.PlayOpeningMove m.San
    else
        board.ResetBoardState()
        board.LoadFen pair.Opening.Fen
        board.StartPosition <- pair.Opening.Fen
        setIsChess960 board.IsFRC

    openingMoves

/// Log opening information to logger
let logOpeningInfo (logger: ILogger) (pair: Pairing) (openingMoves: PlyMove seq) : unit =
    let completeGame =
        openingMoves
        |> Seq.mapi (fun _ m ->
            if m.Color = "w" then
                sprintf "%d. %s" m.MoveNumber m.San
            else
                sprintf "%s" m.San)
        |> String.concat " "
    logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)

/// Log position with moves
let logPosition (logger: ILogger) (board: Board) : unit =
    let posWithMoves =
        let fen = board.StartPosition
        let start = $"position fen {fen} moves"
        board.LongSANMovesPlayed |> Seq.fold (fun state m -> sprintf "%s %s" state m) start
    logger.LogInformation("{position}", posWithMoves)

// ============================================================================
// Round Text Computation
// ============================================================================

/// Compute round text, preferring pair.RoundNr if set
let computeRoundTextFromPairing
    (pair: Pairing)
    (openingsAlreadyPlayed: int)
    (liveGamesPlayed: int)
    : string =
    if String.IsNullOrWhiteSpace pair.RoundNr |> not then
        pair.RoundNr
    else
        $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1}"

/// Count openings already played with same hash
let countOpeningsAlreadyPlayed (gamesAlreadyPlayed: PgnGame[]) (openingHash: string) : int =
    gamesAlreadyPlayed |> Seq.filter (fun e -> e.GameMetaData.OpeningHash = openingHash) |> Seq.length

// ============================================================================
// Full Game Execution with Setup
// ============================================================================

/// Execute a single game with full setup, logging, and cleanup.
/// This is the unified game execution flow used by cup and swiss playPairing.
let executeGameWithSetup
    (logger: ILogger)
    (tourny: Tournament)
    (board: Board)
    (pair: Pairing)
    (epdBook: bool)
    (sb: StringBuilder)
    (cts: CancellationTokenSource)
    (replayDicts: Map<string, ReferenceGameReplay>)
    (replayList: ResizeArray<GameReplay>)
    (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    (callback: Update -> unit)
    (roundTxt: string)
    : Result =

    // Setup board and log opening info
    let openingMoves = setupBoardForGame board pair epdBook tourny.Opening.OpeningsPly (fun v -> tourny.IsChess960 <- v)
    logOpeningInfo logger pair openingMoves
    logPosition logger board

    // Create engines
    let engine1 = EngineHelper.createEngine (pair.White, Some logger)
    let engine2 = EngineHelper.createEngine (pair.Black, Some logger)

    // Notify round
    callback (Update.RoundNr roundTxt)

    // Execute game with exception handling
    let getReplayDictForPlayer name = replayDicts.[name]
    let replayDictWhite = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.White.Name) else None
    let replayDictBlack = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.Black.Name) else None

    let result = executeGame tourny replayDictWhite replayDictBlack sb cts logger board engine1 engine2 pair tryGetUserAdjudication callback

    // Process completed game
    let gameData = buildGameMetadata tourny pair result roundTxt
    addToReplayList replayList tourny result gameData board.LongSANMovesPlayed
    let moveSection = sb.ToString()
    writeGameToPgn pgnAgent tourny gameData moveSection result cts

    if tourny.VerboseLogging then
        logger.LogInformation("Game metadata added to result: {pgnData}", gameData)

    // Cleanup engines
    cleanupEngines engine1 engine2

    result

/// Execute a game for cup/swiss with mutable engine variables and conditional cleanup
let executeGameWithEngineReuse
    (logger: ILogger)
    (tourny: Tournament)
    (board: Board)
    (pair: Pairing)
    (epdBook: bool)
    (sb: StringBuilder)
    (cts: CancellationTokenSource)
    (replayDicts: Map<string, ReferenceGameReplay>)
    (replayList: ResizeArray<GameReplay>)
    (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    (callback: Update -> unit)
    (roundTxt: string)
    (engine1Ref: byref<ChessEngine>)
    (engine2Ref: byref<ChessEngine>)
    (numberOfPlayers: int)
    (forceCreateEngines: bool)
    : Result =

    // Setup board and log opening info
    let openingMoves = setupBoardForGame board pair epdBook tourny.Opening.OpeningsPly (fun v -> tourny.IsChess960 <- v)
    logOpeningInfo logger pair openingMoves
    logPosition logger board

    // Create or reuse engines
    if forceCreateEngines || numberOfPlayers > 2 then
        engine1Ref <- EngineHelper.createEngine (pair.White, Some logger)
        engine2Ref <- EngineHelper.createEngine (pair.Black, Some logger)
    if engine1Ref = Unchecked.defaultof<ChessEngine> || engine2Ref = Unchecked.defaultof<ChessEngine> then
        engine1Ref <- EngineHelper.createEngine (pair.White, Some logger)
        engine2Ref <- EngineHelper.createEngine (pair.Black, Some logger)
    if engine1Ref.Name = pair.Black.Name || engine2Ref.Name = pair.White.Name then
        let (eng1, eng2) = engine2Ref, engine1Ref
        engine1Ref <- eng1
        engine2Ref <- eng2

    // Notify round
    callback (Update.RoundNr roundTxt)

    // Execute game with exception handling
    let getReplayDictForPlayer name = replayDicts.[name]
    let replayDictWhite = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.White.Name) else None
    let replayDictBlack = if tourny.PreventMoveDeviation then Some (getReplayDictForPlayer pair.Black.Name) else None

    let result = executeGame tourny replayDictWhite replayDictBlack sb cts logger board engine1Ref engine2Ref pair tryGetUserAdjudication callback

    result
