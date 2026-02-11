module ChessLibrary.GamePersistence

open System
open System.Text
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.Engine
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.GameExecution
open ChessLibrary.CustomException

// ============================================================================
// Game Metadata Building
// ============================================================================

/// Build GameMetadata from result and pairing
let buildGameMetadata
    (tourny: Tournament)
    (pair: Pairing)
    (result: Result)
    (roundTxt: string)
    : GameMetadata =
    { OpeningHash = pair.OpeningHash
      Event = tourny.Description
      Site = tourny.Name
      Date = DateTime.Now.ToShortDateString()
      Round = roundTxt
      White = result.Player1
      Black = result.Player2
      Result = result.Result
      Reason = result.Reason
      GameTime = result.GameTime
      Moves = result.Moves
      Fen = pair.Opening.Fen
      OpeningName = pair.Opening.GameMetaData.OpeningName
      Deviations = tourny.DeviationCounter
      StartEvals = result.OutOfOpeningEvals
      OtherTags = pair.Opening.GameMetaData.OtherTags }

// ============================================================================
// Replay List Management
// ============================================================================

/// Add game to replay list if deviation tracking enabled
let addToReplayList
    (replayList: ResizeArray<GameReplay>)
    (tourny: Tournament)
    (result: Result)
    (gameData: GameMetadata)
    (longSanMoves: ResizeArray<string>)
    : unit =
    if tourny.PreventMoveDeviation then
        replayList.Add
            { WhitePlayer = result.Player1
              BlackPlayer = result.Player2
              PGNMetaData = gameData
              LongSanMoves = longSanMoves |> ResizeArray }

// ============================================================================
// PGN Writing
// ============================================================================

/// Write game to PGN file via agent (with cancellation and reason checks)
let writeGameToPgn
    (agent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>)
    (tourny: Tournament)
    (gameData: GameMetadata)
    (moveSection: string)
    (result: Result)
    (cts: CancellationTokenSource)
    : unit =
    if result.Reason <> ResultReason.Cancel
       && not cts.IsCancellationRequested
       && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
        agent.Post(ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))

/// Write game to PGN file via agent (simple version without reason check)
let writeGameToPgnSimple
    (agent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>)
    (tourny: Tournament)
    (gameData: GameMetadata)
    (moveSection: string)
    (result: Result)
    (cts: CancellationTokenSource)
    : unit =
    if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
        agent.Post(ChessLibrary.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))

// ============================================================================
// Engine Cleanup
// ============================================================================

/// Stop engines if not already exited
let cleanupEngines (engine1: ChessEngine) (engine2: ChessEngine) : unit =
    try
        if not (engine1.HasExited()) then
            try engine1.Quit() with _ -> ()
            try engine1.StopProcess() with _ -> ()
    with _ -> ()
    try
        if not (engine2.HasExited()) then
            try engine2.Quit() with _ -> ()
            try engine2.StopProcess() with _ -> ()
    with _ -> ()

/// Stop engines conditionally based on reason or player count
let cleanupEnginesConditional
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (forceStop: bool)
    (numberOfPlayers: int)
    (cts: CancellationTokenSource)
    : unit =
    if forceStop || numberOfPlayers > 2 || cts.IsCancellationRequested then
        cleanupEngines engine1 engine2

// ============================================================================
// Exception Context Building
// ============================================================================

/// Create exception context for engine failures
let createExceptionContext
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (pair: Pairing)
    (board: Board)
    (tourny: Tournament)
    : CatchContext =
    let w = tourny.TimeControlTextForPlayer engine1.Config.TimeControlID
    let b = tourny.TimeControlTextForPlayer engine2.Config.TimeControlID
    let timeInfo = $"[{w}; {b}]"
    { EngineName = engine1.Name
      OpponentName = engine2.Name
      GameNumber = pair.GameNr
      MoveNumber = board.MoveNumber()
      TimeControl = timeInfo
      TimeRemaining = None
      PositionFen = board.FEN()
      LastCommand = None
      TimestampUtc = DateTime.UtcNow
      MoveHistory = board.GetMoveHistory() }

// ============================================================================
// Game Execution with Unified Dispatch
// ============================================================================

/// Execute game with unified dispatch (play, playWithPondering, or playDoNotDeviate)
let executeGame
    (tourny: Tournament)
    (replayDictWhite: ReferenceGameReplay option)
    (replayDictBlack: ReferenceGameReplay option)
    (sb: StringBuilder)
    (cts: CancellationTokenSource)
    (logger: ILogger)
    (board: Board)
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (pair: Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    (callback: Update -> unit)
    : Result =
    let gametimer = Stopwatch.GetTimestamp()
    try
        if tourny.PreventMoveDeviation then
            match replayDictWhite, replayDictBlack with
            | Some dictWhite, Some dictBlack ->
                playDoNotDeviate dictWhite dictBlack sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback
                |> Async.RunSynchronously
            | _ ->
                failwith "Replay dictionaries required for PreventMoveDeviation mode"
        else
            if tourny.AllowPondering then
                playWithPondering sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback
                |> Async.RunSynchronously
            else
                play sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback
                |> Async.RunSynchronously
    with
    | :? EngineStartupException as ex ->
        let context = createExceptionContext engine1 engine2 pair board tourny
        EngineFailures.log logger ex context
        handleGameException logger ex cts gametimer board engine1 engine2 pair
    | ex ->
        let context = createExceptionContext engine1 engine2 pair board tourny
        EngineFailures.log logger ex context
        handleGameException logger ex cts gametimer board engine1 engine2 pair

/// Execute game without deviation prevention
let executeGameSimple
    (tourny: Tournament)
    (sb: StringBuilder)
    (cts: CancellationTokenSource)
    (logger: ILogger)
    (board: Board)
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (pair: Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    (callback: Update -> unit)
    : Result =
    executeGame tourny None None sb cts logger board engine1 engine2 pair tryGetUserAdjudication callback

// ============================================================================
// Full Game Processing
// ============================================================================

/// Process a completed game: build metadata, add to replay, write PGN
let processCompletedGame
    (tourny: Tournament)
    (pair: Pairing)
    (result: Result)
    (roundTxt: string)
    (board: Board)
    (sb: StringBuilder)
    (replayList: ResizeArray<GameReplay>)
    (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>)
    (cts: CancellationTokenSource)
    (logger: ILogger)
    : GameMetadata =
    let gameData = buildGameMetadata tourny pair result roundTxt
    addToReplayList replayList tourny result gameData board.UciMovesPlayed
    let moveSection = sb.ToString()
    writeGameToPgn pgnAgent tourny gameData moveSection result cts
    if tourny.VerboseLogging then
        logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
    gameData
