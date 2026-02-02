module ChessLibrary.GameHelpers

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Channels
open Microsoft.Extensions.Logging
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.ChessUtilities
open ChessLibrary.Engine
open ChessLibrary.EngineProtocol
open ChessLibrary.TournamentTypes
open ChessLibrary.GameReplay

// Alias for backward compatibility with existing code that calls Formatting.createResultWithEval
module Formatting = ChessLibrary.TypesDef.CoreTypes

/// The adjudication reason constant for user-initiated adjudication
let adjudicationReason = ResultReason.AdjudicatedByUser

/// Creates a result for a player who lost on time
let lostOnTimeResult (playing: string) (opponent: string) (isWhite: bool) (gameMoveList: ResizeArray<string>) (gametimer: int64) evals : Result =
    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let resStr = if isWhite then "0-1" else "1-0"
    let player1, player2 = if isWhite then playing, opponent else opponent, playing
    Formatting.createResultWithEval player1 player2 gameMoveList resStr ResultReason.ForfeitLimits dur evals

/// Updates the clocks after a move, returning (currentTime, timeLeft)
let updateClocks
    (duration: TimeSpan)
    (pFixedTime: TimeOnly)
    (pIncr: TimeOnly)
    (useNodes: bool) =

    // how many ticks remain after the move (considering increment)
    let currentTicksLeft = pFixedTime.Ticks + pIncr.Ticks - duration.Ticks
    // clamp at zero, update clocks and compute "time left" after increment
    let ticks = max currentTicksLeft 0L
    if useNodes then
        // Node-limit mode: keep times unchanged; report current active as-is
        let currentTime = pFixedTime
        // In node-limit mode we also keep timeLeft the same since clocks are irrelevant
        let timeLeft = pFixedTime
        (currentTime, timeLeft)
    else
        let updatedTimePlaying = TimeOnly ticks
        let timeLeft = TimeOnly (ticks + pIncr.Ticks)
        (updatedTimePlaying, timeLeft)

/// Checks if the app is shutting down
let isAppShuttingDown (cts: CancellationTokenSource) =
    cts.IsCancellationRequested
    || Environment.HasShutdownStarted
    || AppDomain.CurrentDomain.IsFinalizingForUnload()

/// Centralized logging + crash result builder for unexpected exceptions during a game
let handleGameException
    (logger: ILogger)
    (ex: exn)
    (cts: CancellationTokenSource)
    (gametimer: int64)
    (board: Board)
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (pair: Pairing) : Result =

    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let white, black = pair.White.Name, pair.Black.Name
    let shutdown = isAppShuttingDown cts
    let moves = board.ShortSANMovesPlayed

    // Helper to create results
    let createCancelResult () =
        Formatting.createResult white black moves "1/2-1/2" ResultReason.Cancel dur

    let createDisconnectedResult engineName resultStr =
        Formatting.createResult white black moves resultStr (ResultReason.Disconnected engineName) dur

    // Enhanced diagnostics check
    let checkEngineWithDiagnostics (eng: ChessEngine) =
        try
            let exited = eng.HasExited()
            if exited then
                let diag = eng.GetDiagnostics()
                let stderr =
                    eng.ErrorOutput
                    |> Seq.truncate 10
                    |> String.concat "; "
                logger.LogCritical(
                    "Engine {Engine} crashed. Diagnostics: {Diag} | Stderr: {Stderr}",
                    eng.Name, diag, stderr)
            exited
        with _ -> true

    // Async poll with exponential backoff
    let pollEngineStatusAsync maxAttempts = async {
        let rec poll attempt (delay:int) =
            async {
                if attempt >= maxAttempts then
                    return (checkEngineWithDiagnostics engine1, checkEngineWithDiagnostics engine2)
                else
                    let e1 = checkEngineWithDiagnostics engine1
                    let e2 = checkEngineWithDiagnostics engine2
                    if e1 || e2 then
                        return (e1, e2)
                    else
                        do! Async.Sleep delay
                        return! poll (attempt + 1) (min (delay * 2) 1000)
            }
        return! poll 0 100
    }

    let e1Exited, e2Exited = pollEngineStatusAsync 5 |> Async.RunSynchronously

    // Classify exception type
    let isPipeOrIoError =
        match ex with
        | :? IOException -> true
        | :? ObjectDisposedException -> true
        | :? InvalidOperationException as ioe ->
            let msg = ioe.Message.ToLowerInvariant()
            msg.Contains("standardoutput") || msg.Contains("standardinput")
        | _ ->
            let msg = ex.Message.ToLowerInvariant()
            msg.Contains("pipe") || msg.Contains("broken") || msg.Contains("closed")

    // Async cleanup helper
    let forceStopEngineAsync (eng: ChessEngine) = async {
        try
            if not (eng.HasExited()) then
                eng.StopProcess()
                do! Async.Sleep 1000
        with cleanupEx ->
            logger.LogWarning(cleanupEx, "Error stopping {Engine}", eng.Name)
    }

    // Decision tree with async cleanup
    let result =
        match shutdown, e1Exited, e2Exited with
        // Both engines crashed
        | _, true, true ->
            logger.LogCritical(ex, "Both engines crashed: {White} vs {Black}", white, black)
            createCancelResult ()

        // Application shutdown
        | true, _, _ ->
            logger.LogCritical(ex, "App shutdown during game: {White} vs {Black}", white, black)
            async {
                do! forceStopEngineAsync engine1
                do! forceStopEngineAsync engine2
            } |> Async.RunSynchronously
            createCancelResult ()

        // Engine1 crashed
        | false, true, false ->
            logger.LogCritical(ex, "{Engine} crashed: {White} vs {Black}", engine1.Name, white, black)
            forceStopEngineAsync engine2 |> Async.RunSynchronously
            createDisconnectedResult engine1.Name "0-1"

        // Engine2 crashed
        | false, false, true ->
            logger.LogCritical(ex, "{Engine} crashed: {White} vs {Black}", engine2.Name, white, black)
            forceStopEngineAsync engine1 |> Async.RunSynchronously
            createDisconnectedResult engine2.Name "1-0"

        // Both alive - investigate further
        | false, false, false ->
            if isPipeOrIoError then
                // No stderr clues - poll again with longer timeout
                let e1b, e2b = pollEngineStatusAsync 10 |> Async.RunSynchronously
                match e1b, e2b with
                | true, false ->
                    logger.LogCritical("After polling: {Engine} exited", engine1.Name)
                    forceStopEngineAsync engine2 |> Async.RunSynchronously
                    createDisconnectedResult engine1.Name "0-1"

                | false, true ->
                    logger.LogCritical("After polling: {Engine} exited", engine2.Name)
                    forceStopEngineAsync engine1 |> Async.RunSynchronously
                    createDisconnectedResult engine2.Name "1-0"

                | true, true ->
                    logger.LogCritical("After polling: both engines exited")
                    createCancelResult ()

                | false, false ->
                    logger.LogError(ex, "Unresolved pipe error: {White} vs {Black}", white, black)
                    async {
                        do! forceStopEngineAsync engine1
                        do! forceStopEngineAsync engine2
                    } |> Async.RunSynchronously
                    createCancelResult ()
            else
                // Unexpected exception
                logger.LogCritical(ex, "Unexpected error: {White} vs {Black}", white, black)
                async {
                    do! forceStopEngineAsync engine1
                    do! forceStopEngineAsync engine2
                } |> Async.RunSynchronously
                createCancelResult ()

    result

/// Returns the first two evaluations from a full eval list (reversed order)
let firstTwoEvals fullEvalList =
    match fullEvalList |> List.rev with
    | [] -> []
    | [x] -> [x]
    | x::y::_ -> [x; y]

/// Start a background writer that reads from engine.ReadLineAsync()
/// and writes lines into the provided channel for the duration of the game.
let startEngineChannelWriter
    (engine: ChessEngine)
    (engineChannel: Channel<string>)
    (parentCts: CancellationTokenSource)
    (logger: ILogger)
    (inPonderMode: unit -> bool)
    (callback: Update -> unit)
    (isWhite: unit -> bool) =

    let cts = CancellationTokenSource.CreateLinkedTokenSource(parentCts.Token)
    // Poll/throttle settings for ponder updates
    let mutable ponderPollIntervalMs = 1000L
    let sw = Stopwatch.StartNew()
    let rec loop () = async {
        try
            if cts.Token.IsCancellationRequested then
                try engineChannel.Writer.TryComplete() |> ignore with _ -> ()
                logger.LogInformation("Engine channel writer for {Engine} is stopping due to cancellation", engine.Name)
                return ()
            else
                let! line = engine.ReadLineAsync() |> Async.AwaitTask
                let inPonder = inPonderMode()
                let isWhite = isWhite()
                if not (isNull line) then
                    match line with
                    |line when inPonder && sw.ElapsedMilliseconds > ponderPollIntervalMs && line.StartsWith "info depth" ->
                        match Regex.getEssentialData line (not isWhite) with
                        | Some (d, eval, nodes, nps, _pvLine, tbhits, wdl, sd, _mPv) ->
                            let status = {
                                PlayerName = engine.Name; Eval = eval; Depth = d; SD = sd
                                Nodes = nodes; NPS = float nps; TBhits = tbhits
                                WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound }
                            callback (PonderStatus status)
                            sw.Restart()
                        | None -> ()

                    |line when inPonder && line.StartsWith "bestmove" ->
                        sw.Restart()
                    |line when not inPonder ->
                        engineChannel.Writer.TryWrite(line) |> ignore
                    | _ -> ()

                return! loop()
        with
        | :? OperationCanceledException
        | :? ObjectDisposedException ->
            try engineChannel.Writer.TryComplete() |> ignore with _ -> ()
        | ex ->
            logger.LogWarning(ex, "Engine channel writer error for {Engine}", engine.Name)
            try engineChannel.Writer.TryComplete(ex) |> ignore with _ -> ()
    }

    Async.Start(loop(), cts.Token)
    cts

/// Annotation helper for move output
let annotation verboseAnnotation (board: Board) (numberMove : string) (chessMoveInfo : ChessMoveInfo) =
    if verboseAnnotation then
        if board.PlyCount > 1 then
            sprintf " %s {%s}" numberMove chessMoveInfo.Annotation
        else
            sprintf "%s {%s}" numberMove chessMoveInfo.Annotation
    else
        if board.PlyCount > 1 then
            sprintf " %s {%s}" numberMove chessMoveInfo.MinimalAnnotation
        else
            sprintf "%s {%s}" numberMove chessMoveInfo.MinimalAnnotation

/// Log the init commands for both engines
let logEngineInitCommands (logger: ILogger) (player1: ChessEngine) (player2: ChessEngine) =
    logger.LogDebug($"\nInitializing {player1.Name} ....")
    for cmd in player1.GetVerifiedCommands() do
        logger.LogDebug($"{cmd}")
    logger.LogDebug($"\nInitializing {player2.Name} ....")
    for cmd in player2.GetVerifiedCommands() do
        logger.LogDebug($"{cmd}")

// ============================================================================
// Opening and Game Loading Helpers
// ============================================================================

/// Load openings from PGN or EPD file, returning games and whether it's an EPD book
let loadOpenings (path: string option) (maxRounds: int) : PgnGame[] * bool =
    match path with
    | Some p ->
        if p.ToLower().Contains ".epd" then
            let games = EPDExtractor.parseEPDFile p |> Seq.truncate maxRounds |> Seq.toArray
            (games, true)
        else
            let games = ChessLibrary.FullPGNParser.parsePgnFile p |> Seq.truncate maxRounds |> Seq.toArray
            (games, false)
    | None ->
        let games = [| for i = 1 to maxRounds do yield PgnGame.Empty i |]
        (games, false)

/// Load openings from PGN or EPD file without truncating (for Cup/Swiss)
let loadOpeningsUnlimited (path: string option) (defaultRounds: int) : PgnGame[] * bool =
    match path with
    | Some p ->
        if p.ToLower().Contains ".epd" then
            let games = EPDExtractor.parseEPDFile p |> Seq.toArray
            (games, true)
        else
            let games = ChessLibrary.FullPGNParser.parsePgnFile p |> Seq.toArray
            (games, false)
    | None ->
        let games = [| for i = 1 to defaultRounds do yield PgnGame.Empty i |]
        (games, false)

/// Load games already played from PGN output file
let loadGamesAlreadyPlayed (pgnOutPath: string) : PgnGame[] =
    if File.Exists pgnOutPath then
        let parsed = ChessLibrary.FullPGNParser.parsePgnFile pgnOutPath |> Seq.toArray
        parsed |> Array.iter Hash.writeOpeningHashToPgnGame
        parsed
    else
        [||]

/// Load reference games for deviation tracking
let loadReferenceGames (referencePath: string) : PgnGame[] =
    if File.Exists referencePath then
        ChessLibrary.FullPGNParser.parsePgnFile referencePath |> Seq.toArray
    else
        [||]

/// Create replay dictionaries for all engines
let createReplayDicts (engines: EngineConfig list) : Map<string, ReferenceGameReplay> =
    [ for eng in engines -> eng.Name, ReferenceGameReplay() ] |> Map.ofList

// ============================================================================
// Opening Formatting Helpers
// ============================================================================

/// Format opening moves as string (e.g., "1. e4 e5 2. Nf3 Nc6")
let formatOpeningMoves (openingMoves: PlyMove seq) : string =
    openingMoves
    |> Seq.mapi (fun _ m ->
        if m.Color = "w" then
            sprintf "%d. %s" m.MoveNumber m.San
        else
            sprintf "%s" m.San)
    |> String.concat " "

/// Compute round text from pairing and games played
let computeRoundText (openingNumber: int) (openingsAlreadyPlayed: int) (liveGamesPlayed: int) : string =
    $"{openingNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1}"

/// Initialize board from pairing opening
let initBoardFromOpening
    (board: Board)
    (opening: PgnGame)
    (epdBook: bool)
    (openingMoves: PlyMove seq)
    (isChess960Setter: bool -> unit)
    : unit =
    board.ResetBoardState()
    if opening.Fen = "" then
        board.LoadFen Chess.startPos
        board.StartPosition <- Chess.startPos
    else
        board.LoadFen opening.Fen
        board.StartPosition <- opening.Fen
        isChess960Setter board.IsFRC

    if not epdBook then
        for m in openingMoves do
            board.PlayOpeningMove m.San
    else
        board.ResetBoardState()
        board.LoadFen opening.Fen
        board.StartPosition <- opening.Fen
        isChess960Setter board.IsFRC

/// Get position with moves string for logging
let getPositionWithMoves (board: Board) : string =
    let fen = board.StartPosition
    let start = $"position fen {fen} moves"
    board.LongSANMovesPlayed |> Seq.fold (fun state m -> sprintf "%s %s" state m) start

// ============================================================================
// Replay List Helpers
// ============================================================================

/// Search replay list and update deviation counter
let searchAndPrepareReplay
    (pairing: Pairing)
    (replayDicts: Map<string, ReferenceGameReplay>)
    (replayList: ResizeArray<GameReplay>)
    (referencGamesPlayed: PgnGame[])
    (gamesAlreadyPlayed: PgnGame[])
    (tourny: Tournament)
    : unit =
    let lastGame = gamesAlreadyPlayed |> Seq.tryLast
    let deviations = match lastGame with | Some g -> g.GameMetaData.Deviations | _ -> 0
    if deviations > tourny.DeviationCounter then
        tourny.DeviationCounter <- deviations
    prepareGameReplay pairing replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960 false