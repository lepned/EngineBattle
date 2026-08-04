module ChessLibrary.GameInitialization

open System
open System.IO
open System.Text
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.Engine
module PGNWriter = ChessLibrary.PGNWriter
open ChessLibrary.EngineProtocol
open ChessLibrary.CustomException

let createAsyncDelay (delay:int) =
    async {
        do! Async.Sleep(delay)
        return true }

let waitForEngineIsReady (tourny:Tournament) (engine: ChessEngine) (logger: ILogger) = async {
    let isWinBoard = engine.Config.Protocol.ToLower().Contains("winboard")
    // Winboard engines already complete init during StartProcess (wait for done=1)
    // UCI engines need extra time for internal initialization and cleanup
    let pumpCleanupDelay = if isWinBoard then 50 else 200
    let engineStopDelay = if isWinBoard then 100 else 300
    let engineStartDelay = if isWinBoard then 100 else 2000
    let engineRecoveryDelay = if isWinBoard then 100 else 1000

    try
        do! Async.Sleep pumpCleanupDelay

        //Handle engine process state
        let! processReady = async {
            if engine.HasExited() then
                logger.LogDebug("Engine {Engine} has exited, starting fresh", engine.Name)
                try
                    engine.StartProcess()
                    let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                    if not ok then
                        failwith "Engine did not respond to isready command."
                    do! Async.Sleep engineStartDelay
                    return true
                with ex ->
                    logger.LogError(ex, "Failed to start engine {Engine}", engine.Name)
                    return false
            else
                try
                    if isWinBoard then
                        logger.LogDebug("Engine {Engine} running (WinBoard protocol)", engine.Name)
                        return true
                    else
                        logger.LogDebug("Engine {Engine} still running, resetting", engine.Name)
                        // Graceful stop attempt
                        engine.Stop()
                        do! Async.Sleep engineStopDelay
                        let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                        if not ok then
                            failwith "Engine did not respond to isready command."
                        return true
                with ex ->
                    logger.LogWarning(ex, "Error resetting engine {Engine}, forcing restart", engine.Name)
                    engine.StopProcess()
                    do! Async.Sleep engineRecoveryDelay
                    engine.StartProcess()
                    do! Async.Sleep engineStartDelay
                    let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                    if not ok then
                        failwith "Engine did not respond to isready command."
                    return true
        }

        if not processReady then
            return false
        else
            //Verify process is running
            if engine.HasExited() then
                logger.LogError("Engine {Engine} failed to start after all attempts", engine.Name)
                return false
            elif isWinBoard then
                // Winboard engines with reuse=0 (e.g. Crafty 25.2) need a full process restart
                if not engine.CanReuseWinboard then
                    logger.LogDebug("Engine {Engine} has reuse=0, forcing process restart", engine.Name)
                    engine.StopProcess()
                    do! Async.Sleep engineRecoveryDelay
                    engine.StartProcess()
                    do! Async.Sleep engineStartDelay
                    let readyOk = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000)
                    if readyOk then
                        logger.LogDebug("Engine {Engine} restarted and ready (WinBoard reuse=0)", engine.Name)
                        return true
                    else
                        logger.LogError("Engine {Engine} timed out after restart (WinBoard reuse=0)", engine.Name)
                        return false
                else
                    // Winboard engines need 'new' to reset between games
                    engine.UciNewGame()
                    let readyOk = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000)
                    if readyOk then
                        logger.LogDebug("Engine {Engine} ready (WinBoard protocol)", engine.Name)
                        return true
                    else
                        logger.LogError("Engine {Engine} timed out waiting for readyok (WinBoard)", engine.Name)
                        return false
            else
                engine.UciNewGame()

                //Wait for ready acknowledgment
                let timeoutInSec = max 180 tourny.EngineStartupTimeoutInSec
                let timeoutInMs = timeoutInSec * 1000
                let readyOk = engine.WaitForReadyOk(timeoutInMs)

                if readyOk then
                    // The one readyok worth Information: the net is loaded.
                    logger.LogInformation("Engine {Engine} ready", engine.Name)
                    return true
                else
                    logger.LogError("Engine {Engine} timed out waiting for readyok", engine.Name)
                    return false

    with ex ->
        logger.LogWarning(ex, "Exception initializing engine {Engine}, performing cleanup", engine.Name)

        // Cleanup on failure
        try
            if not (engine.HasExited()) then
                engine.StopProcess()
        with cleanupEx ->
            logger.LogWarning(cleanupEx, "Error during cleanup for engine {Engine}", engine.Name)

        return false
}

let appendGameDescription (sb:StringBuilder) (tourny:Tournament) (player1:ChessEngine) (player2:ChessEngine) (openingMoves: ResizeArray<string>) fen =
    let append (txt:string) = sb.Append txt |> ignore
    let isEpd =
        match tourny.Opening.OpeningsPath with
        |Some path ->
            let ext = Path.GetExtension path
            ext.ToLower().Contains ".epd"
        |_ -> false
    let tcWhite = tourny.TimeControl.GetTimeConfig player1.Config.TimeControlID
    let tcBlack = tourny.TimeControl.GetTimeConfig player2.Config.TimeControlID
    let tournyData = "{TournamentOptions: " + tourny.PGNSummary() + (if isEpd then sprintf " FEN=%s;" fen else "")
    let moveOverheadMs = tourny.MoveOverhead.TotalMilliseconds
    let whiteEngineData = $" WhiteEngineOptions: TimeControl: {tcWhite.ToString()}; {player1.Config.Information moveOverheadMs}"
    let blackEngineData = $"BlackEngineOptions: TimeControl: {tcBlack.ToString()}; {player2.Config.Information moveOverheadMs}"
    let wCmds = if player1.IsLc0 then $" (White commands: {UciOptions.createCommandsFromConfig player1.Config})" else ""
    let bCmds = if player2.IsLc0 then $" (Black commands: {UciOptions.createCommandsFromConfig player2.Config})" else ""
    let whiteArgs, blackArgs = player1.Config.Args, player2.Config.Args
    append tournyData
    append whiteEngineData
    if String.IsNullOrEmpty whiteArgs |> not then append $" (Args: {whiteArgs})"
    append blackEngineData
    if String.IsNullOrEmpty blackArgs |> not then append $" (Args: {blackArgs})"
    append wCmds
    append bCmds
    append ("}" + Environment.NewLine)
    if openingMoves.Count > 0 then
        let opMoves = PGNWriter.writeOpeningPGNMoves openingMoves
        append opMoves

let checkAndPrepareContempt (engine1: ChessEngine) (engine2: ChessEngine) =
    let hasKeyCI (options: IDictionary<string, 'a>) (key: string) =
        options.Keys |> Seq.exists (fun k -> String.Equals(k, key, StringComparison.OrdinalIgnoreCase))

    if engine1.Config.ContemptEnabled then
        let ratingDiff = engine1.Config.Rating - engine2.Config.Rating
        if ratingDiff > 0 || engine1.Config.NegativeContemptAllowed then
            let options = engine1.GetDefaultOptions()
            if hasKeyCI options "Contempt" then
                let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
                engine1.AddSetOption engineOption
                printfn "Contempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
            elif hasKeyCI options "DynamicContempt" then
                let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
                engine1.AddSetOption engineOption
                printfn "DynamicContempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
        else
            printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine1.Name ratingDiff engine2.Name

    if engine2.Config.ContemptEnabled then
        let ratingDiff = engine2.Config.Rating - engine1.Config.Rating
        if ratingDiff > 0 || engine2.Config.NegativeContemptAllowed then
            let options = engine2.GetDefaultOptions()
            if hasKeyCI options "Contempt" then
                let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
                engine2.AddSetOption engineOption
                printfn "Contempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
            elif hasKeyCI options "DynamicContempt" then
                let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
                engine2.AddSetOption engineOption
                printfn "DynamicContempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
        else
            printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine2.Name ratingDiff engine1.Name

let initEngines openingDelayMs (tourny:Tournament) (engine1: ChessEngine) (engine2: ChessEngine) (logger: ILogger) =
    async {
        try
            do! Async.Sleep 200

            let delayBetweenGamesMs = tourny.DelayBetweenGames.TotalMilliseconds |> int

            //Pass logger to waitForEngineIsReady
            let startEngines = [
                waitForEngineIsReady tourny engine1 logger
                waitForEngineIsReady tourny engine2 logger
            ]
            let pauseUntilTournamentIsReady = [
                createAsyncDelay openingDelayMs
                createAsyncDelay delayBetweenGamesMs
            ]

            let! res =
                startEngines @ pauseUntilTournamentIsReady
                |> Async.Parallel

            //Only check engine results (first 2)
            let engineResults = res |> Array.take 2
            let failed = engineResults |> Array.exists(fun e -> not e)

            if failed then
                let failedEngines =
                    [| (engine1, engineResults.[0]); (engine2, engineResults.[1]) |]
                    |> Array.filter (fun (_, ok) -> not ok)
                    |> Array.map fst

                let engineNames = String.Join(", ", failedEngines |> Array.map (fun e -> e.Name))
                logger.LogCritical("Failed to start engines: {FailedEngines}", engineNames)
                raise (EngineStartupException($"Failed to start: {engineNames}"))

            else
                checkAndPrepareContempt engine1 engine2

        with ex ->
            // Log the error with context
            logger.LogError(ex, "Exception during initEngines for {White} vs {Black}", engine1.Name, engine2.Name)

            try
                if not (engine1.HasExited()) then
                    engine1.StopProcess()

                if not (engine2.HasExited()) then
                    engine2.StopProcess()
            with cleanupEx ->
                logger.LogWarning(cleanupEx, "Error during cleanup in initEngines")
                raise (EngineStartupException cleanupEx.Message)
            raise ex

    } |> Async.RunSynchronously
