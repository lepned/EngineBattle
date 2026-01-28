namespace ChessLibrary

open System
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.WinboardProtocol
open ChessLibrary.Chess

/// Integration module for adding Winboard support to ChessEngine
module WinboardIntegration =

    // Constants
    [<Literal>]
    let private MaxInitAttempts = 20
    [<Literal>]
    let private FeatureTimeoutMs = 2000

    /// Check if engine config specifies Winboard protocol
    let isWinboardEngine (config: TypesDef.CoreTypes.EngineConfig) =
        match config.Protocol with
        | null | "" -> false
        | protocol ->
            let p = protocol.ToLower()
            p = "winboard" || p = "xboard"

    /// Initialize Winboard engine (supports both v1 and v2)
    /// 1. Send xboard + protover 2
    /// 2. Read output until done=1 or timeout
    /// 3. If no features at all → ForceV1Init
    /// 4. Send post + easy
    let initializeWinboard (proc: Process) (handler: WinboardHandler) (logger: ILogger option) (engineName: string) (timeoutMs: int) =
        async {
            try
                let log msg = logger |> Option.iter (fun l -> l.LogInformation(msg))
                let logDebug msg = logger |> Option.iter (fun l -> l.LogDebug(msg))
                let logWarn msg = logger |> Option.iter (fun l -> l.LogWarning(msg))
                let logCrit msg = logger |> Option.iter (fun l -> l.LogCritical(msg))

                // Step 1: Send xboard + protover 2
                let initCmds = handler.GetInitCommands()
                for cmd in initCmds do
                    proc.StandardInput.WriteLine(cmd)
                    logDebug $"Winboard init: {cmd}"

                // Step 2: Wait for feature negotiation (done=1) or timeout
                // Read directly with cancellable ReadLineAsync — no background task needed
                use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float timeoutMs))
                let rec waitForInit attempts =
                    async {
                        if handler.IsInitialized then
                            log $"Winboard v2 engine {engineName} initialized"
                            return true
                        elif cts.Token.IsCancellationRequested then
                            // Step 3: No done=1 received within timeout
                            if handler.Features = defaultFeatures then
                                logWarn $"Winboard v1 engine {engineName} detected (no protover 2 support)"
                                handler.ForceV1Init()
                            else
                                logWarn $"Winboard v2 engine {engineName} didn't send done=1, accepting partial features"
                                handler.MarkInitialized()
                            return true
                        elif proc.HasExited then
                            logCrit $"Winboard engine {engineName} exited during init"
                            return false
                        else
                            try
                                let! line = proc.StandardOutput.ReadLineAsync(cts.Token).AsTask() |> Async.AwaitTask
                                if not (isNull line) && not (String.IsNullOrWhiteSpace line) then
                                    logDebug $"[WB init {engineName}] {line}"
                                    handler.ProcessOutput(line) |> ignore
                                return! waitForInit (attempts + 1)
                            with
                            | :? OperationCanceledException ->
                                // Timeout — apply V1 fallback
                                if handler.Features = defaultFeatures then
                                    logWarn $"Winboard v1 engine {engineName} detected (no protover 2 support)"
                                    handler.ForceV1Init()
                                else
                                    logWarn $"Winboard v2 engine {engineName} didn't send done=1, accepting partial features"
                                    handler.MarkInitialized()
                                return true
                            | :? System.AggregateException as agg when (agg.InnerException :? OperationCanceledException) ->
                                if handler.Features = defaultFeatures then
                                    logWarn $"Winboard v1 engine {engineName} detected (no protover 2 support)"
                                    handler.ForceV1Init()
                                else
                                    logWarn $"Winboard v2 engine {engineName} didn't send done=1, accepting partial features"
                                    handler.MarkInitialized()
                                return true
                    }

                let! initResult = waitForInit 0

                // Step 4: For V1 engines, probe setboard support (e.g. TheKing)
                if initResult && handler.IsV1Fallback && not handler.Features.SetBoard then
                    log $"Probing setboard support for V1 engine {engineName}"
                    proc.StandardInput.WriteLine("new")
                    proc.StandardInput.WriteLine("force")
                    proc.StandardInput.WriteLine("setboard rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                    // Read briefly to check for error response
                    use probeCts = new CancellationTokenSource(TimeSpan.FromMilliseconds(500.0))
                    let mutable gotError = false
                    let rec drainProbe () = async {
                        try
                            let! line = proc.StandardOutput.ReadLineAsync(probeCts.Token).AsTask() |> Async.AwaitTask
                            if not (isNull line) && not (String.IsNullOrWhiteSpace line) then
                                logDebug $"[WB setboard probe {engineName}] {line}"
                                let trimmed = line.Trim()
                                if trimmed.StartsWith("Error") || trimmed.StartsWith("Illegal") || trimmed.Contains("unknown command") then
                                    gotError <- true
                                elif not gotError then
                                    return! drainProbe ()
                        with
                        | :? OperationCanceledException -> ()
                        | :? System.AggregateException as agg when (agg.InnerException :? OperationCanceledException) -> ()
                    }
                    do! drainProbe ()
                    if not gotError then
                        handler.EnableSetBoard()
                        log $"V1 engine {engineName} supports setboard (probe succeeded)"
                    else
                        logWarn $"V1 engine {engineName} does not support setboard (probe returned error)"

                // Step 5: Send post + easy
                if initResult then
                    for cmd in handler.GetPostInitCommands() do
                        proc.StandardInput.WriteLine(cmd)
                        logDebug $"Sent '{cmd}' to {engineName}"

                return initResult
            with ex ->
                logger |> Option.iter (fun l -> l.LogError($"Error initializing Winboard engine {engineName}: {ex.Message}"))
                return false
        }

    /// Initialize Winboard engine using event-based reading (for use with BeginOutputReadLine)
    /// This version doesn't read from stdout directly; it relies on the event handler to call ProcessOutput
    let initializeWinboardEventBased (handler: WinboardHandler) (logger: ILogger option) (engineName: string) (timeoutMs: int) =
        async {
            try
                let log msg = logger |> Option.iter (fun l -> l.LogInformation(msg))
                let logDebug msg = logger |> Option.iter (fun l -> l.LogDebug(msg))
                let logWarn msg = logger |> Option.iter (fun l -> l.LogWarning(msg))

                logDebug $"Waiting for {engineName} initialization (timeout: {timeoutMs}ms)"

                // Simple polling approach - check IsInitialized flag every 10ms
                let startTime = System.DateTime.UtcNow
                let mutable elapsed = 0
                let pollInterval = 10

                while not handler.IsInitialized && elapsed < timeoutMs do
                    do! Async.Sleep pollInterval
                    elapsed <- int (System.DateTime.UtcNow - startTime).TotalMilliseconds
                    if elapsed % 500 = 0 then
                        logDebug $"[{engineName}] Still waiting... ({elapsed}ms elapsed)"

                logDebug $"Wait completed for {engineName} after {elapsed}ms, initialized={handler.IsInitialized}"

                if handler.IsInitialized then
                    // Initialization completed before timeout
                    log $"Winboard v2 engine {engineName} initialized"
                    return true
                else
                    // Timeout: apply V1 fallback or accept partial features
                    if handler.Features = defaultFeatures then
                        logWarn $"Winboard v1 engine {engineName} detected (no protover 2 support)"
                        handler.ForceV1Init()
                    else
                        logWarn $"Winboard v2 engine {engineName} didn't send done=1, accepting partial features"
                        handler.MarkInitialized()
                    return true
            with ex ->
                logger |> Option.iter (fun l -> l.LogError($"Error initializing Winboard engine {engineName}: {ex.Message}"))
                return false
        }

    /// Create a Winboard handler if the config specifies Winboard protocol
    let createHandlerIfNeeded (config: TypesDef.CoreTypes.EngineConfig) (logger: ILogger option) =
        if isWinboardEngine config then
            match logger with
            | Some log ->
                Some (WinboardHandler(log, config.Name, config.Flipped))
            | None ->
                Some (WinboardHandler(Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, config.Name, config.Flipped))
        else
            None
