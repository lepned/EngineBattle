namespace ChessLibrary

open System
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.WinboardProtocol
open ChessLibrary.Chess

/// Integration module for adding Winboard support to ChessEngine
///
/// This module provides two initialization strategies for Winboard engines:
///
/// 1. **initializeWinboard** - Direct stdout reading using ReadLineAsync
///    - Use when you want synchronous control over initialization
///    - Reads directly from process.StandardOutput
///    - Don't use with BeginOutputReadLine()
///    - Example: Standalone tools, testing, analysis scripts
///
/// 2. **initializeWinboardEventBased** - Event-based initialization
///    - Use when already using BeginOutputReadLine() for async output processing
///    - Polls handler state instead of reading stdout
///    - Works with OutputDataReceived event handler
///    - Example: ChessEngine classes, long-running engine processes
///
/// Both methods support:
/// - Automatic V1/V2 protocol detection
/// - ForceV1Mode for very old engines
/// - Feature negotiation with timeout
/// - Setboard probing for V1 engines
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

    /// Initialize Winboard engine using direct ReadLineAsync (supports both v1 and v2)
    ///
    /// **When to use:** Use this method when you control the process lifecycle and want direct control over stdout reading.
    /// This is typically used in standalone tools or when you want synchronous initialization without background event handlers.
    ///
    /// **How it works:**
    /// 1. Send xboard + protover 2 (unless ForceV1Mode is set)
    /// 2. Read output directly using ReadLineAsync until done=1 or timeout
    /// 3. If no features received → ForceV1Init (V1 engine detected)
    /// 4. Probe setboard support for V1 engines
    /// 5. Send post + easy commands
    ///
    /// **Parameters:**
    /// - proc: The engine process (must have stdout redirected)
    /// - handler: WinboardHandler instance for protocol translation
    /// - logger: Optional logger for diagnostic output
    /// - engineName: Engine name for logging
    /// - timeoutMs: Timeout in milliseconds for feature negotiation (default: 2000ms)
    /// - forceV1: If true, skip protover 2 and immediately use V1 mode
    ///
    /// **Thread safety:** This method reads from stdout directly, so don't use BeginOutputReadLine on the same process.
    let initializeWinboard (proc: Process) (handler: WinboardHandler) (logger: ILogger option) (engineName: string) (timeoutMs: int) (forceV1: bool) =
        async {
            try
                let log msg = logger |> Option.iter (fun l -> l.LogInformation(msg))
                let logDebug msg = logger |> Option.iter (fun l -> l.LogDebug(msg))
                let logWarn msg = logger |> Option.iter (fun l -> l.LogWarning(msg))
                let logCrit msg = logger |> Option.iter (fun l -> l.LogCritical(msg))

                // Check if V1 mode is forced via config
                if forceV1 then
                    log $"Winboard V1 mode forced for {engineName} (ForceV1Mode=true)"
                    proc.StandardInput.WriteLine("xboard")
                    handler.ForceV1Init()
                    for cmd in handler.GetPostInitCommands() do
                        proc.StandardInput.WriteLine(cmd)
                        logDebug $"Sent '{cmd}' to {engineName}"
                    return true
                else
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

                    // Step 4b: For AutoDetect strategy, probe level command support
                    if initResult && handler.ConfiguredTimeControlStrategy = TypesDef.CoreTypes.TimeControlStrategy.AutoDetect then
                        log $"Probing level command support for {engineName} (AutoDetect mode)"
                        proc.StandardInput.WriteLine("new")
                        proc.StandardInput.WriteLine("force")
                        proc.StandardInput.WriteLine("level 0 1 0")  // Simple test: 0 moves, 1 minute base, 0 increment
                        // Read briefly to check for error response
                        use probeCts = new CancellationTokenSource(TimeSpan.FromMilliseconds(500.0))
                        let mutable gotError = false
                        let rec drainProbe () = async {
                            try
                                let! line = proc.StandardOutput.ReadLineAsync(probeCts.Token).AsTask() |> Async.AwaitTask
                                if not (isNull line) && not (String.IsNullOrWhiteSpace line) then
                                    logDebug $"[WB level probe {engineName}] {line}"
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
                            handler.SetResolvedStrategy(TypesDef.CoreTypes.TimeControlStrategy.LevelWithTime)
                            log $"Engine {engineName} supports level command (probe succeeded, using LevelWithTime)"
                        else
                            handler.SetResolvedStrategy(TypesDef.CoreTypes.TimeControlStrategy.TimeOtimOnly)
                            logWarn $"Engine {engineName} does not support level command (probe returned error, using TimeOtimOnly)"

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
    ///
    /// **When to use:** Use this method when you're already using BeginOutputReadLine() for async event-based output processing.
    /// This is typically used in the main Engine classes where output is processed via the OutputDataReceived event.
    ///
    /// **How it works:**
    /// 1. If ForceV1Mode is set, immediately call ForceV1Init and return
    /// 2. Otherwise, wait for handler.IsInitialized to become true (set by event handler calling ProcessOutput)
    /// 3. Poll every 10ms with timeout to detect initialization completion
    /// 4. On timeout, apply V1 fallback or accept partial features
    ///
    /// **Parameters:**
    /// - handler: WinboardHandler instance (receives ProcessOutput calls from event handler)
    /// - logger: Optional logger for diagnostic output
    /// - engineName: Engine name for logging
    /// - timeoutMs: Timeout in milliseconds for initialization (default: 2000ms)
    /// - forceV1: If true, skip protover 2 and immediately use V1 mode
    ///
    /// **Important:** You must call BeginOutputReadLine() on the process BEFORE calling this method, and ensure
    /// the OutputDataReceived event handler calls handler.ProcessOutput(line) for each line received.
    ///
    /// **Thread safety:** This method only polls the handler state, doesn't read from stdout directly.
    /// Safe to use concurrently with BeginOutputReadLine().
    let initializeWinboardEventBased (handler: WinboardHandler) (logger: ILogger option) (engineName: string) (timeoutMs: int) (forceV1: bool) =
        async {
            try
                let log msg = logger |> Option.iter (fun l -> l.LogInformation(msg))
                let logDebug msg = logger |> Option.iter (fun l -> l.LogDebug(msg))
                let logWarn msg = logger |> Option.iter (fun l -> l.LogWarning(msg))

                // Check if V1 mode is forced via config
                if forceV1 then
                    log $"Winboard V1 mode forced for {engineName} (ForceV1Mode=true)"
                    handler.ForceV1Init()
                    return true
                else
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
            // Get WinboardConfig or use default
            let winboardConfig =
                match config.WinboardConfig with
                | Some wbConfig -> wbConfig
                | None -> TypesDef.CoreTypes.WinboardConfig.Default

            match logger with
            | Some log ->
                Some (WinboardHandler(log, config.Name, winboardConfig))
            | None ->
                Some (WinboardHandler(Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, config.Name, winboardConfig))
        else
            None
