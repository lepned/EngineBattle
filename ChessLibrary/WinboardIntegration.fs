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
    let private InitRetryDelayMs = 100
    
    /// Lightweight wrapper around ILogger option for consistent logging
    type private LoggerWrapper(logger: ILogger option) =
        member _.LogDebug(message: string) =
            logger |> Option.iter (fun log -> log.LogDebug(message))
        
        member _.LogInfo(message: string) =
            logger |> Option.iter (fun log -> log.LogInformation(message))
        
        member _.LogWarning(message: string) =
            logger |> Option.iter (fun log -> log.LogWarning(message))
        
        member _.LogError(message: string) =
            logger |> Option.iter (fun log -> log.LogError(message))
        
        member _.LogCritical(message: string) =
            logger |> Option.iter (fun log -> log.LogCritical(message))
    
    /// Create a concise log message for UCI commands (truncates long position commands)
    let private formatCommandForLogging (uciCommand: string) =
        if uciCommand.StartsWith("position") && uciCommand.Length > 100 then
            let movesIdx = uciCommand.IndexOf("moves")
            if movesIdx > 0 then
                let movesPart = uciCommand.Substring(movesIdx + 6)
                let moves = movesPart.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                $"position with {moves.Length} moves"
            else
                "position startpos"
        else
            uciCommand
    
    /// Check if engine config specifies Winboard protocol
    let isWinboardEngine (config: TypesDef.CoreTypes.EngineConfig) =
        match config.Protocol with
        | null | "" -> false
        | protocol -> 
            let p = protocol.ToLower()
            p = "winboard" || p = "xboard"
    
    /// Wrapper that adds Winboard translation to engine write operations
    type ProtocolWriter(proc: Process, handler: WinboardHandler option, logger: ILogger option, engineName: string) =
        
        let log = LoggerWrapper(logger)
        let mutable lastWasPosition = false
        
        /// Write a UCI command, translating to Winboard if needed
        member _.Write(uciCommand: string) =
            try
                if proc <> null && not proc.HasExited then
                    // If last command was position and this is go, add sync delay for engines without ping
                    if lastWasPosition && uciCommand.StartsWith("go") then
                        match handler with
                        | Some wb when wb.IsInitialized && not wb.Features.Ping ->
                            // The King and other v1 engines need time to process position before go
                            System.Threading.Thread.Sleep(250)  // 250ms delay
                            log.LogInfo $"Added 250ms sync delay before 'go' for {engineName} (no ping support)"
                        | _ -> ()
                    
                    lastWasPosition <- uciCommand.StartsWith("position")
                    
                    match handler with
                    | Some wb ->
                        // Translate UCI to Winboard
                        let winboardCmds = wb.UciToWinboard(uciCommand)
                        let logMsg = formatCommandForLogging uciCommand
                        
                        for cmd in winboardCmds do
                            log.LogDebug $"[UCI→Winboard] '{logMsg}' → '{cmd}' for {engineName}"
                            proc.StandardInput.WriteLine(cmd)
                    | None ->
                        // Normal UCI
                        log.LogDebug $"[UCI] Writing to {engineName}: {uciCommand}"
                        proc.StandardInput.WriteLine(uciCommand)
                else
                    log.LogWarning $"Attempted to write to disposed engine {engineName}"
            with ex ->
                log.LogError $"Error writing to engine {engineName}: {ex.Message}"
    
    /// Wrapper that translates Winboard output to UCI
    type ProtocolReader(handler: WinboardHandler option, logger: ILogger option, engineName: string) =
        
        let log = LoggerWrapper(logger)
        
        /// Process a line from the engine, translating Winboard to UCI if needed
        member _.ProcessLine(line: string) : string option =
            if String.IsNullOrWhiteSpace(line) then
                None
            else
                match handler with
                | Some wb ->
                    // Translate Winboard to UCI
                    let translated = wb.ProcessOutput(line)
                    match translated with
                    | Some uciLine ->
                        log.LogDebug $"[Winboard→UCI] '{line}' → '{uciLine}' from {engineName}"
                        Some uciLine
                    | None ->
                        // Not translated, but might be informational
                        log.LogDebug $"[Winboard] {engineName}: {line}"
                        None
                | None ->
                    // Normal UCI - return as-is
                    Some line

    
    /// Initialize Winboard engine (supports both v1 and v2)
    let initializeWinboard (proc: Process) (handler: WinboardHandler) (logger: ILogger option) (engineName: string) (timeoutMs: int) =
        async {
            try
                use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float timeoutMs))
                let log = LoggerWrapper(logger)
                
                // Send xboard and protover 2
                let initCmds = handler.Initialize()
                for cmd in initCmds do
                    proc.StandardInput.WriteLine(cmd)
                    log.LogDebug $"Winboard init: {cmd}"
                
                // Wait for feature negotiation (v2) or timeout for v1 engines
                let rec waitForInit attempts =
                    async {
                        if handler.IsInitialized then
                            log.LogInfo $"Winboard v2 engine {engineName} initialized"
                            return true
                        elif cts.Token.IsCancellationRequested then
                            log.LogCritical $"Winboard engine {engineName} initialization timed out"
                            return false
                        elif attempts >= MaxInitAttempts then
                            // After ~2 seconds with no feature response, assume v1 engine
                            // BUT: if handler has already received features (even without 'done=1'),
                            // don't force-initialize as that would overwrite them!
                            if not handler.IsInitialized && handler.Features = defaultFeatures then
                                log.LogWarning $"Winboard v1 engine {engineName} detected (no protover 2 support)"
                                log.LogInfo $"Using default Winboard v1 feature set for {engineName}"
                                // Mark as initialized with basic v1 features
                                handler.ForceInitialize()
                            else
                                // Handler has partial features but no 'done=1' - accept what we have
                                log.LogWarning $"Winboard v2 engine {engineName} didn't send 'done=1', accepting partial features"
                                log.LogDebug $"Features received: Time={handler.Features.Time}, Analyze={handler.Features.Analyze}"
                                // Manually mark as initialized without overwriting features
                                handler.MarkInitialized()
                            return true
                        else
                            do! Async.Sleep(InitRetryDelayMs)
                            return! waitForInit (attempts + 1)
                    }
                
                let! initResult = waitForInit 0
                
                // After successful initialization, send 'post' to enable thinking output
                if initResult then
                    // Give engines time to finish startup banners before probing.
                    Thread.Sleep(250)

                    // Run cautious probes to detect command support before EngineBattle sends commands.
                    let includeFeatureProbes = handler.IsV1Fallback
                    let probes = handler.GetProbePlan(includeFeatureProbes)
                    if probes.Length > 0 then
                        for probe in probes do                            
                            log.LogDebug $"Winboard probe start: {probe.Name}"

                            // Activate probe before sending commands so we can capture immediate errors.
                            let probeAsync = handler.BeginProbe probe

                            // Ping probe needs a unique ping id.
                            match probe.Kind with
                            | WinboardProtocol.ProbeKind.Ping ->
                                match handler.GetProbePingCommand() with
                                | Some pingCmd -> proc.StandardInput.WriteLine(pingCmd)
                                | None -> ()
                            | _ ->
                                for cmd in probe.Commands do
                                    proc.StandardInput.WriteLine(cmd)

                            let! ok = probeAsync

                            // Cleanup commands regardless of result.
                            for cmd in probe.CleanupCommands do
                                proc.StandardInput.WriteLine(cmd)
                            
                            if ok then
                                log.LogDebug $"Winboard probe ok: {probe.Name}"
                            else
                                log.LogDebug $"Winboard probe failed: {probe.Name}"

                    if handler.SupportsPostCmd then
                        proc.StandardInput.WriteLine("post")
                        log.LogDebug $"Sent 'post' command to enable thinking output for {engineName}"
                        
                    if handler.Features.EasyCmd then
                        proc.StandardInput.WriteLine("easy")
                        log.LogDebug $"Sent 'easy' command to disable pondering for {engineName}"
                
                return initResult
            with ex ->
                let log = LoggerWrapper(logger)
                log.LogError $"Error initializing Winboard engine {engineName}: {ex.Message}"
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
