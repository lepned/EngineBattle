namespace ChessLibrary

open System
open System.Text.RegularExpressions
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core.Operators.Unchecked
open TypesDef.TMove
open TypesDef.Position
open TypesDef.CoreTypes
open Chess
open Chess.BoardUtils

/// Winboard/XBoard protocol implementation for EngineBattle
module WinboardProtocol =

    // Constants
    [<Literal>]
    let private DepthIterationThreshold = 1000
    [<Literal>]
    let private PingIdMin = 10000
    [<Literal>]
    let private PingIdMax = 99999
    [<Literal>]
    let private ProbeTimeoutMs = 200
    [<Literal>]
    let private PingProbeTimeoutMs = 400
    [<Literal>]
    let private AnalyzeProbeTimeoutMs = 300
    [<Literal>]
    let private UserMoveProbeTimeoutMs = 250
    [<Literal>]
    let private SetboardProbeTimeoutMs = 250

    // Cached regex patterns
    let private thinkingOutputRegex = Regex(@"^\d+\s+[+-]?\d+\s+\d+\s+\d+", RegexOptions.Compiled)
    let private wtimeRegex = Regex(@"wtime (\d+)", RegexOptions.Compiled)
    let private btimeRegex = Regex(@"btime (\d+)", RegexOptions.Compiled)
    let private movetimeRegex = Regex(@"movetime (\d+)", RegexOptions.Compiled)
    let private setOptionRegex = Regex(@"^setoption\s+name\s+(.+?)(?:\s+value\s+(.+))?$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let private coordinateNotationRegex = Regex(@"^[a-h][1-8][a-h][1-8][qrbn]?$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let private moveNotationStartRegex = Regex(@"^[a-h][1-8][a-h][1-8]", RegexOptions.Compiled)

    /// Winboard engine feature support state
    type FeatureState = {
        Ping: bool
        SetBoard: bool
        PlayOther: bool
        San: bool
        UserMove: bool
        Time: bool
        TimeCmd: bool
        OTimeCmd: bool
        ForceCmd: bool
        WhiteCmd: bool
        BlackCmd: bool
        PostCmd: bool
        MoveNowCmd: bool
        ExitCmd: bool
        OptionCmd: bool
        Sigint: bool
        EasyCmd: bool
        HardCmd: bool
        Sigterm: bool
        Reuse: bool
        Analyze: bool
        MyName: string option
        Variants: string option
        Colors: bool
        Ics: bool
        Name: bool
        Pause: bool
        Done: bool
        SetBoardNeedsFourFieldFen: bool
    }

    type ProbeKind =
        | Ping
        | SetBoard
        | UserMove
        | Analyze
        | TimeCmd
        | OTimeCmd
        | ForceCmd
        | WhiteCmd
        | BlackCmd
        | PostCmd
        | MoveNowCmd
        | ExitCmd
        | EasyCmd
        | HardCmd

    type ProbeRequest = {
        Name: string
        Kind: ProbeKind
        Commands: string list
        CleanupCommands: string list
        TimeoutMs: int
        AssumeSuccessOnSilence: bool
        TreatIllegalAsSuccess: bool
        OnSuccess: FeatureState -> FeatureState
    }
    
    /// Default Winboard features (all disabled until negotiated)
    let defaultFeatures = {
        Ping = false
        SetBoard = false
        PlayOther = false
        San = false
        UserMove = false
        Time = false
        TimeCmd = false
        OTimeCmd = false
        ForceCmd = false
        WhiteCmd = false
        BlackCmd = false
        PostCmd = false
        MoveNowCmd = false
        ExitCmd = false
        OptionCmd = false
        EasyCmd = false
        HardCmd = false
        Sigint = false
        Sigterm = false
        Reuse = false
        Analyze = false
        MyName = None
        Variants = None
        Colors = false
        Ics = false
        Name = false
        Pause = false
        Done = false
        SetBoardNeedsFourFieldFen = false
    }
    
    /// Strip halfmove and fullmove counters from FEN for engines that can't parse them
    /// Full FEN: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    /// 4-field FEN: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"
    let stripFenMoveCounters (fen: string) =
        let parts = fen.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length >= 4 then
            // Return first 4 fields only (piece placement, side to move, castling, en passant)
            String.Join(" ", parts.[0..3])
        else
            fen
    
    /// Parse a single feature line from Winboard engine
    /// Example: "feature ping=1 setboard=1 done=1"
    let parseFeature (line: string) (features: FeatureState) =
        let parts = line.Replace("feature ", "").Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let mutable updatedFeatures = features
        
        for part in parts do
            let kv = part.Split('=')
            if kv.Length = 2 then
                let key = kv.[0].ToLower()
                let value = kv.[1].Trim('"')
                
                updatedFeatures <-
                    match key with
                    | "ping" -> { updatedFeatures with Ping = value = "1" }
                    | "setboard" -> { updatedFeatures with SetBoard = value = "1" }
                    | "playother" -> { updatedFeatures with PlayOther = value = "1" }
                    | "san" -> { updatedFeatures with San = value = "1" }
                    | "usermove" -> { updatedFeatures with UserMove = value = "1" }
                    | "time" -> 
                        let timeSupported = value = "1"
                        { updatedFeatures with Time = timeSupported }
                    | "sigint" -> { updatedFeatures with Sigint = value = "1" }
                    | "sigterm" -> { updatedFeatures with Sigterm = value = "1" }
                    | "reuse" -> { updatedFeatures with Reuse = value = "1" }
                    | "analyze" -> { updatedFeatures with Analyze = value = "1" }
                    | "myname" -> { updatedFeatures with MyName = Some value }
                    | "variants" -> { updatedFeatures with Variants = Some value }
                    | "colors" -> { updatedFeatures with Colors = value = "1" }
                    | "ics" -> { updatedFeatures with Ics = value = "1" }
                    | "name" -> { updatedFeatures with Name = value = "1" }
                    | "pause" -> { updatedFeatures with Pause = value = "1" }
                    | "done" -> { updatedFeatures with Done = value = "1" }
                    | _ -> updatedFeatures
        
        // Infer command support from feature flags to reduce probing and side effects.
        if updatedFeatures.Time then
            updatedFeatures <- { updatedFeatures with TimeCmd = true; OTimeCmd = true }
        if updatedFeatures.Analyze then
            updatedFeatures <- { updatedFeatures with ExitCmd = true }

        updatedFeatures
    
    /// Convert UCI position command to Winboard commands
    /// UCI: "position startpos moves e2e4 e7e5"
    /// Winboard: "force" "new" "e2e4" "e7e5"
    /// OR: "force" "setboard <fen>" followed by moves (if setboard supported)
    let positionToWinboard (command: string) (features: FeatureState) (supportsForce: bool) =
        let commands = ResizeArray<string>()
        let mutable usedForce = false
        
        // Check if this is the starting position FEN (used in analysis mode)
        let startingPosFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let isStartingPos = command.Contains(startingPosFen)
        
        if command.StartsWith("position startpos") then
            // Extract moves if any
            let movesIdx = command.IndexOf("moves")
            let hasMoves = movesIdx >= 0
            
            // Always start a new game from the standard initial position
            commands.Add("new")

            // Enter force mode after "new" so the engine doesn't try to play while we replay moves
            if hasMoves then
                if supportsForce then
                    commands.Add("force")
                    usedForce <- true
            
            if hasMoves then
                let movesStr = command.Substring(movesIdx + 6)
                let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                for move in moves do
                    if features.UserMove then
                        commands.Add($"usermove {move}")
                    else
                        commands.Add(move)
        
        
        elif command.StartsWith("position fen") then
            let fenIdx = command.IndexOf("fen")
            let movesIdx = command.IndexOf("moves")
            
            // Extract FEN carefully - need full FEN including move counters
            let fen = 
                if fenIdx >= 0 then
                    let fenStart = fenIdx + 4
                    if movesIdx >= 0 && movesIdx > fenStart then
                        command.Substring(fenStart, movesIdx - fenStart).Trim()
                    else
                        command.Substring(fenStart).Trim()
                else
                    ""
            
            let hasMoves = movesIdx >= 0 && movesIdx + 6 < command.Length
            
            // Only enter force mode if:
            // 1. We have moves to apply, OR
            // 2. It's not the starting position (non-standard position needs force+new or setboard)
            let needForceMode = hasMoves || not isStartingPos
            
            if features.SetBoard && not (String.IsNullOrWhiteSpace(fen)) && not isStartingPos then
                // Use setboard for non-standard positions
                if needForceMode && supportsForce then
                    commands.Add("force")
                    usedForce <- true
                commands.Add($"setboard {fen}")
            else
                // For starting position or engines without setboard, use "new"
                commands.Add("new")
                if needForceMode && supportsForce then
                    // "new" resets force mode on many engines; re-enter after it
                    commands.Add("force")
                    usedForce <- true
            
            // Apply moves if present
            if hasMoves then
                let movesStr = command.Substring(movesIdx + 6).Trim()
                if not (String.IsNullOrWhiteSpace(movesStr)) then
                    let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    for move in moves do
                        if features.UserMove then
                            commands.Add($"usermove {move}")
                        else
                            commands.Add(move)
        
        commands |> Seq.toList, usedForce
    
    /// Convert UCI go command to Winboard commands
    /// UCI: "go wtime 300000 btime 300000 winc 0 binc 0"
    /// Winboard: "time 30000" "otim 30000" "go"
    /// UCI: "go infinite"
    /// Winboard: "analyze" (if supported) or "go"
    let goToWinboard (command: string) (features: FeatureState) (isWhite: bool) (logger: ILogger option) (supportsTime: bool) (supportsOTime: bool) (supportsWhite: bool) (supportsBlack: bool) =
        let commands = ResizeArray<string>()
        
        // Parse time controls from UCI command using cached regexes
        let wtimeMatch = wtimeRegex.Match(command)
        let btimeMatch = btimeRegex.Match(command)
        let movetimeMatch = movetimeRegex.Match(command)
        
        // Check for infinite analysis
        let isInfinite = command.Contains("infinite")
        
        if isInfinite then
            if features.Analyze then
                if isWhite then
                    if supportsWhite then
                        commands.Add("white")
                else
                    if supportsBlack then
                        commands.Add("black")
                commands.Add("analyze")
            else
                if isWhite then
                    if supportsWhite then
                        commands.Add("white")
                else
                    if supportsBlack then
                        commands.Add("black")
                commands.Add("go")
        else
            // Timed search - use time/otim for ALL time controls
            // Most Winboard engines support these basic commands
            if wtimeMatch.Success && btimeMatch.Success then
                // Tournament mode: time remaining for both sides
                // milliseconds → centiseconds
                let wtime = Int32.Parse(wtimeMatch.Groups.[1].Value) / 10
                let btime = Int32.Parse(btimeMatch.Groups.[1].Value) / 10
                
                // Send time for side to move, otim for opponent
                if isWhite then
                    if supportsTime then
                        commands.Add($"time {wtime}")
                    if supportsOTime then
                        commands.Add($"otim {btime}")
                else
                    if supportsTime then
                        commands.Add($"time {btime}")
                    if supportsOTime then
                        commands.Add($"otim {wtime}")
            elif movetimeMatch.Success then
                // Fixed-time search: convert movetime directly to centiseconds
                // Winboard 'time' command is the time available for this move
                let timeMs = Int32.Parse(movetimeMatch.Groups.[1].Value)
                let centiseconds = timeMs / 10
                if supportsTime then
                    commands.Add($"time {centiseconds}")
                if supportsOTime then
                    commands.Add($"otim {centiseconds}")
            
            // Always end with "go" command
            commands.Add("go")
        
        commands |> Seq.toList    
    /// Parse Winboard thinking output to UCI info format
    /// Winboard: "depth score time nodes pv..."
    /// Example: "9 12 1000 100000 e2e4 e7e5 Nf3"
    /// Some engines use format like "4001" where depth=4, iteration=001
    /// UCI: "info depth 9 score cp 12 time 10000 nodes 100000 pv e2e4 e7e5 Nf3"
    let parseThinkingOutput (flipped: bool) (currentBoard: Chess.Board) (engineName: string) (line: string) =
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        
        if parts.Length >= 4 then
            try
                let depthRaw = Int32.Parse(parts.[0])
                // Handle engines that encode iteration in depth (e.g., 4001 = depth 4, iteration 1)
                let depth = if depthRaw > DepthIterationThreshold then depthRaw / 1000 else depthRaw
                
                let score = Int32.Parse(parts.[1])
                let time = Int32.Parse(parts.[2])  // centiseconds
                let nodes = Int64.Parse(parts.[3])
                
                // Winboard protocol standard: scores are from side-to-move's perspective
                // UCI standard: scores are from White's perspective
                // Convert: if Black to move, negate the score
                // Exception: A few engines (like "the king", "gnuchess") report from White's perspective already
                let isBlackToMove = currentBoard.Position.STM <> 0uy                
                let reportsFromWhitePerspective = flipped
                
                let adjustedScore = 
                    if reportsFromWhitePerspective then
                        score  // Already in UCI format (White's perspective)
                    elif isBlackToMove then
                        -score  // Convert from Black's perspective to White's perspective
                    else
                        score  // White to move, score is already from White's perspective
                
                // Convert PV from SAN to coordinate notation using the current board state
                let pv = 
                    if parts.Length > 4 then 
                        let sanPv = String.Join(" ", parts.[4..])
                        // Create a temporary board at the current position to parse PV
                        let tempBoard = Chess.Board()
                        tempBoard.LoadFen(currentBoard.FEN())
                        tempBoard.IsFRC <- currentBoard.IsFRC
                        let sanPVline = sanPv.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        getLongSanPVFromShortSanPV moveList.Value &tempBoard sanPVline
                    else 
                        ""
                
                // Calculate NPS (nodes per second)
                // time is in centiseconds, so divide by 100 to get seconds
                let nps = if time > 0 then int64 ((float nodes) / (float time / 100.0)) else 0L
                
                // UCI wants time in milliseconds (centiseconds * 10)
                Some $"info depth {depth} score cp {adjustedScore} time {time * 10} nodes {nodes} nps {nps} pv {pv}"
            with
            | _ -> None
        else
            None
    
    /// Parse Winboard move output to UCI bestmove, handling both coordinate and SAN
    /// Winboard: "move e2e4" / "e2e4" / "move cxd5"
    /// UCI: "bestmove e2e4"
    let tryParseMoveOutput (board: Chess.Board) (line: string) =
        let moveStr = 
            if line.StartsWith("move ") then
                line.Substring(5).Trim()
            else
                line.Trim()
        
        // Extract move (ignore ponder for now)
        let parts = moveStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let move = parts.[0]
        
        if String.IsNullOrWhiteSpace(move) then
            None
        elif coordinateNotationRegex.IsMatch(move) then
            Some $"bestmove {move.ToLower()}"
        else
            // Try to convert SAN (e.g., cxd5, Nf3, O-O) to coordinate notation
            let longSan = BoardUtils.getLongSanPVFromShortSanPV moveList.Value &board [move]
            if String.IsNullOrWhiteSpace(longSan) then
                None
            else
                let converted = longSan.Split([|' '|], StringSplitOptions.RemoveEmptyEntries).[0]
                Some $"bestmove {converted}"
    
    /// Check if a line looks like a move in coordinate notation
    let isMoveNotation (line: string) =
        if String.IsNullOrWhiteSpace(line) then false
        elif line.StartsWith("move ") then true
        elif line.Length >= 4 then
            let moveCandidate = line.Substring(0, min 4 line.Length)
            moveNotationStartRegex.IsMatch(moveCandidate)
        else
            false

    /// Winboard protocol handler - manages state and translation
    type WinboardHandler(logger: ILogger, configuredEngineName: string, flipped: bool) =         
        
        //let moveList = Array.init 256 (fun _ -> defaultof<TMove>)
        let probeLock = obj()  // Thread safety for probe operations
        let mutable features = defaultFeatures
        let mutable isInitialized = false
        let mutable isV1Fallback = false
        let mutable pendingPing = None
        let mutable board = new Board()  // Track current board state
        let mutable moveCount = 0
        let mutable inAnalyzeMode = false  // Track if we're in analyze mode
        let mutable positionUsedForce = false
        let mutable activeProbe : (ProbeRequest * int option * System.Threading.Tasks.TaskCompletionSource<bool>) option = None
        
        member _.Features = features
        member _.IsInitialized = isInitialized
        member _.IsV1Fallback = isV1Fallback
        member _.SupportsTimeCmd = features.TimeCmd
        member _.SupportsOTimeCmd = features.OTimeCmd
        member _.SupportsForceCmd = features.ForceCmd
        member _.SupportsWhiteCmd = features.WhiteCmd
        member _.SupportsBlackCmd = features.BlackCmd
        member _.SupportsPostCmd = features.PostCmd
        member _.SupportsMoveNowCmd = features.MoveNowCmd
        member _.SupportsExitCmd = features.ExitCmd
        member _.SupportsOptionCmd = features.OptionCmd
        // Derive isWhiteToMove from actual board state (STM = 0 means white)
        member _.IsWhiteToMove = board.Position.STM = 0uy
        member _.CurrentFen = board.FEN()  // Use FEN() method instead of CurrentFEN property
        member _.Board = board
        member _.ConfiguredName = configuredEngineName
        
        
        /// Get initialization commands for Winboard protocol
        member _.Initialize() =
            [
                "xboard"
                "protover 2"
            ]
        
        /// Process a line of output from Winboard engine
        /// Returns Some(uci_line) if translation is needed, None otherwise
        member this.ProcessOutput(line: string) =
            if String.IsNullOrWhiteSpace(line) then
                None
            else
                
                // Trim the line to handle leading/trailing whitespace
                let trimmedLine = line.Trim()

                let isProbeErrorLine (text: string) =
                    let lower = text.ToLowerInvariant()
                    lower.StartsWith("error")
                    || lower.StartsWith("illegal")
                    || lower.Contains("unknown command")
                    || lower.Contains("not supported")
                    || lower.Contains("unsupported")

                let tryCompleteProbe (success: bool) =
                    lock probeLock (fun () ->
                        match activeProbe with
                        | Some (probe, _, tcs) ->
                            activeProbe <- None
                            if success then
                                features <- probe.OnSuccess features
                                logger.LogDebug($"Winboard probe '{probe.Name}' succeeded; updated features.")
                            else
                                logger.LogWarning($"Winboard probe '{probe.Name}' failed.")
                            tcs.TrySetResult(success) |> ignore
                            true
                        | None -> false
                    )
                
                // Probe handling: detect error/success while probing
                let probeConsumed =
                    lock probeLock (fun () ->
                        match activeProbe with
                        | Some (probe, pingIdOpt, _) ->
                            match probe.Kind with
                            | Ping ->
                                if trimmedLine.StartsWith("pong", StringComparison.OrdinalIgnoreCase) then
                                    let pongId =
                                        let parts = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                                        if parts.Length > 1 then
                                            match Int32.TryParse(parts.[1]) with
                                            | true, id -> Some id
                                            | _ -> None
                                        else None
                                    if pingIdOpt.IsNone || pingIdOpt = pongId then
                                        ignore (tryCompleteProbe true)
                                        true
                                    else
                                        false
                                else
                                if isProbeErrorLine trimmedLine then
                                    let isIllegal = trimmedLine.StartsWith("Illegal", StringComparison.OrdinalIgnoreCase)
                                    if probe.TreatIllegalAsSuccess && isIllegal then
                                        ignore (tryCompleteProbe true)
                                    else
                                        logger.LogWarning($"Winboard probe error output: {trimmedLine}")
                                        ignore (tryCompleteProbe false)
                                    true
                                else
                                    false
                            | _ ->
                                if isProbeErrorLine trimmedLine then
                                    let isIllegal = trimmedLine.StartsWith("Illegal", StringComparison.OrdinalIgnoreCase)
                                    if probe.TreatIllegalAsSuccess && isIllegal then
                                        ignore (tryCompleteProbe true)
                                    else
                                        logger.LogWarning($"Winboard probe error output: {trimmedLine}")
                                        ignore (tryCompleteProbe false)
                                    true
                                else
                                    false
                        | None -> 
                            false
                    )

                if probeConsumed then
                    None
                elif trimmedLine.StartsWith("feature") then
                    features <- parseFeature trimmedLine features
                    if features.Done then
                        isInitialized <- true
                        let opt = features.MyName |> Option.defaultValue "Unknown"
                        logger.LogInformation($"Winboard engine features negotiated: {opt}")
                    None
                elif trimmedLine.StartsWith("pong") then
                    let pongId =
                        let parts = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                        if parts.Length > 1 then
                            match Int32.TryParse(parts.[1]) with
                            | true, id -> Some id
                            | _ -> None
                        else None
                    match pendingPing with
                    | Some id when pongId = Some id || pongId.IsNone ->
                        pendingPing <- None
                        Some "readyok"
                    | _ ->
                        None
                elif trimmedLine.StartsWith("move ") || isMoveNotation trimmedLine then
                    // Engine is sending its move - translate to UCI format (coordinate or SAN)
                    // Don't try to track it on our internal board, as the position command
                    // will give us the correct state for the next move
                    match tryParseMoveOutput board trimmedLine with
                    | Some uciMove -> Some uciMove
                    | None ->
                        logger.LogWarning($"Winboard move not understood: {trimmedLine}")
                        None
                elif trimmedLine.StartsWith("option ", StringComparison.OrdinalIgnoreCase) then
                    if not features.OptionCmd then
                        features <- { features with OptionCmd = true }
                        logger.LogDebug($"Winboard option support detected for {configuredEngineName}.")
                    None
                elif thinkingOutputRegex.IsMatch(trimmedLine) then
                    // Thinking output: depth score time nodes [pv...]
                    // Use configured engine name for score perspective detection (works for v1 engines too)
                    parseThinkingOutput flipped board configuredEngineName trimmedLine
                elif trimmedLine.StartsWith("Error") || trimmedLine.StartsWith("Illegal") then
                    let lower = trimmedLine.ToLowerInvariant()
                    let mutable updated = false
                    let markFalse (flagName: string) setter =
                        if lower.Contains(flagName) then
                            features <- setter features
                            updated <- true
                    // Downgrade capabilities when the engine reports unknown/illegal commands.
                    markFalse "usermove" (fun f -> { f with UserMove = false })
                    // Check if error is a setboard FEN parsing issue (numeric command error)
                    if lower.Contains("unknown command") then
                        let errorParts = trimmedLine.Split([|':'|], StringSplitOptions.RemoveEmptyEntries)
                        if errorParts.Length > 1 then
                            let cmdPart = errorParts.[1].Trim()
                            // If the "unknown command" is just a number, it's likely from FEN parsing
                            match System.Int32.TryParse(cmdPart) with
                            | true, _ when features.SetBoard ->
                                features <- { features with SetBoardNeedsFourFieldFen = true }
                                logger.LogInformation($"Winboard engine {configuredEngineName} needs 4-field FEN for setboard (detected numeric error)")
                                updated <- true
                            | _ -> ()
                    markFalse "setboard" (fun f -> { f with SetBoard = false })
                    markFalse "analyze" (fun f -> { f with Analyze = false; ExitCmd = false })
                    markFalse "ping" (fun f -> { f with Ping = false })
                    markFalse "time" (fun f -> { f with TimeCmd = false })
                    markFalse "otim" (fun f -> { f with OTimeCmd = false })
                    markFalse "force" (fun f -> { f with ForceCmd = false })
                    markFalse "white" (fun f -> { f with WhiteCmd = false })
                    markFalse "black" (fun f -> { f with BlackCmd = false })
                    markFalse "post" (fun f -> { f with PostCmd = false })
                    markFalse "?" (fun f -> { f with MoveNowCmd = false })
                    markFalse "exit" (fun f -> { f with ExitCmd = false })
                    markFalse "option" (fun f -> { f with OptionCmd = false })
                    if lower.Contains("protover") then
                        isV1Fallback <- true
                        updated <- true
                    if updated then
                        logger.LogWarning($"Winboard capabilities downgraded based on error: {trimmedLine}")
                    else
                        logger.LogWarning($"Winboard engine error: {trimmedLine}")
                    None
                elif trimmedLine.StartsWith("#") then
                    // Comment line - ignore
                    None
                elif trimmedLine.StartsWith("tellics") || trimmedLine.StartsWith("tellusers") then
                    // ICS commands - ignore
                    None
                else
                    // Try SAN move without the "move " prefix
                    match tryParseMoveOutput board trimmedLine with
                    | Some uciMove -> Some uciMove
                    | None ->
                        // Unknown output - log for debugging
                        logger.LogDebug($"Winboard output (ignored): {trimmedLine}")
                        None
        
        
        
        
        /// Convert UCI command to Winboard command(s)
        member this.UciToWinboard(uciCommand: string) =
            let cmd = uciCommand.Trim()
            
            if cmd = "uci" then
                this.Initialize()
            elif cmd = "isready" then
                if not isInitialized then
                    []
                elif features.Ping then
                    // Only use ping if engine supports it
                    let pingId = System.Random().Next(1, 10000)
                    pendingPing <- Some pingId
                    [$"ping {pingId}"]
                else
                    // Winboard v1 engines don't support ping - just return empty
                    // The engine is considered "ready" after initialization
                    []
            elif cmd = "ucinewgame" then
                // Reset board to starting position
                board.ResetBoardState()
                moveCount <- 0
                logger.LogDebug($"Board reset to starting position: {board.FEN()}")
                ["new"]
            elif cmd.StartsWith("position") then
                // Create a fresh board for this position to avoid accumulated state corruption
                board.ResetBoardState() // <- new Board()
                
                // Parse and apply position command to our board
                try
                    if cmd.Contains("startpos") then
                        board.ResetBoardState()
                        logger.LogDebug($"Board set to startpos: {board.FEN()}")
                    elif cmd.Contains("fen") then
                        let fenIdx = cmd.IndexOf("fen")
                        if fenIdx >= 0 && fenIdx + 4 < cmd.Length then
                            let fenStart = fenIdx + 4
                            let movesIdx = cmd.IndexOf("moves")
                            let fen = 
                                if movesIdx >= 0 && movesIdx > fenStart then
                                    cmd.Substring(fenStart, movesIdx - fenStart).Trim()
                                elif fenStart < cmd.Length then
                                    cmd.Substring(fenStart).Trim()
                                else
                                    ""
                            
                            if not (String.IsNullOrWhiteSpace(fen)) then
                                board.LoadFen(fen)
                                logger.LogDebug($"Board set to FEN: {board.FEN()}")
                            else
                                logger.LogWarning($"Empty FEN in position command: {cmd}")
                        else
                            logger.LogWarning($"Invalid position command format: {cmd}")
                    
                    // Apply moves if present - stop on first error to prevent corruption
                    let movesIdx = cmd.IndexOf("moves")
                    if movesIdx >= 0 && movesIdx + 6 <= cmd.Length then
                        let movesStr = cmd.Substring(movesIdx + 6).Trim()
                        if not (String.IsNullOrWhiteSpace(movesStr)) then
                            let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                            let mutable moveIndex = 0
                            let mutable continueApplying = true
                            while continueApplying && moveIndex < moves.Length do
                                let moveStr = moves.[moveIndex]
                                match tryGetTMoveFromCoordinateNotation &board moveStr with
                                | Some tmove ->
                                    try
                                        board.MakeMove(&tmove)
                                        moveIndex <- moveIndex + 1
                                    with ex ->
                                        logger.LogError($"Failed to apply move {moveIndex + 1}/{moves.Length}: {moveStr}")
                                        logger.LogError($"Board FEN before failed move: {board.FEN()}")
                                        logger.LogError($"Error: {ex.Message}")
                                        continueApplying <- false
                                | None ->
                                    logger.LogWarning($"Could not parse move {moveIndex + 1}/{moves.Length}: {moveStr}, stopping at FEN: {board.FEN()}")
                                    continueApplying <- false
                            
                            // Log success
                            if moveIndex = moves.Length then
                                logger.LogDebug($"Successfully applied all {moves.Length} moves, final FEN: {board.FEN()}")
                with ex ->
                    logger.LogError($"Error processing position command: {ex.Message}")
                    logger.LogError($"Full command: {cmd}")
                    logger.LogError($"Stack trace: {ex.StackTrace}")
                    logger.LogError($"Current board state: {board.FEN()}")
                
                // Send commands to Winboard engine, then ping to ensure synchronization
                let positionCommands, usedForce =
                    if features.SetBoard then
                        // Prefer setboard when supported to avoid replaying moves on v1 engines.
                        let fen = 
                            if features.SetBoardNeedsFourFieldFen then
                                stripFenMoveCounters (board.FEN())
                            else
                                board.FEN()
                        ([ $"setboard {fen}" ], true)
                    else
                        positionToWinboard cmd features this.SupportsForceCmd
                positionUsedForce <- usedForce
                if features.Ping && positionCommands.Length > 2 then
                    // Send ping after position update to ensure engine has processed all moves
                    let pingId = System.Random().Next(1, 10000)
                    pendingPing <- Some pingId
                    positionCommands @ [$"ping {pingId}"]
                else
                    positionCommands
            elif cmd.StartsWith("go") then
                let winboardCmds = goToWinboard cmd features this.IsWhiteToMove (Some logger) this.SupportsTimeCmd this.SupportsOTimeCmd this.SupportsWhiteCmd this.SupportsBlackCmd
                let winboardCmds =
                    if positionUsedForce then
                        positionUsedForce <- false
                        if this.IsWhiteToMove && this.SupportsWhiteCmd then
                            "white" :: winboardCmds
                        elif (not this.IsWhiteToMove) && this.SupportsBlackCmd then
                            "black" :: winboardCmds
                        else
                            winboardCmds
                    else
                        winboardCmds
                let isInfinite = cmd.Contains("infinite")
                let useAnalyze = isInfinite && features.Analyze
                inAnalyzeMode <- useAnalyze
                winboardCmds
            elif cmd = "stop" then
                if inAnalyzeMode then
                    // Exit analyze mode
                    inAnalyzeMode <- false
                    if this.SupportsMoveNowCmd && this.SupportsExitCmd then ["?"; "exit"; "force"]
                    elif this.SupportsMoveNowCmd then ["?"; "force"]
                    elif this.SupportsExitCmd then ["exit"; "force"]
                    else []
                else
                    // Force move immediately
                    if this.SupportsMoveNowCmd then ["?"] else []
            elif cmd = "quit" then
                ["quit"]
            elif cmd.StartsWith("setoption") then
                // Translate UCI setoption into Winboard "option" when supported.
                let matchOpt = setOptionRegex.Match(cmd)
                if matchOpt.Success && this.SupportsOptionCmd then
                    let name = matchOpt.Groups.[1].Value.Trim()
                    let value =
                        if matchOpt.Groups.[2].Success then
                            matchOpt.Groups.[2].Value.Trim()
                        else
                            ""
                    if String.IsNullOrWhiteSpace(name) then
                        []
                    elif String.IsNullOrWhiteSpace(value) then
                        [ $"option {name}" ]
                    else
                        [ $"option {name}={value}" ]
                else
                    logger.LogDebug($"Ignoring UCI setoption for Winboard: {cmd}")
                    []
            else
                logger.LogWarning($"Unhandled UCI command for Winboard: {uciCommand}")
                []
        
        
        /// Reset state for new game
        member this.Reset() =
            board.ResetBoardState()
            moveCount <- 0
            pendingPing <- None
            logger.LogDebug("Winboard handler reset")
        
        /// Force initialization for Winboard v1 engines (no protover 2 support)
        /// Sets basic features that work with v1 engines
        member this.ForceInitialize() =
            // Set conservative v1 features (most basic Winboard commands)
            features <- {
                Ping = false          // v1 engines don't support ping
                SetBoard = false      // v1 engines don't support setboard
                PlayOther = false
                San = false
                UserMove = false      // v1 engines expect raw moves like "e2e4"
                Time = false          // v1 engines DON'T support 'st' command (time per move)
                                      // They only support 'time'/'otim' (tournament clock time)
                TimeCmd = false
                OTimeCmd = false
                ForceCmd = false
                WhiteCmd = false
                BlackCmd = false
                PostCmd = false
                MoveNowCmd = false
                ExitCmd = false
                OptionCmd = false
                EasyCmd = false
                HardCmd = false
                Sigint = false
                Sigterm = false
                Reuse = false
                Analyze = false
                MyName = Some "Winboard v1 Engine"
                Variants = None
                Colors = false
                Ics = false
                Name = false
                Pause = false
                Done = true           // Mark as done
                SetBoardNeedsFourFieldFen = false
            }
            isInitialized <- true
            isV1Fallback <- true
            logger.LogInformation("Forced Winboard v1 initialization with basic feature set")
        
        /// Mark as initialized without overwriting features (for v2 engines that don't send 'done=1')
        member this.MarkInitialized() =
            isInitialized <- true
            logger.LogInformation($"Marked Winboard engine as initialized with features: Time={features.Time}, Analyze={features.Analyze}, Ping={features.Ping}")

        member _.GetProbePlan(includeFeatureProbes: bool) =
            let startingPosFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            let commonProbes = [
                {
                    Name = "time"
                    Kind = TimeCmd
                    Commands = [ "time 10" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with TimeCmd = true }
                }
                {
                    Name = "otim"
                    Kind = OTimeCmd
                    Commands = [ "otim 10" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with OTimeCmd = true }
                }
                {
                    Name = "force"
                    Kind = ForceCmd
                    Commands = [ "force" ]
                    CleanupCommands = [ "new" ]
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with ForceCmd = true }
                }
                {
                    Name = "white"
                    Kind = WhiteCmd
                    Commands = [ "force"; "white" ]
                    CleanupCommands = [ "new" ]
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with WhiteCmd = true }
                }
                {
                    Name = "black"
                    Kind = BlackCmd
                    Commands = [ "force"; "black" ]
                    CleanupCommands = [ "new" ]
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with BlackCmd = true }
                }
                {
                    Name = "post"
                    Kind = PostCmd
                    Commands = [ "post" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with PostCmd = true }
                }
                {
                    Name = "movnow"
                    Kind = MoveNowCmd
                    Commands = [ "?" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with MoveNowCmd = true }
                }
                {
                    Name = "exit"
                    Kind = ExitCmd
                    Commands = [ "exit" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with ExitCmd = true }
                }
                {
                    Name = "easy"
                    Kind = EasyCmd
                    Commands = [ "easy" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with EasyCmd = true }
                }
                {
                    Name = "hard"
                    Kind = HardCmd
                    Commands = [ "hard" ]
                    CleanupCommands = []
                    TimeoutMs = ProbeTimeoutMs
                    AssumeSuccessOnSilence = true
                    TreatIllegalAsSuccess = false
                    OnSuccess = fun f -> { f with HardCmd = true }
                }
            ]

            let featureProbes =
                if includeFeatureProbes then
                    [
                        {
                            Name = "ping"
                            Kind = Ping
                            Commands = []  // ping command is set in BeginProbe for unique id
                            CleanupCommands = []
                            TimeoutMs = PingProbeTimeoutMs
                            AssumeSuccessOnSilence = false
                            TreatIllegalAsSuccess = false
                            OnSuccess = fun f -> { f with Ping = true }
                        }
                        {
                            Name = "setboard"
                            Kind = SetBoard
                            Commands = [ $"setboard {startingPosFen}" ]
                            CleanupCommands = [ "new" ]
                            TimeoutMs = SetboardProbeTimeoutMs
                            AssumeSuccessOnSilence = true
                            TreatIllegalAsSuccess = false
                            OnSuccess = fun f -> { f with SetBoard = true }
                        }
                        {
                            Name = "usermove"
                            Kind = UserMove
                            Commands = [ "force"; "usermove a2a5" ]
                            CleanupCommands = [ "new" ]
                            TimeoutMs = UserMoveProbeTimeoutMs
                            AssumeSuccessOnSilence = false
                            TreatIllegalAsSuccess = true
                            OnSuccess = fun f -> { f with UserMove = true }
                        }
                        {
                            Name = "analyze"
                            Kind = Analyze
                            Commands = [ "analyze" ]
                            CleanupCommands = [ "exit" ]
                            TimeoutMs = AnalyzeProbeTimeoutMs
                            AssumeSuccessOnSilence = true
                            TreatIllegalAsSuccess = false
                            OnSuccess = fun f -> { f with Analyze = true }
                        }
                    ]
                else
                    []

            let isSupported kind =
                match kind with
                | Ping -> features.Ping
                | SetBoard -> features.SetBoard
                | UserMove -> features.UserMove
                | Analyze -> features.Analyze
                | TimeCmd -> features.TimeCmd
                | OTimeCmd -> features.OTimeCmd
                | ForceCmd -> features.ForceCmd
                | WhiteCmd -> features.WhiteCmd
                | BlackCmd -> features.BlackCmd
                | PostCmd -> features.PostCmd
                | MoveNowCmd -> features.MoveNowCmd
                | ExitCmd -> features.ExitCmd
                | EasyCmd -> features.EasyCmd
                | HardCmd -> features.HardCmd

            (featureProbes @ commonProbes)
            |> List.filter (fun p -> not (isSupported p.Kind))

        member this.BeginProbe (probe: ProbeRequest) =
            async {
                // Only one probe at a time
                let shouldStart = 
                    lock probeLock (fun () ->
                        if activeProbe.IsSome then
                            logger.LogWarning("Probe already in progress; skipping new probe.")
                            false
                        else
                            let tcs = System.Threading.Tasks.TaskCompletionSource<bool>()
                            let pingId =
                                match probe.Kind with
                                | Ping -> Some (System.Random().Next(PingIdMin, PingIdMax))
                                | _ -> None
                            activeProbe <- Some (probe, pingId, tcs)
                            true
                    )
                
                if not shouldStart then
                    return false
                else
                    let tcs = 
                        lock probeLock (fun () ->
                            match activeProbe with
                            | Some (_, _, t) -> t
                            | None -> System.Threading.Tasks.TaskCompletionSource<bool>()
                        )

                    // Timeout handling
                    Async.Start(async {
                        do! Async.Sleep(probe.TimeoutMs)
                        lock probeLock (fun () ->
                            match activeProbe with
                            | Some (p, _, tcs2) when obj.ReferenceEquals(p, probe) ->
                                activeProbe <- None
                                let success = probe.AssumeSuccessOnSilence
                                if success then
                                    features <- probe.OnSuccess features
                                    logger.LogDebug($"Winboard probe '{probe.Name}' timed out; assuming success.")
                                else
                                    logger.LogDebug($"Winboard probe '{probe.Name}' timed out; assuming failure.")
                                tcs2.TrySetResult(success) |> ignore
                            | _ -> ()
                        )
                    })

                    return! tcs.Task |> Async.AwaitTask
            }

        member _.GetProbePingCommand() =
            lock probeLock (fun () ->
                match activeProbe with
                | Some (probe, Some id, _) when probe.Kind = Ping -> Some $"ping {id}"
                | _ -> None
            )
