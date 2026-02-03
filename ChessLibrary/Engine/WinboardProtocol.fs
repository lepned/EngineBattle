namespace ChessLibrary

open System
open System.Text.RegularExpressions
open Microsoft.Extensions.Logging
open PositionTypes
open TypesDef.CoreTypes
open Chess
open Chess.BoardUtils

/// Winboard/XBoard protocol implementation for EngineBattle
module WinboardProtocol =

    // Constants
    [<Literal>]
    let private DepthIterationThreshold = 1000

    // Cached regex patterns
    let private thinkingOutputRegex = Regex(@"^\s*\d+\s+[+-]?\d+\s+\d+\s+\d+", RegexOptions.Compiled)
    let private cometTellicsRegex = Regex(@"tellics\s+sc=([+-]?\d+\.?\d*)\s+dp=(\d+)\s+nps=(\d+)K?\s+\(([^)]+)\)", RegexOptions.Compiled)
    let private wtimeRegex = Regex(@"wtime (\d+)", RegexOptions.Compiled)
    let private btimeRegex = Regex(@"btime (\d+)", RegexOptions.Compiled)
    let private wincRegex = Regex(@"winc (\d+)", RegexOptions.Compiled)
    let private bincRegex = Regex(@"binc (\d+)", RegexOptions.Compiled)
    let private movetimeRegex = Regex(@"movetime (\d+)", RegexOptions.Compiled)
    let private setOptionRegex = Regex(@"^setoption\s+name\s+(.+?)(?:\s+value\s+(.+))?$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let private coordinateNotationRegex = Regex(@"^[a-h][1-8][a-h][1-8][qrbn]?$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let private moveNotationStartRegex = Regex(@"^[a-h][1-8][a-h][1-8]", RegexOptions.Compiled)
    let private moveNumberPrefixRegex = Regex(@"^\d+\.$", RegexOptions.Compiled)

    /// Winboard engine feature support state (trimmed from 25 fields)
    type FeatureState = {
        Ping: bool
        SetBoard: bool
        UserMove: bool
        San: bool
        Analyze: bool
        Time: bool
        Reuse: bool option  // None = not sent (defaults to true per CECP spec)
        PlayOther: bool
        MyName: string option
        Done: bool
    }

    /// Default features — all disabled until negotiated
    let defaultFeatures = {
        Ping = false
        SetBoard = false
        UserMove = false
        San = false
        Analyze = false
        Time = false
        Reuse = None
        PlayOther = false
        MyName = None
        Done = false
    }

    /// Parse a single feature line from Winboard engine
    /// Example: "feature ping=1 setboard=1 done=1"
    let parseFeatureLine (line: string) (features: FeatureState) =
        let text = if line.StartsWith("feature") then line.Substring(7).Trim() else line
        // Handle quoted values by splitting more carefully
        let mutable updatedFeatures = features
        let mutable i = 0
        while i < text.Length do
            // Find key
            let eqIdx = text.IndexOf('=', i)
            if eqIdx < 0 then
                i <- text.Length // done
            else
                let key = text.Substring(i, eqIdx - i).Trim().ToLower()
                let valueStart = eqIdx + 1
                let mutable value = ""
                let mutable nextStart = 0
                if valueStart < text.Length && text.[valueStart] = '"' then
                    // Quoted value
                    let closeQuote = text.IndexOf('"', valueStart + 1)
                    if closeQuote >= 0 then
                        value <- text.Substring(valueStart + 1, closeQuote - valueStart - 1)
                        nextStart <- closeQuote + 1
                    else
                        value <- text.Substring(valueStart + 1)
                        nextStart <- text.Length
                else
                    // Unquoted value — up to next space
                    let spaceIdx = text.IndexOf(' ', valueStart)
                    if spaceIdx >= 0 then
                        value <- text.Substring(valueStart, spaceIdx - valueStart)
                        nextStart <- spaceIdx + 1
                    else
                        value <- text.Substring(valueStart)
                        nextStart <- text.Length

                updatedFeatures <-
                    match key with
                    | "ping" -> { updatedFeatures with Ping = value = "1" }
                    | "setboard" -> { updatedFeatures with SetBoard = value = "1" }
                    | "playother" -> { updatedFeatures with PlayOther = value = "1" }
                    | "san" -> { updatedFeatures with San = value = "1" }
                    | "usermove" -> { updatedFeatures with UserMove = value = "1" }
                    | "time" -> { updatedFeatures with Time = value = "1" }
                    | "reuse" -> { updatedFeatures with Reuse = Some (value = "1") }
                    | "analyze" -> { updatedFeatures with Analyze = value = "1" }
                    | "myname" -> { updatedFeatures with MyName = Some value }
                    | "done" -> { updatedFeatures with Done = value = "1" }
                    | _ -> updatedFeatures

                i <- nextStart
        updatedFeatures


    /// Convert UCI position command to Winboard commands
    /// Returns list of moves to send to engine (does not include 'new' or 'force')
    let positionToWinboard (command: string) (features: FeatureState) (use4FieldFen: bool) (board: Board inref) =
        let commands = ResizeArray<string>()
        if command.StartsWith("position startpos") || command.StartsWith("position fen") then
            if features.SetBoard then
                // For setboard engines, just set the board position directly
                let fen = board.FEN()
                // Strip halfmove clock and fullmove number for old engines
                let fenToSend =
                    if use4FieldFen then
                        let parts = fen.Split(' ')
                        if parts.Length >= 6 then
                            // Keep only: position, side, castling, en passant (4 fields)
                            String.Join(" ", parts.[0..3])
                        else
                            fen  // Already 4-field or less
                    else
                        fen  // Full 6-field FEN
                commands.Add($"setboard {fenToSend}")
            else
                // For engines without setboard, send all moves from starting position
                let moves = board.InlineTokensFromGraph()
                let hasMoves = moves.Length > 0
                if hasMoves then
                    let prefix = if features.UserMove then "usermove " else ""
                    for move in moves do
                        commands.Add($"{prefix}{move.MoveCoord}")
        commands |> Seq.toList

    /// Parse Winboard thinking output to UCI info format
    /// Winboard: "depth score time nodes pv..."
    /// Handles: standard, tab-indented SAN (Crafty), coordinate+tb (Jonny),
    ///          SAN prefix (EXchess), score*1000 (TheKing), kibitz (Comet)
    let parseThinkingOutput (sideToMovePOV: bool) (currentBoard: Chess.Board) (engineName: string) (line: string) =
        // Handle tab-indented output (Crafty)
        let trimmedLine = line.TrimStart([|'\t'; ' '|])
        let parts = trimmedLine.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)

        if parts.Length >= 4 then
            try
                let depthRaw = Int32.Parse(parts.[0])
                let depth = if depthRaw > DepthIterationThreshold then depthRaw / 1000 else depthRaw

                let score = Int32.Parse(parts.[1])
                let time = Int32.Parse(parts.[2])  // centiseconds
                let nodes = Int64.Parse(parts.[3])

                // Score perspective:
                // Most engines (SideToMovePOV=false): report from White's perspective always
                // Rare engines (SideToMovePOV=true): report from side-to-move's perspective (like Crafty)
                // UCI standard: scores from White's perspective
                let isBlackToMove = currentBoard.Position.STM <> 0uy
                let adjustedScore =
                    if sideToMovePOV && isBlackToMove then
                        -score  // Convert from Black's perspective to White's perspective
                    else
                        score  // Already from White's perspective

                // Convert PV from SAN to coordinate notation
                let pv =
                    if parts.Length > 4 then
                        let rawPv = parts.[4..]
                        let normalized =
                            rawPv
                            |> Array.map (fun t -> t.Trim())
                            |> Array.filter (fun t ->
                                t <> "" && t <> "..." && not (moveNumberPrefixRegex.IsMatch(t))
                                && not (t.StartsWith("tb=")) // Jonny tb suffix
                            )
                            |> Array.map (fun t -> if t.StartsWith("O") then t else t.Replace("-", ""))
                        let sanPVline = String.Join(" ", normalized).Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        getLongSanPVFromShortSanPV moveList.Value &currentBoard sanPVline
                    else
                        ""

                let nps = if time > 0 then int64 ((float nodes) / (float time / 100.0)) else 0L

                Some $"info depth {depth} score cp {adjustedScore} time {time * 10} nodes {nodes} nps {nps} pv {pv}"
            with
            | _ -> None
        else
            None

    /// Parse Comet's tellics thinking output to UCI info format
    /// Format: "tellics  sc=+0.36 dp=10 nps=0K (h4h5 g6h7 b1c3 e7e6 g1e2)"
    let parseCometTellics (currentBoard: Chess.Board) (line: string) =
        let m = cometTellicsRegex.Match(line)
        if m.Success then
            try
                let scoreFloat = Double.Parse(m.Groups.[1].Value)
                let depth = Int32.Parse(m.Groups.[2].Value)
                let npsValue = Int32.Parse(m.Groups.[3].Value)
                let pvRaw = m.Groups.[4].Value

                // Convert score from pawns to centipawns
                let scoreCp = int (scoreFloat * 100.0)

                // Convert nps (already in thousands)
                let nps = int64 npsValue * 1000L

                // Parse PV - Comet uses coordinate notation
                let pvMoves = pvRaw.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                let pv = String.Join(" ", pvMoves)

                Some $"info depth {depth} score cp {scoreCp} nps {nps} pv {pv}"
            with
            | _ -> None
        else
            None

    /// Parse Winboard move output to UCI bestmove
    let tryParseMoveOutput (board: Chess.Board) (line: string) =
        let moveStr =
            if line.StartsWith("move ") then
                line.Substring(5).Trim()
            else
                line.Trim()

        let normalizeToken (token: string) =
            token.Trim().Trim([|'.'; ','; ';'; ':'; '!'; '?'|])

        let isEllipsis (token: string) =
            token = "..." || token = ".." || token = "."

        let tryNormalizeCoordinate (token: string) =
            let t = normalizeToken token
            if coordinateNotationRegex.IsMatch(t) then
                Some t
            else
                let dehyphen = t.Replace("-", "")
                if coordinateNotationRegex.IsMatch(dehyphen) then Some dehyphen else None

        let parts = moveStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length = 0 then
            None
        else
            let move = parts.[0]
            if String.IsNullOrWhiteSpace(move) then
                None
            elif coordinateNotationRegex.IsMatch(move) then
                Some $"bestmove {move.ToLower()}"
            else
                // Try to find a coordinate-notation token anywhere in the line
                // Use tryFind (not tryFindBack) to get the FIRST match for multi-move output
                let coordToken =
                    parts
                    |> Array.tryFind (fun token -> tryNormalizeCoordinate token |> Option.isSome)
                match coordToken |> Option.bind tryNormalizeCoordinate with
                | Some token -> Some $"bestmove {token.ToLower()}"
                | None ->
                    // Try SAN conversion - use tryFind for first match
                    let sanCandidate =
                        parts
                        |> Array.map normalizeToken
                        |> Array.filter (fun t -> not (String.IsNullOrWhiteSpace(t)) && not (isEllipsis t))
                        |> Array.tryFind (fun t ->
                            let mutable v = 0
                            not (Int32.TryParse(t, &v))
                        )
                    match sanCandidate with
                    | Some sanMove ->
                        let longSan = BoardUtils.getLongSanPVFromShortSanPV moveList.Value &board [sanMove]
                        if String.IsNullOrWhiteSpace(longSan) then
                            None
                        else
                            let converted = longSan.Split([|' '|], StringSplitOptions.RemoveEmptyEntries).[0]
                            Some $"bestmove {converted}"
                    | None -> None

    /// Check if a line looks like a move
    let isMoveNotation (line: string) =
        if String.IsNullOrWhiteSpace(line) then false
        elif line.StartsWith("move ") then true
        elif Regex.IsMatch(line, @"^\s*\d+\.\s*(\.\.\.)?\s*\S+") then true
        elif line.Length >= 4 then
            moveNotationStartRegex.IsMatch(line.Substring(0, min 4 line.Length))
        else
            false

    /// Winboard protocol handler — manages state and translation
    ///
    /// **Thread Safety:**
    /// All public methods are thread-safe and can be called concurrently from multiple threads.
    /// Internal mutable state (board, features, flags) is protected by a reentrant lock (stateLock).
    /// The handler maintains its own internal chess board for position tracking and move parsing.
    ///
    /// **Typical Usage:**
    /// - Engine initialization thread: Calls ProcessFeatureLine during startup
    /// - Main game thread: Calls UciToWinboard to send commands
    /// - Output reader thread: Calls ProcessOutput for engine responses
    /// - Reset can be called from any thread between games
    type WinboardHandler(logger: ILogger, configuredEngineName: string, winboardConfig: TypesDef.CoreTypes.WinboardConfig) =
        let startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let stateLock = obj()  // Lock for thread-safe access to mutable state
        let mutable features = defaultFeatures
        let mutable isInitialized = false
        let mutable isV1Fallback = false
        let mutable isProtover2 = false
        let mutable pendingPing = None
        let mutable board = new Board()
        let mutable inAnalyzeMode = false
        let initSemaphore = new System.Threading.SemaphoreSlim(0, 1)
        let mutable levelCommandSent = false  // Track if we've sent level command for current game
        let mutable originalBaseTimeMs = 0   // Store original time control base time
        let mutable originalIncrementMs = 0  // Store original time control increment
        let mutable resolvedStrategy = None  // Resolved time control strategy (None = use configured)
        let mutable gameInitialized = false  // Track if we've sent "new" command to initialize game

        member _.Features = lock stateLock (fun () -> features)
        member _.IsInitialized = lock stateLock (fun () -> isInitialized)
        member _.IsV1Fallback = lock stateLock (fun () -> isV1Fallback)
        member _.IsProtover2 = lock stateLock (fun () -> isProtover2)
        member _.ConfiguredName = configuredEngineName
        /// Per CECP spec, reuse defaults to true when not explicitly set to 0
        member _.CanReuse = lock stateLock (fun () -> features.Reuse |> Option.defaultValue true)
        /// Semaphore that signals when initialization completes
        member _.InitializationSemaphore = initSemaphore
        /// Get the configured time control strategy
        member _.ConfiguredTimeControlStrategy = winboardConfig.TimeControlStrategy

        /// Get initialization commands
        member _.GetInitCommands() = [ "xboard"; "protover 2" ]

        /// Process a feature line during init. Returns true when done=1.
        member _.ProcessFeatureLine(line: string) =
            lock stateLock (fun () ->
                features <- parseFeatureLine line features
                if features.Done then
                    isInitialized <- true
                    isProtover2 <- true
                    if initSemaphore.CurrentCount = 0 then
                        initSemaphore.Release() |> ignore
                features.Done
            )

        /// Force V1 initialization with conservative defaults
        member _.ForceV1Init() =
            lock stateLock (fun () ->
                features <- {
                    Ping = false
                    SetBoard = false
                    UserMove = false
                    San = false
                    Analyze = false
                    Time = false
                    Reuse = None
                    PlayOther = false
                    MyName = Some "Winboard v1 Engine"
                    Done = true
                }
                isInitialized <- true
                isV1Fallback <- true
                if initSemaphore.CurrentCount = 0 then
                    initSemaphore.Release() |> ignore
                logger.LogInformation("Forced Winboard v1 initialization with basic feature set")
            )

        /// Mark as initialized (for V2 engines that didn't send done=1)
        member _.MarkInitialized() =
            lock stateLock (fun () ->
                isInitialized <- true
                if initSemaphore.CurrentCount = 0 then
                    initSemaphore.Release() |> ignore
                let msg = $"Marked Winboard engine as initialized: Time={features.Time}, Analyze={features.Analyze}, Ping={features.Ping}"
                logger.LogInformation(msg)
            )

        /// Set the resolved time control strategy (after AutoDetect probing)
        member _.SetResolvedStrategy(strategy: TypesDef.CoreTypes.TimeControlStrategy) =
            lock stateLock (fun () ->
                resolvedStrategy <- Some strategy
                logger.LogInformation($"Resolved time control strategy for {configuredEngineName}: {strategy}")
            )

        /// Get the effective time control strategy (resolved or configured)
        member _.GetEffectiveStrategy() =
            lock stateLock (fun () ->
                match resolvedStrategy with
                | Some s -> s
                | None ->
                    match winboardConfig.TimeControlStrategy with
                    | TypesDef.CoreTypes.TimeControlStrategy.AutoDetect ->
                        // Default fallback for AutoDetect when not yet resolved
                        TypesDef.CoreTypes.TimeControlStrategy.TimeOtimOnly
                    | s -> s
            )

        /// Get post-init commands (includes configured startup commands)
        member _.GetPostInitCommands() =
            let baseCommands = [ "post"; "easy" ]
            let levelCmd = if winboardConfig.RequiresLevelForThinkingOutput then ["level 40 5 0"] else []
            baseCommands @ levelCmd @ winboardConfig.StartupCommands

        /// Convert UCI command to Winboard command(s)
        /// analysisMode: if true, always send full position setup for analysis engines
        member this.UciToWinboard(uciCommand: string, ?analysisMode: bool) =
            lock stateLock (fun () ->
                let isAnalysis = defaultArg analysisMode false
                let cmd = uciCommand.Trim()

                if cmd = "uci" then
                    this.GetInitCommands()
                elif cmd = "isready" then
                    if not isInitialized then
                        []
                    elif features.Ping then
                        // Use Random.Shared for thread safety and larger ID range
                        let pingId = System.Random.Shared.Next(1, Int32.MaxValue)
                        pendingPing <- Some pingId
                        [$"ping {pingId}"]
                    else
                        // V1 engines don't support ping — considered ready after init
                        []
                elif cmd = "ucinewgame" then
                    this.Reset()
                    gameInitialized <- true  // Mark that we're initializing the game
                    ["new"]
                elif cmd.StartsWith("position") then
                    // Parse and apply position to internal board
                    let fen = board.FEN()
                    try
                        if cmd.Contains("fen") then
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
                                    if fen = startpos then
                                        board.ResetBoardState()
                                    else
                                        board.LoadFen(fen)

                        // Apply moves
                        let movesIdx = cmd.IndexOf("moves")
                        if movesIdx >= 0 && movesIdx + 6 <= cmd.Length then
                            let movesStr = cmd.Substring(movesIdx + 6).Trim()
                            if not (String.IsNullOrWhiteSpace(movesStr)) then
                                let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                                for moveStr in moves do
                                    board.PlayLongSanMove(moveStr)
                    with ex ->
                        logger.LogError($"Error processing position command: {ex.Message}")

                    // Send "new" based on engine capabilities:
                    // - Engines WITH setboard: Send "new" only once (setboard sets absolute position)
                    // - Engines WITHOUT setboard: ALWAYS send "new" (moves are relative to startpos)
                    let positionCmds = positionToWinboard cmd features winboardConfig.Use4FieldFen &board
                    if features.SetBoard then
                        // SetBoard engines: skip "new" after first position (preserves state)
                        if not gameInitialized then
                            gameInitialized <- true
                            "new" :: "force" :: positionCmds
                        else
                            "force" :: positionCmds
                    else
                        // Non-setboard engines: always send "new" (moves need startpos reset)
                        "new" :: "force" :: positionCmds

                elif cmd.StartsWith("go") then
                    let isWhite = board.Position.STM = 0uy
                    let isInfinite = cmd.Contains("infinite")
                    let useAnalyze = isInfinite && features.Analyze
                    inAnalyzeMode <- useAnalyze
                    this.GoToWinboard cmd isWhite isAnalysis

                elif cmd = "stop" then
                    if inAnalyzeMode then
                        inAnalyzeMode <- false
                        if features.Analyze then ["exit"] else ["?"]
                    else
                        ["?"]

                elif cmd = "quit" then
                    ["quit"]

                elif cmd.StartsWith("setoption") then
                    let matchOpt = setOptionRegex.Match(cmd)
                    if matchOpt.Success then
                        let name = matchOpt.Groups.[1].Value.Trim()
                        let value =
                            if matchOpt.Groups.[2].Success then matchOpt.Groups.[2].Value.Trim()
                            else ""
                        if String.IsNullOrWhiteSpace(name) then []
                        elif String.IsNullOrWhiteSpace(value) then [ $"option {name}" ]
                        else [ $"option {name}={value}" ]
                    else
                        []
                else
                    logger.LogWarning($"Unhandled UCI command for Winboard: {uciCommand}")
                    []
            )

        /// Send time/otim commands for current clock state
        member private _.SendTimeOtimCommands (isWhite: bool) (wtimeMs: int) (btimeMs: int) (commands: ResizeArray<string>) =
            let wtime = wtimeMs / 10  // Convert ms to centiseconds
            let btime = btimeMs / 10
            let toMove = if isWhite then "W" else "B"

            logger.LogDebug($"[{configuredEngineName}] Clock state: wtime={wtimeMs}ms ({wtime}cs), btime={btimeMs}ms ({btime}cs), toMove={toMove}")

            if isWhite then
                commands.Add($"time {wtime}")
                commands.Add($"otim {btime}")
            else
                commands.Add($"time {btime}")
                commands.Add($"otim {wtime}")

        /// Send level command (only once per game)
        member private this.SendLevelCommand (wtimeMs: int) (btimeMs: int) (wincMs: int) (bincMs: int) (commands: ResizeArray<string>) =
            // Store original time control from first go command
            if originalBaseTimeMs = 0 then
                originalBaseTimeMs <- max wtimeMs btimeMs
                originalIncrementMs <- max wincMs bincMs

            // Convert to Winboard level format: "level MOVES BASE INC"
            let totalSeconds = originalBaseTimeMs / 1000
            let baseMinutes = totalSeconds / 60
            let baseSecondsRemainder = totalSeconds % 60
            let incSeconds = float originalIncrementMs / 1000.0

            // Format base time: use min:sec if there are seconds, otherwise just min
            let baseFormatted =
                if baseSecondsRemainder > 0 then
                    $"{baseMinutes}:{baseSecondsRemainder:D2}"
                else
                    $"{baseMinutes}"

            // Format increment: Round to nearest integer for compatibility
            let incRounded = int (incSeconds + 0.5)
            let incFormatted = incRounded.ToString(System.Globalization.CultureInfo.InvariantCulture)

            let levelCmd = $"level 0 {baseFormatted} {incFormatted}"
            logger.LogInformation($"[{configuredEngineName}] Sending time control: {levelCmd} (base={originalBaseTimeMs}ms, inc={originalIncrementMs}ms)")
            commands.Add(levelCmd)
            levelCommandSent <- true

        /// Send dynamic st command (for engines with broken level support)
        member private _.SendDynamicStCommand (isWhite: bool) (wtimeMs: int) (btimeMs: int) (commands: ResizeArray<string>) =
            let currentTimeMs = if isWhite then wtimeMs else btimeMs
            let movesPlayed = board.PlyCount / 2  // Convert plies to full moves

            // Estimate moves remaining: start at 40, decrease as game progresses, min 10
            let estimatedMovesRemaining = max 10 (40 - movesPlayed)
            let secondsPerMove = (currentTimeMs / 1000) / estimatedMovesRemaining

            if secondsPerMove < 1 then
                logger.LogWarning($"[{configuredEngineName}] Calculated st < 1s, using st 0")
                commands.Add("st 0")
            else
                logger.LogDebug($"[{configuredEngineName}] Dynamic st: time={currentTimeMs}ms, movesPlayed={movesPlayed}, movesEst={estimatedMovesRemaining}, st={secondsPerMove}")
                commands.Add($"st {secondsPerMove}")

        /// Convert UCI go command to Winboard commands (member method to access state)
        member this.GoToWinboard (command: string) (isWhite: bool) (analysis: bool) =
            let commands = ResizeArray<string>()

            // Parse time control parameters from UCI go command
            let wtimeMatch = wtimeRegex.Match(command)
            let btimeMatch = btimeRegex.Match(command)
            let wincMatch = wincRegex.Match(command)
            let bincMatch = bincRegex.Match(command)
            let movetimeMatch = movetimeRegex.Match(command)
            let isInfinite = command.Contains("infinite")

            // Handle infinite analysis mode
            if isInfinite then
                if features.Analyze then
                    commands.Add "easy"
                    commands.Add "analyze"
                else
                    commands.Add "easy"
                    commands.Add "go"
            else
                // Regular timed search
                if analysis then
                    commands.Add "easy"

                // Fixed time per move (movetime)
                if movetimeMatch.Success then
                    let timeMs = Int32.Parse(movetimeMatch.Groups.[1].Value)
                    let seconds = max 1 ((timeMs + 999) / 1000)
                    commands.Add($"st {seconds}")

                // Standard time control (wtime/btime/winc/binc)
                elif wtimeMatch.Success && btimeMatch.Success then
                    let wtimeMs = Int32.Parse(wtimeMatch.Groups.[1].Value)
                    let btimeMs = Int32.Parse(btimeMatch.Groups.[1].Value)
                    let wincMs = if wincMatch.Success then Int32.Parse(wincMatch.Groups.[1].Value) else 0
                    let bincMs = if bincMatch.Success then Int32.Parse(bincMatch.Groups.[1].Value) else 0

                    // Use time control strategy (V1 engines always use TimeOtimOnly)
                    let strategy = if isV1Fallback then TypesDef.CoreTypes.TimeControlStrategy.TimeOtimOnly else this.GetEffectiveStrategy()

                    match strategy with
                    | TypesDef.CoreTypes.TimeControlStrategy.LevelWithTime ->
                        // Standard: level (once per game) + time/otim (every move)
                        if not levelCommandSent then
                            this.SendLevelCommand wtimeMs btimeMs wincMs bincMs commands
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                    | TypesDef.CoreTypes.TimeControlStrategy.TimeOtimOnly ->
                        // V1 engines or broken level: time/otim only
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                    | TypesDef.CoreTypes.TimeControlStrategy.StWithTime ->
                        // Safety mode: st + time/otim for better time management
                        this.SendDynamicStCommand isWhite wtimeMs btimeMs commands
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                    | TypesDef.CoreTypes.TimeControlStrategy.StOnly ->
                        // Legacy mode: st only (may cause poor time management)
                        this.SendDynamicStCommand isWhite wtimeMs btimeMs commands

                    | TypesDef.CoreTypes.TimeControlStrategy.AutoDetect ->
                        // Should be resolved by now, but fallback to TimeOtimOnly
                        logger.LogWarning($"AutoDetect strategy not resolved for {configuredEngineName}, using TimeOtimOnly")
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                commands.Add("go")

            commands |> Seq.toList

        /// Process a line of output from Winboard engine.
        /// Returns Some(uci_line) if translation is needed, None otherwise.
        member this.ProcessOutput(line: string) =
            lock stateLock (fun () ->
                if String.IsNullOrWhiteSpace(line) then
                    None
                else
                    let trimmedLine = line.Trim()

                    if trimmedLine.StartsWith("feature") then
                        logger.LogDebug($"[{configuredEngineName}] Processing feature line: {trimmedLine}")
                        this.ProcessFeatureLine(trimmedLine) |> ignore
                        let opt = features.MyName |> Option.defaultValue "Unknown"
                        logger.LogInformation($"Winboard engine features negotiated: {opt}")
                        if features.Done then
                            logger.LogDebug($"[{configuredEngineName}] done=1 received, signaling init complete")
                        None

                    elif trimmedLine.StartsWith("pong") then
                        let pongId =
                            let parts = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                            if parts.Length > 1 then
                                match Int32.TryParse(parts.[1]) with
                                | true, id -> Some id
                                | _ -> None
                            else None
                        match pendingPing, pongId with
                        | Some expectedId, Some receivedId when expectedId = receivedId ->
                            // Strict ID matching: only accept pong with exact matching ID
                            pendingPing <- None
                            Some "readyok"
                        | Some _, None ->
                            // Engine sent pong without ID - log warning but don't accept
                            logger.LogWarning($"[{configuredEngineName}] Received pong without ID, expected ID {pendingPing.Value}")
                            None
                        | Some expectedId, Some receivedId ->
                            // Mismatched ID - possible race condition
                            logger.LogWarning($"[{configuredEngineName}] Received pong with ID {receivedId}, expected {expectedId}")
                            None
                        | None, _ ->
                            // No pending ping - ignore unexpected pong
                            None

                    elif trimmedLine.StartsWith("move ") || isMoveNotation trimmedLine then
                        match tryParseMoveOutput board trimmedLine with
                        | Some uciMove -> Some uciMove
                        | None ->
                            logger.LogWarning($"Winboard move not understood: {trimmedLine}")
                            None

                    elif thinkingOutputRegex.IsMatch(trimmedLine) then
                        parseThinkingOutput winboardConfig.SideToMovePOV board configuredEngineName trimmedLine

                    elif cometTellicsRegex.IsMatch(trimmedLine) then
                        parseCometTellics board trimmedLine

                    elif trimmedLine.StartsWith("Error") || trimmedLine.StartsWith("Illegal") then
                        logger.LogWarning($"Winboard engine error: {trimmedLine}")
                        // Fast-fail V1 detection: if engine doesn't understand protover, it's V1
                        if trimmedLine.Contains("protover") && not isInitialized then
                            logger.LogInformation($"[{configuredEngineName}] V1 engine detected (protover error), applying fallback immediately")
                            this.ForceV1Init()
                        None

                    elif trimmedLine.StartsWith("#") || trimmedLine.StartsWith("tellics") || trimmedLine.StartsWith("tellusers") || trimmedLine.StartsWith("kibitz") then
                        None

                    elif trimmedLine = "++" || trimmedLine = "--" then
                        None

                    else
                        // Try as a move without "move " prefix
                        match tryParseMoveOutput board trimmedLine with
                        | Some uciMove -> Some uciMove
                        | None ->
                            logger.LogDebug($"Winboard output (ignored): {trimmedLine}")
                            None
            )

        /// Enable setboard support (used after V1 probe succeeds)
        member _.EnableSetBoard() =
            lock stateLock (fun () ->
                features <- { features with SetBoard = true }
            )

        /// Reset state for new game
        member _.Reset() =
            lock stateLock (fun () ->
                board.ResetBoardState()
                pendingPing <- None
                inAnalyzeMode <- false
                levelCommandSent <- false
                originalBaseTimeMs <- 0
                originalIncrementMs <- 0
                gameInitialized <- false  // Will send "new" for next game
            )
