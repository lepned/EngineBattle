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
    let positionToWinboard (command: string) (features: FeatureState) =
        let commands = ResizeArray<string>()
        let startingPosFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        if command.StartsWith("position startpos") then
            let movesIdx = command.IndexOf("moves")
            if movesIdx >= 0 then
                let movesStr = command.Substring(movesIdx + 6)
                let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if features.SetBoard then
                    // For setboard engines, just set the board position directly
                    // (the handler will compute the correct FEN from the internal board)
                    ()
                else
                    let prefix = if features.UserMove then "usermove " else ""
                    for move in moves do
                        commands.Add($"{prefix}{move}")

        elif command.StartsWith("position fen") then
            let fenIdx = command.IndexOf("fen")
            let movesIdx = command.IndexOf("moves")

            let fen =
                if fenIdx >= 0 then
                    let fenStart = fenIdx + 4
                    if movesIdx >= 0 && movesIdx > fenStart then
                        command.Substring(fenStart, movesIdx - fenStart).Trim()
                    else
                        command.Substring(fenStart).Trim()
                else
                    ""

            let isStartingPos = command.Contains(startingPosFen)
            let hasMoves = movesIdx >= 0 && movesIdx + 6 < command.Length

            if features.SetBoard && not (String.IsNullOrWhiteSpace(fen)) && not isStartingPos then
                commands.Add($"setboard {fen}")

            if hasMoves then
                let movesStr = command.Substring(movesIdx + 6).Trim()
                if not (String.IsNullOrWhiteSpace(movesStr)) then
                    let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    let prefix = if features.UserMove then "usermove " else ""
                    for move in moves do
                        commands.Add($"{prefix}{move}")

        commands |> Seq.toList

    /// Parse Winboard thinking output to UCI info format
    /// Winboard: "depth score time nodes pv..."
    /// Handles: standard, tab-indented SAN (Crafty), coordinate+tb (Jonny),
    ///          SAN prefix (EXchess), score*1000 (TheKing), kibitz (Comet)
    let parseThinkingOutput (flipped: bool) (currentBoard: Chess.Board) (engineName: string) (line: string) =
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
                // Winboard standard: scores from side-to-move's perspective
                // UCI standard: scores from engine's perspective (which Engine.fs treats as side-to-move)
                // If flipped (config.Flipped=true): engine reports from White's perspective always
                let isBlackToMove = currentBoard.Position.STM <> 0uy
                let adjustedScore =
                    if flipped then
                        score  // Already from White's perspective
                    elif isBlackToMove then
                        -score  // Convert from Black's perspective to White's perspective
                    else
                        score  // White to move, already White's perspective

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
                let coordToken =
                    parts
                    |> Array.tryFindBack (fun token -> tryNormalizeCoordinate token |> Option.isSome)
                match coordToken |> Option.bind tryNormalizeCoordinate with
                | Some token -> Some $"bestmove {token.ToLower()}"
                | None ->
                    // Try SAN conversion
                    let sanCandidate =
                        parts
                        |> Array.map normalizeToken
                        |> Array.filter (fun t -> not (String.IsNullOrWhiteSpace(t)) && not (isEllipsis t))
                        |> Array.tryFindBack (fun t ->
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
    type WinboardHandler(logger: ILogger, configuredEngineName: string, flipped: bool) =

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

        member _.Features = features
        member _.IsInitialized = isInitialized
        member _.IsV1Fallback = isV1Fallback
        member _.IsProtover2 = isProtover2
        member _.ConfiguredName = configuredEngineName
        member _.Board = board
        /// Per CECP spec, reuse defaults to true when not explicitly set to 0
        member _.CanReuse = features.Reuse |> Option.defaultValue true
        /// Semaphore that signals when initialization completes
        member _.InitializationSemaphore = initSemaphore

        /// Get initialization commands
        member _.GetInitCommands() = [ "xboard"; "protover 2" ]

        /// Process a feature line during init. Returns true when done=1.
        member _.ProcessFeatureLine(line: string) =
            features <- parseFeatureLine line features
            if features.Done then
                isInitialized <- true
                isProtover2 <- true
                if initSemaphore.CurrentCount = 0 then
                    initSemaphore.Release() |> ignore
            features.Done

        /// Force V1 initialization with conservative defaults
        member _.ForceV1Init() =
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

        /// Mark as initialized (for V2 engines that didn't send done=1)
        member _.MarkInitialized() =
            isInitialized <- true
            if initSemaphore.CurrentCount = 0 then
                initSemaphore.Release() |> ignore
            logger.LogInformation($"Marked Winboard engine as initialized: Time={features.Time}, Analyze={features.Analyze}, Ping={features.Ping}")

        /// Get post-init commands
        member _.GetPostInitCommands() =
            [ "post"; "easy" ]

        /// Convert UCI command to Winboard command(s)
        member this.UciToWinboard(uciCommand: string) =
            let cmd = uciCommand.Trim()

            if cmd = "uci" then
                this.GetInitCommands()
            elif cmd = "isready" then
                if not isInitialized then
                    []
                elif features.Ping then
                    let pingId = System.Random().Next(1, 10000)
                    pendingPing <- Some pingId
                    [$"ping {pingId}"]
                else
                    // V1 engines don't support ping — considered ready after init
                    []
            elif cmd = "ucinewgame" then
                this.Reset()
                ["new"]
            elif cmd.StartsWith("position") then
                // Parse and apply position to internal board
                try
                    if cmd.Contains("startpos") then
                        board.ResetBoardState()
                    elif cmd.Contains("fen") then
                        board.ResetBoardState()
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

                    // Apply moves
                    let movesIdx = cmd.IndexOf("moves")
                    if movesIdx >= 0 && movesIdx + 6 <= cmd.Length then
                        let movesStr = cmd.Substring(movesIdx + 6).Trim()
                        if not (String.IsNullOrWhiteSpace(movesStr)) then
                            let moves = movesStr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                            for moveStr in moves do
                                match tryGetTMoveFromCoordinateNotation &board moveStr with
                                | Some tmove -> board.MakeMove(&tmove)
                                | None -> logger.LogWarning($"Could not parse move: {moveStr}")
                with ex ->
                    logger.LogError($"Error processing position command: {ex.Message}")

                // Generate Winboard commands
                // CECP requires force mode before changing position
                if features.SetBoard then
                    // Use full 6-field FEN — some engines (Chenard) require it.
                    // EXchess logs an error for the halfmove counter but still works.
                    let fen = board.FEN()
                    [ "force"; $"setboard {fen}" ]
                else
                    // V1 engines without setboard: send 'new' to reset to start position,
                    // then 'force' to enter force mode, then replay all moves.
                    // Without 'new', moves would be applied to the engine's current position.
                    "new" :: "force" :: positionToWinboard cmd features

            elif cmd.StartsWith("go") then
                let isWhite = board.Position.STM = 0uy
                let isInfinite = cmd.Contains("infinite")
                let useAnalyze = isInfinite && features.Analyze
                inAnalyzeMode <- useAnalyze
                this.GoToWinboard cmd isWhite

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

        /// Convert UCI go command to Winboard commands (member method to access state)
        member this.GoToWinboard (command: string) (isWhite: bool) =
            let commands = ResizeArray<string>()

            let wtimeMatch = wtimeRegex.Match(command)
            let btimeMatch = btimeRegex.Match(command)
            let wincMatch = wincRegex.Match(command)
            let bincMatch = bincRegex.Match(command)
            let movetimeMatch = movetimeRegex.Match(command)
            let isInfinite = command.Contains("infinite")

            if isInfinite then
                if features.Analyze then
                    commands.Add("analyze")
                else
                    commands.Add("go")
            else
                if movetimeMatch.Success then
                    // Fixed time per move
                    let timeMs = Int32.Parse(movetimeMatch.Groups.[1].Value)
                    let seconds = max 1 ((timeMs + 999) / 1000)
                    commands.Add($"st {seconds}")
                elif wtimeMatch.Success && btimeMatch.Success then
                    let wtimeMs = Int32.Parse(wtimeMatch.Groups.[1].Value)
                    let btimeMs = Int32.Parse(btimeMatch.Groups.[1].Value)
                    let wincMs = if wincMatch.Success then Int32.Parse(wincMatch.Groups.[1].Value) else 0
                    let bincMs = if bincMatch.Success then Int32.Parse(bincMatch.Groups.[1].Value) else 0

                    if isV1Fallback then
                        // V1 engines (like TheKing) only support time/otim
                        let wtime = wtimeMs / 10  // Convert ms to centiseconds
                        let btime = btimeMs / 10
                        if isWhite then
                            commands.Add($"time {wtime}")
                            commands.Add($"otim {btime}")
                        else
                            commands.Add($"time {btime}")
                            commands.Add($"otim {wtime}")
                    else
                        // V2 engines: Send level command ONCE per game to set time control
                        // Exception: TheTurk and Comet have broken level support despite advertising time=1
                        let engineNameLower = configuredEngineName.ToLower()
                        let hasBrokenLevel = engineNameLower.Contains("turk") || engineNameLower.Contains("comet")

                        if hasBrokenLevel then
                            // TheTurk and Comet have broken level support - use st (fixed seconds per move) instead
                            // Dynamically recalculate st every move based on remaining time and moves
                            let currentTimeMs = if isWhite then wtimeMs else btimeMs
                            let movesPlayed = board.PlyCount / 2  // Convert plies to full moves

                            // Estimate moves remaining: start at 40, decrease as game progresses, min 10
                            let estimatedMovesRemaining = max 10 (40 - movesPlayed)
                            let secondsPerMove = (currentTimeMs / 1000) / estimatedMovesRemaining

                            logger.LogDebug($"[{configuredEngineName}] Dynamic st: time={currentTimeMs}ms, movesPlayed={movesPlayed}, movesEst={estimatedMovesRemaining}, st={secondsPerMove}")
                            commands.Add($"st {secondsPerMove}")
                        elif not levelCommandSent then
                            // Store original time control from first go command (use the larger of the two times)
                            // This ensures we capture the actual starting time, not time after first move
                            if originalBaseTimeMs = 0 then
                                originalBaseTimeMs <- max wtimeMs btimeMs
                                originalIncrementMs <- max wincMs bincMs

                            // Convert to Winboard level format: "level MOVES BASE INC"
                            // MOVES=0 for sudden death, BASE can be "min" or "min:sec", INC in seconds
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
                            // Rounding: 0.5 → 1, 1.7 → 2, 2.0 → 2
                            let incRounded = int (incSeconds + 0.5)
                            let incFormatted = incRounded.ToString(System.Globalization.CultureInfo.InvariantCulture)

                            let levelCmd = $"level 0 {baseFormatted} {incFormatted}"
                            logger.LogInformation($"[{configuredEngineName}] Sending time control: {levelCmd} (base={originalBaseTimeMs}ms, inc={originalIncrementMs}ms)")
                            commands.Add(levelCmd)
                            levelCommandSent <- true

                        // Send time/otim for current clock state (every move) unless using st command
                        // Don't send time/otim when using st (fixed time per move) - they're mutually exclusive
                        if not hasBrokenLevel then
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

                commands.Add("go")

            commands |> Seq.toList

        /// Process a line of output from Winboard engine.
        /// Returns Some(uci_line) if translation is needed, None otherwise.
        member this.ProcessOutput(line: string) =
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
                    match pendingPing with
                    | Some id when pongId = Some id || pongId.IsNone ->
                        pendingPing <- None
                        Some "readyok"
                    | _ -> None

                elif trimmedLine.StartsWith("move ") || isMoveNotation trimmedLine then
                    match tryParseMoveOutput board trimmedLine with
                    | Some uciMove -> Some uciMove
                    | None ->
                        logger.LogWarning($"Winboard move not understood: {trimmedLine}")
                        None

                elif thinkingOutputRegex.IsMatch(trimmedLine) then
                    parseThinkingOutput flipped board configuredEngineName trimmedLine

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

        /// Enable setboard support (used after V1 probe succeeds)
        member _.EnableSetBoard() =
            features <- { features with SetBoard = true }

        /// Reset state for new game
        member _.Reset() =
            board.ResetBoardState()
            pendingPing <- None
            inAnalyzeMode <- false
            levelCommandSent <- false
            originalBaseTimeMs <- 0
            originalIncrementMs <- 0
