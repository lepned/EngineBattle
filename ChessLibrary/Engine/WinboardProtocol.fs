namespace ChessLibrary

open System
open System.Text.RegularExpressions
open Microsoft.Extensions.Logging
open PositionTypes
open TypesDef.CoreTypes
open Chess
open ChessLibrary.BoardUtils

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
    let private pvMoveNumberRegex = Regex(@"^\d+\.+", RegexOptions.Compiled)
    // The two ways the protocol (CECP, engine-intf section 9) lets an engine announce its move:
    // "move e2e4", and the older "NUMBER ... MOVE" ("1. ... e2e4", "1...e2e4" - Comet), where the
    // "..." is required even for White and "NUMBER MOVE" is ignored. Group 1 is the move.
    let private moveLineRegexes =
        [| Regex(@"^move\s+(\S+)\s*$", RegexOptions.Compiled)
           Regex(@"^\d+\.?\s*\.\.\.\s*(\S+)\s*$", RegexOptions.Compiled) |]
    // Lines that look like a move in a form the protocol does not define: "12. e4" (no "..."),
    // "My move is: e2e4", a move alone on its line. Not taken; logged once, so an engine that
    // announces its move this way is seen losing on time for a reason.
    let private nonProtocolMoveRegex =
        Regex(@"^(?:\d+\.\s*[A-Za-z]\S*|my\s+move\s+is\b.*|[a-h][1-8]-?[a-h][1-8](?:=?[qrbn])?)\s*$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    // A result claim: "1-0 {White mates}", "0-1 {White resigns}", "1/2-1/2 {Draw by repetition}"
    let private resultClaimRegex = Regex(@"^(1-0|0-1|1/2-1/2)(\s|$)", RegexOptions.Compiled)

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

    /// The name=value pairs of a feature line, in order; a quoted value keeps its spaces.
    /// Example: "feature ping=1 myname=\"Comet B.68\" done=1"
    let parseFeaturePairs (line: string) =
        let text = if line.StartsWith("feature") then line.Substring(7).Trim() else line
        let pairs = ResizeArray<string * string>()
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

                if key <> "" then pairs.Add((key, value))
                i <- nextStart
        List.ofSeq pairs

    /// Features EngineBattle answers `accepted`: the ones it uses and the ones that only tell it
    /// something. Any other is `rejected`, and so is san=1 - moves go to the engine in coordinate
    /// notation, which a rejected san tells it to expect.
    let private knownFeatures =
        set [ "ping"; "setboard"; "playother"; "usermove"; "time"; "draw"; "sigint"; "sigterm"; "reuse"
              "analyze"; "myname"; "variants"; "colors"; "ics"; "name"; "pause"; "nps"; "debug"; "memory"
              "smp"; "egt"; "option"; "done"; "exclude"; "setscore"; "highlight"; "san" ]

    /// The reply to one feature: "accepted ping", "rejected san".
    let featureReply (key: string, value: string) =
        if not (knownFeatures.Contains key) || (key = "san" && value = "1") then $"rejected {key}"
        else $"accepted {key}"

    /// Parse a single feature line from Winboard engine
    /// Example: "feature ping=1 setboard=1 done=1"
    let parseFeatureLine (line: string) (features: FeatureState) =
        parseFeaturePairs line
        |> List.fold (fun f (key, value) ->
            match key with
            | "ping" -> { f with Ping = value = "1" }
            | "setboard" -> { f with SetBoard = value = "1" }
            | "playother" -> { f with PlayOther = value = "1" }
            | "san" -> f  // rejected when 1: the engine gets coordinates (featureReply)
            | "usermove" -> { f with UserMove = value = "1" }
            | "time" -> { f with Time = value = "1" }
            | "reuse" -> { f with Reuse = Some (value = "1") }
            | "analyze" -> { f with Analyze = value = "1" }
            | "myname" -> { f with MyName = Some value }
            | "done" -> { f with Done = value = "1" }
            | _ -> f) features


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

    /// One PV token as the SAN/coordinate converter takes it, or "" for a token that is no move.
    /// Engines annotate their PV moves: Comet marks a fail-high or -low with "!" or "?" (its
    /// "g1f3?" came out as f2f3, its "f1b5!" ended the PV), TheTurk castles as "0-0" (which lost
    /// its hyphen and ended the PV), Crafty adds "<HT>" (hash table) and checks, some write "ep".
    let normalizePvToken (token: string) =
        let t = pvMoveNumberRegex.Replace(token.Trim(), "")   // "12." "12..." "12.Nf3"
        let t = t.TrimEnd([| '!'; '?'; '+'; '#' |])
        let t =
            if t.EndsWith("e.p.") then t.Substring(0, t.Length - 4)
            elif t.Length > 4 && t.EndsWith("ep") then t.Substring(0, t.Length - 2)  // "exd6ep"
            else t
        if t = "" || t = "..." || t = "ep" || t.StartsWith("tb=") || t.StartsWith("<") then ""  // Jonny's tb=, Crafty's <HT>
        else
            match t with
            | "0-0" | "00" | "O-O" | "o-o" -> "O-O"
            | "0-0-0" | "000" | "O-O-O" | "o-o-o" -> "O-O-O"
            | _ -> t.Replace("-", "")  // "e2-e4", "Ng1-f3"

    /// The PV part of a thinking line - the tokens after depth, score, time and nodes - in
    /// coordinate notation, played from `currentBoard`. Move numbers, "...", Jonny's tb= suffix and
    /// the '-' of long algebraic go first; SAN and coordinates are both understood.
    let thinkingPvToCoordinates (currentBoard: Chess.Board) (rawPv: string[]) =
        let normalized =
            rawPv
            |> Array.map normalizePvToken
            |> Array.filter (fun t -> t <> "")
        let sanPVline = String.Join(" ", normalized).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        let mutable board = currentBoard
        getLongSanPVFromShortSanPV moveList.Value &board sanPVline

    /// Parse Winboard thinking output to UCI info format, with the PV converted by `convertPv`
    /// (given the raw PV tokens). The handler passes a cached conversion; parseThinkingOutput the
    /// plain one.
    /// Winboard: "depth score time nodes pv..."
    /// Handles: standard, tab-indented SAN (Crafty), coordinate+tb (Jonny),
    ///          SAN prefix (EXchess), score*1000 (TheKing), kibitz (Comet)
    let parseThinkingOutputWith (convertPv: string[] -> string) (sideToMovePOV: bool) (currentBoard: Chess.Board) (engineName: string) (line: string) =
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

                // Score perspective: the line goes on as a UCI info line, and a UCI score is the
                // side to move's - EngineBattle turns it to White's for Black. That is what the
                // protocol expects of an engine too, so its score passes as it came (SideToMovePOV
                // false, the default). An engine that reports from White's point of view instead
                // (Crafty) is turned to the side to move here first (SideToMovePOV true - the
                // name says the opposite of what it does).
                let isBlackToMove = currentBoard.Position.STM <> 0uy
                let adjustedScore =
                    if sideToMovePOV && isBlackToMove then
                        -score  // White's point of view -> the side to move's (Black)
                    else
                        score  // already the side to move's

                // Convert PV from SAN to coordinate notation
                let pv = if parts.Length > 4 then convertPv parts.[4..] else ""

                let nps = if time > 0 then int64 ((float nodes) / (float time / 100.0)) else 0L

                Some $"info depth {depth} score cp {adjustedScore} time {time * 10} nodes {nodes} nps {nps} pv {pv}"
            with
            | _ -> None
        else
            None

    /// Parse Winboard thinking output to UCI info format (the PV converted afresh every time).
    let parseThinkingOutput (sideToMovePOV: bool) (currentBoard: Chess.Board) (engineName: string) (line: string) =
        parseThinkingOutputWith (thinkingPvToCoordinates currentBoard) sideToMovePOV currentBoard engineName line

    /// Parse Comet's tellics thinking output to UCI info format
    /// Format: "tellics  sc=+0.36 dp=10 nps=0K (h4h5 g6h7 b1c3 e7e6 g1e2)"
    let parseCometTellics (currentBoard: Chess.Board) (line: string) =
        let m = cometTellicsRegex.Match(line)
        if m.Success then
            try
                // Engine output, so invariant regardless of the host's locale.
                let scoreFloat = Double.Parse(m.Groups.[1].Value, Globalization.CultureInfo.InvariantCulture)
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

    /// The move token of a line that announces the engine's move, or None for any other line.
    let private moveToken (line: string) =
        let trimmed = line.Trim()
        moveLineRegexes
        |> Array.tryPick (fun r ->
            let m = r.Match(trimmed)
            if m.Success then Some m.Groups.[1].Value else None)

    /// Parse Winboard move output to UCI bestmove. Only a line that announces a move counts (see
    /// moveLineRegexes); its move may be in coordinates ("e2e4", "e2-e4", "e7e8q") or SAN. A
    /// first coordinate-looking token anywhere in a line used to be taken, so engine chatter
    /// ("Hint: e7e5", a printed PV) could pass for a move.
    let tryParseMoveOutput (board: Chess.Board) (line: string) =
        match moveToken line with
        | None -> None
        | Some token ->
            let t = token.Trim([|'.'; ','; ';'; ':'; '!'; '?'; '+'; '#'|])
            let coord = t.Replace("-", "").Replace("=", "")  // e2-e4, e7e8=q
            if coordinateNotationRegex.IsMatch(coord) then
                let coord = coord.ToLower()
                // A pawn promoting without the piece ("move a7a8") is a queen, as in the PV
                // (getLongSanPVFromShortSanPV); a piece moving to the last rank stays as it is.
                let mutable b = board
                if coord.Length = 4 && (tryGetTMoveFromUciNotation &b coord).IsNone
                   && (tryGetTMoveFromUciNotation &b (coord + "q")).IsSome then
                    Some $"bestmove {coord}q"
                else
                    Some $"bestmove {coord}"
            elif String.IsNullOrWhiteSpace t then
                None
            else
                // SAN; castling written with zeros as well as O's
                let san = if t = "0-0" then "O-O" elif t = "0-0-0" then "O-O-O" else t
                let mutable b = board
                let longSan = getLongSanPVFromShortSanPV moveList.Value &b [san]
                if String.IsNullOrWhiteSpace(longSan) then None
                else Some $"bestmove {longSan.Split([|' '|], StringSplitOptions.RemoveEmptyEntries).[0]}"

    /// Check if a line announces a move
    let isMoveNotation (line: string) =
        not (String.IsNullOrWhiteSpace line) && (moveToken line).IsSome

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
        // accepted/rejected owed for the feature lines read (TakeFeatureReplies), and done=0 seen
        // without a done=1 after it: the engine asked for more time to start
        let pendingReplies = ResizeArray<string>()
        let mutable awaitingDone = false
        // What the last position command became, named when the engine rejects a command
        let mutable lastPositionCommands : string list = []
        // A move-like line outside the protocol's forms has been reported (once is enough)
        let mutable nonProtocolMoveReported = false
        // The rejections already logged in full: EXchess answers every 6-field setboard with
        // "Error (unknown command): 0" and plays on, which is no news after the first time
        let reportedRejections = System.Collections.Generic.HashSet<string>()
        // The last thinking-line PV converted to coordinates, and the board it was converted on.
        // Engines repeat one PV over many thinking lines, and the SAN conversion was nearly all
        // of this handler's cost per line; boardVersion moves whenever the board does, so a PV
        // is never reused against a different position. Guarded by stateLock like the board.
        let mutable boardVersion = 0
        let mutable pvCacheVersion = -1
        let mutable pvCacheRaw : string[] = [||]
        let mutable pvCacheResult = ""
        let cachedPv (rawPv: string[]) =
            if pvCacheVersion = boardVersion && rawPv.Length = pvCacheRaw.Length && Array.forall2 (=) rawPv pvCacheRaw then
                pvCacheResult
            else
                let converted = thinkingPvToCoordinates board rawPv
                pvCacheVersion <- boardVersion
                pvCacheRaw <- rawPv
                pvCacheResult <- converted
                converted

        member _.Features = lock stateLock (fun () -> features)
        member _.IsInitialized = lock stateLock (fun () -> isInitialized)
        member _.IsV1Fallback = lock stateLock (fun () -> isV1Fallback)
        member _.IsProtover2 = lock stateLock (fun () -> isProtover2)
        /// Per CECP spec, reuse defaults to true when not explicitly set to 0
        member _.CanReuse = lock stateLock (fun () -> features.Reuse |> Option.defaultValue true)
        /// Get the configured time control strategy
        member _.ConfiguredTimeControlStrategy = winboardConfig.TimeControlStrategy

        /// Get initialization commands
        member _.GetInitCommands() = [ "xboard"; "protover 2" ]

        /// Process a feature line during init. Returns true when done=1.
        member _.ProcessFeatureLine(line: string) =
            lock stateLock (fun () ->
                let pairs = parseFeaturePairs line
                for pair in pairs do pendingReplies.Add(featureReply pair)
                match pairs |> List.tryFind (fun (key, _) -> key = "done") with
                | Some (_, "0") -> awaitingDone <- true
                | Some _ -> awaitingDone <- false
                | None -> ()
                features <- parseFeatureLine line features
                if features.Done then
                    isInitialized <- true
                    isProtover2 <- true
                    if initSemaphore.CurrentCount = 0 then
                        initSemaphore.Release() |> ignore
                features.Done
            )

        /// The accepted/rejected replies owed for the feature lines read so far (the protocol
        /// expects one per feature); taking them empties the list.
        member _.TakeFeatureReplies() =
            lock stateLock (fun () ->
                let replies = List.ofSeq pendingReplies
                pendingReplies.Clear()
                replies)

        /// done=0 read and no done=1 since: the engine asked not to be timed out while it starts.
        member _.AwaitingDone = lock stateLock (fun () -> awaitingDone)

        /// The pause between the lines of a burst (WinboardConfig.CommandDelayMs).
        member _.CommandDelayMs = winboardConfig.CommandDelayMs

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
            // Sudden death: an engine that keeps the dummy's moves per period (Comet) budgets for a
            // clock it thinks comes back at move 40 - 1.5-3 s a move with 3-12 s left there
            let levelCmd = if winboardConfig.RequiresLevelForThinkingOutput then ["level 0 5 0"] else []
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
                    boardVersion <- boardVersion + 1
                    let fen = board.FEN()
                    try
                        // "position startpos moves ..." is from the start position, not on top of
                        // the last one
                        if cmd.StartsWith("position startpos") then
                            board.ResetBoardState()
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
                                    board.PlayUciMove(moveStr)
                    with ex ->
                        logger.LogError($"Error processing position command: {ex.Message}")

                    // Send "new" based on engine capabilities:
                    // - Engines WITH setboard: Send "new" only once (setboard sets absolute position)
                    // - Engines WITHOUT setboard: ALWAYS send "new" (moves are relative to startpos)
                    let positionCmds = positionToWinboard cmd features winboardConfig.Use4FieldFen &board
                    let sent =
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
                    lastPositionCommands <- sent
                    sent

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
            // An opponent with no clock (a node limit, a time per move) arrives as 0. Sent as
            // `otim 0`, engines read it as an opponent out of time and play instantly (EXchess,
            // Comet and CraftyOld moved in 0.00-0.02 s instead of 1-2 s), so the engine's own time
            // stands in for it.
            let wtimeMs, btimeMs =
                if isWhite && btimeMs <= 0 then wtimeMs, wtimeMs
                elif not isWhite && wtimeMs <= 0 then btimeMs, btimeMs
                else wtimeMs, btimeMs
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

            // `level` takes whole seconds. Rounded down, so the engine is never told more increment
            // than it gets (0.5 s as 1 made Crafty spend 0.43 s with 0.8 s left); an engine that
            // needs a whole second to search at all says so with MinLevelIncrement. That raises an
            // increment there is (10+0.1); a game without one (10+0) is sent as it is.
            let incFloor = int (Math.Floor incSeconds)
            let incWhole = if incSeconds > 0.0 then max incFloor winboardConfig.MinLevelIncrement else incFloor
            let incFormatted = incWhole.ToString(System.Globalization.CultureInfo.InvariantCulture)

            let levelCmd = $"level 0 {baseFormatted} {incFormatted}"
            logger.LogInformation($"[{configuredEngineName}] Sending time control: {levelCmd} (base={originalBaseTimeMs}ms, inc={originalIncrementMs}ms)")
            commands.Add(levelCmd)
            levelCommandSent <- true

        /// Send dynamic st command (for engines with broken level support)
        member private _.SendDynamicStCommand (isWhite: bool) (wtimeMs: int) (btimeMs: int) (incMs: int) (commands: ResizeArray<string>) =
            let currentTimeMs = if isWhite then wtimeMs else btimeMs
            let movesPlayed = board.PlyCount / 2  // Convert plies to full moves

            // Estimate moves remaining: start at 40, decrease as game progresses, min 10
            let estimatedMovesRemaining = max 10 (40 - movesPlayed)
            // The share of the clock plus the increment, in whole seconds (st takes whole seconds;
            // TheTurk reads st 0.5 as 0). Whole seconds of the clock divided first gave 0 for any
            // base under 40 s, and st 0 is no search at all: TheTurk played depth 1 in 3 ms.
            // Rounded down, never up - rounding to the nearest second gave st 1 for 0.5 s a move and
            // TheTurk lost on time: 30+0.5 gives st 1 (1.25 s a move), 60+1 st 2 (2.5 s); under a
            // second a move is st 0, the engine's fastest move.
            // Never more than half the clock: the increment comes after the move, so with 1 s or
            // more of it the share alone kept st at 1 down to a second left (3+1 drained the clock
            // by the overhead each move until the flag fell).
            let perMoveMs = min (currentTimeMs / estimatedMovesRemaining + incMs) (currentTimeMs / 2)
            let secondsPerMove = perMoveMs / 1000

            if secondsPerMove < 1 then
                logger.LogWarning($"[{configuredEngineName}] {currentTimeMs} ms left: st 0, the engine's fastest move")
                commands.Add("st 0")
            else
                logger.LogDebug($"[{configuredEngineName}] Dynamic st: time={currentTimeMs}ms, inc={incMs}ms, movesPlayed={movesPlayed}, movesEst={estimatedMovesRemaining}, st={secondsPerMove}")
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
                        this.SendDynamicStCommand isWhite wtimeMs btimeMs (if isWhite then wincMs else bincMs) commands
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                    | TypesDef.CoreTypes.TimeControlStrategy.StOnly ->
                        // Legacy mode: st only (may cause poor time management)
                        this.SendDynamicStCommand isWhite wtimeMs btimeMs (if isWhite then wincMs else bincMs) commands

                    | TypesDef.CoreTypes.TimeControlStrategy.AutoDetect ->
                        // Should be resolved by now, but fallback to TimeOtimOnly
                        logger.LogWarning($"AutoDetect strategy not resolved for {configuredEngineName}, using TimeOtimOnly")
                        this.SendTimeOtimCommands isWhite wtimeMs btimeMs commands

                commands.Add("go")

            commands |> Seq.toList

        /// A move the engine sends is checked against the position it was given. One that is not
        /// legal there is not this game's: a move the engine finished after the last game ended
        /// on its turn (a loss on time), still in the pipe, was read as the first move of the next
        /// game and lost it as an illegal move. Ignored, with a warning, and the engine's real
        /// move follows.
        member private _.LegalOrDropped(translated: string option, raw: string) =
            match translated with
            | Some bm when bm.StartsWith("bestmove ") ->
                let move = bm.Substring("bestmove ".Length).Trim().Split(' ').[0]
                if (tryGetTMoveFromUciNotation &board move).IsSome then translated
                else
                    logger.LogWarning($"[{configuredEngineName}] Ignored '{raw}': {move} is not legal in the position the engine was given (left over from an earlier search?)")
                    None
            | other -> other

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

                    elif trimmedLine = "resign" || trimmedLine.StartsWith("resign ") then
                        // No UCI equivalent: the game loop scores "bestmove resign" as a resignation.
                        // It waited out the engine's clock before.
                        logger.LogInformation($"[{configuredEngineName}] resigns")
                        Some "bestmove resign"

                    elif resultClaimRegex.IsMatch(trimmedLine) then
                        // A claim of its own loss, made on its move, is a resignation ("0-1 {White
                        // resigns}"); a win or a draw it claims, EngineBattle judges itself.
                        let claim = resultClaimRegex.Match(trimmedLine).Groups.[1].Value
                        let whiteToMove = board.Position.STM = 0uy
                        if (claim = "0-1" && whiteToMove) || (claim = "1-0" && not whiteToMove) then
                            logger.LogInformation($"[{configuredEngineName}] resigns: {trimmedLine}")
                            Some "bestmove resign"
                        else
                            logger.LogDebug($"[{configuredEngineName}] claims {trimmedLine} (judged by EngineBattle)")
                            None

                    elif isMoveNotation trimmedLine then
                        match tryParseMoveOutput board trimmedLine with
                        | None ->
                            logger.LogWarning($"[{configuredEngineName}] Winboard move not understood: {trimmedLine}")
                            None
                        | parsed -> this.LegalOrDropped(parsed, trimmedLine)  // it warns itself

                    elif thinkingOutputRegex.IsMatch(trimmedLine) then
                        parseThinkingOutputWith cachedPv winboardConfig.SideToMovePOV board configuredEngineName trimmedLine

                    elif cometTellicsRegex.IsMatch(trimmedLine) then
                        parseCometTellics board trimmedLine

                    // "Illegal move" also as "illegal move", with or without the colon (the protocol)
                    elif trimmedLine.StartsWith("Error") || trimmedLine.StartsWith("Illegal", StringComparison.OrdinalIgnoreCase) then
                        // Fast-fail V1 detection: if engine doesn't understand protover, it's V1
                        if trimmedLine.Contains("protover") && not isInitialized then
                            logger.LogWarning($"Winboard engine error: {trimmedLine}")
                            logger.LogInformation($"[{configuredEngineName}] V1 engine detected (protover error), applying fallback immediately")
                            this.ForceV1Init()
                        elif isInitialized && not lastPositionCommands.IsEmpty && reportedRejections.Add(trimmedLine) then
                            // In a game this is why an engine that then sits silent until its flag
                            // falls does so: it refused the position (a FEN it cannot read) or a
                            // time command. Named once with what it was sent; some engines say the
                            // same harmless thing on every move.
                            let sentText = String.Join(" | ", lastPositionCommands)
                            logger.LogWarning($"[{configuredEngineName}] rejected a command: '{trimmedLine}'. Position sent last: {sentText}")
                        elif isInitialized && not lastPositionCommands.IsEmpty then
                            logger.LogDebug($"[{configuredEngineName}] rejected a command again: '{trimmedLine}'")
                        else
                            logger.LogWarning($"[{configuredEngineName}] Winboard engine error: {trimmedLine}")
                        None

                    elif trimmedLine.StartsWith("#") || trimmedLine.StartsWith("tellics") || trimmedLine.StartsWith("tellusers") || trimmedLine.StartsWith("kibitz") then
                        None

                    elif trimmedLine = "++" || trimmedLine = "--" then
                        None

                    elif nonProtocolMoveRegex.IsMatch(trimmedLine) then
                        if not nonProtocolMoveReported then
                            nonProtocolMoveReported <- true
                            logger.LogWarning($"[{configuredEngineName}] '{trimmedLine}' looks like a move but is not in a form the Winboard protocol defines ('move e2e4' or '1. ... e2e4'); ignored. An engine that announces its moves this way will not be heard.")
                        else
                            logger.LogDebug($"Winboard output (ignored, not a protocol move): {trimmedLine}")
                        None

                    else
                        // Not a move: only the formats in moveLineRegexes announce one
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
                boardVersion <- boardVersion + 1
                pendingPing <- None
                inAnalyzeMode <- false
                levelCommandSent <- false
                originalBaseTimeMs <- 0
                originalIncrementMs <- 0
                gameInitialized <- false  // Will send "new" for next game
                lastPositionCommands <- []
            )
