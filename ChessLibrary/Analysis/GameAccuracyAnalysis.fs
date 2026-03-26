module ChessLibrary.GameAccuracyAnalysis

open System
open System.Threading
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.Engine

// ──────────────────────────────────────────────────────────────
// Types
// ──────────────────────────────────────────────────────────────

type SearchMode = Time | Nodes | Depth

type GameReviewConfig =
    { SearchMode: SearchMode
      TimePerMove: int   // ms
      Nodes: int
      Depth: int
      MultiPV: int       // default 3
      SkipBookMoves: bool }
    static member Default =
        { SearchMode = Time; TimePerMove = 1000; Nodes = 1000000; Depth = 18; MultiPV = 3; SkipBookMoves = true }

type MoveClassification =
    | Book
    | Forced
    | Brilliant
    | Great
    | Best
    | Excellent
    | Good
    | Inaccuracy
    | Mistake
    | Blunder
    override this.ToString() =
        match this with
        | Book -> "Book" | Forced -> "Forced"
        | Brilliant -> "!!" | Great -> "!" | Best -> "Best"
        | Excellent -> "Excellent" | Good -> "Good"
        | Inaccuracy -> "?!" | Mistake -> "?" | Blunder -> "??"

type MultiPVResult =
    { Eval: EvalType      // from white's perspective (pawn units)
      BestMove: string    // UCI notation
      PV: string
      Depth: int
      Nodes: int64 }

type MoveAnalysisResult =
    { Ply: int
      MoveNumber: int
      Color: string            // "w" or "b"
      San: string
      UciMove: string
      Classification: MoveClassification
      EvalBefore: EvalType     // best eval of position before move (white's perspective)
      BestMove: string         // engine's #1 (UCI)
      BestMoveSan: string      // engine's #1 (SAN)
      BestEval: EvalType       // = EvalBefore (PV1, white's perspective)
      SecondBestEval: EvalType option // PV2 eval (white's perspective)
      WinProbBefore: float     // WP from STM's perspective
      WinProbAfter: float      // WP from STM's perspective after played move
      WinProbLoss: float       // >= 0
      CentipawnLoss: float     // >= 0
      MoveAccuracy: float      // 0-100
      Depth: int
      Nodes: int64
      PV: string }

type PlayerAccuracyStats =
    { Player: string
      Accuracy: float
      ACPL: float
      MoveCount: int
      Classifications: Map<MoveClassification, int>
      OpeningAccuracy: float
      MiddlegameAccuracy: float
      EndgameAccuracy: float }
    static member Empty name =
        { Player = name; Accuracy = 0.0; ACPL = 0.0; MoveCount = 0
          Classifications = Map.empty; OpeningAccuracy = 0.0
          MiddlegameAccuracy = 0.0; EndgameAccuracy = 0.0 }

type GameAnalysisResult =
    { Moves: MoveAnalysisResult array
      WhiteStats: PlayerAccuracyStats
      BlackStats: PlayerAccuracyStats
      WhitePlayer: string
      BlackPlayer: string
      AnalysisEngine: string
      AnalysisDate: DateTime }

// ──────────────────────────────────────────────────────────────
// Win Probability & Accuracy
// ──────────────────────────────────────────────────────────────

/// Negate an eval (flip perspective)
let flipEval (eval: EvalType) =
    match eval with
    | CP cp -> CP (-cp)
    | Mate m -> Mate (-m)
    | NA -> NA

/// Convert eval to win probability [0, 1] for the side whose perspective the eval is in.
/// Uses a logistic model with ply-dependent scaling (Stockfish NormalizeToPawnValue style).
let evalToWinProb (eval: EvalType) (ply: int) : float =
    match eval with
    | Mate m ->
        if m > 0 then 1.0
        elif m < 0 then 0.0
        else 0.5
    | CP cp ->
        let k = 345.0 - 1.4 * float (min ply 60)
        let cpInCentipawns = cp * 100.0  // EvalType.CP stores pawn units
        1.0 / (1.0 + exp(-cpInCentipawns / k))
    | NA -> 0.5

/// Per-move accuracy from WP loss (0 to 1 scale).
/// Returns 0-100. Formula: 103.1668 * exp(-0.04354 * wpLoss%) - 3.1669
let calculateMoveAccuracy (wpLoss: float) : float =
    let wpLossPercent = wpLoss * 100.0
    let raw = 103.1668 * exp(-0.04354 * wpLossPercent) - 3.1669
    max 0.0 (min 100.0 raw)

/// Classify a move based on WP loss, whether it matches engine's best, multi-PV gap, and sacrifice.
let classifyMove (wpLoss: float) (isBestMove: bool) (isForced: bool) (pvGap: float option) (isSacrifice: bool) : MoveClassification =
    if isForced then Forced
    elif isBestMove then
        match pvGap with
        | Some gap when gap > 0.15 && isSacrifice -> Brilliant
        | Some gap when gap > 0.10 -> Great
        | _ -> Best
    elif wpLoss < 0.02 then Excellent
    elif wpLoss < 0.05 then Good
    elif wpLoss < 0.10 then Inaccuracy
    elif wpLoss < 0.20 then Mistake
    else Blunder

// ──────────────────────────────────────────────────────────────
// Sacrifice Detection (simplified v1)
// ──────────────────────────────────────────────────────────────

/// Simple piece values for sacrifice detection
let private pieceValue (c: char) =
    match Char.ToLower c with
    | 'p' -> 1 | 'n' -> 3 | 'b' -> 3 | 'r' -> 5 | 'q' -> 9 | 'k' -> 0 | _ -> 0

/// Count total material from a FEN position string
let countMaterial (fen: string) =
    let boardPart = fen.Split(' ').[0]
    let mutable white = 0
    let mutable black = 0
    for c in boardPart do
        if Char.IsUpper c then white <- white + pieceValue c
        elif Char.IsLower c then black <- black + pieceValue c
    white, black

/// Detect if a move is likely a sacrifice: the moving side loses material immediately
/// by comparing material before and after the move on the board.
let isSacrifice (board: Board) (uciMove: string) : bool =
    let fenBefore = board.FEN()
    let whiteBefore, blackBefore = countMaterial fenBefore
    let isWhite = board.Position.STM = 0uy
    let materialBefore = if isWhite then whiteBefore else blackBefore

    // Play the move on a copy
    let testBoard = Board()
    testBoard.LoadFen fenBefore
    try
        testBoard.PlayUciMove uciMove
        let fenAfter = testBoard.FEN()
        let whiteAfter, blackAfter = countMaterial fenAfter
        let materialAfter = if isWhite then whiteAfter else blackAfter
        let opponentBefore = if isWhite then blackBefore else blackAfter
        let opponentAfter = if isWhite then blackAfter else whiteAfter

        // Sacrifice: our material dropped by more than what we captured
        let ourLoss = materialBefore - materialAfter
        let theirLoss = opponentBefore - opponentAfter
        ourLoss > theirLoss && ourLoss > 0
    with _ -> false

// ──────────────────────────────────────────────────────────────
// Player Stats Computation
// ──────────────────────────────────────────────────────────────

let computePlayerStats (playerName: string) (moves: MoveAnalysisResult array) (color: string) : PlayerAccuracyStats =
    let playerMoves = moves |> Array.filter (fun m -> m.Color = color)
    let classifiable = playerMoves |> Array.filter (fun m -> m.Classification <> Book && m.Classification <> Forced)

    let accuracy =
        if classifiable.Length = 0 then 0.0
        else classifiable |> Array.averageBy (fun m -> m.MoveAccuracy)

    let acpl =
        if classifiable.Length = 0 then 0.0
        else classifiable |> Array.averageBy (fun m -> m.CentipawnLoss)

    let classificationCounts =
        playerMoves
        |> Array.groupBy (fun m -> m.Classification)
        |> Array.map (fun (cls, ms) -> cls, ms.Length)
        |> Map.ofArray

    let phaseAccuracy (minMove: int) (maxMove: int) =
        let phaseMoves =
            classifiable
            |> Array.filter (fun m -> m.MoveNumber >= minMove && m.MoveNumber <= maxMove)
        if phaseMoves.Length = 0 then 0.0
        else phaseMoves |> Array.averageBy (fun m -> m.MoveAccuracy)

    { Player = playerName
      Accuracy = Math.Round(accuracy, 1)
      ACPL = Math.Round(acpl, 1)
      MoveCount = playerMoves.Length
      Classifications = classificationCounts
      OpeningAccuracy = Math.Round(phaseAccuracy 1 15, 1)
      MiddlegameAccuracy = Math.Round(phaseAccuracy 16 40, 1)
      EndgameAccuracy = Math.Round(phaseAccuracy 41 Int32.MaxValue, 1) }

// ──────────────────────────────────────────────────────────────
// Engine Position Analysis (MultiPV)
// ──────────────────────────────────────────────────────────────

/// Analyze a single position with MultiPV. Returns array of MultiPVResult (one per PV line)
/// plus the bestmove string from the engine.
let analyzePosition (engine: ChessEngine) (positionCmd: string) (isWhite: bool) (config: GameReviewConfig) : MultiPVResult array * string =
    engine.UciNewGame()
    engine.Position positionCmd
    let ok = engine.WaitForReadyOk()
    if not ok then [||], ""
    else

    // Send go command
    match config.SearchMode with
    | Time -> engine.Go(config.TimePerMove)
    | Nodes -> engine.GoNodes(config.Nodes)
    | Depth -> engine.Write(sprintf "go depth %d" config.Depth)

    // Collect MultiPV results — keep only the latest info per multipv index
    let pvResults = Collections.Generic.Dictionary<int, MultiPVResult>()
    let mutable bestMoveStr = ""
    let mutable cont = true

    while cont do
        let line = engine.ReadLine()
        if String.IsNullOrEmpty line then ()
        elif line.StartsWith "bestmove" then
            let parts = line.Split(' ')
            bestMoveStr <- if parts.Length > 1 then parts.[1] else ""
            cont <- false
        elif line.StartsWith "info" && line.Contains "depth" then
            match EngineProtocol.Regex.getEssentialData line isWhite with
            | Some (d, eval, nodes, _nps, pvLine, _tbHits, _wdl, _sd, mPv) ->
                let pvIndex = if mPv = 0 then 1 else mPv
                let bestMove =
                    if String.IsNullOrEmpty pvLine then ""
                    else pvLine.Split(' ').[0]
                pvResults.[pvIndex] <-
                    { Eval = eval
                      BestMove = bestMove
                      PV = pvLine
                      Depth = d
                      Nodes = nodes }
            | None -> ()

    let results =
        pvResults
        |> Seq.sortBy (fun kv -> kv.Key)
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.toArray

    results, bestMoveStr

// ──────────────────────────────────────────────────────────────
// Full Game Analysis (engine mode)
// ──────────────────────────────────────────────────────────────

/// Analyze a complete game with an engine. Returns per-move analysis results.
/// progressCallback receives (currentMove, totalMoves).
let analyzeGameWithEngine
    (engine: ChessEngine)
    (game: PgnGame)
    (config: GameReviewConfig)
    (progressCallback: int -> int -> unit)
    (ct: CancellationToken)
    : GameAnalysisResult =

    let board = Board()
    let startFen =
        if String.IsNullOrWhiteSpace game.Fen then startPosition
        else game.Fen
    board.LoadFen startFen
    board.StartPosition <- startFen

    let totalMoves = game.Mainline.Count

    // Set MultiPV
    if config.MultiPV > 1 then
        engine.Write(sprintf "setoption name MultiPV value %d" config.MultiPV)

    // Single-pass: analyze each position BEFORE the move is played, then play the move.
    // Collect: white-perspective eval, MultiPV data, bestmove, and UCI notation per move.
    let whiteEvals = ResizeArray<EvalType>()
    let pvDataPerMove = ResizeArray<MultiPVResult array>()
    let bestMoves = ResizeArray<string>()
    let uciMoves = ResizeArray<string>()

    for i in 0 .. totalMoves - 1 do
        if ct.IsCancellationRequested then ()
        else
            progressCallback (i + 1) (totalMoves + 1) // +1 for final position analysis

            let move = game.Mainline.[i]
            let isWhite = board.Position.STM = 0uy

            // Check legal move count for Forced detection
            let legalMoves = board.GetLegalMoves() |> Seq.length

            // Check if book move (from PGN comment)
            let isBookMove =
                config.SkipBookMoves &&
                not (String.IsNullOrEmpty move.Comment) &&
                move.Comment.ToLower().Contains("book")

            if isBookMove || legalMoves <= 1 then
                whiteEvals.Add(EvalType.NA)
                pvDataPerMove.Add([||])
                bestMoves.Add("")
            else
                // Analyze position before this move
                let posCmd = board.PositionWithMoves()
                let pvResults, bestMove = analyzePosition engine posCmd isWhite config

                let bestEval =
                    if pvResults.Length > 0 then pvResults.[0].Eval
                    else EvalType.NA
                whiteEvals.Add(bestEval)
                pvDataPerMove.Add(pvResults)
                bestMoves.Add(bestMove)

            // Get UCI move for the played move and play it
            let uci = board.GetUciFromSan(move.San) |> Option.defaultValue ""
            uciMoves.Add(uci)
            try board.PlaySanMove move.San
            with _ -> ()

    // Analyze final position to get eval after last move
    if not ct.IsCancellationRequested && totalMoves > 0 then
        progressCallback (totalMoves + 1) (totalMoves + 1)
        let isWhite = board.Position.STM = 0uy
        let posCmd = board.PositionWithMoves()
        let pvResults, _ = analyzePosition engine posCmd isWhite config
        let finalEval =
            if pvResults.Length > 0 then pvResults.[0].Eval
            else EvalType.NA
        whiteEvals.Add(finalEval)

    // Reset MultiPV to 1
    if config.MultiPV > 1 then
        engine.Write("setoption name MultiPV value 1")

    // Build MoveAnalysisResult for each move
    let results = ResizeArray<MoveAnalysisResult>()
    let replayBoard = Board()
    replayBoard.LoadFen startFen
    replayBoard.StartPosition <- startFen

    for i in 0 .. totalMoves - 1 do
        let move = game.Mainline.[i]
        let isWhite = replayBoard.Position.STM = 0uy
        let ply = i

        let legalMoves = replayBoard.GetLegalMoves() |> Seq.length

        let isBookMove =
            config.SkipBookMoves &&
            not (String.IsNullOrEmpty move.Comment) &&
            move.Comment.ToLower().Contains("book")

        let uciMove = if i < uciMoves.Count then uciMoves.[i] else ""

        let evalBefore = whiteEvals.[i]
        let evalAfter =
            if i + 1 < whiteEvals.Count then whiteEvals.[i + 1]
            else EvalType.NA

        // Compute WP from the moving side's perspective
        let stmEvalBefore = if isWhite then evalBefore else flipEval evalBefore
        let stmEvalAfter = if isWhite then evalAfter else flipEval evalAfter

        let wpBefore = evalToWinProb stmEvalBefore ply
        let wpAfter = evalToWinProb stmEvalAfter (ply + 1)
        let wpLoss = max 0.0 (wpBefore - wpAfter)

        // Centipawn loss
        let cpLoss =
            match stmEvalBefore, stmEvalAfter with
            | CP before, CP after -> max 0.0 ((before - after) * 100.0)
            | _ -> wpLoss * 500.0  // rough approximation for mate evals

        let moveAccuracy = calculateMoveAccuracy wpLoss

        // Best move info
        let bestMoveUci = bestMoves.[i]
        let isBestMove = not (String.IsNullOrEmpty bestMoveUci) && uciMove = bestMoveUci
        let bestMoveSan =
            if String.IsNullOrEmpty bestMoveUci then ""
            else
                match replayBoard.GetSanFromUci bestMoveUci with
                | Some san -> san
                | None -> bestMoveUci

        // Multi-PV gap for Brilliant/Great detection
        let pvData = pvDataPerMove.[i]
        let pvGap =
            if pvData.Length >= 2 then
                let pv1Eval = if isWhite then pvData.[0].Eval else flipEval pvData.[0].Eval
                let pv2Eval = if isWhite then pvData.[1].Eval else flipEval pvData.[1].Eval
                let wp1 = evalToWinProb pv1Eval ply
                let wp2 = evalToWinProb pv2Eval ply
                Some (wp1 - wp2)
            else None

        // Sacrifice detection
        let sacrifice =
            if isBestMove && not (String.IsNullOrEmpty uciMove) then
                isSacrifice replayBoard uciMove
            else false

        // Classification
        let classification =
            if isBookMove then Book
            elif legalMoves <= 1 then Forced
            else classifyMove wpLoss isBestMove false pvGap sacrifice

        let depth = if pvData.Length > 0 then pvData.[0].Depth else 0
        let nodes = if pvData.Length > 0 then pvData.[0].Nodes else 0L
        let pv = if pvData.Length > 0 then pvData.[0].PV else ""

        let secondBestEval =
            if pvData.Length >= 2 then Some pvData.[1].Eval else None

        results.Add(
            { Ply = ply
              MoveNumber = move.MoveNumber
              Color = move.Color
              San = move.San
              UciMove = uciMove
              Classification = classification
              EvalBefore = evalBefore
              BestMove = bestMoveUci
              BestMoveSan = bestMoveSan
              BestEval = evalBefore
              SecondBestEval = secondBestEval
              WinProbBefore = wpBefore
              WinProbAfter = wpAfter
              WinProbLoss = wpLoss
              CentipawnLoss = cpLoss
              MoveAccuracy = moveAccuracy
              Depth = depth
              Nodes = nodes
              PV = pv })

        try replayBoard.PlaySanMove move.San
        with _ -> ()

    let movesArray = results.ToArray()

    { Moves = movesArray
      WhiteStats = computePlayerStats (game.GameMetaData.White) movesArray "w"
      BlackStats = computePlayerStats (game.GameMetaData.Black) movesArray "b"
      WhitePlayer = game.GameMetaData.White
      BlackPlayer = game.GameMetaData.Black
      AnalysisEngine = engine.Name
      AnalysisDate = DateTime.UtcNow }

// ──────────────────────────────────────────────────────────────
// Quick Analysis from PGN Annotations
// ──────────────────────────────────────────────────────────────

/// Analyze a game using existing eval annotations in PGN comments (wv= fields).
/// No engine needed. Cannot detect Brilliant/Great (no multi-PV data).
let analyzeGameFromAnnotations (game: PgnGame) : GameAnalysisResult option =
    let board = Board()
    let startFen =
        if String.IsNullOrWhiteSpace game.Fen then startPosition
        else game.Fen
    board.LoadFen startFen
    board.StartPosition <- startFen

    let totalMoves = game.Mainline.Count
    if totalMoves = 0 then None
    else

    // Extract white-perspective evals from PGN annotations
    let whiteEvals = ResizeArray<EvalType>()
    let replayBoard = Board()
    replayBoard.LoadFen startFen
    replayBoard.StartPosition <- startFen

    for move in game.Mainline do
        let isBlack = replayBoard.Position.STM <> 0uy
        let player = if isBlack then game.GameMetaData.Black else game.GameMetaData.White
        let stat = Annotation.getEngineStatData player isBlack move.Comment
        // stat.wv is already from white's perspective (Annotation negates for black)
        let eval =
            if stat.n = 0L && stat.d = 0 && stat.wv = 0.0 then EvalType.NA
            else CP stat.wv
        whiteEvals.Add(eval)
        try replayBoard.PlaySanMove move.San with _ -> ()

    // Check if we have any actual evals
    let hasEvals = whiteEvals |> Seq.exists (fun e -> e <> EvalType.NA)
    if not hasEvals then None
    else

    // Build move analysis results
    let results = ResizeArray<MoveAnalysisResult>()
    let board2 = Board()
    board2.LoadFen startFen
    board2.StartPosition <- startFen

    for i in 0 .. totalMoves - 1 do
        let move = game.Mainline.[i]
        let isWhite = board2.Position.STM = 0uy
        let ply = i

        let legalMoves = board2.GetLegalMoves() |> Seq.length

        let isBookMove =
            not (String.IsNullOrEmpty move.Comment) &&
            move.Comment.ToLower().Contains("book")

        let uciMove = board2.GetUciFromSan(move.San) |> Option.defaultValue ""

        let evalBefore = whiteEvals.[i]
        let evalAfter =
            if i + 1 < whiteEvals.Count then whiteEvals.[i + 1]
            else EvalType.NA

        let stmEvalBefore = if isWhite then evalBefore else flipEval evalBefore
        let stmEvalAfter = if isWhite then evalAfter else flipEval evalAfter

        let wpBefore = evalToWinProb stmEvalBefore ply
        let wpAfter = evalToWinProb stmEvalAfter (ply + 1)
        let wpLoss = max 0.0 (wpBefore - wpAfter)

        let cpLoss =
            match stmEvalBefore, stmEvalAfter with
            | CP before, CP after -> max 0.0 ((before - after) * 100.0)
            | _ -> wpLoss * 500.0

        let moveAccuracy = calculateMoveAccuracy wpLoss

        // No multi-PV data from annotations — can't detect Brilliant/Great
        // Mark all non-losing moves as Best if WP loss < 2%, otherwise classify by WP loss
        let classification =
            if isBookMove then Book
            elif legalMoves <= 1 then Forced
            elif evalBefore = EvalType.NA || evalAfter = EvalType.NA then Good
            elif wpLoss < 0.02 then Best  // Cannot distinguish Best from Excellent without bestmove
            elif wpLoss < 0.05 then Good
            elif wpLoss < 0.10 then Inaccuracy
            elif wpLoss < 0.20 then Mistake
            else Blunder

        results.Add(
            { Ply = ply
              MoveNumber = move.MoveNumber
              Color = move.Color
              San = move.San
              UciMove = uciMove
              Classification = classification
              EvalBefore = evalBefore
              BestMove = ""
              BestMoveSan = ""
              BestEval = evalBefore
              SecondBestEval = None
              WinProbBefore = wpBefore
              WinProbAfter = wpAfter
              WinProbLoss = wpLoss
              CentipawnLoss = cpLoss
              MoveAccuracy = moveAccuracy
              Depth = 0
              Nodes = 0L
              PV = "" })

        try board2.PlaySanMove move.San with _ -> ()

    let movesArray = results.ToArray()

    Some
        { Moves = movesArray
          WhiteStats = computePlayerStats (game.GameMetaData.White) movesArray "w"
          BlackStats = computePlayerStats (game.GameMetaData.Black) movesArray "b"
          WhitePlayer = game.GameMetaData.White
          BlackPlayer = game.GameMetaData.Black
          AnalysisEngine = "PGN Annotations"
          AnalysisDate = DateTime.UtcNow }
