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

type ClassificationThresholds =
    { BrilliantPVGap: float; GreatPVGap: float; BestMinPVGap: float
      ExcellentMaxWPLoss: float; GoodMaxWPLoss: float
      InaccuracyMaxWPLoss: float; MistakeMaxWPLoss: float }
    static member Default =
        { BrilliantPVGap = 0.15; GreatPVGap = 0.10; BestMinPVGap = 0.05
          ExcellentMaxWPLoss = 0.02; GoodMaxWPLoss = 0.03
          InaccuracyMaxWPLoss = 0.05; MistakeMaxWPLoss = 0.10 }

type GameReviewConfig =
    { SearchMode: SearchMode
      TimePerMove: int   // ms
      Nodes: int
      Depth: int
      MultiPV: int       // default 5
      SkipBookMoves: bool
      Thresholds: ClassificationThresholds
      AccuracyDecay: float        // exponential decay for accuracy curve (Lichess = 0.04354, steeper = harsher)
      MicroLossBase: float        // base WP penalty for PV1 matches in easy positions (default 0.02)
      MicroLossScale: float }     // how fast micro-loss shrinks with increasing PV gap (default 0.20)
    static member Default =
        { SearchMode = Time; TimePerMove = 1000; Nodes = 1000000; Depth = 18; MultiPV = 5; SkipBookMoves = true
          Thresholds = ClassificationThresholds.Default
          AccuracyDecay = 0.085
          MicroLossBase = 0.037
          MicroLossScale = 0.20 }

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
/// Returns 0-100. Formula: 103.1668 * exp(-decay * wpLoss%) - 3.1669
/// Lichess uses decay = 0.04354. Higher values produce harsher (lower) accuracy scores.
let calculateMoveAccuracy (decay: float) (wpLoss: float) : float =
    let wpLossPercent = wpLoss * 100.0
    let raw = 103.1668 * exp(-decay * wpLossPercent) - 3.1669
    max 0.0 (min 100.0 raw)

/// Classify a move based on WP loss, whether it matches engine's best, multi-PV gap, and sacrifice.
let classifyMove (t: ClassificationThresholds) (wpLoss: float) (isBestMove: bool) (isForced: bool) (pvGap: float option) (isSacrifice: bool) : MoveClassification =
    if isForced then Forced
    elif isBestMove then
        match pvGap with
        | Some gap when gap > t.BrilliantPVGap && isSacrifice -> Brilliant
        | Some gap when gap > t.GreatPVGap -> Great
        | Some gap when gap >= t.BestMinPVGap -> Best
        | _ -> Excellent
    elif wpLoss < t.ExcellentMaxWPLoss then Excellent
    elif wpLoss < t.GoodMaxWPLoss then Good
    elif wpLoss < t.InaccuracyMaxWPLoss then Inaccuracy
    elif wpLoss < t.MistakeMaxWPLoss then Mistake
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
        let opponentBefore = if isWhite then blackBefore else whiteBefore
        let opponentAfter = if isWhite then blackAfter else whiteAfter

        // Sacrifice: our material dropped by more than what we captured
        let ourLoss = materialBefore - materialAfter
        let theirLoss = opponentBefore - opponentAfter
        ourLoss > theirLoss && ourLoss > 0
    with _ -> false

// ──────────────────────────────────────────────────────────────
// Player Stats Computation
// ──────────────────────────────────────────────────────────────

/// Harmonic mean of accuracies (values floored at 1.0 to avoid division by zero).
/// A few bad moves tank the score much more than arithmetic mean.
let private harmonicMeanAccuracy (accs: float array) : float =
    if accs.Length = 0 then 0.0
    else float accs.Length / (accs |> Array.sumBy (fun a -> 1.0 / max 1.0 a))

/// Lichess-style game accuracy: (volatility-weighted mean + harmonic mean) / 2.
/// allWinProbs = white-perspective WP for every move in the game (both colors, game order).
/// moves = the classifiable moves for one player.
let private gameAccuracy (allWinProbs: float array) (moves: MoveAnalysisResult array) : float =
    if moves.Length = 0 then 0.0
    else
    let accs = moves |> Array.map (fun m -> m.MoveAccuracy)
    let hm = harmonicMeanAccuracy accs

    // Volatility-weighted mean using sliding windows of win%
    if allWinProbs.Length < 2 then (Array.average accs + hm) / 2.0
    else
    let windowSize = allWinProbs.Length / 10 |> max 2 |> min 8
    // Build per-move volatility weight from sliding windows over the full game
    let volatilityWeights = Array.init allWinProbs.Length (fun i ->
        let ws = max 0 (min i (allWinProbs.Length - windowSize))
        let window = allWinProbs.[ws .. ws + windowSize - 1]
        let mean = Array.average window
        let variance = window |> Array.averageBy (fun x -> (x - mean) ** 2.0)
        sqrt variance |> max 0.5 |> min 12.0)
    // Map each player move to its volatility weight via Ply index
    let moveWeights = moves |> Array.map (fun m ->
        if m.Ply >= 0 && m.Ply < volatilityWeights.Length then volatilityWeights.[m.Ply]
        else 0.5)
    let totalW = Array.sum moveWeights
    let wm =
        if totalW < 0.001 then Array.average accs
        else (Array.map2 (*) accs moveWeights |> Array.sum) / totalW

    (wm + hm) / 2.0

/// Phase accuracy uses harmonic mean only (phases are too short for meaningful volatility windows).
let private phaseAccuracy (moves: MoveAnalysisResult array) (minMove: int) (maxMove: int) : float =
    let phaseMoves = moves |> Array.filter (fun m -> m.MoveNumber >= minMove && m.MoveNumber <= maxMove)
    if phaseMoves.Length = 0 then 0.0
    else harmonicMeanAccuracy (phaseMoves |> Array.map (fun m -> m.MoveAccuracy))

/// Compute player stats using Lichess-style aggregation (harmonic mean + volatility weighting).
/// allWinProbs = white-perspective WP for every move in the game (both colors).
let computePlayerStats (allWinProbs: float array) (playerName: string) (moves: MoveAnalysisResult array) (color: string) : PlayerAccuracyStats =
    let playerMoves = moves |> Array.filter (fun m -> m.Color = color)
    let classifiable = playerMoves |> Array.filter (fun m -> m.Classification <> Book && m.Classification <> Forced)

    let accuracy = gameAccuracy allWinProbs classifiable

    let acpl =
        if classifiable.Length = 0 then 0.0
        else classifiable |> Array.averageBy (fun m -> m.CentipawnLoss)

    let classificationCounts =
        playerMoves
        |> Array.groupBy (fun m -> m.Classification)
        |> Array.map (fun (cls, ms) -> cls, ms.Length)
        |> Map.ofArray

    { Player = playerName
      Accuracy = Math.Round(accuracy, 1)
      ACPL = Math.Round(acpl, 1)
      MoveCount = playerMoves.Length
      Classifications = classificationCounts
      OpeningAccuracy = Math.Round(phaseAccuracy classifiable 1 15, 1)
      MiddlegameAccuracy = Math.Round(phaseAccuracy classifiable 16 40, 1)
      EndgameAccuracy = Math.Round(phaseAccuracy classifiable 41 Int32.MaxValue, 1) }

// ──────────────────────────────────────────────────────────────
// Engine Position Analysis (MultiPV)
// ──────────────────────────────────────────────────────────────

/// Mutable callback dispatcher for per-search result accumulation.
type EngineUpdateDispatcher() =
    member val Handler : Action<EngineUpdate> = Action<EngineUpdate>(fun _ -> ()) with get, set

/// Analyze a single position with MultiPV using ChessEngineWithUCIProcessing.
/// The dispatcher's Handler is swapped per-search to accumulate results.
let analyzePosition
    (engine: ChessEngineWithUCIProcessing)
    (dispatcher: EngineUpdateDispatcher)
    (positionCmd: string)
    (_isWhite: bool)
    (config: GameReviewConfig)
    : MultiPVResult array * string =

    // Match EnginePanel pattern: Stop → Position → isready → Go
    // UciNewGame only for Nodes mode (time mode skips it, matching Play() in AnalysisManager)
    engine.SendUCICommand(UCICommand.Stop)
    if config.SearchMode = Nodes then
        engine.SendUCICommand(UCICommand.UciNewGame)
    engine.SendUCICommand(UCICommand.PositionWithMoves positionCmd)
    let ok = engine.WaitForReadyOk()
    if not ok then
        printfn "Engine did not respond to isready"
        [||], ""
    else

    let pvResults = Collections.Generic.Dictionary<int, MultiPVResult>()
    let mutable bestMoveStr = ""
    let done' = new ManualResetEventSlim(false)

    dispatcher.Handler <- Action<EngineUpdate>(fun update ->
        match update with
        | EngineUpdate.Status status ->
            let pvIndex = if status.MultiPV = 0 then 1 else status.MultiPV
            let uciPV = status.PVLongSAN
            let bestMove =
                if String.IsNullOrEmpty uciPV then ""
                else uciPV.Split(' ').[0]
            pvResults.[pvIndex] <-
                { Eval = status.Eval
                  BestMove = bestMove
                  PV = uciPV
                  Depth = status.Depth
                  Nodes = status.Nodes }
        | EngineUpdate.BestMove bm ->
            bestMoveStr <- bm.Move
            done'.Set()
        | _ -> ())

    match config.SearchMode with
    | Time -> engine.SendUCICommand(UCICommand.GoMoveTime config.TimePerMove)
    | Nodes -> engine.SendUCICommand(UCICommand.GoNodes config.Nodes)
    | Depth -> engine.SendUCICommand(UCICommand.RawCommand(sprintf "go depth %d" config.Depth))

    let completed = done'.Wait(30000) // 30s timeout safety net
    if not completed then printfn "Search timed out for position: %s" positionCmd
    done'.Dispose()

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
    (engine: ChessEngineWithUCIProcessing)
    (dispatcher: EngineUpdateDispatcher)
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
        engine.SendUCICommand(UCICommand.RawCommand(sprintf "setoption name MultiPV value %d" config.MultiPV))

    engine.WaitForReadyOk() |> ignore

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

            if isBookMove then
                whiteEvals.Add(EvalType.NA)
                pvDataPerMove.Add([||])
                bestMoves.Add("")
            else
                // Analyze position before this move (including forced moves)
                let posCmd = board.PositionWithMoves()
                let pvResults, bestMove = analyzePosition engine dispatcher posCmd isWhite config

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
        let pvResults, _ = analyzePosition engine dispatcher posCmd isWhite config
        let finalEval =
            if pvResults.Length > 0 then pvResults.[0].Eval
            else EvalType.NA
        whiteEvals.Add(finalEval)

    // Reset MultiPV to 1
    if config.MultiPV > 1 then
        engine.SendUCICommand(UCICommand.RawCommand "setoption name MultiPV value 1")

    // Build MoveAnalysisResult for each move
    let results = ResizeArray<MoveAnalysisResult>()
    let replayBoard = Board()
    replayBoard.LoadFen startFen
    replayBoard.StartPosition <- startFen

    let analyzedMoves = min totalMoves (min whiteEvals.Count pvDataPerMove.Count)
    for i in 0 .. analyzedMoves - 1 do
        let move = game.Mainline.[i]
        let isWhite = replayBoard.Position.STM = 0uy
        let ply = i

        let legalMoves = replayBoard.GetLegalMoves() |> Seq.length

        let isBookMove =
            config.SkipBookMoves &&
            not (String.IsNullOrEmpty move.Comment) &&
            move.Comment.ToLower().Contains("book")

        let uciMove = if i < uciMoves.Count then uciMoves.[i] else ""

        let pvData = pvDataPerMove.[i]
        let evalBefore = whiteEvals.[i]  // PV1 eval (best) at this position
        let evalAfter =
            if i + 1 < whiteEvals.Count then whiteEvals.[i + 1]
            else EvalType.NA

        // Find which PV line the played move matches
        let matchedPVIndex =
            if pvData.Length > 0 && not (String.IsNullOrEmpty uciMove) then
                pvData |> Array.tryFindIndex (fun pv -> pv.BestMove = uciMove)
            else None

        // Multi-PV based WP loss: compare played move against best from same position
        let stmEvalBefore = if isWhite then evalBefore else flipEval evalBefore
        let wpBest = evalToWinProb stmEvalBefore ply

        // PV gap between best and second-best (needed for micro-loss and classification)
        let pvGapValue =
            if pvData.Length >= 2 then
                let pv1Eval = if isWhite then pvData.[0].Eval else flipEval pvData.[0].Eval
                let pv2Eval = if isWhite then pvData.[1].Eval else flipEval pvData.[1].Eval
                let wp1 = evalToWinProb pv1Eval ply
                let wp2 = evalToWinProb pv2Eval ply
                Some (wp1 - wp2)
            else None

        let wpLoss, wpBefore, wpAfter =
            match matchedPVIndex with
            | Some 0 ->
                // Played the best move — apply micro-loss based on position difficulty
                let stmAfter = if isWhite then evalAfter else flipEval evalAfter
                let wpA = evalToWinProb stmAfter (ply + 1)
                let microLoss =
                    match pvGapValue with
                    | Some gap -> max 0.0 (config.MicroLossBase - gap * config.MicroLossScale)
                    | None -> 0.0  // no PV gap data, give full credit
                microLoss, wpBest, wpA
            | Some idx ->
                // Played a lower PV line — compare within same position's analysis
                let matchedEval = if isWhite then pvData.[idx].Eval else flipEval pvData.[idx].Eval
                let wpPlayed = evalToWinProb matchedEval ply
                let loss = max 0.0 (wpBest - wpPlayed)
                loss, wpBest, wpPlayed
            | None ->
                // Move not in any PV — fallback to sequential comparison
                let stmAfter = if isWhite then evalAfter else flipEval evalAfter
                let wpA = evalToWinProb stmAfter (ply + 1)
                let loss = max 0.0 (wpBest - wpA)
                loss, wpBest, wpA

        // Centipawn loss (matching the multi-PV approach)
        let cpLoss =
            match matchedPVIndex with
            | Some 0 -> 0.0
            | Some idx ->
                let bestCp = if isWhite then evalBefore else flipEval evalBefore
                let playedCp = if isWhite then pvData.[idx].Eval else flipEval pvData.[idx].Eval
                match bestCp, playedCp with
                | CP b, CP p -> max 0.0 ((b - p) * 100.0)
                | _ -> wpLoss * 500.0
            | None ->
                let stmAfter = if isWhite then evalAfter else flipEval evalAfter
                match stmEvalBefore, stmAfter with
                | CP before, CP after -> max 0.0 ((before - after) * 100.0)
                | _ -> wpLoss * 500.0

        let moveAccuracy = calculateMoveAccuracy config.AccuracyDecay wpLoss

        // Best move info
        let bestMoveUci = bestMoves.[i]
        let isBestMove = matchedPVIndex = Some 0
        let bestMoveSan =
            if String.IsNullOrEmpty bestMoveUci then ""
            else
                match replayBoard.GetSanFromUci bestMoveUci with
                | Some san -> san
                | None -> bestMoveUci

        // Multi-PV gap for Brilliant/Great detection (already computed above)
        let pvGap = pvGapValue

        // Sacrifice detection
        let sacrifice =
            if isBestMove && not (String.IsNullOrEmpty uciMove) then
                isSacrifice replayBoard uciMove
            else false

        // Classification
        let classification =
            if isBookMove then Book
            elif legalMoves <= 1 then Forced
            else classifyMove config.Thresholds wpLoss isBestMove false pvGap sacrifice

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

    // White-perspective WP for volatility windows
    let allWinProbs = movesArray |> Array.map (fun m ->
        if m.Color = "w" then m.WinProbBefore else 1.0 - m.WinProbBefore)

    { Moves = movesArray
      WhiteStats = computePlayerStats allWinProbs (game.GameMetaData.White) movesArray "w"
      BlackStats = computePlayerStats allWinProbs (game.GameMetaData.Black) movesArray "b"
      WhitePlayer = game.GameMetaData.White
      BlackPlayer = game.GameMetaData.Black
      AnalysisEngine = engine.Name
      AnalysisDate = DateTime.UtcNow }

// ──────────────────────────────────────────────────────────────
// Quick Analysis from PGN Annotations
// ──────────────────────────────────────────────────────────────

let parseClassification (s: string) =
    match s.Trim() with
    | "Book" -> Some Book | "Forced" -> Some Forced
    | "Brilliant" | "!!" -> Some Brilliant | "Great" | "!" -> Some Great
    | "Best" -> Some Best | "Excellent" -> Some Excellent | "Good" -> Some Good
    | "Inaccuracy" | "?!" -> Some Inaccuracy | "Mistake" | "?" -> Some Mistake
    | "Blunder" | "??" -> Some Blunder
    | _ -> None

/// Extract a named field value from a comment string like "wv=0.45, cls=Best, bm=e2e4"
let private extractField (comment: string) (fieldName: string) : string option =
    let prefix = fieldName + "="
    let idx = comment.IndexOf(prefix, StringComparison.Ordinal)
    if idx < 0 then None
    else
        let start = idx + prefix.Length
        // pv= consumes everything to the end (it contains spaces)
        if fieldName = "pv" then
            Some (comment.Substring(start).Trim())
        else
            let endIdx = comment.IndexOf(',', start)
            let value = if endIdx < 0 then comment.Substring(start) else comment.Substring(start, endIdx - start)
            Some (value.Trim())

let private tryParseFloat (s: string) = match Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with true, v -> Some v | _ -> None
let private tryParseInt (s: string) = match Int32.TryParse(s) with true, v -> Some v | _ -> None
let private tryParseInt64 (s: string) = match Int64.TryParse(s) with true, v -> Some v | _ -> None

let private parseEvalFromString (s: string) =
    let s = s.Trim()
    if s = "None" then EvalType.NA
    elif s.StartsWith("-M") then match tryParseInt (s.Substring(2)) with Some m -> Mate (-m) | None -> NA
    elif s.StartsWith("M") then match tryParseInt (s.Substring(1)) with Some m -> Mate m | None -> NA
    else match tryParseFloat s with Some v -> CP v | None -> NA

/// Check if a comment contains rich review annotations (cls= field)
let private hasRichAnnotation (comment: string) =
    not (String.IsNullOrEmpty comment) && comment.Contains("cls=")

/// Reload a game review from rich PGN annotations (exported by Game Review).
/// Reads stored classification, accuracy, wpLoss, etc. directly without recomputation.
let private analyzeFromRichAnnotations (game: PgnGame) : GameAnalysisResult option =
    let totalMoves = game.Mainline.Count
    if totalMoves = 0 then None
    else

    let board = Board()
    let startFen = if String.IsNullOrWhiteSpace game.Fen then startPosition else game.Fen
    board.LoadFen startFen
    board.StartPosition <- startFen

    let results = ResizeArray<MoveAnalysisResult>()
    let mutable hasData = false

    for i in 0 .. totalMoves - 1 do
        let move = game.Mainline.[i]
        let isWhite = board.Position.STM = 0uy
        let ply = i
        let comment = if move.Comment = null then "" else move.Comment

        let uciMove = board.GetUciFromSan(move.San) |> Option.defaultValue ""

        let cls = extractField comment "cls" |> Option.bind parseClassification |> Option.defaultValue Best
        let acc = extractField comment "acc" |> Option.bind (fun s -> tryParseFloat (s.TrimEnd('%'))) |> Option.defaultValue 100.0
        let wpl = extractField comment "wpl" |> Option.bind tryParseFloat |> Option.defaultValue 0.0
        let cpl = extractField comment "cpl" |> Option.bind tryParseFloat |> Option.defaultValue 0.0
        let evalBefore = extractField comment "wv" |> Option.map parseEvalFromString |> Option.defaultValue NA
        let bestMove = extractField comment "bm" |> Option.defaultValue ""
        let bestMoveSan = extractField comment "bms" |> Option.defaultValue ""
        let depth = extractField comment "d" |> Option.bind tryParseInt |> Option.defaultValue 0
        let nodes = extractField comment "n" |> Option.bind tryParseInt64 |> Option.defaultValue 0L
        let pv = extractField comment "pv" |> Option.defaultValue ""

        if hasRichAnnotation comment then hasData <- true

        // Recompute WP before/after from eval (cheap to derive)
        let stmEval = if isWhite then evalBefore else flipEval evalBefore
        let wpBefore = evalToWinProb stmEval ply
        let wpAfter = wpBefore - wpl  // Approximate from stored loss

        results.Add(
            { Ply = ply; MoveNumber = move.MoveNumber; Color = if isWhite then "w" else "b"
              San = move.San; UciMove = uciMove
              Classification = cls; EvalBefore = evalBefore
              BestMove = bestMove; BestMoveSan = bestMoveSan; BestEval = evalBefore
              SecondBestEval = None
              WinProbBefore = wpBefore; WinProbAfter = wpAfter; WinProbLoss = wpl
              CentipawnLoss = cpl; MoveAccuracy = acc
              Depth = depth; Nodes = nodes; PV = pv })

        try board.PlaySanMove move.San with _ -> ()

    if not hasData then None
    else
        let movesArray = results.ToArray()
        let allWinProbs = movesArray |> Array.map (fun m ->
            if m.Color = "w" then m.WinProbBefore else 1.0 - m.WinProbBefore)
        let annotator =
            match game.GameMetaData.OtherTags |> Seq.tryFind (fun kv -> kv.Key = "Annotator") with
            | Some kv -> kv.Value
            | None -> "PGN Annotations"
        Some
            { Moves = movesArray
              WhiteStats = computePlayerStats allWinProbs (game.GameMetaData.White) movesArray "w"
              BlackStats = computePlayerStats allWinProbs (game.GameMetaData.Black) movesArray "b"
              WhitePlayer = game.GameMetaData.White
              BlackPlayer = game.GameMetaData.Black
              AnalysisEngine = annotator
              AnalysisDate = DateTime.UtcNow }

/// Legacy: analyze from basic wv= annotations using sequential eval comparison.
let private analyzeFromBasicAnnotations (thresholds: ClassificationThresholds) (decay: float) (game: PgnGame) : GameAnalysisResult option =
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
        let eval =
            if stat.n = 0L && stat.d = 0 && stat.wv = 0.0 then EvalType.NA
            else CP stat.wv
        whiteEvals.Add(eval)
        try replayBoard.PlaySanMove move.San with _ -> ()

    let hasEvals = whiteEvals |> Seq.exists (fun e -> e <> EvalType.NA)
    if not hasEvals then None
    else

    // Cross-engine games (e.g. TCEC): compare with same engine (i+2) to avoid eval scale noise
    let isCrossEngine = game.GameMetaData.White <> game.GameMetaData.Black

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
        let evalAfterIdx = if isCrossEngine then i + 2 else i + 1
        let evalAfter =
            if evalAfterIdx < whiteEvals.Count then whiteEvals.[evalAfterIdx]
            else EvalType.NA

        let hasEvalGap = evalBefore = EvalType.NA || evalAfter = EvalType.NA
        let stmEvalBefore = if isWhite then evalBefore else flipEval evalBefore
        let stmEvalAfter = if isWhite then evalAfter else flipEval evalAfter

        let wpBefore = evalToWinProb stmEvalBefore ply
        let wpAfter = evalToWinProb stmEvalAfter (ply + 1)
        let wpLoss = if hasEvalGap then 0.0 else max 0.0 (wpBefore - wpAfter)

        let cpLoss =
            if hasEvalGap then 0.0
            else
                match stmEvalBefore, stmEvalAfter with
                | CP before, CP after -> max 0.0 ((before - after) * 100.0)
                | _ -> wpLoss * 500.0

        let moveAccuracy = calculateMoveAccuracy decay wpLoss

        let classification =
            if isBookMove then Book
            elif legalMoves <= 1 then Forced
            elif hasEvalGap then Best
            elif wpLoss < thresholds.ExcellentMaxWPLoss then Best
            elif wpLoss < thresholds.GoodMaxWPLoss then Good
            elif wpLoss < thresholds.InaccuracyMaxWPLoss then Inaccuracy
            elif wpLoss < thresholds.MistakeMaxWPLoss then Mistake
            else Blunder

        results.Add(
            { Ply = ply; MoveNumber = move.MoveNumber; Color = if isWhite then "w" else "b"
              San = move.San; UciMove = uciMove
              Classification = classification; EvalBefore = evalBefore
              BestMove = ""; BestMoveSan = ""; BestEval = evalBefore
              SecondBestEval = None
              WinProbBefore = wpBefore; WinProbAfter = wpAfter; WinProbLoss = wpLoss
              CentipawnLoss = cpLoss; MoveAccuracy = moveAccuracy
              Depth = 0; Nodes = 0L; PV = "" })

        try board2.PlaySanMove move.San with _ -> ()

    let movesArray = results.ToArray()
    let allWinProbs = movesArray |> Array.map (fun m ->
        if m.Color = "w" then m.WinProbBefore else 1.0 - m.WinProbBefore)
    Some
        { Moves = movesArray
          WhiteStats = computePlayerStats allWinProbs (game.GameMetaData.White) movesArray "w"
          BlackStats = computePlayerStats allWinProbs (game.GameMetaData.Black) movesArray "b"
          WhitePlayer = game.GameMetaData.White
          BlackPlayer = game.GameMetaData.Black
          AnalysisEngine = "PGN Annotations"
          AnalysisDate = DateTime.UtcNow }

/// Analyze a game from PGN annotations. Tries rich format (cls= fields) first,
/// falls back to legacy sequential eval comparison for basic wv= annotations.
let analyzeGameFromAnnotations (thresholds: ClassificationThresholds) (decay: float) (game: PgnGame) : GameAnalysisResult option =
    let totalMoves = game.Mainline.Count
    if totalMoves = 0 then None
    else
    // Check if any move has rich annotations
    let hasRich = game.Mainline |> Seq.exists (fun m -> hasRichAnnotation m.Comment)
    if hasRich then analyzeFromRichAnnotations game
    else analyzeFromBasicAnnotations thresholds decay game
