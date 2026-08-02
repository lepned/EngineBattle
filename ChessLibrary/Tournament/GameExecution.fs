module ChessLibrary.GameExecution

open System
open System.Text
open System.Threading
open System.Threading.Channels
open System.Diagnostics
open Microsoft.Extensions.Logging
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PositionTypes
open ChessLibrary.MoveTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.Chess
open ChessLibrary.BoardUtils
open ChessLibrary.ChessUtilities
open ChessLibrary.EngineProtocol
open ChessLibrary.RuntimeUtilities
open ChessLibrary.CustomException
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameAdjudication
open ChessLibrary.GameReplay
open Microsoft.FSharp.Core.Operators.Unchecked

let private adjudicationReason = ResultReason.AdjudicatedByUser

/// Start a game with pondering enabled
let playWithPondering
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing: Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =

  let player1Channel = Channel.CreateBounded<string>(BoundedChannelOptions(capacity = 4096, SingleWriter = true, SingleReader = true, FullMode = BoundedChannelFullMode.Wait))
  let player2Channel = Channel.CreateBounded<string>(BoundedChannelOptions(capacity = 4096, SingleWriter = true, SingleReader = true, FullMode = BoundedChannelFullMode.Wait))

  let mutable player1WriterCts : CancellationTokenSource option = None
  let mutable player2WriterCts : CancellationTokenSource option = None

  // State
  let mutable ponderer : (ChessEngine * string) option = None
  let mutable lastMovePlayed = ""
  let mutable continueGame = true
  let mutable result = Result.Empty
  let mutable player1inPonderMode = false
  let mutable player2inPonderMode = false
  let mutable ponderHit = false
  let mutable currentPos = Position.Default

  // Helper functions to always derive current player from board state
  let whoseTurn stm = if stm = 0uy then player1 else player2
  let opponent stm = if stm = 0uy then player2 else player1
  let isWhiteTurn stm = stm = 0uy

  // ------- helpers & local state (unchanged setup) -------
  let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
  sb.Clear() |> ignore
  let append (txt:string) = sb.Append txt |> ignore

  if tourny.TestOptions.WriteToConsole then
    player1.ShowCommands()
    player2.ShowCommands()

  let mutable lastEngineStatus = EngineStatus.Empty
  let mutable moves = 0
  let mutable numberOfNodes = 0L
  let mutable evalList : EvalType list = []
  let mutable fullEvalList :EvalType list = []

  // Track eval per engine and per move to avoid accidentally reusing the opponent's last ply eval.
  let lastEvalByEngine = System.Collections.Generic.Dictionary<string, EvalType>()
  let lastEvalThisMove = System.Collections.Generic.Dictionary<string, EvalType>()
  do
    lastEvalByEngine[player1.Name] <- EvalType.NA
    lastEvalByEngine[player2.Name] <- EvalType.NA
    lastEvalThisMove[player1.Name] <- EvalType.NA
    lastEvalThisMove[player2.Name] <- EvalType.NA

  let resetThisMoveEval (engineName: string) =
    lastEvalThisMove[engineName] <- EvalType.NA

  let setEval (engineName: string) (eval: EvalType) =
    lastEvalByEngine[engineName] <- eval
    lastEvalThisMove[engineName] <- eval

  let npsList = ResizeArray<float>()
  let gameMoveList = board.SanMovesPlayed

  let findTimeSetting (player : ChessEngine) =
    tourny.FindTimeControl (player.Config.TimeControlID)
  let isNodeLimit player = (findTimeSetting player).NodeLimit
  let wPlayer = (findTimeSetting player1)
  let bPlayer = (findTimeSetting player2)
  let mutable wTime = wPlayer.Fixed
  let mutable bTime = bPlayer.Fixed
  // Fixed for the game, so looked up once rather than on every clock check — the silent
  // engine poll runs four times a second and the per-line check far more often than that.
  let wIncr = tourny.TimeControl.GetIncrementTime(player1.Config.TimeControlID)
  let bIncr = tourny.TimeControl.GetIncrementTime(player2.Config.TimeControlID)
  let moveOverheadInTicks = tourny.MoveOverhead.Ticks

  let msg = $"Initializing players: {player1.Name} vs {player2.Name} with pondering enabled"
  logger.LogInformation msg
  tourny.CurrentGameNr <- tourny.CurrentGameNr + 1

  let gameStartInfo : StartGameInfo =
    {
      WhitePlayer = player1.Config
      BlackPlayer = player2.Config
      StartPos = board.FEN()
      OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
      WhiteTime = wTime
      BlackTime = bTime
      WhiteToMove = board.Position.STM = 0uy
      OpeningName = tourny.OpeningName
      CurrentGameNr = pairing.GameNr
      OpeningHash = pairing.OpeningHash
    }
  board.MovesAndFenPlayed.Clear()
  let mutable moveInfoData = ChessMoveInfo.Empty

  callback(StartOfGame gameStartInfo)
  // Always send UCI_Chess960 so EB is authoritative about the variant,
  // overriding any misconfigured engine.json setting.
  let chess960Option : EngineOption = {Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
  player1.AddSetOption chess960Option
  player2.AddSetOption chess960Option
  if tourny.MoveOverhead.Ticks > 0 then
      let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
      player1.SetMoveOverhead("overhead", ms)
      player2.SetMoveOverhead("overhead", ms)
  if not tourny.ConsoleOnly then
    let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
    let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
    let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
    GameInitialization.initEngines openingDelayMs tourny player1 player2 logger
  else
    GameInitialization.initEngines 0 tourny player1 player2 logger

  try
    player1WriterCts <- Some
      (startEngineChannelWriter player1 player1Channel cts logger
      (fun () -> player1inPonderMode)
      callback
      (fun () -> board.Position.STM = 0uy))
    player2WriterCts <- Some
      (startEngineChannelWriter player2 player2Channel cts logger
      (fun () -> player2inPonderMode)
      callback
      (fun () -> board.Position.STM = 0uy))
  with ex ->
    logger.LogWarning(ex, "Failed to start per-game engine channel writers")

  // Helper to get the channel reader for the current playing engine
  let getEngineReaderFor (eng: ChessEngine) =
    if eng.Name = player1.Name then player1Channel.Reader else player2Channel.Reader

  logEngineInitCommands logger player1 player2
  GameInitialization.appendGameDescription sb tourny player1 player2 (board.OpeningMovesPlayed) (board.FEN())
  callback (GameStarted player1.Name)

  let mutable lastCheck = 0L
  let gametimer = Stopwatch.GetTimestamp()
  let mutable moveTimer = Stopwatch.StartNew()
  let mutable depth = 0
  let mutable selfdepth = 0
  let mutable Player1PV = String.Empty
  let mutable Player2PV = String.Empty
  let mutable PVLine1 = String.Empty
  let mutable PVLine2 = String.Empty
  let fen0 = board.FEN()
  logger.LogDebug $"After opening moves, FEN={fen0}"

  // extracted helper to encapsulate all "PROCESS BESTMOVE" logic
  let processBestMove (currentPlaying: ChessEngine) (currentOpponent: ChessEngine) isWhite (line:string)  = async {
    let duration = moveTimer.Elapsed
    let useNodes = isNodeLimit currentPlaying
    let playerTime = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
    let remainingTicks = playerTime - duration.Ticks
    let lostOnTime = (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L)
    let mutable posToCheck = board.Position
    let piecesLeft = PositionOps.numberOfPieces &posToCheck

    if lostOnTime then
      let firstTwo = firstTwoEvals fullEvalList
      if currentPlaying.HasExited() then
         logger.LogCritical($"Engine {currentPlaying.Name} has exited while lost on time")
      logger.LogCritical("Engine {Engine} lost on time. Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}", currentPlaying.Name, (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds), duration.TotalMilliseconds)
      let res = lostOnTimeResult currentPlaying.Name currentOpponent.Name isWhite gameMoveList gametimer firstTwo
      result <- res
      continueGame <- false
    else
      if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
        let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
        do! Async.Sleep delay

      // Update clocks
      let (currentTime, timeLeft) =
        if isWhite then GameHelpers.updateClocks duration wTime wIncr useNodes
        else GameHelpers.updateClocks duration bTime bIncr useNodes
      if isWhite then wTime <- currentTime else bTime <- currentTime
      // Repeating moves-to-go: top up the mover's base time when it completes a period.
      // board is pre-move here (MakeMove happens below), so NextMoveNumber() is the move just made.
      let mtgPeriod = tourny.TimeControl.MovesToGoPeriod
      if mtgPeriod > 0 && not useNodes && board.NextMoveNumber() % mtgPeriod = 0 then
          let cfg = tourny.TimeControl.GetTimeConfig(currentPlaying.Config.TimeControlID)
          if isWhite then wTime <- TimeOnly(wTime.Ticks + cfg.Fixed.Ticks)
          else bTime <- TimeOnly(bTime.Ticks + cfg.Fixed.Ticks)
      moveInfoData.tl <- int64 ((if isWhite then wTime else bTime).ToTimeSpan().TotalMilliseconds)
      moveInfoData.mt <- int64 duration.TotalMilliseconds
      moveInfoData.pcs <- byte piecesLeft

      let parts = line.Split()
      let move = parts.[1]
      let ponderMove = if line.Contains "ponder" then parts.[3] else ""
      lastMovePlayed <- move
      let mutable shortSan = String.Empty

      match tryGetTMoveFromUciNotation &board move with
      | Some tmove ->
          let mutable moveAdj = tmove
          shortSan <- getSanNotationFromTMove &board tmove
          board.UciMovesPlayed.Add(move)
          gameMoveList.Add(shortSan)
          board.MakeMove(&moveAdj)
          let ponderSan = getShortSanFromLongSan &board ponderMove
          moveInfoData.pd <- ponderSan
          moves <- moves + 1

          let eval =
            if evalList.Length > 0 then
              evalList.[0]
            else
              let thisMove = lastEvalThisMove[currentPlaying.Name]
              if thisMove <> EvalType.NA then
                thisMove
              else
                match lastEvalByEngine.TryGetValue currentPlaying.Name with
                | true, e when e <> EvalType.NA -> e
                | _ -> EvalType.CP 0.0

          let pv, pvLong = if currentPlaying.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
          let nps =
            let last = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
            if last <> 0.0 then last
            else float numberOfNodes / duration.TotalSeconds

          let posNow = board.Position
          let fenNow = BoardHelper.posToFen posNow
          let moveDetail = {
            LongSan = move
            FromSq = move[0..1]
            ToSq = move[2..3]
            Color = if board.Position.STM = 8uy then "w" else "b"
            IsCastling = TMoveOps.isCastlingMove tmove
            Comments = String.Empty
          }
          let moveAndFen = { Move = moveDetail; ShortSan = shortSan; FenAfterMove = fenNow }
          fullEvalList <- eval :: fullEvalList

          let movesLeft =
            GameAdjudication.movesLeftBeforeDrawAdjudication
              eval fullEvalList
              tourny.Adjudication.DrawOption.MinDrawMove
              (tourny.Adjudication.DrawOption.DrawMoveLength * 2)
              tourny.Adjudication.DrawOption.MaxDrawScore

          let bestMove : BestMoveInfo = {
            Player = currentPlaying.Name
            Move = move
            Ponder = ponderSan
            Eval = eval
            TimeLeft = timeLeft
            MoveTime = TimeOnly(duration.Ticks)
            NPS = nps
            Nodes = numberOfNodes
            FEN = fenNow
            PV = pv
            LongPV = pvLong
            MoveAndFen = moveAndFen
            MoveHistory = board.GetSanMoveHistory()
            Move50 = board.Position.Count50 |> int
            R3 = board.RepetitionNr()
            PiecesLeft = piecesLeft
            AdjDrawML = movesLeft
          }

          evalList <- []
          depth <- 0
          selfdepth <- 0
          npsList.Clear()
          resetThisMoveEval currentPlaying.Name

          match GameAdjudication.adjudicateByEval logger board fullEvalList tourny player1.Name player2.Name currentPlaying.Name gametimer gameMoveList moves with
          | Some res ->
              if res.Reason = ResultReason.Checkmate then
                let bm = { bestMove with MoveHistory = bestMove.MoveHistory + "#" }
                let status = { lastEngineStatus with Eval = EvalType.Mate 0 }
                let numberAndMove = (board.SanMoveNumberString shortSan) + "#"
                annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
                callback(BestMove (bm, status))
              else
                let numberAndMove = board.SanMoveNumberString shortSan
                annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
                callback(BestMove (bestMove, lastEngineStatus))
                moveInfoData <- ChessMoveInfo.Empty
              result <- res
              continueGame <- false
          | None ->
              let numberAndMove = board.SanMoveNumberString shortSan
              annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
              moveInfoData <- ChessMoveInfo.Empty
              callback(BestMove (bestMove, lastEngineStatus))
              //check ponder move status
              match ponderer with
              | Some (pEngine, pMove) when pEngine.Name = currentOpponent.Name && lastMovePlayed = pMove ->
                  ponderHit <- true
                  //logger.LogInformation($"PONDER HIT by {currentOpponent.Name} on move {pMove}")
              | Some (pEngine, pMove) when pEngine.Name = currentOpponent.Name && lastMovePlayed <> pMove ->
                  //we pondered, but our opponent played a different move
                  ponderHit <- false
                  //logger.LogInformation($"PONDER MISS by {currentOpponent.Name}, expected {pMove}, got {lastMovePlayed}")
                  pEngine.Stop() //stop pondering
                  do! Async.Sleep 200

              | _ -> ponderHit <- false

              if not (String.IsNullOrEmpty ponderSan) then
                if currentPlaying.Name = player1.Name then
                  player1inPonderMode <- true
                  player2inPonderMode <- false
                else
                  player1inPonderMode <- false
                  player2inPonderMode <- true
                ponderer <- Some (currentPlaying, ponderMove)

                let timeConfig = findTimeSetting currentPlaying
                let timeType = tourny.TimeControl.GetTime(timeConfig)
                let timeCommand = (TimeControlTypes.TimeControlCommands.uciTimePart timeType wTime bTime)
                let ponderCommand = sprintf "%s ponder" timeCommand
                //logger.LogDebug($"PONDER COMMAND: Sending ponder to {currentPlaying.Name}: {ponderCommand}")
                let ponderPosition = board.PositionWithMoves() + " " + ponderMove
                currentPlaying.Position ponderPosition
                currentPlaying.GoPonder ponderCommand

              else
                ponderer <- None
                player1inPonderMode <- false
                player2inPonderMode <- false
      | _ ->
          // Illegal move
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let firstTwo = firstTwoEvals fullEvalList
          let res =
              if currentPlaying.Name = player1.Name then
                  createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur firstTwo
              else
                  createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur firstTwo
          //log error, send end of game
          logger.LogCritical($"Illegal move from {currentPlaying.Name}: {move} FEN={board.FEN()}")
          result <- res
          continueGame <- false
  }

  // Info line processor
  let processInfoLine (channel:ChannelReader<string>) (engine: ChessEngine) (line: string) = async {
    let elapsed = int64 moveTimer.Elapsed.TotalMilliseconds
    let diff = elapsed - lastCheck
    let interval = 500
    let isWhite = engine.Name = player1.Name

    let handleInfoLine (infoLine: string) =
      match Regex.getEssentialDataWithEPS infoLine isWhite with
      | Some (d, eval, nodes, nps, eps, pvLine, tbhits, wdl, sd, mPv) ->
          numberOfNodes <- nodes
          if d > depth then depth <- d
          if sd > selfdepth then selfdepth <- sd
          npsList.Add(float nps)
          evalList <- eval :: evalList
          setEval engine.Name eval
          moveInfoData.d <- depth
          moveInfoData.sd <- selfdepth
          moveInfoData.wv <- eval
          moveInfoData.n <- nodes
          moveInfoData.s <- nps
          moveInfoData.tb <- tbhits
          moveInfoData.eps <- eps

          // Skip fail-high/low lines: their PV is cut to the root move, and a move that
          // ends on one would carry that stump into the PGN comment for good.
          if not (String.IsNullOrEmpty pvLine) && not (Regex.isBoundLine infoLine) then
            if player1.Name = engine.Name then
              Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
              PVLine1 <- pvLine
              moveInfoData.pv <- Player1PV
            else
              Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
              PVLine2 <- pvLine
              moveInfoData.pv <- Player2PV

          let npsLast = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
          let pv, pvLong = if engine.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2

          let status = {
            PlayerName = engine.Name
            Eval = eval; Depth = d; SD = sd; Nodes = nodes; NPS = npsLast; EPS = 0.0
            TBhits = tbhits
            WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
            PV = pv; PVLongSAN = pvLong; MultiPV = mPv
          }
          lastEngineStatus <- status

          if diff > interval then
            lastCheck <- elapsed
            callback (Status status)
      | None -> ()

    if not tourny.TestOptions.PolicyTest && line.StartsWith "info string" && line.Contains "N:" then
      let nnMsg = Regex.getInfoStringData engine.Name line
      let list = ResizeArray<NNValues>()
      list.Add nnMsg
      let mutable cont = not (line.StartsWith "info string node")
      if not cont && tourny.VerboseLogging then
        logger.LogDebug "Only one move in log live stats"

      while cont do
        let! newline = channel.ReadAsync(cts.Token).AsTask() |> Async.AwaitTask
        if tourny.VerboseLogging then
          logger.LogDebug($"In info string loop: {engine.Name} {newline}")
        if String.IsNullOrEmpty newline then
          cont <- false
        elif newline.StartsWith "bestmove" then
          //if tourny.VerboseLogging then logger.LogInformation(board.FEN() + ": new bestmove: " + newline)
          cont <- false
        elif newline.StartsWith "info string node" then
          cont <- false
        else
          let msg = Regex.getInfoStringData engine.Name newline
          list.Add msg

      makeShortSan list &board
      match Engine.calcTopNn list with
      | Some (n1,n2,q1,q2,p1,pt) ->
          moveInfoData.n1 <- n1; moveInfoData.n2 <- n2
          moveInfoData.q1 <- q1; moveInfoData.q2 <- q2
          moveInfoData.p1 <- p1; moveInfoData.pt <- pt
      | None -> logger.LogDebug "No move found in log live stats"
      if list.Count > 0 then callback (NNSeq list)

    elif line.StartsWith "info" then
      handleInfoLine line
  }

  async {
      let cancel() =
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
          logger.LogInformation($"Cancel requested")
          result <- res

      let adjudicate (adj: UserAdjudication) =
          try player1.Stop() with _ -> ()
          try player2.Stop() with _ -> ()
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let firstTwo = firstTwoEvals fullEvalList
          let res = createResultWithEval player1.Name player2.Name gameMoveList adj.Result adjudicationReason dur firstTwo
          logger.LogInformation($"Game adjudicated by user: {adj.Result}")
          result <- res
          continueGame <- false

      try
          while continueGame && not cts.IsCancellationRequested do
              match tryGetUserAdjudication() with
              | Some adj when adj.GameNr = pairing.GameNr -> adjudicate adj
              | _ -> ()

              let sideToMove = board.Position.STM
              let currentPlaying = whoseTurn sideToMove
              let currentOpponent = opponent sideToMove
              let isWhite = isWhiteTurn sideToMove

              if currentPlaying.HasExited() then
                  do! Async.Sleep 1000
                  let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                  let firstTwoEvals = firstTwoEvals fullEvalList
                  let res, msg =
                      if currentOpponent.HasExited() then
                          let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                          res, $"Shutdown detected after {currentPlaying.Name} exited; returning Cancel"
                      else
                          let resValue = if currentPlaying.Name = player1.Name then "0-1" else "1-0"
                          createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected currentPlaying.Name) dur firstTwoEvals,
                          $"Player has exited/crashed {currentPlaying.Name}"
                  logger.LogInformation msg
                  let diagnosis = currentPlaying.GetDiagnostics()
                  logger.LogCritical diagnosis
                  result <- res
                  continueGame <- false

              // A silent engine still burns clock. Loss on time is otherwise only checked in
              // processBestMove, i.e. when a move actually arrives, so a crashed or hung
              // engine is never adjudicated — the channel poll times out every 250ms and the
              // loop spins with the clock past zero. Windows Error Reporting holding a
              // crashed process open while it writes a dump keeps HasExited false for
              // minutes, which is exactly the window this covers.
              if continueGame && currentPos = board.Position then
                  let duration = moveTimer.Elapsed
                  let useNodes = isNodeLimit currentPlaying
                  let playerTime = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
                  let remainingTicks = playerTime - duration.Ticks
                  if (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L) then
                      let firstTwo = firstTwoEvals fullEvalList
                      logger.LogCritical("Engine {Engine} lost on time while sending no output (exited: {Exited}). Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}",
                                         currentPlaying.Name, currentPlaying.HasExited(),
                                         (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds),
                                         duration.TotalMilliseconds)
                      logger.LogCritical(currentPlaying.GetDiagnostics())
                      result <- lostOnTimeResult currentPlaying.Name currentOpponent.Name isWhite gameMoveList gametimer firstTwo
                      continueGame <- false

              if currentPos <> board.Position then
                  currentPos <- board.Position
                  let timeConfig = findTimeSetting currentPlaying
                  evalList <- []
                  npsList.Clear()
                  depth <- 0
                  selfdepth <- 0
                  resetThisMoveEval currentPlaying.Name
                  if ponderHit then
                      try currentPlaying.PonderHit() with ex -> logger.LogWarning(ex, "Failed to send ponderhit to {Engine}", currentPlaying.Name)
                      ponderHit <- false
                      do! Async.Sleep 200
                  else
                      let fenAndMoves = board.PositionWithMoves()
                      currentPlaying.Position fenAndMoves
                      if timeConfig.NodeLimit then
                          currentPlaying.GoNodes timeConfig.Nodes
                      else
                          // moves-to-go control sends a counting-down movestogo; harmless (GetTime) otherwise
                          let movesDone = board.NextMoveNumber() - 1
                          currentPlaying.Go(tourny.TimeControl.GetTimeForMove timeConfig movesDone, wTime, bTime)
                  moveTimer.Restart()
                  lastCheck <- 250L

              //reading from the per-game channel for the engine
              let engineReader = getEngineReaderFor currentPlaying
              try
                  use pollCts = new CancellationTokenSource(250)
                  use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(cts.Token, pollCts.Token)
                  let! line = engineReader.ReadAsync(linkedCts.Token).AsTask() |> Async.AwaitTask
                  if line.StartsWith "bestmove" then
                      do! processBestMove currentPlaying currentOpponent isWhite line
                  else
                      do! processInfoLine engineReader currentPlaying line
              with
              | :? OperationCanceledException ->
                  if cts.IsCancellationRequested then
                      logger.LogInformation("playWithPondering: ReadAsync cancelled, cancelling game loop")
                      cancel()
                      continueGame <- false
              | :? System.Threading.Channels.ChannelClosedException ->
                  // channel closed (writer finished) -> treat as cancellation/stop
                  logger.LogInformation("playWithPondering: engine channel closed")
                  cancel()
                  continueGame <- false
              | ex ->
                  logger.LogWarning(ex, "Error reading engine channel in playWithPondering")
                  // treat as cancellation to ensure we return a result
                  cancel()
                  continueGame <- false

      finally
          // Cleanup on exit: cancel and dispose per-game writer CTSs and complete channels
          match player1WriterCts with
          | Some pcts -> try pcts.Cancel(); pcts.Dispose() with _ -> ()
          | None -> ()
          match player2WriterCts with
          | Some pcts -> try pcts.Cancel(); pcts.Dispose() with _ -> ()
          | None -> ()
          callback (EndOfGame result)
          try player1Channel.Writer.TryComplete() |> ignore with _ -> ()
          try player2Channel.Writer.TryComplete() |> ignore with _ -> ()

      return result
  }


/// Unified play implementation: use optional replay dicts to enable "do not deviate" mode.
let playGeneric
  (skipEngineInit: bool)
  (replayOptWhite: ReferenceGameReplay option)
  (replayOptBlack: ReferenceGameReplay option)
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing: Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =

  let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
  sb.Clear() |> ignore
  let append (txt:string) = sb.Append txt |> ignore

  if tourny.TestOptions.WriteToConsole then
    player1.ShowCommands()
    player2.ShowCommands()
  let stm = board.Position.STM
  let mutable lastEngineStatus = EngineStatus.Empty
  let mutable pos = 0UL
  let mutable moves = 0
  let mutable numberOfNodes = 0L
  let mutable evalList : EvalType list = []
  let mutable fullEvalList :EvalType list = []

  // Track eval per engine and per move to avoid accidentally reusing the opponent's last ply eval.
  let lastEvalByEngine = System.Collections.Generic.Dictionary<string, EvalType>()
  let lastEvalThisMove = System.Collections.Generic.Dictionary<string, EvalType>()
  do
    lastEvalByEngine[player1.Name] <- EvalType.NA
    lastEvalByEngine[player2.Name] <- EvalType.NA
    lastEvalThisMove[player1.Name] <- EvalType.NA
    lastEvalThisMove[player2.Name] <- EvalType.NA

  let resetThisMoveEval (engineName: string) =
    lastEvalThisMove[engineName] <- EvalType.NA

  let setEval (engineName: string) (eval: EvalType) =
    lastEvalByEngine[engineName] <- eval
    lastEvalThisMove[engineName] <- eval
  let npsList = ResizeArray<float>()
  let gameMoveList = board.SanMovesPlayed
  let findTimeSetting (player : ChessEngine) =
    tourny.FindTimeControl (player.Config.TimeControlID)
  let isNodeLimit player = (findTimeSetting player).NodeLimit
  let wPlayer = (findTimeSetting player1)
  let bPlayer = (findTimeSetting player2)
  let mutable wTime = wPlayer.Fixed
  let mutable bTime = bPlayer.Fixed
  let wIncr = tourny.TimeControl.GetIncrementTime(player1.Config.TimeControlID)
  let bIncr = tourny.TimeControl.GetIncrementTime(player2.Config.TimeControlID)
  let moveOverheadInTicks = tourny.MoveOverhead.Ticks
  let delaySeconds = tourny.DelayBetweenGames.ToTimeSpan().TotalSeconds
  let delayMilliseconds = tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds
  let msg = sprintf "Initializing players: %s vs %s with delay: %.2f seconds (%.0f ms)" player1.Name player2.Name delaySeconds delayMilliseconds
  //let mutable enginesInitialized = false
  logger.LogInformation msg
  tourny.CurrentGameNr <- tourny.CurrentGameNr + 1

  let gameStartInfo : StartGameInfo =
    {
      WhitePlayer = player1.Config
      BlackPlayer = player2.Config
      StartPos = board.FEN()
      OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
      WhiteTime = wTime
      BlackTime = bTime
      WhiteToMove = stm = 0uy
      OpeningName = tourny.OpeningName
      CurrentGameNr = pairing.GameNr //tourny.CurrentGameNr
      OpeningHash = pairing.OpeningHash
    }
  board.MovesAndFenPlayed.Clear()
  let mutable moveInfoData = ChessMoveInfo.Empty

  callback(StartOfGame gameStartInfo)
  
  // Send UCI_Chess960 so EB is authoritative about the variant,
  // overriding any misconfigured engine.json.
  let chess960Option : EngineOption = {Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
  player1.AddSetOption chess960Option
  player2.AddSetOption chess960Option
  if not skipEngineInit then
    try
        
        if tourny.MoveOverhead.Ticks > 0 then
            let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
            player1.SetMoveOverhead("overhead", ms)
            player2.SetMoveOverhead("overhead", ms)

        if not tourny.ConsoleOnly then
          let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
          let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
          let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
          GameInitialization.initEngines openingDelayMs tourny player1 player2 logger
          logger.LogInformation($"Initialization done for GUI play: {player1.Name}, {player2.Name}")
        else
          if player1.HasExited() || player2.HasExited() then
            GameInitialization.initEngines 0 tourny player1 player2 logger
            logger.LogInformation($"Initialization done for console play: {player1.Name}, {player2.Name}")
    with ex -> raise (CustomException.EngineStartupException (ex.Message))

  // preserve existing logging/description behavior
  logEngineInitCommands logger player1 player2
  GameInitialization.appendGameDescription sb tourny player1 player2 (board.OpeningMovesPlayed) (board.FEN())
  callback (GameStarted player1.Name)
  let mutable lastCheck = 0L
  let gametimer = Stopwatch.GetTimestamp()
  let mutable moveTimer = Stopwatch.GetTimestamp()
  let mutable depth = 0
  let mutable selfdepth = 0
  let mutable Player1PV = String.Empty
  let mutable Player2PV = String.Empty
  let mutable PVLine1 = String.Empty
  let mutable PVLine2 = String.Empty
  let mutable Q1DifferentFromN1 = 0
  let mutable ct : CancellationToken = CancellationToken.None
  let fen = board.FEN()
  logger.LogDebug $"After opening moves, FEN={fen}"

  let adjudicatedResult (adj: UserAdjudication) =
    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let firstTwo = firstTwoEvals fullEvalList
    createResultWithEval player1.Name player2.Name gameMoveList adj.Result adjudicationReason dur firstTwo

  let mutable userAdjudicationResult : Result option = None

  let applyUserAdjudication (adj: UserAdjudication) =
    match userAdjudicationResult with
    | Some res -> res
    | None ->
        try player1.Stop() with _ -> ()
        try player2.Stop() with _ -> ()
        let res = adjudicatedResult adj
        userAdjudicationResult <- Some res
        callback(EndOfGame res)
        logger.LogInformation($"Game adjudicated by user: {adj.Result}")
        res

  let tryConsumeUserAdjudication () =
    match userAdjudicationResult with
    | Some res -> Some res
    | None ->
        match tryGetUserAdjudication() with
        | Some adj when adj.GameNr = pairing.GameNr -> Some (applyUserAdjudication adj)
        | _ -> None

  let rec readLineOrAdjudication (playing: ChessEngine) (ct: CancellationToken) = async {
    match tryConsumeUserAdjudication() with
    | Some res -> return Choice2Of2 res
    | None ->
        use pollCts = new CancellationTokenSource(250)
        use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(ct, pollCts.Token)
        try
          let! line = playing.ReadLineAsyncWithTimeout(linkedCts.Token) |> Async.AwaitTask
          return Choice1Of2 line
        with
        | :? OperationCanceledException as oce ->
            if ct.IsCancellationRequested then
              return raise oce
            else
              return! readLineOrAdjudication playing ct
    }
  let rec playEngine (playing: ChessEngine) (opponent: ChessEngine) (position:uint64) = async {

    match tryConsumeUserAdjudication() with
    | Some res -> return res
    | None ->

    if cts.IsCancellationRequested then
      let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
      let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
      // Not Critical — a cancel is not a failure.
      logger.LogInformation($"Cancel requested when engine ready to play: {playing.Name}")
      callback(EndOfGame res)
      return res
    else
      if position <> pos then
        let isWhite = playing.Name = player1.Name
        let timeLeftTicks = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
        let timeOutInMs = (TimeSpan(timeLeftTicks).TotalMilliseconds |> int32) + 2000
        ct <- (new CancellationTokenSource(timeOutInMs)).Token

        moveTimer <- Stopwatch.GetTimestamp()
        pos <- position
        evalList <- []
        npsList.Clear()
        depth <- 0
        selfdepth <- 0
        resetThisMoveEval playing.Name

        let fenAndMoves = board.PositionWithMoves()
        if tourny.VerboseLogging then
          logger.LogDebug $"Current position: {fenAndMoves}"
        playing.Position fenAndMoves
        lastCheck <- int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)

        let timeConfig = findTimeSetting playing
        if tourny.TestOptions.ValueTest then
          if playing.IsLc0 then
            playing.GoNodes 2
          else
            playing.GoValue()
        elif tourny.TestOptions.PolicyTest then
          playing.GoNodes 1
        elif timeConfig.NodeLimit then
          playing.GoNodes timeConfig.Nodes
        else
          let movesDone = board.NextMoveNumber() - 1
          playing.Go(tourny.TimeControl.GetTimeForMove timeConfig movesDone, wTime, bTime)

      let! lineOrAdj = readLineOrAdjudication playing ct
      match lineOrAdj with
      | Choice2Of2 res -> return res
      | Choice1Of2 line ->

      if String.IsNullOrWhiteSpace line then
        logger.LogTrace $"Empty line or null from {playing.Name}"
        if playing.HasExited() then
          do! Async.Sleep 1000
          match playing.GetExitCode() with
          | Some code -> logger.LogCritical $"Engine {playing.Name} has exited with exitcode {code}"
          | None -> logger.LogCritical $"Engine {playing.Name} has exited unexpectedly"
          // create result and return
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let firstTwoEvals = firstTwoEvals fullEvalList
          let res, msg =
              if opponent.HasExited() then
                  let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                  res, $"Shutdown detected after {playing.Name} exited; returning Cancel"
              else
                  let resValue = if playing.Name = player1.Name then "0-1" else "1-0"
                  createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected playing.Name) dur firstTwoEvals,
                  $"Player has exited/crashed {playing.Name}"
          logger.LogInformation msg
          // Exit code and the stderr tail, same as the other crash paths — this branch is
          // the one a process that dies mid-search actually takes, and it was the only one
          // not saying why.
          logger.LogCritical(playing.GetDiagnostics())
          callback(EndOfGame res)
          return res

        else
          // A silent engine still burns clock. The loss-on-time check further down only runs
          // when a line arrives, so without this a crashed or hung engine is never
          // adjudicated: the read poll returns null every 250ms and the game recurses
          // forever with the clock past zero. Windows Error Reporting holding a crashed
          // process open while it writes a dump keeps HasExited false for minutes, which is
          // exactly the window this covers.
          let duration = Stopwatch.GetElapsedTime(moveTimer)
          let useNodes = isNodeLimit playing
          let isWhite = playing.Name = player1.Name
          let playerTime = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
          let remainingTicks = playerTime - duration.Ticks
          if (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L) then
            let firstTwoEvals = firstTwoEvals fullEvalList
            logger.LogCritical("Engine {Engine} lost on time while sending no output (exited: {Exited}). Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}",
                               playing.Name, playing.HasExited(),
                               (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds),
                               duration.TotalMilliseconds)
            let res = lostOnTimeResult playing.Name opponent.Name isWhite gameMoveList gametimer firstTwoEvals
            logger.LogCritical(playing.GetDiagnostics())
            callback(EndOfGame res)
            return res
          else
            return! playEngine playing opponent position

      elif line.StartsWith "info engine" then
        MessagesFromEngine ("Ceres", line) |> callback
        logger.LogInformation line
        return! playEngine playing opponent position

      elif isAppShuttingDown cts then
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let firstTwoEvals = firstTwoEvals fullEvalList
          let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
          callback(EndOfGame res)
          logger.LogInformation($"Shutdown/cancel detected while {playing.Name} thinking")
          // The user pressed stop — nothing to diagnose.
          logger.LogDebug(playing.GetDiagnostics())
          logger.LogDebug(opponent.GetDiagnostics())
          return res
      elif playing.HasExited() then
          do! Async.Sleep 1000
          match playing.GetExitCode() with
          | Some code -> logger.LogCritical $"Engine {playing.Name} has exited with exitcode {code}"
          | None -> logger.LogCritical $"Engine {playing.Name} has exited unexpectedly"

          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let firstTwoEvals = firstTwoEvals fullEvalList
          let res, msg =
              if opponent.HasExited() then
                  let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                  res, $"Shutdown detected after {playing.Name} exited; returning Cancel"
              else
                  let resValue = if playing.Name = player1.Name then "0-1" else "1-0"
                  createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected playing.Name) dur firstTwoEvals,
                  $"Player has exited/crashed {playing.Name}"
          callback(EndOfGame res)
          logger.LogInformation msg
          let diagnosis = playing.GetDiagnostics()
          logger.LogCritical diagnosis
          return res

      else
        let duration = Stopwatch.GetElapsedTime(moveTimer)
        let useNodes = isNodeLimit playing
        let isWhite = playing.Name = player1.Name
        let playerTime = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
        let remainingTicks = playerTime - duration.Ticks
        let lostOnTime = (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L)
        if lostOnTime then
          let firstTwoEvals = firstTwoEvals fullEvalList
          if playing.HasExited() then
              logger.LogCritical($"Engine {playing.Name} has exited while lost on time")
          logger.LogCritical("Engine {Engine} lost on time. Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}", playing.Name, (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds), duration.TotalMilliseconds)
          let res = lostOnTimeResult playing.Name opponent.Name isWhite gameMoveList gametimer firstTwoEvals
          let diagnosis = playing.GetDiagnostics()
          logger.LogCritical diagnosis
          // Every other terminal path in this function raises EndOfGame; this one did not,
          // so a plain loss on time reached the PGN and the standings through the returned
          // result but never through the live event, and the GUI only picked it up on the
          // next periodic sync.
          callback(EndOfGame res)
          return res
        else
            if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
                let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
                do! Async.Sleep delay

            if line.StartsWith("bestmove") then
                // make move mutable so "do not deviate" can substitute an old move if needed
                let mutable move = line.Split().[1]
                let ponderMove = if line.Contains "ponder" then (line.Split().[3]) else ""
                let (currentTime, timeLeft) =
                  if isWhite then
                    GameHelpers.updateClocks duration wTime wIncr useNodes
                  else
                    GameHelpers.updateClocks duration bTime bIncr useNodes
                if isWhite then
                  wTime <- currentTime
                else
                  bTime <- currentTime
                // Repeating moves-to-go: top up the mover's base time when it completes a period
                // (board is pre-move here; MakeMove happens below).
                let mtgPeriod = tourny.TimeControl.MovesToGoPeriod
                if mtgPeriod > 0 && not useNodes && board.NextMoveNumber() % mtgPeriod = 0 then
                    let cfg = tourny.TimeControl.GetTimeConfig(playing.Config.TimeControlID)
                    if isWhite then wTime <- TimeOnly(wTime.Ticks + cfg.Fixed.Ticks)
                    else bTime <- TimeOnly(bTime.Ticks + cfg.Fixed.Ticks)
                let mutable posToCheck = board.Position
                let piecesLeft = PositionOps.numberOfPieces &posToCheck
                moveInfoData.tl <- int64 (currentTime.ToTimeSpan().TotalMilliseconds)
                moveInfoData.mt <- int64 duration.TotalMilliseconds
                moveInfoData.pcs <- byte piecesLeft

                match tryGetTMoveFromUciNotation &board move with
                |Some tmove ->
                  let mutable moveAdj = tmove
                  let mutable shortSan = getSanNotationFromTMove &board tmove
                  // Deviation logic only when replay dictionaries provided
                  let shouldPreventForPlayer =
                    tourny.PreventMoveDeviationFor |> isNull ||
                    tourny.PreventMoveDeviationFor.Length = 0 ||
                    tourny.PreventMoveDeviationFor |> Array.contains playing.Name
                  let deviated, oldMove, engName =
                    match replayOptWhite, replayOptBlack with
                    | Some rw, Some rb when shouldPreventForPlayer ->
                        let hash = board.DeviationHash()
                        let replay = if playing.Name = player1.Name then rw else rb
                        match replay.TryGet(hash) with
                        | None -> false, "", ""
                        | Some rd ->
                            if rd.Move <> move then
                              if rd.Engine = playing.Name then true, rd.Move, rd.Engine
                              else false, "", ""
                            else false, "", ""
                    | _ -> false, "", ""

                  if deviated && playing.Config.ContemptEnabled then
                      // if contempt enabled, allow deviation but log it
                      ConsoleUtils.printInColor
                          ConsoleColor.Green
                          $"Deviation detected at plycount {board.PlyCount} and was allowed because of contempt enabled\n  Prev move: {oldMove} by {engName}  Current move: {move} by {playing.Name}"
                      board.UciMovesPlayed.Add(move)
                      gameMoveList.Add(shortSan)
                      board.MakeMove &tmove

                  elif deviated then
                    match tryGetTMoveFromUciNotation &board oldMove with
                    |Some orgMove ->
                      shortSan <- getSanNotationFromTMove &board orgMove
                      tourny.DeviationCounter <- tourny.DeviationCounter + 1
                      move <- oldMove
                      board.UciMovesPlayed.Add(move)
                      gameMoveList.Add(shortSan)
                      board.MakeMove &orgMove
                    |_ -> // quick fix for FRC castling move or illegal previous move
                      if TMoveOps.isCastlingMove tmove && board.IsFRC then
                        RuntimeUtilities.ConsoleUtils.printInColor
                          ConsoleColor.Red
                          $"Deviation bug corrected in FRC castling move with movetype: {tmove.MoveType} - {playing.Name} MoveNr: {moves} Prev move: {oldMove} Current move: {move}"
                      else
                        RuntimeUtilities.ConsoleUtils.printInColor
                          ConsoleColor.Red
                          $"Deviation detected but previous move illegal: {playing.Name} MoveNr: {moves} Prev move: {oldMove} Current move: {move}"
                      board.UciMovesPlayed.Add(move)
                      gameMoveList.Add(shortSan)
                      board.MakeMove &tmove
                  else
                    // record replay entry only when replay dictionaries are supplied
                    let devHash = board.DeviationHash()
                    match replayOptWhite, replayOptBlack with
                    | Some rw, Some rb ->
                        let replay = if playing.Name = player1.Name then rw else rb
                        replay[devHash] <- {Engine=playing.Name; Move = move; TimeLeftInMs = moveInfoData.tl; Hash = pairing.OpeningHash }
                    | _ -> ()
                    board.UciMovesPlayed.Add(move)
                    gameMoveList.Add(shortSan)
                    board.MakeMove &tmove

                  moves <- moves + 1
                  let ponderSan = getShortSanFromLongSan &board ponderMove
                  moveInfoData.pd <- ponderSan
                  let eval =
                    if evalList.Length > 0 then evalList.[0]
                    else
                      //logger.LogInformation($"No eval received for move {move} by {playing.Name} at ply {board.PlyCount}, using last known eval")
                      let thisMove = lastEvalThisMove[playing.Name]
                      if thisMove <> EvalType.NA then
                        thisMove
                      else
                        match lastEvalByEngine.TryGetValue playing.Name with
                        | true, e when e <> EvalType.NA -> e
                        | _ -> EvalType.CP 0.0
                  //maybe add engineStatus1 and engineStatus2 here
                  let pv, pvLong = if playing.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
                  let nps = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
                  let nps =
                    if nps <> 0. then
                      nps
                    else
                      let s = float numberOfNodes/float duration.TotalSeconds
                      moveInfoData.s <- int64 s
                      s

                  let pos = board.Position
                  let fen = BoardHelper.posToFen pos
                  let moveDetail =
                    {
                      LongSan = move
                      FromSq = move[0..1]
                      ToSq = move[2..3]
                      Color = if board.Position.STM = 8uy then "w" else "b"
                      IsCastling = TMoveOps.isCastlingMove tmove
                      Comments = String.Empty
                      }
                  let moveAndFen = {Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen}
                  fullEvalList <- eval::fullEvalList
                  let movesLeft =
                    GameAdjudication.movesLeftBeforeDrawAdjudication
                      eval
                      fullEvalList
                      tourny.Adjudication.DrawOption.MinDrawMove
                      (tourny.Adjudication.DrawOption.DrawMoveLength * 2)
                      tourny.Adjudication.DrawOption.MaxDrawScore
                  let bestMove =
                    { Player = playing.Name
                      Move = move
                      Ponder = ponderSan
                      Eval = eval
                      TimeLeft = timeLeft
                      MoveTime = TimeOnly(duration.Ticks)
                      NPS = nps
                      Nodes = numberOfNodes
                      FEN = fen
                      PV = pv
                      LongPV = pvLong
                      MoveAndFen = moveAndFen
                      MoveHistory = board.GetSanMoveHistory()
                      Move50 = board.Position.Count50 |> int
                      R3 = board.RepetitionNr()
                      PiecesLeft = piecesLeft
                      AdjDrawML = movesLeft
                      }

                  if bestMove.R3 > 1 && tourny.VerboseLogging then
                    logger.LogDebug($"Ply {board.PlyCount} - Repetition occurred: {bestMove.R3} time(s)")

                  evalList <- []
                  depth <- 0
                  selfdepth <- 0
                  npsList.Clear()
                  resetThisMoveEval playing.Name

                  if moveInfoData.q2 > moveInfoData.q1 then
                    Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                  if moveInfoData.n2 > moveInfoData.n1 then
                    Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                  if tourny.VerboseLogging then
                    logger.LogDebug $"FEN={board.FEN()}"

                  //tablebase adjudication here
                  match GameAdjudication.adjudicateByEval logger board fullEvalList tourny player1.Name player2.Name playing.Name gametimer gameMoveList moves with
                  |Some res ->
                    if res.Reason = ResultReason.Checkmate then
                      let bm = {bestMove with MoveHistory=bestMove.MoveHistory + "#"}
                      let status = {lastEngineStatus with Eval = EvalType.Mate 0}
                      let numberAndMove = (board.SanMoveNumberString shortSan) + "#"
                      annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
                      callback(BestMove (bm, status))
                    else
                      let numberAndMove = board.SanMoveNumberString shortSan
                      annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
                      callback(BestMove (bestMove, lastEngineStatus))

                    moveInfoData <- ChessMoveInfo.Empty
                    callback(EndOfGame res)
                    if tourny.VerboseLogging then
                      logger.LogInformation($"Adjudication by eval: {res.Reason}, Pieces left: {piecesLeft} ")
                      logger.LogInformation (sprintf "Info %A: " res)
                    return res
                  |None ->
                    let numberAndMove = board.SanMoveNumberString shortSan
                    annotation tourny.MoveAnnotation board numberAndMove moveInfoData |> append
                    moveInfoData <- ChessMoveInfo.Empty
                    callback(BestMove (bestMove, lastEngineStatus))
                    return! playEngine opponent playing (board.PositionHash())
                |_ ->
                  let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                  let firstTwoEvals = firstTwoEvals fullEvalList
                  if board.AnyLegalMove() |> not then
                    //either checkmate or stalemate
                    if board.IsMate() then
                      let res =
                        if playing.Name = player1.Name then
                          createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Checkmate dur firstTwoEvals
                        else
                          createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Checkmate dur firstTwoEvals
                      callback(EndOfGame res)
                      logger.LogInformation($"Checkmate: {res.Reason}")
                      return res
                    else
                      let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Stalemate dur firstTwoEvals
                      callback(EndOfGame res)
                      logger.LogInformation($"Stalemate: {res.Reason}")
                      return res
                  else
                    let res =
                      if playing.Name = player1.Name then
                        createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur firstTwoEvals
                      else
                        createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur firstTwoEvals
                    callback(EndOfGame res)
                    let fenAndMoves = board.PositionWithMoves()
                    logger.LogCritical($"{playing.Name} failed in bestmove logic with the following response {line} after these moves: \n{fenAndMoves}")
                    return res

            else
              let elapsed = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
              let diff = elapsed - lastCheck
              let interval = 1000

              if not tourny.TestOptions.PolicyTest && line.StartsWith "info string" && line.Contains "N:" then
                let nnMsg = Regex.getInfoStringData playing.Name line
                let list = ResizeArray<NNValues>()
                list.Add(nnMsg)
                let moreItems = line.StartsWith "info string node" |> not
                if not moreItems && tourny.VerboseLogging then
                  logger.LogDebug "Only one move in log live stats"
                let mutable cont = moreItems
                while cont do
                  let! newline = playing.ReadLineAsyncWithTimeout cts.Token |> Async.AwaitTask
                  if newline.StartsWith "bestmove" then
                    cont <- false
                  elif newline.StartsWith "info string node" then
                    cont <- false
                  else
                    let msg = Regex.getInfoStringData playing.Name newline
                    list.Add msg

                makeShortSan list &board
                match Engine.calcTopNn list with
                |Some (n1,n2,q1,q2, p1, pt) ->
                  moveInfoData.n1 <- n1
                  moveInfoData.n2 <- n2
                  moveInfoData.q1 <- q1
                  moveInfoData.q2 <- q2
                  moveInfoData.p1 <- p1
                  moveInfoData.pt <- pt
                |None -> ()

                if list.Count > 0 then
                  callback (NNSeq list)

              elif line.StartsWith "info" then
                let isWhite = playing.Name = player1.Name
                match Regex.getEssentialDataWithEPS line isWhite with
                |Some (d, eval, nodes, nps, eps, pvLine, tbhits, wdl, sd, mPv ) ->
                  numberOfNodes <- nodes
                  if d > depth then
                    depth <- d
                  if sd > selfdepth then
                    selfdepth <- sd
                  npsList.Add(float nps)
                  evalList <- eval :: evalList
                  setEval playing.Name eval
                  moveInfoData.d <- depth
                  moveInfoData.sd <- selfdepth
                  moveInfoData.wv <- eval
                  moveInfoData.n <- nodes
                  moveInfoData.s <- nps
                  moveInfoData.tb <- tbhits
                  moveInfoData.eps <- eps

                  // Skip fail-high/low lines: their PV is cut to the root move, and a move
                  // that ends on one would carry that stump into the PGN comment for good.
                  if not (String.IsNullOrEmpty(pvLine)) && not (Regex.isBoundLine line) then
                    if player1.Name = playing.Name then
                      Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                      PVLine1 <- pvLine
                      moveInfoData.pv <- Player1PV
                    else
                      Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                      PVLine2 <- pvLine
                      moveInfoData.pv <- Player2PV
                  let nps = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
                  let pv, pvLong = if playing.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
                  let status =
                      {
                        PlayerName = playing.Name
                        Eval = eval
                        Depth = d
                        SD = sd
                        Nodes = nodes
                        NPS = nps //avgNps
                        EPS = float eps
                        TBhits = tbhits
                        WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
                        PV = pv
                        PVLongSAN = pvLong
                        MultiPV = mPv
                      }

                  lastEngineStatus <- status
                  if diff > interval && eval <> EvalType.NA then
                    lastCheck <- elapsed
                    if playing.Name.ToLower().Contains("lc0") && status.EPS = 0 then
                      logger.LogInformation $"LC0 is reporting EPS = 0, by {playing.Name} at ply {board.PlyCount}"
                    callback(Status status)

                |None -> ()

              return! playEngine playing opponent position
          }
  let startPos = board.PositionHash()
  if board.Position.STM = 0uy then
    playEngine player1 player2 startPos
  else
    playEngine player2 player1 startPos

/// Play a game without pondering or deviation prevention
let play
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing: Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =
  playGeneric false None None sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback

/// Play a game without engine initialization (engines must already be running).
/// Used by parallel/console execution where engines are pre-initialized.
let playConsole
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing: Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =
  playGeneric true None None sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback

/// Play a game with deviation prevention enabled
let playDoNotDeviate
  (replayWhite: ReferenceGameReplay)
  (replayBlack: ReferenceGameReplay)
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing : Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =
  playGeneric false (Some replayWhite) (Some replayBlack) sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback

/// Play a game with deviation prevention, without engine initialization.
/// Used by parallel/console execution where engines are pre-initialized.
let playConsoleDoNotDeviate
  (replayWhite: ReferenceGameReplay)
  (replayBlack: ReferenceGameReplay)
  (sb : StringBuilder)
  (cts : CancellationTokenSource)
  (logger : ILogger)
  (tourny : Tournament)
  (board : Board)
  (player1 : ChessEngine)
  (player2 : ChessEngine)
  (pairing : Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  callback  =
  playGeneric true (Some replayWhite) (Some replayBlack) sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback
