/// One game between two engines, for every mode: with or without pondering, with or without
/// deviation prevention. The players do the protocol; this does the chess and the clocks.
module ChessLibrary.Game.GameLoop

open System
open System.Text
open System.Threading
open System.Threading.Tasks
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
open ChessLibrary.RuntimeUtilities
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.Game.Clock
open ChessLibrary.Game.PlayerMachine
open ChessLibrary.Game.Player
open Microsoft.FSharp.Core.Operators.Unchecked

/// In-game ping timeout (cutechess: 15 s).
let pingTimeout = TimeSpan.FromSeconds 15.0
/// How long a stopped search may take to send its bestmove (fastchess: 10 s).
let stopWait = TimeSpan.FromSeconds 10.0
let private statusInterval = TimeSpan.FromSeconds 1.0
/// How often a waiting game looks for a user adjudication.
let private pollInterval = 250

type private Side = { Engine: ChessEngine; Player: Player; Config: TimeConfig; Ponders: bool }

/// The move a side has to play, after deviation prevention.
type private Played = { Uci: string; San: string; TMove: TMove }

let private isWhiteToMove (board: Board) = board.Position.STM = 0uy

/// Deviation prevention: the move the side played before in this position, if it must repeat it.
let private replayMove (tourny: Tournament) (replay: (ReferenceGameReplay * ReferenceGameReplay) option)
                       (board: Board) (engine: ChessEngine) isWhite (move: string) =
  let preventFor =
    isNull tourny.PreventMoveDeviationFor || tourny.PreventMoveDeviationFor.Length = 0
    || Array.contains engine.Name tourny.PreventMoveDeviationFor
  match replay with
  | Some (white, black) when preventFor ->
      match (if isWhite then white else black).TryGet(board.DeviationHash()) with
      | Some rd when rd.Move <> move && rd.Engine = engine.Name -> Some rd.Move
      | _ -> None
  | _ -> None

/// Readies both engines for the game: the pooled path only resets them; the full path also sets
/// MoveOverhead, restarts exited engines, sets contempt and paces the GUI.
let private prepareEngines skipEngineInit (tourny: Tournament) (board: Board) (white: ChessEngine) (black: ChessEngine) (logger: ILogger) = async {
  let prepare (engine: ChessEngine) = async {
    // a Winboard engine with reuse=0 needs a new process for every game
    if not engine.CanReuseWinboard && not (engine.HasExited()) then
      engine.Quit()
      do! engine.StopProcessAsync() |> Async.AwaitTask
    if engine.HasExited() then
      logger.LogInformation("Engine {Engine} has exited, starting it", engine.Name)
      do! engine.StartProcessAsync() |> Async.AwaitTask
    let! ready = engine.PrepareNewGameAsync(max 180 tourny.EngineStartupTimeoutInSec * 1000) |> Async.AwaitTask
    if not ready then
      raise (CustomException.EngineStartupException (sprintf "Engine %s not ready for the next game: %s" engine.Name engine.ReadyFailure)) }
  if skipEngineInit then
    do! prepare white
    do! prepare black
  else
    // not on a time per move: there MoveOverhead is EngineBattle's margin only
    if tourny.MoveOverhead.Ticks > 0 then
      let ms = int tourny.MoveOverhead.TotalMilliseconds
      for engine in [ white; black ] do
        if not (tourny.FindTimeControl engine.Config.TimeControlID).IsMoveTime then engine.SetMoveOverhead("overhead", ms)
    // the GUI shows the opening at its own pace while the engines get ready
    let pacing =
      if tourny.ConsoleOnly then 0
      else int (float board.OpeningMovesPlayed.Count * float tourny.MinMoveTimeInMS + 2000.0)
    let delay = max pacing (int tourny.DelayBetweenGames.TotalMilliseconds)
    let! _ = Async.Parallel [ prepare white; prepare black; Async.Sleep delay ]
    GameInitialization.checkAndPrepareContempt white black }

/// Plays one game. `replay`: deviation prevention's white and black replays.
let play
  (skipEngineInit: bool)
  (replay: (ReferenceGameReplay * ReferenceGameReplay) option)
  (sb: StringBuilder)
  (cts: CancellationTokenSource)
  (logger: ILogger)
  (tourny: Tournament)
  (board: Board)
  (white: ChessEngine)
  (black: ChessEngine)
  (pairing: Pairing)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  (callback: Update -> unit) : Async<Result> = async {

  sb.Clear() |> ignore
  if tourny.TestOptions.WriteToConsole then
    white.ShowCommands()
    black.ShowCommands()
  let timeConfig (engine: ChessEngine) = tourny.FindTimeControl engine.Config.TimeControlID
  let clocks =
    [| Clock.create tourny.TimeControl (timeConfig white) tourny.MoveOverhead
       Clock.create tourny.TimeControl (timeConfig black) tourny.MoveOverhead |]
  tourny.CurrentGameNr <- tourny.CurrentGameNr + 1
  callback (StartOfGame
    { WhitePlayer = white.Config; BlackPlayer = black.Config; StartPos = board.FEN()
      OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
      WhiteTime = clocks.[0].Left; BlackTime = clocks.[1].Left; WhiteToMove = isWhiteToMove board
      OpeningName = tourny.OpeningName; CurrentGameNr = pairing.GameNr; OpeningHash = pairing.OpeningHash })
  board.MovesAndFenPlayed.Clear()

  // the variant comes from this game's board: parallel games may mix FRC and standard
  let chess960 : EngineOption = { Name = "UCI_Chess960"; Value = sprintf "%b" board.IsFRC }
  white.AddSetOption chess960
  black.AddSetOption chess960
  try do! prepareEngines skipEngineInit tourny board white black logger
  with ex ->
    match ex with
    | :? CustomException.EngineStartupException -> raise ex
    | _ -> raise (CustomException.EngineStartupException ex.Message)

  logEngineInitCommands logger white black
  GameInitialization.appendGameDescription sb tourny white black board.OpeningMovesPlayed (board.FEN())
  callback (GameStarted white.Name)
  // both engines' CPU and memory beside the game, until it ends
  let monitorCts = new CancellationTokenSource()
  // cancelled however play ends: a stopped tournament cancels this workflow past Async.Catch
  use _stopMonitor = { new IDisposable with member _.Dispose() = monitorCts.Cancel(); monitorCts.Dispose() }
  ResourceMonitor.start white black (fun () -> isWhiteToMove board) (fun w b -> callback (Resources (w, b))) monitorCts.Token |> ignore

  let gameTimer = Stopwatch.GetTimestamp()
  let gameMoves = board.SanMovesPlayed
  let mutable evals : EvalType list = []           // every move's eval, newest first
  let mutable movesPlayed = 0                       // engine moves, not the opening's
  let lastEval = [| EvalType.NA; EvalType.NA |]     // per side, for a move without info lines
  let moveList = Array.init 256 (fun _ -> defaultof<TMove>)

  let canPonder (engine: ChessEngine) =
    Configuration.Validation.willBeAskedToPonder tourny engine.Config && replay.IsNone && engine.SupportsPonder
  let side (engine: ChessEngine) =
    let settings =
      { Player = engine.Name; CanPing = engine.CanPing; PingTimeout = pingTimeout; StopWait = stopWait
        StatusInterval = statusInterval; PolicyTest = tourny.TestOptions.PolicyTest }
    { Engine = engine; Player = new Player(engineIO engine, settings, callback, logger)
      Config = timeConfig engine; Ponders = canPonder engine }
  let sides = [| side white; side black |]

  let elapsedMs () = int64 (Stopwatch.GetElapsedTime(gameTimer).TotalMilliseconds)
  let resultFor (reason: ResultReason) (score: string) =
    createResultWithEval white.Name black.Name gameMoves score reason (elapsedMs ()) (firstTwoEvals evals)
  let loss isWhite reason = resultFor reason (if isWhite then "0-1" else "1-0")
  let draw reason = resultFor reason "1/2-1/2"

  let view () =
    { ShortPv = fun pv -> getShortSanPVFromLongSanPVFast moveList &board pv
      ShortSan = fun block -> makeShortSan block &board }

  let search isWhite (s: Side) =
    let movesDone = board.NextMoveNumber() - 1
    GoCommand.forMove tourny.TestOptions s.Engine.IsLc0 tourny.TimeControl s.Config movesDone clocks.[0] clocks.[1]

  /// Waits for the search, looking for a user adjudication and a cancel meanwhile.
  let awaitSearch (think: Task<SearchOutcome>) = async {
    let mutable outcome = None
    while outcome.IsNone do
      let! _ = Task.WhenAny(think, Task.Delay pollInterval) |> Async.AwaitTask
      if think.IsCompleted then outcome <- Some (Choice1Of2 think.Result)
      else
        match tryGetUserAdjudication () with
        | Some adj when adj.GameNr = pairing.GameNr -> outcome <- Some (Choice2Of2 (Some adj))
        | _ -> if cts.IsCancellationRequested then outcome <- Some (Choice2Of2 None)
    return outcome.Value }

  /// Puts the move on the board; the function it returns takes the tablebase probe's answer for
  /// the new position, writes the move to the PGN, and gives Some result when it ends the game.
  let applyMove isWhite (s: Side) (played: Played) (ponder: string option) (elapsed: TimeSpan) (stats: SearchStats.SearchStats) =
    let idx = if isWhite then 0 else 1
    let piecesLeft = let mutable p = board.Position in PositionOps.numberOfPieces &p
    let info = SearchStats.toMoveInfo stats
    info.tl <- int64 clocks.[idx].Left.TotalMilliseconds
    info.mt <- int64 elapsed.TotalMilliseconds
    info.pcs <- byte piecesLeft
    movesPlayed <- movesPlayed + 1
    board.UciMovesPlayed.Add played.Uci
    gameMoves.Add played.San
    let mutable tmove = played.TMove
    board.MakeMove &tmove
    let ponderSan = match ponder with Some p -> getShortSanFromLongSan &board p | None -> ""
    info.pd <- ponderSan
    let eval =
      match SearchStats.lastEval stats with
      | Some e -> e
      | None -> if lastEval.[idx] <> EvalType.NA then lastEval.[idx] else EvalType.CP 0.0
    lastEval.[idx] <- eval
    evals <- eval :: evals
    let nps =
      if stats.Nps <> 0.0 then stats.Nps
      else
        let s = float stats.Nodes / elapsed.TotalSeconds
        info.s <- int64 s
        s
    let fen = BoardHelper.posToFen board.Position
    let moveAndFen =
      { Move = { LongSan = played.Uci; FromSq = played.Uci.[0..1]; ToSq = played.Uci.[2..3]
                 Color = (if board.Position.STM = 8uy then "w" else "b")
                 IsCastling = TMoveOps.isCastlingMove played.TMove; Comments = String.Empty }
        ShortSan = played.San; FenAfterMove = fen }
    let draw = tourny.Adjudication.DrawOption
    let bestMove =
      { Player = s.Engine.Name; Move = played.Uci; Ponder = ponderSan; Eval = eval
        TimeLeft = clocks.[idx].Left; MoveTime = elapsed; NPS = nps; Nodes = stats.Nodes; FEN = fen
        PV = stats.Pv; LongPV = stats.LongPv; MoveAndFen = moveAndFen
        MoveHistory = board.GetSanMoveHistory(); Move50 = int board.Position.Count50
        R3 = board.RepetitionNr(); PiecesLeft = piecesLeft
        AdjDrawML = GameAdjudication.movesLeftBeforeDrawAdjudication eval evals draw.MinDrawMove (draw.DrawMoveLength * 2) draw.MaxDrawScore }
    let status = { stats.Status with PlayerName = s.Engine.Name }
    let annotate suffix = annotation tourny.MoveAnnotation board (board.SanMoveNumberString played.San + suffix) info |> sb.Append |> ignore
    let evals = evals
    fun tbOutput ->
      match GameAdjudication.adjudicateByEval logger board evals tourny white.Name black.Name s.Engine.Name gameTimer gameMoves movesPlayed tbOutput with
      | Some res when res.Reason = ResultReason.Checkmate ->
          annotate "#"
          callback (BestMove ({ bestMove with MoveHistory = bestMove.MoveHistory + "#" }, { status with Eval = EvalType.Mate 0 }))
          Some res
      | Some res ->
          annotate ""
          callback (BestMove (bestMove, status))
          Some res
      | None ->
          annotate ""
          callback (BestMove (bestMove, status))
          None

  /// The position after the predicted reply; None when the reply is illegal or ends the game
  /// (no ponder then, as cutechess).
  let afterReply (move: string) =
    let after = Board()
    after.LoadFen(board.FEN())
    match tryGetMoveAndSanFromUci &after move with
    | Some (tmove, _) ->
        let mutable m = tmove
        after.MakeMove &m
        if after.AnyLegalMove() && not (after.InsufficientMaterial()) then Some after else None
    | None -> None

  /// After a move: the mover ponders on its predicted reply, if it may.
  let startPonder isWhite (s: Side) (ponder: string option) =
    match ponder |> Option.filter (fun _ -> s.Ponders) |> Option.bind (fun move -> afterReply move |> Option.map (fun b -> move, b)) with
    | Some (move, after) ->
        let movesDone = board.NextMoveNumber() - (if isWhite then 0 else 1)
        let next = GoCommand.forMove tourny.TestOptions s.Engine.IsLc0 tourny.TimeControl s.Config movesDone clocks.[0] clocks.[1]
        match GoCommand.ponderText next with
        | Some command ->
            let view =
              { ShortPv = fun pv -> getShortSanPVFromLongSanPVFast (Array.init 256 (fun _ -> defaultof<TMove>)) &after pv
                ShortSan = fun block -> makeShortSan block &after }
            s.Player.Ponder { Position = board.PositionWithMoves() + " " + move; Move = move; Command = command
                              WhiteToMove = isWhite; View = view }
        | None -> ()
    | None -> ()

  let lostOnTime isWhite (s: Side) remaining elapsed =
    logger.LogCritical("Engine {Engine} lost on time. Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}",
                       s.Engine.Name, clocks.[if isWhite then 0 else 1].Left.TotalMilliseconds, (elapsed: TimeSpan).TotalMilliseconds)
    logger.LogCritical(s.Engine.GetDiagnostics())
    let opponent = sides.[if isWhite then 1 else 0]
    lostOnTimeResult s.Engine.Name opponent.Engine.Name isWhite gameMoves gameTimer remaining (firstTwoEvals evals)

  /// The search's answer as a move or a result.
  let decide isWhite (s: Side) outcome = async {
    let idx = if isWhite then 0 else 1
    match outcome with
    | Moved (move, ponder, elapsed, stats) ->
        match Clock.afterMove clocks.[idx] elapsed (board.NextMoveNumber()) with
        | LostOnTime remaining -> return Choice2Of2 (lostOnTime isWhite s remaining elapsed)
        | OnTime clock ->
            if elapsed.TotalMilliseconds < float tourny.MinMoveTimeInMS then
              do! Async.Sleep (tourny.MinMoveTimeInMS - int elapsed.TotalMilliseconds)
            clocks.[idx] <- clock
            match tryGetMoveAndSanFromUci &board move with
            | Some (tmove, san) ->
                let played =
                  match replayMove tourny replay board s.Engine isWhite move with
                  | Some old when s.Engine.Config.ContemptEnabled ->
                      ConsoleUtils.printInColor ConsoleColor.Green
                        $"Deviation detected at plycount {board.PlyCount} and was allowed because of contempt enabled\n  Prev move: {old} Current move: {move} by {s.Engine.Name}"
                      { Uci = move; San = san; TMove = tmove }
                  | Some old ->
                      match tryGetMoveAndSanFromUci &board old with
                      | Some (oldMove, oldSan) ->
                          Interlocked.Increment(&tourny.DeviationCounter) |> ignore
                          { Uci = old; San = oldSan; TMove = oldMove }
                      | None ->
                          ConsoleUtils.printInColor ConsoleColor.Red
                            $"Deviation detected but previous move illegal: {s.Engine.Name} Prev move: {old} Current move: {move}"
                          { Uci = move; San = san; TMove = tmove }
                  | None ->
                      match replay with
                      | Some (w, b) ->
                          (if isWhite then w else b).[board.DeviationHash()] <-
                            { Engine = s.Engine.Name; Move = move; TimeLeftInMs = int64 clock.Left.TotalMilliseconds; Hash = pairing.OpeningHash }
                      | None -> ()
                      { Uci = move; San = san; TMove = tmove }
                let finish = applyMove isWhite s played ponder elapsed stats
                let! tbOutput =
                  match GameAdjudication.tablebaseProbe tourny board with
                  | Some (dir, fen, pieces) -> TablebaseProbe.probeAsync dir fen pieces cts.Token
                  | None -> async.Return None
                match finish tbOutput with
                | Some res -> return Choice2Of2 res
                | None ->
                    startPonder isWhite s ponder
                    return Choice1Of2 played.Uci
            | None ->
                // "bestmove resign" is a Winboard engine giving up
                let reason = if move = "resign" then ResultReason.Resignation else ResultReason.Illegal
                if reason = ResultReason.Resignation then logger.LogInformation("{Engine} resigns", s.Engine.Name)
                else logger.LogCritical("{Engine} played an illegal move '{Move}' after: {Position}", s.Engine.Name, move, board.PositionWithMoves())
                return Choice2Of2 (loss isWhite reason)
    | NoBestMove elapsed ->
        match Clock.afterMove clocks.[idx] elapsed (board.NextMoveNumber()) with
        | LostOnTime remaining -> return Choice2Of2 (lostOnTime isWhite s remaining elapsed)
        | OnTime _ ->
            logger.LogCritical(s.Engine.GetDiagnostics())
            return Choice2Of2 (loss isWhite (ResultReason.Stalled s.Engine.Name))
    | Stalled reason ->
        logger.LogCritical("Engine {Engine} stalled: {Reason}", s.Engine.Name, reason)
        logger.LogCritical(s.Engine.GetDiagnostics())
        return Choice2Of2 (loss isWhite (ResultReason.Stalled s.Engine.Name))
    | Crashed ->
        let opponent = sides.[if isWhite then 1 else 0]
        // the exit can lag the closed output; wait for it so the exit code and the verdict are right
        let mutable waited = 0
        while not (s.Engine.HasExited()) && waited < 2000 do
          do! Async.Sleep 100
          waited <- waited + 100
        match s.Engine.GetExitCode() with
        | Some code -> logger.LogCritical("Engine {Engine} has exited with exit code {Code}", s.Engine.Name, code)
        | None -> logger.LogCritical("Engine {Engine} stopped sending output", s.Engine.Name)
        logger.LogCritical(s.Engine.GetDiagnostics())
        // both gone, or a cancel: the run is shutting down
        if opponent.Engine.HasExited() || isAppShuttingDown cts then return Choice2Of2 (draw ResultReason.Cancel)
        else return Choice2Of2 (loss isWhite (ResultReason.Disconnected s.Engine.Name))
    | Interrupted -> return Choice2Of2 (draw ResultReason.Cancel) }

  let rec turn (lastMove: string option) = async {
    let isWhite = isWhiteToMove board
    let s = sides.[if isWhite then 0 else 1]
    match tryGetUserAdjudication () with
    | Some adj when adj.GameNr = pairing.GameNr -> return resultFor ResultReason.AdjudicatedByUser adj.Result
    | _ when cts.IsCancellationRequested -> return draw ResultReason.Cancel
    | _ when not (board.AnyLegalMove()) ->
        // only reachable from the opening; later mates are adjudicated after the move
        return if board.IsMate() then loss isWhite ResultReason.Checkmate else draw ResultReason.Stalemate
    | _ ->
        let searchCommand = search isWhite s
        let request =
          { Position = board.PositionWithMoves(); LastMove = lastMove; Search = searchCommand
            StopAfter = (match searchCommand with GoCommand.ClockTimes _ | GoCommand.MoveTimeMs _ -> Clock.stopAfter clocks.[if isWhite then 0 else 1] | _ -> None)
            WhiteToMove = isWhite; View = view () }
        let think = Async.StartAsTask (s.Player.Think request)
        match! awaitSearch think with
        | Choice2Of2 (Some adj) -> return resultFor ResultReason.AdjudicatedByUser adj.Result
        | Choice2Of2 None -> return draw ResultReason.Cancel
        | Choice1Of2 outcome ->
            match! decide isWhite s outcome with
            | Choice1Of2 played -> return! turn (Some played)
            | Choice2Of2 res -> return res }

  let! outcome = Async.Catch (turn None)
  monitorCts.Cancel()

  // stop and drain whatever still runs, so nothing of this game reaches the next
  let endGame = Async.Parallel [ for s in sides -> s.Player.EndGame() ]
  let! idle =
    if cts.IsCancellationRequested then
      async {
        // the run is being torn down: give the engines a moment, no more
        let! child = Async.StartChild(endGame, 2000)
        match! Async.Catch child with
        | Choice1Of2 idle -> return idle
        | Choice2Of2 _ -> return [| true; true |] }
    else endGame
  for s in sides do (s.Player :> IDisposable).Dispose()
  // an engine that stopped answering gets a new process before its next game
  for s, answering in Array.zip sides idle do
    if not answering then
      logger.LogWarning("Engine {Engine} stopped answering; it is restarted before its next game", s.Engine.Name)
      try
        s.Engine.Quit()
        do! s.Engine.StopProcessAsync() |> Async.AwaitTask
      with _ -> ()
  // an exception goes to the caller's handleGameExceptionAsync, as before
  let result =
    match outcome with
    | Choice1Of2 result -> result
    | Choice2Of2 ex -> System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw(); Unchecked.defaultof<Result>
  match result.Reason with
  | ResultReason.AdjudicatedByUser -> logger.LogInformation("Game adjudicated by user: {Result}", result.Result)
  | ResultReason.Cancel -> logger.LogInformation("Game {GameNr} cancelled", pairing.GameNr)
  | _ -> ()
  callback (EndOfGame result)
  return result }
