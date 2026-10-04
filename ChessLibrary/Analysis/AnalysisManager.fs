module ChessLibrary.AnalysisManager

open System
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.AnalysisHelper

/// The analysis engine behind a page. With an Action<SearchUpdate> each update comes with the FEN
/// of the search it belongs to, so the page can tell a result for another position.
type SimpleEngineAnalyzer (engineConfig, board, logger, onSearchUpdate: Action<SearchUpdate>, writeToConsole) =
    let SearchDict = new System.Collections.Generic.Dictionary<string,int>()
    let board : Chess.Board = board
    let moveBoard = Chess.Board()
    let logger : ILogger = logger

    let mutable ChessEngine = None
    let distributionEngine() : ChessEngine =
      match ChessEngine with
      |Some eng -> eng
      |None ->
          let eng = EngineHelper.createEngine (engineConfig, Some logger)
          let isReady = waitForEngineIsReady eng |> Async.RunSynchronously
          if not isReady then
              failwith $"Engine {eng.Name} did not respond to isready command"
          ChessEngine <- Some eng
          eng

    let engine = EngineHelper.createAltEngineForSearches (onSearchUpdate.Invoke, engineConfig, logger, writeToConsole)

    /// The board's position, or None when it has no legal move: then the request still ends, in
    /// turn, with SearchStopped, and a running search is replaced.
    let boardPosition () =
      if board.AnyLegalMove() then Some (board.PositionWithMovesFromGraph())
      else
        logger.LogInformation ("No legal moves with FEN: " + board.FEN())
        None

    /// The request's id; a board with no legal move is a Skip, which ends at once.
    let analyse (go: string) =
      match boardPosition () with
      | Some pos -> engine.Analyse(pos, go)
      | None -> engine.Skip()

    new (engineConfig, board, logger, callback: Action<EngineUpdate>, writeToConsole) =
      SimpleEngineAnalyzer(engineConfig, board, logger, Action<SearchUpdate>(fun u -> callback.Invoke u.Update), writeToConsole)

    member val Board = board with get, set
    member x.Engine = engine
    member x.TryGetMovePolicyAndTopForPosSequence(player:string, qMin:float, qMax:float) =
      let distEngine = distributionEngine()
      tryGetMovePolicyAndTopForPosSequence distEngine board player qMin qMax

    member x.TryGetMoveQAndTopForPosSequence(player:string, qMin:float, qMax:float) =
      let distEngine = distributionEngine()
      tryGetMoveQAndTopForPosSequence distEngine board player qMin qMax

    member x.Stop() = engine.Stop()

    member x.StopDistributionEngine() =
      let distEngine = distributionEngine()
      distEngine.StopProcess()
      ChessEngine <- None

    member x.Reset() =
      engine.Stop()
      engine.NewGame()
      SearchDict.Clear()

    member x.Quit() = engine.Quit()

    member x.UCI() = engine.Raw "uci"

    member x.NewGame() = engine.NewGame()

    member x.AddSetoption (option: EngineOption) = engine.SetOption option

    member x.GetEngineName () = engineConfig.Name

    member x.GetNetwork () = engine.Network

    member _.BackendInfo() = engine.GetBackEnd()

    // The searches below return at once with the request's id: the engine takes the newest
    // request, stops what it ran, and reports through the callback with that id.
    member x.GoInfinite() : int = analyse ("go infinite" + engine.SearchMoveSuffix)

    member x.SearchNodes (nodes: int, keepNodes : bool) : int =
      if not keepNodes then SearchDict.Clear()
      analyse (sprintf "go nodes %d%s" nodes engine.SearchMoveSuffix)

    member x.SearchNodesWithCommand (nodes: int, commands:string, keepNodes : bool) : int =
      if not keepNodes then SearchDict.Clear()
      engine.Analyse(commands, sprintf "go nodes %d%s" nodes engine.SearchMoveSuffix)

    member x.SetSearchMoves (moves: string list) = engine.SetSearchMoves moves
    member x.ClearSearchMoves () = engine.ClearSearchMoves()
    member x.SearchMoves with get() = engine.SearchMoves

    member x.DumpStats command = engine.Raw command

    /// A UCI script line: options while the engine is idle, no search control (false when refused).
    member x.Script (line: string) = engine.Script line

    member x.Play (goCommand: string) : int = analyse goCommand

    /// Search from a position command the caller built from a board snapshot; touches no board.
    member x.PlayPrepared (positionCmd: string, goCommand: string) : int =
      engine.Analyse(positionCmd, goCommand)
