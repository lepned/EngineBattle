module ChessLibrary.AnalysisManager

open System
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.AnalysisHelper

type SimpleEngineAnalyzer (engineConfig, board, logger, callback: Action<EngineUpdate>, writeToConsole) =
    let SearchDict = new System.Collections.Generic.Dictionary<string,int>()
    let board : Chess.Board = board
    let moveBoard = Chess.Board()
    let logger : ILogger = logger
    let sendAnalysisResponse (update: EngineUpdate) = callback.Invoke update

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

    let engine = EngineHelper.createAltEngine (sendAnalysisResponse, engineConfig, logger, writeToConsole)

    member val Board = board with get, set
    member x.Engine = engine
    member x.TryGetMovePolicyAndTopForPosSequence(player:string, qMin:float, qMax:float) =
      let distEngine = distributionEngine()
      tryGetMovePolicyAndTopForPosSequence distEngine board player qMin qMax

    member x.TryGetMoveQAndTopForPosSequence(player:string, qMin:float, qMax:float) =
      let distEngine = distributionEngine()
      tryGetMoveQAndTopForPosSequence distEngine board player qMin qMax

    member x.Stop() =
      engine.SendUCICommand Stop

    member x.StopDistributionEngine() =
      let distEngine = distributionEngine()
      distEngine.StopProcess()
      ChessEngine <- None

    member x.Reset() =
      engine.SendUCICommand Stop
      engine.SendUCICommand UciNewGame
      let fen = board.FEN()
      engine.SendUCICommand (Position fen)
      SearchDict.Clear()

    member x.Quit() = engine.ShutDownEngine()

    member x.UCI() = engine.SendUCICommand UCI

    member x.NewGame() = engine.SendUCICommand UciNewGame

    member x.AddSetoption (option: EngineOption) = engine.SendUCICommand (SetOption option)

    member x.GetEngineName () = engineConfig.Name

    member x.GetNetwork () = engine.Network

    member _.BackendInfo() = engine.GetBackEnd()

    member x.GoInfinite() =
      engine.SendUCICommand Stop
      let fen = board.FEN()
      let moves = board.GetMoveHistoryToCurrentFen fen
      board.PrintPosition moves
      if board.AnyLegalMove() |> not then
        let fen = board.FEN()
        logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
      else
        let command = board.PositionWithMovesFromGraph()
        printfn "Search command: %s" command
        engine.SendUCICommand (PositionWithMoves command)
        let isReady = engine.WaitForReadyOk()
        if not isReady then
          failwith $"Engine {engine.Name} did not respond to isready command before go infinite"
        engine.SendUCICommand (GoInfinite)

    member x.SearchNodes (nodes, keepNodes : bool) =
      engine.SendUCICommand Stop
      let fen = board.FEN()
      let moves = board.GetMoveHistoryToCurrentFen fen
      board.PrintPosition moves
      if board.AnyLegalMove() |> not then
        logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
      else
        if not keepNodes then
          SearchDict.Clear()
        let graphCmds = board.PositionWithMovesFromGraph()
        printfn "Search command: %s" graphCmds
        engine.SendUCICommand (PositionWithMoves graphCmds)
        let isReady = engine.WaitForReadyOk()
        if not isReady then
          failwith $"Engine {engine.Name} did not respond to isready command before go nodes"
        engine.SendUCICommand (GoNodes nodes)

    member x.SearchNodesWithCommand (nodes, commands:string, keepNodes : bool) =
      engine.SendUCICommand Stop
      let fen = board.FEN()
      let moves = board.GetMoveHistoryToCurrentFen fen
      moveBoard.PrintPosition moves
      if moveBoard.AnyLegalMove() |> not then
        logger.LogInformation ("In searchNodesWithCommands - no legal moves with command: " + commands)
      else
        if not keepNodes then
          SearchDict.Clear()
        printfn "Search command: %s" commands
        engine.SendUCICommand (PositionWithMoves commands)
        let isReady = engine.WaitForReadyOk()
        if not isReady then
          failwith $"Engine {engine.Name} did not respond to isready command before go nodes"
        engine.SendUCICommand (GoNodes nodes)

    member x.SetSearchMoves (moves: string list) = engine.SetSearchMoves moves
    member x.ClearSearchMoves () = engine.ClearSearchMoves()
    member x.SearchMoves with get() = engine.SearchMoves

    member x.DumpStats command = engine.SendUCICommand (RawCommand command)

    member x.Play (goCommand: string) =
      engine.SendUCICommand Stop
      if board.AnyLegalMove() |> not then
        let fen = board.FEN()
        logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
      else
        let graphCmds = board.PositionWithMovesFromGraph()
        printfn "Search indexed command: %s" graphCmds
        engine.SendUCICommand (PositionWithMoves graphCmds)
        let isReady = engine.WaitForReadyOk()
        if not isReady then
          failwith $"Engine {engine.Name} did not respond to isready command before search"
        engine.SendUCICommand (RawCommand goCommand)

    /// Search from a position command the caller already built from a board snapshot.
    /// Touches no shared board state, so unlike Play/GoInfinite it is safe to call
    /// from a thread that does not own the board.
    member x.PlayPrepared (positionCmd: string, goCommand: string) =
      engine.SendUCICommand Stop
      printfn "Search prepared command: %s" positionCmd
      engine.SendUCICommand (PositionWithMoves positionCmd)
      let isReady = engine.WaitForReadyOk()
      if not isReady then
        failwith $"Engine {engine.Name} did not respond to isready command before search"
      engine.SendUCICommand (RawCommand goCommand)
