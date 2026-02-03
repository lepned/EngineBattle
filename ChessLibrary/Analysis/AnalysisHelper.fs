module ChessLibrary.AnalysisHelper

open System
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.Chess
open ChessLibrary.BoardUtils

type Binary = | Cuda | ONNX | CPU

let neuralNetSetoptionCmd nnPath =
  let opt = EngineOption.Create "WeightsFile" nnPath
  opt

let currentBestBT3Cuda =
  [
    Binary.Cuda, neuralNetSetoptionCmd "C:/Dev/Chess/Networks/BT3/BT3-768x15x24h-swa-480000.pb.gz"
    Binary.Cuda, neuralNetSetoptionCmd "C:/Dev/Chess/Networks/BT3/BT3-768x15x24h-swa-1170000.pb.gz"
    Binary.Cuda, neuralNetSetoptionCmd "C:/Dev/Chess/Networks/BT3/BT3-768x15x24h-swa-2000000.pb.gz"
    Binary.Cuda, neuralNetSetoptionCmd "C:/Dev/Chess/Networks/BT3/BT3-768x15x24h-swa-2790000.pb.gz"
  ]

let waitForEngineIsReady (engine: ChessEngine) = async {
  try
      if engine.HasExited() then
          engine.StartProcess()
      // Bounded wait for "readyok"
      let timeoutInMs = TimeSpan.FromHours(2).TotalMilliseconds |> int // 2h default
      let ok = engine.WaitForReadyOk(timeoutInMs)
      if ok then
          // Engine is ready
          return true
      else
          // Timed out
          return false
  with _ ->
      return false
}

let run (engine:ChessEngine) (tourny:Tournament) (board:Board) (name, fen) =
  let mutable status = EngineStatus.Empty
  let tc = tourny.FindTimeControl engine.Config.TimeControlID
  let startEngine (engine : ChessEngine) =
    if tourny.TestOptions.PolicyTest then
      engine.GoNodes 1
    elif tourny.TestOptions.ValueTest then
      engine.GoValue()
    else
      if tc.NodeLimit then
        engine.GoNodes tc.Nodes
      else
        let fTime = tourny.TimeControl.GetUnion(1).GetFixedtime()
        engine.Go(tourny.TimeControl.GetUnion(1),fTime,fTime)

  let rec start _ =  task {
    let! line = engine.ReadLineAsync()
    if line.StartsWith "bestmove" then
      printfn "\n%s\n\t%s FEN %s  Move: %s" engine.FullName name fen line
      return status
    elif line.StartsWith "info depth" then
      let isWhite = board.Position.STM = 0uy
      match EngineProtocol.Regex.getEssentialData line isWhite with
      |Some (d, eval, nodes, nps, pvLine, tbHits, wdl, sd, mPv ) ->
        let info =
          {
            PlayerName = engine.Name
            Eval = eval
            Depth = d
            SD = sd
            Nodes = nodes
            NPS = float nps
            EPS = 0.0
            TBhits = tbHits
            WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
            PV = pvLine
            PVLongSAN = pvLine
            MultiPV = mPv  }
        status <- info

      |None -> ()
      return! start ()
    else
      return! start ()
    }

  startEngine engine
  start () |> Async.AwaitTask |> Async.RunSynchronously


let playEPDEntryPositions (logger:ILogger) (tourny:Tournament) (positions:ChessLibrary.EPDTypes.EPDEntry seq) (cts: CancellationTokenSource) = async {
  logger.LogInformation($"Fen positions analysis about to start")
  let board = Board()
  board.LoadFen Chess.startPos
  let gamesAlreadyPlayed =
    let fileInfo = System.IO.FileInfo tourny.PgnOutPath
    if fileInfo.Exists then
      FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
    else
      [||]

  tourny.CurrentGameNr <- gamesAlreadyPlayed.Length
  let sb = System.Text.StringBuilder()
  let mutable engine1 = EngineHelper.createEngine (tourny.EngineSetup.Engines[0], Some logger)
  let mutable engine2 = EngineHelper.createEngine (tourny.EngineSetup.Engines[1], Some logger)

  EngineHelper.initEngines 0 engine1 engine2
  let results = ResizeArray<EngineStatus>()
  for pos in positions do
    for i = 1 to 2 do
      tourny.OpeningName <- pos.Id.ToString()
      if cts.IsCancellationRequested then
        ()
      else
        board.ResetBoardState()
        board.LoadFen(pos.FEN)
        logger.LogInformation("{position}", pos.FEN)
        let fenPos = sprintf "position fen %s" pos.FEN
        let engine1, _ =
          if i % 2 = 1 then
            engine1, engine2
          else
            engine2, engine1
        engine1.UciNewGame()
        engine1.Position fenPos
        let status = run engine1 tourny board (pos.Id.ToString(), pos.FEN)
        results.Add status
        do! Async.Sleep(200)
  return results
}

let tryGetMoveWithQAndTop (move:string) (engine: ChessEngine) (pos:string)  =
      let replayBoard = Board()
      replayBoard.PlayCommands pos
      let getMoveStats () =
          let list = ResizeArray<NNValues>()
          engine.Position pos
          engine.GoNodes 1
          let mutable cont = true
          while cont do
            let line = engine.ReadLine()
            if String.IsNullOrEmpty line then
              () //ignore
            elif line.StartsWith "bestmove" then
              cont <- false
            elif line.StartsWith "info string" && line.Contains "N:" then
              let nnMsg = EngineProtocol.Regex.getInfoStringData engine.Name line
              list.Add(nnMsg)
              let moreItems = if line.StartsWith "info string node" then false else true
              let mutable contNN = moreItems
              while contNN do
                  let newline = engine.ReadLine()
                  if newline.StartsWith "info string node" then
                      contNN <- false
                  else
                      let msg = EngineProtocol.Regex.getInfoStringData engine.Name newline
                      list.Add msg
          list

      let getHighestQvalueForMove newPos =
          engine.Position newPos
          engine.GoNodes 1
          let mutable res = None
          let mutable cont = true
          while cont do
            let line = engine.ReadLine()
            if String.IsNullOrEmpty line then
              () //ignore
            elif line.StartsWith "bestmove" then
              cont <- false
            elif line.StartsWith "info string node" && line.Contains "N:" then
              res <- EngineProtocol.Regex.getInfoStringData engine.Name line |> Some
          res

      let list = getMoveStats ()
      let qList = ResizeArray<NNValues>()
      let loop () =
          for item in list do
              replayBoard.ResetBoardState()
              replayBoard.PlayCommands pos
              let newMove = item.LANMove
              replayBoard.PlayUciMove newMove
              let isMate = replayBoard.IsMate()
              if isMate then
                  item.Q <- if replayBoard.Position.STM = 0uy then -1.0 else 1.0
                  item.LANMove <- move
                  qList.Add item
              else
                  let newPos = replayBoard.PositionWithMoves()
                  match getHighestQvalueForMove newPos with
                  |Some nn ->
                      nn.Q <- nn.Q * -1.0
                      if item.LANMove = move then
                          nn.LANMove <- move
                      qList.Add nn
                  |None -> printfn "Could not find Q value for move %s" move
      loop()
      let test = qList |> Seq.tryFind(fun e -> e.LANMove = move)
      if test.IsNone then
          replayBoard.ResetBoardState()
          replayBoard.PlayCommands pos
          makeShortSan list &replayBoard
          loop()

      match list |> Seq.tryFind (fun x -> x.LANMove = move) with
      |Some movePolicy ->
          let topPolicy =
              match list |> Seq.sortByDescending(fun x -> x.P)|> Seq.tryHead with
              |Some top -> top
              |None -> failwith "Could not find top policy"
          let policyRanked =
              list
              |> Seq.sortByDescending(fun x -> x.P)
              |> Seq.toArray
              |> Array.findIndex(fun x -> x.LANMove = move)
              |> (+) 1
          let qrankForMovePlayed =
              qList
              |> Seq.sortByDescending(fun x -> x.Q)
              |> Seq.toArray
              |> Array.findIndex(fun x -> x.LANMove = move)
              |> (+) 1
          Some (qrankForMovePlayed,policyRanked, movePolicy, topPolicy)
      |None -> None


let tryGetMovePolicyAndTop (move:string) (engine: ChessEngine) (pos:string)  =
      let mutable cont = true
      engine.Position pos
      engine.GoNodes 1
      let list = ResizeArray<NNValues>()
      while cont do
        let line = engine.ReadLine()
        if String.IsNullOrEmpty line then
          () //ignore
        elif line.StartsWith "bestmove" then
          cont <- false
        elif line.StartsWith "info string" && line.Contains "N:" then
          let nnMsg = EngineProtocol.Regex.getInfoStringData engine.Name line
          if list.Count > 0 then
              list.Clear()
          list.Add(nnMsg)
          let moreItems = if line.StartsWith "info string node" then false else true
          let mutable contNN = moreItems
          while contNN do
              let newline = engine.ReadLine()
              if newline.StartsWith "info string node" then
                  contNN <- false
              else
                  let msg = EngineProtocol.Regex.getInfoStringData engine.Name newline
                  list.Add msg
      match list |> Seq.tryFind (fun x -> x.LANMove = move) with
      |Some movePolicy ->
          let topPolicy =
              match list |> Seq.sortByDescending(fun x -> x.P)|> Seq.tryHead with
              |Some top -> top
              |None -> failwith "Could not find top policy"
          let rankForMovePlayed =
              list
              |> Seq.sortByDescending(fun x -> x.P)
              |> Seq.toArray
              |> Array.findIndex(fun x -> x.LANMove = move)
              |> (+) 1
          Some (-1, rankForMovePlayed, movePolicy, topPolicy)
      |None ->
          let board = Board()
          board.PlayCommands pos
          makeShortSan list &board
          match list |> Seq.tryFind (fun x -> x.LANMove = move) with
          |Some movePolicy ->
              let topPolicy =
                  match list |> Seq.sortByDescending(fun x -> x.P)|> Seq.tryHead with
                  |Some top -> top
                  |None -> failwith "Could not find top policy"
              let rankForMovePlayed =
                  list
                  |> Seq.sortByDescending(fun x -> x.P)
                  |> Seq.toArray
                  |> Array.findIndex(fun x -> x.LANMove = move)
                  |> (+) 1
              Some (-1, rankForMovePlayed, movePolicy, topPolicy)
          |_ -> None

let tryGetMoveQAndTopForPosSequence (engine: ChessEngine) (board: Board) (player:string) (qMin:float) (qMax:float)   =
      let policies = ResizeArray<PolicyRankInfo>()
      let startFen = board.StartPosition
      let playoutBoard = Board()
      let checkWhite = player.ToLower() = "w"
      let checkBlack = player.ToLower() = "b"
      let checkAll = player.ToLower() = "all"
      playoutBoard.LoadFen startFen
      playoutBoard.StartPosition <- startFen
      for move in board.MovesAndFenPlayed do
          let pos = playoutBoard.PositionWithMoves()
          let isWhite = playoutBoard.Position.STM = 0uy
          let checkMove =
              if checkAll then
                  true
              elif checkWhite then
                  isWhite
              elif checkBlack then
                  not isWhite
              else
                  false
          if checkMove && move.Move.Comments.ToLower().Contains "book" |> not then
              match tryGetMoveWithQAndTop move.Move.LongSan engine pos with
              |Some (qRank, pRank, move, topMove) ->
                  let topQEval = abs topMove.Q
                  if topQEval <= qMax  && topQEval >= qMin then
                      makeShortSan [move;topMove] &playoutBoard
                      let policy = PolicyRankInfo.Create(qRank, pRank, move, topMove, isWhite)
                      policies.Add (policy)
              |None -> printfn $"Could not find policy for move {move.Move.LongSan}({move.ShortSan}) in position {pos}"
          playoutBoard.PlayUciMove move.Move.LongSan
      engine.Network, policies

let tryGetMovePolicyAndTopForPosSequence (engine: ChessEngine) (board: Board) (player:string) (qMin:float) (qMax:float)   =
      let policies = ResizeArray<PolicyRankInfo>()
      let startFen = board.StartPosition
      let playoutBoard = Board()
      let checkWhite = player.ToLower() = "w"
      let checkBlack = player.ToLower() = "b"
      let checkAll = player.ToLower() = "all"
      playoutBoard.LoadFen startFen
      playoutBoard.StartPosition <- startFen
      for move in board.MovesAndFenPlayed do
          let pos = playoutBoard.PositionWithMoves()
          let isWhite = playoutBoard.Position.STM = 0uy
          let checkMove =
              if checkAll then
                  true
              elif checkWhite then
                  isWhite
              elif checkBlack then
                  not isWhite
              else
                  false
          if checkMove && move.Move.Comments.ToLower().Contains "book" |> not then
              match tryGetMovePolicyAndTop move.Move.LongSan engine pos with
              |Some (qRank, pRank, move, topMove) ->
                  let topQEval = abs topMove.Q
                  if topQEval <= qMax  && topQEval >= qMin then
                      makeShortSan [move;topMove] &playoutBoard
                      let policy = PolicyRankInfo.Create(qRank, pRank, move, topMove, isWhite)
                      policies.Add (policy)
              |None -> printfn $"Could not find policy for move {move.Move.LongSan}({move.ShortSan}) in position {pos}"
          playoutBoard.PlayUciMove move.Move.LongSan
      engine.Network, policies
