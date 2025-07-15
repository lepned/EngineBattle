module ChessLibrary.Analysis

open System.IO
open System.Threading
open System
open System.Diagnostics
open Microsoft.Extensions.Logging
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open System.Text.Json
open Engine
open Parser
open TypesDef.Position
open TypesDef.Tournament
open TypesDef.CoreTypes
open TypesDef.Engine
open TypesDef.EPD
open TypesDef.Puzzle
open Chess
open Utilities

module Helper =  
  
  type Binary = |Cuda |ONNX |CPU

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
        match Utilities.Regex.getEssentialData line isWhite with
        |Some (d, eval, nodes, nps, pvLine, tbHits, wdl, sd, mPv ) ->                 
          let info = 
            { 
              PlayerName = engine.Name
              Eval = eval
              Depth = d
              SD = sd
              Nodes = nodes
              NPS = float nps
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
  
  
  let playEPDEntryPositions (logger:ILogger) (tourny:Tournament) (positions:EPDEntry seq) (cts: CancellationTokenSource) = async {    
    logger.LogInformation($"Fen positions analysis about to start")
    let board = Board()
    board.LoadFen Chess.startPos
    let gamesAlreadyPlayed = 
      let fileInfo = FileInfo tourny.PgnOutPath
      if fileInfo.Exists then
        PGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
      else
        [||]  
    
    tourny.CurrentGameNr <- gamesAlreadyPlayed.Length    
    let sb = StringBuilder()
    let mutable engine1 = tourny.EngineSetup.Engines[0] |> EngineHelper.createEngine
    let mutable engine2 = tourny.EngineSetup.Engines[1] |> EngineHelper.createEngine

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

module Manager =  
  
  type SimpleEngineAnalyzer (engineConfig, board, logger, callback: Action<EngineUpdate>, writeToConsole) =
      let SearchDict = new System.Collections.Generic.Dictionary<string,int>()
      let board : Chess.Board = board
      let moveBoard = Chess.Board()
      let logger : ILogger = logger
      let sendAnalysisResponse (update: EngineUpdate) =
        callback.Invoke update

      let engine = EngineHelper.createAltEngine (sendAnalysisResponse, engineConfig, writeToConsole)
      
      member x.Engine = engine
      
      member x.Stop() = 
        engine.SendUCICommand Stop

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
        let moves = board.GetMoveHistory()
        board.PrintPosition moves
        if board.AnyLegalMove() |> not then
          let fen = board.FEN()
          logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
        else
          let command = board.PositionWithMovesIndexed()
          printfn "Search command: %s" command
          engine.SendUCICommand (PositionWithMoves command)
          engine.SendUCICommand (GoInfinite)

      member x.SearchNodes (nodes, keepNodes : bool) = 
        engine.SendUCICommand Stop
        let commands = board.PositionWithMovesIndexed()
        moveBoard.ResetBoardState()
        moveBoard.PlayCommands commands
        let moves = moveBoard.GetMoveHistory()
        moveBoard.PrintPosition moves        
        if moveBoard.AnyLegalMove() |> not then
          let fen = moveBoard.FEN()
          logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
        else
          engine.SendUCICommand UciNewGame
          if not keepNodes then
            SearchDict.Clear()
          //let nodeLimit = nodes + getPrevNodes fen
          //SearchDict[fen] <- (nodeLimit)
          printfn "Search command: %s" commands
          engine.SendUCICommand (PositionWithMoves commands)
          engine.SendUCICommand (GoNodes nodes)
    
      member x.SearchNodesWithCommand (nodes, commands:string, keepNodes : bool) =       
        engine.SendUCICommand Stop
        moveBoard.ResetBoardState()
        moveBoard.PlayCommands commands
        let moves = moveBoard.GetMoveHistory()
        moveBoard.PrintPosition moves        
        if moveBoard.AnyLegalMove() |> not then
          logger.LogInformation ("In searchNodesWithCommands - no legal moves with command: " + commands)
        else
          engine.SendUCICommand UciNewGame
          if not keepNodes then
            SearchDict.Clear()
          printfn "Search command: %s" commands
          engine.SendUCICommand (PositionWithMoves commands)
          engine.SendUCICommand (GoNodes nodes)
      
      member x.DumpStats command = engine.SendUCICommand (RawCommand command)

      member x.Play (goCommand: string) =
        engine.SendUCICommand Stop    
        if board.AnyLegalMove() |> not then
          let fen = board.FEN()
          logger.LogInformation ("In searchNodes - no legal moves with FEN: " + fen)
        else                   
          let command = board.PositionWithMoves()
          printfn "Search command: %s" command
          engine.SendUCICommand (PositionWithMoves command)          
          engine.SendUCICommand (RawCommand goCommand)

module PuzzleDataUtils =
  
  let chess = Chess.Board()
  
  let parseRatingGroups (s:string) maxRating =
    let gs = s.Split(',') |> Array.filter (not << System.String.IsNullOrWhiteSpace)
    if gs.Length = 0 then
      if maxRating = 0 then failwith "No rating groups or max rating specified"
      else [| maxRating |]
    else gs |> Array.map int

  let parseThemes (s:string) =
    s.Split(',') |> Array.map (fun t -> t.Trim())

  let parseNodes (s:string) =
    if System.String.IsNullOrWhiteSpace s then [|0|]
    else
      s.Split(',')
      |> Array.map int
      //|> Array.filter ((<>) 1)

  let getPVMoves (pv: string) =
    let pvMoves = ResizeArray<string>()
    let arr = pv.Split ' '
    for item in arr do
      if Seq.exists Char.IsDigit item then
        pvMoves.Add(item)
    pvMoves

  let getAllFens (fen: string) (moves: string seq) =
    chess.ResetBoardState()
    let fens = ResizeArray<string>()
    fens.Add(fen)
    chess.LoadFen(fen)
    for move in moves do
      chess.PlayLongSanMove(move)
      fens.Add(chess.FEN())
    fens
  
  let getUpdatedRecord (record: CsvPuzzleData) =
      // 1) parse SAN moves once
      let moves = getPVMoves record.Moves

      // 2) reset & seed the engine
      chess.ResetBoardState()
      chess.LoadFen record.Fen

      // 3) pre‐allocate (optional) for a little extra speed
      let fens = ResizeArray<string>(moves.Count + 1)
      let cmds = ResizeArray<Position>(moves.Count / 2)

      // 4) starting position
      fens.Add record.Fen

      // 5) accumulate a SAN‐string for the “moves so far”
      let mutable movesSoFar = ""

      for idx in 0 .. moves.Count - 1 do
        let mv = moves.[idx]

        // 5a) if this is an odd‐index move, emit a Puzzle.Position
        if idx % 2 = 1 then
          let commandText = $"position fen {record.Fen} moves {movesSoFar}"
          cmds.Add
            { Command     = commandText
              CorrectMove = mv
              MovePlayed  = "" }

        // 5b) play the move and record its FEN
        chess.PlayLongSanMove mv
        fens.Add (chess.FEN())

        // 5c) update the SAN‐string for the next iteration
        movesSoFar <-
          if idx = 0 then mv
          else $"{movesSoFar} {mv}"

      // 6) stitch back into your record
      { record with
          Fens     = List.ofSeq fens
          Commands = List.ofSeq cmds }
  
  let sortPuzzleData (theme:string) ratingGroup input = 
        let byTheme =
            if String.IsNullOrWhiteSpace theme then 
                input.puzzleData
            else
                input.puzzleData
                |> Array.filter (fun e -> e.Themes.IndexOf(theme, StringComparison.OrdinalIgnoreCase) >= 0 )
        // filter then sort only the needed subset
        byTheme
        |> Array.filter (fun e -> e.Rating <= ratingGroup)
        |> Array.sortByDescending (fun e -> e.Rating)
        |> Array.truncate input.sampleSize
        //|> Seq.toArray
        |> Array.map getUpdatedRecord


module PuzzleEngineAnalysis =  
  
  let bestMoveByEvalAsync (nodes:int) (engine: ChessEngine) (fen: string) = async {
     let cmd = sprintf "position fen %s" fen
     engine.UciNewGame()
     engine.Position cmd
     engine.GoNodes nodes
     
     let mutable cont = true
     let mutable infoDepth = ""
     let mutable move = ""
     
     while cont do
       let! line = engine.ReadLineAsync() |> Async.AwaitTask
       if line.StartsWith "bestmove" then
         move <- line
         cont <- false
       elif line.StartsWith "info depth" then
         infoDepth <- line
     
     let res =
       match Regex.parseEvalRegex infoDepth with
       |CP cp -> cp
       |Mate mate -> float mate * 1000.
       |_-> failwith "error parsing eval"
     
     return (res, move)
   }
   
  let bestMoveByEval (nodes:int) (engine: ChessEngine) (fen: string) =    
    let cmd = sprintf "position fen %s" fen
    engine.UciNewGame()
    engine.Position cmd
    engine.GoNodes nodes      
    let mutable cont = true
    let mutable infoDepth = ""
    let mutable move = ""
    while cont do
      let line = engine.ReadLine()
      //printfn "%s" line
      if line.StartsWith "bestmove" then          
        move <- line
        cont <- false    
      elif line.StartsWith "info depth" then
        infoDepth <- line
    let res =
      match Regex.parseEvalRegex infoDepth with
      |CP cp -> cp
      |Mate mate -> float mate * 1000.
      |_-> failwith "error parsing eval" 
    res, move    

  //for Ceres TB run
  let bestQ (nodes:int) (engine: ChessEngine) (pos: EPDEntry) (board:Board inref)  =     
    let qList = ResizeArray<float*string>()
    let fen = pos.FEN
    board.LoadFen fen
    let legalMoves = board.GetAllLegalMoves()    
    for (lSan,_) in legalMoves do     
      let cmd = sprintf "position fen %s moves %s" fen lSan
      engine.Position cmd
      engine.GoNodes nodes
      
      let mutable cont = true
      let mutable infoString = ""
      while cont do
        let line = engine.ReadLine()
        //printfn "%s" line
        if line.StartsWith "bestmove" then          
          cont <- false          
        elif line.StartsWith "info string node" then
          infoString <- line
      
      let res = Regex.floatParser infoString Regex.v    
      qList.Add (res,lSan)
    let m = qList |> Seq.minBy fst
    m

  let getPuzzlePolicyEngine config = 
    let engine = EngineHelper.createEngine(config)
    engine.StartProcess()    
    engine

  let getPuzzleValueEngine config =   
    let isLc0 = config.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
    let isCeres = config.Path.Contains("ceres", StringComparison.OrdinalIgnoreCase)
    
    try
        let engine = EngineHelper.createEngineWithoutValidation(config)        
        engine.StartProcess()
        let options = engine.GetDefaultOptions()
        if isLc0 then
            engine.StopProcess()                
            if options.ContainsKey "ValueOnly" then                
                let dict = Dictionary<string, obj>(config.Options)
                //for some unknow reason we need to remove the backend options in some older lc0 version for valuehead to work
                for item in dict do
                  if item.Key.Contains "Backend" then
                    dict.Remove item.Key |> ignore
                //check if minibatchsize is already set    
                if not (dict.ContainsKey "MinibatchSize") then
                    dict.Add("MinibatchSize", 256)
                if not (dict.ContainsKey "ValueOnly") then
                    dict.Add("ValueOnly", true)
                let config = if isLc0 then {config with Options = dict} else config
                let engine = EngineHelper.createEngineWithoutValidation(config)
                engine.StartProcess()
                Some engine                  
            else            
                let redMsg = sprintf "\nValueOnly option is not available for %s with args: %s, will try valuehead argument next." config.Name config.Args
                let isShowHiddenArgMissing = redMsg.Contains "--show-hidden" |> not
                if isShowHiddenArgMissing then
                    let redMsg = redMsg + " Please add --show-hidden argument to engine config."
                    LowLevelUtilities.ConsoleUtils.redConsole redMsg
                else 
                  LowLevelUtilities.ConsoleUtils.yellowConsole redMsg
                //for Lc0 rewrite
                let config = if isLc0 then {config with Args = "valuehead"} else config
                let engine = EngineHelper.createEngineWithoutValidation(config)
                engine.StartProcess()
                Some engine
        elif isCeres then
            Some engine
        else
            None
    with
        | ex ->
            let redMsg = sprintf "An error occurred while configuring value head engine for %s: \n\t%s\n" config.Name ex.Message
            LowLevelUtilities.ConsoleUtils.redConsole redMsg
            None //raise ex
    
  let bestPolicyMoveWithPolicy (bm:string) (nodes:int) (engine: ChessEngine) (pos:string)  =  
    let mutable cont = true
    let mutable infoString = ""
    engine.Position pos
    engine.GoNodes nodes
    let list = ResizeArray<NNValues>()
    while cont do 
      let line = engine.ReadLine()
      if line.StartsWith "bestmove" then          
        cont <- false
        infoString <- line    
      elif line.StartsWith "info string" && line.Contains "N:" then
        let nnMsg = Utilities.Regex.getInfoStringData engine.Name line
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
                let msg = Utilities.Regex.getInfoStringData engine.Name newline
                list.Add msg
    //example output is: "bestmove e2e4 ponder e7e5"
    let move = infoString.Split().[1]    
    match list |> Seq.tryFind (fun x -> x.LANMove = bm) with
    |Some nnValue ->
        match list |> Seq.tryFind (fun x -> x.LANMove = move) with
        |Some nnBestValue -> 
            if nnValue.LANMove = nnBestValue.LANMove then
                move, [nnValue]
            else
                //printfn "NN value %s != %s" nnValue.LANMove nnBestValue.LANMove
                move, [nnValue; nnBestValue]
        |None -> move, [nnValue]
        //move, Some nnValue
    |None -> move, []

  let bestPolicyMove (nodes:int) (engine: ChessEngine) (pos:string)  =  
    let mutable cont = true
    let mutable infoString = ""
    engine.Position pos
    engine.GoNodes nodes
    let list = ResizeArray<NNValues>()
    while cont do 
      let line = engine.ReadLine()
      if line.StartsWith "bestmove" then          
        cont <- false
        infoString <- line    
      elif line.StartsWith "info string" && line.Contains "N:" then
        let nnMsg = Utilities.Regex.getInfoStringData engine.Name line
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
                let msg = Utilities.Regex.getInfoStringData engine.Name newline
                list.Add msg
    //example output is: "bestmove e2e4 ponder e7e5"
    let move = infoString.Split().[1]    
    match list |> Seq.tryFind (fun x -> x.LANMove = move) with
    |Some nnValue -> move, Some nnValue
    |None -> move, None
  
  let bestMoveWithTime (timeInMs:int) (engine: ChessEngine) (pos:string) =  
    let mutable cont = true
    let mutable infoString = ""
    engine.Position pos
    engine.Go timeInMs
    let list = ResizeArray<NNValues>()
    while cont do 
      let line = engine.ReadLine()
      if line.StartsWith "bestmove" then          
        cont <- false
        infoString <- line    
      elif line.StartsWith "info string" && line.Contains "N:" then
        let nnMsg = Utilities.Regex.getInfoStringData engine.Name line
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
                let msg = Utilities.Regex.getInfoStringData engine.Name newline
                list.Add msg
    //example output is: "bestmove e2e4 ponder e7e5"
    let move = infoString.Split().[1]    
    match list |> Seq.tryFind (fun x -> x.LANMove = move) with
    |Some nnValue -> move, Some nnValue
    |None -> move, None

  let bestQPuzzleValueOnly (engine:ChessEngine) (pos: Position) = 
    let mutable cont = true
    let mutable infoString = ""
    engine.Position pos.Command
    let isCeres = engine.Name.ToLower().Contains "ceres"
    if isCeres then
      engine.GoValue()
    else
      engine.GoNodes 1
    while cont do
      let line = engine.ReadLine()     
      if line.StartsWith "bestmove" then          
        cont <- false
        infoString <- line    
    //example output is: "bestmove e2e4 ponder e7e5"
    let move = infoString.Split().[1]    
    move

  let performPositionEvalTestOnEpdPositions (nodes : ResizeArray<int>) (engineList : ResizeArray<EngineConfig>) (epds:ResizeArray<EPDEntry>) (minEvalScore: string) (maxEvalScore : string) (maxEvalDiff : string) =
    try
        let minEvalScore = if String.IsNullOrWhiteSpace(minEvalScore) then None else Some minEvalScore
        let maxEvalScore = if String.IsNullOrWhiteSpace(maxEvalScore) then None else Some maxEvalScore
        let maxEvalDiff = 
            if String.IsNullOrWhiteSpace(maxEvalDiff) then 
                None 
            else 
                if nodes.Count = 1 then Some 10000 else int maxEvalDiff |> Some

        let board = Board()
        let engines = engineList |> Seq.map(fun e -> EngineHelper.createEngine e) |> Seq.toArray
        let maxConcurrencyCpu = max 1 (HardwareInfo.assessMaxCpuConcurrencyLevel engines)
        let chunkSize = min maxConcurrencyCpu (engines.Length)

        let min, max = 
            match minEvalScore, maxEvalScore with
            |Some min, Some max -> int min, int max
            |Some min, None -> int min, 1000
            |None, Some max -> 0, int max
            |None, None -> 0, 1000

        for engine in engines do    
            engine.StartProcess()
        let filtered =
            seq {
              for epd in epds do
                let fen = epd.FEN
                board.LoadFen fen
                let evals =
                    engines
                    |> Array.chunkBySize chunkSize
                    |> Array.collect(fun chunk ->
                        chunk |> Array.mapi(fun idx eng -> async {
                        let! (eval, move) = bestMoveByEvalAsync nodes[idx] eng fen
                        return (eval, move), nodes[idx], eng.Name
                        }))
                    |> Async.Parallel  // Run all async operations in parallel
                    |> Async.RunSynchronously  // Wait for all to complete
                    |> Array.map(fun ((eval, move), n, name) -> abs eval, move, n, name)
            
                let maxEval, maxMove, maxEng = evals |> Array.map(fun (eval,m,_,n) -> eval,m, n) |> Array.max
                let minEval, minMove, minEng = evals |> Array.map(fun (eval,m,_,n) -> eval, m, n) |> Array.min
                let evalDiff = maxEval - minEval    
                //make sure all evals are within the range
                let maxEvalDiff = 
                    match maxEvalDiff with
                    |Some maxEvalDiff -> maxEvalDiff
                    |None -> 10000
                let passes = evals |> Array.forall(fun (eval,_,_,_) -> eval >= min && eval <= max) && evalDiff < maxEvalDiff        
                if passes then
                  let evalAndMoveSummary = evals |> Array.map(fun (eval,m,nodes, name) -> sprintf "%s eval: %.0f after %d nodes move: %s" name eval nodes m ) |> String.concat ", "
                  let summary = evalAndMoveSummary + (if nodes.Count = 1 then "" else (sprintf " max evalDiff: %.1f" evalDiff))
                  printfn "EPD with fen %s passed:\n %s" epd.FEN summary
                  yield (epd, maxEval, maxMove, maxEng, summary)  }
        for engine in engines do
            engine.StopProcess()
        filtered
        |> Seq.sortByDescending (fun (_,eval,_,_,_) -> abs eval) 
        |> ResizeArray
    with 
    | ex -> 
        printfn "Error in performPositionEvalTestOnEpdPositions: %s" ex.Message
        ResizeArray()

  let performPositionEvalTestOnPgnGames (nodes : ResizeArray<int>) (engineList : ResizeArray<EngineConfig>) (pgns:ResizeArray<TypesDef.PGNTypes.PgnGame>) (minEvalScore: string) (maxEvalScore : string) (maxEvalDiff : string) =         
    try
        let minEvalScore = if String.IsNullOrWhiteSpace(minEvalScore) then None else Some minEvalScore
        let maxEvalScore = if String.IsNullOrWhiteSpace(maxEvalScore) then None else Some maxEvalScore
        let maxEvalDiff = 
            if String.IsNullOrWhiteSpace(maxEvalDiff) then 
                None 
            else 
                if nodes.Count = 1 then Some 10000 else int maxEvalDiff |> Some

        let min, max = 
            match minEvalScore, maxEvalScore with
            |Some min, Some max -> int min, int max
            |Some min, None -> int min, 1000
            |None, Some max -> 0, int max
            |None, None -> 0, 1000

        let board = Board()
        let engines = engineList |> Seq.map(fun e -> EngineHelper.createEngine e) |> Seq.toArray
        
        let cores = Environment.ProcessorCount - 1
        let getThreads (engine:EngineConfig) =
            let threadValue = 
                if engine.Options.ContainsKey("Threads") then
                    let value = engine.Options["Threads"]
                    match value with
                    | :? int as intVal -> intVal
                    | :? string as strVal -> 
                        match Int32.TryParse(strVal) with
                        | true, num -> num
                        | _ -> 1
                    | :? JsonElement as je when je.ValueKind = JsonValueKind.Number ->
                        je.GetInt32()                    
                    | :? JsonElement as je when je.ValueKind = JsonValueKind.String ->
                        let str = je.GetString()
                        match System.Int32.TryParse str with
                        | (true, num) -> num
                        | _ -> 1
                    | _ -> 1
                else
                    1
            if threadValue > cores then cores else threadValue

        let sumEngineThreads = engines |> Seq.sumBy(fun e -> getThreads e.Config)
        let chunkSize = if sumEngineThreads > cores then 1 else engines.Length
        for engine in engines do    
            engine.StartProcess()
        let filtered =
            seq {
                for pgn in pgns do
                board.ResetBoardState()        
                board.LoadFen pgn.Fen
                let moves = Deviation.movesFromPgn pgn
                for move in moves do
                    board.PlaySimpleShortSan move
                let fen = board.FEN()
                //throttled parallelism of evaluation
                let evals =
                    engines
                    |> Array.chunkBySize chunkSize
                    |> Array.collect(fun chunk ->
                        chunk |> Array.mapi(fun idx eng -> async {
                        let! (eval, move) = bestMoveByEvalAsync nodes[idx] eng fen
                        return (eval, move), nodes[idx], eng.Name
                       }))
                    |> Async.Parallel  // Run all async operations in parallel
                    |> Async.RunSynchronously  // Wait for all to complete
                    |> Array.map(fun ((eval, move), n, name) -> abs eval, move, n, name)
              
                let maxEval, maxMove, maxEng = evals |> Array.map(fun (eval,m,_,n) -> eval,m, n) |> Array.max
                let minEval, minMove, minEng = evals |> Array.map(fun (eval,m,_,n) -> eval, m, n) |> Array.min               
                let evalDiff = maxEval - minEval    
                //make sure all evals are within the range
                let maxEvalDiff = 
                    match maxEvalDiff with
                    |Some maxEvalDiff -> maxEvalDiff
                    |None -> 10000
                let passes = evals |> Array.forall(fun (eval,_,_,_) -> eval >= min && eval <= max) && evalDiff < maxEvalDiff              
                if passes then
                    let evalAndMoveSummary = 
                        evals 
                        |> Array.map(fun (eval,m,nodes, name) -> sprintf "%s eval: %.0f after %d nodes move: %s" name eval nodes m ) 
                        |> String.concat ", "
                    let summary = evalAndMoveSummary + (if nodes.Count = 1 then "" else (sprintf " max evalDiff: %.1f" evalDiff))
                    printfn "EPD with fen %s passed:\n %s" fen summary
                    yield (pgn, maxEval, maxMove,maxEng, summary)  }
        for engine in engines do
            engine.StopProcess()
        filtered     
        |> Seq.sortByDescending (fun (_,eval,_,_,_) -> eval) 
        |> ResizeArray
    with 
        | ex -> 
            printfn "Error in performPositionEvalTestOnPgnGames: %s" ex.Message
            ResizeArray()
  
  let performQValueTestOnTB nodes (engineConf:EngineConfig) (puzzles:ResizeArray<TablebaseEPDEntry>) = task {          
    let board = Board()
    let engine = EngineHelper.createEngine engineConf
    engine.StartProcess()
    let failedPuzzles = ResizeArray<TablebaseEPDEntry>()
    let correctPuzzles = ResizeArray<TablebaseEPDEntry>()
    for puzzle in puzzles do    
      if engine.HasExited() then 
        printfn "engine exited"
      else              
        let qValue, move = bestQ nodes engine puzzle.EPD &board
        let res = if qValue < -0.333 then -1 elif qValue > 0.33 then 1 else 0
        let correct = res = puzzle.TBAnswer
        let puzzleWithQ = {puzzle with QAnswer = qValue; Move = move }
        if correct then
          correctPuzzles.Add puzzleWithQ
        else              
          failedPuzzles.Add puzzleWithQ

    let diffElo = EloCalculator.eloDiffWDL correctPuzzles.Count failedPuzzles.Count 0
    let samplefailed = failedPuzzles |> Seq.toArray |> Array.sortBy(fun e -> e.TBAnswer) 
    let sampleCorrect = correctPuzzles |> Seq.toArray |> Array.sortByDescending(fun e -> e.TBAnswer)          
    let test =
      { 
        Name = engine.Name 
        FailedPuzzles = ResizeArray<TablebaseEPDEntry>(samplefailed) 
        CorrectPuzzles = ResizeArray<TablebaseEPDEntry>(sampleCorrect)
        TotalNumber = failedPuzzles.Count + correctPuzzles.Count
        Correct = correctPuzzles.Count
        Wrong = failedPuzzles.Count
        Rating = diffElo
      }
    return test     }


module EngineAgent =
  open PuzzleEngineAnalysis
  //Start the engine agent for value head tests (single consumer of UCI calls)
  let startValueEngineAgent (engineCfg:EngineConfig) =
      MailboxProcessor.Start(fun inbox ->
        // Spin up one engine instance
        match getPuzzleValueEngine engineCfg with
        | Some engine -> 
            //let isLc0 = engineCfg.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)        
            engine.Name <- engine.Name

            let rec loop() = async {
              let! msg = inbox.Receive()
              match msg with
              | Ok reply ->
                  reply.Reply(true)
                  return! loop()
              | BestMove (cmd, reply) ->
                  let mv = bestQPuzzleValueOnly engine cmd                
                  reply.Reply (mv,0.0)
                  return! loop()
              | BestMoveWithPolicy (cmd, correctMove, reply) ->
                  let mv = bestQPuzzleValueOnly engine cmd                 
                  reply.Reply (mv,String.Empty)
                  return! loop()

              | Quit reply ->
                  engine.Quit()
                  do! Async.Sleep 1000
                  reply.Reply()                  
                  // exit loop
              | Network reply -> 
                  reply.Reply engine.Network
                  return! loop()
            }
            loop()
        | None -> 
            let rec loop() = async {
              let! msg = inbox.Receive()
              match msg with
              | Ok reply ->
                  reply.Reply(false)
                  return! loop()              
              | _ ->
                  return! loop()
            }
            loop()
            
      )

  //Start the engine agent for policy head tests (single consumer of UCI calls)
  let startPolicyEngineAgent (engineCfg:EngineConfig) nodes =
      MailboxProcessor.Start(fun inbox ->
        // Spin up one engine instance
        let engine = getPuzzlePolicyEngine engineCfg
        engine.Name <- engine.Name

        let rec loop() = async {
          let! msg = inbox.Receive()
          match msg with
          | Ok reply ->
               reply.Reply(true)
               return! loop()
          | BestMove (cmd, reply) ->
              let mv, nnValue = bestPolicyMove nodes engine cmd.Command
              reply.Reply (mv,(if nnValue.IsSome then 0.0 else 0.0))
              return! loop()          
          | BestMoveWithPolicy (cmd, correctMove, reply) ->
              let mv, nnValue = bestPolicyMoveWithPolicy correctMove nodes engine cmd.Command              
              if nnValue.Length = 0 then
                reply.Reply (mv, String.Empty)
              elif nnValue.Length = 1 then
                reply.Reply (mv, sprintf "%.2f" nnValue.Head.P)
              else
                let nnValueString = nnValue |> List.map (fun v -> sprintf "%.2f" v.P) |> String.concat ", "
                reply.Reply (mv, nnValueString)
              return! loop()

          | Quit reply ->
              engine.StopProcess()             
              reply.Reply()
              // exit loop
          | Network reply -> 
              reply.Reply engine.Network
              return! loop()
        }
        loop()
      )


  //Per‐puzzle async workflow
  let runPuzzleViaAgent (agent:MailboxProcessor<EngineMsg>) (valueHead : bool) (puzzle:CsvPuzzleData)  = async {
      //Reset engine state
      //do! agent.PostAndAsyncReply(fun ch -> NewGame ch) |> Async.Ignore

      // 3b) Fresh board per puzzle
      let board = Board()
      let mutable correct    = true
      let mutable movePlayed = ""
      let mutable failedMove = ""
      let mutable policy = String.Empty

      for cmd in puzzle.Commands do
        if correct then
          // Ask engine for its candidate move
          let! (mv,p) = agent.PostAndAsyncReply(fun ch -> BestMoveWithPolicy(cmd, cmd.CorrectMove, ch))
          movePlayed <- mv

          // Compare to correct move, with mate‐in‐one fallback
          let mutable solved = cmd.CorrectMove = mv
          if not solved then
            failedMove <- cmd.CorrectMove
            policy <- p
            board.PlayCommands cmd.Command
            board.PlayLongSanMove mv
            solved <- not (board.AnyLegalMove())
          correct <- solved
      
      let cmds = puzzle.Commands |> Seq.map (fun el -> if el.CorrectMove = failedMove then {el with MovePlayed = movePlayed} else el)
      let puzzleWithMove = {puzzle with Commands = cmds; Index = 0 }
      
      //Return minimal puzzle result
      return      
        { PuzzleData = puzzleWithMove
          WasCorrect = correct
          MovePlayed = movePlayed
          FailedMove = failedMove
          ValueHead = valueHead
          Policy = policy
        }     
    }

  /// A little wrapper around MailboxProcessor<'Msg> that
  /// blocks PostAndAsyncReply once you’ve got 250 in flight.
  type BoundedAgent<'Msg>(inner: MailboxProcessor<'Msg>, capacity: int) =
      let sem = new SemaphoreSlim(capacity, capacity)
      
      /// Expose the underlying MailboxProcessor
      member _.Inner = inner

      /// Exactly the same signature as MailboxProcessor.PostAndAsyncReply
      member _.PostAndAsyncReply(build: AsyncReplyChannel<'T> -> 'Msg) : Async<'T> =
        async {
          // wait until one “slot” is free
          do! Async.AwaitTask (sem.WaitAsync())
          try
            // send the message and await reply
            let! res = inner.PostAndAsyncReply(build)
            return res
          finally
            // release the slot once the reply comes back
            sem.Release() |> ignore
        }


  //Main test runner
  let performValueNetworkTest
    (nodes:int)
    (engineCfg:EngineConfig)
    (puzzles:CsvPuzzleData[])
    (theme:string)
    (concurrency: int)  =

      let concurrencyLevel = max 1 concurrency
      
      let agents =
          [| for i in 1 .. concurrencyLevel do
              let mb = startValueEngineAgent engineCfg
              yield BoundedAgent<EngineMsg>(mb,250)|]
      
      let ok = agents |> Array.map (fun a -> a.PostAndAsyncReply(fun ch -> Ok ch)) |> Async.Parallel |> Async.RunSynchronously
      if ok |> Array.exists (fun x -> not x) then
            Score.empty
            //failwith "Failed to start engine agent. Engine could not be created."
      else
       // Partition the puzzles among agents
          let puzzleChunks =
              puzzles
              |> Array.indexed
              |> Array.groupBy (fun (idx, _) -> idx % concurrencyLevel)
              |> Array.map (fun (_, items) -> items |> Array.map snd)
    
        // Map each agent to its workload and collect async work items
          let agentJobs =
              Array.zip agents puzzleChunks
              |> Array.map (fun (agent, agentPuzzles) ->
                  agentPuzzles |> Array.map (runPuzzleViaAgent agent.Inner true))
    
        // Execute all jobs in parallel and collect results
          let results =
              agentJobs
              |> Array.map Async.Parallel
              |> Async.Parallel 
              |> Async.RunSynchronously
              |> Array.collect id

          let firstAgent = agents.[0]
          let networkName = firstAgent.PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
          
          //Shutdown the engines    
          let _ =
              agents
              |> Array.map (fun agent -> agent.PostAndAsyncReply(fun ch -> Quit ch))
              |> Async.Parallel
              |> Async.RunSynchronously

          //Aggregate results
          let correct = results |> Array.filter (fun r -> r.WasCorrect)
          let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
          let w, d, l = correct.Length, 0, failed.Length

          let diffElo = EloCalculator.eloDiffWDL w d l
          let error   = EloCalculator.calculateEloError w d l
          let avg     =
            if results.Length = 0 then 0.0
            else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
          let perf    = avg + diffElo
          let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
          printfn "\nValue network rating performance: %.0f (avg %.0f + Δ%.0f) Theme: %s" perf avg diffElo theme
          let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}
      
          //Build and return the final score record
          let score = 
              {
                Engine = engineCfg.Name
                NeuralNet = networkName
                TotalNumber = results.Length
                Correct = w
                Wrong = l
                RatingAvg = avg
                Filter = if theme.Trim() = "" then "none" else theme.Trim()
                PlayerRecord = pRating
                FailedPuzzles = ResizeArray (failed  |> Array.map (fun r -> r.PuzzleData, r.Policy))
                CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
                Nodes = nodes
                WithHistory = false
                Type = "Value"
              } 
    
          score

  let performPolicyOrSearchTest
    (nodes:int)
    (engineCfg:EngineConfig)
    (puzzles:CsvPuzzleData[])
    (theme:string)    
    (concurrency : int)  =   

      let concurrency = max 1 concurrency
      let agents =
          [| for i in 1 .. concurrency do
              let mb = startPolicyEngineAgent engineCfg nodes
              yield BoundedAgent<EngineMsg>(mb,250)
          |]
      
       // Partition the puzzles among agents      
      let puzzleChunks =
          puzzles
          |> Array.indexed
          |> Array.groupBy (fun (idx, _) -> idx % concurrency)
          |> Array.map (fun (_, items) -> items |> Array.map snd)
    
    // Map each agent to its workload and collect async work items
      let agentJobs =
          Array.zip agents puzzleChunks
          |> Array.map (fun (agent, agentPuzzles) ->
              agentPuzzles |> Array.map (runPuzzleViaAgent agent.Inner false))
    
    // Execute all jobs in parallel and collect results
      let results =
          agentJobs
          |> Array.map Async.Parallel
          |> Async.Parallel 
          |> Async.RunSynchronously
          |> Array.collect id

      let firstAgent = agents.[0]
      let networkName = firstAgent.PostAndAsyncReply(fun ch -> Network ch) |> Async.RunSynchronously
      
      //Shutdown the engine      
      let _ =
          agents
          |> Array.map (fun agent -> agent.PostAndAsyncReply(fun ch -> Quit ch))
          |> Async.Parallel
          |> Async.RunSynchronously

      //Aggregate results
      let correct = results |> Array.filter (fun r -> r.WasCorrect)
      let failed  = results |> Array.filter (fun r -> not r.WasCorrect)
      let w, d, l = correct.Length, 0, failed.Length

      let diffElo = EloCalculator.eloDiffWDL w d l
      let error   = EloCalculator.calculateEloError w d l
      let avg     =
        if results.Length = 0 then 0.0
        else results |> Array.averageBy (fun r -> r.PuzzleData.Rating)
      let perf    = avg + diffElo
      let theme = if String.IsNullOrWhiteSpace theme then "none" else theme
      if nodes > 1 then
        printfn "\nSearch rating performance: %.0f (avg %.0f + Δ%.0f) Nodes %d Theme: %s" perf avg diffElo nodes theme
      else
        printfn "\nPolicy network rating performance: %.0f (avg %.0f + Δ%.0f) Nodes %d Theme: %s" perf avg diffElo nodes theme
      let pRating = {Rating = perf; Deviation = error; Volatility = 0.0}
      
      // Build and return the final score record
      let score = 
          {
            Engine = engineCfg.Name
            NeuralNet = networkName
            TotalNumber = results.Length
            Correct = w
            Wrong = l
            RatingAvg = avg
            Filter = if theme.Trim() = "" then "none" else theme.Trim()
            PlayerRecord = pRating
            FailedPuzzles = ResizeArray (failed  |> Array.map (fun r -> r.PuzzleData, r.Policy)) 
            CorrectPuzzles = ResizeArray (correct |> Array.map (fun r -> r.PuzzleData))
            Nodes = nodes
            WithHistory = false
            Type = if nodes > 1 then "Search" else "Policy"
          } 
    
      score


  type SubTest =
      | Value
      | Policy
      | Search of node:int

  let runTest
        (input     : PuzzleInput)
        (callback  : Action<Lichess>)
        (toRun     : SubTest list)
      : ResizeArray<Score> =

      // common setup
      let enginesOnly = input.engines |> Seq.map fst
      Utilities.Validation.validateAllEnginesAndSomeSettings enginesOnly
      let ratings = PuzzleDataUtils.parseRatingGroups input.ratingGroups input.maxRating
      let themes  = PuzzleDataUtils.parseThemes input.puzzleFilter
      //let nodes   = PuzzleInput.parseNodes  input.nodes

      let sendU = callback.Invoke
      let results = ResizeArray<Score>()
      let start   = Stopwatch.GetTimestamp()
      //check if toRun contains Search
      let hasSearch = toRun |> List.exists (function Search _ -> true | _ -> false)

      for engine, nodes in input.engines do
        let isCeresOrLc0 =
          engine.Path.ToLower().Contains("lc0")
          || engine.Name.ToLower().Contains("ceres")

        for theme in themes do
          let themeLabel = if System.String.IsNullOrWhiteSpace theme then "none" else theme

          for rating in ratings do
            // load & log
            let puzzles = PuzzleDataUtils.sortPuzzleData theme rating input
            if puzzles.Length = 0 then 
                LowLevelUtilities.ConsoleUtils.yellowConsole $"\nSkipping tests: No puzzles found matching theme '{theme}' and rating {rating}"                
            else
              let avg = puzzles |> Array.averageBy (fun p -> p.Rating)
              printfn "\nRating group %d (avg %.0f), theme “%s”" rating avg themeLabel

              if hasSearch && nodes > 0 then
                let score = performPolicyOrSearchTest nodes engine puzzles theme input.NumberOfPuzzlesInParallel
                sendU (PuzzleResult score)
                results.Add score
              // run each requested sub-test
              for test in toRun do
                match test with
                | Value when isCeresOrLc0 ->
                    let score = performValueNetworkTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel
                    sendU (PuzzleResult score)
                    results.Add score

                | Policy ->
                    let score = performPolicyOrSearchTest 1 engine puzzles theme input.NumberOfPuzzlesInParallel
                    sendU (PuzzleResult score)
                    results.Add score

                | Search node when node > 1 ->
                    let score = performPolicyOrSearchTest node engine puzzles theme input.NumberOfPuzzlesInParallel
                    sendU (PuzzleResult score)
                    results.Add score
              
                | Search node when node <= 1 ->
                    printfn "  → Skipping Search test with %d nodes (requires node count higher than 1)" node

                | _ -> ()  // skip if node = empty/zero or Value if not lc0/ceres

              // partial timing
              let elapsed = Stopwatch.GetElapsedTime start
              let total   = results |> Seq.sumBy (fun s -> s.TotalNumber)
              printfn "  → %d puzzles in %.0f s (theme: %s)" total elapsed.TotalSeconds themeLabel

      // wrap up
      sendU (Done "Finished!")
      let totalPretty = Stopwatch.GetElapsedTime start |> Time.prettyPrintTimeSpan
      LowLevelUtilities.ConsoleUtils.greenConsole $"\nTotal time: {totalPretty}"

      results

module PuzzleRunners =
  open PuzzleEngineAnalysis
  open EngineAgent

  /// Only value‐head (LC0/Ceres only)
  let runValueHeadTest
      (input    : PuzzleInput,
       callback : Action<Lichess>)
    =
    runTest input callback [ Value ]

  /// Only policy‐head
  let runPolicyHeadTest
      (input    : PuzzleInput,
       callback : Action<Lichess>)
    =
    runTest input callback [ Policy ]

  /// Only “search” runs at each node setting
  let runSearchTests
      (input    : PuzzleInput,
       callback : Action<Lichess>)
    =
    let searches =
      PuzzleDataUtils.parseNodes input.nodes
      //|> Array.filter (fun n -> n > 0) // filter out zero nodes
      |> Array.toList
      |> List.map Search
    runTest input callback searches

  /// Value + Policy
  let runValueAndPolicyHeadTest
      (input      : PuzzleInput,       
       callback   : Action<Lichess>)
    =
    let testTypes = [ Value; Policy ]    
    runTest input callback testTypes

  /// Value + Search
  let runValueAndSearchTest
      (input    : PuzzleInput,
       callback : Action<Lichess>)
    =
    let searches =
      PuzzleDataUtils.parseNodes input.nodes
      //|> Array.filter (fun n -> n > 0) // filter out zero nodes
      |> Array.toList
      |> List.map Search
    runTest input callback (Value :: searches)

  /// Policy + Search
  let runPolicyAndSearchTests
      (input    : PuzzleInput,
       callback : Action<Lichess>)
    =
    let searches =
      PuzzleDataUtils.parseNodes input.nodes
      //|> Array.filter (fun n -> n > 0) // filter out zero nodes
      |> Array.toList
      |> List.map Search
    runTest input callback (Policy :: searches)

  /// All three: Value, Policy, and (if requested) Search
  let runAllTests
      (input      : PuzzleInput,       
       callback   : Action<Lichess>)
    =
    let baseTests = [ Value; Policy ]
    let allTests =      
        baseTests @
        (PuzzleDataUtils.parseNodes input.nodes |> Array.toList |> List.map Search)      
    runTest input callback allTests

  let writeToFile (data:EretConfig) (scores: ERETResults seq) (sw:StreamWriter) (boardBm: Chess.Board) (boardAm: Chess.Board) =
      for item in scores do             
          let tc = if data.RunWithNodeLimit then $"Nodes: {data.Nodes}" else $"TimeMs: {data.TimeInSeconds * 1000}"
          sw.WriteLine($"\n## Failed puzzles by {item.PlayerName} - overall performance: {item.Accuracy:P0} {tc}\n")                       
          for (puzzle, movePlayed) in item.FailedPuzzles do                
              boardBm.LoadFen(puzzle.FEN)
              let bestMoveInLsan =
                match puzzle.BestMove with
                | Some bm -> 
                  boardBm.PlaySimpleShortSan(bm)
                  match boardBm.MovesAndFenPlayed |> Seq.tryLast with
                  | Some m -> m.Move.LongSan
                  | None -> ""                    
                | None -> ""
              boardAm.LoadFen(puzzle.FEN)                
              let avoidMoveInLsan =
                match puzzle.AvoidMove with
                | Some am -> 
                  boardAm.PlaySimpleShortSan(am)
                  match boardAm.MovesAndFenPlayed |> Seq.tryLast with
                  | Some m -> m.Move.LongSan
                  | None -> ""
                | None -> ""
              let msg =
                let puzzleId = match puzzle.Id with | Some id -> id | None -> "No ID"
                if puzzle.BestMove.IsSome then
                    $"{puzzle.FEN} bm {puzzle.BestMove.Value}; am {movePlayed}; id \"{puzzleId}, player {item.PlayerName}\"; other \"{bestMoveInLsan},{movePlayed}\""
                elif puzzle.AvoidMove.IsSome then
                    $"{puzzle.FEN} am {puzzle.AvoidMove.Value}; id \"{puzzleId}, player {item.PlayerName}\"; other \"{avoidMoveInLsan},{movePlayed}\"" 
                else
                    $"{puzzle.FEN} am {puzzle.AvoidMove.Value}; id \"{puzzleId}, player {item.PlayerName} - No best move or avoid move found\""
              sw.WriteLine(msg)

  let runEretTests (timeConfig: TypesDef.TimeControl.UnionType) (engineConfigs: (EngineConfig * int) seq) (data: EretConfig) (callback: Action<ERET>) =
    let sampleSize = data.SampleSize
    let epdPath = data.PuzzleFile
    let isnodes, nodes, time =
      match timeConfig with
      | TypesDef.TimeControl.UnionType.FixedTime t -> false, -1, t.ToTimeSpan().TotalMilliseconds |> int
      | TypesDef.TimeControl.UnionType.Nodes n -> true, n, 0
      |_ -> false, -1, 0
    
    let sendUpdate (update: ERET) = callback.Invoke(update) 

    if isnodes then
      //LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "\nRunning %d Eret positions with node limit: %A\n" numberOfPositions nodes)
      Start(sprintf "Running %d Eret positions with node limit: %A" sampleSize nodes) |> sendUpdate
    else
      Start(sprintf "Running %d Eret positions with time limit in ms: %A" sampleSize time) |> sendUpdate
      //LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "\nRunning %d Eret positions with time limit in ms: %A\n" numberOfPositions time)
    let puzzles = EPDExtractor.readEPDs epdPath |> Seq.truncate sampleSize |> Seq.toArray
    
    let watch = Stopwatch.StartNew()

    let runSingleEngine (engineConfig: (EngineConfig * int)) =
        let board = Board()
        let engineConfigOnly, _ = engineConfig
        let puzzlePolicyEngine = getPuzzlePolicyEngine engineConfigOnly         
        puzzlePolicyEngine.Name <- puzzlePolicyEngine.Name          
        let failedPuzzles = ResizeArray<EPDEntry * string>()
        let correctPuzzles = ResizeArray<EPDEntry>()
        let sbPuzzle = StringBuilder()
        sbPuzzle.AppendLine(sprintf "\nRun Date: %s" (DateTime.Now.ToLongDateString())) |> ignore          
        sbPuzzle.AppendLine(sprintf "\tEngine: %s" puzzlePolicyEngine.Name) |> ignore
        sbPuzzle.AppendLine(sprintf "\t\tNetwork: %s" puzzlePolicyEngine.Network) |> ignore
        sbPuzzle.AppendLine(sprintf "\t\tExecPath: %s" puzzlePolicyEngine.Path) |> ignore
        for opt in puzzlePolicyEngine.Commands do
            sbPuzzle.AppendLine(sprintf "\t\t%s" opt) |> ignore
        let mutable puzzleNr = 1
          
        for puzzle in puzzles do 
            board.ResetBoardState()              
            board.LoadFen puzzle.FEN
            let fen = board.FEN()
            let position = "position fen " + fen
            puzzleNr <- puzzleNr + 1        
            let move, nnValue = 
                if isnodes then
                    bestPolicyMove nodes puzzlePolicyEngine position 
                else            
                    bestMoveWithTime time puzzlePolicyEngine position
            board.PlayLongSanMove move
            let shortSanMove =
                match board.ShortSANMovesPlayed |> Seq.tryLast with
                | Some move -> move
                | None -> 
                    failwithf "No moves played for puzzle with FEN: %s" puzzle.FEN

            let correct =
                match puzzle.BestMove with
                | Some bm -> bm.Contains shortSanMove
                | None -> 
                    match puzzle.AvoidMove with
                    | Some am -> am.Contains shortSanMove |> not
                    | None -> false
      
            puzzlePolicyEngine.UciNewGame()
            if correct then          
                correctPuzzles.Add puzzle
            else          
                failedPuzzles.Add (puzzle,move)
            Puzzle(puzzle, correct) |> sendUpdate

        puzzlePolicyEngine.Quit()
        let accuracy = (float)correctPuzzles.Count / (float)(correctPuzzles.Count + failedPuzzles.Count)
        let desc = if isnodes then sprintf "Nodes: %d" nodes else sprintf "Time: %d ms" time
        let eretResult = { PlayerName = puzzlePolicyEngine.Name; CorrectPuzzles = correctPuzzles; FailedPuzzles = failedPuzzles; Accuracy = accuracy; Desc=desc }
        PlayerResult(eretResult) |> sendUpdate
        eretResult

    let engineConfigsOnly = engineConfigs |> Seq.map fst
    let concurrency = HardwareInfo.concurrencyLevel engineConfigsOnly (engineConfigs |> Seq.length)
    printfn "Concurrency level: %d" concurrency
    
    let resultsNew = 
        if concurrency > 0 then
            engineConfigs
            |> Seq.toArray
            |> Array.Parallel.map runSingleEngine
            |> Seq.toList
        else 
            engineConfigs
            |> Seq.map runSingleEngine
            |> Seq.toList

    watch.Stop()    
    resultsNew |> List.sortByDescending (fun eret -> eret.Accuracy) |> AllResults |> sendUpdate    

    let isnodes, nodes, time =
      match timeConfig with
      | TypesDef.TimeControl.UnionType.FixedTime t -> false, -1, t.ToTimeSpan().TotalMilliseconds |> int
      | TypesDef.TimeControl.UnionType.Nodes n -> true, n, 0
      |_ -> false, -1, 0    
    
    let eretSummary = 
        resultsNew
        |> List.sortByDescending (fun eret -> eret.Accuracy)
        |> List.map (fun eret -> 
            let correctCount = Seq.length eret.CorrectPuzzles
            let failedCount = Seq.length eret.FailedPuzzles
            let accStr = eret.Accuracy.ToString("P1")
            (eret.PlayerName, accStr, correctCount, failedCount)
        )
        
    //get the max length of the engine name for coumn width
    let maxEngineNameLength = eretSummary |> List.map (fun (engine, _, _, _) -> engine.Length) |> List.max
    let colWidths = [maxEngineNameLength + 2; 11; 10; 10]
    let eretFileName = Path.GetFileName epdPath
    let elapsed = watch.Elapsed

    let sb = StringBuilder()

    // Add timing information
    let timeMsg = if isnodes then sprintf "Nodes: %d" nodes else sprintf "Time: %d ms" time

    // Append opening markdown for Discord
    sb.AppendLine("\n```") |> ignore

    // Append ERET Test file name and details
    sb.AppendFormat("ERET Test file name: {0}, number of positions: {1}, {2}\n\n", eretFileName, sampleSize, timeMsg) |> ignore

    // Print header with proper alignment
    let header = sprintf "%-*s %-*s %-*s %-*s" colWidths.[0] "Engine" colWidths.[1] "Accuracy" colWidths.[2] "Correct" colWidths.[3] "Failed"
    sb.AppendLine(header) |> ignore
    sb.AppendLine(String.replicate (List.sum colWidths + 3) "-") |> ignore // Divider line

    // Append each row with proper alignment
    for (engine, acc, correct, failed) in eretSummary do
        sb.AppendFormat("{0,-" + colWidths.[0].ToString() + "} {1,-" + colWidths.[1].ToString() + "} {2,-" + colWidths.[2].ToString() + "} {3,-" + colWidths.[3].ToString() + "}\n", engine, acc, correct, failed) |> ignore

    sb.AppendLine(String.replicate (List.sum colWidths + 3) "-") |> ignore // Divider line
    sb.AppendLine("```") |> ignore // Closing markdown for Discord

    // Retrieve the accumulated string
    let outputString = sb.ToString()
    ResultsInConsole(outputString) |> sendUpdate
    
    let timing = sprintf "Time taken to run Eret tests: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
    ResizeArray(resultsNew)

module Experiments =
  open PuzzleRunners
  //-------------------------------------------------------------------------
  // 1. Generic Helpers
  //-------------------------------------------------------------------------

  /// Update a given parameter (e.g. "V1TEMP") in the config string.
  /// This function uses a regex pattern and returns the updated config.
  let updateParameter (config: string) (paramName: string) (newValue: float) =
      let pattern = sprintf @"(%s=)([\d\.]+)" paramName
      Regex.Replace(config, pattern, fun m -> sprintf "%s%g" m.Groups.[1].Value newValue)

  /// Extract a parameter’s value from the config string.
  let extractParameter (paramName: string) (config: string) : float option =
      let pattern = sprintf @"%s=([\d\.]+)" paramName
      let m = Regex.Match(config, pattern)
      if m.Success then Some (float m.Groups.[1].Value) else None

  //-------------------------------------------------------------------------
  // 2. Generic Parameter Optimizer
  //-------------------------------------------------------------------------

  /// Generic optimizer for one parameter.
  /// - paramName: the parameter to optimize (e.g. "V1TEMP").
  /// - config: the current configuration string.
  /// - currentValue: the current value for the parameter.
  /// - step: how much to change the parameter each iteration (can be negative).
  /// - scoreFunc: a function that takes a config string and returns a score.
  /// - boundCheck: a predicate ensuring the new value is within allowed bounds.
  let rec optimizeParameter paramName (config: string) (currentValue: float)
                            (step: float) (scoreFunc: string -> float)
                            (boundCheck: float -> bool) (bestScore: float) (latestBestValue: float) =
    let newValue = currentValue + step
    let contDescent =
      match paramName with
      | "V1TEMP" -> newValue > 1.0
      | "V2FRAC" -> newValue < 0.2
      | "V2TEMP" -> newValue > 1.0
      | _ -> failwithf "Unknown parameter: %s" paramName

    let step = 
      if contDescent then 
        let p = sign step
        0.2 * float p
      else 
        step
    
    if not (boundCheck newValue) then
        // Cannot step further: return current config, value, and score.
        config, currentValue, bestScore
    else
        let newConfig = updateParameter config paramName newValue
        let newScore = scoreFunc newConfig
        //let currentScore = scoreFunc config
        if newScore > bestScore then
            printfn "Optimized %s: %g -> %g (score: %g -> %g)" paramName currentValue newValue bestScore newScore
            // Continue optimizing recursively.
            optimizeParameter paramName newConfig newValue step scoreFunc boundCheck newScore newValue
        elif contDescent then
            printfn "Continue descending wit step size %f: %s: %g -> %g (score: %g -> %g)" step paramName currentValue newValue bestScore newScore
            // Continue optimizing recursively.
            optimizeParameter paramName newConfig newValue step scoreFunc boundCheck bestScore latestBestValue
        else
            // No further improvement; return current best.
            let config = updateParameter config paramName latestBestValue
            config, latestBestValue, bestScore


  //-------------------------------------------------------------------------
  // 3. Coordinate Descent
  //-------------------------------------------------------------------------

  /// Optimize V1TEMP (by decreasing it) and V2FRAC (by increasing it) and V2TEMP (by decreasing it)
  /// until no further improvement is achieved.
  let rec coordinateDescent (config: string) (step: float) (scoreFunc: string -> float) (bestScore:float) =
    
    // Extract current parameter values; fail if not found.
    let v1 = extractParameter "V1TEMP" config |> Option.defaultWith (fun () -> failwith "V1TEMP not found")
    let v2Frac = extractParameter "V2FRAC" config |> Option.defaultWith (fun () -> failwith "V2FRAC not found")
    let v2temp = extractParameter "V2TEMP" config |> Option.defaultWith (fun () -> failwith "V2TEMP not found")

     // Optimize each parameter in turn.
    let (newConfig, newV1, score) = optimizeParameter "V1TEMP" config v1 (-step) scoreFunc (fun x -> x > 0.5) bestScore v1
    let (newConfig, newV2Frac, score) = optimizeParameter "V2FRAC" newConfig v2Frac step scoreFunc (fun x -> x < 0.6) score v2Frac    
    let (newConfig, newV2Temp, score) = 
      if newV2Frac > 0.0 then
        optimizeParameter "V2TEMP" newConfig v2temp (-step) scoreFunc (fun x -> x > 0.5) score v2temp
      else
        (newConfig, newV2Frac, score)
    
    // If any parameter changed, run another round.
    if config <> newConfig then         
         printfn "Recursively doing a new round of optimization since previous run changed optimal configuration..."
         printfn "New config: %s" newConfig
         printfn "Old config: %s" config
         
         coordinateDescent newConfig step scoreFunc score
    else
         //todo : reduce the step size if it is high and run again
        if step > 0.05 then
          printfn "Reducing step size by half and recursively run optimization..."
          let newStep = step / 2.0
          coordinateDescent newConfig newStep scoreFunc score
        else
          newConfig


  //-------------------------------------------------------------------------
  // 4. Example Score Functions and Policy/Value Tests
  //-------------------------------------------------------------------------

  /// Policy node score function: updates the engine config, runs the test, and returns accuracy.
  let policyNodeScore (input: PuzzleInput) (callback: Action<Lichess>) (config: string)  : float =
      let engine = fst (Seq.head input.engines)      
      engine.Options.["Network"] <- box config      
      let res = runPolicyHeadTest (input, callback)
      let score = res.[0]
      float score.Correct / float score.TotalNumber

  /// Similarly, a value score function.
  let valueScore (input: PuzzleInput) (callback: Action<Lichess>) (config: string)  : float =
      let engine = fst (Seq.head input.engines) 
      engine.Options.["Network"] <- box config      
      let res = runPolicyHeadTest (input, callback)
      let score = res.[0]
      float score.Correct / float score.TotalNumber

  let optimizeValueHeadTest (input : PuzzleInput, withHistory: bool) =    
    let watch = Stopwatch.StartNew()
    let firstEngine = input.engines |> Seq.head
    let engineConfigs = ResizeArray<TypesDef.CoreTypes.EngineConfig * int>()   
    engineConfigs.Add firstEngine
    let newInput = {input with engines = engineConfigs}   
    // The initial configuration string.
    let initialConfig = "C:/Dev/Chess/Networks/CeresTrainNet/Official/C1-640-34.value3_L32_ZDeblundered_x1_d2.onnx.value3.onnx|V1TEMP=1.4;V2FRAC=0.0;V2TEMP=1.8"
    // Define the step size (e.g. 0.05)

    let step = 0.2
    let emptyCallback = fun (e:Lichess) -> ()
    let optimizer = valueScore newInput emptyCallback
    printfn "Initial configuration:\n%s" initialConfig
    printfn "Initial score: %g" (optimizer initialConfig)

    // Run coordinate descent to optimize both parameters.
    let optimizedConfig = coordinateDescent initialConfig step optimizer 0.0
    watch.Stop()
    let elapsed = watch.Elapsed    
     //how long did it take to optimize
    let timing = sprintf "Time taken to optimize: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
    printfn "%s" timing
    printfn "Initial input: Sample size: %d, Nodes: %s, Rating: %d, Ratinggroups: %A" input.sampleSize input.nodes input.maxRating input.ratingGroups 
    let msg = sprintf "\nOptimized configuration:\n%s" optimizedConfig
    msg

  let optimizePolicyHeadTest (input : PuzzleInput, withHistory: bool) =    
    //add timer for the optimization
    let watch = Stopwatch.StartNew()
    let firstEngine = input.engines |> Seq.head
    let engineConfigs = ResizeArray<TypesDef.CoreTypes.EngineConfig * int>()   
    engineConfigs.Add firstEngine
    let newInput = {input with engines = engineConfigs}    
    let initialConfig = "C:/Dev/Chess/Networks/CeresTrainNet/Official/C1-640-34.value3_L32_ZDeblundered_x1_d2.onnx.value3.onnx|V1TEMP=1.8;V2FRAC=0.0;V2TEMP=1.8"
    // Define the step size (e.g. 0.05)
    let step = 0.2
    let emptyCallback = fun (e:Lichess) -> ()
    let optimizer = policyNodeScore newInput emptyCallback
    printfn "Initial configuration:\n%s" initialConfig

    // Run coordinate descent to optimize both parameters.
    let optimizedConfig = coordinateDescent initialConfig step optimizer 0.0

    watch.Stop()
    let elapsed = watch.Elapsed
    //how long did it take to optimize
    let timing = sprintf "Time taken to optimize: %d minutes %d seconds" elapsed.Minutes elapsed.Seconds
    printfn "%s" timing
    printfn "Initial input: Sample size: %d, Nodes: %s, Rating: %d, Ratinggroups: %A" input.sampleSize input.nodes input.maxRating input.ratingGroups    
    let msg = sprintf "\nOptimized configuration:\n%s" optimizedConfig    
    msg
  

  
