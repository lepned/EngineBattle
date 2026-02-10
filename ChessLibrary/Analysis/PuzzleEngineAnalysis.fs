module ChessLibrary.PuzzleEngineAnalysis

open System
open System.Collections.Concurrent
open System.Collections.Generic
open ChessLibrary.Engine
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.EPDTypes
open ChessLibrary.PuzzleTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.EngineProtocol
open ChessLibrary.Statistics
open ChessLibrary.RuntimeUtilities

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
  let legalMoves = board.GetLegalMoves()
  for (lSan,_) in legalMoves do
    let cmd = sprintf "position fen %s moves %s" fen lSan
    engine.Position cmd
    engine.GoNodes nodes

    let mutable cont = true
    let mutable infoString = ""
    while cont do
      let line = engine.ReadLine()
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
  let ok = engine.WaitForReadyOk() // wait for readyok
  if not ok then
      failwith "Engine did not respond to isready command."
  engine

/// Cache of default UCI option names per engine binary.
/// Key: "Path|Args"; Value: set of option names reported during uci handshake.
let private defaultOptionNamesCache = ConcurrentDictionary<string, HashSet<string>>()

let private probeDefaultOptionNames (config: EngineConfig) =
    let key = sprintf "%s|%s" config.Path config.Args
    defaultOptionNamesCache.GetOrAdd(key, fun _ ->
        let engine = EngineHelper.createEngineWithoutValidation(config, None)
        let options = engine.GetDefaultOptions()
        engine.StopProcess()
        HashSet<string>(options.Keys, StringComparer.OrdinalIgnoreCase))

let getPuzzleValueEngine config =
  let isLc0 = config.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
  let isCeres = config.Path.Contains("ceres", StringComparison.OrdinalIgnoreCase)

  try
      if isLc0 then
          let optionNames = probeDefaultOptionNames config
          if optionNames.Contains "ValueOnly" then
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
              let config = {config with Options = dict}
              let engine = EngineHelper.createEngineWithoutValidation(config, None)
              let ok = engine.WaitForReadyOk() // wait for readyok
              if not ok then
                  failwith "Engine did not respond to isready command."
              Some engine
          else
              let redMsg = sprintf "\nValueOnly option is not available for %s with args: %s, will try valuehead argument next." config.Name config.Args
              let isShowHiddenArgMissing = redMsg.Contains "--show-hidden" |> not
              if isShowHiddenArgMissing then
                  let redMsg = redMsg + " Please add --show-hidden argument to engine config."
                  ConsoleUtils.redConsole redMsg
              else
                ConsoleUtils.yellowConsole redMsg
              //for Lc0 rewrite
              let config = {config with Args = "valuehead"}
              let engine = EngineHelper.createEngineWithoutValidation(config, None)
              let ok = engine.WaitForReadyOk() // wait for readyok
              if not ok then
                  failwith "Engine did not respond to isready command."
              Some engine
      elif isCeres then
          let engine = EngineHelper.createEngineWithoutValidation(config, None)
          let ok = engine.WaitForReadyOk() // wait for readyok
          if not ok then
              failwith "Engine did not respond to isready command."
          Some engine
      else
          None
  with
      | ex ->
          let redMsg = sprintf "An error occurred while configuring value head engine for %s: \n\t%s\n" config.Name ex.Message
          ConsoleUtils.redConsole redMsg
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
  //example output is: "bestmove e2e4 ponder e7e5"
  let move = infoString.Split().[1]
  match list |> Seq.tryFind (fun x -> x.LANMove = bm) with
  |Some nnValue ->
      match list |> Seq.tryFind (fun x -> x.LANMove = move) with
      |Some nnBestValue ->
          if nnValue.LANMove = nnBestValue.LANMove then
              move, [nnValue]
          else
              move, [nnValue; nnBestValue]
      |None -> move, [nnValue]
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

let onlyUniqueOpenings (pgns:seq<PgnGame>) =
      let processedGames = ResizeArray<PgnGame>()
      let board = Board()
      let hashSet = HashSet<uint64>()

      for pgn in pgns do
        try
          board.ResetBoardState()
          if String.IsNullOrWhiteSpace pgn.Fen |> not then
             board.LoadFen pgn.Fen
          for move in pgn.Mainline do
            board.PlaySanMove move.San

          let hash = board.DeviationHash()
          if hashSet.Add hash then
            processedGames.Add pgn
          else
            printfn "Transposed game found with hash %A, skipping..." hash
        with ex ->
          // Skip this PGN but continue processing others
          printfn "Error processing PGN in removeTransposedOpenings: %s" ex.Message

      //report how many games with transpositions were removed
      let removedCount = (pgns |> Seq.length) - processedGames.Count
      if removedCount > 0 then
          printfn "Removed %d transposed games." removedCount
      else
          printfn "No transposed games found."
      processedGames


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
      let engines = engineList |> Seq.map(fun e -> EngineHelper.createEngine (e, None)) |> Seq.toArray
      let maxConcurrencyCpu = max 1 (HardwareInfo.assessMaxCpuConcurrencyLevel engines)
      let chunkSize = min maxConcurrencyCpu (engines.Length)

      let min, max =
          match minEvalScore, maxEvalScore with
          |Some min, Some max -> int min, int max
          |Some min, None -> int min, 1000
          |None, Some max -> 0, int max
          |None, None -> 0, 1000

      for engine in engines do
          let ok = engine.WaitForReadyOk() // wait for readyok
          if not ok then
              failwith "Engine did not respond to isready command."
      let filtered =
        try
          [
            for id, epd in epds |> Seq.indexed do
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

              let formatNodes n =
                  if n >= 1000000 then
                      sprintf "%.1fM" (float n / 1000000.0)
                  elif n >= 1000 then
                      sprintf "%.1fK" (float n / 1000.0)
                  else
                      sprintf "%d" n
              if passes then
                let evalAndMoveSummary =
                  evals
                  |> Array.map(fun (eval,m,nodes, name) ->
                          let nodes = formatNodes nodes
                          sprintf "%s eval: %.0f (%s nodes), %s" name eval nodes m )
                  |> String.concat ", "
                let summary = evalAndMoveSummary + (if nodes.Count = 1 then "" else (sprintf " max evalDiff: %.1f" evalDiff))
                printfn "Position %d with fen %s passed:\n %s" (id + 1) epd.FEN summary
                let posEvaluation = EPDTypes.EpdEvaluationResult.Create(epd, maxEval, evalDiff, maxMove, maxEng, summary)
                yield posEvaluation  ]
        finally
          for engine in engines do
              try engine.StopProcess() with _ -> ()
      filtered
      |> List.sortByDescending(fun p -> if engines.Length > 1 then abs p.EvalDiff else  abs p.MaxEval)
      |> ResizeArray
  with
  | ex ->
      printfn "Error in performPositionEvalTestOnEpdPositions: %s" ex.Message
      ResizeArray()

let performPositionEvalTestOnPgnGames (nodes : ResizeArray<int>) (engineList : ResizeArray<EngineConfig>) (pgns:ResizeArray<PgnGame>) (minEvalScore: string) (maxEvalScore : string) (maxEvalDiff : string) =
  try
      let minEvalScore = if String.IsNullOrWhiteSpace(minEvalScore) then None else Some minEvalScore
      let maxEvalScore = if String.IsNullOrWhiteSpace(maxEvalScore) then None else Some maxEvalScore
      let maxEvalDiff =
          if String.IsNullOrWhiteSpace(maxEvalDiff) then
              None
          else
              if nodes.Count = 1 then Some 10000 else int maxEvalDiff |> Some

      let minEv, maxEv =
          match minEvalScore, maxEvalScore with
          |Some min, Some max -> int min, int max
          |Some min, None -> int min, 1000
          |None, Some max -> 0, int max
          |None, None -> 0, 1000

      let board = Board()
      let engines = engineList |> Seq.map(fun e -> EngineHelper.createEngine (e, None)) |> Seq.toArray
      let maxConcurrencyCpu = max 1 (HardwareInfo.assessMaxCpuConcurrencyLevel engines)
      let chunkSize = min maxConcurrencyCpu (engines.Length)
      let openings = onlyUniqueOpenings pgns
      for engine in engines do
          let ok = engine.WaitForReadyOk() // wait for readyok
          if not ok then
              failwith "Engine did not respond to isready command."
      let filtered =
        try
          [
              for pgnIdx, pgn in openings |> Seq.indexed do
                  board.ResetBoardState()
                  board.LoadFen pgn.Fen
                  let moves = DeviationAnalysis.movesFromPgn pgn
                  for move in moves do
                      board.PlaySanMove move
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
                  let passes = evals |> Array.forall(fun (eval,_,_,_) -> eval >= minEv && eval <= maxEv) && evalDiff < maxEvalDiff
                  //format number of nodes for all numbers like millions, billions etc.
                  let formatNodes n =
                      if n >= 1000000 then
                          sprintf "%.1fM" (float n / 1000000.0)
                      elif n >= 1000 then
                          sprintf "%.1fK" (float n / 1000.0)
                      else
                          sprintf "%d" n
                  if passes then
                      let evalAndMoveSummary =
                          evals
                          |> Array.map(fun (eval,m,nodes, name) ->
                              let nodes = formatNodes nodes
                              sprintf "%s eval: %.0f (%s), %s" name eval nodes m )
                          |> String.concat ", "
                      let summary = evalAndMoveSummary + (if nodes.Count = 1 then "" else (sprintf " max evalDiff: %.1f" evalDiff))
                      printfn "Position %d with fen %s passed:\n %s" (pgnIdx + 1) fen summary
                      let posEvaluation = PgnEvaluationResult.Create(pgn, maxEval,evalDiff, maxMove, maxEng, summary)
                      yield posEvaluation ]
        finally
          for engine in engines do
              try engine.StopProcess() with _ -> ()
      filtered
      |> List.sortByDescending (fun p -> if engines.Length > 1 then abs p.EvalDiff else  abs p.MaxEval)
      |> ResizeArray
  with
  | ex ->
      printfn "Error in performPositionEvalTestOnPgnGames: %s" ex.Message
      ResizeArray()

let performQValueTestOnTB nodes (engineConf:EngineConfig) (puzzles:ResizeArray<TablebaseEPDEntry>) = task {
  let board = Board()
  let engine = EngineHelper.createEngine (engineConf, None)
  let ok = engine.WaitForReadyOk() // wait for readyok
  if not ok then
     failwith "Engine did not respond to isready command."
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
