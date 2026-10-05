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

/// Read a line from the engine, failing fast when the stream has closed (the process
/// exited). Null means EOF only — a live engine that stays silent (e.g. it did not
/// understand a command) simply blocks in the read; no timeout is applied by design.
/// Without this, EOF either NullReferenceExceptions on StartsWith or hot-spins forever
/// in the IsNullOrEmpty-ignore loops, wedging the whole puzzle sweep.
let private readLineChecked (engine: ChessEngine) =
    let line = engine.ReadLine()
    if isNull line then
        failwithf "Engine %s closed its output (process exited) during puzzle analysis" engine.Name
    line

/// A puzzle engine never became ready: stop it (a mute-but-alive engine must not be leaked)
/// and fail with the reason WaitForReadyOk recorded — exit code, timeout, or the fatal
/// marker the engine printed — instead of a bare "did not respond to isready".
let private notReady (engine: ChessEngine) =
    let reason = if String.IsNullOrEmpty engine.ReadyFailure then "no readyok" else engine.ReadyFailure
    (try engine.StopProcess() with _ -> ())
    failwithf "Engine %s did not become ready: %s" engine.Name reason

let solvePuzzleSearch (nodes: int) (engine: ChessEngine) (pos: string) =
  // A valid UCI move is 4-5 chars: [a-h][1-8][a-h][1-8][qrbn]?
  // Used to strip non-move tokens from PV (e.g. Ceres appends "string M= N").
  let isUciMove (s: string) =
      (s.Length = 4 || s.Length = 5)
      && s.[0] >= 'a' && s.[0] <= 'h'
      && s.[1] >= '1' && s.[1] <= '8'
      && s.[2] >= 'a' && s.[2] <= 'h'
      && s.[3] >= '1' && s.[3] <= '8'

  let mutable cont = true
  let mutable bestmove = ""
  let mutable lastPV = ""
  let nnList = ResizeArray<EngineTypes.NNValues>()
  engine.Position pos
  engine.GoNodes nodes
  while cont do
    let line = readLineChecked engine
    if line.StartsWith "bestmove" then
      bestmove <- line.Split().[1]
      cont <- false
    elif line.StartsWith "info depth" then
      let pvMatch = EngineProtocol.Regex.pvRegex.Match(line)
      if pvMatch.Success then
        let rawPV = pvMatch.Groups.[1].Value.TrimEnd()
        // Strip trailing non-move tokens (e.g. Ceres "string M= 2")
        lastPV <-
            rawPV.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.takeWhile isUciMove
            |> String.concat " "
    elif line.StartsWith "info string" && line.Contains "N:" then
      let nnMsg = EngineProtocol.Regex.getInfoStringData engine.Name line
      if nnList.Count > 0 then nnList.Clear()
      nnList.Add(nnMsg)
      let mutable contNN = not (line.StartsWith "info string node")
      while contNN do
          let newline = readLineChecked engine
          if newline.StartsWith "info string node" then
              contNN <- false
          elif newline.StartsWith "info string" then
              nnList.Add(EngineProtocol.Regex.getInfoStringData engine.Name newline)
          else
              // Non-NNValues line got interleaved — exit inner loop and handle it
              contNN <- false
              if newline.StartsWith "bestmove" then
                  bestmove <- newline.Split().[1]
                  cont <- false
              elif newline.StartsWith "info depth" then
                  let pvMatch = EngineProtocol.Regex.pvRegex.Match(newline)
                  if pvMatch.Success then
                      let rawPV = pvMatch.Groups.[1].Value.TrimEnd()
                      lastPV <-
                          rawPV.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                          |> Array.takeWhile isUciMove
                          |> String.concat " "
  (bestmove, lastPV, nnList)

let getPuzzlePolicyEngine config =
  let engine = EngineHelper.createEngine(config)
  let ok = engine.WaitForReadyOk() // wait for readyok
  if not ok then
      notReady engine
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
  let pathHasLc0 = config.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
  let pathHasCeres = config.Path.Contains("ceres", StringComparison.OrdinalIgnoreCase)

  try
      // Classify the engine family: executable path first (free), otherwise probe the
      // binary's self-declared UCI identity ("id name Lc0 v..." / "id name Ceres ...")
      // once. Keeps validation rule-consistent with bestQPuzzleValueOnly and makes
      // config display Names and install paths fully cosmetic for value testing.
      let isLc0, isCeres =
          if pathHasLc0 || pathHasCeres then
              pathHasLc0, pathHasCeres
          else
              let probe = EngineHelper.createEngineWithoutValidation(config, None)
              let ok = probe.WaitForReadyOk()
              let idName = if ok then probe.UciIdName else ""
              probe.Quit()
              idName.Contains("lc0", StringComparison.OrdinalIgnoreCase),
              idName.Contains("ceres", StringComparison.OrdinalIgnoreCase)

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
                  notReady engine
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
                  notReady engine
              Some engine
      elif isCeres then
          let engine = EngineHelper.createEngineWithoutValidation(config, None)
          let ok = engine.WaitForReadyOk() // wait for readyok
          if not ok then
              notReady engine
          Some engine
      else
          // Neither the path nor the probed UCI identity says Lc0 or Ceres:
          // the engine has no supported value-test mode.
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
    let line = readLineChecked engine
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
          let newline = readLineChecked engine
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

let bestPolicyMoveAllPolicies (nodes:int) (engine: ChessEngine) (pos:string) =
  let mutable cont = true
  let mutable infoString = ""
  engine.Position pos
  engine.GoNodes nodes
  let list = ResizeArray<NNValues>()
  while cont do
    let line = readLineChecked engine
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
          let newline = readLineChecked engine
          if newline.StartsWith "info string node" then
              contNN <- false
          else
              let msg = EngineProtocol.Regex.getInfoStringData engine.Name newline
              list.Add msg
  let move = infoString.Split().[1]
  let sorted = list |> Seq.sortByDescending (fun v -> v.P) |> Seq.toList
  (move, sorted)

let bestPolicyMove (nodes:int) (engine: ChessEngine) (pos:string)  =
  let mutable cont = true
  let mutable infoString = ""
  engine.Position pos
  engine.GoNodes nodes
  let list = ResizeArray<NNValues>()
  while cont do
    let line = readLineChecked engine
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
          let newline = readLineChecked engine
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
  engine.UciNewGame()
  engine.WaitForReadyOk() |> ignore
  engine.Position pos
  engine.Go timeInMs
  let list = ResizeArray<NNValues>()
  while cont do
    let line = readLineChecked engine
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
          let newline = readLineChecked engine
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
  // Detect Ceres primarily by the engine's SELF-DECLARED UCI identity ("id name ..."),
  // falling back to the executable path (the rule getPuzzleValueEngine's validation
  // uses). Never detect by config display Name: that caused a silent, year-class bug
  // where any config whose cosmetic "Name" lacked the substring "ceres" passed
  // validation (path-based) but fell back to 'go nodes 1' below, making the Value
  // test silently report the Policy result (Value Perf ≡ Policy Perf, digit-identical).
  let isCeres =
    engine.UciIdName.Contains("ceres", StringComparison.OrdinalIgnoreCase)
    || engine.Path.Contains("ceres", StringComparison.OrdinalIgnoreCase)
  if isCeres then
    engine.GoValue()
  else
    engine.GoNodes 1
  while cont do
    let line = readLineChecked engine
    if line.StartsWith "bestmove" then
      cont <- false
      infoString <- line
  //example output is: "bestmove e2e4 ponder e7e5"
  let move = infoString.Split().[1]
  move
