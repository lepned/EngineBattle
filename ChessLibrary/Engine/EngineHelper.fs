namespace ChessLibrary

open System
open System.Diagnostics
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open System.Threading
open System.Text.Json
open Microsoft.FSharp.Core.Operators.Unchecked
open Configuration
open EngineProtocol
open RuntimeUtilities
open TypesDef.CoreTypes
open EngineTypes
open MoveTypes
open PositionTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.BoardUtils
open Microsoft.Extensions.Logging
open ChessLibrary.WinboardIntegration

/// Creating and readying engines: the setoption commands from a def, validated creation, and the
/// console pool's readiness steps.
module EngineHelper =
  open Engine
  
  let createInitialUCICommands (config:EngineConfig) =
      seq {       
        for option in config.Options do
          let mutable value = option.Value.ToString()            
          let (ok,v) = Boolean.TryParse value
          if ok then
            value <- sprintf "%b" v
          elif option.Key = "WeightsFile" && Configuration.Validation.checkPathExists value |> not then          
            let combine = Path.Combine(config.NetworkPath, value)
            value <- combine
          sprintf "setoption name %s value %s" option.Key value        
      }

  let createEngineWithoutValidation (config:EngineConfig, logger: Microsoft.Extensions.Logging.ILogger option) : ChessEngine = 
      let cmds = createInitialUCICommands config
      let eng = new ChessEngine(config, cmds, logger)
      eng.DoNotValidate()
      eng

  let createEngine (config:EngineConfig, logger: Microsoft.Extensions.Logging.ILogger option) : ChessEngine = 
      let validation = Configuration.Validation.validateChessEngineCmds config
      match validation with
      |Configuration.Validation.Errors errors -> 
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Configuration.Validation.Ok -> 
          let cmds = createInitialUCICommands config
          new ChessEngine(config, cmds, logger)

  /// An analysis engine, started: blocks until it is ready (a network load can take minutes, so
  /// call it off a UI thread - createAltEngineAsync), and throws when it cannot start.
  let rec createAltEngine (callback, config:EngineConfig, logger:ILogger, writeToConsole:bool) : AnalysisEngine =
      startAltEngine (fun cmds -> new AnalysisEngine(callback, config, cmds, logger, writeToConsole, logToFile = true)) config

  /// createAltEngine whose updates arrive with the FEN of the search they belong to.
  and createAltEngineForSearches (onSearchUpdate: SearchUpdate -> unit, config: EngineConfig, logger: ILogger, writeToConsole: bool) : AnalysisEngine =
      startAltEngine (fun cmds -> new AnalysisEngine(ignore, config, cmds, logger, writeToConsole, logToFile = true, onSearchUpdate = onSearchUpdate)) config

  and private startAltEngine (create: string seq -> AnalysisEngine) (config: EngineConfig) : AnalysisEngine =
      let validation = Configuration.Validation.validateChessEngineCmds config
      match validation with
      |Configuration.Validation.Errors errors ->
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Configuration.Validation.Ok ->
          let engine = create (createInitialUCICommands config)
          if not (engine.WaitUntilStarted(int (TimeSpan.FromHours 2.0).TotalMilliseconds)) then
            engine.Quit()
            failwith (sprintf "Engine %s could not be started%s" config.Name
                        (match engine.StartFailure with "" -> "" | r -> ": " + r))
          engine

  /// createEngine on a pool thread. The constructor starts the process and waits for uciok, which
  /// blocks; a UI thread (a Blazor handler) must not be the one waiting.
  let createEngineAsync (config: EngineConfig, logger: Microsoft.Extensions.Logging.ILogger option) : Task<ChessEngine> =
      Task.Run(fun () -> createEngine (config, logger))

  /// createAltEngine on a pool thread. The analysis engine's constructor waits for uciok AND
  /// readyok - the network load, about 5 s for Lc0 and 10 s for Ceres, minutes for a first
  /// TensorRT build - so a UI thread that makes one itself freezes the page for that long.
  let createAltEngineAsync (callback, config: EngineConfig, logger: ILogger, writeToConsole: bool) : Task<AnalysisEngine> =
      Task.Run(fun () -> createAltEngine (callback, config, logger, writeToConsole))

  let rec waitForEngineIsReady (delay:int) (engine: ChessEngine) =
    async {
        try
          if delay > 0 then
            do! Async.Sleep(delay*1000)
          if engine.HasExited() then
            engine.StartProcess()
          let! ready = engine.WaitForReadyOkAsync() |> Async.AwaitTask
          if ready then
            return $"{engine.Name} isready"
          else
            return $"{engine.Name} failed to start properly and is not ready"          
        with
        | :? System.IO.IOException as ex ->
            printfn "IO error reading from engine %s: %s" engine.Name ex.Message
            return $"{engine.Name} IO error"
        | :? System.ObjectDisposedException ->
            printfn "Engine %s process has been disposed" engine.Name
            return $"{engine.Name} disposed"
        | :? System.Threading.Tasks.TaskCanceledException ->
            printfn "Read operation timed out for engine %s" engine.Name
            return $"{engine.Name} timeout"
        | ex ->
            printfn "An unexpected error occurred while waiting for engine %s to be ready: %s" engine.Name ex.Message
            return $"{engine.Name} error"
    }

  /// Starts the engine if it has exited, waits for readyok and prepares it for its first game
  /// (warm-up, ucinewgame, readyok). Throws when the engine is not ready.
  let initEngineAsync delay (engine: ChessEngine) : Task =
    task {
      if engine.HasExited() |> not && (delay > 0) then
        do! Task.Delay(delay).ConfigureAwait(false)
      if engine.HasExited() then
        engine.StartProcess()
      let! ok = engine.WaitForReadyOkAsync().ConfigureAwait(false) // wait for readyok
      if not ok then
          failwith "Engine did not respond to isready command."
      else
          printfn "Engine %s isready" engine.Name
          // Console tournaments init each pooled engine only here, so the network must be
          // loaded here too, before any clock runs (see ChessEngine.PrepareNewGame).
          let! prepared = engine.PrepareNewGameAsync().ConfigureAwait(false)
          if not prepared then
            failwithf "Engine %s not ready after the warm-up search: %s" engine.Name engine.ReadyFailure
    }

  let initEngine delay (engine: ChessEngine) = (initEngineAsync delay engine).GetAwaiter().GetResult()

  let initEngines delay (engine1: ChessEngine) (engine2: ChessEngine) =
    async {
      if engine1.HasExited() |> not && engine2.HasExited() |> not && (delay > 0) then
        do! Async.Sleep(delay)
      elif engine1.HasExited() || engine2.HasExited() then
        if engine1.HasExited() then
          engine1.StartProcess()
        if engine2.HasExited() then
          engine2.StartProcess()
        let! res =
          [waitForEngineIsReady delay engine1; waitForEngineIsReady delay engine2]
          |> Async.Parallel
        for e in res do
          printfn "%s" e } |> Async.RunSynchronously
