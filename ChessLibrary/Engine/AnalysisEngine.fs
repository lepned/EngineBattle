namespace ChessLibrary

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core.Operators.Unchecked
open TypesDef.CoreTypes
open EngineTypes
open MoveTypes
open PositionTypes
open ChessLibrary.BoardUtils
open ChessLibrary.RuntimeUtilities
open ChessLibrary.WinboardIntegration
open ChessLibrary.EngineWire

/// How an awaited search ended.
type AnalysisOutcome =
  | Completed of BestMoveInfo option
  /// Replaced by a newer search, or stopped before its go was sent.
  | Superseded
  | Failed of string

/// An update with the search it belongs to - the id `Analyse` returned, and its position (FEN);
/// 0 and "" for the engine's own (Ready, EngineFailed).
type SearchUpdate = { Update: EngineUpdate; Fen: string; SearchId: int }

/// How a line from a UCI script is sent: options only while the engine is idle (a running search is
/// stopped and rerun after them, as the settings dialog does), and no search control - the panel
/// owns the searches, and a go or stop it does not know of would leave it showing the wrong state.
type ScriptLine =
  | AsOption of string
  /// ucinewgame: between searches.
  | AsNewGame of string
  | Refused of reason: string
  | AsIs of string
  with
    static member Of(line: string) =
      let line = line.Trim()
      let first = (line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead |> Option.defaultValue "").ToLowerInvariant()
      match first with
      | "setoption" -> AsOption line
      | "ucinewgame" -> AsNewGame line
      | "go" | "stop" | "position" | "ponderhit" | "isready" | "quit" ->
          Refused (sprintf "'%s' controls the search, which the panel owns" first)
      | _ -> AsIs line

type private Delivery =
  | Deliver of EngineUpdate * fen: string * search: int
  | Run of (unit -> unit)

type private AnalysisMessage =
  | Ev of AnalysisMachine.Event
  | Await of AnalysisMachine.Search * AsyncReplyChannel<AnalysisOutcome>
  /// Quit: every waiting search is answered, and later ones at once.
  | Shutdown of AsyncReplyChannel<unit>

/// An engine for the analysis pages and Game Review. Runs AnalysisMachine in an agent: searches
/// are requests (the newest wins), a stopped search's output never reaches the next one, and the
/// updates reach `onUpdate` through a queue of their own, so it may call back in. Each update comes
/// with its search's id and FEN.
type AnalysisEngine(onUpdate: SearchUpdate -> unit, config: EngineConfig, initCommands: string seq,
                    logger: ILogger, writeToConsole: bool, ?logToFile: bool) =
  let name = config.Name
  let protocol = protocolFor config (Some logger)
  let isLc0 = EngineProcess.pathMentions "lc0" config
  let isCeres = EngineProcess.pathMentions "ceres" config
  let debugOn () = logger.IsEnabled LogLevel.Debug

  let ioLogPath, ioLog =
    if defaultArg logToFile false then
      let path, log = EngineProcess.IoLog.Open name
      logger.LogInformation $"Engine I/O logging to: {path}"
      path, Some log
    else "", None
  let logIO (direction: string) (text: string) = ioLog |> Option.iter (fun log -> log.Write(direction, text))

  // the searched position, for SAN; written and read on the agent thread, read by
  // CurrentPositionCommand from others
  let moveBoard = Chess.Board()
  let boardLock = obj ()
  let mutable whiteToMove = true
  let moveList = Array.init 256 (fun _ -> defaultof<TMove>)
  let sanPvCache = Dictionary<int, struct (string * string)>()
  let convertPv (mpv: int) (lan: string) =
    match sanPvCache.TryGetValue mpv with
    | true, struct (cachedLan, cachedSan) when cachedLan = lan -> cachedSan
    | _ ->
        let san = lock boardLock (fun () -> getShortSanPVFromLongSanPVFast moveList &moveBoard lan)
        sanPvCache.[mpv] <- struct (lan, san)
        san
  let position = AnalysisOutput.boardPosition moveBoard boardLock (fun () -> whiteToMove) convertPv

  let optionsMap = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
  let dict = Dictionary<string, obj>()
  let nonDefaultValues = Dictionary<string, (string * string)>()
  let mutable backend = ""
  let mutable chess960Sent = false
  let mutable searchMoves: string list = []
  // the engine's own text (dump answers, NNUE notes) on the console; a batch run turns it off
  let mutable echoEngineText = true

  let ceresNetworkName =
    match config.Options |> Seq.tryFind (fun e -> e.Key = "Network") with
    | Some net ->
        let nn = net.Value.ToString()
        if nn.Contains "/" then (let parts = nn.Split '/' in parts.[parts.Length - 1]) else ""
    | None ->
        if isCeres && not (String.IsNullOrEmpty config.Args) then
          let parts = config.Args.Split ':'
          if parts.Length > 1 then parts.[1] else ""
        else ""
  let mutable network = if isCeres then ceresNetworkName else ""

  let assignNetworkName (command: string) =
    match EngineStartup.networkIn command with
    | Some _ when ceresNetworkName <> "" -> network <- ceresNetworkName
    | Some n -> network <- n
    | None -> ()
  let assignBackend (command: string) =
    if command.ToLower().Contains "backendoptions" then
      let parts = command.Split ' '
      let at = parts |> Array.findIndex ((=) "value")
      backend <- parts.[at + 1 ..] |> String.concat " "
  let recordCommand (command: string) =
    if command.Contains "UCI_Chess960" then chess960Sent <- command.Contains "true"
    assignNetworkName command
  let rememberOption (key: string) (value: obj) = lock dict (fun () -> dict.[key] <- value)

  let stderr = EngineProcess.StderrRing()
  let mutable lastExitCode : int option = None
  let mutable shutdownRequested = false
  let transport =
    let arguments = EngineStartup.arguments config isLc0
    EngineProcess.Transport(config, arguments, stderr,
      (fun msg -> logger.LogDebug $"[{name}] {msg}"),
      (fun line -> if debugOn () then logger.LogDebug $"[STDERR {name}]: {line}"),
      (fun code ->
        if code.IsSome then lastExitCode <- code
        match code with
        | Some c when c <> 0 ->
            if shutdownRequested then logger.LogDebug $"Engine {name} exited with code {c} after quit"
            else logger.LogInformation $"⚠️ Engine {name} exited unexpectedly with code {c}"
        | _ -> ()))
  let output = Channel.CreateUnbounded<string>()
  let hasExited () = try transport.HasExited with _ -> true

  let write (command: string) =
    try
      if hasExited () then logger.LogWarning("Engine {Engine} has exited; '{Command}' not sent", name, command)
      else
        match protocol with
        | Winboard _ ->
            outbound protocol true command |> List.iteri (fun i line ->
              logIO ">>>" line
              let delay = lineDelayMs config protocol i line
              if delay > 0 then Thread.Sleep delay
              transport.WriteLine line)
        | Uci ->
            transport.WriteLine command
            logIO ">>>" command
    with ex -> logger.LogWarning(ex, "Engine {Engine}: could not send '{Command}'", name, command)

  // each search's position as a FEN, for its updates; written and read on the agent only
  let searchFens = Dictionary<int, string>()

  /// Sets the board for SAN, and tells the engine when the variant changes.
  let usePosition (search: int) (command: string) =
    // the board reads only the fen form; startpos is its start position
    let boardCommand =
      if command.StartsWith("position startpos", StringComparison.Ordinal) then
        "position fen " + Chess.Board().FEN() + command.Substring "position startpos".Length
      else command
    let isFrc =
      lock boardLock (fun () ->
        moveBoard.ResetBoardState()
        moveBoard.PlayCommands boardCommand
        searchFens.[search] <- moveBoard.FEN()
        for old in [ for id in searchFens.Keys do if id <= search - 32 then yield id ] do searchFens.Remove old |> ignore
        whiteToMove <- moveBoard.Position.STM = 0uy
        moveBoard.IsFRC)
    sanPvCache.Clear()
    if isFrc <> chess960Sent then
      write (sprintf "setoption name UCI_Chess960 value %b" isFrc)
      chess960Sent <- isFrc

  /// The init commands, once the engine's options are known: checked against them and printed.
  let initCommandsFor (lines: string list) =
    for line in lines do UciOption.addOptionToMap optionsMap line
    let commands =
      [ for cmd in initCommands do
          match EngineStartup.check optionsMap cmd with
          | EngineStartup.Valid (optName, value, changedFrom) ->
              rememberOption optName (box value)
              changedFrom |> Option.iter (fun def -> nonDefaultValues.[optName] <- (def, value))
              ConsoleUtils.printInColor ConsoleColor.Green (sprintf "The option '%s' with value '%s' is valid." optName value)
          | EngineStartup.Invalid (optName, value) ->
              ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." optName value)
          | EngineStartup.Malformed -> ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)
          assignBackend cmd
          recordCommand cmd
          yield cmd
        // a movetime here is the time asked for: no overhead, whatever the def sets for play
        // (Lc0 defs carry 1000 ms, which turned "go movetime 1000" into an instant answer);
        // last, so it wins over the def's value. Only an engine that has the option gets it.
        match EngineStartup.moveOverhead optionsMap [] 0L with
        | Some (optName, value) ->
            // recorded like the def's options, so the settings dialog shows what the engine runs with
            rememberOption optName (box value)
            yield sprintf "setoption name %s value %d" optName value
        | None -> () ]
    if isCeres then network <- ceresNetworkName
    Engine.printNonDefaultValues name config.Path nonDefaultValues
    // the engine's defaults for the options the config leaves alone
    lock dict (fun () ->
      for key, value in EngineStartup.defaults optionsMap do
        if not (dict.ContainsKey key) then dict.Add(key, value))
    commands

  let settings : AnalysisMachine.Settings =
    { Name = name
      CanPing = not (isWinboard protocol)
      // Winboard's stop is exit while analysing, and exit prints no move
      StopAnswers = (fun go ->
        match protocol with
        | Winboard handler -> not (go.StartsWith("go infinite", StringComparison.Ordinal) && handler.Features.Analyze)
        | Uci -> true)
      StartTimeout = TimeSpan.FromHours 2.0
      // isready after a new network can take minutes (a TensorRT build)
      PingTimeout = TimeSpan.FromMinutes 10.0
      StopWait = TimeSpan.FromSeconds 10.0 }

  let started = TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
  let mutable startFailure = ""
  let deliveries = Channel.CreateUnbounded<Delivery>(UnboundedChannelOptions(SingleReader = true))
  /// One delivery, on the delivery thread: caller code never runs on the agent.
  let deliverOne item =
    match item with
    | Deliver (update, fen, search) ->
        // the caller sees Ready before WaitUntilStarted returns
        (try onUpdate { Update = update; Fen = fen; SearchId = search }
         with ex -> logger.LogWarning(ex, "Engine {Engine}: update callback failed", name))
        match update with
        | Ready _ -> started.TrySetResult true |> ignore
        | EngineFailed (_, reason) when not started.Task.IsCompleted ->
            startFailure <- reason
            started.TrySetResult false |> ignore
        | _ -> ()
    | Run action ->
        try action () with ex -> logger.LogWarning(ex, "Engine {Engine}: reply failed", name)
  let deliveryLoop =
    async {
      let reader = deliveries.Reader
      let mutable more = true
      while more do
        let! ready = reader.WaitToReadAsync().AsTask() |> Async.AwaitTask
        if not ready then more <- false
        else
          let mutable reading = true
          while reading do
            match reader.TryRead() with
            | true, item -> deliverOne item
            | _ -> reading <- false
    }

  let clock = Diagnostics.Stopwatch.StartNew()
  let mutable nextId = 0
  let newSearch position go : AnalysisMachine.Search =
    { Id = Interlocked.Increment &nextId; Position = position; Go = go }

  let toOutcome = function
    | AnalysisMachine.Completed info -> Completed info
    | AnalysisMachine.Superseded -> Superseded
    | AnalysisMachine.Failed reason -> Failed reason

  let agent =
    MailboxProcessor.Start(fun inbox ->
      let waiters = Dictionary<int, AsyncReplyChannel<AnalysisOutcome>>()
      let mutable closed = false
      let run state event =
        let state, effects =
          try AnalysisMachine.step settings position clock.Elapsed state event
          with ex ->
            // a dead agent would leave every caller waiting; drop the event instead
            logger.LogError(ex, "Engine {Engine}: {Event} not handled", name, event)
            state, []
        // after Quit only the answers matter: the exit it causes is no failure
        let effects = if closed then effects |> List.filter (function AnalysisMachine.Reply _ -> true | _ -> false) else effects
        for effect in effects do
          try
            match effect with
            | AnalysisMachine.Send command -> write command
            | AnalysisMachine.UsePosition (search, command) -> usePosition search command
            | AnalysisMachine.Emit (update, search) ->
                let fen =
                  match search with
                  | Some id -> (match searchFens.TryGetValue id with | true, f -> f | _ -> "")
                  | None -> ""
                deliveries.Writer.TryWrite(Deliver (update, fen, defaultArg search 0)) |> ignore
            | AnalysisMachine.Reply (id, outcome) ->
                match waiters.TryGetValue id with
                | true, reply ->
                    waiters.Remove id |> ignore
                    // after the search's updates, through the same queue; directly once it is closed
                    let answer () = reply.Reply (toOutcome outcome)
                    if not (deliveries.Writer.TryWrite(Run answer)) then answer ()
                | _ -> ()
            | AnalysisMachine.OptionsReceived lines ->
                let commands =
                  try initCommandsFor lines
                  with ex ->
                    logger.LogError(ex, "Engine {Engine}: its options could not be read", name)
                    List.ofSeq initCommands
                inbox.Post (Ev (AnalysisMachine.Init commands))
            // one write per line: printfn writes the text and the newline apart, and another thread's
            // line could land between them
            | AnalysisMachine.Print text -> if echoEngineText then Console.Out.WriteLine text
            | AnalysisMachine.Debug text -> if debugOn () then logger.LogDebug text
            | AnalysisMachine.Warn text -> logger.LogWarning text
          with ex ->
            // the agent must survive: every caller waits on it
            logger.LogError(ex, "Engine {Engine}: {Effect} failed", name, effect)
        state
      let runDue state =
        match AnalysisMachine.nextDue settings state with
        | Some due when due <= clock.Elapsed -> run state AnalysisMachine.Tick
        | _ -> state
      let rec loop state = async {
        let timeout =
          match AnalysisMachine.nextDue settings state with
          | Some due -> max 0 (int (due - clock.Elapsed).TotalMilliseconds)
          | None -> Timeout.Infinite
        let! message = inbox.TryReceive timeout
        let state =
          match message with
          | None -> run state AnalysisMachine.Tick
          | Some (Ev event) -> run state event
          | Some (Await (_, reply)) when closed ->
              reply.Reply (Failed "the engine was shut down")
              state
          | Some (Await (search, reply)) ->
              waiters.[search.Id] <- reply
              run state (AnalysisMachine.Analyse search)
          | Some (Shutdown reply) ->
              closed <- true
              for waiter in waiters.Values do waiter.Reply (Failed "the engine was shut down")
              waiters.Clear()
              reply.Reply ()
              state
        return! loop (runDue state) }
      loop (AnalysisMachine.initial (not (isWinboard protocol))))

  let pump =
    async {
      try
        let reader = output.Reader
        let mutable more = true
        while more do
          let! ready = reader.WaitToReadAsync().AsTask() |> Async.AwaitTask
          if not ready then more <- false
          else
            let mutable reading = true
            while reading do
              match reader.TryRead() with
              | true, line ->
                  logIO "<<<" line
                  if writeToConsole then printfn "%s" line
                  match protocol with
                  | Winboard handler ->
                      match handler.ProcessOutput line with
                      | Some uciLine -> agent.Post (Ev (AnalysisMachine.Line uciLine))
                      | None -> ()
                  | Uci -> agent.Post (Ev (AnalysisMachine.Line line))
              | _ -> reading <- false
        agent.Post (Ev AnalysisMachine.OutputClosed)
      with ex ->
        // an exception here would take the process down
        logger.LogWarning(ex, "Engine {Engine}: output reader failed", name)
        try agent.Post (Ev AnalysisMachine.OutputClosed) with _ -> ()
    }

  let startWinboard (handler: WinboardProtocol.WinboardHandler) =
    async {
      try
        for cmd in handler.GetInitCommands() do transport.WriteLine cmd
        let! ok = initializeWinboardEventBased handler (Some logger) name FeatureTimeoutMs (forceV1 config) transport.WriteLine
        if ok then
          for reply in handler.TakeFeatureReplies() do
            if handler.CommandDelayMs > 0 then do! Async.Sleep handler.CommandDelayMs
            transport.WriteLine reply
          for cmd in handler.GetPostInitCommands() do
            if handler.CommandDelayMs > 0 then do! Async.Sleep handler.CommandDelayMs
            transport.WriteLine cmd
          agent.Post (Ev (AnalysisMachine.Init (initCommandsFor [])))
        else
          logger.LogError("Winboard engine {Engine} did not initialize", name)
          try transport.Process.Kill true with _ -> ()
      with ex ->
        logger.LogError(ex, "Winboard engine {Engine}: initialization failed", name)
        try transport.Process.Kill true with _ -> ()
    }

  do
    Async.Start deliveryLoop
    let launched =
      try transport.Start(EngineProcess.Lines output.Writer)
      with ex ->
        logger.LogError(ex, "Engine {Engine} could not be started", name)
        false
    if launched then
      Async.Start pump
      match protocol with
      | Winboard handler -> Async.Start (startWinboard handler)
      | Uci -> write "uci"
    else
      // the machine closes, and fails whatever waits
      agent.Post (Ev AnalysisMachine.OutputClosed)

  let configure (commands: string list) restart =
    for cmd in commands do recordCommand cmd
    agent.Post (Ev (AnalysisMachine.Configure (commands, restart)))

  /// Completes true once the engine is ready, false when it could not start.
  member _.Started = started.Task

  /// Why the engine could not start; "" when it did.
  member _.StartFailure = startFailure

  /// Blocks until the engine is ready; false when it failed or the time ran out.
  member _.WaitUntilStarted(timeoutMs: int) =
    try started.Task.Wait timeoutMs && started.Task.Result with _ -> false

  /// Searches a position; a newer request replaces it. Results arrive as updates.
  /// The search's id: its updates carry it (SearchUpdate).
  member _.Analyse(positionCommand: string, goCommand: string) : int =
    let search = newSearch positionCommand goCommand
    agent.Post (Ev (AnalysisMachine.Analyse search))
    search.Id

  /// Searches a position and waits for its end.
  member _.Search(positionCommand: string, goCommand: string) : Async<AnalysisOutcome> =
    let search = newSearch positionCommand goCommand
    agent.PostAndAsyncReply(fun reply -> Await (search, reply))

  /// Nothing to search (no legal move): a running search is replaced, and this request ends at once.
  member _.Skip() : int =
    let search = newSearch "" ""
    agent.Post (Ev (AnalysisMachine.Analyse search))
    search.Id

  /// Searches the board's position (with the searchmoves set); a board with no legal move is a Skip.
  member this.AnalyseBoard(board: Chess.Board, goCommand: string) : int =
    if board.AnyLegalMove() then this.Analyse(board.PositionWithMovesFromGraph(), goCommand + this.SearchMoveSuffix)
    else
      logger.LogInformation("No legal moves with FEN: {Fen}", board.FEN())
      this.Skip()

  /// Stops the running search; its bestmove still arrives (before its go: a move at once).
  member _.Stop() = agent.Post (Ev AnalysisMachine.Stop)

  /// ucinewgame before the next search.
  member _.NewGame() = configure [ "ucinewgame" ] false

  /// Applied while the engine waits; a running search is stopped and run again after it.
  member _.SetOption(option: EngineOption) =
    rememberOption option.Name (box option.Value)
    configure [ sprintf "setoption name %s value %s" option.Name option.Value ] true

  member _.SetOptions(options: EngineOption seq) =
    let options = List.ofSeq options
    for o in options do rememberOption o.Name (box o.Value)
    configure [ for o in options -> sprintf "setoption name %s value %s" o.Name o.Value ] true

  member _.SetAllOptions(allOptions: Dictionary<string, obj>) =
    let commands =
      [ for opt in allOptions do
          rememberOption opt.Key opt.Value
          match Boolean.TryParse(opt.Value.ToString()) with
          | true, v -> yield sprintf "setoption name %s value %b" opt.Key v
          | _ -> yield sprintf "setoption name %s value %s" opt.Key (opt.Value.ToString()) ]
    for cmd in commands do printfn "%s" cmd
    configure commands true

  /// Only a spin option within its range is sent.
  member _.SetMoveOverhead(optionName: string, ms: int) =
    match UciOption.tryFindOption optionsMap optionName with
    | Some option ->
        match option.OptionType with
        | UciOption.Spin (lo, hi, _) when int64 ms >= lo && int64 ms <= hi ->
            configure [ sprintf "setoption name %s value %d" option.Name ms ] true
        | _ -> ()
    | None -> printfn "Option not found: %s value: %d" optionName ms

  /// Sent at once (uci, dump commands).
  member _.Raw(command: string) =
    recordCommand command
    agent.Post (Ev (AnalysisMachine.Raw command))

  /// A UCI script line (ScriptLine.Of): true when it was sent, false when refused.
  member this.Script(line: string) : bool =
    match ScriptLine.Of line with
    | AsOption command ->
        UciOption.parseSetOptionCommand command |> Option.iter (fun (name, value) -> rememberOption name (box value))
        configure [ command ] true
        true
    | AsNewGame command -> configure [ command ] false; true
    | Refused reason ->
        logger.LogWarning("Engine {Engine}: script line '{Line}' skipped: {Reason}", name, line, reason)
        false
    | AsIs command -> this.Raw command; true

  /// quit, a second to go, then killed.
  member _.Quit() =
    shutdownRequested <- true
    // every waiting search gets its answer before the queue closes
    (try agent.PostAndReply((fun reply -> Shutdown reply), 2000) with _ -> ())
    try
      if not (hasExited ()) then
        logIO ">>>" "quit"
        transport.WriteLine "quit"
      if not (transport.Process.WaitForExit 1000) then transport.Process.Kill true
      transport.Process.Close()
      transport.Process.Dispose()
    with _ -> ()
    deliveries.Writer.TryComplete() |> ignore
    ioLog |> Option.iter (fun log -> (log :> IDisposable).Dispose())
    printfn "Engine %s has been shut down." name

  member _.EchoEngineText with get () = echoEngineText and set v = echoEngineText <- v
  /// The file the engine's commands and answers are written to; "" when none is.
  member _.IoLogPath = ioLogPath
  member _.SetSearchMoves(moves: string list) = searchMoves <- moves
  member _.ClearSearchMoves() = searchMoves <- []
  member _.SearchMoves = searchMoves
  /// " searchmoves …", or "" with none set.
  member _.SearchMoveSuffix = if searchMoves.IsEmpty then "" else " searchmoves " + String.concat " " searchMoves

  member _.HasExited = hasExited ()
  member _.ErrorOutput = stderr.Snapshot() :> seq<string>
  member _.LastExitCode = lastExitCode
  member _.GetDiagnostics() = stderr.Diagnostics(name, lastExitCode)
  member _.GetNoneDefaultSetOptions() = nonDefaultValues
  /// A copy: the engine keeps writing its own.
  member _.GetAllDefaultOptions() = lock dict (fun () -> Dictionary<string, obj>(dict))
  member _.GetUCICommands() = optionsMap
  member _.UciIdName = Engine.uciIdName optionsMap
  member _.IsLc0 = isLc0
  member _.Network = network
  member _.Name = name
  member _.FullName = if network <> "" then $"{name} with net: {network}" else name
  member _.GetBackEnd() = backend
  member _.Config = config
  member _.Path = config.Path
  member _.BenchmarkLC0Cmd = EngineProtocol.Engine.createLC0BenchmarkString config
  member _.PrintUCI() = Engine.printConfigCommands name initCommands
  member _.ShowCommands = fun () -> Engine.printConfigCommands name initCommands
  member _.CurrentPositionCommand() = lock boardLock (fun () -> moveBoard.PositionWithMovesFromGraph())
