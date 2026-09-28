namespace ChessLibrary

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices
open Microsoft.FSharp.Core.Operators.Unchecked
open Microsoft.Extensions.Logging
open Configuration
open EngineProtocol
open RuntimeUtilities
open TypesDef.CoreTypes
open EngineTypes
open MoveTypes
open PositionTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.BoardUtils
open ChessLibrary.WinboardIntegration
open ChessLibrary.EngineWire

/// The two engine wrappers.
///
/// ChessEngineWithUCIProcessing runs an engine for the analysis pages, Game Review and the value
/// head checks: every line the engine prints is parsed on the reader thread and pushed to the
/// caller's callback as an EngineUpdate. ChessEngine runs an engine in tournaments, the tuner, the
/// console tools and the puzzle runner: the caller writes commands and pulls the replies itself.
///
/// Both speak UCI-shaped commands; a Winboard engine gets them translated (EngineWire). The
/// process itself, its stderr and the I/O log are EngineProcess's: both run their engine through
/// an EngineProcess.Transport, one reading its output pushed line by line, the other pulling it.
///
/// The public surface is pinned by TestProject/EngineApiSurfaceTests.fs and the behaviour by
/// TestProject/EngineCharacterizationTests.fs, which run a scripted fake engine as a real child
/// process. Several quirks are kept on purpose and marked QUIRK where they live; change them as
/// their own step, together with the test that pins them.
module Engine =

  /// Kept at this path for its callers; the implementation is EngineProcess.stripAnsi.
  let internal stripAnsi (line: string) = EngineProcess.stripAnsi line

  /// True for a line an engine prints when it has given up initializing without exiting.
  let isFatalInitLine (line: string) = EngineProcess.isFatalInitLine line

  /// Engines whose options were printed in full this run; later instances only print their
  /// device. Reset per tournament run (resetPrintedEngines).
  let printedEngines = HashSet<string>()
  /// Not printedEngines: that set is only filled when option validation runs.
  let loggedVersions = HashSet<string>()
  let printLock = obj()
  let resetPrintedEngines () =
    printedEngines.Clear()
    loggedVersions.Clear()

  let private printNonDefaultValues (name: string) (path: string) (nonDefaultValues: Dictionary<string, (string * string)>) =
    printfn "\nCustomized SetOptions for %s:\n" name
    ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
    for opt in nonDefaultValues do
      let (def, value) = opt.Value
      if String.IsNullOrEmpty def then
        ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
      else
        ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
    printfn ""

  let private printConfigCommands (name: string) (initCommands: string seq) =
    printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
    for cmd in initCommands do
      if cmd.Contains "setopt" then
        printfn "%s" cmd

  /// Engine's self-declared identity from the UCI handshake ("id name ..."), or "".
  let private uciIdName (optionsMap: Dictionary<string, UciOption.UciOption>) =
    match UciOption.tryFindOption optionsMap "name" with
    | Some { OptionType = UciOption.UciOptionType.IdAndAuthor(_, _, value) } -> value
    | _ -> ""

  /// Where an analysis engine is in its start-up conversation. The reader thread moves it on as
  /// uciok and readyok arrive; a caller waiting for one of them watches it.
  type private Handshake =
    | AwaitingUciOk
    | AwaitingReadyOk
    | Running

  /// The process a ChessEngine is running now, and what has been done to it. A restart makes a new
  /// one, so a warm-up or a MoveOverheadMs sent to the old process never counts for the new one.
  [<AllowNullLiteral>]
  type private RunningEngine(transport: EngineProcess.Transport) =
    member _.Transport = transport
    member _.Process = transport.Process
    /// The once-per-process `go nodes 1` has run (ChessEngine.WarmUp).
    member val WarmedUp = false with get, set
    /// The MoveOverheadMs value this process has been sent.
    member val MoveOverheadSent : int64 option = None with get, set

  // ════════════════════════════════════════════════════════════════════════════════════════════
  //  The analysis wrapper: parses on the reader thread and pushes EngineUpdates
  // ════════════════════════════════════════════════════════════════════════════════════════════

  type ChessEngineWithUCIProcessing (callback, config : EngineConfig, initCommands: string seq, logger:ILogger, writeToConsole: bool, ?logToFile: bool)  =
      let name = config.Name
      let isEnabled level = logger.IsEnabled level
      let logDebug (text: string) = logger.LogDebug text
      let logInformation (text: string) = logger.LogInformation text
      let logError (text: string) = logger.LogError text

      let ioLog =
          if defaultArg logToFile false then
              let path, log = EngineProcess.IoLog.Open name
              logInformation $"Engine I/O logging to: {path}"
              Some log
          else None
      let logIO (direction: string) (text: string) =
          match ioLog with
          | Some log -> log.Write(direction, text)
          | None -> ()

      let isLc0 = EngineProcess.pathMentions "lc0" config
      let isCeres = EngineProcess.pathMentions "ceres" config
      let protocol = protocolFor config (Some logger)

      // The position the engine is searching, for turning its moves into SAN. Written by whichever
      // thread sends a position and read by the reader thread while it parses; every access goes
      // through the lock, so a new position waits for the line being parsed instead of changing
      // the board underneath it. The side to move is cached outside the lock: the reader needs it
      // for every info line and it only changes with the position.
      let moveBoard = Chess.Board()
      let moveBoardLock = obj()
      [<VolatileField>]
      let mutable whiteToMove = true
      let moveList = Array.init 256 (fun _ -> defaultof<TMove>)

      // The last UCI_Chess960 value sent. The wrapper used to keep every command it ever sent and
      // search that list backwards on each position; this is the same answer without the list.
      let mutable chess960Sent = false
      let recordCommand (cmd: string) =
          if cmd.Contains "UCI_Chess960" then chess960Sent <- cmd.Contains "true"

      // What the output so far amounts to (AnalysisOutput.State). The reader thread owns it; a
      // thread that sends a new position only raises newPosition, and the reader drops the old
      // variation and the SAN cache itself before its next line.
      let mutable output = AnalysisOutput.State.Initial
      [<VolatileField>]
      let mutable newPosition = false
      // Last (UCI PV, SAN PV) per MultiPV index. Reader thread only.
      let sanPvCache = Dictionary<int, struct (string * string)>()

      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let mutable backend = ""
      let optionsMap = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
      let dict = Dictionary<string, obj>()
      let nonDefaultValues = Dictionary<string, (string * string)>()
      let distribution = ResizeArray<NNValues>()
      let mutable inPolicyDistributionMode = false
      let mutable searchMoves: string list = []
      let searchMoveSuffix () =
        if searchMoves.IsEmpty then "" else " searchmoves " + (searchMoves |> String.concat " ")

      // Handshake state, flipped by the reader thread and waited on by the caller. The reader sets
      // the signal whenever one of them changes (or the engine exits), so a wait ends the moment
      // the answer arrives rather than on the next poll.
      [<VolatileField>]
      let mutable handshake = AwaitingUciOk
      // A fatal init line seen while waiting for uciok/readyok; ends the wait as a failure at once.
      [<VolatileField>]
      let mutable initFailure : string option = None
      let readySignal = new ManualResetEventSlim(false)

      let stderr = EngineProcess.StderrRing()
      [<VolatileField>]
      let mutable lastExitCode : int option = None
      // Ceres and others exit non-zero on a clean quit; only warn when we didn't ask.
      [<VolatileField>]
      let mutable shutdownRequested = false
      let transport =
        let arguments =
          if String.IsNullOrEmpty config.Args |> not then config.Args
          elif isLc0 then "--show-hidden"
          else ""
        EngineProcess.Transport(config, arguments, stderr,
          (fun msg -> logDebug $"[{name}] {msg}"),
          (fun line -> if isEnabled LogLevel.Debug then logDebug $"[STDERR {name}]: {line}"),
          (fun code ->
              if code.IsSome then lastExitCode <- code
              match code with
              | Some c when c <> 0 ->
                  if shutdownRequested then logDebug $"Engine {name} exited with code {c} after quit"
                  else logInformation $"⚠️ Engine {name} exited unexpectedly with code {c}"
              | _ -> ()
              // A wait for uciok/readyok ends now rather than on its next check.
              try readySignal.Set() with _ -> ()))
      let engineProcess = transport.Process

      let ceresNetworkName =
        match config.Options |> Seq.tryFind (fun e -> e.Key = "Network") with
        | Some net ->
            let nn = net.Value.ToString()
            if nn.Contains("/") then
              let nArr = nn.Split('/')
              nArr.[max 0 (nArr.Length - 1)]
            else ""
        | None ->
            let argsExists = isNull config.Args |> not && String.IsNullOrEmpty config.Args |> not
            if isCeres && argsExists then
              let network = config.Args.Split(':')
              if network.Length > 1 then network.[1] else ""
            else ""

      let mutable network = if isCeres then ceresNetworkName else ""

      let getAllDefaultOptions () =
        for opt in optionsMap do
          match opt.Value.OptionType with
          | UciOption.Check b -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, b)
          | UciOption.Spin (_, _, def) -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, def)
          | UciOption.Combo (_, def) -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, def)
          | UciOption.String s -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, s)
          | _ -> ()

      /// Waits for uciok ("uci") or readyok ("readyok"): true when it came, false on timeout, exit or
      /// a fatal init line.
      let waitForInitialization (timeoutMs: int) (uciMode: string) =
        let sw = Stopwatch.StartNew()
        let mutable result = ValueNone
        while result.IsNone do
          let inMode = handshake = (if uciMode = "uci" then AwaitingUciOk else AwaitingReadyOk)
          if sw.ElapsedMilliseconds >= int64 timeoutMs then
            printfn "Timeout for %s" uciMode
            result <- ValueSome false
          elif inMode && engineProcess.HasExited then
            // Dead engines never answer: fail now instead of at the (2 h) timeout.
            let code = try string engineProcess.ExitCode with _ -> "?"
            initFailure <- Some (sprintf "process exited with code %s" code)
            ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Engine %s exited (code %s) while waiting for %s" name code uciMode)
            result <- ValueSome false
          elif inMode && initFailure.IsSome then
            ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Engine %s failed to initialize while waiting for %s: %s" name uciMode initFailure.Value)
            result <- ValueSome false
          elif inMode then
            // Woken by the reader; the bound only limits what a lost wake-up could cost.
            readySignal.Wait 50 |> ignore
            readySignal.Reset()
          else
            result <- ValueSome true
        result.Value

      /// SAN for one MultiPV line. SAN conversion is the hot cost on this thread (it regenerates the
      /// legal moves for every ply), and consecutive info lines usually repeat the same variation -
      /// once a mate is proven the engine repeats it for hundreds of iterations - so the last
      /// conversion per MultiPV index is kept.
      let convertPv (mpv: int) (lan: string) =
        match sanPvCache.TryGetValue mpv with
        | true, struct (cachedLan, cachedSan) when cachedLan = lan -> cachedSan
        | _ ->
          let san = lock moveBoardLock (fun () -> getShortSanPVFromLongSanPVFast moveList &moveBoard lan)
          sanPvCache.[mpv] <- struct (lan, san)
          san

      /// The searched position, as AnalysisOutput asks about it.
      let position = AnalysisOutput.boardPosition moveBoard moveBoardLock (fun () -> whiteToMove) convertPv

      /// A new position was sent: its search must not inherit the previous one's variation.
      let clearPvCache () = newPosition <- true

      /// One line of search output, after the handshake.
      let processLine (line: string) =
        try
          if writeToConsole then printfn "%s" line
          if newPosition then
            newPosition <- false
            sanPvCache.Clear()
            output <- AnalysisOutput.withoutPv output
          let next, effects = AnalysisOutput.step name position output line
          output <- next
          for effect in effects do
            match effect with
            | AnalysisOutput.Update update -> callback update
            | AnalysisOutput.Print text -> printfn "%s" text
            | AnalysisOutput.Debug text -> if isEnabled LogLevel.Debug then logDebug text
        with ex ->
          output <- { output with Mode = AnalysisOutput.Idle }
          printfn "Error processing line from engine %s: %s" name ex.Message

      /// One line the engine printed, in protocol terms: during the handshake it is an option or
      /// the end of the list, while waiting for readyok everything else is dropped, and after that
      /// it is search output.
      let onEngineLine (line: string) =
        match handshake with
        | AwaitingUciOk ->
            if line = "uciok" then
              handshake <- Running
              readySignal.Set()
            else
              UciOption.addOptionToMap optionsMap line
        | AwaitingReadyOk ->
            if line = "readyok" then
              handshake <- Running
              readySignal.Set()
            elif not (isWinboard protocol) && EngineProcess.isFatalInitLine line then
              initFailure <- Some line
              readySignal.Set()
        | Running -> processLine line

      let write (s: string) =
        if engineProcess.HasExited then
          logger.LogCritical(sprintf "Engine %s has exited - please reset engine." name)
        else
          match protocol with
          | Winboard _ ->
              // Winboard in analysis mode: the full position is set up for every search.
              for cmd in outbound protocol true s do
                if isEnabled LogLevel.Debug then logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" s cmd name)
                logIO ">>>" cmd
                let delay = preGoDelayMs config protocol cmd
                if delay > 0 then Thread.Sleep delay
                transport.WriteLine cmd
          | Uci ->
              transport.WriteLine s
              logIO ">>>" s
              if isEnabled LogLevel.Trace then logger.LogTrace(sprintf "Writing to %s: %s" name s)

      let assignBackend (option: string) =
        if option.ToLower().Contains("backendoptions") then
          let arr = option.Split(' ')
          let startIdx = arr |> Array.findIndex (fun e -> e = "value")
          backend <- arr.[startIdx + 1 ..] |> String.concat " "

      let assignNetworkName (option: string) =
        let lower = option.ToLower()
        if lower.Contains("weights") || lower.Contains("evalfile") then
          if String.IsNullOrEmpty ceresNetworkName |> not then
            network <- ceresNetworkName
          else
            let arr = option.Split(' ')
            // QUIRK (pinned): only the last extension goes, so x.pb.gz is called x.pb.
            let n = Path.GetFileNameWithoutExtension arr.[arr.Length - 1]
            if String.IsNullOrEmpty n |> not then network <- n

      /// Written after the config's options: the analysis pages move nothing on a clock.
      let analysisCommands = [ sprintf "setoption name %s value %d" "MoveOverheadMs" 0 ]

      let setupProcess () =
          let mutable pos = moveBoard.Position
          moveBoard.IsFRC <- PositionOps.isFRC &pos
          if String.IsNullOrEmpty config.Args |> not then logDebug $"Args passed: {config.Args}"
          let onLine (line: string) =
              try
                logIO "<<<" line
                match protocol with
                | Winboard handler ->
                    if isEnabled LogLevel.Debug then logDebug $"[{name}] Received output: {line}"
                    match handler.ProcessOutput line with
                    | Some uciLine ->
                        if isEnabled LogLevel.Debug then logDebug $"[{name}] Translated to UCI: {uciLine}"
                        onEngineLine uciLine
                    | None ->
                        // Not translated - normal for some Winboard output.
                        if isEnabled LogLevel.Debug then logDebug $"[{name}] Output not translated (normal for some Winboard output)"
                | Uci -> onEngineLine line
              with ex ->
                // An exception escaping this handler is unhandled on a threadpool thread and ends
                // the whole host process.
                logError (sprintf "Error processing output line from %s: %s" name ex.Message)
          if not (transport.Start(EngineProcess.Push onLine)) then
            failwith (sprintf "Engine %s could not be started" name)

          match protocol with
          | Winboard handler ->
              logInformation $"[{name}] Starting Winboard initialization"
              // These are Winboard commands already: straight to the pipe.
              for cmd in handler.GetInitCommands() do
                logDebug $"[{name}] Sending init command: {cmd}"
                transport.WriteLine cmd
              logDebug $"[{name}] Waiting for Winboard initialization to complete..."
              let startWait = DateTime.UtcNow
              let initSuccess = initializeWinboardEventBased handler (Some logger) name 2000 (forceV1 config) |> Async.RunSynchronously
              let waitTime = (DateTime.UtcNow - startWait).TotalMilliseconds
              logInformation $"[{name}] Winboard init wait completed in {waitTime}ms, success={initSuccess}"
              if not initSuccess then failwith "Winboard engine did not initialize properly."
              // Winboard engines send no uciok.
              handshake <- Running
              for cmd in handler.GetPostInitCommands() do
                logDebug $"[{name}] Sending post-init command: {cmd}"
                transport.WriteLine cmd
          | Uci ->
              write "uci"
              let ok = waitForInitialization (int (TimeSpan.FromHours(2).TotalMilliseconds)) "uci"
              if not ok then
                failwith (sprintf "Engine %s did not respond to the uci command%s" name
                            (match initFailure with Some r -> " (" + r + ")" | None -> ""))

          for cmd in initCommands do
            match UciOption.parseSetOptionCommand cmd with
            | Some (optName, value) ->
                if UciOption.validateSetOption optionsMap (optName, value) then
                  dict.[optName] <- value
                  match UciOption.getNoneDefaultSetOption optionsMap (optName, value) with
                  | Some (n, def, v) -> nonDefaultValues.[n] <- (def, v)
                  | None -> ()
                  ConsoleUtils.printInColor ConsoleColor.Green (sprintf "The option '%s' with value '%s' is valid." optName value)
                else
                  ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." optName value)
            | None ->
                ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)
            write cmd
            assignBackend cmd
            assignNetworkName cmd
            recordCommand cmd
          for cmd in analysisCommands do
            write cmd
            recordCommand cmd
          if isCeres then network <- ceresNetworkName
          printNonDefaultValues name config.Path nonDefaultValues
          write "ucinewgame"

          // Winboard engines need no handshake after initialization.
          if not (isWinboard protocol) then
            readySignal.Reset()
            handshake <- AwaitingReadyOk
            write "isready"
            let ok = waitForInitialization (int (TimeSpan.FromHours(2).TotalMilliseconds)) "readyok"
            if not ok then
              failwith (sprintf "Engine %s did not respond to the isready command%s" name
                          (match initFailure with Some r -> " (" + r + ")" | None -> ""))

          for err in stderr.Snapshot() do
            logDebug $"[STDERR {name}]: {err}"
          let withLiveLog = optionsMap.ContainsKey("LogLiveStats")
          getAllDefaultOptions ()
          callback (EngineUpdate.Ready (name, withLiveLog))

      do setupProcess ()

      /// Snapshot rather than the live list - callers enumerate while the process may still write.
      member _.ErrorOutput = stderr.Snapshot() :> seq<string>
      member _.LastExitCode = lastExitCode
      /// Only called when a game was adjudicated because the engine died: everything the stderr
      /// buffer still holds, for the log.
      member _.GetDiagnostics() = stderr.Diagnostics(name, lastExitCode)
      member _.GetNoneDefaultSetOptions() = nonDefaultValues
      member _.GetAllDefaultOptions() = dict
      member this.IsLc0 = isLc0
      // QUIRK: set once from the field's initial value; the PolicyDistribution command sets the
      // private field, not this property.
      member val InPolicyDistributionMode = inPolicyDistributionMode with get, set
      member val IsFRC = moveBoard.IsFRC with get, set
      member _.PrintUCI() = printConfigCommands name initCommands
      member _.Network = network
      member _.Name = name
      member _.FullName = if network <> "" then $"{name} with net: {network}" else name
      member _.GetBackEnd() = backend
      member val IsReference = false with get, set
      member this.BenchmarkLC0Cmd = benchMarkLC0Cmd
      member this.ShowCommands = fun () -> printConfigCommands name initCommands
      member this.Config = config
      member this.Path = config.Path
      member this.GetUCICommands() = optionsMap
      /// Engine's self-declared identity from the UCI handshake ("id name ..."), or "" if not (yet)
      /// received. More reliable than config display Name or Path.
      member _.UciIdName = uciIdName optionsMap
      member this.ShutDownEngine() = this.SendUCICommand UCICommand.Quit
      member this.HasExited = engineProcess.HasExited

      member this.CurrentPositionCommand() = lock moveBoardLock (fun () -> moveBoard.PositionWithMovesFromGraph())

      member this.SetAllOptions (allOptions: Dictionary<string, obj>) =
        for opt in allOptions do
          let cmd =
            match Boolean.TryParse (opt.Value.ToString()) with
            | true, v -> sprintf "setoption name %s value %s" opt.Key (sprintf "%b" v)
            | _ -> sprintf "setoption name %s value %s" opt.Key (opt.Value.ToString())
          printfn "%s" cmd
          write cmd
          assignNetworkName cmd
          recordCommand cmd
          dict.[opt.Key] <- opt.Value

      member this.WaitForReadyOk(?timeoutMs: int) =
          if engineProcess.HasExited then false
          else
            match protocol with
            | Winboard _ -> true
            | Uci ->
                readySignal.Reset()
                handshake <- AwaitingReadyOk
                write "isready"
                waitForInitialization (defaultArg timeoutMs (int (TimeSpan.FromHours(2).TotalMilliseconds))) "readyok"

      member this.SendUCICommand (command: UCICommand) =
          match command with
          | UCI -> write "uci"
          | Stop ->
              try
                if not engineProcess.HasExited then write "stop"
              with _ ->
                // The engine has closed the pipe already - expected during shutdown.
                printfn "%s" (sprintf "Engine %s pipe already closed it seems" name)
                this.GetDiagnostics() |> printfn "%s"
          | Quit ->
              shutdownRequested <- true
              try
                if not engineProcess.HasExited then write "quit"
                if not (engineProcess.WaitForExit 1000) then engineProcess.Kill()
              with
              | :? IOException as ex when ex.Message.Contains("pipe") ->
                  printfn "%s" (sprintf "Engine %s pipe already closed during shutdown" name)
              | :? ObjectDisposedException ->
                  printfn "%s" (sprintf "Engine %s process already disposed during shutdown" name)
              | _ ->
                  printfn "%s" (sprintf "Error during engine %s shutdown" name)
              try
                engineProcess.Close()
                engineProcess.Dispose()
                ioLog |> Option.iter (fun log -> (log :> IDisposable).Dispose())
                printfn "Engine %s has been shut down." name
              with :? ObjectDisposedException ->
                this.GetDiagnostics() |> printfn "%s"
          | RawCommand cmd ->
              write cmd
              recordCommand cmd
          | PositionWithMoves command ->
              // QUIRK (pinned): the board below understands only "position fen ... moves ...";
              // a startpos command leaves it at the start. Every caller sends the fen form.
              let isFrc =
                lock moveBoardLock (fun () ->
                  moveBoard.ResetBoardState()
                  clearPvCache ()
                  moveBoard.PlayCommands command
                  whiteToMove <- moveBoard.Position.STM = 0uy
                  moveBoard.IsFRC)
              if isFrc && not chess960Sent then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif chess960Sent && not isFrc then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              write command
          | Position fen ->
              let isFrc =
                lock moveBoardLock (fun () ->
                  moveBoard.LoadFen fen
                  clearPvCache ()
                  whiteToMove <- moveBoard.Position.STM = 0uy
                  moveBoard.IsFRC)
              if isFrc then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif chess960Sent && not isFrc then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              // An EPD with no counters is rejected by Lc0 when it has an en-passant square.
              write (sprintf "position fen %s" (Chess.Board.UciFen fen))
          | GoNodes nodes -> write (sprintf "go nodes %d%s" nodes (searchMoveSuffix ()))
          | GoInfinite -> write (sprintf "go infinite%s" (searchMoveSuffix ()))
          | GoMoveTime timeInMs -> write (sprintf "go movetime %d%s" timeInMs (searchMoveSuffix ()))
          | GoValue -> write "go value"
          | GoTimeControl (tc, wTime, bTime) -> write (TimeControlCommands.uciTimeCommand tc wTime bTime + searchMoveSuffix ())
          | UciNewGame ->
              clearPvCache ()
              write "ucinewgame"
          | SetOption option ->
              let cmd = sprintf "setoption name %s value %s" option.Name option.Value
              write cmd
              assignNetworkName cmd
              recordCommand cmd
          | SetOptions options ->
              for option in options do
                let cmd = sprintf "setoption name %s value %s" option.Name option.Value
                write cmd
                assignNetworkName cmd
                recordCommand cmd
          | PolicyDistribution _ ->
              distribution.Clear()
              inPolicyDistributionMode <- true
          | SetMoveOverhead (optionName, ms) ->
              match UciOption.tryFindOption optionsMap optionName with
              | Some option ->
                  match option.OptionType with
                  | UciOption.Spin (min, max, _) ->
                      let intValue = int64 ms
                      if intValue >= min && intValue <= max then
                        write (sprintf "setoption name %s value %d" option.Name intValue)
                  | _ -> ()
              | None -> printfn "Option not found: %s value: %d" optionName ms

      member this.SetSearchMoves (moves: string list) = searchMoves <- moves
      member this.ClearSearchMoves () = searchMoves <- []
      member this.SearchMoves with get() = searchMoves

  // ════════════════════════════════════════════════════════════════════════════════════════════
  //  The tournament wrapper: the caller writes commands and reads the replies itself
  // ════════════════════════════════════════════════════════════════════════════════════════════

  type ChessEngine(config : EngineConfig, initCommands: string seq, logger: ILogger option) =
      let name = config.Name
      let isEnabled level = match logger with Some l -> l.IsEnabled level | None -> false
      let logCritical (text: string) = match logger with Some l -> l.LogCritical text | None -> ()
      let logInformation (text: string) = match logger with Some l -> l.LogInformation text | None -> ()
      let logDebug (text: string) = match logger with Some l -> l.LogDebug text | None -> ()

      let initialCommands = initCommands |> ResizeArray
      /// 12 minutes: some engines take a long time to answer isready after their options are set.
      let defaultTimeoutMs = 180000 * 4
      let mutable passed = true
      let isLc0 = EngineProcess.pathMentions "lc0" config
      let isCeres = EngineProcess.pathMentions "ceres" config
      let mutable network = String.Empty
      let protocol = protocolFor config logger
      let optionsMap = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let commands = ResizeArray<string>()
      let nonDefaultValues = Dictionary<string, (string * string)>()
      let stderr = EngineProcess.StderrRing()
      [<VolatileField>]
      let mutable lastExitCode : int option = None
      // Ceres and others exit non-zero on a clean quit; only warn when we didn't ask.
      [<VolatileField>]
      let mutable shutdownRequested = false
      let mutable isRunning = false
      let mutable validate = true
      // Why the last WaitForReadyOk or WarmUp failed ("" after a successful one). Callers (cmp,
      // analyze) have no logger, so the reason travels with the engine.
      let mutable readyFailure = ""
      // The running process and what has been done to it; null until the first start.
      let mutable running : RunningEngine = null
      // Counts starts, so an exit event from a process already replaced is ignored.
      [<VolatileField>]
      let mutable startGeneration = 0
      let proc () = if isNull running then null else running.Process
      let assignNetworkName (option: string) =
        if isCeres && not (String.IsNullOrEmpty config.Args) then
          let ceresNet = config.Args.Split(':')
          if ceresNet.Length > 1 then network <- ceresNet.[1]
        let lower = option.ToLower()
        if lower.Contains("weights") || lower.Contains("network") || lower.Contains("evalfile") then
          let arr = option.Split(' ')
          // QUIRK (pinned): only the last extension goes, so x.pb.gz is called x.pb.
          let n = Path.GetFileNameWithoutExtension arr.[arr.Length - 1]
          if String.IsNullOrEmpty n |> not then network <- n

      let addCommand (list: ResizeArray<string>) (cmd: string) =
        if not (list.Contains cmd) then list.Add cmd

      let getAllDefaultOptions () =
        let d = Dictionary<string, obj>()
        for opt in optionsMap do
          match opt.Value.OptionType with
          | UciOption.Check b -> d.Add(opt.Key, b)
          | UciOption.Spin (_, _, def) -> d.Add(opt.Key, def)
          | UciOption.Combo (_, def) -> d.Add(opt.Key, def)
          | UciOption.String s -> d.Add(opt.Key, s)
          | _ -> ()
        d

      /// The config's setoption commands with each option name in the engine's own spelling.
      let createVerifiedOptions (options: string seq) =
        [ for opt in options do
            match UciOption.parseSetOptionCommand opt with
            | Some (optName, value) ->
                match optionsMap.TryGetValue optName with
                | true, o -> sprintf "setoption name %s value %s" o.Name value
                | false, _ -> opt
            | None -> opt ]

      /// Waits up to `timeoutMs` for the engine to go, then kills it; always releases the handle.
      let terminateProcess (p: Process) (timeoutMs: int) =
        try
          if not (p.WaitForExit timeoutMs) then
            try p.Kill()
            with ex -> printfn "Warning: could not kill '%s': %s" name ex.Message
            p.WaitForExit 5_000 |> ignore
        finally
          p.Close()
          p.Dispose()

      /// Starts the process. Process.Start does not block on the engine, so this runs on the
      /// caller's thread; it used to be handed to a pool thread and waited for, which only cost a
      /// thread.
      let assignThread () =
          let arguments =
            if not (String.IsNullOrEmpty config.Args) then
              if isLc0 && not (config.Args.Contains("--show-hidden")) then config.Args + " --show-hidden"
              else config.Args
            elif isLc0 then "--show-hidden"
            else ""
          // A restart is a new process: its exit is unexpected again, and it has no exit code yet
          // (the old one's code used to be reported for it). A process that has already exited is
          // let go here; one stopped through StopProcess was disposed there.
          if not (isNull running) then
            try if running.Process.HasExited then running.Process.Dispose() with _ -> ()
          startGeneration <- startGeneration + 1
          let generation = startGeneration
          lastExitCode <- None
          shutdownRequested <- false
          let t =
            EngineProcess.Transport(config, arguments, stderr,
              (fun msg -> printfn "[%s] %s" name msg),
              (fun line -> printfn "[STDERR %s]: %s" name line),
              (fun code ->
                if generation = startGeneration then
                  if code.IsSome then lastExitCode <- code
                  match code with
                  | Some c when c <> 0 ->
                      if shutdownRequested then logDebug (sprintf "Engine %s exited with code %d after quit" name c)
                      // printfn: visible even where logging is filtered.
                      else printfn "⚠️ Engine %s exited unexpectedly with code %d" name c
                  | _ -> ()))
          running <- RunningEngine t
          // Output is read by the caller (ReadLine*), not by events.
          if not (t.Start EngineProcess.Pull) then printfn "\n❌ %s could not be started" name

      let hasExited () =
        try isNull running || running.Process.HasExited
        with _ -> true

      /// The exit code, read from the process when the Exited event has not delivered it yet.
      let exitCodeText () =
        match lastExitCode with
        | Some c -> string c
        | None -> if isNull running then "?" else running.Transport.ExitCodeText()

      /// After a read returned null: the engine closed its output, which it does as it exits. The
      /// exit can lag the closed pipe by a moment; wait for it so the reason names the exit
      /// instead of guessing (this used to report "output closed" or nothing at all on Linux).
      let exitedAfterEndOfOutput () = not (isNull running) && running.Transport.ExitedAfterEndOfOutput()

      let write (s: string) =
        try
          if not (hasExited ()) then
            match protocol with
            | Winboard _ ->
                let logMsg = if isEnabled LogLevel.Debug then shortForLog s else ""
                for cmd in outbound protocol false s do
                  if isEnabled LogLevel.Debug then logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" logMsg cmd name)
                  let delay = preGoDelayMs config protocol cmd
                  if delay > 0 then Thread.Sleep delay
                  running.Transport.WriteLine cmd
            | Uci ->
                if isEnabled LogLevel.Trace then logger.Value.LogTrace(sprintf "Writing to %s: %s" name s)
                running.Transport.WriteLine s
          else
            printfn "Warning: Attempted to write to disposed engine %s" name
        with ex ->
          printfn "Error writing to engine %s: %s" name ex.Message

      let read () = running.Process.StandardOutput.ReadLine()
      let readAsync () = running.Process.StandardOutput.ReadLineAsync()

      /// The next line (Winboard output translated), or null on cancellation, end of stream or a
      /// read error.
      ///
      /// Every await in this class is ConfigureAwait(false). The synchronous members block on
      /// these tasks, and a continuation sent back to a blocked caller's context (the Blazor
      /// dispatcher) would never run - the call would hang for good.
      let readAsyncWithTimeout (token: CancellationToken) =
        task {
          try
            let! line = running.Process.StandardOutput.ReadLineAsync(token).ConfigureAwait(false)
            return inboundOrRaw protocol line
          with
          | :? OperationCanceledException -> return null
          // StreamReader can throw this when the underlying stream is closed or cancelled.
          | :? ArgumentOutOfRangeException -> return null
          | :? IOException as ioex ->
              logCritical (sprintf "IO error reading engine output: %s" ioex.Message)
              return null
          | ex ->
              // Unexpected: log it and end the read rather than crash the host.
              logCritical (sprintf "Unexpected error reading engine output: %s" ex.Message)
              return null
        }

      let getDiagnostics () = stderr.Diagnostics(name, lastExitCode)

      /// Reads to readyok, skipping anything else still queued (the tail of a search). False, with
      /// the reason in ReadyFailure, on a timeout, an exit or a fatal init line; throws
      /// OperationCanceledException when `cancel` fires.
      let readUntilReady (timeoutMs: int) (cancel: CancellationToken) : Task<bool> =
        task {
          use cts = CancellationTokenSource.CreateLinkedTokenSource cancel
          cts.CancelAfter timeoutMs
          let fail (reason: string) =
            readyFailure <- reason
            logCritical (sprintf "Engine %s: %s" name reason)
            false
          let timedOut () =
            cancel.ThrowIfCancellationRequested()
            fail (sprintf "timeout after %d ms waiting for readyok" timeoutMs)
          try
            let mutable result = ValueNone
            while result.IsNone do
              if hasExited () then
                result <- ValueSome (fail (sprintf "exited (code %s) while waiting for readyok" (exitCodeText ())))
              elif cts.IsCancellationRequested then
                result <- ValueSome (timedOut ())
              else
                let! line = (readAsyncWithTimeout cts.Token).ConfigureAwait(false)
                if isNull line then
                  result <-
                    ValueSome (
                      if cts.IsCancellationRequested then timedOut ()
                      elif exitedAfterEndOfOutput () then
                        fail (sprintf "exited (code %s) while waiting for readyok" (exitCodeText ()))
                      else fail "output closed while waiting for readyok")
                elif line = "readyok" then
                  // Every caller lands here; GameInitialization logs the milestone.
                  readyFailure <- ""
                  logDebug (sprintf "Engine %s responded with readyok" name)
                  result <- ValueSome true
                elif EngineProcess.isFatalInitLine line then
                  // Alive but given up (Ceres after a refused net): no readyok will ever come, so
                  // do not sit out the timeout.
                  result <- ValueSome (fail (sprintf "reported a fatal initialization error: %s" line))
            return result.Value
          with ex when not cancel.IsCancellationRequested ->
            return fail (sprintf "error while waiting for readyok: %s" ex.Message)
        }

      /// Reads to a bestmove: Some (Ok line), Some (Error reason) when waiting longer is pointless
      /// (exit, fatal line), None on timeout or when the output could not be read from an engine
      /// still running. Throws OperationCanceledException when `cancel` fires.
      let readUntilBestmove (timeoutMs: int) (cancel: CancellationToken) : Task<Result<string, string> option> =
        task {
          use cts = CancellationTokenSource.CreateLinkedTokenSource cancel
          cts.CancelAfter timeoutMs
          let mutable result = ValueNone
          while result.IsNone do
            let! line = (readAsyncWithTimeout cts.Token).ConfigureAwait(false)
            if isNull line then
              cancel.ThrowIfCancellationRequested()
              result <-
                ValueSome (
                  if cts.IsCancellationRequested then None
                  elif exitedAfterEndOfOutput () then
                    Some (Error (sprintf "exited (code %s) during the warm-up search" (exitCodeText ())))
                  else
                    // A null from a live engine is not proof of failure: readAsyncWithTimeout also
                    // returns null on a read error (logged there). Treated like a timeout, so the
                    // search is stopped and drained and the isready that follows decides; a stdout
                    // that is really closed fails there with its own reason.
                    None)
            elif line.StartsWith("bestmove", StringComparison.Ordinal) then result <- ValueSome (Some (Ok line))
            // Same early exit as readUntilReady: Ceres after a refused net stays alive but will
            // never search.
            elif EngineProcess.isFatalInitLine line then
              result <- ValueSome (Some (Error (sprintf "reported a fatal initialization error: %s" line)))
          return result.Value
        }

      /// The handshake: uci ... uciok for a UCI engine, feature negotiation for a Winboard one.
      let readUciOptionsAsync () : Task<bool> =
        task {
          match protocol with
          | Winboard handler ->
              return! Async.StartAsTask(initializeWinboard running.Process handler logger name 30000 (forceV1 config)).ConfigureAwait(false)
          | Uci ->
              use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float 120000))
              write "uci"
              let! line = (readAsyncWithTimeout cts.Token).ConfigureAwait(false)
              if isNull line then
                logCritical (sprintf "Engine %s: read returned null while waiting for UCI options" name)
                return false
              else
                printfn "%s" line
                let mutable ret = line
                while ret <> "uciok" && not (isNull ret) && not running.Process.HasExited do
                  UciOption.addOptionToMap optionsMap ret
                  let! resp = (readAsyncWithTimeout cts.Token).ConfigureAwait(false)
                  ret <- resp
                let isOk = ret = "uciok"
                if not isOk then logCritical (sprintf "Engine %s did not respond with uciok" name)
                else logInformation (sprintf "Engine %s responded with uciok" name)
                return isOk
        }

      let readUciOptions () =
        try readUciOptionsAsync().GetAwaiter().GetResult()
        with
        | :? OperationCanceledException ->
            logCritical (sprintf "|||||Timeout after %d ms in ReadUci |||||" 120000)
            false
        | :? IOException as ex ->
            logCritical (sprintf "Error reading UCI options: %s" ex.Message)
            false
        | ex ->
            logCritical (sprintf "An unexpected error occurred while reading UCI options for %s: \n%s" name ex.Message)
            false

      let startProcess () =
        try
          assignThread ()
          if not (readUciOptions ()) then
            // Winboard: initializeWinboard only returns false when the process EXITED during init
            // or a hard I/O failure occurred - a healthy-but-quiet v1 engine never lands here.
            let msg =
              match protocol with
              | Winboard _ -> sprintf "Winboard engine %s exited or failed during initialization (path: %s)" name config.Path
              | Uci -> sprintf "Engine %s did not respond to the uci command (path: %s)" name config.Path
            logCritical msg
            raise (CustomException.EngineStartupException msg)
          if not (hasExited ()) then
            logDebug (sprintf "Engine %s is already running." name)
          else
            assignThread ()
            logDebug (sprintf "Engine %s started successfully." name)
          for cmd in createVerifiedOptions initCommands do
            match UciOption.parseSetOptionCommand cmd with
            | Some (optName, value) ->
                let valid = UciOption.validateSetOption optionsMap (optName, value)
                if valid then
                  match UciOption.getNoneDefaultSetOption optionsMap (optName, value) with
                  | Some (n, def, v) -> nonDefaultValues.[n] <- (def, v)
                  | None -> ()
                // QUIRK (pinned): `validate` is still true here even for an engine made by
                // createEngineWithoutValidation, which turns it off after the constructor.
                if validate && not valid then
                  passed <- false
                  ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." optName value)
            | None ->
                passed <- false
                ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)
            write cmd
            assignNetworkName cmd
            commands.Add cmd
          match optionsMap.TryGetValue "name", optionsMap.TryGetValue "author" with
          | (true, nameOpt), (true, authorOpt) ->
              match nameOpt.OptionType, authorOpt.OptionType with
              // Engines restart every game; the version is worth one line per run.
              | UciOption.IdAndAuthor(_, _, n), UciOption.IdAndAuthor(_, _, a) ->
                  if lock printLock (fun () -> loggedVersions.Add name)
                  then logInformation (sprintf "Engine %s is %s by %s" name n a)
                  else logDebug (sprintf "Engine name: %s and author: %s" n a)
              | _ -> ()
          | _ -> ()
          if validate then
            if passed then
              lock printLock (fun () ->
                ConsoleUtils.printInColor ConsoleColor.Green (sprintf "All setoptions passed validation for %s" name)
                if printedEngines.Add(name) then
                  printNonDefaultValues name config.Path nonDefaultValues
                elif not (String.IsNullOrEmpty config.DeviceOption) then
                  match nonDefaultValues.TryGetValue(config.DeviceOption) with
                  | true, (_, value) -> printfn "  %s: %s = %s" name config.DeviceOption value
                  | _ -> ()
                let diagnostics = getDiagnostics ()
                if String.IsNullOrEmpty diagnostics |> not then
                  ConsoleUtils.printInColor ConsoleColor.DarkYellow (sprintf "Engine diagnostics: %s" diagnostics))
            else
              // The offending options were printed in red above.
              ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Some setoptions did not pass validation (check for red lines in console) for %s" name)
        with
        | :? CustomException.EngineStartupException ->
            // Fail fast (user decision 2026-08-08): a binary that never answers "uci" fails at
            // creation with a clear message instead of limping into readyok timeouts
            // mid-tournament. The started-but-mute process is killed so it does not leak, and every
            // creation path catches the exception. Option validation failures stay non-throwing.
            passed <- false
            try if not (hasExited ()) then running.Process.Kill(true) with _ -> ()
            reraise ()
        | :? OperationCanceledException ->
            passed <- false
            logCritical "Engine initialization timed out."
        | :? Channels.ChannelClosedException ->
            passed <- false
            logCritical "Engine channel was closed unexpectedly."
        | ex ->
            passed <- false
            logCritical (sprintf "An unexpected error occurred while starting engine %s: \n%s" name ex.Message)

      do startProcess ()

      member _.GetExitCode() = lastExitCode
      /// Snapshot rather than the live list - callers enumerate while the process may still write.
      member _.ErrorOutput = stderr.Snapshot() :> seq<string>
      member _.LastExitCode = lastExitCode
      member _.GetDiagnostics() = getDiagnostics ()
      member _.GetVerifiedCommands() = createVerifiedOptions initCommands
      member _.InitCommands = initCommands
      member _.Process = proc ()
      member _.PrintNonDefaultValues = fun () -> printNonDefaultValues name config.Path nonDefaultValues
      member _.IsLc0 = isLc0
      member _.Write (s: string) = write s
      member _.Commands = commands
      member _.PrintUCI() = printConfigCommands name initCommands
      member _.Network = network
      member val Name = name with get, set
      member _.FullName = if network <> "" then $"{name} with net: {network}" else name
      member val IsReference = false with get, set
      /// True if the Winboard engine supports reuse (per CECP, defaults to true). Always true for UCI.
      member _.CanReuseWinboard = canReuse protocol
      member this.GetDefaultOptions() = getAllDefaultOptions ()

      /// QUIRK (pinned): the LAST option whose name contains `name` wins, so "hash" finds
      /// "Clear Hash" when the engine lists it after "Hash".
      member this.TryToUpdateOption (name: string) (value: string) =
        let mutable option = String.Empty
        for opt in optionsMap do
          if opt.Key.ToLower().Contains (name.ToLower()) then option <- opt.Key
        match optionsMap.TryGetValue option with
        | true, opt -> this.AddSetOption (EngineOption.Create opt.Name value)
        | false, _ -> ()

      member this.AddSetOptions (config: EngineOption array) =
        this.Stop()
        for option in config do this.AddSetOption option

      member this.AddSetOption (option: EngineOption) =
        match UciOption.tryFindOption optionsMap option.Name with
        | Some opt ->
            this.Stop()
            let cmd = sprintf "setoption name %s value %s" opt.Name option.Value
            write cmd
            this.Config.Options.[option.Name] <- option.Value
            assignNetworkName cmd
            addCommand initialCommands cmd
            addCommand commands cmd
        | _ -> logDebug (sprintf "Option not found for engine %s: %s" name option.Name)

      member _.SetMoveOverhead (optionName: string, milliSeconds: int) =
        let notFound () = logInformation (sprintf "Option not found or value not valid for engine %s: %s value: %d" name optionName milliSeconds)
        match optionsMap.Keys |> Seq.tryFind (fun e -> e.ToLower().Contains(optionName.ToLower())) with
        | Some optName ->
            match UciOption.tryFindOption optionsMap optName with
            | Some option ->
                match option.OptionType with
                | UciOption.Spin (min, max, _) ->
                    let intValue = int64 milliSeconds
                    if intValue >= min && intValue <= max then
                      match config.Options |> Seq.tryFind (fun e -> e.Key = optName) with
                      | Some _ -> ()   // the def sets it; leave it alone
                      | None when running.MoveOverheadSent = Some intValue ->
                          // Already set on this process. The GUI calls this before every game, and
                          // Ceres rebuilds its TensorRT engine on each MoveOverheadMs setoption.
                          ()
                      | None ->
                          let cmd = sprintf "setoption name %s value %d" option.Name intValue
                          write cmd
                          running.MoveOverheadSent <- Some intValue
                          addCommand initialCommands cmd
                          addCommand commands cmd
                | _ -> ()
            | None -> notFound ()
        | None -> notFound ()

      member this.StartProcess() = startProcess ()

      member this.DoNotValidate() = validate <- false

      /// Ends the process without sending quit: three seconds to go on its own, then killed.
      member this.StopProcess() =
        shutdownRequested <- true   // as deliberate as quit; the exit is not unexpected
        try
          if running.Process.HasExited then logInformation (sprintf "Engine %s has already exited" name)
          else terminateProcess running.Process 3000
        with :? InvalidOperationException ->
          logCritical (sprintf "Engine %s: process was never started or is in a bad state — check engine path, permissions, and dependencies" name)

      member _.PassedValidation = passed
      member this.BenchmarkLC0Cmd = benchMarkLC0Cmd
      member this.ShowCommands = fun () -> printConfigCommands name initCommands
      member this.Config = config
      member this.Path = config.Path
      /// QUIRK: sends "uci" and returns nothing, unlike the analysis wrapper's GetUCICommands.
      member this.GetUCICommands() = this.Uci()
      member _.GetOptionsMap() = optionsMap
      /// Engine's self-declared identity from the UCI handshake ("id name ..."), or "" if not (yet)
      /// received. More reliable than config display Name or Path.
      member _.UciIdName = uciIdName optionsMap

      member private _.Send (cmd: string) =
        write cmd
        commands.Add cmd

      member this.Uci() = this.Send "uci"
      member this.IsReady() = this.Send "isready"
      member this.UciNewGame() = this.Send "ucinewgame"
      member this.Position(position: string) = this.Send position
      member this.PositionGoFen (fen: string) = this.Send (sprintf "position fen %s" (Chess.Board.UciFen fen))

      member this.Analyse(fenPosition: string) =
        this.UciNewGame()
        this.Send (sprintf "position fen %s" (Chess.Board.UciFen fenPosition))
        this.Go(100)

      member this.Go(timeInMs: int) =
        isRunning <- true
        this.Send (sprintf "go movetime %d" timeInMs)

      member this.Go (timeControl: UnionType, wTime, bTime) =
        isRunning <- true
        this.Send (TimeControlCommands.uciTimeCommand timeControl wTime bTime)

      member this.GoNodes (nodes: int) =
        isRunning <- true
        this.Send (TimeControlCommands.createNodes nodes)

      member this.GoValue () =
        isRunning <- true
        this.Send "go value"

      member this.Stop() =
        isRunning <- false
        this.Send "stop"

      member this.Quit() =
        shutdownRequested <- true
        this.Send "quit"

      member this.PonderHit() = this.Send "ponderhit"

      /// The full ponder command ("go wtime ... ponder"), written as given.
      member this.GoPonder command = this.Send command

      member this.ReadLineAsync() = readAsync ()
      member this.ReadLineAsyncWithTimeout(token: CancellationToken) = readAsyncWithTimeout token
      member this.ReadLine() = read ()

      member this.ReadUciOptions() = readUciOptions ()

      /// The async forms take .NET optional parameters, so C# can leave them out too: a timeout
      /// of 0 means the default, and the CancellationToken is optional. A cancelled wait throws
      /// OperationCanceledException and records nothing in ReadyFailure; whatever the engine
      /// still sends (its readyok, a bestmove) is skipped by the next WaitForReadyOk, which reads
      /// up to its own readyok. The synchronous forms wait for the same task.
      member this.WaitForReadyOkAsync([<Optional; DefaultParameterValue(0)>] timeoutMs: int,
                                      [<Optional>] cancellationToken: CancellationToken) : Task<bool> =
        let cancel = cancellationToken
        match protocol with
        | Winboard handler when not handler.Features.Ping ->
            logInformation (sprintf "Engine %s is using Winboard protocol without ping support, assuming ready" name)
            Task.FromResult true
        | Winboard _ ->
            // Winboard v2 with ping: these engines answer promptly or not at all - the long
            // NN-engine timeout does not apply.
            task {
              write "isready"
              let! ok = (readUntilReady 1000 cancel).ConfigureAwait(false)
              if not ok then
                // No pong: assume ready rather than block the game start.
                logDebug (sprintf "Engine %s did not respond to ping/isready; assuming ready" name)
              return true
            }
        | Uci ->
            // The high floor is intentional for UCI engines: neural-net engines can spend many
            // minutes building a TRT profile on their first init.
            let timeoutInMs = if timeoutMs < 60000 then defaultTimeoutMs else timeoutMs
            write "isready"
            readUntilReady timeoutInMs cancel

      member this.WaitForReadyOk(?timeoutMs: int) =
        this.WaitForReadyOkAsync(defaultArg timeoutMs 0).GetAwaiter().GetResult()

      /// Why the last WaitForReadyOk or WarmUp failed; "" when it succeeded.
      member _.ReadyFailure = readyFailure

      /// One `go nodes 1` from the start position, once per process, before the first game. Some
      /// engines answer readyok at once and only load their network on the first search: Lc0 0.33
      /// with onnx-trt spent 5.5 s on its first move of a 5s+0.3s game and lost it on time, and
      /// the bestmove it sent afterwards was read as its reply in the next game. The result is
      /// discarded; false means no bestmove came within the timeout, or the engine failed (then
      /// ReadyFailure says why). Callers send ucinewgame + isready afterwards either way, unless
      /// ReadyFailure is set. A cancelled warm-up stops the search before it throws.
      member this.WarmUpAsync(timeoutMs: int, [<Optional>] cancellationToken: CancellationToken) : Task<bool> =
        let cancel = cancellationToken
        if isWinboard protocol || this.HasExited() || running.WarmedUp then Task.FromResult true
        else
          running.WarmedUp <- true
          readyFailure <- ""
          task {
            let sw = Stopwatch.StartNew()
            let fail reason =
              readyFailure <- reason
              logCritical (sprintf "Engine %s: %s" name reason)
            write "position startpos"
            write "go nodes 1"
            let readFirst () =
              task {
                try return! (readUntilBestmove timeoutMs cancel).ConfigureAwait(false)
                with :? OperationCanceledException as ex when cancel.IsCancellationRequested ->
                  write "stop"
                  return raise ex
              }
            let! first = readFirst().ConfigureAwait(false)
            match first with
            | Some (Ok line) ->
                printfn "Engine %s warm-up: %s after %d ms" name line sw.ElapsedMilliseconds
                return true
            | Some (Error reason) ->
                // No search is running, so there is nothing to stop or drain.
                fail reason
                return false
            | None ->
                logCritical (sprintf "Engine %s: no bestmove within %d ms of the warm-up search" name timeoutMs)
                // A search still running would answer into the first game: stop it and consume its
                // bestmove here. An engine that has died has nothing left to drain.
                if not (this.HasExited()) then
                  write "stop"
                  let! drained = (readUntilBestmove 10000 cancel).ConfigureAwait(false)
                  match drained with
                  | Some (Ok _) -> ()
                  | Some (Error reason) -> fail reason
                  | None -> logCritical (sprintf "Engine %s: no bestmove after stop of the warm-up search" name)
                return false
          }

      member this.WarmUp(timeoutMs: int) = this.WarmUpAsync(timeoutMs).GetAwaiter().GetResult()

      /// The UCI steps before a game, shared by the console pool (EngineHelper.initEngine) and the
      /// GUI (GameInitialization): the once-per-process warm-up, then ucinewgame + isready. The
      /// warm-up gets at least the 12-minute default, since a first TensorRT build can take longer
      /// and a timed-out warm-up is not retried. The isready wait runs whether or not the warm-up
      /// got its bestmove, unless it failed for good (exit, fatal line): then no readyok will come.
      /// False leaves the reason in ReadyFailure. Winboard engines are left alone; their init is
      /// the protocol handler's.
      member this.PrepareNewGameAsync([<Optional; DefaultParameterValue(0)>] readyTimeoutMs: int,
                                      [<Optional>] cancellationToken: CancellationToken) : Task<bool> =
        let cancel = cancellationToken
        if isWinboard protocol then Task.FromResult true
        else
          let readyTimeoutMs = if readyTimeoutMs <= 0 then defaultTimeoutMs else readyTimeoutMs
          task {
            let! warmed = this.WarmUpAsync(max defaultTimeoutMs readyTimeoutMs, cancel).ConfigureAwait(false)
            if not warmed && readyFailure <> "" then return false
            else
              this.UciNewGame()
              let! ready = this.WaitForReadyOkAsync(readyTimeoutMs, cancel).ConfigureAwait(false)
              return ready
          }

      member this.PrepareNewGame(?readyTimeoutMs: int) =
        this.PrepareNewGameAsync(defaultArg readyTimeoutMs 0).GetAwaiter().GetResult()

      member this.IsRunning
        with get () = isRunning
        and set (v) = isRunning <- v

      member this.HasExited() = hasExited ()
