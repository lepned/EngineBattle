namespace ChessLibrary

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
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

/// The tournament engine wrapper. ChessEngine runs an engine in tournaments, the tuner, the
/// console tools and the puzzle runner: the caller writes commands and pulls the replies itself.
/// The analysis pages and Game Review use AnalysisEngine (AnalysisEngine.fs).
///
/// Commands are UCI-shaped; a Winboard engine gets them translated (EngineWire). The process, its
/// stderr and the I/O log are EngineProcess's.
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

  let internal printNonDefaultValues (name: string) (path: string) (nonDefaultValues: Dictionary<string, (string * string)>) =
    printfn "\nCustomized SetOptions for %s:\n" name
    ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
    for opt in nonDefaultValues do
      let (def, value) = opt.Value
      if String.IsNullOrEmpty def then
        ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
      else
        ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
    printfn ""

  let internal printConfigCommands (name: string) (initCommands: string seq) =
    printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
    for cmd in initCommands do
      if cmd.Contains "setopt" then
        printfn "%s" cmd

  /// Engine's self-declared identity from the UCI handshake ("id name ..."), or "".
  let internal uciIdName (optionsMap: Dictionary<string, UciOption.UciOption>) =
    match UciOption.tryFindOption optionsMap "name" with
    | Some { OptionType = UciOption.UciOptionType.IdAndAuthor(_, _, value) } -> value
    | _ -> ""

  /// The process a ChessEngine is running now, and what has been done to it. A restart makes a new
  /// one, so a warm-up or a MoveOverheadMs sent to the old process never counts for the new one.
  [<AllowNullLiteral>]
  type private RunningEngine(transport: EngineProcess.Transport, output: Channel<struct (int64 * string)>) =
    member _.Transport = transport
    member _.Process = transport.Process
    /// The engine's stdout, line by line with when each was read (EngineProcess.Stamped).
    member _.Output = output
    /// The once-per-process `go nodes 1` has run (ChessEngine.WarmUp).
    member val WarmedUp = false with get, set
    /// The MoveOverheadMs value this process has been sent.
    member val MoveOverheadSent : int64 option = None with get, set

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
        if isCeres then
          let ceresNet = EngineStartup.ceresNetwork config
          if ceresNet <> "" then network <- ceresNet
        EngineStartup.networkIn option |> Option.iter (fun n -> network <- n)

      let addCommand (list: ResizeArray<string>) (cmd: string) =
        if not (list.Contains cmd) then list.Add cmd

      let getAllDefaultOptions () =
        let d = Dictionary<string, obj>()
        for key, value in EngineStartup.defaults optionsMap do d.Add(key, value)
        d

      /// The config's setoption commands with each option name in the engine's own spelling.
      let createVerifiedOptions (options: string seq) =
        [ for opt in options -> EngineStartup.inEngineSpelling optionsMap opt ]

      /// Waits up to `timeoutMs` for the engine to go, then kills it; always releases the handle.
      let terminateProcess (p: Process) (timeoutMs: int) =
        try
          if not (p.WaitForExit timeoutMs) then
            try p.Kill true
            with ex -> printfn "Warning: could not kill '%s': %s" name ex.Message
            p.WaitForExit 5_000 |> ignore
        finally
          p.Close()
          p.Dispose()

      /// Starts the process. Process.Start does not block on the engine, so this runs on the
      /// caller's thread; it used to be handed to a pool thread and waited for, which only cost a
      /// thread.
      let assignThread () =
          let arguments = EngineStartup.arguments config isLc0
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
              (fun line -> printfn "[STDERR %s]: %s" name (EngineProcess.stripAnsi line)),
              (fun code ->
                if generation = startGeneration then
                  if code.IsSome then lastExitCode <- code
                  match code with
                  | Some c when c <> 0 ->
                      if shutdownRequested then logDebug (sprintf "Engine %s exited with code %d after quit" name c)
                      // printfn: visible even where logging is filtered.
                      else printfn "⚠️ Engine %s exited unexpectedly with code %d" name c
                  | _ -> ()))
          let output = Channel.CreateUnbounded<struct (int64 * string)>()
          running <- RunningEngine(t, output)
          if not (t.Start (EngineProcess.Stamped output.Writer)) then printfn "\n❌ %s could not be started" name

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
                outbound protocol false s |> List.iteri (fun i cmd ->
                  if isEnabled LogLevel.Debug then logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" logMsg cmd name)
                  let delay = lineDelayMs config protocol i cmd
                  if delay > 0 then Thread.Sleep delay
                  running.Transport.WriteLine cmd)
            | Uci ->
                if isEnabled LogLevel.Trace then logger.Value.LogTrace(sprintf "Writing to %s: %s" name s)
                running.Transport.WriteLine s
          else
            printfn "Warning: Attempted to write to disposed engine %s" name
        with ex ->
          printfn "Error writing to engine %s: %s" name ex.Message

      /// The next raw line and the Stopwatch timestamp of its reading; null (stamp 0) when the
      /// output has ended. Cancellation throws.
      let readStamped (token: CancellationToken) =
        let output = running.Output
        task {
          try return! output.Reader.ReadAsync(token).AsTask().ConfigureAwait(false)
          with :? ChannelClosedException -> return struct (0L, null)
        }
      let readRaw (token: CancellationToken) =
        task {
          let! struct (_, line) = (readStamped token).ConfigureAwait(false)
          return line
        }
      let read () = (readRaw CancellationToken.None).GetAwaiter().GetResult()
      let readAsync () = readRaw CancellationToken.None

      /// The next line (Winboard output translated), or null on cancellation, end of stream or a
      /// read error.
      ///
      /// Every await in this class is ConfigureAwait(false). The synchronous members block on
      /// these tasks, and a continuation sent back to a blocked caller's context (the Blazor
      /// dispatcher) would never run - the call would hang for good.
      let readStampedWithTimeout (token: CancellationToken) =
        task {
          try
            let! struct (at, line) = (readStamped token).ConfigureAwait(false)
            return struct (at, (if isNull line then null else inboundOrRaw protocol line))
          with
          | :? OperationCanceledException -> return struct (0L, null)
          // StreamReader can throw this when the underlying stream is closed or cancelled.
          | :? ArgumentOutOfRangeException -> return struct (0L, null)
          | :? IOException as ioex ->
              logCritical (sprintf "IO error reading engine output: %s" ioex.Message)
              return struct (0L, null)
          | ex ->
              // Unexpected: log it and end the read rather than crash the host.
              logCritical (sprintf "Unexpected error reading engine output: %s" ex.Message)
              return struct (0L, null)
        }
      let readAsyncWithTimeout (token: CancellationToken) =
        task {
          let! struct (_, line) = (readStampedWithTimeout token).ConfigureAwait(false)
          return line
        }

      let getDiagnostics () = stderr.Diagnostics(name, lastExitCode)

      /// Reads and throws away output until the engine has said nothing for quietMs, or capMs has
      /// passed: for a Winboard engine without ping, the only way to know its last search is over.
      /// Returns how many lines it dropped.
      let drainQuiet (quietMs: int) (capMs: int) (cancel: CancellationToken) : Task<int> =
        task {
          let sw = Stopwatch.StartNew()
          let mutable dropped = 0
          let mutable quiet = false
          while not quiet && sw.ElapsedMilliseconds < int64 capMs && not (hasExited ()) && not cancel.IsCancellationRequested do
            use cts = CancellationTokenSource.CreateLinkedTokenSource cancel
            cts.CancelAfter quietMs
            let! line = (readAsyncWithTimeout cts.Token).ConfigureAwait(false)
            if isNull line then quiet <- true else dropped <- dropped + 1
          return dropped
        }

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
              return! Async.StartAsTask(initializeWinboard running.Process readRaw handler logger name FeatureTimeoutMs (forceV1 config)).ConfigureAwait(false)
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
            // Ponder is EngineBattle's to set (Configuration.withPonderOption): an engine without the
            // option is not sent it, and does not ponder (SupportsPonder)
            match UciOption.parseSetOptionCommand cmd with
            | Some (optName, _) when optName.Equals("Ponder", StringComparison.OrdinalIgnoreCase) && not (optionsMap.ContainsKey optName) ->
                logDebug (sprintf "Engine %s has no Ponder option: not sent, the engine does not ponder" name)
            | _ ->
              match EngineStartup.check optionsMap cmd with
              | EngineStartup.Valid (optName, value, changedFrom) ->
                  changedFrom |> Option.iter (fun def -> nonDefaultValues.[optName] <- (def, value))
              // QUIRK (pinned): `validate` is still true here even for an engine made by
              // createEngineWithoutValidation, which turns it off after the constructor.
              | EngineStartup.Invalid (optName, value) ->
                  if validate then
                    passed <- false
                    ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." optName value)
              | EngineStartup.Malformed ->
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
      /// Whether the engine can ponder the UCI way (go ... ponder, ponderhit): a UCI engine that
      /// lists a Ponder option. Winboard engines do not - CECP pondering is the engine's own
      /// business (hard/easy), and EngineBattle sends easy.
      member _.SupportsPonder = not (isWinboard protocol) && optionsMap.ContainsKey "Ponder"
      /// Whether a game pings it before every go: UCI only. Winboard pongs are not reliable
      /// (WaitForReadyOkAsync gives them a second, then assumes ready).
      member _.CanPing = not (isWinboard protocol)
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

      /// Nothing for a Winboard engine: it does not ponder the UCI way (SupportsPonder).
      member this.PonderHit() =
        if isWinboard protocol then logDebug (sprintf "Engine %s: ponderhit not sent (Winboard engines do not ponder in EngineBattle)" name)
        else this.Send "ponderhit"

      /// The full ponder command ("go wtime ... ponder"), written as given. Nothing for a Winboard
      /// engine: the protocol has no "go ponder", and the plain go it became searched for the side
      /// to move on the engine's board - the opponent's.
      member this.GoPonder (command: string) =
        if isWinboard protocol then logDebug (sprintf "Engine %s: ponder not sent (Winboard engines do not ponder in EngineBattle)" name)
        else this.Send command

      member this.ReadLineAsync() = readAsync ()
      member this.ReadLineAsyncWithTimeout(token: CancellationToken) = readAsyncWithTimeout token
      /// As ReadLineAsyncWithTimeout, with the Stopwatch timestamp of the line's reading (0 with null).
      member this.ReadStampedLineAsync(token: CancellationToken) = readStampedWithTimeout token
      member this.ReadLine() = read ()

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
      /// False leaves the reason in ReadyFailure.
      ///
      /// A Winboard engine is brought in step instead. The last game may have ended on its turn (a
      /// loss on time, an adjudication) with the engine still searching; the move it then sends
      /// stayed in the pipe and was read as the first move of the next game - an illegal move,
      /// and a lost game (Comet at 10+0.1 lost two that way after each loss on time). `new` stops
      /// and resets it, and what it says before this game is thrown away: up to the pong of a
      /// ping, or, for an engine without ping, until it has been quiet for 300 ms (at most 3 s).
      member this.PrepareNewGameAsync([<Optional; DefaultParameterValue(0)>] readyTimeoutMs: int,
                                      [<Optional>] cancellationToken: CancellationToken) : Task<bool> =
        let cancel = cancellationToken
        if isWinboard protocol then
          task {
            this.UciNewGame()
            match protocol with
            | Winboard handler when handler.Features.Ping ->
                // up to the pong; a second at most, and no pong is taken as ready (WaitForReadyOkAsync)
                return! this.WaitForReadyOkAsync(readyTimeoutMs, cancel).ConfigureAwait(false)
            | _ ->
                let! dropped = (drainQuiet 300 3000 cancel).ConfigureAwait(false)
                // drainQuiet stops quietly on cancellation; the other paths throw, so this does too
                cancel.ThrowIfCancellationRequested()
                if dropped > 0 then logDebug (sprintf "Engine %s: %d line(s) from before this game dropped" name dropped)
                return true
          }
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
