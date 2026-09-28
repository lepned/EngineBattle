namespace ChessLibrary

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
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

  let private startsWith (prefix: string) (line: string) = line.StartsWith(prefix, StringComparison.Ordinal)

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

      // Search output state, owned by the reader thread (the PV fields are also cleared by the
      // thread that sends a position; plain reference writes).
      let mutable state = EngineState.Start
      let mutable numberOfNodes = 0L
      let mutable evalList : MiscTypes.EvalType list = []
      let mutable fullEvalList : MiscTypes.EvalType list = []
      let mutable depth = 0
      let mutable player1PV = String.Empty
      // Long/UCI form of the same line, kept so a fail-high/low line can reuse it instead of
      // publishing its own truncated PV (see isBound in parseInfo).
      let mutable player1PVLong = String.Empty
      // Last (UCI PV, SAN PV) per MultiPV index. Reader thread only: a new position sets the flag
      // and the reader clears the cache itself on its next line.
      let sanPvCache = Dictionary<int, struct (string * string)>()
      [<VolatileField>]
      let mutable sanPvCacheStale = false

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
      let mutable inUciResponsMode = true
      [<VolatileField>]
      let mutable inIsreadyMode = false
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
          let inMode = if uciMode = "uci" then inUciResponsMode else inIsreadyMode
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

      let bestLine (engineName: string) (line: string) =
        // Parse defensively: a bare "bestmove", short lines and "bestmove (none)" (Stockfish in
        // terminal positions) must still fire Done so a waiting caller completes.
        let tokens = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        callback (Done engineName)
        if tokens.Length < 2 || tokens.[1] = "(none)" then
          if isEnabled LogLevel.Debug then logDebug (sprintf "%s: bestmove line carries no move: '%s'" engineName line)
        else
          let move = tokens.[1]
          let ponder =
            match tokens |> Array.tryFindIndex ((=) "ponder") with
            | Some i when i + 1 < tokens.Length -> tokens.[i + 1]
            | _ -> ""
          // Snapshot everything board-derived under the lock, then build the record and invoke the
          // callback outside it.
          let snapshot =
            lock moveBoardLock (fun () ->
              match tryGetMoveAndSanFromUci &moveBoard move with
              | Some (tmove, shortSan) ->
                  let moveNum = moveBoard.MoveNumber()
                  let whiteToMove = moveBoard.Position.STM = 0uy
                  // QUIRK (pinned): the position BEFORE the move - the board is read without
                  // playing it - although the fields are called FEN / FenAfterMove.
                  let fen = BoardHelper.posToFen moveBoard.Position
                  let mutable posToCheck = moveBoard.Position
                  let piecesLeft = PositionOps.numberOfPieces &posToCheck
                  let isCastling = tmove.MoveType &&& TPieceType.CASTLE <> TPieceType.EMPTY
                  Some (shortSan, moveNum, whiteToMove, fen, piecesLeft, isCastling)
              | None -> None)
          match snapshot with
          | Some (shortSan, moveNum, whiteToMove, fen, piecesLeft, isCastling) ->
            let eval =
              if evalList.Length > 0 then evalList.[0]
              elif fullEvalList.Length > 0 then fullEvalList.[0]
              else MiscTypes.EvalType.CP 0.0
            let pv =
              if String.IsNullOrEmpty player1PV then
                let prefix = if whiteToMove then sprintf "%d." moveNum else sprintf "%d..." moveNum
                prefix + shortSan
              else player1PV
            let moveDetail =
              { LongSan = move
                FromSq = move.[0..1]
                ToSq = move.[2..3]
                Color = "w"
                IsCastling = isCastling
                Comments = String.Empty }
            let moveAndFen = { Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen }
            let bestMove =
              { Player = engineName
                Move = move
                Ponder = ponder
                Eval = eval
                TimeLeft = TimeSpan.Zero
                MoveTime = TimeSpan.Zero
                NPS = 0.0 // not tracked on the bestmove path; Status updates carry the live NPS
                Nodes = numberOfNodes
                FEN = fen
                PV = pv
                LongPV = pv
                MoveAndFen = moveAndFen
                MoveHistory = ""
                Move50 = 0
                R3 = 1
                PiecesLeft = piecesLeft
                AdjDrawML = 10 }
            callback (BestMove bestMove)
            fullEvalList <- eval :: fullEvalList
            evalList <- []
            depth <- 0
          | None ->
            let msg = $"{engineName} played an illegal move here: {line} "
            let boardState =
              lock moveBoardLock (fun () ->
                $"Board state: {moveBoard.FEN()} {moveBoard.CurrentFEN} {moveBoard.Position.Ply} ")
            printfn "%s" msg
            printfn "%s" boardState

      /// SAN for one MultiPV line. SAN conversion is the hot cost on this thread (it regenerates the
      /// legal moves for every ply), and consecutive info lines usually repeat the same variation -
      /// once a mate is proven the engine repeats it for hundreds of iterations - so the last
      /// conversion per MultiPV index is kept.
      let convertPv (mpv: int) (lan: string) =
        if sanPvCacheStale then
          sanPvCache.Clear()
          sanPvCacheStale <- false
        match sanPvCache.TryGetValue mpv with
        | true, struct (cachedLan, cachedSan) when cachedLan = lan -> cachedSan
        | _ ->
          let san = lock moveBoardLock (fun () -> getShortSanPVFromLongSanPVFast moveList &moveBoard lan)
          sanPvCache.[mpv] <- struct (lan, san)
          san

      let parseInfo (engineName: string) (line: string) =
        match Regex.getEssentialDataWithEPS line whiteToMove with
        | Some (d, eval, nodes, nps, eps, pvLine, tbHits, wdl, sd, mPv) ->
          numberOfNodes <- nodes
          if d > depth then depth <- d
          evalList <- eval :: evalList
          let mPv = if mPv = 0 then 1 else mPv
          // A fail-high/low line carries a PV cut to the root move; let it through and the last
          // complete variation is lost - permanently, if the search stops right there. Score,
          // depth and node counts are real and flow on unchanged.
          let isBound = Regex.isBoundLine line
          if not (String.IsNullOrEmpty pvLine) && mPv = 1 && not isBound then
            player1PV <- convertPv 1 pvLine
            player1PVLong <- pvLine
          let pvUpdate = if mPv = 1 then player1PV else convertPv mPv pvLine
          let pvLineUpdate = if mPv = 1 && isBound then player1PVLong else pvLine
          callback (Status
            { PlayerName = engineName
              Eval = eval
              Depth = d
              SD = sd
              Nodes = nodes
              NPS = float nps
              EPS = float eps
              TBhits = tbHits
              WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
              PV = pvUpdate
              PVLongSAN = pvLineUpdate
              MultiPV = mPv })
          // Raw line alongside the parsed status: engine-specific extras in info lines survive to
          // GUI consumers (e.g. the CandidateMoves copy output).
          callback (Info (engineName, line))
        | None -> ()

      // The cached PV was converted against the board as it stood then; a new position invalidates
      // it. Without this, a search that never emits a pv line - value head runs, instant tablebase
      // or mate returns - publishes the previous position's variation, and the GUI's "fill an empty
      // PV from bestmove" fallback never fires. Called from the sending thread, so it only flags
      // the cache; the reader clears it.
      let clearPvCache () =
        player1PV <- String.Empty
        player1PVLong <- String.Empty
        sanPvCacheStale <- true

      /// One line of search output, after the handshake.
      let processLine (line: string) =
        try
          if writeToConsole then printfn "%s" line
          if startsWith "readyok" line then
            state <- RegularSearchMode
          elif startsWith "option" line then
            match state with
            | UCIMode list -> list.Add line
            | _ ->
              let list = ResizeArray<string>()
              list.Add line
              state <- UCIMode list
          elif startsWith "bestmove" line then
            state <- InBestMoveMode
          elif startsWith "info string" line && line.Contains "N:" then
            match state with
            | InMoveStatMode list ->
              let nn = Regex.getInfoStringData name line
              // One node in a policy test ("go nodes 1"): the top move carries the node's Q.
              if startsWith "info string node" line && nn.Nodes = 1 then
                let bp = list |> Seq.maxBy (fun e -> e.P)
                bp.Q <- nn.Q
              list.Add nn
            | _ ->
              let list = ResizeArray<NNValues>()
              if not (startsWith "info string node" line) then
                list.Add (Regex.getInfoStringData name line)
              state <- InMoveStatMode list
          elif startsWith "info" line then
            state <- RegularSearchMode

          match state with
          | InMoveStatMode list ->
            // QUIRK (pinned): the closing "node" line is in the sequence too, as a pseudo-move.
            if startsWith "info string node" line then
              lock moveBoardLock (fun () -> makeShortSan list &moveBoard)
              callback (NNSeq list)
              state <- Start
          | RegularSearchMode -> parseInfo name line
          | InBestMoveMode ->
            bestLine name line
            state <- Start
          | UCIMode list ->
            if startsWith "uciok" line then
              callback (UCIInfo list)
              state <- Start
          | _ -> ()
        with ex ->
          state <- Start
          printfn "Error processing line from engine %s: %s" name ex.Message

      /// One line the engine printed, in protocol terms: during the handshake it is an option or
      /// the end of the list, while waiting for readyok everything else is dropped, and after that
      /// it is search output.
      let onEngineLine (line: string) =
        if inUciResponsMode then
          if line = "uciok" then
            inUciResponsMode <- false
            readySignal.Set()
          else
            UciOption.addOptionToMap optionsMap line
        elif inIsreadyMode then
          if line = "readyok" then
            inIsreadyMode <- false
            readySignal.Set()
          elif not (isWinboard protocol) && EngineProcess.isFatalInitLine line then
            initFailure <- Some line
            readySignal.Set()
        else
          processLine line

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
              inUciResponsMode <- false
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
            inIsreadyMode <- true
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
                inIsreadyMode <- true
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
      let mutable proc = defaultof<Process>
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
      // The process WarmUp last ran against. A restarted engine is a new Process object and warms
      // up again; the object, not the OS pid, because Windows can reuse a pid.
      let mutable warmedProc : Process = null
      // The process and value SetMoveOverhead last sent; a restarted engine is sent it again.
      let mutable moveOverheadSentTo : Process = null
      let mutable moveOverheadSent = -1L
      // The running process. Replaced on every StartProcess; the stderr history and the exit code
      // above outlive it.
      let mutable transport : EngineProcess.Transport = null

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

      /// Starts the process. On a pool thread, as it always has been.
      let assignThread () =
        let started = Task.Factory.StartNew(fun () ->
          let arguments =
            if not (String.IsNullOrEmpty config.Args) then
              if isLc0 && not (config.Args.Contains("--show-hidden")) then config.Args + " --show-hidden"
              else config.Args
            elif isLc0 then "--show-hidden"
            else ""
          let t =
            EngineProcess.Transport(config, arguments, stderr,
              (fun msg -> printfn "[%s] %s" name msg),
              (fun line -> printfn "[STDERR %s]: %s" name line),
              (fun code ->
                  if code.IsSome then lastExitCode <- code
                  match code with
                  | Some c when c <> 0 ->
                      if shutdownRequested then logDebug (sprintf "Engine %s exited with code %d after quit" name c)
                      // printfn: visible even where logging is filtered.
                      else printfn "⚠️ Engine %s exited unexpectedly with code %d" name c
                  | _ -> ()))
          transport <- t
          proc <- t.Process
          // Output is read by the caller (ReadLine*), not by events.
          if not (t.Start EngineProcess.Pull) then printfn "\n❌ %s could not be started" name)
        started.Wait()

      let hasExited () =
        try (isNull proc) || proc.HasExited
        with _ -> true

      /// The exit code, read from the process when the Exited event has not delivered it yet.
      let exitCodeText () =
        match lastExitCode with
        | Some c -> string c
        | None -> if isNull transport then "?" else transport.ExitCodeText()

      /// After a read returned null: the engine closed its output, which it does as it exits. The
      /// exit can lag the closed pipe by a moment; wait for it so the reason names the exit
      /// instead of guessing (this used to report "output closed" or nothing at all on Linux).
      let exitedAfterEndOfOutput () = not (isNull transport) && transport.ExitedAfterEndOfOutput()

      let write (s: string) =
        try
          if not (isNull proc) && not proc.HasExited then
            match protocol with
            | Winboard _ ->
                let logMsg = if isEnabled LogLevel.Debug then shortForLog s else ""
                for cmd in outbound protocol false s do
                  if isEnabled LogLevel.Debug then logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" logMsg cmd name)
                  let delay = preGoDelayMs config protocol cmd
                  if delay > 0 then Thread.Sleep delay
                  transport.WriteLine cmd
            | Uci ->
                if isEnabled LogLevel.Trace then logger.Value.LogTrace(sprintf "Writing to %s: %s" name s)
                transport.WriteLine s
          else
            printfn "Warning: Attempted to write to disposed engine %s" name
        with ex ->
          printfn "Error writing to engine %s: %s" name ex.Message

      let read () = proc.StandardOutput.ReadLine()
      let readAsync () = proc.StandardOutput.ReadLineAsync()

      /// The next line (Winboard output translated), or null on cancellation, end of stream or a
      /// read error.
      let readAsyncWithTimeout (token: CancellationToken) =
        task {
          try
            let! line = proc.StandardOutput.ReadLineAsync(token)
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

      /// The handshake: uci ... uciok for a UCI engine, feature negotiation for a Winboard one.
      let readUciOptions () =
        try
          match protocol with
          | Winboard handler ->
              initializeWinboard proc handler logger name 30000 (forceV1 config) |> Async.RunSynchronously
          | Uci ->
              use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float 120000))
              write "uci"
              let rec readUci () = async {
                let! line = readAsyncWithTimeout cts.Token |> Async.AwaitTask
                if isNull line then
                  logCritical (sprintf "Engine %s: read returned null while waiting for UCI options" name)
                  return false
                else
                  printfn "%s" line
                  let mutable ret = line
                  while ret <> "uciok" && not (isNull ret) && not proc.HasExited do
                    UciOption.addOptionToMap optionsMap ret
                    let! resp = readAsyncWithTimeout cts.Token |> Async.AwaitTask
                    ret <- resp
                  let isOk = ret = "uciok"
                  if not isOk then logCritical (sprintf "Engine %s did not respond with uciok" name)
                  else logInformation (sprintf "Engine %s responded with uciok" name)
                  return isOk }
              readUci () |> Async.RunSynchronously
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
          if not (isNull proc) && not proc.HasExited then
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
            try if not (isNull proc) && not proc.HasExited then proc.Kill(true) with _ -> ()
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
      member _.Process = proc
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
                      | None when obj.ReferenceEquals(moveOverheadSentTo, proc) && moveOverheadSent = intValue ->
                          // Already set on this process. The GUI calls this before every game, and
                          // Ceres rebuilds its TensorRT engine on each MoveOverheadMs setoption.
                          ()
                      | None ->
                          let cmd = sprintf "setoption name %s value %d" option.Name intValue
                          write cmd
                          moveOverheadSentTo <- proc
                          moveOverheadSent <- intValue
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
          if proc.HasExited then logInformation (sprintf "Engine %s has already exited" name)
          else terminateProcess proc 3000
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

      member this.WaitForReadyOk(?timeoutMs: int) =
        let timeoutInMs = defaultArg timeoutMs defaultTimeoutMs
        // The high floor is intentional for UCI engines: neural-net engines can spend many minutes
        // building a TRT profile on their first init.
        let timeoutInMs = if timeoutInMs < 60000 then defaultTimeoutMs else timeoutInMs
        let readUntilReady (cts: CancellationTokenSource) (timeoutForLog: int) =
          let fail (reason: string) =
            readyFailure <- reason
            logCritical (sprintf "Engine %s: %s" name reason)
            false
          let rec loop () = async {
            try
              if this.HasExited() then
                return fail (sprintf "exited (code %s) while waiting for readyok" (exitCodeText ()))
              elif cts.Token.IsCancellationRequested then
                return fail (sprintf "timeout after %d ms waiting for readyok" timeoutForLog)
              else
                let! line = readAsyncWithTimeout cts.Token |> Async.AwaitTask
                if isNull line then
                  if cts.Token.IsCancellationRequested then
                    return fail (sprintf "timeout after %d ms waiting for readyok" timeoutForLog)
                  elif exitedAfterEndOfOutput () then
                    return fail (sprintf "exited (code %s) while waiting for readyok" (exitCodeText ()))
                  else
                    return fail "output closed while waiting for readyok"
                elif line = "readyok" then
                  // Every caller lands here; GameInitialization logs the milestone.
                  readyFailure <- ""
                  logDebug (sprintf "Engine %s responded with readyok" name)
                  return true
                elif EngineProcess.isFatalInitLine line then
                  // Alive but given up (Ceres after a refused net): no readyok will ever come, so
                  // do not sit out the timeout.
                  return fail (sprintf "reported a fatal initialization error: %s" line)
                else
                  // Anything else still queued (the tail of a search) is skipped straight away;
                  // this used to sleep 100 ms per line.
                  return! loop ()
            with
            | :? OperationCanceledException ->
                return fail (sprintf "timeout after %d ms waiting for readyok" timeoutForLog)
            | ex ->
                return fail (sprintf "error while waiting for readyok: %s" ex.Message)
          }
          loop ()
        match protocol with
        | Winboard handler when not handler.Features.Ping ->
            logInformation (sprintf "Engine %s is using Winboard protocol without ping support, assuming ready" name)
            true
        | Winboard _ ->
            // Winboard v2 with ping: these engines answer promptly or not at all - the long
            // NN-engine timeout does not apply.
            use cts = new CancellationTokenSource(TimeSpan.FromSeconds(1.0))
            write "isready"
            let res = readUntilReady cts 1000 |> Async.RunSynchronously
            if res then res
            else
              // No pong: assume ready rather than block the game start.
              logDebug (sprintf "Engine %s did not respond to ping/isready; assuming ready" name)
              true
        | Uci ->
            use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float timeoutInMs))
            write "isready"
            readUntilReady cts timeoutInMs |> Async.RunSynchronously

      /// Why the last WaitForReadyOk or WarmUp failed; "" when it succeeded.
      member _.ReadyFailure = readyFailure

      /// One `go nodes 1` from the start position, once per process, before the first game. Some
      /// engines answer readyok at once and only load their network on the first search: Lc0 0.33
      /// with onnx-trt spent 5.5 s on its first move of a 5s+0.3s game and lost it on time, and
      /// the bestmove it sent afterwards was read as its reply in the next game. The result is
      /// discarded; false means no bestmove came within the timeout, or the engine failed (then
      /// ReadyFailure says why). Callers send ucinewgame + isready afterwards either way, unless
      /// ReadyFailure is set.
      member this.WarmUp(timeoutMs: int) =
        if isWinboard protocol || this.HasExited() || obj.ReferenceEquals(warmedProc, proc) then true
        else
          warmedProc <- proc
          readyFailure <- ""
          let sw = Stopwatch.StartNew()
          // Some (Ok bestmove), Some (Error reason) when waiting longer is pointless, None on timeout.
          let readUntilBestmove (ms: int) =
            use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float ms))
            let rec loop () = async {
              let! line = readAsyncWithTimeout cts.Token |> Async.AwaitTask
              if isNull line then
                if cts.IsCancellationRequested then return None
                elif exitedAfterEndOfOutput () then
                  return Some (Error (sprintf "exited (code %s) during the warm-up search" (exitCodeText ())))
                else
                  // Output closed (or unreadable) with the process still there: no bestmove will
                  // come, and the reason must be on record.
                  return Some (Error "output closed during the warm-up search")
              elif line.StartsWith("bestmove", StringComparison.Ordinal) then return Some (Ok line)
              // Same early exit as WaitForReadyOk: Ceres after a refused net stays alive but
              // will never search.
              elif EngineProcess.isFatalInitLine line then
                return Some (Error (sprintf "reported a fatal initialization error: %s" line))
              else return! loop () }
            loop () |> Async.RunSynchronously
          let fail reason =
            readyFailure <- reason
            logCritical (sprintf "Engine %s: %s" name reason)
          write "position startpos"
          write "go nodes 1"
          match readUntilBestmove timeoutMs with
          | Some (Ok line) ->
              printfn "Engine %s warm-up: %s after %d ms" name line sw.ElapsedMilliseconds
              true
          | Some (Error reason) ->
              // No search is running, so there is nothing to stop or drain.
              fail reason
              false
          | None ->
              logCritical (sprintf "Engine %s: no bestmove within %d ms of the warm-up search" name timeoutMs)
              // A search still running would answer into the first game: stop it and consume its
              // bestmove here. An engine that has died has nothing left to drain.
              if not (this.HasExited()) then
                write "stop"
                match readUntilBestmove 10000 with
                | Some (Ok _) -> ()
                | Some (Error reason) -> fail reason
                | None -> logCritical (sprintf "Engine %s: no bestmove after stop of the warm-up search" name)
              false

      /// The UCI steps before a game, shared by the console pool (EngineHelper.initEngine) and the
      /// GUI (GameInitialization): the once-per-process warm-up, then ucinewgame + isready. The
      /// warm-up gets at least the 12-minute default, since a first TensorRT build can take longer
      /// and a timed-out warm-up is not retried. The isready wait runs whether or not the warm-up
      /// got its bestmove, unless it failed for good (exit, fatal line): then no readyok will come.
      /// False leaves the reason in ReadyFailure. Winboard engines are left alone; their init is
      /// the protocol handler's.
      member this.PrepareNewGame(?readyTimeoutMs: int) =
        if isWinboard protocol then true
        else
          let readyTimeoutMs = defaultArg readyTimeoutMs defaultTimeoutMs
          if not (this.WarmUp(max defaultTimeoutMs readyTimeoutMs)) && readyFailure <> "" then false
          else
            this.UciNewGame()
            this.WaitForReadyOk(readyTimeoutMs)

      member this.IsRunning
        with get () = isRunning
        and set (v) = isRunning <- v

      member this.HasExited() = hasExited ()
