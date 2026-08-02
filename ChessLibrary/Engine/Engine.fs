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

module Engine =

  /// Engines that colour their output (Ceres does) leave escape sequences in stderr, and
  /// those survive into the log where they make a stack trace harder to read than the
  /// thing it describes. Compiled once; stderr volume is low enough that this never shows
  /// up in a profile.
  let private ansiEscape = Regex(string (char 27) + @"\[[0-9;]*[A-Za-z]", RegexOptions.Compiled)
  let internal stripAnsi (line: string) =
    if String.IsNullOrEmpty line then line
    elif line.IndexOf '\u001b' < 0 then line   // the common case: nothing to strip
    else ansiEscape.Replace(line, "")

  // Define a class to manage the chess engine process and communicate with it 
  type ChessEngineWithUCIProcessing (callback, config : EngineConfig, initCommands: string seq, logger:ILogger, writeToConsole: bool, ?logToFile: bool)  =
      let logDebug (text: string) = logger.LogDebug text
      let logTrace (text: string) = logger.LogTrace text
      let logInformation (text: string) = logger.LogInformation text
      let logWarning (text: string) = logger.LogWarning text
      let logError (text: string) = logger.LogError text
      let logCritical (text: string) = logger.LogCritical text

      let engineLogWriter =
          if defaultArg logToFile false then
              let dir = Path.Combine(Environment.CurrentDirectory, "logs")
              if not (Directory.Exists dir) then Directory.CreateDirectory(dir) |> ignore
              let safeName = config.Name.Replace(" ", "_").Replace("/", "_").Replace("\\", "_")
              let ts = DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
              let path = Path.Combine(dir, $"engine_{safeName}_{ts}.log")
              logInformation $"Engine I/O logging to: {path}"
              Some (new StreamWriter(path, append = true, AutoFlush = true))
          else
              None

      let logIO (direction: string) (text: string) =
          match engineLogWriter with
          | Some sw ->
              let ts = DateTime.Now.ToString("HH:mm:ss.fff")
              sw.WriteLine($"[{ts}] {direction} {text}")
          | None -> ()

      let isLc0 = (Regex.Match(config.Path, "lc0", RegexOptions.IgnoreCase)).Success
      let isCeres = (Regex.Match(config.Path, "ceres", RegexOptions.IgnoreCase)).Success
      let moveBoard = Chess.Board()
      let winboardHandler = createHandlerIfNeeded config (Some logger)
      let mutable commands = ResizeArray<string>()
      let isChess960Set () = 
          commands |> Seq.tryFindBack (fun e -> e.Contains "UCI_Chess960")         
          |> fun e -> match e with |Some e -> e.Contains "true" |_ -> false

      let mutable state = EngineState.Start
      let mutable numberOfNodes = 0L
      let mutable evalList : MiscTypes.EvalType list = []
      let mutable fullEvalList :MiscTypes.EvalType list = []
      let mutable nps = 0.0
      let mutable depth = 0
      let mutable Player1PV = String.Empty
      // Long/UCI form of the same line, kept so a fail-high/low line can reuse it instead
      // of publishing its own truncated PV (see the isBoundLine guard below).
      let mutable Player1PVLong = String.Empty
      // Last (UCI PV, SAN PV) pair per MultiPV index — see convertPv in `regular`. Owned by
      // the output-reading thread: no other thread may touch it, so a new position sets the
      // flag below and the reader clears the cache itself on its next line.
      let sanPvCache = System.Collections.Generic.Dictionary<int, string * string>()
      let mutable sanPvCacheStale = false
      let mutable callbackFunc : EngineUpdate -> unit = callback //fun update -> ()
      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let name = config.Name
      let configCmds = initCommands    
      let mutable backend = ""
      let mutable isReference = false
      let mutable inUciResponsMode = true
      let mutable inIsreadyMode = false
      let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
      let optionsMap = System.Collections.Generic.Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)     
      let engineProcess = new Process()
      let dict = System.Collections.Generic.Dictionary<string, obj>()
      let nonDefaultValues = System.Collections.Generic.Dictionary<string, (string * string)>()
      let mutable inPolicyDistributionMode = false
      let mutable searchMoves: string list = []
      let searchMoveSuffix() =
        if searchMoves.IsEmpty then "" else " searchmoves " + (searchMoves |> String.concat " ")

      let ceresNetworkName = 
        //check if config.Options contains Network as key
        let network = config.Options |> Seq.tryFind (fun e -> e.Key = "Network")
        match network with
        | Some net -> 
            let nn = net.Value.ToString()
            if nn.Contains("/") then
              let nArr = nn.Split('/')
              let index = max 0 (nArr.Length - 1)
              nArr.[index]
            else
              ""
        | None ->
            let argsExists = isNull config.Args |> not &&  String.IsNullOrEmpty config.Args |> not
            if isCeres && argsExists then
              let network = config.Args.Split(':')
              if network.Length > 1 then
                network.[1]
              else
                ""
            else
              ""

      let mutable network = if isCeres then ceresNetworkName else ""
    
      let printNonDefaultValues () =
        printfn "\nCustomized SetOptions for %s:\n" name
        let path = config.Path
        RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
        for opt in nonDefaultValues do
          let (def, value) = opt.Value
          if String.IsNullOrEmpty def then 
            RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
          else
            RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
        printfn ""
    
      let getAllDefaultOptions() =            
        for opt in optionsMap do
          match opt.Value.OptionType with
          | UciOption.Check b -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, b)
          | UciOption.Spin (min, max, def) -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, def)
          | UciOption.Combo (options, def) -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, def)
          | UciOption.String s -> if dict.ContainsKey opt.Key |> not then dict.Add(opt.Key, s)
          |_ -> ()

      let waitForInitialization (cancellationToken: CancellationToken) uciMode =
        let mutable ok = false
        let mutable cont = true
        async {
            while cont do
                let mode = if uciMode = "uci" then inUciResponsMode else inIsreadyMode
                if cancellationToken.IsCancellationRequested then
                    printfn "Timeout for %s" uciMode
                    ok <- false
                    cont <- false
                elif mode then
                    do! Async.Sleep(10)
                else
                    ok <- true
                    cont <- false
            return ok
        } |> Async.RunSynchronously

      let bestLine callback (engineName:string) (line:string)  =        
        let move, ponder = line.Split().[1], if line.Contains "ponder" then (line.Split().[3]) else ""
        callback (Done engineName)
        match tryGetTMoveFromUciNotation &moveBoard move with
        |Some tmove ->
          //let mutable moveAdj = tmove
          let shortSan = getSanNotationFromTMove &moveBoard tmove    
          let eval = 
            if evalList.Length > 0 then 
              evalList.[0] 
            else 
              if fullEvalList.Length > 0 then fullEvalList[0] else MiscTypes.EvalType.CP 0.0
          let pv =
            if String.IsNullOrEmpty(Player1PV) then
              let moveNum = moveBoard.MoveNumber()
              let prefix = if moveBoard.Position.STM = 0uy then sprintf "%d." moveNum else sprintf "%d..." moveNum
              prefix + shortSan
            else Player1PV
          let fen = BoardHelper.posToFen moveBoard.Position
          let moveDetail = 
            {
              LongSan = move
              FromSq = move[0..1]
              ToSq = move[2..3]
              Color = "w"
              IsCastling = (tmove.MoveType &&& TPieceType.CASTLE <> TPieceType.EMPTY) 
              Comments = String.Empty
              }                 
          let moveAndFen = {Move = moveDetail; ShortSan=shortSan; FenAfterMove = fen}
          let mutable posToCheck = moveBoard.Position
          let piecesLeft = PositionOps.numberOfPieces &posToCheck
          let bestMove = 
            { Player=engineName
              Move=move
              Ponder= ponder
              Eval= eval
              TimeLeft = TimeOnly.MinValue
              MoveTime = TimeOnly.MinValue
              NPS=nps
              Nodes=numberOfNodes
              FEN = fen
              PV=pv 
              LongPV = pv
              MoveAndFen = moveAndFen
              MoveHistory = ""
              Move50 = 0
              R3 = 1
              PiecesLeft = piecesLeft
              AdjDrawML = 10
              }
                    
          callback(BestMove bestMove)
          fullEvalList <- eval::fullEvalList
          evalList <- []
          depth <- 0
        |_ ->
          let msg = $"{engineName} played an illegal move here: {line} "
          let boardState = $"Board state: {moveBoard.FEN()} {moveBoard.CurrentFEN} {moveBoard.Position.Ply} "
          printfn "%s" msg
          printfn "%s" boardState

      let regular callback (engineName:string) (line:string) =
        let mutable avgNps = 0.0
        let isWhite = moveBoard.Position.STM = 0uy
        match EngineProtocol.Regex.getEssentialDataWithEPS line isWhite with
        |Some (d, eval, nodes, nps, eps, pvLine, tbHits, wdl, sd, mPv ) ->         
          numberOfNodes <- nodes
          if d > depth then
            depth <- d        
          evalList <- eval :: evalList
          let mPv = if mPv = 0 then 1 else mPv
          // A fail-high/low line carries a PV cut to the root move; let it through and the
          // last complete variation is lost — permanently, if the search stops right there.
          // Score, depth and node counts are real and flow on unchanged.
          let isBound = EngineProtocol.Regex.isBoundLine line
          // SAN conversion is the hot cost on this thread: it regenerates every legal move
          // for every ply of the PV, allocating as it goes. Consecutive info lines usually
          // repeat the same variation — once a mate is proven the engine repeats it for
          // hundreds of iterations — so hold the last conversion per MultiPV index. Cleared
          // with the rest of the PV state whenever a new position is set.
          let convertPv (mpv: int) (lan: string) =
            // Cleared here rather than in clearPvCache: this is the only thread allowed to
            // touch the dictionary, and a position change can arrive on any other.
            if sanPvCacheStale then
              sanPvCache.Clear()
              sanPvCacheStale <- false
            match sanPvCache.TryGetValue mpv with
            | true, (cachedLan, cachedSan) when cachedLan = lan -> cachedSan
            | _ ->
              let san = getShortSanPVFromLongSanPVFast moveList &moveBoard lan
              sanPvCache[mpv] <- (lan, san)
              san
          if not (String.IsNullOrEmpty(pvLine)) && mPv = 1 && not isBound then
              Player1PV <- convertPv 1 pvLine
              Player1PVLong <- pvLine
          let pvUpdate = if mPv = 1 then Player1PV else convertPv mPv pvLine
          let pvLineUpdate = if mPv = 1 && isBound then Player1PVLong else pvLine
          let status = 
            { 
              PlayerName = engineName
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
              MultiPV = mPv
            }
          callback (Status status)
          // Raw line alongside the parsed status: engine-specific extras in info lines
          // survive to GUI consumers (e.g. the CandidateMoves copy output).
          callback (Info (engineName, line))

        |None -> ()

      // The cached PV was converted against the board as it stood then; a new position
      // command invalidates it. Without this, a search that never emits a pv line — value
      // head runs, instant tablebase or mate returns — publishes the previous position's
      // variation instead of nothing, and the GUI's "fill empty PV from bestmove" fallback
      // never fires because the field isn't empty.
      // Called from whichever thread sends the position, so it must not touch the cache
      // dictionary itself — only flag it. The two string fields are plain reference
      // assignments and safe to write from here.
      let clearPvCache() =
        Player1PV <- String.Empty
        Player1PVLong <- String.Empty
        sanPvCacheStale <- true

      let processLine callback (name:string) (line:string)  =
        try
            if writeToConsole then
              printfn "%s" line
            match line with
            | line when line.StartsWith("readyok") ->
                state <- RegularSearchMode 
            | line when line.StartsWith("option") ->
                match state with
                |UCIMode list -> 
                  list.Add line             
                |_ ->
                  let list = ResizeArray<string>()
                  list.Add line
                  state <- UCIMode list
            | line when line.StartsWith("bestmove") ->        
                state <- InBestMoveMode 
            | line when line.StartsWith "info string" && line.Contains "N:" ->          
                match state with
                |InMoveStatMode list ->
                  if line.StartsWith "info string node" |> not then
                    let nn = EngineProtocol.Regex.getInfoStringData name line
                    list.Add nn
                  else
                    let nn = EngineProtocol.Regex.getInfoStringData name line
                    if nn.Nodes = 1 then //will only be one node in policy test like go nodes 1
                      let bp = list |> Seq.maxBy(fun e -> e.P)
                      bp.Q <- nn.Q
                    list.Add nn
                |_ -> 
                  let list = ResizeArray<NNValues>()  
                  if line.StartsWith "info string node" |> not then
                    let nn = EngineProtocol.Regex.getInfoStringData name line
                    list.Add nn
                  state <- InMoveStatMode list
            | line when line.StartsWith("info") ->
                state <- RegularSearchMode 
            | _  -> ()         
      
            match state with
            |InMoveStatMode list ->
              if line.StartsWith "info string node" then
                makeShortSan list &moveBoard
                callback (NNSeq list)
                state <- Start
            | RegularSearchMode ->
                regular callback name line
            | InBestMoveMode ->
                bestLine callback name line
                state <- Start
            | UCIMode list ->
                if line.StartsWith("uciok") then           
                  callback (UCIInfo list)
                  state <- Start
            |_ -> ()
        with ex ->
            state <- Start
            printfn "Error processing line from engine %s: %s" name ex.Message

      let printCommands () =
        printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
        for cmd in initCommands do
          if cmd.Contains "setopt" then
            printfn "%s" cmd

      let write (s:string) =
        if engineProcess.HasExited then
          logger.LogCritical  (sprintf "Engine %s has exited - please reset engine." name)        
        else
            match winboardHandler with
            | Some handler ->
                // Translate UCI to Winboard (analysis mode)
                let winboardCmds = handler.UciToWinboard(s, analysisMode = true)
                let hasGo = winboardCmds |> List.exists ((=) "go")
                let hasTimeCommands = winboardCmds |> List.exists (fun c -> c.StartsWith("time ") || c.StartsWith("otim "))

                for cmd in winboardCmds do
                    logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" s cmd name)
                    logIO ">>>" cmd
                    // For engines without ping support: add delay after time/otim and before go
                    // to ensure time commands are processed before engine starts searching
                    if cmd.StartsWith "go" then
                        let preGoDelay = config.WinboardConfig |> Option.map (fun wbc -> wbc.PreGoDelayMs) |> Option.defaultValue 100
                        if preGoDelay > 0 then
                            System.Threading.Thread.Sleep(preGoDelay)
                    engineProcess.StandardInput.WriteLine(cmd)
            | None ->
                // Normal UCI
                engineProcess.StandardInput.WriteLine(s)
                logIO ">>>" s
                logTrace (sprintf "Writing to %s: %s" name s)
    
      let assignbackend (option:string) =
        if option.ToLower().Contains("backendoptions") then
          let arr = option.Split(' ')
          let startIdx = arr |> Array.findIndex(fun e -> e = "value")
          let rest = arr[startIdx+1..] |> String.concat " "
          backend <- rest      

      let assignNetworkName (option:string) =
        if option.ToLower().Contains("weights") || option.ToLower().Contains("evalfile") then
          if String.IsNullOrEmpty ceresNetworkName |> not then
            network <- ceresNetworkName
          else
            let arr = option.Split(' ')
            let path = arr[arr.Length - 1]
            let name = Path.GetFileNameWithoutExtension path
            if String.IsNullOrEmpty name |> not then
              network <- name
    
      let analysisCommands =
        [
          sprintf "setoption name %s value %d" "MoveOverheadMs" 0
        ]

      let distribution = ResizeArray<NNValues>()
      
      let myCallback (update: EngineUpdate) =
        match update with        
        | EngineUpdate.NNSeq list -> distribution.AddRange list
        | _ -> ()
            
      
      let tryGetMovePolicyAndTop (data:string) (move:string) =
        processLine myCallback name data
        
        match distribution |> Seq.tryFind (fun x -> x.LANMove = move) with
        |Some movePolicy -> 
            let topPolicy = 
                match distribution |> Seq.sortByDescending(fun x -> x.P)|> Seq.tryHead with
                |Some top -> top
                |None -> failwith "Could not find top policy"
            let rankForMovePlayed = 
                distribution 
                |> Seq.sortByDescending(fun x -> x.P) 
                |> Seq.toArray 
                |> Array.findIndex(fun x -> x.LANMove = move) 
                |> (+) 1
            Some (rankForMovePlayed, movePolicy, topPolicy)
        |None -> None            

      let mutable lastExitCode : int option = None
      // Ceres and others exit non-zero on a clean quit; only warn when we didn't ask.
      let mutable shutdownRequested = false
      // Ring buffer: a crash writes its stack last, so keeping the newest lines always
      // preserves it, while a chatty engine cannot grow this without bound over a long
      // tournament. Trimmed in blocks because RemoveAt(0) shifts the whole list. Guarded by
      // stderrLock: the process writes from its stderr callback thread while the game thread
      // reads the whole buffer in getDiagnostics.
      let stderrMaxLines = 500
      let stderrBuffer = ResizeArray<string>()
      let stderrLock = obj()
      let mutable stderrDropped = 0

      let addStderrLine (line: string) =
        let line = stripAnsi line
        lock stderrLock (fun () ->
          stderrBuffer.Add line
          if stderrBuffer.Count > stderrMaxLines + 100 then
            let excess = stderrBuffer.Count - stderrMaxLines
            stderrBuffer.RemoveRange(0, excess)
            stderrDropped <- stderrDropped + excess)
      
      let setupProcess() =
          let mutable pos = moveBoard.Position
          moveBoard.IsFRC <- PositionOps.isFRC &pos
          engineProcess.StartInfo.FileName <- config.Path
          engineProcess.StartInfo.UseShellExecute <- false
          engineProcess.StartInfo.RedirectStandardInput <- true
          engineProcess.StartInfo.RedirectStandardOutput <- true
          engineProcess.StartInfo.RedirectStandardError <- true
          // Set working directory to engine's directory to prevent cluttering application folder
          let engineDir = System.IO.Path.GetDirectoryName(config.Path)
          if not (String.IsNullOrWhiteSpace(engineDir)) && System.IO.Directory.Exists(engineDir) then
              engineProcess.StartInfo.WorkingDirectory <- engineDir
              logDebug $"[{name}] Working directory set to: {engineDir}"
          else
              logDebug $"[{name}] WARNING: Could not set working directory. Path: {config.Path}, Dir: {engineDir}"
          if String.IsNullOrEmpty config.Args |> not then
            logDebug $"Args passed: {config.Args}"
            engineProcess.StartInfo.Arguments <- config.Args
          elif isLc0 then
            engineProcess.StartInfo.Arguments <- "--show-hidden"
          // Capture stderr asynchronously
          engineProcess.ErrorDataReceived.Add(fun args ->
                try
                    if not (isNull args) && not (String.IsNullOrEmpty args.Data) then
                        addStderrLine args.Data
                        logDebug $"[STDERR {name}]: {args.Data}"
                with _ -> ()
            )
            
            // Track exit events
          engineProcess.Exited.Add(fun _ ->
                try
                    lastExitCode <- Some engineProcess.ExitCode
                    if engineProcess.ExitCode <> 0 then
                        if shutdownRequested then
                            logDebug $"Engine {name} exited with code {engineProcess.ExitCode} after quit"
                        else
                            logInformation $"⚠️ Engine {name} exited unexpectedly with code {engineProcess.ExitCode}"
                with _ -> ()
            )
          engineProcess.EnableRaisingEvents <- true
          
          
          engineProcess.OutputDataReceived.Add(fun args ->
              if not (String.IsNullOrEmpty args.Data) then
                logIO "<<<" args.Data
                match winboardHandler with
                | Some handler ->
                    // Translate Winboard output to UCI
                    logDebug $"[{name}] Received output: {args.Data}"
                    let translated = handler.ProcessOutput(args.Data)
                    match translated with
                    | Some uciLine ->
                        logDebug $"[{name}] Translated to UCI: {uciLine}"
                        // Handle UCI-translated output
                        if inUciResponsMode then
                          if uciLine = "uciok" then
                            inUciResponsMode <- false
                          else
                            UciOption.addOptionToMap optionsMap uciLine
                        elif inIsreadyMode then
                          if uciLine = "readyok" then
                            inIsreadyMode <- false
                        else
                          processLine callbackFunc name uciLine
                    | None ->
                        // Not translated - this is normal for some Winboard output
                        logDebug $"[{name}] Output not translated (normal for some Winboard output)"
                | None ->
                    // Normal UCI processing
                    if inUciResponsMode then  
                      if args.Data = "uciok" then              
                        inUciResponsMode <- false
                      else
                        UciOption.addOptionToMap optionsMap args.Data
                    elif inIsreadyMode then
                      if args.Data = "readyok" then
                        inIsreadyMode <- false           
                    else 
                      processLine callbackFunc name args.Data)
          if engineProcess.Start() then
              // Ensure LF newline and immediate flush so WriteLine uses LF on all platforms
              engineProcess.StandardInput.NewLine <- "\n"
              engineProcess.StandardInput.AutoFlush <- true
              engineProcess.BeginErrorReadLine()  // Start stderr capture
          else
              failwith (sprintf "Engine %s could not be started" name)

          let mutable isWinboard = false

          // Start event-based output reading for BOTH UCI and Winboard
          engineProcess.BeginOutputReadLine()

          // Initialize based on protocol
          match winboardHandler with
          | Some handler ->
              // Winboard initialization using event-based reading
              isWinboard <- true
              logInformation $"[{name}] Starting Winboard initialization"
              // Send init commands DIRECTLY (don't use write - these are already Winboard commands)
              for cmd in handler.GetInitCommands() do
                  logDebug $"[{name}] Sending init command: {cmd}"
                  engineProcess.StandardInput.WriteLine(cmd)
              // Wait for initialization (2 second timeout for V1 engine detection)
              logDebug $"[{name}] Waiting for Winboard initialization to complete..."
              let startWait = System.DateTime.UtcNow
              let forceV1 = config.WinboardConfig |> Option.map (fun wbc -> wbc.ForceV1Mode) |> Option.defaultValue false
              let initSuccess = initializeWinboardEventBased handler (Some logger) name 2000 forceV1 |> Async.RunSynchronously
              let waitTime = (System.DateTime.UtcNow - startWait).TotalMilliseconds
              logInformation $"[{name}] Winboard init wait completed in {waitTime}ms, success={initSuccess}"
              if not initSuccess then
                  failwith "Winboard engine did not initialize properly."
              // Winboard engines don't send "uciok", so manually exit UCI response mode
              inUciResponsMode <- false
              // Send post-init commands DIRECTLY (these are also Winboard commands)
              for cmd in handler.GetPostInitCommands() do
                  logDebug $"[{name}] Sending post-init command: {cmd}"
                  engineProcess.StandardInput.WriteLine(cmd)
          | None ->
              // UCI initialization
              write "uci"
              let timeout = TimeSpan.FromHours(2).TotalMilliseconds |> int
              let ok = waitForInitialization ((new CancellationTokenSource(timeout)).Token) "uci"
              if not ok then
                failwith "Engine did not respond to UCI command."
          
          for cmd in configCmds do
            match UciOption.parseSetOptionCommand cmd with
            | Some (name, value) ->
                if UciOption.validateSetOption optionsMap (name, value) then                  
                    dict.[name] <- value
                    match UciOption.getNoneDefaultSetOption optionsMap (name, value) with
                    | Some (name, def,value) -> nonDefaultValues.[name] <- (def,value)
                    | None -> ()
                    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "The option '%s' with value '%s' is valid." name value)
                else
                    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." name value)
            | None ->
                RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)

            write cmd
            assignbackend cmd
            assignNetworkName cmd
            commands.Add cmd      
          for cmd in analysisCommands do
            write cmd          
            commands.Add cmd
          if isCeres then
            network <- ceresNetworkName
          printNonDefaultValues()
          write "ucinewgame"
          
          // Only set isready mode for UCI engines
          // Winboard engines don't need this handshake after initialization
          if not isWinboard then
              inIsreadyMode <- true
              write "isready"
              
              let timeout = TimeSpan.FromHours(2).TotalMilliseconds |> int
              let ok = waitForInitialization((new CancellationTokenSource(timeout)).Token) "readyok"
              if not ok then
                  failwith "Engine did not respond to isready command."
          
          // Snapshot before enumerating: the stderr callback thread may be appending.
          let pending = lock stderrLock (fun () -> stderrBuffer.ToArray())
          for err in pending do
            logDebug $"[STDERR {name}]: {err}"
          let withLiveLog = optionsMap.ContainsKey("LogLiveStats")
          getAllDefaultOptions()
          callback(EngineUpdate.Ready (name, withLiveLog))

      do
          setupProcess()
      /// Snapshot rather than the live list — callers enumerate while the process may still
      /// be writing.
      member _.ErrorOutput = lock stderrLock (fun () -> stderrBuffer.ToArray()) :> seq<string>
      member _.LastExitCode = lastExitCode
      member _.GetDiagnostics() =
        // Snapshot under the lock, format outside: this is only called when a game was
        // adjudicated because the engine died, and everything the buffer still holds goes
        // to the log. The dropped count is reported rather than hidden, so a truncated
        // history is visible instead of being mistaken for the whole story.
        let lines, dropped =
          lock stderrLock (fun () -> stderrBuffer.ToArray(), stderrDropped)
        let header =
          sprintf "Engine: %s | ExitCode: %s | Stderr lines: %d%s"
            name (match lastExitCode with Some c -> string c | None -> "N/A") lines.Length
            (if dropped > 0 then sprintf " (%d older lines dropped)" dropped else "")
        if lines.Length = 0 then header
        else
          sprintf "%s%s%s stderr:%s%s" header Environment.NewLine name Environment.NewLine
            (String.concat Environment.NewLine lines)
      member _.GetNoneDefaultSetOptions() = nonDefaultValues        
      member _.GetAllDefaultOptions() = dict
      member this.IsLc0 = isLc0
      member val InPolicyDistributionMode = inPolicyDistributionMode with get, set
      member val IsFRC = moveBoard.IsFRC with get, set
      member _.PrintUCI() = printCommands()
      member _.Network = network
      member _.Name = name
      member _.FullName = if network <> "" then $"{name} with net: {network}" else name
      member _.GetBackEnd() = backend
      member val IsReference = isReference with get, set
      member this.BenchmarkLC0Cmd = benchMarkLC0Cmd
      member this.ShowCommands = printCommands
      member this.Config = config
      member this.Path = config.Path
      member this.GetUCICommands() = optionsMap
      /// Engine's self-declared identity from the UCI handshake ("id name ..."),
      /// or "" if not (yet) received. More reliable than config display Name or Path.
      member _.UciIdName =
        match UciOption.tryFindOption optionsMap "name" with
        | Some { OptionType = UciOption.UciOptionType.IdAndAuthor(_, _, value) } -> value
        | _ -> ""
      member this.ShutDownEngine() = this.SendUCICommand UCICommand.Quit
      member this.HasExited = engineProcess.HasExited
      
      member this.CurrentPositionCommand() = moveBoard.PositionWithMovesFromGraph()

      member this.SetAllOptions (allOptions: Dictionary<string,obj>) =
        for opt in allOptions do
          let cmd =            
            match Boolean.TryParse (opt.Value.ToString()) with
            | true, v -> sprintf "setoption name %s value %s" opt.Key (sprintf "%b" v)              
            | _ -> sprintf "setoption name %s value %s" opt.Key (opt.Value.ToString())            
          printfn "%s" cmd
          write cmd
          assignNetworkName cmd
          commands.Add cmd
          dict.[opt.Key] <- opt.Value

      member this.WaitForReadyOk(?timeoutMs: int) =
          if engineProcess.HasExited then
              false
          else
              match winboardHandler with
              | Some _ ->
                  true
              | None ->
                  inIsreadyMode <- true
                  write "isready"
                  let timeoutInMs = defaultArg timeoutMs (TimeSpan.FromHours(2).TotalMilliseconds |> int)
                  waitForInitialization ((new CancellationTokenSource(timeoutInMs)).Token) "readyok"

      member this.SendUCICommand (command: UCICommand) =
          match command with
          | UCI ->
              let cmd = "uci"
              write cmd
              commands.Add cmd
          | Stop ->
                let cmd = "stop"
                try
                    // Check if the process is still running before attempting to write
                    if not engineProcess.HasExited then
                        write cmd
                        commands.Add cmd                        
                with
                | _ ->
                    // Engine process has already closed the pipe - this is expected during shutdown
                    let msg = sprintf "Engine %s pipe already closed it seems" name
                    printfn "%s" msg 
                    this.GetDiagnostics() |> printfn "%s"
          | Quit -> 
                shutdownRequested <- true
                let cmd = "quit"
                try
                    // Check if the process is still running before attempting to write
                    if not engineProcess.HasExited then
                        write cmd
                        commands.Add cmd
                    let hasExited = engineProcess.WaitForExit(1000)
                    if not hasExited then
                        engineProcess.Kill()            
                with
                | :? System.IO.IOException as ex when ex.Message.Contains("pipe") ->
                    // Engine process has already closed the pipe - this is expected during shutdown
                     let msg = sprintf "Engine %s pipe already closed during shutdown" name
                     printfn "%s" msg
                | :? System.ObjectDisposedException ->
                    // Process was already disposed - this is expected during shutdown
                    let msg = sprintf "Engine %s process already disposed during shutdown" name
                    printfn "%s" msg
                | _ ->
                    let msg = sprintf "Error during engine %s shutdown" name
                    printfn "%s" msg
                // Cleanup in a separate try-finally block
                try
                    engineProcess.Close()
                    engineProcess.Dispose()
                    match engineLogWriter with
                    | Some sw -> sw.Dispose()
                    | None -> ()
                    printfn "Engine %s has been shut down." name
                with
                | :? System.ObjectDisposedException ->
                    // Already disposed - ignore
                    this.GetDiagnostics() |> printfn "%s"              
          | RawCommand cmd ->
              write cmd
              commands.Add cmd
          | PositionWithMoves command ->
              //let hmm = board.PositionWithMovesIndexed()
              moveBoard.ResetBoardState()
              clearPvCache()
              moveBoard.PlayCommands command
              let isSet = isChess960Set()
              if moveBoard.IsFRC && not isSet then 
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif isSet && not moveBoard.IsFRC then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              write command
              commands.Add command
          | Position fen ->
              //board.LoadFen fen
              moveBoard.LoadFen fen
              clearPvCache()
              if moveBoard.IsFRC then 
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif isChess960Set() && not moveBoard.IsFRC then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              let cmd = (sprintf "position fen %s" fen)
              write cmd
              commands.Add cmd
          | GoNodes nodes ->
              let cmd = sprintf "go nodes %d%s" nodes (searchMoveSuffix())
              write cmd
              commands.Add cmd
          | GoInfinite ->
              let cmd = sprintf "go infinite%s" (searchMoveSuffix())
              write cmd
              commands.Add cmd
          | GoMoveTime timeInMs ->
              let cmd = sprintf "go movetime %d%s" timeInMs (searchMoveSuffix())
              write cmd
              commands.Add cmd
          | GoValue ->
              let cmd = sprintf "go value"
              write cmd
              commands.Add cmd
          | GoTimeControl (tc,wTime,bTime) ->
              let cmd = TimeControlCommands.uciTimeCommand tc wTime bTime + searchMoveSuffix()
              write cmd
              commands.Add cmd
          | UciNewGame ->
            let cmd = "ucinewgame"
            clearPvCache()
            write cmd
            commands.Add cmd
          | SetOption option ->
            let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
            write cmd
            assignNetworkName cmd
            commands.Add cmd

          | SetOptions options ->
            for option in options do
              let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
              write cmd
              assignNetworkName cmd
              commands.Add cmd
        
          | PolicyDistribution game ->
              distribution.Clear()              
              inPolicyDistributionMode <- true
              
          | SetMoveOverhead (optionName, ms) ->         
              match UciOption.tryFindOption optionsMap optionName with
              | Some option ->
                  match option.OptionType with        
                  | UciOption.Spin (min, max, _) -> 
                      let intValue = int64 ms
                      if intValue >= min && intValue <= max then
                        //create a setoption based on the option and the intValue
                        let cmd = sprintf "setoption name %s value %d" option.Name intValue
                        write cmd
                        commands.Add cmd
                  | _ -> ()
              | None -> printfn "Option not found: %s value: %d" optionName ms

      member this.SetSearchMoves (moves: string list) = searchMoves <- moves
      member this.ClearSearchMoves () = searchMoves <- []
      member this.SearchMoves with get() = searchMoves


  let printedEngines = System.Collections.Generic.HashSet<string>()
  // Not printedEngines: that set is only filled when option validation runs.
  let loggedVersions = System.Collections.Generic.HashSet<string>()
  let printLock = obj()
  let resetPrintedEngines () =
    printedEngines.Clear()
    loggedVersions.Clear()

  type ChessEngine(config : EngineConfig, initCommands: string seq, logger: ILogger option) =
      let printToConsole txt = printfn "%s" txt
      let logCritical text =
        match logger with
        | Some log -> log.LogCritical text
        | None -> () //printfn "%s" text
      let logInformation text =
        match logger with
        | Some log -> log.LogInformation text
        | None -> () //printfn "%s" text
      let logDebug text =
        match logger with
        | Some log -> log.LogDebug text
        | None -> () // printfn "%s" text
      let logTrace text =
        match logger with
        | Some log -> log.LogTrace text
        | None -> ()

      let initialCommands = initCommands |> ResizeArray
      let defaultTimeoutMs = 180000 * 4 // 12 minutes - some engines take a long time to respond to isready after setting options
      let mutable passed = true
      let isLc0 = (Regex.Match(config.Path, "lc0", RegexOptions.IgnoreCase)).Success
      let isCeres = (Regex.Match(config.Path, "ceres", RegexOptions.IgnoreCase)).Success
      let mutable network = String.Empty
      let winboardHandler = createHandlerIfNeeded config logger
      let assignNetworkName (option:string) =        
        let argsExists = String.IsNullOrEmpty config.Args |> not
        if isCeres && argsExists then
            let CeresNet = config.Args.Split(':')
            if CeresNet.Length > 1 then
                network <- CeresNet.[1]
          
        if option.ToLower().Contains("weights") || option.ToLower().Contains("network") || option.ToLower().Contains("evalfile") then
            let arr = option.Split(' ')
            let path = arr[arr.Length - 1]            
            let name = Path.GetFileNameWithoutExtension path
            if String.IsNullOrEmpty name |> not then
                network <- name      
            
      let optionsMap = System.Collections.Generic.Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let name = config.Name     
      let configCmds = initCommands
      let mutable proc = defaultof<Process>
      let mutable commands = ResizeArray<string>()
      let nonDefaultValues = System.Collections.Generic.Dictionary<string, (string * string)>()      

      let addCommand (list : string ResizeArray) (cmd: string) =
        if not (list.Contains cmd) then
          list.Add cmd

      let printNonDefaultValues () =
        printfn "\nCustomized SetOptions for %s:\n" name
        let path = config.Path
        RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
        for opt in nonDefaultValues do
          let (def, value) = opt.Value
          if String.IsNullOrEmpty def then 
            RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
          else
            RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
        printfn ""

      let getAllDefaultOptions() =
        let dict = System.Collections.Generic.Dictionary<string, obj>()      
        for opt in optionsMap do
          //if opt.Key.Contains "Weight" then
          //  printfn "%s" opt.Key
          match opt.Value.OptionType with
          | UciOption.Check b -> dict.Add(opt.Key, b)
          | UciOption.Spin (min, max, def) -> dict.Add(opt.Key, def)
          | UciOption.Combo (options, def) -> dict.Add(opt.Key, def)
          | UciOption.String s -> dict.Add(opt.Key, s)
          |_ -> ()
        dict

      let createVerifiedOptions (options:string seq) =
        [
            for opt in options do
              match UciOption.parseSetOptionCommand opt with
              | Some (name, value) -> 
                    match optionsMap.TryGetValue name with
                    | true, opt -> sprintf "setoption name %s value %s" opt.Name value
                    | false, _ -> opt
              | None -> opt
        ]
      
      let printCommands () =
        printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
        for cmd in initCommands do
          if cmd.Contains "setopt" then
            printfn "%s" cmd

      /// Attempts to terminate the given process within `timeoutMs` milliseconds.
      let terminateProcess (proc: Process) (timeoutMs: int) =
          try
              //wait for it to exit on its own
              if not (proc.WaitForExit timeoutMs) then
                  //still running → try to kill it
                  try
                      proc.Kill()
                  with ex ->
                      printfn "Warning: could not kill '%s': %s" name ex.Message
                  //give it a moment, then check again (and ignore the result again)
                  proc.WaitForExit 5_000 |> ignore
          finally
              //cleanup handles
              proc.Close()
              proc.Dispose()
      
      let mutable lastExitCode : int option = None
      // Ceres and others exit non-zero on a clean quit; only warn when we didn't ask.
      let mutable shutdownRequested = false
      // Ring buffer: a crash writes its stack last, so keeping the newest lines always
      // preserves it, while a chatty engine cannot grow this without bound over a long
      // tournament. Trimmed in blocks because RemoveAt(0) shifts the whole list. Guarded by
      // stderrLock: the process writes from its stderr callback thread while the game thread
      // reads the whole buffer in getDiagnostics.
      let stderrMaxLines = 500
      let stderrBuffer = ResizeArray<string>()
      let stderrLock = obj()
      let mutable stderrDropped = 0

      let addStderrLine (line: string) =
        let line = stripAnsi line
        lock stderrLock (fun () ->
          stderrBuffer.Add line
          if stderrBuffer.Count > stderrMaxLines + 100 then
            let excess = stderrBuffer.Count - stderrMaxLines
            stderrBuffer.RemoveRange(0, excess)
            stderrDropped <- stderrDropped + excess)

      let assignThread () =         
        let task = Task.Factory.StartNew(fun () -> 
            let engine = new Process()
            proc <- engine
            engine.StartInfo.FileName <- config.Path
            engine.StartInfo.UseShellExecute <- false
            engine.StartInfo.RedirectStandardInput <- true
            engine.StartInfo.RedirectStandardOutput <- true
            engine.StartInfo.RedirectStandardError <- true
            let engineDir = System.IO.Path.GetDirectoryName(config.Path)
            if not (String.IsNullOrWhiteSpace(engineDir)) && System.IO.Directory.Exists(engineDir) then
                  engine.StartInfo.WorkingDirectory <- engineDir
                  printfn $"[{name}] Working directory set to: {engineDir}"
            else
                  printfn $"[{name}] WARNING: Could not set working directory. Path: {config.Path}, Dir: {engineDir}"
            if not (String.IsNullOrEmpty config.Args) then                
                let args = 
                    if isLc0 && not (config.Args.Contains("--show-hidden")) then
                        config.Args + " --show-hidden"
                    else
                        config.Args
                engine.StartInfo.Arguments <- args
            elif isLc0 then
                engine.StartInfo.Arguments <- "--show-hidden"
           
            // Capture stderr asynchronously
            engine.ErrorDataReceived.Add(fun args ->
                try
                    if not (isNull args) && not (String.IsNullOrEmpty args.Data) then
                        addStderrLine args.Data
                        printfn "[STDERR %s]: %s" name args.Data
                with _ -> ()
            )
            
            // Track exit events
            engine.Exited.Add(fun _ ->
                try
                    lastExitCode <- Some engine.ExitCode
                    if engine.ExitCode <> 0 then
                        if shutdownRequested then
                            logDebug (sprintf "Engine %s exited with code %d after quit" name engine.ExitCode)
                        else
                            // printfn: visible even where logging is filtered.
                            printfn "⚠️ Engine %s exited unexpectedly with code %d" name engine.ExitCode
                with _ -> ()
            )
            engine.EnableRaisingEvents <- true
            
            if engine.Start() then                
                // Ensure LF newline and immediate flush so WriteLine uses LF on all platforms
                engine.StandardInput.NewLine <- "\n"
                engine.StandardInput.AutoFlush <- true                
                engine.BeginErrorReadLine()  // Start stderr capture
                // Note: For ChessEngine (play mode), we don't use async output reading
                // Output is read synchronously via ReadLine/ReadLineAsync methods
                // OutputDataReceived handlers are only used in ChessEngineWithUCIProcessing                   
            else
                printfn "\n❌ %s could not be started" name
        )
        task.Wait()
      
      let mutable isRunning = false
      let mutable isReference = false
      let mutable validate = true
    
      let write (s:string) = 
        try
            if proc <> null && not proc.HasExited then
                      match winboardHandler with
                      | Some handler ->
                          // Translate UCI to Winboard
                          let winboardCmds = handler.UciToWinboard(s)

                          // Create a shorter log message for position commands
                          let logMsg =
                              if s.StartsWith("position") && s.Length > 100 then
                                  let movesIdx = s.IndexOf("moves")
                                  if movesIdx > 0 then
                                      let movesPart = s.Substring(movesIdx + 6)
                                      let moves = movesPart.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                                      sprintf "position with %d moves" moves.Length
                                  else
                                      "position startpos"
                              else
                                  s

                          for cmd in winboardCmds do
                              logDebug (sprintf "[UCI→Winboard] '%s' → '%s' for %s" logMsg cmd name)

                              // For engines without ping support: add delay after time/otim and before go
                              // to ensure time commands are processed before engine starts searching
                              if cmd.StartsWith "go" then
                                  let preGoDelay = config.WinboardConfig |> Option.map (fun wbc -> wbc.PreGoDelayMs) |> Option.defaultValue 100
                                  if preGoDelay > 0 then
                                      System.Threading.Thread.Sleep(preGoDelay)
                              proc.StandardInput.WriteLine(cmd)                              
                      | None ->
                          // Normal UCI
                          logTrace (sprintf "Writing to %s: %s" name s)
                          proc.StandardInput.WriteLine(s)
            else
                printfn "Warning: Attempted to write to disposed engine %s" name  
        with                    
        | ex ->
            printfn "Error writing to engine %s: %s" name ex.Message
          
      let read () = proc.StandardOutput.ReadLine()
      let readAsync () = proc.StandardOutput.ReadLineAsync()
      let readAsyncWithTimeout (token: CancellationToken) = 
          task {
            try
                let! line = proc.StandardOutput.ReadLineAsync(token)
                match winboardHandler with
                | Some handler when not (String.IsNullOrWhiteSpace line) ->
                    match handler.ProcessOutput(line) with
                    | Some translatedLine -> return translatedLine
                    | None -> return line
                | _ -> return line
            with
            | :? OperationCanceledException ->
                // Read was cancelled - return null marker
                return null
            | :? System.ArgumentOutOfRangeException ->
                // StreamReader can throw this when the underlying stream is closed/cancelled
                return null
            | :? System.IO.IOException as ioex ->
                logCritical (sprintf "IO error reading engine output: %s" ioex.Message)
                return null
            | ex ->
                // Unexpected error - log and return null to avoid crashing the host
                logCritical (sprintf "Unexpected error reading engine output: %s" ex.Message)
                return null
          }

      //let readAsyncWithTimeout (token: CancellationToken) = proc.StandardOutput.ReadLineAsync(token).AsTask()
      let getDiagnostics() =
        // Snapshot under the lock, format outside: this is only called when a game was
        // adjudicated because the engine died, and everything the buffer still holds goes
        // to the log. The dropped count is reported rather than hidden, so a truncated
        // history is visible instead of being mistaken for the whole story.
        let lines, dropped =
          lock stderrLock (fun () -> stderrBuffer.ToArray(), stderrDropped)
        let header =
          sprintf "Engine: %s | ExitCode: %s | Stderr lines: %d%s"
            name (match lastExitCode with Some c -> string c | None -> "N/A") lines.Length
            (if dropped > 0 then sprintf " (%d older lines dropped)" dropped else "")
        if lines.Length = 0 then header
        else
          sprintf "%s%s%s stderr:%s%s" header Environment.NewLine name Environment.NewLine
            (String.concat Environment.NewLine lines)

      let readUciOptions() =
          try
            match winboardHandler with
            | Some handler ->
                // Winboard initialization - use the helper function with v1 detection
                let forceV1 = config.WinboardConfig |> Option.map (fun wbc -> wbc.ForceV1Mode) |> Option.defaultValue false
                initializeWinboard proc handler logger name 30000 forceV1 |> Async.RunSynchronously
    
            | None ->
                // Original UCI initialization
                use cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float 120000))
                write("uci")          
                let rec readUci() = async {
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
                    if not isOk then
                      logCritical (sprintf "Engine %s did not respond with uciok" name)
                    else
                      logInformation (sprintf "Engine %s responded with uciok" name)
                    return isOk }
                readUci() |> Async.RunSynchronously
          with
            | :? OperationCanceledException -> 
                logCritical (sprintf "|||||Timeout after %d ms in ReadUci |||||" 120000)
                false
            | :? System.IO.IOException as ex ->
                logCritical (sprintf "Error reading UCI options: %s" ex.Message)
                false
            |ex -> 
                logCritical (sprintf "An unexpected error occurred while reading UCI options for %s: \n%s" name ex.Message)
                false
      
      let startProcess() =
        try
            assignThread ()
            let ok = readUciOptions()
            if not ok then
              logCritical "Engine did not respond to UCI command."
              failwith "Engine did not respond to UCI command."
            //configCmds |> Seq.iter (fun cmd -> printfn "%s"  (sprintf "Initial command for %s: %s" name cmd))
            //let options = createVerifiedOptions configCmds
            //options |> Seq.iter (fun cmd -> printfn "%s" (sprintf "Verified command for %s: %s" name cmd))
            if proc <> null && not proc.HasExited then
              logDebug (sprintf "Engine %s is already running." name)
            else
              assignThread ()
              logDebug (sprintf "Engine %s started successfully." name)
            let verifiedCommands = createVerifiedOptions configCmds 
            for cmd in verifiedCommands do    
              match UciOption.parseSetOptionCommand cmd with
              | Some (name, value) ->
                  if UciOption.validateSetOption optionsMap (name, value) then                    
                    //cmdUci <- cmdUci.Replace(name, optionsMap.[name].Name)
                    match UciOption.getNoneDefaultSetOption optionsMap (name, value) with
                    | Some (name, def,value) -> nonDefaultValues.[name] <- (def,value)
                    | None -> ()
                  if validate && UciOption.validateSetOption optionsMap (name, value) |> not then
                    passed <- false               
                    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." name value)
              | None ->
                  passed <- false
                  RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)
              write cmd
              assignNetworkName cmd        
              commands.Add cmd      
            let nOk, uciNameOpt = optionsMap.TryGetValue "name" 
            let aOk, uciAuthorOpt = optionsMap.TryGetValue "author"      
            // Only access OptionType if TryGetValue succeeded (for UCI engines)
            // Winboard engines won't have these options in optionsMap
            match nOk, aOk with
            | true, true ->
                match uciNameOpt.OptionType, uciAuthorOpt.OptionType with
                // Engines restart every game; the version is worth one line per run.
                | UciOption.IdAndAuthor(_,_,n), UciOption.IdAndAuthor(_,_,a) ->
                    if lock printLock (fun () -> loggedVersions.Add name)
                    then logInformation (sprintf "Engine %s is %s by %s" name n a)
                    else logDebug (sprintf "Engine name: %s and author: %s" n a)
                | _ -> ()
            | _ -> ()
      
            //write "ucinewgame"
            if validate then
                if passed then
                  lock printLock (fun () ->
                    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "All setoptions passed validation for %s" name)
                    if printedEngines.Add(name) then
                      printNonDefaultValues()
                    elif not (String.IsNullOrEmpty config.DeviceOption) then
                      match nonDefaultValues.TryGetValue(config.DeviceOption) with
                      | true, (_, value) ->
                          printfn "  %s: %s = %s" name config.DeviceOption value
                      | _ -> ()
                    let diagnostics = getDiagnostics()
                    if String.IsNullOrEmpty diagnostics |> not then
                       RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkYellow (sprintf "Engine diagnostics: %s" diagnostics))
                else
                  RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Some setoptions did not pass validation (check for red lines in console) for %s" name)
                  raise (System.Exception(sprintf "Some setoptions did not pass validation for %s" name))
        with
        | :? OperationCanceledException ->
            passed <- false
            logCritical "Engine initialization timed out."
        | :? Channels.ChannelClosedException ->
            passed <- false
            logCritical "Engine channel was closed unexpectedly."
        | ex ->
            passed <- false
            logCritical (sprintf "An unexpected error occurred while starting engine %s: \n%s" name ex.Message)      
      
      do
         startProcess ()
         
      member _.GetExitCode() = lastExitCode
      /// Snapshot rather than the live list — callers enumerate while the process may still
      /// be writing.
      member _.ErrorOutput = lock stderrLock (fun () -> stderrBuffer.ToArray()) :> seq<string>
      member _.LastExitCode = lastExitCode
      member _.GetDiagnostics() = getDiagnostics()
      member _.GetVerifiedCommands() = createVerifiedOptions configCmds
      member _.InitCommands = initCommands
      member _.Process = proc
      member _.PrintNonDefaultValues = printNonDefaultValues
      member _.IsLc0 = isLc0
      member _.Write (s:string) = write s
      member _.Commands = commands
      member _.PrintUCI() = printCommands()
      member _.Network = network
      member val Name = name with get,set
      member _.FullName = if network <> "" then $"{name} with net: {network}" else name

      member val IsReference = isReference with get, set
      /// True if the Winboard engine supports reuse (per CECP spec, defaults to true).
      /// Always true for UCI engines.
      member _.CanReuseWinboard =
          match winboardHandler with
          | Some handler -> handler.CanReuse
          | None -> true
      member this.GetDefaultOptions() = getAllDefaultOptions()
      
      member this.TryToUpdateOption (name:string) (value:string) =        
        let mutable option = String.Empty
        for opt in optionsMap do
            if opt.Key.ToLower().Contains (name.ToLower()) then
              option <- opt.Key
        match optionsMap.TryGetValue option with
        | true, opt -> 
            this.AddSetOption (EngineOption.Create opt.Name value)
        | false, _ -> ()

      member this.AddSetOptions (config:EngineOption array) =
        this.Stop()
        for option in config do
          this.AddSetOption option

      member this.AddSetOption (option:EngineOption) =
        match UciOption.tryFindOption optionsMap option.Name with
        | Some opt ->
            this.Stop()
            let cmd = sprintf "setoption name %s value %s" opt.Name option.Value      
            write cmd
            this.Config.Options[option.Name] <- option.Value // todo - maybe use opt.Value here?
            assignNetworkName cmd
            addCommand initialCommands cmd
            addCommand commands cmd            
        | _ -> logDebug (sprintf "Option not found for engine %s: %s" name option.Name)

      member _.SetMoveOverhead (optionName:string, milliSeconds: int) =
        let option = optionsMap.Keys |> Seq.tryFind (fun e -> e.ToLower().Contains(optionName.ToLower()))
        match option with
        | Some optName ->
            match UciOption.tryFindOption optionsMap optName with
            | Some option ->
                match option.OptionType with        
                | UciOption.Spin (min, max, _) -> 
                    let intValue = int64 milliSeconds
                    if intValue >= min && intValue <= max then
                      match config.Options |> Seq.tryFind (fun e -> e.Key = optName) with
                      | Some _ -> ()
                      | None ->
                          //create a setoption based on the option and the intValue
                          let cmd = sprintf "setoption name %s value %d" option.Name intValue                
                          write cmd
                          addCommand initialCommands cmd
                          addCommand commands cmd                  
                | _ -> ()
            | None -> logInformation (sprintf "Option not found or value not valid for engine %s: %s value: %d" name optionName milliSeconds)
        | None -> logInformation (sprintf "Option not found or value not valid for engine %s: %s value: %d" name optionName milliSeconds)
      
      member this.StartProcess() = startProcess()
        
      member this.DoNotValidate() = validate <- false
      member this.StopProcess() =
        shutdownRequested <- true   // as deliberate as quit; the exit is not unexpected
        try
          if proc.HasExited then
            logInformation (sprintf "Engine %s has already exited" name)
          else
            terminateProcess proc 3000
        with
        | :? InvalidOperationException ->
            logCritical (sprintf "Engine %s: process was never started or is in a bad state — check engine path, permissions, and dependencies" name)        
    
      member _.PassedValidation = passed
      member this.BenchmarkLC0Cmd = benchMarkLC0Cmd
      member this.ShowCommands = printCommands
      member this.Config = config
      member this.Path = config.Path
      member this.GetUCICommands() = this.Uci() //optionsMap
      member _.GetOptionsMap() = optionsMap
      /// Engine's self-declared identity from the UCI handshake ("id name ..."),
      /// or "" if not (yet) received. More reliable than config display Name or Path.
      member _.UciIdName =
        match UciOption.tryFindOption optionsMap "name" with
        | Some { OptionType = UciOption.UciOptionType.IdAndAuthor(_, _, value) } -> value
        | _ -> ""
    
      member this.Uci() =
        let cmd = "uci"        
        write cmd
        commands.Add cmd

      member this.IsReady() =
        let cmd = "isready"        
        write cmd
        commands.Add cmd

      member this.UciNewGame() =
        let cmd = "ucinewgame"        
        write cmd
        commands.Add cmd

      member this.Position(position: string) =
        let cmd = position        
        write cmd
        commands.Add cmd

      member this.PositionGoFen (fen: string) =
        let position = sprintf "position fen %s" fen        
        write position
        commands.Add position
    
      member this.Analyse(fenPosition: string) =
        this.UciNewGame()
        let pos = sprintf "position fen %s" fenPosition
        //position startpos moves e2e4 e7e5
        write pos
        commands.Add pos
        this.Go(100)
        //write ("go infinite")

      member this.Go(timeInMs: int) =
        isRunning <- true
        let cmd = sprintf "go movetime %d" timeInMs
        write cmd
        commands.Add cmd

      member this.Go (timeControl : UnionType, wTime, bTime) =
        isRunning <- true
        let cmd = TimeControlCommands.uciTimeCommand timeControl wTime bTime
        write cmd
        commands.Add cmd

      member this.GoNodes (nodes:int) =
        isRunning <- true
        let cmd = TimeControlCommands.createNodes nodes
        write cmd
        commands.Add cmd

      member this.GoValue () =
        isRunning <- true
        let cmd = "go value"
        write cmd
        commands.Add cmd

      member this.Stop() =
        isRunning <- false
        let cmd = "stop"
        write cmd
        commands.Add cmd

      member this.Quit() = 
        shutdownRequested <- true
        let cmd = "quit"        
        write cmd
        commands.Add cmd
      
      member this.PonderHit() = 
        let cmd = "ponderhit"        
        write cmd
        commands.Add cmd
      
      member this.GoPonder command = 
        //let cmd = "go ponder"        
        write command
        commands.Add command
    
      member this.ReadLineAsync() = readAsync()
      member this.ReadLineAsyncWithTimeout(token: CancellationToken) = readAsyncWithTimeout token
      member this.ReadLine() = read()
    
      member this.ReadUciOptions() = readUciOptions()         

      member this.WaitForReadyOk(?timeoutMs: int) = 
        let timeoutInMs = defaultArg timeoutMs defaultTimeoutMs
        let timeoutInMs = if timeoutInMs < 60000 then defaultTimeoutMs else timeoutInMs
        let cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(float timeoutInMs))
        let rec readUntilReady() = async {
              try
                if this.HasExited() then
                  logCritical (sprintf "Engine %s has exited while waiting for readyok" name)
                  return false
                elif cts.Token.IsCancellationRequested then
                  logCritical (sprintf "|||||Timeout after %d ms in WaitForReadyOk |||||" timeoutInMs)
                  return false
                else
                  let! line = this.ReadLineAsyncWithTimeout(cts.Token) |> Async.AwaitTask

                  if isNull line then
                    logCritical (sprintf "Engine %s: read returned null while waiting for readyok" name)
                    return false
                  elif line = "readyok" then
                    // Every caller lands here; GameInitialization logs the milestone.
                    logDebug (sprintf "Engine %s responded with readyok" name)
                    return true
                  else
                    do! Async.Sleep 100
                    return! readUntilReady()
              with
              | :? OperationCanceledException -> 
                  logCritical (sprintf "|||||Timeout after %d ms in WaitForReadyOk |||||" timeoutInMs)
                  return false
              | ex ->
                  logCritical (sprintf "Error in WaitForReadyOk: %s" ex.Message)
                  return false
            }
        match winboardHandler with
        | Some handler when not handler.Features.Ping ->
            // Winboard v1 engine without ping support - assume ready after initialization
            logInformation (sprintf "Engine %s is using Winboard protocol without ping support, assuming ready" name)
            true
        | Some handler ->
            // Winboard v2 with ping support - wait for pong/readyok
            let cts = new CancellationTokenSource(TimeSpan.FromSeconds(1.0))
            write "isready"            
            let res = readUntilReady() |> Async.RunSynchronously
            cts.Dispose()
            if res then res
            else
                // If ping/readied failed for Winboard engine, assume ready to avoid blocking game start
                logDebug (sprintf "Engine %s did not respond to ping/isready; assuming ready" name)
                true
        | None ->
            // Normal UCI engine
            write "isready"
            let res = readUntilReady() |> Async.RunSynchronously
            cts.Dispose()
            res
      
      member this.IsRunning
        with get () = isRunning 
        and set (v) = isRunning <- v

      member this.HasExited() = 
        try
            if proc = null then true 
            else proc.HasExited
        with
        | :? ObjectDisposedException -> true
        | :? InvalidOperationException -> true
        | _ -> true
      
      //member this.HasExited() = if proc= null then true else proc.HasExited

module EngineHelper =
  open Engine
  
  //create setoption commands for contempt settings
  let createContemptSetoptions (settings: (string*obj) seq) =
    let options = ResizeArray<EngineOption>()
    let settings = settings |> Seq.toArray
    for (key,v) in settings do
      let value = v.ToString()
      let opt = EngineOption.Create key value
      options.Add opt    
    options


  let contemptPlaySettings : ((string*obj) array) =
    [|
      ("Contempt", box 200)
      ("ContemptMode", box "play")
      ("WDLCalibrationElo", box 2200)
      ("WDLContemptAttenuation", box 0.6)
      ("WDLDrawRateReference", box 0.61)
      ("WDLEvalObjectivity", box 1.0)
    |]
  
  let valueHeadCommands =
    [    
      sprintf "setoption name %s value %f" "DirichletNoiseEpsilon" 1.0
      sprintf "setoption name %s value %d" "DirichletNoiseAlpha" 10000000
      sprintf "setoption name %s value %f" "MaxOutOfOrderEvalsFactor" 0.0
      sprintf "setoption name %s value %f" "FpuValue" 10.0
      sprintf "setoption name %s value %s" "FpuStrategy" "absolute"
      sprintf "setoption name %s value %d" "MinibatchSize" 256
      sprintf "setoption name %s value %d" "Threads" 1
      sprintf "setoption name %s value %b" "VerboseMoveStats" true
      sprintf "setoption name %s value %b" "ValueOnly" true
    ]
  
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

  let createAltEngine (callback, config:EngineConfig, logger:ILogger, writeToConsole:bool) : ChessEngineWithUCIProcessing =
      let validation = Configuration.Validation.validateChessEngineCmds config
      match validation with
      |Configuration.Validation.Errors errors ->
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Configuration.Validation.Ok ->
          let cmds = createInitialUCICommands config
          new ChessEngineWithUCIProcessing(callback, config, cmds, logger, writeToConsole, logToFile = true)

  let rec waitForEngineIsReady (delay:int) (engine: ChessEngine) =
    async {
        try
          if delay > 0 then
            do! Async.Sleep(delay*1000)
          if engine.HasExited() then
            engine.StartProcess()
          let ready = engine.WaitForReadyOk()
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

  let initEngine delay (engine: ChessEngine)  =
    async {
      if engine.HasExited() |> not && (delay > 0) then
        do! Async.Sleep(delay)
      if engine.HasExited() then
        engine.StartProcess()      
      let ok = engine.WaitForReadyOk() // wait for readyok
      if not ok then
          failwith "Engine did not respond to isready command."            
      else
          printfn "Engine %s isready" engine.Name
    } |> Async.RunSynchronously

  let initEngines delay (engine1: ChessEngine) (engine2: ChessEngine) =
    async {
      if engine1.HasExited() |> not && engine2.HasExited() |> not && (delay > 0) then
        do! Async.Sleep(delay)
      elif engine1.HasExited() || engine2.HasExited() then
        if engine1.HasExited() then
          engine1.StartProcess()
        if engine2.HasExited() then
          engine2.StartProcess()
        let res =
          [waitForEngineIsReady delay engine1; waitForEngineIsReady delay engine2]
          |> Async.Parallel 
          |> Async.RunSynchronously
        for e in res do
          printfn "%s" e } |> Async.RunSynchronously

  let createCeresEnginesFromFolder (folderName: string) : seq<EngineConfig> =
      seq {
          let template = Directory.GetFiles(folderName, "*.json") |> Seq.tryHead
          match template with
          | Some templateFile ->
              let ceresWeightFiles = Directory.GetFiles(folderName, "*.onnx")
              for weight in ceresWeightFiles do
                  let engineConfig = JSON.readEngineDef folderName templateFile
                  engineConfig.NetworkPath <- folderName
                  for kvb in engineConfig.Options do
                      if kvb.Key.ToLower().Contains("weightsfile") then
                          let name = Path.GetFileNameWithoutExtension(weight)
                          engineConfig.Options[kvb.Key] <- weight
                          engineConfig.Name <- $"Ceres {name}"
                          yield engineConfig
          | None -> ()
      }

  let createLc0EnginesFromFolder (folderName: string) : seq<EngineConfig> =
      seq {
          let template = Directory.GetFiles(folderName, "*.json") |> Seq.tryHead
          match template with
          | Some templateFile ->
              let lc0WeightFiles = 
                  Directory.GetFiles(folderName, "*.*") 
                  |> Seq.filter (fun f -> not (f.Contains("json")))
              for weight in lc0WeightFiles do
                  let engineConfig = JSON.readEngineDef folderName templateFile
                  engineConfig.NetworkPath <- folderName
                  for kvb in engineConfig.Options do
                    if kvb.Key.ToLower().Contains("weightsfile") then
                        let name = Path.GetFileNameWithoutExtension(weight)
                        engineConfig.Options[kvb.Key] <- weight
                        engineConfig.Name <- $"Lc0 {name}"
                        yield engineConfig
          | None -> ()
      }

  let createEnginesFromFolder (folderName: string) : seq<EngineConfig> =
      seq {
          let subFolders = 
              Directory.GetDirectories(folderName) 
              |> Seq.filter (fun e -> not (e.ToLower().Contains("trt")))

          let files = Directory.GetFiles(folderName, "*.onnx")
          if not (Seq.isEmpty files) then
              yield! createCeresEnginesFromFolder folderName
          else
              yield! createLc0EnginesFromFolder folderName
        
          for subFolder in subFolders do
            let files = Directory.GetFiles(subFolder, "*.onnx")
            if not (Seq.isEmpty files) then
                yield! createCeresEnginesFromFolder subFolder
            else
                yield! createLc0EnginesFromFolder subFolder
      }


  type EvalResult = { EngineName: string; Eval: float  }

  let analyzePosition (engine1: ChessEngine) (engine2: ChessEngine) (position: string) (time: int) =
      let board = new Chess.Board()
      board.PlayFenWithMoves position
      let getEval (engine: ChessEngine) =
          async {
              engine.UciNewGame()
              let fenAndMoves = board.PositionWithMoves()
              engine.Position fenAndMoves
              engine.Go(time) // Adjust the time as needed
              let! line = engine.ReadLineAsync() |> Async.AwaitTask
              let eval = 
                  match EngineProtocol.Regex.getEssentialData line (board.Position.STM = 0uy) with
                  | Some (_, eval, _, _, _, _, _, _, _) -> eval
                  | None -> failwith "Failed to get evaluation"
              return { EngineName = engine.Name; Eval = eval.Value }
          }

      async {
          let! eval1 = getEval engine1
          let! eval2 = getEval engine2
          let evalDiff = eval1.Eval - eval2.Eval
          printfn "Engine %s evaluation: %f" eval1.EngineName eval1.Eval
          printfn "Engine %s evaluation: %f" eval2.EngineName eval2.Eval
          printfn "Evaluation difference: %f" evalDiff
          return evalDiff
      } |> Async.RunSynchronously


  let playMovesFromFen (engine1: ChessEngine) (engine2: ChessEngine) (fen: string) (time: int) (numberOfMoves: int) =
      let board = new Chess.Board()
      board.LoadFen fen

      let getBestMove (engine: ChessEngine) =
          async {
              engine.UciNewGame()
              let fenAndMoves = board.PositionWithMoves()
              engine.Position fenAndMoves
              engine.Go(time) // Adjust the time as needed
              let mutable cont = true
              let mutable infoString = ""            
              while cont do
                let! line = engine.ReadLineAsync() |> Async.AwaitTask                  
                if line.StartsWith "bestmove" then          
                  cont <- false
                  infoString <- line    
              //example output is: "bestmove e2e4 ponder e7e5"
              let move = infoString.Split().[1]
              return move
          }

      let rec playMoves (moveCount: int) =
          async {
              if moveCount > 0 then
                  let! bestMove1 = getBestMove engine1                
                  match tryGetTMoveFromUciNotation &board bestMove1 with
                  |Some tmove ->
                    let shortSan = getSanNotationFromTMove &board tmove
                    board.SanMovesPlayed.Add(shortSan)
                    board.UciMovesPlayed.Add(bestMove1)
                    board.MakeMove &tmove
                    let pos = board.Position
                    let boardState = MoveGeneration.PositionOpsToString(($"Board after Engine {engine1.Name} move {bestMove1}:\n"), &pos)
                    printfn "%s" boardState
                  |_ -> ()

                  let! bestMove2 = getBestMove engine2
                  match tryGetTMoveFromUciNotation &board bestMove2 with
                  |Some tmove ->
                    let shortSan = getSanNotationFromTMove &board tmove
                    board.SanMovesPlayed.Add(shortSan)
                    board.UciMovesPlayed.Add(bestMove2)
                    board.MakeMove &tmove
                    let pos = board.Position
                    let boardState = MoveGeneration.PositionOpsToString(($"Board after Engine {engine2.Name} move {bestMove2}:\n"), &pos)
                    printfn "%s" boardState
                  |_ -> ()

                  return! playMoves (moveCount - 1)
              else
                  return ()
          }

      playMoves numberOfMoves |> Async.RunSynchronously
      let moveHistory = board.GetSanMoveHistory()
      printfn "\nMove history: %s\n" moveHistory

module HardwareInfo =
  open Engine
  open EngineHelper
  open System.Collections.Concurrent

  let private footprintCache = ConcurrentDictionary<string, uint64>()

  let private footprintCacheKey (config: EngineConfig) =
        let hash =
            match config.Options.TryGetValue("Hash") with
            | true, v -> string v
            | _ -> ""
        let threads =
            match config.Options.TryGetValue("Threads") with
            | true, v -> string v
            | _ -> ""
        sprintf "%s|H=%s|T=%s" config.Path hash threads

  let estimateEngineRam (config: EngineConfig) : uint64 =
        let key = footprintCacheKey config
        footprintCache.GetOrAdd(key, fun _ ->
            let engine = createEngine (config, None)
            initEngine 0 engine
            // snapshot working‐set
            let ws = uint64 engine.Process.WorkingSet64
            // clean up
            engine.StopProcess()
            ws)

  /// Sequentially walk your configs, measuring memory usage one at a time, after each measurement we GC to reclaim EVERYTHING.
  let footPrints (configs: EngineConfig seq) : (string * uint64)[] =
    let results = ResizeArray<string * uint64>()
    for cfg in configs do
        try
            let mem = estimateEngineRam cfg
            results.Add(cfg.Name, mem)
        with ex ->
            // log and continue if one engine fails
            printfn "⚠️  Measuring %s failed: %s" cfg.Name ex.Message
        // force native cleanup before next iteration
        GC.Collect()
        GC.WaitForPendingFinalizers()
    results.ToArray()
  
  let sumFootprints configs =
    footPrints configs |> Array.sumBy snd

  let concurrencyLevel (configs: EngineConfig seq) requested =
    //printfn "Requested concurrency: %d - calculated concurrency requirement" requested
    if requested = 1 then
        1
    else            
        let totalAvail = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> uint64
        let headroomBytes = totalAvail / 2UL
        let footprints = 
            let sum = sumFootprints configs
            printfn "Sum of one copy each ≈ %d MB" (sum/1_048_576UL)
            sum
        
        let maxSets = int (headroomBytes / footprints)
        let concurrencyNum = min requested maxSets
        //printfn "Max sets of engines = %d" maxSets
        printfn "Using concurrency = %d" concurrencyNum
        concurrencyNum        
  
  let getThreads (engine:EngineConfig) =
        let totalCores = Environment.ProcessorCount - 1
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
        if threadValue > totalCores then totalCores else threadValue

  let sumEngineThreads (engines : ChessEngine seq) = engines |> Seq.sumBy(fun e -> getThreads e.Config)
  
  let assessMaxCpuConcurrencyLevel (engines : ChessEngine seq) =
    let totalThreads = sumEngineThreads engines
    let totalCores = Environment.ProcessorCount - 1
    let maxGames = totalCores / totalThreads
    maxGames
