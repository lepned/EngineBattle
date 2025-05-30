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
open Utilities
open LowLevelUtilities
open TypesDef.CoreTypes
open TypesDef.Engine
open TypesDef.TMove
open TypesDef.Position
open TypesDef.TimeControl
open Chess.BoardUtils

module Engine =

  // Define a class to manage the chess engine process and communicate with it 
  type ChessEngineWithUCIProcessing (callback, config : EngineConfig, initCommands: string seq)  =
      let isLc0 = (Regex.Match(config.Path, "lc0", RegexOptions.IgnoreCase)).Success
      let isCeres = (Regex.Match(config.Path, "ceres", RegexOptions.IgnoreCase)).Success
      let mutable board = Chess.Board()
      let mutable commands = ResizeArray<string>()
      let isChess960Set () = 
          commands |> Seq.tryFindBack (fun e -> e.Contains "UCI_Chess960")         
          |> fun e -> match e with |Some e -> e.Contains "true" |_ -> false

      let mutable state = EngineState.Start
      let mutable numberOfNodes = 0L
      let mutable evalList : EvalType list = []
      let mutable fullEvalList :EvalType list = []
      let mutable nps = 0.0
      let mutable depth = 0
      let mutable Player1PV = String.Empty
      let mutable callbackFunc : EngineUpdate -> unit = callback //fun update -> ()
      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let name = config.Name
      let configCmds = initCommands    
      let mutable backend = ""
      let mutable isReference = false
      let mutable inUciResponsMode = true
      let mutable inIsreadyMode = false
      let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
      let optionsMap = System.Collections.Generic.Dictionary<string, UciOption.UciOption>()
      let engineProcess = new Process()
      let dict = System.Collections.Generic.Dictionary<string, obj>()
      let nonDefaultValues = System.Collections.Generic.Dictionary<string, (string * string)>()

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
        LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
        for opt in nonDefaultValues do
          let (def, value) = opt.Value
          if String.IsNullOrEmpty def then 
            LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
          else
            LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
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
                let mode =
                    if uciMode = "uci" then inUciResponsMode else inIsreadyMode
                if mode then
                    do! Async.Sleep(200)
                    if cancellationToken.IsCancellationRequested then
                        // Handle timeout here and print a message including the ucimode
                        printfn "Timeout for %s" uciMode
                        ok <- false; cont <- false                      
                    else
                        ok <- true
                else
                    cont <- false
            printfn "%s responded with uciMode %s" config.Name uciMode
            return ok
        } |> Async.RunSynchronously

      let bestLine callback (engineName:string) (line:string)  =        
        let move, ponder = line.Split().[1], if line.Contains "ponder" then (line.Split().[3]) else ""
        callback (Done engineName)
        match tryGetTMoveFromCoordinateNotation &board move with
        |Some tmove ->
          //let mutable moveAdj = tmove
          let shortSan = getSanNotationFromTMove &board tmove    
          let eval = 
            if evalList.Length > 0 then 
              evalList.[0] 
            else 
              if fullEvalList.Length > 0 then fullEvalList[0] else EvalType.CP 0.0
          let pv = Player1PV
          let fen = BoardHelper.posToFen board.Position
          let moveDetail = 
            {
              LongSan = move
              FromSq = move[0..1]
              ToSq = move[2..3]
              Color = "w"
              IsCastling = (tmove.MoveType &&& TPieceType.CASTLE <> TPieceType.EMPTY) 
              }                 
          let moveAndFen = {Move = moveDetail; ShortSan=shortSan; FenAfterMove = fen}
          let mutable posToCheck = board.Position
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
          let boardState = $"Board state: {board.FEN()} {board.CurrentFEN} {board.CurrentIndex} "
          printfn "%s" msg
          printfn "%s" boardState

      let regular callback (engineName:string) (line:string) =
        let mutable avgNps = 0.0
        let isWhite = board.Position.STM = 0uy
        match Utilities.Regex.getEssentialData line isWhite with
        |Some (d, eval, nodes, nps, pvLine, tbHits, wdl, sd, mPv ) ->         
          numberOfNodes <- nodes
          if d > depth then
            depth <- d        
          evalList <- eval :: evalList
          let mPv = if mPv = 0 then 1 else mPv        
          if not (String.IsNullOrEmpty(pvLine)) && mPv = 1 then
              Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
          let pvUpdate = if mPv = 1 then Player1PV else getShortSanPVFromLongSanPVFast moveList &board pvLine       
          let status = 
            { 
              PlayerName = engineName
              Eval = eval
              Depth = d
              SD = sd
              Nodes = nodes
              NPS = float nps
              TBhits = tbHits
              WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
              PV = pvUpdate
              PVLongSAN = pvLine
              MultiPV = mPv
            }
          callback (Status status)
                      
        |None -> ()

      let processLine callback (name:string) (line:string)  =
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
              //printfn "%s" line
              if line.StartsWith "info string node" |> not then
                let nn = Utilities.Regex.getInfoStringData name line
                list.Add nn
              else
                let nn = Utilities.Regex.getInfoStringData name line
                if nn.Nodes = 1 then //will only be one node in policy test like go nodes 1
                  let bp = list |> Seq.maxBy(fun e -> e.P)
                  bp.Q <- nn.Q
                list.Add nn
            |_ -> 
              let list = ResizeArray<NNValues>()  
              if line.StartsWith "info string node" |> not then
                let nn = Utilities.Regex.getInfoStringData name line
                list.Add nn
              state <- InMoveStatMode list
        | line when line.StartsWith("info") ->
            state <- RegularSearchMode 
        | _  -> 
          printfn "%s" line
         
      
        match state with
        |InMoveStatMode list ->
          if line.StartsWith "info string node" then
            makeShortSan list &board
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

      let printCommands () =
        printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
        for cmd in initCommands do
          if cmd.Contains "setopt" then
            printfn "%s" cmd

      let write (s:string) = 
        if name.ToLower().Contains "igel" then
          engineProcess.StandardInput.WriteLine(s)
        else
          engineProcess.StandardInput.WriteLine(s + "\n")
    
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
            if path.Contains("/") then
              let nArr = path.Split('/')
              let index = max 0 (nArr.Length - 1)
              network <- nArr[index]
    
      let analysisCommands =
        [
          //sprintf "setoption name %s value %d" "SmartPruningFactor" 0
          sprintf "setoption name %s value %d" "MultiPV" 10
        ]

      let setupProcess() =
          let mutable pos = board.Position
          board.IsFRC <- PositionOps.isFRC &pos
          engineProcess.StartInfo.FileName <- config.Path
          engineProcess.StartInfo.UseShellExecute <- false
          engineProcess.StartInfo.RedirectStandardInput <- true
          engineProcess.StartInfo.RedirectStandardOutput <- true
          if String.IsNullOrEmpty config.Args |> not then
            printfn "Args passed: %s" config.Args
            engineProcess.StartInfo.Arguments <- config.Args
          elif isLc0 then
            engineProcess.StartInfo.Arguments <- "--show-hidden"
          engineProcess.OutputDataReceived.Add(fun args -> 
              if not (String.IsNullOrEmpty args.Data) then
                if inUciResponsMode then  
                  if args.Data = "uciok" then              
                    inUciResponsMode <- false
                  else
                    //printfn "Engine responded with: %s" args.Data
                    UciOption.addOptionToMap optionsMap args.Data
                elif inIsreadyMode then
                  //printfn "Engine responded with: %s" args.Data
                  if args.Data = "readyok" then
                    inIsreadyMode <- false
                    //set start variable to the current time and measure the duration later
                    //startTimer <- Stopwatch.GetTimestamp()                  
                else                
                  //let duration = Stopwatch.GetElapsedTime(startTimer)
                  //printfn "Duration: %A" duration
                  processLine callbackFunc name args.Data)
          engineProcess.Start() |> ignore
          engineProcess.BeginOutputReadLine()
          write "uci"
          let ok = waitForInitialization ((new CancellationTokenSource(280000)).Token) "uci" //wait for maximum 5 seconds        
          if not ok then
            failwith "Engine did not respond to UCI command."
          //else 
          //  printfn "Engine %s responded 'uciok' to uci command." name
          for cmd in configCmds do
            match UciOption.parseSetOptionCommand cmd with
            | Some (name, value) ->
                if UciOption.validateSetOption optionsMap (name, value) then                  
                    dict.[name] <- value
                    match UciOption.getNoneDefaultSetOption optionsMap (name, value) with
                    | Some (name, def,value) -> nonDefaultValues.[name] <- (def,value)
                    | None -> ()
                    LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "The option '%s' with value '%s' is valid." name value)
                else
                    LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." name value)
            | None ->
                LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)

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
          inIsreadyMode <- true
          write "isready"
          //printfn "Sent isready command here: %s" "isready"
          let ok = waitForInitialization((new CancellationTokenSource(280000)).Token) "readyok" //wait for maximum 10 seconds
          if not ok then
            failwith "Engine did not respond to isready command."
          //else
          //  printfn "Engine %s responded 'readyok' to isready command." name

          let withLiveLog = configCmds |> Seq.exists (fun e -> e.Contains("LogLiveStats"))
          getAllDefaultOptions()
          callback(EngineUpdate.Ready (name, withLiveLog))

      do
          setupProcess()
    
      member _.GetAllDefaultOptions() = dict
      member this.IsLc0 = isLc0
      member val IsFRC = board.IsFRC with get, set
      member _.Board 
        with get() = board
        and set(v) = 
          board <- v      
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
      member this.ShutDownEngine() = this.SendUCICommand UCICommand.Quit      
    
      member this.SetAllOptions (allOptions: Dictionary<string,obj>) =
        for opt in allOptions do
          let cmd = sprintf "setoption name %s value %s" opt.Key (opt.Value.ToString())
          printfn "%s" cmd
          write cmd
          assignNetworkName cmd
          commands.Add cmd
          dict.[opt.Key] <- opt.Value

      member this.SendUCICommand (command: UCICommand) =
          match command with
          | UCI ->
              let cmd = "uci"
              write cmd
              commands.Add cmd
          | Stop -> 
              let cmd = "stop"
              write cmd
              commands.Add cmd
          | Quit -> 
              let cmd = "quit"
              write cmd
              commands.Add cmd
              let hasExited = engineProcess.WaitForExit(1000)
              if not hasExited then
                engineProcess.Kill()            
              engineProcess.Close()
              engineProcess.Dispose()            
              printfn "Engine %s has been shut down." name
          | PositionWithMoves command ->
              let isSet = isChess960Set()
              if board.IsFRC && not isSet then 
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif isSet && not board.IsFRC then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              write command
              commands.Add command
          | Position fen -> 
              board.LoadFen fen
              if board.IsFRC then 
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "true"))
              elif isChess960Set() && not board.IsFRC then
                this.SendUCICommand (SetOption (EngineOption.Create "UCI_Chess960" "false"))
              let cmd = (sprintf "position fen %s" fen)
              write cmd
              commands.Add cmd
          | GoNodes nodes ->             
              let cmd = (sprintf "go nodes %d" nodes)
              write cmd

          | GoInfinite ->             
              let cmd = (sprintf "go infinite")
              write cmd
              commands.Add cmd
          | GoMoveTime timeInMs -> 
              let cmd = sprintf "go movetime %d" timeInMs
              write cmd
              commands.Add cmd
          | GoValue -> 
              let cmd = sprintf "go value"
              write cmd
              commands.Add cmd
          | GoTimeControl (tc,wTime,bTime) ->
              let cmd = TimeControlCommands.uciTimeCommand tc wTime bTime
              write cmd
              commands.Add cmd
          | UciNewGame ->
            let cmd = "ucinewgame"
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
        
          | SetMoveOverhead (optionName, ms) ->         
              match UciOption.tryFindOption optionsMap optionName with
              | Some option ->
                  match option.OptionType with        
                  | UciOption.Spin (min, max, _) -> 
                      let intValue = int ms
                      if intValue >= min && intValue <= max then
                        //create a setoption based on the option and the intValue
                        let cmd = sprintf "setoption name %s value %d" option.Name intValue
                        write cmd
                  | _ -> ()
              | None -> printfn "Option not found: %s value: %d" optionName ms


  type PuzzleChessEngine (config : EngineConfig, initCommands: string seq) =    
      let isLc0 = (Regex.Match(config.Path, "lc0", RegexOptions.IgnoreCase)).Success
      let mutable uciDict = Dictionary<string,string>()
      let mutable lookup = fun query -> Utilities.UCI.createDefaultSetOptionCommandForName uciDict query   
      let name = config.Name    
      let configCmds = initCommands
      let mutable proc = defaultof<Process>
      let mutable commands = ResizeArray<string>()
      let mutable network = ""
    
      let backendCommand () =
        let backendOpt = config.Options |> Seq.find (fun e -> e.Key = "BackendOptions")
        let backendOptValue = backendOpt.Value.ToString()
        let backend = config.Options |> Seq.find (fun e -> e.Key = "Backend") 
        let backendV = backend.Value.ToString()
        sprintf "setoption name %s value %s" backend.Key backendV,
        sprintf "setoption name %s value %s" backendOpt.Key backendOptValue

      let printCommands () =
        printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
        for cmd in initCommands do
          if cmd.Contains "setopt" then
            printfn "%s" cmd

      let terminateProcess (engineProcess: Process) (timeout: int) =
        // Wait for the process to exit
        let hasExited = engineProcess.WaitForExit(timeout)

        // If the process hasn't exited, attempt to kill it
        if not hasExited then
            try
                engineProcess.Kill() 
            with
            | :? InvalidOperationException ->
                // Process has already exited
                printfn "%s has already exited when process tried to kill it" name
      
        // Close and dispose the process
        engineProcess.Close()
        engineProcess.Dispose()
   
      let assignThread () = 
          let task =  Task.Factory.StartNew(
            fun () -> 
                  let engine = new Process()
                  engine.StartInfo.FileName <- config.Path
                  engine.StartInfo.UseShellExecute <- false
                  engine.StartInfo.RedirectStandardInput <- true
                  engine.StartInfo.RedirectStandardOutput <- true
                  if isLc0 then
                    engine.StartInfo.Arguments <- "--show-hidden"
                  if engine.Start() then
                    proc <- engine                  
                    //printfn "%s started" name
                  else
                    printfn "%s could not be started" name             
          )
          task.Wait()
    
      let mutable isRunning = false
      let mutable isReference = false
    
      let write (s:string) =       
          proc.StandardInput.WriteLine(s + "\n")
    
      let read () = proc.StandardOutput.ReadLine()
      let readAsync () = proc.StandardOutput.ReadLineAsync()

      member _.IsLc0 = isLc0
      member _.Write (s:string) = write s
      member _.PrintUCI() = printCommands()
      member _.Name = name
      member _.Network = network
      member _.Commands = commands

      member val IsReference = isReference with get, set

      member this.AddSetOptions (config:EngineOption array) =
        //this.Stop()
        for option in config do
          let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
          write cmd       
          commands.Add cmd

      member this.AddSetOption (option:EngineOption) =
        //this.Stop()
        let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
        write cmd
        commands.Add cmd

      member this.StartProcess() =   
        assignThread ()     
        for cmd in configCmds do
          write cmd        
          commands.Add cmd      
        this.IsReady()

      member this.StopProcess() =
        this.Stop()
        this.Quit()
        terminateProcess proc 3000
    
      member this.ShowCommands = printCommands
      member this.Config = config
      member this.Path = config.Path
      member this.GetUCICommands() = 
        Utilities.UCI.getUCIOptionsAsync this.Config.Path |> Async.RunSynchronously   
    
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
        let cmd = "quit"
        write cmd
        commands.Add cmd

      member this.ReadLineAsync() = readAsync()
      member this.ReadLine() = read()


  type ChessEngine(config : EngineConfig, initCommands: string seq) =
      let mutable passed = true
      let isLc0 = (Regex.Match(config.Path, "lc0", RegexOptions.IgnoreCase)).Success
      let isCeres = (Regex.Match(config.Path, "ceres", RegexOptions.IgnoreCase)).Success
      let ceresNetworkName = 
        let argsExists = String.IsNullOrEmpty config.Args |> not
        if isCeres && argsExists then
          let network = config.Args.Split(':')
          if network.Length > 1 then
            network.[1]
          else
            ""
        else
          ""
      let optionsMap = System.Collections.Generic.Dictionary<string, UciOption.UciOption>()
      let benchMarkLC0Cmd = Engine.createLC0BenchmarkString config
      let name = config.Name
      //let alias = if String.IsNullOrEmpty config.Alias then config.Name else config.Alias
      let configCmds = initCommands
      let mutable proc = defaultof<Process>
      let mutable commands = ResizeArray<string>()
      let mutable network = if isCeres then ceresNetworkName else ""
      let nonDefaultValues = System.Collections.Generic.Dictionary<string, (string * string)>()

      let printNonDefaultValues () =
        printfn "\nCustomized SetOptions for %s:\n" name
        let path = config.Path
        LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Engine path: %s" path)
        for opt in nonDefaultValues do
          let (def, value) = opt.Value
          if String.IsNullOrEmpty def then 
            LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s" opt.Key value)
          else
            LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "%s: %s - default is: %s" opt.Key value def)
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


      let printCommands () =
        printfn "%sConfigurations for %s:%s" Environment.NewLine name Environment.NewLine
        for cmd in initCommands do
          if cmd.Contains "setopt" then
            printfn "%s" cmd

      /// Attempts to terminate the given process within `timeoutMs` milliseconds.
      let terminateProcess (proc: Process) (timeoutMs: int) =
          try
              // 1) wait for it to exit on its own
              if not (proc.WaitForExit timeoutMs) then
                  // 2) still running → try to kill it
                  try
                      proc.Kill()
                  with ex ->
                      printfn "Warning: could not kill '%s': %s" name ex.Message
                  // give it a moment, then check again (and ignore the result again)
                  proc.WaitForExit 5_000 |> ignore
          finally
              // cleanup handles
              proc.Close()
              proc.Dispose()

      let assignNetworkName (option:string) =
        if option.ToLower().Contains("weights") || option.ToLower().Contains("evalfile") then
          let arr = option.Split(' ')
          let path = arr[arr.Length - 1]
          //I only want to get the network name from the path not the full path
          let name = Path.GetFileNameWithoutExtension path
          network <- name      
    
      let assignThread () = 
          let task =  Task.Factory.StartNew(
            fun () -> 
                  let engine = new Process()
                  engine.StartInfo.FileName <- config.Path
                  engine.StartInfo.UseShellExecute <- false
                  engine.StartInfo.RedirectStandardInput <- true
                  engine.StartInfo.RedirectStandardOutput <- true
                  if String.IsNullOrEmpty config.Args |> not then
                    engine.StartInfo.Arguments <- config.Args
                  elif isLc0 then
                    engine.StartInfo.Arguments <- "--show-hidden"
                  if engine.Start() then
                    proc <- engine                  
                    //printfn "\n%s started" name
                  else
                    printfn "\n%s could not be started" name             
          )
          task.Wait()
    
      let mutable isRunning = false
      let mutable isReference = false
      let mutable validate = true
    
      let write (s:string) = 
        if name.ToLower().Contains "igel" then
          proc.StandardInput.WriteLine(s)
        else
          proc.StandardInput.WriteLine(s + "\n")
    
      let read () = proc.StandardOutput.ReadLine()
      let readAsync () = proc.StandardOutput.ReadLineAsync()
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
      member this.GetDefaultOptions() = getAllDefaultOptions()
      member this.AddSetOptions (config:EngineOption array) =
        this.Stop()
        for option in config do
          let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
          write cmd
          assignNetworkName cmd
          commands.Add cmd

      member this.AddSetOption (option:EngineOption) =
        match UciOption.tryFindOption optionsMap option.Name with
        | Some _ ->
            this.Stop()
            let cmd = sprintf "setoption name %s value %s" option.Name option.Value      
            write cmd
            this.Config.Options[option.Name] <- option.Value
            assignNetworkName cmd
            commands.Add cmd
        | _ -> ()

      member this.SetMoveOverhead (optionName:string, milliSeconds: int) =
        match UciOption.tryFindOption optionsMap optionName with
        | Some option ->
            match option.OptionType with        
            | UciOption.Spin (min, max, _) -> 
                let intValue = int milliSeconds
                if intValue >= min && intValue <= max then
                  //create a setoption based on the option and the intValue
                  let cmd = sprintf "setoption name %s value %d" option.Name intValue                
                  write cmd
            | _ -> ()
        | None -> printfn "Option not found or value not valid for engine %s: %s value: %d" name optionName milliSeconds
      
      member this.StartProcess() =
        assignThread ()
        let ok = this.ReadUciOptions()
        if not ok then
          failwith "Engine did not respond to UCI command."
        for cmd in configCmds do      
          match UciOption.parseSetOptionCommand cmd with
          | Some (name, value) ->
              if UciOption.validateSetOption optionsMap (name, value) then                  
                //dict.[name] <- value
                match UciOption.getNoneDefaultSetOption optionsMap (name, value) with
                | Some (name, def,value) -> nonDefaultValues.[name] <- (def,value)
                | None -> ()
              if validate && UciOption.validateSetOption optionsMap (name, value) |> not then
                passed <- false               
                LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "The option '%s' with value '%s' is invalid." name value)
          | None ->
              passed <- false
              LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Invalid setoption command: %s" cmd)
          write cmd
          assignNetworkName cmd        
          commands.Add cmd      
        let nOk, uciNameOpt = optionsMap.TryGetValue "name" 
        let aOk, uciAuthorOpt = optionsMap.TryGetValue "author"      
        match (nOk, uciNameOpt.OptionType), (aOk, uciAuthorOpt.OptionType) with
        | (true, UciOption.IdAndAuthor(_,_,n)), (true, UciOption.IdAndAuthor(_,_,a)) -> printfn "Engine name: %s and author: %s" n a
        | _ -> ()
      
        let ok = this.WaitForReadyOk()
        write "ucinewgame"
        if not ok then
          failwith "Engine did not respond to isready command." 
        if validate then
            if passed then
              LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green (sprintf "All setoptions passed validation for %s" name)
            else
              LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Some setoptions did not pass validation (check for red lines in console) for %s" name)

      member this.DoNotValidate() = validate <- false
      member this.StopProcess() =       
        if proc.HasExited then
          printfn "Engine %s has already exited" name
        else
          this.Quit()
          terminateProcess proc 3000        
    
      member _.PassedValidation = passed
      member this.BenchmarkLC0Cmd = benchMarkLC0Cmd
      member this.ShowCommands = printCommands
      member this.Config = config
      member this.Path = config.Path
      member this.GetUCICommands() = this.Uci() //optionsMap    
    
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
        //commands.Add ("go infinite")      

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
        let cmd = "quit"
        write cmd
        commands.Add cmd

      member this.ReadLineAsync() = readAsync()
      member this.ReadLine() = read()
    
      member this.ReadUciOptions() =       
        try
          write("uci")
          let rec readUci() = async {          
              let! line = this.ReadLineAsync() |> Async.AwaitTask
              printfn "%s" line
              let mutable ret = line
              while ret <> "uciok" && not proc.HasExited  do
                  UciOption.addOptionToMap optionsMap ret
                  let! resp = this.ReadLineAsync() |> Async.AwaitTask
                  ret <- resp
              return ret = "uciok" }
          readUci() |> Async.RunSynchronously
        with
          | :? System.IO.IOException as ex ->
              printfn "Error reading UCI options: %s" ex.Message
              false
          |ex -> 
              printfn "An unexpected error occurred while reading UCI options for %s: \n%s" name ex.Message
              false

      member this.WaitForReadyOk() = 
        write "isready"
        let rec waitForReadyOk() = async {          
            let! line = this.ReadLineAsync() |> Async.AwaitTask
            if line = "readyok" then                  
                return true
            else
                return! waitForReadyOk() }
        waitForReadyOk() |> Async.RunSynchronously
    
      member this.IsRunning
        with get () = isRunning 
        and set (v) = isRunning <- v

      member this.HasExited() = if proc= null then true else proc.HasExited

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
      //sprintf "setoption name %s value %s" "WeightsFile" "C:/Dev/Chess/Networks/32930"
      sprintf "setoption name %s value %b" "ValueOnly" true
    ]
  let createInitialUCICommands (config:EngineConfig) =
      seq {
        //"uci"
        for option in config.Options do
          let mutable value = option.Value.ToString()            
          let (ok,v) = Boolean.TryParse value
          if ok then
            value <- sprintf "%b" v
          elif option.Key = "WeightsFile" && Utilities.Validation.checkPathExists value |> not then          
            let combine = Path.Combine(config.NetworkPath, value)
            value <- combine
          sprintf "setoption name %s value %s" option.Key value
        //"ucinewgame"
      }

  let createEngineWithoutValidation (config:EngineConfig) : ChessEngine = 
      let cmds = createInitialUCICommands config
      let eng = new ChessEngine(config, cmds)
      eng.DoNotValidate()
      eng

  let createEngine (config:EngineConfig) : ChessEngine = 
      let validation = Utilities.Validation.validateChessEngineCmds config
      match validation with
      |Utilities.Validation.Errors errors -> 
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Utilities.Validation.Ok -> 
          let cmds = createInitialUCICommands config
          new ChessEngine(config, cmds)

  let createPuzzleEngine (config:EngineConfig) : PuzzleChessEngine = 
      let validation = Utilities.Validation.validateChessEngineCmds config
      match validation with
      |Utilities.Validation.Errors errors -> 
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Utilities.Validation.Ok -> 
          let cmds = createInitialUCICommands config
          new PuzzleChessEngine(config, cmds)

  let createAltEngine (callback, config:EngineConfig) : ChessEngineWithUCIProcessing =     
      let validation = Utilities.Validation.validateChessEngineCmds config
      match validation with
      |Utilities.Validation.Errors errors -> 
        for error in errors do
          ConsoleUtils.printInColor ConsoleColor.Red error
        failwith "Engine could not be created"
      |Utilities.Validation.Ok -> 
          let cmds = createInitialUCICommands config
          new ChessEngineWithUCIProcessing(callback, config, cmds) 

  let rec waitForEngineIsReady (delay:int) (engine: ChessEngine) =
    async {
      if delay > 0 then
        do! Async.Sleep(delay*1000)
      if engine.HasExited() then
        engine.StartProcess() |> ignore
      engine.IsReady()
      let! line = engine.ReadLineAsync() |> Async.AwaitTask
      match line with
      | line when line.StartsWith("readyok") -> 
          return $"{engine.Name} isready"
      | _ -> 
        do! Async.Sleep(200)
        return! waitForEngineIsReady 0 engine }

  let initEngine delay (engine: ChessEngine)  =
    async {
      if engine.HasExited() |> not && (delay > 0) then
        do! Async.Sleep(delay)
      if engine.HasExited() then
        engine.StartProcess()      
        do! waitForEngineIsReady delay engine |> Async.Ignore
    } |> Async.RunSynchronously

  let initEngines delay (engine1: ChessEngine) (engine2: ChessEngine) =
    async {
      if engine1.HasExited() |> not && (delay > 0) then
        do! Async.Sleep(delay)
      if engine1.HasExited() || engine2.HasExited() then
        engine1.StartProcess()
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



  type EvalResult = {
      EngineName: string
      Eval: float
  }

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
                  match Utilities.Regex.getEssentialData line (board.Position.STM = 0uy) with
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
                  match tryGetTMoveFromCoordinateNotation &board bestMove1 with
                  |Some tmove ->
                    let shortSan = getSanNotationFromTMove &board tmove
                    board.ShortSANMovesPlayed.Add(shortSan)
                    board.LongSANMovesPlayed.Add(bestMove1)
                    board.MakeMove &tmove
                    let pos = board.Position
                    let boardState = MoveGeneration.PositionOpsToString(($"Board after Engine {engine1.Name} move {bestMove1}:\n"), &pos)
                    printfn "%s" boardState
                  |_ -> ()

                  let! bestMove2 = getBestMove engine2
                  match tryGetTMoveFromCoordinateNotation &board bestMove2 with
                  |Some tmove ->
                    let shortSan = getSanNotationFromTMove &board tmove
                    board.ShortSANMovesPlayed.Add(shortSan)
                    board.LongSANMovesPlayed.Add(bestMove2)
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
      let moveHistory = board.GetShortSanMoveHistory()
      printfn "\nMove history: %s\n" moveHistory

module HardwareInfo =
  open Engine
  open EngineHelper
  
  let estimateEngineRam (config: EngineConfig) : uint64 =
        let engine = createEngine config
        engine.StartProcess()
        initEngine 0 engine        
        // snapshot working‐set
        let ws = uint64 engine.Process.WorkingSet64
        // clean up
        engine.StopProcess()
        ws

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
