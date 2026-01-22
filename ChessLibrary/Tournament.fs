module ChessLibrary.Tournament

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Text
open System.Threading.Channels
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Unchecked
open System.Diagnostics
open Microsoft.Extensions.Logging
open System.Collections.Concurrent
open System.Text.Json
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.Parser
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef.Engine
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Cup
open ChessLibrary.TypesDef.Position
open ChessLibrary.TypesDef.TMove
open ChessLibrary.TypesDef.TimeControl
open ChessLibrary.TypesDef.Misc
open ChessLibrary.Chess
open ChessLibrary.Chess.BoardUtils
open ChessLibrary.Utilities
open ChessLibrary.LowLevelUtilities
open ChessLibrary.CustomException

module Initialization = 

  let createAsyncDelay (delay:int) =
    async { 
      do! Async.Sleep(delay) 
      return true } 

  let waitForEngineIsReady (tourny:Tournament) (engine: ChessEngine) (logger: ILogger) = async { 
    let pumpCleanupDelay = 200
    let engineStopDelay = 300
    let engineStartDelay = 2000
    let engineRecoveryDelay = 1000
    
    try         
        do! Async.Sleep pumpCleanupDelay
        
        //Handle engine process state
        let! processReady = async {
            if engine.HasExited() then
                logger.LogDebug("Engine {Engine} has exited, starting fresh", engine.Name)
                try
                    engine.StartProcess()
                    let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                    if not ok then
                       failwith "Engine did not respond to isready command."
                    do! Async.Sleep engineStartDelay
                    return true
                with ex ->
                    logger.LogError(ex, "Failed to start engine {Engine}", engine.Name)
                    return false
            else
                logger.LogDebug("Engine {Engine} still running, resetting", engine.Name)
                try
                    // Graceful stop attempt
                    engine.Stop()
                    do! Async.Sleep engineStopDelay
                    let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                    if not ok then
                        failwith "Engine did not respond to isready command."
                    return true
                with ex ->
                    logger.LogWarning(ex, "Error resetting engine {Engine}, forcing restart", engine.Name)
                    engine.StopProcess()
                    do! Async.Sleep engineRecoveryDelay
                    engine.StartProcess()
                    do! Async.Sleep engineStartDelay
                    let ok = engine.WaitForReadyOk(tourny.EngineStartupTimeoutInSec * 1000) // wait for readyok
                    if not ok then
                        failwith "Engine did not respond to isready command."
                    return true
        }
        
        if not processReady then
            return false
        else
            //Verify process is running
            if engine.HasExited() then
                logger.LogError("Engine {Engine} failed to start after all attempts", engine.Name)
                return false
            else
                engine.UciNewGame()

                //Wait for ready acknowledgment
                let timeoutInSec = max 180 tourny.EngineStartupTimeoutInSec
                let timeoutInMs = timeoutInSec * 1000                    
                let readyOk = engine.WaitForReadyOk(timeoutInMs)
                
                if readyOk then
                    logger.LogDebug("Engine {Engine} ready", engine.Name)
                    return true
                else
                    logger.LogError("Engine {Engine} timed out waiting for readyok", engine.Name)
                    return false
                
    with ex -> 
        logger.LogWarning(ex, "Exception initializing engine {Engine}, performing cleanup", engine.Name)
        
        // Cleanup on failure
        try
            if not (engine.HasExited()) then
                engine.StopProcess()
        with cleanupEx ->
            logger.LogWarning(cleanupEx, "Error during cleanup for engine {Engine}", engine.Name)
            
        return false 
  }
  
  let appendGameDescription (sb:StringBuilder) (tourny:Tournament) (player1:ChessEngine) (player2:ChessEngine) (openingMoves: ResizeArray<string>) fen =
    let append (txt:string) = sb.Append txt |> ignore
    let isEpd =
      match tourny.Opening.OpeningsPath with
      |Some path ->
            let ext = Path.GetExtension path
            ext.ToLower().Contains ".epd"
      |_ -> false
    let tcWhite = tourny.TimeControl.GetTimeConfig player1.Config.TimeControlID
    let tcBlack = tourny.TimeControl.GetTimeConfig player2.Config.TimeControlID
    let tournyData = "{TournamentOptions: " + tourny.PGNSummary() + (if isEpd then sprintf " FEN=%s;" fen else "")
    let moveOverheadMs = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds
    let whiteEngineData = $" WhiteEngineOptions: TimeControl: {tcWhite.ToString()}; {player1.Config.Information moveOverheadMs}"
    let blackEngineData = $"BlackEngineOptions: TimeControl: {tcBlack.ToString()}; {player2.Config.Information moveOverheadMs}"
    let wCmds = if player1.IsLc0 then $" (White commands: {UciOptions.createCommandsFromConfig player1.Config})" else ""
    let bCmds = if player2.IsLc0 then $" (Black commands: {UciOptions.createCommandsFromConfig player2.Config})" else ""
    let whiteArgs, blackArgs = player1.Config.Args, player2.Config.Args
    append tournyData
    append whiteEngineData
    if String.IsNullOrEmpty whiteArgs |> not then append $" (Args: {whiteArgs})"
    append blackEngineData
    if String.IsNullOrEmpty blackArgs |> not then append $" (Args: {blackArgs})"
    append wCmds
    append bCmds
    append ("}" + Environment.NewLine)
    if openingMoves.Count > 0 then
      let opMoves = PGNWriter.writeOpeningPGNMoves openingMoves
      append opMoves
  
  let checkAndPrepareContempt (engine1: ChessEngine) (engine2: ChessEngine) =   
    let hasKeyCI (options: IDictionary<string, 'a>) (key: string) =
      options.Keys |> Seq.exists (fun k -> String.Equals(k, key, StringComparison.OrdinalIgnoreCase))
    
    if engine1.Config.ContemptEnabled then      
      let ratingDiff = engine1.Config.Rating - engine2.Config.Rating
      if ratingDiff > 0 || engine1.Config.NegativeContemptAllowed then
        let options = engine1.GetDefaultOptions()
        if hasKeyCI options "Contempt" then
          let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
          engine1.AddSetOption engineOption
          printfn "Contempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
        elif hasKeyCI options "DynamicContempt" then
          let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
          engine1.AddSetOption engineOption
          printfn "DynamicContempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
      else 
        printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine1.Name ratingDiff engine2.Name

    if engine2.Config.ContemptEnabled then      
      let ratingDiff = engine2.Config.Rating - engine1.Config.Rating
      if ratingDiff > 0 || engine2.Config.NegativeContemptAllowed then
        let options = engine2.GetDefaultOptions()
        if hasKeyCI options "Contempt" then
          let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
          engine2.AddSetOption engineOption
          printfn "Contempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
        elif hasKeyCI options "DynamicContempt" then
          let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
          engine2.AddSetOption engineOption
          printfn "DynamicContempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
      else 
        printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
  
  let initEngines openingDelayMs (tourny:Tournament) (engine1: ChessEngine) (engine2: ChessEngine) (logger: ILogger) =          
    async {
        try            
            do! Async.Sleep 200
            
            let delayBetweenGamesMs = tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int
            
            //Pass logger to waitForEngineIsReady
            let startEngines = [
                waitForEngineIsReady tourny engine1 logger
                waitForEngineIsReady tourny engine2 logger
            ]
            let pauseUntilTournamentIsReady = [
                createAsyncDelay openingDelayMs
                createAsyncDelay delayBetweenGamesMs
            ]
            
            let! res =
                startEngines @ pauseUntilTournamentIsReady
                |> Async.Parallel
              
            //Only check engine results (first 2)
            let engineResults = res |> Array.take 2
            let failed = engineResults |> Array.exists(fun e -> not e)
              
            if failed then
                let failedEngines = 
                    [| (engine1, engineResults.[0]); (engine2, engineResults.[1]) |]
                    |> Array.filter (fun (_, ok) -> not ok)
                    |> Array.map fst
                  
                let engineNames = String.Join(", ", failedEngines |> Array.map (fun e -> e.Name))
                logger.LogCritical("Failed to start engines: {FailedEngines}", engineNames)
                raise (EngineStartupException($"Failed to start: {engineNames}"))
                
            else              
                checkAndPrepareContempt engine1 engine2
                
          with ex ->
            // Log the error with context
            logger.LogError(ex, "Exception during initEngines for {White} vs {Black}", engine1.Name, engine2.Name)
           
            try                
                if not (engine1.HasExited()) then
                    engine1.StopProcess()
                    
                if not (engine2.HasExited()) then
                    engine2.StopProcess()
            with cleanupEx ->
                logger.LogWarning(cleanupEx, "Error during cleanup in initEngines")
                raise (EngineStartupException cleanupEx.Message)
            raise ex
            
      } |> Async.RunSynchronously
  
module FathomRunner =
    open System.Runtime.InteropServices 
    open System.Text.RegularExpressions
    
    /// Represents the parsed tablebase result.
    type TablebaseResult = {
        Fen: string option
        Wdl: string option
        Dtz: string option
        WinningMoves: string list
        DrawingMoves: string list
        LosingMoves: string list
    }

    // Compiled regex to match lines like: [FieldName "value"]
    let headerRegex = Regex(@"\[(\w+)\s+""([^""]*)""\]", RegexOptions.Compiled)

    /// Splits a comma-separated moves string into a list of trimmed moves.
    let parseMoves (value: string) =
        if String.IsNullOrWhiteSpace(value) then []
        else
            value.Split(',')
            |> Array.map (fun s -> s.Trim())
            |> Array.filter (fun s -> not (String.IsNullOrEmpty s))
            |> Array.toList

    /// Parses the full Fathom tablebase output into a TablebaseResult record.
    let parse (input: string) : TablebaseResult =
        // Define an initial result with empty values.
        let initial = {
            Fen = None
            Wdl = None
            Dtz = None
            WinningMoves = []
            DrawingMoves = []
            LosingMoves = []
        }
        input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.fold (fun acc line ->
            let m = headerRegex.Match(line)
            if m.Success then
                let key = m.Groups.[1].Value
                let value = m.Groups.[2].Value
                match key with
                | "FEN"           -> { acc with Fen = Some value }
                | "WDL"           -> { acc with Wdl = Some value }
                | "DTZ"           -> { acc with Dtz = Some value }
                | "WinningMoves"  -> { acc with WinningMoves = parseMoves value }
                | "DrawingMoves"  -> { acc with DrawingMoves = parseMoves value }
                | "LosingMoves"   -> { acc with LosingMoves = parseMoves value }
                | _               -> acc
            else acc
        ) initial
      /// Ensures that the specified file has executable permissions (Linux/macOS).
      
    let ensureExecutablePermissions (filePath: string) =
          try
              let startInfo = 
                  ProcessStartInfo(
                      FileName = "chmod",
                      Arguments = sprintf "+x \"%s\"" filePath,
                      UseShellExecute = false,
                      CreateNoWindow = true)
              use proc = new Process(StartInfo = startInfo)
              proc.Start() |> ignore
              proc.WaitForExit()
          with ex ->
              Console.Error.WriteLine(sprintf "Failed to set executable permissions: %s" ex.Message)

      /// Determines the correct Fathom executable path based on the current OS.
    let getFathomExecutablePath () =
          let basePath = AppDomain.CurrentDomain.BaseDirectory
          let exePath =
              if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                  Path.Combine(basePath, "Tools", "fathom.exe")
              elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
                  Path.Combine(basePath, "Tools", "fathom.linux")
              elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                  Path.Combine(basePath, "Tools", "fathom.macosx")
              else
                  failwith "Unsupported OS platform."
    
          // For Linux and macOS, ensure the file has execute permissions.
          if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) ||
             RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
              ensureExecutablePermissions exePath

          //check if exePath exists
          if not (File.Exists exePath) then
                failwithf "Fathom executable not found at path: %s" exePath
          exePath

      /// Runs the Fathom executable with the given tablebase path and FEN, returning its output.
    let runFathom (tablebasePath: string) (fen: string) =
          let exePath = getFathomExecutablePath ()
          let arguments = sprintf "--path=\"%s\" \"%s\"" tablebasePath fen

          let startInfo = 
              ProcessStartInfo(
                  FileName = exePath,
                  Arguments = arguments,
                  UseShellExecute = false,
                  RedirectStandardOutput = true,
                  CreateNoWindow = true)
    
          use proc = new Process(StartInfo = startInfo)
          proc.Start() |> ignore
          let output = proc.StandardOutput.ReadToEnd()
          proc.WaitForExit()
          output
    
    let runFathomSafe (tablebasePath: string) (fen: string) (timeoutMs:int) : string option =
        try
          let exePath = getFathomExecutablePath ()
          let startInfo = ProcessStartInfo()
          startInfo.FileName <- exePath
          startInfo.UseShellExecute <- false
          startInfo.CreateNoWindow <- true
          startInfo.RedirectStandardOutput <- true
          startInfo.RedirectStandardError <- false // avoid potential pipe blocking on Linux
          // Build args safely across platforms
          startInfo.ArgumentList.Add($"--path={tablebasePath}")
          startInfo.ArgumentList.Add(fen)

          use proc = new Process(StartInfo = startInfo)
          if not (proc.Start()) then None
          else
            if proc.WaitForExit(timeoutMs) then
              let out = proc.StandardOutput.ReadToEnd()
              Some out
            else
              try proc.Kill(true) with _ -> ()
              None
        with _ -> None


module Replay =
  
  type ReplayData = {Engine:string; Move: string; TimeLeftInMs: int64; Hash: string }

  type ReferenceGameReplay() = 
      inherit Dictionary<uint64, ReplayData>()

      member this.TryGet (hash) = 
          match this.TryGetValue(hash) with
          | true, data -> Some data
          | false, _ -> None

      member this.Seed (initialData: seq<uint64 * ReplayData>) =
          for (key, value) in initialData do
              this.Add(key, value)

      member this.PrettyPrint() =
        this |> Seq.map (fun kvp -> sprintf "Key: %A, Engine %s played Move: %s, TimeLeft: %d ms" kvp.Key kvp.Value.Engine kvp.Value.Move kvp.Value.TimeLeftInMs)
             |> String.concat "\n"

  type GameReplay = 
    { WhitePlayer: string
      BlackPlayer: string
      LongSanMoves: ResizeArray<string>
      PGNMetaData : PGNTypes.GameMetadata
      }
    with 
      static member InitGame = {WhitePlayer = ""; BlackPlayer = ""; LongSanMoves = ResizeArray<string>(); PGNMetaData = PGNTypes.GameMetadata.Empty}
      member this.HasMoves = this.LongSanMoves.Count > 0
      member this.AddPlayers white black = {this with WhitePlayer = white; BlackPlayer = black }
      member this.AddMove (move:string) = this.LongSanMoves.Add move
      member this.copyGameReplay white black = {WhitePlayer = white; BlackPlayer = black; LongSanMoves = ResizeArray<string>(this.LongSanMoves); PGNMetaData = this.PGNMetaData}

  let prepareGameReplay 
    (pairing : Pairing) 
    (replayDicts : Map<string, ReferenceGameReplay>) 
    (replayList: ResizeArray<GameReplay>)
    (referencGamesPlayed: PgnGame array)
    (gamesAlreadyPlayed: PgnGame array)
    (isChess960: bool)
     = 
        let getReplayDictForPlayer (name:string) = replayDicts.[name]
        let nextGame = pairing
        let replayDictWhite = getReplayDictForPlayer pairing.White.Name
        let replayDictBlack = getReplayDictForPlayer pairing.Black.Name
        let lastRelevantLiveGame = 
            replayList 
            |> Seq.tryFind(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))
        
        let latestLiveGames = 
            replayList 
            |> Seq.filter(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))
        
        let allGames = Array.concat [referencGamesPlayed; gamesAlreadyPlayed]
        let refGamesPlayed = allGames |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash)

        match refGamesPlayed |> Seq.tryLast with
        |Some _ ->
            let lastRelevantGame = 
                refGamesPlayed 
                |> Seq.tryFind(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))  
          
            let lastRelevantGames = 
                refGamesPlayed 
                |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))          
          
            let previousGames = 
                refGamesPlayed 
                |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))
                     
            //for g in previousGames do
            //    printfn "Previous game found in PGN: %s, %s for pairing %s, %s" g.GameMetaData.White g.GameMetaData.Black pairing.White.Name pairing.Black.Name
          
            let replayBoard = Board()
            replayBoard.IsFRC <- isChess960
            let tryInitBoard () = 
                if pairing.Opening.Fen <> "" then
                    replayBoard.LoadFen pairing.Opening.Fen
            for game in lastRelevantGames do
                printfn "Relevant saved game found %s, %s for pairing: %s, %s" game.GameMetaData.White game.GameMetaData.Black pairing.White.Name pairing.Black.Name            
                let isWhite = game.GameMetaData.White = pairing.White.Name
                let rematch = game.GameMetaData.White = pairing.White.Name && game.GameMetaData.Black = pairing.Black.Name
                if rematch then
                    printfn "Rematch found for %s, %s - so games should be identical" game.GameMetaData.White game.GameMetaData.Black
                replayBoard.ResetBoardState()
                tryInitBoard()
                let mutable idx = 0
                for m in game.Mainline do
                    let hash = replayBoard.DeviationHash()
                    replayBoard.PlaySimpleShortSan m.San
                    if m.Color = "w" then                        
                        if replayBoard.LongSANMovesPlayed.Count > idx then
                            let lastmove = replayBoard.LongSANMovesPlayed[idx]
                            let data : ReplayData = {Engine=game.GameMetaData.White; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}
                            if isWhite then
                                replayDictWhite[hash] <- data                                
                            idx <- idx + 1
                    elif m.Color = "b" then
                        if replayBoard.LongSANMovesPlayed.Count > idx then
                            let lastmove = replayBoard.LongSANMovesPlayed[idx]
                            let data : ReplayData = {Engine=game.GameMetaData.Black; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}
                            if not isWhite then
                                replayDictBlack[hash] <- data
                            idx <- idx + 1

            let moves = 
                match lastRelevantGame with
                |Some game ->
                    if game.GameMetaData.White = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
                |None -> 0

            match lastRelevantLiveGame with
            |Some game ->  
                let data = game.PGNMetaData
                let sumGames = (latestLiveGames |> Seq.length) + (lastRelevantGames |> Seq.length)              
                let log = sprintf "First Live game: %s vs %s in round %s, number of games (live and saved) are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves              
                printfn "%s" log
            |None -> 
            
            match lastRelevantGame with
            |Some game ->  
                let sumGames = lastRelevantGames |> Seq.length
                let log = sprintf "First saved game (no live game yet): %s vs %s in round %s, number of games are: %d, tot moves: %d" game.GameMetaData.White game.GameMetaData.Black game.GameMetaData.Round sumGames moves              
                printfn "%s" log
            |None -> 
                let whiteMoves = replayDictWhite |> Seq.length
                let blackMoves = replayDictBlack |> Seq.length
                let log = sprintf "No relevant saved game found for player (%s, %s) for game number %d, tot moves: %d" pairing.White.Name pairing.Black.Name nextGame.GameNr moves
                printfn "%s whiteDict: %d, BlackDict: %d" log whiteMoves blackMoves
        |_ ->           
            match lastRelevantLiveGame with
            |Some game ->  
                let moves = if game.WhitePlayer = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
                let data = game.PGNMetaData
                let sumGames = (latestLiveGames |> Seq.length)              
                let log = sprintf "First Live game: %s vs %s in round %s, number of games are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves              
                printfn "%s" log
            |_ -> ()
            //let log = sprintf "No relevant live game found for player (%s,%s)" pairing.White.Name pairing.Black.Name           
  

module Adjudication =

    // Abstracted function to count evaluations based on a condition
    let countEvalsBasedOnCondition (evaluations: EvalType list) conditionFunc =
      evaluations
      |> Seq.takeWhile conditionFunc
      |> Seq.length

    // Function to check for sufficient high evaluations
    let hasSufficientHighEvals (evaluations: EvalType list) minHighEvalSize minScoreThreshold =
      let isHighEval = function
          | CP score -> abs score >= minScoreThreshold
          | Mate _ -> true
          | _ -> false
      countEvalsBasedOnCondition (List.truncate minHighEvalSize evaluations) isHighEval >= minHighEvalSize

      // Function to check for sufficient low evaluations
    let consecutiveNumberOfLowEvalsLeft (evaluations: EvalType list) minLowEvalSize maxDrawScore =
      let isLowEval = function
          | CP score -> abs score <= maxDrawScore
          | Mate _ -> false
          | _ -> false
      let consecutiveLows = countEvalsBasedOnCondition (List.truncate minLowEvalSize evaluations) isLowEval 
      minLowEvalSize - consecutiveLows

    let movesLeftBeforeDrawAdjudication (eval:EvalType) (evals: EvalType list) minMoveNumber drawPlies maxDrawScore =
      match eval with
      |Mate _ -> drawPlies
      |CP ev when abs ev > maxDrawScore -> drawPlies
      |_ when evals.Length >= (minMoveNumber * 2) -> 
          consecutiveNumberOfLowEvalsLeft evals drawPlies maxDrawScore 
      |_ -> drawPlies

    // A function to check if a list of evaluations is too low for a draw
    let isConsecutiveLowEvalSufficient (evals: EvalType list) drawPlies maxDrawScore =
      let res = consecutiveNumberOfLowEvalsLeft evals drawPlies maxDrawScore
      res <= 0

    // A function to check if the tablebase adjudication should be applied
    let shouldAdjudicateTB (evals: EvalType list) (piecesLeft: int) tbMen =
      if piecesLeft <= tbMen && evals.Length > 1 then
          match evals.[0], evals.[1] with
          |fst, snd when fst.WinAdj 5 && snd.WinAdj 5 -> true //draw
          |fst, snd when fst.DrawAdj 1 && snd.DrawAdj 1 -> true //win
          |_ -> false
      else 
        false

    // A function to determine the winner and the result by evaluation agreement
    let adjudicateByEval 
      (logger: ILogger)
      (board:Board)
      (evals: EvalType list) 
      (tourny: Tournament) 
      (player1: string) 
      (player2: string) 
      (playedLastMove: string)
      gametimer
      gameMoveList
      moves =
      let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
      let drawPlyLength = tourny.Adjudication.DrawOption.DrawMoveLength * 2
      let winPlyLength = tourny.Adjudication.WinOption.WinMoveLength * 2
      let tooHighEvals () = hasSufficientHighEvals evals winPlyLength tourny.Adjudication.WinOption.MinWinScore
      let tooLowEvals () = isConsecutiveLowEvalSufficient evals drawPlyLength tourny.Adjudication.DrawOption.MaxDrawScore
      let mutable posToCheck = board.Position
      let piecesLeft = PositionOps.numberOfPieces &posToCheck
      let withTBadjudicationMen = 
        if tourny.Adjudication.TBAdj.UseTBAdjudication then 
          tourny.Adjudication.TBAdj.TBMen
        else 
          2
      let firstTwoEvals () =
        match evals |> List.rev with
        |[] -> []
        |[x] -> [x]
        |x::y::_ -> [x;y]
      if piecesLeft <= withTBadjudicationMen then        
        let firstTwoEvals = firstTwoEvals ()            
        let tryProbe =
          try
            let dir = tourny.Adjudication.TBAdj.TablebaseDirectory
            if String.IsNullOrEmpty(dir) |> not && Directory.Exists dir then
              let fen = board.FEN()
              match FathomRunner.runFathomSafe dir fen 3000 with
              | Some tableRes ->
                  let tb = FathomRunner.parse tableRes
                  match tb.Wdl with
                  | Some "Win" ->
                      let res = if board.Position.STM = 0uy then "1-0" else "0-1"
                      createResultWithEval player1 player2 gameMoveList res Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | Some "Draw" ->
                      createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | Some "Loss" ->
                      let res = if board.Position.STM = 0uy then "0-1" else "1-0"
                      createResultWithEval player1 player2 gameMoveList res Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                  | _ -> None
              | None -> None
            else None
          with ex ->
            logger.LogWarning(ex, "TB adjudication probe failed; continuing without TB")
            None        
        
        if tryProbe.IsSome then
          tryProbe
        
        elif tryProbe.IsNone && shouldAdjudicateTB evals piecesLeft withTBadjudicationMen then
            try
                match evals.[0] with
                | EvalType.CP ev when ev > 5.0 ->
                    createResultWithEval player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.CP ev when ev < -5.0 -> 
                    createResultWithEval player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.CP _ -> 
                    createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.Mate m when m > 0 ->
                    createResultWithEval player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.Mate m when m < 0 -> 
                    createResultWithEval player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.Mate m -> // mate 0 or mate -0
                    if m = -0 then 
                        createResultWithEval player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                    else 
                        createResultWithEval player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur firstTwoEvals |> Some
                | EvalType.NA ->
                    logger.LogCritical("TB adjudication fallback skipped: NA eval")
                    None
            with ex -> 
                logger.LogCritical(ex, "Error during TB adjudication fallback")
                None
        else None
    
      elif moves >= (tourny.Adjudication.WinOption.MinWinMove * 2 + winPlyLength) && tooHighEvals() then        
        let result = 
          match evals.[0] with
          |EvalType.CP ev when ev >= tourny.Adjudication.WinOption.MinWinScore -> "1-0" |> Some
          |EvalType.CP ev when ev <= -tourny.Adjudication.WinOption.MinWinScore -> "0-1" |> Some
          |EvalType.Mate m when m > 0 -> "1-0" |> Some
          |EvalType.Mate m when m < 0 -> "0-1" |> Some
          |EvalType.Mate m -> //mate 0 or mate -0
              if m = -0 then "0-1" |> Some else "1-0" |> Some
          |EvalType.CP cp ->             
            logger.LogCritical("High evals but not reaching MinWinScore: {Eval}", cp)
            None
          |NA -> 
            logger.LogCritical("High evals but NA eval found")
            None
        match result with
        |None -> None
        |Some result ->
            let res = createResultWithEval player1 player2 gameMoveList result Misc.ResultReason.AdjudicatedEvaluation dur (firstTwoEvals())
            Some res     
  
      elif moves >= (tourny.Adjudication.DrawOption.MinDrawMove * 2 + drawPlyLength) && tooLowEvals() then                   
            let res = createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicatedEvaluation dur (firstTwoEvals())
            Some res

      elif board.InsufficentMaterial() then
            let res = createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicateMaterial dur (firstTwoEvals())
            Some res

      elif board.ClaimThreeFoldRep () then
        let res = createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.Repetition dur (firstTwoEvals())
        Some res

      elif board.AnyLegalMove() |> not then
        let mutable mypos = board.Position
        let check = ChessLibrary.MoveGeneration.InCheck &mypos <> 0UL
        if check then            
          if playedLastMove = player1 then 
            let res = createResultWithEval player1 player2 gameMoveList "1-0" Misc.ResultReason.Checkmate dur  (firstTwoEvals())
            Some res
          else 
            let res = createResultWithEval player1 player2 gameMoveList "0-1" Misc.ResultReason.Checkmate dur (firstTwoEvals())         
            Some res
        else 
          let res = createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.Stalemate dur (firstTwoEvals())
          Some res         

      elif board.Position.Count50 >= 100uy then
          let res = createResultWithEval player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.ExcessiveMoves dur (firstTwoEvals())
          Some res
      else
        None

    //let isInMatePlayout fullEvalList =
    //  //if the two last elements in the evalList are mate values, then we are in a mate playout
    //  let lastTwo = fullEvalList |> Seq.truncate 2 |> Seq.toList
    //  match lastTwo with
    //  |[EvalType.Mate _; EvalType.Mate _] -> true
    //  |_ -> false

module TournamentUtils =

  let validateEnginesInTournament (tourny : Tournament)  =
    async {
      Utilities.Validation.validateTournamentInput tourny
      let mutable valid = tourny.EngineSetup.Engines.Length > 1
      for engConfig in tourny.EngineSetup.Engines do
        let engine = EngineHelper.createEngine (engConfig, None)
        if valid then
          valid <- engine.PassedValidation
          engine.PrintNonDefaultValues()
        engine.StopProcess()
        Async.Sleep(1000) |> ignore
      if valid then
        LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green "\nTournament validation was successful"
      else
        LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red "\nTournament validation failed"
      return valid
    } |> Async.StartAsTask

  let estimateGameDuration (white: TimeConfig) (black:TimeConfig) (movesEst : int) = 
    let wFixedTicks = if white.NodeLimit then 0L else white.Fixed.Ticks
    let wIncrTicks = if white.NodeLimit then 0L else white.Increment.Ticks
    let bFixedTicks = if black.NodeLimit then 0L else black.Fixed.Ticks
    let bIncrTicks = if black.NodeLimit then 0L else black.Increment.Ticks
    let fixedTs = TimeSpan.FromTicks (wFixedTicks + bFixedTicks)
    let incrTs = TimeSpan.FromTicks (wIncrTicks + bIncrTicks)
    let fixedTime = fixedTs.TotalSeconds
    let incrTime = incrTs.TotalSeconds
    let seconds = fixedTime + (incrTime * float movesEst)
    seconds
  
  let estimateTournamentAndGameTime (pairs:int) (tourny:Tournament) (pairings: Pairing seq) =
    let movesEst = tourny.Adjudication.DrawOption.MinDrawMove + tourny.Adjudication.DrawOption.DrawMoveLength + 15
    let delay = tourny.DelayBetweenGames.ToTimeSpan().TotalSeconds
    let mutable secs = 0.0
    for p in pairings do
      let whiteTc = tourny.FindTimeControl p.White.TimeControlID
      let blackTc = tourny.FindTimeControl p.Black.TimeControlID
      let avgGameDurationSec = estimateGameDuration whiteTc blackTc movesEst
      secs <- secs + avgGameDurationSec + delay
    let avgGameDurationSec = 
      if secs = 0.0 then
        0.0
      else
        secs / (pairings |> Seq.length |> float)
    TimeSpan.FromSeconds(secs), TimeSpan.FromSeconds(avgGameDurationSec)
  
  let annotation verboseAnnotation (board: Chess.Board) (numberMove : string) (chessMoveInfo : ChessMoveInfo) = 
      if verboseAnnotation then
        if board.PlyCount > 1 then
          sprintf " %s {%s}" numberMove chessMoveInfo.Annotation
        else
          sprintf "%s {%s}" numberMove chessMoveInfo.Annotation
      else
        if board.PlyCount > 1 then
          sprintf " %s {%s}" numberMove chessMoveInfo.MinimalAnnotation
        else
          sprintf "%s {%s}" numberMove chessMoveInfo.MinimalAnnotation
    
  let logEngineInitCommands (logger: ILogger) (player1: ChessEngine) (player2: ChessEngine) =
    logger.LogDebug($"\nInitializing {player1.Name} ....")
    for cmd in player1.GetVerifiedCommands() do
        logger.LogDebug($"{cmd}")
    logger.LogDebug($"\nInitializing {player2.Name} ....")
    for cmd in player2.GetVerifiedCommands() do
        logger.LogDebug($"{cmd}")
  
  let bestQMove (nodes:int) (engine: ChessEngine) fenWithMoves (tboard:Board) = async {
    let qList = ResizeArray<float*string*EvalType>()    
    tboard.ResetBoardState()
    tboard.PlayCommands fenWithMoves
    let legalMoves = tboard.GetAllLegalMoves()    
    for (lSan,_) in legalMoves do
      let cmd = sprintf "%s %s" fenWithMoves lSan
      engine.Position cmd
      engine.GoNodes nodes
      
      let mutable cont = true
      let mutable infoString = ""
      let mutable eval = EvalType.NA
      while cont do
        let! line = engine.ReadLineAsyncWithTimeout CancellationToken.None |> Async.AwaitTask  // Initialization.readLine engine CancellationToken.None        
        if line.StartsWith "bestmove" then
          cont <- false
        elif line.StartsWith "info string node" then
          infoString <- line
        elif line.StartsWith "info depth" then
          eval <- 
            match Regex.evalParser line with
              |NA -> NA
              |CP eval ->
                let eval = if eval = -0.0 then 0.0 else eval
                (if tboard.Position.STM = 0uy then -eval/100.0 else eval / 100.0) |> CP
              |Mate _ as mate -> mate
      
      let res = Regex.floatParser infoString Regex.v
      qList.Add (res, lSan, eval)
    let score, m, ev = qList |> Seq.minBy (fun (s,_,_) -> s)
    return (-score, sprintf "bestmove %s" m, ev) }

module Match =
  open Replay
  open TournamentUtils

  type Update =
    | GameStarted of White:string
    | EndOfGame of Result: Result
    | BestMove of Info:BestMoveInfo * Status: EngineStatus
    | Info of Player:string * Info: string
    | Eval of Player:string * Type: EvalType
    | Status of Engine:EngineStatus
    | PonderStatus of Engine:EnginePonderStatus
    | Time of Player:string * Time: TimeOnly
    | NNSeq of NNSeq: ResizeArray<NNValues>
    | StartOfGame of Game:StartGameInfo
    | EndOfTournament of Info: Tournament
    | StartOfTournament of Info:StartOfTournamentInfo
    | MessagesFromEngine of Player:string * Message:string
    | PairingList of Pairings: ResizeArray<Pairing>
    | TotalNumberOfPairs of PairingsNumber: int
    | RoundNr of Round: string
    | PeriodicResults of results: ResizeArray<Result>
    | CupBracketUpdated
    | SwissStateUpdated
    | GameSummary of summary: string

  type CupBracketMessage =
    | WriteCupBracket of Bracket: TypesDef.Cup.CupBracket * Reply: AsyncReplyChannel<unit>
    | ReadCupBracket of Reply: AsyncReplyChannel<TypesDef.Cup.CupBracket option>
    | DisposeCupBracket

  type SwissStateMessage =
    | WriteSwissState of State: TypesDef.Swiss.SwissState * Reply: AsyncReplyChannel<unit>
    | ReadSwissState of Reply: AsyncReplyChannel<TypesDef.Swiss.SwissState option>
    | DisposeSwissState

  let private normalizeBracketJson (json: string) =
    if String.IsNullOrWhiteSpace json then
      json
    else
      let trimmed = json.TrimStart()
      if trimmed.StartsWith("\"", StringComparison.Ordinal) && json.TrimEnd().EndsWith("}", StringComparison.Ordinal) then
        "{" + json
      else
        json

  let startCupBracketReaderWriter (filePath: string) =
    MailboxProcessor<CupBracketMessage>.Start(fun inbox ->
      async {
        let optionsWrite = JsonSerializerOptions(WriteIndented = true)
        let optionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let mutable inMemory = ""
        while true do
          let! message = inbox.Receive()
          match message with
          | DisposeCupBracket ->
              return ()
          | WriteCupBracket (bracket, reply) ->
              bracket.UpdatedUtc <- DateTime.UtcNow
              let json = JsonSerializer.Serialize(bracket, optionsWrite)
              if String.IsNullOrWhiteSpace filePath then
                inMemory <- json
              else
                use stream = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.None)
                use writer = new StreamWriter(stream, Encoding.UTF8)
                writer.Write(json)
                writer.Flush()
                stream.Flush()
              reply.Reply()
          | ReadCupBracket reply ->
              try
                let json =
                  if String.IsNullOrWhiteSpace filePath then
                    inMemory
                  else
                    if File.Exists filePath then
                      use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
                      use reader = new StreamReader(stream, Encoding.UTF8)
                      reader.ReadToEnd()
                    else
                      ""
                let json = normalizeBracketJson json
                if String.IsNullOrWhiteSpace json then
                  reply.Reply None
                else
                  let loaded = JsonSerializer.Deserialize<TypesDef.Cup.CupBracket>(json, optionsRead)
                  if obj.ReferenceEquals(loaded, null) then reply.Reply None else reply.Reply (Some loaded)
              with
              | _ -> reply.Reply None
      })

  let startSwissStateReaderWriter (filePath: string) =
    MailboxProcessor<SwissStateMessage>.Start(fun inbox ->
      async {
        let optionsWrite = JsonSerializerOptions(WriteIndented = true)
        let optionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let mutable inMemory = ""
        while true do
          let! message = inbox.Receive()
          match message with
          | DisposeSwissState ->
              return ()
          | WriteSwissState (state, reply) ->
              state.UpdatedUtc <- DateTime.UtcNow
              let json = JsonSerializer.Serialize(state, optionsWrite)
              if String.IsNullOrWhiteSpace filePath then
                inMemory <- json
              else
                use stream = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.None)
                use writer = new StreamWriter(stream, Encoding.UTF8)
                writer.Write(json)
                writer.Flush()
                stream.Flush()
              reply.Reply()
          | ReadSwissState reply ->
              try
                let json =
                  if String.IsNullOrWhiteSpace filePath then
                    inMemory
                  else
                    if File.Exists filePath then
                      use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
                      use reader = new StreamReader(stream, Encoding.UTF8)
                      reader.ReadToEnd()
                    else
                      ""
                let json = normalizeBracketJson json
                if String.IsNullOrWhiteSpace json then
                  reply.Reply None
                else
                  let loaded = JsonSerializer.Deserialize<TypesDef.Swiss.SwissState>(json, optionsRead)
                  if obj.ReferenceEquals(loaded, null) then reply.Reply None else reply.Reply (Some loaded)
              with
              | _ -> reply.Reply None
      })

  type UserAdjudication =
    { GameNr: int
      Result: string }

  let private adjudicationReason = ResultReason.AdjudicatedByUser
  

  // cancel-aware read now uses pump (already in TournamentUtils)
  //let readLineCancelAware (eng: ChessEngine) (cts: CancellationTokenSource) = Initialization.readLineCancelAware eng cts

  let lostOnTimeResult (playing: string) (opponent: string) (isWhite: bool) (gameMoveList: ResizeArray<string>) (gametimer: int64) evals : Result =
    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let resStr = if isWhite then "0-1" else "1-0"
    let player1, player2 = if isWhite then playing, opponent else opponent, playing
    createResultWithEval player1 player2 gameMoveList resStr ResultReason.ForfeitLimits dur evals  
  
  //let private logError (logger:ILogger) (failure: EngineFailure) =    
  //  match failure with
  //  | Timeout (timeoutMs, wasThinking) ->
  //      logger.LogCritical("Engine timeout after {TimeoutMs} ms (was thinking: {WasThinking})", timeoutMs, wasThinking)
  //  | Disconnect (exitCode, stderr) ->
  //      let exitCodeStr = exitCode |> Option.map string |> Option.defaultValue "unknown"
  //      let stderrStr = String.concat "; " (stderr |> List.truncate 5)
  //      logger.LogCritical("Engine disconnected (exit code: {ExitCode}). Stderr: {Stderr}", exitCodeStr, stderrStr)
  //  | Hang (silentDurationMs, lastOutput) ->
  //      let lastOutputStr = lastOutput |> Option.defaultValue "none"
  //      logger.LogError("Engine hang detected after {SilentDurationMs} ms of silence. Last output: {LastOutput}", silentDurationMs, lastOutputStr)
  //  | IllegalMove (attemptedMove, positionFen) ->
  //      logger.LogCritical("Illegal move detected: {AttemptedMove} in position {PositionFen}", attemptedMove, positionFen)
  //  | ProcessCrash diagnostics ->
  //      logger.LogCritical("Engine process crashed. Diagnostics: {Diagnostics}", diagnostics)
  //  | Communication message ->
  //      logger.LogError("Engine communication error: {Message}", message)
  //  | Startup message ->
  //      logger.LogCritical("Engine startup error: {Message}", message)
  //  | AppShuttingDown message ->
  //      logger.LogCritical("Engine app shutting down: {Message}", message)
  //  | ExceptionThrown (message, ex) ->
  //      logger.LogError("Engine exception thrown: {Message}", message)
  //      logger.LogError("Engine exception details: {Exception}", ex)
  //  | NoError -> ()

  let private updateClocks
      (duration: TimeSpan)
      (pFixedTime: TimeOnly)        
      (pIncr: TimeOnly)      
      (useNodes: bool) =
          
    // how many ticks remain after the move (considering increment)
    let currentTicksLeft = pFixedTime.Ticks + pIncr.Ticks - duration.Ticks
    // clamp at zero, update clocks and compute "time left" after increment
    let ticks = max currentTicksLeft 0L
    if useNodes then
    // Node-limit mode: keep times unchanged; report current active as-is
        let currentTime = pFixedTime
        // In node-limit mode we also keep timeLeft the same since clocks are irrelevant
        let timeLeft = pFixedTime        
        (currentTime, timeLeft)
    else
        let updatedTimePlaying = TimeOnly ticks
        let timeLeft = TimeOnly (ticks + pIncr.Ticks)
        (updatedTimePlaying, timeLeft)


  // NEW: prefer app shutdown/cancellation over “engine disconnected”
  let private isAppShuttingDown (cts: CancellationTokenSource) =
    cts.IsCancellationRequested
    || Environment.HasShutdownStarted
    || AppDomain.CurrentDomain.IsFinalizingForUnload()
  
    // Centralized logging + crash result builder for unexpected exceptions during a game
  let private handleGameException
    (logger: ILogger)
    (ex: exn)
    (cts: CancellationTokenSource)
    (gametimer: int64)
    (board: Board)
    (engine1: ChessEngine)
    (engine2: ChessEngine)
    (pair: Pairing) : Result =
  
    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
    let white, black = pair.White.Name, pair.Black.Name
    let shutdown = isAppShuttingDown cts
    let moves = board.ShortSANMovesPlayed

    // Helper to create results
    let createCancelResult () = 
        createResult white black moves "1/2-1/2" ResultReason.Cancel dur
    
    let createDisconnectedResult engineName resultStr = 
        createResult white black moves resultStr (ResultReason.Disconnected engineName) dur
    
    // Enhanced diagnostics check
    let checkEngineWithDiagnostics (eng: ChessEngine) =
        try
            let exited = eng.HasExited()
            if exited then
                let diag = eng.GetDiagnostics()
                let stderr = 
                    eng.ErrorOutput 
                    |> Seq.truncate 10 
                    |> String.concat "; "
                logger.LogCritical(
                    "Engine {Engine} crashed. Diagnostics: {Diag} | Stderr: {Stderr}", 
                    eng.Name, diag, stderr)
            exited
        with _ -> true
    
    // Async poll with exponential backoff
    let pollEngineStatusAsync maxAttempts = async {
        let rec poll attempt (delay:int) =
            async {
                if attempt >= maxAttempts then
                    return (checkEngineWithDiagnostics engine1, checkEngineWithDiagnostics engine2)
                else
                    let e1 = checkEngineWithDiagnostics engine1
                    let e2 = checkEngineWithDiagnostics engine2
                    if e1 || e2 then 
                        return (e1, e2)
                    else
                        do! Async.Sleep delay
                        return! poll (attempt + 1) (min (delay * 2) 1000)
            }
        return! poll 0 100
    }
    
    let e1Exited, e2Exited = pollEngineStatusAsync 5 |> Async.RunSynchronously
    
    // Classify exception type
    let isPipeOrIoError =
        match ex with
        | :? System.IO.IOException -> true
        | :? System.ObjectDisposedException -> true
        | :? InvalidOperationException as ioe ->
            let msg = ioe.Message.ToLowerInvariant()
            msg.Contains("standardoutput") || msg.Contains("standardinput")
        | _ ->
            let msg = ex.Message.ToLowerInvariant()
            msg.Contains("pipe") || msg.Contains("broken") || msg.Contains("closed")
    
    // Async cleanup helper
    let forceStopEngineAsync (eng: ChessEngine) = async {
        try
            if not (eng.HasExited()) then
                eng.StopProcess()
                do! Async.Sleep 1000
        with cleanupEx ->
            logger.LogWarning(cleanupEx, "Error stopping {Engine}", eng.Name)
    }
    
    // Decision tree with async cleanup
    let result =
        match shutdown, e1Exited, e2Exited with
        // Both engines crashed
        | _, true, true ->
            logger.LogCritical(ex, "Both engines crashed: {White} vs {Black}", white, black)
            createCancelResult ()
        
        // Application shutdown
        | true, _, _ ->
            logger.LogCritical(ex, "App shutdown during game: {White} vs {Black}", white, black)
            async {
                do! forceStopEngineAsync engine1
                do! forceStopEngineAsync engine2
            } |> Async.RunSynchronously
            createCancelResult ()
        
        // Engine1 crashed
        | false, true, false ->
            logger.LogCritical(ex, "{Engine} crashed: {White} vs {Black}", engine1.Name, white, black)
            forceStopEngineAsync engine2 |> Async.RunSynchronously
            createDisconnectedResult engine1.Name "0-1"
        
        // Engine2 crashed
        | false, false, true ->
            logger.LogCritical(ex, "{Engine} crashed: {White} vs {Black}", engine2.Name, white, black)
            forceStopEngineAsync engine1 |> Async.RunSynchronously
            createDisconnectedResult engine2.Name "1-0"
        
        // Both alive - investigate further
        | false, false, false ->
            if isPipeOrIoError then
                // No stderr clues - poll again with longer timeout
                let e1b, e2b = pollEngineStatusAsync 10 |> Async.RunSynchronously
                match e1b, e2b with
                | true, false ->
                    logger.LogCritical("After polling: {Engine} exited", engine1.Name)
                    forceStopEngineAsync engine2 |> Async.RunSynchronously
                    createDisconnectedResult engine1.Name "0-1"
                    
                | false, true ->
                    logger.LogCritical("After polling: {Engine} exited", engine2.Name)
                    forceStopEngineAsync engine1 |> Async.RunSynchronously
                    createDisconnectedResult engine2.Name "1-0"
                    
                | true, true ->
                    logger.LogCritical("After polling: both engines exited")
                    createCancelResult ()
                    
                | false, false ->
                    logger.LogError(ex, "Unresolved pipe error: {White} vs {Black}", white, black)
                    async {
                        do! forceStopEngineAsync engine1
                        do! forceStopEngineAsync engine2
                    } |> Async.RunSynchronously
                    createCancelResult ()
            else
                // Unexpected exception
                logger.LogCritical(ex, "Unexpected error: {White} vs {Black}", white, black)
                async {
                    do! forceStopEngineAsync engine1
                    do! forceStopEngineAsync engine2
                } |> Async.RunSynchronously
                createCancelResult ()
    
    result

  let firstTwoEvals fullEvalList = 
    match fullEvalList |> List.rev with
    | [] -> []
    | [x] -> [x]
    | x::y::_ -> [x; y]
  
  /// Start a background writer that reads from engine.ReadLineAsync()
  /// and writes lines into the provided channel for the duration of the game.
  let private startEngineChannelWriter
    (engine: ChessEngine)
    (engineChannel: Channel<string>)
    (parentCts: CancellationTokenSource)
    (logger: ILogger) 
    (inPonderMode: unit -> bool) 
    (callback: Update -> unit)
    (isWhite: unit -> bool) =

    let cts = CancellationTokenSource.CreateLinkedTokenSource(parentCts.Token)
    // Poll/throttle settings for ponder updates
    let mutable ponderPollIntervalMs = 1000L
    let sw = Stopwatch.StartNew()
    let rec loop () = async {
      try
        if cts.Token.IsCancellationRequested then
          try engineChannel.Writer.TryComplete() |> ignore with _ -> ()
          logger.LogInformation("Engine channel writer for {Engine} is stopping due to cancellation", engine.Name)
          return ()
        else
          let! line = engine.ReadLineAsync() |> Async.AwaitTask
          let inPonder = inPonderMode()
          let isWhite = isWhite()   
          if not (isNull line) then            
              match line with
               |line when inPonder && sw.ElapsedMilliseconds > ponderPollIntervalMs && line.StartsWith "info depth" ->
                    match Regex.getEssentialData line (not isWhite) with
                    | Some (d, eval, nodes, nps, _pvLine, tbhits, wdl, sd, _mPv) ->
                        let status = {
                            PlayerName = engine.Name; Eval = eval; Depth = d; SD = sd
                            Nodes = nodes; NPS = float nps; TBhits = tbhits
                            WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound }
                        callback (PonderStatus status)
                        sw.Restart()
                    | None -> ()
                                     
               |line when inPonder && line.StartsWith "bestmove" -> 
                    sw.Restart()
               |line when not inPonder ->   
                    engineChannel.Writer.TryWrite(line) |> ignore                    
               | _ -> ()
              
          return! loop()
      with
      | :? OperationCanceledException
      | :? ObjectDisposedException ->
          try engineChannel.Writer.TryComplete() |> ignore with _ -> ()
      | ex ->
          logger.LogWarning(ex, "Engine channel writer error for {Engine}", engine.Name)
          try engineChannel.Writer.TryComplete(ex) |> ignore with _ -> ()
    }

    Async.Start(loop(), cts.Token)
    cts

  //start a game with pondering enabled
  let playWithPondering
    (sb : StringBuilder)
    (cts : CancellationTokenSource)
    (logger : ILogger)
    (tourny : Tournament) 
    (board : Board)
    (player1 : ChessEngine) 
    (player2 : ChessEngine)
    (pairing: Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    callback  = 

    let player1Channel = Channel.CreateBounded<string>(BoundedChannelOptions(capacity = 4096, SingleWriter = true, SingleReader = true, FullMode = BoundedChannelFullMode.Wait))    
    let player2Channel = Channel.CreateBounded<string>(BoundedChannelOptions(capacity = 4096, SingleWriter = true, SingleReader = true, FullMode = BoundedChannelFullMode.Wait))
    
    let mutable player1WriterCts : CancellationTokenSource option = None
    let mutable player2WriterCts : CancellationTokenSource option = None
  
    // State    
    let mutable ponderer : (ChessEngine * string) option = None
    let mutable lastMovePlayed = ""
    let mutable continueGame = true
    let mutable result = Result.Empty
    let mutable player1inPonderMode = false
    let mutable player2inPonderMode = false
    let mutable ponderHit = false
    let mutable currentPos = Position.Default

    // Helper functions to always derive current player from board state
    let whoseTurn stm = if stm = 0uy then player1 else player2
    let opponent stm = if stm = 0uy then player2 else player1
    let isWhiteTurn stm = stm = 0uy

    // ------- helpers & local state (unchanged setup) -------
    let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
    sb.Clear() |> ignore
    let append (txt:string) = sb.Append txt |> ignore

    if tourny.TestOptions.WriteToConsole then
      player1.ShowCommands()
      player2.ShowCommands()

    let mutable lastEngineStatus = EngineStatus.Empty
    let mutable moves = 0
    let mutable numberOfNodes = 0L
    let mutable evalList : EvalType list = []
    let mutable fullEvalList :EvalType list = []

    // Track eval per engine and per move to avoid accidentally reusing the opponent's last ply eval.
    let lastEvalByEngine = System.Collections.Generic.Dictionary<string, EvalType>()
    let lastEvalThisMove = System.Collections.Generic.Dictionary<string, EvalType>()
    do
      lastEvalByEngine[player1.Name] <- EvalType.NA
      lastEvalByEngine[player2.Name] <- EvalType.NA
      lastEvalThisMove[player1.Name] <- EvalType.NA
      lastEvalThisMove[player2.Name] <- EvalType.NA

    let resetThisMoveEval (engineName: string) =
      lastEvalThisMove[engineName] <- EvalType.NA

    let setEval (engineName: string) (eval: EvalType) =
      lastEvalByEngine[engineName] <- eval
      lastEvalThisMove[engineName] <- eval

    let npsList = ResizeArray<float>()
    let gameMoveList = board.ShortSANMovesPlayed

    let findTimeSetting (player : ChessEngine) =
      tourny.FindTimeControl (player.Config.TimeControlID)
    let isNodeLimit player = (findTimeSetting player).NodeLimit
    let wPlayer = (findTimeSetting player1)
    let bPlayer = (findTimeSetting player2)    
    let mutable wTime = wPlayer.Fixed
    let mutable bTime = bPlayer.Fixed
    let moveOverheadInTicks = tourny.MoveOverhead.Ticks

    let msg = $"Initializing players: {player1.Name} vs {player2.Name} with pondering enabled" 
    logger.LogInformation msg
    tourny.CurrentGameNr <- tourny.CurrentGameNr + 1
    
    let gameStartInfo : StartGameInfo = 
      {
        WhitePlayer = player1.Config
        BlackPlayer = player2.Config
        StartPos = board.FEN()
        OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
        WhiteTime = wTime
        BlackTime = bTime
        WhiteToMove = board.Position.STM = 0uy
        OpeningName = tourny.OpeningName
        CurrentGameNr = pairing.GameNr
        OpeningHash = pairing.OpeningHash
      }
    board.MovesAndFenPlayed.Clear()
    let mutable moveInfoData = ChessMoveInfo.Empty

    callback(StartOfGame gameStartInfo)
    let engineOption : EngineOption = {Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
    player1.AddSetOption engineOption
    player2.AddSetOption engineOption                
    if tourny.MoveOverhead.Ticks > 0 then
        let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
        player1.SetMoveOverhead("overhead", ms)
        player2.SetMoveOverhead("overhead", ms)
    if not tourny.ConsoleOnly then     
      let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
      let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
      let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
      Initialization.initEngines openingDelayMs tourny player1 player2 logger
    else
      Initialization.initEngines 0 tourny player1 player2 logger

    try
      player1WriterCts <- Some 
        (startEngineChannelWriter player1 player1Channel cts logger
        (fun () -> player1inPonderMode) 
        callback
        (fun () -> board.Position.STM = 0uy))
      player2WriterCts <- Some 
        (startEngineChannelWriter player2 player2Channel cts logger 
        (fun () -> player2inPonderMode) 
        callback
        (fun () -> board.Position.STM = 0uy))
    with ex ->
      logger.LogWarning(ex, "Failed to start per-game engine channel writers")

    // Helper to get the channel reader for the current playing engine
    let getEngineReaderFor (eng: ChessEngine) =
      if eng.Name = player1.Name then player1Channel.Reader else player2Channel.Reader
    
    logEngineInitCommands logger player1 player2
    Initialization.appendGameDescription sb tourny player1 player2 (board.OpeningMovesPlayed) (board.FEN())
    callback (GameStarted player1.Name)

    let mutable lastCheck = 0L
    let gametimer = Stopwatch.GetTimestamp()
    let mutable moveTimer = Stopwatch.StartNew()
    let mutable depth = 0
    let mutable selfdepth = 0
    let mutable Player1PV = String.Empty
    let mutable Player2PV = String.Empty
    let mutable PVLine1 = String.Empty
    let mutable PVLine2 = String.Empty
    let fen0 = board.FEN()
    logger.LogDebug $"After opening moves, FEN={fen0}"

    // extracted helper to encapsulate all "PROCESS BESTMOVE" logic
    let processBestMove (currentPlaying: ChessEngine) (currentOpponent: ChessEngine) isWhite (line:string)  = async {
      let duration = moveTimer.Elapsed
      let incr = tourny.TimeControl.GetIncrementTime(currentPlaying.Config.TimeControlID)
      let useNodes = isNodeLimit currentPlaying
      let playerTime = if isWhite then wTime.Ticks + incr.Ticks else bTime.Ticks + incr.Ticks
      let remainingTicks = playerTime - duration.Ticks
      let lostOnTime = (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L)
      let mutable posToCheck = board.Position
      let piecesLeft = PositionOps.numberOfPieces &posToCheck
      
      if lostOnTime then
        let firstTwo = firstTwoEvals fullEvalList
        if currentPlaying.HasExited() then
           logger.LogCritical($"Engine {currentPlaying.Name} has exited while lost on time")        
        logger.LogCritical("Engine {Engine} lost on time. Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}", currentPlaying.Name, (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds), duration.TotalMilliseconds)
        let res = lostOnTimeResult currentPlaying.Name currentOpponent.Name isWhite gameMoveList gametimer firstTwo
        result <- res
        continueGame <- false      
      else
        if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
          let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
          do! Async.Sleep delay

        // Update clocks
        let (currentTime, timeLeft) =
          if isWhite then updateClocks duration wTime incr useNodes
          else updateClocks duration bTime incr useNodes
        if isWhite then wTime <- currentTime else bTime <- currentTime
        moveInfoData.tl <- int64 (currentTime.ToTimeSpan().TotalMilliseconds)
        moveInfoData.mt <- int64 duration.TotalMilliseconds
        moveInfoData.pcs <- byte piecesLeft

        let parts = line.Split()
        let move = parts.[1]
        let ponderMove = if line.Contains "ponder" then parts.[3] else ""
        lastMovePlayed <- move
        let mutable shortSan = String.Empty

        match tryGetTMoveFromCoordinateNotation &board move with
        | Some tmove ->
            let mutable moveAdj = tmove
            shortSan <- getSanNotationFromTMove &board tmove
            board.LongSANMovesPlayed.Add(move)
            gameMoveList.Add(shortSan)
            board.MakeMove(&moveAdj)
            let ponderSan = getShortSanFromLongSan &board ponderMove
            moveInfoData.pd <- ponderSan
            moves <- moves + 1

            let eval =
              if evalList.Length > 0 then
                evalList.[0]
              else
                let thisMove = lastEvalThisMove[currentPlaying.Name]
                if thisMove <> EvalType.NA then
                  thisMove
                else
                  match lastEvalByEngine.TryGetValue currentPlaying.Name with
                  | true, e when e <> EvalType.NA -> e
                  | _ -> EvalType.CP 0.0

            let pv, pvLong = if currentPlaying.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
            let nps =
              let last = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
              if last <> 0.0 then last
              else float numberOfNodes / duration.TotalSeconds

            let posNow = board.Position
            let fenNow = BoardHelper.posToFen posNow
            let moveDetail = {
              LongSan = move
              FromSq = move[0..1]
              ToSq = move[2..3]
              Color = if board.Position.STM = 8uy then "w" else "b"
              IsCastling = TMoveOps.isCastlingMove tmove
              Comments = String.Empty
            }
            let moveAndFen = { Move = moveDetail; ShortSan = shortSan; FenAfterMove = fenNow }            
            fullEvalList <- eval :: fullEvalList

            let movesLeft =
              Adjudication.movesLeftBeforeDrawAdjudication
                eval fullEvalList
                tourny.Adjudication.DrawOption.MinDrawMove
                (tourny.Adjudication.DrawOption.DrawMoveLength * 2)
                tourny.Adjudication.DrawOption.MaxDrawScore

            let bestMove : BestMoveInfo = {
              Player = currentPlaying.Name
              Move = move
              Ponder = ponderSan
              Eval = eval
              TimeLeft = timeLeft
              MoveTime = TimeOnly(duration.Ticks)
              NPS = nps
              Nodes = numberOfNodes
              FEN = fenNow
              PV = pv
              LongPV = pvLong
              MoveAndFen = moveAndFen
              MoveHistory = board.GetShortSanMoveHistory()
              Move50 = board.Position.Count50 |> int
              R3 = board.RepetitionNr()
              PiecesLeft = piecesLeft
              AdjDrawML = movesLeft
            }

            evalList <- []
            depth <- 0
            selfdepth <- 0
            npsList.Clear()
            resetThisMoveEval currentPlaying.Name

            match Adjudication.adjudicateByEval logger board fullEvalList tourny player1.Name player2.Name currentPlaying.Name gametimer gameMoveList moves with
            | Some res ->
                if res.Reason = ResultReason.Checkmate then
                  let bm = { bestMove with MoveHistory = bestMove.MoveHistory + "#" }
                  let status = { lastEngineStatus with Eval = EvalType.Mate 0 }
                  let numberAndMove = (board.SanMoveNumberString shortSan) + "#"
                  annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                  callback(BestMove (bm, status))
                else
                  let numberAndMove = board.SanMoveNumberString shortSan
                  annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                  callback(BestMove (bestMove, lastEngineStatus))
                  moveInfoData <- ChessMoveInfo.Empty                  
                result <- res
                continueGame <- false
            | None ->
                let numberAndMove = board.SanMoveNumberString shortSan
                annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                moveInfoData <- ChessMoveInfo.Empty
                callback(BestMove (bestMove, lastEngineStatus))
                //check ponder move status
                match ponderer with
                | Some (pEngine, pMove) when pEngine.Name = currentOpponent.Name && lastMovePlayed = pMove ->                    
                    ponderHit <- true
                    //logger.LogInformation($"PONDER HIT by {currentOpponent.Name} on move {pMove}")
                | Some (pEngine, pMove) when pEngine.Name = currentOpponent.Name && lastMovePlayed <> pMove ->
                    //we pondered, but our opponent played a different move
                    ponderHit <- false
                    //logger.LogInformation($"PONDER MISS by {currentOpponent.Name}, expected {pMove}, got {lastMovePlayed}")
                    pEngine.Stop() //stop pondering
                    do! Async.Sleep 200                    
                    
                | _ -> ponderHit <- false
                
                if not (String.IsNullOrEmpty ponderSan) then
                  if currentPlaying.Name = player1.Name then
                    player1inPonderMode <- true
                    player2inPonderMode <- false
                  else
                    player1inPonderMode <- false
                    player2inPonderMode <- true
                  ponderer <- Some (currentPlaying, ponderMove)
                 
                  let timeConfig = findTimeSetting currentPlaying
                  let timeType = tourny.TimeControl.GetTime(timeConfig)
                  let timeCommand = (TypesDef.TimeControl.TimeControlCommands.uciTimePart timeType wTime bTime)
                  let ponderCommand = sprintf "%s ponder" timeCommand
                  //logger.LogDebug($"PONDER COMMAND: Sending ponder to {currentPlaying.Name}: {ponderCommand}")
                  let ponderPosition = board.PositionWithMoves() + " " + ponderMove
                  currentPlaying.Position ponderPosition
                  currentPlaying.GoPonder ponderCommand

                else
                  ponderer <- None
                  player1inPonderMode <- false
                  player2inPonderMode <- false
        | _ ->
            // Illegal move
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let firstTwo = firstTwoEvals fullEvalList
            let res = 
                if currentPlaying.Name = player1.Name then
                    createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur firstTwo
                else
                    createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur firstTwo
            //log error, send end of game
            logger.LogCritical($"Illegal move from {currentPlaying.Name}: {move} FEN={board.FEN()}")            
            result <- res
            continueGame <- false
    }
    
    // Info line processor
    let processInfoLine (channel:ChannelReader<string>) (engine: ChessEngine) (line: string) = async {
      let elapsed = int64 moveTimer.Elapsed.TotalMilliseconds
      let diff = elapsed - lastCheck
      let interval = 500
      let isWhite = engine.Name = player1.Name

      let handleInfoLine (infoLine: string) =
        match Regex.getEssentialDataWithEPS infoLine isWhite with
        | Some (d, eval, nodes, nps, eps, pvLine, tbhits, wdl, sd, mPv) ->
            numberOfNodes <- nodes
            if d > depth then depth <- d
            if sd > selfdepth then selfdepth <- sd
            npsList.Add(float nps)
            evalList <- eval :: evalList
            setEval engine.Name eval
            moveInfoData.d <- depth
            moveInfoData.sd <- selfdepth
            moveInfoData.wv <- eval
            moveInfoData.n <- nodes
            moveInfoData.s <- nps
            moveInfoData.tb <- tbhits
            moveInfoData.eps <- eps

            if not (String.IsNullOrEmpty pvLine) then
              if player1.Name = engine.Name then
                Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                PVLine1 <- pvLine
                moveInfoData.pv <- Player1PV
              else
                Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                PVLine2 <- pvLine
                moveInfoData.pv <- Player2PV

            let npsLast = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
            let pv, pvLong = if engine.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2

            let status = {
              PlayerName = engine.Name
              Eval = eval; Depth = d; SD = sd; Nodes = nodes; NPS = npsLast; EPS = 0.0
              TBhits = tbhits
              WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
              PV = pv; PVLongSAN = pvLong; MultiPV = mPv
            }
            lastEngineStatus <- status

            if diff > interval then
              lastCheck <- elapsed
              callback (Status status)
        | None -> ()

      if not tourny.TestOptions.PolicyTest && line.StartsWith "info string" && line.Contains "N:" then
        let nnMsg = Regex.getInfoStringData engine.Name line 
        let list = ResizeArray<NNValues>()
        list.Add nnMsg
        let mutable cont = not (line.StartsWith "info string node")
        if not cont && tourny.VerboseLogging then
          logger.LogDebug "Only one move in log live stats"

        while cont do
          let! newline = channel.ReadAsync(cts.Token).AsTask() |> Async.AwaitTask
          if tourny.VerboseLogging then
            logger.LogDebug($"In info string loop: {engine.Name} {newline}")
          if String.IsNullOrEmpty newline then
            cont <- false
          elif newline.StartsWith "bestmove" then
            //if tourny.VerboseLogging then logger.LogInformation(board.FEN() + ": new bestmove: " + newline)
            cont <- false
          elif newline.StartsWith "info string node" then
            cont <- false
          else
            let msg = Utilities.Regex.getInfoStringData engine.Name newline
            list.Add msg

        makeShortSan list &board
        match Utilities.Engine.calcTopNn list with
        | Some (n1,n2,q1,q2,p1,pt) ->
            moveInfoData.n1 <- n1; moveInfoData.n2 <- n2
            moveInfoData.q1 <- q1; moveInfoData.q2 <- q2
            moveInfoData.p1 <- p1; moveInfoData.pt <- pt
        | None -> logger.LogDebug "No move found in log live stats"
        if list.Count > 0 then callback (NNSeq list)

      elif line.StartsWith "info" then
        handleInfoLine line
    }
    
    async {
        let cancel() =            
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
            logger.LogInformation($"Cancel requested")
            result <- res

        let adjudicate (adj: UserAdjudication) =
            try player1.Stop() with _ -> ()
            try player2.Stop() with _ -> ()
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let firstTwo = firstTwoEvals fullEvalList
            let res = createResultWithEval player1.Name player2.Name gameMoveList adj.Result adjudicationReason dur firstTwo
            logger.LogInformation($"Game adjudicated by user: {adj.Result}")
            result <- res
            continueGame <- false
         
        try
            while continueGame && not cts.IsCancellationRequested do
                match tryGetUserAdjudication() with
                | Some adj when adj.GameNr = pairing.GameNr -> adjudicate adj
                | _ -> ()

                let sideToMove = board.Position.STM
                let currentPlaying = whoseTurn sideToMove
                let currentOpponent = opponent sideToMove
                let isWhite = isWhiteTurn sideToMove
                
                if currentPlaying.HasExited() then
                    do! Async.Sleep 1000
                    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                    let firstTwoEvals = firstTwoEvals fullEvalList
                    let res, msg =
                        if currentOpponent.HasExited() then
                            let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                            res, $"Shutdown detected after {currentPlaying.Name} exited; returning Cancel"
                        else
                            let resValue = if currentPlaying.Name = player1.Name then "0-1" else "1-0"                    
                            createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected currentPlaying.Name) dur firstTwoEvals,
                            $"Player has exited/crashed {currentPlaying.Name}"                    
                    logger.LogInformation msg
                    let diagnosis = currentPlaying.GetDiagnostics()            
                    logger.LogCritical diagnosis
                    result <- res
                    continueGame <- false
                
                if currentPos <> board.Position then
                    currentPos <- board.Position
                    let timeConfig = findTimeSetting currentPlaying
                    evalList <- []
                    npsList.Clear()
                    depth <- 0
                    selfdepth <- 0
                    resetThisMoveEval currentPlaying.Name
                    if ponderHit then
                        try currentPlaying.PonderHit() with ex -> logger.LogWarning(ex, "Failed to send ponderhit to {Engine}", currentPlaying.Name)                    
                        ponderHit <- false
                        do! Async.Sleep 200
                    else
                        let fenAndMoves = board.PositionWithMoves()
                        currentPlaying.Position fenAndMoves
                        if timeConfig.NodeLimit then
                            currentPlaying.GoNodes timeConfig.Nodes
                        else                        
                            currentPlaying.Go(tourny.TimeControl.GetTime(timeConfig), wTime, bTime)
                    moveTimer.Restart()
                    lastCheck <- 250L

                //reading from the per-game channel for the engine
                let engineReader = getEngineReaderFor currentPlaying
                try
                    use pollCts = new CancellationTokenSource(250)
                    use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(cts.Token, pollCts.Token)
                    let! line = engineReader.ReadAsync(linkedCts.Token).AsTask() |> Async.AwaitTask
                    if line.StartsWith "bestmove" then
                        do! processBestMove currentPlaying currentOpponent isWhite line                
                    else                
                        do! processInfoLine engineReader currentPlaying line        
                with
                | :? OperationCanceledException ->                    
                    if cts.IsCancellationRequested then
                        logger.LogInformation("playWithPondering: ReadAsync cancelled, cancelling game loop")
                        cancel()
                        continueGame <- false
                | :? System.Threading.Channels.ChannelClosedException ->
                    // channel closed (writer finished) -> treat as cancellation/stop
                    logger.LogInformation("playWithPondering: engine channel closed")
                    cancel()
                    continueGame <- false
                | ex ->
                    logger.LogWarning(ex, "Error reading engine channel in playWithPondering")
                    // treat as cancellation to ensure we return a result
                    cancel()
                    continueGame <- false
           
        finally
            // Cleanup on exit: cancel and dispose per-game writer CTSs and complete channels
            match player1WriterCts with
            | Some pcts -> try pcts.Cancel(); pcts.Dispose() with _ -> ()
            | None -> ()
            match player2WriterCts with
            | Some pcts -> try pcts.Cancel(); pcts.Dispose() with _ -> ()
            | None -> ()
            callback (EndOfGame result)
            try player1Channel.Writer.TryComplete() |> ignore with _ -> ()
            try player2Channel.Writer.TryComplete() |> ignore with _ -> ()
            
        return result
    }   


  // Unified play implementation: use optional replay dicts to enable "do not deviate" mode.
  let playGeneric
    (replayOptWhite: ReferenceGameReplay option)
    (replayOptBlack: ReferenceGameReplay option)
    (sb : StringBuilder)
    (cts : CancellationTokenSource)
    (logger : ILogger)
    (tourny : Tournament) 
    (board : Board)
    (player1 : ChessEngine) 
    (player2 : ChessEngine)
    (pairing: Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    callback  =    
    
    let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
    sb.Clear() |> ignore
    let append (txt:string) = sb.Append txt |> ignore    

    if tourny.TestOptions.WriteToConsole then
      player1.ShowCommands()
      player2.ShowCommands()
    let stm = board.Position.STM
    let mutable lastEngineStatus = EngineStatus.Empty
    let mutable pos = 0UL
    let mutable moves = 0
    let mutable numberOfNodes = 0L
    let mutable evalList : EvalType list = []
    let mutable fullEvalList :EvalType list = []

    // Track eval per engine and per move to avoid accidentally reusing the opponent's last ply eval.
    let lastEvalByEngine = System.Collections.Generic.Dictionary<string, EvalType>()
    let lastEvalThisMove = System.Collections.Generic.Dictionary<string, EvalType>()
    do
      lastEvalByEngine[player1.Name] <- EvalType.NA
      lastEvalByEngine[player2.Name] <- EvalType.NA
      lastEvalThisMove[player1.Name] <- EvalType.NA
      lastEvalThisMove[player2.Name] <- EvalType.NA

    let resetThisMoveEval (engineName: string) =
      lastEvalThisMove[engineName] <- EvalType.NA

    let setEval (engineName: string) (eval: EvalType) =
      lastEvalByEngine[engineName] <- eval
      lastEvalThisMove[engineName] <- eval
    let npsList = ResizeArray<float>()
    let gameMoveList = board.ShortSANMovesPlayed
    let findTimeSetting (player : ChessEngine) =
      tourny.FindTimeControl (player.Config.TimeControlID)
    let isNodeLimit player = (findTimeSetting player).NodeLimit
    let wPlayer = (findTimeSetting player1)
    let bPlayer = (findTimeSetting player2)    
    let mutable wTime = wPlayer.Fixed
    let mutable bTime = bPlayer.Fixed
    let wIncr = tourny.TimeControl.GetIncrementTime(player1.Config.TimeControlID)
    let bIncr = tourny.TimeControl.GetIncrementTime(player2.Config.TimeControlID)
    let moveOverheadInTicks = tourny.MoveOverhead.Ticks
    let delaySeconds = tourny.DelayBetweenGames.ToTimeSpan().TotalSeconds
    let delayMilliseconds = tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds
    let msg = sprintf "Initializing players: %s vs %s with delay: %.2f seconds (%.0f ms)" player1.Name player2.Name delaySeconds delayMilliseconds
    //let mutable enginesInitialized = false
    logger.LogInformation msg
    tourny.CurrentGameNr <- tourny.CurrentGameNr + 1
    
    let gameStartInfo : StartGameInfo = 
      {
        WhitePlayer = player1.Config
        BlackPlayer = player2.Config
        StartPos = board.FEN()
        OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
        WhiteTime = wTime
        BlackTime = bTime
        WhiteToMove = stm = 0uy
        OpeningName = tourny.OpeningName
        CurrentGameNr = pairing.GameNr //tourny.CurrentGameNr
        OpeningHash = pairing.OpeningHash
      }
    board.MovesAndFenPlayed.Clear()
    let mutable moveInfoData = ChessMoveInfo.Empty
    
    callback(StartOfGame gameStartInfo)
    try
        let engineOption : EngineOption = {Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
        player1.AddSetOption engineOption
        player2.AddSetOption engineOption                
        if tourny.MoveOverhead.Ticks > 0 then
            let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
            player1.SetMoveOverhead("overhead", ms)
            player2.SetMoveOverhead("overhead", ms)
        
        if not tourny.ConsoleOnly then     
          let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
          let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
          let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
          Initialization.initEngines openingDelayMs tourny player1 player2 logger
        else
          if player1.HasExited() || player2.HasExited() then            
            Initialization.initEngines 0 tourny player1 player2 logger 
    with ex -> raise (CustomException.EngineStartupException (ex.Message))
    
    // preserve existing logging/description behavior
    logEngineInitCommands logger player1 player2
    Initialization.appendGameDescription sb tourny player1 player2 (board.OpeningMovesPlayed) (board.FEN())
    callback (GameStarted player1.Name)
    let mutable lastCheck = 0L
    let gametimer = Stopwatch.GetTimestamp()
    let mutable moveTimer = Stopwatch.GetTimestamp()
    let mutable depth = 0
    let mutable selfdepth = 0
    let mutable Player1PV = String.Empty
    let mutable Player2PV = String.Empty
    let mutable PVLine1 = String.Empty
    let mutable PVLine2 = String.Empty
    let mutable Q1DifferentFromN1 = 0
    let mutable ct : CancellationToken = CancellationToken.None
    let fen = board.FEN()
    logger.LogDebug $"After opening moves, FEN={fen}"

    let adjudicatedResult (adj: UserAdjudication) =
      let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
      let firstTwo = firstTwoEvals fullEvalList
      createResultWithEval player1.Name player2.Name gameMoveList adj.Result adjudicationReason dur firstTwo

    let mutable userAdjudicationResult : Result option = None

    let applyUserAdjudication (adj: UserAdjudication) =
      match userAdjudicationResult with
      | Some res -> res
      | None ->
          try player1.Stop() with _ -> ()
          try player2.Stop() with _ -> ()
          let res = adjudicatedResult adj
          userAdjudicationResult <- Some res
          callback(EndOfGame res)
          logger.LogInformation($"Game adjudicated by user: {adj.Result}")
          res

    let tryConsumeUserAdjudication () =
      match userAdjudicationResult with
      | Some res -> Some res
      | None ->
          match tryGetUserAdjudication() with
          | Some adj when adj.GameNr = pairing.GameNr -> Some (applyUserAdjudication adj)
          | _ -> None

    let rec readLineOrAdjudication (playing: ChessEngine) (ct: CancellationToken) = async {
      match tryConsumeUserAdjudication() with
      | Some res -> return Choice2Of2 res
      | None ->
          use pollCts = new CancellationTokenSource(250)
          use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(ct, pollCts.Token)
          try
            let! line = playing.ReadLineAsyncWithTimeout(linkedCts.Token) |> Async.AwaitTask
            return Choice1Of2 line
          with
          | :? OperationCanceledException as oce ->
              if ct.IsCancellationRequested then
                return raise oce
              else
                return! readLineOrAdjudication playing ct
      }

    let rec playEngine (playing: ChessEngine) (opponent: ChessEngine) (position:uint64) = async {
      
      match tryConsumeUserAdjudication() with
      | Some res -> return res
      | None ->
       
      if cts.IsCancellationRequested then
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
        logger.LogCritical($"Cancel requested when engine ready to play: {playing.Name}")
        callback(EndOfGame res)
        return res
      else
        if position <> pos then
          let isWhite = playing.Name = player1.Name
          let timeLeftTicks = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
          let timeOutInMs = (TimeSpan(timeLeftTicks).TotalMilliseconds |> int32) + 2000
          ct <- (new CancellationTokenSource(timeOutInMs)).Token

          moveTimer <- Stopwatch.GetTimestamp()
          pos <- position
          evalList <- []
          npsList.Clear()
          depth <- 0
          selfdepth <- 0
          resetThisMoveEval playing.Name

          let fenAndMoves = board.PositionWithMoves()
          if tourny.VerboseLogging then
            logger.LogDebug $"Current position: {fenAndMoves}"
          playing.Position fenAndMoves
          lastCheck <- int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)

          let timeConfig = findTimeSetting playing
          if tourny.TestOptions.ValueTest then
            if playing.IsLc0 then
              playing.GoNodes 2
            else
              playing.GoValue()
          elif tourny.TestOptions.PolicyTest then
            playing.GoNodes 1
          elif timeConfig.NodeLimit then
            playing.GoNodes timeConfig.Nodes
          else
            playing.Go(tourny.TimeControl.GetTime(timeConfig), wTime, bTime)

        let! lineOrAdj = readLineOrAdjudication playing ct
        match lineOrAdj with
        | Choice2Of2 res -> return res
        | Choice1Of2 line ->
         
        if String.IsNullOrWhiteSpace line then
          logger.LogDebug $"Empty line or null from {playing.Name}"
          if playing.HasExited() then
            do! Async.Sleep 1000
            match playing.GetExitCode() with            
            | Some code -> logger.LogCritical $"Engine {playing.Name} has exited with exitcode {code}"
            | None -> logger.LogCritical $"Engine {playing.Name} has exited unexpectedly"
            // create result and return
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let firstTwoEvals = firstTwoEvals fullEvalList
            let res, msg =
                if opponent.HasExited() then
                    let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                    res, $"Shutdown detected after {playing.Name} exited; returning Cancel"
                else
                    let resValue = if playing.Name = player1.Name then "0-1" else "1-0"                    
                    createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected playing.Name) dur firstTwoEvals,
                    $"Player has exited/crashed {playing.Name}"
            logger.LogInformation msg
            callback(EndOfGame res)
            return res         
            
          else
            return! playEngine playing opponent position

        elif line.StartsWith "info engine" then
          MessagesFromEngine ("Ceres", line) |> callback
          logger.LogInformation line
          return! playEngine playing opponent position        
            
        elif isAppShuttingDown cts then
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let firstTwoEvals = firstTwoEvals fullEvalList
            let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals            
            callback(EndOfGame res)
            logger.LogInformation($"Shutdown/cancel detected while {playing.Name} thinking")
            let diag1 = playing.GetDiagnostics()            
            logger.LogCritical diag1
            let diag2 = opponent.GetDiagnostics()
            logger.LogCritical diag2
            return res
        elif playing.HasExited() then
            do! Async.Sleep 1000
            match playing.GetExitCode() with            
            | Some code -> logger.LogCritical $"Engine {playing.Name} has exited with exitcode {code}"
            | None -> logger.LogCritical $"Engine {playing.Name} has exited unexpectedly"
            
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let firstTwoEvals = firstTwoEvals fullEvalList
            let res, msg =
                if opponent.HasExited() then
                    let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur firstTwoEvals
                    res, $"Shutdown detected after {playing.Name} exited; returning Cancel"
                else
                    let resValue = if playing.Name = player1.Name then "0-1" else "1-0"                    
                    createResultWithEval player1.Name player2.Name gameMoveList resValue (ResultReason.Disconnected playing.Name) dur firstTwoEvals,
                    $"Player has exited/crashed {playing.Name}"
            callback(EndOfGame res)
            logger.LogInformation msg
            let diagnosis = playing.GetDiagnostics()            
            logger.LogCritical diagnosis
            return res
        
        else          
          let duration = Stopwatch.GetElapsedTime(moveTimer)          
          let useNodes = isNodeLimit playing
          let isWhite = playing.Name = player1.Name
          let playerTime = if isWhite then wTime.Ticks + wIncr.Ticks else bTime.Ticks + bIncr.Ticks
          let remainingTicks = playerTime - duration.Ticks
          let lostOnTime = (not useNodes) && (remainingTicks + moveOverheadInTicks < 0L)
          if lostOnTime then
            let firstTwoEvals = firstTwoEvals fullEvalList
            if playing.HasExited() then
                logger.LogCritical($"Engine {playing.Name} has exited while lost on time")
            logger.LogCritical("Engine {Engine} lost on time. Time left (ms): {TimeLeftMs}, Move time (ms): {MoveTimeMs}", playing.Name, (if isWhite then wTime.ToTimeSpan().TotalMilliseconds else bTime.ToTimeSpan().TotalMilliseconds), duration.TotalMilliseconds)
            let res = lostOnTimeResult playing.Name opponent.Name isWhite gameMoveList gametimer firstTwoEvals
            let diagnosis = playing.GetDiagnostics()
            logger.LogCritical diagnosis
            return res
          else             
              if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
                  let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
                  do! Async.Sleep delay
          
              if line.StartsWith("bestmove") then
                  // make move mutable so "do not deviate" can substitute an old move if needed
                  let mutable move = line.Split().[1]
                  let ponderMove = if line.Contains "ponder" then (line.Split().[3]) else ""
                  let (currentTime, timeLeft) = 
                    if isWhite then
                      updateClocks duration wTime wIncr useNodes
                    else
                      updateClocks duration bTime bIncr useNodes
                  if isWhite then
                    wTime <- currentTime
                  else
                    bTime <- currentTime                  
                  let mutable posToCheck = board.Position
                  let piecesLeft = PositionOps.numberOfPieces &posToCheck
                  moveInfoData.tl <- int64 (currentTime.ToTimeSpan().TotalMilliseconds)
                  moveInfoData.mt <- int64 duration.TotalMilliseconds
                  moveInfoData.pcs <- byte piecesLeft
                  
                  match tryGetTMoveFromCoordinateNotation &board move with
                  |Some tmove ->
                    let mutable moveAdj = tmove
                    let mutable shortSan = getSanNotationFromTMove &board tmove
                    // Deviation logic only when replay dictionaries provided
                    let deviated, oldMove, engName =
                      match replayOptWhite, replayOptBlack with
                      | Some rw, Some rb ->
                          let hash = board.DeviationHash()
                          let replay = if playing.Name = player1.Name then rw else rb
                          match replay.TryGet(hash) with
                          | None -> false, "", ""
                          | Some rd ->
                              if rd.Move <> move then
                                if rd.Engine = playing.Name then true, rd.Move, rd.Engine
                                else false, "", ""
                              else false, "", ""
                      | _ -> false, "", ""
                    
                    if deviated && playing.Config.ContemptEnabled then
                        // if contempt enabled, allow deviation but log it
                        ConsoleUtils.printInColor 
                            ConsoleColor.Green
                            $"Deviation detected at plycount {board.PlyCount} and was allowed because of contempt enabled\n  Prev move: {oldMove} by {engName}  Current move: {move} by {playing.Name}"
                        board.LongSANMovesPlayed.Add(move)
                        gameMoveList.Add(shortSan)
                        board.MakeMove &tmove
                    
                    elif deviated then
                      match tryGetTMoveFromCoordinateNotation &board oldMove with
                      |Some orgMove ->
                        shortSan <- getSanNotationFromTMove &board orgMove
                        LowLevelUtilities.ConsoleUtils.printInColor 
                            ConsoleColor.Yellow
                            $"Deviation detected at plycount {board.PlyCount} with time left in ms: {moveInfoData.tl}\n  Prev move: {oldMove} by {engName}  Current move: {move} by {playing.Name}"
                        tourny.DeviationCounter <- tourny.DeviationCounter + 1
                        move <- oldMove
                        board.LongSANMovesPlayed.Add(move)
                        gameMoveList.Add(shortSan)
                        board.MakeMove &orgMove
                      |_ -> // quick fix for FRC castling move or illegal previous move
                        if TMoveOps.isCastlingMove tmove && board.IsFRC then                      
                          LowLevelUtilities.ConsoleUtils.printInColor 
                            ConsoleColor.Red
                            $"Deviation bug corrected in FRC castling move with movetype: {tmove.MoveType} - {playing.Name} MoveNr: {moves} Prev move: {oldMove} Current move: {move}"
                        else
                          LowLevelUtilities.ConsoleUtils.printInColor 
                            ConsoleColor.Red
                            $"Deviation detected but previous move illegal: {playing.Name} MoveNr: {moves} Prev move: {oldMove} Current move: {move}"
                        board.LongSANMovesPlayed.Add(move)
                        gameMoveList.Add(shortSan)
                        board.MakeMove &tmove
                    else
                      // record replay entry only when replay dictionaries are supplied
                      let devHash = board.DeviationHash()
                      match replayOptWhite, replayOptBlack with
                      | Some rw, Some rb ->
                          let replay = if playing.Name = player1.Name then rw else rb
                          replay[devHash] <- {Engine=playing.Name; Move = move; TimeLeftInMs = moveInfoData.tl; Hash = pairing.OpeningHash }
                      | _ -> ()
                      board.LongSANMovesPlayed.Add(move)                
                      gameMoveList.Add(shortSan)
                      board.MakeMove &tmove

                    moves <- moves + 1 
                    let ponderSan = getShortSanFromLongSan &board ponderMove
                    moveInfoData.pd <- ponderSan
                    let eval = 
                      if evalList.Length > 0 then evalList.[0] 
                      else
                        //logger.LogInformation($"No eval received for move {move} by {playing.Name} at ply {board.PlyCount}, using last known eval")
                        let thisMove = lastEvalThisMove[playing.Name]
                        if thisMove <> EvalType.NA then
                          thisMove
                        else
                          match lastEvalByEngine.TryGetValue playing.Name with
                          | true, e when e <> EvalType.NA -> e
                          | _ -> EvalType.CP 0.0
                    //maybe add engineStatus1 and engineStatus2 here
                    let pv, pvLong = if playing.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
                    let nps = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
                    let nps = 
                      if nps <> 0. then 
                        nps 
                      else 
                        let s = float numberOfNodes/float duration.TotalSeconds
                        moveInfoData.s <- int64 s
                        s
                
                    let pos = board.Position
                    let fen = BoardHelper.posToFen pos
                    let moveDetail = 
                      {
                        LongSan = move
                        FromSq = move[0..1]
                        ToSq = move[2..3]
                        Color = if board.Position.STM = 8uy then "w" else "b"
                        IsCastling = TMoveOps.isCastlingMove tmove
                        Comments = String.Empty
                        }                 
                    let moveAndFen = {Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen}
                    fullEvalList <- eval::fullEvalList
                    let movesLeft = 
                      Adjudication.movesLeftBeforeDrawAdjudication
                        eval
                        fullEvalList
                        tourny.Adjudication.DrawOption.MinDrawMove
                        (tourny.Adjudication.DrawOption.DrawMoveLength * 2)
                        tourny.Adjudication.DrawOption.MaxDrawScore
                    let bestMove = 
                      { Player = playing.Name
                        Move = move
                        Ponder = ponderSan
                        Eval = eval
                        TimeLeft = timeLeft
                        MoveTime = TimeOnly(duration.Ticks)
                        NPS = nps
                        Nodes = numberOfNodes
                        FEN = fen
                        PV = pv 
                        LongPV = pvLong
                        MoveAndFen = moveAndFen
                        MoveHistory = board.GetShortSanMoveHistory()
                        Move50 = board.Position.Count50 |> int
                        R3 = board.RepetitionNr()
                        PiecesLeft = piecesLeft
                        AdjDrawML = movesLeft
                        }                  
                    
                    if bestMove.R3 > 1 && tourny.VerboseLogging then
                      logger.LogDebug($"Ply {board.PlyCount} - Repetition occurred: {bestMove.R3} time(s)")
                
                    evalList <- []
                    depth <- 0
                    selfdepth <- 0
                    npsList.Clear()
                    resetThisMoveEval playing.Name
                
                    if moveInfoData.q2 > moveInfoData.q1 then                  
                      Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                    if moveInfoData.n2 > moveInfoData.n1 then                  
                      Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                    if tourny.VerboseLogging then
                      logger.LogDebug $"FEN={board.FEN()}"
                
                    //tablebase adjudication here
                    match Adjudication.adjudicateByEval logger board fullEvalList tourny player1.Name player2.Name playing.Name gametimer gameMoveList moves with
                    |Some res -> 
                      if res.Reason = ResultReason.Checkmate then
                        let bm = {bestMove with MoveHistory=bestMove.MoveHistory + "#"}
                        let status = {lastEngineStatus with Eval = EvalType.Mate 0}
                        let numberAndMove = (board.SanMoveNumberString shortSan) + "#"
                        annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                        callback(BestMove (bm, status))
                      else
                        let numberAndMove = board.SanMoveNumberString shortSan
                        annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                        callback(BestMove (bestMove, lastEngineStatus))
                  
                      moveInfoData <- ChessMoveInfo.Empty
                      callback(EndOfGame res)
                      if tourny.VerboseLogging then
                        logger.LogInformation($"Adjudication by eval: {res.Reason}, Pieces left: {piecesLeft} ")
                        logger.LogInformation (sprintf "Info %A: " res)
                      return res
                    |None ->
                      let numberAndMove = board.SanMoveNumberString shortSan
                      annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                      moveInfoData <- ChessMoveInfo.Empty
                      callback(BestMove (bestMove, lastEngineStatus))
                      return! playEngine opponent playing (board.PositionHash())
                  |_ ->                
                    let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                    let firstTwoEvals = firstTwoEvals fullEvalList                    
                    if board.AnyLegalMove() |> not then
                      //either checkmate or stalemate
                      if board.IsMate() then                   
                        let res = 
                          if playing.Name = player1.Name then
                            createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Checkmate dur firstTwoEvals
                          else
                            createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Checkmate dur firstTwoEvals
                        callback(EndOfGame res)
                        logger.LogInformation($"Checkmate: {res.Reason}")
                        return res
                      else                    
                        let res = createResultWithEval player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Stalemate dur firstTwoEvals
                        callback(EndOfGame res)
                        logger.LogInformation($"Stalemate: {res.Reason}")
                        return res
                    else
                      let res = 
                        if playing.Name = player1.Name then
                          createResultWithEval player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur firstTwoEvals
                        else
                          createResultWithEval player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur firstTwoEvals
                      callback(EndOfGame res)
                      let fenAndMoves = board.PositionWithMoves()
                      logger.LogCritical($"{playing.Name} failed in bestmove logic with the following response {line} after these moves: \n{fenAndMoves}")
                      return res               
                 
              else                
                let elapsed = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                let diff = elapsed - lastCheck
                let interval = 1000
               
                if not tourny.TestOptions.PolicyTest && line.StartsWith "info string" && line.Contains "N:" then
                  let nnMsg = Utilities.Regex.getInfoStringData playing.Name line 
                  let list = ResizeArray<NNValues>()
                  list.Add(nnMsg)              
                  let moreItems = line.StartsWith "info string node" |> not
                  if not moreItems && tourny.VerboseLogging then
                    logger.LogDebug "Only one move in log live stats"
                  let mutable cont = moreItems
                  while cont do
                    let! newline = playing.ReadLineAsyncWithTimeout cts.Token |> Async.AwaitTask
                    if newline.StartsWith "bestmove" then
                      cont <- false
                    elif newline.StartsWith "info string node" then
                      cont <- false                    
                    else
                      let msg = Utilities.Regex.getInfoStringData playing.Name newline
                      list.Add msg
                
                  makeShortSan list &board           
                  match Utilities.Engine.calcTopNn list with
                  |Some (n1,n2,q1,q2, p1, pt) -> 
                    moveInfoData.n1 <- n1
                    moveInfoData.n2 <- n2
                    moveInfoData.q1 <- q1
                    moveInfoData.q2 <- q2
                    moveInfoData.p1 <- p1
                    moveInfoData.pt <- pt
                  |None -> ()
                
                  if list.Count > 0 then
                    callback (NNSeq list)

                elif line.StartsWith "info" then              
                  let isWhite = playing.Name = player1.Name
                  match Utilities.Regex.getEssentialDataWithEPS line isWhite with
                  |Some (d, eval, nodes, nps, eps, pvLine, tbhits, wdl, sd, mPv ) ->                 
                    numberOfNodes <- nodes                
                    if d > depth then
                      depth <- d
                    if sd > selfdepth then  
                      selfdepth <- sd
                    npsList.Add(float nps)
                    evalList <- eval :: evalList
                    setEval playing.Name eval
                    moveInfoData.d <- depth
                    moveInfoData.sd <- selfdepth
                    moveInfoData.wv <- eval
                    moveInfoData.n <- nodes
                    moveInfoData.s <- nps
                    moveInfoData.tb <- tbhits
                    moveInfoData.eps <- eps

                    if not (String.IsNullOrEmpty(pvLine)) then
                      if player1.Name = playing.Name then
                        Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                        PVLine1 <- pvLine
                        moveInfoData.pv <- Player1PV
                      else
                        Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                        PVLine2 <- pvLine
                        moveInfoData.pv <- Player2PV
                    let nps = if npsList.Count > 0 then npsList[npsList.Count - 1] else 0.0
                    let pv, pvLong = if playing.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
                    let status = 
                        { 
                          PlayerName = playing.Name
                          Eval = eval
                          Depth = d
                          SD = sd
                          Nodes = nodes
                          NPS = nps //avgNps
                          EPS = float eps
                          TBhits = tbhits
                          WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
                          PV = pv
                          PVLongSAN = pvLong
                          MultiPV = mPv
                        }
                
                    lastEngineStatus <- status
                    if diff > interval && eval <> EvalType.NA then                  
                      lastCheck <- elapsed
                      if playing.Name.ToLower().Contains("lc0") && status.EPS = 0 then
                        logger.LogInformation $"LC0 is reporting EPS = 0, by {playing.Name} at ply {board.PlyCount}"
                      callback(Status status)
                
                  |None -> ()
            
                return! playEngine playing opponent position
            } 
    let startPos = board.PositionHash()
    if board.Position.STM = 0uy then
      playEngine player1 player2 startPos
    else
      playEngine player2 player1 startPos

  let play
    (sb : StringBuilder)
    (cts : CancellationTokenSource)
    (logger : ILogger)
    (tourny : Tournament) 
    (board : Board)
    (player1 : ChessEngine) 
    (player2 : ChessEngine)
    (pairing: Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    callback  =
    playGeneric None None sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback

  let playDoNotDeviate
    (replayWhite: ReferenceGameReplay)
    (replayBlack: ReferenceGameReplay)
    (sb : StringBuilder)
    (cts : CancellationTokenSource)
    (logger : ILogger)
    (tourny : Tournament) 
    (board : Board)
    (player1 : ChessEngine) 
    (player2 : ChessEngine)
    (pairing : Pairing)
    (tryGetUserAdjudication: unit -> UserAdjudication option)
    callback  =
    playGeneric (Some replayWhite) (Some replayBlack) sb cts logger tourny board player1 player2 pairing tryGetUserAdjudication callback

  
  let gauntlet (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) = async {    
    let mutable gameNr = 0
    let sbDev = new StringBuilder()
    //Utilities.Validation.validateAllEnginesAndSomeSettings tourny.EngineSetup.Engines
    logger.LogInformation($"Gauntlet tournament about to start")
    let mutable epdBook = false
    let board = Board()
    board.LoadFen Chess.startPos
    let mutable results = List.empty<Result>
    let games = 
      match tourny.Opening.OpeningsPath with
      |Some path -> 
        if path.ToLower().Contains ".epd" then
          epdBook <- true
          EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
        else
          FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
    let gamesAlreadyPlayed = 
        let fileExists = File.Exists tourny.PgnOutPath      
        if fileExists then
          let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
          parsed |> Array.iter Hash.writeOpeningHashToPgnGame
          parsed
        else
          [||]

    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]    
      
    let roundsToPlay = games |> Seq.truncate tourny.Rounds |> Seq.toList
    let challengers = tourny.EngineSetup.Engines |> List.take tourny.Challengers
    let opponents = tourny.EngineSetup.Engines |> List.skip tourny.Challengers
    let pairings = 
      if tourny.Opening.OpeningsTwice then
        PairingHelper.gauntletDoubleRound tourny.PreventMoveDeviation challengers opponents roundsToPlay
      else
        PairingHelper.gauntletSingleRound tourny.PreventMoveDeviation challengers opponents roundsToPlay
    let playedSet = Utilities.PairingHelper.playedSet gamesAlreadyPlayed
    let gamesLeftToPlay = 
      [
        for p in pairings do
        if Utilities.PairingHelper.hasPlayedBefore p playedSet |> not then
          yield p
      ]
    
    PairingHelper.printAllOpeningPairs logger gamesLeftToPlay
    let totalGames = pairings.Length
    tourny.TotalGames <- totalGames
    let numberOfGamesPlayed = gamesAlreadyPlayed.Length
    
    if gamesLeftToPlay.Length = 0 then
      return results
    else
      callback (Update.TotalNumberOfPairs pairings.Length)   
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      let pgnGameWriterAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
      tourny.CurrentGameNr <- numberOfGamesPlayed
      let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
      let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
      callback (Update.StartOfTournament startInfo)     
      let replayList = ResizeArray<GameReplay>()
      let replayDicts = 
        [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList

      let getReplayDictForPlayer name = replayDicts.[name]
      
      let prepareGameReplay (pairing : Pairing) =
        let nextGame = pairing
        let replayDictWhite = getReplayDictForPlayer pairing.White.Name
        let replayDictBlack = getReplayDictForPlayer pairing.Black.Name
        let lastRelevantLiveGame = 
          replayList 
          |> Seq.tryFind(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))
        
        let latestLiveGames = 
          replayList 
          |> Seq.filter(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))
        
        let allGames = Array.concat [referencGamesPlayed; gamesAlreadyPlayed]
        let refGamesPlayed = allGames |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash)
        let openingPlayedBefore = replayList |> Seq.exists(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash)            
        
        if not openingPlayedBefore then
          //let log = sprintf "No live games played in this opening so we clear all players dict"
          //printfn "%s" log
          for dict in replayDicts do
            dict.Value.Clear()
        
        match refGamesPlayed |> Seq.tryLast with
        |Some _ ->
          let lastRelevantGame = 
            refGamesPlayed 
            |> Seq.tryFind(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))  
          
          let lastRelevantGames = 
            refGamesPlayed 
            |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))          
          
          let previousGames = 
            refGamesPlayed 
            |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))
                    
          //for g in previousGames do
          //  printfn "Previous game found in PGN: %s, %s for pairing %s, %s" g.GameMetaData.White g.GameMetaData.Black pairing.White.Name pairing.Black.Name
          
          let replayBoard = Board()
          let tryInitBoard () = 
            if pairing.Opening.Fen <> "" then
              replayBoard.LoadFen pairing.Opening.Fen
          for game in lastRelevantGames do
            printfn "Relevant saved game found %s, %s for pairing: %s, %s" game.GameMetaData.White game.GameMetaData.Black pairing.White.Name pairing.Black.Name
            let isWhite = game.GameMetaData.White = pairing.White.Name
            let rematch = game.GameMetaData.White = pairing.White.Name && game.GameMetaData.Black = pairing.Black.Name
            if rematch then
              printfn "Rematch found for %s, %s - so games should be identical" game.GameMetaData.White game.GameMetaData.Black
            replayBoard.ResetBoardState()
            tryInitBoard()
            let mutable idx = 0
            for m in game.Mainline do
                let hash = replayBoard.DeviationHash()
                replayBoard.PlaySimpleShortSan m.San
                if m.Color = "w" then
                    if replayBoard.LongSANMovesPlayed.Count > idx then
                        let lastmove = replayBoard.LongSANMovesPlayed[idx]
                        let data : ReplayData = {Engine=game.GameMetaData.White; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}                  
                        if isWhite then
                            replayDictWhite[hash] <- data
                        idx <- idx + 1
                elif m.Color = "b" then
                    if replayBoard.LongSANMovesPlayed.Count > idx then
                        let lastmove = replayBoard.LongSANMovesPlayed[idx]
                        let data : ReplayData = {Engine=game.GameMetaData.Black; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}                  
                        if isWhite |> not then
                            replayDictBlack[hash] <- data
                        idx <- idx + 1

          let moves = 
            match lastRelevantGame with
            |Some game ->
              if game.GameMetaData.White = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
            |None -> 0

          match lastRelevantLiveGame with
          |Some game ->  
              let data = game.PGNMetaData
              let sumGames = (latestLiveGames |> Seq.length) + (lastRelevantGames |> Seq.length)              
              let log = sprintf "First Live game: %s vs %s in round %s, number of games (live and saved) are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves              
              printfn "%s" log
          |None -> 
            match lastRelevantGame with
            |Some game ->  
              let sumGames = lastRelevantGames |> Seq.length
              let log = sprintf "First saved game (no live game yet): %s vs %s in round %s, number of games are: %d, tot moves: %d" game.GameMetaData.White game.GameMetaData.Black game.GameMetaData.Round sumGames moves              
              printfn "%s" log
            |None -> 
              let whiteMoves = replayDictWhite |> Seq.length
              let blackMoves = replayDictBlack |> Seq.length
              let log = sprintf "No relevant saved game found for player (%s, %s) for game number %d, tot moves: %d" pairing.White.Name pairing.Black.Name nextGame.GameNr moves
              printfn "%s whiteDict: %d, BlackDict: %d" log whiteMoves blackMoves
        |_ ->           
          match lastRelevantLiveGame with
          |Some game ->  
              let moves = if game.WhitePlayer = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
              let data = game.PGNMetaData
              let sumGames = (latestLiveGames |> Seq.length)              
              let log = sprintf "First Live game: %s vs %s in round %s, number of games are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves              
              printfn "%s" log
          |_ -> ()
            //let log = sprintf "No relevant live game found for player (%s,%s)" pairing.White.Name pairing.Black.Name            
            //printfn "%s" log

      let searchReplayList (pairing : Pairing) =        
        let nextGame = pairing
        let lastGame = gamesAlreadyPlayed |> Seq.tryLast
        let deviations = match lastGame with |Some g -> g.GameMetaData.Deviations |_ -> 0
        if deviations > tourny.DeviationCounter then
          tourny.DeviationCounter <- deviations
        prepareGameReplay nextGame

      let sb = StringBuilder()
      
      for pair in gamesLeftToPlay do
        if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
          searchReplayList pair
        tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
        if cts.IsCancellationRequested then
          sbDev.Clear() |> ignore
          
        else
          let limit = tourny.Opening.OpeningsPly
          let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
          let completeGame =             
              openingMoves 
              |> Seq.mapi(fun i m ->                   
                    if m.Color = "w" then
                      sprintf "%d. %s" m.MoveNumber m.San
                    else
                      sprintf "%s" m.San)
              |> String.concat " "
            
          logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
          board.ResetBoardState()
          if pair.Opening.Fen = "" then
            board.LoadFen(Chess.startPos)
            board.StartPosition <- Chess.startPos
          else 
            board.LoadFen(pair.Opening.Fen)
            board.StartPosition <- pair.Opening.Fen
            tourny.IsChess960 <- board.IsFRC
          
          if not epdBook then
            for m in openingMoves do
              board.PlayOpeningMove m.San             
            
          let posWithMoves =
            let fen = board.StartPosition
            let start = $"position fen {fen} moves"
            board.LongSANMovesPlayed |> Seq.fold(fun state m -> 
              sprintf "%s %s" state m) start            
          logger.LogInformation("{position}", posWithMoves)
          let engine1 = EngineHelper.createEngine (pair.White, Some logger)
          let engine2 = EngineHelper.createEngine (pair.Black, Some logger)

          let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
          let liveGamesPlayed = gamesLeftToPlay |> Seq.truncate gameNr |> Seq.filter(fun e -> e.OpeningHash = pair.OpeningHash) |> Seq.length
          let roundTxt = $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1 }"
          Update.RoundNr roundTxt |> callback
          let logException ex =
              let tcPlayer1 , tcPlayer2 = (tourny.FindTimeControl (pair.White.TimeControlID)).ToString(), (tourny.FindTimeControl (pair.Black.TimeControlID)).ToString()
              let tcText = sprintf "%s: %s vs %s: %s" pair.White.Name tcPlayer1 pair.Black.Name tcPlayer2
              let createContext() = {
                  EngineName   = engine1.Name
                  OpponentName = engine2.Name
                  GameNumber   = pair.GameNr
                  MoveNumber   = board.MoveNumber()
                  TimeControl  = tcText
                  TimeRemaining= None
                  PositionFen  = board.FEN()
                  LastCommand  = None
                  TimestampUtc = DateTime.UtcNow
                  MoveHistory = board.GetMoveHistory()}
              EngineFailures.log logger ex (createContext())

          let result =
              let gametimer = Stopwatch.GetTimestamp()
              try
                 if tourny.PreventMoveDeviation then              
                   let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name                  
                   playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                 else
                   if tourny.AllowPondering then
                     playWithPondering sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                   else
                     play sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
              with
              | :? EngineStartupException as ex ->
                    logException ex
                    handleGameException logger ex cts gametimer board engine1 engine2 pair
              | ex -> 
                    logException ex
                    // Decide: swallow, rethrow, or translate to a domain Result                                 
                    handleGameException logger ex cts gametimer board engine1 engine2 pair

          results <- result :: results

          let gameData : PGNTypes.GameMetadata = 
            { OpeningHash = pair.OpeningHash
              Event = tourny.Description
              Site= tourny.Name
              Date= DateTime.Now.ToShortDateString()
              Round= roundTxt
              White=result.Player1
              Black=result.Player2
              Result= result.Result
              Reason = result.Reason
              GameTime = result.GameTime
              Moves = result.Moves
              Fen = pair.Opening.Fen
              OpeningName = pair.Opening.GameMetaData.OpeningName
              Deviations = tourny.DeviationCounter
              StartEvals = result.OutOfOpeningEvals
              OtherTags = pair.Opening.GameMetaData.OtherTags
            }

          if tourny.PreventMoveDeviation then
            replayList.Add 
              {
                WhitePlayer = result.Player1
                BlackPlayer = result.Player2
                PGNMetaData = gameData
                LongSanMoves = board.LongSANMovesPlayed |> ResizeArray                 
              }
          let moveSection = sb.ToString()
          if result.Reason <> ResultReason.Cancel 
            && not cts.IsCancellationRequested 
            && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
            pgnGameWriterAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
            //PGNHelper.writePgnGame tourny.PgnOutPath gameData moveSection result
          if tourny.VerboseLogging then
            logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
          if engine1.HasExited() |> not then
            engine1.StopProcess()
          if engine2.HasExited() |> not then
            engine2.StopProcess()

          do! Async.Sleep(tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int)
          board.ResetBoardState()
          gameNr <- gameNr + 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res) 
      
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      pgnGameWriterAgent.Post(Parser.FullPGNParser.Dispose)
      pgnGameWriterAgent.Dispose()      
      return results
  }

  let roundRobin (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) = async {        
    
    //Utilities.Validation.validateAllEnginesAndSomeSettings tourny.EngineSetup.Engines    
    let mutable gameNr = 0
    logger.LogInformation($"Round robin tournament about to start")
    let numberOfPlayers = tourny.EngineSetup.Engines.Length
    let mutable epdBook = false
    let board = Board()
    board.LoadFen Chess.startPos
    let mutable engine1 = defaultof<ChessEngine>
    let mutable engine2 = defaultof<ChessEngine>
    let mutable results = List.empty<Result>
    let games = 
      match tourny.Opening.OpeningsPath with
      |Some path -> 
        if path.ToLower().Contains ".epd" then
          epdBook <- true
          EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
        else
          FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
   
    let gamesAlreadyPlayed = 
      let fileExists = File.Exists tourny.PgnOutPath
      if fileExists then
          let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
          parsed |> Array.iter Hash.writeOpeningHashToPgnGame
          parsed
      else
        [||]
    
    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]
    
    let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
    let pairings = 
      if tourny.Opening.OpeningsTwice then
        PairingHelper.generateAllRoundRobinDoubleRounds tourny.EngineSetup.Engines gamesToPlay
      else
       PairingHelper.generateAllRoundRobinSingleRounds tourny.EngineSetup.Engines gamesToPlay
    let playedSet = Utilities.PairingHelper.playedSet gamesAlreadyPlayed
    let gamesLeftToPlay = 
      [
        for p in pairings do
        if Utilities.PairingHelper.hasPlayedBefore p playedSet |> not then
          yield p
      ]

    PairingHelper.printAllOpeningPairs logger gamesLeftToPlay
    let totalGames = pairings.Length
    tourny.TotalGames <- totalGames
    
    let numberOfGamesPlayed = gamesAlreadyPlayed.Length    
    if gamesLeftToPlay.Length = 0 then
      return results    
    else
      let pgnGameWriterAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
      callback (Update.TotalNumberOfPairs pairings.Length)      
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))      
      tourny.CurrentGameNr <- numberOfGamesPlayed
      let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny pairings
      let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
      callback (Update.StartOfTournament startInfo)
      let replayList = ResizeArray<GameReplay>()
      let replayDicts = 
        [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
      let getReplayDictForPlayer (name:string) = replayDicts.[name]

      let searchReplayList (pairing : Pairing) =        
        let nextGame = pairing
        let lastGame = gamesAlreadyPlayed |> Seq.tryLast
        let deviations = match lastGame with |Some g -> g.GameMetaData.Deviations |_ -> 0
        if deviations > tourny.DeviationCounter then
          tourny.DeviationCounter <- deviations
        prepareGameReplay nextGame replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960

      let sb = StringBuilder()

      for pair in gamesLeftToPlay do
        if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
          searchReplayList pair
        tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
        if cts.IsCancellationRequested then
          sb.Clear() |> ignore
        else
          let limit = tourny.Opening.OpeningsPly
          let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
          let completeGame =             
              openingMoves 
              |> Seq.mapi(fun i m ->                   
                    if m.Color = "w" then
                      sprintf "%d. %s" m.MoveNumber m.San
                    else
                      sprintf "%s" m.San)
              |> String.concat " "
          
          logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
          board.ResetBoardState()
          if pair.Opening.Fen = "" then
            board.LoadFen Chess.startPos
            board.StartPosition <- Chess.startPos
          else 
            board.LoadFen(pair.Opening.Fen)
            board.StartPosition <- pair.Opening.Fen
          
          let mutable moveIndex = 0
          if not epdBook then
            for m in openingMoves do
              board.PlayOpeningMove m.San
              
          else
            board.ResetBoardState()
            board.LoadFen pair.Opening.Fen
            board.StartPosition <- pair.Opening.Fen
            tourny.IsChess960 <- board.IsFRC

          let posWithMoves =
            let fen = board.StartPosition
            let start = $"position fen {fen} moves"
            board.LongSANMovesPlayed |> Seq.fold(fun state m -> 
              sprintf "%s %s" state m) start            
          logger.LogInformation("{position}", posWithMoves)

          if numberOfPlayers > 2 then
            engine1 <- EngineHelper.createEngine (pair.White, Some logger)
            engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
          if engine1 = defaultof<ChessEngine> || engine2 = defaultof<ChessEngine> then
            engine1 <- EngineHelper.createEngine (pair.White, Some logger)
            engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
          if engine1.Name = pair.Black.Name || engine2.Name = pair.White.Name then
            let (eng1,eng2) = engine2, engine1
            engine1 <- eng1
            engine2 <- eng2
          
          let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
          let liveGamesPlayed = gamesLeftToPlay |> Seq.truncate gameNr |> Seq.filter(fun e -> e.OpeningHash = pair.OpeningHash) |> Seq.length
          let roundTxt = $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1 }"
          Update.RoundNr roundTxt |> callback          
          let logException ex =
              let w = tourny.TimeControlTextForPlayer engine1.Config.TimeControlID
              let b = tourny.TimeControlTextForPlayer engine2.Config.TimeControlID
              let timeInfo = $"[{w}; {b}]"
              let createContext() = {
                  EngineName   = engine1.Name
                  OpponentName = engine2.Name
                  GameNumber   = pair.GameNr
                  MoveNumber   = board.MoveNumber()
                  TimeControl  = timeInfo
                  TimeRemaining= None
                  PositionFen  = board.FEN()
                  LastCommand  = None
                  TimestampUtc = DateTime.UtcNow
                  MoveHistory = board.GetMoveHistory()}
              EngineFailures.log logger ex (createContext())
          
          let result =
              let gametimer = Stopwatch.GetTimestamp()
              try
                  if tourny.PreventMoveDeviation then              
                    let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name                  
                    playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                  else
                    //play sb cts logger tourny board engine1 engine2 pair callback |> Async.RunSynchronously
                    if tourny.AllowPondering then
                      playWithPondering sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                    else
                      play sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously              
              with
              | :? EngineStartupException as ex ->
                    logException ex
                    handleGameException logger ex cts gametimer board engine1 engine2 pair
              | ex -> 
                    logException ex
                    // Decide: swallow, rethrow, or translate to a domain Result                        
                    handleGameException logger ex cts gametimer board engine1 engine2 pair

          let forceStopEngines = match result.Reason with | ResultReason.Disconnected _ -> true | _ -> false              
          results <- result :: results

          let gameData : PGNTypes.GameMetadata = 
            {
              OpeningHash = pair.OpeningHash
              Event = tourny.Description
              Site = tourny.Name
              Date = DateTime.Now.ToShortDateString()
              Round = roundTxt
              White = result.Player1
              Black = result.Player2
              Result = result.Result
              Reason = result.Reason
              GameTime = result.GameTime
              Moves = result.Moves
              Fen = pair.Opening.Fen
              OpeningName = pair.Opening.GameMetaData.OpeningName
              Deviations = tourny.DeviationCounter
              StartEvals = result.OutOfOpeningEvals
              OtherTags = pair.Opening.GameMetaData.OtherTags 
            }
          if tourny.PreventMoveDeviation then
            replayList.Add 
              {
                WhitePlayer = result.Player1
                BlackPlayer = result.Player2
                PGNMetaData = gameData
                LongSanMoves = board.LongSANMovesPlayed |> ResizeArray                 
              }
          
          let moveSection = sb.ToString()
          if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
            pgnGameWriterAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
            
          if tourny.VerboseLogging then
            logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
         
          // Small delay to let pumps finish their cleanup
          do! Async.Sleep 200
          
          if forceStopEngines || numberOfPlayers > 2 || cts.IsCancellationRequested then
            if engine1.HasExited() |> not then 
                engine1.StopProcess()
            if engine2.HasExited() |> not then 
                engine2.StopProcess()          
          do! Async.Sleep(tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int)
          board.ResetBoardState()
          gameNr <- gameNr + 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res) 
      
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      pgnGameWriterAgent.Post(Parser.FullPGNParser.Dispose)
      pgnGameWriterAgent.Dispose()      
      return results
  }

  let cup (strategy: Utilities.PairingHelper.CupSeedingStrategy) (uniquePerMatchOnly: bool) (resumeRequested: bool) (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) = async {        
    let isPowerOfTwo (n: int) = n > 0 && (n &&& (n - 1) = 0)
    let resolveCupBracketPath () =
      let configuredPath =
        if obj.ReferenceEquals(tourny.CupOptions, null) then "" else tourny.CupOptions.BracketPath
      let fullPath =
        if String.IsNullOrWhiteSpace configuredPath then
          let candidates =
            [ Path.Combine(Environment.CurrentDirectory, "wwwroot")
              Path.Combine(Environment.CurrentDirectory, "WebGUI", "wwwroot") ]
          let folder =
            candidates |> List.tryFind Directory.Exists
            |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "wwwroot"))
          Path.Combine(folder, "cup_bracket.json")
        elif Path.IsPathRooted configuredPath then
          configuredPath
        else
          Path.Combine(Environment.CurrentDirectory, configuredPath)
      let dir = Path.GetDirectoryName(fullPath)
      if String.IsNullOrWhiteSpace dir |> not then
        Directory.CreateDirectory(dir) |> ignore
      fullPath

    let writeCupBracket (agent: MailboxProcessor<CupBracketMessage>) (bracket: CupBracket) =
      agent.PostAndReply(fun reply -> WriteCupBracket(bracket, reply))
      callback Update.CupBracketUpdated

    let roundPairIncrements =
      if obj.ReferenceEquals(tourny.CupOptions, null) then [] else tourny.CupOptions.RoundPairIncrements
    let gamesPerMatch = 2
    let gamesPerMatchForRound roundNumber =
      Utilities.PairingHelper.gamesPerMatchForRound gamesPerMatch roundPairIncrements roundNumber

    let mutable gameNr = 0
    logger.LogInformation("Cup tournament about to start")
    let numberOfPlayers = tourny.EngineSetup.Engines.Length
    if isPowerOfTwo numberOfPlayers |> not then
      logger.LogError("Cup tournaments require a power-of-two number of players, got {playerCount}", numberOfPlayers)
      failwith "Cup tournaments require a power-of-two number of players."

    let mutable epdBook = false
    let board = Board()
    board.LoadFen Chess.startPos
    let mutable engine1 = defaultof<ChessEngine>
    let mutable engine2 = defaultof<ChessEngine>
    let mutable results = List.empty<Result>
    let games =
      match tourny.Opening.OpeningsPath with
      | Some path ->
        if path.ToLower().Contains ".epd" then
          epdBook <- true
          EPDExtractor.parseEPDFile path |> Seq.toArray
        else
          FullPGNParser.parsePgnFile path |> Seq.toArray
      | _ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]

    let gamesAlreadyPlayed =
      let fileExists = File.Exists tourny.PgnOutPath
      if fileExists then
        let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
        parsed |> Array.iter Hash.writeOpeningHashToPgnGame
        parsed
      else
        [||]

    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]

    let openings = games |> Seq.truncate tourny.Rounds |> Seq.toList
    if openings.IsEmpty then
      logger.LogError("No openings available for cup tournament")
      failwith "No openings available for cup tournament."
    let randomOpenings = if obj.ReferenceEquals(tourny.CupOptions, null) then false else tourny.CupOptions.RandomOpenings

    let liveGamesByOpening = Dictionary<string,int>()
    let pgnGameWriterAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
    let replayList = ResizeArray<GameReplay>()
    let replayDicts =
      [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
    let getReplayDictForPlayer (name:string) = replayDicts.[name]
    let cupBracketPath = resolveCupBracketPath()
    let cupBracketAgent = startCupBracketReaderWriter cupBracketPath
    let mutable matchId = 1
    let mutable openingIndex = 0
    let loadBracket () =
      cupBracketAgent.PostAndReply(fun reply -> ReadCupBracket reply)

    let ensureMatchOpeningOrder (matchInfo: CupMatch) =
      if obj.ReferenceEquals(matchInfo.OpeningOrder, null) then
        matchInfo.OpeningOrder <- ResizeArray<int>()

    let ensureGlobalOpeningOrder (bracket: CupBracket) =
      if obj.ReferenceEquals(bracket.GlobalOpeningOrder, null) then
        bracket.GlobalOpeningOrder <- ResizeArray<int>()

    let mutable bracket =
      match loadBracket () with
      | Some loaded ->
          ensureGlobalOpeningOrder loaded
          for round in loaded.Rounds do
            for matchInfo in round.Matches do
              ensureMatchOpeningOrder matchInfo
          openingIndex <- loaded.NextOpeningIndex
          matchId <- loaded.Rounds |> Seq.collect (fun r -> r.Matches |> Seq.map (fun m -> m.MatchId)) |> Seq.append [0] |> Seq.max |> (+) 1
          loaded
      | None ->
          if resumeRequested then
            logger.LogError("Resume requested for cup tournament but no bracket data was found.")
            failwith "Resume requested but cup bracket data is missing."
          { TournamentName = tourny.Name
            Strategy = strategy.ToString()
            GamesPerMatch = gamesPerMatchForRound 1
            UniqueOpeningsGlobal = not uniquePerMatchOnly
            NextOpeningIndex = openingIndex
            GlobalOpeningOrder = ResizeArray<int>()
            Rounds = ResizeArray<CupRound>()
            UpdatedUtc = DateTime.UtcNow }

    let totalRounds =
      let mutable players = numberOfPlayers
      let mutable rounds = 0
      while players > 1 do
        players <- players / 2
        rounds <- rounds + 1
      rounds

    let seedBands = Utilities.PairingHelper.autoSeedBands numberOfPlayers

    let seedPlayers (players: EngineConfig list) =
      match strategy with
      | Utilities.PairingHelper.CupSeedingStrategy.Random ->
          let shuffled = players |> List.toArray
          Random.Shuffle(shuffled)
          shuffled |> Array.toList
        | Utilities.PairingHelper.CupSeedingStrategy.ByRating ->
            Utilities.PairingHelper.seedByBands players seedBands true

    let seedPairs (players: EngineConfig list) =
      let half = players.Length / 2
      let top, bottom = players |> List.splitAt half
      List.zip top (List.rev bottom)

    let seededPlayers = seedPlayers tourny.EngineSetup.Engines

    let ensureRound (roundNumber: int) =
      if bracket.Rounds |> Seq.exists (fun r -> r.RoundNumber = roundNumber) then
        ()
      else
        let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
        let roundMatches = ResizeArray<CupMatch>()
        for _ in 1..matchCount do
          let matchInfo =
            { MatchId = matchId
              RoundNumber = roundNumber
              PlayerA = "TBD"
              PlayerB = "TBD"
              PlayerARating = 0
              PlayerBRating = 0
              ScoreA = 0.0
              ScoreB = 0.0
              Winner = None
              IsDecided = false
              Games = ResizeArray<CupGame>()
              OpeningOrder = ResizeArray<int>() }
          matchId <- matchId + 1
          roundMatches.Add matchInfo
        bracket.Rounds.Add { RoundNumber = roundNumber; Matches = roundMatches }
    if bracket.Rounds.Count = 0 then
      if resumeRequested then
        logger.LogError("Resume requested for cup tournament but bracket rounds are empty.")
        failwith "Resume requested but cup bracket is empty."
      for roundNumber in 1..totalRounds do
        let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
        let roundMatches = ResizeArray<CupMatch>()
        if roundNumber = 1 then
          let pairs = seedPairs seededPlayers
          for (a, b) in pairs do
            let matchInfo =
              { MatchId = matchId
                RoundNumber = roundNumber
                PlayerA = a.Name
                PlayerB = b.Name
                PlayerARating = a.Rating
                PlayerBRating = b.Rating
                ScoreA = 0.0
                ScoreB = 0.0
                Winner = None
                IsDecided = false
                Games = ResizeArray<CupGame>()
                OpeningOrder = ResizeArray<int>() }
            matchId <- matchId + 1
            roundMatches.Add matchInfo
        else
          for _ in 1..matchCount do
            let matchInfo =
              { MatchId = matchId
                RoundNumber = roundNumber
                PlayerA = "TBD"
                PlayerB = "TBD"
                PlayerARating = 0
                PlayerBRating = 0
                ScoreA = 0.0
                ScoreB = 0.0
                Winner = None
                IsDecided = false
                Games = ResizeArray<CupGame>()
                OpeningOrder = ResizeArray<int>() }
            matchId <- matchId + 1
            roundMatches.Add matchInfo
        let round = { RoundNumber = roundNumber; Matches = roundMatches }
        bracket.Rounds.Add round

      writeCupBracket cupBracketAgent bracket

    let minTotalGames =
      [ 1 .. totalRounds ]
      |> List.sumBy (fun roundNumber ->
          let matchCount = Math.Max(1, numberOfPlayers / (pown 2 roundNumber))
          matchCount * gamesPerMatchForRound roundNumber)
    tourny.TotalGames <- minTotalGames
    callback (Update.TotalNumberOfPairs minTotalGames)
    callback (Update.PairingList (ResizeArray<Pairing>()))
    let (tTime, gTime) = estimateTournamentAndGameTime minTotalGames tourny []
    let startInfo = { NumberOfGames = minTotalGames; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny }
    callback (Update.StartOfTournament startInfo)

    let ensureGlobalOpeningOrder () =
      if randomOpenings && openings.Length > 1 && not uniquePerMatchOnly then
        if bracket.GlobalOpeningOrder.Count = 0 then
          let order = [0 .. openings.Length - 1]
          let shuffled = order |> List.toArray
          Random.Shuffle(shuffled)
          bracket.GlobalOpeningOrder <- ResizeArray<int>(shuffled)
          writeCupBracket cupBracketAgent bracket

    let globalOpenings =
      ensureGlobalOpeningOrder ()
      if bracket.GlobalOpeningOrder.Count > 0 then
        bracket.GlobalOpeningOrder
        |> Seq.map (fun idx -> openings.[idx % openings.Length])
        |> Seq.toList
      else
        openings

    let getNextOpening (openingsList: PGNTypes.PgnGame list) (localIndex: int) =
      if uniquePerMatchOnly then
        openingsList.[localIndex % openingsList.Length]
      else
        if openingIndex >= openingsList.Length then
          openingIndex <- 0
        let opening = openingsList.[openingIndex]
        openingIndex <- openingIndex + 1
        bracket.NextOpeningIndex <- openingIndex
        opening

    let searchReplayList (pairing : Pairing) =
      let nextGame = pairing
      let lastGame = gamesAlreadyPlayed |> Seq.tryLast
      let deviations = match lastGame with | Some g -> g.GameMetaData.Deviations | _ -> 0
      if deviations > tourny.DeviationCounter then
        tourny.DeviationCounter <- deviations
      prepareGameReplay nextGame replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960

    let sb = StringBuilder()
    tourny.CurrentGameNr <- gamesAlreadyPlayed.Length

    let playPairing (pair: Pairing) = async {
      if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
        searchReplayList pair
      tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
      if cts.IsCancellationRequested then
        sb.Clear() |> ignore
        return Result.Empty
      else
        let limit = tourny.Opening.OpeningsPly
        let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
        let completeGame =
            openingMoves
            |> Seq.mapi(fun i m ->
                  if m.Color = "w" then
                    sprintf "%d. %s" m.MoveNumber m.San
                  else
                    sprintf "%s" m.San)
            |> String.concat " "

        logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
        board.ResetBoardState()
        if pair.Opening.Fen = "" then
          board.LoadFen Chess.startPos
          board.StartPosition <- Chess.startPos
        else
          board.LoadFen(pair.Opening.Fen)
          board.StartPosition <- pair.Opening.Fen

        if not epdBook then
          for m in openingMoves do
            board.PlayOpeningMove m.San
        else
          board.ResetBoardState()
          board.LoadFen pair.Opening.Fen
          board.StartPosition <- pair.Opening.Fen
          tourny.IsChess960 <- board.IsFRC

        let posWithMoves =
          let fen = board.StartPosition
          let start = $"position fen {fen} moves"
          board.LongSANMovesPlayed |> Seq.fold(fun state m ->
            sprintf "%s %s" state m) start
        logger.LogInformation("{position}", posWithMoves)

        if numberOfPlayers > 2 then
          engine1 <- EngineHelper.createEngine (pair.White, Some logger)
          engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
        if engine1 = defaultof<ChessEngine> || engine2 = defaultof<ChessEngine> then
          engine1 <- EngineHelper.createEngine (pair.White, Some logger)
          engine2 <- EngineHelper.createEngine (pair.Black, Some logger)
        if engine1.Name = pair.Black.Name || engine2.Name = pair.White.Name then
          let (eng1,eng2) = engine2, engine1
          engine1 <- eng1
          engine2 <- eng2

        let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
        let liveGamesPlayed = if liveGamesByOpening.ContainsKey pair.OpeningHash then liveGamesByOpening.[pair.OpeningHash] else 0
        let roundTxt =
          if String.IsNullOrWhiteSpace pair.RoundNr |> not then
            pair.RoundNr
          else
            $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1}"
        Update.RoundNr roundTxt |> callback
        let logException ex =
            let w = tourny.TimeControlTextForPlayer engine1.Config.TimeControlID
            let b = tourny.TimeControlTextForPlayer engine2.Config.TimeControlID
            let timeInfo = $"[{w}; {b}]"
            let createContext() = {
                EngineName   = engine1.Name
                OpponentName = engine2.Name
                GameNumber   = pair.GameNr
                MoveNumber   = board.MoveNumber()
                TimeControl  = timeInfo
                TimeRemaining= None
                PositionFen  = board.FEN()
                LastCommand  = None
                TimestampUtc = DateTime.UtcNow
                MoveHistory = board.GetMoveHistory()}
            EngineFailures.log logger ex (createContext())

        let result =
            let gametimer = Stopwatch.GetTimestamp()
            try
                if tourny.PreventMoveDeviation then
                  let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name
                  playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                else
                  if tourny.AllowPondering then
                    playWithPondering sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                  else
                    play sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
            with
            | :? EngineStartupException as ex ->
                  logException ex
                  handleGameException logger ex cts gametimer board engine1 engine2 pair
            | ex ->
                  logException ex
                  handleGameException logger ex cts gametimer board engine1 engine2 pair

        let isCancelled = result.Reason = ResultReason.Cancel
        let forceStopEngines = match result.Reason with | ResultReason.Disconnected _ -> true | _ -> false
        if not isCancelled then
          results <- result :: results

        if not isCancelled then
          let gameData : PGNTypes.GameMetadata =
            {
              OpeningHash = pair.OpeningHash
              Event = tourny.Description
              Site = tourny.Name
              Date = DateTime.Now.ToShortDateString()
              Round = roundTxt
              White = result.Player1
              Black = result.Player2
              Result = result.Result
              Reason = result.Reason
              GameTime = result.GameTime
              Moves = result.Moves
              Fen = pair.Opening.Fen
              OpeningName = pair.Opening.GameMetaData.OpeningName
              Deviations = tourny.DeviationCounter
              StartEvals = result.OutOfOpeningEvals
              OtherTags = pair.Opening.GameMetaData.OtherTags
            }
          if tourny.PreventMoveDeviation then
            replayList.Add
              {
                WhitePlayer = result.Player1
                BlackPlayer = result.Player2
                PGNMetaData = gameData
                LongSanMoves = board.LongSANMovesPlayed |> ResizeArray
              }

          let moveSection = sb.ToString()
          if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
            pgnGameWriterAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))

          if tourny.VerboseLogging then
            logger.LogInformation("Game metadata added to result: {pgnData}", gameData)

        do! Async.Sleep 200

        if forceStopEngines || numberOfPlayers > 2 || cts.IsCancellationRequested then
          if engine1.HasExited() |> not then
              engine1.StopProcess()
          if engine2.HasExited() |> not then
              engine2.StopProcess()
        board.ResetBoardState()
        if not isCancelled then
          gameNr <- gameNr + 1
          if liveGamesByOpening.ContainsKey pair.OpeningHash then
            liveGamesByOpening.[pair.OpeningHash] <- liveGamesByOpening.[pair.OpeningHash] + 1
          else
            liveGamesByOpening.[pair.OpeningHash] <- 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res)
        return result
        }

    let getRoundPlayers (round: CupRound) =
      round.Matches
      |> Seq.collect (fun m -> [ m.PlayerA; m.PlayerB ])
      |> Seq.filter (fun name -> not (String.IsNullOrWhiteSpace name) && not (name.Equals("TBD", StringComparison.OrdinalIgnoreCase)))
      |> Seq.distinct
      |> Seq.toList

    let initialRound =
      bracket.Rounds
      |> Seq.sortBy (fun r -> r.RoundNumber)
      |> Seq.tryFind (fun r -> r.Matches |> Seq.exists (fun m -> not m.IsDecided))
      |> Option.defaultValue (bracket.Rounds |> Seq.sortByDescending (fun r -> r.RoundNumber) |> Seq.tryHead |> Option.defaultValue { RoundNumber = 1; Matches = ResizeArray() })

    let currentRoundPlayers =
      let players = getRoundPlayers initialRound
      if players.IsEmpty then
        seededPlayers |> List.map (fun e -> e.Name)
      else
        players

    let currentPlayersFromNames names =
      names
      |> List.choose (fun name -> tourny.EngineSetup.Engines |> List.tryFind (fun e -> e.Name = name))

    let mutable currentPlayers = currentPlayersFromNames currentRoundPlayers
    let mutable roundNumber = initialRound.RoundNumber
    while currentPlayers.Length > 1 && not cts.IsCancellationRequested do
      let pairs =
        currentPlayers
        |> List.chunkBySize 2
        |> List.choose (function | [a; b] -> Some (a, b) | _ -> None)
      let round =
        bracket.Rounds
        |> Seq.find (fun r -> r.RoundNumber = roundNumber)
      for matchIndex in 0..(pairs.Length - 1) do
        let (a, b) = pairs.[matchIndex]
        let matchInfo = round.Matches.[matchIndex]
        let updated =
          { matchInfo with
              PlayerA = a.Name
              PlayerB = b.Name
              PlayerARating = a.Rating
              PlayerBRating = b.Rating }
        round.Matches.[matchIndex] <- updated

      writeCupBracket cupBracketAgent bracket

      let winners = ResizeArray<EngineConfig>()
      for matchIndex in 0..(pairs.Length - 1) do
        let pair = pairs.[matchIndex]
        let matchInfo = round.Matches.[matchIndex]
        let playerA, playerB = pair
        if matchInfo.IsDecided then
          ()
        else
          let playedInMatch = matchInfo.Games.Count
          let gamesPerMatch = gamesPerMatchForRound roundNumber
          let mutable gamesRemaining = Math.Max(0, gamesPerMatch - playedInMatch)
          let mutable localOpeningIndex = playedInMatch / 2
          let matchOpenings =
            if uniquePerMatchOnly then
              if randomOpenings && openings.Length > 1 then
                if matchInfo.OpeningOrder.Count = 0 then
                  let order = [0 .. openings.Length - 1]
                  let shuffled = order |> List.toArray
                  Random.Shuffle(shuffled)
                  matchInfo.OpeningOrder <- ResizeArray<int>(shuffled)
                  writeCupBracket cupBracketAgent bracket
                matchInfo.OpeningOrder
                |> Seq.map (fun idx -> openings.[idx % openings.Length])
                |> Seq.toList
              else
                openings
            else
              globalOpenings
          let tryGetOpeningByHash (hash: string) =
            match matchOpenings |> List.tryFind (fun o ->
              let openingHash =
                if String.IsNullOrWhiteSpace o.Raw then
                  Hash.computeOpeningHash (o.GameNumber.ToString())
                else
                  Hash.computeOpeningHash o.Raw
              openingHash = hash) with
            | Some opening -> opening
            | None -> getNextOpening matchOpenings localOpeningIndex

          if not matchInfo.IsDecided then
            if matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.PlayerA
              writeCupBracket cupBracketAgent bracket
            elif matchInfo.ScoreB > matchInfo.ScoreA + float gamesRemaining then
              matchInfo.IsDecided <- true
              matchInfo.Winner <- Some matchInfo.PlayerB
              writeCupBracket cupBracketAgent bracket
            elif gamesRemaining = 0 then
              if matchInfo.ScoreA > matchInfo.ScoreB then
                matchInfo.IsDecided <- true
                matchInfo.Winner <- Some matchInfo.PlayerA
                writeCupBracket cupBracketAgent bracket
              elif matchInfo.ScoreB > matchInfo.ScoreA then
                matchInfo.IsDecided <- true
                matchInfo.Winner <- Some matchInfo.PlayerB
                writeCupBracket cupBracketAgent bracket

          while matchInfo.IsDecided |> not && not cts.IsCancellationRequested do
            let tieBreakRound = gamesRemaining = 0
            if gamesRemaining = 0 then
              gamesRemaining <- 2
            let hasOddGame = matchInfo.Games.Count % 2 = 1
            let opening =
              if hasOddGame then
                let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
                tryGetOpeningByHash lastGame.OpeningHash
              else
                let usedHashes =
                  matchInfo.Games
                  |> Seq.map (fun g -> g.OpeningHash)
                  |> Set.ofSeq
                let openingIndexForMatch =
                  if usedHashes.Count < matchOpenings.Length then
                    Utilities.PairingHelper.nextUnusedOpeningIndex usedHashes matchOpenings localOpeningIndex
                  else
                    localOpeningIndex % matchOpenings.Length
                localOpeningIndex <- openingIndexForMatch + 1
                matchOpenings.[openingIndexForMatch]
            let openingHash = Hash.computeOpeningHashFromGame opening
            let playOrder =
              if hasOddGame then
                let lastGame = matchInfo.Games.[matchInfo.Games.Count - 1]
                if lastGame.White = playerA.Name then
                  [ (playerB, playerA) ]
                else
                  [ (playerA, playerB) ]
              else
                [ (playerA, playerB); (playerB, playerA) ]
            let plannedPairings =
              Utilities.PairingHelper.buildRemainingCupPairings
                matchInfo
                playerA
                playerB
                matchOpenings
                opening
                playOrder
                gamesRemaining
                localOpeningIndex
            callback (Update.PairingList plannedPairings)
            for (white, black) in playOrder do
              if matchInfo.IsDecided || gamesRemaining = 0 || cts.IsCancellationRequested then
                ()
              else
                let pairing =
                  { Opening = opening
                    White = white
                    Black = black
                    GameNr = 0
                    RoundNr = $"{matchInfo.RoundNumber}.{matchInfo.Games.Count + 1}"
                    OpeningHash = openingHash }
                let! result = playPairing pairing
                if result.Reason <> ResultReason.Cancel then
                  let game =
                    { GameNr = gameNr
                      White = white.Name
                      Black = black.Name
                      OpeningId = opening.GameNumber.ToString()
                      OpeningHash = openingHash
                      Result = result.Result }
                  matchInfo.Games.Add game
                  let scoreWhite =
                    match result.Result with
                    | "1-0" -> 1.0
                    | "0-1" -> 0.0
                    | "1/2-1/2" -> 0.5
                    | _ -> 0.0
                  let scoreBlack =
                    match result.Result with
                    | "1-0" -> 0.0
                    | "0-1" -> 1.0
                    | "1/2-1/2" -> 0.5
                    | _ -> 0.0
                  if white.Name = matchInfo.PlayerA then
                    matchInfo.ScoreA <- matchInfo.ScoreA + scoreWhite
                    matchInfo.ScoreB <- matchInfo.ScoreB + scoreBlack
                  else
                    matchInfo.ScoreA <- matchInfo.ScoreA + scoreBlack
                    matchInfo.ScoreB <- matchInfo.ScoreB + scoreWhite
                  gamesRemaining <- gamesRemaining - 1
                  if matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining then
                    matchInfo.IsDecided <- true
                    matchInfo.Winner <- Some matchInfo.PlayerA
                  elif matchInfo.ScoreB > matchInfo.ScoreA + float gamesRemaining then
                    matchInfo.IsDecided <- true
                    matchInfo.Winner <- Some matchInfo.PlayerB
                  elif gamesRemaining = 0 then
                    if matchInfo.ScoreA > matchInfo.ScoreB then
                      matchInfo.IsDecided <- true
                      matchInfo.Winner <- Some matchInfo.PlayerA
                    elif matchInfo.ScoreB > matchInfo.ScoreA then
                      matchInfo.IsDecided <- true
                      matchInfo.Winner <- Some matchInfo.PlayerB
                  writeCupBracket cupBracketAgent bracket
        match matchInfo.Winner with
        | Some name when name = playerA.Name ->
            winners.Add playerA
            if roundNumber < totalRounds then
              ensureRound (roundNumber + 1)
              let nextRound = bracket.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber + 1)
              match nextRound with
              | Some next ->
                  let nextIndex = matchIndex / 2
                  let nextMatch = next.Matches.[nextIndex]
                  let updated =
                    if matchIndex % 2 = 0 then
                      { nextMatch with PlayerA = playerA.Name; PlayerARating = playerA.Rating }
                    else
                      { nextMatch with PlayerB = playerA.Name; PlayerBRating = playerA.Rating }
                  next.Matches.[nextIndex] <- updated
                  writeCupBracket cupBracketAgent bracket
              | None -> ()
        | Some name when name = playerB.Name ->
            winners.Add playerB
            if roundNumber < totalRounds then
              ensureRound (roundNumber + 1)
              let nextRound = bracket.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber + 1)
              match nextRound with
              | Some next ->
                  let nextIndex = matchIndex / 2
                  let nextMatch = next.Matches.[nextIndex]
                  let updated =
                    if matchIndex % 2 = 0 then
                      { nextMatch with PlayerA = playerB.Name; PlayerARating = playerB.Rating }
                    else
                      { nextMatch with PlayerB = playerB.Name; PlayerBRating = playerB.Rating }
                  next.Matches.[nextIndex] <- updated
                  writeCupBracket cupBracketAgent bracket
              | None -> ()
        | _ -> ()

      currentPlayers <- winners |> Seq.toList
      roundNumber <- roundNumber + 1

    let res = ResizeArray<Result>(results)
    callback (Update.PeriodicResults res)
    pgnGameWriterAgent.Post(Parser.FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
    cupBracketAgent.Post DisposeCupBracket
    return results
  }

  let swiss (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) (tryGetUserAdjudication: unit -> UserAdjudication option) = async {
    let resolveSwissPath () =
      let configuredPath =
        if obj.ReferenceEquals(tourny.SwissOptions, null) then "" else tourny.SwissOptions.StatePath
      let fullPath =
        if String.IsNullOrWhiteSpace configuredPath then
          let candidates =
            [ Path.Combine(Environment.CurrentDirectory, "wwwroot")
              Path.Combine(Environment.CurrentDirectory, "WebGUI", "wwwroot") ]
          let folder =
            candidates |> List.tryFind Directory.Exists
            |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "wwwroot"))
          Path.Combine(folder, "swiss_state.json")
        elif Path.IsPathRooted configuredPath then
          configuredPath
        else
          Path.Combine(Environment.CurrentDirectory, configuredPath)
      let dir = Path.GetDirectoryName(fullPath)
      if String.IsNullOrWhiteSpace dir |> not then
        Directory.CreateDirectory(dir) |> ignore
      fullPath

    let writeSwissState (agent: MailboxProcessor<SwissStateMessage>) (state: TypesDef.Swiss.SwissState) =
      let maxPersistedOpenings = 50
      let trimOrder (order: ResizeArray<int>) =
        if obj.ReferenceEquals(order, null) then
          order
        elif order.Count > maxPersistedOpenings then
          ResizeArray<int>(order |> Seq.take maxPersistedOpenings)
        else
          order
      let trimmedRounds =
        state.Rounds
        |> Seq.map (fun r ->
            let trimmedPairings =
              r.Pairings
              |> Seq.map (fun p ->
                  { p with OpeningOrder = trimOrder p.OpeningOrder })
              |> ResizeArray
            { r with Pairings = trimmedPairings })
        |> ResizeArray
      let trimmedState =
        { state with
            GlobalOpeningOrder = trimOrder state.GlobalOpeningOrder
            Rounds = trimmedRounds }
      agent.PostAndReply(fun reply -> WriteSwissState(trimmedState, reply))
      callback SwissStateUpdated

    let gamesPerMatchForRound _ =
      let gamesPerMatch = tourny.SwissOptions.GamesPerMatch
      if gamesPerMatch < 2 then 2
      elif gamesPerMatch % 2 = 1 then gamesPerMatch + 1
      else gamesPerMatch

    let mutable gameNr = 0
    logger.LogInformation("Swiss tournament about to start")
    let numberOfPlayers = tourny.EngineSetup.Engines.Length
    if numberOfPlayers % 2 = 1 then
      logger.LogInformation("Swiss tournament has an odd number of players; a bye will be assigned each round.")

    let mutable epdBook = false
    let board = Board()
    board.LoadFen Chess.startPos
    let mutable results = List.empty<Result>
    let games =
      match tourny.Opening.OpeningsPath with
      | Some path ->
          if path.ToLower().Contains ".epd" then
            epdBook <- true
            EPDExtractor.parseEPDFile path |> Seq.toArray
          else
            FullPGNParser.parsePgnFile path |> Seq.toArray
      | _ ->
          [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]

    let openings = games |> Seq.toList
    if openings.IsEmpty then
      logger.LogError("No openings available for Swiss tournament")
      failwith "No openings available for Swiss tournament."

    let gamesAlreadyPlayed =
      let fileExists = File.Exists tourny.PgnOutPath
      if fileExists then
        let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
        parsed |> Array.iter Hash.writeOpeningHashToPgnGame
        parsed
      else
        [||]

    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]

    let pgnGameWriterAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
    let replayList = ResizeArray<GameReplay>()
    let replayDicts =
      [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList
    let getReplayDictForPlayer (name:string) = replayDicts.[name]

    let swissPath = resolveSwissPath ()
    let swissAgent = startSwissStateReaderWriter swissPath
    let mutable openingIndex = 0
    let mutable pairId = 1

    let ensurePairingOpeningOrder (pairing: TypesDef.Swiss.SwissPairing) =
      if obj.ReferenceEquals(pairing.OpeningOrder, null) then
        pairing.OpeningOrder <- ResizeArray<int>()

    let ensureGlobalOpeningOrder (state: TypesDef.Swiss.SwissState) =
      if obj.ReferenceEquals(state.GlobalOpeningOrder, null) then
        state.GlobalOpeningOrder <- ResizeArray<int>()

    let mutable state : TypesDef.Swiss.SwissState =
      match swissAgent.PostAndReply(fun reply -> ReadSwissState reply) with
      | Some loaded ->
          ensureGlobalOpeningOrder loaded
          for round in loaded.Rounds do
            for pairing in round.Pairings do
              ensurePairingOpeningOrder pairing
          openingIndex <- loaded.NextOpeningIndex
          pairId <- loaded.Rounds |> Seq.collect (fun r -> r.Pairings |> Seq.map (fun p -> p.PairId)) |> Seq.append [0] |> Seq.max |> (+) 1
          loaded
      | None ->
          { TournamentName = tourny.Name
            SeedGroupCount = tourny.SwissOptions.SeedGroupCount
            GamesPerMatch = tourny.SwissOptions.GamesPerMatch
            UniqueOpeningsGlobal = not tourny.SwissOptions.UniquePerMatchOnly
            NextOpeningIndex = openingIndex
            GlobalOpeningOrder = ResizeArray<int>()
            Rounds = ResizeArray<TypesDef.Swiss.SwissRound>()
            UpdatedUtc = DateTime.UtcNow }

    let maxRounds = Math.Max(1, numberOfPlayers - 1)
    let configuredRounds = if tourny.SwissOptions.Rounds > 0 then tourny.SwissOptions.Rounds else tourny.Rounds
    let totalRounds = min configuredRounds maxRounds
    let maxPairs = (numberOfPlayers * (numberOfPlayers - 1)) / 2
    if configuredRounds > maxRounds then
      logger.LogInformation("Swiss rounds capped to {MaxRounds} based on {Players} players.", maxRounds, numberOfPlayers)
    let totalGames =
      [ 1 .. totalRounds ]
      |> List.sumBy (fun roundNumber ->
          let matchCount = numberOfPlayers / 2
          matchCount * gamesPerMatchForRound roundNumber)
    tourny.TotalGames <- totalGames
    callback (Update.TotalNumberOfPairs totalGames)
    let (tTime, gTime) = estimateTournamentAndGameTime totalGames tourny []
    let startInfo = { NumberOfGames = totalGames; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny }
    callback (Update.StartOfTournament startInfo)

    let seedOrder = Utilities.PairingHelper.tcecSeedOrder tourny.EngineSetup.Engines tourny.SwissOptions.SeedGroupCount
    let seedMap =
      seedOrder
      |> List.mapi (fun idx p -> p.Name, idx + 1)
      |> Map.ofList

    let buildScores () =
      let scores = Dictionary<string, float>(StringComparer.OrdinalIgnoreCase)
      for p in tourny.EngineSetup.Engines do
        scores.[p.Name] <- 0.0
      for round in state.Rounds do
        for pairing in round.Pairings do
          if scores.ContainsKey pairing.PlayerA then
            scores.[pairing.PlayerA] <- scores.[pairing.PlayerA] + pairing.ScoreA
          if scores.ContainsKey pairing.PlayerB then
            scores.[pairing.PlayerB] <- scores.[pairing.PlayerB] + pairing.ScoreB
      scores |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

    let buildPriorPairs () =
      let pairs = ResizeArray<string>()
      for round in state.Rounds do
        for pairing in round.Pairings do
          if String.IsNullOrWhiteSpace pairing.PlayerA |> not
             && String.IsNullOrWhiteSpace pairing.PlayerB |> not
             && pairing.PlayerB <> "BYE" then
            pairs.Add(Utilities.PairingHelper.swissPairKey pairing.PlayerA pairing.PlayerB)
      pairs |> Set.ofSeq

    let buildByeSet () =
      state.Rounds
      |> Seq.collect (fun r -> r.Pairings)
      |> Seq.filter (fun p -> p.PlayerB = "BYE")
      |> Seq.map (fun p -> p.PlayerA)
      |> Set.ofSeq

    let ensureGlobalOrder () =
      if tourny.SwissOptions.RandomOpenings && openings.Length > 1 && not tourny.SwissOptions.UniquePerMatchOnly then
        if state.GlobalOpeningOrder.Count = 0 then
          let order = [0 .. openings.Length - 1]
          let shuffled = order |> List.toArray
          Random.Shuffle(shuffled)
          state.GlobalOpeningOrder <- ResizeArray<int>(shuffled)
          writeSwissState swissAgent state

    let globalOpenings =
      ensureGlobalOrder ()
      if state.GlobalOpeningOrder.Count > 0 then
        state.GlobalOpeningOrder
        |> Seq.map (fun idx -> openings.[idx % openings.Length])
        |> Seq.toList
      else
        openings

    let getNextOpening (openingsList: PGNTypes.PgnGame list) (localIndex: int) =
      if tourny.SwissOptions.UniquePerMatchOnly then
        openingsList.[localIndex % openingsList.Length]
      else
        if openingIndex >= openingsList.Length then
          openingIndex <- 0
        let opening = openingsList.[openingIndex]
        openingIndex <- openingIndex + 1
        state.NextOpeningIndex <- openingIndex
        opening

    let getOpeningHash (opening: PGNTypes.PgnGame) = Hash.computeOpeningHashFromGame opening

    let searchReplayList (pairing : Pairing) =
      let nextGame = pairing
      let lastGame = gamesAlreadyPlayed |> Seq.tryLast
      let deviations = match lastGame with | Some g -> g.GameMetaData.Deviations | _ -> 0
      if deviations > tourny.DeviationCounter then
        tourny.DeviationCounter <- deviations
      prepareGameReplay nextGame replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960

    let sb = StringBuilder()
    let stateGamesPlayed =
      state.Rounds
      |> Seq.collect (fun r -> r.Pairings |> Seq.collect (fun p -> p.Games))
      |> Seq.length
    let gamesPlayedCount = if stateGamesPlayed > 0 then stateGamesPlayed else gamesAlreadyPlayed.Length
    tourny.CurrentGameNr <- gamesPlayedCount

    let playPairing (pair: Pairing) = async {
      if tourny.PreventMoveDeviation && not cts.Token.IsCancellationRequested then
        searchReplayList pair
      tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
      if cts.IsCancellationRequested then
        sb.Clear() |> ignore
        return Result.Empty
      else
        let limit = tourny.Opening.OpeningsPly
        let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
        let completeGame =
            openingMoves
            |> Seq.mapi(fun i m ->
                  if m.Color = "w" then
                    sprintf "%d. %s" m.MoveNumber m.San
                  else
                    sprintf "%s" m.San)
            |> String.concat " "

        logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
        board.ResetBoardState()
        if pair.Opening.Fen = "" then
          board.LoadFen Chess.startPos
          board.StartPosition <- Chess.startPos
        else
          board.LoadFen(pair.Opening.Fen)
          board.StartPosition <- pair.Opening.Fen

        if not epdBook then
          for m in openingMoves do
            board.PlayOpeningMove m.San
        else
          board.ResetBoardState()
          board.LoadFen pair.Opening.Fen
          board.StartPosition <- pair.Opening.Fen
          tourny.IsChess960 <- board.IsFRC

        let posWithMoves =
          let fen = board.StartPosition
          let start = $"position fen {fen} moves"
          board.LongSANMovesPlayed |> Seq.fold(fun state m ->
            sprintf "%s %s" state m) start
        logger.LogInformation("{position}", posWithMoves)

        let engine1 = EngineHelper.createEngine (pair.White, Some logger)
        let engine2 = EngineHelper.createEngine (pair.Black, Some logger)
        let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
        let roundTxt =
          if String.IsNullOrWhiteSpace pair.RoundNr then
            $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + 1}"
          else
            pair.RoundNr
        Update.RoundNr roundTxt |> callback
        let logException ex =
            let w = tourny.TimeControlTextForPlayer engine1.Config.TimeControlID
            let b = tourny.TimeControlTextForPlayer engine2.Config.TimeControlID
            let timeInfo = $"[{w}; {b}]"
            let createContext() = {
                EngineName   = engine1.Name
                OpponentName = engine2.Name
                GameNumber   = pair.GameNr
                MoveNumber   = board.MoveNumber()
                TimeControl  = timeInfo
                TimeRemaining= None
                PositionFen  = board.FEN()
                LastCommand  = None
                TimestampUtc = DateTime.UtcNow
                MoveHistory = board.GetMoveHistory()}
            EngineFailures.log logger ex (createContext())

        let result =
            let gametimer = Stopwatch.GetTimestamp()
            try
                if tourny.PreventMoveDeviation then
                  let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name
                  playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                else
                  if tourny.AllowPondering then
                    playWithPondering sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
                  else
                    play sb cts logger tourny board engine1 engine2 pair tryGetUserAdjudication callback |> Async.RunSynchronously
            with
            | :? EngineStartupException as ex ->
                  logException ex
                  handleGameException logger ex cts gametimer board engine1 engine2 pair
            | ex ->
                  logException ex
                  handleGameException logger ex cts gametimer board engine1 engine2 pair

        results <- result :: results

        let gameData : PGNTypes.GameMetadata =
          {
            OpeningHash = pair.OpeningHash
            Event = tourny.Description
            Site = tourny.Name
            Date = DateTime.Now.ToShortDateString()
            Round = roundTxt
            White = result.Player1
            Black = result.Player2
            Result = result.Result
            Reason = result.Reason
            GameTime = result.GameTime
            Moves = result.Moves
            Fen = pair.Opening.Fen
            OpeningName = pair.Opening.GameMetaData.OpeningName
            Deviations = tourny.DeviationCounter
            StartEvals = result.OutOfOpeningEvals
            OtherTags = pair.Opening.GameMetaData.OtherTags
          }

        if tourny.PreventMoveDeviation then
          replayList.Add
            {
              WhitePlayer = result.Player1
              BlackPlayer = result.Player2
              PGNMetaData = gameData
              LongSanMoves = board.LongSANMovesPlayed |> ResizeArray
            }

        let moveSection = sb.ToString()
        if result.Reason <> ResultReason.Cancel
          && not cts.IsCancellationRequested
          && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
          pgnGameWriterAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))

        if tourny.VerboseLogging then
          logger.LogInformation("Game metadata added to result: {pgnData}", gameData)

        if engine1.HasExited() |> not then
          engine1.StopProcess()
        if engine2.HasExited() |> not then
          engine2.StopProcess()

        board.ResetBoardState()
        gameNr <- gameNr + 1
        if gameNr % 2 = 0 then
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
        return result
    }

    let roundToStart =
      state.Rounds
      |> Seq.sortBy (fun r -> r.RoundNumber)
      |> Seq.tryFind (fun r -> r.Pairings |> Seq.exists (fun p -> not p.IsDecided))
      |> Option.map (fun r -> r.RoundNumber)
      |> Option.defaultValue (state.Rounds.Count + 1)

    let runRound (roundNumber: int) (roundPairs: (EngineConfig * EngineConfig) list) = async {
      let round : TypesDef.Swiss.SwissRound =
        match state.Rounds |> Seq.tryFind (fun r -> r.RoundNumber = roundNumber) with
        | Some existing -> existing
        | None ->
            let roundPairings = ResizeArray<TypesDef.Swiss.SwissPairing>()
            for (a, b) in roundPairs do
              let pairing : TypesDef.Swiss.SwissPairing =
                { PairId = pairId
                  RoundNumber = roundNumber
                  PlayerA = a.Name
                  PlayerB = b.Name
                  PlayerARating = a.Rating
                  PlayerBRating = b.Rating
                  ScoreA = if b.Name = "BYE" then 1.0 else 0.0
                  ScoreB = 0.0
                  IsDecided = b.Name = "BYE"
                  Games = ResizeArray<TypesDef.Swiss.SwissGame>()
                  OpeningOrder = ResizeArray<int>() }
              pairId <- pairId + 1
              roundPairings.Add pairing
            let newRound : TypesDef.Swiss.SwissRound =
              { RoundNumber = roundNumber
                Pairings = roundPairings }
            state.Rounds.Add newRound
            writeSwissState swissAgent state
            newRound

      let gamesPerMatch = gamesPerMatchForRound roundNumber
      let plannedPairings = ResizeArray<Pairing>()
      let mutable previewIndex = openingIndex
      for pairing in round.Pairings do
        if pairing.PlayerB = "BYE" then
          ()
        else
        ensurePairingOpeningOrder pairing
        let matchOpenings =
          if tourny.SwissOptions.UniquePerMatchOnly then
            if tourny.SwissOptions.RandomOpenings && openings.Length > 1 then
              if pairing.OpeningOrder.Count = 0 then
                let order = [0 .. openings.Length - 1]
                let shuffled = order |> List.toArray
                Random.Shuffle(shuffled)
                pairing.OpeningOrder <- ResizeArray<int>(shuffled)
                writeSwissState swissAgent state
              pairing.OpeningOrder
              |> Seq.map (fun idx -> openings.[idx % openings.Length])
              |> Seq.toList
            else
              openings
          else
            globalOpenings
        let seedA = seedMap.[pairing.PlayerA]
        let seedB = seedMap.[pairing.PlayerB]
        let firstWhite, firstBlack =
          if seedA <= seedB then
            pairing.PlayerA, pairing.PlayerB
          else
            pairing.PlayerB, pairing.PlayerA
        let whiteFirst = tourny.EngineSetup.Engines |> List.find (fun e -> e.Name = firstWhite)
        let blackFirst = tourny.EngineSetup.Engines |> List.find (fun e -> e.Name = firstBlack)
        if tourny.SwissOptions.UniquePerMatchOnly then
          Utilities.PairingHelper.addPlannedPairings plannedPairings whiteFirst blackFirst matchOpenings gamesPerMatch 0
          |> ignore
        else
          previewIndex <- Utilities.PairingHelper.addPlannedPairings plannedPairings whiteFirst blackFirst matchOpenings gamesPerMatch previewIndex
      callback (Update.PairingList plannedPairings)

      for pairIndex in 0 .. round.Pairings.Count - 1 do
        let pairing = round.Pairings.[pairIndex]
        if pairing.PlayerB = "BYE" then
          if pairing.IsDecided |> not then
            pairing.ScoreA <- 1.0
            pairing.ScoreB <- 0.0
            pairing.IsDecided <- true
            writeSwissState swissAgent state
        else
        let pairingPlayers =
          tourny.EngineSetup.Engines
          |> List.filter (fun e -> e.Name = pairing.PlayerA || e.Name = pairing.PlayerB)
        if pairingPlayers.Length = 2 then
          let playerA = pairingPlayers |> List.find (fun e -> e.Name = pairing.PlayerA)
          let playerB = pairingPlayers |> List.find (fun e -> e.Name = pairing.PlayerB)
          let matchOpenings =
            if tourny.SwissOptions.UniquePerMatchOnly then
              if tourny.SwissOptions.RandomOpenings && openings.Length > 1 then
                if pairing.OpeningOrder.Count = 0 then
                  let order = [0 .. openings.Length - 1]
                  let shuffled = order |> List.toArray
                  Random.Shuffle(shuffled)
                  pairing.OpeningOrder <- ResizeArray<int>(shuffled)
                  writeSwissState swissAgent state
                pairing.OpeningOrder
                |> Seq.map (fun idx -> openings.[idx % openings.Length])
                |> Seq.toList
              else
                openings
            else
              globalOpenings
          let seedA = seedMap.[pairing.PlayerA]
          let seedB = seedMap.[pairing.PlayerB]
          let firstWhite, firstBlack =
            if seedA <= seedB then
              playerA, playerB
            else
              playerB, playerA
          let tryGetOpeningByHash (hash: string) =
            match matchOpenings |> List.tryFind (fun o -> getOpeningHash o = hash) with
            | Some opening -> opening
            | None ->
                if tourny.SwissOptions.UniquePerMatchOnly then
                  matchOpenings.[(pairing.Games.Count / 2) % matchOpenings.Length]
                else
                  matchOpenings.[openingIndex % matchOpenings.Length]
          let nextOpening (localIndex: int ref) =
            if tourny.SwissOptions.UniquePerMatchOnly then
              let opening = matchOpenings.[(!localIndex) % matchOpenings.Length]
              localIndex := !localIndex + 1
              opening
            else
              if openingIndex >= matchOpenings.Length then
                openingIndex <- 0
              let opening = matchOpenings.[openingIndex]
              openingIndex <- openingIndex + 1
              state.NextOpeningIndex <- openingIndex
              opening
          let mutable gamesRemaining = Math.Max(0, gamesPerMatch - pairing.Games.Count)
          let localOpeningIndex = ref (pairing.Games.Count / 2)
          while gamesRemaining > 0 && not cts.IsCancellationRequested do
            let hasOddGame = pairing.Games.Count % 2 = 1
            let opening =
              if hasOddGame then
                let lastGame = pairing.Games.[pairing.Games.Count - 1]
                tryGetOpeningByHash lastGame.OpeningHash
              else
                nextOpening localOpeningIndex
            let openingHash = getOpeningHash opening
            let playOrder =
              if hasOddGame then
                let lastGame = pairing.Games.[pairing.Games.Count - 1]
                if lastGame.White = playerA.Name then
                  [ (playerB, playerA) ]
                else
                  [ (playerA, playerB) ]
              else
                [ (firstWhite, firstBlack); (firstBlack, firstWhite) ]
            for (white, black) in playOrder do
              if gamesRemaining = 0 || cts.IsCancellationRequested then
                ()
              else
                let roundGameNumber = (pairIndex * gamesPerMatch) + pairing.Games.Count + 1
                let pairingGame =
                  { Opening = opening
                    White = white
                    Black = black
                    GameNr = 0
                    RoundNr = $"{pairing.RoundNumber}.{roundGameNumber}"
                    OpeningHash = openingHash }
                let! result = playPairing pairingGame
                if result.Reason <> ResultReason.Cancel then
                  let game : TypesDef.Swiss.SwissGame =
                    { GameNr = gameNr
                      White = white.Name
                      Black = black.Name
                      OpeningId = opening.GameNumber.ToString()
                      OpeningHash = openingHash
                      Result = result.Result }
                  pairing.Games.Add game
                  let scoreWhite =
                    match result.Result with
                    | "1-0" -> 1.0
                    | "0-1" -> 0.0
                    | "1/2-1/2" -> 0.5
                    | _ -> 0.0
                  let scoreBlack =
                    match result.Result with
                    | "1-0" -> 0.0
                    | "0-1" -> 1.0
                    | "1/2-1/2" -> 0.5
                    | _ -> 0.0
                  if white.Name = pairing.PlayerA then
                    pairing.ScoreA <- pairing.ScoreA + scoreWhite
                    pairing.ScoreB <- pairing.ScoreB + scoreBlack
                  else
                    pairing.ScoreA <- pairing.ScoreA + scoreBlack
                    pairing.ScoreB <- pairing.ScoreB + scoreWhite
                  gamesRemaining <- gamesRemaining - 1
                  if pairing.Games.Count >= gamesPerMatch then
                    pairing.IsDecided <- true
                  writeSwissState swissAgent state
            if hasOddGame && tourny.SwissOptions.UniquePerMatchOnly then
              localOpeningIndex := !localOpeningIndex + 1
            if gamesRemaining > 0 && not cts.IsCancellationRequested then
              do! Async.Sleep(tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int)
    }

    let mutable roundNumber = roundToStart
    let mutable continueRounds = true
    while continueRounds && roundNumber <= totalRounds && not cts.IsCancellationRequested do
      let scores = buildScores ()
      let priorPairs = buildPriorPairs ()
      if priorPairs.Count >= maxPairs then
        logger.LogInformation("Swiss tournament completed: all unique pairs have been played.")
        continueRounds <- false
      else
        let byeSet = buildByeSet ()
        let roundPairs : (EngineConfig * EngineConfig) list =
          Utilities.PairingHelper.swissRoundPairings tourny.EngineSetup.Engines seedOrder scores priorPairs byeSet
        do! runRound roundNumber roundPairs
        roundNumber <- roundNumber + 1

    if tourny.SwissOptions.AllowExtraPairsOnTie && not cts.IsCancellationRequested then
      let rec resolveTieBreak tieRoundNumber = async {
        let scores = buildScores ()
        if scores.IsEmpty then
          return ()
        else
          let maxScore = scores |> Seq.maxBy (fun kvp -> kvp.Value) |> fun kvp -> kvp.Value
          let tied =
            scores
            |> Seq.filter (fun kvp -> kvp.Value = maxScore)
            |> Seq.map (fun kvp -> kvp.Key)
            |> Seq.toList
          if tied.Length <= 1 then
            return ()
          elif tied.Length % 2 = 1 then
            logger.LogWarning("Swiss tiebreak requires an even number of tied players. Skipping extra pairs.")
            return ()
          else
            let tiedPlayers =
              tourny.EngineSetup.Engines
              |> List.filter (fun p -> tied |> List.contains p.Name)
            logger.LogInformation("Swiss tiebreak: {Count} players tied at {Score}, playing extra pairs.", tiedPlayers.Length, maxScore)
            let byeSet = buildByeSet ()
            let roundPairs : (EngineConfig * EngineConfig) list =
              Utilities.PairingHelper.swissRoundPairings tiedPlayers seedOrder scores Set.empty byeSet
            do! runRound tieRoundNumber roundPairs
            return! resolveTieBreak (tieRoundNumber + 1)
      }
      do! resolveTieBreak roundNumber

    let res = ResizeArray<Result>(results)
    callback (Update.PeriodicResults res)
    pgnGameWriterAgent.Post(Parser.FullPGNParser.Dispose)
    pgnGameWriterAgent.Dispose()
    swissAgent.Post DisposeSwissState
    return results
  }
  
  let parallelTournamentRunBackup (logger: ILogger) (tourny: Tournament) callback (cts: CancellationTokenSource) = async {
    let mutable counter = 0
    let mutable gameNr = 0
    logger.LogInformation($"Tournament in parallel run about to start")
    let board = Board()
    board.LoadFen Chess.startPos
    let results = ResizeArray<Result>()
    let mutable epdBook = false
    let games = 
      match tourny.Opening.OpeningsPath with
      |Some path -> 
        if File.Exists path |> not then
          if tourny.VerboseLogging then
            logger.LogError($"Opening file {path} does not exist")
          [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
        elif path.ToLower().Contains ".epd" then
          epdBook <- true
          let all = EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray 
          all
        else
          let all = FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
          if tourny.VerboseLogging then
            logger.LogInformation $"Total number of openings in PGN = {all.Length}"
          all
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
    let gamesAlreadyPlayed = 
      let fileExists = File.Exists tourny.PgnOutPath      
      if fileExists then
          let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
          parsed |> Array.iter Hash.writeOpeningHashToPgnGame
          parsed
      else
        [||]    
    
    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]
    let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
    let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
    let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    gameNr <- gamesAlreadyPlayed.Length
    
    let allPairings = 
      if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) then
        if tourny.Opening.OpeningsTwice then
          PairingHelper.gauntletDoubleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
        else
          PairingHelper.gauntletSingleRound tourny.PreventMoveDeviation challengers rest gamesToPlay 
      else
        if tourny.Opening.OpeningsTwice then
          PairingHelper.generateAllRoundRobinDoubleRounds tourny.EngineSetup.Engines gamesToPlay
        else
          PairingHelper.generateAllRoundRobinSingleRounds tourny.EngineSetup.Engines gamesToPlay    
    let playedSet = Utilities.PairingHelper.playedSet gamesAlreadyPlayed
    let gamesLeftToPlay = 
      [
        for p in allPairings do
        if Utilities.PairingHelper.hasPlayedBefore p playedSet |> not then
          yield p
      ]

    if tourny.VerboseLogging then
      PairingHelper.printAllOpeningPairs logger gamesLeftToPlay
    let totalGames = allPairings.Length
    tourny.TotalGames <- totalGames
    
    let numberOfGamesPlayed = gamesAlreadyPlayed.Length
    if gamesLeftToPlay.Length = 0 then      
      return results |> Seq.toList    
    
    else
      let pgnGameWriterAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath
      callback (Update.TotalNumberOfPairs allPairings.Length) 
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      let lastGame = gamesAlreadyPlayed |> Seq.tryLast
      let pairings = 
        match lastGame, allPairings.Length > numberOfGamesPlayed with
        |Some last, true ->               
            if last.Mainline.Count > 0 then
              let lastMove = last.Mainline |> Seq.last
              if lastMove.Comment.Contains "cancelled" || lastMove.Comment.Contains "cancelled" then
                allPairings |> List.skip (numberOfGamesPlayed - 1)
              else
                allPairings |> List.skip numberOfGamesPlayed
            else
              allPairings        
        |_ -> allPairings
    
      tourny.CurrentGameNr <- numberOfGamesPlayed
      let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny pairings
      let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}      
      callback (Update.StartOfTournament startInfo)
      
      let nGamesInParallel = max 1 tourny.TestOptions.NumberOfGamesInParallelConsoleOnly
      let replayList = ResizeArray<GameReplay>()
      let replayDicts = 
        [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList      

      let searchReplayList (pairing : Pairing) =        
        let nextGame = pairing
        let lastGame = gamesAlreadyPlayed |> Seq.tryLast
        let deviations = match lastGame with |Some g -> g.GameMetaData.Deviations |_ -> 0
        if deviations > tourny.DeviationCounter then
          tourny.DeviationCounter <- deviations
        prepareGameReplay nextGame replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960
      
      let allEngines =
        tourny.EngineSetup.Engines |> List.map(fun e -> EngineHelper.createEngine (e, Some logger))
      
      let getPairsForTwoEnginesOnlyForParallelRun (engine1:string) (engine2 : string) =          
          [
            yield! gamesLeftToPlay |> List.filter(fun p -> p.White.Name = engine1 && p.Black.Name = engine2 )
            yield! gamesLeftToPlay |> List.filter(fun p -> p.White.Name = engine2 && p.Black.Name = engine1 )              
          ] |> List.sortBy(fun p -> p.Opening.GameNumber)

      let getAllPairsCombinationsFromPairings =
        [
          let mutable played = HashSet<string * string>()
          for engine1 in allEngines do
            for engine2 in allEngines do
              if engine1.Name <> engine2.Name then
                if played.Contains(engine1.Name, engine2.Name) |> not then                  
                  let pairs = getPairsForTwoEnginesOnlyForParallelRun engine1.Name engine2.Name                  
                  played.Add(engine1.Name, engine2.Name) |> ignore
                  played.Add(engine2.Name, engine1.Name) |> ignore                
                  yield engine1, engine2, pairs                
        ] |> List.filter(fun (_,_,p) -> p.Length > 0)
        
      let initializePairForParalellRun (e1:ChessEngine, e2:ChessEngine) (n:int) =
        [
          for _ = 1 to n do
            let eng1 = EngineHelper.createEngine (e1.Config, Some logger)
            let eng2 = EngineHelper.createEngine (e2.Config, Some logger)
            Initialization.initEngines 0 tourny eng1 eng2 logger
            yield eng1, eng2
        ]      
      
      let calculateMemUsage (engine1:ChessEngine) (engine2:ChessEngine) =
        let configs = [engine1.Config; engine2.Config]
        let footPrintBoth = HardwareInfo.sumFootprints configs         
        let totalAvail = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> uint64
        let maxMem = totalAvail / 2UL
        let concurrency = int (maxMem / footPrintBoth)
        concurrency      

      // Helper function to check the status of each engine and restart if necessary
      let engineHealthy (engine:ChessEngine) = task {
            try 
                // Check if engine has exited and try to restart
                if engine.HasExited() then
                    logger.LogWarning($"Engine {engine.Name} has exited, attempting restart")                        
                    try
                        engine.StartProcess()
                        let ok = engine.WaitForReadyOk() // Wait for "readyok" response
                        if ok then
                            logger.LogCritical($"Successfully restarted engine {engine.Name}")
                            return true
                        else
                            logger.LogCritical($"Not able to restart engine {engine.Name}") 
                            return false
                    with
                    | ex ->                                
                        logger.LogCritical(ex, $"Exception restarting engine {engine.Name}")                            
                        return false
                else
                    return true                        
            with
            | ex ->
                logger.LogCritical(ex, $"Failed to get engine {engine.Name} restarted")
                return false
        }
      
      let parallelTasks = 
          let enginePairsToRun = getAllPairsCombinationsFromPairings
          seq {                      
                for (engine1, engine2, myPairs) in enginePairsToRun do
                  let footPrintMemory = calculateMemUsage engine1 engine2
                  let nGamesInParallel = min footPrintMemory nGamesInParallel
                  let engines = initializePairForParalellRun (engine1, engine2) nGamesInParallel
                  let chunks = myPairs |> List.chunkBySize nGamesInParallel
                  
                  for chunk in chunks do                    
                    yield
                      [
                      for idx, pair in chunk |> List.mapi(fun idx e -> idx, e) do                        
                        let currentBoard = Board()
                        let mutable engine1, engine2 = engines[idx]                      
                        if engine1.Name = pair.Black.Name then
                          let (eng1,eng2) = engine2, engine1
                          engine1 <- eng1
                          engine2 <- eng2
                        try
                            let wOk = engine1 |> engineHealthy |> Async.AwaitIAsyncResult |> Async.RunSynchronously
                            let bOk = engine2 |> engineHealthy |> Async.AwaitIAsyncResult |> Async.RunSynchronously
                            if wOk |> not || bOk |> not then                                
                                logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                                Exception("Unhealthy engine detected, potentially skipping game") |> raise
                        with
                        |ex ->
                            logger.LogCritical(ex, "Error checking engine health for {white} vs {black}", pair.White.Name, pair.Black.Name)
                            
                        match pair with
                        |_ when String.IsNullOrEmpty pair.Opening.Fen |> not ->
                            currentBoard.LoadFen(pair.Opening.Fen)
                            currentBoard.StartPosition <- pair.Opening.Fen
                            tourny.IsChess960 <- currentBoard.IsFRC
                        |_ -> 
                          currentBoard.LoadFen Chess.startPos
                        tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening          
                        if cts.IsCancellationRequested then
                          () //todo
                        else
                          let limit = tourny.Opening.OpeningsPly
                          let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
                          let completeGame =             
                              openingMoves 
                              |> Seq.mapi(fun i m ->                   
                                    if m.Color = "w" then
                                      sprintf "%d. %s" m.MoveNumber m.San
                                    else
                                      sprintf "%s" m.San)
                              |> String.concat " "
                          
                          if tourny.VerboseLogging then
                            logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
                
                          if pair.Opening.Fen = "" then
                            currentBoard.LoadFen Chess.startPos
                            board.StartPosition <- Chess.startPos
                          else 
                            currentBoard.LoadFen pair.Opening.Fen
                            currentBoard.StartPosition <- pair.Opening.Fen
                            tourny.IsChess960 <- currentBoard.IsFRC
                          
                          if not epdBook then
                            for m in openingMoves do
                              currentBoard.PlayOpeningMove m.San                              
            
                          let posWithMoves =
                            let fen = currentBoard.StartPosition
                            let start = $"position fen {fen} moves"
                            currentBoard.LongSANMovesPlayed |> Seq.fold(fun state m -> 
                              sprintf "%s %s" state m) start
                          if tourny.VerboseLogging then
                            logger.LogDebug("{position}", posWithMoves)
                     
                          async {
                              let sb = StringBuilder()
                              Interlocked.Increment(&gameNr) |> ignore
                              Update.RoundNr pair.RoundNr |> callback
                              let moreThanTwoPlayers = tourny.EngineSetup.EngineDefList.Length > 2
                              let! result =
                                  let gametimer = Stopwatch.GetTimestamp()
                                  async {
                                      try
                                        if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                            searchReplayList pair
                                            let whiteReplayDict = replayDicts.[pair.White.Name]
                                            let blackReplayDict = replayDicts.[pair.Black.Name]                                    
                                            return! playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard engine1 engine2 pair (fun () -> None) callback
                                        else
                                            return! play sb cts logger tourny currentBoard engine1 engine2 pair (fun () -> None) callback    
                                    
                                      with
                                      | ex -> return handleGameException logger ex cts gametimer board engine1 engine2 pair  }
                              
                              let gameData : PGNTypes.GameMetadata = 
                                {
                                  OpeningHash = pair.OpeningHash
                                  Event = tourny.Description
                                  Site = tourny.Name
                                  Date = DateTime.Now.ToShortDateString()
                                  Round = pair.RoundNr
                                  White = result.Player1
                                  Black = result.Player2
                                  Result = result.Result
                                  Reason = result.Reason
                                  GameTime = result.GameTime
                                  Moves = result.Moves
                                  Fen = pair.Opening.Fen
                                  OpeningName = pair.Opening.GameMetaData.OpeningName
                                  Deviations = tourny.DeviationCounter
                                  StartEvals = result.OutOfOpeningEvals
                                  OtherTags = pair.Opening.GameMetaData.OtherTags
                                }

                              let moveSection = sb.ToString()
                              if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                                pgnGameWriterAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))                                
                              if tourny.VerboseLogging then
                                logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
                              return result, pair
                              }
                      ] 
                    
                  try
                    for (e1,e2) in engines do
                      e1.StopProcess()
                      e2.StopProcess()
                  with e -> 
                      logger.LogError(e, "Unexpected error when trying to shutdown the engines")
          }
      
      for task in parallelTasks do
        let! partialRes = Async.Parallel task
        for (res,p) in partialRes do
          counter <- counter + 1
          results.Add res
          if counter % (nGamesInParallel * 5) = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res) 
      
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      let games = pgnGameWriterAgent.PostAndReply(fun reply -> Parser.FullPGNParser.GetPGNGames(reply))
      pgnGameWriterAgent.Post(Parser.FullPGNParser.Dispose)
      pgnGameWriterAgent.Dispose()
      if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not then        
          let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
          let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn" 
          let combined = Path.Combine(directory,path)
          Parser.PGNWriter.writeRawPgnGamesAdjustedToFile combined games
      return results |> Seq.toList 
      }

  let parallelTournamentRun
    (logger: ILogger)
    (tourny: Tournament)
    (callback: Update -> unit)
    (cts: CancellationTokenSource) = async {

        logger.LogInformation("Tournament in parallel run about to start")
        let mutable epdBook = false
        let games = 
            match tourny.Opening.OpeningsPath with
            |Some path -> 
            if File.Exists path |> not then
                if tourny.VerboseLogging then
                    logger.LogError($"Opening file {path} does not exist")
                [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
            elif path.ToLower().Contains ".epd" then
                epdBook <- true
                let all = EPDExtractor.parseEPDFile path |> Seq.truncate tourny.Rounds |> Seq.toArray 
                all
            else
                let all = FullPGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
                if tourny.VerboseLogging then
                    logger.LogInformation $"Total number of openings in PGN = {all.Length}"
                all
            |_ ->
                [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
        let gamesAlreadyPlayed = 
            let fileExists = File.Exists tourny.PgnOutPath      
            if fileExists then
                let parsed = FullPGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
                parsed |> Array.iter Hash.writeOpeningHashToPgnGame
                parsed
            else
                [||]    
    
        let referencGamesPlayed =
            let fileExists = File.Exists tourny.ReferencePGNPath
            if fileExists then
                FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
            else
                [||]
        let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
        let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
        let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    
        let allPairings = 
            if tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase) then
                if tourny.Opening.OpeningsTwice then
                    PairingHelper.gauntletDoubleRound tourny.PreventMoveDeviation challengers rest gamesToPlay
                else
                    PairingHelper.gauntletSingleRound tourny.PreventMoveDeviation challengers rest gamesToPlay 
            else
                if tourny.Opening.OpeningsTwice then
                    PairingHelper.generateAllRoundRobinDoubleRounds tourny.EngineSetup.Engines gamesToPlay
                else
                    PairingHelper.generateAllRoundRobinSingleRounds tourny.EngineSetup.Engines gamesToPlay    
        let playedSet = Utilities.PairingHelper.playedSet gamesAlreadyPlayed
        let gamesLeftToPlay = 
            [
            for p in allPairings do
            if Utilities.PairingHelper.hasPlayedBefore p playedSet |> not then
                yield p
            ]

        if tourny.VerboseLogging then
            PairingHelper.printAllOpeningPairs logger gamesLeftToPlay
        
        let totalGames = allPairings.Length
        tourny.TotalGames <- totalGames    
        let numberOfGamesPlayed = gamesAlreadyPlayed.Length
        tourny.CurrentGameNr <- numberOfGamesPlayed
        
        let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
        let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}      
        callback (Update.StartOfTournament startInfo)
        
        let replayList = ResizeArray<GameReplay>()
        let replayDicts = 
            [ for eng in tourny.EngineSetup.Engines -> eng.Name, ReferenceGameReplay()] |> Map.ofList      

        let searchReplayList (pairing : Pairing) =        
            let nextGame = pairing
            let lastGame = gamesAlreadyPlayed |> Seq.tryLast
            let deviations = match lastGame with |Some g -> g.GameMetaData.Deviations |_ -> 0
            if deviations > tourny.DeviationCounter then
                tourny.DeviationCounter <- deviations
            prepareGameReplay nextGame replayDicts replayList referencGamesPlayed gamesAlreadyPlayed tourny.IsChess960

        let concurrency = 
            HardwareInfo.concurrencyLevel 
                tourny.EngineSetup.Engines 
                tourny.TestOptions.NumberOfGamesInParallelConsoleOnly
                
        if concurrency < 1 then
            printfn "Concurrency level is less than 1, using backup plan"
            let! results = parallelTournamentRunBackup logger tourny callback cts
            return results
        else
            // 1) build pairing channel
            let pairingCh = Channel.CreateUnbounded<Pairing>()
            for pair in gamesLeftToPlay do 
                pairingCh.Writer.TryWrite(pair) |> ignore
            pairingCh.Writer.Complete()

            // 2) build one engine‐pool channel per engine‐name, capacity = parallelism
            let enginePools =
                tourny.EngineSetup.Engines
                |> List.toArray
                |> Array.Parallel.map (fun e ->
                    let ch = Channel.CreateBounded<ChessEngine>(concurrency)
                    // pre-spawn p instances
                    let engines = 
                        [| 1..concurrency |]
                        |> Array.Parallel.map (fun _ ->
                            let eng = EngineHelper.createEngine (e, Some logger)
                            EngineHelper.initEngine 0 eng
                            ch.Writer.TryWrite(eng) |> ignore
                            eng.Name, ch )
                    engines )
                |> Array.concat
                |> Map.ofArray

            // 3) PGN writer agent stays the same
            let pgnAgent = Parser.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath

            // a thread‐safe result collector
            let results = System.Collections.Concurrent.ConcurrentBag<Result>()

            // Helper function to check the status of each engine and restart if necessary
            let engineHealthy (engine:ChessEngine) = task {
                try 
                    // Check if engine has exited and try to restart
                    if engine.HasExited() then
                        logger.LogWarning($"Engine {engine.Name} has exited, attempting restart")                        
                        try
                            engine.StartProcess()                            
                            let ok = engine.WaitForReadyOk() // Wait for "readyok" response
                            if ok then
                                logger.LogCritical($"Successfully restarted engine {engine.Name}")
                                return true
                            else
                                logger.LogCritical($"Not able to restart engine {engine.Name}") 
                                return false
                        with
                        | ex ->                                
                            logger.LogCritical(ex, $"Exception restarting engine {engine.Name}")                            
                            return false
                    else
                        return true                        
                with
                | ex ->
                    logger.LogCritical(ex, $"Failed to get engine {engine.Name} restarted")
                    return false
            }


            // 4) helper to play one pairing using borrowed engines
            let playOne (pair: Pairing) = task {
                // borrow
                let! wEng = enginePools.[pair.White.Name].Reader.ReadAsync()
                let! bEng = enginePools.[pair.Black.Name].Reader.ReadAsync()
                let! wOk = wEng |> engineHealthy
                let! bOk = bEng |> engineHealthy
                if wOk |> not || bOk |> not then
                    logger.LogCritical($"One of the engines is unhealthy, skipping game between {pair.White.Name} and {pair.Black.Name}")
                    Exception("Unhealthy engine detected, potentially skipping game") |> raise
                try
                    let! (res, pairing) = 
                        async {
                            let currentBoard = Board()
                            match pair with
                            |_ when String.IsNullOrEmpty pair.Opening.Fen |> not ->
                                currentBoard.LoadFen(pair.Opening.Fen)
                                currentBoard.StartPosition <- pair.Opening.Fen
                                tourny.IsChess960 <- currentBoard.IsFRC
                            |_ -> 
                              currentBoard.LoadFen Chess.startPos
                            tourny.OpeningName <- PGNHelper.getOpeningInfo pair.Opening
                            let limit = tourny.Opening.OpeningsPly
                            let openingMoves = pair.Opening.Mainline |> Seq.truncate(limit)
                            let completeGame =             
                              openingMoves 
                              |> Seq.mapi(fun i m ->                   
                                    if m.Color = "w" then
                                      sprintf "%d. %s" m.MoveNumber m.San
                                    else
                                      sprintf "%s" m.San)
                              |> String.concat " "
                            
                            if tourny.VerboseLogging then
                                logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
                
                            if pair.Opening.Fen = "" then
                                currentBoard.LoadFen Chess.startPos
                                currentBoard.StartPosition <- Chess.startPos
                            else 
                                currentBoard.LoadFen pair.Opening.Fen
                                currentBoard.StartPosition <- pair.Opening.Fen
                                tourny.IsChess960 <- currentBoard.IsFRC
                            let mutable moveIndex = 0
                            if not epdBook then
                                for m in openingMoves do
                                    currentBoard.PlayOpeningMove m.San                                    
            
                            let posWithMoves =
                                let fen = currentBoard.StartPosition
                                let start = $"position fen {fen} moves"
                                currentBoard.LongSANMovesPlayed 
                                |> Seq.fold(fun state m -> sprintf "%s %s" state m) start
                            
                            if tourny.VerboseLogging then
                                logger.LogDebug("{position}", posWithMoves)
                     
                          
                            let sb = StringBuilder()
                            Update.RoundNr pair.RoundNr |> callback
                            let moreThanTwoPlayers = tourny.EngineSetup.EngineDefList.Length > 2                            
                            
                            let! result =
                                let gametimer = Stopwatch.GetTimestamp()
                                async {
                                    try
                                        if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                            searchReplayList pair
                                            let whiteReplayDict = replayDicts.[pair.White.Name]
                                            let blackReplayDict = replayDicts.[pair.Black.Name]                                    
                                            return! playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) callback
                                        else
                                            return! play sb cts logger tourny currentBoard wEng bEng pair (fun () -> None) callback
                                    
                                    with
                                    | ex -> return handleGameException logger ex cts gametimer currentBoard wEng bEng pair  }
                              
                            let gameData : PGNTypes.GameMetadata = 
                                {
                                    OpeningHash = pair.OpeningHash
                                    Event = tourny.Description
                                    Site = tourny.Name
                                    Date = DateTime.Now.ToShortDateString()
                                    Round = pair.RoundNr
                                    White = result.Player1
                                    Black = result.Player2
                                    Result = result.Result
                                    Reason = result.Reason
                                    GameTime = result.GameTime
                                    Moves = result.Moves
                                    Fen = pair.Opening.Fen
                                    OpeningName = pair.Opening.GameMetaData.OpeningName
                                    Deviations = tourny.DeviationCounter
                                    StartEvals = result.OutOfOpeningEvals
                                    OtherTags = pair.Opening.GameMetaData.OtherTags
                                }

                            let moveSection = sb.ToString()
                            if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                                pgnAgent.Post (Parser.FullPGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))                                
                            if tourny.VerboseLogging then
                                logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
                            return result, pair
                        } |> Async.StartAsTask                    
                    results.Add res 

                finally
                    enginePools.[pair.White.Name].Writer.WriteAsync(wEng) |> ignore
                    enginePools.[pair.Black.Name].Writer.WriteAsync(bEng) |> ignore              
                }

            // 5) worker loop: pull pairings until done
            let mutable gameCounter = 0 // Counter to track the number of games processed
            let worker i = async {
                while pairingCh.Reader.WaitToReadAsync().Result do
                    match pairingCh.Reader.TryRead() with
                    | true, pair -> 
                        logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                        do! playOne pair |> Async.AwaitTask
                        Interlocked.Increment(&gameCounter) |> ignore // Increment the counter atomically
                        if gameCounter % 10 = 0 then
                            let res = ResizeArray<Result>(results) // Collect results
                            callback (Update.PeriodicResults res) // Call the callback every 10 gam
                    | _ -> ()
                }

            // 6) launch exactly p workers
            let! _ = 
                [| for i in 1..concurrency -> (worker i |> Async.StartAsTask) |]
                |> Task.WhenAll
                |> Async.AwaitTask

            // 7) teardown
            for KeyValue(e, ch) in enginePools do
                ch.Writer.Complete()
                let _ = 
                    [|1.. ch.Reader.Count|]
                    |> Array.Parallel.map (fun _ ->
                        let eng = enginePools.[e].Reader.ReadAsync().AsTask().Result
                        //let! eng = enginePools.[e].Reader.ReadAsync().AsTask() |> Async.AwaitTask
                        eng.StopProcess())
                printfn $"Engine {e} stopped"              
            
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res)
            let games = pgnAgent.PostAndReply(fun reply -> Parser.FullPGNParser.GetPGNGames(reply))
            pgnAgent.Post(Parser.FullPGNParser.Dispose)
            pgnAgent.Dispose()
            if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not then        
                let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
                let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn" 
                let combined = Path.Combine(directory,path)
                Parser.PGNWriter.writeRawPgnGamesAdjustedToFile combined games        
            // return immutable list of results
            return results |> Seq.toList
    }
                

module Manager =  

  let mutable cupResumeRequested = false
  let setCupResumeRequested value =
    cupResumeRequested <- value
  let consumeCupResumeRequested () =
    let value = cupResumeRequested
    cupResumeRequested <- false
    value
  let mutable cupBracketPathOverride : string option = None
  let setCupBracketPathOverride (path: string) =
    if String.IsNullOrWhiteSpace path then
      cupBracketPathOverride <- None
    else
      cupBracketPathOverride <- Some path
  let consumeCupBracketPathOverride () =
    let value = cupBracketPathOverride
    cupBracketPathOverride <- None
    value

  let loadTournament () =
    try 
        let path = DirectoryInfo(Environment.CurrentDirectory).FullName //.Parent.Parent.FullName
        let pathToTournamentJson = Path.Combine(path,"wwwroot","tournament.json")
        let tournyFromJson = Utilities.JSON.readTournamentJson pathToTournamentJson
    
        let tournament = 
          match tournyFromJson with
          |Some tourny ->
            let tourny =           
              if tourny.EngineSetup.EngineDefList.Length > 0 then
                let engineList = Utilities.JSON.readEngineDefs tourny.EngineSetup.EngineDefFolder tourny.EngineSetup.EngineDefList            
                let isGauntlet = tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
                if isGauntlet && tourny.Challengers > 0 then
                  for engine in engineList |> List.truncate tourny.Challengers do
                    engine.IsChallenger <- true
                else
                  for engine in engineList do
                    engine.IsChallenger <- false
                let engineSetup = {tourny.EngineSetup with Engines = engineList}
                let cupDefaults = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
                let cupOptions = if obj.ReferenceEquals(tourny.CupOptions, null) then cupDefaults else tourny.CupOptions
                let tournamentMode =
                  if String.IsNullOrWhiteSpace tourny.TournamentMode then "RR" else tourny.TournamentMode
                let cupOptions =
                  match consumeCupBracketPathOverride () with
                  | Some overridePath -> { cupOptions with BracketPath = overridePath }
                  | None -> cupOptions
                let swissDefaults = { GamesPerMatch = 2; Rounds = tourny.Rounds; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
                let swissOptions =
                  let baseOptions = if obj.ReferenceEquals(tourny.SwissOptions, null) then swissDefaults else tourny.SwissOptions
                  let rounds = if baseOptions.Rounds > 0 then baseOptions.Rounds else tourny.Rounds
                  { baseOptions with Rounds = rounds }
                let updatedTourny = {tourny with EngineSetup = engineSetup; CupOptions = cupOptions; SwissOptions = swissOptions; TournamentMode = tournamentMode }
                Utilities.Validation.validateTournamentInput updatedTourny
                updatedTourny
              else 
                //let enginesTest = createEnginesFromFolder "C:/Dev/Chess/Networks/CeresLatest" |> Seq.toList
                //(enginesTest |> List.head).IsChallenger <- true
                //let engineSetup = {tourny.EngineSetup with Engines = enginesTest}
                let cupDefaults = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
                let cupOptions = if obj.ReferenceEquals(tourny.CupOptions, null) then cupDefaults else tourny.CupOptions
                let tournamentMode =
                  if String.IsNullOrWhiteSpace tourny.TournamentMode then "RR" else tourny.TournamentMode
                let cupOptions =
                  match consumeCupBracketPathOverride () with
                  | Some overridePath -> { cupOptions with BracketPath = overridePath }
                  | None -> cupOptions
                let swissDefaults = { GamesPerMatch = 2; Rounds = tourny.Rounds; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
                let swissOptions =
                  let baseOptions = if obj.ReferenceEquals(tourny.SwissOptions, null) then swissDefaults else tourny.SwissOptions
                  let rounds = if baseOptions.Rounds > 0 then baseOptions.Rounds else tourny.Rounds
                  { baseOptions with Rounds = rounds }
                { tourny with CupOptions = cupOptions; SwissOptions = swissOptions; TournamentMode = tournamentMode }
        
            let openingPath = 
              match tourny.Opening.OpeningsPath with
              |Some path -> 
                if String.IsNullOrEmpty path then None else Some path
              |_ -> None
            let tourny = {tourny with Opening = {tourny.Opening with OpeningsPath = openingPath}}
            if tourny.MinMoveTimeInMS = 0 then
              { tourny with MinMoveTimeInMS = 300 }
            else
              tourny          
          |_ -> 
            ConsoleUtils.printInColor ConsoleColor.Red "Tournament json file not found!"
            failwith "Tournament json file not found!"
        tournament
    with exn -> 
      ConsoleUtils.printInColor ConsoleColor.Red $"Error loading tournament.json: {exn.Message} - please check your engine.json files"
      Tournament.Empty        
  
  let startTournament
    (cts:CancellationTokenSource)
    (tournament : Tournament)
    (logger:ILogger)
    sendResponse
    consoleMode
    (tryGetUserAdjudication: unit -> Match.UserAdjudication option) =
      logger.LogInformation (tournament.Summary())
      let timer = Stopwatch()
      timer.Start()
      let tourny = 
        //let nodeLimit = tournament.EngineSetup.Engines |> List.map(fun e -> tournament.FindTimeControl e.TimeControlID) |> List.forall(fun e -> e.NodeLimit)
        if consoleMode then
          Match.parallelTournamentRun logger tournament sendResponse cts
        else
          let mode =
            if String.IsNullOrWhiteSpace tournament.TournamentMode then "RR"
            else tournament.TournamentMode
          let modeNormalized = mode.Trim().ToLowerInvariant()
          let seeding =
            match tournament.CupOptions.SeedingStrategy with
            | null -> Utilities.PairingHelper.CupSeedingStrategy.ByRating
            | s when s.Equals("random", StringComparison.OrdinalIgnoreCase) -> Utilities.PairingHelper.CupSeedingStrategy.Random
            | _ -> Utilities.PairingHelper.CupSeedingStrategy.ByRating
          match modeNormalized with
          | "gauntlet" ->
              Match.gauntlet logger tournament sendResponse cts tryGetUserAdjudication
          | "cup" ->
              let resumeRequested = consumeCupResumeRequested ()
              Match.cup seeding tournament.CupOptions.UniquePerMatchOnly resumeRequested logger tournament sendResponse cts tryGetUserAdjudication
          | "swiss" ->
              Match.swiss logger tournament sendResponse cts tryGetUserAdjudication
          | "rr" | "roundrobin" | "round-robin" | _ ->
              Match.roundRobin logger tournament sendResponse cts tryGetUserAdjudication            
      
      let mutable validationPassed = true
      //check for value head tests
      if tournament.TestOptions.ValueTest then
        //validate value tests
        for engineConfig in tournament.EngineSetup.Engines do
          let isLc0 = engineConfig.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
          match Analysis.PuzzleEngineAnalysis.getPuzzleValueEngine engineConfig with
          |Some engine -> 
            if isLc0 then
                let conf = engine.Config              
                tournament.EngineSetup.Engines <- tournament.EngineSetup.Engines |> List.map(fun e -> if e.Name = engineConfig.Name then conf else e)
            printfn "Value test engine found: %s" engine.Name
          |_ -> 
            validationPassed <- false //failwith "Value test engine not found"            
            if isLc0 then
              ConsoleUtils.printInColor ConsoleColor.Yellow $"The Lc0 binary used does not support value head testing: {engineConfig.Name}. Please make sure to use a binary that supports value head tests via an option called ValueOnly or by using command line argument valuehead for Lc0 rewrite"              
            else
              let msg = $"The engine {engineConfig.Name} does not support value head testing, only Lc0 and Ceres supports it currently"
              ConsoleUtils.printInColor ConsoleColor.Yellow msg
        if validationPassed then
          ConsoleUtils.printInColor ConsoleColor.Yellow "All engines passed validation for value head tests"
        else
          ConsoleUtils.printInColor ConsoleColor.Red "Validation failed for value head tests"
          
      if validationPassed then
        //run tournament
        let res = tourny |> Async.RunSynchronously
        sendResponse (Match.EndOfTournament tournament)      
        logger.LogInformation($"Elapsed tournament time in seconds: {(timer.ElapsedMilliseconds/1000L)}")
        res
      else
        logger.LogInformation("Tournament validation failed, please make sure that all engines in the tournament supports value head tests.")
        []
  
  type Runner (logger: ILogger, callback: Action<Match.Update>, reloadTournament:bool, consoleOnly : bool) =    
    let cts = new CancellationTokenSource()
    let userAdjudicationChannel = Channel.CreateUnbounded<Match.UserAdjudication>()
    let mutable tournament = if reloadTournament then loadTournament() else Tournament.Empty
    let mutable resultsFromPGN = ResizeArray<Result>()
    let mutable pgnReader = None
    let mutable consoleMode = consoleOnly
    let executablePath() = tournament.OrdoExePath

    let tryDequeueUserAdjudication () =
      let reader = userAdjudicationChannel.Reader
      let mutable last = None
      let mutable item = Unchecked.defaultof<Match.UserAdjudication>
      while reader.TryRead(&item) do
        last <- Some item
      last
    
    member val TotalGames = 0 with get, set
    member x.PgnReader
        with get() = 
          if String.IsNullOrWhiteSpace tournament.PgnOutPath then
            failwith "PgnOutPath is not set in tournament.json"
          match pgnReader with
          |None -> 
            Parser.FullPGNParser.startPgnGameReaderWriter tournament.PgnOutPath
          |Some pgnReader -> pgnReader
        and set(value) = pgnReader <- Some value     

    member x.SendResponse (update: Match.Update) =       
      // Raise the callback with a proper Update response
      match update with
      | Match.PeriodicResults results -> 
          try 
              let pgnGames = x.GetPGNGames()
              if pgnGames.Count > 0 then
                  let consoleResString, data, _, _= PGNCalculator.getEngineDataResults pgnGames
                  let ordoPath = executablePath()
                  if String.IsNullOrEmpty ordoPath |> not && tournament.ConsoleOnly then                
                      let cmd = Utilities.OrdoHelper.createOrdoCommand ordoPath tournament.PgnOutPath ""
                      let ordoCommandString = $"\n Ordo command: {cmd.Arguments} \n"
                      Console.WriteLine(ordoCommandString)
                      let ordo = Utilities.OrdoHelper.runCommandAsync cmd data |> Async.AwaitTask |> Async.RunSynchronously
                      let gameUpdate = Match.Update.GameSummary ordo
                      callback.Invoke gameUpdate
                  else
                      let gameUpdate = Match.Update.GameSummary consoleResString                  
                      callback.Invoke gameUpdate
              else
                let pRes = x.GetPlayerResults results
                let cross = x.GenerateStatsCrosstable results            
                let table = OrdoHelper.getResultsAndPairsInConsoleFormat pRes cross            
                callback.Invoke (Match.Update.GameSummary table)
          with e ->
              let msg = $"Error generating periodic results: {e.Message}"
              printfn "%s" msg
      |_ -> callback.Invoke update
        
    member _.AddTournament tourny = tournament <- tourny 

    member _.AdjudicateGame(gameNr: int, result: string) =
      let isValid =
        match result with
        | "1-0" | "0-1" | "1/2-1/2" -> true
        | _ -> false

      if isValid then
        userAdjudicationChannel.Writer.TryWrite({ GameNr = gameNr; Result = result }) |> ignore
      else
        logger.LogWarning("Invalid user adjudication result string: {Result}", result)

    member x.Run() = 
      try 
        // Ensure external shutdowns are translated to our CTS cancellation
        Console.CancelKeyPress.Add(fun args ->
            cts.Cancel()
            args.Cancel <- true
        )
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
            try cts.Cancel() with _ -> ()
        )
        resultsFromPGN <- x.GetResults() //x.GetFinalResults()
        startTournament cts tournament logger x.SendResponse consoleMode tryDequeueUserAdjudication
      with e -> 
        printfn "Error: %A" e
        logger.LogCritical ("failed to run tournament" + tournament.MinSummary())
        resultsFromPGN |> Seq.toList
        //raise e
    
    member _.LinkCancellation(token: CancellationToken) = token.Register(fun () -> cts.Cancel()) |> ignore
    member _.GetPlayerResults (results: ResizeArray<Result>) : ResizeArray<PlayerResult> =
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let isGauntlet = tournament.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
      PGNCalculator.getFullStat isGauntlet challengers players results

    member _.GetPlayerResultsFromPGN (results: ResizeArray<Result>) : seq<PlayerResult> =
      PGNCalculator.getFullStatFromResults results

    member _.Cancel() = cts.Cancel()

    member _.Tournament() =
      //check if tournament is empty and reload if necessary
      if tournament = Tournament.Empty then
        tournament <- loadTournament ()
      tournament

    member _.LayoutUpdated() =      
        let tourny = loadTournament ()
        tournament.LayoutOption <- tourny.LayoutOption
        tournament

    member val GetPiecesLeft = 0 with get, set

    member val Pairings  = ResizeArray<Pairing>() with get, set

    member x.GetGamesLeftToPlay() =
      let gamesAlreadyPlayed = x.GetPGNGames() |> Seq.toArray
      let playedSet = Utilities.PairingHelper.playedSet gamesAlreadyPlayed
      let gamesLeftToPlay = 
        [
          for p in x.Pairings do
          if Utilities.PairingHelper.hasPlayedBefore p playedSet |> not then
            yield p
        ]
      gamesLeftToPlay
    
    member x.GetAllPairings() =
      let gamesLeftToPlay = x.GetGamesLeftToPlay()
      if gamesLeftToPlay.Length = 0 then
        ResizeArray<_>()
      else        
        gamesLeftToPlay |> ResizeArray

    member x.GetLastestPairings() =      
      let gamesLeftToPlay = x.GetGamesLeftToPlay()
      if gamesLeftToPlay.Length = 0 then
        ResizeArray<_>()
      else
        x.TotalGames <- gamesLeftToPlay.Length + tournament.CurrentGameNr
        gamesLeftToPlay |> Seq.skip 1 |> Seq.truncate 20 |> ResizeArray

    member x.GetResults() : ResizeArray<Result> = 
      let fileExists = File.Exists tournament.PgnOutPath
      if fileExists then
        let results = x.PgnReader.PostAndReply(fun reply -> Parser.FullPGNParser.GetResults reply )
        results        
      else              
        ResizeArray<Result>()

    member x.GetPGNGames() : ResizeArray<PgnGame> = 
      let fileExists = File.Exists tournament.PgnOutPath
      if fileExists then
        let results = x.PgnReader.PostAndReply(fun reply -> Parser.FullPGNParser.GetPGNGames reply )
        // Always recompute opening hashes to keep resume logic compatible across versions.
        results |> Seq.iter Hash.writeOpeningHashToPgnGame
        results
      else              
        ResizeArray<PgnGame>()
 
    member _.GenerateCrosstableEntries (results: ResizeArray<Result>) =
      PGNCalculator.generateCrosstableEntries results
 
    member _.GenerateStatsCrosstable (results: ResizeArray<Result>) = 
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      PGNCalculator.generateSmallStatCrossTable results challengers players

    member _.GetGauntletCrosstable (results: ResizeArray<Result>) = 
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let challengers = 
        if tournament.EngineSetup.Engines.Length = 2 then
          players |> List.take 1
        else          
          tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      PGNCalculator.generateBigStatCrossTable results challengers players
