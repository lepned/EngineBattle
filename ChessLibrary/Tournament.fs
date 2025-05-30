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
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.Parser
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef.Engine
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Position
open ChessLibrary.TypesDef.TMove
open ChessLibrary.TypesDef.TimeControl
open ChessLibrary.TypesDef.Misc
open ChessLibrary.Chess
open ChessLibrary.Chess.BoardUtils
open ChessLibrary.Utilities
open ChessLibrary.LowLevelUtilities

module Initialization = 

  let waitForOpeningMovesPlayed (delay:int) =
    async { 
      do! Async.Sleep(delay) 
      return ""} 

  let waitForBetweenGames (delay:int) =
    async { 
      do! Async.Sleep(delay) 
      return ""} 

  let rec waitForEngineIsReady (tourny:Tournament) (engine: ChessEngine) =
    async {
      if engine.HasExited() then
        //printfn "Starting engine: %s %A" engine.Name DateTime.Now
        engine.StartProcess() |> ignore      
      engine.IsReady()
      let! line = engine.ReadLineAsync() |> Async.AwaitTask
      match line with
      | line when line.StartsWith("readyok") ->                    
          let engineOption : EngineOption = { Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
          engine.AddSetOption engineOption            
          if tourny.MoveOverhead.Ticks > 0 then
            let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
            engine.SetMoveOverhead("overhead", ms)
          engine.UciNewGame()
          return $"{engine.FullName} isready"
      | _ -> 
        return! waitForEngineIsReady tourny engine }

  let appendGameDescription (sb:StringBuilder) (tourny:Tournament) (player1:ChessEngine) (player2:ChessEngine) (openingMoves: ResizeArray<string>) fen =
    let append (txt:string) = sb.Append txt |> ignore
    let isEpd =
      match tourny.Opening.OpeningsPath with
      |Some path -> path.ToLower().Contains ".epd"
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
    if engine1.Config.ContemptEnabled then      
      let ratingDiff = engine1.Config.Rating - engine2.Config.Rating
      if ratingDiff > 0 || engine1.Config.NegativeContemptAllowed then
        let options = engine1.GetDefaultOptions()
        if options.ContainsKey "Contempt" then
          let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
          engine1.AddSetOption engineOption
          printfn "Contempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
        elif options.ContainsKey "DynamicContempt" then
          let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
          engine2.AddSetOption engineOption
          printfn "DynamicContempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
      else 
        printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine1.Name ratingDiff engine2.Name

    if engine2.Config.ContemptEnabled then      
      let ratingDiff = engine2.Config.Rating - engine1.Config.Rating
      if ratingDiff > 0 || engine2.Config.NegativeContemptAllowed then
        let options = engine2.GetDefaultOptions()
        if options.ContainsKey "Contempt" then
          let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
          engine2.AddSetOption engineOption
          printfn "Contempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
        elif options.ContainsKey "DynamicContempt" then
          let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
          engine2.AddSetOption engineOption
          printfn "DynamicContempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
      else 
        printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine2.Name ratingDiff engine1.Name

  let initEngines openingDelayMs (tourny:Tournament) (engine1: ChessEngine) (engine2: ChessEngine) =          
    async {
      let delay = 50
      let properStartup = engine1.HasExited() || engine2.HasExited() 
      if properStartup then
        let! res =
          [waitForEngineIsReady tourny engine1; waitForEngineIsReady tourny engine2; waitForOpeningMovesPlayed openingDelayMs; waitForBetweenGames delay]
          |> Async.Parallel          
        checkAndPrepareContempt engine1 engine2
        do! Async.Sleep 200
        //for e in res do
        //  if e <> "" then
        //    printfn "%s" e
      else
        checkAndPrepareContempt engine1 engine2
        if tourny.MoveOverhead.Ticks > 0 then
          let ms = tourny.MoveOverhead.ToTimeSpan().TotalMilliseconds |> int
          engine1.SetMoveOverhead("overhead", ms)
          engine2.SetMoveOverhead("overhead", ms)                  

        let engineOption : EngineOption = { Name = "UCI_Chess960"; Value = sprintf "%b" tourny.IsChess960 }
        engine1.AddSetOption engineOption
        engine2.AddSetOption engineOption
        engine1.UciNewGame()
        engine2.UciNewGame()
        let actDelay = max openingDelayMs delay
        do! Async.Sleep actDelay } 
    |> Async.RunSynchronously


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
        //if not openingPlayedBefore then
        //    //for dict in replayDicts do
        //    //    dict.Value.Clear()
        //        //printfn "Cleared replay dict for %s" dict.Key
        //        printfn "New opening is played!"
        //else 
        //    printfn "Opening already played before, so we keep the replay dicts"

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
          
            //what to do with previous games here?
            for g in previousGames do
                printfn "Previous game found from referencePGN: %s, %s for pairing %s, %s" g.GameMetaData.White g.GameMetaData.Black pairing.White.Name pairing.Black.Name
          
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
                for m in game.Moves do
                    if m.WhiteSan <> "" then
                        let hash = replayBoard.DeviationHash()
                        replayBoard.PlaySimpleShortSan m.WhiteSan
                        if replayBoard.LongSANMovesPlayed.Count > idx then
                            let lastmove = replayBoard.LongSANMovesPlayed[idx]
                            let data : ReplayData = {Engine=game.GameMetaData.White; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}
                            if isWhite then
                                replayDictWhite[hash] <- data                                
                            idx <- idx + 1
                    if m.BlackSan <> "" then
                        let hash = replayBoard.DeviationHash()
                        replayBoard.PlaySimpleShortSan m.BlackSan
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
      (board:Board)
      (evals: EvalType list) 
      (tourny: Tournament) 
      (player1: string) 
      (player2: string) 
      (playedLastMove: string)
      gametimer
      gameMoveList
      moves =
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
      let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
      
      if piecesLeft <= withTBadjudicationMen then        
        let tryProbe =
          let dir = tourny.Adjudication.TBAdj.TablebaseDirectory
          if String.IsNullOrEmpty(dir) |> not && Directory.Exists tourny.Adjudication.TBAdj.TablebaseDirectory then
            //check the results from a tablebase adjudication
            let fen = board.FEN()          
            let tableRes = FathomRunner.runFathom tourny.Adjudication.TBAdj.TablebaseDirectory fen          
            let tb = FathomRunner.parse tableRes
            match tb.Wdl with
            |Some w -> 
              let res = 
                match w with
                |"Win" -> if board.Position.STM = 0uy then "1-0" else "0-1"
                |"Draw" -> "1/2-1/2"
                |"Loss" -> if board.Position.STM = 0uy then "0-1" else "1-0"
                |_ -> "Tablebase result not found"
              //let stm = if board.Position.STM = 0uy then "white to move" else "black to move"
              //let dtz = match tb.Dtz with |Some d -> d |_ -> "DTZ not found"
              //printfn "Tablebase result: %s: %s %s - DTZ: %s W:%s, D:%s, L:%s" stm w res dtz (String.concat ", " tb.WinningMoves) (String.concat ", " tb.DrawingMoves) (String.concat ", " tb.LosingMoves)
              createResult player1 player2 gameMoveList res Misc.ResultReason.AdjudicateTB dur |> Some
            |_ -> None
          else None
        
        if tryProbe.IsSome then
          tryProbe
        
        elif tryProbe.IsNone && shouldAdjudicateTB evals piecesLeft withTBadjudicationMen then
            match evals.[0] with
            |EvalType.CP ev when ev > 5.0 ->
                createResult player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur |> Some
            |EvalType.CP ev when ev < -5.0 -> 
                createResult player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur |> Some
            |EvalType.CP _ -> 
                createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicateTB dur |> Some
            |EvalType.Mate m when m > 0 ->
                createResult player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur |> Some
            |EvalType.Mate m when m < 0 -> 
                createResult player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur |> Some
            |EvalType.Mate m -> //mate 0 or mate -0
                if m = -0 then 
                  createResult player1 player2 gameMoveList "0-1" Misc.ResultReason.AdjudicateTB dur |> Some
                else 
                  createResult player1 player2 gameMoveList "1-0" Misc.ResultReason.AdjudicateTB dur |> Some
            |NA -> failwith "eval not available error in adjudication rule"   
        else None
    
      elif moves >= (tourny.Adjudication.WinOption.MinWinMove * 2 + winPlyLength) && tooHighEvals() then
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let result = 
          match evals.[0] with
          |EvalType.CP ev when ev >= tourny.Adjudication.WinOption.MinWinScore -> "1-0"
          |EvalType.CP ev when ev <= -tourny.Adjudication.WinOption.MinWinScore -> "0-1"
          |EvalType.Mate m when m > 0 -> "1-0"       
          |EvalType.Mate m when m < 0 -> "0-1"
          |EvalType.Mate m -> //mate 0 or mate -0
              if m = -0 then "0-1" else "1-0"
          |EvalType.CP cp -> 
            printfn "Check adjudication for high evals - latest eval values: %f" cp
            failwith "Fail in adjudication win for high evals"
          |NA -> failwith "eval not available error in adjudication rule"        
         
        let res = createResult player1 player2 gameMoveList result Misc.ResultReason.AdjudicatedEvaluation dur 
        Some res     
  
      elif moves >= (tourny.Adjudication.DrawOption.MinDrawMove * 2 + drawPlyLength) && tooLowEvals() then
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let res = createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicatedEvaluation dur
            Some res

      elif board.InsufficentMaterial() then
            let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let res = createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.AdjudicateMaterial dur
            Some res

      elif board.ClaimThreeFoldRep () then
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let res = createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.Repetition dur
        Some res

      elif board.AnyLegalMove() |> not then
        let mutable mypos = board.Position
        let check = ChessLibrary.MoveGeneration.InCheck &mypos <> 0UL
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        if check then            
          if playedLastMove = player1 then 
            let res = createResult player1 player2 gameMoveList "1-0" Misc.ResultReason.Checkmate dur
            Some res
          else 
            let res = createResult player1 player2 gameMoveList "0-1" Misc.ResultReason.Checkmate dur          
            Some res
        else 
          let res = createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.Stalemate dur 
          Some res         

      elif board.Position.Count50 >= 100uy then
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let res = createResult player1 player2 gameMoveList "1/2-1/2" Misc.ResultReason.ExcessiveMoves dur
          Some res
      else
        None

    let isInMatePlayout fullEvalList =
      //if the two last elements in the evalList are mate values, then we are in a mate playout
      let lastTwo = fullEvalList |> Seq.truncate 2 |> Seq.toList
      match lastTwo with
      |[EvalType.Mate _; EvalType.Mate _] -> true
      |_ -> false

module TournamentUtils =
  
  let validateEnginesInTournament (tourny : Tournament)  =
    async {
      Utilities.Validation.validateTournamentInput tourny (tourny.EngineSetup.Engines)
      let mutable valid = tourny.EngineSetup.Engines.Length > 1
      for engConfig in tourny.EngineSetup.Engines do
        let engine = engConfig |> EngineHelper.createEngine
        engine.StartProcess()
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
  
  let bestQMove (nodes:int) (engine: ChessEngine) fenWithMoves (tboard:Board inref) =     
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
        let line = engine.ReadLine()
        //printfn "%s" line
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
      qList.Add (res,lSan,eval)
    let score, m, ev = qList |> Seq.minBy (fun (s,_,_) -> s)
    (-score, sprintf "bestmove %s" m, ev)

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
    | GameSummary of summary: string      
  
  let playGoValue
    (sb : StringBuilder)
    (cts : CancellationTokenSource)
    (logger : ILogger)
    (tourny : Tournament) 
    (board : Board)
    (player1 : ChessEngine) 
    (player2 : ChessEngine)
    callback  =
        
    sb.Clear() |> ignore 
    let append (txt:string) = sb.Append txt |> ignore
    if tourny.TestOptions.WriteToConsole then
      player1.ShowCommands()
      player2.ShowCommands()
    let stm = board.Position.STM
    let mutable pos = 0UL
    let mutable moves = 0
    let mutable numberOfNodes = 0L
    let mutable evalList : EvalType list = []
    let mutable fullEvalList :EvalType list = []
    let npsList = ResizeArray<float>()
    let gameMoveList = board.ShortSANMovesPlayed

    let msg = $"Initializing players: {player1.Name} vs {player2.Name} with delay: {tourny.DelayBetweenGames}" 
    logger.LogInformation msg
    tourny.CurrentGameNr <- tourny.CurrentGameNr + 1
    
    let gameStartInfo : StartGameInfo = 
      {
        WhitePlayer = player1.Config
        BlackPlayer = player2.Config
        StartPos = board.FEN() // board.PositionWithMoves()
        OpeningMovesAndFen = ResizeArray<MoveAndFen>(board.MovesAndFenPlayed)
        WhiteTime = TimeOnly.MinValue
        BlackTime = TimeOnly.MinValue
        WhiteToMove = stm = 0uy
        OpeningName = tourny.OpeningName
        CurrentGameNr = tourny.CurrentGameNr
      }
    board.MovesAndFenPlayed.Clear()
    let mutable moveInfoData = Engine.ChessMoveInfo.Empty
    
    callback(StartOfGame gameStartInfo)
    let nodeLimit = tourny.EngineSetup.Engines |> List.map(fun e -> tourny.FindTimeControl e.TimeControlID) |> List.forall(fun e -> e.NodeLimit)
    let neuralNetTest = tourny.TestOptions.PolicyTest || tourny.TestOptions.ValueTest || nodeLimit
    if neuralNetTest = false then     
      let timeCalc = float board.OpeningMovesPlayed.Count * 0.5
      let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
      Initialization.initEngines openingDelayMs tourny player1 player2
    
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
    let fen = board.FEN()
    if tourny.VerboseLogging then
      logger.LogDebug $"After opening moves, FEN={fen}"
    let tboard = Chess.Board()
    tboard.IsFRC <- tourny.IsChess960
    let rec playEngine (playing: ChessEngine) (opponent: ChessEngine) (position:uint64) = async {
      let mutable posWithMoves = ""
      if cts.IsCancellationRequested then
        let msg = $"Game between {player1.Name} vs {player2.Name} was cancelled"
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" Misc.ResultReason.Cancel dur
        logger.LogCritical(msg)
        callback(EndOfGame res)
        //callback(EndOfTournament tourny)
        return res
      else
        if position <> pos then            
          moveTimer <- Stopwatch.GetTimestamp()
          pos <- position          
          posWithMoves <- board.PositionWithMoves()          
          if tourny.VerboseLogging then
            logger.LogDebug $"Current position: {posWithMoves}"
          playing.Position posWithMoves          
          lastCheck <- int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        
        let q, line, eval = bestQMove 1 playing posWithMoves &tboard     
        if tourny.VerboseLogging then
          logger.LogDebug line  
        if String.IsNullOrEmpty line then
          logger.LogDebug $"Empty line or null from {playing.Name}"
        elif line.StartsWith "info engine" then
          Update.MessagesFromEngine ("Ceres", line) |> callback
          logger.LogInformation line
        if playing.HasExited() then
          //lost on disconnection/failure
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let resValue = if playing.Name = player1.Name then "0-1" else "1-0"
          let res = createResult player1.Name player2.Name gameMoveList resValue Misc.ResultReason.ForfeitLimits dur
          callback(EndOfGame res)
          logger.LogInformation($"Player has exited {playing.Name}")
          return res
          
        elif cts.IsCancellationRequested then
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" Misc.ResultReason.Cancel dur
          callback(EndOfGame res)
          logger.LogInformation($"Cancel requested when engine ready to play: {playing.Name}")
          return res
          
        else          
          if line.StartsWith("bestmove") then    
              let move, ponderMove = line.Split().[1], if line.Contains "ponder" then (line.Split().[3]) else ""
              match tryGetTMoveFromCoordinateNotation &board move with
              |Some tmove ->
                let mutable moveAdj = tmove
                let shortSan = getSanNotationFromTMove &board tmove
                board.LongSANMovesPlayed.Add(move)                
                gameMoveList.Add(shortSan)
                board.MakeMove(&moveAdj)
                let ponderSan = getShortSanFromLongSan &board ponderMove
                moveInfoData.pd <- ponderSan                
                moves <- moves + 1
                fullEvalList <- eval::fullEvalList
                let eval = 
                  if evalList.Length > 0 then evalList.[0] 
                  elif fullEvalList.Length > 0 then fullEvalList[0] 
                  else EvalType.CP 0.0
                let pv,pvLong = if playing.Name = player1.Name then Player1PV, PVLine1 else Player2PV, PVLine2
                let fen = BoardHelper.posToFen board.Position
                let moveDetail = 
                  {
                    LongSan = move
                    FromSq = move[0..1]
                    ToSq = move[2..3]
                    Color = if board.Position.STM = 8uy then "w" else "b"
                    IsCastling = (tmove.MoveType &&& TPieceType.CASTLE <> TPieceType.EMPTY) 
                    }                 
                let moveAndFen = {Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen}
                let mutable posToCheck = board.Position
                let piecesLeft = PositionOps.numberOfPieces &posToCheck
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
                    TimeLeft = TimeOnly.MinValue
                    MoveTime = TimeOnly.MinValue
                    NPS = 0.
                    Nodes = numberOfNodes
                    FEN = fen
                    PV = pv 
                    LongPV = pvLong
                    MoveAndFen = moveAndFen
                    MoveHistory = board.GetMoveHistory()
                    Move50 = board.Position.Count50 |> int
                    R3 = board.RepetitionNr()
                    PiecesLeft = piecesLeft
                    AdjDrawML = movesLeft
                    }                  
                    
                if bestMove.R3 > 1 && tourny.VerboseLogging then
                  logger.LogInformation($"Ply {board.PlyCount} - Repetition occured: {bestMove.R3} time(s)")
                callback(BestMove (bestMove, EngineStatus.Empty))
                evalList <- []
                depth <- 0
                selfdepth <- 0
                npsList.Clear()
                let numberAndMove = board.MoveNumberString shortSan
                annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append                            
                //tablebase adjudication here
                match Adjudication.adjudicateByEval board fullEvalList tourny player1.Name player2.Name playing.Name gametimer gameMoveList moves with
                |Some res -> 
                  callback(EndOfGame res)
                  let mutable posToCheck = board.Position
                  let piecesLeft = PositionOps.numberOfPieces &posToCheck
                  if tourny.VerboseLogging then
                    logger.LogInformation($"Adjudication by eval: {res.Reason}, Pieces left: {piecesLeft} ")
                    logger.LogInformation (sprintf "Info %A: " res)
                  return res
                |None ->
                  return! playEngine opponent playing (board.PositionHash())
              |_ ->  
                let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                //check if there is any legal move in the position
                if board.AnyLegalMove() |> not then
                  //either checkmate or stalemate
                  if board.IsMate() then                   
                    let res = 
                      if playing.Name = player1.Name then
                        createResult player1.Name player2.Name gameMoveList "0-1" Misc.ResultReason.Checkmate dur
                      else
                        createResult player1.Name player2.Name gameMoveList "1-0" Misc.ResultReason.Checkmate dur
                    callback(EndOfGame res)
                    logger.LogInformation($"Checkmate: {res.Reason}")
                    return res
                  else                    
                    let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" Misc.ResultReason.Stalemate dur
                    callback(EndOfGame res)
                    logger.LogInformation($"Stalemate: {res.Reason}")
                    return res
                else
                  let res = 
                    if playing.Name = player1.Name then
                      createResult player1.Name player2.Name gameMoveList "0-1" Misc.ResultReason.Illegal dur
                    else
                      createResult player1.Name player2.Name gameMoveList "1-0" Misc.ResultReason.Illegal dur
                  callback(EndOfGame res)
                  let fenAndMoves = board.PositionWithMoves()
                  logger.LogCritical($"{playing.Name} failed in bestmove logic with the following response {line} after these moves: \n{fenAndMoves}")
                  return res
                 
          else            
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

    let msg = $"Initializing players: {player1.Name} vs {player2.Name} with delay: {tourny.DelayBetweenGames}" 
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
      }
    board.MovesAndFenPlayed.Clear()
    let mutable moveInfoData = ChessMoveInfo.Empty
    
    callback(StartOfGame gameStartInfo)
    if not tourny.ConsoleOnly then     
      let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
      let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
      let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
      Initialization.initEngines openingDelayMs tourny player1 player2
    else
      Initialization.initEngines 0 tourny player1 player2
    
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
    let fen = board.FEN()
    logger.LogDebug $"After opening moves, FEN={fen}"    

    let rec playEngine (playing: ChessEngine) (opponent: ChessEngine) (position:uint64) = async {
      
      if cts.IsCancellationRequested then
        //let msg = $"Game between {player1.Name} vs {player2.Name} cancelled"
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
        logger.LogCritical($"Cancel requested when engine ready to play: {playing.Name}")
        callback(EndOfGame res)
        //callback(EndOfTournament tourny)
        return res
      else
        if position <> pos then
          moveTimer <- Stopwatch.GetTimestamp()
          pos <- position
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

        let! line = playing.ReadLineAsync() |> Async.AwaitTask
        if tourny.VerboseLogging then
          logger.LogDebug line  
        if String.IsNullOrEmpty line then
          logger.LogDebug $"Empty line or null from {playing.Name}"
        elif line.StartsWith "info engine" then
          MessagesFromEngine ("Ceres", line) |> callback
          logger.LogInformation line
        if playing.HasExited() then
          //lost on disconnection/failure
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let resValue = if playing.Name = player1.Name then "0-1" else "1-0"
          let res = createResult player1.Name player2.Name gameMoveList resValue ResultReason.ForfeitLimits dur
          callback(EndOfGame res)
          logger.LogInformation($"Player has exited {playing.Name}")
          return res
          
        elif cts.IsCancellationRequested then
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
          callback(EndOfGame res)
          logger.LogInformation($"Cancel requested when engine ready to play: {playing.Name}")
          return res
          
        else          
          if line.StartsWith("bestmove") then                          
            let duration = Stopwatch.GetElapsedTime(moveTimer)            
            if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
              let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
              do! Async.Sleep delay
            let incr = tourny.TimeControl.GetIncrementTime(playing.Config.TimeControlID)
            let currentTicksLeft = 
              if player1.Name = playing.Name then
                wTime.Ticks + incr.Ticks - duration.Ticks
              else 
                bTime.Ticks + incr.Ticks - duration.Ticks
            let useNodes = isNodeLimit playing
            if (not useNodes) && currentTicksLeft + moveOverheadInTicks < 0L then
              let moveTime = (TimeSpan.FromTicks duration.Ticks).TotalMilliseconds
              let timeAlloted = if player1.Name = playing.Name then wTime.Ticks + incr.Ticks else bTime.Ticks + incr.Ticks
              let timeAlloted = (TimeSpan.FromTicks timeAlloted).TotalMilliseconds
              let msg = $"{playing.Name} lost on time - moveTime/budget = {moveTime}/{timeAlloted}ms, ticks left: {currentTicksLeft}, MoveOverhead: {tourny.MoveOverhead.Ticks}"
              logger.LogCritical msg
              let res = 
                if playing.Name = player1.Name then
                  createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.ForfeitLimits duration.Milliseconds
                else
                  createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.ForfeitLimits duration.Milliseconds 
              callback(EndOfGame res)
              return res
            else  
              let (currentTime, timeLeft) =
                let ticks = max currentTicksLeft 0L                
                if player1.Name = playing.Name then 
                  if useNodes then
                    wTime, wTime
                  else    
                    wTime <- TimeOnly ticks
                    wTime, TimeOnly (ticks + incr.Ticks)
                else 
                  if useNodes then
                    bTime, bTime
                  else    
                    bTime <- TimeOnly ticks
                    bTime, TimeOnly (ticks + incr.Ticks)
              
              moveInfoData.tl <- int64 (currentTime.ToTimeSpan().TotalMilliseconds)
              moveInfoData.mt <- int64 duration.TotalMilliseconds
              let move, ponderMove = line.Split().[1], if line.Contains "ponder" then (line.Split().[3]) else ""

              match tryGetTMoveFromCoordinateNotation &board move with
              |Some tmove ->
                let mutable moveAdj = tmove
                let shortSan = getSanNotationFromTMove &board tmove                      
                board.LongSANMovesPlayed.Add(move)                
                gameMoveList.Add(shortSan)
                board.MakeMove(&moveAdj)
                let ponderSan = getShortSanFromLongSan &board ponderMove
                moveInfoData.pd <- ponderSan
                moves <- moves + 1                  
                let eval = 
                  if evalList.Length > 0 then evalList.[0] 
                  elif fullEvalList.Length > 0 then fullEvalList[0] 
                  else EvalType.CP 0.0
                
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
                    }                 
                let moveAndFen = {Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen}
                let mutable posToCheck = board.Position
                let piecesLeft = PositionOps.numberOfPieces &posToCheck
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
                  logger.LogDebug($"Ply {board.PlyCount} - Repetition occured: {bestMove.R3} time(s)")
                
                evalList <- []
                depth <- 0
                selfdepth <- 0
                npsList.Clear()
                
                if moveInfoData.q2 > moveInfoData.q1 then                  
                  Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                if moveInfoData.n2 > moveInfoData.n1 then                  
                  Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                if tourny.VerboseLogging then
                  logger.LogDebug $"FEN={board.FEN()}"
                
                //tablebase adjudication here
                match Adjudication.adjudicateByEval board fullEvalList tourny player1.Name player2.Name playing.Name gametimer gameMoveList moves with
                |Some res -> 
                  if res.Reason = ResultReason.Checkmate then
                    let bm = {bestMove with MoveHistory=bestMove.MoveHistory + "#"}
                    let status = {lastEngineStatus with Eval = EvalType.Mate 0}
                    let numberAndMove = (board.MoveNumberString shortSan) + "#"
                    annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                    callback(BestMove (bm, status))
                  else
                    let numberAndMove = board.MoveNumberString shortSan
                    annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                    callback(BestMove (bestMove, lastEngineStatus))
                  
                  moveInfoData <- ChessMoveInfo.Empty
                  callback(EndOfGame res)
                  let mutable posToCheck = board.Position
                  let piecesLeft = PositionOps.numberOfPieces &posToCheck
                  if tourny.VerboseLogging then
                    logger.LogInformation($"Adjudication by eval: {res.Reason}, Pieces left: {piecesLeft} ")
                    logger.LogInformation (sprintf "Info %A: " res)
                  return res
                |None ->
                  let numberAndMove = board.MoveNumberString shortSan
                  annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                  moveInfoData <- ChessMoveInfo.Empty
                  callback(BestMove (bestMove, lastEngineStatus))
                  return! playEngine opponent playing (board.PositionHash())
              |_ ->                
                let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                //check if there is any legal move in the position
                if board.AnyLegalMove() |> not then
                  //either checkmate or stalemate
                  if board.IsMate() then                   
                    let res = 
                      if playing.Name = player1.Name then
                        createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.Checkmate dur
                      else
                        createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.Checkmate dur
                    callback(EndOfGame res)                    
                    logger.LogInformation($"Checkmate: {res.Reason}")
                    return res
                  else                    
                    let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Stalemate dur
                    callback(EndOfGame res)
                    logger.LogInformation($"Stalemate: {res.Reason}")
                    return res
                else
                  let res = 
                    if playing.Name = player1.Name then
                      createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur
                    else
                      createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur
                  callback(EndOfGame res)
                  let fenAndMoves = board.PositionWithMoves()
                  logger.LogCritical($"{playing.Name} failed in bestmove logic with the following response {line} after these moves: \n{fenAndMoves}")
                  return res               
                 
          else
            let elapsed = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
            let diff = elapsed - lastCheck
            let interval = 1000
               
            if not tourny.TestOptions.PolicyTest && line.StartsWith "info string" && line.Contains "N:" then
              let nnMsg = Regex.getInfoStringData playing.Name line 
              let list = ResizeArray<NNValues>()
              list.Add(nnMsg)              
              let moreItems = if line.StartsWith "info string node" then false else true
              if moreItems = false && tourny.VerboseLogging then
                logger.LogDebug "Only one move in log live stats"
              let mutable cont = moreItems
              while cont do
                let newline = playing.ReadLine()
                if tourny.VerboseLogging then
                  logger.LogDebug($"In info string loop: {playing.Name} {newline}")
                if newline.StartsWith "bestmove" then
                  if tourny.VerboseLogging then
                    logger.LogInformation(board.FEN() + ": new bestmove: " + newline)                
                  cont <- false                 
                if newline.StartsWith "info string node" then
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
              |None -> logger.LogDebug "No move found in log live stats"                
                
              if list.Count > 0 then
                callback (NNSeq list)

            elif line.StartsWith "info" then              
              let isWhite = playing.Name = player1.Name              
              match Regex.getEssentialData line isWhite with
              |Some (d, eval, nodes, nps, pvLine, tbhits, wdl, sd, mPv ) ->                 
                numberOfNodes <- nodes
                if d > depth then
                  depth <- d
                if sd > selfdepth then  
                  selfdepth <- sd
                npsList.Add(float nps)
                evalList <- eval :: evalList
                moveInfoData.d <- depth
                moveInfoData.sd <- selfdepth
                moveInfoData.wv <- eval
                moveInfoData.n <- nodes
                moveInfoData.s <- nps
                moveInfoData.tb <- tbhits
                moveInfoData.pv <- pvLine

                if not (String.IsNullOrEmpty(pvLine)) then
                  if player1.Name = playing.Name then
                    Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                    PVLine1 <- pvLine
                  else
                    Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                    PVLine2 <- pvLine
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
                      TBhits = tbhits
                      WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
                      PV = pv
                      PVLongSAN = pvLong
                      MultiPV = mPv
                    }
                
                lastEngineStatus <- status
                if diff > interval && eval <> EvalType.NA then                  
                  lastCheck <- elapsed
                  callback(Status status)
                
              |None -> ()
            
            return! playEngine playing opponent position
        } 
    let startPos = board.PositionHash()
    if board.Position.STM = 0uy then
      playEngine player1 player2 startPos
    else
      playEngine player2 player1 startPos

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
    callback  =
    
    let moveList = Array.init 256 (fun _ -> defaultof<TMove> )
    let deviationOccured (player:ChessEngine) move =      
      let hash = board.DeviationHash()
      let replay = if player.Name = player1.Name then replayWhite else replayBlack
      match replay.TryGet hash with
      |None -> false, "", ""
      |Some replayData ->       
        if replayData.Move <> move then          
          if replayData.Engine = player.Name then            
            true, replayData.Move, player.Name
          else
            false, "", ""
        else
          false, "", ""              

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
    let npsList = ResizeArray<float>()
    let gameMoveList = board.ShortSANMovesPlayed  
   
    let findTimeSetting (player : ChessEngine) = 
      tourny.FindTimeControl (player.Config.TimeControlID)
    let isNodeLimit player = (findTimeSetting player).NodeLimit
    let w = (findTimeSetting player1).Fixed
    let b = (findTimeSetting player2).Fixed    
    let mutable wTime = w 
    let mutable bTime = b
    let moveOverheadInTicks = tourny.MoveOverhead.Ticks
    let msg = $"Initializing players: {player1.Name} vs {player2.Name} with delay: {tourny.DelayBetweenGames} in playSame mode" 
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
        CurrentGameNr = pairing.GameNr // tourny.CurrentGameNr
      }
    board.MovesAndFenPlayed.Clear()
    let mutable moveInfoData = ChessMoveInfo.Empty
    
    callback(StartOfGame gameStartInfo)
    if not tourny.ConsoleOnly then     
      let moveTimeInSeconds = float tourny.MinMoveTimeInMS / 1000.0
      let timeCalc = float board.OpeningMovesPlayed.Count * moveTimeInSeconds
      let openingDelayMs : int = int (TimeSpan.FromSeconds(timeCalc + 2.0)).TotalMilliseconds
      Initialization.initEngines openingDelayMs tourny player1 player2
    else
      Initialization.initEngines 0 tourny player1 player2
    
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
    let fen = board.FEN()
    if tourny.VerboseLogging then
      logger.LogDebug $"After opening moves, FEN={fen}"    

    let rec playEngine (playing: ChessEngine) (opponent: ChessEngine) (position:uint64) = async {
      let replay = if playing.Name = player1.Name then replayWhite else replayBlack
      if cts.IsCancellationRequested then
        let msg = $"Game between {player1.Name} vs {player2.Name} was cancelled"
        let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
        let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
        logger.LogCritical(msg)
        callback(EndOfGame res)
        //callback(EndOfTournament tourny)
        return res
      else
        if position <> pos then            
          moveTimer <- Stopwatch.GetTimestamp()
          pos <- position
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

        let! line = playing.ReadLineAsync() |> Async.AwaitTask 
        if tourny.VerboseLogging then
          logger.LogDebug line
        if String.IsNullOrEmpty line then
          logger.LogDebug $"Empty line or null from {playing.Name}"
        elif line.StartsWith "info engine" then
          MessagesFromEngine ("Ceres", line) |> callback
          logger.LogInformation line
        if playing.HasExited() then
          //lost on disconnection/failure
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let resValue = if playing.Name = player1.Name then "0-1" else "1-0"
          let res = createResult player1.Name player2.Name gameMoveList resValue ResultReason.ForfeitLimits dur
          callback(EndOfGame res)
          logger.LogInformation($"Player has exited {playing.Name}")
          return res
          
        elif cts.IsCancellationRequested then
          let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
          let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Cancel dur
          callback(EndOfGame res)
          logger.LogInformation($"Cancel requested when engine ready to play: {playing.Name}")
          return res
          
        else          
          if line.StartsWith("bestmove") then
            let duration = Stopwatch.GetElapsedTime(moveTimer)            
            if duration.TotalMilliseconds < tourny.MinMoveTimeInMS then
              let delay = tourny.MinMoveTimeInMS - (duration.TotalMilliseconds |> int)
              do! Async.Sleep delay
            let incr = tourny.TimeControl.GetIncrementTime(playing.Config.TimeControlID)
            let currentTicksLeft = 
              if player1.Name = playing.Name then
                wTime.Ticks + incr.Ticks - duration.Ticks
              else 
                bTime.Ticks + incr.Ticks - duration.Ticks

            let useNodes = isNodeLimit playing
            if (not useNodes) && currentTicksLeft + moveOverheadInTicks < 0L then            
              let moveTime = (TimeSpan.FromTicks duration.Ticks).TotalMilliseconds
              let timeAlloted = if player1.Name = playing.Name then wTime.Ticks + incr.Ticks else bTime.Ticks + incr.Ticks
              let timeAlloted = (TimeSpan.FromTicks timeAlloted).TotalMilliseconds
              let msg = $"{playing.Name} lost on time - moveTime/budget = {moveTime}/{timeAlloted}ms, ticks left: {currentTicksLeft}, MoveOverhead: {tourny.MoveOverhead.Ticks}"
              logger.LogCritical msg
              let res = 
                if playing.Name = player1.Name then
                  createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.ForfeitLimits duration.Milliseconds
                else
                  createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.ForfeitLimits duration.Milliseconds 
              callback(EndOfGame res)
              return res
            else  
              let (currentTime, timeLeft) =
                let ticks = max currentTicksLeft 0L
                if player1.Name = playing.Name then 
                  wTime <- TimeOnly ticks
                  wTime, TimeOnly (ticks + incr.Ticks)
                else 
                  bTime <- TimeOnly ticks
                  bTime, TimeOnly (ticks + incr.Ticks)
              
              moveInfoData.tl <- int64 (currentTime.ToTimeSpan().TotalMilliseconds)
              moveInfoData.mt <- int64 duration.TotalMilliseconds
              let move, ponderMove = line.Split().[1], if line.Contains "ponder" then (line.Split().[3]) else ""

              match tryGetTMoveFromCoordinateNotation &board move with
              |Some tmove ->
                let mutable shortSan = getSanNotationFromTMove &board tmove
                let mutable move = move
                let deviated, oldMove, engName = deviationOccured playing move
                if deviated then 
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
                  |_ -> //quick fix for FRC castling move
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
                  let devHash = board.DeviationHash()
                  replay[devHash] <- {Engine=playing.Name; Move = move; TimeLeftInMs = moveInfoData.tl; Hash = pairing.OpeningHash }
                  board.LongSANMovesPlayed.Add(move)                
                  gameMoveList.Add(shortSan)
                  board.MakeMove &tmove

                moves <- moves + 1 
                let ponderSan = getShortSanFromLongSan &board ponderMove
                moveInfoData.pd <- ponderSan
                let eval = 
                  if evalList.Length > 0 then evalList.[0] 
                  elif fullEvalList.Length > 0 then fullEvalList[0] 
                  else EvalType.CP 0.0
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
                    }                 
                let moveAndFen = {Move = moveDetail; ShortSan = shortSan; FenAfterMove = fen}
                let mutable posToCheck = board.Position
                let piecesLeft = PositionOps.numberOfPieces &posToCheck
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
                  logger.LogDebug($"Ply {board.PlyCount} - Repetition occured: {bestMove.R3} time(s)")
                evalList <- []
                depth <- 0
                selfdepth <- 0
                npsList.Clear()
                if moveInfoData.q2 > moveInfoData.q1 then
                  //logger.LogCritical $"{playing.Name} did selected a suboptimal Q-move q1/n1={moveInfoData.q1}/{moveInfoData.n1}, q2/n2={moveInfoData.q2}/{moveInfoData.n2}"
                  Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                if moveInfoData.n2 > moveInfoData.n1 then                  
                  Q1DifferentFromN1 <- Q1DifferentFromN1 + 1
                if tourny.VerboseLogging then
                  logger.LogDebug $"FEN={board.FEN()}"
                //tablebase adjudication here
                match Adjudication.adjudicateByEval board fullEvalList tourny player1.Name player2.Name playing.Name gametimer gameMoveList moves with
                |Some res ->                   
                  if res.Reason = ResultReason.Checkmate then
                    let bm = {bestMove with MoveHistory=bestMove.MoveHistory + "#"}
                    let status = {lastEngineStatus with Eval = EvalType.Mate 0}
                    let numberAndMove = (board.MoveNumberString shortSan) + "#"
                    annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                    callback(BestMove (bm, status))
                  else
                    let numberAndMove = board.MoveNumberString shortSan
                    annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                    callback(BestMove (bestMove, lastEngineStatus))
                  
                  moveInfoData <- ChessMoveInfo.Empty
                  callback(EndOfGame res)
                  let mutable posToCheck = board.Position
                  let piecesLeft = PositionOps.numberOfPieces &posToCheck
                  if tourny.VerboseLogging then
                    logger.LogInformation($"Adjudication by eval: {res.Reason}, Pieces left: {piecesLeft} ")
                    logger.LogInformation (sprintf "Info %A: " res)
                  //logger.LogInformation $"Number of disagreement moves in the game = {Q1DifferentFromN1}"
                  return res
                |None ->
                  let numberAndMove = board.MoveNumberString shortSan
                  annotation tourny.VerboseMoveAnnotation board numberAndMove moveInfoData |> append
                  moveInfoData <- ChessMoveInfo.Empty
                  callback(BestMove (bestMove, lastEngineStatus))
                  return! playEngine opponent playing (board.PositionHash())
              |_ ->                
                let dur = int64 (Stopwatch.GetElapsedTime(gametimer).TotalMilliseconds)
                //check if there is any legal move in the position
                if board.AnyLegalMove() |> not then
                  //either checkmate or stalemate
                  if board.IsMate() then                   
                    let res = 
                      if playing.Name = player1.Name then
                        createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.Checkmate dur
                      else
                        createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.Checkmate dur
                    callback(EndOfGame res)
                    logger.LogInformation($"Checkmate: {res.Reason}")
                    return res
                  else                    
                    let res = createResult player1.Name player2.Name gameMoveList "1/2-1/2" ResultReason.Stalemate dur
                    callback(EndOfGame res)
                    logger.LogInformation($"Stalemate: {res.Reason}")
                    return res
                else
                  let res = 
                    if playing.Name = player1.Name then
                      createResult player1.Name player2.Name gameMoveList "0-1" ResultReason.Illegal dur
                    else
                      createResult player1.Name player2.Name gameMoveList "1-0" ResultReason.Illegal dur
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
              let moreItems = if line.StartsWith "info string node" then false else true
              if moreItems = false && tourny.VerboseLogging then
                logger.LogDebug "Only one move in log live stats"
              let mutable cont = moreItems
              while cont do
                let newline = playing.ReadLine()
                if tourny.VerboseLogging then
                  logger.LogDebug($"In info string loop: {playing.Name} {newline}")
                if newline.StartsWith "bestmove" then
                  if tourny.VerboseLogging then
                    logger.LogInformation(board.FEN() + ": new bestmove: " + newline)                
                  cont <- false                 
                if newline.StartsWith "info string node" then
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
              |None -> () //logger.LogCritical "No move found in log live stats"                
                
              if list.Count > 0 then
                callback (NNSeq list)

            elif line.StartsWith "info" then              
              let isWhite = playing.Name = player1.Name              
              match Utilities.Regex.getEssentialData line isWhite with
              |Some (d, eval, nodes, nps, pvLine, tbhits, wdl, sd, mPv ) ->                 
                numberOfNodes <- nodes                
                if d > depth then
                  depth <- d
                if sd > selfdepth then  
                  selfdepth <- sd
                npsList.Add(float nps)
                evalList <- eval :: evalList
                moveInfoData.d <- depth
                moveInfoData.sd <- selfdepth
                moveInfoData.wv <- eval
                moveInfoData.n <- nodes
                moveInfoData.s <- nps
                moveInfoData.tb <- tbhits
                moveInfoData.pv <- pvLine


                if not (String.IsNullOrEmpty(pvLine)) then
                  if player1.Name = playing.Name then
                    Player1PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                    PVLine1 <- pvLine
                  else
                    Player2PV <- getShortSanPVFromLongSanPVFast moveList &board pvLine
                    PVLine2 <- pvLine
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
                      TBhits = tbhits
                      WDL = if wdl.IsSome then WDLType.HasValue wdl.Value else WDLType.NotFound
                      PV = pv
                      PVLongSAN = pvLong
                      MultiPV = mPv
                    }
                
                lastEngineStatus <- status
                if diff > interval && eval <> EvalType.NA then                  
                  lastCheck <- elapsed
                  callback(Status status)
                
              |None -> ()
            
            return! playEngine playing opponent position
        } 
    let startPos = board.PositionHash()
    if board.Position.STM = 0uy then
      playEngine player1 player2 startPos
    else
      playEngine player2 player1 startPos  

  
  let gauntlet (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) = async {    
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
          PGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
    let gamesAlreadyPlayed = 
      let fileExists = File.Exists tourny.PgnOutPath      
      if fileExists then
        PGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
      else
        [||]

    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        PGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
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
      let pgnGameWriterAgent = Parser.PGNParser.startPgnGameReaderWriter tourny.PgnOutPath
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
          
          //what to do with previous games here?
          for g in previousGames do
            printfn "Previous game found from referencePGN: %s, %s for pairing %s, %s" g.GameMetaData.White g.GameMetaData.Black pairing.White.Name pairing.Black.Name
          
          let replayBoard = Board()
          replayBoard.IsFRC <- tourny.IsChess960
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
            for m in game.Moves do
              if m.WhiteSan <> "" then
                let hash = replayBoard.DeviationHash()
                replayBoard.PlaySimpleShortSan m.WhiteSan
                if replayBoard.LongSANMovesPlayed.Count > idx then
                  let lastmove = replayBoard.LongSANMovesPlayed[idx]
                  let data : ReplayData = {Engine=game.GameMetaData.White; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}                  
                  if isWhite then
                    replayDictWhite[hash] <- data
                  idx <- idx + 1
              if m.BlackSan <> "" then
                let hash = replayBoard.DeviationHash()
                replayBoard.PlaySimpleShortSan m.BlackSan
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
          let limit = (tourny.Opening.OpeningsPly / 2) + (tourny.Opening.OpeningsPly % 2 )
          let openingMoves = pair.Opening.Moves |> Seq.truncate(limit)
          let completeGame = openingMoves |> Seq.fold(fun state m -> sprintf "%s %s %s" state m.WhiteSan m.BlackSan) ""
          logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
          board.ResetBoardState()
          if pair.Opening.Fen = "" then
            board.LoadFen(Chess.startPos)
            board.StartPosition <- Chess.startPos
          else 
            board.LoadFen(pair.Opening.Fen)
            board.StartPosition <- pair.Opening.Fen
            tourny.IsChess960 <- board.IsFRC
          let mutable moveIndex = 0
          if not epdBook then
            for m in openingMoves do
              board.PlayOpeningMove m.WhiteSan
              moveIndex <- moveIndex + 1
              if m.BlackSan <> "" then
                board.PlayOpeningMove m.BlackSan
                moveIndex <- moveIndex + 1
            
          let posWithMoves =
            let fen = board.StartPosition
            let start = $"position fen {fen} moves"
            board.LongSANMovesPlayed |> Seq.fold(fun state m -> 
              sprintf "%s %s" state m) start            
          logger.LogInformation("{position}", posWithMoves)
          let engine1 = pair.White |> EngineHelper.createEngine
          let engine2 = pair.Black |> EngineHelper.createEngine
          
          let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
          let liveGamesPlayed = gamesLeftToPlay |> Seq.truncate gameNr |> Seq.filter(fun e -> e.OpeningHash = pair.OpeningHash) |> Seq.length
          let roundTxt = $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1 }"
          Update.RoundNr roundTxt |> callback
          let result =
            if tourny.PreventMoveDeviation then              
              let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name                  
              playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair callback |> Async.RunSynchronously
            
            else
              play sb cts logger tourny board engine1 engine2 pair callback |> Async.RunSynchronously
          results <- result :: results
          engine1.Stop() 
          engine2.Stop()
                    
          
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
            pgnGameWriterAgent.Post (Parser.PGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
            //PGNHelper.writePgnGame tourny.PgnOutPath gameData moveSection result
          if tourny.VerboseLogging then
            logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
          engine1.StopProcess()
          engine2.StopProcess()
          do! Async.Sleep(tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int)
          board.ResetBoardState()
          gameNr <- gameNr + 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res) 
      
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      pgnGameWriterAgent.Post(Parser.PGNParser.Dispose)
      pgnGameWriterAgent.Dispose()      
      return results
  }

  let roundRobin (logger:ILogger) (tourny:Tournament) callback (cts: CancellationTokenSource) = async {        
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
          PGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
   
    let gamesAlreadyPlayed = 
      let fileExists = File.Exists tourny.PgnOutPath
      if fileExists then
        PGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
      else
        [||]
    
    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        PGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
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
      let pgnGameWriterAgent = Parser.PGNParser.startPgnGameReaderWriter tourny.PgnOutPath
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
          let limit = (tourny.Opening.OpeningsPly / 2) + (tourny.Opening.OpeningsPly % 2 )
          let openingMoves = pair.Opening.Moves |> Seq.truncate(limit)
          let completeGame = openingMoves |> Seq.fold(fun state m -> sprintf "%s %s %s" state m.WhiteSan m.BlackSan) ""
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
              board.PlayOpeningMove m.WhiteSan
              moveIndex <- moveIndex + 1
              if m.BlackSan <> "" then
                board.PlayOpeningMove m.BlackSan
                moveIndex <- moveIndex + 1
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
            engine1 <- pair.White |> EngineHelper.createEngine
            engine2 <- pair.Black |> EngineHelper.createEngine
          if engine1 = defaultof<ChessEngine> || engine2 = defaultof<ChessEngine> then
            engine1 <- pair.White |> EngineHelper.createEngine
            engine2 <- pair.Black |> EngineHelper.createEngine
          if engine1.Name = pair.Black.Name || engine2.Name = pair.White.Name then
            let (eng1,eng2) = engine2, engine1
            engine1 <- eng1
            engine2 <- eng2
          
          let openingsAlreadyPlayed = gamesAlreadyPlayed |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pair.OpeningHash) |> Seq.length
          let liveGamesPlayed = gamesLeftToPlay |> Seq.truncate gameNr |> Seq.filter(fun e -> e.OpeningHash = pair.OpeningHash) |> Seq.length
          let roundTxt = $"{pair.Opening.GameNumber}.{openingsAlreadyPlayed + liveGamesPlayed + 1 }"
          Update.RoundNr roundTxt |> callback          

          let result =
            if tourny.PreventMoveDeviation then              
                let replayDictWhite, replayDictBlack = getReplayDictForPlayer pair.White.Name, getReplayDictForPlayer pair.Black.Name
                playDoNotDeviate replayDictWhite replayDictBlack sb cts logger tourny board engine1 engine2 pair callback |> Async.RunSynchronously
            else
                play sb cts logger tourny board engine1 engine2 pair callback |> Async.RunSynchronously
          results <- result :: results       
          engine1.Stop() 
          engine2.Stop()

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
            pgnGameWriterAgent.Post (Parser.PGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))
            //PGNHelper.writePgnGame tourny.PgnOutPath gameData moveSection result
          if tourny.VerboseLogging then
            logger.LogInformation("Game metadata added to result: {pgnData}", gameData)
          if numberOfPlayers > 2 || cts.IsCancellationRequested then
            engine1.StopProcess()
            engine2.StopProcess()            
          do! Async.Sleep(tourny.DelayBetweenGames.ToTimeSpan().TotalMilliseconds |> int)
          board.ResetBoardState()
          gameNr <- gameNr + 1
          if gameNr % 2 = 0 then
            let res = ResizeArray<Result>(results)
            callback (Update.PeriodicResults res) 
      
      let res = ResizeArray<Result>(results)
      callback (Update.PeriodicResults res)
      pgnGameWriterAgent.Post(Parser.PGNParser.Dispose)
      pgnGameWriterAgent.Dispose()      
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
          let all = PGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
          if tourny.VerboseLogging then
            logger.LogInformation $"Total number of openings in PGN = {all.Length}"
          all
      |_ ->
        [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
    let gamesAlreadyPlayed = 
      let fileExists = File.Exists tourny.PgnOutPath      
      if fileExists then
        PGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
      else
        [||]    
    
    let referencGamesPlayed =
      let fileExists = File.Exists tourny.ReferencePGNPath
      if fileExists then
        PGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
      else
        [||]
    let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
    let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
    let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    gameNr <- gamesAlreadyPlayed.Length
    
    let allPairings = 
      if tourny.Gauntlet then
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
      let pgnGameWriterAgent = Parser.PGNParser.startPgnGameReaderWriter tourny.PgnOutPath
      callback (Update.TotalNumberOfPairs allPairings.Length) 
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      let lastGame = gamesAlreadyPlayed |> Seq.tryLast
      let pairings = 
        match lastGame, allPairings.Length > numberOfGamesPlayed with
        |Some last, true ->               
            if last.Moves.Count > 0 then
              let lastMove = last.Moves |> Seq.last
              if lastMove.BlackComment.Contains "cancelled" || lastMove.WhiteComment.Contains "cancelled" then
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
        tourny.EngineSetup.Engines |> List.map(fun e -> EngineHelper.createEngine e)
      
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
            let eng1 = EngineHelper.createEngine e1.Config
            let eng2 = EngineHelper.createEngine e2.Config
            Initialization.initEngines 0 tourny eng1 eng2
            yield eng1, eng2
        ]      
      
      let calculateMemUsage (engine1:ChessEngine) (engine2:ChessEngine) =
        let configs = [engine1.Config; engine2.Config]
        let footPrintBoth = HardwareInfo.sumFootprints configs         
        let totalAvail = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> uint64
        let maxMem = totalAvail / 2UL
        let concurrency = int (maxMem / footPrintBoth)
        concurrency

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
                          let limit = (tourny.Opening.OpeningsPly / 2) + (tourny.Opening.OpeningsPly % 2 )
                          let openingMoves = pair.Opening.Moves |> Seq.truncate(limit)
                          let completeGame = openingMoves |> Seq.fold(fun state m -> sprintf "%s %s %s" state m.WhiteSan m.BlackSan) ""
                          if tourny.VerboseLogging then
                            logger.LogInformation("Opening number {gameNr} - with opening moves {completeGame}", pair.Opening.GameNumber, completeGame)
                
                          if pair.Opening.Fen = "" then
                            currentBoard.LoadFen Chess.startPos
                            board.StartPosition <- Chess.startPos
                          else 
                            currentBoard.LoadFen pair.Opening.Fen
                            currentBoard.StartPosition <- pair.Opening.Fen
                            tourny.IsChess960 <- currentBoard.IsFRC
                          let mutable moveIndex = 0
                          if not epdBook then
                            for m in openingMoves do
                              currentBoard.PlayOpeningMove m.WhiteSan
                              moveIndex <- moveIndex + 1
                              if m.BlackSan <> "" then
                                currentBoard.PlayOpeningMove m.BlackSan
                                moveIndex <- moveIndex + 1
            
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
                                if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                    searchReplayList pair
                                    let whiteReplayDict = replayDicts.[pair.White.Name]
                                    let blackReplayDict = replayDicts.[pair.Black.Name]                                    
                                    playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard engine1 engine2 pair callback
                                else
                                    play sb cts logger tourny currentBoard engine1 engine2 pair callback                              
                              
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
                                  OtherTags = pair.Opening.GameMetaData.OtherTags
                                }

                              let moveSection = sb.ToString()
                              if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                                pgnGameWriterAgent.Post (Parser.PGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))                                
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
      let games = pgnGameWriterAgent.PostAndReply(fun reply -> Parser.PGNParser.GetPGNGames(reply))
      pgnGameWriterAgent.Post(Parser.PGNParser.Dispose)
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
                let all = PGNParser.parsePgnFile path |> Seq.truncate tourny.Rounds |> Seq.toArray
                if tourny.VerboseLogging then
                    logger.LogInformation $"Total number of openings in PGN = {all.Length}"
                all
            |_ ->
                [| for i = 1 to tourny.Rounds do yield PGNTypes.PgnGame.Empty i |]
    
        let gamesAlreadyPlayed = 
            let fileExists = File.Exists tourny.PgnOutPath      
            if fileExists then
                PGNParser.parsePgnFile tourny.PgnOutPath |> Seq.toArray
            else
                [||]    
    
        let referencGamesPlayed =
            let fileExists = File.Exists tourny.ReferencePGNPath
            if fileExists then
                PGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
            else
                [||]
        let gamesToPlay = games |> Seq.truncate (tourny.Rounds) |> Seq.toList
        let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
        let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    
        let allPairings = 
            if tourny.Gauntlet then
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
                            let eng = EngineHelper.createEngine e
                            EngineHelper.initEngine 0 eng
                            ch.Writer.TryWrite(eng) |> ignore
                            eng.Name, ch )
                    engines )
                |> Array.concat
                |> Map.ofArray

            // 3) PGN writer agent stays the same
            let pgnAgent = Parser.PGNParser.startPgnGameReaderWriter tourny.PgnOutPath

            // a thread‐safe result collector
            let results = System.Collections.Concurrent.ConcurrentBag<Result>()

            // 4) helper to play one pairing using borrowed engines
            let playOne (pair: Pairing) = task {
                // borrow
                let! wEng = enginePools.[pair.White.Name].Reader.ReadAsync()
                let! bEng = enginePools.[pair.Black.Name].Reader.ReadAsync()
                try
                    // (you can extract all your existing “play…” logic into this helper)
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
                            let limit = (tourny.Opening.OpeningsPly / 2) + (tourny.Opening.OpeningsPly % 2 )
                            let openingMoves = pair.Opening.Moves |> Seq.truncate(limit)
                            let completeGame = openingMoves |> Seq.fold(fun state m -> sprintf "%s %s %s" state m.WhiteSan m.BlackSan) ""
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
                                    currentBoard.PlayOpeningMove m.WhiteSan
                                    moveIndex <- moveIndex + 1
                                    if m.BlackSan <> "" then
                                        currentBoard.PlayOpeningMove m.BlackSan
                                    moveIndex <- moveIndex + 1
            
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
                                if tourny.PreventMoveDeviation && tourny.TestOptions.NumberOfGamesInParallelConsoleOnly = 1 && moreThanTwoPlayers then
                                    searchReplayList pair
                                    let whiteReplayDict = replayDicts.[pair.White.Name]
                                    let blackReplayDict = replayDicts.[pair.Black.Name]                                    
                                    playDoNotDeviate whiteReplayDict blackReplayDict sb cts logger tourny currentBoard wEng bEng pair callback
                                else
                                    play sb cts logger tourny currentBoard wEng bEng pair callback                              
                              
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
                                    OtherTags = pair.Opening.GameMetaData.OtherTags
                                }

                            let moveSection = sb.ToString()
                            if not cts.IsCancellationRequested && String.IsNullOrWhiteSpace tourny.PgnOutPath |> not then
                                pgnAgent.Post (Parser.PGNParser.WriteGame(tourny.PgnOutPath, gameData, moveSection, result))                                
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
            let worker i = task {
                while pairingCh.Reader.WaitToReadAsync().Result do
                    match pairingCh.Reader.TryRead() with
                    | true, pair -> 
                        logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                        do! playOne pair
                        Interlocked.Increment(&gameCounter) |> ignore // Increment the counter atomically
                        if gameCounter % 10 = 0 then
                            let res = ResizeArray<Result>(results) // Collect results
                            callback (Update.PeriodicResults res) // Call the callback every 10 gam
                    | _ -> ()
                }

            // 6) launch exactly p workers
            let! _ = 
                [| for i in 1..concurrency -> worker i |]
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
            let games = pgnAgent.PostAndReply(fun reply -> Parser.PGNParser.GetPGNGames(reply))
            pgnAgent.Post(Parser.PGNParser.Dispose)
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
                Utilities.Validation.validateTournamentInput tourny engineList
                if tourny.Gauntlet && tourny.Challengers > 0 then
                  for engine in engineList |> List.truncate tourny.Challengers do
                    engine.IsChallenger <- true
                else
                  for engine in engineList do
                    engine.IsChallenger <- false
                let engineSetup = {tourny.EngineSetup with Engines = engineList}
                {tourny with EngineSetup = engineSetup }
              else 
                //let enginesTest = createEnginesFromFolder "C:/Dev/Chess/Networks/CeresLatest" |> Seq.toList
                //(enginesTest |> List.head).IsChallenger <- true
                //let engineSetup = {tourny.EngineSetup with Engines = enginesTest}
                tourny            
        
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
  
  let startTournament cts (tournament : Tournament) (logger:ILogger) sendResponse consoleMode =
      logger.LogInformation (tournament.Summary())
      let timer = Stopwatch()
      timer.Start()
      let tourny = 
        //let nodeLimit = tournament.EngineSetup.Engines |> List.map(fun e -> tournament.FindTimeControl e.TimeControlID) |> List.forall(fun e -> e.NodeLimit)
        if consoleMode then
          Match.parallelTournamentRun logger tournament sendResponse cts       
        elif tournament.Gauntlet then
          Match.gauntlet logger tournament sendResponse cts          
        else 
          Match.roundRobin logger tournament sendResponse cts            
  
      //run tournament
      let res = tourny |> Async.RunSynchronously
      sendResponse (Match.EndOfTournament tournament)      
      logger.LogInformation($"Elapsed tournament time in seconds: {(timer.ElapsedMilliseconds/1000L)}")
      res
  
  type Runner (logger, callback: Action<Match.Update>, reloadTournament:bool, consoleOnly : bool) =    
    let cts = new CancellationTokenSource()
    let mutable tournament = if reloadTournament then loadTournament() else Tournament.Empty
    let mutable resultsFromPGN = ResizeArray<Result>()
    let mutable pgnReader = None
    let mutable consoleMode = consoleOnly
    
    member val TotalGames = 0 with get, set
    member x.PgnReader
        with get() = 
          if String.IsNullOrWhiteSpace tournament.PgnOutPath then
            failwith "PgnOutPath is not set in tournament.json"
          match pgnReader with
          |None -> 
            Parser.PGNParser.startPgnGameReaderWriter tournament.PgnOutPath
          |Some pgnReader -> pgnReader
        and set(value) = pgnReader <- Some value     

    member x.SendResponse (update: Match.Update) =       
      // Raise the callback with a proper Update response
      match update with
      | Match.PeriodicResults results -> 
          let pgnGames = x.GetPGNGames()
          if pgnGames.Count > 0 then
              let consoleResString, _, _ = PGNCalculator.getEngineDataResults pgnGames
              let gameUpdate = Match.Update.GameSummary consoleResString
              callback.Invoke gameUpdate
          else
            let pRes = x.GetPlayerResults results
            let cross = x.GenerateStatsCrosstableInHtml results            
            let table = OrdoHelper.getResultsAndPairsInConsoleFormat pRes cross            
            callback.Invoke (Match.Update.GameSummary table)
      |_ -> callback.Invoke update
        
    member _.AddTournament tourny = tournament <- tourny 
    member x.Run() = 
      try 
        resultsFromPGN <- x.GetResults() //x.GetFinalResults()
        startTournament cts tournament logger x.SendResponse consoleMode
      with e -> 
        printfn "Error: %A" e
        logger.LogCritical ("failed to run tournament" + tournament.MinSummary())
        raise e

    member _.GetPlayerResults (results: ResizeArray<Result>) : ResizeArray<PlayerResult> =
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let isGauntlet = tournament.Gauntlet
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
        let results = x.PgnReader.PostAndReply(fun reply -> Parser.PGNParser.GetResults reply )
        results        
      else              
        ResizeArray<Result>()

    member x.GetPGNGames() : ResizeArray<PgnGame> = 
      let fileExists = File.Exists tournament.PgnOutPath
      if fileExists then
        let results = x.PgnReader.PostAndReply(fun reply -> Parser.PGNParser.GetPGNGames reply )
        results        
      else              
        ResizeArray<PgnGame>()
 
    member _.GenerateCrosstableEntries (results: ResizeArray<Result>) =
      PGNCalculator.generateCrosstableEntries results
 
    member _.GenerateStatsCrosstableInHtml (results: ResizeArray<Result>) = 
      let challengers = tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      PGNCalculator.generateSmallStatCrossTableHtml results challengers players

    member _.GetGauntletCrosstable (results: ResizeArray<Result>) = 
      let players = tournament.EngineSetup.Engines |> List.map _.Name
      let challengers = 
        if tournament.EngineSetup.Engines.Length = 2 then
          players |> List.take 1
        else          
          tournament.EngineSetup.Engines |> List.filter (fun e -> e.IsChallenger) |> List.map _.Name
      PGNCalculator.generateBigStatCrossTableHtml results challengers players
