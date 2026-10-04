module ChessLibrary.Test

open System
open System.Collections.Generic
open System.IO
open PositionTypes
open MoveTypes
open GameAnalysis
open EngineProtocol
open ChessUtilities
open RuntimeUtilities
open Chess
open ChessLibrary.BoardUtils
open Perft

open System.Diagnostics

let parseChess960Record (input: string) =
    //printfn "%s" input
    let parts = input.Split('\t')
    let positionNumber = int parts.[0]
    let fen = parts.[1]
    let depth1 = int64 parts.[2]
    let depth2 = int64 parts.[3]
    let depth3 = int64 parts.[4]
    let depth4 = int64 parts.[5]
    let depth5 = int64 parts.[6]
    let depth6 = int64 parts.[7]
    { PositionNumber = positionNumber; FEN = fen; Depth1 = depth1; Depth2 = depth2; Depth3 = depth3; Depth4 = depth4; Depth5 = depth5; Depth6 = depth6 }

let parseFile (filePath : string) =
    let lines = File.ReadAllLines(filePath) |> Array.skip 1
    let records = lines |> Array.map parseChess960Record
    records

let records path = parseFile path

let mutable board = Board()
let rnd = Random()
board.LoadFen(Chess.startPos)
//let testFen = BoardHelper.posToFen board.Position
//check hash for these two:

let timeAndReportPerft (records: seq<Chess960Record>) depth =
    let mutable totalNodes = 0L
    let mutable totalTime = 0.0
    let stopwatch = Stopwatch.StartNew()

    for r in records do
        stopwatch.Restart()
        Perft.perftOptChecked r depth
        let nodes = getNumberFromRecord depth r
        stopwatch.Stop()

        let elapsedSeconds = stopwatch.Elapsed.TotalSeconds
        let nps = float nodes / elapsedSeconds

        totalNodes <- totalNodes + nodes
        totalTime <- totalTime + elapsedSeconds

        printfn "\tTime = %.2f seconds, NPS = %s" elapsedSeconds (String.Format("{0:N0}", nps))

    let overallNps = float totalNodes / totalTime
    printfn "\nOverall: Total Nodes = %d, Total Time = %.2f seconds, NPS = %s" totalNodes totalTime (String.Format("{0:N0}", overallNps))



let repeatPerft fen depth n =
  for _ = 1 to n do
    Perft.perftOpt depth fen |> ignore

let smallPerftTestSample depth =
  printfn $"\nStarting perft test on a selection of positions"
  for pos in Perft.selectionOfTestPositions do  
    printfn $"\nStarting perft test on position {pos}"
    Perft.perftOpt depth pos

let completeFRCPerftVerificationTest depth upto =
  let orgColor = Console.ForegroundColor
  let records =
    TestPositions.CHESS960PERFT_POS.Split(Environment.NewLine)
    |> Seq.skip(1)
    |> Seq.map parseChess960Record
    |> Seq.truncate upto
    |> Seq.toArray
  printfn $"\nStarting Chess960 Chess perft test"
  timeAndReportPerft records depth
  Console.ForegroundColor <- orgColor

let completeFRCPerftVerificationTestFast depth upto =
  let orgColor = Console.ForegroundColor
  let records =
    TestPositions.CHESS960PERFT_POS.Split(Environment.NewLine)
    |> Seq.skip(1)
    |> Seq.map parseChess960Record
    |> Seq.truncate upto
    |> Seq.toArray
  printfn $"\nStarting Chess960 Chess perft test"
  let mutable totalNodes = 0L
  let mutable totalTime = 0.0
  let stopwatch = Stopwatch.StartNew()
  for r in records do
    stopwatch.Restart()
    Perft.perftOptCheckedFast r depth
    let nodes = getNumberFromRecord depth r
    stopwatch.Stop()
    let elapsedSeconds = stopwatch.Elapsed.TotalSeconds
    let nps = float nodes / elapsedSeconds
    totalNodes <- totalNodes + nodes
    totalTime <- totalTime + elapsedSeconds
    printfn "\tTime = %.2f seconds, NPS = %s" elapsedSeconds (String.Format("{0:N0}", nps))
  let overallNps = float totalNodes / totalTime
  printfn "\nOverall: Total Nodes = %d, Total Time = %.2f seconds, NPS = %s" totalNodes totalTime (String.Format("{0:N0}", overallNps))
  Console.ForegroundColor <- orgColor

let fen1 = """8/p1r2b2/1p1R1P1k/7p/P1p1N1p1/4N3/1bB2P1P/6K1 w - - 1 39"""
let fen2 = "8/p1r1Nb2/1p1R1P1k/7p/P1p1N1p1/8/1bB2P1P/6K1 w - - 5 41"

let mutable posToUse = board.Position
BoardHelper.loadFen(Some fen1, &posToUse)
let h1 = Hash.hashBoard posToUse
posToUse <- board.Position
BoardHelper.loadFen(Some fen2, &posToUse)
let h2 = Hash.hashBoard posToUse
//printfn "%d vs %d" h1 h2

let randomMoveTest () =
  let m = makeRandomMove rnd &board
  let toMove = (board.Position.STM ^^^ PositionOps.BLACK)
  let move = TMoveOps.moveToStr &m toMove
  printfn "\n%A: %s " m.MoveType move

let playRandomGame () =
  for _ = 0 to 200 do
    board.PrintPosition("\nNew pos")
    randomMoveTest()
    //randomMoveTest()

//playRandomGame()

//let curPos = Perft.pos6
//let depth = 5
//Perft.divide depth Perft.pos2
//Perft.divide depth Perft.pos3
//Perft.divide depth Perft.pos4
//Perft.divide depth Perft.pos5
//Perft.divide depth Perft.pos6
//let tot = Perft.perftOpt depth curPos
//repeatPerft curPos depth 3

module ParsingTests =
  let pgnPath = "C:/Dev/Chess/Openings/sts.pgn"
  let queenOddsGames = "C:/Users/Navn/Downloads/lichess_LeelaQueenOdds_2025-03-14.pgn"
    
  let getAllMatesFromPGN pgnFile printToConsole =
    let board = Board()
    let games = FullPGNParser.parsePgnFile pgnFile |> Seq.truncate 1_000_000
    let mutable numberOfPlys = 0
    let mutable gameIdx = 0
    printfn "Started parsing of PGN file and looking for games with mates: %s" pgnFile
    [
      for pgn in games do
        gameIdx <- gameIdx + 1
        if gameIdx % 10_000 = 0 then
          printfn "Game number: %d Number of plies: %d " gameIdx numberOfPlys
        if printToConsole then
          printfn "Start of game number: %d\n" gameIdx 
        board.ResetBoardState()
        if String.IsNullOrEmpty pgn.Fen |> not then
          board.LoadFen(pgn.Fen)  

        for m in pgn.Mainline do
            board.PlaySanMove m.San
            if printToConsole then
              match Regex.parseEvalRegexOption m.Comment false with
              | Some eval -> 
                    if m.Color = "w" then
                      printfn "White move %s, move number: %d Eval: %f" m.San m.MoveNumber eval
                    else
                    printfn "Black move %s, move number: %d Eval: %f" m.San m.MoveNumber eval
              | None -> ()
          
        let isMat = board.IsMate()
        if isMat then
          let ply = board.PlyCount
          let lastPosition = board.MovesAndFenPlayed |> Seq.last
          yield lastPosition, ply, pgn.GameMetaData.White, pgn.GameMetaData.Black, pgn.GameMetaData.Result
    ]    

  let parsAllPGNgames path printToConsole =
    let board = Board()
    let games = FullPGNParser.parsePgnFile path |> Seq.truncate 1_000_000
    let mutable numberOfPlys = 0
    let mutable gameIdx = 0
    printfn "Started parsing of PGN file: %s" path
    //let find = "br4k1/p4N2/4pn2/8/2P3P1/q2B3P/1r1Q4/2KR1R2 w"
    for pgn in games do
      gameIdx <- gameIdx + 1
      if gameIdx % 10_000 = 0 then
        printfn "Game number: %d Number of plies: %d " gameIdx numberOfPlys
      if printToConsole then
        printfn "Start of game number: %d\n" gameIdx 
      board.ResetBoardState()
      board.LoadFen(pgn.Fen)
      for m in pgn.Mainline do        
          board.PlaySanMove m.San          
          if printToConsole then
            match Regex.parseEvalRegexOption m.Comment false with
            | Some eval -> 
                if m.Color = "b" then
                  printfn "Black move %s, move number: %d Eval: %f" m.San m.MoveNumber eval
                else
                printfn "White move %s, move number: %d Eval: %f" m.San m.MoveNumber eval
            | None -> ()

    printfn "Number of games played: %d and number of moves played: %d" gameIdx numberOfPlys

  type EvalDetail = { Player: string; Move: string; PonderMove:string; Ply:int; Eval: float; FEN:string; Comment: string }
  type EvalDetailResult = {GameNr: int; Result: string; Players:string; EvalDetail: EvalDetail; Color:string }

  let collectAllChessEvaluations (pgn: PGNTypes.PgnGame) =    
      board.ResetBoardState()
      board.LoadFen(pgn.Fen)    
      [
          for m in pgn.Mainline do
                board.PlaySanMove m.San
                match Regex.parseEvalRegexOption m.Comment false with
                | Some eval ->
                    let pd =
                      match Regex.parsePonderMove m.Comment with
                      | Some ponder -> ponder
                      | None -> ""
                    let comment = sprintf "{%s}" m.Comment
                    let evalDetail = 
                        if m.Color = "w" then
                            {Player = pgn.GameMetaData.White; Move = m.San; PonderMove = pd; Ply = m.Ply; Eval = eval; FEN = board.PositionWithMoves(); Comment = comment }
                        else 
                            {Player = pgn.GameMetaData.Black; Move = m.San; PonderMove = pd ; Ply = m.Ply; Eval = eval;  FEN = board.PositionWithMoves(); Comment = comment }
                    let players = pgn.GameMetaData.White + " vs " + pgn.GameMetaData.Black
                    let evalRes = {Result = pgn.GameMetaData.Result; GameNr = pgn.GameNumber; Players = players; EvalDetail = evalDetail; Color = m.Color}
                    yield evalRes
                | None -> ()            
      ]


  let noFilter = fun _ -> true  

  let msg evalDetailResult =  
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f  \nFEN = %s" 
        evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
        evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
        evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.FEN

  let shortMsg evalDetailResult = 
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f - ponder move: %s" 
      evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
      evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
      evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.PonderMove
  
  let shortMsgWithMoves evalDetailResult = 
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f - ponder move: %s \n%s\n" 
      evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
      evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
      evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.PonderMove evalDetailResult.EvalDetail.FEN

  //get a streamwriter

  //write to file
  let writeToFile (path:string) evalDetailResults =
    use writer = new StreamWriter(path)
    for evalDetailResult in evalDetailResults do
      writer.WriteLine(msg evalDetailResult)

  //append to file
  let appendToFile (path:string) writeRawPGN pgnPath (pgn:PGNTypes.PgnGame) evalDetailResults  =
    use writer = new StreamWriter(path, true)
    writer.WriteLine "---------------------------------------------------------"
    let players = sprintf "%s vs %s" pgn.GameMetaData.White pgn.GameMetaData.Black
    let eventInfo =
      let round = sprintf "Round: %s" pgn.GameMetaData.Round
      if String.IsNullOrEmpty pgn.GameMetaData.Event then
        round
      else
        sprintf "Event: %s %s" pgn.GameMetaData.Event round
    writer.WriteLine(sprintf "%s : (%s) %s \n(In PGN-file: %s)\n\t" eventInfo pgn.GameMetaData.Result players pgnPath)
    let mutable counter = 0
    let lastEval = (evalDetailResults |> Seq.length) - 1
    for evalDetailResult in evalDetailResults do
      if lastEval = counter then
        writer.WriteLine(shortMsgWithMoves evalDetailResult)
      else
        writer.WriteLine(shortMsg evalDetailResult)
      counter <- counter + 1
    if writeRawPGN then
      let comments = evalDetailResults |> Seq.fold (fun acc e -> acc + sprintf "%s played %s with comment: %s" e.EvalDetail.Player e.EvalDetail.Move e.EvalDetail.Comment + "\n") ""
      writer.WriteLine(comments)

  // Function to safely extract pairs and calculate differences

  
  let isPotentialMissedWinForCeres (evalChunks: EvalDetailResult list) (whitePlayer,blackPlayer) threshold =
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
    let isDraw = result = "1/2-1/2"
    if not isDraw then
      false, [], [], String.Empty, String.Empty
    else      
      let highEvalInGame = 
        evalChunks |> List.exists (fun e -> abs e.EvalDetail.Eval > threshold)
      if highEvalInGame then        
        let highestEvalFound = evalChunks |> List.maxBy(fun e -> abs e.EvalDetail.Eval)        
        let lastSixMoves = evalChunks |> List.skip (List.length evalChunks - 6)
        let fourLatestEvalsAroundHighestEvalFound = 
          let indexOfMax = evalChunks |> List.findIndex (fun e -> e = highestEvalFound)
          let evalsBefore = if indexOfMax > 4 then evalChunks.[indexOfMax-3..indexOfMax-1] else evalChunks.[0..indexOfMax-1]
          let evalsAfter = if indexOfMax + 5 < evalChunks.Length then evalChunks.[indexOfMax..indexOfMax+5] else evalChunks[indexOfMax..evalChunks.Length - 1]
          let combined = evalsBefore @ evalsAfter
          combined

        let ply = highestEvalFound.EvalDetail.Ply
        let sideWithAdvantage = if highestEvalFound.EvalDetail.Eval > 0 then "White" else "Black"
        if highestEvalFound.Color = "w" then
          let player = if sideWithAdvantage = "White" then whitePlayer else blackPlayer
          let msg = sprintf "%s (%s) missed a potential win around ply %d: Max eval = %f after move %s" player sideWithAdvantage ply highestEvalFound.EvalDetail.Eval highestEvalFound.EvalDetail.Move
          true, fourLatestEvalsAroundHighestEvalFound, lastSixMoves, msg, player
        else
          let player = if sideWithAdvantage = "White" then whitePlayer else blackPlayer
          let msg = sprintf "%s (%s) missed a potential win around ply nr %d: Max eval = %f after move %s" player sideWithAdvantage ply highestEvalFound.EvalDetail.Eval highestEvalFound.EvalDetail.Move
          true, fourLatestEvalsAroundHighestEvalFound, lastSixMoves, msg, player
      else
        false, [], [], String.Empty, String.Empty

  //let appendMissedWin chunks = appendToFile "C:/Dev/Chess/PGNs/missed_wins.txt" chunks
  //let appendMissedDraw chunks = appendToFile "C:/Dev/Chess/PGNs/missed_draws.txt" chunks
  //let appendMisEvaluatedPosition chunks = appendToFile "C:/Dev/Chess/PGNs/mis_evaluated_positions.txt" chunks
  
  type Result = 
    { 
      Player : string
      MissedWin: bool
      Threshold: float
      MaxEval: float
      EndOfGameEval: float
      TwoFold : bool
      ThreeFold : bool
      FiftyMove : bool
      StaleMate : bool
      InSufficientMaterial : bool
    }
  let createResult player missedWin threshold maxEval endOfGameEval twoFold threeFold fiftyMove staleMate inSufficientMaterial =     
    {
      Player = player
      MissedWin = missedWin
      Threshold = threshold
      MaxEval = maxEval
      EndOfGameEval = endOfGameEval
      TwoFold = twoFold
      FiftyMove = fiftyMove
      ThreeFold = threeFold
      StaleMate = staleMate
      InSufficientMaterial = inSufficientMaterial
    }

  let pgnTerminationSummary path chunkSize threshold (highEvalThreshold, lowEvalThreshold) =
    // Function to calculate maximum length for each column
    let calculateMaxLengths (headers:string list) (rows: string list list) =
        let columns = headers |> List.mapi (fun i _ -> rows |> List.map (fun row -> row.[i]))
        headers
        |> List.mapi (fun i header ->
            // fold, not List.max: a table with no rows is legitimate (a PGN with no games
            // matching the engine-name filter) and must not throw "input list was empty"
            let maxRowLength = columns.[i] |> List.fold (fun acc (s: string) -> max acc s.Length) 0
            max (header.Length) maxRowLength
        )

    // Function to create a formatted table
    let formatTable headers rows =
        let maxLengths = calculateMaxLengths headers rows
        let createBorder =
            maxLengths
            |> List.map (fun len -> "+" + (String.replicate (len + 2) "-"))
            |> String.concat ""
            |> (+) "+"
    
        let formatRow row =
            row
            |> List.mapi (fun i col -> sprintf "| %-*s " maxLengths.[i] col)
            |> String.concat ""
            |> (+) "|"
    
        let header = formatRow headers
        let separator =
            maxLengths
            |> List.map (fun len -> "+" + (String.replicate (len + 2) "-"))
            |> String.concat ""
            |> (+) "+"
    
        let formattedRows = rows |> List.map formatRow
        [createBorder; header; separator] @ formattedRows @ [createBorder]
        |> String.concat "\n"
    
    let allPGNGames = FullPGNParser.parsePgnFile path |> Seq.truncate 1_000_000 |> Seq.toList
    let sb = new System.Text.StringBuilder()
    let appendLine (msg:string) = sb.AppendLine(msg) |> ignore
    let games = allPGNGames |> Seq.length
    
    //initialize a dictionary to store one stringbuilder per player
    let playerDict = new System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()
    let appendLineToPlayer (player:string) (msg:string) =
      let sb = 
        match playerDict.TryGetValue player with
        | true, sb -> sb
        | false, _ -> 
            let sb = new System.Text.StringBuilder()
            playerDict.Add(player, sb)
            sb
      sb.AppendLine(msg) |> ignore

    //set console color to green for the following output
    Console.ForegroundColor <- ConsoleColor.Green
    printfn "\nWorking on file: %s\n" path
    Console.ResetColor()
    let detailDesc = sprintf "Detailed analysis of each missed win game in file %s:\n" (Path.GetFileName path)
    appendLine(detailDesc)
    let results = ResizeArray<Result>()
    let board = new Board()
    for pgn in allPGNGames do
      board.ResetBoardState()
      board.LoadFen pgn.GameMetaData.Fen
      for m in pgn.Mainline do      
          board.PlaySanMove m.San        
      let evals = collectAllChessEvaluations pgn
      let mutable currentCollector = fun _ -> ()
      let w,b = pgn.GameMetaData.White, pgn.GameMetaData.Black
      // This summary is built on engine eval comments; human PGNs (Lichess/chess.com
      // exports) carry none, and the branches below take List.last/maxBy of them.
      if evals.IsEmpty then () else
      match isPotentialMissedWinForCeres evals (w,b) threshold  with
      |true, evals, lastSixMoves, msg, player -> 
          let mEval = (evals |> List.maxBy (fun e -> abs e.EvalDetail.Eval)).EvalDetail.Eval
          let lastEval = abs (lastSixMoves |> Seq.last).EvalDetail.Eval
          let fiftyMove = board.Position.Count50 >= 100uy
          let rep = board.RepetitionNr()
          let insufficientMaterial = board.InsufficientMaterial()         
          let stalemate = board.AnyLegalMove() |> not && board.IsMate() |> not          
          let result = createResult player true threshold mEval lastEval (rep = 2) (rep = 3) fiftyMove stalemate insufficientMaterial
          results.Add(result)
          appendLineToPlayer player (sprintf "\nRound %s: %s vs %s with game result: %s" pgn.GameMetaData.Round pgn.GameMetaData.White pgn.GameMetaData.Black pgn.GameMetaData.Result)
          appendLine (sprintf "\nRound %s: %s vs %s with game result: %s" pgn.GameMetaData.Round pgn.GameMetaData.White pgn.GameMetaData.Black pgn.GameMetaData.Result)
          appendLineToPlayer player (sprintf "\t%s"  msg)
          appendLine (sprintf "\t%s"  msg)          
          //appendLine "---------------------------------------------------------"
          //printfn "%s" pgn.Raw
          //appendLine Environment.NewLine
          for e in evals do
            appendLine (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
            appendLineToPlayer player (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
          appendLine "\nlast 6 moves of the game:"
          appendLineToPlayer player "\nlast 6 moves of the game:"
          for e in lastSixMoves do
            appendLine (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
            appendLineToPlayer player (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
          
          if lastEval > highEvalThreshold && pgn.GameMetaData.Result = "1/2-1/2" then
            if fiftyMove then 
              appendLine (sprintf "\tGame ends in high eval with 50move = %b" fiftyMove)
              appendLineToPlayer player (sprintf "\tGame ends in high eval with 50move = %b" fiftyMove)
            if stalemate then 
              appendLine (sprintf "\tGame ends in high eval with stalemate = %b" stalemate)
              appendLineToPlayer player (sprintf "\tGame ends in high eval with stalemate = %b" stalemate)
            if rep = 3 then 
              appendLine (sprintf "\tGame ends in high eval with three-fold = %b" (rep = 3))
              appendLineToPlayer player (sprintf "\tGame ends in high eval with three-fold = %b" (rep = 3))
            if rep = 2 then 
              appendLine (sprintf "\tGame ends in high eval with two-fold = %b" (rep = 2))
              appendLineToPlayer player (sprintf "\tGame ends in high eval with two-fold = %b" (rep = 2))
            if insufficientMaterial then 
              appendLine (sprintf "\tGame ends in high eval with InsufficientMaterial = %b" insufficientMaterial)            
              appendLineToPlayer player (sprintf "\tGame ends in high eval with InsufficientMaterial = %b" insufficientMaterial)
            
          else            
            appendLine "\tGame ends in low eval"
            appendLineToPlayer player "\tGame ends in low eval"
          //appendLine "---------------------------------------------------------"
          //currentCollector <- collectMissedWins pgn
          //currentCollector evals
      |false, _,_,_,_ -> 
        let lastEval = abs (evals |> List.last).EvalDetail.Eval        
        let maxEval = (evals |> List.maxBy (fun e -> e.EvalDetail.Eval)).EvalDetail.Eval        
        let fiftyMove = board.Position.Count50 >= 100uy
        let rep = board.RepetitionNr()
        let insufficientMaterial = board.InsufficientMaterial()         
        let stalemate = board.AnyLegalMove() |> not && board.IsMate() |> not 
        let whoWon = if pgn.GameMetaData.Result = "1-0" then pgn.GameMetaData.White else pgn.GameMetaData.Black
        let result = createResult whoWon false threshold maxEval lastEval (rep = 2) (rep = 3) fiftyMove stalemate insufficientMaterial
        results.Add(result)    

    let allPlayers =        
      let missedWins = results |> Seq.filter (fun r -> r.MissedWin)
      let totalMissedWins = missedWins |> Seq.length
      let totalGames = results |> Seq.length
      // Every ratio here divides by a count that can legitimately be zero (a PGN with no
      // analyzable games, or none with a missed win) — without this the whole table
      // prints NaN%.
      let ratio part whole = if whole = 0 then 0.0 else (float part / float whole) * 100.0
      let missedWinRatio = ratio totalMissedWins totalGames
      let endEloLower = missedWins |> Seq.filter (fun r -> abs r.EndOfGameEval < lowEvalThreshold) |> Seq.length
      let endEloHigher = missedWins |> Seq.filter (fun r -> abs r.EndOfGameEval > highEvalThreshold) |> Seq.length
      let lowEloRatio = ratio endEloLower totalMissedWins
      let highEloRatio = ratio endEloHigher totalMissedWins
      let allTwoFold = results |> Seq.filter (fun r -> r.TwoFold) |> Seq.length
      let twoFoldRatio = ratio allTwoFold totalGames
      let allThreeFold = results |> Seq.filter (fun r -> r.ThreeFold) |> Seq.length
      let threeFoldRatio = ratio allThreeFold totalGames
      let fiftyMoveRule = results |> Seq.filter (fun r -> r.FiftyMove) |> Seq.length
      let fiftyMoveRuleRatio = ratio fiftyMoveRule totalGames
      let staleMate = results |> Seq.filter (fun r -> r.StaleMate) |> Seq.length
      let staleMateRatio = ratio staleMate totalGames
      let insufficientMaterial = results |> Seq.filter (fun r -> r.InSufficientMaterial) |> Seq.length
      let insufficientMaterialRatio = ratio insufficientMaterial totalGames
      let uniquePlayers = results |> Seq.map (fun r -> r.Player) |> Seq.filter(fun p -> p <> "") |> Seq.distinct
      let totalMissedWinsPerPlayer = 
        [for p in uniquePlayers do
          let missedWins = results |> Seq.filter (fun r -> r.Player = p && r.MissedWin)
          let totalMissedWins = missedWins |> Seq.length
          p, totalMissedWins]
        
      //get games that ends in high evals and is not stalemate
      //let highEvalNotStaleMate = results |> Seq.filter (fun r -> abs r.EndOfGameEval >= highEvalThreshold && not r.StaleMate)
      
      let headers = ["Metric"; "Count"; "Percentage"]  
      
      let rows = [
          ["Threshold for missed win in cp"; sprintf "%.1f" (threshold*100.0); "-"]
          // `results` only holds games that carried engine evals, so this is the
          // denominator for every percentage below — not the file's game count.
          ["Games analyzed (with evals)"; sprintf "%d" totalGames; "-"]
          ["Number of Missed wins"; sprintf "%d" totalMissedWins; sprintf "%.2f%%" missedWinRatio]
          for (p,missed) in totalMissedWinsPerPlayer do
            [sprintf "  Missed win for %s" p; sprintf "%d" missed; sprintf "%.2f%%" (ratio missed totalMissedWins) ]
          [sprintf "  Missed win ended with evals < %.1f" lowEvalThreshold; sprintf "%d" endEloLower; sprintf "%.2f%%" lowEloRatio]
          [sprintf "  Missed win ended with evals > %.1f" highEvalThreshold; sprintf "%d" endEloHigher; sprintf "%.2f%%" highEloRatio]
          ["Games ended with Two-fold repetition"; sprintf "%d" allTwoFold; sprintf "%.2f%%" twoFoldRatio]
          ["Games ended with Three-fold repetition"; sprintf "%d" allThreeFold; sprintf "%.2f%%" threeFoldRatio]
          ["Games ended in stalemate"; sprintf "%d" staleMate; sprintf "%.2f%%" staleMateRatio]
          ["Games with fifty-move rule adjudication"; sprintf "%d" fiftyMoveRule; sprintf "%.2f%%" fiftyMoveRuleRatio]
          ["Games with insufficient material"; sprintf "%d" insufficientMaterial; sprintf "%.2f%%" insufficientMaterialRatio]
      ]

      let summary = sprintf "```\nSummary of the analysis for all games in PGN:\n" + formatTable headers rows + "\n```"
      printfn "%s" summary      
      games, summary, sb.ToString()    
    [allPlayers]

  let gameAnalysisFromFolderAndSubFolder folderPath chunkSize threshold (highEvalThreshold, lowEvalThreshold) =
    //create a file to write the results to - that appends the results
    let writer = new StreamWriter("C:/Dev/Chess/PGNs/evals.txt")
    
    let files = Directory.EnumerateFiles(folderPath, "*.pgn", SearchOption.AllDirectories)
    let mutable games = 0
    
    let desc = 
        "\nDescription:\n" + 
        $"1. The threshold for a missed win is set to {threshold} in eval.\n" +
        "2. The missed win percentage is calculated as (total missed wins / total games) * 100.\n" +
        $"3. The end-of-game evaluations lower than {lowEvalThreshold} and higher than {highEvalThreshold} are calculated by filtering the results.\n" +
        "4. The two/three-fold repetition percentage is calculated as (total two-fold+ repetitions / total games) * 100.\n" +
        "5. When a game ends with high eval it is almost certain to be because of two fold repetitions.\n" +
        "6. Missed wins can occur naturally because of suboptimal play during game with low eval at the end - which is fine.\n" +
        "7. The fifty-move rule percentage is calculated as (total games with fifty-move rule adjudication / total games) * 100.\n" +
        "8. The stalemate percentage is calculated as (total games that ends in stalemate / total games) * 100.\n" +
        "9. The insufficient material percentage is calculated as (total games with insufficient material / total games) * 100.\n"

    printfn "%s" desc
    writer.WriteLine(desc)
    writer.WriteLine("---------------------------------------------------------")
    for file in files do        
        writer.WriteLine($"Working on file: {file}")
        //writer.WriteLine("---------------------------------------------------------\n")
        
        for numberOfGames, summary, detailSummary in pgnTerminationSummary file chunkSize threshold (highEvalThreshold, lowEvalThreshold) do
            writer.WriteLine(summary)        
            writer.WriteLine(detailSummary)
            writer.WriteLine("---------------------------------------------------------")
            games <- games + numberOfGames
        
    writer.Flush()
    printfn "Total number of games analyzed: %d" games

let removeEPFensInPGNFile (pgnPath:string) =
  let games = FullPGNParser.parsePgnFile pgnPath |> Seq.toList
  let mutable gameNr = 0  
  let board = new Board()
  let list =
      [for pgn in games do      
          gameNr <- gameNr + 1
          board.ResetBoardState()
          board.LoadFen pgn.GameMetaData.Fen
          if board.Position.EnPassant = 8uy then  
            yield pgn
      ]
  list, games.Length
