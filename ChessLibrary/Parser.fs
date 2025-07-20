module ChessLibrary.Parser

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open TypesDef
open TypesDef.PGNTypes
open LowLevelUtilities
open TypesDef.Engine
open TypesDef.CoreTypes
open TypesDef.TMove
open EPD

module PGNHelper =
  
  let ensureDirectoryExists (filePath: string) =
      let directory = Path.GetDirectoryName(filePath)
      if not (Directory.Exists(directory)) then
          Directory.CreateDirectory(directory) |> ignore

  let writeAndCorrectRoundHeadersInShuffleFile (filePath: string) (games: PgnGame seq) =
    let mutable idx = 0
    for game in games do
        idx <- idx + 1
        let newRaw =            
            if Regex.IsMatch(game.Raw, @"\[Round\s*""[^""]*""\]") then
                Regex.Replace(
                    game.Raw, 
                    @"\[Round\s*""[^""]*""\]", 
                    sprintf "[Round \"%d.%d\"]" idx 1)
            else
                sprintf "[Round \"%d.%d\"]\n%s" idx 1 game.Raw
     
        File.AppendAllText(filePath, newRaw)  
  
  let shufflePgnGames (filePath: string) (games: PgnGame seq) =
    try 
        let gamesArray = games |> Seq.toArray 
        Random.Shared.Shuffle gamesArray
        ensureDirectoryExists filePath

        if File.Exists filePath then
            File.Delete filePath
        writeAndCorrectRoundHeadersInShuffleFile filePath gamesArray
    with 
        | :? System.UnauthorizedAccessException as ex ->
            printfn "Error: %s" ex.Message
            printfn "Please close the file and try again."
        | :? System.IO.IOException as ex ->
            printfn "Error: %s" ex.Message
            printfn "Please close the file and try again."
        | ex -> 
            printfn "An unexpected error occurred: %s" ex.Message
  

  let extractFen (input: string) (idx : int) =
      let parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) 
      let ceresNetPrediction = parts.[0]
      let tablebaseCorrectAnswer = parts.[1]
      let msg = sprintf "Ceres: %s, TB: %s" ceresNetPrediction tablebaseCorrectAnswer
      let rest = String.concat " " parts[2..]        
      let entry =
        { RawInput = input; FEN = rest; BestMove = None; AvoidMove = None; Id = sprintf "Id: %d" idx |> Some  ; Other = Some msg }
      { 
        EPD = entry
        TBAnswer = Int32.Parse tablebaseCorrectAnswer
        QAnswer = Int32.Parse ceresNetPrediction 
        Move = String.Empty
      }
  let readTableBaseResults (filePath:string) =
    let content = File.ReadAllLines filePath
    let mutable nr = 0
    seq { for line in content do 
            nr <- nr + 1
            if line.Contains "BATCH" |> not then 
              extractFen line nr           
       }
  /// Gets the opening information from a PGN game.
  let private fallbackOpeningInfo (opening: Header option) (fen: string) (gameNumber: int) =
    match opening with
    | Some op -> sprintf "Nr %i: %s" gameNumber op.Value
    | None when String.IsNullOrEmpty fen -> sprintf "Nr %i: No opening name" gameNumber
    | None -> sprintf "Nr %i: %s" gameNumber fen

  let private formatOpeningInfo
    (prefix: string option)
    (opening: Header option)
    (variation: Header option)
    (eco: Header option)
    (gameNumber: int)
    (fen: string)
    =
    match opening, variation, eco with
    | Some op, Some v, Some eco -> sprintf "%s%s - %s, ECO: %s" (prefix |> Option.defaultValue "") op.Value v.Value eco.Value
    | Some h, Some v, None -> sprintf "%s%s - %s" (prefix |> Option.defaultValue "") h.Value v.Value
    | Some h, None, Some eco -> sprintf "%s%s, ECO: %s" (prefix |> Option.defaultValue "") h.Value eco.Value
    | None, None, Some eco -> sprintf "%sECO: %s" (prefix |> Option.defaultValue "") eco.Value
    | Some op, None, None -> sprintf "%s%s" (prefix |> Option.defaultValue "") op.Value
    | _ -> fallbackOpeningInfo opening fen gameNumber

  let getOpeningInfo (game: PgnGame) =
    let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
    let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
    let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
    formatOpeningInfo (Some "Opening: ") opening variation eco game.GameNumber game.Fen

  let getOpeningInfoOnly (game: PgnGame) =
    let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
    let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
    let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
    formatOpeningInfo None opening variation eco game.GameNumber game.Fen

  let getOpeningOnly (meta: GameMetadata) =
    let opening =
      meta.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
      |> Option.orElseWith (fun () ->
        if not (String.IsNullOrEmpty meta.OpeningName) then Some { Key = "Opening"; Value = meta.OpeningName }
        else None)
    let variation = meta.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
    let eco = meta.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
    match opening, variation, eco with
    | Some op, Some v, Some eco -> sprintf "%s - %s, ECO: %s" op.Value v.Value eco.Value
    | Some h, Some v, None -> sprintf "%s - %s" h.Value v.Value
    | Some h, _, Some eco -> sprintf "%s, ECO: %s" h.Value eco.Value
    | Some h, _, _ -> sprintf "%s" h.Value
    | None, None, Some eco -> sprintf "ECO: %s" eco.Value
    | _ ->
      match opening with
      | Some op -> op.Value
      | None when String.IsNullOrEmpty meta.Fen -> sprintf "Round %s: No opening name" meta.Round
      | None -> sprintf "Round %s: Fen %s:" meta.Round meta.Fen
  

module PGNExtractor =

  /// Extracts moves from a PGN game.
  /// <param name="game">The PGN game.</param>
  /// <returns>A tuple containing the moves and game metadata.</returns>
  let extractMoves (game:PgnGame) =
    let moves =
      [|for move in game.Moves do
         yield true, game.GameMetaData.White, move.MoveNr, move.WhiteSan
         yield false, game.GameMetaData.Black, move.MoveNr, move.BlackSan |]
    moves, game.GameMetaData

  
  /// Extracts engine book exit statistics from a PGN game.
  /// <param name="game">The PGN game.</param>
  /// <returns>The engine book exit statistics.</returns>
  let extractEngineBookExitStats (game:PgnGame) =
    let mutable foundWhiteMove = false
    let mutable foundBlackMove = false
    let moves =
      [|for moves in game.Moves do
         let white = Annotation.getEngineStatData game.GameMetaData.White false moves.WhiteComment
         let black = Annotation.getEngineStatData game.GameMetaData.Black true moves.BlackComment
         if (not foundWhiteMove && white.n > 0) then
           foundWhiteMove <- true
           yield white
         if (not foundBlackMove && black.n > 0) then
           foundBlackMove <- true
           yield black
      |]
    {White = game.GameMetaData.White; Black = game.GameMetaData.Black; Moves = moves}
  
  /// Extracts all book exit engine statistics from a sequence of PGN games.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine book exit statistics.</returns>
  let extractAllBookExitEngineStatsInPGN (games: PgnGame seq) = 
    games 
    |> Seq.map extractEngineBookExitStats
    |> Seq.toArray

  /// Extracts engine statistics from a PGN game.
  /// <param name="game">The PGN game.</param>
  /// <returns>The engine statistics.</returns>
  let extractEngineStats (game:PgnGame) =
    let moves =
      [|for moves in game.Moves do
         yield Annotation.getEngineStatData game.GameMetaData.White false moves.WhiteComment
         yield Annotation.getEngineStatData game.GameMetaData.Black true moves.BlackComment |]
    {White = game.GameMetaData.White; Black = game.GameMetaData.Black; Moves = moves}

  /// Extracts all engine statistics from a sequence of PGN games.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics.</returns>
  let extractAllEngineStatsInPGN (games: PgnGame seq) = 
    games 
    |> Seq.map extractEngineStats
    |> Seq.toArray

  /// Extracts all engine moves from an array of engine statistics.
  /// <param name="games">The array of engine statistics.</param>
  /// <returns>An array of engine move statistics.</returns>
  let extractAllEngineMovesInPGN (games: EngineStat array) =
    [| for game in games -> game.Moves |]
    |> Array.concat

module PGNStatistics =
  /// Calculates the median of an array of floats.
  let median (nums: float array) =
    match nums.Length with
    | 0 -> 0.0
    | len ->
        let sorted = Array.sort nums
        if len % 2 = 0 then
            let i = len / 2
            (sorted.[i - 1] + sorted.[i]) / 2.0
        else
            sorted.[len / 2]

  /// Gets all engine moves for a player.
  /// <param name="name">The name of the player.</param>
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>An array of engine move statistics for the player.</returns>
  let getAllEngineMovesForPlayer name (moves: EngineMoveStat array) =
    moves |> Array.filter (fun e -> e.Player = name)  

  /// Calculates the median number of nodes.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The median number of nodes.</returns>
  let calculateMedianNodes (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.n > 0)
      |> Array.map (fun e -> float e.n)
      |> Array.sortBy id
      |> median

  /// Calculates the median nodes per second (NPS).
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The median NPS.</returns>
  let calculateMedianNps (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.s > 0)
      |> Array.map (fun e -> float e.s)
      |> Array.sortBy id
      |> median

  /// Calculates the median depth.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The median depth.</returns>
  let calculateMedianDepth (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.d > 0)
      |> Array.map (fun e -> float e.d)
      |> Array.sortBy id
      |> median

  /// Calculates the median self-depth.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The median self-depth.</returns>
  let calculateMedianSelfdepth (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.sd > 0)
      |> Array.map (fun e -> float e.sd)
      |> Array.sortBy id
      |> median

  /// Calculates the median move time.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The median move time.</returns>
  let calculateMedianMoveTime (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.mt > 0)
      |> Array.map (fun e -> float e.mt)
      |> Array.sortBy id
      |> median
  
  /// Extracts time and nodes for a player.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>An array of tuples containing the move time and nodes.</returns>
  let extractTimeAndNodesForPlayer (moves: EngineMoveStat array) =
    moves 
    |> Array.filter (fun e -> e.mt > 0 && e.s > 0)
    |> Array.map (fun e -> (e.mt, e.n))

  /// Calculates the average NPS using a simple method.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average NPS.</returns>
  let calculateAvgNpsSimple (moves: EngineMoveStat array) = 
    moves 
    |> Array.map (fun e -> float e.s)
    |> Array.filter (fun e -> e > 0)
    |> Array.average

  /// Debugs the engine move statistics.
  /// <param name="moves">The array of engine move statistics.</param>
  let debug (moves: EngineMoveStat array) = 
    let sorted = moves |> Array.sortBy (fun e -> e.s)
    for m in sorted do
      let calcAvg = if m.mt = 0L then 0L else (m.n / m.mt) * 1000L
      printfn "Player: %s - NPS: %d, Nodes: %d, Ms: %d CalcNps: %d" m.Player m.s m.n m.mt calcAvg
    printfn "Done with all moves"

  //Calculate the Interquartile Range (IQR).
  //Interquartile Range (IQR) is a measure of statistical dispersion, which is the spread or variability of a dataset. 
  //The IQR is the range between the first quartile (25th percentile) and the third quartile (75th percentile) of a dataset. 
  //In other words, it represents the middle 50% of the data.
  //Determine the lower and upper bounds for outliers.
  //Filter out the outliers from the dataset.
  //Calculate the average of the remaining values.  

  let n1Map (stat : EngineMoveStat) = stat.n1
  let n2Map (stat : EngineMoveStat) = stat.n2
  let q1Map (stat : EngineMoveStat) = stat.q1
  let q2Map (stat : EngineMoveStat) = stat.q2
  let npsMap (stat : EngineMoveStat) = stat.s
  let nodeMap (stat : EngineMoveStat) = stat.n
  let depthMap (stat : EngineMoveStat) = int64 stat.d
  let sdMap (stat : EngineMoveStat) = int64 stat.sd

  /// Calculates the average NPS without outliers.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <param name="mapping">The mapping function to extract the relevant value from the statistics.</param>
  /// <returns>The average NPS without outliers.</returns>
  let averageNpsWithoutOutliers (moves: EngineMoveStat array) mapping =
    let sortedNps = 
      moves 
      |> Array.map mapping
      |> Array.filter (fun m -> m > 0L)
      |> Array.sort

    if sortedNps.Length < 4 then
      0.0
    else
      let q1, q3 =
          let len = sortedNps.Length
          if len % 2 = 0 then
              (sortedNps[(len / 4)], sortedNps[(3 * len / 4)])
          else
              (sortedNps[((len - 1) / 4)], sortedNps[ (3 * (len + 1) / 4)])

      let iqr = q3 - q1
      let lowerBound = float q1 - 1.5 * float iqr
      let upperBound = float q3 + 1.5 * float iqr

      let noOutliers = sortedNps |> Array.filter (fun x -> float x >= lowerBound && float x <= upperBound)
      let sum = noOutliers |> Array.fold (fun acc x -> acc + float x) 0.0 
      let count = Array.length noOutliers

      if count > 0 then
          sum / (float count)
      else
          0.0
  
  /// Calculates the average NPS.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average NPS.</returns>
  let calculateAvgNPS (moves: EngineMoveStat array) = 
    let data = extractTimeAndNodesForPlayer moves
    let totalTimeMs = data |> Array.sumBy fst
    let totalNodes = data |> Array.sumBy snd
    let ratio = 
      if totalTimeMs = 0L then 
        0L 
      else 
        (totalNodes / totalTimeMs) * 1000L
    ratio

  /// Calculates the average depth.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average depth.</returns>
  let calculateAvgDepth (moves: EngineMoveStat array) = 
      moves 
      |> Array.map (fun e -> float e.d)
      |> Array.filter (fun e -> e > 0)
      |> Array.average
  
  /// Calculates the average self-depth.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average self-depth.</returns>
  let calculateAvgSelfdepth (moves: EngineMoveStat array) = 
    let filtered =
      moves 
      |> Array.map (fun e -> float e.sd)
      |> Array.filter (fun e -> e > 0)
    if filtered.Length = 0 then
      0.0
    else
      filtered |> Array.average

  /// Calculates the average move time.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average move time.</returns>

  let calculateAvgMoveTime (moves: EngineMoveStat array) = 
      match moves |> Seq.exists(fun e -> e.mt > 0) with
      |true ->
        moves 
        |> Array.map (fun e -> float e.mt)
        |> Array.filter (fun e -> e > 0)
        |> Array.average
      |_ -> 0.0

  /// Calculates the average N1.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average N1.</returns>      
  let calculateAvgN1 (moves: EngineMoveStat array) = 
      let calc =
        moves 
        |> Array.map (fun e -> float e.n1)
        |> Array.filter (fun e -> e > 0)
      if Array.isEmpty calc then 0.0 else calc |> Array.average
  
  /// Calculates the average N2.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average N2.</returns>
  let calculateAvgN2 (moves: EngineMoveStat array) = 
      let calc =
        moves 
        |> Array.map (fun e -> float e.n2)
        |> Array.filter (fun e -> e > 0)
      if Array.isEmpty calc then 0.0 else calc |> Array.average
  
  /// Calculates the average top P.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average top P.</returns>
  let calculateAvgTopP (moves: EngineMoveStat array) = 
      //let test = moves |> Array.filter (fun e -> abs e.wv > 0.5 && abs e.wv < 1.5)
      let moves = moves |> Array.filter (fun e -> e.n > 0 && abs e.wv < 5.0)
      let pMoves = moves |> Array.sumBy (fun e -> if e.p1 = e.pt then 1 else 0)
      float pMoves / float moves.Length
  
  /// Calculates the average number of nodes.
  /// <param name="moves">The array of engine move statistics.</param>
  /// <returns>The average number of nodes.</returns>
  let calculateAvgNodes (moves: EngineMoveStat array) = 
    moves 
    |> Array.filter (fun e -> e.n > 0L)
    |> Array.map (fun e -> float e.n)
    |> Array.average
  
  /// Calculates the number of games for a player.
  /// <param name="player">The player name.</param>
  /// <param name="moves">The array of engine statistics.</param>
  /// <returns>The number of games.</returns>
  let calcNumberOfGames player (moves: EngineStat array) = 
    moves
    |> Array.filter (fun e -> e.White = player || e.Black = player)
    |> Array.length
  
  /// Gets the players from a sequence of PGN games.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of player names.</returns>  
  let getPlayersFromPGN (games:PgnGame seq) =
    let white = 
      games 
      |> Seq.distinctBy (fun e -> e.GameMetaData.White)
      |> Seq.map (fun e -> e.GameMetaData.White)

    let black = 
      games 
      |> Seq.distinctBy (fun e -> e.GameMetaData.Black)
      |> Seq.map (fun e -> e.GameMetaData.Black)

    Seq.append white black |> Seq.distinct |> Seq.toArray
  
  /// Calculates the node ratio per move per game.
  /// <param name="maxMoves">The maximum number of moves.</param>
  /// <param name="game">The PGN game.</param>
  /// <returns>An array of search data.</returns>  
  let calculateNodeRatioPerMovePerGame maxMoves (game:PgnGame) =
    let players = [|game.GameMetaData.White; game.GameMetaData.Black|]
    let allGames = PGNExtractor.extractAllEngineStatsInPGN [game]
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    [|
      for p in players do
        let moves = getAllEngineMovesForPlayer p allMoves |> Array.truncate maxMoves
        let ok = moves |> Array.exists (fun e -> e.n1 > 0)
        if ok then
          let mutable nr = 0
          for move in moves do
            nr <- nr + 1
            if move.n1 > 0 then
              let n1 = float move.n1
              let n2 = float move.n2
              let nodes = float move.n
              //let timeF = float move.mt / float move.tl
              {
                Player = p
                GameNr = nr
                Navg = nodes
                N1avg = n1
                N2avg = n2
                Q1 = move.q1
                Q2 = move.q2
                FractN1N = n1 / nodes
                FractN2N = n2 / nodes
                FractN2N1 = n2 / n1
                MoveTimeMs = move.mt
                TimeLeftMs = move.tl
                TopPMovePercent = 0.0
              }
    |]
  
  /// Calculates the node ratio per move per game in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <param name="maxMoves">The maximum number of moves.</param>
  /// <returns>An array of search data.</returns>  
  let calculateNodeRatioPerMovePerGameInPgnFile (games:PgnGame seq, maxMoves: int) =
    games
    |> Seq.toArray
    |> Array.Parallel.map (calculateNodeRatioPerMovePerGame maxMoves)
    |> Array.concat

  /// Calculates the node ratio per game.
  /// <param name="maxMoves">The maximum number of moves.</param>
  /// <param name="game">The PGN game.</param>
  /// <returns>An array of search data.</returns>
  let calculateNodeRatioPerGame maxMoves (game:PgnGame) =
    let players = [|game.GameMetaData.White; game.GameMetaData.Black|]
    let allGames = PGNExtractor.extractAllEngineStatsInPGN [game]
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves |> Array.truncate maxMoves
        let ok = moves |> Array.exists (fun e -> e.n > 0)
        if not ok then
          SearchData.Empty
        else
          let avg = averageNpsWithoutOutliers moves
          let nodesAvg = avg nodeMap
          let avgMs = Convert.ToInt64 (calculateAvgMoveTime moves)
          let n1Avg = calculateAvgN1 moves
          let n2Avg = calculateAvgN2 moves
          let topP = calculateAvgTopP moves
          //let nodesAvg = calculateAvgNodes moves

          {
            Player = p
            GameNr = game.GameNumber
            Navg = nodesAvg
            N1avg = n1Avg
            N2avg = n2Avg
            Q1 = 0.0
            Q2 = 0.0
            FractN1N = n1Avg / nodesAvg
            FractN2N = n2Avg / nodesAvg
            FractN2N1 = n2Avg / n1Avg
            MoveTimeMs = avgMs
            TimeLeftMs = 0
            TopPMovePercent = topP
          })

  /// Calculates the node ratio per game in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <param name="maxMoves">The maximum number of moves.</param>
  /// <returns>An array of search data.</returns>
  let calculateNodeRatioPerGameInPgnFile (games:PgnGame seq, maxMoves: int) =
    games
    |> Seq.toArray
    |> Array.Parallel.map (calculateNodeRatioPerGame maxMoves)
    |> Array.concat

  /// Calculates the node ratio per PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of search data.</returns>
  let calculateNodeRatioPerPGNfile (games:PgnGame seq) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let hasMoves = moves |> Array.exists (fun e -> e.n > 0)
        if not hasMoves then
          SearchData.Empty
        else
          let avg = averageNpsWithoutOutliers moves
          let nodesAvg = avg nodeMap
          let avgMT = Convert.ToInt64 (calculateAvgMoveTime moves)
          let n1Avg = calculateAvgN1 moves
          let n2Avg = calculateAvgN2 moves
          let topP = calculateAvgTopP moves
          //let nodesAvg = calculateAvgNodes moves
          {
            Player = p
            GameNr = 0
            Navg = nodesAvg
            N1avg = n1Avg
            N2avg = n2Avg
            Q1 = 0.0
            Q2 = 0.0
            FractN1N = n1Avg / nodesAvg
            FractN2N = n2Avg / nodesAvg
            FractN2N1 = n2Avg / n1Avg
            MoveTimeMs = avgMT
            TimeLeftMs = 0
            TopPMovePercent = topP
          })  


  /// Calculates the book exit median and average speed per game.
  /// <param name="game">The PGN game.</param>
  /// <returns>An array of engine statistics per game.</returns>
  let calculateBookExitMedianAndAvgSpeedPerGame (game:PgnGame) =
    let players = [|game.GameMetaData.White; game.GameMetaData.Black|]
    let allGames = PGNExtractor.extractAllBookExitEngineStatsInPGN [game]
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let ok = moves |> Array.exists (fun e -> e.n > 0)
        if not ok then
          EngineStatsPerGame.Empty
        else
          let move = Array.head moves          
          {
            Player=p
            GameNr=game.GameNumber
            AvgNps= npsMap move |> float
            MedianNps= npsMap move |> float
            AvgNodes= nodeMap move |> float
            MedianNodes= nodeMap move |> float
            AvgDepth= depthMap move |> float
            MedianDepth=depthMap move |> float
            AvgSD= sdMap move |> float
            MedianSD= sdMap move |> float  })

  /// Calculates the median and average speed per game.
  /// <param name="game">The PGN game.</param>
  /// <returns>An array of engine statistics per game.</returns>
  let calculateMedianAndAvgSpeedPerGame (game:PgnGame) =
    let players = [|game.GameMetaData.White; game.GameMetaData.Black|]
    let allGames = PGNExtractor.extractAllEngineStatsInPGN [game]
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let ok = moves |> Array.exists (fun e -> e.n > 0)
        if not ok then
          EngineStatsPerGame.Empty
        else
          let avg = averageNpsWithoutOutliers moves
          let npsMed = calculateMedianNps moves
          let nodesMed = calculateMedianNodes moves
          let depthMed = calculateMedianDepth moves
          let sdMed = calculateMedianSelfdepth moves
          let sdMed = max sdMed depthMed
          let npsAvg = avg npsMap
          let nodesAvg = avg nodeMap
          let depthAvg = avg depthMap
          let sdAvg = max depthAvg (avg sdMap)
          {
            Player=p
            GameNr=game.GameNumber
            AvgNps=npsAvg
            MedianNps=npsMed
            AvgNodes=nodesAvg
            MedianNodes=nodesMed
            AvgDepth=depthAvg
            MedianDepth=depthMed
            AvgSD=sdAvg
            MedianSD=sdMed  })

  /// Calculates the median and average speed per game in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics per game.</returns>  
  let calculateMedianAndAvgSpeedPerGameInPgnFile (games:PgnGame seq) =
    games
    |> Seq.toArray
    |> Array.Parallel.map calculateMedianAndAvgSpeedPerGame
    |> Array.concat

  /// Calculates the book exit median and average speed per game in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics per game.</returns>
  let calculateBookExitMedianAndAvgSpeedPerGameInPgnFile (games:PgnGame seq) =
    games
    |> Seq.toArray
    |> Array.Parallel.map calculateBookExitMedianAndAvgSpeedPerGame
    |> Array.concat

  /// Calculates the median and average speed summary in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics summary.</returns> 
  let calculateMedianAndAvgSpeedSummaryInPgnFile (games:PgnGame seq) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let avg = averageNpsWithoutOutliers moves
        let npsMed = calculateMedianNps moves
        let nodesMed = calculateMedianNodes moves
        let depthMed = calculateMedianDepth moves
        let sdMed = calculateMedianSelfdepth moves
        let moveTime = calculateMedianMoveTime moves
        let sdMed = max sdMed depthMed
        let npsAvg = avg npsMap 
        let nodesAvg = avg nodeMap 
        let depthAvg = avg depthMap 
        let sdAvg = max depthAvg (avg sdMap)
        let games = calcNumberOfGames p allGames
        [|
          {Player=p; Median=true; AvgNPS=npsMed; Games=games; AvgDepth=depthMed; AvgNodes=nodesMed; AvgSelfDepth=sdMed; Time = moveTime |> int64}
          {Player=p; Median = false; AvgNPS=npsAvg; Games=games; AvgDepth=depthAvg; AvgNodes=nodesAvg; AvgSelfDepth=sdAvg; Time = moveTime |> int64}          
        |]) |> Array.concat

  /// Calculates the median and average book exit speed summary in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics summary.</returns>
  let calculateMedianAndAvgBookExitSpeedSummaryInPgnFile (games:PgnGame seq) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllBookExitEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let avg = averageNpsWithoutOutliers moves
        let npsMed = calculateMedianNps moves
        let nodesMed = calculateMedianNodes moves
        let depthMed = calculateMedianDepth moves
        let sdMed = calculateMedianSelfdepth moves
        let moveTime = calculateMedianMoveTime moves
        let sdMed = max sdMed depthMed
        let npsAvg = avg npsMap 
        let nodesAvg = avg nodeMap 
        let depthAvg = avg depthMap 
        let sdAvg = max depthAvg (avg sdMap)
        let games = calcNumberOfGames p allGames
        [|
          {Player=p; Median=true; AvgNPS=npsMed; Games=games; AvgDepth=depthMed; AvgNodes=nodesMed; AvgSelfDepth=sdMed; Time = moveTime |> int64}
          {Player=p; Median = false; AvgNPS=npsAvg; Games=games; AvgDepth=depthAvg; AvgNodes=nodesAvg; AvgSelfDepth=sdAvg; Time = moveTime |> int64}          
        |]) |> Array.concat

  /// Calculates the median speed for all players in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics per player.</returns>  
  let calculateMedianSpeedForAllPlayersInPgnFile (games:PgnGame seq) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let nps = calculateMedianNps moves
        let nodes = calculateMedianNodes moves
        let depth = calculateMedianDepth moves
        let sd = calculateMedianSelfdepth moves
        let moveTime = calculateMedianMoveTime moves
        let sd = max sd depth
        let games = calcNumberOfGames p allGames
        {Player=p; Median = true; AvgNPS=nps; Games=games; AvgDepth=depth; AvgNodes=nodes; AvgSelfDepth=sd; Time = moveTime |> int64})

  /// Calculates the average speed for all players in a PGN file.
  /// <param name="games">The sequence of PGN games.</param>
  /// <returns>An array of engine statistics per player.</returns>
  let calculateAvgSpeedForAllPlayersInPgnFile (games:PgnGame seq) =
    let players = getPlayersFromPGN games    
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let avg = averageNpsWithoutOutliers moves
        let nps = avg npsMap
        let nodes = avg nodeMap
        let depth = avg depthMap
        let sd = max depth (avg sdMap)
        let games = calcNumberOfGames p allGames
        {Player=p; Median = false; AvgNPS=nps; Games=games; AvgDepth=depth; AvgNodes=nodes; AvgSelfDepth=sd; Time = 0L})

module PGNWriter =
  /// Gets the results from a PGN game.
  /// <param name="game">The PGN game.</param>
  /// <returns>The game result.</returns>
  let getResultsFromPGNGame (game:PgnGame) =

    let data = game.GameMetaData
    {
      Player1 = data.White
      Player2 = data.Black
      Moves = data.Moves
      Result = data.Result
      Reason = data.Reason
      GameTime = data.GameTime
    }
  
  /// Writes the opening PGN moves.
  /// <param name="moves">The sequence of moves.</param>
  /// <returns>A string containing the opening PGN moves.</returns>
  let writeOpeningPGNMoves (moves: string seq) =
    let sb = StringBuilder()
    let write (txt : string) = sb.Append txt |> ignore
    let annotation = "{book, mb=+0+0+0+0+0,}"
    
    // Write SAN moves as pairs of white-black moves separated by spaces
    let rec loop i ms idx =
        match ms with
        | [] -> ()  // Return unit when done
        | [m:string] ->
            // Write last move without pair number
            write $"{i}. {m} {annotation}"
        | m1::m2::m3::ms' ->    
            write $"{i}. {m1} {annotation} {m2} {annotation} "
            // Call loop function with next pair number and remaining moves
            loop (i+1) (m3::ms') (idx+2)
        | m1::m2::ms' ->    
            write $"{i}. {m1} {annotation} {m2} {annotation}"
            // Call loop function with next pair number and remaining moves
            loop (i+1) ms' (idx+2) 

    // Call loop function with initial pair number 1
    loop 1 (moves |> Seq.toList) 0
    sb.ToString()

  /// Writes a minimal PGN file.
  /// <param name="pgnGames">The sequence of PGN games.</param>
  /// <param name="filename">The filename.</param>  
  let writeMinimalPgnFile (pgnGames: PgnGame seq) (filename: string) = 
    // Create a new text file using StreamWriter    
    use writer = new StreamWriter(filename, append=false)
    printfn "Created pgn-file at this location: %s" filename
    
    for g in pgnGames do
      writer.WriteLine(sprintf "[White \"%s\"]" g.GameMetaData.White)
      writer.WriteLine(sprintf "[Black \"%s\"]" g.GameMetaData.Black)
      writer.WriteLine(sprintf "[Event \"%s\"]" g.GameMetaData.Event)
      writer.WriteLine(sprintf "[Round \"%s\"]" g.GameMetaData.Round)
      writer.WriteLine(sprintf "[Site \"%s\"]" g.GameMetaData.Site)
      writer.WriteLine(sprintf "[Date \"%s\"]" g.GameMetaData.Date)
      writer.WriteLine(sprintf "[Result \"%s\"]" g.GameMetaData.Result)
      if String.IsNullOrEmpty (g.GameMetaData.Fen) |> not then
        writer.WriteLine(sprintf "[FEN \"%s\"]" g.GameMetaData.Fen)
      for tags in g.GameMetaData.OtherTags do
        writer.WriteLine(sprintf "[%s \"%s\"]" tags.Key tags.Value)        
        // Write an empty line after tags     
      writer.WriteLine()
      let mutable ply = 1
      
      for m in g.Moves do        
        writer.Write(sprintf "%d.%s " ply m.WhiteSan)
        ply <- ply + 1        
        writer.Write(sprintf "%s " m.BlackSan)            
      
      // Write SAN moves as pairs of white-black moves separated by spaces     
      writer.Write("*")
      writer.WriteLine(Environment.NewLine)    
  
    writer.Close()
  
  /// Writes an opening PGN from EPD.
  /// <param name="epds">The sequence of EPD entries.</param>
  /// <param name="filename">The filename.</param>
  let writeOpeningPgnFromEPD (epds: EPD.EPDEntry seq) (filename: string) =      
      if File.Exists(filename) |> not then
        use _ = File.Create(filename)
        printfn "Created pgn-file at this location: %s" filename        
      // Create a new text file using StreamWriter    
      use writer = new StreamWriter(filename, append=true)
      for epd in epds do
        //match epd.Id with
        //|Some id ->
        //  writer.WriteLine(sprintf "[Event \"%s\"]" id)
        //|None -> 
        //  writer.WriteLine(sprintf "[Event \"%s\"]" "From EPD-file")
        writer.WriteLine(sprintf "[FEN \"%s\"]" epd.FEN)         
        // Write an empty line after tags     
        writer.WriteLine()
          // Write SAN moves as pairs of white-black moves separated by spaces     
        writer.Write("*")
        writer.WriteLine(Environment.NewLine)    
        
      writer.Close()

  /// Writes an opening PGN file.
  /// <param name="pgnGames">The sequence of PGN games.</param>
  /// <param name="filename">The filename.</param>
  let writeOpeningPgnFile (pgnGames: PgnGame seq) (filename: string) =      
      if File.Exists(filename) |> not then
        //create file
        use stream = File.Create(filename)
        printfn "Created pgn-file at this location: %s" filename
        
      // Create a new text file using StreamWriter    
      use writer = new StreamWriter(filename, append=true)
      let mutable openingMoves = List.empty<string>      
      let mutable lastRound = 0
      let mutable counter = 0
      let mutable lastFenOpening = ""
      for g in pgnGames do
        let currentOpening = PGNHelper.getOpeningInfo g 
        let sanMoves = 
          let moves = g.Moves |> Seq.takeWhile (fun m -> m.WhiteComment.Contains "book" || m.BlackComment.Contains "book") |> Seq.toList
          if moves.Length = 0 then
            //take while white or black comment is empty
            g.Moves |> Seq.takeWhile (fun m -> m.WhiteComment = "" || m.BlackComment = "") |> Seq.toList
          else
            moves
        let onlyBookMoves = 
          [
            for m in sanMoves do
              if m.WhiteComment.Contains "book" then
                yield m.WhiteSan
              elif String.IsNullOrEmpty m.WhiteComment then
                yield m.WhiteSan
              if m.BlackComment.Contains "book" then
                yield m.BlackSan
              elif String.IsNullOrEmpty m.BlackComment then
                yield m.BlackSan
          ]
        let newOpening = 
          if currentOpening.StartsWith "No opening name" then
            match g.GameMetaData.Round.Split '.' |> Seq.tryHead with
            | Some round -> 
              if (int round)  <> lastRound then
                lastRound <- (int round)
                true
              else 
                false
            | None -> 
                let sameOpening =
                    //compare openingMoves with onlyBookMoves here                
                  if onlyBookMoves.Length > 0 && openingMoves.Length = onlyBookMoves.Length && openingMoves |> Seq.forall2 (=) onlyBookMoves then
                    true
                  //compare fen in case of epd opening names that only consists of FEN 
                  elif g.GameMetaData.Fen <> String.Empty && g.GameMetaData.Fen = lastFenOpening then                
                    true                
                  else
                    lastFenOpening <- g.GameMetaData.Fen
                    false
                not sameOpening
          else
            let sameOpening =
              if onlyBookMoves.Length > 0 && openingMoves.Length = onlyBookMoves.Length && openingMoves |> Seq.forall2 (=) onlyBookMoves then
                 true
              //compare fen in case of epd opening names that only consists of FEN
              elif g.GameMetaData.Fen <> String.Empty && g.GameMetaData.Fen = lastFenOpening then                
                 true                
              else
                 lastFenOpening <- g.GameMetaData.Fen
                 false
            //printfn "Same opening: %b for current opening: %s" sameOpening currentOpening
            not sameOpening
            
        openingMoves <- onlyBookMoves
        if newOpening then
          counter <- counter + 1
          let roundNr = $"{counter}.1"
          writer.WriteLine(sprintf "[Event \"%s\"]" g.GameMetaData.Event)
          writer.WriteLine(sprintf "[Round \"%s\"]" roundNr)
          let opening = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening" )
          let variation = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation" )
          let eco = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO" )
          match opening,variation, eco with
          |Some h,Some v, Some eco -> 
            writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
            writer.WriteLine(sprintf "[Variation \"%s\"]" v.Value )
            writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )
          |Some h,Some v, _ -> 
            writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
            writer.WriteLine(sprintf "[Variation \"%s\"]" v.Value )
          |Some h,_ , Some eco -> 
            writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
            writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )  
          |Some h, _, _ -> 
            writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
          |_, _, Some eco -> 
            writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )
          |_ -> ()
           // Write an empty line after tags     
          writer.WriteLine()
          let mutable ply = 1
          for m in sanMoves do
            //if m.WhiteComment.Contains "book" then
              writer.Write(sprintf "%d.%s " ply m.WhiteSan)
              ply <- ply + 1
            //if m.BlackComment.Contains "book" then
              writer.Write(sprintf "%s " m.BlackSan)
            
           // Write SAN moves as pairs of white-black moves separated by spaces     
          writer.Write("*")
          writer.WriteLine(Environment.NewLine)    
        
      writer.Close()

  /// Creates a PGN writer.
  /// <param name="filename">The filename.</param>
  /// <returns>A StreamWriter for the PGN file.</returns>
  let createPGNWriter (filename: string) : StreamWriter =
      if not (File.Exists(filename)) then
          // Create a new file with a FileStream specifying FileShare.ReadWrite
          let stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
          printfn "Created PGN file at this location: %s" filename
          new StreamWriter(stream)
      else
          // Open the existing file in append mode with shared read/write access
          let stream = new FileStream(filename, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)
          new StreamWriter(stream)

  /// Creates a PGN reader.
  /// <param name="filename">The filename.</param>
  /// <returns>A StreamReader for the PGN file.</returns> 
  let createPGNReader (filename: string) : StreamReader =      
      // Open the existing file in read mode with shared read access
      let stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
      new StreamReader(stream)
   
  /// Writes the PGN header section.
  /// <param name="writer">The StreamWriter.</param>
  /// <param name="header">The game metadata.</param>
  let writePGNHeaderSection (writer: StreamWriter) (header: GameMetadata) =
    // Write header data as tags using brackets [ ]
    writer.WriteLine(sprintf "[Event \"%s\"]" header.Event)
    writer.WriteLine(sprintf "[Site \"%s\"]" header.Site)
    writer.WriteLine(sprintf "[Date \"%s\"]" header.Date)
    writer.WriteLine(sprintf "[Round \"%s\"]" header.Round)
    writer.WriteLine(sprintf "[White \"%s\"]" header.White)
    writer.WriteLine(sprintf "[Black \"%s\"]" header.Black)
    writer.WriteLine(sprintf "[Result \"%s\"]" header.Result)
    writer.WriteLine(sprintf "[Reason \"%s\"]" (header.Reason.ToString()))
    writer.WriteLine(sprintf "[Ply \"%s\"]" (header.Moves.ToString()))
    writer.WriteLine(sprintf "[GameTime \"%s\"]" (header.GameTime.ToString()))
    writer.WriteLine(sprintf "[Opening \"%s\"]" (PGNHelper.getOpeningOnly header))
    writer.WriteLine(sprintf "[OpeningHash \"%s\"]" header.OpeningHash)
    if header.Fen <> "" then
      writer.WriteLine(sprintf "[FEN \"%s\"]" header.Fen)
    if header.Deviations > 0 then
      writer.WriteLine(sprintf "[Deviations \"%d\"]" header.Deviations)    
    
    // Write an empty line after tags     
    writer.WriteLine()

  /// Writes the end of game section.
  /// <param name="writer">The StreamWriter.</param>
  /// <param name="result">The game result.</param>
  let writeEndOfGameSection (writer: StreamWriter) (result:Result) =
    writer.Write(sprintf " {%s}" result.Reason.Explanation)
    // Write result *     
    writer.Write (sprintf " %s " result.Result)
    //writer.Write("*")
    writer.WriteLine(Environment.NewLine)  

  /// Writes the PGN moves section.
  /// <param name="writer">The StreamWriter.</param>
  /// <param name="openingMoves">The sequence of opening moves.</param>
  /// <param name="playMoves">The play moves.</param>
  let writePGNMovesSection (writer: StreamWriter) (openingMoves : string seq) (playMoves:string) =
    let openings = writeOpeningPGNMoves openingMoves
    let both = openings + playMoves
    writer.Write both

  /// Writes the moves section.
  /// <param name="writer">The StreamWriter.</param>
  /// <param name="moveIdx">The move index.</param>
  /// <param name="sanMoves">The sequence of SAN moves.</param>
  let writeMovesSection (writer: StreamWriter) (moveIdx: int) (sanMoves: seq<string>) =
    if moveIdx = 0 then
        writer.Write(sprintf " {no opening moves played}")
    else
        // Write SAN moves as pairs of white-black moves separated by spaces
        let msg = "last opening move played"
        let rec loop i ms idx =
            match ms with
            | [] -> ()  // Return unit when done
            | [m:string] ->
                // Write last move without pair number
                writer.Write(sprintf "%d.%s " i m)
            | m1::m2::ms' ->
                if (idx + 1) = moveIdx then
                    // Write pair number followed by white-black move pair
                    writer.Write(sprintf "%d.%s {%s} %s " i m1 msg m2)
                elif (idx + 2) = moveIdx then
                    writer.Write(sprintf "%d.%s {%s} %s " i m1 msg m2)
                else
                    writer.Write(sprintf "%d.%s %s " i m1 m2)
                
                // Call loop function with next pair number and remaining moves
                loop (i+1) ms' (idx+2)

        // Call loop function with initial pair number 1
        loop 1 (sanMoves |> Seq.toList) 0        
  
  /// Writes a PGN file.
  /// <param name="moveIdx">The move index.</param>
  /// <param name="header">The game metadata.</param>
  /// <param name="moves">The sequence of moves.</param>
  /// <param name="filename">The filename.</param>
  /// <param name="result">The game result.</param>    
  let writePgnFile moveIdx (header: GameMetadata) (moves: ResizeArray<string>) (filename: string) result =
      
      use pgnStreamWriter = createPGNWriter filename
      writePGNHeaderSection pgnStreamWriter header
      writeMovesSection pgnStreamWriter moveIdx moves
      writeEndOfGameSection pgnStreamWriter result
 
  /// Writes a whole PGN game to a file.
  /// <param name="filePath">The file path.</param>
  /// <param name="header">The game metadata.</param>
  /// <param name="moveSection">The move section.</param>
  /// <param name="result">The game result.</param>    
  let writePgnGame (filePath:string) (header: GameMetadata) (moveSection:string) result =
    //use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.Read)   
    //use writer = new StreamWriter(filePath,true)
    use writer = createPGNWriter filePath
    writePGNHeaderSection writer header
    writer.Write moveSection    
    writeEndOfGameSection writer result   
  


  let writeRawPgnGamesAdjustedToFile (filePath: string) (games: PgnGame seq) =
    PGNHelper.ensureDirectoryExists filePath
    File.Delete filePath
    let mutable idx = 0
    let mutable round = 0
        
    let sortByRound (round:string) =        
           match round.Split '.' with
           | [|rNr|] -> int rNr * 1000
           | [|rNr; rest|] -> int rNr * 1000 + (rest |> int)
           | _ -> -1
        
    let groupGames = 
        games 
            |> Seq.sortBy(fun g -> sortByRound g.GameMetaData.Round)
            |> Seq.groupBy (fun g -> g.GameMetaData.Round.Split '.' |> Seq.tryHead)
            |> Seq.toList
    for rNr,games in groupGames  do
        for game in games do
            let newRaw =
                match rNr with
                | Some r -> 
                    let roundNr = r |> int
                    if round <> roundNr then
                        round <- roundNr
                        idx <- 1                               
                    else
                        idx <- idx + 1
                    Regex.Replace(
                            game.Raw, 
                            @"\[Round\s*""[^""]*""\]", 
                            sprintf "[Round \"%s.%d\"]" r idx)
                | None -> game.Raw
     
            File.AppendAllText(filePath, newRaw)

  let removePlayerFromPGN (playerToRemove : string, games : PgnGame seq, fileName : string ) =
    let playerToLower = playerToRemove.Trim().ToLower()
    let outputFile = fileName
    let filteredGames = 
        games 
        |> Seq.filter (fun g -> 
            let playerFound = 
                g.GameMetaData.White.ToLower().Contains playerToLower || 
                g.GameMetaData.Black.ToLower().Contains playerToLower
            not playerFound )
        |> Seq.toList
    writeRawPgnGamesAdjustedToFile outputFile filteredGames  


module ConversionHelper =
  
  /// List of piece characters.
  let pieceChars = ['K';'Q';'R';'B';'N']
  
  /// Gets the piece symbol from a move.
  /// <param name="move">The move.</param>
  /// <returns>The piece symbol.</returns>
  let pieceSymbol (move:TMove inref) =
    match move.MoveType &&& TPieceType.PIECE_MASK with
      | TPieceType.EMPTY -> ""
      | TPieceType.PAWN -> ""
      | TPieceType.KNIGHT -> "N"
      | TPieceType.BISHOP -> "B"
      | TPieceType.ROOK -> "R"
      | TPieceType.QUEEN -> "Q"
      | TPieceType.KING -> "K"
      | _ -> failwith "Unknown piece type"
  
  /// Gets the promotion piece symbol.
  /// <param name="pieceType">The piece type.</param>
  /// <returns>The promotion piece symbol.</returns>
  let promoPiece (pieceType:TPieceType) =
    match pieceType with    
      | TPieceType.KNIGHT -> "n"
      | TPieceType.BISHOP -> "b"
      | TPieceType.ROOK -> "r"
      | TPieceType.QUEEN -> "q"
      | _ -> failwith "Unknown piece type"

  /// Checks if the move is a king move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a king move, otherwise false.</returns>
  let kingMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.KING

  /// Checks if the move is a queen move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a queen move, otherwise false.</returns>
  let queenMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.QUEEN

  /// Checks if the move is a rook move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a rook move, otherwise false.</returns>
  let rookMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.ROOK

  /// Checks if the move is a bishop move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a bishop move, otherwise false.</returns>
  let bishopMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.BISHOP

  /// Checks if the move is a knight move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a knight move, otherwise false.</returns>
  let knightMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.KNIGHT

  /// Checks if the move is a pawn move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a pawn move, otherwise false.</returns>
  let pawnMoved (move:TMove) = (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.PAWN

  /// Checks if the move is a capture move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a capture move, otherwise false.</returns>
  let findCapture (move:TMove) = (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY || (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY

  /// Checks if the move is not a capture move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is not a capture move, otherwise false.</returns>
  let findNoneCapture (move:TMove) = (move.MoveType &&& TPieceType.CAPTURE) = TPieceType.EMPTY

  /// Checks if the move is a promotion move.
  /// <param name="move">The move.</param>
  /// <returns>True if the move is a promotion move, otherwise false.</returns>
  let findPromotion (move:TMove) = (move.MoveType &&& TPieceType.PROMO) <> TPieceType.EMPTY
    
  /// Checks if the move string indicates a capture.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a capture, otherwise false.</returns>
  let isCapture (moveStr: string) = moveStr.ToLower().Contains("x")

  /// Checks if the move string indicates a checkmate.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a checkmate, otherwise false.</returns>
  let isMate (moveStr: string) = moveStr.Contains("#")

  /// Checks if the move string indicates a check.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a check, otherwise false.</returns>
  let isCheck (moveStr: string) = moveStr.Contains("+")

  /// Checks if the move string indicates a promotion.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a promotion, otherwise false.</returns>
  let isPromotion (moveStr: string) = moveStr.Contains("=")

  /// Checks if the move string is a normal move.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string is a normal move, otherwise false.</returns>
  let isNormalMove (moveStr: string) = moveStr.Length <= 3

  /// Checks if the move string is a long move.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string is a long move, otherwise false.</returns>
  let isLongMove (moveStr: string) = moveStr.Length > 3

  /// Checks if the move string indicates a piece move.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a piece move, otherwise false.</returns>
  let pieceMoved (moveStr: string) = pieceChars |> List.exists(fun c -> c = moveStr.[0])

  /// Checks if the move string indicates a castling move.
  /// <param name="moveStr">The move string.</param>
  /// <returns>True if the move string indicates a castling move, otherwise false.</returns>
  let isCastlingMove (moveStr: string) = 
    let trim = moveStr.Trim().ToUpper()
    trim.Contains("0-0") || trim.Contains("O-O") ||
    trim.Contains("0-0-0") || trim.Contains("O-O-O")

  /// Finds the promotion piece in the move string.
  /// <param name="moveStr">The move string.</param>
  /// <returns>The promotion piece character.</returns>
  let findPromoPiece (moveStr: string) = 
    let idx = moveStr.LastIndexOf("=")
    let piece = Char.ToUpper moveStr.[idx + 1]
    let pieceFound = pieceChars |> List.exists (fun c -> c = piece)
    if pieceFound then
      piece
    else failwith "Did not find promoted piece in move string"

  /// Finds the piece moved in the move string.
  /// <param name="moveStr">The move string.</param>
  /// <returns>The piece character.</returns>
  let findPieceMoved (moveStr: string) = moveStr.[0]

  /// Finds the square after a capture in the move string.
  /// <param name="moveStr">The move string.</param>
  /// <returns>The square after the capture.</returns>
  let findSquareAfterCapture (moveStr : string) =
    let idx = moveStr.LastIndexOf('x')
    moveStr.Substring(idx + 1, 2)

  /// Finds the square before a promotion in the move string.
  /// <param name="moveStr">The move string.</param>
  /// <returns>The square before the promotion.</returns>
  let findSquareBeforePromo (moveStr : string) =
    let idx = moveStr.LastIndexOf('=')
    moveStr.Substring(idx - 2, 2)
  
  /// Removes the end symbol from the SAN move string.
  /// <param name="moveStr">The SAN move string.</param>
  /// <returns>The SAN move string without the end symbol.</returns>
  let sanMinusEndSymbol (moveStr: string) = moveStr.Substring(0, moveStr.Length - 1)

  /// Removes the piece character from the move string.
  /// <param name="moveStr">The move string.</param>
  /// <returns>The move string without the piece character.</returns>
  let removePiece (moveStr: string) = moveStr.Substring(1)

  /// Finds the candidate moves for a given SAN move.
  /// <param name="possibleMoves">The array of possible moves.</param>
  /// <param name="san">The SAN move string.</param>
  /// <returns>An array of candidate moves.</returns>
  let findCandidates (possibleMoves: TMove array) san =
    match Char.ToUpper(findPieceMoved san) with
    |'K' -> possibleMoves |> Array.filter (fun m -> kingMoved m )          
    |'Q' -> possibleMoves |> Array.filter (fun m -> queenMoved m )
    |'R' -> possibleMoves |> Array.filter (fun m -> rookMoved m )
    |'B' -> possibleMoves |> Array.filter (fun m -> bishopMoved m )
    |'N' -> possibleMoves |> Array.filter (fun m -> knightMoved m )
    |_ -> possibleMoves


module ConvertTo =
  open ConversionHelper
    
  /// Converts a long SAN move to a standard SAN move.
  /// <param name="longSan">The long SAN move.</param>
  /// <param name="move">The move.</param>
  /// <param name="moves">The array of possible moves.</param>
  /// <param name="side">The side to move.</param>
  /// <returns>The standard SAN move.</returns>
  let standardSAN(longSan: string, move: TMove, moves: TMove array, side) =
      // Check if the input is valid
      if (longSan.Length < 4 || longSan.Length > 5) then    
         "Invalid input"
      
      elif (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
        if move.To = 6uy then
          //printfn "From: %d To: %d \n%A" move.From move.To move
          "0-0"
        else
          //printfn "From: %d To: %d \n%A" move.From move.To move
          "0-0-0"
      else
        let isCapture = TMoveOps.isCaptureMove move // (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
        let isEnpass = TMoveOps.isEnPassantMove move //(move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
        let isPromotion = TMoveOps.isPromotionMove move //(move.MoveType &&& TPieceType.PROMO) <> TPieceType.EMPTY
        let symbol = if (isCapture || isEnpass ) then "x" else ""
        let piece = pieceSymbol &move
        let candidates = moves |> Array.filter(fun m -> m.To = move.To && move.MoveType = m.MoveType)
        let len = candidates.Length
        let start =
          if isPromotion then  //promotion here - may be replaced with longSan.length > 4
            if isCapture then longSan.[0].ToString() else ""
          elif isEnpass then
            longSan.[0].ToString()
          elif String.IsNullOrEmpty(piece) && isCapture then
            longSan.[0].ToString()
          elif len > 1 then
            let rest = candidates |> Array.map (fun m -> TMoveOps.moveToStr &m side)
            let letter = rest |> Array.map(fun moveStr -> moveStr.[0]) |> Array.distinct
            let numbers = rest |> Array.map(fun moveStr -> moveStr.[1]) |> Array.distinct
            if letter.Length >= numbers.Length then
              //use letter now
              if move.Promotion <> TPieceType.EMPTY then
                piece + longSan.[2].ToString()
              else
                piece + longSan.[0].ToString()
            else
              //use number now
              piece + longSan.[1].ToString()
          else
            piece
        let ends = 
          if isPromotion then
            let piece = promoPiece move.Promotion
            "=" + piece.ToUpper()
          //elif isCheck longSan
          else
            ""          
        // Use only one letter for each square name
        let finalSan = start + symbol + longSan.[2].ToString() + longSan.[3].ToString() + ends        
        finalSan.Trim()


module MoveParser =

  type State =
    |Start
    |InTagSection
    |InMoveTextSection

  let nonStandardWhitespaces =
      Collections.Generic.HashSet<char>(['\uFEFF'; '\u2002'; '\u2003'; '\u2004'; '\u2005'; '\u2006';
                                                    '\u2007'; '\u2008'; '\u2009'; '\u200A'; '\u200B';
                                                    '\u202F'; '\u205F'; '\u3000'])
  let pool = Utils.StringBuilderPool(10,100)
  let pgnHeaderRegex = new Regex(@"\[(\w+)\s*""(.*?)""\]", RegexOptions.Compiled)
  let mutable error = String.Empty
  let mutable pos = Position.PositionOps.createEmptyTBoard()
  let load fen = BoardHelper.loadFen(Some fen, &pos)
  let mutable result = String.Empty
  let mutable position = 0
  let mutable input = String.Empty
  let mutable state = Start
  let mutable currentHeader = { Key = ""; Value = "" }
  let mutable gameMetadata = { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = ""; Result = ""; Reason = Misc.ResultReason.NotStarted; OpeningHash = ""; GameTime=0L; Moves = 0; Fen = ""; OpeningName = ""; Deviations = 0; OtherTags = [] }
  let mutable move = { MoveNr = ""; WhiteSan = ""; WhiteComment = ""; BlackSan = ""; BlackComment = "" }
  let mutable parsedCommentEndTag = true
  let moves = ResizeArray<Move>()
  let headers = ResizeArray<Header>()
  let resultPattern = "1-0/2"

  let resetStates () =
    pos <- Position.PositionOps.createEmptyTBoard()
    position <- 0
    input <- String.Empty
    result <- String.Empty
    state <- Start
    currentHeader <- { Key = ""; Value = "" }
    gameMetadata <- { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = ""; Result = ""; Reason = Misc.ResultReason.NotStarted; OpeningHash = ""; GameTime=0L; Moves = 0; Fen = ""; OpeningName = ""; Deviations = 0; OtherTags = [] }
    move <- Move.Empty 
    moves.Clear()
    headers.Clear()

  module PgnStates =
    let isRank c = Char.IsDigit(c) && c >= '1' && c <= '8'

    let isFile c = Char.IsLower(c) && c >= 'a' && c <= 'h'

    let isRankOrFile c = isRank c || isFile c

    let isPiece (c:char) = "KQRBN".Contains(c)

    let isPromoPiece (c:char) = "KQRBN".Contains(c) || "kqrbn".Contains(c)

    let isDisambiguation(c: char) = "NBRQKabcdefgh12345678".Contains(c)

    let skipWhiteSpace () =
      while position < input.Length && System.Char.IsWhiteSpace(input.[position]) do
        position <- position + 1
    
    let Peek () =
      if position < input.Length then
        input.[position]
      else 
        Char.MinValue

    let PeekAt i =
      if i < input.Length then
          input.[i]
      else 
        Char.MinValue
    
    let parseDigit() =
      if position < input.Length && Char.IsDigit(Peek()) then
          let c = Peek()
          position <- position + 1
          c
      else
          raise <| Exception($"Expected a digit but found '{Peek()}'")

    let expect c =
      if (position < input.Length && Peek() = c) then
        position <- position + 1
        c
      else
        failwith ($"Expected '{c}' but found '{Peek()}'"); 
    
    let parseCapture () = expect('x')

    let hasNonStandardWhitespaces (char: char) =
      nonStandardWhitespaces.Contains char

    let removeNonStandardWhitespaces () =
      if hasNonStandardWhitespaces(Peek()) then
        while hasNonStandardWhitespaces(Peek()) do
          position <- position + 1

    let isSanMove() =
      if Peek() = 'O' || (Peek() = '0' && PeekAt (position + 2) = '0') then
          true
      elif Peek() |> isFile && PeekAt(position + 1) |> isRank && PeekAt(position + 2) |> isPromoPiece then
        true
      else
          let mutable i = position
          if isPiece(Peek()) then
              i <- i + 1
          if isDisambiguation(PeekAt i) then
              i <- i + 1
          if PeekAt i = 'x' then
              i <- i + 1
          if isRankOrFile(PeekAt i) then
              true
          else
              false

    let parseAComment() =
        let sb = pool.Get()
        while position < input.Length && Peek() <> '}' do
            sb.Append(Peek()) |> ignore
            position <- position + 1
        let comment = sb.ToString()
        pool.Return(sb)
        comment

    let parseComment () =
      expect '{' |> ignore
      skipWhiteSpace()      
      let comment = parseAComment()
      skipWhiteSpace()
      let whiteMove = move.WhiteSan <> "" && move.BlackSan = ""
      if whiteMove then
          move.WhiteComment <- comment
      else
          move.BlackComment <- comment
      if Peek() <> '}' then
        parsedCommentEndTag <- false        
      else
        expect '}' |> ignore
        parsedCommentEndTag <- true
        
    let parseUnFinishedComment () =
      skipWhiteSpace()      
      let comment = parseAComment()
      skipWhiteSpace()
      if Peek() <> '}' then
        parsedCommentEndTag <- false
      else
        expect '}' |> ignore
        parsedCommentEndTag <- true
        let whiteMove = move.WhiteSan <> "" && move.BlackSan = ""
        if whiteMove then
            move.WhiteComment <- move.WhiteComment + comment
        else
          move.BlackComment <- move.BlackComment + comment
          moves.Add move
          move <- Move.Empty

    let parseDigits() = //maybe remove sb and use string instead
      let sb = pool.Get()
      while position < input.Length && Char.IsDigit(Peek()) do
          sb.Append(parseDigit()) |> ignore
      if sb.Length = 0 then
          raise <| Exception($"Expected a sequence of digits but found '{Peek()}'")
      let digits = sb.ToString()
      pool.Return(sb)
      digits
    
    // numeric_annotation_glyph = "$" digit+
    let parseNumericAnnotationGlyph() =
      expect('$') |> ignore // Expect and consume "$"
      let digits = parseDigits() // todo - store the digits
      Console.WriteLine($"${digits}") // Print the numeric annotation glyph for testing purposes

    let parseMoveNumberIndication () =     
      if not (String.IsNullOrEmpty(move.BlackSan)) then
        moves.Add(move)
        move <- Move.Empty
      
      let digits = parseDigits() // Parse and store the digits
      skipWhiteSpace() // Skip any white space after the digits 
      if position < input.Length && Peek() = '.' then        
        if move.WhiteSan <> "" then
          while Peek() = '.' do
            expect '.' |> ignore // Expect and consume "."            
        else
          move.MoveNr <- digits
          expect '.' |> ignore // Expect and consume "."
          while position < input.Length && Peek() = '.' do
              expect '.' |> ignore

    let parseResult () =
      let mutable ret = 
        if position > 0 then
          input.[position - 1].ToString()
        else ""
      skipWhiteSpace()
      while resultPattern.Contains(Peek()) do
          ret <- ret + string(Peek())
          position <- position + 1
      ret

    let rec parseRecursiveVariation() =
      expect('(') |> ignore
      skipWhiteSpace()
      while position < input.Length && Peek() <> ')' do
          //parseElement()
          position <- position + 1
          skipWhiteSpace()      
      if Peek() = '(' then
        parseRecursiveVariation()
      elif Peek() = '{' then
        parsedCommentEndTag <- false
      else
        parsedCommentEndTag <- true
      if position < input.Length && Peek() = ')' then
          expect(')') |> ignore
      else
          failwith "Expected a closing parenthesis"

    let toGameMetadata () =
      gameMetadata <-
        match currentHeader.Key with
        | "Event"  -> { gameMetadata with Event = currentHeader.Value }
        | "Site"   -> { gameMetadata with Site = currentHeader.Value }
        | "Date"   -> { gameMetadata with Date = currentHeader.Value }
        | "Round"  -> { gameMetadata with Round = currentHeader.Value }
        | "White"  -> { gameMetadata with White = currentHeader.Value }
        | "Black"  -> { gameMetadata with Black = currentHeader.Value }
        | "Result" -> { gameMetadata with Result = currentHeader.Value }
        | "Reason" -> { gameMetadata with Reason = Misc.stringToResultReason currentHeader.Value }
        | "OpeningHash" -> { gameMetadata with OpeningHash = currentHeader.Value }
        | "GameTime" -> { gameMetadata with GameTime = int64 currentHeader.Value }
        | "Ply" -> { gameMetadata with Moves = int currentHeader.Value }
        | "Opening" -> { gameMetadata with OpeningName = currentHeader.Value; OtherTags = currentHeader :: gameMetadata.OtherTags }
        | "Fen" 
        | "FEN" -> { gameMetadata with Fen = currentHeader.Value }
        | "Deviations" -> { gameMetadata with Deviations = int currentHeader.Value }
        | _ -> { gameMetadata with OtherTags = currentHeader :: gameMetadata.OtherTags }

    let expectOneOf (s : string) =
      let peek : char = Peek()
      if position < input.Length && s.Contains(peek) then
          expect (Peek())
      else
          raise <| Exception($"Expected one of '{s}' but found '{Peek()}'")

    let parsePiece() = expectOneOf "KQRBN"

    let parseCheckOrCheckmate() = expectOneOf("+#")

    let parseFile() =
      if position < input.Length && isFile(Peek()) then
          let c = Peek()
          position <- position + 1
          c
      else
          failwith $"Expected a file but found '{Peek()}'"

    let parseRank() =
      if position < input.Length && isRank(Peek()) then
          let c = Peek()
          position <- position + 1
          c
      else
          failwith $"Expected a rank but found '{Peek()}'"
    
    let parseSymbol() =
      let mutable symbol = ""     
      while position < input.Length && Char.IsPunctuation(Peek()) do
          let c = Peek()
          position <- position + 1
          symbol <- symbol + c.ToString()
      symbol


    let parseCastling() =
        let mutable v = Peek()
        let mutable move = expect(v).ToString()
        if Peek() = '-' then
            move <- move + expect('-').ToString()
        if Peek() = 'O' || Peek() = 'o' || Peek() = '0' then
            v <- Peek()
            move <- move + expect(v).ToString()
        if Peek() = '-' then
            move <- move + expect('-').ToString()
        if Peek() = 'O' || Peek() = 'o'|| Peek() = '0' then
            v <- Peek()
            move <- move + expect(v).ToString()
        move
    
    let parseDisambiguation () = 
      let mutable disambiguation = ""
      if isFile(Peek()) then
          disambiguation <- disambiguation + parseFile().ToString()
      if isRank(Peek()) then
          disambiguation <- disambiguation + parseRank().ToString()
      disambiguation
    
    let parsePromotion() =
      if Peek() = '=' then
        let mutable promotion = expect('=').ToString() // Expect and append "=" to the promotion
        promotion <- promotion + parsePiece().ToString() // Parse and append a piece to the promotion
        promotion // Return the promotion
      else
        let ch = Peek()
        position <- position + 1
        ch.ToString()
   
    let parseSquare() =
      let mutable square = ""      
      if Peek() = 'x' then
        square <- square  + (expect 'x').ToString()
      let c = Peek()
      let tmp = PeekAt (position - 1)
      if isPiece tmp && Char.IsDigit(Peek()) then
        square <- square  + (expect (Peek())).ToString()
      elif Char.IsLetter (Peek()) && Char.IsLetter (PeekAt (position + 1)) then
        expect c |> ignore //two letters in a row - consume the first
      if Peek() = 'x' then
        square <- square  + (expect 'x').ToString()
      let c = Peek()
      let d = PeekAt (position + 1)
      match isFile c, isRank d with
      |true, true ->
        let file  = parseFile()
        let rank = parseRank()
        square <- square + file.ToString() + rank.ToString()
      |_ -> () //continue
    
      square

    let parseSanMove() =
      let mutable san = String.Empty
      if Peek() = 'O' || (Peek() = '0' && PeekAt (position + 2) = '0') then
          san <- san + parseCastling()
      if isPiece(Peek()) then
          san <- san + parsePiece().ToString()
      if isDisambiguation(Peek()) then
          san <- san + parseDisambiguation()
      if Peek() = 'x' then
          san <- san + parseCapture().ToString()
      if isRankOrFile(Peek()) then
          san <- san + parseSquare()
      if Peek() = '=' || Peek() |> isPromoPiece then
          san <- san + parsePromotion()
      if Peek() = '+' || Peek() = '#' then
          san <- san + parseCheckOrCheckmate().ToString()
      if Char.IsPunctuation(Peek()) then
          san <- san + parseSymbol()     
      if String.IsNullOrEmpty(move.WhiteSan) then
          if moves.Count > 0 || String.IsNullOrEmpty gameMetadata.Fen  then              
            move.WhiteSan <- san
          else
            let _ = load gameMetadata.Fen
            if pos.STM = 0uy then
              move.WhiteSan <- san
            else
              move.BlackSan <- san
      else
          move.BlackSan <- san
    

    // Span-based parser function
    let parseTagPairSpan (input: ReadOnlySpan<char>) =
      let openBracketIndex = input.IndexOf('[')
      let closeBracketIndex = input.LastIndexOf(']')
    
      if openBracketIndex >= 0 && closeBracketIndex > openBracketIndex then
          let headerSpan = input.Slice(openBracketIndex + 1, closeBracketIndex - openBracketIndex - 1)
          let firstQuoteIndex = headerSpan.IndexOf('"')
          let lastQuoteIndex = headerSpan.LastIndexOf('"')
        
          if firstQuoteIndex >= 0 && lastQuoteIndex > firstQuoteIndex then
              let keySpan = headerSpan.Slice(0, firstQuoteIndex).Trim()
              let valueSpan = headerSpan.Slice(firstQuoteIndex + 1, lastQuoteIndex - firstQuoteIndex - 1)
              let key = keySpan.ToString()
              let value = valueSpan.ToString()
              (key, value)
          else
              ("", "")
      else
          ("", "")

    let parseTagPair() =
        let key,value = parseTagPairSpan (input.AsSpan())
        position <- input.Length
        currentHeader <- { Key = key; Value = value }
        headers.Add(currentHeader) |> ignore
        toGameMetadata ()
        if String.IsNullOrEmpty(key) && String.IsNullOrEmpty(value) then
          let msg = $"No match found in PGN-header - {input}"
          printfn "%s" msg
    

    let parseElement () = 
      if Char.IsDigit(Peek()) && (Peek() <> '0' && not (PeekAt (position + 1) = '-' )) then          
          parseMoveNumberIndication()
      elif isSanMove() then
          parseSanMove()
      elif Peek() = '$' then
          parseNumericAnnotationGlyph()
      elif Peek() = '(' then
          parseRecursiveVariation()
      elif Peek() = '{' then
          parseComment()
      elif Peek() = 'x' then
          parseCapture() |> ignore
      else          
          let res = parseResult()
          let hasRes = not (String.IsNullOrWhiteSpace(res))
          if hasRes then
            result <- res
          if String.IsNullOrEmpty(gameMetadata.Result) && hasRes then              
            gameMetadata <- { gameMetadata with Result = res }
    
    let parseMoveTextSection() =
      removeNonStandardWhitespaces()
      if not parsedCommentEndTag then
          parseUnFinishedComment()
      skipWhiteSpace()      
      while Peek() <> '[' && position < input.Length && Peek() <> '*' do          
          skipWhiteSpace()
          if Peek() = '(' then
            parseRecursiveVariation()
          else
            parseElement()
          skipWhiteSpace()
      if parsedCommentEndTag && String.IsNullOrEmpty(move.BlackSan) |> not then
          moves.Add move
          move <- Move.Empty
      if input.Length = position then            
          if String.IsNullOrWhiteSpace input then
            state <- State.InTagSection
          
      elif position < input.Length && Peek() = '*' then
          expect '*' |> ignore
      elif position < input.Length && Peek() = '[' then
          position <- input.Length
          state <- InTagSection
      else
          raise <| Exception("Expected a game termination")
    

    let parseTagSection() =      
      removeNonStandardWhitespaces()
      if input.StartsWith "##" then
        position <- input.Length
        state <- State.Start
      while position < input.Length && Peek() = '[' do
          parseTagPair()
          //skipWhiteSpace()
      if position = 0 && input.Length > 0 then
        if String.IsNullOrWhiteSpace error then
          let msg = "\nPGN-file had errors - most likely damaged header format or duplicated moveSection: \n" + input + Environment.NewLine
          LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red msg
          error <- msg        
        //position <- input.Length
        state <- State.InMoveTextSection
      if position = input.Length || Peek() <> '[' then
          if state = State.Start && input.StartsWith "##" = false then
              state <- State.InMoveTextSection
      else
        raise <| Exception("Expected a tag pair")    

    
    let parseGame () =
      parseTagSection()
      parseMoveTextSection()

  //todo - move these to a module
  type FinalResult = 
      | WhiteWins
      | Draw
      | BlackWins
      | Unknown

  let parseFinalResult (input: string) =
      match input with
      | "1" -> WhiteWins
      | "0" -> Draw
      | "-1" -> BlackWins
      | _ -> Unknown


  /// Parses a single line of input according to our simple rules.
  let parseLine (line: string) : EPDEntry option =
      if System.String.IsNullOrWhiteSpace(line) then
          None
      else
          // Look for the marker " am " or " bm "
          let iAm = line.IndexOf(" am ")
          let iBm = line.IndexOf(" bm ")
          let markerIndex, marker =
              if iAm >= 0 && (iBm < 0 || iAm < iBm) then (iAm, "am")
              elif iBm >= 0 then (iBm, "bm")
              else
                  // If neither marker is found, use the semicolon (if any)
                  let iSemi = line.IndexOf(';')
                  ((if iSemi >= 0 then iSemi else line.Length), "")
        
          // The FEN is everything before the marker (or semicolon).
          let fen = line.Substring(0, markerIndex).Trim()
                
          // Look for an " id " field anywhere in the line.
          let idOpt =
              let idKey = " id "
              let idIdx = line.IndexOf(idKey)
              if idIdx >= 0 then
                  // Everything after " id ".
                  let afterId = line.Substring(idIdx + idKey.Length).Trim()
                  // If the id is quoted, remove the quotes.
                  if afterId.StartsWith("\"") then
                      let closingQuote = afterId.IndexOf('"', 1)
                      if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                      else Some afterId
                  else Some afterId
              elif String.IsNullOrEmpty fen |> not then
                  Some fen
              else None

          // Look for an " bm " field anywhere in the line.
          let bmOpt =
              let idKey = " bm "
              let idIdx = line.IndexOf(idKey)
              if idIdx >= 0 then
                  // Everything after " bm ".
                  let afterId = line.Substring(idIdx + idKey.Length).Trim()
                  // If the bm is quoted, remove the quotes.
                  if afterId.StartsWith("\"") then
                      let closingQuote = afterId.IndexOf('"', 1)
                      if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                      else Some afterId
                  else 
                    let closingQuote = afterId.IndexOf(';', 1)
                    if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                    else Some afterId
              
              else None
        
          // Look for an " am " field anywhere in the line.
          let amOpt =
              let idKey = " am "
              let idIdx = line.IndexOf(idKey)
              if idIdx >= 0 then
                  // Everything after " am ".
                  let afterId = line.Substring(idIdx + idKey.Length).Trim()
                  // If the am is quoted, remove the quotes.
                  if afterId.StartsWith("\"") then
                      let closingQuote = afterId.IndexOf('"', 1)
                      if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                      else Some afterId
                  else 
                    let closingQuote = afterId.IndexOf(';', 1)
                    if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                    else Some afterId
              else None
        
          let other =
              let idKey = " other "
              let idIdx = line.IndexOf(idKey)
              if idIdx >= 0 then
                  // Everything after " other ".
                  let afterId = line.Substring(idIdx + idKey.Length).Trim()
                  // If the other is quoted, remove the quotes.
                  if afterId.StartsWith("\"") then
                      let closingQuote = afterId.IndexOf('"', 1)
                      if closingQuote > 0 then Some (afterId.Substring(1, closingQuote - 1))
                      else Some afterId
                  else 
                    let closingQuote = afterId.IndexOf(';', 1)
                    if closingQuote > 0 then afterId.Substring(0, closingQuote) |> Some
                    else Some afterId
              else None

          Some {RawInput = line; FEN = fen; BestMove = bmOpt; AvoidMove = amOpt; Id = idOpt; Other = other }


module EPDExtractor =
  
  let extractEDPDetails (input: string) = MoveParser.parseLine input

  let mapEPDToPGN (epd:EPD.EPDEntry) n : PGNTypes.PgnGame =
    let header = { Key = "Opening"; Value = epd.Id |> Option.defaultValue ""}
    let gameData = 
      if header.Value = "" then
        {PGNTypes.GameMetadata.Empty with Fen = epd.FEN}
      else
        {PGNTypes.GameMetadata.Empty with Fen = epd.FEN; OtherTags=[header] }  
    {   
      GameNumber = n
      GameMetaData = gameData
      Moves = ResizeArray<Move>()
      Comments = ""
      Fen = epd.FEN
      Raw = epd.RawInput
    }

  let mapChessRecordToEPD (record: Tests.ChessRecord) n : EPDEntry =
    let fen = record.FEN  
    let other = { RawInput = ""; FEN = fen; BestMove = None; AvoidMove = None; Id = Some (n.ToString()); Other = record.ToString() |> Some }
    other

  let readEPDs (path:string) =
    let content = File.ReadAllLines path
    seq { for line in content do
              if line.StartsWith("##") |> not then
                  extractEDPDetails line }
    |> Seq.choose id

  let getOpeningPGNFromEPD path =  
    readEPDs path
    |> Seq.mapi (fun idx el -> mapEPDToPGN el (idx + 1))

  let parseEPDFile (pgnFilePath: string): seq<PgnGame> =
    let openings = getOpeningPGNFromEPD pgnFilePath
    openings


module PGNParser =

  open MoveParser
  let getMovesFromPGN (pgnGame: PgnGame) =
    seq { 
          for move in pgnGame.Moves do
            if move.WhiteSan <> "" then            
              move.WhiteSan
            if move.BlackSan <> "" then
              move.BlackSan  }
  let parseMoveSectionOfPgnGame (content: string) =
      resetStates()
      let mutable counter = 0
      state <- Start    
      input <- content    
      position <- 0    
      while position < input.Length do
          match state with
          | Start -> 
            PgnStates.parseTagSection()
            if state = InMoveTextSection then
              position <- 0
              PgnStates.parseMoveTextSection()
          
          | InMoveTextSection ->
              PgnStates.parseMoveTextSection()
          | InTagSection -> ()

          if state = InTagSection then
            state <- InMoveTextSection
      if move.WhiteSan <> "" && (moves |> Seq.last).WhiteSan <> move.WhiteSan then
        moves.Add move
      if moves.Count > 0 then              
        {
          GameNumber = counter
          GameMetaData = gameMetadata
          Moves = ResizeArray(moves)
          Comments = "" 
          Fen = gameMetadata.Fen
          Raw = content
        }
      else
        PgnGame.Empty 0

  let parseFullPgnGame (pastedText: string): PgnGame =
      resetStates()
      let mutable counter = 0
      let rawGame = new StringBuilder()
        
      //get the bytes from the pasted text
      let bytes = Encoding.UTF8.GetBytes(pastedText)
      //create a memory stream from the bytes
      use memoryStream = new MemoryStream(bytes)
    
      seq {
            state <- Start          
            use reader = new StreamReader(memoryStream)
            while not reader.EndOfStream do
                input <- reader.ReadLine().Trim()
                rawGame.Append(input).Append(Environment.NewLine) |> ignore
                position <- 0
                while position < input.Length do
                    match state with
                    | Start -> 
                      PgnStates.parseTagSection()
                      state <- InTagSection

                    | InMoveTextSection ->
                        PgnStates.parseMoveTextSection()

                    | InTagSection ->
                        PgnStates.parseTagSection()
                      
                let isEmpty = String.IsNullOrEmpty(input)
                let hasMoves = moves.Count > 0 || String.IsNullOrEmpty move.WhiteSan |> not || String.IsNullOrEmpty move.BlackSan |> not
                let isStar = input.Trim() = "*"
                if isEmpty && state = InTagSection then
                  state <- InMoveTextSection
                elif
                  isStar ||
                  (isEmpty && state = InMoveTextSection && hasMoves ) 
                    || 
                  (not isEmpty && state = InTagSection && hasMoves) then
                      if move.WhiteSan <> "" then
                        moves.Add move
                      counter <- counter + 1
                      //printfn "Counter %d" counter
                      yield 
                        {
                          GameNumber = counter
                          GameMetaData = gameMetadata
                          Moves = ResizeArray(moves)
                          Comments = "" 
                          Fen = gameMetadata.Fen
                          Raw = rawGame.ToString()
                        }                
            if moves.Count > 0 then
              if move.WhiteSan <> "" then
                moves.Add move
              counter <- counter + 1
              yield 
                {
                  GameNumber = counter
                  GameMetaData = gameMetadata
                  Moves = ResizeArray(moves)
                  Comments = "" 
                  Fen = gameMetadata.Fen
                  Raw = rawGame.ToString()
                    }
      } |> Seq.tryHead |> Option.defaultValue (PgnGame.Empty 0)


  let parsePgnFile (pgnFilePath: string): seq<PgnGame> =
      resetStates()
      let mutable counter = 0
      let rawGame = new StringBuilder()
      seq {
            state <- Start
            let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)          
            use reader = new StreamReader(pgnFilePath, options)
            while not reader.EndOfStream do
                input <- reader.ReadLine().TrimStart()              
                rawGame.Append(input).Append(Environment.NewLine) |> ignore              
                position <- 0
                while position < input.Length do
                    match state with
                    | Start -> 
                        state <- InTagSection

                    | InMoveTextSection ->
                        PgnStates.parseMoveTextSection()

                    | InTagSection ->
                        PgnStates.parseTagSection()
                      
                let isEmpty = String.IsNullOrEmpty(input)
                let hasMoves = moves.Count > 0 || String.IsNullOrEmpty move.WhiteSan |> not || String.IsNullOrEmpty move.BlackSan |> not
                let isStar = input.Trim() = "*"              
                if isEmpty && state = InTagSection then
                  state <- InMoveTextSection                
                elif                
                  isStar ||
                  (isEmpty && state = InMoveTextSection && hasMoves ) 
                    || 
                  (not isEmpty && state = InTagSection && hasMoves) 
                  || (String.IsNullOrWhiteSpace result |> not)  then
                      if move.WhiteSan <> "" then
                        moves.Add move
                      counter <- counter + 1
                      yield 
                        {
                          GameNumber = counter
                          GameMetaData = gameMetadata
                          Moves = ResizeArray(moves)
                          Comments = "" 
                          Fen = gameMetadata.Fen
                          Raw = rawGame.ToString()
                        }
                      move <- Move.Empty
                      result <- String.Empty
                      rawGame.Clear() |> ignore
                      state <- Start
                      gameMetadata <- { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = ""; Result = ""; Reason = Misc.ResultReason.NotStarted ; OpeningHash = ""; GameTime=0L; Moves = 0; Fen = ""; OpeningName = ""; Deviations = 0; OtherTags = [] }
                      moves.Clear()
                      headers.Clear()
            if moves.Count > 0 then
              if move.WhiteSan <> "" then
                moves.Add move
              counter <- counter + 1
              yield 
                {
                  GameNumber = counter
                  GameMetaData = gameMetadata
                  Moves = ResizeArray(moves)
                  Comments = "" 
                  Fen = gameMetadata.Fen
                  Raw = rawGame.ToString()
                }
      }
    
  let parsePgnStream (reader: StreamReader): seq<PgnGame> =
      resetStates()
      let mutable counter = 0
      let rawGame = new StringBuilder()
      seq {
            state <- Start          
            while not reader.EndOfStream do
                input <- reader.ReadLine().TrimStart()              
                rawGame.Append(input).Append(Environment.NewLine) |> ignore              
                position <- 0
                while position < input.Length do
                    match state with
                    | Start -> 
                      PgnStates.parseTagSection()                                          
                      state <- InTagSection

                    | InMoveTextSection ->
                        PgnStates.parseMoveTextSection()

                    | InTagSection ->
                        PgnStates.parseTagSection()
                      
                let isEmpty = String.IsNullOrEmpty(input)
                let hasMoves = moves.Count > 0 || String.IsNullOrEmpty move.WhiteSan |> not || String.IsNullOrEmpty move.BlackSan |> not
                let isStar = input.Trim() = "*"
                if isEmpty && state = InTagSection then
                  state <- InMoveTextSection
                elif
                  isStar ||
                  (isEmpty && state = InMoveTextSection && hasMoves ) 
                    || 
                  (not isEmpty && state = InTagSection && hasMoves) 
                  || (String.IsNullOrWhiteSpace result |> not)  then
                      if move.WhiteSan <> "" then
                        moves.Add move
                      counter <- counter + 1
                      //printfn "Counter %d" counter
                      yield 
                        {
                          GameNumber = counter
                          GameMetaData = gameMetadata
                          Moves = ResizeArray(moves)
                          Comments = "" 
                          Fen = gameMetadata.Fen
                          Raw = rawGame.ToString()
                        }
                      move <- Move.Empty
                      rawGame.Clear() |> ignore
                      state <- Start
                      result <- String.Empty
                      gameMetadata <- { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = ""; Result = ""; Reason = Misc.ResultReason.NotStarted ; OpeningHash = ""; GameTime=0L; Moves = 0; Fen = ""; OpeningName = ""; Deviations = 0; OtherTags = [] }
                      moves.Clear()
                      headers.Clear()
            if moves.Count > 0 then
              if move.WhiteSan <> "" then
                moves.Add move
              counter <- counter + 1
              yield 
                {
                  GameNumber = counter
                  GameMetaData = gameMetadata
                  Moves = ResizeArray(moves)
                  Comments = "" 
                  Fen = gameMetadata.Fen
                  Raw = rawGame.ToString()
                }
      }

  let parsePgnFileHeaders (pgnFilePath: string): seq<PgnGame> =
      resetStates()
      let mutable counter = 0
      let rawGame = new StringBuilder()
      seq {
            state <- Start
            use reader = new StreamReader(pgnFilePath)
            while not reader.EndOfStream do
                input <- reader.ReadLine()              
                rawGame.Append(input).Append(Environment.NewLine) |> ignore
                position <- 0
                while position < input.Length do
                    match state with
                    | Start -> 
                        PgnStates.parseTagSection()                      
                        state <- InTagSection

                    | InMoveTextSection -> 
                        if position < input.Length && PgnStates.Peek() = '[' then
                          headers.Clear()
                          PgnStates.parseTagPair()
                          state <- InTagSection
                        position <- input.Length                        

                    | InTagSection ->
                        PgnStates.parseTagSection()                      
              
                if String.IsNullOrEmpty input then
                  if state = InTagSection then                  
                    state <- InMoveTextSection
                  else 
                    state <- Start
                    if gameMetadata <> GameMetadata.Empty then 
                        yield 
                          {
                            GameNumber = counter
                            GameMetaData = gameMetadata
                            Moves = ResizeArray(moves)
                            Comments = "" 
                            Fen = gameMetadata.Fen
                            Raw = rawGame.ToString()
                          }
                        move <- Move.Empty
                        rawGame.Clear() |> ignore                      
                        gameMetadata <- { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = ""; Result = ""; Reason = Misc.ResultReason.NotStarted ; OpeningHash = ""; GameTime=0L; Moves = 0; Fen = ""; OpeningName = ""; Deviations = 0; OtherTags = [] }
                        moves.Clear()
                        headers.Clear()
              
            if gameMetadata <> GameMetadata.Empty then            
              yield 
                {
                  GameNumber = counter
                  GameMetaData = gameMetadata
                  Moves = ResizeArray(moves)
                  Comments = "" 
                  Fen = gameMetadata.Fen
                  Raw = rawGame.ToString()
                }
      }

  type PgnGameMessage =
  | WriteGame of filePath:string * header:GameMetadata * moveSection:string * result:Result
  | GetResults of reply:AsyncReplyChannel<ResizeArray<Result>>
  | GetPGNGames of reply:AsyncReplyChannel<ResizeArray<PgnGame>>
  | Dispose

  let startPgnGameReaderWriter (filePath: string) =
    MailboxProcessor<PgnGameMessage>.Start(fun inbox ->
        async {
            // Open the file once for all future writes
            use writer =
              if String.IsNullOrWhiteSpace filePath then
                  new StreamWriter(new MemoryStream())
              else
                  PGNWriter.createPGNWriter filePath
            use reader = 
              if String.IsNullOrWhiteSpace filePath then
                  new StreamReader(new MemoryStream())
              else
                  PGNWriter.createPGNReader filePath
            writer.AutoFlush <- true
            while true do
                let! message = inbox.Receive()
                match message with
                | Dispose -> writer.Dispose() // Exit the loop and dispose the writer                    
                | WriteGame(_, header, moveSection, result) ->
                    // Write the PGN game to the file
                    PGNWriter.writePGNHeaderSection writer header
                    writer.Write moveSection
                    PGNWriter.writeEndOfGameSection writer result
                | GetPGNGames reply -> 
                    //first set reader to the beginning of the file
                    reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
                    let games = parsePgnStream reader
                    // Reply with the games
                    reply.Reply (ResizeArray<PgnGame>(games))
                | GetResults reply -> 
                    //first set reader to the beginning of the file
                    reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
                    let games = 
                      let allResults = parsePgnStream reader
                      allResults
                      |> Seq.map PGNWriter.getResultsFromPGNGame
                      |> Seq.toArray        
                    games |> Array.Reverse                  
                    // Reply with the results
                    reply.Reply (ResizeArray<Result>(games))
        }
    )