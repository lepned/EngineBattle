module ChessLibrary.Parser

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Text
open System.Text.RegularExpressions
open TypesDef
open TypesDef.PGNTypes
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
      [|for move in game.Mainline do
         if move.Color = "w" then
            yield true, game.GameMetaData.White, move.MoveNumber, move.San
         else
            yield false, game.GameMetaData.Black, move.MoveNumber, move.San |]
    moves, game.GameMetaData

  
  /// Extracts engine book exit statistics from a PGN game.
  /// <param name="game">The PGN game.</param>
  /// <returns>The engine book exit statistics.</returns>
  let extractEngineBookExitStats (game:PgnGame) =
    let mutable foundWhiteMove = false
    let mutable foundBlackMove = false
    let moves =
      [|for move in game.Mainline do
         if move.Color = "w" then
            let white = Annotation.getEngineStatData game.GameMetaData.White false move.Comment
            if (not foundWhiteMove && white.n > 0) then
              foundWhiteMove <- true
              yield white
         else
            let black = Annotation.getEngineStatData game.GameMetaData.Black true move.Comment
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
      [|for move in game.Mainline do
         if move.Color = "w" then
            yield Annotation.getEngineStatData game.GameMetaData.White false move.Comment
         else
            yield Annotation.getEngineStatData game.GameMetaData.Black true move.Comment |]
    {White = game.GameMetaData.White; Black = game.GameMetaData.Black; Moves = moves}

  let extractWhiteAndBlackEngineStats (game:PgnGame) =
    let whiteMoves =
      [|for move in game.Mainline do
            if move.Color = "w" then
              Annotation.getEngineStatData game.GameMetaData.White false move.Comment
      |]
    let blackMoves =
      [|for move in game.Mainline do
            if move.Color = "b" then
              Annotation.getEngineStatData game.GameMetaData.Black true move.Comment
      |]
    whiteMoves, blackMoves

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

  let calculateMedianEPS (moves: EngineMoveStat array) = 
      moves 
      |> Array.filter (fun e -> e.eps > 0)
      |> Array.map (fun e -> float e.eps)
      |> Array.sortBy id
      |> median  
  
  //group engineMoveStat array by #pieces
  let groupMoveDataPerPieceCount (moves: EngineMoveStat array) =      
    moves
    |> Array.filter (fun e -> e.pcs > 0)
    |> Array.groupBy (fun e -> e.pcs)

    // safe: grouping can be empty -> callers expect empty array in that case
  let calculateAvgEPSAndNPSPerPieceCount (moves: EngineMoveStat array) =
    groupMoveDataPerPieceCount moves
    |> Array.map (fun (pcs, moveStats) ->
        // collect only positive values
        let epsVals = moveStats |> Array.choose (fun e -> if e.eps > 0 then Some (float e.eps) else None)
        let npsVals = moveStats |> Array.choose (fun e -> if e.s > 0 then Some (float e.s) else None)

        let avgEPS = if Array.isEmpty epsVals then 0.0 else Array.average epsVals
        let avgNPS = if Array.isEmpty npsVals then 0.0 else Array.average npsVals

        (pcs, avgEPS, avgNPS)
    )  
  
  let calculateAvgEPS (moves: EngineMoveStat array) = 
      let vals =
          moves 
          |> Array.filter (fun e -> e.eps > 0)
          |> Array.map (fun e -> float e.eps)
          |> Array.sortBy id
      if Array.isEmpty vals then 0.0 else Array.average vals
  
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

  //calculate pieceCountData into PieceCountStat per PGN file

  let calculatePieceCountDataPerPgnFile (games:PgnGame seq) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN 
    let res =
        players
        |> Array.Parallel.map (fun p -> 
            let moves = getAllEngineMovesForPlayer p allMoves
            let hasMoves = moves |> Array.exists (fun e -> e.n > 0)
            if not hasMoves then
              [||]
            else
              calculateAvgEPSAndNPSPerPieceCount moves
              |> Array.map (fun (pcs, eps, nps) -> { Player = p; PieceCount = pcs; AvgEps = eps; AvgNps = nps })
              )  
        |> Array.concat
    if res.Length = 0 then
      [||]
    else
      res 
      |> Array.filter (fun e -> e.AvgEps > 0)
      |> Array.sortBy (fun e -> e.Player, e.PieceCount)
      |> Array.groupBy (fun e -> e.Player)
      

  let printPieceCountDataPerPgnFile (filePath:string) (data: (string * PieceCountStat array) array) =
    // Prepare header and rows
    let headers = [| "Player"; "PieceCount"; "AvgEps"; "AvgNps" |]

    // Convert data to rows of strings
    let rows =
        data
        |> Array.collect (fun (player, stats) ->
            stats
            |> Array.map (fun stat ->
                [|
                    player
                    stat.PieceCount.ToString()
                    sprintf "%.2f" stat.AvgEps
                    sprintf "%.2f" stat.AvgNps
                |]
            )
        )

    // Compute column widths (max of header and each column's values)
    let colCount = headers.Length
    let widths =
        Array.init colCount (fun colIdx ->
            let headerLen = headers.[colIdx].Length
            let maxDataLen =
                if rows.Length = 0 then 0
                else rows |> Array.map (fun r -> r.[colIdx].Length) |> Array.max
            max headerLen maxDataLen
        )

    // Build lines
    let sb = StringBuilder()
    let pad (s:string) idx = s.PadRight(widths.[idx])

    // Header line
    let headerLine =
        headers
        |> Array.mapi (fun i h -> pad h i)
        |> String.concat " | "
    sb.AppendLine(headerLine) |> ignore

    // Separator line
    let sepLine =
        widths
        |> Array.map (fun w -> String.replicate w "-")
        |> String.concat "-+-"
    sb.AppendLine(sepLine) |> ignore

    // Data rows
    for row in rows do
        let line =
            row
            |> Array.mapi (fun i v -> pad v i)
            |> String.concat " | "
        sb.AppendLine(line) |> ignore

    // Ensure directory exists then write file
    let directory = Path.GetDirectoryName(filePath)
    if not (String.IsNullOrEmpty(directory)) && not (Directory.Exists(directory)) then
        printfn "Path not valid: %s" filePath     
    else
        File.WriteAllText(filePath, sb.ToString())
  
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
            AvgEPS= calculateAvgEPS moves
            MedianEPS = calculateMedianEPS moves
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
          let epsMed = calculateMedianEPS moves
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
            AvgEPS= calculateAvgEPS moves
            MedianEPS = epsMed
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
  let calculateMedianAndAvgSpeedSummaryInPgnFile (games:PgnGame seq, timeInSecs:int) =
    let players = getPlayersFromPGN games
    let allGames = PGNExtractor.extractAllEngineStatsInPGN games
    let minMoveTimeInMs = int64 (timeInSecs * 1000)
    let allMoves = allGames |> PGNExtractor.extractAllEngineMovesInPGN |> Array.filter (fun e -> e.mt >= minMoveTimeInMs) 
    players
    |> Array.Parallel.map (fun p -> 
        let moves = getAllEngineMovesForPlayer p allMoves
        let avg = averageNpsWithoutOutliers moves
        let epsAvg = calculateAvgEPS moves
        let epsMed = calculateMedianEPS moves
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
          {Player=p; Median=true; AvgNPS=npsMed; Games=games; EPS=epsMed; AvgDepth=depthMed; AvgNodes=nodesMed; AvgSelfDepth=sdMed; Time = moveTime |> int64}
          {Player=p; Median = false; AvgNPS=npsAvg; Games=games; EPS=epsAvg; AvgDepth=depthAvg; AvgNodes=nodesAvg; AvgSelfDepth=sdAvg; Time = moveTime |> int64}          
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
        let epsAvg = calculateAvgEPS moves
        let epsMed = calculateMedianEPS moves
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
          {Player=p; Median=true; AvgNPS=npsMed; Games=games; EPS=epsMed; AvgDepth=depthMed; AvgNodes=nodesMed; AvgSelfDepth=sdMed; Time = moveTime |> int64}
          {Player=p; Median = false; AvgNPS=npsAvg; Games=games; EPS=epsAvg; AvgDepth=depthAvg; AvgNodes=nodesAvg; AvgSelfDepth=sdAvg; Time = moveTime |> int64}          
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
        let epsMed = calculateMedianEPS moves
        let nodes = calculateMedianNodes moves
        let depth = calculateMedianDepth moves
        let sd = calculateMedianSelfdepth moves
        let moveTime = calculateMedianMoveTime moves
        let sd = max sd depth
        let games = calcNumberOfGames p allGames
        {Player=p; Median = true; AvgNPS=nps; Games=games; EPS=epsMed; AvgDepth=depth; AvgNodes=nodes; AvgSelfDepth=sd; Time = moveTime |> int64})

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
        let epsAvg = calculateAvgEPS moves
        let nodes = avg nodeMap
        let depth = avg depthMap
        let sd = max depth (avg sdMap)
        let games = calcNumberOfGames p allGames
        {Player=p; Median = false; AvgNPS=nps; Games=games; EPS=epsAvg; AvgDepth=depth; AvgNodes=nodes; AvgSelfDepth=sd; Time = 0L})

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
      OutOfOpeningEvals = []
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
      
      // Write SAN moves as pairs of white-black moves separated by spaces     
      for m in g.Mainline do
        if m.Color = "w" then
            writer.Write(sprintf "%d.%s " m.MoveNumber m.San)
        else       
            writer.Write(sprintf "%s " m.San)            
      
      if String.IsNullOrWhiteSpace g.GameMetaData.Result |> not then
        writer.WriteLine(g.GameMetaData.Result.Trim())
      else
        // If no result is specified, write an asterisk
        writer.Write("*")
      writer.WriteLine()    
  
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

  /// Extracts all opening moves from a PGN game by collecting moves up until the last book tag
  /// Returns a list of SAN move strings
  let getOpeningMovesFromPgn (pgnGame: PgnGame) : string list =
    // Determine if a comment indicates a book move
    let isBookComment (comment: string) = 
        not (String.IsNullOrEmpty(comment)) && 
        (comment.Contains("book", StringComparison.OrdinalIgnoreCase) || 
            comment.Contains("Book exit", StringComparison.OrdinalIgnoreCase))
    
    // Find the last move with a book tag and whether it's a white or black move
    let mutable lastMoveIdxWithBookTag = -1
    let mutable lastBookTagIsWhite = false
    
    for i = 0 to pgnGame.Mainline.Count - 1 do
        let move = pgnGame.Mainline.[i]
        
        if isBookComment move.Comment then
            lastMoveIdxWithBookTag <- i
            lastBookTagIsWhite <- true
        
        if isBookComment move.Comment then
            lastMoveIdxWithBookTag <- i
            lastBookTagIsWhite <- false
    
    // If no book tags found, return empty list
    if lastMoveIdxWithBookTag = -1 then
        []
    else
        // Extract all SAN moves up to and including the last book tag
        let openingMoves = ResizeArray<string>()
        for i = 0 to lastMoveIdxWithBookTag do
            let move = pgnGame.Mainline.[i]
            
            // Add the white move if present
            if move.Color = "w" then
                openingMoves.Add(move.San)
            
            // Add the black move if present
            elif move.Color = "b" then            
                // Only add the black move at the last index if it has a book tag
                if i < lastMoveIdxWithBookTag || not lastBookTagIsWhite then
                    openingMoves.Add(move.San)
        openingMoves |> List.ofSeq

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
      let totalGames = Seq.length pgnGames
      let games = 
        pgnGames 
        //|> Seq.filter (fun g -> g.GameMetaData.Event.ToLower().Contains "testing" |> not) 
        //|> Seq.filter (fun g -> g.GameMetaData.Moves = 0) // Filter out games with no moves
      let totalFilteredGames = Seq.length games
      let difference = totalGames - totalFilteredGames
      if difference > 0 then
         printfn "Filtered out %d testing games from %d total games." difference totalGames

      // Use HashSet to track unique openings - by moves and by FEN
      let uniqueMoveSequences = System.Collections.Generic.HashSet<string>()
      let uniqueFens = System.Collections.Generic.HashSet<string>()
      let mutable counter = 0
      
      for g in games do
        //let currentOpening = PGNHelper.getOpeningInfo g
        let onlyBookMoves = getOpeningMovesFromPgn g        
        // Create a key representing this opening
        let moveSequenceKey = String.Join("|", onlyBookMoves)
        
        // Check if this opening is unique by moves or by FEN
        let isUniqueByMoves = onlyBookMoves.Length > 0 && not (uniqueMoveSequences.Contains(moveSequenceKey))
        let isUniqueByFen = not (String.IsNullOrEmpty g.GameMetaData.Fen) && not (uniqueFens.Contains(g.GameMetaData.Fen))
        let isNewOpening = isUniqueByMoves || isUniqueByFen
        if isNewOpening then
            // Add to our tracking collections
          if onlyBookMoves.Length > 0 then
                uniqueMoveSequences.Add(moveSequenceKey) |> ignore
                
          if not (String.IsNullOrEmpty g.GameMetaData.Fen) then
                uniqueFens.Add(g.GameMetaData.Fen) |> ignore
                
          counter <- counter + 1
          let roundNr = $"{counter}.1"
          writer.WriteLine(sprintf "[Event \"%s\"]" g.GameMetaData.Event)
          writer.WriteLine(sprintf "[Round \"%s\"]" roundNr)
          if String.IsNullOrWhiteSpace g.GameMetaData.Fen |> not  then
            writer.WriteLine(sprintf "[FEN \"%s\"]" g.GameMetaData.Fen)
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
          for m in onlyBookMoves do
              if ply % 2 = 1 then
                let moveNumber = (ply + 1) / 2
                // Write white move with pair number
                writer.Write(sprintf "%d.%s " moveNumber m)
              else
                  writer.Write(sprintf "%s " m)
              ply <- ply + 1
            
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
    let haveEvals, evals = 
        match header.StartEvals with
        |[] -> false, ""
        |[x] -> true, sprintf "%s" x.ValueStr
        |x::y::_ -> true, sprintf "%s, %s" x.ValueStr y.ValueStr
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
    if haveEvals then
      writer.WriteLine(sprintf "[StartEvals \"%s\"]" evals)
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

  let ensureBlankLinesBetweenGames (filePath: string) (games: PgnGame seq) =
      PGNHelper.ensureDirectoryExists filePath      
      use writer = new StreamWriter(filePath, append=false)
      for g in games do
        // write the raw game and ensure exactly one blank line after each game
        writer.WriteLine(g.Raw.TrimStart().TrimEnd())
        writer.WriteLine() // single blank line

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
      Mainline = ResizeArray()
      RootVariations = ResizeArray()
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


/// Full PGN parser using Span-based scanning for maximum performance.
/// Supports variations, comments, NAGs, and produces PgnGame output.
module FullPGNParser =  

  // ============================================================================
  // Shared state for parsing
  // ============================================================================
  
  // Header state
  let mutable private gameNumber = 0
  let mutable private event' = ""
  let mutable private site = ""
  let mutable private date = ""
  let mutable private round = ""
  let mutable private white = ""
  let mutable private black = ""
  let mutable private result = ""
  let mutable private resultFoundInMovetext = false
  let mutable private fen = ""
  let mutable private reason = Misc.ResultReason.NotStarted
  let mutable private openingHash = ""
  let mutable private gameTime = 0L
  let mutable private plyCount = 0
  let mutable private openingName = ""
  let mutable private deviations = 0
  let private otherTags = ResizeArray<Header>()

  let mutable private currentMoveNr = 1
  let mutable private whiteSan = ""
  let mutable private blackSan = ""
  let mutable private whiteComment = ""
  let mutable private blackComment = ""
  let mutable private pendingComment = ""

  // Variation-aware tree state
  let mutable private currentPly = 0
  let private mainlinePly = ResizeArray<PlyMove>(128)
  let private rootVariations = ResizeArray<PlyLine>()
  let mutable private currentLine: PlyLine = mainlinePly
  let private lineStack = Stack<PlyLine>()
  let private plyStack = Stack<int>()
  let mutable private lastParsedSan: string option = None

  // NAG state
  let mutable private pendingNags = ResizeArray<int>()

  // ============================================================================
  // Helper functions (inline for performance)
  // ============================================================================

  let inline private isDigit c = c >= '0' && c <= '9'
  let inline private isFile c = c >= 'a' && c <= 'h'
  let inline private isRank c = c >= '1' && c <= '8'
  let inline private isPiece c = c = 'K' || c = 'Q' || c = 'R' || c = 'B' || c = 'N'
  let inline private isPromoPiece c = isPiece c || c = 'k' || c = 'q' || c = 'r' || c = 'b' || c = 'n'

  // ============================================================================
  // State management
  // ============================================================================

  let private resetState () =
    event' <- ""
    site <- ""
    date <- ""
    round <- ""
    white <- ""
    black <- ""
    result <- ""
    resultFoundInMovetext <- false
    fen <- ""
    reason <- Misc.ResultReason.NotStarted
    openingHash <- ""
    gameTime <- 0L
    plyCount <- 0
    openingName <- ""
    deviations <- 0
    otherTags.Clear()
    currentMoveNr <- 1
    whiteSan <- ""
    blackSan <- ""
    whiteComment <- ""
    blackComment <- ""
    pendingComment <- ""
    currentPly <- 0
    mainlinePly.Clear()
    rootVariations.Clear()
    currentLine <- mainlinePly
    lineStack.Clear()
    plyStack.Clear()
    lastParsedSan <- None
    pendingNags.Clear()

  let private hasGame () =
     whiteSan <> "" || blackSan <> "" || mainlinePly.Count > 0

  let private hasHeaders () =
    white <> "" || black <> "" || event' <> "" || result <> ""

  // ============================================================================
  // PlyMove tree management
  // ============================================================================

  let private appendPlyMove (san: string) (color: string) =
    let ply = currentPly
    let moveNr = (ply / 2) + 1
    let nags = if pendingNags.Count > 0 then pendingNags |> Seq.toList else []
    let node =
      { Ply = ply
        MoveNumber = moveNr
        Color = color
        San = san
        Comment = ""
        Nags = nags
        Variations = ResizeArray() }
    currentLine.Add node
    currentPly <- currentPly + 1
    lastParsedSan <- Some san
    pendingNags.Clear()
    if color = "b" then
        whiteSan <- ""
        blackSan <- ""
        whiteComment <- ""
        blackComment <- ""

  let private tryUpdateLastNodeComment (cmt: string) =
    if not (String.IsNullOrWhiteSpace cmt) then
      match currentLine |> Seq.tryLast with
      | Some node -> node.Comment <- cmt
      | None ->
          match rootVariations |> Seq.tryLast with
          | Some line when line.Count > 0 ->
              let lastNode = line[line.Count - 1]
              lastNode.Comment <- cmt
          | _ -> ()

  // ============================================================================
  // Text-based header parsing
  // ============================================================================  
  
  let private parseHeaderTexLine (line: string) =
    let spanLine = line.AsSpan()
    let openBracket = spanLine.IndexOf('[')
    let closeBracket = spanLine.LastIndexOf(']')
    if openBracket >= 0 && closeBracket > openBracket then
      let headerSpan = spanLine.Slice(openBracket + 1, closeBracket - openBracket - 1)
      let firstQuote = headerSpan.IndexOf('"')
      let lastQuote = headerSpan.LastIndexOf('"')
      if firstQuote >= 0 && lastQuote > firstQuote then
        let key = headerSpan.Slice(0, firstQuote).Trim().ToString()
        let value = headerSpan.Slice(firstQuote + 1, lastQuote - firstQuote - 1).ToString()
        match key with
        | "Event" -> event' <- value
        | "Site" -> site <- value
        | "Date" -> date <- value
        | "Round" -> round <- value
        | "White" -> white <- value
        | "Black" -> black <- value
        | "Result" -> result <- value
        | "FEN" | "Fen" -> fen <- value
        | "Reason" -> 
            try reason <- Misc.stringToResultReason value
            with _ -> () // Ignore invalid reason values
        | "OpeningHash" -> openingHash <- value
        | "GameTime" -> 
            match Int64.TryParse(value) with
            | true, v -> gameTime <- v
            | _ -> ()
        | "Ply" -> 
            match Int32.TryParse(value) with
            | true, v -> plyCount <- v
            | _ -> ()
        | "Opening" -> 
            openingName <- value
            otherTags.Add({ Key = key; Value = value })
        | "Deviations" -> 
            match Int32.TryParse(value) with
            | true, v -> deviations <- v
            | _ -> ()
        | _ -> otherTags.Add({ Key = key; Value = value })
  
// ============================================================================
  // Span-based movetext parsing (iterative, with variation stack)
  // ============================================================================

  /// Parse movetext from a span, handling variations iteratively with an explicit stack
  let private parseMoveTextLine (line: string) =
    let spanLine = line.AsSpan()
    let len = spanLine.Length
    let mutable p = 0

    // Skip leading whitespace
    while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1

    // Handle full-line comments
    if p < len && (spanLine[p] = ';' || spanLine[p] = '%') then
      // Line comment - extract and store as pending
      if p + 1 < len then
        pendingComment <- new string(spanLine.Slice(p + 1).Trim())
      p <- len
    else
      while p < len do
        let prevP = p

        // Skip whitespace
        while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1
        if p >= len then ()
        else
          let c0 = spanLine[p]

          // New header encountered - stop parsing this line
          if c0 = '[' then
            p <- len

          // Comment block { ... }
          elif c0 = '{' then
            p <- p + 1
            let startComment = p
            while p < len && spanLine[p] <> '}' do p <- p + 1
            let comment = 
              if p > startComment then new string(spanLine.Slice(startComment, p - startComment).Trim())
              else ""
            if p < len && spanLine[p] = '}' then p <- p + 1
            // Attach comment to most recent move
            if blackSan <> "" then
              blackComment <- if blackComment = "" then comment else blackComment + " " + comment
              tryUpdateLastNodeComment comment
            elif whiteSan <> "" then
              whiteComment <- if whiteComment = "" then comment else whiteComment + " " + comment
              tryUpdateLastNodeComment comment
            else
              pendingComment <- if pendingComment = "" then comment else pendingComment + " " + comment

          // Variation start (
          elif c0 = '(' then
            p <- p + 1
            // Push current line and ply onto stack
            let variationLine = ResizeArray<PlyMove>()
            match currentLine |> Seq.tryLast with
            | Some parent -> parent.Variations.Add variationLine
            | None -> rootVariations.Add variationLine
            lineStack.Push currentLine
            plyStack.Push currentPly
            // Set ply to parent's ply + 1 for the variation
            let parentPly =
              match currentLine |> Seq.tryLast with
              | Some node -> node.Ply
              | None -> currentPly - 1
            currentPly <- parentPly + 1
            currentLine <- variationLine

          // Variation end )
          elif c0 = ')' then
            p <- p + 1
            // Pop back to parent line
            if lineStack.Count > 0 then
              currentLine <- lineStack.Pop()
            if plyStack.Count > 0 then
              currentPly <- plyStack.Pop()

          // NAG ($1, $14, etc.)
          elif c0 = '$' then
            p <- p + 1
            let startNag = p
            while p < len && isDigit(spanLine[p]) do p <- p + 1
            if p > startNag then
              let nagStr = new string(spanLine.Slice(startNag, p - startNag))
              match Int32.TryParse(nagStr) with
              | true, nagVal -> pendingNags.Add(nagVal)
              | _ -> ()

          // Game termination *
          elif c0 = '*' then
            p <- p + 1
            if result = "" then result <- "*"
            resultFoundInMovetext <- true

          // Move number (1. or 1... or just digits followed by dots)
          elif isDigit c0 && not (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-') then
            let mutable nr = 0
            while p < len && isDigit(spanLine[p]) do
              nr <- nr * 10 + (int spanLine[p] - int '0')
              p <- p + 1
            // Skip whitespace
            while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1
            // Skip dots (including Unicode ellipsis …)
            while p < len && (spanLine[p] = '.' || spanLine[p] = '…') do p <- p + 1
            if nr > 0 then currentMoveNr <- nr

          // Result tokens (1-0, 0-1, 1/2-1/2, ½-½)
          elif c0 = '1' || (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-' && (p + 2 >= len || spanLine[p + 2] <> '0')) || c0 = '½' then
            let tail = spanLine.Slice(p)
            if tail.StartsWith("1-0".AsSpan()) then
              if result = "" then result <- "1-0"; resultFoundInMovetext <- true
              p <- p + 3
            elif tail.StartsWith("0-1".AsSpan()) then
              if result = "" then result <- "0-1"; resultFoundInMovetext <- true
              p <- p + 3
            elif tail.StartsWith("1/2-1/2".AsSpan()) then
              if result = "" then result <- "1/2-1/2"; resultFoundInMovetext <- true
              p <- p + 7
            elif tail.StartsWith("½-½".AsSpan()) then
              if result = "" then result <- "1/2-1/2"; resultFoundInMovetext <- true
              p <- p + 3
            else
              // Fallback: maybe it's a SAN move starting with 0 (0-0 castling)
              let start = p
              if c0 = 'O' || (c0 = '0' && (p + 2) < len && spanLine[p + 2] = '0') then
                while p < len && (spanLine[p] = 'O' || spanLine[p] = '0' || spanLine[p] = 'o' || spanLine[p] = '-') do p <- p + 1
              if p > start then
                let san = new string(spanLine.Slice(start, p - start))
                // Check/checkmate suffix
                if p < len && (spanLine[p] = '+' || spanLine[p] = '#') then p <- p + 1
                // Annotation symbols
                while p < len && (spanLine[p] = '!' || spanLine[p] = '?') do p <- p + 1
                if san <> "" then
                  if whiteSan = "" then
                    if mainlinePly.Count = 0 && fen <> "" && fen.Contains(" b ") then
                      blackSan <- san
                      appendPlyMove san "b"
                      if pendingComment <> "" then
                        blackComment <- pendingComment
                        tryUpdateLastNodeComment pendingComment
                        pendingComment <- ""
                    else
                      whiteSan <- san
                      appendPlyMove san "w"
                      if pendingComment <> "" then
                        whiteComment <- pendingComment
                        tryUpdateLastNodeComment pendingComment
                        pendingComment <- ""
                  else
                    blackSan <- san
                    appendPlyMove san "b"
              else
                p <- p + 1

          // SAN moves (piece moves, pawn moves, castling)
          elif isPiece c0 || isFile c0 || c0 = 'O' || c0 = 'o' || c0 = '0' then
            let start = p
            // Castling (handles O-O, o-o, 0-0 notations)
            if c0 = 'O' || c0 = 'o' || (c0 = '0' && (p + 2) < len && spanLine[p + 2] = '0') then
              while p < len && (spanLine[p] = 'O' || spanLine[p] = '0' || spanLine[p] = 'o' || spanLine[p] = '-') do p <- p + 1
            else
              // Optional piece letter
              if p < len && isPiece(spanLine[p]) then p <- p + 1
              // Optional disambiguation file/rank
              if p < len && isFile(spanLine[p]) then p <- p + 1
              if p < len && isRank(spanLine[p]) then p <- p + 1
              // Optional capture
              if p < len && spanLine[p] = 'x' then p <- p + 1
              // Destination square
              if p < len && isFile(spanLine[p]) then p <- p + 1
              if p < len && isRank(spanLine[p]) then p <- p + 1
              // Promotion
              if p < len && spanLine[p] = '=' then
                p <- p + 1
                if p < len && (isPiece(spanLine[p]) || isPromoPiece(spanLine[p])) then p <- p + 1
              elif p < len && isPromoPiece(spanLine[p]) then p <- p + 1
              // Check/mate
              if p < len && (spanLine[p] = '+' || spanLine[p] = '#') then p <- p + 1
              // Annotation symbols (!?, ?!, !!, ??)
              while p < len && (spanLine[p] = '!' || spanLine[p] = '?') do p <- p + 1

            if p > start then
              let san = new string(spanLine.Slice(start, p - start))
              if san <> "" then
                if whiteSan = "" then
                  if mainlinePly.Count = 0 && fen <> "" && fen.Contains(" b ") then
                    blackSan <- san
                    appendPlyMove san "b"
                    if pendingComment <> "" then
                      blackComment <- pendingComment
                      tryUpdateLastNodeComment pendingComment
                      pendingComment <- ""
                  else
                    whiteSan <- san
                    appendPlyMove san "w"
                    if pendingComment <> "" then
                      whiteComment <- pendingComment
                      tryUpdateLastNodeComment pendingComment
                      pendingComment <- ""
                else
                  blackSan <- san
                  appendPlyMove san "b"
            else
              p <- p + 1

          else
            // Unknown token; advance to avoid infinite loop
            p <- p + 1

        // Safety: if no progress made this iteration, advance one char
        if p = prevP then p <- p + 1
  
  // ============================================================================
  // Build PgnGame from current state
  // ============================================================================

  let inline moveNumberCount ply =
    if ply % 2 = 0 then ply / 2 else (ply / 2) + 1

  let private buildGameFull (raw:string) : PgnGame =    
    gameNumber <- gameNumber + 1
    let metadata: GameMetadata =
      { Event = event'
        Site = site
        Date = date
        Round = round
        White = white
        Black = black
        Result = result
        Reason = reason
        OpeningHash = openingHash
        GameTime = gameTime
        Moves = moveNumberCount mainlinePly.Count
        OpeningName = openingName
        Fen = fen
        Deviations = deviations
        StartEvals = []
        OtherTags = otherTags |> Seq.toList }
    {
      GameNumber = gameNumber
      GameMetaData = metadata
      Mainline = ResizeArray(mainlinePly)
      RootVariations = ResizeArray(rootVariations)
      Comments = pendingComment
      Fen = fen
      Raw = raw
    }

  let private buildGame() : PgnGame =
    buildGameFull ""
  
  let private buildGameWithRaw raw : PgnGame =
    buildGameFull raw

  // ============================================================================
  // Public API
  // ============================================================================
  
/// Parse a PGN string, yielding full PgnGame records
  let parsePgnStringHelper (content: string) withRaw : seq<PgnGame> =    
    let rawLines = ResizeArray<string>()
    gameNumber <- 0
    resetState()
    let mutable inMoveText = false
    
    let buildGame () =
      if withRaw then
        let raw = String.concat "\n" rawLines
        rawLines.Clear()
        buildGameWithRaw raw
      else
        rawLines.Clear()
        buildGame()
    seq {
        use reader = new StringReader(content)
        let mutable currentLine = reader.ReadLine()
        while currentLine <> null do
          let trimmed = currentLine.TrimStart()
          if withRaw then
            rawLines.Add(currentLine)
          if String.IsNullOrEmpty trimmed then
            if inMoveText && (hasGame() || result <> "") then
              yield buildGame()
              resetState()
              inMoveText <- false
          elif trimmed.Length > 0 && trimmed[0] = '[' then
            if inMoveText && (hasGame() || result <> "") then
              yield buildGame()
              resetState()
              inMoveText <- false
            parseHeaderTexLine trimmed
          else
            inMoveText <- true
            parseMoveTextLine trimmed
            if resultFoundInMovetext && result <> "" && (result = "1-0" || result = "0-1" || result = "1/2-1/2" || result = "*") then
              yield buildGame()
              resetState()
              inMoveText <- false

          currentLine <- reader.ReadLine()

        if hasGame() || result <> "" then
          yield buildGame()
        }

  let parsePgnString (content: string) : seq<PgnGame> =
    parsePgnStringHelper content false
  
  let parsePgnStringWithRaw (content: string) : seq<PgnGame> =
    parsePgnStringHelper content true
      /// Parse a PGN file, yielding full PgnGame records with Raw field populated
  
  let parsePgnFileHelper (pgnFilePath: string) withRaw : seq<PgnGame> =    
    let rawLines = ResizeArray<string>()
    gameNumber <- 0
    resetState()
    let mutable inMoveText = false

    let mayAddRaw line =
      if withRaw then
        rawLines.Add(line)

    let buildGameWithRaw () =
      let game = buildGame()
      let raw = String.concat "\n" rawLines
      rawLines.Clear()
      if withRaw then
        { game with Raw = raw }
      else
        game

    seq {
        let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
        use reader = new StreamReader(pgnFilePath, options)

        while not reader.EndOfStream do
          let currentLine = reader.ReadLine()
          let trimmed = currentLine.TrimStart()          
          if String.IsNullOrEmpty trimmed then
            if inMoveText && (hasGame() || result <> "") then
              yield buildGameWithRaw()
              resetState()
              inMoveText <- false
            elif rawLines.Count > 0 then
              mayAddRaw currentLine
          elif trimmed.Length > 0 && trimmed[0] = '[' then
            if inMoveText && (hasGame() || result <> "") then
              yield buildGameWithRaw()
              resetState()
              inMoveText <- false
            mayAddRaw currentLine
            parseHeaderTexLine trimmed
          else
            mayAddRaw currentLine
            inMoveText <- true
            parseMoveTextLine trimmed
            if resultFoundInMovetext && result <> "" && (result = "1-0" || result = "0-1" || result = "1/2-1/2" || result = "*") then
              yield buildGameWithRaw()
              resetState()
              inMoveText <- false

        if hasGame() || result <> "" then
          yield buildGameWithRaw()
    }
    
  let parsePgnFileWithRaw (pgnFilePath: string) : seq<PgnGame> = parsePgnFileHelper pgnFilePath true

  let parsePgnFile (pgnFilePath: string) : seq<PgnGame> = parsePgnFileHelper pgnFilePath false  
  
  /// Convert a FullSpanParser game to moves as string list (for compatibility)
  let getMovesAsStrings (game: PgnGame) : string list = game.Mainline |> Seq.map (fun mv -> mv.San) |> Seq.toList

  /// Generate PGN string representation of a game on demand
  let toPgnString (game: PgnGame) : string =
    let sb = System.Text.StringBuilder()
    let meta = game.GameMetaData

    // Headers
    sb.AppendLine($"[Event \"{meta.Event}\"]") |> ignore
    sb.AppendLine($"[Site \"{meta.Site}\"]") |> ignore
    sb.AppendLine($"[Date \"{meta.Date}\"]") |> ignore
    sb.AppendLine($"[Round \"{meta.Round}\"]") |> ignore
    sb.AppendLine($"[White \"{meta.White}\"]") |> ignore
    sb.AppendLine($"[Black \"{meta.Black}\"]") |> ignore
    sb.AppendLine($"[Result \"{meta.Result}\"]") |> ignore

    if game.Fen <> "" && game.Fen <> "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" then
      sb.AppendLine($"[FEN \"{game.Fen}\"]") |> ignore
      sb.AppendLine("[SetUp \"1\"]") |> ignore

    // Other tags
    for header in meta.OtherTags do
      sb.AppendLine($"[{header.Key} \"{header.Value}\"]") |> ignore

    sb.AppendLine() |> ignore

    for move in game.Mainline do      
        if move.Color = "w" then
            sb.Append($"{move.MoveNumber}. {move.San} ") |> ignore
        else
            sb.Append($"{move.San} ") |> ignore
    // Result
    if meta.Result <> "" then
      sb.Append(meta.Result) |> ignore

    sb.ToString()

  let parsePgnStreamWithRaw (reader: StreamReader): seq<PgnGame> =
    parsePgnStringWithRaw (reader.ReadToEnd())
  
  let parsePgnStream (reader: StreamReader): seq<PgnGame> =
    parsePgnString (reader.ReadToEnd())

  let parseFullPgnGame (pgn:string) =
    match parsePgnString pgn |> Seq.tryHead with
    | Some game -> game
    | None -> PgnGame.Empty(0)

  /// Parse only headers from a PGN file (span-based, skips movetext for performance)
  let parsePgnFileHeadersOnly (pgnFilePath: string): seq<PgnGame> =    
    gameNumber <- 0
    resetState()
    let mutable inMoveText = false
    seq {
        let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
        use reader = new StreamReader(pgnFilePath, options)

        while not reader.EndOfStream do
          let currentLine = reader.ReadLine()      
          let trimmed = currentLine.TrimStart()

          if String.IsNullOrEmpty trimmed then
            // Empty line - transition from headers to movetext or end of game
            if inMoveText && hasHeaders() then
              // End of game - yield without parsing moves
              yield buildGame()
              resetState()
              inMoveText <- false
            elif hasHeaders() && not inMoveText then
              // Empty line after headers - now in movetext section
              inMoveText <- true
          elif trimmed.Length > 0 && trimmed[0] = '[' then
            // Header line
            if inMoveText && hasHeaders() then
              // New game starting - yield previous
              yield buildGame()
              resetState()
              inMoveText <- false
            parseHeaderTexLine trimmed
          else
            // Movetext line - just mark we're in movetext, don't parse
            inMoveText <- true
            // Only check for result tokens to properly terminate the game
            let trimmedStr = trimmed.ToString()
            if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
               trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
              if result = "" then
                if trimmedStr.Contains("1-0") then result <- "1-0"
                elif trimmedStr.Contains("0-1") then result <- "0-1"
                elif trimmedStr.Contains("1/2-1/2") then result <- "1/2-1/2"
                elif trimmedStr.Contains("*") then result <- "*"

        // Handle last game
        if hasHeaders() then
          yield buildGame()
    }

  /// Parse only headers from a PGN string (span-based, skips movetext for performance)
  let parsePgnStringHeadersOnly (content: string): seq<PgnGame> =   
    gameNumber <- 0
    resetState()
    let mutable inMoveText = false
    seq {
        use reader = new StringReader(content)
        let mutable currentLine = reader.ReadLine()
    
        while currentLine <> null do      
          let trimmed = currentLine.TrimStart()
          if String.IsNullOrEmpty trimmed then
            if inMoveText && hasHeaders() then
              yield buildGame()
              resetState()
              inMoveText <- false
            elif hasHeaders() && not inMoveText then
              inMoveText <- true
          elif trimmed.Length > 0 && trimmed[0] = '[' then
            if inMoveText && hasHeaders() then
              yield buildGame()
              resetState()
              inMoveText <- false
            parseHeaderTexLine trimmed
          else
            inMoveText <- true
            let trimmedStr = trimmed.ToString()
            if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
               trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
              if result = "" then
                if trimmedStr.Contains("1-0") then result <- "1-0"
                elif trimmedStr.Contains("0-1") then result <- "0-1"
                elif trimmedStr.Contains("1/2-1/2") then result <- "1/2-1/2"
                elif trimmedStr.Contains("*") then result <- "*"

          currentLine <- reader.ReadLine()

        if hasHeaders() then
          yield buildGame()
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