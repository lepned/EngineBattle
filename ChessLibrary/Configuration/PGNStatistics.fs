module ChessLibrary.PGNStatistics

open System
open System.IO
open System.Text
open ChessLibrary.PGNTypes
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open TypesDef.CoreTypes

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
