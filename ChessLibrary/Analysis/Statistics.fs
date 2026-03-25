module ChessLibrary.Statistics

open System
open System.Text

open PGNTypes
open PuzzleTypes
open ChessUtilities


module EloCalculator =

  // Define a custom exception type for errors
  type ErrorFunctionException(message: string) =
      inherit System.Exception(message)

  let log10 = log 10.0

  let inline eloDifferenceFromScore score =
    let s = max 0.000001 (min 0.999999 score)
    -log (1.0 / s - 1.0) * 400.0 / log10 + 0.0

  let calculateEloConfidenceInterval wins draws losses confidenceMultiplier =
      let totalGames = float (wins + draws + losses)
      let winningFraction = (float wins + 0.5 * float draws) / totalGames
      let variance = Math.Sqrt((float wins * Math.Pow(1.0 - winningFraction, 2.0) +
                                float losses * Math.Pow(winningFraction, 2.0) +
                                float draws * Math.Pow(0.5 - winningFraction, 2.0)) / totalGames)
      let minFraction = max 0.00001 (winningFraction - confidenceMultiplier * variance / Math.Sqrt(totalGames))
      let maxFraction = min 0.99999 (winningFraction + confidenceMultiplier * variance / Math.Sqrt(totalGames))
      (eloDifferenceFromScore minFraction, eloDifferenceFromScore winningFraction, eloDifferenceFromScore maxFraction)

  // Calculate the error based on the Elo confidence interval
  let calculateEloError wins draws losses =
      let numberOfGames = float (wins + draws + losses)
      let (minElo, _, maxElo) = calculateEloConfidenceInterval wins draws losses 1.72
      //printfn "Error calc: Min Elo: %f, Max Elo: %f" minElo maxElo
      let diff = (maxElo - minElo)
      if diff = 0.0 || Double.IsNaN minElo || Double.IsNaN maxElo then
        Double.PositiveInfinity
      else
        min 699.0 (diff / 2.0)

  // Calculate LOS using the error function
  //from https://www.chessprogramming.org/Match_Statistics#Likelihood_of_superiority
  let calculateLikelihoodOfSuperiority wins losses totalGames =
      0.5 + 0.5 * MathNet.Numerics.SpecialFunctions.Erf((float (wins - losses)) / sqrt (2.0 * totalGames))

  let eloDiffWDL wins draws losses =
      let winPercentage = (float (wins + 0.5 * draws)) / float (wins + losses + draws)
      eloDifferenceFromScore winPercentage

  let calculateIdealized_UHO_EloAndError wins losses ebFactor =
    if wins = 0.0 || losses = 0.0 then
        (0.0, 0.0)
    else
        let elo = Math.Round(100.0 * Math.Log10(wins / losses))
        let error = Math.Round(ebFactor * 100.0 / Math.Log(10.0) * Math.Sqrt(1.0 / wins + 1.0 / losses))
        elo, error


module Glicko2 =
    open MathNet.Numerics

    let pi = Math.PI
    let c = 0.2 // constant value; you can adjust this depending on your requirements

    let toGlickoScale (player: PlayerRecord) =
        let mu = (player.Rating - 1500.0) / 173.7178
        let phi = player.Deviation / 173.7178
        mu, phi, player.Volatility

    let fromGlickoScale (mu: float, phi: float, sigma: float) =
        let rating = 173.7178 * mu + 1500.0
        let deviation = 173.7178 * phi
        { Rating = rating; Deviation = deviation; Volatility = sigma }

    let fromPuzzleDataToPlayerRecord (player: CsvPuzzleData) (score:float) =
      { Rating = player.Rating; Deviation = player.RatingDeviation; Volatility = 0.06 }, score

    let g (phi: float) =
        1.0 / Math.Sqrt(1.0 + 3.0 * phi * phi / (pi * pi))

    let E (mu: float, muj: float, phij: float) =
        1.0 / (1.0 + Math.Exp(-g(phij) * (mu - muj)))

    let update player opponentsResults =
        let mu, phi, sigma = toGlickoScale player

        let varianceInv =
            Seq.fold
                (fun acc (opponent, score) ->
                    let muj, phij, _ = toGlickoScale opponent
                    acc + g(phij) * g(phij) * E(mu, muj, phij) * (1.0 - E(mu, muj, phij)))
                0.0 opponentsResults

        let variance = 1.0 / varianceInv

        let delta =
            variance *
            Seq.fold
                (fun acc (opponent, score) ->
                    let muj, phij, _ = toGlickoScale opponent
                    acc + g(phij) * (score - E(mu, muj, phij)))
                0.0 opponentsResults

        let a = Math.Log(sigma * sigma)
        let b = a + 10.0  // Setting b to be sufficiently far from a.

        let f x =
            (exp(x) * (delta * delta - phi * phi - variance - exp(x)))
            / (2.0 * (phi * phi + variance + exp(x))) * (phi * phi + variance + exp(x))
            - (x - a) / (c * c)

        let computeVolatility f a b = FindRoots.brent 100 1e-5 a b f

        let newVolatility =
          match computeVolatility f a b with
          |Some v ->
            let nV = exp(v / 2.0)
            nV
          |None -> 0.02

        let newPhiStar = Math.Sqrt(phi * phi + newVolatility * newVolatility)
        let newPhi = 1.0 / Math.Sqrt(1.0 / (newPhiStar * newPhiStar) + 1.0 / variance)
        let newMu = mu + newPhi * newPhi *
                    Seq.fold
                        (fun acc (opponent, score) ->
                            let muj, phij, _ = toGlickoScale opponent
                            acc + g(phij) * (score - E(mu, muj, phij)))
                        0.0 opponentsResults

        fromGlickoScale (newMu, newPhi, newVolatility)


    // Initial engine rating
    let mutable playerRating = { Rating = 1500.0; Deviation = 200.0; Volatility = 0.06 }

    let simulate () =
      // Sample set of 30 puzzles
      let puzzles : PlayerRecord seq =
          [ for i in 1..30 -> {Rating = 1400.0 + float i; Deviation = 60.0; Volatility = 0.06 } ]

      printfn "Initial Engine Rating:"
      printfn "%A" playerRating

      // Perform 40 iterations
      let finalEngine =
          Seq.fold
              (fun engine _ ->
                  // Simulate the engine's performance
                  //let scores = solvePuzzles engine puzzles
                  let avgScore = 1.0 //averagePerformance scores

                  // Treat the average score as the result against a "meta-puzzle"
                  let metaPuzzle = { Rating = Seq.average (puzzles |> Seq.map (fun p -> p.Rating)); Deviation = 60.0; Volatility = 0.06 }
                  update engine [(metaPuzzle, avgScore)]
              )
              playerRating [1..100]

      printfn "\nFinal Engine Rating after 10 iterations:"
      printfn "%A" finalEngine


module Pentanomial =
  type Bucket =
    | L2   // 0.0-2.0
    | L15  // 0.5-1.5
    | D    // 1.0-1.0
    | W15  // 1.5-0.5
    | W2   // 2.0-0.0

  type Counts =
    { L2: int
      L15: int
      D: int
      W15: int
      W2: int
      CompletedPairs: int
      IncompletePairs: int }
    static member Empty =
      { L2 = 0; L15 = 0; D = 0; W15 = 0; W2 = 0; CompletedPairs = 0; IncompletePairs = 0 }

  type EngineCounts =
    { Engine: string
      L2: int
      L15: int
      D: int
      W15: int
      W2: int
      CompletedPairs: int
      IncompletePairs: int }
    static member Empty engine =
      { Engine = engine
        L2 = 0; L15 = 0; D = 0; W15 = 0; W2 = 0; CompletedPairs = 0; IncompletePairs = 0 }
    member this.ToCounts() =
      { L2 = this.L2; L15 = this.L15; D = this.D; W15 = this.W15; W2 = this.W2
        CompletedPairs = this.CompletedPairs; IncompletePairs = this.IncompletePairs }

  let private isFinalResult (result: string) =
    match result with
    | "1-0" | "0-1" | "1/2-1/2" -> true
    | _ -> false

  let private scoreFor (engine: string) (game: PgnGame) : float option =
    if isFinalResult game.GameMetaData.Result |> not then None
    else
      let white = game.GameMetaData.White.Trim()
      let black = game.GameMetaData.Black.Trim()
      let engine = engine.Trim()
      if engine <> white && engine <> black then None
      else
        match game.GameMetaData.Result with
        | "1/2-1/2" -> Some 0.5
        | "1-0" -> if engine = white then Some 1.0 else Some 0.0
        | "0-1" -> if engine = black then Some 1.0 else Some 0.0
        | _ -> None

  let private bucketFromSum (sum: float) =
    let eps = 1e-9
    if abs (sum - 0.0) < eps then L2
    elif abs (sum - 0.5) < eps then L15
    elif abs (sum - 1.0) < eps then D
    elif abs (sum - 1.5) < eps then W15
    elif abs (sum - 2.0) < eps then W2
    else failwith $"Invalid pentanomial sum (expected 0/0.5/1/1.5/2): {sum}"

  let private stableOpeningHash (game: PgnGame) =
    if String.IsNullOrWhiteSpace game.GameMetaData.OpeningHash |> not then
        game.GameMetaData.OpeningHash
    else
      Hash.computeOpeningHashFromGame game

  let private incBucket (bucket: Bucket) (counts: Counts) =
    match bucket with
    | L2 -> { counts with L2 = counts.L2 + 1 }
    | L15 -> { counts with L15 = counts.L15 + 1 }
    | D -> { counts with D = counts.D + 1 }
    | W15 -> { counts with W15 = counts.W15 + 1 }
    | W2 -> { counts with W2 = counts.W2 + 1 }

  let private incBucketEngine (bucket: Bucket) (counts: EngineCounts) =
    match bucket with
    | L2 -> { counts with L2 = counts.L2 + 1 }
    | L15 -> { counts with L15 = counts.L15 + 1 }
    | D -> { counts with D = counts.D + 1 }
    | W15 -> { counts with W15 = counts.W15 + 1 }
    | W2 -> { counts with W2 = counts.W2 + 1 }

  let calculateAllMatchups (games: seq<PgnGame>) : ((string * string) * Counts) list =
    let finishedGames =
      games
      |> Seq.filter (fun g -> isFinalResult g.GameMetaData.Result)
      |> Seq.toList

    let gamePairsByOpeningAndEngines =
      finishedGames
      |> Seq.groupBy (fun g ->
          let a = g.GameMetaData.White.Trim()
          let b = g.GameMetaData.Black.Trim()
          let e1, e2 = if a <= b then (a, b) else (b, a)
          stableOpeningHash g, e1, e2)
      |> Seq.toList

    let perMatchupCompletedPairs =
      gamePairsByOpeningAndEngines
      |> Seq.choose (fun ((_, e1, e2), group) ->
          let group = group |> Seq.toList
          let w1b2 =
            group
            |> List.rev
            |> List.tryFind (fun g -> g.GameMetaData.White.Trim() = e1 && g.GameMetaData.Black.Trim() = e2)
          let w2b1 =
            group
            |> List.rev
            |> List.tryFind (fun g -> g.GameMetaData.White.Trim() = e2 && g.GameMetaData.Black.Trim() = e1)

          match w1b2, w2b1 with
          | Some g1, Some g2 ->
              let s1 = scoreFor e1 g1 |> Option.defaultValue 0.0
              let s2 = scoreFor e1 g2 |> Option.defaultValue 0.0
              Some ((e1, e2), s1 + s2)
          | _ -> None)
      |> Seq.toList

    let incompletePairsByMatchup =
      gamePairsByOpeningAndEngines
      |> Seq.choose (fun ((_, e1, e2), group) ->
          let group = group |> Seq.toList
          let hasW1b2 = group |> List.exists (fun g -> g.GameMetaData.White.Trim() = e1 && g.GameMetaData.Black.Trim() = e2)
          let hasW2b1 = group |> List.exists (fun g -> g.GameMetaData.White.Trim() = e2 && g.GameMetaData.Black.Trim() = e1)
          if hasW1b2 && hasW2b1 then None else Some (e1, e2))
      |> Seq.countBy id
      |> Map.ofSeq

    let completedPairsByMatchup =
      perMatchupCompletedPairs
      |> Seq.groupBy fst
      |> Seq.map (fun (matchup, items) -> matchup, (items |> Seq.map snd |> Seq.toList))
      |> Map.ofSeq

    let allMatchups =
      Seq.append (completedPairsByMatchup |> Map.keys) (incompletePairsByMatchup |> Map.keys)
      |> Seq.distinct
      |> Seq.sort

    allMatchups
    |> Seq.map (fun matchup ->
        let baseCounts = Counts.Empty
        let sums = completedPairsByMatchup |> Map.tryFind matchup |> Option.defaultValue []
        let completed = sums.Length
        let bucketCounts =
          sums
          |> Seq.map bucketFromSum
          |> Seq.countBy id
          |> Map.ofSeq

        let getCount bucket = bucketCounts |> Map.tryFind bucket |> Option.defaultValue 0
        let incomplete = incompletePairsByMatchup |> Map.tryFind matchup |> Option.defaultValue 0

        matchup,
        { baseCounts with
            L2 = getCount L2
            L15 = getCount L15
            D = getCount D
            W15 = getCount W15
            W2 = getCount W2
            CompletedPairs = completed
            IncompletePairs = incomplete })
    |> Seq.toList

  let calculatePerEngine (games: seq<PgnGame>) : EngineCounts list =
    let finishedGames =
      games
      |> Seq.filter (fun g -> isFinalResult g.GameMetaData.Result)
      |> Seq.toList

    let allEngines =
      finishedGames
      |> Seq.collect (fun g -> [ g.GameMetaData.White.Trim(); g.GameMetaData.Black.Trim() ])
      |> Seq.distinct
      |> Seq.sort
      |> Seq.toList

    let init =
      allEngines
      |> Seq.map (fun e -> e, EngineCounts.Empty e)
      |> Map.ofSeq

    let updateEngine engine updater (m: Map<string, EngineCounts>) =
      match m |> Map.tryFind engine with
      | None -> m |> Map.add engine (updater (EngineCounts.Empty engine))
      | Some current -> m |> Map.add engine (updater current)

    let groups =
      finishedGames
      |> Seq.groupBy (fun g ->
          let a = g.GameMetaData.White.Trim()
          let b = g.GameMetaData.Black.Trim()
          let e1, e2 = if a <= b then (a, b) else (b, a)
          stableOpeningHash g, e1, e2)

    let acc =
      groups
      |> Seq.fold (fun m ((_, e1, e2), group) ->
          let group = group |> Seq.toList
          let w1b2 =
            group
            |> List.rev
            |> List.tryFind (fun g -> g.GameMetaData.White.Trim() = e1 && g.GameMetaData.Black.Trim() = e2)
          let w2b1 =
            group
            |> List.rev
            |> List.tryFind (fun g -> g.GameMetaData.White.Trim() = e2 && g.GameMetaData.Black.Trim() = e1)

          match w1b2, w2b1 with
          | Some g1, Some g2 ->
              let sum1 =
                (scoreFor e1 g1 |> Option.defaultValue 0.0) +
                (scoreFor e1 g2 |> Option.defaultValue 0.0)
              let sum2 = 2.0 - sum1
              let b1 = bucketFromSum sum1
              let b2 = bucketFromSum sum2
              m
              |> updateEngine e1 (fun c -> c |> incBucketEngine b1 |> fun c2 -> { c2 with CompletedPairs = c2.CompletedPairs + 1 })
              |> updateEngine e2 (fun c -> c |> incBucketEngine b2 |> fun c2 -> { c2 with CompletedPairs = c2.CompletedPairs + 1 })
          | _ ->
              m
              |> updateEngine e1 (fun c -> { c with IncompletePairs = c.IncompletePairs + 1 })
              |> updateEngine e2 (fun c -> { c with IncompletePairs = c.IncompletePairs + 1 })
        ) init

    acc
    |> Map.values
    |> Seq.sortByDescending (fun c -> c.CompletedPairs, c.Engine)
    |> Seq.toList

  let pentanomialEloErrorAndLos (c: Counts) =
    let n = float c.CompletedPairs
    if n < 1.0 then (0.0, Double.PositiveInfinity, 0.5)
    else
      let scores = [| 0.0; 0.5; 1.0; 1.5; 2.0 |]
      let counts = [| float c.L2; float c.L15; float c.D; float c.W15; float c.W2 |]
      let mean = (Array.map2 (*) scores counts |> Array.sum) / n
      let variance =
        (Array.map2 (fun s cnt -> cnt * (s - mean) ** 2.0) scores counts |> Array.sum) / n
        |> Math.Sqrt
      let scoreFrac = mean / 2.0
      let se = variance / (2.0 * Math.Sqrt n)
      let z = 1.96
      let minFrac = max 0.00001 (scoreFrac - z * se)
      let maxFrac = min 0.99999 (scoreFrac + z * se)
      let elo = EloCalculator.eloDifferenceFromScore scoreFrac
      let minElo = EloCalculator.eloDifferenceFromScore minFrac
      let maxElo = EloCalculator.eloDifferenceFromScore maxFrac
      let diff = maxElo - minElo
      let error =
        if diff = 0.0 || Double.IsNaN minElo || Double.IsNaN maxElo then Double.PositiveInfinity
        else min 699.0 (diff / 2.0)
      let los =
        if se > 0.0 then
          0.5 + 0.5 * MathNet.Numerics.SpecialFunctions.Erf((scoreFrac - 0.5) / (se * Math.Sqrt 2.0))
        else 0.5
      (elo, error, los)

  let formatAllMatchups (games: seq<PgnGame>) (maxLines: int) =
    let data = calculateAllMatchups games
    if data.IsEmpty then ""
    else
      let shorten (maxLen: int) (s: string) =
        if s.Length <= maxLen then s
        elif maxLen <= 2 then s.Substring(0, maxLen)
        else s.Substring(0, maxLen - 2) + ".."

      let sb = StringBuilder()
      sb.AppendLine "\n```\n" |> ignore
      sb.AppendLine "Pentanomial (pairs per opening; left engine perspective)" |> ignore

      let lines = data |> List.truncate (max 0 maxLines)
      let matchupStrings =
        lines
        |> List.map (fun ((a, b), _) -> $"{a} vs {b}")
        |> List.map (shorten 60)

      let matchupWidth =
        let minW = "Matchup".Length
        if matchupStrings.IsEmpty then minW else max minW (matchupStrings |> List.maxBy String.length |> String.length)

      let header =
        sprintf "%-*s  %s  %4s  %3s  %7s  %6s"
          matchupWidth "Matchup" "[0-2,0.5-1.5,1-1,1.5-0.5,2-0]" "Done" "Inc" "Elo" "Error"
      sb.AppendLine header |> ignore
      sb.AppendLine (String.replicate header.Length "-") |> ignore

      for i in 0 .. lines.Length - 1 do
        let ((_, _), c) = lines.[i]
        let matchup = matchupStrings.[i].PadRight(matchupWidth)
        let buckets = sprintf "[%d, %d, %d, %d, %d]" c.L2 c.L15 c.D c.W15 c.W2
        let elo, error, _ = pentanomialEloErrorAndLos c
        let eloStr = if Double.IsNaN elo then "---" else sprintf "%7.1f" elo
        let errStr =
          if error = Double.PositiveInfinity then "    \u221E"
          elif Double.IsNaN error then "  ---"
          else sprintf "%6.1f" error
        sb.AppendLine(sprintf "%s  %-29s  %4d  %3d  %s  %s" matchup buckets c.CompletedPairs c.IncompletePairs eloStr errStr) |> ignore

      if data.Length > lines.Length then
        sb.AppendLine($"... truncated ({lines.Length}/{data.Length} matchups shown)") |> ignore
      sb.AppendLine "\n```\n" |> ignore
      sb.ToString()

  let formatAllMatchupsDefault (games: seq<PgnGame>) =
    formatAllMatchups games 200

  let formatSingleMatchupCompact (games: seq<PgnGame>) =
    let data = calculateAllMatchups games
    match data with
    | [ ((a, _), c) ] ->
        let elo, error, los = pentanomialEloErrorAndLos c
        let eloStr = if Double.IsNaN elo then "---" else sprintf "%.1f" elo
        let errStr =
          if error = Double.PositiveInfinity then "\u221E"
          elif Double.IsNaN error then "---"
          else sprintf "%.1f" error
        let cfs = los * 100.0
        let cfsStr = if Double.IsNaN cfs then "---" else sprintf "%.1f" cfs
        let incStr = if c.IncompletePairs > 0 then sprintf " (+%d inc)" c.IncompletePairs else ""
        sprintf "\nPentanomial [%d, %d, %d, %d, %d] Pairs: %d%s | Elo: %s \u00B1%s (95%%) CFS: %s%% [%s]\n"
          c.L2 c.L15 c.D c.W15 c.W2 c.CompletedPairs incStr eloStr errStr cfsStr a
    | _ -> formatAllMatchups games 200

  let formatPerEngine (games: seq<PgnGame>) (maxLines: int) =
    let data = calculatePerEngine games
    if data.IsEmpty then ""
    else
      let shorten (maxLen: int) (s: string) =
        if s.Length <= maxLen then s
        elif maxLen <= 2 then s.Substring(0, maxLen)
        else s.Substring(0, maxLen - 2) + ".."

      let sb = StringBuilder()
      sb.AppendLine "\n```\n" |> ignore
      sb.AppendLine "Pentanomial vs field (pairs per opening; engine perspective)" |> ignore

      let lines = data |> List.truncate (max 0 maxLines)
      let engineStrings = lines |> List.map (fun e -> shorten 40 e.Engine)
      let engineWidth =
        let minW = "Engine".Length
        if engineStrings.IsEmpty then minW else max minW (engineStrings |> List.maxBy String.length |> String.length)

      let header =
        sprintf "%-*s  %s  %4s  %3s"
          engineWidth "Engine" "[0-2,0.5-1.5,1-1,1.5-0.5,2-0]" "Done" "Inc"
      sb.AppendLine header |> ignore
      sb.AppendLine (String.replicate header.Length "-") |> ignore

      for i in 0 .. lines.Length - 1 do
        let e = lines.[i]
        let name = engineStrings.[i].PadRight(engineWidth)
        let buckets = sprintf "[%d, %d, %d, %d, %d]" e.L2 e.L15 e.D e.W15 e.W2
        sb.AppendLine(sprintf "%s  %-29s  %4d  %3d" name buckets e.CompletedPairs e.IncompletePairs) |> ignore

      if data.Length > lines.Length then
        sb.AppendLine($"... truncated ({lines.Length}/{data.Length} engines shown)") |> ignore

      sb.AppendLine "\n```\n" |> ignore
      sb.ToString()

  let formatPerEngineDefault (games: seq<PgnGame>) =
    formatPerEngine games 200
