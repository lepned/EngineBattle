module ChessLibrary.GameAnalysis

open System
open System.IO
open System.Text
open System.Collections.Generic

open TypesDef.CoreTypes
open Statistics
open RuntimeUtilities

module Formatting =

  /// Formats a metric value with SI prefixes (G, M, K)
  let inline private formatMetric (unit: string) (value: double) =
    if value >= 1_000_000_000.0 then sprintf "%.1fG%s" (value / 1_000_000_000.0) unit
    elif value >= 1_000_000.0 then sprintf "%.1fM%s" (value / 1_000_000.0) unit
    elif value >= 1_000.0 then sprintf "%.1fK%s" (value / 1_000.0) unit
    else sprintf "%.1f %s" value unit

  let inline formatNPS (nps: double) = formatMetric "nps" nps

  let inline formatEPS (eps: double) =
    if eps = 0.0 then "NA" else formatMetric "eps" eps

  let inline formatNPM (npm: double) =
    if npm >= 1_000_000_000.0 then sprintf "%.1fGnpm" (npm / 1_000_000_000.0)
    elif npm >= 1_000_000.0 then sprintf "%.1fMnpm" (npm / 1_000_000.0)
    elif npm >= 1_000.0 then sprintf "%.1fKnpm" (npm / 1_000.0)
    else sprintf "%.0f npm" npm

  let formatMoveTime (moveTime: int64) =
    let hours = moveTime / 3600000L
    let remainingAfterHours = moveTime % 3600000L
    let minutes = remainingAfterHours / 60000L
    let remainingAfterMinutes = remainingAfterHours % 60000L
    let seconds = remainingAfterMinutes / 1000L
    let milliseconds = remainingAfterMinutes % 1000L

    let formatTime (h: int64) (m: int64) (s: int64) (ms: int64) =
        let hourStr = if h > 0L then sprintf "%dh" h else ""
        let minuteStr = if m > 0L then sprintf "%dm" m else ""
        let secondStr = if s > 0L then sprintf "%ds" s else ""
        let millisecondStr = if m = 0L && s = 0L && ms > 0L then sprintf "%dms" ms else ""
        sprintf "%s%s%s%s" hourStr minuteStr secondStr millisecondStr

    formatTime hours minutes seconds milliseconds

/// The standings tables as the console prints them: rows, header, pairs, the crosstable matrix.
module StandingsTable =

  let calcPadding (minPadding: int) (delta: int) (selector: 'a -> string) (data: seq<'a>) =
    let lengths = data |> Seq.map (selector >> String.length)
    if Seq.isEmpty lengths then minPadding
    else max ((Seq.max lengths) + delta) minPadding

  let lossCombinations = [ "00"; "01/2"; "1/20" ]
  let winCombinations = [ "11"; "11/2"; "1/21" ]

  let calculatePairs (results: string[]) =
    let pairs =
        results
        |> Seq.take (results.Length - results.Length % 2) // Ensure even number of elements
        |> Seq.chunkBySize 2
        |> Seq.filter (fun arr -> arr.Length = 2)
        |> Seq.map (fun arr -> arr.[0] + arr.[1])

    let wins, draws, losses =
        pairs
        |> Seq.fold (fun (w, d, l) addThem ->
            if List.contains addThem winCombinations then
                if addThem = "11" then (w + 2, d, l)
                else (w + 1, d, l)
            elif List.contains addThem lossCombinations then
                if addThem = "00" then (w, d, l + 2)
                else (w, d, l + 1)
            else (w, d + 1, l)
        ) (0, 0, 0)
    (wins, draws, losses)

  let getAllPairs (entry: CrossTableEntry) =
    entry.ResultsAgainst
    |> Array.fold (fun (accWins, accDraws, accLosses) (_, results) ->
        let wins, draws, losses = calculatePairs results
        (accWins + wins, accDraws + draws, accLosses + losses)) (0, 0, 0)

  let populatePairData (engines: PlayerResult seq) (table: CrossTableEntry seq) =
    if table |> Seq.length = 2 then
      (table |> Seq.head).Challenger <- true
    for p in engines do
      match table |> Seq.tryFind (fun e -> e.Player = p.Player) with
      | Some t ->
          p.Challenger <- t.Challenger
          let w, _, l = getAllPairs t
          p.PairWins <- w
          p.PairLosses <- l
      | _ -> ()

  /// The order of every table: rating, then points per game, NaN last; the CFS is against the next
  /// player in this order.
  let tableOrder (p: PlayerResult) = (if Double.IsNaN p.Elo then Double.NegativeInfinity else p.Elo), p.ScoreRate

  let writeResultHeader (n:int) (speedPadding:int) : string =
    let label = "Speed"
    let leftPad =
      let diff = speedPadding - label.Length
      if diff > 0 then diff / 2 else 0
    let rightPad =
      let diff = speedPadding - label.Length - leftPad
      if diff > 0 then diff else 0
    let speedHeader = (String.replicate leftPad " ") + label + (String.replicate rightPad " ")
    sprintf "%-*s : %7s %6s %7s %7s %4s %6s   %*s %5s %5s %5s %5s %7s %7s %8s"
            n "# PLAYER" "ELO" "ERROR" "POINTS" "PLAYED" "(%)" "CFS%" speedPadding speedHeader "W" "D" "L" "D(%)" "WScore" "BScore" "Pairs"

  let private formatElo (elo: float) =
    if Double.IsNaN(elo) then "---"
    elif elo = System.Double.PositiveInfinity then ConsoleUtils.positiveInfinitySymbol
    elif elo = System.Double.NegativeInfinity then ConsoleUtils.negativeInfinitySymbol
    // -0.0 is 0.0
    else sprintf "%7.1f" (if abs elo < 0.05 then 0.0 else elo)

  // an anchor has no error: its rating is the reference
  let private formatError (error: float) =
      if error = 0.0 then "----"
      elif Double.IsNaN(error) then "---"
      elif error = System.Double.PositiveInfinity then ConsoleUtils.positiveInfinitySymbol
      elif error = System.Double.NegativeInfinity then ConsoleUtils.negativeInfinitySymbol
      else error.ToString("F1")

  let writeEngineLineForPlayer (p: PlayerResult) n (speed: string) (speedPadding: int) =
      let elo = formatElo p.Elo
      let error = formatError p.Error
      // CFS against the next row; the last row has none
      let cfs = if p.CFS < 0 then "---" else string p.CFS
      sprintf "%-*s : %7s %6s %7.1f %7d %4d %6s   %-*s %5d %5d %5d %5d %7.1f %7.1f %8s"
              n p.Player elo error p.Points p.Played p.Percent cfs speedPadding speed p.Win p.Draw p.Loss p.D p.WhiteScore p.BlackScore p.PairsString

  let printStatsMatrix (table: CrossTableEntry seq) =
    let endOfLine = "\n```\n"
    let players = table |> Seq.map (fun t -> t.Player) |> Seq.toArray
    // Empty table (e.g. non-PGN input parsed to zero games) must not crash Seq.max
    let longest = if Array.isEmpty players then 8 else (players |> Seq.map String.length |> Seq.max) + 2
    let columnWidth = 13

    let getShortenedString (s: string) =
        if s.Length > 10 then s.Substring(0, 10) + ".." else s

    let header =
        let playerHeader = sprintf "%-*s" longest "Player"
        let columns = players |> Array.map getShortenedString |> Array.map (sprintf "%-*s" columnWidth) |> String.concat " "
        sprintf "%s %s %s\n" playerHeader columns "Score"

    let rows =
        table
        |> Seq.map (fun t ->
            let row =
                players
                |> Array.map (fun opponent ->
                    match t.StatsAgainst |> Seq.tryFind (fun (o, _) -> o = opponent) with
                    | Some (_, stats) -> sprintf "%d-%d-%d" stats.Wins stats.Draws stats.Losses
                    | None -> "X-X-X"
                )
                |> Array.map (sprintf "%-*s" columnWidth)
                |> String.concat " "
            let played = t.StatsAgainst |> Seq.sumBy (fun (_, stats) -> stats.Wins + stats.Draws + stats.Losses)
            sprintf "%-*s %s %.1f/%.0f" longest t.Player row t.TotalScore (float played)
        )
        |> String.concat "\n"

    let totalGames = table |> Seq.sumBy (fun t -> t.TotalScore) |> int
    sprintf "%s%s\n%s\n(%d games)\n%s" endOfLine header rows totalGames endOfLine

  let getResultsAndPairsInConsoleFormat (engines: PlayerResult seq) (table: CrossTableEntry seq) =
      let sb = new StringBuilder()
      let appendLine (txt: string) = sb.AppendLine txt |> ignore

      appendLine "\n```\n"

      let longest =
          if Seq.isEmpty engines then 10
          else engines |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> e.Player.Length + 2

      // strongest first, as the fit orders them (the CFS is against the next row)
      let sortedPlayers = engines |> Seq.sortByDescending tableOrder |> Seq.toList

      let speedStrings =
          sortedPlayers
          |> List.map (fun p ->
              let baseSpeed = Formatting.formatNPS p.MedSpeed
              if p.EPS > 0.0 then $"{baseSpeed} ({Formatting.formatEPS p.EPS})"
              else baseSpeed)

      let speedPadding = calcPadding 8 0 id speedStrings
      writeResultHeader longest speedPadding |> appendLine

      for (p, speed) in List.zip sortedPlayers speedStrings do
          writeEngineLineForPlayer p longest speed speedPadding |> appendLine

      appendLine "\n```"
      sb.ToString()

  let getIdealized_UHO_Elo (table:CrossTableEntry seq) =
    let eb = 2.0
    [
      for t in table do
        let (wins,draws, losses) = getAllPairs t
        let (elo, error) = EloCalculator.calculateIdealized_UHO_EloAndError wins losses eb
        t.Player, t.TotalScore, elo, error, wins, draws, losses
    ]


module PGNCalculator =
  // Define a function to get a list of all players from a list of results

  // Define a function to get a list of all pairs of players from a list of results

// Define a function to calculate the outcome for a given player and result
  let calculateOutcome player (result: Result) : Outcome =
      if result.Result = "1-0" && result.Player1 = player then
          Win result.Player1
      elif result.Result = "0-1" && result.Player2 = player then
          Win result.Player2
      elif result.Result = "1-0" && result.Player2 = player then
          Loss result.Player1
      elif result.Result = "0-1" && result.Player1 = player then
          Loss result.Player2
      elif result.Result = "1/2-1/2" then
          Outcome.Draw
      else
          NotPlayed //failwith $"Invalid result format for {player}\n{result}"

   // Define a function to calculate the statistics for a given player and list of results
  let calculateStatisticsForPlayer player (results:Result seq) =
      let results = results |> Seq.toList
      let outcomes =
        results
        |> List.filter (fun r -> r.Player1 = player || r.Player2 = player)
        |> List.map (calculateOutcome player)
      if outcomes.Length > 0 then
        let winCount = outcomes |> List.filter (fun o -> o = Win player) |> List.length
        let lossCount = outcomes |> List.filter (fun o -> match o with | Loss _ -> true | _ -> false) |> List.length
        let drawCount = outcomes |> List.filter (fun o -> o = Outcome.Draw) |> List.length
        let whiteOutcome = results |> List.filter (fun r -> r.Player1 = player) |> List.map (calculateOutcome player)
        let whiteWin, whiteDraw =
          whiteOutcome |> List.filter (fun o -> o = Win player) |> List.length,
          whiteOutcome |> List.filter (fun o -> o = Outcome.Draw) |> List.length
        let blackOutcome = results |> List.filter (fun r -> r.Player2 = player) |> List.map (calculateOutcome player)
        let blackWin, blackDraw =
          blackOutcome |> List.filter (fun o -> o = Win player) |> List.length,
          blackOutcome |> List.filter (fun o -> o = Outcome.Draw) |> List.length
        let blackLoss, whiteLoss =
          (blackOutcome |> List.length) - blackWin - blackDraw,
          (whiteOutcome |> List.length) - whiteWin - whiteDraw

        let maxScore = outcomes.Length
        let eloEstimate = EloCalculator.eloDiffWDL (float winCount) (float drawCount) (float lossCount)
        let points = float winCount + (float drawCount / 2.0)
        let error = EloCalculator.calculateEloError winCount drawCount lossCount
        let cfs = (EloCalculator.calculateLikelihoodOfSuperiority winCount lossCount (float (winCount + lossCount))) * 100. |> int32 |> max 0  //remove negative numbers when score is perfect
        // maxScore is the player's game count, which is 0 for a player listed in the results
        // but never paired. Without the guard the division is NaN and the int32 conversion
        // turns it into a number that looks real in the table.
        let percent = if maxScore > 0 then (points / float maxScore) * 100.0 |> int32 else 0
        let played = maxScore
        let dPercent =
          if maxScore > 0 then (float drawCount / float maxScore) * 100.0 |> int32 else 0
        //todo
        let pairWins = 0
        let pairLosses = 0
        createPlayerResult player (Math.Round(points,1)) (Math.Round(eloEstimate,0)) error played percent cfs
          winCount drawCount lossCount dPercent (whiteWin, whiteDraw, whiteLoss) (blackWin, blackDraw, blackLoss) pairWins pairLosses

      else
        createPlayerResult player 0.0 0.0 0.0 0 0 0 0 0 0 0 (0,0,0) (0,0,0) 0 0

  let getResultsFromPGNGames (pgns: PGNTypes.PgnGame seq) =
      pgns
      |> Seq.map PGNWriter.getResultsFromPGNGame
      |> Seq.toArray

  /// The games as the rating fit takes them (Player1 is White); an unfinished game is none.
  let ratingGames (results: Result seq) =
    results
    |> Seq.choose (fun r ->
        let outcome =
          match r.Result with
          | "1-0" -> Some RatingFit.WhiteWin
          | "0-1" -> Some RatingFit.BlackWin
          | "1/2-1/2" -> Some RatingFit.Draw
          | _ -> None
        outcome |> Option.map (fun o -> { RatingFit.White = r.Player1; RatingFit.Black = r.Player2; RatingFit.Outcome = o }))

  /// One rating fit of every player at once (RatingFit, what Ordo did): with two players the
  /// first is the anchor at 0, as the tables show it; otherwise the pool average is 0.
  let ratingFit (players: string list) (results: Result seq) =
    let games = ratingGames results |> Seq.toArray
    if games.Length = 0 then None
    else
      let anchor = match players with [ a; _ ] -> Some a | _ -> None
      Some (RatingFit.fit { RatingFit.defaultOptions with Anchor = anchor } games)

  /// Each player's Elo, error and CFS from the fit (CFS against the next in StandingsTable.tableOrder,
  /// -1 for the last). A player the fit has no games of keeps its own numbers.
  let withRatingFit (fit: RatingFit.Fit option) (players: PlayerResult seq) =
    match fit with
    | None -> players |> Seq.toList
    | Some f ->
      let rated = f.Players |> List.map (fun p -> p.Name, p) |> dict
      let withRating =
        [ for p in players ->
            match rated.TryGetValue p.Player with
            | true, r -> { p with Elo = r.Rating; Error = r.Error }
            | _ -> p ]
      // the chain of "next" follows the table order, ties included
      let next =
        withRating
        |> List.filter (fun p -> rated.ContainsKey p.Player)
        |> List.sortByDescending StandingsTable.tableOrder
        |> List.map (fun p -> p.Player)
        |> List.pairwise
        |> dict
      [ for p in withRating ->
          if rated.ContainsKey p.Player then
              let cfs =
                match next.TryGetValue p.Player with
                | true, n -> let c = f.Cfs p.Player n in if Double.IsNaN c then -1 else int (Math.Round(100.0 * c))
                | _ -> -1
              { p with CFS = cfs }
          else p ]

  /// The lines under a table: the fit's white advantage and draw rate, and a note when the games
  /// do not connect every player.
  let ratingFitLines (fit: RatingFit.Fit option) =
    match fit with
    | None -> ""
    | Some f ->
        sprintf "White advantage = %.1f +/- %.1f (not in the ratings)\nDraw rate (equal opponents) = %.2f %% +/- %.2f\n" (if abs f.WhiteAdvantage < 0.05 then 0.0 else f.WhiteAdvantage) f.WhiteAdvantageError f.DrawRate f.DrawRateError
        + (if f.Groups.Length > 1 then sprintf "%d groups of players the games do not connect: ratings compare within a group only\n" f.Groups.Length else "")
        // a perfect score has no rating of its own: a floor or a ceiling
        + (f.Players
           |> List.choose (fun p ->
               match p.Bound with
               | RatingFit.Floor -> Some (sprintf "%s won every game: rated as if one were a draw, so at least %.1f\n" p.Name p.Rating)
               | RatingFit.Ceiling -> Some (sprintf "%s lost every game: rated as if one were a draw, so at most %.1f\n" p.Name p.Rating)
               | RatingFit.Exact -> None)
           |> String.concat "")

  /// The players rated again with this anchor (None: the pool average), and the fit.
  let rerate (anchor: string option) (results: Result seq) (players: PlayerResult seq) =
    let games = ratingGames results |> Seq.toArray
    let fit = if games.Length = 0 then None else Some (RatingFit.fit { RatingFit.defaultOptions with Anchor = anchor } games)
    withRatingFit fit players, fit

  /// The standings as the console prints them: the table, the fit's lines under it and, for two
  /// players, the paired-openings line.
  let standingsText (players: PlayerResult list) (cross: CrossTableEntry seq) (fit: RatingFit.Fit option) (pgnGames: PGNTypes.PgnGame seq) =
    let standings = StandingsTable.getResultsAndPairsInConsoleFormat players cross + ratingFitLines fit
    if players.Length = 2 then standings + Pentanomial.formatSingleMatchupCompact pgnGames else standings

  /// Every player's statistics, with the rating fit (also returned, for the lines under the table).
  let getFullStatAndFit results =
    let results = Seq.toArray results
    let players =
      [ for r in results do
          yield r.Player1
          yield r.Player2
      ] |> Seq.distinct |> List.ofSeq
    let fit = ratingFit players results
    // Bucket the results by player in one pass. calculateStatisticsForPlayer only ever
    // looks at that player's own games, so handing it the pre-filtered list is
    // equivalent — but it turns an O(players x games) scan (which also re-materialized
    // the whole result set per player) into O(games). Engine tournaments never noticed;
    // a PGN archive with tens of thousands of names ground for minutes.
    let byPlayer = Dictionary<string, ResizeArray<Result>>()
    let add player r =
      match byPlayer.TryGetValue player with
      | true, xs -> xs.Add r
      | _ ->
        let xs = ResizeArray<Result>()
        xs.Add r
        byPlayer[player] <- xs
    for r in results do
      add r.Player1 r
      // a self-play result must count once, exactly as the old filter did
      if r.Player2 <> r.Player1 then add r.Player2 r
    seq {
          for player in players do
            let games =
              match byPlayer.TryGetValue player with
              | true, xs -> xs :> Result seq
              | _ -> Seq.empty
            let res = calculateStatisticsForPlayer player games
            yield res }
    |> withRatingFit fit, fit

  let getFullStatFromResults results = fst (getFullStatAndFit results) :> PlayerResult seq

  let opponentsList (results: Result list) p1 =
    let players = results |> List.collect (fun r -> [r.Player1; r.Player2]) |> List.distinct
    let opponentResults =
        players
        |> List.toArray
        |> Array.filter (fun p2 -> p1 <> p2)  // Exclude self
        |> Array.map (fun p2 ->
            let matchesAgainst =
                results
                |> List.filter (fun r -> (r.Player1 = p1 && r.Player2 = p2) || (r.Player1 = p2 && r.Player2 = p1))
            let resultsStringList =
                matchesAgainst
                |> List.toArray
                |> Array.map (fun m ->
                    match m.Result with
                    | "1-0" -> if m.Player1 = p1 then "1" else "0"
                    | "0-1" -> if m.Player1 = p1 then "0" else "1"
                    | "1/2-1/2" -> "1/2"
                    | _ -> failwith "Invalid result")
            (p2, resultsStringList)
        )
        |> Array.filter (fun (_, resultsList) -> resultsList.Length > 0)  // Remove opponents with no games played
    opponentResults

  let createStatsCrossTableSummary (results: Result list) (challengerList: string list) players =

      // Arbitrary user PGNs legitimately contain unscored games ("*" for
      // unfinished/aborted, or an empty Result). Skip those here instead of
      // failing the whole crosstable — the old failwith escaped out of a sort
      // comparer and killed the Ratings page's circuit.
      let results =
          results
          |> List.filter (fun r -> r.Result = "1-0" || r.Result = "0-1" || r.Result = "1/2-1/2")

      // One-pass indexes. The old code re-filtered the FULL result list per player
      // pair (and the sort projection re-scanned it per comparison), which is
      // O(players² × games) — a TCEC archive (27,896 games, 1384 engine names)
      // effectively never finished. With the indexes this is O(games + players²).
      let playerGames = Dictionary<string, ResizeArray<Result>>()
      let pairGames = Dictionary<struct (string * string), ResizeArray<Result>>()
      let pairKey (a: string) (b: string) =
          if String.CompareOrdinal(a, b) <= 0 then struct (a, b) else struct (b, a)
      let addTo (dict: Dictionary<'k, ResizeArray<Result>>) key r =
          match dict.TryGetValue key with
          | true, l -> l.Add r
          | _ ->
              let l = ResizeArray<Result>()
              l.Add r
              dict.[key] <- l
      // Distinct players in first-appearance order (opponentsList's original ordering)
      let seenPlayers = HashSet<string>()
      let orderedPlayers = ResizeArray<string>()
      for r in results do
          addTo playerGames r.Player1 r
          addTo playerGames r.Player2 r
          addTo pairGames (pairKey r.Player1 r.Player2) r
          if seenPlayers.Add r.Player1 then orderedPlayers.Add r.Player1
          if seenPlayers.Add r.Player2 then orderedPlayers.Add r.Player2

      let emptyGames = ResizeArray<Result>()
      let gamesOf p =
          match playerGames.TryGetValue p with
          | true, l -> l
          | _ -> emptyGames
      let gamesBetween p1 p2 =
          match pairGames.TryGetValue (pairKey p1 p2) with
          | true, l -> l
          | _ -> emptyGames

      let computeScore player =
          let games = gamesOf player
          if games.Count = 0 then 0.0, 0.0
          else
              let mutable score = 0.0
              for r in games do
                  score <- score +
                      (match r.Result with
                       | "1-0" -> if r.Player1 = player then 1.0 else 0.0
                       | "0-1" -> if r.Player1 = player then 0.0 else 1.0
                       | "1/2-1/2" -> 0.5
                       | _ -> failwith "Invalid result")
              score, score / float games.Count
      let scoreCache = Dictionary<string, float * float>()
      let scoreOfPlayer player =
          match scoreCache.TryGetValue player with
          | true, s -> s
          | _ ->
              let s = computeScore player
              scoreCache.[player] <- s
              s

      let statsBetween p1 p2 =
          let mutable w, d, l = 0, 0, 0
          for m in gamesBetween p1 p2 do
              match m.Result with
              | "1-0" -> if m.Player1 = p1 then w <- w + 1 else l <- l + 1
              | "0-1" -> if m.Player1 = p1 then l <- l + 1 else w <- w + 1
              | "1/2-1/2" -> d <- d + 1
              | _ -> failwith "Invalid result"
          { Wins = w; Draws = d; Losses = l }

      // Same semantics as the standalone opponentsList, driven by the pair index:
      // opponents in first-appearance order, per-opponent results in game order,
      // opponents with no games removed.
      let opponentsListFast p1 =
          orderedPlayers
          |> Seq.filter (fun p2 -> p1 <> p2)
          |> Seq.choose (fun p2 ->
              let matches = gamesBetween p1 p2
              if matches.Count = 0 then None
              else
                  let strings =
                      matches
                      |> Seq.map (fun m ->
                          match m.Result with
                          | "1-0" -> if m.Player1 = p1 then "1" else "0"
                          | "0-1" -> if m.Player1 = p1 then "0" else "1"
                          | "1/2-1/2" -> "1/2"
                          | _ -> failwith "Invalid result")
                      |> Seq.toArray
                  Some (p2, strings))
          |> Seq.toArray

      let sorted = players |> List.sortByDescending (fun p -> if challengerList |> List.contains p then (10000. + snd (scoreOfPlayer p)) else snd (scoreOfPlayer p)) |> List.toArray

      sorted
      |> Array.mapi (fun idx p1 ->
          let statsList = sorted
                          |> Array.filter (fun p2 -> p1 <> p2) // Exclude self
                          |> Array.map (fun p2 -> (p2, statsBetween p1 p2))
          let score, eff = scoreOfPlayer p1
          { Player = p1; Alias = p1; Challenger = challengerList |> List.contains p1 ; Rank = idx + 1; ResultsAgainst = opponentsListFast p1; StatsAgainst = statsList; TotalScore = score; Eff = eff }
      )

  let generateSmallStatCrossTable (results:Result seq) (challengerList: string seq) players =
    let challengers = challengerList |> List.ofSeq
    let players = players |> List.ofSeq
    let crossTable = createStatsCrossTableSummary (results |> Seq.rev |> Seq.toList) challengers players
    ResizeArray(crossTable)

  let generateBigStatCrossTable (results:Result seq) (challengerList: string list) players =
    let crossTable = createStatsCrossTableSummary (results |> Seq.rev |> Seq.toList) challengerList players
    crossTable
    |> Array.filter (fun e -> e.Challenger) // if e.Rank = 2 || e.Rank = 3 then true else false )//e.Challenger)
    |> Array.sortByDescending(fun e -> e.Eff)
    |> Array.mapi(fun idx e -> {e with Rank = idx + 1})
    |> ResizeArray

  let generateCrosstableEntries (results: Result seq) =
      let players =
        [ for r in results do
            yield r.Player1
            yield r.Player2
        ] |> Seq.distinct |> List.ofSeq

      generateSmallStatCrossTable results [] players

  let idealizedEloPrint (cross: CrossTableEntry seq) =
    let sb = new StringBuilder()
    sb.AppendLine "\n```\n" |> ignore
    sb.AppendLine "Idealized UHO ELO calculation (glbchess)\n" |> ignore
    // Empty when the crosstable was skipped (player count over the limit)
    let width = if Seq.isEmpty cross then 8 else cross |> Seq.map(fun e -> e.Player.Length) |> Seq.max
    let header = sprintf "%-*s : %7s %6s %7s %10s" width "# PLAYER" "ELO" "ERROR" "POINTS" "PairsWDL"
    sb.AppendLine header |> ignore

    let uho = StandingsTable.getIdealized_UHO_Elo cross
    for (player, score, elo, error, wins, draws, losses) in uho do
      let pairs = sprintf "%d-%d-%d" wins draws losses
      let line = sprintf "%-*s : %7.0f %6.0f %7.1f %10s" width player elo error score pairs
      sb.AppendLine line |> ignore
    sb.AppendLine "\n```\n" |> ignore
    let idealized = sb.ToString()
    printfn "\n%s\n" idealized


  /// The anchor of a two-player table: the player rated 0 with no error (the first listed when
  /// both are, as after nothing but draws); None at the pool average or with more players.
  let anchorOf (players: PlayerResult seq) =
    match Seq.toList players with
    | [ _; _ ] as ps -> ps |> List.tryFind (fun p -> p.Elo = 0.0 && p.Error = 0.0)
    | _ -> None

  /// Two players with an anchor: the other one's error is the pentanomial one, as the match's Elo
  /// line gives it. Nothing at the pool average or with more players.
  let populatePentanomialError (players: PlayerResult seq) (pgnGames: PGNTypes.PgnGame seq) =
    let players = Seq.toList players
    match anchorOf players with
    | Some anchor ->
      let matchups = Pentanomial.calculateAllMatchups pgnGames
      match matchups with
      | [ (_, counts) ] ->
          let _, error, _ = Pentanomial.pentanomialEloErrorAndLos counts
          // no finished pair yet (one game): the fit's own error says more than an infinite one
          if Double.IsFinite error then
            for p in players do
              if p.Player <> anchor.Player then p.Error <- error
      | _ -> ()
    | _ -> ()

  let populateSpeedMetrics (players: PlayerResult seq) (pgnGames: PGNTypes.PgnGame seq) =
    let avgSpeed =
      PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile(pgnGames, 0)
      |> Array.filter _.Median
    // Index by name instead of a linear tryFind per player: both collections grow with
    // the number of distinct names, so the scan was quadratic on large archive PGNs.
    let byName = Dictionary<string, _>()
    for e in avgSpeed do byName[e.Player] <- e
    for player in players do
      match byName.TryGetValue player.Player with
      | true, speed ->
          player.MedSpeed <- speed.AvgNPS
          player.AvgNPM <- speed.AvgNodes
          if speed.EPS > 0.0 then
            player.EPS <- speed.EPS
      | _ -> ()

  /// Hard cap for crosstable computation: entries carry per-opponent stats for every
  /// player pair (quadratic in players). Tournaments are far below this; archive PGNs
  /// (a TCEC "everything" file has 1384 engine names) skip the crosstable entirely —
  /// standings and pentanomial stay available, only pair columns come up empty.
  let maxCrosstablePlayers = 30

  /// Hard cap for the console standings table. Stats are computed for everyone (that is
  /// linear now), but printing tens of thousands of rows is unreadable — beyond this the
  /// console output is trimmed to the busiest players. Programmatic callers (WebGUI) get
  /// the full list regardless.
  let maxConsoleStandingsPlayers = 500

  let getEngineDataResults results =
    let allResults = getResultsFromPGNGames results
    let distinctPlayerCount =
      let seen = HashSet<string>()
      for r in allResults do
        seen.Add r.Player1 |> ignore
        seen.Add r.Player2 |> ignore
      seen.Count
    let cross =
      if distinctPlayerCount <= maxCrosstablePlayers then
        generateCrosstableEntries allResults
      else
        printfn "Crosstable skipped: %d players exceeds the limit of %d" distinctPlayerCount maxCrosstablePlayers
        ResizeArray()
    let playerRes, fit = getFullStatAndFit allResults
    populateSpeedMetrics playerRes results
    populatePentanomialError playerRes results
    StandingsTable.populatePairData playerRes cross
    let consoleContent =
      let forConsole =
        if playerRes.Length <= maxConsoleStandingsPlayers then playerRes
        else
          printfn "Standings trimmed to the %d most active of %d players"
            maxConsoleStandingsPlayers playerRes.Length
          playerRes |> List.sortByDescending (fun p -> p.Played) |> List.truncate maxConsoleStandingsPlayers
      standingsText forConsole cross fit results

    consoleContent, playerRes, cross, allResults

  let calculateStatistics (engines:string list) results =
      engines
      |> List.map (fun player -> calculateStatisticsForPlayer player results)
      |> withRatingFit (ratingFit engines results)

  let processStat (challengers: string list) (playerResults : _ list) =
    let challengerSet = Set(challengers)
    let notInChallengerSet (p:PlayerResult) = not (challengerSet.Contains p.Player)
    let sortByPointsPlayed =
      List.sortByDescending (fun e ->
        let eff = if e.Played > 0 then e.Points / float e.Played else -1.0
        (eff, e.Points, e.Played))
    let normalizeElo (elo: float) firstPlayerElo = elo - firstPlayerElo

    match challengerSet with
    |set when set.IsEmpty ->
      let sorted = playerResults |> sortByPointsPlayed
      let firstPlayerElo =
        match sorted |> List.tryHead with
        | Some p -> p.Elo
        | None -> 0.0

      //for p in sorted do
      //  p.Elo <- normalizeElo p.Elo firstPlayerElo
      sorted |> ResizeArray<_>
    |_ ->
      let rest = playerResults |> List.filter notInChallengerSet
      let allChallengers =
          playerResults
          |> List.filter (fun res -> challengerSet.Contains res.Player)

      for p in allChallengers do
        p.Challenger <- true

      let sorted = (allChallengers |> sortByPointsPlayed) @ (rest |> sortByPointsPlayed)

      //normalize rating based on the first player's elo performance
      let firstPlayerElo =
          match sorted |> List.tryHead with
          | Some p -> p.Elo
          | None -> 0.0

      //for p in sorted do
      //  p.Elo <- normalizeElo p.Elo firstPlayerElo
      sorted |> ResizeArray<_>

  let getFullStat isGauntlet (challengers:string list) (players:string list) tournamentResults =
      match challengers with
      |h::t ->
        if isGauntlet then
          calculateStatistics players tournamentResults |> processStat challengers
        else
          calculateStatistics players tournamentResults |> processStat []
      |[] ->
        calculateStatistics players tournamentResults |> processStat []


module MoveHistoryDeviation =

  let private isResultToken (token: string) =
    match token with
    | "1-0" | "0-1" | "1/2-1/2" | "*" -> true
    | _ -> false

  let private isMoveNumberToken (token: string) =
    if String.IsNullOrWhiteSpace token then
      false
    else
      let trimmed = token.Trim()
      if not (trimmed.EndsWith(".", StringComparison.Ordinal)) then
        false
      else
        let digits = trimmed.TrimEnd([| '.' |])
        match Int32.TryParse digits with
        | true, _ -> true
        | _ -> false

  let private extractSansFromMoveHistory (moveHistory: string) =
    if String.IsNullOrWhiteSpace moveHistory then
      Array.empty<string>
    else
      moveHistory.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
      |> Array.filter (fun t -> not (isMoveNumberToken t) && not (isResultToken t))

  let private startsWithSequence (list: string array) (prefix: string array) =
    if prefix.Length = 0 || list.Length < prefix.Length then
      false
    else
      let mutable ok = true
      let mutable i = 0
      while ok && i < prefix.Length do
        if not (String.Equals(list.[i], prefix.[i], StringComparison.Ordinal)) then
          ok <- false
        i <- i + 1
      ok

  // castling one way however it was written: a PGN's O-O (the standard, what EB writes) and the
  // board's 0-0 are one move, or the live view marks a castle as a deviation
  let private castleAsZeros (sans: string array) =
    sans |> Array.map (fun s -> if s.StartsWith("O-O", StringComparison.Ordinal) then s.Replace('O', '0') else s)

  let private drop (n: int) (arr: string array) =
    if n <= 0 then
      arr
    elif n >= arr.Length then
      Array.empty<string>
    else
      arr.[n..]

  /// Returns the 0-based SAN index within the provided `moveHistory` string (ignores move numbers and results).
  /// If `openingSans` is provided and the sequences start with it, it is ignored for comparison.
  /// The returned index points into the original SAN sequence of `moveHistory` (i.e., includes any opening SANs
  /// if they are present in `moveHistory`).
  [<CompiledName("FirstDeviationIndex")>]
  let firstDeviationIndex (moveHistory: string) (openingSans: string array) (referenceSans: string array) : Nullable<int> =
    if isNull moveHistory then
      Nullable()
    elif isNull referenceSans || referenceSans.Length = 0 then
      Nullable()
    else
      let currentSans = extractSansFromMoveHistory moveHistory |> castleAsZeros
      let openingSans = if isNull openingSans then Array.empty<string> else castleAsZeros openingSans
      let referenceSans = castleAsZeros referenceSans

      let currentHasOpening = openingSans.Length > 0 && startsWithSequence currentSans openingSans
      let referenceHasOpening = openingSans.Length > 0 && startsWithSequence referenceSans openingSans

      let currentOffset = if currentHasOpening then openingSans.Length else 0
      let currentSansTrimmed = if currentHasOpening then drop openingSans.Length currentSans else currentSans
      let referenceSansTrimmed = if referenceHasOpening then drop openingSans.Length referenceSans else referenceSans

      let max = min currentSansTrimmed.Length referenceSansTrimmed.Length
      let mutable i = 0
      let mutable found: int option = None

      while found.IsNone && i < max do
        if not (String.Equals(currentSansTrimmed.[i], referenceSansTrimmed.[i], StringComparison.Ordinal)) then
          found <- Some(i + currentOffset)
        i <- i + 1

      match found with
      | Some idx -> Nullable(idx)
      | None ->
        if currentSansTrimmed.Length > referenceSansTrimmed.Length then
          Nullable(referenceSansTrimmed.Length + currentOffset)
        else
          Nullable()

module Time =
  let prettyPrintTimeSpan (timeSpan: TimeSpan) =
    let hours = timeSpan.Hours
    let minutes = timeSpan.Minutes
    let seconds = timeSpan.Seconds
    sprintf "%02dh %02dm %02ds" hours minutes seconds

module EvalLogistic =

  /// Clamp a value between a minimum and maximum.
  let bounded value minValue maxValue =
    if value < minValue then minValue
    elif value > maxValue then maxValue
    else value

  let CENTIPAWN_MULT = 90.0
  let CENTIPAWN_TAN_MULT = 1.5637541897
  let MAX_CENTIPAWN = 9999.0

  /// Converts a logistic value in [-1, 1] to a centipawn value, clamped to [-9999, 9999].
  let logisticToCentipawn (logistic: float) : float =
    let boundedLogistic = bounded logistic -1.0 1.0
    let centipawn = Math.Round(CENTIPAWN_MULT * Math.Tan(CENTIPAWN_TAN_MULT * boundedLogistic), 2)
    bounded centipawn -MAX_CENTIPAWN MAX_CENTIPAWN
