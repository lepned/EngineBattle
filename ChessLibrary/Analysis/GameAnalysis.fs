module ChessLibrary.GameAnalysis

open System
open System.IO
open System.Text
open CliWrap
open CliWrap.Buffered

open TypesDef.CoreTypes
open Statistics
open RuntimeUtilities

module Formatting =

  /// Formats a metric value with SI prefixes (G, M, K)
  let inline private formatMetric (unit: string) (value: double) =
    if value > 1_000_000_000.0 then sprintf "%.1fG%s" (value / 1_000_000_000.0) unit
    elif value > 1_000_000.0 then sprintf "%.1fM%s" (value / 1_000_000.0) unit
    elif value >= 1_000.0 then sprintf "%.1fK%s" (value / 1_000.0) unit
    else sprintf "%.1f %s" value unit

  let inline formatNPS (nps: double) = formatMetric "nps" nps

  let inline formatEPS (eps: double) =
    if eps = 0.0 then "NA" else formatMetric "eps" eps

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

  let getRoundFormatted (input: string) (divider: int) =
    let parts = input.Split('.')
    if parts.Length = 2 then
        let major = int parts.[0]
        let minor = int parts.[1]

        // Calculate the new major and minor numbers
        let newMajor = (major - 1) / divider + 1
        let newMinor = ((major - 1) % divider) + 1

        sprintf "%d.%d" newMajor newMinor
    else
        "Invalid input"

module OrdoHelper =

  // Configuration (consider moving to a config file)
  let defaultZValue = "200.24"
  let defaultThreads = "4"
  let defaultSkillLevel = "1000"
  let defaultUserList = "0,1,2,3,4,5,6,7,8,9,10"
  let defaultStatsFile = "stats.txt"

  //glbchess uses this ordo commmand:
  //ordo-win64.exe -Q -a 0 -A "Stockfish 17" -D -U "0,1,2,3,4,5,6,7,8,9,10" -s 2000 -n 6  -C ordo_CFS_matrix.csv -o ordo_rating.txt -c ordo_rating.csv -- Ceres.pgn

  // Function to create the command with specified executable path, file, and engine name
  let createOrdoCommand (executablePath: string) (fileName: string) (engineName: string) =
    // Test if the executable exists
    if not (File.Exists(executablePath)) then
        ConsoleUtils.redConsole "Ordo executable not found"

    let baseArgsBeforeEngine = ["-Q"; "-a"; "0"; ]
    let engineArgs =
        match String.IsNullOrEmpty engineName with
        | false -> ["-A"; engineName.Trim()] // Engine name as an argument
        | true -> [] // Do not include the -A argument if engineName is not provided
    let baseArgsAfterEngine = [
        "-N"; "0";
        "-D";
        "-z"; defaultZValue;
        "-n"; defaultThreads;
        "-s"; defaultSkillLevel;
        "-U"; defaultUserList;
        "-j"; defaultStatsFile;
        "--"; fileName // File name as an argument
    ]
    Cli.Wrap(executablePath)
        .WithWorkingDirectory(Path.GetDirectoryName(executablePath))
        .WithArguments(baseArgsBeforeEngine @ engineArgs @ baseArgsAfterEngine)

  let calcPadding (minPadding: int) (delta: int) (selector: 'a -> string) (data: seq<'a>) =
    let lengths = data |> Seq.map (selector >> String.length)
    if Seq.isEmpty lengths then minPadding
    else max ((Seq.max lengths) + delta) minPadding

  let addDataFromEBToOrdo (output: string) (engineData : EngineLineData seq) =
    let lines = output.Split('\n')[3..] // Split the string into lines and remove the first three lines
    let sb = new StringBuilder()
    let minPadding = 8
    let delta = 2 //extra padding for columns with values
    let wScorePadding = calcPadding minPadding delta (fun e -> e.WhiteScore.ToString()) engineData
    let bScorePadding = calcPadding minPadding delta (fun e -> e.BlackScore.ToString()) engineData
    let pairsPadding  = calcPadding minPadding delta (fun e -> e.Pairs) engineData
    let formatSpeed (e: EngineLineData) =
      let nps = Formatting.formatNPS e.Speed
      if e.EPS > 0.0 then $"{nps} ({Formatting.formatEPS e.EPS})" else nps
    let speedPadding  = calcPadding minPadding delta formatSpeed engineData
    let speedHeader =
      let label = "Speed"
      let diff = speedPadding - label.Length
      if diff <= 0 then label
      else
        let left = diff / 2
        let right = diff - left
        (String.replicate left " ") + label + (String.replicate right " ")

    let converted =
      for line in lines do
        match engineData |> Seq.tryFind (fun e -> line.Contains e.Player) with
        | Some e ->
            sb.Append (line.TrimEnd('\r')) |> ignore
            sb.Append (e.WhiteScore.ToString().PadLeft(wScorePadding)) |> ignore
            sb.Append (e.BlackScore.ToString().PadLeft(bScorePadding)) |> ignore
            sb.Append (e.Pairs.PadLeft(pairsPadding)) |> ignore
            sb.Append "   " |> ignore
            let speed = formatSpeed e
            sb.Append (speed.PadRight(speedPadding) + "\n") |> ignore
        | None ->
            match engineData |> Seq.tryFind (fun _ -> line.Contains "RATING") with
            | Some _ ->
                sb.Append (line.TrimEnd('\r')) |> ignore
                sb.Append ("Wscore".PadLeft(wScorePadding)) |> ignore
                sb.Append ("Bscore".PadLeft(bScorePadding)) |> ignore
                sb.Append ("Pairs".PadLeft(pairsPadding)) |> ignore
                sb.Append "   " |> ignore
                sb.Append (speedHeader.PadLeft(speedPadding) + "\n") |> ignore
            | _ -> sb.AppendLine line |> ignore
      sb.ToString()
    converted


    // Execute the command asynchronously and capture the output
  let runCommandAsync (cmd:Command) (engineData : EngineLineData seq) =
      task {
          try
              let cts = new System.Threading.CancellationTokenSource(TimeSpan.FromSeconds(10.0))
              let sb = new StringBuilder()
              let! result = cmd.ExecuteBufferedAsync(cts.Token)
              //let msg = removeTopThreeLines (result.StandardOutput.Trim())
              sb.Append "\n```" |> ignore
              let msg = addDataFromEBToOrdo result.StandardOutput engineData
              sb.Append msg |> ignore
              sb.Append "```\n" |> ignore
              if result.StandardError.Length > 0 then
                //printfn "Standard Error: %s" result.StandardError
                sb.AppendLine (sprintf "Standard Error: %s" result.StandardError) |> ignore
              return sb.ToString()
          with
          | :? OperationCanceledException ->
              return "Ordo command execution timed out after 10 second"
          | ex -> return sprintf "An error occurred: %s" (ex.Message)
      }


  let lossCombinations = [ "00"; "01/2"; "1/20" ]
  let winCombinations = [ "11"; "11/2"; "1/21" ]

  let getEngineLineDataForPlayer (p : PlayerResult) pairWins pairLosses : EngineLineData =
    let bw,bd,_ = p.BlackWDL
    let ww,wd,_ = p.WhiteWDL
    let bscore = float bw + float bd * 0.5
    let wscore = float ww + float wd * 0.5
    let pairs = sprintf "%2d-%d" pairWins pairLosses
    {
        Player = p.Player
        Elo = p.Elo // if p.Challenger then 0.0 else p.Elo
        Error = p.Error //if p.Challenger then 0.0 else p.Error
        Points = p.Points
        Played = p.Played
        Percent = p.Percent
        CFS = p.CFS
        EPS = 0.0
        Speed = p.MedSpeed
        Win = p.Win
        Draw = p.Draw
        Loss = p.Loss
        D = p.D
        WhiteScore = wscore
        BlackScore = bscore
        Pairs = pairs
    }

  let calculatePairs (results: string[]) =
    let welcome = 1
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

  let getEngineLineData (engines: PlayerResult seq) (table:CrossTableEntry seq) =
    if table |> Seq.length = 2 then
      (table |> Seq.head).Challenger <- true
    seq {
      for p in engines do
        let (wins, losses) =
          match table |> Seq.tryFind (fun e -> e.Player = p.Player) with
          |Some t ->
            let pairs = getAllPairs t
            p.Challenger <- t.Challenger
            let (w,_,l) = pairs
            w,l
          |_ -> (0,0)
        getEngineLineDataForPlayer p wins losses
    }

  let writeResultHeader (n:int) : string =
    sprintf "%-*s : %7s %6s %7s %7s %4s %10s %5s %5s %5s %5s %7s %7s %8s" n "# PLAYER" "ELO" "ERROR" "POINTS" "PLAYED" "(%)" "Speed" "W" "D" "L" "D(%)" "WScore" "BScore" "Pairs"

  let writeResultHeaderWithSpeed (n:int) (speedPadding:int) : string =
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

  let private formatElo (elo: float) (isChallenger: bool) =
    if isChallenger then "0.0"
    elif Double.IsNaN(elo) then "---"
    elif elo = System.Double.PositiveInfinity then ConsoleUtils.positiveInfinitySymbol
    elif elo = System.Double.NegativeInfinity then ConsoleUtils.negativeInfinitySymbol
    else sprintf "%7.1f" elo

  let private formatError (error: float) (isChallenger: bool) =
      if isChallenger then "----"
      elif Double.IsNaN(error) then "---"
      elif error = System.Double.PositiveInfinity then ConsoleUtils.positiveInfinitySymbol
      elif error = System.Double.NegativeInfinity then ConsoleUtils.negativeInfinitySymbol
      else error.ToString("F1")

  let writeEngineLineForPlayer (p: PlayerResult) pairWins pairLosses n =
      let bw, bd, _ = p.BlackWDL
      let ww, wd, _ = p.WhiteWDL
      let bscore = float bw + float bd * 0.5
      let wscore = float ww + float wd * 0.5
      let pairs = sprintf "%2d-%d" pairWins pairLosses
      let elo = formatElo p.Elo p.Challenger
      let error = formatError p.Error p.Challenger
      let speed = Formatting.formatNPS p.MedSpeed
      sprintf "%-*s : %7s %6s %7.1f %7d %4d %10s %5d %5d %5d %5d %7.1f %7.1f %8s"
              n p.Player elo error p.Points p.Played p.Percent speed p.Win p.Draw p.Loss p.D wscore bscore pairs

  let writeEngineLineForPlayerWithSpeed (p: PlayerResult) pairWins pairLosses n (speed: string) (speedPadding:int) =
      let bw, bd, _ = p.BlackWDL
      let ww, wd, _ = p.WhiteWDL
      let bscore = float bw + float bd * 0.5
      let wscore = float ww + float wd * 0.5
      let pairs = sprintf "%2d-%d" pairWins pairLosses
      let elo = formatElo p.Elo p.Challenger
      let error = formatError p.Error p.Challenger
      sprintf "%-*s : %7s %6s %7.1f %7d %4d %6d   %-*s %5d %5d %5d %5d %7.1f %7.1f %8s"
              n p.Player elo error p.Points p.Played p.Percent p.CFS speedPadding speed p.Win p.Draw p.Loss p.D wscore bscore pairs

  let printStatsMatrix (table: CrossTableEntry seq) =
    let endOfLine = "\n```\n"
    let players = table |> Seq.map (fun t -> t.Player) |> Seq.toArray
    let longest = (players |> Seq.map String.length |> Seq.max) + 2
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

  let printHeadToHeadStatsToConsole (table: CrossTableEntry seq) =
    printfn "\nGame summary:\n"
    for t in table do
        printfn "Player: %s" t.Player
        if Seq.isEmpty t.StatsAgainst then
            printfn "\tNo games played against any opponent."
        else
            t.StatsAgainst
            |> Seq.sortBy fst
            |> Seq.iter (fun (opponent, stats) ->
                let points = float stats.Wins + float stats.Draws * 0.5
                let gamesPlayed = float (stats.Wins + stats.Draws + stats.Losses)
                let wdl = sprintf "%d-%d-%d" stats.Wins stats.Draws stats.Losses
                let result = sprintf "%.1f/%.0f" points gamesPlayed
                printfn "\tAgainst: %-15s Wins = %2d, Draws = %2d, Losses = %2d (%s) Score = %s"
                    opponent stats.Wins stats.Draws stats.Losses wdl result
            )
        printfn ""

  let getResultsAndPairsInConsoleFormat (engines: PlayerResult seq) (table: CrossTableEntry seq) =
    let sb = new StringBuilder()
    let appendLine (txt: string) = sb.AppendLine txt |> ignore

    appendLine "\n```\n"

    // Find longest player name for formatting
    let longest = engines |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> e.Player.Length + 2
    writeResultHeader longest |> appendLine

    // In a two player table, the first player is always the challenger
    let players =
        engines
        |> Seq.mapi (fun idx p ->
            let isChallenger =
                table |> Seq.length = 2 && idx = 0
            { p with Challenger = isChallenger }
        )
        |> Seq.toList

    // Sort: challengers first, then by Elo descending
    let sortedPlayers =
        let (challengers, rest) = players |> List.partition (fun p -> p.Challenger)
        let restSorted = rest |> List.sortByDescending (fun e -> e.Elo)
        challengers @ restSorted

    //normalize rating based on the first player's elo performance
    let firstPlayerElo =
        match sortedPlayers |> List.tryHead with
        | Some p -> p.Elo
        | None -> 0.0

    let normalizeElo (elo: float) = elo - firstPlayerElo

    // Write each player's line
    for p in sortedPlayers do
        //p.Elo <- normalizeElo p.Elo
        let (wins, losses) =
            match table |> Seq.tryFind (fun e -> e.Player = p.Player) with
            | Some t ->
                let w, _, l = getAllPairs t
                w, l
            | None -> 0, 0
        writeEngineLineForPlayer p wins losses longest |> appendLine

    appendLine "\n```"
    sb.ToString()

  let getResultsAndPairsInConsoleFormatWithEps
    (engines: PlayerResult seq)
    (table: CrossTableEntry seq)
    (epsByPlayer: Map<string, float>) =
      let sb = new StringBuilder()
      let appendLine (txt: string) = sb.AppendLine txt |> ignore

      appendLine "\n```\n"

      let longest = engines |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> e.Player.Length + 2
      let players =
          engines
          |> Seq.mapi (fun idx p ->
              let isChallenger =
                  table |> Seq.length = 2 && idx = 0
              { p with Challenger = isChallenger }
          )
          |> Seq.toList

      let sortedPlayers =
          let (challengers, rest) = players |> List.partition (fun p -> p.Challenger)
          let restSorted = rest |> List.sortByDescending (fun e -> e.Elo)
          challengers @ restSorted

      let speedStrings =
          sortedPlayers
          |> List.map (fun p ->
              let baseSpeed = Formatting.formatNPS p.MedSpeed
              match epsByPlayer |> Map.tryFind p.Player with
              | Some eps when eps > 0.0 -> $"{baseSpeed} ({Formatting.formatEPS eps})"
              | _ -> baseSpeed)

      let speedPadding = calcPadding 8 0 id speedStrings
      writeResultHeaderWithSpeed longest speedPadding |> appendLine

      for p in sortedPlayers do
          let (wins, losses) =
              match table |> Seq.tryFind (fun e -> e.Player = p.Player) with
              | Some t ->
                  let w, _, l = getAllPairs t
                  w, l
              | None -> 0, 0
          let bw, bd, _ = p.BlackWDL
          let ww, wd, _ = p.WhiteWDL
          let bscore = float bw + float bd * 0.5
          let wscore = float ww + float wd * 0.5
          let pairs = sprintf "%2d-%d" wins losses
          let baseSpeed = Formatting.formatNPS p.MedSpeed
          let speed =
              match epsByPlayer |> Map.tryFind p.Player with
              | Some eps when eps > 0.0 ->
                  $"{baseSpeed} ({Formatting.formatEPS eps})"
              | _ -> baseSpeed
          writeEngineLineForPlayerWithSpeed p wins losses longest speed speedPadding |> appendLine

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
  let getAllPlayers results =
      results
      |> List.collect (fun r -> [ r.Player1; r.Player2 ])
      |> List.distinct

  // Define a function to get a list of all pairs of players from a list of results
  let getAllPlayerPairs results =
      getAllPlayers results
      |> List.collect (fun player1 ->
          getAllPlayers results
          |> List.filter (fun player2 -> player1 <> player2)
          |> List.map (fun player2 -> (player1, player2)))

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
        let cfs = (EloCalculator.calculateLikelihoodOfSuperiority winCount drawCount lossCount) * 100. |> int32 |> max 0  //remove negative numbers when score is perfect
        let percent = (points / float maxScore) * 100.0 |> int32
        let played = maxScore
        let dPercent = (float drawCount / float maxScore) * 100.0 |> int32
        //todo
        let pairWins = 0
        let pairLosses = 0
        createPlayerResult player (Math.Round(points,1)) (Math.Round(eloEstimate,0)) error played percent cfs
          winCount drawCount lossCount dPercent (whiteWin, whiteDraw, whiteLoss) (blackWin, blackDraw, blackLoss) pairWins pairLosses

      else
        createPlayerResult player 0.0 0.0 0.0 0 0 0 0 0 0 0 (0,0,0) (0,0,0) 0 0

  let getResultsFromPGNPath (filePath: string) =
    let games =
      FullPGNParser.parsePgnFile filePath
      |> Seq.map PGNWriter.getResultsFromPGNGame
      |> Seq.toArray
      |> Array.rev
    games

  let getResultsFromPGNGames (pgns: PGNTypes.PgnGame seq) =
      pgns
      |> Seq.map PGNWriter.getResultsFromPGNGame
      |> Seq.toArray

  let getFullStatFromResults results =
    let players =
      [ for r in results do
          yield r.Player1
          yield r.Player2
      ] |> Seq.distinct |> List.ofSeq
    seq {
          for player in players do
            let res = calculateStatisticsForPlayer player results
            yield res }

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

      let scoreOfPlayer player =
          let games =
            results
            |> List.filter (fun r -> r.Player1 = player || r.Player2 = player)
          games
          |> List.sumBy (fun r ->
              match r.Result with
              | "1-0" -> if r.Player1 = player then 1.0 else 0.0
              | "0-1" -> if r.Player1 = player then 0.0 else 1.0
              | "1/2-1/2" -> 0.5
              | _ -> failwith "Invalid result"
          )
          |> fun score -> score, score / float games.Length

      let statsBetween p1 p2 =
          let matches = results
                        |> List.filter (fun r -> (r.Player1 = p1 && r.Player2 = p2) || (r.Player1 = p2 && r.Player2 = p1))
          let wins, draws, losses =
              matches
              |> List.fold (fun (w, d, l) m ->
                  match m.Result with
                  | "1-0" -> if m.Player1 = p1 then (w+1, d, l) else (w, d, l+1)
                  | "0-1" -> if m.Player1 = p1 then (w, d, l+1) else (w+1, d, l)
                  | "1/2-1/2" -> (w, d+1, l)
                  | _ -> failwith "Invalid result"
              ) (0, 0, 0)
          { Wins = wins; Draws = draws; Losses = losses }

      let sorted = players |> List.sortByDescending (fun p -> if challengerList |> List.contains p then (10000. + snd (scoreOfPlayer p)) else snd (scoreOfPlayer p)) |> List.toArray

      sorted
      |> Array.mapi (fun idx p1 ->
          let statsList = sorted
                          |> Array.filter (fun p2 -> p1 <> p2) // Exclude self
                          |> Array.map (fun p2 -> (p2, statsBetween p1 p2))
          let score, eff = scoreOfPlayer p1
          { Player = p1; Alias = p1; Challenger = challengerList |> List.contains p1 ; Rank = idx + 1; ResultsAgainst = opponentsList results p1; StatsAgainst = statsList; TotalScore = score; Eff = eff }
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
    let width = cross |> Seq.map(fun e -> e.Player.Length) |> Seq.max
    let header = sprintf "%-*s : %7s %6s %7s %10s" width "# PLAYER" "ELO" "ERROR" "POINTS" "PairsWDL"
    sb.AppendLine header |> ignore

    let uho = OrdoHelper.getIdealized_UHO_Elo cross
    for (player, score, elo, error, wins, draws, losses) in uho do
      let pairs = sprintf "%d-%d-%d" wins draws losses
      let line = sprintf "%-*s : %7.0f %6.0f %7.1f %10s" width player elo error score pairs
      sb.AppendLine line |> ignore
    sb.AppendLine "\n```\n" |> ignore
    let idealized = sb.ToString()
    printfn "\n%s\n" idealized


  let getEngineDataResults results =
    let allResults = getResultsFromPGNGames results
    let cross = generateCrosstableEntries allResults
    let playerRes = getFullStatFromResults allResults |> Seq.toList
    let avgSpeed =
      PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile(results,0)
      |> Array.filter _.Median
      |> Array.sortByDescending _.AvgNPS

    for player in playerRes do
      let speed = avgSpeed |> Seq.tryFind (fun e -> e.Player = player.Player)
      if speed.IsSome then
          player.MedSpeed <- speed.Value.AvgNPS
          player.AvgNPM <- speed.Value.AvgNodes

    let epsByPlayer =
      avgSpeed
      |> Seq.filter (fun e -> e.EPS > 0.0)
      |> Seq.map (fun e -> e.Player, e.EPS)
      |> Map.ofSeq
    let consoleContent =
      OrdoHelper.getResultsAndPairsInConsoleFormatWithEps playerRes cross epsByPlayer
    let engineStats =
      OrdoHelper.getEngineLineData playerRes cross
      |> Seq.map (fun e ->
          match epsByPlayer |> Map.tryFind e.Player with
          | Some eps when eps > 0.0 ->
              e.EPS <- eps
              e
          | _ -> e)
      |> Seq.toList

    consoleContent, engineStats, cross, allResults

  let calculateStatistics (engines:string list) results =
      engines |> List.map (fun player -> calculateStatisticsForPlayer player results)

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
      let currentSans = extractSansFromMoveHistory moveHistory
      let openingSans = if isNull openingSans then Array.empty<string> else openingSans

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
