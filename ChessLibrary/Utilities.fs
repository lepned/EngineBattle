module ChessLibrary.Utilities
open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics
open Microsoft.Extensions.Logging
open System.Xml.Linq
open Microsoft.FSharp.Core.Operators.Unchecked

open TypesDef
open TypesDef.PGNTypes
open TypesDef.Position
open TypesDef.CoreTypes
open TypesDef.Tournament
open Parser
open LowLevelUtilities

module UciOptions =

  let uciToCommand (uciParameter: string) (value: string) : string option =
    let mapping = 
        dict [
            "WeightsFile", "--weights"
            "Backend", "--backend"
            "BackendOptions", "--backend-opts"
            "Threads", "--threads"
            "NNCacheSize", "--nncache"
            "MinibatchSize", "--minibatch-size"
            "CPuct", "--cpuct"
            "CPuctExponent", "--cpuct-exponent"
            "CPuctExponentAtRoot", "--cpuct-exponent-at-root"
            "CPuctBase", "--cpuct-base"
            "CPuctFactor", "--cpuct-factor"
            "TwoFoldDraws", "--two-fold-draws"
            "VerboseMoveStats", "--verbose-move-stats"
            "FpuStrategy", "--fpu-strategy"
            "FpuValue", "--fpu-value"
            "CacheHistoryLength", "--cache-history-length"
            "PolicyTemperature", "--policy-softmax-temp"
            "MaxCollisionEvents", "--max-collision-events"
            "MaxCollisionVisits", "--max-collision-visits"
            "MaxCollisionVisitsScalingStart", "--max-collision-visits-scaling-start"
            "MaxCollisionVisitsScalingEnd", "--max-collision-visits-scaling-end"
            "MaxCollisionVisitsScalingPower", "--max-collision-visits-scaling-power"
            "OutOfOrderEval", "--out-of-order-eval"
            "MaxOutOfOrderEvalsFactor", "--max-out-of-order-evals-factor"
            "StickyEndgames", "--sticky-endgames"
            "SyzygyFastPlay", "--syzygy-fast-play"
            "MultiPV", "--multipv"
            "PerPVCounters", "--per-pv-counters"
            "ScoreType", "--score-type"
            "HistoryFill", "--history-fill"
            "MovesLeftMaxEffect", "--moves-left-max-effect"
            "MovesLeftThreshold", "--moves-left-threshold"
            "MovesLeftSlope", "--moves-left-slope"
            "MovesLeftConstantFactor", "--moves-left-constant-factor"
            "MovesLeftScaledFactor", "--moves-left-scaled-factor"
            "MovesLeftQuadraticFactor", "--moves-left-quadratic-factor"
            "MaxConcurrentSearchers", "--max-concurrent-searchers"
            "DrawScore", "--draw-score"
            "ContemptMode", "--contempt-mode"
            "Contempt", "--contempt"
            "WDLCalibrationElo", "--wdl-calibration-elo"
            "WDLEvalObjectivity", "--wdl-eval-objectivity"
            "WDLDrawRateReference", "--wdl-draw-rate-reference"
            "NodesPerSecondLimit", "--nps-limit"
            "TaskWorkers", "--task-workers"
            "MinimumProcessingWork", "--minimum-processing-work"
            "MinimumPickingWork", "--minimum-picking-work"
            "MinimumRemainingPickingWork", "--minimum-remaining-picking-work"
            "MinimumPerTaskProcessing", "--minimum-per-task-processing"
            "IdlingMinimumWork", "--idling-minimum-work"
            "ThreadIdlingThreshold", "--thread-idling-threshold"
            "CpuctUtilityStdevPrior", "--cpuct-utility-stdev-prior"
            "CpuctUtilityStdevScale", "--cpuct-utility-stdev-scale"
            "CpuctUtilityStdevPriorWeight", "--cpuct-utility-stdev-prior-weight"
            "UseVarianceScaling", "--use-variance-scaling"
            "MoveRuleBucketing", "--move-rule-bucketing"
            "ReportedNodes", "--reported-nodes"
            "UncertaintyWeightingCap", "--uncertainty-weighting-cap"
            "UncertaintyWeightingCoefficient", "--uncertainty-weighting-coefficient"
            "UncertaintyWeightingExponent", "--uncertainty-weighting-exponent"
            "UseUncertaintyWeighting", "--use-uncertainty-weighting"
            "EasyEvalWeightDecay", "--easy-eval-weight-decay"
            "CpuctUncertaintyMinFactor", "--cpuct-uncertainty-min-factor"
            "CpuctUncertaintyMaxFactor", "--cpuct-uncertainty-max-factor"
            "CpuctUncertaintyMinUncertainty", "--cpuct-uncertainty-min-uncertainty"
            "CpuctUncertaintyMaxUncertainty", "--cpuct-uncertainty-max-uncertainty"
            "UseJustFpuUncertainty", "--use-just-fpu-uncertainty"
            "UseCpuctUncertainty", "--use-cpuct-uncertainty"
            "DesperationMultiplier", "--desperation-multiplier"
            "DesperationLow", "--desperation-low"
            "DesperationHigh", "--desperation-high"
            "DesperationPriorWeight", "--desperation-prior-weight"
            "UseDesperation", "--use-desperation"
            "TopPolicyBoost", "--top-policy-boost"
            "TopPolicyNumBoost", "--top-policy-num-boost"
            "SearchSpinBackoff", "--search-spin-backoff"
            "ConfigFile", "--config"
            "SyzygyPath", "--syzygy-paths"
            "UCI_Chess960", "--chess960"
            "UCI_ShowWDL", "--show-wdl"
            "UCI_ShowMovesLeft", "--show-movesleft"
            "SmartPruningFactor", "--smart-pruning-factor"
            "SmartPruningMinimumBatches", "--smart-pruning-minimum-batches"
            "RamLimitMb", "--ramlimit-mb"
            "MoveOverheadMs", "--move-overhead"
            "TimeManager", "--time-manager"
            "LogFile", "--logfile"
        ]

    match mapping.TryGetValue uciParameter with
    | true, flag ->
        match Boolean.TryParse value with
        | true, v ->            
              Some(sprintf "%s=%b" flag v)
        | false, _ -> 
            if uciParameter.Contains "BackendOptions" then
              Some(sprintf "%s=\"%s\"" flag value)
            else              
              Some(sprintf "%s=%s" flag value)
    | false, _ -> None


  let createCommandsFromConfig (config: EngineConfig) =
    let sb = StringBuilder()    
    let append (s:string) = sb.Append (s + " ") |> ignore    
    for option in config.Options do
      let mutable value = option.Value.ToString()
      let (ok,v) = Boolean.TryParse value
      if ok then
        value <- sprintf "%b" v       
      match uciToCommand option.Key value with
      | Some k -> append k
      | None -> ()
    sb.ToString()

  
module EloCalculator =

  // Define a custom exception type for errors
  type ErrorFunctionException(message: string) =
      inherit System.Exception(message)
  
  let log10 = log 10.0

  let inline eloDifferenceFromScore score =
    -log (1.0 / (score + 0.000001) - 1.0) * 400.0 / log10

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
        999.0 / numberOfGames
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
    let elo = Math.Round(100.0 * Math.Log10(wins / losses))
    let error = Math.Round(ebFactor * 100.0 / Math.Log(10.0) * Math.Sqrt(1.0 / wins + 1.0 / losses))
    elo, error

module Formatting =
  
  let inline formatNPS (nps:double) =
    if nps > 1_000_000_000.0 then sprintf "%.1fGnps" (nps / 1_000_000_000.0)
    elif nps > 1_000_000.0 then sprintf "%.1fMnps" (nps / 1_000_000.0)
    elif nps >= 1_000.0 then sprintf "%.1fKnps" (nps / 1_000.0)
    else sprintf "%.1f nps" nps

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

  //let camelToKebab (input: string) =
  //  // Use Regex to find places where a lowercase letter is followed by an uppercase letter
  //  if UciOptions.notCommands.ContainsKey input then
  //    String.Empty
  //  elif UciOptions.commands.ContainsKey (input) then
  //    UciOptions.commands.[input]
  //  else
  //    let pattern = @"([a-z])([A-Z])"
  //    let replacement = "$1-$2"
  //    if input.StartsWith "UCI_" then
  //      let withoutPrefix = input.Substring(4)
  //      let result = Regex.Replace(withoutPrefix, pattern, replacement)
  //      result.ToLower()
  //    elif input.StartsWith("WDL") then
  //      // Remove the "WDL" prefix
  //      let withoutPrefix = input.Substring(3)
  //      let result = Regex.Replace(withoutPrefix, pattern, replacement)
  //      "wdl-" + result.ToLower()
  //    else      
  //    // Replace the matched patterns with the lowercase letter followed by a hyphen and the uppercase letter
  //    let result = Regex.Replace(input, pattern, replacement)      
  //    // Convert the entire string to lowercase
  //    result.ToLower()
  

module OrdoHelper =
  
  open CliWrap
  open CliWrap.Buffered

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
        .WithArguments(baseArgsBeforeEngine @ engineArgs @ baseArgsAfterEngine) // Ensuring -A comes right after -a

  //let executablePath = @"C:/Dev/Chess/Ordo/ordo-win64.exe" // Or the appropriate path on macOS/Linux
  //let cmd = createOrdoCommand executablePath "boosting.pgn" "Stockfish 240203"
  
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
    let speedPadding  = calcPadding minPadding delta (fun e -> Formatting.formatNPS e.Speed) engineData   
    
    let converted =      
      for line in lines do          
        match engineData |> Seq.tryFind (fun e -> line.Contains e.Player) with
        | Some e -> 
            sb.Append (line.TrimEnd('\r')) |> ignore
            sb.Append (e.WhiteScore.ToString().PadLeft(wScorePadding)) |> ignore
            sb.Append (e.BlackScore.ToString().PadLeft(bScorePadding)) |> ignore
            sb.Append (e.Pairs.PadLeft(pairsPadding)) |> ignore
            let nps = Formatting.formatNPS e.Speed
            sb.Append (nps.PadLeft(speedPadding) + "\n") |> ignore
        | None -> 
            match engineData |> Seq.tryFind (fun _ -> line.Contains "RATING") with
            | Some _ -> 
                sb.Append (line.TrimEnd('\r')) |> ignore
                sb.Append ("Wscore".PadLeft(wScorePadding)) |> ignore
                sb.Append ("Bscore".PadLeft(bScorePadding)) |> ignore
                sb.Append ("Pairs".PadLeft(pairsPadding)) |> ignore
                sb.Append ("Speed".PadLeft(speedPadding) + "\n") |> ignore
            | _ -> sb.AppendLine line |> ignore
      sb.ToString()
    converted  
    

  // Execute the command asynchronously and capture the output
  let runCommandAsync (cmd:Command) (engineData : EngineLineData seq) =
      task {
          try
              let sb = new StringBuilder()
              let! result = cmd.ExecuteBufferedAsync()
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
    let sb = System.Text.StringBuilder()
    let appendLine (txt: string) = sb.AppendLine txt |> ignore

    appendLine "\n```\n"

    // Find longest player name for formatting
    let longest = engines |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> e.Player.Length + 2
    writeResultHeader longest |> appendLine

    // Prepare a lookup for challenger status
    let challengerSet =
        table
        |> Seq.filter (fun t -> t.Challenger)
        |> Seq.map (fun t -> t.Player)
        |> Set.ofSeq
    
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

  let getIdealized_UHO_Elo (table:CrossTableEntry seq) =
    let eb = 2.0
    [
      for t in table do 
        let (wins,draws, losses) = getAllPairs t
        let (elo, error) = EloCalculator.calculateIdealized_UHO_EloAndError wins losses eb
        t.Player, t.TotalScore, elo, error, wins, draws, losses    
    ]

module ConsoleHelper =  
  
  let displayTournament (tournament: Tournament) =
    let timeFormat (time:TimeOnly) = time.ToString("HH:mm:ss.fff")
    printfn "Name: %s" tournament.Name
    printfn "Description: %s" tournament.Description
    printfn "OS: %s" tournament.OS
    printfn "CPU: %s" tournament.CPU
    printfn "RAM: %s" tournament.RAM
    printfn "GPU: %s" tournament.GPU
    printfn "Gauntlet: %b" tournament.Gauntlet
    printfn "DoNotDeviate: %b" tournament.PreventMoveDeviation
    printfn "Challengers: %d" tournament.Challengers
    printfn "Rounds: %d" tournament.Rounds
    printfn $"Delay between games: {tournament.DelayBetweenGames}"
    printfn $"Move overhead: {timeFormat tournament.MoveOverhead}"
    printfn "Time control: %A" tournament.TimeControl
    printfn "PgnOut Path: %s" tournament.PgnOutPath
    printfn "Reference PGN path: %s" tournament.ReferencePGNPath
    match tournament.Opening.OpeningsPath with // handle the optional field with pattern matching
    | Some path -> printfn "Opening path: %s" path 
    | None -> printfn "Opening path: None"
    printfn $"{nameof(tournament.Opening.OpeningsTwice)}: {tournament.Opening.OpeningsTwice}"    
    printfn $"{nameof(tournament.Opening.OpeningsPly)}: {tournament.Opening.OpeningsPly}"  
    printfn $"Win adjudication: {tournament.AdjudicationText()}"
    printfn $"Policy head test: {tournament.TestOptions.PolicyTest}"
    printfn $"Value head test: {tournament.TestOptions.ValueTest}"
    printfn $"Number of games in parallel: {tournament.TestOptions.NumberOfGamesInParallelConsoleOnly}"    

    printfn "Engines in tournament:\n"
    let engines = tournament.EngineSetup.Engines
    if engines.Length > 0 then
        for engine in engines do 
            printfn "\tEngine name: %s" engine.Name
            printfn "\tProtocol: %s" engine.Protocol 
            printfn "\tNetwork path: %s" engine.NetworkPath
            printfn "\tPath to executable: %s" engine.Path 
            printfn "\tUCI setoptions:" 
            for opt in engine.Options do
                printfn $"\t\t{(opt.Key)}: {opt.Value}"      
            printfn ""
    else 
        printfn "No engines in tournament"
        

  let writeEngineStatsPerGame (engineStat: EngineStatsPerGame) n =
      let speed nps = Formatting.formatNPS nps
      let line = sprintf "%-*s : %-5d %-10s %-10s %-10s %-10s %-8.0f %-8.0f %-8.0f %-8.0f" 
                    n engineStat.Player engineStat.GameNr (speed engineStat.AvgNodes) (speed engineStat.MedianNodes) (speed engineStat.AvgNps)
                    (speed engineStat.MedianNps) engineStat.AvgDepth engineStat.MedianDepth engineStat.AvgSD engineStat.MedianSD
      line

  let writeSummaryEngineStats (stat: SummaryEngineStat) n =
      let speed nps = Formatting.formatNPS nps
      let time = Formatting.formatMoveTime stat.Time
      let line = sprintf "%-*s : %-7d %-11s %-11s %-8.0f %-7.0f %-7s" 
                    n stat.Player stat.Games (speed stat.AvgNodes) (speed stat.AvgNPS) stat.AvgDepth stat.AvgSelfDepth time
      line
 
  let writeEngineStatHeader (n:int) : string =    
      sprintf "%-*s : %-5s %-10s %-10s %-10s %-10s %-8s %-8s %-8s %-8s" n "# PLAYER" "Game#" "AvgNodes" "MedNodes" "AvgNPS" "MedNPS" "AvgDepth" "MedDepth" "AvgSD" "MedSD"

  let writeSummaryEngineStatHeader (n:int) : string =    
      sprintf "%-*s : %-7s %-11s %-11s %-8s %-7s %-7s" n "# PLAYER" "Games" "Nodes" "NPS" "Depth" "SD" "Time"

  let writeEngineStatsToConsole (engineStats: EngineStatsPerGame seq) = 
      let sb = System.Text.StringBuilder()
      let appendLine (txt:string) = sb.AppendLine txt |> ignore
      sb.Clear() |> ignore
      appendLine "\n```\n"
      //find longest player name
      let longestName = engineStats |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
      writeEngineStatHeader longestName |> appendLine
      for engineStat in engineStats do      
        writeEngineStatsPerGame engineStat longestName |> appendLine
      appendLine "\n```"      
      let res = sb.ToString()
      res

  let writeSummaryEngineStatsToConsole (engineStats: SummaryEngineStat seq) = 
      let sb = System.Text.StringBuilder()
      let appendLine (txt:string) = sb.AppendLine txt |> ignore
      sb.Clear() |> ignore
      appendLine "\n```\n"
      //find longest player name
      let longestName = engineStats |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
      writeSummaryEngineStatHeader longestName |> appendLine
      for engineStat in engineStats do      
        writeSummaryEngineStats engineStat longestName |> appendLine
      appendLine "\n```"      
      let res = sb.ToString()
      res

module Validation =
  
  type ValidationResult = 
      | Ok
      | Errors of string list

  type EngineConfigValidationResult =
      | Valid
      | Invalid of string list

  let validateEngineConfigJson (json: string) : EngineConfigValidationResult =
    let bytes = Encoding.UTF8.GetBytes(json)
    let reader = Utf8JsonReader(bytes, isFinalBlock=true, state=JsonReaderState())

    // Tracking found properties and errors
    let foundProperties = System.Collections.Generic.HashSet<string>()
    let errors = System.Collections.Generic.List<string>()

    try
        while reader.Read() do
            match reader.TokenType with
            | JsonTokenType.PropertyName ->
                let propertyName = reader.GetString()
                if not (String.IsNullOrEmpty(propertyName)) then
                    foundProperties.Add(propertyName) |> ignore

                    // Example type check: Ensure 'TimeControlID' is an integer
                    if propertyName = "TimeControlID" && not (reader.Read() && reader.TokenType = JsonTokenType.Number) then
                        errors.Add("TimeControlID must be a number.")
            | _ -> ()

        // Ensure all required properties are found
        let requiredProperties = 
            ["Name"; "TimeControlID"; "Version"; "Rating"; "LogoPath"; "Protocol"; "Path"; "NetworkPath"; "Options"]
        
        let missingProperties = 
            requiredProperties |> List.filter (fun prop -> not (foundProperties.Contains(prop)))
        
        if missingProperties.Length > 0 then
            errors.Add(sprintf "Missing required properties: %A" missingProperties)

        if errors.Count > 0 then
            Invalid (List.ofSeq errors)
        else
            Valid
    with
    | :? JsonException as ex ->
        Invalid [sprintf "JSON syntax error: %s" ex.Message]

  let readAndValidateEngineConfigJson (json: string) (name: string) =
    match validateEngineConfigJson json with
    | Valid ->
        printfn $"JSON is valid for EngineConfig in engine {name}"
        // Proceed with deserialization or further processing here
        Valid
    | Invalid errMsgs ->
        errMsgs |> List.iter (fun m -> printfn $"Invalid EngineConfig JSON property: {m} in engine {name}")
        Invalid errMsgs

  // Function to check if a folder exists
  let checkFolderExists (path: string) = Directory.Exists(path)

  // Function to check if a path exists
  let checkPathExists (path: string) = File.Exists(path)

  let combinePaths (networkPath: string) (path: string)  = 
    let combined = Path.Combine(networkPath, path)
    checkPathExists combined

  let checkIfNeuralNetExists (networkPath: string) (path: string)  = 
    match File.Exists(path) with
    | true -> true    
    | false -> 
      let combined = Path.Combine(networkPath, path)
      checkPathExists combined

  let accumulateErrors results =
      let errors = 
          results 
          |> List.choose (function | Errors msgs -> Some msgs | Ok -> None)
          |> List.concat
      if errors.Length > 0 then Errors errors else Ok

  let validatePath (config: EngineConfig) =
    if checkPathExists config.Path |> not then   
        let msg = sprintf "Executable path to %s does not exist - Path given: %s" config.Name config.Path
        Errors [msg]
    else Ok

  let validateSyzygyFolder (config: EngineConfig) =
      match config.Options |> Seq.tryFind (fun kvp -> kvp.Key.Contains "SyzygyPath") with
      | None -> Ok
      | Some nn ->
          let folderPath = nn.Value |> string
          if checkFolderExists folderPath |> not then       
              Errors [sprintf "Database path (SyzygyPath) %s in %s does not exist" folderPath config.Name]
          else
              let files = Directory.GetFiles(folderPath)
              if files.Length = 0 then
                  Errors [sprintf "Database path (SyzygyPath) %s in %s does not contain any files" folderPath config.Name]
              else Ok

  let validateWeightsFile (config: EngineConfig) =
      match config.Options |> Seq.tryFind (fun kvp -> kvp.Key.Contains "WeightsFile") with
      | None -> Ok // No weights file option to validate
      | Some nn ->
          let weightsPath = nn.Value |> string
          if checkPathExists weightsPath || checkIfNeuralNetExists config.NetworkPath weightsPath then Ok
          else 
              let combinedPath = Path.Combine(config.NetworkPath, weightsPath)
              Errors [sprintf "Neural net in combined path %s or in direct path %s for %s does not exist" combinedPath weightsPath config.Name]

  let validateChessEngineCmds (config: EngineConfig) =
      [
        validatePath config 
        validateSyzygyFolder config
        validateWeightsFile config

      ] |> accumulateErrors

  let validateUniqueNames (engines: EngineConfig seq) =
      let names = engines |> Seq.map (fun e -> e.Name)
      let uniqueNames = names |> Seq.distinct |> Seq.toArray
      if uniqueNames.Length <> Seq.length names then
          //find duplicates
          let duplicates = 
              names
              |> Seq.groupBy id
              |> Seq.filter (fun (k, v) -> Seq.length v > 1)
              |> Seq.map (fun (k, v) -> k)
              |> Seq.toList
          Errors [sprintf "All engines must have a unique name - duplicates %A" duplicates]
      else Ok

  let validateEnginesPresent (engines: EngineConfig seq) nChallengers =      
      let otherEngines = (engines |> Seq.length) - nChallengers
      if engines |> Seq.length < 2 then
          Errors ["No engines found - at least two engines must be defined in EngineDefList in tournament.json"]
      else if otherEngines < 1 then
          Errors ["Gauntlet tournament configuration error: All engines are marked as challengers. In a gauntlet tournament, challengers need opponents to play against. Please either reduce the 'Challengers' parameter in tournament.json or add more engines to your EngineDefList so that some engines serve as non-challengers."]
      else
          Ok

  let validateEngineNames (engines: EngineConfig seq) =
      engines
      |> Seq.toList
      |> List.map (fun engine -> if String.IsNullOrEmpty engine.Name then Errors ["All engines must have a name"] else Ok)
      |> accumulateErrors

  let validateAllEnginesAndSomeSettings (engines: EngineConfig seq) =
      let engines = engines |> Seq.toList
      let engineValidations = engines |>  List.map validateChessEngineCmds
      let results = 
        validateUniqueNames engines :: validateEngineNames engines :: engineValidations
        |> accumulateErrors

      match results with
      | Ok -> ConsoleUtils.printInColor ConsoleColor.Green "All engines passed limited validation of key settings"
      | Errors msgs ->
          for msg in msgs do
              ConsoleUtils.printInColor ConsoleColor.Red msg

  let validateOpeningPath (tourny: Tournament) =
      match tourny.Opening.OpeningsPath with
      | Some path when not (String.IsNullOrEmpty path) ->
          let fileInfo = FileInfo(path)
          if not fileInfo.Exists then
              Errors [sprintf "The file path set in tournament.json for the OpeningsPath: %s does not exist" fileInfo.FullName]
          else
              // Further validations can be added here as needed
              Ok
      | Some p -> 
            if String.IsNullOrWhiteSpace p then
                Ok
                //Errors ["OpeningsPath is empty in tournament.json"]
            else
                Errors ["OpeningsPath is not set in tournament.json"]
      | None -> Ok  // If path not required, return Ok, otherwise adjust as necessary

  let validatePgnOutPath (tourny: Tournament) =
      if String.IsNullOrEmpty(tourny.PgnOutPath) then
          Errors ["PgnOutPath is not set in tournament.json"]
      else
          let fileInfo = FileInfo(tourny.PgnOutPath)
          if not fileInfo.Directory.Exists then
              Errors [sprintf "The directory for the PgnOutPath: %s in tournament.json does not exist" fileInfo.Directory.FullName]
          else
              Ok

  let validateEngineConfigs (configs: EngineConfig list) =
      if configs.Length = 0 then
          Errors ["No engines found - at least two engines must be defined in EngineDefList in tournament.json in order to run a tournament"]
      elif configs.Length < 2 then
          Errors ["Only one engine found - at least two engines must be defined in EngineDefList in tournament.json in order to run a tournament"]
      else
          Ok

  let validateEngineTimeControls (tourny: Tournament) (configs: EngineConfig list) =
      configs
      |> List.map (fun config ->
          if tourny.TimeControl.TimeConfigs |> List.exists (fun e -> e.Id = config.TimeControlID) |> not then
              Errors [sprintf "Time control id = %d does not exist in tournament.json for %s - please make sure that all engineDef.json files have a valid TimeControlID" config.TimeControlID config.Name]
          else Ok)
      |> accumulateErrors

  let validateDelayBetweenGames (tourny: Tournament) (configs: EngineConfig list) =
      let nodelimit = configs |> List.map (fun e -> tourny.FindTimeControl e.TimeControlID) |> List.forall (fun e -> e.NodeLimit)
      if tourny.DelayBetweenGames.ToTimeSpan().TotalSeconds < 2 && not nodelimit then
          Errors ["Delay between games very low - consider to increase it to at least 5 seconds in tournament.json"]
      else Ok

  let validateTournament (tourny: Tournament) =
      [
          validateEnginesPresent tourny.EngineSetup.Engines tourny.Challengers
          validateOpeningPath tourny
          validatePgnOutPath tourny
          validateEngineConfigs tourny.EngineSetup.Engines
          validateEngineTimeControls tourny tourny.EngineSetup.Engines
          validateDelayBetweenGames tourny tourny.EngineSetup.Engines
          validateUniqueNames tourny.EngineSetup.Engines
          validateEngineNames tourny.EngineSetup.Engines
          for config in tourny.EngineSetup.Engines do
              validateChessEngineCmds config
          // Add more validation functions here as needed
      ] |> accumulateErrors
      
  let validateTournamentInput (tourny: Tournament) =
      match validateTournament tourny with
      | Ok -> ConsoleUtils.printInColor ConsoleColor.Green "Tournament passed limited validation of key settings"
      | Errors msgs ->
          for msg in msgs do
              ConsoleUtils.printInColor ConsoleColor.Red msg

module Time =
  let prettyPrintTimeSpan (timeSpan: TimeSpan) =
    let hours = timeSpan.Hours
    let minutes = timeSpan.Minutes
    let seconds = timeSpan.Seconds
    sprintf "%02dh %02dm %02ds" hours minutes seconds

module FEN =

  let parseFENandMoves (fenMoves: string) =
      if String.IsNullOrWhiteSpace(fenMoves) then
          ("", [||])
      else
          let parts = fenMoves.Split([| "moves" |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
          let fen = if parts.Length > 0 then parts.[0].Trim() else ""
          let moves =
              if parts.Length > 1 then
                  parts.[1].Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
              else
                  [||]
          (fen, moves)

  let extractFEN (inputString: string) =
      let prefix = "position fen "
      let idx = inputString.IndexOf(prefix, StringComparison.Ordinal)
      if idx = -1 then
          None
      else
          let startIndex = idx + prefix.Length
          if startIndex < inputString.Length then
              Some(inputString.Substring(startIndex))
          else
              None

module Engine =

  let calcTopNn (nnValues : NNValues seq) =    
    if nnValues |> Seq.length < 3 then
      None
    else
      let arr = nnValues |> Seq.toArray |> Array.rev |> Array.skip 1
      let nodes = arr |> Array.sortBy(fun e -> -e.Nodes)
      let qs = arr |> Array.sortBy(fun e -> -e.Q)
      let ps = arr |> Array.sortBy(fun e -> -e.P)
      (nodes[0].Nodes, nodes[1].Nodes, nodes[0].Q, qs[0].Q, nodes[0].P, ps[0].P) |> Some

  
  let createLC0BenchmarkString (config: EngineConfig) =
    let sb = StringBuilder()    
    let append (s:string) = sb.Append (s + " ") |> ignore
    //append "& '"
    append config.Path
    //append "'"
    //append " benchmark"
    let options = UciOptions.createCommandsFromConfig config
    append options
    //append " --num-positions=1 --movetime=10000"
    sb.ToString()

module Random =
  let rnd = Random()

  let ShuffleSpan(values: Span<'a>) =
      let n = values.Length

      for i in 0 .. n - 2 do
          let j = rnd.Next(i, n)

          if j <> i then
              let temp = values.[i]
              values.[i] <- values.[j]
              values.[j] <- temp
  
  let Shuffle(values: 'a array) =
      if values = null then
          ArgumentNullException("values") |> raise

      ShuffleSpan(values.AsSpan())


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


module ZobrishHash =

    /// ZobristPiece[pieceType][square], pieceType in [0..11], square in [0..63]
  let ZobristPiece = Array2D.zeroCreate<uint64> 12 64

  /// ZobristCastling[16] if you track 4 bits of castling rights in a single nibble
  let ZobristCastling = Array.zeroCreate<uint64> 16

  /// ZobristEnPassant[8]
  let ZobristEnPassant = Array.zeroCreate<uint64> 8

  /// ZobristSide: 0 => White to move, 1 => Black to move
  let ZobristSide = Array.zeroCreate<uint64> 2

  let initializeZobristTables () =    
    let rnd = System.Random(153)    
    let getRand64 () =
      let bytes = Array.zeroCreate<byte> 8
      rnd.NextBytes(bytes)
      BitConverter.ToUInt64(bytes, 0)
    for i in 0..11 do
        for j in 0..63 do
            ZobristPiece.[i, j] <- getRand64 ()
    for i in 0..15 do
        ZobristCastling.[i] <- getRand64 ()
    for i in 0..7 do
        ZobristEnPassant.[i] <- getRand64 ()
    for i in 0..1 do
        ZobristSide.[i] <- getRand64 ()

  /// Which piece type (0..5) do P2,P1,P0 represent, or -1 if empty?
  let getPieceCode (pos: Position) (s: int) =
    let occ = (1UL <<< s)
    // If not occupied => empty
    if (PositionOps.occupation &pos &&& occ) = 0UL then
        -1
    else
        // figure out the 3-bit code
        let p2 = if (pos.P2 &&& occ) <> 0UL then 1 else 0
        let p1 = if (pos.P1 &&& occ) <> 0UL then 1 else 0
        let p0 = if (pos.P0 &&& occ) <> 0UL then 1 else 0
        match (p2 <<< 2) ||| (p1 <<< 1) ||| p0 with
        | 0 -> -1  // empty
        | 1 ->  0  // pawn
        | 2 ->  1  // knight
        | 3 ->  2  // bishop
        | 4 ->  3  // rook
        | 5 ->  4  // queen
        | 6 ->  5  // king
        | _ -> -1  // shouldn't happen with your design

  let isSquareSideToMove (pos: Position) (s: int) =
    ((pos.PM &&& (1UL <<< s)) <> 0UL)

  // sideToMove for White or Black
  // pos.STM = 0uy => White, 8uy => Black
  let isWhiteSTM (pos: Position) = (pos.STM = 0uy)

  // Convert (pieceCode, color) => index in [0..11]
  let getZobristIndex (pieceCode: int) (isWhite: bool) =
    match pieceCode with
    | 0 -> if isWhite then 0 else 6   // Pawn
    | 1 -> if isWhite then 1 else 7   // Knight
    | 2 -> if isWhite then 2 else 8   // Bishop
    | 3 -> if isWhite then 3 else 9   // Rook
    | 4 -> if isWhite then 4 else 10  // Queen
    | 5 -> if isWhite then 5 else 11  // King
    | _ -> -1
  
  // We want a nibble: [LM, SM, LO, SO], each 1 bit => 1 in that position
  let getCastleIndex (pos: Position) =
    let mutable castleIndex = 0
    if PositionOps.CanCastleLM &pos then castleIndex <- castleIndex ||| 0x1
    if PositionOps.CanCastleSM &pos then castleIndex <- castleIndex ||| 0x2
    if PositionOps.CanCastleLO &pos then castleIndex <- castleIndex ||| 0x4
    if PositionOps.CanCastleSO &pos then castleIndex <- castleIndex ||| 0x8
    castleIndex
  
  let computeBoardHash (pos: Position) =
    let mutable key = 0UL
    for s in 0..63 do
        let pc = getPieceCode pos s
        if pc >= 0 then
            let squareIsSideToMove = isSquareSideToMove pos s
            // is it White or Black occupant?
            // If 'pos.STM=0uy' (White) and 'squareIsSideToMove=true', that occupant is White
            // If 'pos.STM=8uy' (Black) and 'squareIsSideToMove=true', that occupant is Black
            // otherwise it’s the opposite color
            let occupantIsWhite =
                if squareIsSideToMove then
                    isWhiteSTM pos
                else
                    not (isWhiteSTM pos)

            let zobIndex = getZobristIndex pc occupantIsWhite
            if zobIndex >= 0 then
                key <- key ^^^ ZobristPiece.[zobIndex, s]
    key

  /// Compute a fresh Zobrist hash from the entire Position
  let computeZobrist (pos: Position) =
    //let boardStr = positionToString("zobrist", &pos)
    let mutable key = 0UL

    // (1) Board occupancy
    key <- key ^^^ (computeBoardHash pos)

    // (2) Side to move
    if not (isWhiteSTM pos) then
        key <- key ^^^ ZobristSide.[1]
    else
        key <- key ^^^ ZobristSide.[0]

    // (3) Castling flags
    let ci = getCastleIndex pos
    key <- key ^^^ ZobristCastling.[ci]

    // (4) En passant
    let epFile = int (PositionOps.enPass &pos)
    if epFile < 8 then
        key <- key ^^^ ZobristEnPassant.[epFile]    
    key


module Hash =
  open System.Security.Cryptography

  let computeOpeningHash (input: string) =
    use sha256 = SHA256.Create()
    let bytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = sha256.ComputeHash(bytes)
    BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

  let hashBoard board =       
      let zobrist = ZobrishHash.computeZobrist board      
      zobrist      

  let deviationHash board =
      ZobrishHash.computeZobrist board + (uint64 (hash board.Ply)) 
      
  let getOpeningInfo (game:PgnGame) =
    let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening" )
    let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation" )
    let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO" )
    match opening, variation, eco with
    |Some op, Some v, Some eco -> sprintf "Opening: %s - %s, ECO: %s" op.Value v.Value eco.Value
    |Some h,Some v, None -> sprintf "Opening: %s - %s" h.Value v.Value
    |Some h,_ , Some eco -> sprintf "Opening: %s, ECO: %s" h.Value eco.Value
    |None, None, Some eco -> sprintf "ECO: %s" eco.Value
    |Some op, None, None -> sprintf "%s" op.Value
    |_ -> 
      if opening.IsSome then
        sprintf "Nr %i: %s" game.GameNumber opening.Value.Value
      elif String.IsNullOrEmpty game.Fen then
        sprintf "Nr %i: No opening name" game.GameNumber
      else
        sprintf "Nr %i: %s" game.GameNumber game.Fen

  let writeOpeningHashToPgnGame (game: PgnGame)  =      
      let sb = new StringBuilder()
      let sanMoves = 
        let moves = game.Moves |> Seq.takeWhile (fun m -> m.WhiteComment.Contains "book" || m.BlackComment.Contains "book") |> Seq.toList
        if moves.Length = 0 then
          //take while white or black comment is empty
          game.Moves |> Seq.takeWhile (fun m -> m.WhiteComment = "" || m.BlackComment = "") |> Seq.toList
        else
          moves
      let fen = game.GameMetaData.Fen
      if String.IsNullOrEmpty fen |> not then
        sb.AppendLine(sprintf "[Fen \"%s\"]" fen ) |> ignore      
      let mutable ply = 1
      for m in sanMoves do
        //if m.WhiteComment.Contains "book" then
          sb.Append(sprintf "%d.%s " ply m.WhiteSan) |> ignore
          ply <- ply + 1
          if String.IsNullOrEmpty m.BlackComment |> not then
            sb.Append(sprintf "%s " m.BlackSan) |> ignore
      //sb.Append("*") |> ignore
      sb.AppendLine() |> ignore
      let openingText = sb.ToString()
      game.GameMetaData.OpeningHash <- computeOpeningHash openingText      
      

module JSON =
    
  let readEngineConfig path =
      let json = File.ReadAllText(path)
      JsonSerializer.Deserialize<EngineConfig[]>(json, JsonSerializerOptions(AllowTrailingCommas = true))

  let readSingleEngineConfig path =
    let json = File.ReadAllText(path)
    JsonSerializer.Deserialize<EngineConfig>(json, JsonSerializerOptions(AllowTrailingCommas = true))

  let readEngineDef folder fileName = 
    try
      let path = Path.Combine(folder,fileName)
      let json = File.ReadAllText(path)
      match Validation.readAndValidateEngineConfigJson json fileName with
      | Validation.Valid -> JsonSerializer.Deserialize<EngineConfig>(json, JsonSerializerOptions(AllowTrailingCommas = true))
      | Validation.Invalid msgs -> 
          msgs |> List.iter (fun m -> ConsoleUtils.printInColor ConsoleColor.Red m)
          failwith "Invalid engine definition"
      //JsonSerializer.Deserialize<EngineConfig>(json)
    with
    | ex -> 
      printfn "Error in reading engine definition in JSON format from this file: %s/%s" folder fileName
      sprintf "Complete error message: %s" ex.Message |> failwith

  //let readEngineDef folder fileName = 
  //  let path = Path.Combine(folder,fileName)
  //  let json = File.ReadAllText(path)
  //  JsonSerializer.Deserialize<EngineConfig>(json)

  let readEngineDefs folder engineDefList = 
    [ for def in engineDefList -> readEngineDef folder def  ]

  let readTournamentJson (path: string) : Tournament option =
      try
          use reader = new StreamReader(path)
          let json = reader.ReadToEnd()
          let tournament = JsonSerializer.Deserialize<Tournament>(json, JsonSerializerOptions(AllowTrailingCommas = true))
          if tournament.EngineSetup.EngineDefList.Length = 0 then
             Some {tournament with EngineSetup = {tournament.EngineSetup with Engines = []}}             
          else              
              Some tournament         
      with
      | ex -> 
          ConsoleUtils.printInColor ConsoleColor.Red (sprintf "***Note: Tournament.json file %s was not found in working directory" path)
          ConsoleUtils.printInColor ConsoleColor.White "\nA new (empty) tournament.json will be created with default settings\n"
          None

  let writeTournamentJson (tournament: Tournament) (path: string) : unit =
      try                  
          let options = JsonSerializerOptions(WriteIndented = true)
          options.AllowTrailingCommas <- true
          options.PreferredObjectCreationHandling <- JsonObjectCreationHandling.Populate          
          let json = JsonSerializer.Serialize(tournament,options)          
          let combinedPath = Path.Combine(path, "tournament.json")
          File.WriteAllText(combinedPath, json)
      with
      | ex -> ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Error: %s" ex.Message) // handle any exceptions

  
  let loadBaseConfig (jsonPath: string) =
      let json = File.ReadAllText(jsonPath)
      let options = JsonSerializerOptions()      
      options.PropertyNameCaseInsensitive <- true
      JsonSerializer.Deserialize<EngineConfig>(json, options)

  // 2. Clone the Options dictionary for each engineConfig
  let cloneOptions (dict: Dictionary<string,obj>) =
      Dictionary<string,obj>(dict)

  let makeEngineConfigFile (baseConfig: EngineConfig) (networkPath: string) =
      // Copy the dictionary so we don't mutate the base dictionary
      let newOptions = cloneOptions baseConfig.Options
      // Update the WeightsFile path
      newOptions.["WeightsFile"] <- networkPath :> obj

      // Append ONNX filename (without extension) to the base Name
      let onnxName = Path.GetFileNameWithoutExtension(networkPath)
      //only get first part of the baseConfig name here
      let baseConfigName = baseConfig.Name.Trim().Split(" ").[0]
      let newName = sprintf "%s %s" baseConfigName onnxName

      // Create a fresh record
      { baseConfig with
          Name = newName
          Options = newOptions }

  
  let generateCeresJsonFiles (baseConfig: EngineConfig) (onnxFolderPath: string) (outputFolderPath: string) =      
      let onnxFiles = Directory.GetFiles(onnxFolderPath, "*.onnx")

      // For writing JSON, we also set WriteIndented = true for readability
      let writeOptions = JsonSerializerOptions(WriteIndented = true)      

      for onnxFile in onnxFiles do
        let newConfig = makeEngineConfigFile baseConfig onnxFile
        let outputFilename = newConfig.Name + ".json"
        let outputPath = Path.Combine(outputFolderPath, outputFilename)

        // Serialize to JSON
        let newJson = JsonSerializer.Serialize(newConfig, writeOptions)
        File.WriteAllText(outputPath, newJson)
        printfn "Created %s" outputPath
      printfn "Generated %d JSON files" onnxFiles.Length
  
  let generateLc0sonFiles (baseConfig: EngineConfig) (networkFolderPath: string) (outputFolderPath: string) =      
      let networkFiles = Directory.GetFiles(networkFolderPath, "*.pb.gz")
      //Directory.GetFiles(networkFolderPath) |> Array.filter (fun f -> not (f.EndsWith(".onnx") || f.EndsWith(".json")))

      // For writing JSON, we also set WriteIndented = true for readability
      let writeOptions = JsonSerializerOptions(WriteIndented = true)     

      for networkFile in networkFiles do
        let newConfig = makeEngineConfigFile baseConfig networkFile
        let outputFilename = newConfig.Name + ".json"
        let outputPath = Path.Combine(outputFolderPath, outputFilename)

        // Serialize to JSON
        let newJson = JsonSerializer.Serialize(newConfig, writeOptions)
        File.WriteAllText(outputPath, newJson)
        printfn "Created %s" outputPath
      printfn "Generated %d JSON files" networkFiles.Length

  let getAllConfigFiles (folder: string) =
    let engineConfigs = Directory.GetFiles(folder, "*.json") 
    let outputFolder = Path.Combine(folder, "output_EngineJson")
    Directory.CreateDirectory(outputFolder) |> ignore
    for path in engineConfigs do
      let baseConfig = loadBaseConfig path        
      let networkFolderPath = folder
      if baseConfig.Path.ToLower().Contains("ceres") then
        generateCeresJsonFiles baseConfig networkFolderPath outputFolder
      else
        generateLc0sonFiles baseConfig networkFolderPath outputFolder
  
  let createTournamentFile (tournyPath: string) (engineFolder : string) =      
    //get all json files in the engine folder
    let engineFiles = 
      Directory.GetFiles(engineFolder, "*.json") 
      |> Array.map(fun path -> Path.GetFileName path) 
      |> Array.toList
      |> List.filter (fun f -> not (f.ToLower().Contains("tournament")))
    
    match readTournamentJson tournyPath with
    | Some tournament -> 
      { tournament 
          with 
            EngineSetup = { tournament.EngineSetup with EngineDefFolder = engineFolder; EngineDefList = engineFiles; Engines = [] }            
      }
    | None -> failwith "Error in reading tournament file"


module JSONParser =
    open TypesDef.Puzzle
    
    let escapeString (s:string) =
        if String.IsNullOrEmpty s then
            ""
        else
            // Escape special characters for JSON string representation
          let sb = System.Text.StringBuilder()
          for c in s do
              match c with
              | '\\' -> sb.Append("\\\\") |> ignore
              | '"'  -> sb.Append("\\\"") |> ignore
              | '\n' -> sb.Append("\\n")  |> ignore
              | '\r' -> sb.Append("\\r")  |> ignore
              | '\t' -> sb.Append("\\t")  |> ignore
              | c when Char.IsControl c ->
                  // anything else in the 0–31 range → Unicode escape
                  sb.Append(sprintf "\\u%04X" (int c)) |> ignore
              | c ->
                  sb.Append(c) |> ignore
          sb.ToString()

    let parsePuzzle (filePath: string) (random: bool) : CsvPuzzleData[] =
        
        // Parse one line into a CsvPuzzleData
        let parseLine (line: string) =
            let fields = line.Split(',')
            let puzzleId        = fields.[0].GetHashCode()
            let fen             = fields.[1]
            let moves           = fields.[2]
            let rating          = int fields.[3]
            let ratingDeviation = int fields.[4]
            let popularity      = int fields.[5]
            let nbPlays         = int fields.[6]
            let themes          = fields.[7]
            let gameUrl         = fields.[8]
            let openingTags     = fields.[9]
            let fens            = ResizeArray<string>()   // placeholder
            let posList         = ResizeArray<Position>() // placeholder

            CsvPuzzleData.Create(
              puzzleId, fen, moves, rating, ratingDeviation,
              popularity, nbPlays, themes, gameUrl,
              openingTags, line, posList, fens, 0
            )

        // Map each line to a record in order
        let records =           
          File.ReadAllLines(filePath)
          |> Array.skip 1
          |> Array.map parseLine

        // Optionally shuffle
        if random then
            System.Random.Shared.Shuffle(records)

        records

    
    let parsePuzzleInParallel (filePath: string) (random: bool) : CsvPuzzleData[] =
        let lines = File.ReadAllLines(filePath) |> Array.skip 1

        // Map each line to a CsvPuzzleData in parallel, preserving order
        let recordsArray =
            lines
            |> Array.Parallel.map (fun line ->
                let fields = line.Split(',')
                // ... parse fields just like before ...
                CsvPuzzleData.Create(
                    fields.[0].GetHashCode(),
                    fields.[1],
                    fields.[2],
                    int fields.[3],
                    int fields.[4],
                    int fields.[5],
                    int fields.[6],
                    fields.[7],
                    fields.[8],
                    fields.[9],
                    line,
                    ResizeArray<Position>(),
                    ResizeArray<string>(),
                    0 ))

        if random then
            System.Random.Shared.Shuffle(recordsArray)

        recordsArray

    let normalizePath (path: string) = 
      if String.IsNullOrEmpty path then
        ""      
      else
        escapeString path        
  
    let loadEretConfig (filePath: string) : TypesDef.Puzzle.EretConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(TypesDef.Puzzle.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<TypesDef.Puzzle.EretConfig>(json, options)
        else
            failwithf "File not found: %s" filePath
    
    let loadAnalyzeConfig (filePath: string) : TypesDef.Puzzle.AnalyzeConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(TypesDef.Puzzle.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<TypesDef.Puzzle.AnalyzeConfig>(json, options)
        else
            failwithf "File not found: %s" filePath

    let loadPuzzleConfig (filePath: string) : TypesDef.Puzzle.PuzzleConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(TypesDef.Puzzle.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<TypesDef.Puzzle.PuzzleConfig>(json, options)
        else
            failwithf "File not found: %s" filePath
  
    let mapToEngConfig  (engineFolder: string) (engine: TypesDef.Puzzle.PuzzleEngine) =
        match engine with
        | TypesDef.Puzzle.Engine (name, _) -> 
            [Path.Combine(engineFolder, name) |> JSON.readSingleEngineConfig]
        | TypesDef.Puzzle.EngineWithNets (name, _, nets) -> 
            let fullPath = Path.Combine(engineFolder, name)
            [
                for net in nets do
                    let engineConfig = JSON.readSingleEngineConfig fullPath
                    let options = engineConfig.Options
                    
                    if options.ContainsKey("WeightsFile") then
                        options.["WeightsFile"] <- box net
                       
                    elif options.ContainsKey("Network") then
                        match options.["Network"] with
                        | :? JsonElement as jsonElem when jsonElem.ValueKind = JsonValueKind.String -> 
                            let prev = jsonElem.GetString()
                            match prev.Split(':') |> Seq.tryHead with
                            | Some prefix -> 
                                let prefixNet = prefix + ":" + net
                                options.["Network"] <-  box prefixNet
                            |_ -> ()
                        | _ -> ()
                        
                    let netName = Path.GetFileNameWithoutExtension net
                    let firstName = engineConfig.Name.Split(' ') |> Seq.head
                    let engineConfig = {engineConfig with Name = firstName + " " + netName}                                
                    yield engineConfig
            ] 

    let mapToEngPuzzleConfig  (engineFolder: string) (engine: TypesDef.Puzzle.PuzzleEngine) =
        match engine with
        | TypesDef.Puzzle.Engine (name, nodes) -> 
            [Path.Combine(engineFolder, name) |> JSON.readSingleEngineConfig, nodes]
        | TypesDef.Puzzle.EngineWithNets (name, nodes, nets) -> 
            let fullPath = Path.Combine(engineFolder, name)
            [
                for net in nets do
                    let engineConfig = JSON.readSingleEngineConfig fullPath
                    let options = engineConfig.Options
                    
                    if options.ContainsKey("WeightsFile") then
                        options.["WeightsFile"] <- box net
                       
                    elif options.ContainsKey("Network") then
                        match options.["Network"] with
                        | :? JsonElement as jsonElem when jsonElem.ValueKind = JsonValueKind.String -> 
                            let prev = jsonElem.GetString()
                            match prev.Split(':') |> Seq.tryHead with
                            | Some prefix -> 
                                let prefixNet = prefix + ":" + net
                                options.["Network"] <-  box prefixNet
                            |_ -> ()
                        | _ -> ()
                        
                    let netName = Path.GetFileNameWithoutExtension net
                    let firstName = engineConfig.Name.Split(' ') |> Seq.head
                    let engineConfig = {engineConfig with Name = firstName + " " + netName}                                
                    yield engineConfig, nodes
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
      PGNParser.parsePgnFile filePath
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
      PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile(results)
      |> Array.filter _.Median
      |> Array.sortByDescending _.AvgNPS
    
    for player in playerRes do
      let speed = avgSpeed |> Seq.tryFind (fun e -> e.Player = player.Player)
      if speed.IsSome then
          player.MedSpeed <- speed.Value.AvgNPS
          player.AvgNPM <- speed.Value.AvgNodes
    
    let consoleContent = OrdoHelper.getResultsAndPairsInConsoleFormat playerRes cross
    let engineStats = OrdoHelper.getEngineLineData playerRes cross

    consoleContent, engineStats, cross

  let calculateStatistics (engines:string list) results =        
      engines |> List.map (fun player -> calculateStatisticsForPlayer player results)

  let processStat (challengers: string list) (playerResults : _ list) =
    let challengerSet = Set(challengers)
    let notInChallengerSet (p:PlayerResult) = not (challengerSet.Contains p.Player)    
    let sortByPointsPlayed = List.sortBy (fun e -> -(e.Points / float e.Played))        
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


module PairingHelper =

  let seededKnockoutPairings (players: EngineConfig list) (opening: PGNTypes.PgnGame) =
    let sorted = players |> List.sortByDescending (fun e -> e.Rating)
    let rec pair (acc: _ list) lst =
        match lst with
        | [] -> acc
        | [single] -> acc // Odd player: bye (handle separately if needed)
        | hi::tl ->
            match List.rev tl with
            | [] -> acc // Should not happen, but safe
            | lo::midRev ->
                let mid = List.rev midRev
                let openingHash =
                    if System.String.IsNullOrWhiteSpace opening.Raw then
                        Hash.computeOpeningHash (opening.GameNumber.ToString())
                    else
                        Hash.computeOpeningHash opening.Raw
                let pairing = { Opening = opening; White = hi; Black = lo; GameNr = acc.Length + 1; RoundNr = $"1.{acc.Length+1}"; OpeningHash = openingHash }
                pair (pairing :: acc) mid
    pair [] sorted |> List.rev
        
    /// Generate knockout pairings for a single round
  let knockoutRound (players: EngineConfig list) (opening: PGNTypes.PgnGame) =
    let shuffled = players |> List.toArray
    Random.Shuffle(shuffled) // Optional: randomize seeding
    let pairs =
        shuffled
        |> Array.chunkBySize 2
        |> Array.choose (function
            | [|w; b|] -> Some (w, b)
            | [|w|] -> None // Odd player: bye (advance automatically)
            | _ -> None)
    pairs
    |> Array.mapi (fun idx (w, b) ->
        let openingHash =
            if System.String.IsNullOrWhiteSpace opening.Raw then
                Hash.computeOpeningHash (opening.GameNumber.ToString())
            else
                Hash.computeOpeningHash opening.Raw
        { Opening = opening; White = w; Black = b; GameNr = idx + 1; RoundNr = $"1.{idx+1}"; OpeningHash = openingHash }
    )
    |> Array.toList

    /// Recursively generate all rounds for a knockout tournament
  let rec knockoutTournament (players: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    match players with
    | [] | [_] -> [] // No games if 0 or 1 player
    | _ ->
        let opening = List.head openings // Use the same opening or rotate as needed
        let round = knockoutRound players opening
        // After games are played, collect winners and call recursively for next round
        // Here, you need to determine winners from results and pass them to the next round
        round // For pairing generation only; actual tournament logic needs to process results

  // Key generator
  let pairingKey (openingHash: string) (fen: string) (white: string) (black: string) =
        $"{openingHash}|{fen}|{white.Trim()}|{black.Trim()}"

    // Preprocess once
  let playedSet (gamesAlreadyPlayed : PgnGame array) =
        gamesAlreadyPlayed
        |> Array.map (fun e ->
            pairingKey
                (if String.IsNullOrEmpty e.GameMetaData.OpeningHash then e.GameNumber.ToString() else e.GameMetaData.OpeningHash)
                e.GameMetaData.Fen
                e.GameMetaData.White
                e.GameMetaData.Black)
        |> Set.ofArray

    // Fast check
  let hasPlayedBefore (pairing: Pairing) (playedSet: Set<string>) =
        let key = pairingKey pairing.OpeningHash pairing.Opening.GameMetaData.Fen pairing.White.Name pairing.Black.Name
        playedSet.Contains key
  
  /// Rotates a list by moving the first element to the end.
  /// Used in Berger round robin pairing to rotate the player list each round.
  let rotateListByOne (lst: 'a list) : 'a list =
      match lst with
      | [] -> []
      | head :: tail -> tail @ [head]

  /// Rotates a list by moving the last element to the second position.
  /// For [a; b; c; d] returns [a; d; b; c].
  /// This is the standard Berger rotation for round robin tournaments.
  let rotateOnce (players: 'a list) : 'a list =
      match players with
      | [] | [_] -> players
      | h :: t ->
          match List.rev t with
          | [] -> players
          | last :: revRest ->
              let rest = List.rev revRest
              h :: last :: rest
  

  let gauntletSingleRoundPerOpening (challengers: EngineConfig list) (opponents: EngineConfig list) (opening: PGNTypes.PgnGame) =
      
      [ let mutable subIndex = 0
        for o in opponents do
          for p in challengers do
            let openingHash = 
                if String.IsNullOrWhiteSpace opening.Raw then
                    Hash.computeOpeningHash (opening.GameNumber.ToString())
                else
                    Hash.computeOpeningHash opening.Raw
            subIndex <- subIndex + 1                
            let roundString = $"{opening.GameNumber}.{subIndex}"
            let roundMatches : Pairing list= [{Opening = opening; White=p; Black=o; GameNr = 0; RoundNr= roundString; OpeningHash = openingHash }] // Main player has white pieces
            yield roundMatches
      ] |> List.concat
      
   
  let gauntletSingleRound doNotDeviate (challengers: EngineConfig list) (opponents: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    [
      let mutable opponents = opponents
      for opening in openings do
        if doNotDeviate then
          opponents <- rotateListByOne opponents
        let games = gauntletSingleRoundPerOpening challengers opponents opening
        yield games
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})

  let gauntletDoubleRoundPerOpening (challengers: EngineConfig list) (opponents: EngineConfig list) (opening: PGNTypes.PgnGame) = 
      let singleMatches = gauntletSingleRoundPerOpening challengers opponents opening
      let reverseColors (pair:Pairing) (idx:int)  : Pairing = 
        {Opening=pair.Opening; White=pair.Black; Black= pair.White; OpeningHash = pair.OpeningHash; GameNr = pair.GameNr; RoundNr = $"{pair.Opening.GameNumber}.{idx}" }
      let fromIdx = singleMatches.Length + 1
      let reverseGames = singleMatches |> List.mapi (fun idx p -> reverseColors p (fromIdx + idx))
      (singleMatches @ reverseGames)

  let gauntletDoubleRound doNotDeviate (challengers: EngineConfig list) (opponents: EngineConfig list) (openings: PGNTypes.PgnGame list) =
    [
      let mutable opponents = opponents
      for opening in openings do
        if doNotDeviate then
          opponents <- rotateListByOne opponents
        let games = gauntletDoubleRoundPerOpening challengers opponents opening        
        yield games
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})    

  let getRotatedLists (players: EngineConfig list) =
    let padded =
        if List.length players % 2 = 1 then EngineConfig.Empty :: players else players
    let rounds = List.length padded - 1
    let rec rotate acc n current =
        if n = 0 then List.rev acc
        else
            let next = rotateOnce current
            rotate (next :: acc) (n - 1) next
    rotate [padded] (rounds - 1) padded

  let createRRPairs (players: EngineConfig list) (opening: PgnGame) round =
    let evenRound = round % 2 = 0
    let players =
        if List.length players % 2 = 1 then EngineConfig.Empty :: players else players
    let half = List.length players / 2
    let first, second = players |> List.splitAt half
    let zipped = List.zip first (List.rev second)
    [
        for (w, b) in zipped do
            if w.Name <> EngineConfig.Empty.Name && b.Name <> EngineConfig.Empty.Name then
                let (white, black) = if evenRound then (b, w) else (w, b)
                let openingHash =
                    if String.IsNullOrWhiteSpace opening.Raw then
                        Hash.computeOpeningHash (opening.GameNumber.ToString())
                    else
                        Hash.computeOpeningHash opening.Raw
                let roundString = $"{opening.GameNumber}.{1}"
                yield {
                    Opening = opening
                    White = white
                    Black = black
                    OpeningHash = openingHash
                    GameNr = 0
                    RoundNr = roundString
                }
    ]

  let getPairingsPerOpening (players: EngineConfig list) opening =
    let lists = getRotatedLists players
    let mutable round = 1
    [for rotatedList in lists do
      let pairings = createRRPairs rotatedList opening round
      round <- round + 1
      yield! pairings ]

  let generateAllRoundRobinSingleRounds (players: EngineConfig list) (openings: PGNTypes.PgnGame list) =  
    [
      for opening in openings do
        let games = getPairingsPerOpening players opening
        yield games      
    ] |> List.concat |> List.mapi(fun i e -> {e with GameNr = i + 1})

  let generateAllRoundRobinDoubleRounds (players: EngineConfig list) (openings: PGNTypes.PgnGame list) =  
    let reverseColors (pair:Pairing) (idx:int) : Pairing = 
        { Opening = pair.Opening; White = pair.Black; Black = pair.White; OpeningHash = pair.OpeningHash; GameNr = 0;  RoundNr = $"{pair.Opening.GameNumber}.{idx}" }
    [
      for opening in openings do
        let games = 
            getPairingsPerOpening players opening
            |> List.mapi (fun idx p -> {p with RoundNr = $"{opening.GameNumber}.{idx+1}"})

        yield! games
        let fromIdx = games.Length + 1
        let reverseGames = games |> List.mapi (fun idx p -> reverseColors p (fromIdx + idx))
        yield! reverseGames
    ]|> List.mapi(fun i e -> {e with GameNr = i + 1})

  let printAllOpeningPairs (logger: ILogger) (pairings: Pairing list) =
    let sb = StringBuilder()
    sb.AppendLine() |> ignore
    pairings
    |> List.iteri (fun idx p ->
        let openingName = PGNHelper.getOpeningInfo p.Opening
        let opName =
            if openingName.Contains "No opening name" && not (String.IsNullOrEmpty p.Opening.Fen) then
                p.Opening.Fen
            else
                openingName
        let msg = $"Round: {p.RoundNr}  ({idx + 1}): {p.GameNr}. {opName}, {p.White.Name} vs {p.Black.Name}"
        sb.AppendLine(msg) |> ignore
    )
    logger.LogInformation(sb.ToString())

  let getAllOpeningPairs (pairings: Pairing list) =
    let sb = StringBuilder()
    sb.AppendLine() |> ignore
    pairings
    |> List.iteri (fun idx p ->
        let openingName = PGNHelper.getOpeningInfo p.Opening
        let opName =
            if openingName.Contains "No opening name" then
                p.Opening.Fen
            else
                openingName
        let msg = $"Round: {p.RoundNr} ({p.GameNr}): {idx + 1}. {opName}, {p.White.Name} vs {p.Black.Name}"
        sb.AppendLine(msg) |> ignore
    )
    sb.ToString()  

module Regex =
  //"info string c1h6  (69  ) N:       6 (+ 0) (P:  0.41%) (WL: -0.99587) (D: 0.003) (M: 60.0) (Q: -0.99587) (U: 1.12920) (S:  0.09888) (V: -0.9982) "

  let mPvRegex = new Regex(@"\bmultipv\s+(\d+)\b", RegexOptions.Compiled)
  let depthRegex = new Regex(@"depth\s(\d+)", RegexOptions.Compiled)
  let sDepthRegex = new Regex(@"seldepth\s(\d+)", RegexOptions.Compiled)
  let nodesRegex = new Regex(@"nodes\s+(\d+)", RegexOptions.Compiled)
  let npsRegex = new Regex(@"nps\s+(\d+)", RegexOptions.Compiled)
  let pvRegex = new Regex(@"score.*pv\s(.*)", RegexOptions.Compiled)  //@"pv\s(.*)")
  let tbhitsRegex = new Regex("tbhits\s+(\d+)", RegexOptions.Compiled)
  let evalRegex = new Regex(@"score\s+(cp|mate)\s+(-?\d+)", RegexOptions.Compiled) 
  let wdlRegex = new Regex(@"wdl\s+(\d+)\s+(\d+)\s+(\d+)", RegexOptions.Compiled)  //wdl 160 385 454
  let ponderRegex = new Regex(@"pd=([a-zA-Z0-9+#=-]+)", RegexOptions.Compiled)
  let evalWvRegex = new Regex(@"wv=([+-]?M?-?\d+(\.\d+)?)", RegexOptions.Compiled)
  let evalRegexAlt = new Regex(@"([+-]?\d+\.\d+)", RegexOptions.Compiled)

  let parseEvalRegexOption line isblack =
    let test = evalWvRegex.Match(line)
    if test.Success then
      let eval = test.Groups.[1].Value
      if eval.StartsWith("M") then
        // Parse the mate score as an integer
        let mateScore = System.Int32.Parse(eval.TrimStart('M'))
        let highMateScore = if mateScore > 0 then 999.0 else -999.0
        Some highMateScore
      elif eval.StartsWith("-M") then
        //let mateScore = System.Int32.Parse(eval.TrimStart('-').TrimStart('M'))
        Some -999.0
      else
        // Parse the regular score as a float
        Some(float eval)
    else
      let test2 = evalRegexAlt.Match(line)
      if test2.Success then
        let eval = test2.Groups.[1].Value
        if eval.StartsWith("M") then
          // Parse the mate score as an integer
          let mateScore = System.Int32.Parse(eval.TrimStart('M'))
          let maxScore = if mateScore > 0 then 999.0 else -999.0
          Some maxScore
        elif eval.StartsWith("-M") then
          Some -999.0
        else
          // Parse the regular score as a float
          let score = float eval
          if isblack && score <> 0.00 then 
            Some (score * -1.0)
          else
            Some score
      else
        None

  let parsePonderMove line =
    let test = ponderRegex.Match(line)
    if test.Success then
      let ponder = test.Groups.[1].Value
      Some ponder
    else
      None

  let parseRegex myDefault format line (regex : Regex)  =
    let test = regex.Match(line)
    if test.Success then
      test.Groups[1].Value |> format
    else
      myDefault

  let parseWDL line =
    let test = wdlRegex.Match(line)
    if test.Success then
      let w = test.Groups[1].Value
      let d = test.Groups[2].Value
      let l = test.Groups[3].Value
      Some {Win=float w; Draw= float d; Loss= float l}
    else None      

  let parseEvalRegex line =
    let test = evalRegex.Match(line)
    if test.Success then
      if test.Groups[1].Value.Contains("mate") then
        int test.Groups[2].Value |> Mate
      else
        float test.Groups[2].Value |> CP
    else
      NA
 
  let floatParser line regex = parseRegex 0.0 (fun x -> float x) line regex
  let evalParser line = parseEvalRegex line
  let intParser line regex = parseRegex 0 (fun x -> int x) line regex
  let int64Parser line regex = parseRegex 0L (fun x -> int64 x) line regex
  let stringParser line regex = parseRegex "" (fun x -> x.TrimEnd() ) line regex
  let wdlParser line = parseWDL line
 
  let getEssentialData (line:string) isWhite =
    if line.StartsWith "info" then
      let eval = 
        match evalParser line with
          |NA -> NA
          |CP eval -> 
            let eval = if eval = -0.0 then 0.0 else eval
            (if isWhite then eval/100.0 else - eval / 100.0) |> CP
          |Mate m -> (if isWhite then m else -m) |> Mate
      if eval = NA then
        None
      else
        (intParser line depthRegex, 
        eval,
        int64Parser line nodesRegex,
        int64Parser line npsRegex,
        stringParser line pvRegex,
        int64Parser line tbhitsRegex,
        wdlParser line,
        intParser line sDepthRegex,
        intParser line mPvRegex  ) |> Some
    else
      None   

  let move = new Regex("info string\s+(\w+)", RegexOptions.Compiled)
  let nodes = new Regex("N:\s+(\d+)", RegexOptions.Compiled)
  let p = new Regex("P:\s+(-?\d+\.\d+)", RegexOptions.Compiled)
  let q = new Regex("Q:\s+(-?\d+\.\d+)", RegexOptions.Compiled)
  let v = new Regex("V:\s+(-?\d+\.\d+)", RegexOptions.Compiled)
  let e = new Regex("E:\s+(\d+\.\d+)", RegexOptions.Compiled) 
  //info string f8c5  (139 ) N:      19 (+ 0) (P:  0.75%) (WGT:      19.000) (WL: -0.99998) 
  //(D: 0.000) (M: 63.2) (STD: 0.00000) (STDF: 1.00000) (VS: 0.99996) (E: 0.00388) (Q: -0.99998) (U: 0.57593) (S: -0.42405) (V:  -.----) 

  let getInfoStringData player (line:string) =
    {
      Player = player
      LANMove = stringParser line move
      SANMove = String.Empty
      Nodes = int64Parser line nodes
      P = floatParser line p
      Q = floatParser line q
      V = floatParser line v
      E = floatParser line e
      Raw = line
    }


module Chess960 =

  let generateAllChess960Positions () =
    let positions = 
      [ for whiteBishop in 0 .. 2 .. 7 do
          for blackBishop in 1 .. 2 .. 7 do
            let remainingSquares = [ for i in 0 .. 7 do if i <> whiteBishop && i <> blackBishop then yield i ]
            for knights in Seq.choose (fun (a, b) -> if a < b then Some (a, b) else None) (Seq.allPairs remainingSquares remainingSquares) do
              let squaresForRooksKing = List.filter (fun i -> not (List.contains i [fst knights; snd knights])) remainingSquares
              for rook1 in squaresForRooksKing do
                for rook2 in squaresForRooksKing do
                  if rook1 < rook2 then
                    for king in squaresForRooksKing do
                      if rook1 < king && king < rook2 then
                        let mutable position = Array.create 8 ""
                        position.[whiteBishop] <- "B"
                        position.[blackBishop] <- "B"
                        position.[fst knights] <- "N"
                        position.[snd knights] <- "N"
                        position.[rook1] <- "R"
                        position.[rook2] <- "R"
                        position.[king] <- "K"
                        let queenPosition = position |> Array.findIndex (fun x -> x = "")
                        position.[queenPosition] <- "Q"
                        yield String.concat "" position ]
    positions

  // Function to validate the position
  let validatePosition (candidate: string) =
      if candidate.Length <> 8 then
          failwith "Invalid length"
      let validPieces = dict [('R', 2); ('N', 2); ('B', 2); ('Q', 1); ('K', 1)]
      let allChars = candidate.ToCharArray()
      let setOfCandidateChars = Set allChars
      let validChars = Set validPieces.Keys
      if validChars <> setOfCandidateChars then
          failwith "Contains invalid pieces"
      allChars
      |> Array.mapi (fun i e -> i, e = 'B')
      |> Array.filter (fun (_,e) -> e) 
      |> (fun bishops -> if (fst bishops.[0]) % 2 = (fst bishops.[1]) % 2 then failwith "Both bishops on same color")

  // Function to calculate Chess960 PID
  let calcPositionId (startPos: string) =
      validatePosition startPos
      let subsetStep1 = startPos.ToCharArray() |> Array.filter (fun c -> not (c = 'Q' || c = 'B'))
      let knightPositions = subsetStep1 |> Array.mapi (fun i c -> if c = 'N' then Some i else None) |> Array.choose id
      let knightsTable = dict [
        (0, 1), 0
        (0, 2), 1
        (0, 3), 2
        (0, 4), 3
        (1, 2), 4
        (1, 3), 5
        (1, 4), 6
        (2, 3), 7
        (2, 4), 8
        (3, 4), 9 ]

      let N = knightsTable.[(knightPositions.[0], knightPositions.[1])]    
      let subsetStep2 = startPos.ToCharArray() |> Array.filter (fun c -> c <> 'B')
      let Q = Array.findIndex (fun c -> c = 'Q') subsetStep2
      let darkSquares = startPos.ToCharArray() |> Array.mapi (fun i c -> if i % 2 = 0 then Some c else None) |> Array.choose id
      let lightSquares = startPos.ToCharArray() |> Array.mapi (fun i c -> if i % 2 <> 0 then Some c else None) |> Array.choose id
      let D = Array.findIndex (fun c -> c = 'B') darkSquares
      let L = Array.findIndex (fun c -> c = 'B') lightSquares
      4 * (4 * (6 * N + Q) + D) + L


  let chess960ToFen (startingPosition: string) =
    // Mirroring the starting position for black (lowercase)
    let whitePosition = startingPosition.ToLower()    
    // Assuming both kingside and queenside castling are available initially
    let castlingAvailability = "KQkq"    
    // Constructing the FEN string
    sprintf "%s/pppppppp/8/8/8/8/PPPPPPPP/%s w %s - 0 1" whitePosition startingPosition castlingAvailability

  let chess960DoubleToFen (white: string) (black: string) =
    // Mirroring the starting position for black (lowercase)
    let whitePosition = white.ToLower()    
    // Assuming both kingside and queenside castling are available initially
    let castlingAvailability = "KQkq"    
    // Constructing the FEN string
    sprintf "%s/pppppppp/8/8/8/8/PPPPPPPP/%s w %s - 0 1" whitePosition black castlingAvailability

  let getAllChess960PositionsWithPid () = 
    generateAllChess960Positions() 
    |> List.map (fun pos -> pos, calcPositionId pos)
    |> List.sortBy (fun (_, id) -> id)
    |> List.distinct
    |> List.map fst

  let makeDictionaryOfChess960Positions () =
      let positions = getAllChess960PositionsWithPid ()
      let dict = new Dictionary<int, string>()
      let mutable pid = 0
      for pos in positions do
          let fen = chess960ToFen pos
          pid <- pid + 1        
          dict.Add(pid,fen)
      dict
  
  let makeRandomChess960DoublePositions n =
      let positions = getAllChess960PositionsWithPid () |> List.toArray
      let rnd = Random.Shared
      let next () = rnd.Next(0, positions.Length)
      [|
        for _ = 0 to n do
          let wid,bid = next(), next()
          let white = positions[wid]
          let black = positions[bid]
          let fen = chess960DoubleToFen white black
          (wid, bid, fen)
      |] |> Array.distinctBy (fun (_, _, fen) -> fen)
      

  let testPosition pid (pos: string)  = 
    printfn "Position: %s; Chess960 PID= %d" pos pid

  let drawChess960Positions n =      
      let allPos = [|for KeyValue(pid, pos) in makeDictionaryOfChess960Positions() -> (pid, pos) |]
      Random.Shuffle(allPos)
      let n = min n allPos.Length
      allPos |> Seq.truncate n |> Seq.toList      

  let writeChess960PositionsToFile path n =
      let positions = drawChess960Positions n
      let fileName = sprintf "%s/Chess960_%d.epd" path n
      use file = new StreamWriter(fileName)
      for (pid, pos) in positions do
          file.WriteLine(sprintf "%s ; %s;" pos (sprintf "id \"Chess960 - Position nr: %d\"" pid))
      printfn "File %s written to path" fileName

  let writeChess960DoublePositionsToFile path n =
      let positions = makeRandomChess960DoublePositions n
      let fileName = sprintf "%s/Chess960Double_%d.epd" path n
      use file = new StreamWriter(fileName)
      for (wid, bid, pos) in positions do
          file.WriteLine(sprintf "%s ; %s;" pos (sprintf "id \"DFRC - Position nr: %d vs %d\"" wid bid))
      printfn "File %s written to path" fileName


module Glicko2 =
    open Puzzle
    open MathNet.Numerics

    let π = Math.PI
    let c = 0.2 // constant value; you can adjust this depending on your requirements

    let toGlickoScale (player: PlayerRecord) =
        let μ = (player.Rating - 1500.0) / 173.7178
        let φ = player.Deviation / 173.7178
        μ, φ, player.Volatility

    let fromGlickoScale (μ: float, φ: float, σ: float) =
        let rating = 173.7178 * μ + 1500.0
        let deviation = 173.7178 * φ
        { Rating = rating; Deviation = deviation; Volatility = σ }

    let fromPuzzleDataToPlayerRecord (player: CsvPuzzleData) (score:float) =
      { Rating = player.Rating; Deviation = player.RatingDeviation; Volatility = 0.06 }, score

    let g (φ: float) =
        1.0 / Math.Sqrt(1.0 + 3.0 * φ * φ / (π * π))

    let E (μ: float, μj: float, φj: float) =
        1.0 / (1.0 + Math.Exp(-g(φj) * (μ - μj)))

    let update player opponentsResults =
        let μ, φ, σ = toGlickoScale player

        let varianceInv =
            Seq.fold
                (fun acc (opponent, score) ->
                    let μj, φj, _ = toGlickoScale opponent
                    acc + g(φj) * g(φj) * E(μ, μj, φj) * (1.0 - E(μ, μj, φj)))
                0.0 opponentsResults

        let variance = 1.0 / varianceInv

        let delta =
            variance *
            Seq.fold
                (fun acc (opponent, score) ->
                    let μj, φj, _ = toGlickoScale opponent
                    acc + g(φj) * (score - E(μ, μj, φj)))
                0.0 opponentsResults

        let a = Math.Log(σ * σ)
        let b = a + 10.0  // Setting b to be sufficiently far from a.
        
        let f x =
            (exp(x) * (delta * delta - φ * φ - variance - exp(x)))
            / (2.0 * (φ * φ + variance + exp(x))) * (φ * φ + variance + exp(x))
            - (x - a) / (c * c)        

        let computeVolatility f a b = FindRoots.brent 100 1e-5 a b f
        
        let newVolatility =
          match computeVolatility f a b with 
          |Some v -> 
            let nV = exp(v / 2.0)
            nV
          |None -> 0.02              

        let newφStar = Math.Sqrt(φ * φ + newVolatility * newVolatility)
        let newφ = 1.0 / Math.Sqrt(1.0 / (newφStar * newφStar) + 1.0 / variance)
        let newμ = μ + newφ * newφ *
                    Seq.fold
                        (fun acc (opponent, score) ->
                            let μj, φj, _ = toGlickoScale opponent
                            acc + g(φj) * (score - E(μ, μj, φj)))
                        0.0 opponentsResults

        fromGlickoScale (newμ, newφ, newVolatility)


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


module UCI =

  let stockfishUCIOutput = [
    "id name Stockfish 15.1"
    "id author the Stockfish developers (see AUTHORS file)"
    "option name Debug Log File type string default"
    "option name Threads type spin default 1 min 1 max 1024"
    "option name Hash type spin default 16 min 1 max 33554432"
    "option name Clear Hash type button"
    "option name Ponder type check default false"
    "option name MultiPV type spin default 1 min 1 max 500"
    "option name Skill Level type spin default 20 min 0 max 20"
    "option name Move Overhead type spin default 10 min 0 max 5000"
    "option name Slow Mover type spin default 100 min 10 max 1000"
    "option name nodestime type spin default 0 min 0 max 10000"
    "option name UCI_Chess960 type check default false"
    "option name UCI_AnalyseMode type check default false"
    "option name UCI_LimitStrength type check default false"
    "option name UCI_Elo type spin default 1350 min 1350 max 2850"
    "option name UCI_ShowWDL type check default false"
    "option name SyzygyPath type string default <empty>"
    "option name SyzygyProbeDepth type spin default 1 min 1 max 100"
    "option name Syzygy50MoveRule type check default true"
    "option name SyzygyProbeLimit type spin default 7 min 0 max 7"
    "option name Use NNUE type check default true"
    "option name EvalFile type string default nn-ad9b42354671.nnue"
    "uciok"
    ]

  let ceresOutput = [
    "id name Ceres 0.97RC3"
    "id author David Elliott and the Ceres Authors"
    "option name WeightsFile type string default <from DefaultNetworkSpecString in Ceres.json>"
    "option name LogFile type string default"
    "option name SearchLogFile type string default"
    "option name MultiPV type spin default 1 min 1 max 500"
    "option name VerboseMoveStats type check default false"
    "option name LogLiveStats type check default false"
    "option name SmartPruningFactor type string default 1.33"
    "option name MoveOverheadMs type spin default 250 min 0 max 100000000"
    "option name PerPVCounters type check default false"
    "option name ScoreType type combo default centipawn var centipawn var Q var W-L"
    "option name UCI_ShowWDL type check default false"
    "option name SyzygyPath type string default"
    "option name CPUCT type string default 1.745"
    "option name CPUCTAtRoot type string default 1.745"
    "option name CPuctBase type string default 38739"
    "option name CPuctBaseAtRoot type string default 38739"
    "option name CPuctFactor type string default 3.894"
    "option name CPuctFactorAtRoot type string default 3.894"
    "option name PolicyTemperature type string default 1.359"
    "option name FPU type string default 0.33"
    "option name FPUAtRoot type string default 1"
    "option name SearchLimitMultiplier type string default 1.00"
    "option name MaxTreeVisits type string default"
    "option name MaxTreeNodes type string default"
    "option name ReducedMemoryMode type check default false"
    "option name EnableSiblingEval type check default false"
    "option name EnableUncertaintyBoosting type check default false"
    "uciok"   ]


  // Regular expression pattern to capture the option name and its default value    
  let optionRegex = new Regex(@"option name (.*?) type.*?default (\S+)?", RegexOptions.Compiled)
  
  let extractOptionDefaults (uciOutputs: ResizeArray<string>) =
    let dict = new Dictionary<string, string>()
    
    uciOutputs //|> Seq.toList
    |> Seq.filter (fun s -> s.StartsWith("option"))
    |> Seq.iter (fun s -> 
        let ismatch = optionRegex.Match(s)
        if ismatch.Success then
            let optionName = ismatch.Groups.[1].Value              
            if ismatch.Groups.[2].Success then 
                let value = ismatch.Groups.[2].Value  // This should be the second group.
                if not (String.IsNullOrWhiteSpace(value)) then
                    dict.Add(optionName, value)
        else ()
    )
    dict
  

  let createDefaultSetOptionCommandForName (dict: Dictionary<string, string>) (name: string) =
    let matchedKey = 
        dict.Keys 
        |> Seq.tryFind (fun key -> key.ToLower().Contains(name.ToLower()))
    
    match matchedKey with
    | Some key -> 
        match dict.TryGetValue(key) with
        | (true, value) when not (String.IsNullOrWhiteSpace(value)) -> Some (sprintf "setoption name %s value %s" key value)
        | _ -> None
    | None -> None
  
  let getUCIOptionsAsync exePath = async {
    // Create a new process start info
    let psi = ProcessStartInfo()
    psi.FileName <- exePath
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    // Create the process and start it
    let proc = new Process()
    proc.StartInfo <- psi
    proc.Start() |> ignore

    // Send the 'uci' command
    let sw = proc.StandardInput
    sw.WriteLine("uci\n")

    // Read the output
    let output = ResizeArray<string>()
    let sr = proc.StandardOutput

    let rec readLinesAsync () = async {
        let! line = sr.ReadLineAsync() |> Async.AwaitTask
        if line <> "uciok" then
            if not (String.IsNullOrEmpty(line)) then
              output.Add(line)
            return! readLinesAsync ()
    }
    do! readLinesAsync()

    // Close the process
    proc.Kill()
    proc.Close()
    return output      
    }


module PuzzleTheme =

  let puzzleThemes = """<?xml version="1.0" encoding="UTF-8"?>
  <resources>
    <string name="advancedPawn">Advanced pawn</string>
    <string name="advancedPawnDescription">One of your pawns is deep into the opponent position, maybe threatening to promote.</string>
    <string name="advantage">Advantage</string>
    <string name="advantageDescription">Seize your chance to get a decisive advantage. (200cp ≤ eval ≤ 600cp)</string>
    <string name="anastasiaMate">Anastasia's mate</string>
    <string name="anastasiaMateDescription">A knight and rook or queen team up to trap the opposing king between the side of the board and a friendly piece.</string>
    <string name="arabianMate">Arabian mate</string>
    <string name="arabianMateDescription">A knight and a rook team up to trap the opposing king on a corner of the board.</string>
    <string name="attackingF2F7">Attacking f2 or f7</string>
    <string name="attackingF2F7Description">An attack focusing on the f2 or f7 pawn, such as in the fried liver opening.</string>
    <string name="attraction">Attraction</string>
    <string name="attractionDescription">An exchange or sacrifice encouraging or forcing an opponent piece to a square that allows a follow-up tactic.</string>
    <string name="backRankMate">Back rank mate</string>
    <string name="backRankMateDescription">Checkmate the king on the home rank, when it is trapped there by its own pieces.</string>
    <string name="bishopEndgame">Bishop endgame</string>
    <string name="bishopEndgameDescription">An endgame with only bishops and pawns.</string>
    <string name="bodenMate">Boden's mate</string>
    <string name="bodenMateDescription">Two attacking bishops on criss-crossing diagonals deliver mate to a king obstructed by friendly pieces.</string>
    <string name="castling">Castling</string>
    <string name="castlingDescription">Bring the king to safety, and deploy the rook for attack.</string>
    <string name="capturingDefender">Capture the defender</string>
    <string name="capturingDefenderDescription">Removing a piece that is critical to defence of another piece, allowing the now undefended piece to be captured on a following move.</string>
    <string name="crushing">Crushing</string>
    <string name="crushingDescription">Spot the opponent blunder to obtain a crushing advantage. (eval ≥ 600cp)</string>
    <string name="doubleBishopMate">Double bishop mate</string>
    <string name="doubleBishopMateDescription">Two attacking bishops on adjacent diagonals deliver mate to a king obstructed by friendly pieces.</string>
    <string name="dovetailMate">Dovetail mate</string>
    <string name="dovetailMateDescription">A queen delivers mate to an adjacent king, whose only two escape squares are obstructed by friendly pieces.</string>
    <string name="equality">Equality</string>
    <string name="equalityDescription">Come back from a losing position, and secure a draw or a balanced position. (eval ≤ 200cp)</string>
    <string name="kingsideAttack">Kingside attack</string>
    <string name="kingsideAttackDescription">An attack of the opponent's king, after they castled on the king side.</string>
    <string name="clearance">Clearance</string>
    <string name="clearanceDescription">A move, often with tempo, that clears a square, file or diagonal for a follow-up tactical idea.</string>
    <string name="defensiveMove">Defensive move</string>
    <string name="defensiveMoveDescription">A precise move or sequence of moves that is needed to avoid losing material or another advantage.</string>
    <string name="deflection">Deflection</string>
    <string name="deflectionDescription">A move that distracts an opponent piece from another duty that it performs, such as guarding a key square. Sometimes also called "overloading".</string>
    <string name="discoveredAttack">Discovered attack</string>
    <string name="discoveredAttackDescription">Moving a piece (such as a knight), that previously blocked an attack by a long range piece (such as a rook), out of the way of that piece.</string>
    <string name="doubleCheck">Double check</string>
    <string name="doubleCheckDescription">Checking with two pieces at once, as a result of a discovered attack where both the moving piece and the unveiled piece attack the opponent's king.</string>
    <string name="endgame">Endgame</string>
    <string name="endgameDescription">A tactic during the last phase of the game.</string>
    <string name="enPassantDescription">A tactic involving the en passant rule, where a pawn can capture an opponent pawn that has bypassed it using its initial two-square move.</string>
    <string name="exposedKing">Exposed king</string>
    <string name="exposedKingDescription">A tactic involving a king with few defenders around it, often leading to checkmate.</string>
    <string name="fork">Fork</string>
    <string name="forkDescription">A move where the moved piece attacks two opponent pieces at once.</string>
    <string name="hangingPiece">Hanging piece</string>
    <string name="hangingPieceDescription">A tactic involving an opponent piece being undefended or insufficiently defended and free to capture.</string>
    <string name="hookMate">Hook mate</string>
    <string name="hookMateDescription">Checkmate with a rook, knight, and pawn along with one enemy pawn to limit the enemy king's escape.</string>
    <string name="interference">Interference</string>
    <string name="interferenceDescription">Moving a piece between two opponent pieces to leave one or both opponent pieces undefended, such as a knight on a defended square between two rooks.</string>
    <string name="intermezzo">Intermezzo</string>
    <string name="intermezzoDescription">Instead of playing the expected move, first interpose another move posing an immediate threat that the opponent must answer. Also known as "Zwischenzug" or "In between".</string>
    <string name="knightEndgame">Knight endgame</string>
    <string name="knightEndgameDescription">An endgame with only knights and pawns.</string>
    <string name="long">Long puzzle</string>
    <string name="longDescription">Three moves to win.</string>
    <string name="master">Master games</string>
    <string name="masterDescription">Puzzles from games played by titled players.</string>
    <string name="masterVsMaster">Master vs Master games</string>
    <string name="masterVsMasterDescription">Puzzles from games between two titled players.</string>
    <string name="mate">Checkmate</string>
    <string name="mateDescription">Win the game with style.</string>
    <string name="mateIn1">Mate in 1</string>
    <string name="mateIn1Description">Deliver checkmate in one move.</string>
    <string name="mateIn2">Mate in 2</string>
    <string name="mateIn2Description">Deliver checkmate in two moves.</string>
    <string name="mateIn3">Mate in 3</string>
    <string name="mateIn3Description">Deliver checkmate in three moves.</string>
    <string name="mateIn4">Mate in 4</string>
    <string name="mateIn4Description">Deliver checkmate in four moves.</string>
    <string name="mateIn5">Mate in 5 or more</string>
    <string name="mateIn5Description">Figure out a long mating sequence.</string>
    <string name="middlegame">Middlegame</string>
    <string name="middlegameDescription">A tactic during the second phase of the game.</string>
    <string name="oneMove">One-move puzzle</string>
    <string name="oneMoveDescription">A puzzle that is only one move long.</string>
    <string name="opening">Opening</string>
    <string name="openingDescription">A tactic during the first phase of the game.</string>
    <string name="pawnEndgame">Pawn endgame</string>
    <string name="pawnEndgameDescription">An endgame with only pawns.</string>
    <string name="pin">Pin</string>
    <string name="pinDescription">A tactic involving pins, where a piece is unable to move without revealing an attack on a higher value piece.</string>
    <string name="promotion">Promotion</string>
    <string name="promotionDescription">Promote one of your pawn to a queen or minor piece.</string>
    <string name="queenEndgame">Queen endgame</string>
    <string name="queenEndgameDescription">An endgame with only queens and pawns.</string>
    <string name="queenRookEndgame">Queen and Rook</string>
    <string name="queenRookEndgameDescription">An endgame with only queens, rooks and pawns.</string>
    <string name="queensideAttack">Queenside attack</string>
    <string name="queensideAttackDescription">An attack of the opponent's king, after they castled on the queen side.</string>
    <string name="quietMove">Quiet move</string>
    <string name="quietMoveDescription">A move that does neither make a check or capture, nor an immediate threat to capture, but does prepare a more hidden unavoidable threat for a later move.</string>
    <string name="rookEndgame">Rook endgame</string>
    <string name="rookEndgameDescription">An endgame with only rooks and pawns.</string>
    <string name="sacrifice">Sacrifice</string>
    <string name="sacrificeDescription">A tactic involving giving up material in the short-term, to gain an advantage again after a forced sequence of moves.</string>
    <string name="short">Short puzzle</string>
    <string name="shortDescription">Two moves to win.</string>
    <string name="skewer">Skewer</string>
    <string name="skewerDescription">A motif involving a high value piece being attacked, moving out the way, and allowing a lower value piece behind it to be captured or attacked, the inverse of a pin.</string>
    <string name="smotheredMate">Smothered mate</string>
    <string name="smotheredMateDescription">A checkmate delivered by a knight in which the mated king is unable to move because it is surrounded (or smothered) by its own pieces.</string>
    <string name="superGM">Super GM games</string>
    <string name="superGMDescription">Puzzles from games played by the best players in the world.</string>
    <string name="trappedPiece">Trapped piece</string>
    <string name="trappedPieceDescription">A piece is unable to escape capture as it has limited moves.</string>
    <string name="underPromotion">Underpromotion</string>
    <string name="underPromotionDescription">Promotion to a knight, bishop, or rook.</string>
    <string name="veryLong">Very long puzzle</string>
    <string name="veryLongDescription">Four moves or more to win.</string>
    <string name="xRayAttack">X-Ray attack</string>
    <string name="xRayAttackDescription">A piece attacks or defends a square, through an enemy piece.</string>
    <string name="zugzwang">Zugzwang</string>
    <string name="zugzwangDescription">The opponent is limited in the moves they can make, and all moves worsen their position.</string>
    <string name="healthyMix">Healthy mix</string>
    <string name="healthyMixDescription">A bit of everything. You don't know what to expect, so you remain ready for anything! Just like in real games.</string>
    <string name="playerGames">Player games</string>
    <string name="playerGamesDescription">Lookup puzzles generated from your games, or from another player's games.</string>
    <string name="puzzleDownloadInformation">These puzzles are in the public domain, and can be downloaded from %s.</string>
  </resources>
  """

  let parseXml (xmlString: string) =
    let doc = XDocument.Parse(xmlString)
    let resources = doc.Root
    resources.Elements()
    |> Seq.pairwise // Pair each element with its next sibling
    |> Seq.filter (fun (el1, el2) ->
        el2.Attribute(XName.Get("name")).Value = el1.Attribute(XName.Get("name")).Value + "Description")
    |> Seq.map (fun (el1, el2) ->
        let theme = el1.Attribute(XName.Get("name")).Value //el1.Value
        let description = el2.Value
        (theme, description))
    |> Seq.toList

  let puzzleThemeDictionary() = 
    let puzzlesDict = Dictionary<string, string>()
    parseXml puzzleThemes
    |> List.iter (fun (theme, description) -> puzzlesDict.Add(theme, description))
    puzzlesDict  
  
  let printPuzzleThemes () =
    let puzzleData = puzzleThemeDictionary()
    for KeyValue(theme, _) in puzzleData do
      printfn "Theme: %s" theme

  let getThemeDescription (theme: string) =
    let puzzleData = puzzleThemeDictionary()
    match puzzleData.TryGetValue(theme) with
    | (true, description) -> description
    | _ -> "Theme not found"

  let getAllThemes () =
    let puzzleData = puzzleThemeDictionary()
    puzzleData.Keys |> Seq.toList

  let themesAsString () =
    let puzzleData = puzzleThemeDictionary()
    let sb = new StringBuilder()
    for KeyValue(theme, _) in puzzleData do
      sb.Append(theme).Append(", ") |> ignore
    //skip the last comma
    sb.ToString().TrimEnd([|','; ' '|])

