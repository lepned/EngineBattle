/// Configuration loading, validation, and console display utilities
module ChessLibrary.Configuration

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Text
open System.Collections.Generic

open TypesDef.CoreTypes
open TypesDef.Tournament
open RuntimeUtilities
open GameAnalysis

module ConsoleHelper =

  let displayTournament (tournament: Tournament) =
    let timeFormat (time:TimeOnly) = time.ToString("HH:mm:ss.fff")
    printfn "Name: %s" tournament.Name
    printfn "Description: %s" tournament.Description
    printfn "OS: %s" tournament.OS
    printfn "CPU: %s" tournament.CPU
    printfn "RAM: %s" tournament.RAM
    printfn "GPU: %s" tournament.GPU
    printfn "Tournament mode: %s" tournament.TournamentMode
    printfn "DoNotDeviate: %b" tournament.PreventMoveDeviation
    printfn "Challengers: %d" tournament.Challengers
    printfn "Rounds: %d" tournament.Rounds
    printfn $"Delay between games: {tournament.DelayBetweenGames}"
    printfn $"Move overhead: {timeFormat tournament.MoveOverhead}"
    printfn "Time control: %A" tournament.TimeControl
    printfn "PgnOut Path: %s" tournament.PgnOutPath
    printfn "Reference PGN path: %s" tournament.ReferencePGNPath
    match tournament.Opening.OpeningsPath with
    | Some path -> printfn "Opening path: %s" path
    | None -> printfn "Opening path: None"
    printfn $"{nameof(tournament.Opening.OpeningsTwice)}: {tournament.Opening.OpeningsTwice}"
    printfn $"{nameof(tournament.Opening.OpeningsPly)}: {tournament.Opening.OpeningsPly}"
    printfn $"Win adjudication: {tournament.AdjudicationText()}"
    printfn $"Policy head test: {tournament.TestOptions.PolicyTest}"
    printfn $"Value head test: {tournament.TestOptions.ValueTest}"
    printfn $"Number of games in parallel: {tournament.TestOptions.NumberOfGamesInParallelConsoleOnly}"
    if tournament.TestOptions.GPUs <> null && tournament.TestOptions.GPUs.Length > 0 then
      let gpuList = String.Join(", ", tournament.TestOptions.GPUs)
      printfn $"GPUs: {gpuList}"

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

  let writeSwissPairingsPerRoundFromFile (filePath: string) =
    let loadState () =
      try
        if String.IsNullOrWhiteSpace filePath then
          ConsoleUtils.redConsole "Swiss state file path is empty."
          None
        elif not (File.Exists filePath) then
          ConsoleUtils.redConsole (sprintf "Swiss state file not found: %s" filePath)
          None
        else
          let json = File.ReadAllText filePath
          if String.IsNullOrWhiteSpace json then
            ConsoleUtils.redConsole (sprintf "Swiss state file is empty: %s" filePath)
            None
          else
            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            let state = JsonSerializer.Deserialize<SwissTypes.SwissState>(json, options)
            if obj.ReferenceEquals(state, null) then
              ConsoleUtils.redConsole (sprintf "Failed to parse swiss state: %s" filePath)
              None
            else
              Some state
      with ex ->
        ConsoleUtils.redConsole (sprintf "Failed to read swiss state: %s" ex.Message)
        None

    match loadState () with
    | None -> ()
    | Some state ->
        printfn "Swiss pairings for \"%s\"" state.TournamentName
        let rounds =
          if isNull state.Rounds then Seq.empty
          else state.Rounds |> Seq.sortBy (fun r -> r.RoundNumber)
        if Seq.isEmpty rounds then
          printfn "No rounds in swiss state."
        else
          for round in rounds do
            printfn ""
            printfn "Round %d" round.RoundNumber
            if isNull round.Pairings || round.Pairings.Count = 0 then
              printfn "  (no pairings)"
            else
              let maxName =
                round.Pairings
                |> Seq.collect (fun p -> [ p.PlayerA; p.PlayerB ])
                |> Seq.choose (fun s -> if String.IsNullOrWhiteSpace s then None else Some s.Length)
                |> Seq.append [0]
                |> Seq.max
              for pairing in round.Pairings do
                let playerA = if String.IsNullOrWhiteSpace pairing.PlayerA then "" else pairing.PlayerA
                let playerB = if String.IsNullOrWhiteSpace pairing.PlayerB then "" else pairing.PlayerB
                let scoreA = pairing.ScoreA.ToString("0.##")
                let scoreB = pairing.ScoreB.ToString("0.##")
                printfn "  #%d: %-*s vs %-*s   %s-%s" pairing.PairId maxName playerA maxName playerB scoreA scoreB

          let addOpponent (pairsByPlayer: Dictionary<string, ResizeArray<string>>) (player: string) (opponent: string) =
            if not (String.IsNullOrWhiteSpace player) then
              let opp =
                if String.IsNullOrWhiteSpace opponent then "BYE"
                else opponent
              match pairsByPlayer.TryGetValue(player) with
              | true, lst -> lst.Add(opp)
              | false, _ ->
                  let lst = ResizeArray<string>()
                  lst.Add(opp)
                  pairsByPlayer.Add(player, lst)

          let pairsByPlayer = Dictionary<string, ResizeArray<string>>(StringComparer.OrdinalIgnoreCase)
          for round in rounds do
            if not (isNull round.Pairings) then
              for pairing in round.Pairings do
                addOpponent pairsByPlayer pairing.PlayerA pairing.PlayerB
                addOpponent pairsByPlayer pairing.PlayerB pairing.PlayerA

          if pairsByPlayer.Count > 0 then
            printfn ""
            printfn "Pairings by player"
            for KeyValue(player, opponents) in pairsByPlayer |> Seq.sortBy (fun kv -> kv.Key) do
              printfn "%s:" player
              for opp in opponents do
                printfn "  %s" opp

  let writeEngineStatsPerGame (engineStat: EngineStatsPerGame) n includeEps =
      let speed nps = Formatting.formatNPS nps
      let eps = Formatting.formatEPS engineStat.AvgEPS
      let line =
        if includeEps then
            sprintf "%-*s : %-5d %-10s %-10s %-10s %-10s %-8.0f %-8.0f %-8.0f %-8.0f %-9s"
                      n engineStat.Player engineStat.GameNr (speed engineStat.AvgNodes) (speed engineStat.MedianNodes) (speed engineStat.AvgNps)
                      (speed engineStat.MedianNps) engineStat.AvgDepth engineStat.MedianDepth engineStat.AvgSD engineStat.MedianSD eps
        else
            sprintf "%-*s : %-5d %-10s %-10s %-10s %-10s %-8.0f %-8.0f %-8.0f %-8.0f"
                    n engineStat.Player engineStat.GameNr (speed engineStat.AvgNodes) (speed engineStat.MedianNodes) (speed engineStat.AvgNps)
                    (speed engineStat.MedianNps) engineStat.AvgDepth engineStat.MedianDepth engineStat.AvgSD engineStat.MedianSD
      line

  let writeSummaryEngineStats (stat: SummaryEngineStat) n includeEps =
      let speed nps = Formatting.formatNPS nps
      let eps = Formatting.formatEPS stat.EPS
      let time = Formatting.formatMoveTime stat.Time
      let line =
          if includeEps then
            sprintf "%-*s : %-7d %-11s %-11s %-11s %-8.0f %-7.0f %-7s"
                        n stat.Player stat.Games (speed stat.AvgNodes) (speed stat.AvgNPS) eps stat.AvgDepth stat.AvgSelfDepth time
          else
            sprintf "%-*s : %-7d %-11s %-11s %-8.0f %-7.0f %-7s"
                        n stat.Player stat.Games (speed stat.AvgNodes) (speed stat.AvgNPS) stat.AvgDepth stat.AvgSelfDepth time
      line

  let writeEngineStatHeader (n:int) includeEps : string =
      if includeEps then
        sprintf "%-*s : %-5s %-10s %-10s %-10s %-10s %-8s %-8s %-8s %-8s %-9s" n "# PLAYER" "Game#" "AvgNodes" "MedNodes" "AvgNPS" "MedNPS" "AvgDepth" "MedDepth" "AvgSD" "MedSD" "AvgEPS"
      else
        sprintf "%-*s : %-5s %-10s %-10s %-10s %-10s %-8s %-8s %-8s %-8s" n "# PLAYER" "Game#" "AvgNodes" "MedNodes" "AvgNPS" "MedNPS" "AvgDepth" "MedDepth" "AvgSD" "MedSD"

  let writeSummaryEngineStatHeader (n:int) includeEps : string =
      if includeEps then
        sprintf "%-*s : %-7s %-11s %-11s %-11s %-8s %-7s %-7s" n "# PLAYER" "Games" "Nodes" "NPS" "EPS" "Depth" "SD" "Time"
      else
        sprintf "%-*s : %-7s %-11s %-11s %-8s %-7s %-7s" n "# PLAYER" "Games" "Nodes" "NPS" "Depth" "SD" "Time"

  let writeEngineStatsToConsole (engineStats: EngineStatsPerGame seq) =
      let sb = System.Text.StringBuilder()
      let appendLine (txt:string) = sb.AppendLine txt |> ignore
      sb.Clear() |> ignore
      appendLine "\n```\n"
      let longestName = engineStats |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
      let includeEps = engineStats |> Seq.exists (fun e -> e.AvgEPS > 0.0)
      writeEngineStatHeader longestName includeEps |> appendLine
      for engineStat in engineStats do
        writeEngineStatsPerGame engineStat longestName includeEps |> appendLine
      appendLine "\n```"
      let res = sb.ToString()
      res

  let writeSummaryEngineStatsToConsole (engineStats: SummaryEngineStat seq) =
      let sb = System.Text.StringBuilder()
      let appendLine (txt:string) = sb.AppendLine txt |> ignore
      sb.Clear() |> ignore
      appendLine "\n```\n"
      let includeEps = engineStats |> Seq.exists (fun e -> e.EPS > 0.0)
      let longestName = engineStats |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
      writeSummaryEngineStatHeader longestName includeEps |> appendLine
      for engineStat in engineStats do
        writeSummaryEngineStats engineStat longestName includeEps |> appendLine
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

    let foundProperties = System.Collections.Generic.HashSet<string>()
    let errors = System.Collections.Generic.List<string>()

    try
        while reader.Read() do
            match reader.TokenType with
            | JsonTokenType.PropertyName ->
                let propertyName = reader.GetString()
                if not (String.IsNullOrEmpty(propertyName)) then
                    foundProperties.Add(propertyName) |> ignore
                    if propertyName = "TimeControlID" && not (reader.Read() && reader.TokenType = JsonTokenType.Number) then
                        errors.Add("TimeControlID must be a number.")
            | _ -> ()

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
        Valid
    | Invalid errMsgs ->
        errMsgs |> List.iter (fun m -> printfn $"Invalid EngineConfig JSON property: {m} in engine {name}")
        Invalid errMsgs

  let checkFolderExists (path: string) = Directory.Exists(path)
  let checkPathExists (path: string) = File.Exists(path)

  let checkIfNeuralNetExists (networkPath: string) (path: string) =
    File.Exists(path) || File.Exists(Path.Combine(networkPath, path))

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
      | None -> Ok
      | Some nn ->
          let weightsPath = nn.Value |> string
          if checkPathExists weightsPath || checkIfNeuralNetExists config.NetworkPath weightsPath then Ok
          else
              let combinedPath = Path.Combine(config.NetworkPath, weightsPath)
              Errors [sprintf "Neural net in combined path %s or in direct path %s for %s does not exist" combinedPath weightsPath config.Name]

  let validatePonderingAllowed (config: EngineConfig) (tourny: Tournament) =
      if tourny.AllowPondering then
          match config.Options |> Seq.tryFind (fun kvp -> kvp.Key.Contains "Ponder") with
          | None -> Errors ["Ponder option not found in engine options for " + config.Name]
          | Some nn ->
              let ponderValue = nn.Value |> string
              match ponderValue.ToLower() with
              | "true" -> Ok
              | _ -> Errors [sprintf "Ponder option in %s must be set to true, but found: %s" config.Name ponderValue]
      else Ok

  let validateChessEngineCmds (config: EngineConfig) =
      [
        validatePath config
        validateSyzygyFolder config
        validateWeightsFile config
      ] |> accumulateErrors

  let validateContempt (engines: EngineConfig seq) =
      let anyEngineWithContempt =
        engines |> Seq.exists(fun e -> e.ContemptEnabled)
      if anyEngineWithContempt then
          let enginesWithoutRating = engines |> Seq.exists(fun e -> e.Rating = 0)
          if enginesWithoutRating then
            Errors ["At least one engine has ContemptEnabled set to true, but one or more engines do not have a rating set in their EngineConfig. Please ensure that all engines have a valid rating when using contempt settings."]
          else
            match engines |> Seq.tryFind(fun e -> e.ContemptEnabled && e.Name.ToLower().Contains("lc0")) with
            | None -> Ok
            | Some leelaContempt ->
                let normalizeKey (key: string) =
                    key.Replace(" ", "").ToLower()
                let wdlCalibOpt = leelaContempt.Options |> Seq.tryFind (fun kvp -> normalizeKey kvp.Key = "wdlcalibrationelo")
                let wdlEvalObjOpt = leelaContempt.Options |> Seq.tryFind (fun kvp -> normalizeKey kvp.Key = "wdlevalobjectivity")
                let wdlDrawRateOpt = leelaContempt.Options |> Seq.tryFind (fun kvp -> normalizeKey kvp.Key = "wdldrawratereference")
                match wdlCalibOpt, wdlEvalObjOpt, wdlDrawRateOpt with
                | Some _, Some _, Some _ -> Ok
                | _ -> Errors [sprintf "Engine %s has ContemptEnabled set to true, but is missing one or more required options for LC0: WDLCalibrationElo, WDLEvalObjectivity, WDLDrawRateReference. Please add these options to the engine configuration." leelaContempt.Name]
       else Ok

  let validateUniqueNames (engines: EngineConfig seq) =
      let names = engines |> Seq.map (fun e -> e.Name)
      let uniqueNames = names |> Seq.distinct |> Seq.toArray
      if uniqueNames.Length <> Seq.length names then
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
      let engineValidations = engines |> List.map validateChessEngineCmds
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
              Ok
      | Some p ->
            if String.IsNullOrWhiteSpace p then
                Ok
            else
                Errors ["OpeningsPath is not set in tournament.json"]
      | None -> Ok

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

  let validateTournamentModeOptions (tourny: Tournament) =
      let mode =
          if String.IsNullOrWhiteSpace tourny.TournamentMode then "RR"
          else tourny.TournamentMode
      let normalized = mode.Trim().ToLowerInvariant()
      let isKnown =
          normalized = "rr"
          || normalized = "roundrobin"
          || normalized = "round-robin"
          || normalized = "gauntlet"
          || normalized = "cup"
          || normalized = "swiss"
          || normalized = "ladder"
      if not isKnown then
          Errors ["TournamentMode must be one of: RR, Gauntlet, Cup, Swiss, Ladder."]
      elif normalized = "cup" && obj.ReferenceEquals(tourny.CupOptions, null) then
          Errors ["CupOptions must be set when TournamentMode is Cup."]
      elif normalized = "swiss" && obj.ReferenceEquals(tourny.SwissOptions, null) then
          Errors ["SwissOptions must be set when TournamentMode is Swiss."]
      elif normalized = "ladder" && obj.ReferenceEquals(tourny.LadderOptions, null) then
          Errors ["LadderOptions must be set when TournamentMode is Ladder."]
      else
          Ok

  let validateTournament (tourny: Tournament) =
      [
          validateEnginesPresent tourny.EngineSetup.Engines tourny.Challengers
          validateTournamentModeOptions tourny
          validateOpeningPath tourny
          validatePgnOutPath tourny
          validateEngineConfigs tourny.EngineSetup.Engines
          validateEngineTimeControls tourny tourny.EngineSetup.Engines
          validateDelayBetweenGames tourny tourny.EngineSetup.Engines
          validateUniqueNames tourny.EngineSetup.Engines
          validateContempt tourny.EngineSetup.Engines
          validateEngineNames tourny.EngineSetup.Engines
          for config in tourny.EngineSetup.Engines do
              validatePonderingAllowed config tourny
              validateChessEngineCmds config
      ] |> accumulateErrors

  let validateTournamentInput (tourny: Tournament) =
      match validateTournament tourny with
      | Ok -> ConsoleUtils.printInColor ConsoleColor.Green "Tournament passed limited validation of key settings"
      | Errors msgs ->
          for msg in msgs do
              ConsoleUtils.printInColor ConsoleColor.Red msg
      // Non-blocking warnings
      let mode = tourny.ModeLabel().Trim().ToLowerInvariant()
      let isRRorGauntlet = mode = "rr" || mode = "roundrobin" || mode = "round-robin" || mode = "gauntlet"
      if isRRorGauntlet && tourny.Opening.RandomOpenings && not tourny.Opening.OpeningsTwice then
          ConsoleUtils.printInColor ConsoleColor.Yellow
              "Warning: RandomOpenings is enabled but OpeningsTwice is false. Each engine pair will play each opening from only one color side. Set OpeningsTwice to true for balanced color distribution."

module JSON =

  let private createJsonOptions() =
      let options = JsonSerializerOptions(AllowTrailingCommas = true)
      options.Converters.Add(TypesDef.CoreTypes.TimeControlStrategyConverter())
      options

  let readEngineConfig path =
      let json = File.ReadAllText(path)
      JsonSerializer.Deserialize<EngineConfig[]>(json, createJsonOptions())

  let readSingleEngineConfig path =
    try
        let json = File.ReadAllText(path)
        let fileName = Path.GetFileName(path)
        match Validation.readAndValidateEngineConfigJson json fileName with
        | Validation.Valid -> JsonSerializer.Deserialize<EngineConfig>(json, createJsonOptions())
        | Validation.Invalid msgs ->
          msgs |> List.iter (fun m -> ConsoleUtils.printInColor ConsoleColor.Red m)
          failwith "Invalid engine definition"
    with
    | ex ->
      printfn "Error in reading engine config from file: %s" path
      sprintf "Complete error message: %s" ex.Message |> failwith

  let readEngineDef folder fileName =
    try
      let path = Path.Combine(folder,fileName)
      let json = File.ReadAllText(path)
      match Validation.readAndValidateEngineConfigJson json fileName with
      | Validation.Valid -> JsonSerializer.Deserialize<EngineConfig>(json, createJsonOptions())
      | Validation.Invalid msgs ->
          msgs |> List.iter (fun m -> ConsoleUtils.printInColor ConsoleColor.Red m)
          failwith "Invalid engine definition"
    with
    | ex ->
      printfn "Error in reading engine definition in JSON format from this file: %s/%s" folder fileName
      sprintf "Complete error message: %s" ex.Message |> failwith

  let readEngineDefs folder engineDefList =
    [ for def in engineDefList -> readEngineDef folder def ]

  let readTournamentJson (path: string) : Tournament option =
      if not (File.Exists path) then
          ConsoleUtils.printInColor ConsoleColor.Red (sprintf "***Note: Tournament.json file %s was not found" path)
          ConsoleUtils.printInColor ConsoleColor.White "\nA new (empty) tournament.json will be created with default settings\n"
          None
      else
          try
              use reader = new StreamReader(path)
              let json = reader.ReadToEnd()
              let tournament = JsonSerializer.Deserialize<Tournament>(json, JsonSerializerOptions(AllowTrailingCommas = true))
              let tournament =
                  if obj.ReferenceEquals(box tournament.MoveAnnotation, null) then
                      { tournament with MoveAnnotation = MoveAnnotation.Standard }
                  else
                      tournament
              if tournament.EngineSetup.EngineDefList.Length = 0 then
                 Some {tournament with EngineSetup = {tournament.EngineSetup with Engines = []}}
              else
                  Some tournament
          with ex ->
              ConsoleUtils.printInColor ConsoleColor.Red (sprintf "***Error deserializing %s: %s" path ex.Message)
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
      | ex -> ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Error: %s" ex.Message)

  let loadBaseConfig (jsonPath: string) =
      let json = File.ReadAllText(jsonPath)
      let options = createJsonOptions()
      options.PropertyNameCaseInsensitive <- true
      JsonSerializer.Deserialize<EngineConfig>(json, options)

  let cloneOptions (dict: Dictionary<string,obj>) =
      Dictionary<string,obj>(dict)

  let makeEngineConfigFile (baseConfig: EngineConfig) (networkPath: string) =
      let newOptions = cloneOptions baseConfig.Options
      newOptions.["WeightsFile"] <- networkPath :> obj
      let onnxName = Path.GetFileNameWithoutExtension(networkPath)
      let baseConfigName = baseConfig.Name.Trim().Split(" ").[0]
      let newName = sprintf "%s %s" baseConfigName onnxName
      { baseConfig with
          Name = newName
          Options = newOptions }

  let generateCeresJsonFiles (baseConfig: EngineConfig) (onnxFolderPath: string) (outputFolderPath: string) =
      let onnxFiles = Directory.GetFiles(onnxFolderPath, "*.onnx")
      let writeOptions = JsonSerializerOptions(WriteIndented = true)

      for onnxFile in onnxFiles do
        let newConfig = makeEngineConfigFile baseConfig onnxFile
        let outputFilename = newConfig.Name + ".json"
        let outputPath = Path.Combine(outputFolderPath, outputFilename)
        let newJson = JsonSerializer.Serialize(newConfig, writeOptions)
        File.WriteAllText(outputPath, newJson)
        printfn "Created %s" outputPath
      printfn "Generated %d JSON files" onnxFiles.Length

  let generateLc0sonFiles (baseConfig: EngineConfig) (networkFolderPath: string) (outputFolderPath: string) =
      let networkFiles = Directory.GetFiles(networkFolderPath, "*.pb.gz")
      let writeOptions = JsonSerializerOptions(WriteIndented = true)

      for networkFile in networkFiles do
        let newConfig = makeEngineConfigFile baseConfig networkFile
        let outputFilename = newConfig.Name + ".json"
        let outputPath = Path.Combine(outputFolderPath, outputFilename)
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
    open PuzzleTypes

    let escapeString (s:string) =
        if String.IsNullOrEmpty s then
            ""
        else
          let sb = System.Text.StringBuilder()
          for c in s do
              match c with
              | '\\' -> sb.Append("\\\\") |> ignore
              | '"'  -> sb.Append("\\\"") |> ignore
              | '\n' -> sb.Append("\\n")  |> ignore
              | '\r' -> sb.Append("\\r")  |> ignore
              | '\t' -> sb.Append("\\t")  |> ignore
              | c when Char.IsControl c ->
                  sb.Append(sprintf "\\u%04X" (int c)) |> ignore
              | c ->
                  sb.Append(c) |> ignore
          sb.ToString()

    let private emptyPositions : Position seq = Seq.empty
    let private emptyFens : string seq = Seq.empty

    let parsePuzzle (filePath: string) (random: bool) : CsvPuzzleData[] =
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

            CsvPuzzleData.Create(
              puzzleId, fen, moves, rating, ratingDeviation,
              popularity, nbPlays, themes, gameUrl,
              openingTags, "", emptyPositions, emptyFens, 0
            )

        let records =
          File.ReadAllLines(filePath)
          |> Array.skip 1
          |> Array.map parseLine

        if random then
            System.Random.Shared.Shuffle(records)

        records

    let parsePuzzleInParallel (filePath: string) (random: bool) : CsvPuzzleData[] =
        let lines = File.ReadAllLines(filePath) |> Array.skip 1

        let recordsArray =
            lines
            |> Array.Parallel.map (fun line ->
                let fields = line.Split(',')
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
                    "",
                    emptyPositions,
                    emptyFens,
                    0 ))

        if random then
            System.Random.Shared.Shuffle(recordsArray)

        recordsArray

    let normalizePath (path: string) =
      if String.IsNullOrEmpty path then
        ""
      else
        escapeString path

    let loadEretConfig (filePath: string) : PuzzleTypes.EretConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(PuzzleTypes.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<PuzzleTypes.EretConfig>(json, options)
        else
            failwithf "File not found: %s" filePath

    let loadEngineListConfig (filePath: string) : PuzzleTypes.EngineListConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(PuzzleTypes.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<PuzzleTypes.EngineListConfig>(json, options)
        else
            failwithf "File not found: %s" filePath

    let loadPuzzleConfig (filePath: string) : PuzzleTypes.PuzzleConfig =
        let options = new JsonSerializerOptions(AllowTrailingCommas = true)
        options.Converters.Add(PuzzleTypes.PuzzleEngineConverter())
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            JsonSerializer.Deserialize<PuzzleTypes.PuzzleConfig>(json, options)
        else
            failwithf "File not found: %s" filePath

    /// Extracts (prefix option, embedded params option) from an existing Network value.
    /// Prefix = backend identifier like "ONNX_TRT" (distinguished from drive letter by length > 1).
    /// Embedded params = everything from the first "|" onward (e.g., "|cudagraphs=true;V1TEMP=0.55").
    let private extractNetworkParts (existing: string) =
        let pipeIdx = existing.IndexOf('|')
        let mainPart, embeddedParams =
            if pipeIdx >= 0 then existing.Substring(0, pipeIdx), Some (existing.Substring(pipeIdx))
            else existing, None
        let colonIdx = mainPart.IndexOf(':')
        let prefix =
            if colonIdx > 1 then Some (mainPart.Substring(0, colonIdx))
            else None
        prefix, embeddedParams

    let private applyNetToConfig (configFileName: string) (baseConfig: EngineConfig) (net: string) =
        let newOptions = JSON.cloneOptions baseConfig.Options

        if newOptions.ContainsKey("WeightsFile") then
            newOptions.["WeightsFile"] <- box net
        elif newOptions.ContainsKey("Network") then
            let existingStr =
                match newOptions.["Network"] with
                | :? JsonElement as je when je.ValueKind = JsonValueKind.String -> je.GetString()
                | :? string as s -> s
                | _ -> null
            if not (isNull existingStr) then
                let prefix, embeddedParams = extractNetworkParts existingStr
                let result =
                    (match prefix with Some p -> p + ":" | None -> "")
                    + net
                    + (match embeddedParams with Some e -> e | None -> "")
                newOptions.["Network"] <- box result
            else
                newOptions.["Network"] <- box net
        else
            ConsoleUtils.printInColor ConsoleColor.Yellow (sprintf "Warning: Engine '%s' config does not contain 'WeightsFile' or 'Network' option to set net '%s'" baseConfig.Name net)

        let netName = Path.GetFileNameWithoutExtension net
        // Label instances by the config FILE name, not the Name field inside it: the same
        // engine binary is often referenced by several configs differing only in options
        // (e.g. output head), where the Name field — and even the net — can be identical.
        let configName = Path.GetFileNameWithoutExtension configFileName
        let displayNet = Path.GetFileName(net)
        { baseConfig with Name = configName + " " + netName; Options = newOptions; NetworkPath = displayNet }

    let mapToEngConfig (engineFolder: string) (engine: PuzzleTypes.PuzzleEngine) =
        match engine with
        | PuzzleTypes.Engine (name, _) ->
            let fullpath = Path.Combine(engineFolder, name)
            let engineConfig = JSON.readSingleEngineConfig fullpath
            [ engineConfig ]
        | PuzzleTypes.EngineWithNets (name, _, nets) ->
            let fullPath = Path.Combine(engineFolder, name)
            let baseConfig = JSON.readSingleEngineConfig fullPath
            [
                for net in nets do
                    yield applyNetToConfig name baseConfig net
            ]

    let mapToEngPuzzleConfig (engineFolder: string) (engine: PuzzleTypes.PuzzleEngine) =
        try
            match engine with
            | PuzzleTypes.Engine (name, nodes) ->
                try
                    let fullpath = Path.Combine(engineFolder, name)
                    let engineConfig = JSON.readSingleEngineConfig fullpath
                    [ engineConfig, nodes ]
                with ex ->
                    ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Error reading engine config '%s': %s" name ex.Message)
                    []
            | PuzzleTypes.EngineWithNets (name, nodes, nets) ->
                let fullPath = Path.Combine(engineFolder, name)
                let mapNet (net: string) =
                    try
                        let baseConfig = JSON.readSingleEngineConfig fullPath
                        let cfg = applyNetToConfig name baseConfig net
                        Some (cfg, nodes)
                    with ex ->
                        ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Error mapping engine '%s' with net '%s': %s" name net ex.Message)
                        None

                nets |> Seq.choose mapNet |> Seq.toList
        with ex ->
            ConsoleUtils.printInColor ConsoleColor.Red (sprintf "Unexpected error in mapToEngPuzzleConfig: %s" ex.Message)
            []

module PuzzleTheme =
  open System.Xml.Linq
  open PuzzleTypes

  let puzzleThemes = """<?xml version="1.0" encoding="UTF-8"?>
  <resources>
    <string name="advancedPawn">Advanced pawn</string>
    <string name="advancedPawnDescription">One of your pawns is deep into the opponent position, maybe threatening to promote.</string>
    <string name="advantage">Advantage</string>
    <string name="advantageDescription">Seize your chance to get a decisive advantage. (200cp = eval = 600cp)</string>
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
    <string name="crushingDescription">Spot the opponent blunder to obtain a crushing advantage. (eval = 600cp)</string>
    <string name="doubleBishopMate">Double bishop mate</string>
    <string name="doubleBishopMateDescription">Two attacking bishops on adjacent diagonals deliver mate to a king obstructed by friendly pieces.</string>
    <string name="dovetailMate">Dovetail mate</string>
    <string name="dovetailMateDescription">A queen delivers mate to an adjacent king, whose only two escape squares are obstructed by friendly pieces.</string>
    <string name="equality">Equality</string>
    <string name="equalityDescription">Come back from a losing position, and secure a draw or a balanced position. (eval = 200cp)</string>
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

  let getLocalThemes () : PuzzleCategory list =
      let doc = XDocument.Parse(puzzleThemes)
      doc.Descendants(XName.Get "string")
      |> Seq.choose (fun element ->
          let nameAttr = element.Attribute(XName.Get "name")
          match nameAttr with
          | null -> None
          | attr when attr.Value.EndsWith "Description" ->
              let category = attr.Value.Replace("Description", "")
              Some { Category = category; Description = element.Value }
          | _ -> None)
      |> Seq.toList
