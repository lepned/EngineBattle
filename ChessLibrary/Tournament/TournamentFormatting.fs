module ChessLibrary.TournamentFormatting

open System
open System.IO
open System.Text
open TypesDef.Tournament
open ChessLibrary.TimeControlTypes

/// Helper functions for formatting Tournament information.
/// Extracted from Tournament record methods for better organization.

let formatTimeSpan (fixedTime: TimeSpan) (incrementTime: TimeSpan) : string =
    let totalFixedMinutes = fixedTime.TotalMinutes
    let totalFixedSeconds = fixedTime.TotalSeconds
    let totalIncrementSeconds =
        float incrementTime.Seconds +
        (float incrementTime.Milliseconds / 1000.0) +
        float incrementTime.Minutes * 60.0 +
        float incrementTime.Hours * 3600.0
    let fixedTimePart =
        if totalFixedMinutes >= 1.0 then
            if fixedTime.Seconds > 0 then sprintf "%.1f'" totalFixedMinutes
            else sprintf "%.0f'" totalFixedMinutes
        else sprintf "%.0f''" totalFixedSeconds
    let incrementTimePart =
        if incrementTime.Milliseconds > 0 then sprintf "%.1f''" totalIncrementSeconds
        else sprintf "%.0f''" totalIncrementSeconds
    sprintf "%s + %s" fixedTimePart incrementTimePart

let formatNumberWithK (number: int) =
    if number < 1000 then sprintf "%d nodes" number
    else sprintf "%.1fK nodes" (float number / 1000.0)

let formatNumberWithKShort (number: int) =
    if number < 1000 then sprintf "%d n" number
    else sprintf "%.1fK n" (float number / 1000.0)

let modeLabel (t: Tournament) =
    if String.IsNullOrWhiteSpace t.TournamentMode then "RR" else t.TournamentMode

let effectiveSwissRounds (t: Tournament) =
    if obj.ReferenceEquals(t.SwissOptions, null) then t.Rounds
    elif t.SwissOptions.Rounds > 0 then t.SwissOptions.Rounds
    else t.Rounds

let effectiveCupRounds (playerCount: int) =
    let mutable rounds = 0
    let mutable remaining = playerCount
    while remaining > 1 do
        remaining <- remaining / 2
        rounds <- rounds + 1
    rounds

let effectiveRounds (t: Tournament) =
    let mode = modeLabel t
    let playerCount =
        if obj.ReferenceEquals(t.EngineSetup, null) || obj.ReferenceEquals(t.EngineSetup.Engines, null) then 0
        else t.EngineSetup.Engines.Length
    if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) then effectiveSwissRounds t
    elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) then effectiveCupRounds playerCount
    else t.Rounds

let scheduleSummary (t: Tournament) =
    let mode = modeLabel t
    let players =
        if obj.ReferenceEquals(t.EngineSetup, null) || obj.ReferenceEquals(t.EngineSetup.Engines, null) then 0
        else t.EngineSetup.Engines.Length
    if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) then
        let rounds = effectiveSwissRounds t
        let gamesPerMatch = if obj.ReferenceEquals(t.SwissOptions, null) then 2 else t.SwissOptions.GamesPerMatch
        let allowExtra = if obj.ReferenceEquals(t.SwissOptions, null) then false else t.SwissOptions.AllowExtraPairsOnTie
        let byes = if players % 2 = 1 then " + 1 bye/round" else ""
        let plannedGames = rounds * (players / 2) * gamesPerMatch
        let extraPairs = if allowExtra then " + extra pairs on tie" else ""
        sprintf "Schedule: players=%d, rounds=%d, games/match=%d, planned games=%d%s%s" players rounds gamesPerMatch plannedGames byes extraPairs
    elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) then
        let rounds = effectiveCupRounds players
        let roundPairs =
            if obj.ReferenceEquals(t.CupOptions, null) then [] else t.CupOptions.RoundPairIncrements
        let pairsForRound roundNumber =
            if roundPairs.IsEmpty then 1
            else
                let idx = Math.Max(0, roundNumber - 1)
                if idx < roundPairs.Length then roundPairs.[idx] else roundPairs.[roundPairs.Length - 1]
        let plannedGames =
            [1 .. rounds]
            |> List.sumBy (fun roundNumber ->
                let matches = players / (1 <<< roundNumber)
                let pairs = Math.Max(1, pairsForRound roundNumber)
                matches * pairs * 2)
        let pairsText =
            if roundPairs.IsEmpty then "pairs/round=1"
            else sprintf "pairs/round=%s" (roundPairs |> Seq.map string |> String.concat "-")
        sprintf "Schedule: players=%d, rounds=%d, %s, planned games=%d (+ tiebreaks)" players rounds pairsText plannedGames
    else
        sprintf "Schedule: players=%d, rounds=%d" players t.Rounds

let timeControlTextForPlayer (t: Tournament) (id: int) =
    let tc = t.TimeControl.GetTimeConfig id
    if tc.NodeLimit then formatNumberWithK tc.Nodes
    else formatTimeSpan (tc.Fixed) (tc.Increment)

let timeControlTextForPlayers (t: Tournament) (id1: int) (id2: int) =
    let moreThanOneTC = id1 <> id2
    let tc1 = t.TimeControl.GetTimeConfig id1
    if not moreThanOneTC then
        formatTimeSpan (tc1.Fixed) (tc1.Increment)
    else
        let tc2 = t.TimeControl.GetTimeConfig id2
        let tc1Text =
            if tc1.NodeLimit then formatNumberWithKShort tc1.Nodes
            else formatTimeSpan (tc1.Fixed) (tc1.Increment)
        let tc2Text =
            if tc2.NodeLimit then formatNumberWithKShort tc2.Nodes
            else formatTimeSpan (tc2.Fixed) (tc2.Increment)
        sprintf "%s vs %s" tc1Text tc2Text

let timeControlText (t: Tournament) =
    let moreThanOneTC = t.EngineSetup.Engines |> Seq.exists (fun e -> e.TimeControlID > 1)
    let engineConf = t.EngineSetup.Engines |> Seq.tryHead
    match engineConf with
    | None -> "No time control"
    | Some engineConf ->
        let tc1 = t.TimeControl.GetTimeConfig engineConf.TimeControlID
        if not moreThanOneTC then
            tc1.ToString()
        else
            let tcs = t.EngineSetup.Engines |> Seq.filter (fun e -> e.TimeControlID <> tc1.Id) |> Seq.map (fun e -> e.TimeControlID) |> Seq.distinct
            if Seq.length tcs = 1 then
                let tc2Id = Seq.head tcs
                let tc2 = t.TimeControl.GetTimeConfig tc2Id
                let tc1Text =
                    if tc1.NodeLimit then sprintf "%dN" tc1.Nodes
                    else formatTimeSpan (tc1.Fixed) (tc1.Increment)
                let tc2Text =
                    if tc2.NodeLimit then sprintf "%dN" tc2.Nodes
                    else formatTimeSpan (tc2.Fixed) (tc2.Increment)
                sprintf "%s vs %s" tc1Text tc2Text
            else
                tc1.ToString()

let gauntletText (t: Tournament) =
    if t.IsGauntlet && t.EngineSetup.Engines.Length > 0 then
        let players = t.EngineSetup.Engines |> Seq.truncate t.Challengers
        let opponents = t.EngineSetup.Engines |> Seq.skip t.Challengers
        let oppMsg = opponents |> Seq.fold (fun st e -> sprintf "%s %s, " st e.Name) ""
        let mutable txt = "Gauntlet:"
        for p in players do
            txt <- txt + sprintf " %s vs %s" p.Name oppMsg
        txt + " "
    else ""

let tablebaseText (t: Tournament) =
    if t.Adjudication.TBAdj.UseTBAdjudication then
        sprintf "Tablebase adj=%d-men" t.Adjudication.TBAdj.TBMen
    else "Tablebase adj=not in use"

let getOpeningFileName (t: Tournament) =
    if t.Opening.OpeningsPath.IsSome then
        Path.GetFileName t.Opening.OpeningsPath.Value
    else "no opening book"

let adjudicationText (t: Tournament) =
    sprintf "Adjudication: -draw movenumber=%d movecount=%d score=%.1f cp -resign movecount=%d score=%.1f cp "
        t.Adjudication.DrawOption.MinDrawMove
        t.Adjudication.DrawOption.DrawMoveLength
        t.Adjudication.DrawOption.MaxDrawScore
        t.Adjudication.WinOption.WinMoveLength
        t.Adjudication.WinOption.MinWinScore

let summary (t: Tournament) =
    let sb = StringBuilder()
    let mode = modeLabel t
    let formatDurationText (time: TimeSpan) = TypesDef.CoreTypes.formatDuration time
    let openings =
        let book =
            if t.Opening.OpeningsPath.IsSome then Path.GetFileName t.Opening.OpeningsPath.Value
            else "No book"
        let parts = ResizeArray<string>()
        parts.Add(sprintf "Book: %s" book)
        parts.Add(sprintf "ply=%d" t.Opening.OpeningsPly)
        parts.Add(sprintf "twice=%b" t.Opening.OpeningsTwice)
        if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) && not (obj.ReferenceEquals(t.SwissOptions, null)) then
            parts.Add(sprintf "random=%b" t.SwissOptions.RandomOpenings)
            parts.Add(sprintf "unique=%s" (if t.SwissOptions.UniquePerMatchOnly then "per match" else "global"))
        elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) && not (obj.ReferenceEquals(t.CupOptions, null)) then
            parts.Add(sprintf "random=%b" t.CupOptions.RandomOpenings)
            parts.Add(sprintf "unique=%s" (if t.CupOptions.UniquePerMatchOnly then "per match" else "global"))
        String.Join(" | ", parts)
    let tablebases =
        if t.Adjudication.TBAdj.UseTBAdjudication then
            sprintf "Tablebases: %d-man" t.Adjudication.TBAdj.TBMen
        else "Tablebases: off"

    sb.AppendLine(sprintf "Run: %s" t.Name) |> ignore
    sb.AppendLine(sprintf "Hardware: %s" (t.Hardware())) |> ignore
    sb.AppendLine(sprintf "Mode: %s" mode) |> ignore
    sb.AppendLine(scheduleSummary t) |> ignore
    let gauntlet = gauntletText t
    if not (String.IsNullOrWhiteSpace gauntlet) then
        sb.AppendLine gauntlet |> ignore
    sb.AppendLine(sprintf "Time: %s | overhead=%s | min move=%dms | pondering=%b"
                    (timeControlText t) (formatDurationText t.MoveOverhead) t.MinMoveTimeInMS t.AllowPondering) |> ignore
    sb.AppendLine openings |> ignore
    sb.AppendLine tablebases |> ignore
    sb.AppendLine(adjudicationText t) |> ignore
    sb.AppendLine(sprintf "Deviations: %d" t.DeviationCounter) |> ignore
    if not (String.IsNullOrWhiteSpace t.PgnOutPath) then
        if String.IsNullOrWhiteSpace t.ReferencePGNPath then
            sb.AppendLine(sprintf "Output: %s" t.PgnOutPath) |> ignore
        else
            sb.AppendLine(sprintf "Output: %s | reference: %s" t.PgnOutPath t.ReferencePGNPath) |> ignore
    sb.AppendLine(t.Players()) |> ignore
    sb.AppendLine(sprintf "Comment: %s" t.Description) |> ignore
    sb.ToString()

let pgnSummary (t: Tournament) =
    let sb = StringBuilder()
    sb.Append(sprintf "%s; " t.Description) |> ignore
    sb.Append(sprintf "Rounds=%d; " t.Rounds) |> ignore
    if t.Opening.OpeningsPath.IsSome then
        sb.Append(sprintf "Book=%s; " (Path.GetFileName t.Opening.OpeningsPath.Value)) |> ignore
    else
        sb.Append "Book=No book; " |> ignore
    sb.Append(tablebaseText t + "; ") |> ignore
    sb.Append(adjudicationText t + ";") |> ignore
    sb.ToString()

let minSummary (t: Tournament) =
    let sb = StringBuilder()
    let mode = modeLabel t
    sb.AppendLine(sprintf "Mode: %s" mode) |> ignore
    sb.AppendLine(scheduleSummary t) |> ignore
    let gauntlet = gauntletText t
    if not (String.IsNullOrWhiteSpace gauntlet) then
        sb.AppendLine gauntlet |> ignore
    sb.AppendLine(sprintf "Time: %s" (timeControlText t)) |> ignore
    let book =
        if t.Opening.OpeningsPath.IsSome then Path.GetFileName t.Opening.OpeningsPath.Value
        else "No book"
    sb.AppendLine(sprintf "Book: %s" book) |> ignore
    sb.AppendLine(tablebaseText t) |> ignore
    sb.AppendLine(adjudicationText t) |> ignore
    sb.AppendLine(t.Players()) |> ignore
    sb.AppendLine(sprintf "Comment: %s" t.Description) |> ignore
    sb.ToString()

let printTournamentSummary (t: Tournament) =
    printfn "Match: %s (%s)" t.Name t.Description
    printfn "%s" (t.Players())
    printfn "%s" "Engine configuration:"
    for eng in t.EngineSetup.Engines do
        let moveOverhead = t.MoveOverhead.TotalMilliseconds
        let info: string = eng.Information moveOverhead
        printfn "\t%s: %s" eng.Name info
    printfn "Hardware: %s + CPU %s" t.GPU t.CPU
    printfn "Software: %s" "EngineBattle GUI"
    printfn "Time control: %s" (timeControlText t)
    printfn "Lc0 benchmark: ~4300 nps"
    if t.Opening.OpeningsPath.IsSome then
        printfn "Book: %s, %s" (Path.GetFileName t.Opening.OpeningsPath.Value) "sequential, twice"
    else
        printfn "Book: No book"
    printfn "%s" (tablebaseText t)
    printfn "%s" (adjudicationText t)
    if t.IsGauntlet then
        let challengers = t.EngineSetup.Engines |> Seq.truncate t.Challengers |> Seq.length
        let gamesPerRound = t.TotalGames / (t.Rounds * 2) - challengers
        if challengers > 0 then
            let totalDeviationGames = float (gamesPerRound * t.Rounds * (if t.Opening.OpeningsTwice then 2 else 1))
            printfn "ChallengerInDoNotDeviateMode: %b, number of stopped deviations=%d (%.2f times per game)"
                t.PreventMoveDeviation t.DeviationCounter (float t.DeviationCounter / totalDeviationGames)
    printfn "Duration: %s" "Days, hours and minutes"
    printfn "Comment:"
