module ChessLibrary.GameInitialization

open System
open System.IO
open System.Text
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.Engine
module PGNWriter = ChessLibrary.PGNWriter
open ChessLibrary.EngineProtocol
open ChessLibrary.CustomException

let appendGameDescription (sb:StringBuilder) (tourny:Tournament) (player1:ChessEngine) (player2:ChessEngine) (openingMoves: ResizeArray<string>) fen =
    let append (txt:string) = sb.Append txt |> ignore
    let isEpd =
        match tourny.Opening.OpeningsPath with
        |Some path ->
            let ext = Path.GetExtension path
            ext.ToLower().Contains ".epd"
        |_ -> false
    let tcWhite = tourny.TimeControl.GetTimeConfig player1.Config.TimeControlID
    let tcBlack = tourny.TimeControl.GetTimeConfig player2.Config.TimeControlID
    let tournyData = "{TournamentOptions: " + tourny.PGNSummary() + (if isEpd then sprintf " FEN=%s;" fen else "")
    let moveOverheadMs = tourny.MoveOverhead.TotalMilliseconds
    let whiteEngineData = $" WhiteEngineOptions: TimeControl: {tcWhite.ToString()}; {player1.Config.Information moveOverheadMs}"
    let blackEngineData = $"BlackEngineOptions: TimeControl: {tcBlack.ToString()}; {player2.Config.Information moveOverheadMs}"
    let wCmds = if player1.IsLc0 then $" (White commands: {UciOptions.createCommandsFromConfig player1.Config})" else ""
    let bCmds = if player2.IsLc0 then $" (Black commands: {UciOptions.createCommandsFromConfig player2.Config})" else ""
    let whiteArgs, blackArgs = player1.Config.Args, player2.Config.Args
    append tournyData
    append whiteEngineData
    if String.IsNullOrEmpty whiteArgs |> not then append $" (Args: {whiteArgs})"
    append blackEngineData
    if String.IsNullOrEmpty blackArgs |> not then append $" (Args: {blackArgs})"
    append wCmds
    append bCmds
    append ("}" + Environment.NewLine)
    if openingMoves.Count > 0 then
        let opMoves = PGNWriter.writeOpeningPGNMoves openingMoves
        append opMoves

let checkAndPrepareContempt (engine1: ChessEngine) (engine2: ChessEngine) =
    let hasKeyCI (options: IDictionary<string, 'a>) (key: string) =
        options.Keys |> Seq.exists (fun k -> String.Equals(k, key, StringComparison.OrdinalIgnoreCase))

    if engine1.Config.ContemptEnabled then
        let ratingDiff = engine1.Config.Rating - engine2.Config.Rating
        if ratingDiff > 0 || engine1.Config.NegativeContemptAllowed then
            let options = engine1.GetDefaultOptions()
            if hasKeyCI options "Contempt" then
                let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
                engine1.AddSetOption engineOption
                printfn "Contempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
            elif hasKeyCI options "DynamicContempt" then
                let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
                engine1.AddSetOption engineOption
                printfn "DynamicContempt set for %s: %d vs %s" engine1.Name ratingDiff engine2.Name
        else
            printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine1.Name ratingDiff engine2.Name

    if engine2.Config.ContemptEnabled then
        let ratingDiff = engine2.Config.Rating - engine1.Config.Rating
        if ratingDiff > 0 || engine2.Config.NegativeContemptAllowed then
            let options = engine2.GetDefaultOptions()
            if hasKeyCI options "Contempt" then
                let engineOption : EngineOption = { Name = "Contempt"; Value = sprintf "%d" ratingDiff }
                engine2.AddSetOption engineOption
                printfn "Contempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
            elif hasKeyCI options "DynamicContempt" then
                let engineOption : EngineOption = { Name = "DynamicContempt"; Value = sprintf "%d" ratingDiff }
                engine2.AddSetOption engineOption
                printfn "DynamicContempt set for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
        else
            printfn "No contempt set (rating diff negative) for %s: %d vs %s" engine2.Name ratingDiff engine1.Name
