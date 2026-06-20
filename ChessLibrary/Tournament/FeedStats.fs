module ChessLibrary.FeedStats

// Standings / crosstable computation as pure functions of (tournament config, results).
//
// These mirror the Manager.Runner.GetPlayerResults / GenerateStatsCrosstable / GetGauntletCrosstable
// members, but take the Tournament explicitly instead of reading the runner's loaded config. This
// lets the live-feed view (which has no engine-running runner) compute standings from the tournament
// config delivered in the StartOfTournament event plus the results accumulated from EndOfGame events.

open System
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.PGNTypes
open ChessLibrary.GameAnalysis

let private challengersAndPlayers (tournament: Tournament) =
    let challengers =
        tournament.EngineSetup.Engines
        |> List.filter (fun e -> e.IsChallenger)
        |> List.map _.Name
    let players = tournament.EngineSetup.Engines |> List.map _.Name
    challengers, players

/// Standings computed purely from accumulated results (no tournament config / engine list needed)
/// — for the multi-game grid view. Mirrors Runner.GetPlayerResultsFromPGN.
let playerResultsFromResults (results: ResizeArray<Result>) : ResizeArray<PlayerResult> =
    PGNCalculator.getFullStatFromResults results |> ResizeArray

/// Build a minimal PgnGame carrying just the fields pentanomial pairing needs (White/Black/Result/
/// OpeningHash). Lets the feed view compute pentanomial without full PGN data.
let pentanomialGame (white: string) (black: string) (result: string) (openingHash: string) : PgnGame =
    { PgnGame.Empty 0 with
        GameMetaData = { GameMetadata.Empty with White = white; Black = black; Result = result; OpeningHash = openingHash } }

/// Compact pentanomial line for a single matchup (2 engines), from synthesized games.
let pentanomialCompact (games: ResizeArray<PgnGame>) : string =
    ChessLibrary.Statistics.Pentanomial.formatSingleMatchupCompact games

/// Player standings (points, W/D/L, Elo, etc.) — mirrors Runner.GetPlayerResults.
let playerResults (tournament: Tournament) (results: ResizeArray<Result>) : ResizeArray<PlayerResult> =
    let challengers, players = challengersAndPlayers tournament
    let isGauntlet = tournament.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
    PGNCalculator.getFullStat isGauntlet challengers players results

/// Small results crosstable — mirrors Runner.GenerateStatsCrosstable.
let smallCrosstable (tournament: Tournament) (results: ResizeArray<Result>) =
    let challengers, players = challengersAndPlayers tournament
    PGNCalculator.generateSmallStatCrossTable results challengers players

/// Big gauntlet crosstable — mirrors Runner.GetGauntletCrosstable.
let bigCrosstable (tournament: Tournament) (results: ResizeArray<Result>) =
    let players = tournament.EngineSetup.Engines |> List.map _.Name
    let challengers =
        if tournament.EngineSetup.Engines.Length = 2 then
            players |> List.take 1
        else
            tournament.EngineSetup.Engines
            |> List.filter (fun e -> e.IsChallenger)
            |> List.map _.Name
    PGNCalculator.generateBigStatCrossTable results challengers players
