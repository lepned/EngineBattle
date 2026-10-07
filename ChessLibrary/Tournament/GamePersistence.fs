module ChessLibrary.GamePersistence

open System
open System.Text
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.Engine
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.CustomException

// ============================================================================
// Game Metadata Building
// ============================================================================

/// One-line summary for the log; the full record goes to the PGN file.
let gameMetadataSummary (gameData: GameMetadata) =
    let shortHash =
        if String.IsNullOrEmpty gameData.OpeningHash then "-"
        else gameData.OpeningHash.Substring(0, min 8 gameData.OpeningHash.Length)
    sprintf "Game metadata: hash %s, deviations %d" shortHash gameData.Deviations

// ============================================================================
// Replay List Management
// ============================================================================

/// Add game to replay list if deviation tracking enabled
let addToReplayList
    (replayList: ResizeArray<GameReplay>)
    (tourny: Tournament)
    (result: Result)
    (gameData: GameMetadata)
    (longSanMoves: ResizeArray<string>)
    : unit =
    if tourny.PreventMoveDeviation then
        replayList.Add
            { WhitePlayer = result.Player1
              BlackPlayer = result.Player2
              PGNMetaData = gameData
              LongSanMoves = longSanMoves |> ResizeArray }
