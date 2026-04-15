module ChessLibrary.Scheduler.Ladder

open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

/// Build the per-match UI-preview games for a Ladder mini-match. Ladder play
/// is state-machine-driven (matches may extend with tiebreaks), so this
/// helper just shapes the planned games for display — not a full tournament
/// scheduler in the Gauntlet/RR sense.
///
/// `firstOpening` is the opening already in flight (or the next one, for
/// even game counts); `firstPlayOrder` is the explicit (W, B) sequence the
/// runner wants to queue for that opening. Additional games beyond the
/// first opening stride through `subsequentOpenings` modulo, alternating
/// colors.
///
/// Returns (games, nextSubsequentOpeningIndex) so callers can chain.
let buildPreviewGames
    (challenger: EngineConfig)
    (defender: EngineConfig)
    (firstOpening: PgnGame)
    (firstPlayOrder: (EngineConfig * EngineConfig) list)
    (subsequentOpenings: PgnGame list)
    (subsequentOpeningStartIndex: int)
    (gamesRemaining: int)
    (climbNumber: int)
    (priorGameCount: int)
    : PlannedGame list * int
    =
    let games = ResizeArray<PlannedGame>()
    let mutable remaining = gamesRemaining
    let mutable planIdx = 0

    let addGame (opening: PgnGame) (white: EngineConfig) (black: EngineConfig) =
        let openingHash = Hash.computeOpeningHashFromGame opening
        let key : GameKey =
            { OpeningHash = openingHash
              Fen = opening.GameMetaData.Fen
              White = white.Name
              Black = black.Name }
        games.Add
            { White = white
              Black = black
              Opening = opening
              OpeningHash = openingHash
              RoundLabel = sprintf "%d.%d" climbNumber (priorGameCount + planIdx + 1)
              RoleWhite = Standard
              RoleBlack = Standard
              Key = key }
        planIdx <- planIdx + 1
        remaining <- remaining - 1

    for (w, b) in firstPlayOrder do
        if remaining > 0 then addGame firstOpening w b

    let mutable peekIndex = subsequentOpeningStartIndex
    let subsequentArr = subsequentOpenings |> List.toArray
    while remaining > 0 && subsequentArr.Length > 0 do
        let peekOpening = subsequentArr.[peekIndex % subsequentArr.Length]
        peekIndex <- peekIndex + 1
        for (w, b) in [ challenger, defender; defender, challenger ] do
            if remaining > 0 then addGame peekOpening w b

    List.ofSeq games, peekIndex
