module ChessLibrary.Scheduler.Gauntlet

open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

/// Build the opening slice each opponent will play, indexed by the opponent's
/// original position in the opponents list. Pure function of the config.
///
///   * `Spread`  — round-major slot layout (`slot = r × numOpps + oppIdx`), so
///                 within one round all opponents' openings are consecutive in
///                 the book. Matches the legacy gauntlet formula. Falls back
///                 to modulo striding when the book is smaller than needed.
///   * `Shared`  — every opponent gets the same first `R` openings
///                 (or strides with modulo when the book is smaller).
let private buildOpeningsPerOpponent (config: ScheduleConfig) : PgnGame array array =
    let numOpps = config.Opponents.Length
    let book = config.Openings |> List.toArray
    let bookSize = book.Length
    if bookSize = 0 || numOpps = 0 then
        [||]
    else
        match config.Distribution with
        | Spread ->
            [| for i in 0 .. numOpps - 1 ->
                [| for r in 0 .. config.Rounds - 1 ->
                    book.[((r * numOpps) + i) % bookSize] |] |]
        | Shared ->
            let slice = [| for r in 0 .. config.Rounds - 1 -> book.[r % bookSize] |]
            [| for _ in 0 .. numOpps - 1 -> slice |]

/// Apply the PreventMoveDeviation rotation to the opponents list for a given
/// round. The current semantics rotate the list `roundIdx + 1` times (so
/// round 0 already has one rotation applied); preserving this exactly avoids
/// silent behavior drift from the old `gauntletSingleRound` branch.
let private opponentsForRound (config: ScheduleConfig) (roundIdx: int) : (int * EngineConfig) list =
    let baseList = config.Opponents |> List.indexed   // keep the original index alongside
    if config.PreventDeviation then
        let rotations = roundIdx + 1
        let rec rotate lst n =
            if n <= 0 then lst
            else
                match lst with
                | [] -> []
                | h :: t -> rotate (t @ [ h ]) (n - 1)
        rotate baseList rotations
    else
        baseList

/// Build the GameKey used for resume diffing.
let private makeKey (white: EngineConfig) (black: EngineConfig) (opening: PgnGame) (openingHash: string) : GameKey =
    { OpeningHash = openingHash
      Fen = opening.GameMetaData.Fen
      White = white.Name
      Black = black.Name }

/// Build a single `PlannedGame` with all derived fields computed once.
/// `RoundLabel` is left blank — `Diff.applyPairLabels` will re-stamp it
/// after `Diff.diff` and any quota enforcement.
let private planned
    (white: EngineConfig)
    (black: EngineConfig)
    (opening: PgnGame)
    (openingHash: string)
    (roleWhite: Role)
    (roleBlack: Role)
    : PlannedGame =
    { White = white
      Black = black
      Opening = opening
      OpeningHash = openingHash
      RoundLabel = ""
      RoleWhite = roleWhite
      RoleBlack = roleBlack
      Key = makeKey white black opening openingHash }

/// Produce the full list of games the tournament intends to play under the
/// current config. Invariants guaranteed by construction (no post-hoc filter
/// needed):
///   * Total count = `Rounds × numOpp × numChallengers × (OpeningsTwice ? 2 : 1)`
///   * No duplicate `Key` in the output list
///   * Each (opponent, opening) pair appears at most once per challenger
///     (twice with OpeningsTwice, for the two colors)
///   * Under `Spread`, opponents play disjoint opening slices
///   * Under `Shared`, every opponent plays every opening
///
/// Games are emitted in **pair-consecutive** order: each (challenger, opp,
/// opening) pair yields its direct game immediately followed by its reversed
/// game when `OpeningsTwice` is on. This makes `Diff.applyPairLabels` trivial
/// (label = `{pairIdx}.{colorIdx}`) and keeps both games of a pair together
/// in the play queue. `RoundLabel` is a placeholder until a labeler pass
/// (e.g. `Diff.applyPairLabels`) rewrites it.
let generate (config: ScheduleConfig) : PlannedGame list =
    let challengers = config.Challengers
    let openingsFor = buildOpeningsPerOpponent config
    [ for roundIdx in 0 .. config.Rounds - 1 do
        let roundOpps = opponentsForRound config roundIdx
        for (oppOriginalIdx, opp) in roundOpps do
            let opening = openingsFor.[oppOriginalIdx].[roundIdx]
            let openingHash = Hash.computeOpeningHashFromGame opening
            for challenger in challengers do
                yield planned challenger opp opening openingHash Challenger Opponent
                if config.OpeningsTwice then
                    yield planned opp challenger opening openingHash Opponent Challenger ]
