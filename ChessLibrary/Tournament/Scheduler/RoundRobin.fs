module ChessLibrary.Scheduler.RoundRobin

open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

/// Pad the player list to an even length by prepending a sentinel "bye"
/// engine. Odd-sized round-robins use this sentinel to mark the bye slot
/// at each round; pairings involving the sentinel are dropped.
let private padEven (players: EngineConfig list) : EngineConfig list =
    if List.length players % 2 = 1
    then EngineConfig.Empty :: players
    else players

/// Berger-style rotation: the first element stays fixed; the rest cycles by
/// moving the last element to position 1. Returns the N-1 lists representing
/// all rounds of a Berger round-robin.
let private berger (players: EngineConfig list) : EngineConfig list list =
    let rec loop acc n current =
        if n = 0
        then List.rev acc
        else
            let next = Shared.rotateOnce current
            loop (next :: acc) (n - 1) next
    let rounds = List.length players - 1
    if rounds <= 0
    then [ players ]
    else loop [ players ] (rounds - 1) players

/// For a single Berger rotation, return the list of `(white, black)` pairs
/// that play this round (bye pairings dropped). Colors alternate per the
/// classical Berger rule: the first pair alternates with round parity;
/// subsequent pairs alternate with position parity.
let private pairsForRotation
    (rotated: EngineConfig list)
    (round: int)
    : (EngineConfig * EngineConfig) list
    =
    let evenRound = round % 2 = 0
    let half = List.length rotated / 2
    let first, second = rotated |> List.splitAt half
    let zipped = List.zip first (List.rev second)
    [ for idx, (a, b) in zipped |> List.indexed do
        let skip =
            a.Name = EngineConfig.Empty.Name || b.Name = EngineConfig.Empty.Name
        if not skip then
            if idx = 0 then
                if evenRound then yield (b, a) else yield (a, b)
            elif idx % 2 = 1 then yield (a, b)
            else yield (b, a) ]

let private makePlanned (white: EngineConfig) (black: EngineConfig) (opening: PgnGame) (openingHash: string) : PlannedGame =
    { White = white
      Black = black
      Opening = opening
      OpeningHash = openingHash
      RoundLabel = ""   // set later by Diff.applyPairLabels
      RoleWhite = Standard
      RoleBlack = Standard
      Key =
        { OpeningHash = openingHash
          Fen = opening.GameMetaData.Fen
          White = white.Name
          Black = black.Name } }

/// Generate the full plan for a round-robin tournament. Invariants:
///
///   * Under `OpeningsTwice = false`: each (unordered) player pair plays each
///     opening once, with colors alternating across pairs per Berger.
///   * Under `OpeningsTwice = true`: each ordered pair plays each opening;
///     the two colored games for a pair are emitted **back-to-back** so the
///     downstream pair-label pass can number them as `{pairIdx}.1`/`{pairIdx}.2`.
///
/// Player order and opening order drive game order deterministically.
let generate (config: ScheduleConfig) : PlannedGame list =
    let players = config.Challengers @ config.Opponents   // RR ignores the gauntlet split
    let padded = padEven players
    let rotations = berger padded
    [ for opening in config.Openings do
        let openingHash = Hash.computeOpeningHashFromGame opening
        for (rotationIdx, rotated) in List.indexed rotations do
            for (white, black) in pairsForRotation rotated (rotationIdx + 1) do
                yield makePlanned white black opening openingHash
                if config.OpeningsTwice then
                    yield makePlanned black white opening openingHash ]
