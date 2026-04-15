module ChessLibrary.Scheduler.Swiss

open System
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

/// Canonical key for a Swiss pair regardless of color assignment. Two engines
/// with the same (unordered) names produce the same key — used to detect
/// rematches.
let pairKey (a: string) (b: string) : string =
    if String.CompareOrdinal(a, b) <= 0
    then $"{a}|{b}"
    else $"{b}|{a}"

// ---------------------------------------------------------------------------
// Bye selection — shared between the grouped and fallback pairing strategies.
// ---------------------------------------------------------------------------

/// Pick which player (if any) should receive a bye this round. Priority is
///   1. lowest score
///   2. highest seed number (weakest) among equals
///   3. hasn't already received a bye (if possible)
let private chooseByePlayer
    (players: EngineConfig list)
    (seedMap: Map<string, int>)
    (scoreFor: string -> float)
    (byeSet: Set<string>)
    : EngineConfig option
    =
    if players.Length % 2 = 0 then
        None
    else
        let ordered =
            players
            |> List.sortBy (fun p ->
                let seed = seedMap.[p.Name]
                scoreFor p.Name, -seed)
        let preferred = ordered |> List.tryFind (fun p -> byeSet.Contains p.Name |> not)
        preferred |> Option.orElse (ordered |> List.tryHead)

let private partitionBye
    (players: EngineConfig list)
    (byeCandidate: EngineConfig option)
    : EngineConfig list * EngineConfig option
    =
    match byeCandidate with
    | None -> players, None
    | Some bye -> players |> List.filter (fun p -> p.Name <> bye.Name), Some bye

let private byeSentinel () = { EngineConfig.Empty with Name = "BYE" }

// ---------------------------------------------------------------------------
// Primary strategy: score-group top-vs-bottom pairing.
// ---------------------------------------------------------------------------

let private tryGroupedPairings
    (players: EngineConfig list)
    (seedMap: Map<string, int>)
    (scoreFor: string -> float)
    (priorPairs: Set<string>)
    : (EngineConfig * EngineConfig * float) list option
    =
    let groups =
        players
        |> List.groupBy (fun p -> scoreFor p.Name)
        |> List.sortByDescending fst

    let rec matchTopBottom score top bottom acc =
        match top with
        | [] -> Some (List.rev acc)
        | player :: rest ->
            let rec tryOpps options =
                match options with
                | [] -> None
                | opp :: tail ->
                    if priorPairs.Contains (pairKey player.Name opp.Name) then
                        tryOpps tail
                    else
                        let nextBottom = bottom |> List.filter (fun p -> p.Name <> opp.Name)
                        match matchTopBottom score rest nextBottom ((player, opp, score) :: acc) with
                        | Some pairs -> Some pairs
                        | None -> tryOpps tail
            tryOpps bottom

    let rec matchGroup remaining acc =
        match remaining with
        | [] -> Some (List.rev acc)
        | player :: rest ->
            let rec tryOpps options =
                match options with
                | [] -> None
                | opp :: tail ->
                    if priorPairs.Contains (pairKey player.Name opp.Name) then
                        tryOpps tail
                    else
                        let nextRemaining = rest |> List.filter (fun p -> p.Name <> opp.Name)
                        match matchGroup nextRemaining ((player, opp, scoreFor player.Name) :: acc) with
                        | Some pairs -> Some pairs
                        | None -> tryOpps tail
            tryOpps rest

    try
        let mutable carry = List.empty<EngineConfig>
        let pairs = ResizeArray<EngineConfig * EngineConfig * float>()
        for (score, groupPlayers) in groups do
            let group =
                (carry @ groupPlayers)
                |> List.sortBy (fun p -> seedMap.[p.Name])
            carry <- []
            let mutable groupList = group
            if groupList.Length % 2 = 1 then
                carry <- [ groupList.[groupList.Length - 1] ]
                groupList <- groupList |> List.take (groupList.Length - 1)
            let half = groupList.Length / 2
            let top = groupList |> List.take half
            let bottom = groupList |> List.skip half
            let matched =
                match matchTopBottom score top bottom [] with
                | Some p -> p
                | None ->
                    match matchGroup groupList [] with
                    | Some p -> p
                    | None -> failwith "grouped-pairing dead-end"
            for pair in matched do pairs.Add pair
        if carry.Length >= 2 then
            match matchGroup carry [] with
            | Some p -> for pair in p do pairs.Add pair
            | None -> failwith "grouped-pairing dead-end (trailing carry)"
        Some (List.ofSeq pairs)
    with _ ->
        None

// ---------------------------------------------------------------------------
// Fallback strategy: relaxed ordering, minimizes score-diff between opponents.
// Invoked only when the grouped strategy can't satisfy the no-rematch rule.
// ---------------------------------------------------------------------------

let private tryFallbackPairings
    (players: EngineConfig list)
    (seedMap: Map<string, int>)
    (scoreFor: string -> float)
    (priorPairs: Set<string>)
    : (EngineConfig * EngineConfig * float) list option
    =
    let ordered =
        players
        |> List.sortBy (fun p -> (-(scoreFor p.Name), seedMap.[p.Name]))

    let rec findPairs remaining acc =
        match remaining with
        | [] -> Some (List.rev acc)
        | player :: rest ->
            let candidates =
                rest
                |> List.filter (fun opp -> priorPairs.Contains (pairKey player.Name opp.Name) |> not)
                |> List.sortBy (fun opp ->
                    let diff = abs (scoreFor player.Name - scoreFor opp.Name)
                    diff, seedMap.[opp.Name])
            let rec tryCand options =
                match options with
                | [] -> None
                | opp :: tail ->
                    let nextRemaining = rest |> List.filter (fun p -> p.Name <> opp.Name)
                    match findPairs nextRemaining ((player, opp, scoreFor player.Name) :: acc) with
                    | Some pairs -> Some pairs
                    | None -> tryCand tail
            tryCand candidates

    findPairs ordered []

// ---------------------------------------------------------------------------
// Public entry point: pair the next Swiss round.
// ---------------------------------------------------------------------------

/// Pair a Swiss round using only the grouped (score-group top-vs-bottom)
/// strategy — no fallback. Raises if the strategy can't satisfy the
/// no-rematch rule. Mostly useful as a regression probe: callers should
/// prefer `pairNextRound`, which layers the fallback automatically.
let pairNextRoundGroupedOnly
    (players: EngineConfig list)
    (seedOrder: EngineConfig list)
    (scores: Map<string, float>)
    (priorPairs: Set<string>)
    (byeSet: Set<string>)
    : (EngineConfig * EngineConfig) list
    =
    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList
    let scoreFor name =
        scores |> Map.tryFind name |> Option.defaultValue 0.0
    let byeCandidate = chooseByePlayer players seedMap scoreFor byeSet
    let pairingPlayers, byePlayer = partitionBye players byeCandidate
    let paired =
        match tryGroupedPairings pairingPlayers seedMap scoreFor priorPairs with
        | Some p -> p
        | None ->
            failwith "Swiss pairing failed: no valid non-repeat pairings found for this round."
    let withBye =
        match byePlayer with
        | Some bye -> paired @ [ bye, byeSentinel (), scoreFor bye.Name ]
        | None -> paired
    let seedFor name =
        seedMap |> Map.tryFind name |> Option.defaultValue Int32.MaxValue
    withBye
    |> List.sortBy (fun (a, b, score) ->
        let seedA = seedFor a.Name
        let seedB = seedFor b.Name
        let minSeed = if seedA < seedB then seedA else seedB
        score, -minSeed)
    |> List.map (fun (a, b, _) -> a, b)

/// Compute Swiss pairings for a single round. Tries the score-group
/// top-vs-bottom strategy first; falls back to a score-diff-minimizing
/// relaxation if the primary strategy can't satisfy the no-rematch rule.
/// Raises if no valid pairing exists.
///
/// Returns a list of (White-first, Black-first) pairs, with the optional
/// BYE pair appended at the end (opponent = `{ Empty with Name = "BYE" }`).
let pairNextRound
    (players: EngineConfig list)
    (seedOrder: EngineConfig list)
    (scores: Map<string, float>)
    (priorPairs: Set<string>)
    (byeSet: Set<string>)
    : (EngineConfig * EngineConfig) list
    =
    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList
    let scoreFor name =
        scores |> Map.tryFind name |> Option.defaultValue 0.0
    let byeCandidate = chooseByePlayer players seedMap scoreFor byeSet
    let pairingPlayers, byePlayer = partitionBye players byeCandidate

    let paired =
        tryGroupedPairings pairingPlayers seedMap scoreFor priorPairs
        |> Option.orElseWith (fun () ->
            tryFallbackPairings pairingPlayers seedMap scoreFor priorPairs)
        |> Option.defaultWith (fun () ->
            failwith "Swiss pairing failed: no valid non-repeat pairings found for this round.")

    // Include the bye in the sort so it lands naturally by score rather than
    // always at the end. This matches the original grouped-strategy behavior
    // where the bye pair was added into the collection before sorting.
    let withBye =
        match byePlayer with
        | Some bye ->
            paired @ [ bye, byeSentinel (), scoreFor bye.Name ]
        | None -> paired

    let seedFor name =
        seedMap |> Map.tryFind name |> Option.defaultValue Int32.MaxValue
    withBye
    |> List.sortBy (fun (a, b, score) ->
        let seedA = seedFor a.Name
        let seedB = seedFor b.Name
        let minSeed = if seedA < seedB then seedA else seedB
        score, -minSeed)
    |> List.map (fun (a, b, _) -> a, b)

// ---------------------------------------------------------------------------
// Match-game generation: turn a single Swiss pair into the PlannedGames that
// constitute the match between them.
// ---------------------------------------------------------------------------

/// Generate the planned games for one Swiss match between `whiteFirst` and
/// `blackFirst`. Alternates colors game-by-game (White, Black, White, ...).
/// Advances through `openings` modulo, starting at `startOpeningIndex`.
/// Returns (gamesList, nextOpeningIndex) so callers can chain match planning.
let generateMatchGames
    (whiteFirst: EngineConfig)
    (blackFirst: EngineConfig)
    (openings: PgnGame list)
    (gamesPerMatch: int)
    (startOpeningIndex: int)
    : PlannedGame list * int
    =
    if openings.IsEmpty then
        [], startOpeningIndex
    else
        let gamesPerPair = Math.Max(1, gamesPerMatch / 2)
        let openingsArr = openings |> List.toArray
        let games = ResizeArray<PlannedGame>()
        let mutable index = startOpeningIndex
        let mutable gameCountForOpening = Map.empty<string, int>
        for _ in 0 .. gamesPerPair - 1 do
            let opening = openingsArr.[index % openingsArr.Length]
            let openingHash = Hash.computeOpeningHashFromGame opening
            let nextSub () =
                let cur =
                    gameCountForOpening
                    |> Map.tryFind openingHash
                    |> Option.defaultValue 0
                gameCountForOpening <- gameCountForOpening.Add(openingHash, cur + 1)
                cur + 1
            for (w, b) in [ whiteFirst, blackFirst; blackFirst, whiteFirst ] do
                let sub = nextSub ()
                let key : GameKey =
                    { OpeningHash = openingHash
                      Fen = opening.GameMetaData.Fen
                      White = w.Name
                      Black = b.Name }
                games.Add
                    { White = w
                      Black = b
                      Opening = opening
                      OpeningHash = openingHash
                      RoundLabel = sprintf "%d.%d" opening.GameNumber sub
                      RoleWhite = Standard
                      RoleBlack = Standard
                      Key = key }
            index <- index + 1
        List.ofSeq games, index
