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
// Lookahead: a round is paired only in a way the rounds after it can follow without a rematch.
// One round at a time can pair itself into a corner: with 6 players three rounds can leave two
// triangles of unplayed pairs (A-B-C, D-E-F), from which no fourth round can be paired.
// ---------------------------------------------------------------------------

let private byeName = "BYE"

/// Whether `rounds` more rounds can be paired among `names` without a pair in `used` (an odd
/// field gets a BYE to pair with: a second bye counts as a rematch). A search that runs out of
/// steps counts as yes - it only steers a round, it never blocks one.
let private canPairRounds (names: string list) (used: Set<string>) (rounds: int) =
  let names = if names.Length % 2 = 1 then names @ [ byeName ] else names
  let steps = ref 100_000
  let partnersLeft (used: Set<string>) p = names |> List.filter (fun q -> q <> p && not (used.Contains (pairKey p q))) |> List.length
  let rec roundsLeft k (used: Set<string>) =
    k = 0 || (names |> List.forall (fun p -> partnersLeft used p >= k) && pairUp names used [] k)
  and pairUp left used added k =
    steps.Value <- steps.Value - 1
    if steps.Value < 0 then true
    else
      match left with
      | [] -> roundsLeft (k - 1) (added |> List.fold (fun s key -> Set.add key s) used)
      | p :: rest ->
          rest |> List.exists (fun q ->
            let key = pairKey p q
            not (used.Contains key) && pairUp (rest |> List.filter (fun x -> x <> q)) used (key :: added) k)
  roundsLeft rounds used

/// Every pairing of a round without a rematch, the closest scores first (the fallback order).
let private allPairings (players: EngineConfig list) (seedMap: Map<string, int>) (scoreFor: string -> float) (priorPairs: Set<string>) =
  let rec from (remaining: EngineConfig list) acc =
    seq {
      match remaining with
      | [] -> yield List.rev acc
      | player :: rest ->
          let candidates =
            rest
            |> List.filter (fun opp -> not (priorPairs.Contains (pairKey player.Name opp.Name)))
            |> List.sortBy (fun opp -> abs (scoreFor player.Name - scoreFor opp.Name), seedMap.[opp.Name])
          for opp in candidates do
            yield! from (rest |> List.filter (fun p -> p.Name <> opp.Name)) ((player, opp, scoreFor player.Name) :: acc) }
  from (players |> List.sortBy (fun p -> (-(scoreFor p.Name), seedMap.[p.Name]))) []

/// The players who could sit out, in the order chooseByePlayer prefers: lowest score, weakest seed, no bye yet.
let private byeOrder (players: EngineConfig list) (seedMap: Map<string, int>) (scoreFor: string -> float) (byeSet: Set<string>) =
  if players.Length % 2 = 0 then [ None ]
  else
    let ordered = players |> List.sortBy (fun p -> scoreFor p.Name, -seedMap.[p.Name])
    (ordered |> List.filter (fun p -> not (byeSet.Contains p.Name))) @ (ordered |> List.filter (fun p -> byeSet.Contains p.Name))
    |> List.map Some

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
/// `roundsAfter`: the rounds still to come after this one - a pairing that would leave them no
/// way without a rematch gives way to the closest one that does. Raises if no valid pairing exists.
///
/// Returns a list of (White-first, Black-first) pairs, with the optional
/// BYE pair appended at the end (opponent = `{ Empty with Name = "BYE" }`).
let pairRoundLeaving
    (roundsAfter: int)
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

    let usual =
        tryGroupedPairings pairingPlayers seedMap scoreFor priorPairs
        |> Option.orElseWith (fun () ->
            tryFallbackPairings pairingPlayers seedMap scoreFor priorPairs)
        |> Option.map (fun p -> p, byePlayer)
    let names = players |> List.map _.Name
    let leavesRounds (paired: (EngineConfig * EngineConfig * float) list, bye: EngineConfig option) =
        roundsAfter <= 0
        || (let used = byeSet |> Seq.fold (fun s b -> Set.add (pairKey b byeName) s) priorPairs
            let used = paired |> List.fold (fun s (a, b, _) -> Set.add (pairKey a.Name b.Name) s) used
            let used = match bye with Some b -> Set.add (pairKey b.Name byeName) used | None -> used
            canPairRounds names used roundsAfter)
    let chosen =
        match usual with
        | Some choice when leavesRounds choice -> Some choice
        | _ ->
            byeOrder players seedMap scoreFor byeSet
            |> Seq.collect (fun bye ->
                let rest = match bye with Some b -> players |> List.filter (fun p -> p.Name <> b.Name) | None -> players
                allPairings rest seedMap scoreFor priorPairs |> Seq.truncate 2000 |> Seq.map (fun p -> p, bye))
            |> Seq.tryFind leavesRounds
            |> Option.orElse usual
    let paired, byePlayer =
        chosen |> Option.defaultWith (fun () ->
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

/// `pairRoundLeaving` for a round with none after it (a playoff, or a caller that does not look ahead).
let pairNextRound players seedOrder scores priorPairs byeSet = pairRoundLeaving 0 players seedOrder scores priorPairs byeSet
