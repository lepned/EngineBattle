module ChessLibrary.Scheduler.Cup

open System
open System.Collections.Generic
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

/// Strategy for assigning initial bracket seeds.
type SeedingStrategy =
    | Random
    | ByRating

// ---------------------------------------------------------------------------
// Seeding utilities
// ---------------------------------------------------------------------------

/// TCEC-style "column-major" seed order: players are sorted by rating, then
/// distributed into `groupCount` equal-ish groups. Seed 1 goes to group 0
/// rank 0, seed 2 to group 1 rank 0, etc. — so top seeds cluster early
/// without being adjacent.
let tcecSeedOrder (players: EngineConfig list) (groupCount: int) : EngineConfig list =
    let sorted = players |> List.sortByDescending (fun p -> p.Rating)
    let total = sorted.Length
    if total = 0 then
        []
    else
        let groups = Dictionary<int, ResizeArray<EngineConfig>>()
        let numGroups = max 1 groupCount
        for i in 0 .. numGroups - 1 do
            groups.[i] <- ResizeArray<EngineConfig>()
        let mutable engineIndex = 0
        for group in 0 .. numGroups - 1 do
            let groupSize =
                int (Math.Ceiling(float (total - engineIndex) / float (numGroups - group)))
            for _ in 0 .. groupSize - 1 do
                if engineIndex < total then
                    groups.[group].Add sorted.[engineIndex]
                    engineIndex <- engineIndex + 1
        let ordered = ResizeArray<EngineConfig>()
        let mutable rank = 0
        let mutable seed = 1
        while seed <= total do
            for group in 0 .. numGroups - 1 do
                if rank < groups.[group].Count then
                    ordered.Add groups.[group].[rank]
                    seed <- seed + 1
            rank <- rank + 1
        ordered |> Seq.toList

/// Standard single-elimination bracket seed order (1-16, 8-9, 5-12, 4-13, ...).
/// Doubles the bracket size each pass until >= `size`.
let seedOrder (size: int) : int list =
    let rec build order n =
        if n >= size then
            order
        else
            let nextSize = n * 2
            let expanded = order |> List.collect (fun seed -> [ seed; nextSize + 1 - seed ])
            build expanded nextSize
    if size <= 1 then [ 1 ]
    else build [ 1; 2 ] 2

/// Generate seed-bands by powers of two: [1], [2], [3..4], [5..8], [9..16], ...
/// Used for relaxed-randomized seeding within tiers.
let autoSeedBands (total: int) : int list list =
    if total <= 0 then
        []
    else
        let bands = ResizeArray<int list>()
        let mutable seed = 1
        bands.Add [ seed ]
        seed <- seed + 1
        if seed <= total then
            bands.Add [ seed ]
            seed <- seed + 1
        let mutable bandSize = 2
        while seed <= total do
            let size = min bandSize (total - seed + 1)
            bands.Add [ seed .. seed + size - 1 ]
            seed <- seed + size
            bandSize <- bandSize * 2
        bands |> Seq.toList

/// Assign players to bracket slots via seed bands. Top seeds are placed at
/// their canonical bracket slots; within each band, players can optionally
/// be shuffled. Any unplaced players fill the remaining slots in seed order.
let seedByBands
    (players: EngineConfig list)
    (seedBands: int list list)
    (randomizeWithinBands: bool)
    : EngineConfig list
    =
    let sorted = players |> List.sortByDescending (fun p -> p.Rating)
    let total = sorted.Length
    if total = 0 then
        []
    else
        let order = seedOrder total
        let slots : EngineConfig option array = Array.create total None
        let used = HashSet<int>()
        let tryPlace seedNumber =
            if seedNumber >= 1 && seedNumber <= total && not (used.Contains seedNumber) then
                let slotIndex = order |> List.findIndex (fun s -> s = seedNumber)
                slots.[slotIndex] <- Some sorted.[seedNumber - 1]
                used.Add seedNumber |> ignore
        let tryPlaceBand (band: int list) =
            let seeds =
                band
                |> List.filter (fun s -> s >= 1 && s <= total && not (used.Contains s))
            if seeds.IsEmpty then ()
            elif seeds.Length = 1 || not randomizeWithinBands then
                for s in seeds do tryPlace s
            else
                let slotIndices =
                    seeds |> List.map (fun s -> order |> List.findIndex (fun o -> o = s))
                let seedPlayers = seeds |> List.map (fun s -> sorted.[s - 1])
                let shuffled = seedPlayers |> List.toArray
                ChessUtilities.Random.Shuffle shuffled
                for i in 0 .. slotIndices.Length - 1 do
                    slots.[slotIndices.[i]] <- Some shuffled.[i]
                for s in seeds do used.Add s |> ignore
        for band in seedBands do tryPlaceBand band
        let remaining =
            sorted
            |> List.mapi (fun idx player -> idx + 1, player)
            |> List.filter (fun (s, _) -> not (used.Contains s))
            |> List.map snd
        let mutable remainingIndex = 0
        for i in 0 .. total - 1 do
            if slots.[i].IsNone then
                slots.[i] <- Some remaining.[remainingIndex]
                remainingIndex <- remainingIndex + 1
        slots |> Array.choose id |> Array.toList

// ---------------------------------------------------------------------------
// Games-per-match and opening selection
// ---------------------------------------------------------------------------

/// Compute the number of games in a cup match for a given round. Honors the
/// optional `roundPairIncrements` override (pairs per round, each pair = 2
/// games); falls back to the normalized default `gamesPerMatch` otherwise.
let gamesPerMatchForRound
    (gamesPerMatch: int)
    (roundPairIncrements: int list)
    (roundNumber: int)
    : int
    =
    let normalized =
        if gamesPerMatch < 2 then 2
        elif gamesPerMatch % 2 = 1 then gamesPerMatch + 1
        else gamesPerMatch
    match roundPairIncrements with
    | [] -> normalized
    | _ ->
        let idx = max 0 (roundNumber - 1)
        let pairs =
            if idx < roundPairIncrements.Length then roundPairIncrements.[idx]
            else roundPairIncrements.[roundPairIncrements.Length - 1]
        (max 1 pairs) * 2

/// Walk the openings list from `startIndex`, returning the first index whose
/// opening hash is not in `usedOpeningHashes`. Wraps around via modulo and
/// falls back to `startIndex % total` if every opening is used.
let nextUnusedOpeningIndex
    (usedOpeningHashes: Set<string>)
    (openings: PgnGame list)
    (startIndex: int)
    : int
    =
    if openings.IsEmpty then 0
    else
        let total = openings.Length
        let openings = openings |> List.toArray
        let rec loop offset =
            if offset >= total then startIndex % total
            else
                let idx = (startIndex + offset) % total
                let hash = Hash.computeOpeningHashFromGame openings.[idx]
                if usedOpeningHashes.Contains hash then loop (offset + 1)
                else idx
        loop 0

// ---------------------------------------------------------------------------
// Cup match pairing generation (UI preview + runner-shared)
// ---------------------------------------------------------------------------

/// Build the remaining pairings for an in-progress cup match. Starts from the
/// current (in-flight) opening with the given color order, then chooses fresh
/// openings for subsequent games — preferring unused openings, wrapping with
/// modulo when all are consumed. Returns `Pairing list` for backward compat
/// with the existing UI/preview consumers; see `generateMatchGames` for the
/// scheduler-native `PlannedGame` shape.
let buildRemainingCupPairings
    (matchInfo: CupTypes.CupMatch)
    (playerA: EngineConfig)
    (playerB: EngineConfig)
    (matchOpenings: PgnGame list)
    (currentOpening: PgnGame)
    (currentPlayOrder: (EngineConfig * EngineConfig) list)
    (gamesRemaining: int)
    (localOpeningIndex: int)
    : ResizeArray<Pairing>
    =
    let baseIndex = matchInfo.Games.Count
    let mutable remaining = gamesRemaining
    let mutable nextIndex = localOpeningIndex
    let planned = ResizeArray<Pairing>()
    let addPairing idx white black opening =
        let openingHash = Hash.computeOpeningHashFromGame opening
        planned.Add
            { Opening = opening
              White = white
              Black = black
              GameNr = 0
              RoundNr = sprintf "%d.%d" matchInfo.RoundNumber (baseIndex + idx + 1)
              OpeningHash = openingHash }
    let mutable usedHashes =
        matchInfo.Games |> Seq.map (fun g -> g.OpeningHash) |> Set.ofSeq
    let currentHash = Hash.computeOpeningHashFromGame currentOpening
    usedHashes <- usedHashes.Add currentHash
    let mutable idxOffset = 0
    for (white, black) in currentPlayOrder do
        if remaining > 0 then
            addPairing idxOffset white black currentOpening
            idxOffset <- idxOffset + 1
            remaining <- remaining - 1
    let chooseNextOpening () =
        if List.isEmpty matchOpenings then
            currentOpening
        elif usedHashes.Count < matchOpenings.Length then
            let idx = nextUnusedOpeningIndex usedHashes matchOpenings nextIndex
            nextIndex <- idx + 1
            matchOpenings.[idx]
        else
            let idx = nextIndex % matchOpenings.Length
            nextIndex <- idx + 1
            matchOpenings.[idx]
    while remaining > 0 do
        let opening = chooseNextOpening ()
        let openingHash = Hash.computeOpeningHashFromGame opening
        usedHashes <- usedHashes.Add openingHash
        for (white, black) in [ playerA, playerB; playerB, playerA ] do
            if remaining > 0 then
                addPairing idxOffset white black opening
                idxOffset <- idxOffset + 1
                remaining <- remaining - 1
    planned
