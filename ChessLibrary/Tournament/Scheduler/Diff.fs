module ChessLibrary.Scheduler.Diff

open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities

// ---------------------------------------------------------------------------
// The one rule for "has this planned game been played?". Every consumer of that question
// goes through here - the runner deciding what to play, the GUI counting what is left, the
// round label counting how often an opening has been used - so they can never disagree.
//
// Each played game offers TWO keys, because there is no single hash that matches every PGN
// we might be resuming from:
//
//  * the hash EngineBattle stored. Authoritative for games this version wrote - it is the
//    pairing's own hash, taken from the opening book.
//  * a hash recomputed from the played game. The only key that can match a PGN written before
//    2026-01-16, whose stored hash was taken from the raw book text and which no current
//    computation reproduces.
//
// Neither alone is enough. Recomputing everything breaks current files whenever the replayed
// opening does not reproduce the book move for move (measured: a 102-ply book opening replayed
// as 100 plies). Trusting the stored tag alone breaks older files. The two key sets are kept
// apart and tried in order, stored first: mixed into one index, a recomputed key could claim a
// game that another planned entry matches exactly, and the wrong opening would be replayed.
//
// A played game answers for ONE planned game. The GUI count used to keep a Set of keys, so a
// key planned twice - the book wrapping when Rounds exceeds the openings - read as fully played
// after one game, and the count shown disagreed with what was played.
//
// Engine names are compared trimmed. Nothing legitimate differs by whitespace, and one of the
// old checks trimmed while the other did not.
// ---------------------------------------------------------------------------

let private trimmed (name: string) = if isNull name then "" else name.Trim()

let private normalise (k: GameKey) : GameKey =
    { k with White = trimmed k.White; Black = trimmed k.Black }

// The recomputed hash of a played game never changes, and the runners ask for it once per
// game they play, against the same objects, for as long as the tournament runs. Keyed on the
// object, so a freshly parsed copy of the same game simply computes its own.
let private recomputedHashes = System.Runtime.CompilerServices.ConditionalWeakTable<PgnGame, string>()

let private recomputedHash (g: PgnGame) : string =
    recomputedHashes.GetValue(g, fun game -> Hash.computeOpeningHashFromGame game)

let private keyOfPlayed (hash: string) (g: PgnGame) : GameKey =
    normalise
        { OpeningHash = if System.String.IsNullOrEmpty hash then g.GameNumber.ToString() else hash
          Fen = g.GameMetaData.Fen
          White = g.GameMetaData.White
          Black = g.GameMetaData.Black }

/// For each plan key, true when NO played game accounts for it. Multiset, two passes.
let private unmatched (planKeys: GameKey[]) (played: PgnGame array) : bool[] =
    let remaining = Array.create planKeys.Length true
    if played.Length = 0 then remaining
    else
        let consumed = Array.zeroCreate<bool> played.Length
        let storedIndex = Dictionary<GameKey, ResizeArray<int>>()
        let recomputedIndex = Dictionary<GameKey, ResizeArray<int>>()
        let add (index: Dictionary<GameKey, ResizeArray<int>>) (key: GameKey) (i: int) =
            match index.TryGetValue key with
            | true, xs -> xs.Add i
            | false, _ -> index.[key] <- ResizeArray [ i ]
        for i in 0 .. played.Length - 1 do
            let g = played.[i]
            let stored = keyOfPlayed g.GameMetaData.OpeningHash g
            add storedIndex stored i
            let recomputed = keyOfPlayed (recomputedHash g) g
            if recomputed <> stored then add recomputedIndex recomputed i
        /// Claim one unconsumed played game matching this key, if there is one.
        let claim (index: Dictionary<GameKey, ResizeArray<int>>) (key: GameKey) =
            match index.TryGetValue key with
            | true, xs ->
                match xs |> Seq.tryFind (fun i -> not consumed.[i]) with
                | Some i -> consumed.[i] <- true; true
                | None -> false
            | false, _ -> false
        let keys = planKeys |> Array.map normalise
        for p in 0 .. keys.Length - 1 do
            if claim storedIndex keys.[p] then remaining.[p] <- false
        for p in 0 .. keys.Length - 1 do
            if remaining.[p] && claim recomputedIndex keys.[p] then remaining.[p] <- false
        remaining

/// Subtract already-played games (by `GameKey`) from a plan. Multiset-style:
/// each played game consumes at most one planned entry. Order of the returned
/// list follows the plan.
///
/// This is the single source of resume semantics - no separate filter or
/// quota pass is needed because the plan is already correctly sized by
/// `Gauntlet.generate`. Any planned game whose `Key` matches an already-played
/// PGN entry is removed; anything left is what remains to be played.
let diff (plan: PlannedGame list) (played: PgnGame array) : PlannedGame list =
    let keys = plan |> List.map (fun p -> p.Key) |> List.toArray
    let left = unmatched keys played
    plan |> List.mapi (fun i p -> i, p) |> List.filter (fun (i, _) -> left.[i]) |> List.map snd

/// The same subtraction for the legacy `Pairing` shape the runners and the GUI hold.
let diffPairings (plan: Pairing list) (played: PgnGame array) : Pairing list =
    let keys =
        plan
        |> List.map (fun p ->
            { OpeningHash = p.OpeningHash
              Fen = p.Opening.GameMetaData.Fen
              White = p.White.Name
              Black = p.Black.Name })
        |> List.toArray
    let left = unmatched keys played
    plan |> List.mapi (fun i p -> i, p) |> List.filter (fun (i, _) -> left.[i]) |> List.map snd

/// How many played games used this opening, under either hash rule. Feeds the round label.
let countPlayedWithOpening (played: PgnGame array) (openingHash: string) : int =
    played
    |> Array.filter (fun g ->
        g.GameMetaData.OpeningHash = openingHash
        || recomputedHash g = openingHash)
    |> Array.length

/// Per-engine quota enforcement for Gauntlet resume. When a new opponent is
/// added mid-tournament, the regenerated plan may schedule games for existing
/// engines at openings they never played (because the slot formula shifts).
/// Without capping by quota, those engines end up with MORE than the intended
/// `rounds × numOpp × factor` games.
///
/// This pass walks pairings in order, tracking per-engine game counts
/// (seeded from already-played PGN) and dropping any pairing that would push
/// either engine past its quota.
///
/// For non-Gauntlet modes, the plan is already correctly sized and this pass
/// is a no-op.
let enforceGameLimits
    (challengers: EngineConfig list)
    (opponents: EngineConfig list)
    (rounds: int)
    (openingsTwice: bool)
    (gamesAlreadyPlayed: PgnGame array)
    (plan: PlannedGame list)
    : PlannedGame list
    =
    let factor = if openingsTwice then 2 else 1
    let challengerQuota = rounds * opponents.Length * factor
    let opponentQuota = rounds * challengers.Length * factor
    let challengerSet = challengers |> List.map (fun e -> e.Name) |> Set.ofList
    let quotaFor (name: string) =
        if Set.contains name challengerSet then challengerQuota else opponentQuota
    let counts = Dictionary<string, int>()
    let getCount name =
        match counts.TryGetValue name with
        | true, n -> n
        | false, _ -> 0
    for g in gamesAlreadyPlayed do
        counts.[g.GameMetaData.White] <- getCount g.GameMetaData.White + 1
        counts.[g.GameMetaData.Black] <- getCount g.GameMetaData.Black + 1
    [ for p in plan do
        let wCount = getCount p.White.Name
        let bCount = getCount p.Black.Name
        if wCount < quotaFor p.White.Name && bCount < quotaFor p.Black.Name then
            yield p
            counts.[p.White.Name] <- wCount + 1
            counts.[p.Black.Name] <- bCount + 1 ]

/// Convert a scheduler `PlannedGame` to the legacy `Pairing` shape consumed by
/// the existing tournament runners. The runner later overrides `RoundNr` at
/// play time with a counter-based label; `GameNr` is assigned sequentially by
/// the caller. We pass sensible defaults here.
let toPairing (index: int) (p: PlannedGame) : Pairing =
    { Opening = p.Opening
      White = p.White
      Black = p.Black
      GameNr = index + 1
      RoundNr = p.RoundLabel
      OpeningHash = p.OpeningHash }

/// Convert a list of `PlannedGame` to `Pairing list` with 1-based sequential
/// `GameNr`, matching the legacy `... |> List.mapi (fun i e -> {e with GameNr = i + 1})`
/// convention used throughout the runners.
let toPairings (plan: PlannedGame list) : Pairing list =
    plan |> List.mapi toPairing

// ---------------------------------------------------------------------------
// Pair-based round-label assignment
// ---------------------------------------------------------------------------
//
// Labels are `{pairIdx}.{colorIdx}` where:
//   * pairIdx  — a 1-based sequential counter that increments per (pair,
//                opening) combo. Every `gamesPerPair` games share the same
//                pairIdx; the color differentiates them.
//   * colorIdx — 1 or 2 (under `OpeningsTwice`), always 1 with SingleRound.
//
// The scheduler emits games in pair-consecutive order, so with `pos` counted
// in games from the start of the tournament:
//   pairIdx  = pos / gamesPerPair + 1
//   colorIdx = pos % gamesPerPair + 1
//
// The offset is therefore games already played, not pairs. Dividing first
// truncated a half-finished pair away: resuming on an odd game count put the
// remaining half of that pair at `n.1` instead of `n.2`, and every label after
// it was one game behind for the rest of the run.

/// Rewrite every `RoundLabel` in a plan using the pair-based scheme.
/// `priorGames` is how many games precede this plan (0 for a full schedule,
/// the played-game count when labelling the remainder on resume). The plan
/// must already be in pair-consecutive order — the two colors of a pair must
/// be adjacent when `gamesPerPair = 2`.
let applyPairLabels
    (priorGames: int)
    (gamesPerPair: int)
    (plan: PlannedGame list)
    : PlannedGame list
    =
    let gamesPerPair = max 1 gamesPerPair
    let priorGames = max 0 priorGames
    plan
    |> List.mapi (fun i p ->
        let pos = priorGames + i
        let pairIdx = pos / gamesPerPair + 1
        let colorIdx = pos % gamesPerPair + 1
        { p with RoundLabel = sprintf "%d.%d" pairIdx colorIdx })
