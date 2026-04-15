module ChessLibrary.Scheduler.Diff

open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes

/// Subtract already-played games (by `GameKey`) from a plan. Multiset-style:
/// each played game consumes at most one planned entry. Order of the returned
/// list preserves the scheduler's original order.
///
/// This is the single source of resume semantics — no separate filter or
/// quota pass is needed because the plan is already correctly sized by
/// `Gauntlet.generate`. Any planned game whose `Key` matches an already-played
/// PGN entry is removed; anything left is what remains to be played.
let diff (plan: PlannedGame list) (played: PgnGame array) : PlannedGame list =
    if Array.isEmpty played then
        plan
    else
        let counts = Dictionary<GameKey, int>()
        for g in played do
            let key : GameKey =
                { OpeningHash =
                    if System.String.IsNullOrEmpty g.GameMetaData.OpeningHash
                    then g.GameNumber.ToString()
                    else g.GameMetaData.OpeningHash
                  Fen = g.GameMetaData.Fen
                  White = g.GameMetaData.White
                  Black = g.GameMetaData.Black }
            match counts.TryGetValue key with
            | true, n -> counts.[key] <- n + 1
            | false, _ -> counts.[key] <- 1
        [ for p in plan do
            match counts.TryGetValue p.Key with
            | true, n when n > 0 ->
                counts.[p.Key] <- n - 1
            | _ ->
                yield p ]

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
// The scheduler emits games in pair-consecutive order, so:
//   pairIdx  = priorPairs + (positionInPlan / gamesPerPair) + 1
//   colorIdx = (positionInPlan % gamesPerPair) + 1
//
// On resume, `priorPairs = gamesAlreadyPlayed / gamesPerPair`, so the first
// freshly-played pair picks up exactly one past phase-1's last pair.

/// Rewrite every `RoundLabel` in a plan using the pair-based scheme. The
/// plan must already be in pair-consecutive order (the two colors of a
/// pair must be adjacent in the list when `gamesPerPair = 2`).
let applyPairLabels
    (priorPairs: int)
    (gamesPerPair: int)
    (plan: PlannedGame list)
    : PlannedGame list
    =
    let gamesPerPair = max 1 gamesPerPair
    plan
    |> List.mapi (fun i p ->
        let pairIdx = priorPairs + (i / gamesPerPair) + 1
        let colorIdx = (i % gamesPerPair) + 1
        { p with RoundLabel = sprintf "%d.%d" pairIdx colorIdx })
