module ChessLibrary.ReplayGate

open System.Collections.Generic
open System.Threading.Tasks
open ChessLibrary.TypesDef.CoreTypes

// ---------------------------------------------------------------------------
// Ordering for deviation prevention under parallel play.
//
// With PreventMoveDeviation on, an engine that meets an opening again in the same colour is
// held to the moves it played the first time. The parallel runner shares those moves through
// a dictionary that a game merges into when it FINISHES - so a repeat that starts while its
// predecessor is still being played sees a partial line at best, and when both finish the
// later merge overwrites the earlier one: two games racing to define the same line.
//
// The gate hands out pairings in plan order but never starts one whose replay key is held by
// a game in flight. A key is one side of one game: (opening hash, colour, engine) - exactly
// the match `GameReplay.prepareGameReplay` uses to decide which saved games seed which side.
// Everything without a conflict still runs in parallel; the colour-swapped twin of a game has
// different keys and is not held back.
//
// The parallelism this leaves is bounded by how many distinct keys are ready: in a gauntlet
// with the Shared distribution the challenger has White in every game of an opening, so those
// games serialise. That is what prevention means, not a limitation of the gate.
// ---------------------------------------------------------------------------

/// One side of one game, as deviation prevention identifies it.
type ReplayKey =
    { OpeningHash: string
      Colour: char
      Engine: string }

/// What a worker gets when it asks for the next pairing.
type Take =
    /// Play this one.
    | Start of Pairing
    /// Every pending pairing conflicts with a game in flight. The task completes when any
    /// in-flight game releases its keys; ask again then.
    | Wait of Task
    /// Nothing is pending. Games may still be in flight on other workers.
    | Done

/// The keys a pairing holds while it is played. None when prevention is off, and only the
/// listed engines' sides when `PreventMoveDeviationFor` narrows it.
let keysOf (enabled: bool) (preventFor: string[]) (p: Pairing) : ReplayKey list =
    if not enabled then []
    else
        let applies (name: string) =
            isNull preventFor || preventFor.Length = 0 || Array.contains name preventFor
        [ if applies p.White.Name then { OpeningHash = p.OpeningHash; Colour = 'w'; Engine = p.White.Name }
          if applies p.Black.Name then { OpeningHash = p.OpeningHash; Colour = 'b'; Engine = p.Black.Name } ]

type ReplayGate(plan: Pairing list, enabled: bool, preventFor: string[]) =
    // Keys are fixed per pairing, so compute them once rather than on every scan.
    let pending = ResizeArray<Pairing * ReplayKey list>(plan |> List.map (fun p -> p, keysOf enabled preventFor p))
    let inFlight = HashSet<ReplayKey>()
    let sync = obj()
    // Replaced on every release, so a waiter that captured it inside TryTake is woken by the
    // first release after its decision - there is no gap in which a release can be missed.
    let mutable released = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

    let keys p = keysOf enabled preventFor p

    member _.PendingCount = lock sync (fun () -> pending.Count)
    member _.InFlightCount = lock sync (fun () -> inFlight.Count)

    /// The first pending pairing that can start now, in plan order.
    member _.TryTake() : Take =
        lock sync (fun () ->
            if pending.Count = 0 then Done
            else
                match pending |> Seq.tryFindIndex (fun (_, ks) -> not (ks |> List.exists inFlight.Contains)) with
                | Some i ->
                    let p, ks = pending.[i]
                    pending.RemoveAt i
                    for k in ks do inFlight.Add k |> ignore
                    Start p
                | None -> Wait released.Task)

    /// The game is finished and its moves are merged: free its keys and wake the waiters.
    member _.Release(p: Pairing) =
        lock sync (fun () ->
            for k in keys p do inFlight.Remove k |> ignore
            let toWake = released
            released <- TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
            toWake.TrySetResult() |> ignore)
