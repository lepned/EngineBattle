/// The engine pool's bookkeeping, as a pure step function: state + event -> state + effects.
/// LazyPool runs it in an agent; spawning happens outside, its outcome comes back as an event.
module ChessLibrary.PoolMachine

/// A slot is an instance's index: spawned with it, and its GPU follows from it.
type State<'T> =
  { Capacity: int
    /// Returned instances, oldest first.
    Idle: (int * 'T) list
    /// Slots taken: on loan, idle, or being spawned.
    Taken: Set<int>
    /// The borrower a slot is being spawned for.
    Spawning: Map<int, int>
    /// Borrowers waiting on a full pool, first come first served.
    Waiting: int list
    Drained: bool }

type Event<'T> =
  | Borrow of id: int
  | Spawned of slot: int * 'T
  | SpawnFailed of slot: int * exn
  | Return of slot: int * 'T
  /// The instance was stopped instead of returned.
  | Evict of slot: int
  | Drain

type Effect<'T> =
  | Spawn of slot: int
  | Give of id: int * slot: int * 'T
  | Refuse of id: int * exn
  /// The idle instances, for teardown.
  | Release of 'T list

let initial capacity =
  { Capacity = max 1 capacity; Idle = []; Taken = Set.empty; Spawning = Map.empty; Waiting = []; Drained = false }

/// The lowest free slot: a respawn after an eviction gets an index no live instance holds.
let private freeSlot (state: State<'T>) =
  Seq.init state.Capacity id |> Seq.tryFind (fun s -> not (state.Taken.Contains s))

let private spawnFor id slot (state: State<'T>) =
  { state with Taken = state.Taken.Add slot; Spawning = state.Spawning.Add(slot, id) }, [ Spawn slot ]

/// A freed slot goes to the first waiter.
let private serveWaiter (state: State<'T>) =
  match state.Waiting, freeSlot state with
  | id :: rest, Some slot -> spawnFor id slot { state with Waiting = rest }
  | _ -> state, []

let private afterDrain () = System.InvalidOperationException "LazyPool: borrow after Drain" :> exn

let step (state: State<'T>) (event: Event<'T>) : State<'T> * Effect<'T> list =
  match event with
  | Borrow id when state.Drained -> state, [ Refuse (id, afterDrain ()) ]
  | Borrow id ->
      match state.Idle, freeSlot state with
      | (slot, item) :: rest, _ -> { state with Idle = rest }, [ Give (id, slot, item) ]
      | [], Some slot -> spawnFor id slot state
      | [], None -> { state with Waiting = state.Waiting @ [ id ] }, []

  | Spawned (slot, item) ->
      match state.Spawning.TryFind slot with
      | Some id -> { state with Spawning = state.Spawning.Remove slot }, [ Give (id, slot, item) ]
      | None -> state, []

  | SpawnFailed (slot, ex) ->
      match state.Spawning.TryFind slot with
      | Some id ->
          let state = { state with Spawning = state.Spawning.Remove slot; Taken = state.Taken.Remove slot }
          let state, next = serveWaiter state
          state, Refuse (id, ex) :: next
      | None -> state, []

  | Return (slot, item) ->
      match state.Waiting with
      // after teardown began the run's safety net stops it
      | _ when state.Drained -> state, []
      | id :: rest -> { state with Waiting = rest }, [ Give (id, slot, item) ]
      | [] -> { state with Idle = state.Idle @ [ slot, item ] }, []

  | Evict slot -> serveWaiter { state with Taken = state.Taken.Remove slot }

  | Drain ->
      let refused = state.Waiting |> List.map (fun id -> Refuse (id, afterDrain ()))
      { state with Drained = true; Idle = []; Waiting = [] }, refused @ [ Release (state.Idle |> List.map snd) ]
