/// A run's engines as a pure step function: state + event -> state + effects; EngineAgent runs
/// it. One owner for every engine instance of the run: a game asks for its pair and is granted
/// both at once; instances start on demand, are kept or stopped by one policy, and count against
/// their engine's capacity until they have stopped.
module ChessLibrary.EngineMachine

type Status =
  /// Being started for this game's request.
  | Starting of game: int
  /// Ready and held for this game, which still waits for its other engine.
  | Reserved of game: int
  | Playing of game: int
  | Idle
  | Stopping

type Instance<'T> =
  { Id: int
    Name: string
    /// The instance's index among its engine's instances; its GPU follows from it.
    Slot: int
    Status: Status
    Item: 'T option }

type Request = { Game: int; White: string; Black: string }

type Policy =
  { /// Instances of one engine at once: the boards.
    Capacity: int
    /// One board: before a game, the idle engines it does not play are stopped.
    OneBoard: bool
    /// No instance starts while another is stopping (GPU memory for network engines).
    StopBeforeStart: bool }

type State<'T> =
  { Policy: Policy
    Instances: Map<int, Instance<'T>>
    /// Requests not granted yet, oldest first: the oldest takes what is free first.
    Waiting: Request list
    NextId: int
    ShuttingDown: bool
    Drained: bool }

type Event<'T> =
  | Request of Request
  | Started of id: int * 'T
  | StartFailed of id: int * exn
  /// The game ended. `keep`: the engines needed soon (several boards), the others are stopped;
  /// None keeps them all (one board stops what the next game does not play when it asks).
  | Release of game: int * keep: Set<string> option
  | Stopped of id: int
  | Shutdown

type Effect<'T> =
  | Start of id: int * name: string * slot: int
  | Stop of id: int * 'T
  | Grant of game: int * white: 'T * black: 'T
  | Refuse of game: int * name: string * exn
  /// Shutting down and every instance has stopped.
  | Drained

let initial policy =
  { Policy = { policy with Capacity = max 1 policy.Capacity }
    Instances = Map.empty; Waiting = []; NextId = 0; ShuttingDown = false; Drained = false }

let private put (inst: Instance<'T>) (st: State<'T>) = { st with Instances = st.Instances.Add(inst.Id, inst) }

let private instancesOf name (st: State<'T>) =
  st.Instances |> Map.toList |> List.map snd |> List.filter (fun i -> i.Name = name)

/// Stops an instance that has its engine; one still starting is stopped when it arrives.
let private stopping (inst: Instance<'T>) (st: State<'T>, effects: Effect<'T> list) =
  match inst.Item with
  | Some item -> put { inst with Status = Stopping } st, effects @ [ Stop (inst.Id, item) ]
  | None -> st, effects

let private holds game name (st: State<'T>) =
  instancesOf name st
  |> List.tryFind (fun i -> i.Status = Starting game || i.Status = Reserved game)

/// One side of a request: hold an instance of `name` for it, or start one, or wait.
let private take (r: Request) name (st: State<'T>, effects: Effect<'T> list) =
  match holds r.Game name st with
  | Some _ -> st, effects
  | None ->
      let mine = instancesOf name st
      match mine |> List.filter (fun i -> i.Status = Idle) |> List.sortBy _.Id with
      | idle :: _ -> put { idle with Status = Reserved r.Game } st, effects
      | [] ->
          let anyStopping = st.Instances |> Map.exists (fun _ i -> i.Status = Stopping)
          if mine.Length < st.Policy.Capacity && not (st.Policy.StopBeforeStart && anyStopping) then
            let used = mine |> List.map _.Slot |> Set.ofList
            let slot = Seq.initInfinite id |> Seq.find (fun s -> not (used.Contains s))
            let inst = { Id = st.NextId; Name = name; Slot = slot; Status = Starting r.Game; Item = None }
            put inst { st with NextId = st.NextId + 1 }, effects @ [ Start (inst.Id, name, slot) ]
          else st, effects

/// Serves the waiting requests, oldest first: each holds what it can; one holding both
/// engines ready is granted.
let private serve (st: State<'T>, effects: Effect<'T> list) =
  if st.ShuttingDown then st, effects
  else
    st.Waiting
    |> List.fold (fun (st: State<'T>, effects) r ->
        let st, effects = (st, effects) |> take r r.White |> take r r.Black
        match holds r.Game r.White st, holds r.Game r.Black st with
        | Some ({ Status = Reserved _; Item = Some w } as wi), Some ({ Status = Reserved _; Item = Some b } as bi) ->
            let st = st |> put { wi with Status = Playing r.Game } |> put { bi with Status = Playing r.Game }
            { st with Waiting = st.Waiting |> List.filter (fun x -> x.Game <> r.Game) }, effects @ [ Grant (r.Game, w, b) ]
        | _ -> st, effects) (st, effects)

let private drainCheck (st: State<'T>, effects: Effect<'T> list) =
  if st.ShuttingDown && not st.Drained && st.Instances.IsEmpty then { st with Drained = true }, effects @ [ Drained ]
  else st, effects

/// A request that cannot be served any more: refused; what it held is free again.
let private refuse game name (ex: exn) (st: State<'T>, effects: Effect<'T> list) =
  let st =
    st.Instances
    |> Map.fold (fun st _ i -> if i.Status = Reserved game then put { i with Status = Idle } st else st) st
  { st with Waiting = st.Waiting |> List.filter (fun r -> r.Game <> game) }, effects @ [ Refuse (game, name, ex) ]

let private shutDownError () = System.InvalidOperationException "The run's engines are shut down" :> exn

let step (st: State<'T>) (event: Event<'T>) : State<'T> * Effect<'T> list =
  match event with
  | Request r when st.ShuttingDown -> st, [ Refuse (r.Game, "", shutDownError ()) ]
  | Request r ->
      let st = { st with Waiting = st.Waiting @ [ r ] }
      let shed =
        if not st.Policy.OneBoard then (st, [])
        else
          st.Instances |> Map.toList |> List.map snd
          |> List.filter (fun i -> i.Status = Idle && i.Name <> r.White && i.Name <> r.Black)
          |> List.fold (fun acc i -> stopping i acc) (st, [])
      serve shed

  | Started (id, item) ->
      match st.Instances.TryFind id with
      | Some ({ Status = Starting game } as inst) ->
          let inst = { inst with Item = Some item }
          if st.ShuttingDown then stopping inst (put inst st, []) |> drainCheck
          elif st.Waiting |> List.exists (fun r -> r.Game = game) then serve (put { inst with Status = Reserved game } st, [])
          // its request was refused meanwhile: free for the next one
          else serve (put { inst with Status = Idle } st, [])
      | _ -> st, [ Stop (id, item) ]

  | StartFailed (id, ex) ->
      match st.Instances.TryFind id with
      | Some ({ Status = Starting game } as inst) ->
          let st = { st with Instances = st.Instances.Remove id }
          let acc =
            if st.Waiting |> List.exists (fun r -> r.Game = game) then refuse game inst.Name ex (st, [])
            else st, []
          acc |> serve |> drainCheck
      | _ -> st, []

  | Release (game, keep) ->
      let played = st.Instances |> Map.toList |> List.map snd |> List.filter (fun i -> i.Status = Playing game)
      let acc =
        played |> List.fold (fun (st, effects) i ->
          let idle = { i with Status = Idle }
          // a waiting game's engine is kept too: the forecast cannot see a pairing already taken
          let wanted = st.Waiting |> List.exists (fun r -> r.White = i.Name || r.Black = i.Name)
          let keepIt = not st.ShuttingDown && (wanted || match keep with Some names -> names.Contains i.Name | None -> true)
          if keepIt then put idle st, effects else stopping idle (put idle st, effects)) (st, [])
      acc |> serve |> drainCheck

  | Stopped id ->
      ({ st with Instances = st.Instances.Remove id }, []) |> serve |> drainCheck

  | Shutdown when st.ShuttingDown -> drainCheck (st, [])
  | Shutdown ->
      let st = { st with ShuttingDown = true }
      let acc =
        st.Waiting |> List.fold (fun acc r -> refuse r.Game "" (shutDownError ()) acc) (st, [])
      let st, effects = acc
      let acc =
        st.Instances |> Map.toList |> List.map snd
        |> List.filter (fun i -> i.Status = Idle || (match i.Status with Reserved _ -> true | _ -> false))
        |> List.fold (fun acc i -> stopping i acc) (st, effects)
      drainCheck acc
