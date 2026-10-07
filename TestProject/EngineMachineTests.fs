module EngineMachineTests

open Xunit
open ChessLibrary.EngineMachine

// ---------------------------------------------------------------------------
// The run's engines as a pure step: strings stand in for engines ("sf1#0" = sf1's instance 0).
// ---------------------------------------------------------------------------

let private policy capacity oneBoard = { Capacity = capacity; OneBoard = oneBoard; StopBeforeStart = false }

let private req game white black = Request { Game = game; White = white; Black = black }

let private run state events = events |> List.fold (fun (s, _) e -> step s e) (state, [])

/// Starts every pending Start with "name#id" as the engine, as the agent would.
let private startAll (st: State<string>, effects: Effect<string> list) =
  effects
  |> List.fold (fun (st, acc) e ->
      match e with
      | Start (id, name, _) ->
          let st, more = step st (Started (id, sprintf "%s#%d" name id))
          st, acc @ more
      | other -> st, acc @ [ other ]) (st, [])

let private grants effects = effects |> List.choose (function Grant (g, w, b) -> Some (g, w, b) | _ -> None)
let private stops effects = effects |> List.choose (function Stop (_, item) -> Some item | _ -> None)
let private starts effects = effects |> List.choose (function Start (_, name, slot) -> Some (name, slot) | _ -> None)

[<Fact>]
let ``a game is granted both engines at once, started on demand`` () =
  let st, effects = step (initial (policy 1 true)) (req 1 "sf1" "sf2")
  Assert.Equal<(string * int) list>([ "sf1", 0; "sf2", 0 ], starts effects)
  Assert.Empty(grants effects)                            // not before both have started
  let _, effects = startAll (st, effects)
  Assert.Equal<(int * string * string) list>([ 1, "sf1#0", "sf2#1" ], grants effects)

[<Fact>]
let ``one board: the hung run's game 4 to game 5 stops the idle engine the game does not play and starts the one it does`` () =
  // games 1-4 of the run that hung: sf1-sf2, sf2-sf1, sf3-sf1, sf1-sf3
  let mutable st = initial (policy 1 true)
  let play game white black =
    let s, effects = startAll (step st (req game white black))
    let s = effects |> stops |> List.fold (fun s item ->
              let id = s.Instances |> Map.findKey (fun _ i -> i.Item = Some item)
              fst (step s (Stopped id))) s
    Assert.Equal(1, (grants effects).Length)
    st <- fst (step s (Release (game, None)))
    effects
  play 1 "sf1" "sf2" |> ignore
  play 2 "sf2" "sf1" |> ignore
  let g3 = play 3 "sf3" "sf1"
  Assert.Equal<string list>([ "sf2#1" ], stops g3)         // sf2 is not in game 3
  play 4 "sf1" "sf3" |> ignore
  // game 5: sf2-sf3 - sf1 stops, sf2 starts afresh, and the game is granted
  let st5, effects = step st (req 5 "sf2" "sf3")
  Assert.Equal<string list>([ "sf1#0" ], stops effects)
  Assert.Equal<(string * int) list>([ "sf2", 0 ], starts effects)
  let _, effects = startAll (st5, effects)
  Assert.Equal<(int * string * string) list>([ 5, "sf2#3", "sf3#2" ], grants effects)

[<Fact>]
let ``several boards: a request waits at capacity and is granted when a game releases`` () =
  let st, effects = startAll (step (initial (policy 1 false)) (req 1 "a" "b"))
  Assert.Equal(1, (grants effects).Length)
  let st, effects = step st (req 2 "a" "c")                // a's only instance is playing
  Assert.Empty(grants effects)
  let st, effects = startAll (st, effects)                  // c starts, a still busy
  Assert.Empty(grants effects)
  let _, effects = step st (Release (1, Some (Set.ofList [ "a" ])))
  Assert.Equal<(int * string * string) list>([ 2, "a#0", "c#2" ], grants effects)
  Assert.Equal<string list>([ "b#1" ], stops effects)       // b is needed by none of the next games

[<Fact>]
let ``the oldest request takes a free engine first, so a younger one cannot starve it`` () =
  // capacity 1: game 1 plays a-b; game 2 wants a-c, game 3 wants a-d
  let st, _ = startAll (step (initial (policy 1 false)) (req 1 "a" "b"))
  let st, e2 = startAll (step st (req 2 "a" "c"))
  let st, e3 = startAll (step st (req 3 "a" "d"))
  Assert.Empty(grants (e2 @ e3))
  let st, effects = step st (Release (1, None))
  Assert.Equal<int list>([ 2 ], grants effects |> List.map (fun (g, _, _) -> g))
  let _, effects = step st (Release (2, None))
  Assert.Equal<int list>([ 3 ], grants effects |> List.map (fun (g, _, _) -> g))

[<Fact>]
let ``a side that fails to start refuses the request and frees the other side`` () =
  let st, effects = step (initial (policy 1 false)) (req 1 "a" "b")
  let ids = effects |> List.choose (function Start (id, name, _) -> Some (name, id) | _ -> None) |> Map.ofList
  let st, _ = step st (Started (ids.["a"], "a#0"))
  let ex = exn "no binary"
  let st, effects = step st (StartFailed (ids.["b"], ex))
  Assert.Equal<Effect<string> list>([ Refuse (1, "b", ex) ], effects)
  Assert.Equal(Idle, st.Instances.[ids.["a"]].Status)       // a is free again, not leaked
  Assert.False(st.Instances.ContainsKey ids.["b"])          // b's slot is free
  let _, effects = startAll (step st (req 2 "a" "b"))       // a later game starts b afresh
  Assert.Equal<(int * string * string) list>([ 2, "a#0", "b#2" ], grants effects)

[<Fact>]
let ``stop-before-start: a new engine waits until a stopping one has stopped`` () =
  let p = { policy 1 true with StopBeforeStart = true }
  let st, _ = startAll (step (initial p) (req 1 "a" "b"))
  let st, _ = step st (Release (1, None))
  let st, effects = step st (req 2 "c" "d")                // a and b stop, c and d must wait
  Assert.Equal(2, (stops effects).Length)
  Assert.Empty(starts effects)
  let stoppingIds = st.Instances |> Map.filter (fun _ i -> i.Status = Stopping) |> Map.keys |> List.ofSeq
  let st, effects = step st (Stopped stoppingIds.[0])
  Assert.Empty(starts effects)                              // one still stopping
  let _, effects = step st (Stopped stoppingIds.[1])
  Assert.Equal<(string * int) list>([ "c", 0; "d", 0 ], starts effects)

[<Fact>]
let ``shutdown refuses waiting requests, stops every engine and is drained when the last has stopped`` () =
  let st, _ = startAll (step (initial (policy 1 false)) (req 1 "a" "b"))   // playing
  let st, _ = step st (req 2 "c" "d")                                      // c, d starting
  let st, effects = step st Shutdown
  Assert.True(effects |> List.exists (function Refuse (2, _, _) -> true | _ -> false))
  Assert.Empty(stops effects)                                // a, b play on; c, d not started yet
  let cId = st.Instances |> Map.findKey (fun _ i -> i.Name = "c")
  let st, effects = step st (Started (cId, "c#2"))          // arrives after the shutdown: stopped
  Assert.Equal<string list>([ "c#2" ], stops effects)
  let dId = st.Instances |> Map.findKey (fun _ i -> i.Name = "d")
  let st, _ = step st (StartFailed (dId, exn "late"))
  let st, effects = step st (Release (1, None))             // the last game ends: its engines stop
  Assert.Equal<string list>([ "a#0"; "b#1" ], stops effects |> List.sort)
  let st, e1 = step st (Stopped cId)
  let ids = st.Instances |> Map.keys |> List.ofSeq
  let st, e2 = step st (Stopped ids.[0])
  let _, e3 = step st (Stopped ids.[1])
  Assert.Empty(e1 @ e2)
  Assert.Equal<Effect<string> list>([ Drained ], e3)
  // and a request after the shutdown is refused at once
  let _, effects = step st (req 9 "a" "b")
  Assert.True(effects |> List.exists (function Refuse (9, _, _) -> true | _ -> false))

[<Fact>]
let ``an engine's instances get the lowest free slot, so a respawn takes the GPU of the one it replaces`` () =
  let st, _ = startAll (step (initial (policy 2 false)) (req 1 "a" "b"))     // a in slot 0
  let st, effects = step st (req 2 "a" "c")
  Assert.Contains(("a", 1), starts effects)                                   // a second a: slot 1
  let st, _ = startAll (st, effects)
  let st, _ = step st (Release (1, Some Set.empty))                           // game 1's a stops
  let stopped = st.Instances |> Map.findKey (fun _ i -> i.Name = "a" && i.Status = Stopping)
  let st, _ = step st (Stopped stopped)
  let _, effects = step st (req 3 "a" "d")
  Assert.Contains(("a", 0), starts effects)                                   // slot 0 again

[<Fact>]
let ``several boards: a released engine a waiting game needs is kept and handed over, not stopped`` () =
  let st, _ = startAll (step (initial (policy 1 false)) (req 1 "a" "b"))
  let st, effects = startAll (step st (req 2 "b" "c"))      // waits for b, which game 1 plays
  Assert.Empty(grants effects)
  // the forecast does not know game 2 (already taken): b would have been stopped
  let _, effects = step st (Release (1, Some Set.empty))
  Assert.Equal<string list>([ "a#0" ], stops effects)
  Assert.Equal<(int * string * string) list>([ 2, "b#1", "c#2" ], grants effects)

[<Fact>]
let ``an engine that arrives after its game was refused is free, and the next game sheds it`` () =
  let st, effects = step (initial (policy 1 true)) (req 1 "a" "b")
  let ids = effects |> List.choose (function Start (id, name, _) -> Some (name, id) | _ -> None) |> Map.ofList
  let st, _ = step st (StartFailed (ids.["b"], exn "no binary"))   // game 1 refused
  let st, _ = step st (Started (ids.["a"], "a#0"))                 // a arrives afterwards
  Assert.Equal(Idle, st.Instances.[ids.["a"]].Status)
  let _, effects = step st (req 2 "c" "d")
  Assert.Equal<string list>([ "a#0" ], stops effects)
