module LazyPoolTests

open System
open System.Threading.Tasks
open Xunit
open ChessLibrary.GameRunner

// ---------------------------------------------------------------------------
// The engine pool's slot accounting, with ints standing in for engines: at most
// `capacity` instances ever exist, a returned one is reused before a new one is
// spawned, a spawn that throws frees its slot, and teardown gets back what was
// returned.
// ---------------------------------------------------------------------------

[<Fact>]
let ``spawns on demand up to capacity, then waits for a return`` () =
    let pool = LazyPool<int>(2, fun i -> Task.FromResult(i * 10))
    Assert.Equal(0, pool.Borrow().Result)
    Assert.Equal(10, pool.Borrow().Result)
    Assert.Equal(2, pool.Spawned)
    let third = pool.Borrow()
    Assert.False(third.Wait(50))          // nothing to hand out until something comes back
    pool.Return 0
    Assert.Equal(0, third.Result)
    Assert.Equal(2, pool.Spawned)         // still two instances, never a third

[<Fact>]
let ``a returned instance is reused before a new one is spawned`` () =
    let mutable spawns = 0
    let pool = LazyPool<int>(3, fun i -> spawns <- spawns + 1; Task.FromResult i)
    let a = pool.Borrow().Result
    pool.Return a
    let b = pool.Borrow().Result
    Assert.Equal(a, b)
    Assert.Equal(1, spawns)

[<Fact>]
let ``a spawn that throws frees its slot`` () =
    let mutable attempts = 0
    let pool = LazyPool<int>(1, fun i ->
        attempts <- attempts + 1
        if attempts = 1 then failwith "engine binary not found" else Task.FromResult i)
    Assert.ThrowsAny<Exception>(fun () -> pool.Borrow().Result |> ignore) |> ignore
    Assert.Equal(0, pool.Spawned)
    // The slot is free again: this must spawn, not wait forever.
    let again = pool.Borrow()
    Assert.True(again.Wait(1000), "second borrow after a failed spawn must not hang")
    Assert.Equal(0, again.Result)

[<Fact>]
let ``an evicted instance frees its slot for a fresh spawn`` () =
    let mutable spawns = 0
    let pool = LazyPool<int>(1, fun i -> spawns <- spawns + 1; Task.FromResult(spawns * 100))
    let a = pool.Borrow().Result
    pool.Evict a
    Assert.Equal(0, pool.Spawned)
    let b = pool.Borrow()
    Assert.True(b.Wait(1000), "borrow after evict must spawn, not wait")
    Assert.Equal(200, b.Result)
    Assert.Equal(2, spawns)

[<Fact>]
let ``drain returns every instance that was returned`` () =
    let pool = LazyPool<int>(2, Task.FromResult)
    let a = pool.Borrow().Result
    let b = pool.Borrow().Result
    pool.Return a
    pool.Return b
    Assert.Equal<int list>([ 0; 1 ], pool.Drain() |> Array.sort |> Array.toList)

[<Fact>]
let ``an eviction wakes a borrower waiting on a full pool`` () =
    let pool = LazyPool<int>(1, Task.FromResult)
    let a = pool.Borrow().Result
    let waiter = pool.Borrow()
    Assert.False(waiter.Wait(50))         // full, nothing returned: waits
    pool.Evict a                          // the borrower of `a` stopped it instead of returning it
    Assert.True(waiter.Wait(1000), "the waiter must be woken by the freed slot, not wait for a return")
    Assert.Equal(0, waiter.Result)        // spawned fresh into the freed slot
    Assert.Equal(1, pool.Spawned)

[<Fact>]
let ``a return still wakes a waiter`` () =
    let pool = LazyPool<int>(1, fun i -> Task.FromResult(i * 10))
    let a = pool.Borrow().Result
    let waiter = pool.Borrow()
    Assert.False(waiter.Wait(50))
    pool.Return a
    Assert.True(waiter.Wait(1000))
    Assert.Equal(a, waiter.Result)
    Assert.Equal(1, pool.Spawned)

// ---------------------------------------------------------------------------
// PoolMachine, the pure step behind it.
// ---------------------------------------------------------------------------

open ChessLibrary.PoolMachine

let private run state events = events |> List.fold (fun (s, _) e -> step s e) (state, [])

[<Fact>]
let ``a respawn after an eviction takes the free slot, never a live instance's`` () =
  // slots 0, 1, 2 live; 1 is evicted: the next spawn is slot 1, so its GPU is no one else's
  let s, _ = run (initial 3) [ Borrow 1; Spawned (0, "a"); Borrow 2; Spawned (1, "b"); Borrow 3; Spawned (2, "c"); Evict 1 ]
  let _, e = step s (Borrow 4)
  Assert.Equal<Effect<string> list>([ Spawn 1 ], e)

[<Fact>]
let ``a waiter gets the next return, then the next freed slot`` () =
  let s, _ = run (initial 1) [ Borrow 1; Spawned (0, "a"); Borrow 2; Borrow 3 ]
  let s, e = step s (Return (0, "a"))
  Assert.Equal<Effect<string> list>([ Give (2, 0, "a") ], e)
  let _, e = step s (Evict 0)
  Assert.Equal<Effect<string> list>([ Spawn 0 ], e)

[<Fact>]
let ``a failed spawn refuses its borrower and serves the next waiter`` () =
  let ex = exn "no binary"
  let s, _ = run (initial 1) [ Borrow 1; Borrow 2 ]
  let s, e = step s (SpawnFailed (0, ex))
  Assert.Equal<Effect<string> list>([ Refuse (1, ex); Spawn 0 ], e)
  Assert.Equal<int list>([], s.Waiting)

[<Fact>]
let ``drain releases the idle instances and refuses waiters and later borrows`` () =
  let s, _ = run (initial 1) [ Borrow 1; Spawned (0, "a"); Return (0, "a") ]
  let s, e = step s Drain
  Assert.Equal<Effect<string> list>([ Release [ "a" ] ], e)
  let _, e = step s (Borrow 2)
  match e with
  | [ Refuse (2, _) ] -> ()
  | other -> failwithf "%A" other

[<Fact>]
let ``shed releases the idle instances and frees their slots`` () =
  let s, _ = run (initial 2) [ Borrow 1; Spawned (0, "a"); Borrow 2; Spawned (1, "b"); Return (0, "a") ]
  let s, e = step s Shed
  Assert.Equal<Effect<string> list>([ Release [ "a" ] ], e)
  Assert.Equal<Set<int>>(Set.ofList [ 1 ], s.Taken)       // "b" is still out on loan
  let _, e = step s (Borrow 3)
  Assert.Equal<Effect<string> list>([ Spawn 0 ], e)        // a fresh one into the freed slot

[<Fact>]
let ``a shed pool spawns afresh and the shed instance is never handed out again`` () =
  let mutable spawns = 0
  let pool = LazyPool<int>(1, fun _ -> spawns <- spawns + 1; Task.FromResult(spawns * 100))
  let a = pool.Borrow().Result
  pool.Return a
  Assert.Equal<int list>([ 100 ], pool.Shed().Result |> Array.toList)
  Assert.Equal(0, pool.Spawned)
  Assert.Equal(200, pool.Borrow().Result)
  Assert.Equal<int list>([], pool.Drain() |> Array.toList)  // 200 is out on loan, 100 is gone

// ---------------------------------------------------------------------------
// One board end to end: the schedule of the run that hung once (sf2 never respawned at its
// game 5), through the real pools and GameRunner.shedIdleExcept, many times over.
// ---------------------------------------------------------------------------

open System.Threading

[<Fact>]
let ``one board: the hung run's schedule never waits on a pool and never hands out a stopped engine`` () =
    let names = [ "sf1"; "sf2"; "sf3" ]
    let games = [ "sf1", "sf2"; "sf2", "sf1"; "sf3", "sf1"; "sf1", "sf3"; "sf2", "sf3"; "sf3", "sf2" ]
    for _ in 1 .. 200 do
        let next = ref 0
        let live = System.Collections.Concurrent.ConcurrentDictionary<int, string>()
        let pools =
            names
            |> List.map (fun n ->
                n, LazyPool<int>(1, fun _ -> task {
                    do! Task.Yield()                       // a real start awaits, so this one does too
                    let id = Interlocked.Increment(&next.contents)
                    live.[id] <- n
                    return id }))
            |> Map.ofList
        for white, black in games @ games do
            (shedIdleExcept pools [ white; black ] (fun _ id -> task { live.TryRemove id |> ignore })).Wait()
            // sorted, as GameRunner.Play borrows
            let first, second = if String.CompareOrdinal(white, black) <= 0 then white, black else black, white
            let a = pools.[first].Borrow()
            Assert.True(a.Wait 2000, sprintf "borrow of %s waited" first)
            let b = pools.[second].Borrow()
            Assert.True(b.Wait 2000, sprintf "borrow of %s waited" second)
            Assert.True(live.ContainsKey a.Result && live.ContainsKey b.Result, "a stopped engine was handed out")
            // only the game's two engines are running
            Assert.Equal<string list>(List.sort [ white; black ], live.Values |> Seq.sort |> List.ofSeq)
            pools.[first].Return a.Result
            pools.[second].Return b.Result
