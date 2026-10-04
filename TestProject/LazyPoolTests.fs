module LazyPoolTests

open System
open Xunit
open ChessLibrary.ParallelExecution

// ---------------------------------------------------------------------------
// The engine pool's slot accounting, with ints standing in for engines: at most
// `capacity` instances ever exist, a returned one is reused before a new one is
// spawned, a spawn that throws frees its slot, and teardown gets back what was
// returned.
// ---------------------------------------------------------------------------

[<Fact>]
let ``spawns on demand up to capacity, then waits for a return`` () =
    let pool = LazyPool<int>(2, fun i -> i * 10)
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
    let pool = LazyPool<int>(3, fun i -> spawns <- spawns + 1; i)
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
        if attempts = 1 then failwith "engine binary not found" else i)
    Assert.ThrowsAny<Exception>(fun () -> pool.Borrow().Result |> ignore) |> ignore
    Assert.Equal(0, pool.Spawned)
    // The slot is free again: this must spawn, not wait forever.
    let again = pool.Borrow()
    Assert.True(again.Wait(1000), "second borrow after a failed spawn must not hang")
    Assert.Equal(0, again.Result)

[<Fact>]
let ``an evicted instance frees its slot for a fresh spawn`` () =
    let mutable spawns = 0
    let pool = LazyPool<int>(1, fun i -> spawns <- spawns + 1; spawns * 100)
    let a = pool.Borrow().Result
    pool.Evict a
    Assert.Equal(0, pool.Spawned)
    let b = pool.Borrow()
    Assert.True(b.Wait(1000), "borrow after evict must spawn, not wait")
    Assert.Equal(200, b.Result)
    Assert.Equal(2, spawns)

[<Fact>]
let ``drain returns every instance that was returned`` () =
    let pool = LazyPool<int>(2, id)
    let a = pool.Borrow().Result
    let b = pool.Borrow().Result
    pool.Return a
    pool.Return b
    Assert.Equal<int list>([ 0; 1 ], pool.Drain() |> Array.sort |> Array.toList)

[<Fact>]
let ``an eviction wakes a borrower waiting on a full pool`` () =
    let pool = LazyPool<int>(1, fun i -> i)
    let a = pool.Borrow().Result
    let waiter = pool.Borrow()
    Assert.False(waiter.Wait(50))         // full, nothing returned: waits
    pool.Evict a                          // the borrower of `a` stopped it instead of returning it
    Assert.True(waiter.Wait(1000), "the waiter must be woken by the freed slot, not wait for a return")
    Assert.Equal(0, waiter.Result)        // spawned fresh into the freed slot
    Assert.Equal(1, pool.Spawned)

[<Fact>]
let ``a return still wakes a waiter`` () =
    let pool = LazyPool<int>(1, fun i -> i * 10)
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
