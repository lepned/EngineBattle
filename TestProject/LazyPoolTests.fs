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
