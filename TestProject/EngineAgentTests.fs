module EngineAgentTests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open Xunit
open ChessLibrary.EngineMachine
open ChessLibrary.GameRunner

// ---------------------------------------------------------------------------
// EngineAgent with strings for engines: what runs the machine, with starts and stops that take
// real (awaited) time.
// ---------------------------------------------------------------------------

/// An agent whose engines are "name#n"; `live` holds the ones started and not yet stopped, and
/// `crowded` counts starts that found two engines already running.
let private agentWithCount (policy: Policy) (startFails: string -> bool) =
    let live = ConcurrentDictionary<string, string>()
    let crowded = ref 0
    let next = ref 0
    let agent =
        EngineAgent<string>(
            policy,
            (fun name _ -> task {
                if live.Count >= 2 then Interlocked.Increment(&crowded.contents) |> ignore
                do! Task.Yield()
                if startFails name then failwithf "%s will not start" name
                let item = sprintf "%s#%d" name (Interlocked.Increment(&next.contents))
                live.[item] <- name
                return item }),
            (fun item -> task {
                do! Task.Yield()
                live.TryRemove item |> ignore }))
    agent, live, crowded

let private agentWith policy startFails =
    let agent, live, _ = agentWithCount policy startFails
    agent, live

let private granted (t: Task<string * string>) =
    Assert.True(t.Wait 2000, "the request was not granted")
    t.Result

[<Fact>]
let ``one board: the hung run's schedule is always granted and only the game's engines run`` () =
    let games = [ "sf1", "sf2"; "sf2", "sf1"; "sf3", "sf1"; "sf1", "sf3"; "sf2", "sf3"; "sf3", "sf2" ]
    for _ in 1 .. 100 do
        // GameRunner's one-board policy: what a game does not play is gone before anything starts
        let agent, live, crowded = agentWithCount { Capacity = 1; OneBoard = true; StopBeforeStart = true } (fun _ -> false)
        let mutable game = 0
        for white, black in games @ games do
            game <- game + 1
            let w, b = granted (agent.Request(game, white, black))
            Assert.StartsWith(white + "#", w)
            Assert.StartsWith(black + "#", b)
            // a stop of the engine this game does not play may still be on its way: wait for it
            SpinWait.SpinUntil((fun () -> live.Count = 2), 2000) |> ignore
            Assert.Equal<string list>(List.sort [ white; black ], live.Values |> Seq.sort |> List.ofSeq)
            agent.Release(game, None)
        Assert.True(agent.Shutdown().Wait 2000, "shutdown did not drain")
        Assert.Empty(live)
        Assert.Equal(0, crowded.Value)

[<Fact>]
let ``several boards: games wait for each other and are all granted`` () =
    let agent, live = agentWith { Capacity = 2; OneBoard = false; StopBeforeStart = false } (fun _ -> false)
    let names = [| "a"; "b"; "c"; "d" |]
    let rnd = Random 7
    let boards =
        [| for board in 0 .. 1 ->
            task {
                for i in 0 .. 49 do
                    let game = board * 1000 + i
                    let w = names.[rnd.Next names.Length]
                    let b = names |> Array.filter ((<>) w) |> fun r -> r.[rnd.Next r.Length]
                    let! _ = agent.Request(game, w, b).WaitAsync(TimeSpan.FromSeconds 5.0)
                    do! Task.Yield()
                    agent.Release(game, Some (Set.ofList [ "a"; "b" ])) } :> Task |]
    Assert.True(Task.WaitAll(boards, 20000), "a board waited for ever")
    Assert.True(agent.Shutdown().Wait 2000)
    Assert.Empty(live)

[<Fact>]
let ``an engine that fails to start refuses its game by name and leaves the other engine free`` () =
    let agent, live = agentWith { Capacity = 1; OneBoard = false; StopBeforeStart = false } (fun name -> name = "broken")
    let failed = agent.Request(1, "a", "broken")
    let ex = Assert.ThrowsAny<AggregateException>(fun () -> failed.Wait 2000 |> ignore)
    let refused = ex.InnerException :?> EngineRefusedException
    Assert.Equal("broken", refused.Engine)
    // a is free: the next game gets it at once
    let w, _ = granted (agent.Request(2, "a", "b"))
    Assert.StartsWith("a#", w)
    agent.Release(2, None)
    Assert.True(agent.Shutdown().Wait 2000)
    Assert.Empty(live)

[<Fact>]
let ``shutdown refuses a waiting game and stops every engine`` () =
    let agent, live = agentWith { Capacity = 1; OneBoard = false; StopBeforeStart = false } (fun _ -> false)
    granted (agent.Request(1, "a", "b")) |> ignore
    let waiting = agent.Request(2, "a", "c")               // a is playing: waits
    Assert.False(waiting.Wait 100)
    let drained = agent.Shutdown()
    let ex = Assert.ThrowsAny<AggregateException>(fun () -> waiting.Wait 2000 |> ignore)
    Assert.IsType<EngineRefusedException>(ex.InnerException) |> ignore
    agent.Release(1, None)                                   // game 1 ends: its engines stop
    Assert.True(drained.Wait 2000, "shutdown did not drain")
    Assert.Empty(live)
