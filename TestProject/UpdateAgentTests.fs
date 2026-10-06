module UpdateAgentTests

open System
open System.IO
open System.Threading
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.TournamentTypes
open ChessLibrary.Game.UpdateAgent

let private name (u: Update) = match u with GameStarted n -> n | _ -> "?"

[<Fact>]
let ``a game's updates are sent on in the order posted`` () =
  let seen = ResizeArray<string>()
  let agent = UpdateAgent((fun u -> seen.Add(name u)), NullLogger.Instance)
  for i in 1 .. 500 do agent.Post(GameStarted (string i))
  agent.Flush() |> Async.RunSynchronously
  Assert.Equal<string list>([ for i in 1 .. 500 -> string i ], List.ofSeq seen)

[<Fact>]
let ``posting never waits for a slow sink, and Flush waits for all of it`` () =
  let gate = new ManualResetEventSlim(false)
  let seen = Collections.Concurrent.ConcurrentQueue<string>()
  // a sink that waits; it gives up after 3 s, so a caller that waits for it fails instead of hanging
  let agent = UpdateAgent((fun u -> gate.Wait 3000 |> ignore; seen.Enqueue(name u)), NullLogger.Instance)
  let sw = Diagnostics.Stopwatch.StartNew()
  for i in 1 .. 5 do agent.Post(GameStarted (string i))
  Assert.True(sw.ElapsedMilliseconds < 1000L, sprintf "posting waited %d ms" sw.ElapsedMilliseconds)
  let flushed = agent.Flush() |> Async.StartAsTask
  Assert.False(flushed.Wait 100, "Flush answered while updates were still waiting")
  gate.Set()
  Assert.True(flushed.Wait 5000)
  Assert.Equal(5, seen.Count)

[<Fact>]
let ``a sink that throws does not stop the game's updates`` () =
  let seen = ResizeArray<string>()
  let agent = UpdateAgent((fun u -> if name u = "bad" then failwith "sink down" else seen.Add(name u)), NullLogger.Instance)
  agent.Post(GameStarted "bad")
  agent.Post(GameStarted "after")
  Async.RunSynchronously(agent.Flush(), 5000)
  Assert.Equal<string list>([ "after" ], List.ofSeq seen)

[<Fact>]
let ``the live-feed recorder keeps every line from many threads`` () =
  let path = Path.Combine(Path.GetTempPath(), $"eb_feed_{Guid.NewGuid():N}.jsonl")
  try
    let recorder = new LiveFeedRecorder(path)
    [| for t in 1 .. 8 -> Threading.Tasks.Task.Run(fun () -> for i in 1 .. 200 do recorder.RecordLine(sprintf "%d-%d" t i)) |]
    |> Threading.Tasks.Task.WaitAll
    recorder.Dispose()
    recorder.Dispose()   // a second close is harmless
    let lines = File.ReadAllLines path
    Assert.Equal(1600, lines.Length)
    // each thread's lines in its own order
    for t in 1 .. 8 do
      let mine = lines |> Array.filter (fun l -> l.StartsWith(sprintf "%d-" t)) |> Array.map (fun l -> int (l.Split('-').[1]))
      Assert.Equal<int[]>([| 1 .. 200 |], mine)
  finally File.Delete path

/// A sink that holds every update until released (it gives up after 3 s instead of hanging a test).
let private heldSink () =
  let gate = new ManualResetEventSlim(false)
  let seen = Collections.Concurrent.ConcurrentQueue<string>()
  gate, seen, (fun (u: Update) -> gate.Wait 3000 |> ignore; seen.Enqueue(name u))

[<Fact>]
let ``a game's result comes back only after all its updates are out`` () =
  let gate, seen, sink = heldSink ()
  let game (post: Update -> unit) = async {
    for i in 1 .. 3 do post (GameStarted (string i))
    return "1-0" }
  let result = run sink NullLogger.Instance game |> Async.StartAsTask
  Assert.False(result.Wait 200, "the result came back with updates still waiting")
  gate.Set()
  Assert.True(result.Wait 5000)
  Assert.Equal("1-0", result.Result)
  Assert.Equal(3, seen.Count)

[<Fact>]
let ``a game's exception comes back only after all its updates are out, unchanged`` () =
  let gate, seen, sink = heldSink ()
  let game post : Async<string> = async { post (GameStarted "white"); return failwith "engine did not start" }
  let result = run sink NullLogger.Instance game |> Async.StartAsTask
  Assert.False(result.Wait 200, "the exception came back with updates still waiting")
  gate.Set()
  let ex = Assert.Throws<AggregateException>(fun () -> result.Wait 5000 |> ignore)
  Assert.Equal("engine did not start", ex.InnerException.Message)
  Assert.Equal(1, seen.Count)

[<Fact>]
let ``a recorded line is on disk whole before the recorder closes`` () =
  let path = Path.Combine(Path.GetTempPath(), $"eb_feed_{Guid.NewGuid():N}.jsonl")
  let recorder = new LiveFeedRecorder(path)
  try
    recorder.RecordLine """{"type":"GameStarted"}"""
    let read () =
      use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
      use sr = new StreamReader(fs)
      sr.ReadToEnd()
    let sw = Diagnostics.Stopwatch.StartNew()
    while not ((read ()).EndsWith("\"GameStarted\"}" + Environment.NewLine)) && sw.ElapsedMilliseconds < 3000L do Thread.Sleep 10
    Assert.EndsWith("\"GameStarted\"}" + Environment.NewLine, read ())
  finally
    recorder.Dispose()
    File.Delete path
