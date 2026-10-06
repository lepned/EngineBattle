module GamePlayerTests

open System
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary.Game.GoCommand
open ChessLibrary.Game.PlayerMachine
open ChessLibrary.Game.Player

/// An engine in memory: answers isready, go and stop the way a UCI engine does, or nothing.
type private ScriptedEngine(canPing: bool, silent: bool) =
  let output = Channel.CreateUnbounded<string>()
  let sent = ResizeArray<string>()
  let mutable searching = false
  let say (line: string) = output.Writer.TryWrite line |> ignore
  member _.Sent = lock sent (fun () -> List.ofSeq sent)
  member _.Close() = output.Writer.TryComplete() |> ignore
  interface IEngineIO with
    member _.Name = "S"
    member _.CanPing = canPing
    member _.Send command =
      lock sent (fun () -> sent.Add(sprintf "%A" command))
      match command with
      | _ when silent -> ()
      | IsReady -> say "readyok"
      | Go _ ->
          say "info depth 1 score cp 10 nodes 10 nps 100 time 1 pv e2e4"
          say "bestmove e2e4 ponder e7e5"
      | GoPonder _ -> searching <- true
      | PonderHit -> searching <- false; say "bestmove g1f3"
      | Stop -> if searching then searching <- false; say "bestmove d2d4"
      | Position _ -> ()
    member _.ReadLine token =
      task {
        try
          let! line = output.Reader.ReadAsync(token).AsTask()
          return struct (0L, line)
        with
        | :? OperationCanceledException -> return struct (0L, null)
        | :? ChannelClosedException -> return struct (0L, null)
      }

let private settings =
  { Player = "S"; CanPing = true; PingTimeout = TimeSpan.FromSeconds 5.0; StopWait = TimeSpan.FromSeconds 5.0
    StatusInterval = TimeSpan.FromSeconds 1.0; PolicyTest = false }

let private request lastMove =
  { Position = "position startpos"; LastMove = lastMove; Search = MoveTimeMs 100; StopAfter = None
    WhiteToMove = true; View = { ShortPv = id; ShortSan = ignore } }

let private run (a: Async<'T>) = Async.RunSynchronously(a, 5000)

[<Fact>]
let ``a search goes isready, go, and comes back with the move and its stats`` () =
  let engine = ScriptedEngine(true, false)
  use player = new Player(engine, settings, ignore, NullLogger.Instance)
  match run (player.Think (request None)) with
  | Moved ("e2e4", Some "e7e5", _, stats) -> Assert.Equal(1, stats.Depth)
  | other -> failwithf "%A" other
  Assert.Equal<string list>([ "Position \"position startpos\""; "IsReady"; "Go (MoveTimeMs 100)" ], engine.Sent)

[<Fact>]
let ``a ponder hit and the end of the game come back through the agent`` () =
  let engine = ScriptedEngine(true, false)
  use player = new Player(engine, settings, ignore, NullLogger.Instance)
  player.Ponder { Position = "position startpos moves e2e4"; Move = "e2e4"; Command = "go wtime 1 btime 1 ponder"; WhiteToMove = false; View = { ShortPv = id; ShortSan = ignore } }
  match run (player.Think (request (Some "e2e4"))) with
  | Moved ("g1f3", _, _, _) -> ()
  | other -> failwithf "%A" other
  Assert.True(run (player.EndGame ()))   // the engine ended idle
  Assert.Contains("PonderHit", engine.Sent)

[<Fact>]
let ``closed output answers a waiting search with Crashed`` () =
  let engine = ScriptedEngine(true, true)
  use player = new Player(engine, settings, ignore, NullLogger.Instance)
  let think = Async.StartAsTask (player.Think (request None))
  engine.Close()
  Assert.True(think.Wait 5000)
  Assert.Equal(Crashed, think.Result)

/// Writing its go takes 200 ms (a Winboard engine's pre-go delay); it answers at once.
type private SlowWriteEngine() =
  let output = Channel.CreateUnbounded<string>()
  interface IEngineIO with
    member _.Name = "W"
    member _.CanPing = false
    member _.Send command =
      match command with
      | Go _ ->
          Thread.Sleep 200
          output.Writer.TryWrite "bestmove e2e4" |> ignore
      | _ -> ()
    member _.ReadLine token =
      task {
        try
          let! line = output.Reader.ReadAsync(token).AsTask()
          return struct (0L, line)
        with _ -> return struct (0L, null)
      }

[<Fact>]
let ``the time it takes to write the go is not the engine's`` () =
  use player = new Player(SlowWriteEngine(), { settings with CanPing = false }, ignore, NullLogger.Instance)
  match run (player.Think (request None)) with
  | Moved (_, _, elapsed, _) -> Assert.True(elapsed < TimeSpan.FromMilliseconds 150.0, sprintf "charged %A" elapsed)
  | other -> failwithf "%A" other

/// Answers 50 ms after its go; the line then waits 150 ms to be handled (a busy pool).
type private LateHandledEngine() =
  let output = Channel.CreateUnbounded<struct (int64 * string)>()
  interface IEngineIO with
    member _.Name = "L"
    member _.CanPing = false
    member _.Send command =
      match command with
      | Go _ ->
          Task.Delay(50).ContinueWith(fun (_: Task) ->
            output.Writer.TryWrite(struct (Diagnostics.Stopwatch.GetTimestamp(), "bestmove e2e4")) |> ignore) |> ignore
      | _ -> ()
    member _.ReadLine token =
      task {
        try
          let! read = output.Reader.ReadAsync(token).AsTask()
          do! Task.Delay 150
          return read
        with _ -> return struct (0L, null)
      }

[<Fact>]
let ``a bestmove is timed by when it was read, not when the player got to it`` () =
  use player = new Player(LateHandledEngine(), { settings with CanPing = false }, ignore, NullLogger.Instance)
  match run (player.Think (request None)) with
  | Moved (_, _, elapsed, _) ->
      Assert.True(elapsed >= TimeSpan.FromMilliseconds 40.0 && elapsed < TimeSpan.FromMilliseconds 150.0, sprintf "charged %A" elapsed)
  | other -> failwithf "%A" other
