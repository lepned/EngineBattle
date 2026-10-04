module GamePlayerMachineTests

open System
open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.Game.GoCommand
open ChessLibrary.Game.EngineLine
open ChessLibrary.Game.PlayerMachine

let private ms (n: float) = TimeSpan.FromMilliseconds n

let private settings =
  { Player = "E"; CanPing = true; PingTimeout = ms 15000.0; StopWait = ms 10000.0
    StatusInterval = ms 1000.0; PolicyTest = false }

let private view = { ShortPv = id; ShortSan = ignore }

let private think lastMove =
  { Position = "position startpos moves e2e4"; LastMove = lastMove; Search = MoveTimeMs 100
    StopAfter = Some (ms 350.0); WhiteToMove = false; View = view }

let private ponder =
  { Position = "position startpos moves e2e4 e7e5 g1f3"; Move = "g1f3"
    Command = "go wtime 1000 btime 1000 ponder"; WhiteToMove = false; View = view }

/// Runs events from a state; each event at its own time (ms).
let private run state events =
  events |> List.fold (fun (s, _) (at, ev) -> step settings (ms at) s ev) (state, [])

let private sends effects = effects |> List.choose (function Send c -> Some c | _ -> None)
let private reply effects = effects |> List.tryPick (function ReplyThink o -> Some o | _ -> None)
let private info = "info depth 5 seldepth 7 score cp 30 nodes 1000 nps 50000 time 20 pv e7e5 g1f3"

[<Fact>]
let ``bestmove parsing takes the move and the ponder move, and tolerates a bare bestmove`` () =
  Assert.Equal(BestMove ("e2e4", Some "e7e5"), parse "bestmove e2e4 ponder e7e5")
  Assert.Equal(BestMove ("e2e4", None), parse "bestmove e2e4")
  Assert.Equal(BestMove ("", None), parse "bestmove")
  Assert.Equal(ReadyOk, parse "readyok ")

[<Fact>]
let ``a search is pinged before go, and timed from go`` () =
  let s, e = step settings (ms 0.0) Idle (Think (think None))
  Assert.Equal<Command list>([ Position "position startpos moves e2e4"; IsReady ], sends e)
  let s, e = step settings (ms 5.0) s (Line "readyok")
  Assert.Equal<Command list>([ Go (MoveTimeMs 100) ], sends e)
  let s, e = step settings (ms 105.0) s (Line "bestmove e7e5 ponder g1f3")
  Assert.Equal(Idle, s)
  match reply e with
  | Some (Moved ("e7e5", Some "g1f3", elapsed, _)) -> Assert.Equal(ms 100.0, elapsed)
  | other -> failwithf "%A" other

[<Fact>]
let ``an engine without ping gets go at once`` () =
  let _, e = step { settings with CanPing = false } (ms 0.0) Idle (Think (think None))
  Assert.Equal<Command list>([ Position "position startpos moves e2e4"; Go (MoveTimeMs 100) ], sends e)

[<Fact>]
let ``no readyok within the ping timeout is a stall`` () =
  let s, e = run Idle [ 0.0, Think (think None); 15000.0, Tick ]
  Assert.Equal(Unresponsive, s)
  Assert.Equal(Some (Stalled "no readyok"), reply e)

[<Fact>]
let ``a search past its time is stopped, and its late bestmove carries the elapsed time`` () =
  let s, e = run Idle [ 0.0, Think (think None); 0.0, Line "readyok"; 349.0, Tick ]
  Assert.Empty(sends e)
  let s, e = step settings (ms 350.0) s Tick
  Assert.Equal<Command list>([ Stop ], sends e)
  let _, e = step settings (ms 400.0) s (Line "bestmove e7e5")
  match reply e with
  | Some (Moved (_, _, elapsed, _)) -> Assert.Equal(ms 400.0, elapsed)
  | other -> failwithf "%A" other

[<Fact>]
let ``no bestmove within the stop wait is NoBestMove`` () =
  let s, e = run Idle [ 0.0, Think (think None); 0.0, Line "readyok"; 350.0, Tick; 10350.0, Tick ]
  Assert.Equal(Unresponsive, s)
  Assert.Equal(Some (NoBestMove (ms 10350.0)), reply e)

[<Fact>]
let ``a ponder hit sends ponderhit, no isready, and times from the hit`` () =
  let s, e = step settings (ms 0.0) Idle (Ponder ponder)
  Assert.Equal<Command list>([ Position ponder.Position; GoPonder ponder.Command ], sends e)
  let s, e = step settings (ms 500.0) s (Think (think (Some "g1f3")))
  Assert.Equal<Command list>([ PonderHit ], sends e)
  let _, e = step settings (ms 600.0) s (Line "bestmove b8c6")
  match reply e with
  | Some (Moved ("b8c6", _, elapsed, _)) -> Assert.Equal(ms 100.0, elapsed)
  | other -> failwithf "%A" other

[<Fact>]
let ``a ponder miss stops, drops the stale bestmove, then pings and searches`` () =
  let s, e = run Idle [ 0.0, Ponder ponder; 500.0, Think (think (Some "d2d4")) ]
  Assert.Equal<Command list>([ Stop ], sends e)
  let s, e = step settings (ms 510.0) s (Line info)
  Assert.Empty(e)
  let s, e = step settings (ms 520.0) s (Line "bestmove g1f3")
  Assert.Equal(None, reply e)
  Assert.Equal<Command list>([ Position "position startpos moves e2e4"; IsReady ], sends e)
  let _, e = step settings (ms 525.0) s (Line "readyok")
  Assert.Equal<Command list>([ Go (MoveTimeMs 100) ], sends e)

[<Fact>]
let ``a stale bestmove that never comes is a stall`` () =
  let s, e = run Idle [ 0.0, Ponder ponder; 500.0, Think (think (Some "d2d4")); 10500.0, Tick ]
  Assert.Equal(Unresponsive, s)
  Assert.Equal(Some (Stalled "no bestmove after stop"), reply e)

[<Fact>]
let ``a bestmove while pondering leaves the engine idle for a normal search`` () =
  let s, e = run Idle [ 0.0, Ponder ponder; 100.0, Line "bestmove g1f3" ]
  Assert.Equal(Idle, s)
  Assert.Contains(e, fun x -> match x with Warn _ -> true | _ -> false)
  let _, e = step settings (ms 200.0) s (Think (think (Some "g1f3")))
  Assert.Equal<Command list>([ Position "position startpos moves e2e4"; IsReady ], sends e)

[<Fact>]
let ``the end of the game stops a pondering engine and waits for its bestmove`` () =
  let s, e = run Idle [ 0.0, Ponder ponder; 100.0, EndGame ]
  Assert.Equal<Command list>([ Stop ], sends e)
  Assert.DoesNotContain(ReplyEndGame true, e)
  let s, e = step settings (ms 150.0) s (Line "bestmove g1f3")
  Assert.Equal(Idle, s)
  Assert.Contains(ReplyEndGame true, e)

[<Fact>]
let ``the end of the game interrupts a search, stops it and drains it`` () =
  let s, e = run Idle [ 0.0, Think (think None); 0.0, Line "readyok"; 50.0, EndGame ]
  Assert.Equal(Some Interrupted, reply e)
  Assert.Equal<Command list>([ Stop ], sends e)
  let _, e = step settings (ms 60.0) s (Line "bestmove e7e5")
  Assert.Contains(ReplyEndGame true, e)

[<Fact>]
let ``the end of the game answers at once for an idle engine`` () =
  let _, e = step settings (ms 0.0) Idle EndGame
  Assert.Equal<Effect list>([ ReplyEndGame true ], e)

[<Fact>]
let ``closed output during a search is a crash`` () =
  let s, e = run Idle [ 0.0, Think (think None); 0.0, Line "readyok"; 10.0, OutputClosed ]
  Assert.Equal(Closed, s)
  Assert.Equal(Some Crashed, reply e)

[<Fact>]
let ``status goes out at most once per interval, and the stats keep every line`` () =
  let s, e1 = run Idle [ 0.0, Think (think None); 0.0, Line "readyok"; 500.0, Line info ]
  let s, e2 = step settings (ms 1000.0) s (Line info)
  let s, e3 = step settings (ms 1500.0) s (Line info)
  let isStatus = function Emit (Status _) -> true | _ -> false
  Assert.Equal(0, e1 |> List.filter isStatus |> List.length)
  Assert.Equal(1, e2 |> List.filter isStatus |> List.length)
  Assert.Equal(0, e3 |> List.filter isStatus |> List.length)
  let _, e = step settings (ms 1600.0) s (Line "bestmove e7e5")
  match reply e with
  | Some (Moved (_, _, _, stats)) ->
      Assert.Equal(3, stats.Evals.Length)
      Assert.Equal(5, stats.Depth)
      Assert.Equal("e7e5 g1f3", stats.LongPv)
  | other -> failwithf "%A" other

[<Fact>]
let ``a LogLiveStats block is sent when it closes, and an open one before the bestmove`` () =
  let nn move = sprintf "info string %s  (322 ) N:     900 (+ 0) (P: 61.00%%) (WL:  0.10000) (D: 0.300) (M: 60.0) (Q:  0.10000) (U: 0.01000) (S:  0.11000) (V:  0.0900)" move
  let node = "info string node  (  20) N:    1000 (+ 0) (P: 100.0%) (WL:  0.09000) (D: 0.300) (M: 60.0) (Q:  0.09000) (V:  0.0800)"
  let s, _ = run Idle [ 0.0, Think (think None); 0.0, Line "readyok" ]
  let s, e1 = run s [ 10.0, Line (nn "e7e5"); 11.0, Line (nn "d7d5") ]
  let s, e2 = step settings (ms 12.0) s (Line node)
  let isNN = function Emit (NNSeq block) -> Some block.Count | _ -> None
  Assert.Empty(List.choose isNN e1)
  Assert.Equal<int list>([ 2 ], List.choose isNN e2)
  let _, e = run s [ 20.0, Line (nn "e7e5"); 21.0, Line "bestmove e7e5" ]
  Assert.Equal<int list>([ 1 ], List.choose isNN e)
  Assert.True((reply e).IsSome)

[<Fact>]
let ``next due follows the state's deadline`` () =
  let readying, _ = step settings (ms 0.0) Idle (Think (think None))
  Assert.Equal(Some (ms 15000.0), nextDue settings readying)
  let thinking, _ = step settings (ms 10.0) readying (Line "readyok")
  Assert.Equal(Some (ms 360.0), nextDue settings thinking)
  Assert.Equal(None, nextDue settings Idle)

[<Fact>]
let ``a ponder hit keeps the ponder search's stats for a bestmove sent at once`` () =
  let s, _ = run Idle [ 0.0, Ponder ponder; 100.0, Line info; 500.0, Think (think (Some "g1f3")) ]
  let _, e = step settings (ms 501.0) s (Line "bestmove e7e5")
  match reply e with
  | Some (Moved (_, _, _, stats)) ->
      Assert.Equal(5, stats.Depth)
      Assert.Equal(1, stats.Evals.Length)
      Assert.Equal("e7e5 g1f3", stats.LongPv)
  | other -> failwithf "%A" other
