module AnalysisMachineTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.EngineTypes
open ChessLibrary.AnalysisMachine

let private ms (n: float) = TimeSpan.FromMilliseconds n

let private settings =
  { Name = "E"; CanPing = true; StopAnswers = (fun _ -> true)
    StartTimeout = ms 60000.0; PingTimeout = ms 15000.0; StopWait = ms 10000.0 }

/// A position where every move is legal; no SAN.
let private pos =
  { new AnalysisOutput.IPosition with
      member _.WhiteToMove = true
      member _.SanPv(_, lan) = lan
      member _.BestMoveFacts move =
        Some { ShortSan = move; MoveNumber = 1; WhiteToMove = true; Fen = "f"; PiecesLeft = 32; IsCastling = false }
      member _.AddShortSan _ = ()
      member _.Describe () = ""
      member _.HasLegalMove () = true }

let private completedWith move = function
  | (_, Completed (Some (bm: BestMoveInfo))) -> bm.Move = move
  | _ -> false

let private search id = { Id = id; Position = sprintf "position fen p%d" id; Go = "go infinite" }

let private run settings state events =
  events |> List.fold (fun (s, _) (at, ev) -> step settings pos (ms at) s ev) (state, [])

let private sends effects = effects |> List.choose (function Send c -> Some c | _ -> None)
let private replies effects = effects |> List.choose (function Reply (id, o) -> Some (id, o) | _ -> None)
let private emitted effects = effects |> List.choose (function Emit (u, _) -> Some u | _ -> None)
let private emittedFor effects = effects |> List.choose (function Emit (u, s) -> Some (u, s) | _ -> None)

/// An idle engine, as after start-up.
let private idle = { initial true with Phase = Idle }

[<Fact>]
let ``start-up collects the options, sends the init commands, ucinewgame and isready, then Ready`` () =
  let s, e = run settings (initial true) [ 0.0, Line "id name X"; 1.0, Line "option name LogLiveStats type check default false"; 2.0, Line "uciok" ]
  Assert.Equal<Effect list>([ OptionsReceived [ "id name X"; "option name LogLiveStats type check default false" ] ], e)
  let s, e = step settings pos (ms 3.0) s (Init [ "setoption name Hash value 32" ])
  Assert.Equal<string list>([ "setoption name Hash value 32"; "ucinewgame"; "isready" ], sends e)
  let s, e = step settings pos (ms 4.0) s (Line "readyok")
  Assert.Equal(Idle, s.Phase)
  Assert.Equal<EngineUpdate list>([ Ready ("E", true) ], emitted e)

[<Fact>]
let ``a Winboard engine is ready once its init commands are out`` () =
  let wb = { settings with CanPing = false }
  let s, e = step wb pos (ms 0.0) (initial false) (Init [])
  Assert.Equal<string list>([ "ucinewgame" ], sends e)
  Assert.Equal<EngineUpdate list>([ Ready ("E", false) ], emitted e)
  Assert.Equal(Idle, s.Phase)

[<Fact>]
let ``a search asked for during start-up starts once the engine is ready`` () =
  let s, _ = run settings (initial true) [ 0.0, Line "uciok"; 1.0, Analyse (search 1); 2.0, Init [] ]
  let _, e = step settings pos (ms 3.0) s (Line "readyok")
  Assert.Equal<string list>([ "position fen p1"; "isready" ], sends e)

[<Fact>]
let ``a search sets the board, sends the position and isready, then go on readyok`` () =
  let s, e = step settings pos (ms 0.0) idle (Analyse (search 1))
  Assert.Equal<Effect list>([ UsePosition (1, "position fen p1"); Send "position fen p1"; Send "isready" ], e)
  let _, e = step settings pos (ms 1.0) s (Line "readyok")
  Assert.Equal<string list>([ "go infinite" ], sends e)

[<Fact>]
let ``a bestmove completes the search and leaves the engine idle`` () =
  let s, e = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, Line "bestmove e2e4" ]
  Assert.Equal(Idle, s.Phase)
  Assert.True(replies e |> List.exactlyOne |> completedWith "e2e4")
  Assert.Contains(Done "E", emitted e)

[<Fact>]
let ``a new search stops the running one, drops its output, and starts after its bestmove`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok" ]
  let s, e = step settings pos (ms 2.0) s (Analyse (search 2))
  Assert.Equal<string list>([ "stop" ], sends e)
  Assert.Equal<(int * Outcome) list>([ 1, Superseded ], replies e)
  Assert.Contains(SearchStopped "E", emitted e)
  let s, e = step settings pos (ms 3.0) s (Line "info depth 9 score cp 50 nodes 9 nps 9 time 9 pv e2e4")
  Assert.Empty(e)
  let _, e = step settings pos (ms 4.0) s (Line "bestmove e2e4")
  Assert.Empty(emitted e)
  Assert.Equal<string list>([ "position fen p2"; "isready" ], sends e)

[<Fact>]
let ``the newest of several waiting searches wins`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, Analyse (search 2) ]
  let s, e = step settings pos (ms 3.0) s (Analyse (search 3))
  Assert.Equal<(int * Outcome) list>([ 2, Superseded ], replies e)
  let _, e = step settings pos (ms 4.0) s (Line "bestmove e2e4")
  Assert.Equal<string list>([ "position fen p3"; "isready" ], sends e)

[<Fact>]
let ``a search replaced before its go never gets one`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Analyse (search 2) ]
  let _, e = step settings pos (ms 2.0) s (Line "readyok")
  Assert.Equal<(int * Outcome) list>([ 1, Superseded ], replies e)
  Assert.Equal<string list>([ "position fen p2"; "isready" ], sends e)

[<Fact>]
let ``stop lets the running search end with its bestmove`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok" ]
  let s, e = step settings pos (ms 2.0) s Stop
  Assert.Equal<string list>([ "stop" ], sends e)
  Assert.Empty(replies e)
  let _, e = step settings pos (ms 3.0) s (Line "bestmove e2e4")
  Assert.True(replies e |> List.exactlyOne |> completedWith "e2e4")

[<Fact>]
let ``stop before the go asks for a move now: go, then stop at once`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1) ]
  let s, e = step settings pos (ms 1.0) s Stop
  Assert.Empty(e)
  let s, e = step settings pos (ms 2.0) s (Line "readyok")
  Assert.Equal<string list>([ "go infinite"; "stop" ], sends e)
  let _, e = step settings pos (ms 3.0) s (Line "bestmove e2e4")
  Assert.True(replies e |> List.exactlyOne |> completedWith "e2e4")

[<Fact>]
let ``options during a search stop it, are sent, and the search runs again`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok" ]
  let s, e = step settings pos (ms 2.0) s (Configure ([ "setoption name MultiPV value 3" ], true))
  Assert.Equal<string list>([ "stop" ], sends e)
  Assert.Empty(replies e)
  let _, e = step settings pos (ms 3.0) s (Line "bestmove e2e4")
  Assert.Equal<string list>([ "setoption name MultiPV value 3"; "position fen p1"; "isready" ], sends e)

[<Fact>]
let ``options for an idle engine go at once; ucinewgame waits for a running search`` () =
  let _, e = step settings pos (ms 0.0) idle (Configure ([ "setoption name Hash value 64" ], true))
  Assert.Equal<string list>([ "setoption name Hash value 64" ], sends e)
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok" ]
  let s, e = step settings pos (ms 2.0) s (Configure ([ "ucinewgame" ], false))
  Assert.Empty(e)
  let _, e = run settings s [ 3.0, Analyse (search 2); 4.0, Line "bestmove e2e4" ]
  Assert.Equal<string list>([ "ucinewgame"; "position fen p2"; "isready" ], sends e)

[<Fact>]
let ``a Winboard analysis stop prints no move, so the search ends at once`` () =
  let wb = { settings with CanPing = false; StopAnswers = (fun _ -> false) }
  let s, _ = step wb pos (ms 0.0) idle (Analyse (search 1))
  let s, e = step wb pos (ms 1.0) s Stop
  Assert.Equal<string list>([ "stop" ], sends e)
  Assert.Equal<(int * Outcome) list>([ 1, Superseded ], replies e)
  Assert.Equal(Idle, s.Phase)

[<Fact>]
let ``no readyok within the ping timeout fails the search; a late answer revives the engine`` () =
  let s, e = run settings idle [ 0.0, Analyse (search 1); 15000.0, Tick ]
  Assert.Equal<(int * Outcome) list>([ 1, Failed "no readyok" ], replies e)
  Assert.Contains(EngineFailed ("E", "no readyok"), emitted e)
  let s, _ = step settings pos (ms 16000.0) s (Line "readyok")
  Assert.Equal(Idle, s.Phase)

[<Fact>]
let ``a request to an engine that stopped answering tries it again`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 15000.0, Tick ]
  let _, e = step settings pos (ms 15001.0) s (Analyse (search 2))
  Assert.Equal<string list>([ "position fen p2"; "isready" ], sends e)

[<Fact>]
let ``no bestmove within the stop wait fails the engine`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, Stop ]
  let s, e = step settings pos (ms 10002.0) s Tick
  Assert.Equal<(int * Outcome) list>([ 1, Failed "no bestmove after stop" ], replies e)
  match s.Phase with
  | Unresponsive _ -> ()
  | other -> failwithf "%A" other

[<Fact>]
let ``output ending during a search fails it and closes the engine`` () =
  let s, e = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, OutputClosed ]
  Assert.Equal(Closed, s.Phase)
  Assert.Equal<(int * Outcome) list>([ 1, Failed "the engine exited" ], replies e)
  let _, e = step settings pos (ms 3.0) s (Analyse (search 2))
  Assert.Equal<(int * Outcome) list>([ 2, Failed "the engine exited" ], replies e)

[<Fact>]
let ``next due follows the phase's wait`` () =
  let readying, _ = step settings pos (ms 100.0) idle (Analyse (search 1))
  Assert.Equal(Some (ms 15100.0), nextDue settings readying)
  let searching, _ = step settings pos (ms 200.0) readying (Line "readyok")
  Assert.Equal(None, nextDue settings searching)
  let stopping, _ = step settings pos (ms 300.0) searching Stop
  Assert.Equal(Some (ms 10300.0), nextDue settings stopping)

let private skip id = { Id = id; Position = ""; Go = "" }

[<Fact>]
let ``nothing to search ends at once, in turn, and replaces a running search`` () =
  let s, e = step settings pos (ms 0.0) idle (Analyse (skip 1))
  Assert.Equal(Idle, s.Phase)
  Assert.Empty(sends e)
  Assert.Equal<(int * Outcome) list>([ 1, Superseded ], replies e)
  Assert.Equal<EngineUpdate list>([ SearchStopped "E" ], emitted e)
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, Analyse (skip 2) ]
  let s, e = step settings pos (ms 3.0) s (Line "bestmove e2e4")
  Assert.Equal(Idle, s.Phase)
  Assert.Empty(sends e)
  Assert.Equal<(int * Outcome) list>([ 2, Superseded ], replies e)

[<Fact>]
let ``a failure ends the waiting searches for the GUI too`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Analyse (search 2) ]
  let _, e = step settings pos (ms 15000.0) s Tick
  Assert.Equal<(int * Outcome) list>([ 1, Failed "no readyok"; 2, Failed "no readyok" ], replies e)
  Assert.Equal(2, emitted e |> List.filter (function SearchStopped _ -> true | _ -> false) |> List.length)

[<Fact>]
let ``options for an engine that stopped answering are kept for when it answers`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 15000.0, Tick; 15001.0, Configure ([ "setoption name Hash value 64" ], true) ]
  let s, _ = step settings pos (ms 15002.0) s (Line "readyok")
  let _, e = step settings pos (ms 15003.0) s (Analyse (search 2))
  Assert.Equal<string list>([ "position fen p2"; "isready" ], sends e)
  let s2, _ = run settings idle [ 0.0, Analyse (search 1); 15000.0, Tick; 15001.0, Configure ([ "setoption name Hash value 64" ], true) ]
  Assert.Equal<string list>([ "setoption name Hash value 64" ], s2.Queued)
  let _, e = step settings pos (ms 15002.0) s2 (Analyse (search 4))
  Assert.Equal<string list>([ "setoption name Hash value 64"; "position fen p4"; "isready" ], sends e)
  let _, e = step settings pos (ms 15002.0) s2 (Analyse (skip 3))
  Assert.Equal<(int * Outcome) list>([ 3, Superseded ], replies e)

[<Fact>]
let ``after a stop that got no answer, a new search waits for the old bestmove`` () =
  let s, _ = run settings idle [ 0.0, Analyse (search 1); 1.0, Line "readyok"; 2.0, Stop; 10002.0, Tick ]
  // a readyok only says it is still searching
  let s, e = step settings pos (ms 10003.0) s (Line "readyok")
  Assert.Empty(sends e)
  let s, e = step settings pos (ms 10004.0) s (Analyse (search 2))
  Assert.Empty(sends e)
  Assert.Equal(Draining ForBestMove, s.Phase)
  let _, e = step settings pos (ms 10005.0) s (Line "bestmove e2e4")
  Assert.Empty(replies e)
  Assert.Equal<string list>([ "position fen p2"; "isready" ], sends e)

[<Fact>]
let ``a search's updates carry its id; the engine's own carry none`` () =
  let s, e = run settings (initial true) [ 0.0, Line "uciok"; 1.0, Init []; 2.0, Line "readyok" ]
  Assert.Equal<(EngineUpdate * int option) list>([ Ready ("E", false), None ], emittedFor e)
  let s, _ = run settings s [ 3.0, Analyse (search 7); 4.0, Line "readyok" ]
  let s, e = step settings pos (ms 5.0) s (Line "info depth 3 score cp 20 nodes 100 nps 100 time 1 pv e2e4")
  Assert.NotEmpty(emittedFor e)
  Assert.All(emittedFor e, fun (_, id) -> Assert.Equal(Some 7, id))
  // the replaced search's SearchStopped names it, not the new one
  let _, e = step settings pos (ms 6.0) s (Analyse (search 8))
  Assert.Equal<(EngineUpdate * int option) list>([ SearchStopped "E", Some 7 ], emittedFor e)

[<Fact>]
let ``a search's position is set under its own id`` () =
  let _, e = step settings pos (ms 0.0) idle (Analyse (search 3))
  Assert.Contains(UsePosition (3, "position fen p3"), e)

[<Fact>]
let ``output read while idle is the last search's trailing output`` () =
  // a Winboard analyze ends at once on stop; its last thinking lines come after
  let wb = { settings with CanPing = false; StopAnswers = (fun _ -> false) }
  let s, _ = run wb idle [ 0.0, Analyse (search 5); 1.0, Stop ]
  let _, e = step wb pos (ms 2.0) s (Line "info depth 9 score cp 10 nodes 9 nps 9 time 9 pv e2e4")
  Assert.NotEmpty(emittedFor e)
  Assert.All(emittedFor e, fun (_, id) -> Assert.Equal(Some 5, id))
