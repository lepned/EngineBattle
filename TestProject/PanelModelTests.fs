module PanelModelTests

open Xunit
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PanelModel
open ChessLibrary.PageEngine

let private nodes n = { Limit = Nodes n; HasMove = true }
let private run state events = events |> List.fold (fun (s, _) e -> step s e) (state, [])
let private searches effects = effects |> List.choose (function Search (l, m) -> Some (l, m) | _ -> None)

/// An engine that is ready, auto-search on, three lines.
let private ready = { initial 3 with Engine = Ready; Auto = true }

let private line k (move: string) cp =
  Status { EngineStatus.Empty with MultiPV = k; PV = move; PVLongSAN = move + " e7e5"; Eval = CP cp }

let private best move = BestMove { BestMoveInfo.Empty with Move = move }

/// Searching search 1 (started by Start).
let private searching =
  let s, _ = run ready [ Start (nodes 1000); Started 1 ]
  s

[<Fact>]
let ``a start before the engine is there starts it, and searches once it is ready`` () =
  let s, e = step (initial 3) (Start (nodes 1000))
  Assert.Equal<Effect list>([ StartEngine ], e)
  let s, e = step s (Start (nodes 2000))
  Assert.Empty(e)
  let s, e = step s EngineReady
  Assert.Equal<(Limit * string list) list>([ Nodes 2000, [] ], searches e)
  Assert.True(s.Searching)
  Assert.Contains(Timer true, e)

[<Fact>]
let ``only the current search's results are shown`` () =
  let s, e = step searching (Result (7, line 1 "d2d4" 0.3))
  Assert.Empty(e)
  Assert.True(s.Rows.IsEmpty)
  let s, e = step searching (Result (1, line 1 "e2e4" 0.3))
  Assert.Equal("e2e4", s.Rows.[1].PV)
  Assert.Contains(ShowRows, e)

[<Fact>]
let ``A to B and back to A: the first search's late bestmove does not end the new one`` () =
  // search 1 on A; navigate to B and back to A (same position), search 2 starts there
  let s, _ = run searching [ Navigated; Navigated; Settled (2, nodes 1000); Started 2 ]
  Assert.True(s.Searching)
  let s, e = step s (Result (1, best "e2e4"))
  Assert.Empty(e)
  Assert.True(s.Searching)
  let s, e = step s (Result (2, best "d2d4"))
  Assert.False(s.Searching)
  Assert.Contains(Timer false, e)

[<Fact>]
let ``fast navigation searches only the position stopped on`` () =
  let s, e1 = step ready Navigated
  let s, e2 = step s Navigated
  let s, e3 = step s Navigated
  Assert.Contains(Settle (1, autoSearchDelayMs), e1)
  Assert.Contains(Settle (3, autoSearchDelayMs), e3)
  let s, e = step s (Settled (1, nodes 1000))
  Assert.Empty(e)
  let _, e = step s (Settled (3, nodes 1000))
  Assert.Equal(1, (searches e).Length)

[<Fact>]
let ``navigation stops a search also with auto-search off, and starts none`` () =
  let s, _ = run { ready with Auto = false } [ Start (nodes 1000); Started 1 ]
  let s, e = step s Navigated
  Assert.Contains(StopEngine, e)
  Assert.Contains(Timer false, e)
  Assert.DoesNotContain(e, fun x -> match x with Settle _ -> true | _ -> false)
  Assert.False(s.Searching)

[<Fact>]
let ``a navigation clears the rows, the focused moves and the eval to publish`` () =
  let s, _ = run searching [ Result (1, line 1 "e2e4" 0.30); FocusToggled ("e2e4", nodes 1000) ]
  let s, e = step s Navigated
  Assert.True(s.Rows.IsEmpty)
  Assert.Empty(s.Focused)
  Assert.Equal(None, s.LastEval)
  Assert.Contains(ClearLists, e)
  // the charts wait for the next search: no script call per key
  Assert.DoesNotContain(ClearCharts, e)

[<Fact>]
let ``a start while searching replaces the search; while reviewing nothing starts`` () =
  let _, e = step searching (Start (nodes 5000))
  Assert.Equal<(Limit * string list) list>([ Nodes 5000, [] ], searches e)
  let _, e = step { ready with Reviewing = true } (Start (nodes 5000))
  Assert.Empty(e)

[<Fact>]
let ``nothing is searched in a position without a move`` () =
  let s, e = step ready (Start { Limit = Infinite; HasMove = false })
  Assert.Empty(e)
  Assert.False(s.Searching)

[<Fact>]
let ``stop ends the search, and its last results still count`` () =
  let s, e = step searching Stop
  Assert.Equal<Effect list>([ StopEngine; Timer false ], e)
  let _, e = step s (Result (1, best "e2e4"))
  Assert.Contains(e, fun x -> match x with Completed _ -> true | _ -> false)

[<Fact>]
let ``reset stops, resets the engine and clears everything`` () =
  let s, _ = run searching [ Result (1, line 1 "e2e4" 0.3); FocusToggled ("e2e4", nodes 1000) ]
  let s, e = step s Reset
  Assert.Equal<Effect list>([ StopEngine; Timer false; ResetEngine; ClearLists; ClearCharts; ShowRows ], e)
  Assert.True(s.Rows.IsEmpty)
  Assert.Empty(s.Focused)
  Assert.False(s.Searching)

[<Fact>]
let ``fewer lines trim the table, and a higher line arriving later is not shown`` () =
  let s, _ = run searching [ Result (1, line 1 "e2e4" 0.3); Result (1, line 2 "d2d4" 0.2); Result (1, line 3 "c2c4" 0.1) ]
  let s, e = step s (LinesChanged 1)
  Assert.Contains(SetMultiPv 1, e)
  Assert.Equal<int list>([ 1 ], s.Rows |> Map.toList |> List.map fst)
  let s, _ = step s (Result (1, line 3 "c2c4" 0.1))
  Assert.Equal<int list>([ 1 ], s.Rows |> Map.toList |> List.map fst)

[<Fact>]
let ``a focused move reruns a running search with it; an idle panel only remembers it`` () =
  let s, e = step searching (FocusToggled ("e2e4", nodes 1000))
  Assert.Equal<(Limit * string list) list>([ Nodes 1000, [ "e2e4" ] ], searches e)
  let s, e = step s (FocusToggled ("e2e4", nodes 1000))
  Assert.Equal<(Limit * string list) list>([ Nodes 1000, [] ], searches e)
  let idle, _ = step s Stop
  let idle, e = step idle (FocusToggled ("d2d4", nodes 1000))
  Assert.Empty(searches e)
  Assert.Equal<string list>([ "d2d4" ], idle.Focused)
  // clearing nothing reruns nothing
  let _, e = step searching (FocusCleared (nodes 1000))
  Assert.Empty(e)

[<Fact>]
let ``a focused rerun drops the old copy of a line that moved to another index`` () =
  let s, _ = run searching [ Result (1, line 1 "e2e4" 0.3); Result (1, line 2 "d2d4" 0.2) ]
  let s, _ = run s [ FocusToggled ("d2d4", nodes 1000); Started 2; Result (2, line 1 "d2d4" 0.25) ]
  Assert.Equal<(int * string) list>([ 1, "d2d4" ], s.Rows |> Map.toList |> List.map (fun (k, r) -> k, r.PV))

[<Fact>]
let ``the eval goes to the host only when it moved enough`` () =
  let publishes e = e |> List.filter (function PublishEval _ -> true | _ -> false) |> List.length
  let s, e = step searching (Result (1, line 1 "e2e4" 0.10))
  Assert.Equal(1, publishes e)
  let s, e = step s (Result (1, line 1 "e2e4" 0.12))
  Assert.Equal(0, publishes e)
  let s, e = step s (Result (1, line 1 "e2e4" 0.20))
  Assert.Equal(1, publishes e)
  // the second line never publishes
  let _, e = step s (Result (1, line 2 "d2d4" 0.90))
  Assert.Equal(0, publishes e)

[<Fact>]
let ``a value-head engine's move fills the empty main line`` () =
  let empty = Status { EngineStatus.Empty with MultiPV = 1; PV = ""; Eval = CP 0.1 }
  let s, _ = run searching [ Result (1, empty); Result (1, BestMove { BestMoveInfo.Empty with Move = "e2e4"; PV = "1.e4" }) ]
  Assert.Equal("1.e4", s.Rows.[1].PV)
  Assert.Equal("e2e4", s.Rows.[1].PVLongSAN)

[<Fact>]
let ``an engine that failed or exited ends the search; a start after an exit restarts it`` () =
  let s, e = step searching (Result (0, EngineFailed ("E", "no readyok")))
  Assert.False(s.Searching)
  Assert.Contains(Timer false, e)
  let s, _ = step searching EngineGone
  let _, e = step s (Start (nodes 1000))
  Assert.Equal<Effect list>([ StartEngine ], e)

[<Fact>]
let ``while reviewing, the host's results are shown whatever their id`` () =
  let s, _ = step { ready with Reviewing = true } (Result (0, line 1 "e2e4" 0.3))
  Assert.Equal("e2e4", s.Rows.[1].PV)

[<Fact>]
let ``loading an engine stops the search and starts the new one`` () =
  let s, e = step searching LoadEngine
  Assert.Equal<Effect list>([ StopEngine; Timer false; StartEngine ], e)
  Assert.Equal(Starting, s.Engine)
  let s, _ = step s (Start (nodes 1000))
  let _, e = step s EngineReady
  Assert.Equal(1, (searches e).Length)

[<Fact>]
let ``another search's live stats are not shown but go to the host for its cache`` () =
  let stats = NNSeq (ResizeArray())
  let s, e = step searching (Result (9, stats))
  Assert.Equal<Effect list>([ Late stats ], e)
  let _, e = step searching (Result (9, line 1 "e2e4" 0.3))
  Assert.Empty(e)

[<Fact>]
let ``auto-search never starts an engine that is not running`` () =
  let s, e = run { initial 3 with Auto = true } [ Navigated; Settled (1, nodes 1000) ]
  Assert.Empty(e)
  Assert.Equal(NotStarted, s.Engine)

[<Fact>]
let ``stop and escape cancel an auto-search still waiting to start`` () =
  let s, _ = run ready [ Navigated; Stop ]
  let _, e = step s (Settled (1, nodes 1000))
  Assert.Empty(e)
  let s, _ = run ready [ Navigated; Reset ]
  let _, e = step s (Settled (1, nodes 1000))
  Assert.Empty(e)

[<Fact>]
let ``a focused rerun keeps the views, while reviewing the host's lines are not capped`` () =
  let _, e = step searching (FocusToggled ("e2e4", nodes 1000))
  Assert.DoesNotContain(ClearLists, e)
  Assert.DoesNotContain(ClearCharts, e)
  let s, _ = step { ready with Reviewing = true; Lines = 1 } (Result (0, line 3 "c2c4" 0.1))
  Assert.True(s.Rows.ContainsKey 3)

[<Fact>]
let ``a host's engine failing does not stop the panel's own search`` () =
  let s, _ = step { searching with Reviewing = true } (Result (0, EngineFailed ("Host", "gone")))
  Assert.True(s.Searching)

[<Fact>]
let ``a host's review stops the panel's own search`` () =
  let s, e = step searching (ReviewingChanged true)
  Assert.Contains(StopEngine, e)
  Assert.False(s.Searching)
  Assert.True(s.Reviewing)
  let _, e = step s (ReviewingChanged true)
  Assert.Empty(e)

[<Fact>]
let ``auto-search: a move while the engine starts is searched once it is ready`` () =
  let s, e = run (initial 3) [ AutoChanged true; Start (nodes 1000) ]
  Assert.Equal(Starting, s.Engine)
  // a new position arrives during the load: its search waits for the engine
  let s, e = step s Navigated
  let s, e = step s (Settled (s.Navigation, nodes 2000))
  let _, e = step s EngineReady
  Assert.Equal<(Limit * string list) list>([ Nodes 2000, [] ], searches e)

[<Fact>]
let ``auto-search never starts an engine itself`` () =
  let s, _ = step { initial 3 with Auto = true } Navigated
  let _, e = step s (Settled (s.Navigation, nodes 2000))
  Assert.Empty(e)
