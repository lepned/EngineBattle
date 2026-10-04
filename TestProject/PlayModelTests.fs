module PlayModelTests

open System
open Xunit
open ChessLibrary.EngineTypes
open ChessLibrary.PlayModel
open ChessLibrary.PageEngine

let private s n = TimeSpan.FromSeconds(float n)
let private blitz = Clocked (s 60, s 60, s 2, s 2)
let private facts white ply = { WhiteToMove = white; Ply = ply; End = None }
let private run state events = events |> List.fold (fun (st, _) e -> step st e) (state, [])
let private best move = BestMove { BestMoveInfo.Empty with Move = move }
let private searches effects = effects |> List.choose (function Search g -> Some g | _ -> None)
let private ended effects = effects |> List.tryPick (function Ended (r, reason, _) -> Some (r, reason) | _ -> None)

let private ready = { initial with Engine = Ready }

/// Human white, game started at t=0, human to move.
let private playing = fst (step ready (Start (blitz, true, facts true 0, s 0)))

/// The human played 1.e4 at t=10: the engine searches (search 1).
let private engineThinking = fst (run playing [ HumanMoved (facts false 1, s 10); Started 1 ])

[<Fact>]
let ``the clock starts when the engine is ready, not while it loads`` () =
  let st, e = step initial (Start (blitz, true, facts true 0, s 0))
  Assert.Contains(StartEngine, e)
  Assert.DoesNotContain(ClockRunning true, e)
  // a tick during loading never flags
  let st, e = step st (Tick (s 120))
  Assert.True((ended e).IsNone)
  let st, e = step st (EngineReady (s 100))
  Assert.Contains(NewGame, e)
  Assert.Contains(ClockRunning true, e)
  Assert.Equal(s 50, remaining st true (s 110))

[<Fact>]
let ``the engine to move at the start searches at once`` () =
  let _, e = step ready (Start (blitz, false, facts true 0, s 0))
  Assert.Equal<Go list>([ GoClock (s 60 - engineMargin, s 60, s 2, s 2) ], searches e)

[<Fact>]
let ``a human move stops the human's clock with its increment and the engine searches with real times`` () =
  let st, e = step playing (HumanMoved (facts false 1, s 10))
  Assert.Equal(s 52, st.White)
  Assert.Equal<Go list>([ GoClock (s 52, s 60 - engineMargin, s 2, s 2) ], searches e)
  // the engine's clock now runs
  Assert.Equal(s 55, remaining st false (s 15))
  Assert.Equal(s 52, remaining st true (s 15))

[<Fact>]
let ``a human move on the engine's turn or a navigation is not a move`` () =
  let _, e = step engineThinking (HumanMoved (facts true 2, s 12))
  Assert.Empty(e)
  let _, e = step playing (HumanMoved (facts true 0, s 5))
  Assert.Empty(e)

[<Fact>]
let ``only the current search's bestmove is played, and only on the engine's turn`` () =
  let _, e = step engineThinking (Result (9, best "e7e5", s 12))
  Assert.Empty(e)
  let _, e = step engineThinking (Result (1, best "e7e5", s 12))
  Assert.Equal<Effect list>([ PlayEngineMove "e7e5" ], e)

[<Fact>]
let ``the engine's move hands the turn back and plays a queued premove`` () =
  let st, e = step engineThinking (EngineMoved (facts true 2, s 14))
  Assert.Equal(s 58, st.Black)
  Assert.Contains(PlayPremove, e)
  Assert.True(st.Current.IsNone)
  // its late results are dropped
  let _, e = step st (Result (1, best "g8f6", s 12))
  Assert.Empty(e)

[<Fact>]
let ``an illegal engine move or a failed engine aborts the game`` () =
  let st, e = step engineThinking (EngineMoveRefused ("e2e5", s 12))
  Assert.Equal(Over, st.Phase)
  Assert.Equal(Some ("Aborted", "Engine"), ended e)
  let st, e = step engineThinking (Result (1, EngineFailed ("x", "the engine exited"), s 12))
  Assert.Equal(Over, st.Phase)
  Assert.Equal(Some ("Aborted", "Engine"), ended e)
  let _, e = step engineThinking (EngineGone ("engine changed", s 12))
  Assert.Equal(Some ("Aborted", "Engine"), ended e)

[<Fact>]
let ``a search that ends without a move aborts the game`` () =
  let _, e = step engineThinking (Result (1, SearchStopped "x", s 12))
  Assert.Equal(Some ("Aborted", "Engine"), ended e)

[<Fact>]
let ``the side to move loses on time and the engine is stopped`` () =
  let _, e = step engineThinking (Tick (s 69))
  Assert.True((ended e).IsNone)
  let st, e = step engineThinking (Tick (s 71))
  Assert.Equal(Some ("Win", "Time"), ended e)
  Assert.Contains(StopEngine, e)
  Assert.Equal(TimeSpan.Zero, st.Black)
  let _, e = step playing (Tick (s 61))
  Assert.Equal(Some ("Loss", "Time"), ended e)

[<Fact>]
let ``a move after the flag fell loses on time`` () =
  let _, e = step playing (HumanMoved (facts false 1, s 61))
  Assert.Equal(Some ("Loss", "Time"), ended e)

[<Fact>]
let ``no clock: nodes per move searches nodes and never flags`` () =
  let st, _ = step ready (Start (NodesPerMove 800, true, facts true 0, s 0))
  let st, e = step st (HumanMoved (facts false 1, s 1000))
  Assert.Equal<Go list>([ GoNodes 800 ], searches e)
  let _, e = step st (Tick (s 5000))
  Assert.True((ended e).IsNone)

[<Fact>]
let ``checkmate and draws by the board end the game`` () =
  let _, e = step playing (HumanMoved ({ WhiteToMove = false; Ply = 1; End = Some Checkmate }, s 5))
  Assert.Equal(Some ("Win", "Checkmate"), ended e)
  let _, e = step engineThinking (EngineMoved ({ WhiteToMove = true; Ply = 2; End = Some Checkmate }, s 12))
  Assert.Equal(Some ("Loss", "Checkmate"), ended e)
  let _, e = step playing (HumanMoved ({ WhiteToMove = false; Ply = 1; End = Some Threefold }, s 5))
  Assert.Equal(Some ("Draw", "Threefold repetition"), ended e)

[<Fact>]
let ``takeback on the human's turn steps back two plies and restores the clocks there`` () =
  let st, _ = run engineThinking [ EngineMoved (facts true 2, s 14) ]
  let st, e = step st (Takeback (s 20))
  Assert.Equal<Effect list>([ StepBack 2 ], e)
  let st, e = step st (TookBack (facts true 0, s 21))
  Assert.Equal(s 60, st.White)
  Assert.Equal(s 60, st.Black)
  Assert.Equal(0, st.Ply)
  Assert.Empty(searches e)
  Assert.Equal(s 59, remaining st true (s 22))

[<Fact>]
let ``takeback while the engine thinks stops it and steps back one ply`` () =
  let st, e = step engineThinking (Takeback (s 12))
  Assert.Equal<Effect list>([ StopEngine; StepBack 1 ], e)
  // the stopped search's bestmove is not played
  let _, e = step st (Result (1, best "e7e5", s 12))
  Assert.Empty(e)

[<Fact>]
let ``takeback to the engine's turn makes it search`` () =
  // human black: the engine opened, the human answered; back one ply is the engine's turn
  let st, _ = run ready [ Start (blitz, false, facts true 0, s 0); Started 1; EngineMoved (facts false 1, s 3); HumanMoved (facts true 2, s 8) ]
  let st, _ = run st [ Started 2; EngineMoved (facts false 3, s 10) ]
  let _, e = step st (TookBack (facts true 2, s 11))
  Assert.Equal(1, (searches e).Length)

[<Fact>]
let ``force stops the engine's search; resign and abort end the game`` () =
  let _, e = step engineThinking Force
  Assert.Equal<Effect list>([ StopEngine ], e)
  let _, e = step playing Force
  Assert.Empty(e)
  let _, e = step playing (Resign (s 3))
  Assert.Equal(Some ("Loss", "Resignation"), ended e)
  let _, e = step engineThinking (Abort (s 12))
  Assert.Equal(Some ("Aborted", "Agreement"), ended e)
  Assert.Contains(StopEngine, e)

[<Fact>]
let ``the side is locked during a game`` () =
  let st, _ = step playing (SideChanged false)
  Assert.True(st.HumanWhite)
  let st, _ = step ready (SideChanged false)
  Assert.False(st.HumanWhite)

[<Fact>]
let ``a new game while the engine thinks stops it and drops its search`` () =
  let st, e = step engineThinking (Start (blitz, true, facts true 0, s 30))
  Assert.Equal(StopEngine, List.head e)
  let _, e = step st (Result (1, best "e7e5", s 12))
  Assert.Empty(e)

[<Fact>]
let ``an engine that fails to start before the game began records no game`` () =
  let st, _ = step initial (Start (blitz, true, facts true 0, s 0))
  let st, e = step st (EngineGone ("no such file", s 1))
  Assert.Equal(Idle, st.Phase)
  Assert.True((ended e).IsNone)
  let st, e = step st LoadEngine
  Assert.Contains(StartEngine, e)
  let _, e = step st LoadEngine
  Assert.Empty(e)

[<Fact>]
let ``a failed engine is dropped, so the next game starts a new one`` () =
  let st, e = step engineThinking (Result (0, EngineFailed ("x", "exited"), s 12))
  Assert.Contains(QuitEngine, e)
  Assert.Equal(NotStarted, st.Engine)
  let _, e = step st (Start (blitz, true, facts true 0, s 20))
  Assert.Contains(StartEngine, e)

[<Fact>]
let ``a failure while the engine starts records no game, and resign while waiting records none`` () =
  let st, _ = step initial (Start (blitz, true, facts true 0, s 0))
  let _, e = step st (Result (0, EngineFailed ("x", "no net"), s 1))
  Assert.Empty(e)
  let st2, e = step st (Resign (s 1))
  Assert.True((ended e).IsNone)
  Assert.Equal(Idle, st2.Phase)
  // the clock does not run while the engine loads
  Assert.Equal(s 60, remaining st true (s 30))

[<Fact>]
let ``the status says initializing before the engine starts`` () =
  let _, e = step initial (Start (blitz, true, facts true 0, s 0))
  Assert.Equal<Effect list>([ ShowStatus "Initializing engine..."; StartEngine ], e)

[<Fact>]
let ``a bestmove line with no legal move aborts; the Done after a real one does not`` () =
  let _, e = step engineThinking (Result (1, Done "x", s 12))
  Assert.Equal(Some ("Aborted", "Engine"), ended e)
  let st, _ = step engineThinking (Result (1, best "e7e5", s 12))
  // the Done may come while the page plays the move on the board
  let _, e = step st (Result (1, Done "x", s 12))
  Assert.Empty(e)
