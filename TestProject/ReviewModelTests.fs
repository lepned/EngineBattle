module ReviewModelTests

open Xunit
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.GameAccuracyAnalysis
open ChessLibrary.ReviewModel

let private game (moves: string) =
  ChessLibrary.FullPGNParser.parsePgnString ("[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n\n" + moves + " *\n") |> Seq.head

let private pos ply kind = { Ply = ply; PositionCmd = sprintf "position p%d" ply; Kind = kind }
let private three = [| pos 0 Searched; pos 1 Searched; pos 2 Searched |]
let private run state events = events |> List.fold (fun (st, _) e -> step st e) (state, [])
let private line k (move: string) cp =
  Status { EngineStatus.Empty with MultiPV = k; PV = move; PVLongSAN = move + " e7e5"; Eval = CP cp }
let private best move = BestMove { BestMoveInfo.Empty with Move = move }
let private searches effects = effects |> List.choose (function Search (p, _) -> Some p | _ -> None)
let private finished effects = effects |> List.tryPick (function Finished o -> Some o | _ -> None)

let private ready = { initial with Engine = Ready }
/// Reviewing three positions, search 1 on the first.
let private reviewing = fst (run ready [ Start (three, "go nodes 100", 2, false); Started 1 ])

[<Fact>]
let ``the positions are those before each move, then the final one`` () =
  let ps = reviewPositions (game "1. e4 e5 2. Nf3") GameReviewConfig.Default
  Assert.Equal(4, ps.Length)
  Assert.Equal<int list>([ 0; 1; 2; 3 ], ps |> Array.map (fun p -> p.Ply) |> Array.toList)
  Assert.True(ps |> Array.forall (fun p -> p.Kind = Searched))
  Assert.EndsWith("moves e2e4", ps.[1].PositionCmd)

[<Fact>]
let ``a mate at the end is scored by the rules, not searched`` () =
  let ps = reviewPositions (game "1. f3 e5 2. g4 Qh4#") GameReviewConfig.Default
  Assert.Equal(Terminal (Mate -1), ps.[4].Kind)

[<Fact>]
let ``a review sets MultiPV and ucinewgame, then searches the first position`` () =
  let _, e = step ready (Start (three, "go nodes 100", 1, false))
  Assert.Equal(SetMultiPv 1, List.head e)
  Assert.Contains(NewGame, e)
  Assert.Equal<string list>([ "position p0" ], searches e)
  Assert.Contains(ShowPly 0, e)

[<Fact>]
let ``the engine starts first and the review begins when it is ready`` () =
  let st, e = step initial (Start (three, "go nodes 100", 1, false))
  Assert.Equal<Effect list>([ StartEngine ], e)
  let _, e = step st EngineReady
  Assert.Equal<string list>([ "position p0" ], searches e)

[<Fact>]
let ``a bestmove records the lines and moves on, with the eval of that ply`` () =
  let st, _ = run reviewing [ Result (1, line 1 "e2e4" 0.3); Result (1, line 2 "d2d4" 0.2) ]
  let st, e = step st (Result (1, best "e2e4"))
  Assert.Contains(LiveEval (0, CP 0.3), e)
  Assert.Equal<string list>([ "position p1" ], searches e)
  match st.Phase with
  | Reviewing r ->
      Assert.Equal(1, r.Index)
      let o = List.head r.Outcomes
      Assert.Equal("e2e4", o.BestMove)
      Assert.Equal(2, o.Pvs.Length)
      Assert.Equal("d2d4", o.Pvs.[1].BestMove)
  | p -> failwithf "expected Reviewing, got %A" p

[<Fact>]
let ``lines of another search are not this position's`` () =
  let st, e = step reviewing (Result (7, line 1 "a2a3" 9.0))
  Assert.Empty(e)
  let _, e = step st (Result (7, best "a2a3"))
  Assert.Empty(e)

[<Fact>]
let ``the last bestmove finishes the review with every outcome in order`` () =
  let st, e = run reviewing [ Result (1, line 1 "e2e4" 0.3); Result (1, best "e2e4"); Started 2; Result (2, best "e7e5"); Started 3 ]
  Assert.True((finished e).IsNone)
  let st, e = step st (Result (3, best "g1f3"))
  let outcomes = (finished e).Value
  Assert.Equal<string list>([ "e2e4"; "e7e5"; "g1f3" ], outcomes |> Array.map (fun o -> o.BestMove) |> Array.toList)
  Assert.Equal(Idle, st.Phase)

[<Fact>]
let ``book and terminal positions are not searched`` () =
  let ps = [| pos 0 BookMove; pos 1 Searched; pos 2 (Terminal (Mate 1)) |]
  let st, e = step ready (Start (ps, "go nodes 100", 1, false))
  Assert.Equal<string list>([ "position p1" ], searches e)
  let _, e = run st [ Started 1; Result (1, best "e7e5") ]
  let outcomes = (finished e).Value
  Assert.Equal(NA, outcomes.[0].Eval)
  Assert.Equal(Mate 1, outcomes.[2].Eval)
  Assert.Contains(LiveEval (2, Mate 1), e)

[<Fact>]
let ``cancel stops the search, discards the review and ignores its late lines`` () =
  let st, e = step reviewing Cancel
  Assert.Equal<Effect list>([ StopEngine; Cancelled ], e)
  let _, e = step st (Result (1, best "e2e4"))
  Assert.Empty(e)

[<Fact>]
let ``a new review after a cancel takes only its own search`` () =
  let st, _ = run reviewing [ Cancel; Start (three, "go nodes 100", 1, false); Started 2 ]
  let _, e = step st (Result (1, best "e2e4"))
  Assert.Empty(e)

[<Fact>]
let ``a failed engine fails the review and is dropped`` () =
  let st, e = step reviewing (Result (0, EngineFailed ("x", "exited")))
  Assert.Contains(QuitEngine, e)
  Assert.Contains(Failed "exited", e)
  Assert.Equal(NotStarted, st.Engine)
  let _, e = step st (Start (three, "go nodes 100", 1, false))
  Assert.Contains(StartEngine, e)

[<Fact>]
let ``a search ending without a move fails the review`` () =
  let st, e = step reviewing (Result (1, SearchStopped "x"))
  Assert.True(e |> List.exists (function Failed _ -> true | _ -> false))
  Assert.Equal(Idle, st.Phase)

[<Fact>]
let ``node searches get ucinewgame before each search`` () =
  let _, e = run ready [ Start (three, "go nodes 100", 1, true); Started 1; Result (1, best "e2e4") ]
  Assert.Contains(NewGame, e)

[<Fact>]
let ``scoring the outcomes gives one result per move`` () =
  let g = game "1. e4 e5 2. Nf3"
  let config = GameReviewConfig.Default
  let ps = reviewPositions g config
  let o eval mv = { Eval = CP eval; Pvs = [| { Eval = CP eval; BestMove = mv; PV = mv; Depth = 5; Nodes = 100L } |]; BestMove = mv }
  let r = scoreGame g config [| o 0.3 "e2e4"; o 0.3 "e7e5"; o 0.3 "g1f3"; o 0.3 "b8c6" |] "Eng"
  Assert.Equal(3, r.Moves.Length)
  Assert.Equal("Eng", r.AnalysisEngine)
  Assert.Equal("g1f3", r.Moves.[2].UciMove)
  Assert.Equal(ps.Length, 4)

[<Fact>]
let ``a bestmove line without a legal move still moves the review on`` () =
  let st, e = step reviewing (Result (1, line 1 "e2e4" 0.3))
  let st, e = step st (Result (1, Done "x"))
  Assert.Equal<string list>([ "position p1" ], searches e)
  match st.Phase with
  | Reviewing r -> Assert.Equal("", (List.head r.Outcomes).BestMove)
  | p -> failwithf "%A" p
  // after a real bestmove the Done that follows is not a second answer
  let st, _ = run st [ Started 2; Result (2, best "e7e5") ]
  let st2, e = step st (Result (2, Done "x"))
  Assert.Empty(e)
