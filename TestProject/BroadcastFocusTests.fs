module BroadcastFocusTests

open System
open Xunit
open ChessLibrary.BroadcastFocus

let private s (n: float) = TimeSpan.FromSeconds n
let private live key rating = { Key = key; Moves = 20; Finished = false; Rating = rating }
let private over key rating = { live key rating with Finished = true }
let private unstarted key rating = { live key rating with Moves = 0 }
let private shown effects = effects |> List.choose (function Show (k, _) -> Some k | _ -> None)

/// A round where the user follows game "a", which the page put in focus.
let private following () =
  let st, _ = step (initial "r1") (Games ([ live "a" 5000 ], s 0.0))
  st

[<Fact>]
let ``on arrival the strongest live game is shown, not a finished or unstarted one`` () =
  let _, eff = step (initial "r1") (Games ([ over "x" 6000; unstarted "y" 7000; live "a" 5000; live "b" 5200 ], s 0.0))
  Assert.Equal<string list>([ "b" ], shown eff)

[<Fact>]
let ``a finished game the page chose gives way to a live one`` () =
  let st, _ = step (initial "r1") (Games ([ over "x" 6000 ], s 0.0))
  Assert.Equal(Some "x", st.Focus)
  let st, eff = step st (Games ([ over "x" 6000; live "a" 5000 ], s 1.0))
  Assert.Equal<string list>([ "a" ], shown eff)
  Assert.False st.Auto

[<Fact>]
let ``the user's own pick stays even when it is finished`` () =
  let st, _ = step (following ()) (Picked ("x", [ over "x" 6000; live "a" 5000 ]))
  let _, eff = step st (Games ([ over "x" 6000; live "a" 5000; live "b" 5100 ], s 2.0))
  Assert.Empty(shown eff)

[<Fact>]
let ``the followed game ends with another live: the result holds, then the strongest live game`` () =
  let st, eff = step (following ()) (Games ([ over "a" 5000; live "b" 4000 ], s 5.0))
  Assert.Empty(shown eff)
  Assert.Equal(Holding ("b", s 15.0), st.Pending)
  let st, eff = step st (Tick ([ over "a" 5000; live "b" 4000 ], s 10.0))
  Assert.Empty(shown eff)
  // a stronger game started during the hold: it is the one taken
  let st, eff = step st (Tick ([ over "a" 5000; live "b" 4000; live "c" 6000 ], s 15.0))
  Assert.Equal<string list>([ "c" ], shown eff)
  Assert.Equal(NoPending, st.Pending)

[<Fact>]
let ``the followed game ends with none live: wait, and take the next one the moment it starts`` () =
  let st, _ = step (following ()) (Games ([ over "a" 5000 ], s 5.0))
  Assert.True(match st.Pending with Waiting _ -> true | _ -> false)
  // the next game appears before its first move: not yet
  let st, eff = step st (Games ([ over "a" 5000; unstarted "b" 5000 ], s 600.0))
  Assert.Empty(shown eff)
  let st, eff = step st (Games ([ over "a" 5000; live "b" 5000 ], s 610.0))
  Assert.Equal<string list>([ "b" ], shown eff)
  Assert.Equal(NoPending, st.Pending)

[<Fact>]
let ``with the round over, the tour's rounds are asked for once a minute and an ongoing one is started`` () =
  let st, eff = step (following ()) (Games ([ over "a" 5000; over "z" 4000 ], s 5.0))
  Assert.Contains(FetchRounds, eff)
  let st, eff = step st (Tick ([ over "a" 5000; over "z" 4000 ], s 30.0))
  Assert.Empty eff
  let st, eff = step st (Tick ([ over "a" 5000; over "z" 4000 ], s 66.0))
  Assert.Contains(FetchRounds, eff)
  let _, eff = step st (RoundsFetched ([ { Id = "r1"; Ongoing = false }; { Id = "r2"; Ongoing = true } ], s 67.0))
  Assert.Equal<Effect list>([ StartRound "r2" ], eff)

[<Fact>]
let ``no next round is started while the round still has a game to come`` () =
  // the round is not over (b is still to come): no fetch, and a list arriving anyway starts nothing
  let st, eff = step (following ()) (Games ([ over "a" 5000; unstarted "b" 5000 ], s 5.0))
  Assert.DoesNotContain(FetchRounds, eff)
  let _, eff = step st (RoundsFetched ([ { Id = "r2"; Ongoing = true } ], s 6.0))
  Assert.Empty eff

[<Fact>]
let ``browsing away, Stay or a pick ends the hand-over`` () =
  let holding, _ = step (following ()) (Games ([ over "a" 5000; live "b" 4000 ], s 5.0))
  for ev in [ FollowChanged false; Stay; Picked ("a", [ over "a" 5000; live "b" 4000 ]) ] do
    let st, _ = step holding ev
    Assert.Equal(NoPending, st.Pending)
    let _, eff = step st (Tick ([ over "a" 5000; live "b" 4000 ], s 20.0))
    Assert.Empty(shown eff)

[<Fact>]
let ``a game that ends while the user browses it is not handed over`` () =
  let st, _ = step (following ()) (FollowChanged false)
  let st, eff = step st (Games ([ over "a" 5000; live "b" 4000 ], s 5.0))
  Assert.Empty eff
  Assert.Equal(NoPending, st.Pending)

[<Fact>]
let ``a new round starts from scratch`` () =
  let st, _ = step (following ()) (RoundStarted "r2")
  Assert.Equal(initial "r2", st)
