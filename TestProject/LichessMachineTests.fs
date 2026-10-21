/// The Lichess bot's two state machines on their own: LichessGameMachine (one game) and
/// LichessBotMachine (the account: places, challenges, matchmaking, engines). Events in, effects
/// out - no threads, no server, no engine.
module LichessMachineTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.LichessBotLogic

// ---------------------------------------------------------------- one game

module G = LichessGameMachine

let private config book depth : G.Config =
    { BotId = "testbot"; Greeting = "hi"; Goodbye = "gg"; Limit = FixedNodes 1; Book = book; BookDepth = depth; AbortSeconds = 20 }

let private info = { EngineName = "Ceres"; Net = "C1.onnx"; Limit = FixedNodes 1; Playing = 1; MaxGames = 1 }

let private fullAs (botWhite: bool) =
    let line =
        sprintf """{"type":"gameFull","id":"g1","variant":{"key":"standard"},"rated":false,"speed":"blitz","white":{"id":"%s","name":"W"},"black":{"id":"%s","name":"B"},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}"""
            (if botWhite then "testbot" else "alice") (if botWhite then "alice" else "testbot")
    match parseGameMessage line with
    | Some m -> G.Message (m, info)
    | None -> failwith "no gameFull"

let private stateOf moves status =
    let line = sprintf """{"type":"gameState","moves":"%s","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"%s"}""" moves status
    match parseGameMessage line with
    | Some m -> G.Message (m, info)
    | None -> failwith "no gameState"

let private chat room text =
    G.Message (GameMessage.Chat (room, "bob", text), info)

/// Runs events from a start; the state and every effect, in order.
let private run (st: G.State) (events: G.Event list) =
    events |> List.fold (fun (st, all) e -> let st, effects = G.step st e in st, all @ effects) (st, [])

let private moves (effects: G.Effect list) =
    effects |> List.choose (function G.SendMove (c, uci) -> Some (c, uci) | _ -> None)

[<Fact>]
let ``a game: the greeting, a search on the bot's turn, the move, and the PGN and goodbye at the end`` () =
    let st, effects = run (G.initial (config false 8) "g1") [ fullAs true ]
    Assert.Contains(G.Chat ("player", "hi"), effects)
    Assert.Contains(effects, fun e -> match e with G.Search (0, "position startpos", "go nodes 1") -> true | _ -> false)
    let st, effects = run st [ G.Searched (0, Some "e2e4") ]
    Assert.Equal<(int * string) list>([ 0, "e2e4" ], moves effects)
    // the opponent's move: the bot's turn again at 2; the greeting is not said twice
    let st, effects = run st [ G.MoveDelivered 0; stateOf "e2e4 e7e5" "started" ]
    Assert.Contains(effects, fun e -> match e with G.Search (2, _, _) -> true | _ -> false)
    Assert.DoesNotContain(G.Chat ("player", "hi"), effects)
    let st, effects = run st [ G.Searched (2, Some "g1f3"); G.MoveDelivered 2; stateOf "e2e4 e7e5 g1f3" "resign" ]
    Assert.True(st.Over)
    Assert.Contains(effects, fun e -> match e with G.WritePgn (_, s, _) -> s.Status = "resign" | _ -> false)
    Assert.Contains(G.Chat ("player", "gg"), effects)
    Assert.Equal(G.Finished, List.last effects)

[<Fact>]
let ``as Black the bot waits for White's move`` () =
    let st, effects = run (G.initial (config false 8) "g1") [ fullAs false ]
    Assert.DoesNotContain(effects, fun e -> match e with G.Search _ | G.LookUpBook _ -> true | _ -> false)
    let _, effects = run st [ stateOf "d2d4" "started" ]
    Assert.Contains(effects, fun e -> match e with G.Search (1, _, _) -> true | _ -> false)

[<Fact>]
let ``the book plays while it may, the engine after the depth and after the book has nothing`` () =
    let st, effects = run (G.initial (config true 2) "g1") [ fullAs true ]
    Assert.Contains(effects, fun e -> match e with G.LookUpBook (0, _) -> true | _ -> false)
    let st, effects = run st [ G.BookAnswered (0, Some "e2e4") ]
    Assert.Equal<(int * string) list>([ 0, "e2e4" ], moves effects)
    // the chat hears that the move came from the book
    let _, said = run st [ chat "spectator" "!eval" ]
    Assert.Equal<G.Effect list>([ G.Chat ("spectator", G.bookNote) ], said)
    let st, effects = run st [ G.MoveDelivered 0; stateOf "e2e4 e7e5" "started" ]
    Assert.Equal<Set<int>>(set [ 0 ], st.BookPlies)
    // ply 2 is past the depth of 2: the engine
    Assert.Contains(effects, fun e -> match e with G.Search (2, _, _) -> true | _ -> false)
    Assert.DoesNotContain(effects, fun e -> match e with G.LookUpBook _ -> true | _ -> false)
    // a book with nothing: the engine at once, and the book is not asked again
    let st, effects = run (G.initial (config true 8) "g1") [ fullAs true; G.BookAnswered (0, None) ]
    Assert.Contains(effects, fun e -> match e with G.Search (0, _, _) -> true | _ -> false)
    let _, effects = run st [ G.Searched (0, Some "e2e4"); G.MoveDelivered 0; stateOf "e2e4 e7e5" "started" ]
    Assert.DoesNotContain(effects, fun e -> match e with G.LookUpBook _ -> true | _ -> false)

[<Fact>]
let ``a move Lichess did not take is sent again, and a late answer for an old position is ignored`` () =
    let st, _ = run (G.initial (config false 8) "g1") [ fullAs true; G.Searched (0, Some "e2e4") ]
    let st, effects = run st [ G.MoveRefused (0, "HTTP 500") ]
    Assert.Contains(G.RetryAfter (0, 1000), effects)
    let st, effects = run st [ G.Retry 0 ]
    Assert.Equal<(int * string) list>([ 0, "e2e4" ], moves effects)
    // delivered; a search answer for ply 0 that comes late changes nothing
    let st, _ = run st [ G.MoveDelivered 0 ]
    let _, effects = run st [ G.Searched (0, Some "d2d4") ]
    Assert.Empty(effects)

[<Fact>]
let ``an engine that cannot play resigns, and the game is still written when Lichess ends it`` () =
    let st, effects = run (G.initial (config false 8) "g1") [ fullAs true; G.Searched (0, None) ]
    Assert.Contains(G.Resign "the engine cannot play", effects)
    let st, effects = run st [ stateOf "" "resign" ]
    Assert.True(st.Over)
    Assert.Contains(effects, fun e -> match e with G.WritePgn _ -> true | _ -> false)

[<Fact>]
let ``a dropped stream is opened again later and later, a gone game ends`` () =
    let st, effects = run (G.initial (config false 8) "g1") [ G.StreamEnded (0, "reset") ]
    Assert.Contains(G.OpenStream 5000, effects)
    let st, effects = run st [ G.StreamEnded (0, "reset") ]
    Assert.Contains(G.OpenStream 10000, effects)
    let st, effects = run st [ G.StreamEnded (404, "gone") ]
    Assert.True(st.Over)
    Assert.Contains(G.Finished, effects)

[<Fact>]
let ``the chat answers spectators with the engine's view and keeps it from the opponent`` () =
    let st, _ = run (G.initial (config false 8) "g1") [ fullAs true; G.EngineView (Some "+0.3", Some "Policy: e4 40%", Some (Some 0.3, None)) ]
    let _, said = run st [ chat "spectator" "!eval"; chat "player" "!top"; chat "spectator" "!top" ]
    Assert.Equal<G.Effect list>(
        [ G.Chat ("spectator", "+0.3"); G.Chat ("player", "I don't tell my opponent, sorry."); G.Chat ("spectator", "Policy: e4 40%") ], said)

// ---------------------------------------------------------------- the account

module B = LichessBotMachine

let private t0 = DateTime(2026, 10, 20, 12, 0, 0, DateTimeKind.Utc)
let private filter1 = { defaultFilter with MaxGames = 1 }

let private challenge id (who: string) =
    match parseEvent (sprintf """{"type":"challenge","challenge":{"id":"%s","challenger":{"id":"%s","name":"%s","title":null,"rating":1500},"variant":{"key":"standard"},"rated":false,"speed":"blitz","timeControl":{"type":"clock","limit":180,"increment":2}}}""" id (who.ToLowerInvariant()) who) with
    | Some (ChallengeArrived c) -> c
    | _ -> failwith "no challenge"

let private request name : B.Request = { Name = name; Seconds = 180; Increment = 2; Rated = false; Color = "white"; Auto = false }

let private runB (st: B.State<string>) (events: B.Event<string> list) =
    events |> List.fold (fun (st, all) e -> let st, effects = B.step st e in st, all @ effects) (st, [])

let private online matchmaking =
    fst (B.step (B.initial { Filter = filter1; Matchmaking = matchmaking }) (B.WentOnline ("testbot", t0)))

[<Fact>]
let ``a challenge on its way holds the place: one arriving meanwhile is declined later`` () =
    let st, effects = runB (online None) [ B.Send (1, request "alice", t0); B.ChallengeIn (challenge "c9" "Carol", t0) ]
    Assert.Contains(B.PostChallenge (1, request "alice"), effects)
    Assert.Contains(B.Decline ("c9", "later"), effects)
    // and a second one of its own is refused
    let _, effects = runB st [ B.Send (2, request "bob", t0) ]
    Assert.Contains(effects, fun e -> match e with B.Answer (2, Error why) -> why.Contains "full" | _ -> false)

[<Fact>]
let ``an answer before the challenge's id is known is the answer, and frees the place`` () =
    let st, effects = runB (online None) [ B.Send (1, request "erin", t0); B.Declined ("x1", "Not now.", t0); B.Sent (1, B.SentAs "x1", t0) ]
    Assert.Contains(B.Answer (1, Ok "x1"), effects)
    Assert.Equal("erin declined: Not now.", st.LastAnswer)
    Assert.Equal(0, B.taken st)
    Assert.True(st.DeclinedBy.ContainsKey "erin")

[<Fact>]
let ``an accepted challenge becomes a game; one not answered is withdrawn`` () =
    let st, _ = runB (online None) [ B.Send (1, request "alice", t0); B.Sent (1, B.SentAs "a1", t0) ]
    let st2, effects = runB st [ B.GameStarted ("a1", t0) ]
    Assert.Contains(B.StartGame "a1", effects)
    Assert.Equal("alice accepted the challenge", st2.LastAnswer)
    Assert.Equal(1, B.taken st2)
    // no answer in 3 minutes: withdrawn on Lichess
    let st3, effects = runB st [ B.Tick (t0.AddMinutes 3.5, 0) ]
    Assert.Contains(B.CancelOnLichess "a1", effects)
    Assert.Equal(0, B.taken st3)

[<Fact>]
let ``engines wait between games; a game gets an idle one or none, and shutting down quits them`` () =
    let st, effects = runB (online None) [ B.EngineBack "e1"; B.EngineWanted 1; B.EngineWanted 2 ]
    Assert.Equal<B.Effect<string> list>([ B.GiveEngine (1, Some "e1"); B.GiveEngine (2, None) ], effects)
    let _, effects = runB st [ B.EngineBack "e2"; B.Shutdown; B.EngineBack "e3" ]
    Assert.Equal<B.Effect<string> list>([ B.Quit "e2"; B.Quit "e3" ], effects)

[<Fact>]
let ``going offline withdraws the challenges sent`` () =
    let st, _ = runB (online None) [ B.Send (1, request "alice", t0); B.Sent (1, B.SentAs "a1", t0) ]
    let st, effects = runB st [ B.Offline ]
    Assert.Contains(B.CancelOnLichess "a1", effects)
    Assert.False(st.Online)
    // one still on its way when the bot went offline is withdrawn when its id comes
    let st, _ = runB (online None) [ B.Send (1, request "bob", t0) ]
    let st, effects = runB st [ B.Offline; B.Sent (1, B.SentAs "b1", t0) ]
    Assert.Contains(B.CancelOnLichess "b1", effects)
    Assert.Equal(0, B.taken st)

[<Fact>]
let ``matchmaking reads the bots, challenges one in range, then another after a decline, and says when none fits`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 1500; Rated = false; PauseSeconds = 60; GapSeconds = 30 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = Some blitz; Rapid = None; Classical = None }
    let st = online (Some m)
    // no pause after going online: the bot list is read at the first tick
    let st, effects = runB st [ B.Tick (t0.AddSeconds 2.0, 0) ]
    Assert.Equal<B.Effect<string> list>([ B.ListBots ], effects)
    let bots = [| bot "testbot" 2000; bot "Near" 2100; bot "Far" 2600 |]
    let st, effects = runB st [ B.Listed (Ok bots, t0.AddSeconds 62.0); B.Tick (t0.AddSeconds 63.0, 0) ]
    let token, r = effects |> List.pick (function B.PostChallenge (t, r) -> Some (t, r) | _ -> None)
    Assert.Equal("Near", r.Name)
    Assert.True(r.Auto)
    // one of its own waits: no second challenge, even after the gap
    let st, effects = runB st [ B.Sent (token, B.SentAs "n1", t0.AddSeconds 63.0); B.Tick (t0.AddSeconds 100.0, 0) ]
    Assert.DoesNotContain(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)
    // declined: Near is skipped for an hour, and nobody else fits - said once
    let st, effects = runB st [ B.Declined ("n1", "No.", t0.AddSeconds 101.0); B.Tick (t0.AddSeconds 140.0, 0); B.Tick (t0.AddSeconds 180.0, 0) ]
    Assert.Equal(1, effects |> List.filter (function B.Log l -> l.StartsWith "matchmaking: no bot online fits" | _ -> false) |> List.length)
    Assert.DoesNotContain(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)
    // an hour later Near may be asked again
    let _, effects = runB st [ B.Listed (Ok bots, t0.AddMinutes 70.0); B.Tick (t0.AddMinutes 70.5, 0) ]
    Assert.Contains(effects, fun e -> match e with B.PostChallenge (_, r) -> r.Name = "Near" | _ -> false)

[<Fact>]
let ``an opponent that does not make its first move is aborted, and the game is not written`` () =
    let st, effects = run (G.initial (config false 8) "g1") [ fullAs false ]
    Assert.Contains(G.FirstMoveTimer (0, 20000), effects)
    let st, effects = run st [ G.FirstMoveDue 0 ]
    Assert.Contains(G.Abort "W", effects)
    let _, effects = run st [ stateOf "" "aborted" ]
    Assert.DoesNotContain(effects, fun e -> match e with G.WritePgn _ | G.Chat _ -> true | _ -> false)
    Assert.Equal(G.Finished, List.last effects)
    // as White the timer waits for Black's reply; a move in time ends it
    let st, effects = run (G.initial (config false 8) "g1") [ fullAs true; G.Searched (0, Some "e2e4"); G.MoveDelivered 0; stateOf "e2e4" "started" ]
    Assert.Contains(G.FirstMoveTimer (1, 20000), effects)
    let _, effects = run st [ stateOf "e2e4 e7e5" "started"; G.FirstMoveDue 1 ]
    Assert.DoesNotContain(effects, fun e -> match e with G.Abort _ -> true | _ -> false)

[<Fact>]
let ``matchmaking uses the start rating while the bot has no games, and skips a bot that did not move`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 2000; Rated = false; PauseSeconds = 0; GapSeconds = 0 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = blitz; Rapid = None; Classical = None }
    // the bot itself has no blitz games: 2000, so Near (2100) and not Strong (2900)
    let bots = [| bot "testbot" None; bot "Near" (Some 2100); bot "Strong" (Some 2900) |]
    let st, _ = runB (online (Some m)) [ B.Listed (Ok bots, t0) ]
    // every draw: Near, whatever the random number says
    for roll in [ 0; 1 <<< 16; 2 <<< 16; 3 <<< 16 ] do
        let _, effects = runB st [ B.Tick (t0.AddSeconds 1.0, roll) ]
        Assert.Contains(effects, fun e -> match e with B.PostChallenge (_, r) -> r.Name = "Near" | _ -> false)
    let _, effects = runB st [ B.NoShow ("Near", t0); B.Tick (t0.AddSeconds 1.0, 0) ]
    Assert.DoesNotContain(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)
    // 6 hours on, Near may be challenged again
    let _, effects = runB st [ B.NoShow ("Near", t0); B.Listed (Ok bots, t0.AddHours 7.0); B.Tick (t0.AddHours 7.0, 0) ]
    Assert.Contains(effects, fun e -> match e with B.PostChallenge (_, r) -> r.Name = "Near" | _ -> false)

[<Fact>]
let ``a game asking for an engine while one is on its way back waits for it, unless it is lost`` () =
    let st, effects = runB (online None) [ B.EngineComing; B.EngineWanted 1 ]
    Assert.Empty(effects)
    let _, effects = runB st [ B.EngineBack "e1" ]
    Assert.Equal<B.Effect<string> list>([ B.GiveEngine (1, Some "e1") ], effects)
    // lost on the way: the waiting game starts its own
    let _, effects = runB st [ B.EngineLost ]
    Assert.Equal<B.Effect<string> list>([ B.GiveEngine (1, None) ], effects)
    // shutting down: nobody waits
    let _, effects = runB st [ B.Shutdown ]
    Assert.Equal<B.Effect<string> list>([ B.GiveEngine (1, None) ], effects)

// ---------------------------------------------------------------- one engine for every game

module Q = LichessEngineQueue

let private runQ (st: Q.State<string>) (events: Q.Event<string> list) =
    events |> List.fold (fun (st, all) e -> let st, effects = Q.step st e in st, all @ effects) (st, [])

let private searches (effects: Q.Effect<string> list) =
    effects |> List.choose (function Q.RunSearch (e, r) -> Some (e, r.Token, r.Position) | _ -> None)

[<Fact>]
let ``the games' searches run one at a time on the one engine, each answered to its game`` () =
    let st, effects = runQ Q.initial [ Q.Started "e1"; Q.Ask (1, "position startpos", "go nodes 1"); Q.Ask (2, "position startpos moves d2d4", "go nodes 1") ]
    // the second waits for the first
    Assert.Equal<(string * int * string) list>([ "e1", 1, "position startpos" ], searches effects)
    let st, effects = runQ st [ Q.Done (1, Some "e2e4") ]
    Assert.Equal(Q.Reply (1, Some "e2e4"), effects.Head)
    Assert.Equal<(string * int * string) list>([ "e1", 2, "position startpos moves d2d4" ], searches effects)
    let _, effects = runQ st [ Q.Done (2, Some "g8f6") ]
    Assert.Equal<Q.Effect<string> list>([ Q.Reply (2, Some "g8f6") ], effects)

[<Fact>]
let ``a failed engine is started again and the search tried once more; twice is None`` () =
    let st, _ = runQ Q.initial [ Q.Started "e1"; Q.Ask (1, "p", "g") ]
    let st, effects = runQ st [ Q.Done (1, None) ]
    Assert.Contains(Q.QuitEngine "e1", effects)
    Assert.Contains(Q.StartEngine, effects)
    Assert.DoesNotContain(effects, fun e -> match e with Q.Reply _ -> true | _ -> false)
    let st, effects = runQ st [ Q.Started "e2" ]
    Assert.Equal<(string * int * string) list>([ "e2", 1, "p" ], searches effects)
    let _, effects = runQ st [ Q.Done (1, None) ]
    Assert.Contains(Q.Reply (1, None), effects)
    Assert.Contains(Q.QuitEngine "e2", effects)

[<Fact>]
let ``no engine before the first search starts one; one that does not start answers every search None`` () =
    let st, effects = runQ Q.initial [ Q.Ask (1, "p", "g"); Q.Ask (2, "p", "g") ]
    Assert.Equal<Q.Effect<string> list>([ Q.StartEngine ], effects)
    let _, effects = runQ st [ Q.StartFailed "no net" ]
    Assert.Contains(Q.Reply (1, None), effects)
    Assert.Contains(Q.Reply (2, None), effects)

[<Fact>]
let ``shutting down quits the engine, answers what waits, and answers a running search as it ends`` () =
    let st, _ = runQ Q.initial [ Q.Started "e1"; Q.Ask (1, "p", "g"); Q.Ask (2, "p", "g") ]
    let st, effects = runQ st [ Q.Shutdown ]
    Assert.Equal<Q.Effect<string> list>([ Q.QuitEngine "e1"; Q.Reply (2, None) ], effects)
    let st, effects = runQ st [ Q.Done (1, None); Q.Ask (3, "p", "g") ]
    Assert.Equal<Q.Effect<string> list>([ Q.Reply (1, None); Q.Reply (3, None) ], effects)

[<Fact>]
let ``the page's view carries the eval for the bar, and none for a book move`` () =
    let shown (effects: G.Effect list) = effects |> List.choose (function G.Show v -> Some v | _ -> None) |> List.last
    let st, _ = run (G.initial (config false 8) "g1") [ fullAs true; G.EngineView (Some "+0.30", None, Some (Some 0.3, None)); G.Searched (0, Some "e2e4") ]
    let _, effects = run st [ G.MoveDelivered 0; stateOf "e2e4 e7e5" "started" ]
    let v = shown effects
    Assert.Equal(Nullable 0.3, v.EvalPawns)
    Assert.False(v.EvalMate.HasValue)
    let st, _ = run (G.initial (config true 8) "g1") [ fullAs true; G.EngineView (None, None, Some (None, Some 3)); G.BookAnswered (0, Some "e2e4") ]
    let _, effects = run st [ G.MoveDelivered 0; stateOf "e2e4 e7e5" "started" ]
    let v = shown effects
    Assert.False(v.EvalPawns.HasValue || v.EvalMate.HasValue)
    Assert.Equal("book", v.Eval)

[<Fact>]
let ``a game that ends drops its waiting search and stops its running one`` () =
    let st, _ = runQ Q.initial [ Q.Started "e1"; Q.Ask (1, "p", "g"); Q.Ask (2, "p", "g"); Q.Ask (3, "p", "g") ]
    // waiting: dropped and answered, the others keep their order
    let st, effects = runQ st [ Q.Cancel 2 ]
    Assert.Equal<Q.Effect<string> list>([ Q.Reply (2, None) ], effects)
    // running: stopped; its answer comes as usual and the next search runs
    let st, effects = runQ st [ Q.Cancel 1 ]
    Assert.Equal<Q.Effect<string> list>([ Q.StopSearch "e1" ], effects)
    let _, effects = runQ st [ Q.Done (1, Some "e2e4") ]
    Assert.Equal<(string * int * string) list>([ "e1", 3, "p" ], searches effects)
    // a token it does not hold: nothing
    let _, effects = runQ st [ Q.Cancel 9 ]
    Assert.Empty(effects)

[<Fact>]
let ``the page's view has the players' ratings`` () =
    let line = """{"type":"gameFull","id":"g1","variant":{"key":"standard"},"rated":true,"speed":"blitz","white":{"id":"testbot","name":"TestBot","rating":2000},"black":{"id":"alice","name":"Alice","rating":1850},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}"""
    let _, effects = run (G.initial (config false 8) "g1") [ G.Message ((parseGameMessage line).Value, info) ]
    let v = effects |> List.pick (function G.Show v -> Some v | _ -> None)
    Assert.Equal(Nullable 2000, v.WhiteRating)
    Assert.Equal(Nullable 1850, v.BlackRating)

[<Fact>]
let ``with two games at once matchmaking fills the free place while a game runs, the pause only after a game ends`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 2000; Rated = false; PauseSeconds = 60; GapSeconds = 30 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = Some blitz; Rapid = None; Classical = None }
    let st = fst (B.step (B.initial { Filter = { defaultFilter with MaxGames = 2 }; Matchmaking = Some m }) (B.WentOnline ("testbot", t0)))
    let st, _ = runB st [ B.Listed (Ok [| bot "A" 2000; bot "B" 2050 |], t0.AddSeconds 61.0) ]
    // a game starts 70 s in: the other place is filled at once, not a pause later
    let st, _ = runB st [ B.GameStarted ("g1", t0.AddSeconds 70.0) ]
    let _, effects = runB st [ B.Tick (t0.AddSeconds 72.0, 0) ]
    Assert.Contains(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)
    // a game that ends starts the pause: nothing until 60 s after it
    let st, _ = runB st [ B.GameEnded ("g1", t0.AddSeconds 80.0) ]
    let _, effects = runB st [ B.Tick (t0.AddSeconds 100.0, 0) ]
    Assert.DoesNotContain(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)
    let _, effects = runB st [ B.Tick (t0.AddSeconds 141.0, 0) ]
    Assert.Contains(effects, fun e -> match e with B.PostChallenge _ -> true | _ -> false)

[<Fact>]
let ``the wait for the first move grows with the clock: 10 percent, at least 20 s`` () =
    let fullWith (ms: int64) =
        let line = sprintf """{"type":"gameFull","id":"g1","variant":{"key":"standard"},"rated":false,"speed":"classical","white":{"id":"alice","name":"Alice"},"black":{"id":"testbot","name":"TestBot"},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":%d,"btime":%d,"winc":0,"binc":0,"status":"started"}}""" ms ms
        G.Message ((parseGameMessage line).Value, info)
    let timer ms = run (G.initial (config false 8) "g1") [ fullWith ms ] |> snd |> List.pick (function G.FirstMoveTimer (_, t) -> Some t | _ -> None)
    Assert.Equal(20000, timer 60000L)        // 1+0
    Assert.Equal(20000, timer 180000L)       // 3+2
    Assert.Equal(60000, timer 600000L)       // 10+0
    Assert.Equal(180000, timer 1800000L)     // 30+0
    Assert.Equal(540000, timer 5400000L)     // 90+30

[<Fact>]
let ``a rate limit pauses matchmaking a minute, doubling while it lasts, and a challenge that goes through resets it`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 2000; Rated = false; PauseSeconds = 0; GapSeconds = 0 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = Some blitz; Rapid = None; Classical = None }
    let st0, _ = runB (online (Some m)) [ B.Listed (Ok [| bot "A" 2000 |], t0) ]
    /// one challenge at `at`, answered with `result`: the pause it sets, in minutes from `at`
    let attempt (st: B.State<string>) (at: DateTime) result =
        // a fresh list: older than 5 minutes it is read again instead of challenging
        let st, effects = runB st [ B.Listed (Ok [| bot "A" 2000; bot "B" 2000 |], at); B.Tick (at, 0) ]
        let token = effects |> List.pick (function B.PostChallenge (t, _) -> Some t | _ -> None)
        let st, _ = runB st [ B.Sent (token, result, at) ]
        st, (st.NextAuto - at).TotalMinutes
    let limited = B.RateLimitedBy ("", None)
    let st, wait1 = attempt st0 (t0.AddMinutes 1.0) limited
    let st, wait2 = attempt st (t0.AddMinutes 3.0) limited
    let st, wait3 = attempt st (t0.AddMinutes 6.0) limited
    Assert.Equal<float list>([ 1.0; 2.0; 4.0 ], [ wait1; wait2; wait3 ])
    // one that goes through: the next rate limit is a minute again
    let st, _ = attempt st (t0.AddMinutes 11.0) (B.SentAs "x1")
    let st, _ = runB st [ B.Declined ("x1", "No.", t0.AddMinutes 11.0) ]
    let _, wait = attempt st (t0.AddMinutes 12.0) limited
    Assert.Equal(1.0, wait)

[<Fact>]
let ``a rate limit that names its wait is waited out, and its reason is logged`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 2000; Rated = false; PauseSeconds = 0; GapSeconds = 0 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = Some blitz; Rapid = None; Classical = None }
    let st, effects = runB (online (Some m)) [ B.Listed (Ok [| bot "A" 2000 |], t0); B.Tick (t0.AddSeconds 1.0, 0) ]
    let token = effects |> List.pick (function B.PostChallenge (t, _) -> Some t | _ -> None)
    let why = "You played 100 games against other bots today, please wait before challenging another bot."
    let st, effects = runB st [ B.Sent (token, B.RateLimitedBy (why, Some 7200), t0.AddSeconds 1.0) ]
    Assert.Equal(t0.AddSeconds 7201.0, st.NextAuto)
    Assert.Contains(B.Log ("matchmaking: Lichess asks for a pause: " + why + " - waiting 2.0 hours, then challenges 0 s apart for a while"), effects)
    // the page's own challenge gets Lichess's words
    let st, _ = runB st [ B.Send (5, request "bob", t0) ]
    let _, effects = runB st [ B.Sent (5, B.RateLimitedBy (why, Some 7200), t0) ]
    Assert.Contains(B.Answer (5, Error ("Lichess asks for a pause: " + why)), effects)

[<Fact>]
let ``a 429's wait is read from Lichess's ratelimit object`` () =
    Assert.Equal(Some 3600, rateLimitSeconds """{"error":"You played 100 games against other bots today","ratelimit":{"key":"bot.vsBot.day","seconds":3600}}""")
    Assert.Equal(Some 60, rateLimitSeconds """{"seconds":60}""")
    Assert.Equal(None, rateLimitSeconds """{"error":"Too many requests"}""")
    Assert.Equal(None, rateLimitSeconds "Too many requests")
    Assert.Equal("You played 100 games against other bots today", errorText """{"error":"You played 100 games against other bots today","ratelimit":{"key":"bot.vsBot.day","seconds":3600}}""")


[<Fact>]
let ``after a rate limit challenges are spaced further apart, and the gap eases back every 30 minutes`` () =
    let m = { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 2000; Rated = false; PauseSeconds = 0; GapSeconds = 30 }
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = Some blitz; Rapid = None; Classical = None }
    let bots = [| bot "A" 2000; bot "B" 2000; bot "C" 2000; bot "D" 2000 |]
    /// a challenge at `at` answered with `result`, then declined (its place free again): the gap that follows
    let challenge (st: B.State<string>) (at: DateTime) result =
        let st, effects = runB st [ B.Listed (Ok bots, at); B.Tick (at, 0) ]
        let token = effects |> List.pick (function B.PostChallenge (t, _) -> Some t | _ -> None)
        let gap = (st.NextAuto - at).TotalSeconds
        let st, _ = runB st [ B.Sent (token, result, at) ]
        let st = match result with B.SentAs id -> fst (runB st [ B.Declined (id, "No.", at) ]) | _ -> st
        st, gap
    let st = online (Some m)
    let st, gap0 = challenge st (t0.AddMinutes 1.0) (B.SentAs "a")
    let st, _ = challenge st (t0.AddMinutes 2.0) (B.RateLimitedBy ("", Some 60))
    let st, gap1 = challenge st (t0.AddMinutes 4.0) (B.SentAs "b")
    // 30 minutes after the rate limit the gap halves back
    let st, gap2 = challenge st (t0.AddMinutes 40.0) (B.SentAs "c")
    Assert.Equal<float list>([ 30.0; 60.0; 30.0 ], [ gap0; gap1; gap2 ])

[<Fact>]
let ``a declined challenge's log line says what was asked and, for a time control, what the bot takes`` () =
    // the test challenge is 3+2 blitz casual; the bot takes 1+0 and 5+3 only
    let st = fst (B.step (B.initial { Filter = { defaultFilter with TimeControls = [ 60, 0; 300, 3 ] }; Matchmaking = None }) (B.WentOnline ("testbot", t0)))
    let _, effects = runB st [ B.ChallengeIn (challenge "c5" "CruelKen", t0) ]
    Assert.Contains(B.Log "declining c5 from CruelKen (3+2 blitz casual): timecontrol - takes 1+0, 5+3", effects)
    Assert.Contains(B.Decline ("c5", "timecontrol"), effects)
