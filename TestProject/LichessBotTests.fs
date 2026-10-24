/// The Lichess bot (LichessBotLogic, LichessBot): the API's messages, which challenges the bot takes,
/// the go command, the position it searches, the chat - and a whole game against a fake Lichess server
/// played by the fake UCI engine.
module LichessBotTests

open System
open System.IO
open System.Net
open System.Text
open System.Threading
open System.Threading.Tasks
open Xunit
open ChessLibrary
open ChessLibrary.LichessBotLogic

// ---------------------------------------------------------------- messages

let private challengeLine id (challenger: string) (title: string) speed rated limit inc variant =
    sprintf """{"type":"challenge","challenge":{"id":"%s","challenger":{"id":"%s","name":"%s","title":%s,"rating":1500},"variant":{"key":"%s","name":"x"},"rated":%b,"speed":"%s","timeControl":{"type":"clock","limit":%d,"increment":%d,"show":"x"},"color":"random"}}"""
        id (challenger.ToLowerInvariant()) challenger (if title = "" then "null" else "\"" + title + "\"") variant rated speed limit inc

[<Fact>]
let ``the event stream's challenges, games and keep-alive lines are read`` () =
    match parseEvent (challengeLine "c1" "Alice" "" "blitz" true 180 2 "standard") with
    | Some (ChallengeArrived c) ->
        Assert.Equal(("c1", "Alice", "", true, "blitz", Some (180, 2), "standard"),
                     (c.Id, c.Challenger.Name, c.Challenger.Title, c.Rated, c.Speed, c.Clock, c.Variant))
    | other -> failwithf "%A" other
    Assert.Equal(Some (GameStarted "g1"), parseEvent """{"type":"gameStart","game":{"gameId":"g1","id":"g1"}}""")
    Assert.Equal(Some (GameFinished "g1"), parseEvent """{"type":"gameFinish","game":{"gameId":"g1"}}""")
    Assert.Equal(Some (ChallengeGone "c1"), parseEvent """{"type":"challengeCanceled","challenge":{"id":"c1"}}""")
    Assert.Equal(Some (ChallengeDeclined ("c1", "This time control is too fast for me.")),
                 parseEvent """{"type":"challengeDeclined","challenge":{"id":"c1","declineReason":"This time control is too fast for me.","declineReasonKey":"toofast"}}""")
    Assert.Equal(None, parseEvent "")

[<Fact>]
let ``a game stream's full game, states and chat are read`` () =
    let full = """{"type":"gameFull","id":"g1","variant":{"key":"standard"},"rated":false,"speed":"blitz","white":{"id":"bot","name":"Bot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1600},"initialFen":"startpos","state":{"type":"gameState","moves":"e2e4 e7e5","wtime":170000,"btime":175000,"winc":2000,"binc":2000,"status":"started"}}"""
    match parseGameMessage full with
    | Some (Full g) ->
        Assert.Equal(("g1", "bot", "alice", Some 1600, "startpos"), (g.Id, g.White.Id, g.Black.Id, g.Black.Rating, g.InitialFen))
        Assert.Equal<string list>([ "e2e4"; "e7e5" ], g.State.Moves)
        Assert.Equal((170000L, 175000L, 2000L, 2000L), (g.State.WTime, g.State.BTime, g.State.WInc, g.State.BInc))
    | other -> failwithf "%A" other
    match parseGameMessage """{"type":"gameState","moves":"e2e4","wtime":1,"btime":2,"winc":0,"binc":0,"status":"resign","winner":"white"}""" with
    | Some (State s) -> Assert.Equal(("resign", Some "white"), (s.Status, s.Winner)); Assert.True(isOver s)
    | other -> failwithf "%A" other
    Assert.Equal(Some (Chat ("spectator", "bob", "!eval")), parseGameMessage """{"type":"chatLine","room":"spectator","username":"bob","text":"!eval"}""")

// ---------------------------------------------------------------- which challenges

let private challenge speed rated (limit, inc) title variant =
    match parseEvent (challengeLine "c" "X" title speed rated limit inc variant) with
    | Some (ChallengeArrived c) -> c
    | other -> failwithf "%A" other

[<Fact>]
let ``the filter takes what it allows and names Lichess's reason for the rest`` () =
    let f = { defaultFilter with TimeControls = [ 180, 2; 60, 0 ] }
    Assert.Equal(None, declineReason f 0 (challenge "blitz" true (180, 2) "" "standard"))
    Assert.Equal(Some "later", declineReason f 1 (challenge "blitz" true (180, 2) "" "standard"))
    Assert.Equal(Some "toofast", declineReason defaultFilter 0 (challenge "ultraBullet" false (15, 0) "" "standard"))
    Assert.Equal(Some "timecontrol", declineReason f 0 (challenge "blitz" true (300, 3) "" "standard"))
    // Lichess's keys: "standard" tells the challenger the bot plays standard only
    Assert.Equal(Some "standard", declineReason f 0 (challenge "blitz" true (180, 2) "" "chess960"))
    Assert.Equal(Some "variant", declineReason { f with Variants = [ "chess960" ] } 0 (challenge "blitz" true (180, 2) "" "standard"))
    Assert.Equal(Some "casual", declineReason { f with Rated = false } 0 (challenge "blitz" true (180, 2) "" "standard"))
    Assert.Equal(Some "rated", declineReason { f with Casual = false } 0 (challenge "blitz" false (180, 2) "" "standard"))
    Assert.Equal(Some "nobot", declineReason { f with Opponents = Humans } 0 (challenge "blitz" true (180, 2) "BOT" "standard"))
    Assert.Equal(Some "onlybot", declineReason { f with Opponents = Bots } 0 (challenge "blitz" true (180, 2) "GM" "standard"))
    // no time controls given: any but UltraBullet
    Assert.Equal(None, declineReason defaultFilter 0 (challenge "classical" true (1800, 30) "" "standard"))

[<Fact>]
let ``time controls are read as minutes plus seconds`` () =
    Assert.Equal(Ok [ 60, 0; 180, 2; 30, 0 ], parseTimeControls "1+0, 3+2,0.5+0")
    Assert.Equal(Ok [], parseTimeControls "")
    match parseTimeControls "3+2,blitz" with
    | Error e -> Assert.Contains("'blitz'", e)
    | Ok v -> failwithf "%A" v

// ---------------------------------------------------------------- playing

let private state moves = { Moves = moves; WTime = 60000L; BTime = 50000L; WInc = 1000L; BInc = 1000L; Status = "started"; Winner = None }
let private game fen moves =
    { Id = "g"; Variant = "standard"; Rated = false; Speed = "blitz"
      White = { Id = "bot"; Name = "Bot"; Title = "BOT"; Rating = None }; Black = { Id = "x"; Name = "X"; Title = ""; Rating = None }
      InitialFen = fen; State = state moves }

[<Fact>]
let ``the go command is the clock less the overhead, or fixed nodes whatever the clock`` () =
    Assert.Equal("go wtime 59900 btime 49900 winc 1000 binc 1000", goCommand (ByClock 100) (state []))
    Assert.Equal("go wtime 10 btime 10 winc 0 binc 0", goCommand (ByClock 500) { state [] with WTime = 200L; BTime = 5L; WInc = 0L; BInc = 0L })
    Assert.Equal("go nodes 1", goCommand (FixedNodes 1) (state []))

[<Fact>]
let ``the position is startpos with the moves, castles as the engine expects, and says whose move it is`` () =
    let g = game "startpos" []
    Assert.Equal(Some ("position startpos", true), position g (state []) true)
    Assert.Equal(Some ("position startpos moves e2e4", false), position g (state [ "e2e4" ]) true)
    Assert.Equal(Some ("position startpos moves e2e4", true), position g (state [ "e2e4" ]) false)
    // a castle sent as the king taking its rook is the castle
    let line = [ "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1c4"; "f8c5"; "e1h1" ]
    match position g (state line) true with
    | Some (command, ourTurn) -> Assert.EndsWith("f8c5 e1g1", command); Assert.False ourTurn
    | None -> failwith "the castle did not play"
    // a move that does not play
    Assert.Equal(None, position g (state [ "e2e5" ]) true)
    // another start: a FEN
    let fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
    Assert.Equal(Some (sprintf "position fen %s moves h1h8" fen, false), position (game fen []) (state [ "h1h8" ]) true)

[<Fact>]
let ``the chat answers its commands, and keeps the engine's view for spectators`` () =
    let info = { EngineName = "Ceres"; Net = "C1.onnx"; Limit = FixedNodes 1; Playing = 1; MaxGames = 1 }
    let answer room text = chatAnswer info room text (Some "0.31") (Some "Policy: e4 40.0%")
    Assert.Equal(Some "Net: C1.onnx, 1 node, no search", answer "player" "!net")
    Assert.Equal(Some "0.31", answer "spectator" "!eval")
    Assert.Equal(Some "Policy: e4 40.0%", answer "spectator" "!top")
    Assert.Equal(Some "I don't tell my opponent, sorry.", answer "player" "!eval")
    Assert.Equal(Some "I don't tell my opponent, sorry.", answer "player" "!top")
    Assert.Equal(Some "Busy with 1 game(s): try again after.", answer "player" "!queue")
    Assert.Equal(None, answer "player" "hello")

// ---------------------------------------------------------------- a game against a fake Lichess

/// Lichess's bot API on localhost: an account, the event and game streams fed by the test, and every
/// POST recorded (path and form) - the test answers each with the next lines of the streams.
type private FakeLichess(onPost: FakeLichess -> string -> string -> unit, failOnce: string list) as this =
    let port =
        let l = new Sockets.TcpListener(IPAddress.Loopback, 0)
        l.Start()
        let p = (l.LocalEndpoint :?> IPEndPoint).Port
        l.Stop()
        p
    let listener = new HttpListener()
    do listener.Prefixes.Add(sprintf "http://localhost:%d/" port)
    let events = new Collections.Concurrent.BlockingCollection<string>()
    // one queue per game: a stream the bot closed must not take the next game's lines. A gameFull
    // names its game; the lines after it go to that game
    let games = Collections.Concurrent.ConcurrentDictionary<string, Collections.Concurrent.BlockingCollection<string>>()
    let queueOf (id: string) = games.GetOrAdd(id, fun _ -> new Collections.Concurrent.BlockingCollection<string>())
    let mutable lastGame = "g1"
    let posts = Collections.Concurrent.ConcurrentQueue<string * string>()
    // paths answered with 500 the first time (a request Lichess did not take)
    let failing = Collections.Concurrent.ConcurrentDictionary<string, bool>(failOnce |> List.map (fun p -> Collections.Generic.KeyValuePair(p, true)))
    // a POST's answer when it is not {"ok":true}: by path
    let replies = Collections.Concurrent.ConcurrentDictionary<string, string>()
    let cts = new CancellationTokenSource()
    let stream (ctx: HttpListenerContext) (lines: Collections.Concurrent.BlockingCollection<string>) =
        ctx.Response.SendChunked <- true
        ctx.Response.ContentType <- "application/x-ndjson"
        let out = ctx.Response.OutputStream
        try
            while not cts.IsCancellationRequested do
                let mutable line = ""
                if lines.TryTake(&line, 200) then
                    let bytes = Encoding.UTF8.GetBytes(line + "\n")
                    out.Write(bytes, 0, bytes.Length)
                    out.Flush()
        with _ -> ()
        try ctx.Response.Close() with _ -> ()
    let handle (ctx: HttpListenerContext) =
        let path = ctx.Request.Url.AbsolutePath
        match ctx.Request.HttpMethod, path with
        | "GET", "/api/account" ->
            let bytes = Encoding.UTF8.GetBytes """{"id":"testbot","username":"TestBot","title":"BOT"}"""
            ctx.Response.OutputStream.Write(bytes, 0, bytes.Length)
            ctx.Response.Close()
        | "GET", "/api/bot/online" ->
            let bytes = Encoding.UTF8.GetBytes(match replies.TryGetValue "GET /api/bot/online" with | true, r -> r | _ -> "")
            ctx.Response.OutputStream.Write(bytes, 0, bytes.Length)
            ctx.Response.Close()
        | "GET", "/api/stream/event" -> stream ctx events
        | "GET", p when p.StartsWith "/api/bot/game/stream/" -> stream ctx (queueOf (p.Substring(p.LastIndexOf '/' + 1)))
        | _ ->
            use reader = new StreamReader(ctx.Request.InputStream)
            let body = reader.ReadToEnd()
            posts.Enqueue((path, body))
            if failing.TryRemove(path) |> fst then
                ctx.Response.StatusCode <- 500
                ctx.Response.Close()
            else
                let bytes = Encoding.UTF8.GetBytes(match replies.TryGetValue path with | true, r -> r | _ -> """{"ok":true}""")
                ctx.Response.OutputStream.Write(bytes, 0, bytes.Length)
                ctx.Response.Close()
                onPost this path body
    do
        listener.Start()
        Task.Run(fun () ->
            while listener.IsListening do
                try
                    let ctx = listener.GetContext()
                    Task.Run(fun () -> try handle ctx with _ -> ()) |> ignore
                with _ -> ()) |> ignore
    member _.Url = sprintf "http://localhost:%d" port
    member _.Event (line: string) = events.Add line
    member _.Game (line: string) =
        if line.Contains "\"type\":\"gameFull\"" then
            lastGame <- (Text.Json.Nodes.JsonNode.Parse line).["id"].GetValue<string>()
        (queueOf lastGame).Add line
    /// A line of the game `id`'s stream, whatever game came last.
    member _.GameOf (id: string) (line: string) = (queueOf id).Add line
    member _.Posts = posts.ToArray()
    member _.ReplyWith (path: string) (body: string) = replies.[path] <- body
    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            try listener.Stop() with _ -> ()

let private fakeEngine = Path.Combine(AppContext.BaseDirectory, (if OperatingSystem.IsWindows() then "FakeUciEngine.exe" else "FakeUciEngine"))

let private gameState moves status winner =
    sprintf """{"type":"gameState","moves":"%s","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"%s"%s}""" moves status
        (match winner with Some w -> sprintf ""","winner":"%s" """ w | None -> "")

[<Fact>]
let ``the bot accepts what the filter takes, declines the rest, plays its moves, answers the chat and writes the game`` () =
    let pgn = Path.Combine(Path.GetTempPath(), sprintf "eb_bot_%s.pgn" (Guid.NewGuid().ToString("N")))
    let onPost (server: FakeLichess) (path: string) (_: string) =
        match path with
        | "/api/challenge/c1/accept" ->
            server.Game """{"type":"gameFull","id":"g1","variant":{"key":"standard"},"rated":false,"speed":"bullet","white":{"id":"testbot","name":"TestBot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1500},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}"""
            server.Event """{"type":"gameStart","game":{"gameId":"g1","id":"g1"}}"""
        | "/api/bot/game/g1/move/e2e4" ->
            server.Game """{"type":"chatLine","room":"spectator","username":"bob","text":"!net"}"""
            // a challenge the bot would take, but it is full: later
            server.Event (challengeLine "c3" "Carol" "" "bullet" false 60 0 "standard")
            server.Game (gameState "e2e4 e7e5" "started" None)
        | "/api/bot/game/g1/move/g1f3" -> server.Game (gameState "e2e4 e7e5 g1f3 b8c6" "started" None)
        | "/api/bot/game/g1/move/f1b5" -> server.Game (gameState "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6" "started" None)
        | "/api/bot/game/g1/move/b5a4" -> server.Game (gameState "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6" "started" None)
        | "/api/bot/game/g1/move/e1g1" -> server.Game (gameState "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1" "resign" (Some "white"))
        | _ -> ()
    // the bot's fourth move is refused once: it must be sent again
    use server = new FakeLichess(onPost, [ "/api/bot/game/g1/move/b5a4" ])
    server.Event (challengeLine "c1" "Alice" "" "bullet" false 60 0 "standard")
    server.Event (challengeLine "c2" "Fast" "" "ultraBullet" false 15 0 "standard")
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI" }
          Filter = defaultFilter
          Limit = ByClock 100
          PgnPath = pgn
          Greeting = ""
          Goodbye = "gg"
          Book = ""
          BookDepth = 16
          Matchmaking = None
          ShareEngine = true }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let run = bot.Run cts.Token
    try
        // the very first game is on the page while it runs (its 4th move is refused once: a second at least)
        let seen = DateTime.UtcNow.AddSeconds 30.0
        while not (bot.Games |> Array.exists (fun g -> g.GameId = "g1" && g.Fen <> "")) && DateTime.UtcNow < seen do Thread.Sleep 20
        Assert.True(bot.Games |> Array.exists (fun g -> g.GameId = "g1" && g.White = "TestBot"), sprintf "the first game is not on the page; log: %s" (String.Join(" / ", lines)))
        // until the game is in the PGN
        let deadline = DateTime.UtcNow.AddSeconds 45.0
        while not (File.Exists pgn && File.ReadAllText(pgn).Contains "1-0" && server.Posts |> Array.exists (fun (p, _) -> p = "/api/bot/game/g1/chat" )) && DateTime.UtcNow < deadline do Thread.Sleep 100
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore
        let posts = server.Posts |> Array.map fst
        if not (posts |> Array.exists (fun p -> p.Contains "/move/")) then
            failwithf "no move; bot log: %s | posts: %s" (String.Join(" / ", lines)) (String.Join(" / ", posts))
        Assert.Contains("/api/challenge/c1/accept", posts)
        Assert.Contains("/api/challenge/c2/decline", posts)
        let decline = server.Posts |> Array.find (fun (p, _) -> p = "/api/challenge/c2/decline") |> snd
        Assert.Equal("reason=toofast", decline)
        Assert.Equal<string[]>([| "e2e4"; "g1f3"; "f1b5"; "b5a4"; "b5a4"; "e1g1" |], posts |> Array.filter (fun p -> p.Contains "/move/") |> Array.map (fun p -> p.Substring(p.LastIndexOf '/' + 1)))
        Assert.Equal("reason=later", server.Posts |> Array.find (fun (p, _) -> p = "/api/challenge/c3/decline") |> snd)
        // the spectator's !net, and the goodbye to the opponent
        let chats = server.Posts |> Array.filter (fun (p, _) -> p = "/api/bot/game/g1/chat") |> Array.map (fun (_, b) -> Uri.UnescapeDataString(b.Replace('+', ' ')))
        Assert.Contains(chats, fun c -> c.Contains "room=spectator" && c.Contains "No net: Fake")
        // to the opponent and to whoever watches
        Assert.Contains(chats, fun c -> c.Contains "room=player" && c.Contains "text=gg")
        Assert.Contains(chats, fun c -> c.Contains "room=spectator" && c.Contains "text=gg")
        let text = File.ReadAllText pgn
        Assert.Contains("[White \"TestBot\"]", text)
        Assert.Contains("[Site \"https://lichess.org/g1\"]", text)
        Assert.Contains("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O 1-0", text)
        Assert.True(run.IsCompleted)
    finally
        cts.Cancel()
        try File.Delete pgn with _ -> ()

// ---------------------------------------------------------------- the opening book

let private gameFullLine variant (initialFen: string) =
    sprintf """{"type":"gameFull","id":"g1","variant":{"key":"%s"},"rated":false,"speed":"bullet","white":{"id":"testbot","name":"TestBot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1500},"initialFen":"%s","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}""" variant initialFen

let private fullOf line =
    match parseGameMessage line with
    | Some (Full g) -> g
    | other -> failwithf "not a gameFull: %A" other

[<Fact>]
let ``the book plays only the first plies of a standard game from the start`` () =
    let standard = fullOf (gameFullLine "standard" "startpos")
    Assert.True(bookAllowed standard 0 16)
    Assert.True(bookAllowed standard 15 16)
    Assert.False(bookAllowed standard 16 16)
    Assert.False(bookAllowed (fullOf (gameFullLine "chess960" "bqnbrkrn/pppppppp/8/8/8/8/PPPPPPPP/BQNBRKRN w KQkq - 0 1")) 0 16)
    Assert.False(bookAllowed (fullOf (gameFullLine "fromPosition" "8/8/8/4k3/8/8/4K3/7R w - - 0 1")) 0 16)

[<Fact>]
let ``a book move is drawn by the book's weights`` () =
    let move uci w : PolyglotLines.BookMove = { Uci = uci; San = uci; Weight = w; Percent = 0.0 }
    let rng = Random 7
    let picks = Array.init 4000 (fun _ -> pickBookMove [| move "e2e4" 3; move "d2d4" 1 |] rng)
    let e4 = picks |> Array.filter ((=) (Some "e2e4")) |> Array.length
    Assert.Equal(4000, picks |> Array.filter Option.isSome |> Array.length)
    Assert.InRange(e4, 2800, 3200)
    Assert.Equal(None, pickBookMove [||] rng)
    Assert.Equal(None, pickBookMove [| move "e2e4" 0 |] rng)

[<Fact>]
let ``the bot plays the book's move while it has one and the engine after the book depth`` () =
    let pgn = Path.Combine(Path.GetTempPath(), sprintf "eb_bot_book_%s.pgn" (Guid.NewGuid().ToString("N")))
    let book = Path.Combine(Path.GetTempPath(), sprintf "eb_bot_book_%s.bin" (Guid.NewGuid().ToString("N")))
    // e4 at the start, and Nf3 after 1.e4 e5 - past the depth of 2 plies, so the engine plays it
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let afterE5 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    PolyglotFormat.write book
        [ { PolyglotFormat.Entry.Key = PolyglotFormat.keyOfFen startFen; Move = PolyglotFormat.encodeMove "e2e4" false; Weight = 10us }
          { PolyglotFormat.Entry.Key = PolyglotFormat.keyOfFen afterE5; Move = PolyglotFormat.encodeMove "g1f3" false; Weight = 10us } ]
    |> ignore
    let onPost (server: FakeLichess) (path: string) (_: string) =
        match path with
        | "/api/challenge/c1/accept" ->
            server.Game (gameFullLine "standard" "startpos")
            server.Event """{"type":"gameStart","game":{"gameId":"g1","id":"g1"}}"""
        | "/api/bot/game/g1/move/e2e4" ->
            server.Game """{"type":"chatLine","room":"spectator","username":"bob","text":"!eval"}"""
            server.Game (gameState "e2e4 e7e5" "started" None)
        | "/api/bot/game/g1/move/g1f3" -> server.Game (gameState "e2e4 e7e5 g1f3" "resign" (Some "white"))
        | _ -> ()
    use server = new FakeLichess(onPost, [])
    server.Event (challengeLine "c1" "Alice" "" "bullet" false 60 0 "standard")
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI" }
          Filter = defaultFilter
          Limit = ByClock 100
          PgnPath = pgn
          Greeting = ""
          Goodbye = "gg"
          Book = book
          BookDepth = 2
          Matchmaking = None
          ShareEngine = true }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let run = bot.Run cts.Token
    try
        let deadline = DateTime.UtcNow.AddSeconds 45.0
        while not (File.Exists pgn && File.ReadAllText(pgn).Contains "1-0") && DateTime.UtcNow < deadline do Thread.Sleep 100
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore
        let text = if File.Exists pgn then File.ReadAllText pgn else failwithf "no PGN; bot log: %s" (String.Join(" / ", lines))
        Assert.Contains("1. e4 {book} e5 2. Nf3 1-0", text)
        Assert.Contains(lines, fun l -> l.Contains "book move e2e4")
        let chats = server.Posts |> Array.filter (fun (p, _) -> p = "/api/bot/game/g1/chat") |> Array.map (fun (_, b) -> Uri.UnescapeDataString(b.Replace('+', ' ')))
        Assert.Contains(chats, fun c -> c.Contains "room=spectator" && c.Contains "opening book")
    finally
        cts.Cancel()
        try File.Delete pgn with _ -> ()
        try File.Delete book with _ -> ()

// ---------------------------------------------------------------- challenges the bot sends

[<Fact>]
let ``Lichess's answers to a challenge are read: its id, an error, the bots online`` () =
    Assert.Equal(Some "ab12", challengeIdOf """{"id":"ab12","url":"https://lichess.org/ab12","status":"created"}""")
    Assert.Equal(Some "ab12", challengeIdOf """{"challenge":{"id":"ab12"}}""")
    Assert.Equal(None, challengeIdOf "not json")
    Assert.Equal("No such user", errorText """{"error":"No such user"}""")
    Assert.Equal("Invalid clock", errorText """{"global":["Invalid clock"]}""")
    Assert.Equal("gone", errorText "gone")
    let bot = parseOnlineBot """{"id":"maia1","username":"maia1","perfs":{"bullet":{"games":2,"rating":1589},"blitz":{"games":4,"rating":1408}},"title":"BOT"}"""
    Assert.Equal(Some { Name = "maia1"; Bullet = Some 1589; Blitz = Some 1408; Rapid = None; Classical = None; Chess960 = None }, bot)
    Assert.Equal(None, parseOnlineBot "")
    // no games at a speed: Lichess's placeholder rating is no rating
    let fresh = parseOnlineBot """{"id":"lepbot","username":"lepbot","perfs":{"bullet":{"games":0,"rating":3000,"prov":true},"blitz":{"games":3,"rating":2210,"prov":true}}}"""
    Assert.Equal(Some { Name = "lepbot"; Bullet = None; Blitz = Some 2210; Rapid = None; Classical = None; Chess960 = None }, fresh)

[<Fact>]
let ``the bot challenges a player, counts the challenge while it waits and hears the answer`` () =
    let gameFullOf (id: string) status =
        sprintf """{"type":"gameFull","id":"%s","variant":{"key":"standard"},"rated":false,"speed":"blitz","white":{"id":"testbot","name":"TestBot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1500},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"%s"}}""" id status
    let onPost (server: FakeLichess) (path: string) (_: string) =
        match path with
        | "/api/challenge/alice" ->
            // Lichess shows the bot its own challenge too: it must not decline it
            server.Event (challengeLine "out1" "TestBot" "BOT" "blitz" false 180 2 "standard")
            // one the bot would take, but the waiting challenge fills its one place
            server.Event (challengeLine "c9" "Carol" "" "blitz" false 180 2 "standard")
        // a bot that declines at once: the answer can come before the challenge's id is known
        | "/api/challenge/erin" ->
            server.Event """{"type":"challengeDeclined","challenge":{"id":"out4","declineReason":"Not now."}}"""
        | _ -> ()
    use server = new FakeLichess(onPost, [])
    server.ReplyWith "/api/challenge/alice" """{"id":"out1","status":"created"}"""
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI" }
          Filter = defaultFilter
          Limit = ByClock 100
          PgnPath = ""
          Greeting = ""
          Goodbye = ""
          Book = ""
          BookDepth = 16
          Matchmaking = None
          ShareEngine = true }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let until (cond: unit -> bool) =
        let deadline = DateTime.UtcNow.AddSeconds 20.0
        while not (cond ()) && DateTime.UtcNow < deadline do Thread.Sleep 50
        Assert.True(cond (), sprintf "timed out; bot log: %s" (String.Join(" / ", lines)))
    Assert.Equal(Result.Error "the bot is not online", bot.Challenge("alice", 180, 2, false, "white", "standard", cts.Token).Result)
    let run = bot.Run cts.Token
    try
        until (fun () -> bot.IsOnline)
        Assert.Equal(Result.Error "the bot cannot challenge itself", bot.Challenge("@TestBot", 180, 2, false, "white", "standard", cts.Token).Result)
        Assert.Equal(Result.Ok "out1", bot.Challenge("alice", 180, 2, false, "white", "standard", cts.Token).Result)
        let form = server.Posts |> Array.find (fun (p, _) -> p = "/api/challenge/alice") |> snd
        Assert.Equal("clock.limit=180&clock.increment=2&rated=false&color=white&variant=standard", form)
        Assert.Equal(1, bot.Outgoing.Length)
        // full while it waits: another challenge is declined "later", and the bot cannot send a second one
        until (fun () -> server.Posts |> Array.exists (fun (p, _) -> p = "/api/challenge/c9/decline"))
        Assert.Equal("reason=later", server.Posts |> Array.find (fun (p, _) -> p = "/api/challenge/c9/decline") |> snd)
        Assert.False(server.Posts |> Array.exists (fun (p, _) -> p = "/api/challenge/out1/decline"))
        Assert.True((bot.Challenge("bob", 180, 2, false, "white", "standard", cts.Token).Result).IsError)
        // declined: the place is free again and the page hears why
        server.Event """{"type":"challengeDeclined","challenge":{"id":"out1","declineReason":"I'm not accepting challenges at the moment."}}"""
        until (fun () -> bot.Outgoing.Length = 0)
        Assert.Equal("alice declined: I'm not accepting challenges at the moment.", bot.LastChallengeAnswer)
        // accepted: the game starts and is played
        server.ReplyWith "/api/challenge/alice" """{"id":"out2","status":"created"}"""
        Assert.Equal(Result.Ok "out2", bot.Challenge("alice", 180, 2, false, "white", "standard", cts.Token).Result)
        server.Game (gameFullOf "out2" "started")
        server.Event """{"type":"gameStart","game":{"gameId":"out2","id":"out2"}}"""
        until (fun () -> bot.LastChallengeAnswer = "alice accepted the challenge" && bot.Outgoing.Length = 0)
        until (fun () -> server.Posts |> Array.exists (fun (p, _) -> p = "/api/bot/game/out2/move/e2e4"))
        // a challenge left open when the bot goes offline is withdrawn
        server.ReplyWith "/api/challenge/dave" """{"id":"out3","status":"created"}"""
        server.Game (gameState "e2e4" "resign" (Some "white"))
        until (fun () -> bot.Games.Length = 0)
        server.ReplyWith "/api/challenge/erin" """{"id":"out4","status":"created"}"""
        Assert.Equal(Result.Ok "out4", bot.Challenge("erin", 180, 2, false, "white", "standard", cts.Token).Result)
        until (fun () -> bot.Outgoing.Length = 0 && bot.LastChallengeAnswer = "erin declined: Not now.")
        Assert.Equal(Result.Ok "out3", bot.Challenge("dave", 60, 0, true, "random", "standard", cts.Token).Result)
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore
        Assert.Contains("/api/challenge/out3/cancel", server.Posts |> Array.map fst)
    finally
        cts.Cancel()

// ---------------------------------------------------------------- matchmaking

[<Fact>]
let ``a time control's speed is Lichess's, and the opponent is drawn among the bots in range`` () =
    Assert.Equal("ultraBullet", speedOf 15 0)
    Assert.Equal("bullet", speedOf 60 0)
    Assert.Equal("bullet", speedOf 120 1)
    Assert.Equal("blitz", speedOf 180 0)
    Assert.Equal("blitz", speedOf 180 2)
    Assert.Equal("rapid", speedOf 600 0)
    Assert.Equal("classical", speedOf 1800 0)
    let bot name blitz : OnlineBot = { Name = name; Bullet = None; Blitz = blitz; Rapid = None; Classical = None; Chess960 = None }
    let bots = [| bot "near" (Some 2100); bot "far" (Some 2600); bot "none" None; bot "low" (Some 1700) |]
    let rng = Random 3
    let picks = Array.init 200 (fun _ -> pickOpponent bots (Some 2000) 300 "blitz" (fun _ -> false) rng.Next |> Option.map (fun b -> b.Name))
    Assert.True(picks |> Array.forall (fun p -> p = Some "near" || p = Some "low"))
    Assert.Contains(Some "near", picks)
    Assert.Contains(Some "low", picks)
    Assert.Equal(None, pickOpponent bots (Some 2000) 300 "blitz" (fun n -> n = "near" || n = "low") rng.Next)
    // no rating of its own yet: any bot that plays the speed
    Assert.Equal(3, Array.init 300 (fun _ -> pickOpponent bots None 300 "blitz" (fun _ -> false) rng.Next) |> Array.choose id |> Array.distinct |> Array.length)
    Assert.Equal(None, pickOpponent bots (Some 2000) 300 "rapid" (fun _ -> false) rng.Next)

[<Fact>]
let ``matchmaking challenges a bot in range, and after a decline another one, never the same twice`` () =
    let user (name: string) blitz = sprintf """{"id":"%s","username":"%s","title":"BOT","perfs":{"blitz":{"games":100,"rating":%d}}}""" (name.ToLowerInvariant()) name blitz
    use server = new FakeLichess((fun _ _ _ -> ()), [])
    server.ReplyWith "GET /api/bot/online" (String.Join("\n", [ user "TestBot" 2000; user "Near" 2100; user "Other" 1900; user "Far" 2600 ]))
    server.ReplyWith "/api/challenge/Near" """{"id":"mNear"}"""
    server.ReplyWith "/api/challenge/Other" """{"id":"mOther"}"""
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI" }
          Filter = defaultFilter
          Limit = ByClock 100
          PgnPath = ""
          Greeting = ""
          Goodbye = ""
          Book = ""
          BookDepth = 16
          Matchmaking = Some { TimeControls = [ 180, 0 ]; RatingRange = 300; StartRating = 1500; Rated = false; PauseSeconds = 0; GapSeconds = 0 }
          ShareEngine = true }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let challenged () = server.Posts |> Array.map fst |> Array.filter (fun p -> p.StartsWith "/api/challenge/" && p.Split('/').Length = 4)
    let until (cond: unit -> bool) =
        let deadline = DateTime.UtcNow.AddSeconds 20.0
        while not (cond ()) && DateTime.UtcNow < deadline do Thread.Sleep 50
        Assert.True(cond (), sprintf "timed out; bot log: %s" (String.Join(" / ", lines)))
    let run = bot.Run cts.Token
    try
        until (fun () -> challenged().Length = 1)
        let first = challenged().[0].Substring "/api/challenge/".Length
        Assert.Contains(first, [ "Near"; "Other" ])
        let form = server.Posts |> Array.find (fun (p, _) -> p = "/api/challenge/" + first) |> snd
        Assert.Equal("clock.limit=180&clock.increment=0&rated=false&color=random&variant=standard", form)
        // declined: the other one in range is next; Far (out of range) and the bot itself never
        server.Event (sprintf """{"type":"challengeDeclined","challenge":{"id":"m%s","declineReason":"No."}}""" first)
        until (fun () -> challenged().Length = 2)
        let second = challenged().[1].Substring "/api/challenge/".Length
        Assert.NotEqual<string>(first, second)
        Assert.Contains(second, [ "Near"; "Other" ])
        // both declined: nobody fits, nobody is challenged again
        server.Event (sprintf """{"type":"challengeDeclined","challenge":{"id":"m%s","declineReason":"No."}}""" second)
        until (fun () -> lines |> Seq.exists (fun l -> l.StartsWith "matchmaking: no bot online fits"))
        Assert.Equal(2, challenged().Length)
    finally
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore

[<Fact>]
let ``a game that ends during a search hands its engine on only when the search has stopped`` () =
    let full (id: string) =
        sprintf """{"type":"gameFull","id":"%s","variant":{"key":"standard"},"rated":false,"speed":"blitz","white":{"id":"testbot","name":"TestBot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1500},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}""" id
    let onPost (server: FakeLichess) (path: string) (_: string) =
        match path with
        | "/api/challenge/c1/accept" ->
            server.Game (full "c1")
            server.Event """{"type":"gameStart","game":{"gameId":"c1","id":"c1"}}"""
            // the opponent resigns while the bot is still thinking
            Task.Delay(700).ContinueWith(fun (_: Task) -> server.Game (gameState "" "resign" (Some "white"))) |> ignore
        | "/api/bot/game/c1/chat" ->
            // the next game at once: it gets the engine the first one held
            server.Event (challengeLine "c2" "Alice" "" "blitz" false 180 2 "standard")
        | "/api/challenge/c2/accept" ->
            server.Game (full "c2")
            server.Event """{"type":"gameStart","game":{"gameId":"c2","id":"c2"}}"""
        | "/api/bot/game/c2/move/e2e4" -> server.Game (gameState "e2e4" "resign" (Some "white"))
        | _ -> ()
    use server = new FakeLichess(onPost, [])
    server.Event (challengeLine "c1" "Alice" "" "blitz" false 180 2 "standard")
    let options = Collections.Generic.Dictionary<string, obj>(dict [ "FakeGoDelayMs", box 1500L ])
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI"; Options = options }
          Filter = defaultFilter
          Limit = ByClock 100
          PgnPath = ""
          Greeting = ""
          Goodbye = "gg"
          Book = ""
          BookDepth = 8
          Matchmaking = None
          ShareEngine = true }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let run = bot.Run cts.Token
    try
        let deadline = DateTime.UtcNow.AddSeconds 30.0
        while not (server.Posts |> Array.exists (fun (p, _) -> p = "/api/bot/game/c2/chat")) && DateTime.UtcNow < deadline do Thread.Sleep 50
        let log = String.Join(" / ", lines)
        Assert.True(server.Posts |> Array.exists (fun (p, _) -> p = "/api/bot/game/c2/move/e2e4"), sprintf "game 2 did not move; log: %s" log)
        // the first game's stopped search is no engine fault: nothing failed, nothing started again
        Assert.DoesNotContain(lines, fun l -> l.Contains "the engine failed" || l.Contains "does not start")
        Assert.False(server.Posts |> Array.exists (fun (p, _) -> p.StartsWith "/api/bot/game/c1/move/"), "game 1 moved after it ended")
    finally
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore

/// Two games at once, each resigned by the opponent after the bot's first move; the engine
/// processes started (each fake engine logs to its own file in a folder).
let private twoGamesAtOnce (share: bool) =
    let engineLogs = Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), sprintf "eb_bot_engines_%s" (Guid.NewGuid().ToString("N")))).FullName
    let full (id: string) =
        sprintf """{"type":"gameFull","id":"%s","variant":{"key":"standard"},"rated":false,"speed":"blitz","white":{"id":"testbot","name":"TestBot","title":"BOT"},"black":{"id":"alice","name":"Alice","rating":1500},"initialFen":"startpos","state":{"type":"gameState","moves":"","wtime":60000,"btime":60000,"winc":0,"binc":0,"status":"started"}}""" id
    // the opponent resigns a while after the bot's first move: the two games overlap
    let resignLater (server: FakeLichess) (id: string) =
        Task.Delay(1500).ContinueWith(fun (_: Task) -> server.GameOf id (gameState "e2e4" "resign" (Some "white"))) |> ignore
    let onPost (server: FakeLichess) (path: string) (_: string) =
        match path with
        | "/api/challenge/c1/accept" | "/api/challenge/c2/accept" ->
            let id = path.Split('/').[3]
            server.Game (full id)
            server.Event (sprintf """{"type":"gameStart","game":{"gameId":"%s","id":"%s"}}""" id id)
        | "/api/bot/game/c1/move/e2e4" -> resignLater server "c1"
        | "/api/bot/game/c2/move/e2e4" -> resignLater server "c2"
        | _ -> ()
    use server = new FakeLichess(onPost, [])
    server.Event (challengeLine "c1" "Alice" "" "blitz" false 180 2 "standard")
    server.Event (challengeLine "c2" "Bob" "" "blitz" false 180 2 "standard")
    let settings =
        { Engine = { TypesDef.CoreTypes.EngineConfig.Empty with Name = "Fake"; Path = fakeEngine; Protocol = "UCI"; Args = sprintf "--log-dir \"%s\"" engineLogs }
          Filter = { defaultFilter with MaxGames = 2 }
          Limit = FixedNodes 1
          PgnPath = ""
          Greeting = ""
          Goodbye = "gg"
          Book = ""
          BookDepth = 8
          Matchmaking = None
          ShareEngine = share }
    use client = new LichessBotClient(server.Url, "test-token")
    let lines = Collections.Concurrent.ConcurrentQueue<string>()
    let bot = LichessBot(client, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, lines.Enqueue)
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds 60.0)
    let run = bot.Run cts.Token
    try
        let both () = [ "c1"; "c2" ] |> List.forall (fun id -> server.Posts |> Array.exists (fun (p, _) -> p = sprintf "/api/bot/game/%s/chat" id))
        let deadline = DateTime.UtcNow.AddSeconds 30.0
        while not (both ()) && DateTime.UtcNow < deadline do Thread.Sleep 50
        Assert.True(both (), sprintf "both games did not end; log: %s" (String.Join(" / ", lines)))
        for id in [ "c1"; "c2" ] do
            Assert.Contains(sprintf "/api/bot/game/%s/move/e2e4" id, server.Posts |> Array.map fst)
        cts.Cancel()
        run.Wait(TimeSpan.FromSeconds 20.0) |> ignore
        Directory.GetFiles(engineLogs, "*.log").Length
    finally
        cts.Cancel()
        try Directory.Delete(engineLogs, true) with _ -> ()

[<Fact>]
let ``with a fixed number of nodes one engine plays two games at once; without sharing each has its own`` () =
    Assert.Equal(1, twoGamesAtOnce true)
    Assert.Equal(2, twoGamesAtOnce false)
