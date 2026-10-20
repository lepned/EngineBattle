namespace ChessLibrary

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.LichessBotLogic

/// How the bot plays: engine, challenge filter, search limit, PGN, chat lines, opening book ("" for none).
type LichessBotSettings =
    { Engine: EngineConfig
      Filter: Filter
      Limit: SearchLimit
      PgnPath: string
      Greeting: string
      Goodbye: string
      Book: string
      BookDepth: int
      /// Finding opponents on its own; None: it only answers challenges (and sends the page's).
      Matchmaking: Matchmaking option
      /// With a fixed number of nodes: one engine plays every game, its searches in turn (one net
      /// loaded). By the clock each game always has its own: a search must not wait for another game's.
      ShareEngine: bool }

/// An engine of the bot. Engines are kept between games, so its search info goes to whichever game
/// holds it now: `Listener` is switched when a game takes it.
type LichessBotEngine =
    { Engine: AnalysisEngine
      Listener: (SearchUpdate -> unit) ref }

/// A message to the bot's agent: a step of LichessBotMachine, or a wait until every message before
/// it has been handled.
type internal BotMessage =
    | Step of LichessBotMachine.Event<LichessBotEngine>
    | Sync of AsyncReplyChannel<unit>

/// A Lichess bot account played by an engine until the token is cancelled. One agent owns the account
/// (LichessBotMachine), one each game (LichessGameMachine); HTTP and engines run outside and report back.
type LichessBot(client: LichessBotClient, settings: LichessBotSettings, logger: ILogger, log: string -> unit) =
    let say (text: string) =
        logger.LogInformation("Lichess bot: {Text}", text)
        try log text with _ -> ()
    let net =
        settings.Engine.Options
        |> Seq.tryFind (fun kv -> kv.Key = "Network" || kv.Key = "WeightsFile" || kv.Key = "CeresNet" || kv.Key = "EvalFile")
        |> Option.map (fun kv -> Path.GetFileName(string kv.Value))
        |> Option.defaultValue ""
    let pgnLock = obj ()

    /// Waits a pause that a cancel cuts short.
    let pause (ms: int) (ct: CancellationToken) = task { try do! Task.Delay(ms, ct) with :? OperationCanceledException -> () }

    /// A request, once more after a minute when Lichess asks for a pause. A timeout or a network error is
    /// a refusal (status 0), not an exception: a game must not stop over one request.
    let send (request: unit -> Task<LichessReply>) (what: string) (ct: CancellationToken) = task {
        let attempt () = task {
            try return! request ()
            with
            | :? OperationCanceledException when ct.IsCancellationRequested -> return LichessReply.Refused (0, "cancelled")
            | ex -> return LichessReply.Refused (0, ex.Message) }
        let! r = attempt ()
        match r with
        | LichessReply.RateLimited ->
            say (sprintf "%s: Lichess asks for a pause, waiting a minute" what)
            do! pause 60000 ct
            return! attempt ()
        | r -> return r
    }

    /// The finished game appended to the PGN: tags, moves in SAN ({book} after a book move), result.
    let writePgn (full: GameFull) (s: GameState) (bookPlies: Set<int>) =
        if not (String.IsNullOrWhiteSpace settings.PgnPath) then
            try
                let result =
                    match s.Winner with
                    | Some "white" -> "1-0"
                    | Some "black" -> "0-1"
                    | _ -> if s.Status = "draw" || s.Status = "stalemate" || s.Status = "insufficientMaterialClaim" || s.Status = "outoftime" then "1/2-1/2" else "*"
                let start = if full.InitialFen = "startpos" then "" else full.InitialFen
                let board = Chess.Board()
                board.ResetBoardState()
                if start <> "" then board.LoadFen start
                let sans = ResizeArray<string>()
                let whiteFirst = board.Position.STM = 0uy
                let fullMove = board.NextMoveNumber()
                match LichessBotLogic.position full s true with
                | Some (command, _) ->
                    let moves = match command.IndexOf " moves " with -1 -> [||] | i -> command.Substring(i + 7).Split(' ')
                    for m in moves do
                        match BoardUtils.tryGetMoveAndSanFromUci &board m with
                        // the board writes castles with zeros; PGN wants O-O
                        | Some (_, san) ->
                            let san = san.Replace("0-0-0", "O-O-O").Replace("0-0", "O-O")
                            sans.Add(if bookPlies.Contains sans.Count then san + " {book}" else san)
                            board.PlayUciMove m
                        | None -> ()
                | None -> ()
                let sb = StringBuilder()
                let tag (k: string) (v: string) = sb.Append('[').Append(k).Append(" \"").Append(PGNWriter.escapeTagValue v).Append("\"]\n") |> ignore
                tag "Event" (sprintf "Lichess %s %s" (if full.Rated then "rated" else "casual") full.Speed)
                tag "Site" (sprintf "https://lichess.org/%s" full.Id)
                tag "Date" (DateTime.Now.ToString("yyyy.MM.dd", Globalization.CultureInfo.InvariantCulture))
                tag "White" full.White.Name
                tag "Black" full.Black.Name
                tag "Result" result
                full.White.Rating |> Option.iter (fun r -> tag "WhiteElo" (string r))
                full.Black.Rating |> Option.iter (fun r -> tag "BlackElo" (string r))
                if start <> "" then
                    tag "SetUp" "1"
                    tag "FEN" start
                if full.Variant <> "standard" then tag "Variant" full.Variant
                tag "Termination" s.Status
                sb.Append('\n') |> ignore
                let mutable number = fullMove
                let mutable white = whiteFirst
                let units = ResizeArray<string>()
                for san in sans do
                    if white then units.Add(sprintf "%d. %s" number san)
                    elif units.Count = 0 then units.Add(sprintf "%d... %s" number san)
                    else units.Add san
                    if not white then number <- number + 1
                    white <- not white
                units.Add result
                sb.Append(String.Join(" ", units)).Append("\n\n") |> ignore
                lock pgnLock (fun () ->
                    let dir = Path.GetDirectoryName(Path.GetFullPath settings.PgnPath)
                    if not (String.IsNullOrEmpty dir) then Directory.CreateDirectory dir |> ignore
                    File.AppendAllText(settings.PgnPath, sb.ToString()))
            with ex -> logger.LogWarning(ex, "Lichess bot: game {Game} not written to the PGN", full.Id)

    /// A new engine; its search info goes to the listener of the game that holds it.
    let startEngine () =
        let listener = ref (fun (_: SearchUpdate) -> ())
        let e = EngineHelper.createAnalysisEngine ((fun u -> listener.Value u), settings.Engine, logger, false)
        e.EchoEngineText <- false
        { Engine = e; Listener = listener }
    let quit (e: LichessBotEngine) = Task.Run(fun () -> try e.Engine.Quit() with _ -> ())

    // the account's agent, and what waits on it: page challenges and games asking for an engine
    [<VolatileField>]
    let mutable published : LichessBotMachine.State<LichessBotEngine> =
        LichessBotMachine.initial { Filter = settings.Filter; Matchmaking = settings.Matchmaking }
    let challengeReplies = Dictionary<int, TaskCompletionSource<Result<string, string>>>()
    let engineReplies = Dictionary<int, TaskCompletionSource<LichessBotEngine option>>()
    let mutable nextToken = 0
    let take (d: Dictionary<int, 'T>) (token: int) =
        lock d (fun () -> match d.TryGetValue token with | true, v -> d.Remove token |> ignore; Some v | _ -> None)
    // requests and engine quits under way, and the games: waited for when the bot goes offline
    let gameTasks = ConcurrentDictionary<string, Task>()
    let pending = ConcurrentBag<Task>()
    let mutable gamesCt = CancellationToken.None
    let mutable botAgent : MailboxProcessor<BotMessage> option = None
    let postBot (event: LichessBotMachine.Event<LichessBotEngine>) = botAgent |> Option.iter (fun a -> a.Post (Step event))

    /// The bots online on Lichess now (at most `count`), this one among them; Error with why not.
    let botsOnline (count: int) (ct: CancellationToken) : Task<Result<OnlineBot[], string>> = task {
        let! lines = task { try return! client.OnlineBots(count, ct) with ex -> return Error ex.Message }
        return lines |> Result.map (Array.choose (fun l -> try parseOnlineBot l with _ -> None))
    }

    // one engine for every game (LichessEngineQueue): only with a fixed number of nodes
    let shared = settings.ShareEngine && (match settings.Limit with FixedNodes _ -> true | ByClock _ -> false)
    let queueReplies = Dictionary<int, TaskCompletionSource<string option>>()
    let queueListeners = ConcurrentDictionary<int, SearchUpdate -> unit>()
    let mutable queueAgent : MailboxProcessor<LichessEngineQueue.Event<LichessBotEngine>> option = None
    let postQueue e = queueAgent |> Option.iter (fun (a: MailboxProcessor<LichessEngineQueue.Event<LichessBotEngine>>) -> a.Post e)
    let startQueue () =
        MailboxProcessor<LichessEngineQueue.Event<LichessBotEngine>>.Start(fun inbox ->
            let execute effect =
                match effect with
                | LichessEngineQueue.StartEngine ->
                    task {
                        try
                            let! e = Task.Run(fun () -> startEngine ())
                            inbox.Post (LichessEngineQueue.Started e)
                        with ex -> inbox.Post (LichessEngineQueue.StartFailed ex.Message) } |> ignore
                | LichessEngineQueue.RunSearch (e, r) ->
                    // the search's info goes to the game that asked
                    e.Listener.Value <- (match queueListeners.TryGetValue r.Token with | true, l -> l | _ -> ignore)
                    task {
                        let search = task { try return! e.Engine.Search(r.Position, r.Go) |> Async.StartAsTask with ex -> return AnalysisOutcome.Failed ex.Message }
                        // every game waits on this engine: a search that never answers (a hung engine) is a
                        // failure after a minute - the engine is quit and started again
                        let! first = Task.WhenAny(search :> Task, Task.Delay(TimeSpan.FromSeconds 60.0))
                        if obj.ReferenceEquals(first, search) then
                            let move = match search.Result with AnalysisOutcome.Completed (Some info) -> Some info.Move | _ -> None
                            inbox.Post (LichessEngineQueue.Done (r.Token, move))
                        else
                            say "the engine did not answer in a minute"
                            inbox.Post (LichessEngineQueue.Done (r.Token, None)) } |> ignore
                | LichessEngineQueue.StopSearch e -> (try e.Engine.Stop() with _ -> ())
                | LichessEngineQueue.Reply (token, move) ->
                    queueListeners.TryRemove token |> ignore
                    take queueReplies token |> Option.iter (fun tcs -> tcs.TrySetResult move |> ignore)
                | LichessEngineQueue.QuitEngine e -> pending.Add(quit e)
                | LichessEngineQueue.Log text -> say text
            let rec loop state = async {
                let! event = inbox.Receive()
                let state, effects =
                    // a step that throws answers the search it was about: no game waits forever
                    try LichessEngineQueue.step state event
                    with ex ->
                        logger.LogError(ex, "Lichess bot: engine queue: {Event} not handled", sprintf "%A" event)
                        match event with
                        | LichessEngineQueue.Ask (token, _, _) | LichessEngineQueue.Done (token, _) -> state, [ LichessEngineQueue.Reply (token, None) ]
                        | _ -> state, []
                for effect in effects do
                    try execute effect
                    with ex -> logger.LogError(ex, "Lichess bot: engine queue: {Effect} failed", sprintf "%A" effect)
                return! loop state }
            loop LichessEngineQueue.initial)
    /// A search on the shared engine: the move, or None when the engine cannot give one. `asked`
    /// gets the search's token, so a game that ends can cancel it.
    let sharedSearch (position: string) (go: string) (onUpdate: SearchUpdate -> unit) (asked: int -> unit) : Task<string option> =
        match queueAgent with
        | None -> Task.FromResult None
        | Some q ->
            let token = Interlocked.Increment &nextToken
            asked token
            let tcs = TaskCompletionSource<string option>(TaskCreationOptions.RunContinuationsAsynchronously)
            lock queueReplies (fun () -> queueReplies.[token] <- tcs)
            queueListeners.[token] <- onUpdate
            q.Post (LichessEngineQueue.Ask (token, position, go))
            tcs.Task

    /// An engine for a game: an idle one from the account's agent, or a new one.
    let engineForGame () = task {
        let token = Interlocked.Increment &nextToken
        let tcs = TaskCompletionSource<LichessBotEngine option>(TaskCreationOptions.RunContinuationsAsynchronously)
        lock engineReplies (fun () -> engineReplies.[token] <- tcs)
        postBot (LichessBotMachine.EngineWanted token)
        match! tcs.Task with
        | Some e ->
            e.Engine.NewGame()
            return e
        | None -> return! Task.Run(fun () -> startEngine ())
    }

    /// One game, to its end, run by its own agent (LichessGameMachine).
    let runGame (gameId: string) (ct: CancellationToken) : Task = task {
        let config : LichessGameMachine.Config =
            { BotId = published.BotId; Greeting = settings.Greeting; Goodbye = settings.Goodbye; Limit = settings.Limit
              Book = not (String.IsNullOrWhiteSpace settings.Book); BookDepth = settings.BookDepth; AbortSeconds = 20 }
        let finished = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
        let engine : LichessBotEngine option ref = ref None
        let engineFailed = ref false
        let stream : CancellationTokenSource option ref = ref None
        let rng = Random()
        let gameAgent : MailboxProcessor<LichessGameMachine.Event> option ref = ref None
        let post e = gameAgent.Value |> Option.iter (fun (a: MailboxProcessor<LichessGameMachine.Event>) -> a.Post e)
        let onUpdate (u: SearchUpdate) =
            match u.Update with
            | Status s when s.MultiPV <= 1 ->
                let wdl = match s.WDL with HasValue w -> sprintf ", WDL %.0f/%.0f/%.0f" (w.Win / 10.0) (w.Draw / 10.0) (w.Loss / 10.0) | NotFound -> ""
                let score = match s.Eval with MiscTypes.EvalType.CP p -> Some (Some p, None) | MiscTypes.EvalType.Mate m -> Some (None, Some m) | MiscTypes.EvalType.NA -> None
                post (LichessGameMachine.EngineView (Some (sprintf "%s (White's view)%s, depth %d, %d nodes" (s.Eval.ToString()) wdl s.Depth s.Nodes), None, score))
            | NNSeq values when values.Count > 0 ->
                let top = values |> Seq.sortByDescending (fun v -> v.P) |> Seq.truncate 3 |> Seq.map (fun v -> sprintf "%s %.1f%%" (if v.SANMove <> "" then v.SANMove else v.LANMove) v.P)
                post (LichessGameMachine.EngineView (None, Some ("Policy: " + String.concat ", " top), None))
            | _ -> ()
        // the search under way: the game's end waits for it before the engine goes back
        let searching : Task ref = ref Task.CompletedTask
        // its token on the shared engine: a game that ends drops or stops it
        let sharedToken : int option ref = ref None
        let over () = finished.Task.IsCompleted || ct.IsCancellationRequested
        /// The bot's move, or None when the engine cannot give one (after one fresh start). A game that
        /// ended meanwhile is not searched (an engine that started for it is kept for the next game).
        let search (command: string) (go: string) = task {
          if shared then return! sharedSearch command go onUpdate (fun token -> sharedToken.Value <- Some token)
          else
            let mutable played : string option = None
            let mutable tries = 2
            while played.IsNone && tries > 0 && not (over ()) do
                tries <- tries - 1
                let! e =
                    task {
                        match engine.Value with
                        | Some e -> return Some e
                        | None ->
                            try
                                let! e = engineForGame ()
                                e.Listener.Value <- onUpdate
                                engine.Value <- Some e
                                return Some e
                            with ex ->
                                say (sprintf "%s: the engine does not start: %s" gameId ex.Message)
                                return None }
                match e with
                | None -> tries <- 0
                | Some _ when over () -> tries <- 0
                | Some e ->
                    let! outcome = task { try return! e.Engine.Search(command, go) |> Async.StartAsTask with ex -> return AnalysisOutcome.Failed ex.Message }
                    match outcome with
                    | AnalysisOutcome.Completed (Some info) -> played <- Some info.Move
                    // stopped because the game ended: not an engine fault, and no fresh start
                    | _ when over () -> tries <- 0
                    | other ->
                        say (sprintf "%s: the engine failed (%A)%s" gameId other (if tries > 0 then ", starting it again" else ""))
                        pending.Add(quit e)
                        engine.Value <- None
                        engineFailed.Value <- true
            return played }
        let chatInfo () =
            { EngineName = settings.Engine.Name; Net = net; Limit = settings.Limit; Playing = published.Games.Count; MaxGames = settings.Filter.MaxGames }
        let onLine (line: string) =
            match (try parseGameMessage line with _ -> None) with
            | Some m ->
                match m with
                | Full g -> postBot (LichessBotMachine.Opponent (if g.White.Id = config.BotId then g.Black.Name else g.White.Name))
                | _ -> ()
                post (LichessGameMachine.Message (m, chatInfo ()))
            | None -> ()
        /// The game's stream, after `delay`; its end comes back as StreamEnded, unless the game closed it.
        let openStream (delay: int) =
            task {
                do! pause delay ct
                if not ct.IsCancellationRequested && not finished.Task.IsCompleted then
                    let cts = CancellationTokenSource.CreateLinkedTokenSource ct
                    stream.Value <- Some cts
                    let! result = task { try return! client.StreamGame(gameId, onLine, cts.Token) with ex -> return LichessReply.Refused (0, ex.Message) }
                    if not cts.IsCancellationRequested then
                        match result with
                        | LichessReply.Refused (code, why) -> post (LichessGameMachine.StreamEnded (code, why))
                        | LichessReply.RateLimited -> post (LichessGameMachine.StreamEnded (429, "rate limited"))
                        | LichessReply.Done _ -> post (LichessGameMachine.StreamEnded (0, "closed"))
            } |> ignore
        let execute (effect: LichessGameMachine.Effect) =
            match effect with
            | LichessGameMachine.LookUpBook (count, fen) ->
                Task.Run(fun () ->
                    let uci =
                        try pickBookMove (PolyglotLines.movesFor settings.Book fen) rng
                        with ex ->
                            say (sprintf "%s: the book does not read: %s" gameId ex.Message)
                            None
                    post (LichessGameMachine.BookAnswered (count, uci))) |> ignore
            | LichessGameMachine.Search (count, command, go) ->
                searching.Value <-
                    task {
                        let! uci = search command go
                        post (LichessGameMachine.Searched (count, uci)) } :> Task
            | LichessGameMachine.SendMove (count, uci) ->
                task {
                    match! send (fun () -> client.Move(gameId, uci, ct)) "move" ct with
                    | LichessReply.Done _ -> post (LichessGameMachine.MoveDelivered count)
                    | LichessReply.Refused (code, body) -> post (LichessGameMachine.MoveRefused (count, (if code = 0 then body else sprintf "HTTP %d: %s" code body)))
                    | LichessReply.RateLimited -> post (LichessGameMachine.MoveRefused (count, "rate limited")) } |> ignore
            | LichessGameMachine.RetryAfter (count, ms) ->
                task {
                    do! pause ms ct
                    post (LichessGameMachine.Retry count) } |> ignore
            // chat never holds the game: sent on the side
            | LichessGameMachine.Chat (room, text) -> send (fun () -> client.Chat(gameId, room, text, ct)) "chat" ct |> ignore
            | LichessGameMachine.Resign _ -> send (fun () -> client.Resign(gameId, ct)) "resign" ct |> ignore
            | LichessGameMachine.Abort opponent ->
                task {
                    // only an abort Lichess took marks the opponent: one that moved meanwhile played on
                    match! send (fun () -> client.Abort(gameId, ct)) "abort" ct with
                    | LichessReply.Done _ -> postBot (LichessBotMachine.NoShow (opponent, DateTime.UtcNow))
                    | _ -> () } |> ignore
            | LichessGameMachine.FirstMoveTimer (count, ms) ->
                task {
                    do! pause ms ct
                    post (LichessGameMachine.FirstMoveDue count) } |> ignore
            | LichessGameMachine.WritePgn (g, s, plies) -> writePgn g s plies
            | LichessGameMachine.Show v -> postBot (LichessBotMachine.View v)
            | LichessGameMachine.Log text -> say text
            | LichessGameMachine.OpenStream delay -> openStream delay
            | LichessGameMachine.Finished -> finished.TrySetResult() |> ignore
        gameAgent.Value <-
            Some (MailboxProcessor<LichessGameMachine.Event>.Start(fun inbox ->
                let rec loop state = async {
                    let! event = inbox.Receive()
                    let state, effects =
                        // a step that throws loses one event, not the game
                        try LichessGameMachine.step state event
                        with ex ->
                            logger.LogError(ex, "Lichess bot: game {Game}: {Event} not handled", gameId, sprintf "%A" event)
                            state, []
                    for effect in effects do
                        try execute effect
                        with ex -> logger.LogError(ex, "Lichess bot: game {Game}: {Effect} failed", gameId, sprintf "%A" effect)
                    return! loop state }
                loop (LichessGameMachine.initial config gameId)))
        try
            openStream 0
            let! _ = Task.WhenAny(finished.Task, Task.Delay(Timeout.Infinite, ct))
            ()
        with ex -> logger.LogError(ex, "Lichess bot: game {Game}", gameId)
        // the stream is closed by the bot when the game is over: it does not wait for Lichess to close it
        stream.Value |> Option.iter (fun c -> try c.Cancel() with _ -> ())
        // the place is free at once (a rematch is not declined); the engine follows when its search has
        // stopped, and a game that asks for one meanwhile waits for it
        let coming = not shared && (engine.Value.IsSome || not searching.Value.IsCompleted)
        if coming then postBot LichessBotMachine.EngineComing
        postBot (LichessBotMachine.GameEnded (gameId, DateTime.UtcNow))
        // a search under way is stopped and waited for (an engine still starting too): an engine
        // goes back only when nothing of this game runs on it any more
        if not searching.Value.IsCompleted then
            engine.Value |> Option.iter (fun e -> try e.Engine.Stop() with _ -> ())
            if shared then sharedToken.Value |> Option.iter (fun token -> postQueue (LichessEngineQueue.Cancel token))
            let! _ = Task.WhenAny(searching.Value, Task.Delay 15000)
            ()
        // a sound engine waits for the next game; one that failed is not trusted again
        let release () =
            match engine.Value with
            | Some e when not engineFailed.Value && not ct.IsCancellationRequested ->
                e.Listener.Value <- ignore
                postBot (LichessBotMachine.EngineBack e)
            | Some e ->
                pending.Add(quit e)
                if coming then postBot LichessBotMachine.EngineLost
            | None -> if coming then postBot LichessBotMachine.EngineLost
        if searching.Value.IsCompleted then release ()
        else
            // still not done (a net's first load): the engine is quit when it comes
            engineFailed.Value <- true
            pending.Add(searching.Value.ContinueWith(fun (_: Task) -> release ()))
        gameTasks.TryRemove gameId |> ignore
    }

    let executeBot (effect: LichessBotMachine.Effect<LichessBotEngine>) =
        match effect with
        | LichessBotMachine.Accept id -> send (fun () -> client.Accept(id, gamesCt)) "accept" gamesCt |> ignore
        | LichessBotMachine.Decline (id, reason) -> send (fun () -> client.Decline(id, reason, gamesCt)) "decline" gamesCt |> ignore
        | LichessBotMachine.PostChallenge (token, r) ->
            task {
                // no retry here: the page waits for this answer
                let! reply = task { try return! client.Challenge(r.Name, r.Seconds, r.Increment, r.Rated, r.Color, gamesCt) with ex -> return LichessReply.Refused (0, ex.Message) }
                let result =
                    match reply with
                    | LichessReply.Done body ->
                        match challengeIdOf body with
                        | Some id -> LichessBotMachine.SentAs id
                        | None -> LichessBotMachine.SendFailed ("Lichess's answer has no challenge id", false)
                    | LichessReply.RateLimited -> LichessBotMachine.SendFailed (LichessBotMachine.rateLimitedText, true)
                    | LichessReply.Refused (0, text) -> LichessBotMachine.SendFailed (text, false)
                    | LichessReply.Refused (_, body) -> LichessBotMachine.SendFailed (errorText body, false)
                postBot (LichessBotMachine.Sent (token, result, DateTime.UtcNow)) } |> ignore
        | LichessBotMachine.Answer (token, result) ->
            take challengeReplies token |> Option.iter (fun tcs -> tcs.TrySetResult result |> ignore)
        | LichessBotMachine.CancelOnLichess id ->
            pending.Add(task { try do! client.CancelChallenge(id, CancellationToken.None) :> Task with _ -> () })
        | LichessBotMachine.ListBots ->
            task {
                let! bots = botsOnline 1000 gamesCt
                postBot (LichessBotMachine.Listed (bots, DateTime.UtcNow)) } |> ignore
        | LichessBotMachine.StartGame id ->
            let ct = gamesCt
            gameTasks.[id] <- Task.Run(fun () -> runGame id ct)
        | LichessBotMachine.GiveEngine (token, e) ->
            match take engineReplies token with
            | Some tcs -> tcs.TrySetResult e |> ignore
            | None -> e |> Option.iter (fun e -> pending.Add(quit e))
        | LichessBotMachine.Quit e -> pending.Add(quit e)
        | LichessBotMachine.Log text -> say text

    let startAgent () =
        MailboxProcessor<BotMessage>.Start(fun inbox ->
            let rec loop state = async {
                match! inbox.Receive() with
                | Sync reply ->
                    reply.Reply()
                    return! loop state
                | Step event ->
                    let state, effects =
                        // a dead agent would hang the bot: a step that throws answers whoever waits on it
                        try LichessBotMachine.step state event
                        with ex ->
                            logger.LogError(ex, "Lichess bot: {Event} not handled", sprintf "%A" event)
                            match event with
                            | LichessBotMachine.Send (token, _, _) -> state, [ LichessBotMachine.Answer (token, Error ex.Message) ]
                            | LichessBotMachine.EngineWanted token -> state, [ LichessBotMachine.GiveEngine (token, None) ]
                            | LichessBotMachine.EngineBack e -> state, [ LichessBotMachine.Quit e ]
                            | _ -> state, []
                    published <- state
                    for effect in effects do
                        try executeBot effect
                        with ex -> logger.LogError(ex, "Lichess bot: {Effect} failed", sprintf "%A" effect)
                    return! loop state }
            loop published)

    /// Waits until the account's agent has handled every message posted before.
    let sync () : Task =
        match botAgent with
        | Some a -> a.PostAndAsyncReply(fun reply -> Sync reply) |> Async.StartAsTask :> Task
        | None -> Task.CompletedTask

    /// The games being played now.
    member _.Games = published.Games |> Map.toArray |> Array.choose snd
    /// The challenges the bot sent that have no answer yet, oldest first.
    member _.Outgoing = published.Outgoing |> Map.toArray |> Array.map snd |> Array.sortBy (fun c -> c.At)
    /// What became of the last challenge the bot sent: sent, accepted, declined (with Lichess's reason)...
    member _.LastChallengeAnswer = published.LastAnswer

    /// Challenges a player (a person or a bot) to a standard game: the game that follows is played like
    /// any other. Ok with the challenge's id; Error with why not (offline, full, or Lichess's refusal).
    member _.Challenge(username: string, seconds: int, increment: int, rated: bool, color: string, _ct: CancellationToken) : Task<Result<string, string>> =
        match botAgent with
        | None -> Task.FromResult(Error "the bot is not online")
        | Some a ->
            let token = Interlocked.Increment &nextToken
            let tcs = TaskCompletionSource<Result<string, string>>(TaskCreationOptions.RunContinuationsAsynchronously)
            lock challengeReplies (fun () -> challengeReplies.[token] <- tcs)
            let request : LichessBotMachine.Request = { Name = username; Seconds = seconds; Increment = increment; Rated = rated; Color = color; Auto = false }
            a.Post (Step (LichessBotMachine.Send (token, request, DateTime.UtcNow)))
            tcs.Task

    /// The bots online on Lichess now (at most `count`), but this one; Error with why not.
    member _.OnlineBots(count: int, ct: CancellationToken) : Task<Result<OnlineBot[], string>> = task {
        let! bots = botsOnline count ct
        return bots |> Result.map (Array.filter (fun b -> not (String.Equals(b.Name, published.BotId, StringComparison.OrdinalIgnoreCase))))
    }

    /// Withdraws a challenge the bot sent.
    member _.CancelChallenge(challengeId: string, _ct: CancellationToken) : Task =
        postBot (LichessBotMachine.Cancel challengeId)
        Task.CompletedTask
    member _.BotId = published.BotId
    /// True from going online (the engine started) to going offline: before it, the engine is starting.
    member _.IsOnline = published.Online

    /// Runs the bot until the token is cancelled. Error at once when the account is not a bot account or
    /// the engine does not play; on any error the games in play end too.
    member _.Run(ct: CancellationToken) : Task<Result<unit, string>> = task {
        match! client.Account ct with
        | Error e -> return Error e
        | Ok (_, name, title) when title <> "BOT" ->
            return Error (sprintf "%s is not a bot account: upgrade it first (for good; only an account that has played no games)" name)
        | Ok (id, name, _) ->
            // the games end with the bot, whatever ends it
            use stop = CancellationTokenSource.CreateLinkedTokenSource ct
            gamesCt <- stop.Token
            let agent = startAgent ()
            botAgent <- Some agent
            try
                try
                    // one search before going online: a net's first (TensorRT) load must not cost the first game;
                    // the engine then waits in the pool for it
                    say (sprintf "starting %s (a net's first load can take minutes)" settings.Engine.Name)
                    // the start and the first search can take minutes (a TensorRT plan built for a new net):
                    // going offline meanwhile stops them, and an engine that comes up after that is quit
                    let starting = Task.Run(fun () -> startEngine ())
                    let! _ = Task.WhenAny(starting, Task.Delay(Timeout.Infinite, ct))
                    if not starting.IsCompleted then
                        starting.ContinueWith(fun (t: Task<LichessBotEngine>) ->
                            if t.Status = TaskStatus.RanToCompletion then (try t.Result.Engine.Quit() with _ -> ())) |> ignore
                        ct.ThrowIfCancellationRequested()
                    let! warm = starting
                    let! first =
                        task {
                            use _quitOnStop = ct.Register(fun () -> try warm.Engine.Stop(); warm.Engine.Quit() with _ -> ())
                            try return! warm.Engine.Search("position startpos", "go nodes 1") |> Async.StartAsTask
                            with ex -> return AnalysisOutcome.Failed ex.Message }
                    ct.ThrowIfCancellationRequested()
                    match first with
                    | AnalysisOutcome.Completed (Some _) when shared ->
                        let q = startQueue ()
                        queueAgent <- Some q
                        q.Post (LichessEngineQueue.Started warm)
                        say "one engine plays every game"
                    | AnalysisOutcome.Completed (Some _) -> agent.Post (Step (LichessBotMachine.EngineBack warm))
                    | other ->
                        (try warm.Engine.Quit() with _ -> ()) |> ignore
                        failwithf "the engine %s does not play (%A): check its def" settings.Engine.Name other
                    agent.Post (Step (LichessBotMachine.WentOnline (id, DateTime.UtcNow)))
                    do! sync ()
                    say (sprintf "online as %s (%s)" name settings.Engine.Name)
                    settings.Matchmaking |> Option.iter (fun m -> say (sprintf "matchmaking: challenging bots after %d s without a game" m.PauseSeconds))
                    // the clock of the account's agent: expiries and matchmaking
                    let rng = Random()
                    let ticks = gamesCt
                    task {
                        while not ticks.IsCancellationRequested do
                            do! pause 2000 ticks
                            agent.Post (Step (LichessBotMachine.Tick (DateTime.UtcNow, rng.Next()))) } |> ignore
                    let mutable backoff = 5000
                    while not ct.IsCancellationRequested do
                        let onEvent (line: string) =
                            backoff <- 5000
                            let now = DateTime.UtcNow
                            match (try parseEvent line with _ -> None) with
                            | Some (ChallengeArrived c) -> agent.Post (Step (LichessBotMachine.ChallengeIn (c, now)))
                            | Some (ChallengeGone id) -> agent.Post (Step (LichessBotMachine.Gone (id, now)))
                            | Some (ChallengeDeclined (id, reason)) -> agent.Post (Step (LichessBotMachine.Declined (id, reason, now)))
                            | Some (GameStarted gameId) -> agent.Post (Step (LichessBotMachine.GameStarted (gameId, now)))
                            | _ -> ()
                        let! result = task { try return! client.StreamEvents(onEvent, ct) with ex -> return LichessReply.Refused (0, ex.Message) }
                        if not ct.IsCancellationRequested then
                            match result with
                            | LichessReply.Refused (401, _) -> failwith "the token was rejected"
                            | LichessReply.RateLimited ->
                                say "Lichess asks for a pause, waiting a minute"
                                do! pause 60000 ct
                            | r ->
                                say (sprintf "the event stream ended (%s), opening it again in %d s" (match r with LichessReply.Refused (c, b) -> sprintf "HTTP %d %s" c b | _ -> "closed") (backoff / 1000))
                                do! pause backoff ct
                                backoff <- min 60000 (backoff * 2)
                    say "offline"
                    return Ok ()
                with
                | :? OperationCanceledException when ct.IsCancellationRequested -> say "offline"; return Ok ()
                | ex -> say ex.Message; return Error ex.Message
            finally
                // a challenge left open would start a game nobody plays: withdrawn before the games end
                agent.Post (Step LichessBotMachine.Offline)
                (try (sync ()).Wait(TimeSpan.FromSeconds 5.0) |> ignore with _ -> ())
                (try Task.WaitAll(pending.ToArray(), TimeSpan.FromSeconds 5.0) |> ignore with _ -> ())
                stop.Cancel()
                (try Task.WaitAll(gameTasks.Values |> Seq.toArray, TimeSpan.FromSeconds 20.0) |> ignore with _ -> ())
                agent.Post (Step LichessBotMachine.Shutdown)
                // the shared engine is quit; a search asked after that is answered at once, when the quit is under way
                postQueue LichessEngineQueue.Shutdown
                (try (sharedSearch "" "" ignore ignore).Wait(TimeSpan.FromSeconds 5.0) |> ignore with _ -> ())
                (try (sync ()).Wait(TimeSpan.FromSeconds 5.0) |> ignore with _ -> ())
                (try Task.WaitAll(pending.ToArray(), TimeSpan.FromSeconds 10.0) |> ignore with _ -> ())
    }
