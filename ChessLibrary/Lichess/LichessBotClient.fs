namespace ChessLibrary

open System
open System.IO
open System.Net
open System.Net.Http
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

/// What a request to Lichess came to.
type LichessReply =
    | Done of body: string
    /// 429: Lichess asks for a minute's pause
    | RateLimited
    | Refused of status: int * body: string

/// The Lichess bot API over HTTP with a bearer token; `baseUrl` is lichess.org or a test server.
type LichessBotClient(baseUrl: string, token: string) =
    let client = new HttpClient(Timeout = Timeout.InfiniteTimeSpan)
    do client.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Bearer", token)
    do client.DefaultRequestHeaders.UserAgent.ParseAdd "EngineBattle-bot"
    let url (path: string) = baseUrl.TrimEnd('/') + path

    let reply (response: HttpResponseMessage) = task {
        let! body = response.Content.ReadAsStringAsync()
        return
            match response.StatusCode with
            | HttpStatusCode.OK -> Done body
            | HttpStatusCode.TooManyRequests -> RateLimited
            | code -> Refused (int code, body)
    }

    let post (path: string) (form: (string * string) list) (ct: CancellationToken) = task {
        use content = new FormUrlEncodedContent(form |> List.map (fun (k, v) -> Collections.Generic.KeyValuePair(k, v)))
        // a request that hangs must not hold the game: 20 s
        use cts = CancellationTokenSource.CreateLinkedTokenSource ct
        cts.CancelAfter(TimeSpan.FromSeconds 20.0)
        use! response = client.PostAsync(url path, content, cts.Token)
        return! reply response
    }

    /// Reads an ndjson stream to its end; one silent for `idleSeconds` (a dead connection) ends as Refused (0, ...).
    let stream (path: string) (onLine: string -> unit) (ct: CancellationToken) = task {
        let idleSeconds = 30.0
        use request = new HttpRequestMessage(HttpMethod.Get, url path)
        use connect = CancellationTokenSource.CreateLinkedTokenSource ct
        connect.CancelAfter(TimeSpan.FromSeconds idleSeconds)
        use! response = client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead, connect.Token)
        if response.StatusCode <> HttpStatusCode.OK then
            let! r = reply response
            return r
        else
            use! body = response.Content.ReadAsStreamAsync(ct)
            use reader = new StreamReader(body)
            let mutable go = true
            let mutable result = Done ""
            while go do
                use idle = CancellationTokenSource.CreateLinkedTokenSource ct
                idle.CancelAfter(TimeSpan.FromSeconds idleSeconds)
                let! line =
                    task {
                        try
                            let! l = reader.ReadLineAsync(idle.Token).AsTask()
                            return Some l
                        with :? OperationCanceledException when not ct.IsCancellationRequested -> return None }
                match line with
                | None ->
                    result <- Refused (0, sprintf "no data for %.0f s" idleSeconds)
                    go <- false
                | Some null -> go <- false
                | Some l -> onLine l
            return result
    }

    member _.BaseUrl = baseUrl

    /// The account: id, username and title ("BOT" once upgraded).
    member _.Account(ct: CancellationToken) : Task<Result<string * string * string, string>> = task {
        use! response = client.GetAsync(url "/api/account", ct)
        match! reply response with
        | Done body ->
            let n = JsonNode.Parse body
            let s (k: string) = match n.[k] with null -> "" | v -> v.GetValue<string>()
            return Ok (s "id", s "username", s "title")
        | RateLimited -> return Error "Lichess asks for a pause (429): try again in a minute"
        | Refused (401, _) -> return Error "the token was rejected: a token with the bot:play scope is needed"
        | Refused (code, body) -> return Error (sprintf "HTTP %d: %s" code body)
    }

    /// Turns the account into a bot account, for good. Lichess refuses an account that has played games.
    member _.UpgradeToBot(ct) = post "/api/bot/account/upgrade" [] ct
    member _.StreamEvents(onLine: string -> unit, ct: CancellationToken) = stream "/api/stream/event" onLine ct
    member _.StreamGame(gameId: string, onLine: string -> unit, ct: CancellationToken) = stream (sprintf "/api/bot/game/stream/%s" gameId) onLine ct
    member _.Accept(challengeId: string, ct) = post (sprintf "/api/challenge/%s/accept" challengeId) [] ct
    /// `reason`: a declineReasonKey (later, timecontrol, nobot...), shown to the challenger in their language.
    member _.Decline(challengeId: string, reason: string, ct) = post (sprintf "/api/challenge/%s/decline" challengeId) [ "reason", reason ] ct
    member _.Move(gameId: string, uci: string, ct) = post (sprintf "/api/bot/game/%s/move/%s" gameId uci) [] ct
    member _.Resign(gameId: string, ct) = post (sprintf "/api/bot/game/%s/resign" gameId) [] ct
    /// Ends a game before both sides have moved; it counts for nobody.
    member _.Abort(gameId: string, ct) = post (sprintf "/api/bot/game/%s/abort" gameId) [] ct
    /// `room`: player or spectator.
    member _.Chat(gameId: string, room: string, text: string, ct) = post (sprintf "/api/bot/game/%s/chat" gameId) [ "room", room; "text", text ] ct
    /// Challenges a player to a standard game: seconds + increment, rated or casual, the bot's colour
    /// (random, white or black). The answer is the challenge, with its id.
    member _.Challenge(username: string, seconds: int, increment: int, rated: bool, color: string, ct) =
        post (sprintf "/api/challenge/%s" (Uri.EscapeDataString username))
             [ "clock.limit", string seconds; "clock.increment", string increment; "rated", (if rated then "true" else "false")
               "color", color; "variant", "standard" ] ct
    /// The bots online now (at most `count`), one JSON user per line.
    member _.OnlineBots(count: int, ct: CancellationToken) : Task<Result<string[], string>> = task {
        use cts = CancellationTokenSource.CreateLinkedTokenSource ct
        cts.CancelAfter(TimeSpan.FromSeconds 20.0)
        use! response = client.GetAsync(url (sprintf "/api/bot/online?nb=%d" count), cts.Token)
        match! reply response with
        | Done body -> return Ok (body.Split('\n', StringSplitOptions.RemoveEmptyEntries))
        | RateLimited -> return Error "Lichess asks for a pause (429): try again in a minute"
        | Refused (code, body) -> return Error (sprintf "HTTP %d: %s" code body)
    }
    /// Withdraws a challenge the bot sent.
    member _.CancelChallenge(challengeId: string, ct) = post (sprintf "/api/challenge/%s/cancel" challengeId) [] ct

    interface IDisposable with
        member _.Dispose() = client.Dispose()
