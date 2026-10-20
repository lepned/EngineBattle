/// The bot's account as a pure step function: state + event -> state + effects; LichessBot's agent
/// runs it. One owner for the places (games, accepted challenges, challenges it sent), the answers
/// to its challenges, matchmaking and the idle engines: a free place is checked and taken in one
/// step, so nothing can take it in between, and an answer that comes before its challenge's id is
/// known waits in the state for it. Time and chance come in with the events.
module ChessLibrary.LichessBotMachine

open System
open ChessLibrary.LichessBotLogic

type Config =
    { Filter: Filter
      Matchmaking: Matchmaking option }

/// A challenge to send: from the page (a reply waits for it) or from matchmaking.
type Request =
    { Name: string
      Seconds: int
      Increment: int
      Rated: bool
      Color: string
      Auto: bool }

/// An answer to a challenge whose id was not known yet.
type EarlyAnswer =
    | WasAccepted
    | WasDeclined of reason: string
    | WasCancelled

type SendResult =
    | SentAs of id: string
    | SendFailed of why: string * rateLimited: bool

type State<'E> =
    { Config: Config
      BotId: string
      Online: bool
      ShuttingDown: bool
      /// The games in play, with the page's view once the game has shown it.
      Games: Map<string, LichessBotGame option>
      /// Challenges accepted whose game has not started yet (a challenge's id is its game's).
      Accepted: Map<string, DateTime>
      /// Challenges on their way to Lichess, by token: their id is not known yet.
      Sending: Map<int, OutgoingChallenge>
      /// Challenges sent with no answer yet.
      Outgoing: Map<string, OutgoingChallenge>
      Early: Map<string, EarlyAnswer * DateTime>
      /// Who declined (or did not answer) a challenge lately: lower-case name -> when.
      DeclinedBy: Map<string, DateTime>
      /// Who never made its first move: lower-case name -> when.
      NoShows: Map<string, DateTime>
      LastOpponent: string
      /// When a game last started or ended (or the bot went online).
      LastActivity: DateTime
      /// The earliest time for matchmaking's next challenge.
      NextAuto: DateTime
      /// The bots online, as last read, and when; Listing while a read is under way.
      Bots: OnlineBot[]
      BotsAt: DateTime
      Listing: bool
      /// Matchmaking's last note, said once.
      Note: string
      /// What became of the last challenge the bot sent, for the page.
      LastAnswer: string
      /// Tokens for matchmaking's own challenges (negative: the page's are positive).
      NextToken: int
      /// Engines between games: a net loads once, not per game.
      Idle: 'E list
      /// Engines on their way back from games that ended (their search stopping), and the games
      /// waiting for one of them rather than starting a new engine.
      Returning: int
      WaitingForEngine: int list }

type Event<'E> =
    | WentOnline of botId: string * now: DateTime
    /// Every few seconds: the clock, and a random number for matchmaking's draws.
    | Tick of now: DateTime * roll: int
    | ChallengeIn of Challenge * now: DateTime
    | Gone of id: string * now: DateTime
    | Declined of id: string * reason: string * now: DateTime
    | GameStarted of id: string * now: DateTime
    | Opponent of name: string
    /// An opponent that never moved: not challenged again for 6 hours.
    | NoShow of name: string * now: DateTime
    | View of LichessBotGame
    | GameEnded of id: string * now: DateTime
    | Send of token: int * Request * now: DateTime
    | Sent of token: int * SendResult * now: DateTime
    | Cancel of id: string
    | Listed of Result<OnlineBot[], string> * now: DateTime
    | EngineWanted of token: int
    | EngineBack of 'E
    /// A game that ended will hand its engine back once its search has stopped.
    | EngineComing
    /// That engine was quit instead (it failed, or never came).
    | EngineLost
    /// Going offline: no new games or challenges; the challenges sent are withdrawn.
    | Offline
    /// The games have ended: the idle engines are quit, and engines that come back after.
    | Shutdown

type Effect<'E> =
    | Accept of id: string
    | Decline of id: string * reason: string
    | PostChallenge of token: int * Request
    | Answer of token: int * Result<string, string>
    | CancelOnLichess of id: string
    | ListBots
    | StartGame of id: string
    | GiveEngine of token: int * 'E option
    | Quit of 'E
    | Log of string

let rateLimitedText = "Lichess asks for a pause: try again in a minute"

let initial (config: Config) : State<'E> =
    { Config = config; BotId = ""; Online = false; ShuttingDown = false; Games = Map.empty; Accepted = Map.empty
      Sending = Map.empty; Outgoing = Map.empty; Early = Map.empty; DeclinedBy = Map.empty; NoShows = Map.empty; LastOpponent = ""
      LastActivity = DateTime.MinValue; NextAuto = DateTime.MinValue; Bots = [||]; BotsAt = DateTime.MinValue
      Listing = false; Note = ""; LastAnswer = ""; NextToken = -1; Idle = []; Returning = 0; WaitingForEngine = [] }

/// Places taken: games, accepted challenges, challenges sent or on their way.
let taken (st: State<'E>) = st.Games.Count + st.Accepted.Count + st.Sending.Count + st.Outgoing.Count

let private lower (s: string) = s.ToLowerInvariant()
let private same (a: string) (b: string) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)
let private answered text (st: State<'E>) = { st with LastAnswer = text }, [ Log text ]

let private earlyText who answer =
    match answer with
    | WasAccepted -> sprintf "%s accepted the challenge" who
    | WasDeclined reason -> sprintf "%s declined: %s" who reason
    | WasCancelled -> sprintf "the challenge to %s was cancelled" who

/// An answer about challenge `id`: its own when the challenge is known, kept for it otherwise.
let private answer (id: string) (a: EarlyAnswer) (now: DateTime) (st: State<'E>) =
    match st.Outgoing.TryFind id with
    | Some c ->
        let st = { st with Outgoing = st.Outgoing.Remove id }
        let st = match a with WasDeclined _ -> { st with DeclinedBy = st.DeclinedBy.Add(lower c.Opponent, now) } | _ -> st
        answered (earlyText c.Opponent a) st
    | None -> { st with Early = st.Early.Add(id, (a, now)) }, []

/// Matchmaking's turn: a challenge to a bot that fits, when the bot is free and has waited.
let private matchmake (m: Matchmaking) (now: DateTime) (roll: int) (st: State<'E>) =
    let waiting = st.Sending |> Map.exists (fun _ c -> c.Auto) || st.Outgoing |> Map.exists (fun _ c -> c.Auto)
    if not st.Online || st.ShuttingDown || m.TimeControls.IsEmpty || waiting
       || taken st >= st.Config.Filter.MaxGames
       || (now - st.LastActivity).TotalSeconds < float m.PauseSeconds || now < st.NextAuto then st, []
    elif now - st.BotsAt > TimeSpan.FromMinutes 5.0 then
        if st.Listing then st, [] else { st with Listing = true }, [ ListBots ]
    else
        let st = { st with NextAuto = now.AddSeconds(float m.GapSeconds) }
        let seconds, increment = m.TimeControls.[(roll &&& 0xFFFF) % m.TimeControls.Length]
        let speed = speedOf seconds increment
        // no games at this speed yet: the start rating, not Lichess's placeholder
        let own = st.Bots |> Array.tryFind (fun b -> same b.Name st.BotId) |> Option.bind (ratingIn speed) |> Option.defaultValue m.StartRating
        let skip (name: string) =
            same name st.BotId || same name st.LastOpponent
            || (match st.DeclinedBy.TryFind(lower name) with Some at -> now - at < TimeSpan.FromHours 1.0 | None -> false)
            || (match st.NoShows.TryFind(lower name) with Some at -> now - at < TimeSpan.FromHours 6.0 | None -> false)
        match pickOpponent st.Bots (Some own) m.RatingRange speed skip (fun n -> (roll >>> 16) % n) with
        | None ->
            let note = sprintf "matchmaking: no bot online fits (%s, within %d of %d)" speed m.RatingRange own
            if note = st.Note then st, [] else { st with Note = note }, [ Log note ]
        | Some b ->
            let token = st.NextToken
            let request = { Name = b.Name; Seconds = seconds; Increment = increment; Rated = m.Rated; Color = "random"; Auto = true }
            let c = { Id = ""; Opponent = b.Name; Seconds = seconds; Increment = increment; Rated = m.Rated; Color = "random"; At = now; Auto = true }
            { st with Note = ""; NextToken = token - 1; Sending = st.Sending.Add(token, c) }, [ PostChallenge (token, request) ]

let step (st: State<'E>) (event: Event<'E>) : State<'E> * Effect<'E> list =
    match event with
    | WentOnline (botId, now) -> { st with BotId = botId; Online = true; LastActivity = now }, []

    | Tick (now, roll) ->
        // an accepted challenge whose game never came does not hold a place forever
        let accepted = st.Accepted |> Map.filter (fun _ at -> now - at <= TimeSpan.FromMinutes 2.0)
        let early = st.Early |> Map.filter (fun _ (_, at) -> now - at <= TimeSpan.FromMinutes 1.0)
        // a challenge with no answer is withdrawn: matchmaking's after a minute, the page's after 3
        let wait (c: OutgoingChallenge) = if c.Auto then TimeSpan.FromMinutes 1.0 else TimeSpan.FromMinutes 3.0
        let expired = st.Outgoing |> Map.filter (fun _ c -> now - c.At > wait c) |> Map.toList
        let st = { st with Accepted = accepted; Early = early; Outgoing = expired |> List.fold (fun o (id, _) -> Map.remove id o) st.Outgoing }
        let st, withdrawn =
            expired
            |> List.fold (fun (st, effects) (id, c) ->
                let st = if c.Auto then { st with DeclinedBy = st.DeclinedBy.Add(lower c.Opponent, now) } else st
                let text = sprintf "no answer from %s in %.0f minute(s): challenge withdrawn" c.Opponent (wait c).TotalMinutes
                { st with LastAnswer = text }, effects @ [ Log text; CancelOnLichess id ]) (st, [])
        let st, more = match st.Config.Matchmaking with Some m -> matchmake m now roll st | None -> st, []
        st, withdrawn @ more

    | ChallengeIn (c, _) when same c.Challenger.Id st.BotId -> st, []
    | ChallengeIn (c, now) ->
        if not st.Online || st.ShuttingDown then st, [ Decline (c.Id, "later") ]
        else
            match declineReason st.Config.Filter (taken st) c with
            | None ->
                { st with Accepted = st.Accepted.Add(c.Id, now) },
                [ Log (sprintf "accepting %s from %s (%s %s)" c.Id c.Challenger.Name c.Speed (if c.Rated then "rated" else "casual")); Accept c.Id ]
            | Some reason -> st, [ Log (sprintf "declining %s from %s: %s" c.Id c.Challenger.Name reason); Decline (c.Id, reason) ]

    | Gone (id, now) -> answer id WasCancelled now { st with Accepted = st.Accepted.Remove id }
    | Declined (id, reason, now) -> answer id (WasDeclined reason) now { st with Accepted = st.Accepted.Remove id }

    | GameStarted (id, _) when st.Games.ContainsKey id -> st, []
    | GameStarted (id, now) ->
        let st = { st with Accepted = st.Accepted.Remove id; Games = st.Games.Add(id, None); LastActivity = now }
        let st, said = if st.Outgoing.ContainsKey id then answer id WasAccepted now st else { st with Early = st.Early.Add(id, (WasAccepted, now)) }, []
        st, said @ [ Log (sprintf "game %s started" id); StartGame id ]
    | Opponent name -> { st with LastOpponent = name }, []
    | NoShow (name, now) -> { st with NoShows = st.NoShows.Add(lower name, now) }, []
    | View v -> (if st.Games.ContainsKey v.GameId then { st with Games = st.Games.Add(v.GameId, Some v) } else st), []
    | GameEnded (id, now) -> { st with Games = st.Games.Remove id; LastActivity = now }, []

    | Send (token, r, now) ->
        let name = r.Name.Trim().TrimStart('@')
        let refuse why = st, [ Answer (token, Error why) ]
        if not st.Online || st.ShuttingDown then refuse "the bot is not online"
        elif name = "" then refuse "whom to challenge: a Lichess username"
        elif same name st.BotId then refuse "the bot cannot challenge itself"
        elif taken st >= st.Config.Filter.MaxGames then
            refuse (sprintf "the bot is full: %d game(s) at once, counting challenges waiting for an answer" st.Config.Filter.MaxGames)
        else
            let c = { Id = ""; Opponent = name; Seconds = r.Seconds; Increment = r.Increment; Rated = r.Rated; Color = r.Color; At = now; Auto = r.Auto }
            { st with Sending = st.Sending.Add(token, c) }, [ PostChallenge (token, { r with Name = name }) ]

    | Sent (token, result, now) ->
        match st.Sending.TryFind token with
        | None -> st, []
        | Some c ->
            let st = { st with Sending = st.Sending.Remove token }
            match result with
            | SentAs id ->
                let sent = sprintf "challenged %s (%g+%d %s), waiting for an answer" c.Opponent (float c.Seconds / 60.0) c.Increment (if c.Rated then "rated" else "casual")
                match st.Early.TryFind id with
                | Some (a, _) ->
                    // answered before its id was known: that answer stands
                    let st = { st with Early = st.Early.Remove id }
                    let st = match a with WasDeclined _ -> { st with DeclinedBy = st.DeclinedBy.Add(lower c.Opponent, now) } | _ -> st
                    let text = earlyText c.Opponent a
                    { st with LastAnswer = text }, [ Log sent; Log text; Answer (token, Ok id) ]
                // the bot went offline while it was on its way: withdrawn, or a game nobody plays could start
                | None when not st.Online ->
                    st, [ Log (sprintf "withdrawing the challenge to %s" c.Opponent); CancelOnLichess id; Answer (token, Error "the bot went offline") ]
                | None ->
                    { st with Outgoing = st.Outgoing.Add(id, { c with Id = id; At = now }); LastAnswer = sent }, [ Log sent; Answer (token, Ok id) ]
            | SendFailed (why, rateLimited) when c.Auto ->
                if rateLimited then { st with NextAuto = now.AddMinutes 10.0 }, [ Log "matchmaking: Lichess asks for a pause, waiting 10 minutes" ]
                else { st with DeclinedBy = st.DeclinedBy.Add(lower c.Opponent, now) }, [ Log (sprintf "matchmaking: %s: %s" c.Opponent why) ]
            | SendFailed (why, _) -> st, [ Answer (token, Error why) ]

    | Cancel id ->
        match st.Outgoing.TryFind id with
        | Some c ->
            let st, said = answered (sprintf "challenge to %s withdrawn" c.Opponent) { st with Outgoing = st.Outgoing.Remove id }
            st, said @ [ CancelOnLichess id ]
        | None -> st, []

    | Listed (Ok bots, now) -> { st with Bots = bots; BotsAt = now; Listing = false }, []
    | Listed (Error why, now) ->
        // tried again after the next 5 minutes, not every tick
        let note = sprintf "matchmaking: the bots online do not read: %s" why
        { st with BotsAt = now; Listing = false; Note = note }, (if note = st.Note then [] else [ Log note ])

    | EngineWanted token ->
        match st.Idle with
        | e :: rest -> { st with Idle = rest }, [ GiveEngine (token, Some e) ]
        // one is on its way back: wait for it rather than load another net
        | [] when st.Returning > st.WaitingForEngine.Length -> { st with WaitingForEngine = st.WaitingForEngine @ [ token ] }, []
        | [] -> st, [ GiveEngine (token, None) ]
    | EngineComing -> { st with Returning = st.Returning + 1 }, []
    | EngineBack e ->
        let st = { st with Returning = max 0 (st.Returning - 1) }
        if st.ShuttingDown then st, [ Quit e ]
        else
            match st.WaitingForEngine with
            | token :: rest -> { st with WaitingForEngine = rest }, [ GiveEngine (token, Some e) ]
            | [] -> { st with Idle = e :: st.Idle }, []
    | EngineLost ->
        let st = { st with Returning = max 0 (st.Returning - 1) }
        // one fewer on its way: a game that waited for it more than there are to come starts its own
        if st.WaitingForEngine.Length > st.Returning then
            match st.WaitingForEngine with
            | token :: rest -> { st with WaitingForEngine = rest }, [ GiveEngine (token, None) ]
            | [] -> st, []
        else st, []

    | Offline ->
        let withdrawn =
            st.Outgoing |> Map.toList |> List.collect (fun (id, c) -> [ Log (sprintf "withdrawing the challenge to %s" c.Opponent); CancelOnLichess id ])
        { st with Online = false; Outgoing = Map.empty }, withdrawn
    | Shutdown ->
        { st with Online = false; ShuttingDown = true; Idle = []; WaitingForEngine = [] },
        (st.Idle |> List.map Quit) @ (st.WaitingForEngine |> List.map (fun token -> GiveEngine (token, None)))
