/// Which broadcast game the page shows, as a pure step: state + event -> state + effects. The page
/// owns the feed, the board and the kibitzer and carries out the effects; the automatic choices live
/// here: a live game on arrival, the next live game when the followed one ends (after a short hold),
/// waiting for one that has not started yet (TCEC plays one game at a time), and the tour's next
/// round once this one is over.
module ChessLibrary.BroadcastFocus

open System

/// A game as the choice needs it.
type Game =
  { Key: string
    Moves: int
    Finished: bool
    /// Both players' ratings: the strongest pairing goes first.
    Rating: int }

/// Under way: it has moves and no result yet (a game not started yet is not live).
let isLive (g: Game) = g.Moves > 0 && not g.Finished

type Round = { Id: string; Ongoing: bool }

type Pending =
  | NoPending
  /// The followed game ended; the strongest live game is taken at `due`, the result shows meanwhile.
  | Holding of next: string * due: TimeSpan
  /// The followed game ended and none is live: the next to start is taken. `fetchedAt`: when the
  /// tour's rounds were last asked for (once every game of the round has finished).
  | Waiting of fetchedAt: TimeSpan option

type State =
  { Round: string
    Focus: string option
    /// The page chooses: the user has picked no game since the round started.
    Auto: bool
    /// The focus was live at the last update: its end starts the hand-over.
    FocusLive: bool
    /// On the focus's newest move; browsing away clears it.
    Follow: bool
    Pending: Pending }

type Event =
  | RoundStarted of roundId: string
  | Games of Game list * now: TimeSpan
  /// A tile clicked.
  | Picked of key: string * Game list
  | Stay
  | FollowChanged of bool
  | Tick of Game list * now: TimeSpan
  | RoundsFetched of Round list * now: TimeSpan

type Effect =
  /// Show this game from its newest move; `other`: a different game than before (its evals reset).
  | Show of key: string * other: bool
  | FetchRounds
  | StartRound of roundId: string

/// How long a finished game's result shows before the next live game.
let holdTime = TimeSpan.FromSeconds 10.0
/// How often the tour's rounds are asked for while the round is over.
let fetchEvery = TimeSpan.FromSeconds 60.0

let initial roundId =
  { Round = roundId; Focus = None; Auto = true; FocusLive = false; Follow = true; Pending = NoPending }

/// The strongest game, live ones first.
let private best (games: Game list) =
  games |> List.sortBy (fun g -> (if isLive g then 0 else 1), -g.Rating, g.Key) |> List.tryHead

let private bestLive (except: string option) (games: Game list) =
  games
  |> List.filter (fun g -> isLive g && Some g.Key <> except)
  |> List.sortBy (fun g -> -g.Rating, g.Key)
  |> List.tryHead

let private show (g: Game) (state: State) =
  { state with
      Focus = Some g.Key
      FocusLive = isLive g
      Follow = true
      Pending = NoPending
      // a live game in focus ends the page's choosing
      Auto = state.Auto && not (isLive g) },
  [ Show (g.Key, state.Focus <> Some g.Key) ]

/// While waiting with every game of the round over: the tour's rounds, once a minute.
let private askForRound (games: Game list) now (state: State) =
  match state.Pending with
  | Waiting fetched when not games.IsEmpty
                         && games |> List.forall (fun g -> g.Finished)
                         && fetched |> Option.forall (fun t -> now - t >= fetchEvery) ->
      { state with Pending = Waiting (Some now) }, [ FetchRounds ]
  | _ -> state, []

/// The followed game ended: the next live game after the hold, or wait for one.
let private handOver (games: Game list) now (state: State) =
  let state = { state with FocusLive = false }
  if not state.Follow then state, []
  else
    match bestLive state.Focus games with
    | Some g -> { state with Pending = Holding (g.Key, now + holdTime) }, []
    | None -> askForRound games now { state with Pending = Waiting None }

let private onGames (games: Game list) now (state: State) =
  match state.Focus with
  | None ->
      match best games with
      | Some g -> show g state
      | None -> state, []
  | Some key ->
      match games |> List.tryFind (fun g -> g.Key = key) with
      // an update of a round just left
      | None -> state, []
      | Some focus ->
          match state.Pending with
          | Waiting _ ->
              match bestLive None games with
              | Some g -> show g state
              | None -> askForRound games now state
          | Holding _ -> state, []
          | NoPending when state.FocusLive && focus.Finished -> handOver games now state
          // a game the page chose that is not under way gives way to one that is
          | NoPending when state.Auto && not (isLive focus) ->
              match bestLive (Some key) games with
              | Some g -> show g state
              | None -> state, []
          | NoPending -> { state with FocusLive = isLive focus }, []

let step (state: State) (event: Event) : State * Effect list =
  match event with
  | RoundStarted roundId -> initial roundId, []
  | Games (games, now) -> onGames games now state
  | Picked (key, games) ->
      let live = games |> List.exists (fun g -> g.Key = key && isLive g)
      { state with Focus = Some key; Auto = false; FocusLive = live; Follow = true; Pending = NoPending },
      [ Show (key, state.Focus <> Some key) ]
  | Stay -> { state with Pending = NoPending; Auto = false }, []
  // browsing the finished game keeps it: the hand-over and the wait end
  | FollowChanged false -> { state with Follow = false; Pending = NoPending }, []
  | FollowChanged true -> { state with Follow = true }, []
  | Tick (games, now) ->
      match state.Pending with
      | Holding (_, due) when now >= due ->
          // the strongest live game now, not the one announced at the start of the hold
          match bestLive state.Focus games with
          | Some g -> show g state
          | None -> askForRound games now { state with Pending = Waiting None }
      | Waiting _ -> askForRound games now state
      | _ -> state, []
  | RoundsFetched (rounds, _) ->
      // only an answer to our own ask: the round was over then
      match state.Pending, rounds |> List.tryFind (fun r -> r.Ongoing && r.Id <> state.Round) with
      | Waiting (Some _), Some next -> state, [ StartRound next.Id ]
      | _ -> state, []
