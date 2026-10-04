/// A game review (Game Review page) as a pure step: state + event -> state + effects. The page owns
/// the engine, the board and the panel and carries out the effects; which position is searched,
/// which search's lines count and what each position gave live here.
module ChessLibrary.ReviewModel

open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.GameAccuracyAnalysis

type EngineState = NotStarted | Starting | Ready

type Review =
  { Positions: ReviewPosition array
    Go: string
    MultiPv: int
    /// ucinewgame before each search (node searches: no tree carried over)
    NewGameEachSearch: bool
    Index: int
    /// The search of Positions.[Index].
    Current: int option
    Pvs: Map<int, MultiPVResult>
    /// Newest first.
    Outcomes: PositionOutcome list }

type Phase =
  | Idle
  /// Waiting for the engine to start.
  | Waiting of Review
  | Reviewing of Review

type State = { Engine: EngineState; Phase: Phase }

type Event =
  | Start of positions: ReviewPosition array * go: string * multiPv: int * newGameEachSearch: bool
  | EngineReady
  /// The engine failed to start, or was replaced.
  | EngineGone of reason: string
  | Started of search: int
  | Result of search: int * EngineUpdate
  | Cancel

type Effect =
  | StartEngine
  | SetMultiPv of int
  | NewGame
  /// Answer with Started.
  | Search of positionCmd: string * go: string
  | StopEngine
  /// The engine died: drop it, the next review starts a new one.
  | QuitEngine
  /// Board and panel show the position before move `ply` (the final position for the last).
  | ShowPly of ply: int
  | Progress of current: int * total: int
  /// The eval of the position before move `ply`, as soon as it is known.
  | LiveEval of ply: int * EvalType
  /// The review's line for the panel to show.
  | Forward of EngineUpdate
  /// Every position done, in order: score them.
  | Finished of PositionOutcome array
  | Failed of reason: string
  | Cancelled

let initial = { Engine = NotStarted; Phase = Idle }

let isRunning (state: State) = match state.Phase with Idle -> false | _ -> true

/// On to the next position: unsearched ones are recorded at once, a searched one is sent.
let rec private advance (review: Review) (effects: Effect list) =
  if review.Index >= review.Positions.Length then
    Idle, effects @ [ Finished (review.Outcomes |> List.rev |> List.toArray) ]
  else
    let position = review.Positions.[review.Index]
    let shown = effects @ [ Progress (review.Index + 1, review.Positions.Length); ShowPly position.Ply ]
    match position.Kind with
    | Searched ->
        let fresh = if review.NewGameEachSearch then [ NewGame ] else []
        Reviewing { review with Current = None; Pvs = Map.empty },
        shown @ fresh @ [ Search (position.PositionCmd, review.Go) ]
    | _ ->
        let outcome = unsearchedOutcome position
        let live = match outcome.Eval with NA -> [] | eval -> [ LiveEval (position.Ply, eval) ]
        advance { review with Index = review.Index + 1; Outcomes = outcome :: review.Outcomes } (shown @ live)

let private begin' (review: Review) =
  advance review [ SetMultiPv review.MultiPv; NewGame ]

let private failed reason (state: State) =
  { state with Phase = Idle }, [ Failed reason ]

let step (state: State) (event: Event) : State * Effect list =
  match event with
  | Start _ when isRunning state -> state, []
  | Start (positions, go, multiPv, newGameEachSearch) ->
      let review =
        { Positions = positions; Go = go; MultiPv = max 1 multiPv; NewGameEachSearch = newGameEachSearch
          Index = 0; Current = None; Pvs = Map.empty; Outcomes = [] }
      match state.Engine with
      | Ready ->
          let phase, effects = begin' review
          { state with Phase = phase }, effects
      | Starting -> { state with Phase = Waiting review }, []
      | NotStarted -> { state with Engine = Starting; Phase = Waiting review }, [ StartEngine ]

  | EngineReady ->
      let state = { state with Engine = Ready }
      match state.Phase with
      | Waiting review ->
          let phase, effects = begin' review
          { state with Phase = phase }, effects
      | _ -> state, []

  | EngineGone reason ->
      let state = { state with Engine = NotStarted }
      if isRunning state then failed reason state else state, []

  | Started search ->
      match state.Phase with
      | Reviewing review when review.Current.IsNone -> { state with Phase = Reviewing { review with Current = Some search } }, []
      | _ -> state, []

  // a start that fails is reported by EngineGone
  | Result (_, EngineFailed _) when state.Engine <> Ready -> state, []
  | Result (_, EngineFailed (_, reason)) ->
      let state = { state with Engine = NotStarted }
      if isRunning state then
        let state, effects = failed reason state
        state, QuitEngine :: effects
      else state, [ QuitEngine ]

  | Result (search, update) ->
      match state.Phase with
      | Reviewing review when review.Current = Some search ->
          match update with
          | Status status ->
              let k, line = pvOfStatus status
              { state with Phase = Reviewing { review with Pvs = review.Pvs.Add(k, line) } }, [ Forward update ]
          // the bestmove line; Done alone means it held no legal move (scored without one)
          | BestMove _ | Done _ ->
              let move = match update with BestMove info -> info.Move | _ -> ""
              let position = review.Positions.[review.Index]
              let outcome = searchedOutcome review.Pvs move
              let live = match outcome.Eval with NA -> [] | eval -> [ LiveEval (position.Ply, eval) ]
              let phase, effects =
                advance { review with Index = review.Index + 1; Current = None; Outcomes = outcome :: review.Outcomes }
                        (Forward update :: live)
              { state with Phase = phase }, effects
          | SearchStopped _ -> failed "the engine ended a search without a move" state
          | _ -> state, [ Forward update ]
      // a cancelled review's lines, or a search not of this review
      | _ -> state, []

  | Cancel ->
      match state.Phase with
      | Reviewing review ->
          let stop = match review.Current with Some _ -> [ StopEngine ] | None -> []
          { state with Phase = Idle }, stop @ [ Cancelled ]
      | Waiting _ -> { state with Phase = Idle }, [ Cancelled ]
      | Idle -> state, []
