/// The engine panel's search control and the results it shows, as a pure step:
/// state + event -> state + effects. The panel sends events in and carries out the effects
/// (the engine, the charts, the board); every start, stop and clear goes through here once.
module ChessLibrary.PanelModel

open System
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PageEngine

type Limit =
  | Nodes of int
  | MoveTime of ms: int
  | Infinite

/// What a start asks for, and whether the board's position has a move to search.
type Request = { Limit: Limit; HasMove: bool }

type State =
  { Engine: EngineState
    /// A search of ours runs: the timer runs and Stop is enabled.
    Searching: bool
    /// The search whose results are shown; any other's are dropped.
    Current: int option
    /// A start waiting for the engine.
    Pending: Request option
    Auto: bool
    /// Counts navigations; a settle for an older one is stale.
    Navigation: int
    Focused: string list
    /// The MultiPV the engine was told; a line above it is not shown.
    Lines: int
    /// The PV table, by MultiPV index.
    Rows: Map<int, EngineStatus>
    /// The eval last published to the host (cp, mate).
    LastEval: (float option * int option) option
    /// A host (Game Review) drives the searches: no start of our own, every result is its.
    Reviewing: bool }

type Event =
  /// Start, an F-key, Ctrl+G: a running search is replaced.
  | Start of Request
  | Stop
  /// Escape.
  | Reset
  /// The board moved.
  | Navigated
  /// The debounce after a navigation ran out.
  | Settled of navigation: int * Request
  | AutoChanged of bool
  | FocusToggled of move: string * Request
  | FocusRemoved of move: string * Request
  | FocusCleared of Request
  | LinesChanged of int
  /// The Engine button: a new engine starts.
  | LoadEngine
  | EngineReady
  /// The engine exited or could not be started.
  | EngineGone
  /// The engine's id for the search just requested.
  | Started of search: int
  /// An update and the id of the search it belongs to (0 for the engine's own, or a host's).
  | Result of search: int * EngineUpdate
  | ReviewingChanged of bool

type Effect =
  | StartEngine
  /// Run this search (with these searchmoves); answer with Started.
  | Search of Limit * searchMoves: string list
  | StopEngine
  /// The engine died: drop it, the next start starts a new one.
  | QuitEngine
  /// Stop, ucinewgame, no searchmoves.
  | ResetEngine
  /// Answer with Settled after the delay.
  | Settle of navigation: int * delayMs: int
  /// The previous position's info lines, live stats and iteration log (cheap).
  | ClearLists
  /// The previous search's charts (a call into the page's script: once per search, not per key).
  | ClearCharts
  /// Start (true) or stop the search clock.
  | Timer of running: bool
  | SetMultiPv of int
  /// The rows (or the focused moves) changed.
  | ShowRows
  | PublishEval of cp: float option * mate: int option
  | Completed of BestMoveInfo
  /// An accepted update for the panel's own views (charts, live stats, info lines).
  | Show of EngineUpdate
  /// Live stats of another search (a stopped one, after a move): not shown, but the host caches
  /// them under their own position for when the user steps back.
  | Late of EngineUpdate

/// How long auto-search waits on a position before it searches it.
let autoSearchDelayMs = 150

let initial (lines: int) =
  { Engine = NotStarted; Searching = false; Current = None; Pending = None; Auto = false; Navigation = 0
    Focused = []; Lines = max 1 lines; Rows = Map.empty; LastEval = None; Reviewing = false }

let private firstMove (pv: string) =
  if String.IsNullOrEmpty pv then None
  else match pv.Split(' ', StringSplitOptions.RemoveEmptyEntries) with [||] -> None | parts -> Some parts.[0]

let private stopSearch (state: State) =
  let effects = (if state.Engine = Ready then [ StopEngine ] else []) @ (if state.Searching then [ Timer false ] else [])
  { state with Searching = false }, effects

/// A new search: the previous one's charts go; the engine starts first if it is not there.
let private startSearch (request: Request) (state: State) =
  if not request.HasMove then state, []
  else
    match state.Engine with
    | Ready ->
        { state with Searching = true; Current = None; Pending = None; LastEval = None },
        // the charts last: clearing them awaits the page's script, and nothing may wait before the
        // search is sent (an event let in meanwhile would act on a start that already happened)
        [ ClearLists; Search (request.Limit, state.Focused); Timer true; ClearCharts ]
    | Starting -> { state with Pending = Some request }, []
    | NotStarted -> { state with Engine = Starting; Pending = Some request }, [ StartEngine ]

/// New focused moves: a running search is rerun with them, the stale duplicates go.
let private refocus (focused: string list) (request: Request) (state: State) =
  if focused = state.Focused then state, []
  else
    let state = { state with Focused = focused }
    // the views stay: the rerun's lines replace the old ones as they come
    if state.Searching && state.Engine = Ready && request.HasMove then
      { state with Current = None }, [ Search (request.Limit, focused); Timer true; ShowRows ]
    else state, [ ShowRows ]

let private evalOf = function
  | CP cp -> Some (Some cp, None)
  | Mate m -> Some (None, Some m)
  | NA -> None

/// Published when it moved enough to show (a host re-renders on each).
let private evalChange (last: (float option * int option) option) (eval: EvalType) =
  match evalOf eval with
  | None -> last, []
  | Some (cp, mate) ->
      let changed =
        match last with
        | None -> true
        | Some (lastCp, lastMate) ->
            mate <> lastMate
            || cp.IsSome <> lastCp.IsSome
            || (match cp, lastCp with Some a, Some b -> abs (a - b) >= 0.05 | _ -> false)
      if changed then Some (cp, mate), [ PublishEval (cp, mate) ] else last, []

let private status (s: EngineStatus) (update: EngineUpdate) (state: State) =
  let k = s.MultiPV
  // a line above the cap: the slider was lowered while the search ran (a host's are its own)
  if k > state.Lines && not state.Reviewing then state, []
  else
    let rows = state.Rows.Add(k, s)
    let rows =
      match state.Focused, firstMove s.PVLongSAN with
      | [], _ | _, None -> rows
      // a focused rerun moves a line to another index: the old copy goes
      | _, Some move -> rows |> Map.filter (fun key row -> key = k || firstMove row.PVLongSAN <> Some move)
    let lastEval, publish = if k <= 1 then evalChange state.LastEval s.Eval else state.LastEval, []
    { state with Rows = rows; LastEval = lastEval }, [ ShowRows ] @ publish @ [ Show update ]

let private bestMove (info: BestMoveInfo) (state: State) =
  // a value-head engine reports its move only here: it fills an empty main line
  let rows =
    match state.Rows.TryFind 1 with
    | Some row when String.IsNullOrEmpty row.PV && not (String.IsNullOrEmpty info.PV) ->
        state.Rows.Add(1, { row with PV = info.PV; PVLongSAN = info.Move })
    | _ -> state.Rows
  let timer = if state.Searching then [ Timer false ] else []
  { state with Searching = false; Rows = rows }, timer @ [ ShowRows; Completed info ]

let private result (search: int) (update: EngineUpdate) (state: State) =
  match update with
  | EngineFailed _ when state.Reviewing -> state, [ Show update ]
  | EngineFailed _ ->
      let timer = if state.Searching then [ Timer false ] else []
      // a start that fails is the start's to report; a running engine that fails is dropped
      if state.Engine = Ready then
        { state with Engine = NotStarted; Searching = false; Current = None; Pending = None }, timer @ [ QuitEngine; Show update ]
      else { state with Searching = false }, timer @ [ Show update ]
  | EngineUpdate.Ready _ | UCIInfo _ -> state, [ Show update ]
  | NNSeq _ when not (state.Reviewing || state.Current = Some search) -> state, [ Late update ]
  | _ when not (state.Reviewing || state.Current = Some search) -> state, []
  | Status s -> status s update state
  | BestMove info -> bestMove info state
  | SearchStopped _ ->
      let timer = if state.Searching then [ Timer false ] else []
      { state with Searching = false }, timer
  | _ -> state, [ Show update ]

let step (state: State) (event: Event) : State * Effect list =
  match event with
  | Start _ when state.Reviewing -> state, []
  | Start request -> startSearch request state
  // both cancel an auto-search still waiting to start
  | Stop ->
      let state, effects = stopSearch state
      { state with Pending = None; Navigation = state.Navigation + 1 }, effects
  | Reset ->
      let state, stop = stopSearch state
      let reset = if state.Engine = Ready then [ ResetEngine ] else []
      { state with Current = None; Pending = None; Focused = []; Rows = Map.empty; LastEval = None; Navigation = state.Navigation + 1 },
      stop @ reset @ [ ClearLists; ClearCharts; ShowRows ]
  | Navigated ->
      let state, stop = stopSearch state
      let navigation = state.Navigation + 1
      let state =
        { state with Navigation = navigation; Current = None; Pending = None; Focused = []; Rows = Map.empty; LastEval = None }
      let settle = if state.Auto then [ Settle (navigation, autoSearchDelayMs) ] else []
      state, stop @ [ ClearLists; ShowRows ] @ settle
  | Settled (navigation, request) ->
      // auto-search never starts an engine, but one that is starting searches the newest position
      if navigation <> state.Navigation || not state.Auto || state.Reviewing then state, []
      else
        match state.Engine with
        | Ready -> startSearch request state
        | Starting -> { state with Pending = (if request.HasMove then Some request else None) }, []
        | NotStarted -> state, []
  | AutoChanged on -> { state with Auto = on }, []
  | FocusToggled (move, request) ->
      let focused =
        if List.contains move state.Focused then List.filter ((<>) move) state.Focused else state.Focused @ [ move ]
      refocus focused request state
  | FocusRemoved (move, request) -> refocus (List.filter ((<>) move) state.Focused) request state
  | FocusCleared request -> refocus [] request state
  | LinesChanged lines ->
      let lines = max 1 lines
      { state with Lines = lines; Rows = state.Rows |> Map.filter (fun k _ -> k <= lines) }, [ SetMultiPv lines; ShowRows ]
  | LoadEngine ->
      let state, stop = stopSearch state
      { state with Engine = Starting; Current = None; Pending = None }, stop @ [ StartEngine ]
  | EngineReady ->
      let state = { state with Engine = Ready }
      match state.Pending with
      | Some request -> startSearch request { state with Pending = None }
      | None -> state, []
  | EngineGone ->
      let timer = if state.Searching then [ Timer false ] else []
      { state with Engine = NotStarted; Searching = false; Current = None; Pending = None }, timer
  | Started search -> { state with Current = Some search }, []
  | Result (search, update) -> result search update state
  // the host's review takes the panel over: its own search stops
  | ReviewingChanged true when not state.Reviewing ->
      let state, effects = stopSearch state
      { state with Reviewing = true; Current = None; Pending = None; Navigation = state.Navigation + 1 }, effects
  | ReviewingChanged reviewing -> { state with Reviewing = reviewing }, []
