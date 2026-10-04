/// A game against the engine (Play vs computer, the contempt page's Train), as a pure step:
/// state + event -> state + effects. The page owns the board and the engine and carries out the
/// effects; the clocks, whose turn it is, the engine's search and the result live here.
module ChessLibrary.PlayModel

open System
open ChessLibrary.EngineTypes

type TimeControl =
  | Clocked of whiteBase: TimeSpan * blackBase: TimeSpan * whiteInc: TimeSpan * blackInc: TimeSpan
  /// No clock: the engine searches this many nodes a move.
  | NodesPerMove of int
  /// No clock: the engine gets a long one.
  | Unlimited

/// How the game ended by the board's rules.
type BoardEnd =
  | Checkmate
  | Stalemate
  | InsufficientMaterial
  | Threefold
  | FiftyMoves

/// The board after a move (or at the start), as the page sees it.
type Facts = { WhiteToMove: bool; Ply: int; End: BoardEnd option }

type EngineState = NotStarted | Starting | Ready

type Phase = Idle | Playing | Over

/// What the engine is told to search with.
type Go =
  | GoClock of wtime: TimeSpan * btime: TimeSpan * winc: TimeSpan * binc: TimeSpan
  | GoNodes of int

type State =
  { Engine: EngineState
    Phase: Phase
    HumanWhite: bool
    Tc: TimeControl
    WhiteToMove: bool
    Ply: int
    /// Remaining time when the running clock last started (Since).
    White: TimeSpan
    Black: TimeSpan
    Since: TimeSpan
    /// The clocks at each position of the game, newest first: a takeback goes back to them.
    History: (TimeSpan * TimeSpan) list
    /// The engine's search for its move.
    Current: int option
    /// A game waiting for the engine to start.
    Pending: bool }

type Event =
  | Start of TimeControl * humanWhite: bool * Facts * now: TimeSpan
  /// Start the engine now (Load Engine), not at the next game.
  | LoadEngine
  | EngineReady of now: TimeSpan
  /// The engine exited, failed to start, or was changed.
  | EngineGone of reason: string * now: TimeSpan
  | Started of search: int
  | Result of search: int * EngineUpdate * now: TimeSpan
  /// The human moved on the board.
  | HumanMoved of Facts * now: TimeSpan
  /// The page played the engine's move on the board.
  | EngineMoved of Facts * now: TimeSpan
  /// The engine's move could not be played.
  | EngineMoveRefused of move: string * now: TimeSpan
  | Takeback of now: TimeSpan
  /// The page stepped back.
  | TookBack of Facts * now: TimeSpan
  | Force
  | Resign of now: TimeSpan
  | Abort of now: TimeSpan
  | Tick of now: TimeSpan
  /// The side the human plays, between games.
  | SideChanged of humanWhite: bool

type Effect =
  | StartEngine
  | NewGame
  /// Answer with Started.
  | Search of Go
  | StopEngine
  /// The engine died: drop it, the next game starts a new one.
  | QuitEngine
  /// Play it on the board; answer with EngineMoved or EngineMoveRefused.
  | PlayEngineMove of uci: string
  /// Step back this many plies; answer with TookBack.
  | StepBack of plies: int
  | ClockRunning of bool
  /// The game is over: record the result (Win, Loss, Draw, Aborted for the human).
  | Ended of result: string * reason: string * status: string
  | ShowStatus of string
  /// After the engine's move: a premove queued meanwhile is played.
  | PlayPremove

let initial =
  { Engine = NotStarted; Phase = Idle; HumanWhite = true; Tc = Clocked (TimeSpan.FromMinutes 3.0, TimeSpan.FromMinutes 3.0, TimeSpan.FromSeconds 2.0, TimeSpan.FromSeconds 2.0)
    WhiteToMove = true; Ply = 0; White = TimeSpan.Zero; Black = TimeSpan.Zero; Since = TimeSpan.Zero
    History = []; Current = None; Pending = false }

let private clocked state = match state.Tc with Clocked _ -> true | _ -> false

let private humanToMove state = state.HumanWhite = state.WhiteToMove

/// The time left for a side now: the running clock counts down from Since.
let remaining (state: State) (white: bool) (now: TimeSpan) =
  let stored = if white then state.White else state.Black
  if state.Phase = Playing && not state.Pending && clocked state && state.WhiteToMove = white then
    let left = stored - (now - state.Since)
    if left < TimeSpan.Zero then TimeSpan.Zero else left
  else stored

let private increment state white =
  match state.Tc with
  | Clocked (_, _, wi, bi) -> if white then wi else bi
  | _ -> TimeSpan.Zero

/// The game ends: clocks frozen at now, the engine stopped, the result recorded.
let private finish (result: string) (reason: string) (status: string) (now: TimeSpan) (state: State) =
  let white, black = remaining state true now, remaining state false now
  let stop = match state.Current with Some _ -> [ StopEngine ] | None -> []
  { state with Phase = Over; White = white; Black = black; Current = None; Pending = false },
  stop @ [ ClockRunning false; Ended (result, reason, status) ]

let private boardEnd (ending: BoardEnd) (state: State) =
  match ending with
  | Checkmate ->
      // the side to move is mated
      let humanWon = state.HumanWhite <> state.WhiteToMove
      (if humanWon then "Win" else "Loss"), "Checkmate", (if humanWon then "Checkmate! You win." else "Checkmate! Computer wins.")
  | Stalemate -> "Draw", "Stalemate", "Stalemate. Draw."
  | InsufficientMaterial -> "Draw", "Insufficient material", "Draw by insufficient material."
  | Threefold -> "Draw", "Threefold repetition", "Draw by threefold repetition."
  | FiftyMoves -> "Draw", "Fifty-move rule", "Draw by the 50-move rule."

/// Kept off the engine's own clock in go: the time from bestmove to the board counts against it.
let engineMargin = TimeSpan.FromMilliseconds 200.0

let private go (state: State) (now: TimeSpan) =
  let engineSide white t =
    if white = state.HumanWhite then t
    else max (TimeSpan.FromMilliseconds 10.0) (t - engineMargin)
  match state.Tc with
  | Clocked (_, _, wi, bi) -> GoClock (engineSide true (remaining state true now), engineSide false (remaining state false now), wi, bi)
  | NodesPerMove n -> GoNodes n
  | Unlimited -> GoClock (TimeSpan.FromMinutes 300.0, TimeSpan.FromMinutes 300.0, TimeSpan.Zero, TimeSpan.Zero)

/// It is the engine's turn: it searches.
let private engineTurn (now: TimeSpan) (state: State) =
  { state with Current = None }, [ Search (go state now); ShowStatus "Computer to move." ]

/// A move was made (by either side): the mover's clock stops with its increment, the board's
/// end is checked, and the engine searches if it is its turn now.
let private moved (facts: Facts) (now: TimeSpan) (state: State) =
  let mover = state.WhiteToMove
  let left = remaining state mover now + increment state mover
  let white, black = if mover then left, state.Black else state.White, left
  // the mover's clock already ran out: the move came too late
  if clocked state && remaining state mover now <= TimeSpan.Zero then
    let humanLost = mover = state.HumanWhite
    finish (if humanLost then "Loss" else "Win") "Time" (if humanLost then "You lost on time." else "Computer lost on time.") now state
  else
    let state =
      { state with WhiteToMove = facts.WhiteToMove; Ply = facts.Ply; White = white; Black = black; Since = now
                   History = (white, black) :: state.History; Current = None }
    match facts.End with
    | Some ending ->
        let result, reason, status = boardEnd ending state
        finish result reason status now state
    | None when humanToMove state -> state, [ ShowStatus "Your move." ]
    | None -> engineTurn now state

let private startGame (now: TimeSpan) (state: State) =
  let state = { state with Since = now; Pending = false }
  let opening = [ NewGame; ClockRunning (clocked state) ]
  if humanToMove state then state, opening @ [ ShowStatus "Your move." ]
  else
    let state, effects = engineTurn now state
    state, opening @ effects

let step (state: State) (event: Event) : State * Effect list =
  match event with
  | Start (tc, humanWhite, facts, now) ->
      let white, black =
        match tc with
        | Clocked (wb, bb, _, _) -> wb, bb
        | _ -> TimeSpan.Zero, TimeSpan.Zero
      let stop = match state.Current with Some _ -> [ StopEngine ] | None -> []
      let state =
        { state with Phase = Playing; HumanWhite = humanWhite; Tc = tc; WhiteToMove = facts.WhiteToMove; Ply = facts.Ply
                     White = white; Black = black; Since = now; History = [ white, black ]; Current = None }
      match state.Engine with
      | Ready ->
          let state, effects = startGame now state
          state, stop @ effects
      | Starting -> { state with Pending = true }, stop @ [ ShowStatus "Initializing engine..." ]
      | NotStarted -> { state with Engine = Starting; Pending = true }, stop @ [ ShowStatus "Initializing engine..."; StartEngine ]

  | EngineReady now ->
      let state = { state with Engine = Ready }
      // the clock starts once the engine is there, not while it loads
      if state.Pending && state.Phase = Playing then startGame now state
      elif state.Phase = Playing then state, []
      else state, [ ShowStatus "Engine ready." ]

  | LoadEngine when state.Engine = NotStarted -> { state with Engine = Starting }, [ ShowStatus "Initializing engine..."; StartEngine ]
  | LoadEngine -> state, []

  | EngineGone (reason, now) ->
      let state = { state with Engine = NotStarted }
      // the game waited for it: it never began
      if state.Pending then { state with Phase = Idle; Pending = false }, [ ClockRunning false; ShowStatus (sprintf "The game did not start: %s." reason) ]
      elif state.Phase = Playing then finish "Aborted" "Engine" (sprintf "Game aborted: %s." reason) now state
      else { state with Pending = false }, []

  | Started search -> { state with Current = Some search }, []

  // a start that fails is reported by EngineGone
  | Result (_, EngineFailed _, _) when state.Engine <> Ready -> state, []
  | Result (_, EngineFailed (_, reason), now) ->
      // the engine exited or stopped answering, in this game's search or not
      let state = { state with Engine = NotStarted }
      if state.Phase = Playing then
        let state, effects = finish "Aborted" "Engine" (sprintf "Game aborted: %s." reason) now state
        state, QuitEngine :: effects
      else state, [ QuitEngine; ShowStatus (sprintf "The engine stopped: %s." reason) ]
  | Result (search, update, now) when state.Phase = Playing && state.Current = Some search ->
      match update with
      // the search is over: its Done (next) is not news
      | BestMove info when not (humanToMove state) -> { state with Current = None }, [ PlayEngineMove info.Move ]
      // a bestmove line with no legal move in it
      | Done _ -> finish "Aborted" "Engine" "Game aborted: the engine gave no legal move." now state
      | SearchStopped _ -> finish "Aborted" "Engine" "Game aborted: the search ended without a move." now state
      | _ -> state, []
  | Result _ -> state, []

  | HumanMoved (facts, now) ->
      // only the human's own move on its turn counts (navigation is locked during a game)
      if state.Phase = Playing && humanToMove state && facts.Ply = state.Ply + 1 then moved facts now state
      else state, []

  | EngineMoved (facts, now) ->
      if state.Phase = Playing && not (humanToMove state) then
        let state, effects = moved facts now state
        state, effects @ (if state.Phase = Playing then [ PlayPremove ] else [])
      else state, []

  | EngineMoveRefused (move, now) when state.Phase = Playing ->
      finish "Aborted" "Engine" (sprintf "Game aborted: the engine played an illegal move (%s)." move) now state
  | EngineMoveRefused _ -> state, []

  | Takeback _ when state.Phase <> Playing || state.Ply = 0 -> state, []
  | Takeback _ ->
      // back to the human's turn: one ply while the engine thinks, two on the human's turn
      let plies = if humanToMove state then min 2 state.Ply else 1
      let stop = match state.Current with Some _ -> [ StopEngine ] | None -> []
      { state with Current = None }, stop @ [ StepBack plies ]

  | TookBack (facts, now) when state.Phase = Playing ->
      let back = max 0 (state.Ply - facts.Ply)
      let history = List.skip (min back (List.length state.History - 1)) state.History
      let white, black = List.head history
      let state =
        { state with WhiteToMove = facts.WhiteToMove; Ply = facts.Ply; White = white; Black = black; Since = now
                     History = history; Current = None }
      if humanToMove state then state, [ ShowStatus "Your move." ] else engineTurn now state
  | TookBack _ -> state, []

  | Force when state.Phase = Playing && state.Current.IsSome && not (humanToMove state) -> state, [ StopEngine ]
  | Force -> state, []

  // a game still waiting for the engine never began
  | Resign _ | Abort _ when state.Phase = Playing && state.Pending ->
      { state with Phase = Idle; Pending = false }, [ ShowStatus "Game aborted." ]
  | Resign now when state.Phase = Playing -> finish "Loss" "Resignation" "You resigned. Computer wins." now state
  | Resign _ -> state, []
  | Abort now when state.Phase = Playing -> finish "Aborted" "Agreement" "Game aborted." now state
  | Abort _ -> state, []

  | Tick now when state.Phase = Playing && clocked state && state.Engine = Ready && not state.Pending ->
      let side = state.WhiteToMove
      if remaining state side now <= TimeSpan.Zero then
        let humanLost = side = state.HumanWhite
        finish (if humanLost then "Loss" else "Win") "Time" (if humanLost then "You lost on time." else "Computer lost on time.") now state
      else state, []
  | Tick _ -> state, []

  | SideChanged humanWhite when state.Phase <> Playing -> { state with HumanWhite = humanWhite }, []
  | SideChanged _ -> state, []
