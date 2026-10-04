/// A player's protocol state, as a pure step function: state + event -> state + effects.
/// The Player agent runs it; the game loop never sees ponder or stop bookkeeping.
module ChessLibrary.Game.PlayerMachine

open System
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.Game.GoCommand
open ChessLibrary.Game.EngineLine
open ChessLibrary.Game.SearchStats

type Command =
  | Position of string
  | IsReady
  | Go of Search
  | GoPonder of string
  | PonderHit
  | Stop

/// Board-bound conversions for the searched position.
type PositionView =
  { ShortPv: string -> string
    /// Fills in SANMove.
    ShortSan: NNValues list -> unit }

type ThinkRequest =
  { Position: string
    /// The opponent's move just played; pondering on it is a hit.
    LastMove: string option
    Search: Search
    StopAfter: TimeSpan option
    WhiteToMove: bool
    View: PositionView }

type PonderRequest =
  { /// Includes the ponder move.
    Position: string
    Move: string
    Command: string
    /// Side to move after the ponder move.
    WhiteToMove: bool
    /// Conversions for the position after the ponder move.
    View: PositionView }

type SearchOutcome =
  | Moved of move: string * ponder: string option * elapsed: TimeSpan * stats: SearchStats
  /// Stopped after its time, and no bestmove within StopWait.
  | NoBestMove of elapsed: TimeSpan
  | Stalled of reason: string
  | Crashed
  /// The game ended during the search.
  | Interrupted

type Settings =
  { Player: string
    CanPing: bool
    PingTimeout: TimeSpan
    StopWait: TimeSpan
    StatusInterval: TimeSpan
    PolicyTest: bool }

type Waiting =
  | ForBestMove
  | ForReadyOk

type After =
  | ThenThink of ThinkRequest
  | ThenEnd

type Thinking =
  { Request: ThinkRequest
    StartedAt: TimeSpan
    StopSentAt: TimeSpan option
    Stats: SearchStats
    LastStatusAt: TimeSpan }

type Pondering =
  { Request: PonderRequest
    /// Kept for a hit: the engine may answer ponderhit at once, with no new info line.
    Stats: SearchStats
    LastStatusAt: TimeSpan }

type State =
  | Idle
  /// isready sent before go.
  | Readying of ThinkRequest * since: TimeSpan
  | Thinking of Thinking
  | Pondering of Pondering
  /// Output is dropped until the awaited line (stop sent: a bestmove; isready sent: readyok).
  | Draining of waitFor: Waiting * after: After * since: TimeSpan
  /// Stopped answering; not waited for again.
  | Unresponsive
  | Closed

type Event =
  | Line of string
  | OutputClosed
  | Think of ThinkRequest
  | Ponder of PonderRequest
  | EndGame
  /// A deadline may have passed.
  | Tick

type Effect =
  | Send of Command
  | ReplyThink of SearchOutcome
  /// false: the engine stopped answering and needs a restart.
  | ReplyEndGame of idle: bool
  | Emit of Update
  | Note of string
  | Warn of string

let private startThinking (settings: Settings) now (request: ThinkRequest) =
  { Request = request; StartedAt = now; StopSentAt = None; LastStatusAt = now
    Stats = SearchStats.start settings.Player request.WhiteToMove }

let private beginThink (settings: Settings) now (request: ThinkRequest) =
  if settings.CanPing then
    Readying (request, now), [ Send (Position request.Position); Send IsReady ]
  else
    Thinking (startThinking settings now request), [ Send (Position request.Position); Send (Go request.Search) ]

let private emitBlock (view: PositionView) (block: NNValues list) (stats: SearchStats) =
  try
    view.ShortSan block
    withNNTop block stats, [ Emit (NNSeq (ResizeArray block)) ]
  with ex ->
    // a bad stats block must not cost the bestmove that may follow it
    stats, [ Warn (sprintf "LogLiveStats block dropped: %s" ex.Message) ]

/// A line while thinking: everything but the bestmove updates the stats.
let private thinkingLine (settings: Settings) now (t: Thinking) (line: EngineLine) =
  let view = t.Request.View
  let stats, flushed =
    match line with
    | NNStats _ -> t.Stats, []
    | _ ->
        match flushNN t.Stats with
        | stats, Some block -> emitBlock view block stats
        | stats, None -> stats, []
  match line with
  | BestMove (move, ponder) ->
      Idle, flushed @ [ ReplyThink (Moved (move, ponder, now - t.StartedAt, stats)) ]
  | Info text ->
      match onInfo view.ShortPv text stats with
      | Some stats when stats.Status.Eval <> EvalType.NA && now - t.LastStatusAt >= settings.StatusInterval ->
          Thinking { t with Stats = stats; LastStatusAt = now }, flushed @ [ Emit (Status stats.Status) ]
      | Some stats -> Thinking { t with Stats = stats }, flushed
      | None -> Thinking { t with Stats = stats }, flushed
  | NNStats text when not settings.PolicyTest ->
      match onNNLine text stats with
      | stats, Some block ->
          let stats, effects = emitBlock view block stats
          Thinking { t with Stats = stats }, effects
      | stats, None -> Thinking { t with Stats = stats }, []
  | EngineMessage text ->
      Thinking { t with Stats = stats }, flushed @ [ Emit (MessagesFromEngine ("Ceres", text)); Note text ]
  | NNStats _ | ReadyOk | Other _ -> Thinking { t with Stats = stats }, flushed

let private ponderingLine (settings: Settings) now (p: Pondering) (line: EngineLine) =
  match line with
  | BestMove _ ->
      // UCI forbids it; the engine is idle now and gets a normal search
      Idle, [ Warn (sprintf "%s sent bestmove while pondering, before ponderhit or stop" settings.Player) ]
  | Info text ->
      let p = match onInfo p.Request.View.ShortPv text p.Stats with Some stats -> { p with Stats = stats } | None -> p
      if now - p.LastStatusAt >= settings.StatusInterval then
        match ponderStatus settings.Player p.Request.WhiteToMove text with
        | Some status -> Pondering { p with LastStatusAt = now }, [ Emit (PonderStatus status) ]
        | None -> Pondering p, []
      else Pondering p, []
  | _ -> Pondering p, []

/// What a drained line or a give-up leads to.
let rec private finishDrain (settings: Settings) now after =
  match after with
  | ThenThink request -> step settings now Idle (Think request)
  | ThenEnd -> Idle, [ ReplyEndGame true ]

and private giveUp (settings: Settings) after reason =
  let warn = Warn (sprintf "%s: %s" settings.Player reason)
  match after with
  | ThenThink _ -> Unresponsive, [ warn; ReplyThink (Stalled reason) ]
  | ThenEnd -> Unresponsive, [ warn; ReplyEndGame false ]

and step (settings: Settings) (now: TimeSpan) (state: State) (event: Event) : State * Effect list =
  match state, event with
  // the output ended: answer whatever is waiting
  | Readying _, OutputClosed
  | Thinking _, OutputClosed
  | Draining (_, ThenThink _, _), OutputClosed -> Closed, [ ReplyThink Crashed ]
  | Draining (_, ThenEnd, _), OutputClosed -> Closed, [ ReplyEndGame true ]
  | _, OutputClosed -> Closed, []

  | Idle, Think request -> beginThink settings now request
  | Pondering p, Think request when request.LastMove = Some p.Request.Move ->
      Thinking { startThinking settings now request with Stats = p.Stats }, [ Send PonderHit ]
  | Pondering _, Think request ->
      // ponder miss: the stopped search's bestmove is the barrier, not readyok
      Draining (ForBestMove, ThenThink request, now), [ Send Stop ]
  | Unresponsive, Think _ -> Unresponsive, [ ReplyThink (Stalled "not answering") ]
  | Closed, Think _ -> Closed, [ ReplyThink Crashed ]
  | _, Think _ -> state, [ Warn (sprintf "%s: think while busy" settings.Player); ReplyThink Interrupted ]

  | Idle, Ponder request ->
      let stats = SearchStats.start settings.Player request.WhiteToMove
      Pondering { Request = request; Stats = stats; LastStatusAt = now }, [ Send (Position request.Position); Send (GoPonder request.Command) ]
  | _, Ponder _ -> state, []

  | Readying (request, since), Line text ->
      match parse text with
      | ReadyOk ->
          Thinking (startThinking settings now request), [ Send (Go request.Search) ]
      | _ -> Readying (request, since), []
  | Thinking t, Line text -> thinkingLine settings now t (parse text)
  | Pondering p, Line text -> ponderingLine settings now p (parse text)
  | Draining (waitFor, after, since), Line text ->
      match waitFor, parse text with
      | ForBestMove, BestMove _
      | ForReadyOk, ReadyOk -> finishDrain settings now after
      | _ -> Draining (waitFor, after, since), []
  | (Idle | Unresponsive | Closed), Line _ -> state, []

  | Readying (request, since), Tick when now - since >= settings.PingTimeout ->
      Unresponsive, [ Warn (sprintf "%s: no readyok within %.0f s" settings.Player settings.PingTimeout.TotalSeconds)
                      ReplyThink (Stalled "no readyok") ]
  | Thinking t, Tick ->
      match t.StopSentAt, t.Request.StopAfter with
      | None, Some limit when now - t.StartedAt >= limit ->
          Thinking { t with StopSentAt = Some now }, [ Send Stop ]
      | Some stoppedAt, _ when now - stoppedAt >= settings.StopWait ->
          Unresponsive, [ Warn (sprintf "%s: no bestmove within %.0f s of stop" settings.Player settings.StopWait.TotalSeconds)
                          ReplyThink (NoBestMove (now - t.StartedAt)) ]
      | _ -> state, []
  | Draining (ForBestMove, after, since), Tick when now - since >= settings.StopWait ->
      giveUp settings after "no bestmove after stop"
  | Draining (ForReadyOk, after, since), Tick when now - since >= settings.PingTimeout ->
      giveUp settings after "no readyok"
  | _, Tick -> state, []

  | Thinking _, EndGame -> Draining (ForBestMove, ThenEnd, now), [ ReplyThink Interrupted; Send Stop ]
  | Pondering _, EndGame -> Draining (ForBestMove, ThenEnd, now), [ Send Stop ]
  | Readying _, EndGame -> Draining (ForReadyOk, ThenEnd, now), [ ReplyThink Interrupted ]
  | Draining (waitFor, ThenThink _, since), EndGame -> Draining (waitFor, ThenEnd, since), [ ReplyThink Interrupted ]
  | Draining (waitFor, _, since), EndGame -> Draining (waitFor, ThenEnd, since), []
  | (Idle | Closed), EndGame -> state, [ ReplyEndGame true ]
  | Unresponsive, EndGame -> state, [ ReplyEndGame false ]

/// When the next Tick is due, if any.
let nextDue (settings: Settings) (state: State) =
  match state with
  | Readying (_, since) -> Some (since + settings.PingTimeout)
  | Thinking t ->
      match t.StopSentAt, t.Request.StopAfter with
      | Some stoppedAt, _ -> Some (stoppedAt + settings.StopWait)
      | None, Some limit -> Some (t.StartedAt + limit)
      | None, None -> None
  | Draining (ForBestMove, _, since) -> Some (since + settings.StopWait)
  | Draining (ForReadyOk, _, since) -> Some (since + settings.PingTimeout)
  | Idle | Pondering _ | Unresponsive | Closed -> None

/// Restarts a search's timing: the go (or ponderhit) has just been written. Writing can take a
/// while (Winboard's pre-go delay), and that is not the engine's time.
let startedAt (now: TimeSpan) (state: State) =
  match state with
  | Thinking t -> Thinking { t with StartedAt = now; LastStatusAt = now }
  | other -> other
