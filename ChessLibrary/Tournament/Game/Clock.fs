/// A side's clock: what it has left, what a move does to it, when a search is stopped. Pure.
module ChessLibrary.Game.Clock

open System
open ChessLibrary.TimeControlTypes

/// Least margin on a time per move: engines answer 1-3 ms after their movetime.
let minMoveTimeMargin = TimeSpan.FromMilliseconds 50.0

/// Added to a move's time before `stop` is sent (cutechess: 200 ms).
let stopGrace = TimeSpan.FromMilliseconds 200.0

type Limit =
  | Nodes of int
  /// Time per move (st): never charged.
  | MoveTime of TimeSpan
  /// Base time, increment, moves per repeating period (0 = none).
  | Running of baseTime: TimeSpan * increment: TimeSpan * period: int

type SideClock =
  { Limit: Limit
    /// Running: time left. MoveTime: move time + margin, shown and sent as the clock.
    Left: TimeSpan
    /// How far past zero a move may go.
    Margin: TimeSpan }

type MoveTiming =
  | OnTime of SideClock
  /// Negative: its size is the overrun reported in the result.
  | LostOnTime of remaining: TimeSpan

let limitOf (timeControl: TimeControl) (config: TimeConfig) =
  if config.NodeLimit then Nodes config.Nodes
  elif config.IsMoveTime then MoveTime config.MoveTime
  else Running (config.Fixed, config.Increment, timeControl.PeriodFor config)

let create (timeControl: TimeControl) (config: TimeConfig) (moveOverhead: TimeSpan) =
  let limit = limitOf timeControl config
  let margin =
    match limit with
    | MoveTime _ -> max moveOverhead minMoveTimeMargin
    | _ -> moveOverhead
  let left =
    match limit with
    | MoveTime moveTime -> moveTime + margin
    | _ -> config.Fixed
  { Limit = limit; Left = left; Margin = margin }

/// Time left after a move, unclamped. A time per move counts whole milliseconds, as fastchess does.
let remainingAfter (clock: SideClock) (elapsed: TimeSpan) =
  match clock.Limit with
  | Nodes _ -> clock.Left
  | MoveTime moveTime -> moveTime - TimeSpan.FromMilliseconds(Math.Floor elapsed.TotalMilliseconds)
  | Running (_, increment, _) -> clock.Left + increment - elapsed

/// The clock after a move. `moveNumber`: full-move number of the move (before it is made).
let afterMove (clock: SideClock) (elapsed: TimeSpan) (moveNumber: int) : MoveTiming =
  match clock.Limit with
  | Nodes _ -> OnTime clock
  | MoveTime _ ->
      let remaining = remainingAfter clock elapsed
      if remaining + clock.Margin < TimeSpan.Zero then LostOnTime remaining else OnTime clock
  | Running (baseTime, _, period) ->
      let remaining = remainingAfter clock elapsed
      if remaining + clock.Margin < TimeSpan.Zero then LostOnTime remaining
      else
        let left = max remaining TimeSpan.Zero
        // a completed period earns the base time again
        let left = if period > 0 && moveNumber % period = 0 then left + baseTime else left
        OnTime { clock with Left = left }

/// How long after its go a search runs before `stop`. None for a node limit.
let stopAfter (clock: SideClock) =
  match clock.Limit with
  | Nodes _ -> None
  | MoveTime moveTime -> Some (moveTime + clock.Margin + stopGrace)
  | Running (_, increment, _) -> Some (clock.Left + increment + clock.Margin + stopGrace)
