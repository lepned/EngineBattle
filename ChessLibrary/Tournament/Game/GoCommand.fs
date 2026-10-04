/// The go command for a search. Pure.
module ChessLibrary.Game.GoCommand

open System
open ChessLibrary.TimeControlTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.Game.Clock

type Search =
  /// Value head only (ValueTest).
  | Value
  | NodeCount of int
  | MoveTimeMs of int
  | ClockTimes of control: UnionType * white: TimeSpan * black: TimeSpan

/// The search for the side to move. `movesDone`: full moves that side has completed.
let forMove (tests: TestOptions) (isLc0: bool) (timeControl: TimeControl) (config: TimeConfig)
            (movesDone: int) (white: SideClock) (black: SideClock) =
  if tests.ValueTest then (if isLc0 then NodeCount 2 else Value)
  elif tests.PolicyTest then NodeCount 1
  elif config.NodeLimit then NodeCount config.Nodes
  elif config.IsMoveTime then MoveTimeMs (int config.MoveTime.TotalMilliseconds)
  else ClockTimes (timeControl.GetTimeForMove config movesDone, white.Left, black.Left)

let text = function
  | Value -> "go value"
  | NodeCount nodes -> TimeControlCommands.createNodes nodes
  | MoveTimeMs ms -> sprintf "go movetime %d" ms
  | ClockTimes (control, white, black) -> TimeControlCommands.uciTimeCommand control white black

/// The ponder form of a search; only a clock search can ponder.
let ponderText = function
  | ClockTimes _ as search -> Some (text search + " ponder")
  | _ -> None
