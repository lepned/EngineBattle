/// What a line of engine output is, as the game sees it. Pure.
module ChessLibrary.Game.EngineLine

open System

type EngineLine =
  /// `bestmove` with no move gives an empty move (an illegal move to the game).
  | BestMove of move: string * ponder: string option
  | ReadyOk
  /// Lc0/Ceres LogLiveStats (`info string ... N:`); `info string node` ends a block.
  | NNStats of string
  /// Ceres `info engine` messages.
  | EngineMessage of string
  | Info of string
  | Other of string

let private tokens (line: string) =
  line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

let parse (line: string) =
  if line.StartsWith("bestmove", StringComparison.Ordinal) then
    let t = tokens line
    let move = if t.Length > 1 then t.[1] else ""
    let ponder =
      match Array.tryFindIndex ((=) "ponder") t with
      | Some i when i + 1 < t.Length -> Some t.[i + 1]
      | _ -> None
    BestMove (move, ponder)
  elif line.Trim() = "readyok" then ReadyOk
  elif line.StartsWith("info string", StringComparison.Ordinal) && line.Contains "N:" then NNStats line
  elif line.StartsWith("info engine", StringComparison.Ordinal) then EngineMessage line
  elif line.StartsWith("info", StringComparison.Ordinal) then Info line
  else Other line

/// The last line of a LogLiveStats block.
let endsNNBlock (line: string) = line.StartsWith("info string node", StringComparison.Ordinal)
