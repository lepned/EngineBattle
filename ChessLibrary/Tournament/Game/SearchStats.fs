/// What one search has reported so far, built from its info lines. Pure.
module ChessLibrary.Game.SearchStats

open System
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineProtocol
open ChessLibrary.Game.EngineLine

type SearchStats =
  { Player: string
    /// Side to move in the searched position; info evals are turned to White's view with it.
    WhiteToMove: bool
    Depth: int
    SelDepth: int
    Nodes: int64
    Nps: float
    Eps: int64
    TbHits: int64
    /// This search's evals, newest first.
    Evals: EvalType list
    Pv: string
    LongPv: string
    Status: EngineStatus
    /// The open LogLiveStats block, newest first.
    NNBlock: NNValues list
    /// n1, n2, q1, q2, p1, pt of the last complete block (Engine.calcTopNn).
    NNTop: (int64 * int64 * float * float * float * float) option }

let start player whiteToMove =
  { Player = player; WhiteToMove = whiteToMove; Depth = 0; SelDepth = 0; Nodes = 0L; Nps = 0.0
    Eps = 0L; TbHits = 0L; Evals = []; Pv = ""; LongPv = ""; Status = EngineStatus.Empty
    NNBlock = []; NNTop = None }

let lastEval (stats: SearchStats) = List.tryHead stats.Evals

/// An info line; None when it carries no score. `toShortPv` turns a UCI PV into SAN.
let onInfo (toShortPv: string -> string) (line: string) (stats: SearchStats) =
  match Regex.getEssentialDataWithEPS line stats.WhiteToMove with
  | None -> None
  | Some (depth, eval, nodes, nps, eps, pvLine, tbHits, wdl, selDepth, multiPv) ->
      // fail-high/low lines carry only the root move
      let pv, longPv =
        if not (String.IsNullOrEmpty pvLine) && not (Regex.isBoundLine line) then toShortPv pvLine, pvLine
        else stats.Pv, stats.LongPv
      let status =
        { PlayerName = stats.Player; Eval = eval; Depth = depth; SD = selDepth; Nodes = nodes
          NPS = float nps; EPS = float eps; TBhits = tbHits
          WDL = (match wdl with Some w -> WDLType.HasValue w | None -> WDLType.NotFound)
          PV = pv; PVLongSAN = longPv; MultiPV = multiPv }
      Some { stats with
               Depth = max stats.Depth depth
               SelDepth = max stats.SelDepth selDepth
               Nodes = nodes; Nps = float nps; Eps = eps; TbHits = tbHits
               Evals = eval :: stats.Evals
               Pv = pv; LongPv = longPv; Status = status }

/// A ponder search's info line as the GUI shows it; None without a score.
let ponderStatus (player: string) (whiteToMove: bool) (line: string) =
  match Regex.getEssentialData line whiteToMove with
  | Some (depth, eval, nodes, nps, _, tbHits, wdl, selDepth, _) ->
      Some { PlayerName = player; Eval = eval; Depth = depth; SD = selDepth; Nodes = nodes
             NPS = float nps; TBhits = tbHits
             WDL = (match wdl with Some w -> WDLType.HasValue w | None -> WDLType.NotFound) }
  | None -> None

/// A LogLiveStats line. Returns the block, in order, when this line closes it.
let onNNLine (line: string) (stats: SearchStats) =
  let value = Regex.getInfoStringData stats.Player line
  if endsNNBlock line then
    // the node line closes a block without joining it, unless it is the whole block
    let block = if stats.NNBlock.IsEmpty then [ value ] else List.rev stats.NNBlock
    { stats with NNBlock = [] }, Some block
  else
    { stats with NNBlock = value :: stats.NNBlock }, None

/// Closes an open block (a line of another kind came first).
let flushNN (stats: SearchStats) =
  if stats.NNBlock.IsEmpty then stats, None
  else { stats with NNBlock = [] }, Some (List.rev stats.NNBlock)

let withNNTop (block: NNValues list) (stats: SearchStats) =
  match Engine.calcTopNn block with
  | Some top -> { stats with NNTop = Some top }
  | None -> stats

/// The PGN annotation fields this search fills (the loop adds time, ponder and pieces).
let toMoveInfo (stats: SearchStats) =
  let info = ChessMoveInfo.Empty
  info.d <- stats.Depth
  info.sd <- stats.SelDepth
  info.wv <- (match lastEval stats with Some e -> e | None -> EvalType.NA)
  info.n <- stats.Nodes
  info.s <- int64 stats.Nps
  info.tb <- stats.TbHits
  info.eps <- stats.Eps
  info.pv <- stats.Pv
  match stats.NNTop with
  | Some (n1, n2, q1, q2, p1, pt) ->
      info.n1 <- n1; info.n2 <- n2; info.q1 <- q1; info.q2 <- q2; info.p1 <- p1; info.pt <- pt
  | None -> ()
  info
