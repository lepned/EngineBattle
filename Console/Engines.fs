/// The engine the console commands search with: the analysis engine the GUI uses
/// (EngineHelper.createAnalysisEngine), one search per position.
module Engines

open System
open Cli.Groups

type SearchResult =
    { Depth: int
      SelDepth: int
      Eval: ChessLibrary.MiscTypes.EvalType
      Nodes: int64
      Nps: int64
      Eps: float
      Time: TimeSpan
      TbHits: int64
      Wdl: ChessLibrary.EngineTypes.WDL option
      Bestmove: string
      /// UCI.
      Pv: string
      /// Numbered SAN ("1.e4 e5 2.Nf3").
      SanPv: string }

/// A running engine and the newest main-line status of its search. With Verbose its info lines are
/// printed as they come.
type Analyzer =
    { Engine: ChessLibrary.AnalysisEngine
      Last: ChessLibrary.EngineTypes.EngineStatus option ref
      Verbose: bool ref }

/// Starts the engine (blocks until it is ready; throws when it cannot start).
let start (config: ChessLibrary.TypesDef.CoreTypes.EngineConfig) =
    let last = ref None
    let verbose = ref false
    let onUpdate (u: ChessLibrary.SearchUpdate) =
        match u.Update with
        | ChessLibrary.EngineTypes.Status s when s.MultiPV <= 1 -> lock last (fun () -> last.Value <- Some s)
        | ChessLibrary.EngineTypes.Info(_, line) when verbose.Value && not (line.Contains "currmove") -> printfn "%s" line
        | ChessLibrary.EngineTypes.NNSeq moves when verbose.Value ->
            for m in moves do
                if not (String.IsNullOrEmpty m.Raw) then printfn "%s" m.Raw
        | _ -> ()
    let engine =
        ChessLibrary.EngineHelper.createAnalysisEngine (onUpdate, config, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, false)
    { Engine = engine; Last = last; Verbose = verbose }

let quit (a: Analyzer) =
    try a.Engine.Quit() with _ -> ()

/// One search: ucinewgame, the position (a FEN and UCI moves), the limit. An engine that fails or
/// exits during it ends the search with EngineStartupException instead of leaving the command waiting.
let search (a: Analyzer) (fen: string) (moves: string list) (limit: Limit) (verbose: bool) =
    let board = ChessLibrary.Chess.Board()
    board.LoadFen fen
    for m in moves do
        board.PlayUciMove m
    let position =
        let movesText = if moves.IsEmpty then "" else " moves " + String.concat " " moves
        if fen = startFen then "position startpos" + movesText else $"position fen {fen}{movesText}"
    a.Verbose.Value <- verbose
    lock a.Last (fun () -> a.Last.Value <- None)
    a.Engine.NewGame()
    let sw = Diagnostics.Stopwatch.StartNew()
    let outcome = a.Engine.Search(position, limit.Go) |> Async.RunSynchronously
    sw.Stop()
    let bestmove =
        match outcome with
        | ChessLibrary.Completed(Some info) ->
            if verbose then
                printfn "bestmove %s%s" info.Move (if String.IsNullOrEmpty info.Ponder then "" else " ponder " + info.Ponder)
            info.Move
        | ChessLibrary.Completed None
        | ChessLibrary.Superseded -> ""
        | ChessLibrary.Failed reason ->
            raise (ChessLibrary.CustomException.EngineStartupException $"engine {a.Engine.Name}: {reason}")
    let last = lock a.Last (fun () -> a.Last.Value)
    let s = defaultArg last ChessLibrary.EngineTypes.EngineStatus.Empty
    let pv = s.PVLongSAN
    let sanPv =
        if String.IsNullOrWhiteSpace pv then ""
        else
            let buffer = Array.init 256 (fun _ -> Unchecked.defaultof<ChessLibrary.MoveTypes.TMove>)
            ChessLibrary.BoardUtils.getShortSanPVFromLongSanPVFast buffer &board pv
    { Depth = s.Depth
      SelDepth = s.SD
      Eval = (if last.IsSome then s.Eval else ChessLibrary.MiscTypes.EvalType.NA)
      Nodes = s.Nodes
      Nps = int64 s.NPS
      Eps = s.EPS
      Time = sw.Elapsed
      TbHits = s.TBhits
      Wdl = (match s.WDL with ChessLibrary.EngineTypes.WDLType.HasValue w -> Some w | _ -> None)
      Bestmove = bestmove
      Pv = pv
      SanPv = sanPv }

/// An eval for JSON, from White's view: pawns (0.35) or mate in N, never both; nothing when the engine gave none.
type EvalJson =
    { Pawns: Nullable<float>
      Mate: Nullable<int> }

let evalJson (e: ChessLibrary.MiscTypes.EvalType) =
    match e with
    | ChessLibrary.MiscTypes.EvalType.CP cp -> { Pawns = Nullable cp; Mate = Nullable() }
    | ChessLibrary.MiscTypes.EvalType.Mate m -> { Pawns = Nullable(); Mate = Nullable m }
    | _ -> { Pawns = Nullable(); Mate = Nullable() }

/// Pawns (White's view) to compare two evals by (mate is ±999.99), or None.
let pawnsOf (e: ChessLibrary.MiscTypes.EvalType) =
    match e with
    | ChessLibrary.MiscTypes.EvalType.CP cp -> Some cp
    | ChessLibrary.MiscTypes.EvalType.Mate m -> Some(float (sign m) * 999.99)
    | _ -> None
