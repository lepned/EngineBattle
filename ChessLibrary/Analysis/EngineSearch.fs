/// One engine searched position by position: the analysis engine the GUI uses
/// (EngineHelper.createAnalysisEngine), started, searched and quit by the console commands and the
/// repertoire build. The limit is the go command ("go nodes 10000").
module ChessLibrary.EngineSearch

open System

type SearchResult =
    { Depth: int
      SelDepth: int
      Eval: MiscTypes.EvalType
      Nodes: int64
      Nps: int64
      Eps: float
      Time: TimeSpan
      TbHits: int64
      Wdl: EngineTypes.WDL option
      Bestmove: string
      /// UCI.
      Pv: string
      /// Numbered SAN ("1.e4 e5 2.Nf3").
      SanPv: string }

/// A running engine and the newest main-line status of its search. With Verbose its info lines are
/// printed as they come.
type Analyzer =
    { Engine: AnalysisEngine
      Last: EngineTypes.EngineStatus option ref
      /// The newest status of each MultiPV line of the search.
      Lines: Collections.Generic.Dictionary<int, EngineTypes.EngineStatus>
      /// The newest move stats of the search (VerboseMoveStats: each move's policy).
      Stats: EngineTypes.NNValues list ref
      Verbose: bool ref }

/// Starts the engine (blocks until it is ready; throws when it cannot start).
let start (config: TypesDef.CoreTypes.EngineConfig) =
    let last = ref None
    let verbose = ref false
    let lines = Collections.Generic.Dictionary<int, EngineTypes.EngineStatus>()
    let stats = ref []
    let onUpdate (u: SearchUpdate) =
        match u.Update with
        | EngineTypes.Status s ->
            lock lines (fun () -> lines.[max 1 s.MultiPV] <- s)
            if s.MultiPV <= 1 then lock last (fun () -> last.Value <- Some s)
        | EngineTypes.Info(_, line) when verbose.Value && not (line.Contains "currmove") -> printfn "%s" line
        | EngineTypes.NNSeq moves ->
            lock stats (fun () -> stats.Value <- List.ofSeq moves)
            if verbose.Value then
                for m in moves do
                    if not (String.IsNullOrEmpty m.Raw) then printfn "%s" m.Raw
        | _ -> ()
    let engine =
        EngineHelper.createAnalysisEngine (onUpdate, config, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, false)
    { Engine = engine; Last = last; Lines = lines; Stats = stats; Verbose = verbose }

let quit (a: Analyzer) =
    try a.Engine.Quit() with _ -> ()

/// One search: ucinewgame, the position (a FEN and UCI moves), the go command. An engine that fails or
/// exits during it ends the search with EngineStartupException instead of leaving the command waiting.
let search (a: Analyzer) (fen: string) (moves: string list) (go: string) (verbose: bool) =
    let board = Chess.Board()
    board.LoadFen fen
    for m in moves do
        board.PlayUciMove m
    let position =
        let movesText = if moves.IsEmpty then "" else " moves " + String.concat " " moves
        if fen = MiscTypes.startPosition then "position startpos" + movesText else $"position fen {fen}{movesText}"
    a.Verbose.Value <- verbose
    lock a.Last (fun () -> a.Last.Value <- None)
    a.Engine.NewGame()
    let sw = Diagnostics.Stopwatch.StartNew()
    let outcome = a.Engine.Search(position, go) |> Async.RunSynchronously
    sw.Stop()
    let bestmove =
        match outcome with
        | Completed(Some info) ->
            if verbose then
                printfn "bestmove %s%s" info.Move (if String.IsNullOrEmpty info.Ponder then "" else " ponder " + info.Ponder)
            info.Move
        | Completed None
        | Superseded -> ""
        | Failed reason ->
            raise (CustomException.EngineStartupException $"engine {a.Engine.Name}: {reason}")
    let last = lock a.Last (fun () -> a.Last.Value)
    let s = defaultArg last EngineTypes.EngineStatus.Empty
    let pv = s.PVLongSAN
    let sanPv =
        if String.IsNullOrWhiteSpace pv then ""
        else
            let buffer = Array.init 256 (fun _ -> Unchecked.defaultof<MoveTypes.TMove>)
            BoardUtils.getShortSanPVFromLongSanPVFast buffer &board pv
    { Depth = s.Depth
      SelDepth = s.SD
      Eval = (if last.IsSome then s.Eval else MiscTypes.EvalType.NA)
      Nodes = s.Nodes
      Nps = int64 s.NPS
      Eps = s.EPS
      Time = sw.Elapsed
      TbHits = s.TBhits
      Wdl = (match s.WDL with EngineTypes.WDLType.HasValue w -> Some w | _ -> None)
      Bestmove = bestmove
      Pv = pv
      SanPv = sanPv }

/// Every line of a MultiPV search from a FEN, best first (UCI moves, evals from White's view); one line
/// for an engine without MultiPV, none when the position has no legal move (no search is started).
let lines (a: Analyzer) (fen: string) (multiPv: int) (go: string) : OpeningRepertoire.Line list =
    let status = (BoardUtils.getPositionStatus fen).Status
    if status = "checkmate" || status = "stalemate" then []
    else
        if a.Engine.GetAllDefaultOptions().Keys |> Seq.exists (fun k -> k.Equals("MultiPV", StringComparison.OrdinalIgnoreCase)) then
            a.Engine.SetOption(EngineTypes.EngineOption.Create "MultiPV" (string multiPv))
        lock a.Lines (fun () -> a.Lines.Clear())
        let r = search a fen [] go false
        let found =
            lock a.Lines (fun () -> a.Lines |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> kv.Value) |> List.ofSeq)
            |> List.truncate multiPv
            |> List.map (fun s -> ({ Pv = s.PVLongSAN.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray; Eval = s.Eval; Policy = None } : OpeningRepertoire.Line))
            |> List.filter (fun l -> not l.Pv.IsEmpty)
        if not found.IsEmpty then found
        elif r.Bestmove <> "" && r.Bestmove <> "(none)" then [ { Pv = [ r.Bestmove ]; Eval = r.Eval; Policy = None } ]
        else []

/// Every legal move after `moves` from a FEN by the net's policy, highest first, each with its share (0..1)
/// and the position's eval: one search (go nodes 1 is enough) whose move stats (VerboseMoveStats) give
/// the policy; none when the position has no legal move. Throws when the engine gives no move stats.
let policyLines (a: Analyzer) (root: string) (moves: string list) (go: string) : OpeningRepertoire.Line list =
    let fen = BoardUtils.fenAfter root moves
    let status = (BoardUtils.getPositionStatus fen).Status
    if status = "checkmate" || status = "stalemate" then []
    else
        lock a.Stats (fun () -> a.Stats.Value <- [])
        let r = search a root moves go false
        let legal =
            lock a.Stats (fun () -> a.Stats.Value)
            |> List.filter (fun m -> not (String.IsNullOrEmpty m.LANMove) && (BoardUtils.tryMakeMove fen m.LANMove).IsSome)
            |> List.distinctBy (fun m -> m.LANMove)
        let total = legal |> List.sumBy (fun m -> m.P)
        if legal.IsEmpty || total <= 0.0 then
            raise (CustomException.EngineStartupException $"engine {a.Engine.Name}: no move stats with a policy (it needs VerboseMoveStats)")
        legal
        |> List.sortByDescending (fun m -> m.P)
        |> List.map (fun m -> ({ Pv = [ m.LANMove ]; Eval = r.Eval; Policy = Some(m.P / total) } : OpeningRepertoire.Line))
