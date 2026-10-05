/// Book Evaluation: every engine searches each opening's final position, and the openings whose evals
/// all lie in a size window, with the engines agreeing, are kept. Shared by the GUI page and the
/// `bookeval` verb; the engines are analysis engines (EngineHelper.createAnalysisEngine).
module ChessLibrary.BookEvaluation

open System
open System.Collections.Generic
open System.IO
open System.Text
open ChessLibrary.Chess
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TimeControlTypes.TimeControlCommands
open ChessLibrary.EngineTypes
open ChessLibrary.EPDTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes

/// Eval window in centipawns from the side to move, and the largest spread allowed between engines.
type Filter =
    { MinEval: float
      MaxEval: float
      MaxDiff: float }

/// The page's text fields: empty min is 0, empty max 1000, empty diff (or one engine) no limit.
let parseFilter (minEval: string) (maxEval: string) (maxDiff: string) (engineCount: int) =
    let num (s: string) dflt = if String.IsNullOrWhiteSpace s then dflt else float (s.Trim())
    { MinEval = num minEval 0.0
      MaxEval = num maxEval 1000.0
      MaxDiff = if engineCount <= 1 then 10000.0 else num maxDiff 10000.0 }

/// The spread of the engines' evals, sign included (see bookPositionPasses).
let bookEvalSpread (evals: float[]) =
    if evals.Length = 0 then 0.0 else Array.max evals - Array.min evals

/// Whether a book position passes the filter. The evals are each engine's, from the side to move,
/// in centipawns (all engines search the same FEN, so the signs compare). Every eval must lie
/// within [minEval, maxEval] in size - either side may be the one that is better - and the engines
/// must agree: the spread of the evals, sign included, below maxEvalDiff. The spread was taken of
/// the sizes once, so +90 against -90 passed as agreeing although the engines disagree on who is better.
let bookPositionPasses (minEval: float) (maxEval: float) (maxEvalDiff: float) (evals: float[]) =
    let inRange = evals |> Array.forall (fun e -> abs e >= minEval && abs e <= maxEval)
    inRange && bookEvalSpread evals < maxEvalDiff

/// One engine of the run and how long it searches each position.
type BookEngine =
    { Config: EngineConfig
      Limit: SearchLimit }

/// What a run gives: the passing positions (most disputed first), how many were searched, how many
/// duplicates were dropped first, how many could not be read, and why it stopped early, if it did.
type Outcome<'T> =
    { Results: ResizeArray<'T>
      Evaluated: int
      Removed: int
      Skipped: int
      Failure: string option
      Cancelled: bool }

/// Openings that end in a position already seen (a transposition) are dropped; so are openings that
/// do not replay, counted apart.
let onlyUniqueOpenings (pgns: seq<PgnGame>) =
    let unique = ResizeArray<PgnGame>()
    let board = Board()
    let seen = HashSet<uint64>()
    let mutable unreadable = 0
    for pgn in pgns do
        try
            board.ResetBoardState()
            if not (String.IsNullOrWhiteSpace pgn.Fen) then board.LoadFen pgn.Fen
            // an illegal move is skipped by the board, not thrown: such an opening ends elsewhere
            for move in pgn.Mainline do
                if not (board.PlaySanMoveWithComments move.San "") then failwithf "illegal move %s" move.San
            if seen.Add(board.DeviationHash()) then unique.Add pgn
        with _ -> unreadable <- unreadable + 1
    unique, unreadable

let private goCommand (limit: SearchLimit) =
    match limit with
    | SearchLimit.NodeLimit n -> sprintf "go nodes %d" n
    | SearchLimit.TimeLimit ms -> sprintf "go movetime %d" ms

/// Status evals are White's, in pawns; the filter works from the side to move, in centipawns,
/// with a mate as 1000 per move (outside any sensible window).
let private sideToMoveCp (whiteToMove: bool) (eval: EvalType) =
    let sign = if whiteToMove then 1.0 else -1.0
    match eval with
    | CP pawns -> Some (sign * pawns * 100.0)
    | Mate m -> Some (sign * float m * 1000.0)
    | NA -> None

/// A running engine and the newest main-line status of its search.
type private Searcher =
    { Engine: AnalysisEngine
      Last: EngineStatus option ref
      Limit: SearchLimit }

let private start (e: BookEngine) =
    let last = ref None
    let onUpdate (u: SearchUpdate) =
        match u.Update with
        | Status s when s.MultiPV <= 1 -> lock last (fun () -> last.Value <- Some s)
        | _ -> ()
    let engine = EngineHelper.createAnalysisEngine (onUpdate, e.Config, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, false)
    // a note per ucinewgame (Stockfish's NNUE lines) would drown the progress
    engine.EchoEngineText <- false
    { Engine = engine; Last = last; Limit = e.Limit }

/// One search: (eval cp from the side to move, best move), or why there is none.
let private search (s: Searcher) (fen: string) (whiteToMove: bool) = async {
    lock s.Last (fun () -> s.Last.Value <- None)
    s.Engine.NewGame()
    let! outcome = s.Engine.Search(sprintf "position fen %s" (Board.UciFen fen), goCommand s.Limit)
    let last = lock s.Last (fun () -> s.Last.Value)
    return
        match outcome, last with
        | AnalysisOutcome.Failed reason, _ -> Error (sprintf "%s failed: %s" s.Engine.Name reason)
        | AnalysisOutcome.Superseded, _ -> Error (sprintf "%s: search superseded" s.Engine.Name)
        | AnalysisOutcome.Completed None, _ -> Error (sprintf "%s found no move in %s" s.Engine.Name fen)
        | AnalysisOutcome.Completed (Some info), Some st ->
            match sideToMoveCp whiteToMove st.Eval with
            | Some cp -> Ok (cp, info.Move)
            | None -> Error (sprintf "%s gave no score in %s" s.Engine.Name fen)
        | AnalysisOutcome.Completed (Some _), None ->
            Error (sprintf "%s gave no score in %s (search too short?)" s.Engine.Name fen)
}

/// Searches every item's position with every engine (the engines in parallel, within the CPU's
/// threads) and keeps the passing ones. An engine failure ends the run with what passed so far.
let private run (engines: BookEngine list) (filter: Filter) (items: 'a seq) (fenOf: 'a -> string)
                (make: 'a -> float -> float -> string -> string -> string -> 'r)
                (progress: Action<int, int>) (ct: Threading.CancellationToken) : Outcome<'r> =
    let items = Seq.toArray items
    let results = ResizeArray<'r * float * float>()
    let searchers = ResizeArray<Searcher>()
    let mutable failure = None
    let mutable evaluated = 0
    let mutable skipped = 0
    try
        try
            for e in engines do searchers.Add(start e)
            // a cancel stops the searches at once instead of waiting for their limit
            use _ = ct.Register(fun () -> for s in searchers do try s.Engine.Stop() with _ -> ())
            let threads = engines |> List.sumBy (fun e -> max 1 (HardwareInfo.getThreads e.Config))
            let degree = max 1 (min engines.Length ((Environment.ProcessorCount - 1) / max 1 threads))
            let board = Board()
            let mutable i = 0
            while i < items.Length && failure.IsNone && not ct.IsCancellationRequested do
                let item = items.[i]
                // a position that does not read is the book's fault, not the engines': skipped
                match (try let fen = fenOf item in board.LoadFen fen; Some fen with _ -> None) with
                | None -> skipped <- skipped + 1
                | Some fen ->
                    let whiteToMove = board.FEN().Split(' ').[1] = "w"
                    let evals =
                        searchers
                        |> Seq.map (fun s -> search s fen whiteToMove)
                        |> fun cs -> Async.Parallel(cs, maxDegreeOfParallelism = degree)
                        |> Async.RunSynchronously
                    match evals |> Array.tryPick (function Error e -> Some e | Ok _ -> None) with
                    // a cancelled position's search was cut short (its evals do not count); Ctrl+C
                    // reaches the engines too (they share the console): their exit is the cancel
                    | _ when ct.IsCancellationRequested -> ()
                    | Some e -> failure <- Some e
                    | None ->
                        evaluated <- evaluated + 1
                        let found = Array.map2 (fun r (s: Searcher) -> match r with Ok (cp, m) -> cp, m, s | Error _ -> 0.0, "", s) evals (searchers.ToArray())
                        let signed = found |> Array.map (fun (cp, _, _) -> cp)
                        if bookPositionPasses filter.MinEval filter.MaxEval filter.MaxDiff signed then
                            let maxEval, maxMove, maxEngine =
                                found |> Array.map (fun (cp, m, s) -> abs cp, m, s.Engine.Name) |> Array.max
                            let spread = bookEvalSpread signed
                            let summary =
                                (found
                                 |> Array.map (fun (cp, m, s) -> sprintf "%s eval: %.0f (%s), %s" s.Engine.Name cp s.Limit.Label m)
                                 |> String.concat ", ")
                                + (if engines.Length = 1 then "" else sprintf " max evalDiff: %.1f" spread)
                            results.Add(make item maxEval spread maxMove maxEngine summary, maxEval, spread)
                if progress <> null then progress.Invoke(i + 1, items.Length)
                i <- i + 1
        with ex -> if not ct.IsCancellationRequested then failure <- Some ex.Message
    finally
        for s in searchers do
            try s.Engine.Quit() with _ -> ()
    // the most disputed first with several engines, the largest eval first with one
    let sorted =
        results
        |> Seq.sortByDescending (fun (_, maxEval, spread) -> if engines.Length > 1 then abs spread else abs maxEval)
        |> Seq.map (fun (r, _, _) -> r)
        |> ResizeArray
    { Results = sorted; Evaluated = evaluated; Removed = 0; Skipped = skipped; Failure = failure; Cancelled = ct.IsCancellationRequested }

/// EPD positions as they are.
let evaluateEpds (engines: BookEngine list) (filter: Filter) (epds: seq<EPDEntry>) progress ct =
    run engines filter epds (fun e -> e.FEN) (fun e mx d m n s -> EpdEvaluationResult.Create(e, mx, d, m, n, s)) progress ct

/// PGN openings by their final position, transpositions dropped first.
let evaluatePgns (engines: BookEngine list) (filter: Filter) (pgns: seq<PgnGame>) progress ct =
    let pgns = Seq.toArray pgns
    let unique, unreadable = onlyUniqueOpenings pgns
    let fenOf (pgn: PgnGame) =
        let board = Board()
        board.LoadFen(if String.IsNullOrWhiteSpace pgn.Fen then Chess.startPos else pgn.Fen)
        for move in DeviationAnalysis.movesFromPgn pgn do board.PlaySanMove move
        board.FEN()
    let outcome = run engines filter unique fenOf (fun p mx d m n s -> PgnEvaluationResult.Create(p, mx, d, m, n, s)) progress ct
    { outcome with Removed = pgns.Length - unique.Count - unreadable; Skipped = outcome.Skipped + unreadable }

/// "name (10,000N), name (500ms)" - the engines and limits, for file names and reports.
let engineLabel (engines: BookEngine list) =
    engines
    |> List.map (fun e ->
        match e.Limit with
        | SearchLimit.NodeLimit n -> sprintf "%s (%sN)" e.Config.Name (n.ToString("N0"))
        | SearchLimit.TimeLimit ms -> sprintf "%s (%sms)" e.Config.Name (ms.ToString("N0")))
    |> String.concat ", "

/// The run's summary as (label, value) rows, the same for the page's table and the verb's closing
/// block. `read`: the openings read from the book; the eval fields as the user gave them.
let summaryRows (source: string) (read: int) (passed: int) (evaluated: int) (removed: int) (skipped: int)
                (failure: string option) (cancelled: bool) (engines: BookEngine list)
                (minEval: string) (maxEval: string) (maxDiff: string) (elapsed: TimeSpan) (outPath: string) =
    let n (x: int) = x.ToString("N0")
    [ yield "Source file", source
      yield "Positions analyzed", n read
      if removed > 0 then
          yield "Transposed / duplicate removed", n removed
          yield "Unique openings evaluated", n evaluated
      if skipped > 0 then yield "Unreadable openings skipped", n skipped
      yield "Positions passed", n passed
      yield "Pass rate",
            (if evaluated > 0 then (float passed / float evaluated).ToString("P1") else "N/A")
            + (if removed > 0 then " of unique" else "")
      yield "Engines", engineLabel engines
      yield "Eval range", sprintf "%s - %s" minEval maxEval
      yield "Max eval diff", sprintf "%s cp" maxDiff
      yield "Duration", sprintf "%dh %dm %ds" (int elapsed.TotalHours) elapsed.Minutes elapsed.Seconds
      yield "Output file", outPath
      match failure with
      | Some reason -> yield "Status", sprintf "Stopped: %s (what passed before is saved)" reason
      | None -> if cancelled then yield "Status", "Cancelled (partial results saved)" ]

/// The passing EPD positions, each with the engines' evals in an `other` op.
let writeEpds (path: string) (results: seq<EpdEvaluationResult>) =
    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path)) |> ignore
    use w = new StreamWriter(path, false, UTF8Encoding(false))
    for r in results do
        let id =
            match r.EPD.Id with
            | Some id when id <> r.EPD.FEN -> id
            | _ -> "opening analysis by EB"
        w.WriteLine(sprintf "%s; id \"%s\"; other \"%s\";" r.EPD.FEN id r.Summary)

let private tagPair = Text.RegularExpressions.Regex(@"^\[[A-Za-z0-9_]+\s+""(?:[^""\\]|\\.)*""\]$")

/// The passing PGN openings as they were read, each with a [MaxEval] tag after its own tags.
let writePgns (path: string) (results: seq<PgnEvaluationResult>) =
    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path)) |> ignore
    use w = new StreamWriter(path, false, UTF8Encoding(false))
    for r in results do
        let tag = sprintf "[MaxEval \"%g by %s, %s\"]" r.MaxEval r.MaxEngine r.MaxMove
        let lines = (if isNull r.Pgn.Raw then "" else r.Pgn.Raw).Replace("\r\n", "\n").Split('\n') |> Array.map (fun l -> l.TrimEnd())
        // the tags are the leading [Name "value"] lines; from the first other line on it is movetext,
        // kept as it was - a wrapped comment can start a line with [%clk ...] too
        let tagCount =
            lines
            |> Array.tryFindIndex (fun l -> let t = l.Trim() in t <> "" && not (tagPair.IsMatch t))
            |> Option.defaultValue lines.Length
        // [Event] leads (a stable sort keeps the rest in order); a book evaluated before brings its
        // old [MaxEval], which this run's replaces
        let headers =
            lines.[.. tagCount - 1]
            |> Array.map (fun l -> l.Trim())
            |> Array.filter (fun t -> t <> "" && not (t.StartsWith("[MaxEval ", StringComparison.OrdinalIgnoreCase)))
            |> Array.sortBy (fun t -> if t.StartsWith("[Event ", StringComparison.OrdinalIgnoreCase) then 0 else 1)
        // line breaks kept: a ';' comment runs to the end of its line
        let moves = (String.concat "\n" lines.[tagCount ..]).Trim()
        if headers.Length > 0 then w.WriteLine(String.concat "\n" headers)
        w.WriteLine tag
        w.WriteLine()
        w.WriteLine moves
        w.WriteLine()
