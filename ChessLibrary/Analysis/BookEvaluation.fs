/// Book Evaluation: every engine searches each opening's final position, and the openings whose evals
/// all lie in a size window, with the engines agreeing, are kept. Shared by the GUI page and the
/// `bookeval` verb; the engines are analysis engines (EngineHelper.createAnalysisEngine).
module ChessLibrary.BookEvaluation

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open ChessLibrary.Chess
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TimeControlTypes.TimeControlCommands
open ChessLibrary.EngineTypes
open ChessLibrary.EPDTypes
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes

/// What the filter fields mean: pawns of eval (White's view), or percent of the engine's root WDL.
type FilterUnit =
    | Pawns
    | WinPercent

/// The window (its size: either side may be better) and the largest spread allowed between engines,
/// in centipawns or in percent; in Win % also the largest draw chance, when set.
type Filter =
    { Unit: FilterUnit
      MinEval: float
      MaxEval: float
      MaxDiff: float
      MaxDraw: float option }

/// The filter from the page's text fields (or eb-cli's options) - a comma works as the decimal
/// point. Pawns: empty min is 0, empty max 10, empty diff (or one engine) no limit. Win %: percent,
/// empty min 0, empty max 100, empty draw no cap. A field that does not read raises a FormatException
/// that says which and why.
let parseFilter (unit: FilterUnit) (minEval: string) (maxEval: string) (maxDiff: string) (maxDraw: string) (engineCount: int) =
    let parse (text: string) = Double.TryParse(text.Replace(',', '.'), NumberStyles.Float, CultureInfo.InvariantCulture)
    let pawns what (s: string) dflt =
        if String.IsNullOrWhiteSpace s then dflt
        else
            let text = s.Trim()
            match parse text with
            | true, v when v >= 0.0 && v <= 20.0 -> v * 100.0
            // the fields were in centipawns once
            | true, v when v > 20.0 -> raise (FormatException $"{what} is in pawns: {text} centipawns is %g{v / 100.0}")
            | _ -> raise (FormatException $"{what} takes pawns, e.g. 0.8, not '{text}'")
    let percent what (s: string) dflt =
        if String.IsNullOrWhiteSpace s then dflt
        else
            let text = s.Trim()
            match parse text with
            | true, v when v >= 0.0 && v <= 100.0 -> v
            | _ -> raise (FormatException $"{what} takes a percentage, 0 to 100, not '{text}'")
    // the diff bounds the spread of W - L across engines, which runs to 200
    let spread what (s: string) dflt =
        if String.IsNullOrWhiteSpace s then dflt
        else
            let text = s.Trim()
            match parse text with
            | true, v when v >= 0.0 && v <= 200.0 -> v
            | _ -> raise (FormatException $"{what} takes a percentage, 0 to 200, not '{text}'")
    let filter =
        match unit with
        | Pawns ->
            { Unit = Pawns
              MinEval = pawns "min eval" minEval 0.0
              MaxEval = pawns "max eval" maxEval 1000.0
              MaxDiff = if engineCount <= 1 then 10000.0 else pawns "max eval diff" maxDiff 10000.0
              MaxDraw = None }
        | WinPercent ->
            { Unit = WinPercent
              MinEval = percent "min win" minEval 0.0
              MaxEval = percent "max win" maxEval 100.0
              MaxDiff = if engineCount <= 1 then 1000.0 else spread "max win diff" maxDiff 1000.0
              MaxDraw = if String.IsNullOrWhiteSpace maxDraw then None else Some (percent "max draw" maxDraw 100.0) }
    if filter.MinEval > filter.MaxEval then
        raise (FormatException (match unit with Pawns -> "min eval is above max eval" | WinPercent -> "min win is above max win"))
    filter

/// The spread of the engines' evals, sign included (see bookPositionPasses).
let bookEvalSpread (evals: float[]) =
    if evals.Length = 0 then 0.0 else Array.max evals - Array.min evals

/// Whether a book position passes the filter. The evals are each engine's, from White's view,
/// in centipawns (all engines search the same FEN, so the signs compare). Every eval must lie
/// within [minEval, maxEval] in size - either side may be the one that is better - and the engines
/// must agree: the spread of the evals, sign included, below maxEvalDiff. The spread was taken of
/// the sizes once, so +90 against -90 passed as agreeing although the engines disagree on who is better.
let bookPositionPasses (minEval: float) (maxEval: float) (maxEvalDiff: float) (evals: float[]) =
    let inRange = evals |> Array.forall (fun e -> abs e >= minEval && abs e <= maxEval)
    inRange && bookEvalSpread evals < maxEvalDiff

/// Whether a book position passes in Win %. The WDLs are each engine's (win, draw, loss) in percent,
/// White's view. Every engine's favoured side (the larger of W and L) must lie within [minWin, maxWin],
/// its draw chance at most maxDraw when set, and the engines must agree: the spread of W - L, sign
/// included, below maxDiff - engines that disagree on who is better fail, as in pawns.
let wdlPositionPasses (minWin: float) (maxWin: float) (maxDiff: float) (maxDraw: float option) (wdls: (float * float * float)[]) =
    let inRange =
        wdls
        |> Array.forall (fun (w, d, l) ->
            let favoured = max w l
            favoured >= minWin && favoured <= maxWin && (match maxDraw with Some m -> d <= m | None -> true))
    inRange && bookEvalSpread (wdls |> Array.map (fun (w, _, l) -> w - l)) < maxDiff

/// One engine's answer for a position, White's view: the eval in centipawns and as the engine showed
/// it, the WDL in percent when the engine printed one, and the best move.
type Score =
    { Cp: float
      Eval: EvalType
      Wdl: (float * float * float) option
      Move: string }

/// The number the filter compares, White's view: the eval in centipawns, or the win chance minus the
/// loss chance in percent (its sign says who is better, as the eval's does).
let signedValue (unit: FilterUnit) (score: Score) =
    match unit, score.Wdl with
    | WinPercent, Some (w, _, l) -> w - l
    | _ -> score.Cp

/// Whether the engines' scores pass the filter, in its unit. In Win % an engine without a WDL fails
/// the position (the search reports it before this).
let passes (filter: Filter) (scores: Score[]) =
    match filter.Unit with
    | Pawns -> bookPositionPasses filter.MinEval filter.MaxEval filter.MaxDiff (scores |> Array.map (fun s -> s.Cp))
    | WinPercent ->
        let wdls = scores |> Array.map (fun s -> s.Wdl)
        not (wdls |> Array.exists Option.isNone) && wdlPositionPasses filter.MinEval filter.MaxEval filter.MaxDiff filter.MaxDraw (Array.choose id wdls)

/// A WDL as whole percents that sum to 100: the draw takes the rounding.
let wdlPercents (w: float, _: float, l: float) =
    let w, l = Math.Round w, Math.Round l
    w, 100.0 - w - l, l

/// The score as the output shows it: the eval in pawns or a mate (M1, -M2), or in Win % "W62 D31 L7".
let scoreText (unit: FilterUnit) (score: Score) =
    match unit, score.Wdl with
    | WinPercent, Some wdl -> let w, d, l = wdlPercents wdl in sprintf "W%.0f D%.0f L%.0f" w d l
    | _ -> score.Eval.ToString()

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
    | SearchLimit.DepthLimit d -> sprintf "go depth %d" d

/// Status evals are White's, in pawns; the filter works in centipawns (White's view, as everywhere
/// else in EB), with a mate as 1000 per move (outside any sensible window).
let private whiteCp (eval: EvalType) =
    match eval with
    | CP pawns -> Some (pawns * 100.0)
    | Mate m -> Some (float m * 1000.0)
    | NA -> None

/// The engine's WDL (per mille, the side to move's) as White's, in percent: with Black to move the
/// engine's win is White's loss.
let whiteWdl (blackToMove: bool) (wdl: WDLType) =
    match wdl with
    | WDLType.HasValue v ->
        let w, l = if blackToMove then v.Loss, v.Win else v.Win, v.Loss
        Some (w / 10.0, v.Draw / 10.0, l / 10.0)
    | WDLType.NotFound -> None

/// " wdl 62/31/7" after an eval in the Win % summary.
let private wdlText (unit: FilterUnit) (score: Score) =
    match unit, score.Wdl with
    | WinPercent, Some wdl -> let w, d, l = wdlPercents wdl in sprintf " wdl %.0f/%.0f/%.0f" w d l
    | _ -> ""

/// The spread that ends the summary: in pawns (none across a mate: its 1000 per move is no spread), or in percent.
let private spreadText (unit: FilterUnit) (engineCount: int) (scores: Score[]) (spread: float) =
    if engineCount = 1 then ""
    else
        match unit with
        | WinPercent -> sprintf " max winDiff: %.0f%%" spread
        | Pawns when scores |> Array.exists (fun s -> match s.Eval with Mate _ -> true | _ -> false) -> " max evalDiff: -"
        | Pawns -> sprintf " max evalDiff: %.2f" (spread / 100.0)

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

/// UCI_ShowWDL on, for an engine that has the option (by its option list, not its name); the engine is up.
let private showWdl (s: Searcher) =
    if s.Engine.GetUCICommands().ContainsKey "UCI_ShowWDL" then
        s.Engine.SetOption { Name = "UCI_ShowWDL"; Value = "true" }

/// One search: the engine's score, or why there is none. In Win % an engine that printed no WDL is a failure.
let private search (unit: FilterUnit) (s: Searcher) (fen: string) (blackToMove: bool) = async {
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
            match whiteCp st.Eval, whiteWdl blackToMove st.WDL with
            | None, _ -> Error (sprintf "%s gave no score in %s" s.Engine.Name fen)
            | Some _, None when unit = WinPercent -> Error (sprintf "%s prints no WDL (in %s): filter it in pawns" s.Engine.Name fen)
            | Some cp, wdl -> Ok { Cp = cp; Eval = st.Eval; Wdl = wdl; Move = info.Move }
        | AnalysisOutcome.Completed (Some _), None ->
            Error (sprintf "%s gave no score in %s (search too short?)" s.Engine.Name fen)
}

/// Searches every item's position with every engine (the engines in parallel, within the CPU's
/// threads) and keeps the passing ones. An engine failure ends the run with what passed so far.
let private run (engines: BookEngine list) (filter: Filter) (items: 'a seq) (fenOf: 'a -> string)
                (make: 'a -> float -> string -> float -> string -> string -> string -> 'r)
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
            // the WDL line on, for every engine that has the option
            if filter.Unit = WinPercent then for s in searchers do showWdl s
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
                    let evals =
                        searchers
                        |> Seq.map (fun s -> search filter.Unit s fen (board.Position.STM <> 0uy))
                        |> fun cs -> Async.Parallel(cs, maxDegreeOfParallelism = degree)
                        |> Async.RunSynchronously
                    match evals |> Array.tryPick (function Error e -> Some e | Ok _ -> None) with
                    // a cancelled position's search was cut short (its evals do not count); Ctrl+C
                    // reaches the engines too (they share the console): their exit is the cancel
                    | _ when ct.IsCancellationRequested -> ()
                    | Some e -> failure <- Some e
                    | None ->
                        evaluated <- evaluated + 1
                        let scores = evals |> Array.map (function Ok s -> s | Error _ -> { Cp = 0.0; Eval = NA; Wdl = None; Move = "" })
                        let signed = scores |> Array.map (signedValue filter.Unit)
                        if passes filter scores then
                            // the largest in size, its sign kept: who is better
                            let maxEval, best, by = Array.zip3 signed scores (searchers.ToArray()) |> Array.maxBy (fun (v, _, _) -> abs v)
                            let maxText, maxMove, maxEngine = scoreText filter.Unit best, best.Move, by.Engine.Name
                            let spread = bookEvalSpread signed
                            // the eval as in pawns (a mate as M1 / -M2), with the WDL after it in Win %
                            let summary =
                                "White's view: "
                                + (Array.map2 (fun (sc: Score) (s: Searcher) -> sprintf "%s eval: %s%s (%s), %s" s.Engine.Name (sc.Eval.ToString()) (wdlText filter.Unit sc) s.Limit.Label sc.Move) scores (searchers.ToArray())
                                   |> String.concat ", ")
                                + spreadText filter.Unit engines.Length scores spread
                            results.Add(make item maxEval maxText spread maxMove maxEngine summary, maxEval, spread)
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
    run engines filter epds (fun e -> e.FEN) (fun e mx t d m n s -> EpdEvaluationResult.Create(e, mx, t, d, m, n, s)) progress ct

/// PGN openings by their final position, transpositions dropped first.
let evaluatePgns (engines: BookEngine list) (filter: Filter) (pgns: seq<PgnGame>) progress ct =
    let pgns = Seq.toArray pgns
    let unique, unreadable = onlyUniqueOpenings pgns
    let fenOf (pgn: PgnGame) =
        let board = Board()
        board.LoadFen(if String.IsNullOrWhiteSpace pgn.Fen then Chess.startPos else pgn.Fen)
        for move in DeviationAnalysis.movesFromPgn pgn do board.PlaySanMove move
        board.FEN()
    let outcome = run engines filter unique fenOf (fun p mx t d m n s -> PgnEvaluationResult.Create(p, mx, t, d, m, n, s)) progress ct
    { outcome with Removed = pgns.Length - unique.Count - unreadable; Skipped = outcome.Skipped + unreadable }

/// "name (10,000N), name (500ms), name (20D)" - the engines and limits, for file names and reports.
let engineLabel (engines: BookEngine list) =
    engines
    |> List.map (fun e ->
        match e.Limit with
        | SearchLimit.NodeLimit n -> sprintf "%s (%sN)" e.Config.Name (n.ToString("N0"))
        | SearchLimit.TimeLimit ms -> sprintf "%s (%sms)" e.Config.Name (ms.ToString("N0"))
        | SearchLimit.DepthLimit d -> sprintf "%s (%dD)" e.Config.Name d)
    |> String.concat ", "

/// The run's summary as (label, value) rows, the same for the page's table and the verb's closing
/// block. `read`: the openings read from the book; the filter fields as the user gave them, in the unit.
let summaryRows (source: string) (read: int) (passed: int) (evaluated: int) (removed: int) (skipped: int)
                (failure: string option) (cancelled: bool) (engines: BookEngine list) (unit: FilterUnit)
                (minEval: string) (maxEval: string) (maxDiff: string) (maxDraw: string) (elapsed: TimeSpan) (outPath: string) =
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
      // the diff is checked between engines: none with one
      match unit with
      | Pawns ->
          yield "Eval range", sprintf "%s - %s" minEval maxEval
          if engines.Length > 1 then yield "Max eval diff", sprintf "%s pawns" maxDiff
      | WinPercent ->
          yield "Win chance", sprintf "%s - %s %%" minEval maxEval
          if engines.Length > 1 then yield "Max win diff", sprintf "%s %%" maxDiff
          if not (String.IsNullOrWhiteSpace maxDraw) then yield "Max draw", sprintf "%s %%" maxDraw
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

/// The passing PGN openings as they were read, each with a [MaxEval] tag (pawns or WDL, White's view) after its own tags.
let writePgns (path: string) (results: seq<PgnEvaluationResult>) =
    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath path)) |> ignore
    use w = new StreamWriter(path, false, UTF8Encoding(false))
    for r in results do
        let tag = sprintf "[MaxEval \"%s by %s, %s\"]" r.MaxEvalText r.MaxEngine r.MaxMove
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
