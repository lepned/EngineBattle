namespace ChessLibrary

open System
open System.IO
open System.Text
open ChessLibrary.PuzzleTypes

/// Breaks a puzzle result down by Lichess theme, so "value is 8 pp worse" becomes
/// "value is 20 pp worse on endgame, level on mateIn2".
///
/// Every puzzle carries a space-separated theme list, and a Score keeps both the solved
/// and the failed puzzles, so the per-theme rate needs no extra measurement.
module PuzzleThemes =

    type ThemeStat =
        { Theme: string
          Total: int
          Correct: int }

    let accuracyOf (s: ThemeStat) =
        if s.Total = 0 then 0.0 else float s.Correct / float s.Total

    /// One theme's rate for two nets measured on the same sample.
    type ThemeDiff =
        { Theme: string
          Total: int
          AccuracyA: float
          AccuracyB: float
          /// B minus A, in percentage points. Positive means B is better on this theme.
          DeltaPp: float }

    let private separators = [| ' '; '\t'; ','; ';' |]

    /// Lichess stores themes as a space-separated list ("advantage long middlegame").
    let parseThemes (themes: string) : string list =
        if String.IsNullOrWhiteSpace themes then []
        else
            themes.Split(separators, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun t -> t.Trim())
            |> Array.filter (fun t -> t.Length > 0)
            |> Array.distinct
            |> Array.toList

    /// Per-theme totals for one result. A puzzle tagged with three themes counts once
    /// under each, so the theme totals deliberately sum to more than the sample size.
    let breakdown (score: Score) : ThemeStat list =
        let totals = Collections.Generic.Dictionary<string, int * int>()
        let bump theme solved =
            let t, c = match totals.TryGetValue theme with | true, v -> v | _ -> (0, 0)
            totals.[theme] <- (t + 1, (if solved then c + 1 else c))
        if not (isNull (box score.CorrectPuzzles)) then
            for p in score.CorrectPuzzles do
                for theme in parseThemes p.Themes do bump theme true
        if not (isNull (box score.FailedPuzzles)) then
            for (p, _) in score.FailedPuzzles do
                for theme in parseThemes p.Themes do bump theme false
        totals
        |> Seq.map (fun kv ->
            let total, correct = kv.Value
            { Theme = kv.Key; Total = total; Correct = correct })
        // theme name breaks ties: Dictionary enumeration order is not guaranteed, so
        // without it two identical runs can emit equal-Total themes in a different order
        |> Seq.sortBy (fun s -> -s.Total, s.Theme)
        |> Seq.toList

    /// Pairs two breakdowns by theme. Themes with fewer than `minPuzzles` occurrences are
    /// dropped and counted — a theme seen five times produces a rate that swings 20 pp on
    /// one puzzle, which would dominate any sort by delta.
    /// Returns (diffs sorted by delta ascending, number of themes dropped).
    let diff (minPuzzles: int) (a: ThemeStat list) (b: ThemeStat list) : ThemeDiff list * int =
        let byThemeB = b |> List.map (fun s -> s.Theme, s) |> dict
        let paired =
            a
            |> List.choose (fun sa ->
                match byThemeB.TryGetValue sa.Theme with
                | true, sb ->
                    // both nets saw the same sample, so the totals agree; keep the smaller
                    // as the effective count in case a run was filtered differently
                    let total = min sa.Total sb.Total
                    Some { Theme = sa.Theme
                           Total = total
                           AccuracyA = accuracyOf sa
                           AccuracyB = accuracyOf sb
                           DeltaPp = (accuracyOf sb - accuracyOf sa) * 100.0 }
                | _ -> None)
        let kept, dropped = paired |> List.partition (fun d -> d.Total >= minPuzzles)
        // theme name breaks delta ties for the same reason
        (kept |> List.sortBy (fun d -> d.DeltaPp, d.Theme)), dropped.Length

    /// Renders one net's weakest and strongest themes. Used when a run has a single net,
    /// where there is no reference to take a delta against but "where is it weak" is still
    /// the question worth answering.
    let renderSingle (label: string) (minPuzzles: int) (topN: int) (stats: ThemeStat list) : string =
        let usable = stats |> List.filter (fun s -> s.Total >= minPuzzles)
        if List.isEmpty usable then "No themes with enough puzzles."
        else
            let sb = StringBuilder()
            let ranked = usable |> List.sortBy (fun s -> accuracyOf s, s.Theme)
            let themeWidth = ranked |> List.map (fun s -> s.Theme.Length) |> List.fold max 5 |> min 28
            let shortLabel (s: string) = if s.Length <= 30 then s else "…" + s.Substring(s.Length - 29)
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "Net = %s" (shortLabel label)) |> ignore
            if ranked.Length > topN * 2 then
                sb.AppendLine(
                    sprintf "Sorted by accuracy: the %d weakest then the %d strongest themes, of %d. All of them are in the CSV."
                        topN topN ranked.Length) |> ignore
            else
                sb.AppendLine(sprintf "Sorted by accuracy, all %d themes." ranked.Length) |> ignore
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "%-*s  %6s  %10s" themeWidth "theme" "n" "accuracy") |> ignore
            sb.AppendLine(String('-', themeWidth + 20)) |> ignore
            let render (s: ThemeStat) =
                sb.AppendLine(
                    sprintf "%-*s  %6d  %9.1f%%"
                        themeWidth
                        (if s.Theme.Length <= themeWidth then s.Theme else s.Theme.Substring(0, themeWidth))
                        s.Total (accuracyOf s * 100.0)) |> ignore
            let weakest = ranked |> List.truncate topN
            weakest |> List.iter render
            if ranked.Length > topN * 2 then
                sb.AppendLine(
                    sprintf "%-*s  %6s  %10s" themeWidth (sprintf "… %d more" (ranked.Length - topN * 2)) "" "") |> ignore
            let alreadyShown = weakest |> List.map (fun s -> s.Theme) |> Set.ofList
            ranked
            |> List.rev
            |> List.truncate topN
            |> List.rev
            |> List.filter (fun s -> not (alreadyShown.Contains s.Theme))
            |> List.iter render
            sb.ToString()

    /// Renders the themes where two nets differ most, worst first for B.
    let renderDiff (labelA: string) (labelB: string) (topN: int) (diffs: ThemeDiff list) : string =
        if List.isEmpty diffs then "No shared themes with enough puzzles."
        else
            let sb = StringBuilder()
            let themeWidth =
                diffs |> List.map (fun d -> d.Theme.Length) |> List.fold max 5 |> min 28
            let shortLabel (s: string) = if s.Length <= 30 then s else "…" + s.Substring(s.Length - 29)
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "A = %s" (shortLabel labelA)) |> ignore
            sb.AppendLine(sprintf "B = %s" (shortLabel labelB)) |> ignore
            // without this a reader cannot tell whether the table is the whole list
            if diffs.Length > topN * 2 then
                sb.AppendLine(
                    sprintf "Sorted by B-A: the %d worst then the %d best for B, of %d themes. All of them are in the CSV."
                        topN topN diffs.Length) |> ignore
            else
                sb.AppendLine(sprintf "Sorted by B-A, all %d themes." diffs.Length) |> ignore
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "%-*s  %6s  %8s  %8s  %8s" themeWidth "theme" "n" "A %" "B %" "B-A pp") |> ignore
            sb.AppendLine(String('-', themeWidth + 36)) |> ignore

            let render (d: ThemeDiff) =
                sb.AppendLine(
                    sprintf "%-*s  %6d  %8.1f  %8.1f  %+8.1f"
                        themeWidth
                        (if d.Theme.Length <= themeWidth then d.Theme else d.Theme.Substring(0, themeWidth))
                        d.Total (d.AccuracyA * 100.0) (d.AccuracyB * 100.0) d.DeltaPp) |> ignore

            let worst = diffs |> List.truncate topN
            let best = diffs |> List.rev |> List.truncate topN |> List.rev
            worst |> List.iter render
            // say how many themes the gap hides rather than leaving a bare ellipsis:
            // it is usually more than half of them, and they are in the CSV
            if diffs.Length > topN * 2 then
                sb.AppendLine(
                    sprintf "%-*s  %6s  %8s  %8s  %8s"
                        themeWidth (sprintf "… %d more" (diffs.Length - topN * 2)) "" "" "" "") |> ignore
            // avoid printing a theme twice when the list is shorter than 2*topN
            let alreadyShown = worst |> List.map (fun d -> d.Theme) |> Set.ofList
            best
            |> List.filter (fun d -> not (alreadyShown.Contains d.Theme))
            |> List.iter render
            sb.ToString()

    // ---------------------------------------------------------------------------
    // Per-run output
    // ---------------------------------------------------------------------------

    /// One threshold for the whole report: the diff, the single-net filter and the rendered
    /// "omitted" count must agree or the output contradicts itself.
    [<Literal>]
    let MinThemePuzzles = 25

    /// Themes shown at each end of the sorted list.
    [<Literal>]
    let TopThemes = 6

    /// Net names come from user-authored engine defs and may contain a comma.
    let private csvField (v: string) =
        let value = if isNull v then "" else v
        if value.IndexOfAny([| ','; '"'; '\n'; '\r' |]) >= 0
        then "\"" + value.Replace("\"", "\"\"") + "\""
        else value

    /// Rating groups are reported as the average rating of the sampled puzzles.
    let private ratingGroupOf (ratingAvg: float) = int (Math.Round(ratingAvg / 100.0)) * 100

    /// Writes `puzzleThemes_<stamp>.csv` and returns the human-readable summary to append to
    /// the run's text summary ("" when there was nothing to report).
    ///
    /// Shared by the console and the GUI: both run the same analysis and produce the same
    /// files. Scores do not arrive in config order, so the engine names are passed in that
    /// order and the FIRST one becomes the baseline every other net is measured against.
    let writeThemeFiles
        (outputFolder: string)
        (stamp: string)
        (engineNamesInConfigOrder: string seq)
        (scores: Score seq)
        : string =
        let summary = StringBuilder()
        let rows = ResizeArray<string>()
        rows.Add("type,rating_group,net_a,net_b,theme,n,accuracy_a_pct,accuracy_b_pct,delta_pp")

        let order =
            engineNamesInConfigOrder
            |> Seq.mapi (fun i name -> (if isNull name then "" else name), i)
            |> Seq.distinctBy fst
            |> dict
        let orderOf (s: Score) =
            match order.TryGetValue(if isNull s.Engine then "" else s.Engine) with
            | true, i -> i
            | _ -> Int32.MaxValue

        let mutable header = false
        let addHeader (title: string) =
            if not header then
                summary.AppendLine() |> ignore
                summary.AppendLine(title) |> ignore
                header <- true

        let groups =
            scores
            |> Seq.groupBy (fun s -> s.Type, ratingGroupOf s.RatingAvg)
            |> Seq.sortBy fst

        for (testType, ratingGroup), group in groups do
            match group |> Seq.sortBy orderOf |> Seq.toList with
            | baseline :: others when not others.IsEmpty ->
                let baseBreak = breakdown baseline
                if not baseBreak.IsEmpty then
                    for other in others do
                        let diffs, dropped = diff MinThemePuzzles baseBreak (breakdown other)
                        if not diffs.IsEmpty then
                            addHeader "--- Per-Theme Comparison (vs first engine) ---"
                            let groupHeader =
                                sprintf "  %s @ rating %d%s" testType ratingGroup
                                    (if dropped > 0 then sprintf "   (%d themes under %d puzzles omitted)" dropped MinThemePuzzles else "")
                            summary.AppendLine() |> ignore
                            summary.AppendLine(groupHeader) |> ignore
                            summary.Append(renderDiff baseline.NeuralNet other.NeuralNet TopThemes diffs) |> ignore
                            for d in diffs do
                                rows.Add(
                                    String.Format(
                                        Globalization.CultureInfo.InvariantCulture,
                                        "{0},{1},{2},{3},{4},{5},{6:F2},{7:F2},{8:F2}",
                                        testType, ratingGroup, csvField baseline.NeuralNet, csvField other.NeuralNet,
                                        csvField d.Theme, d.Total, d.AccuracyA * 100.0, d.AccuracyB * 100.0, d.DeltaPp))
            | [ only ] ->
                // single-net run: no reference to compare against, but the per-theme rates
                // are still the answer to "where is this net weak"
                let stats = breakdown only
                let usable = stats |> List.filter (fun s -> s.Total >= MinThemePuzzles)
                if not usable.IsEmpty then
                    addHeader "--- Per-Theme Breakdown ---"
                    let groupHeader =
                        sprintf "  %s @ rating %d   (%d themes under %d puzzles omitted)"
                            testType ratingGroup (stats.Length - usable.Length) MinThemePuzzles
                    summary.AppendLine() |> ignore
                    summary.AppendLine(groupHeader) |> ignore
                    summary.Append(renderSingle only.NeuralNet MinThemePuzzles TopThemes stats) |> ignore
                    for s in usable do
                        // no reference net, so net_a and the A/delta columns stay empty
                        rows.Add(
                            String.Format(
                                Globalization.CultureInfo.InvariantCulture,
                                "{0},{1},,{2},{3},{4},,{5:F2},",
                                testType, ratingGroup, csvField only.NeuralNet,
                                csvField s.Theme, s.Total, accuracyOf s * 100.0))
            | _ -> ()

        if rows.Count > 1 then
            let csvPath = Path.Combine(outputFolder, sprintf "puzzleThemes_%s.csv" stamp)
            File.WriteAllLines(csvPath, rows)
            summary.AppendLine() |> ignore
            summary.AppendLine(sprintf "  All themes: %s" csvPath) |> ignore
            summary.ToString()
        else ""
