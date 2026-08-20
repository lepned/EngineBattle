namespace ChessLibrary

open System
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
        |> Seq.sortByDescending (fun s -> s.Total)
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
        (kept |> List.sortBy (fun d -> d.DeltaPp)), dropped.Length

    /// Renders one net's weakest and strongest themes. Used when a run has a single net,
    /// where there is no reference to take a delta against but "where is it weak" is still
    /// the question worth answering.
    let renderSingle (label: string) (minPuzzles: int) (topN: int) (stats: ThemeStat list) : string =
        let usable = stats |> List.filter (fun s -> s.Total >= minPuzzles)
        if List.isEmpty usable then "No themes with enough puzzles."
        else
            let sb = StringBuilder()
            let ranked = usable |> List.sortBy accuracyOf
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
