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

    /// How many standard errors a delta is from zero, for one theme.
    ///
    /// Deliberately the *unpaired* approximation at p = 0.5, which is the widest SE the
    /// data can produce. Both nets saw the same puzzles, so the true paired test (McNemar)
    /// would be tighter - but it needs the discordant counts, which a per-theme correct/total
    /// pair does not carry. Erring wide is the safe direction for a screening number.
    let sigmaOf (total: int) (deltaPp: float) =
        if total <= 0 then 0.0
        else abs deltaPp / (sqrt (2.0 * 0.25 / float total) * 100.0)

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
    ///
    /// Scored on the puzzle's FIRST solver move, not on whether the whole line was played.
    /// A Lichess puzzle's tags describe the move it exists for; the moves after it are
    /// usually forced follow-up and carry the same tags without being about them. Measured
    /// on a real 1500-puzzle sample: 38-43% of FAILED puzzles had the thematic move right
    /// and went wrong later, so the old whole-line scoring charged nearly four in ten
    /// failures to a theme that had nothing to do with them.
    ///
    /// Falls back to the whole-line verdict when a test does not track first moves, which
    /// is declared by `FirstMoveScored = 0` - NOT by the id set being empty. An empty id
    /// set is a legitimate result (nothing was first-move-correct) and must not be read as
    /// "this runner has no first-move data". Tests without tracking are unchanged, not
    /// silently wrong.
    let breakdown (score: Score) : ThemeStat list =
        let totals = Collections.Generic.Dictionary<string, int * int>()
        let bump theme solved =
            let t, c = match totals.TryGetValue theme with | true, v -> v | _ -> (0, 0)
            totals.[theme] <- (t + 1, (if solved then c + 1 else c))
        let ids = score.FirstMoveCorrectIds
        // CAPABILITY, not a data value: `ids.Count > 0` would conflate "this test does not
        // track first moves" with "nothing was first-move-correct", and would silently
        // revert to whole-line scoring for a Score whose id set was dropped in transit.
        // FirstMoveScored states the intended condition directly.
        let tracksFirstMove = score.FirstMoveScored > 0 && not (isNull (box ids))
        if not (isNull (box score.CorrectPuzzles)) then
            for p in score.CorrectPuzzles do
                // a solved puzzle necessarily had its first move right
                for theme in parseThemes p.Themes do bump theme true
        if not (isNull (box score.FailedPuzzles)) then
            for (p, _) in score.FailedPuzzles do
                let firstOk = tracksFirstMove && ids.Contains p.PuzzleId
                for theme in parseThemes p.Themes do bump theme firstOk
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
    /// `label` is a bare net name here, so shortening from the left is right: what
    /// distinguishes two checkpoints is the step number at the end.
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
    ///
    /// Takes the net and engine separately rather than a composed label: the shortening
    /// below keeps the END of what it is given, which silently ate the net name out of a
    /// composed "net (engine)" string and left both sides looking identical.
    let renderDiffFor (netA: string) (netB: string) (engineA: string) (engineB: string)
                      (topN: int) (diffs: ThemeDiff list) : string =
        if List.isEmpty diffs then "No shared themes with enough puzzles."
        else
            let sb = StringBuilder()
            let themeWidth =
                diffs |> List.map (fun d -> d.Theme.Length) |> List.fold max 5 |> min 28
            // nets differ in their trailing step number, engines in their leading word,
            // so each is shortened from the end that carries no information
            let labelA, labelB = PuzzlePaired.fittedSideNames netA netB engineA engineB
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "A = %s" labelA) |> ignore
            sb.AppendLine(sprintf "B = %s" labelB) |> ignore
            // without this a reader cannot tell whether the table is the whole list
            if diffs.Length > topN * 2 then
                sb.AppendLine(
                    sprintf "Sorted by B-A: the %d worst then the %d best for B, of %d themes. All of them are in the CSV."
                        topN topN diffs.Length) |> ignore
            else
                sb.AppendLine(sprintf "Sorted by B-A, all %d themes." diffs.Length) |> ignore
            sb.AppendLine() |> ignore
            sb.AppendLine(sprintf "%-*s  %6s  %8s  %8s  %8s  %6s" themeWidth "theme" "n" "A %" "B %" "B-A pp" "sigma") |> ignore
            sb.AppendLine(String('-', themeWidth + 44)) |> ignore

            let render (d: ThemeDiff) =
                sb.AppendLine(
                    sprintf "%-*s  %6d  %8.1f  %8.1f  %+8.1f  %6.1f"
                        themeWidth
                        (if d.Theme.Length <= themeWidth then d.Theme else d.Theme.Substring(0, themeWidth))
                        d.Total (d.AccuracyA * 100.0) (d.AccuracyB * 100.0) d.DeltaPp
                        (sigmaOf d.Total d.DeltaPp)) |> ignore

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

            // Sorting by delta puts the smallest samples at both ends, so the most
            // reliable rows are exactly the ones the elision hides. Always show them.
            let shownNow =
                Set.union alreadyShown (best |> List.map (fun d -> d.Theme) |> Set.ofList)
            let biggest =
                diffs
                |> List.sortByDescending (fun d -> d.Total)
                |> List.filter (fun d -> not (shownNow.Contains d.Theme))
                |> List.truncate 3
            if not biggest.IsEmpty then
                sb.AppendLine() |> ignore
                sb.AppendLine("largest samples:") |> ignore
                biggest |> List.iter render
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

    /// What a run produced on the theme side.
    type ThemeOutput =
        { /// Full tables, appended to the run's text summary.
          Summary: string
          /// Where every theme landed; "" when nothing was written.
          CsvPath: string
          /// One line worth putting on the console: the strongest difference found, so a
          /// reader knows whether the file is worth opening. "" when nothing cleared 2 sigma.
          Headline: string }

    let private emptyThemeOutput = { Summary = ""; CsvPath = ""; Headline = "" }

    /// Which rule the accuracy columns were computed with. Written into the CSV so a
    /// consumer can tell the two apart - they are otherwise indistinguishable and differ
    /// by 10-25 pp on multi-move themes.
    let private scoringRule (score: Score) =
        if score.FirstMoveScored > 0 then "firstMove" else "wholeLine"

    /// Names the parts of a slice that are NOT already in the header. Both are usually
    /// the defaults, and printing "nodes 1" on every table of every ordinary run is noise
    /// - but when a run has more than one, two tables otherwise carry the same title.
    let private sliceSuffix (nodes: int) (puzzleFilter: string) =
        let filterPart =
            if PuzzlePaired.noFilter puzzleFilter then "" else sprintf ", theme %s" puzzleFilter
        let nodesPart = if nodes > 1 then sprintf ", %d nodes" nodes else ""
        filterPart + nodesPart

    /// Writes `puzzleThemes_<stamp>.csv` and returns the tables plus a console headline.
    ///
    /// Shared by the console and the GUI: both run the same analysis and produce the same
    /// files. Scores do not arrive in config order, so the engine names are passed in that
    /// order and the FIRST one becomes the baseline every other net is measured against.
    let writeThemeFiles
        (outputFolder: string)
        (stamp: string)
        (engineNamesInConfigOrder: string seq)
        (scores: Score seq)
        : ThemeOutput =
        let summary = StringBuilder()
        let rows = ResizeArray<string>()
        // engine_a/engine_b are what make a side identifiable: a cross-engine run puts ONE
        // net under two engines, so net_a and net_b are equal and only the engine tells the
        // two columns apart. Appended, so a reader written against the older layout is
        // unaffected.
        // `nodes` completes the slice key writeThemeFiles groups on. Without it a run with
        // two node budgets (`"Type": "search", "Nodes": "10, 100"`) wrote two sets of rows
        // with identical keys and different accuracies, which the reader could only treat
        // as self-contradictory.
        // `scoring` names the rule the accuracy columns were computed with, so a reader can
        // tell a first-move file from a whole-line one. Without it the two are byte-shaped
        // the same and differ silently by 10-25 pp on multi-move themes.
        rows.Add("type,rating_group,net_a,net_b,theme,n,accuracy_a_pct,accuracy_b_pct,delta_pp,sigma,puzzle_filter,engine_a,engine_b,nodes,scoring")

        let order =
            engineNamesInConfigOrder
            |> Seq.mapi (fun i name -> (if isNull name then "" else name), i)
            |> Seq.distinctBy fst
            |> dict
        let orderOf (s: Score) =
            match order.TryGetValue(if isNull s.Engine then "" else s.Engine) with
            | true, i -> i
            | _ -> Int32.MaxValue

        // strongest difference seen, for the console line
        let mutable best : (string * int * string * float * float) option = None
        let considerHeadline testType ratingGroup (d: ThemeDiff) =
            let sigma = sigmaOf d.Total d.DeltaPp
            if sigma >= 2.0 then
                match best with
                | Some (_, _, _, _, bestSigma) when bestSigma >= sigma -> ()
                | _ -> best <- Some (d.Theme, ratingGroup, testType, d.DeltaPp, sigma)

        let mutable header = false
        let addHeader (title: string) =
            if not header then
                summary.AppendLine() |> ignore
                summary.AppendLine(title) |> ignore
                header <- true

        // The slice key must match PuzzlePaired and PuzzleCrossEngine, or the same run
        // is cut three ways. Keying on (Type, ratingGroup) alone merged the per-filter
        // Scores of a `PuzzleFilter: "fork,pin"` run, so `baseline :: others` made the
        // first comparison netA-on-fork against netA-on-pin: one net against itself,
        // measured on two different puzzle sets.
        let groups =
            scores
            |> Seq.groupBy PuzzlePaired.sliceKeyOf
            |> Seq.sortBy fst

        for (testType, ratingGroup, nodes, puzzleFilter), group in groups do
            // A slice is one measurement per net; two tests that share a Type label
            // (policy vs policyvalue, or a search at nodes <= 1) otherwise make a net
            // the baseline for itself.
            match group |> Seq.sortBy orderOf |> Seq.distinctBy PuzzlePaired.netKeyOf |> Seq.toList with
            | baseline :: others when not others.IsEmpty ->
                let baseBreak = breakdown baseline
                if not baseBreak.IsEmpty then
                    for other in others do
                        let diffs, dropped = diff MinThemePuzzles baseBreak (breakdown other)
                        if not diffs.IsEmpty then
                            addHeader "--- Per-Theme Comparison (vs first engine) ---"
                            let groupHeader =
                                sprintf "  %s @ rating %d%s%s" testType ratingGroup
                                    (sliceSuffix nodes puzzleFilter)
                                    (if dropped > 0 then sprintf "   (%d themes under %d puzzles omitted)" dropped MinThemePuzzles else "")
                            summary.AppendLine() |> ignore
                            summary.AppendLine(groupHeader) |> ignore
                            // the engine columns written below are exactly what tells two
                            // sides apart when they share a net name; the table above them
                            // was still printing "A = netX / B = netX"
                            summary.Append(
                                renderDiffFor baseline.NeuralNet other.NeuralNet
                                    baseline.Engine other.Engine TopThemes diffs) |> ignore
                            for d in diffs do
                                considerHeadline testType ratingGroup d
                                rows.Add(
                                    String.Format(
                                        Globalization.CultureInfo.InvariantCulture,
                                        "{0},{1},{2},{3},{4},{5},{6:F2},{7:F2},{8:F2},{9:F2},{10},{11},{12},{13},{14}",
                                        testType, ratingGroup, csvField baseline.NeuralNet, csvField other.NeuralNet,
                                        csvField d.Theme, d.Total, d.AccuracyA * 100.0, d.AccuracyB * 100.0, d.DeltaPp,
                                        sigmaOf d.Total d.DeltaPp, csvField puzzleFilter,
                                        csvField baseline.Engine, csvField other.Engine, nodes,
                                        scoringRule baseline))
            | [ only ] ->
                // single-net run: no reference to compare against, but the per-theme rates
                // are still the answer to "where is this net weak"
                let stats = breakdown only
                let usable = stats |> List.filter (fun s -> s.Total >= MinThemePuzzles)
                if not usable.IsEmpty then
                    addHeader "--- Per-Theme Breakdown ---"
                    let groupHeader =
                        sprintf "  %s @ rating %d%s   (%d themes under %d puzzles omitted)"
                            testType ratingGroup (sliceSuffix nodes puzzleFilter)
                            (stats.Length - usable.Length) MinThemePuzzles
                    summary.AppendLine() |> ignore
                    summary.AppendLine(groupHeader) |> ignore
                    summary.Append(renderSingle only.NeuralNet MinThemePuzzles TopThemes stats) |> ignore
                    for s in usable do
                        // no reference net, so net_a and the A/delta columns stay empty
                        rows.Add(
                            String.Format(
                                Globalization.CultureInfo.InvariantCulture,
                                "{0},{1},,{2},{3},{4},,{5:F2},,,{6},,{7},{8},{9}",
                                testType, ratingGroup, csvField only.NeuralNet,
                                csvField s.Theme, s.Total, accuracyOf s * 100.0, csvField puzzleFilter,
                                csvField only.Engine, nodes, scoringRule only))
            | _ -> ()

        if rows.Count > 1 then
            let csvPath = Path.Combine(outputFolder, sprintf "puzzleThemes_%s.csv" stamp)
            File.WriteAllLines(csvPath, rows)
            summary.AppendLine() |> ignore
            summary.AppendLine(sprintf "  All themes: %s" csvPath) |> ignore
            let headline =
                match best with
                | Some (theme, rg, testType, delta, sigma) ->
                    sprintf "  Biggest theme gap: %s %+.1f pp on %s @ %d (%.1f sigma)"
                        theme delta testType rg sigma
                | None -> "  No theme difference cleared 2 sigma."
            { Summary = summary.ToString(); CsvPath = csvPath; Headline = headline }
        else emptyThemeOutput
