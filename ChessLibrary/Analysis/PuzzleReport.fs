namespace ChessLibrary

open System
open System.IO
open System.Text.Json

/// Reads a finished puzzle run back from its output files — the
/// `LichessSummary_<stamp>.json` and its sibling `puzzleThemes_<stamp>.csv` —
/// and computes the report views the GUI renders: a leaderboard, per-theme
/// heatmaps against the peer group, and per-net strength/weakness profiles.
///
/// Everything here is file-driven: no live Score objects, no engines. That is
/// deliberate — the page must open any past run, not just the one in memory.
///
/// Do NOT reuse PuzzleTrend.loadFile for this: it drops every net whose name
/// carries no training-step counter (by design, it plots training curves),
/// which would silently discard ordinary nets like `C1-640-34`.
module PuzzleReport =

    // ------------------------------------------------------------------
    // Summary JSON reader
    // ------------------------------------------------------------------

    /// One row of the summary's `scores` array. Mirrors PuzzleJsonOutput's
    /// camelCase schema (see Console/PuzzleJsonSchema.md). estNodes fields are
    /// 0.0 both for non-policy rows and for runs written before those fields
    /// existed — consumers gate on `EstNodesP95 > 0`.
    type ScoreRow =
        { Engine: string
          NeuralNet: string
          Type: string
          /// The PuzzleFilter theme this slice was run on. A run with
          /// `PuzzleFilter: "fork,pin"` emits one row per filter per net, and without
          /// this they are indistinguishable and the leaderboard shows an arbitrary one.
          ///
          /// NOT "" for an unfiltered run - PuzzleEngineAgent stamps the literal "none".
          Filter: string
          Nodes: int
          Accuracy: float
          TotalNumber: int
          RatingAvg: float
          PlayerRating: float
          AvgKLD: float
          AvgEstNodesLog10: float
          EstNodesP95: float
          EstNodesP99: float
          EstNodesMax: float
          EstNodesCdf100: float }

    /// One row of the summary's `paired` array: two nets on the puzzles both
    /// scored, with the discordant counts McNemar needs. Empty for single-net
    /// runs and for runs written before the field existed - consumers gate on
    /// `Discordant > 0`, since a pair that never disagrees carries no test.
    type PairedRow =
        { Type: string
          RatingGroup: int
          /// Nodes and Filter complete the slice key. Without them two rows that the
          /// writer deliberately kept apart are indistinguishable in the table - a
          /// `PuzzleFilter: "fork,pin,skewer"` run renders three identical-looking rows.
          Nodes: int
          Filter: string
          /// Needed when NetA = NetB: one ONNX under two engines is a real comparison
          /// whose sides share a net name, and "netX vs netX" reads as a bug.
          EngineA: string
          EngineB: string
          NetA: string
          NetB: string
          N: int
          OnlyA: int
          OnlyB: int
          Discordant: int
          DeltaPp: float
          Z: float
          P: float }

    type SummaryDoc =
        { PuzzleFile: string
          SampleSize: int
          RatingGroups: string
          StartedUtc: string
          ElapsedSeconds: float
          Scores: ScoreRow[]
          Paired: PairedRow[]
          /// True only when computing the paired stats threw. An empty `Paired` is
          /// normal - a single-net run has nothing to compare - so a consumer must read
          /// this before concluding anything from the absence of comparisons.
          PairedFailed: bool }

    let private tryProp (el: JsonElement) (name: string) =
        match el.TryGetProperty name with
        | true, v -> Some v
        | _ -> None

    let private strProp el name =
        tryProp el name
        |> Option.bind (fun (v: JsonElement) ->
            if v.ValueKind = JsonValueKind.String then Some (v.GetString()) else None)
        |> Option.defaultValue ""

    let private numProp el name =
        tryProp el name
        |> Option.bind (fun (v: JsonElement) ->
            if v.ValueKind = JsonValueKind.Number then Some (v.GetDouble()) else None)
        |> Option.defaultValue 0.0

    /// Reads one LichessSummary JSON, keeping every score row.
    let loadSummary (path: string) : SummaryDoc =
        use doc = JsonDocument.Parse(File.ReadAllText path)
        let root = doc.RootElement
        let scores =
            match tryProp root "scores" with
            | Some s when s.ValueKind = JsonValueKind.Array ->
                [| for el in s.EnumerateArray() ->
                     { Engine = strProp el "engine"
                       NeuralNet = strProp el "neuralNet"
                       Type = strProp el "type"
                       Filter = strProp el "filter"
                       Nodes = int (numProp el "nodes")
                       Accuracy = numProp el "accuracy"
                       TotalNumber = int (numProp el "totalNumber")
                       RatingAvg = numProp el "ratingAvg"
                       PlayerRating = numProp el "playerRating"
                       AvgKLD = numProp el "avgKLD"
                       AvgEstNodesLog10 = numProp el "avgEstNodesLog10"
                       EstNodesP95 = numProp el "estNodesP95"
                       EstNodesP99 = numProp el "estNodesP99"
                       EstNodesMax = numProp el "estNodesMax"
                       EstNodesCdf100 = numProp el "estNodesCdf100" } |]
            | _ -> [||]
        let paired =
            match tryProp root "paired" with
            | Some s when s.ValueKind = JsonValueKind.Array ->
                [| for el in s.EnumerateArray() ->
                     { Type = strProp el "type"
                       RatingGroup = int (numProp el "ratingGroup")
                       Nodes = int (numProp el "nodes")
                       Filter = strProp el "filter"
                       EngineA = strProp el "engineA"
                       EngineB = strProp el "engineB"
                       NetA = strProp el "netA"
                       NetB = strProp el "netB"
                       N = int (numProp el "n")
                       OnlyA = int (numProp el "onlyA")
                       OnlyB = int (numProp el "onlyB")
                       Discordant = int (numProp el "discordant")
                       DeltaPp = numProp el "deltaPp"
                       Z = numProp el "z"
                       P = numProp el "p" } |]
            | _ -> [||]
        { PuzzleFile = strProp root "puzzleFile"
          SampleSize = int (numProp root "sampleSize")
          RatingGroups = strProp root "ratingGroups"
          StartedUtc = strProp root "startedUtc"
          ElapsedSeconds = numProp root "elapsedSeconds"
          Scores = scores
          Paired = paired
          PairedFailed =
            match tryProp root "pairedFailed" with
            | Some v when v.ValueKind = JsonValueKind.True -> true
            | _ -> false }

    /// The themes CSV that PuzzleThemes.writeThemeFiles writes beside the
    /// summary, sharing its timestamp: LichessSummary_<stamp>.json →
    /// puzzleThemes_<stamp>.csv in the same folder.
    let themesPathFor (summaryJsonPath: string) : string =
        let dir = Path.GetDirectoryName summaryJsonPath
        let name = Path.GetFileNameWithoutExtension summaryJsonPath
        let stamp =
            let prefix = "LichessSummary_"
            if name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
            then name.Substring prefix.Length
            else name
        Path.Combine(dir, sprintf "puzzleThemes_%s.csv" stamp)

    // ------------------------------------------------------------------
    // Themes CSV reader
    // ------------------------------------------------------------------

    /// One CSV row. Multi-net runs fill every column; single-net runs leave
    /// NetA/AccA (and delta/sigma, which we recompute anyway) blank — see the
    /// single-net writer in PuzzleThemes.writeThemeFiles.
    type ThemeRow =
        { Type: string
          RatingGroup: string
          /// The PuzzleFilter theme of the slice, matching Score.Filter - so an
          /// unfiltered run reads "none", NOT "".
          ///
          /// "" means the column was absent, i.e. a file written before it existed.
          /// That is not a filter value and must not be compared against one; see
          /// `matchesFilter`.
          Filter: string
          /// Engine names. "" in files written before these columns existed, which is
          /// exactly the case where a shared net name cannot be attributed.
          EngineA: string
          EngineB: string
          /// Which rule the accuracy columns used: "firstMove" (the puzzle's thematic
          /// move) or "wholeLine" (the whole solution). "" in files written before the
          /// column existed, which are wholeLine by definition.
          Scoring: string
          /// Node budget of the slice. `None` means the column was absent - a file
          /// written before it existed - which is NOT the same as a run at zero nodes.
          /// 0 is a real, common value: head runners stamp Score.Nodes with a placeholder
          /// rather than a budget, and older files carry 0 on nearly every row. Using 0 as
          /// an "absent" sentinel collided with 93% of the result files on disk.
          Nodes: int option
          NetA: string
          NetB: string
          Theme: string
          N: int
          /// NaN when the column is blank (single-net rows).
          AccA: float
          AccB: float }

    let private parseFloatOrNaN (s: string) =
        match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> v
        | _ -> Double.NaN

    /// Splits one CSV record, honouring the RFC quoting PuzzleThemes.csvField emits.
    ///
    /// Net names come from user-authored engine defs and may contain a comma, which
    /// the writer quotes — a plain String.Split on ',' shifts every later column and
    /// silently drops the row, taking the whole run's theme data with it.
    let private splitCsv (line: string) : string[] =
        let out = ResizeArray<string>()
        let sb = Text.StringBuilder()
        let mutable inQuotes = false
        let mutable i = 0
        while i < line.Length do
            let c = line.[i]
            if inQuotes then
                if c = '"' then
                    // a doubled quote inside a quoted field is one literal quote
                    if i + 1 < line.Length && line.[i + 1] = '"' then
                        sb.Append '"' |> ignore
                        i <- i + 1
                    else inQuotes <- false
                else sb.Append c |> ignore
            elif c = '"' then inQuotes <- true
            elif c = ',' then
                out.Add(sb.ToString())
                sb.Clear() |> ignore
            else sb.Append c |> ignore
            i <- i + 1
        out.Add(sb.ToString())
        out.ToArray()

    /// Parses puzzleThemes_<stamp>.csv. Malformed lines are skipped, not fatal:
    /// the file is a report artifact, and one bad row must not kill the page.
    let loadThemes (path: string) : ThemeRow[] =
        if not (File.Exists path) then [||]
        else
            let lines = File.ReadAllLines path
            // a truncated or zero-byte file is a header-only file as far as we care;
            // Array.skip would throw here and take the leaderboard down with it
            if lines.Length <= 1 then [||]
            else
            lines
            |> Array.skip 1
            |> Array.choose (fun line ->
                let parts = splitCsv line
                if parts.Length < 8 then None
                else
                    match Int32.TryParse(parts.[5], Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
                    | true, n when n > 0 ->
                        // read positionally but defensively: the trailing columns are
                        // absent in every file written before they were added
                        let at i = if parts.Length > i then parts.[i] else ""
                        Some { Type = parts.[0]
                               RatingGroup = parts.[1]
                               Filter = at 10
                               EngineA = at 11
                               EngineB = at 12
                               Scoring = at 14
                               Nodes =
                                   match Int32.TryParse(at 13, Globalization.NumberStyles.Integer,
                                                        Globalization.CultureInfo.InvariantCulture) with
                                   | true, v -> Some v
                                   | _ -> None
                               NetA = parts.[2]
                               NetB = parts.[3]
                               Theme = parts.[4]
                               N = n
                               AccA = parseFloatOrNaN parts.[6]
                               AccB = parseFloatOrNaN parts.[7] }
                    | _ -> None)

    // ------------------------------------------------------------------
    // Theme filtering
    // ------------------------------------------------------------------

    /// Lichess tags that describe the puzzle rather than a tactical motif —
    /// its solution length, its evaluation swing, or the provenance of the
    /// game. They say nothing about what the position asks the net to see.
    let nonTacticalThemes =
        set [ "short"; "long"; "veryLong"; "oneMove"
              "crushing"; "advantage"; "equality"
              "master"; "masterVsMaster"; "superGM" ]

    /// Lichess tags nest: a puzzle tagged `mateIn3` is also tagged `mate`, and one
    /// tagged `rookEndgame` is also tagged `endgame`. Verified on real runs — at rg2300
    /// `mate` read 119 while mateIn3+mateIn4 alone summed to 80, and at rg2600 `endgame`
    /// (3224) sits over `pawnEndgame` (648) and `rookEndgame` (421).
    ///
    /// Both levels are worth seeing: the parent has the sample size, the child carries
    /// "fine in endgames generally, poor specifically in rook endings". So neither is
    /// dropped up front. What the nesting DOES control is weighting — see
    /// `buildHeatView`, where a child never contributes to a net's overall offset, so
    /// the same puzzles cannot be counted three times just because a family has
    /// subtypes.
    let parentOfTheme : Map<string, string> =
        [ for child in [ "mateIn1"; "mateIn2"; "mateIn3"; "mateIn4"; "mateIn5"
                         // Lichess also tags every named mating pattern with `mate`
                         "backRankMate"; "smotheredMate"; "hookMate"; "arabianMate"
                         "anastasiaMate"; "bodenMate"; "doubleBishopMate"; "dovetailMate"
                         "killBoxMate"; "vukovicMate" ] -> child, "mate"
          for child in [ "rookEndgame"; "pawnEndgame"; "queenEndgame"; "bishopEndgame"
                         "knightEndgame"; "queenRookEndgame" ] -> child, "endgame" ]
        |> Map.ofList

    /// How far a subtype must pull away from its parent, in standard errors on the
    /// subtype's own count, before it earns a column of its own.
    ///
    /// A display threshold, not a test: parent and child share puzzles, so this
    /// overstates the evidence. It sits below the callout bar deliberately — the
    /// question here is "does this deserve screen space", not "is this a finding",
    /// and the callout guard still applies afterwards.
    [<Literal>]
    let SubtypeDivergenceSigma = 1.5

    /// Drops the tags that describe the puzzle rather than the tactic. Nesting is
    /// handled later, where the residuals needed to judge it exist.
    let dropNonTactical (rows: ThemeRow[]) : ThemeRow[] =
        rows |> Array.filter (fun r -> not (nonTacticalThemes.Contains r.Theme))

    // ------------------------------------------------------------------
    // Heatmap view
    // ------------------------------------------------------------------

    /// One heatmap column: a theme, its sample size, and the baseline the
    /// cells are measured against (peer mean; the net's own mean for
    /// single-net runs).
    type ThemeCol =
        { Theme: string
          N: int
          Mean: float
          /// Set when this column is a subtype of another column in the same view
          /// (rookEndgame under endgame). Such a column is shown only because it
          /// diverges from its parent, and is excluded from the offset so its
          /// puzzles are not counted twice.
          Parent: string option }

    /// The heatmap for one (metric, rating group) slice, in net-major layout:
    /// Raw.[i].[j] is net i's accuracy minus the peer mean on theme j, and
    /// Spec additionally removes net i's overall offset — "is this net
    /// unusually good here, for a net of its level". For single-net runs the
    /// peer mean degenerates to the net's own mean across themes, Raw = Spec,
    /// and the page hides the mode toggle.
    type HeatView =
        { Nets: string[]
          /// Per-net mean residual across the kept themes (0.0 for single-net).
          Offsets: float[]
          Cols: ThemeCol[]
          /// Accuracy in percent, net-major.
          Acc: float[][]
          Raw: float[][]
          Spec: float[][] }

    /// One column of the heat view: a net as measured by one engine.
    ///
    /// Net name alone is NOT the identity. A cross-engine run puts one ONNX under Ceres
    /// and Lc0, so both sides of every row carry the same net name and different numbers;
    /// keying on the name merged two engines into one column, or - when their accuracies
    /// happened to tie, routine at the CSV's two decimals - silently reported them as one
    /// net. The engine columns make it exact.
    type SideKey = { Engine: string; Net: string }

    /// How a column is labelled: the net alone where that is unambiguous, and
    /// "net (engine)" where the same net was measured by more than one engine.
    /// The leaderboard uses the same rule so the two views name a net identically.
    let labelFor (allKeys: SideKey seq) (k: SideKey) =
        let sharedNet =
            allKeys |> Seq.filter (fun o -> o.Net = k.Net) |> Seq.distinct |> Seq.length > 1
        if sharedNet && not (String.IsNullOrWhiteSpace k.Engine) then
            sprintf "%s  (%s)" k.Net k.Engine
        else k.Net

    /// theme -> side -> accuracy for one (type, ratingGroup, filter) slice, folding the
    /// A and B sides of every comparison row. Only themes measured for every side are
    /// kept - a partial column cannot be compared against the mean.
    ///
    /// `filter` completes the slice: a run with `PuzzleFilter: "fork,pin"` writes one set
    /// of rows per filter theme, and both sets carry the shared Lichess themes (middlegame,
    /// crushing, ...). Folding them together made every such theme look self-contradictory.
    ///
    /// A theme is still dropped when one side reports two different accuracies for the SAME
    /// engine and net, which is a genuine contradiction rather than an identity problem -
    /// and is what a file written before the engine columns existed looks like for a
    /// cross-engine run.
    /// Whether a themes row belongs to the slice being viewed.
    ///
    /// A row from a file written before `puzzle_filter` existed carries "" - the column
    /// was absent, not empty. Comparing that against the summary's filter excluded every
    /// such row: an unfiltered run stamps the literal "none", so `"" = "none"` is false and
    /// the heat view and every net profile silently vanished for every result file on disk
    /// older than the column. Those files cannot be split by filter anyway, so they match
    /// whatever is selected - exactly the behaviour before the column existed.
    let private matchesFilter (rowFilter: string) (selected: string) =
        String.IsNullOrEmpty rowFilter || rowFilter = selected

    /// An unfiltered run carries the literal "none" - PuzzleEngineAgent stamps it - so
    /// "" and "none" both mean "no theme filter". Shared with PuzzlePaired.noFilter and
    /// the report page so the four renderings of this decision cannot disagree.
    let noFilter (f: string) = String.IsNullOrWhiteSpace f || f = "none"

    /// Same rule for the node budget, but with absence made explicit: a row from a file
    /// without the column (None) predates the split and matches anything. A row that HAS
    /// a budget must match exactly - including 0, which is a real value.
    let private matchesNodes (rowNodes: int option) (selected: int option) =
        match rowNodes, selected with
        | None, _ | _, None -> true
        | Some r, Some sel -> r = sel

    let private themeMatrix (rows: ThemeRow[]) (typ: string) (rg: string) (filter: string) (nodes: int option) =
        let acc = Collections.Generic.Dictionary<string, Collections.Generic.Dictionary<SideKey, float>>()
        let ns = Collections.Generic.Dictionary<string, int>()
        let ambiguous = Collections.Generic.HashSet<string>()
        for r in rows do
            if r.Type = typ && r.RatingGroup = rg && matchesFilter r.Filter filter
               && matchesNodes r.Nodes nodes then
                let cell =
                    match acc.TryGetValue r.Theme with
                    | true, d -> d
                    | _ ->
                        let d = Collections.Generic.Dictionary<SideKey, float>()
                        acc.[r.Theme] <- d
                        d
                // n can differ between pairs (PuzzleThemes.diff uses the pair minimum),
                // so keep the smallest: the gates and the header must not depend on
                // which row happened to be read last.
                ns.[r.Theme] <-
                    match ns.TryGetValue r.Theme with
                    | true, existing -> min existing r.N
                    | _ -> r.N
                let put (engine: string) (net: string) (a: float) =
                    if not (String.IsNullOrWhiteSpace net) && not (Double.IsNaN a) then
                        let key = { Engine = engine; Net = net }
                        match cell.TryGetValue key with
                        | true, prior when abs (prior - a) > 1e-9 -> ambiguous.Add r.Theme |> ignore
                        | _ -> cell.[key] <- a
                put r.EngineA r.NetA r.AccA
                put r.EngineB r.NetB r.AccB
        for theme in ambiguous do
            acc.Remove theme |> ignore
            ns.Remove theme |> ignore
        acc, ns, ambiguous.Count

    /// Themes dropped as unattributable in one slice. With the engine columns present this
    /// should be zero; a result file written before they existed still hits it whenever one
    /// net was run under two engines.
    let unattributableThemes (rows: ThemeRow[]) (typ: string) (rg: string) (filter: string) (nodes: int option) : int =
        let _, _, ambiguous = themeMatrix rows typ rg filter nodes
        ambiguous

    /// Builds the heatmap for one (metric, rating group, filter) slice. `netOrder`
    /// fixes the row order by NET NAME (the page passes nets sorted by headline
    /// accuracy); sides whose net is missing from the order are appended, and one net
    /// measured by two engines contributes two adjacent columns. Returns None when
    /// nothing clears `minN`.
    let buildHeatView (rows: ThemeRow[]) (typ: string) (rg: string) (filter: string) (nodes: int option) (minN: int) (netOrder: string[]) : HeatView option =
        let acc, ns, _ambiguous = themeMatrix rows typ rg filter nodes
        let allKeys =
            acc.Values
            |> Seq.collect (fun d -> d.Keys)
            |> Seq.distinct
            |> Seq.toArray
        let orderIndex (k: SideKey) =
            match netOrder |> Array.tryFindIndex (fun n -> n = k.Net) with
            | Some i -> i
            | None -> Int32.MaxValue
        let nets = allKeys |> Array.sortBy (fun k -> orderIndex k, k.Engine, k.Net)
        if nets.Length = 0 then None
        else
            let kept =
                acc
                |> Seq.filter (fun kv ->
                    ns.[kv.Key] >= minN && nets |> Array.forall kv.Value.ContainsKey)
                |> Seq.map (fun kv -> kv.Key)
                |> Seq.toArray
            if kept.Length = 0 then None
            else
                let single = nets.Length = 1
                let means =
                    if single then
                        // no peers: the baseline is the net's own mean across themes
                        let own = kept |> Array.averageBy (fun t -> acc.[t].[nets.[0]])
                        kept |> Array.map (fun _ -> own)
                    else
                        kept |> Array.map (fun t -> nets |> Array.averageBy (fun n -> acc.[t].[n]))
                // columns easiest-first, by baseline accuracy (single-net: by own accuracy).
                // Theme name breaks ties, for the same reason PuzzleThemes.breakdown does it:
                // accuracies arrive rounded to two decimals so exact ties are routine, and a
                // stable sort would otherwise leave column order decided by CSV row order -
                // which is set by an unrelated writer's sort.
                let order =
                    if single then
                        Array.init kept.Length id
                        |> Array.sortBy (fun j -> -acc.[kept.[j]].[nets.[0]], kept.[j])
                    else
                        Array.init kept.Length id |> Array.sortBy (fun j -> -means.[j], kept.[j])
                let ordered = order |> Array.map (fun j -> kept.[j])
                let orderedMeans = order |> Array.map (fun j -> means.[j])

                // A theme is a child only when its parent is also present here: a run
                // whose slice carries pawnEndgame but no endgame has no nesting to
                // resolve, and pawnEndgame stands on its own.
                let indexOf = ordered |> Array.mapi (fun j t -> t, j) |> Map.ofArray
                let parentIdx =
                    ordered
                    |> Array.map (fun t ->
                        parentOfTheme
                        |> Map.tryFind t
                        |> Option.bind indexOf.TryFind)

                let rawOf (side: SideKey) (t: string) = acc.[t].[side] - orderedMeans.[indexOf.[t]]

                // Keep a child column when some net's residual pulls away from its
                // parent's by more than noise on the child's count. The offset cancels
                // in the difference, so this is the same judgement before or after the
                // strength-removed correction.
                let keepColumn =
                    ordered
                    |> Array.mapi (fun j t ->
                        match parentIdx.[j] with
                        | None -> true
                        | Some pj ->
                            let parent = ordered.[pj]
                            nets |> Array.exists (fun n ->
                                let gap = rawOf n t - rawOf n parent
                                PuzzleThemes.sigmaOf ns.[t] gap >= SubtypeDivergenceSigma))

                // Surviving children sit next to their parent instead of in the global
                // easiest-first order, so the nesting reads as nesting.
                let finalOrder =
                    [| for j in 0 .. ordered.Length - 1 do
                         if keepColumn.[j] && parentIdx.[j].IsNone then
                             yield j
                             for k in 0 .. ordered.Length - 1 do
                                 if keepColumn.[k] && parentIdx.[k] = Some j then yield k |]

                let cols = finalOrder |> Array.map (fun j -> ordered.[j])
                let colParent =
                    finalOrder |> Array.map (fun j -> parentIdx.[j] |> Option.map (fun pj -> ordered.[pj]))
                let colMeans =
                    if single then
                        // Recomputed AFTER column selection: averaging over every kept theme
                        // would fold in columns the viewer cannot see, and would count an
                        // endgame subtype twice alongside its parent - the same double-count
                        // the multi-net offset below excludes.
                        let baseCols =
                            cols |> Array.mapi (fun j t -> j, t)
                                 |> Array.filter (fun (j, _) -> colParent.[j].IsNone)
                                 |> Array.map snd
                        let source = if baseCols.Length = 0 then cols else baseCols
                        let own = source |> Array.averageBy (fun t -> acc.[t].[nets.[0]])
                        cols |> Array.map (fun _ -> own)
                    else
                        finalOrder |> Array.map (fun j -> orderedMeans.[j])
                let accM = nets |> Array.map (fun n -> cols |> Array.map (fun t -> acc.[t].[n]))
                let raw = accM |> Array.map (fun row -> row |> Array.mapi (fun j a -> a - colMeans.[j]))

                // The offset is what "this net's overall level" means, so it must count
                // each puzzle once: children are excluded, however many are displayed.
                let offsets =
                    if single then nets |> Array.map (fun _ -> 0.0)
                    else
                        raw
                        |> Array.map (fun row ->
                            let parentsOnly =
                                row |> Array.mapi (fun j d -> j, d)
                                    |> Array.filter (fun (j, _) -> colParent.[j].IsNone)
                                    |> Array.map snd
                            if parentsOnly.Length = 0 then Array.average row
                            else Array.average parentsOnly)
                let spec = raw |> Array.mapi (fun i row -> row |> Array.map (fun d -> d - offsets.[i]))
                Some { Nets = nets |> Array.map (labelFor allKeys)
                       Offsets = offsets
                       Cols =
                         cols
                         |> Array.mapi (fun j t ->
                             { Theme = t; N = ns.[t]; Mean = colMeans.[j]; Parent = colParent.[j] })
                       Acc = accM
                       Raw = raw
                       Spec = spec }

    // ------------------------------------------------------------------
    // Profiles
    // ------------------------------------------------------------------

    /// A net's most extreme theme in one direction: how far above or below its
    /// own expected level it sits there, and how many binomial standard errors
    /// that distance is.
    type ProfileEntry =
        { Theme: string
          N: int
          /// Strength-removed residual in percentage points; sign carries direction.
          DeltaPp: float
          /// |DeltaPp| in standard errors, from PuzzleThemes.sigmaOf — the same
          /// definition the themes CSV and the console tables use, so one number
          /// never means two things.
          ///
          /// That form fixes p at 0.5, the widest SE the data can produce, and so
          /// does NOT vary with the net's own accuracy. An earlier version here
          /// derived the SE from the net's score on the theme, which handed weak
          /// nets a collapsing SE: in an ablation the knocked-out arm reported the
          /// same residual as ~13x more significant, and a net at exactly 0% or
          /// 100% produced ~10^4 sigma and cleared every bar.
          ///
          /// Read it as a screening number, not a test: see buildProfiles on why
          /// the per-comparison bar understates the risk across many themes.
          Sigma: float }

    type NetProfile =
        { Net: string
          Best: ProfileEntry option
          Worst: ProfileEntry option }

    /// Per-net extreme themes from a heat view's strength-removed residuals,
    /// restricted to themes with at least `calloutN` puzzles. Best requires a
    /// positive residual and Worst a negative one — a net whose residuals all
    /// point one way reports only that side.
    ///
    /// CAUTION for callers: each entry is the extreme of many themes, so its
    /// Sigma is a per-comparison figure and overstates how surprising the entry
    /// is. Over T candidate themes the chance that the largest exceeds 2 sigma
    /// under the null is roughly 1-(1-0.023)^T — about 22% at T=11, per net, per
    /// direction. Show these behind a threshold by default, and do not read a
    /// single surviving entry as a finding.
    let buildProfiles (view: HeatView) (calloutN: int) : NetProfile[] =
        // Subtype columns are excluded: one is displayed precisely because some net's
        // residual there was extreme, so offering it as a callout candidate would rank a
        // pre-selected column against unselected ones, and its puzzles are a subset of the
        // parent column sitting beside it.
        let cols =
            view.Cols
            |> Array.mapi (fun j c -> j, c)
            |> Array.filter (fun (_, c) -> c.N >= calloutN && c.Parent.IsNone)
        view.Nets
        |> Array.mapi (fun i net ->
            let entries =
                cols
                |> Array.map (fun (j, c) ->
                    { Theme = c.Theme
                      N = c.N
                      DeltaPp = view.Spec.[i].[j]
                      Sigma = PuzzleThemes.sigmaOf c.N view.Spec.[i].[j] })
            let best =
                if entries.Length = 0 then None
                else
                    let e = entries |> Array.maxBy (fun e -> e.DeltaPp)
                    if e.DeltaPp > 0.0 then Some e else None
            let worst =
                if entries.Length = 0 then None
                else
                    let e = entries |> Array.minBy (fun e -> e.DeltaPp)
                    if e.DeltaPp < 0.0 then Some e else None
            { Net = net; Best = best; Worst = worst })
