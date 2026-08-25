module ChessLibrary.PuzzlePaired

// --------------------------------------------------------------------------
// Paired (McNemar) comparison of two nets on one puzzle slice.
//
// Every net in a run is scored on the SAME puzzles, so position difficulty is
// common to both and cancels. The unpaired screening sigma in PuzzleThemes
// deliberately ignores that - it has only a correct/total pair per theme and
// cannot recover the agreement structure. At the slice level the solved/failed
// SETS are still around, so the discordant counts are recoverable and the
// paired test is available for free.
// --------------------------------------------------------------------------

open System
open ChessLibrary.PuzzleTypes

/// Rating groups are reported as the average rating of the sampled puzzles.
/// Shared by PuzzleThemes, PuzzleCrossEngine and the paired stats so all three
/// BUCKET a run the same way; they used to bucket independently, and
/// PuzzleCrossEngine's omission of the rating group silently collapsed two
/// groups into one.
///
/// Sharing the bucket is not the same as sharing the slice key - see `sliceKeyOf`,
/// which is the part that has to match. `PuzzleTrend.ratingGroupOf` is still a
/// separate copy of this rule, and the report page uses that one.
let ratingGroupOf (ratingAvg: float) = int (Math.Round(ratingAvg / 100.0)) * 100

/// One net pair on one (type, rating group, nodes, filter) slice.
type PairedComparison =
    { Type: string
      RatingGroup: int
      Nodes: int
      Filter: string
      EngineA: string
      EngineB: string
      NetA: string
      NetB: string
      /// Puzzles both nets scored.
      N: int
      /// Solved by A, failed by B.
      OnlyA: int
      /// Solved by B, failed by A.
      OnlyB: int
      /// OnlyA + OnlyB - the only positions carrying information.
      Discordant: int
      AccuracyAPct: float
      AccuracyBPct: float
      /// B minus A, in percentage points. Same orientation as PuzzleThemes.
      DeltaPp: float
      /// (OnlyB - OnlyA) / sqrt(Discordant). Signed: positive means B is better.
      Z: float }

/// McNemar's z without continuity correction - the normal approximation to the
/// binomial sign test on the discordant pairs.
///
/// Trustworthy from roughly 25 discordant pairs upward; below that it is
/// optimistic and the exact binomial would be the honest test. Discordance is
/// reported alongside so a reader can see when that applies. 0.0 when the nets
/// never disagree, which is the "no information" case rather than a perfect tie.
let zOf (onlyA: int) (onlyB: int) =
    let discordant = onlyA + onlyB
    if discordant <= 0 then 0.0
    else float (onlyB - onlyA) / sqrt (float discordant)

/// Two-sided p-value for a z score. Reported for convenience only: nothing in EB
/// branches on it.
///
/// MathNet is already a dependency and Statistics.fs already uses SpecialFunctions,
/// so this used to be a hand-rolled Abramowitz & Stegun series duplicating a function
/// the library provides to full precision.
let pValueOf (z: float) =
    if Double.IsNaN z || Double.IsInfinity z then 1.0
    else max 0.0 (min 1.0 (MathNet.Numerics.SpecialFunctions.Erfc(abs z / sqrt 2.0)))

/// The four fields that make two Scores comparable: same test, same rating group,
/// same node budget, same puzzle-filter theme. PuzzleThemes and PuzzleCrossEngine
/// slice on this too - a run with `PuzzleFilter: "fork,pin"` produces one Score per
/// filter theme, and a key that ignores Filter puts them in one bucket where a net
/// gets compared against itself measured on a different set of puzzles.
let sliceKeyOf (s: Score) =
    (if isNull s.Type then "" else s.Type),
    ratingGroupOf s.RatingAvg,
    s.Nodes,
    (if isNull s.Filter then "" else s.Filter)

/// Identity of the thing being compared. Two Scores sharing it are two measurements
/// of ONE net, not two nets: `Type: "policy, policyvalue"` runs two different tests
/// that both label themselves Policy at 1 node (PuzzleEngineAgent), and a search at
/// nodes <= 1 is labelled Policy as well. Pairing those produces a net against
/// itself, with a real-looking z, which can then win the console headline.
let netKeyOf (s: Score) =
    (if isNull s.Engine then "" else s.Engine),
    (if isNull s.NeuralNet then "" else s.NeuralNet)

/// Solved, failed, and their union - built once per net, not once per pair.
let private idsOf (s: Score) =
    let solved =
        if isNull (box s.CorrectPuzzles) then Set.empty
        else s.CorrectPuzzles |> Seq.map (fun p -> p.PuzzleId) |> Set.ofSeq
    let failed =
        if isNull (box s.FailedPuzzles) then Set.empty
        else s.FailedPuzzles |> Seq.map (fun (p, _) -> p.PuzzleId) |> Set.ofSeq
    solved, failed, Set.union solved failed

/// Every net pair on every slice.
///
/// Emits all pairs rather than baseline-vs-rest: a three-net step curve wants the
/// adjacent-step pairs, not only the ones against the first net.
///
/// `engineNamesInConfigOrder` orients each pair, exactly as it does for the theme
/// tables: A is whichever net comes first in the config. Scores do NOT arrive in
/// config order, so without it A and B land in whatever order the run finished in
/// and the same comparison reads with its sign flipped in the two tables. An empty
/// sequence falls back to the order the scores arrive in.
let computeOrdered (engineNamesInConfigOrder: string seq) (scores: Score seq) : PairedComparison list =
    let order =
        engineNamesInConfigOrder
        |> Seq.mapi (fun i name -> (if isNull name then "" else name), i)
        |> Seq.distinctBy fst
        |> dict
    let orderOf (s: Score) =
        match order.TryGetValue(if isNull s.Engine then "" else s.Engine) with
        | true, i -> i
        | _ -> Int32.MaxValue
    scores
    |> Seq.filter (fun s -> not (isNull (box s)))
    |> Seq.groupBy sliceKeyOf
    |> Seq.collect (fun ((typ, rg, nodes, filter), group) ->
        // Seq.sortBy is stable, so an unknown name keeps its arrival position.
        // distinctBy AFTER the sort so the surviving row is the first in config
        // order rather than whichever test finished first.
        let nets = group |> Seq.sortBy orderOf |> Seq.distinctBy netKeyOf |> Seq.toArray
        // Each net's sets are needed by every pair it takes part in, so building them
        // inside the pair loop rebuilt them k-1 times over.
        let ids = nets |> Array.map idsOf
        [ for i in 0 .. nets.Length - 2 do
            for j in i + 1 .. nets.Length - 1 do
                let a, b = nets.[i], nets.[j]
                let solvedA, failedA, scoredA = ids.[i]
                let solvedB, failedB, scoredB = ids.[j]
                // Both nets saw the same sample, but a run that died mid-slice can
                // leave one short; scoring only the shared puzzles keeps the pair honest.
                let shared = Set.intersect scoredA scoredB
                let n = shared.Count
                if n > 0 then
                    // No `Set.intersect ... shared` around these: solvedA is a subset of
                    // scoredA and failedB of scoredB, so the result is already inside
                    // shared. The extra intersect only rebuilt a tree for Set.count to walk.
                    let onlyA = Set.intersect solvedA failedB |> Set.count
                    let onlyB = Set.intersect solvedB failedA |> Set.count
                    let accA = float (Set.intersect solvedA shared).Count / float n * 100.0
                    let accB = float (Set.intersect solvedB shared).Count / float n * 100.0
                    yield { Type = (if isNull typ then "" else typ)
                            RatingGroup = rg
                            Nodes = nodes
                            Filter = (if isNull filter then "" else filter)
                            EngineA = (if isNull a.Engine then "" else a.Engine)
                            EngineB = (if isNull b.Engine then "" else b.Engine)
                            NetA = (if isNull a.NeuralNet then "" else a.NeuralNet)
                            NetB = (if isNull b.NeuralNet then "" else b.NeuralNet)
                            N = n
                            OnlyA = onlyA
                            OnlyB = onlyB
                            Discordant = onlyA + onlyB
                            AccuracyAPct = accA
                            AccuracyBPct = accB
                            DeltaPp = accB - accA
                            Z = zOf onlyA onlyB } ])
    |> Seq.toList

/// The paired stats of a run, plus whether computing them failed.
///
/// An EMPTY list is a normal, common result: a single-net run has nothing to compare, and
/// measuring one net at a time against previously measured ones is a routine workflow. So
/// "empty" must never be read as "something went wrong" - the two cases need separate
/// signals, or a consumer treats a failed computation as a single-net run.
type PairedOutcome =
    { Comparisons: PairedComparison list
      Failed: bool }

/// The normal case, including the single-net case.
let outcomeOf (comparisons: PairedComparison list) =
    { Comparisons = comparisons; Failed = false }

/// The computation threw. The list is empty, but for a different reason.
let failedOutcome = { Comparisons = []; Failed = true }

/// Pairs in arrival order. Prefer `computeOrdered` wherever the config order is
/// in hand - see the orientation note there.
let compute (scores: Score seq) : PairedComparison list = computeOrdered Seq.empty scores

/// An unfiltered run carries the literal "none" (PuzzleEngineAgent stamps it), not "".
/// Both mean "no theme filter" and neither is worth a column of its own. Shared with
/// PuzzleThemes and the report page so the renderings of this decision cannot disagree.
let noFilter (f: string) = String.IsNullOrWhiteSpace f || f = "none"

/// Discordance below this is too thin for the normal approximation; the row is
/// still printed, flagged, because "these nets barely disagree" is itself the finding.
[<Literal>]
let ThinDiscordance = 25

/// Rows the text table will print before it starts truncating.
///
/// Pairs grow as k(k-1)/2 per slice: three nets and three metrics at two rating groups
/// is 18 rows, but a 26-net ablation is 975, which would bury the per-theme tables under
/// the paste-ready summary's own opening section. Truncation is always stated, never silent.
[<Literal>]
let MaxRenderedRows = 40

[<Literal>]
let private LabelWidth = 28

/// Keeps the END of a name: checkpoints differ in their trailing step number, so the
/// head of two nets from one arm is identical and cutting it loses nothing.
let private tail (width: int) (s: string) =
    let v = if isNull s then "" else s
    if v.Length <= width then v else "…" + v.Substring(v.Length - (width - 1))

/// Keeps the START of a single name, for values identified by their opening (theme names).
let private fitOne (width: int) (s: string) =
    let v = if isNull s then "" else s
    if v.Length <= width then v else v.Substring(0, width - 1) + "…"

/// Keeps whichever END of two engine names actually differs. "Ceres ..." vs "Lc0 ..."
/// differ at the front; "Ceres-800M-gpu0" vs "-gpu1" differ only at the back, and cutting
/// the wrong end renders both sides identically - the bug this labelling exists to avoid.
let private fitPair (width: int) (a: string) (b: string) =
    let a = if isNull a then "" else a
    let b = if isNull b then "" else b
    if a.Length <= width && b.Length <= width then a, b
    else
        let headDiffers =
            let n = min a.Length b.Length |> min width
            a.Substring(0, n) <> b.Substring(0, n)
        if headDiffers then
            let cut (s: string) = if s.Length <= width then s else s.Substring(0, width - 1) + "…"
            cut a, cut b
        else
            let cut (s: string) = if s.Length <= width then s else "…" + s.Substring(s.Length - (width - 1))
            cut a, cut b

/// Untruncated display names for the two sides of a comparison.
///
/// Anywhere a net is named, a cross-engine pair has to say WHICH engine or it reads as
/// "netX vs netX". Exposed because the console headline and the per-theme tables name the
/// same two sides and were each printing the raw net name.
let sideNames (netA: string) (netB: string) (engineA: string) (engineB: string) =
    if netA = netB && engineA <> engineB then
        sprintf "%s (%s)" netA engineA, sprintf "%s (%s)" netB engineB
    else netA, netB

/// Display names for the two sides, already fitted to the column.
///
/// Two nets are told apart by net name where that is enough, and by engine where it is
/// not: one ONNX under Ceres and Lc0 is a real comparison whose two sides carry the SAME
/// net name, and printing it as "netX vs netX" reads as a bug in the tool.
///
/// The fitting has to happen HERE, per part. Shortening the finished "net (engine)"
/// string from the left cut away the net entirely and left both sides showing the same
/// trailing engine text - strictly worse than the problem it set out to fix.
let fittedSideNames (netA: string) (netB: string) (engineA: string) (engineB: string) =
    if netA = netB && engineA <> engineB then
        let ea, eb = fitPair 11 engineA engineB
        sprintf "%s (%s)" (tail 14 netA) ea, sprintf "%s (%s)" (tail 14 netB) eb
    else tail LabelWidth netA, tail LabelWidth netB

// `sideNames` is the same decision without the column fitting, for callers that print
// untruncated names; both must agree on WHEN the engine is needed.
let private sideLabels (c: PairedComparison) =
    fittedSideNames c.NetA c.NetB c.EngineA c.EngineB

/// Renders the paired table for the run's text summary. "" when there is nothing
/// to compare (single-net run).
let render (comparisons: PairedComparison list) : string =
    if List.isEmpty comparisons then ""
    else
        let sb = Text.StringBuilder()
        // Nodes and Filter are part of the slice key, so two rows can differ ONLY in them.
        // Printing "1 node, theme none" on every row of an ordinary run is noise; leaving
        // it out of a run that has several is a table with duplicate-looking rows.
        let showSlice =
            comparisons
            |> List.map (fun c -> c.Nodes, (if noFilter c.Filter then "" else c.Filter))
            |> List.distinct
            |> List.length > 1

        let total = comparisons.Length
        let truncated = total > MaxRenderedRows
        let rows =
            if truncated then
                // strongest evidence first when we cannot show everything - the browsable
                // slice order is no use if it cuts off before the interesting rows
                comparisons |> List.sortByDescending (fun c -> abs c.Z) |> List.truncate MaxRenderedRows
            else
                comparisons
                |> List.sortBy (fun c -> c.Type, c.RatingGroup, c.Nodes, c.Filter, c.NetA, c.NetB)

        sb.AppendLine() |> ignore
        sb.AppendLine("--- Paired comparison (McNemar) ---") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine("  Both nets scored the same puzzles, so only the positions where they DISAGREE") |> ignore
        sb.AppendLine("  carry information. z = (onlyB - onlyA) / sqrt(discordant), signed so positive") |> ignore
        sb.AppendLine("  favours B. This is the tight test; the per-theme sigma below is the wide one,") |> ignore
        sb.AppendLine("  and it is measured per theme, so the two are not comparable numbers.") |> ignore
        if truncated then
            sb.AppendLine() |> ignore
            sb.AppendLine(
                sprintf "  %d of %d rows, strongest |z| first. All %d are in the run's JSON summary."
                    MaxRenderedRows total total) |> ignore
        sb.AppendLine() |> ignore

        let sliceHeader = if showSlice then sprintf " %6s %-10s" "nodes" "theme" else ""
        sb.AppendLine(
            sprintf "  %-6s %5s%s  %-28s %-28s %6s %7s %7s %8s %7s %7s %6s %8s"
                "type" "rg" sliceHeader "net A" "net B" "n" "A %" "B %" "B-A pp" "onlyA" "onlyB" "disc" "z") |> ignore
        sb.AppendLine("  " + String('-', (if showSlice then 146 else 128))) |> ignore
        for c in rows do
            let labelA, labelB = sideLabels c
            let slicePart =
                if showSlice then
                    // %-10s pads but never truncates: an 11-character theme such as
                    // exposedKing pushed every following column out of line with the header
                    sprintf " %6d %-10s" c.Nodes (fitOne 10 (if noFilter c.Filter then "-" else c.Filter))
                else ""
            sb.AppendLine(
                sprintf "  %-6s %5d%s  %-28s %-28s %6d %7.1f %7.1f %+8.1f %7d %7d %6d %+8.2f%s"
                    c.Type c.RatingGroup slicePart labelA labelB c.N
                    c.AccuracyAPct c.AccuracyBPct c.DeltaPp c.OnlyA c.OnlyB c.Discordant c.Z
                    (if c.Discordant < ThinDiscordance then "  (thin)" else "")) |> ignore
        let thin = rows |> List.filter (fun c -> c.Discordant < ThinDiscordance) |> List.length
        if thin > 0 then
            sb.AppendLine() |> ignore
            sb.AppendLine(
                sprintf "  (thin) = under %d discordant puzzles: the normal approximation is optimistic there."
                    ThinDiscordance) |> ignore
        sb.ToString()
