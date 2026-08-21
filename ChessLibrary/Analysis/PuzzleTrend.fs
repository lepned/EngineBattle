namespace ChessLibrary

open System
open System.IO
open System.Text
open System.Text.Json

/// Aggregates puzzle results across many runs into per-training-run step curves.
///
/// Input is the JSON written beside every puzzle summary (see Console/PuzzleJsonSchema.md).
/// The text summaries are deliberately NOT read: their column layout has drifted across
/// many releases, while the JSON is a versioned contract.
module PuzzleTrend =

    /// A checkpoint identified from a net name, e.g.
    ///   a4000-21bn11_srv_256_10_smol_t91_1B_700012544     -> arm "..._t91_1B",      step 700
    ///   a4000-21bn11_srv_256_10_smol_t91_1B_r600_700014592 -> arm "..._t91_1B_r600", step 700
    ///   a4000-21bn11_srv_512_16_t91_1B_700026880ema        -> arm "..._t91_1B",      step 700, EMA
    type NetId =
        { Arm: string
          StepM: int
          IsEma: bool }

    /// One measured cell: an arm at a step, for one test type and rating group.
    type Point =
        { Arm: string
          StepM: int
          IsEma: bool
          Type: string
          RatingGroup: int
          Accuracy: float
          Kld: float
          Perf: float
          Total: int
          StartedUtc: string
          SourceFile: string }

    /// Checkpoint counters are large; anything shorter is a net name that merely ends in a
    /// digit (e.g. "C1-640-34-I8"), which is not a training checkpoint.
    let private minStepPositions = 500_000L

    /// Splits a net name into arm + step. Returns None for names without a checkpoint counter.
    let parseNetName (rawName: string) : NetId option =
        if String.IsNullOrWhiteSpace rawName then None
        else
            // nets are sometimes recorded as full paths, sometimes as bare names
            let name = Path.GetFileNameWithoutExtension(rawName.Replace('\\', '/'))
            let isEma = name.EndsWith("ema", StringComparison.OrdinalIgnoreCase)
            let core = if isEma then name.Substring(0, name.Length - 3) else name
            let mutable cut = core.Length
            while cut > 0 && Char.IsDigit core.[cut - 1] do
                cut <- cut - 1
            let digits = core.Substring(cut)
            if digits.Length = 0 then None
            else
                match Int64.TryParse digits with
                | true, n when n >= minStepPositions ->
                    // arms disagree on batch size (100003840 vs 100001792 is the same 100M
                    // milestone), so bucket to whole millions
                    let stepM = int (Math.Round(float n / 1_000_000.0))
                    let arm = core.Substring(0, cut).TrimEnd('_', '-')
                    if String.IsNullOrWhiteSpace arm then None
                    else Some { Arm = arm; StepM = stepM; IsEma = isEma }
                | _ -> None

    /// Rating groups are reported as the average rating of the sampled puzzles
    /// (2499, 2693, ...), so snap back to the configured group.
    let ratingGroupOf (ratingAvg: float) = int (Math.Round(ratingAvg / 100.0)) * 100

    let private tryProp (el: JsonElement) (name: string) =
        match el.TryGetProperty name with
        | true, v -> Some v
        | _ -> None

    let private strProp el name =
        tryProp el name
        |> Option.bind (fun v -> if v.ValueKind = JsonValueKind.String then Some (v.GetString()) else None)
        |> Option.defaultValue ""

    let private numProp el name =
        tryProp el name
        |> Option.bind (fun v -> if v.ValueKind = JsonValueKind.Number then Some (v.GetDouble()) else None)
        |> Option.defaultValue 0.0

    /// Reads one result JSON. Rows whose net has no checkpoint counter are skipped.
    let loadFile (path: string) : Point list =
        try
            use doc = JsonDocument.Parse(File.ReadAllText path)
            let root = doc.RootElement
            let started = strProp root "startedUtc"
            match tryProp root "scores" with
            | Some scores when scores.ValueKind = JsonValueKind.Array ->
                [ for s in scores.EnumerateArray() do
                    match parseNetName (strProp s "neuralNet") with
                    | Some net ->
                        yield { Arm = net.Arm
                                StepM = net.StepM
                                IsEma = net.IsEma
                                Type = strProp s "type"
                                RatingGroup = ratingGroupOf (numProp s "ratingAvg")
                                Accuracy = numProp s "accuracy"
                                Kld = numProp s "avgKLD"
                                Perf = numProp s "playerRating"
                                Total = int (numProp s "totalNumber")
                                StartedUtc = started
                                SourceFile = Path.GetFileName path }
                    | None -> () ]
            | _ -> []
        with _ -> []

    /// Reads every result JSON in a folder.
    let loadFolder (dir: string) : Point list =
        if not (Directory.Exists dir) then []
        else
            Directory.GetFiles(dir, "*.json")
            |> Array.sort
            |> Array.toList
            |> List.collect loadFile

    /// A net measured in several runs keeps its most informative reading: the largest sample
    /// first, then the most recent. Sample size leads because a quick 50-puzzle smoke test
    /// must never displace a 2000-puzzle measurement of the same checkpoint.
    /// Deterministic when both tie: the later filename wins.
    let dedupe (points: Point list) : Point list =
        points
        |> List.groupBy (fun p -> p.Arm, p.StepM, p.IsEma, p.Type, p.RatingGroup)
        |> List.map (fun (_, group) ->
            group |> List.maxBy (fun p -> p.Total, p.StartedUtc, p.SourceFile))

    /// A single measured checkpoint is a data point, not a curve. Most nets ever tested are
    /// one-offs that merely happen to carry a step counter, and they swamp the real training
    /// runs (180 arms in the archive, but only a handful have a curve worth plotting).
    /// Keeps series with at least `minSteps` distinct steps; also returns how many series
    /// were dropped so callers can say so rather than silently truncating.
    let filterMinSteps (minSteps: int) (points: Point list) : Point list * int =
        if minSteps <= 1 then points, 0
        else
            let bySeries =
                points |> List.groupBy (fun p -> p.Arm, p.IsEma, p.Type, p.RatingGroup)
            let kept, dropped =
                bySeries
                |> List.partition (fun (_, group) ->
                    group |> List.map (fun p -> p.StepM) |> List.distinct |> List.length >= minSteps)
            (kept |> List.collect snd), dropped.Length

    let private variantLabel (isEma: bool) = if isEma then "ema" else "raw"

    /// Renders one block per (type, rating group): steps down, arm/variant across.
    /// Accuracy is shown as a percentage, matching the puzzle summary tables.
    let render (points: Point list) : string =
        if List.isEmpty points then "Ingen checkpoint-resultater funnet."
        else
            let sb = StringBuilder()
            let blocks =
                points
                |> List.groupBy (fun p -> p.Type, p.RatingGroup)
                |> List.sortBy (fun ((t, rg), _) -> t, rg)

            for (testType, ratingGroup), rows in blocks do
                let columns =
                    rows
                    |> List.map (fun p -> p.Arm, p.IsEma)
                    |> List.distinct
                    |> List.sortBy (fun (arm, ema) -> arm, ema)
                let steps = rows |> List.map (fun p -> p.StepM) |> List.distinct |> List.sort
                let lookup =
                    rows
                    |> List.map (fun p -> (p.Arm, p.IsEma, p.StepM), p)
                    |> dict

                // long arm names would dominate the table; keep the distinguishing tail
                let shortArm (arm: string) =
                    if arm.Length <= 28 then arm else "…" + arm.Substring(arm.Length - 27)
                let headers =
                    columns
                    |> List.map (fun (arm, ema) -> sprintf "%s [%s]" (shortArm arm) (variantLabel ema))
                let widths = headers |> List.map (fun h -> max 10 h.Length)

                sb.AppendLine() |> ignore
                sb.AppendLine(sprintf "%s  —  rating group %d" testType ratingGroup) |> ignore
                sb.Append(sprintf "%-8s" "step") |> ignore
                List.iter2 (fun (h: string) w -> sb.Append(sprintf "  %*s" w h) |> ignore) headers widths
                sb.AppendLine() |> ignore
                sb.AppendLine(String('-', 8 + (widths |> List.sumBy (fun w -> w + 2)))) |> ignore

                for step in steps do
                    sb.Append(sprintf "%-8s" (sprintf "%dM" step)) |> ignore
                    List.iter2
                        (fun (arm, ema) w ->
                            let cell =
                                match lookup.TryGetValue((arm, ema, step)) with
                                | true, p -> sprintf "%.1f" (p.Accuracy * 100.0)
                                | _ -> "-"
                            sb.Append(sprintf "  %*s" w cell) |> ignore)
                        columns widths
                    sb.AppendLine() |> ignore

            sb.ToString()

    /// Flat CSV of every cell, for spreadsheets or plotting.
    let toCsv (points: Point list) : string =
        let sb = StringBuilder()
        sb.AppendLine("arm,step_millions,variant,type,rating_group,accuracy_pct,kld,perf,total,started_utc,source_file") |> ignore
        for p in points |> List.sortBy (fun p -> p.Type, p.RatingGroup, p.Arm, p.StepM, p.IsEma) do
            sb.AppendLine(
                String.Format(
                    Globalization.CultureInfo.InvariantCulture,
                    "{0},{1},{2},{3},{4},{5:F2},{6:F4},{7:F0},{8},{9},{10}",
                    p.Arm, p.StepM, variantLabel p.IsEma, p.Type, p.RatingGroup,
                    p.Accuracy * 100.0, p.Kld, p.Perf, p.Total, p.StartedUtc, p.SourceFile)) |> ignore
        sb.ToString()
