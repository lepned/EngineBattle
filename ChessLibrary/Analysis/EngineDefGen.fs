namespace ChessLibrary

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

/// Generates engine def JSON for training checkpoints that do not have one yet.
///
/// A training run drops a new .onnx every few hours, and each needs a def that differs
/// from its predecessor only in the step number: the net file, the display names, and the
/// def's own file name. Doing that by hand is how step numbers get mistyped.
///
/// An existing def IS the template - its file name carries the naming convention
/// (`<prefix>_<step>M[_ema].json`), and its content carries everything else. So there is no
/// separate template format to keep in sync with the defs.
module EngineDefGen =

    /// A checkpoint that has no def yet, and the def that would be written for it.
    type Planned =
        { NetFile: string
          DefPath: string
          StepM: int }

    type Plan =
        { TemplatePath: string
          TemplateStepM: int
          IsEma: bool
          ArmKey: string
          NetFolder: string
          OutFolder: string
          /// Checkpoints needing a def, oldest first.
          ToWrite: Planned list
          /// Steps already covered by some def in the out folder.
          Covered: int list
          /// Nets rejected, with the reason - surfaced so a silent no-op is explainable.
          Skipped: (string * string) list }

    /// Trailing `_<digits>M` or `_<digits>M_ema` on a def's file name.
    let private stepLabelRe =
        Regex(@"_(\d+)M(_ema)?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    let private netRefRe =
        Regex(@"[A-Za-z0-9_.\-]+\.onnx", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    /// The server writes the same arm under different machine prefixes between runs
    /// (`lepdev_srv_512_16_deg4_t91_1B` and `a4000-21bn11_srv_512_16_deg4_t91_1B` are one
    /// arm). Matching on the part from `srv` onward keeps a renamed host from looking like
    /// a new arm. Arms without that marker are returned unchanged.
    let armKey (arm: string) =
        if String.IsNullOrWhiteSpace arm then ""
        else
            let idx = arm.IndexOf("srv_", StringComparison.OrdinalIgnoreCase)
            if idx > 0 then arm.Substring idx else arm

    /// Splits a def file name into the part before the step and the step itself.
    /// `Ceres_srv_512_16_deg4_t91_100M.json` -> ("Ceres_srv_512_16_deg4_t91", 100, false)
    let parseDefName (path: string) =
        let stem = Path.GetFileNameWithoutExtension(path.Replace('\\', '/'))
        let m = stepLabelRe.Match stem
        if not m.Success then None
        else
            match Int32.TryParse m.Groups.[1].Value with
            | true, step ->
                let prefix = stem.Substring(0, m.Index)
                let isEma = m.Groups.[2].Success
                if String.IsNullOrWhiteSpace prefix then None
                else Some (prefix, step, isEma)
            | _ -> None

    /// Every .onnx file name mentioned by any def in the folder. Read as text rather than
    /// parsed: the folder holds hand-edited defs, and one malformed file must not stop the
    /// scan.
    let private referencedNets (outFolder: string) =
        let seen = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        if Directory.Exists outFolder then
            for file in Directory.EnumerateFiles(outFolder, "*.json") do
                try
                    for m in netRefRe.Matches(File.ReadAllText file) do
                        seen.Add m.Value |> ignore
                with _ -> ()
        seen

    /// Reads the net path a def points at.
    let private netPathOf (defText: string) =
        let m = Regex.Match(defText, "\"Network\"\\s*:\\s*\"([^\"]+)\"", RegexOptions.IgnoreCase)
        if m.Success then Some (m.Groups.[1].Value) else None

    /// Produces the def text for a new checkpoint by substituting into the template.
    ///
    /// Two substitutions, both anchored: the net file name (unique in the template) and the
    /// step label, which is matched with a digit guard so `100M` inside `1100M` survives.
    let renderDef (templateText: string) (templateNetFile: string) (templateStepM: int) (netFile: string) (stepM: int) =
        let withNet = templateText.Replace(templateNetFile, netFile)
        let stepRe = Regex(sprintf @"(?<![0-9])%dM" templateStepM)
        stepRe.Replace(withNet, sprintf "%dM" stepM)

    /// Works out which checkpoints still need a def.
    ///
    /// `netsOverride` is either a folder to scan or a single .onnx to generate one def for;
    /// omitted, it is the folder the template's own Network points at.
    let plan (templatePath: string) (netsOverride: string option) (outFolderOverride: string option) =
        if not (File.Exists templatePath) then
            // a bare file name only resolves against the working directory, which is rarely
            // where defs live - say so rather than leaving the user to guess
            let hint =
                if String.IsNullOrEmpty(Path.GetDirectoryName templatePath)
                then " (give the full path - a bare name is looked up in the current directory)"
                else ""
            Error (sprintf "Template def not found: %s%s" templatePath hint)
        else
            match parseDefName templatePath with
            | None ->
                Error (sprintf
                        "Template file name must end in _<step>M or _<step>M_ema so the naming convention can be read from it: %s"
                        (Path.GetFileName templatePath))
            | Some (prefix, templateStep, isEma) ->
                let templateText = File.ReadAllText templatePath
                match netPathOf templateText with
                | None -> Error (sprintf "Template has no Options.Network entry: %s" templatePath)
                | Some templateNetPath ->
                    let templateNetFile = Path.GetFileName(templateNetPath.Replace('\\', '/'))
                    match PuzzleTrend.parseNetName templateNetFile with
                    | None ->
                        Error (sprintf "Cannot read an arm and step from the template's net name: %s" templateNetFile)
                    | Some templateNet ->
                        // a single .onnx generates one def; anything else is a folder to scan
                        let singleNet =
                            match netsOverride with
                            | Some f when f.EndsWith(".onnx", StringComparison.OrdinalIgnoreCase) -> Some f
                            | _ -> None
                        let netFolder =
                            match singleNet, netsOverride with
                            | Some f, _ -> Path.GetDirectoryName(Path.GetFullPath f)
                            | None, Some f -> f
                            | None, None -> Path.GetDirectoryName(templateNetPath.Replace('\\', '/'))
                        let outFolder =
                            match outFolderOverride with
                            | Some f -> f
                            | None -> Path.GetDirectoryName(Path.GetFullPath templatePath)
                        if singleNet.IsSome && not (File.Exists singleNet.Value) then
                            Error (sprintf "Net file not found: %s" singleNet.Value)
                        elif not (Directory.Exists netFolder) then
                            Error (sprintf "Net folder not found: %s" netFolder)
                        else
                            let wantedArm = armKey templateNet.Arm
                            let covered = referencedNets outFolder
                            let skipped = ResizeArray<string * string>()
                            let candidates = ResizeArray<Planned>()
                            let coveredSteps = HashSet<int>()

                            let netPaths =
                                match singleNet with
                                | Some f -> Seq.singleton (Path.GetFullPath f)
                                | None -> Directory.EnumerateFiles(netFolder, "*.onnx")

                            for netPath in netPaths do
                                let netFile = Path.GetFileName netPath
                                match PuzzleTrend.parseNetName netFile with
                                | None -> skipped.Add(netFile, "no checkpoint counter in the name")
                                | Some net ->
                                    if armKey net.Arm <> wantedArm then ()          // another arm entirely
                                    elif net.IsEma <> isEma then ()                 // wrong variant for this template
                                    elif covered.Contains netFile then
                                        coveredSteps.Add net.StepM |> ignore
                                    else
                                        candidates.Add
                                            { NetFile = netPath.Replace('\\', '/')
                                              DefPath =
                                                Path.Combine(
                                                    outFolder,
                                                    sprintf "%s_%dM%s.json" prefix net.StepM (if isEma then "_ema" else ""))
                                                    .Replace('\\', '/')
                                              StepM = net.StepM }

                            // two nets bucketing to one step would collide on the def name; keep
                            // the higher counter (the later write) and say which ones lost, so the
                            // drop is never silent
                            let deduped =
                                candidates
                                |> Seq.groupBy (fun c -> c.StepM)
                                |> Seq.map (fun (_, g) ->
                                    let ordered = g |> Seq.sortByDescending (fun c -> c.NetFile) |> List.ofSeq
                                    for loser in List.tail ordered do
                                        skipped.Add(
                                            Path.GetFileName loser.NetFile,
                                            sprintf "another net already claims %dM in this arm" loser.StepM)
                                    List.head ordered)
                                |> Seq.sortBy (fun c -> c.StepM)
                                |> List.ofSeq

                            // naming a net explicitly is an instruction, so a mismatch against the
                            // template must be reported, not swallowed as "nothing new"
                            match singleNet with
                            | Some f when deduped.IsEmpty && coveredSteps.Count = 0 ->
                                let netFile = Path.GetFileName f
                                match PuzzleTrend.parseNetName netFile with
                                | None ->
                                    Error (sprintf "No checkpoint counter in the net name, so there is no step to write: %s" netFile)
                                | Some net when armKey net.Arm <> wantedArm ->
                                    Error (sprintf
                                            "Net belongs to arm '%s' but the template is for '%s' - the generated def would carry the wrong name."
                                            (armKey net.Arm) wantedArm)
                                | Some net ->
                                    Error (sprintf
                                            "Net is the %s variant but the template is %s - use the matching template."
                                            (if net.IsEma then "EMA" else "raw")
                                            (if isEma then "EMA" else "raw"))
                            | _ ->

                            Ok { TemplatePath = templatePath
                                 TemplateStepM = templateStep
                                 IsEma = isEma
                                 ArmKey = wantedArm
                                 NetFolder = netFolder.Replace('\\', '/')
                                 OutFolder = outFolder.Replace('\\', '/')
                                 ToWrite = deduped
                                 Covered = coveredSteps |> Seq.sort |> List.ofSeq
                                 Skipped = List.ofSeq skipped }

    /// Writes the planned defs. Returns the paths written.
    /// Existing files are never touched unless `force` is set.
    let write (force: bool) (p: Plan) =
        let templateText = File.ReadAllText p.TemplatePath
        let templateNetFile =
            netPathOf templateText
            |> Option.map (fun s -> Path.GetFileName(s.Replace('\\', '/')))
            |> Option.defaultValue ""
        [ for item in p.ToWrite do
            if force || not (File.Exists item.DefPath) then
                let text = renderDef templateText templateNetFile p.TemplateStepM (Path.GetFileName item.NetFile) item.StepM
                File.WriteAllText(item.DefPath, text)
                yield item.DefPath ]

    let render (p: Plan) =
        let sb = Text.StringBuilder()
        let line fmt = Printf.kprintf (fun s -> sb.AppendLine s |> ignore) fmt
        line "--- Engine Def Generator ---"
        line "  Template : %s  (%dM, %s)" (Path.GetFileName p.TemplatePath) p.TemplateStepM (if p.IsEma then "EMA" else "raw")
        line "  Arm      : %s" p.ArmKey
        line "  Nets     : %s" p.NetFolder
        line "  Out      : %s" p.OutFolder
        line ""
        if not p.Covered.IsEmpty then
            let steps = p.Covered |> List.map (sprintf "%dM") |> String.concat " "
            line "  Already have defs (%d): %s" p.Covered.Length steps
        if p.ToWrite.IsEmpty then
            line "  Nothing new - every checkpoint of this arm already has a def."
        else
            line "  New checkpoints (%d):" p.ToWrite.Length
            for item in p.ToWrite do
                line "    %5dM  %s" item.StepM (Path.GetFileName item.DefPath)
        // a collision means a net was dropped; never let that pass unmentioned
        let collisions = p.Skipped |> List.filter (fun (_, why) -> why.Contains "already claims")
        if not collisions.IsEmpty then
            line ""
            line "  Dropped - two nets map to one def name (%d):" collisions.Length
            for (net, why) in collisions do
                line "    %s  (%s)" net why
        let unparsed = p.Skipped.Length - collisions.Length
        if unparsed > 0 then
            line ""
            line "  Ignored %d file(s) in the net folder with no checkpoint counter in the name." unparsed
        sb.ToString()
