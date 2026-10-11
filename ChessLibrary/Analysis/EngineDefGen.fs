namespace ChessLibrary

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions

/// A copy of an existing def with another network (`engine new <def> --net FILE`): the same
/// engine, options and settings, the network option and NetworkPath pointing at the new net.
///
/// The text is edited, not re-serialised, so a hand-edited def keeps its layout and every field.
/// A def named `<prefix>_<step>M[_ema].json` for a net with a checkpoint counter (a training run's
/// checkpoints) gets the new step in its file name and wherever the old step label stood (name,
/// alias); any other def gets the net's name.
module EngineDefGen =

    /// The def that would be written.
    type NewDef =
        { DefPath: string
          Name: string
          Text: string }

    /// Trailing `_<digits>M` or `_<digits>M_ema` on a def's file name.
    let private stepLabelRe =
        Regex(@"_(\d+)M(_ema)?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    /// One arm whatever host wrote it (PuzzleTrend.armKey).
    let armKey (arm: string) = PuzzleTrend.armKey arm

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

    /// The option a def keeps its network in - Lc0's WeightsFile, Ceres' Network, or any string
    /// option named like one (as EngineProbe.networkOptionKey) - and its value.
    let networkOption (defText: string) =
        match (try JsonNode.Parse defText with _ -> null) with
        | :? JsonObject as def ->
            match def.["Options"] with
            | :? JsonObject as options ->
                options
                |> Seq.tryPick (fun kv ->
                    let n = kv.Key.ToLowerInvariant()
                    match kv.Value with
                    | :? JsonValue as v when (n.Contains "weights" || n.Contains "network") && v.GetValueKind() = JsonValueKind.String ->
                        Some (kv.Key, v.GetValue<string>())
                    | _ -> None)
            | _ -> None
        | _ -> None

    // a path or a name as written (C:/Nett/Bjørn, not Bj\u00F8rn), as EngineProbe.toJson
    let private relaxed = JsonSerializerOptions(Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

    /// The string value of `key` in the text (the first one; Name before the Options), replaced.
    let private setString (key: string) (value: string) (text: string) =
        let re = Regex(sprintf "(\"%s\"\\s*:\\s*)\"(?:[^\"\\\\]|\\\\.)*\"" (Regex.Escape key))
        re.Replace(text, (fun (m: Match) -> m.Groups.[1].Value + JsonSerializer.Serialize(value, relaxed)), 1)

    let private stringOf (key: string) (text: string) =
        match (try JsonNode.Parse text with _ -> null) with
        | :? JsonObject as def ->
            match def.[key] with
            | :? JsonValue as v when v.GetValueKind() = JsonValueKind.String -> Some (v.GetValue<string>())
            | _ -> None
        | _ -> None

    /// A net's name without its extensions: BT4-332.pb.gz -> BT4-332.
    let private netStem (path: string) =
        let mutable name = Path.GetFileName(path.Replace('\\', '/'))
        for ext in [ ".gz"; ".onnx"; ".pb" ] do
            if name.EndsWith(ext, StringComparison.OrdinalIgnoreCase) then name <- name.Substring(0, name.Length - ext.Length)
        name

    /// "Stockfish 19" -> Stockfish19: what a file system takes (as EngineProbe.defFileName).
    let private fileStem (name: string) =
        let cleaned = Regex.Replace(name, @"[^\w.\-]", "")
        if cleaned.Length > 0 then cleaned else "engine"

    let private forward (path: string) = path.Replace('\\', '/')

    /// The def `defPath` with the network `netPath`, written into `outFolder` (default: the def's
    /// own folder) and named `name` when given.
    let fromDef (defPath: string) (netPath: string) (name: string option) (outFolder: string option) : Result<NewDef, string> =
        let text = File.ReadAllText defPath
        match networkOption text with
        | None when (try JsonNode.Parse text :? JsonObject with _ -> false) |> not ->
            Error (sprintf "%s is not a def: not a JSON object" (Path.GetFileName defPath))
        | None ->
            Error (sprintf "%s has no network option (a string option named like WeightsFile or Network in its Options), so --net does not apply" (Path.GetFileName defPath))
        | Some (key, oldNet) ->
            let net = forward (Path.GetFullPath netPath)
            let withNet =
                let t = setString key net text
                // the GUI's neural-net folder, where there is one
                if (stringOf "NetworkPath" text).IsSome then setString "NetworkPath" (forward (Path.GetDirectoryName net)) t else t
            let folder = forward (defaultArg outFolder (Path.GetDirectoryName(Path.GetFullPath defPath)))
            // a training checkpoint: the step label moves on, the rest of the name stays
            let checkpoint =
                match parseDefName defPath, PuzzleTrend.parseNetName oldNet, PuzzleTrend.parseNetName net with
                | Some (prefix, step, isEma), Some old, Some next -> Some (prefix, step, isEma, old, next)
                | _ -> None
            let named (newName: string) (t: string) = setString "Name" newName t
            match checkpoint, name with
            | Some (_, _, isEma, _, next), None when next.IsEma <> isEma ->
                Error (sprintf "the net is the %s variant and the def %s - copy a def of the same variant"
                        (if next.IsEma then "EMA" else "raw") (if isEma then "EMA" else "raw"))
            | Some (_, _, _, old, next), None when armKey old.Arm <> armKey next.Arm ->
                Error (sprintf "the net is of arm '%s' and the def of '%s' - its name would be wrong (--name gives it another)"
                        (armKey next.Arm) (armKey old.Arm))
            | Some (prefix, step, isEma, _, next), None ->
                // the step label is matched with a digit guard, so 100M inside 1100M stays
                let relabelled = Regex(sprintf @"(?<![0-9])%dM" step).Replace(withNet, sprintf "%dM" next.StepM)
                Ok { DefPath = forward (Path.Combine(folder, sprintf "%s_%dM%s.json" prefix next.StepM (if isEma then "_ema" else "")))
                     Name = defaultArg (stringOf "Name" relabelled) ""
                     Text = relabelled }
            | _ ->
                let newName = defaultArg name (netStem net)
                Ok { DefPath = forward (Path.Combine(folder, fileStem newName + ".json"))
                     Name = newName
                     Text = named newName withNet }
