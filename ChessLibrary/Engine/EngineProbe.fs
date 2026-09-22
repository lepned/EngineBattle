namespace ChessLibrary

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open ChessLibrary.TypesDef.CoreTypes

/// Makes an engine def from a running engine: what `uci` answers is what the def gets.
///
/// The Engine creator page and the `mkdef` verb are two fronts on this one module. The probe
/// starts the engine, waits for `uciok`, and stops it again; `build` turns the answer plus the
/// caller's wishes (a base def, a network, tablebases, option overrides) into an EngineConfig;
/// `toJson` and `write` put it on disk in the shape `readSingleEngineConfig` validates.
/// Nothing here matches on an engine's name - a network option is one named like a network
/// option, whichever engine reports it.
module EngineProbe =

    /// What the engine said about itself.
    type Probed =
        { Path: string
          /// `id name`, or the file name when the engine gave none.
          Name: string
          Author: string
          /// Every option with its default. Buttons have none and are left out.
          Defaults: Dictionary<string, obj>
          Options: Dictionary<string, UciOption.UciOption> }

    /// What the caller wants on top of the defaults. Everything is optional.
    type Wishes =
        { /// An existing def: its option values and top-level fields win over the probe, for
          /// the options the engine still has.
          Base: EngineConfig option
          /// Goes into the engine's own network option, and its folder into NetworkPath.
          NetFile: string option
          /// NetworkPath when no NetFile is given (the GUI's neural-net folder setting).
          NetFolder: string option
          /// SyzygyPath, for an engine that has one and does not already name a folder.
          Tablebases: string option
          /// setoption name/value pairs. Each must be an option the engine reported.
          Overrides: (string * string) list }
        static member Empty =
            { Base = None; NetFile = None; NetFolder = None; Tablebases = None; Overrides = [] }

    let private forward (path: string) = path.Replace('\\', '/')

    /// "Stockfish 19" -> "Stockfish", "Lc0 v0.31.2" -> "Lc0": the name up to the first token
    /// that starts like a version number.
    let stripVersion (name: string) =
        let parts = name.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        let kept = parts |> Array.takeWhile (fun p -> not (Regex.IsMatch(p, @"^v?\d")))
        if kept.Length > 0 then String.Join(" ", kept) else name

    /// The string option a network file goes into, if the engine has one: Lc0's WeightsFile,
    /// Ceres' Network, or anything else named like one. Matched on the option, not the engine.
    let networkOptionKey (options: Dictionary<string, UciOption.UciOption>) =
        options.Values
        |> Seq.tryFind (fun o ->
            match o.OptionType with
            | UciOption.String _ ->
                let n = o.Name.ToLowerInvariant()
                n.Contains "weights" || n.Contains "network"
            | _ -> false)
        |> Option.map (fun o -> o.Name)

    /// Starts the engine, reads its `uci` answer, stops it. An engine that never says `uciok`
    /// (not a UCI engine, or one waiting for input) is stopped when the timeout runs out.
    let probe (exePath: string) (timeoutMs: int) : Result<Probed, string> =
        let path = forward exePath
        if not (File.Exists path) then Error (sprintf "Engine not found: %s" path)
        else
            let engine = EngineHelper.createEngine (EngineConfig.EmptyWithPath path, None)
            let started = System.Threading.Tasks.Task.Run(fun () -> engine.StartProcess())
            let answered = try started.Wait timeoutMs with _ -> false
            (try engine.StopProcess() with _ -> ())
            if not answered then
                Error (sprintf "%s did not answer 'uci' within %d s: not a UCI engine, or it is waiting for something (a missing network, a console prompt)" path (timeoutMs / 1000))
            elif started.IsFaulted then
                Error (sprintf "%s failed to start: %s" path started.Exception.InnerException.Message)
            else
                let options = engine.GetOptionsMap()
                let idValue key =
                    match options.TryGetValue key with
                    | true, o ->
                        match o.OptionType with
                        | UciOption.IdAndAuthor (_, _, v) when not (String.IsNullOrWhiteSpace v) -> Some v
                        | _ -> None
                    | _ -> None
                if options.Count = 0 then
                    Error (sprintf "%s answered 'uci' with no options at all - is it a UCI engine?" path)
                else
                    Ok { Path = path
                         Name = idValue "name" |> Option.defaultValue (Path.GetFileNameWithoutExtension path)
                         Author = idValue "author" |> Option.defaultValue ""
                         Defaults = engine.GetDefaultOptions()
                         Options = options }

    let private typedValue (options: Dictionary<string, UciOption.UciOption>) (name: string) (v: string) : obj =
        match options.TryGetValue name with
        | true, o ->
            match o.OptionType with
            | UciOption.Check _ -> (match Boolean.TryParse v with | true, b -> box b | _ -> box v)
            | UciOption.Spin _ -> (match Int64.TryParse v with | true, n -> box n | _ -> box v)
            | _ -> box v
        | _ -> box v

    /// The def: the probe's defaults with the wishes laid over them. Refuses an override for an
    /// option the engine does not have, and a network for an engine with no network option,
    /// rather than writing a def that would fail at the first `setoption`.
    let build (probed: Probed) (wishes: Wishes) : Result<EngineConfig, string> =
        let options = Dictionary<string, obj>(StringComparer.OrdinalIgnoreCase)
        for kv in probed.Defaults do options.[kv.Key] <- kv.Value
        let has key = options.ContainsKey key

        // An option the engine no longer reports is dropped, not carried over blindly.
        match wishes.Base with
        | Some b when not (isNull b.Options) ->
            for kv in b.Options do
                if has kv.Key then options.[kv.Key] <- kv.Value
        | _ -> ()

        // Tablebases: only where nothing names a folder yet. Stockfish reports "<empty>" as the
        // default, which is not a folder either.
        if has "SyzygyPath" then
            let current = string options.["SyzygyPath"]
            if String.IsNullOrWhiteSpace current || current = "<empty>" then
                match wishes.Tablebases with
                | Some tb when not (String.IsNullOrWhiteSpace tb) -> options.["SyzygyPath"] <- box (forward tb)
                | _ -> options.Remove "SyzygyPath" |> ignore

        // The flags EngineBattle reads from the info stream: on, for an engine that has them.
        for key in [ "LogLiveStats"; "VerboseMoveStats"; "UCI_ShowWDL" ] do
            if has key then options.[key] <- box true

        let netKey = networkOptionKey probed.Options
        let netProblem =
            match wishes.NetFile with
            | Some f ->
                match netKey with
                | Some k -> options.[k] <- box (forward f); None
                | None -> Some (sprintf "%s has no network option (a string option named like WeightsFile or Network), so --net does not apply" probed.Name)
            | None -> None

        let unknown =
            wishes.Overrides
            |> List.map fst
            |> List.filter (fun k -> not (probed.Options.ContainsKey k))
        for (k, v) in wishes.Overrides do
            if probed.Options.ContainsKey k then options.[k] <- typedValue probed.Options k v

        match netProblem, unknown with
        | Some msg, _ -> Error msg
        | None, _ :: _ -> Error (sprintf "%s has no option called %s (options are case-insensitive but must exist)" probed.Name (String.Join(", ", unknown)))
        | None, [] ->
            let b = wishes.Base
            let fromBase pick fallback =
                match b with
                | Some b when not (String.IsNullOrWhiteSpace (pick b)) -> pick b
                | _ -> fallback
            let networkPath =
                match wishes.NetFile with
                // Separators first: on Linux a backslash is not one, and GetDirectoryName of a
                // Windows-style path would come back empty.
                | Some f -> forward (Path.GetDirectoryName (forward f))
                | None ->
                    match b with
                    | Some b when not (String.IsNullOrWhiteSpace b.NetworkPath) -> b.NetworkPath
                    | _ -> wishes.NetFolder |> Option.map forward |> Option.defaultValue ""
            // A logo by what the engine has, not what it is called.
            let logo =
                match netKey with
                | Some k when k.Equals("WeightsFile", StringComparison.OrdinalIgnoreCase) -> "Img/lc0.png"
                | Some k when k.Equals("Network", StringComparison.OrdinalIgnoreCase) -> "Img/CeresLogo.png"
                | _ -> "Img/EngineBattle.png"
            let empty = EngineConfig.EmptyWithPath probed.Path
            Ok { empty with
                   Name = probed.Name
                   Alias = fromBase (fun b -> b.Alias) (stripVersion probed.Name)
                   Version = probed.Name
                   Dev = probed.Author
                   Rating = (match b with Some b -> b.Rating | None -> empty.Rating)
                   LogoPath = fromBase (fun b -> b.LogoPath) logo
                   Args = fromBase (fun b -> b.Args) empty.Args
                   ContemptEnabled = (match b with Some b -> b.ContemptEnabled | None -> false)
                   NegativeContemptAllowed = (match b with Some b -> b.NegativeContemptAllowed | None -> false)
                   NetworkPath = networkPath
                   Options = options }

    /// The GUI's call: nulls for what it does not have. Same build, no F# types to construct
    /// from C#.
    let buildForGui (probed: Probed) (baseDef: EngineConfig) (netFolder: string) (tablebases: string) =
        let opt (s: string) = if String.IsNullOrWhiteSpace s then None else Some s
        build probed { Wishes.Empty with
                         Base = (if isNull (box baseDef) then None else Some baseDef)
                         NetFolder = opt netFolder
                         Tablebases = opt tablebases }

    /// The def as a file: the fields a def carries, indented. The run-time fields (challenger
    /// flag, device slot, Winboard block) are not part of a UCI def.
    let toJson (config: EngineConfig) =
        let obj = JsonSerializer.SerializeToNode(config).AsObject()
        for key in [ "IsChallenger"; "WinboardConfig"; "DeviceOption"; "DeviceTemplate" ] do
            obj.Remove key |> ignore
        // Stockfish's "<empty>" default would otherwise be written as "<empty>".
        let options = JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
        obj.ToJsonString(options)

    /// "Stockfish 19" -> Stockfish19.json: the name without spaces or anything a file system
    /// objects to.
    let defFileName (config: EngineConfig) =
        let cleaned = Regex.Replace(config.Name, @"[^\w.\-]", "")
        (if cleaned.Length > 0 then cleaned else "engine") + ".json"

    /// Writes the def into the folder. An existing file is never overwritten unless asked.
    let write (folder: string) (config: EngineConfig) (force: bool) : Result<string, string> =
        let dir = Path.GetFullPath(if String.IsNullOrWhiteSpace folder then Directory.GetCurrentDirectory() else folder)
        if not (Directory.Exists dir) then Error (sprintf "Folder does not exist: %s" (forward dir))
        else
            let path = Path.Combine(dir, defFileName config)
            if File.Exists path && not force then Error (sprintf "%s exists - use --force to overwrite it" (forward path))
            else
                File.WriteAllText(path, toJson config)
                Ok (forward path)
