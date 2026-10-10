/// eb-cli's own settings: eb-cli.json next to the executable, every key optional. They save typing
/// (folders); none of them changes what a command computes. Not the match verb's config.json, which
/// is one match's state in fastchess's format.
module CliSettings

open System
open System.IO
open System.Text.Json

let fileName = "eb-cli.json"

type Settings =
    { /// EB engine defs: a bare engine name (analyze, compare, bookeval, enginecheck, the piece-value
      /// verbs) is looked up here as <name> or <name>.json.
      EngineDefsFolder: string option
      /// Syzygy folders for `tb` (several separated by ';' on Windows, ':' elsewhere).
      SyzygyPath: string option
      /// The folder `puzzletrend` reads when none is given.
      PuzzleResultsFolder: string option }

let empty = { EngineDefsFolder = None; SyzygyPath = None; PuzzleResultsFolder = None }

let private known = [ "engineDefsFolder"; "syzygyPath"; "puzzleResultsFolder" ]

/// The settings in a JSON text, with a warning for each key it does not know (a misspelt key is
/// said, not ignored); Error for text that is not a JSON object or a value of the wrong type. Blank
/// text is no settings.
let parse (json: string) : Result<Settings * string list, string> =
    if String.IsNullOrWhiteSpace json then Ok(empty, [])
    else
        try
            let options = JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)
            use doc = JsonDocument.Parse(json, options)
            let root = doc.RootElement
            if root.ValueKind <> JsonValueKind.Object then Error "not a JSON object"
            else
                // keys in any case: EB's other JSON (engine defs, tournaments) is PascalCase
                let isKnown (name: string) = known |> List.exists (fun k -> String.Equals(k, name, StringComparison.OrdinalIgnoreCase))
                let warnings =
                    [ for p in root.EnumerateObject() do
                          if not (isKnown p.Name) then
                              yield $"""unknown key '{p.Name}' (known: {String.Join(", ", known)})""" ]
                let find (key: string) =
                    root.EnumerateObject()
                    |> Seq.tryFind (fun p -> String.Equals(p.Name, key, StringComparison.OrdinalIgnoreCase))
                    |> Option.map (fun p -> p.Value)
                let text (key: string) =
                    match find key with
                    | Some v when v.ValueKind = JsonValueKind.String ->
                        let s = v.GetString()
                        Ok(if String.IsNullOrWhiteSpace s then None else Some s)
                    | Some v when v.ValueKind = JsonValueKind.Null -> Ok None
                    | Some _ -> Error $"{key} must be a string"
                    | None -> Ok None
                match text "engineDefsFolder", text "syzygyPath", text "puzzleResultsFolder" with
                | Ok defs, Ok syzygy, Ok puzzles ->
                    Ok({ EngineDefsFolder = defs; SyzygyPath = syzygy; PuzzleResultsFolder = puzzles }, warnings)
                | Error e, _, _
                | _, Error e, _
                | _, _, Error e -> Error e
        with :? JsonException as ex ->
            Error ex.Message

/// Where eb-cli.json is read from: the executable's folder (the download's, or bin/ when run from
/// source), whatever the working directory.
let path () =
    Path.Combine(AppContext.BaseDirectory, fileName)

/// Relative folders in the file are relative to the file, not to wherever eb-cli is run from.
let private relativeTo (dir: string) (folder: string) =
    if Path.IsPathRooted folder then folder else Path.GetFullPath(Path.Combine(dir, folder))

/// eb-cli.json from disk; no file (or an empty one) is no settings.
let load (file: string) =
    if not (File.Exists file) then Ok(empty, [])
    else
        try
            parse (File.ReadAllText file)
            |> Result.map (fun (s, warnings) ->
                let dir = Path.GetDirectoryName(Path.GetFullPath file)
                let one = Option.map (relativeTo dir)
                // only syzygyPath is a list of folders, split as the prober splits it
                let many =
                    Option.map (fun folders ->
                        ChessLibrary.TablebaseProbe.tablebaseFolders folders
                        |> Array.map (relativeTo dir)
                        |> String.concat (string Path.PathSeparator))
                { EngineDefsFolder = one s.EngineDefsFolder
                  SyzygyPath = many s.SyzygyPath
                  PuzzleResultsFolder = one s.PuzzleResultsFolder },
                warnings)
            |> Result.mapError (fun e -> $"{file}: {e}")
        with ex -> Error $"{file}: {ex.Message}"

/// The settings of this run, read once and only when a verb asks (its warnings printed then): a
/// broken file matters only to a command that needs a value from it.
let current: Lazy<Result<Settings, string>> =
    lazy
        (let file = path ()
         load file
         |> Result.map (fun (s, warnings) ->
             for w in warnings do eprintfn "%s: %s" file w
             s))

/// Where the tablebases are: --syzygy, then EB_SYZYGY_PATH, then eb-cli.json; None when nowhere. The
/// settings are read only when neither is given.
let resolveSyzygyPath (flag: string option) (env: string option) (settings: unit -> Result<Settings, string>) =
    let given =
        function
        | Some(s: string) when not (String.IsNullOrWhiteSpace s) -> Some s
        | _ -> None
    match given flag, given env with
    | Some p, _
    | None, Some p -> Ok(Some p)
    | None, None -> settings () |> Result.map (fun s -> s.SyzygyPath)

/// A name with no folder in it ("Lc0_BT4_332", not "defs/Lc0_BT4_332.json").
let isBareName (arg: string) =
    not (String.IsNullOrWhiteSpace arg) && arg.IndexOfAny([| '/'; '\\' |]) < 0 && not (Path.IsPathRooted arg)

/// An engine argument as a file: as given when it exists; a bare name otherwise in engineDefsFolder,
/// as <name>.json before <name> (an engine binary next to its def must not win over the def, which
/// carries the network, args and options). The settings are read only for a bare name that is not a
/// file here.
let resolveEngine (exists: string -> bool) (arg: string) (settings: unit -> Result<Settings, string>) : Result<string, string> =
    if exists arg then Ok arg
    elif not (isBareName arg) then Error $"Engine file not found: {arg}"
    else
        match settings () with
        | Error e -> Error $"Engine file not found: {arg} (and {e})"
        | Ok { EngineDefsFolder = None } ->
            Error $"Engine file not found: {arg} (a bare name is looked up in engineDefsFolder in {path ()})"
        | Ok { EngineDefsFolder = Some folder } ->
            [ Path.Combine(folder, arg + ".json"); Path.Combine(folder, arg) ]
            |> List.tryFind exists
            |> function
                | Some p -> Ok p
                | None -> Error $"Engine file not found: {arg} (not in engineDefsFolder {folder} either)"
