/// Every config example and every eb-cli command line in the docs, checked against the code: a config
/// names only fields its type has (System.Text.Json passes over a misspelt or renamed one without a
/// word) and loads with the reader eb-cli uses; a command line parses. What points at the reader's
/// machine (books, folders, engine defs) is not checked; no engine is started. An example that is an
/// excerpt ("...") is left out and listed in the test output.
module DocExamplesTests

open System
open System.IO
open System.Collections
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open Xunit
open Xunit.Abstractions
open ChessLibrary

let private root = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."))

/// The user docs: the repo root's and Console's markdown, not the release notes or the plans in docs/.
let private docs =
    [ yield! Directory.GetFiles(root, "*.md")
      yield! Directory.GetFiles(Path.Combine(root, "Console"), "*.md") ]
    |> List.filter (fun f -> not (Path.GetFileName(f).StartsWith("CLAUDE", StringComparison.OrdinalIgnoreCase)))
    |> List.sort

/// The configs a new installation starts from.
let private templates = Directory.GetFiles(Path.Combine(root, "WebGUI", "Data"), "*.json") |> List.ofArray |> List.sort

let private rel (path: string) = Path.GetRelativePath(root, path).Replace('\\', '/')

/// Every fenced block of a markdown file, with the line it starts on.
let private fencedBlocks (file: string) =
    let lines = File.ReadAllText(file).Replace("\r\n", "\n").Split('\n')
    [ let mutable i = 0
      while i < lines.Length do
          if lines.[i].TrimStart().StartsWith "```" then
              let start = i + 1
              let mutable j = start
              while j < lines.Length && not (lines.[j].TrimStart().StartsWith "```") do j <- j + 1
              yield start + 1, String.Join("\n", lines.[start .. j - 1])
              i <- j + 1
          else i <- i + 1 ]

let private jsonOptions = JsonDocumentOptions(AllowTrailingCommas = true, CommentHandling = JsonCommentHandling.Skip)

let private tryObject (text: string) =
    if not (text.TrimStart().StartsWith "{") then None
    else
        try
            match JsonNode.Parse(text, Nullable(), jsonOptions) with
            | :? JsonObject as o -> Some o
            | _ -> None
        with _ -> None

// ---------------------------------------------------------------- what a config is

type private Kind =
    | TournamentConfig
    | PuzzleConfig
    | EretConfig
    | TunerConfig
    | BenchmarkConfig
    | EngineDef
    | CliSettings
    /// Excerpts of a config, shown on their own: only their field names are checked.
    | TimeConfigExcerpt
    | TunerExcerpt
    | TuneParameterExcerpt

let private has (o: JsonObject) (key: string) = o |> Seq.exists (fun kv -> String.Equals(kv.Key, key, StringComparison.OrdinalIgnoreCase))

/// What a block is, from the fields it has; None for anything else (a live-feed message, a fragment).
let private kindOf (o: JsonObject) =
    // a result document (puzzle run, an envelope), the WebGUI's own settings: not configs of eb-cli
    if has o "schemaVersion" || (has o "command" && has o "ok") || has o "OpeningsFolder" then None
    elif has o "EngineSetup" || has o "TournamentMode" then Some TournamentConfig
    elif has o "PuzzleFile" && (has o "TimeInSeconds" || has o "RunWithNodeLimit") then Some EretConfig
    elif has o "PuzzleFile" then Some PuzzleConfig
    elif has o "engineConfigPath" && (has o "phases" || has o "sprt" || has o "parameters") then Some TunerConfig
    elif has o "engineConfigPath" && has o "optionSets" then Some BenchmarkConfig
    elif has o "Protocol" && (has o "Options" || has o "Path") then Some EngineDef
    elif has o "engineDefsFolder" || has o "syzygyPath" || has o "puzzleResultsFolder" then Some CliSettings
    elif has o "Id" && (has o "Fixed" || has o "Tc" || has o "Nodes") then Some TimeConfigExcerpt
    elif has o "evalMode" || has o "parameters" || has o "phases" then Some TunerExcerpt
    elif has o "name" && has o "option" && has o "min" then Some TuneParameterExcerpt
    else None

let private typeOf =
    function
    | TournamentConfig -> typeof<TypesDef.Tournament.Tournament>
    | PuzzleConfig -> typeof<PuzzleTypes.PuzzleConfig>
    | EretConfig -> typeof<PuzzleTypes.EretConfig>
    | TunerConfig -> typeof<ConsoleApp.TunerRunner.TuneConfig>
    | BenchmarkConfig -> typeof<ConsoleApp.BenchmarkRunner.BenchmarkConfig>
    | EngineDef -> typeof<TypesDef.CoreTypes.EngineConfig>
    | CliSettings -> typeof<CliSettings.Settings>
    | TimeConfigExcerpt -> typeof<TimeControlTypes.TimeConfig>
    | TunerExcerpt -> typeof<ConsoleApp.TunerRunner.TuneConfig>
    | TuneParameterExcerpt -> typeof<ConsoleApp.TunerRunner.TuneParameterDef>

/// Fields the readers take that the type does not carry by that name: the time control's short
/// forms (Tc, St, R; JSON.timeConfigShortForms) and the older tournament shape that is still read
/// (CoreTypes.readOlderShape).
let private aliases =
    set [ "Tc"; "St"; "R"; "TestOptions" ]

/// The element type a field holds: an option's, a list's or an array's, a Nullable's.
let rec private inner (t: Type) =
    if t.IsArray then inner (t.GetElementType())
    elif t.IsGenericType then
        let d = t.GetGenericTypeDefinition()
        if d = typedefof<option<_>> || d = typedefof<Nullable<_>> || d = typedefof<list<_>> || d = typedefof<ResizeArray<_>> then
            inner (t.GetGenericArguments().[0])
        else t
    else t

/// A type whose fields are checked: a record or class of EngineBattle's, not a dictionary (engine
/// options), not one read by a converter of its own (a puzzle config's engine list).
let private checkable (t: Type) =
    t.Namespace <> null
    && (t.Namespace.StartsWith "ChessLibrary" || t.Namespace.StartsWith "ConsoleApp" || t.FullName.StartsWith "CliSettings")
    && not (typeof<IDictionary>.IsAssignableFrom t)
    && t <> typeof<PuzzleTypes.PuzzleEngine>
    && not t.IsEnum

/// Every key of the example that its type has no field for, as a path ("Opening.OpeningsPly").
let rec private unknownKeys (t: Type) (node: JsonNode) (path: string) : string list =
    match node with
    | :? JsonArray as a -> [ for item in a do if not (isNull item) then yield! unknownKeys t item path ]
    | :? JsonObject as o when checkable t ->
        let props = t.GetProperties() |> Array.map (fun p -> p.Name.ToLowerInvariant(), p) |> dict
        [ for kv in o do
            match props.TryGetValue(kv.Key.ToLowerInvariant()) with
            | true, p when not (isNull kv.Value) -> yield! unknownKeys (inner p.PropertyType) kv.Value (path + kv.Key + ".")
            | true, _ -> ()
            | false, _ -> if not (aliases.Contains kv.Key) then yield path + kv.Key ]
    | _ -> []

// ---------------------------------------------------------------- loading as eb-cli does

let private withTempFile (text: string) (read: string -> 'a) =
    let path = Path.Combine(Path.GetTempPath(), $"eb_doc_{Guid.NewGuid():N}.json")
    File.WriteAllText(path, text)
    try read path finally File.Delete path

let private readOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true, AllowTrailingCommas = true)

/// Loads the example with the reader eb-cli uses for its kind; an exception says what is wrong.
let private load (kind: Kind) (o: JsonObject) =
    let text = o.ToJsonString()
    match kind with
    | TournamentConfig ->
        // a whole config (engines and a time control) is validated as the docs' starting point is
        // in DocTournamentConfigTests; a fragment of one (a mode's options) only has to read
        let engines =
            match o.["EngineSetup"] with
            | :? JsonObject as setup when (setup.["EngineDefList"] :? JsonArray) -> setup.["EngineDefList"].AsArray().Count
            | _ -> 0
        // an empty template (no engines yet) is only read: validating it says it has no engines
        let whole = engines > 0 && has o "TimeControl"
        if whole then DocTournamentConfigTests.loadAndValidate text |> ignore
        else
            match withTempFile text Configuration.JSON.tryReadTournamentJson with
            | Error e -> failwith e
            | Ok _ -> ()
    | PuzzleConfig ->
        let c = withTempFile text Configuration.JSONParser.loadPuzzleConfig
        PuzzleDataUtils.parseRatingGroups c.RatingGroups c.MaxRating |> ignore
    | EretConfig -> withTempFile text Configuration.JSONParser.loadEretConfig |> ignore
    | TunerConfig -> JsonSerializer.Deserialize<ConsoleApp.TunerRunner.TuneConfig>(text, readOptions) |> ignore
    | BenchmarkConfig -> JsonSerializer.Deserialize<ConsoleApp.BenchmarkRunner.BenchmarkConfig>(text, readOptions) |> ignore
    // a def without a time control is the part of one an example is about (Winboard.md says so)
    | EngineDef when not (has o "TimeControlID") -> ()
    | EngineDef ->
        match Configuration.Validation.validateEngineConfigJson text with
        | Configuration.Validation.Invalid messages -> failwith (String.Join("; ", messages))
        | Configuration.Validation.Valid -> withTempFile text Configuration.JSON.readSingleEngineConfig |> ignore
    | CliSettings ->
        match CliSettings.parse text with
        | Error e -> failwith e
        | Ok _ -> ()
    | TimeConfigExcerpt | TunerExcerpt | TuneParameterExcerpt -> ()

/// Every config example: (where, kind, object); and what was left out, with why.
let private examples () =
    let found = ResizeArray<string * Kind * JsonObject>()
    let skipped = ResizeArray<string * string>()
    for file in docs do
        for line, text in fencedBlocks file do
            let where = $"{rel file}:{line}"
            if text.TrimStart().StartsWith "{" then
                match tryObject text with
                | None -> skipped.Add(where, "an excerpt, not whole JSON")
                | Some o ->
                    match kindOf o with
                    | Some k -> found.Add(where, k, o)
                    | None -> skipped.Add(where, "not a config (" + String.Join(", ", o |> Seq.truncate 3 |> Seq.map (fun kv -> kv.Key)) + ")")
    for file in templates do
        match tryObject (File.ReadAllText file) with
        | None -> skipped.Add(rel file, "does not read as JSON")
        | Some o ->
            match kindOf o with
            | Some k -> found.Add(rel file, k, o)
            | None -> skipped.Add(rel file, "not a config this test knows")
    List.ofSeq found, List.ofSeq skipped

type ConfigExamples(output: ITestOutputHelper) =

    [<Fact>]
    member _.``every config example in the docs names only fields the code reads, and loads`` () =
        let found, skipped = examples ()
        let problems =
            [ for where, kind, o in found do
                match unknownKeys (typeOf kind) o "" with
                | [] -> ()
                | keys ->
                    let listed = String.Join(", ", keys)
                    yield $"{where} ({kind}): no such field: {listed}"
                match (try load kind o; None with ex -> Some ex.Message) with
                | Some message -> yield $"{where} ({kind}): does not load: {message}"
                | None -> () ]
        for where, kind, _ in found do output.WriteLine $"checked  {where} ({kind})"
        for where, why in skipped do output.WriteLine $"skipped  {where}: {why}"
        // the docs hold examples of every kind; a finder that finds nothing must not pass
        Assert.True(found.Length >= 20, $"only {found.Length} examples found")
        Assert.True(problems.IsEmpty, String.Join("\n", problems))

// ---------------------------------------------------------------- command lines

/// The command lines of the docs' code blocks: `eb-cli ...`, with a trailing `\` joining the next
/// line. A synopsis (<engine>, [--nodes N], a|b, ...) is not a command line; `match` has fastchess's
/// command line, checked by its own tests.
let private commandLines () =
    [ for file in docs do
        for line, text in fencedBlocks file do
            let lines = text.Split('\n')
            let mutable i = 0
            while i < lines.Length do
                let first = lines.[i].Trim().TrimStart('$', ' ')
                if first.StartsWith "eb-cli " then
                    let mutable full = first
                    while full.EndsWith "\\" && i + 1 < lines.Length do
                        i <- i + 1
                        full <- full.TrimEnd('\\').TrimEnd() + " " + lines.[i].Trim()
                    yield $"{rel file}:{line + i}", full
                i <- i + 1 ]

let private synopsis (line: string) =
    Regex.IsMatch(line, @"<[^>]+>|\[[^\]]+\]|\.\.\.|…") || line.Contains "|"

/// The arguments of a command line as a shell gives them: quotes kept together and taken off; a
/// redirection or a comment ends it.
let private tokens (line: string) =
    let parts = Regex.Matches(line, "\"([^\"]*)\"|(\\S+)") |> Seq.map (fun m -> if m.Groups.[1].Success then m.Groups.[1].Value, true else m.Groups.[2].Value, false) |> List.ofSeq
    parts
    |> List.takeWhile (fun (t, quoted) -> quoted || not (t = ">" || t = "2>" || t = "|" || t.StartsWith "#" || t.StartsWith ">"))
    |> List.map fst
    |> List.skip 1   // eb-cli

type CommandLines(output: ITestOutputHelper) =

    [<Fact>]
    member _.``every eb-cli command line in the docs parses`` () =
        let specs = Registry.all |> List.map (fun e -> e.Spec)
        let lines = commandLines ()
        let mutable checkedCount = 0
        let problems =
            [ for where, line in lines do
                let args = tokens line
                if synopsis line then
                    // not a command line, but its command and every --option it names must exist
                    let words = args |> List.takeWhile (fun w -> Regex.IsMatch(w, "^[a-z][a-z-]*$"))
                    let spec =
                        specs
                        |> List.filter (fun s -> s.Path.Length <= words.Length && List.take s.Path.Length words = s.Path)
                        |> List.sortByDescending (fun s -> s.Path.Length)
                        |> List.tryHead
                    match spec with
                    | None when not words.IsEmpty && words.Head <> "match" -> yield $"{where}: {line}\n    no such command"
                    | None -> output.WriteLine $"match    {where}: {line}"
                    | Some s ->
                        let known = set [ for o in s.Options -> o.Name ]
                        let named = [ for m in Regex.Matches(line, @"(?<![\w-])--([a-z0-9][a-z0-9-]*)") -> m.Groups.[1].Value ]
                        match named |> List.filter (fun n -> not (known.Contains n) && n <> "help") with
                        | [] -> output.WriteLine $"synopsis {where}: {line}"
                        | unknown ->
                            let listed = String.Join(", ", unknown)
                            yield $"{where}: {line}\n    no such option: {listed}"
                elif args.IsEmpty || args.Head = "match" || args.Head.StartsWith "-" then output.WriteLine $"match    {where}: {line}"
                else
                    checkedCount <- checkedCount + 1
                    match Cli.Parse.parse specs args with
                    | Cli.Parse.UsageError(message, _) -> yield $"{where}: {line}\n    {message}"
                    | _ -> output.WriteLine $"parses   {where}: {line}" ]
        Assert.True(checkedCount >= 40, $"only {checkedCount} command lines checked")
        Assert.True(problems.IsEmpty, String.Join("\n", problems))
