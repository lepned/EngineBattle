/// The option groups commands share - engine, limit, position, files - declared once and read once,
/// so `--uci`, `--nodes` or a FEN mean the same in every command. A value that cannot be used is a
/// usage error (exit 2) that names the option.
module Cli.Groups

open System
open System.IO
open Cli.Spec
open Cli.Parse

let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

// ---------------------------------------------------------------- files

/// A path as typed: "~" is the home folder, a relative path is relative to the working directory.
let fullPath (path: string) =
    if String.IsNullOrWhiteSpace path then raise (Usage "a path is empty")
    let expanded =
        if path = "~" || path.StartsWith "~/" || path.StartsWith @"~\" then
            Environment.GetFolderPath Environment.SpecialFolder.UserProfile + path.Substring 1
        else path
    Path.GetFullPath expanded

/// A list of folders separated by ';' (Windows) or ':' (elsewhere), each made full.
let fullPaths (paths: string) =
    paths.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map fullPath
    |> fun a -> String.Join(string Path.PathSeparator, a)

/// An input file that must exist.
let inputFile (what: string) (path: string) =
    if String.IsNullOrWhiteSpace path then raise (Usage $"{what}: no path given")
    let full = fullPath path
    if File.Exists full then full else raise (Usage $"{what} not found: {path}")

/// An output file: refused when it exists and --force was not given (no command overwrites silently).
let outputFile (p: Parsed) (path: string) =
    let full = fullPath path
    if File.Exists full && not (has p "force") then
        raise (Usage $"{path} exists; give --force to write over it")
    full

// ---------------------------------------------------------------- engine

let uci = value "uci" "NAME=VALUE" "Set a UCI option of the engine" |> repeatable
let args = value "args" "\"...\"" "The engine's command-line arguments, instead of the def's"
let engineArgHelp = "an engine def (.json), an engine executable, or the name of a def in engineDefsFolder (eb-cli.json)"

/// NAME=VALUE pairs; the value may itself contain '=' and spaces.
let uciPairs (option: string) (values: string list) =
    values
    |> List.map (fun v ->
        match v.IndexOf '=' with
        | i when i > 0 -> v.Substring(0, i).Trim(), v.Substring(i + 1)
        | _ -> raise (Usage $"--{option} expects NAME=VALUE, got '{v}'"))

/// The engine named by `given`: its def (or a bare executable), with --uci and --args applied.
let engineFrom (p: Parsed) (given: string) (uciOption: string) =
    // a bare name may be a file here or a def in engineDefsFolder; a path is just a path
    let target = if CliSettings.isBareName given then given else fullPath given
    let path =
        match CliSettings.resolveEngine File.Exists target (fun () -> CliSettings.current.Force()) with
        | Ok path -> fullPath path
        | Error message -> raise (Usage message)
    let config =
        if path.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
            ChessLibrary.Configuration.JSON.readSingleEngineConfig path
        else
            ChessLibrary.TypesDef.CoreTypes.EngineConfig.EmptyWithPath path
    let options = Collections.Generic.Dictionary<string, obj>(config.Options)
    for name, v in uciPairs uciOption (valuesOf p uciOption) do
        options.[name] <- box v
    let config = { config with Options = options }
    match valueOf p "args" with
    | Some a -> { config with Args = a }
    | None -> config

/// The engine of a positional argument.
let engine (p: Parsed) (argName: string) (uciOption: string) = engineFrom p (argOf p argName).Value uciOption

// ---------------------------------------------------------------- limit

type Limit =
    | Nodes of int
    | MoveTime of int
    | Depth of int

    member l.Go =
        match l with
        | Nodes n -> $"go nodes {n}"
        | MoveTime ms -> $"go movetime {ms}"
        | Depth d -> $"go depth {d}"

    member l.Text =
        match l with
        | Nodes n -> $"""nodes {n.ToString("N0", Globalization.CultureInfo.InvariantCulture)}"""
        | MoveTime ms -> $"movetime {ms} ms"
        | Depth d -> $"depth {d}"

/// --nodes, --movetime, --depth: at most one (Spec.OneOf), shown with the command's default.
let limitOptions (defaultNodes: int) =
    [ value "nodes" "N" "Search this many nodes" |> withDefault (string defaultNodes)
      value "movetime" "MS" "Search this many milliseconds"
      value "depth" "N" "Search to this depth" ]

let limitOneOf = [ "nodes"; "movetime"; "depth" ]

let private positive (p: Parsed) (name: string) =
    intOf p name
    |> Option.map (fun n -> if n > 0 then n else raise (Usage $"--{name} must be above 0, got {n}"))

let limit (p: Parsed) (defaultNodes: int) =
    match positive p "nodes", positive p "movetime", positive p "depth" with
    | Some n, _, _ -> Nodes n
    | _, Some ms, _ -> MoveTime ms
    | _, _, Some d -> Depth d
    | _ -> Nodes defaultNodes

// ---------------------------------------------------------------- position

let fenHelp = "a FEN (quoted, or its six fields as they come) or startpos"
let moves = value "moves" "\"MOVES\"" "Moves played from the position first, SAN or UCI (\"e4 e5 Nf3\" or \"e2e4 e7e5\")"

/// The FEN of an argument: startpos, or a FEN that is valid (short FENs are filled in).
let fen (p: Parsed) (argName: string) =
    match argOf p argName with
    | None
    | Some "startpos" -> startFen
    | Some f when f.Equals("startpos", StringComparison.OrdinalIgnoreCase) -> startFen
    | Some f ->
        let v = ChessLibrary.BoardUtils.validateFen f
        if v.IsValid then ChessLibrary.BoardUtils.normalizeFen f
        else raise (Usage $"""not a valid FEN: {String.Join("; ", v.Errors)}""")

/// The --moves of a position as UCI moves, checked to be legal one after another.
let movesFrom (p: Parsed) (fenText: string) =
    match valueOf p "moves" with
    | None -> []
    | Some line when String.IsNullOrWhiteSpace line -> []
    | Some line ->
        match ChessLibrary.BoardUtils.pvToUci fenText line with
        | Some uci -> uci.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        | None -> raise (Usage $"--moves: '{line}' is not a legal line from {fenText}")

/// The FEN after playing moves.
let after (fenText: string) (uciMoves: string list) =
    uciMoves |> List.fold (fun f m -> (ChessLibrary.BoardUtils.tryMakeMove f m).Value) fenText

// ---------------------------------------------------------------- tablebases

let syzygy = value "syzygy" "DIR" "Syzygy folders (several separated by ';' on Windows, ':' elsewhere)"

/// --syzygy, then EB_SYZYGY_PATH, then syzygyPath in eb-cli.json; "" when none is set.
let syzygyPath (p: Parsed) =
    match CliSettings.resolveSyzygyPath (valueOf p "syzygy") (Environment.GetEnvironmentVariable "EB_SYZYGY_PATH" |> Option.ofObj) (fun () -> CliSettings.current.Force()) with
    | Ok path -> defaultArg path ""
    | Error e -> raise (Usage $"cannot read {e}")
