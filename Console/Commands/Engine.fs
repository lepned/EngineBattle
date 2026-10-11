/// engine check / new / bench: engine definitions and how an engine behaves.
module Commands.Engine

open System
open System.IO
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.Groups
open Cli.App

[<Literal>]
let Group = "Engine setup and testing"

/// A folder that must exist (an output folder is not made up silently).
let private existingFolder (option: string) (path: string) =
    let full = fullPath path
    if Directory.Exists full then full else raise (Usage $"--{option}: folder not found: {path}")

// ---------------------------------------------------------------- check

let checkSpec =
    { command [ "engine"; "check" ] Group "Check that an engine follows the UCI protocol" with
        Description =
          Some $"""Start-up, options, positions, search limits, info lines, stop and isready, ponder, edge cases and quit, and how it behaves in EngineBattle's analysis pages. PASS, WARN, FAIL or SKIP per check, then a summary. Exit 1 when a check fails. Groups: {String.Join(", ", EngineCheck.groups)}."""
        Args = [ arg "engine" engineArgHelp ]
        Options =
          [ value "nodes" "N" "Nodes for the search checks" |> withDefault "2000"
            value "movetime" "MS" "Milliseconds for the search checks, instead of nodes"
            value "only" "GROUPS" "Run only these groups, comma-separated (startup,positions,stop)"
            value "delay" "MS" "Wait between commands to the engine" |> withDefault "30"
            value "stop-delay" "MS" "Wait before each stop" |> withDefault "0"
            value "repeat" "N" "Repeat the stop and isready checks N times" |> withDefault "3"
            value "moves" "\"MOVES\"" "The game the position checks play (SAN or UCI; default a 30-ply Ruy Lopez)"
            uci
            args ]
        OneOf = [ [ "nodes"; "movetime" ] ]
        Examples =
          [ "eb-cli engine check myengine.exe", "Every check"
            "eb-cli engine check lc0.json --only startup,positions,limits,stop", "Only these groups"
            "eb-cli engine check ceres.json --delay 50 --stop-delay 50", "A slower engine" ] }

let check (p: Parsed) =
    let config = engine p "engine" "uci"
    let nonNegative name dflt =
        match intOf p name with
        | Some v when v < 0 -> raise (Usage $"--{name} must be 0 or more, got {v}")
        | Some v -> v
        | None -> dflt
    let moves =
        match valueOf p "moves" with
        | Some line ->
            match ChessLibrary.BoardUtils.pvToUci startFen line with
            | Some uci -> uci.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            | None -> raise (Usage $"--moves: '{line}' is not a legal game from the start position")
        | None -> EngineCheck.defaultMoves
    let only =
        valueOf p "only"
        |> Option.map (fun s -> s.Split([| ','; ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun g -> g.ToLowerInvariant()) |> List.ofArray)
        |> Option.defaultValue []
    match only |> List.filter (fun g -> not (List.contains g EngineCheck.groups)) with
    | [] -> ()
    | unknown ->
        raise (Usage $"""--only: unknown group {String.Join(", ", unknown)}{didYouMean unknown.Head EngineCheck.groups} (groups: {String.Join(", ", EngineCheck.groups)})""")
    let code =
        EngineCheck.run
            { Config = config
              Nodes = nonNegative "nodes" 2000
              MoveTimeMs = intOf p "movetime"
              DelayMs = nonNegative "delay" 30
              StopDelayMs = nonNegative "stop-delay" 0
              Rounds = max 1 (nonNegative "repeat" 3)
              Moves = moves
              Only = only }
    if code = 0 then ExitOk else ExitFailed

// ---------------------------------------------------------------- new

let newSpec =
    { command [ "engine"; "new" ] Group "Create an engine definition (.json) from an executable, or copy one with another net" with
        Description =
          Some "An executable: starts the engine, reads its id and options, and writes <name>.json (the name without spaces) with every option at its default, overridden by --uci, --net and --syzygy - the GUI's Engine creator. A def (.json): writes a copy with the network --net (the same engine and settings), named after the net, or with the next step for a training checkpoint's def (<prefix>_<step>M[_ema].json); --name names it otherwise. A def that exists is not written over without --force."
        Args = [ arg "engine" "the engine executable, or an existing def (.json)" ]
        Options =
          [ value "out-dir" "DIR" "The folder to write the def into (default: the working directory; a def's copy: the def's folder)"
            value "net" "FILE" "The network file, set in the engine's own network option (with a def: required)"
            value "name" "NAME" "The copy's name (with a def)"
            syzygy
            value "base" "DEF" "An existing def whose settings the new one starts from"
            uci
            switch "dry-run" "Print the def, write nothing"
            force
            value "timeout" "MS" "How long the engine has to answer" |> withDefault "15000" ]
        Examples =
          [ "eb-cli engine new C:/Engines/sf/stockfish.exe --uci Threads=8 --uci Hash=4096 --out-dir defs", "A Stockfish def with 8 threads"
            "eb-cli engine new lc0.exe --net BT4.onnx --dry-run", "Show the def without writing it"
            "eb-cli engine new Lc0_BT4.json --net C:/Nets/BT5.pb.gz", "The same Lc0 with another net: BT5.json"
            "eb-cli engine new Ceres_arm_400M.json --net C:/Nets/arm_800003840.onnx", "A training checkpoint's def: Ceres_arm_800M.json" ] }

/// `engine new <def> --net FILE`: the def with another net (EngineDefGen.fromDef).
let private copyDef (p: Parsed) (def: string) =
    for exeOnly in [ "uci"; "base"; "syzygy"; "timeout" ] do
        if has p exeOnly then raise (Usage $"--{exeOnly} is for an executable; a def's copy takes --net, --name, --out-dir, --dry-run and --force")
    let net =
        match valueOf p "net" with
        | Some f -> inputFile "network file" f
        | None -> raise (Usage "a def's copy needs --net: the network it plays with")
    let folder = valueOf p "out-dir" |> Option.map (existingFolder "out-dir")
    match ChessLibrary.EngineDefGen.fromDef def net (valueOf p "name") folder with
    | Error message -> raise (Usage message)
    | Ok copy when String.Equals(Path.GetFullPath copy.DefPath, def, StringComparison.OrdinalIgnoreCase) ->
        raise (Usage $"the copy would be {copy.DefPath} itself (--name or --out-dir puts it elsewhere)")
    | Ok copy ->
        info $"""{copy.Name}: {net.Replace('\\', '/')}"""
        if has p "dry-run" then
            info $"--dry-run: {copy.DefPath} not written"
            printfn "%s" copy.Text
            ExitOk
        elif File.Exists copy.DefPath && not (has p "force") then
            raise (Usage $"{copy.DefPath} exists - use --force to overwrite it")
        else
            File.WriteAllText(copy.DefPath, copy.Text)
            printfn "%s" copy.DefPath
            ExitOk

let newDef (p: Parsed) =
    let engine = inputFile "engine" (argOf p "engine").Value
    if engine.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then copyDef p engine else
    if has p "name" then raise (Usage "--name is for a def's copy; an executable's def is named after the engine's id")
    let exe = engine
    let optional name what = valueOf p name |> Option.map (inputFile what)
    let timeout =
        match intOf p "timeout" with
        // it was in seconds once
        | Some ms when ms < 100 -> raise (Usage $"--timeout is in milliseconds: {ms} seconds is {ms * 1000}")
        | Some ms -> ms
        | None -> 15000
    let folder = valueOf p "out-dir" |> Option.map (existingFolder "out-dir") |> Option.defaultValue (Directory.GetCurrentDirectory())
    let wishes =
        { ChessLibrary.EngineProbe.Wishes.Empty with
            NetFile = optional "net" "network file"
            Tablebases = valueOf p "syzygy" |> Option.map fullPaths
            Base = optional "base" "base def" |> Option.map ChessLibrary.Configuration.JSON.readSingleEngineConfig
            Overrides = uciPairs "uci" (valuesOf p "uci") }
    match ChessLibrary.EngineProbe.probe exe timeout with
    | Error message ->
        error message
        ExitFailed
    | Ok probed ->
        match ChessLibrary.EngineProbe.build probed wishes with
        | Error message -> raise (Usage message)
        | Ok def ->
            let by = if String.IsNullOrWhiteSpace def.Dev then "" else $" by {def.Dev}"
            info $"{def.Name}{by}: {def.Options.Count} options"
            match ChessLibrary.EngineProbe.networkOptionKey probed.Options with
            | Some k -> info $"  {k} = {def.Options.[k]}"
            | None -> ()
            if has p "dry-run" then
                printfn "%s" (ChessLibrary.EngineProbe.toJson def)
                ExitOk
            else
                match ChessLibrary.EngineProbe.write folder def (has p "force") with
                | Ok path ->
                    printfn "%s" path
                    ExitOk
                | Error message -> raise (Usage message)

// ---------------------------------------------------------------- bench

let benchSpec =
    { command [ "engine"; "bench" ] Group "Measure engine speed (NPS) across option sets, from a benchmark config" with
        Description = Some "Runs the benchmark config (engine, option sets, positions, duration; see Console/ConsoleBenchmark.md) and writes the summary to its SummaryOutputPath, or logs/benchmark-summary-<time>.txt."
        Args = [ arg "config" "a benchmark config (JSON)" ]
        Examples = [ "eb-cli engine bench benchmark-options.json", "Run the benchmark" ] }

let bench (p: Parsed) =
    ConsoleApp.BenchmarkRunner.runBenchmark (inputFile "benchmark config" (argOf p "config").Value)
    ExitOk

let entries: Entry list =
    [ { Spec = checkSpec; Run = check }
      { Spec = newSpec; Run = newDef }
      { Spec = benchSpec; Run = bench } ]
