/// engine check / new / gen / bench: engine definitions and how an engine behaves.
module Commands.Engine

open System
open System.IO
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.Groups
open Cli.App

[<Literal>]
let Group = "Engine definitions"

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
            value "rounds" "N" "Repeat the stop and isready checks" |> withDefault "3"
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
              Rounds = max 1 (nonNegative "rounds" 3)
              Moves = moves
              Only = only }
    if code = 0 then ExitOk else ExitFailed

// ---------------------------------------------------------------- new

let newSpec =
    { command [ "engine"; "new" ] Group "Write an engine def from what the engine answers to 'uci'" with
        Description =
          Some "Starts the engine, reads its id and options, and writes <name>.json (the name without spaces) with every option at its default, overridden by --uci, --net and --syzygy. The same as the GUI's Engine creator. A def that exists is not written over without --force."
        Args = [ arg "exe" "the engine executable" ]
        Options =
          [ value "out-dir" "DIR" "The folder to write the def into (default: the working directory)"
            value "net" "FILE" "The network file, set in the engine's own network option"
            syzygy
            value "base" "DEF" "An existing def whose settings the new one starts from"
            uci
            switch "print" "Print the def instead of writing it"
            force
            value "timeout" "SECONDS" "How long the engine has to answer" |> withDefault "15" ]
        Examples =
          [ "eb-cli engine new C:/Engines/sf/stockfish.exe --uci Threads=8 --uci Hash=4096 --out-dir defs", "A Stockfish def with 8 threads"
            "eb-cli engine new lc0.exe --net BT4.onnx --print", "Show the def without writing it" ] }

let newDef (p: Parsed) =
    let exe = inputFile "engine" (argOf p "exe").Value
    let optional name what = valueOf p name |> Option.map (inputFile what)
    let timeout =
        match intOf p "timeout" with
        | Some s when s < 1 -> raise (Usage $"--timeout must be at least 1 second, got {s}")
        | Some s -> s
        | None -> 15
    let folder = valueOf p "out-dir" |> Option.map (existingFolder "out-dir") |> Option.defaultValue (Directory.GetCurrentDirectory())
    let wishes =
        { ChessLibrary.EngineProbe.Wishes.Empty with
            NetFile = optional "net" "network file"
            Tablebases = valueOf p "syzygy" |> Option.map fullPath
            Base = optional "base" "base def" |> Option.map ChessLibrary.Configuration.JSON.readSingleEngineConfig
            Overrides = uciPairs "uci" (valuesOf p "uci") }
    match ChessLibrary.EngineProbe.probe exe (timeout * 1000) with
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
            if has p "print" then
                printfn "%s" (ChessLibrary.EngineProbe.toJson def)
                ExitOk
            else
                match ChessLibrary.EngineProbe.write folder def (has p "force") with
                | Ok path ->
                    printfn "%s" path
                    ExitOk
                | Error message -> raise (Usage message)

// ---------------------------------------------------------------- gen

let genSpec =
    { command [ "engine"; "gen" ] Group "Write defs for training checkpoints, from an existing def" with
        Description =
          Some "The template is an existing def named <prefix>_<step>M[_ema].json: its name carries the naming, its content everything else. Every network of the same arm and variant (raw or ema) in the folder that has no def yet gets one, with the network and the step label replaced. Nothing is written over without --force."
        Args = [ arg "template" "an existing def of the arm (a full path)" ]
        Options =
          [ value "net" "FILE" "Write the def of this one network only"
            value "nets" "DIR" "Look for networks here (default: the folder of the template's network)"
            value "out-dir" "DIR" "Write the defs here (default: the template's folder)"
            switch "dry-run" "Show what would be written, write nothing"
            force ]
        OneOf = [ [ "net"; "nets" ] ]
        Examples =
          [ "eb-cli engine gen C:/Defs/Ceres_srv_deg4_400M.json", "Defs for the arm's new checkpoints"
            "eb-cli engine gen C:/Defs/Ceres_srv_deg4_400M.json --net C:/Nets/srv_deg4_800M.onnx --dry-run", "What one checkpoint's def would be" ] }

let gen (p: Parsed) =
    let template = inputFile "template" (argOf p "template").Value
    let nets =
        match valueOf p "net", valueOf p "nets" with
        | Some f, _ -> Some(inputFile "network" f)
        | None, Some d -> Some(existingFolder "nets" d)
        | None, None -> None
    let outDir = valueOf p "out-dir" |> Option.map (existingFolder "out-dir")
    match ChessLibrary.EngineDefGen.plan template nets outDir with
    | Error message -> raise (Usage message)
    | Ok plan ->
        printfn "%s" (ChessLibrary.EngineDefGen.render plan)
        if not plan.ToWrite.IsEmpty then
            if has p "dry-run" then info "--dry-run: nothing written"
            else
                let written = ChessLibrary.EngineDefGen.write (has p "force") plan
                info $"Wrote {written.Length} def(s)"
                let blocked = plan.ToWrite.Length - written.Length
                if blocked > 0 then warn $"{blocked} not written: a file of that name exists (--force writes over it)"
        ExitOk

// ---------------------------------------------------------------- bench

let benchSpec =
    { command [ "engine"; "bench" ] Group "Benchmark an engine from a config" with
        Description = Some "Runs the benchmark config (engine, option sets, positions, duration; see Console/ConsoleBenchmark.md) and writes the summary to its SummaryOutputPath, or logs/benchmark-summary-<time>.txt."
        Args = [ arg "config" "a benchmark config (JSON)" ]
        Examples = [ "eb-cli engine bench benchmark-options.json", "Run the benchmark" ] }

let bench (p: Parsed) =
    ConsoleApp.BenchmarkRunner.runBenchmark (inputFile "benchmark config" (argOf p "config").Value)
    ExitOk

let entries: Entry list =
    [ { Spec = checkSpec; Run = check }
      { Spec = newSpec; Run = newDef }
      { Spec = genSpec; Run = gen }
      { Spec = benchSpec; Run = bench } ]
