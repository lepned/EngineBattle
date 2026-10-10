/// Every command of eb-cli, in the order the help lists them. A command that has not moved to the
/// new parser yet takes the old verb's arguments and runs it (docs/EbCliRewrite.md, phases 2-6).
module Registry

open Cli.Spec
open Cli.App

[<Literal>]
let private Positions = Commands.Positions.Group

[<Literal>]
let private Engines = "Engine definitions"

[<Literal>]
let private Pgn = "PGN files"

[<Literal>]
let private Runs = "Puzzles, books, tournaments and tuning"

[<Literal>]
let private Other = "Other"

/// A command that still runs the old verb: its arguments are handed over untouched.
let private legacy (runLegacy: string list -> int) (path: string list) group summary (oldVerb: string) =
    { Spec =
        { command path group summary with
            Args =
              [ { Name = "arguments"
                  Help = $"the arguments of the old '{oldVerb}' verb (see ConsoleTools.md)"
                  Required = false
                  Kind = Rest } ]
            Description = Some $"Not moved to the new command line yet: takes the arguments of the old '{oldVerb}' verb." }
      Run = fun p -> runLegacy (oldVerb :: Cli.Parse.restOf p "arguments") }

let all (runLegacy: string list -> int) : Entry list =
    let l = legacy runLegacy
    // the moved commands, by name
    let moved = Commands.Positions.entries @ Commands.Values.entries @ Commands.Pgn.entries @ Commands.Other.entries @ Commands.Engine.entries @ Commands.Runs.entries
    let m (path: string list) = moved |> List.find (fun e -> e.Spec.Path = path)
    [ m [ "analyze" ]
      m [ "compare" ]
      m [ "position" ]
      m [ "tablebase" ]
      m [ "values"; "position" ]
      m [ "values"; "fit" ]
      m [ "values"; "batch" ]
      m [ "engine"; "check" ]
      m [ "engine"; "new" ]
      m [ "engine"; "gen" ]
      m [ "engine"; "bench" ]
      m [ "pgn"; "check" ]
      m [ "pgn"; "validate" ]
      m [ "pgn"; "strip" ]
      m [ "pgn"; "summary" ]
      m [ "pgn"; "elo" ]
      m [ "pgn"; "speed" ]
      m [ "pgn"; "deviations" ]
      m [ "pgn"; "imbalance" ]
      m [ "book"; "eval" ]
      m [ "puzzle"; "run" ]
      m [ "puzzle"; "eret" ]
      m [ "puzzle"; "trend" ]
      m [ "tournament"; "run" ]
      m [ "tournament"; "validate" ]
      m [ "tune"; "run" ]
      m [ "tune"; "dashboard" ]
      m [ "perft" ]
      l [ "gui" ] Other "Start the WebGUI of a source checkout" "gui" ]