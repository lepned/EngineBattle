/// Every command of eb-cli, in the order the help lists them. A command that has not moved to the
/// new parser yet takes the old verb's arguments and runs it (docs/EbCliRewrite.md, phases 2-6).
module Commands

open Cli.Spec
open Cli.App

[<Literal>]
let private Positions = "Engines and positions"

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
    [ l [ "analyze" ] Positions "Analyze a position with an engine" "analyze"
      l [ "compare" ] Positions "Compare two engines on one or more positions" "compare"
      l [ "position" ] Positions "Facts about a position as JSON: legal moves, attacks, pins, insights" "query"
      l [ "tablebase" ] Positions "Every move of a position from the Syzygy tables" "tb"
      l [ "values"; "position" ] Positions "Piece values from an engine's evaluations" "piecevalues"
      l [ "values"; "fit" ] Positions "Piece values fitted over many positions" "piecevaluefit"
      l [ "values"; "batch" ] Positions "Piece values for a folder of nets" "pvbatch"
      l [ "engine"; "check" ] Engines "Check that an engine follows the UCI protocol" "enginecheck"
      l [ "engine"; "new" ] Engines "Write an engine def from what the engine answers to 'uci'" "mkdef"
      l [ "engine"; "gen" ] Engines "Write defs for training checkpoints, from an existing def" "gendefs"
      l [ "engine"; "bench" ] Engines "Benchmark an engine from a config" "benchmark"
      l [ "pgn"; "check" ] Pgn "Parse every game: counts, empty games, speed" "pgncheck"
      l [ "pgn"; "validate" ] Pgn "Replay every game: illegal or ambiguous moves, non-standard SAN" "pgnvalidate"
      l [ "pgn"; "strip" ] Pgn "Write the games without comments, variations or NAGs" "pgnstrip"
      l [ "pgn"; "summary" ] Pgn "How the games ended" "pgnsummary"
      l [ "pgn"; "elo" ] Pgn "Ratings and results" "elo"
      l [ "pgn"; "speed" ] Pgn "Speed statistics" "speed"
      l [ "pgn"; "deviations" ] Pgn "Where engines deviated from each other and from themselves" "deviations"
      l [ "pgn"; "imbalance" ] Pgn "Results by material imbalance" "pvcombo"
      l [ "book"; "eval" ] Runs "Keep the openings every engine scores in an eval window" "bookeval"
      l [ "puzzle"; "run" ] Runs "Run a puzzle test from a config" "puzzlejson"
      l [ "puzzle"; "eret" ] Runs "Run an ERET test from a config" "eretjson"
      l [ "puzzle"; "trend" ] Runs "Step curves per training arm from many puzzle runs" "puzzletrend"
      l [ "tournament"; "run" ] Runs "Run a tournament from a config" "tournamentjson"
      l [ "tournament"; "validate" ] Runs "Check a tournament config without running it" "validate"
      l [ "tune"; "run" ] Runs "Tune engine parameters (Bayesian optimisation)" "tune"
      l [ "tune"; "dashboard" ] Runs "Write the tuning dashboard again from the saved state" "redash"
      l [ "perft" ] Other "Count the move tree on the Chess960 test positions" "perft"
      l [ "gui" ] Other "Start the WebGUI of a source checkout" "gui" ]
