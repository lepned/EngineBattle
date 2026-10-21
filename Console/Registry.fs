/// Every command of eb-cli, in the order the help lists them (docs/EbCliRewrite.md). `match` is not
/// here: Program sends it to MatchMode, with fastchess's command line.
module Registry

open Cli.App

let all: Entry list =
    let entries =
        Commands.Positions.entries
        @ Commands.Values.entries
        @ Commands.Pgn.entries
        @ Commands.Engine.entries
        @ Commands.Runs.entries
        @ Commands.Bot.entries
        @ Commands.Other.entries
        @ Commands.Gui.entries
    let e (path: string list) = entries |> List.find (fun x -> x.Spec.Path = path)
    [ e [ "analyze" ]
      e [ "compare" ]
      e [ "position" ]
      e [ "tablebase" ]
      e [ "piece-values"; "position" ]
      e [ "piece-values"; "fit" ]
      e [ "piece-values"; "batch" ]
      e [ "engine"; "check" ]
      e [ "engine"; "new" ]
      e [ "engine"; "bench" ]
      e [ "pgn"; "parse" ]
      e [ "pgn"; "validate" ]
      e [ "pgn"; "strip" ]
      e [ "pgn"; "dedupe" ]
      e [ "pgn"; "endings" ]
      e [ "pgn"; "elo" ]
      e [ "pgn"; "speed" ]
      e [ "pgn"; "deviations" ]
      e [ "pgn"; "imbalance" ]
      e [ "book"; "eval" ]
      e [ "book"; "make" ]
      e [ "book"; "export" ]
      e [ "puzzle"; "run" ]
      e [ "puzzle"; "eret" ]
      e [ "puzzle"; "trend" ]
      e [ "tournament"; "run" ]
      e [ "tournament"; "validate" ]
      e [ "tune"; "run" ]
      e [ "tune"; "dashboard" ]
      e [ "bot"; "run" ]
      e [ "bot"; "challenge" ]
      e [ "bot"; "online" ]
      e [ "bot"; "upgrade" ]
      e [ "perft" ]
      e [ "gui" ] ]
