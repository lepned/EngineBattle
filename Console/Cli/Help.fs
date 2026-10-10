/// Help, rendered from Spec: the command list, one group, one command. Nothing here is written by
/// hand per command, so a new option is in the help the moment it is in the spec.
module Cli.Help

open System
open System.Text
open Cli.Spec

let private argLabel (a: Arg) =
    let n = match a.Kind with Rest -> $"<{a.Name}...>" | _ -> $"<{a.Name}>"
    if a.Required then n else $"[{n}]"

let usageLine (c: Command) =
    let args = c.Args |> List.map argLabel
    let opts = if c.Options.IsEmpty then [] else [ "[options]" ]
    String.Join(" ", [ "eb-cli"; name c ] @ args @ opts)

let private optLabel (o: Opt) =
    let long = match o.Value with Some v -> $"--{o.Name} {v}" | None -> $"--{o.Name}"
    match o.Short with
    | Some s -> $"-{s}, {long}"
    | None -> $"    {long}"

/// The first column's width for these rows: the longest, up to 30.
let private widthOf (rows: (string * string) list) =
    rows |> List.map (fst >> String.length) |> List.fold max 0 |> min 30

/// Two columns; a first column too long for the width puts the second on its own line.
let private tableWith (width: int) (rows: (string * string) list) (sb: StringBuilder) =
    for left, right in rows do
        if left.Length > width then
            sb.AppendLine($"  {left}").AppendLine($"  {String(' ', width)}  {right}") |> ignore
        else
            sb.AppendLine($"  {left.PadRight width}  {right}") |> ignore

let private table (rows: (string * string) list) (sb: StringBuilder) = tableWith (widthOf rows) rows sb

let exitCodes =
    [ "0", "success"
      "1", "the command ran and failed, or found problems"
      "2", "usage: an unknown command or option, a bad value, a missing input file"
      "130", "interrupted with Ctrl+C" ]

/// `eb-cli`, `eb-cli help`: every command under its group, one line each.
let overview (commands: Command list) =
    let sb = StringBuilder()
    sb.AppendLine("eb-cli - EngineBattle on the command line").AppendLine() |> ignore
    sb.AppendLine("Usage: eb-cli <command> [arguments] [options]").AppendLine() |> ignore
    let row (c: Command) = String.Join(" ", name c :: (c.Args |> List.filter (fun a -> a.Required) |> List.map argLabel)), c.Summary
    // one width for the whole list, so the groups line up
    let width = commands |> List.map row |> widthOf
    for group, cs in commands |> List.groupBy (fun c -> c.Group) do
        sb.AppendLine(group) |> ignore
        tableWith width (cs |> List.map row) sb
        sb.AppendLine() |> ignore
    sb.AppendLine("eb-cli help <command>   options and examples for one command") |> ignore
    sb.AppendLine("eb-cli version          which EngineBattle this is") |> ignore
    sb.AppendLine("eb-cli match ...        an engine match, fastchess-style (eb-cli match -help)") |> ignore
    sb.ToString()

/// `eb-cli help pgn`, `eb-cli pgn --help`: the commands of one group.
let group (commands: Command list) (g: string) =
    let sb = StringBuilder()
    sb.AppendLine($"eb-cli {g} <command>").AppendLine() |> ignore
    table [ for c in commands |> List.filter (fun c -> c.Path.Head = g) -> String.Join(" ", name c :: (c.Args |> List.map argLabel)), c.Summary ] sb
    sb.AppendLine().AppendLine($"Run 'eb-cli help {g} <command>' for options and examples.") |> ignore
    sb.ToString()

/// `eb-cli help pgn check`, `eb-cli pgn check --help`.
let command (c: Command) =
    let sb = StringBuilder()
    sb.AppendLine(c.Summary).AppendLine() |> ignore
    sb.AppendLine($"Usage: {usageLine c}") |> ignore
    match c.Description with
    | Some d -> sb.AppendLine().AppendLine(d) |> ignore
    | None -> ()
    if not c.Args.IsEmpty then
        sb.AppendLine().AppendLine("Arguments:") |> ignore
        table [ for a in c.Args -> argLabel a, a.Help ] sb
    if not c.Options.IsEmpty then
        sb.AppendLine().AppendLine("Options:") |> ignore
        table
            [ for o in c.Options ->
                  let extra =
                      [ match o.Default with Some d -> yield $"default {d}" | None -> ()
                        if o.Repeatable then yield "repeatable" ]
                  optLabel o, (if extra.IsEmpty then o.Help else $"""{o.Help} ({String.Join(", ", extra)})""") ]
            sb
    for group in c.OneOf do
        sb.AppendLine($"""  At most one of {String.Join(", ", group |> List.map (fun n -> "--" + n))}.""") |> ignore
    if not c.Examples.IsEmpty then
        sb.AppendLine().AppendLine("Examples:") |> ignore
        for line, what in c.Examples do
            sb.AppendLine($"  {line}").AppendLine($"      {what}") |> ignore
    sb.AppendLine().AppendLine("Exit codes:") |> ignore
    table exitCodes sb
    sb.ToString()
