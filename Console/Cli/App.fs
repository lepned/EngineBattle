/// The command line, start to end: parse with Spec, run the command, map every outcome to the same
/// exit codes. Commands return an exit code; a usage mistake they find is raised as Parse.Usage.
module Cli.App

open System
open Cli.Spec
open Cli.Parse
open Cli.Output

type Entry =
    { Spec: Command
      Run: Parsed -> int }

let private wantsJson (tokens: string list) = List.contains "--json" tokens

let private usageError (tokens: string list) (message: string) (c: Command option) =
    error message
    match c with
    | Some c ->
        info $"Usage: {Help.usageLine c}"
        info $"Run 'eb-cli help {name c}' for options and examples."
    | None -> info "Run 'eb-cli help' for the commands."
    if wantsJson tokens then
        let commandName = c |> Option.map name |> Option.defaultValue ""
        printJson (envelope commandName false null [ { Code = "usage"; Message = message } ])
    ExitUsage

/// Runs a command line (without the program name) and returns the exit code.
let run (entries: Entry list) (tokens: string list) : int =
    let commands = entries |> List.map (fun e -> e.Spec)
    match parse commands tokens with
    | Help Overview ->
        Console.Out.Write(Help.overview commands)
        ExitOk
    | Help(Group g) ->
        Console.Out.Write(Help.group commands g)
        ExitOk
    | Help(One c) ->
        Console.Out.Write(Help.command c)
        ExitOk
    | Version ->
        Console.Out.WriteLine($"EngineBattle {ChessLibrary.BuildInfo.describe ()}")
        ExitOk
    | UsageError(message, c) -> usageError tokens message c
    | Run p ->
        let entry = entries |> List.find (fun e -> e.Spec.Path = p.Command.Path)
        // with --json, stdout is the JSON and nothing else
        let runIt () = if has p "json" then withJsonStdout (fun () -> entry.Run p) else entry.Run p
        try
            let code = runIt ()
            // a run stopped with Ctrl+C says so in its exit code, whatever it managed to write
            if interrupted then ExitInterrupted else code
        with
        | Usage message -> usageError tokens message (Some p.Command)
        | ex ->
            // a failure nobody planned for: the message, not a stack trace (EB_DEBUG=1 for that)
            error ex.Message
            if Environment.GetEnvironmentVariable "EB_DEBUG" = "1" then info (string ex)
            if wantsJson tokens then
                printJson (envelope (name p.Command) false null [ { Code = "failed"; Message = ex.Message } ])
            ExitFailed
