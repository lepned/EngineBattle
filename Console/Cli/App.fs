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

/// A usage error found while parsing answers in JSON when --json was typed for a command that has
/// it (or for no command at all: the caller still reads stdout as JSON).
let private wantsJson (tokens: string list) (c: Command option) =
    List.contains "--json" tokens
    && (match c with
        | Some c -> c.Options |> List.exists (fun o -> o.Name = "json")
        | None -> true)

let private usageError (json: bool) (message: string) (c: Command option) =
    error message
    match c with
    | Some c ->
        info $"Usage: {Help.usageLine c}"
        info $"Run 'eb-cli help {name c}' for options and examples."
    | None -> info "Run 'eb-cli help' for the commands."
    if json && not jsonPrinted then
        let commandName = c |> Option.map name |> Option.defaultValue ""
        printJson (envelope commandName false null [ { Code = "usage"; Message = message } ])
    ExitUsage

let private versionHelp =
    "Which EngineBattle this is\n\nUsage: eb-cli version [--json]\n"

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
    | Help VersionTopic ->
        Console.Out.Write versionHelp
        ExitOk
    | Version false ->
        Console.Out.WriteLine($"EngineBattle {ChessLibrary.BuildInfo.describe ()}")
        ExitOk
    | Version true ->
        printJson (envelope "version" true (box {| Version = ChessLibrary.BuildInfo.describe () |}) [])
        ExitOk
    | UsageError(message, c) -> usageError (wantsJson tokens c) message c
    | Run p ->
        let entry = entries |> List.find (fun e -> e.Spec.Path = p.Command.Path)
        let json = has p "json"
        jsonPrinted <- false
        // with --json, stdout is the JSON and nothing else
        let runIt () = if json then withJsonStdout (fun () -> entry.Run p) else entry.Run p
        // a run stopped with Ctrl+C says so in its exit code, whatever it managed to write
        let orInterrupted code = if interrupted then ExitInterrupted else code
        try
            let code = runIt ()
            // every --json run leaves one envelope on stdout, also one that forgot to print it
            if json && not jsonPrinted then
                let errors = if code = ExitOk then [] else [ { Code = "failed"; Message = $"exit code {code}" } ]
                printJson (envelope (name p.Command) (code = ExitOk) null errors)
            orInterrupted code
        with
        | Usage message -> usageError json message (Some p.Command)
        | ex ->
            // a failure nobody planned for: the message, not a stack trace (EB_DEBUG=1 for that)
            if not interrupted then
                error ex.Message
                if Environment.GetEnvironmentVariable "EB_DEBUG" = "1" then info (string ex)
            if json && not jsonPrinted then
                let code = if interrupted then "interrupted" else "failed"
                printJson (envelope (name p.Command) false null [ { Code = code; Message = ex.Message } ])
            orInterrupted ExitFailed
