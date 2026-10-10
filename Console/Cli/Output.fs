/// Where output goes and what the process returns. stdout carries a command's result and nothing
/// else; progress, warnings and errors go to stderr. With --json, stdout is always one JSON document.
module Cli.Output

open System
open System.Text.Encodings.Web
open System.Text.Json

/// The same in every command.
[<Literal>]
let ExitOk = 0

[<Literal>]
let ExitFailed = 1

[<Literal>]
let ExitUsage = 2

[<Literal>]
let ExitInterrupted = 130

/// Colour only on a terminal, and never with NO_COLOR set (https://no-color.org).
let private colourOn (redirected: bool) =
    not redirected && String.IsNullOrEmpty(Environment.GetEnvironmentVariable "NO_COLOR")

let private writeColoured (writer: IO.TextWriter) (redirected: bool) (colour: ConsoleColor) (text: string) =
    if colourOn redirected then
        let before = Console.ForegroundColor
        Console.ForegroundColor <- colour
        writer.WriteLine text
        Console.ForegroundColor <- before
    else
        writer.WriteLine text

/// "eb-cli: <message>" on stderr, in red on a terminal.
let error (message: string) =
    writeColoured Console.Error Console.IsErrorRedirected ConsoleColor.Red $"eb-cli: {message}"

/// "warning: <message>" on stderr, in yellow on a terminal.
let warn (message: string) =
    writeColoured Console.Error Console.IsErrorRedirected ConsoleColor.Yellow $"warning: {message}"

/// A line of progress or diagnostics: stderr.
let info (message: string) = Console.Error.WriteLine message

let jsonOptions =
    JsonSerializerOptions(
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        WriteIndented = true,
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

type JsonError = { Code: string; Message: string }

/// The envelope of every --json result: which command, whether it succeeded, its result (in the
/// command's own schema) and what went wrong.
type Envelope =
    { Command: string
      Ok: bool
      EbVersion: string
      Result: obj
      Errors: JsonError list }

let envelope (command: string) (ok: bool) (result: obj) (errors: JsonError list) =
    { Command = command
      Ok = ok
      EbVersion = ChessLibrary.BuildInfo.describe ()
      Result = result
      Errors = errors }

/// Where the JSON goes: the real stdout, kept here while Console.Out points at stderr during a
/// --json command (the library prints its own notices with printfn; they must not land in the JSON).
let mutable private jsonWriter: IO.TextWriter = null

/// Runs `f` with everything printed sent to stderr, and the JSON to the real stdout.
let withJsonStdout (f: unit -> 'a) =
    let real = Console.Out
    jsonWriter <- real
    Console.SetOut Console.Error
    try f ()
    finally
        Console.SetOut real
        jsonWriter <- null

/// The envelope on stdout.
let printJson (e: Envelope) =
    let w = if isNull jsonWriter then Console.Out else jsonWriter
    w.WriteLine(JsonSerializer.Serialize(e, jsonOptions))
    w.Flush()
