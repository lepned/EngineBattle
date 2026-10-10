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

/// The envelope on stdout.
let printJson (e: Envelope) =
    Console.Out.WriteLine(JsonSerializer.Serialize(e, jsonOptions))
