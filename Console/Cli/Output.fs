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

/// Set by the first Ctrl+C of a long run: the run reports what it has, and the exit code is 130.
let mutable interrupted = false

/// One handler for the process: the first Ctrl+C cancels the token, the second exits.
let private ctrlC =
    lazy
        (let cts = new Threading.CancellationTokenSource()
         Console.CancelKeyPress.Add(fun e ->
             e.Cancel <- true
             if not interrupted then
                 interrupted <- true
                 warn "stopping - Ctrl+C again to stop at once"
                 cts.Cancel()
             else
                 Console.Out.Flush()
                 exit ExitInterrupted)
         cts.Token)

/// Ctrl+C for a long run: the first press cancels the returned token - the run stops starting new
/// work and keeps what it has - and the second ends the process at once. Exit code 130 either way.
/// Every call returns the same token.
let onCtrlC () = ctrlC.Force()

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

/// Where a command's result goes: the real stdout, also while Console.Out points at stderr.
let resultWriter () = if isNull jsonWriter then Console.Out else jsonWriter

/// Runs `f` with the library's printfn notices on stderr and the result (resultWriter) on stdout;
/// under --json that is already so.
let withResultsOnStdout (f: unit -> 'a) = if isNull jsonWriter then withJsonStdout f else f ()

/// Whether this command has printed its envelope (stdout holds one JSON document, never two).
let mutable jsonPrinted = false

/// The envelope on stdout.
let printJson (e: Envelope) =
    jsonPrinted <- true
    let w = if isNull jsonWriter then Console.Out else jsonWriter
    w.WriteLine(JsonSerializer.Serialize(e, jsonOptions))
    w.Flush()
