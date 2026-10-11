/// The command line's specification: every command, its arguments and options. The parser and the
/// help both read it, so the help cannot say anything the parser does not do.
module Cli.Spec

type ArgKind =
    /// One token.
    | Single
    /// A FEN: one quoted token, `startpos`, or up to six unquoted tokens (a shell splits a FEN at
    /// its spaces); it stops at the first option.
    | Fen
    /// Every remaining token, untouched: a command that has not moved to the new parser yet.
    | Rest

type Arg =
    { Name: string
      Help: string
      Required: bool
      Kind: ArgKind }

type Opt =
    { /// Without the leading "--".
      Name: string
      Short: char option
      /// What the value is called in help ("N", "FILE"); None for a switch, which takes none.
      Value: string option
      Repeatable: bool
      /// Shown in help when the option is not given.
      Default: string option
      Help: string }

type Command =
    { /// ["pgn"; "parse"]; one word for a command without subcommands.
      Path: string list
      /// The heading the command is listed under.
      Group: string
      Summary: string
      Description: string option
      Args: Arg list
      Options: Opt list
      /// Sets of options of which at most one may be given (--nodes, --movetime, --depth).
      OneOf: string list list
      /// Command lines with what each does.
      Examples: (string * string) list }

let name (c: Command) = String.concat " " c.Path

let arg name help = { Name = name; Help = help; Required = true; Kind = Single }
let optionalArg name help = { Name = name; Help = help; Required = false; Kind = Single }
let fenArg required help = { Name = "fen"; Help = help; Required = required; Kind = Fen }

let switch name help =
    { Name = name; Short = None; Value = None; Repeatable = false; Default = None; Help = help }

let value name meta help =
    { Name = name; Short = None; Value = Some meta; Repeatable = false; Default = None; Help = help }

let withDefault d (o: Opt) = { o with Default = Some d }
let repeatable (o: Opt) = { o with Repeatable = true }
let short c (o: Opt) = { o with Short = Some c }

/// A command with nothing but a name and a summary; the rest is added with `{ c with ... }`.
let command (path: string list) group summary =
    { Path = path
      Group = group
      Summary = summary
      Description = None
      Args = []
      Options = []
      OneOf = []
      Examples = [] }

/// `--json`: the result as one JSON document on stdout.
let json = switch "json" "Print the result as JSON on stdout (everything else goes to stderr)"

/// `-o, --out FILE` and `--force`.
let out what = value "out" "FILE" what |> short 'o'
let force = switch "force" "Write over a file that exists"
