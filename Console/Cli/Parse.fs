/// One parser for every command, driven by Spec: argv in, typed values or a usage error out. It
/// never prints and never exits; Cli.App decides what a result means.
module Cli.Parse

open System
open Cli.Spec

/// A usage mistake found after parsing (a value that is not a number, a file that is not there):
/// thrown by a command, reported by Cli.App as exit 2 with the command's usage line.
exception Usage of string

type Parsed =
    { Command: Command
      /// Positional arguments by name; a FEN given as several tokens is joined with spaces.
      Args: Map<string, string>
      /// Options by name (without "--"); a switch maps to [""], a repeated option to every value.
      Options: Map<string, string list> }

type Outcome =
    | Run of Parsed
    /// `help`, `-h`, `--help`: the overview (None), one group's commands, or one command.
    | Help of HelpTopic
    | Version
    | UsageError of message: string * command: Command option

and HelpTopic =
    | Overview
    | Group of string
    | One of Command

// ---------------------------------------------------------------- suggestions

let private distance (a: string) (b: string) =
    let a, b = a.ToLowerInvariant(), b.ToLowerInvariant()
    let d = Array2D.zeroCreate (a.Length + 1) (b.Length + 1)
    for i in 0 .. a.Length do d.[i, 0] <- i
    for j in 0 .. b.Length do d.[0, j] <- j
    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)
    d.[a.Length, b.Length]

/// The candidates close to what was typed, best first: a small edit distance, or the typed text
/// as a prefix ("pgn val" -> "validate").
let suggest (typed: string) (candidates: string seq) =
    candidates
    |> Seq.distinct
    |> Seq.map (fun c -> c, distance typed c)
    |> Seq.filter (fun (c, d) ->
        d <= max 1 (typed.Length / 3) || (typed.Length >= 2 && c.StartsWith(typed, StringComparison.OrdinalIgnoreCase)))
    |> Seq.sortBy snd
    |> Seq.map fst
    |> Seq.truncate 3
    |> List.ofSeq

let didYouMean (typed: string) (candidates: string seq) =
    match suggest typed candidates with
    | [] -> ""
    | s -> $""" (did you mean {String.Join(" or ", s |> List.map (fun x -> $"'{x}'"))}?)"""

// ---------------------------------------------------------------- commands

let private groups (commands: Command list) =
    commands |> List.filter (fun c -> c.Path.Length > 1) |> List.map (fun c -> c.Path.Head) |> List.distinct

/// The command a command line names, and the tokens after its name.
let resolve (commands: Command list) (tokens: string list) : Result<Command * string list, string * Command option> =
    let byPath path = commands |> List.tryFind (fun c -> c.Path = path)
    match tokens with
    | [] -> Error("no command given", None)
    | first :: rest ->
        match byPath [ first ] with
        | Some c -> Ok(c, rest)
        | None when List.contains first (groups commands) ->
            let subs = commands |> List.filter (fun c -> c.Path.Head = first) |> List.map (fun c -> c.Path.[1])
            let listed = String.Join(", ", subs)
            match rest with
            | second :: rest' ->
                match byPath [ first; second ] with
                | Some c -> Ok(c, rest')
                | None -> Error($"'{first} {second}' is not a command{didYouMean second subs}; {first} has: {listed}", None)
            | [] -> Error($"'{first}' needs a subcommand: {listed}", None)
        | None ->
            let names = (commands |> List.map name) @ groups commands
            Error($"'{first}' is not a command{didYouMean first names}", None)

// ---------------------------------------------------------------- options and arguments

/// "--name" or "-x"; a lone "-" is a FEN field (no castling, no en passant), "-5" a number.
let isOption (token: string) =
    (token.StartsWith "--" && token.Length > 2) || (token.Length = 2 && token.[0] = '-' && Char.IsLetter token.[1])

let private isHelp (token: string) = token = "--help" || token = "-h"

/// The options and arguments of one command.
let parseCommand (c: Command) (tokens: string list) : Outcome =
    let usage message = UsageError(message, Some c)
    let optNames = c.Options |> List.map (fun o -> "--" + o.Name)
    let findOpt (token: string) =
        if token.StartsWith "--" then c.Options |> List.tryFind (fun o -> "--" + o.Name = token)
        else c.Options |> List.tryFind (fun o -> o.Short = Some token.[1])
    let rec go (tokens: string list) (pending: Arg list) (args: Map<string, string>) (opts: Map<string, string list>) (endOfOptions: bool) =
        match tokens with
        | [] ->
            match pending |> List.tryFind (fun a -> a.Required) with
            | Some a -> usage $"missing <{a.Name}>"
            | None ->
                let given = c.OneOf |> List.map (List.filter opts.ContainsKey) |> List.tryFind (fun g -> g.Length > 1)
                match given with
                | Some g -> usage $"""give only one of {String.Join(", ", g |> List.map (fun n -> "--" + n))}"""
                | None -> Run { Command = c; Args = args; Options = opts }
        | t :: rest when not endOfOptions && isHelp t -> Help(One c)
        // a Rest argument takes everything that is left, options included
        | _ :: _ when (match pending with { Kind = Rest } :: _ -> true | _ -> false) ->
            go [] pending.Tail (args.Add(pending.Head.Name, String.Join("\u001f", tokens))) opts true
        | "--" :: rest when not endOfOptions -> go rest pending args opts true
        | t :: rest when not endOfOptions && isOption t ->
            // --name=value is the same as --name value
            let token, inline' =
                match t.IndexOf '=' with
                | i when t.StartsWith "--" && i > 2 -> t.Substring(0, i), Some(t.Substring(i + 1))
                | _ -> t, None
            match findOpt token with
            | None -> usage $"unknown option '{token}'{didYouMean token optNames}"
            | Some o when opts.ContainsKey o.Name && not o.Repeatable -> usage $"--{o.Name} is given twice"
            | Some o ->
                let add v = opts |> Map.change o.Name (fun old -> Some((defaultArg old []) @ [ v ]))
                match o.Value, inline', rest with
                | None, Some _, _ -> usage $"--{o.Name} takes no value"
                | None, None, _ -> go rest pending args (add "") endOfOptions
                | Some _, Some v, _ -> go rest pending args (add v) endOfOptions
                | Some meta, None, v :: rest' -> go rest' pending args (add v) endOfOptions
                | Some meta, None, [] -> usage $"--{o.Name} needs a value ({meta})"
        | t :: rest ->
            match pending with
            | [] -> usage $"unexpected argument '{t}'"
            | a :: more ->
                match a.Kind with
                | Single -> go rest more (args.Add(a.Name, t)) opts endOfOptions
                | Rest -> go [] more (args.Add(a.Name, String.Join("\u001f", tokens))) opts endOfOptions
                | Fen ->
                    // one quoted FEN or `startpos` is the whole FEN; otherwise its fields follow
                    if t.Contains ' ' || t.Equals("startpos", StringComparison.OrdinalIgnoreCase) then
                        go rest more (args.Add(a.Name, t)) opts endOfOptions
                    else
                        let fields = t :: (rest |> List.takeWhile (fun x -> endOfOptions || not (isOption x)) |> List.truncate 5)
                        go (rest |> List.skip (fields.Length - 1)) more (args.Add(a.Name, String.Join(" ", fields))) opts endOfOptions
    go tokens c.Args Map.empty Map.empty false

/// A whole command line (without the program name).
let parse (commands: Command list) (tokens: string list) : Outcome =
    match tokens with
    | []
    | [ "help" ]
    | [ "--help" ]
    | [ "-h" ] -> Help Overview
    | [ "version" ]
    | [ "--version" ] -> Version
    | "help" :: topic ->
        match resolve commands topic with
        | Ok(c, []) -> Help(One c)
        | Ok(c, extra) -> UsageError($"""help takes a command, not '{String.Join(" ", extra)}'""", Some c)
        | Error _ when topic.Length = 1 && List.contains topic.Head (groups commands) -> Help(Group topic.Head)
        | Error(message, c) -> UsageError(message, c)
    | [ group; h ] when isHelp h && List.contains group (groups commands) -> Help(Group group)
    | _ ->
        match resolve commands tokens with
        | Ok(c, rest) -> parseCommand c rest
        | Error(message, c) -> UsageError(message, c)

// ---------------------------------------------------------------- reading what was parsed

/// The tokens of a `Rest` argument, as they were given.
let restOf (p: Parsed) (name: string) =
    match p.Args.TryFind name with
    | Some s when s <> "" -> s.Split('\u001f') |> List.ofArray
    | _ -> []

let argOf (p: Parsed) (name: string) = p.Args.TryFind name
let has (p: Parsed) (name: string) = p.Options.ContainsKey name
let valueOf (p: Parsed) (name: string) = p.Options.TryFind name |> Option.bind List.tryLast
let valuesOf (p: Parsed) (name: string) = p.Options.TryFind name |> Option.defaultValue []

/// A whole number, or a usage error that names the option.
let intOf (p: Parsed) (name: string) =
    valueOf p name
    |> Option.map (fun v ->
        match Int32.TryParse(v, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
        | true, n -> n
        | _ -> raise (Usage $"--{name} expects a whole number, got '{v}'"))

/// A number with a decimal point allowed ("0.5"), or a usage error that names the option.
let floatOf (p: Parsed) (name: string) =
    valueOf p name
    |> Option.map (fun v ->
        match Double.TryParse(v, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, x -> x
        | _ -> raise (Usage $"--{name} expects a number, got '{v}'"))
