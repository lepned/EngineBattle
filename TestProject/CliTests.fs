module CliTests

open Xunit
open Cli.Spec
open Cli.Parse

// A small command line of its own, so these tests do not move with the real commands.
let private analyze =
    { command [ "analyze" ] "G" "Analyze" with
        Args = [ arg "engine" "the engine"; fenArg false "the position" ]
        Options =
          [ value "nodes" "N" "nodes" |> withDefault "1000000"
            value "movetime" "MS" "time"
            value "uci" "NAME=VALUE" "an option" |> repeatable
            json
            out "the file" ]
        OneOf = [ [ "nodes"; "movetime" ] ] }

let private pgnCheck = { command [ "pgn"; "check" ] "G" "Check" with Args = [ arg "pgn" "the PGN" ] }
let private pgnStrip = { command [ "pgn"; "strip" ] "G" "Strip" with Args = [ arg "pgn" "the PGN" ]; Options = [ force ] }
let private old = { command [ "old" ] "G" "Old" with Args = [ { Name = "arguments"; Help = ""; Required = false; Kind = Rest } ] }
let private commands = [ analyze; pgnCheck; pgnStrip; old ]

let private run (line: string) =
    match parse commands (line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
    | Run p -> p
    | other -> failwithf "expected a command to run, got %A" other

let private usage (line: string) =
    match parse commands (line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
    | UsageError(message, _) -> message
    | other -> failwithf "expected a usage error, got %A" other

[<Fact>]
let ``commands and subcommands are found, and a near miss is suggested`` () =
  Assert.Equal<string list>([ "pgn"; "check" ], (run "pgn check a.pgn").Command.Path)
  Assert.Contains("did you mean 'check'", usage "pgn chek a.pgn")
  Assert.Contains("did you mean 'analyze'", usage "analyse sf")
  Assert.Contains("needs a subcommand: check, strip", usage "pgn")

[<Fact>]
let ``options: values, switches, --name=value, repeats`` () =
  let p = run "analyze sf --nodes 500 --uci Threads=4 --uci=Hash=64 --json -o out.txt"
  Assert.Equal(Some "sf", argOf p "engine")
  Assert.Equal(Some 500, intOf p "nodes")
  Assert.Equal<string list>([ "Threads=4"; "Hash=64" ], valuesOf p "uci")
  Assert.True(has p "json")
  Assert.Equal(Some "out.txt", valueOf p "out")

[<Fact>]
let ``what is wrong with options is said, with a suggestion`` () =
  Assert.Contains("unknown option '--node'", usage "analyze sf --node 5")
  Assert.Contains("did you mean '--nodes'", usage "analyze sf --node 5")
  Assert.Contains("--nodes is given twice", usage "analyze sf --nodes 5 --nodes 6")
  Assert.Contains("--nodes needs a value", usage "analyze sf --nodes")
  Assert.Contains("--json takes no value", usage "analyze sf --json=yes")
  Assert.Contains("give only one of --nodes, --movetime", usage "analyze sf --nodes 5 --movetime 100")
  // options are case-sensitive
  Assert.Contains("unknown option '--Nodes'", usage "analyze sf --Nodes 5")

[<Fact>]
let ``a FEN may come whole, quoted, or as startpos, and a '-' field is not an option`` () =
  Assert.Equal(Some "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1", argOf (run "analyze sf 8/8/8/8/8/2k5/2Q5/K7 b - - 0 1 --nodes 5") "fen")
  Assert.Equal(Some "startpos", argOf (run "analyze sf startpos") "fen")
  // a quoted FEN arrives as one token with spaces
  match parse commands [ "analyze"; "sf"; "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1"; "--json" ] with
  | Run p -> Assert.Equal(Some "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1", argOf p "fen")
  | other -> Assert.Fail $"{other}"
  // a placement alone is a FEN of one field; the command fills in the rest later
  Assert.Equal(Some "8/8/8/8/8/2k5/2Q5/K7", argOf (run "analyze sf 8/8/8/8/8/2k5/2Q5/K7 --json") "fen")

[<Fact>]
let ``arguments: missing, unexpected, and the rest of an old verb`` () =
  Assert.Contains("missing <pgn>", usage "pgn check")
  Assert.Contains("unexpected argument 'b.pgn'", usage "pgn check a.pgn b.pgn")
  // an old verb's arguments pass untouched, options included
  Assert.Equal<string list>([ "x.pgn"; "--csv"; "f.csv" ], restOf (run "old x.pgn --csv f.csv") "arguments")
  // `--` ends the options: what follows is an argument even when it looks like one
  Assert.Equal(Some "--odd.pgn", argOf (run "pgn check -- --odd.pgn") "pgn")

[<Fact>]
let ``help, version and a bad number`` () =
  Assert.Equal(Help Overview, parse commands [])
  Assert.Equal(Help(Group "pgn"), parse commands [ "help"; "pgn" ])
  Assert.Equal(Help(Group "pgn"), parse commands [ "pgn"; "--help" ])
  Assert.Equal(Help(One pgnCheck), parse commands [ "help"; "pgn"; "check" ])
  Assert.Equal(Help(One analyze), parse commands [ "analyze"; "sf"; "-h" ])
  Assert.Equal(Version, parse commands [ "--version" ])
  let p = run "analyze sf --nodes 5k"
  let ex = Assert.Throws<Usage>(fun () -> intOf p "nodes" |> ignore)
  Assert.Contains("--nodes expects a whole number, got '5k'", ex.Data0)

[<Fact>]
let ``the help of every real command names every option it has`` () =
  for e in Registry.all do
    let text = Cli.Help.command e.Spec
    for o in e.Spec.Options do
      Assert.Contains("--" + o.Name, text)
    for a in e.Spec.Args do
      Assert.Contains("<" + a.Name, text)

[<Fact>]
let ``every outcome has its exit code`` () =
  let entries =
    [ { Cli.App.Spec = pgnCheck; Cli.App.Run = fun _ -> Cli.Output.ExitFailed }
      { Cli.App.Spec = pgnStrip; Cli.App.Run = fun p -> raise (Usage "bad") }
      { Cli.App.Spec = analyze; Cli.App.Run = fun _ -> failwith "boom" } ]
  Assert.Equal(0, Cli.App.run entries [ "help" ])
  Assert.Equal(0, Cli.App.run entries [ "version" ])
  Assert.Equal(1, Cli.App.run entries [ "pgn"; "check"; "a.pgn" ])
  Assert.Equal(2, Cli.App.run entries [ "pgn"; "strip"; "a.pgn" ])
  Assert.Equal(2, Cli.App.run entries [ "nope" ])
  Assert.Equal(2, Cli.App.run entries [ "pgn"; "check" ])
  // an exception nobody planned for is a failure, not a crash
  Assert.Equal(1, Cli.App.run entries [ "analyze"; "sf" ])

// ---------------------------------------------------------------- the real commands (phase 2)

let private real = Registry.all |> List.map (fun e -> e.Spec)

let private runReal (tokens: string list) =
  match parse real tokens with
  | Run p -> p
  | other -> failwithf "expected a command to run, got %A" other

[<Fact>]
let ``tablebase takes an unquoted FEN whole, and refuses what it does not know`` () =
  let p = runReal [ "tablebase"; "8/8/8/8/8/2k5/8/K6Q"; "b"; "-"; "-"; "0"; "1"; "--json"; "--syzygy"; "D:/t" ]
  Assert.Equal(Some "8/8/8/8/8/2k5/8/K6Q b - - 0 1", argOf p "fen")
  Assert.Equal(Some "D:/t", valueOf p "syzygy")
  Assert.True(has p "json")
  match parse real [ "tablebase"; "startpos"; "--jsn" ] with
  | UsageError(message, _) -> Assert.Contains("did you mean '--json'", message)
  | other -> Assert.Fail $"{other}"

[<Fact>]
let ``engine, limit and position options read the same in every command`` () =
  let p = runReal [ "analyze"; "sf.json"; "startpos"; "--moves"; "e4 e5"; "--uci"; "Threads=8"; "--uci"; "Hash=64"; "--depth"; "12" ]
  Assert.Equal(Cli.Groups.Depth 12, Cli.Groups.limit p 1_000_000)
  Assert.Equal<(string * string) list>([ "Threads", "8"; "Hash", "64" ], Cli.Groups.uciPairs "uci" (valuesOf p "uci"))
  Assert.Equal<string list>([ "e2e4"; "e7e5" ], Cli.Groups.movesFrom p (Cli.Groups.fen p "fen"))
  // the default limit is the command's own
  Assert.Equal(Cli.Groups.Nodes 1, Cli.Groups.limit (runReal [ "values"; "position"; "sf.json" ]) 1)
  // two limits, a bad number, a bad option value, illegal moves: usage errors that say so
  match parse real [ "analyze"; "sf.json"; "--nodes"; "5"; "--depth"; "3" ] with
  | UsageError(message, _) -> Assert.Contains("give only one of", message)
  | other -> Assert.Fail $"{other}"
  let bad = runReal [ "analyze"; "sf.json"; "--nodes"; "0"; "--uci"; "Threads"; "--moves"; "e5" ]
  Assert.Contains("must be above 0", (Assert.Throws<Usage>(fun () -> Cli.Groups.limit bad 1 |> ignore)).Data0)
  Assert.Contains("NAME=VALUE", (Assert.Throws<Usage>(fun () -> Cli.Groups.uciPairs "uci" (valuesOf bad "uci") |> ignore)).Data0)
  Assert.Contains("not a legal line", (Assert.Throws<Usage>(fun () -> Cli.Groups.movesFrom bad Cli.Groups.startFen |> ignore)).Data0)
  // a FEN that is not one
  let notFen = runReal [ "tablebase"; "8/8/8/9/8/8/8/8"; "w" ]
  Assert.Contains("not a valid FEN", (Assert.Throws<Usage>(fun () -> Cli.Groups.fen notFen "fen" |> ignore)).Data0)
// ---------------------------------------------------------------- the reference in ConsoleTools.md

[<Fact>]
let ``ConsoleTools.md carries every command's help as the program prints it`` () =
  // the repo root, from TestProject/bin/<config>/<tfm>
  let doc = System.IO.File.ReadAllText(System.IO.Path.Combine(System.AppContext.BaseDirectory, "..", "..", "..", "..", "ConsoleTools.md")).Replace("\r\n", "\n")
  for e in Registry.all do
    let help = (Cli.Help.command e.Spec).Replace("\r\n", "\n")
    // the doc states the exit codes once, not under every command
    let body = help.Substring(0, help.IndexOf("\nExit codes:")).TrimEnd()
    Assert.True(doc.Contains($"### {Cli.Spec.name e.Spec}\n"), $"no section for '{Cli.Spec.name e.Spec}'")
    Assert.True(doc.Contains(body), $"the help of '{Cli.Spec.name e.Spec}' in ConsoleTools.md is not what eb-cli prints: write it again from 'eb-cli help {Cli.Spec.name e.Spec}'")