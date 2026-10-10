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
  for e in Commands.all (fun _ -> 0) do
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
