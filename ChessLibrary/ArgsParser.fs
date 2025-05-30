module ChessLibrary.ArgsParser

open CommandLine

let findVerbs (argv:string[]) (verbs: string list) =
  let input = argv |> Array.toList
  let rec loop (argv:string list) temp acc =
    match argv with
    | [] -> 
        if temp |> List.isEmpty then
          acc //|> List.rev 
        else 
          temp::acc //|> List.rev 
    | x::xs -> 
      if List.exists(fun v -> v = x) verbs then
        if temp |> List.isEmpty then
          loop xs [x] acc
        else
          loop xs [x] (temp::acc)
      else
        loop xs (x::temp) acc
  loop input [] []

[<Verb("engine", HelpText = "Add options to your engine")>]
type EngineOptions =
    {
        [<Option('c', "conf", Required = true,
            HelpText = "Use an engine with a name from engines.json configuration file.")>]
        Conf: string
        [<Option('n', "name", Required = false,
            HelpText = "Set the name for the engine.")>]
        Name: string
        [<Option('m', "cmd", Required = false,
            HelpText = "Set the command for running the engine.")>]
        Cmd: string
        [<Option('d', "dir", Required = false,
            HelpText = "Set the working directory for the engine.")>]
        Dir: string option
        [<Option('a', "arg", Required = false,
            HelpText = "Pass a command line argument to the engine.")>]
        Arg: string option
        [<Option('i', "initstr", Required = false,
            HelpText = "Send text to the engine’s standard input at startup.")>]
        InitStr: string option
    }
    static member Empty = { Conf = ""; Name = ""; Cmd = ""; Dir = None; Arg = None; InitStr = None; }

[<Verb("time", HelpText = "Add time control to your engine")>]
type TimeControlOptions =
    {
        [<Option('t', "tc", Required = false,
            HelpText = "Set the time control to TIMECONTROL. The format is: moves/time+increment.")>]
        Tc: string
        [<Option('s', "st", Required = true,
            HelpText = "Set the time limit for each move to N seconds.")>]
        St: int option
    }
    static member Empty = { Tc = ""; St = None }

[<Verb("tourny", HelpText = "Configure your tournament")>]
type TournamentOptions =
    {
        [<Option('r', "roundrobin", Required = false,
            HelpText = "Round robin tournament mode")>]
        Roundrobin: bool option
        [<Option('g', "gauntlet", Required = false,
            HelpText = "Gauntlet tournament mode")>]
        Gauntlet: bool option
        [<Option('o', "openbook", Required = false,
            HelpText = "Opening book for tournament run")>]
        Openbook: string option
        [<Option('n', "rounds", Required = false,
            HelpText = "Number of rounds to play")>]
        Rounds: int option
    }

