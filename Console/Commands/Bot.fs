/// bot: a Lichess bot account played by an engine (ChessLibrary LichessBot). The token comes from
/// EB_LICHESS_BOT_TOKEN only, so it is never on the command line.
module Commands.Bot

open System
open System.IO
open System.Threading
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.App
open ChessLibrary
open ChessLibrary.LichessBotLogic

[<Literal>]
let Group = "Lichess bot"

let private tokenVar = "EB_LICHESS_BOT_TOKEN"

let private server = value "server" "URL" "The Lichess server" |> withDefault "https://lichess.org"

let private token () =
    match Environment.GetEnvironmentVariable tokenVar with
    | t when not (String.IsNullOrWhiteSpace t) -> t.Trim()
    | _ -> raise (Usage $"set {tokenVar} to the bot account's API token (scopes bot:play and challenge:write)")

let private client (p: Parsed) = new LichessBotClient(defaultArg (valueOf p "server") "https://lichess.org", token ())

let runSpec =
    { command [ "bot"; "run" ] Group "Play on Lichess as a bot with an engine, until Ctrl+C" with
        Description =
          Some "Goes online with the bot account of EB_LICHESS_BOT_TOKEN and plays the challenges the filter takes, declining the rest; every game goes to --pgn and Ctrl+C goes offline. With --nodes one engine plays all games. --matchmaking finds bot opponents, --book plays the opening from a Polyglot book. Chat: !commands."
        Args = [ arg "engine" Cli.Groups.engineArgHelp ]
        Options =
          [ value "tc" "M+S,..." "Time controls taken, minutes+seconds (1+0,3+2,0.5+0); every one but UltraBullet when not given"
            value "variants" "LIST" "Variants taken: standard, chess960, fromPosition" |> withDefault "standard"
            value "opponents" "humans|bots|anyone" "Who may challenge" |> withDefault "anyone"
            switch "rated-only" "Decline casual games"
            switch "casual-only" "Decline rated games"
            value "games" "N" "Games at once; a challenge while full is declined with 'later'" |> withDefault "1"
            value "nodes" "N" "Nodes per move whatever the clock, instead of the clock (1: no search)"
            value "overhead" "MS" "Time kept back from each move for lag, by the clock" |> withDefault "100"
            value "pgn" "FILE" "The games, appended" |> withDefault "lichess-bot.pgn"
            value "greeting" "TEXT" "Said at the start of a game (empty: nothing)"
            value "goodbye" "TEXT" "Said at the end of a game" |> withDefault "Good game!"
            value "book" "FILE" "A Polyglot book (.bin) for the opening: a move drawn by the book's weights while it has one"
            value "book-depth" "N" "Plies the book may play, counted from the start of the game" |> withDefault "8"
            switch "engine-per-game" "With --nodes, each game its own engine (default: one engine plays every game, one net loaded)"
            switch "matchmaking" "Challenge bots online on its own whenever a place is free (after --match-pause once a game ends)"
            value "match-tc" "M+S,..." "Matchmaking's time controls, one drawn per challenge" |> withDefault "3+0,3+2,5+3"
            value "match-range" "N" "Challenge bots rated within N of the bot's own rating at that speed" |> withDefault "300"
            switch "match-rated" "Matchmaking's challenges are rated (casual when not given)"
            value "match-pause" "S" "Seconds after a game ends before matchmaking challenges (none while a game runs and a place is free)" |> withDefault "60"
            value "match-start" "N" "The bot's rating at a speed where it has no rated games yet" |> withDefault "2000"
            Cli.Groups.uci
            server ]
        OneOf = [ [ "rated-only"; "casual-only" ] ]
        Examples =
          [ "eb-cli bot run Ceres_C1.json --nodes 1 --tc 1+0,3+0,3+2", "A one-node policy bot for bullet and blitz"
            "eb-cli bot run Ceres_C1.json --nodes 1 --book performance.bin", "The same, with the openings from a book"
            "eb-cli bot run Ceres_C1.json --nodes 1 --matchmaking --match-tc 3+0,5+3 --match-rated", "Plays rated bots on its own, between the challenges it gets"
            "eb-cli bot run sf.json --tc 3+2,5+3 --opponents humans --casual-only", "Casual blitz against people" ] }

let run (p: Parsed) =
    let positive name dflt = match intOf p name with Some n when n > 0 -> n | Some n -> raise (Usage $"--{name} must be above 0, got {n}") | None -> dflt
    let tcs =
        match parseTimeControls (defaultArg (valueOf p "tc") "") with
        | Ok t -> t
        | Error e -> raise (Usage $"--tc: {e}")
    let variants = (defaultArg (valueOf p "variants") "standard").Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> List.ofArray
    let opponents =
        match defaultArg (valueOf p "opponents") "anyone" with
        | "humans" -> Humans
        | "bots" -> Bots
        | "anyone" -> Anyone
        | other -> raise (Usage $"--opponents expects humans, bots or anyone, got '{other}'")
    let limit =
        match intOf p "nodes" with
        | Some n when n > 0 -> FixedNodes n
        | Some n -> raise (Usage $"--nodes must be above 0, got {n}")
        | None ->
            match intOf p "overhead" with
            | Some ms when ms < 0 -> raise (Usage $"--overhead must be 0 or more, got {ms}")
            | ms -> ByClock (defaultArg ms 100)
    let book =
        match valueOf p "book" with
        | Some f when not (File.Exists f) -> raise (Usage $"--book: {f} not found")
        | Some f -> Path.GetFullPath f
        | None -> ""
    let engine = Cli.Groups.engineFrom p (argOf p "engine").Value "uci"
    let settings : LichessBotSettings =
        { Engine = engine
          Filter =
            { TimeControls = tcs; Variants = variants; Rated = not (has p "casual-only"); Casual = not (has p "rated-only")
              Opponents = opponents; MaxGames = positive "games" 1 }
          Limit = limit
          PgnPath = Path.GetFullPath(defaultArg (valueOf p "pgn") "lichess-bot.pgn")
          Greeting = defaultArg (valueOf p "greeting") ""
          Goodbye = defaultArg (valueOf p "goodbye") "Good game!"
          Book = book
          BookDepth = positive "book-depth" 8
          ShareEngine = not (has p "engine-per-game")
          Matchmaking =
            if not (has p "matchmaking") then None
            else
                let tcs =
                    match parseTimeControls (defaultArg (valueOf p "match-tc") "3+0,3+2,5+3") with
                    | Ok [] -> raise (Usage "--match-tc needs at least one time control")
                    | Ok t -> t
                    | Error e -> raise (Usage $"--match-tc: {e}")
                let pauseSeconds = match intOf p "match-pause" with Some s when s < 0 -> raise (Usage $"--match-pause must be 0 or more, got {s}") | s -> defaultArg s 60
                Some { TimeControls = tcs; RatingRange = positive "match-range" 300; StartRating = positive "match-start" 2000
                       Rated = has p "match-rated"; PauseSeconds = pauseSeconds; GapSeconds = 30 } }
    use c = client p
    let ct = onCtrlC ()
    let log (text: string) = printfn "%s  %s" (DateTime.Now.ToString "HH:mm:ss") text
    let bot = LichessBot(c, settings, Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance, log)
    printfn "Games go to %s; Ctrl+C goes offline" settings.PgnPath
    match bot.Run(ct).GetAwaiter().GetResult() with
    | Result.Ok () -> if ct.IsCancellationRequested then ExitInterrupted else ExitOk
    | Result.Error e ->
        error e
        ExitFailed

let upgradeSpec =
    { command [ "bot"; "upgrade" ] Group "Turn the account into a Lichess bot account, for good" with
        Description =
          Some "Upgrades the account of EB_LICHESS_BOT_TOKEN to a bot account. This cannot be undone, and Lichess allows it only for an account that has never played a game: use a new account for the bot. Needs --yes."
        Options = [ switch "yes" "Yes, upgrade it for good"; server ]
        Examples = [ "eb-cli bot upgrade --yes", "The new account becomes a bot account" ] }

let upgrade (p: Parsed) =
    if not (has p "yes") then raise (Usage "upgrading cannot be undone: give --yes to do it")
    use c = client p
    let ct = CancellationToken.None
    match c.Account(ct).GetAwaiter().GetResult() with
    | Result.Error e -> error e; ExitFailed
    | Result.Ok (_, name, "BOT") -> printfn "%s is a bot account already" name; ExitOk
    | Result.Ok (_, name, _) ->
        match c.UpgradeToBot(ct).GetAwaiter().GetResult() with
        | LichessReply.Done _ -> printfn "%s is a bot account now" name; ExitOk
        | LichessReply.RateLimited (why, _) -> error ("Lichess asks for a pause: " + (if why = "" then "try again in a minute" else why)); ExitFailed
        | LichessReply.Refused (code, body) -> error $"Lichess refused the upgrade (HTTP {code}): {body}"; ExitFailed

let challengeSpec =
    { command [ "bot"; "challenge" ] Group "Challenge a player or a bot on Lichess from the bot account" with
        Description =
          Some "Sends a standard-chess challenge from the bot account of EB_LICHESS_BOT_TOKEN and prints its id. The bot running with that account (bot run, or the bot page) plays the game when it is accepted; nothing plays it when no bot is online. Lichess limits how many challenges a bot may send."
        Args = [ arg "user" "The Lichess username to challenge" ]
        Options =
          [ value "tc" "M+S" "The time control, minutes+seconds" |> withDefault "3+2"
            switch "rated" "A rated game (casual when not given)"
            value "color" "random|white|black" "The bot's colour" |> withDefault "random"
            server ]
        Examples =
          [ "eb-cli bot challenge maia1 --tc 3+0", "A casual 3+0 game against another bot"
            "eb-cli bot challenge SomeBot --tc 5+3 --rated --color white", "Rated, the bot with White" ] }

let challenge (p: Parsed) =
    let seconds, increment =
        match parseTimeControls (defaultArg (valueOf p "tc") "3+2") with
        | Ok [ tc ] -> tc
        | Ok _ -> raise (Usage "--tc takes one time control, like 3+2")
        | Error e -> raise (Usage $"--tc: {e}")
    let color =
        match defaultArg (valueOf p "color") "random" with
        | "random" | "white" | "black" as c -> c
        | other -> raise (Usage $"--color expects random, white or black, got '{other}'")
    let user = (argOf p "user").Value.Trim().TrimStart('@')
    use c = client p
    match c.Challenge(user, seconds, increment, has p "rated", color, CancellationToken.None).GetAwaiter().GetResult() with
    | LichessReply.Done body ->
        match challengeIdOf body with
        | Some id -> printfn "Challenged %s: https://lichess.org/%s" user id; ExitOk
        | None -> error "Lichess's answer has no challenge id"; ExitFailed
    | LichessReply.RateLimited (why, _) -> error ("Lichess asks for a pause: " + (if why = "" then "try again in a minute" else why)); ExitFailed
    | LichessReply.Refused (code, body) -> error $"Lichess refused the challenge (HTTP {code}): {errorText body}"; ExitFailed

let onlineSpec =
    { command [ "bot"; "online" ] Group "List the bots online on Lichess, with their ratings" with
        Description = Some "The bots online on Lichess now, strongest in blitz first: name, bullet, blitz and rapid rating ('-' with no games). Needs EB_LICHESS_BOT_TOKEN."
        Options = [ value "count" "N" "At most this many" |> withDefault "50"; server ]
        Examples = [ "eb-cli bot online --count 20", "The 20 first bots Lichess lists, by blitz rating" ] }

let online (p: Parsed) =
    let count = match intOf p "count" with Some n when n > 0 -> n | Some n -> raise (Usage $"--count must be above 0, got {n}") | None -> 50
    use c = client p
    match c.OnlineBots(count, CancellationToken.None).GetAwaiter().GetResult() with
    | Result.Error e -> error e; ExitFailed
    | Result.Ok lines ->
        let bots =
            lines
            |> Array.choose (fun l -> try parseOnlineBot l with _ -> None)
            |> Array.sortByDescending (fun b -> defaultArg b.Blitz 0)
        let r = function Some (v: int) -> string v | None -> "-"
        let width = bots |> Array.fold (fun w b -> max w b.Name.Length) 3
        printfn "%-*s  %6s  %6s  %6s" width "Bot" "Bullet" "Blitz" "Rapid"
        for b in bots do printfn "%-*s  %6s  %6s  %6s" width b.Name (r b.Bullet) (r b.Blitz) (r b.Rapid)
        ExitOk

let entries: Entry list =
    [ { Spec = runSpec; Run = run }
      { Spec = challengeSpec; Run = challenge }
      { Spec = onlineSpec; Run = online }
      { Spec = upgradeSpec; Run = upgrade } ]
