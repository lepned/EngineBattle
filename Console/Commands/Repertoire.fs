/// book repertoire: an opening repertoire for one side as a PGN with variations (OpeningRepertoire).
module Commands.Repertoire

open System
open System.IO
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.Groups
open Cli.App

[<Literal>]
let Group = "Puzzles, books, tournaments and tuning"

let private defaults = ChessLibrary.OpeningRepertoire.Settings.Defaults
let private num (x: float) = x.ToString(Globalization.CultureInfo.InvariantCulture)

let repertoireSpec =
    { command [ "book"; "repertoire" ] Group "An opening repertoire for one side, as a PGN with variations" with
        Description =
          Some "From the position (and --moves) the side of --side plays one move: the objectively best when the position is clear, a sound idea when it is level (|eval| below --equal) - or a riskier one (up to --risk) after which the opponent must find --traps only moves in a row, hard over the board; those come first. Up to --consider replies of the opponent are looked at, leaving out any that lose more than --catastrophe; those within --window of its best (at most --replies) are explored further, every other one gets our answer and the engine's line - marked ?!/?/?? with our ! move where it gives winning chances away. A line never ends mid-exchange. Where the opponent has one good move (the next is --only-move worse) the line runs on and the second best is punished. Every move carries [%eval] (White's view) in its comment. With --human a net that plays like people (Lc0 with a Maia net for the opponent's rating) adds the replies it plays at least --human-share of the time, however bad, and the replies explored are the most played of those near the best; each reply shows how often people play it. With --contempt an Lc0 picks the ideas for that rating gap, the evals staying with an objective engine (--judge, else a second copy of the engine without contempt). Searches are kept in <out>.judge.cache (and .ideas.cache, .human.cache): a run started again, or with other tree settings, does not search a position twice. The first Ctrl+C stops; run again to go on from the cache."
        Args = [ arg "engine" engineArgHelp; fenArg false $"{fenHelp} (default startpos)" ]
        Options =
          [ moves
            value "side" "w|b" "The side the repertoire is for"
            value "plies" "N" "Plies to explore from the position" |> withDefault (string defaults.Plies)
            value "consider" "N" "Opponent replies looked at per position (the engine's best and the book's)" |> withDefault (string defaults.Consider)
            value "catastrophe" "PAWNS" "Replies that lose more than this against the opponent's best are left out" |> withDefault (num defaults.Catastrophe)
            value "replies" "N" "Replies explored further per position; the rest considered get our answer only" |> withDefault (string defaults.Replies)
            value "window" "PAWNS" "Replies this close to the opponent's best are explored further" |> withDefault (num defaults.Window)
            value "ideas" "N" "Our moves looked at in a level position" |> withDefault (string defaults.Ideas)
            value "equal" "PAWNS" "Below this either way a position is level: ideas instead of the best move" |> withDefault (num defaults.Equal)
            value "margin" "PAWNS" "An idea may cost this much against the objective best" |> withDefault (num defaults.Margin)
            value "traps" "N" "An idea after which the opponent must find N only moves in a row may cost up to --risk (0: off)" |> withDefault (string defaults.Traps)
            value "risk" "PAWNS" "What such an idea may cost against the objective best" |> withDefault (num defaults.Risk)
            value "only-move" "PAWNS" "The opponent's best this far ahead of its second is an only move" |> withDefault (num defaults.OnlyMove)
            value "decided" "PAWNS" "From this either way the line ends with the engine's PV" |> withDefault (num defaults.Decided)
            value "keep" "all|critical" "critical: only the mainline and the branches with an opponent mistake (? or ??), an only move or a trap" |> withDefault "all"
            value "book" "FILE" "A Polyglot book (.bin): its moves played at least --book-share are answered too"
            value "book-share" "PCT" "The share of a book move to answer it" |> withDefault (num defaults.BookShare)
            value "human" "ENGINE" "A net that plays like the opponent (Lc0 with a Maia net): its policy at one node is how often people play each reply"
            value "uci-human" "NAME=VALUE" "Set a UCI option of the human net" |> repeatable
            value "human-share" "PCT" "Replies the human net plays at least this share (percent) are answered, however bad" |> withDefault (num defaults.HumanShare)
            value "contempt" "RATING,OPPONENT" "Ideas from Lc0's contempt for this rating gap (e.g. 2200,1700)"
            value "judge" "ENGINE" "The engine for the evals (default: the engine)"
            value "uci-judge" "NAME=VALUE" "Set a UCI option of the judge" |> repeatable ]
          @ limitOptions 10_000
          @ [ uci; out "The PGN (default Repertoire_<side>.pgn)"; force ]
        OneOf = [ limitOneOf ]
        Examples =
          [ "eb-cli book repertoire lc0.json --moves \"e4 d5\" --side b -o scandi.pgn", "Black's answers to everything White tries after 1.e4 d5"
            "eb-cli book repertoire lc0.json --moves \"d4 d5 Nf3 Nf6 g3\" --side w --contempt 2300,1800", "Ideas for White against a weaker player, evals from Lc0 without contempt"
            "eb-cli book repertoire lc0.json --moves \"e4 d5\" --side b --book human.bin --plies 14", "Also the moves people play, deeper"
            "eb-cli book repertoire sf.json --moves \"e4 c5 Nf3 e6\" --side b --human maia1500.json", "The replies 1500 players make, their mistakes punished" ] }

/// What makes a cache valid: the engine, its options and the limit.
let private signature (config: ChessLibrary.TypesDef.CoreTypes.EngineConfig) (lim: Limit) =
    ChessLibrary.RepertoireBuild.signature config lim.Text

let repertoire (p: Parsed) =
    let config = engine p "engine" "uci"
    let fenText = fen p "fen"
    let played = movesFrom p fenText
    let lim = limit p 10_000
    let white =
        match valueOf p "side" with
        | Some "w" | Some "white" -> true
        | Some "b" | Some "black" -> false
        | Some other -> raise (Usage $"--side expects w or b, got '{other}'")
        | None -> raise (Usage "give the side the repertoire is for: --side w or --side b")
    let count name dflt =
        match intOf p name with
        | Some n when n < 1 -> raise (Usage $"--{name} must be at least 1, got {n}")
        | n -> defaultArg n dflt
    let pawns name dflt =
        match floatOf p name with
        | Some x when x < 0.0 -> raise (Usage $"--{name} cannot be negative, got {x}")
        | x -> defaultArg x dflt
    let settings =
        { defaults with
            White = white
            Plies = count "plies" defaults.Plies
            Consider = count "consider" defaults.Consider
            Catastrophe = pawns "catastrophe" defaults.Catastrophe
            Replies = count "replies" defaults.Replies
            Window = pawns "window" defaults.Window
            Ideas = count "ideas" defaults.Ideas
            Equal = pawns "equal" defaults.Equal
            Margin = pawns "margin" defaults.Margin
            Traps = (match intOf p "traps" with Some n when n < 0 -> raise (Usage $"--traps cannot be negative, got {n}") | n -> defaultArg n defaults.Traps)
            Risk = pawns "risk" defaults.Risk
            OnlyMove = pawns "only-move" defaults.OnlyMove
            Decided = pawns "decided" defaults.Decided
            BookShare = pawns "book-share" defaults.BookShare
            HumanShare = pawns "human-share" defaults.HumanShare
            CriticalOnly =
                match defaultArg (valueOf p "keep") "all" with
                | "all" -> false
                | "critical" -> true
                | other -> raise (Usage $"--keep expects all or critical, got '{other}'") }
    let book =
        valueOf p "book"
        |> Option.map (fun path ->
            let full = inputFile "book" path
            if not (ChessLibrary.PolyglotFormat.isBookPath full) then raise (Usage "--book expects a Polyglot book (.bin)")
            full)
    // the ideas' engine carries the contempt; the judge is objective
    let contempt =
        valueOf p "contempt"
        |> Option.map (fun text ->
            match text.Split(',') |> Array.map (fun s -> Int32.TryParse(s.Trim())) with
            | [| true, rating; true, opponent |] -> rating, opponent
            | _ -> raise (Usage $"--contempt expects RATING,OPPONENT, e.g. 2200,1700, got '{text}'"))
    let judgeConfig =
        match valueOf p "judge" with
        | Some j -> engineFrom p j "uci-judge"
        | None -> config
    let sideName = if white then "White" else "Black"
    let outPath = outputFile p (defaultArg (valueOf p "out") $"Repertoire_{sideName}.pgn")
    let start = after fenText played
    let status = (ChessLibrary.BoardUtils.getPositionStatus start).Status
    if status = "checkmate" || status = "stalemate" then raise (Usage $"the position is {status}: nothing to explore")
    let request : ChessLibrary.RepertoireBuild.Request =
        { Judge = judgeConfig
          Ideas = contempt |> Option.map (fun (rating, opponent) -> ChessLibrary.RepertoireBuild.withContempt config white rating opponent)
          Human = valueOf p "human" |> Option.map (fun h -> engineFrom p h "uci-human")
          Book = book
          Settings = settings
          Fen = fenText
          Moves = played
          Go = lim.Go
          LimitText = lim.Text
          OutPath = outPath
          Title = $"Repertoire for {sideName}" }
    let token = onCtrlC ()
    let onSearch (n: int) = if n % 10 = 0 then info $"  {n} positions searched"
    try
        let outcome = ChessLibrary.RepertoireBuild.run request info onSearch token
        info $"Searched {outcome.Searched} positions in {outcome.Elapsed:``hh\:mm\:ss``}"
        printfn "%s: %d lines, the deepest %d plies" outPath outcome.Report.Lines outcome.Report.Depth
        ExitOk
    with
    | :? OperationCanceledException ->
        warn "stopped; run the same command again to go on from the cache"
        ExitInterrupted
    | ChessLibrary.RepertoireBuild.NoContempt message -> raise (Usage $"--{message}")

// ---------------------------------------------------------------- book bot

let private botDefaults = ChessLibrary.OpeningRepertoire.BotBookSettings.Defaults

let botSpec =
    { command [ "book"; "bot" ] Group "An opening book (.bin) for a bot, both sides, searched by an engine" with
        Description =
          Some "Searches a Polyglot book for a bot that plays White and Black from the start position. In a position of ours the engine's best move goes in, and every move within --within of it (the bot varies its play; each weighs 1). The opponent's replies explored are its best --replies within --window, and every move the games of --games show at least --min-games times (with --player, only the moves of that player's opponents: what is played against the bot). From --narrow-from on the tree no longer branches: our best move and the opponent's best reply (and the games' moves), so the lines run deep at little cost. With --policy the net's policy decides instead of evals (a net engine with VerboseMoveStats; --nodes 1 is enough and fast): our moves with --policy-within of the top move's policy, the opponent's replies with --min-policy. The book for Black answers --first. A position at --decided or more either way ends the line. The tree is written as PGN next to the book (<out>.pgn) to look through. Searches are kept in <out>.cache: a run started again, or with other settings, does not search a position twice. The first Ctrl+C stops; run again to go on from the cache."
        Args = [ arg "engine" engineArgHelp ]
        Options =
          [ value "plies" "N" "Plies from the start position" |> withDefault (string botDefaults.Plies)
            value "within" "PAWNS" "Our moves this close to the best go in beside it" |> withDefault (num botDefaults.Within)
            value "ideas" "N" "Our moves looked at per position" |> withDefault (string botDefaults.Ideas)
            value "replies" "N" "The engine's replies for the opponent explored per position" |> withDefault (string botDefaults.Replies)
            value "window" "PAWNS" "Replies this close to the opponent's best are explored" |> withDefault (num botDefaults.Window)
            value "decided" "PAWNS" "From this either way the line ends" |> withDefault (num botDefaults.Decided)
            value "first" "MOVES" "White's first moves the book for Black answers, comma-separated (none: the engine's and the games')" |> withDefault "e4,d4,c4,Nf3"
            value "games" "FILE" "Games (.pgn) whose moves are answered too; Chess960 games are skipped"
            value "player" "NAME" "Only the moves of this player's opponents in --games (the bot's account)"
            value "min-games" "N" "Times a move of the games must be played to be answered" |> withDefault (string botDefaults.MinGames)
            value "narrow-from" "PLY" "From this ply on only our best move and the opponent's best reply (and the games' moves)" |> withDefault (string botDefaults.NarrowFrom)
            switch "policy" "The net's policy decides instead of evals (Lc0, Ceres, Mycel with VerboseMoveStats; with --nodes 1 fast)"
            value "policy-within" "SHARE" "With --policy: our moves with this share of the top move's policy go in beside it" |> withDefault (num botDefaults.PolicyWithin)
            value "min-policy" "PCT" "With --policy: the opponent's replies with this policy in percent" |> withDefault (num (100.0 * botDefaults.MinPolicy)) ]
          @ limitOptions 10_000
          @ [ uci; out "The book (default Bot.bin)"; force ]
        OneOf = [ limitOneOf ]
        Examples =
          [ "eb-cli book bot Stockfish19.json --movetime 2000 --uci Threads=16 -o mycel.bin", "Both sides, 16 plies, 2 seconds a position"
            "eb-cli book bot sf.json --movetime 2000 --games lichess-bot.pgn --player lepbot -o mycel.bin", "Also every move the bot's opponents played" ] }

let bot (p: Parsed) =
    let config = engine p "engine" "uci"
    let lim = limit p 10_000
    let count name dflt =
        match intOf p name with
        | Some n when n < 1 -> raise (Usage $"--{name} must be at least 1, got {n}")
        | n -> defaultArg n dflt
    let pawns name dflt =
        match floatOf p name with
        | Some x when x < 0.0 -> raise (Usage $"--{name} cannot be negative, got {x}")
        | x -> defaultArg x dflt
    let first =
        (defaultArg (valueOf p "first") "e4,d4,c4,Nf3").Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map (fun san ->
            match ChessLibrary.BoardUtils.pvToUci startFen san with
            | Some uci when not (uci.Contains ' ') -> uci
            | _ -> raise (Usage $"--first: '{san}' is not a first move of White"))
        |> List.ofArray
    let settings : ChessLibrary.OpeningRepertoire.BotBookSettings =
        { Plies = count "plies" botDefaults.Plies
          Within = pawns "within" botDefaults.Within
          Ideas = count "ideas" botDefaults.Ideas
          Replies = count "replies" botDefaults.Replies
          Window = pawns "window" botDefaults.Window
          Decided = pawns "decided" botDefaults.Decided
          First = first
          MinGames = count "min-games" botDefaults.MinGames
          NarrowFrom = count "narrow-from" botDefaults.NarrowFrom
          PolicyWithin = pawns "policy-within" botDefaults.PolicyWithin
          MinPolicy = (pawns "min-policy" (100.0 * botDefaults.MinPolicy)) / 100.0 }
    let outPath = outputFile p (defaultArg (valueOf p "out") "Bot.bin")
    if not (ChessLibrary.PolyglotFormat.isBookPath outPath) then raise (Usage "-o expects a Polyglot book (.bin)")
    let pgnPath = outputFile p (Path.ChangeExtension(outPath, ".pgn"))
    let player = defaultArg (valueOf p "player") ""
    let played =
        match valueOf p "games" with
        | Some path ->
            let full = inputFile "games" path
            let moves = ChessLibrary.PolyglotLines.playedMoves (ChessLibrary.FullPGNParser.parsePgnFile full) settings.Plies player
            let whose = if player <> "" then $" (opponents of {player})" else ""
            info $"{moves.Count} positions with moves from {Path.GetFileName full}{whose}"
            fun key -> match moves.TryGetValue key with | true, m -> m | _ -> []
        | None ->
            if player <> "" then raise (Usage "--player needs --games")
            fun _ -> []
    let policy = has p "policy"
    let token = onCtrlC ()
    let searched = ref 0
    let onSearch (_: string) =
        let n = Threading.Interlocked.Increment &searched.contents
        if n % 10 = 0 then info $"  {n} positions searched"
    info $"Starting {config.Name}"
    let analyzer = Engines.start config
    let agent =
        if policy then
            new ChessLibrary.OpeningRepertoire.SearchAgent(config.Name, signature config lim + " | policy", Some(outPath + ".cache"), (fun f m _ -> Engines.policyLines analyzer f m lim), onSearch)
        else
            new ChessLibrary.OpeningRepertoire.SearchAgent(config.Name, signature config lim, Some(outPath + ".cache"), (fun f m k -> Engines.lines analyzer (after f m) k lim), onSearch)
    try
        try
            if agent.Loaded > 0 then info $"  {agent.Loaded} positions from {Path.GetFileName outPath}.cache"
            let ours = if policy then $"our moves with {num settings.PolicyWithin} of the top policy" else $"our moves within {num settings.Within}"
            info $"Book for both sides: {lim.Text} per position, {settings.Plies} plies, {ours}"
            let sw = Diagnostics.Stopwatch.StartNew()
            let book = Async.RunSynchronously(ChessLibrary.OpeningRepertoire.botBook settings agent played, cancellationToken = token)
            if book.Entries.Length = 0 then raise (Usage "the engine gave no moves: no book written")
            ChessLibrary.PolyglotFormat.write outPath book.Entries |> ignore
            File.WriteAllText(pgnPath, book.Pgn)
            info $"Searched {searched.Value} positions in {sw.Elapsed:``hh\:mm\:ss``}"
            let positions = book.Entries |> Array.distinctBy (fun e -> e.Key) |> Array.length
            printfn "%s: %d positions, %d moves (the tree: %s)" outPath positions book.Entries.Length pgnPath
            ExitOk
        with :? OperationCanceledException ->
            warn "stopped; run the same command again to go on from the cache"
            ExitInterrupted
    finally
        agent.Close()
        Engines.quit analyzer

let entries: Entry list = [ { Spec = repertoireSpec; Run = repertoire }; { Spec = botSpec; Run = bot } ]
