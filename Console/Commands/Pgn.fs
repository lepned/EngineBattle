/// pgn parse / validate / strip / endings / elo / speed / deviations: tools that read a PGN file.
/// Files are streamed, never held whole; progress goes to stderr every 100,000 games.
module Commands.Pgn

open System
open System.IO
open ChessLibrary
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.Groups
open Cli.App

[<Literal>]
let Group = "PGN files"

let private n0 (n: int64) = n.ToString("N0", Globalization.CultureInfo.InvariantCulture)
let private n0i (n: int) = n0 (int64 n)
let private mb (path: string) = float (FileInfo(path).Length) / 1048576.0
let private peakMb () = float (Diagnostics.Process.GetCurrentProcess().PeakWorkingSet64) / 1048576.0

/// The PGN of a command's <pgn> argument, which must exist.
let private pgnArg (p: Parsed) = inputFile "PGN file" (argOf p "pgn").Value

let private pgnSubject = arg "pgn" "a PGN file"

/// The [Event lines of a PGN file, for the records the parser made no game of: a vectorised search
/// for a newline and "[Event " in 1 MB blocks (a UTF-8 BOM before the first is skipped).
let countEventLines (path: string) =
    let pattern = "\n[Event "B
    use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 1 <<< 20, FileOptions.SequentialScan)
    let bom = Array.zeroCreate<byte> 3
    if not (fs.Read(bom, 0, 3) = 3 && bom.[0] = 0xEFuy && bom.[1] = 0xBBuy && bom.[2] = 0xBFuy) then fs.Position <- 0L
    let buffer = Array.zeroCreate<byte> (1 <<< 20)
    buffer.[0] <- byte '\n' // the first line counts as one after a newline
    let mutable kept = 1
    let mutable count = 0
    let mutable read = fs.Read(buffer, kept, buffer.Length - kept)
    while read > 0 do
        let filled = kept + read
        let mutable rest = ReadOnlySpan<byte>(buffer, 0, filled)
        let mutable at = rest.IndexOf(ReadOnlySpan<byte>(pattern))
        while at >= 0 do
            count <- count + 1
            rest <- rest.Slice(at + 1)
            at <- rest.IndexOf(ReadOnlySpan<byte>(pattern))
        // the tail a match could still start in moves to the front
        kept <- min filled (pattern.Length - 1)
        Buffer.BlockCopy(buffer, filled - kept, buffer, 0, kept)
        read <- fs.Read(buffer, kept, buffer.Length - kept)
    count

// ---------------------------------------------------------------- parse

let parseSpec =
    { command [ "pgn"; "parse" ] Group "Parse every game: counts, empty games, parse speed" with
        Description =
          Some "A health check of the parser on a file: games, plies, games without moves or a result, speed and memory. It replays no moves (pgn validate does). Exit 1 when a game could not be read."
        Args = [ pgnSubject ]
        Options = [ json ]
        Examples = [ "eb-cli pgn parse big.pgn", "How many games, and how fast they parse" ] }

let parseFile (p: Parsed) =
    let path = pgnArg p
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable games = 0
    let mutable plies = 0L
    let mutable empty = 0
    let mutable noResult = 0
    let mutable withFen = 0
    let mutable longest = 0
    let mutable failed = 0
    let mutable stoppedBy: string = null
    try
        for game in FullPGNParser.parsePgnFile path do
            games <- games + 1
            try
                let n = Seq.length game.Mainline
                plies <- plies + int64 n
                if n = 0 then empty <- empty + 1
                if n > longest then longest <- n
                if String.IsNullOrWhiteSpace game.GameMetaData.Result then noResult <- noResult + 1
                if not (String.IsNullOrWhiteSpace game.GameMetaData.Fen) then withFen <- withFen + 1
            with _ -> failed <- failed + 1
            if games % 100_000 = 0 then info $"  {n0i games} games, {n0 plies} plies, {sw.Elapsed.TotalSeconds:F1} s..."
    with ex -> stoppedBy <- ex.Message
    let secs = max sw.Elapsed.TotalSeconds 0.001
    let ok = failed = 0 && isNull stoppedBy
    if has p "json" then
        let result =
            {| File = path; SizeMb = mb path; Games = games; Plies = plies; LongestGame = longest
               GamesWithoutMoves = empty; GamesWithoutResult = noResult; GamesWithFen = withFen
               GamesThatFailed = failed; Seconds = secs; PeakMemoryMb = peakMb () |}
        let errors =
            [ if failed > 0 then { Code = "games-failed"; Message = $"{failed} games could not be read" }
              if not (isNull stoppedBy) then { Code = "parse-failed"; Message = $"parsing stopped after {games} games: {stoppedBy}" } ]
        printJson (envelope (name p.Command) ok (box result) errors)
    else
        printfn "File           : %s (%.1f MB)" path (mb path)
        printfn "Games          : %s" (n0i games)
        printfn "Plies          : %s (avg %.1f per game)" (n0 plies) (if games > 0 then float plies / float games else 0.0)
        printfn "Longest game   : %d plies" longest
        printfn "Without moves  : %s" (n0i empty)
        printfn "Without result : %s" (n0i noResult)
        printfn "With FEN tag   : %s" (n0i withFen)
        printfn "Failed         : %s" (n0i failed)
        printfn "Time           : %.1f s (%.1f MB/s, %s plies/s)" secs (mb path / secs) (n0 (int64 (float plies / secs)))
        printfn "Peak memory    : %.0f MB" (peakMb ())
        if not (isNull stoppedBy) then error $"parsing stopped after {n0i games} games: {stoppedBy}"
    if ok then ExitOk else ExitFailed

// ---------------------------------------------------------------- validate

let validateSpec =
    { command [ "pgn"; "validate" ] Group "Replay every game: illegal or ambiguous moves, non-standard SAN" with
        Description =
          Some "Every move is matched against the legal moves of its position. A move that fits none (or more than one) is an error and ends that game's check; a legal move written otherwise than standard SAN is a warning. The first 20 findings are shown; --out writes them all to a CSV. Exit 1 when there are errors."
        Args = [ pgnSubject ]
        Options =
          [ out "Write every finding to this CSV file"
            force
            value "threads" "N" "Check games on N threads (up to the core count)" |> withDefault "1"
            json ]
        Examples =
          [ "eb-cli pgn validate games.pgn", "Errors and warnings, with a summary"
            "eb-cli pgn validate big.pgn -o findings.csv --threads 8", "Every finding to a CSV, on 8 threads" ] }

let validate (p: Parsed) =
    let path = pgnArg p
    let csvPath = valueOf p "out" |> Option.map (outputFile p)
    // the CSV is opened before the PGN is read: the same file would be emptied first
    if csvPath |> Option.exists (fun c -> String.Equals(c, path, StringComparison.OrdinalIgnoreCase)) then
        raise (Usage "--out would write over the PGN: give another file")
    let threads =
        match intOf p "threads" with
        | Some t when t < 1 -> raise (Usage $"--threads must be at least 1, got {t}")
        | Some t -> min t Environment.ProcessorCount
        | None -> 1
    let sw = Diagnostics.Stopwatch.StartNew()
    let field (s: string) = "\"" + (if isNull s then "" else s.Replace("\"", "\"\"")) + "\""
    let shown = ResizeArray<PgnValidation.Finding>()
    let counts = Collections.Generic.Dictionary<PgnValidation.Kind, int>()
    let mutable games = 0
    let mutable plies = 0L
    let mutable gamesWithErrors = 0
    let mutable gamesWithWarnings = 0
    let mutable noMoves = 0
    let mutable stoppedBy: string = null
    // one board per thread; the findings come out in file order whatever the thread count
    let boards = new Threading.ThreadLocal<Chess.Board>(fun () -> Chess.Board())
    let checkGame (game: PGNTypes.PgnGame) =
        let findings, played = PgnValidation.validateGame boards.Value game
        findings, played, game.Mainline.Count = 0
    let checkedGames =
        let source = FullPGNParser.parsePgnFile path
        if threads = 1 then Seq.map checkGame source
        else Linq.ParallelEnumerable.Select(Linq.ParallelEnumerable.AsOrdered(Linq.ParallelEnumerable.WithDegreeOfParallelism(Linq.ParallelEnumerable.AsParallel source, threads)), checkGame) :> seq<_>
    let csv =
        csvPath |> Option.map (fun out ->
            let w = new StreamWriter(out)
            w.WriteLine "game,round,white,black,ply,move,kind,expected,fen"
            w)
    try
        try
            for findings, played, empty in checkedGames do
                games <- games + 1
                plies <- plies + int64 played
                if empty then noMoves <- noMoves + 1
                if findings |> List.exists (fun f -> PgnValidation.isError f.Kind) then gamesWithErrors <- gamesWithErrors + 1
                if findings |> List.exists (fun f -> not (PgnValidation.isError f.Kind)) then gamesWithWarnings <- gamesWithWarnings + 1
                for f in findings do
                    counts.[f.Kind] <- (match counts.TryGetValue f.Kind with | true, n -> n + 1 | _ -> 1)
                    if shown.Count < 20 then shown.Add f
                    csv |> Option.iter (fun w ->
                        w.WriteLine(String.Join(",", [ string f.Game; field f.Round; field f.White; field f.Black; string f.Ply
                                                       field f.Move; field (PgnValidation.kindName f.Kind); field f.Expected; field f.Fen ])))
                if games % 100_000 = 0 then info $"  {n0i games} games, {n0 plies} plies, {sw.Elapsed.TotalSeconds:F1} s..."
        with ex ->
            let ex = match ex with :? AggregateException as a when not (isNull a.InnerException) -> a.InnerException | _ -> ex
            stoppedBy <- ex.Message
    finally
        csv |> Option.iter (fun w -> w.Dispose())
    let eventLines = countEventLines path
    let secs = max sw.Elapsed.TotalSeconds 0.001
    let count kind = match counts.TryGetValue kind with | true, n -> n | _ -> 0
    let errors = count PgnValidation.IllegalMove + count PgnValidation.AmbiguousMove + count PgnValidation.BadStart
    let ok = errors = 0 && isNull stoppedBy
    let findingJson (f: PgnValidation.Finding) =
        {| Game = f.Game; Round = f.Round; White = f.White; Black = f.Black; Ply = f.Ply; Move = f.Move
           Kind = PgnValidation.kindName f.Kind; Expected = f.Expected; Fen = f.Fen |}
    if has p "json" then
        let result =
            {| File = path; Games = games; PliesReplayed = plies; GamesWithoutMoves = noMoves
               RecordsSkipped = max 0 (eventLines - games); GamesWithErrors = gamesWithErrors; GamesWithWarnings = gamesWithWarnings
               Illegal = count PgnValidation.IllegalMove; Ambiguous = count PgnValidation.AmbiguousMove
               BadStartFen = count PgnValidation.BadStart; NonStandardSan = count PgnValidation.NonStandardSan
               FirstFindings = shown |> Seq.map findingJson |> Array.ofSeq; Csv = Option.toObj csvPath
               Threads = threads; Seconds = secs |}
        let errs =
            [ if errors > 0 then { Code = "invalid-games"; Message = $"{gamesWithErrors} games have illegal or ambiguous moves or a bad start FEN" }
              if not (isNull stoppedBy) then { Code = "parse-failed"; Message = $"parsing stopped after {games} games: {stoppedBy}" } ]
        printJson (envelope (name p.Command) ok (box result) errs)
    else
        for f in shown do
            printfn "  game %d (round %s, %s - %s) ply %d %s: %s%s" f.Game f.Round f.White f.Black f.Ply f.Move (PgnValidation.kindName f.Kind)
                (if String.IsNullOrEmpty f.Expected then ""
                 elif f.Kind = PgnValidation.BadStart then " - " + f.Expected
                 else " - expected " + f.Expected)
            printfn "    %s" f.Fen
        let total = counts.Values |> Seq.sum
        if total > shown.Count then
            printfn "  ... and %s more%s" (n0i (total - shown.Count)) (if csvPath.IsSome then " (all in the CSV)" else " (-o <file.csv> lists them all)")
        printfn ""
        printfn "File            : %s (%.1f MB)" path (mb path)
        printfn "Games           : %s" (n0i games)
        printfn "Plies replayed  : %s" (n0 plies)
        printfn "Without moves   : %s" (n0i noMoves)
        printfn "Records skipped : %s ([Event lines the parser made no game of)" (n0i (max 0 (eventLines - games)))
        printfn "With errors     : %s (illegal %s, ambiguous %s, bad start FEN %s)" (n0i gamesWithErrors)
            (n0i (count PgnValidation.IllegalMove)) (n0i (count PgnValidation.AmbiguousMove)) (n0i (count PgnValidation.BadStart))
        printfn "With warnings   : %s (non-standard SAN %s)" (n0i gamesWithWarnings) (n0i (count PgnValidation.NonStandardSan))
        csvPath |> Option.iter (printfn "CSV             : %s")
        printfn "Threads         : %d (of %d cores)" threads Environment.ProcessorCount
        printfn "Time            : %.1f s (%.1f MB/s)" secs (mb path / secs)
        if not (isNull stoppedBy) then error $"parsing stopped after {n0i games} games: {stoppedBy}"
    if ok then ExitOk else ExitFailed

// ---------------------------------------------------------------- strip

let stripSpec =
    { command [ "pgn"; "strip" ] Group "Write the games without comments, variations or NAGs" with
        Description =
          Some "Every game written again: its tags as they were, the main line numbered and wrapped at 80 characters, the result. The output goes to a temporary file first, so a failed run leaves no half file; the input is never written over."
        Args = [ pgnSubject ]
        Options =
          [ out "The output PGN (default <name>_stripped.pgn next to the input)"
            force
            switch "skip-empty" "Leave out games without moves"
            switch "skip-invalid" "Leave out games with an illegal or ambiguous move or a bad start FEN"
            json ]
        Examples =
          [ "eb-cli pgn strip games.pgn", "games_stripped.pgn next to it"
            "eb-cli pgn strip games.pgn -o clean.pgn --skip-empty --skip-invalid --force", "Only playable games, over clean.pgn" ] }

let strip (p: Parsed) =
    let path = pgnArg p
    let outPath =
        match valueOf p "out" with
        | Some o -> outputFile p o
        | None ->
            outputFile p (Path.Combine(Path.GetDirectoryName path, Path.GetFileNameWithoutExtension path + "_stripped.pgn"))
    if String.Equals(path, outPath, StringComparison.OrdinalIgnoreCase) || String.Equals(path, outPath + ".tmp", StringComparison.OrdinalIgnoreCase) then
        raise (Usage "the output would write over the input: give another --out")
    let skipEmpty, skipInvalid = has p "skip-empty", has p "skip-invalid"
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable games = 0
    let mutable plies = 0L
    let mutable comments = 0L
    let mutable variations = 0L
    let mutable nags = 0L
    let mutable emptyLeft = 0
    let mutable invalidLeft = 0
    let board = Chess.Board()
    let leftOut (game: PGNTypes.PgnGame) =
        if skipEmpty && game.Mainline.Count = 0 then
            emptyLeft <- emptyLeft + 1
            true
        elif skipInvalid && (fst (PgnValidation.validateGame board game) |> List.exists (fun f -> PgnValidation.isError f.Kind)) then
            invalidLeft <- invalidLeft + 1
            true
        else false
    let temp = outPath + ".tmp"
    try
        do
            use writer = new StreamWriter(temp, false, Text.UTF8Encoding(false), 1 <<< 16)
            for game in FullPGNParser.parsePgnFileWithRaw path |> Seq.filter (leftOut >> not) do
                if games > 0 then writer.Write('\n')
                writer.Write(PGNWriter.strippedGameText game)
                games <- games + 1
                for m in game.Mainline do
                    plies <- plies + 1L
                    if not (String.IsNullOrWhiteSpace m.Comment) then comments <- comments + 1L
                    if not (isNull m.Variations) then variations <- variations + int64 m.Variations.Count
                    nags <- nags + int64 m.Nags.Length + (if m.San.EndsWith "!" || m.San.EndsWith "?" then 1L else 0L)
                if games % 100_000 = 0 then info $"  {n0i games} games, {sw.Elapsed.TotalSeconds:F1} s..."
        File.Move(temp, outPath, true)
    with ex ->
        (try File.Delete temp with _ -> ())
        failwith $"stopped after {n0i games} games: {ex.Message}"
    let skipped = max 0 (countEventLines path - games - emptyLeft - invalidLeft)
    let secs = max sw.Elapsed.TotalSeconds 0.001
    if has p "json" then
        let result =
            {| File = path; Written = outPath; Games = games; Plies = plies; CommentsRemoved = comments
               VariationsRemoved = variations; NagsRemoved = nags; LeftOutEmpty = emptyLeft
               LeftOutInvalid = invalidLeft; RecordsSkipped = skipped; Seconds = secs |}
        printJson (envelope (name p.Command) true (box result) [])
    else
        printfn "File       : %s (%.1f MB)" path (mb path)
        printfn "Written    : %s (%.1f MB)" outPath (mb outPath)
        printfn "Games      : %s, plies %s" (n0i games) (n0 plies)
        printfn "Removed    : %s comments, %s variations, %s NAGs (main line)" (n0 comments) (n0 variations) (n0 nags)
        if skipEmpty then printfn "Left out   : %s games without moves" (n0i emptyLeft)
        if skipInvalid then printfn "Left out   : %s games with an illegal or ambiguous move or a bad FEN" (n0i invalidLeft)
        printfn "Skipped    : %s records the parser made no game of" (n0i skipped)
        printfn "Time       : %.1f s (%.1f MB/s)" secs (mb path / secs)
    ExitOk

// ---------------------------------------------------------------- summary, elo, speed, deviations

let summarySpec =
    { command [ "pgn"; "endings" ] Group "How the games ended: missed wins, repetition, 50-move rule, stalemate..." with
        Description = Some "From the games with engine evals: the missed wins (a game an engine stood +4 in and did not win) per engine and how they ended, and how many games ended by repetition, the 50-move rule, stalemate or insufficient material."
        Args = [ pgnSubject ]
        Options = [ json ]
        Examples = [ "eb-cli pgn endings games.pgn", "How the games ended" ] }

let summary (p: Parsed) =
    let path = pgnArg p
    match Test.ParsingTests.pgnTerminationSummary path 2 4.0 (3.0, 0.5) with
    | [ _, _, _, counts ] when has p "json" -> printJson (envelope (name p.Command) true (box counts) [])
    | _ -> ()
    ExitOk

let eloSpec =
    { command [ "pgn"; "elo" ] Group "Ratings and results" with
        Description = Some "Elo, score and results per engine pair from the games."
        Args = [ pgnSubject ]
        Options = [ json ]
        Examples = [ "eb-cli pgn elo match.pgn", "The rating table" ] }

let elo (p: Parsed) =
    let games = FullPGNParser.parsePgnFile (pgnArg p)
    let text, players, cross, _ = GameAnalysis.PGNCalculator.getEngineDataResults games
    if has p "json" then
        // NaN or infinity (a 100% score has no finite Elo) is null in JSON
        let num (x: float) = if Double.IsFinite x then Nullable x else Nullable()
        let result =
            {| Players =
                 [ for r in players |> List.sortByDescending (fun r -> r.Points) ->
                     let ww, wd, wl = r.WhiteWDL
                     let bw, bd, bl = r.BlackWDL
                     {| Player = r.Player; Played = r.Played; Points = r.Points; Percent = r.Percent; Elo = num r.Elo
                        Error = num r.Error; Cfs = r.CFS; Win = r.Win; Draw = r.Draw; Loss = r.Loss
                        White = {| Win = ww; Draw = wd; Loss = wl |}; Black = {| Win = bw; Draw = bd; Loss = bl |} |} ]
               Pairs =
                 [ for e in cross do
                     for opponent, s in e.StatsAgainst do
                         {| Player = e.Player; Opponent = opponent; Win = s.Wins; Draw = s.Draws; Loss = s.Losses |} ] |}
        printJson (envelope (name p.Command) true (box result) [])
    else printfn "%s" text
    ExitOk

let speedSpec =
    { command [ "pgn"; "speed" ] Group "Speed statistics per engine" with
        Description = Some "Nodes per second (median and average) and other speed figures per engine, from the PGN's comments."
        Args = [ pgnSubject ]
        Options = [ json ]
        Examples = [ "eb-cli pgn speed match.pgn", "NPS per engine" ] }

let speed (p: Parsed) =
    let games = FullPGNParser.parsePgnFile (pgnArg p)
    let stats =
        PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile (games, 0)
        |> Array.filter _.Median
        |> Array.sortByDescending _.AvgNPS
    if has p "json" then printJson (envelope (name p.Command) true (box stats) [])
    else printfn "%s" (Configuration.ConsoleHelper.writeSummaryEngineStatsToConsole stats)
    ExitOk

let deviationsSpec =
    { command [ "pgn"; "deviations" ] Group "Positions reached in several games, and the moves each engine chose there" with
        Description = Some "Positions reached in more than one game where the moves differ: which engine played what there, and engines that chose differently in the same position themselves (the deviation page's position view), with how the openings were told apart from the engines' own moves."
        Args = [ pgnSubject ]
        Options = [ json ]
        Examples = [ "eb-cli pgn deviations gauntlet.pgn", "Self and cross deviations" ] }

let deviations (p: Parsed) =
    let path = pgnArg p
    let sw = Diagnostics.Stopwatch.StartNew()
    let games = FullPGNParser.parsePgnFile path |> Seq.toList
    info $"Deviation analysis: {path} ({games.Length} games)"
    let devs, summary, coverage = DeviationAnalysis.analyzePositionDeviations games
    if has p "json" then
        printJson (envelope (name p.Command) true (box {| Games = games.Length; Engines = summary; Positions = devs; Openings = coverage |}) [])
    else printf "%s" (DeviationAnalysis.printPositionDeviationsToConsole devs summary coverage)
    info $"Done in {sw.Elapsed.TotalSeconds:F1} s"
    ExitOk

let entries: Entry list =
    [ { Spec = parseSpec; Run = parseFile }
      { Spec = validateSpec; Run = validate }
      { Spec = stripSpec; Run = strip }
      { Spec = summarySpec; Run = summary }
      { Spec = eloSpec; Run = elo }
      { Spec = speedSpec; Run = speed }
      { Spec = deviationsSpec; Run = deviations } ]
