/// `match`: a match from a match command line, played by EngineBattle's
/// tournament runner and reported the way the reference reports it (MatchMode.md). The parts
/// live in ChessLibrary/Match; this puts them together and owns the process's stdout:
/// everything else that would print there - the library's own lines, engine output, warm-up
/// messages - goes to the -log file, or nowhere, so a tool reading stdout sees only the reference's
/// lines, with the platform's line ends.
module MatchMode

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Text
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary
open ChessLibrary.TournamentTypes
open ChessLibrary.Match

/// `-version`: "EngineBattle 1.8.2 (abc1234)", or "EngineBattle dev build (abc1234)".
let private versionLine () = "EngineBattle " + BuildInfo.describe ()

/// What to run again to resume: this program, as it was started.
let private programName (viaVerb: bool) =
    let processPath = Environment.ProcessPath
    let exe =
        if not (isNull processPath) && Path.GetFileNameWithoutExtension(processPath).Equals("dotnet", StringComparison.OrdinalIgnoreCase) then
            $"dotnet {Environment.GetCommandLineArgs().[0]}"
        elif isNull processPath then "EngineBattle.Console"
        else processPath
    if viaVerb then exe + " match" else exe

/// A UCI option's value as the engine plays with it: the value the command line sets, else the
/// engine's default; None when the engine has no such option (the report's NULL - Lc0 and Ceres
/// have no Hash). The defaults are what the runner's own engines reported at `uciok`
/// (EngineStarted), so no engine is started an extra time for them; an engine not started yet
/// has only the command line's values.
let private optionValues (engines: MatchArgs.EngineConfig list) (started: Collections.Concurrent.ConcurrentDictionary<string, Map<string, string>>) =
    fun (name: string) (option: string) ->
        let set =
            engines |> List.tryFind (fun e -> e.Name = name)
            |> Option.bind (fun e -> e.Options |> List.rev |> List.tryFind (fun (k, _) -> k = option) |> Option.map snd)
        match started.TryGetValue name with
        | true, d ->
            match Map.tryFind option d with
            | Some v -> Some(defaultArg set v)
            | None -> None
        | _ -> set

/// The runner's logger in a match: its lines go to the -log file with the rest (the reference's
/// place for them), not to EngineBattle's usual log folder, which is relative to wherever the
/// tool that runs the match happens to be.
type private LogWriterLogger(log: TextWriter, minimum: LogLevel) =
    interface ILogger with
        member _.BeginScope _ = { new IDisposable with member _.Dispose() = () }
        member _.IsEnabled level = level >= minimum && not (obj.ReferenceEquals(log, TextWriter.Null))
        member this.Log(level, _, state, ex, formatter) =
            if (this :> ILogger).IsEnabled level then
                log.WriteLine($"[{level}] {formatter.Invoke(state, ex)}")
                if not (isNull ex) then log.WriteLine(ex.ToString())

/// The match itself; everything for stdout and stderr goes through `output`.
let private runWith (args: string list) (viaVerb: bool) (output: MatchOutput.QueuedWriter) : int =
    let clock = Stopwatch.StartNew()
    let realOut = Console.Out
    let realErr = Console.Error
    // queued, never blocking: printed under the reporter's lock, which every game shares
    let emit (text: string) = output.Write(text.Replace("\n", Environment.NewLine))
    let emitMessages (messages: MatchArgs.Message list) =
        for m in messages do
            match m with
            | MatchArgs.Stdout line -> emit (line + "\n")
            | MatchArgs.Stderr line -> output.WriteError(line + Environment.NewLine)
    let env = { MatchArgs.defaultEnv () with LoadConfig = MatchConfigJson.load; Version = versionLine () }
    match MatchArgs.parse env args with
    | MatchArgs.Exit(messages, text) ->
        emitMessages messages
        emit text
        0
    | MatchArgs.Failed(messages, error) ->
        emitMessages messages
        emit (error + "\n")
        1
    | MatchArgs.Run parsed ->
    emitMessages parsed.Messages
    match MatchMapping.map parsed with
    | Error e ->
        emit (e + "\n")
        1
    | Ok mapped ->
    let t = parsed.Tournament
    let tourny = mapped.Tournament
    // Everything but the reference's lines goes to the log (or nowhere) from here on.
    let log : TextWriter =
        if t.Log.File = "" then TextWriter.Null
        else
            try TextWriter.Synchronized(new StreamWriter(t.Log.File, t.Log.AppendFile, UTF8Encoding(false), AutoFlush = true))
            with _ ->
                // the reference's text for it, on stderr; the match runs without a log
                output.WriteError("Failed to open log file." + Environment.NewLine)
                TextWriter.Null
    Console.SetOut log
    Console.SetError log
    let minimum =
        match t.Log.Level with
        | MatchArgs.All | MatchArgs.Trace -> LogLevel.Trace
        | MatchArgs.Info -> LogLevel.Information
        | MatchArgs.Warn -> LogLevel.Warning
        | MatchArgs.Err -> LogLevel.Error
        | MatchArgs.Fatal -> LogLevel.Critical
    let logger = LogWriterLogger(log, minimum) :> ILogger
    try
        try
            for note in mapped.Notes do log.WriteLine("Note: " + note)
            // -each ponder: every engine is started once first, as a tournament with AllowPondering
            // does, and all must be able to ponder (their start-up goes to the log, not stdout)
            let ponderErrors =
                if tourny.AllowPondering then (let _, _, errors = Tournament.TournamentUtils.checkPonderingEngines tourny in errors)
                else []
            match ponderErrors with
            | _ :: _ ->
                for e in ponderErrors do emit ("Error: " + e + "\n")
                1
            | [] ->
            match Path.GetDirectoryName(Path.GetFullPath tourny.PgnOutPath) with
            | null | "" -> ()
            | dir -> Directory.CreateDirectory dir |> ignore
            let mutable interrupted = false
            let mutable runnerRef : Tournament.Manager.Runner option = None
            // From here on, before the runner starts, so a CTRL-C at any point ends as
            // interrupted, with config.json. A second CTRL-C quits at once, for a
            // shutdown that hangs (the runner's own handler cancels every press).
            use _ctrlC =
                Console.CancelKeyPress.Subscribe(fun e ->
                    if interrupted then
                        log.WriteLine "Second CTRL-C: quitting."
                        output.Complete 2000
                        exit 1
                    interrupted <- true
                    e.Cancel <- true
                    runnerRef |> Option.iter (fun r -> r.Cancel()))
            let started = Collections.Concurrent.ConcurrentDictionary<string, Map<string, string>>()
            let optionOf = optionValues parsed.Engines started
            let configToSave =
                if t.Pgn.File = "" then { t with Pgn = { t.Pgn with File = tourny.PgnOutPath } } else t
            let sprt = MatchSprt.create t.Sprt.Alpha t.Sprt.Beta t.Sprt.Elo0 t.Sprt.Elo1 t.Sprt.Model t.Sprt.Enabled
            let gate = obj ()
            let mutable reporter : MatchOutput.Reporter option = None
            let mutable total = 0
            let mutable prior = 0
            let mutable finished = 0
            let mutable sprtStopped = false
            let newReporter totalGames priorGames =
                MatchOutput.Reporter(
                    { Output = t.Output; ReportPenta = t.ReportPenta; RatingInterval = t.RatingInterval
                      ScoreInterval = t.ScoreInterval; Sprt = sprt; Engines = parsed.Engines; EngineOption = optionOf
                      Book = t.Opening.File; TotalGames = totalGames; PriorGames = priorGames },
                    emit)
            // A config.json that cannot be written (a missing folder, a locked file) is logged and
            // otherwise ignored, as the reference's plain ofstream does: it must not cost the
            // report of a match that was played.
            let save () =
                try
                    match reporter with
                    | Some r -> MatchConfigJson.save t.ConfigName configToSave parsed.Engines (r.WithScoreboard(MatchConfigJson.statsOf parsed.Engines))
                    | None -> MatchConfigJson.save t.ConfigName configToSave parsed.Engines []
                with ex -> log.WriteLine($"{t.ConfigName} not written: {ex.Message}")
            let callback (u: Update) =
                match u with
                | StartOfTournament info ->
                    lock gate (fun () ->
                        let played = GameHelpers.loadGamesAlreadyPlayed tourny.PgnOutPath
                        let r = newReporter info.NumberOfGames played.Length
                        for g in played do
                            let m = g.GameMetaData
                            r.Preload(m.White, m.Black, m.Result, m.OpeningHash)
                        total <- info.NumberOfGames
                        prior <- played.Length
                        reporter <- Some r)
                | StartOfGame g ->
                    reporter |> Option.iter (fun r -> r.Started(g.CurrentGameNr, g.WhitePlayer.Name, g.BlackPlayer.Name))
                | GameFinished g ->
                    match reporter with
                    | None -> ()
                    | Some r ->
                        match r.Finished g with
                        | Some _ ->
                            sprtStopped <- true
                            runnerRef |> Option.iter (fun run -> run.Cancel())
                        | None -> ()
                        lock gate (fun () ->
                            finished <- finished + 1
                            if t.AutoSaveInterval > 0 && (prior + finished) % t.AutoSaveInterval = 0 then save ())
                | EngineStarted(name, defaults) -> started.TryAdd(name, defaults) |> ignore
                | EngineStartFailed(name, reason) ->
                    // The reference's text for it (match.cpp:342-353); the run stops as interrupted
                    emit $"Fatal; {name} engine startup failure: \"{reason}\"\n"
                | _ -> ()
            // What the reference never checks, on stdout since it changes the match: fewer games at
            // once when the engines do not fit in memory (Lc0 and Ceres grow with their network),
            // and more search threads than the machine has - games x the thinking engine's Threads
            // (without ponder one engine per game thinks, with it both). Threads the command line
            // leaves to the engine count as 1.
            if not interrupted && tourny.TestOptions.NumberOfGamesInParallel > 1 then
                let games = ParallelExecution.concurrencyFor tourny
                if games < tourny.TestOptions.NumberOfGamesInParallel then
                    emit $"Info: Adjusted concurrency to {games}, as many as fit in 70%% of the memory.\n"
                let threads =
                    parsed.Engines
                    |> List.map (fun e ->
                        match e.Options |> List.rev |> List.tryFind (fun (k, _) -> k = "Threads") with
                        | Some(_, v) -> (match Int32.TryParse v with | true, n -> max 1 n | _ -> 1)
                        | None -> 1)
                    |> List.fold max 1
                    |> (fun n -> if tourny.AllowPondering then 2 * n else n)
                if games * threads > Environment.ProcessorCount then
                    emit $"Warning: {games} games x {threads} threads = {games * threads} search threads on {Environment.ProcessorCount} hardware threads.\n"
            if not interrupted then
                let runner = Tournament.Manager.Runner(logger, Action<Update>(callback), false, true)
                runner.SuppressDashboard <- true
                runner.AddTournament tourny
                runnerRef <- Some runner
                runner.Run() |> ignore
                (try runner.DisposePgnReader() with _ -> ())
            let r = match reporter with Some r -> r | None -> newReporter 0 0
            let ending =
                if sprtStopped then MatchOutput.SprtStopped
                // every game this run had left was reported (the scoreboard is no measure: it
                // leaves out a PGN's games of engines not in this match, which `total` counts)
                elif interrupted || reporter.IsNone || finished < total - prior then
                    MatchOutput.Interrupted(programName viaVerb, t.ConfigName)
                else MatchOutput.Completed
            save ()
            r.End(ending, clock.Elapsed)
        with ex ->
            log.WriteLine(ex.ToString())
            emit (ex.Message + "\n")
            1
    finally
        Console.SetOut realOut
        Console.SetError realErr
        log.Dispose()

/// Runs a match from its command-line arguments (without the verb) and returns the exit code.
let run (args: string list) (viaVerb: bool) : int =
    let output = MatchOutput.QueuedWriter(Console.Out, Console.Error)
    // every way out prints what is queued first: the final statistics are the last of it
    try runWith args viaVerb output
    finally
        // a console that never takes the rest (a pipe nobody reads) can still be left with CTRL-C
        use _quit = Console.CancelKeyPress.Subscribe(fun _ -> exit 1)
        output.Complete Threading.Timeout.Infinite
