module ChessLibrary.ParallelExecution

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary
open ChessLibrary.Engine
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.Chess
open ChessLibrary.ChessUtilities
open ChessLibrary.TournamentPairing
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers
open ChessLibrary.GameReplay
open ChessLibrary.GamePersistence
open ChessLibrary.TournamentRunners.TournamentUtils

/// How many games the runner plays at once: the games asked for, fewer when one copy of each
/// engine times that number does not fit in 70% of the memory (measured once per engine setup and
/// cached, so asking again - as match does to report it - starts no engine), at least one per GPU.
let concurrencyFor (tourny: Tournament) =
    let gpus = tourny.TestOptions.GPUs
    let memBased =
        HardwareInfo.concurrencyLevel
            tourny.EngineSetup.Engines
            tourny.TestOptions.NumberOfGamesInParallel
    let gpuBased =
        if gpus <> null && gpus.Length > 1 then
            max memBased gpus.Length
        else
            memBased
    max 1 gpuBased

/// What a run plays, worked out once before any engine starts: the book sized for the mode,
/// the plan, and the plan diffed against the games already in the output PGN (a resume plays
/// only what is missing). Sets TotalGames and CurrentGameNr on the tournament as before.
type private Schedule =
    { /// The whole plan with its round labels, for the total the page shows.
      AllPairings: Pairing list
      GamesLeftToPlay: Pairing list
      GamesAlreadyPlayed: PgnGame[]
      /// PreventMoveDeviation's reference games, if a ReferencePGNPath is set.
      ReferenceGames: PgnGame[]
      /// An EPD book carries no moves to play out.
      EpdBook: bool
      StartInfo: StartOfTournamentInfo }

let private buildSchedule (logger: ILogger) (tourny: Tournament) : Schedule =
    let challengers = tourny.EngineSetup.Engines |> List.filter(fun e -> e.IsChallenger)
    let rest = tourny.EngineSetup.Engines |>  List.filter(fun e -> not e.IsChallenger)
    let isGauntlet = tourny.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)

    // The new Scheduler expects the book to already be sized for the chosen
    // distribution. For Gauntlet + Spread (= RandomOpenings=true) with more
    // than one opponent, that means `rounds × numOpp` distinct openings; for
    // Gauntlet + Shared and all non-Gauntlet modes, just `rounds` openings.
    let numOpps = rest.Length
    let gauntletDistribution =
        if tourny.Opening.RandomOpenings then Scheduler.Spread else Scheduler.Shared
    let effectiveBookSize =
        if isGauntlet && gauntletDistribution = Scheduler.Spread && numOpps > 1 then
            tourny.Rounds * numOpps
        else
            tourny.Rounds

    let mutable epdBook = false
    // random openings come from the whole book, not its first openings
    let take (openings: seq<PGNTypes.PgnGame>) =
        if tourny.Opening.RandomOpenings then PairingHelper.sampleWithSeed tourny.Opening.Seed effectiveBookSize openings
        else openings |> Seq.truncate effectiveBookSize |> Seq.toArray
    let games =
        match tourny.Opening.OpeningsPath with
        |Some path ->
        if File.Exists path |> not then
            if tourny.VerboseLogging then
                logger.LogError($"Opening file {path} does not exist")
            [| for i = 1 to effectiveBookSize do yield PGNTypes.PgnGame.Empty i |]
        elif path.ToLower().Contains ".epd" then
            epdBook <- true
            take (EPDExtractor.parseEPDFile path)
        else
            let all = take (ChessLibrary.FullPGNParser.parsePgnFile path)
            if tourny.VerboseLogging then
                logger.LogInformation $"Total number of openings in PGN = {all.Length}"
            all
        |_ ->
            [| for i = 1 to effectiveBookSize do yield PGNTypes.PgnGame.Empty i |]

    if isGauntlet && gauntletDistribution = Scheduler.Spread && numOpps > 1 && games.Length < effectiveBookSize then
        ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow
            (sprintf "Warning: Gauntlet with %d opponents and %d rounds needs %d openings to avoid wrap, but the book only has %d. Some openings will be reused and engines may play them more than expected."
                numOpps tourny.Rounds effectiveBookSize games.Length)

    let gamesAlreadyPlayed =
        let fileExists = File.Exists tourny.PgnOutPath
        if fileExists then
            // Shared with the sequential runners on purpose: this used to be a second copy that
            // overwrote the stored opening hash, which undoes the two-key matching in Diff.diff.
            GameHelpers.loadGamesAlreadyPlayed tourny.PgnOutPath
        else
            [||]

    let referencGamesPlayed =
        let fileExists = File.Exists tourny.ReferencePGNPath
        if fileExists then
            ChessLibrary.FullPGNParser.parsePgnFile tourny.ReferencePGNPath |> Seq.toArray
        else
            [||]
    let gamesToPlay =
        let openings = games |> Seq.truncate effectiveBookSize |> Seq.toList
        if tourny.Opening.RandomOpenings then PairingHelper.shuffleOpeningsForTournament tourny.Opening openings
        else openings

    // Build one ScheduleConfig and reuse it for both total-plan and diff.
    let scheduleCfg : Scheduler.ScheduleConfig =
        if isGauntlet then
            { Mode = Scheduler.Gauntlet
              Challengers = challengers
              Opponents = rest
              Openings = gamesToPlay
              Rounds = tourny.Rounds
              OpeningsTwice = tourny.Opening.OpeningsTwice
              PreventDeviation = tourny.PreventMoveDeviation
              Distribution = gauntletDistribution }
        else
            { Mode = Scheduler.RoundRobin
              Challengers = tourny.EngineSetup.Engines
              Opponents = []
              Openings = gamesToPlay
              Rounds = tourny.Rounds
              OpeningsTwice = tourny.Opening.OpeningsTwice
              PreventDeviation = tourny.PreventMoveDeviation
              Distribution = Scheduler.Shared }
    let plan =
        if isGauntlet
        then ChessLibrary.Scheduler.Gauntlet.generate scheduleCfg
        else ChessLibrary.Scheduler.RoundRobin.generate scheduleCfg
    let gamesPerPair = if tourny.Opening.OpeningsTwice then 2 else 1
    let priorGames = gamesAlreadyPlayed.Length
    let allPairings =
        plan
        |> ChessLibrary.Scheduler.Diff.applyPairLabels 0 gamesPerPair
        |> ChessLibrary.Scheduler.Diff.toPairings
    let gamesLeftToPlay =
        let afterDiff = ChessLibrary.Scheduler.Diff.diff plan gamesAlreadyPlayed
        let afterLimits =
            if isGauntlet
            then ChessLibrary.Scheduler.Diff.enforceGameLimits challengers rest tourny.Rounds tourny.Opening.OpeningsTwice gamesAlreadyPlayed afterDiff
            else afterDiff
        afterLimits
        |> ChessLibrary.Scheduler.Diff.applyPairLabels priorGames gamesPerPair
        |> ChessLibrary.Scheduler.Diff.toPairings
        // a resumed run numbers its games after the ones already played, as cup, swiss and ladder do
        |> List.map (fun p -> { p with GameNr = priorGames + p.GameNr })

    if tourny.VerboseLogging then
        PairingHelper.logOpeningPairs logger gamesLeftToPlay

    let totalGames = allPairings.Length
    tourny.TotalGames <- totalGames
    let numberOfGamesPlayed = gamesAlreadyPlayed.Length
    tourny.CurrentGameNr <- numberOfGamesPlayed

    let (tTime, gTime) = estimateTournamentAndGameTime (gamesLeftToPlay.Length) tourny gamesLeftToPlay
    let startInfo = {NumberOfGames=numberOfGamesPlayed + gamesLeftToPlay.Length; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny}
    { AllPairings = allPairings
      GamesLeftToPlay = gamesLeftToPlay
      GamesAlreadyPlayed = gamesAlreadyPlayed
      ReferenceGames = referencGamesPlayed
      EpdBook = epdBook
      StartInfo = startInfo }

/// Every outlet for gameId-stamped updates: the optional file recorder and HTTP sink from
/// tournament.json's LiveFeed section (an EB_LIVEFEED_* env var overrides the matching field;
/// nothing configured means no feed, the normal case), and the in-process sink the WebGUI
/// multi-board grid listens on. `Any` says whether per-game events need stamping at all.
type private LiveFeed =
    { Any: bool
      Emit: string -> Update -> unit
      Dispose: unit -> unit }

let private openLiveFeed (logger: ILogger) (tourny: Tournament) (taggedSink: (string -> Update -> unit) option) : LiveFeed =
    let liveFeedCfg = if obj.ReferenceEquals(box tourny.LiveFeed, null) then LiveFeedConfig.Empty else tourny.LiveFeed
    let pickFeed (envName: string) (cfgVal: string) =
        match Environment.GetEnvironmentVariable envName with
        | null | "" -> (if isNull cfgVal then "" else cfgVal)
        | v -> v
    let feedFile   = pickFeed "EB_LIVEFEED_FILE"   liveFeedCfg.File
    let feedUrl    = pickFeed "EB_LIVEFEED_URL"    liveFeedCfg.Url
    let feedSource = pickFeed "EB_LIVEFEED_SOURCE" liveFeedCfg.Source
    let feedToken  = pickFeed "EB_LIVEFEED_TOKEN"  liveFeedCfg.Token
    let liveFeedRecorder : LiveFeedRecorder option =
        if String.IsNullOrEmpty feedFile then None
        else
            try
                logger.LogInformation("Live feed recording to {path}", feedFile)
                Some (new LiveFeedRecorder(feedFile))
            with ex ->
                logger.LogError("Failed to open live feed file {path}: {msg}", feedFile, ex.Message)
                None
    let liveFeedHttpSink : LiveFeedHttpSink option =
        if String.IsNullOrEmpty feedUrl then None
        else
            try
                logger.LogInformation("Live feed posting to {url}", feedUrl)
                Some (new LiveFeedHttpSink(feedUrl, feedSource, feedToken))
            with ex ->
                logger.LogError("Failed to init live feed URL sink {url}: {msg}", feedUrl, ex.Message)
                None
    { Any = liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome || taggedSink.IsSome
      Emit =
        fun gid u ->
            // Serialise once, fan out to file and/or HTTP; the in-process sink gets the Update itself.
            if LiveFeedWire.onWire u && (liveFeedRecorder.IsSome || liveFeedHttpSink.IsSome) then
                let line = LiveFeedWire.withGameId gid (LiveFeedWire.serializeUpdate u)
                liveFeedRecorder |> Option.iter (fun r -> r.RecordLine line)
                liveFeedHttpSink |> Option.iter (fun s -> s.Send line)
            taggedSink |> Option.iter (fun s -> s gid u)
      Dispose =
        fun () ->
            liveFeedRecorder |> Option.iter (fun r -> r.Dispose())
            liveFeedHttpSink |> Option.iter (fun s -> s.Dispose()) }

let parallelTournamentRun
  (logger: ILogger)
  (tourny: Tournament)
  (callback: Update -> unit)
  (taggedSink: (string -> Update -> unit) option)
  (tryGetUserAdjudication: unit -> UserAdjudication option)
  (cts: CancellationTokenSource)
  (externalPgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) =
  // Ladder, Cup, and Swiss manage their own pairings — dispatch directly
  let mode = if String.IsNullOrWhiteSpace tourny.TournamentMode then "" else tourny.TournamentMode.Trim().ToLowerInvariant()
  ChessLibrary.Engine.resetPrintedEngines()
  // Same reason, and this sits above the mode dispatch so it covers cup, swiss and ladder too:
  // the WebGUI runs many tournaments in one process, and a prober failure reported during the
  // first must not leave every run after it silent. It also opens the tables in the background.
  ChessLibrary.TablebaseProbe.startRun tourny.Adjudication.TBAdj.UseTBAdjudication tourny.Adjudication.TBAdj.TBMen tourny.Adjudication.TBAdj.TablebaseDirectory
  match mode with
  | "ladder" ->
      TournamentRunners.ladder logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | "cup" ->
      let seeding =
        match tourny.CupOptions.SeedingStrategy with
        | null -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
        | s when s.Equals("random", StringComparison.OrdinalIgnoreCase) -> TournamentPairing.PairingHelper.CupSeedingStrategy.Random
        | _ -> TournamentPairing.PairingHelper.CupSeedingStrategy.ByRating
      TournamentRunners.cup seeding tourny.CupOptions.UniquePerMatchOnly false logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | "swiss" ->
      TournamentRunners.swiss logger tourny callback cts tryGetUserAdjudication externalPgnAgent
  | _ ->
  async {

      logger.LogInformation("Tournament in parallel run about to start")

      let { AllPairings = allPairings; GamesLeftToPlay = gamesLeftToPlay
            GamesAlreadyPlayed = gamesAlreadyPlayed; ReferenceGames = referenceGames
            EpdBook = epdBook; StartInfo = startInfo } = buildSchedule logger tourny
      let feed = openLiveFeed logger tourny taggedSink
      // The pairing table and the total the page shows come from these two; the sequential
      // runner sent them and this path never did, so the page sat empty until the first game.
      callback (Update.TotalNumberOfPairs allPairings.Length)
      callback (Update.PairingList (ResizeArray<Pairing>(gamesLeftToPlay)))
      callback (Update.StartOfTournament startInfo)
      feed.Emit "" (Update.StartOfTournament startInfo)

      // Verbose-only diagnostics go to stdout: the console host's logger is silent below Critical.
      let verbose (msg: string) =
          if tourny.VerboseLogging then
              ChessLibrary.RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.DarkGray msg

      let concurrency = concurrencyFor tourny

      // Standings refresh. Console: every 10 games as before - each refresh re-reads the PGN
      // and runs Ordo there, and the tuner's SPRT check hangs off it. GUI: every 2 games with
      // one board, what the sequential runner it replaces did, and every `concurrency` above.
      let periodicEvery = if tourny.ConsoleOnly then 10 else max 2 concurrency

      if gamesLeftToPlay.Length = 0 then
          feed.Dispose()
          return []
      else
          // 1) the pairing queue: plan order, except that with deviation prevention on a pairing
          //    whose replay key is held by a game still in flight waits for it - a repeat of an
          //    earlier game's opening and colours must see that game finished and merged, or it
          //    plays against a partial line and the two then race to define it. See ReplayGate.
          let gate = ReplayGate.ReplayGate(gamesLeftToPlay, tourny.PreventMoveDeviation, tourny.PreventMoveDeviationFor)

          try // the feed is closed on every exit path

          // 2) PGN agent: use external if provided, else create local
          let pgnAgent, ownsAgent =
              match externalPgnAgent with
              | Some a -> a, false
              | None -> ChessLibrary.FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
          // Close the owned agent (open FileStream on the output PGN) on every exit path, and
          // wait for it: the next run may open the same file at once.
          use _pgnGuard =
              { new IDisposable with
                  member _.Dispose() =
                      if ownsAgent then ChessLibrary.FullPGNParser.closePgnAgent pgnAgent }

          // the run's record - results, replay, the deviation total, when the standings are due - is
          // one agent: games only send it what they did, so they share no state
          let record =
              RecordAgent.RecordAgent(
                  { Tourny = tourny
                    Pgn = Some pgnAgent
                    ReferenceGames = referenceGames
                    GamesAlreadyPlayed = gamesAlreadyPlayed
                    PeriodicEvery = periodicEvery }, logger)

          // 3) the engines and the games played on them. With several boards, "the next games"
          //    an engine is kept for are the next `concurrency` pairings in plan order, roughly
          //    what the boards pick next (under prevention the gate can pick differently).
          use runner =
              new GameRunner.GameRunner(
                  { Logger = logger
                    Tourny = tourny
                    Callback = callback
                    Feed = feed.Emit
                    FeedAny = feed.Any
                    Cts = cts
                    // A user can adjudicate "the" running game only when there is exactly one; with
                    // several boards the request has no single target, so it is answered with None.
                    Adjudicate = if concurrency = 1 then tryGetUserAdjudication else (fun () -> None)
                    Record = record
                    Concurrency = concurrency
                    EpdBook = epdBook
                    NeededSoon = fun name -> gate.PeekNext concurrency |> List.exists (fun p -> p.White.Name = name || p.Black.Name = name) })

          // 4) worker loop: task CE with proper cancellation
          let worker i = task {
              try
                  let mutable keepGoing = true
                  while keepGoing do
                      // The channel this loop replaced threw out of WaitToReadAsync(cts.Token) the
                      // moment the run was cancelled; the gate has no token, so ask before taking
                      // the next pairing, or a cancelled run marches through the rest of the plan.
                      if cts.IsCancellationRequested then keepGoing <- false else
                      match gate.TryTake() with
                      | ReplayGate.Done -> keepGoing <- false
                      | ReplayGate.Wait released ->
                          // One line per wake-up explains why fewer boards are busy.
                          verbose (sprintf "Gate: worker %d waits - every pending game repeats one still in flight" i)
                          do! released.WaitAsync(cts.Token)
                      | ReplayGate.Start pair ->
                          try
                              try
                                  // The round label is the one id that is unique per game; the
                                  // console's own G-number is not reliable once games overlap.
                                  verbose (sprintf "Gate: worker %d starts round %s: %s vs %s" i pair.RoundNr pair.White.Name pair.Black.Name)
                                  logger.LogDebug("Worker {worker} starting {white} vs {black}", i, pair.White.Name, pair.Black.Name)
                                  let! _ = runner.Play(i, pair)
                                  ()
                              with ex ->
                                  logger.LogError(ex, "Worker {Worker} failed game {White} vs {Black}, continuing",
                                      i, pair.White.Name, pair.Black.Name)
                          finally
                              // Released whether the game was played, cancelled or failed: a key
                              // held by a dead game would stall every repeat of it for the run.
                              gate.Release pair
                              verbose (sprintf "Gate: worker %d finished round %s" i pair.RoundNr)
                          // The pause the sequential runner gave the GUI after every game: the final
                          // position stays on the board for DelayBetweenGames before the next game
                          // starts. GameLoop.prepareEngines also waits this long at the NEXT start, in parallel
                          // with readyok, which is what the old runner did too - but that wait is
                          // invisible, the board has already moved on. The console sets it to zero.
                          // After Release, so a repeat of this key is not held for the pause as well.
                          if tourny.DelayBetweenGames > TimeSpan.Zero && not cts.IsCancellationRequested then
                              do! Task.Delay(tourny.DelayBetweenGames, cts.Token)
              with
              | :? OperationCanceledException -> ()
              | :? AggregateException as ae
                  when ae.InnerExceptions |> Seq.exists (fun e -> e :? OperationCanceledException) -> ()
          }

          // 5) launch exactly p workers
          let! _ =
              [| for i in 1..concurrency -> worker i |]
              |> Task.WhenAll
              |> Async.AwaitTask

          // 6) teardown: every engine stopped at once
          do! runner.Shutdown() |> Async.AwaitTask

          // 7) collect results
          let! results = record.Results()
          let res = ResizeArray<Result>(results)
          callback (Update.PeriodicResults res)
          // Signal tournament completion over the live feed so a grid/feed viewer can show a
          // distinct "Completed" state (vs a silently dropped feed). The internal callback's own
          // EndOfTournament fires later in Tournament.fs - after these sinks are disposed - so we
          // tee it here while the recorder/HTTP sinks are still alive. No-op when no feed is set.
          feed.Emit "" (Update.EndOfTournament tourny)
          // Bounded like every other agent round-trip in the project: if the PGN agent has
          // died, an unbounded PostAndReply hangs the tournament permanently at the point
          // where all games are already played and only the ordered copy is left to write.
          let games =
            match pgnAgent.TryPostAndReply((fun reply -> ChessLibrary.FullPGNParser.GetPGNGamesWithRaw(reply)), 30000) with
            | Some games -> games
            | None ->
                logger.LogError "PGN agent did not answer within 30s; leaving the ordered PGN copy untouched"
                ResizeArray<PgnGame>()
          // writeRawPgnGamesAdjustedToFile deletes the target before writing, so handing it an
          // empty sequence would erase a good "_ordered" file from an earlier run - turning a
          // hang into data loss. Only rewrite it when we actually have the games.
          if String.IsNullOrWhiteSpace (tourny.PgnOutPath) |> not && games.Count > 0 then
              let directory = DirectoryInfo(tourny.PgnOutPath).Parent.ToString()
              let path = Path.GetFileNameWithoutExtension(tourny.PgnOutPath) + "_ordered" + ".pgn"
              let combined = Path.Combine(directory,path)
              ChessLibrary.PGNWriter.writeRawPgnGamesAdjustedToFile combined games
          return results
          finally
              feed.Dispose()
  }
