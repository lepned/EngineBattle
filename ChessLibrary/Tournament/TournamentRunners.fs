module ChessLibrary.TournamentRunners

open System
open System.IO
open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.CupTypes
open ChessLibrary.SwissTypes
open ChessLibrary.LadderTypes
open ChessLibrary.TimeControlTypes
open ChessLibrary.TournamentPairing
open ChessLibrary.TournamentTypes
open ChessLibrary.GameHelpers

/// Utility functions for tournament time estimation
module TournamentUtils =
  let estimateGameDuration (white: TimeConfig) (black:TimeConfig) (movesEst : int) =
    let wFixedTicks = if white.NodeLimit then 0L else white.Fixed.Ticks
    // a time per move counts as an increment with no base
    let wIncrTicks = if white.NodeLimit then 0L elif white.IsMoveTime then white.MoveTime.Ticks else white.Increment.Ticks
    let bFixedTicks = if black.NodeLimit then 0L else black.Fixed.Ticks
    let bIncrTicks = if black.NodeLimit then 0L elif black.IsMoveTime then black.MoveTime.Ticks else black.Increment.Ticks
    let fixedTs = TimeSpan.FromTicks (wFixedTicks + bFixedTicks)
    let incrTs = TimeSpan.FromTicks (wIncrTicks + bIncrTicks)
    let fixedTime = fixedTs.TotalSeconds
    let incrTime = incrTs.TotalSeconds
    let seconds = fixedTime + (incrTime * float movesEst)
    seconds

  let estimateTournamentAndGameTime (pairs:int) (tourny:Tournament) (pairings: Pairing seq) =
    let movesEst = tourny.Adjudication.DrawOption.MinDrawMove + tourny.Adjudication.DrawOption.DrawMoveLength + 15
    let delay = tourny.DelayBetweenGames.TotalSeconds
    let mutable secs = 0.0
    for p in pairings do
      let whiteTc = tourny.FindTimeControl p.White.TimeControlID
      let blackTc = tourny.FindTimeControl p.Black.TimeControlID
      let avgGameDurationSec = estimateGameDuration whiteTc blackTc movesEst
      secs <- secs + avgGameDurationSec + delay
    let avgGameDurationSec =
      if secs = 0.0 then
        0.0
      else
        secs / (pairings |> Seq.length |> float)
    TimeSpan.FromSeconds(secs), TimeSpan.FromSeconds(avgGameDurationSec)

open TournamentUtils

/// Runs the given cleanup on every exit path of the enclosing async (bind with `use`),
/// so PGN/state agents are not leaked when a runner throws mid-tournament (e.g. a stale
/// state file naming players missing from the engine list). Owned PGN agents hold an
/// open FileStream on the output PGN.
let private onRunnerExit (cleanup: unit -> unit) =
  { new IDisposable with
      member _.Dispose() = try cleanup() with _ -> () }

/// Standard cleanup for a runner-owned PGN agent: its file is closed before the agent goes
let private pgnAgentGuard (ownsAgent: bool) (agent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage>) =
  onRunnerExit (fun () ->
    if ownsAgent then
      FullPGNParser.closePgnAgent agent
      agent.Dispose())

/// A cup, swiss or ladder run's record and engines: the round robin's, on one board.
let private oneBoard (logger: ILogger) (tourny: Tournament) callback cts adjudicate pgn referenceGames gamesAlreadyPlayed epdBook =
  let record =
    RecordAgent.RecordAgent(
      { Tourny = tourny
        Pgn = Some pgn
        ReferenceGames = referenceGames
        GamesAlreadyPlayed = gamesAlreadyPlayed
        // the round robin's cadence on one board: the console's refresh runs Ordo
        PeriodicEvery = if tourny.ConsoleOnly then 10 else 2 }, logger)
  let runner =
    new GameRunner.GameRunner(
      { Logger = logger
        Tourny = tourny
        Callback = callback
        Feed = fun _ _ -> ()
        FeedAny = false
        Cts = cts
        Adjudicate = adjudicate
        Record = record
        Concurrency = 1
        EpdBook = epdBook
        NeededSoon = fun _ -> true })
  record, runner

/// Plays a pairing on one board, then the pause between games. Some result when the game was
/// played and recorded; None when it was not (cancelled, never started, or failed - logged).
let private playOnBoard (logger: ILogger) (tourny: Tournament) (cts: CancellationTokenSource) (runner: GameRunner.GameRunner) (pair: Pairing) = async {
  if cts.IsCancellationRequested then return None
  else
    let! outcome =
      async {
        try
          let! result, (recorded: RecordAgent.Recorded) = runner.Play(1, pair) |> Async.AwaitTask
          return if recorded.Played then Some result else None
        with ex ->
          logger.LogError(ex, "Game {White} vs {Black} not played", pair.White.Name, pair.Black.Name)
          return None }
    // the final position stays on the board this long; the console sets it to zero
    if tourny.DelayBetweenGames > TimeSpan.Zero && not cts.IsCancellationRequested then
      try do! Threading.Tasks.Task.Delay(tourny.DelayBetweenGames, cts.Token) |> Async.AwaitTask
      with _ -> ()
    return outcome }


/// A mode's state file, behind the agent that reads and writes it.
type private Store<'S> =
  { Read: unit -> 'S option
    Write: 'S -> unit
    Close: unit -> unit }

/// What makes a cup, a Swiss or a ladder; the rest of a run is the same for all three.
type private Mode<'S, 'M> =
  { Name: string
    /// whose state file it keeps (StatePaths: next to the PGN unless configured)
    Kind: StatePaths.Mode
    /// before anything starts; throws when the tournament cannot be played
    Check: ILogger -> Tournament -> unit
    Store: string -> Store<'S>
    Create: ILogger -> Tournament -> ChessLibrary.PGNTypes.PgnGame list -> 'S option -> int -> 'M
    Step: 'M -> ModeRunner.Event -> 'M * ModeRunner.Effect<'S> list
    TotalGames: 'M -> int
    /// the games StartOfTournament announces (a cup: the fewest it can take)
    Announced: 'M -> int
    Played: 'M -> int
    /// tells the GUI the state file changed
    Updated: Update }

/// One run of a mode: its machine driven over one board, its state file saved as it goes.
/// `play`: a stand-in for the games (tests run whole tournaments with it); None plays them.
let private runMode (mode: Mode<'S, 'M>) (play: (Pairing -> Async<Result option>) option) (logger: ILogger) (tourny: Tournament) (callback: Update -> unit) (cts: CancellationTokenSource) tryGetUserAdjudication (pgnAgent: MailboxProcessor<ChessLibrary.FullPGNParser.PgnGameMessage> option) = async {
  // what makes the draw and the opening orders, so a run can be repeated
  RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Cyan (sprintf "Opening.Seed = %d (draw and opening orders)" tourny.Opening.Seed)
  logger.LogInformation("{Mode} tournament about to start", mode.Name)
  mode.Check logger tourny

  let (games, epdBook) = loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds
  let openings = games |> Seq.toList
  if openings.IsEmpty then
    logger.LogError("No openings available for {Mode} tournament", mode.Name)
    failwith $"No openings available for {mode.Name} tournament."
  let gamesAlreadyPlayed = loadGamesAlreadyPlayed tourny.PgnOutPath
  let referenceGames = loadReferenceGames tourny.ReferencePGNPath

  let pgnGameWriterAgent, ownsAgent =
    match pgnAgent with
    | Some a -> a, false
    | None -> FullPGNParser.startPgnGameReaderWriter tourny.PgnOutPath, true
  use _pgnGuard = pgnAgentGuard ownsAgent pgnGameWriterAgent
  let record, runner = oneBoard logger tourny callback cts tryGetUserAdjudication pgnGameWriterAgent referenceGames gamesAlreadyPlayed epdBook
  use _runner = runner
  let statePath, prepared = StatePaths.prepare Environment.CurrentDirectory tourny mode.Kind
  prepared |> Option.iter (fun text ->
    logger.LogInformation("{Text}", text)
    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Yellow text)
  // a new tournament in a PGN that has another's games mixes the two: only when asked for
  let orphan = StatePaths.orphanGames Environment.CurrentDirectory tourny mode.Kind
  if orphan > 0 && not tourny.AppendToPgn then
    let text =
      $"{tourny.PgnOutPath} already has {orphan} game(s) but there is no {mode.Name} state file to resume from ({statePath}). "
      + "Starting would put a new tournament into the same PGN. Set another PgnOutPath, or start with --append to add to this file."
    // the run's error: the runner prints and logs it once, and the console exits 1
    failwith text
  let store = mode.Store statePath
  use _stateGuard = onRunnerExit store.Close

  let machine = mode.Create logger tourny openings (store.Read ()) gamesAlreadyPlayed.Length
  tourny.TotalGames <- mode.TotalGames machine
  callback (Update.TotalNumberOfPairs tourny.TotalGames)
  let announced = mode.Announced machine
  let (tTime, gTime) = estimateTournamentAndGameTime announced tourny []
  callback (Update.StartOfTournament { NumberOfGames = announced; TournamentDurationSec = tTime; GameDurationInSec = gTime; Tournament = Some tourny })
  tourny.CurrentGameNr <- mode.Played machine

  let outlets : ModeRunner.Outlets<'S> =
    { Play = (match play with Some f -> f | None -> playOnBoard logger tourny cts runner)
      // saved before the GUI hears of it: its dialogs read the file
      Persist = fun state ->
        store.Write state
        callback mode.Updated
      Callback = callback
      Logger = logger
      AddTotalGames = fun n ->
        tourny.TotalGames <- tourny.TotalGames + n
        callback (Update.TotalNumberOfPairs tourny.TotalGames) }
  let! _ = ModeRunner.drive outlets cts mode.Step machine

  do! runner.Shutdown() |> Async.AwaitTask
  let! results = record.Results()
  callback (Update.PeriodicResults (ResizeArray<Result>(results)))
  return results
}

let private cupMode strategy uniquePerMatchOnly resumeRequested : Mode<CupBracket, CupMachine.State> =
  { Name = "Cup"
    Kind = StatePaths.Cup
    Check = fun logger t ->
      let n = t.EngineSetup.Engines.Length
      if not (n > 0 && n &&& (n - 1) = 0) then
        logger.LogError("Cup tournaments require a power-of-two number of players, got {playerCount}", n)
        failwith "Cup tournaments require a power-of-two number of players."
    Store = fun path ->
      let a = TournamentState.startCupBracketReaderWriter path
      { Read = fun () -> a.PostAndReply(fun reply -> ReadCupBracket reply)
        Write = fun b -> a.PostAndReply(fun reply -> WriteCupBracket(b, reply))
        Close = fun () -> a.Post DisposeCupBracket }
    Create = fun logger t openings loaded played ->
      if resumeRequested then
        match loaded with
        | None ->
            logger.LogError("Resume requested for cup tournament but no bracket data was found.")
            failwith "Resume requested but cup bracket data is missing."
        | Some b when b.Rounds.Count = 0 ->
            logger.LogError("Resume requested for cup tournament but bracket rounds are empty.")
            failwith "Resume requested but cup bracket is empty."
        | Some _ -> ()
      CupMachine.create (CupMachine.configOf t strategy uniquePerMatchOnly openings) loaded played
    Step = CupMachine.step
    TotalGames = CupMachine.currentTotalGames
    Announced = CupMachine.minTotalGames
    Played = CupMachine.played
    Updated = Update.CupBracketUpdated }

let private swissMode : Mode<SwissState, SwissMachine.State> =
  { Name = "Swiss"
    Kind = StatePaths.Swiss
    Check = fun logger t ->
      if t.EngineSetup.Engines.Length % 2 = 1 then
        logger.LogInformation("Swiss tournament has an odd number of players; a bye will be assigned each round.")
    Store = fun path ->
      let a = TournamentState.startSwissStateReaderWriter path
      { Read = fun () -> a.PostAndReply(fun reply -> ReadSwissState reply)
        Write = fun s -> a.PostAndReply(fun reply -> WriteSwissState(s, reply))
        Close = fun () -> a.Post DisposeSwissState }
    Create = fun logger t openings loaded played ->
      let cfg = SwissMachine.configOf t openings
      let configuredRounds = if t.SwissOptions.Rounds > 0 then t.SwissOptions.Rounds else t.Rounds
      if configuredRounds > cfg.TotalRounds then
        logger.LogInformation("Swiss rounds capped to {MaxRounds} based on {Players} players.", cfg.TotalRounds, cfg.Players.Length)
      SwissMachine.create cfg loaded played
    Step = SwissMachine.step
    TotalGames = SwissMachine.totalGamesOf
    Announced = SwissMachine.totalGamesOf
    Played = SwissMachine.played
    Updated = SwissStateUpdated }

let private ladderMode : Mode<LadderState, LadderMachine.State> =
  { Name = "Ladder"
    Kind = StatePaths.Ladder
    Check = fun logger t ->
      let n = t.EngineSetup.Engines.Length
      if n < 2 then
        logger.LogError("Ladder tournaments require at least 2 engines, got {playerCount}", n)
        failwith "Ladder tournaments require at least 2 engines."
    Store = fun path ->
      let a = TournamentState.startLadderStateReaderWriter path
      { Read = fun () -> a.PostAndReply(fun reply -> ReadLadderState reply)
        Write = fun s -> a.PostAndReply(fun reply -> WriteLadderState(s, reply))
        Close = fun () -> a.Post DisposeLadderState }
    Create = fun _ t openings loaded played -> LadderMachine.create (LadderMachine.configOf t openings) loaded played
    Step = LadderMachine.step
    TotalGames = LadderMachine.totalGames
    Announced = LadderMachine.totalGames
    Played = LadderMachine.played
    Updated = Update.LadderStateUpdated }

let cupWith play strategy uniquePerMatchOnly resumeRequested logger tourny callback cts tryGetUserAdjudication pgnAgent =
  runMode (cupMode strategy uniquePerMatchOnly resumeRequested) play logger tourny callback cts tryGetUserAdjudication pgnAgent

let swissWith play logger tourny callback cts tryGetUserAdjudication pgnAgent =
  runMode swissMode play logger tourny callback cts tryGetUserAdjudication pgnAgent

let ladderWith play logger tourny callback cts tryGetUserAdjudication pgnAgent =
  runMode ladderMode play logger tourny callback cts tryGetUserAdjudication pgnAgent

let private modeOf (tourny: Tournament) =
  if String.IsNullOrWhiteSpace tourny.TournamentMode then "" else tourny.TournamentMode.Trim().ToLowerInvariant()

/// Cup, Swiss and ladder play from their own state file, one game at a time; the round robin
/// and gauntlet run on the worker runner.
let keepsItsOwnState (tourny: Tournament) =
  match modeOf tourny with
  | "cup" | "swiss" | "ladder" -> true
  | _ -> false

/// The run of a cup, Swiss or ladder - the one place a mode picks its runner; None for the
/// round robin and gauntlet. `cupResumeRequested` is asked only for a cup.
let tryRun (cupResumeRequested: unit -> bool) logger (tourny: Tournament) callback cts tryGetUserAdjudication pgnAgent =
  match modeOf tourny with
  | "cup" ->
      let seeding =
        match tourny.CupOptions.SeedingStrategy with
        | s when not (isNull s) && s.Equals("random", StringComparison.OrdinalIgnoreCase) -> PairingHelper.CupSeedingStrategy.Random
        | _ -> PairingHelper.CupSeedingStrategy.ByRating
      Some (cupWith None seeding tourny.CupOptions.UniquePerMatchOnly (cupResumeRequested ()) logger tourny callback cts tryGetUserAdjudication pgnAgent)
  | "swiss" -> Some (swissWith None logger tourny callback cts tryGetUserAdjudication pgnAgent)
  | "ladder" -> Some (ladderWith None logger tourny callback cts tryGetUserAdjudication pgnAgent)
  | _ -> None
