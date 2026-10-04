/// Runs one engine's PlayerMachine for a game: a pump reads its output, an agent steps the
/// machine and carries out the effects.
module ChessLibrary.Game.Player

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open ChessLibrary.Engine
open ChessLibrary.TournamentTypes
open ChessLibrary.Game.GoCommand
open ChessLibrary.Game.PlayerMachine

/// The engine as the player needs it; tests script one in memory.
type IEngineIO =
  abstract Name: string
  abstract CanPing: bool
  abstract Send: Command -> unit
  /// The next line; null when the output ended or the token fired.
  abstract ReadLine: CancellationToken -> Task<string>

let engineIO (engine: ChessEngine) =
  { new IEngineIO with
      member _.Name = engine.Name
      member _.CanPing = engine.CanPing
      member _.Send command =
        match command with
        | Position position -> engine.Position position
        | IsReady -> engine.IsReady()
        | Go Value -> engine.GoValue()
        | Go (NodeCount nodes) -> engine.GoNodes nodes
        | Go (MoveTimeMs ms) -> engine.Go ms
        | Go (ClockTimes (control, white, black)) -> engine.Go(control, white, black)
        | GoPonder text -> engine.GoPonder text
        | PonderHit -> engine.PonderHit()
        | Stop -> engine.Stop()
      member _.ReadLine token = engine.ReadLineAsyncWithTimeout token }

type private Message =
  | Event of Event
  | ThinkMsg of ThinkRequest * AsyncReplyChannel<SearchOutcome>
  | EndMsg of AsyncReplyChannel<bool>

type Player(io: IEngineIO, settings: Settings, emit: Update -> unit, logger: ILogger) =
  let clock = Stopwatch.StartNew()
  let pumpCts = new CancellationTokenSource()

  let run (state, thinkReply: AsyncReplyChannel<SearchOutcome> option, endReply: AsyncReplyChannel<bool> option) event =
    let state, effects =
      try step settings clock.Elapsed state event
      with ex ->
        // a dead agent would leave the game waiting forever; drop the event instead
        logger.LogError(ex, "{Engine}: {Event} not handled", io.Name, event)
        state, []
    let mutable thinkReply = thinkReply
    let mutable endReply = endReply
    let mutable searchStarted = false
    for effect in effects do
      match effect with
      | Send command ->
          try io.Send command
          with ex -> logger.LogWarning(ex, "{Engine}: could not send {Command}", io.Name, command)
          match command with
          | Go _ | PonderHit -> searchStarted <- true
          | _ -> ()
      | ReplyThink outcome ->
          thinkReply |> Option.iter (fun reply -> reply.Reply outcome)
          thinkReply <- None
      | ReplyEndGame idle ->
          endReply |> Option.iter (fun reply -> reply.Reply idle)
          endReply <- None
      | Emit update ->
          try emit update
          with ex -> logger.LogWarning(ex, "{Engine}: update callback failed", io.Name)
      | Note text -> logger.LogInformation text
      | Warn text -> logger.LogWarning text
    let state = if searchStarted then startedAt clock.Elapsed state else state
    state, thinkReply, endReply

  /// A deadline is due even while messages keep coming (an engine flooding info lines).
  let runDue ((state, _, _) as current) =
    match nextDue settings state with
    | Some due when due <= clock.Elapsed -> run current Tick
    | _ -> current

  let agent =
    MailboxProcessor.Start(fun inbox ->
      let rec loop ((state, thinkReply, endReply) as current) = async {
        let timeout =
          match nextDue settings state with
          | Some due -> max 0 (int (due - clock.Elapsed).TotalMilliseconds)
          | None -> Timeout.Infinite
        let! message = inbox.TryReceive timeout
        let next =
          match message with
          | None -> run current Tick
          | Some (Event event) -> run current event
          | Some (ThinkMsg (request, reply)) -> run (state, Some reply, endReply) (Think request)
          | Some (EndMsg reply) -> run (state, thinkReply, Some reply) EndGame
        return! loop (runDue next) }
      loop (Idle, None, None))

  let pump =
    async {
      try
        let mutable reading = true
        while reading do
          let! line = io.ReadLine pumpCts.Token |> Async.AwaitTask
          if isNull line then
            reading <- false
            if not pumpCts.IsCancellationRequested then agent.Post (Event OutputClosed)
          else agent.Post (Event (Line line))
      with ex ->
        // an exception here would take the process down
        logger.LogWarning(ex, "{Engine}: output reader failed", io.Name)
        try agent.Post (Event OutputClosed) with _ -> ()
    }

  do Async.Start pump

  member _.Name = io.Name

  /// The engine's search for the position; with a ponder on `LastMove`, a hit or a miss.
  member _.Think (request: ThinkRequest) = agent.PostAndAsyncReply(fun reply -> ThinkMsg (request, reply))

  member _.Ponder (request: PonderRequest) = agent.Post (Event (Ponder request))

  /// Stops whatever runs and waits until the engine is idle; false when it stopped answering.
  member _.EndGame () = agent.PostAndAsyncReply EndMsg

  /// Stops reading. The agent is left to the GC: disposing it could fail a late post.
  interface IDisposable with
    member _.Dispose () = pumpCts.Cancel()
