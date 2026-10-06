/// One game's updates, sent on by an agent of their own: the engines' agents and the game loop only
/// post, so a slow sink (the live-feed file, the pages) never holds up reading an engine.
module ChessLibrary.Game.UpdateAgent

open System
open Microsoft.Extensions.Logging
open ChessLibrary.TournamentTypes

type private Message =
  | Publish of Update
  /// Answered once everything posted before it has been sent on.
  | Flush of AsyncReplyChannel<unit>

/// How long a game waits at its end for its updates to go out: a sink that never returns must not
/// keep the game from ending.
let flushTimeoutMs = 10_000

/// Sends each update on to `callback`, in the order posted.
type UpdateAgent(callback: Update -> unit, logger: ILogger) =
  let agent =
    MailboxProcessor<Message>.Start(fun inbox ->
      let rec loop () = async {
        match! inbox.Receive() with
        | Publish update ->
            // a failing sink must not stop the agent: an exception here would end the process
            try callback update
            with ex -> logger.LogWarning(ex, "Update {Update} not delivered", update.GetType().Name)
        | Flush reply -> reply.Reply ()
        return! loop () }
      loop ())

  /// Never waits.
  member _.Post(update: Update) = agent.Post (Publish update)

  /// Completes once every update posted so far has been sent on, or after flushTimeoutMs.
  member _.Flush() : Async<unit> = async {
    let! answered = agent.PostAndTryAsyncReply(Flush, flushTimeoutMs)
    if answered.IsNone then logger.LogWarning("Updates still not sent on after {Ms} ms", flushTimeoutMs) }

/// Runs `work` with a new agent's Post as its callback. Everything it posted has been sent on
/// before its result, or its exception, comes back: what follows a game (the next game, its result,
/// the feed's close) never overtakes the game's own updates.
let run (callback: Update -> unit) (logger: ILogger) (work: (Update -> unit) -> Async<'T>) : Async<'T> = async {
  let updates = UpdateAgent(callback, logger)
  let! outcome = Async.Catch (work updates.Post)
  do! updates.Flush()
  match outcome with
  | Choice1Of2 result -> return result
  | Choice2Of2 ex ->
      Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw()
      return Unchecked.defaultof<'T> }
