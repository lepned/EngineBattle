/// What the Cup, Swiss and Ladder machines have in common, and the one driver that runs them: it
/// plays the games a machine asks for, feeds back how they ended and carries out the rest.
module ChessLibrary.ModeRunner

open System.Threading
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TournamentTypes

type Event =
  | Start
  /// The game asked for ended: Some result when it was played, None when it was not.
  | GameEnded of Result option
  /// The run was cancelled: the machine finishes the round's bookkeeping, plays nothing more.
  | Cancel

/// `'S`: the mode's state as its file holds it.
type Effect<'S> =
  | Play of Pairing
  /// Write the state file; the driver sends the mode's state-updated notice after it.
  | Persist of 'S
  | Notify of Update
  /// Games added to the tournament's total (a playoff, a tiebreak).
  | AddTotalGames of int
  | Info of string
  | Critical of string
  /// A line for the console (a ladder's standings).
  | Print of string
  /// Stop the run (a cup match that cannot be played): the driver cancels it.
  | StopRun
  | Finished

/// The ways the driver reaches the world, so a test can stand in for the games.
type Outlets<'S> =
  { Play: Pairing -> Async<Result option>
    /// Writes the state file and sends the state-updated notice.
    Persist: 'S -> unit
    Callback: Update -> unit
    Logger: ILogger
    /// The tournament's total games, which a playoff raises.
    AddTotalGames: int -> unit }

/// Runs a machine from Start until it finishes or the run is cancelled; the machine as it ended.
/// Every step's effects are carried out. A game that ends as the run is cancelled is still
/// reported (and saved when it was played) after the machine is told of the cancellation, as
/// the runners did: the round's bookkeeping goes on, nothing more is played.
let drive (outlets: Outlets<'S>) (cts: CancellationTokenSource) (step: 'M -> Event -> 'M * Effect<'S> list) (machine: 'M) : Async<'M> =
  let carryOut (machine: 'M, effects: Effect<'S> list) =
    let mutable next = None
    for effect in effects do
      match effect with
      | Persist s -> outlets.Persist s
      | Notify u -> outlets.Callback u
      | AddTotalGames n -> outlets.AddTotalGames n
      | Info text -> outlets.Logger.LogInformation("{Text}", text)
      | Critical text -> outlets.Logger.LogCritical("{Text}", text)
      | Print text -> printfn "%s" text
      | StopRun -> cts.Cancel()
      | Finished -> ()
      | Play pair -> next <- Some pair
    machine, next
  let rec loop (machine: 'M) (event: Event) = async {
    match carryOut (step machine event) with
    | machine, Some pair when not cts.IsCancellationRequested ->
        let! result = outlets.Play pair
        if cts.IsCancellationRequested then
          let machine, _ = carryOut (step machine Cancel)
          let machine, _ = carryOut (step machine (GameEnded result))
          return machine
        else return! loop machine (GameEnded result)
    | machine, _ -> return machine }
  loop machine Start
