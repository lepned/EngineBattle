namespace ChessLibrary

open System
open EngineTypes

/// The analysis engine's protocol state, as a pure step function: state + event -> state + effects.
/// AnalysisEngine runs it in an agent; the parsing of search output is AnalysisOutput's.
module internal AnalysisMachine =

  /// A search with an empty Go is a request for nothing (a position with no legal move): it ends at
  /// once as superseded, in turn with the others.
  type Search = { Id: int; Position: string; Go: string }

  /// How a search ended, for a caller that awaits it.
  type Outcome =
    | Completed of BestMoveInfo option
    /// Replaced by a newer search, or stopped before its go was sent.
    | Superseded
    | Failed of string

  type Barrier =
    | ForBestMove
    | ForReadyOk

  type Phase =
    /// `uci` sent; the lines until uciok (newest first).
    | AwaitingUciOk of lines: string list
    /// Options known; waiting for the agent's init commands.
    | AwaitingInit
    /// Init commands and isready sent.
    | AwaitingStartReady
    | Idle
    /// isready sent before go; `stop` asks for a go that is stopped at once (a move now).
    | Readying of Search * stop: bool
    | Searching of Search * stopSent: bool
    /// Output is dropped until the barrier line (a stopped search's bestmove, or readyok).
    | Draining of Barrier
    /// Stopped answering, or never started; a late answer revives it.
    /// `owesBestMove`: a stopped search never answered, so its bestmove may still come.
    | Unresponsive of reason: string * owesBestMove: bool
    | Closed

  type State =
    { Phase: Phase
      /// When the phase's wait began.
      Since: TimeSpan
      Output: AnalysisOutput.State
      /// The newest search waiting for the engine.
      Next: Search option
      /// Commands for an idle engine, oldest first.
      Queued: string list
      LiveStats: bool
      /// The search started last: output read while idle is its trailing output.
      Last: int option }

  type Settings =
    { Name: string
      CanPing: bool
      /// Whether `stop` makes the engine print a bestmove for this go (Winboard's exit does not).
      StopAnswers: string -> bool
      StartTimeout: TimeSpan
      PingTimeout: TimeSpan
      StopWait: TimeSpan }

  type Event =
    | Line of string
    | OutputClosed
    | Init of commands: string list
    /// The newest wins.
    | Analyse of Search
    | Stop
    /// Commands for an idle engine; `restart` reruns a running search after them.
    | Configure of commands: string list * restart: bool
    /// Sent at once (UCI script lines, uci).
    | Raw of string
    | Tick

  type Effect =
    | Send of string
    /// Set the board for SAN before the position is sent.
    | UsePosition of search: int * command: string
    /// The search it belongs to; None for the engine's own (Ready, EngineFailed).
    | Emit of EngineUpdate * search: int option
    | Reply of id: int * Outcome
    /// The handshake lines until uciok; the agent answers with Init.
    | OptionsReceived of string list
    | Print of string
    | Debug of string
    | Warn of string

  let initial uci =
    { Phase = (if uci then AwaitingUciOk [] else AwaitingInit); Since = TimeSpan.Zero
      Output = AnalysisOutput.State.Initial; Next = None; Queued = []; LiveStats = false; Last = None }

  let private goto phase now (state: State) = { state with Phase = phase; Since = now }

  let private isBestMove (line: string) = line.StartsWith("bestmove", StringComparison.Ordinal)
  let private isReadyOk (line: string) = line.Trim() = "readyok"

  /// A search that ends without a bestmove: the caller is told, and so is the GUI.
  let private superseded (settings: Settings) (search: Search) =
    [ Reply (search.Id, Superseded); Emit (SearchStopped settings.Name, Some search.Id) ]

  let private dropNext settings (state: State) =
    match state.Next with
    | Some next -> { state with Next = None }, superseded settings next
    | None -> state, []

  let private startSearch (settings: Settings) now (search: Search) (state: State) =
    if search.Go = "" then goto Idle now state, superseded settings search else
    let state = { state with Output = AnalysisOutput.withoutPv state.Output; Last = Some search.Id }
    let open' = [ UsePosition (search.Id, search.Position); Send search.Position ]
    if settings.CanPing then goto (Readying (search, false)) now state, open' @ [ Send "isready" ]
    else goto (Searching (search, false)) now state, open' @ [ Send search.Go ]

  /// The engine is free: queued commands first, then the newest search.
  let private whenIdle (settings: Settings) now (state: State) =
    let sent = state.Queued |> List.map Send
    let state = { state with Queued = [] }
    match state.Next with
    | Some next ->
        let state, effects = startSearch settings now next { state with Next = None }
        state, sent @ effects
    | None -> goto Idle now state, sent

  let private parse (settings: Settings) (pos: AnalysisOutput.IPosition) (search: int option) (state: State) (line: string) =
    let output, effects = AnalysisOutput.step settings.Name pos state.Output line
    let converted =
      effects |> List.map (function
        | AnalysisOutput.Update update -> Emit (update, search)
        | AnalysisOutput.Print text -> Print text
        | AnalysisOutput.Debug text -> Debug text)
    { state with Output = output }, converted

  let private bestMoveIn effects =
    effects |> List.tryPick (function Emit (BestMove info, _) -> Some info | _ -> None)

  /// Stops the running search so `next` can follow; its output from here on is dropped.
  let private replace (settings: Settings) now (current: Search) stopSent (state: State) =
    let ended = superseded settings current
    if settings.StopAnswers current.Go then
      goto (Draining ForBestMove) now state, (if stopSent then [] else [ Send "stop" ]) @ ended
    elif settings.CanPing then
      goto (Draining ForReadyOk) now state, [ Send "stop"; Send "isready" ] @ ended
    else
      // no bestmove and no ping: the engine is idle once it has read the stop
      let state, effects = whenIdle settings now state
      state, [ Send "stop" ] @ ended @ effects

  let private fail (settings: Settings) now (reason: string) (state: State) =
    // every waiting search ends, for a caller and for the GUI
    let ended (s: Search) = [ Reply (s.Id, Failed reason); Emit (SearchStopped settings.Name, Some s.Id) ]
    let pending =
      match state.Phase with
      | Readying (s, _) | Searching (s, _) -> ended s
      | _ -> []
    let next = match state.Next with Some n -> ended n | None -> []
    let owes = match state.Phase with Draining ForBestMove | Searching (_, true) -> true | _ -> false
    { goto (Unresponsive (reason, owes)) now state with Next = None },
    [ Warn (sprintf "%s: %s" settings.Name reason); Emit (EngineFailed (settings.Name, reason), None) ] @ pending @ next

  let private lineEvent (settings: Settings) (pos: AnalysisOutput.IPosition) now (state: State) (line: string) =
    match state.Phase with
    | AwaitingUciOk lines ->
        if line.Trim() = "uciok" then
          let lines = List.rev lines
          let live = lines |> List.exists (fun l -> l.Contains "name LogLiveStats")
          { goto AwaitingInit now state with LiveStats = live }, [ OptionsReceived lines ]
        else { state with Phase = AwaitingUciOk (line :: lines) }, []
    | AwaitingStartReady ->
        if isReadyOk line then
          let state, effects = whenIdle settings now state
          state, [ Emit (Ready (settings.Name, state.LiveStats), None) ] @ effects
        elif EngineProcess.isFatalInitLine line then fail settings now (sprintf "failed to initialize: %s" line) state
        else state, []
    | AwaitingInit -> state, []
    | Idle -> parse settings pos state.Last state line
    | Readying (search, stop) when isReadyOk line ->
        match state.Next, state.Queued with
        | Some _, _ ->
            let state, effects = whenIdle settings now state
            state, superseded settings search @ effects
        | None, (_ :: _ as queued) ->
            goto (Readying (search, stop)) now { state with Queued = [] }, (queued |> List.map Send) @ [ Send "isready" ]
        | None, [] when stop && settings.StopAnswers search.Go ->
            goto (Searching (search, true)) now state, [ Send search.Go; Send "stop" ]
        | None, [] -> goto (Searching (search, false)) now state, [ Send search.Go ]
    | Readying _ -> state, []
    | Searching (search, _) ->
        let state, effects = parse settings pos (Some search.Id) state line
        if isBestMove line then
          let state, next = whenIdle settings now state
          state, effects @ [ Reply (search.Id, Completed (bestMoveIn effects)) ] @ next
        else state, effects
    | Draining ForBestMove when isBestMove line -> whenIdle settings now state
    | Draining ForReadyOk when isReadyOk line -> whenIdle settings now state
    | Draining _ -> state, []
    // while a bestmove is owed, a readyok only says it is still searching
    | Unresponsive (_, owes) when isBestMove line || (isReadyOk line && not owes) ->
        let state, effects = whenIdle settings now state
        state, [ Print (sprintf "%s answers again" settings.Name) ] @ effects
    | Unresponsive _ | Closed -> state, []

  let step (settings: Settings) (pos: AnalysisOutput.IPosition) (now: TimeSpan) (state: State) (event: Event) : State * Effect list =
    match event with
    | Line line -> lineEvent settings pos now state line

    | OutputClosed ->
        match state.Phase with
        | Closed -> state, []
        | _ ->
            let state, effects = fail settings now "the engine exited" state
            goto Closed now state, effects

    | Init commands ->
        match state.Phase with
        | AwaitingInit ->
            let sent = (commands @ [ "ucinewgame" ]) |> List.map Send
            if settings.CanPing then goto AwaitingStartReady now state, sent @ [ Send "isready" ]
            else
              let state, effects = whenIdle settings now state
              state, sent @ [ Emit (Ready (settings.Name, state.LiveStats), None) ] @ effects
        | _ -> state, []

    | Analyse search ->
        match state.Phase with
        | Idle -> startSearch settings now search state
        | Searching (current, stopSent) ->
            replace settings now current stopSent { state with Next = Some search }
        | Readying _ | Draining _ | AwaitingUciOk _ | AwaitingInit | AwaitingStartReady ->
            let state, dropped = dropNext settings state
            { state with Next = Some search }, dropped
        | Unresponsive _ when search.Go = "" -> state, superseded settings search
        | Unresponsive (_, true) ->
            // the old bestmove first, or it would answer the new search
            goto (Draining ForBestMove) now { state with Next = Some search },
            [ Warn (sprintf "%s: waiting again for the bestmove it owes" settings.Name) ]
        | Unresponsive _ ->
            // it may have come back; if not, the ping times out again
            let state, effects = whenIdle settings now { state with Next = Some search }
            state, [ Warn (sprintf "%s: trying again after it stopped answering" settings.Name) ] @ effects
        | Closed -> state, [ Reply (search.Id, Failed "the engine exited"); Emit (SearchStopped settings.Name, Some search.Id) ]

    | Stop ->
        let state, dropped = dropNext settings state
        match state.Phase with
        | Searching (search, false) ->
            if settings.StopAnswers search.Go then
              // its bestmove still ends it as usual
              goto (Searching (search, true)) now state, dropped @ [ Send "stop" ]
            else
              let state, effects = whenIdle settings now state
              state, dropped @ [ Send "stop" ] @ superseded settings search @ effects
        | Readying (search, _) ->
            // a move now: go, then stop at once
            { state with Phase = Readying (search, true) }, dropped
        | _ -> state, dropped

    | Configure (commands, restart) ->
        match state.Phase with
        | Idle -> state, commands |> List.map Send
        | Searching (search, false) when restart && state.Next.IsNone ->
            let state = { state with Queued = state.Queued @ commands; Next = Some search }
            if settings.StopAnswers search.Go then goto (Draining ForBestMove) now state, [ Send "stop" ]
            else
              let state, effects = whenIdle settings now state
              state, [ Send "stop" ] @ effects
        | Closed -> state, []
        // kept for when it answers again
        | _ -> { state with Queued = state.Queued @ commands }, []

    | Raw command ->
        match state.Phase with
        | AwaitingUciOk _ | AwaitingInit | AwaitingStartReady -> { state with Queued = state.Queued @ [ command ] }, []
        | Closed -> state, []
        | _ -> state, [ Send command ]

    | Tick ->
        let waited = now - state.Since
        match state.Phase with
        | AwaitingUciOk _ when waited >= settings.StartTimeout -> fail settings now "no uciok" state
        | AwaitingStartReady when waited >= settings.StartTimeout -> fail settings now "no readyok after start-up" state
        | AwaitingInit when waited >= settings.StartTimeout -> fail settings now "did not initialize" state
        | Readying _ when waited >= settings.PingTimeout -> fail settings now "no readyok" state
        | Draining ForBestMove when waited >= settings.StopWait -> fail settings now "no answer to stop" state
        | Draining ForReadyOk when waited >= settings.PingTimeout -> fail settings now "no readyok" state
        | Searching (_, true) when waited >= settings.StopWait -> fail settings now "no bestmove after stop" state
        | _ -> state, []

  /// When the next Tick is due, if any.
  let nextDue (settings: Settings) (state: State) =
    match state.Phase with
    | AwaitingUciOk _ | AwaitingInit | AwaitingStartReady -> Some (state.Since + settings.StartTimeout)
    | Readying _ -> Some (state.Since + settings.PingTimeout)
    | Draining ForBestMove | Searching (_, true) -> Some (state.Since + settings.StopWait)
    | Draining ForReadyOk -> Some (state.Since + settings.PingTimeout)
    | _ -> None
