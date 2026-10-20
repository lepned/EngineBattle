/// One engine for all the bot's games, as a pure step function: state + event -> state + effects;
/// LichessBot runs it in an agent. With a fixed number of nodes a move takes milliseconds, so one
/// engine (one net loaded) plays every game: the games' searches queue and run one at a time, each
/// with its full position. An engine that fails is started again and the search tried once more on
/// it; a search that fails twice is answered None (the game resigns, as with an engine of its own).
module ChessLibrary.LichessEngineQueue

type Request =
    { Token: int
      Position: string
      Go: string
      /// Tried on an engine that failed already.
      Retried: bool }

type EngineState<'E> =
    | NoEngine
    | Starting
    | Ready of 'E

type State<'E> =
    { Engine: EngineState<'E>
      Queue: Request list
      Running: Request option
      Stopped: bool }

type Event<'E> =
    | Ask of token: int * position: string * go: string
    /// The engine started (or the warm one is handed over).
    | Started of 'E
    | StartFailed of why: string
    /// The search's move; None when the engine failed.
    | Done of token: int * move: string option
    /// The game that asked has ended: a waiting search is dropped, a running one stopped.
    | Cancel of token: int
    | Shutdown

type Effect<'E> =
    | StartEngine
    | RunSearch of 'E * Request
    /// Ends the running search now (it answers with its best move so far).
    | StopSearch of 'E
    | Reply of token: int * move: string option
    | QuitEngine of 'E
    | Log of string

let initial : State<'E> = { Engine = NoEngine; Queue = []; Running = None; Stopped = false }

/// The next search when the engine is free, or the engine started when there is none.
let private next (st: State<'E>) =
    match st.Running, st.Queue, st.Engine with
    | None, r :: rest, Ready e -> { st with Running = Some r; Queue = rest }, [ RunSearch (e, r) ]
    | None, _ :: _, NoEngine when not st.Stopped -> { st with Engine = Starting }, [ StartEngine ]
    | _ -> st, []

let step (st: State<'E>) (event: Event<'E>) : State<'E> * Effect<'E> list =
    match event with
    | Ask (token, _, _) when st.Stopped -> st, [ Reply (token, None) ]
    | Ask (token, position, go) -> next { st with Queue = st.Queue @ [ { Token = token; Position = position; Go = go; Retried = false } ] }
    | Started e when st.Stopped -> st, [ QuitEngine e ]
    | Started e ->
        match st.Engine with
        // a second one (a warm engine handed over while one started): not needed
        | Ready _ -> st, [ QuitEngine e ]
        | _ -> next { st with Engine = Ready e }
    | StartFailed why ->
        // no engine: every search waiting is answered None, the next game asks again
        { st with Engine = NoEngine; Queue = [] },
        Log (sprintf "the engine does not start: %s" why) :: (st.Queue |> List.map (fun r -> Reply (r.Token, None)))
    // shutting down: the answer as it is, nothing started again
    | Done (token, move) when st.Stopped -> { st with Running = None }, [ Reply (token, move) ]
    | Done (token, move) ->
        match st.Running with
        | Some r when r.Token = token ->
            match move, st.Engine with
            | Some _, _ -> next { st with Running = None } |> fun (st, effects) -> st, Reply (token, move) :: effects
            | None, engine ->
                // the engine failed: it is quit and started again; this search tried once more on it
                let quit = match engine with Ready e -> [ QuitEngine e ] | _ -> []
                let st = { st with Running = None; Engine = NoEngine }
                if r.Retried then
                    let st, effects = next st
                    st, quit @ [ Log "the engine failed again"; Reply (token, None) ] @ effects
                else
                    let st, effects = next { st with Queue = { r with Retried = true } :: st.Queue }
                    st, quit @ [ Log "the engine failed, starting it again" ] @ effects
        | _ -> st, []
    | Cancel token ->
        match st.Running, st.Engine with
        | Some r, Ready e when r.Token = token -> st, [ StopSearch e ]
        | _ ->
            if st.Queue |> List.exists (fun r -> r.Token = token) then
                { st with Queue = st.Queue |> List.filter (fun r -> r.Token <> token) }, [ Reply (token, None) ]
            else st, []
    | Shutdown ->
        let quit = match st.Engine with Ready e -> [ QuitEngine e ] | _ -> []
        { st with Stopped = true; Engine = NoEngine; Queue = [] },
        quit @ (st.Queue |> List.map (fun r -> Reply (r.Token, None)))
