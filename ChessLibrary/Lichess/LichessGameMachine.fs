/// One Lichess game as a pure step function; its agent in LichessBot carries out the effects and posts
/// their outcome back as events.
module ChessLibrary.LichessGameMachine

open System
open ChessLibrary.LichessBotLogic

type Config =
    { BotId: string
      Greeting: string
      Goodbye: string
      Limit: SearchLimit
      /// A book is set: its moves are asked for while bookAllowed.
      Book: bool
      BookDepth: int
      /// An opponent that has not made its first move after this long is aborted (0: never).
      AbortSeconds: int }

type State =
    { Config: Config
      GameId: string
      Full: GameFull option
      /// The newest state of the game.
      Latest: GameState option
      /// The move count the bot last played at (-1: none yet).
      Moved: int
      /// The move count the bot is deciding a move at: asking the book, searching or sending.
      Busy: int option
      /// The move on its way to Lichess: count, UCI, from the book.
      Sending: (int * string * bool) option
      Greeted: bool
      /// The bot resigned: it plays no more, and still writes the game when Lichess ends it.
      Resigned: bool
      Over: bool
      BookPlies: Set<int>
      LeftBook: bool
      LastFromBook: bool
      /// The engine's last view, for the chat and the page.
      Eval: string option
      Top: string option
      /// The same eval as a number, White's view: pawns, or mate in N.
      Score: (float option * int option)
      /// The wait before the stream is opened again (ms), doubling to a minute.
      Backoff: int
      /// The move count the opponent's first move is awaited at (a timer runs), and whether the bot aborted.
      AwaitingFirst: int option
      Aborting: bool }

type Event =
    | Message of GameMessage * ChatInfo
    | BookAnswered of count: int * uci: string option
    /// None: the engine could not give a move.
    | Searched of count: int * uci: string option
    | EngineView of eval: string option * top: string option * score: (float option * int option) option
    | MoveDelivered of count: int
    | MoveRefused of count: int * why: string
    | Retry of count: int
    /// The stream ended: 404 is a game that is gone, anything else is opened again.
    | StreamEnded of status: int * why: string
    /// The timer for the opponent's first move at this count has run out.
    | FirstMoveDue of count: int

type Effect =
    | LookUpBook of count: int * fen: string
    | Search of count: int * position: string * go: string
    | SendMove of count: int * uci: string
    | RetryAfter of count: int * ms: int
    | Chat of room: string * text: string
    | Resign of why: string
    /// Aborts the game; when Lichess takes it, the opponent never moved and matchmaking leaves it alone a while.
    | Abort of opponent: string
    | FirstMoveTimer of count: int * ms: int
    | WritePgn of GameFull * GameState * bookPlies: Set<int>
    | Show of LichessBotGame
    | Log of string
    | OpenStream of delayMs: int
    /// The game is over: its stream is closed and its engine goes back.
    | Finished

let bookNote = "The last move came from the opening book: no search yet."

let initial (config: Config) (gameId: string) =
    { Config = config; GameId = gameId; Full = None; Latest = None; Moved = -1; Busy = None; Sending = None
      Greeted = false; Resigned = false; Over = false; BookPlies = Set.empty; LeftBook = not config.Book
      LastFromBook = false; Eval = None; Top = None; Score = (None, None); Backoff = 5000; AwaitingFirst = None; Aborting = false }

/// The position now: the game, its state, the position command, the FEN and the move count; None
/// before the game is known or when a move does not play.
let private current (st: State) =
    match st.Full, st.Latest with
    | Some g, Some s ->
        match position g s (g.White.Id = st.Config.BotId) with
        | Some (command, ourTurn) ->
            let b = Chess.Board()
            b.ResetBoardState()
            b.PlayCommands(command.Replace("position startpos", "position fen " + b.FEN()))
            Some (g, s, command, b.FEN(), s.Moves.Length, ourTurn)
        | None -> None
    | _ -> None

let private view (st: State) (g: GameFull) (s: GameState) (fen: string) (eval: string) : LichessBotGame =
    let pawns, mate = if st.LastFromBook then None, None else st.Score
    { GameId = st.GameId; White = g.White.Name; Black = g.Black.Name; BotIsWhite = (g.White.Id = st.Config.BotId)
      WhiteRating = Option.toNullable g.White.Rating; BlackRating = Option.toNullable g.Black.Rating
      Fen = fen; WTime = s.WTime; BTime = s.BTime; Eval = eval; Status = s.Status
      EvalPawns = Option.toNullable pawns; EvalMate = Option.toNullable mate }

let private shownEval (st: State) = if st.LastFromBook then "book" else defaultArg st.Eval ""

/// The game ended: the PGN first, then the goodbye (a chat that fails loses nothing). An aborted
/// game is not a game: neither.
let private finish (st: State) (g: GameFull) (s: GameState) =
    let result = match s.Winner with Some w -> sprintf "%s won" w | None -> s.Status
    { st with Over = true; Busy = None; Sending = None },
    if s.Status = "aborted" then [ Log (sprintf "%s aborted" st.GameId); Finished ]
    else
        [ Log (sprintf "%s finished: %s (%s)" st.GameId result s.Status)
          WritePgn (g, s, st.BookPlies)
          if st.Config.Goodbye <> "" then Chat ("player", st.Config.Goodbye)
          Finished ]

/// The bot's turn, when nothing is under way: the book's move while it may have one, else a search.
let private decide (st: State) =
    if st.Over then st, []
    else
        match st.Full, st.Latest with
        | Some g, Some s when isOver s -> finish st g s
        | Some _, Some _ ->
            match current st with
            | None when not st.Resigned ->
                { st with Resigned = true }, [ Log (sprintf "%s: a move from Lichess does not play on the board, resigning" st.GameId); Resign "a move from Lichess does not play on the board" ]
            | None -> st, []
            | Some (g, s, command, fen, count, ourTurn) ->
                let shown = Show (view st g s fen (shownEval st))
                // the opponent's first move (before both have moved a game can be aborted): a timer
                if not ourTurn && count < 2 && st.Config.AbortSeconds > 0 && st.AwaitingFirst <> Some count && not st.Aborting then
                    { st with AwaitingFirst = Some count }, [ shown; FirstMoveTimer (count, st.Config.AbortSeconds * 1000) ]
                elif not ourTurn || count = st.Moved || st.Busy.IsSome || st.Resigned then st, [ shown ]
                elif not st.LeftBook && bookAllowed g count st.Config.BookDepth then
                    { st with Busy = Some count }, [ shown; LookUpBook (count, fen) ]
                else
                    { st with Busy = Some count; LeftBook = true; LastFromBook = false },
                    [ shown; Search (count, command, goCommand st.Config.Limit s) ]
        | _ -> st, []

/// A move decided at `count`: sent, unless the game moved on meanwhile.
let private play (st: State) (count: int) (uci: string) (fromBook: bool) =
    match current st with
    | Some (_, _, _, _, now, true) when now = count && not st.Over && not st.Resigned ->
        // marked before the move goes out: a chat line can come back before Lichess's answer
        { st with Sending = Some (count, uci, fromBook); LastFromBook = fromBook }, [ SendMove (count, uci) ]
    | _ -> decide { st with Busy = None }

let step (st: State) (event: Event) : State * Effect list =
    match event with
    | Message (GameMessage.Full g, _) ->
        let greet = not st.Greeted && st.Config.Greeting <> ""
        let st, effects = decide { st with Full = Some g; Latest = Some g.State; Greeted = st.Greeted || greet; Backoff = 5000 }
        st, (if greet then [ Chat ("player", st.Config.Greeting) ] else []) @ effects
    | Message (GameMessage.State s, _) ->
        match st.Full with
        | Some _ ->
            let greet = not st.Greeted && st.Config.Greeting <> ""
            let st, effects = decide { st with Latest = Some s; Greeted = st.Greeted || greet; Backoff = 5000 }
            st, (if greet then [ Chat ("player", st.Config.Greeting) ] else []) @ effects
        | None -> st, []
    | Message (GameMessage.Chat (room, user, text), info) ->
        if user = "lichess" || String.Equals(user, st.Config.BotId, StringComparison.OrdinalIgnoreCase) then st, []
        else
            let eval, top = if st.LastFromBook then Some bookNote, Some bookNote else st.Eval, st.Top
            match chatAnswer info room text eval top with
            | Some answer -> st, [ Chat (room, answer) ]
            | None -> st, []
    | Message ((OpponentGone _ | OtherGameMessage _), _) -> st, []
    | BookAnswered (count, Some uci) when st.Busy = Some count -> play st count uci true
    | BookAnswered (count, None) when st.Busy = Some count ->
        // out of the book for good: the engine plays this move and the rest
        match current st with
        | Some (_, s, command, _, now, true) when now = count && not st.Over ->
            { st with LeftBook = true; LastFromBook = false }, [ Search (count, command, goCommand st.Config.Limit s) ]
        | _ -> decide { st with Busy = None; LeftBook = true }
    | Searched (count, Some uci) when st.Busy = Some count -> play st count uci false
    | Searched (count, None) when st.Busy = Some count && not st.Over ->
        { st with Busy = None; Resigned = true }, [ Log (sprintf "%s: the engine cannot play, resigning" st.GameId); Resign "the engine cannot play" ]
    | EngineView (eval, top, score) ->
        { st with Eval = (match eval with Some _ -> eval | None -> st.Eval); Top = (match top with Some _ -> top | None -> st.Top)
                  Score = defaultArg score st.Score }, []
    | MoveDelivered count ->
        match st.Sending with
        | Some (c, uci, fromBook) when c = count ->
            let st = { st with Moved = count; Busy = None; Sending = None; BookPlies = (if fromBook then st.BookPlies.Add count else st.BookPlies) }
            let note = if fromBook then [ Log (sprintf "%s: book move %s" st.GameId uci) ] else []
            let st, effects = decide st
            st, note @ effects
        | _ -> st, []
    | MoveRefused (count, why) ->
        match st.Sending with
        | Some (c, uci, _) when c = count -> st, [ Log (sprintf "%s: the move %s did not reach Lichess (%s), trying again" st.GameId uci why); RetryAfter (count, 1000) ]
        | _ -> st, []
    | Retry count ->
        match st.Sending, current st with
        | Some (c, uci, _), Some (_, _, _, _, now, true) when c = count && now = count && not st.Over -> st, [ SendMove (count, uci) ]
        | _ -> decide { st with Sending = None; Busy = None }
    | StreamEnded (status, why) ->
        if st.Over then st, []
        elif status = 404 then { st with Over = true }, [ Log (sprintf "%s: the game is gone" st.GameId); Finished ]
        else
            { st with Backoff = min 60000 (st.Backoff * 2) },
            [ Log (sprintf "%s: the game stream ended (%s), opening it again in %d s" st.GameId why (st.Backoff / 1000)); OpenStream st.Backoff ]
    | FirstMoveDue count ->
        match current st with
        | Some (g, _, _, _, now, false) when now = count && not st.Over && not st.Aborting ->
            let opponent = if g.White.Id = st.Config.BotId then g.Black.Name else g.White.Name
            { st with Aborting = true },
            [ Log (sprintf "%s: %s did not move in %d s, aborting" st.GameId opponent st.Config.AbortSeconds); Abort opponent ]
        | _ -> st, []
    | BookAnswered _ | Searched _ -> st, []
