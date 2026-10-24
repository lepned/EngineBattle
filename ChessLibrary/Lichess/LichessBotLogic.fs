/// The Lichess bot API's messages, the challenge filter, the go command and the chat; pure.
module ChessLibrary.LichessBotLogic

open System
open System.Text.Json.Nodes
open ChessLibrary.Chess

// ---------------------------------------------------------------- messages

/// A player of a game or a challenge: Lichess id, name, title ("BOT" for a bot), rating.
type Player = { Id: string; Name: string; Title: string; Rating: int option }

type Challenge =
    { Id: string
      Challenger: Player
      Variant: string            // standard, chess960, fromPosition, crazyhouse...
      Rated: bool
      Speed: string              // ultraBullet, bullet, blitz, rapid, classical, correspondence
      /// Seconds and seconds per move; None for correspondence
      Clock: (int * int) option
      InitialFen: string option }

/// The clocks and moves of a game as the game stream sends them (ms).
type GameState =
    { Moves: string list          // UCI, as Lichess sends them
      WTime: int64
      BTime: int64
      WInc: int64
      BInc: int64
      Status: string              // started, mate, resign, draw, outoftime, aborted...
      Winner: string option }

type GameFull =
    { Id: string
      Variant: string
      Rated: bool
      Speed: string
      White: Player
      Black: Player
      InitialFen: string          // "startpos" or a FEN
      State: GameState }

/// The account's event stream.
type Event =
    | ChallengeArrived of Challenge
    | ChallengeGone of id: string
    /// A challenge declined, with the reason Lichess shows (in English).
    | ChallengeDeclined of id: string * reason: string
    | GameStarted of gameId: string
    | GameFinished of gameId: string
    | OtherEvent of kind: string

/// One game's stream.
type GameMessage =
    | Full of GameFull
    | State of GameState
    | Chat of room: string * user: string * text: string
    | OpponentGone of gone: bool
    | OtherGameMessage of kind: string

let private str (n: JsonNode) (key: string) =
    match n with
    | null -> ""
    | n -> match n.[key] with null -> "" | v -> (try v.GetValue<string>() with _ -> v.ToJsonString())

let private int64Of (n: JsonNode) (key: string) =
    match n with
    | null -> 0L
    | n -> match n.[key] with null -> 0L | v -> (try v.GetValue<int64>() with _ -> 0L)

let private boolOf (n: JsonNode) (key: string) =
    match n with
    | null -> false
    | n -> match n.[key] with null -> false | v -> (try v.GetValue<bool>() with _ -> false)

let private player (n: JsonNode) : Player =
    { Id = str n "id"; Name = str n "name"; Title = str n "title"
      Rating = match n with null -> None | n -> (match n.["rating"] with null -> None | v -> (try Some (v.GetValue<int>()) with _ -> None)) }

let private variantKey (n: JsonNode) = match n with null -> "standard" | n -> (match str n.["variant"] "key" with "" -> "standard" | k -> k)

let private state (n: JsonNode) : GameState =
    { Moves = (str n "moves").Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
      WTime = int64Of n "wtime"; BTime = int64Of n "btime"; WInc = int64Of n "winc"; BInc = int64Of n "binc"
      Status = str n "status"
      Winner = match str n "winner" with "" -> None | w -> Some w }

let private challenge (c: JsonNode) : Challenge =
    let tc = c.["timeControl"]
    { Id = str c "id"
      Challenger = player c.["challenger"]
      Variant = variantKey c
      Rated = boolOf c "rated"
      Speed = str c "speed"
      Clock = if str tc "type" = "clock" then Some (int (int64Of tc "limit"), int (int64Of tc "increment")) else None
      InitialFen = match str c "initialFen" with "" -> None | f -> Some f }

/// One line of the event stream; None for the keep-alive empty lines.
let parseEvent (line: string) : Event option =
    if String.IsNullOrWhiteSpace line then None
    else
        let n = JsonNode.Parse line
        Some (
            match str n "type" with
            | "challenge" -> ChallengeArrived (challenge n.["challenge"])
            | "challengeCanceled" -> ChallengeGone (str n.["challenge"] "id")
            | "challengeDeclined" ->
                let c = n.["challenge"]
                ChallengeDeclined (str c "id", match str c "declineReason" with "" -> str c "declineReasonKey" | r -> r)
            | "gameStart" -> GameStarted (match str n.["game"] "gameId" with "" -> str n.["game"] "id" | id -> id)
            | "gameFinish" -> GameFinished (match str n.["game"] "gameId" with "" -> str n.["game"] "id" | id -> id)
            | other -> OtherEvent other)

/// A bot online on Lichess, from /api/bot/online: its name and ratings (None where it has no games:
/// Lichess shows a placeholder rating there).
type OnlineBot = { Name: string; Bullet: int option; Blitz: int option; Rapid: int option; Classical: int option; Chess960: int option }

/// One line of /api/bot/online (ndjson, one user per line); None for an empty line.
let parseOnlineBot (line: string) : OnlineBot option =
    if String.IsNullOrWhiteSpace line then None
    else
        let n = JsonNode.Parse line
        let rating (perf: string) =
            match n.["perfs"] with
            | null -> None
            | p ->
                match p.[perf] with
                | null -> None
                | r ->
                    let games = match r.["games"] with null -> 1 | g -> (try g.GetValue<int>() with _ -> 1)
                    if games = 0 then None
                    else match r.["rating"] with null -> None | v -> (try Some (v.GetValue<int>()) with _ -> None)
        Some { Name = (match str n "username" with "" -> str n "id" | u -> u)
               Bullet = rating "bullet"; Blitz = rating "blitz"; Rapid = rating "rapid"; Classical = rating "classical"
               Chess960 = rating "chess960" }

// ---------------------------------------------------------------- matchmaking

/// Matchmaking: after `PauseSeconds` without a game, challenge a bot rated within `RatingRange` of the bot's
/// own rating (`StartRating` until it has games), at one of `TimeControls`, at most once per `GapSeconds`.
type Matchmaking =
    { TimeControls: (int * int) list
      RatingRange: int
      StartRating: int
      Rated: bool
      PauseSeconds: int
      GapSeconds: int }

/// Lichess's speed of a time control: the estimated game time, base + 40 x increment.
let speedOf (seconds: int) (increment: int) =
    let estimate = seconds + 40 * increment
    if estimate < 30 then "ultraBullet"
    elif estimate < 180 then "bullet"
    elif estimate < 480 then "blitz"
    elif estimate < 1500 then "rapid"
    else "classical"

/// The rating a game counts for: the speed in standard chess, one Chess960 rating whatever the clock.
let perfOf (variant: string) (seconds: int) (increment: int) =
    if variant = "chess960" then "chess960" else speedOf seconds increment

/// A bot's rating in a perf (a speed, or chess960); None when it has none there.
let ratingIn (speed: string) (b: OnlineBot) =
    match speed with
    | "chess960" -> b.Chess960
    | "bullet" -> b.Bullet
    | "blitz" -> b.Blitz
    | "rapid" -> b.Rapid
    | "classical" -> b.Classical
    | _ -> None

/// The bot to challenge at a speed: one with a rating there, within `range` of `own` (any rating when
/// the bot has none there yet), not in `skip`; `draw n` picks one of n (a random index). None when no bot fits.
let pickOpponent (bots: OnlineBot[]) (own: int option) (range: int) (speed: string) (skip: string -> bool) (draw: int -> int) =
    let fits =
        bots
        |> Array.filter (fun b ->
            not (skip b.Name)
            && match ratingIn speed b, own with
               | Some r, Some mine -> abs (r - mine) <= range
               | Some _, None -> true
               | None, _ -> false)
    if fits.Length = 0 then None else Some fits.[draw fits.Length]

/// The challenge's id in Lichess's answer to a challenge (the challenge itself, or wrapped as "challenge").
let challengeIdOf (body: string) : string option =
    try
        let n = JsonNode.Parse body
        match str n "id" with
        | "" -> (match str n.["challenge"] "id" with "" -> None | id -> Some id)
        | id -> Some id
    with _ -> None

/// Lichess's error message in a refusal's body ({"error": ...} or {"global": [...]} or a field's list);
/// the body itself when it is not one of those.
let errorText (body: string) : string =
    try
        match JsonNode.Parse body with
        | :? JsonObject as o ->
            match o.["error"] with
            | null ->
                o |> Seq.tryPick (fun kv ->
                    match kv.Value with
                    | :? JsonArray as a when a.Count > 0 -> Some (string a.[0])
                    | :? JsonValue as v -> Some (string v)
                    | _ -> None)
                |> Option.defaultValue body
            | e -> string e
        | _ -> body
    with _ -> body

/// The wait a 429 asks for, in seconds: its JSON's "seconds" (Lichess's ratelimit object) or None.
let rateLimitSeconds (body: string) : int option =
    try
        match JsonNode.Parse body with
        | :? JsonObject as o ->
            let read (n: JsonNode) = match n with null -> None | n -> (try Some (n.GetValue<int>()) with _ -> None)
            match o.["ratelimit"] with
            | :? JsonObject as r -> read r.["seconds"]
            | _ -> read o.["seconds"]
        | _ -> None
    with _ -> None

/// A wait for the log: "45 s", "12 minutes", "5.3 hours".
let waitText (t: TimeSpan) =
    if t.TotalSeconds < 90.0 then sprintf "%.0f s" t.TotalSeconds
    elif t.TotalMinutes < 90.0 then sprintf "%.0f minutes" t.TotalMinutes
    else sprintf "%.1f hours" t.TotalHours

/// One line of a game stream; None for the keep-alive empty lines.
let parseGameMessage (line: string) : GameMessage option =
    if String.IsNullOrWhiteSpace line then None
    else
        let n = JsonNode.Parse line
        Some (
            match str n "type" with
            | "gameFull" ->
                Full { Id = str n "id"; Variant = variantKey n; Rated = boolOf n "rated"; Speed = str n "speed"
                       White = player n.["white"]; Black = player n.["black"]
                       InitialFen = (match str n "initialFen" with "" -> "startpos" | f -> f)
                       State = state n.["state"] }
            | "gameState" -> State (state n)
            | "chatLine" -> Chat (str n "room", str n "username", str n "text")
            | "opponentGone" -> OpponentGone (boolOf n "gone")
            | other -> OtherGameMessage other)

// ---------------------------------------------------------------- which challenges

/// Who may challenge the bot.
type Opponents =
    | Humans
    | Bots
    | Anyone

/// What the bot takes: time controls (seconds, increment) - empty for any the speeds allow -, variants,
/// rated and casual, humans and/or bots, and games at once.
type Filter =
    { TimeControls: (int * int) list
      Variants: string list
      Rated: bool
      Casual: bool
      Opponents: Opponents
      MaxGames: int }

let defaultFilter =
    { TimeControls = []; Variants = [ "standard" ]; Rated = true; Casual = true; Opponents = Anyone; MaxGames = 1 }

/// The variants the bot can challenge in, of those it takes.
let challengeVariants (f: Filter) = f.Variants |> List.filter (fun v -> v = "standard" || v = "chess960")

/// "1+0,3+2,0.5+0" (minutes+seconds) as (seconds, increment) pairs.
let parseTimeControls (text: string) : Result<(int * int) list, string> =
    if String.IsNullOrWhiteSpace text then Ok []
    else
        let one (t: string) =
            match t.Trim().Split('+') with
            | [| m; i |] ->
                match Double.TryParse(m, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture), Int32.TryParse i with
                | (true, minutes), (true, inc) when minutes >= 0.0 && inc >= 0 -> Ok (int (Math.Round(minutes * 60.0)), inc)
                | _ -> Error t
            | _ -> Error t
        let parsed = text.Split(',', StringSplitOptions.RemoveEmptyEntries) |> Array.map one
        match parsed |> Array.tryPick (function Error t -> Some t | Ok _ -> None) with
        | Some bad -> Error (sprintf "'%s' is not a time control like 3+2 (minutes+seconds)" (bad.Trim()))
        | None -> Ok (parsed |> Array.choose (function Ok v -> Some v | Error _ -> None) |> List.ofArray)

/// Lichess's decline reason (its declineReasonKey) for a challenge the bot does not take; None to accept.
/// `playing`: games the bot plays now.
let declineReason (f: Filter) (playing: int) (c: Challenge) : string option =
    let isBot = c.Challenger.Title = "BOT"
    // a challenge the bot never takes gets its reason; "later" only for one it takes when it has room
    if c.Speed = "ultraBullet" then Some "toofast"
    elif c.Clock.IsNone then Some "timecontrol"                      // correspondence
    // Lichess's keys: "standard" = "only standard chess", "variant" = "not standard chess now"
    elif not (List.contains c.Variant f.Variants) then Some (if c.Variant = "standard" then "variant" else "standard")
    elif c.Rated && not f.Rated then Some "casual"
    elif not c.Rated && not f.Casual then Some "rated"
    elif isBot && f.Opponents = Humans then Some "nobot"
    elif not isBot && f.Opponents = Bots then Some "onlybot"
    elif not f.TimeControls.IsEmpty && not (List.contains c.Clock.Value f.TimeControls) then Some "timecontrol"
    elif playing >= f.MaxGames then Some "later"
    else None

// ---------------------------------------------------------------- playing

/// How the engine searches a move: by the clock (with an overhead kept back for lag) or a fixed number
/// of nodes whatever the clock (a one-node policy bot).
type SearchLimit =
    | ByClock of overheadMs: int
    | FixedNodes of nodes: int

/// A clock search: the times less the overhead (never below 10 ms), and the increment.
let private clockSearch (overhead: int) (wtime: int64) (btime: int64) (inc: int64) =
    let clamp (t: int64) = TimeSpan.FromMilliseconds(float (max 10L (t - int64 overhead)))
    let control =
        if inc > 0L then TimeControlTypes.WithIncrement (TimeSpan.Zero, TimeSpan.FromMilliseconds(float inc))
        else TimeControlTypes.FixedTime TimeSpan.Zero
    Game.GoCommand.ClockTimes (control, clamp wtime, clamp btime)

/// The search for our move. By the clock the overhead comes off both times (never below 10 ms).
let goCommand (limit: SearchLimit) (s: GameState) =
    match limit with
    | FixedNodes n -> Game.GoCommand.NodeCount n
    | ByClock overhead -> clockSearch overhead s.WTime s.BTime s.WInc

/// The search to ponder with after our move, played `elapsedMs` after the state `s` came: our clock
/// less that time plus the increment, the opponent's as it was. None with a fixed number of nodes.
let ponderSearch (limit: SearchLimit) (s: GameState) (botIsWhite: bool) (elapsedMs: int64) =
    match limit with
    | FixedNodes _ -> None
    | ByClock overhead ->
        let inc = if botIsWhite then s.WInc else s.BInc
        let ours (t: int64) = max 0L (t - elapsedMs) + inc
        let w, b = if botIsWhite then ours s.WTime, s.BTime else s.WTime, ours s.BTime
        Some (clockSearch overhead w b inc)

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// The position command (Lichess's king-takes-rook castles mapped for standard chess) and whether it is
/// the bot's move; None when a move does not play.
let position (game: GameFull) (s: GameState) (botIsWhite: bool) : (string * bool) option =
    let start = if game.InitialFen = "startpos" || String.IsNullOrWhiteSpace game.InitialFen then startFen else game.InitialFen
    let board = Board()
    board.ResetBoardState()
    board.LoadFen start
    // a Chess960 game is Chess960 whatever its start (one may look like the standard one): it castles as
    // the king taking its rook, as Lichess sends and wants it, and the engine plays it with UCI_Chess960
    if game.Variant = "chess960" then board.IsFRC <- true
    let standard = not board.IsFRC
    let moves = ResizeArray<string>()
    let mutable ok = true
    for m in s.Moves do
        if ok then
            let fen = board.FEN()
            let uci =
                if standard then
                    match m with
                    | "e1h1" when PolyglotFormat.pieceAt fen "e1" = 'K' -> "e1g1"
                    | "e1a1" when PolyglotFormat.pieceAt fen "e1" = 'K' -> "e1c1"
                    | "e8h8" when PolyglotFormat.pieceAt fen "e8" = 'k' -> "e8g8"
                    | "e8a8" when PolyglotFormat.pieceAt fen "e8" = 'k' -> "e8c8"
                    | other -> other
                else m
            match BoardUtils.tryGetMoveAndSanFromUci &board uci with
            | Some _ ->
                board.PlayUciMove uci
                moves.Add uci
            | None -> ok <- false
    if not ok then None
    else
        // the standard start as startpos, as every UCI engine knows it
        let from = if start = startFen then "position startpos" else sprintf "position fen %s" start
        let command = if moves.Count = 0 then from else sprintf "%s moves %s" from (String.concat " " moves)
        Some (command, (board.Position.STM = 0uy) = botIsWhite)

/// The last move of a position command; None without moves.
let lastMove (command: string) =
    match command.IndexOf " moves " with
    | -1 -> None
    | i -> command.Substring(i + 7).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.tryLast

/// A position command with more moves after it.
let withMoves (command: string) (moves: string list) =
    if moves.IsEmpty then command
    elif command.Contains " moves " then command + " " + String.concat " " moves
    else command + " moves " + String.concat " " moves

/// The board of a position command (startpos or fen, then the moves), a Chess960 board in a Chess960
/// game (castles as the king taking its rook); None when a move does not play.
let boardOf (chess960: bool) (command: string) =
    let b = Board()
    b.ResetBoardState()
    let start, moves =
        match command.IndexOf " moves " with
        | -1 -> command, [||]
        | i -> command.Substring(0, i), command.Substring(i + 7).Split(' ', StringSplitOptions.RemoveEmptyEntries)
    if start.StartsWith "position fen " then b.LoadFen(start.Substring 13) else b.LoadFen startFen
    if chess960 then b.IsFRC <- true
    let mutable ok = true
    for m in moves do
        if ok then
            match BoardUtils.tryGetMoveAndSanFromUci &b m with
            | Some _ -> b.PlayUciMove m
            | None -> ok <- false
    if ok then Some b else None

/// The position to ponder on: ours, our move and the reply the engine expects. None when the reply
/// does not play or ends the game (no ponder then, as in tournaments).
let ponderPosition (chess960: bool) (command: string) (ours: string) (reply: string) =
    let after = withMoves command [ ours; reply ]
    match boardOf chess960 after with
    | Some b when b.AnyLegalMove() && not (b.InsufficientMaterial()) -> Some after
    | _ -> None

/// Whether the game has ended (any status but created and started).
let isOver (s: GameState) = s.Status <> "started" && s.Status <> "created" && s.Status <> ""

// ---------------------------------------------------------------- opening book

/// Whether the book may give the move at this ply: a standard game from the start, before `depth` plies.
let bookAllowed (game: GameFull) (ply: int) (depth: int) =
    game.Variant = "standard" && game.InitialFen = "startpos" && ply < depth

/// A book move drawn at random by the book's weights; None when the book has none.
let pickBookMove (moves: PolyglotLines.BookMove[]) (rng: Random) : string option =
    let total = moves |> Array.sumBy (fun m -> m.Weight)
    if total <= 0 then None
    else
        let mutable left = rng.Next total
        moves |> Array.tryFind (fun m -> left <- left - m.Weight; left < 0) |> Option.map (fun m -> m.Uci)

// ---------------------------------------------------------------- chat

/// What the bot says about itself in the chat.
type ChatInfo =
    { EngineName: string
      Net: string                // "" when the engine has no net
      Limit: SearchLimit
      /// Games the bot plays now, and how many it takes at once
      Playing: int
      MaxGames: int }

/// The bot's answer to a chat line, None when it is not a command. `eval`: the engine's last evaluation
/// as text, `top`: the net's top moves as text - both only for the spectator room.
let chatAnswer (info: ChatInfo) (room: string) (text: string) (eval: string option) (top: string option) : string option =
    let spectators = room = "spectator"
    let secret = "I don't tell my opponent, sorry."
    match text.Trim().ToLowerInvariant().Split(' ') |> Array.head with
    | "!commands" | "!help" -> Some "Commands: !name !net !eval !top !howto !queue (!eval and !top for spectators)"
    | "!name" -> Some (sprintf "%s, played by EngineBattle" info.EngineName)
    | "!net" ->
        let limit = match info.Limit with FixedNodes 1 -> ", 1 node, no search" | FixedNodes n -> sprintf ", %d nodes per move" n | ByClock _ -> ""
        Some ((if info.Net = "" then "No net: " + info.EngineName else "Net: " + info.Net) + limit)
    | "!eval" -> Some (if not spectators then secret else defaultArg eval "No evaluation yet.")
    | "!top" -> Some (if not spectators then secret else defaultArg top "No move statistics yet.")
    | "!howto" -> Some "This bot runs on EngineBattle (github.com/lepned/EngineBattle): an engine def and a Lichess bot account."
    | "!queue" ->
        // a challenge the bot cannot take now is declined with "later", not kept waiting
        Some (if info.Playing < info.MaxGames then "Free: challenge me." else sprintf "Busy with %d game(s): try again after." info.Playing)
    | _ -> None
