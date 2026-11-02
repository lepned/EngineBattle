/// An opening repertoire for one side, from a position: that side plays one move (the objectively best
/// when the position is clear, an idea when it is level), every sensible reply of the opponent is
/// answered, and the weaker replies are punished. Written as one PGN game with variations, evals and
/// lines in the comments. The engines sit behind search agents that own their caches.
module ChessLibrary.OpeningRepertoire

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open ChessLibrary.MiscTypes

/// One line of a search: UCI moves and the eval from White's view; with a policy search (one node, the
/// net's move stats) the first move's share of the policy, 0..1, and the position's eval.
type Line = { Pv: string list; Eval: EvalType; Policy: float option }

type Settings =
    { /// The side the repertoire is for.
      White: bool
      /// Plies from the start position to explore.
      Plies: int
      /// Opponent replies looked at per position (the engine's best and the book's), at most.
      Consider: int
      /// Replies that lose more than this many pawns against the opponent's best are left out.
      Catastrophe: float
      /// Replies explored further per position, at most; the other ones considered get our answer only.
      Replies: int
      /// A reply within this many pawns of the opponent's best is answered.
      Window: float
      /// Moves of ours looked at in a level position (the ideas, MultiPV).
      Ideas: int
      /// Below this many pawns either way a position is level: ideas instead of the best move.
      Equal: float
      /// An idea may cost this many pawns against the objective best, no more.
      Margin: float
      /// An idea after which the opponent must find this many only moves in a row may cost up to Risk:
      /// hard to meet over the board. 0 turns it off.
      Traps: int
      Risk: float
      /// An opponent's best move this many pawns ahead of the second is an only move: the line runs longer.
      OnlyMove: float
      /// Plies a line of only moves may run past Plies.
      MaxExtension: int
      /// From this many pawns either way the position is decided: the line ends with the engine's PV.
      Decided: float
      /// Plies of PV shown after a punishing move, a decided position or an idea.
      PvPlies: int
      /// Book moves played at least this share (percent) are answered too, however bad.
      BookShare: float
      /// Replies a human model (--human) plays at least this share (percent) are answered too, however
      /// bad: those are the mistakes worth knowing the answer to.
      HumanShare: float
      /// Only the mainline and the branches that hold a critical move.
      CriticalOnly: bool }

    static member Defaults =
        { White = true
          Plies = 10
          Consider = 5
          Catastrophe = 2.0
          Replies = 3
          Window = 0.5
          Ideas = 3
          Equal = 0.3
          Margin = 0.3
          Traps = 2
          Risk = 1.0
          OnlyMove = 0.7
          MaxExtension = 6
          Decided = 2.0
          PvPlies = 8
          BookShare = 10.0
          HumanShare = 10.0
          CriticalOnly = false }

/// A move of the tree; Next holds the moves after it, the mainline first.
type Node =
    { Uci: string
      San: string
      /// "", "!", "?!", "?", "??".
      Nag: string
      Comment: string
      /// An opponent mistake (? or ??), an only move and the move that loses beside it, or a trap of ours:
      /// what --keep critical keeps.
      Critical: bool
      /// The position this move reaches was explored in another line (its key): the line ends here.
      Transposes: string
      Next: Node list }

// ---------------------------------------------------------------- search agents

let private positionKey (fen: string) = fen.Split(' ') |> Array.truncate 4 |> String.concat " "

let private evalCode (e: EvalType) =
    match e with
    | CP p -> "cp:" + p.ToString("0.00", CultureInfo.InvariantCulture)
    | Mate m -> "m:" + string m
    | NA -> "na"

let private parseEval (s: string) =
    if s.StartsWith "cp:" then CP(Double.Parse(s.Substring 3, CultureInfo.InvariantCulture))
    elif s.StartsWith "m:" then Mate(int (s.Substring 2))
    else NA

/// A cache line: key, then eval|pv (|p:policy) per line, tab-separated.
let private cacheLine (key: string) (lines: Line list) =
    let policy (l: Line) = match l.Policy with Some p -> "|p:" + p.ToString("0.0000", CultureInfo.InvariantCulture) | None -> ""
    key + "\t" + (lines |> List.map (fun l -> evalCode l.Eval + "|" + String.concat " " l.Pv + policy l) |> String.concat "\t")

let private parseCacheLine (text: string) =
    let parts = text.Split '\t'
    if parts.Length < 1 || parts.[0] = "" then None
    else
        let lines =
            parts
            |> Array.skip 1
            |> Array.choose (fun p ->
                match p.IndexOf '|' with
                | i when i > 0 ->
                    let rest = p.Substring(i + 1)
                    let pv, policy =
                        match rest.IndexOf "|p:" with
                        | -1 -> rest, None
                        | j -> rest.Substring(0, j), Some(Double.Parse(rest.Substring(j + 3), CultureInfo.InvariantCulture))
                    Some { Eval = parseEval (p.Substring(0, i)); Pv = pv.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray; Policy = policy }
                | _ -> None)
            |> List.ofArray
        Some(parts.[0], lines)

type private SearchMsg =
    | Search of fen: string * moves: string list * multiPv: int * AsyncReplyChannel<Result<Line list, exn>>
    | Count of AsyncReplyChannel<int * int>
    | Close of AsyncReplyChannel<unit>

/// One engine behind an agent: its searches run one at a time, each result is kept by position and
/// appended to the cache file, so a run started again skips what is done. A file written with another
/// engine or limit (its first line, `signature`) is started afresh; a search with more lines (MultiPV)
/// answers one with fewer. `search` gets a FEN and the moves played from it (a net like Maia reads the
/// history); the cache keys the position they reach. `onSearch` hears of each new search.
type SearchAgent(name: string, signature: string, cachePath: string option, search: string -> string list -> int -> Line list, onSearch: string -> unit) =
    let loaded =
        match cachePath with
        | Some path when File.Exists path ->
            let all = File.ReadAllLines path
            if all.Length > 0 && all.[0] = signature then
                // per position the search with the most lines: it answers every smaller MultiPV too
                all
                |> Array.skip 1
                |> Array.choose parseCacheLine
                |> Array.choose (fun (key, lines) ->
                    match key.LastIndexOf '|' with
                    | i when i > 0 -> Some(key.Substring(0, i), (int (key.Substring(i + 1)), lines))
                    | _ -> None)
                |> Array.fold (fun (m: Map<string, int * Line list>) (pos, (k, lines)) ->
                    match m.TryFind pos with
                    | Some(k0, _) when k0 >= k -> m
                    | _ -> m.Add(pos, (k, lines))) Map.empty
            else Map.empty
        | _ -> Map.empty
    let writer =
        cachePath
        |> Option.map (fun path ->
            let w = new StreamWriter(path, false, UTF8Encoding false)
            w.AutoFlush <- true
            w.WriteLine signature
            for KeyValue(pos, (k, v)) in loaded do
                w.WriteLine(cacheLine (pos + "|" + string k) v)
            w)
    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (cache: Map<string, int * Line list>) (hits: int) (searches: int) =
                async {
                    match! inbox.Receive() with
                    | Search(fen, moves, k, reply) ->
                        let pos = moves |> List.fold (fun f m -> (BoardUtils.tryMakeMove f m).Value) fen |> positionKey
                        match cache.TryFind pos with
                        | Some(k0, lines) when k0 >= k ->
                            reply.Reply(Ok(List.truncate k lines))
                            return! loop cache (hits + 1) searches
                        | _ ->
                            match (try Ok(search fen moves k) with e -> Error e) with
                            | Ok lines ->
                                writer |> Option.iter (fun w -> w.WriteLine(cacheLine (pos + "|" + string k) lines))
                                onSearch name
                                reply.Reply(Ok lines)
                                return! loop (cache.Add(pos, (k, lines))) hits (searches + 1)
                            | Error e ->
                                reply.Reply(Error e)
                                return! loop cache hits searches
                    | Count reply ->
                        reply.Reply((hits, searches))
                        return! loop cache hits searches
                    | Close reply ->
                        writer |> Option.iter (fun w -> w.Dispose())
                        reply.Reply()
                }
            loop loaded 0 0)

    member _.Name = name
    /// The position's lines, best first; [] when it has no legal move.
    member x.Search(fen: string, multiPv: int) = x.SearchFrom(fen, [], multiPv)
    /// The lines of the position after `moves` from `fen`, the engine given the moves too.
    member _.SearchFrom(fen: string, moves: string list, multiPv: int) =
        async {
            match! agent.PostAndAsyncReply(fun r -> Search(fen, moves, multiPv, r)) with
            | Ok lines -> return lines
            | Error e -> return raise e
        }
    /// Answers from the cache and new searches.
    member _.Counts() = agent.PostAndReply Count
    /// Positions in the cache file when the agent started.
    member _.Loaded = loaded.Count
    member _.Close() = agent.PostAndReply Close

// ---------------------------------------------------------------- evals and moves

let private pawns (e: EvalType) =
    match e with
    | CP p -> p
    | Mate m -> if m > 0 then 100.0 else -100.0
    | NA -> 0.0

/// Lichess's winning chances, -1..1, from pawns.
let winChance (p: float) =
    let cp = Math.Clamp(p * 100.0, -1000.0, 1000.0)
    2.0 / (1.0 + Math.Exp(-0.00368208 * cp)) - 1.0

/// The NAG of a move that gives away this much winning chance (lichess's inaccuracy/mistake/blunder).
let nagOf (drop: float) =
    if drop >= 0.3 then "??" elif drop >= 0.2 then "?" elif drop >= 0.1 then "?!" else ""

/// Pawns to two decimals; what rounds to zero is 0.00, never -0.00.
let private twoDecimals (p: float) = Math.Round(p, 2) + 0.0

let private evalTag (e: EvalType) =
    match e with
    | CP p -> "[%eval " + (twoDecimals p).ToString("0.00", CultureInfo.InvariantCulture) + "]"
    | Mate m -> $"[%%eval #{m}]"
    | NA -> ""

let private evalText (e: EvalType) =
    match e with
    | CP p ->
        let r = twoDecimals p
        (if r >= 0.0 then "+" else "") + r.ToString("0.00", CultureInfo.InvariantCulture)
    | Mate m -> $"#{m}"
    | NA -> "?"

let private after (fen: string) (uci: string) = (BoardUtils.tryMakeMove fen uci).Value

let private sanOf (fen: string) (uci: string) =
    let board = Chess.Board()
    board.LoadFen fen
    match BoardUtils.tryGetMoveAndSanFromUci &board uci with
    | Some(_, san) -> san
    | None -> uci

/// Move number and whether White moves, from a FEN.
let private turn (fen: string) =
    let f = fen.Split ' '
    int f.[5], f.[1] = "w"

/// "5.Nf3 Bg4 6.Be2" or "5...Bg4 6.Be2" from a FEN and UCI moves.
let private numbered (fen: string) (pv: string list) =
    let sb = StringBuilder()
    let mutable f = fen
    let mutable first = true
    for m in pv do
        match BoardUtils.tryMakeMove f m with
        | Some next ->
            let no, white = turn f
            if sb.Length > 0 then sb.Append ' ' |> ignore
            if white then sb.Append(no).Append('.') |> ignore
            elif first then sb.Append(no).Append("...") |> ignore
            sb.Append(sanOf f m) |> ignore
            first <- false
            f <- next
        | None -> ()
    sb.ToString()

let private node fen uci nag comment next =
    { Uci = uci; San = sanOf fen uci; Nag = nag; Comment = comment; Critical = false; Transposes = ""; Next = next }

let private join (parts: string list) = parts |> List.filter (fun s -> s <> "") |> String.concat " "

// ---------------------------------------------------------------- the tree

type private Ctx =
    { S: Settings
      Judge: SearchAgent
      Chooser: SearchAgent option
      /// The book's moves of a position: UCI and share in percent.
      Book: string -> (string * float) list
      /// A net that plays like people (Maia): its policy is how often each move is played.
      Human: SearchAgent option
      /// MultiPV of the judge: enough for the replies considered (two at least, to see an only move) and the ideas.
      K: int
      /// The tree's first position, and the FEN and moves before it (the human net reads the history).
      Start: string
      Root: string
      Prefix: string list
      /// Each position explored, by key: the line (UCI from Start) that reached it and the plies it had left.
      Visited: Dictionary<string, string list * int>
      /// What follows each explored position, for --keep critical.
      Expanded: Dictionary<string, Node list> }

let private ours (c: Ctx) (e: EvalType) = if c.S.White then pawns e else -(pawns e)

/// Where a position was explored before, at least as deep as `plies` would: that line from where it
/// parts from `path` (reversed UCI from Start), "4.Nf3 Nf6 5.Bc4".
let private transposition (c: Ctx) (path: string list) (plies: int) (fen: string) =
    match c.Visited.TryGetValue(positionKey fen) with
    | true, (first, left) when left >= plies ->
        let current = List.rev path
        let common = Seq.zip first current |> Seq.takeWhile (fun (a, b) -> a = b) |> Seq.length
        let from = first |> List.truncate common |> List.fold after c.Start
        Some(numbered from (List.skip common first))
    | _ -> None

let private visit (c: Ctx) (path: string list) (plies: int) (fen: string) = c.Visited.[positionKey fen] <- (List.rev path, plies)

/// The opponent's best is an only move: its second best is OnlyMove worse for it and leaves us
/// OnlyMove ahead. A best move that only wins more (a refutation of ours) is not one.
let private onlyMove (c: Ctx) (best: Line) (second: Line) =
    ours c second.Eval - ours c best.Eval >= c.S.OnlyMove && ours c second.Eval >= c.S.OnlyMove

/// The judge's lines with a move, or what the position after a move it did not list is worth.
let private judgeMove (c: Ctx) (fen: string) (lines: Line list) (uci: string) =
    async {
        match lines |> List.tryFind (fun l -> l.Pv.Head = uci) with
        | Some l -> return Some l
        | None ->
            match! c.Judge.Search(after fen uci, c.K) with
            | best :: _ -> return Some { Pv = uci :: best.Pv; Eval = best.Eval; Policy = None }
            | [] -> return None
    }

/// A piece (not a pawn) of either side hangs: a static exchange on its square wins it.
let private pieceHangs (fen: string) =
    let insights = BoardUtils.getPositionInsights fen
    let placement = fen.Split(' ').[0].Split('/')
    let pieceAt (square: string) =
        let rank = placement.[8 - int (square.[1] - '0')]
        let mutable file = 0
        let mutable found = ' '
        for ch in rank do
            if Char.IsDigit ch then file <- file + int (ch - '0')
            else
                if file = int (square.[0] - 'a') then found <- ch
                file <- file + 1
        found
    Array.append insights.White.HangingPieces insights.Black.HangingPieces
    |> Array.exists (fun h -> Char.ToLower(pieceAt h.Square) <> 'p')

/// The first `n` moves of a line as a chain of nodes, the first with a NAG and comment. A line never
/// stops mid-exchange or under a threat: past `n` it goes on while the last move captured or checked,
/// the next one captures or a piece hangs, searching on where the line runs out, at most 12 plies more.
let private pvChain (c: Ctx) (fen: string) (pv: string list) (n: int) (nag: string) (comment: string) : Async<Node list> =
    let forcing (san: string) = san.Contains 'x' || san.EndsWith "+"
    let rec go (f: string) (moves: string list) (i: int) (lastSan: string) =
        async {
            // asked only where the line could end: at its length, or where its moves run out
            let pending =
                (i >= n || moves.IsEmpty)
                && (forcing lastSan
                    || (match moves with m :: _ -> (sanOf f m).Contains 'x' | [] -> false)
                    || pieceHangs f)
            if i >= n + 12 || (i >= n && not pending) then return []
            else
                match moves with
                | [] when i > 0 && pending ->
                    match! c.Judge.Search(f, c.K) with
                    | l :: _ when not l.Pv.IsEmpty -> return! go f l.Pv i lastSan
                    | _ -> return []
                | [] -> return []
                | m :: rest ->
                    match BoardUtils.tryMakeMove f m with
                    | Some next ->
                        let san = sanOf f m
                        let! tail = go next rest (i + 1) san
                        return [ { Uci = m; San = san; Nag = ""; Comment = ""; Critical = false; Transposes = ""; Next = tail } ]
                    | None -> return []
        }
    async {
        match! go fen pv 0 "" with
        | head :: _ -> return [ { head with Nag = nag; Comment = comment } ]
        | [] -> return []
    }

/// The square our move captured on, where taking back is no feat: "d5" for a capture there.
let private takenOn (fen: string) (uci: string) = if (sanOf fen uci).Contains 'x' then Some(uci.Substring(2, 2)) else None

/// Taking back on the square we just captured on.
let private recapture (taken: string option) (uci: string) = taken = Some(uci.Substring(2, 2))

/// The opponent's only moves in a row from `fen` (its move, after ours captured on `taken`): along the
/// judge's line, each move whose second best is OnlyMove worse, until one is not; as "5.Nf3!". A
/// recapture is not counted (anyone finds it), the line goes on through it.
let rec private onlyMoves (c: Ctx) (fen: string) (left: int) (taken: string option) : Async<string list> =
    async {
        if left <= 0 then return []
        else
            let! lines = c.Judge.Search(fen, c.K)
            match lines |> List.filter (fun l -> not l.Pv.IsEmpty) with
            | best :: second :: _ when onlyMove c best second ->
                let m = best.Pv.Head
                let f = after fen m
                let found = if recapture taken m then [] else [ numbered fen [ m ] + "!" ]
                match! c.Judge.Search(f, c.K) with
                | reply :: _ when not reply.Pv.IsEmpty ->
                    let r = reply.Pv.Head
                    let! rest = onlyMoves c (after f r) (left - 1) (takenOn f r)
                    return found @ rest
                | _ -> return found
            | _ -> return []
    }

/// Our move: the best when the position is clear, an idea that is sound when it is level; the line
/// played out when it only answers a reply (`answer`, the NAG of our move) or the position is decided.
let rec private ourMove (c: Ctx) (fen: string) (plies: int) (ext: int) (answer: string option) (path: string list) : Async<Node list> =
    async {
        let! lines = c.Judge.Search(fen, c.K)
        let lines = lines |> List.filter (fun l -> not l.Pv.IsEmpty)
        match lines with
        | [] -> return []
        | best :: _ ->
            let objective = ours c best.Eval
            if answer.IsSome || abs objective >= c.S.Decided then
                return! pvChain c fen best.Pv c.S.PvPlies (defaultArg answer "") (evalTag best.Eval)
            else
                let! picked, ideas =
                    if abs objective >= c.S.Equal then async { return (best, []), [] }
                    else
                        async {
                            let! candidates =
                                match c.Chooser with
                                | Some chooser -> chooser.Search(fen, c.S.Ideas)
                                | None -> async { return lines |> List.truncate c.S.Ideas }
                            let! judged =
                                candidates
                                |> List.filter (fun l -> not l.Pv.IsEmpty)
                                |> List.map (fun l -> judgeMove c fen lines l.Pv.Head)
                                |> Async.Sequential
                            // sound ideas, and riskier ones the opponent must meet with only moves; those first
                            let! weighed =
                                judged
                                |> Array.choose id
                                |> Array.filter (fun j -> objective - ours c j.Eval <= max c.S.Margin c.S.Risk)
                                |> Array.map (fun j ->
                                    async {
                                        let! only = if c.S.Traps > 0 then onlyMoves c (after fen j.Pv.Head) 6 (takenOn fen j.Pv.Head) else async { return [] }
                                        return j, only
                                    })
                                |> Async.Sequential
                            let trap (only: string list) = c.S.Traps > 0 && only.Length >= c.S.Traps
                            let accepted =
                                weighed
                                |> Array.filter (fun (j, only) -> objective - ours c j.Eval <= c.S.Margin || trap only)
                                |> Array.sortBy (fun (_, only) -> if trap only then -only.Length else 0)
                                |> List.ofArray
                            match accepted with
                            | p :: rest -> return p, rest
                            | [] -> return (best, []), []
                        }
                let picked, only = picked
                let move = picked.Pv.Head
                let next = after fen move
                let path' = move :: path
                let transposed = transposition c path' (plies - 1) next
                let! replies =
                    match transposed with
                    | Some _ -> async { return [] }
                    | None ->
                        async {
                            visit c path' (plies - 1) next
                            return! oppMove c next (plies - 1) ext path'
                        }
                let opponent = if c.S.White then "Black" else "White"
                let must (only: string list) = if only.IsEmpty then "" else $"""{opponent} must find {String.concat " " only}."""
                let trap (only: string list) = c.S.Traps > 0 && only.Length >= c.S.Traps
                let note =
                    join
                        [ (if move <> best.Pv.Head then $"Idea; objective best {numbered fen [ best.Pv.Head ]} ({evalText best.Eval})." else "")
                          must only
                          (match transposed with Some t -> $"Transposes to {t}." | None -> "") ]
                // a line that ends here goes on with the engine's moves, playable like the rest
                let! next' = if replies.IsEmpty && transposed.IsNone then pvChain c next picked.Pv.Tail (c.S.PvPlies - 1) "" "" else async { return replies }
                if transposed.IsNone then c.Expanded.[positionKey next] <- next'
                let main =
                    { node fen move "" (join [ evalTag picked.Eval; note ]) next' with
                        Critical = trap only
                        Transposes = (if transposed.IsSome then positionKey next else "") }
                // the objective best is always there to play when the idea is another move
                let ideas =
                    if move <> best.Pv.Head && not (ideas |> List.exists (fun (j, _) -> j.Pv.Head = best.Pv.Head)) then (best, []) :: ideas
                    else ideas
                let! alternatives =
                    ideas
                    |> List.filter (fun (j, _) -> j.Pv.Head <> move)
                    |> List.sortBy (fun (j, _) -> if j.Pv.Head = best.Pv.Head then 0 else 1)
                    |> List.truncate (max 1 (c.S.Ideas - 1))
                    |> List.map (fun (j, only) ->
                        let objectiveBest = j.Pv.Head = best.Pv.Head
                        let label = if objectiveBest then "Objective best." else "Also good."
                        async {
                            let! chain = pvChain c fen j.Pv c.S.PvPlies "" (join [ evalTag j.Eval; label; must only ])
                            // beside a trap the objective best stays: its comment names it
                            return chain |> List.map (fun n -> { n with Critical = trap only || (objectiveBest && main.Critical) })
                        })
                    |> Async.Sequential
                return main :: (alternatives |> List.concat)
    }

/// The opponent's replies: the engine's best Consider and the book's, but none that loses more than
/// Catastrophe; ordered best first. Those near the best (at most Replies) are explored further, the
/// rest get our answer and its line, punished where they give winning chances away. An only move runs
/// the line longer.
and private oppMove (c: Ctx) (fen: string) (plies: int) (ext: int) (path: string list) : Async<Node list> =
    async {
        if plies <= 0 then return []
        else
            let! lines = c.Judge.Search(fen, c.K)
            let lines = lines |> List.filter (fun l -> not l.Pv.IsEmpty)
            match lines with
            | [] -> return []
            | best :: _ when abs (ours c best.Eval) >= c.S.Decided -> return []
            | best :: rest ->
                // taking back what we just took is no only move, however forced
                let taken =
                    match path with
                    | m :: earlier -> takenOn (List.rev earlier |> List.fold after c.Start) m
                    | [] -> None
                let critical =
                    match rest with
                    | second :: _ -> onlyMove c best second && not (recapture taken best.Pv.Head)
                    | [] -> false
                let engineMoves = lines |> List.truncate c.S.Consider |> List.map (fun l -> l.Pv.Head)
                let bookMoves =
                    c.Book fen
                    |> List.filter (fun (m, share) -> share >= c.S.BookShare && not (List.contains m engineMoves))
                    |> List.map fst
                let shares = dict (c.Book fen)
                let! humanLines =
                    match c.Human with
                    | Some h -> h.SearchFrom(c.Root, c.Prefix @ List.rev path, 256)
                    | None -> async { return [] }
                let humanShares =
                    humanLines
                    |> List.choose (fun l -> match l.Pv, l.Policy with m :: _, Some p -> Some(m, 100.0 * p) | _ -> None)
                    |> dict
                let humanShare m = match humanShares.TryGetValue m with | true, share -> share | _ -> 0.0
                let popular m = humanShare m >= c.S.HumanShare
                let humanMoves =
                    humanShares.Keys
                    |> Seq.filter (fun m -> popular m && not (List.contains m engineMoves) && not (List.contains m bookMoves))
                    |> List.ofSeq
                let candidates = engineMoves @ bookMoves @ humanMoves
                // what each reply is worth: the search our answer starts from (cached for it)
                let! evaluated =
                    candidates
                    |> List.map (fun m ->
                        async {
                            let f = after fen m
                            let! answers = c.Judge.Search(f, c.K)
                            let eval =
                                match answers with
                                | a :: _ -> a.Eval
                                | [] -> if (BoardUtils.getPositionStatus f).Status = "checkmate" then Mate(if c.S.White then -1 else 1) else CP 0.0
                            return m, f, eval
                        })
                    |> Async.Sequential
                let sorted = evaluated |> Array.sortBy (fun (_, _, e) -> ours c e) |> List.ofArray
                let _, _, mainEval = sorted.Head
                // a move people often play stays however bad: its punishment is what to know
                let sorted = sorted |> List.filter (fun (m, _, e) -> ours c e - ours c mainEval <= c.S.Catastrophe || popular m)
                let mainMove, _, _ = sorted.Head
                let second = sorted |> List.tryItem 1
                let drop e = winChance (ours c e) - winChance (ours c mainEval)
                // explored beside the best: the replies near it, the most played first
                let explored =
                    sorted
                    |> List.tail
                    |> List.filter (fun (_, _, e) -> drop e < 0.1 && ours c e - ours c mainEval <= c.S.Window)
                    |> List.sortByDescending (fun (m, _, _) -> humanShare m)
                    |> List.truncate (c.S.Replies - 1)
                    |> List.map (fun (m, _, _) -> m)
                    |> set
                let! nodes =
                    sorted
                    |> List.mapi (fun i (m, f, e) ->
                        async {
                            let drop = drop e
                            let punished = i > 0 && drop >= 0.1
                            let explored = i = 0 || explored.Contains m
                            let onlyMove = i = 0 && critical && second.IsSome
                            let plies', ext' = if onlyMove && ext < c.S.MaxExtension then plies + 1, ext + 1 else plies - 1, ext
                            let path' = m :: path
                            let transposed = transposition c path' plies' f
                            let! answer =
                                match transposed with
                                | Some _ -> async { return [] }
                                | None ->
                                    async {
                                        if explored then visit c path' plies' f
                                        let! answer = ourMove c f plies' ext' (if explored then None elif punished then Some "!" else Some "") path'
                                        if explored then c.Expanded.[positionKey f] <- answer
                                        return answer
                                    }
                            let note =
                                if onlyMove then
                                    let m2, _, e2 = second.Value
                                    $"Only move; {numbered fen [ m2 ]} {evalText e2}."
                                elif punished then $"{numbered fen [ mainMove ]} is better ({evalText mainEval})."
                                else ""
                            let book =
                                match shares.TryGetValue m with
                                | true, share -> $"Book {share:F0}%%."
                                | _ -> ""
                            let people =
                                match c.Human with
                                | Some h when humanShare m >= 0.5 -> $"{h.Name} {humanShare m:F0}%%."
                                | _ -> ""
                            let nag = if onlyMove then "!" elif i > 0 then nagOf drop else ""
                            let isCritical = onlyMove || nag = "?" || nag = "??" || (i = 1 && critical && second.IsSome)
                            let transposes = match transposed with Some t -> $"Transposes to {t}." | None -> ""
                            return
                                { node fen m nag (join [ evalTag e; note; book; people; transposes ]) answer with
                                    Critical = isCritical
                                    Transposes = (if transposed.IsSome then positionKey f else "") }
                        })
                    |> Async.Sequential
                return List.ofArray nodes
    }

// ---------------------------------------------------------------- PGN

let private nagCode (nag: string) =
    match nag with
    | "!" -> "$1"
    | "?" -> "$2"
    | "!!" -> "$3"
    | "??" -> "$4"
    | "!?" -> "$5"
    | "?!" -> "$6"
    | _ -> ""

/// The moves as PGN movetext with nested variations, wrapped at 80 columns.
let movetext (fen: string) (nodes: Node list) =
    let tokens = ResizeArray<string>()
    let rec move (n: Node) (f: string) (force: bool) =
        let no, white = turn f
        tokens.Add((if white then $"{no}. " elif force then $"{no}... " else "") + n.San)
        if n.Nag <> "" then tokens.Add(nagCode n.Nag)
        if n.Comment <> "" then tokens.Add("{" + n.Comment.Replace("}", ")") + "}")
    and line (alternatives: Node list) (f: string) (force: bool) =
        match alternatives with
        | [] -> ()
        | main :: others ->
            move main f force
            for o in others do
                tokens.Add "("
                move o f true
                line o.Next (after f o.Uci) (o.Comment <> "")
                tokens.Add ")"
            line main.Next (after f main.Uci) (not others.IsEmpty || main.Comment <> "")
    line nodes fen false
    tokens.Add "*"
    let sb = StringBuilder()
    let mutable col = 0
    let mutable glue = false
    for t in tokens do
        let words = t.Split ' '
        for i in 0 .. words.Length - 1 do
            let w = words.[i]
            let attach = glue || w = ")"
            if not attach && col > 0 && col + 1 + w.Length > 80 then
                sb.AppendLine() |> ignore
                col <- 0
            elif not attach && col > 0 then
                sb.Append ' ' |> ignore
                col <- col + 1
            sb.Append w |> ignore
            col <- col + w.Length
            glue <- w = "(" && i = words.Length - 1
    sb.AppendLine().ToString()

// ---------------------------------------------------------------- critical lines

/// The tree cut to its critical lines: the mainline, and each branch that holds a critical move. Where
/// a branch is kept, the position's own main move stays beside it (the comments name it), and below a
/// kept main or critical move its main continuation. A transposition to a line cut away plays that
/// line out where it stands (`expanded`: what follows each explored position).
let critical (expanded: IReadOnlyDictionary<string, Node list>) (nodes: Node list) =
    let rec holds (n: Node) = n.Critical || List.exists holds n.Next
    let rec prune (nodes: Node list) (main: bool) =
        let first = main || List.exists holds nodes
        nodes
        |> List.mapi (fun i n -> i, n)
        |> List.filter (fun (i, n) -> (first && i = 0) || holds n)
        |> List.map (fun (i, n) ->
            match expanded.TryGetValue n.Transposes with
            | true, target when not (List.exists holds target) ->
                // the line it names is cut away: played out here, the reference dropped
                let i = n.Comment.IndexOf "Transposes to "
                { n with Comment = (if i >= 0 then n.Comment.Substring(0, i).TrimEnd() else n.Comment); Next = prune target true }
            | _ -> { n with Next = prune n.Next ((first && i = 0) || n.Critical) })
    prune nodes true

// ---------------------------------------------------------------- the report

type Report =
    { Pgn: string
      /// Lines in the tree (its leaves).
      Lines: int
      /// The deepest line, in plies from the start position.
      Depth: int }

let rec private leaves (nodes: Node list) =
    match nodes with
    | [] -> 1
    | _ -> nodes |> List.sumBy (fun n -> leaves n.Next)

let rec private depth (nodes: Node list) =
    match nodes with
    | [] -> 0
    | _ -> 1 + (nodes |> List.map (fun n -> depth n.Next) |> List.max)

/// The repertoire from `fen` after `prefix` (UCI moves played first, written as the game's moves).
/// `book` gives a position's book moves with their share in percent, `human` the opponent's moves as
/// people play them (a policy search); `title` heads the first comment.
let build (settings: Settings) (judge: SearchAgent) (chooser: SearchAgent option) (human: SearchAgent option) (book: string -> (string * float) list) (fen: string) (prefix: string list) (title: string) =
    async {
        let start = prefix |> List.fold after fen
        let c =
            { S = settings
              Judge = judge
              Chooser = chooser
              Book = book
              Human = human
              K = max (max settings.Consider 2) settings.Ideas
              Start = start
              Root = fen
              Prefix = prefix
              Visited = Dictionary()
              Expanded = Dictionary() }
        visit c [] settings.Plies start
        let _, whiteMoves = turn start
        let! tree = if whiteMoves = settings.White then ourMove c start settings.Plies 0 None [] else oppMove c start settings.Plies 0 []
        let tree = if settings.CriticalOnly then critical c.Expanded tree else tree
        // the prefix as the game's first moves, the tree after the last
        let positions = prefix |> List.scan after fen |> List.truncate prefix.Length
        let rooted = List.foldBack2 (fun (f: string) (m: string) (next: Node list) -> [ node f m "" "" next ]) positions prefix tree
        let header =
            [ "Event", title
              "Site", "EngineBattle"
              "Date", DateTime.Now.ToString("yyyy.MM.dd", CultureInfo.InvariantCulture)
              "Round", "-"
              "White", "?"
              "Black", "?"
              "Result", "*"
              "Annotator",
              judge.Name
              + (match chooser with Some ch -> $", ideas {ch.Name}" | None -> "")
              + (match human with Some h -> $", people {h.Name}" | None -> "") ]
            @ (if fen <> MiscTypes.startPosition then [ "SetUp", "1"; "FEN", fen ] else [])
        let sb = StringBuilder()
        for k, v in header do
            sb.Append('[').Append(k).Append(" \"").Append(v.Replace("\"", "'")).AppendLine("\"]") |> ignore
        sb.AppendLine() |> ignore
        sb.Append(movetext fen rooted) |> ignore
        return { Pgn = sb.ToString(); Lines = leaves tree; Depth = prefix.Length + depth tree }
    }

// ---------------------------------------------------------------- a book for a bot

type BotBookSettings =
    { /// Plies from the start position.
      Plies: int
      /// Our moves this many pawns from the best go into the book beside it (it varies its play).
      Within: float
      /// Our moves looked at per position (MultiPV), at most.
      Ideas: int
      /// The opponent's replies explored per position: the engine's best, those within Window of its best.
      Replies: int
      Window: float
      /// From this many pawns either way the book ends: the engine plays on.
      Decided: float
      /// White's first moves the book for Black answers (UCI); []: the replies of any position.
      First: string list
      /// A move from the games is answered when played this many times.
      MinGames: int
      /// From this ply on the tree is narrow: our best move only and the opponent's best reply (and the
      /// games' moves), so the lines run deep without branching.
      NarrowFrom: int
      /// With a policy search (lines that carry the policy): our moves with at least this share of the
      /// top move's policy go in beside it, instead of Within.
      PolicyWithin: float
      /// With a policy search: the opponent's replies with at least this policy (0..1), instead of Window.
      MinPolicy: float }

    static member Defaults =
        { Plies = 16
          Within = 0.10
          Ideas = 3
          Replies = 3
          Window = 0.5
          Decided = 2.0
          First = [ "e2e4"; "d2d4"; "c2c4"; "g1f3" ]
          MinGames = 1
          NarrowFrom = 8
          PolicyWithin = 0.6
          MinPolicy = 0.10 }

type BotBook =
    { /// Our moves for both sides as Polyglot entries, each weighing 1.
      Entries: PolyglotFormat.Entry[]
      /// The book for White and the book for Black as two PGN games with variations, to look through.
      Pgn: string }

/// A move's comment: its eval, or "P 34%" from a policy search (the eval there is the position's,
/// the same for every move).
let private lineNote (l: Line) =
    match l.Policy with
    | Some p -> "P " + (100.0 * p).ToString("0", CultureInfo.InvariantCulture) + "%"
    | None -> evalTag l.Eval

/// A castle in UCI as the standard board writes it: the king's two-square move.
let private castles (fen: string) (uci: string) =
    List.contains (uci.Substring(0, 4)) [ "e1g1"; "e1c1"; "e8g8"; "e8c8" ]
    && PolyglotFormat.pieceAt fen (uci.Substring(0, 2)) = (if uci.[1] = '1' then 'K' else 'k')

/// An opening book for a bot that plays both sides from the start position, searched by one engine: in
/// each position of ours the best move and those within Within of it; the opponent's replies are its
/// best ones within Window and every move the games (`played`, by Polyglot key: UCI and times played)
/// show at least MinGames times; from NarrowFrom on only the best of each. With a policy search (one
/// node: no eval per move) the policy decides instead: PolicyWithin of the top move for ours, MinPolicy
/// for the opponent's; the position's eval still ends a decided line. A position explored before with as many plies left is not searched
/// again; a decided position ends the line.
let botBook (s: BotBookSettings) (judge: SearchAgent) (played: uint64 -> (string * int) list) : Async<BotBook> =
    async {
        let entries = HashSet<struct (uint64 * uint16)>()
        let k = max 2 (max s.Ideas (s.Replies + 1))
        let tree (white: bool) =
            let ours (e: EvalType) = if white then pawns e else -(pawns e)
            let visited = Dictionary<string, int>()
            let fresh (fen: string) (plies: int) =
                match visited.TryGetValue(positionKey fen) with
                | true, left when left >= plies -> false
                | _ ->
                    visited.[positionKey fen] <- plies
                    true
            let rec ourMove (fen: string) (plies: int) : Async<Node list> =
                async {
                    if plies <= 0 || not (fresh fen plies) then return []
                    else
                        let! lines = judge.Search(fen, k)
                        match lines |> List.filter (fun l -> not l.Pv.IsEmpty) with
                        | [] -> return []
                        | best :: _ as lines ->
                            let decided = abs (ours best.Eval) >= s.Decided
                            let narrow = s.Plies - plies >= s.NarrowFrom
                            let near (l: Line) =
                                match l.Policy, best.Policy with
                                | Some p, Some top -> p >= top * s.PolicyWithin
                                | _ -> ours best.Eval - ours l.Eval <= s.Within
                            let picked =
                                if decided || narrow then [ best ]
                                else lines |> List.truncate s.Ideas |> List.filter near
                            let! nodes =
                                picked
                                |> List.map (fun l ->
                                    async {
                                        let m = l.Pv.Head
                                        entries.Add(struct (PolyglotFormat.keyOfFen fen, PolyglotFormat.encodeMove m (castles fen m))) |> ignore
                                        let! next = if decided then async { return [] } else oppMove (after fen m) (plies - 1) None
                                        return node fen m "" (lineNote l) next
                                    })
                                |> Async.Sequential
                            return List.ofArray nodes
                }
            and oppMove (fen: string) (plies: int) (first: string list option) : Async<Node list> =
                async {
                    if plies <= 0 || not (fresh fen plies) then return []
                    else
                        let! lines = judge.Search(fen, k)
                        match lines |> List.filter (fun l -> not l.Pv.IsEmpty) with
                        | [] -> return []
                        | best :: _ when first.IsNone && abs (ours best.Eval) >= s.Decided -> return []
                        | best :: _ as lines ->
                            let fromGames =
                                played (PolyglotFormat.keyOfFen fen)
                                |> List.filter (fun (m, n) -> n >= s.MinGames && (BoardUtils.tryMakeMove fen m).IsSome)
                            let moves =
                                match first with
                                | Some moves -> moves
                                | None ->
                                    let engine =
                                        lines
                                        |> List.truncate (if s.Plies - plies >= s.NarrowFrom then 1 else s.Replies)
                                        |> List.filter (fun l ->
                                            match l.Policy with
                                            | Some p -> p >= s.MinPolicy
                                            | None -> ours l.Eval - ours best.Eval <= s.Window)
                                        |> List.map (fun l -> l.Pv.Head)
                                    List.distinct (engine @ List.map fst fromGames)
                            let! nodes =
                                moves
                                |> List.map (fun m ->
                                    async {
                                        let! next = ourMove (after fen m) (plies - 1)
                                        let eval = lines |> List.tryFind (fun l -> l.Pv.Head = m) |> Option.map lineNote
                                        let times =
                                            match fromGames |> List.tryFind (fun (g, _) -> g = m) with
                                            | Some (_, n) -> $"Played {n}x."
                                            | None -> ""
                                        return node fen m "" (join [ defaultArg eval ""; times ]) next
                                    })
                                |> Async.Sequential
                            return List.ofArray nodes
                }
            if white then ourMove MiscTypes.startPosition s.Plies
            else oppMove MiscTypes.startPosition s.Plies (if s.First.IsEmpty then None else Some s.First)
        let! forWhite = tree true
        let! forBlack = tree false
        let game (title: string) (nodes: Node list) =
            let sb = StringBuilder()
            for key, value in [ "Event", title; "Site", "EngineBattle"; "Annotator", judge.Name; "Result", "*" ] do
                sb.Append('[').Append(key).Append(" \"").Append(value.Replace("\"", "'")).AppendLine("\"]") |> ignore
            sb.AppendLine().Append(movetext MiscTypes.startPosition nodes).ToString()
        let book =
            entries
            |> Seq.map (fun (struct (key, move)) -> ({ Key = key; Move = move; Weight = 1us } : PolyglotFormat.Entry))
            |> Array.ofSeq
        return { Entries = book; Pgn = game "Book for White" forWhite + Environment.NewLine + game "Book for Black" forBlack }
    }
