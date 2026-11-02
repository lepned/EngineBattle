/// Training a repertoire (a book repertoire PGN): the board plays the opponent's moves, chosen by how
/// often people play them, and the trainee finds the answers. A position missed comes back sooner
/// (Leitner boxes). A pure step: state + event -> state; the page shows the state and saves the
/// progress when it changed.
module ChessLibrary.RepertoireTrainer

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open ChessLibrary.PGNTypes

/// A move of the repertoire. Fen is the position it is played from; Eval is White's view in pawns.
type Node =
    { Uci: string
      San: string
      Fen: string
      Comment: string
      Eval: float option
      /// How often it is played, in percent (the human net's or the book's share).
      Share: float option
      /// An alternative of ours the repertoire accepts beside its move ("Also good.", "Objective best.").
      AlsoGood: bool
      /// An opponent move that gives winning chances away: "?" or "??".
      Mistake: string
      /// A mistake of the opponent somewhere below: what Traps mode trains.
      HasTrap: bool
      Next: Node list }

type Tree =
    { /// The side being trained.
      White: bool
      StartFen: string
      Root: Node list
      /// Positions where the trainee moves (keys), for the counts.
      OurPositions: int }

// ---------------------------------------------------------------- reading the PGN


let private evalRegex = Regex(@"\[%eval\s+(#?-?[0-9.]+)\]", RegexOptions.Compiled)
let private shareRegex = Regex(@"(\S+) (\d+(?:\.\d+)?)%\.", RegexOptions.Compiled)

let private evalOf (comment: string) =
    let m = evalRegex.Match comment
    if not m.Success then None
    else
        let v = m.Groups.[1].Value
        if v.StartsWith "#" then Some(if v.Contains "-" then -100.0 else 100.0)
        else
            match Double.TryParse(v, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, x -> Some x
            | _ -> None

let private shareOf (comment: string) =
    let found = shareRegex.Matches comment
    if found.Count = 0 then None
    else Some(Double.Parse(found.[found.Count - 1].Groups.[2].Value, CultureInfo.InvariantCulture))

/// The comment without the [%eval] tag, as the page shows it.
let text (comment: string) = evalRegex.Replace(comment, "").Trim()

/// A move of the engine's line after the repertoire's last choice: no comment, nor any move after it.
let rec isEngineLine (n: Node) = String.IsNullOrWhiteSpace n.Comment && List.forall isEngineLine n.Next

/// A line of PlyMoves (each with its variations, the alternatives to it) as nodes; the opponent's
/// alternatives are marked "?"/"??" by how much winning chance they give away against the first.
let rec private nodesOf (white: bool) (fen: string) (line: PlyLine) (from: int) : Node list =
    if from >= line.Count then []
    else
        let first = line.[from]
        let alternatives = Seq.append [ (first, line, from) ] (first.Variations |> Seq.filter (fun v -> v.Count > 0) |> Seq.map (fun v -> (v.[0], v, 0)))
        let ours = (fen.Split(' ').[1] = "w") = white
        let built =
            alternatives
            |> Seq.choose (fun (m, l, i) ->
                BoardUtils.tryParseSan fen m.San
                |> Option.map (fun uci ->
                    let next = (BoardUtils.tryMakeMove fen uci).Value
                    let children = nodesOf white next l (i + 1)
                    { Uci = uci
                      San = m.San
                      Fen = fen
                      Comment = m.Comment
                      Eval = evalOf m.Comment
                      Share = shareOf m.Comment
                      AlsoGood = ours && (m.Comment.Contains OpeningRepertoire.alsoGoodLabel || m.Comment.Contains OpeningRepertoire.objectiveBestLabel)
                      Mistake = ""
                      HasTrap = false
                      Next = children }))
            |> List.ofSeq
        match built with
        | main :: _ when not ours ->
            let view (e: float) = if white then e else -e
            built
            |> List.mapi (fun i n ->
                let mistake =
                    match i, n.Eval, main.Eval with
                    | 0, _, _ -> ""
                    | _, Some e, Some m -> OpeningRepertoire.nagOf (OpeningRepertoire.winChance (view e) - OpeningRepertoire.winChance (view m))
                    | _ -> ""
                let mistake = if mistake = "?" || mistake = "??" then mistake else ""
                { n with Mistake = mistake; HasTrap = mistake <> "" || n.Next |> List.exists (fun c -> c.HasTrap) })
        | _ -> built |> List.map (fun n -> { n with HasTrap = n.Next |> List.exists (fun c -> c.HasTrap) })

/// The repertoire of a book repertoire PGN; the side from its Event ("Repertoire for Black"), else `white`.
let load (pgn: string) (white: bool) : Result<Tree, string> =
    match FullPGNParser.parsePgnString pgn |> Seq.tryHead with
    | None -> Error "no game in the file"
    | Some game ->
        let side =
            let e = game.GameMetaData.Event
            if e.Contains(OpeningRepertoire.title false) then false elif e.Contains(OpeningRepertoire.title true) then true else white
        let fen = if String.IsNullOrEmpty game.GameMetaData.Fen then MiscTypes.startPosition else game.GameMetaData.Fen
        let root = nodesOf side fen game.Mainline 0
        if root.IsEmpty then Error "the game has no moves"
        else
            let ours = HashSet<string>()
            let rec count (nodes: Node list) =
                for n in nodes do
                    if (n.Fen.Split(' ').[1] = "w") = side && n.Comment <> "" then ours.Add(BoardUtils.positionKey n.Fen) |> ignore
                    count n.Next
            count root
            // a game without comments has nothing to train (and would be played through as one line)
            if ours.Count = 0 then Error "no moves to train: not a repertoire made with book repertoire"
            else Ok { White = side; StartFen = fen; Root = root; OurPositions = ours.Count }

// ---------------------------------------------------------------- progress

/// A position of ours: its Leitner box (1..5) and the line count from which it is due again.
type Card = { Box: int; DueAt: int; Misses: int }

type Progress = { Lines: int; Cards: Map<string, Card> }

let emptyProgress = { Lines = 0; Cards = Map.empty }

/// Lines until a position in this box comes back.
let interval (box: int) = match box with 1 -> 0 | 2 -> 2 | 3 -> 5 | 4 -> 12 | _ -> 30

/// A position seen before whose time has come (a new one is not due, it is new).
let private due (p: Progress) (fen: string) =
    match p.Cards.TryFind(BoardUtils.positionKey fen) with
    | Some c -> c.DueAt <= p.Lines
    | None -> false

/// The progress file's shape (public: a private record's properties are not serialized; in a module
/// of its own, so its field names do not shadow Card and Progress).
module ProgressFile =
    [<CLIMutable>]
    type CardDto = { Box: int; DueAt: int; Misses: int }

    [<CLIMutable>]
    type ProgressDto = { Lines: int; Cards: Dictionary<string, CardDto> }

/// The progress file of a repertoire: <pgn>.train.json.
let progressPath (pgnPath: string) = pgnPath + ".train.json"

let saveProgress (path: string) (p: Progress) =
    let cards = Dictionary<string, ProgressFile.CardDto>()
    for KeyValue(k, c) in p.Cards do
        cards.[k] <- { ProgressFile.CardDto.Box = c.Box; DueAt = c.DueAt; Misses = c.Misses }
    let dto: ProgressFile.ProgressDto = { Lines = p.Lines; Cards = cards }
    File.WriteAllText(path, JsonSerializer.Serialize(dto, JsonSerializerOptions(WriteIndented = true)))

/// The saved progress, or none when the file is missing or unreadable.
let loadProgress (path: string) =
    try
        if not (File.Exists path) then emptyProgress
        else
            let dto = JsonSerializer.Deserialize<ProgressFile.ProgressDto>(File.ReadAllText path)
            let cards =
                if isNull dto.Cards then Map.empty
                else dto.Cards |> Seq.map (fun kv -> kv.Key, { Card.Box = kv.Value.Box; DueAt = kv.Value.DueAt; Misses = kv.Value.Misses }) |> Map.ofSeq
            { Lines = dto.Lines; Cards = cards }
    with _ -> emptyProgress

// ---------------------------------------------------------------- the session

type Mode =
    /// Every line, the most played most often, due ones first.
    | AllLines
    /// Only lines where the opponent makes a mistake to punish.
    | Traps
    /// Only lines with a position that is due.
    | Due

type Feedback =
    | NoFeedback
    /// The repertoire's move.
    | Correct of Node
    /// An alternative it accepts; its own move is played instead.
    | AlsoGood of played: Node * main: Node
    /// Not the repertoire's move: try again, or Show.
    | Wrong of played: string * expected: Node
    /// Shown after Show: the move is played.
    | Shown of Node
    /// The line ends here.
    | LineDone
    /// Nothing to train in this mode (no traps, nothing due).
    | NothingToTrain

type State =
    { Tree: Tree
      Mode: Mode
      Progress: Progress
      /// The moves played this line, newest first.
      Played: Node list
      /// The moves possible now.
      Here: Node list
      Fen: string
      /// The opponent's last move: the one to answer.
      Opponent: Node option
      /// The opponent's move the trainee has just answered, shown with its comment (not the next one:
      /// its comment would tell a mistake before it is punished).
      Answered: Node option
      Feedback: Feedback
      /// A miss in the current position: no promotion when it is then found.
      Missed: bool
      Right: int
      Wrongs: int
      /// The line has ended: NextLine starts the next.
      Over: bool
      /// The progress changed: the page saves it.
      Changed: bool }

type Event =
    /// A new line (also the first); roll is 0..1 for the opponent's pick.
    | NextLine of roll: float
    /// The trainee's move (UCI); roll for the opponent's reply.
    | Play of uci: string * roll: float
    /// Show the move: it is played (the position counts as missed).
    | Show of roll: float
    | SetMode of Mode * roll: float

let private oursAt (t: Tree) (fen: string) = (fen.Split(' ').[1] = "w") = t.White

let private trained (s: State) (n: Node) = oursAt s.Tree n.Fen && n.Comment <> ""

/// Due positions of ours below (with a comment: the trained part of the tree).
let rec private hasDue (s: State) (n: Node) =
    (trained s n && due s.Progress n.Fen) || n.Next |> List.exists (hasDue s)

/// Positions of ours below never seen.
let rec private hasNew (s: State) (n: Node) =
    (trained s n && not (s.Progress.Cards.ContainsKey(BoardUtils.positionKey n.Fen))) || n.Next |> List.exists (hasNew s)

/// The opponent's move, by share (5% when none is known): a line with a due position weighs four
/// times as much, one seen to the end and not due a quarter.
let private pick (s: State) (moves: Node list) (roll: float) =
    let allowed =
        match s.Mode with
        | AllLines -> moves
        // once the mistake is on the board the line goes on as it comes
        | Traps when s.Played |> List.exists (fun n -> n.Mistake <> "") -> moves
        | Traps -> moves |> List.filter (fun n -> n.HasTrap)
        | Due -> moves |> List.filter (hasDue s)
    match allowed with
    | [] -> None
    | _ ->
        let factor (n: Node) = if hasDue s n then 4.0 elif hasNew s n then 1.0 else 0.25
        let weight (n: Node) = (defaultArg n.Share 5.0 |> max 0.5) * factor n
        let total = allowed |> List.sumBy weight
        let target = Math.Clamp(roll, 0.0, 0.999999) * total
        let rec go acc (rest: Node list) =
            match rest with
            | [ n ] -> n
            | n :: tail -> if acc + weight n > target then n else go (acc + weight n) tail
            | [] -> failwith "unreachable"
        Some(go 0.0 allowed)

let private advance (s: State) (n: Node) =
    { s with Played = n :: s.Played; Here = n.Next; Fen = (BoardUtils.tryMakeMove n.Fen n.Uci).Value; Missed = false }

/// The line counts as trained: one more line for the boxes.
let private finish (s: State) feedback =
    { s with Feedback = feedback; Here = []; Over = true; Progress = { s.Progress with Lines = s.Progress.Lines + 1 }; Changed = true }

/// Moves without a comment are not trained: the opening's first moves are played for the trainee, and
/// mid-line they are the engine's continuation, played out when the line ends there.
let rec private playOn (s: State) (roll: float) (feedback: Feedback) : State =
    match s.Here with
    | [] -> if s.Played.IsEmpty then { s with Feedback = NothingToTrain } else finish s (if feedback = NoFeedback then LineDone else feedback)
    | moves ->
        let opening = s.Played |> List.forall (fun n -> n.Comment = "")
        match moves with
        | [ only ] when only.Comment = "" && opening -> playOn (advance s only) roll feedback
        | main :: _ when main.Comment = "" ->
            // the continuation: played out, then the line is done
            let rec out (st: State) = match st.Here with n :: _ -> out (advance st n) | [] -> st
            finish (out s) (match feedback with NoFeedback -> LineDone | f -> f)
        | _ when oursAt s.Tree s.Fen -> { s with Feedback = feedback }
        | _ ->
            match pick s moves roll with
            | Some n -> playOn { (advance s n) with Opponent = Some n } roll feedback
            | None -> if s.Played |> List.forall (fun n -> n.Comment = "") then { s with Feedback = NothingToTrain } else finish s LineDone

// a new position starts in box 1: found, it goes to 2 and comes back after a few lines
let private card (s: State) = s.Progress.Cards.TryFind(BoardUtils.positionKey s.Fen) |> Option.defaultValue { Box = 1; DueAt = 0; Misses = 0 }

let private mark (s: State) (right: bool) =
    let c = card s
    let c' =
        if right then
            let box = min 5 (c.Box + 1)
            { c with Box = box; DueAt = s.Progress.Lines + 1 + interval box }
        else { c with Box = 1; DueAt = s.Progress.Lines + 1; Misses = c.Misses + 1 }
    { s with Progress = { s.Progress with Cards = s.Progress.Cards.Add(BoardUtils.positionKey s.Fen, c') }; Changed = true }

let private newLine (s: State) (roll: float) =
    playOn { s with Played = []; Here = s.Tree.Root; Fen = s.Tree.StartFen; Opponent = None; Answered = None; Feedback = NoFeedback; Missed = false; Over = false } roll NoFeedback

let start (tree: Tree) (progress: Progress) (mode: Mode) (roll: float) =
    newLine
        { Tree = tree; Mode = mode; Progress = progress; Played = []; Here = tree.Root; Fen = tree.StartFen
          Opponent = None; Answered = None; Feedback = NoFeedback; Missed = false; Right = 0; Wrongs = 0; Over = false; Changed = false }
        roll

let step (s: State) (e: Event) : State =
    let s = { s with Changed = false }
    match e with
    | NextLine roll -> newLine s roll
    | SetMode(mode, roll) -> newLine { s with Mode = mode } roll
    | Play(uci, roll) ->
        match s.Here with
        | main :: _ when oursAt s.Tree s.Fen ->
            if uci = main.Uci then
                let s' = if s.Missed then s else mark s true
                let s' = { s' with Right = (if s.Missed then s.Right else s.Right + 1); Answered = s.Opponent }
                playOn (advance s' main) roll (Correct main)
            else
                match s.Here |> List.tryFind (fun n -> n.Uci = uci && n.AlsoGood) with
                | Some alt ->
                    let s' = if s.Missed then s else mark s true
                    let s' = { s' with Right = (if s.Missed then s.Right else s.Right + 1); Answered = s.Opponent }
                    playOn (advance s' main) roll (AlsoGood(alt, main))
                | None ->
                    let s' = if s.Missed then s else { (mark s false) with Wrongs = s.Wrongs + 1 }
                    { s' with Missed = true; Feedback = Wrong(uci, main) }
        | _ -> s
    | Show roll ->
        match s.Here with
        | main :: _ when oursAt s.Tree s.Fen ->
            let s' = if s.Missed then s else { (mark s false) with Wrongs = s.Wrongs + 1 }
            playOn { (advance s' main) with Answered = s.Opponent } roll (Shown main)
        | _ -> s

type Counts = { Due: int; New: int }

/// Positions of ours in the tree that are due, and those never seen.
let counts (s: State) : Counts =
    let dueNow = HashSet<string>()
    let unseen = HashSet<string>()
    let rec walk (nodes: Node list) =
        for n in nodes do
            if oursAt s.Tree n.Fen && n.Comment <> "" then
                let key = BoardUtils.positionKey n.Fen
                if due s.Progress n.Fen then dueNow.Add key |> ignore
                elif not (s.Progress.Cards.ContainsKey key) then unseen.Add key |> ignore
            walk n.Next
    walk s.Tree.Root
    { Due = dueNow.Count; New = unseen.Count }
