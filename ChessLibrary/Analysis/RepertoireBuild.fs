/// Building a repertoire with engines (book repertoire, the trainer's Build): the engines started, the
/// searches cached by position next to the PGN, the tree built and written. The command line and the
/// GUI ask for a build with a Request and hear of its progress.
module ChessLibrary.RepertoireBuild

open System
open System.IO
open System.Threading
open ChessLibrary.TypesDef.CoreTypes

type Request =
    { /// The engine for the evals.
      Judge: EngineConfig
      /// The engine that picks the ideas in level positions (Lc0 with contempt), if any.
      Ideas: EngineConfig option
      /// A net that plays like people (Maia): the opponent's replies as people play them.
      Human: EngineConfig option
      /// A Polyglot book (.bin) whose moves are answered too.
      Book: string option
      Settings: OpeningRepertoire.Settings
      /// The position and the moves played from it before the repertoire starts.
      Fen: string
      Moves: string list
      /// The judge's and the ideas' search: the go command ("go nodes 2000000") and how a
      /// cache names it ("nodes 2,000,000"); a cache made with another limit is not used.
      Go: string
      LimitText: string
      /// The PGN; the caches go next to it.
      OutPath: string
      Title: string }

/// The ideas' engine has no contempt (it is not Lc0); the message names it.
exception NoContempt of string

type Outcome =
    { Report: OpeningRepertoire.Report
      /// Positions searched by an engine (the rest came from the caches).
      Searched: int
      Elapsed: TimeSpan }

// ---------------------------------------------------------------- what the GUI asks for

/// A search limit as a Request names it: the go command and the cache's name for it, the same as the
/// command line's --nodes, --movetime and --depth, so the GUI and eb-cli share a cache. `kind` is
/// "nodes", "movetime" (ms) or "depth".
let limit (kind: string) (value: int) =
    match kind with
    | "movetime" -> $"go movetime {value}", $"movetime {value} ms"
    | "depth" -> $"go depth {value}", $"depth {value}"
    | _ -> $"go nodes {value}", $"""nodes {value.ToString("N0", Globalization.CultureInfo.InvariantCulture)}"""

/// The FEN and UCI moves of a position command ("position startpos moves e2e4 e7e5").
let ofPositionCommand (command: string) =
    let parts = command.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let rec split (fen: string list) (rest: string list) =
        match rest with
        | "moves" :: moves -> List.rev fen, moves
        | t :: tail -> split (t :: fen) tail
        | [] -> List.rev fen, []
    match parts with
    | "position" :: "startpos" :: rest -> MiscTypes.startPosition, snd (split [] rest)
    | "position" :: "fen" :: rest ->
        let fen, moves = split [] rest
        String.concat " " fen, moves
    | _ -> MiscTypes.startPosition, []

let private whiteToMove (fen: string) = fen.Split(' ').[1] = "w"

/// The side a repertoire from these moves is for: the side that made the last move (after 3...c5,
/// Black's); with no moves, the side to move.
let sideOf (fen: string) (moves: string list) =
    let toMove = whiteToMove (moves |> List.fold (fun f m -> (BoardUtils.tryMakeMove f m).Value) fen)
    if moves.IsEmpty then toMove else not toMove

/// A file name for the repertoire: "Repertoire_Black_d4_d5_c4_e6_Nc3_c5.pgn" (at most 16 moves).
let fileName (white: bool) (fen: string) (moves: string list) =
    let sans =
        moves
        |> List.truncate 16
        |> List.mapFold (fun f m ->
            let board = Chess.Board()
            board.LoadFen f
            let san = match BoardUtils.tryGetMoveAndSanFromUci &board m with Some(_, s) -> s | None -> m
            san.TrimEnd('+', '#'), (BoardUtils.tryMakeMove f m).Value) fen
        |> fst
    let side = if white then "White" else "Black"
    String.concat "_" ([ $"Repertoire_{side}" ] @ sans) + ".pgn"

/// The repertoire's settings from the GUI's fields, the rest at their defaults.
let settings (white: bool) (plies: int) (replies: int) (window: float) (traps: int) (risk: float) (criticalOnly: bool) =
    { OpeningRepertoire.Settings.Defaults with
        White = white
        Plies = plies
        Replies = replies
        Window = window
        Traps = traps
        Risk = risk
        CriticalOnly = criticalOnly }

/// The GUI's request: the judge's engine def, the human net's and the book's (empty for none), the
/// moves from the board, the search limit (`limit`).
let guiRequest (judgePath: string) (humanPath: string) (bookPath: string) (s: OpeningRepertoire.Settings)
               (fen: string) (moves: string seq) (limitKind: string) (limitValue: int) (outPath: string) : Request =
    let def (path: string) = if String.IsNullOrWhiteSpace path then None else Some(Configuration.JSON.readSingleEngineConfig path)
    let go, text = limit limitKind limitValue
    { Judge = Configuration.JSON.readSingleEngineConfig judgePath
      Ideas = None
      Human = def humanPath
      Book = (if String.IsNullOrWhiteSpace bookPath then None else Some bookPath)
      Settings = s
      Fen = fen
      Moves = List.ofSeq moves
      Go = go
      LimitText = text
      OutPath = outPath
      Title = (if s.White then "Repertoire for White" else "Repertoire for Black") }

/// The book's moves of a position that are legal there, with their share in percent.
let bookMoves (path: string) =
    let book = PolyglotFormat.BookFiles.get path
    fun (fen: string) ->
        let entries =
            book.Lookup(PolyglotFormat.keyOfFen fen)
            |> Array.filter (fun e -> e.Weight > 0us)
            |> Array.choose (fun e ->
                let uci = PolyglotFormat.moveToUci fen e.Move
                BoardUtils.tryMakeMove fen uci |> Option.map (fun _ -> uci, float e.Weight))
        let total = entries |> Array.sumBy snd
        if total <= 0.0 then [] else entries |> Array.map (fun (m, w) -> m, 100.0 * w / total) |> List.ofArray

/// What makes a cache valid: the engine, its options and the limit.
let signature (config: EngineConfig) (limitText: string) =
    let options = config.Options |> Seq.sortBy (fun kv -> kv.Key) |> Seq.map (fun kv -> $"{kv.Key}={kv.Value}") |> String.concat ";"
    $"eb repertoire cache 1 | {config.Path} | {config.Args} | {config.NetworkPath} | {options} | {limitText}"

/// The engine with Lc0's contempt for a rating gap, for the side the repertoire is for.
let withContempt (config: EngineConfig) (white: bool) (rating: int) (opponent: int) =
    let applied = (Lc0Contempt.tryAnalysisPreset "prep").Value.Apply rating opponent
    let settings = { applied.Settings with Mode = (if white then Lc0Contempt.WhiteSideAnalysis else Lc0Contempt.BlackSideAnalysis) }
    let options = Collections.Generic.Dictionary<string, obj>(config.Options)
    for o in Lc0Contempt.toOptions settings do
        options.[o.Name] <- box o.Value
    { config with Options = options }

/// Builds the repertoire and writes it to OutPath. `log` hears what is started and loaded, `onSearch`
/// the count of positions searched so far. Cancelled, it throws OperationCanceledException: what was
/// searched is in the caches, and the same request goes on from there. Throws NoContempt when the
/// ideas' engine has no contempt.
let run (r: Request) (log: string -> unit) (onSearch: int -> unit) (token: CancellationToken) : Outcome =
    let searched = ref 0
    let counted (_: string) = onSearch (Interlocked.Increment &searched.contents)
    let started = Collections.Generic.List<EngineSearch.Analyzer>()
    let agents = Collections.Generic.List<OpeningRepertoire.SearchAgent>()
    let cacheName (role: string) = $"{Path.GetFileName r.OutPath}.{role}.cache"
    let agentFor (role: string) (c: EngineConfig) =
        log $"Starting {c.Name} ({role})"
        let a = EngineSearch.start c
        started.Add a
        let agent =
            new OpeningRepertoire.SearchAgent(c.Name, signature c r.LimitText, Some(r.OutPath + $".{role}.cache"),
                                              (fun f m k -> EngineSearch.lines a (List.fold (fun f u -> (BoardUtils.tryMakeMove f u).Value) f m) k r.Go), counted)
        agents.Add agent
        if agent.Loaded > 0 then log $"  {agent.Loaded} positions from {cacheName role}"
        a, agent
    try
        let _, judge = agentFor "judge" r.Judge
        let chooser =
            r.Ideas
            |> Option.map (fun c ->
                let a, agent = agentFor "ideas" c
                let support = Lc0Contempt.supportOf (a.Engine.GetAllDefaultOptions().Keys)
                if not support.HasContempt then raise (NoContempt $"contempt needs Lc0 (ContemptMode); {c.Name} does not have it")
                agent)
        // the human net: one node, its policy is the answer; move stats switched on where it has them
        let human =
            r.Human
            |> Option.map (fun c ->
                log $"Starting {c.Name} (human)"
                let a = EngineSearch.start c
                started.Add a
                if a.Engine.GetAllDefaultOptions().Keys |> Seq.exists (fun k -> k.Equals("VerboseMoveStats", StringComparison.OrdinalIgnoreCase)) then
                    a.Engine.SetOption(EngineTypes.EngineOption.Create "VerboseMoveStats" "true")
                let one = "go nodes 1"
                let agent =
                    new OpeningRepertoire.SearchAgent(c.Name, signature c "nodes 1" + " | policy", Some(r.OutPath + ".human.cache"),
                                                      (fun f m _ -> EngineSearch.policyLines a f m one), counted)
                agents.Add agent
                let file = cacheName "human"
                if agent.Loaded > 0 then log $"  {agent.Loaded} positions from {file}"
                agent)
        let book = match r.Book with Some path -> bookMoves path | None -> fun _ -> []
        log $"{r.Title}: {r.LimitText} per position, {r.Settings.Plies} plies, up to {r.Settings.Replies} opponent replies explored per position"
        let sw = Diagnostics.Stopwatch.StartNew()
        let report =
            Async.RunSynchronously(OpeningRepertoire.build r.Settings judge chooser human book r.Fen r.Moves r.Title, cancellationToken = token)
        File.WriteAllText(r.OutPath, report.Pgn)
        { Report = report; Searched = searched.Value; Elapsed = sw.Elapsed }
    finally
        for a in agents do a.Close()
        for a in started do EngineSearch.quit a
