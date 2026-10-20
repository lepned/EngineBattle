/// Tournament openings drawn from a Polyglot book (.bin): a move per ply by the book's weights, seeded so
/// a resumed tournament draws the same lines.
module ChessLibrary.PolyglotLines

open System
open System.Collections.Generic
open ChessLibrary.Chess
open ChessLibrary.PGNTypes

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// A position without its move counters: what makes two lines the same opening, and the cache key.
let private positionKey (fen: string) = fen.Split(' ') |> Array.truncate 4 |> String.concat " "

/// The book's moves for the board's position that are legal there (a damaged or foreign book can
/// hold others) and have a weight: UCI, SAN, weight. The one rule for the tournament and the page.
let private legalBookMoves (board: Board) (fen: string) (entries: PolyglotFormat.Entry[]) =
    entries
    |> Array.filter (fun e -> e.Weight > 0us && e.Move <> 0us)
    |> Array.choose (fun e ->
        let uci = PolyglotFormat.moveToUci fen e.Move
        match BoardUtils.tryGetMoveAndSanFromUci &board uci with
        | Some (_, san) -> Some (uci, san, int e.Weight)
        | None -> None)

/// A position's book moves, looked up once: many lines pass the same positions.
type private Cache = Dictionary<string, (string * string * int)[]>

/// One line of at most `maxPly` plies: the moves in SAN and the position it ends in (FEN).
let private drawLineCached (cache: Cache) (lookup: uint64 -> PolyglotFormat.Entry[]) (rng: Random) (maxPly: int) : string list * string =
    let board = Board()
    board.ResetBoardState()
    let moves = ResizeArray<string>()
    let mutable go = true
    while go && moves.Count < maxPly do
        let fen = board.FEN()
        let position = positionKey fen
        let candidates =
            match cache.TryGetValue position with
            | true, c -> c
            | _ ->
                let c = legalBookMoves board fen (lookup (PolyglotFormat.keyOfFen fen))
                cache.[position] <- c
                c
        if candidates.Length = 0 then go <- false
        else
            let total = candidates |> Array.sumBy (fun (_, _, w) -> w)
            let mutable pick = rng.Next total
            let mutable chosen = candidates.[0]
            let mutable found = false
            for (_, _, w) as c in candidates do
                if not found then
                    if pick < w then
                        chosen <- c
                        found <- true
                    else pick <- pick - w
            let uci, san, _ = chosen
            board.PlayUciMove uci
            moves.Add san
    List.ofSeq moves, board.FEN()

/// One line of at most `maxPly` plies: the moves in SAN and the position it ends in (FEN).
let drawLine (book: PolyglotFormat.Entry[]) (rng: Random) (maxPly: int) =
    drawLineCached (Cache()) (PolyglotFormat.entriesFor book) rng maxPly

/// A line as a PGN opening game, numbered `n`.
let toPgnGame (n: int) (bookName: string) (moves: string list) =
    let body =
        moves
        |> List.mapi (fun i san -> if i % 2 = 0 then sprintf "%d. %s" (i / 2 + 1) san else san)
        |> String.concat " "
    let text = sprintf "[Event \"%s\"]\n[Round \"%d\"]\n[Result \"*\"]\n\n%s *\n" bookName n body
    { FullPGNParser.parseFullPgnGame text with Raw = text; GameNumber = n }

/// `count` different lines, the same for the same seed (the first n whatever the count), capped at `maxPly`
/// plies (0: to the book's end); fewer when the book has fewer.
let lines (path: string) (count: int) (seed: int) (maxPly: int) : PgnGame[] =
    let onDisk = if IO.FileInfo(path).Length > 64L * 1024L * 1024L then Some (new PolyglotFormat.BookFile(path)) else None
    use _close = { new IDisposable with member _.Dispose() = onDisk |> Option.iter (fun b -> (b :> IDisposable).Dispose()) }
    let lookup =
        match onDisk with
        | Some book -> book.Lookup
        | None -> PolyglotFormat.entriesFor (PolyglotFormat.read path)
    let maxPly = if maxPly > 0 then maxPly else Int32.MaxValue
    let rng = Random(seed)
    let seen = HashSet<string>()
    let result = ResizeArray<PgnGame>()
    // a narrow book repeats itself: give up after many draws in a row that bring nothing new
    let mutable misses = 0
    let name = IO.Path.GetFileName path
    let cache = Cache()
    while result.Count < count && misses < 2000 do
        let moves, fen = drawLineCached cache lookup rng maxPly
        // the position decides what is new: transpositions are one opening, as for PGN books
        let key = positionKey fen
        if not moves.IsEmpty && seen.Add key then
            result.Add(toPgnGame (result.Count + 1) name moves)
            misses <- 0
        else misses <- misses + 1
    result.ToArray()

/// A book move of a position, for the analysis page's Book card: the move, its weight and its share
/// of the position's weights in percent.
type BookMove = { Uci: string; San: string; Weight: int; Percent: float }

/// The book's moves for a position, heaviest first: only moves legal there and with a weight above
/// 0. Empty when the position is not in the book. The book is looked up on disk (PolyglotFormat.BookFiles).
let movesFor (path: string) (fen: string) : BookMove[] =
    let book = PolyglotFormat.BookFiles.get path
    let board = Board()
    board.LoadFen fen
    let found = legalBookMoves board fen (book.Lookup(PolyglotFormat.keyOfFen fen))
    let total = found |> Array.sumBy (fun (_, _, w) -> w)
    found
    |> Array.map (fun (uci, san, w) -> { Uci = uci; San = san; Weight = w; Percent = 100.0 * float w / float total })
    |> Array.sortByDescending (fun m -> m.Weight)

// ---------------------------------------------------------------- making a book from games

/// How a book is made from games: the plies of each game that go in, the games a move needs to be
/// kept, and whether the results weigh the moves (Polyglot's 2*wins + draws) or every move counts 1.
type MakeOptions = { MaxPly: int; MinGames: int; Uniform: bool }

/// What making a book did.
type MakeSummary =
    { Games: int
      Used: int
      /// Chess960, another variant, or a game with no result
      Skipped: int
      /// Games with a move the board could not play: used up to it
      Broken: int
      Positions: int
      Moves: int }

/// A book from games' first `MaxPly` plies: a move is kept when `MinGames` played it, weighted 2 x wins +
/// draws (1 each with `Uniform`, for files without results).
let makeBook (games: seq<PgnGame>) (options: MakeOptions) (outPath: string) (progress: int -> unit) : MakeSummary =
    let stats = Dictionary<struct (uint64 * uint16), struct (int * int)>()
    let mutable total = 0
    let mutable used = 0
    let mutable skipped = 0
    let mutable broken = 0
    let board = Board()
    for g in games do
        total <- total + 1
        if total % 1000 = 0 then progress total
        let meta = g.GameMetaData
        let variant =
            meta.OtherTags |> List.tryFind (fun t -> t.Key.Equals("Variant", StringComparison.OrdinalIgnoreCase)) |> Option.map (fun t -> t.Value)
        let standardVariant = match variant with None -> true | Some v -> v.Equals("Standard", StringComparison.OrdinalIgnoreCase) || v = ""
        // White's points: 2 a win, 1 a draw, 0 a loss; a game without a result says nothing - unless
        // every move weighs 1, where results do not count (an opening file's lines end in *)
        let whitePoints = match meta.Result with "1-0" -> Some 2 | "0-1" -> Some 0 | "1/2-1/2" -> Some 1 | _ -> None
        if not standardVariant || (whitePoints.IsNone && not options.Uniform) then skipped <- skipped + 1
        else
            let ok =
                try
                    // a clean board per game: LoadFen alone keeps the previous game's move graph
                    board.ResetBoardState()
                    if not (String.IsNullOrWhiteSpace meta.Fen) then board.LoadFen meta.Fen
                    not board.IsFRC
                with _ -> false
            if not ok then skipped <- skipped + 1
            else
                used <- used + 1
                let mutable ply = 0
                let mutable go = true
                use moves = (g.Mainline :> seq<_>).GetEnumerator()
                while go && ply < options.MaxPly && moves.MoveNext() do
                    let fen = board.FEN()
                    let san = moves.Current.San.TrimEnd('+', '#', '!', '?')
                    // a move in coordinates (e2e4) as the board accepts it, otherwise SAN
                    // files are lower case: SAN's R1a3 or N5f3 has the same shape with a piece letter
                    let file (c: char) = c >= 'a' && c <= 'h'
                    let rank (c: char) = c >= '1' && c <= '8'
                    let coordinates = (san.Length = 4 || san.Length = 5) && file san.[0] && rank san.[1] && file san.[2] && rank san.[3]
                    let resolved = if coordinates then Some san else board.GetUciFromSan san
                    match resolved |> Option.filter (fun uci -> (BoardUtils.tryGetMoveAndSanFromUci &board uci).IsSome) with
                    | Some uci ->
                        let whiteToMove = fen.Split(' ').[1] = "w"
                        // a castle: O-O in SAN, or the king's two-square move in coordinates (a queen e1-g1 is not)
                        let kingCastles =
                            san.StartsWith "O-O" || san.StartsWith "0-0"
                            || (coordinates && List.contains (uci.Substring(0, 4)) [ "e1g1"; "e1c1"; "e8g8"; "e8c8" ]
                                && PolyglotFormat.pieceAt fen (uci.Substring(0, 2)) = (if uci.[1] = '1' then 'K' else 'k'))
                        let key = struct (PolyglotFormat.keyOfFen fen, PolyglotFormat.encodeMove uci kingCastles)
                        let white = defaultArg whitePoints 1
                        let points = if whiteToMove then white else 2 - white
                        let struct (n, score) = match stats.TryGetValue key with | true, v -> v | _ -> struct (0, 0)
                        stats.[key] <- struct (n + 1, score + points)
                        board.PlayUciMove uci
                        ply <- ply + 1
                    | None ->
                        broken <- broken + 1
                        go <- false
    let kept =
        [| for KeyValue (struct (key, move), struct (n, score)) in stats do
             let weight = if options.Uniform then 1 else score
             if n >= options.MinGames && weight > 0 then yield key, move, weight |]
    let top = if kept.Length = 0 then 1 else kept |> Array.map (fun (_, _, w) -> w) |> Array.max
    let scale (w: int) = if top <= 65535 then w else max 1 (int (int64 w * 65535L / int64 top))
    let entries = kept |> Array.map (fun (key, move, w) -> ({ Key = key; Move = move; Weight = uint16 (scale w) } : PolyglotFormat.Entry))
    // an empty book is not written: a book already at the path is kept
    if entries.Length > 0 then PolyglotFormat.write outPath entries |> ignore
    { Games = total; Used = used; Skipped = skipped; Broken = broken
      Positions = entries |> Array.distinctBy (fun e -> e.Key) |> Array.length
      Moves = entries.Length }

// ---------------------------------------------------------------- a book back to opening lines

/// The book's lines as a tree from the start: moves with at least `minShare` percent, to `maxPly` plies,
/// at most `maxLines` (the bool: the cap was hit); transpositions count once.
let exportTree (path: string) (maxPly: int) (minShare: float) (maxLines: int) : PgnGame[] * bool =
    use book = new PolyglotFormat.BookFile(path)
    let cache = Cache()
    let seen = HashSet<string>()
    // a position reached again by another move order, with no more plies left, has been walked:
    // transpositions would otherwise walk the same subtree again for every way into it
    let walked = Dictionary<string, int>()
    let result = ResizeArray<PgnGame>()
    let mutable capped = false
    let name = IO.Path.GetFileName path
    let maxPly = if maxPly > 0 then maxPly else Int32.MaxValue
    let rec walk (moves: (string * string) list) (fen: string) =
        if result.Count >= maxLines then capped <- true
        else
            let board = Board()
            board.LoadFen fen
            let position = positionKey fen
            let left = maxPly - moves.Length
            let again = match walked.TryGetValue position with | true, l -> l >= left | _ -> false
            if not again then walked.[position] <- left
            let candidates =
                if again || moves.Length >= maxPly then [||]
                else
                    let all =
                        match cache.TryGetValue position with
                        | true, c -> c
                        | _ ->
                            let c = legalBookMoves board fen (book.Lookup(PolyglotFormat.keyOfFen fen))
                            cache.[position] <- c
                            c
                    let total = all |> Array.sumBy (fun (_, _, w) -> w)
                    all |> Array.filter (fun (_, _, w) -> 100.0 * float w / float total >= minShare) |> Array.sortByDescending (fun (_, _, w) -> w)
            if candidates.Length = 0 then
                if not again && not moves.IsEmpty && seen.Add position then
                    result.Add(toPgnGame (result.Count + 1) name (moves |> List.rev |> List.map snd))
            else
                for uci, san, _ in candidates do
                    if result.Count < maxLines then
                        let next = Board()
                        next.LoadFen fen
                        next.PlayUciMove uci
                        walk ((uci, san) :: moves) (next.FEN())
                    else capped <- true
    walk [] startFen
    result.ToArray(), capped

/// Opening games as a PGN file, as a book is read back (OpeningsPath, Book Evaluation).
let writePgn (path: string) (games: seq<PgnGame>) =
    let dir = IO.Path.GetDirectoryName(IO.Path.GetFullPath path)
    if not (String.IsNullOrEmpty dir) then IO.Directory.CreateDirectory dir |> ignore
    IO.File.WriteAllText(path, games |> Seq.map (fun g -> g.Raw) |> String.concat "\n")
