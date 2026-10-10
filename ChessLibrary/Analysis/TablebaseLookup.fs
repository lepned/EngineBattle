module ChessLibrary.TablebaseLookup

open System
open EngineBattle.Tablebases
open ChessLibrary.BoardUtils

/// <summary>
/// What the tablebases say about every move of a position, for the tablebase page and the `tb`
/// verb. The halfmove clock counts: a cursed win is a win the 50-move rule turns into a draw from
/// this clock on. Everything is from the side to move's view.
/// </summary>
type TbCategory =
    | Win
    | CursedWin
    | Draw
    | BlessedLoss
    | Loss

/// One legal move. Dtz is |DTZ| counted from the position before the move (the best move has the
/// position's own DTZ); None for a zeroing move (it resets the clock, DTZ says nothing more), a
/// drawing move, or a set without DTZ tables. Uncertain: a win or loss from the WDL tables alone
/// with the clock running, where the 50-move rule could not be checked.
type TbMoveRow =
    { Uci: string
      San: string
      Category: TbCategory
      Dtz: int option
      IsZeroing: bool
      IsCapture: bool
      GivesCheck: bool
      IsCheckmate: bool
      IsStalemate: bool
      Uncertain: bool }

type TbStatus =
    /// The position's value and |DTZ| (None without DTZ tables); Moves holds every legal move.
    | Answered of TbCategory * int option
    | Checkmate
    | Stalemate
    | NoTablebaseFolder
    | FoldersMissing of string[]
    | InvalidFen of string[]
    | CastlingRights
    /// The folders hold no Syzygy tables (.rtbw).
    | NoTablesInFolders
    /// More pieces than the tables in the folders hold (largest table size).
    | TooManyPieces of pieces: int * largest: int
    /// No table for this material, e.g. "KRBvKN".
    | NoTable of material: string

type TbLookup =
    { /// The position as probed: normalized, castling rights the placement cannot support dropped.
      Fen: string
      Status: TbStatus
      Moves: TbMoveRow[]
      /// false: the WDL tables answered alone (no DTZ table for this material).
      HasDtz: bool
      /// The position's own win or loss is from the WDL tables with the clock running (see TbMoveRow).
      Uncertain: bool }

let private categoryOf (wdl: TbWdl) =
    match wdl with
    | TbWdl.Win -> Some Win
    | TbWdl.CursedWin -> Some CursedWin
    | TbWdl.Draw -> Some Draw
    | TbWdl.BlessedLoss -> Some BlessedLoss
    | TbWdl.Loss -> Some Loss
    | _ -> None

/// The same result for the other side (a child position's value seen from its parent).
let private flip (wdl: TbWdl) =
    if wdl = TbWdl.Failed then wdl else enum<TbWdl> (4 - int wdl)

let private categoryRank =
    function
    | Win -> 0
    | CursedWin -> 1
    | Draw -> 2
    | BlessedLoss -> 3
    | Loss -> 4

/// Best move first: categories Win..Loss; a win by mate, then by a zeroing move, then the
/// shortest DTZ; a loss by the longest DTZ, zeroing moves (which lose soonest) last; then SAN.
let sortMoves (moves: TbMoveRow[]) =
    moves
    |> Array.sortBy (fun m ->
        let dtz = defaultArg m.Dtz 0
        let within =
            match m.Category with
            | Win
            | CursedWin -> (if m.IsCheckmate then 0 elif m.IsZeroing then 1 else 2), dtz
            | Draw -> 0, 0
            | BlessedLoss
            | Loss -> (if m.IsZeroing then 1 else 0), -dtz
        categoryRank m.Category, within, m.San)

/// "KRBvKN": white's pieces, then black's, strongest first (the Syzygy file name).
let materialOf (fen: string) =
    let placement = (normalizeFen fen).Split(' ').[0]
    let side (pieces: string) =
        pieces |> Seq.collect (fun p -> placement |> Seq.filter ((=) p)) |> Seq.map Char.ToUpperInvariant |> String.Concat
    side "KQRBNP" + "v" + side "kqrbnp"

let private pieceCount (fen: string) =
    (normalizeFen fen).Split(' ').[0] |> Seq.filter Char.IsLetter |> Seq.length

/// The FEN without castling rights (the tablebases have none); None when it has none.
let withoutCastling (fen: string) =
    let f = (normalizeFen fen).Split(' ')
    if f.[2] = "-" then None
    else
        f.[2] <- "-"
        Some(String.Join(" ", f))

/// The FEN as probed: the castling rights as the board reads them - a right without its castle
/// rook is none, an X-FEN or Shredder right with one stays (and is refused as CastlingRights).
/// The halfmove clock is kept: it counts.
let probeFen (fen: string) =
    let f = (normalizeFen fen).Split(' ')
    let board = ChessLibrary.Chess.Board()
    board.LoadFen(String.Join(" ", f))
    f.[2] <- board.FEN().Split(' ').[2]
    String.Join(" ", f)

/// The FEN's halfmove clock; 0 when it has none.
let halfmoveClock (fen: string) =
    TablebaseProbe.halfmoveClock (normalizeFen fen)

/// "The halfmove clock is at 100: a draw can be claimed (50-move rule)"; None below 100.
let clockNote (r: TbLookup) =
    let clock = halfmoveClock r.Fen
    if clock >= 100 then Some $"The halfmove clock is at {clock}: a draw can be claimed (50-move rule)" else None

let private row (m: MoveNotation) category dtz childStatus uncertain =
    { Uci = m.Uci
      San = m.San
      Category = category
      Dtz = (if m.IsZeroing || category = Draw then None else dtz)
      IsZeroing = m.IsZeroing
      IsCapture = m.IsCapture
      GivesCheck = m.GivesCheck
      IsCheckmate = (childStatus = "checkmate")
      IsStalemate = (childStatus = "stalemate")
      Uncertain = uncertain }

let private childStatusOf fen (m: MoveNotation) =
    match tryMakeMove fen m.Uci with
    | Some child -> child, (getPositionStatus child).Status
    | None -> "", ""

/// With DTZ tables: the root probe gives every move's value and DTZ in one go.
let rowsFromRootProbe fen (legal: MoveNotation[]) (r: TbRootResult) =
    let byUci = r.Moves |> Array.map (fun m -> m.Uci, m) |> dict
    let rows =
        legal
        |> Array.choose (fun m ->
            match byUci.TryGetValue m.Uci with
            | true, p ->
                let _, status = childStatusOf fen m
                categoryOf p.Wdl |> Option.map (fun c -> row m c (Some p.Dtz) status false)
            | _ -> None)
    if rows.Length <> legal.Length then None else Some rows

/// A WDL table reads the clock as 0, so with the clock running its win or loss may be one the
/// 50-move rule turns into a draw. Draws, cursed wins and blessed losses stay what they are.
let private wdlUncertain clock category =
    clock > 0 && (category = Win || category = Loss)

/// Without DTZ tables: each child's WDL. A zeroing move or a mate is exact; any other win or loss
/// is Uncertain when the clock runs (at clock 0 only a win exactly on the 100-ply border can differ).
let rowsFromWdl fen (legal: MoveNotation[]) =
    let clock = halfmoveClock fen
    let rows =
        legal
        |> Array.map (fun m ->
            let child, status = childStatusOf fen m
            let category =
                match status with
                | "checkmate" -> Some Win
                | "stalemate" -> Some Draw
                | _ when child = "" -> None
                | _ -> categoryOf (flip (Syzygy.ProbeWdl child))
            category
            |> Option.map (fun c ->
                let uncertain = not m.IsZeroing && status <> "checkmate" && wdlUncertain clock c
                row m c None status uncertain))
    if rows |> Array.forall Option.isSome then Some(rows |> Array.map Option.get) else None

/// The material of the table a failed probe needed. A probe resolves captures first, so a missing
/// table after a capture or promotion fails the position above it too: follow the material-changing
/// moves down to the deepest position that still fails (pieces only ever decrease, so it ends).
let rec private missingMaterial fen =
    legalMovesOf fen
    |> Array.filter (fun m -> m.IsCapture || m.Uci.Length = 5)
    |> Array.tryPick (fun m ->
        match childStatusOf fen m with
        | child, ("ok" | "check") when Syzygy.ProbeWdl child = TbWdl.Failed -> Some(missingMaterial child)
        | _ -> None)
    |> Option.defaultValue (materialOf fen)

/// The FEN with the halfmove clock read as 0: what the position is worth if the clock starts now
/// (as lichess shows it).
let clockZero (fen: string) =
    let f = (normalizeFen fen).Split(' ')
    f.[4] <- "0"
    String.Join(" ", f)

/// The tablebase answer for a position. tablebasePath is the setting: one or more folders.
/// countClock false reads the halfmove clock as 0. Tables added to a loaded folder are seen after
/// TablebaseProbe.useTables true (the page does it when the folder is set).
let lookupWith (countClock: bool) (tablebasePath: string) (fen: string) : TbLookup =
    let answer fen status = { Fen = fen; Status = status; Moves = [||]; HasDtz = false; Uncertain = false }
    let validation = validateFen fen
    if not validation.IsValid then answer fen (InvalidFen validation.Errors)
    else
        let fen = if countClock then probeFen fen else clockZero (probeFen fen)
        let folders = TablebaseProbe.tablebaseFolders tablebasePath
        let missing = folders |> Array.filter (not << IO.Directory.Exists)
        match (getPositionStatus fen).Status with
        | "checkmate" -> answer fen Checkmate
        | "stalemate" -> answer fen Stalemate
        | _ when folders.Length = 0 -> answer fen NoTablebaseFolder
        | _ when missing.Length > 0 -> answer fen (FoldersMissing missing)
        | _ ->
            let pieces = pieceCount fen
            // Listed per lookup (one *.rtbw listing): the prober holds every folder ever used, so
            // without it a wrong or empty folder would still be answered from an earlier one.
            let largest = TablebaseProbe.largestTable tablebasePath
            if largest = 0 then answer fen NoTablesInFolders
            elif pieces > largest then answer fen (TooManyPieces(pieces, largest))
            elif (withoutCastling fen).IsSome then answer fen CastlingRights
            else
                TablebaseProbe.useTables false tablebasePath
                let legal = legalMovesOf fen
                let root = Syzygy.ProbeRoot fen
                let withDtz =
                    if root.Ok then
                        rowsFromRootProbe fen legal root
                        |> Option.bind (fun rows ->
                            categoryOf root.Wdl |> Option.map (fun c -> rows, c, Some root.Dtz))
                    else None
                let result =
                    match withDtz with
                    | Some(rows, c, dtz) -> Some(rows, c, (if c = Draw then None else dtz), true)
                    | None ->
                        match categoryOf (Syzygy.ProbeWdl fen), rowsFromWdl fen legal with
                        | Some c, Some rows -> Some(rows, c, None, false)
                        | _ -> None
                match result with
                | Some(rows, c, dtz, hasDtz) ->
                    { Fen = fen; Status = Answered(c, dtz); Moves = sortMoves rows; HasDtz = hasDtz
                      Uncertain = not hasDtz && wdlUncertain (halfmoveClock fen) c }
                | None -> answer fen (NoTable(missingMaterial fen))

/// The tablebase answer with the halfmove clock counted (the default).
let lookup (tablebasePath: string) (fen: string) : TbLookup =
    lookupWith true tablebasePath fen

let categoryText =
    function
    | Win -> "Win"
    | CursedWin -> "Cursed win"
    | Draw -> "Draw"
    | BlessedLoss -> "Blessed loss"
    | Loss -> "Loss"

/// What a move does, in a word or two: "Checkmate", "Zeroing", "DTZ 19", "50-move rule not
/// checked"; "" for a plain draw.
let moveNote (m: TbMoveRow) =
    if m.IsCheckmate then "Checkmate"
    elif m.IsStalemate then "Stalemate"
    elif m.IsZeroing && m.Category <> Draw then "Zeroing"
    elif m.Uncertain then "50-move rule not checked"
    else m.Dtz |> Option.map (sprintf "DTZ %d") |> Option.defaultValue ""

/// The line under the moves when they come from the WDL tables alone; None with DTZ tables.
let wdlOnlyNote (r: TbLookup) =
    if r.Moves.Length = 0 || r.HasDtz then None
    elif halfmoveClock r.Fen > 0 then
        Some "No DTZ tables (.rtbz) for this material: win, draw or loss only, and with the halfmove clock running a win or loss may be one the 50-move rule turns into a draw (marked)"
    else
        Some "No DTZ tables (.rtbz) for this material: win, draw or loss only, and a move exactly on the 50-move border shows as a plain win or loss"

/// The answer in words, for the page and the tb verb (each adds where its folder setting lives).
let statusText (tablebasePath: string) (r: TbLookup) =
    let side, other = if (r.Fen.Split(' ') |> Array.tryItem 1) = Some "b" then "Black", "White" else "White", "Black"
    let dtz = function Some d -> $" (DTZ {d})" | None -> ""
    match r.Status with
    | Answered(c, _) when r.Uncertain ->
        let verb = if c = Win then "wins" else "loses"
        $"{side} {verb}, unless the 50-move rule draws it (no DTZ tables to check)"
    | Answered(Win, d) -> $"{side} wins{dtz d}"
    | Answered(CursedWin, d) -> $"{side} wins, but the 50-move rule makes it a draw{dtz d}"
    | Answered(Draw, _) -> "Draw"
    | Answered(BlessedLoss, d) -> $"{side} loses, but the 50-move rule saves the draw{dtz d}"
    | Answered(Loss, d) -> $"{side} loses{dtz d}"
    | Checkmate -> $"Checkmate: {other} wins"
    | Stalemate -> "Stalemate: draw"
    | NoTablebaseFolder -> "No tablebase folder set"
    | FoldersMissing folders -> $"""Tablebase folder not found: {String.Join(", ", folders)}"""
    | InvalidFen errors -> $"""Invalid FEN: {String.Join("; ", errors)}"""
    | CastlingRights -> "The tablebases have no positions with castling rights"
    | NoTablesInFolders -> $"No Syzygy tables (.rtbw) in {tablebasePath}"
    | TooManyPieces(n, largest) -> $"{n} pieces: the tables in {tablebasePath} go up to {largest}"
    | NoTable material -> $"No table for {material} in {tablebasePath}"
