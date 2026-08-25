module ChessLibrary.PuzzleDataUtils

open System
open ChessLibrary.PuzzleTypes
open ChessLibrary.TypesDef.PuzzleInput

let chess = Chess.Board()

/// What PuzzleEngineAgent stamps into MovePlayed when an engine returned no bestmove at
/// all, so a failure is never recorded as an empty field. It is NOT a move, and every
/// consumer that renders MovePlayed has to know that.
[<Literal>]
let NullBestmove = "0000"

/// SAN of the move an engine actually played, or "?" when there is no such move.
///
/// Board.PlayUciMove silently no-ops on ANY move it cannot match, so reading the last SAN
/// afterwards returns whatever PlayCommands last pushed - the OPPONENT's setup move - and
/// the EPD then records a legal-looking but wrong `am`. That output feeds back into
/// search-type configs, which is why this is not merely cosmetic.
///
/// Checking whether a move was actually appended covers the whole class: the "0000"
/// sentinel, an illegal or stale move, and any non-UCI token long enough to pass the
/// callers' length guard. Testing only for the sentinel left the rest silently wrong.
///
/// Lives here because three writers need it: the console EPD writer, the cross-engine EPD
/// writer, and the GUI's. Only the GUI had any guard at all.
let sanOfMovePlayed (board: Chess.Board) (command: string) (movePlayed: string) =
    if movePlayed = NullBestmove then "?"
    else
        board.PlayCommands command
        let before = board.SanMovesPlayed.Count
        board.PlayUciMove movePlayed
        if board.SanMovesPlayed.Count > before then board.SanMovesPlayed.[board.SanMovesPlayed.Count - 1]
        else "?"

let parseRatingGroups (s:string) maxRating =
  let gs = s.Split(',') |> Array.filter (not << System.String.IsNullOrWhiteSpace)
  if gs.Length = 0 then
    if maxRating = 0 then failwith "No rating groups or max rating specified"
    else [| maxRating |]
  else gs |> Array.map int

let parseThemes (s:string) =
  s.Split(',') |> Array.map (fun t -> t.Trim())

let parseNodes (s:string) =
  if System.String.IsNullOrWhiteSpace s then [|0|]
  else
    s.Split(',')
    |> Array.map int

let getPVMoves (pv: string) =
  let pvMoves = ResizeArray<string>()
  let arr = pv.Split ' '
  for item in arr do
    if Seq.exists Char.IsDigit item then
      pvMoves.Add(item)
  pvMoves

let getAllFens (fen: string) (moves: string seq) =
  chess.ResetBoardState()
  let fens = ResizeArray<string>()
  fens.Add(fen)
  chess.LoadFen(fen)
  for move in moves do
    chess.PlayUciMove(move)
    fens.Add(chess.FEN())
  fens

let getUpdatedRecord (record: CsvPuzzleData) =
    // 1) parse SAN moves once
    let moves = getPVMoves record.Moves

    // 2) reset & seed the engine
    chess.ResetBoardState()
    chess.LoadFen record.Fen

    // 3) pre-allocate (optional) for a little extra speed
    let fens = ResizeArray<string>(moves.Count + 1)
    let cmds = ResizeArray<Position>(moves.Count / 2)

    // 4) starting position
    fens.Add record.Fen

    // 5) accumulate a SAN-string for the "moves so far"
    let mutable movesSoFar = ""

    for idx in 0 .. moves.Count - 1 do
      let mv = moves.[idx]

      // 5a) if this is an odd-index move, emit a Puzzle.Position
      if idx % 2 = 1 then
        let commandText = $"position fen {record.Fen} moves {movesSoFar}"
        cmds.Add
          { Command     = commandText
            CorrectMove = mv
            MovePlayed  = "" }

      // 5b) play the move and record its FEN
      chess.PlayUciMove mv
      fens.Add (chess.FEN())

      // 5c) update the SAN-string for the next iteration
      movesSoFar <-
        if idx = 0 then mv
        else $"{movesSoFar} {mv}"

    // 6) stitch back into your record
    { record with
        Fens     = List.ofSeq fens
        Commands = List.ofSeq cmds }

/// Outcome of preparing a run's output folder. Empty is a legitimate configuration
/// ("write no result files"), so it is a distinct case from a folder that could not
/// be created — the two used to be indistinguishable, since both simply skipped.
type OutputFolderStatus =
    /// No FailedPuzzlesOutputFolder configured: deliberate, write nothing.
    | NotConfigured
    /// Folder is usable. `created` is true when this call made it.
    | Ready of path: string * created: bool
    /// Folder cannot be used; `message` says why. The run may continue without files.
    | Failed of path: string * message: string

/// Prepares a run's output folder, creating it when absent.
///
/// Call this ONCE at the start of a run, before any engine is created: an unusable
/// path then surfaces in seconds rather than after the whole run has been computed.
/// A puzzle run used to guard every writer with `Directory.Exists`, so a missing
/// folder silently produced no files at all.
///
/// Deliberately does NOT go through Configuration.JSONParser.normalizePath: that is
/// a JSON string escaper (it doubles backslashes), which is the wrong tool for a
/// filesystem path. Path.GetFullPath is, and it also makes relative paths absolute
/// so the reported location is unambiguous.
///
/// Never throws — the caller decides how loudly to report a failure.
let ensureOutputFolder (folder: string) : OutputFolderStatus =
    if String.IsNullOrWhiteSpace folder then NotConfigured
    else
        let raw = folder.Trim()
        try
            let full = IO.Path.GetFullPath raw
            if IO.Directory.Exists full then Ready(full, false)
            else
                IO.Directory.CreateDirectory full |> ignore
                Ready(full, true)
        with ex ->
            Failed(raw, ex.Message)

let sortPuzzleData (theme:string) ratingGroup input =
      let byTheme =
          if String.IsNullOrWhiteSpace theme then
              input.puzzleData
          else
              input.puzzleData
              |> Array.filter (fun e -> e.Themes.IndexOf(theme, StringComparison.OrdinalIgnoreCase) >= 0 )
      // filter then sort only the needed subset
      byTheme
      |> Array.filter (fun e -> e.Rating <= ratingGroup)
      |> Array.sortByDescending (fun e -> e.Rating)
      |> Array.truncate input.sampleSize
      |> Array.map getUpdatedRecord
