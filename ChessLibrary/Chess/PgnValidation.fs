/// Whether a PGN's moves are right: every game replayed on a board, each move matched against the
/// legal moves of its position. Streams - one game at a time - so a file of any size takes the
/// same memory. Results and comments are not looked at: only whether the moves can be played.
module ChessLibrary.PgnValidation

open System
open System.Text.RegularExpressions
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open MoveTypes

type Kind =
  /// no legal move fits: the rest of the game is not checked
  | IllegalMove
  /// more than one legal move fits: the rest of the game is not checked
  | AmbiguousMove
  /// the FEN tag cannot be set up: the game is not checked
  | BadStart
  /// a legal move, written otherwise than standard SAN (a missing x, needless disambiguation,
  /// coordinates): the game goes on
  | NonStandardSan

let kindName kind =
  match kind with
  | IllegalMove -> "illegal move"
  | AmbiguousMove -> "ambiguous move"
  | BadStart -> "bad start FEN"
  | NonStandardSan -> "non-standard SAN"

let isError kind = kind <> NonStandardSan

type Finding =
  { Game: int
    Round: string
    White: string
    Black: string
    /// half-moves from the start of the game, 1 for the first; 0 for the start position
    Ply: int
    /// "23." or "23..." with the move as written
    Move: string
    Kind: Kind
    /// the standard SAN, or the candidates of an ambiguous move, or why the FEN failed
    Expected: string
    /// the position the move was played in
    Fen: string }

// the spelling that matters: no check or annotation marks, castling with zeros, promotion without '='
let private normalize (san: string) =
  san.Trim().TrimEnd('+', '#', '!', '?').Replace('O', '0').Replace("=", "")

let private pieceMove = Regex(@"^([KQRBN])[a-h]?[1-8]?x?([a-h][1-8])$", RegexOptions.Compiled)

// what a piece move says without its disambiguation and capture mark: the piece and its square
let private bare (san: string) =
  let n = normalize san
  let m = pieceMove.Match n
  if m.Success then m.Groups.[1].Value + m.Groups.[2].Value else n

// A move-list buffer per thread: the usual move needs no new array
let private buffers = new System.Threading.ThreadLocal<TMove[]>(fun () -> Array.zeroCreate 256)

let private markEnd (s: string) =
  let mutable e = s.Length
  while e > 0 && (let c = s.[e - 1] in c = '+' || c = '#' || c = '!' || c = '?' || c = ' ') do e <- e - 1
  e

let private textStart (s: string) =
  let mutable i = 0
  while i < s.Length && s.[i] = ' ' do i <- i + 1
  i

/// normalize a = normalize b, compared in place: marks at the end, '=' and O for 0 do not count
let private sameSan (a: string) (b: string) =
  let ea = markEnd a
  let eb = markEnd b
  let mutable i = textStart a
  let mutable j = textStart b
  let mutable same = true
  while same && (i < ea || j < eb) do
    if i < ea && a.[i] = '=' then i <- i + 1
    elif j < eb && b.[j] = '=' then j <- j + 1
    elif i < ea && j < eb then
      let ca = if a.[i] = 'O' then '0' else a.[i]
      let cb = if b.[j] = 'O' then '0' else b.[j]
      if ca <> cb then same <- false
      i <- i + 1
      j <- j + 1
    else same <- false
  same

/// The square a SAN names last - where the move goes - as the board numbers it for the side to
/// move; -1 for castling and for text with no square.
let private targetSquare (san: string) (stm: byte) =
  let mutable k = markEnd san - 1
  while k > 0 && not (san.[k] >= '1' && san.[k] <= '8' && san.[k - 1] >= 'a' && san.[k - 1] <= 'h') do k <- k - 1
  if k <= 0 then -1
  else
    let mutable side = stm
    match (TMoveOps.dictNameToNumber &side).TryGetValue(san.Substring(k - 1, 2)) with
    | true, sq -> int sq
    | _ -> -1

let private isCastlingText (san: string) =
  let i = textStart san
  i + 2 < san.Length && (san.[i] = 'O' || san.[i] = '0') && san.[i + 1] = '-'

/// One game's findings and the half-moves played before the first error (all of them when none).
/// `board` is reused from game to game.
let validateGame (board: Board) (game: PgnGame) : Finding list * int =
  let md = game.GameMetaData
  let finding ply (pm: PlyMove option) kind expected fen =
    { Game = game.GameNumber; Round = md.Round; White = md.White; Black = md.Black; Ply = ply
      Move = (match pm with
              | Some pm -> sprintf "%d%s %s" pm.MoveNumber (if pm.Color = "b" then "..." else ".") pm.San
              | None -> "")
      Kind = kind; Expected = expected; Fen = fen }
  let fen = if String.IsNullOrWhiteSpace md.Fen then game.Fen else md.Fen
  match (try board.ResetBoardStateFromFen fen; None with ex -> Some ex.Message) with
  | Some why -> [ finding 0 None BadStart $"cannot be set up ({why})" fen ], 0
  | None ->
      let findings = ResizeArray<Finding>()
      let buffer = buffers.Value
      let moves = game.Mainline
      let mutable ply = 0
      let mutable stopped = false
      while not stopped && ply < moves.Count do
        let pm = moves.[ply]
        let position = board.Position
        // the usual case: exactly one legal move to the written square whose standard SAN is what
        // is written. A SAN ends with the move's square, so no other move could be written so; only
        // these few get their SAN made, compared in place, the move list in the thread's buffer
        let count = board.GenerateMovesToBuffer(buffer.AsSpan())
        let castling = isCastlingText pm.San
        let target = if castling then -1 else targetSquare pm.San position.STM
        let mutable found = -1
        let mutable matches = 0
        for i in 0 .. count - 1 do
          let m = buffer.[i]
          if (castling && TMoveOps.isCastlingMove m) || (not castling && int m.To = target) then
            if sameSan (TMoveOps.getShortSanMoveFromTmoveN buffer count m position) pm.San then
              matches <- matches + 1
              found <- i
        if matches = 1 then
          let mutable move = buffer.[found]
          board.MakeMove(&move)
          ply <- ply + 1
        else
          // anything else is classified against every legal move
          let legal = board.GenerateMoves()
          let standard = legal |> Array.map (fun m -> m, TMoveOps.getShortSanMoveFromTmoveN legal legal.Length m position)
          let written = normalize pm.San
          let playOn (m: TMove) =
            let mutable move = m
            board.MakeMove(&move)
            ply <- ply + 1
          match standard |> Array.filter (fun (_, san) -> normalize san = written) with
          | [| m, _ |] -> playOn m
          | _ ->
              let fits = standard |> Array.filter (fun (_, san) -> bare san = bare pm.San)
              if fits.Length > 1 then
                findings.Add(finding (ply + 1) (Some pm) AmbiguousMove (fits |> Array.map snd |> String.concat " or ") (board.FEN()))
                stopped <- true
              else
                match TMoveOps.tryFindMoveBySanOrUci legal position.STM (fun _ -> true) pm.San with
                | Some m ->
                    let san = standard |> Array.find (fun (c, _) -> c = m) |> snd
                    findings.Add(finding (ply + 1) (Some pm) NonStandardSan san (board.FEN()))
                    playOn m
                | None ->
                    findings.Add(finding (ply + 1) (Some pm) IllegalMove "" (board.FEN()))
                    stopped <- true
      List.ofSeq findings, ply
