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
      let rec play (moves: PlyMove list) ply =
        match moves with
        | [] -> ply
        | pm :: rest ->
            let legal = board.GenerateMoves()
            let position = board.Position
            let written = normalize pm.San
            let ply = ply + 1
            let playOn (m: TMove) =
              let mutable move = m
              board.MakeMove(&move)
              play rest ply
            // the usual case first: the move the matcher finds, written exactly as standard SAN. No
            // two legal moves share a standard SAN, so that is the move - unambiguous, nothing more to
            // ask - and only its SAN is made (making all of them took three quarters of the time)
            let quick =
              match TMoveOps.tryFindMoveBySanOrUci legal position.STM (fun _ -> true) pm.San with
              | Some m when normalize (TMoveOps.getShortSanMoveFromTmoveN legal legal.Length m position) = written -> Some m
              | _ -> None
            match quick with
            | Some m -> playOn m
            | None ->
            let standard = legal |> Array.map (fun m -> m, TMoveOps.getShortSanMoveFromTmoveN legal legal.Length m position)
            match standard |> Array.filter (fun (_, san) -> normalize san = written) with
            | [| m, _ |] -> playOn m
            | _ ->
                let fits = standard |> Array.filter (fun (_, san) -> bare san = bare pm.San)
                if fits.Length > 1 then
                  findings.Add(finding ply (Some pm) AmbiguousMove (fits |> Array.map snd |> String.concat " or ") (board.FEN()))
                  ply - 1
                else
                  match TMoveOps.tryFindMoveBySanOrUci legal position.STM (fun _ -> true) pm.San with
                  | Some m ->
                      let san = standard |> Array.find (fun (c, _) -> c = m) |> snd
                      findings.Add(finding ply (Some pm) NonStandardSan san (board.FEN()))
                      playOn m
                  | None ->
                      findings.Add(finding ply (Some pm) IllegalMove "" (board.FEN()))
                      ply - 1
      let played = play (List.ofSeq game.Mainline) 0
      List.ofSeq findings, played
