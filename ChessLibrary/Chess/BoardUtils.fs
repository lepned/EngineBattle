module ChessLibrary.BoardUtils

open System
open MoveTypes
open PositionTypes
open EngineTypes
open MoveGeneration
open RuntimeUtilities
open ChessLibrary.Chess

/// Generates all legal moves (captures + quiets) into the provided buffer and returns the move slice
let inline generateAllMovesToBuffer (moveList: TMove[]) (position: Position byref) isFRC =
  let mutable index = 0
  generateCaptures (moveList.AsSpan()) &index &position
  generateQuiets (moveList.AsSpan()) &index &position isFRC
  moveList[0..index-1]

let getSanNotationFromTMove (board: Board inref) (move: TMove) =
  let moveList = board.GenerateMoves()
  TMoveOps.getShortSanMoveFromTmove moveList move (board.Position)

let tryGetTMoveFromUciNotation (board: Board inref) (uciMove: string) =
  if uciMove.Length < 4 then
    None
  else
    let moveList = board.GenerateMoves()
    let stm = board.Position.STM
    moveList
    |> Array.tryFind (fun m -> TMoveOps.getUciNotation m stm = uciMove)

let getShortSanFromLongSan (board: Board inref) uciMove =
  match tryGetTMoveFromUciNotation &board uciMove with
  | Some move -> getSanNotationFromTMove &board move
  | None -> ""

let tryGetTMoveFromUciNotationSimple moveList (position: Position byref) (moveLong: string) =
  if moveLong.Length < 4 then
    None
  else
    let stm = position.STM
    moveList
    |> Array.tryFind (fun m -> TMoveOps.getUciNotation m stm = moveLong)

let getLongSanPVFromShortSanPV moveList (board: Board inref) (sanMoves: string seq) =
  let isFRC = board.IsFRC
  let mutable position = board.Position
  let ret = ResizeArray<string>(sanMoves |> Seq.length)
  for m in sanMoves do
    let moves = generateAllMovesToBuffer moveList &position isFRC
    let islegal move = BoardHelper.Illegal &move &position |> not

    // Try SAN format first
    let moveResult =
      match TMoveOps.getTMoveFromShortSan m moves position.STM islegal with
      | Some tmove -> Some tmove
      | None ->
          // Try coordinate notation (some Winboard engines use this)
          if m.Length >= 4 && m.Length <= 5 then
            // Create temporary board to test coordinate notation
            let tempBoard = Board()
            tempBoard.LoadFen(BoardHelper.posToFen position)
            tempBoard.IsFRC <- isFRC
            tryGetTMoveFromUciNotation &tempBoard m
          else
            None

    match moveResult with
    | Some tmove ->
      let moveStr = TMoveOps.getUciNotation tmove position.STM
      ret.Add(moveStr)
      makeMove &tmove &position
    | None ->
      // Stop processing PV on first error to avoid corrupted position
      ()
  // return the long san moves as a string
  String.concat " " (ret |> Seq.toArray)

let getShortSanPVFromLongSanPVFast moveList (board: Board inref) (pv: string) =
  let isFRC = board.IsFRC
  let mutable position = board.Position
  let mutable plyCount = board.PlyCount
  let start = board.PlyCount
  let allMoves = pv.Split(' ')
  let ret = ResizeArray<string>(allMoves.Length)
  for move in allMoves do
    let moves = generateAllMovesToBuffer moveList &position isFRC
    match tryGetTMoveFromUciNotationSimple moves &position move with
    | Some tmove ->
      let moveNr = plyCount / 2 + 1
      let san = TMoveOps.getShortSanMoveFromTmove moves tmove position
      if san <> "" then
        if plyCount % 2 = 1 then
          // black move
          if plyCount = start then
            ret.Add(sprintf "%d.... %s" moveNr san)
          else
            ret.Add(san)
        else
          let mStr = sprintf "%d.%s" moveNr san
          ret.Add(mStr)
        makeMove &tmove &position
        plyCount <- plyCount + 1
    | _ -> ()
  String.concat " " (ret |> Seq.toArray)

let makeShortSan (moves: NNValues seq) (board: Board inref) =
  for nnMove in moves do
    match tryGetTMoveFromUciNotation &board nnMove.LANMove with
    | Some tmove ->
      nnMove.SANMove <- getSanNotationFromTMove &board tmove
    | None ->
      if nnMove.LANMove.Trim() = "e1a1" then nnMove.SANMove <- "0-0-0"
      elif nnMove.LANMove.Trim() = "e8a8" then nnMove.SANMove <- "0-0-0"
      elif nnMove.LANMove.Trim() = "e1h1" then nnMove.SANMove <- "0-0"
      elif nnMove.LANMove.Trim() = "e8h8" then nnMove.SANMove <- "0-0"

      let transformedLanMove =
        let trim = nnMove.LANMove.Trim().ToLower()
        match trim with
        | "e1a1" -> "e1c1"
        | "e8a8" -> "e8c8"
        | "e1h1" -> "e1g1"
        | "e8h8" -> "e8g8"
        | _ -> trim
      match tryGetTMoveFromUciNotation &board transformedLanMove with
      | Some _ ->
        nnMove.LANMove <- transformedLanMove
      | None -> ()

let makeRandomMove (rnd: Random) (board: Board inref) =
  let position = board.Position
  let moveList =
    board.GenerateMoves()
    |> Array.filter (fun m -> BoardHelper.Illegal &m &position |> not)
  if moveList.Length = 0 then
    printfn "No legal moves available"
    Unchecked.defaultof<TMove>
  else
    let mutable move = moveList.[rnd.Next(0, moveList.Length)]
    board.MakeMove &move
    move
