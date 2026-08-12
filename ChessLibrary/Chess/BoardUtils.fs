module ChessLibrary.BoardUtils

open System
open MoveTypes
open PositionTypes
open EngineTypes
open MoveGeneration
open RuntimeUtilities
open ChessLibrary.Chess

/// Generates all legal moves (captures + quiets) into the provided buffer and returns the
/// count — no slice allocation
let inline generateAllMovesToBufferCount (moveList: TMove[]) (position: Position byref) isFRC =
  let mutable index = 0
  let ctx = createLegalityContext &position
  generateLegalCaptures (moveList.AsSpan()) &index &position &ctx
  generateLegalQuiets (moveList.AsSpan()) &index &position isFRC &ctx
  index

/// Generates all legal moves (captures + quiets) into the provided buffer and returns the move slice
let inline generateAllMovesToBuffer (moveList: TMove[]) (position: Position byref) isFRC =
  let index = generateAllMovesToBufferCount moveList &position isFRC
  moveList[0..index-1]

let getSanNotationFromTMove (board: Board inref) (move: TMove) =
  let moveList = board.GenerateMoves()
  TMoveOps.getShortSanMoveFromTmove moveList move (board.Position)

let tryGetTMoveFromUciNotation (board: Board inref) (uciMove: string) =
  let moveList = board.GenerateMoves()
  TMoveOps.tryFindMoveByUciNotation moveList moveList.Length board.Position.STM uciMove

/// Matches a UCI move against the board's legal moves and converts it to short SAN
/// with a single move-list generation (the match-then-SAN call pattern otherwise
/// generates the list twice).
let tryGetMoveAndSanFromUci (board: Board inref) (uciMove: string) =
  let moveList = board.GenerateMoves()
  match TMoveOps.tryFindMoveByUciNotation moveList moveList.Length board.Position.STM uciMove with
  | Some tmove -> Some (tmove, TMoveOps.getShortSanMoveFromTmoveN moveList moveList.Length tmove board.Position)
  | None -> None

let getShortSanFromLongSan (board: Board inref) uciMove =
  match tryGetMoveAndSanFromUci &board uciMove with
  | Some (_, san) -> san
  | None -> ""

let getLongSanPVFromShortSanPV moveList (board: Board inref) (sanMoves: string seq) =
  let isFRC = board.IsFRC
  let mutable position = board.Position
  let ret = ResizeArray<string>(sanMoves |> Seq.length)
  for m in sanMoves do
    let moves = generateAllMovesToBuffer moveList &position isFRC
    // the generated list is legal-only, so the legality callback can no longer reject anything
    let islegal _ = true

    // Try SAN format first
    let moveResult =
      match TMoveOps.getTMoveFromShortSan m moves position.STM islegal with
      | Some tmove -> Some tmove
      | None ->
          // Try coordinate notation (some Winboard engines use this) — numeric matching
          // over the same legal list (previously round-tripped through a temp Board + FEN)
          TMoveOps.tryFindMoveByUciNotation moves moves.Length position.STM m

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

let getShortSanPVFromLongSanPVFast (moveList: TMove[]) (board: Board inref) (pv: string) =
  let isFRC = board.IsFRC
  let mutable position = board.Position
  let mutable plyCount = board.PlyCount
  let start = board.PlyCount
  let allMoves = pv.Split(' ')
  let ret = ResizeArray<string>(allMoves.Length)
  for move in allMoves do
    // count-based generation (no slice), numeric UCI matching (no per-candidate strings),
    // plain concatenation (no sprintf) — see MoveGenOptimizationPlan.md
    let count = generateAllMovesToBufferCount moveList &position isFRC
    match TMoveOps.tryFindMoveByUciNotation moveList count position.STM move with
    | Some tmove ->
      let moveNr = plyCount / 2 + 1
      let san = TMoveOps.getShortSanMoveFromTmoveN moveList count tmove position
      if san <> "" then
        if plyCount % 2 = 1 then
          // black move
          if plyCount = start then
            ret.Add(string moveNr + ".... " + san)
          else
            ret.Add(san)
        else
          ret.Add(string moveNr + "." + san)
        makeMove &tmove &position
        plyCount <- plyCount + 1
    | _ -> ()
  String.Join(" ", ret)

let makeShortSan (moves: NNValues seq) (board: Board inref) =
  // all NN moves share one position: generate the legal list once, not per move
  let moveList = board.GenerateMoves()
  let stm = board.Position.STM
  let pos = board.Position
  for nnMove in moves do
    match TMoveOps.tryFindMoveByUciNotation moveList moveList.Length stm nnMove.LANMove with
    | Some tmove ->
      nnMove.SANMove <- TMoveOps.getShortSanMoveFromTmoveN moveList moveList.Length tmove pos
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
      match TMoveOps.tryFindMoveByUciNotation moveList moveList.Length stm transformedLanMove with
      | Some _ ->
        nnMove.LANMove <- transformedLanMove
      | None -> ()

let makeRandomMove (rnd: Random) (board: Board inref) =
  // GenerateMoves is legal-only — no post-filter needed
  let moveList = board.GenerateMoves()
  if moveList.Length = 0 then
    printfn "No legal moves available"
    Unchecked.defaultof<TMove>
  else
    let mutable move = moveList.[rnd.Next(0, moveList.Length)]
    board.MakeMove &move
    move
