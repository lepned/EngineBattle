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

// ---------------------------------------------------------------------------
// Position insights for GUI overlays (pins / checks), derived from the movegen
// LegalityContext. Display-only helpers — not on any hot path.
// ---------------------------------------------------------------------------

/// An absolute pin: the enemy slider (Attacker), the single own piece between it and
/// the king (Pinned), and the King square. All squares are absolute board names
/// ("e4"), regardless of side to move (the QBB frame flip is handled internally).
type PinInfo = { Attacker: string; Pinned: string; King: string }

/// A hanging piece of one side: its square and the enemy squares attacking it
/// (direct attackers only; x-ray backers join during the exchange, not this list).
/// Hanging = a static exchange evaluation on the square wins material for the enemy:
/// both sides capture with their least valuable piece, either side may stop when
/// continuing loses material, x-ray attackers join as pieces leave the board, and a
/// king never captures a still-defended piece. An absolutely pinned defender only
/// counts along its pin ray. Remaining approximations: pins are not re-evaluated
/// mid-exchange, enemy attackers' own pins are ignored, and en-passant captures are
/// not considered.
type HangingInfo = { Square: string; Attackers: string[] }

/// Pins/checks for one color. CheckBlockSquares are the squares where a block or
/// capture resolves the check — populated only for single checks (a double check
/// cannot be blocked). KingDangerSquares/KingEscapeSquares cover the king's
/// NEIGHBORHOOD only (adjacent squares not occupied by own pieces): danger = the
/// king may not step there because the square is enemy-attacked (attack map computed
/// with the king removed from occupancy, so retreating along a check ray counts as
/// attacked); escape = the king may legally step or capture there.
type SideInsights =
  { King: string
    IsSideToMove: bool
    InCheck: bool
    Checkers: string[]
    CheckBlockSquares: string[]
    Pins: PinInfo[]
    KingDangerSquares: string[]
    KingEscapeSquares: string[]
    HangingPieces: HangingInfo[] }

type PositionInsights = { White: SideInsights; Black: SideInsights }

let private emptySideInsights isStm =
  { King = ""; IsSideToMove = isStm; InCheck = false
    Checkers = [||]; CheckBlockSquares = [||]; Pins = [||]
    KingDangerSquares = [||]; KingEscapeSquares = [||]; HangingPieces = [||] }

/// Piece value by QBB code (0 empty, 1 P, 2 N, 3 B, 4 R, 5 Q, 6 K). The king's 100
/// keeps a king attacker from ever winning a value comparison.
let private pieceValueByCode = [| 0; 1; 3; 3; 5; 9; 100 |]

/// Square and value of the least valuable piece in `set` (-1 when empty).
let private leastValuablePiece (position: Position inref) (set: uint64) =
  let mutable bestSq = -1
  let mutable bestVal = System.Int32.MaxValue
  let mutable s = set
  while s <> 0UL do
    let psq = int (QBBOperations.LSB s)
    let v = pieceValueByCode.[int (TPieceType.Piece(psq, &position))]
    if v < bestVal then
      bestVal <- v
      bestSq <- psq
    s <- QBBOperations.ClearLSB s
  struct (bestSq, bestVal)

/// Pieces of both colors attacking `sq` under `occNow`, with own-side defenders that
/// are absolutely pinned kept only when `sq` lies on their pin ray. (The pin filter is
/// static: pins are not re-evaluated as the exchange strips pieces — second-order.)
let private effectiveAttackersOn (position: Position inref) (ctxPinned: uint64) (ctxKingSq: int) (occNow: uint64) (sq: int) =
  let own = PositionOps.sideToMove &position
  let all = attackersTo sq occNow &position &&& occNow
  let mutable kept = all &&& ~~~own
  let mutable d = all &&& own
  while d <> 0UL do
    let dsq = int (QBBOperations.LSB d)
    let dBit = 1UL <<< dsq
    let effective =
      (dBit &&& ctxPinned) = 0UL ||
      (lineBB.[ctxKingSq * 64 + dsq] &&& (1UL <<< sq)) <> 0UL
    if effective then kept <- kept ||| dBit
    d <- QBBOperations.ClearLSB d
  kept

/// Static exchange evaluation on `sq` with the enemy capturing first: both sides
/// capture with their least valuable piece, x-ray attackers join as occupancy shrinks,
/// each side may stop when continuing loses material, and a king never captures while
/// the other side still attacks the square. Returns the enemy's best net gain
/// (<= 0 = no profitable capture exists).
let private seeOnSquare (position: Position inref) (ctxPinned: uint64) (ctxKingSq: int) (occ: uint64) (sq: int) (victimValue: int) =
  let own = PositionOps.sideToMove &position
  let opposing = PositionOps.opposing &position
  let atts0 = effectiveAttackersOn &position ctxPinned ctxKingSq occ sq
  let struct (firstSq, firstVal) = leastValuablePiece &position (atts0 &&& opposing)
  if firstSq < 0 then 0
  elif firstVal >= 100 && (atts0 &&& own) <> 0UL then 0   // king can't take a defended piece
  else
    let gains = Array.zeroCreate<int> 34
    gains.[0] <- victimValue
    let mutable seeOcc = occ ^^^ (1UL <<< firstSq)
    let mutable occupantValue = firstVal   // the piece now standing on sq
    let mutable fromSide = own
    let mutable depth = 0
    let mutable running = true
    while running && depth < 32 do
      let atts = effectiveAttackersOn &position ctxPinned ctxKingSq seeOcc sq
      let side = atts &&& fromSide
      if side = 0UL then running <- false
      else
        let struct (lvaSq, lvaVal) = leastValuablePiece &position side
        // a king may only capture when the opponent no longer attacks the square
        if lvaVal >= 100 && (atts &&& ~~~fromSide) <> 0UL then running <- false
        else
          depth <- depth + 1
          gains.[depth] <- occupantValue - gains.[depth - 1]
          occupantValue <- lvaVal
          seeOcc <- seeOcc ^^^ (1UL <<< lvaSq)
          fromSide <- if fromSide = opposing then own else opposing
    // Backward pass: at each step the side to move takes the better of stopping the
    // exchange or continuing it.
    let mutable i = depth
    while i > 0 do
      gains.[i - 1] <- - (max (- gains.[i - 1]) gains.[i])
      i <- i - 1
    gains.[0]

/// Insights for the side to move of the given position. The position's bitboards are
/// in the QBB side-to-move-relative frame; the STM-aware name dictionary maps frame
/// squares back to absolute names.
let private sideInsightsFromPosition (position: Position inref) isStm =
  let ctx = createLegalityContext &position
  if ctx.KingSq > 63 then emptySideInsights isStm
  else
    let names =
      if position.STM = PositionOps.WHITE then QBBOperations.squareNumberToNameDictWhite
      else QBBOperations.squareNumberToNameDictBlack
    let name (sq: int) = names.[sq]
    let squaresOf (bb: uint64) =
      let ret = ResizeArray<string>()
      let mutable b = bb
      while b <> 0UL do
        ret.Add(name (int (QBBOperations.LSB b)))
        b <- QBBOperations.ClearLSB b
      ret.ToArray()
    // Re-run the createLegalityContext sniper loop to recover attacker->pinned pairs
    // (ctx.Pinned is only the bitboard of pinned pieces, without their attackers)
    let occ = PositionOps.occupation &position
    let own = PositionOps.sideToMove &position
    let opposing = PositionOps.opposing &position
    let enemyRQ = PositionOps.queenOrRooks &position &&& opposing
    let enemyBQ = PositionOps.queenOrBishops &position &&& opposing
    let pins = ResizeArray<PinInfo>()
    let mutable snipers = (rookRays.[ctx.KingSq] &&& enemyRQ) ||| (bishopRays.[ctx.KingSq] &&& enemyBQ)
    while snipers <> 0UL do
      let sniperSq = int (QBBOperations.LSB snipers)
      let btw = betweenBB.[ctx.KingSq * 64 + sniperSq] &&& occ
      if btw <> 0UL && QBBOperations.ClearLSB btw = 0UL && (btw &&& own) <> 0UL then
        pins.Add { Attacker = name sniperSq; Pinned = name (int (QBBOperations.LSB btw)); King = name ctx.KingSq }
      snipers <- QBBOperations.ClearLSB snipers
    // Hanging pieces: own non-king pieces that are enemy-attacked and either have no
    // effective defender or are attacked by something cheaper. A pinned defender only
    // counts when the defended square lies on its pin ray (lineBB through king and
    // defender) — which also rules out pinned knights, whose attack squares are never
    // collinear with the knight.
    let hanging = ResizeArray<HangingInfo>()
    let mutable ownPieces = own &&& ~~~(1UL <<< ctx.KingSq)
    while ownPieces <> 0UL do
      let sq = int (QBBOperations.LSB ownPieces)
      let attackers = attackersTo sq occ &position &&& opposing
      if attackers <> 0UL then
        let victimValue = pieceValueByCode.[int (TPieceType.Piece(sq, &position))]
        if seeOnSquare &position ctx.Pinned ctx.KingSq occ sq victimValue > 0 then
          hanging.Add { Square = name sq; Attackers = squaresOf attackers }
      ownPieces <- QBBOperations.ClearLSB ownPieces
    let inCheck = ctx.Checkers <> 0UL
    let blockSquares =
      if inCheck && QBBOperations.ClearLSB ctx.Checkers = 0UL then
        squaresOf (betweenBB.[ctx.KingSq * 64 + int (QBBOperations.LSB ctx.Checkers)])
      else [||]
    // king neighborhood: own-occupied squares are excluded entirely (blocked either
    // way); the rest splits into attacked (danger) and steppable (escape, including
    // captures of undefended enemy pieces)
    let kingAdj = QBBOperations.KingDest.[ctx.KingSq] &&& ~~~own
    { King = name ctx.KingSq
      IsSideToMove = isStm
      InCheck = inCheck
      Checkers = squaresOf ctx.Checkers
      CheckBlockSquares = blockSquares
      Pins = pins.ToArray()
      KingDangerSquares = squaresOf (kingAdj &&& ctx.KingDanger)
      KingEscapeSquares = squaresOf (kingAdj &&& ~~~ctx.KingDanger)
      HangingPieces = hanging.ToArray() }

/// Computes pins and checks for BOTH colors of a FEN position, for GUI overlay
/// display. The non-moving side is evaluated on a side-swapped copy (pins and check
/// facts are properties of the position, not of whose turn it is). Throws on a
/// malformed FEN — callers rendering live user input should catch.
let getPositionInsights (fen: string) : PositionInsights =
  let mutable pos = BoardHelper.getPosFromFen (Some fen)
  let stmSide = sideInsightsFromPosition &pos true
  let mutable flipped = PositionOps.copy &pos
  PositionOps.changeSide &flipped
  let otherSide = sideInsightsFromPosition &flipped false
  if pos.STM = PositionOps.WHITE then { White = stmSide; Black = otherSide }
  else { White = otherSide; Black = stmSide }

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
