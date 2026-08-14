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

/// A fork delivered by one piece: it attacks two or more enemy pieces that are each
/// the king or already losable (in the enemy hanging set). The forker itself must not
/// be hanging — a capturable "forker" is no fork. Approximation: a target counts if it
/// hangs to anyone, not strictly to the forker.
type ForkInfo = { Forker: string; Targets: string[] }

/// Two enemy pieces on one ray of an own slider. Kind "skewer": the front piece is the
/// king or worth more than the back one (it must move, exposing the back); Kind
/// "relativePin": the front piece is worth less (moving it loses the back one). Equal
/// values are skipped (x-ray pressure, not a tactic) and back-is-king is the absolute
/// pin, already reported in Pins.
type SkewerInfo = { Attacker: string; Front: string; Back: string; Kind: string }

/// An own piece that is the sole effective defender of two or more attacked own
/// pieces, each of which would hang if the defender vanished.
type OverloadInfo = { Defender: string; Defends: string[] }

/// An own slider with exactly one own blocker on a ray toward an enemy target: moving
/// the blocker unveils either check (IsCheck) or a winning attack (the target hangs
/// once the blocker leaves, judged by SEE on a blocker-removed board). A pinned
/// blocker or a hanging slider disqualifies the motif.
type DiscoveredAttackInfo = { Slider: string; Blocker: string; Target: string; IsCheck: bool }

/// An ENEMY piece (never the king) that is the sole effective defender of enemy
/// material we attack, and that can itself be captured without material loss — take
/// the defender, then win what it defended (the Lichess "capture the defender" motif).
type RemovableDefenderInfo = { Defender: string; Defends: string[] }

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
    HangingPieces: HangingInfo[]
    Forks: ForkInfo[]
    Skewers: SkewerInfo[]
    OverloadedDefenders: OverloadInfo[]
    DiscoveredAttacks: DiscoveredAttackInfo[]
    RemovableDefenders: RemovableDefenderInfo[] }

type PositionInsights = { White: SideInsights; Black: SideInsights }

let private emptySideInsights isStm =
  { King = ""; IsSideToMove = isStm; InCheck = false
    Checkers = [||]; CheckBlockSquares = [||]; Pins = [||]
    KingDangerSquares = [||]; KingEscapeSquares = [||]; HangingPieces = [||]
    Forks = [||]; Skewers = [||]; OverloadedDefenders = [||]
    DiscoveredAttacks = [||]; RemovableDefenders = [||] }

/// Piece value by QBB code (0 empty, 1 P, 2 N, 3 B, 4 R, 5 Q, 6 K). The king's 100
/// keeps a king attacker from ever winning a value comparison.
let private pieceValueByCode = [| 0; 1; 3; 3; 5; 9; 100 |]

/// Fills in missing trailing FEN fields with defaults (side "w", castling "-",
/// en-passant "-", counters "0 1"), so common short forms like "<placement> w" are
/// parser-safe — the raw parser walks off the end of a FEN that stops after the side
/// field. Full FENs pass through untouched; extra fields are preserved for the
/// validator to flag.
let normalizeFen (fen: string) : string =
  if String.IsNullOrWhiteSpace fen then fen
  else
    let fields = fen.Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    if fields.Length >= 6 then String.Join(" ", fields)
    else
      let defaults = [| ""; "w"; "-"; "-"; "0"; "1" |]
      Array.init 6 (fun i -> if i < fields.Length then fields.[i] else defaults.[i])
      |> String.concat " "

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

/// Pseudo attack set of the piece on `frameSq` (either color; pawns give their capture
/// directions only, own-occupied targets are included). 0 for an empty square.
let private pieceAttacksBB (position: Position inref) (frameSq: int) =
  let bit = 1UL <<< frameSq
  let occ = PositionOps.occupation &position
  if occ &&& bit = 0UL then 0UL
  else
    let isStmPiece = (position.PM &&& bit) <> 0UL
    match int (TPieceType.Piece(frameSq, &position)) with
    | 1 ->
      if isStmPiece then ((bit <<< 9) &&& 0xFEFEFEFEFEFEFEFEUL) ||| ((bit <<< 7) &&& 0x7F7F7F7F7F7F7F7FUL)
      else ((bit >>> 9) &&& 0x7F7F7F7F7F7F7F7FUL) ||| ((bit >>> 7) &&& 0xFEFEFEFEFEFEFEFEUL)
    | 2 -> QBBOperations.KnightDest.[frameSq]
    | 3 -> GenBishop(frameSq, occ)
    | 4 -> GenRook(frameSq, occ)
    | 5 -> GenRook(frameSq, occ) ||| GenBishop(frameSq, occ)
    | 6 -> QBBOperations.KingDest.[frameSq]
    | _ -> 0UL

/// Bitboard of the side-to-move's hanging pieces (see HangingInfo for the rules).
let private hangingBB (position: Position inref) (ctxPinned: uint64) (ctxKingSq: int) =
  let occ = PositionOps.occupation &position
  let opposing = PositionOps.opposing &position
  let mutable result = 0UL
  let mutable ownPieces = PositionOps.sideToMove &position &&& ~~~(1UL <<< ctxKingSq)
  while ownPieces <> 0UL do
    let sq = int (QBBOperations.LSB ownPieces)
    if attackersTo sq occ &position &&& opposing <> 0UL then
      let victimValue = pieceValueByCode.[int (TPieceType.Piece(sq, &position))]
      if seeOnSquare &position ctxPinned ctxKingSq occ sq victimValue > 0 then
        result <- result ||| (1UL <<< sq)
    ownPieces <- QBBOperations.ClearLSB ownPieces
  result

/// (defender, defended) frame-square pairs where the defender is the SOLE effective
/// defender of an attacked, not-currently-hanging own piece that hangs the moment the
/// defender is lifted off the board. Shared by the overload pass (own side, filtered to
/// two or more) and the removable-defender pass (enemy side via a flipped copy).
let private soleDefenderPairs (position: Position inref) (ctxPinned: uint64) (ctxKingSq: int) (ownHangingSet: uint64) =
  let occ = PositionOps.occupation &position
  let own = PositionOps.sideToMove &position
  let opposing = PositionOps.opposing &position
  let pairs = ResizeArray<struct (int * int)>()
  let mutable tp = own &&& ~~~(1UL <<< ctxKingSq) &&& ~~~ownHangingSet
  while tp <> 0UL do
    let t = int (QBBOperations.LSB tp)
    if attackersTo t occ &position &&& opposing <> 0UL then
      let defenders = effectiveAttackersOn &position ctxPinned ctxKingSq occ t &&& own
      if System.Numerics.BitOperations.PopCount defenders = 1 then
        let d = int (QBBOperations.LSB defenders)
        let victimValue = pieceValueByCode.[int (TPieceType.Piece(t, &position))]
        if seeOnSquare &position ctxPinned ctxKingSq (occ ^^^ (1UL <<< d)) t victimValue > 0 then
          pairs.Add(struct (d, t))
    tp <- QBBOperations.ClearLSB tp
  pairs

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
    // Hanging pieces (rules in the HangingInfo doc) — computed as a bitboard first so
    // the tactics passes below can reuse the set.
    let ownHanging = hangingBB &position ctx.Pinned ctx.KingSq
    let hanging = ResizeArray<HangingInfo>()
    let mutable hb = ownHanging
    while hb <> 0UL do
      let sq = int (QBBOperations.LSB hb)
      hanging.Add { Square = name sq; Attackers = squaresOf (attackersTo sq occ &position &&& opposing) }
      hb <- QBBOperations.ClearLSB hb
    // Side-swapped copy: the enemy's own analysis (hanging, sole defenders, exchange
    // probes) runs in their frame; single-bit squares map back with ^^^ 56, whole
    // bitboards with RevBB.
    let mutable flipped = PositionOps.copy &position
    PositionOps.changeSide &flipped
    let ectx = createLegalityContext &flipped
    let enemyKingOk = ectx.KingSq <= 63
    let flippedOcc = PositionOps.occupation &flipped
    // Enemy hanging set mapped back into THIS frame: a fork target must be losable,
    // and "losable" IS the enemy-side hanging computation.
    let enemyHangingFlipped = if enemyKingOk then hangingBB &flipped ectx.Pinned ectx.KingSq else 0UL
    let enemyHanging = QBBOperations.RevBB enemyHangingFlipped
    let enemyKing = PositionOps.kings &position &&& opposing
    // Forks: a safe own piece attacking two or more targets that are the enemy king
    // or losable.
    let forks = ResizeArray<ForkInfo>()
    let mutable fp = own &&& ~~~ownHanging
    while fp <> 0UL do
      let p = int (QBBOperations.LSB fp)
      let targets = pieceAttacksBB &position p &&& (enemyHanging ||| enemyKing)
      if System.Numerics.BitOperations.PopCount targets >= 2 then
        forks.Add { Forker = name p; Targets = squaresOf targets }
      fp <- QBBOperations.ClearLSB fp
    // Skewers / relative pins: own slider, enemy front piece, enemy piece revealed
    // behind it on the same ray (remove the front blocker and re-probe; only its ray
    // continuation becomes visible).
    let skewers = ResizeArray<SkewerInfo>()
    let mutable sliders = (PositionOps.rooks &position ||| PositionOps.bishops &position ||| PositionOps.queens &position) &&& own
    while sliders <> 0UL do
      let x = int (QBBOperations.LSB sliders)
      let sliderCode = int (TPieceType.Piece(x, &position))
      for geometry in 0 .. 1 do
        let applies =
          (geometry = 0 && (sliderCode = 4 || sliderCode = 5)) ||
          (geometry = 1 && (sliderCode = 3 || sliderCode = 5))
        if applies then
          let att = if geometry = 0 then GenRook(x, occ) else GenBishop(x, occ)
          let mutable fs = att &&& opposing
          while fs <> 0UL do
            let f = int (QBBOperations.LSB fs)
            let fBit = 1UL <<< f
            let revealed = (if geometry = 0 then GenRook(x, occ ^^^ fBit) else GenBishop(x, occ ^^^ fBit)) &&& ~~~att
            let bBit = revealed &&& (occ ^^^ fBit) &&& opposing
            if bBit <> 0UL then
              let b = int (QBBOperations.LSB bBit)
              let fVal = pieceValueByCode.[int (TPieceType.Piece(f, &position))]
              let bVal = pieceValueByCode.[int (TPieceType.Piece(b, &position))]
              let fIsKing = (fBit &&& enemyKing) <> 0UL
              let bIsKing = (bBit &&& enemyKing) <> 0UL
              if fIsKing || fVal > bVal then
                skewers.Add { Attacker = name x; Front = name f; Back = name b; Kind = "skewer" }
              elif fVal < bVal && not bIsKing then
                skewers.Add { Attacker = name x; Front = name f; Back = name b; Kind = "relativePin" }
            fs <- QBBOperations.ClearLSB fs
      sliders <- QBBOperations.ClearLSB sliders
    // Overloaded defenders: sole effective defender of two or more attacked-but-safe
    // own pieces, each of which hangs once the defender is lifted off the board.
    let overloads = ResizeArray<OverloadInfo>()
    let defenderMap = System.Collections.Generic.Dictionary<int, ResizeArray<int>>()
    for struct (d, t) in soleDefenderPairs &position ctx.Pinned ctx.KingSq ownHanging do
      match defenderMap.TryGetValue d with
      | true, lst -> lst.Add t
      | false, _ ->
          let lst = ResizeArray<int>()
          lst.Add t
          defenderMap.[d] <- lst
    for kvp in defenderMap do
      if kvp.Value.Count >= 2 then
        overloads.Add { Defender = name kvp.Key; Defends = kvp.Value |> Seq.map name |> Seq.toArray }
    // Discovered attacks: own slider, exactly one OWN blocker on a ray, enemy piece
    // revealed behind it — worth showing when the reveal is check or when the target
    // hangs on a blocker-removed board (probed in the enemy frame). A pinned blocker
    // cannot unveil and a hanging slider makes the motif fake.
    let discovered = ResizeArray<DiscoveredAttackInfo>()
    if enemyKingOk then
      let mutable dsliders =
        (PositionOps.rooks &position ||| PositionOps.bishops &position ||| PositionOps.queens &position)
        &&& own &&& ~~~ownHanging
      while dsliders <> 0UL do
        let x = int (QBBOperations.LSB dsliders)
        let sliderCode = int (TPieceType.Piece(x, &position))
        for geometry in 0 .. 1 do
          let applies =
            (geometry = 0 && (sliderCode = 4 || sliderCode = 5)) ||
            (geometry = 1 && (sliderCode = 3 || sliderCode = 5))
          if applies then
            let att = if geometry = 0 then GenRook(x, occ) else GenBishop(x, occ)
            let mutable bs = att &&& own &&& ~~~ctx.Pinned
            while bs <> 0UL do
              let b = int (QBBOperations.LSB bs)
              let bBit = 1UL <<< b
              let revealed = (if geometry = 0 then GenRook(x, occ ^^^ bBit) else GenBishop(x, occ ^^^ bBit)) &&& ~~~att
              let tBit = revealed &&& (occ ^^^ bBit) &&& opposing
              if tBit <> 0UL then
                let t = int (QBBOperations.LSB tBit)
                let isCheck = (tBit &&& enemyKing) <> 0UL
                let worthIt =
                  isCheck ||
                  (let victimValue = pieceValueByCode.[int (TPieceType.Piece(t, &position))]
                   seeOnSquare &flipped ectx.Pinned ectx.KingSq (flippedOcc &&& ~~~(1UL <<< (b ^^^ 56))) (t ^^^ 56) victimValue > 0)
                if worthIt then
                  discovered.Add { Slider = name x; Blocker = name b; Target = name t; IsCheck = isCheck }
              bs <- QBBOperations.ClearLSB bs
        dsliders <- QBBOperations.ClearLSB dsliders
    // Removable defenders (capture-the-defender): the enemy's sole-defender pairs,
    // kept when the defender is not their king, we attack it, and taking it costs
    // nothing by SEE (a free capture or an equal trade that wins the defended piece).
    let removables = ResizeArray<RemovableDefenderInfo>()
    if enemyKingOk then
      let removableMap = System.Collections.Generic.Dictionary<int, ResizeArray<int>>()
      for struct (dF, tF) in soleDefenderPairs &flipped ectx.Pinned ectx.KingSq enemyHangingFlipped do
        if dF <> ectx.KingSq then
          match removableMap.TryGetValue dF with
          | true, lst -> lst.Add tF
          | false, _ ->
              let lst = ResizeArray<int>()
              lst.Add tF
              removableMap.[dF] <- lst
      for kvp in removableMap do
        let dF = kvp.Key
        let d = dF ^^^ 56
        let ourAttackers = attackersTo d occ &position &&& own
        if ourAttackers <> 0UL then
          let struct (_, cheapestVal) = leastValuablePiece &position ourAttackers
          let theirDefenders =
            effectiveAttackersOn &flipped ectx.Pinned ectx.KingSq flippedOcc dF
            &&& PositionOps.sideToMove &flipped
          // our king cannot start the capture if the defender is itself defended
          let canCapture = not (cheapestVal >= 100 && theirDefenders <> 0UL)
          if canCapture then
            let dValue = pieceValueByCode.[int (TPieceType.Piece(d, &position))]
            if seeOnSquare &flipped ectx.Pinned ectx.KingSq flippedOcc dF dValue >= 0 then
              removables.Add
                { Defender = name d
                  Defends = kvp.Value |> Seq.map (fun tF -> name (tF ^^^ 56)) |> Seq.toArray }
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
      HangingPieces = hanging.ToArray()
      Forks = forks.ToArray()
      Skewers = skewers.ToArray()
      OverloadedDefenders = overloads.ToArray()
      DiscoveredAttacks = discovered.ToArray()
      RemovableDefenders = removables.ToArray() }

/// Computes pins and checks for BOTH colors of a FEN position, for GUI overlay
/// display. The non-moving side is evaluated on a side-swapped copy (pins and check
/// facts are properties of the position, not of whose turn it is). Throws on a
/// malformed FEN — callers rendering live user input should catch.
let getPositionInsights (fen: string) : PositionInsights =
  let fen = normalizeFen fen
  let mutable pos = BoardHelper.getPosFromFen (Some fen)
  let stmSide = sideInsightsFromPosition &pos true
  let mutable flipped = PositionOps.copy &pos
  PositionOps.changeSide &flipped
  let otherSide = sideInsightsFromPosition &flipped false
  if pos.STM = PositionOps.WHITE then { White = stmSide; Black = otherSide }
  else { White = otherSide; Black = stmSide }

/// Outcome of moving a piece to one of its legal destinations: Net = captured material
/// minus the enemy's best static exchange on the destination after the move
/// (> 0 wins material, 0 safe/even, < 0 the moved piece can be won there).
type SafeDestination = { Dest: string; Net: int; IsCapture: bool }

/// Safe-square preview for the piece on `fromSquare` (absolute name, e.g. "g1"): every
/// legal destination with its static material outcome. Castling is always 0 and legal
/// king moves never land on attacked squares, so both skip the exchange. Promotions are
/// deduplicated to one entry per destination (the queen promotion, generated first) and
/// use the promoted piece's value as the exchange victim. The pre-move pin context is
/// reused for the post-move exchange (approximation). Empty array when the square has
/// no legal moves; throws on a malformed FEN.
let getSafeDestinations (fen: string) (fromSquare: string) : SafeDestination[] =
  let fen = normalizeFen fen
  let board = Board()
  board.LoadFen fen
  let mutable pos = board.Position
  let ctx = createLegalityContext &pos
  let occ = PositionOps.occupation &pos
  let moves = board.GenerateMoves()
  let results = ResizeArray<SafeDestination>()
  let seenDests = System.Collections.Generic.HashSet<string>()
  for i in 0 .. moves.Length - 1 do
    let mutable mv = moves.[i]
    let uci = TMoveOps.moveToStr &mv pos.STM
    if uci.StartsWith fromSquare then
      let dest = uci.Substring(2, 2)
      if seenDests.Add dest then
        let fromSq = int mv.From
        let toSq = int mv.To
        let isCastle = (mv.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
        let isEp = (mv.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
        let isCapture = isEp || (mv.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
        let moverCode = int (TPieceType.Piece(fromSq, &pos))
        let capturedValue =
          if isEp then 1
          elif isCapture then pieceValueByCode.[int (TPieceType.Piece(toSq, &pos))]
          else 0
        let net =
          if isCastle then 0
          elif moverCode = int TPieceType.KING then capturedValue
          else
            let victimValue =
              if (mv.MoveType &&& TPieceType.PROMO) <> TPieceType.EMPTY then pieceValueByCode.[int mv.Promotion]
              else pieceValueByCode.[moverCode]
            let occAfter =
              let moved = (occ ^^^ (1UL <<< fromSq)) ||| (1UL <<< toSq)
              if isEp then moved &&& ~~~(1UL <<< (toSq - 8)) else moved
            // The enemy only enters the exchange when it profits — clamp at 0.
            capturedValue - max (seeOnSquare &pos ctx.Pinned ctx.KingSq occAfter toSq victimValue) 0
        results.Add { Dest = dest; Net = net; IsCapture = isCapture }
  results.ToArray()

// ---------------------------------------------------------------------------
// Position queries — python-chess-style per-square API (attackers/attacks/pin/
// ray/between) with absolute square names, for validator and tooling use.
// FEN-based and stateless; invalid square names throw ArgumentException.
// ---------------------------------------------------------------------------

/// Squares of each color's pieces attacking one square.
type SquareAttackers = { White: string[]; Black: string[] }

let private absSquareIndex (square: string) =
  if isNull square || square.Length <> 2 then invalidArg "square" (sprintf "Invalid square name '%s'" square)
  let file = int square.[0] - int 'a'
  let rank = int square.[1] - int '1'
  if file < 0 || file > 7 || rank < 0 || rank > 7 then invalidArg "square" (sprintf "Invalid square name '%s'" square)
  rank * 8 + file

/// Frame bitboard -> absolute names, using the dictionary matching the frame's STM.
let private squareNamesIn (isWhiteFrame: bool) (bb: uint64) =
  let names =
    if isWhiteFrame then QBBOperations.squareNumberToNameDictWhite
    else QBBOperations.squareNumberToNameDictBlack
  let ret = ResizeArray<string>()
  let mutable b = bb
  while b <> 0UL do
    ret.Add(names.[int (QBBOperations.LSB b)])
    b <- QBBOperations.ClearLSB b
  ret.ToArray()

/// Pieces of each color attacking `square`. Includes defenders of an occupied square
/// (the piece standing on it is never its own attacker).
let attackersOf (fen: string) (square: string) : SquareAttackers =
  let fen = normalizeFen fen
  let absSq = absSquareIndex square
  let mutable pos = BoardHelper.getPosFromFen (Some fen)
  let isWhiteFrame = pos.STM = PositionOps.WHITE
  let frameSq = QBBOperations.AbsSq(absSq, int pos.STM)
  let occ = PositionOps.occupation &pos
  let atts = attackersTo frameSq occ &pos
  let stmSide = squareNamesIn isWhiteFrame (atts &&& pos.PM)
  let oppSide = squareNamesIn isWhiteFrame (atts &&& ~~~pos.PM)
  if isWhiteFrame then { White = stmSide; Black = oppSide }
  else { White = oppSide; Black = stmSide }

/// The pseudo attack set of the piece on `square` (empty array for an empty square).
/// Attack semantics, not move semantics: own-occupied targets are included, pawn
/// pushes are not (only the two capture directions) — python-chess `attacks()` parity.
let attacksFrom (fen: string) (square: string) : string[] =
  let fen = normalizeFen fen
  let absSq = absSquareIndex square
  let mutable pos = BoardHelper.getPosFromFen (Some fen)
  let isWhiteFrame = pos.STM = PositionOps.WHITE
  let frameSq = QBBOperations.AbsSq(absSq, int pos.STM)
  squareNamesIn isWhiteFrame (pieceAttacksBB &pos frameSq)

/// Some ray (full king-pinner line, both endpoints included) when the piece on
/// `square` is absolutely pinned to its own king; None otherwise or for empty squares.
let private tryPinRay (fen: string) (square: string) : string[] option =
  let fen = normalizeFen fen
  let absSq = absSquareIndex square
  let mutable pos = BoardHelper.getPosFromFen (Some fen)
  let frameSq0 = QBBOperations.AbsSq(absSq, int pos.STM)
  let bit0 = 1UL <<< frameSq0
  if (PositionOps.occupation &pos &&& bit0) = 0UL then None
  else
    // Evaluate in the frame where the piece's own side is to move (pins are relative
    // to the piece's own king) — same side-swap as getPositionInsights.
    let isStmPiece = (pos.PM &&& bit0) <> 0UL
    let mutable work = PositionOps.copy &pos
    if not isStmPiece then PositionOps.changeSide &work
    let frameSq = if isStmPiece then frameSq0 else frameSq0 ^^^ 56
    let ctx = createLegalityContext &work
    if ctx.KingSq > 63 || (ctx.Pinned &&& (1UL <<< frameSq)) = 0UL then None
    else
      let occ = PositionOps.occupation &work
      let opposing = PositionOps.opposing &work
      let enemyRQ = PositionOps.queenOrRooks &work &&& opposing
      let enemyBQ = PositionOps.queenOrBishops &work &&& opposing
      let mutable snipers = (rookRays.[ctx.KingSq] &&& enemyRQ) ||| (bishopRays.[ctx.KingSq] &&& enemyBQ)
      let mutable rayBB = 0UL
      while snipers <> 0UL && rayBB = 0UL do
        let sniperSq = int (QBBOperations.LSB snipers)
        if betweenBB.[ctx.KingSq * 64 + sniperSq] &&& occ = (1UL <<< frameSq) then
          rayBB <- lineBB.[ctx.KingSq * 64 + sniperSq]
        snipers <- QBBOperations.ClearLSB snipers
      if rayBB = 0UL then None
      else Some (squareNamesIn (work.STM = PositionOps.WHITE) rayBB)

/// True when the piece on `square` is absolutely pinned to its own king.
let isPinned (fen: string) (square: string) : bool =
  tryPinRay fen square |> Option.isSome

/// The full king-pinner line (both endpoints included) restricting an absolutely
/// pinned piece; empty array when the piece is not pinned (python-chess pin() parity).
let pinRay (fen: string) (square: string) : string[] =
  tryPinRay fen square |> Option.defaultValue [||]

/// FEN placement field to a 64-cell board array: index = rank * 8 + file with a1 = 0,
/// piece chars as in FEN, '\000' = empty. Best-effort on malformed input (extra files
/// and ranks are dropped) — gate on validateFen where it matters.
let boardOfFen (fen: string) : char[] =
  let b = Array.zeroCreate<char> 64
  if not (String.IsNullOrWhiteSpace fen) then
    let ranks = fen.Trim().Split(' ').[0].Split('/')
    for i in 0 .. min 7 (ranks.Length - 1) do
      let rank = 7 - i
      let mutable file = 0
      for c in ranks.[i] do
        if Char.IsDigit c then file <- file + (int c - int '0')
        elif file < 8 then
          b.[rank * 8 + file] <- c
          file <- file + 1
  b

/// Builds a FEN from editor-style state (the inverse of boardOfFen; '\000' or ' ' =
/// empty square). Empty castling/en-passant normalize to "-". No legality checks of
/// its own — run validateFen on the result.
let buildFen (board: char[]) (stm: char) (castling: string) (enPassant: string) (halfmove: int) (fullmove: int) : string =
  let sb = System.Text.StringBuilder()
  for rank in 7 .. -1 .. 0 do
    let mutable empties = 0
    for file in 0 .. 7 do
      let c = board.[rank * 8 + file]
      if c = '\000' || c = ' ' then
        empties <- empties + 1
      else
        if empties > 0 then
          sb.Append(empties) |> ignore
          empties <- 0
        sb.Append(c) |> ignore
    if empties > 0 then sb.Append(empties) |> ignore
    if rank > 0 then sb.Append('/') |> ignore
  let castling = if String.IsNullOrWhiteSpace castling then "-" else castling
  let ep = if String.IsNullOrWhiteSpace enPassant then "-" else enPassant
  sprintf "%s %c %s %s %d %d" (sb.ToString()) stm castling ep halfmove fullmove

/// En-passant target squares consistent with the position: the target and the pawn's
/// origin empty, the double-stepped enemy pawn on its landing rank. The board editor's
/// dropdown, validateFen and the query tooling share this definition.
let epCandidates (fen: string) : string[] =
  let fields = (normalizeFen fen).Split(' ')
  let b = boardOfFen fen
  let stm = if fields.Length > 1 then fields.[1] else "w"
  [| for f in 0 .. 7 do
       if stm = "w" then
         if b.[5 * 8 + f] = '\000' && b.[6 * 8 + f] = '\000' && b.[4 * 8 + f] = 'p' then
           yield sprintf "%c6" (char (int 'a' + f))
       else
         if b.[2 * 8 + f] = '\000' && b.[1 * 8 + f] = '\000' && b.[3 * 8 + f] = 'P' then
           yield sprintf "%c3" (char (int 'a' + f)) |]

/// Structured FEN validation result: all problems found, not just the first.
type FenValidation = { IsValid: bool; Errors: string[] }

/// Validates a FEN without ever throwing or building a position from garbage — the
/// parser (`BoardHelper.getPosFromFen`) has no checks of its own and can silently
/// produce corrupt positions, so callers taking user input should gate on this first.
/// Structural checks are pure string work; only a structurally clean FEN is parsed for
/// the one semantic rule (the side not to move may not be in check). 4-field EPD-style
/// FENs (no counters) are accepted, as elsewhere in EB.
let validateFen (fen: string) : FenValidation =
  let errors = ResizeArray<string>()
  if String.IsNullOrWhiteSpace fen then errors.Add "FEN is empty"
  else
    // Short forms are normalized (missing trailing fields get defaults) before the
    // checks, so "<placement> w" validates; only over-long field counts are errors.
    let fields = (normalizeFen fen).Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    if fields.Length > 6 then
      errors.Add (sprintf "expected at most 6 FEN fields, got %d" fields.Length)
    if fields.Length >= 1 then
      let ranks = fields.[0].Split('/')
      if ranks.Length <> 8 then errors.Add (sprintf "expected 8 ranks, got %d" ranks.Length)
      let mutable whiteKings = 0
      let mutable blackKings = 0
      for i in 0 .. min 7 (ranks.Length - 1) do
        let rankNo = 8 - i
        let mutable files = 0
        let mutable prevDigit = false
        for c in ranks.[i] do
          if Char.IsDigit c then
            let d = int c - int '0'
            if d < 1 || d > 8 then errors.Add (sprintf "invalid digit '%c' in rank %d" c rankNo)
            elif prevDigit then errors.Add (sprintf "consecutive digits in rank %d" rankNo)
            files <- files + d
            prevDigit <- true
          else
            prevDigit <- false
            match Char.ToLowerInvariant c with
            | 'p' | 'n' | 'b' | 'r' | 'q' | 'k' ->
              if c = 'K' then whiteKings <- whiteKings + 1
              elif c = 'k' then blackKings <- blackKings + 1
              if Char.ToLowerInvariant c = 'p' && (rankNo = 1 || rankNo = 8) then
                errors.Add (sprintf "pawn on rank %d" rankNo)
              files <- files + 1
            | _ -> errors.Add (sprintf "invalid piece character '%c' in rank %d" c rankNo)
        if files <> 8 then errors.Add (sprintf "rank %d has %d files" rankNo files)
      if ranks.Length = 8 then
        if whiteKings <> 1 then errors.Add (sprintf "white has %d kings" whiteKings)
        if blackKings <> 1 then errors.Add (sprintf "black has %d kings" blackKings)
    if fields.Length >= 2 then
      match fields.[1] with
      | "w" | "b" -> ()
      | s -> errors.Add (sprintf "side to move must be 'w' or 'b', got '%s'" s)
    if fields.Length >= 3 && fields.[2] <> "-" then
      // FRC rights letters (file letters) are legal alongside KQkq
      let castleOk =
        fields.[2]
        |> Seq.forall (fun ch ->
            "KQkq".Contains ch || (ch >= 'A' && ch <= 'H') || (ch >= 'a' && ch <= 'h'))
      if not castleOk then errors.Add (sprintf "invalid castling field '%s'" fields.[2])
    if fields.Length >= 4 && fields.[3] <> "-" then
      let ep = fields.[3]
      let epOk = ep.Length = 2 && ep.[0] >= 'a' && ep.[0] <= 'h' && (ep.[1] = '3' || ep.[1] = '6')
      if not epOk then errors.Add (sprintf "invalid en-passant field '%s'" ep)
    if fields.Length >= 5 then
      match Int32.TryParse fields.[4] with
      | true, v when v >= 0 -> ()
      | _ -> errors.Add (sprintf "halfmove counter '%s' is not a non-negative number" fields.[4])
    if fields.Length >= 6 then
      match Int32.TryParse fields.[5] with
      | true, v when v >= 0 -> ()
      | _ -> errors.Add (sprintf "fullmove counter '%s' is not a non-negative number" fields.[5])
    if errors.Count = 0 then
      // Semantic: a set en-passant square must be supported by the pawns (target and
      // origin empty, double-stepped pawn on its landing rank) — python-chess parity;
      // a well-formed but impossible ep field is a classic paste error that engines
      // handle inconsistently.
      let joined = String.Join(" ", fields)
      if fields.Length >= 4 && fields.[3] <> "-"
         && not (epCandidates joined |> Array.contains fields.[3]) then
        errors.Add (sprintf "en-passant square '%s' is not consistent with the position" fields.[3])
      try
        let mutable pos = BoardHelper.getPosFromFen (Some joined)
        let mutable flipped = PositionOps.copy &pos
        PositionOps.changeSide &flipped
        if InCheck &flipped <> 0UL then
          errors.Add "side not to move is in check"
      with ex -> errors.Add (sprintf "FEN failed to parse: %s" ex.Message)
  { IsValid = errors.Count = 0; Errors = errors.ToArray() }

// ---------------------------------------------------------------------------
// Board editing — stateless FEN-in/FEN-out piece manipulation for the editor
// and validator tooling (python-chess set_piece_at/remove_piece_at parity).
// ---------------------------------------------------------------------------

/// The piece on `square` as its FEN char; '\000' for an empty square.
let pieceAt (fen: string) (square: string) : char =
  (boardOfFen fen).[absSquareIndex square]

/// The KQkq subset of castling rights the piece placement supports (king and rook on
/// their standard home squares). Standard chess only — FRC file letters never appear.
let availableCastlingRights (fen: string) : string =
  let b = boardOfFen fen
  let has sq p = b.[absSquareIndex sq] = p
  let sb = System.Text.StringBuilder()
  if has "e1" 'K' && has "h1" 'R' then sb.Append 'K' |> ignore
  if has "e1" 'K' && has "a1" 'R' then sb.Append 'Q' |> ignore
  if has "e8" 'k' && has "h8" 'r' then sb.Append 'k' |> ignore
  if has "e8" 'k' && has "a8" 'r' then sb.Append 'q' |> ignore
  sb.ToString()

/// Returns the FEN with `piece` placed on `square` ('\000' or ' ' clears it), pruning
/// castling rights whose king/rook no longer stand on the required squares (KQkq and
/// FRC file letters alike) and an en-passant square the pawns no longer support.
/// Rights are only pruned, never added back — placing a rook on h1 again does not
/// restore K; set rights explicitly in the FEN. No legality checks beyond the piece
/// char — gate the result on validateFen where it matters.
let setPieceAt (fen: string) (square: string) (piece: char) : string =
  if not (piece = '\000' || piece = ' ' || "PNBRQKpnbrqk".IndexOf piece >= 0) then
    invalidArg "piece" (sprintf "Invalid piece char '%c'" piece)
  let fields = (normalizeFen fen).Split(' ')
  let b = boardOfFen fen
  b.[absSquareIndex square] <- (if piece = ' ' then '\000' else piece)
  let hasAt idx p = b.[idx] = p
  // Ranks 1/8 are indices 0..7 and 56..63 (a1 = 0 convention of boardOfFen/buildFen).
  let whiteKingOnRank1 = { 0 .. 7 } |> Seq.exists (fun f -> b.[f] = 'K')
  let blackKingOnRank8 = { 0 .. 7 } |> Seq.exists (fun f -> b.[56 + f] = 'k')
  let keepRight (c: char) =
    match c with
    | 'K' -> hasAt 4 'K' && hasAt 7 'R'
    | 'Q' -> hasAt 4 'K' && hasAt 0 'R'
    | 'k' -> hasAt 60 'k' && hasAt 63 'r'
    | 'q' -> hasAt 60 'k' && hasAt 56 'r'
    | c when c >= 'A' && c <= 'H' -> whiteKingOnRank1 && hasAt (int c - int 'A') 'R'
    | c when c >= 'a' && c <= 'h' -> blackKingOnRank8 && hasAt (56 + int c - int 'a') 'r'
    | _ -> false
  let castling =
    if fields.[2] = "-" then "-"
    else
      let kept = fields.[2] |> Seq.filter keepRight |> Seq.toArray |> System.String
      if kept.Length = 0 then "-" else kept
  let parseOr fallback (s: string) =
    match Int32.TryParse s with
    | true, v -> v
    | _ -> fallback
  let half, full = parseOr 0 fields.[4], parseOr 1 fields.[5]
  let fenNoEp = buildFen b fields.[1].[0] castling "-" half full
  if fields.[3] <> "-" && epCandidates fenNoEp |> Array.contains fields.[3] then
    buildFen b fields.[1].[0] castling fields.[3] half full
  else fenNoEp

/// Returns the FEN with `square` cleared (same pruning rules as setPieceAt).
let removePieceAt (fen: string) (square: string) : string =
  setPieceAt fen square '\000'

/// A legal move in both notations plus per-move predicates (python-chess parity:
/// gives_check/is_capture/is_castling/is_en_passant/is_zeroing/is_irreversible).
/// SAN castling uses EB's "0-0"/"0-0-0" spelling. GivesCheck includes discovered
/// checks — the move is made on a scratch copy and the resulting position probed.
/// IsZeroing = pawn move or capture (resets the halfmove clock). IsIrreversible =
/// zeroing, or touches a square tied to a current castling right, or a legal en
/// passant exists in the position (playing anything cedes it) — same false-negative
/// as python-chess for forced lines.
type MoveNotation =
  { Uci: string
    San: string
    IsCapture: bool
    IsCastling: bool
    IsEnPassant: bool
    GivesCheck: bool
    IsZeroing: bool
    IsIrreversible: bool }

/// Position status: "checkmate" | "stalemate" | "check" | "ok", plus a dead-position
/// test. InsufficientMaterial uses the standard approximation — kings only, kings plus
/// one minor piece, or kings plus bishops all on one square color — NOT python-chess's
/// per-side helpmate rules (notably K+N vs K+N is NOT insufficient here).
/// FiftyMoveDraw = a draw CAN be claimed (halfmove clock >= 100 and the game is not
/// already over); SeventyFiveMoveDraw = automatic draw (>= 150). Both are pure
/// halfmove-counter checks — repetition needs history and lives in adjudication.
type PositionStatus =
  { Status: string
    InsufficientMaterial: bool
    FiftyMoveDraw: bool
    SeventyFiveMoveDraw: bool }

/// All legal moves of the position as UCI + SAN pairs with per-move predicates.
let legalMovesOf (fen: string) : MoveNotation[] =
  let fen = normalizeFen fen
  let board = Board()
  board.LoadFen fen
  let mutable pos = board.Position
  let moves = board.GenerateMoves()
  // GetLegalMoves re-runs the same deterministic generator on the unchanged board, so
  // its SAN sequence is index-aligned with `moves`; the uci equality below guards the
  // assumption against future reordering.
  let notations = board.GetLegalMoves() |> Seq.toArray
  // Irreversibility inputs: the squares tied to a current castling right (moving from
  // or capturing onto one destroys the right). KQkq map to the actual king square and
  // the classic corner rooks; FRC file letters map to that file's back-rank rook.
  let boardArr = boardOfFen fen
  let castlingField = fen.Split(' ').[2]
  let rightSquares =
    if castlingField = "-" then Set.empty
    else
      let whiteKing = System.Array.IndexOf(boardArr, 'K')
      let blackKing = System.Array.IndexOf(boardArr, 'k')
      castlingField
      |> Seq.collect (fun c ->
          match c with
          | 'K' -> [ whiteKing; 7 ]
          | 'Q' -> [ whiteKing; 0 ]
          | 'k' -> [ blackKing; 63 ]
          | 'q' -> [ blackKing; 56 ]
          | c when c >= 'A' && c <= 'H' -> [ whiteKing; int c - int 'A' ]
          | c when c >= 'a' && c <= 'h' -> [ blackKing; 56 + int c - int 'a' ]
          | _ -> [])
      |> Seq.filter (fun i -> i >= 0)
      |> Set.ofSeq
  let sqIdx (uci: string) (o: int) = (int uci.[o + 1] - int '1') * 8 + (int uci.[o] - int 'a')
  let result =
    Array.init moves.Length (fun i ->
      let mutable mv = moves.[i]
      let (uci, san) = notations.[i]
      if uci <> TMoveOps.moveToStr &mv pos.STM then
        invalidOp "legal move enumeration order mismatch between GenerateMoves and GetLegalMoves"
      let isEp = (mv.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
      let isCapture = isEp || (mv.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
      let fromIdx = sqIdx uci 0
      let isZeroing = isCapture || boardArr.[fromIdx] = 'P' || boardArr.[fromIdx] = 'p'
      let givesCheck =
        let mutable after = PositionOps.copy &pos
        makeMove &mv &after
        InCheck &after <> 0UL
      { Uci = uci
        San = san
        IsCapture = isCapture
        IsCastling = (mv.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
        IsEnPassant = isEp
        GivesCheck = givesCheck
        IsZeroing = isZeroing
        IsIrreversible =
          isZeroing || rightSquares.Contains fromIdx || rightSquares.Contains (sqIdx uci 2) })
  // A legal en passant is use-it-or-lose-it: when one exists, EVERY move is
  // irreversible (python-chess semantics).
  if result |> Array.exists (fun m -> m.IsEnPassant) then
    result |> Array.map (fun m -> { m with IsIrreversible = true })
  else result

/// Applies one legal move (UCI, e.g. "e2e4"/"e7e8q") to a FEN and returns the resulting
/// FEN; None when the position is invalid or the move is not legal in it (python-chess
/// push() parity in FEN-in/FEN-out form). Counters, castling rights and the en-passant
/// field all come from the real move machinery.
let tryMakeMove (fen: string) (uciMove: string) : string option =
  if not (validateFen fen).IsValid then None
  else
    let fen = normalizeFen fen
    let uci = (if isNull uciMove then "" else uciMove.Trim()).ToLowerInvariant()
    let board = Board()
    board.LoadFen fen
    let mutable pos = board.Position
    let moves = board.GenerateMoves()
    let mutable result = None
    let mutable i = 0
    while result.IsNone && i < moves.Length do
      let mutable mv = moves.[i]
      if TMoveOps.moveToStr &mv pos.STM = uci then
        board.MakeMove &mv
        result <- Some (board.FEN())
      i <- i + 1
    result

// Short-SAN input: strip decorations and unify spellings so input matches the
// generator's own SAN regardless of style ("Qxf7+", "O-O" vs "0-0", "e8=Q" vs "e8Q").
let private normalizeSanToken (s: string) =
  s.Trim().TrimEnd('+', '#', '!', '?').Replace("O", "0").Replace("o", "0").Replace("=", "")

/// Resolves a short-SAN move ("Nf3", "exd5", "0-0", "e8=Q") to its UCI string by matching
/// against the generator's own SAN for the position — disambiguation, castling spelling
/// and promotion format therefore can't drift from EB's SAN emission. None when nothing
/// matches, including ambiguous under-specified input ("Nd2" with two knights able —
/// python-chess raises AmbiguousMoveError; here that is indistinguishable from illegal).
let tryParseSan (fen: string) (san: string) : string option =
  if String.IsNullOrWhiteSpace san then None
  else
    let target = normalizeSanToken san
    let moves = legalMovesOf fen
    let candidates = moves |> Array.filter (fun m -> normalizeSanToken m.San = target)
    match candidates with
    | [| m |] -> Some m.Uci
    | [||] when target.Length >= 3 && "KQRBN".IndexOf target.[0] >= 0 ->
        // Over-disambiguated piece moves ("Ngf3", "R1a2", "Qh4e1") don't match the
        // generator's minimal SAN — python-chess and many PGN exporters emit them, so
        // parse loosely: piece + destination + optional origin hints, filtered against
        // the legal list. Still None unless exactly one move fits.
        let core = target.Replace("x", "")
        let dest = core.Substring(core.Length - 2)
        if dest.[0] < 'a' || dest.[0] > 'h' || dest.[1] < '1' || dest.[1] > '8' then None
        else
          let hints = core.Substring(1, core.Length - 3)
          let fileHint = hints |> Seq.tryFind (fun c -> c >= 'a' && c <= 'h')
          let rankHint = hints |> Seq.tryFind (fun c -> c >= '1' && c <= '8')
          let b = boardOfFen fen
          let stmWhite = (normalizeFen fen).Split(' ').[1] = "w"
          let pieceChar = if stmWhite then target.[0] else Char.ToLowerInvariant target.[0]
          let fits =
            moves |> Array.filter (fun m ->
              m.Uci.Length = 4 && m.Uci.Substring(2, 2) = dest
              && b.[(int m.Uci.[1] - int '1') * 8 + (int m.Uci.[0] - int 'a')] = pieceChar
              && (fileHint |> Option.forall (fun f -> m.Uci.[0] = f))
              && (rankHint |> Option.forall (fun r -> m.Uci.[1] = r)))
          match fits with
          | [| m |] -> Some m.Uci
          | _ -> None
    | _ -> None

/// Applies one short-SAN move to a FEN (tryParseSan >> tryMakeMove).
let tryMakeSanMove (fen: string) (san: string) : string option =
  tryParseSan fen san |> Option.bind (fun uci -> tryMakeMove fen uci)

/// Normalizes a move line to a space-joined UCI sequence by walking the position move by
/// move. Tokens may be UCI or short SAN, freely mixed; move numbers ("1.", "12...", also
/// glued as "1.e4") and result markers are skipped. None when any move fails to resolve
/// or apply; Some "" for an empty line.
let pvToUci (fen: string) (line: string) : string option =
  if String.IsNullOrWhiteSpace line then Some ""
  else
    let isUci (t: string) =
      (t.Length = 4 || t.Length = 5)
      && t.[0] >= 'a' && t.[0] <= 'h' && t.[1] >= '1' && t.[1] <= '8'
      && t.[2] >= 'a' && t.[2] <= 'h' && t.[3] >= '1' && t.[3] <= '8'
      && (t.Length = 4 || "qrbn".IndexOf t.[4] >= 0)
    let isSkippable (t: string) =
      t = "..." || t = "*" || t = "1-0" || t = "0-1" || t = "1/2-1/2"
      || (t.EndsWith "." && t.TrimEnd('.') |> Seq.forall Char.IsDigit)
    let mutable cur = normalizeFen fen
    let acc = ResizeArray<string>()
    let mutable ok = true
    for raw in line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) do
      if ok then
        // "1.e4" / "12...Nf6": strip a digits-and-dots move-number prefix glued to the move
        let t =
          let idx = raw.LastIndexOf '.'
          if idx >= 0 && idx < raw.Length - 1 then
            // digits-and-dots only (empty covers glued "...Nf6") — else leave untouched
            let prefix = raw.Substring(0, idx + 1).TrimEnd('.')
            if prefix |> Seq.forall Char.IsDigit then raw.Substring(idx + 1) else raw
          else raw
        if not (isSkippable t) then
          let uciCandidate = t.ToLowerInvariant()
          let uci = if isUci uciCandidate then Some uciCandidate else tryParseSan cur t
          match uci |> Option.bind (fun u -> tryMakeMove cur u |> Option.map (fun next -> u, next)) with
          | Some (u, next) ->
            acc.Add u
            cur <- next
          | None -> ok <- false
    if ok then Some (String.Join(" ", acc)) else None

/// Operand of an EPD opcode for epdOf. Move operands accept SAN or UCI and are
/// canonicalized to the generator's SAN on output.
type EpdOperand =
  | EpdStr of string        // written quoted: id "pos-1";
  | EpdInt of int
  | EpdFloat of float       // invariant culture
  | EpdMove of string       // single move: sm Nf3;
  | EpdMoves of string[]    // move lists: bm Nf3 e4;
  | EpdPv of string         // a line, walked position by position: pv e4 e5 Nf3;
  | EpdBare                 // opcode without operand

// Interop spelling on the way OUT only: EB's SAN says "0-0" but EPD consumers
// (python-chess and friends) expect "O-O"; internal APIs keep the zeros.
let private interopSan (san: string) =
  if san.StartsWith "0-0" then san.Replace('0', 'O') else san

/// Builds an EPD line: the 4-field FEN prefix plus opcodes per the EPD spec, each
/// terminated by ';'. Move operands are validated as legal and written as canonical
/// SAN (castling spelled O-O for interop); EpdPv walks the line via pvToUci. The
/// counters are dropped per the spec — pass ("hmvc", EpdInt n)/("fmvn", EpdInt n)
/// explicitly if needed. Throws ArgumentException on an invalid FEN, a bad opcode
/// name, an embedded quote in a string operand, or an illegal move.
let epdOf (fen: string) (ops: (string * EpdOperand) list) : string =
  let v = validateFen fen
  if not v.IsValid then invalidArg "fen" (String.concat "; " v.Errors)
  let fen = normalizeFen fen
  let fields = fen.Split(' ')
  let sb = System.Text.StringBuilder(String.Join(" ", fields.[0..3]))
  let sanOf (position: string) (move: string) =
    let moves = legalMovesOf position
    let byUci = moves |> Array.tryFind (fun m -> m.Uci = move.Trim().ToLowerInvariant())
    let notation =
      match byUci with
      | Some _ -> byUci
      | None -> tryParseSan position move |> Option.bind (fun uci -> moves |> Array.tryFind (fun m -> m.Uci = uci))
    match notation with
    | Some m -> interopSan m.San
    | None -> invalidArg "ops" (sprintf "move '%s' is not legal in '%s'" move position)
  for (name, operand) in ops do
    if String.IsNullOrWhiteSpace name || name = "-"
       || name |> Seq.exists (fun c -> Char.IsWhiteSpace c || c = ';' || c = '"') then
      invalidArg "ops" (sprintf "invalid EPD opcode name '%s'" name)
    sb.Append(' ').Append(name) |> ignore
    match operand with
    | EpdBare -> ()
    | EpdStr s ->
        if s.Contains "\"" then invalidArg "ops" (sprintf "string operand for '%s' contains a quote" name)
        sb.Append(" \"").Append(s).Append('"') |> ignore
    | EpdInt i -> sb.Append(' ').Append(i) |> ignore
    | EpdFloat f -> sb.Append(' ').Append(f.ToString("0.####", Globalization.CultureInfo.InvariantCulture)) |> ignore
    | EpdMove m -> sb.Append(' ').Append(sanOf fen m) |> ignore
    | EpdMoves ms ->
        if Array.isEmpty ms then invalidArg "ops" (sprintf "empty move list for '%s'" name)
        for m in ms do sb.Append(' ').Append(sanOf fen m) |> ignore
    | EpdPv line ->
        match pvToUci fen line with
        | None | Some "" -> invalidArg "ops" (sprintf "pv '%s' is not a legal line from '%s'" line fen)
        | Some uciLine ->
            let mutable cur = fen
            for u in uciLine.Split(' ') do
              sb.Append(' ').Append(sanOf cur u) |> ignore
              cur <- (tryMakeMove cur u).Value
    sb.Append(';') |> ignore
  sb.ToString()

/// Checkmate/stalemate/check detection and the dead-position approximation.
let getPositionStatus (fen: string) : PositionStatus =
  let fen = normalizeFen fen
  let board = Board()
  board.LoadFen fen
  let mutable pos = board.Position
  let inCheck = InCheck &pos <> 0UL
  let anyMoves = board.GenerateMoves().Length > 0
  let status =
    if not anyMoves && inCheck then "checkmate"
    elif not anyMoves then "stalemate"
    elif inCheck then "check"
    else "ok"
  let insufficient =
    let heavy = PositionOps.pawns &pos ||| PositionOps.rooks &pos ||| PositionOps.queens &pos
    if heavy <> 0UL then false
    else
      let knights = PositionOps.knights &pos
      let bishops = PositionOps.bishops &pos
      let minors =
        System.Numerics.BitOperations.PopCount knights + System.Numerics.BitOperations.PopCount bishops
      if minors <= 1 then true
      elif knights <> 0UL then false
      else
        // bishops only: dead when they all live on one square color (color parity is
        // preserved under the frame's vertical flip for the all-same comparison)
        let dark = 0xAA55AA55AA55AA55UL
        bishops &&& dark = 0UL || bishops &&& ~~~dark = 0UL
  // Halfmove-counter draws: only meaningful while the game is still playable
  // (checkmate on the 100th halfmove is a mate, not a claimable draw).
  let halfmoves =
    match Int32.TryParse (fen.Split(' ').[4]) with
    | true, v -> v
    | _ -> 0
  { Status = status
    InsufficientMaterial = insufficient
    FiftyMoveDraw = anyMoves && halfmoves >= 100
    SeventyFiveMoveDraw = anyMoves && halfmoves >= 150 }

/// Squares strictly between two aligned squares; empty when not aligned. Pure geometry.
let between (a: string) (b: string) : string[] =
  squareNamesIn true (betweenBB.[absSquareIndex a * 64 + absSquareIndex b])

/// The full rank/file/diagonal through two aligned squares, endpoints included;
/// empty when not aligned. Pure geometry.
let ray (a: string) (b: string) : string[] =
  squareNamesIn true (lineBB.[absSquareIndex a * 64 + absSquareIndex b])

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
