module MoveGenLegalityTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.Chess
open ChessLibrary.MoveTypes
open ChessLibrary.MoveGeneration
open ChessLibrary.RuntimeUtilities

/// Differential tests for the LegalityContext fast paths against the reference per-move
/// test (BoardHelper.Illegal / MoveGeneration.Illegal). At every node of a bounded
/// game-tree walk:
///   1. isIllegalCtx must agree with the reference Illegal on every PSEUDO-legal move
///      (generated directly via generateCaptures/generateQuiets — Board.GenerateMoves is
///      legal-only since the Phase 3 rework and no longer produces illegal candidates)
///   2. Board.GenerateMoves() (the legal generators) must equal the pseudo-legal list
///      filtered by the reference Illegal, element-for-element IN THE SAME ORDER.

let private startposFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// Pseudo-legal move list via the original (untouched) generators, in generation order
let private pseudoLegalMoves (board: Board) =
    let mutable pos = board.Position
    let buf = Array.zeroCreate<TMove> 256
    let mutable idx = 0
    generateCaptures (buf.AsSpan()) &idx &pos
    generateQuiets (buf.AsSpan()) &idx &pos board.IsFRC
    buf.[0 .. idx - 1]

/// Reference perft using the original generators + per-move Illegal test (bulk leaf counting)
let rec private perftReference (board: Board) depth =
    if depth = 0 then 1L
    else
        let mutable nodes = 0L
        let pos = board.Position
        let moves = pseudoLegalMoves board
        for i in 0 .. moves.Length - 1 do
            let mutable m = moves.[i]
            if not (BoardHelper.Illegal &m &pos) then
                if depth = 1 then nodes <- nodes + 1L
                else
                    board.MakeMoveNoHash &m
                    nodes <- nodes + perftReference board (depth - 1)
                    board.UndoMove()
        nodes

/// Walks the legal game tree to the given depth performing both differential checks.
/// Returns the number of pseudo-legal moves checked.
let rec private walkAndCompare (board: Board) depth =
    let pos = board.Position
    let ctx = createLegalityContext &pos
    let moves = pseudoLegalMoves board
    let mutable checkedMoves = int64 moves.Length
    // check 1: ctx verdict agrees with reference on every pseudo-legal move,
    // and build the reference legal list in generation order
    let refLegal = ResizeArray<TMove>(moves.Length)
    for i in 0 .. moves.Length - 1 do
        let mutable m = moves.[i]
        let refIllegal = BoardHelper.Illegal &m &pos
        let fastIllegal = isIllegalCtx &ctx &m &pos
        if refIllegal <> fastIllegal then
            let moveStr = TMoveOps.moveToStr &m pos.STM
            let fen = BoardHelper.posToFen pos
            Assert.Fail($"Legality disagreement on move {moveStr} (reference illegal={refIllegal}, ctx illegal={fastIllegal}) in position {fen}")
        if not refIllegal then refLegal.Add m
    // check 2: the legal generators produce exactly the filtered list, same order
    let legalNew = board.GenerateMoves()
    if legalNew.Length <> refLegal.Count then
        let fen = BoardHelper.posToFen pos
        Assert.Fail($"Legal-generator count mismatch: generator={legalNew.Length}, reference={refLegal.Count} in position {fen}")
    for i in 0 .. legalNew.Length - 1 do
        if legalNew.[i] <> refLegal.[i] then
            let mutable a = legalNew.[i]
            let mutable b = refLegal.[i]
            let fen = BoardHelper.posToFen pos
            Assert.Fail($"Legal-generator order/content mismatch at index {i}: generator={TMoveOps.moveToStr &a pos.STM}, reference={TMoveOps.moveToStr &b pos.STM} in position {fen}")
    if depth > 1 then
        for i in 0 .. legalNew.Length - 1 do
            let mutable m = legalNew.[i]
            board.MakeMoveNoHash &m
            checkedMoves <- checkedMoves + walkAndCompare board (depth - 1)
            board.UndoMove()
    checkedMoves

let private makeBoard (fen: string) =
    let board = Board()
    board.LoadFen(fen)
    board

// Standard and FRC middlegame positions (from Perft.fs) plus targeted edge-case FENs:
// EP pins, EP giving check, castling through/into attacks, promotions in/giving check,
// discovered checks, pinned pieces, double check.
[<Theory>]
// standard suite
[<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 3)>]
[<InlineData("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3)>]
[<InlineData("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 4)>]
[<InlineData("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 3)>]
[<InlineData("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 3)>]
[<InlineData("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 3)>]
// FRC positions
[<InlineData("bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9", 3)>]
[<InlineData("2nnrbkr/p1qppppp/8/1ppb4/6PP/3PP3/PPP2P2/BQNNRBKR w HEhe - 1 9", 3)>]
[<InlineData("b1q1rrkb/pppppppp/3nn3/8/P7/1PPP4/4PPPP/BQNNRKRB w GE - 1 9", 3)>]
[<InlineData("nrbkn2r/pppp1p1p/4p1p1/3P4/6P1/P3B3/P1P1PP1P/qR1KNBQR w KQkq - 0 10", 3)>]
// EP edge cases
[<InlineData("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 4)>]         // EP would expose king along rank 5
[<InlineData("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 4)>]        // EP would expose king along diagonal
[<InlineData("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 4)>]       // EP capture gives check
[<InlineData("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 b - - 0 1", 4)>] // endgame suite pos, black to move
// castling edge cases
[<InlineData("5k2/8/8/8/8/8/8/4K2R w K - 0 1", 4)>]            // short castle gives check
[<InlineData("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 4)>]            // long castle gives check
[<InlineData("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 3)>] // castling with attacked squares
[<InlineData("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 3)>]  // castling prevented by attacks
// promotion / check edge cases
[<InlineData("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 4)>]         // promote out of check
[<InlineData("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 4)>]       // discovered check
[<InlineData("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 4)>]            // promote to give check
[<InlineData("8/P1k5/K7/8/8/8/8/8 w - - 0 1", 4)>]             // underpromotion race
[<InlineData("K1k5/8/P7/8/8/8/8/8 w - - 0 1", 4)>]             // near-stalemate
[<InlineData("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 4)>]            // stalemate/checkmate boundary
// pins and double check
[<InlineData("4k3/8/4r3/8/4Q3/8/8/4K3 b - - 0 1", 4)>]         // rook pinned by queen
[<InlineData("4k3/8/8/b7/8/2N5/8/4K3 w - - 0 1", 4)>]          // knight pinned by bishop
[<InlineData("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4)>]         // queen+knight attack net
let ``isIllegalCtx agrees with reference Illegal on every move in tree walk`` (fen: string) (depth: int) =
    let board = makeBoard fen
    let checkedMoves = walkAndCompare board depth
    Assert.True(checkedMoves > 0L, "walk should have checked at least one move")

[<Theory>]
[<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 4)>]
[<InlineData("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3)>]
[<InlineData("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5)>]
[<InlineData("bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9", 4)>]
[<InlineData("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 5)>]
[<InlineData("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 5)>]
[<InlineData("5k2/8/8/8/8/8/8/4K2R w K - 0 1", 5)>]
[<InlineData("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 5)>]
let ``ctx-based perftFast matches reference perft node counts`` (fen: string) (depth: int) =
    let fastBoard = makeBoard fen
    let refBoard = makeBoard fen
    let fastCount = Perft.perftFast fastBoard depth
    let refCount = perftReference refBoard depth
    Assert.Equal(refCount, fastCount)

[<Theory>]
[<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 4, 197_281L)>]
[<InlineData("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3, 97_862L)>]
[<InlineData("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5, 674_624L)>]
let ``ctx-based perftFast matches known perft values`` (fen: string) (depth: int) (expected: int64) =
    let board = makeBoard fen
    Assert.Equal(expected, Perft.perftFast board depth)

[<Fact>]
let ``LegalityContext reports check state consistently with InCheck`` () =
    let fens =
        [ startposFen
          "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
          "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1"           // white in check
          "4k3/8/4r3/8/4Q3/8/8/4K3 b - - 0 1"           // black not in check, rook pinned
          "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1" ]
    for fen in fens do
        let mutable pos = BoardHelper.getPosFromFen (Some fen)
        let ctx = createLegalityContext &pos
        let inCheck = InCheck &pos <> 0UL
        Assert.True((ctx.Checkers <> 0UL) = inCheck, $"Checkers vs InCheck mismatch for {fen}")

// ===== getShortSanPVFromLongSanPVFast (numeric matching + count-based generation) =====

let private convertPv fen pv =
    let board = makeBoard fen
    let buf = Array.zeroCreate<TMove> 256
    BoardUtils.getShortSanPVFromLongSanPVFast buf &board pv

[<Fact>]
let ``PV conversion handles castling, captures and numbering`` () =
    let sanPv = convertPv startposFen "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1 f8e7 f1e1 b7b5 a4b3 d7d6 c2c3 e8g8"
    Assert.Equal("1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 5.0-0 Be7 6.Re1 b5 7.Bb3 d6 8.c3 0-0", sanPv)

[<Fact>]
let ``PV conversion starting with a black move uses ellipsis numbering`` () =
    let sanPv = convertPv "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1" "e7e5 g1f3 b8c6"
    Assert.Equal("1.... e5 2.Nf3 Nc6", sanPv)

[<Fact>]
let ``PV conversion handles capture promotion`` () =
    let sanPv = convertPv "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1" "e7f8q"
    Assert.Equal("1.exf8=Q", sanPv)

[<Fact>]
let ``PV conversion handles underpromotion`` () =
    let sanPv = convertPv "4k3/1P6/8/8/8/8/8/4K3 w - - 0 1" "b7b8n"
    Assert.Equal("1.b8=N", sanPv)

[<Fact>]
let ``PV conversion disambiguates between two knights`` () =
    // knights a1 and c1 both reach b3
    let sanPv = convertPv "4k3/8/8/8/8/8/8/N1N1K3 w - - 0 1" "a1b3"
    Assert.Equal("1.Nab3", sanPv)

[<Fact>]
let ``PV conversion skips disambiguation when the alternative piece is pinned`` () =
    // knights e3 (pinned by Re8) and g3 both reach f5 pseudo-legally, but only Ng3 legally:
    // SAN over legal moves is Nf5, not Ngf5 (PGN spec disambiguates among legal moves)
    let sanPv = convertPv "4r3/k7/8/8/8/6N1/4N3/4K3 w - - 0 1" "g3f5"
    Assert.Equal("1.Nf5", sanPv)

[<Fact>]
let ``PV conversion handles FRC castling token`` () =
    // frc3 position: white g-side castle is king f1 -> rook g1 (token f1g1).
    // (frc1's short castle is NOT legal there — the second rook blocks f1.)
    let sanPv = convertPv "b1q1rrkb/pppppppp/3nn3/8/P7/1PPP4/4PPPP/BQNNRKRB w GE - 1 9" "f1g1"
    Assert.Equal("9.0-0", sanPv)

[<Fact>]
let ``PV conversion skips an unmatched move and continues`` () =
    // e2e5 matches no legal move: the token is skipped WITHOUT playing anything (the
    // position stays intact), and conversion continues — same behavior as the original
    // string-matching implementation
    let sanPv = convertPv startposFen "e2e4 e7e5 e2e5 g1f3"
    Assert.Equal("1.e4 e5 2.Nf3", sanPv)

// ===== FRC castling with ambiguous rook configurations =====
// A Shredder-FEN rights letter is authoritative and classified relative to the KING;
// a second own rook on the back rank beyond the castle rook must not confuse
// identification (parser guess, canKingReach* masks, Illegal castle branch).

let private playCastleAndGetFen fen san =
    let board = makeBoard fen
    board.PlaySanMove san
    board.FEN()

[<Fact>]
let ``FRC kingside castle with extra rook beyond the castle rook`` () =
    // Kf1, castle rook g1 (right G), maneuvered rook h1: 0-0 = king->g1, rook->f1, h1 stays
    let board = makeBoard "6k1/8/8/8/8/8/8/5KRR w G - 0 1"
    Assert.Equal(6uy, board.Position.RookInfo.WhiteKRInitPlacement)
    let sans = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    Assert.Contains("0-0", sans)
    let after = playCastleAndGetFen "6k1/8/8/8/8/8/8/5KRR w G - 0 1" "0-0"
    Assert.StartsWith("6k1/8/8/8/8/8/8/5RKR b", after)

[<Fact>]
let ``FRC queenside castle with extra rook beyond the castle rook`` () =
    // Kc1, castle rook b1 (right B), extra rook a1: 0-0-0 = king stays c1, rook->d1
    let board = makeBoard "6k1/8/8/8/8/8/8/RRK5 w B - 0 1"
    Assert.Equal(1uy, board.Position.RookInfo.WhiteQRInitPlacement)
    let sans = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    Assert.Contains("0-0-0", sans)
    Assert.DoesNotContain("0-0", sans)
    let after = playCastleAndGetFen "6k1/8/8/8/8/8/8/RRK5 w B - 0 1" "0-0-0"
    Assert.StartsWith("6k1/8/8/8/8/8/8/R1KR4 b", after)

[<Fact>]
let ``FRC kingside castle labeled and played correctly when rook stays on its square`` () =
    // Kd1, castle rook f1 (right F), extra rook h1: 0-0 = king->g1, rook stays f1
    let board = makeBoard "6k1/8/8/8/8/8/8/3K1R1R w F - 0 1"
    Assert.Equal(5uy, board.Position.RookInfo.WhiteKRInitPlacement)
    let sans = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    Assert.Contains("0-0", sans)
    let after = playCastleAndGetFen "6k1/8/8/8/8/8/8/3K1R1R w F - 0 1" "0-0"
    Assert.StartsWith("6k1/8/8/8/8/8/8/5RKR b", after)

[<Fact>]
let ``FRC black mirror: queenside castle with extra rook`` () =
    // black: Ra8, castle rook b8 (right b), Kc8: 0-0-0 = king stays c8, rook->d8
    let board = makeBoard "rrk5/8/8/8/8/8/8/6K1 b b - 0 1"
    Assert.Equal(1uy, board.Position.RookInfo.BlackQRInitPlacement)
    let sans = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    Assert.Contains("0-0-0", sans)
    let after = playCastleAndGetFen "rrk5/8/8/8/8/8/8/6K1 b b - 0 1" "0-0-0"
    Assert.StartsWith("r1kr4/8/8/8/8/8/8/6K1 w", after)

[<Fact>]
let ``missing black queenside rook stores the no-rook sentinel, not a phantom h-file rook`` () =
    // black has only the h-right and its a-rook is off the back rank: BlackQR must be 15
    // (the old -56 wrap decoded the sentinel to 7, colliding with the real KR and routing
    // black's short castle through the long-castle legality test — caught by the 960 sweep)
    let board = makeBoard "1qbbn1kr/1ppQpppp/r7/6n1/p1P5/P7/1P1PPPPP/R1BBNNKR b HAh - 0 10"
    Assert.Equal(15uy, board.Position.RookInfo.BlackQRInitPlacement)
    Assert.Equal(7uy, board.Position.RookInfo.BlackKRInitPlacement)
    let sans = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    Assert.Contains("0-0", sans)

[<Fact>]
let ``castling right letter without a rook on that square is stripped`` () =
    // pre-existing behavior, must survive the parser rework
    let board = makeBoard "r5k1/pp4r1/2n1b1pR/q1p1p2Q/P3P3/2NP4/1P3PP1/4K1N1 w G - 0 19"
    Assert.Equal(0uy, board.Position.CastleFlags)

[<Fact>]
let ``standard FRC start position still parses rook placements correctly`` () =
    let board = makeBoard "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
    Assert.Equal(7uy, board.Position.RookInfo.WhiteKRInitPlacement)
    Assert.Equal(5uy, board.Position.RookInfo.WhiteQRInitPlacement)
    Assert.Equal(7uy, board.Position.RookInfo.BlackKRInitPlacement)
    Assert.Equal(5uy, board.Position.RookInfo.BlackQRInitPlacement)

[<Fact>]
let ``pinned rook has no legal moves off the pin file`` () =
    // black rook on e6 pinned by white queen on e4 against king on e8
    let board = makeBoard "4k3/8/4r3/8/4Q3/8/8/4K3 b - - 0 1"
    let legal = board.GetLegalMoves() |> Seq.map snd |> Set.ofSeq
    // rook may slide along the e-file (block-distance squares) but never off-file
    Assert.Contains("Re7", legal)
    Assert.Contains("Re5", legal)
    Assert.Contains("Rxe4", legal)
    Assert.DoesNotContain("Ra6", legal)
    Assert.DoesNotContain("Rd6", legal)
