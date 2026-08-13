module PositionInsightsTests

open Xunit
open ChessLibrary.BoardUtils

/// Tests for BoardUtils.getPositionInsights — pins and checks for GUI overlays.
/// The critical cases are black-to-move positions: LegalityContext bitboards live in
/// the QBB side-to-move-relative frame, so a missing/incorrect square flip would
/// report vertically mirrored squares for black.

let private sorted (xs: string[]) = xs |> Array.sort

let private pinTriple (p: PinInfo) = (p.Attacker, p.Pinned, p.King)

let private hangingPair (h: HangingInfo) = (h.Square, sorted h.Attackers)

[<Fact>]
let ``Start position has no pins and no checks for either side`` () =
    let i = getPositionInsights "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.Equal("e1", i.White.King)
    Assert.Equal("e8", i.Black.King)
    Assert.True(i.White.IsSideToMove)
    Assert.False(i.Black.IsSideToMove)
    Assert.False(i.White.InCheck)
    Assert.False(i.Black.InCheck)
    Assert.Empty(i.White.Pins)
    Assert.Empty(i.Black.Pins)
    Assert.Empty(i.White.Checkers)
    Assert.Empty(i.Black.Checkers)
    // every king-adjacent square is own-occupied -> excluded from both categories
    Assert.Empty(i.White.KingDangerSquares)
    Assert.Empty(i.White.KingEscapeSquares)
    Assert.Empty(i.Black.KingDangerSquares)
    Assert.Empty(i.Black.KingEscapeSquares)
    Assert.Empty(i.White.HangingPieces)
    Assert.Empty(i.Black.HangingPieces)

[<Fact>]
let ``White pawn pinned by bishop, white to move`` () =
    // Ba5 - c3 pawn - Ke1 on the a5-e1 diagonal, c3 the single blocker
    let i = getPositionInsights "4k3/8/8/b7/8/2P5/8/4K3 w - - 0 1"
    Assert.Equal<(string * string * string)[]>([| ("a5", "c3", "e1") |], i.White.Pins |> Array.map pinTriple)
    Assert.Empty(i.Black.Pins)
    Assert.False(i.White.InCheck)
    Assert.False(i.Black.InCheck)

[<Fact>]
let ``Same pin reported identically when black is to move (frame flip)`` () =
    // Pins are a property of the position, not of whose turn it is. With black to
    // move the white side is computed on the side-swapped copy — both paths and
    // both name dictionaries are exercised.
    let i = getPositionInsights "4k3/8/8/b7/8/2P5/8/4K3 b - - 0 1"
    Assert.Equal<(string * string * string)[]>([| ("a5", "c3", "e1") |], i.White.Pins |> Array.map pinTriple)
    Assert.Empty(i.Black.Pins)
    Assert.False(i.White.IsSideToMove)
    Assert.True(i.Black.IsSideToMove)

[<Fact>]
let ``Cross pin on the e-file pins a piece of each color`` () =
    // e-file: Ke1, Qe4 (white), Re7 (black), Ke8. The white queen is pinned by the
    // rook; the black rook is pinned by the queen.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/4r3/8/8/4Q3/8/8/4K3 %s - - 0 1" stm)
        Assert.Equal<(string * string * string)[]>([| ("e7", "e4", "e1") |], i.White.Pins |> Array.map pinTriple)
        Assert.Equal<(string * string * string)[]>([| ("e4", "e7", "e8") |], i.Black.Pins |> Array.map pinTriple)

[<Fact>]
let ``Rook check on the back rank reports checker and block squares`` () =
    let i = getPositionInsights "R3k3/8/8/8/8/8/8/4K3 b - - 0 1"
    Assert.True(i.Black.InCheck)
    Assert.Equal<string[]>([| "a8" |], i.Black.Checkers)
    Assert.Equal<string[]>([| "b8"; "c8"; "d8" |], sorted i.Black.CheckBlockSquares)
    Assert.False(i.White.InCheck)
    Assert.Empty(i.White.Checkers)
    // the rook rakes rank 8 THROUGH the removed king: d8/f8 are danger, the 7th rank is safe
    Assert.Equal<string[]>([| "d8"; "f8" |], sorted i.Black.KingDangerSquares)
    Assert.Equal<string[]>([| "d7"; "e7"; "f7" |], sorted i.Black.KingEscapeSquares)
    // nothing black attacks the white king's neighborhood
    Assert.Empty(i.White.KingDangerSquares)
    Assert.Equal<string[]>([| "d1"; "d2"; "e2"; "f1"; "f2" |], sorted i.White.KingEscapeSquares)

[<Fact>]
let ``Double check reports both checkers and no block squares`` () =
    // Bb5 (via c6-d7) and Re1 (open e-file) both check Ke8
    let i = getPositionInsights "4k3/8/8/1B6/8/8/8/4R1K1 b - - 0 1"
    Assert.True(i.Black.InCheck)
    Assert.Equal<string[]>([| "b5"; "e1" |], sorted i.Black.Checkers)
    Assert.Empty(i.Black.CheckBlockSquares)
    // d7 covered by the bishop, e7 by the rook; d8/f7/f8 are the escape squares
    Assert.Equal<string[]>([| "d7"; "e7" |], sorted i.Black.KingDangerSquares)
    Assert.Equal<string[]>([| "d8"; "f7"; "f8" |], sorted i.Black.KingEscapeSquares)

[<Fact>]
let ``Knight check has no block squares`` () =
    // Nf6+ against Ke8: contact/knight checks cannot be blocked
    let i = getPositionInsights "4k3/8/5N2/8/8/8/8/4K3 b - - 0 1"
    Assert.True(i.Black.InCheck)
    Assert.Equal<string[]>([| "f6" |], i.Black.Checkers)
    Assert.Empty(i.Black.CheckBlockSquares)

[<Fact>]
let ``Ruy Lopez style pin on c6 knight`` () =
    // After 1.e4 e5 2.Nf3 Nc6 3.Bb5 d6: Bb5-c6-d7(empty)-e8 with Nc6 the single blocker
    let i = getPositionInsights "r1bqkbnr/ppp2ppp/2np4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"
    Assert.Equal<(string * string * string)[]>([| ("b5", "c6", "e8") |], i.Black.Pins |> Array.map pinTriple)
    Assert.Empty(i.White.Pins)

[<Fact>]
let ``Multiple simultaneous pins are all reported`` () =
    // Ke1 pinned pieces: Re2 by Re7 (e-file), Bd2 by Ba5 (a5-e1 diagonal)
    let i = getPositionInsights "4k3/4r3/8/b7/8/8/3BR3/4K3 w - - 0 1"
    let pins = i.White.Pins |> Array.map pinTriple |> Array.sort
    Assert.Equal<(string * string * string)[]>([| ("a5", "d2", "e1"); ("e7", "e2", "e1") |], pins)

// --- Hanging pieces -------------------------------------------------------

[<Fact>]
let ``Attacked and undefended piece is hanging`` () =
    // Rd2 attacks the undefended Nd5 up the open d-file; nothing attacks white.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/8/8/3n4/8/8/3R4/4K3 %s - - 0 1" stm)
        Assert.Equal<(string * string[])[]>([| ("d5", [| "d2" |]) |], i.Black.HangingPieces |> Array.map hangingPair)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``Defended piece attacked by equal value is not hanging`` () =
    // Nc3 attacks Nd5, which Rd7 defends -> equal trade, not hanging. But Nd5 also
    // attacks Nc3, which nothing defends -> the WHITE knight is the hanging one.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/3r4/8/3n4/8/2N5/8/4K3 %s - - 0 1" stm)
        Assert.Empty(i.Black.HangingPieces)
        Assert.Equal<(string * string[])[]>([| ("c3", [| "d5" |]) |], i.White.HangingPieces |> Array.map hangingPair)

[<Fact>]
let ``Defended rook attacked by a pawn is hanging (cheaper attacker)`` () =
    // Rd5 is defended by Rd8, but the e4 pawn attacks it: losing the exchange either way.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "3rk3/8/8/3r4/4P3/8/8/4K3 %s - - 0 1" stm)
        Assert.Equal<(string * string[])[]>([| ("d5", [| "e4" |]) |], i.Black.HangingPieces |> Array.map hangingPair)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``Defender pinned off its ray does not defend`` () =
    // Re7 is pinned to Ke8 by Re2, so its rank-7 "defense" of Nb7 does not count:
    // Bf3 wins the knight. Re7 itself is defended by the king against the equal rook.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/1n2r3/8/8/8/5B2/4R3/4K3 %s - - 0 1" stm)
        Assert.Equal<(string * string[])[]>([| ("b7", [| "f3" |]) |], i.Black.HangingPieces |> Array.map hangingPair)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``Pinned knight cannot defend`` () =
    // Ruy-style skeleton with d7 EMPTY so Bb5 really pins Nc6 (with a pawn on d7 the
    // diagonal has two blockers and there is no pin). e5's only "defender" is the
    // pinned knight — a pinned knight can never recapture — so e5 hangs to Nf3.
    // The unprotected Nc6 itself also hangs to the bishop.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/8/2n5/1B2p3/8/5N2/8/4K3 %s - - 0 1" stm)
        let hanging = i.Black.HangingPieces |> Array.map hangingPair |> Array.sortBy fst
        Assert.Equal<(string * string[])[]>([| ("c6", [| "b5" |]); ("e5", [| "f3" |]) |], hanging)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``King-only defense still counts`` () =
    // f7 is attacked by Bc4 but defended by Ke8 — not hanging (static heuristic:
    // the king is a normal defender).
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/5p2/8/8/2B5/8/8/4K3 %s - - 0 1" stm)
        Assert.Empty(i.Black.HangingPieces)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``Two attackers overwhelm a single defender`` () =
    // Nd5 is defended once (Rd7) but attacked twice (Nc3 and Bg2 through f3/e4):
    // NxN RxN BxR wins the exchange, so the knight is hanging. White's Nc3 is
    // attacked by Nd5 but pawn-defended by b2 — an equal trade, not hanging.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/3r4/8/3n4/8/2N5/1P4B1/4K3 %s - - 0 1" stm)
        Assert.Equal<(string * string[])[]>([| ("d5", [| "c3"; "g2" |]) |], i.Black.HangingPieces |> Array.map hangingPair)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``Rook battery does not win a pawn-defended pawn`` () =
    // Rd2 (with Rd1 x-raying behind it) attacks d5, defended by the c6 pawn:
    // Rxd5 cxd5 Rxd5 loses the exchange, so nothing hangs. Exercises the x-ray
    // join in the exchange loop.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/8/2p5/3p4/8/8/3R4/3RK3 %s - - 0 1" stm)
        Assert.Empty(i.Black.HangingPieces)
        Assert.Empty(i.White.HangingPieces)

[<Fact>]
let ``King attacker does not trigger the cheaper-attacker rule`` () =
    // Kd4 attacks the c6-defended d5 pawn: defended and the king's "value" never
    // undercuts the victim, so nothing hangs.
    for stm in [ "w"; "b" ] do
        let i = getPositionInsights (sprintf "4k3/8/2p5/3p4/3K4/8/8/8 %s - - 0 1" stm)
        Assert.Empty(i.Black.HangingPieces)
        Assert.Empty(i.White.HangingPieces)
