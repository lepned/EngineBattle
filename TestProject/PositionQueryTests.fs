module PositionQueryTests

open System
open Xunit
open ChessLibrary
open ChessLibrary.BoardUtils

/// Tests for the position-query facade (attackersOf/attacksFrom/isPinned/pinRay/
/// between/ray) — python-chess-style per-square API used for validator work. As with
/// the insights tests, black-to-move positions are the critical cases: a missing frame
/// flip reports vertically mirrored squares.

let private sorted (xs: string[]) = xs |> Array.sort

// --- attackersOf ----------------------------------------------------------

[<Fact>]
let ``attackersOf splits attackers and defenders by color`` () =
    // Rd2 attacks the knight; the white king "attacks" (defends) its own rook square.
    for stm in [ "w"; "b" ] do
        let fen = sprintf "4k3/8/8/3n4/8/8/3R4/4K3 %s - - 0 1" stm
        let d5 = attackersOf fen "d5"
        Assert.Equal<string[]>([| "d2" |], d5.White)
        Assert.Empty(d5.Black)
        let d2 = attackersOf fen "d2"
        Assert.Equal<string[]>([| "e1" |], d2.White)
        Assert.Empty(d2.Black)

[<Fact>]
let ``attackersOf of an empty contested square sees both colors`` () =
    // e4 knight and e5 knight both attack d3/d6-ish squares; use c4: white Ne4? Use a
    // simple contested square: both rooks on the d-file attack d4.
    for stm in [ "w"; "b" ] do
        let fen = sprintf "3rk3/8/8/8/8/8/8/3RK3 %s - - 0 1" stm
        let d4 = attackersOf fen "d4"
        Assert.Equal<string[]>([| "d1" |], d4.White)
        Assert.Equal<string[]>([| "d8" |], d4.Black)

// --- attacksFrom ----------------------------------------------------------

[<Fact>]
let ``attacksFrom pawn gives capture directions only, for both colors`` () =
    for stm in [ "w"; "b" ] do
        let fen = sprintf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR %s KQkq - 0 1" stm
        Assert.Equal<string[]>([| "d3"; "f3" |], sorted (attacksFrom fen "e2"))
        Assert.Equal<string[]>([| "d6"; "f6" |], sorted (attacksFrom fen "e7"))
        // edge pawn: single capture direction, no wrap
        Assert.Equal<string[]>([| "b3" |], attacksFrom fen "a2")
        Assert.Equal<string[]>([| "g6" |], attacksFrom fen "h7")

[<Fact>]
let ``attacksFrom includes own-occupied targets and stops sliders at blockers`` () =
    for stm in [ "w"; "b" ] do
        let fen = sprintf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR %s KQkq - 0 1" stm
        // knight attack set includes the own-occupied d2
        Assert.Equal<string[]>([| "a3"; "c3"; "d2" |], sorted (attacksFrom fen "b1"))
        // rook ray stops at (and includes) the first blocker in each direction
        Assert.Equal<string[]>([| "a2"; "b1" |], sorted (attacksFrom fen "a1"))
        // empty square -> empty set
        Assert.Empty(attacksFrom fen "e4")

// --- isPinned / pinRay ----------------------------------------------------

[<Fact>]
let ``cross pin reports both pinned pieces with the e-file ray`` () =
    // e-file: Ke1, Qe4 (white, pinned by Re7), Re7 (black, pinned by Qe4), Ke8.
    let efile = [| "e1"; "e2"; "e3"; "e4"; "e5"; "e6"; "e7"; "e8" |]
    for stm in [ "w"; "b" ] do
        let fen = sprintf "4k3/4r3/8/8/4Q3/8/8/4K3 %s - - 0 1" stm
        Assert.True(isPinned fen "e4")
        Assert.True(isPinned fen "e7")
        Assert.Equal<string[]>(efile, sorted (pinRay fen "e4"))
        Assert.Equal<string[]>(efile, sorted (pinRay fen "e7"))
        // kings are never pinned; empty squares are not pinned
        Assert.False(isPinned fen "e1")
        Assert.False(isPinned fen "d4")
        Assert.Empty(pinRay fen "d4")

// --- between / ray --------------------------------------------------------

[<Fact>]
let ``between and ray follow python-chess geometry`` () =
    Assert.Equal<string[]>([| "e2"; "e3"; "e4"; "e5"; "e6"; "e7" |], sorted (between "e1" "e8"))
    Assert.Empty(between "a1" "h7")
    Assert.Equal<string[]>([| "a1"; "b2"; "c3"; "d4"; "e5"; "f6"; "g7"; "h8" |], sorted (ray "a1" "h8"))
    Assert.Equal<string[]>([| "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4" |], sorted (ray "c4" "f4"))
    Assert.Empty(ray "e4" "d7")
    Assert.Throws<ArgumentException>(fun () -> between "z9" "a1" |> ignore) |> ignore

// --- validateFen ----------------------------------------------------------

let private errorsOf fen = (validateFen fen).Errors

[<Fact>]
let ``validateFen accepts standard, EPD-style and FRC castling FENs`` () =
    Assert.True((validateFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").IsValid)
    Assert.True((validateFen "4k3/4r3/8/8/4Q3/8/8/4K3 w - -").IsValid)                 // 4-field EPD
    Assert.True((validateFen "bqnbrkrn/pppppppp/8/8/8/8/PPPPPPPP/BQNBRKRN w GEge - 0 1").IsValid)  // FRC letters
    Assert.True((validateFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").IsValid) // ep square

[<Fact>]
let ``validateFen reports each structural error class`` () =
    Assert.Contains(errorsOf "", (fun (e: string) -> e.Contains "empty"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1", (fun (e: string) -> e.Contains "8 ranks"))
    Assert.Contains(errorsOf "rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", (fun (e: string) -> e.Contains "rank 7 has 9 files"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNK w KQkq - 0 1", (fun (e: string) -> e.Contains "white has 2 kings"))
    Assert.Contains(errorsOf "rnbq1bnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", (fun (e: string) -> e.Contains "black has 0 kings"))
    Assert.Contains(errorsOf "rnbqkbnP/pppppppp/8/8/8/8/PPPPPPP1/RNBQKBNR w KQkq - 0 1", (fun (e: string) -> e.Contains "pawn on rank 8"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1", (fun (e: string) -> e.Contains "side to move"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkz - 0 1", (fun (e: string) -> e.Contains "castling"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e5 0 1", (fun (e: string) -> e.Contains "en-passant"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - x 1", (fun (e: string) -> e.Contains "halfmove"))
    Assert.Contains(errorsOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 extra", (fun (e: string) -> e.Contains "at most 6"))

[<Fact>]
let ``validateFen accepts short FENs via normalization`` () =
    // "<placement> w" is a common paste form: missing trailing fields get defaults.
    Assert.True((validateFen "3k4/8/8/8/3N4/8/8/3RK3 w").IsValid)
    Assert.True((validateFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w").IsValid)
    Assert.Equal("3k4/8/8/8/3N4/8/8/3RK3 w - - 0 1", normalizeFen "3k4/8/8/8/3N4/8/8/3RK3 w")
    Assert.Equal("4k3/4r3/8/8/4Q3/8/8/4K3 w - - 0 1", normalizeFen "4k3/4r3/8/8/4Q3/8/8/4K3 w - -")
    // full FENs pass through untouched
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                 normalizeFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    // and the FEN-based APIs are normalization-safe end to end
    let i = getPositionInsights "3k4/8/8/8/3N4/8/8/3RK3 w"
    Assert.Equal(1, i.White.DiscoveredAttacks.Length)
    Assert.Equal(20, (legalMovesOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w").Length)

// --- boardOfFen / buildFen ------------------------------------------------

[<Fact>]
let ``buildFen round-trips boardOfFen for full FENs`` () =
    // Placement + all trailing fields survive the array round-trip unchanged.
    let fens =
        [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"   // kiwipete
          "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"            // ep square
          "8/2k5/8/8/8/8/5K2/8 w - - 42 97" ]                                       // sparse + counters
    for fen in fens do
        let f = fen.Split(' ')
        let rebuilt = buildFen (boardOfFen fen) f.[1].[0] f.[2] f.[3] (int f.[4]) (int f.[5])
        Assert.Equal(fen, rebuilt)
        Assert.True((validateFen rebuilt).IsValid)

[<Fact>]
let ``buildFen normalizes empty castling and en passant to dashes`` () =
    let board = boardOfFen "4k3/8/8/8/8/8/8/4K3 w - - 0 1"
    Assert.Equal("4k3/8/8/8/8/8/8/4K3 w - - 0 1", buildFen board 'w' "" "" 0 1)
    Assert.Equal("4k3/8/8/8/8/8/8/4K3 b - - 3 7", buildFen board 'b' null null 3 7)

[<Fact>]
let ``boardOfFen indexes a1 as zero with FEN piece chars`` () =
    let b = boardOfFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.Equal('R', b.[0])                  // a1
    Assert.Equal('K', b.[4])                  // e1
    Assert.Equal('P', b.[1 * 8 + 3])          // d2
    Assert.Equal('k', b.[7 * 8 + 4])          // e8
    Assert.Equal('\000', b.[3 * 8 + 3])       // d4 empty

// --- board editing (pieceAt/setPieceAt/rights/ep/tryMakeMove) -------------

let private startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

[<Fact>]
let ``pieceAt reads squares by name`` () =
    Assert.Equal('R', pieceAt startpos "a1")
    Assert.Equal('k', pieceAt startpos "e8")
    Assert.Equal('\000', pieceAt startpos "e4")

[<Fact>]
let ``setPieceAt prunes castling rights and never restores them`` () =
    let noH1 = removePieceAt startpos "h1"
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN1 w Qkq - 0 1", noH1)
    // Stateless pruning: putting the rook back does NOT bring the right back.
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Qkq - 0 1", setPieceAt noH1 "h1" 'R')

[<Fact>]
let ``setPieceAt prunes an unsupported en-passant square`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    Assert.Contains(" b KQkq - ", removePieceAt fen "e4")   // the double-stepped pawn left
    Assert.Contains(" b KQkq e3 ", removePieceAt fen "a2")  // unrelated edit keeps it

[<Fact>]
let ``availableCastlingRights follows piece placement`` () =
    Assert.Equal("KQkq", availableCastlingRights startpos)
    Assert.Equal("Kkq", availableCastlingRights "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBNR w KQkq - 0 1")
    Assert.Equal("", availableCastlingRights "4k3/8/8/8/8/8/8/4K3 w - - 0 1")

[<Fact>]
let ``epCandidates offers only consistent squares`` () =
    // White to move, black pawn double-stepped to e5: target e6 and origin e7 empty.
    Assert.Equal<string[]>([| "e6" |], epCandidates "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 2")
    // Origin still occupied (pawn on e7 too): no candidate.
    Assert.Empty(epCandidates "rnbqkbnr/pppppppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 2")

[<Fact>]
let ``tryMakeMove applies legal moves and rejects the rest`` () =
    match tryMakeMove startpos "e2e4" with
    | Some fen ->
        Assert.StartsWith("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq", fen)
        Assert.True((validateFen fen).IsValid)
    | None -> failwith "e2e4 should be legal"
    Assert.True((tryMakeMove startpos "e2e5").IsNone)
    Assert.True((tryMakeMove "not a fen" "e2e4").IsNone)

[<Fact>]
let ``tryMakeMove handles castling and promotion`` () =
    // UCI castling spelling comes from the generator itself, so the test can't get
    // the e1g1-vs-e1h1 convention wrong.
    let f = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    let shortCastle = (legalMovesOf f) |> Array.find (fun m -> m.IsCastling && m.San = "0-0")
    match tryMakeMove f shortCastle.Uci with
    | Some fen ->
        Assert.Equal('K', pieceAt fen "g1")
        Assert.Equal('R', pieceAt fen "f1")
        Assert.Contains(" b kq ", fen)
    | None -> failwith "castling should be legal"
    match tryMakeMove "4k3/P7/8/8/8/8/8/4K3 w - - 0 1" "a7a8q" with
    | Some fen -> Assert.Equal('Q', pieceAt fen "a8")
    | None -> failwith "promotion should be legal"

// --- short-SAN input (tryParseSan/tryMakeSanMove/pvToUci) -------

[<Fact>]
let ``tryParseSan resolves moves and tolerates decorations`` () =
    Assert.Equal(Some "g1f3", tryParseSan startpos "Nf3")
    Assert.Equal(Some "g1f3", tryParseSan startpos "Nf3!?")
    Assert.Equal(Some "e2e4", tryParseSan startpos "e4")
    Assert.True((tryParseSan startpos "Nf6").IsNone)   // black's move, not legal for white
    // Promotion: both "=Q" and bare "Q" spellings resolve.
    Assert.Equal(Some "a7a8q", tryParseSan "4k3/P7/8/8/8/8/8/4K3 w - - 0 1" "a8=Q")
    Assert.Equal(Some "a7a8q", tryParseSan "4k3/P7/8/8/8/8/8/4K3 w - - 0 1" "a8Q")

[<Fact>]
let ``tryParseSan accepts over-disambiguated SAN when unique`` () =
    // Review finding: python-chess and many PGN exporters over-disambiguate; the exact
    // match against the generator's minimal SAN rejected them.
    Assert.Equal(Some "g1f3", tryParseSan startpos "Ngf3")
    Assert.Equal(Some "e2e4 e7e5 g1f3", pvToUci startpos "1.e4 e5 2.Ngf3")
    // Rank-hint form on a unique rook, and a hint that matches nothing stays None.
    let rook = "4k3/8/8/8/8/8/8/R3K3 w - - 0 1"
    Assert.Equal(Some "a1a2", tryParseSan rook "R1a2")
    Assert.True((tryParseSan rook "R5a2").IsNone)
    // Under-specified ambiguity is still rejected, with or without the fallback.
    Assert.True((tryParseSan "4k3/8/8/R7/8/8/8/R3K3 w - - 0 1" "Ra3").IsNone)

[<Fact>]
let ``tryParseSan requires disambiguation and accepts both castling spellings`` () =
    // Knights on b1 and f3 both reach d2: bare "Nd2" is ambiguous -> None.
    let twoKnights = "4k3/8/8/8/8/5N2/8/1N2K3 w - - 0 1"
    Assert.True((tryParseSan twoKnights "Nd2").IsNone)
    Assert.Equal(Some "b1d2", tryParseSan twoKnights "Nbd2")
    Assert.Equal(Some "f3d2", tryParseSan twoKnights "Nfd2")
    let castle = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    Assert.True((tryParseSan castle "0-0").IsSome)
    Assert.Equal(tryParseSan castle "0-0", tryParseSan castle "O-O")

[<Fact>]
let ``every generated SAN parses back to its own UCI`` () =
    // Roundtrip over full move lists — a free consistency check of the SAN emitter.
    for fen in [ startpos; "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" ] do
        for m in legalMovesOf fen do
            Assert.Equal(Some m.Uci, tryParseSan fen m.San)

[<Fact>]
let ``tryMakeSanMove composes parse and apply`` () =
    match tryMakeSanMove startpos "e4" with
    | Some fen -> Assert.StartsWith("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b", fen)
    | None -> failwith "e4 should apply"

[<Fact>]
let ``pvToUci walks mixed SAN and UCI lines with move numbers`` () =
    Assert.Equal(Some "e2e4 e7e5 g1f3", pvToUci startpos "1.e4 e5 2.Nf3")
    Assert.Equal(Some "e2e4 e7e5 g1f3", pvToUci startpos "e2e4 e5 Nf3")
    Assert.Equal(Some "e2e4 g8f6", pvToUci startpos "1.e4 1...Nf6")
    Assert.Equal(Some "", pvToUci startpos "")
    Assert.True((pvToUci startpos "1.e4 e4").IsNone)          // illegal for black
    Assert.True((pvToUci startpos "1.e4 e5 2.Qd4").IsNone)    // queen blocked by the d2 pawn

// --- epdOf ----------------------------------------------------------------

[<Fact>]
let ``epdOf writes spec-formatted opcodes with SAN canonicalization`` () =
    let line = epdOf startpos [ ("bm", EpdMoves [| "g1f3"; "e4" |]); ("id", EpdStr "test-1"); ("dm", EpdInt 5); ("ce", EpdFloat 1.25) ]
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - bm Nf3 e4; id \"test-1\"; dm 5; ce 1.25;", line)

[<Fact>]
let ``epdOf walks pv operands and spells castling O-O for interop`` () =
    Assert.EndsWith("pv e4 e5 Nf3;", epdOf startpos [ ("pv", EpdPv "1.e4 e5 2.Nf3") ])
    let castle = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    Assert.EndsWith("bm O-O;", epdOf castle [ ("bm", EpdMove "0-0") ])

[<Fact>]
let ``epdOf rejects illegal moves, bad opcode names and embedded quotes`` () =
    Assert.Throws<ArgumentException>(fun () -> epdOf startpos [ ("bm", EpdMove "Nf6") ] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> epdOf startpos [ ("bad name", EpdInt 1) ] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> epdOf startpos [ ("id", EpdStr "a\"b") ] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> epdOf "not a fen" [] |> ignore) |> ignore

[<Fact>]
let ``readEPDs parses epdOf output back`` () =
    // The writer must produce what EB's own EPD reader understands.
    let line = epdOf startpos [ ("bm", EpdMove "Nf3"); ("id", EpdStr "rt-1") ]
    let path = System.IO.Path.GetTempFileName()
    try
        System.IO.File.WriteAllLines(path, [| line |])
        let entries = ChessLibrary.EPDExtractor.readEPDs path |> Seq.toArray
        Assert.Equal(1, entries.Length)
        Assert.StartsWith("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -", entries.[0].FEN)
        Assert.Equal(Some "Nf3", entries.[0].BestMove)
        Assert.Equal(Some "rt-1", entries.[0].Id)
    finally
        System.IO.File.Delete path

// --- status/predicate parity (50/75-move, zeroing/irreversible, ep check) --

[<Fact>]
let ``validateFen rejects an inconsistent en-passant square`` () =
    // Well-formed ep field but no pawn double-stepped to e5: classic paste error.
    let v = validateFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 1"
    Assert.False(v.IsValid)
    Assert.Contains(v.Errors, fun (e: string) -> e.Contains "not consistent")
    // The genuine double-step still validates (both colors).
    Assert.True((validateFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").IsValid)
    Assert.True((validateFen "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 2").IsValid)

[<Fact>]
let ``getPositionStatus reports halfmove-counter draws`` () =
    let at n = sprintf "4k3/8/8/8/8/8/4B3/4K3 w - - %d 80" n
    let s99 = getPositionStatus (at 99)
    Assert.False(s99.FiftyMoveDraw)
    let s100 = getPositionStatus (at 100)
    Assert.True(s100.FiftyMoveDraw)
    Assert.False(s100.SeventyFiveMoveDraw)
    let s150 = getPositionStatus (at 150)
    Assert.True(s150.FiftyMoveDraw)
    Assert.True(s150.SeventyFiveMoveDraw)
    // Checkmate trumps the counter: no claimable draw in a finished game.
    let mate = getPositionStatus "R3k3/8/4K3/8/8/8/8/8 b - - 100 80"
    Assert.Equal("checkmate", mate.Status)
    Assert.False(mate.FiftyMoveDraw)

[<Fact>]
let ``legalMovesOf flags zeroing and irreversible moves`` () =
    let moves = legalMovesOf startpos
    let byUci u = moves |> Array.find (fun m -> m.Uci = u)
    // Pawn move: zeroing (and thus irreversible); knight move: neither.
    Assert.True((byUci "e2e4").IsZeroing)
    Assert.True((byUci "e2e4").IsIrreversible)
    Assert.False((byUci "g1f3").IsZeroing)
    Assert.False((byUci "g1f3").IsIrreversible)
    // King/rook moves off castling-right squares: irreversible but not zeroing.
    let castle = legalMovesOf "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    let kingMove = castle |> Array.find (fun m -> m.Uci = "e1d1")
    Assert.False(kingMove.IsZeroing)
    Assert.True(kingMove.IsIrreversible)
    let rookMove = castle |> Array.find (fun m -> m.Uci = "h1g1")
    Assert.True(rookMove.IsIrreversible)
    // No rights left: the same king move is reversible.
    let noRights = legalMovesOf "r3k2r/8/8/8/8/8/8/R3K2R w - - 0 1"
    Assert.False((noRights |> Array.find (fun m -> m.Uci = "e1d1")).IsIrreversible)

[<Fact>]
let ``a legal en passant makes every move irreversible`` () =
    // White d5 pawn can capture e5 en passant: the right is use-it-or-lose-it.
    let fen = "4k3/8/8/3Pp3/8/8/8/4K3 w - e6 0 2"
    let moves = legalMovesOf fen
    Assert.True(moves |> Array.exists (fun m -> m.IsEnPassant))
    Assert.True(moves |> Array.forall (fun m -> m.IsIrreversible))
    // Same position without the ep right: king moves are reversible again.
    let without = legalMovesOf "4k3/8/8/3Pp3/8/8/8/4K3 w - - 0 2"
    Assert.False((without |> Array.find (fun m -> m.Uci = "e1d1")).IsIrreversible)

// --- BoardSvg -------------------------------------------------------------

[<Fact>]
let ``render produces valid XML with the expected board structure`` () =
    let svg = BoardSvg.render startpos false [] [] (Some startpos)
    // Well-formed XML is the load-bearing property — viewers reject anything less.
    let doc = System.Xml.XmlDocument()
    doc.LoadXml svg
    // 64 squares + the frame rect
    let rects = doc.GetElementsByTagName "rect"
    Assert.Equal(65, rects.Count)
    // 32 pieces as <use>, 12 distinct defs referenced
    Assert.Equal(32, doc.GetElementsByTagName("use").Count)
    Assert.Equal(12, doc.GetElementsByTagName("g") |> Seq.cast<System.Xml.XmlNode>
                     |> Seq.filter (fun n -> n.Attributes.["id"] <> null) |> Seq.length)
    // coordinates + caption
    Assert.Equal(17, doc.GetElementsByTagName("text").Count)

[<Fact>]
let ``render draws overlay shapes flip-aware`` () =
    let arrows : BoardSvg.SvgArrow list = [ { From = "g1"; To = "f3"; Color = "#123456"; Opacity = 0.6; Width = 1.0; Head = "arrow" } ]
    let circles : BoardSvg.SvgCircle list = [ { Square = "e4"; Color = "#654321"; Opacity = 0.4; Size = 0.9 } ]
    // The piece art itself contains circles/lines inside <defs> — overlay counts must
    // exclude that subtree.
    let overlay (doc: System.Xml.XmlDocument) (tag: string) =
        doc.SelectNodes(sprintf "//*[local-name()='%s' and not(ancestor::*[local-name()='defs'])]" tag)
    let svg = BoardSvg.render startpos false arrows circles None
    let doc = System.Xml.XmlDocument()
    doc.LoadXml svg
    Assert.Equal(1, (overlay doc "polygon").Count)   // arrow head
    let circle = (overlay doc "circle").[0]
    // e4 center, white orientation: x = 20 + 4.5*45 = 222.5, y = 20 + 4.5*45 = 222.5
    Assert.Equal("222.5", circle.Attributes.["cx"].Value)
    Assert.Equal("222.5", circle.Attributes.["cy"].Value)
    // flipped: e4 mirrors to (3.5, 3.5) squares -> 177.5
    let flipped = BoardSvg.render startpos true arrows circles None
    let doc2 = System.Xml.XmlDocument()
    doc2.LoadXml flipped
    Assert.Equal("177.5", (overlay doc2 "circle").[0].Attributes.["cx"].Value)

[<Fact>]
let ``renderWithInsights draws the scholars-mate threat overlay`` () =
    // Qf7# threat position: hanging f7, checkers after ... — use the position insights
    // themselves as the oracle: shape counts must match the detector output.
    let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPPQPPP/RNB1K2R b KQkq - 0 1"
    let ins = getPositionInsights fen
    let arrowCount = (BoardSvg.insightArrows ins true true true).Length
    let circleCount = (BoardSvg.insightCircles ins true false true true).Length
    let svg = BoardSvg.renderWithInsights fen
    let doc = System.Xml.XmlDocument()
    doc.LoadXml svg
    let overlay (tag: string) =
        doc.SelectNodes(sprintf "//*[local-name()='%s' and not(ancestor::*[local-name()='defs'])]" tag).Count
    let lines = overlay "line"
    let polys = overlay "polygon"
    let circles = overlay "circle"
    Assert.Equal(circleCount, circles)
    // every arrow renders exactly one line; "arrow" heads add one polygon each
    Assert.Equal(arrowCount, lines)
    Assert.Equal(polys, arrowCount - (BoardSvg.insightArrows ins true true true |> List.filter (fun a -> a.Head = "line") |> List.length))

[<Fact>]
let ``validateFen rejects side not to move in check`` () =
    // White to move while the BLACK king sits in check from Ra8 — illegal position.
    let v = validateFen "R3k3/8/8/8/8/8/8/4K3 w - - 0 1"
    Assert.False(v.IsValid)
    Assert.Contains(v.Errors, fun (e: string) -> e.Contains "side not to move")
    // ...but perfectly legal with black to move
    Assert.True((validateFen "R3k3/8/8/8/8/8/8/4K3 b - - 0 1").IsValid)

[<Fact>]
let ``validateFen accumulates multiple errors`` () =
    // 9 files in rank 7 AND a bad side-to-move field
    let v = validateFen "rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1"
    Assert.False(v.IsValid)
    Assert.True(v.Errors.Length >= 2)

// --- legalMovesOf / getPositionStatus -------------------------------------

[<Fact>]
let ``start position has twenty legal moves in both notations`` () =
    for stm in [ "w"; "b" ] do
        let ms = legalMovesOf (sprintf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR %s KQkq - 0 1" stm)
        Assert.Equal(20, ms.Length)
        let uci = if stm = "w" then "e2e4" else "e7e5"
        let san = if stm = "w" then "e4" else "e5"
        Assert.True(ms |> Array.exists (fun m -> m.Uci = uci))
        Assert.True(ms |> Array.exists (fun m -> m.San = san))

[<Fact>]
let ``start position moves carry no flags`` () =
    let ms = legalMovesOf "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.True(ms |> Array.forall (fun m ->
        not m.IsCapture && not m.IsCastling && not m.IsEnPassant && not m.GivesCheck))

[<Fact>]
let ``move flags: capture, en passant, castling and gives check`` () =
    // plain capture
    let cap = legalMovesOf "4k3/8/8/3n4/8/8/3R4/4K3 w - - 0 1" |> Array.find (fun m -> m.Uci = "d2d5")
    Assert.True(cap.IsCapture)
    Assert.False(cap.IsEnPassant)
    // en passant: black just played d7d5, exd6 is both capture and ep
    let ep = legalMovesOf "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1" |> Array.find (fun m -> m.Uci = "e5d6")
    Assert.True(ep.IsEnPassant)
    Assert.True(ep.IsCapture)
    // castling: exactly one castle move with rights "Q"; castling here gives no check
    let castles = legalMovesOf "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1" |> Array.filter (fun m -> m.IsCastling)
    Assert.Equal(1, castles.Length)
    Assert.False(castles.[0].GivesCheck)
    // gives check: Ra8+ along the freed 8th rank; the quiet Ra2 does not
    let ms = legalMovesOf "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1" |> Seq.map (fun m -> m.Uci, m) |> Map.ofSeq
    Assert.True(ms.["a1a8"].GivesCheck)
    Assert.False(ms.["a1a2"].GivesCheck)
    // same probe through the black frame
    let bs = legalMovesOf "r3k3/8/8/8/8/8/8/4K3 b q - 0 1" |> Seq.map (fun m -> m.Uci, m) |> Map.ofSeq
    Assert.True(bs.["a8a1"].GivesCheck)
    Assert.False(bs.["a8a7"].GivesCheck)

[<Fact>]
let ``status detects checkmate stalemate check and ok`` () =
    Assert.Equal("ok", (getPositionStatus "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").Status)
    // fool's mate: 1.f3 e5 2.g4 Qh4#
    Assert.Equal("checkmate", (getPositionStatus "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3").Status)
    // Kh8 boxed by Qf7 + Kg6, not in check
    Assert.Equal("stalemate", (getPositionStatus "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1").Status)
    Assert.Equal("check", (getPositionStatus "R3k3/8/8/8/8/8/8/4K3 b - - 0 1").Status)

[<Fact>]
let ``insufficient material follows the dead-position approximation`` () =
    Assert.True((getPositionStatus "8/8/4k3/8/8/4K3/8/8 w - - 0 1").InsufficientMaterial)      // K vs K
    Assert.True((getPositionStatus "8/8/4k3/8/2B5/4K3/8/8 w - - 0 1").InsufficientMaterial)    // KB vs K
    Assert.True((getPositionStatus "8/8/4k3/8/2B1B3/4K3/8/8 w - - 0 1").InsufficientMaterial)  // bishops on one color
    Assert.False((getPositionStatus "8/8/4k3/8/1B2B3/4K3/8/8 w - - 0 1").InsufficientMaterial) // opposite-color bishops
    Assert.False((getPositionStatus "8/8/4k3/8/2B2N2/4K3/8/8 w - - 0 1").InsufficientMaterial) // two minors incl. knight
    Assert.False((getPositionStatus "8/8/4k3/8/2B5/3PK3/8/8 w - - 0 1").InsufficientMaterial)  // pawn on the board

// --- duality property: attacksFrom <-> attackersOf ------------------------

/// Independent FEN parse (deliberately NOT using ChessLibrary) mapping occupied
/// squares to their color, so the duality check has no shared code with the API.
let private occupiedSquares (fen: string) =
    let placement = fen.Split(' ').[0]
    let ranks = placement.Split('/')
    [ for r in 0 .. 7 do
        let rankStr = ranks.[r]
        let rank = 7 - r
        let mutable file = 0
        for c in rankStr do
            if Char.IsDigit c then file <- file + (int c - int '0')
            else
                let sq = sprintf "%c%d" (char (int 'a' + file)) (rank + 1)
                yield sq, (if Char.IsUpper c then "w" else "b")
                file <- file + 1 ]

let private assertDuality (fen: string) =
    let occupied = occupiedSquares fen
    let colorOf = dict occupied
    for (sq, color) in occupied do
        // forward: every square the piece attacks lists it as an attacker of that color
        for target in attacksFrom fen sq do
            let atts = attackersOf fen target
            let bucket = if color = "w" then atts.White else atts.Black
            Assert.True(Array.contains sq bucket,
                sprintf "%s: %s attacks %s but is missing from attackersOf" fen sq target)
    // backward: every listed attacker really attacks the square
    for file in 'a' .. 'h' do
        for rank in '1' .. '8' do
            let target = string file + string rank
            let atts = attackersOf fen target
            for attacker in Array.append atts.White atts.Black do
                Assert.True(colorOf.ContainsKey attacker, sprintf "%s: attacker %s of %s is an empty square" fen attacker target)
                Assert.True(Array.contains target (attacksFrom fen attacker),
                    sprintf "%s: attackersOf(%s) lists %s but attacksFrom disagrees" fen target attacker)

[<Fact>]
let ``attacksFrom and attackersOf are dual on fixed positions`` () =
    [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"   // kiwipete
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1"
      "4k3/4r3/8/8/4Q3/8/8/4K3 w - - 0 1"
      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 b - - 0 1" ]                            // TalkChess pos 3
    |> List.iter assertDuality

[<Fact>]
let ``attacksFrom and attackersOf are dual along random games`` () =
    // Random walks from the start position: every visited position must satisfy the
    // duality. Fixed seeds keep the walk deterministic.
    for seed in [ 1; 7; 42 ] do
        let rnd = Random(seed)
        let mutable board = Chess.Board()
        board.LoadFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        for _ in 1 .. 12 do
            makeRandomMove rnd &board |> ignore
            assertDuality (board.FEN())
