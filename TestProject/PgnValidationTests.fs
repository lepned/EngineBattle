module PgnValidationTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.PgnValidation

// ---------------------------------------------------------------------------
// pgnvalidate: every move against the legal moves of its position. An illegal or ambiguous move
// (or a FEN that cannot be set up) is an error and ends the game's check; a legal move written
// otherwise than standard SAN is a warning.
// ---------------------------------------------------------------------------

let private game (moves: string) (fen: string option) =
  let fenTags = match fen with Some f -> sprintf "[SetUp \"1\"]\n[FEN \"%s\"]\n" f | None -> ""
  let path = Path.Combine(Path.GetTempPath(), "eb-pgnvalidate-" + Guid.NewGuid().ToString "N" + ".pgn")
  File.WriteAllText(path, sprintf "[Event \"t\"]\n[Round \"1\"]\n[White \"A\"]\n[Black \"B\"]\n%s[Result \"*\"]\n\n%s *\n" fenTags moves)
  FullPGNParser.parsePgnFile path |> Seq.head

let private validate moves fen = validateGame (Chess.Board()) (game moves fen)

[<Fact>]
let ``a correct game has no findings and is replayed to the end`` () =
  let findings, played = validate "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. O-O Nf6 5. Re1+ Be7" None
  Assert.Empty(findings)
  Assert.Equal(10, played)

[<Fact>]
let ``an illegal move is an error and ends the game's check`` () =
  let findings, played = validate "1. e4 e5 2. Ke3 Nc6 3. Nf3" None
  let f = Assert.Single(findings)
  Assert.Equal(IllegalMove, f.Kind)
  Assert.Equal((3, "2. Ke3"), (f.Ply, f.Move))
  Assert.Equal("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2", f.Fen)
  Assert.Equal(2, played)

[<Fact>]
let ``a move two pieces can make is an error, not a guess`` () =
  let findings, played = validate "1. d4 d5 2. Nf3 Nf6 3. e3 e6 4. Nd2 Be7" None
  let f = Assert.Single(findings)
  Assert.Equal(AmbiguousMove, f.Kind)
  Assert.Equal("Nbd2 or Nfd2", f.Expected)
  Assert.Equal(6, played)

[<Fact>]
let ``a legal move written otherwise than standard SAN is a warning and the game goes on`` () =
  let findings, played = validate "1. e2e4 e5 2. Nf3 Nf6 3. Ne5 d6 4. Nef3 Nc6" None
  Assert.Equal<(Kind * string) list>(
    [ NonStandardSan, "e4"; NonStandardSan, "Nxe5"; NonStandardSan, "Nf3" ],
    findings |> List.map (fun f -> f.Kind, f.Expected))
  Assert.Equal(8, played)

[<Fact>]
let ``check marks, zeros in castling and = in promotions are not differences`` () =
  let findings, _ = validate "1. e4 d5 2. exd5 Qxd5 3. Nc3 Qa5 4. Nf3 Nf6 5. Bc4 Bg4 6. 0-0 e6 7. h3 Bh5 8. g4 Bg6 9. Ne5 Nbd7 10. Nxg6 hxg6" None
  Assert.Empty(findings)
  let findings, _ = validate "1. e8Q+" (Some "8/4P3/8/8/8/8/8/k6K w - - 0 1")
  Assert.Empty(findings)

[<Fact>]
let ``a FEN that cannot be set up is an error`` () =
  let findings, played = validate "1. e4" (Some "rnbqkbnr/pppp/8 w")
  let f = Assert.Single(findings)
  Assert.Equal(BadStart, f.Kind)
  Assert.Equal(0, played)

// three queens can reach f6 (h6, e7, h8) - the TCEC S30 position that showed the case
let private threeQueens = Some "1K5Q/4Q3/Bp1R3Q/1PpP2p1/2b3Pb/1qn1Rn2/6k1/q4r2 w - - 5 103"

[<Fact>]
let ``a move written with more disambiguation than needed is a warning, not ambiguous`` () =
  let findings, played = validate "103. Qh6f6" threeQueens
  let f = Assert.Single(findings)
  Assert.Equal((NonStandardSan, "Q6f6"), (f.Kind, f.Expected))
  Assert.Equal(1, played)

[<Fact>]
let ``disambiguation that still leaves two pieces is ambiguous`` () =
  let findings, _ = validate "103. Qhf6" threeQueens
  let f = Assert.Single(findings)
  Assert.Equal((AmbiguousMove, "Q6f6 or Q8f6"), (f.Kind, f.Expected))

[<Fact>]
let ``disambiguation that names no piece able to move there is illegal`` () =
  let findings, _ = validate "103. Qaf6" threeQueens
  Assert.Equal(IllegalMove, (Assert.Single(findings)).Kind)

[<Fact>]
let ``a move that names where it starts must start there`` () =
  // d5 for exd5: a pawn capture written as a push
  let findings, played = validate "1. e4 d5 2. d5" None
  Assert.Equal((IllegalMove, 2), ((Assert.Single(findings)).Kind, played))
  // fxd5: the capture named from the wrong file
  let findings, _ = validate "1. e4 d5 2. fxd5" None
  Assert.Equal(IllegalMove, (Assert.Single(findings)).Kind)
  // Ngb5 with the only knight that can go there on c3
  let findings, _ = validate "1. Nc3 e5 2. Ngb5" None
  Assert.Equal(IllegalMove, (Assert.Single(findings)).Kind)
  // the forgiving spellings stay warnings: no x, the knight's own file, coordinates, ed5
  let findings, played = validate "1. e4 d5 2. ed5 Qd5 3. Nbc3 Qa5 4. g1f3 Nf6" None
  Assert.Equal<Kind list>([ NonStandardSan; NonStandardSan; NonStandardSan; NonStandardSan ], findings |> List.map _.Kind)
  Assert.Equal(8, played)
