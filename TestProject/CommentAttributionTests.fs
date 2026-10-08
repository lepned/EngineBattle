module CommentAttributionTests

open System
open System.Text.RegularExpressions
open Xunit
open ChessLibrary

// ---------------------------------------------------------------------------
// A move's engine comment belongs to the engine that played it. An older eb-cli sometimes put
// them on the other side's moves. The texts below are EngineBattle's own output (a 2026-10-08 match,
// A at 1,000 nodes and B at 20,000, trimmed): an opening book that ends with Black to move, and FEN
// starts with Black to move at move 1 and at move 30. Read back by the parser, as the GUI reads
// them, every comment must carry its own engine's nodes and a pv that starts with its own move.
// ---------------------------------------------------------------------------

let private bookEndsWithBlackToMove =
  """[Event "t"]
[White "A"]
[Black "B"]
[Result "*"]

1. e4 {book, mb=+0+0+0+0+0,} Nc6 {book, mb=+0+0+0+0+0,} 2. Nf3 {book, mb=+0+0+0+0+0,} d6 {book, mb=+0+0+0+0+0,} 3. d4 {book, mb=+0+0+0+0+0,} 3 ...e5 {wv=0.77, mt=18, s=1054368, eps=0, n=20033, d=13, sd=16, pd=d5, tl=0, tb=0, pv=3.... e5 4.d5 Nce7 5.c4 f5} 4. Bb5 {wv=0.67, mt=1, s=1005000, eps=0, n=1005, d=5, sd=11, pd=Bd7, tl=0, tb=0, pv=4.Bb5 exd4 5.Nxd4 Ne7} 4 ...exd4 {wv=0.59, mt=14, s=1334600, eps=0, n=20019, d=13, sd=20, pd=Nxd4, tl=0, tb=0, pv=4.... exd4 5.Nxd4 Bd7 6.Nc3} 5. Nxd4 {wv=0.82, mt=0, s=1000000, eps=0, n=1000, d=7, sd=11, pd=Bd7, tl=0, tb=0, pv=5.Nxd4 Bd7 6.Nxc6 bxc6 7.Ba4} 5 ...Bd7 {wv=0.45, mt=12, s=1538923, eps=0, n=20006, d=12, sd=18, pd=Nc3, tl=0, tb=0, pv=5.... Bd7 6.Nc3 Nf6} *
"""

let private fenBlackToMove (moveNumber: int) =
  let n = moveNumber
  sprintf """[Event "t"]
[White "B"]
[Black "A"]
[Result "*"]
[SetUp "1"]
[FEN "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 %d"]

%d ...c5 {wv=0.18, mt=1, s=1000000, eps=0, n=1000, d=5, sd=8, pd=Nf3, tl=0, tb=0, pv=%d.... c5 %d.Nf3 Nc6} %d. Nf3 {wv=0.38, mt=17, s=1177352, eps=0, n=20015, d=12, sd=18, pd=Nc6, tl=0, tb=0, pv=%d.Nf3 Nc6 %d.Bb5 g6} %d ...Nc6 {wv=0.20, mt=0, s=1000000, eps=0, n=1000, d=6, sd=10, pd=Nc3, tl=0, tb=0, pv=%d.... Nc6 %d.Nc3 Nf6} %d. Nc3 {wv=0.35, mt=15, s=1334333, eps=0, n=20015, d=13, sd=19, pd=e5, tl=0, tb=0, pv=%d.Nc3 e5 %d.Bc4 d6} *
"""
    n n n (n + 1) (n + 1) (n + 1) (n + 2) (n + 1) (n + 1) (n + 2) (n + 2) (n + 2) (n + 3)

/// Each engine move: (colour, the engine that played it, the engine its comment's nodes say, the
/// first move of its pv, the move itself)
let private engineMoves (pgn: string) =
  let game = FullPGNParser.parseFullPgnGame pgn
  let md = game.GameMetaData
  [ for m in game.Mainline do
      let nodes = Regex.Match(m.Comment, @"\bn=(\d+)")
      if nodes.Success then
        let pv = Regex.Match(m.Comment, @"pv=\d+\.+\s*(\S+)").Groups.[1].Value
        let player = if m.Color = "w" then md.White else md.Black
        let byNodes = if int nodes.Groups.[1].Value < 5000 then "A" else "B"
        yield m.Color, player, byNodes, pv, m.San ]

let private assertOwnComments (pgn: string) (colours: string list) =
  let moves = engineMoves pgn
  Assert.Equal<string list>(colours, moves |> List.map (fun (c, _, _, _, _) -> c))
  for colour, player, byNodes, pv, san in moves do
    Assert.True((player = byNodes), $"{san} ({colour}) is {player}'s move but carries {byNodes}'s comment")
    Assert.True((pv = san), $"{san} carries the pv of {pv}")

[<Fact>]
let ``a book that ends with Black to move: every engine comment on its own engine and move`` () =
  assertOwnComments bookEndsWithBlackToMove [ "b"; "w"; "b"; "w"; "b" ]

[<Fact>]
let ``a FEN start with Black to move: every engine comment on its own engine and move`` () =
  assertOwnComments (fenBlackToMove 1) [ "b"; "w"; "b"; "w" ]

[<Fact>]
let ``a FEN start with Black to move at move 30: every engine comment on its own engine and move`` () =
  assertOwnComments (fenBlackToMove 30) [ "b"; "w"; "b"; "w" ]
