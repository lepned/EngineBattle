module PgnStripTests

open System
open System.IO
open System.Text
open Xunit
open ChessLibrary

// ---------------------------------------------------------------------------
// pgnstrip: a game written again without comments, variations or NAGs keeps every tag as written
// and every main-line move, numbered as the standard wants it and wrapped at 80 characters.
// ---------------------------------------------------------------------------

let private file (text: string) (bom: bool) =
  let path = Path.Combine(Path.GetTempPath(), "eb-pgnstrip-" + Guid.NewGuid().ToString "N" + ".pgn")
  File.WriteAllText(path, text, UTF8Encoding(bom))
  path

let private stripped path =
  FullPGNParser.parsePgnFileWithRaw path |> Seq.map PGNWriter.strippedGameText |> String.concat "\n"

let private sample =
  "% an escape line\n" +
  "[Event \"e\"]\n[Site \"s\"]\n[Date \"2026.10.08\"]\n[Round \"1.2\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"1-0\"]\n" +
  "[GameTime \"12345\"]\n[Reason \"Checkmate\"]\n\n" +
  "1. e4 {book} e5 $1 (1... c5 2. Nf3 {a sideline}) 2. Nf3 {wv=0.31, d=20,\nmore on a second line} Nc6!? 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O\n" +
  "9. h3 Nb8 10. d4 Nbd7 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5 Nxe4 1-0\n\n" +
  "[Event \"e\"]\n[Site \"s\"]\n[Date \"2026.10.08\"]\n[Round \"2\"]\n[White \"B\"]\n[Black \"A\"]\n[Result \"1/2-1/2\"]\n" +
  "[SetUp \"1\"]\n[FEN \"8/8/8/4k3/8/8/4P3/4K3 b - - 0 30\"]\n\n30... Kd5 {only move} 31. Kd2 Kc4 1/2-1/2\n"

[<Fact>]
let ``comments, variations and NAGs go, every tag and main-line move stays`` () =
  let path = file sample true
  let text = stripped path
  Assert.DoesNotContain("{", text)
  Assert.DoesNotContain("(", text)
  Assert.DoesNotContain("$1", text)
  Assert.DoesNotContain("!?", text)
  // EngineBattle's own tags, which the parser keeps apart, are written as they stood
  Assert.Contains("[GameTime \"12345\"]\n[Reason \"Checkmate\"]\n", text)
  Assert.StartsWith("[Event \"e\"]\n", text)
  let before = FullPGNParser.parsePgnFile path |> List.ofSeq
  let after = FullPGNParser.parsePgnFile (file text false) |> List.ofSeq
  Assert.Equal(before.Length, after.Length)
  for b, a in List.zip before after do
    // the moves, their !? glyphs gone with the other annotations
    Assert.Equal<string list>(b.Mainline |> Seq.map (fun m -> m.San.TrimEnd('!', '?')) |> List.ofSeq, a.Mainline |> Seq.map _.San |> List.ofSeq)
    let tags (m: PGNTypes.GameMetadata) = m.Event, m.Site, m.Date, m.Round, m.White, m.Black, m.Result, m.Fen, m.Reason, m.GameTime, m.OtherTags
    Assert.Equal(tags b.GameMetaData, tags a.GameMetaData)

[<Fact>]
let ``a game Black starts is numbered 30... and lines stay within 80 characters`` () =
  let text = stripped (file sample false)
  Assert.Contains("\n\n30... Kd5 31. Kd2 Kc4 1/2-1/2\n", text)
  Assert.Contains("1. e4 e5 2. Nf3 Nc6 3. Bb5", text)
  Assert.All(text.Split('\n'), fun line -> Assert.True(line.Length <= 80, $"{line.Length} characters: {line}"))
  // the long game needed a second line
  Assert.Contains("\n", text.Substring(text.IndexOf "1. e4", text.IndexOf "1-0\n" - text.IndexOf "1. e4"))

[<Fact>]
let ``without the game's text the parsed tags are written, the standard seven first`` () =
  let game = FullPGNParser.parsePgnFile (file sample false) |> Seq.last
  let text = PGNWriter.strippedGameText game
  Assert.StartsWith("[Event \"e\"]\n[Site \"s\"]\n[Date \"2026.10.08\"]\n[Round \"2\"]\n[White \"B\"]\n[Black \"A\"]\n[Result \"1/2-1/2\"]\n", text)
  Assert.Contains("[FEN \"8/8/8/4k3/8/8/4P3/4K3 b - - 0 30\"]", text)
  Assert.EndsWith("\n\n30... Kd5 31. Kd2 Kc4 1/2-1/2\n", text)

let private numbers (pgn: string) =
  FullPGNParser.parsePgnFile (file pgn false) |> Seq.head |> fun g -> g.Mainline |> Seq.map (fun m -> m.MoveNumber, m.Color) |> List.ofSeq

[<Fact>]
let ``a game's move numbers start from its FEN`` () =
  let tags fen = sprintf "[Event \"e\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n%s\n" (match fen with Some f -> sprintf "[SetUp \"1\"]\n[FEN \"%s\"]\n" f | None -> "")
  Assert.Equal<(int * string) list>([ 1, "w"; 1, "b"; 2, "w" ], numbers (tags None + "1. e4 e5 2. Nf3 *\n"))
  Assert.Equal<(int * string) list>([ 30, "b"; 31, "w"; 31, "b" ], numbers (tags (Some "8/8/8/4k3/8/8/4P3/4K3 b - - 0 30") + "30... Kd5 31. Kd2 Kc4 *\n"))
  Assert.Equal<(int * string) list>([ 12, "w"; 12, "b"; 13, "w" ], numbers (tags (Some "8/8/8/4k3/8/8/4P3/4K3 w - - 0 12") + "12. Kd2 Kd5 13. Kd3 *\n"))
  // a variation is numbered like its line
  let g = FullPGNParser.parsePgnFile (file (tags (Some "8/8/8/4k3/8/8/4P3/4K3 b - - 0 30") + "30... Kd5 (30... Ke4 31. Kf2) 31. Kd2 *\n") false) |> Seq.head
  let variation = g.Mainline.[0].Variations.[0]
  Assert.Equal<(int * string) list>([ 30, "b"; 31, "w" ], variation |> Seq.map (fun m -> m.MoveNumber, m.Color) |> List.ofSeq)

[<Fact>]
let ``a record with tags and no moves leaves no tag twice in the next game`` () =
  let text =
    stripped (file ("[Event \"x\"]\n\n[Event \"x\"]\n[Site \"s\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"1-0\"]\n\n1. e4 e5 1-0\n") false)
  Assert.Equal("[Event \"x\"]\n[Site \"s\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"1-0\"]\n\n1. e4 e5 1-0\n", text)
