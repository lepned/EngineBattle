/// Polyglot books (.bin): the position key against the format's published test keys, the move
/// encoding (castles written as king takes rook), and opening lines drawn from a small book made here.
module PolyglotTests

open System
open System.IO
open System.Buffers.Binary
open Xunit
open ChessLibrary

// the test keys of the format description (hgm.nubati.net/book_format.html)
[<Theory>]
[<InlineData("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "463b96181691fc9c")>]
[<InlineData("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", "823c9b50fd114196")>]
[<InlineData("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", "0756b94461c50fb0")>]
[<InlineData("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", "662fafb965db29d4")>]
[<InlineData("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3", "22a48b5a8e47ff78")>]
[<InlineData("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3", "652a607ca3f242c1")>]
[<InlineData("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 0 4", "00fdd303c946bdd9")>]
[<InlineData("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3", "3c8123ea7b067637")>]
[<InlineData("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 0 4", "5c3f9b829b279560")>]
let ``the position key is the format's own`` (fen: string, key: string) =
    Assert.Equal(Convert.ToUInt64(key, 16), PolyglotFormat.keyOfFen fen)

/// A move in Polyglot's bit field.
let private encode (uci: string) =
    let file (c: char) = int c - int 'a'
    let row (c: char) = int c - int '1'
    let promo = if uci.Length > 4 then (match uci.[4] with 'n' -> 1 | 'b' -> 2 | 'r' -> 3 | _ -> 4) else 0
    uint16 (file uci.[2] ||| (row uci.[3] <<< 3) ||| (file uci.[0] <<< 6) ||| (row uci.[1] <<< 9) ||| (promo <<< 12))

[<Fact>]
let ``moves decode to UCI, a castle written as king takes rook included`` () =
    let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.Equal("e2e4", PolyglotFormat.moveToUci start (encode "e2e4"))
    Assert.Equal("a7a8q", PolyglotFormat.moveToUci start (encode "a7a8q"))
    let castles = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    Assert.Equal("e1g1", PolyglotFormat.moveToUci castles (encode "e1h1"))
    Assert.Equal("e1c1", PolyglotFormat.moveToUci castles (encode "e1a1"))
    Assert.Equal("e8g8", PolyglotFormat.moveToUci castles (encode "e8h8"))
    // no king on e1: a rook move along the rank is what it says
    Assert.Equal("e1h1", PolyglotFormat.moveToUci "4k3/8/8/8/8/8/8/R3R1K1 w - - 0 1" (encode "e1h1"))

/// A book file from (fen, uci, weight) rows, sorted by key as the format wants.
let private writeBook (rows: (string * string * int) list) =
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_polyglot_%s.bin" (Guid.NewGuid().ToString("N")))
    let entries = rows |> List.map (fun (fen, uci, w) -> PolyglotFormat.keyOfFen fen, encode uci, uint16 w) |> List.sortBy (fun (k, _, _) -> k)
    use f = File.Create path
    let buf = Array.zeroCreate<byte> 16
    for k, m, w in entries do
        BinaryPrimitives.WriteUInt64BigEndian(Span<byte>(buf, 0, 8), k)
        BinaryPrimitives.WriteUInt16BigEndian(Span<byte>(buf, 8, 2), m)
        BinaryPrimitives.WriteUInt16BigEndian(Span<byte>(buf, 10, 2), uint16 w)
        BinaryPrimitives.WriteUInt32BigEndian(Span<byte>(buf, 12, 4), 0u)
        f.Write(buf, 0, 16)
    path

let private start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let private afterE4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
let private afterD4 = "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq d3 0 1"

[<Fact>]
let ``lines follow the book's moves, differ, and come again with the same seed`` () =
    let path =
        writeBook [ start, "e2e4", 3; start, "d2d4", 1; start, "a2a3", 0      // weight 0: never played
                    afterE4, "e7e5", 1; afterE4, "c7c5", 1; afterD4, "d7d5", 1 ]
    try
        let lines = PolyglotLines.lines path 10 7 0
        let texts = lines |> Array.map (fun g -> g.Mainline |> Seq.map (fun m -> m.San) |> String.concat " ") |> Array.sort
        // the book holds three lines, and only three come out
        Assert.Equal<string[]>([| "d4 d5"; "e4 c5"; "e4 e5" |], texts)
        Assert.Equal<string[]>(texts, PolyglotLines.lines path 10 7 0 |> Array.map (fun g -> g.Mainline |> Seq.map (fun m -> m.San) |> String.concat " ") |> Array.sort)
        // the first line, for the same seed, is the same line
        Assert.Equal((PolyglotLines.lines path 1 42 0).[0].Raw, (PolyglotLines.lines path 1 42 0).[0].Raw)
        // OpeningsPly caps the line
        Assert.True(PolyglotLines.lines path 5 7 1 |> Array.forall (fun g -> Seq.length g.Mainline = 1))
        Assert.Equal(1, (PolyglotLines.lines path 1 7 0).[0].GameNumber)
    finally File.Delete path

[<Fact>]
let ``a castle from the book is played as a castle`` () =
    let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
    // a book whose first position is not the start: drawLine starts at the start position and finds
    // nothing; the castle is checked through the decode and the board instead
    let uci = PolyglotFormat.moveToUci fen (encode "e1h1")
    let board = ChessLibrary.Chess.Board()
    board.LoadFen fen
    match BoardUtils.tryGetMoveAndSanFromUci &board uci with
    | Some (_, san) -> Assert.Contains(san, [ "O-O"; "0-0" ])
    | None -> failwith "the castle is not legal"

[<Fact>]
let ``a file that is not 16-byte entries is refused with its size`` () =
    let path = Path.GetTempFileName()
    try
        File.WriteAllBytes(path, Array.zeroCreate 17)
        let ex = Assert.Throws<InvalidDataException>(fun () -> PolyglotFormat.read path |> ignore)
        Assert.Contains("17 bytes", ex.Message)
    finally File.Delete path

[<Fact>]
let ``the analysis lookup on disk finds what the in-memory book holds`` () =
    let path =
        writeBook [ start, "e2e4", 3; start, "d2d4", 1; start, "a2a3", 0
                    afterE4, "e7e5", 1; afterE4, "c7c5", 1; afterD4, "d7d5", 1 ]
    try
        let inMemory = PolyglotFormat.read path
        let onDisk = PolyglotFormat.BookFiles.get path
        Assert.Equal(int64 inMemory.Length, onDisk.Count)
        for key in inMemory |> Array.map (fun e -> e.Key) |> Array.distinct do
            Assert.Equal<PolyglotFormat.Entry[]>(PolyglotFormat.entriesFor inMemory key, onDisk.Lookup key)
        // a position the book does not hold, keys below and above all of them
        for key in [ 0UL; UInt64.MaxValue; PolyglotFormat.keyOfFen "8/8/8/8/8/8/8/K6k w - - 0 1" ] do
            Assert.Empty(onDisk.Lookup key)
        // the book's moves for the start position: legal, weight above 0, heaviest first, shares
        let moves = PolyglotLines.movesFor path start
        Assert.Equal<string[]>([| "e4"; "d4" |], moves |> Array.map (fun m -> m.San))
        Assert.Equal<string[]>([| "e2e4"; "d2d4" |], moves |> Array.map (fun m -> m.Uci))
        Assert.Equal(75.0, moves.[0].Percent, 6)
        Assert.Equal(100.0, moves |> Array.sumBy (fun m -> m.Percent), 6)
        Assert.Empty(PolyglotLines.movesFor path "8/8/8/8/8/8/8/K6k w - - 0 1")
    finally
        (PolyglotFormat.BookFiles.get path :> IDisposable).Dispose()
        File.Delete path

[<Fact>]
let ``more lines keep the first ones: a larger Rounds keeps the openings of a smaller one`` () =
    let path =
        writeBook [ start, "e2e4", 3; start, "d2d4", 1; afterE4, "e7e5", 1; afterE4, "c7c5", 1; afterD4, "d7d5", 1 ]
    try
        let text (g: PGNTypes.PgnGame) = g.Mainline |> Seq.map (fun m -> m.San) |> String.concat " "
        let two = PolyglotLines.lines path 2 9 0 |> Array.map text
        let three = PolyglotLines.lines path 3 9 0 |> Array.map text
        Assert.Equal<string[]>(two, three.[.. 1])
        // the start position is what tells a book for standard chess from a file of the right size
        Assert.True(PolyglotFormat.BookFiles.hasStartPosition path)
        let other = writeBook [ afterE4, "e7e5", 1 ]
        try Assert.False(PolyglotFormat.BookFiles.hasStartPosition other)
        finally File.Delete other
    finally File.Delete path

// ---------------------------------------------------------------- making and exporting books

let private pgnGames (text: string) = ChessLibrary.FullPGNParser.parsePgnString text |> Seq.toArray

[<Fact>]
let ``a move encodes as the format writes it, and decodes back, castles and promotions included`` () =
    let castles = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
    for uci in [ "e1g1"; "e1c1" ] do
        Assert.Equal(uci, PolyglotFormat.moveToUci castles (PolyglotFormat.encodeMove uci true))
    Assert.Equal(encode "e1h1", PolyglotFormat.encodeMove "e1g1" true)
    // a queen from e1 to g1 is not a castle
    Assert.Equal(encode "e1g1", PolyglotFormat.encodeMove "e1g1" false)
    Assert.Equal(encode "a7a8q", PolyglotFormat.encodeMove "a7a8q" false)
    Assert.Equal(encode "b2b1n", PolyglotFormat.encodeMove "b2b1n" false)

[<Fact>]
let ``a book made from games weighs each move by its results for the side that played it`` () =
    let game result moves = sprintf "[Event \"t\"]\n[Result \"%s\"]\n\n%s %s\n\n" result moves result
    let text =
        game "1-0" "1. e4 e5 2. Nf3"            // e4: a White win, 2
        + game "1/2-1/2" "1. e4 c5"              // e4: a draw, 1; c5: a draw for Black, 1
        + game "0-1" "1. e4 e5"                  // e4: 0; e5 now two games for Black: a loss and a win
        + game "1-0" "1. d4 d5"                  // d4 once
        + game "*" "1. c4"                       // no result: skipped
        + "[Event \"t\"]\n[Variant \"Chess960\"]\n[FEN \"bqrbknrn/p1pppppp/1p6/8/1P6/8/P1PPPPPP/BNNQRBKR w HEgc - 0 2\"]\n[Result \"1-0\"]\n\n2. e4 1-0\n\n"
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        let s = PolyglotLines.makeBook (pgnGames text) { MaxPly = 2; MinGames = 1; Uniform = false } path ignore
        Assert.Equal((6, 4, 2, 0), (s.Games, s.Used, s.Skipped, s.Broken))
        let at fen = PolyglotLines.movesFor path fen |> Array.map (fun m -> m.San, m.Weight) |> Array.toList
        Assert.Equal<(string * int) list>([ "e4", 3; "d4", 2 ], at start)
        Assert.Equal<(string * int) list>([ "e5", 2; "c5", 1 ], at afterE4)
        // the third ply is beyond MaxPly: Nf3 is not in the book
        Assert.Empty(PolyglotLines.movesFor path "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
        // a move needs MinGames games; uniform weighs every move 1
        PolyglotLines.makeBook (pgnGames text) { MaxPly = 2; MinGames = 2; Uniform = false } path ignore |> ignore
        Assert.Equal<(string * int) list>([ "e4", 3 ], at start)
        // uniform: results do not count, so the game without one (1. c4) is in too
        PolyglotLines.makeBook (pgnGames text) { MaxPly = 2; MinGames = 1; Uniform = true } path ignore |> ignore
        Assert.Equal<int list>([ 1; 1; 1 ], at start |> List.map snd)
    finally File.Delete path

[<Fact>]
let ``a castle in the games is written as the format writes it and read back as a castle`` () =
    let text = "[Event \"t\"]\n[Result \"1-0\"]\n\n1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 1-0\n\n"
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        PolyglotLines.makeBook (pgnGames text) { MaxPly = 8; MinGames = 1; Uniform = false } path ignore |> ignore
        let before = "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
        let moves = PolyglotLines.movesFor path before
        Assert.Equal<string[]>([| "e1g1" |], moves |> Array.map (fun m -> m.Uci))
        Assert.Contains(moves.[0].San, [ "O-O"; "0-0" ])
    finally File.Delete path

[<Fact>]
let ``the tree export follows every move with the share asked for, to the depth asked for`` () =
    let path =
        writeBook [ start, "e2e4", 3; start, "d2d4", 1; afterE4, "e7e5", 9; afterE4, "c7c5", 1; afterD4, "d7d5", 1 ]
    try
        let text (g: PGNTypes.PgnGame) = g.Mainline |> Seq.map (fun m -> m.San) |> String.concat " "
        let all, capped = PolyglotLines.exportTree path 10 0.0 100
        Assert.False capped
        Assert.Equal<string[]>([| "e4 e5"; "e4 c5"; "d4 d5" |], all |> Array.map text)
        // c5 has 10 % of its position, below 20: left out
        Assert.Equal<string[]>([| "e4 e5"; "d4 d5" |], PolyglotLines.exportTree path 10 20.0 100 |> fst |> Array.map text)
        // depth 1: the first moves only
        Assert.Equal<string[]>([| "e4"; "d4" |], PolyglotLines.exportTree path 1 0.0 100 |> fst |> Array.map text)
        // the cap
        let some, hit = PolyglotLines.exportTree path 10 0.0 2
        Assert.Equal(2, some.Length)
        Assert.True hit
    finally File.Delete path

[<Fact>]
let ``games in coordinates make the same book, and an empty book leaves the old one in place`` () =
    let san = "[Event \"t\"]\n[Result \"1-0\"]\n\n1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O 1-0\n\n"
    let coords = "[Event \"t\"]\n[Result \"1-0\"]\n\n1. e2e4 e7e5 2. g1f3 b8c6 3. f1c4 f8c5 4. e1g1 1-0\n\n"
    let a = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    let b = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        let opts = { MaxPly = 8; MinGames = 1; Uniform = false } : PolyglotLines.MakeOptions
        PolyglotLines.makeBook (pgnGames san) opts a ignore |> ignore
        let s = PolyglotLines.makeBook (pgnGames coords) opts b ignore
        Assert.Equal(0, s.Broken)
        Assert.Equal<byte[]>(File.ReadAllBytes a, File.ReadAllBytes b)
        // nothing reaches MinGames: no book is written, the one at the path stays
        let before = File.ReadAllBytes a
        let empty = PolyglotLines.makeBook (pgnGames san) { opts with MinGames = 5 } a ignore
        Assert.Equal(0, empty.Moves)
        Assert.Equal<byte[]>(before, File.ReadAllBytes a)
    finally
        File.Delete a
        File.Delete b

[<Fact>]
let ``a book open on the analysis page can be made again`` () =
    let text = "[Event \"t\"]\n[Result \"1-0\"]\n\n1. e4 e5 1-0\n\n"
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        let opts = { MaxPly = 2; MinGames = 1; Uniform = false } : PolyglotLines.MakeOptions
        PolyglotLines.makeBook (pgnGames text) opts path ignore |> ignore
        // the page holds it open
        Assert.NotEmpty(PolyglotLines.movesFor path start)
        PolyglotLines.makeBook (pgnGames (text + "[Event \"t\"]\n[Result \"1-0\"]\n\n1. d4 d5 1-0\n\n")) opts path ignore |> ignore
        Assert.Equal<string[]>([| "d4"; "e4" |], PolyglotLines.movesFor path start |> Array.map (fun m -> m.San) |> Array.sort)
        Assert.False(File.Exists(path + ".old"))
        Assert.False(File.Exists(path + ".tmp"))
    finally
        try File.Delete path with _ -> ()

[<Fact>]
let ``the tree export walks a transposed position once`` () =
    // 1. e4 e5 2. Nf3 and 1. Nf3 e5 2. e4 reach the same position, which continues with Nc6
    let afterE4E5 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    let afterNf3 = "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1"
    let afterNf3E5 = "rnbqkbnr/pppp1ppp/8/4p3/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2"
    let joined = "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
    let path =
        writeBook [ start, "e2e4", 1; start, "g1f3", 1; afterE4, "e7e5", 1; afterE4E5, "g1f3", 1
                    afterNf3, "e7e5", 1; afterNf3E5, "e2e4", 1; joined, "b8c6", 1 ]
    try
        let text (g: PGNTypes.PgnGame) = g.Mainline |> Seq.map (fun m -> m.San) |> String.concat " "
        let lines, _ = PolyglotLines.exportTree path 10 0.0 100
        Assert.Equal<string[]>([| "e4 e5 Nf3 Nc6" |], lines |> Array.map text)
    finally File.Delete path

[<Fact>]
let ``a SAN move with a rank in it is SAN, not coordinates`` () =
    // two rooks on the a-file: R1a3 names the one on a1
    let text = "[Event \"t\"]\n[FEN \"4k3/8/8/8/8/8/8/R3K2R w - - 0 1\"]\n[SetUp \"1\"]\n[Result \"1-0\"]\n\n1. Ra3 Kd7 1-0\n\n[Event \"t\"]\n[FEN \"k7/8/8/R7/8/8/8/R3K3 w - - 0 1\"]\n[SetUp \"1\"]\n[Result \"1-0\"]\n\n1. R1a3 Kb7 1-0\n\n"
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        let s = PolyglotLines.makeBook (pgnGames text) { MaxPly = 2; MinGames = 1; Uniform = false } path ignore
        Assert.Equal(0, s.Broken)
        Assert.Equal<string[]>([| "a1a3" |], PolyglotLines.movesFor path "k7/8/8/R7/8/8/8/R3K3 w - - 0 1" |> Array.map (fun m -> m.Uci))
    finally File.Delete path

[<Fact>]
let ``an opening file without results makes a book when every move weighs 1`` () =
    let text =
        "[Event \"Mainline\"]\n\n1. d4 d5 2. c4 e6 *\n\n"
        + "[Event \"Variation 1\"]\n\n1. d4 d5 2. c4 c6 *\n\n"
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_make_%s.bin" (Guid.NewGuid().ToString("N")))
    try
        // with results counting, lines ending in * say nothing: every game is skipped
        let scored = PolyglotLines.makeBook (pgnGames text) { MaxPly = 10; MinGames = 1; Uniform = false } path ignore
        Assert.Equal((2, 0), (scored.Skipped, scored.Moves))
        Assert.False(File.Exists path)
        // uniform: both lines go in, the branch after 2. c4 included
        let s = PolyglotLines.makeBook (pgnGames text) { MaxPly = 10; MinGames = 1; Uniform = true } path ignore
        Assert.Equal((0, 2), (s.Skipped, s.Used))
        let afterC4 = "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2"
        Assert.Equal<string[]>([| "c6"; "e6" |], PolyglotLines.movesFor path afterC4 |> Array.map (fun m -> m.San) |> Array.sort)
    finally File.Delete path
