module PGNCommentTests

open Xunit
open ChessLibrary

/// The `[%key value]` convention inside PGN comments (clocks, evals, coloured squares and
/// arrows). Parsing must never lose prose: anything that is not a well-formed command has
/// to survive into the display text, because a comment is free text by the standard.

let private parse = PGNComment.parse

[<Fact>]
let ``splits commands from free text`` () =
    let p = parse "{ [%eval 0.24] Sharp line [%clk 0:03:00] }"
    Assert.Equal<(string * string) list>(
        [ "eval", "0.24"; "clk", "0:03:00" ], p.Commands)
    Assert.Equal("Sharp line", p.Text)

[<Fact>]
let ``a comment of only commands displays as nothing`` () =
    Assert.Equal("", PGNComment.displayText "[%eval -1.2] [%clk 0:00:14]")
    // ...and a comment with no commands is untouched apart from trimming
    Assert.Equal("book move", PGNComment.displayText "{ book move }")
    Assert.Equal("", PGNComment.displayText "")

[<Fact>]
let ``keeps arrow and square lists verbatim as values`` () =
    let p = parse "[%cal Ge2e4,Rd1h5] [%csl Gd4,Re5]"
    Assert.Equal(Some "Ge2e4,Rd1h5", PGNComment.tryFind "cal" p)
    Assert.Equal(Some "Gd4,Re5", PGNComment.tryFind "csl" p)
    Assert.Equal(None, PGNComment.tryFind "eval" p)

[<Fact>]
let ``malformed input degrades to text rather than being swallowed`` () =
    // Unterminated command: the whole thing is prose, not a silently dropped move note.
    let unterminated = parse "[%eval 0.3 and then some"
    Assert.Empty(unterminated.Commands)
    Assert.Equal("[%eval 0.3 and then some", unterminated.Text)
    // A plain bracket is not a command.
    let plain = parse "see [diagram] here"
    Assert.Empty(plain.Commands)
    Assert.Equal("see [diagram] here", plain.Text)
    // A command with no value still parses, with an empty value.
    Assert.Equal<(string * string) list>([ "novelty", "" ], (parse "[%novelty]").Commands)

[<Fact>]
let ``lookup is case-insensitive and takes the first occurrence`` () =
    let p = parse "[%Eval 1.0] [%eval 2.0]"
    Assert.Equal(Some "1.0", PGNComment.tryFind "EVAL" p)
    Assert.True(PGNComment.hasCommands "[%clk 0:01:00]")
    Assert.False(PGNComment.hasCommands "just a note")

[<Fact>]
let ``consecutive comments on one move are both kept`` () =
    // Lichess writes prose and machine data as separate braces; keeping only the last
    // silently threw away the analysis text.
    let pgn = "[Event \"T\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n\n\
               1. e4 { Best by test. } { [%eval 0.3] [%clk 0:05:00] } *\n"
    let path = System.IO.Path.GetTempFileName()
    try
        System.IO.File.WriteAllText(path, pgn)
        let game = FullPGNParser.parsePgnFile path |> Seq.head
        let move = game.Mainline |> Seq.head
        Assert.Equal("Best by test. [%eval 0.3] [%clk 0:05:00]", move.Comment)
        Assert.Equal("Best by test.", PGNComment.displayText move.Comment)
        Assert.Equal(Some "0.3", PGNComment.tryFind "eval" (PGNComment.parse move.Comment))
    finally
        System.IO.File.Delete path

[<Fact>]
let ``prose is separated from engine telemetry`` () =
    // Reader-facing text survives...
    Assert.Equal("(-2.59 → -1.20) Mistake. f6 was best.",
                 PGNComment.proseText "(-2.59 → -1.20) Mistake. f6 was best. [%eval -1.2] [%clk 0:21:18]")
    Assert.Equal("", PGNComment.proseText "{book, mb=+0+0+0+0+0,}")   // book marker is machine data too
    // ...while an EB tournament telemetry comment is not shown inline.
    Assert.Equal("", PGNComment.proseText "wv=1.06, mt=12875, s=9282899, n=119508052, d=35, sd=65, pv=10.Ng5 Qe7")
    // The tooltip keeps everything the reader might want to inspect.
    Assert.StartsWith("wv=1.06", PGNComment.displayText "wv=1.06, mt=12875, s=9282899, d=35")

[<Fact>]
let ``eval values format like the move list shows engine evals`` () =
    Assert.Equal(Some "+0.3", PGNComment.formatEval "0.28")
    Assert.Equal(Some "-1.5", PGNComment.formatEval "-1.5")
    Assert.Equal(Some "0.0", PGNComment.formatEval "0")      // no sign on a dead-equal score
    Assert.Equal(Some "#4", PGNComment.formatEval "#4")      // mate scores pass through
    Assert.Equal(Some "#-3", PGNComment.formatEval "#-3")
    Assert.Equal(None, PGNComment.formatEval "")
    Assert.Equal(None, PGNComment.formatEval "not a number")

[<Fact>]
let ``move list evals fall back to lichess eval commands`` () =
    // EB's own annotation wins where present; a lichess `[%eval]` fills the gap where it is
    // not. Both are White-relative, so black's move keeps its sign either way.
    let pgn =
        "[Event \"t\"]\n[White \"W\"]\n[Black \"B\"]\n[Result \"*\"]\n\n\
         1. e4 { [%eval 0.28] } 1... e5 { [%eval -1.5] } \
         2. Nf3 { wv=1.06, mt=12875, s=9282899, n=119508052, d=35 } 2... Nc6 { just prose } *\n"
    let path = System.IO.Path.GetTempFileName() + ".pgn"
    try
        System.IO.File.WriteAllText(path, pgn)
        let game = FullPGNParser.parsePgnFile path |> Seq.head
        let evals = PGNHelper.evalsByPly game
        Assert.Equal("+0.3", evals.[0])
        Assert.Equal("-1.5", evals.[1])
        Assert.Equal("+1.1", evals.[2])          // EB annotation, not the command path
        Assert.False(evals.ContainsKey 3)        // prose alone carries no evaluation
    finally
        System.IO.File.Delete path

[<Fact>]
let ``engine annotation dialects are telemetry, not prose`` () =
    // cutechess / CCC: eval/depth, move time and time left. Only one `=`, so the old
    // punctuation heuristic let it through and captioned every move of a CCC game.
    Assert.Equal("", PGNComment.proseText "+0.54/32 35.500s, tl=148.500s")
    Assert.Equal("", PGNComment.proseText "0.00/29 3.905s, tl=78.128s")
    Assert.Equal("", PGNComment.proseText "-0.58/32 7.279s, tl=176.721s")
    // Prose that merely contains numbers, evals or move numbers stays visible.
    Assert.Equal("White is winning after 25.Qd4", PGNComment.proseText "White is winning after 25.Qd4")
    Assert.Equal("Blunder. 0.00 was the eval before this.",
                 PGNComment.proseText "Blunder. 0.00 was the eval before this.")

[<Fact>]
let ``tooltip keeps the clocks the display text drops`` () =
    // The clock is the one command with nowhere else to go: `%eval` has the move list's
    // own column, and the arrows are drawn on the board.
    Assert.Equal("Best by test. — clock 0:05:00",
                 PGNComment.tooltipText "Best by test. [%eval 0.28] [%clk 0:05:00]")
    // A comment that was nothing but commands still says something useful.
    Assert.Equal("clock 0:21:18", PGNComment.tooltipText "[%eval -1.2] [%clk 0:21:18]")
    Assert.Equal("moved in 0:00:12, elapsed 0:04:31",
                 PGNComment.tooltipText "[%emt 0:00:12] [%egt 0:04:31]")
    // No clocks, no decoration — and an eval-only comment yields an empty tooltip.
    Assert.Equal("The main line.", PGNComment.tooltipText "The main line.")
    Assert.Equal("", PGNComment.tooltipText "[%eval 0.28]")
    Assert.Equal("", PGNComment.tooltipText "")

[<Fact>]
let ``near-zero negative evals do not produce a double sign`` () =
    // .NET applies the negative section of "+0.0;-0.0" to -0.04 and then adds the sign of
    // negative zero, yielding "-+0.0". Lichess exports are full of such evals.
    Assert.Equal(Some "0.0", PGNComment.formatEval "-0.04")
    Assert.Equal(Some "0.0", PGNComment.formatEval "-0.001")
    Assert.Equal(Some "0.0", PGNComment.formatEval "0.0")
    Assert.Equal(Some "-0.1", PGNComment.formatEval "-0.05")
    Assert.Equal("0.0", PGNComment.formatPawns -0.04)

[<Fact>]
let ``mate annotations reach the move list as mate, not as the sentinel`` () =
    // getEngineStatData collapses every mate to ±200.0 and drops the distance.
    Assert.Equal(Some 5, PGNComment.tryMateDistance "wv=M5, mt=100")
    Assert.Equal(Some 3, PGNComment.tryMateDistance "+M3/32 12.5s, tl=148.500s")
    Assert.Equal(None, PGNComment.tryMateDistance "Mate is unavoidable")
    let pgn =
        "[Event \"t\"]\n[White \"W\"]\n[Black \"B\"]\n[Result \"*\"]\n\n\
         1. e4 { wv=M5, mt=100, s=1000, n=1000, d=10 } \
         1... e5 { wv=-M5, mt=100, s=1000, n=1000, d=10 } *\n"
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".pgn")
    try
        System.IO.File.WriteAllText(path, pgn)
        let evals = FullPGNParser.parsePgnFile path |> Seq.head |> PGNHelper.evalsByPly
        Assert.Equal("#5", evals.[0])
        // EB writes `wv` White-relative already, so black's "-M5" means black is mating.
        Assert.Equal("#-5", evals.[1])
    finally
        System.IO.File.Delete path

[<Fact>]
let ``an unclosed command does not swallow the prose after it`` () =
    // The close-bracket search must stop at the next "[", or everything up to some unrelated
    // bracket later in the comment disappears.
    let parsed = PGNComment.parse "[%eval 0.3 forgot bracket [see diagram] here"
    Assert.Equal<(string * string) list>([], parsed.Commands)
    Assert.Equal("[%eval 0.3 forgot bracket [see diagram] here", parsed.Text)
    // A malformed opener must not hide a well-formed command later on.
    let mixed = PGNComment.parse "[% ] still here [%clk 0:03:00]"
    Assert.Equal(Some "0:03:00", PGNComment.tryFind "clk" mixed)
    Assert.Equal("[% ] still here", mixed.Text)

[<Fact>]
let ``prose is not mistaken for telemetry by its punctuation`` () =
    // Promotions are the trap for an "=" count, "Book" for a prefix match, and a written
    // result for an eval/depth pattern.
    Assert.Equal("The point: 34.e8=Q wins, since 34...f1=Q 35.Qxf1 g1=Q loses",
                 PGNComment.proseText "The point: 34.e8=Q wins, since 34...f1=Q 35.Qxf1 g1=Q loses")
    Assert.Equal("Book line ends here, now the real fight begins",
                 PGNComment.proseText "Book line ends here, now the real fight begins")
    Assert.Equal("1/2-1/2 was agreed", PGNComment.proseText "1/2-1/2 was agreed")
    // ...while the machine dialects those rules exist for are still caught.
    Assert.Equal("", PGNComment.proseText "book, mb=+0+0+0+0+0,")
    Assert.Equal("", PGNComment.proseText "+0.34/12")
    Assert.Equal("", PGNComment.proseText "wv=0.00, mt=0, s=0, n=0, d=0")

[<Fact>]
let ``EB's tournament header is telemetry, not an annotation on move one`` () =
    // Written into the first move's comment, with mixed-case keys that a lowercase-only
    // key pattern misses. Real excerpt, shortened.
    let header =
        "TournamentOptions: SF vs Ceres and Lc0; Rounds=300; Book=TCEC28_sufibook.pgn; \
         Tablebase adj=6-men; WhiteEngineOptions: Protocol=UCI; Threads=12; Hash=44096; \
         UCI_Chess960=false; book, mb=+0+0+0+0+0,"
    Assert.Equal("", PGNComment.proseText header)

[<Fact>]
let ``the evaluation chart reads lichess evals too`` () =
    // Without the fallback the chart plots a flat line at zero for any lichess-annotated
    // game, since `wv` only ever comes from an engine harness annotation.
    let pgn =
        "[Event \"t\"]\n[White \"W\"]\n[Black \"B\"]\n[Result \"1-0\"]\n\n\
         1. e4 { [%eval 0.28] } 1... e5 { [%eval -1.5] } \
         2. Nf3 { [%eval #3] } 2... Nc6 { [%eval #-2] } *\n"
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".pgn")
    try
        System.IO.File.WriteAllText(path, pgn)
        let game = FullPGNParser.parsePgnFile path |> Seq.head
        let white, black = PGNExtractor.extractWhiteAndBlackEngineStats game
        Assert.Equal(0.28, white.[0].wv, 3)
        Assert.Equal(-1.5, black.[0].wv, 3)   // White-relative already, so no flip for black
        Assert.Equal(200.0, white.[1].wv, 3)  // mate scores use the same sentinel as "M3"
        Assert.Equal(-200.0, black.[1].wv, 3)
    finally
        System.IO.File.Delete path
