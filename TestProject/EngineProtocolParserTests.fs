/// The fast info-line parser (EngineProtocol.Regex.getEssentialData / getEssentialDataWithEPS)
/// must answer exactly as the regex version it replaced - which is kept as legacyGetEssential* for
/// that reason. Both run over TestData/InfoLinesCorpus.txt (1,200 real lines from Lc0, Stockfish,
/// Ceres and our own nets: every line shape in 1.19M logged lines - its first, shortest and
/// longest line - plus a fixed random spread) and over hand-made edge
/// cases that exercise the regexes' quirks. Where the old version threw, the new one must throw
/// the same exception type.
module EngineProtocolParserTests

open System
open System.IO
open Xunit
open ChessLibrary.EngineProtocol

let private outcome (f: unit -> 'a) =
    try Choice1Of2 (f ()) with ex -> Choice2Of2 (ex.GetType().FullName)

let private assertSame (line: string) =
    for isWhite in [ true; false ] do
        let legacyEps = outcome (fun () -> Regex.legacyGetEssentialDataWithEPS line isWhite)
        let fastEps = outcome (fun () -> Regex.getEssentialDataWithEPS line isWhite)
        if legacyEps <> fastEps then
            Assert.Fail(sprintf "WithEPS differs (isWhite=%b) for line:\n%s\nlegacy: %A\nfast:   %A" isWhite line legacyEps fastEps)
        let legacy = outcome (fun () -> Regex.legacyGetEssentialData line isWhite)
        let fast = outcome (fun () -> Regex.getEssentialData line isWhite)
        if legacy <> fast then
            Assert.Fail(sprintf "differs (isWhite=%b) for line:\n%s\nlegacy: %A\nfast:   %A" isWhite line legacy fast)

[<Fact>]
let ``Fast info parser matches the regex parser on real engine output`` () =
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "InfoLinesCorpus.txt")
    let lines = File.ReadAllLines path |> Array.filter (fun l -> l <> "")
    Assert.True(lines.Length >= 1200)
    for line in lines do assertSame line

[<Theory>]
// ordinary lines
[<InlineData("info depth 12 seldepth 30 multipv 1 score cp 23 wdl 400 450 150 nodes 567890 nps 123456 tbhits 0 time 1234 pv e2e4 e7e5 g1f3")>]
[<InlineData("info depth 5 score mate -3 nodes 10 pv e2e4")>]
[<InlineData("info depth 5 score mate 0 nodes 10 pv e2e4")>]
[<InlineData("info depth 5 score cp -0 nodes 10 pv e2e4")>]
[<InlineData("info depth 5 score cp 0 nodes 10 pv e2e4")>]
[<InlineData("info depth 7 seldepth 9 score cp 99 upperbound nodes 1 nps 1 tbhits 0 time 1 pv e2e4")>]
[<InlineData("info depth 7 seldepth 9 score cp 99 lowerbound nodes 1 nps 1 eps 12 tbhits 0 time 1 pv e2e4")>]
// the regex quirks the fast parser must keep
[<InlineData("info seldepth 30 depth 12 score cp 5 pv e2e4")>]           // depth lands in seldepth
[<InlineData("info depth 3 multipv 2 score cp 5 nodes 7")>]               // multipv before score and no pv: empty pv
[<InlineData("info depth 3 score cp 5 multipv 2 nodes 7")>]               // no pv: the regex reads "2 nodes 7"
[<InlineData("info depth 3 score cp 5 nodes 7 pv")>]                       // "pv" at the end, no space after
[<InlineData("info depth 3 score cp 5 nodes 7 pv   ")>]                    // pv followed by spaces only
[<InlineData("info depth 3 score cp 5 pv e2e4 pv d2d4")>]                  // last pv wins
[<InlineData("info depth 3 score cp 5 xmultipv 4 pv e2e4")>]               // no word boundary before multipv
[<InlineData("info depth 3 score cp 5 multipv 4x pv e2e4")>]               // no word boundary after
[<InlineData("info depth 3 score cp 5 multipv 4_ pv e2e4")>]
[<InlineData("info depth 3 score cp 5 multipv 4, pv e2e4")>]
[<InlineData("info depth\t3 score\tcp\t-17 nodes\t\t9 pv\te2e4")>]         // tabs are whitespace
[<InlineData("info depth  3 score cp 5 pv e2e4")>]                          // depth needs exactly one space
[<InlineData("info depth x score cp 5 depth 4 pv e2e4")>]                 // first matching occurrence
[<InlineData("info score cpx 5 score cp 6 pv e2e4")>]
[<InlineData("info score cp - 5 score cp -6 pv e2e4")>]
[<InlineData("info score cp --5 score cp 7 pv e2e4")>]
[<InlineData("info score mate")>]
[<InlineData("info score cp")>]
[<InlineData("info score")>]
[<InlineData("info depth 3 nodes 5")>]                                      // no score: None
[<InlineData("info string N: 5 score cp 3")>]
[<InlineData("info wdl 1 2 wdl 3 4 5 score cp 1 pv a2a3")>]                // first complete wdl
[<InlineData("info wdl 1 2 score cp 1 pv a2a3")>]                          // incomplete wdl
[<InlineData("info nodes x nodes 12 nps y nps 34 tbhits z tbhits 5 score cp 1")>]
[<InlineData("info steps 4 eps 9 score cp 1")>]
[<InlineData("infodepth 3 score cp 1 pv e2e4")>]                            // "info" prefix without space
[<InlineData("INFO depth 3 score cp 1 pv e2e4")>]                           // not an info line
[<InlineData("bestmove e2e4")>]
[<InlineData("")>]
// numbers too long for the fast path: it declines and the regex version answers or throws
[<InlineData("info depth 99999999999 score cp 1 pv e2e4")>]
[<InlineData("info depth 2147483647 score cp 1 pv e2e4")>]
[<InlineData("info depth 1 nodes 99999999999999999999 score cp 1 pv e2e4")>]
[<InlineData("info depth 1 score cp 1234567890123456789 pv e2e4")>]
[<InlineData("info depth 1 score mate 99999999999 pv e2e4")>]
[<InlineData("info depth 1 score cp 1 wdl 1234567890123456789 1 1 pv e2e4")>]
// non-ASCII and control characters go to the regex version
[<InlineData("info depth 1 score cp 1 pv e2e4 é")>]
[<InlineData("info depth ١ score cp 1 pv e2e4")>]
[<InlineData("info depth 1 score cp 1\r pv e2e4")>]
let ``Fast info parser matches the regex parser on edge cases`` (line: string) =
    assertSame line

// ── Verbose move stats ("info string <move> ... N: ... P: ... Q: ...") ─────────────────────────

let private assertSameStat (line: string) =
    let legacy = outcome (fun () -> Regex.legacyGetInfoStringData "p" line)
    let fast = outcome (fun () -> Regex.getInfoStringData "p" line)
    if legacy <> fast then
        Assert.Fail(sprintf "move stats differ for line:\n%s\nlegacy: %A\nfast:   %A" line legacy fast)

[<Fact>]
let ``Fast move-stats parser matches the regex parser on real engine output`` () =
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "InfoLinesCorpus.txt")
    let lines = File.ReadAllLines path |> Array.filter (fun l -> l.StartsWith("info string", StringComparison.Ordinal))
    Assert.True(lines.Length > 500)
    for line in lines do assertSameStat line

[<Theory>]
[<InlineData("info string e1d1  (100 ) N:       0 (+ 0) (P:  0.56%) (WL:  0.00000) (D: 0.000) (M:  0.0) (Q:  0.00000) (U: 0.27748) (S:  0.39140) (V:  -.----) ")>]
[<InlineData("info string f8c5  (139 ) N:      19 (+ 0) (P:  0.75%) (WGT:      19.000) (WL: -0.99998) (D: 0.000) (M: 63.2) (STD: 0.00000) (STDF: 1.00000) (VS: 0.99996) (E: 0.00388) (Q: -0.99998) (U: 0.57593) (S: -0.42405) (V:  -.----)")>]
[<InlineData("info string node  (  20) N:    1000 (+ 0) (P: 100.0%) (Q:  0.09000) (V:  0.0800)")>]
[<InlineData("info string e2e4 N: 5 (P:0.5%) (P: 0.6%) (Q: 1,25) (V: -0,5) (E: -0.1) (E: 0.2)")>]   // no space; comma; E has no minus
[<InlineData("info string e2e4 N: x N: 7 (P: -.5) (P: 1.) (P: 2.5) (Q: --1.0) (Q: -1.0)")>]
[<InlineData("info string e2e4")>]
[<InlineData("info string  (100) N: 3")>]                                                        // no move word
[<InlineData("info string\te2e4 N:\t9 (P:\t1.5%)")>]
[<InlineData("info stringe2e4 N: 1")>]
[<InlineData("info string e2e4 N: 99999999999999999999 (P: 1.0%)")>]                               // overflow: declines
[<InlineData("info string e2e4 N: 1 (P: 1.0%) \u00e9")>]                                          // non-ASCII: declines
[<InlineData("")>]
let ``Fast move-stats parser matches the regex parser on edge cases`` (line: string) =
    assertSameStat line
