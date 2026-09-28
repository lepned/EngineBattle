/// The fast move-comment parser (EngineTypes.Annotation.getEngineStatData) must answer exactly as
/// the regex version it replaced - kept as legacyGetEngineStatData for that reason. Both run over
/// TestData/CommentCorpus.txt (real comments: every shape in 711k from EB, Ceres, Banksia and
/// lichess PGNs and in 2.8M from TCEC and CCC, plus a fixed random spread) and over hand-made
/// edge cases for the regexes' quirks. Where the old version threw, the new one must throw the
/// same exception type. On all 3.5M comments: 0 differences; EB comments 1.51 -> 0.28 us and
/// 5.9 KB -> 0.4 KB, TCEC/CCC 1.09 -> 0.29 us.
module AnnotationParserTests

open System
open System.IO
open Xunit
open ChessLibrary.EngineTypes

let private outcome (f: unit -> 'a) =
    try Choice1Of2 (f ()) with ex -> Choice2Of2 (ex.GetType().FullName)

let private assertSame (line: string) =
    for isBlack in [ false; true ] do
        let legacy = outcome (fun () -> Annotation.legacyGetEngineStatData "p" isBlack line)
        let fast = outcome (fun () -> Annotation.getEngineStatData "p" isBlack line)
        if legacy <> fast then
            Assert.Fail(sprintf "differs (isBlack=%b) for comment:\n%s\nlegacy: %A\nfast:   %A" isBlack line legacy fast)

[<Fact>]
let ``Fast comment parser matches the regex parser on real PGN comments`` () =
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "CommentCorpus.txt")
    let lines = File.ReadAllLines path |> Array.filter (fun l -> l <> "")
    Assert.True(lines.Length >= 260)
    for line in lines do assertSame line

[<Theory>]
// EB's own format
[<InlineData("d=29, sd=51, pd=Rc6, mt=3773, tl=8580, s=6335926, n=23905452, tb=14132, wv=1.61, n1=0, n2=0, q1=0, q2=0, p1=0, pt=0")>]
[<InlineData("d=6, sd=12, pd=Rc4, mt=20, tl=79699, s=54884, n=1105, tb=0, wv=9.72, n1=347, n2=295, q1=0.94587, q2=0.97187, p1=17.51, pt=18.08")>]
[<InlineData("wv=0.25, mt=00:00:05, s=1200, eps=300, n=6000, d=12, sd=20, pd=e5, tl=00:01:30, tb=0, pv=e4 e5, pcs=32")>]
// the regex quirks the fast parser must keep
[<InlineData("eps=500, wv=1.0, s=700")>]            // s= matches inside eps=
[<InlineData("pcs=31, wv=1.0, s=700")>]             // ... and inside pcs=
[<InlineData("pd=5, d=10, wv=0.1")>]                // d= inside pd= (only sd= is excluded)
[<InlineData("sd=12, wv=1")>]                       // no d= but the one in sd=: 0
[<InlineData("pd=Rc6, d=7, wv=1")>]                 // d= inside pd= not followed by a digit: the next one
[<InlineData("wv=1, mt=12:34")>]                     // not a full clock: the digits
[<InlineData("wv=1, mt=123:45:67")>]                 // three digits first: the digits
[<InlineData("wv=1, mt=12:34:567")>]                 // a clock, then more digits
[<InlineData("wv=1, tl=01:00:00")>]                  // tl= has no clock form: the digits
[<InlineData("wv=-M5, d=3")>]
[<InlineData("wv=M, d=3")>]
[<InlineData("wv=M12")>]
[<InlineData("wv=-5")>]
[<InlineData("wv=1.")>]
[<InlineData("wv=.5, wv=2.5")>]                      // first fitting occurrence
[<InlineData("wv=abc wv=2")>]
[<InlineData("wv=-")>]
[<InlineData("wv=")>]
[<InlineData("wv=- M5")>]
[<InlineData("wv=1, q1=-0.5, q2=1.")>]               // q2 needs digits after the point: 0
[<InlineData("wv=1, q1=--1.0 q1=2.5")>]
[<InlineData("wv=1, p1=17.51, pt=18")>]              // p1/pt read the digits before the point
[<InlineData("wv=1, n1=5, n=7")>]                    // n= is not inside n1=
[<InlineData("wv=1, tn=9, n=7")>]                    // ... but is inside tn=
// s= with a space or unit: the regex version reads it (and throws on "N/s", as it always did)
[<InlineData("wv=1, s=123 kN/s, n=5")>]
[<InlineData("wv=1, s=123kN/s")>]
[<InlineData("wv=1, s=123 N/s")>]
[<InlineData("wv=1, s=123 , n=5")>]
[<InlineData("+1.17/27 3.0s, ev=1.17, d=27, pd=Bxf3, mt=00:00:02, tl=00:00:58, s=125680 kN/s, n=371384518, pv=h3 Bxf3, tb=0, R50=49, wv=1.17")>]  // TCEC/CCC
[<InlineData("-M2/2 0.014s, ev=-M2, d=2, pd=Qa8#, mt=00:00:00, tl=00:00:13, s=27 kN/s, n=388, pv=Kg8 Qa8#, tb=0, R50=46, wv=M2")>]
[<InlineData("wv=1, s=5 \t kN/s")>]
[<InlineData("wv=1, s=5 kN/sec")>]
[<InlineData("wv=1, s=99999999999999999 kN/s")>]   // the regex version's * 1000 wraps; so must this
[<InlineData("book")>]
[<InlineData("Book exit")>]
[<InlineData("wv=M")>]                                // no digit, but EB's form
// numbers too long for the fast path
[<InlineData("wv=1, d=99999999999")>]
[<InlineData("wv=1, d=2147483647")>]
[<InlineData("wv=1, n=99999999999999999999")>]
[<InlineData("wv=1, pcs=9999999999")>]
[<InlineData("wv=123456789012345678901234567890123")>]
// non-ASCII and line breaks go to the regex version
[<InlineData("wv=1.0, d=5 é")>]
[<InlineData("wv=1.0, d=١")>]
[<InlineData("wv=1.0,\nd=5")>]
// the other formats (regex only)
[<InlineData("+0.28/12 1.2s")>]
[<InlineData("{+0.30/15 0.5s, tl=12.3s}")>]
[<InlineData("-1.25/18 2.35s")>]
[<InlineData("0.25/12 3000 45000")>]
[<InlineData("M3/20 1.0s")>]
[<InlineData("book, mb=+0.0+0.0+0.0+0.0+0.0,")>]
[<InlineData("[%eval 0.31] [%clk 0:01:00]")>]
[<InlineData("")>]
let ``Fast comment parser matches the regex parser on edge cases`` (line: string) =
    assertSame line
