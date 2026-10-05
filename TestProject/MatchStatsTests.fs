/// The reference's statistics and SPRT (ChessLibrary/Match) held to the reference's own test values:
/// app/tests/elo_test.cpp and app/tests/sprt_test.cpp at 60d7a7a, same inputs, same expected
/// numbers and tolerances. The formatting cases pin what fmt prints for ties and non-finite values.
module MatchStatsTests

open System
open Xunit
open ChessLibrary.Match
open ChessLibrary.Match.MatchStats

// The reference's Stats(ll, ld, wl, dd, wd, ww)
let private ofPenta (ll, ld, wl, dd, wd, ww) =
    { Stats.Empty with PentaLL = ll; PentaLD = ld; PentaWL = wl; PentaDD = dd; PentaWD = wd; PentaWW = ww }

let private close (expected: float) (actual: float) (eps: float) =
    Assert.True(abs (actual - expected) <= eps, sprintf "expected %g, got %g" expected actual)

// ---- elo_test.cpp ----

[<Theory>]
[<InlineData(76, 89, 123, -20.76, 40.13, "15.53 %", 0.477)>]
[<InlineData(136, 96, 111, 49.77, 36.77, "99.60 %", 0.558)>]
[<InlineData(34, 356, 0, -508.44, 34.48, null, 0.087)>]
let ``WDL nElo, LOS and score match the reference`` (w: int, l: int, d: int, nelo: float, nerr: float, los: string, score: float) =
    let e = eloWdl (Stats.OfWld(w, l, d))
    close nelo e.NEloDiff 0.01
    close nerr e.NEloError 0.01
    close score e.Score 0.001
    if not (isNull los) then Assert.Equal(los, e.Los)

[<Fact>]
let ``WDL Elo line matches the reference`` () =
    let e = eloWdl (Stats.OfWld(136, 96, 111))
    Assert.Equal("40.70 +/- 30.43", e.GetElo)
    Assert.Equal("49.77 +/- 36.77", e.NElo)

[<Theory>]
[<InlineData(34, 54, 31, 32, 64, 75, 57.94, 28.28, "55.58 +/- 27.65", "100.00 %", 0.579)>]
[<InlineData(332, 433, 457, 41, 333, 334, -9.17, 10.96, "-8.64 +/- 10.33", "5.05 %", 0.488)>]
[<InlineData(7895, 8757, 5485, 200, 568, 9999, -19.01, 2.65, "-21.04 +/- 2.95", "0.00 %", 0.470)>]
let ``pentanomial Elo, nElo, LOS and score match the reference``
        (ll: int, ld: int, wl: int, dd: int, wd: int, ww: int, nelo: float, nerr: float, eloLine: string, los: string, score: float) =
    let e = eloPenta (ofPenta(ll, ld, wl, dd, wd, ww))
    close nelo e.NEloDiff 0.01
    close nerr e.NEloError 0.01
    close score e.Score 0.001
    Assert.Equal(eloLine, e.GetElo)
    Assert.Equal(los, e.Los)

[<Fact>]
let ``Inverted swaps sides`` () =
    let s = { ofPenta(1, 2, 3, 4, 5, 6) with Wins = 7; Losses = 8; Draws = 9 }
    let i = s.Inverted
    Assert.Equal((8, 7, 9), (i.Wins, i.Losses, i.Draws))
    Assert.Equal((6, 5, 3, 4, 2, 1), (i.PentaLL, i.PentaLD, i.PentaWL, i.PentaDD, i.PentaWD, i.PentaWW))
    Assert.Equal(s, i.Inverted)

// ---- sprt_test.cpp (alpha = beta = 0.05, 1% relative tolerance as there) ----

let private sprt model elo0 elo1 = MatchSprt.create 0.05 0.05 elo0 elo1 model true

let private closeRel (expected: float) (actual: float) =
    Assert.True(abs (actual - expected) <= abs expected * 0.01, sprintf "expected %g, got %g" expected actual)

[<Theory>]
[<InlineData("normalized", 36433, 36027, 68692, 0.0, 2.0, 0.92)>]
[<InlineData("normalized", 10871, 10650, 20431, -1.75, 0.25, 2.30)>]
[<InlineData("normalized", 4250, 0, 0, 0.0, 10.0, 120.56)>]
[<InlineData("logistic", 21404, 21184, 40708, 0.5, 2.5, -1.57)>]
[<InlineData("logistic", 57433, 57030, 106593, 0.0, 2.0, -2.59)>]
[<InlineData("bayesian", 68965, 68526, 128429, 0.0, 2.0, -1.26)>]
[<InlineData("bayesian", 21629, 21484, 41111, 0.5, 2.5, -1.13)>]
let ``trinomial LLR matches the reference`` (model: string, w: int, l: int, d: int, elo0: float, elo1: float, expected: float) =
    closeRel expected (MatchSprt.llr (sprt model elo0 elo1) (Stats.OfWld(w, l, d)) false)

[<Theory>]
[<InlineData("normalized", 365, 16618, 36029, 200, 16974, 390, 0.0, 2.0, 2.25)>]
[<InlineData("normalized", 127, 4883, 10311, 401, 5150, 104, -1.75, 0.25, 3.01)>]
[<InlineData("normalized", 0, 0, 0, 0, 0, 5550, 0.0, 5.0, 111.82)>]
[<InlineData("logistic", 223, 9863, 20279, 1000, 10037, 246, 0.5, 2.5, -3.07)>]
[<InlineData("logistic", 871, 26175, 55003, 980, 26678, 821, 0.0, 2.0, -4.98)>]
let ``pentanomial LLR matches the reference``
        (model: string, ll: int, ld: int, wl: int, dd: int, wd: int, ww: int, elo0: float, elo1: float, expected: float) =
    closeRel expected (MatchSprt.llr (sprt model elo0 elo1) (ofPenta(ll, ld, wl, dd, wd, ww)) true)

[<Fact>]
let ``bounds, result and texts`` () =
    let s = sprt "normalized" 0.0 5.0
    Assert.Equal("(-2.94, 2.94)", MatchSprt.bounds s)
    Assert.Equal("[0.00, 5.00]", MatchSprt.eloRange s)
    Assert.Equal(MatchSprt.H1, MatchSprt.result s 2.95)
    Assert.Equal(MatchSprt.H0, MatchSprt.result s -2.95)
    Assert.Equal(MatchSprt.Continue, MatchSprt.result s 0.0)
    Assert.Equal(MatchSprt.Continue, MatchSprt.result (MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" false) 100.0)
    close 0.5 (MatchSprt.fraction s (s.Upper / 2.0)) 1e-12
    close -0.5 (MatchSprt.fraction s (s.Lower / 2.0)) 1e-12   // negative below zero: "LLR: -1.47 (-50.0%)"

[<Fact>]
let ``validation errors come in the reference's order and words`` () =
    let err = function Error e -> e | Ok _ -> "ok"
    Assert.Equal("Error; SPRT: elo0 must be less than elo1!", err (MatchSprt.validate 0.05 0.05 5.0 5.0 "normalized" true))
    Assert.Equal("Error; SPRT: alpha must be a decimal number between 0 and 1!", err (MatchSprt.validate 0.0 0.05 0.0 5.0 "normalized" true))
    Assert.Equal("Error; SPRT: beta must be a decimal number between 0 and 1!", err (MatchSprt.validate 0.05 1.0 0.0 5.0 "normalized" true))
    Assert.Equal("Error; SPRT: sum of alpha and beta must be less than 1!", err (MatchSprt.validate 0.5 0.5 0.0 5.0 "normalized" true))
    Assert.Equal("Error; SPRT: invalid SPRT model!", err (MatchSprt.validate 0.05 0.05 0.0 5.0 "gaussian" true))
    match MatchSprt.validate 0.05 0.05 0.0 5.0 "bayesian" true with
    | Ok (penta, Some _) -> Assert.False penta
    | other -> failwithf "expected the bayesian warning, got %A" other
    Assert.Equal(Ok (true, None), MatchSprt.validate 0.05 0.05 0.0 5.0 "normalized" true)

// ---- fmt formatting ----

[<Fact>]
let ``fixedPoint prints as fmt does`` () =
    Assert.Equal("12.12", MatchFormat.fixedPoint 2 12.125)   // exact tie: half to even
    Assert.Equal("0.38", MatchFormat.fixedPoint 2 0.375)
    Assert.Equal("-0.00", MatchFormat.fixedPoint 2 -0.0)
    Assert.Equal("-nan", MatchFormat.fixedPoint 2 (Double.NaN))  // x86 0/0 has the sign bit set
    // a positive NaN too (glibc's log10, ARM): the text must not depend on the machine
    Assert.Equal("-nan", MatchFormat.fixedPoint 2 (BitConverter.Int64BitsToDouble 0x7FF8000000000000L))
    Assert.Equal("-nan", MatchFormat.fixedPoint 2 (Math.Log10 -1.0))
    Assert.Equal("inf", MatchFormat.fixedPoint 2 Double.PositiveInfinity)
    Assert.Equal("-inf", MatchFormat.fixedPoint 2 Double.NegativeInfinity)

[<Fact>]
let ``empty stats print what the reference prints`` () =
    let e = eloWdl Stats.Empty
    Assert.Equal("-nan +/- -nan", e.GetElo)

// ---- exact agreement with the reference's own code ----
// Printed by a driver compiled against the reference 60d7a7a's sprt.cpp, elo_wdl.cpp and
// elo_pentanomial.cpp with the reference's release flags (g++ 13 -O3 -march=x86-64 -static, glibc,
// WSL; -O2 flips the sign of one NaN below): Elo diff, error, nElo, nElo error and LOS (std::erf) to
// 17 digits, the strings it prints, and the LLR of each model (trinomial: elo0 0, elo1 5;
// pentanomial: elo0 -1.75, elo1 0.25; alpha = beta = 0.05). Includes zero cells, one-sided and
// empty-ish results, where regularization and nan/inf printing matter.
let private referenceTable = """
WDL 76 89 123 | -15.693520705432237 30.45800071698045 -20.756449777395378 40.126025557578572 0.15532643492749831 | -15.69 +/- 30.46 | -20.76 +/- 40.13 | 15.53 %
LLRT normalized 76 89 123 | -0.27696474959817463
LLRT logistic 76 89 123 | -0.37841945460141102
LLRT bayesian 76 89 123 | -0.30129577585901224
WDL 136 96 111 | 40.702458186527075 30.42721794360099 49.768415381022194 36.76845059495362 0.99601023573349723 | 40.70 +/- 30.43 | 49.77 +/- 36.77 | 99.60 %
LLRT normalized 136 96 111 | 0.66448806033639296
LLRT logistic 136 96 111 | 0.79900550144721461
LLRT bayesian 136 96 111 | 0.71774635650975505
WDL 34 356 0 | -407.98843237224798 63.164514735819154 -508.43532503968595 34.481812485442134 0 | -407.99 +/- 63.16 | -508.44 +/- 34.48 | 0.00 %
LLRT normalized 34 356 0 | -4.6741799592836006
LLRT logistic 34 356 0 | -4.6743486680693946
LLRT bayesian 34 356 0 | -4.6743486677634563
WDL 36433 36027 68692 | 0.9993428129307208 1.298633596042416 1.3947974625341837 1.8125035725495355 0.93425784760950992 | 1.00 +/- 1.30 | 1.39 +/- 1.81 | 93.43 %
LLRT normalized 36433 36027 68692 | -6.4611145081571868
LLRT logistic 36433 36027 68692 | -17.089345640261467
LLRT bayesian 36433 36027 68692 | -7.897290084034343
WDL 4250 0 0 | inf -nan inf -nan 1 | inf +/- -nan | inf +/- -nan | 100.00 %
LLRT normalized 4250 0 0 | 60.720244778381264
LLRT logistic 4250 0 0 | 60.722332544483386
LLRT bayesian 4250 0 0 | 0
WDL 0 7 3 | -301.33106666344463 341.90077940895964 -530.71662325954151 215.33884995273513 6.8109360495949289e-07 | -301.33 +/- 341.90 | -530.72 +/- 215.34 | 0.00 %
LLRT normalized 0 7 3 | -0.12134040880196947
LLRT logistic 0 7 3 | -0.14491199155635154
LLRT bayesian 0 7 3 | 0
WDL 10 10 10 | -0 104.55786329400974 0 124.32594298719597 0.5 | -0.00 +/- 104.56 | 0.00 +/- 124.33 | 50.00 %
LLRT normalized 10 10 10 | -0.0031063396807406735
LLRT logistic 10 10 10 | -0.0046587481655024909
LLRT bayesian 10 10 10 | -0.0036809423800287806
WDL 5 0 12 | 105.29657390983257 84.429825940392277 224.2687061014764 165.15735865240509 0.99610979262528088 | 105.30 +/- 84.43 | 224.27 +/- 165.16 | 99.61 %
LLRT normalized 5 0 12 | 0.13119815171250937
LLRT logistic 5 0 12 | 0.2427892882143122
LLRT bayesian 5 0 12 | 0
WDL 1 1 0 | -0 -nan 0 481.51230669094298 0.5 | -0.00 +/- -nan | 0.00 +/- 481.51 | 50.00 %
LLRT normalized 1 1 0 | -0.00020718749891722877
LLRT logistic 1 1 0 | -0.00020724458361771105
LLRT bayesian 1 1 0 | -0.00020724448007897775
PENTA 34 54 31 32 64 75 | 55.579780420445879 27.653429506393451 57.935599938124312 28.275376243408669 0.99997039300843127 | 55.58 +/- 27.65 | 57.94 +/- 28.28 | 100.00 %
LLRP normalized 34 54 31 32 64 75 | 0.55149733115457911
LLRP logistic 34 54 31 32 64 75 | 0.56691144853737108
PENTA 332 433 457 41 333 334 | -8.64266726796901 10.33419442439302 -9.1729142700471353 10.960458877511137 0.050470066982530093 | -8.64 +/- 10.33 | -9.17 +/- 10.96 | 5.05 %
LLRP normalized 332 433 457 41 333 334 | -0.53772795367490167
LLRP logistic 332 433 457 41 333 334 | -0.56697525994598141
PENTA 0 0 0 0 0 5550 | inf -nan inf -nan 1 | inf +/- -nan | inf +/- -nan | 100.00 %
LLRP normalized 0 0 0 0 0 5550 | 45.319413037184034
LLRP logistic 0 0 0 0 0 5550 | 32.017345514748783
PENTA 3 0 10 2 0 1 | -43.657787770027198 85.490787850110777 -63.432769156914709 120.37807667273569 0.15084979123917397 | -43.66 +/- 85.49 | -63.43 +/- 120.38 | 15.08 %
LLRP normalized 3 0 10 2 0 1 | -0.032184993232021987
LLRP logistic 3 0 10 2 0 1 | -0.045401128723441507
PENTA 0 4 0 9 5 0 | 9.6534718866878055 57.12550208140815 19.36182820851073 113.49353909531413 0.63094865368217035 | 9.65 +/- 57.13 | 19.36 +/- 113.49 | 63.09 %
LLRP normalized 0 4 0 9 5 0 | 0.011954909709669635
LLRP logistic 0 4 0 9 5 0 | 0.024782640029486956
PENTA 1 1 1 1 1 1 | -7.7146197324262942e-14 198.57641290279929 -4.2254712500243522e-14 196.57657604388874 0.49999999999999983 | -0.00 +/- 198.58 | -0.00 +/- 196.58 | 50.00 %
LLRP normalized 1 1 1 1 1 1 | 0.00014911403155859655
LLRP logistic 1 1 1 1 1 1 | 0.00017891027443539802
PENTA 0 0 1 0 0 0 | -0 0 -nan -nan -nan | -0.00 +/- 0.00 | -nan +/- -nan | -nan %
LLRP normalized 0 0 1 0 0 0 | 2.5061372114068304e-05
LLRP logistic 0 0 1 0 0 0 | 0.0027572446482504536
"""

let private referenceLines prefix =
    referenceTable.Split('\n')
    |> Array.map (fun l -> l.Trim())
    |> Array.filter (fun l -> l.StartsWith(prefix + " "))
    |> Array.map (fun l -> l.Split('|') |> Array.map (fun p -> p.Trim()))

let private parseC (s: string) =
    match s with
    | "inf" -> Double.PositiveInfinity
    | "-inf" -> Double.NegativeInfinity
    | "nan" | "-nan" -> Double.NaN
    | s -> Double.Parse(s, Globalization.CultureInfo.InvariantCulture)

/// Same value to 1e-12 relative (1e-15 absolute near zero); both NaN counts as the same.
let private same (what: string) (expected: float) (actual: float) =
    let ok =
        (Double.IsNaN expected && Double.IsNaN actual)
        || expected = actual
        || abs (actual - expected) <= max (abs expected * 1e-12) 1e-15
    Assert.True(ok, sprintf "%s: the reference %.17g, ours %.17g" what expected actual)

let private ints (s: string) = s.Split(' ') |> Array.skip 1 |> Array.map int

let private checkElo (label: string) (row: string[]) (e: Elo) =
    let v = row.[1].Split(' ') |> Array.map parseC
    same (label + " diff") v.[0] e.Diff
    same (label + " error") v.[1] e.Error
    same (label + " nElo") v.[2] e.NEloDiff
    same (label + " nElo error") v.[3] e.NEloError
    same (label + " LOS") v.[4] e.LosValue
    Assert.Equal(row.[2], e.GetElo)
    Assert.Equal(row.[3], e.NElo)
    Assert.Equal(row.[4], e.Los)

[<Fact>]
let ``WDL Elo equals the reference's to the last digit`` () =
    let rows = referenceLines "WDL"
    Assert.Equal(9, rows.Length)
    for row in rows do
        let c = ints row.[0]
        checkElo row.[0] row (eloWdl (Stats.OfWld(c.[0], c.[1], c.[2])))

[<Fact>]
let ``pentanomial Elo equals the reference's to the last digit`` () =
    let rows = referenceLines "PENTA"
    Assert.Equal(7, rows.Length)
    for row in rows do
        let c = ints row.[0]
        checkElo row.[0] row (eloPenta (ofPenta(c.[0], c.[1], c.[2], c.[3], c.[4], c.[5])))

[<Fact>]
let ``LLR equals the reference's in every model`` () =
    let tri = referenceLines "LLRT"
    let pen = referenceLines "LLRP"
    Assert.Equal(27, tri.Length)
    Assert.Equal(14, pen.Length)
    for row in tri do
        let p = row.[0].Split(' ')
        let c = p |> Array.skip 2 |> Array.map int
        same row.[0] (parseC row.[1]) (MatchSprt.llr (sprt p.[1] 0.0 5.0) (Stats.OfWld(c.[0], c.[1], c.[2])) false)
    for row in pen do
        let p = row.[0].Split(' ')
        let c = p |> Array.skip 2 |> Array.map int
        let stats = ofPenta(c.[0], c.[1], c.[2], c.[3], c.[4], c.[5])
        same row.[0] (parseC row.[1]) (MatchSprt.llr (sprt p.[1] -1.75 0.25) stats true)
