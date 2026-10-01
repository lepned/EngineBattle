/// The reference's console output (ChessLibrary/Match/MatchOutput.fs) held to the reference:
/// two runs of the reference 60d7a7a (release build, WSL, Stockfish at fixed nodes) whose own
/// `Finished game` lines are replayed into the Reporter, which must print every line the reference
/// printed (engine warnings aside). Five more such runs - three engines, pentanomial off, SPRT
/// stops in both formats - were compared the same way when this was written. Plus the reason
/// texts, the end lines and fmt's number formats, checked against fmt itself.
module MatchOutputTests

open System
open System.Text.RegularExpressions
open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.Match

// ---- replayed reference runs ----

let private env =
    { MatchArgs.defaultEnv () with HardwareThreads = 24; IsWindows = false; PathExists = (fun _ -> true); IsFile = (fun _ -> true) }

let private each = "-each option.Threads=1 option.Hash=16 -concurrency 1 -openings file=/tmp/fcbuild/book.epd format=epd order=sequential"

/// run A: default format, pentanomial, rating interval 2, an SPRT that does not decide
let private runA =
    "-engine cmd=sf name=SF-a nodes=4000 -engine cmd=sf name=SF-b nodes=3000 " + each
    + " -rounds 6 -games 2 -ratinginterval 2 -sprt elo0=0 elo1=5 alpha=0.05 beta=0.05"

let private outA = """
Started game 1 of 12 (SF-a vs SF-b)
Finished game 1 (SF-a vs SF-b): 1-0 {Black makes an illegal move}
Started game 2 of 12 (SF-b vs SF-a)
Finished game 2 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
Started game 3 of 12 (SF-a vs SF-b)
Finished game 3 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Started game 4 of 12 (SF-b vs SF-a)
Finished game 4 (SF-b vs SF-a): 1/2-1/2 {Draw by insufficient mating material}
--------------------------------------------------
Results of SF-a vs SF-b (4000 nodes - 3000 nodes, 1t, 16MB, book.epd):
Elo: -88.74 +/- 136.27, nElo: -245.67 +/- 340.48
LOS: 7.86 %, DrawRatio: 50.00 %, PairsRatio: 0.00
Games: 4, Wins: 1, Losses: 2, Draws: 1, Points: 1.5 (37.50 %)
Ptnml(0-2): [0, 1, 1, 0, 0], WL/DD Ratio: inf
LLR: -0.02 (-0.7%) (-2.94, 2.94) [0.00, 5.00]
--------------------------------------------------
Started game 5 of 12 (SF-a vs SF-b)
Finished game 5 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Started game 6 of 12 (SF-b vs SF-a)
Finished game 6 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
Started game 7 of 12 (SF-a vs SF-b)
Finished game 7 (SF-a vs SF-b): 1-0 {Black makes an illegal move}
Started game 8 of 12 (SF-b vs SF-a)
Finished game 8 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
--------------------------------------------------
Results of SF-a vs SF-b (4000 nodes - 3000 nodes, 1t, 16MB, book.epd):
Elo: -136.97 +/- 187.60, nElo: -222.22 +/- 240.76
LOS: 3.52 %, DrawRatio: 50.00 %, PairsRatio: 0.00
Games: 8, Wins: 2, Losses: 5, Draws: 1, Points: 2.5 (31.25 %)
Ptnml(0-2): [1, 1, 2, 0, 0], WL/DD Ratio: inf
LLR: -0.05 (-1.7%) (-2.94, 2.94) [0.00, 5.00]
--------------------------------------------------
Started game 9 of 12 (SF-a vs SF-b)
Finished game 9 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Started game 10 of 12 (SF-b vs SF-a)
Finished game 10 (SF-b vs SF-a): 1/2-1/2 {Draw by insufficient mating material}
Started game 11 of 12 (SF-a vs SF-b)
Finished game 11 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Started game 12 of 12 (SF-b vs SF-a)
Finished game 12 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
--------------------------------------------------
Results of SF-a vs SF-b (4000 nodes - 3000 nodes, 1t, 16MB, book.epd):
Elo: -190.85 +/- 174.13, nElo: -300.89 +/- 196.58
LOS: 0.13 %, DrawRatio: 33.33 %, PairsRatio: 0.00
Games: 12, Wins: 2, Losses: 8, Draws: 2, Points: 3.0 (25.00 %)
Ptnml(0-2): [2, 2, 2, 0, 0], WL/DD Ratio: inf
LLR: -0.09 (-3.0%) (-2.94, 2.94) [0.00, 5.00]
--------------------------------------------------
Finished match
"""

/// run B: cutechess format, a score line per game, rating interval 3, SPRT
let private runB =
    "-engine cmd=sf name=SF-a nodes=4000 -engine cmd=sf name=SF-b nodes=3000 " + each
    + " -rounds 4 -games 2 -ratinginterval 3 -scoreinterval 1 -output format=cutechess -sprt elo0=0 elo1=5 alpha=0.05 beta=0.05"

let private outB = """
Started game 1 of 8 (SF-a vs SF-b)
Finished game 1 (SF-a vs SF-b): 1-0 {Black makes an illegal move}
Score of SF-a vs SF-b: 1 - 0 - 0  [1.000] 1
Started game 2 of 8 (SF-b vs SF-a)
Finished game 2 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
Score of SF-a vs SF-b: 1 - 1 - 0  [0.500] 2
Started game 3 of 8 (SF-a vs SF-b)
Finished game 3 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Score of SF-a vs SF-b: 1 - 2 - 0  [0.333] 3
Elo difference: -120.41 +/- -nan, LOS: 27.01 %, DrawRatio: 0.00 %
SPRT: llr -0.01 (0.5%), lbound -2.94, ubound 2.94
Started game 4 of 8 (SF-b vs SF-a)
Finished game 4 (SF-b vs SF-a): 1/2-1/2 {Draw by insufficient mating material}
Score of SF-a vs SF-b: 1 - 2 - 1  [0.375] 4
Started game 5 of 8 (SF-a vs SF-b)
Finished game 5 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Score of SF-a vs SF-b: 1 - 3 - 1  [0.300] 5
Started game 6 of 8 (SF-b vs SF-a)
Finished game 6 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
Score of SF-a vs SF-b: 1 - 4 - 1  [0.250] 6
Elo difference: -190.85 +/- -nan, LOS: 5.44 %, DrawRatio: 16.67 %
SPRT: llr -0.05 (1.6%), lbound -2.94, ubound 2.94
Started game 7 of 8 (SF-a vs SF-b)
Finished game 7 (SF-a vs SF-b): 1-0 {Black makes an illegal move}
Score of SF-a vs SF-b: 2 - 4 - 1  [0.357] 7
Started game 8 of 8 (SF-b vs SF-a)
Finished game 8 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
Score of SF-a vs SF-b: 2 - 5 - 1  [0.312] 8
Elo difference: -136.97 +/- 398.73, LOS: 10.79 %, DrawRatio: 12.50 %
SPRT: llr -0.05 (1.6%), lbound -2.94, ubound 2.94
Finished match
"""

/// run C: three engines, the ranking table with Ptnml, rating interval 1
let private runC =
    "-engine cmd=sf name=SF-a nodes=4000 -engine cmd=sf name=SF-b nodes=3000 -engine cmd=sf name=SF-c nodes=2000 " + each
    + " -rounds 2 -games 2 -ratinginterval 1"

let private outC = """
Started game 1 of 12 (SF-a vs SF-b)
Finished game 1 (SF-a vs SF-b): 1-0 {Black makes an illegal move}
Started game 2 of 12 (SF-b vs SF-a)
Finished game 2 (SF-b vs SF-a): 1-0 {Black makes an illegal move}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-a                           -0.00       0.00       -nan       -nan          2      50.0%     100.0%      [0, 0, 1, 0, 0]
   2 SF-b                           -0.00       0.00       -nan       -nan          2      50.0%     100.0%      [0, 0, 1, 0, 0]
   3 SF-c                            -nan       -nan       -nan       -nan          0      -nan%      -nan%      [0, 0, 0, 0, 0]
--------------------------------------------------
Started game 3 of 12 (SF-a vs SF-c)
Finished game 3 (SF-a vs SF-c): 1-0 {Black makes an illegal move}
Started game 4 of 12 (SF-c vs SF-a)
Finished game 4 (SF-c vs SF-a): 0-1 {White makes an illegal move}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-a                          190.85       -nan     245.67     340.48          4      75.0%      50.0%      [0, 0, 1, 0, 1]
   2 SF-b                           -0.00       0.00       -nan       -nan          2      50.0%     100.0%      [0, 0, 1, 0, 0]
   3 SF-c                            -inf       -nan       -inf       -nan          2       0.0%       0.0%      [1, 0, 0, 0, 0]
--------------------------------------------------
Started game 5 of 12 (SF-b vs SF-c)
Finished game 5 (SF-b vs SF-c): 1-0 {Black makes an illegal move}
Started game 6 of 12 (SF-c vs SF-b)
Finished game 6 (SF-c vs SF-b): 0-1 {White makes an illegal move}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-a                          190.85       -nan     245.67     340.48          4      75.0%      50.0%      [0, 0, 1, 0, 1]
   2 SF-b                          190.85       -nan     245.67     340.48          4      75.0%      50.0%      [0, 0, 1, 0, 1]
   3 SF-c                            -inf       -nan       -inf       -nan          4       0.0%       0.0%      [2, 0, 0, 0, 0]
--------------------------------------------------
Started game 7 of 12 (SF-a vs SF-b)
Finished game 7 (SF-a vs SF-b): 0-1 {White makes an illegal move}
Started game 8 of 12 (SF-b vs SF-a)
Finished game 8 (SF-b vs SF-a): 1/2-1/2 {Draw by insufficient mating material}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-b                          190.85     335.90     300.89     278.00          6      75.0%      33.3%      [0, 0, 1, 1, 1]
   2 SF-a                           58.45     337.97      65.66     278.00          6      58.3%      33.3%      [0, 1, 1, 0, 1]
   3 SF-c                            -inf       -nan       -inf       -nan          4       0.0%       0.0%      [2, 0, 0, 0, 0]
--------------------------------------------------
Started game 9 of 12 (SF-a vs SF-c)
Finished game 9 (SF-a vs SF-c): 0-1 {White makes an illegal move}
Started game 10 of 12 (SF-c vs SF-a)
Finished game 10 (SF-c vs SF-a): 1-0 {Black makes an illegal move}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-b                          190.85     335.90     300.89     278.00          6      75.0%      33.3%      [0, 0, 1, 1, 1]
   2 SF-a                          -43.66     338.36     -41.53     240.76          8      43.8%      25.0%      [1, 1, 1, 0, 1]
   3 SF-c                         -120.41       -nan     -86.86     278.00          6      33.3%       0.0%      [2, 0, 0, 0, 1]
--------------------------------------------------
Started game 11 of 12 (SF-b vs SF-c)
Finished game 11 (SF-b vs SF-c): 0-1 {White makes an illegal move}
Started game 12 of 12 (SF-c vs SF-b)
Finished game 12 (SF-c vs SF-b): 1-0 {Black makes an illegal move}
--------------------------------------------------
Rank Name                             Elo        +/-       nElo        +/-      Games      Score       Draw           Ptnml(0-2)
   1 SF-b                           43.66     338.36      41.53     240.76          8      56.2%      25.0%      [1, 0, 1, 1, 1]
   2 SF-c                           -0.00     798.10       0.00     240.76          8      50.0%       0.0%      [2, 0, 0, 0, 2]
   3 SF-a                          -43.66     338.36     -41.53     240.76          8      43.8%      25.0%      [1, 1, 1, 0, 1]
--------------------------------------------------
Finished match
"""

let private reasonOf (white: string) (black: string) (text: string) =
    if text.EndsWith " mates" then Checkmate
    elif text = "Draw by stalemate" then Stalemate
    elif text = "Draw by insufficient mating material" then AdjudicateMaterial
    elif text = "Draw by 3-fold repetition" then Repetition
    elif text = "Draw by fifty moves rule" then ExcessiveMoves
    elif text.EndsWith "SyzygyTB" then AdjudicateTB
    elif text.Contains "by adjudication" then AdjudicatedEvaluation
    elif text.Contains "loses on time" then ForfeitLimits
    elif text.EndsWith "makes an illegal move" then Illegal
    elif text.EndsWith " disconnects" then Disconnected(if text.StartsWith "White" then white else black)
    else failwithf "unknown reason %s" text

/// Replays the Started/Finished lines of a reference run into a Reporter set up from the same
/// command line; returns the reference's text and the Reporter's.
let private replay (args: string) (reference: string) =
    let p =
        match MatchArgs.parse env (args.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
        | MatchArgs.Run p -> p
        | other -> failwithf "%A" other
    let t = p.Tournament
    let lines = reference.Replace("\r", "").Trim('\n').Split('\n')
    let total =
        lines |> Array.pick (fun l -> let m = Regex.Match(l, @"^Started game \d+ of (\d+)") in if m.Success then Some(int m.Groups.[1].Value) else None)
    let cfg : MatchOutput.Config =
        { Output = t.Output; ReportPenta = t.ReportPenta; RatingInterval = t.RatingInterval; ScoreInterval = t.ScoreInterval
          Sprt = MatchSprt.create t.Sprt.Alpha t.Sprt.Beta t.Sprt.Elo0 t.Sprt.Elo1 t.Sprt.Model t.Sprt.Enabled
          Engines = p.Engines
          EngineOption = (fun _ opt -> match opt with "Threads" -> Some "1" | "Hash" -> Some "16" | _ -> None)
          Book = t.Opening.File; TotalGames = total; PriorGames = 0 }
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(cfg, fun s -> sb.Append s |> ignore)
    let mutable ending = MatchOutput.Completed
    for l in lines do
        let m = Regex.Match(l, @"^Started game (\d+) of \d+ \((.+) vs (.+)\)$")
        if m.Success then rep.Started(int m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)
        let m = Regex.Match(l, @"^Finished game (\d+) \((.+) vs (.+)\): (\S+) \{(.*)\}$")
        if m.Success then
            let id = int m.Groups.[1].Value
            let w, b = m.Groups.[2].Value, m.Groups.[3].Value
            let pair = (id + 1) / 2
            let g =
                { GameNr = id; RoundNr = $"{pair}.{(id - 1) % 2 + 1}"; White = w; Black = b; OpeningHash = string pair
                  Result = { Result.Empty with Player1 = w; Player2 = b; Result = m.Groups.[4].Value; Reason = reasonOf w b m.Groups.[5].Value } }
            if (rep.Finished g).IsSome then ending <- MatchOutput.SprtStopped
    rep.End(ending, TimeSpan.FromSeconds 8.0) |> ignore
    // The reference's Total Time line is the run's own; everything else must match
    let ours = Regex.Replace(sb.ToString(), @"Total Time: [^\n]*\n\n$", "")
    String.concat "\n" lines, ours.TrimEnd('\n')

[<Fact>]
let ``default format, pentanomial, SPRT: the same text as the reference`` () =
    let expected, actual = replay runA outA
    Assert.Equal(expected, actual)

[<Fact>]
let ``cutechess format with a score line per game: the same text as the reference`` () =
    let expected, actual = replay runB outB
    Assert.Equal(expected, actual)

[<Fact>]
let ``three engines, ranking table: the same text as the reference`` () =
    let expected, actual = replay runC outC
    Assert.Equal(expected, actual)

// ---- reasons, end lines ----

let private result w b r reason = { Result.Empty with Player1 = w; Player2 = b; Result = r; Reason = reason }

[<Fact>]
let ``EngineBattle reasons in the reference's words`` () =
    let a r reason = MatchOutput.annotation "W" (result "W" "B" r reason)
    Assert.Equal("White mates", a "1-0" Checkmate)
    Assert.Equal("Black mates", a "0-1" Checkmate)
    Assert.Equal("Draw by stalemate", a "1/2-1/2" Stalemate)
    Assert.Equal("Draw by insufficient mating material", a "1/2-1/2" AdjudicateMaterial)
    Assert.Equal("Draw by 3-fold repetition", a "1/2-1/2" Repetition)
    Assert.Equal("Draw by fifty moves rule", a "1/2-1/2" ExcessiveMoves)
    Assert.Equal("Black wins by adjudication: SyzygyTB", a "0-1" AdjudicateTB)
    Assert.Equal("Draw by adjudication: SyzygyTB", a "1/2-1/2" AdjudicateTB)
    Assert.Equal("White wins by adjudication", a "1-0" AdjudicatedEvaluation)
    Assert.Equal("Draw by adjudication", a "1/2-1/2" AdjudicatedEvaluation)
    Assert.Equal("White loses on time (0ms overrun)", a "0-1" ForfeitLimits)
    Assert.Equal("Black loses on time (37ms overrun)", MatchOutput.annotation "W" { result "W" "B" "1-0" ForfeitLimits with TimeOverrunMs = 37L })
    Assert.Equal("Black makes an illegal move", a "1-0" Illegal)
    Assert.Equal("White resigns", a "0-1" Resignation)
    Assert.Equal("Black disconnects", a "1-0" (Disconnected "B"))
    Assert.Equal("White disconnects", a "0-1" (Disconnected "W"))

let private h2h output =
    let p =
        match MatchArgs.parse env [ "-engine"; "cmd=a"; "name=A"; "tc=10+0.1"; "-engine"; "cmd=b"; "name=B"; "tc=40/60+1"; "-output"; "format=" + output ] with
        | MatchArgs.Run p -> p
        | other -> failwithf "%A" other
    let t = p.Tournament
    let cfg : MatchOutput.Config =
      { Output = t.Output; ReportPenta = false; RatingInterval = 1; ScoreInterval = 1
        Sprt = MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" false; Engines = p.Engines
        EngineOption = (fun name opt -> if name = "A" && opt = "Threads" then Some "2" else None)
        Book = ""; TotalGames = 4; PriorGames = 2 }
    cfg

let private game nr w b r reason = { GameNr = nr; RoundNr = "1.1"; White = w; Black = b; OpeningHash = "h"; Result = result w b r reason }

[<Fact>]
let ``resumed numbering, a header without a book, the player table and an interrupted end`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(h2h "fastchess", fun s -> sb.Append s |> ignore)
    rep.Started(1, "A", "B")
    rep.Finished(game 1 "A" "B" "0-1" ForfeitLimits) |> ignore
    let code = rep.End(MatchOutput.Interrupted("./eb", "config.json"), TimeSpan(1, 2, 3))
    Assert.Equal(1, code)
    let text = sb.ToString()
    Assert.StartsWith("Started game 3 of 4 (A vs B)\nFinished game 3 (A vs B): 0-1 {White loses on time (0ms overrun)}\n", text)
    Assert.Contains("Results of A vs B (10+0.1 - 40/60+1, 2t - NULL, NULL):\n", text)
    Assert.EndsWith(
        "\nPlayer: A\n  Timeouts: 1\n  Crashed: 0\n\n"
        + "Tournament was interrupted. To resume the tournament, run: ./eb -config file=config.json\n"
        + "Finished match\nTotal Time: 01:02:03 (hours:minutes:seconds)\n\n", text)

[<Fact>]
let ``a completed run without time losses or crashes has no player table`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(h2h "cutechess", fun s -> sb.Append s |> ignore)
    Assert.Equal(0, rep.End(MatchOutput.Completed, TimeSpan.FromSeconds 5.0))
    Assert.Equal("Finished match\nTotal Time: 00:00:05 (hours:minutes:seconds)\n\n", sb.ToString())

[<Fact>]
let ``after an SPRT decision nothing more is reported`` () =
    let sb = Text.StringBuilder()
    let cfg = { h2h "fastchess" with Sprt = MatchSprt.create 0.4 0.4 0.0 5.0 "normalized" true; ReportPenta = false; TotalGames = 1000; PriorGames = 0 }
    let rep = MatchOutput.Reporter(cfg, fun s -> sb.Append s |> ignore)
    let mutable n = 0
    let mutable decided = false
    while not decided && n < 500 do
        n <- n + 1
        decided <- (rep.Finished(game n "A" "B" "1-0" Checkmate)).IsSome
    Assert.True decided
    let before = sb.Length
    rep.Started(n + 1, "A", "B")
    Assert.Equal(None, rep.Finished(game (n + 1) "A" "B" "1-0" Checkmate))
    Assert.Equal(before, sb.Length)
    Assert.Contains("SPRT ([0.00, 5.00]) completed - H1 was accepted\n", sb.ToString())

// ---- fmt's number formats, as fmt printed them: {:.17g} | {} | {:.2g} ----

let private fmtReference = """
10|10|10
0.5|0.5|0.5
60|60|60
0.02|0.02|0.02
0.10000000000000001|0.1|0.1
1|1|1
100|100|1e+02
1234.5|1234.5|1.2e+03
69.650000000000006|69.65|70
0.002|0.002|0.002
0.0001|0.0001|0.0001
1.0000000000000001e-05|1e-05|1e-05
1.2345e-05|1.2345e-05|1.2e-05
1000000000000000|1000000000000000|1e+15
10000000000000000|1e+16|1e+16
15000000000000000|1.5e+16|1.5e+16
1.2345678901234568e+17|1.2345678901234568e+17|1.2e+17
0.050000000000000003|0.05|0.05
0.25|0.25|0.25
0.095000000000000001|0.095|0.095
0.099500000000000005|0.0995|0.1
9.5|9.5|9.5
99.5|99.5|1e+02
0.29999999999999999|0.3|0.3
2.6749999999999998|2.675|2.7
0.33333333333333331|0.3333333333333333|0.33
86400|86400|8.6e+04
3600.5|3600.5|3.6e+03
7.2000000000000002e-05|7.2e-05|7.2e-05
4.9406564584124654e-324|5e-324|4.9e-324
"""

[<Fact>]
let ``shortest and general2 print as fmt's {} and {:.2g}`` () =
    for line in fmtReference.Trim().Split('\n') do
        let p = line.Trim().Split('|')
        let x = Double.Parse(p.[0], Globalization.CultureInfo.InvariantCulture)
        Assert.Equal(p.[1], MatchFormat.shortest x)
        Assert.Equal(p.[2], MatchFormat.general2 x)

// ---- details the replayed runs do not reach ----

let private cute scoreInterval total sprt =
    { h2h "cutechess" with ScoreInterval = scoreInterval; TotalGames = total; PriorGames = 0; RatingInterval = 100; Sprt = sprt }

[<Fact>]
let ``cutechess: a score line on the score interval and at the last game`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(cute 3 4 (MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" false), fun s -> sb.Append s |> ignore)
    for n in 1 .. 4 do rep.Finished(game n "A" "B" "1/2-1/2" Repetition) |> ignore
    let scores = sb.ToString().Split('\n') |> Array.filter (fun l -> l.StartsWith "Score of")
    Assert.Equal<string[]>([| "Score of A vs B: 0 - 0 - 3  [0.500] 3"; "Score of A vs B: 0 - 0 - 4  [0.500] 4" |], scores)

[<Fact>]
let ``cutechess SPRT stop: score line, Elo line, SPRT line, Tournament finished`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(cute 1000 1000 (MatchSprt.create 0.4 0.4 0.0 5.0 "normalized" true), fun s -> sb.Append s |> ignore)
    let mutable n = 0
    while (n <- n + 1; (rep.Finished(game n "A" "B" "1-0" Checkmate)).IsNone) && n < 500 do ()
    let tail = sb.ToString().TrimEnd('\n').Split('\n') |> Array.rev |> Array.truncate 5 |> Array.rev
    Assert.StartsWith("Finished game", tail.[0])
    Assert.StartsWith($"Score of A vs B: {n} - 0 - 0", tail.[1])
    Assert.StartsWith("Elo difference:", tail.[2])
    Assert.StartsWith("SPRT: llr", tail.[3])
    Assert.EndsWith(" - H1 was accepted", tail.[3])
    Assert.Equal("Tournament finished", tail.[4])

[<Fact>]
let ``cutechess has no player table`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(cute 1 4 (MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" false), fun s -> sb.Append s |> ignore)
    rep.Finished(game 1 "A" "B" "0-1" ForfeitLimits) |> ignore
    sb.Clear() |> ignore
    rep.End(MatchOutput.Completed, TimeSpan.Zero) |> ignore
    Assert.Equal("Finished match\nTotal Time: 00:00:00 (hours:minutes:seconds)\n\n", sb.ToString())

[<Fact>]
let ``a game of an engine not in the match is reported but not counted`` () =
    let sb = Text.StringBuilder()
    let rep = MatchOutput.Reporter(cute 1 4 (MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" false), fun s -> sb.Append s |> ignore)
    Assert.Equal(None, rep.Finished(game 1 "A" "Renamed" "1-0" Checkmate))
    Assert.Equal("Finished game 1 (A vs Renamed): 1-0 {White mates}\n", sb.ToString())
    Assert.Equal(0, rep.Scoreboard.Games)

[<Fact>]
let ``a time loss carries how far below zero the clock went`` () =
    let r = ChessLibrary.GameHelpers.lostOnTimeResult "A" "B" false (Collections.Generic.List<string>()) (Diagnostics.Stopwatch.GetTimestamp()) (TimeSpan.FromMilliseconds -37.4) []
    Assert.Equal(("B", "A", "1-0", ForfeitLimits, 37L), (r.Player1, r.Player2, r.Result, r.Reason, r.TimeOverrunMs))
    Assert.Equal("Black loses on time (37ms overrun)", MatchOutput.annotation r.Player1 r)

[<Fact>]
let ``a resumed run starts from the games already played`` () =
    let sb = Text.StringBuilder()
    let cfg = { h2h "fastchess" with ReportPenta = true; RatingInterval = 1; TotalGames = 4; PriorGames = 2 }
    let rep = MatchOutput.Reporter(cfg, fun s -> sb.Append s |> ignore)
    rep.Preload("A", "B", "1-0", "h1")
    rep.Preload("B", "A", "0-1", "h1")
    Assert.Equal(0, sb.Length)
    // the runner numbers this run's games from 1; the output adds the two played before
    rep.Finished({ game 1 "A" "B" "1/2-1/2" Repetition with RoundNr = "2.1"; OpeningHash = "h2" }) |> ignore
    rep.Finished({ game 2 "B" "A" "1/2-1/2" Repetition with RoundNr = "2.2"; OpeningHash = "h2" }) |> ignore
    let text = sb.ToString()
    Assert.Contains("Finished game 3 (A vs B)", text)
    Assert.Contains("Finished game 4 (B vs A)", text)
    Assert.Contains("Games: 4, Wins: 2, Losses: 0, Draws: 2", text)
    Assert.Contains("Ptnml(0-2): [0, 0, 1, 0, 1]", text)
