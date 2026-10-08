module SpeedFormatTests

open Xunit
open ChessLibrary.GameAnalysis

// The speed tables: node counts per move are "npm", speeds "nps"; a value of exactly a million (or
// a billion) moves up a prefix rather than reading 1000.0K.

[<Fact>]
let ``nodes per move are npm and speeds nps`` () =
  Assert.Equal("9.3Knpm", Formatting.formatNPM 9_300.0)
  Assert.Equal("9.3Knps", Formatting.formatNPS 9_300.0)

[<Fact>]
let ``exactly a million or a billion moves up a prefix`` () =
  Assert.Equal("1.0Mnps", Formatting.formatNPS 1_000_000.0)
  Assert.Equal("1.0Gnps", Formatting.formatNPS 1_000_000_000.0)
  Assert.Equal("1.0Mnpm", Formatting.formatNPM 1_000_000.0)
  Assert.Equal("999.9Knps", Formatting.formatNPS 999_900.0)

open System
open System.IO
open ChessLibrary
open ChessLibrary.Configuration
open ChessLibrary.TypesDef.CoreTypes

// The speed table says what its numbers are; the average row's time is an average, not the median.

let private row median = { Player = "A"; Median = median; Games = 1; EPS = 0.0; AvgNPS = 40_000.0; AvgNodes = 9_244.0; AvgDepth = 10.0; AvgSelfDepth = 14.0; Time = 211L }

[<Fact>]
let ``the speed table says whether it shows medians or averages`` () =
  Assert.Contains("Medians per move", ConsoleHelper.writeSummaryEngineStatsToConsole [ row true ])
  Assert.Contains("Averages per move", ConsoleHelper.writeSummaryEngineStatsToConsole [ row false ])
  Assert.Contains("9.2Knpm", ConsoleHelper.writeSummaryEngineStatsToConsole [ row true ])

[<Fact>]
let ``the average row's move time is the average, the median row's the median`` () =
  // A's five moves take 100, 100, 100, 400 and 400 ms: median 100, average 220, no outliers
  let comment mt = sprintf "{wv=0.10, mt=%d, s=40000, eps=0, n=%d, d=10, sd=14, pd=e5, tl=0, tb=0, pv=x}" mt (mt * 40)
  let moves =
    [ "e4"; "e5"; "Nf3"; "Nc6"; "Bb5"; "a6"; "Ba4"; "Nf6"; "O-O"; "Be7" ]
    |> List.mapi (fun i san ->
      let mt = if i % 2 = 0 then [| 100; 100; 100; 400; 400 |].[i / 2] else 100
      (if i % 2 = 0 then sprintf "%d. " (i / 2 + 1) else "") + san + " " + comment mt)
    |> String.concat " "
  let path = Path.Combine(Path.GetTempPath(), "eb-speed-" + Guid.NewGuid().ToString "N" + ".pgn")
  File.WriteAllText(path, sprintf "[Event \"t\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n\n%s *\n" moves)
  let rows = PGNStatistics.calculateMedianAndAvgSpeedSummaryInPgnFile(FullPGNParser.parsePgnFile path |> Array.ofSeq, 0)
  let a median = rows |> Array.find (fun r -> r.Player = "A" && r.Median = median)
  Assert.Equal(100L, (a true).Time)
  Assert.Equal(220L, (a false).Time)
