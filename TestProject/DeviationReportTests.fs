module DeviationReportTests

open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes

// ---------------------------------------------------------------------------
// The console verb's report. Two real, replayable games with no comments: the
// opening cannot be identified, so every ply counts as a choice, and A plays
// 2.Nf3 in one game and 2.Nc3 in the other from the same position - one self
// deviation, nothing else shared after that.
// ---------------------------------------------------------------------------

let private twoGames () =
  let pgn moves =
    sprintf "[Event \"t\"]\n[White \"A\"]\n[Black \"B\"]\n[Result \"1/2-1/2\"]\n\n%s 1/2-1/2\n" moves
  // Distinct game numbers matter: the scan keeps one move per game per position, keyed on
  // the number, so two games sharing 0 would collapse into one and show no deviation.
  let g1 = { FullPGNParser.parseFullPgnGame (pgn "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6") with GameNumber = 1 }
  let g2 = { FullPGNParser.parseFullPgnGame (pgn "1. e4 e5 2. Nc3 Nc6 3. Bc4 Nf6") with GameNumber = 2 }
  [ g1; g2 ]

[<Fact>]
let ``the report names the self-deviating engine, both moves, and warns about the opening`` () =
  let devs, summary, coverage = DeviationAnalysis.analyzePositionDeviations (twoGames ())
  let report = DeviationAnalysis.printPositionDeviationsToConsole devs summary coverage

  Assert.Single devs |> ignore
  Assert.Contains("Position deviations: 1 (1 self, 0 cross)", report)
  Assert.Contains("(self: A)", report)
  Assert.Contains("Nf3 [A: #1 1/2-1/2]", report)
  Assert.Contains("Nc3 [A: #2 1/2-1/2]", report)
  // No search data and no book marker in either game.
  Assert.Contains("WARNING: 2 game(s)", report)
  // Two positions reached in both games (the start, and after 1.e4 e5), one of them played
  // differently: 50%, shown as it is with the denominator beside it.
  Assert.Contains("50.0%", report)

[<Fact>]
let ``the report on no games says so instead of throwing`` () =
  let devs, summary, coverage = DeviationAnalysis.analyzePositionDeviations Seq.empty
  let report = DeviationAnalysis.printPositionDeviationsToConsole devs summary coverage

  Assert.Contains("Position deviations: 0 (0 self, 0 cross)", report)
  Assert.Contains("not measurable", report)

let private game number (tags: string) moves =
  let text = sprintf "[Event \"t\"]\n[White \"A\"]\n[Black \"B\"]\n%s[Result \"*\"]\n\n%s *\n" tags moves
  { FullPGNParser.parseFullPgnGame text with GameNumber = number }

let private badFen = game 3 "[SetUp \"1\"]\n[FEN \"rnbqkbnr/pppp/8 w\"]\n" "1. e4 e5"

[<Fact>]
let ``a game whose FEN cannot be set up is left out of the scan and counted, not a crash`` () =
  let devs, summary, coverage = DeviationAnalysis.analyzePositionDeviations (twoGames () @ [ badFen ])
  let report = DeviationAnalysis.printPositionDeviationsToConsole devs summary coverage
  Assert.Single devs |> ignore                 // the two good games still give their deviation
  Assert.Equal(1, coverage.BadStart)
  Assert.Equal(3, coverage.Total)
  Assert.Contains("bad start FEN 1 (left out)", report)

[<Fact>]
let ``the move-difference finder leaves out a game whose FEN cannot be set up`` () =
  DeviationAnalysis.findMoveDifferencesInPGN (twoGames () @ [ badFen ]) "" [ "A" ] |> ignore

[<Fact>]
let ``the move-difference finder stops a game at a move that cannot be played`` () =
  // the first move cannot be played: there was no previous move to read
  DeviationAnalysis.findMoveDifferencesInPGN (twoGames () @ [ game 4 "" "1. Ke3 e5 2. Nf3" ]) "" [ "A" ] |> ignore
