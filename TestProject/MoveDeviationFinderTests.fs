[<Xunit.Collection("NonParallelParser")>]
module TestProject.MoveDeviationFinderTests

open System
open System.IO
open System.Reflection
open Xunit
open ChessLibrary
open TypesDef.PGNTypes

[<Literal>]
let private samplePgnFile = "SF_vs_Ceres_and_Lc0.pgn"

let private tryFindFile (relativeParts: string list) =
  let fileName = samplePgnFile

  let asmLoc = Assembly.GetExecutingAssembly().Location
  let asmDir =
    let d = Path.GetDirectoryName asmLoc
    if isNull d then "" else d

  let candidates =
    [
      Path.Combine(AppContext.BaseDirectory, Path.Combine(Array.ofList (relativeParts @ [ fileName ])))
      Path.Combine(Directory.GetCurrentDirectory(), Path.Combine(Array.ofList (relativeParts @ [ fileName ])))
      Path.Combine(asmDir, Path.Combine(Array.ofList (relativeParts @ [ fileName ])))
    ]

  candidates |> List.tryFind File.Exists

let private tryFindByWalkingUp (startDir: string) =
  let rec loop dir depth =
    if String.IsNullOrWhiteSpace dir || depth <= 0 then None
    else
      let probe1 = Path.Combine(dir, "TestProject", "TestData", samplePgnFile)
      let probe2 = Path.Combine(dir, "TestData", samplePgnFile)
      if File.Exists probe1 then Some probe1
      elif File.Exists probe2 then Some probe2
      else
        let parent = Directory.GetParent dir
        if isNull parent then None else loop parent.FullName (depth - 1)
  loop startDir 10

[<Fact>]
let ``MoveDeviationFinder runs on sample PGN`` () =
  let pgnPath =
    tryFindFile [ "TestData" ]
    |> Option.orElseWith (fun () -> tryFindFile [ "TestProject"; "TestData" ])
    |> Option.orElseWith (fun () -> tryFindByWalkingUp AppContext.BaseDirectory)
    |> Option.defaultWith (fun () ->
      failwith $"Missing test PGN '{samplePgnFile}'. Looked in output/current directories and walked up from '{AppContext.BaseDirectory}'.")

  let options =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.FixedPly 0
        EventFilter = MoveDeviationFinder.IncludeAll
        MinSamplesForBaseline = 1 }

  let report = MoveDeviationFinder.analyzePgnFile pgnPath options

  Assert.True(report.Stats.Games > 0)
  Assert.True(report.Stats.Openings > 0)
  Assert.True(report.Stats.Nodes > 0)

[<Fact>]
let ``MoveDeviationFinder runs on hard path PGN and returns events/self-deviations`` () =
  let pgnPath = @"C:\Dev\Chess\PGNs\TestOfCeresV2_24.pgn"
  Assert.True(File.Exists(pgnPath), $"Hard-path PGN not found: {pgnPath}")

  let options =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.WhileBookComment
        Baseline = MoveDeviationFinder.Majority MoveDeviationFinder.DecisiveOnly
        EventFilter = MoveDeviationFinder.OnlyDecisive
        DeviationSignificance = MoveDeviationFinder.AllDeviations
        KeepOnlyLastDeviationPerGame = true
        MinSamplesForBaseline = 1 }

  let report = MoveDeviationFinder.analyzePgnFile pgnPath options
  let opts = MoveDeviationFinder.ReportFormatOptions.Default
  let res = MoveDeviationFinder.formatReport report opts
  

  Assert.True(report.Stats.Games > 0)
  Assert.Equal(report.Events.Length, report.Stats.Events)
  Assert.Equal(report.SelfDeviations.Length, report.Stats.SelfDeviations)
  if report.Events.Length > 0 then
    let maxPerGame =
      report.Events
      |> Seq.countBy (fun e -> e.GameNumber)
      |> Seq.maxBy snd
      |> snd
    Assert.True(maxPerGame <= 1, $"Expected at most 1 event per game when KeepOnlyLastDeviationPerGame=true, but saw {maxPerGame}.")
  Assert.True(report.Events.Length > 0 || report.SelfDeviations.Length > 0, "Expected at least one deviation signal (events or self-deviations).")

[<Fact>]
let ``MoveDeviationFinder ignores same-game repeated-position deviations`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  let mkGame (gameNumber: int) (round: string) (white: string) (black: string) (moves: Move list) =
    let meta =
      { GameMetadata.Empty with
          Round = round
          White = white
          Black = black
          Result = "1/2-1/2"
          OpeningHash = "test-opening" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Moves = ResizeArray(moves) }

  // Game 1 repeats the starting position after 1.Nf3 Nf6 2.Ng1 Ng8, then plays 3.e4 e5.
  // Without per-game dedupe, this could look like a "self deviation" within the same game.
  let g1 =
    mkGame 1 "1.1" "EngineA" "EngineB"
      [ mkMove 1 "Nf3" "Nf6"
        mkMove 2 "Ng1" "Ng8"
        mkMove 3 "e4" "e5" ]

  // Game 2 chooses Nf3 from the start position.
  let g2 =
    mkGame 2 "1.2" "EngineA" "EngineB"
      [ mkMove 1 "Nf3" "Nf6" ]

  // Game 3 chooses e4 from the start position.
  let g3 =
    mkGame 3 "1.3" "EngineA" "EngineB"
      [ mkMove 1 "e4" "e5" ]

  let options =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.FixedPly 0
        Baseline = MoveDeviationFinder.Majority MoveDeviationFinder.AllGames
        EventFilter = MoveDeviationFinder.IncludeAll
        MinSamplesForBaseline = 1 }

  let report = MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ] options

  // Deviation event should be for Game 3 at ply 0: e4 deviates from majority baseline Nf3 (2 games).
  Assert.Contains(report.Events, fun e -> e.GameNumber = 3 && e.Ply = 0 && e.Played.San = "e4" && e.Baseline.San = "Nf3")

  // The repeated start position inside Game 1 should not create an event for e4 within that same game.
  Assert.DoesNotContain(report.Events, fun e -> e.GameNumber = 1 && e.Played.San = "e4")

  // Self deviations should be based on across-game differences (Nf3 in games 1+2, e4 in game 3),
  // and should not attribute e4 to game 1 even though it occurred after repetition.
  let sd =
    report.SelfDeviations
    |> List.find (fun s -> s.Engine = "EngineA" && s.OpeningKey = "test-opening" && s.Moves |> List.exists (fun m -> m.San = "Nf3") && s.Moves |> List.exists (fun m -> m.San = "e4"))

  let e4Lan = sd.Moves |> List.find (fun m -> m.San = "e4") |> fun m -> m.Lan
  let e4Games =
    sd.ExamplesByMove
    |> Map.tryFind e4Lan
    |> Option.defaultValue []
    |> List.map (fun o -> o.GameNumber)
    |> Set.ofList

  Assert.DoesNotContain(1, e4Games)
  Assert.Contains(3, e4Games)

[<Fact>]
let ``WinnerMove baseline picks move with decisive wins`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  let mkGame (gameNumber: int) (round: string) (white: string) (black: string) (result: string) (moves: Move list) =
    let meta =
      { GameMetadata.Empty with
          Round = round
          White = white
          Black = black
          Result = result
          OpeningHash = "winner-open" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Moves = ResizeArray(moves) }

  // Same opening/position: EngineA chooses different first moves across games with decisive outcomes.
  let g1 = mkGame 1 "1.1" "EngineA" "EngineB" "1-0" [ mkMove 1 "Nf3" "d5" ]
  let g2 = mkGame 2 "1.2" "EngineA" "EngineB" "0-1" [ mkMove 1 "e4" "d5" ]
  let g3 = mkGame 3 "1.3" "EngineA" "EngineB" "1-0" [ mkMove 1 "Nf3" "d5" ]

  let options =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.FixedPly 0
        Baseline = MoveDeviationFinder.WinnerMove
        EventFilter = MoveDeviationFinder.IncludeAll
        MinSamplesForBaseline = 1 }

  let report = MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ] options

  // WinnerMove should baseline to Nf3, so game 2 should be a deviation event at ply 0.
  Assert.Contains(report.Events, fun e -> e.GameNumber = 2 && e.Ply = 0 && e.Played.San = "e4" && e.Baseline.San = "Nf3")

[<Fact>]
let ``OnlyMissedWin filters to deviations with clearly worse outcomes`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  let mkGame (gameNumber: int) (round: string) (white: string) (black: string) (result: string) (moves: Move list) =
    let meta =
      { GameMetadata.Empty with
          Round = round
          White = white
          Black = black
          Result = result
          OpeningHash = "missed-open" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Moves = ResizeArray(moves) }

  // Baseline candidate: Nf3 wins twice decisively for EngineA.
  let g1 = mkGame 1 "1.1" "EngineA" "EngineB" "1-0" [ mkMove 1 "Nf3" "d5" ]
  let g2 = mkGame 2 "1.2" "EngineA" "EngineB" "1-0" [ mkMove 1 "Nf3" "d5" ]
  // Inferior candidate: e4 loses decisively for EngineA.
  let g3 = mkGame 3 "1.3" "EngineA" "EngineB" "0-1" [ mkMove 1 "e4" "d5" ]

  let baseOptions =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.FixedPly 0
        Baseline = MoveDeviationFinder.WinnerMove
        EventFilter = MoveDeviationFinder.IncludeAll
        MinSamplesForBaseline = 1 }

  let allReport = MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ] { baseOptions with DeviationSignificance = MoveDeviationFinder.AllDeviations }
  Assert.Contains(allReport.Events, fun e -> e.GameNumber = 3 && e.Played.San = "e4")

  let missedReport =
    MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ]
      { baseOptions with
          DeviationSignificance = MoveDeviationFinder.OnlyMissedWin
          MissedWinMinDelta = 0.50
          MissedWinMinBestScore = 0.80
          MissedWinUseDecisiveOnly = true }

  Assert.Contains(missedReport.Events, fun e -> e.GameNumber = 3 && e.Played.San = "e4" && e.Baseline.San = "Nf3")

[<Fact>]
let ``OnlyMissedWin ignores openings with all draws`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  let mkGame (gameNumber: int) (round: string) (white: string) (black: string) (moves: Move list) =
    let meta =
      { GameMetadata.Empty with
          Round = round
          White = white
          Black = black
          Result = "1/2-1/2"
          OpeningHash = "all-draw-open" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Moves = ResizeArray(moves) }

  // Same opening: two different moves, but all games are draws => no missed-win events.
  let g1 = mkGame 1 "1.1" "EngineA" "EngineB" [ mkMove 1 "Nf3" "d5" ]
  let g2 = mkGame 2 "1.2" "EngineA" "EngineB" [ mkMove 1 "e4" "d5" ]

  let report =
    MoveDeviationFinder.analyzePgnGames [ g1; g2 ]
      { MoveDeviationFinder.AnalysisOptions.Default with
          OpeningCutoff = MoveDeviationFinder.FixedPly 0
          Baseline = MoveDeviationFinder.Majority MoveDeviationFinder.AllGames
          DeviationSignificance = MoveDeviationFinder.OnlyMissedWin
          MissedWinRequireDecisiveOpening = true }

  Assert.Empty(report.Events)

[<Fact>]
let ``OnlyMissedWin uses FEN side-to-move (not ply parity)`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  // Start position with BLACK to move.
  let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

  let mkGame (gameNumber: int) (result: string) (blackFirstMove: string) =
    let meta =
      { GameMetadata.Empty with
          Round = string gameNumber
          White = "EngineW"
          Black = "EngineB"
          Result = result
          OpeningHash = "fen-stm-open" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Fen = startFen
        Moves = ResizeArray([ mkMove 1 "" blackFirstMove ]) }

  // From this position, black plays either ...Nf6 (wins for black) or ...a6 (wins for white).
  // With WinnerMove baseline and OnlyMissedWin, ...a6 should be flagged as a missed win for black,
  // but only if side-to-move is derived from the FEN ('b'), not ply parity.
  let g1 = mkGame 1 "0-1" "Nf6"
  let g2 = mkGame 2 "1-0" "a6"
  let g3 = mkGame 3 "0-1" "Nf6"

  let report =
    MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ]
      { MoveDeviationFinder.AnalysisOptions.Default with
          OpeningCutoff = MoveDeviationFinder.FixedPly 0
          Baseline = MoveDeviationFinder.WinnerMove
          DeviationSignificance = MoveDeviationFinder.OnlyMissedWin
          MissedWinMinDelta = 0.50
          MissedWinMinBestScore = 0.80
          MissedWinUseDecisiveOnly = true
          MissedWinRequireDecisiveOpening = false }

  Assert.Contains(report.Events, fun e -> e.GameNumber = 2 && e.EngineToMove = "EngineB" && e.Played.San = "a6")

[<Fact>]
let ``KeepOnlyLastDeviationPerGame keeps only last deviation in a game`` () =
  let mkMove (nr: int) (w: string) (b: string) =
    { MoveNr = string nr
      WhiteSan = w
      WhiteComment = ""
      BlackSan = b
      BlackComment = "" }

  let mkGame (gameNumber: int) (round: string) (result: string) (moves: Move list) =
    let meta =
      { GameMetadata.Empty with
          Round = round
          White = "EngineA"
          Black = "EngineB"
          Result = result
          OpeningHash = "keep-last-open" }
    { PgnGame.Empty gameNumber with
        GameMetaData = meta
        Moves = ResizeArray(moves) }

  // g1/g2 establish baselines:
  // - After 1.Nf3 (black to move), baseline is 1...d5.
  // - After reaching the transposed position (1.Nf3 Nf6 2.g3 d5 == 1.Nf3 d5 2.g3 Nf6), baseline is 3.Bg2.
  let g1 =
    mkGame 1 "1.1" "1/2-1/2"
      [ mkMove 1 "Nf3" "d5"
        mkMove 2 "g3" "Nf6"
        mkMove 3 "Bg2" "e6" ]

  let g2 =
    mkGame 2 "1.2" "1/2-1/2"
      [ mkMove 1 "Nf3" "d5"
        mkMove 2 "g3" "Nf6"
        mkMove 3 "Bg2" "e6" ]

  // g3 deviates twice but still reaches the same transposed position before the second deviation:
  // - Deviation 1: 1...Nf6 instead of 1...d5 (black).
  // - Deviation 2: 3.d4 instead of 3.Bg2 (white) from the transposed position.
  let g3 =
    mkGame 3 "1.3" "1/2-1/2"
      [ mkMove 1 "Nf3" "Nf6"
        mkMove 2 "g3" "d5"
        mkMove 3 "d4" "e6" ]

  let baseOptions =
    { MoveDeviationFinder.AnalysisOptions.Default with
        OpeningCutoff = MoveDeviationFinder.FixedPly 0
        Baseline = MoveDeviationFinder.Majority MoveDeviationFinder.AllGames
        EventFilter = MoveDeviationFinder.IncludeAll
        MinSamplesForBaseline = 2 }

  let reportAll = MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ] { baseOptions with KeepOnlyLastDeviationPerGame = false }
  let g3EventsAll = reportAll.Events |> List.filter (fun e -> e.GameNumber = 3) |> List.sortBy (fun e -> e.Ply)
  Assert.True(g3EventsAll.Length >= 2, "Expected at least two deviation events in game 3 before filtering.")

  let reportLast = MoveDeviationFinder.analyzePgnGames [ g1; g2; g3 ] { baseOptions with KeepOnlyLastDeviationPerGame = true }
  let g3EventsLast = reportLast.Events |> List.filter (fun e -> e.GameNumber = 3)
  Assert.Equal(1, g3EventsLast.Length)
  let expectedPly = g3EventsAll |> List.maxBy (fun e -> e.Ply) |> fun e -> e.Ply
  Assert.Equal(expectedPly, g3EventsLast[0].Ply)
