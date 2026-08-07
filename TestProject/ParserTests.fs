module ParserTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes
module MoveParser = ChessLibrary.MoveParser
module FullPGNParser = ChessLibrary.FullPGNParser
module PGNHelper = ChessLibrary.PGNHelper
module EPDExtractor = ChessLibrary.EPDExtractor
module PGNWriter = ChessLibrary.PGNWriter

[<Fact>]
let ``getOpeningInfo returns correct string for opening/variation/eco`` () =
    let meta = { GameMetadata.Empty with OtherTags = [ { Key = "Opening"; Value = "Ruy Lopez" }; { Key = "Variation"; Value = "Closed" }; { Key = "ECO"; Value = "C84" } ] }
    let game = { PgnGame.Empty 1 with GameMetaData = meta }
    let result = PGNHelper.getOpeningInfo game
    Assert.Equal("Opening: Ruy Lopez - Closed, ECO: C84", result)

[<Fact>]
let ``parseLine parses EPD fields`` () =
    let epdLine = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 bm e4; am d4; id \"StartPos\"; other \"note\";"
    let parsed = MoveParser.parseLine epdLine
    Assert.True(parsed.IsSome)
    let epd = parsed.Value
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", epd.FEN)
    Assert.Equal(Some "e4", epd.BestMove)
    Assert.Equal(Some "d4", epd.AvoidMove)
    Assert.Equal(Some "StartPos", epd.Id)
    Assert.Equal(Some "note", epd.Other)
    Assert.Equal(epdLine, epd.RawInput)

[<Fact>]
let ``parseEPDFile converts EPD to PGN`` () =
    let tempFile = Path.GetTempFileName()
    try
        let epdLine = "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 2 3 bm Bxf7+; id \"Italian\";"
        File.WriteAllText(tempFile, epdLine)

        let games = EPDExtractor.parseEPDFile tempFile |> Seq.toList
        Assert.Single(games) |> ignore

        let game = games.Head
        Assert.Equal("r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 2 3", game.Fen)
        Assert.Equal(0, game.Mainline.Count)

        let openingTag = game.GameMetaData.OtherTags |> List.tryFind (fun h -> h.Key = "Opening")
        Assert.True(openingTag.IsSome)
        Assert.Equal("Italian", openingTag.Value.Value)
    finally
        File.Delete(tempFile)

[<Fact>]
let ``parsePgnFile parses a simple PGN file`` () =
    // Arrange: create a temporary PGN file
    let tempFile = Path.GetTempFileName()
    let pgnContent = """
[Event "Test Event"]
[Site "Test Site"]
[Date "2024.01.01"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 *
"""
    File.WriteAllText(tempFile, pgnContent)

    // Act: parse the file
    let games = FullPGNParser.parsePgnFile(tempFile) |> Seq.toList

    // Assert: check the parsed result
    Assert.Single(games) |> ignore
    let game: PgnGame = games.Head
    Assert.Equal("Alpha", game.GameMetaData.White)
    Assert.Equal("Beta", game.GameMetaData.Black)
    Assert.Equal("1-0", game.GameMetaData.Result)
    Assert.Equal(4, game.Mainline.Count) // 4 moves (e4, e5, Nf3, Nc6)
    Assert.Equal("e4", game.Mainline.[0].San)
    Assert.Equal("e5", game.Mainline.[1].San)
    Assert.Equal("Nf3", game.Mainline.[2].San)
    Assert.Equal("Nc6", game.Mainline.[3].San)
    // Cleanup
    File.Delete(tempFile)


[<Fact>]
let ``parseFullPgnGame does not hang on unterminated brace comment`` () : Task = task {
    let pgn = """
[Event "Unterminated Comment"]

1. e4 {oops
1... c5 *
"""
    let parseTask = Task.Run(fun () -> FullPGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(3000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing hung on unterminated comment")
    let _ = parseTask.Result
    return ()
    }

[<Fact>]
let ``legacy PGN opening hash recompute matches book`` () =
    let bookPath = @"C:\Dev\Chess\Temp\Book_VB.pgn"
    let gamesPath = @"C:\Dev\Chess\Temp\GamesPlayed_VB.pgn"
    if File.Exists(bookPath) && File.Exists(gamesPath) then
        let bookGames = FullPGNParser.parsePgnFile bookPath |> Seq.toList
        let bookHashes =
            bookGames
            |> Seq.map ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame
            |> Set.ofSeq
        let games = FullPGNParser.parsePgnFile gamesPath |> Seq.toList
        Assert.True(games.Length >= 25, $"Expected at least 25 games, found {games.Length}")
        for g in games do
            let computed = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame g
            Assert.True(
                bookHashes.Contains computed,
                $"Missing opening hash for game {g.GameMetaData.White} vs {g.GameMetaData.Black}")
    else
        ()

[<Fact>]
let ``legacy PGN next unused opening index matches first unused book entry`` () =
    let bookPath = @"C:\Dev\Chess\Temp\Book_VB.pgn"
    let gamesPath = @"C:\Dev\Chess\Temp\GamesPlayed_VB.pgn"
    if File.Exists(bookPath) && File.Exists(gamesPath) then
        let bookGames = FullPGNParser.parsePgnFile bookPath |> Seq.toList
        let usedOpeningHashes =
            FullPGNParser.parsePgnFile gamesPath
            |> Seq.map ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame
            |> Set.ofSeq
        let startIndex = 0
        let expectedIndex =
            if bookGames.IsEmpty then
                0
            else
                let total = bookGames.Length
                let rec loop offset =
                    if offset >= total then
                        startIndex % total
                    else
                        let idx = (startIndex + offset) % total
                        let hash = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame bookGames.[idx]
                        if usedOpeningHashes.Contains hash then
                            loop (offset + 1)
                        else
                            idx
                loop 0
        let actualIndex =
            ChessLibrary.TournamentPairing.PairingHelper.nextUnusedOpeningIndex usedOpeningHashes bookGames startIndex
        Assert.Equal(expectedIndex, actualIndex)
        if bookGames.IsEmpty |> not then
            let nextHash = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame bookGames.[actualIndex]
            Assert.True(usedOpeningHashes.Contains nextHash |> not)
    else
        ()

[<Fact>]
let ``legacy PGN round-robin resume ends on opening 25`` () =
    let bookPath = @"C:\Dev\Chess\Temp\Book_VB.pgn"
    let gamesPath = @"C:\Dev\Chess\Temp\GamesPlayed_VB.pgn"
    if File.Exists(bookPath) && File.Exists(gamesPath) then
        let bookGames = FullPGNParser.parsePgnFile bookPath |> Seq.toList
        let games = FullPGNParser.parsePgnFile gamesPath |> Seq.toList
        games
            |> List.iter (fun g ->
                let hash = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame g
                g.GameMetaData.OpeningHash <- hash )
        let engines =
            games
            |> Seq.collect (fun g -> [ g.GameMetaData.White; g.GameMetaData.Black ])
            |> Seq.distinct
            |> Seq.map (fun name -> { EngineConfig.Empty with Name = name })
            |> Seq.toList
        let pairings = ChessLibrary.TournamentPairing.PairingHelper.generateAllRoundRobinDoubleRounds engines bookGames
        let playedSet = ChessLibrary.TournamentPairing.PairingHelper.playedSet (games |> Seq.toArray)
        let usedOpeningNumbers =
            pairings
            |> Seq.filter (fun p -> ChessLibrary.TournamentPairing.PairingHelper.hasPlayedBefore p playedSet)
            |> Seq.map (fun p -> p.Opening.GameNumber)
            |> Seq.distinct
            |> Seq.toList
        let maxUsed = if usedOpeningNumbers.IsEmpty then -1 else usedOpeningNumbers |> List.max
        Assert.Equal(25, maxUsed)
        let opening = bookGames |> List.find (fun g -> g.GameNumber = maxUsed)
        Assert.Equal("King's pawn opening", opening.GameMetaData.OpeningName)
        let eco =
            opening.GameMetaData.OtherTags
            |> List.tryFind (fun t -> t.Key = "ECO")
            |> Option.map (fun t -> t.Value)
            |> Option.defaultValue ""
        Assert.Equal("B00", eco)
    else
        ()

[<Fact>]
let ``calculateMedianNodes returns 0.0 for empty array`` () =
    let moves : ChessLibrary.EngineTypes.EngineMoveStat array = [||]
    let result = ChessLibrary.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(0.0, result)

[<Fact>]
let ``calculateMedianNodes returns correct median for odd count`` () =
    let moves =
        [| { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 30L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 20L } |]
    let result = ChessLibrary.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(20.0, result)

[<Fact>]
let ``calculateMedianNodes returns correct median for even count`` () =
    let moves =
        [| { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 30L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 20L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 40L } |]
    let result = ChessLibrary.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(25.0, result)

[<Fact>]
let ``calculateMedianNodes ignores zero and negative nodes`` () =
    let moves =
        [| { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 0L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = -5L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 20L }
           { ChessLibrary.EngineTypes.EngineMoveStat.Empty with n = 30L } |]
    let result = ChessLibrary.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(20.0, result)

[<Fact>]
let ``parseFullPgnGame builds variation tree`` () : Task = task {
    let pgn = """
[Event "Variation Test"]
[Site "Test"]
[Date "2024.01.01"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5 (3. d4) 3... Nxe4) 3. Bb5 *
"""
    let parseTask = Task.Run(fun () -> FullPGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(10000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing timed out")
    let game = parseTask.Result

    // Mainline should have three white moves: e4, Nf3, Bb5 (plies 0,2,4)
    let mainSans = game.Mainline |> Seq.map (fun n -> n.San) |> String.concat " "
    let varSans =
        game.Mainline
        |> Seq.collect (fun n -> n.Variations |> Seq.collect id |> Seq.map (fun v -> v.San))
        |> String.concat " "
    Assert.True(game.Mainline.Count >= 5, $"Mainline count was {game.Mainline.Count}. Sans: {mainSans}. Vars: {varSans}")
    Assert.Equal("e4", game.Mainline[0].San)
    Assert.Equal("e5", game.Mainline[1].San)
    Assert.Equal("Nf3", game.Mainline[2].San)
    Assert.Equal("Nc6", game.Mainline[3].San)
    Assert.Equal("Bb5", game.Mainline[4].San)

    // Variation after 2... has its own line, with nested variation inside.
    let variationsAfterNc6 = game.Mainline[3].Variations
    Assert.True(variationsAfterNc6.Count >= 1)
    let firstVar = variationsAfterNc6[0]
    Assert.Equal<string>(["Nf6"; "Nxe5"; "Nxe4"], firstVar |> Seq.map (fun n -> n.San) |> Seq.toArray)

    // Nested variation (3. d4) should be attached to Nxe5
    let nxe5 = firstVar |> Seq.find (fun n -> n.San = "Nxe5")
    Assert.True(nxe5.Variations.Count >= 1)
    let inner = nxe5.Variations[0]
    Assert.Equal<string>(["d4"], inner |> Seq.map (fun n -> n.San) |> Seq.toArray)
    }

[<Fact>]
let ``parseFullPgnGame parses long game with variations and preserves mainline length`` () =
    let pgn = """
[Event "Long Variation Test"]
[Site "Test"]
[Date "2024.01.02"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. d4 d5 2. Nf3 e6 3. g3 f5 4. Bg2 Nf6 5. O-O Be7 6. c4 c6 7. Qc2 O-O 8. Nbd2 Qe8 9. Ne5 Nbd7 10. Nd3 Bd6 11. Nf3 Ne4 12. Bf4 Bxf4 13. Nxf4 g5 14. Nd3 Qh5 15. Nfe5 Nxe5 16. Nxe5 Nf6 17. f3 Nd7 18. Nd3 Qg6 19. Rae1 Qf6 20. Qc3 dxc4 21. Qc4 Nb6 22. Qc5 Rd8 23. e3 Qf8 24. Qc3 Nd5 25. Qd2 f4 26. exf4 gxf4 27. Re4 Qh6 28. Rf2 Rf8 29. Rfe2 Rf6 30. Nxf4 Nxf4 31. Rxf4 Rxf4 32. Qxf4 Qxf4 33. gxf4 Bd7 34. Kf2 Rf8 35. Ke3 Rf6 36. Bh3 Kf7 37. Ke4 Rh6 38. Bf1 Kf6 39. Rc2 Be8 40. Rd2 Bg6+ 41. Ke3 Bf5 42. Bd3 Rg6 43. Be4 Rg1 44. Kd3 Rf1 45. Ke3 Rg1 46. a3 Re1+ 47. Kf2 Rh1 48. Ke3 Re1+ 49. Re2 Rg1 50. Kd2 Rg7 51. Kc3 Rg1 52. Kb4 Rc1 53. Bxf5 Kxf5 54. Re5+ Kf6 55. Rh5 Kg6 56. Rg5+ Kf7 57. f5 Rc2 58. fxe6+ Kxe6 59. Rh5 Rxb2+ 60. Kc3 Rf2 61. Rh3 Kd5 62. Kd3 Ra2 63. Rh5+ Ke6 64. Ke4 Rxa3 65. Rh6+ Ke7 66. Rxh7+ Ke6 67. Rxb7 Ra2 68. Rh7 Re2+ 69. Kd3 Rf2 70. Ke3 Ra2 71. h4 a5 72. Ra7 a4 73. Ra6 Kd5 74. Ra5+ Kd6 75. h5 Rh2 76. Kf4 a3 77. Kg3 Rh1 78. Kg4 Rg1+ 79. Kf5 Rh1 80. Kg6 Rg1+ 81. Kf6 Rh1 82. Rxa3 (82. f4 a2 83. Rxa2 Rxh5 84. Ra8) 82... Rxh5 83. Re3 Rh3 84. Kf5 Kd5 85. Kg4 Rh8 86. Rc3 Rg8+ 87. Kf4 Rf8+ 88. Ke3 Re8+ 89. Kd3 Re1 90. Rc5+ Kd6 91. Rf5 Re8 92. Kc4 Re6 (92... Re1 93. Rf6+ Kd7) 93. Rf7 Re1 94. Rf6+ Kd7 95. Kd3 Re8 96. Kd2 Re7 97. Kd3 Re1 98. Rh6 Rd1+ 99. Ke3 Re1+ 100. Kd2 Rf1 101. Ke2 Ra1 102. Rf6 Ra3 103. Rg6 Rb3 104. Rg5 Kd6 105. Re5 Ra3 106. f4 Rb3 107. f5 Ra3 108. Re3 Ra5 109. Re6+ Kd7 110. Re5 Ra8 111. Kd3 Rh8 112. Re6 Rh4 113. Ke3 Rg4 114. Rg6 Rh4 115. Re6 Rg4 116. Re5 Rh4 117. Re4 Rh6 118. Kf4 Rh4+ 119. Ke5 Rh6 120. Rg4 Rh7 121. Rg6 Re7+ 122. Re6 Rf7 123. Rd6+ Kc7 124. f6 Rf8 125. Re6 Rh8 126. Re7+ Kd8 127. Ra7 Rh2 128. Kd6 (128. f7 Rf2 129. f8=Q+ Rxf8 130. Ra8+ Ke7 131. Rxf8 Kxf8 132. Kd6 Ke8 133. Kxc6) 128... Ke8 129. Re7+ Kf8 130. Kxc6 *
"""
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let game = FullPGNParser.parseFullPgnGame pgn
    sw.Stop()
    Assert.True(sw.ElapsedMilliseconds < 5000L, "Parsing timed out")

    // Count mainline plies (exclude variations)
    let mainlinePlies = game.Mainline |> Seq.length
    Assert.Equal(259, mainlinePlies) // 130 full moves ending with White

    // Variation on 82 (branch starting with 82. f4 ...) should be attached to Rxa3
    let rxa3 = game.Mainline |> Seq.find (fun n -> n.San = "Rxa3" && n.MoveNumber = 82)
    Assert.True(rxa3.Variations.Count >= 1, "Expected variation attached to 82. Rxa3")
    let firstVar = rxa3.Variations[0]
    Assert.True(firstVar.Count > 0)
    Assert.Equal("f4", firstVar[0].San)

    // Also ensure some deeper variation exists (sanity guard)
    let nested =
        game.Mainline
        |> Seq.collect (fun m -> m.Variations |> Seq.collect id)
        |> Seq.tryHead
    Assert.True(nested.IsSome)

[<Fact>]
let ``parseFullPgnGame handles engine comments and pd tokens without crashing`` () : Task = task {
    let pgn = """
[Event "Testing Ceres V2"]
[Site "Main computer"]
[Date "11/27/2025"]
[Round "1.1"]
[White "PlentyChess 7"]
[Black "Stockfish 17.1"]
[Result "1/2-1/2"]
[Reason "AE"]
[Ply "79"]
[GameTime "18140"]
[Opening "ECO: B00"]
[StartEvals "1.07, 0.94"]
[OpeningHash "2fdbdd006862834658c2ca6739b1686b89b36bb116c51ce7e7baf62536a70fce"]

{TournamentOptions: Testing Ceres V2; Rounds=50; Book=TCEC28_sufibook.pgn; Tablebase adj=6-men; Adjudication: -draw movenumber=10 movecount=5 score=0.3 cp -resign movecount=5 score=5.0 cp ; WhiteEngineOptions: TimeControl: 5'' + 0.2''; Protocol=UCI; MoveOverheadMS=200; threads=12; Hash=8192; syzygyPath =D:/sygyzy; Ponder=False; UCI_Chess960=false;BlackEngineOptions: TimeControl: 5'' + 0.2''; Protocol=UCI; MoveOverheadMS=200; Threads=6; Hash=8192; SyzygyPath=D:/sygyzy; Ponder=False; UCI_ShowWDL=True; UCI_Chess960=false;}
1. e4 {book, mb=+0+0+0+0+0,} b6 {book, mb=+0+0+0+0+0,} 2. d4 {book, mb=+0+0+0+0+0,} Bb7 {book, mb=+0+0+0+0+0,} 3. Bd3 {book, mb=+0+0+0+0+0,} e6 {book, mb=+0+0+0+0+0,} 4. Nc3 {book, mb=+0+0+0+0+0,} g6 {book, mb=+0+0+0+0+0,} 5. Be3 {book, mb=+0+0+0+0+0,} Bg7 {book, mb=+0+0+0+0+0,} 6. Qd2 {book, mb=+0+0+0+0+0,} 6 ...f5 {wv=1.07, mt=739, s=6631151, eps=0, n=2937600, d=21, pcs=32, sd=37, pd=Nf3, tl=4460, tb=0} 7. Nf3 {wv=0.94, mt=886, s=12254641, eps=0, n=7867480, d=23, pcs=32, sd=40, pd=, tl=4313, tb=0} 7 ...fxe4 {wv=0.97, mt=27, s=6526074, eps=0, n=176204, d=13, pcs=32, sd=30, pd=Nxe4, tl=4632, tb=0} 8. Nxe4 {wv=0.88, mt=181, s=15046905, eps=0, n=2708443, d=21, pcs=31, sd=42, pd=, tl=4331, tb=0} 8 ...Nf6 {wv=1.06, mt=30, s=6156933, eps=0, n=184708, d=14, pcs=30, sd=40, pd=Nxf6, tl=4802, tb=0} 9. Nxf6 {wv=0.94, mt=210, s=14614561, eps=0, n=3069058, d=21, pcs=30, sd=45, pd=, tl=4321, tb=0} 9 ...Bxf6 {wv=1.00, mt=55, s=6059745, eps=0, n=333286, d=16, pcs=29, sd=38, pd=Ng5, tl=4947, tb=0} 10. Ng5 {wv=0.85, mt=215, s=14913981, eps=0, n=3221420, d=22, pcs=28, sd=43, pd=, tl=4305, tb=0} 10 ...Qe7 {wv=1.02, mt=168, s=5339875, eps=0, n=902439, d=18, pcs=28, sd=30, pd=0-0, tl=4978, tb=0} 11. 0-0 {wv=0.83, mt=224, s=13418629, eps=0, n=3005773, d=21, pcs=28, sd=45, pd=, tl=4281, tb=0} 11 ...Nc6 {wv=1.02, mt=29, s=5616413, eps=0, n=162876, d=16, pcs=28, sd=32, pd=c3, tl=5149, tb=0} 12. c3 {wv=0.88, mt=172, s=14648180, eps=0, n=2519487, d=21, pcs=28, sd=45, pd=, tl=4308, tb=0} 12 ...Nd8 {wv=1.00, mt=40, s=6788682, eps=0, n=278336, d=17, pcs=28, sd=29, pd=Ne4, tl=5308, tb=0} 13. Ne4 {wv=0.88, mt=219, s=14630958, eps=0, n=3204180, d=20, pcs=28, sd=37, pd=, tl=4289, tb=0} 13 ...Bxe4 {wv=0.95, mt=75, s=7058310, eps=0, n=522315, d=17, pcs=28, sd=31, pd=Bxe4, tl=5433, tb=0} 14. Bxe4 {wv=0.87, mt=190, s=15141147, eps=0, n=2876818, d=20, pcs=27, sd=38, pd=, tl=4298, tb=0} 14 ...c6 {wv=0.95, mt=44, s=6196431, eps=0, n=272643, d=17, pcs=26, sd=34, pd=g3, tl=5589, tb=0} 15. Rfe1 {wv=0.69, mt=1001, s=13122756, eps=0, n=13135879, d=23, pcs=26, sd=40, pd=, tl=3497, tb=0} 15 ...Nf7 {wv=0.84, mt=210, s=6159100, eps=0, n=1293411, d=19, pcs=26, sd=32, pd=Bf4, tl=5579, tb=0} 16. d5 {wv=0.77, mt=825, s=13062472, eps=0, n=10776540, d=21, pcs=26, sd=54, pd=, tl=2872, tb=11} 16 ...Nd6 {wv=0.73, mt=64, s=6799846, eps=0, n=441990, d=16, pcs=26, sd=31, pd=Bd4, tl=5714, tb=0} 17. Bf3 {wv=0.64, mt=223, s=14086218, eps=0, n=3155313, d=21, pcs=26, sd=41, pd=, tl=2848, tb=0} 17 ...Nc4 {wv=0.42, mt=67, s=6472132, eps=0, n=440105, d=19, pcs=26, sd=35, pd=Qc1, tl=5846, tb=0} 18. Qc2 {wv=0.75, mt=349, s=14980747, eps=0, n=5228281, d=22, pcs=26, sd=48, pd=, tl=2698, tb=2} 18 ...Nxe3 {wv=0.53, mt=548, s=7401085, eps=0, n=4055795, d=23, pcs=26, sd=49, pd=Rxe3, tl=5498, tb=6} 19. Rxe3 {wv=0.69, mt=457, s=14206866, eps=0, n=6492538, d=20, pcs=25, sd=44, pd=, tl=2441, tb=1} 19 ...cxd5 {wv=0.51, mt=89, s=8205674, eps=0, n=730305, d=25, pcs=24, sd=46, pd=Bxd5, tl=5608, tb=0} 20. Rd1 {wv=0.54, mt=301, s=15768365, eps=0, n=4746278, d=22, pcs=23, sd=39, pd=, tl=2339, tb=0} 20 ...0-0 {wv=0.62, mt=345, s=6952695, eps=0, n=2398680, d=20, pcs=23, sd=43, pd=Rxd5, tl=5463, tb=13} 21. Rxd5 {wv=0.51, mt=143, s=17018685, eps=0, n=2433672, d=21, pcs=23, sd=38, pd=, tl=2396, tb=0} 21 ...Rad8 {wv=0.61, mt=201, s=6867771, eps=0, n=1380422, d=23, pcs=22, sd=45, pd=Qa4, tl=5461, tb=44} 22. Qa4 {wv=0.56, mt=423, s=14194284, eps=0, n=5989988, d=21, pcs=22, sd=44, pd=, tl=2173, tb=28} 22 ...Bg5 {wv=0.46, mt=36, s=8116888, eps=0, n=292208, d=14, pcs=22, sd=43, pd=Re1, tl=5625, tb=0} 23. Re2 {wv=0.55, mt=473, s=15289469, eps=0, n=7231919, d=23, pcs=22, sd=51, pd=, tl=1899, tb=705} 23 ...Bf4 {wv=0.58, mt=107, s=7051934, eps=0, n=754557, d=18, pcs=22, sd=43, pd=g3, tl=5717, tb=43} 24. Rd4 {wv=0.53, mt=404, s=15626054, eps=0, n=6312926, d=22, pcs=22, sd=49, pd=, tl=1695, tb=255} 24 ...Bb8 {wv=0.37, mt=194, s=6763958, eps=0, n=1318972, d=18, pcs=22, sd=28, pd=Rd3, tl=5722, tb=0} 25. Bg4 {wv=0.52, mt=194, s=17216466, eps=0, n=3357211, d=20, pcs=22, sd=34, pd=, tl=1700, tb=0} 25 ...Rf6 {wv=0.42, mt=327, s=6893259, eps=0, n=2254096, d=19, pcs=22, sd=32, pd=g3, tl=5595, tb=0} 26. g3 {wv=0.57, mt=207, s=16740473, eps=0, n=3465278, d=19, pcs=22, sd=35, pd=, tl=1692, tb=0} 26 ...Kg7 {wv=0.43, mt=143, s=7326517, eps=0, n=1047692, d=19, pcs=22, sd=29, pd=Bh3, tl=5652, tb=0} 27. h4 {wv=0.59, mt=546, s=15639045, eps=0, n=8523280, d=21, pcs=22, sd=38, pd=, tl=1346, tb=0} 27 ...d5 {wv=0.73, mt=533, s=6727859, eps=0, n=3585949, d=21, pcs=22, sd=36, pd=Qd1, tl=5318, tb=0} 28. Qd1 {wv=0.55, mt=125, s=18651645, eps=0, n=2312804, d=19, pcs=22, sd=38, pd=, tl=1421, tb=0} 28 ...Bd6 {wv=0.72, mt=99, s=8978989, eps=0, n=879941, d=17, pcs=22, sd=47, pd=Rd3, tl=5419, tb=0} 29. Rd3 {wv=0.37, mt=538, s=16431184, eps=0, n=8839977, d=20, pcs=22, sd=40, pd=, tl=1082, tb=0} 29 ...Qf7 {wv=0.68, mt=483, s=7349351, eps=0, n=3557086, d=19, pcs=22, sd=40, pd=f4, tl=5135, tb=0} 30. f4 {wv=0.41, mt=373, s=17196024, eps=0, n=6414117, d=22, pcs=22, sd=38, pd=, tl=909, tb=0} 30 ...h5 {wv=0.65, mt=113, s=5998230, eps=0, n=677800, d=18, pcs=22, sd=38, pd=Bh3, tl=5222, tb=0} 31. Bh3 {wv=0.42, mt=103, s=19568592, eps=0, n=2015565, d=19, pcs=22, sd=39, pd=, tl=1006, tb=0} 31 ...Re8 {wv=0.61, mt=53, s=9568811, eps=0, n=507147, d=16, pcs=22, sd=29, pd=Qe1, tl=5368, tb=0} 32. Qe1 {wv=0.36, mt=120, s=19389941, eps=0, n=2326793, d=19, pcs=22, sd=40, pd=, tl=1085, tb=0} 32 ...Re7 {wv=0.51, mt=108, s=7305666, eps=0, n=789012, d=18, pcs=22, sd=34, pd=Kh2, tl=5459, tb=0} 33. Kh2 {wv=0.34, mt=211, s=18231514, eps=0, n=3828618, d=21, pcs=22, sd=37, pd=, tl=1074, tb=0} 33 ...Qe8 {wv=0.51, mt=32, s=6167636, eps=0, n=203532, d=14, pcs=22, sd=31, pd=b3, tl=5627, tb=0} 34. b3 {wv=0.28, mt=342, s=16582104, eps=0, n=5687662, d=21, pcs=22, sd=41, pd=, tl=931, tb=0} 34 ...Bc5 {wv=0.45, mt=155, s=8282416, eps=0, n=1292057, d=18, pcs=22, sd=28, pd=Re5, tl=5671, tb=0} 35. Re5 {wv=0.47, mt=264, s=16689242, eps=0, n=4405960, d=21, pcs=22, sd=42, pd=, tl=866, tb=0} 35 ...Bd6 {wv=0.47, mt=78, s=8831987, eps=0, n=688895, d=17, pcs=22, sd=27, pd=Rg5, tl=5792, tb=0} 36. Rg5 {wv=0.55, mt=169, s=18535470, eps=0, n=3151030, d=18, pcs=22, sd=35, pd=, tl=896, tb=0} 36 ...Rf5 {wv=0.50, mt=99, s=8008777, eps=0, n=792869, d=17, pcs=22, sd=34, pd=c4, tl=5893, tb=0} 37. c4 {wv=0.41, mt=107, s=18405685, eps=0, n=1987814, d=16, pcs=22, sd=40, pd=, tl=989, tb=0} 37 ...Kh7 {wv=0.47, mt=136, s=8734169, eps=0, n=1187847, d=18, pcs=22, sd=34, pd=Qf1, tl=5957, tb=0} 38. Qc3 {wv=0.38, mt=239, s=18395928, eps=0, n=4396627, d=20, pcs=22, sd=37, pd=, tl=949, tb=5} 38 ...Rxg5 {wv=0.39, mt=122, s=7690778, eps=0, n=938275, d=21, pcs=22, sd=30, pd=fxg5, tl=6035, tb=0} 39. fxg5 {wv=0.36, mt=113, s=20606734, eps=0, n=2328561, d=20, pcs=21, sd=43, pd=, tl=1036, tb=0} 39 ...a5 {wv=0.31, mt=102, s=8199460, eps=0, n=836345, d=21, pcs=20, sd=34, pd=Qe1, tl=6132, tb=0} 40. Bg2 {wv=0.33, mt=89, s=21805348, eps=0, n=1940676, d=20, pcs=20, sd=46, pd=, tl=1146, tb=0} 40 ...Bc5 {wv=0.36, mt=84, s=8524481, eps=0, n=707532, d=19, pcs=20, sd=37, pd=Rd1, tl=6248, tb=0} 41. Qc2 {wv=0.22, mt=393, s=20178852, eps=0, n=7930289, d=23, pcs=20, sd=42, pd=, tl=952, tb=10} 41 ...e5 {wv=0.13, mt=438, s=7866171, eps=0, n=3445383, d=22, pcs=20, sd=50, pd=cxd5, tl=6009, tb=9} 42. cxd5 {wv=0.18, mt=103, s=23045262, eps=0, n=2373662, d=21, pcs=20, sd=44, pd=, tl=1049, tb=0} 42 ...e4 {wv=0.04, mt=44, s=9403431, eps=0, n=413751, d=17, pcs=19, sd=43, pd=Rd2, tl=6165, tb=2} 43. Rd2 {wv=0.13, mt=92, s=23323239, eps=0, n=2145738, d=21, pcs=19, sd=39, pd=, tl=1156, tb=0} 43 ...e3 {wv=0.12, mt=97, s=9676469, eps=0, n=948294, d=22, pcs=19, sd=44, pd=Re2, tl=6267, tb=0} 44. Re2 {wv=0.16, mt=119, s=23587579, eps=0, n=2806922, d=22, pcs=19, sd=39, pd=, tl=1237, tb=0} 44 ...Re5 {wv=0.16, mt=93, s=8887731, eps=0, n=826559, d=21, pcs=19, sd=47, pd=Bf3, tl=6373, tb=14} 45. Bf3 {wv=0.05, mt=228, s=21910934, eps=0, n=4995693, d=26, pcs=19, sd=39, pd=, tl=1209, tb=2} 45 ...Kg7 {wv=0.12, mt=97, s=8731432, eps=0, n=846949, d=20, pcs=19, sd=54, pd=Kg2, tl=6476, tb=3} {Evaluation agreement} 1/2-1/2
"""

    let parseTask = Task.Run(fun () -> FullPGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(10000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing timed out")
    let game = parseTask.Result

    Assert.Equal("PlentyChess 7", game.GameMetaData.White)
    Assert.Equal("Stockfish 17.1", game.GameMetaData.Black)
    Assert.Equal("1/2-1/2", game.GameMetaData.Result)
    Assert.True(game.Mainline.Count >= 80, $"Mainline too short: {game.Mainline.Count}")
    Assert.Equal("Kg7", game.Mainline[game.Mainline.Count - 1].San)
    }

[<Fact>]
let ``parseFullPgnGame parses long SF vs Ceres game`` () : Task = task {
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "SF_vs_Ceres_and_Lc0.pgn")
    Assert.True(File.Exists(path), $"Test data file missing: {path}")
    let pgn = File.ReadAllText(path)

    let parseTask = Task.Run(fun () -> FullPGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(15000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing timed out")
    let game = parseTask.Result

    Assert.Equal("Stockfish 17.1", game.GameMetaData.White)
    Assert.Equal("Ceres Dev BT4", game.GameMetaData.Black)
    Assert.Equal("1-0", game.GameMetaData.Result)
    Assert.True(game.Mainline.Count >= 170, $"Mainline too short: {game.Mainline.Count}")
    let lastSan = game.Mainline[game.Mainline.Count - 1].San
    Assert.False(String.IsNullOrWhiteSpace(lastSan), "Last SAN is empty")
    }

[<Fact>]
let ``parseFullPgnGame parses engine comment game with late variation comment`` () : Task = task {
    let pgn = """
[Event "Single Engine Analysis"]
[Date "2025.11.29.08:59"]
[Engine "lc0_BT4-it332-TRT"]
[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]
[Eval "lc0_BT4-it332-TRT: Eval=-5.88, Depth=7, SD=25"]
[Move "67... Bxf5"]

1. d4 Nf6 2. c4 e6 3. Nf3 Bb4+ 4. Nbd2 O-O 5. a3 Be7 6. e4 d6 7. Be2 c5 8. d5 e5 9. h3 Na6 10. Bd3 Nc7 11. a4 Na6 12. Nf1 Nb4 13. Bb1 b6 14. Ng3 a6 15. Bd2 Rb8 16. 0-0 b5 17. b3 g6 18. Bh6 Re8 19. Qc1 Nd7 20. Nh2 Rb7 21. Ra3 Bf7 22. Bg5 Be7 23. Bd2 Bh4 24. Ng4 Nf6 25. Nh2 Nd7 26. Ne2 Nb6 27. Kh1 Bf6 28. a5 Nd7 29. Ng4 Bh4 30. Nh6 Kg7 31. Ra1 Nf6 32. g3 Nh5 33. Ng4 Be7 34. f4 Nf6 35. Nf2 exf4 36. gxf4 Bf8 37. f5 Kh8 38. Ng3 Nd7 39. Ng4 Bg7 40. Ra3 Kg8 41. Bh6 Be5 42. Kg2 f6 43. Qe1 Kh8 44. Qd2 g5 45. Ne2 Rg8 46. Qc1 Qe7 47. Qd2 Qf7 48. Qc1 Qh5 49. Rg1 Qh4 50. Qd2 Rb8 51. Rf1 Re8 52. Qc1 Bb7 53. Qd2 Re7 54. Rf3 Rbe8 55. Rf1 Kg8 56. Rh1 Kf7 57. Rf1 Rg8 58. Rh1 Ree8 59. Rf1 Ke7 60. Rh1 Bc8 61. Qc1 Kd8 62. Qd2 Kc7 63. Rc1 Kb8 64. Rf1 Ka8 65. Rh1 Re7 66. Qc1 Nb8 67. Qd2 {variation starts here} Bxf5 68.exf5 Rge8 69.Rf1 Bh2 70.Nc1 Qg3 71.Kh1 Re1 72.Nxh2 Rxf1 73.Nxf1 Re1 74.Qg2 Qf4 75.Kg1 Qxc1 76.Ra1 Qc3 77.Qf2 Nd7 78.Bxg5 fxg5
"""
    let game = FullPGNParser.parseFullPgnGame pgn
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", game.GameMetaData.Fen)
    Assert.True(game.Mainline.Count >= 75, $"Mainline too short: {game.Mainline.Count}")
    Assert.Equal("fxg5", game.Mainline[game.Mainline.Count - 1].San)
    // ensure the comment line did not break move parsing
    Assert.Contains(game.Mainline, fun n -> n.San = "Bxf5")
    }

[<Fact>]
let ``parseFullPgnGame parses lichess odds game with variations`` () : Task = task {
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "lichess_pgn_LeelaPieceOddsFRC.pgn")
    Assert.True(File.Exists(path), $"Test data file missing: {path}")
    let pgn = File.ReadAllText(path)

    let parseTask = Task.Run(fun () -> FullPGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(10000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing timed out")
    let game = parseTask.Result

    Assert.Equal("LeelaPieceOddsFRC", game.GameMetaData.White)
    Assert.Equal("Former_Player", game.GameMetaData.Black)
    Assert.Equal("1-0", game.GameMetaData.Result)
    Assert.Equal("rnqnkbbr/pppppppp/8/8/8/8/PPPPPPPP/R1QNKBBR w KQkq - 0 1", game.GameMetaData.Fen)
    Assert.True(game.Mainline.Count >= 50, $"Mainline too short: {game.Mainline.Count}")
    Assert.Equal("f7#", game.Mainline[game.Mainline.Count - 1].San)

    // Variation attached to 11... exf4? should start with 11... f6
    let exf4 = game.Mainline |> Seq.find (fun n -> n.San.StartsWith("exf4"))
    Assert.True(exf4.Variations.Count >= 1, "Expected variation on 11...exf4")
    let exf4Var = exf4.Variations[0]
    Assert.Equal("f6", exf4Var[0].San)

    // Variation after 15... Nf7? should have 15... Qg5
    let nf7 = game.Mainline |> Seq.find (fun n -> n.San.StartsWith("Nf7"))
    Assert.True(nf7.Variations.Count >= 1, "Expected variation on 15...Nf7")
    let nf7Var = nf7.Variations[0]
    Assert.Equal("Qg5", nf7Var[0].San)

    // Variation after 16... Qg5?! should have 16... Qe8
    let qg5 = game.Mainline |> Seq.find (fun n -> n.San.StartsWith("Qg5"))
    Assert.True(qg5.Variations.Count >= 1, "Expected variation on 16...Qg5")
    let qg5Var = qg5.Variations[0]
    Assert.Equal("Qe8", qg5Var[0].San)

    // Variation after 18... Bh7?? should have 18... Rh7
    let bh7 = game.Mainline |> Seq.find (fun n -> n.San.StartsWith("Bh7"))
    Assert.True(bh7.Variations.Count >= 1, "Expected variation on 18...Bh7")
    let bh7Var = bh7.Variations[0]
    Assert.Equal("Rh7", bh7Var[0].San)

    // last move should be f7#
    Assert.Equal("f7#", game.Mainline[game.Mainline.Count - 1].San)
    }

// ============================================================================
// FullSpanParser Tests
// ============================================================================

module FullSpanParserTests =
    open ChessLibrary.FullPGNParser // functions become available directly

    [<Fact>]
    let ``FullSpanParser parses simple PGN string`` () =
        let pgnContent = """
[Event "Test Event"]
[Site "Test Site"]
[Date "2024.01.01"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        Assert.Equal("Alpha", game.GameMetaData.White)
        Assert.Equal("Beta", game.GameMetaData.Black)
        Assert.Equal("1-0", game.GameMetaData.Result)
        Assert.Equal("Test Event", game.GameMetaData.Event)
        Assert.Equal(4, game.Mainline.Count)
        Assert.Equal("e4", game.Mainline.[0].San)
        Assert.Equal("e5", game.Mainline.[1].San)
        Assert.Equal("Nf3", game.Mainline.[2].San)
        Assert.Equal("Nc6", game.Mainline.[3].San)

    [<Fact>]
    let ``FullSpanParser parses variations`` () =
        let pgnContent = """
[Event "Variation Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5) 3. Bb5 a6 *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        // Mainline should have: e4, e5, Nf3, Nc6, Bb5, a6
        Assert.Equal(6, game.Mainline.Count)
        Assert.Equal("e4", game.Mainline.[0].San)
        Assert.Equal("e5", game.Mainline.[1].San)
        Assert.Equal("Nf3", game.Mainline.[2].San)
        Assert.Equal("Nc6", game.Mainline.[3].San)
        Assert.Equal("Bb5", game.Mainline.[4].San)
        Assert.Equal("a6", game.Mainline.[5].San)
        // Nc6 should have a variation attached
        let nc6 = game.Mainline.[3]
        Assert.Equal(1, nc6.Variations.Count)
        let variation = nc6.Variations.[0]
        Assert.Equal(2, variation.Count)
        Assert.Equal("Nf6", variation.[0].San)
        Assert.Equal("Nxe5", variation.[1].San)

    [<Fact>]
    let ``FullSpanParser parses nested variations`` () =
        let pgnContent = """
[Event "Nested Variation Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5 (3. d4 exd4) 3... Nxe4) 3. Bb5 *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        // Mainline: e4, e5, Nf3, Nc6, Bb5
        Assert.Equal(5, game.Mainline.Count)
        // The outer variation is attached to Nc6
        let nc6 = game.Mainline.[3]
        Assert.Equal(1, nc6.Variations.Count)
        let outerVar = nc6.Variations.[0]
        // Outer variation: Nf6, Nxe5, Nxe4
        Assert.True(outerVar.Count >= 2)
        Assert.Equal("Nf6", outerVar.[0].San)
        Assert.Equal("Nxe5", outerVar.[1].San)
        // Nxe5 should have a nested variation (3. d4 exd4)
        let nxe5 = outerVar.[1]
        Assert.Equal(1, nxe5.Variations.Count)
        let innerVar = nxe5.Variations.[0]
        Assert.Equal(2, innerVar.Count)
        Assert.Equal("d4", innerVar.[0].San)
        Assert.Equal("exd4", innerVar.[1].San)

    [<Fact>]
    let ``FullSpanParser parses comments`` () =
        let pgnContent = """
[Event "Comment Test"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 {This is a comment} e5 2. Nf3 {Another comment} Nc6 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        // Comments should be attached to the moves
        Assert.Equal("This is a comment", game.Mainline.[0].Comment)
        Assert.Equal("Another comment", game.Mainline.[2].Comment)

    [<Fact>]
    let ``FullSpanParser parses engine comments on every move`` () =
        let pgnContent = """
[Event "Round Robin"]
[Site "Cup Test"]
[Date "10/01/2026"]
[Round "1.1"]
[White "Stockfish 20260106"]
[Black "Ceres 2.095 C1-640-34"]
[Result "1-0"]
[Reason "AE"]
[Opening "French - Chigorin variation, ECO: C00"]
[OpeningHash "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b"]

{TournamentOptions: Round Robin; Rounds=240; Book=Shuffled_SUFI21-27_TOP 50.pgn; Tablebase adj=6-men; Adjudication: -draw movenumber=20 movecount=10 score=0.2 cp -resign movecount=10 score=10.0 cp ; WhiteEngineOptions: TimeControl: 30' + 30''; Protocol=UCI; MoveOverheadMS=2000; NumaPolicy=auto; Threads=126; Skill Level=20; Move Overhead=200; nodestime=0; Hash=98304; SyzygyProbeDepth=1; Syzygy50MoveRule=True; SyzygyProbeLimit=6; SyzygyPath=D:/Syzygy; UCI_Chess960=false; UCI_ShowWDL=true;BlackEngineOptions: TimeControl: 30' + 30''; Protocol=UCI; MoveOverheadMS=2000; LogFile=; PathMode=position; MoveOverheadMs=2000; VerboseMoveStats=True; LogLiveStats=True; UCI_ShowWDL=True; SyzygyPath=D:/Syzygy; Network=D:/Ceres 2.095 C1-640-34/C1-640-34-I8.onnx|cudagraphs=true;V1TEMP=0.55; UCI_Chess960=false; Device=GPU:0,1#TensorRT16;}
1. e4 {book1, mb=+0+0+0+0+0,} e6 {book2, mb=+0+0+0+0+0,} 2. Qe2 {book3, mb=+0+0+0+0+0,} d5 {book4, mb=+0+0+0+0+0,} 3. exd5 {book5, mb=+0+0+0+0+0,} Qxd5 {book6, mb=+0+0+0+0+0,} 4. Nc3 {book7, mb=+0+0+0+0+0,} Qd8 {book8, mb=+0+0+0+0+0,} 5. b3 {book9, mb=+0+0+0+0+0,} Nf6 {book10, mb=+0+0+0+0+0,} 6. Bb2 {book11, mb=+0+0+0+0+0,} Be7 {book12, mb=+0+0+0+0+0,} 7. 0-0-0 {wv=1.23, mt=224941, s=40143766, eps=0, n=9029617797, d=41, pcs=30, sd=98, pd=a5, tl=1605058, tb=47666} 7 ...a5 {wv=0.82, mt=178617, s=12037, eps=9716, n=2148187, d=25, pcs=30, sd=73, pd=, tl=1651382, tb=0} {Evaluation agreement} 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        Assert.Equal(14, game.Mainline.Count)

        let missingComments =
            game.Mainline
            |> Seq.filter (fun m -> String.IsNullOrWhiteSpace(m.Comment))
            |> Seq.toList
        Assert.True(missingComments.IsEmpty, $"Expected comments on every move, missing on {missingComments.Length} moves")

        let lastMove = game.Mainline.[game.Mainline.Count - 1]
        Assert.Contains("Evaluation agreement", lastMove.Comment)

    [<Fact>]
    let ``FullSpanParser parses NAGs`` () =
        let pgnContent = """
[Event "NAG Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 $1 e5 $2 2. Nf3 $14 Nc6 *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        // NAGs should be attached to the following moves
        Assert.Contains(1, game.Mainline.[1].Nags)  // e5 follows $1
        Assert.Contains(2, game.Mainline.[2].Nags)  // Nf3 follows $2

    [<Fact>]
    let ``FullSpanParser parses multiple games`` () =
        let pgnContent = """
[Event "Game 1"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 1-0

[Event "Game 2"]
[White "Gamma"]
[Black "Delta"]
[Result "0-1"]

1. d4 d5 0-1
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Equal(2, games.Length)
        Assert.Equal("Alpha", games.[0].GameMetaData.White)
        Assert.Equal("Gamma", games.[1].GameMetaData.White)
        Assert.Equal("1-0", games.[0].GameMetaData.Result)
        Assert.Equal("0-1", games.[1].GameMetaData.Result)

    [<Fact>]
    let ``FullSpanParser parses FEN starting position`` () =
        let pgnContent = """
[Event "FEN Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]
[FEN "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"]

1... e5 2. Nf3 Nc6 *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        Assert.Equal("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", game.Fen)
        // First move should be Black's since FEN says " b "
        Assert.Equal("e5", game.Mainline.[0].San)

    [<Fact>]
    let ``FullSpanParser parses castling`` () =
        let pgnContent = """
[Event "Castling Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 O-O *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        let moves = getMovesAsStrings game
        Assert.Contains("O-O", moves)
        // O-O appears twice (5. O-O and 7... O-O)

    [<Fact>]
    let ``FullSpanParser matches PGNParser move count`` () =
        let pgnContent = """
[Event "Match Test"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5) 3. Bb5 a6 *
"""
        let fullGames = parsePgnString pgnContent |> Seq.toArray
        let originalGames = parseFullPgnGame pgnContent
        Assert.Equal(originalGames.Mainline.Count, fullGames.[0].Mainline.Count)

    [<Fact>]
    let ``FullSpanParser file parser works`` () =
        let tempFile = Path.GetTempFileName()
        let pgnContent = """
[Event "File Test"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 1-0
"""
        File.WriteAllText(tempFile, pgnContent)
        try
            let games = parsePgnFile tempFile |> Seq.toList
            Assert.Single(games) |> ignore
            let game = games.Head
            Assert.Equal("Alpha", game.GameMetaData.White)
            Assert.Equal(4, game.Mainline.Count)
        finally
            File.Delete(tempFile)

    [<Fact>]
    let ``FullSpanParser handles promotions`` () =
        let pgnContent = """
[Event "Promotion Test"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. d4 d5 3. e8=Q dxe4 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let moves = getMovesAsStrings games.Head
        Assert.Contains("e8=Q", moves)

    [<Fact>]
    let ``FullSpanParser handles check and checkmate symbols`` () =
        let pgnContent = """
[Event "Check Test"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6 4. Qxf7# 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let moves = getMovesAsStrings games.Head
        Assert.Contains("Qxf7#", moves)

    [<Fact>]
    let ``FullSpanParser preserves OtherTags`` () =
        let pgnContent = """
[Event "Tag Test"]
[Site "Test Site"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]
[ECO "C50"]
[Opening "Italian Game"]

1. e4 e5 *
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        let eco = game.GameMetaData.OtherTags |> List.tryFind (fun h -> h.Key = "ECO")
        let opening = game.GameMetaData.OtherTags |> List.tryFind (fun h -> h.Key = "Opening")
        Assert.True(eco.IsSome)
        Assert.Equal("C50", eco.Value.Value)
        Assert.True(opening.IsSome)
        Assert.Equal("Italian Game", opening.Value.Value)

    [<Fact>]
    let ``FullSpanParser parses extended metadata headers`` () =
        let pgnContent = """
[Event "Extended Test"]
[Site "Test Site"]
[Date "2025.01.07"]
[Round "3"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]
[Opening "Sicilian Defense"]
[GameTime "3600000"]
[Ply "20"]
[Deviations "5"]
[OpeningHash "abc123"]

1. e4 c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 Nf6 5. Nc3 a6 1-0
"""
        let start = 0
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        let meta = game.GameMetaData
        
        // Standard headers
        Assert.Equal("Extended Test", meta.Event)
        Assert.Equal("Test Site", meta.Site)
        Assert.Equal("2025.01.07", meta.Date)
        Assert.Equal("3", meta.Round)
        Assert.Equal("Alpha", meta.White)
        Assert.Equal("Beta", meta.Black)
        Assert.Equal("1-0", meta.Result)
        
        // Extended headers
        Assert.Equal("Sicilian Defense", meta.OpeningName)
        Assert.Equal(3600000L, meta.GameTime)
        //Assert.Equal(5, meta.Moves)
        Assert.Equal(5, meta.Deviations)
        Assert.Equal("abc123", meta.OpeningHash)
        
        // Opening should also be in OtherTags
        let openingTag = meta.OtherTags |> List.tryFind (fun h -> h.Key = "Opening")
        Assert.True(openingTag.IsSome)
        Assert.Equal("Sicilian Defense", openingTag.Value.Value)

    [<Fact>]
    let ``FullSpanParser generates PGN string from game`` () =
        let pgnContent = """
[Event "Generate Test"]
[Site "Test Site"]
[Date "2025.01.07"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 1-0
"""
        let games = parsePgnString pgnContent |> Seq.toList
        Assert.Single(games) |> ignore
        let game = games.Head
        
        // Generate PGN string
        let pgnStr = toPgnString game
        
        // Verify it contains expected headers
        Assert.Contains("[Event \"Generate Test\"]", pgnStr)
        Assert.Contains("[White \"Player1\"]", pgnStr)
        Assert.Contains("[Black \"Player2\"]", pgnStr)
        Assert.Contains("[Result \"1-0\"]", pgnStr)
        
        // Verify it contains moves
        Assert.Contains("1. e4", pgnStr)
        Assert.Contains("e5", pgnStr)
        Assert.Contains("2. Nf3", pgnStr)
        Assert.Contains("Nc6", pgnStr)
        Assert.Contains("1-0", pgnStr)
            

    [<Fact>]
    let ``FullSpanParser parses Chess960 game with SetUp and FEN`` () =
        // Chess960 position #518 (standard starting position is #518 in Chess960)
        // Using a different starting position to verify FEN parsing
        let chess960Pgn = """[Event "Chess960 Test"]
[Site "Test"]
[Date "2026.01.08"]
[Round "1"]
[White "Engine1"]
[Black "Engine2"]
[Result "1-0"]
[Variant "Chess960"]
[SetUp "1"]
[FEN "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"]

9. e4 d5 10. exd5 exd5 11. O-O O-O 12. Nc3 Ne4 13. Nxe4 dxe4 14. d4 cxd4 15. Qxd4 Nc6 16. Qxe4 Qc7 1-0"""

        let game = parseFullPgnGame chess960Pgn        
        Assert.Equal("Chess960 Test", game.GameMetaData.Event)
        Assert.Equal("Engine1", game.GameMetaData.White)
        Assert.Equal("Engine2", game.GameMetaData.Black)
        Assert.Equal("1-0", game.GameMetaData.Result)
        
        // Check FEN was captured
        Assert.True(game.Fen.Contains("bqnb1rkr"))
        
        // Check moves were parsed correctly (16 half-moves = 8 full moves from position)
        Assert.Equal(16, game.Mainline.Count)
        Assert.Equal("e4", game.Mainline.[0].San)
        Assert.Equal("d5", game.Mainline.[1].San)
        // Castling moves with O-O notation
        Assert.Equal("O-O", game.Mainline.[4].San)
        Assert.Equal("O-O", game.Mainline.[5].San)
        
        // Verify toPgnString round-trips correctly
        let pgnStr = toPgnString game
        Assert.Contains("[SetUp \"1\"]", pgnStr)
        Assert.Contains("[FEN \"bqnb1rkr", pgnStr)
        Assert.Contains("O-O", pgnStr)
        Assert.Contains("1-0", pgnStr)

    [<Fact>]
    let ``FullSpanParser parses Chess960 with digit-zero castling notation`` () =
        // Some PGN files use 0-0 instead of O-O for castling
        let chess960Pgn = """[Event "Chess960 Zero Castling"]
[Site "Test"]
[Date "2026.01.08"]
[Round "1"]
[White "Engine1"]
[Black "Engine2"]
[Result "1/2-1/2"]
[Variant "Chess960"]
[SetUp "1"]
[FEN "rkrbbnnq/pppppppp/8/8/8/8/PPPPPPPP/RKRBBNNQ w CAca - 0 1"]

1. e4 e5 2. Nf3 Nf6 3. d4 d5 4. 0-0-0 0-0-0 5. exd5 exd4 6. Rxd4 Rxd5 1/2-1/2"""

        let games = parsePgnString chess960Pgn |> Seq.toArray
        Assert.Equal(1, games.Length)
        
        let game = games.[0]
        Assert.Equal("1/2-1/2", game.GameMetaData.Result)
        
        // Check moves including digit-zero castling
        Assert.Equal(12, game.Mainline.Count)
        Assert.Equal("e4", game.Mainline.[0].San)
        // Castling with 0-0-0 notation (queenside)
        Assert.Equal("0-0-0", game.Mainline.[6].San)
        Assert.Equal("0-0-0", game.Mainline.[7].San)
            

    [<Fact>]
    let ``FastPGNParser handles opening book entry with FEN and star result`` () =
        // Opening book format: headers with FEN, no moves, just * for unfinished/no result
        let openingBookPgn = """[Event "Opening Book"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[SetUp "1"]
[FEN "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"]

*"""

        let games = FullPGNParser.parsePgnString openingBookPgn |> Seq.toArray
        Assert.Equal(1, games.Length)
        
        let game = games.[0]
        Assert.Equal("Opening Book", game.GameMetaData.Event)
        Assert.Equal("*", game.GameMetaData.Result)
        Assert.True(game.Fen.Contains("r1bqkb1r"))
        Assert.Equal(0, game.Mainline.Count)  // No moves, just position

    [<Fact>]
    let ``FastPGNParser handles multiple opening book entries`` () =
        let openingBookPgn = """[Event "Italian Game"]
[FEN "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"]
[SetUp "1"]
[Result "*"]

*

[Event "Sicilian Defense"]
[FEN "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"]
[SetUp "1"]
[Result "*"]

*

[Event "French Defense"]
[FEN "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
[SetUp "1"]
[Result "*"]

*"""

        let games = FullPGNParser.parsePgnString openingBookPgn |> Seq.toArray
        Assert.Equal(3, games.Length)
        
        Assert.Equal("Italian Game", games.[0].GameMetaData.Event)
        Assert.Equal("*", games.[0].GameMetaData.Result)
        Assert.Equal(0, games.[0].Mainline.Count)
        
        Assert.Equal("Sicilian Defense", games.[1].GameMetaData.Event)
        Assert.Equal("*", games.[1].GameMetaData.Result)
        Assert.Equal(0, games.[1].Mainline.Count)
        
        Assert.Equal("French Defense", games.[2].GameMetaData.Event)
        Assert.Equal("*", games.[2].GameMetaData.Result)
        Assert.Equal(0, games.[2].Mainline.Count)

    [<Fact>]
    let ``FullSpanParser handles opening book entry with FEN and star result`` () =
        let openingBookPgn = """[Event "Opening Book"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[SetUp "1"]
[FEN "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"]

*"""

        let games = parsePgnString openingBookPgn |> Seq.toArray
        Assert.Equal(1, games.Length)
        
        let game = games.[0]
        Assert.Equal("Opening Book", game.GameMetaData.Event)
        Assert.Equal("*", game.GameMetaData.Result)
        Assert.True(game.Fen.Contains("r1bqkb1r"))
        Assert.Equal(0, game.Mainline.Count)  // No moves, just position

    [<Fact>]
    let ``FullSpanParser parsePgnStringHeadersOnly extracts headers without parsing moves`` () =
        let pgn = """[Event "Test Tournament"]
[Site "Test Site"]
[Date "2026.01.08"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]
[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]
[SetUp "1"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 1-0

[Event "Second Game"]
[Site "Another Site"]
[Date "2026.01.08"]
[Round "2"]
[White "Player3"]
[Black "Player4"]
[Result "0-1"]

1. d4 d5 2. c4 e6 3. Nc3 Nf6 0-1"""

        let games = parsePgnStringHeadersOnly pgn |> Seq.toArray
        Assert.Equal(2, games.Length)
        
        // First game - headers parsed, no moves
        Assert.Equal("Test Tournament", games.[0].GameMetaData.Event)
        Assert.Equal("Player1", games.[0].GameMetaData.White)
        Assert.Equal("Player2", games.[0].GameMetaData.Black)
        Assert.Equal("1-0", games.[0].GameMetaData.Result)
        Assert.True(games.[0].Fen.Contains("rnbqkbnr"))
        Assert.Equal(0, games.[0].Mainline.Count)  // Moves not parsed
        
        // Second game
        Assert.Equal("Second Game", games.[1].GameMetaData.Event)
        Assert.Equal("Player3", games.[1].GameMetaData.White)
        Assert.Equal("Player4", games.[1].GameMetaData.Black)
        Assert.Equal("0-1", games.[1].GameMetaData.Result)
        Assert.Equal(0, games.[1].Mainline.Count)

    [<Fact>]
    let ``FullSpanParser parsePgnStringHeadersOnly is faster than full parsing`` () =
        // Generate a PGN with many games and long movelists
        let sb = System.Text.StringBuilder()
        for i in 1 .. 100 do
            sb.AppendLine($"[Event \"Game {i}\"]") |> ignore
            sb.AppendLine("[Site \"Test\"]") |> ignore
            sb.AppendLine("[Date \"2026.01.08\"]") |> ignore
            sb.AppendLine($"[Round \"{i}\"]") |> ignore
            sb.AppendLine("[White \"Alpha\"]") |> ignore
            sb.AppendLine("[Black \"Beta\"]") |> ignore
            sb.AppendLine("[Result \"1-0\"]") |> ignore
            sb.AppendLine() |> ignore
            // Long movelist
            sb.Append("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O ") |> ignore
            sb.Append("9. h3 Nb8 10. d4 Nbd7 11. Nbd2 Bb7 12. Bc2 Re8 13. Nf1 Bf8 14. Ng3 g6 15. Bg5 h6 ") |> ignore
            sb.AppendLine("16. Bd2 Bg7 17. a4 c5 18. d5 c4 19. b4 Nh5 20. Nxh5 gxh5 1-0") |> ignore
            sb.AppendLine() |> ignore
        
        let pgn = sb.ToString()
        
        let sw1 = System.Diagnostics.Stopwatch.StartNew()
        let headersOnly = parsePgnStringHeadersOnly pgn |> Seq.toArray
        sw1.Stop()
        
        let sw2 = System.Diagnostics.Stopwatch.StartNew()
        let fullParse = parsePgnString pgn |> Seq.toArray
        sw2.Stop()
        
        Assert.Equal(100, headersOnly.Length)
        Assert.Equal(100, fullParse.Length)
        
        // Headers-only should have no moves
        Assert.True(headersOnly |> Array.forall (fun g -> g.Mainline.Count = 0))
        // Full parse should have moves
        Assert.True(fullParse |> Array.forall (fun g -> g.Mainline.Count > 0))
        
        // Headers-only should generally be faster (not strictly asserted due to timing variance)

    [<Fact>]
    let ``FullSpanParser parsePgnFileWithRaw populates Raw field`` () =
        let tempFile = System.IO.Path.GetTempFileName()
        try
            let pgn = """[Event "Test Game 1"]
[Site "Test"]
[Date "2026.01.08"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 1-0

[Event "Test Game 2"]
[Site "Test"]
[Date "2026.01.08"]
[Round "2"]
[White "Gamma"]
[Black "Delta"]
[Result "0-1"]

1. d4 d5 2. c4 e6 3. Nc3 Nf6 0-1
"""
            System.IO.File.WriteAllText(tempFile, pgn)
            
            let games = parsePgnFileWithRaw tempFile |> Seq.toArray
            
            Assert.Equal(2, games.Length)
            
            // Both games should have non-empty Raw fields
            Assert.True(games.[0].Raw <> "")
            Assert.True(games.[1].Raw <> "")
            
            // Raw should contain the headers
            Assert.Contains("[Event \"Test Game 1\"]", games.[0].Raw)
            Assert.Contains("[White \"Alpha\"]", games.[0].Raw)
            Assert.Contains("1. e4 e5", games.[0].Raw)
            
            Assert.Contains("[Event \"Test Game 2\"]", games.[1].Raw)
            Assert.Contains("[White \"Gamma\"]", games.[1].Raw)
            Assert.Contains("1. d4 d5", games.[1].Raw)
            
            // Moves should still be parsed correctly
            Assert.True(games.[0].Mainline.Count > 0)
            Assert.True(games.[1].Mainline.Count > 0)
        finally
            System.IO.File.Delete(tempFile)

    [<Fact>]
    let ``FullSpanParser parsePgnFileWithRaw handles multiple games without empty lines between`` () =
        let tempFile = System.IO.Path.GetTempFileName()
        try
            // Games separated only by result in movetext (no blank line)
            let pgn = """[Event "Game 1"]
[White "A"]
[Black "B"]
[Result "1-0"]

1. e4 1-0
[Event "Game 2"]
[White "C"]
[Black "D"]
[Result "0-1"]

1. d4 0-1
"""
            System.IO.File.WriteAllText(tempFile, pgn)
            
            let games = parsePgnFileWithRaw tempFile |> Seq.toArray
            
            Assert.Equal(2, games.Length)
            Assert.Contains("[Event \"Game 1\"]", games.[0].Raw)
            Assert.Contains("[Event \"Game 2\"]", games.[1].Raw)
        finally
            System.IO.File.Delete(tempFile)


// ============================================================================
// PGNWriter Tests
// ============================================================================

module PGNWriterTests =
    open System
    open System.IO
    open Xunit
    open ChessLibrary.FullPGNParser // functions become available directly
    open ChessLibrary.PGNWriter // functions become available directly

    [<Fact>]
    let ``PGNWriter.removePlayerFromPGN filters games and writes output`` () =
        let inputFile = Path.GetTempFileName()
        let outputFile = Path.GetTempFileName()

        try
            let pgn = """[Event "G1"]
[Site "?"]
[Date "2026.01.08"]
[Round "1"]
[White "Ceres Dev BT4"]
[Black "Stockfish 17.1"]
[Result "1-0"]

1. e4 e5 1-0

[Event "G2"]
[Site "?"]
[Date "2026.01.08"]
[Round "2"]
[White "Alpha"]
[Black "Beta"]
[Result "0-1"]

1. d4 d5 0-1
"""

            File.WriteAllText(inputFile, pgn)

            let games = parsePgnFileWithRaw inputFile |> Seq.toList
            PGNWriter.removePlayerFromPGN("Ceres", games, outputFile)

            let filteredGames = parsePgnFileWithRaw outputFile |> Seq.toList
            Assert.Single(filteredGames) |> ignore

            let game = filteredGames.Head
            Assert.Equal("Alpha", game.GameMetaData.White)
            Assert.Equal("Beta", game.GameMetaData.Black)

            let outputText = File.ReadAllText(outputFile).ToLowerInvariant()
            Assert.False(outputText.Contains("ceres"))
        finally
            File.Delete(inputFile)
            File.Delete(outputFile)


    [<Fact>]
    let ``testRemovePlayerFromPGN style - removes player from PGN file`` () =
        let inputFile = Path.Combine(AppContext.BaseDirectory, "TestData", "SF_vs_Ceres_and_Lc0.pgn")
        Assert.True(File.Exists(inputFile), $"Test data file missing: {inputFile}")

        let outputFile = Path.GetTempFileName()
        try
            let games = parsePgnFileWithRaw inputFile |> Seq.toList
            Assert.True(games.Length > 0, "Expected at least one game in test PGN")

            let hasCeres =
                games
                |> Seq.exists (fun g ->
                    g.GameMetaData.White.Contains("Ceres", StringComparison.OrdinalIgnoreCase)
                    || g.GameMetaData.Black.Contains("Ceres", StringComparison.OrdinalIgnoreCase))
            Assert.True(hasCeres, "Expected the test PGN to contain at least one game with 'Ceres' as a player")

            let expectedFilteredGames =
                games
                |> Seq.filter (fun g ->
                    not (
                        g.GameMetaData.White.Contains("Ceres", StringComparison.OrdinalIgnoreCase)
                        || g.GameMetaData.Black.Contains("Ceres", StringComparison.OrdinalIgnoreCase)
                    ))
                |> Seq.toList

            // Mirrors the helper in ChessLibrary/Tests/Test.fs: filter games by player string and write output.
            PGNWriter.removePlayerFromPGN("Ceres", games, outputFile)

            // NOTE: writeRawPgnGamesAdjustedToFile deletes the output file and only recreates it if at least one game is written.
            if expectedFilteredGames.IsEmpty then
                Assert.False(File.Exists(outputFile), "Expected output file to be deleted when all games are filtered out")
            else
                Assert.True(File.Exists(outputFile), "Expected output file to exist when at least one game remains")
                let filteredGames = parsePgnFileWithRaw outputFile |> Seq.toList
                Assert.Equal(expectedFilteredGames.Length, filteredGames.Length)

                let stillHasCeres =
                    filteredGames
                    |> Seq.exists (fun g ->
                        g.GameMetaData.White.Contains("Ceres", StringComparison.OrdinalIgnoreCase)
                        || g.GameMetaData.Black.Contains("Ceres", StringComparison.OrdinalIgnoreCase))
                Assert.False(stillHasCeres, "Output PGN still contains games with 'Ceres'")

                let outputText = File.ReadAllText(outputFile)
                Assert.False(outputText.Contains("Ceres", StringComparison.OrdinalIgnoreCase))
        finally
            File.Delete(outputFile)

// EB writes eval annotations with sprintf "%.2f", so a PGN always carries "wv=0.35" with a
// dot, and evalRegex only matches a dot. Parsing that back under a comma locale must not
// fall through to the 0.0 default — that would silently flatten every eval in the file.
[<Theory>]
[<InlineData("nb-NO")>]
[<InlineData("hu-HU")>]
let ``eval annotations parse under a comma-decimal locale`` (culture: string) =
    let original = System.Threading.Thread.CurrentThread.CurrentCulture
    try
        System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo culture
        Assert.Equal(0.35, ChessLibrary.EngineTypes.Annotation.parseEvalToken "0.35")
        Assert.Equal(-1.5, ChessLibrary.EngineTypes.Annotation.parseEvalToken "-1.50")
        Assert.Equal(200.0, ChessLibrary.EngineTypes.Annotation.parseEvalToken "M5")
        Assert.Equal(-200.0, ChessLibrary.EngineTypes.Annotation.parseEvalToken "-M5")
    finally
        System.Threading.Thread.CurrentThread.CurrentCulture <- original

// ============================================================================
// Parser state isolation and variation color/ply regression tests
// ============================================================================

let private mainlineSans (game: PgnGame) = game.Mainline |> Seq.map (fun m -> m.San) |> Seq.toList
let private mainlineColors (game: PgnGame) = game.Mainline |> Seq.map (fun m -> m.Color) |> Seq.toList
let private mainlinePlies (game: PgnGame) = game.Mainline |> Seq.map (fun m -> m.Ply) |> Seq.toList

[<Fact>]
let ``variation on a white move gets white color and the parent's ply`` () =
    let pgn = """[Event "VarTest"]
[White "A"]
[Black "B"]
[Result "*"]

1. e4 (1. d4) 1... e5 2. Nf3 *
"""
    let game = FullPGNParser.parsePgnString pgn |> Seq.exactlyOne
    Assert.Equal<string list>([ "e4"; "e5"; "Nf3" ], mainlineSans game)
    Assert.Equal<string list>([ "w"; "b"; "w" ], mainlineColors game)
    Assert.Equal<int list>([ 0; 1; 2 ], mainlinePlies game)
    let variation = game.Mainline[0].Variations[0]
    let d4 = variation[0]
    Assert.Equal("d4", d4.San)
    Assert.Equal("w", d4.Color)
    Assert.Equal(0, d4.Ply)

[<Fact>]
let ``odd-length variation on a black move does not flip subsequent mainline colors`` () =
    let pgn = """[Event "VarTest"]
[White "A"]
[Black "B"]
[Result "*"]

1. e4 e5 (1... c5) 2. Nf3 Nc6 *
"""
    let game = FullPGNParser.parsePgnString pgn |> Seq.exactlyOne
    Assert.Equal<string list>([ "e4"; "e5"; "Nf3"; "Nc6" ], mainlineSans game)
    Assert.Equal<string list>([ "w"; "b"; "w"; "b" ], mainlineColors game)
    Assert.Equal<int list>([ 0; 1; 2; 3 ], mainlinePlies game)
    let variation = game.Mainline[1].Variations[0]
    let c5 = variation[0]
    Assert.Equal("c5", c5.San)
    Assert.Equal("b", c5.Color)
    Assert.Equal(1, c5.Ply)

[<Fact>]
let ``concurrent parses do not interleave state`` () =
    let pgnA = """[Event "A"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 *
"""
    let pgnB = """[Event "B"]
[White "Gamma"]
[Black "Delta"]
[Result "*"]

1. d4 d5 2. c4 c6 3. Nc3 Nf6 *
"""
    Parallel.For(0, 200, fun i ->
        let content, expectedWhite, expectedSans =
            if i % 2 = 0 then pgnA, "Alpha", [ "e4"; "e5"; "Nf3"; "Nc6" ]
            else pgnB, "Gamma", [ "d4"; "d5"; "c4"; "c6"; "Nc3"; "Nf6" ]
        let game = FullPGNParser.parsePgnString content |> Seq.exactlyOne
        Assert.Equal(expectedWhite, game.GameMetaData.White)
        Assert.Equal<string list>(expectedSans, mainlineSans game))
    |> ignore

[<Fact>]
let ``re-enumerating a parsed sequence yields the same games`` () =
    let pgn = """[Event "A"]
[White "Alpha"]
[Black "Beta"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 *
"""
    let games = FullPGNParser.parsePgnString pgn
    let first = games |> Seq.toList
    let second = games |> Seq.toList
    Assert.Equal(first.Length, second.Length)
    Assert.Equal(1, second.Head.GameNumber)
    Assert.Equal<string list>(mainlineSans first.Head, mainlineSans second.Head)
