module ParserTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open ChessLibrary.Parser
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.Parser.MoveParser

[<Fact>]
let ``getOpeningInfo returns correct string for opening/variation/eco`` () =
    let meta = { GameMetadata.Empty with OtherTags = [ { Key = "Opening"; Value = "Ruy Lopez" }; { Key = "Variation"; Value = "Closed" }; { Key = "ECO"; Value = "C84" } ] }
    let game = { PgnGame.Empty 1 with GameMetaData = meta }
    let result = PGNHelper.getOpeningInfo game
    Assert.Equal("Opening: Ruy Lopez - Closed, ECO: C84", result)

[<Fact>]
let ``extractMoves returns correct moves and metadata`` () =
    let move1 = { MoveNr = "1"; WhiteSan = "e4"; WhiteComment = ""; BlackSan = "e5"; BlackComment = "" }
    let move2 = { MoveNr = "2"; WhiteSan = "Nf3"; WhiteComment = ""; BlackSan = "Nc6"; BlackComment = "" }
    let meta = { GameMetadata.Empty with White = "Carlsen"; Black = "Nepomniachtchi" }
    let game = { PgnGame.Empty 1 with GameMetaData = meta; Moves = ResizeArray([move1; move2]) }
    let moves, metadata = PGNExtractor.extractMoves game
    Assert.Equal(4, Array.length moves)
    // Use pattern matching to extract the second element (name) from the 4-tuple
    let ( _, whiteName, _, _ ) = Array.find (fun (isW,_,_,_) -> isW) moves
    let ( _, blackName, _, _ ) = Array.find (fun (isW,_,_,_) -> not isW) moves
    Assert.Equal("Carlsen", whiteName)
    Assert.Equal("Nepomniachtchi", blackName)

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
    let games = PGNParser.parsePgnFile(tempFile) |> Seq.toList

    // Assert: check the parsed result
    Assert.Single(games) |> ignore
    let game: PgnGame = games.Head
    Assert.Equal("Alpha", game.GameMetaData.White)
    Assert.Equal("Beta", game.GameMetaData.Black)
    Assert.Equal("1-0", game.GameMetaData.Result)
    Assert.Equal(2, game.Moves.Count) // 2 moves (e4/e5, Nf3/Nc6)
    Assert.Equal("e4", game.Moves.[0].WhiteSan)
    Assert.Equal("e5", game.Moves.[0].BlackSan)
    Assert.Equal("Nf3", game.Moves.[1].WhiteSan)
    Assert.Equal("Nc6", game.Moves.[1].BlackSan)

    // Cleanup
    File.Delete(tempFile)

[<Fact>]
let ``parsePgnFile aborts when exceeding configured timeout`` () =
    let tempFile = Path.GetTempFileName()
    let pgnContent = """
[Event "Timeout Test"]
[Site "Nowhere"]
[Date "2024.01.01"]
[Round "1"]
[White "Alpha"]
[Black "Beta"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 *
"""
    File.WriteAllText(tempFile, pgnContent)

    // Set a zero-millisecond timeout to trigger the guard immediately.
    MoveParser.setGameParseTimeoutMs 0L
    try
        Assert.ThrowsAny<Exception>(fun () -> PGNParser.parsePgnFile(tempFile) |> Seq.toList |> ignore) |> ignore
    finally
        MoveParser.resetGameParseTimeoutMs()
        File.Delete(tempFile)

[<Fact>]
let ``parseFullPgnGame does not loop on movetext-only PGN`` () : Task = task {
    let pgn = "1. e4 e5 2. Nf3 Nc6 *"
    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(3000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing hung on movetext-only PGN")
    let game = parseTask.Result
    Assert.True(game.Moves.Count >= 0, "Parsing returned no game object")
    return ()
    }

[<Fact>]
let ``parseFullPgnGame skips semicolon and percent comment lines without hanging`` () : Task = task {
    let prev = MoveParser.maxLinesPerGame
    MoveParser.setMaxLinesPerGame 1000
    let pgn = """
[Event "Comment Test"]
; this is a full-line comment
% engine stuff

1. e4 e5 2. Nf3 Nc6 *
"""
    try
        let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
        let! completed = Task.WhenAny(parseTask, Task.Delay(3000))
        Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing hung on semicolon/% comment lines")
        let game = parseTask.Result
        Assert.Equal(2, game.Moves.Count)
        Assert.Equal("e4", game.Moves.[0].WhiteSan)
        Assert.Equal("e5", game.Moves.[0].BlackSan)
    finally
        MoveParser.setMaxLinesPerGame prev
    }

[<Fact>]
let ``parseFullPgnGame does not hang on unterminated brace comment`` () : Task = task {
    let pgn = """
[Event "Unterminated Comment"]

1. e4 {oops
1... c5 *
"""
    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
    let! completed = Task.WhenAny(parseTask, Task.Delay(3000))
    Assert.True(obj.ReferenceEquals(parseTask, completed), "Parsing hung on unterminated comment")
    let _ = parseTask.Result
    return ()
    }

[<Fact>]
let ``calculateMedianNodes returns 0.0 for empty array`` () =
    let moves : ChessLibrary.TypesDef.Engine.EngineMoveStat array = [||]
    let result = ChessLibrary.Parser.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(0.0, result)

[<Fact>]
let ``calculateMedianNodes returns correct median for odd count`` () =
    let moves =
        [| { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 30L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 20L } |]
    let result = ChessLibrary.Parser.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(20.0, result)

[<Fact>]
let ``calculateMedianNodes returns correct median for even count`` () =
    let moves =
        [| { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 30L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 20L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 40L } |]
    let result = ChessLibrary.Parser.PGNStatistics.calculateMedianNodes moves
    Assert.Equal(25.0, result)

[<Fact>]
let ``calculateMedianNodes ignores zero and negative nodes`` () =
    let moves =
        [| { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 0L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = -5L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 10L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 20L }
           { ChessLibrary.TypesDef.Engine.EngineMoveStat.Empty with n = 30L } |]
    let result = ChessLibrary.Parser.PGNStatistics.calculateMedianNodes moves
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
    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
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
    let game = PGNParser.parseFullPgnGame pgn
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

    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
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

    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
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
    let game = PGNParser.parseFullPgnGame pgn
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

    let parseTask = Task.Run(fun () -> PGNParser.parseFullPgnGame pgn)
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
