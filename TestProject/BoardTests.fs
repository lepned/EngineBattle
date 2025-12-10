module BoardTests

open Xunit
open System
open System.IO
open ChessLibrary.Chess
open ChessLibrary.Parser

[<Fact>]
let ``ResetBoardState should reset board to initial state`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    board.ResetBoardState()
    Assert.Equal(0, board.PlyCount)
    Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", board.FEN())

[<Fact>]
let ``LoadFen should set position from FEN string`` () =
    let board = Board()
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    board.LoadFen(fen)
    Assert.Equal(fen, board.FEN())

[<Fact>]
let ``PlaySimpleShortSan should make a legal move and update FEN`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    Assert.Equal(1, board.PlyCount)
    Assert.Contains("e2e4", board.LongSANMovesPlayed)

[<Fact>]
let ``UnMakeMove should revert the last move`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    let fenAfterMove = board.FEN()
    board.UnMakeMove()   
    Assert.Equal(0, board.PlyCount)
    Assert.NotEqual<string>(fenAfterMove, board.FEN())

[<Fact>]
let ``GenerateMoves should return legal moves for initial position`` () =
    let board = Board()
    let moves = board.GenerateMoves()
    Assert.True(moves.Length > 0)

[<Fact>]
let ``IsMate should return false for initial position`` () =
    let board = Board()
    Assert.False(board.IsMate())

[<Fact>]
let ``ClaimThreeFoldRep should return false for new game`` () =
    let board = Board()
    Assert.False(board.ClaimThreeFoldRep())

[<Fact>]
let ``PlayLongSanMove should add move to LongSANMovesPlayed and update PlyCount`` () =
    let board = Board()
    board.PlayLongSanMove "e2e4"
    Assert.Equal(1, board.PlyCount)
    Assert.Contains("e2e4", board.LongSANMovesPlayed)

[<Fact>]
let ``TryGetNextMoveAndFen returns None for empty move list`` () =
    let board = Board()
    let fen = board.FEN()
    Assert.True(board.TryGetNextMoveAndFen(fen).IsNone)

[<Fact>]
let ``TryGetPreviousMoveAndFen returns None for empty move list`` () =
    let board = Board()
    let fen = board.FEN()
    let test = board.TryGetPreviousMoveAndFen(fen)
    let result = 
        match test with
        | Some maf -> maf.FenAfterMove
        | None -> System.String.Empty
    Assert.Equal(result, fen)

[<Fact>]
let ``GetMoveHistoryWithVariations returns PGN styled string with variations`` () =
    let board = Board()
    // Main line: 1. e4 e5 2. Nf3 Nc6
    board.PlaySimpleShortSan "e4"
    let fenAfterE4 = board.FEN()
    board.PlaySimpleShortSan "e5"
    board.PlaySimpleShortSan "Nf3"
    board.PlaySimpleShortSan "Nc6"

    // Variation starting after 1. e4: 1... c5
    board.LoadFen(fenAfterE4)
    board.PlaySimpleShortSan "c5"

    let history = board.GetMoveHistoryWithVariations()
    let expected = "1. e4 e5 (1... c5) 2. Nf3 Nc6"
    Assert.Equal(expected, history)

[<Fact>]
let ``LoadMoveHistoryWithVariations rebuilds tree and formats identically`` () =
    let history = "1. e4 e5 (1... c5) 2. Nf3 Nc6"
    let board = Board()

    board.LoadMoveHistoryWithVariations history

    // Verify round-trip formatting
    let formatted = board.GetMoveHistoryWithVariations()
    Assert.Equal(history, formatted)

    // Ensure the variation exists under the first move in the DAG
    let rootChildren = board.MoveGraphChildren(board.MoveGraphRootId)
    let main = rootChildren |> List.find (fun e -> e.IsMainline)
    let childrenAfterFirst = board.MoveGraphChildren(main.To)
    let variations = childrenAfterFirst |> List.filter (fun e -> not e.IsMainline)
    Assert.True(variations.Length >= 1)

[<Fact>]
let ``LoadMoveHistoryWithVariations supports nested variations`` () =
    // Main line with a variation after 2... plus a nested variation reply
    let history = "1. e4 e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5 (3. d4) 3... Nxe4)"
    let board = Board()
    //main line: 1. e4 e5 2. Nf3 Nc6
    //board.PlaySimpleShortSan "e4"
    //board.PlaySimpleShortSan "e5"
    //board.PlaySimpleShortSan "Nf3"
    //let fenAfterNf3 = board.FEN()
    //board.PlaySimpleShortSan "Nc6"
    
    //// variation after 2... : 2... Nf6
    //board.LoadFen(fenAfterNf3)
    //board.PlaySimpleShortSan "Nf6"
    //let fenAfterNf6 = board.FEN()
    //// continue main variation: 3. Nxe5 Nxe4    
    //board.PlaySimpleShortSan "Nxe5"
    //board.PlaySimpleShortSan "Nxe4"
    //// nested variation after 2... Nf6 : 3. d4
    //board.LoadFen(fenAfterNf6)
    //board.PlaySimpleShortSan "d4"
    //let formatted = board.GetMoveHistoryWithVariations()
    //Assert.Equal(history, formatted)

    board.LoadMoveHistoryWithVariations history
    // Round-trip formatting should match
    let formatted = board.GetMoveHistoryWithVariations()
    Assert.Equal(history, formatted)

    // Verify variation counts: after 2. Nf3 there should be main + a variation
    let rootChildren = board.MoveGraphChildren(board.MoveGraphRootId)
    let m1 = rootChildren |> List.find (fun e -> e.IsMainline) // 1. e4
    let childrenAfterM1 = board.MoveGraphChildren(m1.To)
    let m2 = childrenAfterM1 |> List.find (fun e -> e.IsMainline) // ... e5
    let childrenAfterM2 = board.MoveGraphChildren(m2.To)
    let m3 = childrenAfterM2 |> List.find (fun e -> e.IsMainline) // 2. Nf3
    let repliesToM3 = board.MoveGraphChildren(m3.To)
    Assert.True(repliesToM3 |> List.exists (fun e -> not e.IsMainline))

    // In the variation branch (2... Nf6), there should be a nested variation (3. d4)
    let variationBranch = repliesToM3 |> List.find (fun e -> not e.IsMainline)
    let variationChildren = board.MoveGraphChildren(variationBranch.To)
    Assert.True(variationChildren |> List.exists (fun e -> not e.IsMainline && e.San = "d4"))

[<Fact>]
let ``LoadMoveHistoryWithVariations parses long game with deep variations`` () =
    let history =
        "1. d4 d5 2. Nf3 e6 3. g3 f5 4. Bg2 Nf6 5. O-O Be7 6. c4 c6 7. Qc2 O-O 8. Nbd2 Qe8 9. Ne5 Nbd7 10. Nd3 Bd6 11. Nf3 Ne4 12. Bf4 Bxf4 13. Nxf4 g5 14. Nd3 Qh5 15. Nfe5 Nxe5 16. Nxe5 Nf6 17. f3 Nd7 18. Nd3 Qg6 19. Rae1 Qf6 20. Qc3 dxc4 21. Qc4 Nb6 22. Qc5 Rd8 23. e3 Qf8 24. Qc3 Nd5 25. Qd2 f4 26. exf4 gxf4 27. Re4 Qh6 28. Rf2 Rf8 29. Rfe2 Rf6 30. Nxf4 Nxf4 31. Rxf4 Rxf4 32. Qxf4 Qxf4 33. gxf4 Bd7 34. Kf2 Rf8 35. Ke3 Rf6 36. Bh3 Kf7 37. Ke4 Rh6 38. Bf1 Kf6 39. Rc2 Be8 40. Rd2 Bg6+ 41. Ke3 Bf5 42. Bd3 Rg6 43. Be4 Rg1 44. Kd3 Rf1 45. Ke3 Rg1 46. a3 Re1+ 47. Kf2 Rh1 48. Ke3 Re1+ 49. Re2 Rg1 50. Kd2 Rg7 51. Kc3 Rg1 52. Kb4 Rc1 53. Bxf5 Kxf5 54. Re5+ Kf6 55. Rh5 Kg6 56. Rg5+ Kf7 57. f5 Rc2 58. fxe6+ Kxe6 59. Rh5 Rxb2+ 60. Kc3 Rf2 61. Rh3 Kd5 62. Kd3 Ra2 63. Rh5+ Ke6 64. Ke4 Rxa3 65. Rh6+ Ke7 66. Rxh7+ Ke6 67. Rxb7 Ra2 68. Rh7 Re2+ 69. Kd3 Rf2 70. Ke3 Ra2 71. h4 a5 72. Ra7 a4 73. Ra6 Kd5 74. Ra5+ Kd6 75. h5 Rh2 76. Kf4 a3 77. Kg3 Rh1 78. Kg4 Rg1+ 79. Kf5 Rh1 80. Kg6 Rg1+ 81. Kf6 Rh1 82. Rxa3 (82. f4 a2 83. Rxa2 Rxh5 84. Ra8) 82... Rxh5 83. Re3 Rh3 84. Kf5 Kd5 85. Kg4 Rh8 86. Rc3 Rg8+ 87. Kf4 Rf8+ 88. Ke3 Re8+ 89. Kd3 Re1 90. Rc5+ Kd6 91. Rf5 Re8 92. Kc4 Re6 (92... Re1 93. Rf6+ Kd7) 93. Rf7 Re1 94. Rf6+ Kd7 95. Kd3 Re8 96. Kd2 Re7 97. Kd3 Re1 98. Rh6 Rd1+ 99. Ke3 Re1+ 100. Kd2 Rf1 101. Ke2 Ra1 102. Rf6 Ra3 103. Rg6 Rb3 104. Rg5 Kd6 105. Re5 Ra3 106. f4 Rb3 107. f5 Ra3 108. Re3 Ra5 109. Re6+ Kd7 110. Re5 Ra8 111. Kd3 Rh8 112. Re6 Rh4 113. Ke3 Rg4 114. Rg6 Rh4 115. Re6 Rg4 116. Re5 Rh4 117. Re4 Rh6 118. Kf4 Rh4+ 119. Ke5 Rh6 120. Rg4 Rh7 121. Rg6 Re7+ 122. Re6 Rf7 123. Rd6+ Kc7 124. f6 Rf8 125. Re6 Rh8 126. Re7+ Kd8 127. Ra7 Rh2 128. Kd6 (128. f7 Rf2 129. f8=Q+ Rxf8 130. Ra8+ Ke7 131. Rxf8 Kxf8 132. Kd6 Ke8 133. Kxc6) 128... Ke8 129. Re7+ Kf8 130. Kxc6"
    let board = Board()

    board.LoadMoveHistoryWithVariations history

    let formatted = board.GetMoveHistoryWithVariations()
    let normalize (s:string) =
        s.Replace("(", " ( ").Replace(")", " ) ").Split([|' '; '\t'; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

    // No exact string equality because the formatter may legitimately choose different spacing for long branches.
    // We only compare token-by-token after normalization.

    Assert.Equal<string>(normalize history, normalize formatted)
    let countMainline tokens =
        tokens
        |> Array.fold (fun (depth,count) tok ->
            if tok = "(" then (depth + 1, count)
            elif tok = ")" then (System.Math.Max(0, depth - 1), count)
            elif depth = 0 && not (tok.EndsWith(".") || tok.EndsWith("...")) then (depth, count + 1)
            else (depth, count)
        ) (0,0)
        |> snd

    let expectedMainline = countMainline (normalize history)
    let actualMainline = countMainline (normalize formatted)
    Assert.Equal(expectedMainline, actualMainline)
    // PGN ends on White's 130th move (no black reply): 2*(130-1)+1 = 259 plies
    Assert.Equal(259, expectedMainline)

[<Fact>]
let ``Navigation advances past repeated positions without looping`` () =
    let history =
        "1. d4 d5 2. Nf3 e6 3. g3 f5 4. Bg2 Nf6 5. O-O Be7 6. c4 c6 7. Qc2 O-O 8. Nbd2 Qe8 9. Ne5 Nbd7 10. Nd3 Bd6 11. Nf3 Ne4 12. Bf4 Bxf4 13. Nxf4 g5 14. Nd3 Qh5 15. Nfe5 Nxe5 16. Nxe5 Nf6 17. f3 Nd7 18. Nd3 Qg6 19. Rae1 Qf6 20. Qc3 dxc4 21. Qc4 Nb6 22. Qc5 Rd8 23. e3 Qf8 24. Qc3 Nd5 25. Qd2 f4 26. exf4 gxf4 27. Re4 Qh6 28. Rf2 Rf8 29. Rfe2 Rf6 30. Nxf4 Nxf4 31. Rxf4 Rxf4 32. Qxf4 Qxf4 33. gxf4 Bd7 34. Kf2 Rf8 35. Ke3 Rf6 36. Bh3 Kf7 37. Ke4 Rh6 38. Bf1 Kf6 39. Rc2 Be8 40. Rd2 Bg6+ 41. Ke3 Bf5 42. Bd3 Rg6 43. Be4 Rg1 44. Kd3 Rf1 45. Ke3 Rg1 46. a3 Re1+ 47. Kf2 Rh1 48. Ke3 Re1+ 49. Re2 Rg1 50. Kd2 Rg7 51. Kc3 Rg1 52. Kb4 Rc1 53. Bxf5 Kxf5 54. Re5+ Kf6 55. Rh5 Kg6 56. Rg5+ Kf7 57. f5 Rc2 58. fxe6+ Kxe6 59. Rh5 Rxb2+ 60. Kc3 Rf2 61. Rh3 Kd5 62. Kd3 Ra2 63. Rh5+ Ke6 64. Ke4 Rxa3 65. Rh6+ Ke7 66. Rxh7+ Ke6 67. Rxb7 Ra2 68. Rh7 Re2+ 69. Kd3 Rf2 70. Ke3 Ra2 71. h4 a5 72. Ra7 a4 73. Ra6 Kd5 74. Ra5+ Kd6 75. h5 Rh2 76. Kf4 a3 77. Kg3 Rh1 78. Kg4 Rg1+ 79. Kf5 Rh1 80. Kg6 Rg1+ 81. Kf6 Rh1 82. Rxa3 (82. f4 a2 83. Rxa2 Rxh5 84. Ra8) 82... Rxh5 83. Re3 Rh3 84. Kf5 Kd5 85. Kg4 Rh8 86. Rc3 Rg8+ 87. Kf4 Rf8+ 88. Ke3 Re8+ 89. Kd3 Re1 90. Rc5+ Kd6 91. Rf5 Re8 92. Kc4 Re6 (92... Re1 93. Rf6+ Kd7) 93. Rf7 Re1 94. Rf6+ Kd7 95. Kd3 Re8 96. Kd2 Re7 97. Kd3 Re1 98. Rh6 Rd1+ 99. Ke3 Re1+ 100. Kd2 Rf1 101. Ke2 Ra1 102. Rf6 Ra3 103. Rg6 Rb3 104. Rg5 Kd6 105. Re5 Ra3 106. f4 Rb3 107. f5 Ra3 108. Re3 Ra5 109. Re6+ Kd7 110. Re5 Ra8 111. Kd3 Rh8 112. Re6 Rh4 113. Ke3 Rg4 114. Rg6 Rh4 115. Re6 Rg4 116. Re5 Rh4 117. Re4 Rh6 118. Kf4 Rh4+ 119. Ke5 Rh6 120. Rg4 Rh7 121. Rg6 Re7+ 122. Re6 Rf7 123. Rd6+ Kc7 124. f6 Rf8 125. Re6 Rh8 126. Re7+ Kd8 127. Ra7 Rh2 128. Kd6 (128. f7 Rf2 129. f8=Q+ Rxf8 130. Ra8+ Ke7 131. Rxf8 Kxf8 132. Kd6 Ke8 133. Kxc6) 128... Ke8 129. Re7+ Kf8 130. Kxc6"
    let board = Board()
    board.LoadMoveHistoryWithVariations history

    // Start navigation from the initial position, just like the GUI.
    let startFen = board.StartPosition
    board.LoadFen(startFen)
    let mutable fen = startFen
    let sanSeq = ResizeArray<string>()

    let mutable safety = 0
    let mutable keepGoing = true
    while keepGoing && safety < 200 do
        match board.TryGetNextMoveAndFen(fen) with
        | Some move ->
            sanSeq.Add(move.ShortSan)
            fen <- move.FenAfterMove
            board.LoadFen(fen)
            safety <- safety + 1
        | None -> keepGoing <- false

    // There should be at least two Rg1 occurrences, and they must be distinct forward steps.
    let rg1Indices =
        sanSeq
        |> Seq.indexed
        |> Seq.filter (fun (_, san) -> san = "Rg1")
        |> Seq.map fst
        |> Seq.toArray

    Assert.True(rg1Indices.Length >= 2)
    let firstRg1 = rg1Indices[0]
    let secondRg1 = rg1Indices[1]
    // Ensure we advanced forward to the later Rg1 (not looped back).
    Assert.True(secondRg1 > firstRg1 + 2)
    // Verify the expected local sequence around the first Rg1.
    let window =
        [ for i in firstRg1 - 1 .. firstRg1 + 4 do
            if i >= 0 && i < sanSeq.Count then yield sanSeq[i] ]
    Assert.Equal<string>(["Be4"; "Rg1"; "Kd3"; "Rf1"; "Ke3"; "Rg1"], window)

[<Fact>]
let ``RemoveVariationNode removes only the targeted variation move`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    let fenAfterE4 = board.FEN()
    board.PlaySimpleShortSan "e5"

    // Create a single-move side variation: 1... c5
    board.LoadFen(fenAfterE4)
    board.PlaySimpleShortSan "c5"
    let fenAfterC5 = board.FEN()

    // Variation should exist alongside the mainline reply.
    let rootChildren = board.MoveGraphChildren(board.MoveGraphRootId)
    let mainAfterRoot = rootChildren |> List.find (fun e -> e.IsMainline)
    let childrenAfterE4 = board.MoveGraphChildren(mainAfterRoot.To)
    Assert.True(childrenAfterE4 |> List.exists (fun e -> not e.IsMainline && e.San = "c5"))

    let removed = board.RemoveVariationNode "c5" fenAfterC5 false
    Assert.True(removed)

    let childrenAfterRemoval = board.MoveGraphChildren(mainAfterRoot.To)
    Assert.False(childrenAfterRemoval |> List.exists (fun e -> e.San = "c5"))
    Assert.True(childrenAfterRemoval |> List.exists (fun e -> e.IsMainline && e.San = "e5"))

[<Fact>]
let ``RemoveVariationNode with full branch pruning removes entire variation`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    let fenAfterE4 = board.FEN()
    board.PlaySimpleShortSan "e5"

    // Build a two-ply variation: 1... c5 2. Nf3
    board.LoadFen(fenAfterE4)
    board.PlaySimpleShortSan "c5"
    board.PlaySimpleShortSan "Nf3"
    let fenAfterC5Nf3 = board.FEN()

    let rootChildren = board.MoveGraphChildren(board.MoveGraphRootId)
    let mainAfterRoot = rootChildren |> List.find (fun e -> e.IsMainline)
    let childrenAfterE4 = board.MoveGraphChildren(mainAfterRoot.To)
    Assert.True(childrenAfterE4 |> List.exists (fun e -> not e.IsMainline && e.San = "c5"))

    let removed = board.RemoveVariationNode "Nf3" fenAfterC5Nf3 true
    Assert.True(removed)

    let childrenAfterRemoval = board.MoveGraphChildren(mainAfterRoot.To)
    // The whole c5 ... branch should be gone.
    Assert.False(childrenAfterRemoval |> List.exists (fun e -> e.San = "c5"))
    Assert.True(childrenAfterRemoval.Length = 1 && childrenAfterRemoval.Head.IsMainline)

[<Fact>]
let ``PromoteVariationToMainline promotes side line to main`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    let fenAfterE4 = board.FEN()
    board.PlaySimpleShortSan "e5"

    // Variation after 1. e4: 1... c5
    board.LoadFen(fenAfterE4)
    board.PlaySimpleShortSan "c5"
    let fenAfterC5 = board.FEN()

    let rootChildren = board.MoveGraphChildren(board.MoveGraphRootId)
    let mainAfterRoot = rootChildren |> List.find (fun e -> e.IsMainline) // 1. e4
    let childrenAfterE4 = board.MoveGraphChildren(mainAfterRoot.To)
    let mainBefore = childrenAfterE4 |> List.find (fun e -> e.IsMainline)
    let variationBefore = childrenAfterE4 |> List.find (fun e -> not e.IsMainline)
    Assert.Equal("e5", mainBefore.San)
    Assert.Equal("c5", variationBefore.San)

    let promoted = board.PromoteVariationToMainline "c5" fenAfterC5
    Assert.True(promoted)

    let childrenAfterPromotion = board.MoveGraphChildren(mainAfterRoot.To)
    let newMain = childrenAfterPromotion |> List.find (fun e -> e.IsMainline)
    let oldMain = childrenAfterPromotion |> List.find (fun e -> e.San = "e5")
    Assert.Equal("c5", newMain.San)
    Assert.False(oldMain.IsMainline)
    Assert.True(newMain.Order = 0)

    let moveHistory = board.GetMoveHistoryWithVariations()
    Assert.Equal("1. e4 c5 (1... e5)", moveHistory)

[<Fact>]
let ``PromoteVariationToMainline from variation leaf promotes the branch`` () =
    let board = Board()
    // Main line up to 6...c6
    for san in [ "d4"; "d5"; "Nf3"; "Nf6"; "g3"; "e6"; "Bg2"; "Be7"; "O-O"; "O-O"; "c4"; "c6" ] do
        board.PlaySimpleShortSan san
    let fenAfter6c6 = board.FEN() // White to move on move 7

    // Main continues with 7. Qc2
    board.PlaySimpleShortSan "Qc2"
    let fenAfterQc2 = board.FEN()

    // Create variation: 7. b3 Nbd7
    board.LoadFen(fenAfter6c6)
    board.PlaySimpleShortSan "b3"
    board.PlaySimpleShortSan "Nbd7"
    let fenAfterNbd7 = board.FEN()
    board.PlaySimpleShortSan "Bb2"
    board.PlaySimpleShortSan "a5"
    let fenAftera5 = board.FEN()
    let oldMoveHistory = board.GetMoveHistoryWithVariations()
    // Promote by selecting the leaf move ("a5" in the variation)
    let promoted = board.PromoteVariationToMainline "a5" fenAftera5
    Assert.True(promoted)

    // Mainline should now follow ...c6 b3 Nbd7 Bb2 a5, and the old main (Qc2) should be a variation.
    let lines = board.MoveLinesFromGraph false
    let mainLine = lines.Head
    let lastFour = mainLine |> List.skip (mainLine.Length - 4)
    Assert.Equal<string list>(["b3"; "Nbd7"; "Bb2"; "a5"], lastFour)
    // Ensure the former main move Qc2 exists as a variation line from the same parent.
    Assert.True(lines |> List.exists (fun l -> l |> List.rev |> List.tryHead = Some "Qc2"))
    let newMoveHistory = board.GetMoveHistoryWithVariations()
    Assert.NotSame(oldMoveHistory, newMoveHistory)
    let promoted = board.PromoteVariationToMainline "Qc2" fenAfterQc2
    Assert.True(promoted)
    let restoredMoveHistory = board.GetMoveHistoryWithVariations()
    Assert.Equal(oldMoveHistory, restoredMoveHistory)
    let promoted = board.PromoteVariationToMainline "Nbd7" fenAfterNbd7
    Assert.True(promoted)
    let finalMoveHistory = board.GetMoveHistoryWithVariations()
    Assert.NotSame(restoredMoveHistory, finalMoveHistory)


 
[<Fact>]
let ``RemoveVariationTail trims only tail of variation`` () =
    let board = Board()
    // Main line: 1. e4 e5 2. Nf3 Nc6
    ["e4"; "e5"; "Nf3"] |> List.iter board.PlaySimpleShortSan
    let fenAfterNf3 = board.FEN()
    board.PlaySimpleShortSan "Nc6"

    // Variation: 2... Nf6 3. d4 d6 4. Nc3
    board.LoadFen(fenAfterNf3)
    board.PlaySimpleShortSan "Nf6"
    board.PlaySimpleShortSan "d4"
    let fenAfterD4 = board.FEN()
    board.PlaySimpleShortSan "d6"
    board.PlaySimpleShortSan "Nc3"

    // Delete from d4 onward in the variation
    let removed = board.RemoveVariationTail "d4" fenAfterD4
    Assert.True(removed)

    let lines = board.MoveLinesFromGraph false
    let mainLine = lines.Head
    Assert.Equal<string list>(["e4"; "e5"; "Nf3"; "Nc6"], mainLine) // mainline intact

    let variationLines = lines |> List.filter (fun l -> l <> mainLine)
    Assert.True(variationLines |> List.exists (fun l -> l = ["e4"; "e5"; "Nf3"; "Nf6"]))
    Assert.False(variationLines |> List.exists (fun l -> l |> List.exists (fun san -> san = "d4" || san = "d6" || san = "Nc3")))

[<Fact>]
let ``RemoveVariationTail trims mainline when no variations`` () =
    let board = Board()
    board.PlaySimpleShortSan "e4"
    board.PlaySimpleShortSan "e5"
    board.PlaySimpleShortSan "Nf3"
    let fenAfterNf3 = board.FEN()
    board.PlaySimpleShortSan "Nc6"
    board.PlaySimpleShortSan "Bb5"

    let removed = board.RemoveVariationTail "Nf3" fenAfterNf3
    Assert.True(removed)

    let lines = board.MoveLinesFromGraph false
    let mainLine = lines.Head
    Assert.Equal<string list>(["e4"; "e5"], mainLine)
    Assert.Equal(1, lines.Length)

[<Fact>]
let ``InlineTokensFromGraph preserves lichess odds variations`` () =
    let path = Path.Combine(AppContext.BaseDirectory, "TestData", "lichess_pgn_LeelaPieceOddsFRC.pgn")
    let raw = File.ReadAllText(path)
    let game = PGNParser.parseFullPgnGame raw
    let board = Board()
    let fen = if String.IsNullOrWhiteSpace(game.GameMetaData.Fen) then startPos else game.GameMetaData.Fen
    board.LoadFen(fen)
    board.StartPosition <- fen

    board.LoadPGNGameWithVariations game
    let tokens = board.InlineTokensFromGraph() |> Seq.toList
    let texts = tokens |> List.filter (fun t -> not t.IsBracket) |> List.map (fun t -> t.Text)

    Assert.Contains("f6", texts)   // variation after 11...exf4?
    Assert.Contains("Qg5", texts)  // variation after 15...Nf7?
    Assert.Contains("Qe8", texts)  // variation after 16...Qg5?!
    Assert.Contains("Rh7", texts)  // variation after 18...Bh7??
    // Ensure the final mate move is present (allowing move number prefix)
    Assert.True(texts |> List.exists (fun t -> t.EndsWith("f7#")))

[<Fact>]
let ``InlineTokensFromGraph handles late variation comment game`` () =
    let pgn = """
[Event "Single Engine Analysis"]
[Date "2025.11.29.08:59"]
[Engine "lc0_BT4-it332-TRT"]
[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]
[Eval "lc0_BT4-it332-TRT: Eval=-5.88, Depth=7, SD=25"]
[Move "67... Bxf5"]

1. d4 Nf6 2. c4 e6 3. Nf3 Bb4+ 4. Nbd2 O-O 5. a3 Be7 6. e4 d6 7. Be2 c5 8. d5 e5 9. h3 Na6 10. Bd3 Nc7 11. a4 Na6 12. Nf1 Nb4 13. Bb1 b6 14. Ng3 a6 15. Bd2 Rb8 16. 0-0 b5 17. b3 g6 18. Bh6 Re8 19. Qc1 Nd7 20. Nh2 Rb7 21. Ra3 Bf7 22. Bg5 Be7 23. Bd2 Bh4 24. Ng4 Nf6 25. Nh2 Nd7 26. Ne2 Nb6 27. Kh1 Bf6 28. a5 Nd7 29. Ng4 Bh4 30. Nh6 Kg7 31. Ra1 Nf6 32. g3 Nh5 33. Ng4 Be7 34. f4 Nf6 35. Nf2 exf4 36. gxf4 Bf8 37. f5 Kh8 38. Ng3 Nd7 39. Ng4 Bg7 40. Ra3 Kg8 41. Bh6 Be5 42. Kg2 f6 43. Qe1 Kh8 44. Qd2 g5 45. Ne2 Rg8 46. Qc1 Qe7 47. Qd2 Qf7 48. Qc1 Qh5 49. Rg1 Qh4 50. Qd2 Rb8 51. Rf1 Re8 52. Qc1 Bb7 53. Qd2 Re7 54. Rf3 Rbe8 55. Rf1 Kg8 56. Rh1 Kf7 57. Rf1 Rg8 58. Rh1 Ree8 59. Rf1 Ke7 60. Rh1 Bc8 61. Qc1 Kd8 62. Qd2 Kc7 63. Rc1 Kb8 64. Rf1 Ka8 65. Rh1 Re7 66. Qc1 Nb8 67. Qd2 {variation starts here} Bxf5 68.exf5 Rge8 69.Rf1 Bh2 70.Nc1 Qg3 71.Kh1 Re1 72.Nxh2 Rxf1 73.Nxf1 Re1 74.Qg2 Qf4 75.Kg1 Qxc1 76.Ra1 Qc3 77.Qf2 Nd7 78.Bxg5 fxg5
"""
    let board = Board()
    board.LoadFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    board.StartPosition <- board.FEN()
    let movesOnly =
        pgn.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter (fun line -> not (line.TrimStart().StartsWith("[")))
        |> String.concat " "
    Assert.Contains("fxg5", movesOnly)
    // Manually step through moves to catch the first failure point.
    let playSequential (txt:string) =
        let normalized =
            txt.Replace("(", " ( ").Replace(")", " ) ").Split([|' '; '\t'; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        let isMoveNumber (tok:string) = tok.EndsWith(".") || tok.EndsWith("...") || tok |> Seq.forall Char.IsDigit
        let mutable lastOk = ""
        let mutable idx = 0
        let mutable failed = None
        while idx < normalized.Length && failed.IsNone do
            let tok = normalized[idx]
            match tok with
            | "(" | ")" -> ()
            | t when isMoveNumber t -> ()
            | san ->
                try
                    board.PlaySimpleShortSan san
                    lastOk <- san
                with ex ->
                    failed <- Some (san, idx, ex.Message)
            idx <- idx + 1
        failed, lastOk

    let failed, lastOk = playSequential movesOnly
    Assert.True(failed.IsNone, $"Failed at {failed}")
    board.ResetBoardStateFromFen(board.StartPosition)
    board.LoadMoveHistoryWithVariations movesOnly
    let tokens = board.InlineTokensFromGraph() |> Seq.toList
    let texts = tokens |> List.filter (fun t -> not t.IsBracket) |> List.map (fun t -> t.Text)
    let lastText = texts |> List.last
    Assert.True(board.MovesAndFenPlayed.Count > 70, $"Only {board.MovesAndFenPlayed.Count} moves parsed")
    let lastMove = board.MovesAndFenPlayed |> Seq.last
    Assert.Equal("fxg5", lastMove.ShortSan)
    Assert.Equal("fxg5", lastText)
