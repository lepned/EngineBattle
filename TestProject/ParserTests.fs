module ParserTests

open System
open System.IO
open Xunit
open ChessLibrary.Parser
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes

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
