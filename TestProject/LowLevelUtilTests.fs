module LowLevelUtilitiesTests

open System
open System.IO
open Xunit
open ChessLibrary.LowLevelUtilities
open ChessLibrary.LowLevelUtilities.Agents
open ChessLibrary.LowLevelUtilities.ConsoleUtils
open ChessLibrary.LowLevelUtilities.BoardHelper
open ChessLibrary.TypesDef.Position

// Test for parseUciResponse
[<Fact>]
let ``parseUciResponse should parse valid UCI responses`` () =
    let response = parseUciResponse "id name Stockfish"
    Assert.Equal(Some(Id("name", "Stockfish")), response)

    let response2 = parseUciResponse "uciok"
    Assert.Equal(Some(UciOk), response2)

    let response3 = parseUciResponse "readyok"
    Assert.Equal(Some(ReadyOk), response3)

    let response4 = parseUciResponse "bestmove e2e4"
    Assert.Equal(Some(BestMove("e2e4")), response4)

    let response5 = parseUciResponse "info depth 20"
    Assert.Equal(Some(Info("info depth 20")), response5)

    let response6 = parseUciResponse "unknown command"
    Assert.Equal(Some(Unknown("unknown command")), response6)

// Test for handleUciResponse
[<Fact>]
let ``handleUciResponse should handle UCI responses correctly`` () =
    use sw = new StringWriter()
    Console.SetOut(sw)

    handleUciResponse (Id("name", "Stockfish"))
    handleUciResponse UciOk
    handleUciResponse ReadyOk
    handleUciResponse (BestMove("e2e4"))
    handleUciResponse (Info("info depth 20"))
    handleUciResponse (Unknown("unknown command"))

    let output = sw.ToString()
    Assert.Contains("ID: name Stockfish", output)
    Assert.Contains("UCI OK", output)
    Assert.Contains("Ready OK", output)
    Assert.Contains("Best Move: e2e4", output)
    Assert.Contains("Info: info depth 20", output)
    Assert.Contains("Unknown response: unknown command", output)

// Test for flipVertical
[<Fact>]
let ``flipVertical should flip the bitboard vertically`` () =
    let input = 0x00000000000000FFUL
    let expected = 0xFF00000000000000UL
    let result = flipVertical input
    Assert.Equal(expected, result)

// Test for posToFen
[<Fact>]
let ``posToFen should convert position to FEN string`` () =
    let position = Position.Default
    let fen = posToFen position
    Assert.Equal("8/8/8/8/8/8/8/8 w - - 0 1", fen)

// Test for getPosFromFen
[<Fact>]
let ``getPosFromFen should parse FEN string into position`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let position = getPosFromFen (Some fen)
    Assert.Equal(PositionOps.WHITE, position.STM)
    Assert.Equal(0uy, position.Count50)
    Assert.Equal(0uy, position.Ply)

// Test for createOutputAgent
[<Fact>]
let ``createOutputAgent should process messages correctly`` () =
    let agent = createOutputAgent()
    agent.Post(Line "uciok")
    agent.Post(Stop)
    // No exception should occur

// Test for printInColor
[<Fact>]
let ``printInColor should print text in specified color`` () =
    use sw = new StringWriter()
    Console.SetOut(sw)

    printInColor ConsoleColor.Green "Test Message"
    let output = sw.ToString()
    Assert.Contains("Test Message", output)
