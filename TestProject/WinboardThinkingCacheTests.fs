/// The Winboard handler keeps the last thinking-line PV it converted to coordinates, because
/// engines repeat one PV over many lines and the SAN conversion was nearly all of its cost. These
/// tests hold the cache to two things: a PV is never reused on a different position, and the
/// cached translation is exactly what the plain one (parseThinkingOutput) gives.
module WinboardThinkingCacheTests

open Xunit
open Microsoft.Extensions.Logging.Abstractions
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.WinboardProtocol

let private newHandler () =
    let h = WinboardHandler(NullLogger.Instance, "wb", WinboardConfig.Default)
    h.ProcessFeatureLine "feature setboard=1 ping=1 done=1" |> ignore
    h

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

[<Fact>]
let ``A cached PV is not reused after the position changes`` () =
    let h = newHandler ()
    h.UciToWinboard "position startpos" |> ignore
    Assert.Equal(Some "info depth 1 score cp 20 time 10 nodes 1000 nps 100000 pv e2e4", h.ProcessOutput "1 20 1 1000 e4")
    // The same text, another position: "e4" is now the e3 pawn's move.
    h.UciToWinboard "position startpos moves e2e3 e7e6" |> ignore
    Assert.Equal(Some "info depth 1 score cp 20 time 10 nodes 1000 nps 100000 pv e3e4", h.ProcessOutput "1 20 1 1000 e4")
    // And a new game resets the board, and with it the cache.
    h.Reset()
    Assert.Equal(Some "info depth 1 score cp 20 time 10 nodes 1000 nps 100000 pv e2e4", h.ProcessOutput "1 20 1 1000 e4")

[<Fact>]
let ``The cached translation equals the plain one over a sequence of positions`` () =
    let h = newHandler ()
    let reference = Chess.Board()
    let steps =
        [ "position startpos", [ "1 12 1 100 e4 e5 Nf3"; "2 15 2 200 e4 e5 Nf3"; "2 15 3 300 e4 e5 Nf3"; "3 9 4 400 d4 d5" ]
          "position startpos moves e2e4", [ "1 -10 1 50 e5 Nf3"; "2 -12 2 90 e5 Nf3"; "3 -8 3 120 c5 Nf3 d6" ]
          "position startpos moves e2e4 e7e5", [ "1 30 1 10 Nf3 Nc6 Bb5"; "1 30 1 10 Nf3 Nc6 Bb5"; "2 25 2 20 g1-f3 b8-c6" ]
          "position fen " + startFen + " moves d2d4", [ "1 5 1 10 d5"; "1 5 1 10 1. ... d5 2. c4"; "2 7 2 30 Nf6 c4 e6" ] ]
    for positionCommand, lines in steps do
        h.UciToWinboard positionCommand |> ignore
        reference.PlayCommands(if positionCommand.StartsWith "position fen" then positionCommand else "position fen " + startFen + positionCommand.Substring("position startpos".Length))
        for line in lines do
            let expected = parseThinkingOutput false reference "wb" line
            Assert.Equal(expected, h.ProcessOutput line)
