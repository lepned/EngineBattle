/// Repertoire build as the GUI asks for it: the limit shared with eb-cli, the moves from the board, the side and the file name.
module RepertoireBuildTests

open Xunit
open ChessLibrary
open ChessLibrary.RepertoireBuild

let private tarrasch = [ "d2d4"; "d7d5"; "c2c4"; "e7e6"; "b1c3"; "c7c5" ]

[<Fact>]
let ``the GUI's limits are eb-cli's, so the two share a cache`` () =
    for kind, value, cli in [ "nodes", 2_000_000, Cli.Groups.Nodes 2_000_000; "movetime", 1500, Cli.Groups.MoveTime 1500; "depth", 22, Cli.Groups.Depth 22 ] do
        Assert.Equal((cli.Go, cli.Text), limit kind value)

[<Fact>]
let ``a position command gives its FEN and moves`` () =
    Assert.Equal((MiscTypes.startPosition, tarrasch), ofPositionCommand ("position startpos moves " + String.concat " " tarrasch))
    Assert.Equal((MiscTypes.startPosition, []), ofPositionCommand "position startpos")
    let fen = "8/8/8/8/8/2k5/8/K6Q w - - 0 1"
    Assert.Equal((fen, [ "h1h8" ]), ofPositionCommand $"position fen {fen} moves h1h8")

[<Fact>]
let ``the side is the one that made the last move, and the file is named after the moves`` () =
    Assert.False(sideOf MiscTypes.startPosition tarrasch)
    Assert.True(sideOf MiscTypes.startPosition [ "e2e4" ])
    Assert.True(sideOf MiscTypes.startPosition [])
    Assert.Equal("Repertoire_Black_d4_d5_c4_e6_Nc3_c5.pgn", fileName false MiscTypes.startPosition tarrasch)
    // check signs are left out of a file name
    Assert.Equal("Repertoire_White_e4_f5_Qh5.pgn", fileName true MiscTypes.startPosition [ "e2e4"; "f7f5"; "d1h5" ])
