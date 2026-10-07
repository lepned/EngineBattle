module PgnAgentTests

open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.CoreTypes

[<Fact>]
let ``a closed PGN agent has let go of its file: the next run can delete or reopen it at once`` () =
  for _ in 1 .. 20 do
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb-pgnagent-%s.pgn" (System.Guid.NewGuid().ToString "N"))
    let agent = FullPGNParser.startPgnGameReaderWriter path
    let result = createResult "A" "B" (ResizeArray<string>()) "1-0" ChessLibrary.MiscTypes.ResultReason.Checkmate 1000L
    agent.Post(FullPGNParser.WriteGame(path, { GameMetadata.Empty with White = "A"; Black = "B" }, "1. e4 1-0", result))
    FullPGNParser.closePgnAgent agent
    // no retry: the file must already be closed, written game included
    Assert.Contains("[White \"A\"]", File.ReadAllText path)
    File.Delete path
    Assert.False(File.Exists path)
