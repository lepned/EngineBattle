module PgnHeaderTests

open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes

let private headerText (header: GameMetadata) =
  let path = Path.GetTempFileName()
  try
    do
      use writer = new StreamWriter(path)
      PGNWriter.writePGNHeaderSection writer header
    File.ReadAllText path
  finally File.Delete path

[<Fact>]
let ``the header counts the game's half-moves in the standard PlyCount tag`` () =
  // a 16-ply book and 90 engine plies; Moves is EngineBattle's 45 moves out of book
  let text = headerText { GameMetadata.Empty with Moves = 45; PlyCount = 106 }
  Assert.Contains("[PlyCount \"106\"]", text)
  Assert.DoesNotContain("[Ply \"", text)

[<Fact>]
let ``a parsed game's PlyCount is its half-moves, whatever an old Ply tag said`` () =
  let path = Path.GetTempFileName()
  try
    File.WriteAllText(path, "[White \"A\"]\n[Black \"B\"]\n[Result \"*\"]\n[Ply \"1\"]\n\n1. e4 e5 2. Nf3 Nc6 3. Bb5 *\n")
    let game = FullPGNParser.parsePgnFile path |> Seq.head
    Assert.Equal(5, game.GameMetaData.PlyCount)
  finally File.Delete path
