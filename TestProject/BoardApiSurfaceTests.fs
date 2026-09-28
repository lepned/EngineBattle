/// The public surface of module ChessLibrary.Chess - `type Board` and the module's own values -
/// pinned as text, as EngineApiSurfaceTests does for the engine layer. The Board rewrite keeps
/// this API unchanged (the WebGUI's C# compiles against it); a deliberate change updates
/// TestData/BoardApi.txt in the same commit.
module BoardApiSurfaceTests

open System
open System.IO
open Xunit

let boardApiSurface () =
    let asm = typeof<ChessLibrary.Chess.Board>.Assembly
    EngineApiSurfaceTests.describeType (asm.GetType("ChessLibrary.Chess", true))
    |> String.concat "\n"

[<Fact>]
let ``The board's public API matches the pinned surface`` () =
    let expectedPath = Path.Combine(AppContext.BaseDirectory, "TestData", "BoardApi.txt")
    let expected = File.ReadAllText(expectedPath).Replace("\r\n", "\n").TrimEnd()
    let actual = boardApiSurface ()
    if expected <> actual then
        let dump = Path.Combine(AppContext.BaseDirectory, "BoardApi.actual.txt")
        File.WriteAllText(dump, actual)
        Assert.Fail(sprintf "The board API changed. Current surface written to %s - diff it against TestData/BoardApi.txt." dump)
