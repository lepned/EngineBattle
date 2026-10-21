/// pgn dedupe / PGN Tools' Remove Duplicate Openings: the tournament's own rule, the file kept as it was.
module OpeningDedupeTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.OpeningDedupe

let private pgn =
    String.concat "\n"
        [ "[Event \"A\"]"; "[Round \"1\"]"; "[Annotator \"kept as it was\"]"; "[Result \"*\"]"; ""; "1. e4 e5 2. Nf3 *"; ""
          "[Event \"B\"]"; "[Round \"2\"]"; "[Result \"*\"]"; ""; "1. e4 e5 2. Nf3 *"; ""
          "[Event \"C\"]"; "[Round \"3\"]"; "[Result \"*\"]"; ""; "1. Nf3 e5 2. e4 *"; "" ]

[<Fact>]
let ``a repeated line is left out, a transposition is kept, and the kept openings are written as they were`` () =
    let dir = Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), "eb_dedupe_" + Guid.NewGuid().ToString("N"))).FullName
    try
        let input, output = Path.Combine(dir, "book.pgn"), Path.Combine(dir, "book_unique.pgn")
        File.WriteAllText(input, pgn)
        let s = OpeningDedupe.removeDuplicates input output
        Assert.Equal({ Openings = 3; Kept = 2; Removed = 1; Groups = 1 }, s)
        let text = File.ReadAllText output
        // A (with all its tags) and C stay, B goes; the same position by other moves (C) is another opening
        Assert.Contains("[Event \"A\"]", text)
        Assert.Contains("[Annotator \"kept as it was\"]", text)
        Assert.DoesNotContain("[Event \"B\"]", text)
        Assert.Contains("1. Nf3 e5 2. e4", text)
        Assert.Throws<ArgumentException>(fun () -> OpeningDedupe.removeDuplicates input input |> ignore) |> ignore
    finally
        try Directory.Delete(dir, true) with _ -> ()

[<Fact>]
let ``a Windows file comes out with one line ending throughout, and no temporary file is left`` () =
    let dir = Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), "eb_dedupe_" + Guid.NewGuid().ToString("N"))).FullName
    try
        let input, output = Path.Combine(dir, "book.pgn"), Path.Combine(dir, "book_unique.pgn")
        File.WriteAllText(input, pgn.Replace("\n", "\r\n"))
        OpeningDedupe.removeDuplicates input output |> ignore
        let text = File.ReadAllText output
        Assert.DoesNotContain("\r", text)
        Assert.False(File.Exists(output + ".tmp"))
    finally
        try Directory.Delete(dir, true) with _ -> ()
