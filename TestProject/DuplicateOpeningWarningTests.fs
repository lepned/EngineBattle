module DuplicateOpeningWarningTests

open System
open System.IO
open Xunit
open ChessLibrary.Configuration
open ChessLibrary.TypesDef.Tournament

let private withBook (pgn: string) (test: Tournament -> unit) =
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_book_%s.pgn" (Guid.NewGuid().ToString("N")))
    File.WriteAllText(path, pgn)
    try
        let tourny = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPath = Some path } }
        test tourny
    finally
        File.Delete path

let private entry (name: string) (eco: string) (moves: string) =
    sprintf "[Event \"book\"]\n[Opening \"%s\"]\n[ECO \"%s\"]\n\n%s *\n\n" name eco moves

[<Fact>]
let ``Book entries with the same moves are reported as one opening`` () =
    // The first two entries of Sufi17-26.pgn: two TCEC names, both nothing but 1.d4.
    let book =
        entry "Queen's pawn game" "E10" "1.d4"
        + entry "QGD semi-Slav" "D43" "1.d4"
        + entry "Sicilian" "B81" "1.e4 c5 2.Nf3 e6"
    withBook book (fun tourny ->
        let warnings = Validation.duplicateOpeningWarnings tourny
        let w = Assert.Single(warnings)
        Assert.Contains("2 openings", w)
        Assert.Contains("#1 Queen's pawn game (E10)", w)
        Assert.Contains("#2 QGD semi-Slav (D43)", w)
        Assert.Contains("1.d4", w)
        Assert.DoesNotContain("#3", w))

[<Fact>]
let ``A book of distinct openings gives no warning`` () =
    let book =
        entry "Queen's pawn game" "E10" "1.d4"
        + entry "King's pawn" "C20" "1.e4"
        + entry "Sicilian" "B81" "1.e4 c5 2.Nf3 e6"
    withBook book (fun tourny ->
        Assert.Empty(Validation.duplicateOpeningWarnings tourny))

[<Fact>]
let ``No book gives no warning`` () =
    let tourny = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPath = None } }
    Assert.Empty(Validation.duplicateOpeningWarnings tourny)

[<Fact>]
let ``A book edited after a check is read again`` () =
    let path = Path.Combine(Path.GetTempPath(), sprintf "eb_book_%s.pgn" (Guid.NewGuid().ToString("N")))
    try
        File.WriteAllText(path, entry "A" "A00" "1.d4" + entry "B" "A01" "1.d4")
        let tourny = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPath = Some path } }
        Assert.Single(Validation.duplicateOpeningWarnings tourny) |> ignore
        File.WriteAllText(path, entry "A" "A00" "1.d4" + entry "B" "A01" "1.e4 e5")
        File.SetLastWriteTimeUtc(path, DateTime.UtcNow.AddMinutes 1.0)
        Assert.Empty(Validation.duplicateOpeningWarnings tourny)
    finally
        File.Delete path
