module OpeningsSkipTests

open System
open System.IO
open System.Text.Json
open Xunit
open ChessLibrary
open ChessLibrary.Configuration
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament

// Opening.OpeningsSkip: the first N raw entries of the book are not used, in every mode.

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-skip-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

let private hashOf (g: PGNTypes.PgnGame) = ChessUtilities.Hash.computeOpeningHashFromGame g

[<Fact>]
let ``skipOpenings drops the first entries and nothing else`` () =
    let book = [| for i in 1 .. 5 -> { PGNTypes.PgnGame.Empty i with GameNumber = i } |]
    let numbers skip = PGNHelper.skipOpenings skip book |> Seq.map (fun g -> g.GameNumber) |> Seq.toArray
    Assert.Same(book, PGNHelper.skipOpenings 0 book)
    Assert.Same(book, PGNHelper.skipOpenings -1 book)
    Assert.Equal<int[]>([| 3; 4; 5 |], numbers 2)
    // past the end it is empty, not an exception
    Assert.Empty(numbers 5)
    Assert.Empty(numbers 9)

[<Fact>]
let ``the mode runners' book starts after the skipped entries`` () =
    let path = GoldenBook.writeBook (freshDir ())
    let all, _ = GameHelpers.loadOpeningsUnlimited (Some path) 0 2
    let rest, _ = GameHelpers.loadOpeningsUnlimited (Some path) 3 2
    Assert.Equal(all.Length - 3, rest.Length)
    Assert.Equal(hashOf all.[3], hashOf rest.[0])
    // without a book the skip has nothing to act on: still one empty game per round
    let none, _ = GameHelpers.loadOpeningsUnlimited None 3 4
    Assert.Equal(4, none.Length)

[<Fact>]
let ``a config without OpeningsSkip reads as 0 and a skip reads as given`` () =
    let opts = JsonSerializerOptions(AllowTrailingCommas = true)
    let old = JsonSerializer.Deserialize<Tournament>("""{ "Name":"old", "Opening": { "OpeningsPath":"b.pgn", "OpeningsPly":8, "OpeningsTwice":true, "RandomOpenings":false, "Seed":1 } }""", opts)
    Assert.Equal(0, old.Opening.OpeningsSkip)
    let skip = JsonSerializer.Deserialize<Tournament>("""{ "Name":"skip", "Opening": { "OpeningsPath":"b.pgn", "OpeningsSkip":40 } }""", opts)
    Assert.Equal(40, skip.Opening.OpeningsSkip)

[<Fact>]
let ``a negative skip is a validation error, zero or more is not`` () =
    let t skip = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsSkip = skip } }
    Assert.Equal(Validation.Ok, Validation.validateOpeningSkip (t 0))
    Assert.Equal(Validation.Ok, Validation.validateOpeningSkip (t 7))
    match Validation.validateOpeningSkip (t -1) with
    | Validation.Errors [ msg ] -> Assert.Contains("OpeningsSkip=-1", msg)
    | other -> failwithf "expected one error, got %A" other

[<Fact>]
let ``a skip past the end of the book fails validation with the book size`` () =
    let path = GoldenBook.writeBook (freshDir ())
    let t skip = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPath = Some path; OpeningsSkip = skip } }
    let size = GoldenBook.bookLines.Length
    Assert.Equal(Validation.Ok, Validation.validateOpeningSkip (t (size - 1)))
    match Validation.validateOpeningSkip (t size) with
    | Validation.Errors [ msg ] -> Assert.Contains(sprintf "has %d entries" size, msg)
    | other -> failwithf "expected one error, got %A" other
    // no file, nothing to count
    Assert.Equal(Validation.Ok, Validation.validateOpeningSkip { t 99 with Opening = { (t 99).Opening with OpeningsPath = Some "missing.pgn" } })

[<Fact>]
let ``duplicate warnings look at the book after the skip`` () =
    let dir = freshDir ()
    let path = Path.Combine(dir, "dup.pgn")
    let game n line = sprintf "[Event \"b\"]\n[Round \"%d\"]\n[Result \"*\"]\n\n%s *\n" n line
    File.WriteAllText(path, String.Join("\n", [ game 1 "1. e4 e5"; game 2 "1. e4 e5"; game 3 "1. d4 d5"; game 4 "1. d4 d5" ]))
    let t skip = { Tournament.Empty with Opening = { Tournament.Empty.Opening with OpeningsPath = Some path; OpeningsSkip = skip } }
    // entries are numbered as in the file
    let text = Validation.duplicateOpeningWarnings (t 2) |> String.concat " "
    Assert.Contains("#3", text)
    Assert.DoesNotContain("#1", text)
    // skipping one of a pair leaves no duplicate; the cache does not answer for another skip
    Assert.Empty(Validation.duplicateOpeningWarnings (t 3))
