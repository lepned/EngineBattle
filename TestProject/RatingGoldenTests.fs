/// RatingFit against Ordo's numbers for real games: TestData/RatingGolden holds, per data set, the
/// results of a PGN and the table Ordo made of it (a one-off script wrote both, from Ordo). The fit
/// must give Ordo's ratings and draw rate, and errors near Ordo's simulated ones.
module RatingGoldenTests

open System
open System.IO
open System.Text.Json
open Xunit
open ChessLibrary.RatingFit

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "TestData", "RatingGolden")

let datasets : seq<obj[]> =
    Directory.GetFiles(goldenDir, "*.expected.json")
    |> Seq.map (fun f -> [| box (Path.GetFileName(f).Replace(".expected.json", "")) |])

let private readGames (name: string) =
    File.ReadAllLines(Path.Combine(goldenDir, name + ".results.txt"))
    |> Array.choose (fun line ->
        match line.Split '\t' with
        | [| w; b; r |] ->
            let outcome = match r with "1-0" -> WhiteWin | "0-1" -> BlackWin | _ -> Draw
            Some { White = w; Black = b; Outcome = outcome }
        | _ -> None)

[<Theory>]
[<MemberData(nameof datasets)>]
let ``the fit gives Ordo's ratings, errors and draw rate`` (name: string) =
    use doc = JsonDocument.Parse(File.ReadAllText(Path.Combine(goldenDir, name + ".expected.json")))
    let root = doc.RootElement
    let anchor = match root.GetProperty("anchor") with a when a.ValueKind = JsonValueKind.String -> Some(a.GetString()) | _ -> None
    // a set made with Ordo's -W has the white advantage it fitted; the fit's prior moves ours by about 1 %
    let whiteAdvantage =
        match root.TryGetProperty "whiteAdvantage" with
        | true, w when w.ValueKind = JsonValueKind.Number -> Some(w.GetDouble())
        | _ -> None
    let f = fit { defaultOptions with WhiteAdvantage = (if whiteAdvantage.IsSome then None else Some 0.0); Anchor = anchor } (readGames name)
    let problems = ResizeArray<string>()
    let check what (expected: float) (actual: float) (tolerance: float) =
        if abs (expected - actual) > tolerance then problems.Add(sprintf "%s: Ordo %.2f, fit %.2f" what expected actual)
    let ratingTolerance = if whiteAdvantage.IsSome then 1.5 else 0.5
    whiteAdvantage |> Option.iter (fun w -> check "white advantage" w f.WhiteAdvantage (max 2.0 (0.03 * abs w)))
    // the draw rate for equal opponents is Ordo's at Ordo's white advantage: with a large advantage
    // it is a steep extrapolation that a 1 % change in the advantage moves by points, so a fitted-
    // advantage set compares it at Ordo's value, and not its error (Ordo's simulations keep the
    // advantage fixed; the delta method here does not)
    let atOrdosWhite = match whiteAdvantage with Some w -> fit { defaultOptions with WhiteAdvantage = Some w; Anchor = anchor } (readGames name) | None -> f
    check "draw rate" (root.GetProperty("drawRate").GetDouble()) atOrdosWhite.DrawRate 0.05
    match root.GetProperty("drawRateError") with
    | e when e.ValueKind = JsonValueKind.Number && whiteAdvantage.IsNone -> check "draw rate error" (e.GetDouble()) f.DrawRateError (max 0.1 (0.15 * e.GetDouble()))
    | _ -> ()
    let expectedPlayers = root.GetProperty("players").EnumerateArray() |> Seq.toList
    Assert.Equal(expectedPlayers.Length, f.Players.Length)
    for p in expectedPlayers do
        let pname = p.GetProperty("name").GetString()
        match f.Players |> List.tryFind (fun x -> x.Name = pname) with
        | None -> problems.Add(sprintf "%s: not rated" pname)
        | Some x ->
            check (pname + " rating") (p.GetProperty("rating").GetDouble()) x.Rating ratingTolerance
            // Ordo's errors are a thousand simulations: within 10 %, or 2 Elo for the smallest
            let e = p.GetProperty("error").GetDouble()
            check (pname + " error") e x.Error (max 2.0 (0.1 * e))
    Assert.True(problems.Count = 0, String.Join("\n", problems))
