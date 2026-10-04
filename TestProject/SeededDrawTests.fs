module SeededDrawTests

/// Opening.Seed fixes the opening orders and the cup draw.
open Xunit
open ChessLibrary.TournamentPairing.PairingHelper

[<Fact>]
let ``a seeded order is the same for the same seed and purpose, and differs between them`` () =
    let a = seededOrder 7 "cup-openings" 50
    Assert.Equal<int[]>(a, seededOrder 7 "cup-openings" 50)
    Assert.Equal<int list>([ 0 .. 49 ], a |> Array.sort |> Array.toList)
    Assert.NotEqual<int[]>(a, seededOrder 8 "cup-openings" 50)
    Assert.NotEqual<int[]>(a, seededOrder 7 "swiss-openings" 50)

[<Fact>]
let ``the cup draw within bands is reproducible from the seed`` () =
    let players =
        [ for i in 1 .. 16 -> { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = sprintf "E%d" i; Rating = 3000 - i } ]
    let bands = [ [ 1; 2 ]; [ 3; 4 ]; [ 5 .. 8 ]; [ 9 .. 16 ] ]
    let draw seed =
        let rng = seededRandom seed "cup-draw"
        seedByBandsWith (Some (fun a -> rng.Shuffle(a))) players bands
        |> List.map (fun p -> p.Name)
    Assert.Equal<string list>(draw 3, draw 3)
    Assert.NotEqual<string list>(draw 3, draw 4)
