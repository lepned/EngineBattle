module PairingTests

open Xunit
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.Utilities.PairingHelper
open ChessLibrary

let private mkEngine name =
    { EngineConfig.Empty with Name = name }

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

let private mkRatedPlayer name rating =
    { mkEngine name with Rating = rating }

[<Fact>]
let ``gauntletSingleRound pairs challengers with all opponents`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletSingleRound false challengers opponents openings

    Assert.Equal(2, games.Length)
    Assert.All(games, fun g -> Assert.Equal("A", g.White.Name))
    Assert.Equal<string list>([ "1.1"; "1.2" ], games |> List.map (fun g -> g.RoundNr))
    Assert.Equal<int list>([ 1; 2 ], games |> List.map (fun g -> g.GameNr))

[<Fact>]
let ``gauntletDoubleRound includes color-reversed rematches`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletDoubleRound false challengers opponents openings

    Assert.Equal(4, games.Length)

    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, orderedPairs.[("A", "B")])
    Assert.Equal(1, orderedPairs.[("A", "C")])
    Assert.Equal(1, orderedPairs.[("B", "A")])
    Assert.Equal(1, orderedPairs.[("C", "A")])

[<Fact>]
let ``round robin single round creates one game per unordered pair`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinSingleRounds players openings

    Assert.Equal(6, games.Length)

    let unorderedPairs =
        games
        |> List.map (fun g ->
            let a, b = g.White.Name, g.Black.Name
            if a < b then (a, b) else (b, a))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, unorderedPairs.[("A", "B")])
    Assert.Equal(1, unorderedPairs.[("A", "C")])
    Assert.Equal(1, unorderedPairs.[("A", "D")])
    Assert.Equal(1, unorderedPairs.[("B", "C")])
    Assert.Equal(1, unorderedPairs.[("B", "D")])
    Assert.Equal(1, unorderedPairs.[("C", "D")])

[<Fact>]
let ``round robin double round creates both colors for each pair`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinDoubleRounds players openings

    Assert.Equal(12, games.Length)

    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, orderedPairs.[("A", "B")])
    Assert.Equal(1, orderedPairs.[("B", "A")])
    Assert.Equal(1, orderedPairs.[("A", "C")])
    Assert.Equal(1, orderedPairs.[("C", "A")])
    Assert.Equal(1, orderedPairs.[("A", "D")])
    Assert.Equal(1, orderedPairs.[("D", "A")])
    Assert.Equal(1, orderedPairs.[("B", "C")])
    Assert.Equal(1, orderedPairs.[("C", "B")])
    Assert.Equal(1, orderedPairs.[("B", "D")])
    Assert.Equal(1, orderedPairs.[("D", "B")])
    Assert.Equal(1, orderedPairs.[("C", "D")])
    Assert.Equal(1, orderedPairs.[("D", "C")])

[<Fact>]
let ``cup draw by rating pairs highest with lowest`` () =
    let players =
        [ { mkEngine "A" with Rating = 3200 }
          { mkEngine "B" with Rating = 2800 }
          { mkEngine "C" with Rating = 2600 }
          { mkEngine "D" with Rating = 3000 } ]
    let openings = [ mkOpening 1 ]

    let games = cupNRound 2 CupSeedingStrategy.ByRating players openings

    Assert.Equal(4, games.Length)
    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> Set.ofList
    let expectedPairs =
        Set.ofList [ ("A", "C"); ("C", "A"); ("B", "D"); ("D", "B") ]
    Assert.Equal<Set<string * string>>(expectedPairs, orderedPairs)

[<Fact>]
let ``cup N round uses same opening for both colors`` () =
    let players =
        [ { mkEngine "A" with Rating = 3200 }
          { mkEngine "B" with Rating = 2800 }
          { mkEngine "C" with Rating = 2600 }
          { mkEngine "D" with Rating = 3000 } ]
    let openings = [ mkOpening 1 ]

    let games = cupNRound 2 CupSeedingStrategy.ByRating players openings

    Assert.Equal(4, games.Length)

    let byMatch =
        games
        |> List.groupBy (fun g ->
            let a, b = g.White.Name, g.Black.Name
            if a < b then (a, b) else (b, a))

    Assert.Equal(2, byMatch.Length)
    for _, matchGames in byMatch do
        Assert.Equal(2, matchGames.Length)
        Assert.Equal(matchGames.[0].Opening.Raw, matchGames.[1].Opening.Raw)

[<Fact>]
let ``cup N round uses unique openings per pairing`` () =
    let players =
        [ { mkEngine "A" with Rating = 3200 }
          { mkEngine "B" with Rating = 2800 }
          { mkEngine "C" with Rating = 2600 }
          { mkEngine "D" with Rating = 3000 } ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = cupNRound 2 CupSeedingStrategy.ByRating players openings

    let byMatch =
        games
        |> List.groupBy (fun g ->
            let a, b = g.White.Name, g.Black.Name
            if a < b then (a, b) else (b, a))

    Assert.Equal(2, byMatch.Length)
    let openingNames =
        byMatch
        |> List.map (fun (_, matchGames) -> matchGames.[0].Opening.Raw)
        |> Set.ofList
    Assert.Equal(2, openingNames.Count)

[<Fact>]
let ``cup round pair increments override games per match`` () =
    Assert.Equal(2, gamesPerMatchForRound 2 [1; 2; 3] 1)
    Assert.Equal(4, gamesPerMatchForRound 2 [1; 2; 3] 2)
    Assert.Equal(6, gamesPerMatchForRound 2 [1; 2; 3] 3)
    Assert.Equal(6, gamesPerMatchForRound 2 [1; 2; 3] 4)

[<Fact>]
let ``cup tiebreak picks unused opening when available`` () =
    let openings = [ mkOpening 1; mkOpening 2; mkOpening 3 ]
    let used =
        [ openings.[0]; openings.[1] ]
        |> Seq.map (fun o ->
            if System.String.IsNullOrWhiteSpace o.Raw then
                ChessLibrary.Utilities.Hash.computeOpeningHash (o.GameNumber.ToString())
            else
                ChessLibrary.Utilities.Hash.computeOpeningHash o.Raw)
        |> Set.ofSeq
    let idx = nextUnusedOpeningIndex used openings 0
    Assert.Equal(2, idx)

[<Fact>]
let ``cup seed bands place top seeds in standard order`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let bands = [ [1]; [2]; [3; 4]; [5; 6; 7; 8] ]
    let seeded = seedByBands players bands false
    let orderedNames = seeded |> List.map (fun p -> p.Name)
    Assert.Equal<string list>([ "P1"; "P8"; "P5"; "P4"; "P3"; "P6"; "P7"; "P2" ], orderedNames)
