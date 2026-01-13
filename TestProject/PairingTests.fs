module PairingTests

open System.Collections.Generic
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

let private seedOrderNames players groupCount =
    tcecSeedOrder players groupCount |> List.map (fun p -> p.Name)

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
let ``round robin keeps colors balanced across first four rounds for 8 players`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F"; mkEngine "G"; mkEngine "H" ]
    let opening = mkOpening 1
    let games = getPairingsPerOpening players opening

    let gamesPerRound = players.Length / 2
    let firstFourRounds = games |> List.take (gamesPerRound * 4)

    let addCount name delta counts =
        let current = counts |> Map.tryFind name |> Option.defaultValue 0
        counts |> Map.add name (current + delta)

    let colorCounts =
        firstFourRounds
        |> List.fold (fun acc g ->
            acc
            |> addCount g.White.Name 1
            |> addCount g.Black.Name -1) Map.empty

    for player in players do
        let diff = colorCounts.[player.Name]
        Assert.Equal(0, diff)

[<Fact>]
let ``round robin keeps colors within one for 9 players`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"; mkEngine "E"
          mkEngine "F"; mkEngine "G"; mkEngine "H"; mkEngine "I" ]
    let opening = mkOpening 1
    let games = getPairingsPerOpening players opening

    let addCount name delta counts =
        let current = counts |> Map.tryFind name |> Option.defaultValue 0
        counts |> Map.add name (current + delta)

    let colorCounts =
        games
        |> List.fold (fun acc g ->
            acc
            |> addCount g.White.Name 1
            |> addCount g.Black.Name -1) Map.empty

    for player in players do
        let diff = colorCounts.[player.Name]
        Assert.True(abs diff <= 1, $"Color imbalance too large for {player.Name}: {diff}")

[<Fact>]
let ``swiss first round schedules weaker seeded pairs earlier`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 4

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList
    let minSeed (a: EngineConfig, b: EngineConfig) =
        let sa = seedMap.[a.Name]
        let sb = seedMap.[b.Name]
        if sa < sb then sa else sb

    let mins = pairs |> List.map minSeed
    Assert.Equal<int list>(mins |> List.sortDescending, mins)

[<Fact>]
let ``swiss seed order matches expected for group counts`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]

    let orderGroup1 = seedOrderNames players 1
    let orderGroup2 = seedOrderNames players 2
    let orderGroup4 = seedOrderNames players 4

    Assert.Equal<string list>([ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H" ], orderGroup1)
    Assert.Equal<string list>([ "A"; "E"; "B"; "F"; "C"; "G"; "D"; "H" ], orderGroup2)
    Assert.Equal<string list>([ "A"; "C"; "E"; "G"; "B"; "D"; "F"; "H" ], orderGroup4)

[<Fact>]
let ``swiss first round pair ordering is deterministic for group count 4`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 4

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let names = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "G-H"; "E-F"; "C-D"; "A-B" ], names)

[<Fact>]
let ``swiss first round pair ordering is deterministic for group count 1`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 1

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let names = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "D-H"; "C-G"; "B-F"; "A-E" ], names)

[<Fact>]
let ``swiss first round pair ordering is deterministic for group count 2`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 2

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let names = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "F-H"; "B-D"; "E-G"; "A-C" ], names)

[<Fact>]
let ``swiss second round ordering uses score then seed for group count 4`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let scores =
        [ "A", 1.0; "B", 1.0; "C", 1.0; "D", 1.0
          "E", 0.0; "F", 0.0; "G", 0.0; "H", 0.0 ]
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 4

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let names = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "G-H"; "E-F"; "C-D"; "A-B" ], names)

[<Fact>]
let ``swiss seeding uses rating descending for seed one`` () =
    let players =
        [ mkRatedPlayer "Top" 3200
          mkRatedPlayer "Mid" 2900
          mkRatedPlayer "Low" 2500 ]
    let seedOrder = tcecSeedOrder players 2
    Assert.Equal("Top", seedOrder.Head.Name)

[<Fact>]
let ``swiss seeding distributes players across seed groups`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let seedOrder = tcecSeedOrder players 2 |> List.map (fun p -> p.Name)
    Assert.Equal<string list>([ "A"; "E"; "B"; "F"; "C"; "G"; "D"; "H" ], seedOrder)

[<Fact>]
let ``swiss seeding is deterministic when ratings are equal`` () =
    let players =
        [ mkRatedPlayer "Zeta" 3000
          mkRatedPlayer "Alpha" 3000
          mkRatedPlayer "Echo" 3000
          mkRatedPlayer "Beta" 3000 ]
    let seedOrder = tcecSeedOrder players 2 |> List.map (fun p -> p.Name)
    Assert.Equal<string list>([ "Zeta"; "Echo"; "Alpha"; "Beta" ], seedOrder)

[<Fact>]
let ``swiss assigns bye to lowest seeded player when odd`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 1

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let byePair = pairs |> List.tryFind (fun (a, b) -> b.Name = "BYE")
    Assert.True(byePair.IsSome)
    Assert.Equal("E", byePair.Value |> fst |> fun p -> p.Name)

[<Fact>]
let ``swiss bye pair is ordered first for odd group`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800 ]
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 1

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    let firstPair = pairs |> List.head
    Assert.Equal("BYE", (snd firstPair).Name)

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
let ``cup auto seed bands for 8 players`` () =
    let bands = autoSeedBands 8
    Assert.Equal<int list list>([ [1]; [2]; [3; 4]; [5; 6; 7; 8] ], bands)

[<Fact>]
let ``cup auto seed bands for 16 players`` () =
    let bands = autoSeedBands 16
    Assert.Equal<int list list>([ [1]; [2]; [3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12; 13; 14; 15; 16] ], bands)

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
let ``cup auto seed bands randomize within bands`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands true
    let orderedNames = seeded |> List.map (fun p -> p.Name)
    let order = seedOrder players.Length
    let getSlotIndex seedNumber = order |> List.findIndex (fun s -> s = seedNumber)
    let bandSet seeds =
        seeds
        |> List.map (fun seed -> players.[seed - 1].Name)
        |> Set.ofList
    let assertBand seeds =
        let bandNames = bandSet seeds
        let positions = seeds |> List.map getSlotIndex
        let actual =
            positions
            |> List.map (fun idx -> orderedNames.[idx])
            |> Set.ofList
        Assert.Equal<Set<string>>(bandNames, actual)
    assertBand [1]
    assertBand [2]
    assertBand [3; 4]
    assertBand [5; 6; 7; 8]

[<Fact>]
let ``cup auto seed bands randomize 5-8 when enabled`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let bands = autoSeedBands players.Length
    let seedBandNames =
        [ "P5"; "P6"; "P7"; "P8" ] |> Set.ofList
    let order = seedOrder players.Length
    let positions = [5; 6; 7; 8] |> List.map (fun seed -> order |> List.findIndex (fun s -> s = seed))
    let results = HashSet<string>()
    for _ in 1 .. 25 do
        let seeded = seedByBands players bands true
        let orderedNames = seeded |> List.map (fun p -> p.Name)
        let bandOrder =
            positions
            |> List.map (fun idx -> orderedNames.[idx])
        if Set.ofList bandOrder = seedBandNames then
            results.Add(System.String.Join("|", bandOrder)) |> ignore
    Assert.True(results.Count > 1, "Band [5..8] should randomize within its slots.")

[<Fact>]
let ``cup auto seed bands randomize 3-4 positions when enabled`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let bands = autoSeedBands players.Length
    let order = seedOrder players.Length
    let positions = [3; 4] |> List.map (fun seed -> order |> List.findIndex (fun s -> s = seed))
    let seenPositions = HashSet<int>()
    for _ in 1 .. 25 do
        let seeded = seedByBands players bands true
        let orderedNames = seeded |> List.map (fun p -> p.Name)
        let idx =
            positions
            |> List.find (fun i -> orderedNames.[i] = "P3")
        seenPositions.Add(idx) |> ignore
    Assert.Equal(2, seenPositions.Count)

[<Fact>]
let ``cup auto seed bands avoid 1 vs 2 and 3 vs 4 in round one`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands true
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let pairs = List.zip top (List.rev bottom)
    let allPlayers =
        pairs
        |> List.collect (fun (a, b) -> [ a.Name; b.Name ])
        |> Set.ofList
    Assert.Equal(8, allPlayers.Count)
    let pairNames =
        pairs
        |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
        |> Set.ofList
    Assert.DoesNotContain(Set.ofList [ "P1"; "P2" ], pairNames)
    Assert.DoesNotContain(Set.ofList [ "P3"; "P4" ], pairNames)
