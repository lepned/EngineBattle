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

let private mkPlyMove ply moveNumber color san comment =
    { Ply = ply
      MoveNumber = moveNumber
      Color = color
      San = san
      Comment = comment
      Nags = []
      Variations = ResizeArray() }

let private mkGameWithMoves gameNr fen (moves: PlyMove list) =
    { PgnGame.Empty gameNr with
        GameMetaData = { GameMetadata.Empty with Fen = fen }
        Mainline = ResizeArray<PlyMove>(moves) }

let private mkRatedPlayer name rating =
    { mkEngine name with Rating = rating }

let private mkCupMatch roundNumber (playerA: EngineConfig) (playerB: EngineConfig) : ChessLibrary.TypesDef.Cup.CupMatch =
    { MatchId = 1
      RoundNumber = roundNumber
      PlayerA = playerA.Name
      PlayerB = playerB.Name
      PlayerARating = playerA.Rating
      PlayerBRating = playerB.Rating
      ScoreA = 0.0
      ScoreB = 0.0
      Winner = None
      IsDecided = false
      Games = ResizeArray<ChessLibrary.TypesDef.Cup.CupGame>()
      OpeningOrder = ResizeArray<int>() }

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
let ``opening hash is deterministic for identical content`` () =
    let moves =
        [ mkPlyMove 0 1 "w" "e4" ""
          mkPlyMove 1 1 "b" "e5" "" ]
    let game = mkGameWithMoves 1 "startpos" moves
    let hash1 = Utilities.Hash.computeOpeningHashFromGame game
    let hash2 = Utilities.Hash.computeOpeningHashFromGame game
    Assert.Equal(hash1, hash2)

[<Fact>]
let ``opening hash changes when fen or moves change`` () =
    let moves =
        [ mkPlyMove 0 1 "w" "e4" ""
          mkPlyMove 1 1 "b" "e5" "" ]
    let game = mkGameWithMoves 1 "startpos" moves
    let baseHash = Utilities.Hash.computeOpeningHashFromGame game

    let gameDiffFen = { game with GameMetaData = { game.GameMetaData with Fen = "different" } }
    let diffFenHash = Utilities.Hash.computeOpeningHashFromGame gameDiffFen
    Assert.NotEqual<string>(baseHash, diffFenHash)

    let movesDiff =
        [ mkPlyMove 0 1 "w" "d4" ""
          mkPlyMove 1 1 "b" "d5" "" ]
    let gameDiffMoves = mkGameWithMoves 1 "startpos" movesDiff
    let diffMovesHash = Utilities.Hash.computeOpeningHashFromGame gameDiffMoves
    Assert.NotEqual<string>(baseHash, diffMovesHash)

[<Fact>]
let ``opening hash falls back to game number when empty`` () =
    let hash1 = Utilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 1)
    let hash2 = Utilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 2)
    Assert.NotEqual<string>(hash1, hash2)

[<Fact>]
let ``opening hash uses fen when present (epd style)`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let game = mkGameWithMoves 1 fen []
    let hash = Utilities.Hash.computeOpeningHashFromGame game
    let expected =
        let nl = System.Environment.NewLine
        Utilities.Hash.computeOpeningHash (sprintf "[Fen \"%s\"]%s%s" fen nl nl)
    Assert.Equal(expected, hash)

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
let ``cup planned pairings include remaining games in match`` () =
    let playerA = mkRatedPlayer "A" 3000
    let playerB = mkRatedPlayer "B" 2900
    let openings = [ mkOpening 1; mkOpening 2; mkOpening 3 ]
    let matchInfo = mkCupMatch 1 playerA playerB
    let playOrder = [ (playerA, playerB); (playerB, playerA) ]

    let planned =
        buildRemainingCupPairings matchInfo playerA playerB openings openings.[0] playOrder 6 0

    Assert.Equal(6, planned.Count)
    let openingRaws = planned |> Seq.map (fun p -> p.Opening.Raw) |> Seq.toList
    Assert.Equal<string list>([ "opening-1"; "opening-1"; "opening-2"; "opening-2"; "opening-3"; "opening-3" ], openingRaws)
    let roundNrs = planned |> Seq.map (fun p -> p.RoundNr) |> Seq.toList
    Assert.Equal<string list>([ "1.1"; "1.2"; "1.3"; "1.4"; "1.5"; "1.6" ], roundNrs)

[<Fact>]
let ``cup planned pairings continue after odd game`` () =
    let playerA = mkRatedPlayer "A" 3000
    let playerB = mkRatedPlayer "B" 2900
    let openings = [ mkOpening 1; mkOpening 2; mkOpening 3 ]
    let matchInfo = mkCupMatch 2 playerA playerB
    let openingHash = Utilities.Hash.computeOpeningHashFromGame openings.[0]
    matchInfo.Games.Add
        { GameNr = 1
          White = playerA.Name
          Black = playerB.Name
          OpeningId = openings.[0].GameNumber.ToString()
          OpeningHash = openingHash
          Result = "1-0" }

    let planned =
        buildRemainingCupPairings matchInfo playerA playerB openings openings.[0] [ (playerB, playerA) ] 3 1

    Assert.Equal(3, planned.Count)
    let openingRaws = planned |> Seq.map (fun p -> p.Opening.Raw) |> Seq.toList
    Assert.Equal<string list>([ "opening-1"; "opening-2"; "opening-2" ], openingRaws)
    let roundNrs = planned |> Seq.map (fun p -> p.RoundNr) |> Seq.toList
    Assert.Equal<string list>([ "2.2"; "2.3"; "2.4" ], roundNrs)

[<Fact>]
let ``cup tiebreak picks unused opening when available`` () =
    let openings = [ mkOpening 1; mkOpening 2; mkOpening 3 ]
    let used =
        [ openings.[0]; openings.[1] ]
        |> Seq.map (fun o ->
            Utilities.Hash.computeOpeningHashFromGame o)
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
let ``swiss tournament with 8 players round 1 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3100
          mkRatedPlayer "Player3" 3000
          mkRatedPlayer "Player4" 2900
          mkRatedPlayer "Player5" 2800
          mkRatedPlayer "Player6" 2700
          mkRatedPlayer "Player7" 2600
          mkRatedPlayer "Player8" 2500 ]
    
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(4, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "Player4-Player8"; "Player3-Player7"; "Player2-Player6"; "Player1-Player5" ], pairNames)

[<Fact>]
let ``swiss tournament with 8 players round 2 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3100
          mkRatedPlayer "Player3" 3000
          mkRatedPlayer "Player4" 2900
          mkRatedPlayer "Player5" 2800
          mkRatedPlayer "Player6" 2700
          mkRatedPlayer "Player7" 2600
          mkRatedPlayer "Player8" 2500 ]
    
    let scores =
        [ "Player1", 1.0; "Player2", 1.0; "Player3", 1.0; "Player4", 1.0
          "Player5", 0.0; "Player6", 0.0; "Player7", 0.0; "Player8", 0.0 ]
        |> Map.ofList
    
    let priorPairs = 
        Set.ofList [ 
            swissPairKey "Player1" "Player5"
            swissPairKey "Player2" "Player6"
            swissPairKey "Player3" "Player7"
            swissPairKey "Player4" "Player8"
        ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(4, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "Player6-Player8"; "Player5-Player7"; "Player2-Player4"; "Player1-Player3" ], pairNames)

[<Fact>]
let ``swiss tournament with 8 players round 3 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3100
          mkRatedPlayer "Player3" 3000
          mkRatedPlayer "Player4" 2900
          mkRatedPlayer "Player5" 2800
          mkRatedPlayer "Player6" 2700
          mkRatedPlayer "Player7" 2600
          mkRatedPlayer "Player8" 2500 ]
    
    let scores =
        [ "Player1", 2.0; "Player2", 2.0
          "Player3", 1.0; "Player4", 1.0; "Player5", 1.0; "Player6", 1.0
          "Player7", 0.0; "Player8", 0.0 ]
        |> Map.ofList
    
    let priorPairs = 
        Set.ofList [ 
            swissPairKey "Player1" "Player5"
            swissPairKey "Player2" "Player6"
            swissPairKey "Player3" "Player7"
            swissPairKey "Player4" "Player8"
            swissPairKey "Player1" "Player3"
            swissPairKey "Player2" "Player4"
            swissPairKey "Player5" "Player7"
            swissPairKey "Player6" "Player8"
        ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(4, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "Player7-Player8"; "Player4-Player6"; "Player3-Player5"; "Player1-Player2" ], pairNames)

[<Fact>]
let ``swiss tournament 8 players round 1-3 no repeated pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3100
          mkRatedPlayer "Player3" 3000
          mkRatedPlayer "Player4" 2900
          mkRatedPlayer "Player5" 2800
          mkRatedPlayer "Player6" 2700
          mkRatedPlayer "Player7" 2600
          mkRatedPlayer "Player8" 2500 ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let round1Scores =
        players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let round1Pairs = swissRoundPairings players seedOrder round1Scores Set.empty Set.empty
    
    let round1PriorPairs =
        round1Pairs
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
        |> Set.ofList
    
    let round2Scores =
        [ "Player1", 1.0; "Player2", 1.0; "Player3", 1.0; "Player4", 1.0
          "Player5", 0.0; "Player6", 0.0; "Player7", 0.0; "Player8", 0.0 ]
        |> Map.ofList
    let round2Pairs = swissRoundPairings players seedOrder round2Scores round1PriorPairs Set.empty
    
    let round2PriorPairs =
        round1PriorPairs
        |> Set.union (round2Pairs |> List.map (fun (a, b) -> swissPairKey a.Name b.Name) |> Set.ofList)
    
    let round3Scores =
        [ "Player1", 2.0; "Player2", 2.0
          "Player3", 1.0; "Player4", 1.0; "Player5", 1.0; "Player6", 1.0
          "Player7", 0.0; "Player8", 0.0 ]
        |> Map.ofList
    let round3Pairs = swissRoundPairings players seedOrder round3Scores round2PriorPairs Set.empty
    
    let allPairs =
        round1Pairs @ round2Pairs @ round3Pairs
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
    
    let uniquePairs = allPairs |> Set.ofList
    
    Assert.Equal(allPairs.Length, uniquePairs.Count)
    Assert.Equal(12, uniquePairs.Count)

[<Fact>]
let ``swiss tournament 8 players round 1 correct pairing order with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3100
          mkRatedPlayer "Player3" 3000
          mkRatedPlayer "Player4" 2900
          mkRatedPlayer "Player5" 2800
          mkRatedPlayer "Player6" 2700
          mkRatedPlayer "Player7" 2600
          mkRatedPlayer "Player8" 2500 ]
    
    let seedOrder = tcecSeedOrder players 1
    let seedOrderNames = seedOrder |> List.map (fun p -> p.Name)
    
    Assert.Equal<string list>(
        [ "Player1"; "Player2"; "Player3"; "Player4"; "Player5"; "Player6"; "Player7"; "Player8" ],
        seedOrderNames)
    
    let scores = players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    
    Assert.Equal(4, pairs.Length)

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

[<Fact>]
let ``swiss tournament with 16 players round 1 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3150
          mkRatedPlayer "Player3" 3100
          mkRatedPlayer "Player4" 3050
          mkRatedPlayer "Player5" 3000
          mkRatedPlayer "Player6" 2950
          mkRatedPlayer "Player7" 2900
          mkRatedPlayer "Player8" 2850
          mkRatedPlayer "Player9" 2800
          mkRatedPlayer "Player10" 2750
          mkRatedPlayer "Player11" 2700
          mkRatedPlayer "Player12" 2650
          mkRatedPlayer "Player13" 2600
          mkRatedPlayer "Player14" 2550
          mkRatedPlayer "Player15" 2500
          mkRatedPlayer "Player16" 2450 ]
    
    let scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    
    let priorPairs = Set.empty
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(8, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>(
        [ "Player8-Player16"; "Player7-Player15"; "Player6-Player14"; "Player5-Player13"
          "Player4-Player12"; "Player3-Player11"; "Player2-Player10"; "Player1-Player9" ], 
        pairNames)

[<Fact>]
let ``swiss tournament with 16 players round 2 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3150
          mkRatedPlayer "Player3" 3100
          mkRatedPlayer "Player4" 3050
          mkRatedPlayer "Player5" 3000
          mkRatedPlayer "Player6" 2950
          mkRatedPlayer "Player7" 2900
          mkRatedPlayer "Player8" 2850
          mkRatedPlayer "Player9" 2800
          mkRatedPlayer "Player10" 2750
          mkRatedPlayer "Player11" 2700
          mkRatedPlayer "Player12" 2650
          mkRatedPlayer "Player13" 2600
          mkRatedPlayer "Player14" 2550
          mkRatedPlayer "Player15" 2500
          mkRatedPlayer "Player16" 2450 ]
    
    let scores =
        [ "Player1", 1.0; "Player2", 1.0; "Player3", 1.0; "Player4", 1.0
          "Player5", 1.0; "Player6", 1.0; "Player7", 1.0; "Player8", 1.0
          "Player9", 0.0; "Player10", 0.0; "Player11", 0.0; "Player12", 0.0
          "Player13", 0.0; "Player14", 0.0; "Player15", 0.0; "Player16", 0.0 ]
        |> Map.ofList
    
    let priorPairs = 
        Set.ofList [ 
            swissPairKey "Player1" "Player9"
            swissPairKey "Player2" "Player10"
            swissPairKey "Player3" "Player11"
            swissPairKey "Player4" "Player12"
            swissPairKey "Player5" "Player13"
            swissPairKey "Player6" "Player14"
            swissPairKey "Player7" "Player15"
            swissPairKey "Player8" "Player16"
        ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(8, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>(
        [ "Player12-Player16"; "Player11-Player15"; "Player10-Player14"; "Player9-Player13"
          "Player4-Player8"; "Player3-Player7"; "Player2-Player6"; "Player1-Player5" ], 
        pairNames)

[<Fact>]
let ``swiss tournament with 16 players round 3 pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3150
          mkRatedPlayer "Player3" 3100
          mkRatedPlayer "Player4" 3050
          mkRatedPlayer "Player5" 3000
          mkRatedPlayer "Player6" 2950
          mkRatedPlayer "Player7" 2900
          mkRatedPlayer "Player8" 2850
          mkRatedPlayer "Player9" 2800
          mkRatedPlayer "Player10" 2750
          mkRatedPlayer "Player11" 2700
          mkRatedPlayer "Player12" 2650
          mkRatedPlayer "Player13" 2600
          mkRatedPlayer "Player14" 2550
          mkRatedPlayer "Player15" 2500
          mkRatedPlayer "Player16" 2450 ]
    
    let scores =
        [ "Player1", 2.0; "Player2", 2.0; "Player3", 2.0; "Player4", 2.0
          "Player5", 1.0; "Player6", 1.0; "Player7", 1.0; "Player8", 1.0
          "Player9", 1.0; "Player10", 1.0; "Player11", 1.0; "Player12", 1.0
          "Player13", 0.0; "Player14", 0.0; "Player15", 0.0; "Player16", 0.0 ]
        |> Map.ofList
    
    let priorPairs = 
        Set.ofList [ 
            swissPairKey "Player1" "Player9"
            swissPairKey "Player2" "Player10"
            swissPairKey "Player3" "Player11"
            swissPairKey "Player4" "Player12"
            swissPairKey "Player5" "Player13"
            swissPairKey "Player6" "Player14"
            swissPairKey "Player7" "Player15"
            swissPairKey "Player8" "Player16"
            swissPairKey "Player1" "Player5"
            swissPairKey "Player2" "Player6"
            swissPairKey "Player3" "Player7"
            swissPairKey "Player4" "Player8"
            swissPairKey "Player9" "Player13"
            swissPairKey "Player10" "Player14"
            swissPairKey "Player11" "Player15"
            swissPairKey "Player12" "Player16"
        ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    
    Assert.Equal(8, pairs.Length)
    
    let pairNames = pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>(
        [ "Player14-Player16"; "Player13-Player15"; "Player8-Player12"; "Player7-Player11"
          "Player6-Player10"; "Player5-Player9"; "Player2-Player4"; "Player1-Player3" ], 
        pairNames)

[<Fact>]
let ``swiss tournament 16 players round 1-3 no repeated pairings with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3150
          mkRatedPlayer "Player3" 3100
          mkRatedPlayer "Player4" 3050
          mkRatedPlayer "Player5" 3000
          mkRatedPlayer "Player6" 2950
          mkRatedPlayer "Player7" 2900
          mkRatedPlayer "Player8" 2850
          mkRatedPlayer "Player9" 2800
          mkRatedPlayer "Player10" 2750
          mkRatedPlayer "Player11" 2700
          mkRatedPlayer "Player12" 2650
          mkRatedPlayer "Player13" 2600
          mkRatedPlayer "Player14" 2550
          mkRatedPlayer "Player15" 2500
          mkRatedPlayer "Player16" 2450 ]
    
    let seedOrder = tcecSeedOrder players 1
    
    let round1Scores =
        players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let round1Pairs = swissRoundPairings players seedOrder round1Scores Set.empty Set.empty
    
    let round1PriorPairs =
        round1Pairs
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
        |> Set.ofList
    
    let round2Scores =
        [ "Player1", 1.0; "Player2", 1.0; "Player3", 1.0; "Player4", 1.0
          "Player5", 1.0; "Player6", 1.0; "Player7", 1.0; "Player8", 1.0
          "Player9", 0.0; "Player10", 0.0; "Player11", 0.0; "Player12", 0.0
          "Player13", 0.0; "Player14", 0.0; "Player15", 0.0; "Player16", 0.0 ]
        |> Map.ofList
    let round2Pairs = swissRoundPairings players seedOrder round2Scores round1PriorPairs Set.empty
    
    let round2PriorPairs =
        round1PriorPairs
        |> Set.union (round2Pairs |> List.map (fun (a, b) -> swissPairKey a.Name b.Name) |> Set.ofList)
    
    let round3Scores =
        [ "Player1", 2.0; "Player2", 2.0; "Player3", 2.0; "Player4", 2.0
          "Player5", 1.0; "Player6", 1.0; "Player7", 1.0; "Player8", 1.0
          "Player9", 1.0; "Player10", 1.0; "Player11", 1.0; "Player12", 1.0
          "Player13", 0.0; "Player14", 0.0; "Player15", 0.0; "Player16", 0.0 ]
        |> Map.ofList
    let round3Pairs = swissRoundPairings players seedOrder round3Scores round2PriorPairs Set.empty
    
    let allPairs =
        round1Pairs @ round2Pairs @ round3Pairs
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
    
    let uniquePairs = allPairs |> Set.ofList
    
    Assert.Equal(allPairs.Length, uniquePairs.Count)
    Assert.Equal(24, uniquePairs.Count)

[<Fact>]
let ``swiss tournament 16 players round 1 correct pairing order with seed group 1`` () =
    let players =
        [ mkRatedPlayer "Player1" 3200
          mkRatedPlayer "Player2" 3150
          mkRatedPlayer "Player3" 3100
          mkRatedPlayer "Player4" 3050
          mkRatedPlayer "Player5" 3000
          mkRatedPlayer "Player6" 2950
          mkRatedPlayer "Player7" 2900
          mkRatedPlayer "Player8" 2850
          mkRatedPlayer "Player9" 2800
          mkRatedPlayer "Player10" 2750
          mkRatedPlayer "Player11" 2700
          mkRatedPlayer "Player12" 2650
          mkRatedPlayer "Player13" 2600
          mkRatedPlayer "Player14" 2550
          mkRatedPlayer "Player15" 2500
          mkRatedPlayer "Player16" 2450 ]
    
    let seedOrder = tcecSeedOrder players 1
    let seedOrderNames = seedOrder |> List.map (fun p -> p.Name)
    
    Assert.Equal<string list>(
        [ "Player1"; "Player2"; "Player3"; "Player4"; "Player5"; "Player6"; "Player7"; "Player8"
          "Player9"; "Player10"; "Player11"; "Player12"; "Player13"; "Player14"; "Player15"; "Player16" ],
        seedOrderNames)
    
    let scores = players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    
    Assert.Equal(8, pairs.Length)
