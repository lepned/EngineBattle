module CupBracketTests

open System
open System.Collections.Generic
open System.IO
open Xunit
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef.Cup
open ChessLibrary.Utilities.PairingHelper
open System.Text.Json

let private mkEngine name =
    { EngineConfig.Empty with Name = name }

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

let private mkRatedPlayer name rating =
    { mkEngine name with Rating = rating }

let private mkCupMatch roundNumber (playerA: EngineConfig) (playerB: EngineConfig) : CupMatch =
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
      Games = ResizeArray<CupGame>()
      OpeningOrder = ResizeArray<int>() }

[<Fact>]
let ``cup bracket persists randomized opening order`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"cup_bracket_{Guid.NewGuid()}.json")
    try
        let openings = [ mkOpening 1; mkOpening 2; mkOpening 3 ]
        let globalOrder = ResizeArray<int>([ 2; 0; 1 ])
        let matchOrder = ResizeArray<int>([ 1; 2; 0 ])
        let matchInfo =
            { MatchId = 1
              RoundNumber = 1
              PlayerA = "A"
              PlayerB = "B"
              PlayerARating = 3000
              PlayerBRating = 2900
              ScoreA = 0.0
              ScoreB = 0.0
              Winner = None
              IsDecided = false
              Games = ResizeArray<CupGame>()
              OpeningOrder = matchOrder }
        let round = { RoundNumber = 1; Matches = ResizeArray<CupMatch>([ matchInfo ]) }
        let bracket =
            { TournamentName = "Test"
              Strategy = "Random"
              GamesPerMatch = 2
              UniqueOpeningsGlobal = true
              NextOpeningIndex = 0
              GlobalOpeningOrder = globalOrder
              Rounds = ResizeArray<CupRound>([ round ])
              UpdatedUtc = DateTime.UtcNow }

        let optionsWrite = JsonSerializerOptions(WriteIndented = true)
        let optionsRead = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let json = JsonSerializer.Serialize(bracket, optionsWrite)
        File.WriteAllText(tempPath, json)
        let loaded =
            File.ReadAllText(tempPath)
            |> fun contents -> JsonSerializer.Deserialize<CupBracket>(contents, optionsRead)
        Assert.False(obj.ReferenceEquals(loaded, null))
        let loadedBracket = loaded
        Assert.Equal<int list>(Seq.toList globalOrder, loadedBracket.GlobalOpeningOrder |> Seq.toList)
        let loadedMatch = loadedBracket.Rounds.[0].Matches.[0]
        Assert.Equal<int list>(Seq.toList matchOrder, loadedMatch.OpeningOrder |> Seq.toList)

        let expectedGlobal =
            globalOrder
            |> Seq.map (fun idx -> (openings |> List.item idx).Raw)
            |> Seq.toList
        let loadedGlobal =
            loadedBracket.GlobalOpeningOrder
            |> Seq.map (fun idx -> (openings |> List.item idx).Raw)
            |> Seq.toList
        Assert.Equal<string list>(expectedGlobal, loadedGlobal)
    finally
        if File.Exists tempPath then
            File.Delete tempPath

[<Fact>]
let ``cup draw by rating does not pair seed 1 vs seed 2 in round one`` () =
    let players =
        [ { mkEngine "A" with Rating = 3200 }
          { mkEngine "B" with Rating = 2800 }
          { mkEngine "C" with Rating = 2600 }
          { mkEngine "D" with Rating = 3000 } ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let pairs = List.zip top (List.rev bottom)
    let pairSets = pairs |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
    Assert.DoesNotContain(Set.ofList [ "A"; "D" ], pairSets)

[<Fact>]
let ``cup draw by rating includes all players`` () =
    let players =
        [ { mkEngine "A" with Rating = 3200 }
          { mkEngine "B" with Rating = 2800 }
          { mkEngine "C" with Rating = 2600 }
          { mkEngine "D" with Rating = 3000 } ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let pairs = List.zip top (List.rev bottom)
    let allNames = pairs |> List.collect (fun (a, b) -> [ a.Name; b.Name ]) |> Set.ofList
    Assert.Equal(4, allNames.Count)

[<Fact>]
let ``cup auto seed bands for 8 players`` () =
    let bands = autoSeedBands 8
    Assert.Equal<int list list>([ [1]; [2]; [3; 4]; [5; 6; 7; 8] ], bands)

[<Fact>]
let ``cup auto seed bands for 16 players`` () =
    let bands = autoSeedBands 16
    Assert.Equal<int list list>([ [1]; [2]; [3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12; 13; 14; 15; 16] ], bands)

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
    let openingHash = ChessLibrary.Utilities.Hash.computeOpeningHashFromGame openings.[0]
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
            ChessLibrary.Utilities.Hash.computeOpeningHashFromGame o)
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
            results.Add(String.Join("|", bandOrder)) |> ignore
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

[<Fact>]
let ``cup seed order for 4 players produces standard bracket`` () =
    let order = seedOrder 4
    // Adjacent pairs should be 1v4 and 2v3 (top seeds separated)
    Assert.Equal<int list>([ 1; 4; 2; 3 ], order)

[<Fact>]
let ``cup seed order for 8 players separates top seeds`` () =
    let order = seedOrder 8
    Assert.Equal<int list>([ 1; 8; 4; 5; 2; 7; 3; 6 ], order)
    // Adjacent pairs: (1,8), (4,5), (2,7), (3,6) — proper bracket seeding
    // Seeds 1 and 2 are in different halves (indices 0 and 4)
    let firstHalf = order |> List.take 4 |> Set.ofList
    Assert.Contains(1, firstHalf)
    Assert.DoesNotContain(2, firstHalf)

[<Fact>]
let ``cup seeded pairs for 8 players places seed 1 and seed 2 in opposite halves`` () =
    let players =
        [ for i in 1..8 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let topNames = seeded |> List.take half |> List.map (fun p -> p.Name) |> Set.ofList
    let bottomNames = seeded |> List.skip half |> List.map (fun p -> p.Name) |> Set.ofList
    // Seed 1 (P1) and seed 2 (P2) must be in different halves
    Assert.True(
        (topNames.Contains "P1" && bottomNames.Contains "P2") ||
        (topNames.Contains "P2" && bottomNames.Contains "P1"))

[<Fact>]
let ``cup seeded pairs for 16 players avoid top 4 meeting before semifinals`` () =
    let players =
        [ for i in 1..16 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let pairs = List.zip top (List.rev bottom)
    let pairSets = pairs |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ]) |> Set.ofList
    // Top 4 seeds should never face each other in round 1
    Assert.DoesNotContain(Set.ofList [ "P1"; "P2" ], pairSets)
    Assert.DoesNotContain(Set.ofList [ "P1"; "P3" ], pairSets)
    Assert.DoesNotContain(Set.ofList [ "P1"; "P4" ], pairSets)
    Assert.DoesNotContain(Set.ofList [ "P2"; "P3" ], pairSets)
    Assert.DoesNotContain(Set.ofList [ "P2"; "P4" ], pairSets)
    Assert.DoesNotContain(Set.ofList [ "P3"; "P4" ], pairSets)

[<Fact>]
let ``cup bracket advancement maps match index to correct next round slot`` () =
    // Matches 0,1 feed into next-round match 0; matches 2,3 feed into match 1
    // Even-index match winner goes to PlayerA, odd-index to PlayerB
    let round1 = ResizeArray<CupMatch>()
    let round2 = ResizeArray<CupMatch>()
    let players = [ for i in 1..4 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    for i in 0..1 do
        let a = players.[i * 2]
        let b = players.[i * 2 + 1]
        let m = { mkCupMatch 1 a b with MatchId = i + 1 }
        m.Winner <- Some a.Name
        m.IsDecided <- true
        round1.Add m
    let emptyA = mkRatedPlayer "TBD" 0
    let emptyB = mkRatedPlayer "TBD" 0
    round2.Add(mkCupMatch 2 emptyA emptyB)
    // Simulate bracket advancement: match 0 winner -> PlayerA, match 1 winner -> PlayerB
    for matchIndex in 0..1 do
        let winner = round1.[matchIndex].Winner.Value
        let nextIndex = matchIndex / 2
        let nextMatch = round2.[nextIndex]
        if matchIndex % 2 = 0 then
            round2.[nextIndex] <- { nextMatch with PlayerA = winner }
        else
            round2.[nextIndex] <- { nextMatch with PlayerB = winner }
    Assert.Equal("P1", round2.[0].PlayerA)
    Assert.Equal("P3", round2.[0].PlayerB)

[<Fact>]
let ``cup full 8 player bracket with highest seed advancing`` () =
    let players =
        [ for i in 1..8 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let round1Pairs = List.zip top (List.rev bottom)

    // Round 1: 4 matches. Higher seed (lower number) always wins.
    let round1Winners =
        round1Pairs
        |> List.map (fun (a, b) ->
            if a.Rating >= b.Rating then a else b)

    // Verify all 8 players appear in round 1
    let round1AllNames =
        round1Pairs |> List.collect (fun (a, b) -> [ a.Name; b.Name ]) |> Set.ofList
    Assert.Equal(8, round1AllNames.Count)

    // P1 and P2 must NOT face each other in round 1
    let round1PairSets = round1Pairs |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
    Assert.DoesNotContain(Set.ofList [ "P1"; "P2" ], round1PairSets)

    // Round 2 (semifinals): pair winners using bracket advancement (matchIndex / 2)
    Assert.Equal(4, round1Winners.Length)
    let round2Pairs =
        [ for i in 0..1 ->
            let matchA = round1Winners.[i * 2]
            let matchB = round1Winners.[i * 2 + 1]
            (matchA, matchB) ]
    let round2Winners =
        round2Pairs
        |> List.map (fun (a, b) ->
            if a.Rating >= b.Rating then a else b)

    // P1 and P2 must NOT face each other in the semifinals
    let round2PairSets = round2Pairs |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
    Assert.DoesNotContain(Set.ofList [ "P1"; "P2" ], round2PairSets)

    // Round 3 (final): the two semifinal winners
    Assert.Equal(2, round2Winners.Length)
    let finalPair = (round2Winners.[0], round2Winners.[1])
    let champion =
        if (fst finalPair).Rating >= (snd finalPair).Rating then fst finalPair else snd finalPair

    // With highest seed always advancing, P1 and P2 meet in the final
    Assert.Equal<Set<string>>(Set.ofList [ "P1"; "P2" ], Set.ofList [ (fst finalPair).Name; (snd finalPair).Name ])
    Assert.Equal("P1", champion.Name)

[<Fact>]
let ``cup games per match increases with round pair increments`` () =
    // Round 1: 1 pair = 2 games, Round 2: 2 pairs = 4 games, Round 3 (final): 3 pairs = 6 games
    let increments = [ 1; 2; 3 ]
    Assert.Equal(2, gamesPerMatchForRound 2 increments 1)
    Assert.Equal(4, gamesPerMatchForRound 2 increments 2)
    Assert.Equal(6, gamesPerMatchForRound 2 increments 3)
    // Beyond configured rounds, last value is reused
    Assert.Equal(6, gamesPerMatchForRound 2 increments 5)

[<Fact>]
let ``cup games per match defaults to base when no increments`` () =
    Assert.Equal(2, gamesPerMatchForRound 2 [] 1)
    Assert.Equal(2, gamesPerMatchForRound 2 [] 3)
    // Odd base gets rounded up to even
    Assert.Equal(4, gamesPerMatchForRound 3 [] 1)
    // Below 2 gets clamped to 2
    Assert.Equal(2, gamesPerMatchForRound 1 [] 1)

[<Fact>]
let ``cup tiebreak cycles openings when all are exhausted`` () =
    let openings = [ mkOpening 1; mkOpening 2 ]
    let allUsed =
        openings
        |> Seq.map (fun o -> ChessLibrary.Utilities.Hash.computeOpeningHashFromGame o)
        |> Set.ofSeq
    // When all openings used, should wrap around using modular index
    let idx = 3
    let wrappedIdx = idx % openings.Length
    Assert.Equal(1, wrappedIdx)
    // nextUnusedOpeningIndex falls back when all hashes are used
    let resultIdx = nextUnusedOpeningIndex allUsed openings 0
    // All are used, so it should return the localOpeningIndex mod length
    Assert.Equal(0, resultIdx)

[<Fact>]
let ``cup bracket persists match results through json roundtrip`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"cup_test_{Guid.NewGuid()}.json")
    try
        let game : CupGame =
            { GameNr = 1; White = "A"; Black = "B"; OpeningId = "1"; OpeningHash = "abc"; Result = "1-0" }
        let matchInfo : CupMatch =
            { MatchId = 1; RoundNumber = 1
              PlayerA = "A"; PlayerB = "B"
              PlayerARating = 3000; PlayerBRating = 2900
              ScoreA = 1.5; ScoreB = 0.5
              Winner = Some "A"; IsDecided = true
              Games = ResizeArray<CupGame>([ game ])
              OpeningOrder = ResizeArray<int>([ 0 ]) }
        let round : CupRound = { RoundNumber = 1; Matches = ResizeArray<CupMatch>([ matchInfo ]) }
        let bracket : CupBracket =
            { TournamentName = "Test"; Strategy = "ByRating"; GamesPerMatch = 2
              UniqueOpeningsGlobal = false; NextOpeningIndex = 1
              GlobalOpeningOrder = ResizeArray<int>()
              Rounds = ResizeArray<CupRound>([ round ])
              UpdatedUtc = DateTime.UtcNow }
        let opts = JsonSerializerOptions(WriteIndented = true)
        let json = JsonSerializer.Serialize(bracket, opts)
        File.WriteAllText(tempPath, json)
        let readOpts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let loaded = JsonSerializer.Deserialize<CupBracket>(File.ReadAllText(tempPath), readOpts)
        let m = loaded.Rounds.[0].Matches.[0]
        Assert.Equal(1.5, m.ScoreA)
        Assert.Equal(0.5, m.ScoreB)
        Assert.Equal(Some "A", m.Winner)
        Assert.True(m.IsDecided)
        Assert.Equal(1, m.Games.Count)
        Assert.Equal("1-0", m.Games.[0].Result)
    finally
        if File.Exists tempPath then File.Delete tempPath

[<Fact>]
let ``cup match decided early when lead is insurmountable`` () =
    // With 6 games per match, if PlayerA leads 4-0 after 4 games,
    // PlayerB cannot catch up (max 2 remaining = 2 points, 4 > 0 + 2)
    let playerA = mkRatedPlayer "A" 3000
    let playerB = mkRatedPlayer "B" 2900
    let matchInfo = mkCupMatch 1 playerA playerB
    matchInfo.ScoreA <- 4.0
    matchInfo.ScoreB <- 0.0
    let gamesRemaining = 2
    let isInsurmountable = matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining
    Assert.True(isInsurmountable)

[<Fact>]
let ``cup match not decided early when comeback possible`` () =
    let playerA = mkRatedPlayer "A" 3000
    let playerB = mkRatedPlayer "B" 2900
    let matchInfo = mkCupMatch 1 playerA playerB
    matchInfo.ScoreA <- 2.0
    matchInfo.ScoreB <- 1.0
    let gamesRemaining = 3
    let isInsurmountable = matchInfo.ScoreA > matchInfo.ScoreB + float gamesRemaining
    Assert.False(isInsurmountable)

[<Fact>]
let ``cup seed order for 16 players separates top 4 into quarters`` () =
    let order = seedOrder 16
    Assert.Equal(16, order.Length)
    // Seeds 1 and 2 in different halves
    let firstHalf = order |> List.take 8 |> Set.ofList
    Assert.Contains(1, firstHalf)
    Assert.DoesNotContain(2, firstHalf)
    // Seeds 1-4 each in a different quarter
    let q1 = order |> List.take 4 |> Set.ofList
    let q2 = order |> List.skip 4 |> List.take 4 |> Set.ofList
    let q3 = order |> List.skip 8 |> List.take 4 |> Set.ofList
    let q4 = order |> List.skip 12 |> List.take 4 |> Set.ofList
    let topSeeds = [ 1; 2; 3; 4 ]
    let quartersWithTopSeed =
        [ q1; q2; q3; q4 ]
        |> List.filter (fun q -> topSeeds |> List.exists (fun s -> q.Contains s))
    Assert.Equal(4, quartersWithTopSeed.Length)

[<Fact>]
let ``cup auto seed bands for 4 players`` () =
    let bands = autoSeedBands 4
    Assert.Equal<int list list>([ [1]; [2]; [3; 4] ], bands)

[<Fact>]
let ``cup auto seed bands for 32 players`` () =
    let bands = autoSeedBands 32
    Assert.Equal<int list list>([ [1]; [2]; [3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12; 13; 14; 15; 16]; [17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32] ], bands)

[<Fact>]
let ``cup seedByBands without randomization is deterministic`` () =
    let players =
        [ for i in 1..8 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    let bands = autoSeedBands players.Length
    let run1 = seedByBands players bands false |> List.map (fun p -> p.Name)
    let run2 = seedByBands players bands false |> List.map (fun p -> p.Name)
    Assert.Equal<string list>(run1, run2)

[<Fact>]
let ``cup full 4 player bracket with highest seed advancing`` () =
    let players =
        [ for i in 1..4 -> mkRatedPlayer $"P{i}" (4000 - i * 100) ]
    let bands = autoSeedBands players.Length
    let seeded = seedByBands players bands false
    let half = seeded.Length / 2
    let top, bottom = seeded |> List.splitAt half
    let round1Pairs = List.zip top (List.rev bottom)
    // P1 and P2 should not face each other in round 1
    let round1PairSets = round1Pairs |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
    Assert.DoesNotContain(Set.ofList [ "P1"; "P2" ], round1PairSets)
    // Both matches produce winners (highest seed)
    let winners =
        round1Pairs
        |> List.map (fun (a, b) -> if a.Rating >= b.Rating then a else b)
    // Final: should be P1 vs P2
    Assert.Equal(2, winners.Length)
    Assert.Equal<Set<string>>(Set.ofList [ "P1"; "P2" ], winners |> List.map (fun p -> p.Name) |> Set.ofList)
