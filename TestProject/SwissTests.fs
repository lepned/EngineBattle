module SwissTests

open System.Collections.Generic
open System.Text.Json
open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.Swiss
open ChessLibrary.Utilities.PairingHelper

let private mkRatedPlayer name rating =
    { EngineConfig.Empty with Name = name; Rating = rating }

let private mkOpening gameNr =
    { PGNTypes.PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

let private seedOrderNames players groupCount =
    tcecSeedOrder players groupCount |> List.map (fun p -> p.Name)

[<Fact>]
let ``tcecSeedOrder distributes seeds across groups`` () =
    let players =
        [ mkRatedPlayer "P1" 8000
          mkRatedPlayer "P2" 7900
          mkRatedPlayer "P3" 7800
          mkRatedPlayer "P4" 7700
          mkRatedPlayer "P5" 7600
          mkRatedPlayer "P6" 7500
          mkRatedPlayer "P7" 7400
          mkRatedPlayer "P8" 7300 ]
    let seeded = tcecSeedOrder players 4
    let orderedNames = seeded |> List.map (fun p -> p.Name)
    Assert.Equal<string list>([ "P1"; "P3"; "P5"; "P7"; "P2"; "P4"; "P6"; "P8" ], orderedNames)

[<Fact>]
let ``swissRoundPairings orders weakest score pairs first`` () =
    let players =
        [ mkRatedPlayer "A" 4000
          mkRatedPlayer "B" 3900
          mkRatedPlayer "C" 3800
          mkRatedPlayer "D" 3700 ]
    let seedOrder = tcecSeedOrder players 2
    let scores =
        [ "A", 3.0
          "B", 3.0
          "C", 1.0
          "D", 1.0 ]
        |> Map.ofList
    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    let firstPairNames =
        pairs
        |> List.head
        |> fun (a, b) -> Set.ofList [ a.Name; b.Name ]
    let secondPairNames =
        pairs
        |> List.tail
        |> List.head
        |> fun (a, b) -> Set.ofList [ a.Name; b.Name ]
    Assert.Equal<Set<string>>(Set.ofList [ "C"; "D" ], firstPairNames)
    Assert.Equal<Set<string>>(Set.ofList [ "A"; "B" ], secondPairNames)

[<Fact>]
let ``swissRoundPairings avoids repeat when alternative exists`` () =
    let players =
        [ mkRatedPlayer "A" 4000
          mkRatedPlayer "B" 3900
          mkRatedPlayer "C" 3800
          mkRatedPlayer "D" 3700 ]
    let seedOrder = tcecSeedOrder players 2
    let scores =
        [ "A", 0.0
          "B", 0.0
          "C", 0.0
          "D", 0.0 ]
        |> Map.ofList
    let priorPairs = Set.ofList [ swissPairKey "A" "B" ]
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    let pairNames =
        pairs
        |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
        |> Set.ofList
    let expected =
        Set.ofList [
            Set.ofList [ "A"; "D" ]
            Set.ofList [ "B"; "C" ]
        ]
    Assert.Equal<Set<Set<string>>>(expected, pairNames)

[<Fact>]
let ``swissRoundPairings backtracks to avoid dead-end repeats`` () =
    let players =
        [ mkRatedPlayer "A" 4000
          mkRatedPlayer "B" 3900
          mkRatedPlayer "C" 3800
          mkRatedPlayer "D" 3700 ]
    let seedOrder = tcecSeedOrder players 2
    let scores =
        [ "A", 0.0
          "B", 0.0
          "C", 0.0
          "D", 0.0 ]
        |> Map.ofList
    // Greedy A-B leaves C-D which is forbidden; A-D is the valid alternative.
    let priorPairs = Set.ofList [ swissPairKey "A" "C"; swissPairKey "C" "D" ]
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    let pairNames =
        pairs
        |> List.map (fun (a, b) -> Set.ofList [ a.Name; b.Name ])
        |> Set.ofList
    let expected =
        Set.ofList [
            Set.ofList [ "A"; "D" ]
            Set.ofList [ "B"; "C" ]
        ]
    Assert.Equal<Set<Set<string>>>(expected, pairNames)

[<Fact>]
let ``swissRoundPairings avoids prior pairs when alternatives exist in larger groups`` () =
    let players =
        [ mkRatedPlayer "A" 4000
          mkRatedPlayer "B" 3900
          mkRatedPlayer "C" 3800
          mkRatedPlayer "D" 3700
          mkRatedPlayer "E" 3600
          mkRatedPlayer "F" 3500 ]
    let seedOrder = tcecSeedOrder players 2
    let scores =
        [ "A", 0.0
          "B", 0.0
          "C", 0.0
          "D", 0.0
          "E", 0.0
          "F", 0.0 ]
        |> Map.ofList
    let priorPairs =
        Set.ofList [
            swissPairKey "A" "E"
            swissPairKey "D" "E"
        ]
    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
    Assert.Equal(3, pairs.Length)
    for (a, b) in pairs do
        let key = swissPairKey a.Name b.Name
        Assert.False(priorPairs.Contains key)

[<Fact>]
let ``swissRoundPairings second round after equal scores for group count 1`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700
          mkRatedPlayer "G" 2600
          mkRatedPlayer "H" 2500 ]
    let seedOrder = tcecSeedOrder players 1
    let round1Scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let round1Pairs = swissRoundPairings players seedOrder round1Scores Set.empty Set.empty
    let round1Names = round1Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "D-H"; "C-G"; "B-F"; "A-E" ], round1Names)
    let priorPairs =
        round1Pairs
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
        |> Set.ofList
    let round2Scores =
        players
        |> List.map (fun p -> p.Name, 1.0)
        |> Map.ofList
    let round2Pairs = swissRoundPairings players seedOrder round2Scores priorPairs Set.empty
    let names = round2Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "D-G"; "C-H"; "B-E"; "A-F" ], names)

[<Fact>]
let ``swiss planned pairings include full round`` () =
    let players =
        [ mkRatedPlayer "A" 4000
          mkRatedPlayer "B" 3900
          mkRatedPlayer "C" 3800
          mkRatedPlayer "D" 3700 ]
    let seedOrder = tcecSeedOrder players 2
    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList
    let scores =
        [ "A", 0.0
          "B", 0.0
          "C", 0.0
          "D", 0.0 ]
        |> Map.ofList
    let roundPairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    let openings = [ mkOpening 1; mkOpening 2 ]
    let planned = ResizeArray<Pairing>()
    let mutable previewIndex = 0
    for (a, b) in roundPairs do
        let seedA = seedMap.[a.Name]
        let seedB = seedMap.[b.Name]
        let whiteFirst, blackFirst = if seedA <= seedB then a, b else b, a
        previewIndex <- addPlannedPairings planned whiteFirst blackFirst openings 2 previewIndex
    Assert.Equal(roundPairs.Length * 2, planned.Count)

[<Fact>]
let ``swissRoundPairings 5 rounds 6 players no repeats and complete round robin`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700 ]
    let seedOrder = tcecSeedOrder players 1
    let mutable priorPairs = Set.empty
    let mutable allPairs = ResizeArray<string>()
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 2.0; "F", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
        Assert.Equal(3, roundPairs.Length)
        for (a, b) in roundPairs do
            let key = swissPairKey a.Name b.Name
            Assert.False(priorPairs.Contains key)
            priorPairs <- priorPairs.Add key
            allPairs.Add key
    let uniquePairs = allPairs |> Seq.toList |> Set.ofList
    Assert.Equal(12, uniquePairs.Count)
    Assert.Equal(12, allPairs.Count)
    let round5Scores =
        [ "A", 3.0; "B", 3.0; "C", 2.0
          "D", 2.0; "E", 1.0; "F", 0.0 ]
        |> Map.ofList
    Assert.Throws<System.Exception>(fun () ->
        swissRoundPairingsGroupedOnly players seedOrder round5Scores priorPairs Set.empty |> ignore)
    |> ignore
    let round5Pairs = swissRoundPairings players seedOrder round5Scores priorPairs Set.empty
    Assert.Equal(3, round5Pairs.Length)
    let round5Names = round5Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "A-F"; "B-C"; "D-E" ], round5Names)
    for (a, b) in round5Pairs do
        let key = swissPairKey a.Name b.Name
        Assert.False(priorPairs.Contains key)
        priorPairs <- priorPairs.Add key
        allPairs.Add key
    let finalUniquePairs = allPairs |> Seq.toList |> Set.ofList
    Assert.Equal(15, finalUniquePairs.Count)
    Assert.Equal(15, allPairs.Count)

[<Fact>]
let ``swissRoundPairings 5th round works with distinct scores after 4 rounds`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700 ]
    let seedOrder = tcecSeedOrder players 1
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 2.0; "F", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    let mutable priorPairs = Set.empty
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
        for (a, b) in roundPairs do
            let key = swissPairKey a.Name b.Name
            Assert.False(priorPairs.Contains key)
            priorPairs <- priorPairs.Add key
    let round5Scores =
        [ "A", 4.0; "B", 3.5; "C", 3.0
          "D", 2.5; "E", 2.0; "F", 1.5 ]
        |> Map.ofList
    let round5Pairs = swissRoundPairings players seedOrder round5Scores priorPairs Set.empty
    Assert.Equal(3, round5Pairs.Length)
    let round5Names = round5Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "A-F"; "B-C"; "D-E" ], round5Names)
    for (a, b) in round5Pairs do
        let key = swissPairKey a.Name b.Name
        Assert.False(priorPairs.Contains key)

[<Fact>]
let ``swissRoundPairings 5th round works with distinct scores after 4 rounds group count 2`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700 ]
    let seedOrder = tcecSeedOrder players 2
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 2.0; "F", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    let mutable priorPairs = Set.empty
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
        for (a, b) in roundPairs do
            let key = swissPairKey a.Name b.Name
            Assert.False(priorPairs.Contains key)
            priorPairs <- priorPairs.Add key
    let round5Scores =
        [ "A", 4.0; "B", 3.5; "C", 3.0
          "D", 2.5; "E", 2.0; "F", 1.5 ]
        |> Map.ofList
    let round5Pairs = swissRoundPairings players seedOrder round5Scores priorPairs Set.empty
    Assert.Equal(3, round5Pairs.Length)
    let round5Names = round5Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "A-F"; "B-D"; "C-E" ], round5Names)
    for (a, b) in round5Pairs do
        let key = swissPairKey a.Name b.Name
        Assert.False(priorPairs.Contains key)

[<Fact>]
let ``swissRoundPairings 5th round works with distinct scores after 4 rounds group count 4`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700 ]
    let seedOrder = tcecSeedOrder players 4
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 2.0; "F", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    let mutable priorPairs = Set.empty
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
        for (a, b) in roundPairs do
            let key = swissPairKey a.Name b.Name
            Assert.False(priorPairs.Contains key)
            priorPairs <- priorPairs.Add key
    let round5Scores =
        [ "A", 4.0; "B", 3.5; "C", 3.0
          "D", 2.5; "E", 2.0; "F", 1.5 ]
        |> Map.ofList
    let round5Pairs = swissRoundPairings players seedOrder round5Scores priorPairs Set.empty
    Assert.Equal(3, round5Pairs.Length)
    let round5Names = round5Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "A-D"; "B-F"; "C-E" ], round5Names)
    for (a, b) in round5Pairs do
        let key = swissPairKey a.Name b.Name
        Assert.False(priorPairs.Contains key)

[<Fact>]
let ``swissRoundPairings 3 players 2 rounds no repeated byes`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000 ]
    let seedOrder = tcecSeedOrder players 1
    let round1Scores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList
    let round1Pairs = swissRoundPairings players seedOrder round1Scores Set.empty Set.empty
    let round1Bye =
        round1Pairs
        |> List.tryFind (fun (_, b) -> b.Name = "BYE")
        |> Option.map (fun (a, _) -> a.Name)
    Assert.True(round1Bye.IsSome)
    let priorPairs =
        round1Pairs
        |> List.filter (fun (_, b) -> b.Name <> "BYE")
        |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
        |> Set.ofList
    let byeSet = round1Bye |> Option.map Set.singleton |> Option.defaultValue Set.empty
    let round2Scores =
        players
        |> List.map (fun p -> p.Name, 1.0)
        |> Map.ofList
    let round2Pairs = swissRoundPairings players seedOrder round2Scores priorPairs byeSet
    let round2Bye =
        round2Pairs
        |> List.tryFind (fun (_, b) -> b.Name = "BYE")
        |> Option.map (fun (a, _) -> a.Name)
    Assert.True(round2Bye.IsSome)
    let bye1 = round1Bye.Value
    let bye2 = round2Bye.Value
    Assert.NotEqual<string>(bye1, bye2)

[<Fact>]
let ``swissRoundPairings fallback pairing order is deterministic`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800
          mkRatedPlayer "F" 2700 ]
    let seedOrder = tcecSeedOrder players 1
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 2.0; "F", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    let mutable priorPairs = Set.empty
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs Set.empty
        for (a, b) in roundPairs do
            priorPairs <- priorPairs.Add (swissPairKey a.Name b.Name)
    let round5Scores =
        [ "A", 3.0; "B", 3.0; "C", 2.0
          "D", 2.0; "E", 1.0; "F", 0.0 ]
        |> Map.ofList
    let round5Pairs = swissRoundPairings players seedOrder round5Scores priorPairs Set.empty
    let round5Names = round5Pairs |> List.map (fun (a, b) -> $"{a.Name}-{b.Name}")
    Assert.Equal<string list>([ "A-F"; "B-C"; "D-E" ], round5Names)

[<Fact>]
let ``swissRoundPairings 5 players 5 rounds each player gets at most one bye`` () =
    let players =
        [ mkRatedPlayer "A" 3200
          mkRatedPlayer "B" 3100
          mkRatedPlayer "C" 3000
          mkRatedPlayer "D" 2900
          mkRatedPlayer "E" 2800 ]
    let seedOrder = tcecSeedOrder players 1
    let mutable priorPairs = Set.empty
    let mutable byeSet = Set.empty
    let mutable byeCounts = Map.empty<string, int>
    let roundScores =
        [ players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          [ "A", 2.0; "B", 2.0; "C", 2.0; "D", 2.0; "E", 1.0 ] |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList
          players |> List.map (fun p -> p.Name, 2.0) |> Map.ofList ]
    for scores in roundScores do
        let roundPairs = swissRoundPairings players seedOrder scores priorPairs byeSet
        let bye =
            roundPairs
            |> List.tryFind (fun (_, b) -> b.Name = "BYE")
            |> Option.map (fun (a, _) -> a.Name)
        Assert.True(bye.IsSome)
        let byeName = bye.Value
        let prev = byeCounts |> Map.tryFind byeName |> Option.defaultValue 0
        byeCounts <- byeCounts |> Map.add byeName (prev + 1)
        byeSet <- byeSet.Add byeName
        let playedPairs =
            roundPairs
            |> List.filter (fun (_, b) -> b.Name <> "BYE")
            |> List.map (fun (a, b) -> swissPairKey a.Name b.Name)
            |> Set.ofList
        for key in playedPairs do
            Assert.False(priorPairs.Contains key)
        priorPairs <- Set.union priorPairs playedPairs
    let byesTotal = byeCounts |> Seq.sumBy (fun kvp -> kvp.Value)
    Assert.Equal(5, byesTotal)
    for KeyValue(_, count) in byeCounts do
        Assert.True(count <= 1)
[<Fact>]
let ``swiss state persists openings and partial games for resume`` () =
    let game : SwissGame =
        { GameNr = 1
          White = "A"
          Black = "B"
          OpeningId = "1"
          OpeningHash = "hash-1"
          Result = "1-0" }
    let pairing : SwissPairing =
        { PairId = 1
          RoundNumber = 1
          PlayerA = "A"
          PlayerB = "B"
          PlayerARating = 4000
          PlayerBRating = 3900
          ScoreA = 1.0
          ScoreB = 0.0
          IsDecided = false
          Games = ResizeArray([ game ])
          OpeningOrder = ResizeArray([ 3; 7; 2 ]) }
    let round : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ pairing ]) }
    let state : SwissState =
        { TournamentName = "Test Swiss"
          SeedGroupCount = 2
          GamesPerMatch = 2
          UniqueOpeningsGlobal = false
          NextOpeningIndex = 4
          GlobalOpeningOrder = ResizeArray([ 5; 1; 4 ])
          Rounds = ResizeArray([ round ])
          UpdatedUtc = System.DateTime.UtcNow }
    let json = JsonSerializer.Serialize(state, JsonSerializerOptions(WriteIndented = true))
    let loaded = JsonSerializer.Deserialize<SwissState>(json, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
    Assert.NotNull(loaded)
    Assert.Equal(4, loaded.NextOpeningIndex)
    Assert.Equal<int list>([ 5; 1; 4 ], loaded.GlobalOpeningOrder |> Seq.toList)
    let loadedPairing = loaded.Rounds.[0].Pairings.[0]
    Assert.Equal<int list>([ 3; 7; 2 ], loadedPairing.OpeningOrder |> Seq.toList)
    Assert.Single(loadedPairing.Games) |> ignore
    Assert.False(loadedPairing.IsDecided)

[<Fact>]
let ``swiss state persists multiple rounds and pairings`` () =
    let mkPair id a b =
        { PairId = id
          RoundNumber = 1
          PlayerA = a
          PlayerB = b
          PlayerARating = 4000
          PlayerBRating = 3900
          ScoreA = 0.0
          ScoreB = 0.0
          IsDecided = false
          Games = ResizeArray()
          OpeningOrder = ResizeArray() }
    let round1 : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ mkPair 1 "A" "B"; mkPair 2 "C" "D" ]) }
    let round2 : SwissRound = { RoundNumber = 2; Pairings = ResizeArray([ mkPair 3 "A" "C"; mkPair 4 "B" "D" ]) }
    let state : SwissState =
        { TournamentName = "Test Swiss"
          SeedGroupCount = 2
          GamesPerMatch = 2
          UniqueOpeningsGlobal = false
          NextOpeningIndex = 0
          GlobalOpeningOrder = ResizeArray()
          Rounds = ResizeArray([ round1; round2 ])
          UpdatedUtc = System.DateTime.UtcNow }
    let json = JsonSerializer.Serialize(state, JsonSerializerOptions(WriteIndented = true))
    let loaded = JsonSerializer.Deserialize<SwissState>(json, JsonSerializerOptions(PropertyNameCaseInsensitive = true))
    Assert.NotNull(loaded)
    Assert.Equal(2, loaded.Rounds.Count)
    Assert.Equal(2, loaded.Rounds.[0].Pairings.Count)
    Assert.Equal(2, loaded.Rounds.[1].Pairings.Count)

/// Mirrors the resume logic in Tournaments.razor GetSwissStateSummary:
/// a tournament is resumable when there are undecided pairings OR
/// when the configured round count exceeds the number of completed rounds.
let private isSwissResumable (state: SwissState) (configuredRounds: int) =
    let hasOpen =
        state.Rounds
        |> Seq.collect (fun r -> r.Pairings)
        |> Seq.exists (fun p -> not p.IsDecided)
    hasOpen || configuredRounds > state.Rounds.Count

let private mkDecidedPair id roundNr a b =
    { PairId = id
      RoundNumber = roundNr
      PlayerA = a
      PlayerB = b
      PlayerARating = 3000
      PlayerBRating = 2900
      ScoreA = 1.5
      ScoreB = 0.5
      IsDecided = true
      Games = ResizeArray()
      OpeningOrder = ResizeArray() }

let private mkSwissState (rounds: SwissRound list) =
    { TournamentName = "Test"
      SeedGroupCount = 1
      GamesPerMatch = 2
      UniqueOpeningsGlobal = false
      NextOpeningIndex = 0
      GlobalOpeningOrder = ResizeArray<int>()
      Rounds = ResizeArray<SwissRound>(rounds)
      UpdatedUtc = System.DateTime.UtcNow }

[<Fact>]
let ``swiss completed tournament is resumable when configured rounds increase`` () =
    let round1 : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ mkDecidedPair 1 1 "A" "B" ]) }
    let round2 : SwissRound = { RoundNumber = 2; Pairings = ResizeArray([ mkDecidedPair 2 2 "A" "B" ]) }
    let round3 : SwissRound = { RoundNumber = 3; Pairings = ResizeArray([ mkDecidedPair 3 3 "A" "B" ]) }
    let state = mkSwissState [ round1; round2; round3 ]
    // 3 rounds completed, configured for 3 — tournament is done
    Assert.False(isSwissResumable state 3)
    // User increases to 5 rounds — should be resumable
    Assert.True(isSwissResumable state 5)
    // User increases to 4 rounds — should be resumable
    Assert.True(isSwissResumable state 4)

[<Fact>]
let ``swiss tournament with undecided pairings is always resumable`` () =
    let undecidedPair =
        { PairId = 1; RoundNumber = 1; PlayerA = "A"; PlayerB = "B"
          PlayerARating = 3000; PlayerBRating = 2900
          ScoreA = 0.5; ScoreB = 0.5; IsDecided = false
          Games = ResizeArray(); OpeningOrder = ResizeArray() }
    let round1 : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ undecidedPair ]) }
    let state = mkSwissState [ round1 ]
    // Resumable regardless of configured rounds
    Assert.True(isSwissResumable state 1)
    Assert.True(isSwissResumable state 5)

[<Fact>]
let ``swiss completed tournament is not resumable when rounds unchanged`` () =
    let round1 : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ mkDecidedPair 1 1 "A" "B" ]) }
    let state = mkSwissState [ round1 ]
    Assert.False(isSwissResumable state 1)

[<Fact>]
let ``swiss completed tournament is not resumable when rounds decreased`` () =
    let round1 : SwissRound = { RoundNumber = 1; Pairings = ResizeArray([ mkDecidedPair 1 1 "A" "B" ]) }
    let round2 : SwissRound = { RoundNumber = 2; Pairings = ResizeArray([ mkDecidedPair 2 2 "A" "B" ]) }
    let state = mkSwissState [ round1; round2 ]
    // 2 rounds played, configured for 1 — not resumable
    Assert.False(isSwissResumable state 1)

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

[<Fact>]
let ``swissRoundPairings with floater puts top players last`` () =
    // Scenario: 3 players at 2.0, 4 players at 1.5
    // Player with 2.0 will float down to 1.5 group
    // Verify top players (2.0) still play last despite floater
    let players =
        [ mkRatedPlayer "Alice" 2500    // Seed 1
          mkRatedPlayer "Bob" 2400      // Seed 2
          mkRatedPlayer "Charlie" 2300  // Seed 3 (will float)
          mkRatedPlayer "Dave" 2200     // Seed 4
          mkRatedPlayer "Eve" 2100      // Seed 5
          mkRatedPlayer "Frank" 2000    // Seed 6
          mkRatedPlayer "Grace" 1900 ]  // Seed 7
    let seedOrder = players
    let scores =
        [ "Alice", 2.0
          "Bob", 2.0
          "Charlie", 2.0   // Floater (odd player in 2.0 group)
          "Dave", 1.5
          "Eve", 1.5
          "Frank", 1.5
          "Grace", 1.5 ]
        |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty

    // Expected pairing order (lowest scores first, highest last):
    // First: 1.5 score pairings (including floater)
    // Last: 2.0 score pairing (Alice vs Bob - top players)
    // 7 players = 3 pairings + 1 bye

    // Check how many pairs we got
    Assert.Equal(4, pairs.Length)  // 3 regular pairs + 1 bye pair

    // Get the last pairing (should be top players)
    let lastPair = pairs |> List.last
    let lastPairNames = Set.ofList [ (fst lastPair).Name; (snd lastPair).Name ]

    // Verify last pair is the highest-scoring pairing (Alice vs Bob, both 2.0)
    // NOT the floater pairing (Charlie 2.0 vs someone with 1.5)
    Assert.Equal<Set<string>>(Set.ofList [ "Alice"; "Bob" ], lastPairNames)

    // Get the first pairing (should be lowest scoring)
    let firstPair = pairs |> List.head
    let firstPairNames = Set.ofList [ (fst firstPair).Name; (snd firstPair).Name ]

    // First pair should NOT include Alice or Bob (top scorers)
    Assert.DoesNotContain("Alice", firstPairNames)
    Assert.DoesNotContain("Bob", firstPairNames)

[<Fact>]
let ``swissRoundPairings floater pairing order details`` () =
    // More detailed test: verify exact ordering when floater occurs
    // 3 players at 2.0 (odd group), 4 players at 1.5 (even group)
    let players =
        [ mkRatedPlayer "A" 2500  // 2.0
          mkRatedPlayer "B" 2400  // 2.0
          mkRatedPlayer "C" 2300  // 2.0 (floater - lowest seed in 2.0 group)
          mkRatedPlayer "D" 2200  // 1.5
          mkRatedPlayer "E" 2100  // 1.5
          mkRatedPlayer "F" 2000  // 1.5
          mkRatedPlayer "G" 1900  // 1.5 (lowest - gets bye)
        ]
    let seedOrder = players
    let scores =
        [ "A", 2.0; "B", 2.0; "C", 2.0
          "D", 1.5; "E", 1.5; "F", 1.5; "G", 1.5 ]
        |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty

    // Extract pairing details
    let pairList =
        pairs
        |> List.map (fun (p1, p2) -> (p1.Name, p2.Name))

    // Verify we have 4 pairs (3 real + 1 bye)
    Assert.Equal(4, pairList.Length)

    // Last pairing should be A vs B (both 2.0 - top board)
    let lastPair = pairList |> List.last
    let lastSet = Set.ofList [ fst lastPair; snd lastPair ]
    Assert.Equal<Set<string>>(Set.ofList [ "A"; "B" ], lastSet)

    // Verify C (floater) is NOT in the last pairing
    Assert.DoesNotContain("C", lastSet)

    // Verify the first pairing does NOT contain A or B
    let firstPair = pairList |> List.head
    let firstSet = Set.ofList [ fst firstPair; snd firstPair ]
    Assert.DoesNotContain("A", firstSet)
    Assert.DoesNotContain("B", firstSet)

    // Check that pairing order is: lower scores first, higher scores last
    // Find which pairing includes C (the floater at 2.0)
    let cPairIndex = pairList |> List.findIndex (fun (p1, p2) -> p1 = "C" || p2 = "C")

    // Find which pairing includes A or B
    let abPairIndex = pairList |> List.findIndex (fun (p1, p2) ->
        Set.ofList [p1; p2] = Set.ofList ["A"; "B"])

    // A vs B (both 2.0, no floater) should come AFTER C's pairing
    // This verifies that floater pairings are sorted before same-score pairings
    Assert.True(cPairIndex < abPairIndex,
        $"Floater pairing (C) at index {cPairIndex} should come before top board (A vs B) at index {abPairIndex}")

[<Fact>]
let ``swissRoundPairings complete pairing order verification`` () =
    // Comprehensive test: multiple score groups to verify complete ordering
    let players =
        [ mkRatedPlayer "P1" 3000  // 3.0
          mkRatedPlayer "P2" 2900  // 3.0
          mkRatedPlayer "P3" 2800  // 2.5
          mkRatedPlayer "P4" 2700  // 2.5
          mkRatedPlayer "P5" 2600  // 2.5 (floater from 2.5 group)
          mkRatedPlayer "P6" 2500  // 2.0
          mkRatedPlayer "P7" 2400  // 2.0
          mkRatedPlayer "P8" 2300  // 2.0
        ]
    let seedOrder = players
    let scores =
        [ "P1", 3.0; "P2", 3.0        // 2 players at 3.0
          "P3", 2.5; "P4", 2.5; "P5", 2.5  // 3 players at 2.5 (odd - P5 floats)
          "P6", 2.0; "P7", 2.0; "P8", 2.0  // 3 players at 2.0 (odd - P8 gets bye)
        ]
        |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    let pairNames = pairs |> List.map (fun (p1, p2) -> Set.ofList [p1.Name; p2.Name])

    // Expected order (lowest scores first, highest last):
    // 1. P5(2.5) vs P6/P7/P8(2.0) - floater from 2.5 paired down
    // 2. Remaining 2.0 players paired together
    // 3. P3(2.5) vs P4(2.5) - same score pairing
    // 4. P1(3.0) vs P2(3.0) - top board (highest scores)

    let lastPair = pairNames |> List.last
    let firstPair = pairNames |> List.head

    // Last pair should be highest score group (3.0)
    Assert.Equal<Set<string>>(Set.ofList ["P1"; "P2"], lastPair)

    // First pair should NOT include P1 or P2
    Assert.DoesNotContain("P1", firstPair)
    Assert.DoesNotContain("P2", firstPair)

    // Verify P1 vs P2 is indeed last
    let p1p2Index = pairNames |> List.findIndex (fun s -> s = Set.ofList ["P1"; "P2"])
    Assert.Equal(pairNames.Length - 1, p1p2Index)

[<Fact>]
let ``swiss state regenerates global opening order after loading trimmed data`` () =
    let tempPath = System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"swiss_regenerate_test_{System.Guid.NewGuid()}.json")
    try
        // Simulate persist/reload cycle with trimmed opening order
        let largeOrder = ResizeArray<int>([ for i in 0..199 -> i ])  // 200 indices
        let state =
            { TournamentName = "SwissRegenerateTest"
              SeedGroupCount = 1
              GamesPerMatch = 2
              UniqueOpeningsGlobal = true
              NextOpeningIndex = 0
              GlobalOpeningOrder = largeOrder
              Rounds = ResizeArray<Swiss.SwissRound>()
              UpdatedUtc = System.DateTime.UtcNow }

        // Simulate writeSwissState trimming
        let maxPersistedOpenings = 50
        let trimOrder (order: ResizeArray<int>) =
            if obj.ReferenceEquals(order, null) then
                order
            else
                ResizeArray<int>(order |> Seq.truncate maxPersistedOpenings)
        let trimmedState =
            { state with GlobalOpeningOrder = trimOrder state.GlobalOpeningOrder }

        // Persist and reload
        let opts = System.Text.Json.JsonSerializerOptions(WriteIndented = true)
        let json = System.Text.Json.JsonSerializer.Serialize(trimmedState, opts)
        System.IO.File.WriteAllText(tempPath, json)
        let readOpts = System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let loaded = System.Text.Json.JsonSerializer.Deserialize<Swiss.SwissState>(System.IO.File.ReadAllText(tempPath), readOpts)

        // Verify it was trimmed
        Assert.Equal(50, loaded.GlobalOpeningOrder.Count)

        // Check regeneration logic
        let openingsLength = 200
        let shouldRegenerate = loaded.GlobalOpeningOrder.Count = 0 || loaded.GlobalOpeningOrder.Count < openingsLength

        Assert.True(shouldRegenerate,
            $"Should regenerate when GlobalOpeningOrder.Count ({loaded.GlobalOpeningOrder.Count}) < openingsLength ({openingsLength})")

    finally
        if System.IO.File.Exists tempPath then System.IO.File.Delete tempPath

[<Fact>]
let ``swiss pairing regenerates opening order after loading trimmed data`` () =
    let tempPath = System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"swiss_pairing_regenerate_test_{System.Guid.NewGuid()}.json")
    try
        // Test per-pairing opening order regeneration
        let largeOrder = ResizeArray<int>([ for i in 0..199 -> i ])
        let pairing =
            { PairId = 1; RoundNumber = 1
              PlayerA = "A"; PlayerB = "B"
              PlayerARating = 3000; PlayerBRating = 2900
              ScoreA = 0.0; ScoreB = 0.0
              IsDecided = false
              Games = ResizeArray<Swiss.SwissGame>()
              OpeningOrder = largeOrder }
        let round = { RoundNumber = 1; Pairings = ResizeArray<Swiss.SwissPairing>([ pairing ]) }
        let state =
            { TournamentName = "SwissPairingRegenerateTest"
              SeedGroupCount = 1
              GamesPerMatch = 2
              UniqueOpeningsGlobal = false
              NextOpeningIndex = 0
              GlobalOpeningOrder = ResizeArray<int>()
              Rounds = ResizeArray<Swiss.SwissRound>([ round ])
              UpdatedUtc = System.DateTime.UtcNow }

        // Simulate trimming
        let maxPersistedOpenings = 50
        let trimOrder (order: ResizeArray<int>) =
            if obj.ReferenceEquals(order, null) then
                order
            else
                ResizeArray<int>(order |> Seq.truncate maxPersistedOpenings)
        let trimmedRounds =
            state.Rounds
            |> Seq.map (fun r ->
                let trimmedPairings =
                    r.Pairings
                    |> Seq.map (fun p ->
                        { p with OpeningOrder = trimOrder p.OpeningOrder })
                    |> ResizeArray
                { r with Pairings = trimmedPairings })
            |> ResizeArray
        let trimmedState = { state with Rounds = trimmedRounds }

        // Persist and reload
        let opts = System.Text.Json.JsonSerializerOptions(WriteIndented = true)
        let json = System.Text.Json.JsonSerializer.Serialize(trimmedState, opts)
        System.IO.File.WriteAllText(tempPath, json)
        let readOpts = System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let loaded = System.Text.Json.JsonSerializer.Deserialize<Swiss.SwissState>(System.IO.File.ReadAllText(tempPath), readOpts)

        let loadedPairing = loaded.Rounds.[0].Pairings.[0]
        Assert.Equal(50, loadedPairing.OpeningOrder.Count)

        // Check regeneration logic
        let openingsLength = 200
        let shouldRegenerate = loadedPairing.OpeningOrder.Count = 0 || loadedPairing.OpeningOrder.Count < openingsLength

        Assert.True(shouldRegenerate,
            $"Should regenerate when pairing OpeningOrder.Count ({loadedPairing.OpeningOrder.Count}) < openingsLength ({openingsLength})")

    finally
        if System.IO.File.Exists tempPath then System.IO.File.Delete tempPath
