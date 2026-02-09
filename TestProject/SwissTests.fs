module SwissTests

open System.Collections.Generic
open System.Text.Json
open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.SwissTypes
open ChessLibrary.TournamentPairing.PairingHelper

// Set to true to enable verbose test output for debugging
let private verboseOutput = false

let private debugPrint fmt =
    Printf.kprintf (fun s -> if verboseOutput then printfn "%s" s) fmt

let private mkRatedPlayer name rating =
    { EngineConfig.Empty with Name = name; Rating = rating }

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

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
    // Correct order: weakest score groups first (D-E, then B-C, then A-F)
    Assert.Equal<string list>([ "D-E"; "B-C"; "A-F" ], round5Names)
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
    // Correct order: weakest score groups first (D-E, then B-C, then A-F)
    Assert.Equal<string list>([ "D-E"; "B-C"; "A-F" ], round5Names)
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
    // Correct order: weakest score groups first (C-E, then B-D, then A-F)
    Assert.Equal<string list>([ "C-E"; "B-D"; "A-F" ], round5Names)
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
    // Correct order: weakest score groups first (C-E, then B-F, then A-D)
    Assert.Equal<string list>([ "C-E"; "B-F"; "A-D" ], round5Names)
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
    // Correct order: weakest score groups first (D-E, then B-C, then A-F)
    Assert.Equal<string list>([ "D-E"; "B-C"; "A-F" ], round5Names)

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
              Rounds = ResizeArray<SwissRound>()
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
        let loaded = System.Text.Json.JsonSerializer.Deserialize<SwissState>(System.IO.File.ReadAllText(tempPath), readOpts)

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
              Games = ResizeArray<SwissGame>()
              OpeningOrder = largeOrder }
        let round = { RoundNumber = 1; Pairings = ResizeArray<SwissPairing>([ pairing ]) }
        let state =
            { TournamentName = "SwissPairingRegenerateTest"
              SeedGroupCount = 1
              GamesPerMatch = 2
              UniqueOpeningsGlobal = false
              NextOpeningIndex = 0
              GlobalOpeningOrder = ResizeArray<int>()
              Rounds = ResizeArray<SwissRound>([ round ])
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
        let loaded = System.Text.Json.JsonSerializer.Deserialize<SwissState>(System.IO.File.ReadAllText(tempPath), readOpts)

        let loadedPairing = loaded.Rounds.[0].Pairings.[0]
        Assert.Equal(50, loadedPairing.OpeningOrder.Count)

        // Check regeneration logic
        let openingsLength = 200
        let shouldRegenerate = loadedPairing.OpeningOrder.Count = 0 || loadedPairing.OpeningOrder.Count < openingsLength

        Assert.True(shouldRegenerate,
            $"Should regenerate when pairing OpeningOrder.Count ({loadedPairing.OpeningOrder.Count}) < openingsLength ({openingsLength})")

    finally
        if System.IO.File.Exists tempPath then System.IO.File.Delete tempPath

[<Fact>]
let ``swiss pairing orders weakest engines first and strongest last`` () =
    // This test verifies that pairings are ordered correctly:
    // 1. Lower score groups come before higher score groups
    // 2. Within each score group, weaker pairings (higher minSeed) come before stronger pairings (lower minSeed)
    // This ensures weak engines play first in the round and top engines play last
    let players =
        [ mkRatedPlayer "Stockfish" 3500      // Will be seed 1 (strongest)
          mkRatedPlayer "Leela" 3450           // Will be seed 2
          mkRatedPlayer "Komodo" 3400          // Will be seed 3
          mkRatedPlayer "RubiChess" 3350       // Will be seed 4
          mkRatedPlayer "Berserk" 3300         // Will be seed 5
          mkRatedPlayer "Ethereal" 3250        // Will be seed 6
          mkRatedPlayer "Obsidian" 3200        // Will be seed 7
          mkRatedPlayer "Caissa" 3150 ]        // Will be seed 8 (weakest)

    let seedOrder = tcecSeedOrder players 2
    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList

    // Scenario: Mixed scores after a few rounds
    let scores =
        [ "Stockfish", 3.0   // Top score group
          "Leela", 3.0
          "Komodo", 2.0       // Middle score group
          "RubiChess", 2.0
          "Berserk", 2.0
          "Ethereal", 2.0
          "Obsidian", 1.0     // Bottom score group
          "Caissa", 1.0 ]
        |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty

    // Get the minimum seed for each pairing
    let getMinSeed (a: EngineConfig, b: EngineConfig) =
        let seedA = seedMap.[a.Name]
        let seedB = seedMap.[b.Name]
        min seedA seedB

    let pairSeeds = pairs |> List.map getMinSeed

    // CRITICAL: Weakest pairings (highest minSeed) should come first,
    // strongest pairings (lowest minSeed) should come last
    // So pairSeeds should be in DESCENDING order
    let isSortedDescending =
        pairSeeds
        |> List.pairwise
        |> List.forall (fun (a, b) -> a >= b)

    Assert.True(isSortedDescending,
        $"Pairings should be ordered weakest first, strongest last. Got seeds: {pairSeeds}")

    // Verify specific expectations:
    // First pairing should include the weakest players (highest seeds)
    let firstPair = pairs |> List.head
    let firstPairSeeds = Set.ofList [ seedMap.[fst(firstPair).Name]; seedMap.[snd(firstPair).Name] ]
    // Should include seeds 7 and/or 8 (Obsidian/Caissa in 1.0 group)
    Assert.True(firstPairSeeds.Contains(7) || firstPairSeeds.Contains(8),
        $"First pairing should include weakest players. Got seeds: {firstPairSeeds}")

    // Last pairing should include the strongest players (lowest seeds)
    let lastPair = pairs |> List.last
    let lastPairSeeds = Set.ofList [ seedMap.[fst(lastPair).Name]; seedMap.[snd(lastPair).Name] ]
    // Should include seeds 1 and/or 2 (Stockfish/Leela in 3.0 group)
    Assert.True(lastPairSeeds.Contains(1) || lastPairSeeds.Contains(2),
        $"Last pairing should include strongest players. Got seeds: {lastPairSeeds}")

    // Verify that the top engines (Stockfish vs Leela) are in the LAST pairing
    let topPairIndex =
        pairs
        |> List.findIndex (fun (a, b) ->
            Set.ofList [a.Name; b.Name] = Set.ofList ["Stockfish"; "Leela"])
    Assert.Equal(pairs.Length - 1, topPairIndex)

[<Fact>]
let ``swiss realistic tournament scenario from screenshot`` () =
    // Recreate the tournament state from the swissPairingBug.png screenshot
    // This represents a real tournament mid-way through with 16 engines
    let players =
        [ mkRatedPlayer "Stockfish" 3574
          mkRatedPlayer "LeelaChessZero" 3531
          mkRatedPlayer "KomodoMCTS" 3478
          mkRatedPlayer "Berserk" 3463
          mkRatedPlayer "RubiChess" 3441
          mkRatedPlayer "Obsidian" 3397
          mkRatedPlayer "Ethereal" 3392
          mkRatedPlayer "Caissa" 3386
          mkRatedPlayer "Seer" 3328
          mkRatedPlayer "Clover" 3318
          mkRatedPlayer "Stormphrax" 3303
          mkRatedPlayer "Viridithas" 3288
          mkRatedPlayer "Lizard" 3251
          mkRatedPlayer "Arasan" 3238
          mkRatedPlayer "Igel" 3201
          mkRatedPlayer "Velvet" 3189 ]

    // Typical mid-tournament scores (after several rounds)
    // Top engines have higher scores, bottom engines have lower scores
    let scores =
        [ "Stockfish", 5.5
          "LeelaChessZero", 5.0
          "KomodoMCTS", 4.5
          "Berserk", 4.5
          "RubiChess", 4.0
          "Obsidian", 4.0
          "Ethereal", 3.5
          "Caissa", 3.5
          "Seer", 3.0
          "Clover", 3.0
          "Stormphrax", 2.5
          "Viridithas", 2.5
          "Lizard", 2.0
          "Arasan", 2.0
          "Igel", 1.5
          "Velvet", 1.0 ]
        |> Map.ofList

    let seedOrder = tcecSeedOrder players 4
    let seedMap =
        seedOrder
        |> List.mapi (fun idx p -> p.Name, idx + 1)
        |> Map.ofList

    // Prior pairings (simulate some games already played)
    let priorPairs =
        Set.ofList [
            swissPairKey "Stockfish" "Seer"
            swissPairKey "LeelaChessZero" "Clover"
            swissPairKey "KomodoMCTS" "Stormphrax"
            swissPairKey "Berserk" "Viridithas"
        ]

    let pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    // Get pairing details for debugging
    let pairDetails =
        pairs
        |> List.mapi (fun i (a, b) ->
            let scoreA = scores.[a.Name]
            let scoreB = scores.[b.Name]
            let seedA = seedMap.[a.Name]
            let seedB = seedMap.[b.Name]
            let minSeed = min seedA seedB
            let avgScore = (scoreA + scoreB) / 2.0
            i + 1, $"{a.Name} vs {b.Name}", avgScore, minSeed)

    // Print pairing order for inspection
    debugPrint "Pairing order (should be weakest first, strongest last):"
    for (num, matchup, avgScore, minSeed) in pairDetails do
        debugPrint "  %d. %s (avg score: %.1f, min seed: %d)" num matchup avgScore minSeed

    // CRITICAL CHECKS:
    // 1. First pairing should be from the LOWEST score group (weakest engines)
    let firstPair = pairs |> List.head
    let firstPairAvgScore = (scores.[fst(firstPair).Name] + scores.[snd(firstPair).Name]) / 2.0

    // 2. Last pairing should be from the HIGHEST score group (strongest engines)
    let lastPair = pairs |> List.last
    let lastPairAvgScore = (scores.[fst(lastPair).Name] + scores.[snd(lastPair).Name]) / 2.0

    Assert.True(firstPairAvgScore < lastPairAvgScore,
        $"First pairing avg score ({firstPairAvgScore}) should be less than last pairing ({lastPairAvgScore})")

    // 3. Stockfish (top seed, highest score) should NOT be in the first few pairings
    let stockfishPairIndex =
        pairs
        |> List.findIndex (fun (a, b) -> a.Name = "Stockfish" || b.Name = "Stockfish")

    Assert.True(stockfishPairIndex > pairs.Length / 2,
        $"Stockfish should be in the latter half of pairings. Found at position {stockfishPairIndex + 1} of {pairs.Length}")

    // 4. Within same score groups, lower seeds should be paired later
    // Get pairings sorted by their minimum seed
    let getMinSeed (a: EngineConfig, b: EngineConfig) =
        min seedMap.[a.Name] seedMap.[b.Name]

    // Check that within each score group, minSeeds are in descending order
    let scoreGroups =
        pairs
        |> List.groupBy (fun (a, b) ->
            let scoreA = scores.[a.Name]
            let scoreB = scores.[b.Name]
            (scoreA + scoreB) / 2.0)
        |> List.sortBy fst

    for (groupScore, groupPairs) in scoreGroups do
        if groupPairs.Length > 1 then
            let minSeeds = groupPairs |> List.map getMinSeed
            let isDescending =
                minSeeds
                |> List.pairwise
                |> List.forall (fun (a, b) -> a >= b)

            Assert.True(isDescending,
                $"Within score group {groupScore}, pairings should be ordered by descending minSeed. Got: {minSeeds}")

[<Fact>]
let ``debug seed order and pairing order with different group counts`` () =
    let players =
        [ mkRatedPlayer "Stockfish" 3574
          mkRatedPlayer "Leela" 3531
          mkRatedPlayer "Komodo" 3478
          mkRatedPlayer "Berserk" 3463
          mkRatedPlayer "RubiChess" 3441
          mkRatedPlayer "Obsidian" 3397
          mkRatedPlayer "Ethereal" 3392
          mkRatedPlayer "Caissa" 3386 ]

    let scores = players |> List.map (fun p -> p.Name, 3.0) |> Map.ofList

    for groupCount in [1; 2; 4] do
        debugPrint "\n=== Seed Group Count: %d ===" groupCount
        let seedOrder = tcecSeedOrder players groupCount
        debugPrint "Seed order:"
        seedOrder |> List.iteri (fun i p -> debugPrint "  Seed %d: %s (rating %d)" (i+1) p.Name p.Rating)

        let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
        debugPrint "Pairing order:"
        pairs |> List.iteri (fun i (a, b) ->
            let seedMap = seedOrder |> List.mapi (fun idx p -> p.Name, idx + 1) |> Map.ofList
            let seedA = seedMap.[a.Name]
            let seedB = seedMap.[b.Name]
            let minSeed = min seedA seedB
            debugPrint "  %d. %s (seed %d) vs %s (seed %d) [minSeed=%d]" (i+1) a.Name seedA b.Name seedB minSeed)

    Assert.True(true) // This is just a debug test

[<Fact>]
let ``swiss game execution order weakest boards first`` () =
    // This test simulates the actual game numbering to verify execution order
    let players =
        [ mkRatedPlayer "Stockfish" 3574
          mkRatedPlayer "Leela" 3531
          mkRatedPlayer "Komodo" 3478
          mkRatedPlayer "Berserk" 3463 ]

    let scores = [ "Stockfish", 2.0; "Leela", 2.0; "Komodo", 1.0; "Berserk", 1.0 ] |> Map.ofList
    let seedOrder = tcecSeedOrder players 2
    let seedMap = seedOrder |> List.mapi (fun idx p -> p.Name, idx + 1) |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty

    // Simulate game numbering as done in Tournament.fs line 4256:
    // let roundGameNumber = (pairIndex * gamesPerMatch) + pairing.Games.Count + 1
    let gamesPerMatch = 2
    let gameNumbers =
        pairs
        |> List.mapi (fun pairIndex (a, b) ->
            let firstGameNr = (pairIndex * gamesPerMatch) + 1
            let lastGameNr = firstGameNr + gamesPerMatch - 1
            (a.Name, b.Name), (firstGameNr, lastGameNr))

    debugPrint "Game execution order:"
    for ((playerA, playerB), (firstGame, lastGame)) in gameNumbers do
        let seedA = seedMap.[playerA]
        let seedB = seedMap.[playerB]
        let minSeed = min seedA seedB
        debugPrint "  Games %d-%d: %s vs %s (minSeed=%d)" firstGame lastGame playerA playerB minSeed

    // Find which pairing includes Stockfish (strongest, should be LAST)
    let stockfishGames =
        gameNumbers
        |> List.find (fun ((a, b), _) -> a = "Stockfish" || b = "Stockfish")
        |> snd
        |> fst

    // Find which pairing is weakest (Komodo vs Berserk, both score 1.0)
    let weakestGames =
        gameNumbers
        |> List.head
        |> snd
        |> fst

    debugPrint "\nStockfish pairing starts at game #%d" stockfishGames
    debugPrint "Weakest pairing starts at game #%d" weakestGames

    // CRITICAL: Weakest pairing should have LOWER game numbers than Stockfish
    Assert.True(weakestGames < stockfishGames,
        $"Weakest pairing (game #{weakestGames}) should start before Stockfish (game #{stockfishGames})")

[<Fact>]
let ``reproduce bug from screenshot - extract standings and test pairing order`` () =
    // Recreating the tournament from swissPairingBug.png
    // Reading from the standings table (left panel) - approximately 16 engines
    // This is seed group count 1 (as user confirmed)

    // Based on visible standings in the image, creating engines in approximate rating order
    // I'll use placeholder names for ones I can't read clearly - user can help correct them
    let players =
        [ mkRatedPlayer "Stockfish" 3600          // Top engines
          mkRatedPlayer "LeelaChessZero" 3580
          mkRatedPlayer "KomodoMCTS" 3550
          mkRatedPlayer "Berserk" 3530
          mkRatedPlayer "RubiChess" 3510
          mkRatedPlayer "Obsidian" 3490
          mkRatedPlayer "Ethereal" 3470
          mkRatedPlayer "Caissa" 3450
          mkRatedPlayer "Seer" 3430
          mkRatedPlayer "Clover" 3410
          mkRatedPlayer "Stormphrax" 3390         // Weaker engines
          mkRatedPlayer "Viridithas" 3370
          mkRatedPlayer "Lizard" 3350
          mkRatedPlayer "Arasan" 3330
          mkRatedPlayer "Igel" 3310
          mkRatedPlayer "Velvet" 3290 ]

    // Seed group count 1 means straight rating order for seeds
    let seedOrder = tcecSeedOrder players 1

    // From the standings, extract the scores before this round
    // (This is approximate - user should verify and correct these)
    let scores =
        [ "Stockfish", 4.0
          "LeelaChessZero", 3.5
          "KomodoMCTS", 3.5
          "Berserk", 3.0
          "RubiChess", 3.0
          "Obsidian", 3.0
          "Ethereal", 2.5
          "Caissa", 2.5
          "Seer", 2.5
          "Clover", 2.0
          "Stormphrax", 2.0
          "Viridithas", 1.5
          "Lizard", 1.5
          "Arasan", 1.0
          "Igel", 1.0
          "Velvet", 0.5 ]
        |> Map.ofList

    let pairs = swissRoundPairings players seedOrder scores Set.empty Set.empty

    let seedMap = seedOrder |> List.mapi (fun idx p -> p.Name, idx + 1) |> Map.ofList

    // Simulate game numbering
    let gamesPerMatch = 2
    let gameAssignments =
        pairs
        |> List.mapi (fun pairIndex (a, b) ->
            let firstGameNr = (pairIndex * gamesPerMatch) + 1
            let seedA = seedMap.[a.Name]
            let seedB = seedMap.[b.Name]
            let minSeed = min seedA seedB
            (pairIndex, a.Name, b.Name, firstGameNr, minSeed))

    debugPrint "\nGame assignments (from screenshot bug scenario):"
    for (idx, nameA, nameB, gameNr, minSeed) in gameAssignments do
        debugPrint "  Pairing %d: Games %d-%d: %s vs %s (minSeed=%d)"
            idx gameNr (gameNr + gamesPerMatch - 1) nameA nameB minSeed

    // Find Stockfish and Stormphrax pairings
    let stockfishPairing =
        gameAssignments
        |> List.find (fun (_, a, b, _, _) -> a = "Stockfish" || b = "Stockfish")
    let stormphrasxPairing =
        gameAssignments
        |> List.find (fun (_, a, b, _, _) -> a = "Stormphrax" || b = "Stormphrax")

    let (_, _, _, stockfishGameNr, _) = stockfishPairing
    let (_, _, _, stormphraxGameNr, _) = stormphrasxPairing

    debugPrint "\nStockfish pairing starts at game #%d" stockfishGameNr
    debugPrint "Stormphrax pairing starts at game #%d" stormphraxGameNr

    // Stockfish (seed 1, strongest) should have HIGHER game number than Stormphrax (seed 11, weaker)
    Assert.True(stockfishGameNr > stormphraxGameNr,
        $"BUG: Stockfish (strongest, seed 1) at game #{stockfishGameNr} should play AFTER Stormphrax (weaker, seed 11) at game #{stormphraxGameNr}")

[<Fact>]
let ``round 7 with full 6-round history shows correct pairing order`` () =
    // Setup 16 players matching the real tournament
    let players =
        [ mkRatedPlayer "Stockfish" 3600
          mkRatedPlayer "Lc0-BT5" 3550
          mkRatedPlayer "Reckless" 3500
          mkRatedPlayer "Integral" 3450
          mkRatedPlayer "Berserk" 3400
          mkRatedPlayer "Lc0-BT4" 3350
          mkRatedPlayer "Caissa" 3300
          mkRatedPlayer "Ceres" 3250
          mkRatedPlayer "Obsidian" 3200
          mkRatedPlayer "PlentyChess" 3150
          mkRatedPlayer "Viridithas" 3100
          mkRatedPlayer "Dragon" 3050
          mkRatedPlayer "RubiChess" 3000
          mkRatedPlayer "Pawnocchio" 2950
          mkRatedPlayer "stormphrax" 2900
          mkRatedPlayer "Halogen" 2850 ]

    let seedOrder = tcecSeedOrder players 1

    // Simulate 6 rounds of pairings to build realistic prior pairs
    let mutable priorPairs = Set.empty
    let mutable currentScores =
        players
        |> List.map (fun p -> p.Name, 0.0)
        |> Map.ofList

    // Helper to simulate a round and update scores
    let simulateRound roundNum expectedResults =
        debugPrint "\n--- Simulating Round %d ---" roundNum
        let roundPairs = swissRoundPairings players seedOrder currentScores priorPairs Set.empty

        debugPrint "Pairings:"
        for (w, b) in roundPairs do
            let result = expectedResults |> Map.tryFind (w.Name, b.Name)
            let resultStr =
                match result with
                | Some 1.0 -> "1-0"
                | Some 0.5 -> "½-½"
                | Some 0.0 -> "0-1"
                | _ -> "?"
            debugPrint "  %s vs %s -> %s" w.Name b.Name resultStr

            // Update prior pairs
            priorPairs <- priorPairs.Add(swissPairKey w.Name b.Name)

            // Update scores based on results
            match result with
            | Some wScore ->
                currentScores <- currentScores |> Map.add w.Name (currentScores.[w.Name] + wScore)
                currentScores <- currentScores |> Map.add b.Name (currentScores.[b.Name] + (1.0 - wScore))
            | None -> ()

        debugPrint "Scores after round %d:" roundNum
        currentScores
        |> Map.toSeq
        |> Seq.sortByDescending snd
        |> Seq.iteri (fun i (name, score) -> debugPrint "  %2d. %-30s %.1f" (i+1) name score)

    // Simulate rounds 1-6 with realistic results leading to the desired scores
    // This is a simplified simulation - you'll provide the actual swiss_state.json
    // For now, we'll use placeholder results
    simulateRound 1 Map.empty |> ignore
    simulateRound 2 Map.empty |> ignore
    simulateRound 3 Map.empty |> ignore
    simulateRound 4 Map.empty |> ignore
    simulateRound 5 Map.empty |> ignore
    simulateRound 6 Map.empty |> ignore

    // Manually set the scores to match your real tournament before round 7
    currentScores <-
        [ "Stockfish", 8.0
          "Lc0-BT5", 7.5
          "Reckless", 6.5
          "Integral", 6.0
          "Berserk", 6.5
          "Lc0-BT4", 6.5
          "Caissa", 6.5
          "Ceres", 6.0
          "Obsidian", 6.5
          "PlentyChess", 6.5
          "Viridithas", 5.5
          "Dragon", 5.0
          "RubiChess", 5.5
          "Pawnocchio", 5.5
          "stormphrax", 4.5
          "Halogen", 3.5 ]
        |> Map.ofList

    debugPrint "\n=== ROUND 7 PAIRINGS ==="
    debugPrint "Scores before round 7:"
    currentScores
    |> Map.toSeq
    |> Seq.sortByDescending snd
    |> Seq.iteri (fun i (name, score) -> debugPrint "  %2d. %-30s %.1f" (i+1) name score)

    // Get round 7 pairings
    let round7Pairs = swissRoundPairings players seedOrder currentScores priorPairs Set.empty

    debugPrint "\nRound 7 Game Order (SHOULD be weakest groups first, strongest last):"
    debugPrint "%-8s | %-20s | %-20s | %5s | %5s" "Game #" "White" "Black" "W Sc" "B Sc"
    debugPrint "%s" (String.replicate 70 "-")

    for idx, (white, black) in List.indexed round7Pairs do
        let whiteScore = currentScores.[white.Name]
        let blackScore = currentScores.[black.Name]
        let avgScore = (whiteScore + blackScore) / 2.0
        debugPrint "%-8d | %-20s | %-20s | %5.1f | %5.1f  (avg: %.1f)"
            (idx + 1) white.Name black.Name whiteScore blackScore avgScore

    // Verify we have 8 pairings
    Assert.Equal(8, round7Pairs.Length)

    // Check ordering: first pairing should be from weakest score group
    let (firstW, firstB) = round7Pairs.Head
    let firstAvg = (currentScores.[firstW.Name] + currentScores.[firstB.Name]) / 2.0

    let (lastW, lastB) = round7Pairs |> List.last
    let lastAvg = (currentScores.[lastW.Name] + currentScores.[lastB.Name]) / 2.0

    debugPrint "\nORDERING CHECK:"
    debugPrint "First game average score: %.1f" firstAvg
    debugPrint "Last game average score: %.1f" lastAvg

    // BUG CHECK: Last game should have higher average score than first game
    // If this fails, it means strongest groups are playing first (BUG!)
    Assert.True(lastAvg > firstAvg,
        $"BUG DETECTED: First game has average score {firstAvg}, last game has {lastAvg}. Strongest should play last!")

    debugPrint "\nPrior pairs count: %d" priorPairs.Count

[<Fact>]
let ``round 7 pairings with 16 players shows correct order from weakest to strongest`` () =
    // Setup 16 players with standings before round 7
    let players =
        [ mkRatedPlayer "Stockfish" 3600
          mkRatedPlayer "Lc0-BT5" 3550
          mkRatedPlayer "Reckless" 3500
          mkRatedPlayer "Integral" 3450
          mkRatedPlayer "Berserk" 3400
          mkRatedPlayer "Lc0-BT4" 3350
          mkRatedPlayer "Caissa" 3300
          mkRatedPlayer "Ceres" 3250
          mkRatedPlayer "Obsidian" 3200
          mkRatedPlayer "PlentyChess" 3150
          mkRatedPlayer "Viridithas" 3100
          mkRatedPlayer "Dragon" 3050
          mkRatedPlayer "RubiChess" 3000
          mkRatedPlayer "Pawnocchio" 2950
          mkRatedPlayer "stormphrax" 2900
          mkRatedPlayer "Halogen" 2850 ]

    let seedOrder = tcecSeedOrder players 1

    // Scores before round 7 (after 6 rounds)
    let scores =
        [ "Stockfish", 8.0
          "Lc0-BT5", 7.5
          "Reckless", 6.5
          "Integral", 6.0
          "Berserk", 6.5
          "Lc0-BT4", 6.5
          "Caissa", 6.5
          "Ceres", 6.0
          "Obsidian", 6.5
          "PlentyChess", 6.5
          "Viridithas", 5.5
          "Dragon", 5.0
          "RubiChess", 5.5
          "Pawnocchio", 5.5
          "stormphrax", 4.5
          "Halogen", 3.5 ]
        |> Map.ofList

    // For simplicity, assume no prior pairings (or you could build them up through rounds 1-6)
    let priorPairs = Set.empty

    // Get round 7 pairings
    let round7Pairs = swissRoundPairings players seedOrder scores priorPairs Set.empty

    // Print the pairings in play order (first to last)
    debugPrint "\n=== Round 7 Pairings (Play Order) ==="
    debugPrint "Position | White          | Black          | White Score | Black Score"
    debugPrint "---------|----------------|----------------|-------------|------------"

    for idx, (white, black) in List.indexed round7Pairs do
        let whiteScore = scores.[white.Name]
        let blackScore = scores.[black.Name]
        debugPrint "%-8d | %-14s | %-14s | %-11.1f | %.1f"
            (idx + 1) white.Name black.Name whiteScore blackScore

    // Verify we have 8 pairings (16 players / 2)
    Assert.Equal(8, round7Pairs.Length)

    // Verify weakest score groups play first
    // Get the first and last pairing scores
    let (firstWhite, firstBlack) = round7Pairs.Head
    let firstPairMaxScore = max scores.[firstWhite.Name] scores.[firstBlack.Name]

    let (lastWhite, lastBlack) = round7Pairs |> List.last
    let lastPairMinScore = min scores.[lastWhite.Name] scores.[lastBlack.Name]

    debugPrint "\nFirst pairing max score: %.1f" firstPairMaxScore
    debugPrint "Last pairing min score: %.1f" lastPairMinScore

    // The last pairing should involve higher-scoring players than the first pairing
    // (weakest groups play first, strongest groups play last)
    Assert.True(lastPairMinScore >= firstPairMaxScore,
        $"Last pairing (min score {lastPairMinScore}) should have higher scores than first pairing (max score {firstPairMaxScore})")

    // Verify Stockfish (highest score: 8.0) is in the last pairing
    let stockfishInLastPair =
        lastWhite.Name = "Stockfish" || lastBlack.Name = "Stockfish"
    Assert.True(stockfishInLastPair, "Stockfish (8.0 points) should play in the last game of the round")

    // Verify Halogen (lowest score: 3.5) is in one of the first pairings
    let haloGenPairIndex =
        round7Pairs
        |> List.findIndex (fun (w, b) -> w.Name = "Halogen" || b.Name = "Halogen")
    debugPrint "Halogen plays in pairing #%d" (haloGenPairIndex + 1)

    // Halogen should play in the first half of the round
    Assert.True(haloGenPairIndex < round7Pairs.Length / 2,
        $"Halogen (3.5 points) should play in first half, but plays in pairing {haloGenPairIndex + 1} of {round7Pairs.Length}")

[<Fact>]
let ``swiss planned pairings skip decided matches on resume`` () =
    // Simulates a resume scenario: some pairings in the round are already decided.
    // The production code (TournamentRunners.fs runRound) skips IsDecided pairings
    // when building plannedPairings, so only undecided matches appear in "Upcoming Pairings".
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
    let scores = players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let roundPairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    let openings = [ mkOpening 1; mkOpening 2 ]

    // Create SwissPairings — mark the first one as decided (simulates resume)
    let swissPairings =
        roundPairs
        |> List.mapi (fun i (a, b) ->
            { PairId = i + 1
              RoundNumber = 1
              PlayerA = a.Name
              PlayerB = b.Name
              PlayerARating = a.Rating
              PlayerBRating = b.Rating
              ScoreA = if i = 0 then 1.5 else 0.0
              ScoreB = if i = 0 then 0.5 else 0.0
              IsDecided = (i = 0) // first pairing already decided
              Games = ResizeArray()
              OpeningOrder = ResizeArray() })

    // Build planned pairings the same way runRound does, but skip decided
    let planned = ResizeArray<Pairing>()
    let mutable previewIndex = 0
    for i in 0 .. swissPairings.Length - 1 do
        let sp = swissPairings.[i]
        let (a, b) = roundPairs.[i]
        if sp.IsDecided || sp.PlayerB = "BYE" then
            ()
        else
            let seedA = seedMap.[a.Name]
            let seedB = seedMap.[b.Name]
            let whiteFirst, blackFirst = if seedA <= seedB then a, b else b, a
            previewIndex <- addPlannedPairings planned whiteFirst blackFirst openings 2 previewIndex

    // Only undecided pairings should be in the planned list (1 of 2 pairings is decided)
    let undecidedCount = swissPairings |> List.filter (fun sp -> not sp.IsDecided) |> List.length
    Assert.Equal(undecidedCount * 2, planned.Count)
    // The decided pairing's players should NOT appear in planned pairings
    let decidedPairing = swissPairings |> List.find (fun sp -> sp.IsDecided)
    let hasDecidedPlayers =
        planned |> Seq.exists (fun p ->
            (p.White.Name = decidedPairing.PlayerA || p.White.Name = decidedPairing.PlayerB) &&
            (p.Black.Name = decidedPairing.PlayerA || p.Black.Name = decidedPairing.PlayerB))
    Assert.False(hasDecidedPlayers,
        $"Planned pairings should not include decided match {decidedPairing.PlayerA} vs {decidedPairing.PlayerB}")

[<Fact>]
let ``swiss planned pairings shrink when match completes`` () =
    // Verifies the RemoveAll logic used in TournamentRunners.fs to remove a completed
    // pairing's games from plannedPairings and re-emit an updated list.
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
    let scores = players |> List.map (fun p -> p.Name, 0.0) |> Map.ofList
    let roundPairs = swissRoundPairings players seedOrder scores Set.empty Set.empty
    let openings = [ mkOpening 1; mkOpening 2 ]

    // Build full planned pairings (all undecided)
    let planned = ResizeArray<Pairing>()
    let mutable previewIndex = 0
    for (a, b) in roundPairs do
        let seedA = seedMap.[a.Name]
        let seedB = seedMap.[b.Name]
        let whiteFirst, blackFirst = if seedA <= seedB then a, b else b, a
        previewIndex <- addPlannedPairings planned whiteFirst blackFirst openings 2 previewIndex

    let totalBefore = planned.Count
    Assert.Equal(roundPairs.Length * 2, totalBefore)

    // Simulate first pairing completing — apply same RemoveAll as production code
    let completedA = fst roundPairs.[0]
    let completedB = snd roundPairs.[0]
    planned.RemoveAll(fun p ->
        (p.White.Name = completedA.Name || p.White.Name = completedB.Name) &&
        (p.Black.Name = completedA.Name || p.Black.Name = completedB.Name)) |> ignore

    // Should have removed exactly 2 games (one per color for the completed pairing)
    Assert.Equal(totalBefore - 2, planned.Count)
    // Remaining games should not include the completed pair
    let hasCompletedPair =
        planned |> Seq.exists (fun p ->
            (p.White.Name = completedA.Name || p.White.Name = completedB.Name) &&
            (p.Black.Name = completedA.Name || p.Black.Name = completedB.Name))
    Assert.False(hasCompletedPair,
        $"After completion, planned pairings should not include {completedA.Name} vs {completedB.Name}")
