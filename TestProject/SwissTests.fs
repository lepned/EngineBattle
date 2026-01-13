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
