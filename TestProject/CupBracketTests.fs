module CupBracketTests

open System
open System.IO
open Xunit
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.TypesDef.Cup
open System.Text.Json

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

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
