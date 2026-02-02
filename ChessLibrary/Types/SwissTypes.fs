namespace ChessLibrary

open System

module SwissTypes =
  type SwissGame =
    { GameNr: int
      White: string
      Black: string
      OpeningId: string
      OpeningHash: string
      Result: string }

  type SwissPairing =
    { PairId: int
      RoundNumber: int
      PlayerA: string
      PlayerB: string
      PlayerARating: int
      PlayerBRating: int
      mutable ScoreA: float
      mutable ScoreB: float
      mutable IsDecided: bool
      Games: ResizeArray<SwissGame>
      mutable OpeningOrder: ResizeArray<int> }

  type SwissRound =
    { RoundNumber: int
      Pairings: ResizeArray<SwissPairing> }

  type SwissState =
    { TournamentName: string
      SeedGroupCount: int
      GamesPerMatch: int
      UniqueOpeningsGlobal: bool
      mutable NextOpeningIndex: int
      mutable GlobalOpeningOrder: ResizeArray<int>
      Rounds: ResizeArray<SwissRound>
      mutable UpdatedUtc: DateTime }
