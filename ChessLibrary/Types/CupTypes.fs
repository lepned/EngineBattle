namespace ChessLibrary

open System

module CupTypes =
  type CupGame =
    { GameNr: int
      White: string
      Black: string
      OpeningId: string
      OpeningHash: string
      Result: string }

  type CupMatch =
    { MatchId: int
      RoundNumber: int
      PlayerA: string
      PlayerB: string
      PlayerARating: int
      PlayerBRating: int
      mutable ScoreA: float
      mutable ScoreB: float
      mutable Winner: string option
      mutable IsDecided: bool
      Games: ResizeArray<CupGame>
      mutable OpeningOrder: ResizeArray<int> }

  type CupRound =
    { RoundNumber: int
      Matches: ResizeArray<CupMatch> }

  type CupBracket =
    { TournamentName: string
      Strategy: string
      GamesPerMatch: int
      UniqueOpeningsGlobal: bool
      mutable NextOpeningIndex: int
      mutable GlobalOpeningOrder: ResizeArray<int>
      Rounds: ResizeArray<CupRound>
      mutable UpdatedUtc: DateTime }
