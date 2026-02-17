namespace ChessLibrary

open System

module LadderTypes =
  type LadderGame =
    { GameNr: int
      White: string
      Black: string
      OpeningId: string
      OpeningHash: string
      Result: string }

  type LadderMatch =
    { MatchId: int
      ClimbNumber: int
      Challenger: string
      Defender: string
      ChallengerRating: int
      DefenderRating: int
      mutable ScoreChallenger: float
      mutable ScoreDefender: float
      mutable Winner: string option
      mutable IsDecided: bool
      Games: ResizeArray<LadderGame> }

  type LadderState =
    { TournamentName: string
      GamePairsPerMatch: int
      InitialRankings: ResizeArray<string>
      mutable SurvivingEngines: ResizeArray<string>
      mutable EliminatedEngines: ResizeArray<string>
      mutable CurrentClimbNumber: int
      mutable CurrentClimberIndex: int
      Matches: ResizeArray<LadderMatch>
      mutable NextOpeningIndex: int
      mutable GlobalOpeningOrder: ResizeArray<int>
      mutable UpdatedUtc: DateTime }
