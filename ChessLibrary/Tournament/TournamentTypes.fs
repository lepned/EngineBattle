module ChessLibrary.TournamentTypes

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.CupTypes
open ChessLibrary.SwissTypes

/// Update messages sent during tournament execution for UI callbacks
type Update =
    | GameStarted of White:string
    | EndOfGame of Result: Result
    | BestMove of Info:BestMoveInfo * Status: EngineStatus
    | Info of Player:string * Info: string
    | Eval of Player:string * Type: EvalType
    | Status of Engine:EngineStatus
    | PonderStatus of Engine:EnginePonderStatus
    | Time of Player:string * Time: TimeOnly
    | NNSeq of NNSeq: ResizeArray<NNValues>
    | StartOfGame of Game:StartGameInfo
    | EndOfTournament of Info: Tournament
    | StartOfTournament of Info:StartOfTournamentInfo
    | MessagesFromEngine of Player:string * Message:string
    | PairingList of Pairings: ResizeArray<Pairing>
    | TotalNumberOfPairs of PairingsNumber: int
    | RoundNr of Round: string
    | PeriodicResults of results: ResizeArray<Result>
    | CupBracketUpdated
    | SwissStateUpdated
    | GameSummary of summary: string

/// Messages for the cup bracket state MailboxProcessor
type CupBracketMessage =
    | WriteCupBracket of Bracket: CupBracket * Reply: AsyncReplyChannel<unit>
    | ReadCupBracket of Reply: AsyncReplyChannel<CupBracket option>
    | DisposeCupBracket

/// Messages for the Swiss state MailboxProcessor
type SwissStateMessage =
    | WriteSwissState of State: SwissState * Reply: AsyncReplyChannel<unit>
    | ReadSwissState of Reply: AsyncReplyChannel<SwissState option>
    | DisposeSwissState

/// User adjudication request for a specific game
type UserAdjudication =
    { GameNr: int
      Result: string }
