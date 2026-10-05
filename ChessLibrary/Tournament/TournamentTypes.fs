module ChessLibrary.TournamentTypes

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.EngineTypes
open ChessLibrary.MiscTypes
open ChessLibrary.CupTypes
open ChessLibrary.SwissTypes
open ChessLibrary.LadderTypes

/// A game that was played to its end and written, with what identifies it in the plan: its
/// number, its pair label and its opening hash (the key two games of a pair share).
type FinishedGame =
    { GameNr: int
      RoundNr: string
      White: string
      Black: string
      OpeningHash: string
      Result: Result }

/// One engine's process over the last few seconds of a game: CPU (100 = one core), memory now and
/// at its peak, and whether it was the engine's turn when measured.
type EngineResources =
    { Player: string
      CpuPercent: float
      RamBytes: int64
      PeakRamBytes: int64
      ToMove: bool }

/// Update messages sent during tournament execution for UI callbacks
type Update =
    | GameStarted of White:string
    | EndOfGame of Result: Result
    | BestMove of Info:BestMoveInfo * Status: EngineStatus
    | Info of Player:string * Info: string
    | Eval of Player:string * Type: EvalType
    | Status of Engine:EngineStatus
    | PonderStatus of Engine:EnginePonderStatus
    | Time of Player:string * Time: TimeSpan
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
    | LadderStateUpdated
    | GameSummary of summary: string
    /// Sent once per game written to the PGN, after the write is queued (round robin and
    /// gauntlet, ParallelExecution); not for cancelled or unplayed games, not on the live feed.
    /// Arrives on the worker threads, so games that end together arrive concurrently.
    | GameFinished of Game: FinishedGame
    /// An engine could not be started, which stops the run (ParallelExecution's borrow).
    | EngineStartFailed of Engine: string * Reason: string
    /// An engine instance answered `uciok` (ParallelExecution's spawn): every option it reported,
    /// with its default as text (buttons have none and are left out). Sent per instance, so an
    /// engine played on several boards sends it more than once.
    | EngineStarted of Engine: string * Defaults: Map<string, string>
    /// Both engines' processes, every couple of seconds during a game (ResourceMonitor).
    | Resources of White: EngineResources * Black: EngineResources

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

/// Messages for the Ladder state MailboxProcessor
type LadderStateMessage =
    | WriteLadderState of State: LadderState * Reply: AsyncReplyChannel<unit>
    | ReadLadderState of Reply: AsyncReplyChannel<LadderState option>
    | DisposeLadderState

/// User adjudication request for a specific game
type UserAdjudication =
    { GameNr: int
      Result: string }
