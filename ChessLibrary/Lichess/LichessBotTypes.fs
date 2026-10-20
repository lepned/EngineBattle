namespace ChessLibrary

open System

/// A game the bot is playing, for the page: who, the position, the clocks and the engine's view.
type LichessBotGame =
    { GameId: string
      White: string
      Black: string
      /// The players' ratings at the start of the game (null when Lichess gives none).
      WhiteRating: Nullable<int>
      BlackRating: Nullable<int>
      BotIsWhite: bool
      Fen: string
      WTime: int64
      BTime: int64
      Eval: string
      /// The eval for the bar, White's view: pawns or mate in N (null for none, or in the book).
      EvalPawns: Nullable<float>
      EvalMate: Nullable<int>
      Status: string }

/// A challenge the bot sent that has no answer yet.
type OutgoingChallenge =
    { Id: string
      Opponent: string
      Seconds: int
      Increment: int
      Rated: bool
      Color: string
      At: DateTime
      /// Sent by matchmaking (withdrawn after a minute without an answer), not by hand (3 minutes).
      Auto: bool }
