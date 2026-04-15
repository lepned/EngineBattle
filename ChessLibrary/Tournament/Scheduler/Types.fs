namespace ChessLibrary.Scheduler

open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes

/// What role an engine plays in a scheduled game, relative to the tournament mode.
type Role =
    /// The engine acts as the gauntlet "main player" (plays White first in each
    /// pairing). Only used in Gauntlet mode.
    | Challenger
    /// The engine is paired against a challenger. Only used in Gauntlet mode.
    | Opponent
    /// Non-gauntlet role (RR, Swiss, Cup, Ladder). Both players are symmetric.
    | Standard

type ScheduleMode =
    | Gauntlet
    | RoundRobin
    | Cup
    | Swiss
    | Ladder

/// Controls how openings from the book are distributed across opponents in
/// Gauntlet mode.
type OpeningDistribution =
    /// Each opponent gets a disjoint slice of the book of length `rounds`,
    /// so `numOpponents × rounds` openings are consumed in total. This is the
    /// default when `RandomOpenings = true` (and what the user calls
    /// "different openings pairwise").
    | Spread
    /// Every opponent plays every opening. `rounds` openings are consumed in
    /// total; each opening is played by `numOpponents × numChallengers` pairs.
    /// This is the default when `RandomOpenings = false`.
    | Shared

/// Composite identifier used for resume diffing: two games with the same key
/// are considered interchangeable, i.e. one already-played game in the PGN can
/// satisfy one planned game.
type GameKey =
    { OpeningHash : string
      Fen         : string
      White       : string
      Black       : string }

/// A game the scheduler intends to play, fully specified. Schedulers produce
/// a list of these up front; the runner plays them (or diffs against the PGN
/// and plays the remainder). Nothing about a PlannedGame depends on play time.
type PlannedGame =
    { White       : EngineConfig
      Black       : EngineConfig
      Opening     : PgnGame
      OpeningHash : string
      RoundLabel  : string
      RoleWhite   : Role
      RoleBlack   : Role
      Key         : GameKey }

/// Input to a scheduler. Captures everything needed to generate the plan
/// deterministically from the current tournament configuration.
type ScheduleConfig =
    { Mode             : ScheduleMode
      Challengers      : EngineConfig list
      Opponents        : EngineConfig list
      /// Shuffled-and-truncated opening book, sized for the chosen distribution:
      ///   Spread  → expected length = `Rounds × Opponents.Length`
      ///   Shared  → expected length = `Rounds`
      /// Schedulers stride with modulo when smaller, but callers should size
      /// correctly to avoid unintended repetition.
      Openings         : PgnGame list
      Rounds           : int
      OpeningsTwice    : bool
      PreventDeviation : bool
      Distribution     : OpeningDistribution }
