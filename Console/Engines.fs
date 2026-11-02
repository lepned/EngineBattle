/// The engine the console commands search with: ChessLibrary.EngineSearch with the command line's
/// limit (Cli.Groups.Limit), and the JSON shapes of a search.
module Engines

open System
open Cli.Groups

type SearchResult = ChessLibrary.EngineSearch.SearchResult
type Analyzer = ChessLibrary.EngineSearch.Analyzer

/// Starts the engine (blocks until it is ready; throws when it cannot start).
let start config = ChessLibrary.EngineSearch.start config
let quit a = ChessLibrary.EngineSearch.quit a

/// One search of a FEN after UCI moves; with verbose the engine's lines are printed as they come.
let search (a: Analyzer) (fen: string) (moves: string list) (limit: Limit) (verbose: bool) =
    ChessLibrary.EngineSearch.search a fen moves limit.Go verbose

/// Every line of a MultiPV search, best first (EngineSearch.lines).
let lines (a: Analyzer) (fen: string) (multiPv: int) (limit: Limit) = ChessLibrary.EngineSearch.lines a fen multiPv limit.Go

/// Every legal move by the net's policy (EngineSearch.policyLines).
let policyLines (a: Analyzer) (root: string) (moves: string list) (limit: Limit) =
    ChessLibrary.EngineSearch.policyLines a root moves limit.Go

/// An eval for JSON, from White's view: pawns (0.35) or mate in N, never both; nothing when the engine gave none.
type EvalJson =
    { Pawns: Nullable<float>
      Mate: Nullable<int> }

let evalJson (e: ChessLibrary.MiscTypes.EvalType) =
    match e with
    | ChessLibrary.MiscTypes.EvalType.CP cp -> { Pawns = Nullable cp; Mate = Nullable() }
    | ChessLibrary.MiscTypes.EvalType.Mate m -> { Pawns = Nullable(); Mate = Nullable m }
    | _ -> { Pawns = Nullable(); Mate = Nullable() }

/// Pawns (White's view) to compare two evals by (mate is ±999.99), or None.
let pawnsOf (e: ChessLibrary.MiscTypes.EvalType) =
    match e with
    | ChessLibrary.MiscTypes.EvalType.CP cp -> Some cp
    | ChessLibrary.MiscTypes.EvalType.Mate m -> Some(float (sign m) * 999.99)
    | _ -> None
