module ChessLibrary.PGNExtractor

open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes

/// Extracts moves from a PGN game.
/// <param name="game">The PGN game.</param>
/// <returns>A tuple containing the moves and game metadata.</returns>
let extractMoves (game:PgnGame) =
  let moves =
    [|for move in game.Mainline do
       if move.Color = "w" then
          yield true, game.GameMetaData.White, move.MoveNumber, move.San
       else
          yield false, game.GameMetaData.Black, move.MoveNumber, move.San |]
  moves, game.GameMetaData


/// Extracts engine book exit statistics from a PGN game.
/// <param name="game">The PGN game.</param>
/// <returns>The engine book exit statistics.</returns>
let extractEngineBookExitStats (game:PgnGame) =
  let mutable foundWhiteMove = false
  let mutable foundBlackMove = false
  let moves =
    [|for move in game.Mainline do
       if move.Color = "w" then
          let white = Annotation.getEngineStatData game.GameMetaData.White false move.Comment
          if (not foundWhiteMove && white.n > 0) then
            foundWhiteMove <- true
            yield white
       else
          let black = Annotation.getEngineStatData game.GameMetaData.Black true move.Comment
          if (not foundBlackMove && black.n > 0) then
            foundBlackMove <- true
            yield black
    |]
  {White = game.GameMetaData.White; Black = game.GameMetaData.Black; Moves = moves}

/// Extracts all book exit engine statistics from a sequence of PGN games.
/// <param name="games">The sequence of PGN games.</param>
/// <returns>An array of engine book exit statistics.</returns>
let extractAllBookExitEngineStatsInPGN (games: PgnGame seq) =
  games
  |> Seq.map extractEngineBookExitStats
  |> Seq.toArray

/// Extracts engine statistics from a PGN game.
/// <param name="game">The PGN game.</param>
/// <returns>The engine statistics.</returns>
let extractEngineStats (game:PgnGame) =
  let moves =
    [|for move in game.Mainline do
       if move.Color = "w" then
          yield Annotation.getEngineStatData game.GameMetaData.White false move.Comment
       else
          yield Annotation.getEngineStatData game.GameMetaData.Black true move.Comment |]
  {White = game.GameMetaData.White; Black = game.GameMetaData.Black; Moves = moves}

let extractWhiteAndBlackEngineStats (game:PgnGame) =
  let whiteMoves =
    [|for move in game.Mainline do
          if move.Color = "w" then
            Annotation.getEngineStatData game.GameMetaData.White false move.Comment
    |]
  let blackMoves =
    [|for move in game.Mainline do
          if move.Color = "b" then
            Annotation.getEngineStatData game.GameMetaData.Black true move.Comment
    |]
  whiteMoves, blackMoves

/// Extracts all engine statistics from a sequence of PGN games.
/// <param name="games">The sequence of PGN games.</param>
/// <returns>An array of engine statistics.</returns>
let extractAllEngineStatsInPGN (games: PgnGame seq) =
  games
  |> Seq.map extractEngineStats
  |> Seq.toArray

/// Extracts all engine moves from an array of engine statistics.
/// <param name="games">The array of engine statistics.</param>
/// <returns>An array of engine move statistics.</returns>
let extractAllEngineMovesInPGN (games: EngineStat array) =
  [| for game in games -> game.Moves |]
  |> Array.concat
