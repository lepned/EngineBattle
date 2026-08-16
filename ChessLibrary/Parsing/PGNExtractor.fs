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

/// Supplies `wv` from a lichess-style `[%eval …]` when the comment carries no engine
/// annotation of its own — otherwise the evaluation chart is a flat line at zero for every
/// game annotated by lichess rather than by an engine harness.
///
/// `[%eval]` is White-relative whichever side moved, so no sign flip applies here. A mate
/// becomes the same ±200.0 sentinel `getEngineStatData` produces for "M5", which the chart
/// already clamps, so a mate plots identically however the game was annotated.
let private withCommentEval (comment: string) (stat: EngineMoveStat) =
  if stat.wv <> 0.0 || stat.d <> 0 || stat.n <> 0L || stat.mt <> 0L || stat.tl <> 0L then stat
  else
    match PGNComment.parse comment |> PGNComment.tryFind "eval" with
    | Some v when v.StartsWith "#" ->
      let distance = v.Substring 1
      match System.Int32.TryParse(distance, System.Globalization.NumberStyles.Integer,
                                  System.Globalization.CultureInfo.InvariantCulture) with
      | true, n when n < 0 -> { stat with wv = -200.0 }
      | true, _ -> { stat with wv = 200.0 }
      | _ -> stat
    | Some v ->
      match System.Double.TryParse(v, System.Globalization.NumberStyles.Float,
                                   System.Globalization.CultureInfo.InvariantCulture) with
      | true, d -> { stat with wv = d }
      | _ -> stat
    | None -> stat

let extractWhiteAndBlackEngineStats (game:PgnGame) =
  let whiteMoves =
    [|for move in game.Mainline do
          if move.Color = "w" then
            Annotation.getEngineStatData game.GameMetaData.White false move.Comment
            |> withCommentEval move.Comment
    |]
  let blackMoves =
    [|for move in game.Mainline do
          if move.Color = "b" then
            Annotation.getEngineStatData game.GameMetaData.Black true move.Comment
            |> withCommentEval move.Comment
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
