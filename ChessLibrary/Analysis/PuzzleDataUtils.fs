module ChessLibrary.PuzzleDataUtils

open System
open ChessLibrary.PuzzleTypes
open ChessLibrary.TypesDef.PuzzleInput

let chess = Chess.Board()

let parseRatingGroups (s:string) maxRating =
  let gs = s.Split(',') |> Array.filter (not << System.String.IsNullOrWhiteSpace)
  if gs.Length = 0 then
    if maxRating = 0 then failwith "No rating groups or max rating specified"
    else [| maxRating |]
  else gs |> Array.map int

let parseThemes (s:string) =
  s.Split(',') |> Array.map (fun t -> t.Trim())

let parseNodes (s:string) =
  if System.String.IsNullOrWhiteSpace s then [|0|]
  else
    s.Split(',')
    |> Array.map int

let getPVMoves (pv: string) =
  let pvMoves = ResizeArray<string>()
  let arr = pv.Split ' '
  for item in arr do
    if Seq.exists Char.IsDigit item then
      pvMoves.Add(item)
  pvMoves

let getAllFens (fen: string) (moves: string seq) =
  chess.ResetBoardState()
  let fens = ResizeArray<string>()
  fens.Add(fen)
  chess.LoadFen(fen)
  for move in moves do
    chess.PlayLongSanMove(move)
    fens.Add(chess.FEN())
  fens

let getUpdatedRecord (record: CsvPuzzleData) =
    // 1) parse SAN moves once
    let moves = getPVMoves record.Moves

    // 2) reset & seed the engine
    chess.ResetBoardState()
    chess.LoadFen record.Fen

    // 3) pre-allocate (optional) for a little extra speed
    let fens = ResizeArray<string>(moves.Count + 1)
    let cmds = ResizeArray<Position>(moves.Count / 2)

    // 4) starting position
    fens.Add record.Fen

    // 5) accumulate a SAN-string for the "moves so far"
    let mutable movesSoFar = ""

    for idx in 0 .. moves.Count - 1 do
      let mv = moves.[idx]

      // 5a) if this is an odd-index move, emit a Puzzle.Position
      if idx % 2 = 1 then
        let commandText = $"position fen {record.Fen} moves {movesSoFar}"
        cmds.Add
          { Command     = commandText
            CorrectMove = mv
            MovePlayed  = "" }

      // 5b) play the move and record its FEN
      chess.PlayLongSanMove mv
      fens.Add (chess.FEN())

      // 5c) update the SAN-string for the next iteration
      movesSoFar <-
        if idx = 0 then mv
        else $"{movesSoFar} {mv}"

    // 6) stitch back into your record
    { record with
        Fens     = List.ofSeq fens
        Commands = List.ofSeq cmds }

let sortPuzzleData (theme:string) ratingGroup input =
      let byTheme =
          if String.IsNullOrWhiteSpace theme then
              input.puzzleData
          else
              input.puzzleData
              |> Array.filter (fun e -> e.Themes.IndexOf(theme, StringComparison.OrdinalIgnoreCase) >= 0 )
      // filter then sort only the needed subset
      byTheme
      |> Array.filter (fun e -> e.Rating <= ratingGroup)
      |> Array.sortByDescending (fun e -> e.Rating)
      |> Array.truncate input.sampleSize
      |> Array.map getUpdatedRecord
