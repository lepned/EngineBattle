module ChessLibrary.PGNHelper

open System
open System.IO
open System.Text.RegularExpressions
open ChessLibrary.PGNTypes
open ChessLibrary.EPDTypes

let ensureDirectoryExists (filePath: string) =
    let directory = Path.GetDirectoryName(filePath)
    if not (Directory.Exists(directory)) then
        Directory.CreateDirectory(directory) |> ignore

let writeAndCorrectRoundHeadersInShuffleFile (filePath: string) (games: PgnGame seq) =
  let mutable idx = 0
  for game in games do
      idx <- idx + 1
      let newRaw =
          if Regex.IsMatch(game.Raw, @"\[Round\s*""[^""]*""\]") then
              Regex.Replace(
                  game.Raw,
                  @"\[Round\s*""[^""]*""\]",
                  sprintf "[Round \"%d.%d\"]" idx 1)
          else
              sprintf "[Round \"%d.%d\"]\n%s" idx 1 game.Raw

      File.AppendAllText(filePath, newRaw)

let shufflePgnGames (filePath: string) (games: PgnGame seq) =
  try
      let gamesArray = games |> Seq.toArray
      Random.Shared.Shuffle gamesArray
      ensureDirectoryExists filePath

      if File.Exists filePath then
          File.Delete filePath
      writeAndCorrectRoundHeadersInShuffleFile filePath gamesArray
  with
      | :? System.UnauthorizedAccessException as ex ->
          printfn "Error: %s" ex.Message
          printfn "Please close the file and try again."
      | :? System.IO.IOException as ex ->
          printfn "Error: %s" ex.Message
          printfn "Please close the file and try again."
      | ex ->
          printfn "An unexpected error occurred: %s" ex.Message


let extractFen (input: string) (idx : int) =
    let parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let ceresNetPrediction = parts.[0]
    let tablebaseCorrectAnswer = parts.[1]
    let msg = sprintf "Ceres: %s, TB: %s" ceresNetPrediction tablebaseCorrectAnswer
    let rest = String.concat " " parts[2..]
    let entry =
      { RawInput = input; FEN = rest; BestMove = None; AvoidMove = None; Id = sprintf "Id: %d" idx |> Some  ; Other = Some msg }
    {
      EPD = entry
      TBAnswer = Int32.Parse tablebaseCorrectAnswer
      QAnswer = Int32.Parse ceresNetPrediction
      Move = String.Empty
    }

let readTableBaseResults (filePath:string) =
  let content = File.ReadAllLines filePath
  let mutable nr = 0
  seq { for line in content do
          nr <- nr + 1
          if line.Contains "BATCH" |> not then
            extractFen line nr
     }

/// Gets the opening information from a PGN game.
let private fallbackOpeningInfo (opening: Header option) (fen: string) (gameNumber: int) =
  match opening with
  | Some op -> sprintf "Nr %i: %s" gameNumber op.Value
  | None when String.IsNullOrEmpty fen -> sprintf "Nr %i: No opening name" gameNumber
  | None -> sprintf "Nr %i: %s" gameNumber fen

let private formatOpeningInfo
  (prefix: string option)
  (opening: Header option)
  (variation: Header option)
  (eco: Header option)
  (gameNumber: int)
  (fen: string)
  =
  match opening, variation, eco with
  | Some op, Some v, Some eco -> sprintf "%s%s - %s, ECO: %s" (prefix |> Option.defaultValue "") op.Value v.Value eco.Value
  | Some h, Some v, None -> sprintf "%s%s - %s" (prefix |> Option.defaultValue "") h.Value v.Value
  | Some h, None, Some eco -> sprintf "%s%s, ECO: %s" (prefix |> Option.defaultValue "") h.Value eco.Value
  | None, None, Some eco -> sprintf "%sECO: %s" (prefix |> Option.defaultValue "") eco.Value
  | Some op, None, None -> sprintf "%s%s" (prefix |> Option.defaultValue "") op.Value
  | _ -> fallbackOpeningInfo opening fen gameNumber

let getOpeningInfo (game: PgnGame) =
  let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
  let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
  let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
  formatOpeningInfo (Some "Opening: ") opening variation eco game.GameNumber game.Fen

let getOpeningInfoOnly (game: PgnGame) =
  let opening = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
  let variation = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
  let eco = game.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
  formatOpeningInfo None opening variation eco game.GameNumber game.Fen

let getOpeningOnly (meta: GameMetadata) =
  let opening =
    meta.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening")
    |> Option.orElseWith (fun () ->
      if not (String.IsNullOrEmpty meta.OpeningName) then Some { Key = "Opening"; Value = meta.OpeningName }
      else None)
  let variation = meta.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation")
  let eco = meta.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO")
  match opening, variation, eco with
  | Some op, Some v, Some eco -> sprintf "%s - %s, ECO: %s" op.Value v.Value eco.Value
  | Some h, Some v, None -> sprintf "%s - %s" h.Value v.Value
  | Some h, _, Some eco -> sprintf "%s, ECO: %s" h.Value eco.Value
  | Some h, _, _ -> sprintf "%s" h.Value
  | None, None, Some eco -> sprintf "ECO: %s" eco.Value
  | _ ->
    match opening with
    | Some op -> op.Value
    | None when String.IsNullOrEmpty meta.Fen -> sprintf "Round %s: No opening name" meta.Round
    | None -> sprintf "Round %s: Fen %s:" meta.Round meta.Fen

/// Per-ply evaluations for a game's mainline, formatted for a move list ("+0.3", "#4").
///
/// Two sources, in priority order. EB's own engine annotations come first (`wv=…, d=…, n=…`,
/// plus the Banksia, Ceres and `+0.28/12` variants `getEngineStatData` understands); when a
/// comment carries none of those, a lichess-style `[%eval …]` command is used instead. Both
/// conventions are White-relative, so a game annotated by either — or by both, as a lichess
/// export of an EB game would be — yields one consistent series.
let evalsByPly (game: PgnGame) : System.Collections.Generic.Dictionary<int, string> =
  let evals = System.Collections.Generic.Dictionary<int, string>()
  for i in 0 .. game.Mainline.Count - 1 do
    let move = game.Mainline.[i]
    if not (String.IsNullOrEmpty move.Comment) then
      let isBlack = move.Color = "b"
      let player = if isBlack then game.GameMetaData.Black else game.GameMetaData.White
      let stat = EngineTypes.Annotation.getEngineStatData player isBlack move.Comment
      if stat.d <> 0 || stat.n <> 0L || stat.wv <> 0.0 then
        // A mate is a ±200.0 sentinel by the time the annotation parser is done with it, and
        // the distance is gone with it — so "M5" would reach the move list as "+200.0". The
        // sentinel keeps the side (it is flipped White-relative like any other score); the
        // number comes back out of the comment.
        if abs stat.wv >= 200.0 then
          let sign = if stat.wv < 0.0 then "-" else ""
          evals.[i] <-
            match PGNComment.tryMateDistance move.Comment with
            | Some n -> sprintf "#%s%d" sign n
            // No mate token behind the sentinel means this is not a mate at all but an
            // engine reporting a score of 200 pawns or more. Saying so is more honest than
            // a distanceless "#".
            | None -> PGNComment.formatPawns stat.wv
        else
          evals.[i] <- PGNComment.formatPawns stat.wv
      else
        match PGNComment.parse move.Comment |> PGNComment.tryFind "eval" |> Option.bind PGNComment.formatEval with
        | Some e -> evals.[i] <- e
        | None -> ()
  evals
