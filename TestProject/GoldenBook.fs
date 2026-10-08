/// The book the Swiss, Cup and Ladder golden traces are played from, and a name for each opening.
module GoldenBook

open System
open System.IO
open ChessLibrary

/// A small book: distinct short openings.
let bookLines =
  [ "1. e4 e5"; "1. d4 d5"; "1. c4 e5"; "1. Nf3 d5"; "1. e4 c5"; "1. d4 Nf6"; "1. e4 e6"; "1. e4 c6"
    "1. g3 d5"; "1. b3 e5"; "1. f4 d5"; "1. Nc3 d5" ]

let writeBook dir =
  let path = Path.Combine(dir, "book.pgn")
  let games =
    bookLines |> List.mapi (fun i line ->
      sprintf "[Event \"b\"]\n[Site \"\"]\n[Round \"%d\"]\n[White \"w\"]\n[Black \"b\"]\n[Result \"*\"]\n\n%s *\n" (i + 1) line)
  File.WriteAllText(path, String.Join("\n", games))
  path

// the opening hash ends its text with Environment.NewLine, so it differs between Windows and Linux
let private labels =
  lazy (
    let dir = Path.Combine(Path.GetTempPath(), "eb-golden-book-" + Guid.NewGuid().ToString "N")
    Directory.CreateDirectory dir |> ignore
    let games, _ = GameHelpers.loadOpeningsUnlimited (Some (writeBook dir)) 2
    games |> Array.mapi (fun i g -> ChessUtilities.Hash.computeOpeningHashFromGame g, sprintf "o%d" (i + 1)) |> Map.ofArray)

/// The book opening a hash belongs to ("o1".."o12"), as the traces name it on every platform.
let opening (hash: string) =
  match labels.Value.TryFind hash with
  | Some label -> label
  | None -> if hash.Length > 8 then hash.Substring(0, 8) else hash

/// Between a run and its resume: the stand-in games write no PGN, and a state file whose PGN has
/// no games is set aside as stale (StatePaths.prepare), so the PGN gets one, as a real run's would.
let markPgnPlayed (pgnPath: string) =
  if not (File.Exists pgnPath) || not (File.ReadAllText(pgnPath).Contains "[Event ") then
    File.AppendAllText(pgnPath, "[Event \"stand-in\"]\n[White \"w\"]\n[Black \"b\"]\n[Result \"*\"]\n\n*\n\n")
