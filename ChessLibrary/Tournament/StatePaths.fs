/// Where a cup, Swiss or ladder keeps its state - by default next to the tournament's PGN, named
/// after it - and making that file ready for a run. The runner and the WebGUI both resolve it here.
module ChessLibrary.StatePaths

open System
open System.IO
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes

type Mode =
  | Cup
  | Swiss
  | Ladder

let private fileName mode =
  match mode with
  | Cup -> "cup_bracket.json"
  | Swiss -> "swiss_state.json"
  | Ladder -> "ladder_state.json"

let private configured (tourny: Tournament) mode =
  match mode with
  | Cup -> if obj.ReferenceEquals(tourny.CupOptions, null) then "" else tourny.CupOptions.BracketPath
  | Swiss -> if obj.ReferenceEquals(tourny.SwissOptions, null) then "" else tourny.SwissOptions.StatePath
  | Ladder -> if obj.ReferenceEquals(tourny.LadderOptions, null) then "" else tourny.LadderOptions.StatePath

let private rooted (baseDir: string) (p: string) = Path.GetFullPath(if Path.IsPathRooted p then p else Path.Combine(baseDir, p))

// the shared default every config used to carry counts as not set
let private notSet mode (p: string) =
  String.IsNullOrWhiteSpace p
  || p.Trim().Replace('\\', '/').TrimStart('.', '/').Equals("wwwroot/" + fileName mode, StringComparison.OrdinalIgnoreCase)

/// The shared file under wwwroot where every tournament without a path of its own used to keep its state.
let shared (baseDir: string) mode =
  let candidates =
    [ Path.Combine(baseDir, "wwwroot", fileName mode)
      Path.Combine(baseDir, "WebGUI", "wwwroot", fileName mode) ]
  Path.GetFullPath(candidates |> List.tryFind File.Exists |> Option.defaultValue candidates.Head)

/// A tournament's state file: a configured path as given; otherwise next to its PGN and named after
/// it (MyCup.pgn -> MyCup_cup_bracket.json); with no PGN either, the shared file.
let path (baseDir: string) (tourny: Tournament) mode =
  let c = configured tourny mode
  if not (notSet mode c) then rooted baseDir c
  elif String.IsNullOrWhiteSpace tourny.PgnOutPath then shared baseDir mode
  else
    let pgn = rooted baseDir tourny.PgnOutPath
    Path.Combine(Path.GetDirectoryName pgn, Path.GetFileNameWithoutExtension pgn + "_" + fileName mode)

/// What a state file says about its tournament.
type private Saved = { Name: string; Players: string list; Played: int }

let private items (xs: ResizeArray<'T>) = if obj.ReferenceEquals(xs, null) then [] else List.ofSeq xs

let private saved mode (file: string) : Saved option =
  match mode with
  | Cup ->
      let agent = TournamentState.startCupBracketReaderWriter file
      let state = agent.PostAndReply(fun reply -> ReadCupBracket reply)
      agent.Post DisposeCupBracket
      state |> Option.map (fun b ->
        let matches = items b.Rounds |> List.collect (fun r -> items r.Matches)
        { Name = b.TournamentName
          Players = matches |> List.collect (fun m -> [ m.PlayerA; m.PlayerB ])
          Played = matches |> List.sumBy (fun m -> (items m.Games).Length) })
  | Swiss ->
      let agent = TournamentState.startSwissStateReaderWriter file
      let state = agent.PostAndReply(fun reply -> ReadSwissState reply)
      agent.Post DisposeSwissState
      state |> Option.map (fun s ->
        let pairs = items s.Rounds |> List.collect (fun r -> items r.Pairings)
        { Name = s.TournamentName
          Players = pairs |> List.collect (fun p -> [ p.PlayerA; p.PlayerB ])
          Played = pairs |> List.sumBy (fun p -> (items p.Games).Length) })
  | Ladder ->
      let agent = TournamentState.startLadderStateReaderWriter file
      let state = agent.PostAndReply(fun reply -> ReadLadderState reply)
      agent.Post DisposeLadderState
      state |> Option.map (fun l ->
        let matches = items l.Matches
        { Name = l.TournamentName
          Players = items l.InitialRankings
          Played = matches |> List.sumBy (fun m -> (items m.Games).Length) })

// unreadable counts as having games: a state is never set aside on a doubt
let private pgnHasGames (pgn: string) =
  File.Exists pgn
  && (try
        use reader = new StreamReader(new FileStream(pgn, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete))
        let rec scan () =
          match reader.ReadLine() with
          | null -> false
          | line when line.StartsWith("[Event ", StringComparison.Ordinal) -> true
          | _ -> scan ()
        scan ()
      with _ -> true)

let private belongs (tourny: Tournament) (s: Saved) =
  let engines = tourny.EngineSetup.Engines |> List.map _.Name |> Set.ofList
  let isPlayer (p: string) = not (String.IsNullOrWhiteSpace p) && p <> "TBD" && p <> "BYE"
  String.Equals(s.Name, tourny.Name, StringComparison.Ordinal)
  && s.Players |> List.filter isPlayer |> List.forall engines.Contains

/// A tournament's state file made ready for a run, and what was done to it (for the log):
/// - none of its own, but the shared file holds this tournament (same name, its engines) and the
///   PGN has its games: the shared file is taken over (a run paused before state files moved) and
///   renamed .migrated, so a Restart that deletes the new file is not undone by a second takeover;
/// - it records games but the PGN has none (deleted to start over): set aside as .bak, so the
///   tournament starts afresh. A tournament without a PGN path keeps its state.
let prepare (baseDir: string) (tourny: Tournament) mode : string * string option =
  let file = path baseDir tourny mode
  let pgnGames () = not (String.IsNullOrWhiteSpace tourny.PgnOutPath) && pgnHasGames (rooted baseDir tourny.PgnOutPath)
  let dir = Path.GetDirectoryName file
  if not (String.IsNullOrWhiteSpace dir) then Directory.CreateDirectory dir |> ignore
  // a file that cannot be read (cut short by a crash, say) is set aside as .corrupt: a run that
  // started afresh would overwrite it, and the PGN still has its games (the guard below then asks)
  let corrupt =
    if File.Exists file && (saved mode file).IsNone then
      File.Move(file, file + ".corrupt", true)
      Some $"{file} could not be read: set aside as .corrupt"
    else None
  let takenOver =
    let old = shared baseDir mode
    if File.Exists file || not (File.Exists old) || String.Equals(old, file, StringComparison.OrdinalIgnoreCase) then None
    else
      match saved mode old with
      | Some s when s.Played > 0 && belongs tourny s && pgnGames () ->
          File.Copy(old, file)
          File.Move(old, old + ".migrated", true)
          Some $"{file}: taken over from {old} (renamed .migrated)"
      | _ -> None
  let setAside =
    // with no PGN at all (allowed: nothing is written) the state is all there is
    if not (File.Exists file) || String.IsNullOrWhiteSpace tourny.PgnOutPath then None
    else
      match saved mode file with
      | Some s when s.Played > 0 && not (pgnGames ()) ->
          File.Move(file, file + ".bak", true)
          Some $"{file} records {s.Played} games but the PGN has none: set aside as .bak, the tournament starts afresh"
      | _ -> None
  file, ([ corrupt; takenOver; setAside ] |> List.choose id |> function [] -> None | notes -> Some (String.concat "; " notes))

/// The games in the tournament's PGN (0 without a PGN path or file); unreadable counts as one.
let pgnGameCount (baseDir: string) (tourny: Tournament) =
  if String.IsNullOrWhiteSpace tourny.PgnOutPath then 0
  else
    let pgn = rooted baseDir tourny.PgnOutPath
    if not (File.Exists pgn) then 0
    else
      try
        use reader = new StreamReader(new FileStream(pgn, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete))
        let mutable count = 0
        let mutable line = reader.ReadLine()
        while not (isNull line) do
          if line.StartsWith("[Event ", StringComparison.Ordinal) then count <- count + 1
          line <- reader.ReadLine()
        count
      // unreadable counts as having games: a tournament is never mixed into a file on a doubt
      with _ -> 1

/// The games of the tournament's PGN when it has no state file to resume from: a cup, Swiss or
/// ladder started there afresh would mix two tournaments in one file. 0 when there is a state
/// file, no PGN path, or a PGN without games.
let orphanGames (baseDir: string) (tourny: Tournament) mode =
  if File.Exists(path baseDir tourny mode) then 0 else pgnGameCount baseDir tourny
