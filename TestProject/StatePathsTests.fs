module StatePathsTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament

// ---------------------------------------------------------------------------
// Where a cup, Swiss or ladder keeps its state: next to its PGN unless configured, an old shared
// file under wwwroot taken over when it holds this tournament, a state without its PGN's games
// set aside.
// ---------------------------------------------------------------------------

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-statepaths-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

let private tournament dir (statePath: string) =
  { Tournament.Empty with
      Name = "T"
      PgnOutPath = Path.Combine(dir, "pgns", "MyLadder.pgn")
      LadderOptions = { Tournament.Empty.LadderOptions with StatePath = statePath }
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = [ for n in [ "e1"; "e2" ] -> { EngineConfig.Empty with Name = n } ] } }

let private ladderJson name games =
  let gameList = String.Join(",", [ for i in 1 .. games -> sprintf """{"GameNr":%d,"White":"e2","Black":"e1","Result":"1-0"}""" i ])
  sprintf """{"TournamentName":"%s","InitialRankings":["e1","e2"],"SurvivingEngines":["e1","e2"],"EliminatedEngines":[],"Matches":[{"MatchId":1,"Challenger":"e2","Defender":"e1","Games":[%s]}]}""" name gameList

let private writePgn (t: Tournament) =
  Directory.CreateDirectory(Path.GetDirectoryName t.PgnOutPath) |> ignore
  File.WriteAllText(t.PgnOutPath, "[Event \"T\"]\n[White \"e2\"]\n[Black \"e1\"]\n[Result \"1-0\"]\n\n1-0\n")

[<Fact>]
let ``a state file not configured goes next to the PGN, named after it`` () =
  let dir = freshDir ()
  let expected = Path.Combine(dir, "pgns", "MyLadder_ladder_state.json")
  Assert.Equal(expected, StatePaths.path dir (tournament dir "") StatePaths.Ladder)
  // the shared default every config used to carry counts as not configured
  Assert.Equal(expected, StatePaths.path dir (tournament dir "wwwroot/ladder_state.json") StatePaths.Ladder)

[<Fact>]
let ``a configured state file is used as given, a relative one from the base folder`` () =
  let dir = freshDir ()
  Assert.Equal(Path.Combine(dir, "mine.json"), StatePaths.path dir (tournament dir (Path.Combine(dir, "mine.json"))) StatePaths.Ladder)
  Assert.Equal(Path.Combine(dir, "states", "mine.json"), StatePaths.path dir (tournament dir "states/mine.json") StatePaths.Ladder)

[<Fact>]
let ``with no PGN either, the state stays in the shared file`` () =
  let dir = freshDir ()
  let t = { tournament dir "" with PgnOutPath = "" }
  Assert.Equal(Path.Combine(dir, "wwwroot", "ladder_state.json"), StatePaths.path dir t StatePaths.Ladder)

[<Fact>]
let ``a tournament paused in the shared file is taken over, another tournament's is not`` () =
  let dir = freshDir ()
  Directory.CreateDirectory(Path.Combine(dir, "wwwroot")) |> ignore
  let shared = Path.Combine(dir, "wwwroot", "ladder_state.json")
  let t = tournament dir ""
  writePgn t
  File.WriteAllText(shared, ladderJson "another tournament" 1)
  let file, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.False(File.Exists file, "another tournament's state was taken over")
  Assert.True(note.IsNone)
  File.WriteAllText(shared, ladderJson "T" 1)
  let file, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.True(File.Exists file, "the paused tournament's state was not taken over")
  Assert.True(note.IsSome)
  Assert.True(File.Exists(shared + ".migrated"), "the shared file is kept, renamed")
  // a Restart deletes the new file: the old state must not come back
  File.Delete file
  let file, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.False(File.Exists file, "the old shared state was taken over again after a restart")
  Assert.True(note.IsNone)

[<Fact>]
let ``a state whose PGN has no games is set aside, a state with no games is kept`` () =
  let dir = freshDir ()
  let t = tournament dir ""
  let file = StatePaths.path dir t StatePaths.Ladder
  Directory.CreateDirectory(Path.GetDirectoryName file) |> ignore
  // nothing played yet: nothing to contradict the missing PGN
  File.WriteAllText(file, ladderJson "T" 0)
  let _, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.True(File.Exists file && note.IsNone, "a state with no games was set aside")
  // games recorded, but the PGN was deleted to start over
  File.WriteAllText(file, ladderJson "T" 2)
  let _, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.False(File.Exists file, "a stale state was kept")
  Assert.True(File.Exists(file + ".bak") && note.IsSome)
  // with the PGN's games present it is the tournament's state
  File.WriteAllText(file, ladderJson "T" 2)
  writePgn t
  let _, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.True(File.Exists file && note.IsNone, "a state with its PGN was set aside")

[<Fact>]
let ``a tournament without a PGN keeps its state: there is nothing to compare it with`` () =
  let dir = freshDir ()
  let t = { tournament dir "" with PgnOutPath = "" }
  let file = StatePaths.path dir t StatePaths.Ladder
  Directory.CreateDirectory(Path.GetDirectoryName file) |> ignore
  File.WriteAllText(file, ladderJson "T" 2)
  let prepared, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.Equal(file, prepared)
  Assert.True(File.Exists file && note.IsNone, "the state of a tournament without a PGN was set aside")

[<Fact>]
let ``a PGN with games and no state file to resume from is counted, so a new tournament does not mix into it`` () =
  let dir = freshDir ()
  let t = tournament dir ""
  // no PGN yet, or an empty one: nothing to mix with
  Assert.Equal(0, StatePaths.orphanGames dir t StatePaths.Ladder)
  Directory.CreateDirectory(Path.GetDirectoryName t.PgnOutPath) |> ignore
  File.WriteAllText(t.PgnOutPath, "")
  Assert.Equal(0, StatePaths.orphanGames dir t StatePaths.Ladder)
  // games and no state file: counted
  File.WriteAllText(t.PgnOutPath, "[Event \"A\"]\n[Result \"1-0\"]\n\n1-0\n\n[Event \"A\"]\n[Result \"0-1\"]\n\n0-1\n")
  Assert.Equal(2, StatePaths.orphanGames dir t StatePaths.Ladder)
  // a state file to resume from: not an orphan
  File.WriteAllText(StatePaths.path dir t StatePaths.Ladder, ladderJson "T" 2)
  Assert.Equal(0, StatePaths.orphanGames dir t StatePaths.Ladder)
  // no PGN path: nothing is written, nothing to mix
  Assert.Equal(0, StatePaths.orphanGames dir { t with PgnOutPath = "" } StatePaths.Ladder)

[<Fact>]
let ``a state file that cannot be read is set aside as .corrupt, not overwritten`` () =
  let dir = freshDir ()
  let t = tournament dir ""
  writePgn t
  let file = StatePaths.path dir t StatePaths.Ladder
  File.WriteAllText(file, "{\"TournamentName\": \"T\", \"Matches\": [")
  let prepared, note = StatePaths.prepare dir t StatePaths.Ladder
  Assert.Equal(file, prepared)
  Assert.False(File.Exists file, "the unreadable file was left to be overwritten")
  Assert.True(File.Exists(file + ".corrupt"))
  Assert.Contains(".corrupt", note.Value)
  // and the PGN's games then stop a new tournament from mixing in
  Assert.Equal(1, StatePaths.orphanGames dir t StatePaths.Ladder)
