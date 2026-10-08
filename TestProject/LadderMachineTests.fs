module LadderMachineTests

open System
open System.IO
open System.Threading
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.LadderTypes

// ---------------------------------------------------------------------------
// The Ladder runner against golden traces of the runner it replaces: every game asked for, every
// pairing list sent, the totals and the final ladder. TestData/LadderGolden holds the old
// runner's traces (recorded with EB_WRITE_GOLDEN=1 while it existed).
// ---------------------------------------------------------------------------

let private stable (s: string) = s |> Seq.fold (fun h c -> (h * 31 + int c) % 1000003) 7

let private alternating white black (round: string) =
  match (stable white + 3 * stable black + 7 * stable round) % 3 with
  | 0 -> "1-0"
  | 1 -> "0-1"
  | _ -> "1/2-1/2"

type private Scenario =
  { Name: string
    Players: int
    Pairs: int
    Random: bool
    Failures: Set<int>
    ResumeAt: int option
    Result: string -> string -> string -> string }

let private tournamentFor (sc: Scenario) dir =
  let engines = [ for i in 1 .. sc.Players -> { EngineConfig.Empty with Name = sprintf "e%d" i; Rating = 3000 - i * 10 } ]
  { Tournament.Empty with
      Name = sc.Name
      TournamentMode = "Ladder"
      Rounds = 2
      ConsoleOnly = true
      PgnOutPath = Path.Combine(dir, "out.pgn")
      Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = 11 }
      LadderOptions = { GamePairsPerMatch = sc.Pairs; RandomOpenings = sc.Random; StatePath = Path.Combine(dir, "ladder_state.json") }
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines } }

let private stateLines (s: LadderState) =
  [ yield sprintf "ladder next=%d order=%s climb=%d climber=%d" s.NextOpeningIndex (String.Join(",", s.GlobalOpeningOrder)) s.CurrentClimbNumber s.CurrentClimberIndex
    yield sprintf "  rankings=%s surviving=%s eliminated=%s" (String.Join(",", s.InitialRankings)) (String.Join(",", s.SurvivingEngines)) (String.Join(",", s.EliminatedEngines))
    for m in s.Matches do
      yield sprintf "  #%d climb%d %s-%s %.1f-%.1f winner=%s %b" m.MatchId m.ClimbNumber m.Challenger m.Defender m.ScoreChallenger m.ScoreDefender
              (defaultArg m.Winner "-") m.IsDecided
      for g in m.Games do
        yield sprintf "    g%d %s-%s %s %s" g.GameNr g.White g.Black (GoldenBook.opening g.OpeningHash) g.Result ]

let private pairingLine (list: ResizeArray<Pairing>) =
  "pairings " + String.Join(" ", list |> Seq.map (fun p -> sprintf "%s:%s-%s#%d:%s" p.RoundNr p.White.Name p.Black.Name p.GameNr (GoldenBook.opening p.OpeningHash)))

let private playLine (pair: Pairing) = sprintf "play #%d %s %s-%s %s" pair.GameNr pair.RoundNr pair.White.Name pair.Black.Name (GoldenBook.opening pair.OpeningHash)

/// The old runner's label for a game - round (or climb) and the game's number within its match -
/// that the goldens' results were drawn from: games now are numbered across the round. `meetings`
/// counts the games each pair has played there, over both runs of a resume.
let private oldLabel (meetings: Collections.Generic.Dictionary<string, int>) (pair: Pairing) =
  let prefix = pair.RoundNr.Substring(0, pair.RoundNr.IndexOf '.')
  let a, b = if String.CompareOrdinal(pair.White.Name, pair.Black.Name) < 0 then pair.White.Name, pair.Black.Name else pair.Black.Name, pair.White.Name
  let key = $"{prefix}|{a}|{b}"
  let n = match meetings.TryGetValue key with | true, v -> v | _ -> 0
  key, n, $"{prefix}.{n + 1}"

type private Player(sc: Scenario, stopAt: int option, start: int, meetings: Collections.Generic.Dictionary<string, int>) =
  let mutable calls = 0
  member _.Calls = calls
  member _.Answer (pair: Pairing) : Result option * bool =
    let i = start + calls
    calls <- calls + 1
    if stopAt = Some i then None, true
    elif sc.Failures.Contains i then None, false
    else
      let key, n, label = oldLabel meetings pair
      meetings.[key] <- n + 1
      Some (createResult pair.White.Name pair.Black.Name (ResizeArray()) (sc.Result pair.White.Name pair.Black.Name label) MiscTypes.ResultReason.Checkmate 1000L), false

let private readState path =
  let file = TournamentState.startLadderStateReaderWriter path
  let saved = file.PostAndReply(fun r -> LadderStateMessage.ReadLadderState r)
  file.Post LadderStateMessage.DisposeLadderState
  saved

let private runRunner (sc: Scenario) dir (stopAt: int option) (calls: int) meetings =
  let tourny = tournamentFor sc dir
  let trace = ResizeArray<string>()
  let player = Player(sc, stopAt, calls, meetings)
  use cts = new CancellationTokenSource()
  let play (pair: Pairing) = async {
    trace.Add (playLine pair)
    let result, stop = player.Answer pair
    if stop then cts.Cancel()
    return result }
  let callback (u: Update) =
    match u with
    | Update.PairingList list -> trace.Add (pairingLine list)
    | Update.TotalNumberOfPairs n -> trace.Add (sprintf "total %d" n)
    | _ -> ()
  TournamentRunners.ladderWith (Some play) NullLogger.Instance tourny callback cts (fun () -> None) None
  |> Async.RunSynchronously |> ignore
  match readState tourny.LadderOptions.StatePath with
  | Some s -> trace.AddRange (stateLines s)
  | None -> trace.Add "no state"
  if cts.IsCancellationRequested then trace.Add "cancelled"
  List.ofSeq trace, calls + player.Calls

let private scenarios : Scenario list =
  let baseSc = { Name = ""; Players = 4; Pairs = 1; Random = false; Failures = Set.empty; ResumeAt = None; Result = alternating }
  [ { baseSc with Name = "book-order" }
    { baseSc with Name = "random-openings"; Players = 5; Random = true }
    { baseSc with
        Name = "tiebreaks"
        Result = fun w b round ->
          let game = int (round.Substring(round.IndexOf '.' + 1))
          if game <= 2 then "1/2-1/2" else alternating w b round }
    // two pairs a match, the lower-numbered engine always wins: matches end early
    { baseSc with Name = "early-decisions"; Pairs = 2; Result = fun w b _ -> if String.CompareOrdinal(w, b) < 0 then "1-0" else "0-1" }
    { baseSc with Name = "failures-not-in-a-row"; Failures = Set.ofList [ 1; 3 ] }
    { baseSc with Name = "abandon"; Failures = Set.ofList [ 2; 3; 4 ] }
    { baseSc with Name = "resume-mid-match"; Players = 5; Random = true; ResumeAt = Some 3 }
    { baseSc with Name = "resume-new-match"; Players = 5; Pairs = 2; ResumeAt = Some 4 } ]

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "TestData", "LadderGolden")

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-ladder-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

let private runnerTrace (sc: Scenario) =
  let dir = freshDir ()
  match sc.ResumeAt with
  | None -> fst (runRunner sc dir None 0 (Collections.Generic.Dictionary()))
  | Some at ->
      let meetings = Collections.Generic.Dictionary()
      let first, calls = runRunner sc dir (Some at) 0 meetings
      GoldenBook.markPgnPlayed (Path.Combine(dir, "out.pgn"))
      let second, _ = runRunner sc dir None calls meetings
      first @ [ "--- resume" ] @ second

let scenarioNames : obj[] seq = scenarios |> Seq.map (fun s -> [| box s.Name |])

[<Theory>]
[<MemberData(nameof scenarioNames)>]
let ``the Ladder runner plays the tournament the old runner played, game for game`` (name: string) =
  let sc = scenarios |> List.find (fun s -> s.Name = name)
  let trace = runnerTrace sc
  let golden = Path.Combine(goldenDir, name + ".txt")
  if Environment.GetEnvironmentVariable "EB_WRITE_GOLDEN" = "1" then
    Directory.CreateDirectory goldenDir |> ignore
    File.WriteAllLines(golden, trace)
  Assert.True(File.Exists golden, $"no golden trace for {name}")
  Assert.Equal<string list>(List.ofArray (File.ReadAllLines golden), trace)

[<Fact>]
let ``a match started in a cancelled run does not move the ladder, even if its last game decided it`` () =
  let sc = { (scenarios |> List.head) with Name = "cancel-decides" }
  let dir = freshDir ()
  let tourny = tournamentFor sc dir
  let openings = GameHelpers.loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds |> fst |> List.ofArray
  let st = LadderMachine.create (LadderMachine.configOf tourny openings) None 0
  let result (p: Pairing) r = Some (createResult p.White.Name p.Black.Name (ResizeArray()) r MiscTypes.ResultReason.Checkmate 1000L)
  let play effects = effects |> List.pick (function ModeRunner.Play p -> Some p | _ -> None)
  let st, effects = LadderMachine.step st ModeRunner.Start
  let first = play effects
  let st, effects = LadderMachine.step st (ModeRunner.GameEnded (result first (if first.White.Name = "e4" then "1-0" else "0-1")))
  let second = play effects
  // the run is cancelled; the second game still comes back with the challenger's second win
  let st, _ = LadderMachine.step st ModeRunner.Cancel
  let _, effects = LadderMachine.step st (ModeRunner.GameEnded (result second (if second.White.Name = "e4" then "1-0" else "0-1")))
  let saved = effects |> List.choose (function ModeRunner.Persist s -> Some s | _ -> None) |> List.last
  Assert.True(saved.Matches.[0].IsDecided)                  // the game is recorded and decides the match
  Assert.Equal<string list>([ "e1"; "e2"; "e3"; "e4" ], List.ofSeq saved.SurvivingEngines)   // but nobody is out

[<Fact>]
let ``a ladder resumed without one of its engines still saves the match it decides`` () =
  let sc = { (scenarios |> List.head) with Name = "engine-removed" }
  let dir = freshDir ()
  let tourny = tournamentFor sc dir
  let openings = GameHelpers.loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds |> fst |> List.ofArray
  let cfg = LadderMachine.configOf tourny openings
  let result (p: Pairing) r = Some (createResult p.White.Name p.Black.Name (ResizeArray()) r MiscTypes.ResultReason.Checkmate 1000L)
  let play effects = effects |> List.pick (function ModeRunner.Play p -> Some p | _ -> None)
  let saved effects = effects |> List.choose (function ModeRunner.Persist s -> Some s | _ -> None)
  // the first match (e4 climbing against e3) is saved before its first game
  let _, effects = LadderMachine.step (LadderMachine.create cfg None 0) ModeRunner.Start
  let file = saved effects |> List.last
  // e1, not in that match, has left the config since
  let cfg = { cfg with Players = cfg.Players |> List.filter (fun e -> e.Name <> "e1") }
  let st, effects = LadderMachine.step (LadderMachine.create cfg (Some file) 0) ModeRunner.Start
  let win (p: Pairing) = result p (if p.White.Name = "e4" then "1-0" else "0-1")
  let first = play effects
  let st, effects = LadderMachine.step st (ModeRunner.GameEnded (win first))
  let _, effects = LadderMachine.step st (ModeRunner.GameEnded (win (play effects)))
  let decided = saved effects |> List.tryFind (fun s -> s.Matches |> Seq.exists (fun m -> m.IsDecided))
  Assert.True(decided.IsSome, "the decided match was not saved")
