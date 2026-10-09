module SwissMachineTests

open System
open System.IO
open System.Threading
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.SwissTypes

// ---------------------------------------------------------------------------
// The Swiss runner (on ModeRunner and SwissMachine) and the bare machine against the runner they
// replaced: the same tournament, the same results, game for game. A trace is every game asked
// for, every pairing list sent, the totals and the final state file. The golden traces in
// TestData/SwissGolden were recorded from the old runner before it was removed (2383a5c).
// ---------------------------------------------------------------------------

type private Scenario =
  { Name: string
    Players: int
    GamesPerMatch: int
    Rounds: int
    Unique: bool
    Random: bool
    Tie: bool
    /// Calls (0-based) that come back unplayed.
    Failures: Set<int>
    /// Stop the first run at this call and resume from its file.
    ResumeAt: int option
    /// The result of a game: white, black, round label.
    Result: string -> string -> string -> string }

/// A stable hash: String.GetHashCode differs from process to process, so results and goldens would too.
let private stable (s: string) = s |> Seq.fold (fun h c -> (h * 31 + int c) % 1000003) 7

let private alternating white black (round: string) =
  match (stable white + 3 * stable black + 7 * stable round) % 3 with
  | 0 -> "1-0"
  | 1 -> "0-1"
  | _ -> "1/2-1/2"

let private tournamentFor (sc: Scenario) dir =
  let engines =
    [ for i in 1 .. sc.Players ->
        { EngineConfig.Empty with Name = sprintf "e%d" i; Rating = 3000 - i * 10 } ]
  { Tournament.Empty with
      Name = sc.Name
      TournamentMode = "Swiss"
      Rounds = sc.Rounds
      ConsoleOnly = true
      PgnOutPath = Path.Combine(dir, "out.pgn")
      Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = 11 }
      SwissOptions =
        { GamesPerMatch = sc.GamesPerMatch; Rounds = sc.Rounds; SeedGroupCount = 2; UniquePerMatchOnly = sc.Unique
          RandomOpenings = sc.Random; AllowExtraPairsOnTie = sc.Tie; StatePath = Path.Combine(dir, "swiss_state.json") }
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines } }

let private stateLines (s: SwissState) =
  [ yield sprintf "state next=%d order=%s" s.NextOpeningIndex (String.Join(",", s.GlobalOpeningOrder))
    for r in s.Rounds do
      for p in r.Pairings do
        yield sprintf "  r%d #%d %s-%s %.1f-%.1f %b order=%s" r.RoundNumber p.PairId p.PlayerA p.PlayerB p.ScoreA p.ScoreB p.IsDecided (String.Join(",", p.OpeningOrder))
        for g in p.Games do
          yield sprintf "    g%d %s-%s %s %s" g.GameNr g.White g.Black (GoldenBook.opening g.OpeningHash) g.Result ]

let private pairingLine (list: ResizeArray<Pairing>) =
  "pairings " + String.Join(" ", list |> Seq.map (fun p -> sprintf "%s:%s-%s#%d" p.RoundNr p.White.Name p.Black.Name p.GameNr))

/// Shared by both runs: what a call returns, and whether the run stops there.
type private Player(sc: Scenario, stopAt: int option, start: int) =
  let mutable calls = 0
  member _.Calls = calls
  /// `i` counts calls across both runs of a resume, so failures and the stop are the same calls.
  member _.Answer (pair: Pairing) : Result option * bool =
    let i = start + calls
    calls <- calls + 1
    if stopAt = Some i then None, true
    elif sc.Failures.Contains i then None, false
    else Some (createResult pair.White.Name pair.Black.Name (ResizeArray()) (sc.Result pair.White.Name pair.Black.Name pair.RoundNr) MiscTypes.ResultReason.Checkmate 1000L), false

let private playLine (pair: Pairing) = sprintf "play #%d %s %s-%s %s" pair.GameNr pair.RoundNr pair.White.Name pair.Black.Name (GoldenBook.opening pair.OpeningHash)

/// The Swiss runner (setup, ModeRunner, machine), one run: its trace, read from what it asks and sends and the file it leaves.
let private runRunner (sc: Scenario) dir (stopAt: int option) (calls: int) =
  let tourny = tournamentFor sc dir
  let trace = ResizeArray<string>()
  let player = Player(sc, stopAt, calls)
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
  TournamentRunners.swissWith (Some play) NullLogger.Instance tourny callback cts (fun () -> None) None
  |> Async.RunSynchronously |> ignore
  let file = TournamentState.startSwissStateReaderWriter tourny.SwissOptions.StatePath
  let saved = file.PostAndReply(fun r -> SwissStateMessage.ReadSwissState r)
  file.Post SwissStateMessage.DisposeSwissState
  trace.AddRange (stateLines saved.Value)
  List.ofSeq trace, calls + player.Calls

/// The machine, one run, as the driver will run it.
let private runMachine (sc: Scenario) dir (loaded: SwissState option) (stopAt: int option) (calls: int) =
  let tourny = tournamentFor sc dir
  let openings = GameHelpers.loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds |> fst |> List.ofArray
  let cfg = SwissMachine.configOf tourny openings
  let trace = ResizeArray<string>()
  let player = Player(sc, stopAt, calls)
  let mutable total = SwissMachine.totalGames cfg
  trace.Add (sprintf "total %d" total)
  let mutable st = SwissMachine.create cfg loaded 0
  let mutable last = loaded
  let mutable pending = [ ModeRunner.Start ]
  let mutable running = true
  while running && not pending.IsEmpty do
    let event = pending.Head
    pending <- pending.Tail
    let next, effects = SwissMachine.step st event
    st <- next
    for e in effects do
      match e with
      | ModeRunner.Play pair ->
          trace.Add (playLine pair)
          let result, stop = player.Answer pair
          // as the driver: a cancelled run is told so, then how the game ended
          if stop then pending <- pending @ [ ModeRunner.Cancel; ModeRunner.GameEnded result ]
          else pending <- pending @ [ ModeRunner.GameEnded result ]
      | ModeRunner.Persist s -> last <- Some s
      | ModeRunner.Notify (Update.PairingList list) -> trace.Add (pairingLine list)
      | ModeRunner.Notify _ -> ()
      | ModeRunner.AddTotalGames n ->
          total <- total + n
          trace.Add (sprintf "total %d" total)
      | ModeRunner.Finished -> running <- false
      | ModeRunner.Info _ | ModeRunner.Critical _ | ModeRunner.Print _ | ModeRunner.StopRun -> ()
  trace.AddRange (stateLines last.Value)
  List.ofSeq trace, last.Value, calls + player.Calls

let private scenarios : Scenario list =
  let baseSc =
    { Name = ""; Players = 4; GamesPerMatch = 2; Rounds = 3; Unique = false; Random = false; Tie = false
      Failures = Set.empty; ResumeAt = None; Result = alternating }
  [ { baseSc with Name = "book-order" }
    { baseSc with Name = "random-openings"; Players = 6; Random = true }
    { baseSc with Name = "odd-players-bye"; Players = 5; Rounds = 4 }
    { baseSc with Name = "unique-random"; Players = 5; Unique = true; Random = true }
    { baseSc with Name = "unique-book-4-games"; Players = 4; GamesPerMatch = 4; Unique = true }
    { baseSc with Name = "failures"; Players = 4; Failures = Set.ofList [ 1; 4; 5; 6; 9 ] }
    { baseSc with Name = "tie-sonneborn-berger"; Players = 4; Tie = true; Result = fun _ _ _ -> "1/2-1/2" }
    { baseSc with
        Name = "tie-playoff"; Players = 4; Rounds = 3; Tie = true
        // e1 and e2 beat everyone and draw each other; e1 wins the playoff
        Result = fun w b round ->
          let playoff = round.StartsWith "4." || round.StartsWith "5."
          match w, b with
          | ("e1", "e2") | ("e2", "e1") when playoff -> if w = "e1" then "1-0" else "0-1"
          | ("e1", "e2") | ("e2", "e1") -> "1/2-1/2"
          | ("e1" | "e2"), _ -> "1-0"
          | _, ("e1" | "e2") -> "0-1"
          | _ -> "1/2-1/2" }
    // a failure leaves a pair on an odd count, so its next unit continues on the same opening
    { baseSc with Name = "unique-4-games-odd-units"; Players = 4; GamesPerMatch = 4; Unique = true; Failures = Set.ofList [ 0; 6; 13 ] }
    // more units than the book holds: the tournament's opening index wraps
    { baseSc with Name = "book-wraps"; Players = 6; Rounds = 4; GamesPerMatch = 4 }
    // fail, play, fail, fail: a played game resets the count, so the pair is not abandoned
    { baseSc with Name = "failures-not-in-a-row"; Players = 4; Failures = Set.ofList [ 0; 2; 3 ] }
    { baseSc with Name = "resume-mid-pair"; Players = 5; Random = true; ResumeAt = Some 3 }
    { baseSc with Name = "resume-unique"; Players = 6; Unique = true; Random = true; GamesPerMatch = 4; ResumeAt = Some 7 } ]

let private goldenDir =
  Path.Combine(__SOURCE_DIRECTORY__, "TestData", "SwissGolden")

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-swiss-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

/// Both runs of a scenario (a resume is two runs, the second from the first's file).
let private traces (sc: Scenario) =
  let oldDir, newDir = freshDir (), freshDir ()
  match sc.ResumeAt with
  | None ->
      let oldTrace, _ = runRunner sc oldDir None 0
      let newTrace, _, _ = runMachine sc newDir None None 0
      oldTrace, newTrace
  | Some at ->
      let old1, oldCalls = runRunner sc oldDir (Some at) 0
      // the file as the first run left it: the machine resumes from what the old runner wrote
      let file = TournamentState.startSwissStateReaderWriter (Path.Combine(oldDir, "swiss_state.json"))
      let saved = file.PostAndReply(fun r -> SwissStateMessage.ReadSwissState r)
      file.Post SwissStateMessage.DisposeSwissState
      GoldenBook.markPgnPlayed (Path.Combine(oldDir, "out.pgn"))
      let old2, _ = runRunner sc oldDir None oldCalls
      let new1, _, newCalls = runMachine sc newDir None (Some at) 0
      let new2, _, _ = runMachine sc newDir saved None newCalls
      old1 @ [ "--- resume" ] @ old2, new1 @ [ "--- resume" ] @ new2

let scenarioNames : obj[] seq = scenarios |> Seq.map (fun s -> [| box s.Name |])

[<Theory>]
[<MemberData(nameof scenarioNames)>]
let ``the Swiss machine plays the tournament the old runner played, game for game`` (name: string) =
  let sc = scenarios |> List.find (fun s -> s.Name = name)
  let oldTrace, newTrace = traces sc
  let golden = Path.Combine(goldenDir, name + ".txt")
  // the golden file is what the old runner played (the failures trace re-recorded when unplayable
  // games in a row came to stop the tournament instead of skipping the pair)
  if Environment.GetEnvironmentVariable "EB_WRITE_GOLDEN" = "1" then
    File.WriteAllLines(golden, oldTrace)
  Assert.True(File.Exists golden, $"no golden trace for {name}")
  Assert.Equal<string list>(List.ofArray (File.ReadAllLines golden), oldTrace)
  Assert.Equal<string list>(List.ofArray (File.ReadAllLines golden), newTrace)
  // first difference, readable
  let firstDiff =
    Seq.zip (Seq.append oldTrace (Seq.replicate 1 "<end>")) (Seq.append newTrace (Seq.replicate 1 "<end>"))
    |> Seq.indexed |> Seq.tryFind (fun (_, (a, b)) -> a <> b)
  match firstDiff with
  | Some (i, (a, b)) -> failwithf "line %d differs:\n old: %s\n new: %s" i a b
  | None -> Assert.Equal(oldTrace.Length, newTrace.Length)

[<Fact>]
let ``a resumed file's undecided bye is scored and saved before play goes on`` () =
  let dir = freshDir ()
  let sc = { (scenarios |> List.find (fun s -> s.Name = "odd-players-bye")) with Name = "bye-resume" }
  let tourny = tournamentFor sc dir
  let openings = GameHelpers.loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds |> fst |> List.ofArray
  let cfg = SwissMachine.configOf tourny openings
  let pairing a b bye : SwissPairing =
    { PairId = (if bye then 3 else 1); RoundNumber = 1; PlayerA = a; PlayerB = b; PlayerARating = 0; PlayerBRating = 0
      ScoreA = 0.0; ScoreB = 0.0; IsDecided = false; Games = ResizeArray(); OpeningOrder = ResizeArray() }
  let saved : SwissState =
    { TournamentName = "t"; SeedGroupCount = 2; GamesPerMatch = 2; UniqueOpeningsGlobal = true; NextOpeningIndex = 0
      GlobalOpeningOrder = ResizeArray()
      Rounds = ResizeArray [ { RoundNumber = 1; Pairings = ResizeArray [ pairing "e5" "BYE" true; pairing "e1" "e2" false ] } ]
      UpdatedUtc = DateTime.UtcNow }
  let _, effects = SwissMachine.step (SwissMachine.create cfg (Some saved) 0) ModeRunner.Start
  let bye =
    effects |> List.pick (function
      | ModeRunner.Persist s -> s.Rounds.[0].Pairings |> Seq.tryFind (fun p -> p.PlayerB = "BYE" && p.IsDecided)
      | _ -> None)
  Assert.Equal(1.0, bye.ScoreA)
  // and play goes on with the round's real pairing
  Assert.True(effects |> List.exists (function ModeRunner.Play p -> p.White.Name = "e1" || p.White.Name = "e2" | _ -> false))
