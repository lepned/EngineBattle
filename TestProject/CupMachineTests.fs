module CupMachineTests

open System
open System.IO
open System.Threading
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.CupTypes
open ChessLibrary.TournamentPairing
open ChessLibrary.ChessUtilities

// ---------------------------------------------------------------------------
// The Cup runner against golden traces of the runner it replaces: every game asked for, every
// pairing list sent, the totals and the final bracket. TestData/CupGolden holds the old runner's
// traces (recorded with EB_WRITE_GOLDEN=1 while it existed).
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
    Strategy: PairingHelper.CupSeedingStrategy
    Unique: bool
    Random: bool
    Increments: int list
    Failures: Set<int>
    ResumeAt: int option
    Result: string -> string -> string -> string }

let private tournamentFor (sc: Scenario) dir =
  let engines = [ for i in 1 .. sc.Players -> { EngineConfig.Empty with Name = sprintf "e%d" i; Rating = 3000 - i * 10 } ]
  { Tournament.Empty with
      Name = sc.Name
      TournamentMode = "Cup"
      Rounds = 2
      ConsoleOnly = true
      PgnOutPath = Path.Combine(dir, "out.pgn")
      Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = 11 }
      CupOptions =
        { RoundPairIncrements = sc.Increments; SeedingStrategy = string sc.Strategy; UniquePerMatchOnly = sc.Unique
          BracketPath = Path.Combine(dir, "cup_bracket.json"); RandomOpenings = sc.Random }
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines } }

let private bracketLines (b: CupBracket) =
  [ yield sprintf "bracket next=%d order=%s" b.NextOpeningIndex (String.Join(",", b.GlobalOpeningOrder))
    for r in b.Rounds do
      for m in r.Matches do
        yield sprintf "  r%d #%d %s-%s %.1f-%.1f winner=%s %b order=%s" r.RoundNumber m.MatchId m.PlayerA m.PlayerB m.ScoreA m.ScoreB
                (defaultArg m.Winner "-") m.IsDecided (String.Join(",", m.OpeningOrder))
        for g in m.Games do
          yield sprintf "    g%d %s-%s %s %s" g.GameNr g.White g.Black (GoldenBook.opening g.OpeningHash) g.Result ]

let private pairingLine (list: ResizeArray<Pairing>) =
  "pairings " + String.Join(" ", list |> Seq.map (fun p -> sprintf "%s:%s-%s#%d" p.RoundNr p.White.Name p.Black.Name p.GameNr))

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

let private readBracket path =
  let file = TournamentState.startCupBracketReaderWriter path
  let saved = file.PostAndReply(fun r -> CupBracketMessage.ReadCupBracket r)
  file.Post CupBracketMessage.DisposeCupBracket
  saved

/// The Cup runner, one run: its trace.
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
  TournamentRunners.cupWith (Some play) sc.Strategy sc.Unique false NullLogger.Instance tourny callback cts (fun () -> None) None
  |> Async.RunSynchronously |> ignore
  match readBracket tourny.CupOptions.BracketPath with
  | Some b -> trace.AddRange (bracketLines b)
  | None -> trace.Add "no bracket"
  if cts.IsCancellationRequested then trace.Add "cancelled"
  List.ofSeq trace, calls + player.Calls

let private scenarios : Scenario list =
  let baseSc =
    { Name = ""; Players = 4; Strategy = PairingHelper.CupSeedingStrategy.ByRating; Unique = false; Random = false
      Increments = []; Failures = Set.empty; ResumeAt = None; Result = alternating }
  [ { baseSc with Name = "rating-book-order" }
    { baseSc with Name = "random-draw-random-openings"; Players = 8; Strategy = PairingHelper.CupSeedingStrategy.Random; Random = true }
    { baseSc with Name = "unique-random"; Players = 8; Unique = true; Random = true }
    { baseSc with Name = "pair-increments"; Players = 4; Increments = [ 1; 2 ] }
    // the first two games of every match drawn: every match goes to tiebreak pairs
    { baseSc with
        Name = "tiebreaks"; Players = 4
        Result = fun w b round ->
          let game = int (round.Substring(round.IndexOf '.' + 1))
          if game <= 2 then "1/2-1/2" else alternating w b round }
    { baseSc with Name = "failures-not-in-a-row"; Players = 4; Failures = Set.ofList [ 1; 3 ] }
    // three in a row: the match is abandoned and the run stopped
    { baseSc with Name = "abandon"; Players = 4; Failures = Set.ofList [ 2; 3; 4 ] }
    { baseSc with Name = "resume-mid-match"; Players = 8; Random = true; ResumeAt = Some 5 }
    { baseSc with Name = "resume-at-round"; Players = 8; Unique = true; ResumeAt = Some 8 } ]

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "TestData", "CupGolden")

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-cup-" + Guid.NewGuid().ToString "N")
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
let ``the Cup runner plays the tournament the old runner played, game for game`` (name: string) =
  let sc = scenarios |> List.find (fun s -> s.Name = name)
  let trace = runnerTrace sc
  let golden = Path.Combine(goldenDir, name + ".txt")
  if Environment.GetEnvironmentVariable "EB_WRITE_GOLDEN" = "1" then
    Directory.CreateDirectory goldenDir |> ignore
    File.WriteAllLines(golden, trace)
  Assert.True(File.Exists golden, $"no golden trace for {name}")
  Assert.Equal<string list>(List.ofArray (File.ReadAllLines golden), trace)

// ---- the machine directly: rules the bracket traces cannot see -------------------------

let private machineFor (sc: Scenario) (loaded: CupBracket option) =
  let dir = freshDir ()
  let tourny = tournamentFor sc dir
  let openings = GameHelpers.loadOpeningsUnlimited tourny.Opening.OpeningsPath tourny.Rounds |> fst |> List.ofArray
  CupMachine.create (CupMachine.configOf tourny sc.Strategy sc.Unique openings) loaded 0, openings

let private plays effects = effects |> List.choose (function ModeRunner.Play p -> Some p | _ -> None)

/// Plays every game asked for with `result`, until `stop` says so; every step's effects.
let private playOut (st: CupMachine.State) (result: Pairing -> string) (stop: Pairing -> bool) =
  let log = ResizeArray<ModeRunner.Effect<CupBracket> list>()
  let rec go st event =
    let st, effects = CupMachine.step st event
    log.Add effects
    match plays effects with
    | [ p ] when not (stop p) ->
        go st (ModeRunner.GameEnded (Some (createResult p.White.Name p.Black.Name (ResizeArray()) (result p) MiscTypes.ResultReason.Checkmate 1000L)))
    | _ -> st
  let st = go st ModeRunner.Start
  st, List.ofSeq log

[<Fact>]
let ``a match the leader can no longer lose ends early and its unplayed games come off the total`` () =
  // four games a match; the lower-numbered player wins every game
  let sc = { (scenarios |> List.head) with Name = "early"; Increments = [ 2 ] }
  let st, _ = machineFor sc None
  let wins (p: Pairing) = if String.CompareOrdinal(p.White.Name, p.Black.Name) < 0 then "1-0" else "0-1"
  let _, log = playOut st wins (fun _ -> false)
  let effects = List.concat log
  Assert.Contains(ModeRunner.AddTotalGames -1, effects)       // 3-0 with one game left
  let final = effects |> List.choose (function ModeRunner.Persist b -> Some b | _ -> None) |> List.last
  Assert.True(final.Rounds.[0].Matches |> Seq.forall (fun m -> m.IsDecided && m.Games.Count = 3))

[<Fact>]
let ``the winner is in the next round's slot in the very save that records the deciding game`` () =
  let sc = { (scenarios |> List.head) with Name = "propagate" }
  let st, _ = machineFor sc None
  // each match's first game to white, its second to black: player A takes the match 2-0 (matches
  // start on odd game numbers; labels number the whole round, so they cannot tell)
  let result (p: Pairing) = if p.GameNr % 2 = 1 then "1-0" else "0-1"
  let _, log = playOut st result (fun p -> p.RoundNr.StartsWith "2.")
  // the step after a match's deciding game: its first save already holds the next round's slot
  let saves =
    log |> List.tryPick (fun effects ->
      match effects |> List.choose (function ModeRunner.Persist b -> Some b | _ -> None) with
      | first :: _ when first.Rounds.[0].Matches.[0].IsDecided && first.Rounds.[0].Matches.[0].Games.Count = 2 -> Some first
      | _ -> None)
  let first = saves.Value
  let winner = first.Rounds.[0].Matches.[0].Winner.Value
  Assert.Equal(winner, first.Rounds.[1].Matches.[0].PlayerA)

[<Fact>]
let ``a match's next opening is the first one it has not used, from where it stands`` () =
  // a resumed match that played book opening 1 twice: the next pair takes opening 2, not 1 again
  let sc = { (scenarios |> List.head) with Name = "unused" }
  let st0, openings = machineFor sc None
  let fresh = CupMachine.toFile st0
  let m = fresh.Rounds.[0].Matches.[0]
  let hash1 = Hash.computeOpeningHashFromGame openings.[1]
  let game nr white black : CupGame = { GameNr = nr; White = white; Black = black; OpeningId = "2"; OpeningHash = hash1; Result = "1/2-1/2" }
  m.Games.Add (game 1 m.PlayerA m.PlayerB)
  m.Games.Add (game 2 m.PlayerB m.PlayerA)
  m.ScoreA <- 1.0
  m.ScoreB <- 1.0
  let st, _ = machineFor sc (Some fresh)
  let _, effects = CupMachine.step st ModeRunner.Start
  let first = (plays effects).Head
  Assert.Equal(Hash.computeOpeningHashFromGame openings.[2], first.OpeningHash)
