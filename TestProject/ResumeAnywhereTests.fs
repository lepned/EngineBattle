module ResumeAnywhereTests

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.TournamentPairing

// ---------------------------------------------------------------------------
// A tournament stopped after any game and resumed from its file plays exactly what it would have
// played uninterrupted: the same games (pairings, colours, openings, results, numbers), the same
// final file, the same total. Results are mostly draws, so matches and Swiss tops keep tying and
// every stop point inside a tiebreak gets tried.
// ---------------------------------------------------------------------------

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-resume-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

/// The i-th game played (1-based): four draws in seven. The cycle is odd, so the two games of a
/// pair do not always mirror each other (mirrored results would tie every tiebreak pair for ever).
let private resultOf (i: int) =
  [| "1/2-1/2"; "1-0"; "1/2-1/2"; "1/2-1/2"; "0-1"; "1-0"; "1/2-1/2" |].[(i - 1) % 7]

/// Another cycle, mirrored pairs and all: it gives the Swiss a playoff.
let private resultOfTen (i: int) =
  match (i * 7919 + 13) % 10 with
  | 6 | 7 -> "1-0"
  | 8 | 9 -> "0-1"
  | _ -> "1/2-1/2"

/// A run that plays this many games has not ended.
let private gameLimit = 500

type private Outcome<'M, 'S> =
  { Games: string list
    Notes: string list
    Machine: 'M
    Saved: 'S option
    Total: int }

/// Steps a machine as ModeRunner.drive does. `stopAfter`: the run is cancelled while that game is
/// played; the machine hears Cancel, then the game's result, and the run ends.
let private run (results: int -> string) (step: 'M -> ModeRunner.Event -> 'M * ModeRunner.Effect<'S> list) (machine: 'M) (total: int)
                (playedBefore: int) (stopAfter: int option) : Outcome<'M, 'S> =
  let games = ResizeArray<string>()
  let notes = ResizeArray<string>()
  let mutable preview : Pairing list = []
  let mutable total = total
  let mutable saved = None
  let carry (effects: ModeRunner.Effect<'S> list) =
    let mutable next = None
    for e in effects do
      match e with
      | ModeRunner.Persist s -> saved <- Some s
      | ModeRunner.AddTotalGames n -> total <- total + n
      | ModeRunner.Play p -> next <- Some p
      | ModeRunner.Notify (Update.PairingList list) when list.Count > 0 -> preview <- List.ofSeq list
      | ModeRunner.Info text | ModeRunner.Critical text -> notes.Add text
      | _ -> ()
    next
  let rec loop machine event count =
    let machine, effects = step machine event
    match carry effects with
    | Some (p: Pairing) when count >= gameLimit ->
        games.Add(sprintf "STILL PLAYING after %d games" gameLimit)
        machine
    | Some (p: Pairing) ->
        // the preview shown before a game names it as it is played
        if not preview.IsEmpty && not (preview |> List.exists (fun q -> q.RoundNr = p.RoundNr && q.White.Name = p.White.Name && q.Black.Name = p.Black.Name)) then
          notes.Add(sprintf "PREVIEW MISSES %s %s-%s" p.RoundNr p.White.Name p.Black.Name)
        let r = results (playedBefore + count + 1)
        games.Add(sprintf "#%d %s %s-%s %s %s" p.GameNr p.RoundNr p.White.Name p.Black.Name (GoldenBook.opening p.OpeningHash) r)
        // "*": the game could not be played
        let result = if r = "*" then None else Some (createResult p.White.Name p.Black.Name (ResizeArray()) r MiscTypes.ResultReason.Checkmate 1000L)
        if stopAfter = Some (count + 1) then
          let machine, effects = step machine ModeRunner.Cancel
          carry effects |> ignore
          let machine, effects = step machine (ModeRunner.GameEnded result)
          carry effects |> ignore
          machine
        else loop machine (ModeRunner.GameEnded result) (count + 1)
    | None -> machine
  let machine = loop machine ModeRunner.Start 0
  { Games = List.ofSeq games; Notes = List.ofSeq notes; Machine = machine; Saved = saved; Total = total }

/// The file as written, without its time stamp.
let private fileText (state: 'S option) =
  match state with
  | None -> "no file"
  | Some s ->
      let node = JsonSerializer.SerializeToNode(s).AsObject()
      node.Remove "UpdatedUtc" |> ignore
      node.ToJsonString()

let private openingsOf (t: Tournament) =
  GameHelpers.loadOpeningsUnlimited t.Opening.OpeningsPath t.Rounds |> fst |> List.ofArray

let private engines n = [ for i in 1 .. n -> { EngineConfig.Empty with Name = sprintf "e%d" i; Rating = 3000 - i * 10 } ]

let private baseTournament dir mode players =
  { Tournament.Empty with
      Name = "resume-anywhere"
      TournamentMode = mode
      Rounds = 2
      ConsoleOnly = true
      PgnOutPath = Path.Combine(dir, "out.pgn")
      Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = 11 }
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines players } }

/// Labels numbered round.n in the order played: within each round 1, 2, 3 ... with no gap or repeat.
let private labelsInPlayOrder (games: string list) =
  let labels = games |> List.map (fun g -> (g.Split ' ').[1])
  labels
  |> List.groupBy (fun l -> l.Substring(0, l.IndexOf '.'))
  |> List.forall (fun (_, ls) -> (ls |> List.map (fun l -> int (l.Substring(l.IndexOf '.' + 1)))) = [ 1 .. ls.Length ])

/// Every stop point against the uninterrupted run; the failures, one line each.
let private compareAll (uninterrupted: Outcome<'M, 'S>) (interrupted: int -> Outcome<'M, 'S> * Outcome<'M, 'S>) =
  [ for k in 1 .. uninterrupted.Games.Length - 1 do
      let first, second = interrupted k
      let games = first.Games @ second.Games
      if games <> uninterrupted.Games then
        let at = List.zip (List.truncate (min games.Length uninterrupted.Games.Length) games)
                          (List.truncate (min games.Length uninterrupted.Games.Length) uninterrupted.Games)
                 |> List.tryFindIndex (fun (a, b) -> a <> b)
        let at = defaultArg at (min games.Length uninterrupted.Games.Length)
        yield sprintf "stop after %d: game %d is %s, uninterrupted %s" k (at + 1)
                (List.tryItem at games |> Option.defaultValue "none") (List.tryItem at uninterrupted.Games |> Option.defaultValue "none")
      elif fileText second.Saved <> fileText uninterrupted.Saved then
        yield sprintf "stop after %d: the final file differs" k
      elif second.Total <> uninterrupted.Total then
        yield sprintf "stop after %d: total %d, uninterrupted %d" k second.Total uninterrupted.Total ]

[<Fact>]
let ``a cup stopped after any game and resumed plays what it would have played`` () =
  let dir = freshDir ()
  let t = { baseTournament dir "Cup" 8 with CupOptions = { Tournament.Empty.CupOptions with BracketPath = Path.Combine(dir, "b.json") } }
  let cfg = CupMachine.configOf t PairingHelper.CupSeedingStrategy.ByRating false (openingsOf t)
  let start (loaded, played) = let m = CupMachine.create cfg loaded played in m, CupMachine.currentTotalGames m
  let whole = let m, total = start (None, 0) in run resultOf CupMachine.step m total 0 None
  let gpm = 2
  Assert.True(whole.Saved.Value.Rounds |> Seq.exists (fun r -> r.Matches |> Seq.exists (fun m -> m.Games.Count > gpm)), "no tiebreak in the scenario")
  let interrupted k =
    let m, total = start (None, 0)
    let first = run resultOf CupMachine.step m total 0 (Some k)
    let m, total = start (first.Saved, k)
    first, run resultOf CupMachine.step m total k None
  Assert.True(labelsInPlayOrder whole.Games, "labels not round.n in the order played")
  Assert.DoesNotContain(whole.Notes, fun n -> n.StartsWith "PREVIEW")
  Assert.Empty(compareAll whole interrupted)
  for k in 1 .. whole.Games.Length - 1 do
    let _, second = interrupted k
    Assert.DoesNotContain(second.Notes, fun n -> n.StartsWith "PREVIEW")

[<Fact>]
let ``a ladder stopped after any game and resumed plays what it would have played`` () =
  let dir = freshDir ()
  let t = { baseTournament dir "Ladder" 5 with LadderOptions = { GamePairsPerMatch = 1; RandomOpenings = false; StatePath = Path.Combine(dir, "l.json") } }
  let cfg = LadderMachine.configOf t (openingsOf t)
  let start (loaded, played) = let m = LadderMachine.create cfg loaded played in m, LadderMachine.totalGames m
  let whole = let m, total = start (None, 0) in run resultOf LadderMachine.step m total 0 None
  Assert.True(whole.Saved.Value.Matches |> Seq.exists (fun m -> m.Games.Count > 2), "no tiebreak in the scenario")
  let interrupted k =
    let m, total = start (None, 0)
    let first = run resultOf LadderMachine.step m total 0 (Some k)
    let m, total = start (first.Saved, k)
    first, run resultOf LadderMachine.step m total k None
  Assert.True(labelsInPlayOrder whole.Games, "labels not climb.n in the order played")
  Assert.DoesNotContain(whole.Notes, fun n -> n.StartsWith "PREVIEW")
  Assert.Empty(compareAll whole interrupted)
  for k in 1 .. whole.Games.Length - 1 do
    let _, second = interrupted k
    Assert.DoesNotContain(second.Notes, fun n -> n.StartsWith "PREVIEW")

[<Fact>]
let ``a Swiss stopped after any game and resumed plays what it would have played, playoff included`` () =
  let dir = freshDir ()
  let t =
    { baseTournament dir "Swiss" 4 with
        SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 2; Rounds = 3; AllowExtraPairsOnTie = true; StatePath = Path.Combine(dir, "s.json") } }
  let cfg = SwissMachine.configOf t (openingsOf t)
  let start (loaded, played) = let m = SwissMachine.create cfg loaded played in m, SwissMachine.totalGamesOf m
  let playoff (o: Outcome<_, SwissTypes.SwissState>) = o.Saved.Value.Rounds |> Seq.exists (fun r -> r.RoundNumber > 3)
  // every set of results (a cycle and an offset into it) whose Swiss ends in a playoff
  let candidates = [ for cycle in [ resultOfTen; resultOf ] do for offset in 0 .. 9 -> fun i -> cycle (i + offset) ]
  let withPlayoff = candidates |> List.filter (fun results -> let m, total = start (None, 0) in playoff (run results SwissMachine.step m total 0 None))
  Assert.True(withPlayoff.Length >= 2, "too few playoffs in the scenarios")
  let failures =
    [ for results in withPlayoff do
        let whole = let m, total = start (None, 0) in run results SwissMachine.step m total 0 None
        let interrupted k =
          let m, total = start (None, 0)
          let first = run results SwissMachine.step m total 0 (Some k)
          let m, total = start (first.Saved, k)
          first, run results SwissMachine.step m total k None
        yield! compareAll whole interrupted ]
  Assert.Empty(failures)

/// A Swiss whose three rounds leave two players tied at the top, then `playoff` for every game after.
let private swissPlayoff (playoff: string) =
  let dir = freshDir ()
  let t =
    { baseTournament dir "Swiss" 4 with
        SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 2; Rounds = 3; AllowExtraPairsOnTie = true; StatePath = Path.Combine(dir, "s.json") } }
  let cfg = SwissMachine.configOf t (openingsOf t)
  let regular = 12
  let tiedAfterRounds =
    [ for cycle in [ resultOfTen; resultOf ] do for offset in 0 .. 9 -> fun i -> cycle (i + offset) ]
    |> List.find (fun results ->
      let o = run results SwissMachine.step (SwissMachine.create cfg None 0) 0 0 None
      o.Saved.Value.Rounds |> Seq.exists (fun r -> r.RoundNumber > 3))
  run (fun i -> if i <= regular then tiedAfterRounds i else playoff) SwissMachine.step (SwissMachine.create cfg None 0) 0 0 None

[<Fact>]
let ``a Swiss playoff that cannot be played stops the tournament, its pair left for a resume`` () =
  let o = swissPlayoff "*"
  Assert.DoesNotContain(o.Games, fun g -> g.StartsWith "STILL PLAYING")
  Assert.Contains(o.Notes, fun n -> n.StartsWith "Stopping the Swiss")
  let playoff = o.Saved.Value.Rounds |> Seq.filter (fun r -> r.RoundNumber > 3) |> List.ofSeq
  Assert.Equal(1, playoff.Length)
  Assert.Contains(playoff.Head.Pairings, fun p -> not p.IsDecided)

[<Fact>]
let ``a Swiss stopped by unplayable games plays the pair on in its round when resumed`` () =
  let dir = freshDir ()
  let t =
    { baseTournament dir "Swiss" 6 with
        SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 2; Rounds = 3; StatePath = Path.Combine(dir, "s.json") } }
  let cfg = SwissMachine.configOf t (openingsOf t)
  // round 1's second pair cannot play: games 3, 4 and 5 fail
  let failing i = if i >= 3 && i <= 5 then "*" else resultOf i
  let first = run failing SwissMachine.step (SwissMachine.create cfg None 0) 0 0 None
  Assert.Contains(first.Notes, fun n -> n.StartsWith "Stopping the Swiss")
  Assert.Equal(1, first.Saved.Value.Rounds.Count)
  // the engine fixed: the resume plays that pair in round 1 before round 2 is paired
  let played = first.Games |> List.filter (fun g -> not (g.EndsWith "*")) |> List.length
  let m = SwissMachine.create cfg first.Saved played
  let second = run resultOf SwissMachine.step m (SwissMachine.totalGamesOf m) played None
  Assert.StartsWith("1.", (second.Games.Head.Split ' ').[1])
  let rounds = second.Saved.Value.Rounds |> List.ofSeq
  Assert.Equal<int list>([ 1; 2; 3 ], rounds |> List.map _.RoundNumber)
  Assert.All(rounds, fun r -> Assert.All(r.Pairings, fun p -> Assert.True(p.IsDecided, $"round {r.RoundNumber}: {p.PlayerA}-{p.PlayerB} undecided")))
  let met = rounds |> List.collect (fun r -> List.ofSeq r.Pairings) |> List.map (fun p -> if String.CompareOrdinal(p.PlayerA, p.PlayerB) <= 0 then p.PlayerA + "|" + p.PlayerB else p.PlayerB + "|" + p.PlayerA)
  Assert.Equal(met.Length, (List.distinct met).Length)

[<Fact>]
let ``a Swiss playoff drawn round after round ends after three by Sonneborn-Berger`` () =
  let o = swissPlayoff "1/2-1/2"
  Assert.DoesNotContain(o.Games, fun g -> g.StartsWith "STILL PLAYING")
  Assert.Equal(3, o.Saved.Value.Rounds |> Seq.filter (fun r -> r.RoundNumber > 3) |> Seq.length)
  Assert.Contains(o.Notes, fun n -> n.Contains "after 3 playoff rounds" && n.Contains "Sonneborn-Berger")

[<Fact>]
let ``a Swiss resumed without an engine that still has a pair to play stops and says so`` () =
  let dir = freshDir ()
  let t =
    { baseTournament dir "Swiss" 6 with
        SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 2; Rounds = 3; StatePath = Path.Combine(dir, "s.json") } }
  let cfg = SwissMachine.configOf t (openingsOf t)
  // stopped after the round's first game: its other pairs are still to play
  let first = run resultOf SwissMachine.step (SwissMachine.create cfg None 0) 0 0 (Some 1)
  let round1 = first.Saved.Value.Rounds.[0].Pairings |> List.ofSeq
  let open' = round1 |> List.find (fun p -> p.Games.Count = 0)
  // one of that pair's engines has left the config since
  let cfg = SwissMachine.configOf { t with EngineSetup = { t.EngineSetup with Engines = t.EngineSetup.Engines |> List.filter (fun e -> e.Name <> open'.PlayerB) } } (openingsOf t)
  let m = SwissMachine.create cfg first.Saved 1
  let second = run resultOf SwissMachine.step m (SwissMachine.totalGamesOf m) 1 None
  Assert.Contains(second.Notes, fun n -> n.StartsWith "Stopping the Swiss" && n.Contains open'.PlayerB && n.Contains "no longer in the tournament")
  // nothing played around the missing engine, nothing paired past it
  Assert.DoesNotContain(second.Games, fun g -> g.Contains open'.PlayerB)
  Assert.Equal(1, second.Saved |> Option.map (fun s -> s.Rounds.Count) |> Option.defaultValue 1)
