module OpeningRuleTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.TournamentPairing

// ---------------------------------------------------------------------------
// The opening rules of Cup, Swiss and Ladder, checked on the games each machine asks for (the
// golden traces only say nothing changed; a cup broke the tournament-wide rule with its traces green):
// - each pair plays one opening with the colours swapped;
// - unique in the tournament: no opening again before the whole book is used;
// - unique per match: no opening twice in one match before its book is used;
// - random openings: the same seed plays the same games, another seed another order, neither the book's.
// ---------------------------------------------------------------------------

let private bookSize = GoldenBook.bookLines.Length

let private freshDir () =
  let d = Path.Combine(Path.GetTempPath(), "eb-openings-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory d |> ignore
  d

/// Mostly draws, so matches tie and tiebreak pairs are played too.
let private resultOf (i: int) =
  [| "1/2-1/2"; "1-0"; "1/2-1/2"; "1/2-1/2"; "0-1"; "1-0"; "1/2-1/2" |].[(i - 1) % 7]

/// The games a machine asks for, every one played, in order.
let private playAll (step: 'M -> ModeRunner.Event -> 'M * ModeRunner.Effect<'S> list) (machine: 'M) : Pairing list =
  let games = ResizeArray<Pairing>()
  let rec loop machine event =
    let machine, effects = step machine event
    match effects |> List.tryPick (function ModeRunner.Play p -> Some p | _ -> None) with
    | Some p when games.Count < 1000 ->
        games.Add p
        let r = resultOf games.Count
        loop machine (ModeRunner.GameEnded (Some (createResult p.White.Name p.Black.Name (ResizeArray()) r MiscTypes.ResultReason.Checkmate 1000L)))
    | _ -> ()
  loop machine ModeRunner.Start
  Assert.True(games.Count < 1000, "the tournament did not end")
  List.ofSeq games

/// A match: its round (the label before the dot) and its two players, either colour.
let private matchOf (p: Pairing) =
  let round = p.RoundNr.Substring(0, p.RoundNr.IndexOf '.')
  round, min p.White.Name p.Black.Name, max p.White.Name p.Black.Name

/// The pairs in play order: a match's games on one opening, two at most (a match decided after the
/// first game of a pair leaves it at one).
let private pairsOf (games: Pairing list) =
  games
  |> List.fold (fun (acc: Pairing list list) g ->
      match acc with
      | (last :: _ as unit) :: rest when matchOf last = matchOf g && last.OpeningHash = g.OpeningHash && unit.Length < 2 ->
          (g :: unit) :: rest
      | _ -> [ g ] :: acc) []
  |> List.rev |> List.map List.rev

/// Openings in a row with one twice among any `bookSize` of them.
let private repeatsWithinBook (openings: string list) =
  openings |> List.windowed (min bookSize openings.Length)
  |> List.filter (fun w -> (List.distinct w).Length < w.Length)

let private colourRule (games: Pairing list) =
  let pairs = pairsOf games
  Assert.True(pairs |> List.exists (fun u -> u.Length = 2), "no pair played out")
  for u in pairs do
    match u with
    | [ a; b ] -> Assert.True((a.White.Name = b.Black.Name && a.Black.Name = b.White.Name), $"{a.RoundNr}: colours not swapped in the pair")
    | _ -> ()

let private tournamentWide (games: Pairing list) =
  let openings = pairsOf games |> List.map (fun u -> u.Head.OpeningHash)
  Assert.True(openings.Length > bookSize, $"only {openings.Length} pairs: the book is never used up")
  Assert.Empty(repeatsWithinBook openings |> List.map (List.map GoldenBook.opening >> String.concat " "))

let private perMatch (games: Pairing list) =
  let byMatch = pairsOf games |> List.groupBy (fun u -> matchOf u.Head)
  Assert.True(byMatch |> List.exists (fun (_, us) -> us.Length > 1), "no match of more than one pair")
  for (m, us) in byMatch do
    Assert.True((repeatsWithinBook (us |> List.map (fun u -> u.Head.OpeningHash))).IsEmpty, $"{m}: an opening twice in the match")

/// The order the pairs take their openings in.
let private openingOrder (games: Pairing list) = pairsOf games |> List.map (fun u -> GoldenBook.opening u.Head.OpeningHash)

let private engines n = [ for i in 1 .. n -> { EngineConfig.Empty with Name = sprintf "e%02d" i; Rating = 3000 - i * 10 } ]

let private tournament mode players seed =
  let dir = freshDir ()
  let t =
    { Tournament.Empty with
        Name = "openings"
        TournamentMode = mode
        Rounds = 2
        ConsoleOnly = true
        PgnOutPath = Path.Combine(dir, "out.pgn")
        Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = seed }
        EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines players } }
  t, GameHelpers.loadOpeningsUnlimited t.Opening.OpeningsPath t.Rounds |> fst |> List.ofArray

// 16 players: 15 matches and their tiebreaks, more pairs than the book has openings
let private cup unique random seed =
  let t, openings = tournament "Cup" 16 seed
  let t = { t with CupOptions = { Tournament.Empty.CupOptions with UniquePerMatchOnly = unique; RandomOpenings = random; RoundPairIncrements = [ 2 ] } }
  playAll CupMachine.step (CupMachine.create (CupMachine.configOf t PairingHelper.CupSeedingStrategy.ByRating unique openings) None 0)

// 8 players, 3 rounds of 4-game matches: 24 pairs
let private swiss unique random seed =
  let t, openings = tournament "Swiss" 8 seed
  let t = { t with SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 4; Rounds = 3; UniquePerMatchOnly = unique; RandomOpenings = random } }
  playAll SwissMachine.step (SwissMachine.create (SwissMachine.configOf t openings) None 0)

let private ladder random seed =
  let t, openings = tournament "Ladder" 9 seed
  let t = { t with LadderOptions = { Tournament.Empty.LadderOptions with GamePairsPerMatch = 2; RandomOpenings = random } }
  playAll LadderMachine.step (LadderMachine.create (LadderMachine.configOf t openings) None 0)

let modes : obj[] seq =
  seq { for mode in [ "Cup"; "Swiss" ] do
          for unique in [ false; true ] do
            for random in [ false; true ] -> [| box mode; box unique; box random |]
        for random in [ false; true ] -> [| box "Ladder"; box false; box random |] }

let private play mode unique random seed =
  match mode with
  | "Cup" -> cup unique random seed
  | "Swiss" -> swiss unique random seed
  | _ -> ladder random seed

[<Theory>]
[<MemberData(nameof modes)>]
let ``each pair plays one opening with the colours swapped`` (mode: string) (unique: bool) (random: bool) =
  colourRule (play mode unique random 11)

[<Theory>]
[<MemberData(nameof modes)>]
let ``openings follow their uniqueness rule`` (mode: string) (unique: bool) (random: bool) =
  let games = play mode unique random 11
  if unique then perMatch games else tournamentWide games

[<Theory>]
[<MemberData(nameof modes)>]
let ``random openings are fixed by the seed and are not the book's order`` (mode: string) (unique: bool) (random: bool) =
  if random then
    let a = play mode unique true 11
    let describe (games: Pairing list) = games |> List.map (fun p -> sprintf "%s %s-%s %s" p.RoundNr p.White.Name p.Black.Name p.OpeningHash)
    Assert.Equal<string list>(describe a, describe (play mode unique true 11))
    Assert.NotEqual<string list>(openingOrder a, openingOrder (play mode unique true 12))
    Assert.NotEqual<string list>(openingOrder a, openingOrder (play mode unique false 11))
