module SwissPairingRuleTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes

// ---------------------------------------------------------------------------
// A Swiss of n players runs any number of rounds up to n - 1 without a rematch: such a schedule
// always exists (a round robin), so the pairing must never pair itself into a corner. With 6
// players a third round can leave two triangles of unplayed pairs (A-B-C, D-E-F), and no fourth
// round can be paired from them.
// ---------------------------------------------------------------------------

/// Result cycles: draws, wins, losses in different orders, so the score groups differ run to run.
let private cycles =
  [ [| "1/2-1/2" |]
    [| "1-0" |]
    [| "0-1" |]
    [| "1-0"; "0-1" |]
    [| "1/2-1/2"; "1-0"; "1/2-1/2"; "1/2-1/2"; "0-1"; "1-0"; "1/2-1/2" |]
    [| "1-0"; "1-0"; "0-1"; "1/2-1/2"; "0-1" |]
    [| "0-1"; "1/2-1/2"; "1-0"; "1-0"; "0-1"; "0-1"; "1/2-1/2"; "1-0"; "1/2-1/2"; "1-0"; "0-1" |] ]

let private engines n = [ for i in 1 .. n -> { EngineConfig.Empty with Name = sprintf "e%02d" i; Rating = 3000 - i * 10 } ]

/// The pairs of each regular round, played to the end; the error text if the run threw.
let private runSwiss players rounds (cycle: string[]) seedGroups =
  let dir = Path.Combine(Path.GetTempPath(), "eb-swiss-pairing-" + Guid.NewGuid().ToString "N")
  Directory.CreateDirectory dir |> ignore
  let t =
    { Tournament.Empty with
        Name = "swiss-pairing"
        TournamentMode = "Swiss"
        Rounds = rounds
        ConsoleOnly = true
        PgnOutPath = Path.Combine(dir, "out.pgn")
        Opening = { Tournament.Empty.Opening with OpeningsPath = Some (GoldenBook.writeBook dir); OpeningsPly = 4; Seed = 11 }
        EngineSetup = { Tournament.Empty.EngineSetup with Engines = engines players }
        SwissOptions = { Tournament.Empty.SwissOptions with GamesPerMatch = 2; Rounds = rounds; SeedGroupCount = seedGroups } }
  let openings = GameHelpers.loadOpeningsUnlimited t.Opening.OpeningsPath t.Rounds |> fst |> List.ofArray
  let mutable played = 0
  let mutable saved = None
  let rec loop machine event =
    let machine, effects = SwissMachine.step machine event
    for e in effects do
      match e with
      | ModeRunner.Persist s -> saved <- Some s
      | _ -> ()
    match effects |> List.tryPick (function ModeRunner.Play p -> Some p | _ -> None) with
    | Some p when played < 2000 ->
        played <- played + 1
        let r = cycle.[(played - 1) % cycle.Length]
        loop machine (ModeRunner.GameEnded (Some (createResult p.White.Name p.Black.Name (ResizeArray()) r MiscTypes.ResultReason.Checkmate 1000L)))
    | _ -> ()
  try
    loop (SwissMachine.create (SwissMachine.configOf t openings) None 0) ModeRunner.Start
    let regular = saved.Value.Rounds |> Seq.filter (fun r -> r.RoundNumber <= rounds) |> List.ofSeq
    Ok [ for r in regular -> [ for p in r.Pairings -> p.PlayerA, p.PlayerB ] ]
  with ex -> Error ex.Message

let sizes : obj[] seq =
  seq { for players in 4 .. 10 do
          for rounds in 1 .. players - 1 -> [| box players; box rounds |] }

[<Theory>]
[<MemberData(nameof sizes)>]
let ``a Swiss of up to n - 1 rounds is paired to the end without a rematch`` (players: int) (rounds: int) =
  let failures =
    [ for cycle in cycles do
        for seedGroups in [ 1; 2; 4 ] do
          let what = sprintf "%d players, %d rounds, results %s, %d seed groups" players rounds (String.Join(" ", cycle)) seedGroups
          match runSwiss players rounds cycle seedGroups with
          | Error text -> yield $"{what}: {text}"
          | Ok rounds' ->
              if rounds'.Length <> rounds then yield $"{what}: {rounds'.Length} rounds played"
              let met = rounds' |> List.collect id |> List.filter (fun (_, b) -> b <> "BYE") |> List.map (fun (a, b) -> if String.CompareOrdinal(a, b) <= 0 then a + "|" + b else b + "|" + a)
              if (List.distinct met).Length <> met.Length then yield $"{what}: a rematch"
              let byes = rounds' |> List.collect id |> List.filter (fun (_, b) -> b = "BYE") |> List.map fst
              if (List.distinct byes).Length <> byes.Length then yield $"{what}: a second bye" ]
  Assert.True(failures.IsEmpty, String.Join("\n", failures))
