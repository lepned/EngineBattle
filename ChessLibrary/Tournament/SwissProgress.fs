/// Where a Swiss stands, from its saved rounds alone: pure, so a resume rebuilds it the same way.
module ChessLibrary.SwissProgress

open System
open System.Collections.Generic
open ChessLibrary.SwissTypes

/// Each player's points; a name not among the players (BYE) is left out.
let standings (players: string seq) (rounds: SwissRound seq) =
  let scores = Dictionary<string, float>(StringComparer.OrdinalIgnoreCase)
  for p in players do scores.[p] <- 0.0
  for round in rounds do
    for pairing in round.Pairings do
      if scores.ContainsKey pairing.PlayerA then scores.[pairing.PlayerA] <- scores.[pairing.PlayerA] + pairing.ScoreA
      if scores.ContainsKey pairing.PlayerB then scores.[pairing.PlayerB] <- scores.[pairing.PlayerB] + pairing.ScoreB
  scores |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

/// The pairs already met, as Scheduler.Swiss.pairKey; byes are not pairs.
let priorPairs (rounds: SwissRound seq) =
  rounds
  |> Seq.collect (fun r -> r.Pairings)
  |> Seq.filter (fun p -> not (String.IsNullOrWhiteSpace p.PlayerA) && not (String.IsNullOrWhiteSpace p.PlayerB) && p.PlayerB <> "BYE")
  |> Seq.map (fun p -> Scheduler.Swiss.pairKey p.PlayerA p.PlayerB)
  |> Set.ofSeq

/// The players who have had a bye.
let byes (rounds: SwissRound seq) =
  rounds |> Seq.collect (fun r -> r.Pairings) |> Seq.filter (fun p -> p.PlayerB = "BYE") |> Seq.map (fun p -> p.PlayerA) |> Set.ofSeq
