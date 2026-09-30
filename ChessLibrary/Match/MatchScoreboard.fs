namespace ChessLibrary.Match

open System.Collections.Generic
open ChessLibrary.Match.MatchStats

/// The reference's scoreboard (matchmaking/scoreboard.hpp at 60d7a7a) over EngineBattle's games: W/L/D
/// and pentanomial counts per pair of engines, from the view of the engine that comes first in
/// the command line, as the reference keeps them.
///
/// Where the reference pairs two games by its scheduler's pairing id, this pairs them by
/// EngineBattle's rule: the same two engines, the same opening hash, opposite colours. That holds
/// whatever order the games finish in, and a book that starts over (the same opening again, a
/// later pair) only pairs a game with an open game of the other colour. In pentanomial mode W/L/D
/// count completed pairs only, as in the reference.
module MatchScoreboard =

  /// Not thread-safe: GameFinished arrives on several workers, so the caller serializes Add.
  type Scoreboard(players: string list, penta: bool) =
    let order = players |> List.mapi (fun i n -> n, i) |> dict
    /// (first, second) in command-line order -> stats from first's view
    let results = Dictionary<string * string, Stats>()
    /// (hash, first, second) -> open games: white's name and the game from first's view
    let open' = Dictionary<string * string * string, ResizeArray<string * Stats>>()
    let mutable games = 0
    let mutable pairs = 0

    let key (a: string) (b: string) = if order.[a] <= order.[b] then a, b else b, a

    let commit k (s: Stats) =
      results.[k] <- (match results.TryGetValue k with | true, old -> old + s | _ -> s)

    /// Adds a finished game ("1-0", "0-1" or "1/2-1/2"; anything else is not a result, and a game
    /// of an engine not in the list - a resumed PGN with a renamed engine - is not this match's:
    /// both are ignored). True when it completes a pair - always true without pentanomial.
    member _.Add(white: string, black: string, result: string, openingHash: string) =
      let ours = order.ContainsKey white && order.ContainsKey black && white <> black
      let first, second = if ours then key white black else white, black
      let whiteIsFirst = white = first
      let game =
        match result with
        | _ when not ours -> None
        | "1-0" -> Some(if whiteIsFirst then Stats.OfWld(1, 0, 0) else Stats.OfWld(0, 1, 0))
        | "0-1" -> Some(if whiteIsFirst then Stats.OfWld(0, 1, 0) else Stats.OfWld(1, 0, 0))
        | "1/2-1/2" -> Some(Stats.OfWld(0, 0, 1))
        | _ -> None
      match game with
      | None -> false
      | Some g ->
        games <- games + 1
        if not penta then
          commit (first, second) g
          true
        else
          let k = (openingHash, first, second)
          let pending = match open'.TryGetValue k with | true, l -> l | _ -> (let l = ResizeArray() in open'.[k] <- l; l)
          match pending |> Seq.tryFindIndex (fun (w, _) -> w <> white) with
          | None ->
            pending.Add((white, g))
            false
          | Some i ->
            let _, other = pending.[i]
            pending.RemoveAt i
            if pending.Count = 0 then open'.Remove k |> ignore
            let both = other + g
            let bucket =
              match both.Wins, both.Draws, both.Losses with
              | 2, _, _ -> { Stats.Empty with PentaWW = 1 }
              | 1, 1, _ -> { Stats.Empty with PentaWD = 1 }
              | 1, _, 1 -> { Stats.Empty with PentaWL = 1 }
              | _, 2, _ -> { Stats.Empty with PentaDD = 1 }
              | _, 1, 1 -> { Stats.Empty with PentaLD = 1 }
              | _ -> { Stats.Empty with PentaLL = 1 }
            commit (first, second) (both + bucket)
            pairs <- pairs + 1
            true

    /// getStats(a, b): a's results against b.
    member _.Stats(a: string, b: string) =
      let k = key a b
      let s = match results.TryGetValue k with | true, s -> s | _ -> Stats.Empty
      if fst k = a then s else s.Inverted

    /// An engine's results against everyone.
    member x.EngineStats(name: string) =
      players |> List.filter ((<>) name) |> List.fold (fun acc other -> acc + x.Stats(name, other)) Stats.Empty

    /// Games added (the reference's match_count_).
    member _.Games = games
    /// Pairs completed (pentanomial only).
    member _.CompletedPairs = pairs
