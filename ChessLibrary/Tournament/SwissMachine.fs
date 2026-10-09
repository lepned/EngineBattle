/// A Swiss tournament as a pure step: state + event -> state + effects. ModeRunner plays the
/// games it asks for and carries out the rest. Mirrors the runner it replaces game for game:
/// rounds of pairings, each pairing a match of units (an opening and its colours), byes, then the
/// tiebreak. Unplayable games in a row stop the tournament, as in a cup or a ladder: a resume plays
/// the pair on where it stands (it was skipped before, and a resume replayed it rounds later).
module ChessLibrary.SwissMachine

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.SwissTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.TournamentPairing
open ChessLibrary.ModeRunner

type Config =
  { /// The engines in the tournament's order (the pairing reads it).
    Players: EngineConfig list
    SeedOrder: EngineConfig list
    /// Games per match: even, at least 2.
    GamesPerMatch: int
    TotalRounds: int
    UniquePerMatchOnly: bool
    RandomOpenings: bool
    Seed: int
    Openings: PgnGame list
    /// the same book, indexed: the shuffled orders look openings up by number
    Book: PgnGame[]
    AllowExtraPairsOnTie: bool
    /// Unplayable games in a row after which a pair is abandoned.
    MaxFailures: int
    TournamentName: string
    SeedGroupCount: int
    /// GamesPerMatch as configured, which the file keeps.
    ConfiguredGamesPerMatch: int }

type Pair =
  { Id: int
    Round: int
    A: string
    B: string
    RatingA: int
    RatingB: int
    ScoreA: float
    ScoreB: float
    Decided: bool
    Games: SwissGame list
    Order: int list }

type Round = { Number: int; Pairs: Pair list }

/// An opening and the colours still to play with it.
type private Unit = { Opening: PgnGame; Hash: string; Colours: (EngineConfig * EngineConfig) list; Odd: bool }

/// Where a pairing's match stands while it is being played.
type private PairCursor =
  { Index: int
    Entered: bool
    Remaining: int
    Local: int
    Failures: int
    Unit: Unit option
    FirstWhite: EngineConfig option
    FirstBlack: EngineConfig option }

type private Phase =
  /// The main rounds: the next round to play is this one.
  | Rounds of int
  /// The playoff: the next playoff round is this one.
  | Tie of int
  | Over

type State =
  private
    { Config: Config
      Rounds: Round list
      NextOpeningIndex: int
      GlobalOrder: int list
      NextPairId: int
      /// Games played, the ones in the file included: the next game's number follows it.
      Played: int
      Phase: Phase
      Current: (int * PairCursor) option
      /// The round's games still to come, as the pairing list shows them.
      Planned: Pairing list
      /// The game asked for and not ended yet.
      Waiting: Pairing option
      /// A resumed file's own header (name, seed groups, games per match, unique openings), kept as it was.
      Header: (string * int * int * bool) option
      /// The run was cancelled: the round's bookkeeping goes on, no game or round starts.
      Stopped: bool }

let private openingHash (o: PgnGame) = Hash.computeOpeningHashFromGame o

let private engine (cfg: Config) name = cfg.Players |> List.find (fun e -> e.Name = name)

let private hasEngines (cfg: Config) (p: Pair) =
  (cfg.Players |> List.filter (fun e -> e.Name = p.A || e.Name = p.B)).Length = 2

let private seedMap (cfg: Config) = cfg.SeedOrder |> List.mapi (fun i p -> p.Name, i + 1) |> Map.ofList

let private ofPair (p: SwissPairing) =
  { Id = p.PairId; Round = p.RoundNumber; A = p.PlayerA; B = p.PlayerB; RatingA = p.PlayerARating; RatingB = p.PlayerBRating
    ScoreA = p.ScoreA; ScoreB = p.ScoreB; Decided = p.IsDecided
    Games = List.ofSeq p.Games
    Order = if isNull p.OpeningOrder then [] else List.ofSeq p.OpeningOrder }

let private toPairing (p: Pair) : SwissPairing =
  { PairId = p.Id; RoundNumber = p.Round; PlayerA = p.A; PlayerB = p.B; PlayerARating = p.RatingA; PlayerBRating = p.RatingB
    ScoreA = p.ScoreA; ScoreB = p.ScoreB; IsDecided = p.Decided
    Games = ResizeArray p.Games
    // as before: at most 50 indices are kept (a resume reshuffles the same order from the seed)
    OpeningOrder = ResizeArray(p.Order |> List.truncate 50) }

/// The state as its file holds it.
let toFile (st: State) : SwissState =
  let name, groups, gpm, uniqueGlobal =
    st.Header |> Option.defaultValue (st.Config.TournamentName, st.Config.SeedGroupCount, st.Config.ConfiguredGamesPerMatch, not st.Config.UniquePerMatchOnly)
  { TournamentName = name
    SeedGroupCount = groups
    GamesPerMatch = gpm
    UniqueOpeningsGlobal = uniqueGlobal
    NextOpeningIndex = st.NextOpeningIndex
    GlobalOpeningOrder = ResizeArray(st.GlobalOrder |> List.truncate 50)
    Rounds = ResizeArray [ for r in st.Rounds -> { RoundNumber = r.Number; Pairings = ResizeArray (r.Pairs |> List.map toPairing) } ]
    UpdatedUtc = DateTime.UtcNow }

let private persist (st: State) (acc: Effect<SwissState> list) = acc @ [ Persist (toFile st) ]

let private globalOpenings (st: State) =
  let cfg = st.Config
  if not st.GlobalOrder.IsEmpty then st.GlobalOrder |> List.map (fun i -> cfg.Book.[i % cfg.Book.Length])
  else cfg.Openings

let private shuffled (cfg: Config) purpose = Scheduler.Shared.seededOrder cfg.Seed purpose cfg.Openings.Length |> List.ofArray

/// A pairing's openings: its own shuffled order (unique per match, random), the book, or the
/// tournament's order.
let private matchOpenings (st: State) (p: Pair) =
  let cfg = st.Config
  if cfg.UniquePerMatchOnly then
    if cfg.RandomOpenings && cfg.Openings.Length > 1 then p.Order |> List.map (fun i -> cfg.Book.[i % cfg.Book.Length])
    else cfg.Openings
  else globalOpenings st

let private updatePair (st: State) (p: Pair) =
  { st with
      Rounds = st.Rounds |> List.map (fun r ->
        if r.Number <> p.Round then r else { r with Pairs = r.Pairs |> List.map (fun x -> if x.Id = p.Id then p else x) }) }

let private roundOf (st: State) n = st.Rounds |> List.tryFind (fun r -> r.Number = n)

/// The rounds as SwissProgress reads them.
let private asRounds (st: State) =
  st.Rounds |> List.map (fun r -> { RoundNumber = r.Number; Pairings = ResizeArray (r.Pairs |> List.map toPairing) } : SwissRound)

/// A machine for a new Swiss, or one resumed from its file. `gamesAlreadyPlayed`: the games in
/// the PGN, for the game numbers when the file has none.
let create (cfg: Config) (loaded: SwissState option) (gamesAlreadyPlayed: int) : State =
  let rounds, nextOpening, globalOrder =
    match loaded with
    | Some s ->
        [ for r in s.Rounds -> { Number = r.RoundNumber; Pairs = [ for p in r.Pairings -> ofPair p ] } ],
        s.NextOpeningIndex,
        (if isNull s.GlobalOpeningOrder then [] else List.ofSeq s.GlobalOpeningOrder)
    | None -> [], 0, []
  let nextPairId = (rounds |> List.collect (fun r -> r.Pairs |> List.map _.Id) |> List.fold max 0) + 1
  let inFile = rounds |> List.sumBy (fun r -> r.Pairs |> List.sumBy (fun p -> p.Games.Length))
  let roundToStart =
    rounds |> List.sortBy _.Number
    |> List.tryFind (fun r -> r.Pairs |> List.exists (fun p -> not p.Decided))
    |> Option.map _.Number
    |> Option.defaultValue (rounds.Length + 1)
  { Config = cfg
    Rounds = rounds
    NextOpeningIndex = nextOpening
    GlobalOrder = globalOrder
    NextPairId = nextPairId
    Played = (if inFile > 0 then inFile else gamesAlreadyPlayed)
    Phase = Rounds roundToStart
    Current = None
    Planned = []
    Waiting = None
    Header = loaded |> Option.map (fun s -> s.TournamentName, s.SeedGroupCount, s.GamesPerMatch, s.UniqueOpeningsGlobal)
    Stopped = false }

/// The machine's config from the tournament and its book.
let configOf (tourny: ChessLibrary.TypesDef.Tournament.Tournament) (openings: PgnGame list) : Config =
  let o = tourny.SwissOptions
  let engines = tourny.EngineSetup.Engines
  let gpm = if o.GamesPerMatch < 2 then 2 elif o.GamesPerMatch % 2 = 1 then o.GamesPerMatch + 1 else o.GamesPerMatch
  let configured = if o.Rounds > 0 then o.Rounds else tourny.Rounds
  { Players = engines
    SeedOrder = PairingHelper.tcecSeedOrder engines o.SeedGroupCount
    GamesPerMatch = gpm
    TotalRounds = min configured (max 1 (engines.Length - 1))
    UniquePerMatchOnly = o.UniquePerMatchOnly
    RandomOpenings = o.RandomOpenings
    Seed = tourny.Opening.Seed
    Openings = openings
    Book = Array.ofList openings
    AllowExtraPairsOnTie = o.AllowExtraPairsOnTie
    MaxFailures = 3
    TournamentName = tourny.Name
    SeedGroupCount = o.SeedGroupCount
    ConfiguredGamesPerMatch = o.GamesPerMatch }

/// The games the rounds hold, before any playoff.
let totalGames (cfg: Config) = cfg.TotalRounds * (cfg.Players.Length / 2) * cfg.GamesPerMatch

/// The same for a machine, with the playoff rounds it has begun.
let totalGamesOf (st: State) =
  let playoffPairs = st.Rounds |> List.filter (fun r -> r.Number > st.Config.TotalRounds) |> List.sumBy (fun r -> r.Pairs.Length)
  totalGames st.Config + playoffPairs * st.Config.GamesPerMatch

/// The game number the next played game gets.
let played (st: State) = st.Played

let private standings (st: State) = SwissProgress.standings (st.Config.Players |> List.map _.Name) (asRounds st)

/// Opens round `n`: the pairings it already has (a resume), or `pairs` as new ones; then the
/// preview of its games.
let private openRound (st: State) n (pairs: (EngineConfig * EngineConfig) list) (acc: Effect<SwissState> list) =
  let cfg = st.Config
  let st, acc =
    match roundOf st n with
    | Some _ -> st, acc
    | None ->
        let made =
          pairs |> List.mapi (fun i (a, b) ->
            { Id = st.NextPairId + i; Round = n; A = a.Name; B = b.Name; RatingA = a.Rating; RatingB = b.Rating
              ScoreA = (if b.Name = "BYE" then 1.0 else 0.0); ScoreB = 0.0; Decided = (b.Name = "BYE"); Games = []; Order = [] })
        let st = { st with Rounds = st.Rounds @ [ { Number = n; Pairs = made } ]; NextPairId = st.NextPairId + made.Length }
        st, persist st acc
  // the preview: the same inputs as the games below, so it names each game as the PGN will
  let round = (roundOf st n).Value
  let seeds = seedMap cfg
  let planned = ResizeArray<Pairing>()
  let mutable previewIndex = st.NextOpeningIndex
  let mutable st = st
  let mutable acc = acc
  round.Pairs |> List.iteri (fun pairIndex p0 ->
    if not (p0.Decided || p0.B = "BYE") then
      let p =
        if cfg.UniquePerMatchOnly && cfg.RandomOpenings && cfg.Openings.Length > 1 && p0.Order.Length < cfg.Openings.Length then
          let p = { p0 with Order = shuffled cfg $"swiss-pair|{p0.Round}|{p0.Id}" }
          st <- updatePair st p
          acc <- persist st acc
          p
        else p0
      let openings = matchOpenings st p
      let firstWhite, firstBlack = if seeds.[p.A] <= seeds.[p.B] then p.A, p.B else p.B, p.A
      let halfPairOpening =
        if p.Games.Length % 2 = 1 then
          let last = List.last p.Games
          openings |> List.tryFind (fun o -> openingHash o = last.OpeningHash)
        else None
      let start = if cfg.UniquePerMatchOnly then p.Games.Length / 2 else previewIndex
      let next =
        PairingHelper.addPlannedPairings planned (engine cfg firstWhite) (engine cfg firstBlack) openings cfg.GamesPerMatch start
          n (pairIndex * cfg.GamesPerMatch) p.Games.Length st.Played halfPairOpening
      if not cfg.UniquePerMatchOnly then previewIndex <- next)
  let st = { st with Planned = List.ofSeq planned; Current = Some (n, { Index = 0; Entered = false; Remaining = 0; Local = 0; Failures = 0; Unit = None; FirstWhite = None; FirstBlack = None }) }
  st, acc @ [ Notify (Update.PairingList (ResizeArray planned)) ]

/// The next unit of a pair's match: an opening and the colours to play it with.
let private nextUnit (st: State) (p: Pair) (pc: PairCursor) (acc: Effect<SwissState> list) =
  let cfg = st.Config
  let openings = matchOpenings st p
  let odd = p.Games.Length % 2 = 1
  let st, pc, opening, acc =
    if odd then
      let last = List.last p.Games
      match openings |> List.tryFind (fun o -> openingHash o = last.OpeningHash) with
      | Some o -> st, pc, o, acc
      | None ->
          let o =
            if cfg.UniquePerMatchOnly then openings.[(p.Games.Length / 2) % openings.Length]
            else openings.[st.NextOpeningIndex % openings.Length]
          st, pc, o, acc @ [ Info $"Swiss resume: opening hash {last.OpeningHash} not found in opening book - using next available opening." ]
    elif cfg.UniquePerMatchOnly then
      st, { pc with Local = pc.Local + 1 }, openings.[pc.Local % openings.Length], acc
    else
      let index = if st.NextOpeningIndex >= openings.Length then 0 else st.NextOpeningIndex
      { st with NextOpeningIndex = index + 1 }, pc, openings.[index], acc
  let a, b = engine cfg p.A, engine cfg p.B
  let colours =
    if odd then
      let last = List.last p.Games
      if last.White = p.A then [ b, a ] else [ a, b ]
    else [ pc.FirstWhite.Value, pc.FirstBlack.Value; pc.FirstBlack.Value, pc.FirstWhite.Value ]
  st, { pc with Unit = Some { Opening = opening; Hash = openingHash opening; Colours = colours; Odd = odd } }, acc

/// Playoff rounds before a tie still standing is resolved by Sonneborn-Berger.
let private maxPlayoffRounds = 3

/// The tiebreak once the rounds are over: a playoff round when exactly two lead, Sonneborn-Berger
/// for more or after maxPlayoffRounds playoffs.
let private startTie (st: State) n (acc: Effect<SwissState> list) =
  let cfg = st.Config
  let scores = standings st
  if scores.IsEmpty then { st with Phase = Over }, acc
  else
    let best = scores |> Seq.maxBy (fun kv -> kv.Value) |> fun kv -> kv.Value
    let tied = scores |> Seq.filter (fun kv -> kv.Value = best) |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    let playoffs = st.Rounds |> List.filter (fun r -> r.Number > cfg.TotalRounds)
    if tied.Length <= 1 then { st with Phase = Over }, acc
    elif tied.Length > 2 || playoffs.Length >= maxPlayoffRounds then
      let why =
        if tied.Length > 2 then $"{tied.Length} players tied at {best}"
        else $"2 players still tied at {best} after {playoffs.Length} playoff rounds"
      // Sonneborn-Berger: the points taken off each opponent times that opponent's score
      let sb = Dictionary<string, float>(StringComparer.OrdinalIgnoreCase)
      for p in cfg.Players do sb.[p.Name] <- 0.0
      for r in st.Rounds do
        for p in r.Pairs do
          if p.B <> "BYE" then
            let oppA = scores |> Map.tryFind p.B |> Option.defaultValue 0.0
            let oppB = scores |> Map.tryFind p.A |> Option.defaultValue 0.0
            if sb.ContainsKey p.A then sb.[p.A] <- sb.[p.A] + p.ScoreA * oppA
            if sb.ContainsKey p.B then sb.[p.B] <- sb.[p.B] + p.ScoreB * oppB
      let sbOf name = match sb.TryGetValue name with | true, v -> v | _ -> 0.0
      let ranked = tied |> List.sortByDescending sbOf
      let text = ranked |> List.map (fun n -> sprintf "%s (SB=%.1f)" n (sbOf n)) |> String.concat ", "
      { st with Phase = Over }, acc @ [ Info $"Swiss tiebreak: {why}. Resolved by Sonneborn-Berger: {text}" ]
    else
      let tiedPlayers = cfg.Players |> List.filter (fun p -> tied |> List.contains p.Name)
      let byes = SwissProgress.byes (asRounds st)
      let pairs = Scheduler.Swiss.pairNextRound tiedPlayers cfg.SeedOrder scores Set.empty byes
      let acc = acc @ [ Info $"Swiss tiebreak: 2 players tied at {best}, playing playoff match."; AddTotalGames (pairs.Length * cfg.GamesPerMatch) ]
      openRound { st with Phase = Tie n } n pairs acc

/// Runs the tournament forward to the next game to play, or to its end.
let rec private advance (st: State) (acc: Effect<SwissState> list) : State * Effect<SwissState> list =
  let cfg = st.Config
  match st.Waiting, st.Phase, st.Current with
  | Some _, _, _ -> st, acc
  | None, Over, _ -> st, acc @ [ Finished ]
  | None, (Rounds _ | Tie _), None when st.Stopped -> advance { st with Phase = Over } acc
  | None, Rounds n, None ->
      let maxPairs = (cfg.Players.Length * (cfg.Players.Length - 1)) / 2
      let toTie () = if cfg.AllowExtraPairsOnTie then advance { st with Phase = Tie n } acc else advance { st with Phase = Over } acc
      if (roundOf st n).IsSome then
        // a round the file already holds (a resume): finish it - a playoff as a playoff. Its pairs
        // count as played, and its scores as standing, so the checks below would end the rounds early
        let st = if n > cfg.TotalRounds then { st with Phase = Tie n } else st
        let st, acc = openRound st n [] acc
        advance st acc
      elif n > cfg.TotalRounds then toTie ()
      else
        let prior = SwissProgress.priorPairs (asRounds st)
        if prior.Count >= maxPairs then
          let acc = acc @ [ Info "Swiss tournament completed: all unique pairs have been played." ]
          if cfg.AllowExtraPairsOnTie then advance { st with Phase = Tie n } acc else advance { st with Phase = Over } acc
        else
          // paired so that the rounds after it can still be paired without a rematch
          let pairs = Scheduler.Swiss.pairRoundLeaving (cfg.TotalRounds - n) cfg.Players cfg.SeedOrder (standings st) prior (SwissProgress.byes (asRounds st))
          let st, acc = openRound st n pairs acc
          advance st acc
  | None, Tie n, None ->
      let st, acc = startTie st n acc
      advance st acc
  | None, phase, Some (n, pc) ->
      let round = (roundOf st n).Value
      let next () =
        match phase with
        | Tie _ -> { st with Current = None; Phase = Tie (n + 1) }
        | _ -> { st with Current = None; Phase = Rounds (n + 1) }
      if pc.Index >= round.Pairs.Length then advance (next ()) acc
      else
        let p = round.Pairs.[pc.Index]
        let skip st = { st with Current = Some (n, { pc with Index = pc.Index + 1; Entered = false; Unit = None }) }
        if p.B = "BYE" then
          if p.Decided then advance (skip st) acc
          else
            let st = updatePair st { p with ScoreA = 1.0; ScoreB = 0.0; Decided = true }
            advance (skip st) (persist st acc)
        elif not (hasEngines cfg p) then advance (skip st) acc
        elif not pc.Entered then
          // entering the pair: its opening order (unique per match, random), its colours, its games left
          let st, p, acc =
            if cfg.UniquePerMatchOnly && cfg.RandomOpenings && cfg.Openings.Length > 1 && p.Order.IsEmpty then
              let p = { p with Order = shuffled cfg $"swiss-pair|{p.Round}|{p.Id}" }
              let st = updatePair st p
              st, p, persist st acc
            else st, p, acc
          let seeds = seedMap cfg
          let fw, fb = if seeds.[p.A] <= seeds.[p.B] then p.A, p.B else p.B, p.A
          let pc =
            { pc with Entered = true; Remaining = max 0 (cfg.GamesPerMatch - p.Games.Length); Local = p.Games.Length / 2
                      Failures = 0; Unit = None; FirstWhite = Some (engine cfg fw); FirstBlack = Some (engine cfg fb) }
          advance { st with Current = Some (n, pc) } acc
        else
          match pc.Unit with
          | None when pc.Remaining > 0 && pc.Failures < cfg.MaxFailures && not st.Stopped ->
              let st, pc, acc = nextUnit st p pc acc
              advance { st with Current = Some (n, pc) } acc
          | None -> advance (skip st) acc
          | Some u ->
              match u.Colours with
              | (white, black) :: rest when pc.Remaining > 0 && pc.Failures < cfg.MaxFailures && not st.Stopped ->
                  let game =
                    { Opening = u.Opening; White = white; Black = black; GameNr = st.Played + 1
                      RoundNr = $"{p.Round}.{pc.Index * cfg.GamesPerMatch + p.Games.Length + 1}"; OpeningHash = u.Hash }
                  let pc = { pc with Unit = Some { u with Colours = rest } }
                  { st with Current = Some (n, pc); Waiting = Some game }, acc @ [ Play game ]
              | _ ->
                  // the unit is over: the per-match index moves on past a pair finished on its opening
                  let local = if u.Odd && cfg.UniquePerMatchOnly then pc.Local + 1 else pc.Local
                  advance { st with Current = Some (n, { pc with Unit = None; Local = local }) } acc

let step (st: State) (event: Event) : State * Effect<SwissState> list =
  match event with
  | Cancel -> { st with Stopped = true }, []
  | Start ->
      // the tournament's opening order, made once (and again on a resume of more than the 50 kept)
      let cfg = st.Config
      let st, acc =
        if cfg.RandomOpenings && cfg.Openings.Length > 1 && not cfg.UniquePerMatchOnly
           && (st.GlobalOrder.IsEmpty || st.GlobalOrder.Length < cfg.Openings.Length) then
          let st = { st with GlobalOrder = shuffled cfg "swiss-openings" }
          st, persist st []
        else st, []
      advance st acc
  | GameEnded result ->
      match st.Waiting, st.Current with
      | Some game, Some (n, pc) ->
          let cfg = st.Config
          let p = (roundOf st n).Value.Pairs.[pc.Index]
          let st = { st with Waiting = None }
          match result with
          | Some r ->
              let played = st.Played + 1
              let swissGame =
                { GameNr = played; White = game.White.Name; Black = game.Black.Name
                  OpeningId = string game.Opening.GameNumber; OpeningHash = game.OpeningHash; Result = r.Result }
              let a, b = MatchScore.addGame (p.ScoreA, p.ScoreB) (game.White.Name = p.A) r.Result
              let games = p.Games @ [ swissGame ]
              let decided = games.Length >= cfg.GamesPerMatch
              let p = { p with Games = games; ScoreA = a; ScoreB = b; Decided = p.Decided || decided }
              let st = updatePair st p
              let st = { st with Played = played; Current = Some (n, { pc with Failures = 0; Remaining = pc.Remaining - 1 }) }
              let st, acc =
                if decided then
                  let planned =
                    st.Planned |> List.filter (fun x ->
                      not ((x.White.Name = p.A || x.White.Name = p.B) && (x.Black.Name = p.A || x.Black.Name = p.B)))
                  { st with Planned = planned }, [ Notify (Update.PairingList (ResizeArray planned)) ]
                else st, []
              advance st (persist st acc)
          | None ->
              let failures = pc.Failures + 1
              let st = { st with Current = Some (n, { pc with Failures = failures }) }
              if failures >= cfg.MaxFailures then
                // a pair cannot be skipped and played later without changing the rounds after it:
                // the tournament stops, and a resume plays the pair on in its round
                let text = $"Stopping the Swiss: {game.White.Name} vs {game.Black.Name} had {failures} consecutive unplayable games - fix the engine and resume, the pair is played on in round {n}"
                advance { st with Stopped = true } [ Critical text; StopRun ]
              else advance st []
      | _ -> st, []
