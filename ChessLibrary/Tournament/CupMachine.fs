/// A knockout cup as a pure step: state + event -> state + effects; ModeRunner plays the games it
/// asks for. Mirrors the runner it replaces game for game: a seeded bracket, each round's
/// survivors paired in order, matches decided once the leader cannot be caught (two more games
/// after a tie), the winner moved into the next round before the bracket is saved.
module ChessLibrary.CupMachine

open System
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.CupTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.TournamentPairing
open ChessLibrary.ModeRunner

type Config =
  { Players: EngineConfig list
    Strategy: PairingHelper.CupSeedingStrategy
    UniquePerMatchOnly: bool
    RandomOpenings: bool
    Seed: int
    Openings: PgnGame list
    /// the same book, indexed: the shuffled orders look openings up by number
    Book: PgnGame[]
    RoundPairIncrements: int list
    /// Unplayable games in a row after which the cup stops (it cannot drop a player).
    MaxFailures: int
    TournamentName: string }

type Match =
  { Id: int
    Round: int
    A: string
    B: string
    RatingA: int
    RatingB: int
    ScoreA: float
    ScoreB: float
    Winner: string option
    Decided: bool
    Games: CupGame list
    Order: int list }

type Round = { Number: int; Matches: Match list }

type private Unit = { Opening: PgnGame; Hash: string; Colours: (EngineConfig * EngineConfig) list }

type private MatchCursor =
  { Entered: bool
    Remaining: int
    Local: int
    Failures: int
    Openings: PgnGame list
    Unit: Unit option }

type State =
  private
    { Config: Config
      Header: string * string * int * bool
      Rounds: Round list
      NextOpeningIndex: int
      GlobalOrder: int list
      NextMatchId: int
      Played: int
      TotalRounds: int
      Seeded: EngineConfig list
      /// The round being played: its number, its players, the match at hand, the winners so far.
      RoundNumber: int
      Players: EngineConfig list
      RoundStarted: bool
      MatchIndex: int
      Winners: EngineConfig list
      Cursor: MatchCursor
      Waiting: Pairing option
      Stopped: bool
      Done: bool
      /// The bracket was made new: saved at Start.
      Created: bool }

let private openingHash (o: PgnGame) = Hash.computeOpeningHashFromGame o

let private gamesFor (cfg: Config) round = PairingHelper.gamesPerMatchForRound 2 cfg.RoundPairIncrements round

let private matchCount (cfg: Config) round = max 1 (cfg.Players.Length / pown 2 round)

let private ofMatch (m: CupMatch) =
  { Id = m.MatchId; Round = m.RoundNumber; A = m.PlayerA; B = m.PlayerB; RatingA = m.PlayerARating; RatingB = m.PlayerBRating
    ScoreA = m.ScoreA; ScoreB = m.ScoreB; Winner = m.Winner; Decided = m.IsDecided; Games = List.ofSeq m.Games
    Order = if isNull m.OpeningOrder then [] else List.ofSeq m.OpeningOrder }

let private toCupMatch (m: Match) : CupMatch =
  { MatchId = m.Id; RoundNumber = m.Round; PlayerA = m.A; PlayerB = m.B; PlayerARating = m.RatingA; PlayerBRating = m.RatingB
    ScoreA = m.ScoreA; ScoreB = m.ScoreB; Winner = m.Winner; IsDecided = m.Decided; Games = ResizeArray m.Games
    // as before: at most 50 indices are kept
    OpeningOrder = ResizeArray(m.Order |> List.truncate 50) }

/// The bracket as its file holds it.
let toFile (st: State) : CupBracket =
  let name, strategy, gpm, uniqueGlobal = st.Header
  { TournamentName = name; Strategy = strategy; GamesPerMatch = gpm; UniqueOpeningsGlobal = uniqueGlobal
    NextOpeningIndex = st.NextOpeningIndex
    GlobalOpeningOrder = ResizeArray(st.GlobalOrder |> List.truncate 50)
    Rounds = ResizeArray [ for r in st.Rounds -> { RoundNumber = r.Number; Matches = ResizeArray (r.Matches |> List.map toCupMatch) } ]
    UpdatedUtc = DateTime.UtcNow }

let private persist (st: State) (acc: Effect<CupBracket> list) = acc @ [ Persist (toFile st) ]

let private shuffled (cfg: Config) purpose = Scheduler.Shared.seededOrder cfg.Seed purpose cfg.Openings.Length |> List.ofArray

let private seed (cfg: Config) =
  match cfg.Strategy with
  | PairingHelper.CupSeedingStrategy.Random ->
      let players = cfg.Players |> List.toArray
      (Scheduler.Shared.seededRandom cfg.Seed "cup-draw").Shuffle(players)
      List.ofArray players
  | PairingHelper.CupSeedingStrategy.ByRating ->
      let rng = Scheduler.Shared.seededRandom cfg.Seed "cup-draw"
      Scheduler.Cup.seedByBandsWith (Some (fun a -> rng.Shuffle(a))) cfg.Players (PairingHelper.autoSeedBands cfg.Players.Length)

let private emptyMatch id round =
  { Id = id; Round = round; A = "TBD"; B = "TBD"; RatingA = 0; RatingB = 0; ScoreA = 0.0; ScoreB = 0.0
    Winner = None; Decided = false; Games = []; Order = [] }

let private freshCursor = { Entered = false; Remaining = 0; Local = 0; Failures = 0; Openings = []; Unit = None }

/// The machine's config from the tournament and its book.
let configOf (tourny: ChessLibrary.TypesDef.Tournament.Tournament) (strategy: PairingHelper.CupSeedingStrategy) (uniquePerMatchOnly: bool) (openings: PgnGame list) =
  let o = tourny.CupOptions
  { Players = tourny.EngineSetup.Engines
    Strategy = strategy
    UniquePerMatchOnly = uniquePerMatchOnly
    RandomOpenings = (if obj.ReferenceEquals(o, null) then false else o.RandomOpenings)
    Seed = tourny.Opening.Seed
    Openings = openings
    Book = Array.ofList openings
    RoundPairIncrements = (if obj.ReferenceEquals(o, null) then [] else o.RoundPairIncrements)
    MaxFailures = 3
    TournamentName = tourny.Name }

/// A machine for a new cup, or one resumed from its bracket. `gamesAlreadyPlayed`: the games in
/// the PGN, for the game numbers when the bracket has none.
let create (cfg: Config) (loaded: CupBracket option) (gamesAlreadyPlayed: int) : State =
  let totalRounds =
    let mutable players = cfg.Players.Length
    let mutable rounds = 0
    while players > 1 do
      players <- players / 2
      rounds <- rounds + 1
    rounds
  let seeded = seed cfg
  let header, rounds, nextOpening, globalOrder =
    match loaded with
    | Some b ->
        (b.TournamentName, b.Strategy, b.GamesPerMatch, b.UniqueOpeningsGlobal),
        [ for r in b.Rounds -> { Number = r.RoundNumber; Matches = [ for m in r.Matches -> ofMatch m ] } ],
        b.NextOpeningIndex,
        (if isNull b.GlobalOpeningOrder then [] else List.ofSeq b.GlobalOpeningOrder)
    | None -> (cfg.TournamentName, string cfg.Strategy, gamesFor cfg 1, not cfg.UniquePerMatchOnly), [], 0, []
  let mutable nextId = (rounds |> List.collect (fun r -> r.Matches |> List.map _.Id) |> List.fold max 0) + 1
  let created = rounds.IsEmpty
  let rounds =
    if not created then rounds
    else
      [ for round in 1 .. totalRounds ->
          let matches =
            if round = 1 then
              seeded |> List.chunkBySize 2 |> List.map (fun pair ->
                match pair with
                | [ a; b ] ->
                    let m = { emptyMatch nextId 1 with A = a.Name; B = b.Name; RatingA = a.Rating; RatingB = b.Rating }
                    nextId <- nextId + 1
                    m
                | _ -> failwith "Cup bracket requires even number of players")
            else
              [ for _ in 1 .. matchCount cfg round ->
                  let m = emptyMatch nextId round
                  nextId <- nextId + 1
                  m ]
          { Number = round; Matches = matches } ]
  let inBracket = rounds |> List.sumBy (fun r -> r.Matches |> List.sumBy (fun m -> m.Games.Length))
  // the round to play: the first with a match undecided, else the last
  let initial =
    rounds |> List.sortBy _.Number |> List.tryFind (fun r -> r.Matches |> List.exists (fun m -> not m.Decided))
    |> Option.orElse (rounds |> List.sortByDescending _.Number |> List.tryHead)
    |> Option.defaultValue { Number = 1; Matches = [] }
  let roundPlayers =
    initial.Matches |> List.collect (fun m -> [ m.A; m.B ])
    |> List.filter (fun n -> not (String.IsNullOrWhiteSpace n) && not (n.Equals("TBD", StringComparison.OrdinalIgnoreCase)))
    |> List.distinct
  let names = if roundPlayers.IsEmpty then seeded |> List.map _.Name else roundPlayers
  { Config = cfg
    Header = header
    Rounds = rounds
    NextOpeningIndex = nextOpening
    GlobalOrder = globalOrder
    NextMatchId = nextId
    Played = (if inBracket > 0 then inBracket else gamesAlreadyPlayed)
    TotalRounds = totalRounds
    Seeded = seeded
    RoundNumber = initial.Number
    Players = names |> List.choose (fun n -> cfg.Players |> List.tryFind (fun e -> e.Name = n))
    RoundStarted = false
    MatchIndex = 0
    Winners = []
    Cursor = freshCursor
    Waiting = None
    Stopped = false
    Done = false
    Created = created }

/// The games the bracket holds at the least (no tiebreaks, every match played out).
let minTotalGames (st: State) =
  [ 1 .. st.TotalRounds ] |> List.sumBy (fun round -> matchCount st.Config round * gamesFor st.Config round)

/// The total as the bracket stands: decided matches count the games they took, undecided ones
/// every tiebreak pair they have begun.
let currentTotalGames (st: State) =
  st.Rounds |> List.fold (fun total r ->
    let gpm = gamesFor st.Config r.Number
    r.Matches |> List.fold (fun total m ->
      let counted = if m.Decided then m.Games.Length else MatchScore.gamesCounted gpm m.Games.Length
      total + (counted - gpm)) total) (minTotalGames st)

/// The game number the next played game gets.
let played (st: State) = st.Played

let private updateMatch (st: State) (m: Match) =
  let replace (r: Round) =
    if r.Number <> m.Round then r else { r with Matches = r.Matches |> List.map (fun x -> if x.Id = m.Id then m else x) }
  { st with Rounds = st.Rounds |> List.map replace }

let private roundOf (st: State) n = st.Rounds |> List.find (fun r -> r.Number = n)

let private currentMatch (st: State) = (roundOf st st.RoundNumber).Matches.[st.MatchIndex]

/// The games a round has played: its games are labelled round.n in the order played, across its
/// matches and their tiebreaks, so no two share a label.
let private roundGames (st: State) n = (roundOf st n).Matches |> List.sumBy (fun m -> m.Games.Length)

let private pairs (st: State) =
  st.Players |> List.chunkBySize 2 |> List.choose (function [ a; b ] -> Some (a, b) | _ -> None)

/// The winner into the next round's slot (the round made when it is missing).
let private placeInNextRound (st: State) (player: EngineConfig) =
  if st.RoundNumber >= st.TotalRounds then st
  else
    let next = st.RoundNumber + 1
    let st =
      if st.Rounds |> List.exists (fun r -> r.Number = next) then st
      else
        let count = matchCount st.Config next
        let made = [ for i in 0 .. count - 1 -> emptyMatch (st.NextMatchId + i) next ]
        { st with Rounds = st.Rounds @ [ { Number = next; Matches = made } ]; NextMatchId = st.NextMatchId + count }
    let index, asA = MatchScore.nextSlot st.MatchIndex
    let target = (roundOf st next).Matches.[index]
    updateMatch st (if asA then { target with A = player.Name; RatingA = player.Rating } else { target with B = player.Name; RatingB = player.Rating })

let private winnerOf (st: State) (m: Match) =
  let a, b = (pairs st).[st.MatchIndex]
  match m.Winner with
  | Some name when name = a.Name -> Some a
  | Some name when name = b.Name -> Some b
  | _ -> None

let private globalOpenings (st: State) =
  let cfg = st.Config
  if not st.GlobalOrder.IsEmpty then st.GlobalOrder |> List.map (fun i -> cfg.Book.[i % cfg.Book.Length]) else cfg.Openings

/// The next opening of the tournament's order (or the match's, unique per match).
let private nextOpening (st: State) (openings: PgnGame list) (local: int) =
  if st.Config.UniquePerMatchOnly then st, openings.[local % openings.Length]
  else
    let index = if st.NextOpeningIndex >= openings.Length then 0 else st.NextOpeningIndex
    { st with NextOpeningIndex = index + 1 }, openings.[index]

/// The match is over (or abandoned): its winner joins the next round.
let private endMatch (st: State) (acc: Effect<CupBracket> list) =
  let m = currentMatch st
  let st, acc =
    match winnerOf st m with
    | Some player ->
        let st = { st with Winners = st.Winners @ [ player ] }
        if st.RoundNumber < st.TotalRounds then
          let st = placeInNextRound st player
          st, persist st acc
        else st, acc
    | None -> st, acc
  { st with MatchIndex = st.MatchIndex + 1; Cursor = freshCursor }, acc

let rec private advance (st: State) (acc: Effect<CupBracket> list) : State * Effect<CupBracket> list =
  let cfg = st.Config
  if st.Waiting.IsSome then st, acc
  elif st.Done then st, acc @ [ Finished ]
  elif not st.RoundStarted then
    if st.Players.Length <= 1 || st.Stopped then advance { st with Done = true } acc
    else
      // the round's matches get this round's players
      let round = roundOf st st.RoundNumber
      let assigned =
        pairs st |> List.indexed |> List.fold (fun (st: State) (i, (a, b)) ->
          updateMatch st { round.Matches.[i] with A = a.Name; B = b.Name; RatingA = a.Rating; RatingB = b.Rating }) st
      let st = { assigned with RoundStarted = true; MatchIndex = 0; Winners = []; Cursor = freshCursor }
      advance st (persist st acc)
  elif st.MatchIndex >= (pairs st).Length then
    advance { st with Players = st.Winners; RoundNumber = st.RoundNumber + 1; RoundStarted = false } acc
  else
    let m = currentMatch st
    let c = st.Cursor
    if m.Decided && not c.Entered then
      let st, acc = endMatch st acc
      advance st acc
    elif not c.Entered then
      // entering the match: its openings, its games left, a decision the score already makes
      let played = m.Games.Length
      // a resume inside a tiebreak finishes the half-played pair first
      let remaining = MatchScore.gamesLeft (gamesFor cfg st.RoundNumber) played
      let st, m, acc =
        if cfg.UniquePerMatchOnly && cfg.RandomOpenings && cfg.Openings.Length > 1 && m.Order.Length < cfg.Openings.Length then
          let m = { m with Order = shuffled cfg $"cup-match|{m.Round}|{m.Id}" }
          let st = updateMatch st m
          st, m, persist st acc
        else st, m, acc
      let openings =
        if cfg.UniquePerMatchOnly then
          if cfg.RandomOpenings && cfg.Openings.Length > 1 then m.Order |> List.map (fun i -> cfg.Book.[i % cfg.Book.Length])
          else cfg.Openings
        else globalOpenings st
      let st, acc =
        match MatchScore.decide m.ScoreA m.ScoreB remaining with
        | Some side ->
            let m = { m with Decided = true; Winner = Some (if side = MatchScore.SideA then m.A else m.B) }
            let st = updateMatch st m
            let st = match winnerOf st m with Some p -> placeInNextRound st p | None -> st
            st, persist st acc
        | None -> st, acc
      advance { st with Cursor = { Entered = true; Remaining = remaining; Local = played / 2; Failures = 0; Openings = openings; Unit = None } } acc
    elif m.Decided || c.Failures >= cfg.MaxFailures || st.Stopped then
      let st, acc = endMatch st acc
      advance st acc
    else
      match c.Unit with
      | None ->
          // the next unit: an opening (the last game's when one is left half-played) and its colours
          let st, acc, c =
            if c.Remaining = 0 then st, acc @ [ AddTotalGames 2 ], { c with Remaining = 2 } else st, acc, c
          let a, b = (pairs st).[st.MatchIndex]
          let odd = m.Games.Length % 2 = 1
          let st, opening, local, acc =
            if odd then
              let last = List.last m.Games
              match c.Openings |> List.tryFind (fun o -> openingHash o = last.OpeningHash) with
              | Some o -> st, o, c.Local, acc
              | None ->
                  let st, o = nextOpening st c.Openings c.Local
                  st, o, c.Local, acc @ [ Info $"Cup resume: opening hash {last.OpeningHash} not found in opening book - using next available opening." ]
            else
              let used = m.Games |> List.map _.OpeningHash |> Set.ofList
              let index =
                if used.Count < c.Openings.Length then PairingHelper.nextUnusedOpeningIndex used c.Openings c.Local
                else c.Local % c.Openings.Length
              st, c.Openings.[index], index + 1, acc
          let colours =
            if odd then (if (List.last m.Games).White = a.Name then [ b, a ] else [ a, b ])
            else [ a, b; b, a ]
          let planned =
            PairingHelper.buildRemainingCupPairings (toCupMatch m) a b c.Openings opening colours c.Remaining local (roundGames st m.Round)
          let unit = { Opening = opening; Hash = openingHash opening; Colours = colours }
          advance { st with Cursor = { c with Local = local; Unit = Some unit } } (acc @ [ Notify (Update.PairingList planned) ])
      | Some u ->
          match u.Colours with
          | (white, black) :: rest when not m.Decided && c.Remaining > 0 && not st.Stopped ->
              let game =
                { Opening = u.Opening; White = white; Black = black; GameNr = st.Played + 1
                  RoundNr = $"{m.Round}.{roundGames st m.Round + 1}"; OpeningHash = u.Hash }
              { st with Cursor = { c with Unit = Some { u with Colours = rest } }; Waiting = Some game }, acc @ [ Play game ]
          | _ -> advance { st with Cursor = { c with Unit = None } } acc

let step (st: State) (event: Event) : State * Effect<CupBracket> list =
  match event with
  | Start ->
      let cfg = st.Config
      // the GUI's pairing list starts empty
      let acc = Notify (Update.PairingList (ResizeArray<Pairing>())) :: (if st.Created then persist st [] else [])
      // the tournament's opening order, made once (and again on a resume of more than the 50 kept)
      let st, acc =
        if cfg.RandomOpenings && cfg.Openings.Length > 1 && not cfg.UniquePerMatchOnly
           && (st.GlobalOrder.IsEmpty || st.GlobalOrder.Length < cfg.Openings.Length) then
          let st = { st with GlobalOrder = shuffled cfg "cup-openings" }
          st, persist st acc
        else st, acc
      advance st acc
  | Cancel -> { st with Stopped = true }, []
  | GameEnded result ->
      match st.Waiting with
      | None -> st, []
      | Some game ->
          let cfg = st.Config
          let st = { st with Waiting = None }
          let m = currentMatch st
          let c = st.Cursor
          match result with
          | Some r ->
              let played = st.Played + 1
              let cupGame =
                { GameNr = played; White = game.White.Name; Black = game.Black.Name
                  OpeningId = string game.Opening.GameNumber; OpeningHash = game.OpeningHash; Result = r.Result }
              let a, b = MatchScore.addGame (m.ScoreA, m.ScoreB) (game.White.Name = m.A) r.Result
              let remaining = c.Remaining - 1
              let m = { m with Games = m.Games @ [ cupGame ]; ScoreA = a; ScoreB = b }
              let m =
                match MatchScore.decide a b remaining with
                | Some side -> { m with Decided = true; Winner = Some (if side = MatchScore.SideA then m.A else m.B) }
                | None -> m
              let acc = if m.Decided && remaining > 0 then [ AddTotalGames (-remaining) ] else []
              let st = updateMatch { st with Played = played; Cursor = { c with Failures = 0; Remaining = remaining } } m
              let st = if m.Decided then (match winnerOf st m with Some p -> placeInNextRound st p | None -> st) else st
              advance st (persist st acc)
          | None ->
              let failures = c.Failures + 1
              let st = { st with Cursor = { c with Failures = failures } }
              if failures >= cfg.MaxFailures then
                // an abandoned match has no winner and the next round pairs the survivors: the cup cannot go on
                let text = $"Abandoning cup match {game.White.Name} vs {game.Black.Name} after {failures} consecutive unplayable games - stopping the tournament"
                advance { st with Stopped = true } [ Critical text; StopRun ]
              else advance st []
