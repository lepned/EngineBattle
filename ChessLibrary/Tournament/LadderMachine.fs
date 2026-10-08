/// A ladder as a pure step: state + event -> state + effects; ModeRunner plays the games it asks
/// for. Mirrors the runner it replaces game for game: a match left undecided is finished first,
/// then the bottom engine climbs, challenging the one above in mini-matches until one engine is left.
module ChessLibrary.LadderMachine

open System
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.LadderTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.ModeRunner

type Config =
  { Players: EngineConfig list
    /// Games a match: two per game pair.
    GamesPerMatch: int
    RandomOpenings: bool
    Seed: int
    Openings: PgnGame list
    /// the same book, indexed: the shuffled orders look openings up by number
    Book: PgnGame[]
    /// Unplayable games in a row after which the ladder stops (it cannot decide the match).
    MaxFailures: int
    TournamentName: string }

type Match =
  { Id: int
    Climb: int
    Challenger: string
    Defender: string
    ChallengerRating: int
    DefenderRating: int
    ScoreChallenger: float
    ScoreDefender: float
    Winner: string option
    Decided: bool
    Games: LadderGame list }

type private Unit = { Opening: PgnGame; Hash: string; Colours: (EngineConfig * EngineConfig) list }

type private Phase =
  /// A match the file left undecided: its result counts even if the run is then cancelled.
  | Resumed of int
  | Climbing
  | Playing of int
  | Over

type private Cursor = { Remaining: int; Failures: int; Unit: Unit option }

type State =
  private
    { Config: Config
      Header: string * int
      Rankings: string list
      Ladder: LadderProgress.Ladder
      Matches: Match list
      NextOpeningIndex: int
      GlobalOrder: int list
      NextMatchId: int
      Played: int
      Phase: Phase
      Cursor: Cursor
      Waiting: Pairing option
      Stopped: bool
      /// Saved at Start: a resume whose decided matches put the ladder elsewhere.
      Replayed: bool }

let private openingHash (o: PgnGame) = Hash.computeOpeningHashFromGame o

let private engine (cfg: Config) name = cfg.Players |> List.find (fun e -> e.Name = name)

let private ofMatch (m: LadderMatch) =
  { Id = m.MatchId; Climb = m.ClimbNumber; Challenger = m.Challenger; Defender = m.Defender
    ChallengerRating = m.ChallengerRating; DefenderRating = m.DefenderRating
    ScoreChallenger = m.ScoreChallenger; ScoreDefender = m.ScoreDefender; Winner = m.Winner; Decided = m.IsDecided
    Games = List.ofSeq m.Games }

let private toLadderMatch (m: Match) : LadderMatch =
  { MatchId = m.Id; ClimbNumber = m.Climb; Challenger = m.Challenger; Defender = m.Defender
    ChallengerRating = m.ChallengerRating; DefenderRating = m.DefenderRating
    ScoreChallenger = m.ScoreChallenger; ScoreDefender = m.ScoreDefender; Winner = m.Winner; IsDecided = m.Decided
    Games = ResizeArray m.Games }

/// The ladder as its file holds it.
let toFile (st: State) : LadderState =
  let name, gamePairs = st.Header
  { TournamentName = name
    GamePairsPerMatch = gamePairs
    InitialRankings = ResizeArray st.Rankings
    SurvivingEngines = ResizeArray st.Ladder.Surviving
    EliminatedEngines = ResizeArray st.Ladder.Eliminated
    CurrentClimbNumber = st.Ladder.Climb
    CurrentClimberIndex = st.Ladder.Climber
    Matches = ResizeArray (st.Matches |> List.map toLadderMatch)
    NextOpeningIndex = st.NextOpeningIndex
    GlobalOpeningOrder = ResizeArray st.GlobalOrder
    UpdatedUtc = DateTime.UtcNow }

let private persist (st: State) (acc: Effect<LadderState> list) = acc @ [ Persist (toFile st) ]

/// The machine's config from the tournament and its book.
let configOf (tourny: ChessLibrary.TypesDef.Tournament.Tournament) (openings: PgnGame list) =
  let o = tourny.LadderOptions
  let pairs = if obj.ReferenceEquals(o, null) then 4 else o.GamePairsPerMatch
  { Players = tourny.EngineSetup.Engines
    GamesPerMatch = max 1 pairs * 2
    RandomOpenings = (if obj.ReferenceEquals(o, null) then false else o.RandomOpenings)
    Seed = tourny.Opening.Seed
    Openings = openings
    Book = Array.ofList openings
    MaxFailures = 3
    TournamentName = tourny.Name }

/// A machine for a new ladder, or one resumed from its file. `gamesAlreadyPlayed`: the games in
/// the PGN, for the game numbers when the file has none.
let create (cfg: Config) (loaded: LadderState option) (gamesAlreadyPlayed: int) : State =
  match loaded with
  | Some s ->
      let matches = [ for m in s.Matches -> ofMatch m ]
      let saved : LadderProgress.Ladder =
        { Surviving = List.ofSeq s.SurvivingEngines; Eliminated = List.ofSeq s.EliminatedEngines
          Climb = s.CurrentClimbNumber; Climber = s.CurrentClimberIndex }
      // a resume replays the decided matches: a crash between saving a result and advancing
      // the ladder cannot leave it behind (and the same match played again)
      let ladder, replayed =
        if matches.IsEmpty then saved, false
        else
          let decided =
            matches |> List.filter _.Decided |> List.sortBy _.Id
            |> List.map (fun m -> m.Challenger, m.Defender, m.Winner |> Option.defaultValue m.Defender)
          let replayed = LadderProgress.replay (List.ofSeq s.InitialRankings) decided
          if replayed <> saved then replayed, true else saved, false
      let inFile = matches |> List.sumBy (fun m -> m.Games.Length)
      { Config = cfg
        Header = s.TournamentName, s.GamePairsPerMatch
        Rankings = List.ofSeq s.InitialRankings
        Ladder = ladder
        Matches = matches
        NextOpeningIndex = s.NextOpeningIndex
        GlobalOrder = (if isNull s.GlobalOpeningOrder then [] else List.ofSeq s.GlobalOpeningOrder)
        NextMatchId = (matches |> List.map _.Id |> List.fold max 0) + 1
        Played = (if inFile > 0 then inFile else gamesAlreadyPlayed)
        Phase = Climbing
        Cursor = { Remaining = 0; Failures = 0; Unit = None }
        Waiting = None
        Stopped = false
        Replayed = replayed }
  | None ->
      let sorted = cfg.Players |> List.sortByDescending (fun e -> e.Rating) |> List.map (fun e -> e.Name)
      { Config = cfg
        Header = cfg.TournamentName, cfg.GamesPerMatch
        Rankings = sorted
        Ladder = LadderProgress.start sorted
        Matches = []
        NextOpeningIndex = 0
        GlobalOrder = []
        NextMatchId = 1
        Played = gamesAlreadyPlayed
        Phase = Climbing
        Cursor = { Remaining = 0; Failures = 0; Unit = None }
        Waiting = None
        Stopped = false
        Replayed = false }

/// The tournament's total: a match's games for each engine but one, decided matches as they
/// ended, an undecided one with every tiebreak pair it has begun.
let totalGames (st: State) =
  let gpm = st.Config.GamesPerMatch
  st.Matches |> List.fold (fun total m ->
    let counted = if m.Decided then m.Games.Length else MatchScore.gamesCounted gpm m.Games.Length
    total + (counted - gpm)) ((st.Config.Players.Length - 1) * gpm)

/// The game number the next played game gets.
let played (st: State) = st.Played

let private globalOpenings (st: State) =
  let cfg = st.Config
  if not st.GlobalOrder.IsEmpty then st.GlobalOrder |> List.map (fun i -> cfg.Book.[i % cfg.Book.Length]) else cfg.Openings

let private nextOpening (st: State) =
  let openings = globalOpenings st
  let index = if st.NextOpeningIndex >= openings.Length then 0 else st.NextOpeningIndex
  { st with NextOpeningIndex = index + 1 }, openings.[index]

let private matchOf (st: State) id = st.Matches |> List.find (fun m -> m.Id = id)

/// The games a climb has played: its games are labelled climb.n in the order played, across its
/// matches and their tiebreaks, so no two share a label.
let private climbGames (st: State) climb = st.Matches |> List.filter (fun m -> m.Climb = climb) |> List.sumBy (fun m -> m.Games.Length)

let private updateMatch (st: State) (m: Match) =
  { st with Matches = st.Matches |> List.map (fun x -> if x.Id = m.Id then m else x) }

let private standings (st: State) (climbInfo: string) =
  [ yield ""
    yield climbInfo
    yield "Current Ladder:"
    for i in 0 .. st.Ladder.Surviving.Length - 1 do
      let name = st.Ladder.Surviving.[i]
      let marker = if i = st.Ladder.Climber && st.Ladder.Surviving.Length > 1 then " <- climbing" else ""
      // a printout must not throw: the step's effects (the save of the game just played) go with it
      let rating =
        match st.Config.Players |> List.tryFind (fun e -> e.Name = name) with
        | Some e -> string e.Rating
        | None -> "?"
      yield sprintf "  %d. %s (Rating: %s)%s" (i + 1) name rating marker
    if not st.Ladder.Eliminated.IsEmpty then
      yield sprintf "Eliminated: %s" (st.Ladder.Eliminated |> List.rev |> String.concat ", ")
    yield "" ]
  |> List.map Print

/// A decided match moves the ladder on: the loser is out.
let private processResult (st: State) (m: Match) (acc: Effect<LadderState> list) =
  if not m.Decided then st, acc
  else
    let winner = m.Winner |> Option.defaultValue m.Defender
    let loser = if winner = m.Challenger then m.Defender else m.Challenger
    let info =
      sprintf "=== Ladder Match %d (Climb %d) === [Challenger] %s vs [Defender] %s: %s wins %.1f-%.1f. %s eliminated."
        m.Id st.Ladder.Climb m.Challenger m.Defender winner m.ScoreChallenger m.ScoreDefender loser
    let st = { st with Ladder = LadderProgress.advance st.Ladder (m.Challenger, m.Defender, winner) }
    st, persist st acc @ standings st info

let private champion (st: State) =
  if st.Ladder.Surviving.Length <> 1 then []
  else
    [ yield ""
      yield "========================================="
      yield sprintf "  LADDER CHAMPION: %s" st.Ladder.Surviving.[0]
      yield "========================================="
      yield "Final standings:"
      yield sprintf "  1. %s (Champion)" st.Ladder.Surviving.[0]
      yield! st.Ladder.Eliminated |> List.rev |> List.mapi (fun i name -> sprintf "  %d. %s (Eliminated)" (i + 2) name)
      yield "" ]
    |> List.map Print

let rec private advance (st: State) (acc: Effect<LadderState> list) : State * Effect<LadderState> list =
  let cfg = st.Config
  match st.Waiting, st.Phase with
  | Some _, _ -> st, acc
  | None, Over -> st, acc @ [ Finished ]
  | None, Climbing ->
      if st.Stopped || st.Ladder.Surviving.Length <= 1 then advance { st with Phase = Over } (acc @ champion st)
      else
        match LadderProgress.next st.Ladder with
        | None -> advance { st with Phase = Over } (acc @ champion st)
        | Some (ladder, (challenger, defender)) ->
            let c, d = engine cfg challenger, engine cfg defender
            let m =
              { Id = st.NextMatchId; Climb = ladder.Climb; Challenger = challenger; Defender = defender
                ChallengerRating = c.Rating; DefenderRating = d.Rating; ScoreChallenger = 0.0; ScoreDefender = 0.0
                Winner = None; Decided = false; Games = [] }
            let st =
              { st with Ladder = ladder; Matches = st.Matches @ [ m ]; NextMatchId = st.NextMatchId + 1
                        Phase = Playing m.Id; Cursor = { Remaining = cfg.GamesPerMatch; Failures = 0; Unit = None } }
            let header =
              [ Print ""; Print (sprintf "=== Ladder Match %d (Climb %d) ===" m.Id ladder.Climb)
                Print (sprintf "[Challenger] %s (%d) vs [Defender] %s (%d)" challenger c.Rating defender d.Rating) ]
            advance st (persist st acc @ header)
  | None, (Resumed id | Playing id) ->
      let m = matchOf st id
      let c = st.Cursor
      if m.Decided || c.Failures >= cfg.MaxFailures || st.Stopped then
        // the match is over: a resumed one counts even after a cancellation, a new one only without
        let resumed = match st.Phase with Resumed _ -> true | _ -> false
        let st, acc = if resumed || not st.Stopped then processResult st m acc else st, acc
        advance { st with Phase = Climbing; Cursor = { Remaining = 0; Failures = 0; Unit = None } } acc
      else
        let challenger, defender = engine cfg m.Challenger, engine cfg m.Defender
        match c.Unit with
        | None ->
            let st, acc, c =
              if c.Remaining = 0 then
                st, acc @ [ AddTotalGames 2; Print (sprintf "  Tiebreak: scores tied %.1f-%.1f, playing 2 extra games" m.ScoreChallenger m.ScoreDefender) ],
                { c with Remaining = 2 }
              else st, acc, c
            let odd = m.Games.Length % 2 = 1
            let st, opening =
              if odd then
                let last = List.last m.Games
                match globalOpenings st |> List.tryFind (fun o -> openingHash o = last.OpeningHash) with
                | Some o -> st, o
                | None -> nextOpening st
              else nextOpening st
            let hash = openingHash opening
            let colours =
              if odd then (if (List.last m.Games).White = challenger.Name then [ defender, challenger ] else [ challenger, defender ])
              else [ challenger, defender; defender, challenger ]
            // the preview: this unit's games, then the book's next openings for the games left
            let planned = ResizeArray<Pairing>()
            let mutable planIndex = 0
            let mutable planRemaining = c.Remaining
            let add opening hash (white: EngineConfig) (black: EngineConfig) =
              planned.Add { Opening = opening; White = white; Black = black; GameNr = 0
                            RoundNr = $"{m.Climb}.{climbGames st m.Climb + planIndex + 1}"; OpeningHash = hash }
              planIndex <- planIndex + 1
              planRemaining <- planRemaining - 1
            for white, black in colours do
              if planRemaining > 0 then add opening hash white black
            let openings = globalOpenings st
            let mutable peek = st.NextOpeningIndex
            while planRemaining > 0 do
              let o = openings.[peek % openings.Length]
              peek <- peek + 1
              for white, black in [ challenger, defender; defender, challenger ] do
                if planRemaining > 0 then add o (openingHash o) white black
            let unit = { Opening = opening; Hash = hash; Colours = colours }
            advance { st with Cursor = { c with Unit = Some unit } } (acc @ [ Notify (Update.PairingList planned) ])
        | Some u ->
            match u.Colours with
            | (white, black) :: rest when c.Remaining > 0 ->
                let game =
                  { Opening = u.Opening; White = white; Black = black; GameNr = st.Played + 1
                    RoundNr = $"{m.Climb}.{climbGames st m.Climb + 1}"; OpeningHash = u.Hash }
                { st with Cursor = { c with Unit = Some { u with Colours = rest } }; Waiting = Some game }, acc @ [ Play game ]
            | _ -> advance { st with Cursor = { c with Unit = None } } acc

let step (st: State) (event: Event) : State * Effect<LadderState> list =
  match event with
  | Start ->
      let cfg = st.Config
      // the GUI's pairing list starts empty
      let acc = Notify (Update.PairingList (ResizeArray<Pairing>())) :: (if st.Replayed then [ Info "Ladder: the saved standing did not match its decided matches - replayed them"; Persist (toFile st) ] else [])
      let st, acc =
        if cfg.RandomOpenings && cfg.Openings.Length > 1 && (st.GlobalOrder.IsEmpty || st.GlobalOrder.Length < cfg.Openings.Length) then
          let st = { st with GlobalOrder = Scheduler.Shared.seededOrder cfg.Seed "ladder-openings" cfg.Openings.Length |> List.ofArray }
          st, persist st acc
        else st, acc
      // a match the file left undecided is finished first
      let st =
        match st.Matches |> List.tryFind (fun m -> not m.Decided) with
        | Some m ->
            // in tiebreak territory: finish the current pair if it is half-played
            let remaining = MatchScore.gamesLeft cfg.GamesPerMatch m.Games.Length
            { st with Phase = Resumed m.Id; Cursor = { Remaining = remaining; Failures = 0; Unit = None } }
        | None -> st
      advance st acc
  | Cancel -> { st with Stopped = true }, []
  | GameEnded result ->
      match st.Waiting, st.Phase with
      | Some game, (Resumed id | Playing id) ->
          let cfg = st.Config
          let st = { st with Waiting = None }
          let m = matchOf st id
          let c = st.Cursor
          match result with
          | Some r ->
              let played = st.Played + 1
              let ladderGame =
                { GameNr = played; White = game.White.Name; Black = game.Black.Name
                  OpeningId = string game.Opening.GameNumber; OpeningHash = game.OpeningHash; Result = r.Result }
              let sc, sd = MatchScore.addGame (m.ScoreChallenger, m.ScoreDefender) (game.White.Name = m.Challenger) r.Result
              let remaining = c.Remaining - 1
              let m = { m with Games = m.Games @ [ ladderGame ]; ScoreChallenger = sc; ScoreDefender = sd }
              let m =
                match MatchScore.decide sc sd remaining with
                | Some side -> { m with Decided = true; Winner = Some (if side = MatchScore.SideA then m.Challenger else m.Defender) }
                | None -> m
              let acc = if m.Decided && remaining > 0 then [ AddTotalGames (-remaining) ] else []
              let st = updateMatch { st with Played = played; Cursor = { c with Failures = 0; Remaining = remaining } } m
              advance st (persist st acc)
          | None ->
              let failures = c.Failures + 1
              let st = { st with Cursor = { c with Failures = failures } }
              if failures >= cfg.MaxFailures then
                // an undecided match eliminates nobody: the same pairing would be rebuilt for ever
                let text = $"Abandoning ladder match {m.Challenger} vs {m.Defender} after {failures} consecutive unplayable games - stopping the tournament"
                advance { st with Stopped = true } [ Critical text; StopRun ]
              else advance st []
      | _ -> st, []
