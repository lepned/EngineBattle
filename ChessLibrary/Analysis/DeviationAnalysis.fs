module ChessLibrary.DeviationAnalysis

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.GameAnalysis

let movesFromPgn (pgn:PgnGame) =
  [
    for m in pgn.Mainline -> m.San
  ]

type MoveStore = {Move: string; Fen: string; White: string; Black:string; Hash: UInt64; MoveNr: int }
  with static member Empty = {Move=""; Fen=""; White=""; Black=""; Hash=0UL; MoveNr=0}

let createMoveStore (move: string) (moveNr: int) (fen: string) (white: string) (black: string) (hash: UInt64) =
    {Move=move; Fen=fen; White=white; Black=black; Hash=hash; MoveNr=moveNr}

type GameStore = {Moves: MoveStore list; Game: PgnGame; Board: Chess.Board; Opening : string }

let createGameStore (moves: MoveStore list) (pgn: PgnGame) (board: Chess.Board) (opening : string ) =
  {Moves=moves; Game=pgn; Board=board ; Opening = opening}

let findAllDeviationsForPlayersAlt (pgnGames: PgnGame seq) (refPlayer: string option) (comparePlayers: string list ) =
  let players =
    match comparePlayers with
    |[] ->
      if refPlayer.IsSome then
        pgnGames |> Seq.map(fun e -> e.GameMetaData.White) |> Seq.distinct |> Seq.except [refPlayer.Value] |> Seq.toList
      else
        pgnGames |> Seq.map(fun e -> e.GameMetaData.White) |> Seq.distinct |> Seq.toList
    |list -> list

  let allGames = pgnGames |> Seq.toList
  // Per game, not all-or-nothing - see prepareDeviationPlay.
  allGames
  |> List.iter (fun game ->
      if String.IsNullOrWhiteSpace game.GameMetaData.OpeningHash then
        Hash.writeOpeningHashToPgnGame game)
  let gamesGroupedPerOpening = allGames |> List.groupBy (fun game -> game.GameMetaData.OpeningHash)

  let gameStore = ResizeArray<GameStore>()
  for (openingHash, gamesInOpening) in gamesGroupedPerOpening do
    for player in players do
      let games =
        match refPlayer with
        |Some p ->
          gamesInOpening
          |> Seq.filter(fun e -> e.GameMetaData.White = p || e.GameMetaData.Black = p)
          |> Seq.filter(fun e -> e.GameMetaData.White = player || e.GameMetaData.Black = player)
          |> Seq.toList
        |None ->
          gamesInOpening
          |> Seq.filter(fun e -> e.GameMetaData.White = player || e.GameMetaData.Black = player)
          |> Seq.toList
      for game in games do
        let gameMoveStore = ResizeArray<MoveStore>()
        let replayBoard = new Chess.Board()
        let mutable pos = replayBoard.Position
        if game.Fen <> "" then
          replayBoard.LoadFen game.Fen

        let mutable idx = 0
        let moves = movesFromPgn game
        for m in moves do
          idx <- idx + 1
          replayBoard.PlaySanMove m
          let hash = replayBoard.DeviationHash()
          let fen = replayBoard.FEN()
          let longMove = replayBoard.UciMovesPlayed.[replayBoard.UciMovesPlayed.Count - 1]
          let moveStore = createMoveStore longMove idx fen game.GameMetaData.White game.GameMetaData.Black hash
          gameMoveStore.Add moveStore

        let moveStore = gameMoveStore |> Seq.toList
        gameStore.Add (createGameStore moveStore game replayBoard openingHash)
  gameStore


// Define a record to hold details of a deviating game.
type DeviationDetail = {
    ReferenceGame: PgnGame
    DevGame : PgnGame
    PreviousMove: string*string
    DeviationMove: string*string
    /// Position before either move - the shared starting point of the disagreement.
    FENBefore: string
    FENPrev: string
    FENDev: string
    WhitePlayer: string
    BlackPlayer: string
    DeviatedBy: string
    MoveNr: int
    GameStore: GameStore
}

let findDeviationDetailsAlt (collection: GameStore array) =
    //get the first game and use it as reference
    let refGame =
      match collection |> Array.tryHead with
      | None -> None
      | Some refGame -> Some refGame

    if refGame.IsNone || collection.Length < 2 then
      None
    else
      let refGame = refGame.Value
      // Loop over each move index until we find a deviation.
      let rec loop moveIndex (gamesLeft:GameStore array) (devs: DeviationDetail list) =
          // Determine the minimum move count across all games.
          let minMoves =
                if gamesLeft.Length = 0 then 0
                else gamesLeft |> Array.map (fun gs -> gs.Board.MovesAndFenPlayed.Count) |> Array.min
          if gamesLeft.Length = 0 || moveIndex >= minMoves then
              Some devs
          else
            // Collect the current hash for each game at moveIndex.
            let currentHashes =
                gamesLeft |> Array.map (fun gs -> gs.Game, gs.Board.HashKeys.[moveIndex])
            if currentHashes |> Array.distinctBy snd |> Array.length > 1 then
                // For each game that deviates from the majority hash, record its details.
                let details =
                    gamesLeft
                    |> Array.choose (fun gs ->
                        if gs.Board.HashKeys.[moveIndex] <> refGame.Board.HashKeys[moveIndex] then
                            let fen = gs.Board.MovesAndFenPlayed.[moveIndex].FenAfterMove
                            let moveFen = refGame.Board.MovesAndFenPlayed.[moveIndex]
                            let moveFenDev = gs.Board.MovesAndFenPlayed.[moveIndex]
                            Some {
                                ReferenceGame = refGame.Game
                                DevGame = gs.Game
                                FENBefore =
                                  if moveIndex > 0 then
                                    refGame.Board.MovesAndFenPlayed.[moveIndex - 1].FenAfterMove
                                  elif refGame.Game.Fen <> "" then refGame.Game.Fen
                                  else "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                                PreviousMove = moveFen.ShortSan, moveFen.Move.LongSan  // the expected (majority) move at this index
                                DeviationMove = moveFenDev.ShortSan, moveFenDev.Move.LongSan
                                FENPrev = refGame.Board.MovesAndFenPlayed.[moveIndex].FenAfterMove
                                FENDev = fen
                                WhitePlayer = gs.Game.GameMetaData.White
                                BlackPlayer = gs.Game.GameMetaData.Black
                                DeviatedBy = if gs.Board.Game[moveIndex].STM = 0uy then gs.Game.GameMetaData.White else gs.Game.GameMetaData.Black
                                MoveNr = moveIndex
                                GameStore = gs
                            }
                        else None)
                    |> Array.toList
                if details.Length > 0 then
                  let gsToRemove = details |> List.map(fun gs -> gs.GameStore)
                  let gamesUpdated = gamesLeft |> Array.except gsToRemove
                  let devsUpdated = devs @ details
                  loop (moveIndex + 1) gamesUpdated devsUpdated
                else  // should not happen
                  loop (moveIndex + 1) gamesLeft devs
            else
                loop (moveIndex + 1) gamesLeft devs
      loop 0 collection []

let getMoveDifferences (games: PgnGame seq) compareList  refPlayer =
    let refPlayerOption = if String.IsNullOrEmpty refPlayer then None else Some refPlayer //"Stockfish_250213"
    let gameStore = findAllDeviationsForPlayersAlt games refPlayerOption compareList
    let uniqueStore = gameStore |> Seq.distinctBy(fun e -> e.Game.GameNumber) |> Seq.toList
    let players = gameStore |> Seq.map(fun e -> e.Game.GameMetaData.White) |> Seq.distinct |> Seq.toList
    [
      for p in players do
        let group =
          uniqueStore
          |> Seq.filter(fun store -> store.Game.GameMetaData.White = p || store.Game.GameMetaData.Black = p)
          |> Seq.groupBy (fun store -> store.Opening)
          |> Seq.toList

        for (key, value) in group do
          let arr = value |> Seq.toArray
          match findDeviationDetailsAlt arr with
          | Some devs -> yield! devs
          | _ -> ()
    ]

let mapDevDetailToMoveDeviation (dev: DeviationDetail) =
  let moveDeviation =
    { Round = dev.ReferenceGame.GameMetaData.Round + " - " + dev.DevGame.GameMetaData.Round
      GameNr = dev.ReferenceGame.GameNumber
      MoveNr = dev.MoveNr
      Color = if dev.WhitePlayer = dev.DeviatedBy then "w" else "b"
      PrevSanMove = dev.PreviousMove
      PlayerToDeviate = dev.DeviatedBy
      Opponent = if dev.DeviatedBy = dev.WhitePlayer then dev.BlackPlayer else dev.WhitePlayer
      DevSanMove = dev.DeviationMove
      Result = dev.ReferenceGame.GameMetaData.Result
      DevRes = dev.DevGame.GameMetaData.Result
      PgnGamePair = dev.ReferenceGame, dev.DevGame
      PreFen = dev.FENBefore
      PrevFen = dev.FENPrev
      DevFen = dev.FENDev }
  moveDeviation

let findMoveDifferencesInPGN (pgn: PgnGame seq) refPlayer compareList =
  let compareList = compareList |> Seq.toList
  let res =
    getMoveDifferences pgn compareList refPlayer
    |> Seq.map mapDevDetailToMoveDeviation
    |> Seq.distinctBy(fun e -> e.Round)
    |> Seq.sortBy(fun e -> e.Round)
    |> Seq.truncate 100
  res

// ---------------------------------------------------------------------------
// Position-keyed deviation analysis.
//
// The older functions above replay a reference game and compare by ply index. That finds
// disagreements between two games of the same opening, but it cannot see an engine
// contradicting *itself*: in a colour-reversed pair the same engine is never on move in the
// same position twice, and in a gauntlet the repeats it does get are spread across games
// that the reference-replay never compares. Measured against four real PGNs, the old
// reference-replay self-comparison returned 0 where 8, 4, 3 and 2 genuine self-deviations
// existed; it has since been removed, and this is the only self view.
//
// Keying on the position instead removes that blind spot. The unit of a deviation is one
// position: if three games reach it and play two different moves, that is one deviation with
// two choices, not two deviations.
//
// The key is the position plus the halfmove clock, so a transposition is matched when the two
// games are equally far from a fifty-move draw, and not otherwise. One game can still reach a
// bucket more than once, so each game contributes only its first move there: a repetition
// inside a single game is not a disagreement with anyone, and taking the first occurrence also
// means both games have seen the position the same number of times.
// ---------------------------------------------------------------------------

/// One game in which a particular move was chosen here.
type ChoiceInstance =
  { GameNumber: int
    Engine: string
    /// The game's result as written in the PGN, which is always White-relative.
    Result: string
    /// The same result from the perspective of the engine that made this choice: 1, 0.5 or 0.
    /// Without this a "1-0" tells you nothing about whether choosing this move worked out,
    /// because the chooser may have been Black.
    Score: float }

/// One move that was played in a given position, and by whom.
type PositionChoice =
  { Move: string          // UCI
    San: string
    /// One entry per game, so game, engine and result stay tied together. Keeping them as
    /// three separate de-duplicated lists lost that: two games and two results could not be
    /// matched up again.
    Instances: ChoiceInstance list }
  member this.Engines = this.Instances |> List.map (fun i -> i.Engine) |> List.distinct |> List.sort
  member this.GameNumbers = this.Instances |> List.map (fun i -> i.GameNumber) |> List.sort
  /// Points from the games in which this move was chosen, from the perspective of the engine
  /// that chose it. A win and a loss are one point of two, as in any chess score - the average
  /// this used to be read as half a point of two.
  member this.Points = this.Instances |> List.sumBy (fun i -> i.Score)

/// A position that more than one game reached, where not everyone played the same move.
type PositionDeviation =
  { /// FEN of the position before the move - this is how a caller identifies it. The scan's
    /// internal key is deliberately not exposed: it mixes in the halfmove clock and means
    /// nothing outside a single run.
    Fen: string
    MoveNumber: int
    /// Side to move, taken from the FEN rather than ply parity - games can start from a FEN.
    Color: string
    /// Engines that played more than one distinct move here across different games.
    /// Empty means the disagreement is purely between different engines.
    SelfEngines: string list
    Choices: PositionChoice list }
  member this.IsSelfDeviation = not this.SelfEngines.IsEmpty
  member this.GameCount = this.Choices |> List.sumBy (fun c -> c.Instances.Length)

/// Per-engine self-consistency, which is the headline number for a gauntlet.
type EngineSelfSummary =
  { Engine: string
    /// Positions where this engine contradicted an earlier choice of its own.
    SelfDeviations: int
    /// Positions this engine reached in more than one game - the denominator. A position seen
    /// once can never show an inconsistency, so it is not evidence of consistency either.
    RepeatedPositions: int
    /// Every distinct position this engine chose a move in. RepeatedPositions is a subset, and
    /// often a small one: in a reverse-colour pairing it is structurally zero, because the
    /// engine is never the side to move in the same position twice.
    DistinctPositions: int }

/// How the opening was identified for one game, which decides how much its numbers are worth.
/// The rule lives in ChessUtilities.Opening so the opening hash and this analysis cannot drift
/// apart about which plies came out of the book.
type OpeningSource = Opening.OpeningSource

/// How trustworthy the opening detection was across a set of games.
type OpeningCoverage =
  { FromSearchData: int
    FromBookMarker: int
    Unknown: int
    Empty: int }
  member this.Total = this.FromSearchData + this.FromBookMarker + this.Unknown + this.Empty
  /// True when some games had no way to tell opening moves from choices.
  member this.HasUnverifiedOpenings = this.Unknown > 0

[<RequireQualifiedAccess>]
module private PositionScan =

  type Entry =
    { Engine: string
      Uci: string
      San: string
      GameNumber: int
      Result: string
      MoveNumber: int
      Color: string
      Fen: string }

  let wasSearched = Opening.wasSearched

  /// Moves since the last capture or pawn push - FEN field five. Part of the position key,
  /// because it is what separates two identical boards that are different distances from a
  /// fifty-move draw.
  let private halfmoveClock (fen: string) =
    let parts = fen.Split(' ')
    if parts.Length > 4 then
      match System.Int32.TryParse parts.[4] with
      | true, n -> n
      | _ -> 0
    else 0

  let choicePlies = Opening.choicePlies

  /// Replays every game once and buckets each ply by the hash of the position before it.
  ///
  /// Unsearched plies are replayed but not recorded: both sides were following the same opening
  /// line without thinking, so they are not choices. Counting them would inflate the denominator
  /// of every self-consistency rate, and - worse - a book move in one game meeting a searched
  /// move in another would be reported as a disagreement that never happened.
  let scan (games: PgnGame list) =
    let table = Dictionary<uint64, ResizeArray<Entry>>()
    let mutable fromSearch = 0
    let mutable fromMarker = 0
    let mutable unknown = 0
    let mutable empty = 0
    for game in games do
      let board = Chess.Board()
      if game.Fen <> "" then board.LoadFen game.Fen
      let mutable abandoned = false
      let isChoice, source = choicePlies game
      match source with
      | Opening.FromSearchData -> fromSearch <- fromSearch + 1
      | Opening.FromBookMarker -> fromMarker <- fromMarker + 1
      | Opening.Unknown -> unknown <- unknown + 1
      | Opening.NoMoves -> empty <- empty + 1
      let mutable plyIndex = -1
      for san in movesFromPgn game do
        plyIndex <- plyIndex + 1
        let searched = plyIndex < isChoice.Length && isChoice.[plyIndex]
        let fenBefore = board.FEN()
        let parts = fenBefore.Split(' ')
        let color = if parts.Length > 1 && parts.[1] = "b" then "b" else "w"
        let moveNumber =
          if parts.Length > 5 then
            match System.Int32.TryParse parts.[5] with
            | true, n -> n
            | _ -> 0
          else 0
        let engine =
          if color = "w" then game.GameMetaData.White else game.GameMetaData.Black
        // Key = Zobrist position + halfmove clock.
        //
        // The Zobrist hash alone covers the board, side to move, castling and en passant, but
        // not how the position was arrived at. Two games can show the same board while one is
        // far closer to a fifty-move draw than the other, and an engine may rightly choose
        // differently there. Measured on a 955-game file, keying on the position alone produced
        // 129 self-deviations of which 48 had a different clock.
        //
        // The obvious guard is the ply number (what Board.DeviationHash adds), but ply is only a
        // proxy: it also rejects transpositions whose context is genuinely identical. Keying on
        // the clock instead kept all 48 out and recovered 8 of those, for 81.
        let hashBefore = board.PositionHash() ^^^ (uint64 (halfmoveClock fenBefore) * 0x9E3779B97F4A7C15UL)
        // PlaySanMove ignores input it cannot resolve - a null move, an ambiguous or illegal
        // SAN - without throwing and without advancing the board. Reading the last UCI blindly
        // would attribute the PREVIOUS ply's move to this position, inventing a deviation, and
        // every later ply would be replayed from a board that no longer matches the PGN.
        let movesBefore = board.UciMovesPlayed.Count
        board.PlaySanMove san
        let advanced = board.UciMovesPlayed.Count > movesBefore
        let uci = if advanced then board.UciMovesPlayed.[board.UciMovesPlayed.Count - 1] else ""
        let entry =
          { Engine = engine
            Uci = uci
            San = san
            GameNumber = game.GameNumber
            Result = game.GameMetaData.Result
            MoveNumber = moveNumber
            Color = color
            Fen = fenBefore }
        if not advanced then
          // Board and PGN have diverged; nothing later in this game can be trusted.
          abandoned <- true

        if searched && not abandoned then
          match table.TryGetValue hashBefore with
          | true, list -> list.Add entry
          | _ ->
            let list = ResizeArray<Entry>()
            list.Add entry
            table.[hashBefore] <- list
    table, ({ FromSearchData = fromSearch; FromBookMarker = fromMarker; Unknown = unknown; Empty = empty } : OpeningCoverage)

let private deviationsFromScan (table: Dictionary<uint64, ResizeArray<PositionScan.Entry>>) : PositionDeviation list =
  [ for kv in table do
      // One entry per game: the move that game played the FIRST time it reached this position.
      // A position repeated inside a single game is not a disagreement, and collapsing it here
      // stops the tests below from pairing two moves that both came from the same game - which
      // a ply-free key now makes possible.
      let entries =
        kv.Value
        |> Seq.groupBy (fun e -> e.GameNumber)
        |> Seq.map (fun (_, es) -> Seq.head es)
        |> Seq.sortBy (fun e -> e.GameNumber)
        |> Seq.toList
      let distinctGames = entries.Length
      let distinctMoves = entries |> List.map (fun e -> e.Uci) |> List.distinct |> List.length
      if distinctGames > 1 && distinctMoves > 1 then
        // entries already holds one move per game, so two distinct moves for one engine here
        // necessarily come from two different games.
        let selfEngines =
          entries
          |> List.groupBy (fun e -> e.Engine)
          |> List.filter (fun (_, es) ->
               (es |> List.map (fun e -> e.Uci) |> List.distinct |> List.length) > 1)
          |> List.map fst
          |> List.sort
        // The PGN result is White-relative; the chooser may have been either colour.
        let scoreFor (color: string) (result: string) =
          match result, color with
          | "1-0", "w" | "0-1", "b" -> 1.0
          | "1-0", "b" | "0-1", "w" -> 0.0
          | "1/2-1/2", _ -> 0.5
          | _ -> 0.5   // unfinished or unknown: neither a win nor a loss
        let choices =
          entries
          |> Seq.groupBy (fun e -> e.Uci)
          |> Seq.map (fun (uci, es) ->
               let es = es |> Seq.toList
               { Move = uci
                 San = (es |> List.head).San
                 Instances =
                   es
                   |> List.map (fun e ->
                        { GameNumber = e.GameNumber
                          Engine = e.Engine
                          Result = e.Result
                          Score = scoreFor e.Color e.Result })
                   |> List.sortBy (fun i -> i.GameNumber) })
          |> Seq.sortByDescending (fun c -> c.Instances.Length)
          |> Seq.toList
        let first = List.head entries
        yield
          { Fen = first.Fen
            MoveNumber = first.MoveNumber
            Color = first.Color
            SelfEngines = selfEngines
            Choices = choices } ]
  |> List.sortBy (fun d -> d.MoveNumber)

let private selfSummaryFromScan (table: Dictionary<uint64, ResizeArray<PositionScan.Entry>>) : EngineSelfSummary list =
  let repeated = Dictionary<string, int>()
  let deviated = Dictionary<string, int>()
  let distinct = Dictionary<string, int>()
  let bump (d: Dictionary<string, int>) key =
    match d.TryGetValue key with
    | true, v -> d.[key] <- v + 1
    | _ -> d.[key] <- 1

  for kv in table do
    // Same reduction as above: one move per game, so a repetition inside one game is never
    // mistaken for the engine contradicting itself between games.
    let firstPerGame =
      kv.Value
      |> Seq.groupBy (fun e -> e.GameNumber)
      |> Seq.map (fun (_, es) -> Seq.head es)
      |> Seq.toList
    for (engine, es) in firstPerGame |> List.groupBy (fun e -> e.Engine) do
      bump distinct engine
      if es.Length > 1 then
        bump repeated engine
        if (es |> List.map (fun e -> e.Uci) |> List.distinct |> List.length) > 1 then
          bump deviated engine

  // Keyed on every engine that made a move, not just those with repeats: an engine with no
  // repeated positions is exactly the case the caller must be told about.
  [ for kv in distinct do
      yield
        { Engine = kv.Key
          SelfDeviations = (match deviated.TryGetValue kv.Key with | true, v -> v | _ -> 0)
          RepeatedPositions = (match repeated.TryGetValue kv.Key with | true, v -> v | _ -> 0)
          DistinctPositions = kv.Value } ]
  |> List.sortByDescending (fun s -> s.SelfDeviations)


/// Both views from a single replay of the games. Scanning a large PGN is the expensive part
/// -- 1811 games take ~3.5s -- so callers that want deviations and the per-engine summary
/// should ask for them together rather than paying for two scans.
let analyzePositionDeviations (pgnGames: PgnGame seq) =
  let table, coverage = PositionScan.scan (pgnGames |> Seq.toList)
  deviationsFromScan table, selfSummaryFromScan table, coverage



/// Text report of the position-keyed analysis, for the console verb. Same content the GUI
/// shows: opening coverage, per-engine self-consistency, then one line per position.
let printPositionDeviationsToConsole (devs: PositionDeviation list) (summary: EngineSelfSummary list) (coverage: OpeningCoverage) =
  let sb = System.Text.StringBuilder()
  let line (s: string) = sb.AppendLine s |> ignore
  line ""
  line (sprintf "Opening detection: %d games - search data %d, book marker %d, unknown %d, no moves %d"
          coverage.Total coverage.FromSearchData coverage.FromBookMarker coverage.Unknown coverage.Empty)
  if coverage.HasUnverifiedOpenings then
    line (sprintf "  WARNING: %d game(s) carry neither search data nor a book marker - self-deviations there may be book artefacts"
            coverage.Unknown)
  line ""
  line "Engine self-consistency - positions reached in more than one game, played differently the second time:"
  let measurable =
    summary |> List.filter (fun s -> s.RepeatedPositions > 0) |> List.sortByDescending (fun s -> s.RepeatedPositions)
  let unmeasurable = summary |> List.filter (fun s -> s.RepeatedPositions = 0)
  if measurable.IsEmpty then
    line "  not measurable: no engine reached the same position in more than one game"
    line "  (normal for reverse-colour pairings: the engine that faces a position in one game is the opponent in the other)"
  else
    let w = measurable |> List.map (fun s -> s.Engine.Length) |> List.max |> max 8
    // The rate is always shown when there is a denominator; the denominator beside it is the
    // measure of how much it is worth. 1 of 1 and 18 of 239 are left for the reader to weigh.
    line (sprintf "  %-*s %9s %9s %7s %9s" w "ENGINE" "SelfDevs" "Repeated" "Rate" "Distinct")
    for s in measurable do
      let rate = 100.0 * float s.SelfDeviations / float s.RepeatedPositions
      line (sprintf "  %-*s %9d %9d %6.1f%% %9d" w s.Engine s.SelfDeviations s.RepeatedPositions rate s.DistinctPositions)
  if not unmeasurable.IsEmpty then
    line (sprintf "  never met the same position twice: %s" (unmeasurable |> List.map (fun s -> s.Engine) |> String.concat ", "))
  line ""
  let selfN = devs |> List.filter (fun d -> d.IsSelfDeviation) |> List.length
  line (sprintf "Position deviations: %d (%d self, %d cross)" devs.Length selfN (devs.Length - selfN))
  if not devs.IsEmpty then
    line "  Move Side Type  Games  Choices as move [engines: #game result, ...]"
    for d in devs do
      let choices =
        d.Choices
        |> List.map (fun c ->
             let played = c.Instances |> List.map (fun i -> sprintf "#%d %s" i.GameNumber i.Result) |> String.concat ", "
             sprintf "%s [%s: %s]" c.San (c.Engines |> String.concat "/") played)
        |> String.concat "  |  "
      let selfNote = if d.IsSelfDeviation then sprintf "  (self: %s)" (d.SelfEngines |> String.concat ", ") else ""
      line (sprintf "  %4d %-4s %-5s %5d  %s%s"
              d.MoveNumber (if d.Color = "w" then "W" else "B")
              (if d.IsSelfDeviation then "self" else "cross") d.GameCount choices selfNote)
  sb.ToString()

/// Where the opening ends for one game: how many leading plies were not the engine's own
/// choice, and how that was decided. Exposed so the rule can be tested directly - it is the
/// single most consequential judgement the analysis makes, and the two PGN conventions for
/// marking a book mean opposite things.
let openingPlyCount (game: PgnGame) : int * OpeningSource = Opening.plyCount game
