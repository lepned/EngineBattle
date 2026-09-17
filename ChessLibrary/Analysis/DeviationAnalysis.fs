module ChessLibrary.DeviationAnalysis

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.EngineTypes
open ChessLibrary.ChessUtilities
open ChessLibrary.GameAnalysis

type DeviationInput = {Board: Chess.Board; PGN: PgnGame; Moves: string list }
type DeviationDescription = {Result:string; White:string; Black:string; Move: MoveAndFen; MoveHistory:string }
type DeviationPlayerSummary = {Player:string; Deviations:int; Points:float; OwnDeviationScore: float; GauntletDeviationScore: float; AdjustedScore: float; Ref:bool }

let res (moveDev: MoveDeviation) =
    let _,pgnGame = moveDev.PgnGamePair
    let score =
      match moveDev.Result, moveDev.DevRes with
      | "1-0", "1/2-1/2" -> -0.5
      | "1-0", "0-1" -> -1.0
      | "1-0", "1-0" -> 0.0
      | "1/2-1/2", "1-0" -> 0.5
      | "1/2-1/2", "0-1" -> -0.5
      | "1/2-1/2", "1/2-1/2" -> 0.0
      | "0-1", "1-0" -> 1.0
      | "0-1", "1/2-1/2" -> 0.5
      | "0-1", "0-1" -> 0.0
      | _ -> 0.0

    let isWhite = moveDev.PlayerToDeviate = pgnGame.GameMetaData.White
    if isWhite then score else -score

let getScore (res: string) iswhite =
  if iswhite then
    match res with
    | "1-0" -> 1.0
    | "1/2-1/2" -> 0.5
    | "0-1" -> 0.0
    | _ -> 0.0
  else
    match res with
    | "1-0" -> 0.0
    | "1/2-1/2" -> 0.5
    | "0-1" -> 1.0
    | _ -> 0.0

let createDeviationSummary (moveDeviations: MoveDeviation seq) (pgn: PgnGame seq) =
  [
    let distinctAll = moveDeviations|> Seq.distinctBy(fun e -> e.PlayerToDeviate)
    //create deviation summary here
    let criticalDevs = moveDeviations |> Seq.filter(fun e -> e.Result <> e.DevRes)
    let criticalDevCount player = criticalDevs |> Seq.filter(fun e -> e.PlayerToDeviate = player) |> Seq.length
    let ownDevsScore player = criticalDevs |> Seq.filter(fun e -> e.PlayerToDeviate = player) |> Seq.sumBy(fun e -> res e)

    for p in distinctAll do
       let devScore = ownDevsScore p.PlayerToDeviate
       let opponentsDeviated = moveDeviations |> Seq.filter(fun e -> e.PlayerToDeviate <> p.PlayerToDeviate && e.Opponent = p.PlayerToDeviate)
       let opponentDevScore = opponentsDeviated |> Seq.sumBy(fun e -> res e)
       let allOpponentsDevScore =
        if opponentDevScore = 0.0 then
          0.0
        else
          -opponentDevScore

       let adjusted = allOpponentsDevScore //totalDevScore + devScore
       let myGames =
          pgn
          |> Seq.filter(fun e -> e.GameMetaData.White = p.PlayerToDeviate || e.GameMetaData.Black = p.PlayerToDeviate)
       let totalScore =
          myGames
          |> Seq.sumBy(fun e ->
                let isWhite = e.GameMetaData.White = p.PlayerToDeviate
                getScore e.GameMetaData.Result isWhite)
       {
        Player = p.PlayerToDeviate
        Deviations = criticalDevCount p.PlayerToDeviate
        Points = totalScore
        OwnDeviationScore = devScore
        GauntletDeviationScore = allOpponentsDevScore
        AdjustedScore = totalScore + adjusted
        Ref = false }
  ]


let createDeviationDescription (input:DeviationInput) =
  let move = input.Board.MovesAndFenPlayed |> Seq.last
  let result = input.PGN.GameMetaData.Result
  let history = input.Board.GetMoveHistory()
  {Result=result; White = input.PGN.GameMetaData.White; Black = input.PGN.GameMetaData.Black; Move=move; MoveHistory = history; }

  //collect all moves in a pgn-game
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

type ReplayDataExtended =
  { Engine:string
    Move: string * string
    TimeLeftInMs: int64
    FirstGame: PgnGame
    SecondGame: PgnGame
    Fen1:string
  }

type ReferenceGameReplayExtended() =
    inherit Dictionary<uint64, ReplayDataExtended>()

    member this.TryGet (hash) =
        match this.TryGetValue(hash) with
        | true, data -> Some data
        | false, _ -> None

    member this.Seed (initialData: seq<uint64 * ReplayDataExtended>) =
        for (key, value) in initialData do
            this.Add(key, value)

    member this.PrettyPrint() =
      this |> Seq.map (fun kvp -> sprintf "Key: %A, Engine %s played Move: %A, TimeLeft: %d ms" kvp.Key kvp.Value.Engine kvp.Value.Move kvp.Value.TimeLeftInMs)
           |> String.concat "\n"

let findAllDeviationsForPlayers (pgnGames: PgnGame seq) (refPlayer: string option) (comparePlayers: string list option ) =
  let replayBoard = Chess.Board()
  let oppBoard = Chess.Board()
  let players =
    match comparePlayers with
    |Some p -> p
    |None -> pgnGames |> Seq.map(fun e -> e.GameMetaData.White) |> Seq.distinct |> Seq.toList

  let replayDicts =
      [ for eng in players -> eng, ReferenceGameReplayExtended()] |> Map.ofList

  let getReplayDictForPlayer name = replayDicts.[name]

  let prepareDeviationPlay () =
    let allGames = pgnGames |> Seq.toList
    let openingHashesExists = allGames |> List.exists(fun e -> e.GameMetaData.OpeningHash <> "")
    let gamesGroupedPerOpening =
      if openingHashesExists then
        allGames |> List.groupBy(fun e -> e.GameMetaData.OpeningHash)
      else
        allGames |> List.iter(fun game -> Hash.writeOpeningHashToPgnGame game)
        allGames |> List.groupBy (fun game -> game.GameMetaData.OpeningHash )
    let devs =
      [
        for (openingHash, gamesInOpening) in gamesGroupedPerOpening do
          for player in players do
            let dict = getReplayDictForPlayer player
            dict.Clear()
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
              let iAmWhite = game.GameMetaData.White = player
              replayBoard.ResetBoardState()
              oppBoard.ResetBoardState()
              if game.Fen <> "" then
                replayBoard.LoadFen game.Fen

              let mutable idx = 0
              let moves = movesFromPgn game
              let mutable cont = true

              for m in moves do
                if cont then
                  let whiteToMove = replayBoard.Position.STM = 0uy
                  let hash = replayBoard.DeviationHash()
                  let lastmove = m
                  let oldFen = replayBoard.FEN()
                  replayBoard.PlaySanMove lastmove
                  let newFen = replayBoard.FEN()
                  let moveCombo = lastmove, (replayBoard.MovesAndFenPlayed |> Seq.last).Move.LongSan
                  if iAmWhite && whiteToMove then
                    match dict.TryGet hash with
                    |None ->
                      let data : ReplayDataExtended =
                        {
                          Engine=player
                          Move = moveCombo
                          TimeLeftInMs = 0
                          FirstGame = game
                          SecondGame = PgnGame.Empty game.GameNumber
                          Fen1 = oldFen }
                      dict[hash] <- data
                    |Some replayData ->
                      let (sSan,_) = replayData.Move
                      if sSan <> lastmove then
                        oppBoard.LoadFen replayData.Fen1
                        oppBoard.PlaySanMove sSan
                        let oppFen = oppBoard.FEN()
                        let prevMoveCombo = sSan, (oppBoard.MovesAndFenPlayed |> Seq.last).Move.LongSan
                        cont <- false
                        let opp = game.GameMetaData.Black
                        let moveDeviation =
                          { Round = game.GameMetaData.Round
                            GameNr = game.GameNumber
                            MoveNr = idx
                            Color = "w"
                            PrevSanMove = prevMoveCombo
                            PlayerToDeviate = player
                            Opponent = opp
                            DevSanMove = moveCombo
                            Result = game.GameMetaData.Result
                            DevRes = replayData.FirstGame.GameMetaData.Result
                            PgnGamePair = replayData.FirstGame, game
                            PrevFen = oppFen
                            DevFen = newFen }
                        let data : ReplayDataExtended =
                          {
                            Engine=player
                            Move = moveCombo
                            TimeLeftInMs = 0
                            FirstGame = game
                            SecondGame = replayData.FirstGame
                            Fen1 = oldFen }
                        dict[hash] <- data
                        yield moveDeviation, game.GameNumber


                  elif not iAmWhite && not whiteToMove then
                    match dict.TryGet hash with
                    |None ->
                      let data : ReplayDataExtended =
                        {
                          Engine=player
                          Move = moveCombo
                          TimeLeftInMs = 0
                          FirstGame = game
                          SecondGame = PgnGame.Empty game.GameNumber
                          Fen1 = oldFen }
                      dict[hash] <- data
                    |Some replayData ->
                      let (sSan,_) = replayData.Move
                      if sSan <> lastmove then
                        oppBoard.LoadFen replayData.Fen1
                        oppBoard.PlaySanMove sSan
                        let oppFen = oppBoard.FEN()
                        let prevMoveCombo = sSan, (oppBoard.MovesAndFenPlayed |> Seq.last).Move.LongSan
                        cont <- false
                        let opp = game.GameMetaData.White
                        let moveDeviation =
                          { Round = game.GameMetaData.Round
                            GameNr = game.GameNumber
                            MoveNr = idx
                            Color = "b"
                            PrevSanMove = prevMoveCombo
                            PlayerToDeviate = player
                            Opponent = opp
                            DevSanMove = moveCombo  //replayData.Move
                            Result = game.GameMetaData.Result
                            DevRes = replayData.FirstGame.GameMetaData.Result
                            PgnGamePair = replayData.FirstGame, game
                            PrevFen = oppFen
                            DevFen = newFen}
                        yield moveDeviation, game.GameNumber
                        let data : ReplayDataExtended =
                          {
                            Engine=player
                            Move = moveCombo
                            TimeLeftInMs = 0
                            FirstGame = game
                            SecondGame = replayData.FirstGame
                            Fen1 = oldFen
                            }
                        dict[hash] <- data

                  if cont then
                    idx <- idx + 1

                    ] |> List.sortBy(fun (dev,nr) -> nr) |> List.map fst |> List.toSeq
    devs
  prepareDeviationPlay()

let findAllDeviationsForAllPlayers (pgnGames: PgnGame seq) =
    let res =
      try
          findAllDeviationsForPlayers (pgnGames |> Seq.toList) None None
      with
      | ex ->
          printfn "Exception in findAllDeviationsForPlayers: %s" ex.Message
          Seq.empty
    res

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
  let openingHashesExists = allGames |> List.exists(fun e -> e.GameMetaData.OpeningHash <> "")
  let gamesGroupedPerOpening =
    if openingHashesExists then
      allGames |> List.groupBy(fun e -> e.GameMetaData.OpeningHash)
    else
      allGames |> List.iter(fun game -> Hash.writeOpeningHashToPgnGame game)
      allGames |> List.groupBy (fun game -> game.GameMetaData.OpeningHash )

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

let returnListOfPositionsToCheck (fen:string) (list: string ResizeArray) =
    let movesToCheck = ResizeArray<PuzzleTypes.Position>()
    let mutable moves = ""
    list
    |> Seq.iteri(fun idx el ->
          moves <-
          if idx % 2 = 1 then
            let puzzle = $"position fen {fen} moves {moves}"
            let pos : PuzzleTypes.Position = {Command=puzzle; CorrectMove = list[idx]; MovePlayed = ""}
            movesToCheck.Add pos
          if idx = 0 then
            sprintf "%s" el
          else
            $"{moves} {el}" )
    movesToCheck

let analyzeDeviations (pgnGames: PgnGame seq) =
  let pgnGames = pgnGames |> Seq.toList
  let consoleResString, engineStats, crossTable, allResults = PGNCalculator.getEngineDataResults pgnGames
  let moveDevs = findAllDeviationsForAllPlayers pgnGames
  let devSummary = createDeviationSummary moveDevs pgnGames
  let numberOfGames = pgnGames.Length
  let numberOfDevs = moveDevs |> Seq.length
  // 0/0 is NaN, and this fraction is returned to callers that print it. An empty PGN, or a
  // filter that matched nothing, is a normal input - not a reason to hand back NaN.
  let fraction =
    if numberOfGames > 0 then float numberOfDevs / float numberOfGames else 0.0
  let sortedSummary = devSummary |> Seq.sortByDescending(fun e -> e.AdjustedScore)
  allResults, consoleResString, sortedSummary, engineStats, crossTable, fraction

let writeSummaryHeader (n:int) : string =
    sprintf "%-*s : %8s %7s %8s %8s %14s" n "# PLAYER" "Points" "Devs" "OwnDevs" "OppDevs" "ScoreAdjusted"

let writeSummaryForPlayer (p : DeviationPlayerSummary) (n:int) : string =
    let player = if p.Ref then p.Player + " *" else p.Player
    sprintf "%-*s : %8.1f %7d %8.1f %8.1f %14.1f" n player p.Points p.Deviations p.OwnDeviationScore p.GauntletDeviationScore p.AdjustedScore

let printDeviationsToConsole (summary: DeviationPlayerSummary seq) =
  let sb = System.Text.StringBuilder()
  let appendLine (txt:string) = sb.AppendLine txt |> ignore
  appendLine "\n```\n"
  appendLine "Game deviations (devs) summary (not validated):\n"
  //find longest player name and add 2 chars
  let longest =
    if Seq.isEmpty summary then 10
    else summary |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
  writeSummaryHeader longest |> appendLine
  for player in summary do
    writeSummaryForPlayer player longest |> appendLine
  let allPoints = summary |> Seq.sumBy(fun e -> e.Points)
  if summary |>Seq.exists(fun e -> e.Ref) then
    appendLine "\n* indicates reference player - the game result which is used in the comparison"
  appendLine "\nDevs: Number of critical deviations played by the player"
  appendLine "OwnDevs: The score of the player's own deviation from his perspective"
  appendLine "OppDevs: The score of the gauntlet players's own deviations (from his perspective) against the opponent"
  appendLine "ScoreAdjusted: Points + OppDevs"
  appendLine $"Total points: {allPoints}"
  appendLine "\n```\n"
  sb.ToString()


// ---------------------------------------------------------------------------
// Position-keyed deviation analysis.
//
// The older functions above replay a reference game and compare by ply index. That finds
// disagreements between two games of the same opening, but it cannot see an engine
// contradicting *itself*: in a colour-reversed pair the same engine is never on move in the
// same position twice, and in a gauntlet the repeats it does get are spread across games
// that the reference-replay never compares. Measured against four real PGNs,
// findAllDeviationsForAllPlayers returned 0 where 8, 4, 3 and 2 genuine self-deviations existed.
//
// Keying on the position instead removes that blind spot. The unit of a deviation is one
// position: if three games reach it and play two different moves, that is one deviation with
// two choices, not two deviations.
// ---------------------------------------------------------------------------

/// One move that was played in a given position, and by whom.
type PositionChoice =
  { Move: string          // UCI
    San: string
    Engines: string list  // engines that chose this move here
    GameNumbers: int list
    Results: string list }

/// A position that more than one game reached, where not everyone played the same move.
type PositionDeviation =
  { PositionHash: uint64
    /// FEN of the position before the move, so callers can show or link to it.
    Fen: string
    OpeningHash: string
    MoveNumber: int
    /// Side to move, taken from the FEN rather than ply parity - games can start from a FEN.
    Color: string
    /// Engines that played more than one distinct move here across different games.
    /// Empty means the disagreement is purely between different engines.
    SelfEngines: string list
    Choices: PositionChoice list }
  member this.IsSelfDeviation = not this.SelfEngines.IsEmpty
  member this.GameCount = this.Choices |> List.sumBy (fun c -> c.GameNumbers.Length)

/// Per-engine self-consistency, which is the headline number for a gauntlet.
type EngineSelfSummary =
  { Engine: string
    /// Positions where this engine contradicted an earlier choice of its own.
    SelfDeviations: int
    /// Positions this engine reached in more than one game - the denominator.
    RepeatedPositions: int }

/// How the opening was identified for one game, which decides how much its numbers are worth.
type OpeningSource =
  /// Per-move search data present: each ply is classified individually. The reliable case.
  | FromSearchData
  /// No search data, but a book marker gives the boundary - TCEC archives write "{ Book exit }".
  | FromBookMarker
  /// Neither. Opening moves cannot be separated from choices, so none are excluded and any
  /// self-deviation the caller sees may be an artefact of both sides following the same line.
  | Unknown

/// How trustworthy the opening detection was across a set of games.
type OpeningCoverage =
  { FromSearchData: int
    FromBookMarker: int
    Unknown: int }
  member this.Total = this.FromSearchData + this.FromBookMarker + this.Unknown
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
      OpeningHash: string
      MoveNumber: int
      Color: string
      Fen: string }

  /// Whether a ply was actually searched, judged by the per-move data EngineBattle records.
  ///
  /// A searched move carries move time, time left and the engine's eval; a book move carries
  /// only "book, mb=...". Testing for the search data is more reliable than looking for the
  /// word "book": it needs no assumption that the book is a contiguous prefix, and it handles
  /// the first ply, whose comment is the pre-game tournament header rather than the book
  /// marker. Anything we cannot attribute to a search is treated as not-a-choice and left out.
  let private searchMarkers = [| "mt="; "tl="; "wv=" |]

  let wasSearched (comment: string) =
    comment <> null &&
    searchMarkers |> Array.exists (fun m -> comment.Contains(m, System.StringComparison.Ordinal))

  let private mentionsBook (comment: string) =
    comment <> null && comment.Contains("book", System.StringComparison.OrdinalIgnoreCase)

  /// Which plies of a game count as a choice the engine made, rather than an opening move.
  let choicePlies (game: PgnGame) =
    let plies = game.Mainline |> Seq.toArray
    let searched = plies |> Array.map (fun p -> wasSearched p.Comment)

    if Array.exists id searched then
      searched, FromSearchData
    else
      let lastBook =
        plies
        |> Array.mapi (fun i p -> i, p)
        |> Array.filter (fun (_, p) -> mentionsBook p.Comment)
        |> Array.tryLast
        |> Option.map fst
      match lastBook with
      | Some i -> plies |> Array.mapi (fun j _ -> j > i), FromBookMarker
      | None -> plies |> Array.map (fun _ -> true), Unknown


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
    for game in games do
      let board = Chess.Board()
      if game.Fen <> "" then board.LoadFen game.Fen
      let isChoice, source = choicePlies game
      match source with
      | FromSearchData -> fromSearch <- fromSearch + 1
      | FromBookMarker -> fromMarker <- fromMarker + 1
      | Unknown -> unknown <- unknown + 1
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
        let hashBefore = board.DeviationHash()
        board.PlaySanMove san
        let uci =
          if board.UciMovesPlayed.Count > 0 then board.UciMovesPlayed.[board.UciMovesPlayed.Count - 1]
          else ""
        let entry =
          { Engine = engine
            Uci = uci
            San = san
            GameNumber = game.GameNumber
            Result = game.GameMetaData.Result
            OpeningHash = game.GameMetaData.OpeningHash
            MoveNumber = moveNumber
            Color = color
            Fen = fenBefore }
        if searched then
          match table.TryGetValue hashBefore with
          | true, list -> list.Add entry
          | _ ->
            let list = ResizeArray<Entry>()
            list.Add entry
            table.[hashBefore] <- list
    table, ({ FromSearchData = fromSearch; FromBookMarker = fromMarker; Unknown = unknown } : OpeningCoverage)

let private deviationsFromScan (table: Dictionary<uint64, ResizeArray<PositionScan.Entry>>) : PositionDeviation list =
  [ for kv in table do
      let entries = kv.Value
      let distinctGames = entries |> Seq.map (fun e -> e.GameNumber) |> Seq.distinct |> Seq.length
      let distinctMoves = entries |> Seq.map (fun e -> e.Uci) |> Seq.distinct |> Seq.length
      // A position repeated inside a single game is not a deviation, so require two games.
      if distinctGames > 1 && distinctMoves > 1 then
        let selfEngines =
          entries
          |> Seq.groupBy (fun e -> e.Engine)
          |> Seq.filter (fun (_, es) ->
               (es |> Seq.map (fun e -> e.Uci) |> Seq.distinct |> Seq.length) > 1 &&
               (es |> Seq.map (fun e -> e.GameNumber) |> Seq.distinct |> Seq.length) > 1)
          |> Seq.map fst
          |> Seq.sort
          |> Seq.toList
        let choices =
          entries
          |> Seq.groupBy (fun e -> e.Uci)
          |> Seq.map (fun (uci, es) ->
               let es = es |> Seq.toList
               { Move = uci
                 San = (es |> List.head).San
                 Engines = es |> List.map (fun e -> e.Engine) |> List.distinct |> List.sort
                 GameNumbers = es |> List.map (fun e -> e.GameNumber) |> List.distinct |> List.sort
                 Results = es |> List.map (fun e -> e.Result) |> List.distinct })
          |> Seq.sortByDescending (fun c -> c.GameNumbers.Length)
          |> Seq.toList
        let first = entries.[0]
        yield
          { PositionHash = kv.Key
            Fen = first.Fen
            OpeningHash = first.OpeningHash
            MoveNumber = first.MoveNumber
            Color = first.Color
            SelfEngines = selfEngines
            Choices = choices } ]
  |> List.sortBy (fun d -> d.MoveNumber)

let private selfSummaryFromScan (table: Dictionary<uint64, ResizeArray<PositionScan.Entry>>) : EngineSelfSummary list =
  let repeated = Dictionary<string, int>()
  let deviated = Dictionary<string, int>()
  let bump (d: Dictionary<string, int>) key =
    match d.TryGetValue key with
    | true, v -> d.[key] <- v + 1
    | _ -> d.[key] <- 1

  for kv in table do
    for (engine, es) in kv.Value |> Seq.groupBy (fun e -> e.Engine) do
      let games = es |> Seq.map (fun e -> e.GameNumber) |> Seq.distinct |> Seq.length
      if games > 1 then
        bump repeated engine
        if (es |> Seq.map (fun e -> e.Uci) |> Seq.distinct |> Seq.length) > 1 then
          bump deviated engine

  [ for kv in repeated do
      yield
        { Engine = kv.Key
          SelfDeviations = (match deviated.TryGetValue kv.Key with | true, v -> v | _ -> 0)
          RepeatedPositions = kv.Value } ]
  |> List.sortByDescending (fun s -> s.SelfDeviations)


/// Both views from a single replay of the games. Scanning a large PGN is the expensive part
/// -- 1811 games take ~3.5s -- so callers that want deviations and the per-engine summary
/// should ask for them together rather than paying for two scans.
let analyzePositionDeviations (pgnGames: PgnGame seq) =
  let table, coverage = PositionScan.scan (pgnGames |> Seq.toList)
  deviationsFromScan table, selfSummaryFromScan table, coverage

/// Every position reached by more than one game where the played move was not unanimous.
let findPositionDeviations (pgnGames: PgnGame seq) : PositionDeviation list =
  PositionScan.scan (pgnGames |> Seq.toList) |> fst |> deviationsFromScan

/// Per-engine self-consistency: how often an engine contradicted an earlier choice of its own.
let summarizeSelfDeviations (pgnGames: PgnGame seq) : EngineSelfSummary list =
  PositionScan.scan (pgnGames |> Seq.toList) |> fst |> selfSummaryFromScan
