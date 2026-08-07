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
                gamesLeft |> Array.map (fun gs -> gs.Board.MovesAndFenPlayed.Count) |> Array.min
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
  let fraction = float numberOfDevs / float numberOfGames
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
