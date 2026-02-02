module ChessLibrary.GameReplay

open System
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess

/// Data captured for replay deviation prevention
type ReplayData = {
    Engine: string
    Move: string
    TimeLeftInMs: int64
    Hash: string
}

/// Dictionary storing replay data keyed by position hash
type ReferenceGameReplay() =
    inherit Dictionary<uint64, ReplayData>()

    member this.TryGet (hash) =
        match this.TryGetValue(hash) with
        | true, data -> Some data
        | false, _ -> None

    member this.Seed (initialData: seq<uint64 * ReplayData>) =
        for (key, value) in initialData do
            this.Add(key, value)

    member this.PrettyPrint() =
        this |> Seq.map (fun kvp -> sprintf "Key: %A, Engine %s played Move: %s, TimeLeft: %d ms" kvp.Key kvp.Value.Engine kvp.Value.Move kvp.Value.TimeLeftInMs)
             |> String.concat "\n"

/// Tracks game replay information for live games
type GameReplay =
    { WhitePlayer: string
      BlackPlayer: string
      LongSanMoves: ResizeArray<string>
      PGNMetaData: GameMetadata
    }
    with
        static member InitGame = {WhitePlayer = ""; BlackPlayer = ""; LongSanMoves = ResizeArray<string>(); PGNMetaData = GameMetadata.Empty}
        member this.HasMoves = this.LongSanMoves.Count > 0
        member this.AddPlayers white black = {this with WhitePlayer = white; BlackPlayer = black }
        member this.AddMove (move:string) = this.LongSanMoves.Add move
        member this.copyGameReplay white black = {WhitePlayer = white; BlackPlayer = black; LongSanMoves = ResizeArray<string>(this.LongSanMoves); PGNMetaData = this.PGNMetaData}

/// Prepares game replay data for deviation prevention
/// When clearDictsOnNewOpening is true, all replay dictionaries are cleared if the current opening
/// hasn't been played before (used by gauntlet mode when switching openings)
let prepareGameReplay
    (pairing : Pairing)
    (replayDicts : Map<string, ReferenceGameReplay>)
    (replayList: ResizeArray<GameReplay>)
    (referencGamesPlayed: PgnGame array)
    (gamesAlreadyPlayed: PgnGame array)
    (isChess960: bool)
    (clearDictsOnNewOpening: bool)
    =
    let getReplayDictForPlayer (name:string) = replayDicts.[name]
    let nextGame = pairing
    let replayDictWhite = getReplayDictForPlayer pairing.White.Name
    let replayDictBlack = getReplayDictForPlayer pairing.Black.Name

    // Gauntlet mode: clear all replay dicts when switching to a new opening
    if clearDictsOnNewOpening then
        let openingPlayedBefore = replayList |> Seq.exists(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash)
        if not openingPlayedBefore then
            for dict in replayDicts do
                dict.Value.Clear()

    let lastRelevantLiveGame =
        replayList
        |> Seq.tryFind(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))

    let latestLiveGames =
        replayList
        |> Seq.filter(fun e -> e.PGNMetaData.OpeningHash = pairing.OpeningHash && (e.WhitePlayer = pairing.White.Name || e.BlackPlayer = pairing.Black.Name ))

    let allGames = Array.concat [referencGamesPlayed; gamesAlreadyPlayed]
    let refGamesPlayed = allGames |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash)

    match refGamesPlayed |> Seq.tryLast with
    |Some _ ->
        let lastRelevantGame =
            refGamesPlayed
            |> Seq.tryFind(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))

        let lastRelevantGames =
            refGamesPlayed
            |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))

        let previousGames =
            refGamesPlayed
            |> Seq.filter(fun e -> e.GameMetaData.OpeningHash = pairing.OpeningHash && (e.GameMetaData.White = pairing.White.Name || e.GameMetaData.Black = pairing.Black.Name ))

        let replayBoard = Board()
        replayBoard.IsFRC <- isChess960
        let tryInitBoard () =
            if pairing.Opening.Fen <> "" then
                replayBoard.LoadFen pairing.Opening.Fen
        for game in lastRelevantGames do
            printfn "Relevant saved game found %s, %s for pairing: %s, %s" game.GameMetaData.White game.GameMetaData.Black pairing.White.Name pairing.Black.Name
            let isWhite = game.GameMetaData.White = pairing.White.Name
            let rematch = game.GameMetaData.White = pairing.White.Name && game.GameMetaData.Black = pairing.Black.Name
            if rematch then
                printfn "Rematch found for %s, %s - so games should be identical" game.GameMetaData.White game.GameMetaData.Black
            replayBoard.ResetBoardState()
            tryInitBoard()
            let mutable idx = 0
            for m in game.Mainline do
                let hash = replayBoard.DeviationHash()
                replayBoard.PlaySimpleShortSan m.San
                if m.Color = "w" then
                    if replayBoard.LongSANMovesPlayed.Count > idx then
                        let lastmove = replayBoard.LongSANMovesPlayed[idx]
                        let data : ReplayData = {Engine=game.GameMetaData.White; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}
                        if isWhite then
                            replayDictWhite[hash] <- data
                        idx <- idx + 1
                elif m.Color = "b" then
                    if replayBoard.LongSANMovesPlayed.Count > idx then
                        let lastmove = replayBoard.LongSANMovesPlayed[idx]
                        let data : ReplayData = {Engine=game.GameMetaData.Black; Move = lastmove; TimeLeftInMs = 0; Hash = game.GameMetaData.OpeningHash}
                        if not isWhite then
                            replayDictBlack[hash] <- data
                        idx <- idx + 1

        let moves =
            match lastRelevantGame with
            |Some game ->
                if game.GameMetaData.White = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
            |None -> 0

        match lastRelevantLiveGame with
        |Some game ->
            let data = game.PGNMetaData
            let sumGames = (latestLiveGames |> Seq.length) + (lastRelevantGames |> Seq.length)
            let log = sprintf "First Live game: %s vs %s in round %s, number of games (live and saved) are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves
            printfn "%s" log
        |None ->

        match lastRelevantGame with
        |Some game ->
            let sumGames = lastRelevantGames |> Seq.length
            let log = sprintf "First saved game (no live game yet): %s vs %s in round %s, number of games are: %d, tot moves: %d" game.GameMetaData.White game.GameMetaData.Black game.GameMetaData.Round sumGames moves
            printfn "%s" log
        |None ->
            let whiteMoves = replayDictWhite |> Seq.length
            let blackMoves = replayDictBlack |> Seq.length
            let log = sprintf "No relevant saved game found for player (%s, %s) for game number %d, tot moves: %d" pairing.White.Name pairing.Black.Name nextGame.GameNr moves
            printfn "%s whiteDict: %d, BlackDict: %d" log whiteMoves blackMoves
    |_ ->
        match lastRelevantLiveGame with
        |Some game ->
            let moves = if game.WhitePlayer = pairing.White.Name then replayDictWhite |> Seq.length else replayDictBlack |> Seq.length
            let data = game.PGNMetaData
            let sumGames = (latestLiveGames |> Seq.length)
            let log = sprintf "First Live game: %s vs %s in round %s, number of games are: %d, tot moves: %d" data.White data.Black data.Round sumGames moves
            printfn "%s" log
        |_ -> ()
