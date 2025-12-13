module ChessLibrary.MoveDeviationFinder

open System
open System.Collections.Generic
open TypesDef
open TypesDef.PGNTypes
open ChessLibrary.Chess
open Parser
open Utilities

[<StructuralEquality; StructuralComparison>]
type NodeKey =
  { Hash: uint64
    FenKey: string }

type MoveChoice =
  { San: string
    Lan: string }

type OutcomeFilter =
  | AllGames
  | DecisiveOnly

type BaselinePolicy =
  | Majority of OutcomeFilter
  | SpecificEngine of engineName: string * OutcomeFilter
  | BestByOutcome of OutcomeFilter
  | WinnerMove

type OpeningCutoff =
  | FixedPly of int
  | WhileBookComment
  | WhileCommentEmpty

type EventFilter =
  | IncludeAll
  | OnlyDecisive

type SelfDeviationScope =
  | AcrossGamesOnly
  | IncludeSameGameRepetitions

type DeviationSignificance =
  | AllDeviations
  | OnlyMissedWin

type AnalysisOptions =
  { TargetEngine: string option
    Baseline: BaselinePolicy
    OpeningCutoff: OpeningCutoff
    EventFilter: EventFilter
    SelfDeviationScope: SelfDeviationScope
    DeviationSignificance: DeviationSignificance
    KeepOnlyLastDeviationPerGame: bool
    MissedWinMinDelta: float
    MissedWinMinBestScore: float
    MissedWinUseDecisiveOnly: bool
    MissedWinRequireDecisiveOpening: bool
    MissedWinKeepOnlyLastPerGame: bool
    MinSamplesForBaseline: int }
  with
    static member Default =
      { TargetEngine = None
        Baseline = Majority AllGames
        OpeningCutoff = WhileBookComment
        EventFilter = IncludeAll
        SelfDeviationScope = AcrossGamesOnly
        DeviationSignificance = AllDeviations
        KeepOnlyLastDeviationPerGame = false
        MissedWinMinDelta = 0.20
        MissedWinMinBestScore = 0.50
        MissedWinUseDecisiveOnly = true
        MissedWinRequireDecisiveOpening = true
        MissedWinKeepOnlyLastPerGame = false
        MinSamplesForBaseline = 1 }

type EdgeAgg =
  { Move: MoveChoice
    mutable SamplesAll: int
    mutable SamplesDecisive: int
    mutable PointsAll: float
    mutable PointsDecisive: float }

type MoveOccurrence =
  { GameNumber: int
    Round: string
    Opponent: string
    Result: string
    Ply: int
    MoveNumber: int }

type NodeAgg =
  { Node: NodeKey
    mutable FenExample: string
    EdgesByLan: Dictionary<string, EdgeAgg>
    EngineCountsAll: Dictionary<string, Dictionary<string, int>>
    EngineCountsDecisive: Dictionary<string, Dictionary<string, int>>
    EngineMoveExamplesAll: Dictionary<string, Dictionary<string, ResizeArray<MoveOccurrence>>> }

type OpeningAgg =
  { OpeningKey: string
    mutable OpeningFenKey: string
    Nodes: Dictionary<NodeKey, NodeAgg> }

type DeviationEvent =
  { OpeningKey: string
    Node: NodeKey
    Ply: int
    MoveNumber: int
    WhiteToMove: bool
    EngineToMove: string
    Opponent: string
    GameNumber: int
    Round: string
    Result: string
    FenBefore: string
    FenAfter: string
    Played: MoveChoice
    Baseline: MoveChoice
    BaselineReason: string }

type EngineSelfDeviation =
  { OpeningKey: string
    Engine: string
    Node: NodeKey
    Moves: MoveChoice list
    TotalSamples: int
    ExamplesByMove: Map<string, MoveOccurrence list> }

type ReportStats =
  { Games: int
    Openings: int
    Nodes: int
    Events: int
    SelfDeviations: int }

type DeviationReport =
  { Stats: ReportStats
    OpeningInfo: Map<string, string>
    NodeStats: Map<string, Map<NodeKey, NodeStat>>
    Events: DeviationEvent list
    SelfDeviations: EngineSelfDeviation list }

and NodeMoveStat =
  { Move: MoveChoice
    SamplesAll: int
    SamplesDecisive: int
    PointsAll: float
    DecisiveWins: float }

and NodeStat =
  { TotalGames: int
    Moves: NodeMoveStat list }

type ReportFormatOptions =
  { MaxOpenings: int
    MaxNodesPerOpening: int
    MaxEventsPerOpening: int
    MaxSelfDevs: int
    ShowOpeningKey: bool
    ShowFen: bool
    FenMaxLen: int }
  with
    static member Default =
      { MaxOpenings = 20
        MaxNodesPerOpening = 20
        MaxEventsPerOpening = 40
        MaxSelfDevs = 50
        ShowOpeningKey = false
        ShowFen = true
        FenMaxLen = 90 }

let private isDecisive (result: string) =
  match result.Trim() with
  | "1-0"
  | "0-1" -> true
  | _ -> false

let private pointsFor (result: string) (isWhite: bool) =
  match result.Trim(), isWhite with
  | "1-0", true -> 1.0
  | "1-0", false -> 0.0
  | "0-1", true -> 0.0
  | "0-1", false -> 1.0
  | "1/2-1/2", _ -> 0.5
  | _, _ -> 0.0

let private normalizeFenForKey (fen: string) =
  let parts = fen.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
  if parts.Length >= 4 then
    String.concat " " parts[0..3]
  else
    fen.Trim()

let private flattenPliesWithComments (game: PgnGame) =
  seq {
    for m in game.Moves do
      if String.IsNullOrWhiteSpace m.WhiteSan |> not then
        yield m.WhiteSan.Trim(), (if isNull m.WhiteComment then "" else m.WhiteComment)
      if String.IsNullOrWhiteSpace m.BlackSan |> not then
        yield m.BlackSan.Trim(), (if isNull m.BlackComment then "" else m.BlackComment)
  }

let private computeCutoffPly (cutoff: OpeningCutoff) (plies: (string * string) list) =
  match cutoff with
  | FixedPly n -> max 0 n
  | WhileBookComment ->
      plies
      |> Seq.takeWhile (fun (_, comment) -> comment.IndexOf("book", StringComparison.OrdinalIgnoreCase) >= 0)
      |> Seq.length
  | WhileCommentEmpty ->
      plies
      |> Seq.takeWhile (fun (_, comment) -> String.IsNullOrWhiteSpace comment)
      |> Seq.length

let private ensureOpeningHash (game: PgnGame) =
  if String.IsNullOrWhiteSpace game.GameMetaData.OpeningHash then
    Hash.writeOpeningHashToPgnGame game
  game.GameMetaData.OpeningHash

let private getOrCreateNodeAgg (opening: OpeningAgg) (nodeKey: NodeKey) (fenExample: string) =
  match opening.Nodes.TryGetValue nodeKey with
  | true, nodeAgg ->
      if String.IsNullOrWhiteSpace nodeAgg.FenExample then nodeAgg.FenExample <- fenExample
      nodeAgg
  | false, _ ->
      let nodeAgg =
        { Node = nodeKey
          FenExample = fenExample
          EdgesByLan = Dictionary<string, EdgeAgg>()
          EngineCountsAll = Dictionary<string, Dictionary<string, int>>()
          EngineCountsDecisive = Dictionary<string, Dictionary<string, int>>()
          EngineMoveExamplesAll = Dictionary<string, Dictionary<string, ResizeArray<MoveOccurrence>>>() }
      opening.Nodes[nodeKey] <- nodeAgg
      nodeAgg

let private bumpEngineCount (counts: Dictionary<string, Dictionary<string, int>>) (engine: string) (lan: string) =
  let moveMap =
    match counts.TryGetValue engine with
    | true, m -> m
    | false, _ ->
        let m = Dictionary<string, int>()
        counts[engine] <- m
        m
  moveMap[lan] <- (match moveMap.TryGetValue lan with | true, c -> c + 1 | _ -> 1)

let private bumpEngineExample (examples: Dictionary<string, Dictionary<string, ResizeArray<MoveOccurrence>>>) (engine: string) (lan: string) (occ: MoveOccurrence) =
  let moveMap =
    match examples.TryGetValue engine with
    | true, m -> m
    | false, _ ->
        let m = Dictionary<string, ResizeArray<MoveOccurrence>>()
        examples[engine] <- m
        m
  let list =
    match moveMap.TryGetValue lan with
    | true, l -> l
    | false, _ ->
        let l = ResizeArray<MoveOccurrence>()
        moveMap[lan] <- l
        l
  if list.Count < 3 then list.Add occ

let private buildAggregates (games: PgnGame seq) (options: AnalysisOptions) =
  let byOpening = Dictionary<string, OpeningAgg>()

  let getOrCreateOpening openingKey =
    match byOpening.TryGetValue openingKey with
    | true, o -> o
    | false, _ ->
        let o = { OpeningKey = openingKey; OpeningFenKey = ""; Nodes = Dictionary<NodeKey, NodeAgg>() }
        byOpening[openingKey] <- o
        o

  for game in games do
    let openingKey = ensureOpeningHash game
    let openingAgg = getOrCreateOpening openingKey

    let board = Board()
    if String.IsNullOrWhiteSpace game.Fen |> not then board.LoadFen(game.Fen)

    let plies = flattenPliesWithComments game |> Seq.toList
    let cutoffPly = computeCutoffPly options.OpeningCutoff plies

    let result = game.GameMetaData.Result
    let decisive = isDecisive result

    // For "across games" comparisons, repeated positions within the same game (repetition, transpositions)
    // should not contribute multiple observations from that single game.
    let seenNodeInGame = HashSet<NodeKey>()
    // Prevent "self-deviations" from being explained by repeated positions within the same game
    // unless the user explicitly opts in to include that behavior.
    let seenFirstMoveInGame = Dictionary<string * NodeKey, string>()

    let mutable ply = 0
    for (san, comment) in plies do
      let fenBefore = board.FEN()
      if ply = cutoffPly && String.IsNullOrWhiteSpace openingAgg.OpeningFenKey then
        openingAgg.OpeningFenKey <- normalizeFenForKey fenBefore
      let nodeKey = { Hash = board.PositionHash(); FenKey = normalizeFenForKey fenBefore }

      let whiteToMove = board.Position.STM = 0uy
      let engineToMove = if whiteToMove then game.GameMetaData.White else game.GameMetaData.Black
      let opponent = if whiteToMove then game.GameMetaData.Black else game.GameMetaData.White
      let points = pointsFor result whiteToMove

      let beforeCount = board.LongSANMovesPlayed.Count
      let absPly = board.PlyCount
      let moveNumber = board.MoveNumber()
      board.PlaySimpleShortSanWithComments san comment
      let afterCount = board.LongSANMovesPlayed.Count
      if afterCount = beforeCount then
        ply <- plies.Length
      else
        if ply >= cutoffPly then
          let lan = board.LongSANMovesPlayed[afterCount - 1]
          let moveChoice = { San = san; Lan = lan }
          let nodeAgg = getOrCreateNodeAgg openingAgg nodeKey fenBefore
          let occ =
            { GameNumber = game.GameNumber
              Round = game.GameMetaData.Round
              Opponent = opponent
              Result = result
              Ply = absPly
              MoveNumber = moveNumber }

          let isFirstTimeThisGame = seenNodeInGame.Add nodeKey
          let perGameKey = (engineToMove, nodeKey)
          let shouldCountForSelfDeviation =
            match options.SelfDeviationScope with
            | IncludeSameGameRepetitions -> true
            | AcrossGamesOnly ->
                match seenFirstMoveInGame.TryGetValue perGameKey with
                | true, firstLan ->
                    // If we see the same node again in the same game, don't count it again (even if it differs).
                    // The goal is to find cross-game deviations.
                    false
                | false, _ ->
                    seenFirstMoveInGame[perGameKey] <- lan
                    true

          match nodeAgg.EdgesByLan.TryGetValue lan with
          | true, edge ->
              if isFirstTimeThisGame then
                edge.SamplesAll <- edge.SamplesAll + 1
                edge.PointsAll <- edge.PointsAll + points
                if decisive then
                  edge.SamplesDecisive <- edge.SamplesDecisive + 1
                  edge.PointsDecisive <- edge.PointsDecisive + points
          | false, _ ->
              nodeAgg.EdgesByLan[lan] <-
                { Move = moveChoice
                  SamplesAll = (if isFirstTimeThisGame then 1 else 0)
                  SamplesDecisive = (if isFirstTimeThisGame && decisive then 1 else 0)
                  PointsAll = (if isFirstTimeThisGame then points else 0.0)
                  PointsDecisive = (if isFirstTimeThisGame && decisive then points else 0.0) }

          if isFirstTimeThisGame && shouldCountForSelfDeviation then
            bumpEngineCount nodeAgg.EngineCountsAll engineToMove lan
            if decisive then bumpEngineCount nodeAgg.EngineCountsDecisive engineToMove lan
            bumpEngineExample nodeAgg.EngineMoveExamplesAll engineToMove lan occ

        ply <- ply + 1

  byOpening.Values |> Seq.toList

let private chooseByCounts (edges: (string * int) list) =
  edges
  |> List.sortBy (fun (lan, c) -> (-c, lan))
  |> List.tryHead
  |> Option.map fst

let private chooseBaseline (policy: BaselinePolicy) (nodeAgg: NodeAgg) (minSamples: int) =
  let pickMove (lan: string) =
    match nodeAgg.EdgesByLan.TryGetValue lan with
    | true, e -> Some e.Move
    | _ -> None

  let availableAll =
    nodeAgg.EdgesByLan.Values
    |> Seq.filter (fun e -> e.SamplesAll >= minSamples)
    |> Seq.toList

  let availableDecisive =
    nodeAgg.EdgesByLan.Values
    |> Seq.filter (fun e -> e.SamplesDecisive >= minSamples)
    |> Seq.toList

  match policy with
  | Majority filter ->
      let candidates =
        match filter with
        | AllGames -> availableAll |> List.map (fun e -> e.Move.Lan, e.SamplesAll)
        | DecisiveOnly -> availableDecisive |> List.map (fun e -> e.Move.Lan, e.SamplesDecisive)
      match chooseByCounts candidates with
      | Some lan ->
          pickMove lan |> Option.map (fun mv -> mv, $"Majority({filter})")
      | None -> None

  | BestByOutcome filter ->
      let candidates =
        match filter with
        | AllGames ->
            availableAll
            |> List.map (fun e -> e, (e.PointsAll / float e.SamplesAll), e.SamplesAll)
        | DecisiveOnly ->
            availableDecisive
            |> List.map (fun e -> e, (e.PointsDecisive / float e.SamplesDecisive), e.SamplesDecisive)

      candidates
      |> List.sortBy (fun (e, score, samples) -> (-score, -samples, e.Move.Lan))
      |> List.tryHead
      |> Option.map (fun (e, _, _) -> e.Move, $"BestByOutcome({filter})")

  | SpecificEngine (engineName, filter) ->
      let counts =
        match filter with
        | AllGames ->
            match nodeAgg.EngineCountsAll.TryGetValue engineName with
            | true, m -> m |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList
            | _ -> []
        | DecisiveOnly ->
            match nodeAgg.EngineCountsDecisive.TryGetValue engineName with
            | true, m -> m |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList
            | _ -> []

      let countsFiltered = counts |> List.filter (fun (_, c) -> c >= minSamples)
      match chooseByCounts countsFiltered with
      | Some lan -> pickMove lan |> Option.map (fun mv -> mv, $"SpecificEngine({engineName},{filter})")
      | None -> None

  | WinnerMove ->
      // Decisive-only "winner move": pick the move with the most wins (from player-to-move perspective).
      // Uses PointsDecisive where each win contributes +1 and each loss contributes +0.
      availableDecisive
      |> List.map (fun e -> e, e.PointsDecisive, e.SamplesDecisive)
      |> List.sortBy (fun (e, wins, samples) -> (-wins, -samples, e.Move.Lan))
      |> List.tryHead
      |> Option.map (fun (e, _, _) -> e.Move, "WinnerMove(DecisiveOnly)")

let private shouldEmitEvent (options: AnalysisOptions) (result: string) =
  match options.EventFilter with
  | IncludeAll -> true
  | OnlyDecisive -> isDecisive result

let private collectSelfDeviations (openingAggs: OpeningAgg list) (options: AnalysisOptions) =
  let results = ResizeArray<EngineSelfDeviation>()
  for opening in openingAggs do
    for node in opening.Nodes.Values do
      let engines =
        node.EngineCountsAll.Keys
        |> Seq.filter (fun e -> options.TargetEngine |> Option.map ((=) e) |> Option.defaultValue true)
        |> Seq.toList
      for engine in engines do
        match node.EngineCountsAll.TryGetValue engine with
        | true, moveCounts ->
            let distinctMoves = moveCounts.Keys |> Seq.toList
            if distinctMoves.Length > 1 then
              let moves =
                distinctMoves
                |> List.choose (fun lan -> node.EdgesByLan.TryGetValue lan |> function | true, e -> Some e.Move | _ -> None)
              let totalSamples = moveCounts.Values |> Seq.sum
              let examplesByMove =
                match node.EngineMoveExamplesAll.TryGetValue engine with
                | true, examples ->
                    examples
                    |> Seq.map (fun kv -> kv.Key, kv.Value |> Seq.toList)
                    |> Map.ofSeq
                | _ -> Map.empty
              results.Add
                { OpeningKey = opening.OpeningKey
                  Engine = engine
                  Node = node.Node
                  Moves = moves
                  TotalSamples = totalSamples
                  ExamplesByMove = examplesByMove }
        | _ -> ()
  results |> Seq.toList

let analyzePgnGames (games: PgnGame seq) (options: AnalysisOptions) : DeviationReport =
  let games = games |> Seq.toList
  let openingAggs = buildAggregates games options
  let openingInfo = openingAggs |> Seq.map (fun o -> o.OpeningKey, o.OpeningFenKey) |> Map.ofSeq
  let openingHasDecisive =
    games
    |> Seq.groupBy ensureOpeningHash
    |> Seq.map (fun (k, gs) -> k, gs |> Seq.exists (fun g -> isDecisive g.GameMetaData.Result))
    |> Map.ofSeq
  let nodeStats =
    openingAggs
    |> Seq.map (fun opening ->
        let perNode =
          opening.Nodes.Values
          |> Seq.map (fun nodeAgg ->
              let moves =
                nodeAgg.EdgesByLan.Values
                |> Seq.map (fun e ->
                    { Move = e.Move
                      SamplesAll = e.SamplesAll
                      SamplesDecisive = e.SamplesDecisive
                      PointsAll = e.PointsAll
                      DecisiveWins = e.PointsDecisive })
                |> Seq.sortBy (fun ms -> (-ms.SamplesAll, ms.Move.Lan))
                |> Seq.toList
              let totalGames = moves |> List.sumBy (fun ms -> ms.SamplesAll)
              nodeAgg.Node, { TotalGames = totalGames; Moves = moves })
          |> Map.ofSeq
        opening.OpeningKey, perNode)
    |> Map.ofSeq

  let events = ResizeArray<DeviationEvent>()

  let tryGetNodeStat (openingKey: string) (nodeKey: NodeKey) =
    nodeStats
    |> Map.tryFind openingKey
    |> Option.bind (fun m -> Map.tryFind nodeKey m)

  let moveScore (useDecisiveOnly: bool) (ms: NodeMoveStat) =
    if useDecisiveOnly && ms.SamplesDecisive > 0 then
      ms.DecisiveWins / float ms.SamplesDecisive
    elif ms.SamplesAll > 0 then
      ms.PointsAll / float ms.SamplesAll
    else
      0.0

  let isMissedWin (ev: DeviationEvent) =
    let decisiveInOpening = openingHasDecisive |> Map.tryFind ev.OpeningKey |> Option.defaultValue false
    if options.MissedWinRequireDecisiveOpening && not decisiveInOpening then
      false
    else
    match tryGetNodeStat ev.OpeningKey ev.Node with
    | None -> false
    | Some ns ->
        let playedStat = ns.Moves |> List.tryFind (fun ms -> ms.Move.Lan = ev.Played.Lan)
        let bestStat =
          ns.Moves
          |> List.map (fun ms -> ms, moveScore options.MissedWinUseDecisiveOnly ms)
          |> List.sortBy (fun (ms, score) -> (-score, -ms.SamplesAll, ms.Move.Lan))
          |> List.tryHead
        match playedStat, bestStat with
        | Some ps, Some (bestMs, bestScore) ->
            let playedScore = moveScore options.MissedWinUseDecisiveOnly ps
            // If the mover already won, don't call it a missed win.
            let moverPoints = pointsFor ev.Result ev.WhiteToMove
            moverPoints < 1.0 &&
            bestScore >= options.MissedWinMinBestScore &&
            (bestScore - playedScore) >= options.MissedWinMinDelta &&
            bestMs.Move.Lan <> ps.Move.Lan
        | _ -> false

  // Second pass: replay games and emit events vs chosen baseline per node.
  for game in games do
    let openingKey = ensureOpeningHash game
    let openingAggOpt = openingAggs |> List.tryFind (fun o -> o.OpeningKey = openingKey)
    match openingAggOpt with
    | None -> ()
    | Some openingAgg ->
        let board = Board()
        if String.IsNullOrWhiteSpace game.Fen |> not then board.LoadFen(game.Fen)

        let plies = flattenPliesWithComments game |> Seq.toList
        let cutoffPly = computeCutoffPly options.OpeningCutoff plies

        let result = game.GameMetaData.Result
        if shouldEmitEvent options result then
          let seenNodeInGame = HashSet<NodeKey>()
          let mutable ply = 0
          for (san, comment) in plies do
            let fenBefore = board.FEN()
            let nodeKey = { Hash = board.PositionHash(); FenKey = normalizeFenForKey fenBefore }
            let whiteToMove = board.Position.STM = 0uy
            let engineToMove = if whiteToMove then game.GameMetaData.White else game.GameMetaData.Black
            let opponent = if whiteToMove then game.GameMetaData.Black else game.GameMetaData.White

            let beforeCount = board.LongSANMovesPlayed.Count
            let absPly = board.PlyCount
            let moveNumber = board.MoveNumber()
            board.PlaySimpleShortSanWithComments san comment
            let afterCount = board.LongSANMovesPlayed.Count
            if afterCount = beforeCount then
              ply <- plies.Length
            else
              if ply >= cutoffPly then
                let isFirstTimeThisGame = seenNodeInGame.Add nodeKey
                if isFirstTimeThisGame then
                  match openingAgg.Nodes.TryGetValue nodeKey with
                  | true, nodeAgg ->
                      match chooseBaseline options.Baseline nodeAgg options.MinSamplesForBaseline with
                      | None -> ()
                      | Some (baselineMove, baselineReason) ->
                          let playedLan = board.LongSANMovesPlayed[afterCount - 1]
                          match nodeAgg.EdgesByLan.TryGetValue playedLan with
                          | true, playedEdge ->
                              let isTarget =
                                options.TargetEngine
                                |> Option.map (fun t -> String.Equals(t, engineToMove, StringComparison.OrdinalIgnoreCase))
                                |> Option.defaultValue true

                              if isTarget && playedEdge.Move.Lan <> baselineMove.Lan then
                                let ev =
                                  { OpeningKey = openingKey
                                    Node = nodeKey
                                    Ply = absPly
                                    MoveNumber = moveNumber
                                    WhiteToMove = whiteToMove
                                    EngineToMove = engineToMove
                                    Opponent = opponent
                                    GameNumber = game.GameNumber
                                    Round = game.GameMetaData.Round
                                    Result = result
                                    FenBefore = fenBefore
                                    FenAfter = board.FEN()
                                    Played = playedEdge.Move
                                    Baseline = baselineMove
                                    BaselineReason = baselineReason }

                                let includeEvent =
                                  match options.DeviationSignificance with
                                  | AllDeviations -> true
                                  | OnlyMissedWin -> isMissedWin ev

                                if includeEvent then events.Add ev
                          | _ -> ()
                  | _ -> ()
              ply <- ply + 1

  let eventsList =
    let raw = events |> Seq.toList
    let keepOnlyLast =
      options.KeepOnlyLastDeviationPerGame
      || (options.DeviationSignificance = OnlyMissedWin && options.MissedWinKeepOnlyLastPerGame)
    if keepOnlyLast then
      raw
      |> Seq.groupBy (fun e -> e.GameNumber)
      |> Seq.choose (fun (_, es) -> es |> Seq.maxBy (fun e -> e.Ply) |> Some)
      |> Seq.sortBy (fun e -> e.GameNumber)
      |> Seq.toList
    else
      raw

  let selfDevs = collectSelfDeviations openingAggs options

  let stats =
    { Games = games.Length
      Openings = openingAggs.Length
      Nodes = openingAggs |> Seq.sumBy (fun o -> o.Nodes.Count)
      Events = eventsList.Length
      SelfDeviations = selfDevs.Length }

  { Stats = stats
    OpeningInfo = openingInfo
    NodeStats = nodeStats
    Events = eventsList
    SelfDeviations = selfDevs }

let analyzePgnFile (pgnPath: string) (options: AnalysisOptions) =
  let games = PGNParser.parsePgnFile pgnPath |> Seq.toList
  analyzePgnGames games options

let formatReport (report: DeviationReport) (options: ReportFormatOptions) =
  let sb = Text.StringBuilder()
  let appendLine (s: string) = sb.AppendLine(s) |> ignore

  let clip (maxLen: int) (s: string) =
    let s = if isNull s then "" else s.Trim()
    if maxLen <= 0 || s.Length <= maxLen then s
    else s.Substring(0, maxLen - 1) + "…"

  let moverOutcome (result: string) (whiteToMove: bool) =
    let pts = pointsFor result whiteToMove
    if pts >= 0.999 then "WIN"
    elif pts <= 0.001 then "LOSS"
    else "DRAW"

  appendLine "Move deviation finder report"
  appendLine "----------------------------"
  appendLine $"Games: {report.Stats.Games} | Openings: {report.Stats.Openings} | Nodes: {report.Stats.Nodes} | Events: {report.Stats.Events} | SelfDevs: {report.Stats.SelfDeviations}"

  let byOpening =
    report.Events
    |> Seq.groupBy (fun e -> e.OpeningKey)
    |> Seq.map (fun (k, evs) -> k, evs |> Seq.toList)
    |> Seq.sortByDescending (fun (_, evs) -> evs.Length)
    |> Seq.truncate options.MaxOpenings
    |> Seq.toList

  if byOpening.Length = 0 then
    appendLine ""
    appendLine "No deviation events found."
  else
    appendLine ""
    appendLine $"Top openings by deviation events (showing {byOpening.Length}):"
    for (openingKey, evs) in byOpening do
      let openingFen =
        report.OpeningInfo
        |> Map.tryFind openingKey
        |> Option.defaultValue ""

      let nodes =
        evs
        |> Seq.groupBy (fun e -> e.Node)
        |> Seq.map (fun (nodeKey, nodeEvs) ->
            let nodeEvs = nodeEvs |> Seq.toList
            let baseline =
              nodeEvs
              |> Seq.tryHead
              |> Option.map (fun e -> e.Baseline.San, e.BaselineReason)
            nodeKey, nodeEvs, baseline)
        |> Seq.sortByDescending (fun (_, nodeEvs, _) -> nodeEvs.Length)
        |> Seq.truncate options.MaxNodesPerOpening
        |> Seq.toList

      appendLine ""
      let openingLabel =
        if String.IsNullOrWhiteSpace openingFen then
          openingKey
        else
          clip options.FenMaxLen openingFen
      if options.ShowOpeningKey then
        appendLine $"Opening {openingLabel} | Key {openingKey} | Events: {evs.Length} | Nodes: {nodes.Length}"
      else
        appendLine $"Opening {openingLabel} | Events: {evs.Length} | Nodes: {nodes.Length}"

      for (nodeKey, nodeEvs, baselineOpt) in nodes do
        let nodeStat =
          report.NodeStats
          |> Map.tryFind openingKey
          |> Option.bind (fun m -> Map.tryFind nodeKey m)

        let baselineCount =
          match nodeStat, baselineOpt with
          | Some ns, Some (baselineSan, _) ->
              ns.Moves |> List.tryFind (fun ms -> ms.Move.San = baselineSan) |> Option.map (fun ms -> ms.SamplesAll)
          | _ -> None

        let byEngine =
          nodeEvs
          |> Seq.groupBy (fun e -> e.EngineToMove)
          |> Seq.map (fun (eng, xs) -> eng, xs |> Seq.length)
          |> Seq.sortByDescending snd
          |> Seq.toList

        let baselineText =
          match baselineOpt with
          | Some (san, reason) ->
              match nodeStat, baselineCount with
              | Some ns, Some bc when ns.TotalGames > 0 -> $"{san} ({bc}/{ns.TotalGames}, {reason})"
              | _ -> $"{san} ({reason})"
          | None -> "n/a"

        let topEngText =
          byEngine
          |> Seq.truncate 4
          |> Seq.map (fun (eng, n) -> $"{eng}={n}")
          |> String.concat ", "

        let nodeGamesText =
          match nodeStat with
          | Some ns when ns.TotalGames > 0 -> $" / {ns.TotalGames} games"
          | _ -> ""
        appendLine $"  Node {nodeEvs.Length}x{nodeGamesText} | Baseline: {baselineText} | Engines: {topEngText}"
        if options.ShowFen then appendLine $"    FEN: {clip options.FenMaxLen nodeKey.FenKey}"

        nodeEvs
        |> Seq.sortBy (fun e -> e.GameNumber, e.Ply)
        |> Seq.truncate options.MaxEventsPerOpening
        |> Seq.iter (fun e ->
            let color = if e.WhiteToMove then "white" else "black"
            let outcome = moverOutcome e.Result e.WhiteToMove
            appendLine $"    G{e.GameNumber} R{e.Round} m{e.MoveNumber} ply {e.Ply} {e.EngineToMove} ({color}) played {e.Played.San} (vs {e.Baseline.San}) vs {e.Opponent} res {e.Result} (mover {outcome})")

  let selfDevs =
    report.SelfDeviations
    |> Seq.sortByDescending (fun sd -> sd.TotalSamples, sd.Moves.Length)
    |> Seq.truncate options.MaxSelfDevs
    |> Seq.toList

  appendLine ""
  appendLine $"Self deviations (same engine chose multiple moves from same position) (showing {selfDevs.Length}):"
  if selfDevs.Length = 0 then
    appendLine "  None."
  else
    for sd in selfDevs do
      let openingFen =
        report.OpeningInfo
        |> Map.tryFind sd.OpeningKey
        |> Option.defaultValue ""
      let openingLabel =
        if String.IsNullOrWhiteSpace openingFen then
          sd.OpeningKey
        else
          clip options.FenMaxLen openingFen

      let moveText =
        sd.Moves
        |> Seq.map (fun m ->
            let examples =
              sd.ExamplesByMove
              |> Map.tryFind m.Lan
              |> Option.defaultValue []
            let tags =
              examples
              |> Seq.truncate 2
              |> Seq.map (fun o -> $"G{o.GameNumber} R{o.Round} m{o.MoveNumber} ply {o.Ply}")
              |> String.concat ", "
            if String.IsNullOrWhiteSpace tags then m.San else $"{m.San} ({tags})")
        |> Seq.distinct
        |> String.concat " | "

      if options.ShowOpeningKey then
        appendLine $"  {sd.Engine} | Opening {openingLabel} | Key {sd.OpeningKey} | Samples {sd.TotalSamples} | Moves: {moveText}"
      else
        appendLine $"  {sd.Engine} | Opening {openingLabel} | Samples {sd.TotalSamples} | Moves: {moveText}"
      if options.ShowFen then appendLine $"    FEN: {clip options.FenMaxLen sd.Node.FenKey}"

  sb.ToString()

let printReportToConsole (report: DeviationReport) (options: ReportFormatOptions) =
  formatReport report options |> printfn "%s"
