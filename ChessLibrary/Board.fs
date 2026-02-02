module ChessLibrary.Chess

open System
open System.Threading
open System.Collections.Generic
open QBBOperations
open TypesDef
open TypesDef.Position
open TypesDef.TMove
open TypesDef.CoreTypes
open LowLevelUtilities
open Utilities
open MoveGeneration
open Parser
open System.Text.RegularExpressions

let startPos = Misc.startPosition

let [<Literal>] MAX_PLY = 1000
let [<Literal>] MAX_MOVES = 256

let moveList = new ThreadLocal<TMove array> (fun () -> Array.zeroCreate<TMove>(MAX_MOVES))

// DAG representation of game tree with shared positions (transpositions).
type NodeId = int
type EdgeId = int
type PositionNode = { Id: NodeId; Hash: uint64; Fen: string; Parents: EdgeId list; Children: EdgeId list }
type MoveEdge =
  { Id: EdgeId
    From: NodeId
    To: NodeId
    San: string
    Lan: string
    Comments: string
    Color: string
    IsCastling: bool
    Order: int
    IsMainline: bool }
type MoveGraph =
  { mutable Root: NodeId
    NodesById: Dictionary<NodeId, PositionNode>
    EdgesById: Dictionary<EdgeId, MoveEdge>
    NodesByHash: Dictionary<uint64, ResizeArray<NodeId>> }

[<CLIMutable>]
type InlineMoveToken =
  { Text:string
    DisplayText:string
    Fen:string
    MoveCoord:string
    IsBracket:bool
    Hash:uint64
    FromVariation:bool
    Ply:int
    Evaluation:string
    IsLineStart:bool }

type Board() =        
    let mutable isFRC = false
    let mutable startPos = ""
    let mutable mostCurrentFEN = ""
    let mutable captures = 0L
    let mutable castles = 0L
    let mutable eps = 0L
    let mutable iPosition = 0
    let mutable numberOfMoves = 0L
    let mutable game = Array.init MAX_PLY (fun _ -> Position.Default)
    let mutable position = game.[0]    
    let mutable hashKeys = ResizeArray<uint64>()
    let mutable lastHistoryTokens : string array option = None
    let sbPool = Utils.StringBuilderPool(10,20)
    let moves = ResizeArray<TMove>()
    let longSanMoves = ResizeArray<string>()
    let shortSanMoves = ResizeArray<string>()
    let shortSanOpeningMoves = ResizeArray<string>()
    let openingMoves = ResizeArray<string>()
    let moveAndFens = ResizeArray<MoveAndFen>()    

    let mutable graphNodeIdCounter = 0
    let mutable graphEdgeIdCounter = 0
    let nextGraphNodeId() =
      graphNodeIdCounter <- graphNodeIdCounter + 1
      graphNodeIdCounter
    let nextGraphEdgeId() =
      graphEdgeIdCounter <- graphEdgeIdCounter + 1
      graphEdgeIdCounter

    let mutable moveGraph =
      { Root = 0
        NodesById = Dictionary<NodeId, PositionNode>()
        EdgesById = Dictionary<EdgeId, MoveEdge>()
        NodesByHash = Dictionary<uint64, ResizeArray<NodeId>>() }
    let mutable currentGraphNodeId = 0

    let mutable createNodeIsVariation = false
    let lockObject = obj()
    
    let updatePosition action =
      Monitor.Enter(lockObject)
      try
          action()
      finally
          Monitor.Exit(lockObject)

    // --- MoveGraph helpers --------------------------------------------------
    let addNodeToHashMap (node: PositionNode) =
      match moveGraph.NodesByHash.TryGetValue node.Hash with
      | true, ids -> ids.Add node.Id
      | _ ->
        let ids = ResizeArray<NodeId>()
        ids.Add node.Id
        moveGraph.NodesByHash[node.Hash] <- ids

    let registerGraphNode (node: PositionNode) =
      moveGraph.NodesById[node.Id] <- node
      addNodeToHashMap node

    let updateGraphNode (node: PositionNode) =
      moveGraph.NodesById[node.Id] <- node

    let removeNodeFromHashMap (node: PositionNode) =
      match moveGraph.NodesByHash.TryGetValue node.Hash with
      | true, ids ->
          ids.Remove node.Id |> ignore
          if ids.Count = 0 then
            moveGraph.NodesByHash.Remove node.Hash |> ignore
      | _ -> ()

    let unlinkEdge edgeId fromId toId =
      match moveGraph.NodesById.TryGetValue fromId with
      | true, parent ->
          let updated = { parent with Children = parent.Children |> List.filter (fun id -> id <> edgeId) }
          updateGraphNode updated
      | _ -> ()
      match moveGraph.NodesById.TryGetValue toId with
      | true, child ->
          let updated = { child with Parents = child.Parents |> List.filter (fun id -> id <> edgeId) }
          updateGraphNode updated
      | _ -> ()

    let reindexChildOrders parentId =
      match moveGraph.NodesById.TryGetValue parentId with
      | true, parent ->
          parent.Children
          |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
          |> List.sortBy (fun e -> e.Order)
          |> List.mapi (fun idx e -> e.Id, idx)
          |> List.iter (fun (eid, order) ->
              match moveGraph.EdgesById.TryGetValue eid with
              | true, edge when edge.Order <> order -> moveGraph.EdgesById[eid] <- { edge with Order = order }
              | _ -> ())
      | _ -> ()

    let setMainChild parentId mainEdgeId =
      match moveGraph.NodesById.TryGetValue parentId with
      | true, parent ->
          let edges =
            parent.Children
            |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)

          let reordered =
            edges
            |> List.sortBy (fun e -> e.Order)
            |> List.sortBy (fun e -> if e.Id = mainEdgeId then 0 else 1)

          reordered
          |> List.mapi (fun idx e ->
              let isMain = e.Id = mainEdgeId
              let edge' = if e.IsMainline <> isMain || e.Order <> idx then { e with IsMainline = isMain; Order = idx } else e
              moveGraph.EdgesById[e.Id] <- edge')
          |> ignore
          ()
      | _ -> ()

    let rec removeEdgeSubtree edgeId =
      match moveGraph.EdgesById.TryGetValue edgeId with
      | true, edge ->
          let childId = edge.To
          let childNodeOpt =
            match moveGraph.NodesById.TryGetValue childId with
            | true, n -> Some n
            | _ -> None

          match childNodeOpt with
          | Some childNode ->
              // Remove descendants first
              childNode.Children |> List.iter removeEdgeSubtree

              // Detach this edge
              unlinkEdge edgeId edge.From childId
              moveGraph.EdgesById.Remove edgeId |> ignore

              // If the child becomes orphaned and isn't the root, prune it.
              match moveGraph.NodesById.TryGetValue childId with
              | true, updatedChild when List.isEmpty updatedChild.Parents && childId <> moveGraph.Root ->
                  updatedChild.Children |> List.iter removeEdgeSubtree
                  removeNodeFromHashMap updatedChild
                  moveGraph.NodesById.Remove childId |> ignore
              | _ -> ()

              // Reindex the remaining siblings of the parent.
              reindexChildOrders edge.From
          | None -> moveGraph.EdgesById.Remove edgeId |> ignore
      | _ -> ()

    let getGraphNodeByHash (hash:uint64) =
      match moveGraph.NodesByHash.TryGetValue hash with
      | true, ids when ids.Count > 0 ->
        let id = ids[0]
        moveGraph.NodesById[id] |> Some
      | _ -> None

    let createGraphNode (hash:uint64) (fen:string) =
      let id = nextGraphNodeId()
      let node = { Id = id; Hash = hash; Fen = fen; Parents = []; Children = [] }
      registerGraphNode node
      node
    // Navigation stability: always create a new node for each move, but still index by hash for lookup.
    let findOrCreateGraphNode (hash:uint64) (fen:string) (_fromId: NodeId) =
      createGraphNode hash fen

    let addGraphEdge (fromId: NodeId) (toHash:uint64) (toFen:string) (san:string) (lan:string) (color:string) (isCastling: bool) (comments:string) (isMainlineOpt: bool option) (orderOpt:int option) =
      let fromNode = moveGraph.NodesById[fromId]
      // Avoid introducing cycles when the same position (hash) appears again along the mainline.
      let toNode = findOrCreateGraphNode toHash toFen fromId

      let children =
        fromNode.Children
        |> List.choose (fun eid -> moveGraph.EdgesById.TryGetValue eid |> function | true, e -> Some e | _ -> None)
      let hasMainline = children |> List.exists (fun e -> e.IsMainline)
      let order = defaultArg orderOpt children.Length
      let isMainline =
        match isMainlineOpt with
        | Some b -> b
        | None -> not hasMainline

      let eid = nextGraphEdgeId()
      let edge =
        { Id = eid
          From = fromId
          To = toNode.Id
          San = san
          Lan = lan
          Comments = comments
          Color = color
          IsCastling = isCastling
          Order = order
          IsMainline = isMainline }
      moveGraph.EdgesById[eid] <- edge

      let updatedFrom = { fromNode with Children = edge.Id :: fromNode.Children }
      let updatedTo = { toNode with Parents = edge.Id :: toNode.Parents }
      updateGraphNode updatedFrom
      updateGraphNode updatedTo
      edge.Id

    let mainChildEdgeId (nodeId: NodeId) =
      let children =
        moveGraph.NodesById[nodeId].Children
        |> List.choose (fun eid -> moveGraph.EdgesById.TryGetValue eid |> function | true, e -> Some e | _ -> None)
        |> List.sortBy (fun e -> e.Order)
      children
      |> List.tryFind (fun e -> e.IsMainline)
      |> Option.orElseWith (fun () -> children |> List.tryHead)
      |> Option.map (fun e -> e.Id)

    let resetGraph rootFen rootHash =
      graphNodeIdCounter <- 0
      graphEdgeIdCounter <- 0
      moveGraph <-
        { Root = 0
          NodesById = Dictionary<NodeId, PositionNode>()
          EdgesById = Dictionary<EdgeId, MoveEdge>()
          NodesByHash = Dictionary<uint64, ResizeArray<NodeId>>() }
      let rootNode =
        { Id = nextGraphNodeId()
          Hash = rootHash
          Fen = rootFen
          Parents = []
          Children = [] }
      registerGraphNode rootNode
      moveGraph.Root <- rootNode.Id
      currentGraphNodeId <- rootNode.Id

    let setCurrentGraphNodeFromHash fen hash =
      match moveGraph.NodesByHash.TryGetValue hash with
      | true, ids when ids.Count > 0 ->
          let matchingFen =
            ids
            |> Seq.filter (fun id -> moveGraph.NodesById[id].Fen = fen)
            |> Seq.toList
          let candidates = if matchingFen.Length > 0 then matchingFen else ids |> Seq.toList
          let preferredChild =
            candidates
            |> List.tryFind (fun id ->
                moveGraph.NodesById[id].Parents
                |> List.exists (fun eid ->
                    match moveGraph.EdgesById.TryGetValue eid with
                    | true, e -> e.From = currentGraphNodeId
                    | _ -> false))
          let chosen =
            match preferredChild with
            | Some id -> id
            | None -> candidates |> List.max
          currentGraphNodeId <- chosen
      | _ -> currentGraphNodeId <- moveGraph.Root

    let moveAndFenFromEdge (edge: MoveEdge) (toNode: PositionNode) =
      let lan = edge.Lan
      let fromSq = if lan.Length >= 2 then lan.Substring(0,2) else ""
      let toSq = if lan.Length >= 4 then lan.Substring(2,2) else ""
      let md =
        { MoveDetail.Empty with
            LongSan = lan
            FromSq = fromSq
            ToSq = toSq
            Color = edge.Color
            IsCastling = edge.IsCastling
            Comments = edge.Comments }
      { Move = md; ShortSan = edge.San; FenAfterMove = toNode.Fen }

    let updatePathFromCurrent () =
      moveAndFens.Clear()
      longSanMoves.Clear()
      let visited = HashSet<NodeId>()
      let rec ascend nodeId acc =
        if visited.Contains nodeId then acc
        else
          visited.Add nodeId |> ignore
          let node = moveGraph.NodesById[nodeId]
          match node.Parents |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None) with
          | [] -> acc
          | parents ->
              let chosen =
                parents
                |> List.sortBy (fun e -> e.Order)
                |> List.head
              ascend chosen.From (chosen :: acc)
      let pathEdges = ascend currentGraphNodeId []
      for e in pathEdges do
        let toNode = moveGraph.NodesById[e.To]
        moveAndFens.Add (moveAndFenFromEdge e toNode)
        longSanMoves.Add e.Lan

    let endCurrentVariation () =
      moveAndFens.Clear()
      let visitedAscend = HashSet<NodeId>()
      let rec ascend nodeId acc =
        if visitedAscend.Contains nodeId then acc
        else
          visitedAscend.Add nodeId |> ignore
          let node = moveGraph.NodesById[nodeId]
          match node.Parents |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None) with
          | [] -> acc
          | parents ->
              let chosen = parents |> List.sortBy (fun e -> e.Order) |> List.head
              ascend chosen.From (chosen :: acc)
      let visitedDescend = HashSet<NodeId>()
      let rec descend nodeId =
        if visitedDescend.Contains nodeId then ()
        else
          visitedDescend.Add nodeId |> ignore
          match mainChildEdgeId nodeId with
          | Some eid ->
              let edge = moveGraph.EdgesById[eid]
              let toNode = moveGraph.NodesById[edge.To]
              moveAndFens.Add (moveAndFenFromEdge edge toNode)
              descend edge.To
          | None -> ()
      ascend currentGraphNodeId [] |> List.iter (fun e -> let toNode = moveGraph.NodesById[e.To] in moveAndFens.Add (moveAndFenFromEdge e toNode))
      descend currentGraphNodeId

    let resetWithFen (fenOpt:string option) =
      iPosition <- 0
      moves.Clear()
      longSanMoves.Clear()
      shortSanMoves.Clear()      
      openingMoves.Clear()
      shortSanOpeningMoves.Clear()
      moveAndFens.Clear()
      hashKeys.Clear()
      position <- game.[0]
      let newPos =
        match fenOpt with
        | Some fen -> BoardHelper.getPosFromFen(Some fen)
        | None -> BoardHelper.getPosFromFen(None)
      let update = fun () -> position <- newPos
      updatePosition update
      startPos <- BoardHelper.posToFen position
      mostCurrentFEN <- startPos
      isFRC <- PositionOps.isFRC &position
      numberOfMoves <- 0L
      numberOfMoves <- 0L
      let rootHash = Utilities.Hash.hashBoard position
      resetGraph startPos rootHash
      updatePathFromCurrent()
      lastHistoryTokens <- None

    let initBoard () =
      iPosition <- 0
      moves.Clear()
      longSanMoves.Clear()
      shortSanMoves.Clear()      
      openingMoves.Clear()
      shortSanOpeningMoves.Clear()
      moveAndFens.Clear()
      hashKeys.Clear()
      game <- Array.init MAX_PLY (fun _ -> Position.Default)
      position <- game.[0]
      BoardHelper.loadFen(None, &position)
      startPos <- BoardHelper.posToFen position
      mostCurrentFEN <- startPos
      let rootHash = Utilities.Hash.hashBoard position
      resetGraph startPos rootHash
      updatePathFromCurrent()
      ZobrishHash.initializeZobristTables()
    do
        initBoard ()
    
    member _.EndCurrentVariation () = endCurrentVariation ()

    member val inAnalysisMode = false with get, set

    member _.ResetBoardState () =
      resetWithFen None

    member _.ResetBoardStateFromFen (fen:string) =
      let fenOpt = if String.IsNullOrWhiteSpace fen then None else Some fen
      resetWithFen fenOpt

    member this.Game
      with get() = game
      and set(v) = game <- v

    member this.Captures
      with get() = captures
      and set(v) = captures <- v

    member this.Castles
      with get() = castles
      and set(v) = castles <- v

    member this.EP
      with get() = eps
      and set(v) = eps <- v

    member this.PlyCount with get() = int position.Ply

    member this.Position
      with get() = position
      and set(value) = position <- value

    member this.LoadFen(?fen:string) =
      if fen.IsSome then
        if String.IsNullOrEmpty fen.Value then
          let newPos = BoardHelper.getPosFromFen(None)
          position <- newPos
        else
          let newPos = BoardHelper.getPosFromFen(fen)
          position <- newPos
          mostCurrentFEN <- fen.Value
        game.[int position.Ply] <- PositionOps.copy &position
        isFRC <- PositionOps.isFRC &position
        // Try to move the graph cursor to the matching node for this FEN.
        let fenStr = this.FEN()
        let hash = Utilities.Hash.hashBoard position
        setCurrentGraphNodeFromHash fenStr hash
        updatePathFromCurrent()

    member this.FEN() = BoardHelper.posToFen position

    member this.StartPosition
      with get() = startPos
      and set(v) = startPos <- v

    member this.CurrentFEN
      with get() = mostCurrentFEN
      and set(v) = mostCurrentFEN <- v

    member this.PositionWithMoves() =      
      if longSanMoves.Count = 0 then
        sprintf $"position fen {startPos}"
      else
        let start = sprintf $"position fen {startPos} moves"
        longSanMoves |> Seq.fold (fun state m -> sprintf "%s %s" state m) start

    member this.GetCurrentEdgeComment () =      
        // update the incoming edge to the current node
        match moveGraph.NodesById.TryGetValue currentGraphNodeId with
        | true, node ->
            node.Parents
            |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
            |> List.sortBy (fun e -> e.Order)
            |> List.tryHead
            |> Option.map (fun edge -> edge.Comments)
            |> Option.defaultValue String.Empty
        | _ -> String.Empty
    
    member this.SetCommentOnCurrentEdge (comment:string) =
      if String.IsNullOrWhiteSpace comment |> not then
        // keep MovesAndFenPlayed in sync
        if moveAndFens.Count > 0 then
          let lastIdx = moveAndFens.Count - 1
          let last = moveAndFens[lastIdx]
          moveAndFens[lastIdx] <- { last with Move = { last.Move with Comments = comment } }

        // update the incoming edge to the current node
        match moveGraph.NodesById.TryGetValue currentGraphNodeId with
        | true, node ->
            node.Parents
            |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
            |> List.sortBy (fun e -> e.Order)
            |> List.tryHead
            |> Option.iter (fun edge -> moveGraph.EdgesById[edge.Id] <- { edge with Comments = comment })
        | _ -> ()
    
    // Graph-aware callers expect a path command
    member this.PositionWithMovesFromGraph() = 
        updatePathFromCurrent()
        this.PositionWithMoves()

    member this.PositionWithFenAndMoves (fen:string) =
      if longSanMoves.Count = 0 then
        sprintf $"position fen {fen}"
      else
        longSanMoves
        |> Seq.fold (fun state m -> sprintf "%s %s" state m) (sprintf $"position fen {fen} moves")

    member this.SanMoveNumberString san =
      if String.IsNullOrWhiteSpace san then
        ""
      else
        let ply = position.Ply |> int
        let moveNr = ((ply + 1) / 2)
        if position.STM = 0uy then // white to move => black just moved
          if moveNr = 0 then sprintf "%d. %s" 1 san else sprintf "%d ...%s" moveNr san
        else
          sprintf "%d. %s" moveNr san

    member this.MoveNumber() =
      let ply = int position.Ply
      max 1 ((ply + 1) / 2)

    //stats - castles, captures and eps
    member this.CollectStat (move:_ inref) =
      if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
        castles <- castles + 1L
      elif (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
        captures <- captures + 1L
      if (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then
        eps <- eps + 1L

    member this.WriteGraphLinesToPgn (path:string) (opening:string) (variation:string) =      
      let formatMoveLine (moves: string list) =
        let sb = sbPool.Get()
        for idx, lan in moves |> List.indexed do
          if idx % 2 = 0 then
            let moveNr = (idx / 2) + 1
            if sb.Length > 0 then sb.Append(" ") |> ignore
            sb.Append($"{moveNr}. {lan}") |> ignore
          else
            sb.Append($" {lan}") |> ignore
        sb.Append(" *") |> ignore
        sbPool.Return(sb)
        sb.ToString()

      let ensureDirectory () =
        match System.IO.Path.GetDirectoryName path with
        | null | "" -> ()
        | dir when System.IO.Directory.Exists(dir) |> not -> System.IO.Directory.CreateDirectory(dir) |> ignore
        | _ -> ()

      let lines : string list list = this.MoveLinesFromGraph false
      if lines.Length > 0 then      
        ensureDirectory()
        use writer = new System.IO.StreamWriter(path, false, System.Text.Encoding.UTF8)
        for idx, moves in lines |> List.indexed do
          let eventTag =
            if idx = 0 then "Mainline"
            else sprintf "Variation %d" idx
          writer.WriteLine $"[Event \"{eventTag}\"]"
          if not (String.IsNullOrWhiteSpace opening) then
            writer.WriteLine $"[Opening \"{opening}\"]"
          if not (String.IsNullOrWhiteSpace variation) then
            writer.WriteLine $"[Variation \"{variation}\"]"
          writer.WriteLine $"[FEN \"{startPos}\"]"
          writer.WriteLine()
          writer.WriteLine (formatMoveLine moves)
          writer.WriteLine()
    
    member this.MoveLinesFromGraph (asLAN:bool) =
      let childEdges nodeId =
        moveGraph.NodesById[nodeId].Children
        |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
        |> List.sortBy (fun e -> e.Order)

      let rec depthFrom nodeId =
        match childEdges nodeId with
        | [] -> 0
        | children -> children |> List.map (fun e -> 1 + depthFrom e.To) |> List.max

      let appendMove path (move : MoveEdge) =        
          let moveText =
              let baseText = if asLAN then move.Lan else move.San
              if String.IsNullOrWhiteSpace baseText then None
              //elif not withComments then Some baseText
              elif String.IsNullOrWhiteSpace move.Comments then Some baseText
              else 
                let comment = baseText + " {" + move.Comments + "}"
                Some comment
          match moveText with
          | Some t -> path @ [t]
          | None -> path

      let rec traverse nodeId path =
        match childEdges nodeId with
        | [] -> [path]
        | children ->
          let main =
            match children |> List.tryFind (fun e -> e.IsMainline) with
            | Some mc -> mc
            | None -> children |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))
          let mainPaths = traverse main.To (appendMove path main)
          let variationPaths =
              children
              |> List.filter (fun e -> e.Id <> main.Id)
              |> List.collect (fun v -> traverse v.To (appendMove path v))            
          mainPaths @ variationPaths

      match childEdges moveGraph.Root with
      | [] -> []
      | rootChildren ->
        let main =
          match rootChildren |> List.tryFind (fun e -> e.IsMainline) with
          | Some mc -> mc
          | None -> rootChildren |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))
        let rootVariations = rootChildren |> List.filter (fun e -> e.Id <> main.Id)
        let mainPaths = traverse main.To (appendMove [] main)            
        let variationPaths = rootVariations |> List.collect (fun v -> traverse v.To (appendMove [] v))          
        mainPaths @ variationPaths
    
    member this.GetMoveHistoryWithVariations () =      
      let tokens = this.InlineTokensFromGraph() |> Seq.toList
      let sb = sbPool.Get()
      let mutable depth = 0
      let mutable lastNumber : int option = None

      let appendToken (tok:InlineMoveToken) =
        match tok.IsBracket, tok.Text with
        | true, "(" ->
            if sb.Length > 0 && sb[sb.Length - 1] <> ' ' then sb.Append(" ") |> ignore
            sb.Append("(") |> ignore
            depth <- depth + 1
        | true, ")" ->
            sb.Append(")") |> ignore
            depth <- Math.Max(0, depth - 1)
        | _ ->
            let moveNr = (tok.Ply / 2) + 1
            let prefix =
              if tok.Ply % 2 = 0 then
                lastNumber <- Some moveNr
                sprintf "%d. " moveNr
              elif depth > 0 then
                if tok.IsLineStart || lastNumber <> Some moveNr then
                  lastNumber <- Some moveNr
                  sprintf "%d... " moveNr
                else ""
              elif tok.IsLineStart then
                sprintf "%d... " moveNr
              else ""
            if sb.Length > 0 && sb[sb.Length - 1] <> '(' && sb[sb.Length - 1] <> ' ' then sb.Append(" ") |> ignore
            let withComment =
              if String.IsNullOrWhiteSpace tok.Evaluation then
                tok.Text
              elif tok.Evaluation.StartsWith("{") then
                sprintf "%s %s" tok.Text tok.Evaluation
              else
                sprintf "%s {%s}" tok.Text tok.Evaluation
            sb.Append(prefix).Append(withComment) |> ignore

      tokens |> List.iter appendToken
      sbPool.Return(sb)
      sb.ToString().Trim()

    member this.IsFRC
      with get() = isFRC
      and set(v) = isFRC <- v

    member this.HashKeys  
      with get() = hashKeys
      and set(v) = hashKeys <- v
    
    member this.PositionHash () = Utilities.Hash.hashBoard position
    
    member this.DeviationHash () = Utilities.Hash.deviationHash position

    member this.TryGetNextMoveAndFen (fen:string) =
      if String.IsNullOrWhiteSpace fen |> not then
        let pos = BoardHelper.getPosFromFen(Some fen)
        let hash = Utilities.Hash.hashBoard pos
        setCurrentGraphNodeFromHash fen hash
      match mainChildEdgeId currentGraphNodeId with
      | Some eid ->
          let edge = moveGraph.EdgesById[eid]
          let toNode = moveGraph.NodesById[edge.To]
          currentGraphNodeId <- toNode.Id
          updatePathFromCurrent()
          Some (moveAndFenFromEdge edge toNode)
      | None -> None

    member this.TryGetPreviousMoveAndFen (fen:string) =
      if String.IsNullOrWhiteSpace fen |> not then
        let pos = BoardHelper.getPosFromFen(Some fen)
        let hash = Utilities.Hash.hashBoard pos
        setCurrentGraphNodeFromHash fen hash
      let node = moveGraph.NodesById[currentGraphNodeId]
      match node.Parents |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None) with
      | [] -> Some {MoveAndFen.FirstEntry with FenAfterMove=startPos}
      | parents ->
          let chosen = parents |> List.sortBy (fun e -> e.Order) |> List.head
          let fromNode = moveGraph.NodesById[chosen.From]
          currentGraphNodeId <- fromNode.Id
          updatePathFromCurrent()
          Some (moveAndFenFromEdge chosen fromNode)

    member val MovesPlayed = moves with set, get

    member _.LongSANMovesPlayed = longSanMoves

    member val ShortSANMovesPlayed = shortSanMoves with get, set

    member val ShortSANOpeningMovesPlayed = shortSanOpeningMoves with get, set

    member val OpeningMovesPlayed = openingMoves with get, set

    member val MovesAndFenPlayed = moveAndFens with get, set    

    member _.CurrentNodeHash = moveGraph.NodesById[currentGraphNodeId].Hash

    member _.CurrentGraphNodeId = currentGraphNodeId
    member _.MoveGraphRootId = moveGraph.Root
    member _.MoveGraphNode(nodeId: NodeId) = moveGraph.NodesById[nodeId]
    member _.MoveGraphChildren(nodeId: NodeId) =
      moveGraph.NodesById[nodeId].Children
      |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
      |> List.sortBy (fun e -> e.Order)

    member this.JumpToHash (hash:uint64) =
      match moveGraph.NodesByHash.TryGetValue hash with
      | true, ids when ids.Count > 0 ->
          let preferredChild =
            ids
            |> Seq.tryFind (fun id ->
                moveGraph.NodesById[id].Parents
                |> List.exists (fun eid ->
                    match moveGraph.EdgesById.TryGetValue eid with
                    | true, e -> e.From = currentGraphNodeId
                    | _ -> false))
          let preferred =
            match preferredChild with
            | Some id -> id
            // If no direct child from the current node, pick the most recently created node (highest id)
            | None -> ids |> Seq.max
          let node = moveGraph.NodesById[preferred]
          this.LoadFen(node.Fen)
          currentGraphNodeId <- preferred
          updatePathFromCurrent()
          true
      | _ -> false

    // Remove a variation edge that leads to the provided FEN (matching SAN if supplied).
    // When removeEntireVariation = true the whole branch is pruned from the variation head.
    member this.RemoveVariationNode (san:string) (fen:string) (removeEntireVariation: bool) =
      if String.IsNullOrWhiteSpace fen then false
      else
        let pos = BoardHelper.getPosFromFen(Some fen)
        let hash = Utilities.Hash.hashBoard pos

        let matchingNodes =
          match moveGraph.NodesByHash.TryGetValue hash with
          | true, ids when ids.Count > 0 -> ids |> Seq.filter (fun id -> moveGraph.NodesById[id].Fen = fen) |> Seq.toList
          | _ -> []

        let tryFindEdge nodeId =
          moveGraph.NodesById[nodeId].Parents
          |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
          |> List.tryFind (fun e -> String.IsNullOrWhiteSpace san || e.San.Equals(san, StringComparison.OrdinalIgnoreCase))
          |> Option.map (fun e -> e, nodeId)

        let target =
          matchingNodes
          |> List.tryPick tryFindEdge
          |> Option.orElseWith (fun () ->
              match matchingNodes with
              | nodeId :: _ when moveGraph.NodesById[nodeId].Parents.IsEmpty |> not ->
                  let parentEdgeId = moveGraph.NodesById[nodeId].Parents |> List.head
                  match moveGraph.EdgesById.TryGetValue parentEdgeId with
                  | true, e -> Some (e, nodeId)
                  | _ -> None
              | _ -> None)

        match target with
        | None -> false
        | Some (edge, _) ->
            // Allow deleting a "mainline" edge only if it belongs to a variation branch.
            let rec hasVariationAncestor (e: MoveEdge) =
              if not e.IsMainline then true
              else
                moveGraph.NodesById[e.From].Parents
                |> List.choose (fun pid -> match moveGraph.EdgesById.TryGetValue pid with | true, pe -> Some pe | _ -> None)
                |> List.exists hasVariationAncestor
            if edge.IsMainline && not (hasVariationAncestor edge) then false
            else
            let rec findVariationHead (e: MoveEdge) =
              let parents =
                moveGraph.NodesById[e.From].Parents
                |> List.choose (fun pid -> match moveGraph.EdgesById.TryGetValue pid with | true, pe -> Some pe | _ -> None)
                |> List.filter (fun pe -> pe.IsMainline |> not)
              match parents with
              | parent :: _ -> findVariationHead parent
              | [] -> e

            let edgeIdToRemove =
              if removeEntireVariation then
                findVariationHead edge |> fun e -> e.Id
              else edge.Id

            let parentId =
              match moveGraph.EdgesById.TryGetValue edgeIdToRemove with
              | true, e -> e.From
              | _ -> moveGraph.Root

            removeEdgeSubtree edgeIdToRemove
            if moveGraph.NodesById.ContainsKey currentGraphNodeId |> not then
              currentGraphNodeId <- moveGraph.Root
            else
              reindexChildOrders parentId
            updatePathFromCurrent()
            true

    member this.RemoveVariationTail (san:string) (fen:string) =
      if String.IsNullOrWhiteSpace fen then false
      else
        let pos = BoardHelper.getPosFromFen(Some fen)
        let hash = Utilities.Hash.hashBoard pos

        let matchingNodes =
          match moveGraph.NodesByHash.TryGetValue hash with
          | true, ids when ids.Count > 0 -> ids |> Seq.filter (fun id -> moveGraph.NodesById[id].Fen = fen) |> Seq.toList
          | _ -> []

        let tryFindEdge nodeId =
          moveGraph.NodesById[nodeId].Parents
          |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
          |> List.tryFind (fun e -> String.IsNullOrWhiteSpace san || e.San.Equals(san, StringComparison.OrdinalIgnoreCase))
          |> Option.map (fun e -> e, nodeId)

        let target =
          matchingNodes
          |> List.tryPick tryFindEdge
          |> Option.orElseWith (fun () ->
              match matchingNodes with
              | nodeId :: _ when moveGraph.NodesById[nodeId].Parents.IsEmpty |> not ->
                  let parentEdgeId = moveGraph.NodesById[nodeId].Parents |> List.head
                  match moveGraph.EdgesById.TryGetValue parentEdgeId with
                  | true, e -> Some (e, nodeId)
                  | _ -> None
              | _ -> None)

        match target with
        | None -> false
        | Some (edge, _) ->
            let parentId = edge.From
            removeEdgeSubtree edge.Id
            if moveGraph.NodesById.ContainsKey currentGraphNodeId |> not then
              currentGraphNodeId <- moveGraph.Root
            else
              reindexChildOrders parentId
            updatePathFromCurrent()
            true

    member this.PromoteVariationToMainline (san:string) (fen:string) =
      if String.IsNullOrWhiteSpace fen then false
      else
        let pos = BoardHelper.getPosFromFen(Some fen)
        let hash = Utilities.Hash.hashBoard pos
        let matchingNodes =
          match moveGraph.NodesByHash.TryGetValue hash with
          | true, ids when ids.Count > 0 -> ids |> Seq.filter (fun id -> moveGraph.NodesById[id].Fen = fen) |> Seq.toList
          | _ -> []

        let tryFindEdge nodeId =
          moveGraph.NodesById[nodeId].Parents
          |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None)
          |> List.tryFind (fun e -> String.IsNullOrWhiteSpace san || e.San.Equals(san, StringComparison.OrdinalIgnoreCase))
          |> Option.map (fun e -> e, nodeId)

        let target =
          matchingNodes
          |> List.tryPick tryFindEdge
          |> Option.orElseWith (fun () ->
              match matchingNodes with
              | nodeId :: _ when moveGraph.NodesById[nodeId].Parents.IsEmpty |> not ->
                  let parentEdgeId = moveGraph.NodesById[nodeId].Parents |> List.head
                  match moveGraph.EdgesById.TryGetValue parentEdgeId with
                  | true, e -> Some (e, nodeId)
                  | _ -> None
              | _ -> None)

        match target with
        | None -> false
        | Some (edge, _) ->
            let rec variationHead (e: MoveEdge) (lastNonMain: MoveEdge option) =
              let nextLast = if e.IsMainline then lastNonMain else Some e
              match moveGraph.NodesById[e.From].Parents |> List.choose (fun pid -> match moveGraph.EdgesById.TryGetValue pid with | true, pe -> Some pe | _ -> None) |> List.tryHead with
              | Some parentEdge -> variationHead parentEdge nextLast
              | None -> defaultArg nextLast e

            let headEdge = variationHead edge None
            let parentId = headEdge.From
            setMainChild parentId headEdge.Id
            updatePathFromCurrent()
            true

    member this.LoadMoveHistoryWithVariations (history:string) =
      // Reset board and graph, then parse the provided PGN-style move string (with variations).
      let originalStart = this.StartPosition
      this.ResetBoardStateFromFen(originalStart)
      createNodeIsVariation <- false
  
      // Extract comments and their positions BEFORE stripping them
      let commentPattern = @"\{([^}]*)\}"
      let commentMatches = Regex.Matches(history, commentPattern)
      let commentsMap = Dictionary<int, string>()
  
      // Map each comment to its position in the original string
      for m in commentMatches do
        commentsMap[m.Index] <- m.Groups.[1].Value

      // Strip PGN comments to avoid treating them as SAN tokens.
      let cleaned =
        history
        |> fun h -> Regex.Replace(h, @"\{[^}]*\}", " ")
        |> fun h -> h.Replace("\n", " ")
        |> fun h -> h.Replace("\r", " ")

      // Tokenize with explicit parentheses.
      let normalized =
        cleaned
          .Replace("(", " ( ")
          .Replace(")", " ) ")
          .Split([|' '; '\t'; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.toArray
      lastHistoryTokens <- Some normalized

      // Create a mapping from move tokens to their comments
      // In PGN: "1. e4 {good move} e5 {also good}"
      // Comments appear AFTER the move they describe
      let moveToCommentMap = Dictionary<string, string>()
  
      // Build a map of token positions in the original history string
      let tokenPositions = ResizeArray<int * string>()
      let mutable searchPos = 0
      for token in normalized do
        let foundPos = history.IndexOf(token, searchPos)
        if foundPos >= 0 then
          tokenPositions.Add(foundPos, token)
          searchPos <- foundPos + token.Length
  
      // Now map each token to comments that appear immediately after it
      for i = 0 to tokenPositions.Count - 1 do
        let (tokenPos, token) = tokenPositions.[i]
        let tokenEnd = tokenPos + token.Length
    
        // Find the position where the next token starts (or end of string)
        let nextTokenStart = 
          if i + 1 < tokenPositions.Count then
            fst tokenPositions.[i + 1]
          else
            history.Length
    
        // Look for comments between this token's end and the next token's start
        let relevantComments = 
          commentsMap
          |> Seq.filter (fun kvp -> kvp.Key >= tokenEnd && kvp.Key < nextTokenStart)
          |> Seq.map (fun kvp -> kvp.Value)
          |> String.concat "; "
    
        if not (String.IsNullOrWhiteSpace relevantComments) then
          moveToCommentMap[token] <- relevantComments

      let isMoveNumber (tok:string) =
        tok.EndsWith(".") || tok.EndsWith("...") || tok |> Seq.forall Char.IsDigit

      let mutable currentNodeId = moveGraph.Root
      let mutable lastMoveNodeId : NodeId option = None
      let stack = Stack<(NodeId * NodeId option * bool)>()

      let setBoardToNode (nodeId: NodeId) =
        let fen = moveGraph.NodesById[nodeId].Fen
        this.LoadFen(fen)
        currentNodeId <- nodeId
        currentGraphNodeId <- nodeId

      let playSan (san:string) =
        setBoardToNode currentNodeId
        let before = this.MovesAndFenPlayed.Count
    
        // Get comment for this move if it exists
        let comment = 
          match moveToCommentMap.TryGetValue(san) with
          | true, c -> c
          | _ -> ""
    
        // Use the comment-aware method
        this.PlaySimpleShortSanWithComments san comment
        currentNodeId <- currentGraphNodeId
    
        if this.MovesAndFenPlayed.Count > before then
          lastMoveNodeId <- Some currentNodeId
        else
          // Lenient fallback for unparsable SAN
          let parts = this.FEN().Split(' ')
          if parts.Length >= 6 then
            parts[1] <- if parts[1] = "w" then "b" else "w"
            if parts[1] = "b" then
              let fm = Int32.Parse(parts[5])
              parts[5] <- string (fm + 1)
            let altFen = String.Join(" ", parts)
            this.LoadFen(altFen)
            let altHash = Utilities.Hash.hashBoard position
            let md =
              { MoveDetail.Empty with
                  LongSan = san
                  FromSq = ""
                  ToSq = ""
                  Color = if parts[1] = "w" then "b" else "w"
                  IsCastling = false
                  Comments = comment }
            let maf = { Move = md; ShortSan = san; FenAfterMove = altFen }
            moveAndFens.Add maf
            let edgeId =
              addGraphEdge
                currentNodeId
                altHash
                altFen
                san
                san
                md.Color
                false
                comment
                (if createNodeIsVariation then Some false else None)
                None
            currentGraphNodeId <- moveGraph.EdgesById[edgeId].To
            updatePathFromCurrent()
            lastMoveNodeId <- Some currentGraphNodeId

      let lastIndex = normalized.Length - 1
      let inline peek idx = if idx <= lastIndex then normalized[idx] else ""

      let sanitizeTok (t:string) =
        let mutable s = t.Trim()
        // Strip leading dots from black move markers like "...fxe4" or "...0-0"
        while s.StartsWith(".") do
          s <- s.Substring(1)
        // Normalize numeric castling notation to standard SAN.
        s <-
          match s with
          | "0-0-0" -> "O-O-O"
          | "0-0" -> "O-O"
          | _ -> s
        // If this is a standalone move number token, keep it so it can be skipped explicitly.
        if Regex.IsMatch(s, @"^\d+\.{0,3}$") |> not then
          // Remove inline move number prefixes such as "68.exf5" or "12...Qe7"
          s <- Regex.Replace(s, @"^\d+\.(\.\.)?", "")
          // Recover common PGN typos like "gff4" -> "gxf4" where the capture marker is missing.
          if s.Length >= 4 then
            let isFile c = c >= 'a' && c <= 'h'
            if isFile s[0] && isFile s[1] && s[1] <> 'x' && Char.IsLetter s[2] && Char.IsDigit s[s.Length - 1] then
              s <- $"{s[0]}x{s.Substring(2)}"
        while s.Length > 0 && (s.EndsWith("?") || s.EndsWith("!")) do
          s <- s.Substring(0, s.Length - 1)
        s

      for i = 0 to lastIndex do
        let tok = sanitizeTok normalized[i]
        match tok with
        | "(" ->
            let nextTok = peek (i + 1)
            let isAlt = nextTok.EndsWith(".") || nextTok.EndsWith("...")
            let baseNode =
              match lastMoveNodeId with
              | Some id when isAlt ->
                  let node = moveGraph.NodesById[id]
                  match node.Parents |> List.choose (fun eid -> match moveGraph.EdgesById.TryGetValue eid with | true, e -> Some e | _ -> None) with
                  | parent :: _ -> parent.From
                  | [] -> moveGraph.Root
              | Some id -> id
              | None -> moveGraph.Root
            stack.Push(currentNodeId, lastMoveNodeId, createNodeIsVariation)
            createNodeIsVariation <- true
            currentNodeId <- baseNode
            lastMoveNodeId <- Some baseNode
        | ")" ->
            if stack.Count > 0 then
              let (nodeId, lastId, flag) = stack.Pop()
              createNodeIsVariation <- flag
              currentNodeId <- nodeId
              lastMoveNodeId <- lastId
        | _ when isMoveNumber tok -> ()
        | san ->
            playSan san

      match lastMoveNodeId with
      | Some id -> currentGraphNodeId <- id; createNodeIsVariation <- false; updatePathFromCurrent()
      | None -> ()

    member this.LoadPGNGameWithVariations (pgn:PGNTypes.PgnGame) =
      // Reset to the PGN's starting position (or keep the current start if none is provided).
      let startFen = if String.IsNullOrWhiteSpace pgn.Fen then this.StartPosition else pgn.Fen
      this.ResetBoardStateFromFen(startFen)
      createNodeIsVariation <- false

      let setBoardToNode (nodeId: NodeId) =
        let fen = moveGraph.NodesById[nodeId].Fen
        this.LoadFen(fen)
        currentGraphNodeId <- nodeId

      let rec playLine (startNode: NodeId) (line: PGNTypes.PlyLine) (isMainline: bool) =
        let prevFlag = createNodeIsVariation
        setBoardToNode startNode
        createNodeIsVariation <- not isMainline
        let mutable currentNode = startNode
        for ply in line do
          let nodeBeforeMove = currentNode
          // Play the move
          let comment = if String.IsNullOrWhiteSpace ply.Comment then "" else ply.Comment
          this.PlaySimpleShortSanWithComments ply.San comment
          currentNode <- currentGraphNodeId

          // Recurse into any variations that branch from this node
          let afterMoveNode = currentNode
          for variation in ply.Variations do
            let _ = playLine nodeBeforeMove variation false
            setBoardToNode afterMoveNode
            createNodeIsVariation <- not isMainline
        createNodeIsVariation <- prevFlag
        currentNode

      let lastMainNode =
        if pgn.Mainline.Count = 0 then moveGraph.Root
        else playLine moveGraph.Root pgn.Mainline true

      for variation in pgn.RootVariations do
        playLine moveGraph.Root variation false |> ignore

      currentGraphNodeId <- lastMainNode
      createNodeIsVariation <- false
      updatePathFromCurrent()
      lastHistoryTokens <- None
    
    member this.GetMoveHistory() =      
      let fen = this.FEN()
      this.GetMoveHistoryToCurrentFen fen
    
    member this.GetMoveHistoryToCurrentFen (fen : string) =      
      let mutable priorMoves = this.MovesAndFenPlayed |> Seq.takeWhile (fun e -> e.FenAfterMove <> fen)
      let currentMove = this.MovesAndFenPlayed |> Seq.tryFind (fun e -> e.FenAfterMove = fen)
      let ply = if openingMoves.Count = 0 then game.[0].Ply |> int else openingMoves.Count
      // add current move to the prior moves
      match currentMove with
      | Some move -> priorMoves <- Seq.append priorMoves (seq { yield move })
      | None -> ()
      let sb = sbPool.Get()
      let mutable nr = ply
      let mutable moveNr = 
        if ply % 2 = 1 then
          ply / 2 + ply % 2
        else      
          ply / 2 + 1
      
      for moveStr in priorMoves do
          if moveStr.Move.Color = "w" then
            sb.Append $" {moveNr}. {moveStr.ShortSan}" |> ignore
            nr <- nr + 1
          elif moveStr.Move.Color = "b" && nr = ply then                        
            sb.Append $" {moveNr}... {moveStr.ShortSan}" |> ignore
            nr <- nr + 1
            moveNr <- moveNr + 1
          else
            sb.Append $" {moveStr.ShortSan}" |> ignore
            moveNr <- moveNr + 1
            nr <- nr + 1
      sbPool.Return(sb)
      sb.ToString().TrimStart()

    member this.GetShortSanMoveHistory() =      
      let sb = sbPool.Get()      
      let ply = if openingMoves.Count = 0 then game.[0].Ply |> int else openingMoves.Count
      let mutable white = game.[0].STM = 0uy      
      let mutable nr = ply
      let mutable moveNr = 
        if ply % 2 = 1 then
          let n = ply / 2 + ply % 2
          n
        else      
          ply / 2 + 1
      for moveStr in this.ShortSANMovesPlayed do
        if nr % 2 = 1 && not white then                        
          sb.Append $" {moveNr}... {moveStr}" |> ignore
          white <- true
          moveNr <- moveNr + 1
        elif nr % 2 = 0 then                        
          sb.Append $" {moveNr}. {moveStr}" |> ignore
        else
          sb.Append $" {moveStr}" |> ignore
          moveNr <- moveNr + 1
        nr <- nr + 1
      sbPool.Return(sb)
      sb.ToString().TrimStart()

    member this.GetOpeningMoves() =
      let sb = sbPool.Get()      
      let mutable nr = 0
      let mutable moveNr = 1
      for moveStr in this.OpeningMovesPlayed do
        if nr % 2 = 0 then                        
          sb.Append $" {moveNr}. {moveStr}" |> ignore
        else
          sb.Append $" {moveStr}" |> ignore
          moveNr <- moveNr + 1
        nr <- nr + 1
      sbPool.Return(sb)
      sb.ToString().TrimStart()        

    member this.InlineTokensFromGraph () =
      let tokens = ResizeArray<InlineMoveToken>()
      let formatTokenText ply san isLineStart inVariation =
        let moveNr = (ply / 2) + 1
        if ply % 2 = 0 then sprintf "%d. %s" moveNr san
        elif inVariation && isLineStart then sprintf "%d... %s" moveNr san
        else san

      let childEdges nodeId =
        moveGraph.NodesById[nodeId].Children
        |> List.choose (fun eid ->
            match moveGraph.EdgesById.TryGetValue eid with
            | true, e -> Some e
            | _ -> None)
        |> List.sortBy (fun e -> e.Order)

      let rec depthFrom nodeId =
        let children =
          moveGraph.NodesById[nodeId].Children
          |> List.choose (fun eid ->
              match moveGraph.EdgesById.TryGetValue eid with
              | true, e -> Some e
              | _ -> None)
        match children with
        | [] -> 0
        | _ -> children |> List.map (fun e -> 1 + depthFrom e.To) |> List.max

      let rec emitBranch (edge: MoveEdge) ply isLineStart inVariation =
        tokens.Add
          { Text = edge.San
            DisplayText = formatTokenText ply edge.San isLineStart inVariation
            Fen = moveGraph.NodesById[edge.To].Fen
            MoveCoord = edge.Lan //if edge.Lan.Length >= 4 then edge.Lan.Substring(0,4) else String.Empty
            IsBracket = false
            Hash = moveGraph.NodesById[edge.To].Hash
            FromVariation = inVariation
            Ply = ply
            Evaluation = edge.Comments
            IsLineStart = isLineStart }

        let children = childEdges edge.To
        match children with
        | [] -> ()
        | _ ->
            let mainChild =
              match children |> List.tryFind (fun e -> e.IsMainline) with
              | Some mc -> mc
              | None -> children |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))
            let variations = children |> List.filter (fun e -> e.Id <> mainChild.Id)

            let firstMoveWasWhiteAtVariationStart = isLineStart && inVariation && (ply % 2 = 0)
            let mainChildIsLineStart =
              if firstMoveWasWhiteAtVariationStart then false else (isLineStart && inVariation)

            if String.IsNullOrWhiteSpace mainChild.San |> not then
              tokens.Add
                { Text = mainChild.San
                  DisplayText = formatTokenText (ply + 1) mainChild.San mainChildIsLineStart inVariation
                  Fen = moveGraph.NodesById[mainChild.To].Fen
                  MoveCoord = mainChild.Lan
                  IsBracket = false
                  Hash = moveGraph.NodesById[mainChild.To].Hash
                  FromVariation = inVariation
                  Ply = ply + 1
                  Evaluation = mainChild.Comments
                  IsLineStart = false }

            for variation in variations do
              tokens.Add { Text = "("; DisplayText = "("; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = ply + 1; Evaluation = variation.Comments; IsLineStart = false }
              emitBranch variation (ply + 1) true true
              tokens.Add { Text = ")"; DisplayText = ")"; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = ply + 1; Evaluation = variation.Comments; IsLineStart = false }

            emitMainLine mainChild (ply + 1) inVariation (variations.Length > 0)

      and emitMainLine (edge: MoveEdge) ply inVariation isLineStart =
        let children = childEdges edge.To
        match children with
        | [] -> ()
        | _ ->
            let mainChild =
              match children |> List.tryFind (fun e -> e.IsMainline) with
              | Some mc -> mc
              | None -> children |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))
            let variations = children |> List.filter (fun e -> e.Id <> mainChild.Id)

            if String.IsNullOrWhiteSpace mainChild.San |> not then
              tokens.Add
                { Text = mainChild.San
                  DisplayText = formatTokenText (ply + 1) mainChild.San isLineStart inVariation
                  Fen = moveGraph.NodesById[mainChild.To].Fen
                  MoveCoord = mainChild.Lan
                  IsBracket = false
                  Hash = moveGraph.NodesById[mainChild.To].Hash
                  FromVariation = inVariation
                  Ply = ply + 1
                  Evaluation = mainChild.Comments
                  IsLineStart = isLineStart }

            for variation in variations do
              tokens.Add { Text = "("; DisplayText = "("; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = ply + 1; Evaluation = variation.Comments; IsLineStart = false }
              emitBranch variation (ply + 1) true true
              tokens.Add { Text = ")"; DisplayText = ")"; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = ply + 1; Evaluation = variation.Comments; IsLineStart = false }

            emitMainLine mainChild (ply + 1) inVariation (variations.Length > 0)

      let rootChildren = childEdges moveGraph.Root
      match rootChildren with
      | [] -> List.empty
      | _ ->
          let main =
            match rootChildren |> List.tryFind (fun e -> e.IsMainline) with
            | Some mc -> mc
            | None -> rootChildren |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))
          let rootVariations = rootChildren |> List.filter (fun e -> e.Id <> main.Id)
          emitBranch main 0 true false
          for variation in rootVariations do
            tokens.Add { Text = "("; DisplayText = "("; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = 0; IsLineStart = false; Evaluation = variation.Comments }
            emitBranch variation 0 true true
            tokens.Add { Text = ")"; DisplayText = ")"; Fen = ""; MoveCoord = ""; IsBracket = true; Hash = moveGraph.NodesById[variation.To].Hash; FromVariation = true; Ply = 0; IsLineStart = false; Evaluation = variation.Comments }
          tokens |> Seq.toList
   
    member this.GenerateMoves () = this.GenerateMovesFast()

    member this.GenerateMovesFast () =
      let mutable index = 0      
      let span = moveList.Value.AsSpan()
      generateCapturesInSpan span &index &position      
      generateQuietsInSpan span &index &position isFRC
      span.Slice(0, index).ToArray()

    member this.SafeMoveGenerator() =
        let span = Span<TMove>(Array.zeroCreate<TMove>(256))
        let mutable index = 0
        generateCapturesInSpan span &index &position      
        generateQuietsInSpan span &index &position this.IsFRC
        span.Slice(0, index).ToArray()
          
    member this.MakeMove (move:TMove inref) =
      game.[iPosition] <- PositionOps.copy(&position)
      iPosition <- iPosition + 1
      makeMove &move &position 
      let hash = this.PositionHash()
      this.HashKeys.Add hash
      this.MovesPlayed.Add move      
      //this.CollectStat &move

    member this.ClaimThreeFoldRep () =       
      let key = this.PositionHash()
      let keys = hashKeys |> Seq.sumBy (fun e -> if e = key then 1 else 0)
      keys >= 3
    
    member this.InsufficentMaterial() =
      let piecesLeft = PositionOps.numberOfPieces &position
      if piecesLeft > 4 then
        false
      else
        let occ = PositionOps.occupation &position
        let kings = PositionOps.kings &position
        let bishops = PositionOps.bishops &position
        let knights = PositionOps.knights &position
        if occ = (kings ||| bishops ||| knights) then          
          let bUs = bishops &&& PositionOps.sideToMove &position
          let bOpp = bishops &&& PositionOps.opposing &position
          let bUsN = QBBOperations.Pop bUs |> int32
          let bOppN = QBBOperations.Pop bOpp |> int32
          let nUs = knights &&& PositionOps.sideToMove &position
          let nOpp = knights &&& PositionOps.opposing &position
          let nUsN = QBBOperations.Pop nUs |> int32
          let nOppN = QBBOperations.Pop nOpp |> int32
          let allTest = abs (nUsN + bUsN - nOppN - bOppN) <= 1
          occ = kings 
          || (piecesLeft = 3 && kings ||| bishops = occ)  
          || (piecesLeft = 3 && kings ||| knights = occ)
          || (piecesLeft = 4 && kings ||| knights = occ)
          || (piecesLeft = 4 && allTest)
          || (piecesLeft = 4 && (kings ||| knights) = occ)
        else
          false
    
    member this.UnMakeMove () =                
      iPosition <- iPosition - 1
      position <- game.[iPosition]

    member this.PrintPosition (label:string) = 
      PositionOpsToString(label, &position) |> printfn "%s"

    member this.GetPieceAndColorOnSquare (square:string) =
      MoveGeneration.getPieceAndColorOnSquare(&position, square)
    
    // Check if the current position is a repetition or not, and set Rep field   
    member this.UpdateRepetition() = position.Rep <- this.RepetitionNr() |> byte

    member this.RepetitionNr() =
      let key = this.PositionHash()
      hashKeys |> Seq.sumBy (fun e -> if e = key then 1 else 0)     
    
    member this.GetAllLegalMoves() =      
      let moveList = this.GenerateMoves()

      seq {
        for move in moveList do
          let legal = BoardHelper.Illegal &move &position |> not
          if legal then
            let longSan = TMoveOps.moveToStr &move position.STM
            if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
              //since it could be a chess960 game we need to check if move to is to the same square as the short rook or the long rook
              let toSq = int move.To
              let rookM = PositionOps.rooksM &position
              let rookShortSq = MSB(rookM) |> int
              let rookLongSq = LSB(rookM) |> int
              if rookShortSq = toSq then
                longSan, "0-0"
              elif rookLongSq = toSq then
                longSan, "0-0-0"
              elif toSq = 2 then
                longSan, "0-0-0"
              elif toSq = 6 then
                longSan, "0-0"
            else
              let shortSan = ConvertTo.standardSAN (longSan, move, moveList, position.STM) 
              longSan,shortSan
      }
    
    member this.AnyLegalMove() =
      this.GenerateMoves ()
      |> Array.exists(fun m -> BoardHelper.Illegal &m &position |> not)      

    member this.IsMate() =
      MoveGeneration.InCheck &position <> 0UL && this.AnyLegalMove() |> not     
    
    member this.IllegalMove (move: TMove inref) =
      BoardHelper.Illegal &move &position
    
    member this.getSANFromEngineAN (move:string) = 
      let moveList = this.GenerateMoves ()
      let rec loop moves =
        match moves with
        | [] -> None
        | tmove::t -> 
          let mStr = TMoveOps.moveToStr &tmove this.Position.STM
          if (mStr.Trim().ToLower()) = move.Trim().ToLower() then
            if (tmove.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
              //since it could be a chess960 game we need to check if move to is to the same square as the short rook or the long rook
              let toSq = int tmove.To
              let rookM = PositionOps.rooksM &position
              let rookShortSq = MSB(rookM) |> int
              let rookLongSq = LSB(rookM) |> int
              if rookShortSq = toSq then
                Some "0-0"
              elif rookLongSq = toSq then
                Some "0-0-0"
              else
                None              
            else
              let san = ConvertTo.standardSAN(move, tmove, moveList, this.Position.STM)
              Some san
          else
            loop t
      loop (moveList|>Array.toList)    

    member this.FindEpMove move =      
        let islegal move = this.IllegalMove &move |> not
        let moveList = this.GenerateMoves()
        match TMoveOps.getTMoveFromShortSan move moveList position.STM islegal with
        |Some tmove -> (tmove.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY               
        |None -> false        

    member this.PlayLongSanMove move =
      let moveList = this.GenerateMoves()
      
      match TMoveOps.getTmoveFromSanMove moveList move position.STM with
      |Some tmove ->
        this.LongSANMovesPlayed.Add(move)
        let shortSan = TMoveOps.getShortSanMoveFromTmove moveList tmove position
        this.ShortSANMovesPlayed.Add(shortSan)
        this.MakeMove(&tmove)
        let fenAndMoves = 
          {
            LongSan = move
            FromSq = move[0..1]
            ToSq = move[2..3]
            Color = (if position.STM=0uy then "b" else "w")
            IsCastling = (tmove.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY 
            Comments = String.Empty
          }
        let fenAfter = this.FEN()
        let hashAfter = this.PositionHash()
        let maf = {Move=fenAndMoves; ShortSan=shortSan; FenAfterMove=fenAfter}
        moveAndFens.Add maf

        let childrenEdges =
          moveGraph.NodesById[currentGraphNodeId].Children
          |> List.choose (fun eid ->
              match moveGraph.EdgesById.TryGetValue eid with
              | true, e -> Some e
              | _ -> None)
        let existingEdge =
          childrenEdges
          |> List.tryFind (fun e ->
              e.San = shortSan &&
              (match moveGraph.NodesById.TryGetValue e.To with | true, n -> n.Fen = fenAfter | _ -> false))
        let mainChild =
          childrenEdges
          |> List.tryFind (fun e -> e.IsMainline)
          |> Option.orElseWith (fun () -> childrenEdges |> List.tryHead)
        let isVariation =
          match mainChild with
          | Some m ->
              let toFen =
                match moveGraph.NodesById.TryGetValue m.To with
                | true, n -> n.Fen
                | _ -> ""
              m.San <> shortSan || toFen <> fenAfter
          | None -> false

        let edgeId =
          match existingEdge with
          | Some e -> e.Id
          | None ->
              let isMainlineOpt = if isVariation then Some false else None
              addGraphEdge
                currentGraphNodeId
                hashAfter
                fenAfter
                shortSan
                move
                fenAndMoves.Color
                fenAndMoves.IsCastling
                fenAndMoves.Comments
                isMainlineOpt
                None
        currentGraphNodeId <-
          match moveGraph.EdgesById.TryGetValue edgeId with
          | true, e -> e.To
          | _ -> currentGraphNodeId
        updatePathFromCurrent()
      |None -> ()    

    member this.PlayOpeningMove (fromSan: string) = 
      let islegal move = this.IllegalMove &move |> not      
      let moveList = this.GenerateMoves ()
      match TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal with
      | Some move -> 
        let moveStr = TMoveOps.getSanLong move position.STM
        this.MakeMove(&move)
        openingMoves.Add fromSan
        this.LongSANMovesPlayed.Add(moveStr.Trim())
        let fenPos = BoardHelper.posToFen position
        let fenAndMoves = 
          {
            LongSan = moveStr
            FromSq = moveStr[0..1]
            ToSq = moveStr[2..3]
            Color = (if position.STM=0uy then "b" else "w")
            IsCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY 
            Comments = String.Empty
          }
        moveAndFens.Add({Move=fenAndMoves; ShortSan=fromSan; FenAfterMove=fenPos})

      | None -> failwith $"failed to parse opening move {fromSan}"

    member this.PlayPgnToPly (pgn : PGNTypes.PgnGame, lastPly : int) =
      this.ResetBoardState()
      if String.IsNullOrEmpty(pgn.Fen) |> not then         
        this.LoadFen(pgn.Fen)            
      
      for m in pgn.Mainline do
        if m.Ply <= lastPly then
          this.PlaySimpleShortSan m.San               
      let movesAndFen = this.MovesAndFenPlayed |> Seq.last
      movesAndFen

    member this.PlayPgn (pgn : PGNTypes.PgnGame, lastMove : int, color : string) =
      this.ResetBoardState()
      let fen = 
        if String.IsNullOrEmpty(pgn.Fen) then 
          Misc.startPosition
        else pgn.Fen
      this.LoadFen(fen)      
      for m in pgn.Mainline do        
        if m.Ply < lastMove then
          this.PlaySimpleShortSan m.San          
        elif m.Ply = lastMove then          
            this.PlaySimpleShortSan m.San

      let movesAndFen = this.MovesAndFenPlayed |> Seq.last
      movesAndFen
        
    member this.PlaySimpleShortSan (fromSan: string) = 
      let islegal move = this.IllegalMove &move |> not
      let moveList = this.GenerateMoves ()
      match TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal with
      | Some (move) -> 
        let moveStr = TMoveOps.getSanLong move position.STM
        this.MakeMove(&move)
        longSanMoves.Add (moveStr.Trim())
        let fenPos = BoardHelper.posToFen position
        let hashPos = this.PositionHash()
        let fenAndMoves = 
          {
            LongSan = moveStr
            FromSq = moveStr[0..1]
            ToSq = moveStr[2..3]
            Color = (if position.STM=0uy then "b" else "w")
            IsCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
            Comments = String.Empty
          }
        let maf = {Move=fenAndMoves; ShortSan=fromSan; FenAfterMove=fenPos}
        moveAndFens.Add maf
        let isMainlineOpt = if createNodeIsVariation then Some false else None
        let edgeId =
          addGraphEdge
            currentGraphNodeId
            hashPos
            fenPos
            fromSan
            moveStr
            fenAndMoves.Color
            fenAndMoves.IsCastling
            fenAndMoves.Comments
            isMainlineOpt
            None
        currentGraphNodeId <- moveGraph.EdgesById[edgeId].To
        updatePathFromCurrent()
      | None ->        
        () // keep quiet in parsing errors to avoid writing to closed TextWriter contexts
      lastHistoryTokens <- None

    member this.PlaySimpleShortSanWithComments (fromSan: string) (comments : string) = 
      let islegal move = this.IllegalMove &move |> not
      let moveList = this.GenerateMoves ()
      match TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal with
      | Some (move) ->         
        let moveStr = TMoveOps.getSanLong move position.STM
        this.MakeMove(&move)
        longSanMoves.Add (moveStr.Trim())
        let fenPos = BoardHelper.posToFen position
        let hashPos = this.PositionHash()
        let fenAndMoves = 
          {
            LongSan = moveStr
            FromSq = moveStr[0..1]
            ToSq = moveStr[2..3]
            Color = (if position.STM=0uy then "b" else "w")
            IsCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
            Comments = comments
          }
        let maf = {Move=fenAndMoves; ShortSan=fromSan; FenAfterMove=fenPos}
        moveAndFens.Add maf
        let isMainlineOpt = if createNodeIsVariation then Some false else None
        let edgeId =
          addGraphEdge
            currentGraphNodeId
            hashPos
            fenPos
            fromSan
            moveStr
            fenAndMoves.Color
            fenAndMoves.IsCastling
            comments
            isMainlineOpt
            None
        currentGraphNodeId <- moveGraph.EdgesById[edgeId].To
        updatePathFromCurrent()
      | None ->        
        () // keep quiet in parsing errors to avoid writing to closed TextWriter contexts
      lastHistoryTokens <- None

    // Method that needs to be made thread-safe
    member this.PlayPVLineThreadSafe moves fen = 
        lock lockObject (fun () -> 
          this.ResetBoardState()
          this.LoadFen fen
          for m in moves do
            this.PlayLongSanMove m
          if this.MovesAndFenPlayed.Count > 0 then
            let movesAndFen = this.MovesAndFenPlayed |> Seq.last
            movesAndFen
          else
            MoveAndFen.FirstEntry )
    
    member this.PlayPVLine (moves: string seq, fen: string) =
      this.ResetBoardState()
      this.LoadFen fen
      for m in moves do
        this.PlayLongSanMove m
      if this.MovesAndFenPlayed.Count > 0 then
        let movesAndFen = this.MovesAndFenPlayed |> Seq.last
        movesAndFen
      else
        MoveAndFen.FirstEntry

    member this.PlayCommands (fenMoves : string) =
      this.ResetBoardState()
      let (fenCmd, moves) = Utilities.FEN.parseFENandMoves fenMoves
      let fenOption = FEN.extractFEN fenCmd
      match fenOption with 
      |Some fen -> 
        this.LoadFen fen
        this.StartPosition <- fen
        this.CurrentFEN <- fen
        for m in moves do
          this.PlayLongSanMove m
      |_ -> ()      
    
    member this.PlayFenWithMoves (fenMoves : string) =
      this.ResetBoardState()
      let (fen, moves) = Utilities.FEN.parseFENandMoves fenMoves
      this.LoadFen fen      
      this.CurrentFEN <- fen
      this.StartPosition <- fen
      for m in moves do
        this.PlayLongSanMove m

module Deviation =
  
  type DeviationInput = {Board: Board; PGN: PGNTypes.PgnGame; Moves: string list }  
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

  let createDeviationSummary (moveDeviations: MoveDeviation seq) (pgn: PGNTypes.PgnGame seq) =
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
  let movesFromPgn (pgn:PGNTypes.PgnGame) =
    [
      for m in pgn.Mainline -> m.San 
    ]

  type MoveStore = {Move: string; Fen: string; White: string; Black:string; Hash: UInt64; MoveNr: int }
    with static member Empty = {Move=""; Fen=""; White=""; Black=""; Hash=0UL; MoveNr=0}

  let createMoveStore (move: string) (moveNr: int) (fen: string) (white: string) (black: string) (hash: UInt64) = 
      {Move=move; Fen=fen; White=white; Black=black; Hash=hash; MoveNr=moveNr}

  type GameStore = {Moves: MoveStore list; Game: PGNTypes.PgnGame; Board: Board; Opening : string }

  let createGameStore (moves: MoveStore list) (pgn: PGNTypes.PgnGame) (board: Board) (opening : string ) = 
    {Moves=moves; Game=pgn; Board=board ; Opening = opening}

  type ReplayDataExtended = 
    { Engine:string
      Move: string * string
      TimeLeftInMs: int64
      FirstGame: PGNTypes.PgnGame
      SecondGame: PGNTypes.PgnGame
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
  
  let findAllDeviationsForPlayers (pgnGames: PGNTypes.PgnGame seq) (refPlayer: string option) (comparePlayers: string list option ) =    
    let replayBoard = Board()
    let oppBoard = Board()
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
          allGames |> List.iter(fun game -> Utilities.Hash.writeOpeningHashToPgnGame game)
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
                    replayBoard.PlaySimpleShortSan lastmove
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
                            SecondGame = PGNTypes.PgnGame.Empty game.GameNumber
                            Fen1 = oldFen }
                        dict[hash] <- data
                      |Some replayData ->       
                        let (sSan,_) = replayData.Move
                        if sSan <> lastmove then                                                    
                          oppBoard.LoadFen replayData.Fen1
                          oppBoard.PlaySimpleShortSan sSan
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
                            SecondGame = PGNTypes.PgnGame.Empty game.GameNumber
                            Fen1 = oldFen }                        
                        dict[hash] <- data
                      |Some replayData ->
                        let (sSan,_) = replayData.Move
                        if sSan <> lastmove then
                          oppBoard.LoadFen replayData.Fen1
                          oppBoard.PlaySimpleShortSan sSan
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

  let findAllDeviationsForAllPlayers (pgnGames: PGNTypes.PgnGame seq) =    
      let res =
        try
            findAllDeviationsForPlayers (pgnGames |> Seq.toList) None None
        with
        | ex ->            
            printfn "Exception in findAllDeviationsForPlayers: %s" ex.Message
            Seq.empty
      res

  let findAllDeviationsForPlayersAlt (pgnGames: PGNTypes.PgnGame seq) (refPlayer: string option) (comparePlayers: string list ) =
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
        allGames |> List.iter(fun game -> Utilities.Hash.writeOpeningHashToPgnGame game)
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
          let replayBoard = new Board()
          let mutable pos = replayBoard.Position
          if game.Fen <> "" then
            replayBoard.LoadFen game.Fen                      
          
          let mutable idx = 0
          let moves = movesFromPgn game
          for m in moves do             
            idx <- idx + 1
            replayBoard.PlaySimpleShortSan m
            let hash = replayBoard.DeviationHash()
            let fen = replayBoard.FEN()
            let longMove = replayBoard.LongSANMovesPlayed.[replayBoard.LongSANMovesPlayed.Count - 1]
            let moveStore = createMoveStore longMove idx fen game.GameMetaData.White game.GameMetaData.Black hash
            gameMoveStore.Add moveStore
          
          let moveStore = gameMoveStore |> Seq.toList
          gameStore.Add (createGameStore moveStore game replayBoard openingHash)
    gameStore
  

  // Define a record to hold details of a deviating game.
  type DeviationDetail = {
      ReferenceGame: PGNTypes.PgnGame
      DevGame : PGNTypes.PgnGame
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

  let getMoveDifferences (games: PGNTypes.PgnGame seq) compareList  refPlayer =
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
  
  let findMoveDifferencesInPGN (pgn: PGNTypes.PgnGame seq) refPlayer compareList =    
    let compareList = compareList |> Seq.toList
    let res = 
      getMoveDifferences pgn compareList refPlayer
      |> Seq.map mapDevDetailToMoveDeviation
      |> Seq.distinctBy(fun e -> e.Round)
      |> Seq.sortBy(fun e -> e.Round)
      |> Seq.truncate 100
    res
  
  let returnListOfPositionsToCheck (fen:string) (list: string ResizeArray) =
      let movesToCheck = ResizeArray<Puzzle.Position>()
      let mutable moves = ""
      list 
      |> Seq.iteri(fun idx el ->            
            moves <- 
            if idx % 2 = 1 then 
              let puzzle = $"position fen {fen} moves {moves}" 
              let pos : Puzzle.Position = {Command=puzzle; CorrectMove = list[idx]; MovePlayed = ""}
              movesToCheck.Add pos 
            if idx = 0 then 
              sprintf "%s" el
            else
              $"{moves} {el}" )
      movesToCheck
  
  let analyzeDeviations (pgnGames: PGNTypes.PgnGame seq) =
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
    let longest = summary |> Seq.maxBy (fun e -> e.Player.Length) |> fun e -> (e.Player.Length + 2)
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

module BoardUtils =
  
  let getSanNotationFromTMove (board:Board inref) (move:TMove) =    
    let moveList = board.GenerateMoves ()
    TMoveOps.getShortSanMoveFromTmove moveList move (board.Position)

  let tryGetTMoveFromCoordinateNotation (board:Board inref) (moveLong:string) =
    if moveLong.Length < 4 then
      None
    else      
      let moveList = board.GenerateMoves ()
      let stm = board.Position.STM 
      let r =
        moveList
        |> Array.tryFind(fun m -> TMoveOps.getSanLong m stm = moveLong)
      r

  let getShortSanFromLongSan (board:Board inref) longSan =
    match tryGetTMoveFromCoordinateNotation &board longSan with
    |Some move -> getSanNotationFromTMove &board move      
    |None -> ""

  let tryGetTMoveFromCoordinateNotationSimple moveList (position:Position byref) (moveLong:string) =
    if moveLong.Length < 4 then
      None
    else
      let stm = position.STM 
      moveList
      |> Array.tryFind(fun m -> TMoveOps.getSanLong m stm = moveLong)

  
  let getLongSanPVFromShortSanPV  moveList (board:Board inref) (shortSanMoves : string seq) =
        let isFRC = board.IsFRC
        let mutable position = board.Position
        let ret = ResizeArray<string>(shortSanMoves |> Seq.length)
        for m in shortSanMoves do
          let mutable index = 0
          generateCaptures &(moveList) &index &position      
          generateQuiets &(moveList) &index &position isFRC
          let moves = moveList[0..index-1]
          let islegal move = BoardHelper.Illegal &move &position |> not
          
          // Try SAN format first
          let moveResult = 
            match TMoveOps.getTMoveFromShortSan m moves position.STM islegal with
            | Some tmove -> Some tmove
            | None ->
                // Try coordinate notation (some Winboard engines use this)
                if m.Length >= 4 && m.Length <= 5 then
                  // Create temporary board to test coordinate notation
                  let tempBoard = Board()
                  tempBoard.LoadFen(BoardHelper.posToFen position)
                  tempBoard.IsFRC <- isFRC
                  tryGetTMoveFromCoordinateNotation &tempBoard m
                else
                  None
          
          match moveResult with
          | Some tmove -> 
            let moveStr = TMoveOps.getSanLong tmove position.STM            
            ret.Add(moveStr)
            makeMove &tmove &position
          | None -> 
            // Stop processing PV on first error to avoid corrupted position
            ()
        //return the long san moves as a string
        String.concat " " (ret |> Seq.toArray)
  
  let getShortSanPVFromLongSanPVFast moveList (board:Board inref) (pv:string) =    
    let isFRC = board.IsFRC
    let mutable position = board.Position
    let mutable plyCount = board.PlyCount
    let start = board.PlyCount
    let allMoves = pv.Split(' ')
    let ret = ResizeArray<string>(allMoves.Length)
    for move in allMoves do
      let mutable index = 0
      generateCaptures &(moveList) &index &position      
      generateQuiets &(moveList) &index &position isFRC
      let moves = moveList[0..index-1]
      match tryGetTMoveFromCoordinateNotationSimple moves &position move with
      | Some tmove ->
        let moveNr = plyCount / 2 + 1
        let san = TMoveOps.getShortSanMoveFromTmove moves tmove position
        if san <> "" then
          if plyCount % 2 = 1 then
            //black move
            if plyCount = start then
              ret.Add(sprintf "%d.... %s" moveNr san)
            else
              ret.Add(san)
          else
            let mStr = sprintf "%d.%s" moveNr san
            ret.Add(mStr)
          makeMove &tmove &position
          plyCount <- plyCount + 1
        
      |_ -> () //printfn $"{nameof getSanNotationFromTMove}: failed to parse move {move}"
    String.concat " " (ret |> Seq.toArray)

  let makeShortSan (moves: NNValues seq) (board: Board inref) =
    for nnMove in moves do
      match tryGetTMoveFromCoordinateNotation &board nnMove.LANMove with
      |Some tmove -> 
        nnMove.SANMove <- getSanNotationFromTMove &board tmove
      |None ->         
        if nnMove.LANMove.Trim() = "e1a1" then nnMove.SANMove <- "0-0-0"
        elif nnMove.LANMove.Trim() = "e8a8" then nnMove.SANMove <- "0-0-0"
        elif nnMove.LANMove.Trim() = "e1h1" then nnMove.SANMove <- "0-0"
        elif nnMove.LANMove.Trim() = "e8h8" then nnMove.SANMove <- "0-0"
        //elif nnMove.LANMove.Trim() = "e8g8" then nnMove.SANMove <- "0-0" 
        //elif nnMove.LANMove.Trim() = "e1g1" then nnMove.SANMove <- "0-0"
        //elif nnMove.LANMove.Trim() = "e8c8" then nnMove.SANMove <- "0-0-0"
        //elif nnMove.LANMove.Trim() = "e1c1" then nnMove.SANMove <- "0-0-0"
        
        let transformedLanMove = 
            let trim = nnMove.LANMove.Trim().ToLower()
            match trim with
            | "e1a1" -> "e1c1"
            | "e8a8" -> "e8c8"
            | "e1h1" -> "e1g1"
            | "e8h8" -> "e8g8"
            |_ -> trim
        match tryGetTMoveFromCoordinateNotation &board transformedLanMove with
        |Some _ -> 
            nnMove.LANMove <- transformedLanMove
        |None -> ()    

  let makeRandomMove (rnd:Random) (board: Board inref) =
      let position = board.Position
      let mutable index = 0      
      let moveList = 
        board.GenerateMoves ()
        |> Array.filter(fun m -> BoardHelper.Illegal &m &position |> not)
      let mutable move = moveList.[rnd.Next(0,index)]
      if moveList.Length = 0 then
        printfn "No legal moves available"
      else
        board.MakeMove &move
      move
  
  
    
