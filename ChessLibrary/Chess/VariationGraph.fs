namespace ChessLibrary

open System
open System.Collections.Generic
open System.Text
open GameGraphTypes
open EngineTypes

/// The move graph behind a Board: every move played is an edge to a NEW node (a transposition
/// never merges two lines, so the graph is a tree and every node but the root has one parent),
/// the nodes indexed by position hash for lookup, and a cursor - the node the board is on. The
/// children of a node are kept newest first; their Order field says how they are shown, and one of
/// them is the mainline.
///
/// This is the graph code that used to be ~25 closures inside `type Board`, unchanged in what it
/// does, in one place with a name.
[<Sealed>]
type internal VariationGraph() =
  let mutable nodeCounter = 0
  let mutable edgeCounter = 0
  let mutable nodes = Dictionary<NodeId, PositionNode>()
  let mutable edges = Dictionary<EdgeId, MoveEdge>()
  let mutable byHash = Dictionary<uint64, ResizeArray<NodeId>>()
  let mutable root = 0
  let mutable current = 0

  let nextNodeId () =
    nodeCounter <- nodeCounter + 1
    nodeCounter

  let nextEdgeId () =
    edgeCounter <- edgeCounter + 1
    edgeCounter

  /// The edges of the ids that still exist, in the order given.
  let edgesOf (ids: EdgeId list) =
    ids |> List.choose (fun eid ->
      match edges.TryGetValue eid with
      | true, e -> Some e
      | _ -> None)

  let indexByHash (node: PositionNode) =
    match byHash.TryGetValue node.Hash with
    | true, ids -> ids.Add node.Id
    | _ ->
      let ids = ResizeArray<NodeId>()
      ids.Add node.Id
      byHash.[node.Hash] <- ids

  let register (node: PositionNode) =
    nodes.[node.Id] <- node
    indexByHash node

  let unindexByHash (node: PositionNode) =
    match byHash.TryGetValue node.Hash with
    | true, ids ->
        ids.Remove node.Id |> ignore
        if ids.Count = 0 then byHash.Remove node.Hash |> ignore
    | _ -> ()

  let unlinkEdge edgeId fromId toId =
    match nodes.TryGetValue fromId with
    | true, parent -> nodes.[fromId] <- { parent with Children = parent.Children |> List.filter (fun id -> id <> edgeId) }
    | _ -> ()
    match nodes.TryGetValue toId with
    | true, child -> nodes.[toId] <- { child with Parents = child.Parents |> List.filter (fun id -> id <> edgeId) }
    | _ -> ()

  /// Renumbers the Order of a node's children 0, 1, 2 ... keeping their order.
  let reindexChildOrders parentId =
    match nodes.TryGetValue parentId with
    | true, parent ->
        parent.Children
        |> edgesOf
        |> List.sortBy (fun e -> e.Order)
        |> List.iteri (fun idx e ->
            match edges.TryGetValue e.Id with
            | true, edge when edge.Order <> idx -> edges.[e.Id] <- { edge with Order = idx }
            | _ -> ())
    | _ -> ()

  let rec removeEdgeSubtree edgeId =
    match edges.TryGetValue edgeId with
    | true, edge ->
        let childId = edge.To
        match nodes.TryGetValue childId with
        | true, childNode ->
            // Descendants first.
            childNode.Children |> List.iter removeEdgeSubtree
            unlinkEdge edgeId edge.From childId
            edges.Remove edgeId |> ignore
            // An orphaned child that is not the root goes too.
            match nodes.TryGetValue childId with
            | true, updatedChild when List.isEmpty updatedChild.Parents && childId <> root ->
                updatedChild.Children |> List.iter removeEdgeSubtree
                unindexByHash updatedChild
                nodes.Remove childId |> ignore
            | _ -> ()
            reindexChildOrders edge.From
        | _ -> edges.Remove edgeId |> ignore
    | _ -> ()

  /// Children in display order.
  let childEdgesOf nodeId = nodes.[nodeId].Children |> edgesOf |> List.sortBy (fun e -> e.Order)

  let rec depthFrom nodeId =
    match childEdgesOf nodeId with
    | [] -> 0
    | children -> children |> List.map (fun e -> 1 + depthFrom e.To) |> List.max

  // ── Building ───────────────────────────────────────────────────────────────────────────────────

  /// A new graph with only the root, the cursor on it.
  member _.Reset(rootFen: string, rootHash: uint64) =
    nodeCounter <- 0
    edgeCounter <- 0
    nodes <- Dictionary<NodeId, PositionNode>()
    edges <- Dictionary<EdgeId, MoveEdge>()
    byHash <- Dictionary<uint64, ResizeArray<NodeId>>()
    let rootNode = { Id = nextNodeId (); Hash = rootHash; Fen = rootFen; Parents = []; Children = [] }
    register rootNode
    root <- rootNode.Id
    current <- rootNode.Id

  /// A move from `fromId` to a new node. Without `isMainlineOpt` the move is the mainline when
  /// the node has none yet; without `orderOpt` it goes after the existing children.
  member _.AddEdge(fromId: NodeId, toHash: uint64, toFen: string, san: string, lan: string, color: string,
                   isCastling: bool, comments: string, isMainlineOpt: bool option, orderOpt: int option) =
    let fromNode = nodes.[fromId]
    let toNode = { Id = nextNodeId (); Hash = toHash; Fen = toFen; Parents = []; Children = [] }
    register toNode
    let children = fromNode.Children |> edgesOf
    let hasMainline = children |> List.exists (fun e -> e.IsMainline)
    let order = defaultArg orderOpt children.Length
    let isMainline = defaultArg isMainlineOpt (not hasMainline)
    let eid = nextEdgeId ()
    let edge =
      { Id = eid; From = fromId; To = toNode.Id; San = san; Lan = lan; Comments = comments; Color = color
        IsCastling = isCastling; Order = order; IsMainline = isMainline }
    edges.[eid] <- edge
    nodes.[fromId] <- { fromNode with Children = edge.Id :: fromNode.Children }
    nodes.[toNode.Id] <- { toNode with Parents = edge.Id :: toNode.Parents }
    edge.Id

  // ── Reading ────────────────────────────────────────────────────────────────────────────────────

  member _.Root = root
  member _.Current with get () = current and set v = current <- v
  member _.Node(id: NodeId) = nodes.[id]
  member _.HasNode(id: NodeId) = nodes.ContainsKey id
  member _.Edge(id: EdgeId) = edges.[id]
  member _.TryEdge(id: EdgeId) = match edges.TryGetValue id with | true, e -> Some e | _ -> None
  member _.EdgesOf(ids: EdgeId list) = edgesOf ids
  member _.ChildEdges(nodeId: NodeId) = childEdgesOf nodeId

  /// The mainline child of a node, else its first child in display order.
  member _.MainChildEdgeId(nodeId: NodeId) =
    let children = childEdgesOf nodeId
    children
    |> List.tryFind (fun e -> e.IsMainline)
    |> Option.orElseWith (fun () -> children |> List.tryHead)
    |> Option.map (fun e -> e.Id)

  /// The mainline child, else the child with the deepest line (the earliest on a tie).
  member _.SelectMainChild(children: MoveEdge list) =
    match children |> List.tryFind (fun e -> e.IsMainline) with
    | Some mc -> mc
    | None -> children |> List.maxBy (fun e -> (depthFrom e.To, -e.Order))

  /// The edge a node is reached by: its parent edge first in display order.
  member _.ParentEdge(nodeId: NodeId) =
    match nodes.[nodeId].Parents |> edgesOf with
    | [] -> None
    | parents -> Some (parents |> List.sortBy (fun e -> e.Order) |> List.head)

  /// The edges from the root to a node, root first.
  member _.PathTo(nodeId: NodeId) =
    let visited = HashSet<NodeId>()
    let rec loop id acc =
      if visited.Contains id then acc
      else
        visited.Add id |> ignore
        match nodes.[id].Parents |> edgesOf with
        | [] -> acc
        | parents ->
            let chosen = parents |> List.sortBy (fun e -> e.Order) |> List.head
            loop chosen.From (chosen :: acc)
    loop nodeId []

  /// Moves the cursor to a node with this position: among those with the FEN (else any with the
  /// hash), a child of the cursor if there is one, else the newest. True when a node was found;
  /// otherwise the cursor goes to the root.
  member _.MoveCursorToPosition(fen: string, hash: uint64) =
    match byHash.TryGetValue hash with
    | true, ids when ids.Count > 0 ->
        let matchingFen = ids |> Seq.filter (fun id -> nodes.[id].Fen = fen) |> Seq.toList
        let candidates = if matchingFen.Length > 0 then matchingFen else ids |> Seq.toList
        let childOfCurrent =
          candidates
          |> List.tryFind (fun id ->
              nodes.[id].Parents
              |> List.exists (fun eid ->
                  match edges.TryGetValue eid with
                  | true, e -> e.From = current
                  | _ -> false))
        current <- (match childOfCurrent with Some id -> id | None -> candidates |> List.max)
        true
    | _ ->
        current <- root
        false

  // ── Editing ────────────────────────────────────────────────────────────────────────────────────

  /// Makes `mainEdgeId` the mainline among its siblings and puts it first.
  member _.SetMainChild(parentId: NodeId, mainEdgeId: EdgeId) =
    match nodes.TryGetValue parentId with
    | true, parent ->
        parent.Children
        |> edgesOf
        |> List.sortBy (fun e -> e.Order)
        |> List.sortBy (fun e -> if e.Id = mainEdgeId then 0 else 1)
        |> List.iteri (fun idx e ->
            let isMain = e.Id = mainEdgeId
            edges.[e.Id] <- if e.IsMainline <> isMain || e.Order <> idx then { e with IsMainline = isMain; Order = idx } else e)
    | _ -> ()

  member _.RemoveEdgeSubtree(edgeId: EdgeId) = removeEdgeSubtree edgeId
  member _.ReindexChildOrders(parentId: NodeId) = reindexChildOrders parentId

  /// A move's comment, on the edge itself.
  member _.SetEdgeComment(edgeId: EdgeId, comment: string) =
    edges.[edgeId] <- { edges.[edgeId] with Comments = comment }

  /// The edge a variation operation names by its SAN and the FEN it leads to: the first matching
  /// edge into a node with that FEN, else that node's first parent edge.
  member _.FindVariationEdge(san: string, fen: string, hash: uint64) =
    let matchingNodes =
      match byHash.TryGetValue hash with
      | true, ids when ids.Count > 0 -> ids |> Seq.filter (fun id -> nodes.[id].Fen = fen) |> Seq.toList
      | _ -> []
    let tryFindEdge nodeId =
      nodes.[nodeId].Parents
      |> edgesOf
      |> List.tryFind (fun e -> String.IsNullOrWhiteSpace san || e.San.Equals(san, StringComparison.OrdinalIgnoreCase))
    matchingNodes
    |> List.tryPick tryFindEdge
    |> Option.orElseWith (fun () ->
        match matchingNodes with
        | nodeId :: _ when not nodes.[nodeId].Parents.IsEmpty ->
            match edges.TryGetValue (List.head nodes.[nodeId].Parents) with
            | true, e -> Some e
            | _ -> None
        | _ -> None)

  /// A GUI entry for a move: the move, its squares and colour, and the FEN after it.
  static member MoveAndFenOf(edge: MoveEdge, toNode: PositionNode) =
    let lan = edge.Lan
    let fromSq = if lan.Length >= 2 then lan.Substring(0, 2) else ""
    let toSq = if lan.Length >= 4 then lan.Substring(2, 2) else ""
    { Move = MoveDetail.Create(lan, fromSq, toSq, edge.Color, edge.IsCastling, edge.Comments)
      ShortSan = edge.San
      FenAfterMove = toNode.Fen }

/// The graph as text: the inline token stream the move list is drawn from, PGN movetext with the
/// variations in brackets, and every line from the root to a leaf.
module internal VariationText =

  let inlineTokens (graph: VariationGraph) =
    let tokens = ResizeArray<InlineMoveToken>()

    // the half-moves before the root, from its FEN: move 30 with Black to move starts 30...
    let basePly =
      let parts = (graph.Node graph.Root).Fen.Split(' ', StringSplitOptions.RemoveEmptyEntries)
      let fullMove = if parts.Length >= 6 then (match Int32.TryParse parts.[5] with | true, n when n > 0 -> n | _ -> 1) else 1
      (fullMove - 1) * 2 + (if parts.Length >= 2 && parts.[1] = "b" then 1 else 0)
    let numberOf ply = (basePly + ply) / 2 + 1
    let isWhite ply = (basePly + ply) % 2 = 0

    // a Black move is numbered where a line starts: a variation, or the game itself
    let formatTokenText ply san isLineStart inVariation =
      let moveNr = numberOf ply
      if isWhite ply then sprintf "%d. %s" moveNr san
      elif isLineStart && (inVariation || ply = 0) then sprintf "%d... %s" moveNr san
      else san

    let addMoveToken (edge: MoveEdge) ply isLineStart inVariation =
      let toNode = graph.Node edge.To
      tokens.Add
        { Text = edge.San
          DisplayText = formatTokenText ply edge.San isLineStart inVariation
          Fen = toNode.Fen
          MoveCoord = edge.Lan
          IsBracket = false
          Hash = toNode.Hash
          FromVariation = inVariation
          Ply = ply
          MoveNumber = numberOf ply
          IsWhite = isWhite ply
          Evaluation = edge.Comments
          IsLineStart = isLineStart }

    let addVariationBracket text (edge: MoveEdge) ply =
      tokens.Add
        { Text = text
          DisplayText = text
          Fen = ""
          MoveCoord = ""
          IsBracket = true
          Hash = (graph.Node edge.To).Hash
          FromVariation = true
          Ply = ply
          MoveNumber = numberOf ply
          IsWhite = isWhite ply
          Evaluation = edge.Comments
          IsLineStart = false }

    let rec emitLine (edge: MoveEdge) ply isLineStart inVariation emitCurrentMove =
      if emitCurrentMove && not (String.IsNullOrWhiteSpace edge.San) then
        addMoveToken edge ply isLineStart inVariation
      match graph.ChildEdges edge.To with
      | [] -> ()
      | children ->
          let mainChild = graph.SelectMainChild children
          let variations = children |> List.filter (fun e -> e.Id <> mainChild.Id)
          let mainChildIsLineStart =
            if emitCurrentMove then
              // A variation that starts with a White move numbers only that move.
              let firstMoveWasWhiteAtVariationStart = isLineStart && inVariation && (ply % 2 = 0)
              if firstMoveWasWhiteAtVariationStart then false else (isLineStart && inVariation)
            else isLineStart
          if not (String.IsNullOrWhiteSpace mainChild.San) then
            addMoveToken mainChild (ply + 1) mainChildIsLineStart inVariation
          for variation in variations do
            addVariationBracket "(" variation (ply + 1)
            emitLine variation (ply + 1) true true true
            addVariationBracket ")" variation (ply + 1)
          emitLine mainChild (ply + 1) (variations.Length > 0) inVariation false

    match graph.ChildEdges graph.Root with
    | [] -> List.empty
    | rootChildren ->
        let main = graph.SelectMainChild rootChildren
        let rootVariations = rootChildren |> List.filter (fun e -> e.Id <> main.Id)
        if not (String.IsNullOrWhiteSpace main.San) then addMoveToken main 0 true false
        for variation in rootVariations do
          addVariationBracket "(" variation 0
          emitLine variation 0 true true true
          addVariationBracket ")" variation 0
        emitLine main 0 (rootVariations.Length > 0) false false
        tokens |> Seq.toList

  /// PGN movetext of the whole graph: "1. e4 e5 2. Nf3 (2. Nc3) Nc6", comments in braces.
  let movetext (graph: VariationGraph) =
    let sb = StringBuilder()
    let mutable depth = 0
    let mutable lastNumber : int option = None
    for tok in inlineTokens graph do
      match tok.IsBracket, tok.Text with
      | true, "(" ->
          if sb.Length > 0 && sb.[sb.Length - 1] <> ' ' then sb.Append(' ') |> ignore
          sb.Append('(') |> ignore
          depth <- depth + 1
      | true, ")" ->
          sb.Append(')') |> ignore
          depth <- Math.Max(0, depth - 1)
      | _ ->
          let moveNr = tok.MoveNumber
          let prefix =
            if tok.IsWhite then
              lastNumber <- Some moveNr
              sprintf "%d. " moveNr
            elif depth > 0 then
              if tok.IsLineStart || lastNumber <> Some moveNr then
                lastNumber <- Some moveNr
                sprintf "%d... " moveNr
              else ""
            elif tok.IsLineStart then sprintf "%d... " moveNr
            else ""
          if sb.Length > 0 && sb.[sb.Length - 1] <> '(' && sb.[sb.Length - 1] <> ' ' then sb.Append(' ') |> ignore
          let withComment =
            if String.IsNullOrWhiteSpace tok.Evaluation then tok.Text
            elif tok.Evaluation.StartsWith("{") then sprintf "%s %s" tok.Text tok.Evaluation
            else sprintf "%s {%s}" tok.Text tok.Evaluation
          sb.Append(prefix).Append(withComment) |> ignore
    sb.ToString().Trim()

  /// Every line from the root to a leaf, mainline first, as SAN (or UCI) with comments.
  let lines (graph: VariationGraph) (asLAN: bool) =
    let moveText (move: MoveEdge) =
      let baseText = if asLAN then move.Lan else move.San
      if String.IsNullOrWhiteSpace baseText then None
      elif String.IsNullOrWhiteSpace move.Comments then Some baseText
      else Some (baseText + " {" + move.Comments + "}")
    // Paths are built newest-first and reversed once at a leaf, instead of appending to a list at
    // every move.
    let push (reversedPath: string list) move =
      match moveText move with
      | Some t -> t :: reversedPath
      | None -> reversedPath
    let rec traverse nodeId reversedPath =
      match graph.ChildEdges nodeId with
      | [] -> [ List.rev reversedPath ]
      | children ->
          let main = graph.SelectMainChild children
          let mainPaths = traverse main.To (push reversedPath main)
          let variationPaths =
            children
            |> List.filter (fun e -> e.Id <> main.Id)
            |> List.collect (fun v -> traverse v.To (push reversedPath v))
          mainPaths @ variationPaths
    match graph.ChildEdges graph.Root with
    | [] -> []
    | rootChildren ->
        let main = graph.SelectMainChild rootChildren
        let rootVariations = rootChildren |> List.filter (fun e -> e.Id <> main.Id)
        let mainPaths = traverse main.To (push [] main)
        let variationPaths = rootVariations |> List.collect (fun v -> traverse v.To (push [] v))
        mainPaths @ variationPaths
