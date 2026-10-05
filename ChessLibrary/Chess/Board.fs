module ChessLibrary.Chess

open System
open System.Threading
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open QBBOperations
open MiscTypes
open PositionTypes
open MoveTypes
open EngineTypes
open GameGraphTypes
open RuntimeUtilities
open ChessUtilities
open MoveGeneration
open MoveParser

let startPos = startPosition

let [<Literal>] MAX_PLY = 1000
let [<Literal>] MAX_MOVES = 256

let moveList = new ThreadLocal<TMove array> (fun () -> Array.zeroCreate<TMove>(MAX_MOVES))

/// A chess board with its game: the position and the positions before it, the moves played in
/// the forms callers want (TMove, UCI, SAN, the GUI's MoveAndFen, the position hashes), and every
/// line tried, in a move graph with a cursor (VariationGraph).
///
/// Three kinds of state, kept apart:
/// - the POSITION and its undo stack (`game`, grown as needed - it used to be a fixed 1,000
///   entries that a long game with variations ran past), plus the hash of every position a move
///   reached (`hashKeys`), for repetitions;
/// - the GRAPH of lines, with the cursor;
/// - the LINE: MovesAndFenPlayed and UciMovesPlayed describe the path from the graph's root to the
///   cursor. They are public, mutable lists that callers also add to and clear, so after a graph
///   move they are brought back to the path - by one entry when the cursor moved one move forward
///   or back along it, in full only when a caller changed the lists or the cursor jumped. (They
///   used to be rebuilt in full after every move: 28 KB a move at move 60, 57 KB at move 150.)
///
/// The public surface is pinned by TestProject/BoardApiSurfaceTests.fs and the behaviour by
/// BoardCharacterizationTests. The rewrite (branch rewrite/board-fs, 2026-09) was held to the
/// board it replaced by a differential test over random operation sequences, removed once it was
/// merged (it ran against a frozen copy of the old board; see commit ea09382 to bring it back).
/// QUIRK marks behaviour kept on purpose.
type Board() =
    // ── The position ─────────────────────────────────────────────────────────────────────────────
    let mutable isFRC = false
    let mutable startFen = ""
    let mutable mostCurrentFEN = ""
    let mutable captures = 0L
    let mutable castles = 0L
    let mutable eps = 0L
    /// The undo stack: game.[i] is the position before the i-th MakeMove since the last reset.
    let mutable iPosition = 0
    let mutable game = Array.init MAX_PLY (fun _ -> Position.Default)
    let mutable position = game.[0]
    /// The hash of every position reached by MakeMove, for repetitions.
    let mutable hashKeys = ResizeArray<uint64>()
    /// The position the game started from, for repetitions (hashKeys never holds it).
    let mutable rootPositionHash = 0UL
    let lockObject = obj ()

    // ── The move lists ───────────────────────────────────────────────────────────────────────────
    let uciMoves = ResizeArray<string>()
    let sanMoves = ResizeArray<string>()
    let openingMoves = ResizeArray<string>()
    let moveAndFens = ResizeArray<MoveAndFen>()

    // ── The graph and the line to its cursor ─────────────────────────────────────────────────────
    let graph = VariationGraph()
    /// Moves played by PlaySanMove go in as variations (while a PGN variation is loaded).
    let mutable createNodeIsVariation = false
    /// The moves the last PGN load could not play ("21... Bf7"), main line and variations.
    let skippedPgnMoves = ResizeArray<string>()
    // What moveAndFens / uciMoves were last set to, entry for entry, with the node and position
    // hash each move reached, and the node the line ends at. `lineValid` is false until the lists
    // have been built from the graph since it last changed shape.
    let lineMaf = ResizeArray<MoveAndFen>()
    let lineUci = ResizeArray<string>()
    let lineNodes = ResizeArray<NodeId>()
    let lineHashes = ResizeArray<uint64>()
    let mutable lineEnd = 0
    let mutable lineValid = false

    let colorOfPlayerWhoJustMoved (stm: byte) = if stm = 0uy then "b" else "w"

    /// The history array large enough to write index `i`.
    let ensureHistory (i: int) =
      if i >= game.Length then
        let grown = Array.init (max (i + 1) (game.Length * 2)) (fun _ -> Position.Default)
        Array.blit game 0 grown 0 game.Length
        game <- grown

    /// The public lists still hold exactly what the line last put there: nobody added, removed or
    /// replaced an entry since. Reference comparisons, no allocation.
    let listsHoldLine () =
      lineValid
      && moveAndFens.Count = lineMaf.Count
      && uciMoves.Count = lineUci.Count
      && (let mutable same = true
          let mutable i = 0
          while same && i < lineMaf.Count do
            same <- obj.ReferenceEquals(moveAndFens.[i], lineMaf.[i]) && obj.ReferenceEquals(uciMoves.[i], lineUci.[i])
            i <- i + 1
          same)

    let appendToLine (edge: MoveEdge) (toNode: PositionNode) =
      let maf = VariationGraph.MoveAndFenOf(edge, toNode)
      moveAndFens.Add maf
      uciMoves.Add edge.Lan
      lineMaf.Add maf
      lineUci.Add edge.Lan
      lineNodes.Add toNode.Id
      lineHashes.Add toNode.Hash

    let truncateLine (length: int) =
      let cut (xs: ResizeArray<'a>) = if xs.Count > length then xs.RemoveRange(length, xs.Count - length)
      cut moveAndFens; cut uciMoves; cut lineMaf; cut lineUci; cut lineNodes; cut lineHashes

    /// MovesAndFenPlayed and UciMovesPlayed become the path from the root to the cursor.
    let updatePathFromCurrent () =
      let target = graph.Current
      let intact = listsHoldLine ()
      if intact && target = lineEnd then ()
      elif intact && (match graph.ParentEdge target with Some e -> e.From = lineEnd | None -> false) then
        // One move forward.
        appendToLine (graph.ParentEdge target).Value (graph.Node target)
      elif intact && target = graph.Root then truncateLine 0
      elif intact && lineNodes.Contains target then
        // Back along the line.
        truncateLine (lineNodes.IndexOf target + 1)
      else
        moveAndFens.Clear(); uciMoves.Clear()
        lineMaf.Clear(); lineUci.Clear(); lineNodes.Clear(); lineHashes.Clear()
        for e in graph.PathTo target do appendToLine e (graph.Node e.To)
      lineEnd <- target
      lineValid <- true

    /// The graph changed shape (a line removed or promoted, a reset): the next update rebuilds.
    let invalidateLine () = lineValid <- false

    /// The hash keys are the positions of the line to the cursor - true after moves played
    /// through the graph, false once positions were added outside it (tournament moves through
    /// MakeMove, book moves, a probe taken back with UndoMove). Only then may moving the cursor
    /// take them along. Compared against the line's own hashes, not the public lists: the GUI
    /// edits those (a comment on the last move, EndCurrentVariation) without changing a position,
    /// and the keys must not stop following for the rest of the session because of it.
    let hashesFollowLine () =
      lineValid
      && lineEnd = graph.Current
      && hashKeys.Count = lineHashes.Count
      && (let mutable same = true
          let mutable i = 0
          while same && i < lineHashes.Count do
            same <- hashKeys.[i] = lineHashes.[i]
            i <- i + 1
          same)

    /// After the cursor moved: the hash keys follow the line, when they did before. Without this
    /// a line taken back kept counting towards threefold (Play vs Engine takes a move back by
    /// loading the earlier position).
    let followLine (wasFollowing: bool) =
      if wasFollowing then
        updatePathFromCurrent ()
        let mutable common = 0
        while common < hashKeys.Count && common < lineHashes.Count && hashKeys.[common] = lineHashes.[common] do
          common <- common + 1
        if hashKeys.Count > common then hashKeys.RemoveRange(common, hashKeys.Count - common)
        for i in common .. lineHashes.Count - 1 do hashKeys.Add lineHashes.[i]

    let resetWithFen (fenOpt: string option) =
      iPosition <- 0
      uciMoves.Clear()
      sanMoves.Clear()
      openingMoves.Clear()
      moveAndFens.Clear()
      hashKeys.Clear()
      position <- game.[0]
      let newPos = BoardHelper.getPosFromFen fenOpt
      lock lockObject (fun () -> position <- newPos)
      startFen <- BoardHelper.posToFen position
      mostCurrentFEN <- startFen
      isFRC <- PositionOps.isFRC &position
      let rootHash = Hash.hashBoard position
      rootPositionHash <- rootHash
      graph.Reset(startFen, rootHash)
      invalidateLine ()
      updatePathFromCurrent ()

    let initBoard () =
      iPosition <- 0
      uciMoves.Clear()
      sanMoves.Clear()
      openingMoves.Clear()
      moveAndFens.Clear()
      hashKeys.Clear()
      game <- Array.init MAX_PLY (fun _ -> Position.Default)
      position <- game.[0]
      BoardHelper.loadFen(None, &position)
      startFen <- BoardHelper.posToFen position
      mostCurrentFEN <- startFen
      let rootHash = Hash.hashBoard position
      rootPositionHash <- rootHash
      graph.Reset(startFen, rootHash)
      invalidateLine ()
      updatePathFromCurrent ()

    do initBoard ()

    /// The edge into the cursor, which a comment belongs to.
    let currentIncomingEdge () =
      if graph.HasNode graph.Current then graph.ParentEdge graph.Current else None

    /// "position fen <fen>", with " moves m1 m2 ..." when there are moves.
    let positionCommand (fen: string) =
      let sb = StringBuilder(24 + (if isNull fen then 0 else fen.Length) + 6 * uciMoves.Count)
      sb.Append("position fen ").Append(fen) |> ignore
      if uciMoves.Count > 0 then
        sb.Append(" moves") |> ignore
        for m in uciMoves do sb.Append(' ').Append(m) |> ignore
      sb.ToString()

    /// The ply the SAN histories number from: the start position's, or after the book moves.
    let historyStartPly () = if openingMoves.Count = 0 then int game.[0].Ply else openingMoves.Count

    /// The path to the cursor, then on down the main line (for a GUI list that shows the rest).
    member _.EndCurrentVariation () =
      moveAndFens.Clear()
      for e in graph.PathTo graph.Current do moveAndFens.Add(VariationGraph.MoveAndFenOf(e, graph.Node e.To))
      let visited = HashSet<NodeId>()
      let rec descend nodeId =
        if visited.Add nodeId then
          match graph.MainChildEdgeId nodeId with
          | Some eid ->
              let edge = graph.Edge eid
              moveAndFens.Add(VariationGraph.MoveAndFenOf(edge, graph.Node edge.To))
              descend edge.To
          | None -> ()
      descend graph.Current

    member val inAnalysisMode = false with get, set

    member _.ResetBoardState () = resetWithFen None

    member _.ResetBoardStateFromFen (fen: string) =
      resetWithFen (if String.IsNullOrWhiteSpace fen then None else Some fen)

    member this.Game
      with get () = game
      and set (v) = game <- v

    member this.Captures
      with get () = captures
      and set (v) = captures <- v

    member this.Castles
      with get () = castles
      and set (v) = castles <- v

    member this.EP
      with get () = eps
      and set (v) = eps <- v

    member this.PlyCount with get () = int position.Ply

    member this.Position
      with get () = position
      and set (value) = position <- value

    /// Sets up a position. When it is one in the graph the cursor goes to it, and the lists (and
    /// the hash keys, when they followed the line) with it - this is how the GUI navigates.
    /// QUIRK (pinned): the undo stack is not reset, and StartPosition is not set.
    member this.LoadFen(?fen: string) =
      match fen with
      | None -> ()
      | Some fen ->
          let following = hashesFollowLine ()
          position <- BoardHelper.getPosFromFen (if String.IsNullOrEmpty fen then None else Some fen)
          if not (String.IsNullOrEmpty fen) then mostCurrentFEN <- fen
          ensureHistory (int position.Ply)
          game.[int position.Ply] <- PositionOps.copy &position
          isFRC <- PositionOps.isFRC &position
          let hash = Hash.hashBoard position
          let found = graph.MoveCursorToPosition(this.FEN(), hash)
          if found && following then
            // A position of the game on the board: repetitions still count from its start, which
            // rootPositionHash already holds. Not the graph's root: after ResetBoardState +
            // LoadFen(custom) that is the standard start position, and the custom start's first
            // occurrence was lost on the first step back.
            followLine true
          else
            // A new position: the game starts here for repetitions (tournaments reset the board
            // and then load the opening).
            rootPositionHash <- hash
            updatePathFromCurrent ()

    member this.FEN() = BoardHelper.posToFen position

    member this.StartPosition
      with get () = startFen
      and set (v) = startFen <- v

    member this.CurrentFEN
      with get () = mostCurrentFEN
      and set (v) = mostCurrentFEN <- v

    /// An EPD book line is a 4-field FEN with no halfmove/fullmove counters. Lc0 rejects such a
    /// FEN when its en-passant field is set ("Bad fen string (en passant square expected)") and
    /// then searches the previous position, so the counters are added here, on the way to the
    /// engine. startPos itself is left as read: the PGN [FEN] tag and OpeningHash come from it.
    static member UciFen (fen: string) =
      if isNull fen then fen
      else
        let fen = fen.Trim()
        match fen.Split(' ', StringSplitOptions.RemoveEmptyEntries).Length with
        | 4 -> fen + " 0 1"
        | 5 -> fen + " 1"   // halfmove clock present, fullmove number missing
        | _ -> fen

    member this.PositionWithMoves() = positionCommand (Board.UciFen startFen)

    member this.GetCurrentEdgeComment () =
      match currentIncomingEdge () with
      | Some edge -> edge.Comments
      | None -> String.Empty

    member this.SetCommentOnCurrentEdge (comment: string) =
      if not (String.IsNullOrWhiteSpace comment) then
        // MovesAndFenPlayed's last entry carries it too.
        if moveAndFens.Count > 0 then
          let lastIdx = moveAndFens.Count - 1
          let last = moveAndFens.[lastIdx]
          let commented = { last with Move = { last.Move with Comments = comment } }
          moveAndFens.[lastIdx] <- commented
          // Same entry in the line's copy, so the lists still hold the line and the next cursor
          // move is not a full rebuild.
          if lastIdx < lineMaf.Count && obj.ReferenceEquals(lineMaf.[lastIdx], last) then
            lineMaf.[lastIdx] <- commented
        match currentIncomingEdge () with
        | Some edge -> graph.SetEdgeComment(edge.Id, comment)
        | None -> ()

    /// The position command for the line to the cursor.
    member this.PositionWithMovesFromGraph() =
      updatePathFromCurrent ()
      this.PositionWithMoves()

    member this.SanMoveNumberString san =
      if String.IsNullOrWhiteSpace san then ""
      else
        let ply = int position.Ply
        let moveNr = (ply + 1) / 2
        if position.STM = 0uy then // White to move: Black just moved
          if moveNr = 0 then sprintf "%d. %s" 1 san else sprintf "%d ...%s" moveNr san
        else sprintf "%d. %s" moveNr san

    member this.MoveNumber() = max 1 ((int position.Ply + 1) / 2)

    /// Move number for the side about to move (before a move is played)
    member this.NextMoveNumber() = max 1 ((int position.Ply + 2) / 2)

    /// Castles, captures and en passant captures, counted when a caller asks.
    member this.CollectStat (move: _ inref) =
      if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then castles <- castles + 1L
      elif (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then captures <- captures + 1L
      if (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then eps <- eps + 1L

    /// Every line of the graph as its own PGN game (mainline first), in LAN.
    member this.WriteGraphLinesToPgn (path: string) (opening: string) (variation: string) =
      let formatMoveLine (moves: string list) =
        let sb = StringBuilder()
        moves |> List.iteri (fun idx lan ->
          if idx % 2 = 0 then
            if sb.Length > 0 then sb.Append(' ') |> ignore
            sb.Append((idx / 2) + 1).Append(". ").Append(lan) |> ignore
          else sb.Append(' ').Append(lan) |> ignore)
        sb.Append(" *").ToString()
      let lines : string list list = this.MoveLinesFromGraph false
      if lines.Length > 0 then
        match IO.Path.GetDirectoryName path with
        | null | "" -> ()
        | dir when not (IO.Directory.Exists dir) -> IO.Directory.CreateDirectory dir |> ignore
        | _ -> ()
        use writer = new IO.StreamWriter(path, false, Encoding.UTF8)
        lines |> List.iteri (fun idx moves ->
          let eventTag = if idx = 0 then "Mainline" else sprintf "Variation %d" idx
          writer.WriteLine $"[Event \"{eventTag}\"]"
          if not (String.IsNullOrWhiteSpace opening) then writer.WriteLine $"[Opening \"{opening}\"]"
          if not (String.IsNullOrWhiteSpace variation) then writer.WriteLine $"[Variation \"{variation}\"]"
          writer.WriteLine $"[FEN \"{startFen}\"]"
          writer.WriteLine()
          writer.WriteLine(formatMoveLine moves)
          writer.WriteLine())

    member this.MoveLinesFromGraph (asLAN: bool) = VariationText.lines graph asLAN

    member this.GetMoveHistoryWithVariations () = VariationText.movetext graph

    member this.IsFRC
      with get () = isFRC
      and set (v) = isFRC <- v

    member this.HashKeys
      with get () = hashKeys
      and set (v) = hashKeys <- v

    member this.PositionHash () = Hash.hashBoard position

    member this.DeviationHash () = Hash.deviationHash position

    /// Moves the cursor one move down the main line from `fen` (or from where it is): that move.
    /// QUIRK (pinned): only the cursor moves; the GUI loads the returned FEN itself.
    member this.TryGetNextMoveAndFen (fen: string) =
      let following = hashesFollowLine ()
      if not (String.IsNullOrWhiteSpace fen) then
        let pos = BoardHelper.getPosFromFen (Some fen)
        graph.MoveCursorToPosition(fen, Hash.hashBoard pos) |> ignore
      match graph.MainChildEdgeId graph.Current with
      | Some eid ->
          let edge = graph.Edge eid
          let toNode = graph.Node edge.To
          graph.Current <- toNode.Id
          updatePathFromCurrent ()
          followLine following
          Some (VariationGraph.MoveAndFenOf(edge, toNode))
      | None -> None

    /// Moves the cursor one move back from `fen` (or from where it is): the move taken back, with
    /// the position BEFORE it as FenAfterMove. At the root: an empty entry with StartPosition.
    /// QUIRK (pinned): only the cursor moves; the GUI loads the returned FEN itself.
    member this.TryGetPreviousMoveAndFen (fen: string) =
      let following = hashesFollowLine ()
      if not (String.IsNullOrWhiteSpace fen) then
        let pos = BoardHelper.getPosFromFen (Some fen)
        graph.MoveCursorToPosition(fen, Hash.hashBoard pos) |> ignore
      match graph.ParentEdge graph.Current with
      | None -> Some { MoveAndFen.FirstEntry with FenAfterMove = startFen }
      | Some chosen ->
          let fromNode = graph.Node chosen.From
          graph.Current <- fromNode.Id
          updatePathFromCurrent ()
          followLine following
          Some (VariationGraph.MoveAndFenOf(chosen, fromNode))

    member _.UciMovesPlayed = uciMoves

    member val SanMovesPlayed = sanMoves with get, set

    member val OpeningMovesPlayed = openingMoves with get, set

    member val MovesAndFenPlayed = moveAndFens with get, set

    member _.MoveGraphRootId = graph.Root
    member _.MoveGraphChildren(nodeId: NodeId) = graph.ChildEdges nodeId

    /// The edge a variation operation names: by SAN (any when empty) into a node with this FEN.
    member private _.FindVariationEdge (san: string) (fen: string) =
      if String.IsNullOrWhiteSpace fen then None
      else graph.FindVariationEdge(san, fen, Hash.hashBoard (BoardHelper.getPosFromFen (Some fen)))

    /// After an edit that removed part of the graph: the cursor back on the root if its node went,
    /// else the edited node's children renumbered; the lists (and hash keys) follow.
    member private _.AfterRemoval (parentId: NodeId) (following: bool) =
      if not (graph.HasNode graph.Current) then graph.Current <- graph.Root
      else graph.ReindexChildOrders parentId
      invalidateLine ()
      updatePathFromCurrent ()
      followLine following

    /// Removes the move into a node with this FEN (matching SAN if given), or with
    /// removeEntireVariation the whole variation it belongs to. A mainline move is only removed
    /// when it lies inside a variation.
    member this.RemoveVariationNode (san: string) (fen: string) (removeEntireVariation: bool) =
      match this.FindVariationEdge san fen with
      | None -> false
      | Some edge ->
          let rec hasVariationAncestor (e: MoveEdge) =
            not e.IsMainline
            || ((graph.Node e.From).Parents |> graph.EdgesOf |> List.exists hasVariationAncestor)
          if edge.IsMainline && not (hasVariationAncestor edge) then false
          else
            let rec findVariationHead (e: MoveEdge) =
              match (graph.Node e.From).Parents |> graph.EdgesOf |> List.filter (fun pe -> not pe.IsMainline) with
              | parent :: _ -> findVariationHead parent
              | [] -> e
            let edgeIdToRemove = if removeEntireVariation then (findVariationHead edge).Id else edge.Id
            let parentId =
              match graph.TryEdge edgeIdToRemove with
              | Some e -> e.From
              | None -> graph.Root
            let following = hashesFollowLine ()
            graph.RemoveEdgeSubtree edgeIdToRemove
            this.AfterRemoval parentId following
            true

    /// Removes the move into a node with this FEN and everything after it.
    member this.RemoveVariationTail (san: string) (fen: string) =
      match this.FindVariationEdge san fen with
      | None -> false
      | Some edge ->
          let following = hashesFollowLine ()
          graph.RemoveEdgeSubtree edge.Id
          this.AfterRemoval edge.From following
          true

    /// Makes the variation holding the move into a node with this FEN the main line where it
    /// branches off.
    member this.PromoteVariationToMainline (san: string) (fen: string) =
      match this.FindVariationEdge san fen with
      | None -> false
      | Some edge ->
          let rec variationHead (e: MoveEdge) (lastNonMain: MoveEdge option) =
            let nextLast = if e.IsMainline then lastNonMain else Some e
            match (graph.Node e.From).Parents |> graph.EdgesOf |> List.tryHead with
            | Some parentEdge -> variationHead parentEdge nextLast
            | None -> defaultArg nextLast e
          let headEdge = variationHead edge None
          graph.SetMainChild(headEdge.From, headEdge.Id)
          invalidateLine ()
          updatePathFromCurrent ()
          true

    /// The board at a graph node: its position loaded, then the cursor put on that very node
    /// (LoadFen alone may pick another node with the same position).
    /// QUIRK (pinned): the lists are not brought to the node; the loaders do that at the end.
    member private this.SetBoardToNode (nodeId: NodeId) =
      this.LoadFen((graph.Node nodeId).Fen)
      graph.Current <- nodeId

    /// Replaces the game with a parsed PGN game and its variations; the cursor ends on the last
    /// move of the main line.
    member this.LoadPGNGameWithVariations (pgn: PGNTypes.PgnGame) =
      this.ResetBoardStateFromFen(if String.IsNullOrWhiteSpace pgn.Fen then this.StartPosition else pgn.Fen)
      createNodeIsVariation <- false
      skippedPgnMoves.Clear()

      let rec playLine (startNode: NodeId) (line: PGNTypes.PlyLine) (isMainline: bool) =
        let prevFlag = createNodeIsVariation
        this.SetBoardToNode startNode
        createNodeIsVariation <- not isMainline
        let mutable currentNode = startNode
        for ply in line do
          let nodeBeforeMove = currentNode
          if not (this.PlaySanMoveWithComments ply.San (if String.IsNullOrWhiteSpace ply.Comment then "" else ply.Comment)) then
            skippedPgnMoves.Add(sprintf "%d%s %s" ply.MoveNumber (if ply.Color = "w" then "." else "...") ply.San)
          currentNode <- graph.Current
          let afterMoveNode = currentNode
          for variation in ply.Variations do
            playLine nodeBeforeMove variation false |> ignore
            this.SetBoardToNode afterMoveNode
            createNodeIsVariation <- not isMainline
        createNodeIsVariation <- prevFlag
        currentNode

      let lastMainNode = if pgn.Mainline.Count = 0 then graph.Root else playLine graph.Root pgn.Mainline true
      for variation in pgn.RootVariations do
        playLine graph.Root variation false |> ignore
      graph.Current <- lastMainNode
      createNodeIsVariation <- false
      updatePathFromCurrent ()

    /// The moves the last LoadPGNGameWithVariations could not play (illegal or unreadable), in load order.
    member _.SkippedPgnMoves : IReadOnlyList<string> = skippedPgnMoves.ToArray()

    member this.GetMoveHistory() = this.GetMoveHistoryToCurrentFen(this.FEN())

    /// Numbered SAN of the line up to and including the move that reached `fen`.
    member this.GetMoveHistoryToCurrentFen (fen: string) =
      let priorMoves = this.MovesAndFenPlayed |> Seq.takeWhile (fun e -> e.FenAfterMove <> fen) |> Seq.toList
      let currentMove = this.MovesAndFenPlayed |> Seq.tryFind (fun e -> e.FenAfterMove = fen)
      let ply = historyStartPly ()
      let sb = StringBuilder()
      let mutable nr = ply
      let mutable moveNr = (ply / 2) + 1
      for moveStr in priorMoves @ Option.toList currentMove do
        if moveStr.Move.Color = "w" then
          sb.Append($" {moveNr}. {moveStr.ShortSan}") |> ignore
          nr <- nr + 1
        elif moveStr.Move.Color = "b" && nr = ply then
          sb.Append($" {moveNr}... {moveStr.ShortSan}") |> ignore
          nr <- nr + 1
          moveNr <- moveNr + 1
        else
          sb.Append($" {moveStr.ShortSan}") |> ignore
          moveNr <- moveNr + 1
          nr <- nr + 1
      sb.ToString().TrimStart()

    /// Numbered SAN of the moves played with PlayUciMove.
    member this.GetSanMoveHistory() =
      let sb = StringBuilder()
      let ply = historyStartPly ()
      let mutable white = game.[0].STM = 0uy
      let mutable nr = ply
      let mutable moveNr = (ply / 2) + 1
      for moveStr in this.SanMovesPlayed do
        if nr % 2 = 1 && not white then
          sb.Append($" {moveNr}... {moveStr}") |> ignore
          white <- true
          moveNr <- moveNr + 1
        elif nr % 2 = 0 then sb.Append($" {moveNr}. {moveStr}") |> ignore
        else
          sb.Append($" {moveStr}") |> ignore
          moveNr <- moveNr + 1
        nr <- nr + 1
      sb.ToString().TrimStart()

    member this.InlineTokensFromGraph () = VariationText.inlineTokens graph

    /// Generates LEGAL moves only (since the Phase 3 movegen rework; previously pseudo-legal)
    member this.GenerateMoves () =
      let mutable index = 0
      let span = moveList.Value.AsSpan()
      let ctx = MoveGeneration.createLegalityContext &position
      generateLegalCaptures span &index &position &ctx
      generateLegalQuiets span &index &position isFRC &ctx
      span.Slice(0, index).ToArray()

    /// Generates LEGAL moves only into the provided buffer, returns the count
    member this.GenerateMovesToBuffer (buffer: TMove Span) : int =
      let mutable index = 0
      let ctx = MoveGeneration.createLegalityContext &position
      generateLegalCaptures buffer &index &position &ctx
      generateLegalQuiets buffer &index &position isFRC &ctx
      index

    /// Makes a move on the position: the undo stack and the hash keys. The move lists and the graph
    /// are the callers'.
    member this.MakeMove (move: TMove inref) =
      ensureHistory iPosition
      game.[iPosition] <- PositionOps.copy &position
      iPosition <- iPosition + 1
      makeMove &move &position
      this.HashKeys.Add(this.PositionHash())

    /// MakeMove for search: the undo stack only.
    member this.MakeMoveNoHash (move: TMove inref) =
      ensureHistory iPosition
      game.[iPosition] <- PositionOps.copy &position
      iPosition <- iPosition + 1
      makeMove &move &position

    member this.ClaimThreeFoldRep () = this.RepetitionNr() >= 3

    /// FIDE dead position — see MaterialRules.isDeadPosition, the single material rule
    /// shared with BoardUtils.getPositionStatus (and thus the query API and the GUI
    /// status line). Endings where mate cannot be FORCED but remains possible with help
    /// (K+N vs K+N, K+B vs K+N, opposite-color bishops, K+N+N vs K) are deliberately NOT
    /// drawn here: tournament play ends them via the eval-based draw adjudication and the
    /// 50-move rule instead.
    member this.InsufficientMaterial() = MaterialRules.isDeadPosition &position

    /// Positional undo only: rewinds the position stack but does NOT pop hashKeys or the
    /// SAN/UCI/FEN lists. Pair with MakeMoveNoHash (perft-style
    /// search); after a full PlayXxxMove the side lists keep the phantom entry.
    member this.UndoMove () =
      iPosition <- iPosition - 1
      position <- game.[iPosition]

    member this.PrintPosition (label: string) = PositionOpsToString(label, &position) |> printfn "%s"

    member this.GetPieceAndColorOnSquare (square: string) = MoveGeneration.getPieceAndColorOnSquare(&position, square)

    /// How often the current position has occurred in the game, this time included.
    member this.RepetitionNr() =
      let key = this.PositionHash()
      let mutable inGame = 0
      for h in hashKeys do
        if h = key then inGame <- inGame + 1
      // hashKeys never contains the start position (it only records positions reached by
      // MakeMove), so it is counted here - otherwise a shuffle back to the starting position is
      // undercounted by one and threefold is claimed a repetition late.
      if key = rootPositionHash then inGame + 1 else inGame

    /// (UCI, SAN) of every legal move - lazily: the SAN is made when the sequence is enumerated.
    member this.GetLegalMoves() =
      let moveList = this.GenerateMoves()
      seq {
        for move in moveList do
          let longSan = TMoveOps.moveToStr &move position.STM
          if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
            // Chess960 castles onto the rook's square; standard castling onto c/g.
            let toSq = move.To
            let kr, qr =
              if position.STM = 0uy then position.RookInfo.WhiteKRInitPlacement, position.RookInfo.WhiteQRInitPlacement
              else position.RookInfo.BlackKRInitPlacement, position.RookInfo.BlackQRInitPlacement
            if toSq = 2uy then longSan, "0-0-0"
            elif toSq = 6uy then longSan, "0-0"
            elif kr = toSq then longSan, "0-0"
            elif qr = toSq then longSan, "0-0-0"
          else longSan, ConvertTo.standardSAN (longSan, move, moveList, position.STM)
      }

    member this.AnyLegalMove() = this.GenerateMovesToBuffer(moveList.Value.AsSpan()) > 0

    member this.IsMate() = MoveGeneration.InCheck &position <> 0UL && not (this.AnyLegalMove())

    member this.IllegalMove (move: TMove inref) = BoardHelper.Illegal &move &position

    member this.GetSanFromUci (move: string) =
      let moveList = this.GenerateMoves ()
      // Numeric matching, case-insensitive like the old string comparison.
      match TMoveOps.tryFindMoveByUciNotation moveList moveList.Length position.STM (move.Trim().ToLower()) with
      | Some tmove ->
          if (tmove.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
            // Standard castling targets g1/c1 (to = 6/2); Chess960 castling the rook's initial
            // square - the same dual check as GetLegalMoves.
            let toSq = tmove.To
            let kr, qr =
              if position.STM = 0uy then position.RookInfo.WhiteKRInitPlacement, position.RookInfo.WhiteQRInitPlacement
              else position.RookInfo.BlackKRInitPlacement, position.RookInfo.BlackQRInitPlacement
            if toSq = 2uy then Some "0-0-0"
            elif toSq = 6uy then Some "0-0"
            elif kr = toSq then Some "0-0"
            elif qr = toSq then Some "0-0-0"
            else None
          else Some (ConvertTo.standardSAN(move, tmove, moveList, this.Position.STM))
      | None -> None

    member this.GetUciFromSan (san: string) =
      let moveList = this.GenerateMoves()
      match TMoveOps.getTMoveFromShortSan san moveList position.STM (fun _ -> true) with
      | Some move -> Some (TMoveOps.getUciNotation move position.STM)
      | None -> None

    /// Plays a UCI move through the graph: an existing child with the same move is followed,
    /// otherwise a new edge is added (a variation when the node already has another main move).
    /// A move that does not match is ignored.
    member this.PlayUciMove move =
      let moveList = this.GenerateMoves()
      match TMoveOps.tryFindMoveByUciNotation moveList moveList.Length position.STM move with
      | Some tmove ->
          let shortSan = TMoveOps.getShortSanMoveFromTmove moveList tmove position
          this.SanMovesPlayed.Add(shortSan)
          this.MakeMove(&tmove)
          let color = colorOfPlayerWhoJustMoved position.STM
          let isCastling = (tmove.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
          let fenAfter = this.FEN()
          let hashAfter = this.PositionHash()
          let childrenEdges = (graph.Node graph.Current).Children |> graph.EdgesOf
          let toFen (e: MoveEdge) = if graph.HasNode e.To then (graph.Node e.To).Fen else ""
          let existingEdge = childrenEdges |> List.tryFind (fun e -> e.San = shortSan && graph.HasNode e.To && toFen e = fenAfter)
          let mainChild =
            childrenEdges
            |> List.tryFind (fun e -> e.IsMainline)
            |> Option.orElseWith (fun () -> childrenEdges |> List.tryHead)
          let isVariation =
            match mainChild with
            | Some m -> m.San <> shortSan || toFen m <> fenAfter
            | None -> false
          let edgeId =
            match existingEdge with
            | Some e -> e.Id
            | None ->
                graph.AddEdge(graph.Current, hashAfter, fenAfter, shortSan, move, color, isCastling, String.Empty,
                              (if isVariation then Some false else None), None)
          match graph.TryEdge edgeId with
          | Some e -> graph.Current <- e.To
          | None -> ()
          // The line gains the move: UciMovesPlayed and MovesAndFenPlayed with it.
          updatePathFromCurrent ()
      | None -> ()

    /// A book move: made, and added to the book and move lists - not to the graph.
    member this.PlayOpeningMove (fromSan: string) =
      let moveList = this.GenerateMoves ()
      // Opening books are written in SAN or in long algebraic ("1. e2e4 e7e5"), so
      // accept both — this used to fail hard on coordinate tokens and abort the game.
      match TMoveOps.tryFindMoveBySanOrUci moveList position.STM (fun _ -> true) fromSan with
      | Some move ->
          let moveStr = TMoveOps.getUciNotation move position.STM
          // A book in long algebraic hands us "e2e4"; record the real SAN instead, or the
          // opening line and the game's SAN header would be written in coordinates — wrong
          // for exactly the books this path exists to support. SAN input is kept verbatim so
          // a book's own spelling survives.
          let shortSan =
            if TMoveOps.isCoordinateNotation (fromSan.Trim())
            then TMoveOps.getShortSanMoveFromTmoveN moveList moveList.Length move position
            else fromSan
          this.MakeMove(&move)
          openingMoves.Add shortSan
          this.UciMovesPlayed.Add(moveStr.Trim())
          let color = colorOfPlayerWhoJustMoved position.STM
          let isCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
          let fenAndMoves = MoveDetail.Create(moveStr, moveStr.[0..1], moveStr.[2..3], color, isCastling)
          moveAndFens.Add({ Move = fenAndMoves; ShortSan = shortSan; FenAfterMove = BoardHelper.posToFen position })
      | None -> failwith $"failed to parse opening move {fromSan}"

    member this.PlaySanMove (san: string) = this.PlaySanMoveWithComments san String.Empty |> ignore

    /// Plays a SAN (or coordinate) move as a new edge from the cursor - a variation while a PGN
    /// variation is loaded. A move that does not match is ignored and false is returned.
    member this.PlaySanMoveWithComments (san: string) (comments: string) =
      let moveList = this.GenerateMoves ()
      // SAN is the normal input here, but coordinate notation reaches this from pasted
      // lines and hand-written PGNs; resolving both beats silently dropping the move.
      match TMoveOps.tryFindMoveBySanOrUci moveList position.STM (fun _ -> true) san with
      | Some move ->
          let moveStr = TMoveOps.getUciNotation move position.STM
          // Coordinate input must be converted, or "e2e4" would end up in the move list, the
          // move graph and the SAN history. Real SAN is kept verbatim so a PGN round-trips
          // with its own spelling ("O-O" stays "O-O") — which does mean PlayUciMove's dedup
          // (it compares generated SAN, always "0-0" and suffix-free) can still miss such a
          // move and branch instead of following the mainline. Pre-existing, and the price of
          // the round-trip guarantee. Computed before the move is made.
          let shortSan =
            if TMoveOps.isCoordinateNotation (san.Trim())
            then TMoveOps.getShortSanMoveFromTmoveN moveList moveList.Length move position
            else san
          this.MakeMove(&move)
          let colorAfterMove = colorOfPlayerWhoJustMoved position.STM
          let isCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
          let edgeId =
            graph.AddEdge(graph.Current, this.PositionHash(), BoardHelper.posToFen position, shortSan, moveStr,
                          colorAfterMove, isCastling, comments, (if createNodeIsVariation then Some false else None), None)
          graph.Current <- (graph.Edge edgeId).To
          // The line gains the move: UciMovesPlayed and MovesAndFenPlayed with it.
          updatePathFromCurrent ()
          true
      | None -> false // the caller reports it: printing here can hit a closed TextWriter

    /// PlayPVLine under the board's lock, for callers on several threads.
    member this.PlayPVLineThreadSafe moves fen = lock lockObject (fun () -> this.PlayPVLine(moves, fen))

    /// A fresh game from `fen` with these UCI moves: the last move's entry, or the first entry.
    member this.PlayPVLine (moves: string seq, fen: string) =
      this.ResetBoardState()
      this.LoadFen fen
      for m in moves do this.PlayUciMove m
      if this.MovesAndFenPlayed.Count > 0 then this.MovesAndFenPlayed |> Seq.last
      else MoveAndFen.FirstEntry

    /// A fresh game from a "position fen ... moves ..." command.
    member this.PlayCommands (fenMoves: string) =
      this.ResetBoardState()
      let (fenCmd, moves) = FEN.parseFENandMoves fenMoves
      match FEN.extractFEN fenCmd with
      | Some fen ->
          this.LoadFen fen
          this.StartPosition <- fen
          this.CurrentFEN <- fen
          for m in moves do this.PlayUciMove m
      | _ -> ()

    /// A fresh game from "<fen> moves ...", the FEN normalised.
    member this.PlayFenWithMoves (fenMoves: string) =
      let (fen, moves) = FEN.parseFENandMoves fenMoves
      this.ResetBoardStateFromFen fen
      let normalizedFen = this.FEN()
      this.CurrentFEN <- normalizedFen
      this.StartPosition <- normalizedFen
      for m in moves do this.PlayUciMove m
