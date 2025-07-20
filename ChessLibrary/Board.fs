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

let startPos = Misc.startPosition

let [<Literal>] MAX_PLY = 1000
let [<Literal>] MAX_MOVES = 256

let moveList = new ThreadLocal<TMove array> (fun () -> Array.zeroCreate<TMove>(MAX_MOVES))

type Board() =        
    let mutable isFRC = false
    let mutable currentIndex = 0
    let mutable startPos = ""
    let mutable mostCurrentFEN = ""
    let mutable captures = 0L
    let mutable castles = 0L
    let mutable eps = 0L
    let mutable iPosition = 0
    let mutable numberOfMoves = 0L
    let mutable game = Array.init MAX_PLY (fun _ -> Position.Default)
    let mutable position = game.[0]
    //let mutable movesLists = new ThreadLocal<TMove array array> (fun () -> Array.init MAX_PLY (fun _ -> Array.init MAX_MOVES (fun _ -> defaultof<TMove> )))
    let mutable plyCount = 0
    let mutable hashKeys = ResizeArray<uint64>()
    let sbPool = Utils.StringBuilderPool(10,20)
    let moves = ResizeArray<TMove>()
    let longSanMoves = ResizeArray<string>()
    let shortSanMoves = ResizeArray<string>()
    let shortSanOpeningMoves = ResizeArray<string>()
    let openingMoves = ResizeArray<string>()
    let moveAndFens = ResizeArray<MoveAndFen>()
    let lockObject = obj()
    
    let updatePosition action =
      Monitor.Enter(lockObject)
      try
          action()
      finally
          Monitor.Exit(lockObject)

     //debugging
    //let moveHis = "1. Nf3 e6 2. g3 d5 3. d4 Bd6 4. b3 Nf6 5. Bb2 b6 6. Bg2 c5 7. e3 cxd4 8. exd4 Nc6 9. O-O Ba6 10. Re1 O-O 11. Nbd2 Nb4 12. a3 Nc6 13. Rc1 Re8 14. c4 Ng4 15. cxd5 exd5 16. Rxc6 Bd3 17. Ne5 Bxe5 18. dxe5 Rc8 19. Rxc8 Qxc8 20. Bxd5 Bc2 21. Qf3 Nxe5 22. Bxe5 Rf8 23. Qc3 g6 24. Qxc8 Rxc8 25. Rc1 Rc5 26. Bc4 Bf5 27. Bxf7+ Kxf7 28. Rxc5 Bd3 29. Rc7+ Ke6 30. Bf4 g5 31. Bxg5 h6 32. Bxh6 a5 33. Rc6+ Kd5 34. Rxb6 a4 35. bxa4 Bc4 36. Nxc4 Kxc4 37. a5 Kc5 38. Rb1 Kc6 39. a6 Kd7 40. a7 Ke6 41. a8=Q Kf5 42. Qh1 Kg4 43. a4 Kh5 44. Bc1 Kg4 45. a5 Kh3 46. a6 Kg4 47. a7 Kh3 48. a8=Q Kg4 49. Qag2 Kh5 50. h4 Kg4 51. h5 Kf5 52. h6 Kg4 53. h7 Kf5 54. h8=Q Kg4 55. Q8h2 Kf5 56. g4+ Kf6 57. g5+ Kg6 58. Qhg3 Kg7 59. g6 Kg8 60. g7 Kf7 61. g8=Q+ Kf6 62. Q8g4 Kf7 63. f4 Kf6 64. Q4f3 Kf5"
    
    let reset() =
      iPosition <- 0
      moves.Clear()
      longSanMoves.Clear()
      shortSanMoves.Clear()      
      openingMoves.Clear()
      shortSanOpeningMoves.Clear()
      moveAndFens.Clear()
      hashKeys.Clear()
      position <- game.[0]
      let newPos = BoardHelper.getPosFromFen(None)
      let update = fun () -> position <- newPos
      updatePosition update      
      startPos <- BoardHelper.posToFen position
      mostCurrentFEN <- startPos
      isFRC <- false
      plyCount <- 0
      numberOfMoves <- 0L
      currentIndex <- 0
      numberOfMoves <- 0L

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
      ZobrishHash.initializeZobristTables()
    do
        initBoard ()
    
    member val inAnalysisMode = false with get, set

    member _.ResetBoardState () = 
        reset ()        
    member this.IsFRC
      with get() = isFRC
      and set(v) = isFRC <- v

    member this.HashKeys  
      with get() = hashKeys
      and set(v) = hashKeys <- v
    
    member this.PositionHash () = Utilities.Hash.hashBoard position
    
    member this.DeviationHash () = Utilities.Hash.deviationHash position

    member this.TryGetNextMoveAndFen (fen:string) =      
      match moveAndFens |> Seq.tryFindIndex(fun p -> p.FenAfterMove = fen) with
      |Some idx ->
        if moveAndFens.Count - 1 > idx then 
          Some moveAndFens[idx + 1] 
        else None       
      |_ -> 
        if (fen = startPos || fen.Contains(startPos)) && moveAndFens.Count > 0 then
          Some moveAndFens[0]
        else
          let fenPart = fen.Split(' ') |> Seq.tryHead
          match fenPart with
          |Some fenPart ->
            if (fen = fenPart || fen.Contains(fenPart)) && moveAndFens.Count > 0 then
              Some moveAndFens[0]
            else
              None
          |None -> None

    member this.TryGetPreviousMoveAndFen (fen:string) =      
      match moveAndFens |> Seq.tryFindIndex(fun p -> p.FenAfterMove = fen) with
      |Some idx -> 
        if idx = 0 then
          Some {MoveAndFen.FirstEntry with FenAfterMove=startPos}
        elif idx > 0 then 
          Some moveAndFens[idx - 1] 
        else 
          None       
      |_ -> None

    member val MovesPlayed = moves with set, get

    member _.LongSANMovesPlayed = longSanMoves

    member val ShortSANMovesPlayed = shortSanMoves with get, set

    member val ShortSANOpeningMovesPlayed = shortSanOpeningMoves with get, set

    member val OpeningMovesPlayed = openingMoves with get, set

    member val MovesAndFenPlayed = moveAndFens with get, set

    member this.CurrentIndex
      with get() = currentIndex 
      and set(v) = currentIndex <- v

    member this.StartPosition 
      with get() = startPos
      and set(v) = startPos <- v

    member this.CurrentFEN 
      with get() = mostCurrentFEN
      and set(v) = mostCurrentFEN <- v

    member this.PositionWithMovesUpToMoveNr moveNr =     
      let start = sprintf $"position fen {startPos} moves"
      this.LongSANMovesPlayed
        |> Seq.truncate moveNr
        |> Seq.fold(fun state m -> sprintf "%s %s" state m) start

    member this.PositionWithMovesIndexed () = 
      let start = sprintf $"position fen {startPos} moves"
      this.LongSANMovesPlayed
        |> Seq.truncate currentIndex
        |> Seq.fold(fun state m -> sprintf "%s %s" state m) start

    member this.PositionWithFenAndMovesIndexed (fen:string) =
      if longSanMoves.Count = 0 then
        let res = sprintf $"position fen {fen} moves"
        res
      else
        longSanMoves 
        |> Seq.truncate currentIndex
        |> Seq.fold(fun state m -> sprintf "%s %s" state m) $"position fen {fen} moves"

    member this.PositionWithMoves() = 
      let start = sprintf $"position fen {startPos} moves"
      this.LongSANMovesPlayed |> Seq.fold(fun state m -> 
        sprintf "%s %s" state m) start

    member this.PositionWithFenAndMoves (fen:string) =
      if longSanMoves.Count = 0 then
        let res = sprintf $"position fen {fen} moves"
        res
      else
        longSanMoves |> Seq.fold(fun state m -> 
          sprintf "%s %s" state m) $"position fen {fen} moves"
    
    member this.MoveNumberString san = 
      let f ply (move:string) = 
        if ply % 2 = 1 then
          let n = ply / 2 + ply % 2
          sprintf "%d. %s" n move
        else      
          sprintf "%s" move
      f plyCount san
    
    member this.NextMoveNumberString san = 
      let f ply (move:string) = 
        let n = ply / 2 + ply % 2
        if ply % 2 = 1 then
          sprintf "%d. %s" n move
        else      
          sprintf "%d ...%s" n move
      f (plyCount + 1) san
    
    member this.CollectStat (move:_ inref) =
          //stats here - castles, captures and eps
      if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then
        castles <- castles + 1L
      elif (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY then
        captures <- captures + 1L
      if (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then        
        eps <- eps + 1L

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

    member this.PlyCount 
      with get() = plyCount
      and set(v) = plyCount <- v
    
    member this.Position 
        with get() = position
        and set(value) = position <- value

    member this.LoadFen(?fen:string) =       
      if fen.IsSome then
        if fen.Value = "" then
          let newPos = BoardHelper.getPosFromFen(None)
          position <- newPos
        else
          let newPos = BoardHelper.getPosFromFen(fen)                    
          position <- newPos
          mostCurrentFEN <- fen.Value
        plyCount <- int position.Ply
        game.[plyCount] <- PositionOps.copy &position
        isFRC <- PositionOps.isFRC &position
      
    member this.FEN() = BoardHelper.posToFen position

    member this.GetNumberedMoveList() : string seq =       
      seq { 
        let ply = if openingMoves.Count = 0 then game.[0].Ply |> int else openingMoves.Count
        let mutable nr = ply
        let mutable moveNr = 
          if ply % 2 = 1 then
            let n = ply / 2 + ply % 2
            n
          else      
            ply / 2 + 1
        for moveStr in this.MovesAndFenPlayed do
          if moveStr.Move.Color = "w" then
            $" {moveNr}. {moveStr.ShortSan}"
            nr <- nr + 1
          elif moveStr.Move.Color = "b" && nr = ply then                        
            $" {moveNr}... {moveStr.ShortSan}"
            nr <- nr + 1
            moveNr <- moveNr + 1
          else
            $" {moveStr.ShortSan}"
            moveNr <- moveNr + 1
            nr <- nr + 1
          }      
    
    member this.GetMoveHistory() =      
      let sb = sbPool.Get()      
      let ply = if openingMoves.Count = 0 then game.[0].Ply |> int else openingMoves.Count
      let mutable nr = ply
      let mutable moveNr = 
        if ply % 2 = 1 then
          ply / 2 + ply % 2
        else      
          ply / 2 + 1
      for moveStr in this.MovesAndFenPlayed do
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
      sb.ToString().TrimStart()
    
    member this.GetMoveHistoryToCurrentFen (fen : string) =      
      let mutable priorMoves = this.MovesAndFenPlayed |> Seq.takeWhile (fun e -> e.FenAfterMove <> fen)
      let currentMove = this.MovesAndFenPlayed |> Seq.tryFind (fun e -> e.FenAfterMove = fen)
      let ply = if openingMoves.Count = 0 then game.[0].Ply |> int else openingMoves.Count
        // add current move to the prior moves
      match currentMove with
      | Some move -> priorMoves <- Seq.append priorMoves (seq { yield move })
      | None -> ()
      let sb = sbPool.Get()      
      //let ply = openingMoves.Count        
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
      sb.ToString().TrimStart()        
   
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

    //member this.GenerateMovesFast () =
    //  let mutable index = 0
    //  let span = movesLists.Value.[plyCount].AsSpan()
    //  generateCapturesInSpan span &index &position      
    //  generateQuietsInSpan span &index &position this.IsFRC 
    //  span.Slice(0, index)
          
    member this.MakeMove (move:TMove inref) =
      game.[iPosition] <- PositionOps.copy(&position)
      iPosition <- iPosition + 1
      plyCount <- plyCount + 1
      makeMove &move &position
      position.Ply <- max (position.Ply + 1uy) (byte plyCount)
      let hash = this.PositionHash()
      this.HashKeys.Add hash
      this.MovesPlayed.Add move      
      //this.CollectStat &move
      //printfn "Position hash: %d" hash

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
      if plyCount = 0 then
        printfn "ply count should never be less than zero"
      plyCount <- plyCount - 1

    member this.PrintPosition (label:string) = 
      PositionOpsToString(label, &position) |> printfn "%s"

    member this.GetPieceAndColorOnSquare (square:string) =
      MoveGeneration.getPieceAndColorOnSquare(&position, square)
    
    // Check if the current position is a repetition or not, and set Rep field   
    member this.UpdateRepetition() =
      position.Rep <- this.RepetitionNr() |> byte

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
      //let mutable index = 0
      this.GenerateMoves ()
      |> Array.exists(fun m -> BoardHelper.Illegal &m &position |> not)      

    member this.IsMate() =
      MoveGeneration.InCheck &position <> 0UL && this.AnyLegalMove() |> not     
    
    member this.IllegalMove (move: TMove inref) =
      BoardHelper.Illegal &move &position
    
    member this.getSANFromEngineAN (move:string) =      
      //let mutable index = 0
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
    
    member this.PlayLongSanMove move =      
      //let mutable index = 0
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
        moveAndFens.Add({Move=fenAndMoves; ShortSan=shortSan; FenAfterMove=this.FEN()})
      |None -> 
        //let moveL = this.GenerateMoves()
        ()
    

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

      | None ->         
            //let hmm = TMove.getTMoveFromShortSan fromSan moveList position.STM islegal
            failwith $"failed to parse opening move {fromSan}"

    member this.PlayPgnToPly (pgn : PGNTypes.PgnGame, lastPly : int) =
      this.ResetBoardState()
      if String.IsNullOrEmpty(pgn.Fen) |> not then         
        this.LoadFen(pgn.Fen)
        
      let movesFromPgn (pgn:PGNTypes.PgnGame) =
        [
        for m in pgn.Moves do
          if m.WhiteSan <> "" then
            m.WhiteSan
          if m.BlackSan <> "" then
            m.BlackSan ]      
      let mutable plyIdx = 0
      for m in movesFromPgn pgn do
        if plyIdx <= lastPly then
          this.PlaySimpleShortSan m
          plyIdx <- plyIdx + 1      
      let movesAndFen = this.MovesAndFenPlayed |> Seq.last
      movesAndFen

    member this.PlayPgn (pgn : PGNTypes.PgnGame, lastMove : int, color : string) =
      this.ResetBoardState()
      let fen = 
        if String.IsNullOrEmpty(pgn.Fen) then 
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        else pgn.Fen
      this.LoadFen(fen)
      let mutable moveidx = 1
      for m in pgn.Moves do
        let moveNr = int m.MoveNr
        if moveNr < lastMove then
          this.PlaySimpleShortSan m.WhiteSan
          if m.BlackSan <> "" then
            this.PlaySimpleShortSan m.BlackSan
          moveidx <- moveidx + 1
        elif moveNr = lastMove then
          if color = "w" then
            this.PlaySimpleShortSan m.WhiteSan
            moveidx <- moveidx + 1
          elif color = "b" then
            this.PlaySimpleShortSan m.WhiteSan
            this.PlaySimpleShortSan m.BlackSan
            moveidx <- moveidx + 1
      let movesAndFen = this.MovesAndFenPlayed |> Seq.last
      movesAndFen
    
    member this.PlaySimpleShortSan (fromSan: string) = 
      let islegal move = this.IllegalMove &move |> not
      //let mutable index = 0      
      let moveList = this.GenerateMoves ()
      match TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal with
      | Some (move) -> 
        //let moveHistory = this.GetMoveHistory()
        //if moveHistory = moveHis then
        //  this.PrintPosition($"Current position state:")
        //  let shortSan = TMove.getShortSanMoveFromTmove moveList move position
        //  let ha =TMove.getTMoveFromShortSan fromSan moveList position.STM islegal
        //  printfn $"found move {fromSan} in {nameof this.PlaySimpleShortSan}"
        let moveStr = TMoveOps.getSanLong move position.STM
        this.MakeMove(&move)
        longSanMoves.Add (moveStr.Trim())
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
      | None ->        
        let moveHistory = this.GetMoveHistory()
        printfn $"{moveHistory}"
        //printfn $"{this.PositionHash()}: failed to parse move {fromSan}"        
        let hmm = TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal
        printfn $"failed to parse move {fromSan} in {nameof this.PlaySimpleShortSan}"

    member this.PlaySimpleShortSanWithComments (fromSan: string) (comments : string) = 
      let islegal move = this.IllegalMove &move |> not
      //let mutable index = 0      
      let moveList = this.GenerateMoves ()
      match TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal with
      | Some (move) ->         
        let moveStr = TMoveOps.getSanLong move position.STM
        this.MakeMove(&move)
        longSanMoves.Add (moveStr.Trim())
        let fenPos = BoardHelper.posToFen position
        let fenAndMoves = 
          {
            LongSan = moveStr
            FromSq = moveStr[0..1]
            ToSq = moveStr[2..3]
            Color = (if position.STM=0uy then "b" else "w")
            IsCastling = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
            Comments = comments
          }
        moveAndFens.Add({Move=fenAndMoves; ShortSan=fromSan; FenAfterMove=fenPos})
      | None ->        
        let moveHistory = this.GetMoveHistory()
        printfn $"{moveHistory}"
        //printfn $"{this.PositionHash()}: failed to parse move {fromSan}"        
        let hmm = TMoveOps.getTMoveFromShortSan fromSan moveList position.STM islegal
        printfn $"failed to parse move {fromSan} in {nameof this.PlaySimpleShortSan}"

    // Method that needs to be made thread-safe
    member this.PlayPVLineThreadSafe moves fen = 
        lock lockObject (fun () -> 
          this.ResetBoardState()
          this.LoadFen fen
          for m in moves do
            this.PlayLongSanMove m
          currentIndex <- plyCount
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
      currentIndex <- plyCount
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
        currentIndex <- plyCount
      |_ -> ()      
    
    member this.PlayFenWithMoves (fenMoves : string) =
      this.ResetBoardState()
      let (fen, moves) = Utilities.FEN.parseFENandMoves fenMoves
      this.LoadFen fen      
      this.CurrentFEN <- fen
      this.StartPosition <- fen
      for m in moves do
        this.PlayLongSanMove m
      currentIndex <- plyCount

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
      for m in pgn.Moves do
        if m.WhiteSan <> "" then
          m.WhiteSan
        if m.BlackSan <> "" then
          m.BlackSan ]

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
    let consoleResString, engineStats, crossTable = PGNCalculator.getEngineDataResults pgnGames
    let moveDevs = findAllDeviationsForAllPlayers pgnGames
    let devSummary = createDeviationSummary moveDevs pgnGames
    let numberOfGames = pgnGames.Length
    let numberOfDevs = moveDevs |> Seq.length
    let fraction = float numberOfDevs / float numberOfGames
    let sortedSummary = devSummary |> Seq.sortByDescending(fun e -> e.AdjustedScore)            
    consoleResString, sortedSummary, engineStats, crossTable, fraction

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
  
  
    