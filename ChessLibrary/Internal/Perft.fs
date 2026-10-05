module ChessLibrary.Perft

open System.Diagnostics
open PositionTypes
open Chess
open RuntimeUtilities
open System

type Chess960Record = {
    PositionNumber : int
    FEN : string
    Depth1 : int64
    Depth2 : int64
    Depth3 : int64
    Depth4 : int64
    Depth5 : int64
    Depth6 : int64
}

let getNumberFromRecord depth (r:Chess960Record) = 
    match depth with
    | 1 -> r.Depth1
    | 2 -> r.Depth2
    | 3 -> r.Depth3
    | 4 -> r.Depth4
    | 5 -> r.Depth5
    | 6 -> r.Depth6
    | _ -> failwith "Invalid depth"

let pos2 = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "
let pos3 = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - "
let pos4 = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
let pos5 = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
let pos6 = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
let bug = "nrbkn2r/pppp1p1p/4p1p1/3P4/6P1/P3B3/P1P1PP1P/qR1KNBQR w KQkq - 0 10"

let selectionOfTestPositions =
  [|
     startPos
     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "
     "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - "
     "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
     "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
     "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
     "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
     "2nnrbkr/p1qppppp/8/1ppb4/6PP/3PP3/PPP2P2/BQNNRBKR w HEhe - 1 9"
     "b1q1rrkb/pppppppp/3nn3/8/P7/1PPP4/4PPPP/BQNNRKRB w GE - 1 9"
     "nrbkn2r/pppp1p1p/4p1p1/3P4/6P1/P3B3/P1P1PP1P/qR1KNBQR w KQkq - 0 10"
  |]

let timeIt f depth =  
  let start = Stopwatch.GetTimestamp()
  printfn "\nPerft for depth = %d started... \n" depth
  let result : int64 = f()
  let mutable time = int64 (Stopwatch.GetElapsedTime(start).TotalMilliseconds)
  if time = 0L then
    printfn "Duration = 0 ms"
    time <- 1L
  let nps = (result / time ) * 1000L
  printfn "Elapsed time: %d ms" time
  printfn $"Nodes: {result:N0} - NPS: {nps:N0}"
  result

// GenerateMoves is legal-only (Phase 3 movegen rework), so the walkers below need no
// per-move legality filtering. The reference pseudo-legal + Illegal walker lives in
// TestProject/MoveGenLegalityTests.fs (perftReference) as the permanent cross-check.
let perft (board:Board) depth =
  let rec perft depth =
    if depth = 0 then
      1L
    else
      let mutable nodes = 0L
      let moves = board.GenerateMoves ()
      for move in moves do
        board.MakeMoveNoHash (&move)
        nodes <- nodes + perft (depth - 1)
        board.UndoMove () //position move undo
      nodes
  perft depth

// Fast perft for testing only (skips hash tracking - not for real games).
// GenerateMovesToBuffer emits legal moves only, so leaves are bulk-counted directly.
let perftFast (board:Board) depth =
  let maxDepth = max depth 10
  let buffer = Array.zeroCreate<MoveTypes.TMove> (256 * maxDepth)

  let rec search depth offset =
    if depth = 0 then 1L
    else
      let bufferSpan = buffer.AsSpan(offset, 256)
      let count = board.GenerateMovesToBuffer(bufferSpan)
      if depth = 1 then
        // Bulk leaf counting - the move list is already legal-only
        int64 count
      else
        let mutable nodes = 0L
        for i = 0 to count - 1 do
          let mutable move = buffer.[offset + i]
          board.MakeMoveNoHash(&move)
          nodes <- nodes + search (depth - 1) (offset + 256)
          board.UndoMove()
        nodes
  search depth 0

let perftOpt depth fen = 
  let board = Board()
  board.LoadFen(fen)  
  
  if board.IsFRC then
    printfn "Chess960 position"
  else
    printfn "Standard position"
  board.PrintPosition "Perft start"
  let rec perft depth =
    let mutable nodes = 0L
    let moves = board.GenerateMoves ()
    for move in moves do
      if depth > 1 then
        board.MakeMoveNoHash (&move)
        nodes <- nodes + perft (depth - 1)
        board.UndoMove() //position move undo
      else
          nodes <- nodes + 1L
          board.CollectStat &move
    nodes
  timeIt (fun _ -> perft depth) depth |> ignore
  printfn $"Captures: {board.Captures:N0} Castles: {board.Castles:N0} EP: {board.EP:N0}"
  
let perftOptChecked (record:Chess960Record) depth = 
  let board = Board()  
  board.LoadFen(record.FEN)
  let rec perft depth =
    let mutable nodes = 0L
    let moves = board.GenerateMoves()
    for move in moves do
      if depth > 1 then
        board.MakeMoveNoHash (&move)
        nodes <- nodes + perft (depth - 1)
        board.UndoMove() //position move undo
      else
          nodes <- nodes + 1L
          board.CollectStat &move
    nodes

  let res = perft depth
  let correct = getNumberFromRecord depth record
  if res = correct then
    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Green
      $"{record.PositionNumber} - TEST PASSED - Correct: {correct:N0} = {res:N0} nodes"
  else
    board.PrintPosition "Error"
    printfn $"Captures: {board.Captures:N0} Castles: {board.Castles:N0} EP: {board.EP:N0}"
    let diff = if res > correct then sprintf "%d too many" (res-correct) else sprintf "%d too few" (res-correct)
    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red
      $"\nPosition {record.PositionNumber} depth {depth}: ERROR FEN: {record.FEN}\n\tcorrect number of positions are {correct:N0}, you got {res:N0} ({diff})"

let perftOptCheckedFast (record:Chess960Record) depth =
  let board = Board()
  board.LoadFen(record.FEN)
  let res = perftFast board depth
  let correct = getNumberFromRecord depth record
  if res = correct then
    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Green
      $"{record.PositionNumber} - TEST PASSED - Correct: {correct:N0} = {res:N0} nodes"
  else
    board.PrintPosition "Error"
    let diff = if res > correct then sprintf "%d too many" (res-correct) else sprintf "%d too few" (res-correct)
    RuntimeUtilities.ConsoleUtils.printInColor ConsoleColor.Red
      $"\nPosition {record.PositionNumber} depth {depth}: ERROR FEN: {record.FEN}\n\tcorrect number of positions are {correct:N0}, you got {res:N0} ({diff})"

let divide depth fen =
  let board = Board()
  board.LoadFen(fen)
  let mutable position = board.Position
  let mutable total = 0L
  let moves = board.GenerateMoves ()
  printfn "\nDivide with depth = %d started\n" depth
  for move in moves do
    let mutable nodes = 1L
    if depth > 0 then
      board.MakeMoveNoHash &move
      nodes <- perft board (depth - 1)
      board.UndoMove()
      printfn $"  {MoveTypes.TMoveOps.moveToStr &move position.STM}:   {nodes:N0}"
    total <- total + nodes
  printfn $"\nTotalt: {total:N0}"
