module ChessLibrary.Perft

open System.Diagnostics
open TypesDef
open Chess
open LowLevelUtilities
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
let frc1 = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
let frc2 = "2nnrbkr/p1qppppp/8/1ppb4/6PP/3PP3/PPP2P2/BQNNRBKR w HEhe - 1 9"
let frc3 = "b1q1rrkb/pppppppp/3nn3/8/P7/1PPP4/4PPPP/BQNNRKRB w GE - 1 9"
let bug = "nrbkn2r/pppp1p1p/4p1p1/3P4/6P1/P3B3/P1P1PP1P/qR1KNBQR w KQkq - 0 10"

let startPos5TimesTestPositions =
  [|
      startPos
      startPos
      startPos
      startPos
      startPos
      startPos
      startPos
  |]

let ceresPerftTestPositions =
  let fens = [
      7, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      5, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
      7, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
      6, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
      3, "rnbqkb1r/pp1p1ppp/2p5/4P3/2B5/8/PPP1NnPP/RNBQK2R w KQkq - 0 6"
      5, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    ]

  [|
      yield! fens

      5, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 25"
      7, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0"
      6, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      6, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
      5, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
      
  |]

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

let perft (board:Board) depth = 
  let rec perft depth =
    if depth = 0 then 
      1L
    else      
      let mutable nodes = 0L
      //let mutable index = 0
      let mutable pos = board.Position
      let moves = board.GenerateMoves ()
      for move in moves do
        if not ( BoardHelper.Illegal &move &pos) then        
          board.MakeMove (&move)
          nodes <- nodes + perft (depth - 1)
          board.UnMakeMove () //position move undo
      nodes
  perft depth


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
    let mutable pos = board.Position
    let moves = board.GenerateMoves ()
    for move in moves do
      //printfn $"Move {i}: {TMove.moveToStr &move pos.STM}"
      if ( BoardHelper.Illegal &move &pos) then
        //board.PrintPosition "Illegal move"
        () //do nothing
      elif depth > 1 then       
        board.MakeMove (&move)
        nodes <- nodes + perft (depth - 1)
        board.UnMakeMove() //position move undo
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
    let mutable pos = board.Position
    let moves = board.GenerateMoves()
    for move in moves do      
      //printfn $"Move {i}: {TMove.moveToStr &move pos.STM}"
      if BoardHelper.Illegal &move &pos then
        () //do nothing
      elif depth > 1 then       
        board.MakeMove (&move)
        nodes <- nodes + perft (depth - 1)
        board.UnMakeMove() //position move undo
      else              
          nodes <- nodes + 1L
          board.CollectStat &move
    nodes
 
  let res = perft depth
  let correct = getNumberFromRecord depth record
  if res = correct then
    LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Green
      $"{record.PositionNumber} - TEST PASSED - Correct: {correct:N0} = {res:N0} nodes"
  else
    board.PrintPosition "Error"
    printfn $"Captures: {board.Captures:N0} Castles: {board.Castles:N0} EP: {board.EP:N0}"
    let diff = if res > correct then sprintf "%d too many" (res-correct) else sprintf "%d too few" (res-correct)
    LowLevelUtilities.ConsoleUtils.printInColor ConsoleColor.Red
      $"\nPosition {record.PositionNumber} depth {depth}: ERROR FEN: {record.FEN}\n\tcorrect number of positions are {correct:N0}, you got {res:N0} ({diff})"

let runPerft0 depth fen =
  let board = Board() 
  board.LoadFen(fen)  
  if board.IsFRC then
    printfn "Chess960 position"
  else
    printfn "Standard position"
  let mutable pos = board.Position
  let rec perft depth = 
    let mutable nodes = 0L
    if depth = 0 then 
      1L
    else      
      let moves = board.GenerateMoves()
      for move in moves do        
        pos <- board.Position
        if not ( BoardHelper.Illegal &move &pos) then        
          board.MakeMove(&move)
          nodes <- nodes + perft (depth - 1)
          board.UnMakeMove ()
      nodes
  timeIt (fun _ -> perft depth) depth |> ignore
  printfn $"Captures: {board.Captures:N0} Castles: {board.Castles:N0} EP: {board.EP:N0}" 

let runPerft1 depth fen =
  let board = Board()
  board.LoadFen(fen)  
  let rec perft depth =
    let mutable nodes = 0L    
    let moves = board.GenerateMoves()
    let mutable pos = board.Position
    if depth = 1 then
      for move in moves do
        if not ( BoardHelper.Illegal &move &pos) then 
          nodes <- nodes + 1L
      nodes
    else      
      for move in moves do
        if not ( BoardHelper.Illegal &move &pos) then        
          board.MakeMove(&move)
          nodes <- nodes + perft (depth - 1)
          board.UnMakeMove()
      nodes
  timeIt (fun _ -> perft depth) depth

let divide depth fen =
  let board = Board()
  board.LoadFen(fen) 
  let mutable position = board.Position
  let mutable total = 0L
  let mutable index = 0
  let moves = board.GenerateMoves ()
  printfn "\nDivide with depth = %d started\n" depth
  for move in moves do    
    let mutable nodes = 1L
    if not ( BoardHelper.Illegal &move &position) then        
      if depth > 0 then
        board.MakeMove &move
        nodes <- perft board (depth - 1)
        board.UnMakeMove()
        printfn $"  {TMove.TMoveOps.moveToStr &move position.STM}:   {nodes:N0}"
      total <- total + nodes
  printfn $"\nTotalt: {total:N0}"


let runPerftTests from to' =
  let board = Board()
  board.LoadFen()
  for i = from to to' do
    divide i ""

module StandardPositions =

 // Standard positions copied from Stockfish and LC0. - use 30 seconds per position
  let BENCHMARK_POS = [|
      1, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      2, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10"
      3, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11"
      4, "4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19"
      5, "rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14 moves d4e6"
      6, "r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14 moves g2g4"
      7, "r3r1k1/2p2ppp/p1p1bn2/8/1q2P3/2NPQN2/PPP3PP/R4RK1 b - - 2 15"
      8, "r1bbk1nr/pp3p1p/2n5/1N4p1/2Np1B2/8/PPP2PPP/2KR1B1R w kq - 0 13"
      9, "r1bq1rk1/ppp1nppp/4n3/3p3Q/3P4/1BP1B3/PP1N2PP/R4RK1 w - - 1 16"
      10, "4r1k1/r1q2ppp/ppp2n2/4P3/5Rb1/1N1BQ3/PPP3PP/R5K1 w - - 1 17"
      11, "2rqkb1r/ppp2p2/2npb1p1/1N1Nn2p/2P1PP2/8/PP2B1PP/R1BQK2R b KQ - 0 11"
      12, "r1bq1r1k/b1p1npp1/p2p3p/1p6/3PP3/1B2NN2/PP3PPP/R2Q1RK1 w - - 1 16"
      13, "3r1rk1/p5pp/bpp1pp2/8/q1PP1P2/b3P3/P2NQRPP/1R2B1K1 b - - 6 22"
      14, "r1q2rk1/2p1bppp/2Pp4/p6b/Q1PNp3/4B3/PP1R1PPP/2K4R w - - 2 18"
      15, "4k2r/1pb2ppp/1p2p3/1R1p4/3P4/2r1PN2/P4PPP/1R4K1 b - - 3 22"
      16, "3q2k1/pb3p1p/4pbp1/2r5/PpN2N2/1P2P2P/5PP1/Q2R2K1 b - - 4 26"
      17, "6k1/6p1/6Pp/ppp5/3pn2P/1P3K2/1PP2P2/3N4 b - - 0 1"
      18, "3b4/5kp1/1p1p1p1p/pP1PpP1P/P1P1P3/3KN3/8/8 w - - 0 1"
      19, "2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1 moves g5g6 f3e3 g6g5 e3f3"
      20, "8/6pk/1p6/8/PP3p1p/5P2/4KP1q/3Q4 w - - 0 1"
      21, "7k/3p2pp/4q3/8/4Q3/5Kp1/P6b/8 w - - 0 1"
      22, "8/2p5/8/2kPKp1p/2p4P/2P5/3P4/8 w - - 0 1"
      23, "8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - 0 1"
      24, "8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - 0 1"
      25, "8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - 0 1"
      26, "5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - 0 1"
      27, "6k1/6p1/P6p/r1N5/5p2/7P/1b3PP1/4R1K1 w - - 0 1"
      28, "1r3k2/4q3/2Pp3b/3Bp3/2Q2p2/1p1P2P1/1P2KP2/3N4 w - - 0 1"
      29, "6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - 0 1"
      30, "8/3p3B/5p2/5P2/p7/PP5b/k7/6K1 w - - 0 1"
      31, "5rk1/q6p/2p3bR/1pPp1rP1/1P1Pp3/P3B1Q1/1K3P2/R7 w - - 93 90"
      32, "4rrk1/1p1nq3/p7/2p1P1pp/3P2bp/3Q1Bn1/PPPB4/1K2R1NR w - - 40 21"
      33, "r3k2r/3nnpbp/q2pp1p1/p7/Pp1PPPP1/4BNN1/1P5P/R2Q1RK1 w kq - 0 16"
      34, "3Qb1k1/1r2ppb1/pN1n2q1/Pp1Pp1Pr/4P2p/4BP2/4B1R1/1R5K b - - 11 40"
  |] 
  
  let getAllFENPositions = BENCHMARK_POS
  