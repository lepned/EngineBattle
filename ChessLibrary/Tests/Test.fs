module ChessLibrary.Test

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Unchecked
open System.IO
open TypesDef.Position
open TypesDef.TMove
open TypesDef
open Utilities
open LowLevelUtilities
open Chess
open Chess.BoardUtils
open Parser
open Perft
open System.Diagnostics

module Deviations =  
    
  let deviationSummaryFromPGNs (filePath: string) =    
    printfn "Parsing and analyzing file: %s\n" filePath
    let games = PGNParser.parsePgnFile filePath |> Seq.truncate 100000 |> Seq.toList
    if games |> Seq.forall (fun e -> e.GameMetaData.Round <> "") then      
      printfn "Start of deviation test - number of games %d\n" games.Length
      let consoleRes, devSummary, data, cross, fraction = Deviation.analyzeDeviations games
      let consoleSummary = Deviation.printDeviationsToConsole devSummary
      PGNCalculator.idealizedEloPrint cross
      //printfn "Gauntlet player: %s" gauntletPlayer
      printfn "%s" consoleRes
      printfn "%s" consoleSummary

    else
      printfn "Not all games has a valid round number in file: %s" filePath

let parseChess960Record (input: string) =
    //printfn "%s" input
    let parts = input.Split('\t')
    let positionNumber = int parts.[0]
    let fen = parts.[1]
    let depth1 = int64 parts.[2]
    let depth2 = int64 parts.[3]
    let depth3 = int64 parts.[4]
    let depth4 = int64 parts.[5]
    let depth5 = int64 parts.[6]
    let depth6 = int64 parts.[7]
    { PositionNumber = positionNumber; FEN = fen; Depth1 = depth1; Depth2 = depth2; Depth3 = depth3; Depth4 = depth4; Depth5 = depth5; Depth6 = depth6 }

let parseFile (filePath : string) =
    let lines = File.ReadAllLines(filePath) |> Array.skip 1
    let records = lines |> Array.map parseChess960Record
    records

let records path = parseFile path

let mutable board = Board()
let rnd = Random()
board.LoadFen(Chess.startPos)
//let testFen = BoardHelper.posToFen board.Position
//check hash for these two:

let knightMaskTests () =
  printfn "\nKnight mask tests\n"
  QBBOperations.getKnightMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getKnightMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getKnightMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getKnightMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getKnightMask 1 6 |> printfn "from 1 to 6 Long: %d"

let bishopMaskTests () =
  printfn "\nBishop mask tests\n"
  QBBOperations.getBishopMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getBishopMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getBishopMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getBishopMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getBishopMask 1 6 |> printfn "from 1 to 6 Long: %d"

let rookMaskTests () =
  printfn "\nRook mask tests\n"
  QBBOperations.getRookMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getRookMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getRookMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getRookMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getRookMask 1 6 |> printfn "from 1 to 6 Long: %d"

let rookAndBishopTests () =
  printfn "\nRook and bishop mask tests\n"
  QBBOperations.getRookBishopMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getRookBishopMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getRookBishopMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getRookBishopMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getRookBishopMask 1 6 |> printfn "from 1 to 6 Long: %d"

let pawnMaskTests () =
  printfn "\nPawn mask tests\n"
  QBBOperations.getPawnMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getPawnMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getPawnMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getPawnMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getPawnMask 1 6 |> printfn "from 1 to 6 Long: %d"

let kingMaskTests () =
  printfn "\nKing mask tests\n"
  QBBOperations.getKingMask 4 6 |> printfn "4-6 Standard Short: %d"
  QBBOperations.getKingMask 4 2 |> printfn "4-2 Standard Long: %d"
  QBBOperations.getKingMask 1 2 |> printfn "from 1 to 2 Long: %d"
  QBBOperations.getKingMask 2 2 |> printfn "from 2 to 2 Long: %d"
  QBBOperations.getKingMask 1 6 |> printfn "from 1 to 6 Long: %d"

let moveParsingTests () =
  let board = Chess.Board()
  let mutable pos = board.Position
  //let mutable index = 0
  let moveList = board.GenerateMoves ()
    
  //generate some test moves
  let queenMove = {From = 12uy; To = 28uy; MoveType = TPieceType.QUEEN ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let rookMove = {From = 0uy; To = 8uy; MoveType = TPieceType.ROOK ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let pawnMove = {From = 8uy; To = 17uy; MoveType = TPieceType.PAWN ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let kingMove = {From = 4uy; To = 12uy; MoveType = TPieceType.KING ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let knightMove = {From = 1uy; To = 18uy; MoveType = TPieceType.KNIGHT ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let bishopMove = {From = 2uy; To = 16uy; MoveType = TPieceType.BISHOP ||| TPieceType.CAPTURE ; Promotion = TPieceType.EMPTY}
  let enPassantMove = {From = 8uy; To = 16uy; MoveType = TPieceType.PAWN ||| TPieceType.EP ; Promotion = TPieceType.EMPTY}
  let nonCapturePawnMove = {From = 8uy; To = 16uy; MoveType = TPieceType.PAWN ; Promotion = TPieceType.EMPTY}
  let promotionMove = {From = 8uy; To = 16uy; MoveType = TPieceType.PAWN ||| TPieceType.PROMO ; Promotion = TPieceType.QUEEN}
  let promotionCaptureMove = {From = 8uy; To = 16uy; MoveType = TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE ; Promotion = TPieceType.QUEEN}

  let results = ResizeArray<_>()
  for i = 1 to 1 do
    let res = TMoveOps.getShortSanMoveFromTmove moveList queenMove      
    //board.MakeMove &queenMove
    //let res1 = TMove.getShortSanMoveFromTmove moveList rookMove
    //let res2 = TMove.getShortSanMoveFromTmove moveList pawnMove
    //let res3 = TMove.getShortSanMoveFromTmove moveList kingMove
    //let res4 = TMove.getShortSanMoveFromTmove moveList knightMove
    //let res5 = TMove.getShortSanMoveFromTmove moveList bishopMove
    //let res6 = TMove.getShortSanMoveFromTmove moveList enPassantMove
    //let res7 = TMove.getShortSanMoveFromTmove moveList nonCapturePawnMove
    //let res8 = TMove.getShortSanMoveFromTmove moveList promotionMove
    //let res9 = TMove.getShortSanMoveFromTmove moveList promotionCaptureMove
    //let stop = 1
    results.Add res

//let runKingAttackBitboardTests () =
//  let board = Board()
//  board.LoadFen(Perft.pos4)
//  printfn "Perft fen: %s\n" Perft.pos4
//  board.PrintPosition("Position 4")
//  let position = board.Position
//  let occupation = Position.occupation &position
//  let opposing = position.PM ^^^ occupation
//  let tL = row1Long occupation
//  let tS = row1Short occupation
//  let mutable res = 0UL
//  for i = 0 to 7 do
//    res <- getKingAttackLong i 
//    printfn "Accumulated attack for i = %d -> %d" i res
//  printfn "All attacks by opponents in one bishop bitboard: %A" (opposing &&& res)

//let runKingBishopLongBitboardTests () =
//  let board = Board()
//  board.LoadFen(Perft.pos4)
//  printfn "Perft fen: %s\n" Perft.pos4
//  board.PrintPosition("Position 4")
//  let position = board.Position
//  let occupation = Position.occupation &position
//  let opposing = position.PM ^^^ occupation
//  let mutable res = 0UL
//  for i = 0 to 7 do
//    res <- getKingBishopAttackLong i occupation 
//    printfn "Accumulated attack for i = %d - %d" i res
//  printfn "All attacks by opponents in one bishop bitboard: %A" (opposing &&& res)

//let runBishopBitboardTests () =
//  let board = Board()
//  board.LoadFen(Perft.pos4)
//  printfn "Perft fen: %s\n" Perft.pos4
//  board.PrintPosition("Position 4")
//  let position = board.Position
//  let occupation = Position.occupation &position
//  let opposing = position.PM ^^^ occupation
//  let afile = QBBOperations.getAFileBishopAttack occupation
//  let bfile = QBBOperations.getBFileBishopAttack occupation
//  let cfile = QBBOperations.getCFileBishopAttack occupation
//  let dfile = QBBOperations.getDFileBishopAttack occupation
//  let efile = QBBOperations.getEFileBishopAttack occupation
//  let ffile = QBBOperations.getFFileBishopAttack occupation
//  let gfile = QBBOperations.getGFileBishopAttack occupation
//  let hfile = QBBOperations.getHFileBishopAttack occupation
//  //add all attack bitboards together
//  let allAttacks = afile ||| bfile ||| cfile ||| dfile ||| efile ||| ffile ||| gfile ||| hfile
//  //for i = 0 to 7 do
//  //print all bitboards on a new line in one row
//  printfn "%A %A %A %A %A %A %A %A" afile bfile cfile dfile efile ffile gfile hfile
//  printfn "All attacks by opponents in one bishop bitboard: %A" (opposing &&& allAttacks)

let runFileBitboardTests () =
  let board = Board()
  board.LoadFen(Perft.pos4)
  printfn "Perft fen: %s\n" Perft.pos4
  board.PrintPosition("Position 4")
  let position = board.Position
  let occupation = PositionOps.occupation &position
  let opposing = position.PM ^^^ occupation
  let afile = QBBOperations.getAFileKingAttack occupation
  let bfile = QBBOperations.getBFileKingAttack occupation
  let cfile = QBBOperations.getCFileKingAttack occupation
  let dfile = QBBOperations.getDFileKingAttack occupation
  let efile = QBBOperations.getEFileKingAttack occupation
  let ffile = QBBOperations.getFFileKingAttack occupation
  let gfile = QBBOperations.getGFileKingAttack occupation
  let hfile = QBBOperations.getHFileKingAttack occupation
  //add all attack bitboards together
  let kingAttacksByOpposingForce = (afile ||| bfile ||| cfile ||| dfile ||| efile ||| ffile ||| gfile ||| hfile) &&& opposing
  //for i = 0 to 7 do
  //print all bitboards on a new line in one row
  printfn "%A %A %A %A %A %A %A %A" afile bfile cfile dfile efile ffile gfile hfile
  printfn "All attacks by opponents in one king file bitboard: %A" kingAttacksByOpposingForce


let timeAndReportPerft (records: seq<Chess960Record>) depth =
    let mutable totalNodes = 0L
    let mutable totalTime = 0.0
    let stopwatch = Stopwatch.StartNew()

    for r in records do
        stopwatch.Restart()
        Perft.perftOptChecked r depth
        let nodes = getNumberFromRecord depth r
        stopwatch.Stop()

        let elapsedSeconds = stopwatch.Elapsed.TotalSeconds
        let nps = float nodes / elapsedSeconds

        totalNodes <- totalNodes + nodes
        totalTime <- totalTime + elapsedSeconds

        printfn "\tTime = %.2f seconds, NPS = %s" elapsedSeconds (String.Format("{0:N0}", nps))

    let overallNps = float totalNodes / totalTime
    printfn "\nOverall: Total Nodes = %d, Total Time = %.2f seconds, NPS = %s" totalNodes totalTime (String.Format("{0:N0}", overallNps))


let repeatPerft fen depth n =
  for _ = 1 to n do
    Perft.perftOpt depth fen |> ignore

let smallStartPosPerftTest depth =
  printfn $"\nStarting perft test on start position"
  for (d, pos) in Perft.ceresPerftTestPositions do  
    printfn $"\nStarting perft test on position {pos}"
    if d > 0 then
      Perft.perftOpt d pos
    else
      Perft.perftOpt depth pos    

let smallPerftTestSample depth =
  printfn $"\nStarting perft test on a selection of positions"
  for pos in Perft.selectionOfTestPositions do  
    printfn $"\nStarting perft test on position {pos}"
    Perft.perftOpt depth pos

let smallFRCPerftVerificationTest upto =
  printfn $"\nStarting small Fischer Random Chess perft test {nameof Perft.bug} {Perft.bug}"
  for i = 1 to upto do
    printfn "\nfastest"
    Perft.perftOpt i Perft.bug
    //Perft.perftOpt i Perft.frc2 true
    //Perft.perftOpt i Perft.frc3 true
    //Perft.perftOpt i Perft.frc4 true
    //Perft.perftOpt i Perft.frc5 true
    //printfn "\nslower with more backtracking"
    //Perft.runPerft0 depth Perft.frc1
    //Perft.runPerft0 depth Perft.frc2

let completeFRCPerftVerificationTest depth upto =
  let orgColor = Console.ForegroundColor
  let records = 
    TestPositions.CHESS960PERFT_POS.Split(Environment.NewLine) 
    |> Seq.skip(1) 
    |> Seq.map parseChess960Record 
    |> Seq.truncate upto 
    |> Seq.toArray
  printfn $"\nStarting Chess960 Chess perft test"  
  timeAndReportPerft records depth 
  Console.ForegroundColor <- orgColor

let bigPerftTestSample depth =
  printfn $"\nStarting small perft test on start position {Chess.startPos}"
  for i = 0 to 5 do
    Perft.perftOpt depth Chess.startPos
    Perft.runPerft0 depth Chess.startPos
   
  printfn $"\nStarting small perft test on position {Perft.pos2}"
  Perft.divide depth Perft.pos2
  printfn $"\nStarting small perft test on position {Perft.pos3}"
  Perft.divide depth Perft.pos3
  printfn $"\nStarting small perft test on position {Perft.pos4}"
  Perft.divide depth Perft.pos4
  printfn $"\nStarting small perft test on position {Perft.pos5}"
  Perft.divide depth Perft.pos5
  printfn $"\nStarting small perft test on position {Perft.pos6}"
  Perft.divide depth Perft.pos6
  let curPos = Chess.startPos
  let tot = Perft.perftOpt depth curPos
  printfn $"\nStarting repeat perft function: {nameof repeatPerft}"
  repeatPerft curPos depth 3



let fen1 = """8/p1r2b2/1p1R1P1k/7p/P1p1N1p1/4N3/1bB2P1P/6K1 w - - 1 39"""
let fen2 = "8/p1r1Nb2/1p1R1P1k/7p/P1p1N1p1/8/1bB2P1P/6K1 w - - 5 41"

let mutable posToUse = board.Position
BoardHelper.loadFen(Some fen1, &posToUse)
let h1 = Utilities.Hash.hashBoard posToUse
posToUse <- board.Position
BoardHelper.loadFen(Some fen2, &posToUse)
let h2 = Utilities.Hash.hashBoard posToUse
//printfn "%d vs %d" h1 h2

let testMovegenerator() =  
  //let mutable index = 0
  let moves = board.GenerateMoves ()
  for move in moves do
    if move.MoveType <> TPieceType.EMPTY then
      //printfn "%A" m
      let move = TMoveOps.moveToStr &move board.Position.STM
      printfn "%s" move
    else
      printfn "Should not be here since piece type is empty"

let randomMoveTest () =
  let m = makeRandomMove rnd &board
  let toMove = (board.Position.STM ^^^ PositionOps.BLACK)
  let move = TMoveOps.moveToStr &m toMove
  printfn "\n%A: %s " m.MoveType move

let playRandomGame () =
  for _ = 0 to 200 do
    board.PrintPosition("\nNew pos")
    randomMoveTest()
    //randomMoveTest()

//playRandomGame()

//let curPos = Perft.pos6
//let depth = 5
//Perft.divide depth Perft.pos2
//Perft.divide depth Perft.pos3
//Perft.divide depth Perft.pos4
//Perft.divide depth Perft.pos5
//Perft.divide depth Perft.pos6
//let tot = Perft.perftOpt depth curPos
//repeatPerft curPos depth 3

module BlunderGames =
  let game1 = """
    [Event "TCEC Cup 14 Quarterfinal"]
    [Site "https://tcec-chess.com"]
    [Date "2024.10.08"]
    [Round "1.3"]
    [White "RubiChess 20240817"]
    [Black "Stockfish dev-20240928-d6043970"]
    [Result "1-0"]

    1. d4 d6 2. Nf3 Bg4 3. e4 Nd7 4. h3 Bh5 5. g4 Bg6 6. Qe2 c6 7. h4 h5 8. g5 d5 9.exd5 cxd5 10. Nc3 e6 11. Nxd5 Qa5+ 12. Nc3 Bb4 13. Bd2 Ne7 14. Qb5 Qxb5 
    15. Nxb5 Bxd2+ 16. Nxd2 O-O 17. O-O-O Rac8 18. c3 a6 19. Nd6 Rc7 20. N2c4 Rd8 21. Ne5 Nf8 22. Nxg6 Nfxg6 23. Ne4 Nf4 24. Rd2 Nf5 25. Kd1 Kf8 26. Nc5 Ne7 27. Nd3 Neg6 
    28. Nxf4 Nxf4 29. Kc2 b5 30. a3 Rc6 31. Rd1 Rcc8 {31... e5 32. dxe5 Rxd1 33. Kxd1 Rc5 34. Kd2 Rxe5 35. c4 Ke7 36. b4 Kd6 37. c5+ Kc7 38. Rg1 Ng6 39. Be2 Rd5+ 40. Kc2 Nxh4 41. Bxh5 Rf5 42. Re1 g6 43. Bg4 Rf4 44. Re7+ Kd8 45. Rd7+ Ke8 46. Bh3 Rxf2+ 47. Rd2 Rxd2+ 48. Kxd2 Nf3+ 49. Ke3 Nxg5 50. Bc8 Ne6 51. Bxa6 Nc7 52. Bc8 (EV: 99.5%) g5 (EV: 68.7%)} 
    32. Rg1 Rd5 33. Rd2 Ke7 34. Rg3 Rf5 35. Rf3 Ng6 36. Rxf5 exf5 37. Be2 Rh8 38. a4 bxa4 39. c4 Nxh4 40. c5 Kd7 41. Rd1 Re8 
    42. Bxa6 Kc6 43. Ra1 Nf3 44. Rxa4 Rd8 45. Kc3 h4 46. Bf1 Nxg5 47. Kc4 Kd7 48. d5 Ke7 49. Ra7+ Rd7 50. d6+ Ke6 51. Rxd7 Kxd7 52. Kd5 Ne6 53. c6+ Ke8 54. c7 Nxc7+ 
    55. dxc7 Kd7 56. Ke5 f4 57. Kxf4 Kc8 58. Kg4 h3 59. Kxh3 Kxc7 60. Bc4 f6 61. Kg4 f5+ 62. Kxf5 *"""


module ParsingTests =
  let navsPgn = "C:/Dev/Chess/PGNs/FRC.pgn"
  let gamePgn = "C:/Dev/Chess/PGNs/firstTest04.pgn"
  let pgnPath = "C:/Dev/Chess/Openings/sts.pgn"
  let pgntest = "C:/Dev/Chess/PGNs/ccc22-rapid-semifinals.pgn"
  let enginBattlePath = "C:/Dev/Chess/PGNs/dfrc_UncertaintyMatch.pgn"
  let bigLichessFile = "C:/Dev/Chess/lichess_db_standard_rated_2017-10.pgn"
  let queenOddsGames = "C:/Users/Navn/Downloads/lichess_LeelaQueenOdds_2025-03-14.pgn"
  
  let testRemovePlayerFromPGN (playerToRemove : string ) =
    let playerToLower = playerToRemove.ToLower()
    let games = PGNParser.parsePgnFile gamePgn |> Seq.toList
    let getFileName () =
      let fileName = Path.GetFileNameWithoutExtension gamePgn
      let fileDir = Path.GetDirectoryName navsPgn
      let fileExt = Path.GetExtension navsPgn
      let outputFile = Path.Combine(fileDir, sprintf "%s_%s_Removed%s" fileName playerToRemove fileExt)
      outputFile
    let outputFile = getFileName ()
    let filteredGames = 
        games 
        |> Seq.filter (fun g -> 
            let playerFound = 
                g.GameMetaData.White.ToLower().Contains playerToLower || 
                g.GameMetaData.Black.ToLower().Contains playerToLower
            not playerFound )
        |> Seq.toList
    PGNWriter.writeRawPgnGamesAdjustedToFile outputFile filteredGames
  
  let testPGNHeaderParsing pgnFile =
    let games = PGNParser.parsePgnFileHeaders pgnFile
    printfn "Number of games: %d" (games |> Seq.length)
    let mutable gameNr = 1
    let allGames = games |> Seq.truncate 10_000
    for g in allGames  do
      printfn "Game %d: %s %s vs %s %s" gameNr g.GameMetaData.Event g.GameMetaData.White g.GameMetaData.Black g.GameMetaData.Result      
      gameNr <- gameNr + 1

  let testOfPVParsing () =
    let pv = "d2d4 d7d5 c2c4 e7e6 b1c3 c7c6 e2e3 g8f6 f1d3 f8d6 g1f3 b8d7"
  
    let moveList = Array.init 256 (fun _ -> defaultof<TMove>) 
    for n in 1 .. 1_000 do
      let board = Chess.Board()
      let shortPV = getShortSanPVFromLongSanPVFast moveList &board pv
      if n % 100 = 0 then
        printfn "n = %d, shortPV = %s" n shortPV
      ()
    let board = Chess.Board()
    for n in 1 .. 1_000_000 do
      let shortPV = getShortSanPVFromLongSanPVFast moveList &board pv
      if n % 100_000 = 0 then
        printfn "n = %d, shortPV = %s" n shortPV
      ()

  
  let getAllMatesFromPGN pgnFile printToConsole =
    let board = Board()
    let games = PGNParser.parsePgnFile pgnFile |> Seq.truncate 1_000_000
    let mutable numberOfPlys = 0
    let mutable gameIdx = 0
    printfn "Started parsing of PGN file and looking for games with mates: %s" pgnFile
    [
      for pgn in games do
        gameIdx <- gameIdx + 1
        if gameIdx % 10_000 = 0 then
          printfn "Game number: %d Number of plies: %d " gameIdx numberOfPlys
        if printToConsole then
          printfn "Start of game number: %d\n" gameIdx 
        board.ResetBoardState()
        if String.IsNullOrEmpty pgn.Fen |> not then
          board.LoadFen(pgn.Fen)        
      
        let mutable moveidx = 0
        for m in pgn.Moves do
          if m.WhiteSan <> "" then
            board.PlaySimpleShortSan m.WhiteSan
            moveidx <- moveidx + 1
            numberOfPlys <- numberOfPlys + 1
            if printToConsole then
              match Utilities.Regex.parseEvalRegexOption m.WhiteComment false with
              | Some eval -> printfn "White move %s, move number: %d Eval: %f" m.WhiteSan moveidx eval
              | None -> ()

          if m.BlackSan <> "" then
            board.PlaySimpleShortSan m.BlackSan
            moveidx <- moveidx + 1
            numberOfPlys <- numberOfPlys + 1
            if printToConsole then
              match Utilities.Regex.parseEvalRegexOption m.BlackComment true with
              | Some eval -> 
                printfn "Black move %s, move number: %d Eval: %f" m.BlackSan moveidx eval
              | None -> ()
        let isMat = board.IsMate()
        if isMat then
          let ply = board.PlyCount
          let lastPosition = board.MovesAndFenPlayed |> Seq.last
          yield lastPosition, ply, pgn.GameMetaData.White, pgn.GameMetaData.Black, pgn.GameMetaData.Result
    ]    

  let parsAllPGNgames path printToConsole =
    let board = Board()
    //let games = parsePgnFile path |> Seq.skip 5_000_000 |> Seq.truncate 5_000_000 
    let games = PGNParser.parsePgnFile path |> Seq.truncate 1_000_000
    let mutable numberOfPlys = 0
    let mutable gameIdx = 0
    printfn "Started parsing of PGN file: %s" path
    //let find = "br4k1/p4N2/4pn2/8/2P3P1/q2B3P/1r1Q4/2KR1R2 w"
    for pgn in games do
      gameIdx <- gameIdx + 1
      if gameIdx % 10_000 = 0 then
        printfn "Game number: %d Number of plies: %d " gameIdx numberOfPlys
      if printToConsole then
        printfn "Start of game number: %d\n" gameIdx 
      board.ResetBoardState()
      board.LoadFen(pgn.Fen)
      let mutable moveidx = 0
      for m in pgn.Moves do
        if m.WhiteSan <> "" then
          board.PlaySimpleShortSan m.WhiteSan
          moveidx <- moveidx + 1
          numberOfPlys <- numberOfPlys + 1
          if printToConsole then
            match Utilities.Regex.parseEvalRegexOption m.WhiteComment false with
            | Some eval -> printfn "White move %s, move number: %d Eval: %f" m.WhiteSan moveidx eval
            | None -> ()

        if m.BlackSan <> "" then
          board.PlaySimpleShortSan m.BlackSan
          moveidx <- moveidx + 1
          numberOfPlys <- numberOfPlys + 1
          if printToConsole then
            match Utilities.Regex.parseEvalRegexOption m.BlackComment true with
            | Some eval -> 
              printfn "Black move %s, move number: %d Eval: %f" m.BlackSan moveidx eval
            | None -> ()

    printfn "Number of games played: %d and number of moves played: %d" gameIdx numberOfPlys

  type EvalDetail = { Player: string; Move: string; PonderMove:string; Ply:int; Eval: float; FEN:string; Comment: string }
  type EvalDetailResult = {GameNr: int; Result: string; Players:string; EvalDetail: EvalDetail; Color:string }

  let withinInterval intervalMin intervalMax (evalPair: EvalDetailResult) =
      evalPair.EvalDetail.Eval >= intervalMin && evalPair.EvalDetail.Eval <= intervalMax

  let collectChessEvaluationsInChunks (pgn: PGNTypes.PgnGame) (chunkSize: int) =    
      board.ResetBoardState()
      board.LoadFen(pgn.Fen)
      let mutable ply = 0
      let mutable recentEvalsWhite = Queue<EvalDetailResult>()
      let mutable recentEvalsBlack = Queue<EvalDetailResult>()
      [
          for m in pgn.Moves do
            if m.WhiteSan <> "" then
                ply <- ply + 1
                board.PlaySimpleShortSan m.WhiteSan
                match Utilities.Regex.parseEvalRegexOption m.WhiteComment false with
                | Some eval ->
                    let pd =
                      match Utilities.Regex.parsePonderMove m.WhiteComment with
                      | Some ponder -> ponder
                      | None -> ""
                    let comment = sprintf "{%s}" m.WhiteComment
                    let evalDetail = {Player = pgn.GameMetaData.White; Move = m.WhiteSan; PonderMove = pd; Ply = ply; Eval = eval; FEN = board.PositionWithMoves(); Comment = comment }
                    let players = pgn.GameMetaData.White + " vs " + pgn.GameMetaData.Black
                    let evalRes = {Result = pgn.GameMetaData.Result; GameNr = pgn.GameNumber; Players = players; EvalDetail = evalDetail; Color = "w"}
                    if recentEvalsWhite.Count >= chunkSize then
                        recentEvalsWhite.Dequeue() |> ignore
                    recentEvalsWhite.Enqueue evalRes
                | None -> ()
            if m.BlackSan <> "" then
                ply <- ply + 1
                board.PlaySimpleShortSan m.BlackSan
                match Utilities.Regex.parseEvalRegexOption m.BlackComment true with
                | Some eval ->
                    let pd =
                      match Utilities.Regex.parsePonderMove m.BlackComment with
                      | Some ponder -> ponder
                      | None -> ""
                    let comment = sprintf "{%s}" m.BlackComment
                    let evalDetail = {Player = pgn.GameMetaData.Black; Move = m.BlackSan; PonderMove = pd ; Ply = ply; Eval = eval;  FEN = board.PositionWithMoves(); Comment = comment }
                    let players = pgn.GameMetaData.White + " vs " + pgn.GameMetaData.Black
                    let evalRes = {Result = pgn.GameMetaData.Result; GameNr = pgn.GameNumber; Players = players; EvalDetail = evalDetail; Color = "b"}
                    if recentEvalsBlack.Count >= chunkSize then
                        recentEvalsBlack.Dequeue() |> ignore
                    recentEvalsBlack.Enqueue evalRes
                | None -> ()
            if recentEvalsWhite.Count = chunkSize && recentEvalsBlack.Count = chunkSize then
                let results = (recentEvalsWhite |> Seq.toList) @ (recentEvalsBlack |> Seq.toList) |> List.sortBy(fun e -> e.EvalDetail.Ply)
                recentEvalsWhite.Clear()
                recentEvalsBlack.Clear()
                yield! results
          
          let all = (recentEvalsWhite |> Seq.toList) @ (recentEvalsBlack |> Seq.toList)|> List.sortBy(fun e -> e.EvalDetail.Ply)
          if all.Length > 0 then
            yield! all

      ] |> List.chunkBySize (2 * chunkSize)

  let collectAllChessEvaluations (pgn: PGNTypes.PgnGame) =    
      board.ResetBoardState()
      board.LoadFen(pgn.Fen)
      let mutable ply = board.PlyCount     
      [
          for m in pgn.Moves do
            if m.WhiteSan <> "" then
                ply <- ply + 1
                board.PlaySimpleShortSan m.WhiteSan
                match Utilities.Regex.parseEvalRegexOption m.WhiteComment false with
                | Some eval ->
                    let pd =
                      match Utilities.Regex.parsePonderMove m.WhiteComment with
                      | Some ponder -> ponder
                      | None -> ""
                    let comment = sprintf "{%s}" m.WhiteComment
                    let evalDetail = {Player = pgn.GameMetaData.White; Move = m.WhiteSan; PonderMove = pd; Ply = ply; Eval = eval; FEN = board.PositionWithMoves(); Comment = comment }
                    let players = pgn.GameMetaData.White + " vs " + pgn.GameMetaData.Black
                    let evalRes = {Result = pgn.GameMetaData.Result; GameNr = pgn.GameNumber; Players = players; EvalDetail = evalDetail; Color = "w"}
                    yield evalRes
                | None -> ()
            if m.BlackSan <> "" then
                ply <- ply + 1
                board.PlaySimpleShortSan m.BlackSan
                match Utilities.Regex.parseEvalRegexOption m.BlackComment true with
                | Some eval ->
                    let pd =
                      match Utilities.Regex.parsePonderMove m.BlackComment with
                      | Some ponder -> ponder
                      | None -> ""
                    let comment = sprintf "{%s}" m.BlackComment
                    let evalDetail = {Player = pgn.GameMetaData.Black; Move = m.BlackSan; PonderMove = pd ; Ply = ply; Eval = eval;  FEN = board.PositionWithMoves(); Comment = comment }
                    let players = pgn.GameMetaData.White + " vs " + pgn.GameMetaData.Black
                    let evalRes = {Result = pgn.GameMetaData.Result; GameNr = pgn.GameNumber; Players = players; EvalDetail = evalDetail; Color = "b"}
                    yield evalRes
                | None -> () 
      ]

  let analyzeChessEvaluationChanges (evalChunks: EvalDetailResult list) =
      if List.length evalChunks < 2 then None
      else
        if evalChunks.Length = 2 then
          let changes = List.pairwise evalChunks |> List.map (fun ({EvalDetail = prev}, {EvalDetail = curr}) ->
              abs (curr.Eval - prev.Eval))
          let totalChange = List.sum changes
          Some (totalChange, evalChunks)
        else
          let players = evalChunks |> List.distinctBy (fun e -> e.EvalDetail.Player)
          let p1 = players.Head.EvalDetail.Player
          let p2 = players.Tail.Head.EvalDetail.Player
          let p1Chunks = evalChunks |> List.filter (fun e -> e.EvalDetail.Player = p1)
          let p2Chunks = evalChunks |> List.filter (fun e -> e.EvalDetail.Player = p2)
          let p1Changes = List.pairwise p1Chunks |> List.map (fun ({EvalDetail = prev}, {EvalDetail = curr}) ->
              abs (curr.Eval - prev.Eval))
          let p2Changes = List.pairwise p2Chunks |> List.map (fun ({EvalDetail = prev}, {EvalDetail = curr}) ->
              abs (curr.Eval - prev.Eval))
          let totalChange = abs (List.sum p1Changes + List.sum p2Changes)
          Some (totalChange, evalChunks)
            

  let findMostSignificantChange (isWithinInterval: EvalDetailResult -> bool) (evalChunksList: EvalDetailResult list list) =
    let filteredEvalsList =
        evalChunksList
        |> List.map (List.filter isWithinInterval) // Apply filtering based on a custom function
    
    let res = filteredEvalsList |> List.choose analyzeChessEvaluationChanges
    if List.isEmpty res then
        None
    else
        Some (res |> List.maxBy fst)

  let customInterval = withinInterval -2.0 2.0
  let whiteMayWin = withinInterval 0.0 2.0
  let blackMayWin = withinInterval -2.0 0.0
  let noFilter = fun _ -> true  
  let calcDiffInEvals (a,b) = abs (a - b)

  let msg evalDetailResult =  
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f  \nFEN = %s" 
        evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
        evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
        evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.FEN

  let shortMsg evalDetailResult = 
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f - ponder move: %s" 
      evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
      evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
      evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.PonderMove
  
  let shortMsgWithMoves evalDetailResult = 
    sprintf "\tGameNr %d (ply: %d): %s played %s with eval %f - ponder move: %s \n%s\n" 
      evalDetailResult.GameNr evalDetailResult.EvalDetail.Ply 
      evalDetailResult.EvalDetail.Player evalDetailResult.EvalDetail.Move 
      evalDetailResult.EvalDetail.Eval evalDetailResult.EvalDetail.PonderMove evalDetailResult.EvalDetail.FEN

  //get a streamwriter
  let getWriter (path:string) =
    new StreamWriter(path)

  //write to file
  let writeToFile (path:string) evalDetailResults =
    use writer = new StreamWriter(path)
    for evalDetailResult in evalDetailResults do
      writer.WriteLine(msg evalDetailResult)

  //append to file
  let appendToFile (path:string) writeRawPGN pgnPath (pgn:PGNTypes.PgnGame) evalDetailResults  =
    use writer = new StreamWriter(path, true)
    writer.WriteLine "---------------------------------------------------------"
    let players = sprintf "%s vs %s" pgn.GameMetaData.White pgn.GameMetaData.Black
    let eventInfo =
      let round = sprintf "Round: %s" pgn.GameMetaData.Round
      if String.IsNullOrEmpty pgn.GameMetaData.Event then
        round
      else
        sprintf "Event: %s %s" pgn.GameMetaData.Event round
    writer.WriteLine(sprintf "%s : (%s) %s \n(In PGN-file: %s)\n\t" eventInfo pgn.GameMetaData.Result players pgnPath)
    let mutable counter = 0
    let lastEval = (evalDetailResults |> Seq.length) - 1
    for evalDetailResult in evalDetailResults do
      if lastEval = counter then
        writer.WriteLine(shortMsgWithMoves evalDetailResult)
      else
        writer.WriteLine(shortMsg evalDetailResult)
      counter <- counter + 1
    if writeRawPGN then
      let comments = evalDetailResults |> Seq.fold (fun acc e -> acc + sprintf "%s played %s with comment: %s" e.EvalDetail.Player e.EvalDetail.Move e.EvalDetail.Comment + "\n") ""
      writer.WriteLine(comments)

  // Function to safely extract pairs and calculate differences
  let safeDiff pair =
    match pair with
    | [a; b] -> Some (a.EvalDetail.Eval, b.EvalDetail.Eval)
    | _ -> None

  let gameAnalysis path chunkSize =
      let games = PGNParser.parsePgnFile path |> Seq.truncate 1_000_000
      printfn "Working on file: %s" path
      //write started games to file
      //writeToFile "C:/Dev/Chess/PGNs/evals.txt" []
      let collector = appendToFile "C:/Dev/Chess/PGNs/evals.txt" false path
      for pgn in games do
          let evalChunks = collectChessEvaluationsInChunks pgn chunkSize
          match findMostSignificantChange customInterval evalChunks with
          | Some (change, chunks) ->
              if change > 1.0 && chunks.Length = chunkSize * 2 then
                printfn "Most significant change: %f" change
                collector pgn chunks
                //for p in pairs do
                //  printfn "%s" (msg p) 
          | _ -> printfn "No significant changes found."

  
  let isMisEvaluatedPosition (evalChunks: EvalDetailResult list) (player, color) threshold =
    let whiteEvals = evalChunks |> List.filter (fun e -> e.Color = "w")
    let blackEvals = evalChunks |> List.filter (fun e -> e.Color = "b")   
    let whiteSum = whiteEvals |> List.sumBy (fun e -> e.EvalDetail.Eval |> abs)
    let blackSum = blackEvals |> List.sumBy (fun e -> e.EvalDetail.Eval |> abs)
    
    let collect () =
      let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
      let maxWhiteEvals = if whiteEvals.Length > 0 then (whiteEvals |> List.maxBy(fun e -> e.EvalDetail.Eval |> abs)).EvalDetail.Eval else 0.0
      let maxBlackEvals = if blackEvals.Length > 0 then (blackEvals |> List.maxBy(fun e -> e.EvalDetail.Eval |> abs)).EvalDetail.Eval else 0.0
      match result with
      | "1-0" -> 
          whiteSum > blackSum && color = "b" && maxBlackEvals < threshold            
      | "0-1" -> 
          blackSum > whiteSum && color = "w" && maxWhiteEvals > -threshold  
      | _ -> 
         blackSum > whiteSum && color = "b" ||
         whiteSum > blackSum && color = "w"

    //let diff = abs (maxWhiteEvals - maxBlackEvals)
    let diff = abs (whiteSum - blackSum)
    diff > threshold && collect()

  let isPotentialMissedWinAlt1 (evalChunks: EvalDetailResult list list) (player, color) bothWinningTreshold =
    let allChunks = evalChunks |> List.concat 
    let maxEvalsInAllChunks = allChunks |> List.maxBy(fun e -> e.EvalDetail.Eval |> abs)
    
    let whiteEvals = allChunks |> List.filter (fun e -> e.Color = "w") |> List.exists (fun e -> abs e.EvalDetail.Eval > bothWinningTreshold)
    let blackEvals = allChunks |> List.filter (fun e -> e.Color = "b") |> List.exists (fun e -> abs e.EvalDetail.Eval > bothWinningTreshold)
    let bothAboveThreshold = whiteEvals && blackEvals
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Head.Result
    let draw = result = "1/2-1/2"
    if not draw then
      false, []
    else
      if bothAboveThreshold then
        let index = allChunks |> List.findIndex (fun e -> e = maxEvalsInAllChunks)
        let evalsBefore = allChunks.[index-2..index-1]
        let evalsAfter = allChunks.[index..index+2]
        let evals = evalsBefore @ evalsAfter
        printfn "Game number = %d - potential win with both players above threshold %s, color = %s" maxEvalsInAllChunks.GameNr maxEvalsInAllChunks.Result color
        printfn "Eval: %f" maxEvalsInAllChunks.EvalDetail.Eval
        true, evals
      elif maxEvalsInAllChunks.EvalDetail.Eval > bothWinningTreshold then
        //get two evals before the maxEvalsInAllChunks and two evals (included the maxEvalsInAllChunks) after the maxEvalsInAllChunks
        //maybe use the indexed version of the list and look at the maxEvalsInAllChunks index
        let index = allChunks |> List.findIndex (fun e -> e = maxEvalsInAllChunks)
        let evalsBefore = allChunks.[index-2..index-1]
        let evalsAfter = allChunks.[index..index+2]
        let evals = evalsBefore @ evalsAfter
        printfn "Game number = %d - potential win with at least one player above threshold %s, color = %s" maxEvalsInAllChunks.GameNr maxEvalsInAllChunks.Result color
        printfn "Eval: %f" maxEvalsInAllChunks.EvalDetail.Eval
        true , evals
      else 
        false, []

  let bigEvalChange (evalChunks: EvalDetailResult list) threshold color =   
      let min = evalChunks |> List.minBy(fun e -> e.EvalDetail.Eval)
      let max = evalChunks |> List.maxBy(fun e -> e.EvalDetail.Eval)
      let diff = abs (max.EvalDetail.Eval - min.EvalDetail.Eval)
      if diff > threshold && color <> max.Color then
        Some (evalChunks, min, max)
      else
        None

  let maxAndMinEval (evalChunks: EvalDetailResult list) =   
      let min = evalChunks |> List.minBy(fun e -> abs e.EvalDetail.Eval)
      let max = evalChunks |> List.maxBy(fun e -> abs e.EvalDetail.Eval)
      max, min
  
  let isPotentialMissedWinWithPonder (evalChunks: EvalDetailResult list) (player, color) =
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
    let isDraw = result = "1/2-1/2"
    if not isDraw then
      []   
    else
      match bigEvalChange evalChunks 1.5 color with
      | Some (evals, min, max) ->
          for e in evals do
            printfn "ply: %d Color: %s %f (%s)" e.EvalDetail.Ply e.Color e.EvalDetail.Eval e.EvalDetail.Comment
          if max.EvalDetail.Eval > 0.1 && color = "w"  || max.EvalDetail.Eval < -0.1 && color = "b" then
            let indexOfMax = evals |> List.findIndex (fun e -> e = max)
            let ponder = max.EvalDetail.PonderMove
            let nextPonderMove = 
              let elems = evals.Length
              let idx = indexOfMax + 1
              let e = evals.[idx].EvalDetail
              if idx < elems then
                let hhhhh = e.Move
                e.PonderMove
              else
                ""
            if ponder <> nextPonderMove then
              //write about ponder move change
              printfn "Ponder move change: %s %s" ponder nextPonderMove
              let evalsBefore = if indexOfMax > 4 then evals.[indexOfMax-4..indexOfMax-1] else evals.[0..indexOfMax-1]
              let evalsAfter = evals.[indexOfMax..] |> List.takeWhile (fun e -> e.Color <> max.Color || e.EvalDetail.Eval > 1.0)
              //add the next element after evalsafter with boundary check
              let lastEval = 
                let elems = evalsAfter.Length
                let idx = indexOfMax + elems           
                if idx < evals.Length then
                  evals.[idx]
                else
                  evals.[evals.Length - 1]
              let after = evalsAfter @ [lastEval]
              if after.Length < 8 then
                printfn "%s (%s) missed a potential win: big change in eval: Min eval: %f, Max eval = %f" player color min.EvalDetail.Eval max.EvalDetail.Eval
                let evals = evalsBefore @ after
                evals
              else
                []
            else
              let evalsBefore = if indexOfMax > 4 then evals.[indexOfMax-4..indexOfMax-1] else evals.[0..indexOfMax-1]
              let evalsAfter = evals.[indexOfMax..] |> List.takeWhile (fun e -> e.Color <> max.Color || e.EvalDetail.Eval > 1.0)
              //add the next element after evalsafter with boundary check
              let lastEval = 
                let elems = evalsAfter.Length
                let idx = indexOfMax + elems           
                if idx < evals.Length then
                  evals.[idx]
                else
                  evals.[evals.Length - 1]
              let after = evalsAfter @ [lastEval]
              if after.Length < 8 then
                printfn "%s (%s) missed a potential win: big change in eval: Min eval: %f, Max eval = %f" player color min.EvalDetail.Eval max.EvalDetail.Eval
                let evals = evalsBefore @ after
                evals
              else
                []
            //let evals = evalsBefore @ after

            //printfn "%s (%s) missed a potential win: big change in eval: Min eval: %f, Max eval = %f" player color min.EvalDetail.Eval max.EvalDetail.Eval
            //let evals = evalsBefore @ evalsAfter
            //evals
          else
            []
      | None -> []
              
  let isPotentialMissedWinForCeres (evalChunks: EvalDetailResult list) (whitePlayer,blackPlayer) threshold =
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
    let isDraw = result = "1/2-1/2"
    if not isDraw then
      false, [], [], String.Empty, String.Empty
    else      
      let highEvalInGame = 
        evalChunks |> List.exists (fun e -> abs e.EvalDetail.Eval > threshold)
      if highEvalInGame then        
        let highestEvalFound = evalChunks |> List.maxBy(fun e -> abs e.EvalDetail.Eval)        
        let lastSixMoves = evalChunks |> List.skip (List.length evalChunks - 6)
        let fourLatestEvalsAroundHighestEvalFound = 
          let indexOfMax = evalChunks |> List.findIndex (fun e -> e = highestEvalFound)
          let evalsBefore = if indexOfMax > 4 then evalChunks.[indexOfMax-3..indexOfMax-1] else evalChunks.[0..indexOfMax-1]
          let evalsAfter = if indexOfMax + 5 < evalChunks.Length then evalChunks.[indexOfMax..indexOfMax+5] else evalChunks[indexOfMax..evalChunks.Length - 1]
          let combined = evalsBefore @ evalsAfter
          combined

        let ply = highestEvalFound.EvalDetail.Ply
        let sideWithAdvantage = if highestEvalFound.EvalDetail.Eval > 0 then "White" else "Black"
        if highestEvalFound.Color = "w" then
          let player = if sideWithAdvantage = "White" then whitePlayer else blackPlayer
          let msg = sprintf "%s (%s) missed a potential win around ply %d: Max eval = %f after move %s" player sideWithAdvantage ply highestEvalFound.EvalDetail.Eval highestEvalFound.EvalDetail.Move
          true, fourLatestEvalsAroundHighestEvalFound, lastSixMoves, msg, player
        else
          let player = if sideWithAdvantage = "White" then whitePlayer else blackPlayer
          let msg = sprintf "%s (%s) missed a potential win around ply nr %d: Max eval = %f after move %s" player sideWithAdvantage ply highestEvalFound.EvalDetail.Eval highestEvalFound.EvalDetail.Move
          true, fourLatestEvalsAroundHighestEvalFound, lastSixMoves, msg, player
      else
        false, [], [], String.Empty, String.Empty

  let isPotentialMissedWinAlt (evalChunks: EvalDetailResult list) (player, color) =
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
    let isDraw = result = "1/2-1/2"
    if not isDraw then
      []
    
    else
      match bigEvalChange evalChunks 1.5 color with
      | Some (evals, min, max) ->
          if max.EvalDetail.Eval > 0.1 && color = "w"  || max.EvalDetail.Eval < -0.1 && color = "b" then
            let indexOfMax = evals |> List.findIndex (fun e -> e = max)
            let evalsBefore = if indexOfMax > 4 then evals.[indexOfMax-4..indexOfMax-1] else evals.[0..indexOfMax-1]
            let evalsAfter = evals.[indexOfMax..] |> List.takeWhile (fun e -> e.Color <> max.Color || e.EvalDetail.Eval > 1.0)
            //add the next element after evalsafter with boundary check
            let lastEval = 
              let elems = evalsAfter.Length
              let idx = indexOfMax + elems           
              if idx < evals.Length then
                evals.[idx]
              else
                evals.[evals.Length - 1]
            let after = evalsAfter @ [lastEval]
            if after.Length < 8 then
              printfn "%s (%s) missed a potential win: big change in eval: Min eval: %f, Max eval = %f" player color min.EvalDetail.Eval max.EvalDetail.Eval
              let evals = evalsBefore @ after
              evals
            else
              []
           
          else
            []
      | None -> []

  let isPotentialMissedDraw (evalChunks: EvalDetailResult list) (player, color) drawThreshold winningThreshold =    
    let result = if evalChunks |> List.isEmpty then "" else evalChunks.Head.Result
    if result = "1/2-1/2" || (result = "0-1" && color = "b") || (result = "1-0" && color = "w") then
      false
    else //wins or losses
      let evals =
        evalChunks
        |> List.chunkBySize 2
        |> List.choose safeDiff
      
      let bothBelowThreshold =
        evals |> List.exists (fun (a,b) -> abs a < drawThreshold && abs b < drawThreshold )
      
      if bothBelowThreshold then        
        let changedToWinning = evals |> List.exists (fun (a,b) -> abs a > winningThreshold || abs b > winningThreshold)        
        let max, min = maxAndMinEval evalChunks
        let missed =
            if max.EvalDetail.Eval > 0.1 && color = "w" then
              true
            elif max.EvalDetail.Eval < -0.1 && color = "b" then
              true
            else
              false
        if missed then
          printfn "%s (%s) missed a potential draw: big change in eval: Min eval: %f, Max eval = %f" player color min.EvalDetail.Eval max.EvalDetail.Eval
        //missed
        
        if changedToWinning && missed then
          let diffs = evals |> List.map calcDiffInEvals //(fun (a,b) -> abs (a - b))
          printfn "Big change found %s color = %s" result color
          List.iter (fun (a,b) -> printfn "Eval: %f vs %f" a b) evals
          List.iter (fun d -> printfn "Diff: %f" d) diffs
          true
        else 
          false
      else
        false

  //let appendMissedWin chunks = appendToFile "C:/Dev/Chess/PGNs/missed_wins.txt" chunks
  //let appendMissedDraw chunks = appendToFile "C:/Dev/Chess/PGNs/missed_draws.txt" chunks
  //let appendMisEvaluatedPosition chunks = appendToFile "C:/Dev/Chess/PGNs/mis_evaluated_positions.txt" chunks
  
  type Result = 
    { 
      Player : string
      MissedWin: bool
      Threshold: float
      MaxEval: float
      EndOfGameEval: float
      TwoFold : bool
      ThreeFold : bool
      FiftyMove : bool
      StaleMate : bool
      InSufficientMaterial : bool
    }
  let createResult player missedWin threshold maxEval endOfGameEval twoFold threeFold fiftyMove staleMate inSufficientMaterial =     
    {
      Player = player
      MissedWin = missedWin
      Threshold = threshold
      MaxEval = maxEval
      EndOfGameEval = endOfGameEval
      TwoFold = twoFold
      FiftyMove = fiftyMove
      ThreeFold = threeFold
      StaleMate = staleMate
      InSufficientMaterial = inSufficientMaterial
    }
  let defaultResult = 
    {
      Player = ""
      MissedWin = false
      Threshold = 0.0
      MaxEval = 0.0
      EndOfGameEval = 0.0
      TwoFold = false
      FiftyMove = false
      ThreeFold = false
      StaleMate = false
      InSufficientMaterial = false
    }

  let gameAnalysisNew path chunkSize threshold (highEvalThreshold, lowEvalThreshold) =
    // Function to calculate maximum length for each column
    let calculateMaxLengths (headers:string list) (rows: string list list) =
        let columns = headers |> List.mapi (fun i _ -> rows |> List.map (fun row -> row.[i]))
        headers
        |> List.mapi (fun i header ->
            let maxRowLength = columns.[i] |> List.map String.length |> List.max
            max (header.Length) maxRowLength
        )

    // Function to create a formatted table
    let formatTable headers rows =
        let maxLengths = calculateMaxLengths headers rows
        let createBorder =
            maxLengths
            |> List.map (fun len -> "+" + (String.replicate (len + 2) "-"))
            |> String.concat ""
            |> (+) "+"
    
        let formatRow row =
            row
            |> List.mapi (fun i col -> sprintf "| %-*s " maxLengths.[i] col)
            |> String.concat ""
            |> (+) "|"
    
        let header = formatRow headers
        let separator =
            maxLengths
            |> List.map (fun len -> "+" + (String.replicate (len + 2) "-"))
            |> String.concat ""
            |> (+) "+"
    
        let formattedRows = rows |> List.map formatRow
        [createBorder; header; separator] @ formattedRows @ [createBorder]
        |> String.concat "\n"
    
    let collectMissedWins = appendToFile "C:/Dev/Chess/PGNs/missed_wins.txt" true path
    let collectMissedDraws = appendToFile "C:/Dev/Chess/PGNs/missed_draws.txt" true path
    let collectMisEvaluated = appendToFile "C:/Dev/Chess/PGNs/mis_evaluated_positions.txt" true path
    let allPGNGames = PGNParser.parsePgnFile path |> Seq.truncate 1_000_000 |> Seq.toList
    let possibleNames = ["lc0"; "lczero"; "base"; "opt"; "ceres"]
    let nameExists (name:string) = 
      possibleNames |> List.exists (fun e -> name.Contains e)
    
    let filteredGames = 
      allPGNGames 
      |> Seq.filter (fun game -> 
          let metaData = game.GameMetaData
          let (w, b) = (metaData.White.ToLower(), metaData.Black.ToLower())
          nameExists w || nameExists b)
    
    let findPlayer = 
      match filteredGames |> Seq.tryHead with
      | Some game -> 
          let metaData = game.GameMetaData
          let white = metaData.White.ToLower()
          if nameExists white then
            metaData.White, "w"
          else
            metaData.Black, "b"
      | None -> "",""

    let sb = new System.Text.StringBuilder()
    let appendLine (msg:string) = sb.AppendLine(msg) |> ignore
    let games = allPGNGames |> Seq.length
    
    //initialize a dictionary to store one stringbuilder per player
    let playerDict = new System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()
    let appendLineToPlayer (player:string) (msg:string) =
      let sb = 
        match playerDict.TryGetValue player with
        | true, sb -> sb
        | false, _ -> 
            let sb = new System.Text.StringBuilder()
            playerDict.Add(player, sb)
            sb
      sb.AppendLine(msg) |> ignore

    //set console color to green for the following output
    Console.ForegroundColor <- ConsoleColor.Green
    printfn "\nWorking on file: %s" path
    Console.ResetColor()
    let detailDesc = sprintf "Detailed analysis of each missed win game in file %s:\n" (Path.GetFileName path)
    appendLine(detailDesc)
    let results = ResizeArray<Result>()
    let board = new Board()
    for pgn in allPGNGames do
      board.ResetBoardState()
      board.LoadFen pgn.GameMetaData.Fen
      for m in pgn.Moves do
        if m.WhiteSan <> "" then
          board.PlaySimpleShortSan m.WhiteSan
        if m.BlackSan <> "" then
          board.PlaySimpleShortSan m.BlackSan
      let evals = collectAllChessEvaluations pgn        
      let mutable currentCollector = fun _ -> ()
      let w,b = pgn.GameMetaData.White, pgn.GameMetaData.Black
      match isPotentialMissedWinForCeres evals (w,b) threshold  with      
      |true, evals, lastSixMoves, msg, player -> 
          let mEval = (evals |> List.maxBy (fun e -> abs e.EvalDetail.Eval)).EvalDetail.Eval
          let lastEval = abs (lastSixMoves |> Seq.last).EvalDetail.Eval
          let fiftyMove = board.Position.Count50 >= 100uy
          let rep = board.RepetitionNr()
          let insufficientMaterial = board.InsufficentMaterial()         
          let stalemate = board.AnyLegalMove() |> not && board.IsMate() |> not          
          let result = createResult player true threshold mEval lastEval (rep = 2) (rep = 3) fiftyMove stalemate insufficientMaterial
          results.Add(result)
          appendLineToPlayer player (sprintf "\nRound %s: %s vs %s with game result: %s" pgn.GameMetaData.Round pgn.GameMetaData.White pgn.GameMetaData.Black pgn.GameMetaData.Result)
          appendLine (sprintf "\nRound %s: %s vs %s with game result: %s" pgn.GameMetaData.Round pgn.GameMetaData.White pgn.GameMetaData.Black pgn.GameMetaData.Result)
          appendLineToPlayer player (sprintf "\t%s"  msg)
          appendLine (sprintf "\t%s"  msg)          
          //appendLine "---------------------------------------------------------"
          //printfn "%s" pgn.Raw
          //appendLine Environment.NewLine
          for e in evals do
            appendLine (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
            appendLineToPlayer player (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
          appendLine "\nlast 6 moves of the game:"
          appendLineToPlayer player "\nlast 6 moves of the game:"
          for e in lastSixMoves do
            appendLine (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
            appendLineToPlayer player (sprintf "%d %s (%s) (%s)" e.EvalDetail.Ply e.EvalDetail.Move e.Color e.EvalDetail.Comment)
          
          if lastEval > highEvalThreshold && pgn.GameMetaData.Result = "1/2-1/2" then
            if fiftyMove then 
              appendLine (sprintf "\tGame ends in high eval with 50move = %b" fiftyMove)
              appendLineToPlayer player (sprintf "\tGame ends in high eval with 50move = %b" fiftyMove)
            if stalemate then 
              appendLine (sprintf "\tGame ends in high eval with stalemate = %b" stalemate)
              appendLineToPlayer player (sprintf "\tGame ends in high eval with stalemate = %b" stalemate)
            if rep = 3 then 
              appendLine (sprintf "\tGame ends in high eval with three-fold = %b" (rep = 3))
              appendLineToPlayer player (sprintf "\tGame ends in high eval with three-fold = %b" (rep = 3))
            if rep = 2 then 
              appendLine (sprintf "\tGame ends in high eval with two-fold = %b" (rep = 2))
              appendLineToPlayer player (sprintf "\tGame ends in high eval with two-fold = %b" (rep = 2))
            if insufficientMaterial then 
              appendLine (sprintf "\tGame ends in high eval with InsufficentMaterial = %b" insufficientMaterial)            
              appendLineToPlayer player (sprintf "\tGame ends in high eval with InsufficentMaterial = %b" insufficientMaterial)
            
          else            
            appendLine "\tGame ends in low eval"
            appendLineToPlayer player "\tGame ends in low eval"
          //appendLine "---------------------------------------------------------"
          //currentCollector <- collectMissedWins pgn
          //currentCollector evals
      |false, _,_,_,_ -> 
        let lastEval = abs (evals |> List.last).EvalDetail.Eval        
        let maxEval = (evals |> List.maxBy (fun e -> e.EvalDetail.Eval)).EvalDetail.Eval        
        let fiftyMove = board.Position.Count50 >= 100uy
        let rep = board.RepetitionNr()
        let insufficientMaterial = board.InsufficentMaterial()         
        let stalemate = board.AnyLegalMove() |> not && board.IsMate() |> not 
        let whoWon = if pgn.GameMetaData.Result = "1-0" then pgn.GameMetaData.White else pgn.GameMetaData.Black
        let result = createResult whoWon false threshold maxEval lastEval (rep = 2) (rep = 3) fiftyMove stalemate insufficientMaterial
        results.Add(result)    

    let allPlayers =        
      let missedWins = results |> Seq.filter (fun r -> r.MissedWin)
      let totalMissedWins = missedWins |> Seq.length
      let totalGames = results |> Seq.length
      let missedWinRatio = (float totalMissedWins / float totalGames) * 100.0
      let endEloLower = missedWins |> Seq.filter (fun r -> abs r.EndOfGameEval < lowEvalThreshold) |> Seq.length
      let endEloHigher = missedWins |> Seq.filter (fun r -> abs r.EndOfGameEval > highEvalThreshold) |> Seq.length
      let lowEloRatio = (float endEloLower / float totalMissedWins) * 100.0
      let highEloRatio = (float endEloHigher / float totalMissedWins) * 100.0
      let allTwoFold = results |> Seq.filter (fun r -> r.TwoFold) |> Seq.length
      let twoFoldRatio = (float allTwoFold / float totalGames) * 100.0
      let allThreeFold = results |> Seq.filter (fun r -> r.ThreeFold) |> Seq.length
      let threeFoldRatio = (float allThreeFold / float totalGames) * 100.0
      let fiftyMoveRule = results |> Seq.filter (fun r -> r.FiftyMove) |> Seq.length
      let fiftyMoveRuleRatio = (float fiftyMoveRule / float totalGames) * 100.0
      let staleMate = results |> Seq.filter (fun r -> r.StaleMate) |> Seq.length
      let staleMateRatio = (float staleMate / float totalGames) * 100.0
      let insufficientMaterial = results |> Seq.filter (fun r -> r.InSufficientMaterial) |> Seq.length
      let insufficientMaterialRatio = (float insufficientMaterial / float totalGames) * 100.0
      let uniquePlayers = results |> Seq.map (fun r -> r.Player) |> Seq.filter(fun p -> p <> "") |> Seq.distinct
      let totalMissedWinsPerPlayer = 
        [for p in uniquePlayers do
          let missedWins = results |> Seq.filter (fun r -> r.Player = p && r.MissedWin)
          let totalMissedWins = missedWins |> Seq.length
          p, totalMissedWins]
        
      //get games that ends in high evals and is not stalemate
      //let highEvalNotStaleMate = results |> Seq.filter (fun r -> abs r.EndOfGameEval >= highEvalThreshold && not r.StaleMate)
      
      let headers = ["Metric"; "Count"; "Percentage"]  
      
      let rows = [
          ["Threshold for missed win in cp"; sprintf "%.1f" (threshold*100.0); "-"]
          ["Total number of games"; sprintf "%d" totalGames; "-"]
          ["Number of Missed wins"; sprintf "%d" totalMissedWins; sprintf "%.2f%%" missedWinRatio]
          for (p,missed) in totalMissedWinsPerPlayer do
            [sprintf "  Missed win for %s" p; sprintf "%d" missed; sprintf "%.2f%%" ((float missed / float totalMissedWins) * 100.0) ]
          [sprintf "  Missed win ended with evals < %.1f" lowEvalThreshold; sprintf "%d" endEloLower; sprintf "%.2f%%" lowEloRatio]
          [sprintf "  Missed win ended with evals > %.1f" highEvalThreshold; sprintf "%d" endEloHigher; sprintf "%.2f%%" highEloRatio]
          ["Games ended with Two-fold repetition"; sprintf "%d" allTwoFold; sprintf "%.2f%%" twoFoldRatio]
          ["Games ended with Three-fold repetition"; sprintf "%d" allThreeFold; sprintf "%.2f%%" threeFoldRatio]
          ["Games ended in stalemate"; sprintf "%d" staleMate; sprintf "%.2f%%" staleMateRatio]
          ["Games with fifty-move rule adjudication"; sprintf "%d" fiftyMoveRule; sprintf "%.2f%%" fiftyMoveRuleRatio]
          ["Games with insufficient material"; sprintf "%d" insufficientMaterial; sprintf "%.2f%%" insufficientMaterialRatio]
      ]

      let summary = sprintf "```\nSummary of the analysis for all games in PGN:\n" + formatTable headers rows + "\n```"
      printfn "%s" summary      
      games, summary, sb.ToString()    
    [allPlayers]

  let gameAnalysisFromFolderAndSubFolder folderPath chunkSize threshold (highEvalThreshold, lowEvalThreshold) =
    //create a file to write the results to - that appends the results
    let writer = new StreamWriter("C:/Dev/Chess/PGNs/evals.txt")
    
    let files = Directory.EnumerateFiles(folderPath, "*.pgn", SearchOption.AllDirectories)
    let mutable games = 0
    
    let desc = 
        "\nDescription:\n" + 
        $"1. The threshold for a missed win is set to {threshold} in eval.\n" +
        "2. The missed win percentage is calculated as (total missed wins / total games) * 100.\n" +
        $"3. The end-of-game evaluations lower than {lowEvalThreshold} and higher than {highEvalThreshold} are calculated by filtering the results.\n" +
        "4. The two/three-fold repetition percentage is calculated as (total two-fold+ repetitions / total games) * 100.\n" +
        "5. When a game ends with high eval it is almost certain to be because of two fold repetitions.\n" +
        "6. Missed wins can occur naturally because of suboptimal play during game with low eval at the end - which is fine.\n" +
        "7. The fifty-move rule percentage is calculated as (total games with fifty-move rule adjudication / total games) * 100.\n" +
        "8. The stalemate percentage is calculated as (total games that ends in stalemate / total games) * 100.\n" +
        "9. The insufficient material percentage is calculated as (total games with insufficient material / total games) * 100.\n"

    printfn "%s" desc
    writer.WriteLine(desc)
    writer.WriteLine("---------------------------------------------------------")
    for file in files do        
        writer.WriteLine($"Working on file: {file}")
        //writer.WriteLine("---------------------------------------------------------\n")
        
        for numberOfGames, summary, detailSummary in gameAnalysisNew file chunkSize threshold (highEvalThreshold, lowEvalThreshold) do
            writer.WriteLine(summary)        
            writer.WriteLine(detailSummary)
            writer.WriteLine("---------------------------------------------------------")
            games <- games + numberOfGames
        
    writer.Flush()
    printfn "Total number of games analyzed: %d" games
