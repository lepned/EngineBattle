module ChessLibrary.LowLevelUtilities

open System
open System.IO
open System.Text
open System.Diagnostics
open QBBOperations
open TypesDef.Position
open TypesDef.TMove
open MoveGeneration

module Agents =

  // Define a message type for the agent
  type OutputMessage =
      | Line of string
      | Stop

  // Define types for UCI commands and responses
  type UciCommand =
      | Uci
      | IsReady
      | Position of string
      | Go of string * string
      | Quit
  
  type CommandMessage =
      | Schedule of UciCommand * int // UciCommand and delay in milliseconds
      | StopScheduling


  type UciResponse =
      | Id of string * string
      | UciOk
      | ReadyOk
      | BestMove of string
      | Info of string
      | Unknown of string

// Function to parse UCI responses
  let parseUciResponse (line: string) : UciResponse option =
      let tokens = line.Split(' ')
      match tokens.[0] with
      | "id" when tokens.Length > 2 -> Some(Id(tokens.[1], String.Join(" ", tokens.[2..])))
      | "uciok" -> Some UciOk
      | "readyok" -> Some ReadyOk
      | "bestmove" when tokens.Length > 1 -> Some(BestMove(tokens.[1]))
      | "info" -> Some(Info(line))
      | _ -> Some(Unknown(line))

  // Function to handle UCI responses
  let handleUciResponse (response: UciResponse) =
      match response with
      | Id(name, value) -> Console.WriteLine(sprintf "ID: %s %s" name value)
      | UciOk -> Console.WriteLine("UCI OK")
      | ReadyOk -> Console.WriteLine("Ready OK")
      | BestMove move -> Console.WriteLine(sprintf "Best Move: %s" move)
      | Info info -> Console.WriteLine(sprintf "Info: %s" info)
      | Unknown line -> Console.WriteLine(sprintf "Unknown response: %s" line)

  // Function to create an agent for reading output
  let createOutputAgent () =
      MailboxProcessor.Start(fun inbox ->
          let rec loop () = async {
              let! msg = inbox.Receive()
              match msg with
              | Line line -> 
                  match parseUciResponse line with
                  | Some response -> handleUciResponse response
                  | None -> Console.WriteLine("Unknown response: " + line)
                  return! loop()
              | Stop -> 
                  () // Exit the loop
          }
          loop ())

  // Function to create an agent for scheduling commands
  let createCommandSchedulingAgent (sendCommand: UciCommand -> unit) =
      MailboxProcessor.Start(fun inbox ->
          let rec loop () = async {
              let! msg = inbox.Receive()
              match msg with
              | Schedule (command, delay) ->
                  do! Async.Sleep delay
                  sendCommand(command)
                  return! loop()
              | StopScheduling -> 
                  () // Exit the loop
          }
          loop ())

  // Function to create an agent for logging
  let createLoggingAgent (logFilePath: string) =
      let logWriter = new StreamWriter(logFilePath, true)
      MailboxProcessor.Start(fun inbox ->
          let rec loop () = async {
              let! msg = inbox.Receive()
              match msg with
              | Line line -> 
                  let logLine = sprintf "[%s] %s" (DateTime.Now.ToString("o")) line
                  logWriter.WriteLine(logLine)
                  logWriter.Flush()
                  return! loop()
              | Stop -> 
                  logWriter.Close()
                  () // Exit the loop
          }
          loop ())
  
  // Function to run a UCI-compatible chess engine
  let runUciChessEngine (executablePath: string) =
      try
          // Create a new process
          let myProcess = new Process()
          myProcess.StartInfo.FileName <- executablePath

          // Set up process to capture input, output, and error
          myProcess.StartInfo.RedirectStandardInput <- true
          myProcess.StartInfo.RedirectStandardOutput <- true
          myProcess.StartInfo.RedirectStandardError <- true
          myProcess.StartInfo.UseShellExecute <- false
          myProcess.StartInfo.CreateNoWindow <- true

          // Start the process
          myProcess.Start() |> ignore

          // Get the standard input, output, and error streams
          let inputWriter = myProcess.StandardInput
          let outputReader = myProcess.StandardOutput
          let errorReader = myProcess.StandardError

          // Create agents for output and error streams
          let outputAgent = createOutputAgent()
          let errorAgent = createOutputAgent()
          let loggingAgent = createLoggingAgent("chess_engine.log")

          // Function to send a command directly
          let sendCommandDirectly (command: UciCommand) =
              let commandStr = 
                  match command with
                  | Uci -> "uci"
                  | IsReady -> "isready"
                  | Position pos -> sprintf "position %s" pos
                  | Go (nodes, number) -> sprintf "go %s %s" nodes number
                  | Quit -> "quit"
              inputWriter.WriteLine(commandStr)
              inputWriter.Flush()

          // Create a command scheduling agent
          let commandSchedulingAgent = createCommandSchedulingAgent(sendCommandDirectly)

          // Function to read stream asynchronously and send lines to the agent
          let readStreamAsync (reader: StreamReader) (agent: MailboxProcessor<OutputMessage>) =
              async {
                  try
                      while not reader.EndOfStream do
                          let line = reader.ReadLine()
                          agent.Post(Line line)
                          loggingAgent.Post(Line line)
                      agent.Post(Stop)
                      loggingAgent.Post(Stop)
                  with ex -> 
                      Console.WriteLine("Exception: " + ex.Message)
                      agent.Post(Stop)
                      loggingAgent.Post(Stop)
              }

          // Start reading output and error streams asynchronously
          let outputTask = readStreamAsync outputReader outputAgent |> Async.StartAsTask
          let errorTask = readStreamAsync errorReader errorAgent |> Async.StartAsTask

          // Function to send a command with or without delay
          let sendCommand (command: UciCommand) =
              match command with
              | Go (nodes, number) -> commandSchedulingAgent.Post(Schedule(command, 500))
              | _ -> sendCommandDirectly(command)

          // Function to handle user input
          let rec handleUciCommands () =
              printf "Enter UCI command: "
              let commandStr = Console.ReadLine()
              if String.IsNullOrEmpty(commandStr) then
                  Console.WriteLine("engine is shutting down?")
              else
                match commandStr.Split(' ') with
                | [| "uci" |] -> sendCommand Uci
                | [| "isready" |] -> sendCommand IsReady
                | [| "position"; pos |] -> sendCommand (Position pos)
                | [| "go"; nodes; number |] -> sendCommand (Go (nodes, number))
                | [| "quit" |] -> sendCommand Quit
                | _ -> Console.WriteLine("Unknown command")
                handleUciCommands()

          // Start handling UCI commands
          handleUciCommands()

          // Wait for the process to exit
          myProcess.WaitForExit()

          // Wait for the output reading tasks to complete
          outputTask.Wait()
          errorTask.Wait()

      with ex ->
          Console.WriteLine("Exception: " + ex.Message)

  let testUciAgents () =     
      Console.WriteLine("Usage: ChessEngineAgent <path-to-uci-engine>")
      printfn "Hello World from F# agents!"   
      let ceresPath = @"C:/Dev/Chess/Engines/Ceres/v0.97RC3/Ceres.exe"
      runUciChessEngine(ceresPath)

module ConsoleUtils =

  let positiveInfinitySymbol = "\u221E" // Positive infinity symbol
  let negativeInfinitySymbol = "\u221E" // Negative infinity symbol
  let mutable originalColor = ConsoleColor.Gray
  let mutable originalBGColor = ConsoleColor.Black

// Function to print text in a specified color
  let printInColor (color: ConsoleColor) (text: string) =
    //let originalColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printfn "%s" text
    Console.ForegroundColor <- originalColor
  
  let originalConsoleColor (text: string) = printInColor originalColor text
  let greenConsole (text: string) = printInColor ConsoleColor.Green text
  let redConsole (text: string) = printInColor ConsoleColor.Red text
  let yellowConsole (text: string) = printInColor ConsoleColor.Yellow text

module BoardHelper =
  
  let frcFen = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
  let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  let normalCastle = ['Q'; 'K'; 'q'; 'k']

  let charToNumber (c : char) white =
    let lowerCase = Char.ToLower c
    let baseChar = 'a'
    let file = int lowerCase - int baseChar
    if white then
      file
    else
      56 + file

  let getPosFromFen (fenOption: string option) =
    let fen = defaultArg fenOption start
    let mutable pos = Position.Default
    pos.EnPassant <- 8uy
    pos.STM <- PositionOps.WHITE
    pos.CastleFlags <- 0uy
    let mutable square = 0
    let mutable cursor = 0
    while fen.[cursor] <> ' ' do
      let cur = fen.[cursor]
      if cur >= '1' && cur <= '8' then
          square <- square + (int (cur - '0'))
      elif cur <> '/' then
        let bit = OppSq square
        let pieceside = if Char.IsUpper cur then PositionOps.WHITE else PositionOps.BLACK
        let mutable piece = 
          match cur with
          | 'p' -> uint64 TPieceType.PAWN
          | 'n' -> uint64 TPieceType.KNIGHT
          | 'b' -> uint64 TPieceType.BISHOP
          | 'r' -> uint64 TPieceType.ROOK
          | 'q' -> uint64 TPieceType.QUEEN
          | 'k' -> uint64 TPieceType.KING
          | 'P' -> uint64 TPieceType.PAWN 
          | 'N' -> uint64 TPieceType.KNIGHT 
          | 'B' -> uint64 TPieceType.BISHOP 
          | 'R' -> uint64 TPieceType.ROOK 
          | 'Q' -> uint64 TPieceType.QUEEN
          | 'K' -> uint64 TPieceType.KING 
          | _ -> failwith "Invalid character in FEN"
        pos.P0 <- pos.P0 ||| ((piece &&& 1UL) <<< bit)  //001
        pos.P1 <- pos.P1 ||| (((piece >>> 1) &&& 1UL) <<< bit)  //010
        pos.P2 <- pos.P2 ||| ((piece >>> 2) <<< bit)  //100
        if pieceside = PositionOps.WHITE then
            pos.PM <- pos.PM ||| (1UL <<< bit)
            piece <- piece ||| uint64 PositionOps.BLACK
        square <- square + 1
      cursor <- cursor + 1

    cursor <- cursor + 1  
    let sidetomove = 
        match fen.[cursor] with
        | 'w' -> PositionOps.WHITE
        | 'b' -> PositionOps.BLACK
        | _ -> failwith "Invalid character in FEN"
    cursor <- cursor + 2
    
    //set the rook initial positions for castling
    let (wKr,wQr),(bKr,bQr) = PositionOps.getRookPositionsForCastling &pos
    pos.RookInfo.WhiteKRInitPlacement <- (byte)wKr 
    pos.RookInfo.WhiteQRInitPlacement <- (byte)wQr 
    pos.RookInfo.BlackKRInitPlacement <- (byte)(bKr-56) 
    pos.RookInfo.BlackQRInitPlacement <- (byte)(bQr-56)
    
    if fen.[cursor] <> '-' then  
        let mutable i = cursor
        let len = fen.Length
        if normalCastle |> List.contains fen.[i] then
            while i < len && fen.[i] <> ' ' do
                let cur = fen.[i]
                match cur with
                | 'K' -> pos.CastleFlags <- pos.CastleFlags ||| 0x02uy
                | 'Q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x01uy
                | 'k' -> pos.CastleFlags <- pos.CastleFlags ||| 0x20uy
                | 'q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x10uy
                | _ -> failwith "Invalid character in FEN"
                i <- i + 1
            cursor <- i + 1          
        else //chess960 castling logic
          while i < len && fen.[i] <> ' ' do
            let cur = fen.[i]
            let color = if Char.IsUpper cur then true else false
            let curRookPlacement = charToNumber cur color             
            match cur with
            | _ when Char.IsUpper cur -> // Adjust for white's castling flags in Chess960                
                if pos.RookInfo.WhiteKRInitPlacement = (byte)curRookPlacement then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x02uy                  
                elif pos.RookInfo.WhiteQRInitPlacement = (byte)curRookPlacement then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x01uy
                elif normalCastle |> List.exists(fun e -> e = cur) then
                  match cur with
                  | 'K' -> pos.CastleFlags <- pos.CastleFlags ||| 0x02uy
                  | 'Q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x01uy
                  | _ -> failwith "Invalid character in FEN"
                else
                  failwith "Invalid character in FEN"                  
            | _ when Char.IsLower cur -> // Adjust for black's castling flags in Chess960
                if pos.RookInfo.BlackKRInitPlacement = (byte)(curRookPlacement-56) then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x20uy                  
                elif pos.RookInfo.BlackQRInitPlacement = (byte)(curRookPlacement-56) then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x10uy 
                elif normalCastle |> List.exists(fun e -> e = cur) then
                  match cur with
                  | 'k' -> pos.CastleFlags <- pos.CastleFlags ||| 0x20uy
                  | 'q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x10uy
                  | _ -> failwith "Invalid character in FEN"
                else
                  failwith "Invalid character in FEN"                   
            | '-' -> () // No castling available
            | _ -> failwith "Invalid character in FEN"
            i <- i + 1
        cursor <- i + 1        
    else
        cursor <- cursor + 2
    if fen.Length >= cursor && fen.[cursor] <> '-' then  // Read the enpassant column
        pos.EnPassant <- byte (fen.[cursor] - 'a')
    cursor <- cursor + 2
    if fen.Length > cursor then
      let rest = fen.Substring(cursor).TrimStart().Split(' ')
      if rest.Length = 2 then
        pos.Count50 <- byte rest.[0]
        let ply = byte rest.[1]
        if ply > 0uy then
          pos.Ply <-  (if sidetomove = PositionOps.BLACK then (ply - 1uy) * 2uy + 1uy else (ply - 1uy) * 2uy)
    if sidetomove = PositionOps.BLACK then
        PositionOps.changeSide (&pos)
    pos
    
  
  let loadFen (fenOption: string option, position: Position outref) =
    let fen = defaultArg fenOption start
    let mutable pos = Position.Default
    pos.EnPassant <- 8uy
    pos.STM <- PositionOps.WHITE
    pos.CastleFlags <- 0uy
    let mutable square = 0
    let mutable cursor = 0
    while fen.[cursor] <> ' ' do
      let cur = fen.[cursor]
      if cur >= '1' && cur <= '8' then
          square <- square + (int (cur - '0'))
      elif cur <> '/' then
        let bit = OppSq square
        let pieceside = if Char.IsUpper cur then PositionOps.WHITE else PositionOps.BLACK
        let mutable piece = 
          match cur with
          | 'p' -> uint64 TPieceType.PAWN
          | 'n' -> uint64 TPieceType.KNIGHT
          | 'b' -> uint64 TPieceType.BISHOP
          | 'r' -> uint64 TPieceType.ROOK
          | 'q' -> uint64 TPieceType.QUEEN
          | 'k' -> uint64 TPieceType.KING
          | 'P' -> uint64 TPieceType.PAWN 
          | 'N' -> uint64 TPieceType.KNIGHT 
          | 'B' -> uint64 TPieceType.BISHOP 
          | 'R' -> uint64 TPieceType.ROOK 
          | 'Q' -> uint64 TPieceType.QUEEN
          | 'K' -> uint64 TPieceType.KING 
          | _ -> failwith "Invalid character in FEN"
        pos.P0 <- pos.P0 ||| ((piece &&& 1UL) <<< bit)  //001
        pos.P1 <- pos.P1 ||| (((piece >>> 1) &&& 1UL) <<< bit)  //010
        pos.P2 <- pos.P2 ||| ((piece >>> 2) <<< bit)  //100
        if pieceside = PositionOps.WHITE then
            pos.PM <- pos.PM ||| (1UL <<< bit)
            piece <- piece ||| uint64 PositionOps.BLACK
        square <- square + 1
      cursor <- cursor + 1

    cursor <- cursor + 1  
    let sidetomove = 
        match fen.[cursor] with
        | 'w' -> PositionOps.WHITE
        | 'b' -> PositionOps.BLACK
        | _ -> failwith "Invalid character in FEN"
    cursor <- cursor + 2
    
    //set the rook positions for rookInfo
    let (wKr,wQr),(bKr,bQr) = PositionOps.getRookPositionsForCastling &pos
    pos.RookInfo.WhiteKRInitPlacement <- (byte)wKr 
    pos.RookInfo.WhiteQRInitPlacement <- (byte)wQr 
    pos.RookInfo.BlackKRInitPlacement <- (byte)(bKr-56) 
    pos.RookInfo.BlackQRInitPlacement <- (byte)(bQr-56)

    if fen.[cursor] <> '-' then  
        let mutable i = cursor
        let len = fen.Length
        
        if normalCastle |> List.contains fen.[i] then
            while i < len && fen.[i] <> ' ' do
                let cur = fen.[i]
                match cur with
                | 'K' -> pos.CastleFlags <- pos.CastleFlags ||| 0x02uy
                | 'Q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x01uy 
                | 'k' -> pos.CastleFlags <- pos.CastleFlags ||| 0x20uy 
                | 'q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x10uy
                | _ -> failwith "Invalid character in FEN"
                i <- i + 1
            cursor <- i + 1            
        else

          while i < len && fen.[i] <> ' ' do
            let cur = fen.[i]
            let color = if Char.IsUpper cur then true else false
            let curRookPlacement = charToNumber cur color 
            
            match cur with
            | _ when Char.IsUpper cur -> // Adjust for white's castling flags in Chess960                
                if pos.RookInfo.WhiteKRInitPlacement = (byte)curRookPlacement then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x02uy                  
                elif pos.RookInfo.WhiteQRInitPlacement = (byte)curRookPlacement then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x01uy
                elif normalCastle |> List.exists(fun e -> e = cur) then
                  match cur with
                  | 'K' -> pos.CastleFlags <- pos.CastleFlags ||| 0x02uy
                  | 'Q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x01uy
                  | _ -> failwith "Invalid character in FEN"
                else
                  failwith "Invalid character in FEN"                  
            | _ when Char.IsLower cur -> // Adjust for black's castling flags in Chess960
                if pos.RookInfo.BlackKRInitPlacement = (byte)(curRookPlacement-56) then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x20uy                  
                elif pos.RookInfo.BlackQRInitPlacement = (byte)(curRookPlacement-56) then
                  pos.CastleFlags <- pos.CastleFlags ||| 0x10uy 
                elif normalCastle |> List.exists(fun e -> e = cur) then
                  match cur with
                  | 'k' -> pos.CastleFlags <- pos.CastleFlags ||| 0x20uy
                  | 'q' -> pos.CastleFlags <- pos.CastleFlags ||| 0x10uy
                  | _ -> failwith "Invalid character in FEN"
                else
                  failwith "Invalid character in FEN"                   
            | '-' -> () // No castling available
            | _ -> failwith "Invalid character in FEN"
            i <- i + 1
        cursor <- i + 1

    else
        cursor <- cursor + 2
    if fen.[cursor] <> '-' then  // Read the enpassant column
        pos.EnPassant <- byte (fen.[cursor] - 'a')
    cursor <- cursor + 2
    if fen.Length > cursor then
      let rest = fen.Substring(cursor).TrimStart().Split(' ')
      if rest.Length = 2 then
        pos.Count50 <- byte rest.[0]
        let ply = byte rest.[1]
        if ply > 0uy then
          pos.Ply <-  (if sidetomove = PositionOps.BLACK then (ply - 1uy) * 2uy + 1uy else (ply - 1uy) * 2uy) 
    if sidetomove = PositionOps.BLACK then
        PositionOps.changeSide (&pos)
    position <- pos
  
  let Illegal (move: TMove inref) (position: _ inref) =
      let from = 1UL <<< int move.From
      let mutable toSq = 1UL <<< int move.To
      let mutable king = 0UL
      let mutable kingsq = 0
      let mutable newoccupation = ((PositionOps.occupation &position) ^^^ from) ||| toSq
      let mutable newopposing = (PositionOps.opposing &position) &&& ~~~toSq
      let mutable legal = true
      
      if (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY then         
         let rooks = PositionOps.rooksM &position
         let shortM = MSB rooks |> byte
         let rook = ExtractLSB rooks
         let kings = PositionOps.kings &position
         let kingExt = kings &&& PositionOps.sideToMove &position
         king <- toSq
         kingsq <- int move.To
         if toSq = rook then //chess960 castling
           //remove the king from the old occupation
           newoccupation <- newoccupation &&& ~~~kingExt &&& ~~~rook
           if rook > kingExt then
              king <- 1UL <<< 6
              kingsq <- 6
           else
              king <- 1UL <<< 2
              kingsq <- 2
           //add the king to the new occupation
           newoccupation <- newoccupation ||| king
         elif shortM = move.To then //chess960 castling
           let rookS = 1UL <<< (int shortM)
           //remove the king and rook from the old occupation
           newoccupation <- newoccupation &&& ~~~kingExt &&& ~~~rookS
           if rookS > kingExt then
              king <- 1UL <<< 6
              kingsq <- 6
           else
              king <- 1UL <<< 2
              kingsq <- 2
           //add the king to the new occupation
           newoccupation <- newoccupation ||| king

      elif legal && (move.MoveType &&& TPieceType.PIECE_MASK) = TPieceType.KING then          
          king <- toSq
          kingsq <- int move.To
          if (kingsq > 63) then
            legal <- false

      else
          king <- PositionOps.kings &position &&& PositionOps.sideToMove &position        
          kingsq <- int (LSB king)
          if (kingsq > 63) then
            legal <- false
          if legal && (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY then
              let epmask = toSq >>> 8
              newopposing <- newopposing ^^^ epmask
              newoccupation <- newoccupation ^^^ epmask
      if legal then
        let mutable mask = KnightDest.[kingsq] &&& newopposing
        if legal && mask <> 0UL && (mask &&& PositionOps.knights &position) > 0UL then
            legal <- false

        mask <- (((king <<< 9) &&& 0xFEFEFEFEFEFEFEFEUL) ||| ((king <<< 7) &&& 0x7F7F7F7F7F7F7F7FUL)) &&& newopposing
        if legal && mask <> 0UL && (mask &&& PositionOps.pawns &position) > 0UL then
            legal <- false

        mask <- PositionOps.queenOrBishops &position &&& newopposing
        if legal && mask <> 0UL && (mask &&&  GenBishop(kingsq, newoccupation)) > 0UL then
            let bishops = GenBishop(kingsq, newoccupation)
            legal <- false

        mask <- PositionOps.queenOrRooks &position &&& newopposing
        if legal && mask <> 0UL && (mask &&& GenRook(kingsq, newoccupation)) > 0UL then
            legal <- false

        if legal then 
          mask <- newopposing &&& KingDest.[kingsq]
          mask <> 0UL && (mask &&& PositionOps.kings &position) > 0UL
        else
          true
      else
        true
  
  //only testing and debugging
  let flipVertical (bitboard: uint64) =
    let k1 = 0x00FF00FF00FF00FFUL
    let k2 = 0x0000FFFF0000FFFFUL
    let x = bitboard
    let x = ((x >>> 8) &&& k1) ||| ((x &&& k1) <<< 8)
    let x = ((x >>> 16) &&& k2) ||| ((x &&& k2) <<< 16)
    let x = (x >>> 32) ||| (x <<< 32)
    x
  
  let posToFen (position : Position) =
    let stm = position.STM
    let mutable pos = position
    if stm = PositionOps.BLACK then 
      PositionOps.changeSide(&pos)
    let pieceChar (side: byte) (pieceType: uint64) =
      let piece = byte pieceType |> LanguagePrimitives.EnumOfValue
      match piece with
      | TPieceType.PAWN -> if side = PositionOps.WHITE then 'P' else 'p'
      | TPieceType.KNIGHT -> if side = PositionOps.WHITE then 'N' else 'n'
      | TPieceType.BISHOP -> if side = PositionOps.WHITE then 'B' else 'b'
      | TPieceType.ROOK -> if side = PositionOps.WHITE then 'R' else 'r'
      | TPieceType.QUEEN -> if side = PositionOps.WHITE then 'Q' else 'q'
      | TPieceType.KING -> if side = PositionOps.WHITE then 'K' else 'k'
      | _ -> failwith (sprintf "Invalid piece type in posToFen: received: %A converted to: %A in position %A" pieceType piece position)

    let sb = StringBuilder()
    for row in [7; 6; 5; 4; 3; 2; 1; 0] do
        let mutable emptyCount = 0
        for col in [0..7] do
            let square = row * 8 + col
            let bit = 1UL <<< square
            let p0 = (pos.P0 >>> square) &&& 1UL
            let p1 = (pos.P1 >>> square) &&& 1UL
            let p2 = (pos.P2 >>> square) &&& 1UL
            let pieceType = p0 ||| (p1 <<< 1) ||| (p2 <<< 2)

            if pieceType = 0UL then
                emptyCount <- emptyCount + 1
            else
                if emptyCount > 0 then
                    sb.Append emptyCount |> ignore
                    emptyCount <- 0
                let side = if (pos.PM &&& bit) = 0UL then PositionOps.BLACK else PositionOps.WHITE
                sb.Append(string (pieceChar side pieceType)) |> ignore

            if col = 7 then
                if emptyCount > 0 then
                    sb.Append(string emptyCount) |> ignore
                if row > 0 then
                    sb.Append("/") |> ignore

    sb.Append(" " + (if stm = PositionOps.WHITE then "w" else "b") + " ") |> ignore
    let numberToChar n = char (byte 'a' + n)
    if pos.CastleFlags = 0uy then
        sb.Append("-") |> ignore

    elif PositionOps.isFRC &pos then
      if (pos.CastleFlags &&& 0x02uy) <> 0uy then sb.Append (numberToChar pos.RookInfo.WhiteKRInitPlacement |>Char.ToUpper) |> ignore
      if (pos.CastleFlags &&& 0x01uy) <> 0uy then sb.Append (numberToChar pos.RookInfo.WhiteQRInitPlacement |>Char.ToUpper) |> ignore
      if (pos.CastleFlags &&& 0x20uy) <> 0uy then sb.Append (numberToChar pos.RookInfo.BlackKRInitPlacement) |> ignore
      if (pos.CastleFlags &&& 0x10uy) <> 0uy then sb.Append (numberToChar pos.RookInfo.BlackQRInitPlacement) |> ignore
      
    else
        if (pos.CastleFlags &&& 0x02uy) <> 0uy then sb.Append "K" |> ignore
        if (pos.CastleFlags &&& 0x01uy) <> 0uy then sb.Append "Q" |> ignore
        if (pos.CastleFlags &&& 0x20uy) <> 0uy then sb.Append "k" |> ignore
        if (pos.CastleFlags &&& 0x10uy) <> 0uy then sb.Append "q" |> ignore
        
    let epChar = char (byte 'a' + pos.EnPassant) |> string
    let epStr = if stm = PositionOps.WHITE then (epChar + string 6) else (epChar + string 3)    
    let moveNr = pos.Ply / 2uy + 1uy    

    sb.Append(" " + (if pos.EnPassant = 8uy then "-" else epStr)) |> ignore
    sb.Append(" " + string pos.Count50 + " " + moveNr.ToString()) |> ignore
    
    sb.ToString()

  let writeBoardStateFromFENToConsole header (fen: string) =
    let mutable position = Position.Default
    loadFen(Some fen, &position)
    PositionOpsToString(header, &position) |> printfn "%s"
