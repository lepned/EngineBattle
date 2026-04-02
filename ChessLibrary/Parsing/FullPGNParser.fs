/// Full PGN parser using Span-based scanning for maximum performance.
/// Supports variations, comments, NAGs, and produces PgnGame output.
module ChessLibrary.FullPGNParser

open System
open System.IO
open System.Collections.Generic
open ChessLibrary.PGNTypes
open ChessLibrary.MiscTypes
open TypesDef.CoreTypes

// ============================================================================
// Shared state for parsing
// ============================================================================

// Header state
let mutable private gameNumber = 0
let mutable private event' = ""
let mutable private site = ""
let mutable private date = ""
let mutable private round = ""
let mutable private white = ""
let mutable private black = ""
let mutable private result = ""
let mutable private resultFoundInMovetext = false
let mutable private fen = ""
let mutable private reason = ResultReason.NotStarted
let mutable private openingHash = ""
let mutable private gameTime = 0L
let mutable private plyCount = 0
let mutable private openingName = ""
let mutable private deviations = 0
let private otherTags = ResizeArray<Header>()

let mutable private currentMoveNr = 1
let mutable private whiteSan = ""
let mutable private blackSan = ""
let mutable private whiteComment = ""
let mutable private blackComment = ""
let mutable private pendingComment = ""

// Variation-aware tree state
let mutable private currentPly = 0
let private mainlinePly = ResizeArray<PlyMove>(128)
let private rootVariations = ResizeArray<PlyLine>()
let mutable private currentLine: PlyLine = mainlinePly
let private lineStack = Stack<PlyLine>()
let private plyStack = Stack<int>()
let mutable private lastParsedSan: string option = None

// NAG state
let mutable private pendingNags = ResizeArray<int>()

// ============================================================================
// Helper functions (inline for performance)
// ============================================================================

let inline private isDigit c = c >= '0' && c <= '9'
let inline private isFile c = c >= 'a' && c <= 'h'
let inline private isRank c = c >= '1' && c <= '8'
let inline private isPiece c = c = 'K' || c = 'Q' || c = 'R' || c = 'B' || c = 'N'
let inline private isPromoPiece c = isPiece c || c = 'k' || c = 'q' || c = 'r' || c = 'b' || c = 'n'

// ============================================================================
// State management
// ============================================================================

let private resetState () =
  event' <- ""
  site <- ""
  date <- ""
  round <- ""
  white <- ""
  black <- ""
  result <- ""
  resultFoundInMovetext <- false
  fen <- ""
  reason <- ResultReason.NotStarted
  openingHash <- ""
  gameTime <- 0L
  plyCount <- 0
  openingName <- ""
  deviations <- 0
  otherTags.Clear()
  currentMoveNr <- 1
  whiteSan <- ""
  blackSan <- ""
  whiteComment <- ""
  blackComment <- ""
  pendingComment <- ""
  currentPly <- 0
  mainlinePly.Clear()
  rootVariations.Clear()
  currentLine <- mainlinePly
  lineStack.Clear()
  plyStack.Clear()
  lastParsedSan <- None
  pendingNags.Clear()

let private hasGame () =
   whiteSan <> "" || blackSan <> "" || mainlinePly.Count > 0

let private hasHeaders () =
  white <> "" || black <> "" || event' <> "" || result <> ""

// ============================================================================
// PlyMove tree management
// ============================================================================

let private appendPlyMove (san: string) (color: string) =
  let ply = currentPly
  let moveNr = (ply / 2) + 1
  let nags = if pendingNags.Count > 0 then pendingNags |> Seq.toList else []
  let node =
    { Ply = ply
      MoveNumber = moveNr
      Color = color
      San = san
      Comment = ""
      Nags = nags
      Variations = ResizeArray() }
  currentLine.Add node
  currentPly <- currentPly + 1
  lastParsedSan <- Some san
  pendingNags.Clear()
  if color = "b" then
      whiteSan <- ""
      blackSan <- ""
      whiteComment <- ""
      blackComment <- ""

let private tryUpdateLastNodeComment (cmt: string) =
  if not (String.IsNullOrWhiteSpace cmt) then
    match currentLine |> Seq.tryLast with
    | Some node -> node.Comment <- cmt; true
    | None ->
        match rootVariations |> Seq.tryLast with
        | Some line when line.Count > 0 ->
            let lastNode = line[line.Count - 1]
            lastNode.Comment <- cmt
            true
        | _ -> false
  else
    false

// ============================================================================
// Text-based header parsing
// ============================================================================

let private parseHeaderTexLine (line: string) =
  let spanLine = line.AsSpan()
  let openBracket = spanLine.IndexOf('[')
  let closeBracket = spanLine.LastIndexOf(']')
  if openBracket >= 0 && closeBracket > openBracket then
    let headerSpan = spanLine.Slice(openBracket + 1, closeBracket - openBracket - 1)
    let firstQuote = headerSpan.IndexOf('"')
    let lastQuote = headerSpan.LastIndexOf('"')
    if firstQuote >= 0 && lastQuote > firstQuote then
      let key = headerSpan.Slice(0, firstQuote).Trim().ToString()
      let value = headerSpan.Slice(firstQuote + 1, lastQuote - firstQuote - 1).ToString()
      match key with
      | "Event" -> event' <- value
      | "Site" -> site <- value
      | "Date" -> date <- value
      | "Round" -> round <- value
      | "White" -> white <- value
      | "Black" -> black <- value
      | "Result" -> result <- value
      | "FEN" | "Fen" -> fen <- value
      | "Reason" ->
          try reason <- stringToResultReason value
          with _ -> () // Ignore invalid reason values
      | "OpeningHash" -> openingHash <- value
      | "GameTime" ->
          match Int64.TryParse(value) with
          | true, v -> gameTime <- v
          | _ -> ()
      | "Ply" ->
          match Int32.TryParse(value) with
          | true, v -> plyCount <- v
          | _ -> ()
      | "Opening" ->
          openingName <- value
          otherTags.Add({ Key = key; Value = value })
      | "Deviations" ->
          match Int32.TryParse(value) with
          | true, v -> deviations <- v
          | _ -> ()
      | _ -> otherTags.Add({ Key = key; Value = value })

// ============================================================================
// Span-based movetext parsing (iterative, with variation stack)
// ============================================================================

/// Parse movetext from a span, handling variations iteratively with an explicit stack
let private parseMoveTextLine (line: string) =
  let spanLine = line.AsSpan()
  let len = spanLine.Length
  let mutable p = 0
  // Skip leading whitespace
  while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1

  // Handle full-line comments
  if p < len && (spanLine[p] = ';' || spanLine[p] = '%') then
    // Line comment - extract and store as pending
    if p + 1 < len then
      pendingComment <- new string(spanLine.Slice(p + 1).Trim())
    p <- len
  else
    while p < len do
      let prevP = p

      // Skip whitespace
      while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1
      if p >= len then ()
      else
        let c0 = spanLine[p]

        // New header encountered - stop parsing this line
        if c0 = '[' then
          p <- len

        // Comment block { ... }
        elif c0 = '{' then
          p <- p + 1
          let startComment = p
          while p < len && spanLine[p] <> '}' do p <- p + 1
          let comment =
            if p > startComment then new string(spanLine.Slice(startComment, p - startComment).Trim())
            else ""
          if p < len && spanLine[p] = '}' then p <- p + 1
          let mutable i = p
          while i < len && Char.IsWhiteSpace(spanLine[i]) do i <- i + 1
          let tailResultOnly =
            if i >= len then
              false
            else
              let tail = spanLine.Slice(i)
              let mutable after = i
              let mutable matched = true
              if tail.StartsWith("1-0") then
                after <- i + 3
              elif tail.StartsWith("0-1") then
                after <- i + 3
              elif tail.StartsWith("1/2-1/2") then
                after <- i + 7
              elif tail.StartsWith("*") then
                after <- i + 1
              elif tail.StartsWith("Ť-Ť") then
                after <- i + 3
              else
                matched <- false
              if not matched then
                false
              else
                while after < len && Char.IsWhiteSpace(spanLine[after]) do after <- after + 1
                after >= len

          // Always try to attach to the most recent move first
          let attached = tryUpdateLastNodeComment comment
          if not attached then
            pendingComment <- if pendingComment = "" then comment else pendingComment + " " + comment

        // Variation start (
        elif c0 = '(' then
          p <- p + 1
          // Push current line and ply onto stack
          let variationLine = ResizeArray<PlyMove>()
          match currentLine |> Seq.tryLast with
          | Some parent -> parent.Variations.Add variationLine
          | None -> rootVariations.Add variationLine
          lineStack.Push currentLine
          plyStack.Push currentPly
          // Set ply to parent's ply + 1 for the variation
          let parentPly =
            match currentLine |> Seq.tryLast with
            | Some node -> node.Ply
            | None -> currentPly - 1
          currentPly <- parentPly + 1
          currentLine <- variationLine

        // Variation end )
        elif c0 = ')' then
          p <- p + 1
          // Pop back to parent line
          if lineStack.Count > 0 then
            currentLine <- lineStack.Pop()
          if plyStack.Count > 0 then
            currentPly <- plyStack.Pop()

        // NAG ($1, $14, etc.)
        elif c0 = '$' then
          p <- p + 1
          let startNag = p
          while p < len && isDigit(spanLine[p]) do p <- p + 1
          if p > startNag then
            let nagStr = new string(spanLine.Slice(startNag, p - startNag))
            match Int32.TryParse(nagStr) with
            | true, nagVal -> pendingNags.Add(nagVal)
            | _ -> ()

        // Game termination *
        elif c0 = '*' then
          p <- p + 1
          if result = "" then result <- "*"
          resultFoundInMovetext <- true

        // Move number (1. or 1... or just digits followed by dots)
        elif isDigit c0 && not (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-') then
          let mutable nr = 0
          while p < len && isDigit(spanLine[p]) do
            nr <- nr * 10 + (int spanLine[p] - int '0')
            p <- p + 1
          // Skip whitespace
          while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1
          // Skip dots (including Unicode ellipsis …)
          while p < len && (spanLine[p] = '.' || spanLine[p] = '…') do p <- p + 1
          if nr > 0 then currentMoveNr <- nr

        // Result tokens (1-0, 0-1, 1/2-1/2, ½-½)
        elif c0 = '1' || (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-' && (p + 2 >= len || spanLine[p + 2] <> '0')) || c0 = '½' then
          let tail = spanLine.Slice(p)
          if tail.StartsWith("1-0".AsSpan()) then
            if result = "" then result <- "1-0"; resultFoundInMovetext <- true
            p <- p + 3
          elif tail.StartsWith("0-1".AsSpan()) then
            if result = "" then result <- "0-1"; resultFoundInMovetext <- true
            p <- p + 3
          elif tail.StartsWith("1/2-1/2".AsSpan()) then
            if result = "" then result <- "1/2-1/2"; resultFoundInMovetext <- true
            p <- p + 7
          elif tail.StartsWith("½-½".AsSpan()) then
            if result = "" then result <- "1/2-1/2"; resultFoundInMovetext <- true
            p <- p + 3
          else
            // Fallback: maybe it's a SAN move starting with 0 (0-0 castling)
            let start = p
            if c0 = 'O' || (c0 = '0' && (p + 2) < len && spanLine[p + 2] = '0') then
              while p < len && (spanLine[p] = 'O' || spanLine[p] = '0' || spanLine[p] = 'o' || spanLine[p] = '-') do p <- p + 1
            if p > start then
              let san = new string(spanLine.Slice(start, p - start))
              // Check/checkmate suffix
              if p < len && (spanLine[p] = '+' || spanLine[p] = '#') then p <- p + 1
              // Annotation symbols
              while p < len && (spanLine[p] = '!' || spanLine[p] = '?') do p <- p + 1
              if san <> "" then
                if whiteSan = "" then
                  if mainlinePly.Count = 0 && fen <> "" && fen.Contains(" b ") then
                    blackSan <- san
                    appendPlyMove san "b"
                    if pendingComment <> "" then
                      blackComment <- pendingComment
                      tryUpdateLastNodeComment pendingComment |> ignore
                      pendingComment <- ""
                  else
                    whiteSan <- san
                    appendPlyMove san "w"
                    if pendingComment <> "" then
                      whiteComment <- pendingComment
                      tryUpdateLastNodeComment pendingComment |> ignore
                      pendingComment <- ""
                else
                  blackSan <- san
                  appendPlyMove san "b"
            else
              p <- p + 1

        // SAN moves (piece moves, pawn moves, castling)
        elif isPiece c0 || isFile c0 || c0 = 'O' || c0 = 'o' || c0 = '0' then
          let start = p
          // Castling (handles O-O, o-o, 0-0 notations)
          if c0 = 'O' || c0 = 'o' || (c0 = '0' && (p + 2) < len && spanLine[p + 2] = '0') then
            while p < len && (spanLine[p] = 'O' || spanLine[p] = '0' || spanLine[p] = 'o' || spanLine[p] = '-') do p <- p + 1
          else
            // Optional piece letter
            if p < len && isPiece(spanLine[p]) then p <- p + 1
            // Optional disambiguation file/rank
            if p < len && isFile(spanLine[p]) then p <- p + 1
            if p < len && isRank(spanLine[p]) then p <- p + 1
            // Optional capture
            if p < len && spanLine[p] = 'x' then p <- p + 1
            // Destination square
            if p < len && isFile(spanLine[p]) then p <- p + 1
            if p < len && isRank(spanLine[p]) then p <- p + 1
            // Promotion
            if p < len && spanLine[p] = '=' then
              p <- p + 1
              if p < len && (isPiece(spanLine[p]) || isPromoPiece(spanLine[p])) then p <- p + 1
            elif p < len && isPromoPiece(spanLine[p]) then p <- p + 1
            // Check/mate
            if p < len && (spanLine[p] = '+' || spanLine[p] = '#') then p <- p + 1
            // Annotation symbols (!?, ?!, !!, ??)
            while p < len && (spanLine[p] = '!' || spanLine[p] = '?') do p <- p + 1

          if p > start then
            let san = new string(spanLine.Slice(start, p - start))
            if san <> "" then
              if whiteSan = "" then
                if mainlinePly.Count = 0 && fen <> "" && fen.Contains(" b ") then
                  blackSan <- san
                  appendPlyMove san "b"
                  if pendingComment <> "" then
                    blackComment <- pendingComment
                    tryUpdateLastNodeComment pendingComment |> ignore
                    pendingComment <- ""
                else
                  whiteSan <- san
                  appendPlyMove san "w"
                  if pendingComment <> "" then
                    whiteComment <- pendingComment
                    tryUpdateLastNodeComment pendingComment |> ignore
                    pendingComment <- ""
              else
                blackSan <- san
                appendPlyMove san "b"
          else
            p <- p + 1

        else
          // Unknown token; advance to avoid infinite loop
          p <- p + 1

      // Safety: if no progress made this iteration, advance one char
      if p = prevP then p <- p + 1

// ============================================================================
// Build PgnGame from current state
// ============================================================================

let inline moveNumberCount ply =
  if ply % 2 = 0 then ply / 2 else (ply / 2) + 1

let private buildGameFull (raw:string) : PgnGame =
  gameNumber <- gameNumber + 1
  let metadata: GameMetadata =
    { Event = event'
      Site = site
      Date = date
      Round = round
      White = white
      Black = black
      Result = result
      Reason = reason
      OpeningHash = openingHash
      GameTime = gameTime
      Moves = moveNumberCount mainlinePly.Count
      OpeningName = openingName
      Fen = fen
      Deviations = deviations
      StartEvals = []
      OtherTags = otherTags |> Seq.toList }
  {
    GameNumber = gameNumber
    GameMetaData = metadata
    Mainline = ResizeArray(mainlinePly)
    RootVariations = ResizeArray(rootVariations)
    Comments = pendingComment
    Fen = fen
    Raw = raw
  }

let private buildGame() : PgnGame =
  buildGameFull ""

let private buildGameWithRaw raw : PgnGame =
  buildGameFull raw

// ============================================================================
// Public API
// ============================================================================

/// Parse a PGN string, yielding full PgnGame records
let parsePgnStringHelper (content: string) withRaw : seq<PgnGame> =
  let rawLines = ResizeArray<string>()
  gameNumber <- 0
  resetState()
  let mutable inMoveText = false

  let buildGame () =
    if withRaw then
      let raw = String.concat "\n" rawLines
      rawLines.Clear()
      buildGameWithRaw raw
    else
      rawLines.Clear()
      buildGame()
  seq {
      use reader = new StringReader(content)
      let mutable currentLine = reader.ReadLine()
      while currentLine <> null do
        let trimmed = currentLine.TrimStart()
        if withRaw then
          rawLines.Add(currentLine)
        if String.IsNullOrEmpty trimmed then
          if inMoveText && (hasGame() || result <> "") then
            yield buildGame()
            resetState()
            inMoveText <- false
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && (hasGame() || result <> "") then
            yield buildGame()
            resetState()
            inMoveText <- false
          parseHeaderTexLine trimmed
        else
          inMoveText <- true
          parseMoveTextLine trimmed
          if resultFoundInMovetext && result <> "" && (result = "1-0" || result = "0-1" || result = "1/2-1/2" || result = "*") then
            yield buildGame()
            resetState()
            inMoveText <- false

        currentLine <- reader.ReadLine()

      if hasGame() || result <> "" then
        yield buildGame()
      }

let parsePgnString (content: string) : seq<PgnGame> =
  parsePgnStringHelper content false

let parsePgnStringWithRaw (content: string) : seq<PgnGame> =
  parsePgnStringHelper content true
    /// Parse a PGN file, yielding full PgnGame records with Raw field populated

let parsePgnFileHelper (pgnFilePath: string) withRaw : seq<PgnGame> =
  let rawLines = ResizeArray<string>()
  gameNumber <- 0
  resetState()
  let mutable inMoveText = false

  let mayAddRaw line =
    if withRaw then
      rawLines.Add(line)

  let buildGameWithRaw () =
    let game = buildGame()
    let raw = String.concat "\n" rawLines
    rawLines.Clear()
    if withRaw then
      { game with Raw = raw }
    else
      game

  seq {
      let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
      use reader = new StreamReader(pgnFilePath, options)

      while not reader.EndOfStream do
        let currentLine = reader.ReadLine()
        let trimmed = currentLine.TrimStart()
        if String.IsNullOrEmpty trimmed then
          if inMoveText && (hasGame() || result <> "") then
            yield buildGameWithRaw()
            resetState()
            inMoveText <- false
          elif rawLines.Count > 0 then
            mayAddRaw currentLine
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && (hasGame() || result <> "") then
            yield buildGameWithRaw()
            resetState()
            inMoveText <- false
          mayAddRaw currentLine
          parseHeaderTexLine trimmed
        else
          mayAddRaw currentLine
          inMoveText <- true
          parseMoveTextLine trimmed
          if resultFoundInMovetext && result <> "" && (result = "1-0" || result = "0-1" || result = "1/2-1/2" || result = "*") then
              yield buildGameWithRaw()
              resetState()
              inMoveText <- false

      if hasGame() || result <> "" then
        yield buildGameWithRaw()
  }

let parsePgnFileWithRaw (pgnFilePath: string) : seq<PgnGame> = parsePgnFileHelper pgnFilePath true

let parsePgnFile (pgnFilePath: string) : seq<PgnGame> = parsePgnFileHelper pgnFilePath false

/// Convert a FullSpanParser game to moves as string list (for compatibility)
let getMovesAsStrings (game: PgnGame) : string list = game.Mainline |> Seq.map (fun mv -> mv.San) |> Seq.toList

/// Generate PGN string representation of a game on demand
let toPgnString (game: PgnGame) : string =
  let sb = System.Text.StringBuilder()
  let meta = game.GameMetaData

  // Headers
  sb.AppendLine($"[Event \"{meta.Event}\"]") |> ignore
  sb.AppendLine($"[Site \"{meta.Site}\"]") |> ignore
  sb.AppendLine($"[Date \"{meta.Date}\"]") |> ignore
  sb.AppendLine($"[Round \"{meta.Round}\"]") |> ignore
  sb.AppendLine($"[White \"{meta.White}\"]") |> ignore
  sb.AppendLine($"[Black \"{meta.Black}\"]") |> ignore
  sb.AppendLine($"[Result \"{meta.Result}\"]") |> ignore

  if game.Fen <> "" && game.Fen <> "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" then
    sb.AppendLine($"[FEN \"{game.Fen}\"]") |> ignore
    sb.AppendLine("[SetUp \"1\"]") |> ignore

  // Other tags
  for header in meta.OtherTags do
    sb.AppendLine($"[{header.Key} \"{header.Value}\"]") |> ignore

  sb.AppendLine() |> ignore

  for move in game.Mainline do
      if move.Color = "w" then
          sb.Append($"{move.MoveNumber}. {move.San} ") |> ignore
      else
          sb.Append($"{move.San} ") |> ignore
  // Result
  if meta.Result <> "" then
    sb.Append(meta.Result) |> ignore

  sb.ToString()

let parsePgnStreamWithRaw (reader: StreamReader): seq<PgnGame> =
  parsePgnStringWithRaw (reader.ReadToEnd())

let parsePgnStream (reader: StreamReader): seq<PgnGame> =
  parsePgnString (reader.ReadToEnd())

let parseFullPgnGame (pgn:string) =
  match parsePgnString pgn |> Seq.tryHead with
  | Some game -> game
  | None -> PgnGame.Empty(0)

/// Parse only headers from a PGN file (span-based, skips movetext for performance)
let parsePgnFileHeadersOnly (pgnFilePath: string): seq<PgnGame> =
  gameNumber <- 0
  resetState()
  let mutable inMoveText = false
  seq {
      let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
      use reader = new StreamReader(pgnFilePath, options)

      while not reader.EndOfStream do
        let currentLine = reader.ReadLine()
        let trimmed = currentLine.TrimStart()

        if String.IsNullOrEmpty trimmed then
          // Empty line - transition from headers to movetext or end of game
          if inMoveText && hasHeaders() then
            // End of game - yield without parsing moves
            yield buildGame()
            resetState()
            inMoveText <- false
          elif hasHeaders() && not inMoveText then
            // Empty line after headers - now in movetext section
            inMoveText <- true
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          // Header line
          if inMoveText && hasHeaders() then
            // New game starting - yield previous
            yield buildGame()
            resetState()
            inMoveText <- false
          parseHeaderTexLine trimmed
        else
          // Movetext line - just mark we're in movetext, don't parse
          inMoveText <- true
          // Only check for result tokens to properly terminate the game
          let trimmedStr = trimmed.ToString()
          if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
             trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
            if result = "" then
              if trimmedStr.Contains("1-0") then result <- "1-0"
              elif trimmedStr.Contains("0-1") then result <- "0-1"
              elif trimmedStr.Contains("1/2-1/2") then result <- "1/2-1/2"
              elif trimmedStr.Contains("*") then result <- "*"

      // Handle last game
      if hasHeaders() then
        yield buildGame()
  }

/// Parse only headers from a PGN string (span-based, skips movetext for performance)
let parsePgnStringHeadersOnly (content: string): seq<PgnGame> =
  gameNumber <- 0
  resetState()
  let mutable inMoveText = false
  seq {
      use reader = new StringReader(content)
      let mutable currentLine = reader.ReadLine()

      while currentLine <> null do
        let trimmed = currentLine.TrimStart()
        if String.IsNullOrEmpty trimmed then
          if inMoveText && hasHeaders() then
            yield buildGame()
            resetState()
            inMoveText <- false
          elif hasHeaders() && not inMoveText then
            inMoveText <- true
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && hasHeaders() then
            yield buildGame()
            resetState()
            inMoveText <- false
          parseHeaderTexLine trimmed
        else
          inMoveText <- true
          let trimmedStr = trimmed.ToString()
          if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
             trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
            if result = "" then
              if trimmedStr.Contains("1-0") then result <- "1-0"
              elif trimmedStr.Contains("0-1") then result <- "0-1"
              elif trimmedStr.Contains("1/2-1/2") then result <- "1/2-1/2"
              elif trimmedStr.Contains("*") then result <- "*"

        currentLine <- reader.ReadLine()

      if hasHeaders() then
        yield buildGame()
}

type PgnGameMessage =
    | WriteGame of filePath:string * header:GameMetadata * moveSection:string * result:Result
    | GetResults of reply:AsyncReplyChannel<ResizeArray<Result>>
    | GetPGNGames of reply:AsyncReplyChannel<ResizeArray<PgnGame>>
    | Dispose
    | DisposeReply of reply:AsyncReplyChannel<unit>

let startPgnGameReaderWriter (filePath: string) =
  MailboxProcessor<PgnGameMessage>.Start(fun inbox ->
      async {
          // Open the file once for all future writes
          use writer =
            if String.IsNullOrWhiteSpace filePath then
                new StreamWriter(new MemoryStream())
            else
                PGNWriter.createPGNWriter filePath
          use reader =
            if String.IsNullOrWhiteSpace filePath then
                new StreamReader(new MemoryStream())
            else
                PGNWriter.createPGNReader filePath
          writer.AutoFlush <- true
          let mutable running = true
          while running do
              let! message = inbox.Receive()
              match message with
              | Dispose ->
                  writer.Dispose()
                  reader.Dispose()
                  running <- false
              | DisposeReply reply ->
                  writer.Dispose()
                  reader.Dispose()
                  reply.Reply(())
                  running <- false
              | WriteGame(_, header, moveSection, result) ->
                  try
                      PGNWriter.writePGNHeaderSection writer header
                      writer.Write moveSection
                      PGNWriter.writeEndOfGameSection writer result
                  with ex ->
                      System.Diagnostics.Debug.WriteLine($"PGN WriteGame error: {ex.Message}")
              | GetPGNGames reply ->
                  try
                      reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
                      reader.DiscardBufferedData()
                      let games = parsePgnStream reader
                      reply.Reply (ResizeArray<PgnGame>(games))
                  with ex ->
                      System.Diagnostics.Debug.WriteLine($"PGN GetPGNGames error: {ex.Message}")
                      reply.Reply(ResizeArray<PgnGame>())
              | GetResults reply ->
                  try
                      reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
                      reader.DiscardBufferedData()
                      let games =
                        let allResults = parsePgnStream reader
                        allResults
                        |> Seq.map PGNWriter.getResultsFromPGNGame
                        |> Seq.toArray
                      games |> Array.Reverse
                      reply.Reply (ResizeArray<Result>(games))
                  with ex ->
                      System.Diagnostics.Debug.WriteLine($"PGN GetResults error: {ex.Message}")
                      reply.Reply(ResizeArray<Result>())
      }
  )
