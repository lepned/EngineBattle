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
// Parser state
// ============================================================================

// All parse state lives in a per-parse instance created inside each seq { }.
// This makes concurrent parses (Blazor pages, tournament workers, the PGN
// agent's GetPGNGames) independent, and makes re-enumerating a returned seq
// start from a fresh state instead of continuing from stale module state.
type private ParserState =
  { // Header state
    mutable GameNumber: int
    mutable Event: string
    mutable Site: string
    mutable Date: string
    mutable Round: string
    mutable White: string
    mutable Black: string
    mutable Result: string
    mutable ResultFoundInMovetext: bool
    mutable Fen: string
    mutable Reason: ResultReason
    mutable OpeningHash: string
    mutable GameTime: int64
    mutable PlyCount: int
    mutable OpeningName: string
    mutable Deviations: int
    OtherTags: ResizeArray<Header>
    // Movetext state
    mutable CurrentMoveNr: int
    mutable WhiteSan: string
    mutable BlackSan: string
    mutable PendingComment: string
    // Multi-line { comment } state: set when a '{' was not closed on its line;
    // subsequent lines are comment text until the matching '}'
    mutable InComment: bool
    mutable CommentAcc: string
    // Variation-aware tree state
    mutable CurrentPly: int
    MainlinePly: ResizeArray<PlyMove>
    RootVariations: ResizeArray<PlyLine>
    mutable CurrentLine: PlyLine
    LineStack: Stack<PlyLine>
    PlyStack: Stack<int>
    // Saved WhiteSan/BlackSan across '(' ')' so a variation gets the parent
    // move's color and the mainline color toggle survives odd-length variations
    SanStack: Stack<string * string>
    mutable LastParsedSan: string option
    // NAG state
    PendingNags: ResizeArray<int> }

  static member Create() =
    let mainline = ResizeArray<PlyMove>(128)
    { GameNumber = 0
      Event = ""
      Site = ""
      Date = ""
      Round = ""
      White = ""
      Black = ""
      Result = ""
      ResultFoundInMovetext = false
      Fen = ""
      Reason = ResultReason.NotStarted
      OpeningHash = ""
      GameTime = 0L
      PlyCount = 0
      OpeningName = ""
      Deviations = 0
      OtherTags = ResizeArray<Header>()
      CurrentMoveNr = 1
      WhiteSan = ""
      BlackSan = ""
      PendingComment = ""
      InComment = false
      CommentAcc = ""
      CurrentPly = 0
      MainlinePly = mainline
      RootVariations = ResizeArray<PlyLine>()
      CurrentLine = mainline
      LineStack = Stack<PlyLine>()
      PlyStack = Stack<int>()
      SanStack = Stack<string * string>()
      LastParsedSan = None
      PendingNags = ResizeArray<int>() }

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

/// Reset per-game state (GameNumber intentionally survives across games)
let private resetState (st: ParserState) =
  st.Event <- ""
  st.Site <- ""
  st.Date <- ""
  st.Round <- ""
  st.White <- ""
  st.Black <- ""
  st.Result <- ""
  st.ResultFoundInMovetext <- false
  st.Fen <- ""
  st.Reason <- ResultReason.NotStarted
  st.OpeningHash <- ""
  st.GameTime <- 0L
  st.PlyCount <- 0
  st.OpeningName <- ""
  st.Deviations <- 0
  st.OtherTags.Clear()
  st.CurrentMoveNr <- 1
  st.WhiteSan <- ""
  st.BlackSan <- ""
  st.PendingComment <- ""
  st.InComment <- false
  st.CommentAcc <- ""
  st.CurrentPly <- 0
  st.MainlinePly.Clear()
  st.RootVariations.Clear()
  st.CurrentLine <- st.MainlinePly
  st.LineStack.Clear()
  st.PlyStack.Clear()
  st.SanStack.Clear()
  st.LastParsedSan <- None
  st.PendingNags.Clear()

let private hasGame (st: ParserState) =
   st.WhiteSan <> "" || st.BlackSan <> "" || st.MainlinePly.Count > 0

let private hasHeaders (st: ParserState) =
  st.White <> "" || st.Black <> "" || st.Event <> "" || st.Result <> ""

// ============================================================================
// PlyMove tree management
// ============================================================================

let private appendPlyMove (st: ParserState) (san: string) (color: string) =
  let ply = st.CurrentPly
  let moveNr = (ply / 2) + 1
  let nags = if st.PendingNags.Count > 0 then st.PendingNags |> Seq.toList else []
  let node =
    { Ply = ply
      MoveNumber = moveNr
      Color = color
      San = san
      Comment = ""
      Nags = nags
      Variations = ResizeArray() }
  st.CurrentLine.Add node
  st.CurrentPly <- st.CurrentPly + 1
  st.LastParsedSan <- Some san
  st.PendingNags.Clear()
  if color = "b" then
      st.WhiteSan <- ""
      st.BlackSan <- ""

/// Consecutive comments both belong to the same move — lichess routinely writes
/// `{ prose } { [%eval …] [%clk …] }` — so a second one is appended, not substituted.
/// Overwriting dropped the prose and kept only the machine data. The pending-comment
/// path below already concatenated; this one did not.
let private appendComment (existing: string) (cmt: string) =
  if String.IsNullOrWhiteSpace existing then cmt else existing + " " + cmt

let private tryUpdateLastNodeComment (st: ParserState) (cmt: string) =
  if not (String.IsNullOrWhiteSpace cmt) then
    match st.CurrentLine |> Seq.tryLast with
    | Some node -> node.Comment <- appendComment node.Comment cmt; true
    | None ->
        match st.RootVariations |> Seq.tryLast with
        | Some line when line.Count > 0 ->
            let lastNode = line[line.Count - 1]
            lastNode.Comment <- appendComment lastNode.Comment cmt
            true
        | _ -> false
  else
    false

/// Attach a completed comment to the most recent move, or hold it as pending
/// (a comment before any move applies to the next one).
let private attachComment (st: ParserState) (comment: string) =
  let attached = tryUpdateLastNodeComment st comment
  if not attached then
    st.PendingComment <- if st.PendingComment = "" then comment else st.PendingComment + " " + comment

/// Record a parsed SAN token as the next move, using the WhiteSan toggle
/// (empty = white to move) with the FEN black-to-move special case.
let private recordSanMove (st: ParserState) (san: string) =
  if st.WhiteSan = "" then
    if st.MainlinePly.Count = 0 && st.Fen <> "" && st.Fen.Contains(" b ") then
      st.BlackSan <- san
      appendPlyMove st san "b"
      if st.PendingComment <> "" then
        tryUpdateLastNodeComment st st.PendingComment |> ignore
        st.PendingComment <- ""
    else
      st.WhiteSan <- san
      appendPlyMove st san "w"
      if st.PendingComment <> "" then
        tryUpdateLastNodeComment st st.PendingComment |> ignore
        st.PendingComment <- ""
  else
    st.BlackSan <- san
    appendPlyMove st san "b"

/// Enter a variation: save line/ply/color-toggle state, then position the
/// parser so the variation's first move replaces the last move of the current
/// line (same ply, same color). The saved state is restored on ')'.
let private lineStackPush (st: ParserState) (variationLine: PlyLine) =
  st.LineStack.Push st.CurrentLine
  st.PlyStack.Push st.CurrentPly
  st.SanStack.Push (st.WhiteSan, st.BlackSan)
  match st.CurrentLine |> Seq.tryLast with
  | Some parent ->
      st.CurrentPly <- parent.Ply
      if parent.Color = "w" then
        // Variation replaces a white move: next parsed move is white
        st.WhiteSan <- ""
        st.BlackSan <- ""
      else
        // Variation replaces a black move: next parsed move is black
        st.WhiteSan <- parent.San
        st.BlackSan <- ""
  | None -> () // Variation with no preceding move: keep current ply/toggle
  st.CurrentLine <- variationLine

// ============================================================================
// Text-based header parsing
// ============================================================================

let private parseHeaderTexLine (st: ParserState) (line: string) =
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
      | "Event" -> st.Event <- value
      | "Site" -> st.Site <- value
      | "Date" -> st.Date <- value
      | "Round" -> st.Round <- value
      | "White" -> st.White <- value
      | "Black" -> st.Black <- value
      | "Result" -> st.Result <- value
      | "FEN" | "Fen" -> st.Fen <- value
      | "Reason" ->
          try st.Reason <- stringToResultReason value
          with _ -> () // Ignore invalid reason values
      | "OpeningHash" -> st.OpeningHash <- value
      | "GameTime" ->
          match Int64.TryParse(value) with
          | true, v -> st.GameTime <- v
          | _ -> ()
      | "Ply" ->
          match Int32.TryParse(value) with
          | true, v -> st.PlyCount <- v
          | _ -> ()
      | "Opening" ->
          st.OpeningName <- value
          st.OtherTags.Add({ Key = key; Value = value })
      | "Deviations" ->
          match Int32.TryParse(value) with
          | true, v -> st.Deviations <- v
          | _ -> ()
      | _ -> st.OtherTags.Add({ Key = key; Value = value })

// ============================================================================
// Span-based movetext parsing (iterative, with variation stack)
// ============================================================================

/// Parse movetext from a span, handling variations iteratively with an explicit stack
let private parseMoveTextLine (st: ParserState) (line: string) =
  let spanLine = line.AsSpan()
  let len = spanLine.Length
  let mutable p = 0

  // Continuation of a { comment } opened on a previous line: everything up to the
  // closing '}' is comment text, not movetext.
  if st.InComment then
    let mutable q = p
    while q < len && spanLine[q] <> '}' do q <- q + 1
    if q >= len then
      // The whole line is still inside the comment
      let piece = line.Trim()
      if piece <> "" then
        st.CommentAcc <- if st.CommentAcc = "" then piece else st.CommentAcc + " " + piece
      p <- len
    else
      let piece = (new string(spanLine.Slice(0, q))).Trim()
      let full =
        if st.CommentAcc = "" then piece
        elif piece = "" then st.CommentAcc
        else st.CommentAcc + " " + piece
      st.InComment <- false
      st.CommentAcc <- ""
      attachComment st full
      p <- q + 1

  // Skip leading whitespace
  while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1

  // Handle full-line comments
  if p < len && (spanLine[p] = ';' || spanLine[p] = '%') then
    // Line comment - extract and store as pending
    if p + 1 < len then
      st.PendingComment <- new string(spanLine.Slice(p + 1).Trim())
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
          if p < len && spanLine[p] = '}' then
            p <- p + 1
            attachComment st comment
          else
            // No '}' on this line — the comment continues on subsequent lines.
            // Previously the continuation was parsed as movetext (phantom moves).
            st.InComment <- true
            st.CommentAcc <- comment

        // Rest-of-line comment: ';' is legal mid-line in PGN, not just line-initial
        elif c0 = ';' then
          let comment = if p + 1 < len then new string(spanLine.Slice(p + 1).Trim()) else ""
          if comment <> "" then attachComment st comment
          p <- len

        // Variation start (
        elif c0 = '(' then
          p <- p + 1
          // Push current line and ply onto stack
          let variationLine = ResizeArray<PlyMove>()
          match st.CurrentLine |> Seq.tryLast with
          | Some parent -> parent.Variations.Add variationLine
          | None -> st.RootVariations.Add variationLine
          lineStackPush st variationLine

        // Variation end )
        elif c0 = ')' then
          p <- p + 1
          // Pop back to parent line
          if st.LineStack.Count > 0 then
            st.CurrentLine <- st.LineStack.Pop()
          if st.PlyStack.Count > 0 then
            st.CurrentPly <- st.PlyStack.Pop()
          if st.SanStack.Count > 0 then
            let (w, b) = st.SanStack.Pop()
            st.WhiteSan <- w
            st.BlackSan <- b

        // NAG ($1, $14, etc.)
        elif c0 = '$' then
          p <- p + 1
          let startNag = p
          while p < len && isDigit(spanLine[p]) do p <- p + 1
          if p > startNag then
            let nagStr = new string(spanLine.Slice(startNag, p - startNag))
            match Int32.TryParse(nagStr) with
            | true, nagVal -> st.PendingNags.Add(nagVal)
            | _ -> ()

        // Game termination *
        elif c0 = '*' then
          p <- p + 1
          if st.Result = "" then st.Result <- "*"
          st.ResultFoundInMovetext <- true

        // Move number (1. or 1... or just digits followed by dots).
        // Exempt result tokens so they reach the result branch below: "0-1" (0 followed
        // by '-'), "1-0" (1 followed by '-') and "1/2-1/2" (1 followed by '/') — a move
        // number is always followed by a digit, '.' or whitespace, never '-' or '/'.
        elif isDigit c0
             && not (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-')
             && not (c0 = '1' && p + 1 < len && (spanLine[p + 1] = '-' || spanLine[p + 1] = '/')) then
          let mutable nr = 0
          while p < len && isDigit(spanLine[p]) do
            nr <- nr * 10 + (int spanLine[p] - int '0')
            p <- p + 1
          // Skip whitespace
          while p < len && Char.IsWhiteSpace(spanLine[p]) do p <- p + 1
          // Skip dots (including Unicode ellipsis …)
          while p < len && (spanLine[p] = '.' || spanLine[p] = '…') do p <- p + 1
          if nr > 0 then st.CurrentMoveNr <- nr

        // Result tokens (1-0, 0-1, 1/2-1/2, ½-½)
        elif c0 = '1' || (c0 = '0' && p + 1 < len && spanLine[p + 1] = '-' && (p + 2 >= len || spanLine[p + 2] <> '0')) || c0 = '½' then
          let tail = spanLine.Slice(p)
          if tail.StartsWith("1-0".AsSpan()) then
            if st.Result = "" then st.Result <- "1-0"; st.ResultFoundInMovetext <- true
            p <- p + 3
          elif tail.StartsWith("0-1".AsSpan()) then
            if st.Result = "" then st.Result <- "0-1"; st.ResultFoundInMovetext <- true
            p <- p + 3
          elif tail.StartsWith("1/2-1/2".AsSpan()) then
            if st.Result = "" then st.Result <- "1/2-1/2"; st.ResultFoundInMovetext <- true
            p <- p + 7
          elif tail.StartsWith("½-½".AsSpan()) then
            if st.Result = "" then st.Result <- "1/2-1/2"; st.ResultFoundInMovetext <- true
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
                recordSanMove st san
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
              recordSanMove st san
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

let private buildGameFull (st: ParserState) (raw: string) : PgnGame =
  st.GameNumber <- st.GameNumber + 1
  let metadata: GameMetadata =
    { Event = st.Event
      Site = st.Site
      Date = st.Date
      Round = st.Round
      White = st.White
      Black = st.Black
      Result = st.Result
      Reason = st.Reason
      OpeningHash = st.OpeningHash
      GameTime = st.GameTime
      Moves = moveNumberCount st.MainlinePly.Count
      OpeningName = st.OpeningName
      Fen = st.Fen
      Deviations = st.Deviations
      StartEvals = []
      OtherTags = st.OtherTags |> Seq.toList }
  {
    GameNumber = st.GameNumber
    GameMetaData = metadata
    Mainline = ResizeArray(st.MainlinePly)
    RootVariations = ResizeArray(st.RootVariations)
    Comments = st.PendingComment
    Fen = st.Fen
    Raw = raw
  }

let private buildGame (st: ParserState) : PgnGame =
  buildGameFull st ""

// ============================================================================
// Public API
// ============================================================================

/// Parse a PGN string, yielding full PgnGame records
let parsePgnStringHelper (content: string) withRaw : seq<PgnGame> =
  seq {
      // State is created per enumeration: concurrent and repeated enumerations
      // are fully independent.
      let st = ParserState.Create()
      let rawLines = ResizeArray<string>()
      let mutable inMoveText = false

      let emitGame () =
        if withRaw then
          let raw = String.concat "\n" rawLines
          rawLines.Clear()
          buildGameFull st raw
        else
          rawLines.Clear()
          buildGame st

      use reader = new StringReader(content)
      let mutable currentLine = reader.ReadLine()
      while currentLine <> null do
        let trimmed = currentLine.TrimStart()
        if withRaw then
          rawLines.Add(currentLine)
        if String.IsNullOrEmpty trimmed then
          if st.InComment then
            () // blank line inside a multi-line comment is not a game boundary
          elif inMoveText && (hasGame st || st.Result <> "") then
            yield emitGame()
            resetState st
            inMoveText <- false
        elif not st.InComment && trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && (hasGame st || st.Result <> "") then
            yield emitGame()
            resetState st
            inMoveText <- false
          parseHeaderTexLine st trimmed
        else
          inMoveText <- true
          parseMoveTextLine st trimmed
          if st.ResultFoundInMovetext && st.Result <> "" && (st.Result = "1-0" || st.Result = "0-1" || st.Result = "1/2-1/2" || st.Result = "*") then
            yield emitGame()
            resetState st
            inMoveText <- false

        currentLine <- reader.ReadLine()

      if hasGame st || st.Result <> "" then
        yield emitGame()
      }

let parsePgnString (content: string) : seq<PgnGame> =
  parsePgnStringHelper content false

let parsePgnStringWithRaw (content: string) : seq<PgnGame> =
  parsePgnStringHelper content true

/// Parse a PGN file, yielding full PgnGame records with Raw field populated
let parsePgnFileHelper (pgnFilePath: string) withRaw : seq<PgnGame> =
  seq {
      let st = ParserState.Create()
      let rawLines = ResizeArray<string>()
      let mutable inMoveText = false

      let mayAddRaw (line: string) =
        if withRaw then
          rawLines.Add(line)

      let emitGame () =
        let game = buildGame st
        let raw = String.concat "\n" rawLines
        rawLines.Clear()
        if withRaw then
          { game with Raw = raw }
        else
          game

      let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
      use reader = new StreamReader(pgnFilePath, options)

      while not reader.EndOfStream do
        let currentLine = reader.ReadLine()
        let trimmed = currentLine.TrimStart()
        if String.IsNullOrEmpty trimmed then
          if st.InComment then
            mayAddRaw currentLine // blank line inside a multi-line comment is not a game boundary
          elif inMoveText && (hasGame st || st.Result <> "") then
            yield emitGame()
            resetState st
            inMoveText <- false
          elif rawLines.Count > 0 then
            mayAddRaw currentLine
        elif not st.InComment && trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && (hasGame st || st.Result <> "") then
            yield emitGame()
            resetState st
            inMoveText <- false
          mayAddRaw currentLine
          parseHeaderTexLine st trimmed
        else
          mayAddRaw currentLine
          inMoveText <- true
          parseMoveTextLine st trimmed
          if st.ResultFoundInMovetext && st.Result <> "" && (st.Result = "1-0" || st.Result = "0-1" || st.Result = "1/2-1/2" || st.Result = "*") then
              yield emitGame()
              resetState st
              inMoveText <- false

      if hasGame st || st.Result <> "" then
        yield emitGame()
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
  seq {
      let st = ParserState.Create()
      let mutable inMoveText = false
      let options = FileStreamOptions(Access = FileAccess.Read, Share = FileShare.ReadWrite, Mode = FileMode.Open)
      use reader = new StreamReader(pgnFilePath, options)

      while not reader.EndOfStream do
        let currentLine = reader.ReadLine()
        let trimmed = currentLine.TrimStart()

        if String.IsNullOrEmpty trimmed then
          // Empty line - transition from headers to movetext or end of game
          if inMoveText && hasHeaders st then
            // End of game - yield without parsing moves
            yield buildGame st
            resetState st
            inMoveText <- false
          elif hasHeaders st && not inMoveText then
            // Empty line after headers - now in movetext section
            inMoveText <- true
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          // Header line
          if inMoveText && hasHeaders st then
            // New game starting - yield previous
            yield buildGame st
            resetState st
            inMoveText <- false
          parseHeaderTexLine st trimmed
        else
          // Movetext line - just mark we're in movetext, don't parse
          inMoveText <- true
          // Only check for result tokens to properly terminate the game
          let trimmedStr = trimmed.ToString()
          if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
             trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
            if st.Result = "" then
              if trimmedStr.Contains("1-0") then st.Result <- "1-0"
              elif trimmedStr.Contains("0-1") then st.Result <- "0-1"
              elif trimmedStr.Contains("1/2-1/2") then st.Result <- "1/2-1/2"
              elif trimmedStr.Contains("*") then st.Result <- "*"

      // Handle last game
      if hasHeaders st then
        yield buildGame st
  }

/// Parse only headers from a PGN string (span-based, skips movetext for performance)
let parsePgnStringHeadersOnly (content: string): seq<PgnGame> =
  seq {
      let st = ParserState.Create()
      let mutable inMoveText = false
      use reader = new StringReader(content)
      let mutable currentLine = reader.ReadLine()

      while currentLine <> null do
        let trimmed = currentLine.TrimStart()
        if String.IsNullOrEmpty trimmed then
          if inMoveText && hasHeaders st then
            yield buildGame st
            resetState st
            inMoveText <- false
          elif hasHeaders st && not inMoveText then
            inMoveText <- true
        elif trimmed.Length > 0 && trimmed[0] = '[' then
          if inMoveText && hasHeaders st then
            yield buildGame st
            resetState st
            inMoveText <- false
          parseHeaderTexLine st trimmed
        else
          inMoveText <- true
          let trimmedStr = trimmed.ToString()
          if trimmedStr = "1-0" || trimmedStr = "0-1" || trimmedStr = "1/2-1/2" || trimmedStr = "*" ||
             trimmedStr.EndsWith(" 1-0") || trimmedStr.EndsWith(" 0-1") || trimmedStr.EndsWith(" 1/2-1/2") || trimmedStr.EndsWith(" *") then
            if st.Result = "" then
              if trimmedStr.Contains("1-0") then st.Result <- "1-0"
              elif trimmedStr.Contains("0-1") then st.Result <- "0-1"
              elif trimmedStr.Contains("1/2-1/2") then st.Result <- "1/2-1/2"
              elif trimmedStr.Contains("*") then st.Result <- "*"

        currentLine <- reader.ReadLine()

      if hasHeaders st then
        yield buildGame st
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
