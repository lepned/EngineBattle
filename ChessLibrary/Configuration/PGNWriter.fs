module ChessLibrary.PGNWriter

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open ChessLibrary.PGNTypes
open ChessLibrary.EPDTypes
open TypesDef.CoreTypes

/// Gets the results from a PGN game.
/// <param name="game">The PGN game.</param>
/// <returns>The game result.</returns>
let getResultsFromPGNGame (game:PgnGame) =

  let data = game.GameMetaData
  {
    Player1 = data.White
    Player2 = data.Black
    Moves = data.Moves
    Result = data.Result
    Reason = data.Reason
    GameTime = data.GameTime
    OutOfOpeningEvals = []
  }

/// Writes the opening PGN moves.
/// <param name="moves">The sequence of moves.</param>
/// <returns>A string containing the opening PGN moves.</returns>
let writeOpeningPGNMoves (moves: string seq) =
  let sb = StringBuilder()
  let write (txt : string) = sb.Append txt |> ignore
  let annotation = "{book, mb=+0+0+0+0+0,}"

  // Write SAN moves as pairs of white-black moves separated by spaces
  let rec loop i ms idx =
      match ms with
      | [] -> ()  // Return unit when done
      | [m:string] ->
          // Write last move without pair number
          write $"{i}. {m} {annotation}"
      | m1::m2::m3::ms' ->
          write $"{i}. {m1} {annotation} {m2} {annotation} "
          // Call loop function with next pair number and remaining moves
          loop (i+1) (m3::ms') (idx+2)
      | m1::m2::ms' ->
          write $"{i}. {m1} {annotation} {m2} {annotation}"
          // Call loop function with next pair number and remaining moves
          loop (i+1) ms' (idx+2)

  // Call loop function with initial pair number 1
  loop 1 (moves |> Seq.toList) 0
  sb.ToString()

/// Writes a minimal PGN file.
/// <param name="pgnGames">The sequence of PGN games.</param>
/// <param name="filename">The filename.</param>
let writeMinimalPgnFile (pgnGames: PgnGame seq) (filename: string) =
  // Create a new text file using StreamWriter
  use writer = new StreamWriter(filename, append=false)
  printfn "Created pgn-file at this location: %s" filename

  for g in pgnGames do
    writer.WriteLine(sprintf "[White \"%s\"]" g.GameMetaData.White)
    writer.WriteLine(sprintf "[Black \"%s\"]" g.GameMetaData.Black)
    writer.WriteLine(sprintf "[Event \"%s\"]" g.GameMetaData.Event)
    writer.WriteLine(sprintf "[Round \"%s\"]" g.GameMetaData.Round)
    writer.WriteLine(sprintf "[Site \"%s\"]" g.GameMetaData.Site)
    writer.WriteLine(sprintf "[Date \"%s\"]" g.GameMetaData.Date)
    writer.WriteLine(sprintf "[Result \"%s\"]" g.GameMetaData.Result)
    if String.IsNullOrEmpty (g.GameMetaData.Fen) |> not then
      writer.WriteLine(sprintf "[FEN \"%s\"]" g.GameMetaData.Fen)
    for tags in g.GameMetaData.OtherTags do
      writer.WriteLine(sprintf "[%s \"%s\"]" tags.Key tags.Value)
      // Write an empty line after tags
    writer.WriteLine()

    // Write SAN moves as pairs of white-black moves separated by spaces
    for m in g.Mainline do
      if m.Color = "w" then
          writer.Write(sprintf "%d.%s " m.MoveNumber m.San)
      else
          writer.Write(sprintf "%s " m.San)

    if String.IsNullOrWhiteSpace g.GameMetaData.Result |> not then
      writer.WriteLine(g.GameMetaData.Result.Trim())
    else
      // If no result is specified, write an asterisk
      writer.Write("*")
    writer.WriteLine()

  writer.Close()

/// Writes an opening PGN from EPD.
/// <param name="epds">The sequence of EPD entries.</param>
/// <param name="filename">The filename.</param>
let writeOpeningPgnFromEPD (epds: EPDEntry seq) (filename: string) =
    if File.Exists(filename) |> not then
      use _ = File.Create(filename)
      printfn "Created pgn-file at this location: %s" filename
    // Create a new text file using StreamWriter
    use writer = new StreamWriter(filename, append=true)
    for epd in epds do
      //match epd.Id with
      //|Some id ->
      //  writer.WriteLine(sprintf "[Event \"%s\"]" id)
      //|None ->
      //  writer.WriteLine(sprintf "[Event \"%s\"]" "From EPD-file")
      writer.WriteLine(sprintf "[FEN \"%s\"]" epd.FEN)
      // Write an empty line after tags
      writer.WriteLine()
        // Write SAN moves as pairs of white-black moves separated by spaces
      writer.Write("*")
      writer.WriteLine(Environment.NewLine)

    writer.Close()

/// Extracts all opening moves from a PGN game by collecting moves up until the last book tag
/// Returns a list of SAN move strings
let getOpeningMovesFromPgn (pgnGame: PgnGame) : string list =
  // Determine if a comment indicates a book move
  let isBookComment (comment: string) =
      not (String.IsNullOrEmpty(comment)) &&
      (comment.Contains("book", StringComparison.OrdinalIgnoreCase) ||
          comment.Contains("Book exit", StringComparison.OrdinalIgnoreCase))

  // Find the last move with a book tag and whether it's a white or black move
  let mutable lastMoveIdxWithBookTag = -1
  let mutable lastBookTagIsWhite = false

  for i = 0 to pgnGame.Mainline.Count - 1 do
      let move = pgnGame.Mainline.[i]

      if isBookComment move.Comment then
          lastMoveIdxWithBookTag <- i
          lastBookTagIsWhite <- true

      if isBookComment move.Comment then
          lastMoveIdxWithBookTag <- i
          lastBookTagIsWhite <- false

  // If no book tags found, return empty list
  if lastMoveIdxWithBookTag = -1 then
      []
  else
      // Extract all SAN moves up to and including the last book tag
      let openingMoves = ResizeArray<string>()
      for i = 0 to lastMoveIdxWithBookTag do
          let move = pgnGame.Mainline.[i]

          // Add the white move if present
          if move.Color = "w" then
              openingMoves.Add(move.San)

          // Add the black move if present
          elif move.Color = "b" then
              // Only add the black move at the last index if it has a book tag
              if i < lastMoveIdxWithBookTag || not lastBookTagIsWhite then
                  openingMoves.Add(move.San)
      openingMoves |> List.ofSeq

/// Writes an opening PGN file.
/// <param name="pgnGames">The sequence of PGN games.</param>
/// <param name="filename">The filename.</param>
let writeOpeningPgnFile (pgnGames: PgnGame seq) (filename: string) =
    if File.Exists(filename) |> not then
      //create file
      use stream = File.Create(filename)
      printfn "Created pgn-file at this location: %s" filename

    // Create a new text file using StreamWriter
    use writer = new StreamWriter(filename, append=true)
    let totalGames = Seq.length pgnGames
    let games =
      pgnGames
      //|> Seq.filter (fun g -> g.GameMetaData.Event.ToLower().Contains "testing" |> not)
      //|> Seq.filter (fun g -> g.GameMetaData.Moves = 0) // Filter out games with no moves
    let totalFilteredGames = Seq.length games
    let difference = totalGames - totalFilteredGames
    if difference > 0 then
       printfn "Filtered out %d testing games from %d total games." difference totalGames

    // Use HashSet to track unique openings - by moves and by FEN
    let uniqueMoveSequences = System.Collections.Generic.HashSet<string>()
    let uniqueFens = System.Collections.Generic.HashSet<string>()
    let mutable counter = 0

    for g in games do
      //let currentOpening = PGNHelper.getOpeningInfo g
      let onlyBookMoves = getOpeningMovesFromPgn g
      // Create a key representing this opening
      let moveSequenceKey = String.Join("|", onlyBookMoves)

      // Check if this opening is unique by moves or by FEN
      let isUniqueByMoves = onlyBookMoves.Length > 0 && not (uniqueMoveSequences.Contains(moveSequenceKey))
      let isUniqueByFen = not (String.IsNullOrEmpty g.GameMetaData.Fen) && not (uniqueFens.Contains(g.GameMetaData.Fen))
      let isNewOpening = isUniqueByMoves || isUniqueByFen
      if isNewOpening then
          // Add to our tracking collections
        if onlyBookMoves.Length > 0 then
              uniqueMoveSequences.Add(moveSequenceKey) |> ignore

        if not (String.IsNullOrEmpty g.GameMetaData.Fen) then
              uniqueFens.Add(g.GameMetaData.Fen) |> ignore

        counter <- counter + 1
        let roundNr = $"{counter}.1"
        writer.WriteLine(sprintf "[Event \"%s\"]" g.GameMetaData.Event)
        writer.WriteLine(sprintf "[Round \"%s\"]" roundNr)
        if String.IsNullOrWhiteSpace g.GameMetaData.Fen |> not  then
          writer.WriteLine(sprintf "[FEN \"%s\"]" g.GameMetaData.Fen)
        let opening = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "opening" )
        let variation = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.ToLower().Contains "variation" )
        let eco = g.GameMetaData.OtherTags |> List.tryFind (fun e -> e.Key.Contains "ECO" )
        match opening,variation, eco with
        |Some h,Some v, Some eco ->
          writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
          writer.WriteLine(sprintf "[Variation \"%s\"]" v.Value )
          writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )
        |Some h,Some v, _ ->
          writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
          writer.WriteLine(sprintf "[Variation \"%s\"]" v.Value )
        |Some h,_ , Some eco ->
          writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
          writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )
        |Some h, _, _ ->
          writer.WriteLine(sprintf "[Opening \"%s\"]" h.Value )
        |_, _, Some eco ->
          writer.WriteLine(sprintf "[ECO \"%s\"]" eco.Value )
        |_ -> ()
         // Write an empty line after tags
        writer.WriteLine()
        let mutable ply = 1
        for m in onlyBookMoves do
            if ply % 2 = 1 then
              let moveNumber = (ply + 1) / 2
              // Write white move with pair number
              writer.Write(sprintf "%d.%s " moveNumber m)
            else
                writer.Write(sprintf "%s " m)
            ply <- ply + 1

         // Write SAN moves as pairs of white-black moves separated by spaces
        writer.Write("*")
        writer.WriteLine(Environment.NewLine)

    writer.Close()

/// Creates a PGN writer.
/// <param name="filename">The filename.</param>
/// <returns>A StreamWriter for the PGN file.</returns>
let createPGNWriter (filename: string) : StreamWriter =
    if not (File.Exists(filename)) then
        // Create a new file with a FileStream specifying FileShare.ReadWrite
        let stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
        printfn "Created PGN file at this location: %s" filename
        new StreamWriter(stream)
    else
        // Open the existing file in append mode with shared read/write access
        let stream = new FileStream(filename, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)
        new StreamWriter(stream)

/// Creates a PGN reader.
/// <param name="filename">The filename.</param>
/// <returns>A StreamReader for the PGN file.</returns>
let createPGNReader (filename: string) : StreamReader =
    // Open the existing file in read mode with shared read access
    let stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    new StreamReader(stream)

/// Writes the PGN header section.
/// <param name="writer">The StreamWriter.</param>
/// <param name="header">The game metadata.</param>
let writePGNHeaderSection (writer: StreamWriter) (header: GameMetadata) =
  let haveEvals, evals =
      match header.StartEvals with
      |[] -> false, ""
      |[x] -> true, sprintf "%s" x.ValueStr
      |x::y::_ -> true, sprintf "%s, %s" x.ValueStr y.ValueStr
  // Write header data as tags using brackets [ ]
  writer.WriteLine(sprintf "[Event \"%s\"]" header.Event)
  writer.WriteLine(sprintf "[Site \"%s\"]" header.Site)
  writer.WriteLine(sprintf "[Date \"%s\"]" header.Date)
  writer.WriteLine(sprintf "[Round \"%s\"]" header.Round)
  writer.WriteLine(sprintf "[White \"%s\"]" header.White)
  writer.WriteLine(sprintf "[Black \"%s\"]" header.Black)
  writer.WriteLine(sprintf "[Result \"%s\"]" header.Result)
  writer.WriteLine(sprintf "[Reason \"%s\"]" (header.Reason.ToString()))
  writer.WriteLine(sprintf "[Ply \"%s\"]" (header.Moves.ToString()))
  writer.WriteLine(sprintf "[GameTime \"%s\"]" (header.GameTime.ToString()))
  writer.WriteLine(sprintf "[Opening \"%s\"]" (PGNHelper.getOpeningOnly header))
  if haveEvals then
    writer.WriteLine(sprintf "[StartEvals \"%s\"]" evals)
  writer.WriteLine(sprintf "[OpeningHash \"%s\"]" header.OpeningHash)
  if header.Fen <> "" then
    writer.WriteLine(sprintf "[FEN \"%s\"]" header.Fen)
  if header.Deviations > 0 then
    writer.WriteLine(sprintf "[Deviations \"%d\"]" header.Deviations)

  // Write an empty line after tags
  writer.WriteLine()

/// Writes the end of game section.
/// <param name="writer">The StreamWriter.</param>
/// <param name="result">The game result.</param>
let writeEndOfGameSection (writer: StreamWriter) (result:Result) =
  writer.Write(sprintf " {%s}" result.Reason.Explanation)
  // Write result *
  writer.Write (sprintf " %s " result.Result)
  //writer.Write("*")
  writer.WriteLine(Environment.NewLine)

/// Writes the PGN moves section.
/// <param name="writer">The StreamWriter.</param>
/// <param name="openingMoves">The sequence of opening moves.</param>
/// <param name="playMoves">The play moves.</param>
let writePGNMovesSection (writer: StreamWriter) (openingMoves : string seq) (playMoves:string) =
  let openings = writeOpeningPGNMoves openingMoves
  let both = openings + playMoves
  writer.Write both

/// Writes the moves section.
/// <param name="writer">The StreamWriter.</param>
/// <param name="moveIdx">The move index.</param>
/// <param name="sanMoves">The sequence of SAN moves.</param>
let writeMovesSection (writer: StreamWriter) (moveIdx: int) (sanMoves: seq<string>) =
  if moveIdx = 0 then
      writer.Write(sprintf " {no opening moves played}")
  else
      // Write SAN moves as pairs of white-black moves separated by spaces
      let msg = "last opening move played"
      let rec loop i ms idx =
          match ms with
          | [] -> ()  // Return unit when done
          | [m:string] ->
              // Write last move without pair number
              writer.Write(sprintf "%d.%s " i m)
          | m1::m2::ms' ->
              if (idx + 1) = moveIdx then
                  // Write pair number followed by white-black move pair
                  writer.Write(sprintf "%d.%s {%s} %s " i m1 msg m2)
              elif (idx + 2) = moveIdx then
                  writer.Write(sprintf "%d.%s {%s} %s " i m1 msg m2)
              else
                  writer.Write(sprintf "%d.%s %s " i m1 m2)

              // Call loop function with next pair number and remaining moves
              loop (i+1) ms' (idx+2)

      // Call loop function with initial pair number 1
      loop 1 (sanMoves |> Seq.toList) 0

/// Writes a PGN file.
/// <param name="moveIdx">The move index.</param>
/// <param name="header">The game metadata.</param>
/// <param name="moves">The sequence of moves.</param>
/// <param name="filename">The filename.</param>
/// <param name="result">The game result.</param>
let writePgnFile moveIdx (header: GameMetadata) (moves: ResizeArray<string>) (filename: string) result =

    use pgnStreamWriter = createPGNWriter filename
    writePGNHeaderSection pgnStreamWriter header
    writeMovesSection pgnStreamWriter moveIdx moves
    writeEndOfGameSection pgnStreamWriter result

/// Writes a whole PGN game to a file.
/// <param name="filePath">The file path.</param>
/// <param name="header">The game metadata.</param>
/// <param name="moveSection">The move section.</param>
/// <param name="result">The game result.</param>
let writePgnGame (filePath:string) (header: GameMetadata) (moveSection:string) result =
  //use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.Read)
  //use writer = new StreamWriter(filePath,true)
  use writer = createPGNWriter filePath
  writePGNHeaderSection writer header
  writer.Write moveSection
  writeEndOfGameSection writer result

let ensureBlankLinesBetweenGames (filePath: string) (games: PgnGame seq) =
    PGNHelper.ensureDirectoryExists filePath
    use writer = new StreamWriter(filePath, append=false)
    for g in games do
      // write the raw game and ensure exactly one blank line after each game
      writer.WriteLine(g.Raw.TrimStart().TrimEnd())
      writer.WriteLine() // single blank line

let writeRawPgnGamesAdjustedToFile (filePath: string) (games: PgnGame seq) =
  PGNHelper.ensureDirectoryExists filePath
  File.Delete filePath
  let mutable idx = 0
  let mutable round = 0

  let sortByRound (round:string) =
         match round.Split '.' with
         | [|rNr|] -> int rNr * 1000
         | [|rNr; rest|] -> int rNr * 1000 + (rest |> int)
         | _ -> -1

  let groupGames =
      games
          |> Seq.sortBy(fun g -> sortByRound g.GameMetaData.Round)
          |> Seq.groupBy (fun g -> g.GameMetaData.Round.Split '.' |> Seq.tryHead)
          |> Seq.toList

  for rNr,games in groupGames  do
      for game in games do
          let newRaw =
              match rNr with
              | Some r ->
                  let roundNr = r |> int
                  if round <> roundNr then
                      round <- roundNr
                      idx <- 1
                  else
                      idx <- idx + 1
                  Regex.Replace(
                          game.Raw,
                          @"\[Round\s*""[^""]*""\]",
                          sprintf "[Round \"%s.%d\"]" r idx)
              | None -> game.Raw

          File.AppendAllText(filePath, newRaw)

let removePlayerFromPGN (playerToRemove : string, games : PgnGame seq, fileName : string ) =
  let playerToLower = playerToRemove.Trim().ToLower()
  let outputFile = fileName
  let filteredGames =
      games
      |> Seq.filter (fun g ->
          let playerFound =
              g.GameMetaData.White.ToLower().Contains playerToLower ||
              g.GameMetaData.Black.ToLower().Contains playerToLower
          not playerFound )
      |> Seq.toList
  writeRawPgnGamesAdjustedToFile outputFile filteredGames
