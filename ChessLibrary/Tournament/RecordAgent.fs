/// A run's record: its results, the moves shared for deviation prevention, the running deviation
/// total and when the standings are due. One agent owns all of it; games only send it messages, so
/// nothing here is shared between threads, and it writes the PGN itself, in the order games end.
module ChessLibrary.RecordAgent

open System
open System.Collections.Generic
open Microsoft.Extensions.Logging
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.PGNTypes
open ChessLibrary.GameReplay

/// What a run's record starts from.
type Setup =
  { Tourny: Tournament
    /// None when the run writes no PGN.
    Pgn: MailboxProcessor<FullPGNParser.PgnGameMessage> option
    ReferenceGames: PgnGame[]
    GamesAlreadyPlayed: PgnGame[]
    /// Standings after every this many games played.
    PeriodicEvery: int }

/// A game as it ended, for the record.
type FinishedGame =
  { Pairing: Pairing
    Result: Result
    /// Half-moves in the movetext, book included.
    Plies: int
    /// The game's moves from its start position, for deviation prevention.
    Moves: ResizeArray<string>
    Movetext: string
    /// The replay copies the game played with (Seed), merged back once it is recorded.
    Replay: (ReferenceGameReplay * ReferenceGameReplay) option
    Cancelled: bool }

type Recorded =
  { /// Played and recorded: not cancelled, not a game that never started.
    Played: bool
    Metadata: GameMetadata option
    /// The standings are due after this game (the caller takes Results when it sends them, so
    /// a slower game can never send an older snapshot after a newer one).
    PeriodicDue: bool }

let private notRecorded = { Played = false; Metadata = None; PeriodicDue = false }

/// A crashed or aborted game carries "1/2-1/2" purely as a placeholder (NotStarted is
/// Result.Empty from a cancellation race), so it is neither scored, written to the PGN nor
/// replayed: a written game counts in standings/SPRT and makes Scheduler.Diff treat the pair as
/// played on resume.
let isPlayed (game: FinishedGame) =
  not game.Cancelled
  && game.Result.Reason <> ResultReason.Cancel && game.Result.Reason <> ResultReason.NotStarted

type private Message =
  | Seed of Pairing * AsyncReplyChannel<Choice<(ReferenceGameReplay * ReferenceGameReplay) option, string>>
  | Finish of FinishedGame * AsyncReplyChannel<Choice<Recorded, string>>
  | Results of AsyncReplyChannel<Result list>

/// How long a game waits for the record. It never blocks, so a missing answer means it died.
let replyTimeoutMs = 30_000

type RecordAgent(setup: Setup, logger: ILogger) =
  let tourny = setup.Tourny
  // the moves established so far, one dictionary per engine; games get seeded copies
  let replayDicts = Dictionary<string, ReferenceGameReplay>()
  do for e in tourny.EngineSetup.Engines do replayDicts.[e.Name] <- ReferenceGameReplay()
  let replayList = ResizeArray<GameReplay>()
  let results = ResizeArray<Result>()
  // a resumed run continues the Deviations tags of the games already in the file, and so does
  // the live total the GUI shows (GameLoop counts on from it)
  let mutable deviations =
    let seeded = setup.GamesAlreadyPlayed |> Seq.tryLast |> Option.map (fun g -> g.GameMetaData.Deviations) |> Option.defaultValue 0
    max seeded tourny.DeviationCounter
  do tourny.DeviationCounter <- deviations
  let mutable played = 0

  let metadataOf (game: FinishedGame) (deviations: int) : GameMetadata =
    let pair, result = game.Pairing, game.Result
    // a Chess960 start position says so ([Variant]): read as standard chess, its castling is wrong
    let chess960 =
      not (String.IsNullOrWhiteSpace pair.Opening.Fen)
      && (try (let b = Chess.Board() in b.LoadFen pair.Opening.Fen; b.IsFRC) with _ -> false)
    // the game's own tags; a book game's tags of the same names (its players' Elo, its clock) do not carry over
    let tcOf (e: EngineConfig) =
      try (let c = tourny.FindTimeControl e.TimeControlID in c.PgnText (tourny.TimeControl.PeriodFor c)) with _ -> ""
    let whiteTc, blackTc = tcOf pair.White, tcOf pair.Black
    let ownTags =
      [ if chess960 then "Variant", "Chess960"
        if pair.White.Rating > 0 then "WhiteElo", string pair.White.Rating
        if pair.Black.Rating > 0 then "BlackElo", string pair.Black.Rating
        if whiteTc = blackTc && whiteTc <> "" then "TimeControl", whiteTc
        if whiteTc <> blackTc && whiteTc <> "" then "WhiteTimeControl", whiteTc
        if whiteTc <> blackTc && blackTc <> "" then "BlackTimeControl", blackTc ]
      |> List.map (fun (key, value) -> { Key = key; Value = value })
    let own = set [ "Variant"; "WhiteElo"; "BlackElo"; "TimeControl"; "WhiteTimeControl"; "BlackTimeControl"; "Termination" ]
    let bookTags = pair.Opening.GameMetaData.OtherTags |> List.filter (fun t -> not (own.Contains t.Key))
    { OpeningHash = pair.OpeningHash
      Event = tourny.Name
      Site = if String.IsNullOrWhiteSpace tourny.Site then "?" else tourny.Site
      // the PGN standard's date, YYYY.MM.DD
      Date = DateTime.Now.ToString("yyyy.MM.dd", Globalization.CultureInfo.InvariantCulture)
      Round = pair.RoundNr
      White = result.Player1
      Black = result.Player2
      Result = result.Result
      Reason = result.Reason
      GameTime = result.GameTime
      Moves = result.Moves
      PlyCount = game.Plies
      Fen = pair.Opening.Fen
      OpeningName = pair.Opening.GameMetaData.OpeningName
      Deviations = deviations
      StartEvals = result.OutOfOpeningEvals
      OtherTags = ownTags @ bookTags }

  let seed (pair: Pairing) =
    if not tourny.PreventMoveDeviation then None
    else
      let white, black = ReferenceGameReplay(), ReferenceGameReplay()
      // saved and reference games first, then what earlier games of this run established
      prepareGameReplay pair (Map.ofList [ pair.White.Name, white; pair.Black.Name, black ]) replayList setup.ReferenceGames setup.GamesAlreadyPlayed
      for kvp in replayDicts.[pair.White.Name] do
        if not (white.ContainsKey kvp.Key) then white.[kvp.Key] <- kvp.Value
      for kvp in replayDicts.[pair.Black.Name] do
        if not (black.ContainsKey kvp.Key) then black.[kvp.Key] <- kvp.Value
      Some (white, black)

  let finish (game: FinishedGame) =
    if not (isPlayed game) then notRecorded
    else
      // worked out first, changed after: a game that fails to be recorded leaves no trace
      let total = deviations + game.Result.GameDeviations
      let metadata = metadataOf game total
      let merge =
        match game.Replay with
        | Some (white, black) when tourny.PreventMoveDeviation ->
            Some (white, replayDicts.[game.Pairing.White.Name], black, replayDicts.[game.Pairing.Black.Name])
        | _ -> None
      merge |> Option.iter (fun (white, whiteMoves, black, blackMoves) ->
        for kvp in white do whiteMoves.[kvp.Key] <- kvp.Value
        for kvp in black do blackMoves.[kvp.Key] <- kvp.Value
        GamePersistence.addToReplayList replayList tourny game.Result metadata game.Moves)
      deviations <- total
      results.Add game.Result
      match setup.Pgn with
      | Some pgn when not (String.IsNullOrWhiteSpace tourny.PgnOutPath) ->
          pgn.Post (FullPGNParser.WriteGame(tourny.PgnOutPath, metadata, game.Movetext, game.Result))
      | _ -> ()
      played <- played + 1
      { Played = true
        Metadata = Some metadata
        PeriodicDue = setup.PeriodicEvery > 0 && played % setup.PeriodicEvery = 0 }

  let agent =
    MailboxProcessor<Message>.Start(fun inbox ->
      let rec loop () = async {
        let! message = inbox.Receive()
        // every message is answered: an exception that ended the loop would hang every game
        match message with
        | Seed (pair, reply) ->
            // a failure fails the game: played without its replay it would deviate and still be scored
            try reply.Reply (Choice1Of2 (seed pair))
            with ex ->
              logger.LogError(ex, "Replay for {White} vs {Black} not seeded", pair.White.Name, pair.Black.Name)
              reply.Reply (Choice2Of2 ex.Message)
        | Finish (game, reply) ->
            try reply.Reply (Choice1Of2 (finish game))
            with ex ->
              logger.LogError(ex, "Game {White} vs {Black} not recorded", game.Pairing.White.Name, game.Pairing.Black.Name)
              reply.Reply (Choice2Of2 ex.Message)
        | Results reply -> reply.Reply (List.ofSeq results)
        return! loop () }
      loop ())

  let ask (build: AsyncReplyChannel<'T> -> Message) (what: string) = async {
    match! agent.PostAndTryAsyncReply(build, replyTimeoutMs) with
    | Some answer -> return answer
    | None -> return failwithf "The run's record did not answer (%s) within %d ms" what replyTimeoutMs }

  /// The replay copies a game plays with; None without deviation prevention.
  member _.Seed(pair: Pairing) = async {
    match! ask (fun reply -> Seed (pair, reply)) "seed" with
    | Choice1Of2 replay -> return replay
    | Choice2Of2 error -> return failwithf "Replay not seeded: %s" error }

  /// Records a game that ended: scored, merged for replay and written to the PGN when it was played.
  member _.Finish(game: FinishedGame) : Async<Recorded> = async {
    match! ask (fun reply -> Finish (game, reply)) "finish" with
    | Choice1Of2 recorded -> return recorded
    | Choice2Of2 error -> return failwithf "Game not recorded: %s" error }

  /// Every result recorded so far, in the order the games ended.
  member _.Results() = ask Results "results"
