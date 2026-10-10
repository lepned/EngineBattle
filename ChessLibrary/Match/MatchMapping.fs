namespace ChessLibrary.Match

open System
open System.Collections.Generic
open System.IO
open ChessLibrary
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TimeControlTypes

/// A parsed match command line (MatchArgs) as an EngineBattle tournament: the settings EngineBattle
/// runs on, with EngineBattle's own rules for how games are scheduled, played and adjudicated
/// (MatchMode.md, "Approach"). What EngineBattle cannot honour yet either stops the run with
/// an error - when the games would be different games, such as a fixed time per move - or is kept
/// as a note, when only a detail differs; the notes go to the log, never to stdout.
module MatchMapping =

  type Mapped =
    { Tournament: Tournament
      /// settings that are accepted but not (yet) acted on, one line each
      Notes: string list }

  /// Eval adjudication has no off switch in EngineBattle (zeros adjudicate at once), so an
  /// adjudication the reference leaves off starts at a move number no game reaches: the longest legal
  /// game is 5949 moves. Kept that small because the tournament's time estimate counts it.
  let private never = 6_000

  let private pawns (cp: int) = float cp / 100.0

  /// The engine's path as EngineBattle starts it: dir/cmd made absolute; a bare command name
  /// (no directory) stays bare, so the OS finds it on PATH as the reference's would.
  let enginePath (e: MatchArgs.EngineConfig) =
    let p = e.EnginePath
    if e.Dir = "" && Path.GetDirectoryName p = "" then p else Path.GetFullPath p

  /// One EngineBattle time setting for one engine limit, or why it cannot be one.
  let private timeConfig (e: MatchArgs.EngineConfig) : Result<TimeConfig * string list, string> =
    let tc = e.Tc
    let timed = tc.Time + tc.Increment <> 0L
    let notes = ResizeArray<string>()
    let perMove = tc.FixedTime <> 0L
    // timemargin with st is the tournament's MoveOverhead (map, below); a clock is EngineBattle's own rule
    if tc.TimeMargin <> 0L && not (perMove && e.Nodes = 0L) then
      notes.Add $"{e.Name}: timemargin={tc.TimeMargin} is not applied; a time loss is EngineBattle's clock rule"
    if e.Plies <> 0L && e.Nodes = 0L && not timed && not perMove then
      Error $"Error: {e.Name}: a depth limit (depth=/plies=) is not supported by EngineBattle yet."
    elif e.Nodes > int64 Int32.MaxValue then Error $"Error: {e.Name}: nodes={e.Nodes} is larger than EngineBattle supports ({Int32.MaxValue})."
    else
      let limitedBy = if e.Nodes > 0L then "nodes" elif perMove then "st" else "the clock"
      if e.Plies <> 0L then notes.Add $"{e.Name}: depth={e.Plies} is not applied; the search is limited by {limitedBy} only"
      let none = { Id = 0; Fixed = TimeSpan.Zero; Increment = TimeSpan.Zero; NodeLimit = false; Nodes = 0
                   MoveTime = TimeSpan.Zero; MovesToGo = 0 }
      if e.Nodes > 0L then
        if timed then notes.Add $"{e.Name}: tc= is not applied together with nodes=; the search is limited by nodes only"
        if perMove then notes.Add $"{e.Name}: st= is not applied together with nodes=; the search is limited by nodes only"
        Ok({ none with NodeLimit = true; Nodes = int e.Nodes }, List.ofSeq notes)
      elif perMove then
        // `go movetime`, no clock (tc= together with st= is refused by the parser)
        Ok({ none with MoveTime = TimeSpan.FromMilliseconds(float tc.FixedTime) }, List.ofSeq notes)
      else
        // moves per period are the engine's own, as the reference's clock keeps them per engine
        if tc.Moves > int64 Int32.MaxValue then notes.Add $"{e.Name}: moves-to-go {tc.Moves} is capped at {Int32.MaxValue}"
        Ok({ none with
               Fixed = TimeSpan.FromMilliseconds(float tc.Time)
               Increment = TimeSpan.FromMilliseconds(float tc.Increment)
               MovesToGo = int (min tc.Moves (int64 Int32.MaxValue)) }, List.ofSeq notes)

  /// The reference sends Threads first (a stable partition of the option list); a repeated option
  /// keeps its first place and takes its last value, which is what the engine ends up with.
  let private options (e: MatchArgs.EngineConfig) =
    let d = Dictionary<string, obj>()
    let ordered = (e.Options |> List.filter (fun (k, _) -> k = "Threads")) @ (e.Options |> List.filter (fun (k, _) -> k <> "Threads"))
    for k, v in ordered do d.[k] <- box v
    d

  let map (p: MatchArgs.Parsed) : Result<Mapped, string> =
    let t = p.Tournament
    let notes = ResizeArray<string>()
    let note (s: string) = notes.Add s

    // ponder: all engines or none - pondering is the tournament's (AllowPondering), and a field
    // where some ponder and others do not is not played on equal terms
    let pondering = p.Engines |> List.filter (fun e -> e.Ponder) |> List.length
    match (if pondering > 0 && pondering < p.Engines.Length then Some "Error: ponder is given to some engines only. EngineBattle lets all engines ponder or none: use -each ponder, or leave it out." else None) with
    | Some e -> Error e
    | None ->
    // one time setting per distinct engine limit
    let limits = p.Engines |> List.map timeConfig
    match limits |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
    | Some e -> Error e
    | None ->
    let limits = limits |> List.map (function Ok x -> x | Error _ -> failwith "unreachable")
    for _, ns in limits do notes.AddRange ns
    let distinct = limits |> List.map fst |> List.distinct
    let timeConfigs = distinct |> List.mapi (fun i c -> { c with Id = i + 1 })
    let idOf (c: TimeConfig) = (List.findIndex ((=) c) distinct) + 1

    // timemargin= with st= is the tournament's MoveOverhead: a margin for a time per move, not sent
    // to the engines. One for all, so it applies only when every engine plays st=, the largest.
    let perMove (e: MatchArgs.EngineConfig) = e.Tc.FixedTime <> 0L && e.Nodes = 0L
    let moveOverhead =
      let margins = p.Engines |> List.filter perMove |> List.map (fun e -> max 0L e.Tc.TimeMargin)
      if margins.IsEmpty || List.max margins = 0L then 0L
      elif not (List.forall perMove p.Engines) then
        note "timemargin= is not applied: EngineBattle has one margin for all engines, and only when all play st="
        0L
      else
        if (List.distinct margins).Length > 1 then
          note $"engines have different timemargin= values; EngineBattle uses one for all: {List.max margins}"
        List.max margins

    let gauntlet = t.Type = MatchArgs.Gauntlet
    let seeds = if gauntlet then max 1 t.GauntletSeeds else 0
    if gauntlet && seeds > 1 then note $"-seeds {seeds}: EngineBattle's gauntlet does not pair the seeds with each other"

    let engines =
      List.zip p.Engines limits
      |> List.mapi (fun i (e, (limit, _)) ->
        { CoreTypes.EngineConfig.Empty with
            Name = e.Name
            Alias = e.Name
            // a command line gives no rating: unknown, not Empty's placeholder (it would be written as Elo)
            Rating = 0
            Path = enginePath e
            Args = e.Args
            Options = options e
            TimeControlID = idOf limit
            IsChallenger = gauntlet && i < seeds })
    for e in p.Engines do
      if e.Restart then note $"{e.Name}: restart=on is not applied; EngineBattle reuses the engine process"
      if e.Dir <> "" && Path.GetDirectoryName e.Cmd <> "" then
        note $"{e.Name}: the engine runs in its own folder ({Path.GetDirectoryName(enginePath e)}), not in dir={e.Dir}"

    // openings
    let o = t.Opening
    if o.Start > 1 then note $"-openings start={o.Start} is not applied; the book is used from its first opening"
    let extensionFormat =
      if o.File.ToLowerInvariant().Contains ".epd" then MatchArgs.Epd
      elif o.File <> "" then MatchArgs.Pgn
      else MatchArgs.NoFormat
    if o.File <> "" && o.Format <> extensionFormat then
      let readAs = if extensionFormat = MatchArgs.Epd then "EPD" else "PGN"
      note $"-openings format= is not applied; EngineBattle reads {o.File} as {readAs} by its name"
    if not t.NoSwap && t.Reverse then note "-reverse is not applied"
    if t.NoSwap then note "-noswap is not applied; the colours alternate"

    // adjudication
    let draw =
      if t.Draw.Enabled then { MinDrawMove = t.Draw.MoveNumber; DrawMoveLength = t.Draw.MoveCount; MaxDrawScore = pawns t.Draw.Score }
      else { MinDrawMove = never; DrawMoveLength = 1; MaxDrawScore = 0.0 }
    let win =
      if t.Resign.Enabled then { MinWinMove = 0; WinMoveLength = t.Resign.MoveCount; MinWinScore = pawns t.Resign.Score }
      else { MinWinMove = never; WinMoveLength = 1; MinWinScore = 1000.0 }
    if t.MaxMoves.Enabled then note $"-maxmoves {t.MaxMoves.MoveCount} is not applied"
    let tb = t.TbAdjudication
    let tbDir =
      if not tb.Enabled then ""
      else
        // several folders: joined as Fathom expects them on this platform
        let dirs = tb.SyzygyDirs.Split([| ';'; Path.PathSeparator |], StringSplitOptions.RemoveEmptyEntries)
        if tb.Ignore50MoveRule then note "-tbignore50 is not applied"
        if tb.ResultType <> MatchArgs.Both then note "-tbadjudicate is not applied; wins, losses and draws are all adjudicated"
        String.Join(string Path.PathSeparator, dirs)
    // without -tbpieces, as large as the tables go (7 when the folders hold none yet)
    let tbMen =
      if tb.MaxPieces > 0 then tb.MaxPieces
      else match ChessLibrary.TablebaseProbe.largestTable tbDir with 0 -> 7 | n -> n
    let tbAdj = { TablebaseDirectory = tbDir; UseTBAdjudication = tb.Enabled; TBMen = tbMen }

    // output and the rest
    let pgnPath =
      if t.Pgn.File <> "" then t.Pgn.File
      else
        let tmp = Path.Combine(Path.GetTempPath(), "EngineBattle", "match_" + DateTime.Now.ToString("yyyyMMdd_HHmmss") + ".pgn")
        note $"no -pgnout: the games are written to {tmp}"
        tmp
    if t.Epd.File <> "" then note "-epdout is not applied"
    if t.Recover |> not then note "without -recover, EngineBattle still restarts an engine that crashes"

    let tournament =
      { Tournament.Empty with
          // the PGN's Event and Site, as -event and -site give them
          Name = t.Pgn.EventName
          Description = t.Pgn.EventName
          Site = t.Pgn.Site
          ConsoleOnly = true
          TournamentMode = if gauntlet then "Gauntlet" else "RR"
          Challengers = seeds
          Rounds = t.Rounds
          AllowPondering = (pondering > 0)
          DelayBetweenGames = TimeSpan.FromMilliseconds(float (max 0 t.Wait))
          MoveOverhead = TimeSpan.FromMilliseconds(float moveOverhead)
          EngineStartupTimeoutInSec = int (Math.Ceiling(float t.StartupMs / 1000.0))
          Adjudication = { DrawOption = draw; WinOption = win; TBAdj = tbAdj }
          TestOptions = { Tournament.Empty.TestOptions with NumberOfGamesInParallel = max 1 t.Concurrency }
          Opening =
            { OpeningsPath = (if o.File = "" then None else Some o.File)
              OpeningsTwice = (t.Games = 2)
              OpeningsPly = (if o.Plies < 0 then Int32.MaxValue else o.Plies)
              RandomOpenings = (o.Order = MatchArgs.Random)
              Seed = int (t.Seed % uint64 Int32.MaxValue) }
          PgnOutPath = pgnPath
          EngineSetup = { Engines = engines; EngineDefFolder = ""; EngineDefList = [] }
          TimeControl = { TimeConfigs = timeConfigs; WmovesToGo = 0; BmovesToGo = 0 } }
    Ok { Tournament = tournament; Notes = List.ofSeq notes }
