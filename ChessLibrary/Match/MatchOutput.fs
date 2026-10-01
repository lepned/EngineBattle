namespace ChessLibrary.Match

open System
open System.Text
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.MiscTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.Match.MatchStats

/// What the reference prints while a match runs and when it ends (matchmaking/output/*.hpp,
/// tournament/roundrobin/roundrobin.cpp, tournament/base/tournament.cpp and main.cpp at 60d7a7a),
/// in both its own and the cutechess format, driven by EngineBattle's games. Text is produced with "\n" line ends, as the reference writes them; the console
/// turns them into the platform's (the reference's stdout is in text mode on Windows).
module MatchOutput =

  let private f2 = MatchFormat.fixedPoint 2
  let private dashes = String('-', 50) + "\n"

  let private colour (white: bool) = if white then "White" else "Black"

  /// The `{...}` text of `Finished game` for an EngineBattle result (§3). A time loss's overrun is
  /// how far below zero EngineBattle's clock went (Result.TimeOverrunMs).
  let annotation (white: string) (r: Result) =
    let whiteWon = r.Result = "1-0"
    let decisive = r.Result = "1-0" || r.Result = "0-1"
    let winner = colour whiteWon
    let loser = colour (not whiteWon)
    match r.Reason with
    | Checkmate -> $"{winner} mates"
    | Stalemate -> "Draw by stalemate"
    | AdjudicateMaterial -> "Draw by insufficient mating material"
    | Repetition -> "Draw by 3-fold repetition"
    | ExcessiveMoves -> "Draw by fifty moves rule"
    | AdjudicateTB -> if decisive then $"{winner} wins by adjudication: SyzygyTB" else "Draw by adjudication: SyzygyTB"
    | AdjudicatedEvaluation
    | AdjudicatedByUser -> if decisive then $"{winner} wins by adjudication" else "Draw by adjudication"
    | ForfeitLimits -> $"{loser} loses on time ({r.TimeOverrunMs}ms overrun)"
    | Illegal -> $"{loser} makes an illegal move"
    | Resignation -> $"{loser} resigns"
    | Disconnected name -> $"{colour (name = white)} disconnects"
    | Cancel
    | NotStarted -> "Game interrupted"

  /// formatStats: the white-view result.
  let resultText (r: string) =
    match r with
    | "1-0" | "0-1" | "1/2-1/2" -> r
    | _ -> "*"

  /// getTime: an engine's limit as the H2H header shows it.
  let timeControl (e: MatchArgs.EngineConfig) =
    let tc = e.Tc
    if tc.Time + tc.Increment > 0L then
      let moves = if tc.Moves > 0L then $"{tc.Moves}/" else ""
      let inc = if tc.Increment > 0L then "+" + MatchFormat.general2 (float tc.Increment / 1000.0) else ""
      moves + MatchFormat.shortest (float tc.Time / 1000.0) + inc
    elif tc.FixedTime > 0L then MatchFormat.shortest (float tc.FixedTime / 1000.0) + "/move"
    elif e.Plies > 0L then $"{e.Plies} plies"
    elif e.Nodes > 0L then $"{e.Nodes} nodes"
    else ""

  let private oneOrBoth (a: string) (b: string) = if a = b then a else $"{a} - {b}"

  let private shortName (path: string) =
    match path.LastIndexOfAny [| '/'; '\\' |] with
    | -1 -> path
    | i -> path.Substring(i + 1)

  let private pentaCounts (s: Stats) = $"[{s.PentaLL}, {s.PentaLD}, {s.PentaWL + s.PentaDD}, {s.PentaWD}, {s.PentaWW}]"

  let started (id: int) (total: int) (white: string) (black: string) = $"Started game {id} of {total} ({white} vs {black})\n"

  let finished (id: int) (white: string) (black: string) (result: string) (note: string) =
    $"Finished game {id} ({white} vs {black}): {resultText result} {{{note}}}\n"

  /// The ranking table for more than two engines; `penta` adds the Ptnml column (default format only).
  let private table (penta: bool) (withPtnml: bool) (rows: (string * Stats) list) =
    let elos = rows |> List.map (fun (n, s) -> n, s, MatchStats.elo penta s)
    let less (a: Elo) (b: Elo) = (not (Double.IsNaN a.Diff) && Double.IsNaN b.Diff) || a.Diff > b.Diff
    let sorted = elos |> List.sortWith (fun (_, _, a) (_, _, b) -> if less a b then -1 elif less b a then 1 else 0)
    let w = max 25 (rows |> List.map (fst >> String.length) |> List.fold max 0)
    let sb = StringBuilder()
    let head = $"""{"Rank",-4} {"Name".PadRight w} {"Elo",10} {"+/-",10} {"nElo",10} {"+/-",10} {"Games",10} {"Score",10} {"Draw",10}"""
    sb.Append(head).Append(if withPtnml then $""" {"Ptnml(0-2)",20}""" else "").Append('\n') |> ignore
    sorted
    |> List.iteri (fun i (name, s, e) ->
      let draw = if penta then s.DrawRatioPenta else s.DrawRatio
      let pad10 (x: float) = (f2 x).PadLeft 10
      let pct (x: float) = (MatchFormat.fixedPoint 1 x).PadLeft 9 + "%"
      sb.Append(string (i + 1) |> fun r -> r.PadLeft 4).Append(' ').Append(name.PadRight w).Append(' ')
        .Append(pad10 e.Diff).Append(' ').Append(pad10 e.Error).Append(' ').Append(pad10 e.NEloDiff).Append(' ')
        .Append(pad10 e.NEloError).Append(' ').Append((string s.Sum).PadLeft 10).Append(' ')
        .Append(pct s.PointsRatio).Append(' ').Append(pct draw) |> ignore
      if withPtnml then sb.Append(' ').Append((if penta then pentaCounts s else "").PadLeft 20) |> ignore
      sb.Append('\n') |> ignore)
    sb.ToString()

  /// What the output needs to know about the match.
  type Config =
    { Output: MatchArgs.OutputType
      ReportPenta: bool
      RatingInterval: int
      ScoreInterval: int
      Sprt: MatchSprt.Sprt
      /// command-line order
      Engines: MatchArgs.EngineConfig list
      /// an engine's current UCI option value (its default or what was set), None without the option
      EngineOption: string -> string -> string option
      Book: string
      /// the whole plan, games already played included
      TotalGames: int
      /// games played before this run (a resume)
      PriorGames: int }

  /// How the run ended.
  type Ending =
    | Completed
    | SprtStopped
    | Interrupted of program: string * configName: string

  /// The reference's output for one match, fed from EngineBattle's updates. Thread-safe: every call
  /// takes the output lock, as the reference's output_mutex_. `write` gets the exact text.
  type Reporter(cfg: Config, write: string -> unit) =
    let gate = obj ()
    let names = cfg.Engines |> List.map (fun e -> e.Name)
    let board = MatchScoreboard.Scoreboard(names, cfg.ReportPenta)
    let order = names |> List.mapi (fun i n -> n, i) |> dict
    let byName = cfg.Engines |> List.map (fun e -> e.Name, e) |> dict
    let mutable matchCount = cfg.PriorGames
    let mutable decided = false
    let tracked = Collections.Generic.List<string * int ref * int ref>()   // name, timeouts, crashes

    let track (name: string) (timeout: bool) =
      let entry =
        match tracked |> Seq.tryFind (fun (n, _, _) -> n = name) with
        | Some e -> e
        | None -> (let e = (name, ref 0, ref 0) in tracked.Add e; e)
      let _, t, c = entry
      if timeout then t.Value <- t.Value + 1 else c.Value <- c.Value + 1

    let option (name: string) (opt: string) (unit': string) =
      match cfg.EngineOption name opt with
      | Some v -> v + unit'
      | None -> "NULL"

    let printResult (s: Stats) (first: string) (second: string) =
      if cfg.Output = MatchArgs.Cutechess then
        let e = MatchStats.eloWdl s
        write $"Score of {first} vs {second}: {s.Wins} - {s.Losses} - {s.Draws}  [{MatchFormat.fixedPoint 3 e.Score}] {s.Sum}\n"

    let printElo (s: Stats) (first: string) (second: string) =
      match cfg.Output with
      | MatchArgs.Cutechess ->
        if names.Length = 2 then
          let e = MatchStats.eloWdl s
          $"Elo difference: {e.GetElo}, LOS: {e.Los}, DrawRatio: {f2 s.DrawRatio} %%\n"
        else table false false (names |> List.map (fun n -> n, board.EngineStats n))
      | MatchArgs.Default ->
        if names.Length = 2 then
          let e = MatchStats.elo cfg.ReportPenta s
          let a, b = byName.[first], byName.[second]
          let book = shortName cfg.Book
          let header =
            let tc = oneOrBoth (timeControl a) (timeControl b)
            let threads = oneOrBoth (option first "Threads" "t") (option second "Threads" "t")
            let hash = oneOrBoth (option first "Hash" "MB") (option second "Hash" "MB")
            let bookPart = if book = "" then "" else $", {book}"
            $"Results of {first} vs {second} ({tc}, {threads}, {hash}{bookPart}):"
          let draw = if cfg.ReportPenta then s.DrawRatioPenta else s.DrawRatio
          let pairs = if cfg.ReportPenta then $", PairsRatio: {f2 s.PairsRatio}" else ""
          let lines =
            [ header
              $"Elo: {e.GetElo}, nElo: {e.NElo}"
              $"LOS: {e.Los}, DrawRatio: {f2 draw} %%{pairs}"
              $"Games: {s.Sum}, Wins: {s.Wins}, Losses: {s.Losses}, Draws: {s.Draws}, Points: {MatchFormat.fixedPoint 1 s.Points} ({f2 s.PointsRatio} %%)" ]
            @ (if cfg.ReportPenta then [ $"Ptnml(0-2): {pentaCounts s}, WL/DD Ratio: {f2 s.WlDdRatio}" ] else [])
          String.Join("\n", lines) + "\n"
        else table cfg.ReportPenta true (names |> List.map (fun n -> n, board.EngineStats n))

    let printSprt (s: Stats) =
      let sp = cfg.Sprt
      if not sp.Enabled then ""
      else
        match cfg.Output with
        | MatchArgs.Default ->
          let llr = MatchSprt.llr sp s cfg.ReportPenta
          $"LLR: {f2 llr} ({MatchFormat.fixedPoint 1 (MatchSprt.fraction sp llr * 100.0)}%%) {MatchSprt.bounds sp} {MatchSprt.eloRange sp}\n"
        | MatchArgs.Cutechess ->
          let llr = MatchSprt.llr sp s false
          let pct = if llr < 0.0 then llr / sp.Lower * 100.0 else llr / sp.Upper * 100.0
          let result = if llr >= sp.Upper then " - H1 was accepted" elif llr < sp.Lower then " - H0 was accepted" else ""
          $"SPRT: llr {f2 llr} ({MatchFormat.fixedPoint 1 pct}%%), lbound {f2 sp.Lower}, ubound {f2 sp.Upper}{result}\n"

    let printInterval (s: Stats) (first: string) (second: string) =
      match cfg.Output with
      | MatchArgs.Default -> write (dashes + printElo s first second + printSprt s + dashes)
      | MatchArgs.Cutechess -> write (printElo s first second + printSprt s)

    /// the pair index of a game's `{pair}.{colour}` label
    let pairIndex (round: string) =
      match Int32.TryParse(round.Split('.').[0]) with
      | true, n -> n
      | _ -> 0

    member _.Scoreboard = board

    /// Reads the scoreboard under the output lock (it is not thread-safe, and games keep ending
    /// on other threads while, say, config.json is autosaved).
    member _.WithScoreboard(read: MatchScoreboard.Scoreboard -> 'a) : 'a = lock gate (fun () -> read board)

    /// A game played before this run (a resume): counted on the scoreboard, not printed. The
    /// stats are rebuilt from the PGN this way rather than taken from config.json, so they are the
    /// games EngineBattle resumes from. Config.PriorGames is their number.
    member _.Preload(white: string, black: string, result: string, openingHash: string) =
      lock gate (fun () -> board.Add(white, black, result, openingHash) |> ignore)

    /// `Started game`, for a game of this run numbered from 1 (EngineBattle's pairing number).
    member _.Started(gameNr: int, white: string, black: string) =
      lock gate (fun () -> if not decided then write (started (cfg.PriorGames + gameNr) cfg.TotalGames white black))

    /// A game's end: `Finished game`, the scoreboard, the result and rating intervals and the SPRT,
    /// in the reference's order. Returns the SPRT decision that stops the match, once.
    member _.Finished(g: FinishedGame) : MatchSprt.SprtResult option =
      lock gate (fun () ->
        if decided then None
        else
          let id = cfg.PriorGames + g.GameNr
          write (finished id g.White g.Black g.Result.Result (annotation g.White g.Result))
          match g.Result.Reason with
          | ForfeitLimits -> track (if g.Result.Result = "1-0" then g.Black else g.White) true
          | Disconnected name -> track name false
          | _ -> ()
          if not (order.ContainsKey g.White && order.ContainsKey g.Black) then None   // not this match's game
          else
          let pairDone = board.Add(g.White, g.Black, g.Result.Result, g.OpeningHash)
          let first, second = if order.[g.White] <= order.[g.Black] then g.White, g.Black else g.Black, g.White
          let stats = board.Stats(first, second)
          let last = matchCount + 1 = cfg.TotalGames
          if (matchCount + 1) % cfg.ScoreInterval = 0 || last then printResult stats first second
          let index = if cfg.ReportPenta then pairIndex g.RoundNr else matchCount + 1
          if (index % cfg.RatingInterval = 0 && (pairDone || not cfg.ReportPenta)) || last then
            printInterval stats first second
          let decision =
            if not cfg.Sprt.Enabled then None
            else
              match MatchSprt.result cfg.Sprt (MatchSprt.llr cfg.Sprt stats cfg.ReportPenta) with
              | MatchSprt.Continue -> None
              | r ->
                decided <- true
                printResult stats first second
                printInterval stats first second
                let h = if r = MatchSprt.H0 then "H0" else "H1"
                write (if cfg.Output = MatchArgs.Default then $"SPRT ({MatchSprt.eloRange cfg.Sprt}) completed - {h} was accepted\n"
                       else "Tournament finished\n")
                Some r
          matchCount <- matchCount + 1
          decision)

    /// The end of the run: the player table (default format), the interrupted message,
    /// `Finished match` and `Total Time`. Returns the exit code.
    member _.End(ending: Ending, elapsed: TimeSpan) : int =
      lock gate (fun () ->
        if cfg.Output = MatchArgs.Default && tracked.Count > 0 then
          write "\n"
          for name, t, c in tracked do
            write $"Player: {name}\n  Timeouts: {t.Value}\n  Crashed: {c.Value}\n"
          write "\n"
        match ending with
        | Interrupted(program, configName) ->
          write $"Tournament was interrupted. To resume the tournament, run: {program} -config file={configName}\n"
        | _ -> ()
        write "Finished match\n"
        let h = int elapsed.TotalHours
        write $"Total Time: {h:D2}:{elapsed.Minutes:D2}:{elapsed.Seconds:D2} (hours:minutes:seconds)\n\n"
        match ending with
        | Interrupted _ -> 1
        | _ -> 0)
