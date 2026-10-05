/// `enginecheck` (diagnostic): checks that a UCI engine follows the protocol - start-up, options,
/// positions, search limits, info output, stop/isready, ponder, edge cases, quit - and that it
/// behaves in EngineBattle's analysis pages. Exit code 1 when a check fails.
module EngineCheck

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Threading.Channels
open Microsoft.Extensions.Logging.Abstractions
open ChessLibrary
open ChessLibrary.EngineTypes
open ChessLibrary.MoveTypes
open ChessLibrary.TypesDef.CoreTypes

type Params =
  { Config: EngineConfig
    Nodes: int
    /// Searches by time instead of nodes (CECP has no node limit).
    MoveTimeMs: int option
    /// Pause between navigation requests (ms).
    DelayMs: int
    /// Pause between go and stop in "stop right after go" (ms); 0 tests the race itself.
    StopDelayMs: int
    Rounds: int
    Moves: string list
    /// Groups to run; empty = all.
    Only: string list }

let groups = [ "startup"; "options"; "positions"; "limits"; "info"; "stop"; "ponder"; "edge"; "quit"; "analysis" ]

/// A Ruy Lopez, 30 plies.
let defaultMoves =
  [ "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1b5"; "a7a6"; "b5a4"; "g8f6"; "e1g1"; "f8e7"
    "f1e1"; "b7b5"; "a4b3"; "d7d6"; "c2c3"; "e8g8"; "h2h3"; "c6b8"; "d2d4"; "b8d7"
    "b1d2"; "c8b7"; "b3c2"; "f8e8"; "d2f1"; "e7f8"; "f1g3"; "g7g6"; "a2a4"; "c7c5" ]

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

// ── Verdicts ────────────────────────────────────────────────────────────────────────────────────

type private Verdict = Pass | Warn | Fail | Skip

type private Report() =
  let results = ResizeArray<Verdict>()
  // every verdict with its group, for the summary at the end
  let entries = ResizeArray<Verdict * string * string * string>()
  /// The group the checks belong to.
  member val Group = "startup" with get, set
  /// After a FAIL line: what led to it (the session's last commands, or where the I/O log is).
  member val OnFail : unit -> unit = ignore with get, set
  member this.Add (verdict: Verdict) (name: string) (detail: string) =
    results.Add verdict
    entries.Add((verdict, this.Group, name, detail))
    let text, color =
      match verdict with
      | Pass -> "PASS", ConsoleColor.Green
      | Warn -> "WARN", ConsoleColor.Yellow
      | Fail -> "FAIL", ConsoleColor.Red
      | Skip -> "SKIP", ConsoleColor.DarkGray
    let old = Console.ForegroundColor
    Console.Write "  "
    Console.ForegroundColor <- color
    Console.Write text
    Console.ForegroundColor <- old
    printfn "  %-26s %s" name detail
    if verdict = Fail then (try this.OnFail () with _ -> ())
  member _.Count v = results |> Seq.filter ((=) v) |> Seq.length
  member _.Entries = entries.ToArray()

let private header (name: string) = printfn "\n%s" name

/// One transcript line on one console line.
let private cut (line: string) = if line.Length > 150 then line.Substring(0, 147) + "..." else line

/// Pass within `good` ms, Warn within `bad`, else Fail.
let private byTime (ms: int64) (good: int64) (bad: int64) =
  if ms <= good then Pass elif ms <= bad then Warn else Fail

// ── EB's board, for legality ───────────────────────────────────────────────────────────────────

let private isLegal (board: Chess.Board) (move: string) =
  let moves = board.GenerateMoves()
  (TMoveOps.tryFindMoveByUciNotation moves moves.Length board.Position.STM move).IsSome

/// The board after a position's moves; None when one of them is not legal for EB.
let private boardAfter (fen: string) (moves: string list) =
  let board = Chess.Board()
  board.LoadFen fen
  let ok = moves |> List.forall (fun m -> isLegal board m && (board.PlayUciMove m; true))
  if ok then Some board else None

/// How many moves of a PV are legal in turn.
let private legalPrefix (fen: string) (pv: string list) =
  let b = Chess.Board()
  b.LoadFen fen
  pv |> List.takeWhile (fun m -> isLegal b m && (b.PlayUciMove m; true)) |> List.length

let private tokens (line: string) = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

/// The value after `key` in a line.
let private field (key: string) (line: string) =
  let t = tokens line
  match Array.tryFindIndex ((=) key) t with
  | Some i when i + 1 < t.Length -> Some t.[i + 1]
  | _ -> None

/// A token shaped like a UCI move (e2e4, a7a8q, 0000).
let private isMoveToken (t: string) =
  t = "0000"
  || ((t.Length = 4 || t.Length = 5)
      && t.[0] >= 'a' && t.[0] <= 'h' && t.[1] >= '1' && t.[1] <= '8'
      && t.[2] >= 'a' && t.[2] <= 'h' && t.[3] >= '1' && t.[3] <= '8'
      && (t.Length = 4 || "qrbn".Contains(Char.ToLower t.[4])))

/// The pv's moves; it ends at the next keyword (some engines add `string ...` after it).
let private pvOf (line: string) =
  let t = tokens line
  match Array.tryFindIndex ((=) "pv") t with
  | Some i -> t.[i + 1 ..] |> Array.takeWhile isMoveToken |> List.ofArray
  | None -> []

let private isBestMove (line: string) = line.StartsWith("bestmove", StringComparison.Ordinal)
let private isReadyOk (line: string) = line.Trim() = "readyok"
let private bestMoveOf (line: string) = field "bestmove" line |> Option.defaultValue ""

/// Entries (ms, ">> command" / "<< answer") from the last `commands` sent on, each with its time
/// from the first; a run of info lines is one line.
let private transcriptLines (commands: int) (entries: struct (int64 * string)[]) =
  let sends = entries |> Array.mapi (fun i (struct (_, t)) -> i, t) |> Array.filter (fun (_, t) -> t.StartsWith ">>") |> Array.map fst
  let from = if sends.Length = 0 then max 0 (entries.Length - 20) else sends.[max 0 (sends.Length - commands)]
  let tail = entries.[from ..]
  if tail.Length = 0 then [] else
  let struct (t0, _) = tail.[0]
  let isInfo (t: string) = t.StartsWith "<< info"
  let infoRun n (last: string) =
    if n = 1 then last else sprintf "           << (%d info lines, the last:) %s" n (last.Substring(last.IndexOf("<< ") + 3))
  [ let mutable infos = 0
    let mutable lastInfo = ""
    for struct (ms, text) in tail do
      if isInfo text then
        infos <- infos + 1
        lastInfo <- sprintf "%6d ms  %s" (ms - t0) text
      else
        if infos > 0 then yield infoRun infos lastInfo
        infos <- 0
        yield sprintf "%6d ms  %s" (ms - t0) text
    if infos > 0 then yield infoRun infos lastInfo ]

/// The analysis engine's I/O log ("[HH:mm:ss.fff] >>> command" / "<<< answer") as transcript entries.
let private ioLogEntries (path: string) =
  try
    use stream = new IO.FileStream(path, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
    use reader = new IO.StreamReader(stream)
    let lines = reader.ReadToEnd().Split('\n')
    [| for line in lines do
         let line = line.TrimEnd('\r')
         if line.Length > 19 && line.[0] = '[' && line.[13] = ']' then
           match TimeSpan.TryParse(line.Substring(1, 12)) with
           | true, at ->
               let rest = line.Substring 15
               let text =
                 if rest.StartsWith ">>> " then ">> " + rest.Substring 4
                 elif rest.StartsWith "<<< " then "<< " + rest.Substring 4
                 else ""
               if text <> "" then yield struct (int64 at.TotalMilliseconds, text)
           | _ -> () |]
  with _ -> [||]

// ── A raw UCI session ──────────────────────────────────────────────────────────────────────────

type private Session(config: EngineConfig) =
  let lines = Channel.CreateUnbounded<string>()
  let stderr = ConcurrentQueue<string>()
  let isLc0 = config.Path.Contains("lc0", StringComparison.OrdinalIgnoreCase)
  let proc = new Process()
  // the last commands and answers, with their time, for a failed check
  let clock = Stopwatch.StartNew()
  let transcript = ConcurrentQueue<struct (int64 * string)>()
  let note (text: string) =
    transcript.Enqueue(struct (clock.ElapsedMilliseconds, text))
    while transcript.Count > 400 do transcript.TryDequeue() |> ignore

  /// Started in the engine's own folder with its arguments, as EngineBattle starts it.
  member _.Start() =
    try
      proc.StartInfo.FileName <- config.Path
      proc.StartInfo.Arguments <- EngineStartup.arguments config isLc0
      proc.StartInfo.WorkingDirectory <- IO.Path.GetDirectoryName config.Path
      proc.StartInfo.UseShellExecute <- false
      proc.StartInfo.RedirectStandardInput <- true
      proc.StartInfo.RedirectStandardOutput <- true
      proc.StartInfo.RedirectStandardError <- true
      proc.StartInfo.CreateNoWindow <- true
      proc.OutputDataReceived.Add(fun a ->
        if isNull a.Data then
          note "<< (output closed)"
          lines.Writer.TryComplete() |> ignore
        else
          note ("<< " + a.Data)
          lines.Writer.TryWrite a.Data |> ignore)
      proc.ErrorDataReceived.Add(fun a ->
        if not (String.IsNullOrEmpty a.Data) then
          stderr.Enqueue a.Data
          if stderr.Count > 40 then stderr.TryDequeue() |> ignore)
      if proc.Start() then
        proc.StandardInput.NewLine <- "\n"
        proc.StandardInput.AutoFlush <- true
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        true
      else false
    with _ -> false

  member _.Send(command: string) =
    note (">> " + command)
    try proc.StandardInput.WriteLine command with _ -> ()

  /// From the last `commands` sent on: each line with its time, a run of info lines as one.
  member _.Transcript(commands: int) = transcriptLines commands (transcript.ToArray())

  member _.Exited = try proc.HasExited with _ -> true
  member _.ExitCode = try (if proc.HasExited then string proc.ExitCode else "?") with _ -> "?"
  member _.Stderr = stderr.ToArray()
  member _.Kill() = try proc.Kill(true) with _ -> ()
  member _.WaitForExit(ms: int) = try proc.WaitForExit ms with _ -> true

  /// Lines until one matches, the time is up or the output closes: the lines, the match, the ms.
  member _.Until(matches: string -> bool, timeoutMs: int) =
    let sw = Stopwatch.StartNew()
    let read = ResizeArray<string>()
    let mutable found = None
    let mutable closed = false
    while found.IsNone && not closed && sw.ElapsedMilliseconds < int64 timeoutMs do
      use cts = new CancellationTokenSource(max 1 (timeoutMs - int sw.ElapsedMilliseconds))
      try
        let line = lines.Reader.ReadAsync(cts.Token).AsTask().GetAwaiter().GetResult()
        read.Add line
        if matches line then found <- Some line
      with _ -> if lines.Reader.Completion.IsCompleted then closed <- true
    List.ofSeq read, found, sw.ElapsedMilliseconds

  /// Whatever the engine wrote that nobody read.
  member _.Drain() =
    let mutable line = null
    while lines.Reader.TryRead(&line) do ()

  /// Ready for the next check: stop whatever runs, then readyok within the time.
  member this.Resync(timeoutMs: int) =
    this.Send "stop"
    this.Send "isready"
    let _, ok, _ = this.Until(isReadyOk, timeoutMs)
    this.Drain()
    ok.IsSome

/// A searched position and its info lines, for the info group.
type private Searched = { Name: string; Fen: string; Infos: string list }

type private Ctx =
  { mutable S: Session
    Config: EngineConfig
    Report: Report
    Options: Dictionary<string, UciOption.UciOption>
    /// Options the config sets (network, device...), with their values: left alone.
    Configured: Dictionary<string, string>
    Searched: ResizeArray<Searched>
    Limited: string
    StopDelayMs: int
    /// Why the engine can no longer be checked; the rest is skipped.
    mutable Stuck: string option
    mutable LastFailed: string }

/// A failure, or a skip once the engine is stuck (its failure was reported already).
let private failed (c: Ctx) name detail =
  match c.Stuck with
  | Some reason -> c.Report.Add Skip name reason
  | None ->
      c.LastFailed <- name
      c.Report.Add Fail name (if c.S.Exited then sprintf "%s - the engine exited (code %s)" detail c.S.ExitCode else detail)

/// Ready again after a failure, or marked stuck.
let private recover (c: Ctx) =
  if c.Stuck.IsNone then
    if c.S.Exited then c.Stuck <- Some (sprintf "the engine exited (code %s) at '%s'" c.S.ExitCode c.LastFailed)
    elif not (c.S.Resync 10000) then c.Stuck <- Some (sprintf "the engine stopped answering at '%s'" c.LastFailed)

let private position (fen: string) (moves: string list) =
  if moves.IsEmpty then sprintf "position fen %s" fen
  else sprintf "position fen %s moves %s" fen (String.Join(" ", moves))

/// One search: the info lines, the bestmove line, the ms from go to bestmove.
let private search (c: Ctx) (positionCommand: string) (go: string) (timeoutMs: int) =
  if c.Stuck.IsSome then [], None, 0L
  else
    c.S.Drain()
    c.S.Send positionCommand
    c.S.Send go
    let read, best, ms = c.S.Until(isBestMove, timeoutMs)
    read |> List.filter (fun l -> l.StartsWith("info", StringComparison.Ordinal)), best, ms

/// A search whose bestmove must be legal; its infos are kept for the info group.
let private checkedSearch (c: Ctx) name (fen: string) (moves: string list) (go: string) =
  match boardAfter fen moves with
  | None -> c.Report.Add Skip name "EngineBattle's board cannot play these moves"; None
  | Some board ->
      let infos, best, ms = search c (position fen moves) go 30000
      c.Searched.Add { Name = name; Fen = board.FEN(); Infos = infos }
      match best with
      | None ->
          failed c name "no bestmove within 30 s"
          recover c
          None
      | Some line ->
          let move = bestMoveOf line
          if isLegal board move then c.Report.Add Pass name (sprintf "%s in %d ms" move ms)
          else failed c name (sprintf "bestmove %s is not legal in %s" move (board.FEN()))
          Some (line, ms)

let private hasOption (c: Ctx) name = c.Options.ContainsKey name

// ── Groups ─────────────────────────────────────────────────────────────────────────────────────

/// uci, the config's options, isready, ucinewgame. False when the engine is not usable.
let private startup (c: Ctx) (config: EngineConfig) =
  header "startup"
  c.S.Send "uci"
  let read, uciok, ms = c.S.Until((fun l -> l.Trim() = "uciok"), 10000)
  match uciok with
  | None ->
      failed c "uci" (if c.S.Exited then sprintf "the engine exited (code %s)" c.S.ExitCode else "no uciok within 10 s")
      false
  | Some _ ->
      c.Report.Add Pass "uci" (sprintf "uciok in %d ms" ms)
      match read |> List.tryFind (fun l -> l.StartsWith("id name", StringComparison.Ordinal)) with
      | Some l -> c.Report.Add Pass "id name" (l.Substring(7).Trim())
      | None -> c.Report.Add Warn "id name" "missing"
      let hasAuthor = read |> List.exists (fun l -> l.StartsWith("id author", StringComparison.Ordinal))
      c.Report.Add (if hasAuthor then Pass else Warn) "id author" (if hasAuthor then "present" else "missing")
      let optionLines = read |> List.filter (fun l -> l.StartsWith("option ", StringComparison.Ordinal))
      for l in optionLines do UciOption.addOptionToMap c.Options l
      let missed = optionLines.Length - c.Options.Count
      c.Report.Add (if missed = 0 then Pass else Warn) "options listed"
        (if missed = 0 then sprintf "%d, all understood" c.Options.Count else sprintf "%d of %d not understood" missed optionLines.Length)

      let commands = EngineHelper.createInitialUCICommands config |> List.ofSeq
      for cmd in commands do
        UciOption.parseSetOptionCommand cmd |> Option.iter (fun (name, value) -> c.Configured.[name] <- string value)
        c.S.Send cmd
      c.S.Send "isready"
      // a network load (a TensorRT build) can take minutes
      let _, ready, ms = c.S.Until(isReadyOk, 600000)
      match ready with
      | None ->
          failed c "config + isready" (if c.S.Exited then sprintf "the engine exited (code %s)" c.S.ExitCode else "no readyok within 10 min")
          false
      | Some _ ->
          c.Report.Add Pass "config + isready" (sprintf "%d options from the def, readyok in %d ms" commands.Length ms)
          c.S.Send "isready"
          let _, ready, ms = c.S.Until(isReadyOk, 10000)
          c.Report.Add (if ready.IsSome then byTime ms 1000L 10000L else Fail) "isready"
            (if ready.IsSome then sprintf "readyok in %d ms" ms else "no readyok within 10 s")
          // a late readyok must not answer the next isready
          if ready.IsNone then c.S.Resync 10000 |> ignore
          c.S.Send "ucinewgame"
          c.S.Send "isready"
          let _, ready, ms = c.S.Until(isReadyOk, 30000)
          c.Report.Add (if ready.IsSome then Pass else Fail) "ucinewgame + isready"
            (if ready.IsSome then sprintf "readyok in %d ms" ms else "no readyok within 30 s")
          ready.IsSome

/// Every option the config leaves alone, set to its own default: no crash, still ready.
let private optionsGroup (c: Ctx) =
  header "options"
  let toDefault =
    [ for KeyValue (_, o) in c.Options do
        if not (c.Configured.ContainsKey o.Name) then
          match o.OptionType with
          | UciOption.Check b -> yield o.Name, (if b then "true" else "false")
          | UciOption.Spin (_, _, d) -> yield o.Name, string d
          | UciOption.Combo (_, d) when d <> "" -> yield o.Name, d
          | UciOption.String d when d <> "" && d <> "<empty>" -> yield o.Name, d
          | _ -> () ]
  for name, value in toDefault do c.S.Send(sprintf "setoption name %s value %s" name value)
  c.S.Send "isready"
  let _, ready, ms = c.S.Until(isReadyOk, 60000)
  match ready with
  | Some _ -> c.Report.Add Pass "defaults accepted" (sprintf "%d options set to their defaults, readyok in %d ms" toDefault.Length ms)
  | None ->
      failed c "defaults accepted"
        (if c.S.Exited then sprintf "the engine exited (code %s) after %d options" c.S.ExitCode toDefault.Length
         else "no readyok within 60 s")

/// Positions in every form; each bestmove must be legal.
let private positionsGroup (c: Ctx) =
  header "positions"
  let ruy = "r1bqkbnr/1ppp1ppp/p1n5/4p3/B3P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 1 4"
  checkedSearch c "startpos" startFen [] c.Limited |> ignore
  // the startpos keyword itself, not a FEN
  let infos, best, ms = search c "position startpos moves e2e4 e7e5 g1f3" c.Limited 30000
  (match best, boardAfter startFen [ "e2e4"; "e7e5"; "g1f3" ] with
   | Some line, Some board when isLegal board (bestMoveOf line) ->
       c.Searched.Add { Name = "startpos moves"; Fen = board.FEN(); Infos = infos }
       c.Report.Add Pass "startpos moves" (sprintf "%s in %d ms" (bestMoveOf line) ms)
   | Some line, Some board -> failed c "startpos moves" (sprintf "bestmove %s is not legal in %s" (bestMoveOf line) (board.FEN()))
   | _ -> failed c "startpos moves" "no bestmove within 30 s"; recover c)
  checkedSearch c "fen" ruy [] c.Limited |> ignore
  // the def's own UCI_Chess960 stays: true writes castling as king takes rook, false is standard
  let defChess960 =
    match c.Configured.TryGetValue "UCI_Chess960" with
    | true, v -> Some (v.Trim().Equals("true", StringComparison.OrdinalIgnoreCase))
    | _ -> None
  if defChess960 = Some true then c.Report.Add Skip "castling in moves" "the def turns UCI_Chess960 on (castling is king takes rook)"
  else checkedSearch c "castling in moves" startFen [ "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1c4"; "g8f6"; "e1g1" ] c.Limited |> ignore
  checkedSearch c "en passant in moves" startFen [ "e2e4"; "a7a6"; "e4e5"; "d7d5" ] c.Limited |> ignore
  checkedSearch c "en passant in fen" "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3" [] c.Limited |> ignore
  checkedSearch c "promotion in moves" "8/P6k/8/8/8/8/6K1/8 w - - 0 1" [ "a7a8q" ] c.Limited |> ignore
  checkedSearch c "underpromotion in moves" "8/P6k/8/8/8/8/6K1/8 w - - 0 1" [ "a7a8n" ] c.Limited |> ignore
  checkedSearch c "black to move, fen" "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" [] c.Limited |> ignore
  if defChess960 = Some false then c.Report.Add Skip "Chess960" "the def turns UCI_Chess960 off; left as it is"
  elif defChess960 = Some true then c.Report.Add Skip "Chess960" "the def turns UCI_Chess960 on; left as it is"
  elif hasOption c "UCI_Chess960" then
    c.S.Send "setoption name UCI_Chess960 value true"
    checkedSearch c "Chess960 start" "nrbkqbrn/pppppppp/8/8/8/8/PPPPPPPP/NRBKQBRN w GBgb - 0 1" [] c.Limited |> ignore
    // king takes rook: castling as Chess960 writes it
    checkedSearch c "Chess960 castling" "1r2k2r/pppppppp/8/8/8/8/PPPPPPPP/1R2K2R w HBhb - 0 1" [ "e1h1" ] c.Limited |> ignore
    c.S.Send "setoption name UCI_Chess960 value false"
    recover c
  else c.Report.Add Skip "Chess960" "no UCI_Chess960 option"

/// depth, nodes, movetime and the clock: each ends the search, the time ones in time.
let private limitsGroup (c: Ctx) =
  header "limits"
  let ruy = "r1bqkbnr/1ppp1ppp/p1n5/4p3/B3P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 1 4"
  let pos = position ruy []
  let last key (infos: string list) = infos |> List.rev |> List.tryPick (field key) |> Option.bind (fun v -> match Int64.TryParse v with | true, n -> Some n | _ -> None)
  // a position not searched before, so no hash answers for it
  c.S.Send "ucinewgame"
  let infos, best, ms = search c (position "r2q1rk1/pp2bppp/2n1bn2/3p4/3P4/2NBBN2/PP3PPP/R2Q1RK1 w - - 4 11" []) "go depth 5" 60000
  (match best with
   | None -> failed c "go depth 5" "no bestmove within 60 s"; recover c
   | Some _ ->
       match last "depth" infos with
       | Some d when d > 5L -> c.Report.Add Warn "go depth 5" (sprintf "ended in %d ms, but reported depth %d" ms d)
       | _ -> c.Report.Add Pass "go depth 5" (sprintf "ended in %d ms" ms))
  let infos, best, ms = search c pos "go nodes 5000" 60000
  (match best with
   | None -> failed c "go nodes 5000" "no bestmove within 60 s"; recover c
   | Some _ ->
       match last "nodes" infos with
       | Some n when n > 15000L -> c.Report.Add Warn "go nodes 5000" (sprintf "searched %d nodes" n)
       | Some n -> c.Report.Add Pass "go nodes 5000" (sprintf "%d nodes, %d ms" n ms)
       | None -> c.Report.Add Pass "go nodes 5000" (sprintf "ended in %d ms (no nodes reported)" ms))
  let _, best, ms = search c pos "go movetime 500" 10000
  (match best with
   | None -> failed c "go movetime 500" "no bestmove within 10 s"; recover c
   | Some _ when ms < 100L ->
       // far too early: usually a move overhead larger than the movetime
       c.Report.Add Warn "go movetime 500" (sprintf "bestmove after only %d ms - a move overhead of 500 ms or more?" ms)
   | Some _ -> c.Report.Add (byTime ms 700L 1500L) "go movetime 500" (sprintf "bestmove after %d ms" ms))
  let _, best, ms = search c pos "go wtime 2000 btime 2000 winc 0 binc 0" 10000
  (match best with
   | None -> failed c "2 s on the clock" "no bestmove within 10 s"; recover c
   | Some _ ->
       let v = if ms >= 2000L then Fail elif ms > 1000L then Warn else Pass
       c.Report.Add v "2 s on the clock" (sprintf "bestmove after %d ms%s" ms (if v = Fail then " - lost on time" else "")))
  let _, best, ms = search c pos "go wtime 3000 btime 3000 winc 100 binc 100 movestogo 1" 10000
  (match best with
   | None -> failed c "movestogo 1" "no bestmove within 10 s"; recover c
   | Some _ -> c.Report.Add (if ms < 3000L then Pass else Fail) "movestogo 1" (sprintf "bestmove after %d ms of 3000" ms))

/// The info lines of every search so far: scores, and PVs legal from their position.
let private infoGroup (c: Ctx) =
  header "info"
  let all = c.Searched |> Seq.collect (fun s -> s.Infos) |> List.ofSeq
  if all.IsEmpty then c.Report.Add Skip "info lines" "no search ran"
  else
    c.Report.Add Pass "info lines" (sprintf "%d lines from %d searches" all.Length c.Searched.Count)
    let scored = all |> List.exists (fun l -> l.Contains " score cp " || l.Contains " score mate ")
    c.Report.Add (if scored then Pass else Warn) "score" (if scored then "cp or mate reported" else "no score cp/mate seen")
    let withPv = c.Searched |> Seq.filter (fun s -> s.Infos |> List.exists (fun l -> l.Contains " pv "))
    let bad =
      [ for s in withPv do
          let pv = s.Infos |> List.rev |> List.find (fun l -> l.Contains " pv ") |> pvOf
          let legal = legalPrefix s.Fen pv
          if legal < pv.Length then yield sprintf "%s: move %d (%s) of the last pv" s.Name (legal + 1) pv.[legal] ]
    match bad with
    | [] -> c.Report.Add Pass "pv legal" (sprintf "the last pv of %d searches" (Seq.length withPv))
    | b -> failed c "pv legal" (sprintf "illegal - %s" (String.Join("; ", b)))

/// go infinite until stop, isready during a search, stop at once.
let private stopGroup (c: Ctx) =
  header "stop"
  if c.Stuck.IsSome then c.Report.Add Skip "stop" c.Stuck.Value else
  let pos = position startFen [ "d2d4"; "g8f6"; "c2c4" ]
  c.S.Drain()
  c.S.Send pos
  c.S.Send "go infinite"
  let _, early, _ = c.S.Until(isBestMove, 1000)
  if early.IsSome then
    failed c "go infinite" "bestmove before stop"
    c.Report.Add Skip "isready while searching" "the search had ended"
    c.Report.Add Skip "stop" "the search had ended"
    recover c
  else
    c.Report.Add Pass "go infinite" "still searching after 1 s"
    c.S.Send "isready"
    let _, ready, ms = c.S.Until(isReadyOk, 5000)
    c.Report.Add (if ready.IsSome then byTime ms 500L 5000L else Fail) "isready while searching"
      (if ready.IsSome then sprintf "readyok in %d ms" ms else "no readyok within 5 s (UCI asks for it at once)")
    c.S.Send "stop"
    let _, best, ms = c.S.Until(isBestMove, 10000)
    match best with
    | None -> failed c "stop" "no bestmove within 10 s"; recover c
    | Some line ->
        let legal = boardAfter startFen [ "d2d4"; "g8f6"; "c2c4" ] |> Option.map (fun b -> isLegal b (bestMoveOf line)) |> Option.defaultValue true
        if not legal then failed c "stop" (sprintf "bestmove %s is not legal" (bestMoveOf line))
        else c.Report.Add (byTime ms 1000L 5000L) "stop" (sprintf "bestmove %d ms after stop" ms)
  // stop sent right behind go: the engine must still answer; --stop-delay puts a pause between
  // them, and the check's name says so
  let name = if c.StopDelayMs > 0 then sprintf "stop %d ms after go" c.StopDelayMs else "stop right after go"
  let rounds = 20
  let mutable answered = 0
  let mutable worst = 0L
  let mutable i = 0
  while i < rounds do
    c.S.Drain()
    c.S.Send pos
    c.S.Send "go infinite"
    if c.StopDelayMs > 0 then Thread.Sleep c.StopDelayMs
    c.S.Send "stop"
    let _, best, ms = c.S.Until(isBestMove, 5000)
    match best with
    | Some _ ->
        answered <- answered + 1
        worst <- max worst ms
        i <- i + 1
    | None -> i <- rounds
  if answered = rounds then c.Report.Add Pass name (sprintf "%d of %d answered, slowest %d ms" answered rounds worst)
  else
    failed c name (sprintf "round %d: no bestmove within 5 s%s" (answered + 1) (if c.S.Exited then sprintf " - the engine exited (code %s)" c.S.ExitCode else ""))
    recover c

/// go ponder, then ponderhit or stop.
let private ponderGroup (c: Ctx) =
  header "ponder"
  if c.Stuck.IsSome then c.Report.Add Skip "ponder" c.Stuck.Value
  elif not (hasOption c "Ponder") then c.Report.Add Skip "ponder" "no Ponder option"
  else
    c.S.Send "setoption name Ponder value true"
    let first = [ "e2e4" ]
    let _, best, _ = search c (position startFen first) "go nodes 5000" 30000
    match best, best |> Option.bind (field "ponder") with
    | None, _ -> failed c "ponder move" "no bestmove within 30 s"; recover c
    | Some _, None -> c.Report.Add Warn "ponder move" "no ponder move with the bestmove"
    | Some _, Some ponderMove ->
        let moves = first @ [ bestMoveOf best.Value; ponderMove ]
        match boardAfter startFen moves with
        | None -> failed c "ponder move" (sprintf "%s is not legal after %s" ponderMove (bestMoveOf best.Value))
        | Some board ->
            c.Report.Add Pass "ponder move" ponderMove
            let go = "go ponder wtime 3000 btime 3000 winc 0 binc 0"
            c.S.Drain()
            c.S.Send(position startFen moves)
            c.S.Send go
            let _, early, _ = c.S.Until(isBestMove, 300)
            if early.IsSome then failed c "go ponder" "bestmove before ponderhit"
            else
              c.S.Send "ponderhit"
              let _, best, ms = c.S.Until(isBestMove, 5000)
              match best with
              | None -> failed c "ponderhit" "no bestmove within 5 s of 3 s on the clock"; recover c
              | Some line when not (isLegal board (bestMoveOf line)) -> failed c "ponderhit" (sprintf "bestmove %s is not legal" (bestMoveOf line))
              | Some _ -> c.Report.Add (if ms < 3000L then Pass else Fail) "ponderhit" (sprintf "bestmove %d ms after ponderhit" ms)
            c.S.Drain()
            c.S.Send(position startFen moves)
            c.S.Send go
            Thread.Sleep 300
            c.S.Send "stop"
            let _, best, ms = c.S.Until(isBestMove, 10000)
            match best with
            | None -> failed c "ponder, then stop" "no bestmove within 10 s"; recover c
            | Some _ -> c.Report.Add (byTime ms 1000L 5000L) "ponder, then stop" (sprintf "bestmove %d ms after stop" ms)
    c.S.Send "setoption name Ponder value false"
    recover c

/// Positions without a move, searchmoves, MultiPV.
let private edgeGroup (c: Ctx) =
  header "edge"
  let noMove name fen =
    let _, best, _ = search c (position fen []) c.Limited 10000
    match best with
    | None -> failed c name "no answer within 10 s"; recover c
    | Some line ->
        match bestMoveOf line with
        | "0000" | "(none)" | "none" as m -> c.Report.Add Pass name (sprintf "bestmove %s" m)
        | "" -> c.Report.Add Warn name "bestmove without a move"
        | m -> c.Report.Add Warn name (sprintf "bestmove %s, but there is no legal move (0000 or (none) expected)" m)
  if hasOption c "MultiPV" then
    c.S.Send "setoption name MultiPV value 3"
    let infos, best, _ = search c (position startFen []) "go nodes 20000" 60000
    let lines = infos |> List.choose (field "multipv") |> List.distinct
    if best.IsNone then
      failed c "MultiPV 3" "no bestmove within 60 s"
      recover c
    else
      c.Report.Add (if List.contains "3" lines then Pass else Warn) "MultiPV 3" (sprintf "multipv lines seen: %s" (String.Join(",", lines |> List.sort)))
    c.S.Send "setoption name MultiPV value 1"
    recover c
  else c.Report.Add Skip "MultiPV" "no MultiPV option"
  // last: some engines crash on searchmoves or on a position without moves
  let _, best, _ = search c (position startFen []) (c.Limited + " searchmoves a2a3 h2h3") 30000
  (match best with
   | None -> failed c "searchmoves" "no bestmove within 30 s"; recover c
   | Some line ->
       let m = bestMoveOf line
       c.Report.Add (if m = "a2a3" || m = "h2h3" then Pass else Warn) "searchmoves" (sprintf "bestmove %s, asked for a2a3 or h2h3" m))
  noMove "checkmated" "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
  noMove "stalemated" "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"

let private quitGroup (c: Ctx) =
  header "quit"
  if c.Stuck.IsSome then c.Report.Add Skip "quit" c.Stuck.Value else
  c.S.Send "quit"
  let sw = Stopwatch.StartNew()
  let gone = c.S.WaitForExit 5000
  c.Report.Add (if gone then byTime sw.ElapsedMilliseconds 2000L 5000L else Fail) "quit"
    (if gone then sprintf "exited in %d ms" sw.ElapsedMilliseconds else "still running 5 s after quit")

// ── The analysis group (the engine behind EngineBattle's analysis pages) ────────────────────────

/// (position command, FEN) after each prefix of the moves, start included.
let private prefixes (moves: string list) =
  let board = Chess.Board()
  board.LoadFen startFen
  [ yield "position fen " + startFen, board.FEN()
    for i, m in List.indexed moves do
      board.PlayUciMove m
      let cmd = sprintf "position fen %s moves %s" startFen (String.Join(" ", moves |> List.take (i + 1)))
      yield cmd, board.FEN() ]

let private analysisGroup (p: Params) (report: Report) =
  header "analysis (as EngineBattle's analysis pages drive it)"
  let updates = ConcurrentQueue<EngineUpdate>()
  let failed = ref ""
  let callback (u: EngineUpdate) =
    updates.Enqueue u
    match u with
    | EngineFailed (_, reason) -> failed.Value <- reason
    | _ -> ()
  if (boardAfter startFen p.Moves).IsNone then report.Add Fail "moves" "--moves is not a legal game from the start position"
  else
  match (try Ok (EngineHelper.createAnalysisEngine ((fun u -> callback u.Update), p.Config, NullLogger.Instance, false)) with ex -> Error ex.Message) with
  | Error message -> report.Add Fail "start" message
  | Ok engine ->
      // what led to a failure: the tail of the I/O log the analysis engine writes, the path once
      let logShown = ref false
      report.OnFail <- fun () ->
        if engine.IoLogPath <> "" then
          let lines = transcriptLines 6 (ioLogEntries engine.IoLogPath)
          if not lines.IsEmpty then
            printfn "        what was sent and answered before it:"
            for line in lines do printfn "        %s" (cut line)
          if not logShown.Value then
            printfn "        full log: %s" engine.IoLogPath
            logShown.Value <- true
      let positions = prefixes p.Moves
      let limited = match p.MoveTimeMs with Some ms -> sprintf "go movetime %d" ms | None -> sprintf "go nodes %d" p.Nodes
      let await (a: Async<AnalysisOutcome>) = Async.RunSynchronously(a, 120000)
      let take () =
        let items = updates.ToArray()
        updates.Clear()
        items
      let bestMovesOf items = items |> Array.choose (function BestMove b -> Some b | _ -> None)
      let stoppedOf items = items |> Array.filter (function SearchStopped _ -> true | _ -> false) |> Array.length
      // once the engine has failed, the checks after it are skipped, not run against a dead engine
      let stuckAt : string option ref = ref None
      let skipped name = report.Add Skip name (sprintf "the engine stopped answering at '%s'" stuckAt.Value.Value)
      let check name ok detail =
        match stuckAt.Value with
        | Some _ -> skipped name
        | None ->
            report.Add (if ok then Pass else Fail) name detail
            if failed.Value <> "" then stuckAt.Value <- Some name
      try
        try
          take () |> ignore

          // every position in turn, awaited (as Game Review)
          let sw = Stopwatch.StartNew()
          let mutable wrong = 0
          let mutable incomplete = 0
          for cmd, fen in positions do
            match await (engine.Search(cmd, limited)) with
            | Completed (Some bm) when bm.FEN = fen -> ()
            | Completed (Some _) -> wrong <- wrong + 1
            | _ -> incomplete <- incomplete + 1
          let items = take ()
          check "review" (wrong = 0 && incomplete = 0)
            (sprintf "%d positions, %d bestmoves, %d for another position, %d without a move, %.1f s"
               positions.Length (bestMovesOf items).Length wrong incomplete sw.Elapsed.TotalSeconds)

          // fast moves through the game, go infinite each time; only the last one counts
          for round in 1 .. p.Rounds do
            if failed.Value = "" then
              let sw = Stopwatch.StartNew()
              for cmd, _ in positions do
                engine.Analyse(cmd, "go infinite") |> ignore
                if p.DelayMs > 0 then Thread.Sleep p.DelayMs
              let lastCmd, lastFen = List.last positions
              let outcome = await (engine.Search(lastCmd, limited))
              let items = take ()
              let bms = bestMovesOf items
              let ok =
                match outcome with
                | Completed (Some bm) -> bm.FEN = lastFen && bms.Length = 1
                | _ -> false
              check "navigate" ok
                (sprintf "round %d: %d requests, %d stopped, %d bestmoves (want 1, for the last position), %.1f s"
                   round (positions.Length + 1) (stoppedOf items) bms.Length sw.Elapsed.TotalSeconds)

          // MultiPV changed during an infinite search reruns it
          let isWinboard = WinboardIntegration.isWinboardEngine p.Config
          let cmd, fen = positions.[positions.Length / 2]
          if stuckAt.Value.IsSome then skipped "options"
          elif not (engine.GetUCICommands().ContainsKey "MultiPV") then
            report.Add Skip "options" "the engine has no MultiPV option"
          else
            engine.Analyse(cmd, "go infinite") |> ignore
            Thread.Sleep 500
            engine.SetOption(EngineOption.Create "MultiPV" "3")
            Thread.Sleep 1000
            let midItems = take ()
            let sawThree = midItems |> Array.exists (function Status s -> s.MultiPV = 3 | _ -> false)
            engine.SetOption(EngineOption.Create "MultiPV" "1")
            let outcome = await (engine.Search(cmd, limited))
            let ok = sawThree && (match outcome with Completed (Some bm) -> bm.FEN = fen | _ -> false)
            check "options" ok (sprintf "MultiPV 3 seen during the search: %b, then MultiPV 1 and a search: %s" sawThree (match outcome with Completed (Some bm) -> bm.Move | o -> sprintf "%A" o))
            take () |> ignore

          // an infinite search stopped still answers with its bestmove
          if stuckAt.Value.IsSome then skipped "stop"
          else
            let running = engine.Search(fst positions.[0], "go infinite")
            Thread.Sleep 500
            engine.Stop()
            // Winboard's stop in analysis is exit, which prints no move
            let ok =
              match await running with
              | Completed (Some _) -> true
              | Superseded -> isWinboard
              | _ -> false
            check "stop" ok (if isWinboard then "analyze, then exit: the search ends without a move" else "go infinite, then stop: the bestmove arrives")

          if failed.Value <> "" then
            // a failure no check caught is one of its own; either way, why the engine went
            if stuckAt.Value.IsNone then check "engine" false ("EngineFailed: " + failed.Value)
            printfn "\n  the engine failed: %s; exit code %s" failed.Value (match engine.LastExitCode with Some c -> string c | None -> "unknown")
            for line in engine.ErrorOutput |> Seq.truncate 40 do printfn "  stderr: %s" line
        with ex -> report.Add Fail "error" ex.Message
      finally
        engine.Quit()

/// A new process for an engine that stopped answering, through the start-up handshake (not checked
/// again): the groups after a failure still run.
let private restart (c: Ctx) =
  c.S.Kill()
  let s = Session(c.Config)
  c.S <- s
  c.Stuck <- None
  c.LastFailed <- ""
  s.Start()
  && (s.Send "uci"
      let _, uciok, _ = s.Until((fun l -> l.Trim() = "uciok"), 10000)
      uciok.IsSome)
  && (for cmd in EngineHelper.createInitialUCICommands c.Config do s.Send cmd
      s.Send "ucinewgame"
      s.Send "isready"
      // a network load (a TensorRT build) can take minutes
      let _, ready, _ = s.Until(isReadyOk, 600000)
      ready.IsSome)

// ── Run ────────────────────────────────────────────────────────────────────────────────────────

let run (p: Params) : int =
  let unknown = p.Only |> List.filter (fun g -> not (List.contains g groups))
  if not unknown.IsEmpty then
    eprintfn "Unknown group(s) for --only: %s (groups: %s)" (String.Join(", ", unknown)) (String.Join(", ", groups))
    2
  else
  let report = Report()
  let want g = p.Only.IsEmpty || List.contains g p.Only
  // restarts of an engine that stopped answering, each with why; a few, then the rest is skipped
  let restarts = ResizeArray<string>()
  let maxRestarts = 3
  let isWinboard = WinboardIntegration.isWinboardEngine p.Config
  printfn "Checking %s (%s)" p.Config.Name p.Config.Path
  if isWinboard then
    printfn "A Winboard engine: the UCI groups do not apply%s." (if want "analysis" then "; only the analysis group runs" else "")
  else if groups |> List.exists (fun g -> g <> "analysis" && want g) then
    let c =
      { S = Session(p.Config); Config = p.Config; Report = report
        Options = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
        Configured = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        Searched = ResizeArray()
        Limited = match p.MoveTimeMs with Some ms -> sprintf "go movetime %d" ms | None -> sprintf "go nodes %d" p.Nodes
        StopDelayMs = p.StopDelayMs
        Stuck = None; LastFailed = "" }
    // the session in use: a restart replaces it
    report.OnFail <- fun () ->
      printfn "        what was sent and answered before it:"
      for line in c.S.Transcript 6 do printfn "        %s" (cut line)
    if not (c.S.Start()) then report.Add Fail "start" (sprintf "%s could not be started" p.Config.Path)
    else
      try
        try
          if startup c p.Config then
            // a group runs on an engine that answers: one that stopped is restarted (a few times),
            // so a failure early on does not hide the groups after it
            let usable = ref true
            let step name (group: Ctx -> unit) =
              report.Group <- name
              if want name then
                if usable.Value && c.Stuck.IsNone && (c.S.Exited || not (c.S.Resync 10000)) then
                  let reason = sprintf "the engine stopped answering before '%s'%s" name (if c.S.Exited then sprintf " (exited, code %s)" c.S.ExitCode else "")
                  // a FAIL first: once Stuck is set, failed records a SKIP
                  failed c "engine" reason
                  c.Stuck <- Some reason
                match c.Stuck with
                | Some reason when usable.Value && restarts.Count < maxRestarts ->
                    printfn "\n  %s - the engine is restarted for '%s'" reason name
                    for line in c.S.Stderr |> Seq.truncate 20 do printfn "  stderr: %s" line
                    restarts.Add(sprintf "before '%s': %s" name reason)
                    if restart c then group c
                    else
                      usable.Value <- false
                      c.Stuck <- Some "the engine did not start again"
                      report.Add Skip "(the whole group)" c.Stuck.Value
                | Some reason ->
                    if usable.Value then printfn "\n  %s - the remaining groups are skipped" reason
                    usable.Value <- false
                    // recorded, so the summary says which groups never ran
                    report.Add Skip "(the whole group)" reason
                | None -> group c
            step "options" optionsGroup
            step "positions" positionsGroup
            step "limits" limitsGroup
            if want "info" then
              report.Group <- "info"
              infoGroup c
            step "stop" stopGroup
            step "ponder" ponderGroup
            step "edge" edgeGroup
            step "quit" quitGroup
            if c.Stuck.IsSome then
              for line in c.S.Stderr |> Seq.truncate 20 do printfn "  stderr: %s" line
        with ex -> report.Add Fail "error" ex.Message
      finally
        if not c.S.Exited then c.S.Kill()
  if want "analysis" then
    report.Group <- "analysis"
    analysisGroup p report
  let fails, warns, passes, skips = report.Count Fail, report.Count Warn, report.Count Pass, report.Count Skip
  // what went wrong, together: a failure in an early group has scrolled away by now
  let entries = report.Entries
  let notable = entries |> Array.filter (fun (v, _, _, _) -> v = Fail || v = Warn)
  // per reason, what it skipped: a group for a group not run, group/check for a check
  let skipReasons =
    entries
    |> Array.choose (fun (v, g, n, d) -> if v = Skip then Some (d, (if n = "(the whole group)" then g else g + "/" + n)) else None)
    |> Array.groupBy fst
    |> Array.map (fun (reason, items) -> reason, items |> Array.map snd)
  if notable.Length > 0 || skipReasons.Length > 0 || restarts.Count > 0 then
    printfn "\nSummary"
    for verdict, group, name, detail in notable do
      let old = Console.ForegroundColor
      Console.Write "  "
      Console.ForegroundColor <- (if verdict = Fail then ConsoleColor.Red else ConsoleColor.Yellow)
      Console.Write(if verdict = Fail then "FAIL" else "WARN")
      Console.ForegroundColor <- old
      printfn "  %-10s %-26s %s" group name (cut detail)
    for why in restarts do printfn "  RESTARTED %s" (cut why)
    for reason, what in skipReasons do
      printfn "  SKIP  %s: %s" (String.Join(", ", what)) (cut reason)
  printfn "\n%s: %d passed, %d warnings, %d failed, %d skipped" p.Config.Name passes warns fails skips
  if passes + warns + fails = 0 then
    printfn "No check ran."
    1
  elif fails = 0 then 0
  else 1
