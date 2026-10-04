namespace ChessLibrary.Match

open System
open System.Collections.Generic
open System.Globalization
open System.IO

/// The reference's command line (cli/cli.hpp, cli/cli.cpp, cli/sanitize.cpp at 60d7a7a), ported so the
/// same arguments give the same settings and the same error texts: its tokeniser, option table,
/// defaults, `-each`, `-quick`, and the sanitising that runs after parsing.
/// The reference is MIT-licensed; its notice is in THIRD-PARTY-NOTICE.txt next to this file. The
/// -help text is match-help.txt.
///
/// The parser does no I/O of its own: what it needs from the machine comes in through `Env`, and
/// what the reference would print while parsing comes back, in order, in `Messages`. `-config file=`
/// hands the file to `Env.LoadConfig`. Not ported: the Windows cap of 63 threads before Windows 11
/// and the POSIX file-descriptor limit, both limits of the reference's own process model.
module MatchArgs =

  // ---- settings, named and defaulted as the reference's (types/*.hpp) ----

  type OutputType = Default | Cutechess
  type NotationType = San | Lan | Uci
  type FormatType = Epd | Pgn | NoFormat
  type OrderType = Random | Sequential
  type VariantType = Standard | Frc
  type TournamentType = RoundRobin | Gauntlet
  type LogLevel = All | Trace | Info | Warn | Err | Fatal
  type TbResultType = WinLoss | DrawOnly | Both

  /// TimeControl::Limits, all in milliseconds (moves = moves to go per period, 0 = whole game).
  type TcLimits =
    { Increment: int64; FixedTime: int64; Time: int64; Moves: int64; TimeMargin: int64 }
    static member Zero = { Increment = 0L; FixedTime = 0L; Time = 0L; Moves = 0L; TimeMargin = 0L }

  type EngineConfig =
    { Name: string
      Dir: string
      Cmd: string
      Args: string
      Restart: bool
      /// `ponder` (cutechess-cli's engine option): ponder on the opponent's time
      Ponder: bool
      /// `option.X=v` pairs in command-line order; a repeated X keeps both, the last one wins in the engine
      Options: (string * string) list
      Tc: TcLimits
      Nodes: int64
      Plies: int64
      Variant: VariantType }
    static member Empty =
      { Name = ""; Dir = ""; Cmd = ""; Args = ""; Restart = false; Ponder = false; Options = []
        Tc = TcLimits.Zero; Nodes = 0L; Plies = 0L; Variant = Standard }
    /// getEnginePath: dir/cmd, or cmd.
    member e.EnginePath = if e.Dir = "" then e.Cmd else Path.Combine(e.Dir, e.Cmd)

  type Opening = { File: string; Format: FormatType; Order: OrderType; Plies: int; Start: int }
  type PgnOut =
    { AdditionalLinesRgx: string list; EventName: string; Site: string; File: string; Notation: NotationType
      AppendFile: bool; TrackNodes: bool; TrackSeldepth: bool; TrackNps: bool; TrackHashfull: bool
      TrackTbhits: bool; TrackTimeleft: bool; TrackLatency: bool; TrackPv: bool; Min: bool; Crc: bool }
  type EpdOut = { File: string; AppendFile: bool }
  type SprtConfig = { Enabled: bool; Alpha: float; Beta: float; Elo0: float; Elo1: float; Model: string }
  type DrawAdjudication = { MoveNumber: int; MoveCount: int; Score: int; Enabled: bool }
  type ResignAdjudication = { MoveCount: int; Score: int; TwoSided: bool; Enabled: bool }
  type MaxMovesAdjudication = { MoveCount: int; Enabled: bool }
  type TbAdjudication = { SyzygyDirs: string; MaxPieces: int; ResultType: TbResultType; Ignore50MoveRule: bool; Enabled: bool }
  type LogConfig = { File: string; Level: LogLevel; AppendFile: bool; Compress: bool; Realtime: bool; EngineComs: bool }

  type Tournament =
    { Opening: Opening
      Pgn: PgnOut
      Epd: EpdOut
      Sprt: SprtConfig
      ConfigName: string
      /// MoveNumber is uint32 in the reference, where a negative value wraps
      Draw: DrawAdjudication
      Resign: ResignAdjudication
      MaxMoves: MaxMovesAdjudication
      TbAdjudication: TbAdjudication
      Variant: VariantType
      Type: TournamentType
      GauntletSeeds: int
      Output: OutputType
      AutoSaveInterval: int
      RatingInterval: int
      /// size_t in the reference: a negative value compares as huge (see sanitizeTournament)
      Games: int
      Rounds: int
      ReportPenta: bool
      Seed: uint64
      ScoreInterval: int
      Concurrency: int
      /// ms after each game; uint32 in the reference, where a negative value wraps
      Wait: int
      ForceConcurrency: bool
      NoSwap: bool
      Reverse: bool
      Recover: bool
      Affinity: bool
      CheckMatePvs: bool
      ShowLatency: bool
      TestEnv: bool
      Strict: bool
      StartupMs: uint64
      UciNewGameMs: uint64
      PingMs: uint64
      AffinityCpus: int list
      Log: LogConfig }

  let defaultTournament (seed: uint64) =
    { Opening = { File = ""; Format = NoFormat; Order = Sequential; Plies = -1; Start = 1 }
      Pgn =
        { AdditionalLinesRgx = []; EventName = "EngineBattle Match"; Site = "?"; File = ""; Notation = San
          AppendFile = true; TrackNodes = false; TrackSeldepth = false; TrackNps = false; TrackHashfull = false
          TrackTbhits = false; TrackTimeleft = false; TrackLatency = false; TrackPv = false; Min = false; Crc = false }
      Epd = { File = ""; AppendFile = true }
      Sprt = { Enabled = false; Alpha = 0.0; Beta = 0.0; Elo0 = 0.0; Elo1 = 0.0; Model = "normalized" }
      ConfigName = "config.json"
      Draw = { MoveNumber = 0; MoveCount = 1; Score = 0; Enabled = false }
      Resign = { MoveCount = 1; Score = 0; TwoSided = false; Enabled = false }
      MaxMoves = { MoveCount = 1; Enabled = false }
      TbAdjudication = { SyzygyDirs = ""; MaxPieces = 0; ResultType = Both; Ignore50MoveRule = false; Enabled = false }
      Variant = Standard
      Type = RoundRobin
      GauntletSeeds = 1
      Output = Default
      AutoSaveInterval = 20
      RatingInterval = 10
      Games = 2
      Rounds = 2
      ReportPenta = true
      Seed = seed
      ScoreInterval = 1
      Concurrency = 1
      Wait = 0
      ForceConcurrency = false
      NoSwap = false
      Reverse = false
      Recover = false
      Affinity = false
      CheckMatePvs = false
      ShowLatency = false
      TestEnv = false
      Strict = false
      StartupMs = 10000UL
      UciNewGameMs = 60000UL
      PingMs = 60000UL
      AffinityCpus = []
      Log = { File = ""; Level = Warn; AppendFile = true; Compress = false; Realtime = true; EngineComs = false } }

  /// A resume file's content (§1.6): what `-config file=` replaces the command line's settings with.
  type LoadedConfig = { Tournament: Tournament; Engines: EngineConfig list; Stats: (string * MatchStats.Stats) list }

  /// What the parser needs from the machine.
  type Env =
    { HardwareThreads: int
      IsWindows: bool
      /// std::filesystem::exists (a file or a directory)
      PathExists: string -> bool
      /// std::filesystem::is_regular_file
      IsFile: string -> bool
      Now: unit -> DateTime
      RandomSeed: unit -> uint64
      LoadConfig: string -> LoadedConfig
      /// the line -version prints: EngineBattle and its build (BuildInfo)
      Version: string }

  let defaultEnv () =
    { HardwareThreads = Environment.ProcessorCount
      IsWindows = OperatingSystem.IsWindows()
      PathExists = fun p -> File.Exists p || Directory.Exists p
      IsFile = File.Exists
      Now = fun () -> DateTime.Now
      RandomSeed =
        fun () ->
          let bytes = Array.zeroCreate<byte> 8
          Random.Shared.NextBytes bytes
          BitConverter.ToUInt64(bytes, 0)
      LoadConfig = fun _ -> failwith "-config file= is not supported yet"
      Version = "EngineBattle " + ChessLibrary.BuildInfo.describe () }

  /// A line the reference prints while parsing: stdout (Logger::print) or stderr.
  type Message =
    | Stdout of string
    | Stderr of string

  type Parsed =
    { Tournament: Tournament
      Engines: EngineConfig list
      Stats: (string * MatchStats.Stats) list
      Messages: Message list }

  type Outcome =
    /// run the tournament
    | Run of Parsed
    /// -help, -version or no arguments: print the text (after any messages) and exit 0
    | Exit of messages: Message list * text: string
    /// print the messages, then the error, and exit 1
    | Failed of messages: Message list * error: string

  /// The line `-version` prints.
  let version (env: Env) = env.Version

  /// What -help (and no arguments at all) prints: match-help.txt.
  let helpText =
    lazy
      (use s = typeof<Env>.Assembly.GetManifestResourceStream("ChessLibrary.Match.match-help.txt")
       use r = new StreamReader(s)
       r.ReadToEnd().Replace("\r", ""))

  // ---- parsing helpers ----

  exception private FcError of string
  exception private ExitRequested of string

  let private fail msg = raise (FcError msg)
  let private inv = CultureInfo.InvariantCulture

  let private throwMissing (name: string) (key: string) (value: string) =
    fail $"Unrecognized {name} option \"{key}\" with value \"{value}\"."

  let private invalidNumber (s: string) = fail $"Invalid numeric value: \"{s}\""

  let private hasSpace (s: string) = s |> Seq.exists Char.IsWhiteSpace

  /// parseScalar<integer>: std::stoll/stoull over the whole string, then the target's range.
  let private parseInt64 (lo: int64) (hi: int64) (s: string) =
    if s = "" || hasSpace s then invalidNumber s
    match Int64.TryParse(s, NumberStyles.AllowLeadingSign, inv) with
    | true, v when v >= lo && v <= hi -> v
    | _ -> invalidNumber s

  let private parseInt (s: string) = int (parseInt64 (int64 Int32.MinValue) (int64 Int32.MaxValue) s)
  let private parseLong (s: string) = parseInt64 Int64.MinValue Int64.MaxValue s

  let private parseUInt64 (s: string) =
    if s = "" || hasSpace s || s.StartsWith "-" then invalidNumber s
    match UInt64.TryParse(s, NumberStyles.AllowLeadingSign, inv) with
    | true, v -> v
    | _ -> invalidNumber s

  /// parseScalar<double>: std::stold over the whole string, finite and within double.
  let private parseDouble (s: string) =
    if s = "" || hasSpace s then invalidNumber s
    match Double.TryParse(s, NumberStyles.Float, inv) with
    | true, v when Double.IsFinite v -> v
    | _ -> invalidNumber s

  let private isBool (s: string) = s = "true" || s = "false"

  let private splitTimeControl (value: string) (delimiter: char) =
    let sep = value.IndexOf delimiter
    if sep <= 0 || sep + 1 = value.Length || value.IndexOf(delimiter, sep + 1) >= 0 then
      fail $"Invalid time control: \"{value}\""
    value.Substring(0, sep), value.Substring(sep + 1)

  /// parseDuration: seconds (or minutes) to rounded milliseconds; a seconds value may end in `s`.
  /// Parsed as decimal, so 0.002s is exactly 2 ms (the reference uses long double).
  let private parseDuration (value: string) (multiplier: int64) =
    let value = if value.Length > 1 && value.EndsWith "s" && multiplier = 1000L then value.Substring(0, value.Length - 1) else value
    let tooLong () = fail $"Invalid time control duration: \"{value}\""
    let d = parseDouble value
    if d < 0.0 then tooLong ()
    match Decimal.TryParse(value, NumberStyles.Float, inv) with
    | true, m ->
      // past decimal's range (about 7.9e28) the product throws: that is too long as well
      let scaled = try Some(Math.Round(m * decimal multiplier, MidpointRounding.AwayFromZero)) with :? OverflowException -> None
      match scaled with
      | Some v when v <= decimal Int64.MaxValue -> int64 v
      | _ -> tooLong ()
    | _ -> tooLong ()   // finite but beyond decimal: far beyond int64 milliseconds

  /// engine::parseTc: [moves/]time[+inc], time = sec or min:sec; replaces the whole limit.
  let parseTc (tc: string) =
    if tc.Contains "hg" then fail "Hourglass time control not supported."
    if tc = "infinite" || tc = "inf" then TcLimits.Zero
    else
      if tc = "" then fail "Invalid time control: empty value"
      let mutable limits = TcLimits.Zero
      let mutable remaining = tc
      if remaining.Contains '/' then
        let moves, rest = splitTimeControl remaining '/'
        if moves <> "inf" && moves <> "infinite" then
          let m = parseLong moves
          if m <= 0L then fail "Time control move count must be positive"
          limits <- { limits with Moves = m }
        remaining <- rest
      if remaining.Contains '+' then
        let time, increment = splitTimeControl remaining '+'
        limits <- { limits with Increment = parseDuration increment 1000L }
        remaining <- time
      if remaining.Contains ':' then
        let minutes, seconds = splitTimeControl remaining ':'
        let m = parseDuration minutes 60000L
        let s = parseDuration seconds 1000L
        if s > Int64.MaxValue - m then fail $"Invalid time control duration: \"{remaining}\""
        { limits with Time = m + s }
      else
        { limits with Time = parseDuration remaining 1000L }

  /// parseTc for a caller outside the command line (tournament.json's "Tc"): the error text
  /// instead of an exception.
  let tryParseTc (tc: string) : Result<TcLimits, string> =
    try Ok(parseTc tc) with FcError msg -> Error msg

  let private applyEngineKey (e: EngineConfig) (key: string) (value: string) =
    match key with
    | "cmd" -> { e with Cmd = value }
    | "name" -> { e with Name = value }
    | "tc" -> { e with Tc = parseTc value }
    | "st" -> { e with Tc = { e.Tc with FixedTime = parseDuration value 1000L } }
    | "timemargin" ->
      let m = parseLong value
      if m < 0L then fail "The value for timemargin cannot be a negative number."
      { e with Tc = { e.Tc with TimeMargin = m } }
    | "nodes" -> { e with Nodes = parseLong value }
    | "plies" | "depth" -> { e with Plies = parseLong value }
    | "dir" -> { e with Dir = value }
    | "args" -> { e with Args = value }
    | "restart" ->
      if value <> "on" && value <> "off" then fail $"Invalid parameter (must be either \"on\" or \"off\"): {value}"
      { e with Restart = (value = "on") }
    | "ponder" ->
      // bare (cutechess-cli's form), or ponder=on / ponder=off
      if value <> "" && value <> "on" && value <> "off" then fail $"Invalid parameter (must be either \"on\" or \"off\"): {value}"
      { e with Ponder = (value <> "off") }
    | k when k.StartsWith "option." -> { e with Options = e.Options @ [ k.Substring(k.IndexOf '.' + 1), value ] }
    | "proto" ->
      if value <> "uci" then fail "Unsupported protocol."
      e
    | _ -> throwMissing "engine" key value

  /// parseIntList over an istringstream: 5,10,13-17,23. True = bad list.
  let private parseCpuList (s: string) =
    let list = ResizeArray<int>()
    let mutable pos = 0
    let skipWs () = while pos < s.Length && Char.IsWhiteSpace s.[pos] do pos <- pos + 1
    let readInt () =
      skipWs ()
      let start = pos
      if pos < s.Length && (s.[pos] = '+' || s.[pos] = '-') then pos <- pos + 1
      let digits = pos
      while pos < s.Length && Char.IsAsciiDigit s.[pos] do pos <- pos + 1
      if pos = digits then None
      else
        match Int32.TryParse(s.AsSpan(start, pos - start), NumberStyles.AllowLeadingSign, inv) with
        | true, v -> Some v
        | _ -> None
    let readChar () =
      skipWs ()
      if pos < s.Length then (pos <- pos + 1; Some s.[pos - 1]) else None
    let rec go () =
      match readInt () with
      | None -> true
      | Some a ->
        list.Add a
        match readChar () with
        | None -> false
        | Some '-' ->
          match readInt () with
          | Some b when b > a ->
            for i in a + 1 .. b do list.Add i
            match readChar () with
            | None -> false
            | Some ',' -> go ()
            | Some _ -> true
          | _ -> true
        | Some ',' -> go ()
        | Some _ -> true
    let bad = go ()
    bad, List.ofSeq list

  // ---- the parser ----

  /// How an option takes its parameters (cli.hpp ParamStyle).
  type private ParamStyle =
    | FreeParams
    | NoParam
    | SingleParam
    | KeyValueParams
    | OptionalKeyValueParams

  type private State(env: Env) =
    member val T = defaultTournament (env.RandomSeed ()) with get, set
    member val Engines = ResizeArray<EngineConfig>() with get, set
    /// what `-config discard=true` restores: before any `file=`, a fresh default, as in the reference
    member val OldT = defaultTournament (env.RandomSeed ()) with get, set
    member val OldEngines = ResizeArray<EngineConfig>() with get, set
    member val Stats: (string * MatchStats.Stats) list = [] with get, set
    member val Messages = ResizeArray<Message>()
    member s.Print(line: string) = s.Messages.Add(Stdout line)
    member _.Env = env

  type private Handler =
    | NoArgs of (State -> unit)
    | OneArg of (string -> State -> unit)
    | Pairs of ((string * string) list -> State -> unit)
    | Tokens of (string list -> State -> unit)

  let private datetimeName (env: Env) (ext: string) =
    "match_" + env.Now().ToString("yyyyMMdd_HHmmss", inv) + ext

  let private parseEngine (kv: (string * string) list) (s: State) =
    s.Engines.Add EngineConfig.Empty
    let i = s.Engines.Count - 1
    for k, v in kv do s.Engines.[i] <- applyEngineKey s.Engines.[i] k v

  let private parseEach (kv: (string * string) list) (s: State) =
    for k, v in kv do
      for i in 0 .. s.Engines.Count - 1 do s.Engines.[i] <- applyEngineKey s.Engines.[i] k v

  let private parsePgnOut (kv: (string * string) list) (s: State) =
    let mutable p = { s.T.Pgn with File = datetimeName s.Env ".pgn" }
    for key, value in kv do
      p <-
        match key with
        | "file" -> { p with File = value }
        | "append" when isBool value -> { p with AppendFile = (value = "true") }
        | "nodes" when isBool value -> { p with TrackNodes = (value = "true") }
        | "seldepth" when isBool value -> { p with TrackSeldepth = (value = "true") }
        | "nps" when isBool value -> { p with TrackNps = (value = "true") }
        | "hashfull" when isBool value -> { p with TrackHashfull = (value = "true") }
        | "tbhits" when isBool value -> { p with TrackTbhits = (value = "true") }
        | "timeleft" when isBool value -> { p with TrackTimeleft = (value = "true") }
        | "latency" when isBool value -> { p with TrackLatency = (value = "true") }
        | "min" when isBool value -> { p with Min = (value = "true") }
        | "notation" ->
          match value with
          | "san" -> { p with Notation = San }
          | "lan" -> { p with Notation = Lan }
          | "uci" -> { p with Notation = Uci }
          | _ -> throwMissing "pgnout notation" key value
        | "match_line" -> { p with AdditionalLinesRgx = p.AdditionalLinesRgx @ [ value ] }
        | "pv" when isBool value -> { p with TrackPv = (value = "true") }
        | _ -> throwMissing "pgnout" key value
    s.T <- { s.T with Pgn = p }

  let private parseEpdOut (kv: (string * string) list) (s: State) =
    s.T <- { s.T with Epd = { s.T.Epd with File = datetimeName s.Env ".epd" } }
    for key, value in kv do
      match key with
      | "file" -> s.T <- { s.T with Epd = { s.T.Epd with File = value } }
      | "append" when isBool value -> s.T <- { s.T with Epd = { s.T.Epd with AppendFile = (value = "true") } }
      | _ -> throwMissing "epdout" key value

  let private parseOpening (kv: (string * string) list) (s: State) =
    for key, value in kv do
      let o = s.T.Opening
      let o =
        match key with
        | "file" ->
          if not (s.Env.PathExists value) then fail $"Opening file does not exist: {value}"
          if value.EndsWith ".epd" then { o with File = value; Format = Epd }
          elif value.EndsWith ".pgn" then { o with File = value; Format = Pgn }
          else { o with File = value }
        | "format" ->
          match value with
          | "epd" -> { o with Format = Epd }
          | "pgn" -> { o with Format = Pgn }
          | _ -> throwMissing "openings format" key value
        | "order" ->
          match value with
          | "sequential" -> { o with Order = Sequential }
          | "random" -> { o with Order = Random }
          | _ -> throwMissing "openings order" key value
        | "plies" -> { o with Plies = parseInt value }
        | "start" ->
          let start = parseInt value
          if start < 1 then fail "Starting offset must be at least 1!"
          { o with Start = start }
        | "policy" ->
          if value <> "round" then fail "Unsupported opening book policy."
          o
        | _ -> throwMissing "openings" key value
      s.T <- { s.T with Opening = o }

  let private parseSprt (kv: (string * string) list) (s: State) =
    s.T <- { s.T with Sprt = { s.T.Sprt with Enabled = true } }
    if s.T.Rounds = 0 then s.T <- { s.T with Rounds = 500000 }
    for key, value in kv do
      let p = s.T.Sprt
      let p =
        match key with
        | "elo0" -> { p with Elo0 = parseDouble value }
        | "elo1" -> { p with Elo1 = parseDouble value }
        | "alpha" -> { p with Alpha = parseDouble value }
        | "beta" -> { p with Beta = parseDouble value }
        | "model" -> { p with Model = value }
        | _ -> throwMissing "sprt" key value
      s.T <- { s.T with Sprt = p }

  let private nonNegativeScore (value: string) =
    let score = parseInt value
    if score < 0 then fail "Score cannot be negative."
    score

  let private parseDraw (kv: (string * string) list) (s: State) =
    s.T <- { s.T with Draw = { s.T.Draw with Enabled = true } }
    for key, value in kv do
      let d = s.T.Draw
      let d =
        match key with
        | "movenumber" -> { d with MoveNumber = parseInt value }
        | "movecount" -> { d with MoveCount = parseInt value }
        | "score" -> { d with Score = nonNegativeScore value }
        | _ -> throwMissing "draw" key value
      s.T <- { s.T with Draw = d }

  let private parseResign (kv: (string * string) list) (s: State) =
    s.T <- { s.T with Resign = { s.T.Resign with Enabled = true } }
    for key, value in kv do
      let r = s.T.Resign
      let r =
        match key with
        | "movecount" -> { r with MoveCount = parseInt value }
        | "twosided" when isBool value -> { r with TwoSided = (value = "true") }
        | "score" -> { r with Score = nonNegativeScore value }
        | _ -> throwMissing "resign" key value
      s.T <- { s.T with Resign = r }

  let private parseLog (kv: (string * string) list) (s: State) =
    for key, value in kv do
      let l = s.T.Log
      let l =
        match key with
        | "file" -> { l with File = value }
        | "level" ->
          match value with
          | "trace" -> { l with Level = Trace }
          | "warn" -> { l with Level = Warn }
          | "info" -> { l with Level = Info }
          | "err" -> { l with Level = Err }
          | "fatal" -> { l with Level = Fatal }
          | _ -> throwMissing "log level" key value
        | "append" when isBool value -> { l with AppendFile = (value = "true") }
        | "compress" when isBool value -> { l with Compress = (value = "true") }
        | "realtime" when isBool value -> { l with Realtime = (value = "true") }
        | "engine" when isBool value -> { l with EngineComs = (value = "true") }
        | _ -> throwMissing "log" key value
      s.T <- { s.T with Log = l }

  let private parseConfig (kv: (string * string) list) (s: State) =
    let mutable dropStats = false
    for key, value in kv do
      match key with
      | "file" ->
        s.Print $"Loading config file: {value}"
        let loaded = s.Env.LoadConfig value
        s.OldEngines <- ResizeArray s.Engines
        s.OldT <- s.T
        s.T <- loaded.Tournament
        s.Engines <- ResizeArray loaded.Engines
        s.Stats <- loaded.Stats
      | "outname" -> s.T <- { s.T with ConfigName = value }
      | "discard" when value = "true" ->
        s.Print "Discarding config file"
        s.T <- s.OldT
        s.Engines <- ResizeArray s.OldEngines
        s.Stats <- []
      | "stats" -> dropStats <- (value = "false")
      | _ -> throwMissing "config" key value
    if s.Engines.Count > 2 then
      s.Messages.Add(Stderr "Warning: Stats will be dropped for more than 2 engines.")
      s.Stats <- []
    if dropStats then s.Stats <- []

  let private parseReport (kv: (string * string) list) (s: State) =
    for key, value in kv do
      if key = "penta" && isBool value then s.T <- { s.T with ReportPenta = (value = "true") }
      else throwMissing "report" key value

  let private parseOutput (kv: (string * string) list) (s: State) =
    for key, value in kv do
      match key, value with
      | "format", "cutechess" -> s.T <- { s.T with Output = Cutechess }
      | "format", "fastchess" -> s.T <- { s.T with Output = Default }
      | _ -> throwMissing "output" key value

  let private parseCrc (kv: (string * string) list) (s: State) =
    for key, value in kv do
      if key = "pgn" && isBool value then s.T <- { s.T with Pgn = { s.T.Pgn with Crc = (value = "true") } }
      else throwMissing "crc" key value

  let private parseQuick (kv: (string * string) list) (s: State) =
    let first = s.Engines.Count
    let mutable engines = 0
    for key, value in kv do
      match key with
      | "cmd" ->
        s.Engines.Add { EngineConfig.Empty with Cmd = value; Name = value; Tc = { TcLimits.Zero with Time = 10000L; Increment = 100L } }
        s.T <- { s.T with Recover = true }
        engines <- engines + 1
      | "book" ->
        let format =
          if value.EndsWith ".pgn" then Pgn
          elif value.EndsWith ".epd" then Epd
          else fail "Please include the .pgn or .epd file extension for the opening book."
        s.T <- { s.T with Opening = { s.T.Opening with File = value; Order = Random; Format = format } }
      | _ -> throwMissing "quick" key value
    if engines <> 2 then fail "Option \"-quick\" requires exactly two cmd entries."
    if s.T.Opening.File = "" then fail "Option \"-quick\" requires a book=FILE entry."
    if s.Engines.[first].Name = s.Engines.[first + 1].Name then
      s.Engines.[first] <- { s.Engines.[first] with Name = s.Engines.[first].Name + "1" }
      s.Engines.[first + 1] <- { s.Engines.[first + 1] with Name = s.Engines.[first + 1].Name + "2" }
    s.T <-
      { s.T with
          Games = 2
          Rounds = 25000
          Concurrency = max 1 (s.Env.HardwareThreads - 2)
          Recover = true
          Draw = { Enabled = true; MoveNumber = 30; MoveCount = 8; Score = 8 }
          Output = Cutechess }

  let private parseAffinity (ps: string list) (s: State) =
    s.T <- { s.T with Affinity = true }
    match ps with
    | first :: _ ->
      let bad, cpus = parseCpuList first
      s.T <- { s.T with AffinityCpus = s.T.AffinityCpus @ cpus }
      if bad then fail "Bad cpu list."
    | [] -> ()

  /// The option table, in the reference's registration order (which also orders the suggestions).
  let private options: (string * ParamStyle * bool * Handler) list =
    let set f = OneArg(fun v (s: State) -> s.T <- f s.T v)
    let flag f = NoArgs(fun (s: State) -> s.T <- f s.T)
    [ "-engine", KeyValueParams, false, Pairs parseEngine
      "-each", KeyValueParams, true, Pairs parseEach
      "-pgnout", OptionalKeyValueParams, false, Pairs parsePgnOut
      "-epdout", OptionalKeyValueParams, false, Pairs parseEpdOut
      "-openings", KeyValueParams, false, Pairs parseOpening
      "-sprt", KeyValueParams, false, Pairs parseSprt
      "-draw", KeyValueParams, false, Pairs parseDraw
      "-resign", KeyValueParams, false, Pairs parseResign
      "-maxmoves", SingleParam, false, set (fun t v -> { t with MaxMoves = { MoveCount = parseInt v; Enabled = true } })
      "-tb", SingleParam, false, set (fun t v -> { t with TbAdjudication = { t.TbAdjudication with SyzygyDirs = v; Enabled = true } })
      "-tbpieces", SingleParam, false, set (fun t v -> { t with TbAdjudication = { t.TbAdjudication with MaxPieces = parseInt v } })
      "-tbignore50", NoParam, false, flag (fun t -> { t with TbAdjudication = { t.TbAdjudication with Ignore50MoveRule = true } })
      "-tbadjudicate", SingleParam, false,
      set (fun t v ->
        let r =
          match v with
          | "WIN_LOSS" -> WinLoss
          | "DRAW" -> DrawOnly
          | "BOTH" -> Both
          | _ -> fail $"Invalid tb adjudication type: {v}"
        { t with TbAdjudication = { t.TbAdjudication with ResultType = r } })
      "-autosaveinterval", SingleParam, false, set (fun t v -> { t with AutoSaveInterval = parseInt v })
      "-log", KeyValueParams, false, Pairs parseLog
      "-config", KeyValueParams, false, Pairs parseConfig
      "-report", KeyValueParams, false, Pairs parseReport
      "-output", KeyValueParams, false, Pairs parseOutput
      "-concurrency", SingleParam, false, set (fun t v -> { t with Concurrency = parseInt v })
      "-crc32", KeyValueParams, false, Pairs parseCrc
      "-force-concurrency", NoParam, false, flag (fun t -> { t with ForceConcurrency = true })
      "-event", FreeParams, false, Tokens(fun ps s -> s.T <- { s.T with Pgn = { s.T.Pgn with EventName = String.concat "" ps } })
      "-site", FreeParams, false, Tokens(fun ps s -> s.T <- { s.T with Pgn = { s.T.Pgn with Site = String.concat "" ps } })
      "-games", SingleParam, false, set (fun t v -> { t with Games = parseInt v })
      "-rounds", SingleParam, false, set (fun t v -> { t with Rounds = parseInt v })
      "-wait", SingleParam, false, set (fun t v -> { t with Wait = parseInt v })
      "-noswap", NoParam, false, flag (fun t -> { t with NoSwap = true })
      "-reverse", NoParam, false, flag (fun t -> { t with Reverse = true })
      "-ratinginterval", SingleParam, false, set (fun t v -> { t with RatingInterval = parseInt v })
      "-scoreinterval", SingleParam, false, set (fun t v -> { t with ScoreInterval = parseInt v })
      "-srand", SingleParam, false, set (fun t v -> { t with Seed = parseUInt64 v })
      "-seeds", SingleParam, false, set (fun t v -> { t with GauntletSeeds = parseInt v })
      "-version", NoParam, false, NoArgs(fun s -> raise (ExitRequested(version s.Env + "\n")))
      "--version", NoParam, false, NoArgs(fun s -> raise (ExitRequested(version s.Env + "\n")))
      "-v", NoParam, false, NoArgs(fun s -> raise (ExitRequested(version s.Env + "\n")))
      "--v", NoParam, false, NoArgs(fun s -> raise (ExitRequested(version s.Env + "\n")))
      "-help", NoParam, false, NoArgs(fun _ -> raise (ExitRequested helpText.Value))
      "--help", NoParam, false, NoArgs(fun _ -> raise (ExitRequested helpText.Value))
      "-recover", NoParam, false, flag (fun t -> { t with Recover = true })
      "-repeat", FreeParams, false,
      Tokens(fun ps s ->
        match ps with
        | [ p ] when p <> "" && p |> Seq.forall Char.IsAsciiDigit -> s.T <- { s.T with Games = parseInt p }
        | _ -> s.T <- { s.T with Games = 2 })
      "-variant", SingleParam, false,
      set (fun t v ->
        let t = if v = "fischerandom" then { t with Variant = Frc } else t
        if v <> "fischerandom" && v <> "standard" then fail "Unknown variant."
        t)
      "-tournament", SingleParam, false,
      set (fun t v ->
        match v with
        | "gauntlet" -> { t with Type = Gauntlet }
        | "roundrobin" -> { t with Type = RoundRobin }
        | _ -> fail "Unsupported tournament format. Only supports roundrobin and gauntlet.")
      "-quick", KeyValueParams, false, Pairs parseQuick
      "-use-affinity", FreeParams, false, Tokens parseAffinity
      "-check-mate-pvs", NoParam, false, flag (fun t -> { t with CheckMatePvs = true })
      "-show-latency", NoParam, false, flag (fun t -> { t with ShowLatency = true })
      "-debug", NoParam, false,
      NoArgs(fun _ ->
        fail "The 'debug' option does not exist. Use the 'log' option instead to write all engine input and output into a text file.")
      "-testEnv", NoParam, false, flag (fun t -> { t with TestEnv = true })
      "-strict", NoParam, false, flag (fun t -> { t with Strict = true })
      "-startup-ms", SingleParam, false, set (fun t v -> { t with StartupMs = parseUInt64 v })
      "-ucinewgame-ms", SingleParam, false, set (fun t v -> { t with UciNewGameMs = parseUInt64 v })
      "-ping-ms", SingleParam, false, set (fun t v -> { t with PingMs = parseUInt64 v }) ]

  let private optionMap = options |> List.map (fun (f, style, deferred, h) -> f, (style, deferred, h)) |> dict

  /// Whether a token is one of the reference's options (a command line that starts with one is a match).
  let isOption (token: string) = optionMap.ContainsKey token

  let levenshtein (a: string) (b: string) =
    let a, b = if a.Length > b.Length then b, a else a, b
    let mutable prev = Array.init (a.Length + 1) id
    let mutable curr = Array.zeroCreate (a.Length + 1)
    for j in 1 .. b.Length do
      curr.[0] <- j
      for i in 1 .. a.Length do
        let substitution = prev.[i - 1] + (if a.[i - 1] <> b.[j - 1] then 1 else 0)
        curr.[i] <- min substitution (min (curr.[i - 1] + 1) (prev.[i] + 1))
      let t = prev in prev <- curr; curr <- t
    prev.[a.Length]

  /// The nearest option within distance 2. On a tie the reference takes the first minimum in its hash
  /// map's order, which differs between its Linux and Windows builds; this takes the first in
  /// registration order. Against the Linux build, 243 of 248 one-edit typos of every option get
  /// the same suggestion; the rest are ties (-t: "-v" there, "-tb" here).
  let suggestion (arg: string) =
    let mutable best = None
    let mutable bestDistance = Int32.MaxValue
    for flag, _, _, _ in options do
      let d = levenshtein arg flag
      if d < bestDistance then
        bestDistance <- d
        best <- Some flag
    if bestDistance <= 2 then best else None

  let private run (flag: string) (style: ParamStyle) (handler: Handler) (ps: string list) (s: State) =
    let pairs optional =
      if ps.IsEmpty && not optional then fail $"Option \"{flag}\" expects key=value parameters."
      ps
      |> List.map (fun p ->
        // `ponder` stands alone, as cutechess-cli writes it (-each ponder)
        if p = "ponder" && (flag = "-engine" || flag = "-each") then "ponder", "" else
        let pos = p.IndexOf '='
        if pos <= 0 || pos + 1 = p.Length then fail $"Option \"{flag}\" expects key=value pairs, got \"{p}\"."
        p.Substring(0, pos), p.Substring(pos + 1))
    match style, handler with
    | NoParam, NoArgs f ->
      if not ps.IsEmpty then fail $"Option \"{flag}\" does not accept parameters."
      f s
    | SingleParam, OneArg f ->
      match ps with
      | [ v ] -> f v s
      | _ -> fail $"Option \"{flag}\" expects exactly one value."
    | KeyValueParams, Pairs f -> f (pairs false) s
    | OptionalKeyValueParams, Pairs f -> f (pairs true) s
    | FreeParams, Tokens f -> f ps s
    | _ -> invalidOp $"option table: {flag} has a handler of the wrong style"

  let private wrapped (flag: string) (shown: string) (e: exn) =
    let reason =
      match e with
      | FcError m -> m
      | e -> e.Message
    $"Error while reading option \"{flag}\" with value \"{shown}\"\nReason: {reason}"

  // ---- sanitising (sanitize.cpp) ----

  let private sanitizeTournament (s: State) =
    // fixConfig: games and rounds are size_t, so a negative count is huge
    let t = s.T
    let t =
      if uint64 (int64 t.Games) > 2UL then
        let t = { t with Games = t.Rounds; Rounds = t.Games }
        if uint64 (int64 t.Games) > 2UL then fail "Error: Exceeded -game limit! Must be less than 2"
        t
      else t
    let t = if t.ReportPenta && t.Output = Cutechess then { t with ReportPenta = false } else t
    let t = if t.ReportPenta && t.Games <> 2 then { t with ReportPenta = false } else t
    // setDefaults
    let t = if t.RatingInterval = 0 then { t with RatingInterval = Int32.MaxValue } else t
    let t = if t.ScoreInterval = 0 then { t with ScoreInterval = Int32.MaxValue } else t
    // adjustConcurrency
    let t =
      if t.Concurrency <= 0 then
        // in int64: abs Int32.MinValue throws (the reference's std::abs is undefined there)
        let c = int (max (int64 Int32.MinValue) (int64 s.Env.HardwareThreads - abs (int64 t.Concurrency)))
        s.Print $"Info: Adjusted concurrency to {c} based on number of available hardware threads."
        { t with Concurrency = c }
      else t
    if t.Concurrency > s.Env.HardwareThreads && not t.ForceConcurrency then
      fail "Error: Concurrency exceeds number of CPUs. Use -force-concurrency to override."
    // validateConfig
    let t =
      if t.Sprt.Enabled then
        match MatchSprt.validate t.Sprt.Alpha t.Sprt.Beta t.Sprt.Elo0 t.Sprt.Elo1 t.Sprt.Model t.ReportPenta with
        | Error e -> fail e
        | Ok(penta, warning) ->
          warning |> Option.iter s.Print
          { t with ReportPenta = penta }
      else t
    if t.Variant = Frc && t.Opening.File = "" then fail "Error: Please specify a Chess960 opening book"
    if t.Opening.File = "" then
      s.Print "Warning: No opening book specified! Consider using one, otherwise all games will be played from the starting position."
    if t.Opening.Format <> Epd && t.Opening.Format <> Pgn then
      s.Print "Warning: Unknown opening format, 2. All games will be played from the starting position."
    if t.TbAdjudication.Enabled && t.TbAdjudication.SyzygyDirs = "" then
      fail "Error: Must provide a ;-separated list of Syzygy tablebase directories."
    s.T <- t

  /// std::filesystem::path::stem: the file name without its last extension, where a leading dot
  /// does not start one (".hidden" stays ".hidden"; .NET's GetFileNameWithoutExtension gives "").
  let stem (path: string) =
    let name = Path.GetFileName path
    let dot = name.LastIndexOf '.'
    if name = "." || name = ".." || dot <= 0 then name else name.Substring(0, dot)

  let private sanitizeEngines (s: State) =
    if s.Engines.Count < 2 then fail "Error: Need at least two engines to start!"
    for i in 0 .. s.Engines.Count - 1 do
      let mutable e = s.Engines.[i]
      if s.Env.IsWindows && not (e.Cmd.Contains '.') then e <- { e with Cmd = e.Cmd + ".exe" }
      let tc = e.Tc
      if tc.Time + tc.Increment = 0L && tc.FixedTime = 0L && e.Nodes = 0L && e.Plies = 0L then
        fail "Error; no TimeControl specified!"
      if tc.Time + tc.Increment <> 0L && tc.FixedTime <> 0L then fail "Error; cannot use tc and st together!"
      let path = e.EnginePath
      if (e.Dir <> "" || Path.IsPathFullyQualified path) && not (s.Env.IsFile path) then
        fail $"Engine binary does not exist: {path}"
      if e.Name = "" then e <- { e with Name = stem e.Cmd }
      if e.Name = "" then fail "Error; please specify a name for each engine!"
      s.Engines.[i] <- e
    let seen = Dictionary<string, int>()
    for i in 0 .. s.Engines.Count - 1 do
      let name = s.Engines.[i].Name
      let n = (match seen.TryGetValue name with | true, n -> n | _ -> 0) + 1
      seen.[name] <- n
      if n > 1 then s.Engines.[i] <- { s.Engines.[i] with Name = $"{name}_{n}" }

  /// OptionsParser: parse argv (without the program name) the way the reference does.
  let parse (env: Env) (args: string list) : Outcome =
    let s = State(env)
    let messages () = List.ofSeq s.Messages
    try
      if args.IsEmpty then raise (ExitRequested helpText.Value)
      let args = Array.ofList args
      let deferred = Dictionary<string, ResizeArray<string>>()
      let mutable i = 0
      while i < args.Length do
        let arg = args.[i]
        match optionMap.TryGetValue arg with
        | false, _ ->
          let f = $"Unrecognized option: {arg} parsing failed."
          match suggestion arg with
          | Some flag -> fail $"{f}: Did you mean \"{flag}\"?"
          | None -> fail f
        | true, (style, isDeferred, handler) ->
          try
            let ps = ResizeArray<string>()
            let mutable stop = false
            while not stop && i + 1 < args.Length do
              let v = args.[i + 1]
              // a following token starting with '-' is the next option, unless it is a negative number
              if v.Length > 0 && v.[0] = '-' && not (v.Length > 1 && Char.IsAsciiDigit v.[1]) then stop <- true
              else
                ps.Add v
                i <- i + 1
            if isDeferred then
              match deferred.TryGetValue arg with
              | true, slot -> slot.AddRange ps
              | _ -> deferred.[arg] <- ps
            else run arg style handler (List.ofSeq ps) s
          with
          | ExitRequested _ -> reraise ()
          | e -> fail (wrapped arg args.[i] e)
        i <- i + 1
      for KeyValue(flag, ps) in deferred do
        let style, _, handler = optionMap.[flag]
        try
          run flag style handler (List.ofSeq ps) s
        with
        | ExitRequested _ -> reraise ()
        | e -> fail (wrapped flag (if ps.Count = 0 then "<none>" else String.Join(" ", ps)) e)
      for j in 0 .. s.Engines.Count - 1 do
        s.Engines.[j] <- { s.Engines.[j] with Variant = s.T.Variant }
      sanitizeTournament s
      sanitizeEngines s
      Run { Tournament = s.T; Engines = List.ofSeq s.Engines; Stats = s.Stats; Messages = messages () }
    with
    | ExitRequested text -> Exit(messages (), text)
    | FcError e -> Failed(messages (), e)
    | e -> Failed(messages (), e.Message)
