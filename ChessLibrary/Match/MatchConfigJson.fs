namespace ChessLibrary.Match

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Encodings.Web
open ChessLibrary.Match.MatchArgs
open ChessLibrary.Match.MatchStats

/// The reference's config.json (cli.cpp:484-538, tournament/base/tournament.cpp:128-172,
/// types/*.hpp at 60d7a7a): the settings, the engines and the stats so far, written as the reference
/// writes them - nlohmann's ordered_json with four-space indents, enums as integers - so the
/// resume command a run prints (`-config file=config.json`) works, and a tool reading the file
/// finds what it expects.
///
/// On resume EngineBattle rebuilds the stats from the games in the PGN, the file it resumes
/// from anyway; the stats in this file are read (the reference's `-config` rules for them apply) but
/// only written for tools.
module MatchConfigJson =

  let private outputCode = function Default -> 0 | Cutechess -> 1
  let private notationCode = function San -> 0 | Lan -> 1 | Uci -> 2
  let private orderCode = function Random -> 0 | Sequential -> 1
  let private formatCode = function Epd -> 0 | Pgn -> 1 | NoFormat -> 2
  let private variantCode = function Standard -> 0 | Frc -> 1
  let private typeCode = function RoundRobin -> 0 | Gauntlet -> 1
  let private levelCode = function All -> 0 | Trace -> 1 | Info -> 2 | Warn -> 3 | Err -> 4 | Fatal -> 5

  let private ofCode (name: string) (all: 'a list) (code: 'a -> int) (n: int) =
    match all |> List.tryFind (fun a -> code a = n) with
    | Some a -> a
    | None -> failwith $"config.json: {name} {n} is not a valid value"

  /// nlohmann writes a double in its shortest form and gives an integral value a ".0".
  let private jsonDouble (x: float) =
    let s = MatchFormat.shortest x
    if s.Contains '.' || s.Contains 'e' || s.Contains "nan" || s.Contains "inf" then s else s + ".0"

  /// The file's text for a match: its settings (after sanitising), engines and stats, the stats as
  /// "{A} vs {B}" for each pair in command-line order, from A's view.
  let write (t: Tournament) (engines: EngineConfig list) (stats: (string * Stats) list) =
    let opts = JsonWriterOptions(Indented = true, IndentSize = 4, NewLine = "\n", Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
    use ms = new MemoryStream()
    (
      use w = new Utf8JsonWriter(ms, opts)
      let obj (name: string) (body: unit -> unit) =
        w.WriteStartObject name
        body ()
        w.WriteEndObject()
      let num (name: string) (v: int64) = w.WriteNumber(name, v)
      let dbl (name: string) (v: float) =
        w.WritePropertyName name
        w.WriteRawValue(jsonDouble v, skipInputValidation = true)
      w.WriteStartObject()
      obj "resign" (fun () ->
        num "move_count" t.Resign.MoveCount; num "score" t.Resign.Score
        w.WriteBoolean("twosided", t.Resign.TwoSided); w.WriteBoolean("enabled", t.Resign.Enabled))
      obj "draw" (fun () ->
        num "move_number" (int64 (uint32 t.Draw.MoveNumber)); num "move_count" t.Draw.MoveCount; num "score" t.Draw.Score
        w.WriteBoolean("enabled", t.Draw.Enabled))
      obj "maxmoves" (fun () -> num "move_count" t.MaxMoves.MoveCount; w.WriteBoolean("enabled", t.MaxMoves.Enabled))
      obj "tb_adjudication" (fun () ->
        w.WriteString("syzygy_dirs", t.TbAdjudication.SyzygyDirs); num "max_pieces" t.TbAdjudication.MaxPieces
        w.WriteBoolean("ignore_50_move_rule", t.TbAdjudication.Ignore50MoveRule); w.WriteBoolean("enabled", t.TbAdjudication.Enabled))
      obj "opening" (fun () ->
        w.WriteString("file", t.Opening.File); num "format" (formatCode t.Opening.Format); num "order" (orderCode t.Opening.Order)
        num "plies" t.Opening.Plies; num "start" t.Opening.Start)
      obj "pgn" (fun () ->
        let p = t.Pgn
        w.WriteStartArray "additional_lines_rgx"
        for r in p.AdditionalLinesRgx do w.WriteStringValue r
        w.WriteEndArray()
        w.WriteString("event_name", p.EventName); w.WriteString("site", p.Site); w.WriteString("file", p.File)
        num "notation" (notationCode p.Notation); w.WriteBoolean("append_file", p.AppendFile)
        w.WriteBoolean("track_nodes", p.TrackNodes); w.WriteBoolean("track_seldepth", p.TrackSeldepth)
        w.WriteBoolean("track_nps", p.TrackNps); w.WriteBoolean("track_hashfull", p.TrackHashfull)
        w.WriteBoolean("track_tbhits", p.TrackTbhits); w.WriteBoolean("track_timeleft", p.TrackTimeleft)
        w.WriteBoolean("track_latency", p.TrackLatency); w.WriteBoolean("track_pv", p.TrackPv)
        w.WriteBoolean("min", p.Min); w.WriteBoolean("crc", p.Crc))
      obj "epd" (fun () -> w.WriteString("file", t.Epd.File); w.WriteBoolean("append_file", t.Epd.AppendFile))
      obj "sprt" (fun () ->
        dbl "alpha" t.Sprt.Alpha; dbl "beta" t.Sprt.Beta; dbl "elo0" t.Sprt.Elo0; dbl "elo1" t.Sprt.Elo1
        w.WriteString("model", t.Sprt.Model); w.WriteBoolean("enabled", t.Sprt.Enabled))
      w.WriteString("config_name", t.ConfigName)
      num "output" (outputCode t.Output)
      w.WriteNumber("seed", t.Seed)
      num "variant" (variantCode t.Variant)
      num "type" (typeCode t.Type)
      num "gauntlet_seeds" t.GauntletSeeds
      num "ratinginterval" t.RatingInterval
      num "scoreinterval" t.ScoreInterval
      num "wait" (int64 (uint32 t.Wait))
      num "autosaveinterval" t.AutoSaveInterval
      // size_t in the reference: a negative count is written as it wraps
      w.WriteNumber("games", uint64 (int64 t.Games))
      w.WriteNumber("rounds", uint64 (int64 t.Rounds))
      num "concurrency" t.Concurrency
      w.WriteBoolean("force_concurrency", t.ForceConcurrency)
      w.WriteBoolean("recover", t.Recover)
      w.WriteBoolean("noswap", t.NoSwap)
      w.WriteBoolean("reverse", t.Reverse)
      w.WriteBoolean("report_penta", t.ReportPenta)
      w.WriteBoolean("affinity", t.Affinity)
      w.WriteBoolean("check_mate_pvs", t.CheckMatePvs)
      w.WriteBoolean("show_latency", t.ShowLatency)
      obj "log" (fun () ->
        let l = t.Log
        w.WriteString("file", l.File); num "level" (levelCode l.Level); w.WriteBoolean("append_file", l.AppendFile)
        w.WriteBoolean("compress", l.Compress); w.WriteBoolean("realtime", l.Realtime); w.WriteBoolean("engine_coms", l.EngineComs))
      w.WriteStartArray "engines"
      for e in engines do
        w.WriteStartObject()
        w.WriteString("name", e.Name); w.WriteString("dir", e.Dir); w.WriteString("cmd", e.Cmd); w.WriteString("args", e.Args)
        w.WriteBoolean("restart", e.Restart)
        w.WriteStartArray "options"
        for k, v in e.Options do
          w.WriteStartArray()
          w.WriteStringValue k
          w.WriteStringValue v
          w.WriteEndArray()
        w.WriteEndArray()
        obj "limit" (fun () ->
          obj "tc" (fun () ->
            num "increment" e.Tc.Increment; num "fixed_time" e.Tc.FixedTime; num "time" e.Tc.Time
            num "moves" e.Tc.Moves; num "timemargin" e.Tc.TimeMargin)
          w.WriteNumber("nodes", uint64 e.Nodes); w.WriteNumber("plies", uint64 e.Plies))
        num "variant" (variantCode e.Variant)
        w.WriteEndObject()
      w.WriteEndArray()
      obj "stats" (fun () ->
        for key, s in stats do
          obj key (fun () ->
            num "wins" s.Wins; num "losses" s.Losses; num "draws" s.Draws
            num "penta_WW" s.PentaWW; num "penta_WD" s.PentaWD; num "penta_WL" s.PentaWL
            num "penta_DD" s.PentaDD; num "penta_LD" s.PentaLD; num "penta_LL" s.PentaLL))
      w.WriteEndObject()
    )
    Encoding.UTF8.GetString(ms.ToArray()) + "\n"

  /// The stats for the file from a scoreboard: every pair in command-line order.
  let statsOf (engines: EngineConfig list) (board: MatchScoreboard.Scoreboard) =
    let names = engines |> List.map (fun e -> e.Name)
    [ for i in 0 .. names.Length - 1 do
        for j in i + 1 .. names.Length - 1 do
          yield $"{names.[i]} vs {names.[j]}", board.Stats(names.[i], names.[j]) ]

  /// Writes the file, as the reference's saveJson: replaced whole, never half-written (a temporary
  /// file moved over it). The temporary file is this process's own, so two runs saving in one
  /// folder do not collide on it, and it is removed when the write fails. Throws on failure.
  let save (path: string) (t: Tournament) (engines: EngineConfig list) (stats: (string * Stats) list) =
    let full = Path.GetFullPath path
    let tmp = $"{full}.{Environment.ProcessId}.tmp"
    try
      File.WriteAllText(tmp, write t engines stats, UTF8Encoding(false))
      File.Move(tmp, full, true)
    with _ ->
      (try File.Delete tmp with _ -> ())
      reraise ()

  // ---- reading ----

  let private prop (e: JsonElement) (name: string) =
    match e.TryGetProperty name with
    | true, v -> Some v
    | _ -> None

  /// Reads a file written by the reference or by this module. A missing key keeps the reference's default.
  let parse (json: string) : LoadedConfig =
    use doc = JsonDocument.Parse json
    let root = doc.RootElement
    let d = defaultTournament 0UL
    let sub (name: string) = prop root name
    let str (e: JsonElement option) (k: string) (dflt: string) = e |> Option.bind (fun e -> prop e k) |> Option.map (fun v -> v.GetString()) |> Option.defaultValue dflt
    // a size_t or uint32 above int64 reads back as the int it wrapped from
    let asInt (v: JsonElement) = match v.TryGetInt64() with | true, n -> int n | _ -> int (int64 (v.GetUInt64()))
    let int' (e: JsonElement option) (k: string) (dflt: int) = e |> Option.bind (fun e -> prop e k) |> Option.map asInt |> Option.defaultValue dflt
    let i64 (e: JsonElement option) (k: string) (dflt: int64) = e |> Option.bind (fun e -> prop e k) |> Option.map (fun v -> v.GetInt64()) |> Option.defaultValue dflt
    let bool' (e: JsonElement option) (k: string) (dflt: bool) = e |> Option.bind (fun e -> prop e k) |> Option.map (fun v -> v.GetBoolean()) |> Option.defaultValue dflt
    let dbl (e: JsonElement option) (k: string) (dflt: float) = e |> Option.bind (fun e -> prop e k) |> Option.map (fun v -> v.GetDouble()) |> Option.defaultValue dflt
    let top = Some root
    let resign = sub "resign"
    let draw = sub "draw"
    let maxmoves = sub "maxmoves"
    let tb = sub "tb_adjudication"
    let opening = sub "opening"
    let pgn = sub "pgn"
    let epd = sub "epd"
    let sprt = sub "sprt"
    let log = sub "log"
    let t =
      { d with
          Resign = { MoveCount = int' resign "move_count" d.Resign.MoveCount; Score = int' resign "score" d.Resign.Score
                     TwoSided = bool' resign "twosided" false; Enabled = bool' resign "enabled" false }
          Draw = { MoveNumber = int' draw "move_number" 0; MoveCount = int' draw "move_count" 1; Score = int' draw "score" 0
                   Enabled = bool' draw "enabled" false }
          MaxMoves = { MoveCount = int' maxmoves "move_count" 1; Enabled = bool' maxmoves "enabled" false }
          TbAdjudication =
            { d.TbAdjudication with
                SyzygyDirs = str tb "syzygy_dirs" ""; MaxPieces = int' tb "max_pieces" 0
                Ignore50MoveRule = bool' tb "ignore_50_move_rule" false; Enabled = bool' tb "enabled" false }
          Opening =
            { File = str opening "file" ""
              Format = ofCode "opening format" [ Epd; Pgn; NoFormat ] formatCode (int' opening "format" 2)
              Order = ofCode "opening order" [ Random; Sequential ] orderCode (int' opening "order" 1)
              Plies = int' opening "plies" -1
              Start = int' opening "start" 1 }
          Pgn =
            { AdditionalLinesRgx =
                pgn |> Option.bind (fun p -> prop p "additional_lines_rgx")
                |> Option.map (fun a -> [ for x in a.EnumerateArray() -> x.GetString() ]) |> Option.defaultValue []
              EventName = str pgn "event_name" d.Pgn.EventName; Site = str pgn "site" d.Pgn.Site; File = str pgn "file" ""
              Notation = ofCode "pgn notation" [ San; Lan; Uci ] notationCode (int' pgn "notation" 0)
              AppendFile = bool' pgn "append_file" true; TrackNodes = bool' pgn "track_nodes" false
              TrackSeldepth = bool' pgn "track_seldepth" false; TrackNps = bool' pgn "track_nps" false
              TrackHashfull = bool' pgn "track_hashfull" false; TrackTbhits = bool' pgn "track_tbhits" false
              TrackTimeleft = bool' pgn "track_timeleft" false; TrackLatency = bool' pgn "track_latency" false
              TrackPv = bool' pgn "track_pv" false; Min = bool' pgn "min" false; Crc = bool' pgn "crc" false }
          Epd = { File = str epd "file" ""; AppendFile = bool' epd "append_file" true }
          Sprt =
            { Alpha = dbl sprt "alpha" 0.0; Beta = dbl sprt "beta" 0.0; Elo0 = dbl sprt "elo0" 0.0; Elo1 = dbl sprt "elo1" 0.0
              Model = str sprt "model" "normalized"; Enabled = bool' sprt "enabled" false }
          ConfigName = str top "config_name" d.ConfigName
          Output = ofCode "output" [ Default; Cutechess ] outputCode (int' top "output" 0)
          Seed = (match prop root "seed" with Some v -> v.GetUInt64() | None -> d.Seed)
          Variant = ofCode "variant" [ Standard; Frc ] variantCode (int' top "variant" 0)
          Type = ofCode "type" [ RoundRobin; Gauntlet ] typeCode (int' top "type" 0)
          GauntletSeeds = int' top "gauntlet_seeds" 1
          RatingInterval = int' top "ratinginterval" 10
          ScoreInterval = int' top "scoreinterval" 1
          Wait = int' top "wait" 0
          AutoSaveInterval = int' top "autosaveinterval" 20
          Games = int' top "games" 2
          Rounds = int' top "rounds" 2
          Concurrency = int' top "concurrency" 1
          ForceConcurrency = bool' top "force_concurrency" false
          Recover = bool' top "recover" false
          NoSwap = bool' top "noswap" false
          Reverse = bool' top "reverse" false
          ReportPenta = bool' top "report_penta" true
          Affinity = bool' top "affinity" false
          CheckMatePvs = bool' top "check_mate_pvs" false
          ShowLatency = bool' top "show_latency" false
          Log =
            { File = str log "file" ""
              Level = ofCode "log level" [ All; Trace; Info; Warn; Err; Fatal ] levelCode (int' log "level" 3)
              AppendFile = bool' log "append_file" true; Compress = bool' log "compress" false
              Realtime = bool' log "realtime" true; EngineComs = bool' log "engine_coms" false } }
    let engines =
      match prop root "engines" with
      | None -> []
      | Some a ->
        [ for e in a.EnumerateArray() do
            let e' = Some e
            let limit = prop e "limit"
            let tc = limit |> Option.bind (fun l -> prop l "tc")
            yield
              { Name = str e' "name" ""; Dir = str e' "dir" ""; Cmd = str e' "cmd" ""; Args = str e' "args" ""
                Restart = bool' e' "restart" false
                Options =
                  match prop e "options" with
                  | Some o -> [ for p in o.EnumerateArray() -> p.[0].GetString(), p.[1].GetString() ]
                  | None -> []
                Tc =
                  { Increment = i64 tc "increment" 0L; FixedTime = i64 tc "fixed_time" 0L; Time = i64 tc "time" 0L
                    Moves = i64 tc "moves" 0L; TimeMargin = i64 tc "timemargin" 0L }
                Nodes = (match limit |> Option.bind (fun l -> prop l "nodes") with Some v -> int64 (v.GetUInt64()) | None -> 0L)
                Plies = (match limit |> Option.bind (fun l -> prop l "plies") with Some v -> int64 (v.GetUInt64()) | None -> 0L)
                Variant = ofCode "engine variant" [ Standard; Frc ] variantCode (int' e' "variant" 0) } ]
    let stats =
      match prop root "stats" with
      | None -> []
      | Some s ->
        [ for p in s.EnumerateObject() do
            let v = Some p.Value
            yield p.Name,
                  { Wins = int' v "wins" 0; Losses = int' v "losses" 0; Draws = int' v "draws" 0
                    PentaWW = int' v "penta_WW" 0; PentaWD = int' v "penta_WD" 0; PentaWL = int' v "penta_WL" 0
                    PentaDD = int' v "penta_DD" 0; PentaLD = int' v "penta_LD" 0; PentaLL = int' v "penta_LL" 0 } ]
    { Tournament = t; Engines = engines; Stats = stats }

  /// Env.LoadConfig: the reference's "File not found: {f}" when it cannot be opened.
  let load (path: string) : LoadedConfig =
    let text =
      try File.ReadAllText path
      with _ -> failwith $"File not found: {path}"
    parse text
