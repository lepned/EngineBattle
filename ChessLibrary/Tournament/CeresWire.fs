module ChessLibrary.CeresWire

// Maps Ceres's CELT/1.0 live-tournament wire schema (NDJSON; see the Ceres
// Ceres.Features/Tournaments/Streaming namespace) onto EngineBattle's own Live Feed Contract.
//
// This lets EB consume a live Ceres tournament broadcast through the SAME pipeline as its own
// feed: each CELT line is translated into an EB `Update`, serialized via `LiveFeedWire`
// (guaranteeing valid contract JSON), and stamped with an envelope `gameId` (= Ceres threadId)
// and `source` (= Ceres host) so it can be `JsonFeedService.Ingest`-ed and routed by the grid.
//
// CELT fields are camelCase (the publisher uses JsonNamingPolicy.CamelCase). Unit conversions:
//   evalCp (centipawns)  -> EvalType.CP (pawns, /100)
//   wdl {w,d,l} (0..1)   -> WDL per-mille (*1000), matching EB's own producer
//   *Ms (milliseconds)   -> TimeOnly (HH:mm:ss.fff)
// Known gaps (MVP): CELT carries top-moves but no PV string -> PV = played move only; moves are
// UCI (no SAN); opening moves are not expanded (board starts at startFEN). Eval is mover's
// perspective (matches EB's own feed).

open System
open System.Text.Json.Nodes
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Chess
open ChessLibrary.TimeControlTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes

// ---------------------------------------------------------------------------------------------
// CELT JSON readers (camelCase)
// ---------------------------------------------------------------------------------------------

let private parseObj (line: string) : JsonObject option =
    try
        match JsonNode.Parse(line) with
        | :? JsonObject as o -> Some o
        | _ -> None
    with _ -> None

let private getStr (o: JsonObject) (k: string) (d: string) =
    match o[k] with
    | null -> d
    | n -> (try n.GetValue<string>() with _ -> d)

let private getInt (o: JsonObject) (k: string) (d: int) =
    match o[k] with
    | null -> d
    | n -> (try n.GetValue<int>() with _ -> (try int (n.GetValue<float>()) with _ -> d))

let private getI64 (o: JsonObject) (k: string) (d: int64) =
    match o[k] with
    | null -> d
    | n -> (try n.GetValue<int64>() with _ -> (try int64 (n.GetValue<float>()) with _ -> d))

let private getF64 (o: JsonObject) (k: string) (d: float) =
    match o[k] with
    | null -> d
    | n -> (try n.GetValue<float>() with _ -> d)

let private getBool (o: JsonObject) (k: string) (d: bool) =
    match o[k] with
    | null -> d
    | n -> (try n.GetValue<bool>() with _ -> d)

let private getIntOpt (o: JsonObject) (k: string) : int option =
    match o[k] with
    | null -> None
    | n -> (try Some(n.GetValue<int>()) with _ -> (try Some(int (n.GetValue<float>())) with _ -> None))

// ---------------------------------------------------------------------------------------------
// Value conversions
// ---------------------------------------------------------------------------------------------

let private timeOnlyMs (ms: int) =
    let clamped = float (max 0 ms)
    // clocks are well under 24h; guard against pathological values
    let capped = min clamped (float (TimeSpan.FromHours(23.999).TotalMilliseconds))
    TimeOnly.FromTimeSpan(TimeSpan.FromMilliseconds capped)

let private evalCp (o: JsonObject) =
    match getIntOpt o "evalCp" with
    | Some cp -> EvalType.CP(float cp / 100.0)
    | None -> EvalType.NA

/// Interim eval: a forced mate takes precedence over the cp eval.
let private interimEval (o: JsonObject) =
    match getIntOpt o "mate" with
    | Some m -> EvalType.Mate m
    | None -> evalCp o

let private wdlOf (o: JsonObject) : WDLType =
    match o["wdl"] with
    | :? JsonObject as w ->
        WDLType.HasValue
            { Win = getF64 w "w" 0.0 * 1000.0
              Draw = getF64 w "d" 0.0 * 1000.0
              Loss = getF64 w "l" 0.0 * 1000.0 }
    | _ -> WDLType.NotFound

let private moveDetailOf (lan: string) (side: string) =
    { LongSan = lan
      FromSq = (if lan.Length >= 2 then lan.Substring(0, 2) else "")
      ToSq = (if lan.Length >= 4 then lan.Substring(2, 2) else "")
      Color = side
      IsCastling = false
      Comments = "" }

let private moveAndFenOf (lan: string) (side: string) (fen: string) (san: string) =
    { Move = moveDetailOf lan side
      ShortSan = san
      FenAfterMove = fen }

/// Convert a UCI/long move to SAN given the position *before* the move; falls back to the UCI string.
let private sanOfMove (beforeFen: string) (lan: string) : string =
    try
        if String.IsNullOrWhiteSpace beforeFen then lan
        else
            let mutable board = Board()
            board.LoadFen(beforeFen)
            match BoardUtils.tryGetTMoveFromUciNotation &board lan with
            | Some tmove ->
                let san = BoardUtils.getSanNotationFromTMove &board tmove
                if String.IsNullOrEmpty san then lan else san
            | None -> lan
    with _ -> lan

// EngineBattle shows evals from White's perspective; CELT eval is the mover's perspective, so
// negate when Black moved. WDL win/loss swap likewise so it stays consistent with the eval.
let private flipEval (side: string) (e: EvalType) =
    if side = "b" then
        match e with
        | EvalType.CP v -> EvalType.CP(-v)
        | EvalType.Mate m -> EvalType.Mate(-m)
        | EvalType.NA -> EvalType.NA
    else e

let private flipWdl (side: string) (w: WDLType) =
    if side = "b" then
        match w with
        | WDLType.HasValue x -> WDLType.HasValue { x with Win = x.Loss; Loss = x.Win }
        | WDLType.NotFound -> WDLType.NotFound
    else w

/// Map the CELT tournament-meta time control (kind/valueMs/incrementMs/nodes/movesToGo) to an EB
/// TimeControl, so the feed tournament carries a real time control instead of a null one.
let private toTimeControl (o: JsonObject) : TimeControl option =
    match o["timeControl"] with
    | :? JsonObject as tc ->
        let kind = getStr tc "kind" "other"
        let valueMs = getInt tc "valueMs" 0
        let incrMs = getInt tc "incrementMs" 0
        let nodes = getInt tc "nodes" 0
        let movesToGo = getInt tc "movesToGo" 0
        let config =
            match kind with
            | "nodesPerMove" | "nodesForAllMoves" ->
                { Id = 1; Fixed = timeOnlyMs 0; Increment = timeOnlyMs 0; NodeLimit = true; Nodes = nodes }
            | "secondsPerMove" ->
                // EB has no per-move (movetime) mode; model it as a small 1s fixed base + an increment
                // equal to the per-move time, so each move effectively gets ~that time.
                { Id = 1; Fixed = timeOnlyMs 1000; Increment = timeOnlyMs valueMs; NodeLimit = false; Nodes = 0 }
            | _ -> // secondsForAllMoves / other: fixed + increment
                { Id = 1; Fixed = timeOnlyMs valueMs; Increment = timeOnlyMs incrMs; NodeLimit = false; Nodes = 0 }
        Some { TimeConfigs = [ config ]; WmovesToGo = movesToGo; BmovesToGo = movesToGo }
    | _ -> None

let private emit (source: string) (gameId: string) (u: Update) : string =
    LiveFeedWire.serializeUpdate u
    |> LiveFeedWire.withGameId gameId
    |> LiveFeedWire.withSource source

// ---------------------------------------------------------------------------------------------
// Public API — one mapper per CELT type. Each returns contract JSON (stamped gameId + source),
// or None when the line is not the expected type / malformed.
// ---------------------------------------------------------------------------------------------

/// Read the CELT `type` discriminator ("" if absent/malformed).
let celtType (line: string) : string =
    match parseObj line with
    | Some o -> getStr o "type" ""
    | None -> ""

/// "tournamentInfo" -> StartOfTournament (header: name, mode, players). gameId is global ("").
let mapTournamentInfo (source: string) (line: string) : string option =
    match parseObj line with
    | Some o when getStr o "type" "" = "tournamentInfo" ->
        let players =
            match o["players"] with
            | :? JsonArray as a ->
                [ for x in a do
                      match x with
                      | :? JsonObject as p -> yield getStr p "name" ""
                      | _ -> () ]
            | _ -> []
        let engines = players |> List.map (fun name -> { EngineConfig.Empty with Name = name })
        let t =
            { Tournament.Empty with
                Name = getStr o "name" ""
                Description = getStr o "machineName" ""   // host machine (shown in the grid header)
                TournamentMode = getStr o "mode" "RR"
                EngineSetup = { Engines = engines; EngineDefFolder = ""; EngineDefList = [] }
                TimeControl = (match toTimeControl o with Some tc -> tc | None -> Tournament.Empty.TimeControl) }
        let info =
            { NumberOfGames = (getInt o "numGamePairs" 0) * 2
              GameDurationInSec = TimeSpan.Zero
              TournamentDurationSec = TimeSpan.Zero
              Tournament = Some t }
        Some(emit source "" (Update.StartOfTournament info))
    | _ -> None

/// "gameStart" -> StartOfGame. Returns (white, black, startFen, json) so the caller can cache the
/// names (needed to attribute move/interim frames, which carry only a side) and the starting FEN
/// (needed to convert subsequent moves to SAN).
let mapGameStart (source: string) (gameId: string) (line: string) : (string * string * string * string) option =
    match parseObj line with
    | Some o when getStr o "type" "" = "gameStart" ->
        let white = getStr o "whiteName" ""
        let black = getStr o "blackName" ""
        let startFen = getStr o "startFEN" startPosition
        let g =
            { WhitePlayer = { EngineConfig.Empty with Name = white }
              BlackPlayer = { EngineConfig.Empty with Name = black }
              StartPos = startFen
              OpeningMovesAndFen = ResizeArray()
              WhiteTime = timeOnlyMs (getInt o "whiteTimeMs" 0)
              BlackTime = timeOnlyMs (getInt o "blackTimeMs" 0)
              WhiteToMove = getBool o "whiteToMove" true
              OpeningName = getStr o "openingName" ""
              CurrentGameNr = getInt o "roundNumber" 0
              OpeningHash = "" }
        Some(white, black, startFen, emit source gameId (Update.StartOfGame g))
    | _ -> None

/// "move" -> BestMove (advances the board). `white`/`black` are the cached names for this thread and
/// `beforeFen` is the position before this move (for SAN). Returns (json, fenAfterMove) so the caller
/// can track the position for the next move's SAN.
let mapMove (source: string) (gameId: string) (white: string) (black: string) (beforeFen: string) (priorHistory: string) (line: string) : (string * string * string) option =
    match parseObj line with
    | Some o when getStr o "type" "" = "move" ->
        let side = getStr o "side" "w"
        let player = if side = "w" then white else black
        let lan = getStr o "lan" ""
        let fen = getStr o "fen" startPosition
        let ev = flipEval side (evalCp o)
        let wdl = flipWdl side (wdlOf o)
        let san = sanOfMove beforeFen lan
        // Running SAN move list: a "N. " number token before each White move, then the SAN.
        // StreamingChessboard re-parses this string (number tokens vs SAN tokens) into the move list.
        let moveNum =
            let parts = beforeFen.Split(' ')
            if parts.Length >= 6 then (match Int32.TryParse(parts.[parts.Length - 1]) with | true, n -> n | _ -> 1) else 1
        let token = if side = "w" then sprintf "%d. %s" moveNum san else san
        let newHistory = if String.IsNullOrEmpty priorHistory then token else priorHistory + " " + token
        let nodes = getI64 o "nodes" 0L
        let nps = getF64 o "nps" 0.0
        let info =
            { Player = player
              Move = lan
              Ponder = ""
              Eval = ev
              TimeLeft = timeOnlyMs (getInt o "timeLeftMs" 0)
              MoveTime = timeOnlyMs (getInt o "moveTimeMs" 0)
              Nodes = nodes
              NPS = nps
              FEN = fen
              PV = san
              LongPV = lan
              MoveAndFen = moveAndFenOf lan side fen san
              MoveHistory = newHistory
              Move50 = 0
              R3 = 0
              PiecesLeft = getInt o "piecesLeft" 32
              AdjDrawML = 0 }
        let status =
            { PlayerName = player
              Eval = ev
              Nodes = nodes
              NPS = nps
              EPS = getF64 o "eps" 0.0
              Depth = getInt o "depth" 0
              SD = getInt o "selDepth" 0
              TBhits = 0L
              WDL = wdl
              PV = san
              PVLongSAN = lan
              MultiPV = 1 }
        Some(emit source gameId (Update.BestMove(info, status)), fen, newHistory)
    | _ -> None

/// "interim" -> Status (transient mid-search thinking; does NOT advance the board).
let mapInterim (source: string) (gameId: string) (white: string) (black: string) (line: string) : string option =
    match parseObj line with
    | Some o when getStr o "type" "" = "interim" ->
        let side = getStr o "side" "w"
        let player = if side = "w" then white else black
        let status =
            { PlayerName = player
              Eval = flipEval side (interimEval o)
              Nodes = getI64 o "nodes" 0L
              NPS = getF64 o "nps" 0.0
              EPS = getF64 o "eps" 0.0
              Depth = getInt o "depth" 0
              SD = getInt o "selDepth" 0
              TBhits = 0L
              WDL = flipWdl side (wdlOf o)
              PV = ""
              PVLongSAN = ""
              MultiPV = 1 }
        Some(emit source gameId (Update.Status status))
    | _ -> None

/// "gameEnd" (per-thread) or "gameResult" (global) -> EndOfGame (player1 = white, player2 = black,
/// matching EB standings attribution). Same payload; the global "gameResult" stream is replayed in
/// full on connect, which is what enables full-standings catch-up when joining mid-tournament.
let mapGameEnd (source: string) (gameId: string) (line: string) : string option =
    match parseObj line with
    | Some o when (let t = getStr o "type" "" in t = "gameEnd" || t = "gameResult") ->
        let reason =
            try
                stringToResultReason (getStr o "reason" "XX")
            with _ -> ResultReason.NotStarted
        let r =
            { Player1 = getStr o "whiteName" ""
              Player2 = getStr o "blackName" ""
              Moves = getInt o "moves" 0
              Result = getStr o "result" "1/2-1/2"
              Reason = reason
              GameTime = getI64 o "gameTimeMs" 0L
              OutOfOpeningEvals = [] }
        // Global "gameResult" carries the producing thread id; tag the gameId "r{threadId}" so the
        // consumer can pair pentanomial by thread (each thread plays an opening then its reverse,
        // back-to-back). "r" marks it result-only (standings/pentanomial, no board tile). Per-thread
        // "gameEnd" keeps the passed gameId (the board tile).
        let effectiveGameId =
            if getStr o "type" "" = "gameResult" then
                match getIntOpt o "threadId" with
                | Some tid when tid >= 0 -> sprintf "r%d" tid
                | _ -> gameId
            else gameId
        Some(emit source effectiveGameId (Update.EndOfGame r))
    | _ -> None

/// "tournamentEnd" -> EndOfTournament. gameId is global ("").
let mapTournamentEnd (source: string) (line: string) : string option =
    match parseObj line with
    | Some o when getStr o "type" "" = "tournamentEnd" ->
        let t = { Tournament.Empty with Name = getStr o "name" "" }
        Some(emit source "" (Update.EndOfTournament t))
    | _ -> None
