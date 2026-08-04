module ChessLibrary.LiveFeedWire

// Wire codec for the Live Feed Contract (see LiveFeedContract.md).
//
// Serializes the internal `Update` event stream to/from a stable JSON wire format so an external
// tournament runner (or a recording/replay driver) can drive EB's live visualization. The wire
// format is deliberately decoupled from the internal F# types: explicit `type` tag, camelCase
// fields, no reliance on F# DU auto-serialization.
//
// The codec is a (lossy) projection: only contract-carried fields survive a round-trip. The
// authoritative property tested in TestProject is JSON idempotence:
//     serializeUpdate (tryParseUpdate (serializeUpdate u) |> Option.get) = serializeUpdate u
// i.e. parsing recovers everything serialization emits.

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes

// ---------------------------------------------------------------------------------------------
// Low-level helpers
// ---------------------------------------------------------------------------------------------

let private inv = System.Globalization.CultureInfo.InvariantCulture

/// Options used only for the embedded full `Tournament` config (reuses its attribute-based
/// converters plus the time-control strategy converter).
let private tournamentOpts =
    let o = JsonSerializerOptions(AllowTrailingCommas = true)
    o.Converters.Add(TimeControlStrategyConverter())
    o

let private js (s: string) : JsonNode = (JsonValue.Create(if isNull s then "" else s) :> JsonNode)
let private ji (i: int) : JsonNode = (JsonValue.Create(i) :> JsonNode)
let private jl (l: int64) : JsonNode = (JsonValue.Create(l) :> JsonNode)
let private jf (f: float) : JsonNode = (JsonValue.Create(f) :> JsonNode)
let private jb (x: bool) : JsonNode = (JsonValue.Create(x) :> JsonNode)

let private jobj (pairs: (string * JsonNode) list) : JsonNode =
    let o = JsonObject()
    for (k, v) in pairs do
        o[k] <- v
    o :> JsonNode

// Tolerant readers. Nodes always originate from JsonNode.Parse (JsonElement-backed), so numeric
// GetValue<_> conversions across int/long/double are safe.
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

let private asObj (n: JsonNode) : JsonObject option =
    match n with
    | :? JsonObject as o -> Some o
    | _ -> None

let private timeStr (t: TimeSpan) = TypesDef.CoreTypes.formatDuration t

let private parseTime (s: string) =
    if String.IsNullOrWhiteSpace s then
        TimeSpan.Zero
    else
        // Same two shapes as before, hh:mm:ss with or without the fraction. A feed is not a
        // place to reject a peer over formatting, so anything unreadable stays zero rather
        // than throwing the way the config parser does.
        try TypesDef.CoreTypes.parseDuration s with _ -> TimeSpan.Zero

let private spanStr (ts: TimeSpan) = ts.ToString("c")

let private parseSpan (s: string) =
    match TimeSpan.TryParse(s, inv) with
    | true, ts -> ts
    | _ -> TimeSpan.Zero

// ---------------------------------------------------------------------------------------------
// Value codecs
// ---------------------------------------------------------------------------------------------

let private evalToNode (e: EvalType) : JsonNode =
    match e with
    | CP v -> jobj [ "kind", js "cp"; "value", jf v ]
    | Mate m -> jobj [ "kind", js "mate"; "value", ji m ]
    | NA -> jobj [ "kind", js "na" ]

let private nodeToEval (n: JsonNode) : EvalType =
    match asObj n with
    | Some o ->
        match getStr o "kind" "na" with
        | "cp" -> CP(getF64 o "value" 0.0)
        | "mate" -> Mate(getInt o "value" 0)
        | _ -> NA
    | None -> NA

let private optEval (o: JsonObject) (k: string) =
    match o[k] with
    | null -> NA
    | n -> nodeToEval n

let private wdlToNode (w: WDLType) : JsonNode =
    match w with
    | HasValue v -> jobj [ "win", jf v.Win; "draw", jf v.Draw; "loss", jf v.Loss ]
    | NotFound -> null

let private nodeToWdl (o: JsonObject) (k: string) : WDLType =
    match o[k] with
    | :? JsonObject as w -> HasValue { Win = getF64 w "win" 0.0; Draw = getF64 w "draw" 0.0; Loss = getF64 w "loss" 0.0 }
    | _ -> NotFound

let private moveDetailToNode (m: MoveDetail) =
    jobj [ "longSan", js m.LongSan
           "fromSq", js m.FromSq
           "toSq", js m.ToSq
           "color", js m.Color
           "isCastling", jb m.IsCastling
           "comments", js m.Comments ]

let private nodeToMoveDetail (o: JsonObject) =
    { LongSan = getStr o "longSan" ""
      FromSq = getStr o "fromSq" ""
      ToSq = getStr o "toSq" ""
      Color = getStr o "color" ""
      IsCastling = getBool o "isCastling" false
      Comments = getStr o "comments" "" }

let private moveAndFenToNode (m: MoveAndFen) =
    jobj [ "shortSan", js m.ShortSan
           "fenAfterMove", js m.FenAfterMove
           "move", moveDetailToNode m.Move ]

let private nodeToMoveAndFen (o: JsonObject) =
    { Move = (match o["move"] with :? JsonObject as mo -> nodeToMoveDetail mo | _ -> MoveDetail.Empty)
      ShortSan = getStr o "shortSan" ""
      FenAfterMove = getStr o "fenAfterMove" startPosition }

/// EngineConfig reduced to the contract `player` projection (§2.5).
let private playerToNode (c: EngineConfig) =
    let optsNode = JsonObject()
    if not (isNull (box c.Options)) then
        for kv in c.Options do
            optsNode[kv.Key] <- js (if isNull kv.Value then "" else string kv.Value)
    jobj [ "name", js c.Name
           "version", js c.Version
           "dev", js c.Dev
           "logoPath", js c.LogoPath
           "alias", js c.Alias
           "rating", ji c.Rating
           "options", (optsNode :> JsonNode) ]

let private nodeToPlayer (o: JsonObject) =
    let d = Dictionary<string, obj>()
    match o["options"] with
    | :? JsonObject as oo ->
        for kv in oo do
            d[kv.Key] <- box (try kv.Value.GetValue<string>() with _ -> (if isNull kv.Value then "" else kv.Value.ToString()))
    | _ -> ()
    { EngineConfig.Empty with
        Name = getStr o "name" ""
        Version = getStr o "version" "Version"
        Dev = getStr o "dev" ""
        LogoPath = getStr o "logoPath" ""
        Alias = getStr o "alias" ""
        Rating = getInt o "rating" 3600
        Options = d }

let private statusFields (s: EngineStatus) =
    [ "playerName", js s.PlayerName
      "eval", evalToNode s.Eval
      "depth", ji s.Depth
      "sd", ji s.SD
      "nodes", jl s.Nodes
      "nps", jf s.NPS
      "eps", jf s.EPS
      "tbhits", jl s.TBhits
      "wdl", wdlToNode s.WDL
      "pv", js s.PV
      "pvLongSAN", js s.PVLongSAN
      "multiPV", ji s.MultiPV ]

let private statusToNode (s: EngineStatus) = jobj (statusFields s)

let private nodeToStatus (o: JsonObject) : EngineStatus =
    { PlayerName = getStr o "playerName" ""
      Eval = optEval o "eval"
      Nodes = getI64 o "nodes" 0L
      NPS = getF64 o "nps" 0.0
      EPS = getF64 o "eps" 0.0
      Depth = getInt o "depth" 0
      SD = getInt o "sd" 0
      TBhits = getI64 o "tbhits" 0L
      WDL = nodeToWdl o "wdl"
      PV = getStr o "pv" ""
      PVLongSAN = getStr o "pvLongSAN" ""
      MultiPV = getInt o "multiPV" 1 }

let private ponderFields (s: EnginePonderStatus) =
    [ "playerName", js s.PlayerName
      "eval", evalToNode s.Eval
      "depth", ji s.Depth
      "sd", ji s.SD
      "nodes", jl s.Nodes
      "nps", jf s.NPS
      "tbhits", jl s.TBhits
      "wdl", wdlToNode s.WDL ]

let private nodeToPonder (o: JsonObject) : EnginePonderStatus =
    { PlayerName = getStr o "playerName" ""
      Eval = optEval o "eval"
      Nodes = getI64 o "nodes" 0L
      NPS = getF64 o "nps" 0.0
      Depth = getInt o "depth" 0
      SD = getInt o "sd" 0
      TBhits = getI64 o "tbhits" 0L
      WDL = nodeToWdl o "wdl" }

let private bmiToNode (b: BestMoveInfo) =
    jobj [ "player", js b.Player
           "move", js b.Move
           "ponder", js b.Ponder
           "eval", evalToNode b.Eval
           "timeLeft", js (timeStr b.TimeLeft)
           "moveTime", js (timeStr b.MoveTime)
           "nodes", jl b.Nodes
           "nps", jf b.NPS
           "fen", js b.FEN
           "pv", js b.PV
           "longPV", js b.LongPV
           "moveAndFen", moveAndFenToNode b.MoveAndFen
           "moveHistory", js b.MoveHistory
           "move50", ji b.Move50
           "r3", ji b.R3
           "piecesLeft", ji b.PiecesLeft
           "adjDrawML", ji b.AdjDrawML ]

let private nodeToBmi (o: JsonObject) : BestMoveInfo =
    { Player = getStr o "player" ""
      Move = getStr o "move" ""
      Ponder = getStr o "ponder" ""
      Eval = optEval o "eval"
      TimeLeft = parseTime (getStr o "timeLeft" "")
      MoveTime = parseTime (getStr o "moveTime" "")
      Nodes = getI64 o "nodes" 0L
      NPS = getF64 o "nps" 0.0
      FEN = getStr o "fen" startPosition
      PV = getStr o "pv" ""
      LongPV = getStr o "longPV" ""
      MoveAndFen = (match o["moveAndFen"] with :? JsonObject as mo -> nodeToMoveAndFen mo | _ -> MoveAndFen.FirstEntry)
      MoveHistory = getStr o "moveHistory" ""
      Move50 = getInt o "move50" 0
      R3 = getInt o "r3" 0
      PiecesLeft = getInt o "piecesLeft" 32
      AdjDrawML = getInt o "adjDrawML" 0 }

let private evalListToNode (es: EvalType list) =
    let a = JsonArray()
    for e in es do
        a.Add(evalToNode e)
    a :> JsonNode

let private nodeToEvalList (n: JsonNode) =
    match n with
    | :? JsonArray as a -> [ for x in a -> nodeToEval x ]
    | _ -> []

let private resultToNode (r: Result) =
    jobj [ "player1", js r.Player1
           "player2", js r.Player2
           "result", js r.Result
           "reason", js (r.Reason.ToString())
           "moves", ji r.Moves
           "gameTime", jl r.GameTime
           "outOfOpeningEvals", evalListToNode r.OutOfOpeningEvals ]

let private nodeToResult (o: JsonObject) : Result =
    { Player1 = getStr o "player1" ""
      Player2 = getStr o "player2" ""
      Moves = getInt o "moves" 0
      Result = getStr o "result" "1/2-1/2"
      Reason = (try stringToResultReason (getStr o "reason" "NS") with _ -> ResultReason.NotStarted)
      GameTime = getI64 o "gameTime" 0L
      OutOfOpeningEvals = (match o["outOfOpeningEvals"] with null -> [] | n -> nodeToEvalList n) }

let private nnToNode (v: NNValues) =
    jobj [ "player", js v.Player
           "sanMove", js v.SANMove
           "lanMove", js v.LANMove
           "nodes", jl v.Nodes
           "p", jf v.P
           "q", jf v.Q
           "v", jf v.V
           "e", jf v.E
           "raw", js v.Raw ]

let private nodeToNN (o: JsonObject) : NNValues =
    { NNValues.Empty with
        Player = getStr o "player" ""
        SANMove = getStr o "sanMove" ""
        LANMove = getStr o "lanMove" ""
        Nodes = getI64 o "nodes" 0L
        P = getF64 o "p" 0.0
        Q = getF64 o "q" 0.0
        V = getF64 o "v" 0.0
        E = getF64 o "e" 0.0
        Raw = getStr o "raw" "" }

let private pairingToNode (p: Pairing) =
    jobj [ "white", playerToNode p.White
           "black", playerToNode p.Black
           "gameNr", ji p.GameNr
           "roundNr", js p.RoundNr
           "openingName", js p.Opening.GameMetaData.OpeningName
           "openingHash", js p.OpeningHash ]

let private nodeToPairing (o: JsonObject) : Pairing =
    let gameNr = getInt o "gameNr" 0
    let openingName = getStr o "openingName" ""
    let openingHash = getStr o "openingHash" ""
    let pgn =
        { PgnGame.Empty gameNr with
            GameMetaData = { GameMetadata.Empty with OpeningName = openingName; OpeningHash = openingHash } }
    { Opening = pgn
      White = (match o["white"] with :? JsonObject as w -> nodeToPlayer w | _ -> EngineConfig.Empty)
      Black = (match o["black"] with :? JsonObject as b -> nodeToPlayer b | _ -> EngineConfig.Empty)
      GameNr = gameNr
      RoundNr = getStr o "roundNr" ""
      OpeningHash = openingHash }

let private mafArrToNode (xs: ResizeArray<MoveAndFen>) =
    let a = JsonArray()
    for m in xs do
        a.Add(moveAndFenToNode m)
    a :> JsonNode

let private nodeToMafArr (n: JsonNode) =
    let r = ResizeArray<MoveAndFen>()
    match n with
    | :? JsonArray as a ->
        for x in a do
            match x with
            | :? JsonObject as xo -> r.Add(nodeToMoveAndFen xo)
            | _ -> ()
    | _ -> ()
    r

let private sgiFields (g: StartGameInfo) =
    [ "whitePlayer", playerToNode g.WhitePlayer
      "blackPlayer", playerToNode g.BlackPlayer
      "startPos", js g.StartPos
      "openingMovesAndFen", mafArrToNode g.OpeningMovesAndFen
      "whiteTime", js (timeStr g.WhiteTime)
      "blackTime", js (timeStr g.BlackTime)
      "whiteToMove", jb g.WhiteToMove
      "openingName", js g.OpeningName
      "currentGameNr", ji g.CurrentGameNr
      "openingHash", js g.OpeningHash ]

let private nodeToSgi (o: JsonObject) : StartGameInfo =
    { WhitePlayer = (match o["whitePlayer"] with :? JsonObject as w -> nodeToPlayer w | _ -> EngineConfig.Empty)
      BlackPlayer = (match o["blackPlayer"] with :? JsonObject as b -> nodeToPlayer b | _ -> EngineConfig.Empty)
      StartPos = getStr o "startPos" startPosition
      OpeningMovesAndFen = (match o["openingMovesAndFen"] with null -> ResizeArray() | n -> nodeToMafArr n)
      WhiteTime = parseTime (getStr o "whiteTime" "")
      BlackTime = parseTime (getStr o "blackTime" "")
      WhiteToMove = getBool o "whiteToMove" true
      OpeningName = getStr o "openingName" ""
      CurrentGameNr = getInt o "currentGameNr" 0
      OpeningHash = getStr o "openingHash" "" }

let private sotFields (i: StartOfTournamentInfo) =
    [ "numberOfGames", ji i.NumberOfGames
      "gameDurationSec", js (spanStr i.GameDurationInSec)
      "tournamentDurationSec", js (spanStr i.TournamentDurationSec)
      "tournament", (match i.Tournament with
                     | Some t -> JsonSerializer.SerializeToNode(t, tournamentOpts)
                     | None -> null) ]

let private nodeToSot (o: JsonObject) : StartOfTournamentInfo =
    { NumberOfGames = getInt o "numberOfGames" 0
      GameDurationInSec = parseSpan (getStr o "gameDurationSec" "")
      TournamentDurationSec = parseSpan (getStr o "tournamentDurationSec" "")
      Tournament =
        (match o["tournament"] with
         | null -> None
         | n -> (try Some(JsonSerializer.Deserialize<Tournament>(n, tournamentOpts)) with _ -> None)) }

// ---------------------------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------------------------

/// Serialize an `Update` to the JSON wire format. Total over the DU.
let serializeUpdate (u: Update) : string =
    let o =
        match u with
        | Update.GameStarted w -> jobj [ "type", js "GameStarted"; "player", js w ]
        | Update.EndOfGame r -> jobj [ "type", js "EndOfGame"; "result", resultToNode r ]
        | Update.BestMove (info, status) ->
            jobj [ "type", js "BestMove"; "info", bmiToNode info; "status", statusToNode status ]
        | Update.Info (p, i) -> jobj [ "type", js "Info"; "player", js p; "info", js i ]
        | Update.Eval (p, e) -> jobj [ "type", js "Eval"; "player", js p; "eval", evalToNode e ]
        | Update.Status s -> jobj (("type", js "Status") :: statusFields s)
        | Update.PonderStatus s -> jobj (("type", js "PonderStatus") :: ponderFields s)
        | Update.Time (p, t) -> jobj [ "type", js "Time"; "player", js p; "time", js (timeStr t) ]
        | Update.NNSeq seq ->
            let a = JsonArray()
            for v in seq do
                a.Add(nnToNode v)
            jobj [ "type", js "NNSeq"; "moves", (a :> JsonNode) ]
        | Update.StartOfGame g -> jobj (("type", js "StartOfGame") :: sgiFields g)
        | Update.EndOfTournament t ->
            jobj [ "type", js "EndOfTournament"; "tournament", JsonSerializer.SerializeToNode(t, tournamentOpts) ]
        | Update.StartOfTournament i -> jobj (("type", js "StartOfTournament") :: sotFields i)
        | Update.MessagesFromEngine (p, m) ->
            jobj [ "type", js "MessagesFromEngine"; "player", js p; "message", js m ]
        | Update.PairingList ps ->
            let a = JsonArray()
            for p in ps do
                a.Add(pairingToNode p)
            jobj [ "type", js "PairingList"; "pairings", (a :> JsonNode) ]
        | Update.TotalNumberOfPairs n -> jobj [ "type", js "TotalNumberOfPairs"; "count", ji n ]
        | Update.RoundNr r -> jobj [ "type", js "RoundNr"; "round", js r ]
        | Update.PeriodicResults rs ->
            let a = JsonArray()
            for r in rs do
                a.Add(resultToNode r)
            jobj [ "type", js "PeriodicResults"; "results", (a :> JsonNode) ]
        | Update.CupBracketUpdated -> jobj [ "type", js "CupBracketUpdated" ]
        | Update.SwissStateUpdated -> jobj [ "type", js "SwissStateUpdated" ]
        | Update.LadderStateUpdated -> jobj [ "type", js "LadderStateUpdated" ]
        | Update.GameSummary s -> jobj [ "type", js "GameSummary"; "summary", js s ]
    o.ToJsonString()

/// Parse a JSON wire event back to an `Update`. Returns None on malformed input or unknown type.
let tryParseUpdate (json: string) : Update option =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o ->
            match getStr o "type" "" with
            | "GameStarted" -> Some(Update.GameStarted(getStr o "player" ""))
            | "EndOfGame" ->
                match o["result"] with
                | :? JsonObject as r -> Some(Update.EndOfGame(nodeToResult r))
                | _ -> None
            | "BestMove" ->
                match o["info"], o["status"] with
                | (:? JsonObject as i), (:? JsonObject as s) -> Some(Update.BestMove(nodeToBmi i, nodeToStatus s))
                | _ -> None
            | "Info" -> Some(Update.Info(getStr o "player" "", getStr o "info" ""))
            | "Eval" -> Some(Update.Eval(getStr o "player" "", optEval o "eval"))
            | "Status" -> Some(Update.Status(nodeToStatus o))
            | "PonderStatus" -> Some(Update.PonderStatus(nodeToPonder o))
            | "Time" -> Some(Update.Time(getStr o "player" "", parseTime (getStr o "time" "")))
            | "NNSeq" ->
                let r = ResizeArray<NNValues>()
                match o["moves"] with
                | :? JsonArray as a ->
                    for x in a do
                        match x with
                        | :? JsonObject as xo -> r.Add(nodeToNN xo)
                        | _ -> ()
                | _ -> ()
                Some(Update.NNSeq r)
            | "StartOfGame" -> Some(Update.StartOfGame(nodeToSgi o))
            | "StartOfTournament" -> Some(Update.StartOfTournament(nodeToSot o))
            | "EndOfTournament" ->
                match o["tournament"] with
                | null -> None
                | n -> (try Some(Update.EndOfTournament(JsonSerializer.Deserialize<Tournament>(n, tournamentOpts))) with _ -> None)
            | "MessagesFromEngine" -> Some(Update.MessagesFromEngine(getStr o "player" "", getStr o "message" ""))
            | "PairingList" ->
                let r = ResizeArray<Pairing>()
                match o["pairings"] with
                | :? JsonArray as a ->
                    for x in a do
                        match x with
                        | :? JsonObject as xo -> r.Add(nodeToPairing xo)
                        | _ -> ()
                | _ -> ()
                Some(Update.PairingList r)
            | "TotalNumberOfPairs" -> Some(Update.TotalNumberOfPairs(getInt o "count" 0))
            | "RoundNr" -> Some(Update.RoundNr(getStr o "round" ""))
            | "PeriodicResults" ->
                let r = ResizeArray<Result>()
                match o["results"] with
                | :? JsonArray as a ->
                    for x in a do
                        match x with
                        | :? JsonObject as xo -> r.Add(nodeToResult xo)
                        | _ -> ()
                | _ -> ()
                Some(Update.PeriodicResults r)
            | "CupBracketUpdated" -> Some Update.CupBracketUpdated
            | "SwissStateUpdated" -> Some Update.SwissStateUpdated
            | "LadderStateUpdated" -> Some Update.LadderStateUpdated
            | "GameSummary" -> Some(Update.GameSummary(getStr o "summary" ""))
            | _ -> None
        | _ -> None
    with _ -> None

// ---------------------------------------------------------------------------------------------
// Envelope helpers (gameId / type) — for multi-game routing (LiveFeedContract.md §1, §8).
// gameId is envelope metadata, not part of the single-game `Update` type.
// ---------------------------------------------------------------------------------------------

/// Read the envelope `type` discriminator ("" if absent/malformed).
let readType (json: string) : string =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o -> getStr o "type" ""
        | _ -> ""
    with _ -> ""

/// Read the envelope `gameId` ("" if absent — treated as the single active game).
let readGameId (json: string) : string =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o -> getStr o "gameId" ""
        | _ -> ""
    with _ -> ""

/// Return the wire line with its `gameId` field set/overridden. Lets a replay driver tag
/// recordings (which have no gameId) so several games can be routed to separate views.
let withGameId (gameId: string) (json: string) : string =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o ->
            o["gameId"] <- js gameId
            o.ToJsonString()
        | _ -> json
    with _ -> json

/// Read the envelope `source` (producer/server id, "" if absent). Set by the ingest endpoint from
/// the connection, so games from different servers are distinguishable in one grid.
let readSource (json: string) : string =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o -> getStr o "source" ""
        | _ -> ""
    with _ -> ""

/// Return the wire line with its `source` field set/overridden.
let withSource (source: string) (json: string) : string =
    try
        match JsonNode.Parse(json) with
        | :? JsonObject as o ->
            o["source"] <- js source
            o.ToJsonString()
        | _ -> json
    with _ -> json
