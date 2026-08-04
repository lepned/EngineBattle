module CeresWireTests

open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TournamentTypes
open ChessLibrary.LiveFeedWire
open ChessLibrary.CeresWire
open ChessLibrary.TypesDef.CoreTypes

// CELT/1.0 sample lines (camelCase, as the Ceres TournamentStreamPublisher emits them).

let private gameStartLine =
    """{"type":"gameStart","seq":1,"threadId":0,"gameSequenceNum":1,"openingIndex":0,"roundNumber":3,"whiteName":"Stockfish 18","blackName":"Ceres","initialFEN":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1","startFEN":"rnbqkbnr/pp1ppppp/2p5/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 2","openingUci":"e2e4 c7c6","openingName":"Caro-Kann","whiteToMove":true,"whiteTimeMs":60000,"blackTimeMs":60000}"""

let private moveLine =
    """{"type":"move","seq":2,"threadId":0,"ply":13,"side":"w","lan":"h4h5","fen":"rn1qkbnr/pp2ppp1/2p3bp/3pP2P/3P2P1/5P2/PPP5/RNBQKBNR b KQkq - 0 7","evalCp":105,"scoreQ":0.21,"moveTimeMs":3166,"timeLeftMs":58833,"nodes":13657466,"nps":4321982.0,"eps":0,"depth":23,"selDepth":46,"mAvg":12.3,"piecesLeft":32,"instamove":false,"wdl":{"w":0.547,"d":0.453,"l":0.0},"top":[{"lan":"h4h5","n":1000,"p":45.0,"q":0.21,"wl":0.21,"d":0.45}]}"""

let private interimCpLine =
    """{"type":"interim","seq":3,"threadId":0,"ply":13,"side":"b","evalCp":-30,"scoreQ":-0.06,"nodes":50000,"nps":100000.0,"eps":0,"depth":10,"selDepth":20,"mAvg":0,"moveTimeMs":500,"timeLeftMs":59000,"wdl":{"w":0.4,"d":0.5,"l":0.1}}"""

let private interimMateLine =
    """{"type":"interim","seq":4,"threadId":0,"ply":40,"side":"w","evalCp":3000,"mate":5,"scoreQ":0.99,"nodes":80000,"nps":160000.0,"eps":0,"depth":15,"selDepth":30,"moveTimeMs":600,"timeLeftMs":58000}"""

let private gameEndLine =
    """{"type":"gameEnd","seq":40,"threadId":0,"whiteName":"Stockfish 18","blackName":"Ceres","result":"1-0","reason":"CM","moves":37,"plyCount":73,"gameTimeMs":176296}"""

let private tournamentInfoLine =
    """{"type":"tournamentInfo","seq":0,"threadId":-1,"id":"T","name":"Ceres vs SF","machineName":"rig","mode":"RR","numGamePairs":10,"threadCount":2,"players":[{"name":"Stockfish 18","description":"SF"},{"name":"Ceres","description":"net"}],"timeControl":{"kind":"secondsForAllMoves","valueMs":60000,"incrementMs":1000}}"""

// ---------------------------------------------------------------------------------------------

[<Fact>]
let ``gameStart maps to StartOfGame with names, startPos, round, and envelope`` () =
    match mapGameStart "rigA" "0" gameStartLine with
    | Some(white, black, _startFen, json) ->
        Assert.Equal("Stockfish 18", white)
        Assert.Equal("Ceres", black)
        Assert.Equal("0", readGameId json)
        Assert.Equal("rigA", readSource json)
        match tryParseUpdate json with
        | Some(Update.StartOfGame g) ->
            Assert.Equal("Stockfish 18", g.WhitePlayer.Name)
            Assert.Equal("Ceres", g.BlackPlayer.Name)
            Assert.Equal(3, g.CurrentGameNr)
            Assert.True(g.WhiteToMove)
            Assert.StartsWith("rnbqkbnr/pp1ppppp", g.StartPos)
            Assert.Equal("00:01:00.000", formatDuration g.WhiteTime)
        | other -> failwithf "expected StartOfGame, got %A" other
    | None -> failwith "mapGameStart returned None"

[<Fact>]
let ``move maps to BestMove with eval/100, wdl*1000, SAN, envelope`` () =
    // position before h4h5 (white to move; pawn on h4)
    let beforeFen = "rn1qkbnr/pp2ppp1/2p3bp/3pP3/3P2PP/5P2/PPP5/RNBQKBNR w KQkq - 0 7"
    match mapMove "rigA" "0" "Stockfish 18" "Ceres" beforeFen "" moveLine with
    | Some(json, afterFen, history) ->
        Assert.Equal("0", readGameId json)
        Assert.Equal("rigA", readSource json)
        Assert.StartsWith("rn1qkbnr/pp2ppp1/2p3bp/3pP2P", afterFen)
        Assert.Equal("7. h5", history)                       // white move 7 -> "N. SAN" token
        match tryParseUpdate json with
        | Some(Update.BestMove(info, status)) ->
            Assert.Equal("Stockfish 18", info.Player)        // side "w" -> white
            Assert.Equal("h4h5", info.Move)                  // raw move stays UCI
            Assert.Equal("h5", info.MoveAndFen.ShortSan)     // UCI -> SAN
            Assert.Equal("7. h5", info.MoveHistory)          // running SAN list
            match info.Eval with
            | CP v -> Assert.Equal(1.05, v, 3)               // white move: 105 cp -> 1.05, no flip
            | other -> failwithf "expected CP, got %A" other
            Assert.Equal(13657466L, info.Nodes)
            Assert.Equal("00:00:58.833", formatDuration info.TimeLeft)
            Assert.Equal("h4", info.MoveAndFen.Move.FromSq)
            Assert.Equal("h5", info.MoveAndFen.Move.ToSq)
            Assert.Equal(23, status.Depth)
            Assert.Equal(46, status.SD)
            match status.WDL with
            | HasValue w ->
                Assert.Equal(547.0, w.Win, 1)                // 0.547 -> per-mille
                Assert.Equal(453.0, w.Draw, 1)
            | NotFound -> failwith "expected WDL"
        | other -> failwithf "expected BestMove, got %A" other
    | None -> failwith "mapMove returned None"

[<Fact>]
let ``black move eval is flipped to White perspective`` () =
    // 1...e5 (black to move after 1.e4); CELT evalCp +20 (good for black mover) -> White persp. -0.20
    let beforeFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    let blackMove = """{"type":"move","threadId":0,"ply":1,"side":"b","lan":"e7e5","fen":"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2","evalCp":20,"scoreQ":0.05,"moveTimeMs":900,"timeLeftMs":59000,"nodes":40000,"nps":44000.0,"eps":0,"depth":12,"selDepth":24,"piecesLeft":32,"wdl":{"w":0.30,"d":0.50,"l":0.20}}"""
    match mapMove "rigA" "0" "White" "Black" beforeFen "" blackMove with
    | Some(json, _, history) ->
        Assert.Equal("e5", history)                          // black move -> bare SAN (no number token)
        match tryParseUpdate json with
        | Some(Update.BestMove(info, status)) ->
            Assert.Equal("Black", info.Player)
            Assert.Equal("e5", info.MoveAndFen.ShortSan)
            match info.Eval with
            | CP v -> Assert.Equal(-0.20, v, 3)              // +20cp (black) -> -0.20 (white perspective)
            | other -> failwithf "expected CP, got %A" other
            match status.WDL with
            | HasValue w -> Assert.Equal(200.0, w.Win, 1)    // black win 0.20 -> white win 200 per-mille
            | NotFound -> failwith "expected WDL"
        | other -> failwithf "expected BestMove, got %A" other
    | None -> failwith "mapMove returned None"

[<Fact>]
let ``interim maps to Status (black eval flipped to White perspective)`` () =
    match mapInterim "rigB" "1" "Stockfish 18" "Ceres" "" interimCpLine with
    | Some json ->
        Assert.Equal("1", readGameId json)
        match tryParseUpdate json with
        | Some(Update.Status s) ->
            Assert.Equal("Ceres", s.PlayerName)              // side "b" -> black
            match s.Eval with
            | CP v -> Assert.Equal(0.30, v, 3)               // -30cp (black) -> +0.30 (white perspective)
            | other -> failwithf "expected CP, got %A" other
            Assert.Equal(10, s.Depth)
        | other -> failwithf "expected Status, got %A" other
    | None -> failwith "mapInterim returned None"

[<Fact>]
let ``interim with mate maps eval to Mate`` () =
    match mapInterim "rigB" "0" "Stockfish 18" "Ceres" "" interimMateLine with
    | Some json ->
        match tryParseUpdate json with
        | Some(Update.Status s) ->
            match s.Eval with
            | Mate m -> Assert.Equal(5, m)
            | other -> failwithf "expected Mate, got %A" other
        | other -> failwithf "expected Status, got %A" other
    | None -> failwith "mapInterim returned None"

[<Fact>]
let ``interim pv (UCI) maps to PVLongSAN raw and numbered SAN PV`` () =
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let line = """{"type":"interim","threadId":0,"ply":1,"side":"w","evalCp":20,"nodes":1000,"nps":2000.0,"depth":8,"selDepth":16,"pv":"e2e4 e7e5 g1f3"}"""
    match mapInterim "rigB" "0" "SF" "Ceres" startFen line with
    | Some json ->
        match tryParseUpdate json with
        | Some(Update.Status s) ->
            Assert.Equal("e2e4 e7e5 g1f3", s.PVLongSAN)   // raw UCI line preserved (board playback)
            Assert.Contains("e4", s.PV)                    // converted to SAN for display
        | other -> failwithf "expected Status, got %A" other
    | None -> failwith "mapInterim returned None"

[<Fact>]
let ``gameEnd maps to EndOfGame with player1=white, player2=black`` () =
    match mapGameEnd "rigA" "0" gameEndLine with
    | Some json ->
        Assert.Equal("0", readGameId json)
        match tryParseUpdate json with
        | Some(Update.EndOfGame r) ->
            Assert.Equal("Stockfish 18", r.Player1)          // white
            Assert.Equal("Ceres", r.Player2)                 // black
            Assert.Equal("1-0", r.Result)
            Assert.Equal(37, r.Moves)
            Assert.Equal("CM", r.Reason.ToString())
        | other -> failwithf "expected EndOfGame, got %A" other
    | None -> failwith "mapGameEnd returned None"

[<Fact>]
let ``tournamentInfo maps to StartOfTournament with name and players`` () =
    match mapTournamentInfo "rigA" tournamentInfoLine with
    | Some json ->
        Assert.Equal("", readGameId json)                    // global
        match tryParseUpdate json with
        | Some(Update.StartOfTournament i) ->
            match i.Tournament with
            | Some t ->
                // Name/Mode survive the Tournament wire round-trip; EngineSetup.Engines is
                // [<JsonIgnore>] so it does NOT (the grid derives standings from results, not the
                // engine list, so this is by design).
                Assert.Equal("Ceres vs SF", t.Name)
                Assert.Equal("RR", t.TournamentMode)
                // TimeControl is now mapped from the CELT meta (secondsForAllMoves 60s+1s) and must
                // survive the wire round-trip (non-null + real text, not the null-guard "").
                Assert.False(obj.ReferenceEquals(box t.TimeControl, null))
                Assert.False(System.String.IsNullOrEmpty(t.TimeControlTextForPlayer 1))
            | None -> failwith "expected Some Tournament"
        | other -> failwithf "expected StartOfTournament, got %A" other
    | None -> failwith "mapTournamentInfo returned None"

[<Fact>]
let ``mapGameEnd tags global gameResult with r{threadId} for thread pairing`` () =
    // Global result carries the producing thread id -> gameId "r{threadId}" so the consumer pairs
    // pentanomial by thread (each thread plays a pair back-to-back). "r" marks it results-only (no tile).
    let gameResultLine = """{"type":"gameResult","threadId":1,"whiteName":"Stockfish 18","blackName":"Ceres","result":"1-0","reason":"CM","moves":37,"plyCount":73,"gameTimeMs":176296}"""
    match mapGameEnd "rigA" "" gameResultLine with
    | Some json ->
        Assert.Equal("r1", readGameId json)
        match tryParseUpdate json with
        | Some(Update.EndOfGame r) ->
            Assert.Equal("Stockfish 18", r.Player1)
            Assert.Equal("Ceres", r.Player2)
            Assert.Equal("1-0", r.Result)
        | other -> failwithf "expected EndOfGame, got %A" other
    | None -> failwith "mapGameEnd returned None for gameResult"

[<Fact>]
let ``mapGameEnd tags gameResult with r{threadId}~o{openingIndex} when opening id present`` () =
    // When Ceres sends openingIndex on the result, the consumer should pair pentanomial by opening
    // (robust to threads/parallelism/distribution), so the envelope carries "r{threadId}~o{openingIndex}".
    let gameResultLine = """{"type":"gameResult","threadId":1,"openingIndex":5,"whiteName":"Stockfish 18","blackName":"Ceres","result":"1-0","reason":"CM","moves":37,"plyCount":73,"gameTimeMs":176296}"""
    match mapGameEnd "rigA" "" gameResultLine with
    | Some json -> Assert.Equal("r1~o5", readGameId json)
    | None -> failwith "mapGameEnd returned None for gameResult"

// toTimeControl is private, so exercise it through mapTournamentInfo (+ the wire round-trip) and
// assert the resulting TimeControl config. tcLine builds a tournamentInfo line with a given TC.
let private tcLine (tcJson: string) =
    sprintf """{"type":"tournamentInfo","threadId":-1,"name":"T","mode":"RR","numGamePairs":1,"threadCount":1,"players":[{"name":"A"},{"name":"B"}],"timeControl":%s}""" tcJson

let private parseTC (line: string) =
    match mapTournamentInfo "rig" line with
    | Some json ->
        match tryParseUpdate json with
        | Some(Update.StartOfTournament i) ->
            match i.Tournament with
            | Some t -> t.TimeControl
            | None -> failwith "no tournament"
        | other -> failwithf "expected StartOfTournament, got %A" other
    | None -> failwith "mapTournamentInfo returned None"

[<Fact>]
let ``toTimeControl maps secondsForAllMoves to fixed + increment`` () =
    let cfg = (parseTC (tcLine """{"kind":"secondsForAllMoves","valueMs":60000,"incrementMs":1000}""")).GetTimeConfig 1
    Assert.False(cfg.NodeLimit)
    Assert.Equal(60.0, cfg.Fixed.TotalSeconds, 1)
    Assert.Equal(1.0, cfg.Increment.TotalSeconds, 1)

[<Fact>]
let ``toTimeControl maps secondsPerMove to 1s fixed + per-move increment`` () =
    let cfg = (parseTC (tcLine """{"kind":"secondsPerMove","valueMs":1500}""")).GetTimeConfig 1
    Assert.False(cfg.NodeLimit)
    Assert.Equal(1.0, cfg.Fixed.TotalSeconds, 2)        // 1s base
    Assert.Equal(1.5, cfg.Increment.TotalSeconds, 2)    // increment = per-move time

[<Fact>]
let ``toTimeControl maps nodesPerMove to a node limit`` () =
    let cfg = (parseTC (tcLine """{"kind":"nodesPerMove","nodes":800}""")).GetTimeConfig 1
    Assert.True(cfg.NodeLimit)
    Assert.Equal(800, cfg.Nodes)

[<Fact>]
let ``toTimeControl maps movesToGo`` () =
    let tc = parseTC (tcLine """{"kind":"secondsForAllMoves","valueMs":120000,"incrementMs":0,"movesToGo":40}""")
    Assert.Equal(40, tc.WmovesToGo)
    Assert.Equal(40, tc.BmovesToGo)

[<Fact>]
let ``celtType reads discriminator and mappers reject wrong type`` () =
    Assert.Equal("move", celtType moveLine)
    Assert.Equal("gameEnd", celtType gameEndLine)
    Assert.True((mapMove "s" "0" "W" "B" "" "" gameEndLine).IsNone) // wrong type -> None
    Assert.True((mapGameEnd "s" "0" moveLine).IsNone)
