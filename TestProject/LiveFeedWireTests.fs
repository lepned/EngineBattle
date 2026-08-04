module LiveFeedWireTests

open System
open System.Collections.Generic
open Xunit
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.LiveFeedWire

// ---------------------------------------------------------------------------------------------
// Sample values
// ---------------------------------------------------------------------------------------------

let private mkPlayer name =
    let d = Dictionary<string, obj>()
    d["Hash"] <- box "256"
    d["Threads"] <- box "8"
    { EngineConfig.Empty with
        Name = name
        Version = "1.2"
        Dev = "Dev " + name
        LogoPath = "Img/" + name + ".png"
        Alias = "Alias " + name
        Rating = 3500
        Options = d }

let private status =
    { EngineStatus.Empty with
        PlayerName = "A"
        Eval = CP 0.35
        Nodes = 45000000L
        NPS = 3000000.0
        EPS = 12.5
        Depth = 28
        SD = 35
        TBhits = 7L
        WDL = WDLType.HasValue { Win = 450.0; Draw = 500.0; Loss = 50.0 }
        PV = "c5 Nf3 d6"
        PVLongSAN = "c7c5 g1f3 d7d6"
        MultiPV = 1 }

let private ponder =
    { EnginePonderStatus.Empty with
        PlayerName = "A"
        Eval = CP 0.30
        Nodes = 35000000L
        NPS = 2800000.0
        Depth = 25
        SD = 30
        TBhits = 0L
        WDL = WDLType.HasValue { Win = 440.0; Draw = 510.0; Loss = 50.0 } }

let private maf =
    { Move = MoveDetail.Create("e2e4", "e2", "e4", "w", false)
      ShortSan = "e4"
      FenAfterMove = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" }

let private bmi =
    { BestMoveInfo.Empty with
        Player = "A"
        Move = "e2e4"
        Ponder = "c7c5"
        Eval = CP 0.35
        TimeLeft = TimeSpan(0, 4, 45)
        MoveTime = TimeSpan(0, 0, 15)
        Nodes = 45000000L
        NPS = 3000000.0
        FEN = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        PV = "c5 Nf3"
        LongPV = "c7c5 g1f3"
        MoveAndFen = maf
        MoveHistory = "1. e4"
        Move50 = 0
        R3 = 0
        PiecesLeft = 32
        AdjDrawML = 50 }

let private result =
    { Player1 = "A"
      Player2 = "B"
      Moves = 42
      Result = "1-0"
      Reason = ResultReason.Checkmate
      GameTime = 450000L
      OutOfOpeningEvals = [ CP 0.1; Mate 3; NA ] }

let private nn =
    { NNValues.Empty with
        Player = "A"
        SANMove = "e4"
        LANMove = "e2e4"
        Nodes = 1200000L
        P = 0.34
        Q = 0.18
        V = 0.20
        E = 0.0
        Raw = "raw" }

let private pairing =
    { Opening =
        { PgnGame.Empty 1 with
            GameMetaData = { GameMetadata.Empty with OpeningName = "Sicilian"; OpeningHash = "ab12" } }
      White = mkPlayer "A"
      Black = mkPlayer "B"
      GameNr = 1
      RoundNr = "1"
      OpeningHash = "ab12" }

let private sgi =
    { WhitePlayer = mkPlayer "A"
      BlackPlayer = mkPlayer "B"
      StartPos = startPosition
      OpeningMovesAndFen = ResizeArray([ maf ])
      WhiteTime = TimeSpan(0, 5, 0)
      BlackTime = TimeSpan(0, 5, 0)
      WhiteToMove = true
      OpeningName = "Sicilian"
      CurrentGameNr = 42
      OpeningHash = "ab12" }

/// Events that do NOT embed the full Tournament config — strict JSON-idempotence applies.
let private coreSamples: Update list =
    [ Update.GameStarted "A"
      Update.EndOfGame result
      Update.BestMove(bmi, status)
      Update.Info("A", "hello world")
      Update.Eval("A", CP 0.2)
      Update.Status status
      Update.PonderStatus ponder
      Update.Time("A", TimeSpan(0, 4, 30))
      Update.NNSeq(ResizeArray([ nn ]))
      Update.StartOfGame sgi
      Update.MessagesFromEngine("A", "loaded net")
      Update.PairingList(ResizeArray([ pairing ]))
      Update.TotalNumberOfPairs 100
      Update.RoundNr "5/10"
      Update.PeriodicResults(ResizeArray([ result ]))
      Update.CupBracketUpdated
      Update.SwissStateUpdated
      Update.LadderStateUpdated
      Update.GameSummary "summary text" ]

// ---------------------------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------------------------

[<Fact>]
let ``core Update events round-trip through the wire codec (JSON idempotence)`` () =
    for u in coreSamples do
        let json = serializeUpdate u
        match tryParseUpdate json with
        | Some u2 -> Assert.Equal(json, serializeUpdate u2)
        | None -> Assert.True(false, sprintf "failed to parse: %s" json)

[<Fact>]
let ``every serialized event carries a type and re-parses to a Some`` () =
    for u in coreSamples do
        let json = serializeUpdate u
        Assert.Contains("\"type\"", json)
        Assert.True((tryParseUpdate json).IsSome, sprintf "did not parse: %s" json)

[<Fact>]
let ``BestMove decodes its key fields`` () =
    match tryParseUpdate (serializeUpdate (Update.BestMove(bmi, status))) with
    | Some (Update.BestMove (i, s)) ->
        Assert.Equal("e2e4", i.Move)
        Assert.Equal(CP 0.35, i.Eval)
        Assert.Equal(bmi.FEN, i.FEN)
        Assert.Equal("1. e4", i.MoveHistory)
        Assert.Equal(TimeSpan(0, 4, 45), i.TimeLeft)
        Assert.Equal("e4", i.MoveAndFen.ShortSan)
        Assert.Equal(28, s.Depth)
        Assert.Equal(45000000L, s.Nodes)
        match s.WDL with
        | WDLType.HasValue w -> Assert.Equal(450.0, w.Win)
        | _ -> Assert.True(false, "WDL lost")
    | _ -> Assert.True(false, "expected a BestMove")

[<Fact>]
let ``StartOfGame decodes players and opening`` () =
    match tryParseUpdate (serializeUpdate (Update.StartOfGame sgi)) with
    | Some (Update.StartOfGame g) ->
        Assert.Equal("A", g.WhitePlayer.Name)
        Assert.Equal("B", g.BlackPlayer.Name)
        Assert.Equal("Sicilian", g.OpeningName)
        Assert.Equal(42, g.CurrentGameNr)
        Assert.True(g.WhiteToMove)
        Assert.Equal(1, g.OpeningMovesAndFen.Count)
        // the reduced player projection carries UCI options for the hover tooltip
        Assert.Equal("256", string g.WhitePlayer.Options["Hash"])
    | _ -> Assert.True(false, "expected a StartOfGame")

[<Fact>]
let ``EvalType variants round-trip`` () =
    let check e =
        match tryParseUpdate (serializeUpdate (Update.Eval("A", e))) with
        | Some (Update.Eval (_, e2)) -> Assert.Equal(e, e2)
        | _ -> Assert.True(false, "expected an Eval")
    check (CP 0.5)
    check (Mate 3)
    check (Mate -7)
    check NA

[<Fact>]
let ``WDL NotFound round-trips to NotFound`` () =
    let s = { status with WDL = WDLType.NotFound }
    match tryParseUpdate (serializeUpdate (Update.Status s)) with
    | Some (Update.Status s2) -> Assert.Equal(WDLType.NotFound, s2.WDL)
    | _ -> Assert.True(false, "expected a Status")

[<Fact>]
let ``EndOfGame preserves result and reason`` () =
    match tryParseUpdate (serializeUpdate (Update.EndOfGame result)) with
    | Some (Update.EndOfGame r) ->
        Assert.Equal("1-0", r.Result)
        Assert.Equal(ResultReason.Checkmate, r.Reason)
        Assert.Equal(42, r.Moves)
        Assert.Equal(450000L, r.GameTime)
        Assert.Equal(3, r.OutOfOpeningEvals.Length)
    | _ -> Assert.True(false, "expected an EndOfGame")

[<Fact>]
let ``StartOfTournament carries counts and an embedded tournament`` () =
    let sot =
        { NumberOfGames = 100
          GameDurationInSec = TimeSpan.FromSeconds 150.0
          TournamentDurationSec = TimeSpan.FromMinutes 250.0
          Tournament = Some Tournament.Empty }
    match tryParseUpdate (serializeUpdate (Update.StartOfTournament sot)) with
    | Some (Update.StartOfTournament i) ->
        Assert.Equal(100, i.NumberOfGames)
        Assert.Equal(TimeSpan.FromSeconds 150.0, i.GameDurationInSec)
        Assert.True(i.Tournament.IsSome)
    | _ -> Assert.True(false, "expected a StartOfTournament")

[<Fact>]
let ``EndOfTournament decodes to the EndOfTournament case`` () =
    match tryParseUpdate (serializeUpdate (Update.EndOfTournament Tournament.Empty)) with
    | Some (Update.EndOfTournament _) -> ()
    | _ -> Assert.True(false, "expected an EndOfTournament")

[<Fact>]
let ``malformed or unknown input returns None`` () =
    Assert.True((tryParseUpdate "{\"type\":\"NopeNotReal\"}").IsNone)
    Assert.True((tryParseUpdate "not json at all").IsNone)
    Assert.True((tryParseUpdate "[]").IsNone)
    Assert.True((tryParseUpdate "{}").IsNone)
    Assert.True((tryParseUpdate "null").IsNone)

[<Fact>]
let ``readType returns the event discriminator`` () =
    Assert.Equal("BestMove", readType (serializeUpdate (Update.BestMove(bmi, status))))
    Assert.Equal("Status", readType (serializeUpdate (Update.Status status)))
    Assert.Equal("", readType "not json")

[<Fact>]
let ``withGameId stamps a gameId that readGameId recovers`` () =
    let line = serializeUpdate (Update.Status status)
    Assert.Equal("", readGameId line) // recordings have no gameId
    let stamped = withGameId "game-3" line
    Assert.Equal("game-3", readGameId stamped)
    // overriding replaces, not duplicates
    let restamped = withGameId "game-7" stamped
    Assert.Equal("game-7", readGameId restamped)

[<Fact>]
let ``withGameId preserves the event payload`` () =
    let line = serializeUpdate (Update.BestMove(bmi, status))
    let stamped = withGameId "g1" line
    match tryParseUpdate stamped with
    | Some (Update.BestMove (i, _)) -> Assert.Equal("e2e4", i.Move)
    | _ -> Assert.True(false, "stamped line should still parse as BestMove")
