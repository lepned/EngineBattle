namespace ChessLibrary

open System
open System.Text
open System.IO
open System.Text.RegularExpressions
open System.Text.Json.Serialization
open MiscTypes
open PGNTypes
open EngineTypes

module TypesDef =
  // -----------------------------------------------------------------------------
  // CORE TYPES
  // -----------------------------------------------------------------------------
  module CoreTypes =
    open System.Text.Json

    /// Time control strategy for Winboard engines
    /// - LevelWithTime: Standard V2 approach (send level + time/otim)
    /// - TimeOtimOnly: For V1 engines or those with broken level command (send time/otim only)
    /// - StWithTime: Safety mode (send st + time/otim for better time management)
    /// - StOnly: Legacy mode (send st only, may cause poor time management)
    /// - AutoDetect: Probe level command at runtime, fallback to TimeOtimOnly on error
    type TimeControlStrategy =
        | LevelWithTime
        | TimeOtimOnly
        | StWithTime
        | StOnly
        | AutoDetect

    /// JSON converter for TimeControlStrategy discriminated union
    type TimeControlStrategyConverter() =
        inherit JsonConverter<TimeControlStrategy>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            let value = reader.GetString()
            match value with
            | "LevelWithTime" -> TimeControlStrategy.LevelWithTime
            | "TimeOtimOnly" -> TimeControlStrategy.TimeOtimOnly
            | "StWithTime" -> TimeControlStrategy.StWithTime
            | "StOnly" -> TimeControlStrategy.StOnly
            | "AutoDetect" -> TimeControlStrategy.AutoDetect
            | _ -> failwith $"Unknown TimeControlStrategy: {value}"

        override _.Write(writer: Utf8JsonWriter, value: TimeControlStrategy, options: JsonSerializerOptions) =
            let str =
                match value with
                | TimeControlStrategy.LevelWithTime -> "LevelWithTime"
                | TimeControlStrategy.TimeOtimOnly -> "TimeOtimOnly"
                | TimeControlStrategy.StWithTime -> "StWithTime"
                | TimeControlStrategy.StOnly -> "StOnly"
                | TimeControlStrategy.AutoDetect -> "AutoDetect"
            writer.WriteStringValue(str)

    // -----------------------------------------------------------------------------
    // DURATIONS
    // -----------------------------------------------------------------------------
    //
    // Clocks, increments, move overhead and delays are all *amounts of time*. .NET's own
    // parsing of "hh:mm:ss" rejects an hour field above 23 and a minute or second field
    // above 59, because those ranges describe a time of day. A duration has no such
    // ceilings, and users kept running into all three of them: 60 seconds, 60 minutes and
    // anything past a day.
    //
    // The format is exactly hh:mm:ss with an optional .fff, and every field is unbounded.
    // Days are deliberately not part of it — "30:00:00" says thirty hours more plainly than
    // TimeSpan's own "1.06:00:00", and one form is one thing to document and to test.

    /// Renders a duration as hh:mm:ss.fff with the hour field carrying everything above a
    /// day, so the output is always in the same shape the parser accepts.
    let formatDuration (v: TimeSpan) : string =
        let sign = if v < TimeSpan.Zero then "-" else ""
        let a = if v < TimeSpan.Zero then v.Negate() else v
        sprintf "%s%02d:%02d:%02d.%03d" sign (int a.TotalHours) a.Minutes a.Seconds a.Milliseconds

    /// Parses hh:mm:ss[.fff] with no upper bound on any field. Raises with a message that
    /// states the format when the input cannot be read.
    let parseDuration (raw: string) : TimeSpan =
        let s = if isNull raw then "" else raw.Trim()
        let fail () : TimeSpan =
            failwithf
                "Invalid duration '%s'. Expected hh:mm:ss or hh:mm:ss.fff. Every field may \
                 overflow — \"00:00:90\" is 90 seconds and \"30:00:00\" is 30 hours. All three \
                 fields are required, and days are not part of the format."
                s
        if s = "" then fail ()
        else
        // A dot before the first colon is TimeSpan's day prefix. Refusing it keeps the dot
        // meaning one thing only, and nothing can already be written that way: the old type
        // could not hold a day in the first place.
        let firstColon = s.IndexOf ':'
        let firstDot = s.IndexOf '.'
        if firstColon < 0 || (firstDot >= 0 && firstDot < firstColon) then fail ()
        else
        let parts = s.Split ':'
        // Three fields, always. "10:00" is the one input worth refusing outright: .NET reads
        // it as ten hours while someone writing a time control means ten minutes, and either
        // guess is a silently wrong game.
        if parts.Length <> 3 then fail ()
        else
        let inv = Globalization.CultureInfo.InvariantCulture
        let secText, fracText =
            let sec = parts.[2]
            let dot = sec.IndexOf '.'
            if dot < 0 then sec, "" else sec.Substring(0, dot), sec.Substring(dot + 1)
        let ok, hours = Int64.TryParse(parts.[0], Globalization.NumberStyles.None, inv)
        let ok2, minutes = Int64.TryParse(parts.[1], Globalization.NumberStyles.None, inv)
        let ok3, seconds = Int64.TryParse(secText, Globalization.NumberStyles.None, inv)
        if not (ok && ok2 && ok3) then fail ()
        elif fracText <> "" && not (fracText |> Seq.forall Char.IsDigit) then fail ()
        else
        // Fraction as ticks: pad or truncate to the 7 digits TimeSpan actually stores.
        let fracTicks =
            if fracText = "" then 0L
            else
                let padded = (fracText + String('0', 7)).Substring(0, 7)
                match Int64.TryParse(padded, Globalization.NumberStyles.None, inv) with
                | true, v -> v
                | _ -> 0L
        TimeSpan.FromTicks(
            hours * TimeSpan.TicksPerHour
            + minutes * TimeSpan.TicksPerMinute
            + seconds * TimeSpan.TicksPerSecond
            + fracTicks)

    /// JSON converter for durations. Reading is tolerant of overflowing fields; writing
    /// normalises, so "00:00:90" is stored back as "00:01:30" and a file always states what
    /// the value actually became.
    type DurationConverter() =
        inherit JsonConverter<TimeSpan>()

        override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: Type, _options: JsonSerializerOptions) =
            parseDuration (reader.GetString())

        override _.Write(writer: Utf8JsonWriter, value: TimeSpan, _options: JsonSerializerOptions) =
            writer.WriteStringValue(formatDuration value)

    /// Move annotation detail level for PGN output
    [<RequireQualifiedAccess>]
    type MoveAnnotation =
        | Off
        | Minimal
        | Standard
        | Full

    /// JSON converter for MoveAnnotation with backward-compat bool reading (true→Full, false→Standard)
    type MoveAnnotationConverter() =
        inherit JsonConverter<MoveAnnotation>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            match reader.TokenType with
            | JsonTokenType.True -> MoveAnnotation.Full
            | JsonTokenType.False -> MoveAnnotation.Standard
            | JsonTokenType.String ->
                match reader.GetString().ToLowerInvariant() with
                | "none" | "off" -> MoveAnnotation.Off
                | "minimal" -> MoveAnnotation.Minimal
                | "standard" -> MoveAnnotation.Standard
                | "full" -> MoveAnnotation.Full
                | v -> failwith $"Unknown MoveAnnotation: {v}"
            | t -> failwith $"Unexpected token for MoveAnnotation: {t}"

        override _.Write(writer: Utf8JsonWriter, value: MoveAnnotation, options: JsonSerializerOptions) =
            let str =
                match value with
                | MoveAnnotation.Off -> "None"
                | MoveAnnotation.Minimal -> "Minimal"
                | MoveAnnotation.Standard -> "Standard"
                | MoveAnnotation.Full -> "Full"
            writer.WriteStringValue(str)

    /// Winboard-specific configuration options
    type WinboardConfig = {
        /// If true, engine reports scores from side-to-move's perspective (requires negation for Black)
        SideToMovePOV: bool
        /// Time control strategy to use for this engine
        TimeControlStrategy: TimeControlStrategy
        /// Commands to send after 'post' and 'easy' during initialization (e.g., ["level 16"])
        StartupCommands: string list
        /// If true, force V1 mode (skip protover 2 negotiation, use conservative defaults)
        ForceV1Mode: bool
        /// If true, send a dummy 'level' command at startup to enable thinking output (for engines like Comet)
        RequiresLevelForThinkingOutput: bool
        /// If true, send 4-field FEN (omit halfmove clock and fullmove number) for old engines that crash/hang on 6-field FEN
        Use4FieldFen: bool
        /// Delay in milliseconds before sending 'go' command (workaround for time control race conditions)
        /// Default is 100ms. Set to 0 for engines that don't need it.
        PreGoDelayMs: int
    }
    with
        static member Default = {
            SideToMovePOV = false
            TimeControlStrategy = LevelWithTime
            StartupCommands = []
            ForceV1Mode = false
            RequiresLevelForThinkingOutput = false
            Use4FieldFen = false
            PreGoDelayMs = 100
        }

    type EngineConfig =
        { mutable Name: string
          Alias: string
          TimeControlID: int
          Version: string
          Rating: int
          Dev: string
          LogoPath: string
          mutable IsChallenger: bool
          ContemptEnabled: bool
          NegativeContemptAllowed: bool
          Protocol: string
          Path: string
          mutable NetworkPath: string
          Args: string
          Options: System.Collections.Generic.Dictionary<string, obj>
          WinboardConfig: WinboardConfig option
          DeviceOption: string
          DeviceTemplate: string }
        with
            member x.Information moveOverhead =
              let sb = new StringBuilder()
              sb.Append(sprintf "Protocol=%s;" x.Protocol) |> ignore
              sb.Append(sprintf " MoveOverheadMS=%.0f;" moveOverhead) |> ignore
              for opt in x.Options do
                sb.Append(sprintf " %s=%s;" opt.Key (opt.Value.ToString())) |> ignore
              sb.ToString()
            static member Empty =
              { Name = ""
                Alias = ""
                TimeControlID = 0
                Version = "Version"
                Rating = 3600
                Dev = ""
                LogoPath = ""
                IsChallenger = false
                ContemptEnabled = false
                NegativeContemptAllowed = false
                Protocol = "UCI"
                Path = ""
                NetworkPath = ""
                Args = String.Empty
                Options = new System.Collections.Generic.Dictionary<string, obj>()
                WinboardConfig = None
                DeviceOption = ""
                DeviceTemplate = "" }
            static member EmptyWithPath (path:string) =
              let fileName = Path.GetFileNameWithoutExtension path
              let isLc0 = Regex.Match(fileName, "lc0", RegexOptions.IgnoreCase).Success
              { Name = fileName
                Alias = "Engine xx"
                TimeControlID = 1
                Version = "Version"
                Rating = 3600
                Dev = "from xxx"
                LogoPath = "Img/lc0.png"
                IsChallenger = false
                ContemptEnabled = false
                NegativeContemptAllowed = false
                Protocol = "UCI"
                Path = path
                NetworkPath = ""
                Args = if isLc0 then "--show-hidden" else String.Empty
                Options = new System.Collections.Generic.Dictionary<string, obj>()
                WinboardConfig = None
                DeviceOption = ""
                DeviceTemplate = "" }
            static member AddOptions (config: EngineConfig) options =
              { config with Options = options }

    type Pairing =
      { Opening: PgnGame
        White: EngineConfig
        Black: EngineConfig
        GameNr : int
        RoundNr: string
        OpeningHash : string}

    type WDLStats = { Wins: int; Draws: int; Losses: int }

    type CrossTableEntry = {
        Player : string
        Alias : string
        Rank : int
        mutable Challenger : bool
        StatsAgainst : (string * WDLStats) seq  // (Opponent, Stats)
        ResultsAgainst: (string * string array) array  // Opponent * List of results
        TotalScore : float
        Eff : float }

    type SearchData =
      { Player: string
        GameNr: int
        Navg: float
        N1avg: float
        N2avg: float
        Q1: float
        Q2: float
        FractN1N: float
        FractN2N: float
        FractN2N1: float
        MoveTimeMs : int64
        TimeLeftMs : int64
        TopPMovePercent: float }
      with
        static member Empty =
          { Player = ""
            GameNr = 0
            Navg = 0.0
            N1avg = 0.0
            N2avg = 0.0
            Q1 = 0.0
            Q2 = 0.0
            FractN1N = 0.0
            FractN2N = 0.0
            FractN2N1 = 0.0
            MoveTimeMs = 0
            TimeLeftMs = 0
            TopPMovePercent = 0.0 }

    type MoveDeviation =
      { Round: string
        GameNr : int
        MoveNr: int
        Color: string
        PrevSanMove: string*string
        PlayerToDeviate: string
        Opponent: string
        DevSanMove: string*string
        Result: string
        DevRes: string
        PgnGamePair: PgnGame * PgnGame
        PrevFen: string
        DevFen: string}
      with
        static member Empty =
          { Round = ""
            GameNr = 0
            Color = ""
            MoveNr = 0
            PrevSanMove = "",""
            PlayerToDeviate = ""
            Opponent = ""
            DevSanMove = "",""
            Result = ""
            DevRes = ""
            PgnGamePair = PgnGame.Empty 0, PgnGame.Empty 0
            PrevFen = String.Empty
            DevFen = String.Empty}

    type SummaryEngineStat =
      { Player: string; Median: bool; Games: int; EPS: float; AvgNPS: float; AvgNodes: float; AvgDepth: float; AvgSelfDepth: float; Time: int64 }

    type PieceCountStat =
      { Player: string
        PieceCount: int
        AvgEps: float
        AvgNps: float }
      with
        static member Empty =
          { Player = "NA"
            PieceCount = 0
            AvgEps = 0.0
            AvgNps = 0.0 }

    type EngineStatsPerGame =
      { Player: string
        GameNr: int
        AvgEPS: float
        MedianEPS: float
        AvgNps: float
        MedianNps: float
        AvgNodes: float
        MedianNodes: float
        AvgDepth: float
        MedianDepth: float
        AvgSD: float
        MedianSD: float }
      with
        static member Empty =
          { Player = "NA"
            GameNr = 0
            AvgEPS = 0.0
            MedianEPS = 0.0
            AvgNps = 0.0
            MedianNps = 0.0
            AvgNodes = 0.0
            MedianNodes = 0.0
            AvgDepth = 0.0
            MedianDepth = 0.0
            AvgSD = 0.0
            MedianSD = 0.0 }

    type EnginePonderStatus =
      { mutable PlayerName: string
        mutable Eval: EvalType
        Nodes: int64
        NPS: float
        Depth: int
        SD: int
        TBhits: int64
        WDL: WDLType }
      with
        static member Empty =
          { PlayerName = ""
            Eval = EvalType.NA
            Nodes = 0L
            NPS = 0.0
            Depth = 0
            SD = 0
            TBhits = 0L
            WDL = WDLType.NotFound }
        static member Create playerName eval nodes nps depth sd tbhits wdl =
          { PlayerName = playerName
            Eval = eval
            Nodes = nodes
            NPS = nps
            Depth = depth
            SD = sd
            TBhits = tbhits
            WDL = wdl }

    type Result =
      { Player1: string
        Player2: string
        Moves: int
        Result: string
        Reason: ResultReason
        GameTime: int64
        OutOfOpeningEvals: EvalType list }
        override x.ToString() =
          let time = float x.GameTime / 1000.0
          sprintf "%s vs %s: %s (%s), %d moves, %.1f seconds" x.Player1 x.Player2 x.Result (x.Reason.Explanation) x.Moves time
        static member Empty =
          { Player1 = "White"; Player2 = "Black"; Moves = 0; Result = "1/2-1/2"; Reason = ResultReason.NotStarted; GameTime = 0L; OutOfOpeningEvals = [] }
    let createResult p1 p2 (moves: ResizeArray<string>) result reason gameTime =
      let moveCount = if moves.Count % 2 = 0 then moves.Count / 2 else (moves.Count / 2) + 1
      { Player1 = p1; Player2 = p2; Moves = moveCount; Result = result; Reason = reason; GameTime = gameTime; OutOfOpeningEvals = []}
    let createResultWithEval p1 p2 (moves: ResizeArray<string>) result reason gameTime evals =
      let moveCount = if moves.Count % 2 = 0 then moves.Count / 2 else (moves.Count / 2) + 1
      { Player1 = p1; Player2 = p2; Moves = moveCount; Result = result; Reason = reason; GameTime = gameTime; OutOfOpeningEvals = evals}


    type PlayerResult =
      { Player: string
        mutable Points: float
        mutable Elo: float
        mutable Error: float
        Played: int
        Percent: int
        CFS: int
        Win: int
        Draw: int
        Loss: int
        D: int
        WhiteWDL: (int * int * int)
        BlackWDL: (int * int * int)
        mutable PairWins: int
        mutable PairLosses: int
        mutable MedSpeed: double
        mutable AvgNPM: double
        mutable EPS: double
        mutable Challenger: bool }
        member this.WhiteScore =
          let ww, wd, _ = this.WhiteWDL
          float ww + float wd * 0.5
        member this.BlackScore =
          let bw, bd, _ = this.BlackWDL
          float bw + float bd * 0.5
        member this.PairsString = sprintf "%d-%d" this.PairWins this.PairLosses
        override this.ToString() =
          sprintf "%s: %.1f (%d) %d %d %d %d %.1f %.1f %d" this.Player this.Points this.Played this.Win this.Draw this.Loss this.CFS this.Error this.Elo this.Percent

    let createPlayerResult player points score error played percent cfs win draw loss d white black pairWins pairLosses =
      { Player = player
        Points = points
        Elo = score
        Error = error
        Played = played
        Percent = percent
        CFS = cfs
        Win = win
        Draw = draw
        Loss = loss
        D = d
        WhiteWDL = white
        BlackWDL = black
        PairWins = pairWins
        PairLosses = pairLosses
        MedSpeed = 0.0
        AvgNPM = 0.0
        EPS = 0.0
        Challenger = false }

    type Outcome =
        | Win of string
        | Loss of string
        | Draw
        | NotPlayed
        override this.ToString() =
          match this with
          | Win s -> s
          | Loss s -> s
          | Draw -> "Draw"
          | NotPlayed -> "Not Played"

    type StartGameInfo =
      {
        WhitePlayer:EngineConfig
        BlackPlayer:EngineConfig
        StartPos:string
        OpeningMovesAndFen: ResizeArray<MoveAndFen>
        WhiteTime : TimeSpan
        BlackTime : TimeSpan
        WhiteToMove : bool
        OpeningName : string
        CurrentGameNr : int
        OpeningHash : string
      }
      override this.ToString() =
        sprintf "Start of game number %d: %s vs %s" this.CurrentGameNr this.WhitePlayer.Name this.BlackPlayer.Name

    type PolicyRankInfo =
        {
            QRank: int
            PolicyRank: int
            BestMove: NNValues
            PlayedMove: NNValues
            IsWhite: bool
            MoveNumber: int
        }
        with
            static member Create(qRank: int, policyRank: int, bestMove: NNValues, playedMove: NNValues, isWhite: bool, moveNumber: int) =
                {
                    QRank = qRank
                    PolicyRank = policyRank
                    BestMove = bestMove
                    PlayedMove = playedMove
                    IsWhite = isWhite
                    MoveNumber = moveNumber
                }

  // -----------------------------------------------------------------------------
  // LAYOUT & TOURNAMENT DETAILS
  // -----------------------------------------------------------------------------

  // Tournament details, including methods for summarizing configuration.
  module Tournament =
    open System.Text.Json
    open LayoutTypes
    open TimeControlTypes

    type DrawOption = { DrawMoveLength: int; MaxDrawScore: float; MinDrawMove: int }
    type WinOption = { MinWinMove: int; MinWinScore: float; WinMoveLength: int }
    type TableBaseAdjudication = {TablebaseDirectory:string; UseTBAdjudication: bool; TBMen: int }
    type Adjudication = { DrawOption: DrawOption; WinOption: WinOption; TBAdj: TableBaseAdjudication }
    type Opening = { OpeningsPath: string option; OpeningsTwice: bool; OpeningsPly: int; RandomOpenings: bool; Seed: int }
    type CupOptions =
      { RoundPairIncrements: int list
        SeedingStrategy: string
        UniquePerMatchOnly: bool
        BracketPath: string
        RandomOpenings: bool }
    type SwissOptions =
      { GamesPerMatch: int
        Rounds: int
        SeedGroupCount: int
        UniquePerMatchOnly: bool
        RandomOpenings: bool
        AllowExtraPairsOnTie: bool
        StatePath: string }
    type LadderOptions =
      { GamePairsPerMatch: int
        RandomOpenings: bool
        StatePath: string }
    type EngineSetup =
      { [<JsonIgnore>] mutable Engines: CoreTypes.EngineConfig list
        EngineDefFolder: string
        EngineDefList: string list }
    // NumberOfGamesInParallel was called NumberOfGamesInParallelConsoleOnly before parallel
    // play reached the WebGUI; TestOptionsConverter below still accepts the old JSON name on load.
    type TestOptions = { PolicyTest: bool; ValueTest: bool; WriteToConsole: bool; NumberOfGamesInParallel: int; GPUs: int[] }

    /// JSON converter for TestOptions: reads NumberOfGamesInParallelConsoleOnly (pre-rename)
    /// as an alias for NumberOfGamesInParallel; always writes the new name.
    type TestOptionsConverter() =
        inherit JsonConverter<TestOptions>()

        override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: Type, _options: JsonSerializerOptions) =
            if reader.TokenType <> JsonTokenType.StartObject then
                failwith $"Unexpected token for TestOptions: {reader.TokenType}"
            let mutable policyTest = false
            let mutable valueTest = false
            let mutable writeToConsole = false
            let mutable parallelGames = 1
            let mutable gpus : int[] = null
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                let name = reader.GetString()
                reader.Read() |> ignore
                match name with
                | "PolicyTest" -> policyTest <- reader.GetBoolean()
                | "ValueTest" -> valueTest <- reader.GetBoolean()
                | "WriteToConsole" -> writeToConsole <- reader.GetBoolean()
                | "NumberOfGamesInParallel" | "NumberOfGamesInParallelConsoleOnly" ->
                    parallelGames <- reader.GetInt32()
                | "GPUs" ->
                    if reader.TokenType = JsonTokenType.Null then gpus <- null
                    else
                        let items = ResizeArray<int>()
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            items.Add(reader.GetInt32())
                        gpus <- items.ToArray()
                | _ -> reader.Skip()
            { PolicyTest = policyTest
              ValueTest = valueTest
              WriteToConsole = writeToConsole
              NumberOfGamesInParallel = parallelGames
              GPUs = gpus }

        override _.Write(writer: Utf8JsonWriter, value: TestOptions, _options: JsonSerializerOptions) =
            writer.WriteStartObject()
            writer.WriteBoolean("PolicyTest", value.PolicyTest)
            writer.WriteBoolean("ValueTest", value.ValueTest)
            writer.WriteBoolean("WriteToConsole", value.WriteToConsole)
            writer.WriteNumber("NumberOfGamesInParallel", value.NumberOfGamesInParallel)
            if isNull value.GPUs then
                writer.WriteNull("GPUs")
            else
                writer.WritePropertyName("GPUs")
                writer.WriteStartArray()
                for g in value.GPUs do writer.WriteNumberValue(g)
                writer.WriteEndArray()
            writer.WriteEndObject()

    /// Optional live-feed output for streaming a tournament to the WebGUI grid (see
    /// LiveFeedContract.md). All-empty / missing in JSON => no feed (normal tournament).
    /// Env vars EB_LIVEFEED_URL / _SOURCE / _FILE / _TOKEN override these at runtime.
    type LiveFeedConfig =
      { Url: string       // POST wire events here, e.g. http://host:5018/api/livefeed
        Source: string    // server label (X-Feed-Source); blank => WebGUI uses the remote IP
        File: string      // also write NDJSON to this file (record / local tail)
        Token: string }   // X-Feed-Token auth, if the WebGUI requires one
      with
        static member Empty = { Url = ""; Source = ""; File = ""; Token = "" }

    type Tournament =
      { Name: string
        Description: string
        OS: string
        CPU: string
        RAM: string
        GPU: string
        MainLogoFileName: string
        ConsoleOnly: bool
        VerboseLogging: bool
        [<JsonConverter(typeof<CoreTypes.MoveAnnotationConverter>)>]
        MoveAnnotation : CoreTypes.MoveAnnotation
        MinMoveTimeInMS: int
        TournamentMode: string
        PreventMoveDeviation: bool
        AllowPondering: bool
        EngineStartupTimeoutInSec: int
        Challengers: int
        [<JsonIgnore>] mutable IsChess960: bool
        [<JsonIgnore>] mutable DeviationCounter: int
        [<JsonIgnore>] mutable PreventMoveDeviationFor: string[]
        mutable Rounds: int
        PauseAfterRound: int
        DelayBetweenGames: TimeSpan
        MoveOverhead: TimeSpan
        OrdoExePath: string
        Adjudication: Adjudication
        TestOptions: TestOptions
        Opening: Opening
        CupOptions: CupOptions
        SwissOptions: SwissOptions
        LadderOptions: LadderOptions
        PgnOutPath: string
        ReferencePGNPath: string
        EngineSetup: EngineSetup
        mutable LayoutOption: LayoutOption
        TimeControl: TimeControl
        LiveFeed: LiveFeedConfig
        [<JsonIgnore>] mutable OpeningName: string
        [<JsonIgnore>] mutable TotalGames: int
        [<JsonIgnore>] mutable CurrentGameNr: int  }
      with
        member x.Hardware() = sprintf "%s : %s : %s" x.CPU x.RAM x.GPU
        member x.IsGauntlet =
          x.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
        member x.IsLadder =
          x.TournamentMode.Equals("Ladder", StringComparison.OrdinalIgnoreCase)
        member x.Players() =
          match x.EngineSetup.Engines with
          | [] -> "No players"
          | first::rest ->
              let start = sprintf "Players: %s" first.Name
              rest |> List.fold (fun acc e -> sprintf "%s, %s" acc e.Name) start

        member x.PlayerCount =
          if obj.ReferenceEquals(x.EngineSetup, null) || obj.ReferenceEquals(x.EngineSetup.Engines, null) then 0
          else x.EngineSetup.Engines.Length

        member x.ModeLabel() =
          if String.IsNullOrWhiteSpace x.TournamentMode then "RR" else x.TournamentMode

        member x.EffectiveSwissRounds() =
          if obj.ReferenceEquals(x.SwissOptions, null) then x.Rounds
          elif x.SwissOptions.Rounds > 0 then x.SwissOptions.Rounds
          else x.Rounds

        member x.EffectiveCupRounds(playerCount: int) =
          let mutable rounds = 0
          let mutable remaining = playerCount
          while remaining > 1 do
            remaining <- remaining / 2
            rounds <- rounds + 1
          rounds

        member x.EffectiveRounds() =
          let mode = x.ModeLabel()
          if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) then x.EffectiveSwissRounds()
          elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) then x.EffectiveCupRounds(x.PlayerCount)
          else x.Rounds

        member x.ScheduleSummary() =
          let mode = x.ModeLabel()
          let players = x.PlayerCount
          if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) then
            let rounds = x.EffectiveSwissRounds()
            let gamesPerMatch = if obj.ReferenceEquals(x.SwissOptions, null) then 2 else x.SwissOptions.GamesPerMatch
            let allowExtra = if obj.ReferenceEquals(x.SwissOptions, null) then false else x.SwissOptions.AllowExtraPairsOnTie
            let byes = if players % 2 = 1 then " + 1 bye/round" else ""
            let plannedGames = rounds * (players / 2) * gamesPerMatch
            let extraPairs = if allowExtra then " + extra pairs on tie" else ""
            sprintf "Schedule: players=%d, rounds=%d, games/match=%d, planned games=%d%s%s" players rounds gamesPerMatch plannedGames byes extraPairs
          elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) then
            let rounds = x.EffectiveCupRounds(players)
            let roundPairs =
              if obj.ReferenceEquals(x.CupOptions, null) then [] else x.CupOptions.RoundPairIncrements
            let pairsForRound roundNumber =
              if roundPairs.IsEmpty then 1
              else
                let idx = Math.Max(0, roundNumber - 1)
                if idx < roundPairs.Length then roundPairs.[idx] else roundPairs.[roundPairs.Length - 1]
            let plannedGames =
              [1 .. rounds]
              |> List.sumBy (fun roundNumber ->
                let matches = players / (1 <<< roundNumber)
                let pairs = Math.Max(1, pairsForRound roundNumber)
                matches * pairs * 2)
            let pairsText =
              if roundPairs.IsEmpty then "pairs/round=1"
              else sprintf "pairs/round=%s" (roundPairs |> Seq.map string |> String.concat "-")
            sprintf "Schedule: players=%d, rounds=%d, %s, planned games=%d (+ tiebreaks)" players rounds pairsText plannedGames
          else
            let rounds = x.Rounds
            sprintf "Schedule: players=%d, rounds=%d" players rounds

        member x.FindTimeControl id = x.TimeControl.GetTimeConfig id

        member x.TimeControlTextForPlayer (id:int) =
          if obj.ReferenceEquals(box x.TimeControl, null) then "" else
          let tc = x.FindTimeControl id
          tc.ToString()

        member x.TimeControlText() =
          // A tournament delivered over the live feed may carry no TimeControl (e.g. Tournament.Empty);
          // don't NRE the UI over a missing time control.
          if obj.ReferenceEquals(box x.TimeControl, null) then "No time control" else
          let moreThanOneTC = x.EngineSetup.Engines |> Seq.exists(fun e -> e.TimeControlID > 1)
          let engineConf = (x.EngineSetup.Engines |> Seq.tryHead)
          match engineConf with
          |None -> "No time control"
          |Some engineConf ->
            let tc1 = x.FindTimeControl engineConf.TimeControlID

            if not moreThanOneTC then
              tc1.ToString()
            else
              let tcs = x.EngineSetup.Engines |> Seq.filter(fun e -> e.TimeControlID <> tc1.Id) |> Seq.map(fun e -> e.TimeControlID) |> Seq.distinct
              if tcs |> Seq.length = 1 then
                let tc2 = x.FindTimeControl (tcs |> Seq.head)
                sprintf "%s vs %s" (tc1.ToString()) (tc2.ToString())
              else
                tc1.ToString()

        member x.GauntletText() =
          if x.IsGauntlet && x.EngineSetup.Engines.Length > 0 then
            let players = x.EngineSetup.Engines |> Seq.truncate(x.Challengers)
            let opponents = x.EngineSetup.Engines |> Seq.skip(x.Challengers)
            let oppMsg = opponents |> Seq.fold (fun st e -> sprintf "%s %s, " st e.Name ) ""
            let mutable txt = "Gauntlet:"
            for p in players do
              txt <- txt + sprintf " %s vs %s"  p.Name oppMsg
            txt + " "
          else ""

        member x.TablebaseText() =
          if x.Adjudication.TBAdj.UseTBAdjudication then
            $"Tablebase adj={x.Adjudication.TBAdj.TBMen}-men"
          else "Tablebase adj=not in use"

        member x.GetOpeningFileName() =
          if x.Opening.OpeningsPath.IsSome then
            Path.GetFileName (x.Opening.OpeningsPath.Value)
          else "no opening book"

        member x.AdjudicationText() = //"-draw movenumber=50 movecount=5 score=8 -resign movecount=5 score=1000"
          let t = sprintf "Adjudication: -draw movenumber=%d movecount=%d score=%.1f cp -resign movecount=%d score=%.1f cp "
                    x.Adjudication.DrawOption.MinDrawMove x.Adjudication.DrawOption.DrawMoveLength
                    x.Adjudication.DrawOption.MaxDrawScore x.Adjudication.WinOption.WinMoveLength x.Adjudication.WinOption.MinWinScore
          t

        member x.Summary() =
          let sb = new StringBuilder()
          let mode = x.ModeLabel()
          let formatOverhead (time: TimeSpan) = CoreTypes.formatDuration time
          let openings =
            let book =
              if x.Opening.OpeningsPath.IsSome then Path.GetFileName(x.Opening.OpeningsPath.Value)
              else "No book"
            let parts = ResizeArray<string>()
            parts.Add(sprintf "Book: %s" book)
            parts.Add(sprintf "ply=%d" x.Opening.OpeningsPly)
            parts.Add(sprintf "twice=%b" x.Opening.OpeningsTwice)
            if mode.Equals("Swiss", StringComparison.OrdinalIgnoreCase) && not (obj.ReferenceEquals(x.SwissOptions, null)) then
              parts.Add(sprintf "random=%b" x.SwissOptions.RandomOpenings)
              parts.Add(sprintf "unique=%s" (if x.SwissOptions.UniquePerMatchOnly then "per match" else "global"))
            elif mode.Equals("Cup", StringComparison.OrdinalIgnoreCase) && not (obj.ReferenceEquals(x.CupOptions, null)) then
              parts.Add(sprintf "random=%b" x.CupOptions.RandomOpenings)
              parts.Add(sprintf "unique=%s" (if x.CupOptions.UniquePerMatchOnly then "per match" else "global"))
            else
              parts.Add(sprintf "random=%b" x.Opening.RandomOpenings)
            String.Join(" | ", parts)
          let tablebases =
            if x.Adjudication.TBAdj.UseTBAdjudication then
              sprintf "Tablebases: %d-man" x.Adjudication.TBAdj.TBMen
            else
              "Tablebases: off"

          sb.AppendLine (sprintf "Run: %s" x.Name) |> ignore
          sb.AppendLine (sprintf "Hardware: %s" (x.Hardware())) |> ignore
          sb.AppendLine (sprintf "Mode: %s" mode) |> ignore
          sb.AppendLine (x.ScheduleSummary()) |> ignore
          let gauntlet = x.GauntletText()
          if String.IsNullOrWhiteSpace(gauntlet) |> not then
            sb.AppendLine gauntlet |> ignore
          sb.AppendLine (sprintf "Time: %s | overhead=%s | min move=%dms | pondering=%b"
                            (x.TimeControlText()) (formatOverhead x.MoveOverhead) x.MinMoveTimeInMS x.AllowPondering) |> ignore
          sb.AppendLine openings |> ignore
          sb.AppendLine tablebases |> ignore
          sb.AppendLine (x.AdjudicationText()) |> ignore
          sb.AppendLine (sprintf "Deviations: %d" x.DeviationCounter) |> ignore
          if String.IsNullOrWhiteSpace x.PgnOutPath |> not then
            if String.IsNullOrWhiteSpace x.ReferencePGNPath then
              sb.AppendLine (sprintf "Output: %s" x.PgnOutPath) |> ignore
            else
              sb.AppendLine (sprintf "Output: %s | reference: %s" x.PgnOutPath x.ReferencePGNPath) |> ignore
          sb.AppendLine (x.Players()) |> ignore
          sb.AppendLine (sprintf "Comment: %s" x.Description) |> ignore
          sb.ToString()

        member x.PGNSummary() =
          let sb = new StringBuilder()
          sb.Append $"{x.Description}; " |> ignore
          //sb.AppendLine x.Hardware |> ignore
          sb.Append (sprintf "Rounds=%d; " x.Rounds)  |> ignore
          //sb.Append x.GauntletText |> ignore
          //sb.Append (x.TimeControlText() + "; ") |> ignore
          if x.Opening.OpeningsPath.IsSome then
            sb.Append (sprintf "Book=%s; " (Path.GetFileName(x.Opening.OpeningsPath.Value))) |> ignore
          else
            sb.Append "Book=No book; " |> ignore
          sb.Append (x.TablebaseText() + "; ") |> ignore
          sb.Append (x.AdjudicationText() + ";") |> ignore
          //sb.AppendLine (sprintf "Comment: %s" x.Description) |> ignore
          sb.ToString()


        member x.MinSummary() =
          let sb = new StringBuilder()
          let mode = x.ModeLabel()
          sb.AppendLine (sprintf "Mode: %s" mode) |> ignore
          sb.AppendLine (x.ScheduleSummary()) |> ignore
          let gauntlet = x.GauntletText()
          if String.IsNullOrWhiteSpace(gauntlet) |> not then
            sb.AppendLine gauntlet |> ignore
          sb.AppendLine (sprintf "Time: %s" (x.TimeControlText())) |> ignore
          let book =
            if x.Opening.OpeningsPath.IsSome then Path.GetFileName(x.Opening.OpeningsPath.Value)
            else "No book"
          sb.AppendLine (sprintf "Book: %s" book) |> ignore
          sb.AppendLine (x.TablebaseText()) |> ignore
          sb.AppendLine (x.AdjudicationText()) |> ignore
          sb.AppendLine (x.Players()) |> ignore
          sb.AppendLine (sprintf "Comment: %s" x.Description) |> ignore
          sb.ToString()

        member x.PrintTournamentSummary() =
          printfn "Match: %s (%s)" x.Name x.Description
          printfn "%s" (x.Players())
          printfn "%s" "Engine configuration:"
          for eng in x.EngineSetup.Engines do
            let moveOverhead = x.MoveOverhead.TotalMilliseconds
            let info : string = eng.Information moveOverhead
            printfn "\t%s: %s" eng.Name info
          //printfn "LC0-version: %s" "v0.31.0-dag+git.dirty built Feb  7 2024 (cuda)" // Example, adjust as needed
          //printfn "LC0 options: %s" "default, minibatch-size=246" // Example, adjust as needed
          printfn "Hardware: %s + CPU %s" x.GPU x.CPU
          printfn "Software: %s" "EngineBattle GUI"
          printfn "Time control: %s" (x.TimeControlText())
          printfn "Lc0 benchmark: ~4300 nps" // Example, adjust as needed
          if x.Opening.OpeningsPath.IsSome then
            printfn "Book: %s, %s" (Path.GetFileName(x.Opening.OpeningsPath.Value)) "sequential, twice"
          else
            printfn "Book: No book"
          printfn "%s" (x.TablebaseText())
          printfn "%s" (x.AdjudicationText())
          if x.IsGauntlet then
            let challengers = x.EngineSetup.Engines |> Seq.truncate(x.Challengers) |> Seq.length
            let gamesPerRound = x.TotalGames / (x.Rounds * 2) - challengers
            if challengers > 0 then
              let totalDeviationGames = float (gamesPerRound * x.Rounds * (if x.Opening.OpeningsTwice then 2 else 1))
              printfn "ChallengerInDoNotDeviateMode: %b, number of stopped deviations=%d (%.2f times per game)"
                x.PreventMoveDeviation x.DeviationCounter (float x.DeviationCounter/totalDeviationGames)

          printfn "Duration: %s" "Days, hours and minutes"
          printfn "Comment:"

        static member Empty = {
          Name = ""
          Description = ""
          OS = ""
          CPU = ""
          RAM = ""
          GPU = ""
          MainLogoFileName = ""
          ConsoleOnly = false
          VerboseLogging = false
          MoveAnnotation = CoreTypes.MoveAnnotation.Standard
          MinMoveTimeInMS  = 0
          TournamentMode = "RR"
          PgnOutPath = ""
          ReferencePGNPath = ""
          IsChess960 = false
          PreventMoveDeviation = false
          AllowPondering = false
          EngineStartupTimeoutInSec = 180
          DeviationCounter = 0
          PreventMoveDeviationFor = null
          Challengers = 0
          Rounds = 0
          PauseAfterRound = 0
          OrdoExePath = String.Empty
          TimeControl = Unchecked.defaultof<TimeControl>
          LiveFeed = LiveFeedConfig.Empty
          EngineSetup = {Engines = []; EngineDefFolder = ""; EngineDefList = [] }
          Opening = {OpeningsPath = None; OpeningsTwice = false; OpeningsPly = 0; RandomOpenings = false; Seed = 0 }
          CupOptions = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
          SwissOptions = { GamesPerMatch = 2; Rounds = 0; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
          LadderOptions = { GamePairsPerMatch = 4; RandomOpenings = false; StatePath = "wwwroot/ladder_state.json" }
          TestOptions = {WriteToConsole = false; PolicyTest = false; ValueTest = false; NumberOfGamesInParallel = 1; GPUs = null }
          Adjudication =
            {
              DrawOption = {MinDrawMove = 0; MaxDrawScore = 0.0; DrawMoveLength = 0 }
              WinOption = {MinWinMove = 0; MinWinScore = 0.0; WinMoveLength = 0  }
              TBAdj = {TablebaseDirectory = String.Empty; UseTBAdjudication = true; TBMen = 6  }
            }

          DelayBetweenGames = TimeSpan.Zero
          MoveOverhead = TimeSpan.Zero
          OpeningName = ""
          // Feed fallback (e.g. the Ceres feed, which carries no EB layout) uses Empty's layout —
          // default it to a standings-focused broadcast view with the live PV board shown.
          LayoutOption =
            { LayoutOption.Default with
                ShowPVBoard = true
                OnlyShowStandings = true
                // Broadcast-sized fonts (mirror the typical DFRC Test config, scaled down ~3pt) so the
                // Ceres feed comes up readable rather than at the small UI defaults.
                Fonts =
                  { StandingsFont = 17
                    PairingsFont = 13
                    LatestGamesFont = 13
                    CrossTableFont = 15
                    CupBracketFont = 15
                    SwissOverviewFont = 15
                    LadderOverviewFont = 15
                    MoveListFont = 14
                    InfoBannerFont = 16
                    TournamentDescFont = 15
                    EnginesPanelFont = 20
                    PVLabelFont = 14 } }
          TotalGames = 0
          CurrentGameNr = 0
        }

    type StartOfTournamentInfo =
      {
        NumberOfGames: int
        GameDurationInSec : TimeSpan
        TournamentDurationSec : TimeSpan
        Tournament : Tournament option
      }
      with static member Empty = {NumberOfGames = 0; GameDurationInSec = TimeSpan.Zero; TournamentDurationSec = TimeSpan.Zero; Tournament = None}

  // -----------------------------------------------------------------------------
  // PUZZLE INPUT TYPE (depends on CoreTypes.EngineConfig)
  // -----------------------------------------------------------------------------
  module PuzzleInput =
    open PuzzleTypes

    type PuzzleInput =
      { puzzleData: CsvPuzzleData array
        maxRating: int
        minRating: int
        ratingGroups: string
        puzzleFilter: string
        engines: ResizeArray<CoreTypes.EngineConfig * int>
        iterations: int
        sampleSize: int
        nodes: string
        failed: int
        solved: int
        NumberOfPuzzlesInParallel: int
        IncludeFailedPuzzles: bool }
      with
        static member Create (puzzleData, maxRating, minRating, ratingGroup, puzzleFilter, engineConfigs, iterations, sampleSize, nodes, failed, solved, concurrency, ?includeFailedPuzzles) =
          { puzzleData = puzzleData
            maxRating = maxRating
            minRating = minRating
            ratingGroups = ratingGroup
            puzzleFilter = puzzleFilter
            engines = engineConfigs
            iterations = iterations
            sampleSize = sampleSize
            nodes = nodes
            failed = failed
            solved = solved
            NumberOfPuzzlesInParallel = concurrency
            IncludeFailedPuzzles = defaultArg includeFailedPuzzles false }
