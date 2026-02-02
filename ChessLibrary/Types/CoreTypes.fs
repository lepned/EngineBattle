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
          OptionsPath: string
          mutable NetworkPath: string
          Args: string
          Options: System.Collections.Generic.Dictionary<string, obj>
          WinboardConfig: WinboardConfig option }
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
                Rating = 3000
                Dev = ""
                LogoPath = ""
                IsChallenger = false
                ContemptEnabled = false
                NegativeContemptAllowed = false
                Protocol = "UCI"
                Path = ""
                OptionsPath = ""
                NetworkPath = ""
                Args = String.Empty
                Options = new System.Collections.Generic.Dictionary<string, obj>()
                WinboardConfig = None }
            static member EmptyWithPath (path:string) =
              let fileName = Path.GetFileNameWithoutExtension path
              let isLc0 = Regex.Match(fileName, "lc0", RegexOptions.IgnoreCase).Success
              { Name = fileName
                Alias = "Engine xx"
                TimeControlID = 1
                Version = "Version"
                Rating = 3000
                Dev = "from xxx"
                LogoPath = "img/lc0.png"
                IsChallenger = false
                ContemptEnabled = false
                NegativeContemptAllowed = false
                Protocol = "UCI"
                Path = path
                OptionsPath = ""
                NetworkPath = ""
                Args = if isLc0 then "--show-hidden" else String.Empty
                Options = new System.Collections.Generic.Dictionary<string, obj>()
                WinboardConfig = None }
            static member AddOptions (config: EngineConfig) options =
              { config with Options = options }

    type Pairing =
      { Opening: PgnGame
        White: EngineConfig
        Black: EngineConfig
        GameNr : int
        RoundNr: string
        OpeningHash : string}

    type EngineLineData = {
        Player : string
        Elo : float
        Error : float
        Points : float
        Played : int
        Percent : float
        CFS : int
        mutable EPS: double
        mutable Speed: double
        Win : int
        Draw : int
        Loss : int
        D : int
        WhiteScore : float
        BlackScore : float
        Pairs : string }

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
        PairWins: int
        PairLosses: int
        mutable MedSpeed: double
        mutable AvgNPM: double
        mutable Challenger: bool }
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
        WhiteTime : TimeOnly
        BlackTime : TimeOnly
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
        }
        with
            static member Create(qRank: int, policyRank: int, bestMove: NNValues, playedMove: NNValues, isWhite: bool) =
                {
                    QRank = qRank
                    PolicyRank = policyRank
                    BestMove = bestMove
                    PlayedMove = playedMove
                    IsWhite = isWhite
                }

  // -----------------------------------------------------------------------------
  // LAYOUT & TOURNAMENT DETAILS
  // -----------------------------------------------------------------------------

  // Tournament details, including methods for summarizing configuration.
  module Tournament =
    open LayoutTypes
    open TimeControlTypes

    type DrawOption = { DrawMoveLength: int; MaxDrawScore: float; MinDrawMove: int }
    type WinOption = { MinWinMove: int; MinWinScore: float; WinMoveLength: int }
    type TableBaseAdjudication = {TablebaseDirectory:string; UseTBAdjudication: bool; TBMen: int }
    type Adjudication = { DrawOption: DrawOption; WinOption: WinOption; TBAdj: TableBaseAdjudication }
    type Opening = { OpeningsPath: string option; OpeningsTwice: bool; OpeningsPly: int }
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
    type EngineSetup =
      { [<JsonIgnore>] mutable Engines: CoreTypes.EngineConfig list
        EngineDefFolder: string
        EngineDefList: string list }
    type TestOptions = { PolicyTest: bool; ValueTest: bool; WriteToConsole: bool; NumberOfGamesInParallelConsoleOnly: int }

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
        VerboseMoveAnnotation : bool
        MinMoveTimeInMS: int
        TournamentMode: string
        PreventMoveDeviation: bool
        AllowPondering: bool
        EngineStartupTimeoutInSec: int
        Challengers: int
        [<JsonIgnore>] mutable IsChess960: bool
        [<JsonIgnore>] mutable DeviationCounter: int
        mutable Rounds: int
        PauseAfterRound: int
        DelayBetweenGames: TimeOnly
        MoveOverhead: TimeOnly
        OrdoExePath: string
        Adjudication: Adjudication
        TestOptions: TestOptions
        Opening: Opening
        CupOptions: CupOptions
        SwissOptions: SwissOptions
        PgnOutPath: string
        ReferencePGNPath: string
        EngineSetup: EngineSetup
        mutable LayoutOption: LayoutOption
        TimeControl: TimeControl
        [<JsonIgnore>] mutable OpeningName: string
        [<JsonIgnore>] mutable TotalGames: int
        [<JsonIgnore>] mutable CurrentGameNr: int  }
      with
        member x.Hardware() = sprintf "%s : %s : %s" x.CPU x.RAM x.GPU
        member x.IsGauntlet =
          x.TournamentMode.Equals("Gauntlet", StringComparison.OrdinalIgnoreCase)
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

        member x.FormatNumberWithK (number: int) =
            if number < 1000 then
                sprintf "%d nodes" number
            else
                let num = float number
                sprintf "%.1fK nodes" (num / 1000.0)

        member x.TimeControlTextForPlayer (id:int) =
          let tc = x.FindTimeControl id
          if tc.NodeLimit then
                  x.FormatNumberWithK tc.Nodes
          else
            x.FormatTimeSpan (tc.Fixed.ToTimeSpan()) (tc.Increment.ToTimeSpan())
            //sprintf "%ds + %.1fs"
            //    (tc.Fixed.ToTimeSpan().TotalSeconds|> int)
            //    (tc.Increment.ToTimeSpan().TotalSeconds)

        member x.TimeControlTextForPlayers (id1:int, id2:int) =
          let moreThanOneTC = id1 <> id2
          let formatNumberWithK (number: int) =
            if number < 1000 then
                sprintf "%d n" number
            else
                let num = float number
                sprintf "%.1fK n" (num / 1000.0)

          let tc1 = x.FindTimeControl id1
          if not moreThanOneTC then
            x.FormatTimeSpan (tc1.Fixed.ToTimeSpan()) (tc1.Increment.ToTimeSpan())
              //sprintf "%ds + %.1fs"
              //    (tc1.Fixed.ToTimeSpan().TotalSeconds|> int)
              //    (tc1.Increment.ToTimeSpan().TotalSeconds)
          else
            let tc2 = x.FindTimeControl id2
            let tc1 =
                if tc1.NodeLimit then
                  formatNumberWithK tc1.Nodes
                else
                  x.FormatTimeSpan (tc1.Fixed.ToTimeSpan()) (tc1.Increment.ToTimeSpan())
                  //sprintf "%ds + %.1fs"
                  //  (tc1.Fixed.ToTimeSpan().TotalSeconds|> int)
                  //  (tc1.Increment.ToTimeSpan().TotalSeconds)
            let tc2 =
                if tc2.NodeLimit then
                  let nodes = formatNumberWithK tc2.Nodes
                  nodes
                else
                  x.FormatTimeSpan (tc2.Fixed.ToTimeSpan()) (tc2.Increment.ToTimeSpan())
                  //sprintf "%ds + %.1fs"
                  //  (tc2.Fixed.ToTimeSpan().TotalSeconds|> int)
                  //  (tc2.Increment.ToTimeSpan().TotalSeconds)
            sprintf "%s vs %s" tc1 tc2

        member x.FormatTimeSpan (fixedTime: TimeSpan) (incrementTime: TimeSpan) : string =
                let totalFixedMinutes = fixedTime.TotalMinutes
                let totalFixedSeconds = fixedTime.TotalSeconds
                let totalIncrementSeconds = float incrementTime.Seconds + (float incrementTime.Milliseconds / 1000.0) + float incrementTime.Minutes * 60.0 + float incrementTime.Hours * 3600.0
                let fixedTimePart =
                    if totalFixedMinutes >= 1.0 then sprintf "%.0f'" totalFixedMinutes
                    else sprintf "%.0f''" totalFixedSeconds
                let incrementTimePart = if incrementTime.Milliseconds > 0 then sprintf "%.1f''" totalIncrementSeconds else sprintf "%.0f''" totalIncrementSeconds
                sprintf "%s + %s" fixedTimePart incrementTimePart

        member x.TimeControlText() =
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
                let tc2 = tcs |> Seq.head
                let tc2 = x.FindTimeControl tc2
                let tc1 = x.FindTimeControl tc1.Id
                let tc1 =
                  if tc1.NodeLimit then
                    sprintf "%dN" tc1.Nodes
                  else
                    x.FormatTimeSpan (tc1.Fixed.ToTimeSpan()) (tc1.Increment.ToTimeSpan())
                    //sprintf "%ds + %.1fs"
                    //  (tc1.Fixed.ToTimeSpan().TotalSeconds|> int)
                    //  (tc1.Increment.ToTimeSpan().TotalSeconds)

                let tc2 =
                  if tc2.NodeLimit then
                    sprintf "%dN" tc2.Nodes
                  else
                    x.FormatTimeSpan (tc2.Fixed.ToTimeSpan()) (tc2.Increment.ToTimeSpan())
                    //sprintf "%ds + %.1fs"
                    //  (tc2.Fixed.ToTimeSpan().TotalSeconds|> int)
                    //  (tc2.Increment.ToTimeSpan().TotalSeconds)
                sprintf "%s vs %s" tc1 tc2
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
          let formatTimeOnly (time: TimeOnly) = time.ToString("HH:mm:ss.fff")
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
                            (x.TimeControlText()) (formatTimeOnly x.MoveOverhead) x.MinMoveTimeInMS x.AllowPondering) |> ignore
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
            let moveOverhead = x.MoveOverhead.ToTimeSpan().TotalMilliseconds
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
          VerboseMoveAnnotation = false
          MinMoveTimeInMS  = 0
          TournamentMode = "RR"
          PgnOutPath = ""
          ReferencePGNPath = ""
          IsChess960 = false
          PreventMoveDeviation = false
          AllowPondering = false
          EngineStartupTimeoutInSec = 180
          DeviationCounter = 0
          Challengers = 0
          Rounds = 0
          PauseAfterRound = 0
          OrdoExePath = String.Empty
          TimeControl = Unchecked.defaultof<TimeControl>
          EngineSetup = {Engines = []; EngineDefFolder = ""; EngineDefList = [] }
          Opening = {OpeningsPath = None; OpeningsTwice = false; OpeningsPly = 0 }
          CupOptions = { RoundPairIncrements = []; SeedingStrategy = "ByRating"; UniquePerMatchOnly = false; BracketPath = "wwwroot/cup_bracket.json"; RandomOpenings = false }
          SwissOptions = { GamesPerMatch = 2; Rounds = 0; SeedGroupCount = 4; UniquePerMatchOnly = false; RandomOpenings = false; AllowExtraPairsOnTie = false; StatePath = "wwwroot/swiss_state.json" }
          TestOptions = {WriteToConsole = false; PolicyTest = false; ValueTest = false; NumberOfGamesInParallelConsoleOnly = 1 }
          Adjudication =
            {
              DrawOption = {MinDrawMove = 0; MaxDrawScore = 0.0; DrawMoveLength = 0 }
              WinOption = {MinWinMove = 0; MinWinScore = 0.0; WinMoveLength = 0  }
              TBAdj = {TablebaseDirectory = String.Empty; UseTBAdjudication = true; TBMen = 6  }
            }

          DelayBetweenGames = TimeOnly.MinValue
          MoveOverhead = TimeOnly.MinValue
          OpeningName = ""
          LayoutOption = LayoutOption.Default
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
        NumberOfPuzzlesInParallel: int }
      with
        static member Create (puzzleData, maxRating, minRating, ratingGroup, puzzleFilter, engineConfigs, iterations, sampleSize, nodes, failed, solved, concurrency) =
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
            NumberOfPuzzlesInParallel = concurrency }
