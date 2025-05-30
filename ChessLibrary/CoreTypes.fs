
namespace ChessLibrary

open System
open System.Text
open System.Collections.Concurrent
open System.IO
open System.Text.RegularExpressions
open System.Net.Http
open System.Xml.Linq
open System.Text.Json.Serialization
open QBBOperations

module TypesDef =
// -----------------------------------------------------------------------------
// Misc TYPES
// -----------------------------------------------------------------------------
  module Misc =
    /// The starting position in FEN notation.
    let startPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    type ResultReason =
        | Checkmate
        | Stalemate
        | AdjudicateTB
        | AdjudicateMaterial
        | ExcessiveMoves
        | Repetition
        | AdjudicatedEvaluation
        | ForfeitLimits
        | Cancel
        | Illegal
        | NotStarted

        override this.ToString() =
            match this with
            | Checkmate -> "CM"
            | Stalemate -> "SM"
            | AdjudicateTB -> "TB"
            | AdjudicateMaterial -> "AM"
            | ExcessiveMoves -> "50m"
            | Repetition -> "R3"
            | AdjudicatedEvaluation -> "AE"
            | ForfeitLimits -> "FL"
            | Cancel -> "XX"
            | Illegal -> "IM"
            | NotStarted -> "NS"

        member this.Explanation =
            match this with
            | Checkmate -> "Checkmate"
            | Stalemate -> "Stalemate"
            | AdjudicateTB -> "Tablebase known result"
            | AdjudicateMaterial -> "Insufficient material"
            | ExcessiveMoves -> "Too many moves"
            | Repetition -> "Repetition draw"
            | AdjudicatedEvaluation -> "Evaluation agreement"
            | ForfeitLimits -> "Time/node limit forfeit"
            | Cancel -> "Game was cancelled" 
            | Illegal -> "Illegal move"
            | NotStarted -> "Not started"

    let stringToResultReason (str: string): ResultReason =
        match str with
        | "CM" -> Checkmate
        | "SM" -> Stalemate
        | "TB" -> AdjudicateTB
        | "AM" -> AdjudicateMaterial
        | "50m" -> ExcessiveMoves
        | "R3" -> Repetition
        | "AE" -> AdjudicatedEvaluation
        | "FL" -> ForfeitLimits
        | "XX" -> Cancel
        | "IM" -> Illegal
        | "NS" -> NotStarted
        | _ -> failwith "Invalid ResultReason string"

  // -----------------------------------------------------------------------------
  // PGN TYPES
  // -----------------------------------------------------------------------------
  module PGNTypes =
    type Move = 
      { mutable MoveNr: string
        mutable WhiteSan: string
        mutable WhiteComment: string 
        mutable BlackSan: string 
        mutable BlackComment: string }
      static member Empty =
        { MoveNr = ""; WhiteSan = ""; WhiteComment = ""; BlackSan = ""; BlackComment = "" }

    type Header = { Key: string; Value: string }

    type GameMetadata = 
      { Event: string
        Site: string
        Date: string
        mutable Round: string
        White: string
        Black: string
        Result: string
        Reason: Misc.ResultReason
        mutable OpeningHash: string
        GameTime: int64
        Moves: int
        OpeningName: string
        Fen: string
        Deviations: int
        OtherTags: Header list }
      with 
        static member Empty =
          { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = "";
            Result = ""; Reason = Misc.ResultReason.NotStarted; OpeningHash = ""; GameTime = 0L;
            Moves = 0; OpeningName = ""; Fen = ""; Deviations = 0; OtherTags = [] }
        member x.Opening =
          let rec loop (list: Header list) =
            match list with
            | [] -> "No opening"
            | h :: t ->
                if h.Key.ToLower().Contains "opening" then h.Value else loop t
          loop x.OtherTags

    type PgnGame = 
      { 
        GameNumber: int
        GameMetaData: GameMetadata
        Moves: ResizeArray<Move>
        Comments: string
        Fen: string
        Raw: string }
      with 
        static member Empty i =
          { 
            GameNumber = i
            GameMetaData = GameMetadata.Empty
            Moves = ResizeArray()
            Comments = ""
            Fen = ""
            Raw = "" }

    type GameResult = 
      | WhiteWins
      | BlackWins
      | DrawAgreed

    type OpportunityDetail = 
      { Game: PgnGame
        MoveIndex: int
        MovePlayed: Move
        EvalDifference: float
        WhiteEval: float
        BlackEval: float
        GameResult: GameResult }
      with 
        override this.ToString() =
          sprintf "\nGame %d. %s vs %s Result: %A\n EVAL: W: %f B: %f\n %A"
            this.Game.GameNumber
            this.Game.GameMetaData.White
            this.Game.GameMetaData.Black
            this.GameResult
            this.WhiteEval
            this.BlackEval
            this.MovePlayed

    type MissedOpportunity =
      | MissedWin of OpportunityDetail
      | MissedDraw of OpportunityDetail
      | NoneInGame
      with 
        override this.ToString() =
          match this with
          | MissedWin op
          | MissedDraw op -> op.ToString()
          | NoneInGame -> "None missed opportunity found"

  // -----------------------------------------------------------------------------
// CORE TYPES
// -----------------------------------------------------------------------------  
  module CoreTypes =
    type EvalType =
      | CP of Info: float
      | Mate of Info: int
      | NA
      with 
        override this.ToString() =
          match this with
          | CP info -> if Math.Abs(info) < 0.005 then "0.00" else sprintf "%.2f" info
          | Mate info -> if info > 0 then sprintf "M%d" info else sprintf "-M%d" (abs info)
          | NA -> "None"
        member x.Value =
          match x with 
          | CP cp -> cp
          | Mate m -> float m
          | _ -> failwith "EvalType is NA"
        member x.ValueStr =
          match x with 
          | CP cp -> cp.ToString()
          | Mate m -> sprintf "M%d" m
          | _ -> failwith "EvalType is NA"
        member x.WinAdj v =
          match x with 
          | CP cp -> abs cp > v
          | Mate _ -> true
          | _ -> failwith "EvalType is NA"
        member x.DrawAdj v =
          match x with 
          | CP cp -> abs cp < v
          | Mate _ -> true
          | _ -> failwith "EvalType is NA"  
  
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
          Options: System.Collections.Generic.Dictionary<string, obj> }
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
                Options = new System.Collections.Generic.Dictionary<string, obj>() }
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
                Options = new System.Collections.Generic.Dictionary<string, obj>() }
            static member AddOptions (config: EngineConfig) options =
              { config with Options = options }  
    
    type Pairing = 
      { Opening: PGNTypes.PgnGame
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
        PgnGamePair: PGNTypes.PgnGame * PGNTypes.PgnGame
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
            PgnGamePair = PGNTypes.PgnGame.Empty 0, PGNTypes.PgnGame.Empty 0
            PrevFen = String.Empty
            DevFen = String.Empty}

    type WDL = { Win: float; Draw: float; Loss: float }
      with 
        static member Empty = { Win = 0.0; Draw = 0.0; Loss = 0.0 }

    type WDLType =
      | HasValue of Values: WDL
      | NotFound
      with
        member x.Value() =
          match x with
          | HasValue v -> v
          | NotFound -> WDL.Empty

 

    /// For GUI use only.
    type MoveDetail = { LongSan: string; FromSq: string; ToSq: string; Color: string; IsCastling: bool }
      with 
        static member Empty = { LongSan = ""; FromSq = ""; ToSq = ""; Color = ""; IsCastling = false }
        static member Create(longSan, fromsq, tosq, color, iscastling) = 
          { LongSan = longSan; FromSq = fromsq; ToSq = tosq; Color = color; IsCastling = iscastling }

    type MoveAndFen = { Move: MoveDetail; ShortSan: string; FenAfterMove: string }
      with 
        static member FirstEntry = { Move = MoveDetail.Empty; ShortSan = ""; FenAfterMove = Misc.startPosition }
        static member Init(fen) = { Move = MoveDetail.Empty; ShortSan = ""; FenAfterMove = fen }

    type SummaryEngineStat = 
      { Player: string; Median: bool; Games: int; AvgNPS: float; AvgNodes: float; AvgDepth: float; AvgSelfDepth: float; Time: int64 }

    type EngineStatsPerGame = 
      { Player: string
        GameNr: int
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
            AvgNps = 0.0
            MedianNps = 0.0
            AvgNodes = 0.0
            MedianNodes = 0.0
            AvgDepth = 0.0
            MedianDepth = 0.0
            AvgSD = 0.0
            MedianSD = 0.0 }

    type EngineOption = { Name: string; Value: string }
      with 
        static member Create name value = { Name = name; Value = value }

  

    type EngineStatus =
      { mutable PlayerName: string
        mutable Eval: EvalType
        Nodes: int64
        NPS: float
        Depth: int
        SD: int
        TBhits: int64
        WDL: WDLType
        PV: string
        PVLongSAN: string
        MultiPV: int }
      with 
        static member Empty =
          { PlayerName = ""
            Eval = EvalType.NA
            Nodes = 0L
            NPS = 0.0
            Depth = 0
            SD = 0
            TBhits = 0L
            WDL = WDLType.NotFound
            PV = ""
            PVLongSAN = ""
            MultiPV = 1 }

    type NNValues = 
      { Player: string
        mutable SANMove: string
        LANMove: string
        Nodes: int64
        P: float
        mutable Q: float
        V: float
        E: float
        Raw: string }
      with 
        static member Empty =
          { Player = ""
            SANMove = ""
            LANMove = ""
            Nodes = 0L
            P = 0.0
            Q = 0.0
            V = 0.0
            E = 0.0
            Raw = "" }

    type BestMoveInfo = 
      { Player: string
        Move: string
        Ponder: string
        Eval: EvalType
        TimeLeft: TimeOnly
        MoveTime: TimeOnly
        Nodes: int64
        NPS: float
        FEN: string
        PV: string  
        LongPV: string
        MoveAndFen: MoveAndFen
        MoveHistory: string
        Move50: int
        R3: int
        PiecesLeft: int
        AdjDrawML: int }

    type Result = 
      { Player1: string
        Player2: string
        Moves: int
        Result: string
        Reason: Misc.ResultReason
        GameTime: int64 }
        override x.ToString() =
          let time = float x.GameTime / 1000.0
          sprintf "%s vs %s: %s (%s), %d moves, %.1f seconds" x.Player1 x.Player2 x.Result (x.Reason.Explanation) x.Moves time
    let createResult p1 p2 (moves: ResizeArray<string>) result reason gameTime =
      { Player1 = p1; Player2 = p2; Moves = moves.Count; Result = result; Reason = reason; GameTime = gameTime }


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
      }
      override this.ToString() =
        sprintf "Start of game number %d: %s vs %s" this.CurrentGameNr this.WhitePlayer.Name this.BlackPlayer.Name


  // -----------------------------------------------------------------------------
  // TIME CONTROL & RELATED COMMANDS
  // -----------------------------------------------------------------------------
  module TimeControl =
    
    type UnionType =
      | FixedTime of InMSFixed: TimeOnly
      | WithIncrement of InMSFixed: TimeOnly * IncrementMS: TimeOnly
      | WithMoves of InMSFixed: TimeOnly * IncrementMS: TimeOnly * Wmoves: int * Bmoves: int
      | Nodes of nodes: int
      with
        member x.GetFixedtime() =
          match x with
          | FixedTime time -> time
          | WithIncrement (t, _) -> t
          | WithMoves (t, _, _, _) -> t
          | Nodes _ -> TimeOnly.MinValue
        member x.GetIncrementTime() =
          match x with
          | FixedTime _ -> TimeOnly.MinValue
          | WithIncrement (_, incr) -> incr
          | WithMoves (_, incr, _, _) -> incr
          | Nodes _ -> TimeOnly.MinValue

    type TimeConfig = { Id: int; Fixed: TimeOnly; Increment: TimeOnly; NodeLimit: bool; Nodes: int }
      with
        member x.Times (fraction: double) =
          let fixedTicks = float x.Fixed.Ticks
          let newFixedTicks = fixedTicks * fraction |> int64
          let incrTicks = float x.Increment.Ticks
          let newIncrTicks = incrTicks * fraction |> int64
          let newNodes = float x.Nodes * fraction |> int32
          { x with Fixed = new TimeOnly(newFixedTicks); Increment = new TimeOnly(newIncrTicks); Nodes = newNodes }
        member x.ShortString() =
          if x.NodeLimit then
            sprintf "Node limit=%d " x.Nodes
          else
            sprintf "%ds + %.1fs " (x.Fixed.ToTimeSpan().TotalSeconds |> int) (x.Increment.ToTimeSpan().TotalSeconds)
        member x.FormatTimeSpan (fixedTime: TimeSpan) (incrementTime: TimeSpan) : string =
          let totalFixedMinutes = fixedTime.TotalMinutes
          let totalFixedSeconds = fixedTime.TotalSeconds
          let totalIncrementSeconds =
            float incrementTime.Seconds +
            (float incrementTime.Milliseconds / 1000.0) +
            float incrementTime.Minutes * 60.0 +
            float incrementTime.Hours * 3600.0
          let fixedTimePart =
            if totalFixedMinutes >= 1.0 then 
                if fixedTime.Seconds > 0 then
                    sprintf "%.1f'" totalFixedMinutes
                else
                    sprintf "%.0f'" totalFixedMinutes
            else sprintf "%.0f''" totalFixedSeconds
          let incrementTimePart =
            if incrementTime.Milliseconds > 0 then 
                sprintf "%.1f''" totalIncrementSeconds
            else sprintf "%.0f''" totalIncrementSeconds
          sprintf "%s + %s" fixedTimePart incrementTimePart
        override x.ToString() =
          if x.NodeLimit then
            sprintf "Node limit=%d" x.Nodes
          else
            x.FormatTimeSpan (x.Fixed.ToTimeSpan()) (x.Increment.ToTimeSpan())

    type TimeControl =
      { TimeConfigs: TimeConfig list; WmovesToGo: int; BmovesToGo: int }
      with
        member x.GetTimeConfig (idx: int) =
          match x.TimeConfigs |> Seq.tryFind (fun e -> e.Id = idx) with
          | Some tc -> tc
          | None -> x.TimeConfigs |> Seq.head
        member x.GetFixedtime(idx: int) =
          (x.GetTimeConfig idx).Fixed
        member x.GetTime (config: TimeConfig) =
          match (config.Increment, config.Increment, x.WmovesToGo, x.BmovesToGo) with
          | (w, b, _, _) when w.Ticks = 0 && b.Ticks = 0 ->
              UnionType.FixedTime(config.Fixed)
          | (w, b, _, _) when w.Ticks > 0 || b.Ticks > 0 ->
              UnionType.WithIncrement(config.Fixed, config.Increment)
          | (_, _, w, b) when w > 0 || b > 0 ->
              UnionType.WithMoves (config.Fixed, config.Increment, x.WmovesToGo, x.BmovesToGo)
          | _ -> UnionType.Nodes(config.Nodes)
        member x.GetUnion(idx: int) =
          let config = x.GetTimeConfig idx
          match (config.Increment, config.Increment, x.WmovesToGo, x.BmovesToGo) with
          | (w, b, _, _) when w.Ticks = 0 && b.Ticks = 0 ->
              UnionType.FixedTime(config.Fixed)
          | (w, b, _, _) when w.Ticks > 0 || b.Ticks > 0 ->
              UnionType.WithIncrement(config.Fixed, config.Increment)
          | (_, _, w, b) when w > 0 || b > 0 ->
              UnionType.WithMoves (config.Fixed, config.Increment, x.WmovesToGo, x.BmovesToGo)
          | _ -> UnionType.Nodes(config.Nodes)
        member x.GetIncrementTime(idx: int) =
          (x.GetTimeConfig idx).Increment
        member x.TimeInfo(idx: int) =
          let config = x.GetTimeConfig idx
          sprintf "%fs + %fs" (config.Fixed.ToTimeSpan().TotalSeconds) (config.Increment.ToTimeSpan().TotalSeconds)

    module TimeControlCommands =
      let createTimeControlWithIncrement (wtime: TimeOnly) (btime: TimeOnly) (winc: TimeOnly) (binc: TimeOnly) : string =
        let white = int (TimeSpan(wtime.Ticks).TotalMilliseconds)
        let black = int (TimeSpan(btime.Ticks).TotalMilliseconds)
        let wInc = int (TimeSpan(winc.Ticks).TotalMilliseconds)
        let bInc = int (TimeSpan(binc.Ticks).TotalMilliseconds)
        sprintf "go wtime %d btime %d winc %d binc %d" white black wInc bInc

      let createTimeControl (wtime: TimeOnly) (btime: TimeOnly) : string =
        let white = int (TimeSpan(wtime.Ticks).TotalMilliseconds)
        let black = int (TimeSpan(btime.Ticks).TotalMilliseconds)
        sprintf "go wtime %d btime %d" white black

      let createTimeControlWithMovesToGo (wtime: TimeOnly) (btime: TimeOnly) (winc: TimeOnly) (binc: TimeOnly) (wmoves: int) (bmoves: int) : string =
        let white = int (TimeSpan(wtime.Ticks).TotalMilliseconds)
        let black = int (TimeSpan(btime.Ticks).TotalMilliseconds)
        let wincMs = int (TimeSpan(winc.Ticks).TotalMilliseconds)
        let bincMs = int (TimeSpan(binc.Ticks).TotalMilliseconds)
        sprintf "go wtime %d btime %d winc %d binc %d movestogo %d %d" white black wincMs bincMs wmoves bmoves

      let createNodes nodes =
        sprintf "go nodes %d" nodes

      let getFixedTime (time: TimeOnly) = FixedTime time
      let getNodeTime (nodes: int) = Nodes nodes 

      let uciTimeCommand (time: UnionType) wTime bTime =
        match time with
        | FixedTime _ -> createTimeControl wTime bTime
        | WithIncrement (_, incr) -> createTimeControlWithIncrement wTime bTime incr incr
        | WithMoves (_, incr, wMoves, bMoves) -> createTimeControlWithMovesToGo wTime bTime incr incr wMoves bMoves
        | Nodes nodes -> createNodes nodes


  // -----------------------------------------------------------------------------
  // LAYOUT & TOURNAMENT DETAILS
  // -----------------------------------------------------------------------------
  module Layout =
    type Fonts =
      { StandingsFont: int
        PairingsFont: int
        LatestGamesFont: int
        CrossTableFont: int
        MoveListFont: int
        InfoBannerFont: int
        TournamentDescFont: int
        EnginesPanelFont: int
        PVLabelFont: int }
    type Sizes =
      { LiveChartHeight: int
        MoveChartHeight: int
        PVboardSize: string }
    type Charts =
      { ShowNPS: bool
        ShowEval: bool
        ShowNodes: bool
        ShowTime: bool
        NumberOfLines: int
        Qdiff: float }
      with 
        static member Default =
          { ShowNPS = false
            ShowEval = true
            ShowNodes = false
            ShowTime = true
            NumberOfLines = 3
            Qdiff = 0.5 }
    type LayoutOption =
      { Fonts: Fonts
        Sizes: Sizes
        Charts: Charts
        ShowPVBoard: bool
        UseNPM: bool
        BestMoveWithPolicy: bool
        OnlyShowStandings: bool
        ShowCrosstableBetweenGames: bool
        ShowCrosstableBelowStandings: bool
        AutoCycleTimeInSec: int }
      with 
        static member Default =
          { Fonts =
              { StandingsFont = 12
                PairingsFont = 12
                LatestGamesFont = 12
                CrossTableFont = 13
                MoveListFont = 15
                InfoBannerFont = 14
                TournamentDescFont = 12
                EnginesPanelFont = 14
                PVLabelFont = 12 }
            Sizes =
              { LiveChartHeight = 200
                MoveChartHeight = 200
                PVboardSize = "medium" }
            Charts = Charts.Default
            ShowPVBoard = false
            UseNPM = false
            BestMoveWithPolicy = false
            OnlyShowStandings = false
            ShowCrosstableBetweenGames = false
            ShowCrosstableBelowStandings = false
            AutoCycleTimeInSec = 30 }


  // Tournament details, including methods for summarizing configuration.
  module Tournament =
    open Layout

    type DrawOption = { DrawMoveLength: int; MaxDrawScore: float; MinDrawMove: int }
    type WinOption = { MinWinMove: int; MinWinScore: float; WinMoveLength: int }
    type TableBaseAdjudication = {TablebaseDirectory:string; UseTBAdjudication: bool; TBMen: int }
    type Adjudication = { DrawOption: DrawOption; WinOption: WinOption; TBAdj: TableBaseAdjudication }
    type Opening = { OpeningsPath: string option; OpeningsTwice: bool; OpeningsPly: int }
    type EngineSetup =
      { [<JsonIgnore>] Engines: CoreTypes.EngineConfig list
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
        Gauntlet: bool
        PreventMoveDeviation: bool
        Challengers: int
        [<JsonIgnore>] mutable IsChess960: bool
        [<JsonIgnore>] mutable DeviationCounter: int
        mutable Rounds: int
        PauseAfterRound: int
        DelayBetweenGames: TimeOnly
        MoveOverhead: TimeOnly
        Adjudication: Adjudication
        TestOptions: TestOptions
        Opening: Opening
        PgnOutPath: string
        ReferencePGNPath: string
        EngineSetup: EngineSetup
        mutable LayoutOption: LayoutOption
        TimeControl: TimeControl.TimeControl
        [<JsonIgnore>] mutable OpeningName: string
        [<JsonIgnore>] mutable TotalGames: int
        [<JsonIgnore>] mutable CurrentGameNr: int  }
      with
        member x.Hardware() = sprintf "%s : %s : %s" x.CPU x.RAM x.GPU
        member x.Players() =
          match x.EngineSetup.Engines with
          | [] -> "No players"
          | first::rest ->
              let start = sprintf "Players: %s" first.Name
              rest |> List.fold (fun acc e -> sprintf "%s, %s" acc e.Name) start
        
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
          if x.Gauntlet && x.EngineSetup.Engines.Length > 0 then 
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
          sb.AppendLine "Description:" |> ignore
          sb.AppendLine (x.Hardware()) |> ignore
          sb.AppendLine (sprintf "Rounds: %d" x.Rounds)  |> ignore
          sb.AppendLine (x.Players()) |> ignore
          sb.AppendLine (x.GauntletText()) |> ignore
          sb.AppendLine (x.TimeControlText()) |> ignore
          if x.Opening.OpeningsPath.IsSome then
            sb.AppendLine (sprintf "Book: %s" (Path.GetFileName(x.Opening.OpeningsPath.Value))) |> ignore
          else
            sb.AppendLine "Book: No book" |> ignore
          sb.AppendLine (x.TablebaseText()) |> ignore
          sb.AppendLine (x.AdjudicationText()) |> ignore
          sb.AppendLine (sprintf "Deviations: %d" x.DeviationCounter) |> ignore
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
          //sb.AppendLine "Description:" |> ignore      
          sb.AppendLine (sprintf "Rounds: %d" x.Rounds)  |> ignore
          sb.AppendLine (x.Players()) |> ignore
          sb.AppendLine (x.GauntletText()) |> ignore
          sb.AppendLine (x.TimeControlText()) |> ignore
          if x.Opening.OpeningsPath.IsSome then
            sb.AppendLine (sprintf "Book: %s" (Path.GetFileName(x.Opening.OpeningsPath.Value))) |> ignore
          else
            sb.AppendLine "Book: No book" |> ignore
          sb.AppendLine (x.TablebaseText()) |> ignore
          sb.AppendLine (x.AdjudicationText()) |> ignore
          sb.AppendLine (sprintf "Deviations: %d" x.DeviationCounter) |> ignore
          //sb.AppendLine (sprintf "Comment: %s" x.Description) |> ignore
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
          if x.Gauntlet then     
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
          PgnOutPath = ""
          ReferencePGNPath = ""
          Gauntlet = false
          IsChess960 = false
          PreventMoveDeviation = false
          DeviationCounter = 0
          Challengers = 0
          Rounds = 0
          PauseAfterRound = 0
          TimeControl = Unchecked.defaultof<TimeControl.TimeControl>
          EngineSetup = {Engines = []; EngineDefFolder = ""; EngineDefList = [] }
          Opening = {OpeningsPath = None; OpeningsTwice = false; OpeningsPly = 0 }
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
  // POSITION & BITBOARD OPERATIONS
  // -----------------------------------------------------------------------------
  module Position =

    [<Struct>]
    type Position =
      { mutable PM: uint64
        mutable P0: uint64
        mutable P1: uint64
        mutable P2: uint64
        mutable CastleFlags: byte
        mutable EnPassant: byte
        mutable Count50: byte
        mutable Rep: byte
        mutable STM: byte
        mutable Ply: byte
        mutable RookInfo: RookPlacementInfo }
      with
        static member Default =
          { PM = 0UL; P0 = 0UL; P1 = 0UL; P2 = 0UL;
            CastleFlags = 0uy; EnPassant = 8uy; Count50 = 0uy; Rep = 0uy;
            STM = 0uy; Ply = 0uy; RookInfo = RookPlacementInfo() }

    module PositionOps =
      let copy (board: Position inref) : Position =
        { PM = board.PM; P0 = board.P0; P1 = board.P1; P2 = board.P2;
          CastleFlags = board.CastleFlags; EnPassant = board.EnPassant;
          Count50 = board.Count50; Rep = board.Rep; STM = board.STM;
          Ply = board.Ply; RookInfo = board.RookInfo }
      let createEmptyTBoard () =
        { PM = 0UL; P0 = 0UL; P1 = 0UL; P2 = 0UL;
          CastleFlags = 0uy; EnPassant = 0uy; Count50 = 0uy; Rep = 0uy;
          STM = 0uy; Ply = 0uy; RookInfo = RookPlacementInfo() }
    
      // (Additional inline functions for castling, occupancy, etc.)
      let WHITE = 0uy
      let BLACK = 8uy

      let inline CanCastleSM (position: Position inref) = (position.CastleFlags &&& 0x02uy) <> 0uy
      let inline CanCastleLM(position: Position inref) = (position.CastleFlags &&& 0x01uy) <> 0uy
      let inline CanCastleSO (position: Position inref) = (position.CastleFlags &&& 0x20uy) <> 0uy
      let inline CanCastleLO(position: Position inref) = (position.CastleFlags &&& 0x10uy) <> 0uy
      let inline ResetCastleSM(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xFDuy
      let inline ResetCastleLM(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xFEuy
      let inline ResetCastleSO(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xDFuy
      let inline ResetCastleLO(position: outref<Position>) = position.CastleFlags <- position.CastleFlags &&& 0xEFuy

       //these planes are used to calculate the bitboard of a particular kind of piece
       //      P2 P1 P0
       //       0  0  0    empty
       //       0  0  1    pawn
       //       0  1  0    knight
       //       0  1  1    bishop
       //       1  0  0    rook
       //       1  0  1    queen
       //       1  1  0    king

      let inline occupation (pos:Position inref) = pos.P0 ||| pos.P1 ||| pos.P2 // board occupation
      let inline pawns (pos:Position inref) = pos.P0 &&& ~~~pos.P1 &&& ~~~pos.P2 // all the pawns on the board
      let inline knights (pos:Position inref) = ~~~ pos.P0 &&& pos.P1 &&& ~~~ pos.P2
      let inline bishops (pos:Position inref) = pos.P0 &&& pos.P1
      let inline rooks (pos:Position inref) = ~~~ pos.P0 &&& ~~~ pos.P1 &&& pos.P2
      let inline queens (pos:Position inref) = pos.P0 &&& pos.P2
      let inline queenOrRooks (pos:Position inref) = ~~~ pos.P1 &&& pos.P2
      let inline queenOrBishops (pos:Position inref) = pos.P0 &&& (pos.P2 ||| pos.P1)
      let inline kings (pos:Position inref) = pos.P1 &&& pos.P2 // a bitboard with the 2 kings
      let inline sideToMove (pos:Position inref) = pos.PM
      let inline enPass (pos:Position inref) = pos.EnPassant
      let inline opposing (pos:Position inref) = pos.PM ^^^ (pos.P0 ||| pos.P1 ||| pos.P2)
      let inline numberOfPieces (pos:Position inref) = QBBOperations.Pop (occupation &pos) |> int32
      let inline changeSide (pos: Position outref) =
        pos.PM <- pos.PM ^^^ (occupation &pos) // update the side to move pieces
        pos.PM <- QBBOperations.RevBB pos.PM
        pos.P0 <- QBBOperations.RevBB pos.P0
        pos.P1 <- QBBOperations.RevBB pos.P1
        pos.P2 <- QBBOperations.RevBB pos.P2
        //let tempCastleFlags = pos.CastleFlags
        pos.CastleFlags <- byte ((pos.CastleFlags >>> 4) ||| (pos.CastleFlags <<< 4)) // roll the castle rights
        pos.STM <- pos.STM ^^^ BLACK // change the side to move      

      let inline rooksM (pos:Position inref) = rooks &pos &&& pos.PM &&& firstRank //Rooks for player to move
      let inline rooksO (pos:Position inref) = rooks &pos &&& ~~~pos.PM &&& lastRank //Rooks for opponent
    
      let isStandardChessPosition (pos: Position inref) =      
        let kings = kings &pos
        let myKsq = QBBOperations.MSB (kings &&& pos.PM)
        let oppKsq = QBBOperations.MSB (kings &&& ~~~pos.PM)
        let weCanCastle = CanCastleSM &pos || CanCastleLM &pos
        let opponentCanCastle = CanCastleSO &pos || CanCastleLO &pos
        let canCastle = weCanCastle || opponentCanCastle     
        if not canCastle then
            true
        else
               // Check standard king positions
            let isNormalMyKingPosition = myKsq = 4UL
            let isNormalOppkKingPosition = oppKsq = 60UL          

            // Initialize standardChess to true
            let mutable standardChess = true
          
            if pos.STM = WHITE then
              if CanCastleSM &pos then
                standardChess <- standardChess && pos.RookInfo.WhiteKRInitPlacement = 7uy && isNormalMyKingPosition

              // Check if White can castle queenside and if the rook is in the initial position
              if CanCastleLM &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteQRInitPlacement = 0uy && isNormalMyKingPosition

              // Check if black can castle kingside and if the rook is in the initial position
              if CanCastleSO &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackKRInitPlacement = 7uy && isNormalOppkKingPosition

              // Check if Black can castle queenside and if the rook is in the initial position
              if CanCastleLO &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackQRInitPlacement = 0uy && isNormalOppkKingPosition

            else             
              if CanCastleSM &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackKRInitPlacement = 7uy && isNormalMyKingPosition

              // Check if black can castle queenside and if the rook is in the initial position
              if CanCastleLM &pos then
                  standardChess <- standardChess && pos.RookInfo.BlackQRInitPlacement = 0uy && isNormalMyKingPosition
            
              // Check if White can castle kingside and if the rook is in the initial position
              if CanCastleSO &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteKRInitPlacement = 7uy && isNormalOppkKingPosition

              // Check if Black can castle queenside and if the rook is in the initial position
              if CanCastleLO &pos then
                  standardChess <- standardChess && pos.RookInfo.WhiteQRInitPlacement = 0uy && isNormalOppkKingPosition

            // Return true if all conditions for standard chess are met, otherwise false
            standardChess    
    
      let isFRC (pos: Position inref) = isStandardChessPosition &pos |> not      

      //get the rook index from the bitboard where 0 is a1 , 7 is h1, 56 is a8 and 63 is h8
      let getRookPositionsForCastling (pos:Position inref) =      
        let mKingSq = QBBOperations.MSB (kings &pos &&& pos.PM &&& firstRank)
        let oKingSq = QBBOperations.MSB (kings &pos &&& ~~~pos.PM &&& lastRank)
        let rookM = rooksM &pos
        let rookO = rooksO &pos
        let ksM = QBBOperations.MSB rookM
        let qsM = QBBOperations.LSB rookM
        let ksO = QBBOperations.MSB rookO
        let qsO = QBBOperations.LSB rookO
        let nRooksM = QBBOperations.Pop rookM
        let nRooksO = QBBOperations.Pop rookO
        let mKr, mQr =
            if nRooksM > 1UL then
                (int ksM, int qsM)
            elif nRooksM = 1UL && mKingSq < ksM then
                (int ksM, 15)
            elif nRooksM = 1UL && mKingSq > ksM then
                (15, int qsM)
            else
                (15, 15)
        let oKr, oQr =
            if nRooksO > 1UL then
                (int ksO, int qsO)
            elif nRooksO = 1UL && oKingSq < ksO then
                (int ksO, 15)
            elif nRooksO = 1UL && oKingSq > ksO then
                (15, int qsO)
            else
                (15, 15)
        if pos.STM = WHITE then 
          (mKr,mQr),(oKr,oQr) 
        else 
          (oKr,oQr),(mKr,mQr)
      

      
      let printBits debugMsg (value: uint64) =
        let binaryString = Convert.ToString(int64 value, 2)
        printfn "%s %d -> %s" debugMsg value binaryString

  // -----------------------------------------------------------------------------
  // TPIECE & TMOVE (Bitboard move representations)
  // -----------------------------------------------------------------------------
  module TMove =
    [<Flags>]
    type TPieceType =
        | EMPTY = 0uy
        | PAWN = 1uy
        | KNIGHT = 2uy
        | BISHOP = 3uy
        | ROOK = 4uy
        | QUEEN = 5uy
        | KING = 6uy
        | PIECE_MASK = 0x07uy
        | CASTLE = 0x40uy
        | PROMO = 0x20uy
        | EP = 0x10uy
        | CAPTURE = 0x08uy

    module TPieceType =
      let pieceTraversal = [| TPieceType.KING; TPieceType.QUEEN; TPieceType.ROOK; TPieceType.BISHOP; TPieceType.KNIGHT |]
      let piecePromoTraversal = [| TPieceType.QUEEN; TPieceType.ROOK; TPieceType.BISHOP; TPieceType.KNIGHT |]
      let symbolFromPieceType (pieceType: uint64) color =
        let piece = byte pieceType |> LanguagePrimitives.EnumOfValue
        match piece with
        | TPieceType.EMPTY -> '.'
        | TPieceType.PAWN -> if color = 8uy then 'p' else 'P'
        | TPieceType.KNIGHT -> if color = 8uy then 'n' else 'N'
        | TPieceType.BISHOP -> if color = 8uy then 'b' else 'B'
        | TPieceType.ROOK -> if color = 8uy then 'r' else 'R'
        | TPieceType.QUEEN -> if color = 8uy then 'q' else 'Q'
        | TPieceType.KING -> if color = 8uy then 'k' else 'K'
        | _ -> failwith "Unknown piece type"
      let Piece (sq: int, position: Position.Position inref) =
        ((position.P2 >>> sq &&& 1UL) <<< 2) |||
        ((position.P1 >>> sq &&& 1UL) <<< 1) |||
        (position.P0 >>> sq &&& 1UL)

    [<Struct>]
    type TMove =
      { MoveType: TPieceType
        From: byte
        To: byte
        Promotion: TPieceType }
    module TMoveOps =
      let inline getPiece (move: TMove) = move.MoveType &&& TPieceType.PIECE_MASK
      let inline isCaptureMove (move: TMove) = (move.MoveType &&& TPieceType.CAPTURE) <> TPieceType.EMPTY
      let inline isCastlingMove (move: TMove) = (move.MoveType &&& TPieceType.CASTLE) <> TPieceType.EMPTY
      let inline isPromotionMove (move: TMove) = move.Promotion <> TPieceType.EMPTY
      let inline isEnPassantMove (move: TMove) = (move.MoveType &&& TPieceType.EP) <> TPieceType.EMPTY
      let inline isEmptyMove (move: TMove) = move.MoveType = TPieceType.EMPTY
      let inline isKingMove (move: TMove) = getPiece move = TPieceType.KING
      let inline isQueenMove (move: TMove) = getPiece move = TPieceType.QUEEN
      let inline isRookMove (move: TMove) = getPiece move = TPieceType.ROOK
      let inline isBishopMove (move: TMove) = getPiece move = TPieceType.BISHOP
      let inline isKnightMove (move: TMove) = getPiece move = TPieceType.KNIGHT
      let inline isPawnMove (move: TMove) = getPiece move = TPieceType.PAWN
      let inline isPawnCaptureMove (move: TMove) = isCaptureMove move && isPawnMove move
      let inline isKingCastleMove (move: TMove) = isCastlingMove move && isKingMove move
      let inline getPromoPiece (move: TMove) = move.Promotion &&& TPieceType.PIECE_MASK
      //get piece on square
      let inline getPieceOnSquare (pos:Position.Position inref) (sq:byte) =
        let sq = int sq
        let piece = 
          ((pos.P2 >>> sq &&& 1UL) <<< 2) ||| 
          ((pos.P1 >>> sq &&& 1UL) <<< 1) ||| 
          (pos.P0 >>> sq &&& 1UL)
        piece

      let inline removeSpecialEndSymbols (input: string) = input.TrimEnd([|'+'; '#';'!';'?'|])
  
      let pieceSymbolFromPieceType (piece:TPieceType) =    
        match piece with
        | TPieceType.EMPTY -> ""
        | TPieceType.PAWN -> ""
        | TPieceType.KNIGHT -> "N"
        | TPieceType.BISHOP -> "B"
        | TPieceType.ROOK -> "R"
        | TPieceType.QUEEN -> "Q"
        | TPieceType.KING -> "K"
        | _ -> failwith "Unknown piece type"  

      //the inverse of the above function

      let pieceTypeFromSymbol (symbol:char) =
        match symbol with    
        | 'N' -> TPieceType.KNIGHT
        | 'B' -> TPieceType.BISHOP
        | 'R' -> TPieceType.ROOK
        | 'Q' -> TPieceType.QUEEN
        | 'K' -> TPieceType.KING
        | 'a' -> TPieceType.PAWN
        | 'b' -> TPieceType.PAWN
        | 'c' -> TPieceType.PAWN
        | 'd' -> TPieceType.PAWN
        | 'e' -> TPieceType.PAWN
        | 'f' -> TPieceType.PAWN
        | 'g' -> TPieceType.PAWN
        | 'h' -> TPieceType.PAWN
        |_ -> TPieceType.EMPTY

      let createMove (moveType:TPieceType) (from:byte) (to_:byte) (promotion:TPieceType) =
        {
          MoveType = moveType
          From = from
          To = to_
          Promotion = promotion
        }

      let createEmptyMove () =
        {
          MoveType = TPieceType.EMPTY
          From = 0uy
          To = 0uy
          Promotion = TPieceType.EMPTY
        }

      //Since promotion value (TPieceType) Empty and Pawn are not allowed during promotion, we keep empty values for them
      let promo = [|' '; ' '; 'n'; 'b'; 'r'; 'q'|]
 
      let charSet = Set.ofList ['n'; 'b'; 'r'; 'q'; 'N'; 'B'; 'R'; 'Q'; '=']

      let empty = { MoveType = TPieceType.EMPTY; From=0uy; To=0uy; Promotion=TPieceType.EMPTY }

      let moveToStr (move: TMove inref) (sideToMove: byte) =
        let result = new StringBuilder(6)
        let from = AbsSq(int move.From, int sideToMove)
        let to_ = AbsSq(int move.To, int sideToMove)
        result.Append(char(int('a') + from % 8)) |> ignore
        result.Append(char(int('1') + from / 8)) |> ignore
        result.Append(char(int('a') + to_ % 8)) |> ignore
        result.Append(char(int('1') + to_ / 8)) |> ignore
        result.Append(promo.[int move.Promotion]) |> ignore
        result.ToString().TrimEnd()

      let dictNameToNumber (stm: inref<byte>) = if stm = 0uy then QBBOperations.squareNameToNumberDictWhite else QBBOperations.squareNameToNumberDictBlack
      let dictNumberToName (stm: inref<byte>) = if stm = 0uy then QBBOperations.squareNumberToNameDictWhite else QBBOperations.squareNumberToNameDictBlack

      //get TMove from SAN string
      let getTMoveFromShortSan (sanShort: string) (moves : TMove array) stm checkIsLegal =     
        let piece = pieceTypeFromSymbol sanShort.[0]
        let isCapture = sanShort.Contains("x")
        let isCastling = sanShort.Contains("0-0") || sanShort.Contains("O-O") || sanShort.Contains("0-0-0") || sanShort.Contains("O-O-O")
        let adjustedSan = removeSpecialEndSymbols sanShort
        let lastChar = adjustedSan.[adjustedSan.Length - 1]
        let isPromotion = charSet.Contains lastChar          
    
        if isCastling then
          if adjustedSan = "0-0" || adjustedSan = "O-O" then        
            let castlingShort = moves |> Array.tryFind(fun m -> isCastlingMove m && m.To > m.From)
            castlingShort
          elif adjustedSan = "0-0-0" || adjustedSan = "O-O-O" then        
            //let filter = moves |> Array.filter(fun m -> isCastlingMove m && m.To < m.From)
            let castlingShort = moves |> Array.tryFind(fun m -> isCastlingMove m && m.To < m.From)
            castlingShort   
          else
            failwith $"Invalid castling move: {sanShort}"
    
        else      
          let nameToSqNumber = dictNameToNumber &stm
          let moveType, promo, dest = 
            if isPromotion && isCapture then
              let dest = nameToSqNumber.Item (adjustedSan.[2 .. 3]) |> byte
              TPieceType.PAWN ||| TPieceType.PROMO ||| TPieceType.CAPTURE , pieceTypeFromSymbol adjustedSan.[adjustedSan.Length - 1], dest       
            elif isPromotion then
              let dest = nameToSqNumber.Item (adjustedSan.[0 .. 1]) |> byte        
              TPieceType.PAWN ||| TPieceType.PROMO, pieceTypeFromSymbol adjustedSan.[adjustedSan.Length - 1], dest  
            elif isCapture then
              let dest = nameToSqNumber.Item (adjustedSan.[adjustedSan.Length - 2 .. adjustedSan.Length - 1]) |> byte        
              piece ||| TPieceType.CAPTURE, TPieceType.EMPTY, dest 
            else
              let dest = nameToSqNumber.Item (adjustedSan.[adjustedSan.Length - 2 .. adjustedSan.Length - 1]) |> byte        
              piece, TPieceType.EMPTY, dest    
    
          let start = moves |> Array.filter(fun m -> m.To = dest && getPiece m = piece)    
      
          if start.Length = 1 then
            Some start.[0]
          else
          
            let start1 = start |> Array.filter (fun m -> m.MoveType = moveType && m.Promotion = promo && checkIsLegal m)
            if start1.Length = 1 then
              Some start1[0]
            else 
              //disambiguate by file or rank
              let getDisambig (san:string) = 
                match piece with
                | TPieceType.PAWN -> san.[0]
                | _ -> san.[1]
            
              let isUniqueFile move san = (moveToStr &move stm).[0] = getDisambig san            
              let start2 = start1 |> Array.filter(fun m -> isUniqueFile m adjustedSan && checkIsLegal m )
              if start2.Length = 1 then
                start2.[0] |> Some
              else //check rank               
                let isUniqueRank move san = (moveToStr &move stm).[1] = getDisambig san
                let start3 = start1 |> Array.filter(fun m -> isUniqueRank m adjustedSan && checkIsLegal m )
                let trank = start |> Array.filter(fun m -> isUniqueRank m adjustedSan && checkIsLegal m )
                let tfile = start |> Array.filter(fun m -> isUniqueFile m adjustedSan && checkIsLegal m )
                if start3.Length = 1 then
                  start3.[0] |> Some
                elif tfile.Length = 1 then
                  tfile.[0] |> Some
                elif trank.Length = 1 then
                  trank.[0] |> Some
                else 
                  let f sq = (dictNumberToName &stm).Item (int sq)
                  let fromSq = start |> Array.tryFind (fun m -> adjustedSan.Contains(f m.From) && checkIsLegal m)
                  let toSq = start |> Array.tryFind (fun m -> adjustedSan.Contains(f m.To) && checkIsLegal m)                
                  let maybe =
                    match fromSq, toSq with
                    | Some f, Some t ->
                      if f.To = t.To then
                        //printfn "Ambiguous move: %s" adjustedSan
                        Some f
                      else
                        None
                    
                    //| Some f, None -> 
                    //  //found only from
                    //  Some f
                    //| None, Some t -> 
                    //  //found only to
                    //  Some t
                    |_ -> None
                  if maybe.IsSome then
                    maybe                
                
                  else
                    let sizeTrank = trank.Length
                    let sizeTfile = tfile.Length
                    let sizeStart = start.Length
                    printfn "SanMove: %s -> start: %d, trank: %d, tfile: %d" sanShort sizeStart sizeTrank sizeTfile                  
                    start |> Array.tryFind(fun m -> checkIsLegal m )
    
        ///The order of standard SAN string is:
        ///1. piece symbol (if not pawn) - if pawn then use the file
        ///2. capture symbol (if capture)
        ///3. destination square (always) in algebraic notation
        ///4. promotion symbol (if promotion)
      let getShortSan fileOrRank (move:TMove) stm =
        let isCapture = isCaptureMove move
        let numberToName = dictNumberToName &stm  // if stm = 0uy then QBBOperations.squareNumberToNameDictWhite else QBBOperations.squareNumberToNameDictBlack    
        let shortMoveStart = numberToName.[int move.From]
        let shortMoveDest = numberToName.[int move.To]
        let piece = getPiece move
        let mutable san = ""
        if isCastlingMove move then      
          if move.From < move.To then
            san <- "0-0" //"O-O"
          else
            san <- "0-0-0" //"O-O-O"    
        elif piece = TPieceType.PAWN then
          if isCapture || isEnPassantMove move  then
            san <- sprintf "%cx%s" shortMoveStart[0] shortMoveDest      
          else
            san <- sprintf "%s" shortMoveDest      
        else
          let pieceSymbol = pieceSymbolFromPieceType piece
          if isCapture then
            san <- sprintf "%s%sx%s" pieceSymbol fileOrRank shortMoveDest
          else
            san <- sprintf "%s%s%s" pieceSymbol fileOrRank shortMoveDest
    
        if isPromotionMove move then
          let promoPiece = getPromoPiece move
          let promoSymbol = pieceSymbolFromPieceType promoPiece
          san <- sprintf "%s=%s" san promoSymbol    
        san
    

      // get sanLong from Tmove
      let getSanLong (move: TMove) side = moveToStr &move side  
  
      let getTmoveFromSanMove (moves : TMove array) (moveLong: string) stm = 
        moves |> Array.tryFind(fun m -> getSanLong m stm = moveLong)
    
      let pieceOnSquare p sq = getPieceOnSquare &p sq

      let foundRookOnSquare p (moves : TMove array) = 
          let piece = TPieceType.ROOK |> uint64       
          moves
          |> Array.tryFind (fun m -> (pieceOnSquare p m.To) = piece)        
      
      let getSameCastlingMoveSanMove (moves : TMove array) (moveLong: string) stm = 
        moves     
        |> Array.filter(fun m -> isCastlingMove m)      

      let getShortSanMoveFromTmove (moves : TMove array) (move: TMove) (pos : Position.Position) =
        if isCastlingMove move then      
          if move.To < move.From then
            "0-0-0"
          elif move.To > move.From then
            "0-0"            
          //elif move.To = move.From then
          //  if move.From = 2uy then "0-0-0" else "0-0"
          else     
            failwith "Invalid castling move"
        else
          let movedPiece = getPiece move
          let promoPiece = getPromoPiece move
          let castling = isCastlingMove move
          let numberToSq = dictNumberToName &pos.STM //if pos.STM = 0uy then QBBOperations.squareNumberToNameDictWhite else QBBOperations.squareNumberToNameDictBlack
          let allMovesToSameSquare = 
            moves |> Array.filter(fun m -> m.To = move.To && getPiece m = movedPiece && promoPiece = m.Promotion && castling = isCastlingMove m )
        
          //debugging
          //let files =  
          //  allMovesToSameSquare 
          //  |> Array.map (fun m -> numberToSq.[int m.From][0] )
          //let ranks =  
          //  allMovesToSameSquare 
          //  |> Array.map (fun m -> numberToSq.[int m.From].[1])
          //let distF = files |> Array.distinct |> Array.length
          //let distR = ranks |> Array.distinct |> Array.length
        
          if allMovesToSameSquare.Length > 1 then
            //check if the moves are from the same file
            let sq = numberToSq.[int move.From]
            let isFileAmbiguous = 
              allMovesToSameSquare 
              |> Array.filter (fun m -> numberToSq.[int m.From].[0] = sq.[0])           
              |> Array.length > 1

            //if file is ambiguous - we need to use rank instead
            if isFileAmbiguous then
              let isRankAmbiguous = 
                allMovesToSameSquare 
                |> Array.filter (fun m -> numberToSq.[int m.From].[1] = sq.[1])             
                |> Array.length > 1
              if isRankAmbiguous then
                //both file and rank are ambiguous - we need to use both
                let both = numberToSq.[int move.From].ToString()
                //let longSan move = getSanLong move pos.STM
                //let sanMoves = allMovesToSameSquare |> Array.fold (fun acc m -> acc + longSan m + " ") ""                            
                //printfn "Ambiguous move to square : %s all available moves: %s" both sanMoves
                getShortSan both move pos.STM
              else
                let rank = numberToSq.[int move.From].[1].ToString()          
                getShortSan rank move pos.STM
            else //we need to use file
              let file = numberToSq.[int move.From].[0].ToString()
              getShortSan file move pos.STM
          else      
            getShortSan "" move pos.STM

      // (Additional functions for converting moves to SAN strings, disambiguation, etc.)

  // -----------------------------------------------------------------------------
  // EPD TYPES
  // -----------------------------------------------------------------------------
  module EPD =
    type EPDEntry =
      { RawInput: string
        FEN: string
        BestMove: string option
        AvoidMove: string option
        Id: string option
        Other: string option }
      with 
        static member Empty = { RawInput = ""; FEN = "" ; BestMove = None; AvoidMove = None; Id = None; Other = None }
        override this.ToString() = this.RawInput


    type TablebaseEPDEntry =
      { EPD: EPDEntry
        TBAnswer: int
        QAnswer: float
        Move: string }
      with 
        static member create epd tbA Qa move =
          { EPD = epd; TBAnswer = tbA; QAnswer = Qa; Move = move }

    type TBscore =
      { Name: string
        FailedPuzzles: ResizeArray<TablebaseEPDEntry>
        CorrectPuzzles: ResizeArray<TablebaseEPDEntry>
        TotalNumber: int
        Correct: int
        Wrong: int
        Rating: float }

  // -----------------------------------------------------------------------------
  // PUZZLE TYPES & FUNCTIONS
  // -----------------------------------------------------------------------------
  module Puzzle =    
    open System.Text.Json

    type PuzzleEngine =
    |Engine of ConfigName: string * Nodes: int
    |EngineWithNets of ConfigName: string * Nodes: int * ListOfNetsWithPaths: string list

    type PuzzleConfig =
        { 
            PuzzleFile: string
            Type: string
            MaxRating: int
            MinRating: int
            RatingGroups: string
            mutable PuzzleFilter: string
            EngineFolder: string
            Engines: ResizeArray<PuzzleEngine>        
            SampleSize: int
            Nodes: string
            //add an entry for path to write to file here
            FailedPuzzlesOutputFolder: string
            Failed: int
            Solved: int
            mutable Concurrency: int    }
    
    type EretConfig = {
        EngineFolder: string
        Engines: ResizeArray<PuzzleEngine>
        PuzzleFile: string
        SampleSize: int
        Nodes: int
        TimeInSeconds: int
        RunWithNodeLimit : bool
        FailedPuzzlesOutputFolder: string
      }
        with 
        static member empty =
            { 
              EngineFolder = ""
              Engines = ResizeArray<PuzzleEngine>()
              PuzzleFile = ""
              SampleSize = 1000
              Nodes = 1
              TimeInSeconds = 10
              RunWithNodeLimit = false
              FailedPuzzlesOutputFolder = ""
            }

    type AnalyzeConfig = {
        EngineFolder: string
        Engines: ResizeArray<PuzzleEngine>        
        Nodes: int
        ChartLines: int
    }
      with 
        static member empty =
             { 
              EngineFolder = ""
              Engines = ResizeArray<PuzzleEngine>()
              Nodes = 1
              ChartLines = 4
             }

    type PuzzleEngineConverter() =
          inherit JsonConverter<PuzzleEngine>()

          override _.Write(writer: Utf8JsonWriter, value: PuzzleEngine, _options) : unit =
            writer.WriteStartObject()
            match value with
            | Engine (configName, nodes) ->
                writer.WritePropertyName("Engine")
                writer.WriteStartObject()
                writer.WriteString("ConfigName", configName)
                writer.WriteNumber("Nodes", nodes)
                writer.WriteEndObject()

            | EngineWithNets (configName, nodes, nets) ->
                writer.WritePropertyName("EngineWithNets")
                writer.WriteStartObject()
                writer.WriteString("ConfigName", configName)
                writer.WriteNumber("Nodes", nodes)
                writer.WriteStartArray("ListOfNetsWithPaths")
                for net in nets do
                  writer.WriteStringValue(net)
                writer.WriteEndArray()
                writer.WriteEndObject()

            writer.WriteEndObject()

          override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: Type, _options: JsonSerializerOptions) : PuzzleEngine =
            // Expect the outer object start
            if reader.TokenType <> JsonTokenType.StartObject then
              raise (JsonException("Expected StartObject"))

            // Move into the first property (the case name)
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.PropertyName then
              raise (JsonException("Expected a DU‐case property"))

            // Grab the case name: "Engine" or "EngineWithNets"
            let caseName = reader.GetString()

            // Advance into the inner object
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.StartObject then
              raise (JsonException("Expected StartObject for case payload"))

            // Read the contents of the inner object
            let mutable configName = ""
            let mutable nodes = 0
            let nets = ResizeArray<string>()
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
              if reader.TokenType = JsonTokenType.PropertyName then
                let prop = reader.GetString()
                reader.Read() |> ignore
                match prop with
                | "Nodes" ->
                    nodes <- reader.GetInt32()
                | "ConfigName" ->
                    configName <- reader.GetString()
                | "ListOfNetsWithPaths" ->
                    if reader.TokenType <> JsonTokenType.StartArray then
                      raise (JsonException("Expected StartArray for ListOfNetWithPaths"))
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                      nets.Add(reader.GetString())
                | _ ->
                    reader.Skip()

            // Now we’re at the inner EndObject; advance to the outer EndObject
            reader.Read() |> ignore

            // Dispatch on the case name
            match caseName with
            | "Engine" ->
                Engine (configName, nodes)
            | "EngineWithNets" ->
                EngineWithNets(configName, nodes, List.ofSeq nets)
            | other ->
                raise (JsonException($"Unknown PuzzleEngine case: {other}"))

    type ERETResults = {
        PlayerName: string
        CorrectPuzzles: ResizeArray<EPD.EPDEntry>
        FailedPuzzles: ResizeArray<EPD.EPDEntry*string>
        Accuracy: float
        Desc: string
    }

    type ERET =    
        | Start of Info: string
        | Puzzle of EPD: EPD.EPDEntry * Correct: bool
        | PlayerResult of ERETResults
        | AllResults of ERETResults list
        | ResultsInConsole of string

    type BlunderRecord =
      { Date: DateTime
        FEN: string
        Source: string option
        Lc0Version: string option
        NetworkUsed: string
        Nodes: int64
        IncorrectMove: string
        CorrectMove: string
        BlunderType: string
        Comments: string option
        DiscordContact: string option }

    type PlayerRecord = { Rating: float; Deviation: float; Volatility: float }

    type Position = { Command: string; CorrectMove: string; MovePlayed: string }

    //Define the engine‐actor message type
    type EngineMsg =
        | Ok      of AsyncReplyChannel<bool>        
        | BestMove  of cmd:Position * AsyncReplyChannel<string*float>
        | BestMoveWithPolicy  of cmd:Position * correct:string * AsyncReplyChannel<string * string>
        | Quit      of AsyncReplyChannel<unit>
        | Network   of AsyncReplyChannel<string>

    type CsvPuzzleData =
      { PuzzleId: int
        Fen: string
        Moves: string
        Rating: float
        RatingDeviation: float
        Popularity: int
        NbPlays: int
        Themes: string
        GameUrl: string
        OpeningTags: string
        Puzzle: string
        Commands: Position seq
        Fens: string seq
        Index: int }
      with 
        static member Create(puzzleId, fen, moves, rating, ratingDev, popular, nbPlays, themes, gameUrl, openingTags, puzzle, commands, fens, index) =
          { PuzzleId = puzzleId
            Fen = fen
            Moves = moves
            Rating = rating
            RatingDeviation = ratingDev
            Popularity = popular
            NbPlays = nbPlays
            Themes = themes
            GameUrl = gameUrl
            OpeningTags = openingTags
            Puzzle = puzzle
            Commands = commands
            Fens = fens
            Index = index }

    type PuzzleResult =
      { PuzzleData : CsvPuzzleData
        WasCorrect : bool
        MovePlayed : string
        FailedMove : string
        ValueHead: bool
        Policy: string
      }

    type Score =
      { Engine: string
        NeuralNet: string
        TotalNumber: int
        Correct: int
        Wrong: int 
        RatingAvg: float
        Filter: string
        PlayerRecord: PlayerRecord
        FailedPuzzles: ResizeArray<CsvPuzzleData * string>
        CorrectPuzzles: ResizeArray<CsvPuzzleData>
        Nodes: int
        WithHistory: bool
        Type: string }
        with
          static member empty =
            { Engine = ""
              NeuralNet = ""
              TotalNumber = 0
              Correct = 0
              Wrong = 0
              RatingAvg = 0.0
              Filter = ""
              PlayerRecord = { Rating = 0.0; Deviation = 0.0; Volatility = 0.0 }
              FailedPuzzles = ResizeArray<CsvPuzzleData * string>()
              CorrectPuzzles = ResizeArray<CsvPuzzleData>()
              Nodes = 0
              WithHistory = false
              Type = "" }

    type Lichess =      
      | PuzzleResult of Score
      | Done of string

    type Iteration = { Id: int; Positions: CsvPuzzleData seq; Theme: string }

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

    type PuzzleCategory = { Category: string; Description: string }

    let getPuzzleCategoriesAsync = async {
        try
            let url = "https://raw.githubusercontent.com/lichess-org/lila/master/translation/source/puzzleTheme.xml"
            let fetchXmlContentAsync (url: string) = async {
                use client = new HttpClient()
                let! response = client.GetAsync(url) |> Async.AwaitTask
                response.EnsureSuccessStatusCode() |> ignore
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                return content
            }
            let parseXmlToRecords (xmlContent: string) =
                let doc = XDocument.Parse(xmlContent)
                doc.Descendants(XName.Get "string")
                |> Seq.choose (fun element ->
                    let nameAttr = element.Attribute(XName.Get "name")
                    let value = element.Value
                    match nameAttr with
                    | null -> None
                    | attr when attr.Value.EndsWith "Description" ->
                        let category = attr.Value.Replace("Description", "")
                        Some { Category = category; Description = value }
                    | _ -> None)
                |> Seq.toList
            let! xmlContent = fetchXmlContentAsync url
            let puzzleCategories = parseXmlToRecords xmlContent
            return puzzleCategories
        with ex ->
            printfn "Error: %s" ex.Message
            return []
    }
    let getPuzzleCategories() = getPuzzleCategoriesAsync |> Async.RunSynchronously
    let drawPuzzleCategories n =
        let rnd = Random()
        getPuzzleCategories()
        |> Seq.sortBy (fun _ -> rnd.Next())
        |> Seq.truncate n
        |> Seq.map (fun cat -> cat.Category)
        |> String.concat ","

  // -----------------------------------------------------------------------------
  // ENGINE COMMUNICATION & UCI COMMANDS
  // -----------------------------------------------------------------------------
  module Engine =   
    open CoreTypes
    open TimeControl

    type EngineState =
        | Start
        | InBestMoveMode
        | InMoveStatMode of ResizeArray<NNValues>
        | RegularSearchMode
        | UCIMode of Option: ResizeArray<string>

    type UCICommand =
        | UCI
        | PositionWithMoves of command: string
        | Position of fen: string
        | UciNewGame     
        | GoMoveTime of ms: int
        | GoTimeControl of TC: UnionType * wTime: TimeOnly * bTime: TimeOnly
        | GoValue
        | GoNodes of nodes: int
        | GoInfinite
        | Stop
        | Quit
        | SetOption of EngineOption
        | SetOptions of EngineOption seq
        | SetMoveOverhead of optionName: string * milliSeconds: int

    type EngineUpdate =
        | Done of Player: string
        | Ready of Player: string * HasLiveStat: bool
        | Info of Player: string * Info: string
        | Eval of Player: string * Eval: EvalType
        | Status of EngineStatus
        | NNSeq of NNSeq: ResizeArray<NNValues>
        | BestMove of BestMoveInfo
        | BestMoveSimple of Move: string * Ponder: string option
        | UCIInfo of Data: ResizeArray<string>

    type EngineMoveStat = 
      { Player: string
        d: int
        sd: int
        mt: int64
        tl: int64
        s: int64
        n: int64
        wv: float
        tb: int64
        n1: int64
        n2: int64
        q1: float
        q2: float
        p1: float
        pt: float }
      with 
        static member Empty =
          { Player = String.Empty; d = 0; sd = 0; mt = 0L; tl = 0L; s = 0L; n = 0L;
            wv = 0.0; tb = 0L; n1 = 0L; n2 = 0L; q1 = 0.0; q2 = 0.0; p1 = 0.0; pt = 0.0 }

    type EngineStat = { White: string; Black: string; Moves: EngineMoveStat array }

    type ChessMoveInfo =
      { mutable d: int
        mutable sd: int
        mutable pd: string
        mutable mt: int64
        mutable tl: int64
        mutable s: int64
        mutable n: int64
        mutable n1: int64
        mutable n2: int64
        mutable pv: string
        mutable tb: int64
        mutable h: float
        mutable ph: float
        mutable wv: EvalType
        mutable R50: int
        mutable Rd: int
        mutable Rr: int
        mutable mb: string
        mutable q1: float
        mutable q2: float
        mutable p1: float
        mutable pt: float }
      with 
        static member Empty =
          { d = 0; sd = 0; pd = ""; mt = 0L; tl = 0L; s = 0L; n = 0L;
            n1 = 0L; n2 = 0L; pv = ""; tb = 0L; h = 0.0; ph = 0.0; wv = EvalType.NA;
            R50 = 0; Rd = 0; Rr = 0; mb = ""; q1 = 0.0; q2 = 0.0; p1 = 0.0; pt = 0.0 }
        member x.Annotation =
          sprintf "d=%d, sd=%d, pd=%s, mt=%d, tl=%d, s=%d, n=%d, tb=%d, wv=%O, n1=%d, n2=%d, q1=%.2f, q2=%.2f, p1=%.2f, pt=%.2f"
            x.d x.sd x.pd x.mt x.tl x.s x.n x.tb x.wv x.n1 x.n2 x.q1 x.q2 x.p1 x.pt
        member x.MinimalAnnotation = //todo - only add minor info here
          sprintf "d=%d, sd=%d, pd=%s, mt=%d, tl=%d, s=%d, n=%d, tb=%d, wv=%O"
            x.d x.sd x.pd x.mt x.tl x.s x.n x.tb x.wv

    module Annotation =
        open System.Text.RegularExpressions
        let mPvRegex = new Regex(@"\bmultipv\s+(\d+)\b", RegexOptions.Compiled)
        let dRegex = new Regex(@"(?<!s)d=(\d+)", RegexOptions.Compiled)
        let sdRegex = new Regex(@"sd=(\d+)", RegexOptions.Compiled)
        let sRegex = new Regex(@"s=(\d+\s*(kN/s|N/s)?)", RegexOptions.Compiled)
        let nRegex = new Regex(@"n=(\d+)", RegexOptions.Compiled)
        let tbRegex = new Regex(@"tb=(\d+)", RegexOptions.Compiled)
        let mtRegex = new Regex(@"mt=((\d{2}:\d{2}:\d{2})|(\d+))", RegexOptions.Compiled)
        let tlRegex = new Regex(@"tl=(\d+)", RegexOptions.Compiled)
        let n1Regex = new Regex(@"n1=(\d+)", RegexOptions.Compiled)
        let n2Regex = new Regex(@"n2=(\d+)", RegexOptions.Compiled)
        let q1Regex = new Regex(@"q1=(-?\d+\.\d+)", RegexOptions.Compiled)
        let q2Regex = new Regex(@"q2=(-?\d+\.\d+)", RegexOptions.Compiled)
        let p1Regex = new Regex(@"p1=(\d+)", RegexOptions.Compiled)
        let ptRegex = new Regex(@"pt=(\d+)", RegexOptions.Compiled)
        let evalRegex = new Regex(@"wv=(-?\d+(\.\d*)?|-M\d*|M\d*)", RegexOptions.Compiled)
        let evalRegexCeres = new Regex(@"([+-]?\d+\.\d+)/(\d+)\s?(\d+\.\d+)s", RegexOptions.Compiled)
        let banksiaRegex = new Regex(@"([+-]?\d+\.\d+)/(\d+)\s(\d+)\s(\d+)", RegexOptions.Compiled)

        // Parsing helper functions...
        let parseRegex myDefault format line (regex: Regex) =
          let test = regex.Match(line)
          if test.Success then test.Groups.[1].Value |> format else myDefault
        let isTimeFormat (timeStr: string) = timeStr.Contains(":")
        let convertToMilliseconds (timeStr: string) =
          let parts = timeStr.Split(':')
          let hours = int64 parts.[0]
          let minutes = int64 parts.[1]
          let seconds = int64 parts.[2]
          (hours * 3600L + minutes * 60L + seconds) * 1000L
        let formatTimeOrMilliseconds (timeStr: string) =
          if isTimeFormat timeStr then convertToMilliseconds timeStr else int64 timeStr
        let convertToNps (npsStr: string) =
          if npsStr.Contains("kN/s") then (npsStr.Replace("kN/s", "") |> int64) * 1000L
          else int64 npsStr
        let parseEvalRegex line =
          let test = evalRegex.Match(line)
          if test.Success then
              let value = test.Groups.[1].Value
              match value.[0] with
              | '-' when value.Length > 1 && value.[1] = 'M' -> -200.0
              | 'M' -> 200.0
              | _ -> match System.Double.TryParse(value) with | true, num -> num | _ -> 0.0
          else 0.0
        let evalParser line = parseEvalRegex line
        let intParser line regex = parseRegex 0 int line regex
        let int64Parser line regex = parseRegex 0L formatTimeOrMilliseconds line regex
        let floatParser line regex = parseRegex 0.0 float line regex
        let npsParser line regex = parseRegex 0L convertToNps line regex

        let getEngineStatData player isBlack (line: string) =
          if String.IsNullOrEmpty line then
            { EngineMoveStat.Empty with Player = player }
          elif not (evalRegex.IsMatch line) then
            if banksiaRegex.IsMatch line then
              let test = banksiaRegex.Match(line)
              let eval = float test.Groups.[1].Value * (if isBlack then -1.0 else 1.0)
              let depth = int test.Groups.[2].Value
              let time = (int64 test.Groups.[3].Value) / 1000L
              let nodes = int64 test.Groups.[4].Value
              let nps = float nodes / float time
              { EngineMoveStat.Empty with Player = player; wv = eval; n = nodes; mt = time * 1000L; d = depth; s = int64 nps }
            elif evalRegexCeres.IsMatch line then
              let test = evalRegexCeres.Match(line)
              let eval = float test.Groups.[1].Value * (if isBlack then -1.0 else 1.0)
              let depth = int test.Groups.[2].Value
              let time = float test.Groups.[3].Value
              { EngineMoveStat.Empty with Player = player; wv = eval; d = depth; mt = int64 time }
            else
              { EngineMoveStat.Empty with Player = player }
          else
            { EngineMoveStat.Empty with
                Player = player
                d = intParser line dRegex
                sd = intParser line sdRegex
                mt = int64Parser line mtRegex
                tl = int64Parser line tlRegex
                s = npsParser line sRegex
                n = int64Parser line nRegex
                wv = evalParser line
                tb = int64Parser line tbRegex
                n1 = int64Parser line n1Regex
                n2 = int64Parser line n2Regex
                q1 = floatParser line q1Regex
                q2 = floatParser line q2Regex
                p1 = floatParser line p1Regex
                pt = floatParser line ptRegex }

  // -----------------------------------------------------------------------------
  // TESTS & SAMPLE PARSING
  // -----------------------------------------------------------------------------
  module Tests =
    type ChessRecord =
      { Corr: float
        Ceres: float
        BT4: float
        SF: float
        V1: float
        Unc: float
        QDn: float
        QUp: float
        FEN: string }
      override this.ToString() =
        sprintf "Corr: %f Ceres: %f BT4: %f SF: %f v1: %f Unc: %f Qdn: %f Qup: %f" this.Corr this.Ceres this.BT4 this.SF this.V1 this.Unc this.QDn this.QUp

    let parseLine (line: string) =
        let parts = line.Split([|' '; ':'|], StringSplitOptions.RemoveEmptyEntries)
        { ChessRecord.Corr = float parts.[0]
          Ceres = float parts.[2]
          BT4 = float parts.[4]
          SF = float parts.[6]
          V1 = float parts.[7]
          Unc = float parts.[8]
          QDn = float parts.[9]
          QUp = float parts.[10]
          FEN = String.Join(" ", parts.[11..]) }

    let text = """
      1.00  Ceres:  0.99   BT4: 0.97   SF: 1.00      0.95   0.03   0.09   0.04    5rk1/1bqN2bp/p3p1p1/6B1/1Qpr4/2N4P/5PP1/R3R1K1 b - - 0 25
      0.00  Ceres:  0.96   BT4: 0.94   SF: 1.00      0.90   0.04   0.12   0.09    5rk1/1bqr2bp/p3R1p1/6B1/1Qp5/2N4P/5PP1/R5K1 b - - 0 26
      -1.00  Ceres:  0.86   BT4: 0.44   SF: 0.74      0.71   0.10   0.39   0.05    rnbqkb1r/pp1p1ppp/5n2/2pPp3/2P5/8/PP2PPPP/RNBQKBNR w KQkq e6 0 4
      -0.99  Ceres:  0.91   BT4: 0.40   SF: 0.81      0.78   0.07   0.35   0.03    rnbqkb1r/pp3p1p/3p1np1/2pPp3/2P1P3/2N5/PP3PPP/R1BQKBNR w KQkq - 0 6
      """

    let records filePath =
        File.ReadAllLines filePath
        |> Array.map parseLine
        |> Array.toList

  // -----------------------------------------------------------------------------
  // UTILITY: STRING BUILDER POOL
  // -----------------------------------------------------------------------------
  module Utils =

    type StringBuilderPool(initialCapacity: int, maxCapacity: int) =
        let pool = ConcurrentQueue<StringBuilder>()
        let mutable maxCapacity_ = maxCapacity
        do
            for i in 1 .. initialCapacity do
                pool.Enqueue(StringBuilder())
        member this.Get() =
            match pool.TryDequeue() with
            | true, sb -> sb.Clear() |> ignore; sb
            | _ -> StringBuilder()
        member this.Return(sb: StringBuilder) =
            if sb.Capacity > maxCapacity_ then ()
            elif pool.Count < maxCapacity_ then pool.Enqueue(sb)
            else
                match pool.TryDequeue() with
                | true, oldSb -> oldSb.Clear() |> ignore; pool.Enqueue(sb)
                | _ -> ()