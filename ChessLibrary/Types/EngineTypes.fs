namespace ChessLibrary

open System
open System.Text.RegularExpressions
open MiscTypes
open TimeControlTypes
open PGNTypes

/// Engine communication and UCI command types.
/// Contains EngineState, UCICommand, EngineUpdate, and related types.
module EngineTypes =

    type EngineState =
        | Start
        | InBestMoveMode
        | InMoveStatMode of ResizeArray<NNValues>
        | RegularSearchMode
        | UCIMode of Option: ResizeArray<string>

    and NNValues =
      { Player: string
        mutable SANMove: string
        mutable LANMove: string
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

    type EngineOption = { Name: string; Value: string }
      with
        static member Create name value = { Name = name; Value = value }

    type UCICommand =
        | UCI
        | RawCommand of command: string
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
        | PolicyDistribution of PgnGame

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

    type EngineStatus =
      { mutable PlayerName: string
        mutable Eval: EvalType
        Nodes: int64
        NPS: float
        EPS: float
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
            EPS = 0.0
            Depth = 0
            SD = 0
            TBhits = 0L
            WDL = WDLType.NotFound
            PV = ""
            PVLongSAN = ""
            MultiPV = 1 }
        static member Create playerName eval nodes nps depth sd tbhits wdl pv pvlongsan multipv =
            {   PlayerName = playerName
                Eval = eval
                Nodes = nodes
                NPS = nps
                EPS = 0.0
                Depth = depth
                SD = sd
                TBhits = tbhits
                WDL = wdl
                PV = pv
                PVLongSAN = pvlongsan
                MultiPV = multipv }

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
        | PolicyDistributionOutCome of ResizeArray<Int32 * (float * string * bool) * (float * string * bool)>

    and MoveAndFen = { Move: MoveDetail; ShortSan: string; FenAfterMove: string }
      with
        static member FirstEntry = { Move = MoveDetail.Empty; ShortSan = ""; FenAfterMove = startPosition }
        static member Init(fen) = { Move = MoveDetail.Empty; ShortSan = ""; FenAfterMove = fen }

    /// For GUI use only.
    and MoveDetail = { LongSan: string; FromSq: string; ToSq: string; Color: string; IsCastling: bool; Comments: string }
      with
        static member Empty = { LongSan = ""; FromSq = ""; ToSq = ""; Color = ""; IsCastling = false; Comments = String.Empty }
        static member Create(longSan, fromsq, tosq, color, iscastling, ?comments) =
          { LongSan = longSan; FromSq = fromsq; ToSq = tosq; Color = color; IsCastling = iscastling; Comments = defaultArg comments String.Empty }

    and BestMoveInfo =
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
        static member Empty =
          { Player = ""
            Move = ""
            Ponder = ""
            Eval = EvalType.NA
            TimeLeft = TimeOnly.MinValue
            MoveTime = TimeOnly.MinValue
            Nodes = 0L
            NPS = 0.0
            FEN = startPosition
            PV = ""
            LongPV = ""
            MoveAndFen = MoveAndFen.FirstEntry
            MoveHistory = ""
            Move50 = 0
            R3 = 0
            PiecesLeft = 32
            AdjDrawML = 0 }

    type EngineMoveStat =
      { Player: string
        d: int
        sd: int
        mt: int64
        tl: int64
        s: int64
        eps: int64
        n: int64
        wv: float
        tb: int64
        n1: int64
        n2: int64
        q1: float
        q2: float
        p1: float
        pt: float
        pcs: int }
      with
        static member Empty =
          { Player = String.Empty; d = 0; sd = 0; mt = 0L; tl = 0L; s = 0L; eps = 0L; n = 0L;
            wv = 0.0; tb = 0L; n1 = 0L; n2 = 0L; q1 = 0.0; q2 = 0.0; p1 = 0.0; pt = 0.0; pcs = 0 }

    type EngineStat = { White: string; Black: string; Moves: EngineMoveStat array }

    type ChessMoveInfo =
      { mutable d: int
        mutable sd: int
        mutable pd: string
        mutable mt: int64
        mutable tl: int64
        mutable s: int64
        mutable eps: int64
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
        mutable pt: float
        mutable pcs: byte }
      with
        static member Empty =
          { d = 0; sd = 0; pd = ""; mt = 0L; tl = 0L; s = 0L; eps = 0L; n = 0L;
            n1 = 0L; n2 = 0L; pv = ""; tb = 0L; h = 0.0; ph = 0.0; wv = EvalType.NA;
            R50 = 0; Rd = 0; Rr = 0; mb = ""; q1 = 0.0; q2 = 0.0; p1 = 0.0; pt = 0.0; pcs = 0uy }
        member x.Annotation =
          sprintf "wv=%O, mt=%d, s=%d, eps=%d, n=%d, d=%d, sd=%d, pd=%s, tl=%d, tb=%d, pcs=%d, pv=%s, n1=%d, n2=%d, q1=%.2f, q2=%.2f, p1=%.2f, pt=%.2f"
            x.wv x.mt x.s x.eps x.n x.d x.sd x.pd x.tl x.tb x.pcs x.pv x.n1 x.n2 x.q1 x.q2 x.p1 x.pt
        member x.MinimalAnnotation =
          sprintf "wv=%O, mt=%d, s=%d, eps=%d, n=%d, d=%d, pcs=%d, sd=%d, pd=%s, tl=%d, tb=%d"
            x.wv x.mt x.s x.eps x.n x.d x.pcs x.sd x.pd x.tl x.tb
        member x.CompactAnnotation =
          sprintf "wv=%O, n=%d, s=%d, mt=%d" x.wv x.n x.s x.mt

    module Annotation =

        let mPvRegex = new Regex(@"\bmultipv\s+(\d+)\b", RegexOptions.Compiled)
        let dRegex = new Regex(@"(?<!s)d=(\d+)", RegexOptions.Compiled)
        let sdRegex = new Regex(@"sd=(\d+)", RegexOptions.Compiled)
        let sRegex = new Regex(@"s=(\d+\s*(kN/s|N/s)?)", RegexOptions.Compiled)
        let epsRegex = new Regex(@"eps=(\d+)", RegexOptions.Compiled)
        let pcsRegex = new Regex(@"pcs=(\d+)", RegexOptions.Compiled)
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
        let mateRegex = new Regex(@"([+-]?\d+(\.\d*)?|-M\d*|M\d*)/(\d+)\s+(\d+(\.\d*)?)s", RegexOptions.Compiled)
        let moveBracketsOptionalBraces =
            new Regex(@"^\s*\{?\s*([+-]?(?:\d+(?:\.\d*)?|\.\d+))(?:/(\d+))?(?:\s+([0-9]*\.?[0-9]+)s)?\s*,\s*tl\s*=\s*([0-9]*\.?[0-9]+)s\s*\}?\s*$",
                RegexOptions.Compiled ||| RegexOptions.IgnoreCase)


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
          else
            // Avoid calling IsMatch and Match twice for the same regex by matching once and reusing the Match result.
            let evalMatch = evalRegex.Match(line)
            if not evalMatch.Success then
              let m = moveBracketsOptionalBraces.Match(line)
              if m.Success then
                let eval = if m.Groups.[1].Success then (float m.Groups.[1].Value * (if isBlack then -1.0 else 1.0)) else 0.0
                let depth = if m.Groups.[2].Success then int m.Groups.[2].Value else 0
                let moveTime = if m.Groups.[3].Success then (int64 (float m.Groups.[3].Value * 1000.0)) else 0L
                let timeLeft = if m.Groups.[4].Success then (int64 (float m.Groups.[4].Value * 1000.0)) else 0L
                { EngineMoveStat.Empty with Player = player; wv = eval; d = depth; mt = moveTime; tl = timeLeft }
              else
                let b = banksiaRegex.Match(line)
                if b.Success then
                  let eval = float b.Groups.[1].Value * (if isBlack then -1.0 else 1.0)
                  let depth = int b.Groups.[2].Value
                  let time = (int64 b.Groups.[3].Value) / 1000L
                  let nodes = int64 b.Groups.[4].Value
                  let nps = if time = 0L then 0.0 else float nodes / float time
                  { EngineMoveStat.Empty with Player = player; wv = eval; n = nodes; mt = time * 1000L; d = depth; s = int64 nps }
                else
                  let c = evalRegexCeres.Match(line)
                  if c.Success then
                    let eval = float c.Groups.[1].Value * (if isBlack then -1.0 else 1.0)
                    let depth = int c.Groups.[2].Value
                    let time = if c.Groups.[3].Success then (int64 (float c.Groups.[3].Value * 1000.0)) else 0L
                    { EngineMoveStat.Empty with Player = player; wv = eval; d = depth; mt = int64 time }
                  else
                    let d = mateRegex.Match(line)
                    if d.Success then
                      let value = d.Groups.[1].Value
                      let eval =
                        match value.[0] with
                        | '-' when value.Length > 1 && value.[1] = 'M' -> -200.0
                        | 'M' -> 200.0
                        | _ -> match System.Double.TryParse(value) with | true, num -> num | _ -> 0.0
                      let depth = int d.Groups.[3].Value
                      let time = float d.Groups.[4].Value
                      { EngineMoveStat.Empty with Player = player; wv = eval * (if isBlack then -1.0 else 1.0); d = depth; mt = int64 (time * 1000.0) }
                    else
                      { EngineMoveStat.Empty with Player = player }
            else
              // When evalRegex matched, use the existing small parsers (they each do their own Match).
                {
                    Player = player
                    d = intParser line dRegex
                    sd = intParser line sdRegex
                    mt = int64Parser line mtRegex
                    tl = int64Parser line tlRegex
                    s = npsParser line sRegex
                    eps = int64Parser line epsRegex
                    n = int64Parser line nRegex
                    wv = evalParser line
                    tb = int64Parser line tbRegex
                    n1 = int64Parser line n1Regex
                    n2 = int64Parser line n2Regex
                    q1 = floatParser line q1Regex
                    q2 = floatParser line q2Regex
                    p1 = floatParser line p1Regex
                    pt = floatParser line ptRegex
                    pcs = intParser line pcsRegex}
