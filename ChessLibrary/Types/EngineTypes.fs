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
        | GoTimeControl of TC: UnionType * wTime: TimeSpan * bTime: TimeSpan
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
        /// A search that ended without a bestmove: replaced by a newer one, or stopped before its go.
        | SearchStopped of Player: string
        /// The engine stopped answering or exited; a new engine is needed.
        | EngineFailed of Player: string * Reason: string

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
        TimeLeft: TimeSpan
        MoveTime: TimeSpan
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
            TimeLeft = TimeSpan.Zero
            MoveTime = TimeSpan.Zero
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
        member x.FullAnnotation =
          sprintf "wv=%O, mt=%d, s=%d, eps=%d, n=%d, d=%d, sd=%d, pd=%s, tl=%d, tb=%d, pcs=%d, pv=%s, n1=%d, n2=%d, q1=%.2f, q2=%.2f, p1=%.2f, pt=%.2f"
            x.wv x.mt x.s x.eps x.n x.d x.sd x.pd x.tl x.tb x.pcs x.pv x.n1 x.n2 x.q1 x.q2 x.p1 x.pt
        member x.StandardAnnotation =
          sprintf "wv=%O, mt=%d, s=%d, eps=%d, n=%d, d=%d, sd=%d, pd=%s, tl=%d, tb=%d, pv=%s"
            x.wv x.mt x.s x.eps x.n x.d x.sd x.pd x.tl x.tb x.pv
        member x.MinimalAnnotation =
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
        /// An eval token: a mate score as "M5"/"-M5", otherwise centipawns. Invariant because
        /// evalRegex only matches a dot and the writer is sprintf "%.2f" — a locale-sensitive
        /// parse silently returns the 0.0 default instead. Covered by ParserTests.
        let parseEvalToken (value: string) =
          match value.[0] with
          | '-' when value.Length > 1 && value.[1] = 'M' -> -200.0
          | 'M' -> 200.0
          | _ ->
            match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, num -> num
            | _ -> 0.0
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
              parseEvalToken test.Groups.[1].Value
          else 0.0
        let evalParser line = parseEvalRegex line
        let intParser line regex = parseRegex 0 int line regex
        let int64Parser line regex = parseRegex 0L formatTimeOrMilliseconds line regex
        let floatParser line regex = parseRegex 0.0 float line regex
        let npsParser line regex = parseRegex 0L convertToNps line regex

        /// The regex implementation. Kept for two jobs: every comment the fast parser below is not
        /// sure it reads the same way goes here (and every format but EB's own), and it is the
        /// reference the fast parser is tested against (AnnotationParserTests).
        let legacyGetEngineStatData player isBlack (line: string) =
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
                      let eval = parseEvalToken d.Groups.[1].Value
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

        // ── Fast parser for EB's own comments ──────────────────────────────────────────────────
        // "d=29, sd=51, pd=Rc6, mt=3773, tl=8580, s=6335926, n=23905452, tb=14132, wv=1.61, ..."
        // is nearly every comment in an EB PGN, and the regex version above spent up to 16
        // matches and ~6 KB on each - more time than parsing the rest of the PGN (173k comments:
        // 308 ms against 211 ms). This reads each field with IndexOf scans and reproduces the
        // regexes exactly, quirks included: each field is the FIRST place its pattern matches, so
        // `s=` can land inside `eps=` or `pcs=` and `d=` inside `pd=` when a digit follows; `mt=`
        // prefers hh:mm:ss; `wv=` prefers a number to -M/M. What it is not sure of - a non-ASCII
        // character or a line break, a number long enough to overflow, `s=` followed by a space or
        // a unit - goes to the regex version, which then answers (or throws) as it always did.

        exception private Decline

        let inline private isDigit (c: char) = c >= '0' && c <= '9'

        let private isPlainAscii (line: string) =
          let mutable ok = true
          let mutable i = 0
          while ok && i < line.Length do
            let c = line.[i]
            if c > '\u007f' || c = '\n' then ok <- false
            i <- i + 1
          ok

        /// The index after the first `key` that is followed by a digit (and, with `notAfter`, is
        /// not preceded by that character); -1 when there is none.
        let private findKey (line: string) (key: string) (notAfter: char voption) =
          let mutable from = 0
          let mutable result = -1
          while result < 0 && from <= line.Length - key.Length do
            let k = line.IndexOf(key, from, StringComparison.Ordinal)
            if k < 0 then from <- line.Length
            else
              let i = k + key.Length
              let excluded =
                match notAfter with
                | ValueSome c -> k > 0 && line.[k - 1] = c
                | ValueNone -> false
              if i < line.Length && isDigit line.[i] && not excluded then result <- i
              else from <- k + 1
          result

        /// Digits at `i` as int64 and the index after them; Decline past `maxDigits`.
        let private readDigits (line: string) (i: int) (maxDigits: int) =
          let mutable j = i
          let mutable v = 0L
          while j < line.Length && isDigit line.[j] do
            if j - i >= maxDigits then raise Decline
            v <- v * 10L + int64 (int line.[j] - int '0')
            j <- j + 1
          v, j

        /// `key=(\d+)` as int (intParser).
        let private intField line key notAfter =
          match findKey line key notAfter with
          | -1 -> 0
          | i -> int (fst (readDigits line i 9))

        /// `key=(\d+)` as int64 (int64Parser).
        let private int64Field line key =
          match findKey line key ValueNone with
          | -1 -> 0L
          | i -> fst (readDigits line i 18)

        /// `key=(\d+)` as float (floatParser over an integer pattern: p1, pt).
        let private digitsAsFloat line key =
          match findKey line key ValueNone with
          | -1 -> 0.0
          | i -> float (fst (readDigits line i 18))

        /// `key=(-?\d+\.\d+)` as float: the first occurrence where the whole pattern fits.
        let private decimalField (line: string) (key: string) =
          let mutable from = 0
          let mutable result = ValueNone
          while result.IsNone && from <= line.Length - key.Length do
            let k = line.IndexOf(key, from, StringComparison.Ordinal)
            if k < 0 then from <- line.Length
            else
              let start = k + key.Length
              let mutable i = start
              if i < line.Length && line.[i] = '-' then i <- i + 1
              let intStart = i
              while i < line.Length && isDigit line.[i] do i <- i + 1
              if i > intStart && i + 1 < line.Length && line.[i] = '.' && isDigit line.[i + 1] then
                i <- i + 1
                while i < line.Length && isDigit line.[i] do i <- i + 1
                if i - start > 30 then raise Decline
                result <- ValueSome (float (line.Substring(start, i - start)))
              else from <- k + 1
          match result with
          | ValueSome v -> v
          | ValueNone -> 0.0

        /// `mt=((\d{2}:\d{2}:\d{2})|(\d+))` in milliseconds.
        let private mtField (line: string) =
          match findKey line "mt=" ValueNone with
          | -1 -> 0L
          | i ->
              let two j = int64 (int line.[j] - int '0') * 10L + int64 (int line.[j + 1] - int '0')
              let clock =
                i + 7 < line.Length
                && isDigit line.[i + 1] && line.[i + 2] = ':'
                && isDigit line.[i + 3] && isDigit line.[i + 4] && line.[i + 5] = ':'
                && isDigit line.[i + 6] && isDigit line.[i + 7]
              if clock then (two i * 3600L + two (i + 3) * 60L + two (i + 6)) * 1000L
              else fst (readDigits line i 18)

        /// The regex's \s on ASCII (and the whitespace Int64.Parse allows at the end).
        let inline private isRegexSpace (c: char) = c = ' ' || (c >= '\009' && c <= '\013')

        /// `s=(\d+\s*(kN/s|N/s)?)` through convertToNps: digits, then spaces, then an optional
        /// unit; "kN/s" multiplies by 1000. A bare "N/s" made the regex version throw
        /// (int64 "123 N/s"), so that one still goes there. TCEC and CCC write "s=125680 kN/s" on
        /// every move; sending those to the regex (as this did at first) made their comments
        /// slower than before.
        let private sField (line: string) =
          match findKey line "s=" ValueNone with
          | -1 -> 0L
          | i ->
              let v, j = readDigits line i 18
              let mutable k = j
              while k < line.Length && isRegexSpace line.[k] do k <- k + 1
              if String.CompareOrdinal(line, k, "kN/s", 0, 4) = 0 then v * 1000L
              elif String.CompareOrdinal(line, k, "N/s", 0, 3) = 0 then raise Decline
              else v

        /// `wv=(-?\d+(\.\d*)?|-M\d*|M\d*)` through parseEvalToken; ValueNone when no occurrence
        /// fits (then the comment is not in EB's format).
        let private wvField (line: string) =
          let at j = if j < line.Length then line.[j] else ' '
          let mutable from = 0
          let mutable result = ValueNone
          while result.IsNone && from <= line.Length - 3 do
            let k = line.IndexOf("wv=", from, StringComparison.Ordinal)
            if k < 0 then from <- line.Length
            else
              let start = k + 3
              let numberFrom =
                if isDigit (at start) then start
                elif at start = '-' && isDigit (at (start + 1)) then start + 1
                else -1
              if numberFrom >= 0 then
                let mutable i = numberFrom
                while isDigit (at i) do i <- i + 1
                if at i = '.' then
                  i <- i + 1
                  while isDigit (at i) do i <- i + 1
                if i - start > 30 then raise Decline
                match Double.TryParse(line.AsSpan(start, i - start), Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | true, num -> result <- ValueSome num
                | _ -> result <- ValueSome 0.0
              elif at start = '-' && at (start + 1) = 'M' then result <- ValueSome -200.0
              elif at start = 'M' then result <- ValueSome 200.0
              else from <- k + 1
          result

        let private fastEngineStatData player (line: string) =
          match wvField line with
          | ValueNone -> ValueNone
          | ValueSome wv ->
              ValueSome
                { Player = player
                  d = intField line "d=" (ValueSome 's')
                  sd = intField line "sd=" ValueNone
                  mt = mtField line
                  tl = int64Field line "tl="
                  s = sField line
                  eps = int64Field line "eps="
                  n = int64Field line "n="
                  wv = wv
                  tb = int64Field line "tb="
                  n1 = int64Field line "n1="
                  n2 = int64Field line "n2="
                  q1 = decimalField line "q1="
                  q2 = decimalField line "q2="
                  p1 = digitsAsFloat line "p1="
                  pt = digitsAsFloat line "pt="
                  pcs = intField line "pcs=" ValueNone }

        /// The engine data in a move comment: EB's own "wv=... d=... n=..." (the fast parser), or
        /// the Banksia, Ceres and "+0.28/12 1.2s" forms other GUIs write (the regexes). Same
        /// answers as legacyGetEngineStatData.
        let getEngineStatData player isBlack (line: string) =
          if String.IsNullOrEmpty line || not (isPlainAscii line) then legacyGetEngineStatData player isBlack line
          else
            try
              match fastEngineStatData player line with
              | ValueSome stat -> stat
              // No eval in EB's form and not a single digit ("book", "Book exit"): every other
              // format needs a digit, so the regex version would find nothing either.
              | ValueNone when line.AsSpan().IndexOfAnyInRange('0', '9') < 0 -> { EngineMoveStat.Empty with Player = player }
              | ValueNone -> legacyGetEngineStatData player isBlack line
            with Decline -> legacyGetEngineStatData player isBlack line
