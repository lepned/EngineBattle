namespace ChessLibrary

open MiscTypes

/// PGN (Portable Game Notation) related types.
/// Contains Move, Header, PlyMove, GameMetadata, PgnGame, and related types.
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

    // Variation-aware move tree (per half-move).
    type PlyMove =
      { Ply: int
        MoveNumber: int
        Color: string
        San: string
        mutable Comment: string
        Nags: int list
        Variations: ResizeArray<PlyLine> }
    and PlyLine = ResizeArray<PlyMove>

    type GameMetadata =
      { Event: string
        Site: string
        Date: string
        mutable Round: string
        White: string
        Black: string
        Result: string
        Reason: ResultReason
        mutable OpeningHash: string
        GameTime: int64
        Moves: int
        /// Half-moves in the movetext, book included (the PGN PlyCount tag).
        PlyCount: int
        OpeningName: string
        Fen: string
        Deviations: int
        StartEvals: EvalType list
        OtherTags: Header list }
      with
        static member Empty =
          { Event = ""; Site = ""; Date = ""; Round = ""; White = ""; Black = "";
            Result = ""; Reason = ResultReason.NotStarted; OpeningHash = ""; GameTime = 0L;
            Moves = 0; PlyCount = 0; OpeningName = ""; Fen = ""; Deviations = 0; StartEvals = []; OtherTags = [] }
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
        Mainline: PlyLine
        RootVariations: ResizeArray<PlyLine>
        Comments: string
        Fen: string
        Raw: string }
      with
        static member Empty i =
          {
            GameNumber = i
            GameMetaData = GameMetadata.Empty
            Mainline = ResizeArray()
            RootVariations = ResizeArray()
            Comments = ""
            Fen = ""
            Raw = "" }

    type PgnEvaluationResult =
        { Pgn: PgnGame
          MaxEval: float
          EvalDiff: float
          MaxMove: string
          MaxEngine: string
          Summary: string }
        static member Create(pgn: PgnGame, maxEval: float, evalDiff: float, maxMove: string, maxEngine: string, summary: string) =
            { Pgn = pgn
              MaxEval = maxEval
              EvalDiff = evalDiff
              MaxMove = maxMove
              MaxEngine = maxEngine
              Summary = summary }

    type GameResult =
      | WhiteWins
      | BlackWins
      | DrawAgreed


