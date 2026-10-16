namespace ChessLibrary

module EPDTypes =
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

  type EpdEvaluationResult =
      { EPD: EPDEntry
        MaxEval: float
        /// MaxEval as shown: pawns (0.95) or a mate (M1, -M2), or by WDL "W62 D31 L7"; White's view
        MaxEvalText: string
        EvalDiff: float
        MaxMove: string
        MaxEngine: string
        Summary: string }
      static member Create(epd: EPDEntry, maxEval: float, maxEvalText: string, evalDiff: float, maxMove: string, maxEngine: string, summary: string) =
          { EPD = epd
            MaxEval = maxEval
            MaxEvalText = maxEvalText
            EvalDiff = evalDiff
            MaxMove = maxMove
            MaxEngine = maxEngine
            Summary = summary }

  type TablebaseEPDEntry =
    { EPD: EPDEntry
      TBAnswer: int
      QAnswer: float
      Move: string }
    with
      static member create epd tbA Qa move =
        { EPD = epd; TBAnswer = tbA; QAnswer = Qa; Move = move }

