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
        EvalDiff: float
        MaxMove: string
        MaxEngine: string
        Summary: string }
      static member Create(epd: EPDEntry, maxEval: float, evalDiff: float, maxMove: string, maxEngine: string, summary: string) =
          { EPD = epd
            MaxEval = maxEval
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

  type TBscore =
    { Name: string
      FailedPuzzles: ResizeArray<TablebaseEPDEntry>
      CorrectPuzzles: ResizeArray<TablebaseEPDEntry>
      TotalNumber: int
      Correct: int
      Wrong: int
      Rating: float }
