namespace ChessLibrary

open System

module TimeControlTypes =

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
        // moves-to-go checked FIRST so a repeating control isn't shadowed by the increment cases
        match (config.Increment, config.Increment, x.WmovesToGo, x.BmovesToGo) with
        | (_, _, w, b) when w > 0 || b > 0 ->
            UnionType.WithMoves (config.Fixed, config.Increment, x.WmovesToGo, x.BmovesToGo)
        | (w, b, _, _) when w.Ticks = 0 && b.Ticks = 0 ->
            UnionType.FixedTime(config.Fixed)
        | (w, b, _, _) when w.Ticks > 0 || b.Ticks > 0 ->
            UnionType.WithIncrement(config.Fixed, config.Increment)
        | _ -> UnionType.Nodes(config.Nodes)
      member x.GetUnion(idx: int) =
        let config = x.GetTimeConfig idx
        match (config.Increment, config.Increment, x.WmovesToGo, x.BmovesToGo) with
        | (_, _, w, b) when w > 0 || b > 0 ->
            UnionType.WithMoves (config.Fixed, config.Increment, x.WmovesToGo, x.BmovesToGo)
        | (w, b, _, _) when w.Ticks = 0 && b.Ticks = 0 ->
            UnionType.FixedTime(config.Fixed)
        | (w, b, _, _) when w.Ticks > 0 || b.Ticks > 0 ->
            UnionType.WithIncrement(config.Fixed, config.Increment)
        | _ -> UnionType.Nodes(config.Nodes)
      member x.GetIncrementTime(idx: int) =
        (x.GetTimeConfig idx).Increment
      member x.TimeInfo(idx: int) =
        let config = x.GetTimeConfig idx
        sprintf "%fs + %fs" (config.Fixed.ToTimeSpan().TotalSeconds) (config.Increment.ToTimeSpan().TotalSeconds)
      member x.GetFullTimeInMS(idx: int) =
        let config = x.GetTimeConfig idx
        let fixedMs = int (TimeSpan(config.Fixed.Ticks).TotalMilliseconds)
        let incrMs = int (TimeSpan(config.Increment.Ticks).TotalMilliseconds)
        (fixedMs + incrMs)
      /// Moves per (repeating) time-control period; 0 = not a moves-to-go control.
      /// Symmetric for now: both sides use max(W,B).
      member x.MovesToGoPeriod = max x.WmovesToGo x.BmovesToGo
      /// Like GetTime, but for a repeating moves-to-go control it bakes the COUNTDOWN
      /// (moves left in the current period) into the WithMoves union, given how many
      /// moves the side to move has already completed (e.g. board.NextMoveNumber() - 1).
      member x.GetTimeForMove (config: TimeConfig) (movesDoneBySideToMove: int) =
        let period = x.MovesToGoPeriod
        if not config.NodeLimit && period > 0 && config.Fixed.Ticks > 0L then
          let rem = period - (movesDoneBySideToMove % period)
          let mtg = if rem <= 0 then period else rem
          UnionType.WithMoves (config.Fixed, config.Increment, mtg, mtg)
        else
          x.GetTime config

  module TimeControlCommands =
    let createTimeControlWithIncrementWithPonder (wtime: TimeOnly) (btime: TimeOnly) (winc: TimeOnly) (binc: TimeOnly) : string =
      let white = int (TimeSpan(wtime.Ticks).TotalMilliseconds)
      let black = int (TimeSpan(btime.Ticks).TotalMilliseconds)
      let wInc = int (TimeSpan(winc.Ticks).TotalMilliseconds)
      let bInc = int (TimeSpan(binc.Ticks).TotalMilliseconds)
      sprintf "go ponder wtime %d btime %d winc %d binc %d" white black wInc bInc

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
      // UCI movestogo is a SINGLE value for the side to move; wmoves==bmoves here
      // (symmetric repeating period), so emit one (bmoves kept for signature compatibility).
      ignore bmoves
      sprintf "go wtime %d btime %d winc %d binc %d movestogo %d" white black wincMs bincMs wmoves

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

    type SearchLimit =
      | NodeLimit of nodes: int
      | TimeLimit of ms: int
      with
        member x.Label =
            match x with
            | NodeLimit n ->
                if n >= 1000000 then sprintf "%.1fM nodes" (float n / 1000000.0)
                elif n >= 1000 then sprintf "%.1fK nodes" (float n / 1000.0)
                else sprintf "%d nodes" n
            | TimeLimit ms ->
                if ms >= 60000 then sprintf "%.1fmin" (float ms / 60000.0)
                elif ms >= 1000 then sprintf "%.1fs" (float ms / 1000.0)
                else sprintf "%dms" ms
        member x.Value =
            match x with
            | NodeLimit n -> n
            | TimeLimit ms -> ms

    let uciTimePart (time: UnionType) wTime bTime =
      match time with
      | FixedTime _ -> createTimeControl wTime bTime
      | WithIncrement (_, incr) -> createTimeControlWithIncrement wTime bTime incr incr
      | WithMoves (_, incr, wMoves, bMoves) -> createTimeControlWithMovesToGo wTime bTime incr incr wMoves bMoves
      | Nodes nodes -> createNodes nodes
