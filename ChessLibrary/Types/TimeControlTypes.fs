namespace ChessLibrary

open System
open System.Globalization

module TimeControlTypes =

  /// Time control as the banner and console quote it: one tick is minutes, two is seconds,
  /// so "1:30' + 2''" is a ninety second base with a two second increment. Bases under a
  /// minute stay in seconds ("30'' + 0.5''"); increments stay in seconds at any size.
  /// TimeControlFormatTests is the spec.
  // int64 throughout: "999999:0:0" is a legal config and that many seconds overflows int32,
  // which saturates silently rather than throwing.
  let formatTimeControl (fixedTime: TimeSpan) (incrementTime: TimeSpan) : string =
    // Whole values drop the decimal point.
    let secondsText (totalSeconds: float) =
      let rounded = Math.Round(totalSeconds, 3)
      if rounded = Math.Floor rounded then string (int64 rounded)
      else rounded.ToString("0.###", CultureInfo.InvariantCulture)

    let fixedPart =
      // Round once, so the branch and the digits agree: 59.9997s must not print "60''".
      let total = Math.Round(fixedTime.TotalSeconds, 3)
      if total < 60.0 then sprintf "%s''" (secondsText total)
      else
        let wholeSeconds = int64 (Math.Round total)
        let minutes, seconds = wholeSeconds / 60L, wholeSeconds % 60L
        if seconds = 0L then sprintf "%d'" minutes
        else sprintf "%d:%02d'" minutes seconds

    sprintf "%s + %s''" fixedPart (secondsText incrementTime.TotalSeconds)

  /// The other half of a time control: "1 node", "800 nodes", "10.0K nodes", "1.5M nodes".
  let formatNodes (nodes: int) : string =
    if nodes = 1 then "1 node"   // 1-node searches are routine in puzzle testing
    elif nodes < 1000 then sprintf "%d nodes" nodes
    else
      // k picks the unit — rounded, or 999999 stays in the K branch and prints "1000.0K".
      // It must not also produce the M digits: rounding nodes->k->M is a second rounding,
      // and 1349960 comes out "1.4M" that way instead of "1.3M". Each unit off the source.
      let k = Math.Round(float nodes / 1000.0, 1)
      if k < 1000.0 then sprintf "%.1fK nodes" k
      else sprintf "%.1fM nodes" (float nodes / 1_000_000.0)

  type UnionType =
    | FixedTime of InMSFixed: TimeSpan
    | WithIncrement of InMSFixed: TimeSpan * IncrementMS: TimeSpan
    | WithMoves of InMSFixed: TimeSpan * IncrementMS: TimeSpan * Wmoves: int * Bmoves: int
    | Nodes of nodes: int
    with
      member x.GetFixedtime() =
        match x with
        | FixedTime time -> time
        | WithIncrement (t, _) -> t
        | WithMoves (t, _, _, _) -> t
        | Nodes _ -> TimeSpan.Zero
      member x.GetIncrementTime() =
        match x with
        | FixedTime _ -> TimeSpan.Zero
        | WithIncrement (_, incr) -> incr
        | WithMoves (_, incr, _, _) -> incr
        | Nodes _ -> TimeSpan.Zero

  type TimeConfig = { Id: int; Fixed: TimeSpan; Increment: TimeSpan; NodeLimit: bool; Nodes: int }
    with
      member x.Times (fraction: double) =
        let fixedTicks = float x.Fixed.Ticks
        let newFixedTicks = fixedTicks * fraction |> int64
        let incrTicks = float x.Increment.Ticks
        let newIncrTicks = incrTicks * fraction |> int64
        let newNodes = float x.Nodes * fraction |> int32
        { x with Fixed = TimeSpan(newFixedTicks); Increment = TimeSpan(newIncrTicks); Nodes = newNodes }
      override x.ToString() =
        if x.NodeLimit then formatNodes x.Nodes
        else formatTimeControl x.Fixed x.Increment

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
        sprintf "%fs + %fs" config.Fixed.TotalSeconds config.Increment.TotalSeconds
      member x.GetFullTimeInMS(idx: int) =
        let config = x.GetTimeConfig idx
        let fixedMs = int config.Fixed.TotalMilliseconds
        let incrMs = int config.Increment.TotalMilliseconds
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
    let createTimeControlWithIncrementWithPonder (wtime: TimeSpan) (btime: TimeSpan) (winc: TimeSpan) (binc: TimeSpan) : string =
      let white = int wtime.TotalMilliseconds
      let black = int btime.TotalMilliseconds
      let wInc = int winc.TotalMilliseconds
      let bInc = int binc.TotalMilliseconds
      sprintf "go ponder wtime %d btime %d winc %d binc %d" white black wInc bInc

    let createTimeControlWithIncrement (wtime: TimeSpan) (btime: TimeSpan) (winc: TimeSpan) (binc: TimeSpan) : string =
      let white = int wtime.TotalMilliseconds
      let black = int btime.TotalMilliseconds
      let wInc = int winc.TotalMilliseconds
      let bInc = int binc.TotalMilliseconds
      sprintf "go wtime %d btime %d winc %d binc %d" white black wInc bInc

    let createTimeControl (wtime: TimeSpan) (btime: TimeSpan) : string =
      let white = int wtime.TotalMilliseconds
      let black = int btime.TotalMilliseconds
      sprintf "go wtime %d btime %d" white black

    let createTimeControlWithMovesToGo (wtime: TimeSpan) (btime: TimeSpan) (winc: TimeSpan) (binc: TimeSpan) (wmoves: int) (bmoves: int) : string =
      let white = int wtime.TotalMilliseconds
      let black = int btime.TotalMilliseconds
      let wincMs = int winc.TotalMilliseconds
      let bincMs = int binc.TotalMilliseconds
      // UCI movestogo is a SINGLE value for the side to move; wmoves==bmoves here
      // (symmetric repeating period), so emit one (bmoves kept for signature compatibility).
      ignore bmoves
      sprintf "go wtime %d btime %d winc %d binc %d movestogo %d" white black wincMs bincMs wmoves

    let createNodes nodes =
      sprintf "go nodes %d" nodes

    let getFixedTime (time: TimeSpan) = FixedTime time
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
