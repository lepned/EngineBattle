module GameClockTests

open System
open Xunit
open ChessLibrary.TimeControlTypes
open ChessLibrary.Game.Clock

let private ms (n: float) = TimeSpan.FromMilliseconds n
let private sec (n: float) = TimeSpan.FromSeconds n

let private config fixedTime increment =
  { Id = 1; Fixed = fixedTime; Increment = increment; NodeLimit = false; Nodes = 0; MoveTime = TimeSpan.Zero; MovesToGo = 0 }

let private control (c: TimeConfig) = { TimeConfigs = [ c ]; WmovesToGo = 0; BmovesToGo = 0 }

let private clockOf (c: TimeConfig) overhead = create (control c) c overhead

[<Fact>]
let ``a move costs its time and earns the increment`` () =
  let clock = clockOf (config (sec 60.0) (sec 1.0)) TimeSpan.Zero
  match afterMove clock (sec 10.0) 1 with
  | OnTime c -> Assert.Equal(sec 51.0, c.Left)
  | LostOnTime _ -> failwith "on time"

[<Fact>]
let ``an overrun within the margin is on time and leaves zero`` () =
  let clock = { clockOf (config (sec 1.0) TimeSpan.Zero) (ms 100.0) with Left = sec 1.0 }
  match afterMove clock (ms 1050.0) 1 with
  | OnTime c -> Assert.Equal(TimeSpan.Zero, c.Left)
  | LostOnTime _ -> failwith "within the margin"

[<Fact>]
let ``past the margin is a loss, with the overrun counted from zero`` () =
  let clock = clockOf (config (sec 1.0) TimeSpan.Zero) (ms 100.0)
  Assert.Equal(LostOnTime (ms -200.0), afterMove clock (ms 1200.0) 1)

[<Fact>]
let ``completing a repeating period adds the base time`` () =
  let c = { config (sec 60.0) TimeSpan.Zero with MovesToGo = 40 }
  let clock = clockOf c TimeSpan.Zero
  match afterMove clock (sec 1.0) 40, afterMove clock (sec 1.0) 39 with
  | OnTime atPeriod, OnTime before ->
      Assert.Equal(sec 119.0, atPeriod.Left)
      Assert.Equal(sec 59.0, before.Left)
  | _ -> failwith "on time"

[<Fact>]
let ``a time per move shows move time plus margin, at least 50 ms, and is never charged`` () =
  let c = { config TimeSpan.Zero TimeSpan.Zero with MoveTime = ms 100.0 }
  let clock = clockOf c (ms 10.0)
  Assert.Equal(ms 50.0, clock.Margin)
  Assert.Equal(ms 150.0, clock.Left)
  // whole milliseconds: 100.9 ms is on time
  Assert.Equal(OnTime clock, afterMove clock (ms 100.9) 1)
  Assert.Equal(OnTime clock, afterMove clock (ms 150.0) 1)
  Assert.Equal(LostOnTime (ms -51.0), afterMove clock (ms 151.0) 1)

[<Fact>]
let ``a node limit never loses on time and has no stop time`` () =
  let c = { config TimeSpan.Zero TimeSpan.Zero with NodeLimit = true; Nodes = 1000 }
  let clock = clockOf c TimeSpan.Zero
  Assert.Equal(OnTime clock, afterMove clock (sec 1000.0) 1)
  Assert.Equal(None, stopAfter clock)

[<Fact>]
let ``stop comes after the move's time, margin and grace`` () =
  let running = clockOf (config (sec 10.0) (sec 1.0)) (ms 100.0)
  Assert.Equal(Some (sec 11.0 + ms 100.0 + stopGrace), stopAfter running)
  let perMove = clockOf { config TimeSpan.Zero TimeSpan.Zero with MoveTime = ms 500.0 } TimeSpan.Zero
  Assert.Equal(Some (ms 550.0 + stopGrace), stopAfter perMove)
