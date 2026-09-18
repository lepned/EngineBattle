module ReplayGateTests

open Xunit
open ChessLibrary.PGNTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.ReplayGate

// ---------------------------------------------------------------------------
// The gate that orders games under parallel play when deviation prevention is
// on: a game repeating an earlier game's opening AND colours must not start
// until that game has finished. Pure, no engines, no workers.
// ---------------------------------------------------------------------------

let private eng name = { EngineConfig.Empty with Name = name }

let private pairing (opening: string) (white: string) (black: string) : Pairing =
    { Opening = PgnGame.Empty 1
      White = eng white
      Black = eng black
      GameNr = 0
      RoundNr = ""
      OpeningHash = opening }

let private start (t: Take) =
    match t with
    | Start p -> p
    | Wait _ -> failwith "expected Start, got Wait"
    | Done -> failwith "expected Start, got Done"

let private isWait (t: Take) = match t with Wait _ -> true | _ -> false

[<Fact>]
let ``without prevention pairings come out in plan order and never wait`` () =
    let plan = [ pairing "o1" "A" "B"; pairing "o1" "A" "B"; pairing "o1" "A" "B" ]
    let gate = ReplayGate(plan, false, null)
    let first = start (gate.TryTake())
    let second = start (gate.TryTake())   // same key as the first, still in flight - no gate
    let third = start (gate.TryTake())
    Assert.Equal(0, gate.InFlightCount)
    Assert.Equal<Pairing list>([ first; second; third ], plan)
    Assert.Equal(Done, gate.TryTake())

[<Fact>]
let ``a repeat of the same opening and colours waits until the first game is released`` () =
    let first = pairing "o1" "A" "B"
    let repeat = pairing "o1" "A" "B"
    let gate = ReplayGate([ first; repeat ], true, null)
    Assert.Same(first, start (gate.TryTake()))
    Assert.True(isWait (gate.TryTake()))
    gate.Release first
    Assert.Same(repeat, start (gate.TryTake()))

[<Fact>]
let ``the colour-swapped twin does not wait`` () =
    let gate = ReplayGate([ pairing "o1" "A" "B"; pairing "o1" "B" "A" ], true, null)
    start (gate.TryTake()) |> ignore
    start (gate.TryTake()) |> ignore   // A as Black and B as White are different keys
    Assert.Equal(4, gate.InFlightCount)

[<Fact>]
let ``a different opening does not wait`` () =
    let gate = ReplayGate([ pairing "o1" "A" "B"; pairing "o2" "A" "B" ], true, null)
    start (gate.TryTake()) |> ignore
    start (gate.TryTake()) |> ignore

[<Fact>]
let ``one shared side is enough to wait`` () =
    // Same White in the same opening against a different opponent: A must replay its line.
    let first = pairing "o1" "A" "B"
    let gate = ReplayGate([ first; pairing "o1" "A" "C" ], true, null)
    start (gate.TryTake()) |> ignore
    Assert.True(isWait (gate.TryTake()))
    gate.Release first
    start (gate.TryTake()) |> ignore

[<Fact>]
let ``a waiting pairing is passed over for a later one that can start`` () =
    let first = pairing "o1" "A" "B"
    let repeat = pairing "o1" "A" "B"
    let other = pairing "o2" "C" "D"
    let gate = ReplayGate([ first; repeat; other ], true, null)
    Assert.Same(first, start (gate.TryTake()))
    Assert.Same(other, start (gate.TryTake()))   // plan order where possible, but never idle
    Assert.True(isWait (gate.TryTake()))
    gate.Release first
    Assert.Same(repeat, start (gate.TryTake()))

[<Fact>]
let ``the wait task completes on release and a new wait is armed after it`` () =
    let first = pairing "o1" "A" "B"
    let gate = ReplayGate([ first; pairing "o1" "A" "B"; pairing "o1" "A" "B" ], true, null)
    start (gate.TryTake()) |> ignore
    let waited = match gate.TryTake() with Wait t -> t | _ -> failwith "expected Wait"
    Assert.False(waited.IsCompleted)
    gate.Release first
    Assert.True(waited.IsCompleted)
    let second = start (gate.TryTake())
    let waitedAgain = match gate.TryTake() with Wait t -> t | _ -> failwith "expected Wait"
    Assert.False(waitedAgain.IsCompleted)
    gate.Release second
    Assert.True(waitedAgain.IsCompleted)

[<Fact>]
let ``PreventMoveDeviationFor narrows the keys to the listed engines`` () =
    // Only B is held; A repeating as White is not a conflict, B repeating as Black is.
    let gate = ReplayGate([ pairing "o1" "A" "B"; pairing "o1" "A" "C"; pairing "o1" "D" "B" ], true, [| "B" |])
    start (gate.TryTake()) |> ignore   // A-B: holds (o1, b, B) only
    start (gate.TryTake()) |> ignore   // A-C: A is not held, C is not listed
    Assert.True(isWait (gate.TryTake()))   // D-B: B as Black is in flight

[<Fact>]
let ``PeekNext shows the next pairing in plan order without taking it`` () =
    let a, b = pairing "o1" "A" "B", pairing "o2" "C" "D"
    let gate = ReplayGate([ a; b ], true, null)
    Assert.Same(a, (gate.PeekNext()).Value)
    start (gate.TryTake()) |> ignore
    Assert.Same(b, (gate.PeekNext()).Value)
    start (gate.TryTake()) |> ignore
    Assert.True((gate.PeekNext()).IsNone)

[<Fact>]
let ``PeekNext n lists the next n pending pairings, fewer when fewer remain`` () =
    let a, b, c = pairing "o1" "A" "B", pairing "o2" "C" "D", pairing "o3" "E" "F"
    let gate = ReplayGate([ a; b; c ], true, null)
    Assert.Equal<Pairing list>([ a; b ], gate.PeekNext 2)
    start (gate.TryTake()) |> ignore
    Assert.Equal<Pairing list>([ b; c ], gate.PeekNext 5)
    Assert.Empty(gate.PeekNext 0)

[<Fact>]
let ``Done once everything is taken even while games are still in flight`` () =
    let gate = ReplayGate([ pairing "o1" "A" "B" ], true, null)
    start (gate.TryTake()) |> ignore
    Assert.Equal(Done, gate.TryTake())
    Assert.Equal(0, gate.PendingCount)
    Assert.Equal(2, gate.InFlightCount)
