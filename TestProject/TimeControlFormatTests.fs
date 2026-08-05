module TimeControlFormatTests

open Xunit
open System
open System.Globalization
open System.Threading
open ChessLibrary.TimeControlTypes

// The notation, written down nowhere else:  '  minutes   ''  seconds
// So "1:30' + 2''" is a ninety second base with a two second increment.
// These tests are the spec — change the notation here first.

let private fmt (fixedSeconds: float) (incrementSeconds: float) =
    formatTimeControl (TimeSpan.FromSeconds fixedSeconds) (TimeSpan.FromSeconds incrementSeconds)

// ── Bases under a minute ──
// 30s + 0.5s must not become "0.5' + 0.5''".

[<Theory>]
[<InlineData(30.0, 0.5, "30'' + 0.5''")>]
[<InlineData(10.0, 0.1, "10'' + 0.1''")>]
[<InlineData(45.0, 0.0, "45'' + 0''")>]
[<InlineData(59.0, 0.6, "59'' + 0.6''")>]
let ``sub-minute bases are quoted in seconds`` (fixedS: float, incS: float, expected: string) =
    Assert.Equal(expected, fmt fixedS incS)

// A sub-second base must say more than "0''".
[<Fact>]
let ``fractional sub-second base keeps its digits`` () =
    Assert.Equal("0.4'' + 0.1''", fmt 0.4 0.1)

// ── Whole minutes ──

[<Theory>]
[<InlineData(60.0, 0.6, "1' + 0.6''")>]
[<InlineData(180.0, 2.0, "3' + 2''")>]
[<InlineData(300.0, 3.0, "5' + 3''")>]
let ``whole minutes drop the seconds field`` (fixedS: float, incS: float, expected: string) =
    Assert.Equal(expected, fmt fixedS incS)

// ── Minutes with a seconds remainder ──
// The old formatter printed %.0f of the total minutes, which lost the remainder: 90s and
// 2m30s both came out "2'", and half-to-even sent 2m30s down to "2'" while 3m30s went up
// to "4'" — the same remainder rounded in opposite directions.

[<Theory>]
[<InlineData(90.0, 1.0, "1:30' + 1''")>]
[<InlineData(150.0, 1.0, "2:30' + 1''")>]
[<InlineData(210.0, 1.0, "3:30' + 1''")>]
[<InlineData(65.0, 1.0, "1:05' + 1''")>]      // zero-padded, so it cannot read as minutes
[<InlineData(125.0, 1.0, "2:05' + 1''")>]
[<InlineData(3661.0, 1.0, "61:01' + 1''")>]
let ``a seconds remainder is spelled out as m:ss`` (fixedS: float, incS: float, expected: string) =
    Assert.Equal(expected, fmt fixedS incS)

// ── Hour-scale bases ──
// No hours field: chess quotes classical as "90' + 30''", so minutes run past sixty.

[<Theory>]
[<InlineData(3600.0, 0.0, "60' + 0''")>]        // one hour
[<InlineData(5400.0, 30.0, "90' + 30''")>]      // the classical 90+30
[<InlineData(5415.0, 30.0, "90:15' + 30''")>]   // 1h 30m 15s
let ``hour-scale bases keep counting in minutes`` (fixedS: float, incS: float, expected: string) =
    Assert.Equal(expected, fmt fixedS incS)

// The old formatter summed a span's Hours/Minutes/Seconds by hand and never read Days, and
// held the total in an int32, which saturates silently — "999999:0:0" is a legal config and
// used to render "35791394:07'" for the base and "2147483647''" for the increment.
[<Fact>]
let ``long spans are read whole, not clamped to int range`` () =
    let huge = TimeSpan.FromHours 999999.0            // 59,999,940 minutes
    Assert.Equal("1500' + 0''", formatTimeControl (TimeSpan.FromHours 25.0) TimeSpan.Zero)
    Assert.Equal("59999940' + 0''", formatTimeControl huge TimeSpan.Zero)
    Assert.Equal("5' + 3599996400''", formatTimeControl (TimeSpan.FromMinutes 5.0) huge)

// The branch and the digits must round the same value: 59.9997s once printed "60''".
[<Fact>]
let ``the sixty second boundary rounds consistently`` () =
    Assert.Equal("1' + 0''", formatTimeControl (TimeSpan.FromSeconds 59.9997) TimeSpan.Zero)
    Assert.Equal("59.9'' + 0''", formatTimeControl (TimeSpan.FromSeconds 59.9) TimeSpan.Zero)

// ── Increments ──

[<Theory>]
[<InlineData(0.0, "0''")>]      // no increment
[<InlineData(2.0, "2''")>]      // whole seconds carry no decimal point
[<InlineData(0.5, "0.5''")>]
[<InlineData(0.1, "0.1''")>]
[<InlineData(1.25, "1.25''")>]  // finer than a tenth is kept, not rounded to "1.2''"
[<InlineData(60.0, "60''")>]    // stays in seconds rather than becoming "1'"
let ``increments are seconds, trimmed of trailing zeros`` (incS: float, expected: string) =
    Assert.Equal(sprintf "5' + %s" expected, fmt 300.0 incS)

// Stays in seconds whichever unit it arrived in.
[<Fact>]
let ``an increment given in minutes still prints seconds`` () =
    Assert.Equal("5' + 120''", formatTimeControl (TimeSpan.FromMinutes 5.0) (TimeSpan.FromMinutes 2.0))

// ── The entry points agree ──
// Two copies of this formatter used to disagree on the minutes format.

[<Fact>]
let ``TimeConfig ToString matches the shared formatter`` () =
    let tc = { Id = 1; Fixed = TimeSpan.FromSeconds 90.0; Increment = TimeSpan.FromSeconds 1.0
               NodeLimit = false; Nodes = 0 }
    Assert.Equal("1:30' + 1''", tc.ToString())

// ── Node limits ──
// Had three renderings in adjacent members — "10.0K nodes", "10.0K n" and "10000N" — plus
// "Node limit=10000" from ToString(), so one tournament read differently per code path.

[<Fact>]
let ``node limited configs report nodes, not a clock`` () =
    let tc = { Id = 1; Fixed = TimeSpan.Zero; Increment = TimeSpan.Zero
               NodeLimit = true; Nodes = 10000 }
    Assert.Equal("10.0K nodes", tc.ToString())

[<Theory>]
[<InlineData(1, "1 node")>]     // singular: 1-node searches are routine in puzzle testing
[<InlineData(2, "2 nodes")>]
[<InlineData(800, "800 nodes")>]
[<InlineData(999, "999 nodes")>]
[<InlineData(1000, "1.0K nodes")>]
[<InlineData(10000, "10.0K nodes")>]
[<InlineData(100000, "100.0K nodes")>]
[<InlineData(1000000, "1.0M nodes")>]
[<InlineData(1500000, "1.5M nodes")>]
let ``node limits switch to K and M`` (nodes: int, expected: string) =
    Assert.Equal(expected, formatNodes nodes)

// Same seam as the sixty second boundary: the unit and the digits must agree on the value,
// or 999999 rounds up within the K branch and prints "1000.0K nodes".
[<Theory>]
[<InlineData(999949, "999.9K nodes")>]
[<InlineData(999950, "1.0M nodes")>]
[<InlineData(999999, "1.0M nodes")>]
let ``the K to M boundary rounds consistently`` (nodes: int, expected: string) =
    Assert.Equal(expected, formatNodes nodes)

// The rounded thousands pick the unit but must not also produce the M digits: rounding
// twice, nodes -> K -> M, walks the value up a tenth. These are the values it moves.
[<Theory>]
[<InlineData(1050000, "1.1M nodes")>]
[<InlineData(1349960, "1.3M nodes")>]
[<InlineData(1949960, "1.9M nodes")>]
[<InlineData(12349960, "12.3M nodes")>]
let ``millions digits come off the node count, not the rounded thousands`` (nodes: int, expected: string) =
    Assert.Equal(expected, formatNodes nodes)

// ── Culture ──
// Half of Europe writes decimals with a comma, so "0.5''" and "1.5M" are one careless
// ToString() away from "0,5''" and "1,5M". Everything here runs on the developer's own
// machine, where that would be the normal locale rather than an exotic one.

[<Theory>]
[<InlineData("nb-NO")>]
[<InlineData("de-DE")>]
[<InlineData("fr-FR")>]
let ``decimal separators stay invariant under any culture`` (culture: string) =
    let original = Thread.CurrentThread.CurrentCulture
    try
        Thread.CurrentThread.CurrentCulture <- CultureInfo culture
        Assert.Equal("1:30' + 0.5''", formatTimeControl (TimeSpan.FromSeconds 90.0) (TimeSpan.FromSeconds 0.5))
        Assert.Equal("59.9'' + 0.05''", formatTimeControl (TimeSpan.FromSeconds 59.9) (TimeSpan.FromSeconds 0.05))
        Assert.Equal("1.5M nodes", formatNodes 1_500_000)
        Assert.Equal("10.0K nodes", formatNodes 10_000)
    finally
        Thread.CurrentThread.CurrentCulture <- original
