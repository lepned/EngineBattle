module ResourceMonitorTests

open System
open Xunit
open ChessLibrary.TournamentTypes
open ChessLibrary.Game.ResourceMonitor

let private mb (n: int64) = n * 1048576L
let private sample pid (atS: float) (cpuS: float) ram = { Pid = pid; At = TimeSpan.FromSeconds atS; Cpu = TimeSpan.FromSeconds cpuS; Ram = ram; Peak = ram }
let private res player cpu ram toMove = { Player = player; CpuPercent = cpu; RamBytes = ram; PeakRamBytes = ram; ToMove = toMove }

[<Fact>]
let ``CPU is per core: four busy cores over two seconds is 400 percent`` () =
  Assert.Equal(400.0, cpuPercent (sample 1 10.0 5.0 0L) (sample 1 12.0 13.0 0L), 6)

[<Fact>]
let ``a restarted process (another pid) or no time between readings gives 0`` () =
  Assert.Equal(0.0, cpuPercent (sample 1 10.0 5.0 0L) (sample 2 12.0 13.0 0L))
  Assert.Equal(0.0, cpuPercent (sample 1 10.0 5.0 0L) (sample 1 10.0 6.0 0L))

[<Fact>]
let ``growth is suspicious only over five games, 30 percent and 200 MB`` () =
  Assert.True(growthSuspicious 5 (mb 1000L) (mb 1400L))
  Assert.False(growthSuspicious 4 (mb 1000L) (mb 1400L))     // too few games
  Assert.False(growthSuspicious 9 (mb 1000L) (mb 1250L))     // under 30 %
  Assert.False(growthSuspicious 9 (mb 300L) (mb 450L))       // 50 % but only 150 MB

[<Fact>]
let ``the closing table averages CPU over the engine's own turns and warns on growth`` () =
  let t = Totals()
  // SF: four cores on its turns, idle on the opponent's: the mean is 4.0, not 2.0
  for g in 0 .. 5 do
    t.Add(res "SF" 400.0 (mb 300L) true)
    t.Add(res "SF" 0.0 (mb 300L) false)
    t.Add(res "Leaky" 100.0 (mb (1000L + int64 g * 100L)) true)
    t.GameEnded [ "SF"; "Leaky" ]
  let report = (t.Report()).Value
  let sfLine = report.Split('\n') |> Array.find (fun l -> l.TrimStart().StartsWith "SF")
  Assert.Contains("4.0", sfLine)
  Assert.DoesNotContain("2.0", sfLine)
  Assert.Contains("300 MB -> 300 MB (6 games)", sfLine)
  Assert.Contains("Leaky: memory grew from 1000 MB to 1.5 GB over 6 games", report)
  Assert.DoesNotContain("SF: memory grew", report)

[<Fact>]
let ``cores are the percentage over 100, one decimal`` () =
  Assert.Equal("1.0", formatCores 98.0)
  Assert.Equal("29.5", formatCores 2950.0)

[<Fact>]
let ``nothing read gives no table`` () =
  Assert.True((Totals()).Report().IsNone)

[<Fact>]
let ``resources stay off the live-feed wire and the recording`` () =
  let r = Resources (res "A" 100.0 (mb 1L) true, res "B" 0.0 (mb 1L) false)
  Assert.False(ChessLibrary.LiveFeedWire.onWire r)
  Assert.True(ChessLibrary.LiveFeedWire.onWire (GameStarted "A"))
  let path = IO.Path.Combine(IO.Path.GetTempPath(), $"eb_res_{Guid.NewGuid():N}.jsonl")
  let recorder = ChessLibrary.LiveFeedRecorder(path)
  recorder.Record r
  recorder.Record (GameStarted "A")
  recorder.Dispose()
  let lines = IO.File.ReadAllLines path
  IO.File.Delete path
  Assert.Equal(1, lines.Length)
  Assert.DoesNotContain("Resources", lines.[0])
