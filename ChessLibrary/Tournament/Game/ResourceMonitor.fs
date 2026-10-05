/// Each engine's CPU and memory during a game, read beside it every couple of seconds (never in the
/// game loop), and the totals per engine over a tournament for the console's closing table.
module ChessLibrary.Game.ResourceMonitor

open System
open System.Collections.Generic
open System.Diagnostics
open System.Text
open System.Threading
open System.Threading.Tasks
open ChessLibrary.Engine
open ChessLibrary.TournamentTypes

/// One reading of a process.
type Sample = { Pid: int; At: TimeSpan; Cpu: TimeSpan; Ram: int64; Peak: int64 }

/// CPU between two readings of the same process: 100 = one core busy; 0 across a restart.
let cpuPercent (earlier: Sample) (later: Sample) =
  let wall = (later.At - earlier.At).TotalSeconds
  if later.Pid <> earlier.Pid || wall <= 0.0 then 0.0
  else max 0.0 (100.0 * (later.Cpu - earlier.Cpu).TotalSeconds / wall)

/// How often both engines are read: short, so that in fast games most readings fall inside one turn.
let interval = TimeSpan.FromMilliseconds 500.0

let private read (engine: ChessEngine) (clock: Stopwatch) =
  try
    let p = engine.Process
    if isNull p || p.HasExited then None
    else
      p.Refresh()
      Some { Pid = p.Id; At = clock.Elapsed; Cpu = p.TotalProcessorTime; Ram = p.WorkingSet64; Peak = p.PeakWorkingSet64 }
  with _ -> None

/// One round of readings: both engines' resources since `last` sent, the new readings and the side to
/// move returned. An interval counts as an engine's turn only when that side was to move at both of
/// its ends: one that spans a move would mix the engine's thinking with its opponent's.
let private measure (white: ChessEngine) (black: ChessEngine) (whiteToMove: unit -> bool)
                    (send: EngineResources -> EngineResources -> unit) (clock: Stopwatch) (last: Sample option[]) (lastWtm: bool) =
  let now = [| read white clock; read black clock |]
  let wtm = try whiteToMove () with _ -> true
  let sameTurn = wtm = lastWtm
  let of' i (engine: ChessEngine) toMove =
    match last.[i], now.[i] with
    | Some a, Some b -> Some { Player = engine.Name; CpuPercent = cpuPercent a b; RamBytes = b.Ram; PeakRamBytes = b.Peak; ToMove = toMove }
    | _ -> None
  match of' 0 white (sameTurn && wtm), of' 1 black (sameTurn && not wtm) with
  | Some w, Some b -> (try send w b with _ -> ())
  | _ -> ()
  now, wtm

/// Reads both engines until `ct` is cancelled and sends each pair; `whiteToMove` says whose turn it is.
let start (white: ChessEngine) (black: ChessEngine) (whiteToMove: unit -> bool)
          (send: EngineResources -> EngineResources -> unit) (ct: CancellationToken) : Task =
  Task.Run((fun () ->
    task {
      let clock = Stopwatch.StartNew()
      let mutable last = [| read white clock; read black clock |]
      let mutable lastWtm = try whiteToMove () with _ -> true
      while not ct.IsCancellationRequested do
        try do! Task.Delay(interval, ct)
        with :? OperationCanceledException -> ()
        if not ct.IsCancellationRequested then
          let now, wtm = measure white black whiteToMove send clock last lastWtm
          last <- now
          lastWtm <- wtm
    } :> Task), ct)

/// Cores busy from a CPU percentage: "1.0", "29.5".
let formatCores (cpuPercent: float) = sprintf "%.1f" (cpuPercent / 100.0)

/// "312 MB", "2.1 GB".
let formatBytes (bytes: int64) =
  let mb = float bytes / 1048576.0
  if mb >= 1024.0 then sprintf "%.1f GB" (mb / 1024.0) else sprintf "%.0f MB" mb

/// Memory that kept growing from game to game: at least five games, and the last game's at least
/// 30% and 200 MB above the first's.
let growthSuspicious (games: int) (first: int64) (last: int64) =
  games >= 5 && float last >= float first * 1.3 && last - first >= 200L * 1048576L

type private Tally =
  { mutable CpuSum: float
    mutable CpuCount: int
    mutable Peak: int64
    mutable Last: int64
    GameRam: ResizeArray<int64> }

/// One tournament's readings per engine, for the closing table.
type Totals() =
  let tallies = Dictionary<string, Tally>()
  let sync = obj ()

  member _.Add(r: EngineResources) =
    lock sync (fun () ->
      let t =
        match tallies.TryGetValue r.Player with
        | true, t -> t
        | _ ->
            let t = { CpuSum = 0.0; CpuCount = 0; Peak = 0L; Last = 0L; GameRam = ResizeArray() }
            tallies.[r.Player] <- t
            t
      // the CPU an engine uses on its own turn is the figure worth a mean
      if r.ToMove then
        t.CpuSum <- t.CpuSum + r.CpuPercent
        t.CpuCount <- t.CpuCount + 1
      t.Peak <- max t.Peak r.PeakRamBytes
      t.Last <- r.RamBytes)

  /// A game between these engines ended: their memory now, for the growth check (games run in
  /// parallel, so only the two that played it).
  member _.GameEnded(players: string list) =
    lock sync (fun () ->
      for p in players do
        match tallies.TryGetValue p with
        | true, t when t.Last > 0L -> t.GameRam.Add t.Last
        | _ -> ())

  member _.Clear() = lock sync (fun () -> tallies.Clear())

  /// The closing table, or None when nothing was read.
  member _.Report() : string option =
    lock sync (fun () ->
      if tallies.Count = 0 then None
      else
        let sb = StringBuilder()
        let width = max 6 (tallies.Keys |> Seq.map String.length |> Seq.max)
        sb.AppendLine("\nEngine resources") |> ignore
        sb.AppendLine(sprintf "  %-*s  %15s  %9s  %s" width "Engine" "Cores (to move)" "Peak RAM" "RAM first -> last game") |> ignore
        let warnings = ResizeArray<string>()
        for KeyValue(name, t) in tallies |> Seq.sortBy (fun kv -> kv.Key) do
          let cpu = if t.CpuCount > 0 then formatCores (t.CpuSum / float t.CpuCount) else "-"
          let games =
            if t.GameRam.Count > 0 then sprintf "%s -> %s (%d games)" (formatBytes t.GameRam.[0]) (formatBytes t.GameRam.[t.GameRam.Count - 1]) t.GameRam.Count
            else "-"
          sb.AppendLine(sprintf "  %-*s  %15s  %9s  %s" width name cpu (formatBytes t.Peak) games) |> ignore
          if t.GameRam.Count > 0 && growthSuspicious t.GameRam.Count t.GameRam.[0] t.GameRam.[t.GameRam.Count - 1] then
            warnings.Add(sprintf "  %s: memory grew from %s to %s over %d games - a leak?" name (formatBytes t.GameRam.[0]) (formatBytes t.GameRam.[t.GameRam.Count - 1]) t.GameRam.Count)
        for w in warnings do sb.AppendLine w |> ignore
        Some (sb.ToString()))
