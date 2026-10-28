/// The NVIDIA GPUs' temperature, load and power, read through NVML (the library nvidia-smi uses; it
/// ships with the driver, needs no admin rights). One agent for the process reads every ten
/// seconds while anyone listens; without an NVIDIA driver there is nothing to read and no reading.
module ChessLibrary.GpuMonitor

open System
open System.Runtime.InteropServices
open System.Text

/// One GPU's state: temperature in °C, how far below the card's own limit it is (nvidia-smi's
/// "T.Limit", the card slows down a couple of degrees past it), load in percent, power in watts;
/// None where the card or driver does not say.
type GpuReading = { Index: int; Name: string; TempC: int; MarginC: int option; LoadPercent: int option; PowerW: float option }

/// How hot a GPU reads: Warm within 5 °C of the card's limit, Hot at it; a card that does not give
/// its limit is taken to have it at 90 °C.
type GpuHeat =
    | Normal
    | Warm
    | Hot

let heat (r: GpuReading) =
    let margin = r.MarginC |> Option.defaultValue (90 - r.TempC)
    if margin <= 0 then Hot elif margin <= 5 then Warm else Normal

/// "67 °C"; several GPUs "67 · 54 °C".
let tempText (readings: GpuReading list) =
    if readings.IsEmpty then "" else (readings |> List.map (fun r -> string r.TempC) |> String.concat " · ") + " °C"

/// The hottest GPU's heat, for the colour.
let hottest (readings: GpuReading list) =
    readings |> List.map heat |> List.fold max Normal

/// One line per GPU for a tooltip: "GPU 0 NVIDIA GeForce RTX 5090: 67 °C (23 °C below its limit), 98 %, 412 W".
let detailText (readings: GpuReading list) =
    readings
    |> List.map (fun r ->
        let margin = r.MarginC |> Option.map (fun m -> if m > 0 then $" ({m} °C below its limit)" else " (at its limit)") |> Option.defaultValue ""
        let load = r.LoadPercent |> Option.map (fun p -> ", " + string p + " %") |> Option.defaultValue ""
        let power = r.PowerW |> Option.map (fun w -> $", {w:F0} W") |> Option.defaultValue ""
        $"GPU {r.Index} {r.Name}: {r.TempC} °C{margin}{load}{power}")
    |> String.concat "\n"

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private NoArg = delegate of unit -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetCount = delegate of uint32[] -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetHandle = delegate of uint32 * nativeint[] -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetName = delegate of nativeint * byte[] * uint32 -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetTemp = delegate of nativeint * int * uint32[] -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetValues = delegate of nativeint * uint32[] -> int
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type private GetMargin = delegate of nativeint * int[] -> int

type private Nvml =
    { Count: GetCount
      Handle: GetHandle
      Name: GetName
      Temp: GetTemp
      Load: GetValues
      Power: GetValues
      /// nvmlDeviceGetMarginTemperature, in drivers since 2024; None in older ones.
      Margin: GetMargin option }

let private export<'d when 'd :> Delegate> (lib: nativeint) (name: string) =
    Marshal.GetDelegateForFunctionPointer<'d>(NativeLibrary.GetExport(lib, name))

/// NVML loaded and initialised, or None (no NVIDIA driver, or it failed to start).
let private load () : Nvml option =
    try
        let names = if OperatingSystem.IsWindows() then [ "nvml.dll" ] else [ "libnvidia-ml.so.1"; "libnvidia-ml.so" ]
        names
        |> List.tryPick (fun n -> match NativeLibrary.TryLoad n with | true, h -> Some h | _ -> None)
        |> Option.bind (fun lib ->
            if (export<NoArg> lib "nvmlInit_v2").Invoke() <> 0 then None
            else
                Some { Count = export lib "nvmlDeviceGetCount_v2"
                       Handle = export lib "nvmlDeviceGetHandleByIndex_v2"
                       Name = export lib "nvmlDeviceGetName"
                       Temp = export lib "nvmlDeviceGetTemperature"
                       Load = export lib "nvmlDeviceGetUtilizationRates"
                       Power = export lib "nvmlDeviceGetPowerUsage"
                       Margin =
                           match NativeLibrary.TryGetExport(lib, "nvmlDeviceGetMarginTemperature") with
                           | true, f -> Some(Marshal.GetDelegateForFunctionPointer<GetMargin> f)
                           | _ -> None })
    with _ -> None

let private readAll (nvml: Nvml) : GpuReading list =
    let count = [| 0u |]
    if nvml.Count.Invoke count <> 0 then []
    else
        [ for n in 0 .. int count.[0] - 1 do
            let i = uint32 n
            let handle = [| 0n |]
            let temp = [| 0u |]
            // temperature sensor 0 = the GPU die
            if nvml.Handle.Invoke(i, handle) = 0 && nvml.Temp.Invoke(handle.[0], 0, temp) = 0 then
                let name = Array.zeroCreate<byte> 96
                let text = if nvml.Name.Invoke(handle.[0], name, 96u) = 0 then Encoding.ASCII.GetString(name, 0, max 0 (Array.IndexOf(name, 0uy))) else ""
                // utilization is { gpu; memory }, power in milliwatts
                let load = [| 0u; 0u |]
                let power = [| 0u |]
                // nvmlMarginTemperature_v1: { version = size 8 | 1 << 24; marginTemperature }
                let margin = [| 0x01000008; 0 |]
                yield { Index = n
                        Name = text
                        TempC = int temp.[0]
                        MarginC = nvml.Margin |> Option.bind (fun f -> if f.Invoke(handle.[0], margin) = 0 then Some margin.[1] else None)
                        LoadPercent = (if nvml.Load.Invoke(handle.[0], load) = 0 then Some(int load.[0]) else None)
                        PowerW = (if nvml.Power.Invoke(handle.[0], power) = 0 then Some(float power.[0] / 1000.0) else None) } ]

let private nvml = lazy (load ())

/// True when there is an NVIDIA GPU to read.
let available () = match nvml.Force() with Some n -> not (readAll n).IsEmpty | None -> false

/// The GPUs now; empty without NVML.
let read () = match nvml.Force() with Some n -> (try readAll n with _ -> []) | None -> []

/// How often the agent reads while anyone listens.
let interval = TimeSpan.FromSeconds 10.0

type private Msg =
    | Subscribe of (GpuReading list -> unit) * AsyncReplyChannel<int>
    | Unsubscribe of int

/// A listener that throws does not stop the others.
let private safely (f: GpuReading list -> unit) readings = try f readings with _ -> ()

/// Reads every `interval` while it has listeners and hands each one the readings; a new listener gets
/// readings at once (fresh when nobody was listening: the last ones may be hours old).
let private agent =
    MailboxProcessor<Msg>.Start(fun inbox ->
        let rec loop (listeners: Map<int, GpuReading list -> unit>) (next: int) (last: GpuReading list) =
            async {
                let! msg =
                    if listeners.IsEmpty then
                        async {
                            let! m = inbox.Receive()
                            return Some m
                        }
                    else inbox.TryReceive(int interval.TotalMilliseconds)
                match msg with
                | Some(Subscribe(f, reply)) ->
                    let last = if listeners.IsEmpty || last.IsEmpty then read () else last
                    reply.Reply next
                    safely f last
                    return! loop (listeners.Add(next, f)) (next + 1) last
                | Some(Unsubscribe id) -> return! loop (listeners.Remove id) next last
                | None ->
                    let now = read ()
                    for f in listeners.Values do safely f now
                    return! loop listeners next now
            }
        loop Map.empty 0 [])

/// Starts listening; dispose the result to stop. `onReading` runs on the agent's thread.
let subscribe (onReading: Action<GpuReading list>) : IDisposable =
    let id = agent.PostAndReply(fun r -> Subscribe(onReading.Invoke, r))
    { new IDisposable with member _.Dispose() = agent.Post(Unsubscribe id) }
