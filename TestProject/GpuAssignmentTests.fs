module GpuAssignmentTests

open System
open Xunit
open System.Collections.Generic
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.ParallelExecution

let makeConfig deviceOption deviceTemplate (options: (string * obj) list) =
    let dict = Dictionary<string, obj>(StringComparer.OrdinalIgnoreCase)
    for k, v in options do dict.[k] <- v
    { EngineConfig.Empty with
        Name = "TestEngine"
        DeviceOption = deviceOption
        DeviceTemplate = deviceTemplate
        Options = dict }

[<Fact>]
let ``assignDeviceToConfig sets device option for GPU engine`` () =
    let config = makeConfig "Device" "GPU:{0}#TensorRT" ["Device", box "GPU:0#TensorRT"]
    let result = assignDeviceToConfig config 2
    Assert.Equal("GPU:2#TensorRT", result.Options.["Device"] :?> string)

[<Fact>]
let ``assignDeviceToConfig skips engine with empty DeviceOption`` () =
    let config = makeConfig "" "GPU:{0}#TensorRT" ["Threads", box 8]
    let result = assignDeviceToConfig config 1
    Assert.Same(config, result)

[<Fact>]
let ``assignDeviceToConfig skips engine with empty DeviceTemplate`` () =
    let config = makeConfig "Device" "" ["Threads", box 8]
    let result = assignDeviceToConfig config 1
    Assert.Same(config, result)

[<Fact>]
let ``assignDeviceToConfig skips engine with null DeviceOption`` () =
    let config = makeConfig null "GPU:{0}" []
    let result = assignDeviceToConfig config 0
    Assert.Same(config, result)

[<Fact>]
let ``assignDeviceToConfig does not mutate original config`` () =
    let config = makeConfig "Device" "GPU:{0}#TensorRT" ["Device", box "GPU:0#TensorRT"]
    let _ = assignDeviceToConfig config 3
    Assert.Equal("GPU:0#TensorRT", config.Options.["Device"] :?> string)

[<Fact>]
let ``assignDeviceToConfig preserves other options`` () =
    let config = makeConfig "Device" "GPU:{0}#TensorRT" ["Device", box "GPU:0#TensorRT"; "Threads", box 4]
    let result = assignDeviceToConfig config 1
    Assert.Equal("GPU:1#TensorRT", result.Options.["Device"] :?> string)
    Assert.Equal(4, result.Options.["Threads"] :?> int)

[<Fact>]
let ``round-robin assigns GPUs correctly with wrapping`` () =
    let gpus = [| 0; 1; 2 |]
    let assignments = [| for i in 0..5 -> gpus.[i % gpus.Length] |]
    Assert.Equal<int[]>([| 0; 1; 2; 0; 1; 2 |], assignments)

[<Fact>]
let ``single GPU assigns same device to all instances`` () =
    let gpus = [| 0 |]
    let assignments = [| for i in 0..3 -> gpus.[i % gpus.Length] |]
    Assert.Equal<int[]>([| 0; 0; 0; 0 |], assignments)

[<Fact>]
let ``EngineConfig Empty defaults DeviceOption and DeviceTemplate to empty`` () =
    let config = EngineConfig.Empty
    Assert.Equal("", config.DeviceOption)
    Assert.Equal("", config.DeviceTemplate)

[<Fact>]
let ``TestOptions GPUs defaults to null in Tournament Empty`` () =
    let tourny = ChessLibrary.TypesDef.Tournament.Tournament.Empty
    Assert.Null(tourny.TestOptions.GPUs)
