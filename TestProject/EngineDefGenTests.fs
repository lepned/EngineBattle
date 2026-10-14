module EngineDefGenTests

open System
open System.IO
open Xunit
open ChessLibrary

/// `engine new <def> --net FILE` copies a def with another network (EngineDefGen.fromDef). A
/// training checkpoint's def (`<prefix>_<step>M[_ema].json`) moves its step on; any other def is
/// named after the net.
///
/// Names here are real ones from C:/Dev/Chess/Networks/CeresNet/lepned/Server.

let private templateJson = """{
  "Name": "Ceres srv 512x16 deg4 t91 100M",
  "Alias": "Ceres srv 512x16 deg4 100M",
  "Version": "server-trained 512-dim 16-layer deg4 t91 net, 100M positions.",
  "Path": "C:/Users/lepne/source/repos/Ceres/artifacts/release/net10.0/Ceres.exe",
  "NetworkPath": "C:/Dev/Chess/Networks/CeresNet/lepned/Server",
  "Options": {
    "Network": "C:/Dev/Chess/Networks/CeresNet/lepned/Server/lepdev_srv_512_16_deg4_t91_1B_100003840.onnx",
    "Device": "GPU:0#TensorRTNative",
    "RamLimitMb": 10096
  }
}"""

// ---------- def file name ----------

[<Fact>]
let ``def name splits into prefix and step`` () =
    match EngineDefGen.parseDefName "C:/defs/Ceres_srv_512_16_deg4_t91_100M.json" with
    | Some (prefix, step, isEma) ->
        Assert.Equal("Ceres_srv_512_16_deg4_t91", prefix)
        Assert.Equal(100, step)
        Assert.False isEma
    | None -> failwith "expected a step in the def name"

[<Fact>]
let ``ema suffix is recognised and kept out of the prefix`` () =
    match EngineDefGen.parseDefName "Ceres_srv_512_16_deg4_t91_1000M_ema.json" with
    | Some (prefix, step, isEma) ->
        Assert.Equal("Ceres_srv_512_16_deg4_t91", prefix)
        Assert.Equal(1000, step)
        Assert.True isEma
    | None -> failwith "expected a step in the def name"

[<Fact>]
let ``a def without a step label is rejected rather than guessed at`` () =
    Assert.True((EngineDefGen.parseDefName "Ceres_BT4_332.json").IsNone)
    Assert.True((EngineDefGen.parseDefName "Stockfish17.json").IsNone)

// ---------- arm matching ----------

[<Fact>]
let ``the machine prefix does not split an arm`` () =
    // the server flipped the host name mid-run: 100M/200M landed as lepdev_, 300M+ as
    // a4000-21bn11_. One training run, so one arm.
    let a = EngineDefGen.armKey "lepdev_srv_512_16_deg4_t91_1B"
    let b = EngineDefGen.armKey "a4000-21bn11_srv_512_16_deg4_t91_1B"
    Assert.Equal(a, b)
    Assert.Equal("srv_512_16_deg4_t91_1B", a)

[<Fact>]
let ``different arms on the same host stay apart`` () =
    let deg4 = EngineDefGen.armKey "a4000-21bn11_srv_512_16_deg4_t91_1B"
    let t91 = EngineDefGen.armKey "a4000-21bn11_srv_512_16_t91_1B"
    let v7 = EngineDefGen.armKey "a4000-21bn11_srv_512_16_v7_1B"
    Assert.NotEqual<string>(deg4, t91)
    Assert.NotEqual<string>(deg4, v7)
    Assert.NotEqual<string>(t91, v7)

[<Fact>]
let ``a name without the srv marker is left alone`` () =
    Assert.Equal("BT4-332", EngineDefGen.armKey "BT4-332")

// ---------- a copy with another net ----------

/// A def in a throwaway folder; returns the folder and the def's path.
let private withDef (fileName: string) (text: string) (test: string -> string -> unit) =
    let dir = Path.Combine(Path.GetTempPath(), "ebDefCopy_" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory dir |> ignore
    try
        let path = Path.Combine(dir, fileName)
        File.WriteAllText(path, text)
        test dir path
    finally Directory.Delete(dir, true)

/// An absolute path on this OS: a Windows drive is not one on Linux, where GetFullPath would put the cwd in front
let private absolute (p: string) = if OperatingSystem.IsWindows() then p else p.Substring 2

let private copied defPath net name =
    match EngineDefGen.fromDef defPath net name None with
    | Ok d -> d
    | Error e -> failwith e

let private failed defPath net name =
    match EngineDefGen.fromDef defPath net name None with
    | Ok d -> failwith $"expected an error, got {d.DefPath}"
    | Error e -> e

[<Fact>]
let ``a checkpoint's def moves its step on and points at the new net`` () =
    withDef "Ceres_srv_512_16_deg4_t91_100M.json" templateJson (fun dir path ->
        let net = absolute "C:/Nets/a4000-21bn11_srv_512_16_deg4_t91_1B_700026880.onnx"
        let d = copied path net None
        Assert.Equal((Path.Combine(dir, "Ceres_srv_512_16_deg4_t91_700M.json")).Replace('\\', '/'), d.DefPath)
        Assert.Equal("Ceres srv 512x16 deg4 t91 700M", d.Name)
        Assert.Contains("\"Network\": \"" + net + "\"", d.Text)
        Assert.Contains("\"NetworkPath\": \"" + absolute "C:/Nets" + "\"", d.Text)
        Assert.Contains("\"Alias\": \"Ceres srv 512x16 deg4 700M\"", d.Text)
        // everything else as it was
        Assert.Contains("\"Device\": \"GPU:0#TensorRTNative\"", d.Text)
        Assert.Contains("\"RamLimitMb\": 10096", d.Text)
        Assert.DoesNotContain("100M", d.Text))

[<Fact>]
let ``the step label is not matched inside a longer number`` () =
    let text = templateJson.Replace("\"RamLimitMb\": 10096", "\"Note\": \"1100M is not 100M\"")
    withDef "Ceres_srv_512_16_deg4_t91_100M.json" text (fun _ path ->
        let d = copied path "C:/Nets/lepdev_srv_512_16_deg4_t91_1B_200007680.onnx" None
        Assert.Contains("1100M is not 200M", d.Text))

[<Fact>]
let ``any other def is named after the net, or --name`` () =
    let lc0 = """{
  "Name": "LC0 BT4 332",
  "Path": "C:/lc0/lc0.exe",
  "Options": {
    "WeightsFile": "C:/Nets/BT4-332.pb.gz",
    "Threads": 2
  }
}"""
    withDef "Lc0_BT4_332.json" lc0 (fun dir path ->
        let d = copied path (absolute "C:/Nets/BT5 big.pb.gz") None
        Assert.Equal("BT5 big", d.Name)
        Assert.Equal((Path.Combine(dir, "BT5big.json")).Replace('\\', '/'), d.DefPath)
        Assert.Contains("\"WeightsFile\": \"" + absolute "C:/Nets/BT5 big.pb.gz" + "\"", d.Text)
        Assert.Contains("\"Name\": \"BT5 big\"", d.Text)
        Assert.Contains("\"Threads\": 2", d.Text)
        // a path as written, not \u-escaped
        Assert.Contains("\"WeightsFile\": \"" + absolute "C:/Nett/Bjørn & co/BT5.pb.gz" + "\"", (copied path (absolute "C:/Nett/Bjørn & co/BT5.pb.gz") None).Text)
        // no NetworkPath in the def, none added
        Assert.DoesNotContain("NetworkPath", d.Text)
        let named = copied path "C:/Nets/BT5.pb.gz" (Some "LC0 BT5")
        Assert.Equal("LC0BT5.json", Path.GetFileName named.DefPath)
        Assert.Contains("\"Name\": \"LC0 BT5\"", named.Text))

[<Fact>]
let ``a def without a network option is told, as is a net of another variant or arm`` () =
    withDef "Stockfish19.json" """{ "Name": "Stockfish 19", "Options": { "Threads": 8, "SyzygyPath": "C:/tb" } }""" (fun _ path ->
        Assert.Contains("has no network option", failed path "C:/Nets/x.onnx" None))
    withDef "Broken.json" """{ "Name": "x", """ (fun _ path ->
        Assert.Contains("is not a def", failed path "C:/Nets/x.onnx" None))
    withDef "Ceres_srv_512_16_deg4_t91_100M.json" templateJson (fun _ path ->
        Assert.Contains("EMA variant and the def raw", failed path "C:/Nets/lepdev_srv_512_16_deg4_t91_1B_200007680ema.onnx" None)
        Assert.Contains("arm 'srv_512_16_v7_1B'", failed path "C:/Nets/lepdev_srv_512_16_v7_1B_200007680.onnx" None)
        // a name of its own takes any net
        Assert.Equal("v7 200M", (copied path "C:/Nets/lepdev_srv_512_16_v7_1B_200007680.onnx" (Some "v7 200M")).Name))
