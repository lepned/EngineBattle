module EngineDefGenTests

open System
open System.IO
open Xunit
open ChessLibrary

/// `gendefs` writes engine defs from an existing def used as a template. The load-bearing
/// parts are the naming convention read off the template's file name, the substitution that
/// produces the new def's text, and the arm match that decides which checkpoints belong.
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

// ---------- substitution ----------

[<Fact>]
let ``rendering swaps the net file and every step label`` () =
    let out =
        EngineDefGen.renderDef
            templateJson
            "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
            100
            "a4000-21bn11_srv_512_16_deg4_t91_1B_600027136.onnx"
            600
    Assert.Contains("a4000-21bn11_srv_512_16_deg4_t91_1B_600027136.onnx", out)
    Assert.DoesNotContain("lepdev_srv_512_16_deg4_t91_1B_100003840.onnx", out)
    Assert.Contains("\"Name\": \"Ceres srv 512x16 deg4 t91 600M\"", out)
    Assert.Contains("\"Alias\": \"Ceres srv 512x16 deg4 600M\"", out)
    Assert.Contains("600M positions.", out)
    Assert.DoesNotContain("100M", out)

[<Fact>]
let ``the step label is not matched inside a longer number`` () =
    // going from 100M to 1000M must not rewrite the "100M" hiding inside "1100M"
    let text = "\"Version\": \"net at 100M, forked from the 1100M run.\""
    let out = EngineDefGen.renderDef text "x.onnx" 100 "y.onnx" 1000
    Assert.Contains("net at 1000M", out)
    Assert.Contains("the 1100M run", out)

[<Fact>]
let ``untouched fields survive verbatim`` () =
    let out = EngineDefGen.renderDef templateJson "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx" 100 "n.onnx" 700
    Assert.Contains("\"Device\": \"GPU:0#TensorRTNative\"", out)
    Assert.Contains("\"RamLimitMb\": 10096", out)
    Assert.Contains("artifacts/release/net10.0/Ceres.exe", out)

// ---------- planning against a folder ----------

/// Builds a throwaway net folder + def folder and returns both paths.
let private sandbox (nets: string list) (defs: (string * string) list) =
    let root = Path.Combine(Path.GetTempPath(), "ebGenDefs_" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let netDir = Path.Combine(root, "nets")
    let defDir = Path.Combine(root, "defs")
    Directory.CreateDirectory netDir |> ignore
    Directory.CreateDirectory defDir |> ignore
    for n in nets do File.WriteAllText(Path.Combine(netDir, n), "")
    for (name, text) in defs do File.WriteAllText(Path.Combine(defDir, name), text)
    root, netDir, defDir

let private templatePointingAt (netDir: string) (netFile: string) =
    templateJson.Replace(
        "C:/Dev/Chess/Networks/CeresNet/lepned/Server/lepdev_srv_512_16_deg4_t91_1B_100003840.onnx",
        Path.Combine(netDir, netFile).Replace('\\', '/'))

[<Fact>]
let ``plan proposes only the checkpoints that have no def`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx"
          "a4000-21bn11_srv_512_16_deg4_t91_1B_300011520.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplText = templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, tmplText)

        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            // 100M is covered by the template itself; 200M and 300M are not
            Assert.Equal<int list>([ 100 ], p.Covered)
            Assert.Equal<int list>([ 200; 300 ], p.ToWrite |> List.map (fun w -> w.StepM))
            let written = EngineDefGen.write false p
            Assert.Equal(2, written.Length)
            let text300 = File.ReadAllText(Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_300M.json"))
            Assert.Contains("a4000-21bn11_srv_512_16_deg4_t91_1B_300011520.onnx", text300)
            Assert.Contains("300M positions.", text300)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``a raw template never picks up ema nets`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680ema.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            Assert.Equal<int list>([ 200 ], p.ToWrite |> List.map (fun w -> w.StepM))
            Assert.Equal("Ceres_srv_512_16_deg4_t91_200M.json", Path.GetFileName p.ToWrite.Head.DefPath)
            Assert.DoesNotContain("ema", p.ToWrite.Head.NetFile)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``an ema template picks up only ema nets`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840ema.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680ema.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M_ema.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840ema.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            Assert.True p.IsEma
            Assert.Equal<int list>([ 200 ], p.ToWrite |> List.map (fun w -> w.StepM))
            Assert.Equal("Ceres_srv_512_16_deg4_t91_200M_ema.json", Path.GetFileName p.ToWrite.Head.DefPath)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``another arm in the same folder is ignored`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "a4000-21bn11_srv_512_16_t91_1B_900034560.onnx"      // t91, a different arm
          "a4000-21bn11_srv_512_16_v7_1B_100003840.onnx" ]     // v7, another one again
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p -> Assert.Empty p.ToWrite
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``an existing def under a different name still counts as covered`` () =
    // coverage is decided by which net a def points at, not by the def's file name -
    // otherwise a hand-named def gets silently duplicated
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx" ]
    let handNamed =
        templateJson.Replace(
            "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx",
            "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx")
    let root, netDir, defDir = sandbox nets [ "MyOwnName.json", handNamed ]
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p -> Assert.Empty p.ToWrite
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``write refuses to clobber an existing file unless forced`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        // a file already sits on the target name, but mentions no net so it is not "covered"
        let target = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_200M.json")
        File.WriteAllText(target, "{ \"hand\": \"written\" }")

        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            Assert.Empty(EngineDefGen.write false p)
            Assert.Equal("{ \"hand\": \"written\" }", File.ReadAllText target)
            Assert.Single(EngineDefGen.write true p) |> ignore
            Assert.Contains("200M", File.ReadAllText target)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``a template whose name carries no step is refused with a reason`` () =
    let root, netDir, defDir = sandbox [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx" ] []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_deg4.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Ok _ -> failwith "expected a refusal"
        | Result.Error msg -> Assert.Contains("_<step>M", msg)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``two nets bucketing to one step report the dropped one`` () =
    // batch counters differ but both round to 200M, so both want the same def name.
    // One has to lose - it must be named, not silently discarded.
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007681.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath None (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            Assert.Equal<int list>([ 200 ], p.ToWrite |> List.map (fun w -> w.StepM))
            let dropped = p.Skipped |> List.filter (fun (_, why) -> why.Contains "already claims")
            Assert.Single dropped |> ignore
            Assert.Equal("lepdev_srv_512_16_deg4_t91_1B_200007680.onnx", fst dropped.Head)
            Assert.Contains("Dropped", EngineDefGen.render p)
            Assert.Contains("200007680.onnx", EngineDefGen.render p)
    finally
        try Directory.Delete(root, true) with _ -> ()

// ---------- one net named directly ----------

[<Fact>]
let ``a single net path generates just that one def`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680.onnx"
          "a4000-21bn11_srv_512_16_deg4_t91_1B_300011520.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        let one = Path.Combine(netDir, "a4000-21bn11_srv_512_16_deg4_t91_1B_300011520.onnx")
        match EngineDefGen.plan tmplPath (Some one) (Some defDir) with
        | Result.Error e -> failwith e
        | Result.Ok p ->
            // 200M is also missing, but was not asked for
            Assert.Equal<int list>([ 300 ], p.ToWrite |> List.map (fun w -> w.StepM))
            Assert.Equal(1, (EngineDefGen.write false p).Length)
            Assert.False(File.Exists(Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_200M.json")))
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``naming a net from another arm is an error, not a silent no-op`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "a4000-21bn11_srv_512_16_v7_1B_100003840.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        let wrong = Path.Combine(netDir, "a4000-21bn11_srv_512_16_v7_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath (Some wrong) (Some defDir) with
        | Result.Ok _ -> failwith "expected an arm mismatch to be reported"
        | Result.Error msg ->
            Assert.Contains("srv_512_16_v7_1B", msg)
            Assert.Contains("srv_512_16_deg4_t91_1B", msg)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``naming an ema net against a raw template is an error`` () =
    let nets =
        [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx"
          "lepdev_srv_512_16_deg4_t91_1B_200007680ema.onnx" ]
    let root, netDir, defDir = sandbox nets []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        let ema = Path.Combine(netDir, "lepdev_srv_512_16_deg4_t91_1B_200007680ema.onnx")
        match EngineDefGen.plan tmplPath (Some ema) (Some defDir) with
        | Result.Ok _ -> failwith "expected a variant mismatch to be reported"
        | Result.Error msg -> Assert.Contains("EMA variant but the template is raw", msg)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``a missing net file is named in the error`` () =
    let root, netDir, defDir = sandbox [ "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx" ] []
    try
        let tmplPath = Path.Combine(defDir, "Ceres_srv_512_16_deg4_t91_100M.json")
        File.WriteAllText(tmplPath, templatePointingAt netDir "lepdev_srv_512_16_deg4_t91_1B_100003840.onnx")
        match EngineDefGen.plan tmplPath (Some (Path.Combine(netDir, "nope_900000000.onnx"))) (Some defDir) with
        | Result.Ok _ -> failwith "expected a missing-file error"
        | Result.Error msg -> Assert.Contains("nope_900000000.onnx", msg)
    finally
        try Directory.Delete(root, true) with _ -> ()

[<Fact>]
let ``a bare template name says the path is missing`` () =
    match EngineDefGen.plan "Ceres_srv_512_16_v7_100M.json" None None with
    | Result.Ok _ -> failwith "expected a not-found error"
    | Result.Error msg -> Assert.Contains("full path", msg)
