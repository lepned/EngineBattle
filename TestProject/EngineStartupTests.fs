module EngineStartupTests

open System
open System.Collections.Generic
open Xunit
open ChessLibrary
open ChessLibrary.TypesDef.CoreTypes

let private options () =
  let d = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
  for line in [ "option name Hash type spin default 16 min 1 max 1024"
                "option name Ponder type check default false"
                "option name WeightsFile type string default <autodiscover>" ] do
    UciOption.addOptionToMap d line
  d

[<Fact>]
let ``Lc0 gets --show-hidden, with or without its own arguments`` () =
  Assert.Equal("--show-hidden", EngineStartup.arguments EngineConfig.Empty true)
  Assert.Equal("--x --show-hidden", EngineStartup.arguments { EngineConfig.Empty with Args = "--x" } true)
  Assert.Equal("--show-hidden --x", EngineStartup.arguments { EngineConfig.Empty with Args = "--show-hidden --x" } true)
  Assert.Equal("--x", EngineStartup.arguments { EngineConfig.Empty with Args = "--x" } false)

[<Fact>]
let ``a network is named after its file, and Ceres takes it from its arguments`` () =
  Assert.Equal(Some "BT4", EngineStartup.networkIn "setoption name WeightsFile value C:/nets/BT4.onnx")
  Assert.Equal(Some "nn-1", EngineStartup.networkIn "setoption name EvalFile value nn-1.nnue")
  Assert.Equal(None, EngineStartup.networkIn "setoption name Hash value 64")
  Assert.Equal("C1-640", EngineStartup.ceresNetwork { EngineConfig.Empty with Args = "network=CERES:C1-640" })

[<Fact>]
let ``an option is valid, changed from its default, invalid, or not a setoption`` () =
  let o = options ()
  Assert.Equal(EngineStartup.Valid ("Hash", "64", Some "16"), EngineStartup.check o "setoption name Hash value 64")
  Assert.Equal(EngineStartup.Valid ("Hash", "16", None), EngineStartup.check o "setoption name Hash value 16")
  Assert.Equal(EngineStartup.Invalid ("Hash", "4096"), EngineStartup.check o "setoption name Hash value 4096")
  Assert.Equal(EngineStartup.Invalid ("Threads", "2"), EngineStartup.check o "setoption name Threads value 2")
  Assert.Equal(EngineStartup.Malformed, EngineStartup.check o "isready")

[<Fact>]
let ``an option goes out in the engine's spelling, and its defaults are known`` () =
  let o = options ()
  Assert.Equal("setoption name Hash value 64", EngineStartup.inEngineSpelling o "setoption name hash value 64")
  Assert.Equal("setoption name Threads value 2", EngineStartup.inEngineSpelling o "setoption name Threads value 2")
  let d = EngineStartup.defaults o |> dict
  Assert.Equal(box 16L, d.["Hash"])
  Assert.Equal(box false, d.["Ponder"])

[<Fact>]
let ``move overhead goes to the engine's own option, in its spelling, within its range`` () =
  let withOptions lines =
    let d = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
    for l in lines do UciOption.addOptionToMap d l
    d
  let sf = withOptions [ "option name Move Overhead type spin default 10 min 0 max 5000" ]
  Assert.Equal(Some ("Move Overhead", 0L), EngineStartup.moveOverhead sf [] 0L)
  let lc0 = withOptions [ "option name MoveOverheadMs type spin default 100 min 0 max 100000000" ]
  Assert.Equal(Some ("MoveOverheadMs", 0L), EngineStartup.moveOverhead lc0 [] 0L)
  let floor = withOptions [ "option name Move Overhead type spin default 30 min 10 max 5000" ]
  Assert.Equal(Some ("Move Overhead", 10L), EngineStartup.moveOverhead floor [] 0L)
  // none listed, or the def sets it: nothing is sent
  Assert.Equal(None, EngineStartup.moveOverhead (options ()) [] 0L)
  Assert.Equal(None, EngineStartup.moveOverhead lc0 [ "moveoverheadms" ] 0L)

[<Fact>]
let ``a UCI script sends options while idle, ucinewgame between searches, and no search control`` () =
  Assert.Equal(AsOption "setoption name Hash value 64", ScriptLine.Of "  setoption name Hash value 64 ")
  Assert.Equal(AsNewGame "ucinewgame", ScriptLine.Of "ucinewgame")
  for line in [ "go infinite"; "stop"; "position startpos"; "isready"; "ponderhit"; "quit"; "GO nodes 5" ] do
    Assert.True((match ScriptLine.Of line with Refused _ -> true | _ -> false), line)
  Assert.Equal(AsIs "dump-uci", ScriptLine.Of "dump-uci")
