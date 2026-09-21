/// `mkdef` and the Engine creator page make a def from what an engine answers to `uci`. The
/// probe itself needs a real engine; everything after it is pure and tested here from a
/// hand-made answer shaped like Stockfish's and Lc0's.
module EngineProbeTests

open System
open System.IO
open System.Collections.Generic
open Xunit
open ChessLibrary
open ChessLibrary.EngineProbe
open ChessLibrary.TypesDef.CoreTypes

let private opt name t = { UciOption.Name = name; UciOption.OptionType = t }

let private answer (name: string) (author: string) (options: UciOption.UciOption list) =
    let map = Dictionary<string, UciOption.UciOption>(StringComparer.OrdinalIgnoreCase)
    let defaults = Dictionary<string, obj>()
    for o in options do
        map.[o.Name] <- o
        match o.OptionType with
        | UciOption.Check b -> defaults.[o.Name] <- box b
        | UciOption.Spin (_, _, d) -> defaults.[o.Name] <- box d
        | UciOption.Combo (_, d) -> defaults.[o.Name] <- box d
        | UciOption.String s -> defaults.[o.Name] <- box s
        | _ -> ()
    { Path = "C:/Engines/" + name.Replace(" ", "") + ".exe"; Name = name; Author = author; Defaults = defaults; Options = map }

let private stockfish =
    answer "Stockfish 19" "the Stockfish developers" [
        opt "Threads" (UciOption.Spin (1L, 1024L, 1L))
        opt "Hash" (UciOption.Spin (1L, 33554432L, 16L))
        opt "Ponder" (UciOption.Check false)
        opt "SyzygyPath" (UciOption.String "<empty>")
        opt "UCI_ShowWDL" (UciOption.Check false)
        opt "Clear Hash" UciOption.Button ]

let private lc0 =
    answer "Lc0 v0.31.2" "The LCZero Authors" [
        opt "WeightsFile" (UciOption.String "<autodiscover>")
        opt "Backend" (UciOption.Combo ([ "cuda"; "cpu" ], "cuda"))
        opt "LogLiveStats" (UciOption.Check false)
        opt "VerboseMoveStats" (UciOption.Check false) ]

let private ok r = match r with Ok v -> v | Error m -> failwithf "expected Ok, got: %s" m
let private err r = match r with Error m -> m | Ok _ -> failwith "expected Error"

[<Fact>]
let ``the id lines become name, alias, version and author`` () =
    let def = ok (build stockfish Wishes.Empty)
    Assert.Equal("Stockfish 19", def.Name)
    Assert.Equal("Stockfish", def.Alias)
    Assert.Equal("Stockfish 19", def.Version)
    Assert.Equal("the Stockfish developers", def.Dev)
    Assert.Equal("UCI", def.Protocol)
    Assert.Equal("C:/Engines/Stockfish19.exe", def.Path)

[<Fact>]
let ``defaults are carried, buttons are not, and an empty SyzygyPath is dropped`` () =
    let def = ok (build stockfish Wishes.Empty)
    Assert.Equal(box 16L, def.Options.["Hash"])
    Assert.Equal(box false, def.Options.["Ponder"])
    Assert.False(def.Options.ContainsKey "Clear Hash")
    Assert.False(def.Options.ContainsKey "SyzygyPath")

[<Fact>]
let ``tablebases fill an empty SyzygyPath`` () =
    let def = ok (build stockfish { Wishes.Empty with Tablebases = Some @"D:\syzygy" })
    Assert.Equal(box "D:/syzygy", def.Options.["SyzygyPath"])

[<Fact>]
let ``the flags EngineBattle reads are switched on when the engine has them`` () =
    let sf = ok (build stockfish Wishes.Empty)
    let l = ok (build lc0 Wishes.Empty)
    Assert.Equal(box true, sf.Options.["UCI_ShowWDL"])
    Assert.Equal(box true, l.Options.["LogLiveStats"])
    Assert.Equal(box true, l.Options.["VerboseMoveStats"])
    Assert.False(sf.Options.ContainsKey "LogLiveStats")

[<Fact>]
let ``a network goes into the engine's own network option and its folder into NetworkPath`` () =
    let def = ok (build lc0 { Wishes.Empty with NetFile = Some @"C:\Nets\BT4\BT4-332.pb.gz" })
    Assert.Equal(box "C:/Nets/BT4/BT4-332.pb.gz", def.Options.["WeightsFile"])
    Assert.Equal("C:/Nets/BT4", def.NetworkPath)
    Assert.Equal("Img/lc0.png", def.LogoPath)

[<Fact>]
let ``a network for an engine without a network option is refused, not silently dropped`` () =
    let msg = err (build stockfish { Wishes.Empty with NetFile = Some "C:/Nets/x.pb.gz" })
    Assert.Contains("no network option", msg)

[<Fact>]
let ``overrides take the option's type and must exist`` () =
    let def = ok (build stockfish { Wishes.Empty with Overrides = [ "Hash", "512"; "ponder", "true" ] })
    Assert.Equal(box 512L, def.Options.["Hash"])
    Assert.Equal(box true, def.Options.["Ponder"])
    let msg = err (build stockfish { Wishes.Empty with Overrides = [ "Hash", "512"; "Contempt", "20" ] })
    Assert.Contains("Contempt", msg)

[<Fact>]
let ``a base def's values win where the engine still has the option`` () =
    let baseDef =
        { EngineConfig.EmptyWithPath "C:/old/stockfish.exe" with
            Alias = "SF"
            Rating = 3700
            LogoPath = "Img/Stockfish.png"
            Options = Dictionary<string, obj>(dict [ "Hash", box 2048L; "Contempt", box 20L ]) }
    let def = ok (build stockfish { Wishes.Empty with Base = Some baseDef })
    Assert.Equal("SF", def.Alias)
    Assert.Equal(3700, def.Rating)
    Assert.Equal("Img/Stockfish.png", def.LogoPath)
    Assert.Equal(box 2048L, def.Options.["Hash"])
    Assert.False(def.Options.ContainsKey "Contempt", "an option the engine no longer reports is not carried over")
    Assert.Equal("C:/Engines/Stockfish19.exe", def.Path)

[<Fact>]
let ``the json is a def readSingleEngineConfig accepts and carries no run-time fields`` () =
    let def = ok (build lc0 { Wishes.Empty with NetFile = Some "C:/Nets/BT4/BT4-332.pb.gz" })
    let json = toJson def
    Assert.DoesNotContain("IsChallenger", json)
    Assert.DoesNotContain("WinboardConfig", json)
    match Configuration.Validation.readAndValidateEngineConfigJson json "Lc0.json" with
    | Configuration.Validation.Valid -> ()
    | Configuration.Validation.Invalid msgs -> failwithf "def rejected: %A" msgs
    let back = System.Text.Json.JsonSerializer.Deserialize<EngineConfig>(json)
    Assert.Equal("Lc0 v0.31.2", back.Name)
    Assert.Equal("C:/Nets/BT4/BT4-332.pb.gz", string back.Options.["WeightsFile"])

[<Fact>]
let ``the file is named after the engine without spaces and is never overwritten unasked`` () =
    let def = ok (build stockfish Wishes.Empty)
    Assert.Equal("Stockfish19.json", defFileName def)
    let dir = Path.Combine(Path.GetTempPath(), "eb-mkdef-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    try
        let first = ok (write dir def false)
        Assert.True(File.Exists first)
        Assert.Contains("--force", err (write dir def false))
        ok (write dir def true) |> ignore
    finally
        Directory.Delete(dir, true)
