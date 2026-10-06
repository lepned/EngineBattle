module EngineLogTests

open System
open System.IO
open Xunit
open ChessLibrary.EngineProcess

[<Fact>]
let ``two engines of one name started in the same second get a log each`` () =
  let dir = Directory.CreateTempSubdirectory("eb_iolog_").FullName
  try
    let path1, log1 = IoLog.OpenAt(dir, "Stockfish 19", "2026-10-06_20-02-47")
    let path2, log2 = IoLog.OpenAt(dir, "Stockfish 19", "2026-10-06_20-02-47")
    log1.Write(">>>", "uci from the first")
    log2.Write(">>>", "uci from the second")
    (log1 :> IDisposable).Dispose()
    (log2 :> IDisposable).Dispose()
    Assert.EndsWith("engine_Stockfish_19_2026-10-06_20-02-47.log", path1)
    Assert.EndsWith("engine_Stockfish_19_2026-10-06_20-02-47_2.log", path2)
    Assert.Contains("uci from the first", File.ReadAllText path1)
    Assert.DoesNotContain("uci from the second", File.ReadAllText path1)
    Assert.Contains("uci from the second", File.ReadAllText path2)
  finally Directory.Delete(dir, true)
