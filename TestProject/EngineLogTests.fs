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

// Windows forbids these characters (a ':' even opened an alternate data stream); Linux only '/' and '\0',
// so there the test passes either way.
[<Fact>]
let ``an engine name with characters a file name cannot hold still gets a log`` () =
  let dir = Directory.CreateTempSubdirectory("eb_iolog_").FullName
  try
    let path, log = IoLog.OpenAt(dir, "Lc0 v0.31: <net?*>|\"x\"", "2026-10-06_20-30-00")
    log.Write(">>>", "uci")
    (log :> IDisposable).Dispose()
    Assert.True(File.Exists path)
    let name = Path.GetFileName path
    Assert.DoesNotContain(" ", name)
    Assert.Equal(-1, name.IndexOfAny(Path.GetInvalidFileNameChars()))
  finally Directory.Delete(dir, true)
