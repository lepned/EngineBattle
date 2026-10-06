module TablebaseProbeTests

open System
open System.Diagnostics
open System.IO
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.Chess
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.MiscTypes
open ChessLibrary.TablebaseProbe

let private withTables (names: string list) (test: string -> unit) =
  let dir = Directory.CreateTempSubdirectory("eb_tb_").FullName
  try
    for n in names do File.WriteAllText(Path.Combine(dir, n), "")
    test dir
  finally Directory.Delete(dir, true)

[<Fact>]
let ``the largest table is read from the file names`` () =
  withTables [ "KQvK.rtbw"; "KBBBBvK.rtbw"; "KBBBBvK.rtbz"; "KRPvKR.rtbw" ] (fun dir -> Assert.Equal(6, largestTable dir))
  withTables [] (fun dir -> Assert.Equal(0, largestTable dir))

[<Fact>]
let ``a position with more pieces than the tables is not probed`` () =
  // only 3-piece tables: a 5-piece position gets no answer without Fathom being started
  withTables [ "KQvK.rtbw" ] (fun dir ->
    let answer = probeAsync dir "8/8/8/2k5/8/1R6/4P3/4K1n1 w - - 0 1" 5 Threading.CancellationToken.None |> Async.RunSynchronously
    Assert.True(answer.IsNone))

let private tourny (dir: string) =
  { Tournament.Empty with
      Adjudication = { Tournament.Empty.Adjudication with TBAdj = { TablebaseDirectory = dir; UseTBAdjudication = true; TBMen = 6 } } }

let private boardAt (fen: string) =
  let board = Board()
  board.LoadFen fen
  board

/// White to move with rook and pawn against a knight: a tablebase win.
let private won = "8/8/8/2k5/8/1R6/4P3/4K1n1 w - - 0 1"

[<Fact>]
let ``a position is probed only within the men and with a tablebase directory`` () =
  let dir = Directory.CreateTempSubdirectory("eb_tb_").FullName
  try
    match GameAdjudication.tablebaseProbe (tourny dir) (boardAt won) with
    | Some (d, fen, pieces) ->
        Assert.Equal(dir, d)
        Assert.Equal(5, pieces)
        Assert.StartsWith("8/8/8/2k5/8/1R6/4P3/4K1n1 w", fen)
    | None -> failwith "expected a probe"
    Assert.True((GameAdjudication.tablebaseProbe (tourny dir) (boardAt startPos)).IsNone)
    // castling rights: tablebases have none
    Assert.True((GameAdjudication.tablebaseProbe (tourny dir) (boardAt "r3k3/8/8/8/8/8/8/R3K3 w Qq - 0 1")).IsNone)
    Assert.True((GameAdjudication.tablebaseProbe (tourny dir) (boardAt "r3k3/8/8/8/8/8/8/R3K3 w - - 0 1")).IsSome)
    // already over: insufficient material
    Assert.True((GameAdjudication.tablebaseProbe (tourny dir) (boardAt "8/8/8/4k3/8/8/8/4K3 w - - 0 1")).IsNone)
    Assert.True((GameAdjudication.tablebaseProbe (tourny (Path.Combine(dir, "missing"))) (boardAt won)).IsNone)
  finally Directory.Delete dir

let private adjudicate (evals: EvalType list) tbOutput =
  GameAdjudication.adjudicateByEval NullLogger.Instance (boardAt won) evals (tourny "") "A" "B" "x"
    (Stopwatch.GetTimestamp()) (ResizeArray<string>()) 40 tbOutput

[<Fact>]
let ``the probe's answer adjudicates the game`` () =
  match adjudicate [ EvalType.CP 0.5 ] (Some "[WDL \"Win\"]") with
  | Some r ->
      Assert.Equal("1-0", r.Result)
      Assert.Equal(ResultReason.AdjudicateTB, r.Reason)
  | None -> failwith "expected a tablebase result"
  match adjudicate [ EvalType.CP 0.5 ] (Some "[WDL \"Draw\"]") with
  | Some r -> Assert.Equal("1/2-1/2", r.Result)
  | None -> failwith "expected a tablebase result"

[<Fact>]
let ``without an answer there is no tablebase verdict`` () =
  match adjudicate [ EvalType.CP 0.5 ] None with
  | Some r -> Assert.NotEqual(ResultReason.AdjudicateTB, r.Reason)
  | None -> ()
