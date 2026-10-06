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
let ``a position with more pieces than the largest table is skipped`` () =
  Assert.Equal(Skip, probeDecision 6 true 7)
  Assert.Equal(Run probeTimeoutMs, probeDecision 6 true 6)
  // the first probe of a run opens the tables: it may take longer
  Assert.Equal(Run firstProbeTimeoutMs, probeDecision 6 false 5)

[<Fact>]
let ``several tablebase folders count together`` () =
  withTables [ "KQvK.rtbw"; "KRPvKR.rtbw" ] (fun five ->
    withTables [ "KBBBBvK.rtbw" ] (fun six ->
      let both = String.Join(string Path.PathSeparator, [ five; six ])
      Assert.Equal(6, largestTable both)
      Assert.True(tablebasesExist both)
      Assert.False(tablebasesExist (String.Join(string Path.PathSeparator, [ five; Path.Combine(six, "missing") ])))
      Assert.False(tablebasesExist "")))

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
let ``the tablebase answer decides over the engines' evals`` () =
  // both engines say White is winning; the tablebase says draw
  match adjudicate [ EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0 ] (Some "[WDL \"Draw\"]") with
  | Some r ->
      Assert.Equal("1/2-1/2", r.Result)
      Assert.Equal(ResultReason.AdjudicateTB, r.Reason)
  | None -> failwith "expected a tablebase result"
