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
open EngineBattle.Tablebases

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
  match adjudicate [ EvalType.CP 0.5 ] (Some TbWdl.Win) with
  | Some r ->
      Assert.Equal("1-0", r.Result)
      Assert.Equal(ResultReason.AdjudicateTB, r.Reason)
  | None -> failwith "expected a tablebase result"
  match adjudicate [ EvalType.CP 0.5 ] (Some TbWdl.Draw) with
  | Some r -> Assert.Equal("1/2-1/2", r.Result)
  | None -> failwith "expected a tablebase result"
  match adjudicate [ EvalType.CP 0.5 ] (Some TbWdl.Loss) with
  | Some r -> Assert.Equal("0-1", r.Result)
  | None -> failwith "expected a tablebase result"

[<Fact>]
let ``a win or loss the 50-move rule takes away is no tablebase answer`` () =
  let outcome tb = adjudicate [ EvalType.CP 0.5 ] tb |> Option.map (fun r -> r.Result, r.Reason)
  Assert.Equal(outcome None, outcome (Some TbWdl.CursedWin))
  Assert.Equal(outcome None, outcome (Some TbWdl.BlessedLoss))

[<Fact>]
let ``the tablebase answer decides over the engines' evals`` () =
  // both engines say White is winning; the tablebase says draw
  match adjudicate [ EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0 ] (Some TbWdl.Draw) with
  | Some r ->
      Assert.Equal("1/2-1/2", r.Result)
      Assert.Equal(ResultReason.AdjudicateTB, r.Reason)
  | None -> failwith "expected a tablebase result"

/// The tournament's own evaluation rules off (as match without -draw and -resign), or the win rule
/// on: both engines above 5 pawns for a move.
let private adjudicateWith (win: bool) (evals: EvalType list) tbOutput =
  let never = 10000
  let t = tourny ""
  let rules =
    { t.Adjudication with
        DrawOption = { MinDrawMove = never; DrawMoveLength = 1; MaxDrawScore = 0.0 }
        WinOption = if win then { MinWinMove = 0; WinMoveLength = 1; MinWinScore = 5.0 }
                    else { MinWinMove = never; WinMoveLength = 1; MinWinScore = 1000.0 } }
  GameAdjudication.adjudicateByEval NullLogger.Instance (boardAt won) evals { t with Adjudication = rules } "A" "B" "x"
    (Stopwatch.GetTimestamp()) (ResizeArray<string>()) 40 tbOutput

[<Fact>]
let ``without an answer from the tables, the engines' evals do not stand in for one`` () =
  // the old fallback: two evals within a pawn were a draw, two above 5 a win, a mate score
  // counted as both - each called SyzygyTB
  for evals in [ [ EvalType.CP 0.5; EvalType.CP 0.5 ]; [ EvalType.CP 9.0; EvalType.CP 9.0 ]
                 [ EvalType.CP 9.0; EvalType.CP (-9.0) ]; [ EvalType.Mate 3; EvalType.CP 0.5 ] ] do
    Assert.Equal(None, adjudicateWith false evals None)
    Assert.Equal(None, adjudicateWith false evals (Some TbWdl.CursedWin))

[<Fact>]
let ``without an answer from the tables, the tournament's evaluation rules decide`` () =
  match adjudicateWith true [ EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0; EvalType.CP 9.0 ] None with
  | Some r ->
      Assert.Equal("1-0", r.Result)
      Assert.Equal(ResultReason.AdjudicatedEvaluation, r.Reason)
  | None -> failwith "expected the win rule to adjudicate"

[<Fact>]
let ``a position the tables cannot answer is no answer`` () =
  for fen in [ ""; "not a fen"; "8/8/8/8/8/8/8/8 w - - 0 1"                // no kings
               "r3k3/8/8/8/8/8/8/R3K3 w Qq - 0 1"                          // castling rights
               "8/8/8/4k3/8/8/4P3/4K3 x - - 0 1"                           // no side to move
               "8/8/8/4k3/8/8/4P3/4K3/8 w - - 0 1" ] do                   // nine ranks
    let r = Syzygy.ProbeRoot fen
    Assert.False(r.Ok, fen)
    Assert.Equal(TbWdl.Failed, r.Wdl)
    Assert.Equal(TbWdl.Failed, Syzygy.ProbeWdl fen)

// ------------------------------------------------------------------ against Fathom
// TestData/SyzygyGolden.txt: what fathom.exe (tb_probe_root) answered for 910 random positions
// with 3-6 pieces, en passant positions and halfmove clocks up to past the 50-move boundary.
// The tables are not in the repository: the test runs where EB_SYZYGY_PATH names
// a folder with them and is skipped elsewhere.

let private syzygyPath =
  match Environment.GetEnvironmentVariable "EB_SYZYGY_PATH" with
  | null -> ""
  | p -> p

type SyzygyFactAttribute() =
  inherit FactAttribute()
  override _.Skip
    with get () = if largestTable syzygyPath >= 6 then null else $"no 6-piece Syzygy tables in EB_SYZYGY_PATH ('{syzygyPath}')"
    and set _ = ()

let private sorted (moves: seq<string>) = moves |> Seq.sort |> String.concat " "

[<SyzygyFact>]
let ``every answer matches Fathom's`` () =
  startRun true 6 syzygyPath
  let lines =
    File.ReadAllLines(Path.Combine(AppContext.BaseDirectory, "TestData", "SyzygyGolden.txt"))
    |> Array.filter (fun l -> l <> "" && not (l.StartsWith "#"))
  Assert.True(lines.Length > 900)
  let wrong =
    lines
    |> Array.Parallel.choose (fun line ->
        match line.Split ';' with
        | [| fen; wdl; dtz; winning; drawing; losing |] ->
            let r = Syzygy.ProbeRoot fen
            let movesWith (ws: TbWdl list) = r.Moves |> Seq.filter (fun m -> List.contains m.Wdl ws) |> Seq.map (fun m -> m.Uci) |> sorted
            let got =
              [ string r.Wdl; string r.Dtz
                movesWith [ TbWdl.Win ]; movesWith [ TbWdl.CursedWin; TbWdl.Draw; TbWdl.BlessedLoss ]; movesWith [ TbWdl.Loss ] ]
            // checkmated: fathom.exe prints the root as [WDL "Win"] (TB_WIN), with no moves
            let mated = wdl = "Win" && dtz = "0" && winning = "" && drawing = "" && losing = ""
            if mated && r.Ok && r.Checkmate && r.Wdl = TbWdl.Loss then None
            elif r.Ok && got = [ wdl; dtz; winning; drawing; losing ] then None
            else Some $"{fen}: Fathom {wdl};{dtz};{winning};{drawing};{losing}  ours {r.Ok} {String.Join(';', got)}"
        | _ -> Some $"malformed line: {line}")
  Assert.True(wrong.Length = 0, String.Join("\n", wrong |> Array.truncate 10))

[<SyzygyFact>]
let ``the game loop's probe gives the WDL`` () =
  startRun true 6 syzygyPath
  Assert.Equal(Some TbWdl.Win, probe syzygyPath won 5)
  // the halfmove clock counts: KQ against K is a win, unless the 50-move rule ends it first
  Assert.Equal(Some TbWdl.Win, probe syzygyPath "8/8/8/8/8/2k5/8/K6Q w - - 0 1" 3)
  Assert.Equal(Some TbWdl.CursedWin, probe syzygyPath "8/8/8/8/8/2k5/8/K6Q w - - 99 1" 3)
  // seven pieces: more than these tables hold
  Assert.Equal(None, probe syzygyPath "8/8/8/2k5/8/1R6/2PPP3/4K1n1 w - - 0 1" 7)
