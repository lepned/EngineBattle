/// `enginecheck` (diagnostic): drives the analysis engine (AnalysisEngine) the way the GUI does,
/// against a real engine, and checks what comes back. Exit code 1 when a check fails.
module EngineCheck

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading
open Microsoft.Extensions.Logging.Abstractions
open ChessLibrary
open ChessLibrary.EngineTypes
open ChessLibrary.TypesDef.CoreTypes

type Params =
  { Config: EngineConfig
    Nodes: int
    /// Searches by time instead of nodes (CECP has no node limit).
    MoveTimeMs: int option
    /// Pause between navigation requests (ms).
    DelayMs: int
    Rounds: int
    Moves: string list }

/// A Ruy Lopez, 30 plies.
let defaultMoves =
  [ "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1b5"; "a7a6"; "b5a4"; "g8f6"; "e1g1"; "f8e7"
    "f1e1"; "b7b5"; "a4b3"; "d7d6"; "c2c3"; "e8g8"; "h2h3"; "c6b8"; "d2d4"; "b8d7"
    "b1d2"; "c8b7"; "b3c2"; "f8e8"; "d2f1"; "e7f8"; "f1g3"; "g7g6"; "a2a4"; "c7c5" ]

let private startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// (position command, FEN) after each prefix of the moves, start included.
let private positions (moves: string list) =
  let board = Chess.Board()
  board.LoadFen startFen
  [ yield "position fen " + startFen, board.FEN()
    for i, m in List.indexed moves do
      board.PlayUciMove m
      let cmd = sprintf "position fen %s moves %s" startFen (String.Join(" ", moves |> List.take (i + 1)))
      yield cmd, board.FEN() ]

type private Check = { Name: string; Ok: bool; Detail: string }

let run (p: Params) : int =
  let updates = ConcurrentQueue<EngineUpdate>()
  let failed = ref ""
  let callback (u: EngineUpdate) =
    updates.Enqueue u
    match u with
    | EngineFailed (_, reason) -> failed.Value <- reason
    | _ -> ()
  printfn "Starting %s ..." p.Config.Name
  let engine = EngineHelper.createAltEngine (callback, p.Config, NullLogger.Instance, false)
  let positions = positions p.Moves
  let limited = match p.MoveTimeMs with Some ms -> sprintf "go movetime %d" ms | None -> sprintf "go nodes %d" p.Nodes
  let await (a: Async<AnalysisOutcome>) = Async.RunSynchronously(a, 120000)
  let take () =
    let items = updates.ToArray()
    updates.Clear()
    items
  let bestMovesOf items = items |> Array.choose (function BestMove b -> Some b | _ -> None)
  let stoppedOf items = items |> Array.filter (function SearchStopped _ -> true | _ -> false) |> Array.length
  let checks = ResizeArray<Check>()
  let check name ok detail =
    checks.Add { Name = name; Ok = ok; Detail = detail }
    printfn "  %-9s %s  %s" name (if ok then "ok  " else "FAIL") detail
  try
    try
      take () |> ignore

      // 1. review: every position in turn, awaited
      let sw = Stopwatch.StartNew()
      let mutable wrong = 0
      let mutable incomplete = 0
      for cmd, fen in positions do
        match await (engine.Search(cmd, limited)) with
        | Completed (Some bm) when bm.FEN = fen -> ()
        | Completed (Some _) -> wrong <- wrong + 1
        | _ -> incomplete <- incomplete + 1
      let items = take ()
      check "review" (wrong = 0 && incomplete = 0)
        (sprintf "%d positions, %d bestmoves, %d for another position, %d without a move, %.1f s"
           positions.Length (bestMovesOf items).Length wrong incomplete sw.Elapsed.TotalSeconds)

      // 2. navigate: fast moves through the game, go infinite each time; only the last one counts
      for round in 1 .. p.Rounds do
        if failed.Value = "" then
          let sw = Stopwatch.StartNew()
          for cmd, _ in positions do
            engine.Analyse(cmd, "go infinite")
            if p.DelayMs > 0 then Thread.Sleep p.DelayMs
          let lastCmd, lastFen = List.last positions
          let outcome = await (engine.Search(lastCmd, limited))
          let items = take ()
          let bms = bestMovesOf items
          let ok =
            match outcome with
            | Completed (Some bm) -> bm.FEN = lastFen && bms.Length = 1
            | _ -> false
          check "navigate" ok
            (sprintf "round %d: %d requests, %d stopped, %d bestmoves (want 1, for the last position), %.1f s"
               round (positions.Length + 1) (stoppedOf items) bms.Length sw.Elapsed.TotalSeconds)

      // 3. options: MultiPV changed during an infinite search restarts it
      let isWinboard = WinboardIntegration.isWinboardEngine p.Config
      let cmd, fen = positions.[positions.Length / 2]
      if not (engine.GetUCICommands().ContainsKey "MultiPV") then
        check "options" true "skipped: the engine has no MultiPV option"
      else
        engine.Analyse(cmd, "go infinite")
        Thread.Sleep 500
        engine.SetOption(EngineOption.Create "MultiPV" "3")
        Thread.Sleep 1000
        let midItems = take ()
        let sawThree = midItems |> Array.exists (function Status s -> s.MultiPV = 3 | _ -> false)
        engine.SetOption(EngineOption.Create "MultiPV" "1")
        let outcome = await (engine.Search(cmd, limited))
        let ok = sawThree && (match outcome with Completed (Some bm) -> bm.FEN = fen | _ -> false)
        check "options" ok (sprintf "MultiPV 3 seen during the search: %b, then MultiPV 1 and a search: %A" sawThree (match outcome with Completed (Some bm) -> bm.Move | o -> sprintf "%A" o))
        take () |> ignore

      // 4. stop: an infinite search stopped still answers with its bestmove
      let running = engine.Search(fst positions.[0], "go infinite")
      Thread.Sleep 500
      engine.Stop()
      // Winboard's stop in analysis is exit, which prints no move
      let ok =
        match await running with
        | Completed (Some _) -> true
        | Superseded -> isWinboard
        | _ -> false
      check "stop" ok (if isWinboard then "analyze, then exit: the search ends without a move" else "go infinite, then stop: the bestmove arrives")

      if failed.Value <> "" then
        check "engine" false ("EngineFailed: " + failed.Value)
        // the engine's own last words, to tell its crash from ours
        printfn "  exit code %s" (match engine.LastExitCode with Some c -> string c | None -> "unknown")
        for line in engine.ErrorOutput |> Seq.truncate 40 do printfn "  stderr: %s" line
    with ex -> check "error" false ex.Message
  finally
    engine.Quit()
  let bad = checks |> Seq.filter (fun c -> not c.Ok) |> Seq.length
  printfn "%s: %d of %d checks passed" p.Config.Name (checks.Count - bad) checks.Count
  if bad = 0 then 0 else 1
