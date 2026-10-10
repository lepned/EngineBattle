/// perft: the move generator counted against the known Chess960 move-tree sizes.
module Commands.Other

open System
open ChessLibrary
open Cli.Spec
open Cli.Parse
open Cli.Output
open Cli.App

[<Literal>]
let Group = "Other"

let perftSpec =
    { command [ "perft" ] Group "Count the move tree on the 960 Chess960 test positions" with
        Description =
          Some "Counts every legal move sequence to the depth and compares it with the known count for each position (Chess960 start positions with castling rights, from the published perft table). Exit 1 when a count is wrong."
        Args = [ arg "depth" "1 to 6" ]
        Options =
          [ value "count" "N" "Test the first N positions (1-960)" |> withDefault "10"
            json ]
        Examples =
          [ "eb-cli perft 5", "Depth 5 on the first 10 positions"
            "eb-cli perft 5 --count 960", "All of them (about a minute)" ] }

let perft (p: Parsed) =
    let given = (argOf p "depth").Value
    let depth =
        match Int32.TryParse given with
        | true, d when d >= 1 && d <= 6 -> d
        | _ -> raise (Usage $"depth must be 1 to 6, got '{given}'")
    let count =
        match intOf p "count" with
        | Some c when c >= 1 && c <= 960 -> c
        | Some c -> raise (Usage $"--count must be 1 to 960, got {c}")
        | None -> 10
    let records =
        // any line ending: the file's checkout may have either
        TestPositions.CHESS960PERFT_POS.Split([| '\r'; '\n' |])
        |> Seq.skip 1
        |> Seq.filter (fun l -> not (String.IsNullOrWhiteSpace l))
        |> Seq.map Test.parseChess960Record
        |> Seq.truncate count
        |> Array.ofSeq
    // no positions would pass with nothing checked
    if records.Length = 0 then failwith "no perft positions were read"
    let jsonMode = has p "json"
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable nodes = 0L
    let wrong = Collections.Generic.List<obj>()
    for r in records do
        let board = Chess.Board()
        board.LoadFen r.FEN
        let got = Perft.perftFast board depth
        let expected = Perft.getNumberFromRecord depth r
        nodes <- nodes + got
        if got <> expected then
            wrong.Add(box {| Position = r.PositionNumber; Fen = r.FEN; Expected = expected; Got = got |})
            if not jsonMode then error $"position {r.PositionNumber}: {got:N0} nodes, expected {expected:N0} ({r.FEN})"
    let secs = max sw.Elapsed.TotalSeconds 0.001
    if jsonMode then
        let result = {| Depth = depth; Positions = records.Length; Wrong = wrong; Nodes = nodes; Seconds = secs; Nps = int64 (float nodes / secs) |}
        let errors = if wrong.Count = 0 then [] else [ { Code = "wrong-count"; Message = $"{wrong.Count} positions have a wrong count" } ]
        printJson (envelope (name p.Command) (wrong.Count = 0) (box result) errors)
    else
        printfn "Depth %d, %d positions: %d right, %d wrong" depth records.Length (records.Length - wrong.Count) wrong.Count
        printfn "Nodes %s in %.1f s (%s nodes/s)" (nodes.ToString("N0")) secs ((int64 (float nodes / secs)).ToString("N0"))
    if wrong.Count = 0 then ExitOk else ExitFailed

let entries: Entry list = [ { Spec = perftSpec; Run = perft } ]
