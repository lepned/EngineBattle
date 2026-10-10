module TablebaseLookupTests

open System
open System.IO
open Xunit
open ChessLibrary.BoardUtils
open ChessLibrary.TablebaseLookup
open TablebaseProbeTests
open EngineBattle.Tablebases

let private withFolder (names: string list) (test: string -> unit) =
  let dir = Directory.CreateTempSubdirectory("eb_tbl_").FullName
  try
    for n in names do File.WriteAllText(Path.Combine(dir, n), "")
    test dir
  finally Directory.Delete(dir, true)

let private row san category dtz zeroing mate =
  { Uci = san; San = san; Category = category; Dtz = dtz; IsZeroing = zeroing; IsCapture = zeroing
    GivesCheck = mate; IsCheckmate = mate; IsStalemate = false; Uncertain = false }

// ------------------------------------------------------------------ without tables

[<Fact>]
let ``moves sort by category, then mate, zeroing and DTZ`` () =
  let rows =
    [| row "Ka2" Loss (Some 10) false false
       row "Kxb2" Loss None true false
       row "Kb1" Loss (Some 30) false false
       row "Qd4" Draw None false false
       row "Qc4" Draw None false false
       row "Qh2" CursedWin (Some 120) false false
       row "Qg7" Win (Some 9) false false
       row "Qb7" Win (Some 3) false false
       row "Qxb2" Win None true false
       row "Qh8" Win (Some 1) false true
       row "Qa7" BlessedLoss (Some 110) false false |]
  let order = sortMoves rows |> Array.map (fun r -> r.San)
  // a win by mate, then zeroing, then the shortest DTZ; a loss by the longest DTZ, zeroing last
  Assert.Equal<string[]>([| "Qh8"; "Qxb2"; "Qb7"; "Qg7"; "Qh2"; "Qc4"; "Qd4"; "Qa7"; "Kb1"; "Ka2"; "Kxb2" |], order)

[<Fact>]
let ``material is named as the Syzygy file`` () =
  Assert.Equal("KRBvKN", materialOf "8/8/3k4/8/3n4/8/1B6/R3K3 w - - 0 1")
  Assert.Equal("KvK", materialOf "8/8/3k4/8/8/8/8/4K3 w")

[<Fact>]
let ``castling rights are removed on request`` () =
  Assert.Equal(Some "r3k3/8/8/8/8/8/8/4K2R w - - 0 1", withoutCastling "r3k3/8/8/8/8/8/8/4K2R w Kq - 0 1")
  Assert.Equal(None, withoutCastling "8/8/3k4/8/8/8/8/4K3 w - - 0 1")

[<Fact>]
let ``what stops an answer is said, not swallowed`` () =
  let kqk = "8/8/8/8/8/2k5/8/K6Q w - - 0 1"
  Assert.True(match (lookup "" "not a fen").Status with InvalidFen errors -> errors.Length > 0 | _ -> false)
  Assert.Equal(NoTablebaseFolder, (lookup "" kqk).Status)
  let gone = Path.Combine(Path.GetTempPath(), "eb_no_such_tb_folder")
  Assert.Equal(FoldersMissing [| gone |], (lookup gone kqk).Status)
  // checkmate and stalemate need no tables
  Assert.Equal(Checkmate, (lookup "" "k6Q/8/1K6/8/8/8/8/8 b - - 0 1").Status)
  Assert.Equal(Stalemate, (lookup "" "k7/2Q5/1K6/8/8/8/8/8 b - - 0 1").Status)
  withFolder [] (fun dir -> Assert.Equal(NoTablesInFolders, (lookup dir kqk).Status))
  withFolder [ "KQvK.rtbw"; "KRvKR.rtbw" ] (fun dir ->
    Assert.Equal(TooManyPieces(5, 4), (lookup dir "8/8/8/8/8/2k5/1p6/K5RQ w - - 0 1").Status)
    // too many pieces is the reason that matters, before castling rights
    Assert.Equal(TooManyPieces(32, 4), (lookup dir "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").Status)
    Assert.Equal(CastlingRights, (lookup dir "r3k3/8/8/8/8/8/8/4K2R w Kq - 0 1").Status)
    // an X-FEN right (king on f1, outermost rook on h1) is a right as the board reads it
    Assert.Equal(CastlingRights, (lookup dir "4k3/8/8/8/8/8/8/R4K1R w K - 0 1").Status))

// ------------------------------------------------------------------ with tables

let private syzygyPath =
  match Environment.GetEnvironmentVariable "EB_SYZYGY_PATH" with
  | null -> ""
  | p -> p

[<SyzygyFact>]
let ``the halfmove clock counts`` () =
  // KQ against K: a win at clock 0, a cursed win at 99
  let fresh = lookup syzygyPath "8/8/8/8/8/2k5/8/K6Q w - - 0 1"
  let late = lookup syzygyPath "8/8/8/8/8/2k5/8/K6Q w - - 99 1"
  Assert.Equal("8/8/8/8/8/2k5/8/K6Q w - - 99 1", late.Fen)
  Assert.True(fresh.HasDtz && late.HasDtz)
  match fresh.Status, late.Status with
  | Answered(Win, Some dtz), Answered(CursedWin, Some lateDtz) ->
      // the best move has the position's own DTZ; the clock does not change the distance
      Assert.Equal(Some dtz, fresh.Moves.[0].Dtz)
      Assert.Equal(dtz, lateDtz)
  | s -> Assert.Fail $"{s}"
  Assert.Equal(None, clockNote late)
  // switched off, the clock reads as 0: the same position is a plain win again
  let ignored = lookupWith false syzygyPath "8/8/8/8/8/2k5/8/K6Q w - - 99 1"
  Assert.Equal("8/8/8/8/8/2k5/8/K6Q w - - 0 1", ignored.Fen)
  Assert.True(match ignored.Status with Answered(Win, _) -> true | _ -> false)
  // Qd5 at clock 99: black's loss is saved by the rule, and a draw can be claimed
  let afterQd5 = lookup syzygyPath (tryMakeMove "8/8/8/8/8/2k5/8/K6Q w - - 99 1" "h1d5").Value
  Assert.True(match afterQd5.Status with Answered(BlessedLoss, Some _) -> true | _ -> false)
  Assert.True((clockNote afterQd5).IsSome)
[<SyzygyFact>]
let ``castling rights without king and rook at home are no rights`` () =
  // Kkq with no rook on h1 and no black rooks: KRvK, answered
  let r = lookup syzygyPath "4k3/8/8/8/8/8/8/R3K3 w Kkq - 0 1"
  Assert.Equal("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", r.Fen)
  Assert.True(match r.Status with Answered(Win, Some _) -> true | _ -> false)

[<SyzygyFact>]
let ``mate comes first and a zeroing move has no DTZ`` () =
  let mate = lookup syzygyPath "k7/8/1K6/8/8/8/7Q/8 w - - 0 1"
  Assert.Equal("h2h8", mate.Moves.[0].Uci)
  Assert.True(mate.Moves.[0].IsCheckmate)
  // black takes the queen: the only draw, before every loss
  let take = lookup syzygyPath "8/8/8/8/8/2k5/2Q5/K7 b - - 0 1"
  Assert.Equal(Answered(Draw, None), take.Status)
  let first = take.Moves.[0]
  Assert.Equal(("c3c2", Draw, None, true), (first.Uci, first.Category, first.Dtz, first.IsZeroing))
  Assert.True(take.Moves |> Array.skip 1 |> Array.forall (fun m -> m.Category = Loss && m.Dtz.IsSome))

[<SyzygyFact>]
let ``every answer matches Fathom's, also from the WDL tables alone`` () =
  let lines =
    File.ReadAllLines(Path.Combine(AppContext.BaseDirectory, "TestData", "SyzygyGolden.txt"))
    |> Array.filter (fun l -> l <> "" && not (l.StartsWith "#"))
  Assert.True(lines.Length > 900)
  let names (rows: TbMoveRow[]) (cs: TbCategory list) =
    rows |> Seq.filter (fun m -> List.contains m.Category cs) |> Seq.map (fun m -> m.Uci) |> Seq.sort |> String.concat " "
  let wrong =
    lines
    |> Array.Parallel.choose (fun line ->
        let f = line.Split ';'
        let fen, wdl, dtz = f.[0], f.[1], f.[2]
        let r = lookup syzygyPath fen
        match r.Status with
        | Checkmate when wdl = "Win" && dtz = "0" -> None
        | Stalemate when wdl = "Draw" && f.[3..] |> Array.forall ((=) "") -> None
        | Answered(c, d) ->
            let got =
              [ string c; string (defaultArg d 0)
                names r.Moves [ Win ]; names r.Moves [ CursedWin; Draw; BlessedLoss ]; names r.Moves [ Loss ] ]
            let fromWdl = rowsFromWdl r.Fen (legalMovesOf r.Fen)
            let sameWdl =
              match fromWdl with
              | Some rows ->
                  // The WDL table reads the child's clock as 0: where the DTZ answer is a cursed
                  // win or blessed loss and the WDL one a plain win or loss, the WDL row must say
                  // it is Uncertain - or, at clock 0, be the move exactly on the border (DTZ 101).
                  let clock = halfmoveClock r.Fen
                  let agrees (m: TbMoveRow) =
                    let o = rows |> Array.find (fun x -> x.Uci = m.Uci)
                    let ruleOnly = (m.Category, o.Category) |> fun p -> p = (CursedWin, Win) || p = (BlessedLoss, Loss)
                    o.Category = m.Category
                    || (ruleOnly && (o.Uncertain || (clock = 0 && m.Dtz = Some 101)))
                  rows.Length = r.Moves.Length && r.Moves |> Array.forall agrees
              | None -> false
            if got = List.ofArray f.[1..] && sameWdl then None
            else Some $"{line}  ours {String.Join(';', got)} wdl-only agrees {sameWdl}"
        | s -> Some $"{line}  ours {s}")
  Assert.True(wrong.Length = 0, String.Join("\n", wrong |> Array.truncate 10))

// ------------------------------------------------------------------ the tb verb's command line

[<Fact>]
let ``tb takes an unquoted FEN whole, and refuses what it does not know`` () =
  let parsed (args: string list) = CliParser.CustomParser.parse (Array.ofList ("eb-cli" :: args))
  match parsed [ "tb"; "8/8/8/8/8/2k5/8/K6Q"; "b"; "-"; "-"; "0"; "1"; "--json" ] with
  | [ CliParser.Verb(CliParser.Tablebase(fen, None, true, false)) ] -> Assert.Equal("8/8/8/8/8/2k5/8/K6Q b - - 0 1", fen)
  | other -> Assert.Fail $"{other}"
  match parsed [ "tb"; "8/8/8/8/8/2k5/8/K6Q b - - 0 1"; "--tb"; "D:/t"; "--ignore-clock" ] with
  | [ CliParser.Verb(CliParser.Tablebase(fen, Some "D:/t", false, true)) ] -> Assert.Equal("8/8/8/8/8/2k5/8/K6Q b - - 0 1", fen)
  | other -> Assert.Fail $"{other}"
  Assert.ThrowsAny<exn>(fun () -> parsed [ "tb"; "8/8/8/8/8/2k5/8/K6Q b - - 0 1"; "--jsn" ] |> ignore) |> ignore
