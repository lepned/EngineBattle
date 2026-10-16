/// The Book Evaluation filter (BookEvaluation.bookPositionPasses): a position passes when
/// every engine's eval lies in the size window and the engines agree, the spread taken of the
/// evals with their signs - engines that disagree on who is better do not agree.
module BookEvaluationFilterTests

open Xunit
open ChessLibrary.BookEvaluation

// the page's defaults: min 80, max 100, max diff 40
let private passes = bookPositionPasses 80.0 100.0 40.0

[<Fact>]
let ``engines that agree in the window pass`` () =
    Assert.True(passes [| 90.0; 85.0 |])
    Assert.True(passes [| -90.0; -85.0 |])   // Black better, and both say so

[<Fact>]
let ``engines that disagree on who is better do not pass`` () =
    // the sizes are equal, and they used to be all that was compared
    Assert.False(passes [| 90.0; -90.0 |])
    Assert.Equal(180.0, bookEvalSpread [| 90.0; -90.0 |])

[<Fact>]
let ``an eval outside the window fails, on either side`` () =
    Assert.False(passes [| 90.0; 70.0 |])     // too drawish
    Assert.False(passes [| 90.0; 110.0 |])    // too one-sided
    Assert.False(passes [| -90.0; -110.0 |])

[<Fact>]
let ``the window's edges belong to it, the spread's does not`` () =
    Assert.True(passes [| 80.0; 100.0 |])            // spread 20
    Assert.False(bookPositionPasses 80.0 100.0 20.0 [| 80.0; 100.0 |])   // spread must be below the limit

[<Fact>]
let ``one engine has no spread to check`` () =
    Assert.True(bookPositionPasses 80.0 100.0 10000.0 [| -95.0 |])
    Assert.Equal(0.0, bookEvalSpread [| -95.0 |])

[<Fact>]
let ``empty fields fall back, and one engine has no diff limit`` () =
    let f = parseFilter Pawns "" " " "" "" 2
    Assert.Equal((0.0, 1000.0, 10000.0), (f.MinEval, f.MaxEval, f.MaxDiff))
    Assert.Equal(10000.0, (parseFilter Pawns "0.8" "1.0" "0.4" "" 1).MaxDiff)
    Assert.Equal(40.0, (parseFilter Pawns "0.8" "1.0" "0.4" "" 2).MaxDiff)

[<Fact>]
let ``the fields are pawns, a comma as decimal point, and a centipawn value is told`` () =
    let f = parseFilter Pawns "0,3" "1.2" " 0.5 " "" 2
    Assert.Equal((30.0, 120.0, 50.0), (f.MinEval, f.MaxEval, f.MaxDiff))
    let told (a, b, c) = (Assert.Throws<System.FormatException>(fun () -> parseFilter Pawns a b c "" 2 |> ignore)).Message
    Assert.Equal("min eval is in pawns: 80 centipawns is 0.8", told ("80", "1", "0.4"))
    Assert.Contains("max eval diff takes pawns", told ("0.8", "1", "x"))
    Assert.Contains("min eval is above max eval", told ("1.2", "0.8", ""))

[<Fact>]
let ``a book evaluated before gets this run's MaxEval, not a second one`` () =
    let raw = "[Event \"x\"]\n[MaxEval \"80 by Old, e2e4\"]\n[White \"a\"]\n\n1. e4 e5 *"
    let game = { ChessLibrary.FullPGNParser.parseFullPgnGame raw with Raw = raw }
    let r = ChessLibrary.PGNTypes.PgnEvaluationResult.Create(game, 95.0, "0.95", 0.0, "g1f3", "New", "")
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "eb_bookeval_%s.pgn" (System.Guid.NewGuid().ToString("N")))
    try
        writePgns path [ r ]
        let text = System.IO.File.ReadAllText path
        Assert.DoesNotContain("by Old", text)
        Assert.Contains("[MaxEval \"0.95 by New, g1f3\"]", text)
        Assert.StartsWith("[Event \"x\"]", text)
        Assert.Contains("1. e4 e5 *", text)
    finally System.IO.File.Delete path

[<Fact>]
let ``a mate is written as a mate in the MaxEval tag, not as pawns`` () =
    let raw = "[Event \"x\"]\n\n1. e4 e5 *"
    let game = { ChessLibrary.FullPGNParser.parseFullPgnGame raw with Raw = raw }
    let r = ChessLibrary.PGNTypes.PgnEvaluationResult.Create(game, -2000.0, "-M2", 0.0, "d8h4", "New", "")
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "eb_bookeval_%s.pgn" (System.Guid.NewGuid().ToString("N")))
    try
        writePgns path [ r ]
        Assert.Contains("[MaxEval \"-M2 by New, d8h4\"]", System.IO.File.ReadAllText path)
    finally System.IO.File.Delete path

[<Fact>]
let ``a wrapped comment that starts a line with a bracket stays in the movetext`` () =
    let raw = "[Event \"x\"]\n[White \"a\"]\n\n1. e4 { book move,\n[%clk 0:03:00] } e5 2. Nf3 ; rest of line\nNc6 *"
    let game = { ChessLibrary.FullPGNParser.parseFullPgnGame raw with Raw = raw }
    let r = ChessLibrary.PGNTypes.PgnEvaluationResult.Create(game, 95.0, "0.95", 0.0, "g1f3", "New", "")
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), sprintf "eb_bookeval_%s.pgn" (System.Guid.NewGuid().ToString("N")))
    try
        writePgns path [ r ]
        let back = ChessLibrary.FullPGNParser.parseFullPgnGame (System.IO.File.ReadAllText path)
        Assert.Equal<string list>([ "e4"; "e5"; "Nf3"; "Nc6" ], back.Mainline |> Seq.map (fun m -> m.San) |> List.ofSeq)
        Assert.Equal("a", back.GameMetaData.White)
    finally System.IO.File.Delete path

[<Fact>]
let ``an opening with an illegal move is unreadable, a transposition is a duplicate`` () =
    let game (text: string) = ChessLibrary.FullPGNParser.parseFullPgnGame text
    let unique, unreadable =
        onlyUniqueOpenings [ game "1. e4 e5 2. Nf3 Nc6 *"; game "1. Nf3 Nc6 2. e4 e5 *"; game "1. e4 Ke7 2. Qh8 *" ]
    Assert.Equal(1, unique.Count)
    Assert.Equal(1, unreadable)

[<Fact>]
let ``every search limit has a label, in the report and in the file name`` () =
    let limit = ChessLibrary.TimeControlTypes.TimeControlCommands.SearchLimit.DepthLimit 20
    Assert.Equal("depth 20", limit.Label)
    Assert.Equal(20, limit.Value)
    let engine name l = ({ Config = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name }; Limit = l } : BookEngine)
    let label =
        engineLabel
            [ engine "sf" (ChessLibrary.TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit 10000)
              engine "lc0" (ChessLibrary.TimeControlTypes.TimeControlCommands.SearchLimit.TimeLimit 300)
              engine "berserk" limit ]
    Assert.Equal(sprintf "sf (%sN), lc0 (300ms), berserk (20D)" ((10000).ToString("N0")), label)

// ---------------------------------------------------------------- Win %

let private wdl w d l = (float w, float d, float l)
// the page's defaults: win 55-70, diff 10, no draw cap
let private winPasses = wdlPositionPasses 55.0 70.0 10.0 None

[<Fact>]
let ``in Win % the favoured side's win chance must lie in the window, whichever side it is`` () =
    Assert.True(winPasses [| wdl 60 30 10; wdl 62 28 10 |])
    Assert.True(winPasses [| wdl 10 30 60; wdl 10 28 62 |])    // Black favoured, and both say so
    Assert.False(winPasses [| wdl 50 40 10; wdl 60 30 10 |])   // one engine too drawish
    Assert.False(winPasses [| wdl 75 20 5; wdl 60 30 10 |])    // one engine too one-sided
    // the window's edges belong to it
    Assert.True(winPasses [| wdl 55 35 10; wdl 55 35 10 |])
    Assert.True(winPasses [| wdl 70 20 10; wdl 70 20 10 |])

[<Fact>]
let ``in Win % the engines must agree on who is better, by win minus loss`` () =
    // the favoured sides are equal in size, and the engines disagree on the colour
    Assert.False(winPasses [| wdl 60 30 10; wdl 10 30 60 |])
    Assert.Equal(100.0, bookEvalSpread [| 50.0; -50.0 |])
    // W - L: 50 against 40 is a spread of 10, not below the limit
    Assert.False(winPasses [| wdl 60 30 10; wdl 60 20 20 |])
    Assert.True(wdlPositionPasses 55.0 70.0 11.0 None [| wdl 60 30 10; wdl 60 20 20 |])

[<Fact>]
let ``the draw cap is per engine and off when None`` () =
    Assert.True(wdlPositionPasses 55.0 70.0 10.0 (Some 40.0) [| wdl 60 30 10; wdl 58 40 2 |])
    Assert.False(wdlPositionPasses 55.0 70.0 10.0 (Some 35.0) [| wdl 60 30 10; wdl 58 40 2 |])
    Assert.True(winPasses [| wdl 56 44 0 |])

[<Fact>]
let ``passes dispatches on the unit, and a missing WDL fails in Win %`` () =
    let score cp w = { Cp = cp; Eval = ChessLibrary.MiscTypes.CP (cp / 100.0); Wdl = w; Move = "e2e4" }
    let pawns = parseFilter Pawns "0.8" "1.0" "0.4" "" 2
    let win = parseFilter WinPercent "55" "70" "10" "" 2
    Assert.True(ChessLibrary.BookEvaluation.passes pawns [| score 90.0 None; score 85.0 None |])
    Assert.True(ChessLibrary.BookEvaluation.passes win [| score 90.0 (Some (wdl 60 30 10)); score 85.0 (Some (wdl 62 28 10)) |])
    Assert.False(ChessLibrary.BookEvaluation.passes win [| score 90.0 (Some (wdl 60 30 10)); score 85.0 None |])
    // the signed value and the text follow the unit
    Assert.Equal(50.0, signedValue WinPercent (score 90.0 (Some (wdl 60 30 10))))
    Assert.Equal(90.0, signedValue Pawns (score 90.0 (Some (wdl 60 30 10))))
    Assert.Equal("W60 D30 L10", scoreText WinPercent (score 90.0 (Some (wdl 60 30 10))))
    Assert.Equal("0.90", scoreText Pawns (score 90.0 (Some (wdl 60 30 10))))
    // whole percents that sum to 100: the draw takes the rounding
    Assert.Equal("W62 D31 L7", scoreText WinPercent (score 90.0 (Some (62.5, 30.5, 7.0))))
    Assert.Equal((64.0, 29.0, 7.0), wdlPercents (63.5, 29.5, 7.0))

[<Fact>]
let ``the engine's WDL is the side to move's; with Black to move its win is White's loss`` () =
    let printed = ChessLibrary.EngineTypes.WDLType.HasValue { Win = 300.0; Draw = 500.0; Loss = 200.0 }
    Assert.Equal(Some (30.0, 50.0, 20.0), whiteWdl false printed)
    Assert.Equal(Some (20.0, 50.0, 30.0), whiteWdl true printed)
    Assert.Equal(None, whiteWdl true ChessLibrary.EngineTypes.WDLType.NotFound)

[<Fact>]
let ``the Win % fields are percent, a blank draw cap is off, and a bad value is told`` () =
    let f = parseFilter WinPercent "55" "70" "10" "" 2
    Assert.Equal((55.0, 70.0, 10.0, None), (f.MinEval, f.MaxEval, f.MaxDiff, f.MaxDraw))
    Assert.Equal(Some 40.0, (parseFilter WinPercent "55" "70" "10" " 40 " 2).MaxDraw)
    Assert.Equal((0.0, 100.0, 1000.0), (let f = parseFilter WinPercent "" "" "" "" 1 in f.MinEval, f.MaxEval, f.MaxDiff))
    Assert.Equal(1000.0, (parseFilter WinPercent "55" "70" "10" "" 1).MaxDiff)
    let told (a, b, c, d) = (Assert.Throws<System.FormatException>(fun () -> parseFilter WinPercent a b c d 2 |> ignore)).Message
    Assert.Contains("max draw takes a percentage", told ("55", "70", "10", "150"))
    // the diff bounds a spread of W - L, up to 200
    Assert.Equal(150.0, (parseFilter WinPercent "55" "70" "150" "" 2).MaxDiff)
    Assert.Contains("max win diff takes a percentage, 0 to 200", told ("55", "70", "250", ""))
    Assert.Contains("min win takes a percentage", told ("x", "70", "10", ""))
    Assert.Contains("min win is above max win", told ("80", "70", "10", ""))

[<Fact>]
let ``the summary rows name the unit`` () =
    let engine name = ({ Config = { ChessLibrary.TypesDef.CoreTypes.EngineConfig.Empty with Name = name }; Limit = ChessLibrary.TimeControlTypes.TimeControlCommands.SearchLimit.NodeLimit 1 } : BookEngine)
    let rows engines unit maxDraw = summaryRows "b.pgn" 10 3 10 0 0 None false engines unit "55" "70" "10" maxDraw System.TimeSpan.Zero "out.pgn" |> Map.ofList
    let two = [ engine "a"; engine "b" ]
    Assert.Equal("55 - 70 %", (rows two WinPercent "").["Win chance"])
    Assert.Equal("10 %", (rows two WinPercent "").["Max win diff"])
    Assert.False((rows two WinPercent "").ContainsKey "Max draw")
    Assert.Equal("40 %", (rows two WinPercent "40").["Max draw"])
    Assert.Equal("55 - 70", (rows two Pawns "").["Eval range"])
    Assert.Equal("10 pawns", (rows two Pawns "").["Max eval diff"])
    // one engine: no diff is checked, none is shown
    Assert.False((rows [ engine "a" ] Pawns "").ContainsKey "Max eval diff")
    Assert.False((rows [ engine "a" ] WinPercent "").ContainsKey "Max win diff")
