module RatingFitTests

open System
open Xunit
open ChessLibrary.RatingFit

/// `wins` games won by White, `draws` drawn, `losses` won by Black, between the two.
let private games white black (wins: int, draws: int, losses: int) =
    [ for _ in 1 .. wins -> { White = white; Black = black; Outcome = WhiteWin }
      for _ in 1 .. draws -> { White = white; Black = black; Outcome = Draw }
      for _ in 1 .. losses -> { White = white; Black = black; Outcome = BlackWin } ]

/// A scores the same with both colours, so the white advantage cancels.
let private bothColours a b (w, d, l) = games a b (w, d, l) @ games b a (l, d, w)

let private player (f: Fit) name = f.Players |> List.find (fun p -> p.Name = name)
let private noWhite = { defaultOptions with WhiteAdvantage = Some 0.0 }

[<Fact>]
let ``two players: the difference is the Elo of the score, with the anchor at 0`` () =
    // 740 of 1000 points: 400 log10(0.74 / 0.26) = 181.7
    let g = bothColours "A" "B" (320, 100, 80)
    let f = fit { noWhite with Anchor = Some "B" } g
    Assert.Equal(0.0, (player f "B").Rating, 6)
    Assert.Equal(0.0, (player f "B").Error, 6)
    // the rating prior's pull on 1000 games is a few thousandths of an Elo
    Assert.Equal(181.7, (player f "A").Rating, 1)
    Assert.True((player f "A").Error > 10.0 && (player f "A").Error < 40.0)
    Assert.Equal(1000, (player f "A").Games)
    Assert.Equal((640, 200, 160), ((player f "A").Wins, (player f "A").Draws, (player f "A").Losses))
    // without an anchor the pool average is 0
    let f2 = fit noWhite g
    Assert.Equal(-(player f2 "B").Rating, (player f2 "A").Rating, 6)
    // the same difference: the prior's centre (the anchor or the average) moves it by less than 0.01
    Assert.Equal((player f "A").Rating, (player f2 "A").Rating - (player f2 "B").Rating, 2)

[<Fact>]
let ``a symmetric field is all zeros`` () =
    let g = games "A" "B" (1, 0, 1) @ games "B" "C" (1, 0, 1) @ games "C" "A" (1, 0, 1)
    let f = fit noWhite g
    for p in f.Players do Assert.Equal(0.0, p.Rating, 6)
    Assert.Equal(0.5, f.Cfs "A" "B", 6)

[<Fact>]
let ``a perfect score is a floor or a ceiling, rated as if one game were a draw`` () =
    let f = fit noWhite (games "A" "B" (10, 0, 0))
    Assert.Equal(Floor, (player f "A").Bound)
    Assert.Equal(Ceiling, (player f "B").Bound)
    Assert.True((player f "A").Rating > (player f "B").Rating)
    Assert.False(Double.IsInfinity (player f "A").Rating)
    // the ten games plus a virtual draw: 10.5 of 11, 400 log10(10.5 / 0.5) = 528.9 apart, less the
    // rating prior's pull on eleven games (about 1 %)
    Assert.InRange((player f "A").Rating - (player f "B").Rating, 515.0, 528.9)
    Assert.Equal((10, 0, 0), ((player f "A").Wins, (player f "A").Draws, (player f "A").Losses))

[<Fact>]
let ``unconnected pools are rated apart`` () =
    let f = fit noWhite (bothColours "A" "B" (6, 2, 2) @ bothColours "C" "D" (2, 2, 6))
    Assert.Equal(2, f.Groups.Length)
    Assert.True(Double.IsNaN(f.Cfs "A" "C"))
    Assert.False(Double.IsNaN(f.Cfs "A" "B"))
    Assert.True((player f "A").Rating > 0.0 && (player f "D").Rating > 0.0)

[<Fact>]
let ``the draw model gives d0 for equal opponents and sums to one`` () =
    Assert.Equal(0.4, drawProbability 0.4 0.5, 9)
    Assert.Equal(0.75, drawProbability 0.75 0.5, 9)
    Assert.Equal(0.0, drawProbability 0.0 0.5, 9)
    for d0 in [ 0.05; 0.3; 0.5; 0.7; 0.95 ] do
        for e in [ 0.01; 0.3; 0.5; 0.8; 0.99 ] do
            let w, d, l = wdl d0 e
            Assert.Equal(1.0, w + d + l, 9)
            Assert.Equal(e, w + d / 2.0, 9)
            Assert.True(w >= 0.0 && d >= 0.0 && l >= 0.0)
    // d0 = 1/2 is D = 2 e (1 - e)
    Assert.Equal(2.0 * 0.8 * 0.2, drawProbability 0.5 0.8, 9)
    // a stronger player draws less
    Assert.True(drawProbability 0.4 0.9 < drawProbability 0.4 0.6)

[<Fact>]
let ``the draw rate is fitted from the games`` () =
    let f = fit noWhite (bothColours "A" "B" (150, 200, 150))
    Assert.Equal(40.0, f.DrawRate, 0)
    Assert.True(f.DrawRateError > 0.5 && f.DrawRateError < 5.0)
    // given, it is used as it is
    let f2 = fit { noWhite with DrawRate = Some 55.0 } (bothColours "A" "B" (150, 200, 150))
    Assert.Equal(55.0, f2.DrawRate, 6)
    Assert.Equal(0.0, f2.DrawRateError, 6)
    // no draws at all: 0 and no error
    let f3 = fit noWhite (bothColours "A" "B" (10, 0, 10))
    Assert.Equal(0.0, f3.DrawRate, 6)

[<Fact>]
let ``errors shrink with the square root of the games, and with the confidence`` () =
    let e games = (player (fit { noWhite with Anchor = Some "B" } (bothColours "A" "B" games)) "A").Error
    // the error bars' draw rate is shrunk a little at 100 games, so not exactly 2
    Assert.Equal(2.0, e (30, 10, 10) / e (120, 40, 40), 1)
    let at c = (player (fit { noWhite with Anchor = Some "B"; Confidence = c } (bothColours "A" "B" (30, 10, 10))) "A").Error
    Assert.Equal(1.96, at 0.95 / at 0.6827, 2)
    // more draws at the same score: a smaller error
    Assert.True(e (20, 60, 20) < e (40, 20, 40))

[<Fact>]
let ``one drawn game leaves the rating wide open`` () =
    let f = fit { noWhite with Anchor = Some "B" } (games "A" "B" (0, 1, 0))
    Assert.Equal(0.0, (player f "A").Rating, 6)
    Assert.True((player f "A").Error > 300.0, sprintf "error %.1f" (player f "A").Error)
    // five draws: still hundreds, not tens
    let f5 = fit { noWhite with Anchor = Some "B" } (games "A" "B" (0, 5, 0))
    Assert.True((player f5 "A").Error > 100.0)

[<Fact>]
let ``CFS says how sure it is that the first is stronger`` () =
    let f = fit noWhite (bothColours "A" "B" (60, 20, 40))
    Assert.True(f.Cfs "A" "B" > 0.5)
    Assert.Equal(1.0, f.Cfs "A" "B" + f.Cfs "B" "A", 9)
    let sure = fit noWhite (bothColours "A" "B" (600, 200, 400))
    Assert.True(sure.Cfs "A" "B" > f.Cfs "A" "B")
    Assert.True(sure.Cfs "A" "B" > 0.99)

[<Fact>]
let ``the white advantage is reported from the games, and in the ratings only when asked`` () =
    // both score 70 % with White: equal players, White 147 Elo up
    let g = games "A" "B" (60, 20, 20) @ games "B" "A" (60, 20, 20)
    // the default holds it at 0 in the ratings and reports what the games show (the prior pulls
    // a 200-game estimate a little towards 0)
    let f0 = fit defaultOptions g
    Assert.Equal(0.0, (player f0 "A").Rating, 6)
    Assert.True(f0.WhiteAdvantage > 130.0 && f0.WhiteAdvantage < 147.2 && f0.WhiteAdvantageError > 0.0)
    // fitted with the ratings: the joint estimate, the same here since the players are equal
    let f = fit { defaultOptions with WhiteAdvantage = None } g
    Assert.True(f.WhiteAdvantage > 130.0 && f.WhiteAdvantage < 147.2)
    Assert.Equal((player f "A").Rating, (player f "B").Rating, 6)
    let many = fit { defaultOptions with WhiteAdvantage = None } (games "A" "B" (600, 200, 200) @ games "B" "A" (600, 200, 200))
    Assert.True(many.WhiteAdvantage > f.WhiteAdvantage && many.WhiteAdvantage < 147.2)
    // held at 0, a one-sided score is strength: 70 % with White only is 147 Elo of it
    let oneSided = fit defaultOptions (games "A" "B" (60, 20, 20))
    Assert.Equal(400.0 * log10 (0.7 / 0.3), (player oneSided "A").Rating - (player oneSided "B").Rating, 0)
    // six games where one side was perfect with one colour: finite, moderate numbers
    let tiny = fit { defaultOptions with WhiteAdvantage = None } (games "A" "B" (2, 0, 1) @ games "B" "A" (0, 0, 3))
    Assert.True(abs tiny.WhiteAdvantage < 300.0 && abs (player tiny "A").Rating < 600.0)

[<Fact>]
let ``simulated errors agree with the information matrix`` () =
    let g = bothColours "A" "B" (300, 100, 100)
    let exact = fit { noWhite with Anchor = Some "B" } g
    let simulated = fit { noWhite with Anchor = Some "B"; Simulations = 400; Seed = 7 } g
    Assert.Equal((player exact "A").Rating, (player simulated "A").Rating, 6)
    // 400 replays estimate a standard deviation to about 3.5 %
    Assert.InRange((player simulated "A").Error / (player exact "A").Error, 0.9, 1.1)
    Assert.Equal(0.0, (player simulated "B").Error, 6)

/// The stats pipeline (console tables, GUI standings, pgn elo) takes Elo, error and CFS from the fit.
[<Fact>]
let ``the player statistics carry the fit: two players anchored at the first, one in every game anchored, a round robin at the average`` () =
    let result w b r = ChessLibrary.TypesDef.CoreTypes.createResult w b (ResizeArray()) r ChessLibrary.MiscTypes.ResultReason.Checkmate 1000L
    let pair a b (w, d, l) = [ for _ in 1 .. w -> result a b "1-0" ] @ [ for _ in 1 .. d -> result a b "1/2-1/2" ] @ [ for _ in 1 .. l -> result a b "0-1" ]
    let two = pair "A" "B" (30, 40, 30) @ pair "B" "A" (20, 40, 40) |> ResizeArray
    let stats = ChessLibrary.GameAnalysis.PGNCalculator.getFullStatFromResults two |> Seq.toList
    let a, b = stats |> List.find (fun p -> p.Player = "A"), stats |> List.find (fun p -> p.Player = "B")
    Assert.Equal(0.0, a.Elo, 6)
    Assert.Equal(0.0, a.Error, 6)
    Assert.True(b.Elo < 0.0 && b.Error > 0.0)
    Assert.True(a.CFS > 50 && b.CFS = -1)
    // three in a chain: B plays every game, so B is the reference (0, no error); the CFS against
    // the next in rating order
    let three = pair "A" "B" (60, 20, 20) @ pair "B" "A" (20, 20, 60) @ pair "B" "C" (60, 20, 20) @ pair "C" "B" (20, 20, 60) |> ResizeArray
    let stats = ChessLibrary.GameAnalysis.PGNCalculator.getFullStatFromResults three |> Seq.toList
    let find n = stats |> List.find (fun p -> p.Player = n)
    Assert.Equal(0.0, (find "B").Elo, 6)
    Assert.Equal(0.0, (find "B").Error, 6)
    Assert.Equal(-(find "C").Elo, (find "A").Elo, 6)
    Assert.True((find "A").CFS > 90 && (find "B").CFS > 90 && (find "C").CFS = -1)
    // a gauntlet's challenger is the reference from its first game, before a third player has played
    let early = ChessLibrary.GameAnalysis.PGNCalculator.getFullStat true [ "S" ] [ "S"; "M"; "C" ] (pair "S" "C" (1, 1, 0) |> ResizeArray)
    let s = early |> Seq.find (fun p -> p.Player = "S")
    Assert.True(s.Elo = 0.0 && s.Error = 0.0)
    Assert.True((early |> Seq.find (fun p -> p.Player = "C")).Elo < 0.0)
    // two challengers: the pool average, also while only one of them has played (a star so far)
    let pa = ChessLibrary.GameAnalysis.PGNCalculator.getFullStat
    let two = pa true [ "S"; "T" ] [ "S"; "T"; "C"; "D" ] (pair "S" "C" (1, 1, 0) @ pair "S" "D" (0, 1, 1) |> ResizeArray)
    Assert.True(two |> Seq.filter (fun p -> p.Played > 0) |> Seq.forall (fun p -> p.Error > 0.0))
    // the same games outside a gauntlet: the pool average
    let rr = ChessLibrary.GameAnalysis.PGNCalculator.getFullStat false [] [ "S"; "M"; "C" ] (pair "S" "C" (1, 1, 0) |> ResizeArray)
    Assert.True((rr |> Seq.find (fun p -> p.Player = "S")).Error > 0.0)
    // a player listed but not yet played has no rating ("---"), never the reference's 0 with no error
    let listed = ChessLibrary.GameAnalysis.PGNCalculator.calculateStatistics [ "A"; "B"; "M" ] (pair "A" "B" (3, 2, 1))
    let m = listed |> List.find (fun p -> p.Player = "M")
    Assert.True(Double.IsNaN m.Elo && Double.IsNaN m.Error && m.CFS = -1)
    Assert.Equal(None, ChessLibrary.GameAnalysis.PGNCalculator.anchorOf listed |> Option.filter (fun p -> p.Player = "M"))
    // a round robin of the three (A and C play too): the pool average, nobody without an error
    let rr = Seq.append three (pair "A" "C" (30, 40, 30) @ pair "C" "A" (30, 40, 30)) |> ResizeArray
    let stats = ChessLibrary.GameAnalysis.PGNCalculator.getFullStatFromResults rr |> Seq.toList
    Assert.Equal(0.0, stats |> List.sumBy (fun p -> p.Elo), 6)
    Assert.True(stats |> List.forall (fun p -> p.Error > 0.0))

/// Two players with exactly the same rating: the CFS chain must follow the table's order, not the
/// fit's (there a tie lands in either order), or the --- lands on the wrong row.
[<Fact>]
let ``an exact tie in the rating keeps the CFS on the right rows`` () =
    let result w b r = ChessLibrary.TypesDef.CoreTypes.createResult w b (ResizeArray()) r ChessLibrary.MiscTypes.ResultReason.Checkmate 1000L
    let pair a b (w, d, l) = [ for _ in 1 .. w -> result a b "1-0" ] @ [ for _ in 1 .. d -> result a b "1/2-1/2" ] @ [ for _ in 1 .. l -> result a b "0-1" ]
    // A and B both beat C 4-0 (C's perfect loss gets the virtual draw): A and B tie exactly
    let games = pair "A" "C" (2, 0, 0) @ pair "C" "A" (0, 0, 2) @ pair "B" "C" (2, 0, 0) @ pair "C" "B" (0, 0, 2) |> ResizeArray
    let stats = ChessLibrary.GameAnalysis.PGNCalculator.getFullStatFromResults games |> Seq.sortByDescending ChessLibrary.GameAnalysis.StandingsTable.tableOrder |> Seq.toList
    Assert.Equal((stats.[0]).Elo, (stats.[1]).Elo, 6)
    Assert.Equal("C", stats.[2].Player)
    Assert.True(stats.[0].CFS >= 0 && stats.[1].CFS >= 0, "the first two rows have a next row")
    Assert.Equal(-1, stats.[2].CFS)
    Assert.Equal(50, stats.[0].CFS)

[<Fact>]
let ``a gauntlet rates the challenger against every opponent at once`` () =
    // C beats B as much as B beats A; A plays only B, C plays only B
    let g = bothColours "A" "B" (20, 20, 60) @ bothColours "B" "C" (20, 20, 60)
    let f = fit { noWhite with Anchor = Some "B" } g
    Assert.Equal(0.0, (player f "B").Rating, 6)
    Assert.Equal(-(player f "A").Rating, (player f "C").Rating, 6)
    Assert.Equal<string list>([ "C"; "B"; "A" ], f.Players |> List.map (fun p -> p.Name))
    Assert.Equal(1, f.Groups.Length)

/// A user's gauntlet: Stockfish against five, four games each, White winning nearly every game.
let private userGauntlet =
    let sf = "Stockfish"
    // the opponent's results with White, then with Black, against Stockfish
    let opponent name asWhite asBlack =
        [ for o in asWhite -> { White = name; Black = sf; Outcome = o } ] @ [ for o in asBlack -> { White = sf; Black = name; Outcome = o } ]
    opponent "A" [ WhiteWin; WhiteWin ] [ WhiteWin; WhiteWin ]
    @ opponent "B" [ WhiteWin; WhiteWin ] [ WhiteWin; WhiteWin ]
    @ opponent "C" [ WhiteWin; WhiteWin ] [ WhiteWin; WhiteWin ]
    @ opponent "Ceres" [ WhiteWin; Draw ] [ WhiteWin; WhiteWin ]
    @ opponent "Lc0" [ Draw; Draw ] [ WhiteWin; WhiteWin ]

[<Fact>]
let ``with an anchor the rating prior pulls towards it: an even score against it is exactly 0`` () =
    let f = fit { defaultOptions with Anchor = Some "Stockfish" } userGauntlet
    Assert.Equal(Some "Stockfish", f.Anchor)
    Assert.Equal(0.0, (player f "Stockfish").Rating, 9)
    // exactly 0, not a residual of the solver (which -0.0 would show)
    for even in [ "A"; "B"; "C" ] do Assert.True((player f even).Rating = 0.0 && not (Double.IsNegative (player f even).Rating))
    // 1.5 of 4 and 1 of 4 against it: the score's Elo (-88.7 and -190.8) less the prior's pull on four games
    Assert.InRange((player f "Ceres").Rating, -88.8, -88.0)
    Assert.InRange((player f "Lc0").Rating, -191.0, -188.5)
    // at the pool average nobody is the reference
    Assert.Equal(None, (fit defaultOptions userGauntlet).Anchor)

[<Fact>]
let ``the table's own reference: the first of two, the one in every game, else the pool average`` () =
    let pa = ChessLibrary.GameAnalysis.PGNCalculator.defaultAnchor
    Assert.Equal(Some "Stockfish", pa [ "Stockfish"; "A"; "B"; "C"; "Ceres"; "Lc0" ] userGauntlet)
    // the order of the players does not matter for a gauntlet
    Assert.Equal(Some "Stockfish", pa [ "Lc0"; "A"; "Stockfish" ] userGauntlet)
    Assert.Equal(Some "B", pa [ "B"; "A" ] (bothColours "A" "B" (3, 2, 1)))
    // a round robin, and a gauntlet with only its first opponent so far
    Assert.Equal(None, pa [ "A"; "B"; "C" ] (games "A" "B" (1, 0, 1) @ games "B" "C" (1, 0, 1) @ games "C" "A" (1, 0, 1)))
    Assert.Equal(None, pa [ "S"; "A"; "B" ] (bothColours "S" "A" (2, 0, 2)))
    // an opponent's game against itself (no game of the pool) does not hide the gauntlet
    Assert.Equal(Some "Stockfish", pa [ "Stockfish"; "A"; "B" ] ({ White = "A"; Black = "A"; Outcome = Draw } :: userGauntlet))

[<Fact>]
let ``the pages list a gauntlet in the table order: the reference above those even with it`` () =
    let result w b r = ChessLibrary.TypesDef.CoreTypes.createResult w b (ResizeArray()) r ChessLibrary.MiscTypes.ResultReason.Checkmate 1000L
    let asResult (g: Game) = result g.White g.Black (match g.Outcome with WhiteWin -> "1-0" | Draw -> "1/2-1/2" | BlackWin -> "0-1")
    let stats = ChessLibrary.GameAnalysis.PGNCalculator.getFullStatFromResults (userGauntlet |> List.map asResult |> ResizeArray)
    let order = ChessLibrary.GameAnalysis.StandingsTable.byTableOrder stats |> List.map (fun p -> p.Player)
    // Stockfish (57 %) and the three at 50 % all show 0.0: the score decides, the CFS "---" is last
    Assert.Equal("Stockfish", List.head order)
    Assert.Equal<string list>([ "Ceres"; "Lc0" ], order |> List.skip 4)
    Assert.Equal(-1.0, (stats |> Seq.find (fun p -> p.Player = "Lc0")).CFS)
