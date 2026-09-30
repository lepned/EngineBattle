/// The reference scoreboard over EngineBattle games (ChessLibrary/Match/MatchScoreboard.fs), and
/// the SPRT decision on it: pairs by opening hash and colour, W/L/D of completed pairs only in
/// pentanomial mode, results from the first engine's view.
module MatchScoreboardTests

open Xunit
open ChessLibrary.Match
open ChessLibrary.Match.MatchStats
open ChessLibrary.Match.MatchScoreboard

let private penta () = Scoreboard([ "A"; "B" ], true)

[<Fact>]
let ``without pentanomial every game counts at once, from the first engine's view`` () =
    let s = Scoreboard([ "A"; "B" ], false)
    Assert.True(s.Add("A", "B", "1-0", "h1"))
    Assert.True(s.Add("B", "A", "1-0", "h1"))
    Assert.True(s.Add("B", "A", "1/2-1/2", "h2"))
    Assert.Equal(Stats.OfWld(1, 1, 1), s.Stats("A", "B"))
    Assert.Equal(Stats.OfWld(1, 1, 1), s.Stats("B", "A"))
    Assert.Equal(3, s.Games)

[<Fact>]
let ``a pair counts when its second game ends, with one pentanomial bucket`` () =
    let s = penta ()
    Assert.False(s.Add("A", "B", "1-0", "h1"))
    Assert.Equal(Stats.Empty, s.Stats("A", "B"))
    Assert.True(s.Add("B", "A", "1/2-1/2", "h1"))
    Assert.Equal({ Stats.OfWld(1, 0, 1) with PentaWD = 1 }, s.Stats("A", "B"))
    Assert.Equal({ Stats.OfWld(0, 1, 1) with PentaLD = 1 }, s.Stats("B", "A"))
    Assert.Equal((2, 1), (s.Games, s.CompletedPairs))

[<Theory>]
[<InlineData("1-0", "0-1", "WW")>]
[<InlineData("1-0", "1/2-1/2", "WD")>]
[<InlineData("1-0", "1-0", "WL")>]
[<InlineData("1/2-1/2", "1/2-1/2", "DD")>]
[<InlineData("0-1", "1/2-1/2", "LD")>]
[<InlineData("0-1", "1-0", "LL")>]
let ``each bucket, A white first`` (first: string, second: string, bucket: string) =
    let s = penta ()
    s.Add("A", "B", first, "h") |> ignore
    s.Add("B", "A", second, "h") |> ignore
    let st = s.Stats("A", "B")
    let got =
        [ "WW", st.PentaWW; "WD", st.PentaWD; "WL", st.PentaWL; "DD", st.PentaDD; "LD", st.PentaLD; "LL", st.PentaLL ]
        |> List.filter (fun (_, n) -> n = 1) |> List.map fst
    Assert.Equal<string list>([ bucket ], got)

[<Fact>]
let ``games finishing out of order pair by opening and colour`` () =
    let s = penta ()
    s.Add("A", "B", "1-0", "h1") |> ignore
    s.Add("A", "B", "1-0", "h2") |> ignore
    // the same opening again (a book that wrapped) with the same colours: no pair yet
    Assert.False(s.Add("A", "B", "0-1", "h1"))
    Assert.True(s.Add("B", "A", "0-1", "h2"))    // A wins both on h2
    Assert.True(s.Add("B", "A", "1-0", "h1"))    // pairs with the first open h1 game (A won): WL
    Assert.Equal(2, s.CompletedPairs)
    let st = s.Stats("A", "B")
    Assert.Equal((1, 1), (st.PentaWW, st.PentaWL))
    Assert.Equal(Stats.OfWld(3, 1, 0).Wins, st.Wins)
    Assert.Equal(1, st.Losses)

[<Fact>]
let ``unfinished results and games of other engines are ignored`` () =
    let s = penta ()
    Assert.False(s.Add("A", "B", "*", "h"))
    Assert.False(s.Add("A", "Renamed", "1-0", "h"))
    Assert.False(s.Add("A", "A", "1-0", "h"))
    Assert.Equal(0, s.Games)
    Assert.Equal(Stats.Empty, s.Stats("A", "B"))

[<Fact>]
let ``three engines: per pair and per engine`` () =
    let s = Scoreboard([ "A"; "B"; "C" ], false)
    s.Add("A", "B", "1-0", "h") |> ignore
    s.Add("C", "A", "1-0", "h") |> ignore
    s.Add("B", "C", "1/2-1/2", "h") |> ignore
    Assert.Equal(Stats.OfWld(1, 1, 0), s.EngineStats "A")
    Assert.Equal(Stats.OfWld(0, 1, 1), s.EngineStats "B")
    Assert.Equal(Stats.OfWld(1, 0, 1), s.EngineStats "C")
    Assert.Equal(Stats.OfWld(0, 1, 0), s.Stats("A", "C"))

[<Fact>]
let ``an SPRT on the scoreboard decides H1 for a clearly stronger engine`` () =
    let s = penta ()
    let sprt = MatchSprt.create 0.05 0.05 0.0 5.0 "normalized" true
    let mutable decided = None
    let mutable n = 0
    while decided.IsNone && n < 1000 do
        n <- n + 1
        let h = string n
        s.Add("A", "B", (if n % 3 = 0 then "1/2-1/2" else "1-0"), h) |> ignore
        s.Add("B", "A", (if n % 4 = 0 then "1/2-1/2" else "0-1"), h) |> ignore
        match MatchSprt.result sprt (MatchSprt.llr sprt (s.Stats("A", "B")) true) with
        | MatchSprt.Continue -> ()
        | r -> decided <- Some r
    // The reference's own SPRT on these counts (driver against sprt.cpp, WSL): 14/72/87 DD/WD/WW is
    // LLR 2.9341382340242133, continue; 14/73/87 is 2.9485499661806305, H1 (upper 2.9444)
    Assert.Equal(Some MatchSprt.H1, decided)
    Assert.Equal(174, n)
    let st = s.Stats("A", "B")
    Assert.Equal((14, 73, 87), (st.PentaDD, st.PentaWD, st.PentaWW))
    Assert.Equal(2.9485499661806305, MatchSprt.llr sprt st true, 12)
