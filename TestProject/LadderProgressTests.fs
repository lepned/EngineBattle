module LadderProgressTests

open Xunit
open ChessLibrary.LadderProgress

let private abcd = [ "A"; "B"; "C"; "D" ]

[<Fact>]
let ``the bottom engine opens the ladder against the one above it`` () =
  let ladder, pair = (next (start abcd)).Value
  Assert.Equal(("D", "C"), pair)
  Assert.Equal(1, ladder.Climb)

[<Fact>]
let ``a winning challenger climbs on against the next engine up`` () =
  let ladder = advance (start abcd) ("D", "C", "D")
  Assert.Equal<string list>([ "A"; "B"; "D" ], ladder.Surviving)
  Assert.Equal<string list>([ "C" ], ladder.Eliminated)
  Assert.Equal(("D", "B"), snd (next ladder).Value)
  Assert.Equal(1, ladder.Climb)

[<Fact>]
let ``a losing challenger is out and the new bottom engine starts the next climb`` () =
  let ladder = advance (start abcd) ("D", "C", "C")
  Assert.Equal<string list>([ "A"; "B"; "C" ], ladder.Surviving)
  Assert.Equal(2, ladder.Climb)
  Assert.Equal(("C", "B"), snd (next ladder).Value)

[<Fact>]
let ``beating the top engine starts a new climb from the bottom`` () =
  // B challenges A with C below (a ladder saved mid-climb)
  let ladder = advance { Surviving = [ "A"; "B"; "C" ]; Eliminated = []; Climb = 3; Climber = 1 } ("B", "A", "B")
  Assert.Equal<string list>([ "B"; "C" ], ladder.Surviving)
  Assert.Equal(4, ladder.Climb)
  Assert.Equal(("C", "B"), snd (next ladder).Value)

[<Fact>]
let ``it ends with one engine left, the others out in order`` () =
  let ladder = replay abcd [ "D", "C", "C"; "C", "B", "B"; "B", "A", "A" ]
  Assert.Equal<string list>([ "A" ], ladder.Surviving)
  Assert.Equal<string list>([ "D"; "C"; "B" ], ladder.Eliminated)
  Assert.True((next ladder).IsNone)

[<Fact>]
let ``a match is decided once the leader cannot be caught, and a tie at the end is not`` () =
  Assert.Equal(Some ChessLibrary.MatchScore.SideA, ChessLibrary.MatchScore.decide 2.0 0.0 1)
  Assert.Equal(None, ChessLibrary.MatchScore.decide 1.5 0.5 1)
  Assert.Equal(Some ChessLibrary.MatchScore.SideB, ChessLibrary.MatchScore.decide 0.5 1.5 0)
  Assert.Equal(None, ChessLibrary.MatchScore.decide 1.0 1.0 0)

[<Fact>]
let ``a game adds its points to the side that played each colour`` () =
  Assert.Equal((1.0, 0.0), ChessLibrary.MatchScore.addGame (0.0, 0.0) true "1-0")
  Assert.Equal((1.0, 0.0), ChessLibrary.MatchScore.addGame (0.0, 0.0) false "0-1")
  Assert.Equal((1.5, 0.5), ChessLibrary.MatchScore.addGame (1.0, 0.0) false "1/2-1/2")
  Assert.Equal((1.0, 0.0), ChessLibrary.MatchScore.addGame (1.0, 0.0) true "*")

[<Fact>]
let ``a cup winner moves to half the match index, as A from an even match`` () =
  Assert.Equal((0, true), ChessLibrary.MatchScore.nextSlot 0)
  Assert.Equal((0, false), ChessLibrary.MatchScore.nextSlot 1)
  Assert.Equal((3, false), ChessLibrary.MatchScore.nextSlot 7)

// ── Swiss standing from its saved rounds ────────────────────────────────────────────────────────

open ChessLibrary.SwissTypes

let private pairing id a b sa sb : SwissPairing =
  { PairId = id; RoundNumber = 1; PlayerA = a; PlayerB = b; PlayerARating = 0; PlayerBRating = 0
    ScoreA = sa; ScoreB = sb; IsDecided = true; Games = ResizeArray(); OpeningOrder = ResizeArray() }

let private rounds : SwissRound list =
  [ { RoundNumber = 1; Pairings = ResizeArray [ pairing 1 "A" "B" 1.5 0.5; pairing 2 "C" "BYE" 1.0 0.0 ] }
    { RoundNumber = 2; Pairings = ResizeArray [ pairing 3 "a" "C" 1.0 1.0; pairing 4 "B" "BYE" 1.0 0.0 ] } ]

[<Fact>]
let ``Swiss standings add each player's points, names in any case, and leave the bye out`` () =
  let s = ChessLibrary.SwissProgress.standings [ "A"; "B"; "C" ] rounds
  Assert.Equal(2.5, s.["A"])
  Assert.Equal(1.5, s.["B"])
  Assert.Equal(2.0, s.["C"])
  Assert.False(s.ContainsKey "BYE")

[<Fact>]
let ``Swiss remembers the pairs met and the byes, a bye not being a pair`` () =
  Assert.Equal(2, (ChessLibrary.SwissProgress.priorPairs rounds).Count)
  Assert.Equal<Set<string>>(set [ "B"; "C" ], ChessLibrary.SwissProgress.byes rounds)
