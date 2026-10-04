/// Scoring a match of games between two players (Cup, Swiss and Ladder): pure.
module ChessLibrary.MatchScore

type Side = SideA | SideB

/// White's and Black's points for a result; anything unfinished scores nothing.
let points (result: string) =
  match result with
  | "1-0" -> 1.0, 0.0
  | "0-1" -> 0.0, 1.0
  | "1/2-1/2" -> 0.5, 0.5
  | _ -> 0.0, 0.0

/// The match score after one more game.
let addGame (scoreA: float, scoreB: float) (whiteIsA: bool) (result: string) =
  let white, black = points result
  if whiteIsA then scoreA + white, scoreB + black else scoreA + black, scoreB + white

/// The winner once the leader cannot be caught in the games left; a tie with none left is not
/// decided (two more games follow).
let decide (scoreA: float) (scoreB: float) (gamesLeft: int) =
  if scoreA > scoreB + float gamesLeft then Some SideA
  elif scoreB > scoreA + float gamesLeft then Some SideB
  else None

/// Where a cup match's winner plays next: the match index in the next round, and whether as
/// player A (even matches) or B.
let nextSlot (matchIndex: int) = matchIndex / 2, matchIndex % 2 = 0
