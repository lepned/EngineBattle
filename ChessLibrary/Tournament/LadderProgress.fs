/// Ladder progression as pure functions: where the ladder stands follows from the initial
/// rankings and the decided matches, so a resume replays them instead of trusting saved fields.
module ChessLibrary.LadderProgress

type Ladder =
  { /// Best first.
    Surviving: string list
    /// In the order they went out.
    Eliminated: string list
    Climb: int
    /// The climber's index in Surviving; it challenges the engine just above.
    Climber: int }

let start (rankings: string list) =
  { Surviving = rankings; Eliminated = []; Climb = 1; Climber = rankings.Length - 1 }

let private newClimb (ladder: Ladder) =
  { ladder with Climb = ladder.Climb + 1; Climber = ladder.Surviving.Length - 1 }

/// After a decided match the loser is out; a winning challenger climbs on, else the bottom
/// engine starts a new climb (as it does after beating the top).
let advance (ladder: Ladder) (challenger: string, defender: string, winner: string) =
  let loser = if winner = challenger then defender else challenger
  let ladder =
    { ladder with
        Surviving = ladder.Surviving |> List.filter ((<>) loser)
        Eliminated = ladder.Eliminated @ [ loser ] }
  if winner = challenger then
    match List.tryFindIndex ((=) winner) ladder.Surviving with
    | Some idx when idx > 0 -> { ladder with Climber = idx }
    | _ -> newClimb ladder
  else newClimb ladder

/// The next match as (challenger, defender), with the ladder it is played on; None once one
/// engine is left.
let next (ladder: Ladder) =
  if ladder.Surviving.Length < 2 then None
  else
    let ladder =
      if ladder.Climber < 1 || ladder.Climber >= ladder.Surviving.Length then newClimb ladder else ladder
    Some (ladder, (ladder.Surviving.[ladder.Climber], ladder.Surviving.[ladder.Climber - 1]))

/// The ladder after these decided matches, in the order they were played.
let replay (rankings: string list) (decided: (string * string * string) seq) =
  Seq.fold advance (start rankings) decided
