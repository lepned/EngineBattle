module RecordAgentTests

open System
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open ChessLibrary
open ChessLibrary.PGNTypes
open ChessLibrary.MiscTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.GameReplay
open ChessLibrary.RecordAgent

let private engine name = { EngineConfig.Empty with Name = name }

let private pairingOf (white: string) (black: string) : Pairing =
  { Opening = PgnGame.Empty 1; White = engine white; Black = engine black
    GameNr = 1; RoundNr = "1.1"; OpeningHash = "same-opening" }

let private tournament prevent =
  { Tournament.Empty with
      PgnOutPath = "run.pgn"
      PreventMoveDeviation = prevent
      EngineSetup = { Tournament.Empty.EngineSetup with Engines = [ engine "A"; engine "B" ] } }

/// A PGN writer that keeps what it is sent; GetResults answers once everything before it is in.
let private fakePgn () =
  let written = ResizeArray<GameMetadata>()
  let agent =
    MailboxProcessor<FullPGNParser.PgnGameMessage>.Start(fun inbox ->
      let rec loop () = async {
        match! inbox.Receive() with
        | FullPGNParser.WriteGame (_, header, _, _) -> written.Add header
        | FullPGNParser.GetResults reply -> reply.Reply (ResizeArray())
        | _ -> ()
        return! loop () }
      loop ())
  let writtenSoFar () =
    agent.PostAndReply FullPGNParser.GetResults |> ignore
    List.ofSeq written
  agent, writtenSoFar

let private recordWith prevent every (already: PgnGame[]) =
  let pgn, written = fakePgn ()
  let record =
    RecordAgent({ Tourny = tournament prevent; Pgn = Some pgn; ReferenceGames = [||]; GamesAlreadyPlayed = already; PeriodicEvery = every },
                NullLogger.Instance)
  record, written

let private game white black reason deviations cancelled =
  { Pairing = pairingOf white black
    Result = { createResult white black (ResizeArray<string>()) "1-0" reason 1000L with GameDeviations = deviations }
    Plies = 10; Moves = ResizeArray<string>(); Movetext = "1. e4 e5"; Replay = None; Cancelled = cancelled }

let private finish (record: RecordAgent) g = record.Finish g |> Async.RunSynchronously

[<Fact>]
let ``played games are recorded and written in the order they end; games never played are not`` () =
  let record, written = recordWith false 0 [||]
  let outcomes =
    [ game "A" "B" ResultReason.Checkmate 0 false
      game "B" "A" ResultReason.NotStarted 0 false
      game "A" "B" ResultReason.Cancel 0 false
      game "B" "A" ResultReason.Checkmate 0 true      // ended as the run was cancelled
      game "B" "A" ResultReason.Checkmate 0 false ]
    |> List.map (fun g -> (finish record g).Played)
  Assert.Equal<bool list>([ true; false; false; false; true ], outcomes)
  Assert.Equal<string list>([ "A"; "B" ], record.Results() |> Async.RunSynchronously |> List.map (fun r -> r.Player1))
  Assert.Equal<string list>([ "A"; "B" ], written () |> List.map (fun h -> h.White))

[<Fact>]
let ``the standings are due after every N games played`` () =
  let record, _ = recordWith false 2 [||]
  let due g = (finish record g).PeriodicDue
  Assert.False(due (game "A" "B" ResultReason.Checkmate 0 false))
  Assert.False(due (game "B" "A" ResultReason.NotStarted 0 false))   // not played: not counted
  Assert.True(due (game "B" "A" ResultReason.Checkmate 0 false))
  Assert.False(due (game "A" "B" ResultReason.Checkmate 0 false))
  Assert.True(due (game "B" "A" ResultReason.Checkmate 0 false))

[<Fact>]
let ``no replay is seeded without deviation prevention`` () =
  let record, _ = recordWith false 0 [||]
  Assert.True((record.Seed (pairingOf "A" "B") |> Async.RunSynchronously).IsNone)

[<Fact>]
let ``a replay that cannot be seeded fails the game instead of playing it unprotected`` () =
  let record, _ = recordWith true 0 [||]
  // an engine the run does not have: seeding throws inside the agent
  Assert.ThrowsAny<exn>(fun () -> record.Seed (pairingOf "C" "A") |> Async.RunSynchronously |> ignore) |> ignore
  Assert.True((record.Seed (pairingOf "A" "B") |> Async.RunSynchronously).IsSome)

[<Fact>]
let ``a game that fails to be recorded leaves the deviation total as it was`` () =
  let record, _ = recordWith true 0 [||]
  let moves = ReferenceGameReplay()
  moves.[1UL] <- { Engine = "C"; Move = "e2e4"; TimeLeftInMs = 0L; Hash = "h" }
  let unknown = { game "C" "D" ResultReason.Checkmate 4 false with Replay = Some (moves, ReferenceGameReplay()) }
  Assert.ThrowsAny<exn>(fun () -> finish record unknown |> ignore) |> ignore
  Assert.Equal(1, (finish record (game "A" "B" ResultReason.Checkmate 1 false)).Metadata.Value.Deviations)

[<Fact>]
let ``Deviations tags carry on from the file and add each game's own count`` () =
  let saved = { PgnGame.Empty 1 with GameMetaData = { GameMetadata.Empty with Deviations = 5 } }
  let record, _ = recordWith false 0 [| saved |]
  let tags =
    [ 2; 0; 3 ]
    |> List.map (fun d -> (finish record (game "A" "B" ResultReason.Checkmate d false)).Metadata.Value.Deviations)
  Assert.Equal<int list>([ 7; 7; 10 ], tags)

[<Fact>]
let ``a game seeded after another was recorded plays its moves; one seeded before does not`` () =
  let record, _ = recordWith true 0 [||]
  let seed () = (record.Seed (pairingOf "A" "B") |> Async.RunSynchronously).Value
  let first, firstBlack = seed ()
  let early, _ = seed ()        // seeded while the first game is still being played
  first.[42UL] <- { Engine = "A"; Move = "e2e4"; TimeLeftInMs = 0L; Hash = "h" }
  let played = { game "A" "B" ResultReason.Checkmate 0 false with Replay = Some (first, firstBlack); Moves = ResizeArray [ "e2e4" ] }
  Assert.True((finish record played).Played)
  let late, _ = seed ()
  Assert.True(late.ContainsKey 42UL)
  Assert.False(early.ContainsKey 42UL)

[<Fact>]
let ``a game the record cannot take fails without stopping the record`` () =
  let record, _ = recordWith true 0 [||]
  // engines the run does not have: merging their replay throws inside the agent
  let moves = ReferenceGameReplay()
  moves.[1UL] <- { Engine = "C"; Move = "e2e4"; TimeLeftInMs = 0L; Hash = "h" }
  let unknown = { game "C" "D" ResultReason.Checkmate 0 false with Replay = Some (moves, ReferenceGameReplay()) }
  // answered at once, not after the reply timeout
  let watch = Diagnostics.Stopwatch.StartNew()
  Assert.ThrowsAny<exn>(fun () -> finish record unknown |> ignore) |> ignore
  Assert.True(watch.ElapsedMilliseconds < 5000L)
  Assert.True((finish record (game "A" "B" ResultReason.Checkmate 0 false)).Played)
  Assert.Equal(1, record.Results() |> Async.RunSynchronously |> List.length)

[<Fact>]
let ``a game's exception carries its deviation count, also when wrapped`` () =
  let ex = InvalidOperationException("engine gone")
  ex.Data.[Game.GameLoop.deviationsKey] <- 3
  Assert.Equal(3, Game.GameLoop.deviationsOf ex)
  Assert.Equal(3, Game.GameLoop.deviationsOf (AggregateException(ex)))
  Assert.Equal(0, Game.GameLoop.deviationsOf (Exception "no game"))

[<Fact>]
let ``the live deviation total starts from the games already in the file`` () =
  let saved = { PgnGame.Empty 1 with GameMetaData = { GameMetadata.Empty with Deviations = 5 } }
  let tourny = tournament false
  RecordAgent({ Tourny = tourny; Pgn = None; ReferenceGames = [||]; GamesAlreadyPlayed = [| saved |]; PeriodicEvery = 0 }, NullLogger.Instance) |> ignore
  Assert.Equal(5, tourny.DeviationCounter)

[<Fact>]
let ``a game that fails to be recorded leaves no moves for later games`` () =
  let record, _ = recordWith true 0 [||]
  // White is one of the run's engines, Black is not: the record fails on Black's moves
  let whiteMoves = ReferenceGameReplay()
  whiteMoves.[7UL] <- { Engine = "A"; Move = "e2e4"; TimeLeftInMs = 0L; Hash = "h" }
  let half = { game "A" "D" ResultReason.Checkmate 0 false with Replay = Some (whiteMoves, ReferenceGameReplay()) }
  Assert.ThrowsAny<exn>(fun () -> finish record half |> ignore) |> ignore
  let white, _ = (record.Seed (pairingOf "A" "B") |> Async.RunSynchronously).Value
  Assert.False(white.ContainsKey 7UL)
