module PairingTests

// Tests for the narrow slice of pairing utilities that don't belong to any
// specific scheduler: opening-hash computation (used everywhere as the resume
// key) and the `Opening.Seed`-driven shuffle. Gauntlet / RoundRobin / Swiss /
// Cup scheduler behavior lives in `SchedulerTests.fs`.

open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.PGNTypes
open ChessLibrary.TournamentPairing.PairingHelper
open ChessLibrary

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

let private mkPlyMove ply moveNumber color san comment =
    { Ply = ply
      MoveNumber = moveNumber
      Color = color
      San = san
      Comment = comment
      Nags = []
      Variations = ResizeArray() }

let private mkGameWithMoves gameNr fen (moves: PlyMove list) =
    { PgnGame.Empty gameNr with
        GameMetaData = { GameMetadata.Empty with Fen = fen }
        Mainline = ResizeArray<PlyMove>(moves) }

// ============================================================================
// Opening-hash invariants
// ============================================================================

[<Fact>]
let ``opening hash is deterministic for identical content`` () =
    let moves =
        [ mkPlyMove 0 1 "w" "e4" ""
          mkPlyMove 1 1 "b" "e5" "" ]
    let game = mkGameWithMoves 1 "startpos" moves
    let hash1 = ChessUtilities.Hash.computeOpeningHashFromGame game
    let hash2 = ChessUtilities.Hash.computeOpeningHashFromGame game
    Assert.Equal(hash1, hash2)

[<Fact>]
let ``opening hash changes when fen or moves change`` () =
    let moves =
        [ mkPlyMove 0 1 "w" "e4" ""
          mkPlyMove 1 1 "b" "e5" "" ]
    let game = mkGameWithMoves 1 "startpos" moves
    let baseHash = ChessUtilities.Hash.computeOpeningHashFromGame game

    let gameDiffFen = { game with GameMetaData = { game.GameMetaData with Fen = "different" } }
    let diffFenHash = ChessUtilities.Hash.computeOpeningHashFromGame gameDiffFen
    Assert.NotEqual<string>(baseHash, diffFenHash)

    let movesDiff =
        [ mkPlyMove 0 1 "w" "d4" ""
          mkPlyMove 1 1 "b" "d5" "" ]
    let gameDiffMoves = mkGameWithMoves 1 "startpos" movesDiff
    let diffMovesHash = ChessUtilities.Hash.computeOpeningHashFromGame gameDiffMoves
    Assert.NotEqual<string>(baseHash, diffMovesHash)

[<Fact>]
let ``opening hash falls back to game number when empty`` () =
    let hash1 = ChessUtilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 1)
    let hash2 = ChessUtilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 2)
    Assert.NotEqual<string>(hash1, hash2)

[<Fact>]
let ``opening hash uses fen when present (epd style)`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let game = mkGameWithMoves 1 fen []
    let hash = ChessUtilities.Hash.computeOpeningHashFromGame game
    let expected =
        let nl = System.Environment.NewLine
        ChessUtilities.Hash.computeOpeningHash (sprintf "[Fen \"%s\"]%s%s" fen nl nl)
    Assert.Equal(expected, hash)

// ============================================================================
// Opening shuffle (salt-based and seed-based)
// ============================================================================

[<Fact>]
let ``shuffleOpenings produces same order for same salt across calls`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]
    let salt = "C:/output/test.pgn|Alpha,Beta"

    let r1 = shuffleOpenings salt openings |> List.map (fun g -> g.Raw)
    let r2 = shuffleOpenings salt openings |> List.map (fun g -> g.Raw)

    Assert.Equal<string list>(r1, r2)

[<Fact>]
let ``shuffleOpenings with different salts produces different order`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]

    let r1 = shuffleOpenings "salt-A" openings |> List.map (fun g -> g.Raw)
    let r2 = shuffleOpenings "salt-B" openings |> List.map (fun g -> g.Raw)

    Assert.NotEqual<string list>(r1, r2)

[<Fact>]
let ``shuffleOpeningsWithSeed is deterministic for same seed`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]

    let r1 = shuffleOpeningsWithSeed 42 openings |> List.map (fun g -> g.Raw)
    let r2 = shuffleOpeningsWithSeed 42 openings |> List.map (fun g -> g.Raw)

    Assert.Equal<string list>(r1, r2)

[<Fact>]
let ``shuffleOpeningsWithSeed differs across different seeds`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]

    let r1 = shuffleOpeningsWithSeed 42 openings |> List.map (fun g -> g.Raw)
    let r2 = shuffleOpeningsWithSeed 43 openings |> List.map (fun g -> g.Raw)

    Assert.NotEqual<string list>(r1, r2)

let private mkOpeningCfg (seed: int) : Opening =
    { OpeningsPath = None
      OpeningsTwice = false
      OpeningsPly = 0
      RandomOpenings = true
      Seed = seed }

[<Fact>]
let ``shuffleOpeningsForTournament uses the Seed field and is stable across restarts`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]
    let opening = mkOpeningCfg 12345

    let r1 = shuffleOpeningsForTournament opening openings |> List.map (fun g -> g.Raw)
    let r2 = shuffleOpeningsForTournament opening openings |> List.map (fun g -> g.Raw)

    Assert.Equal<string list>(r1, r2)

[<Fact>]
let ``shuffleOpeningsForTournament with Seed=0 matches shuffleOpeningsWithSeed 0`` () =
    // Seed=0 is the F# default for the int field; make sure the behavior is
    // the same as passing 0 explicitly (i.e. no special "unset" handling).
    let openings = [ for i in 1..20 -> mkOpening i ]
    let opening = mkOpeningCfg 0

    let viaHelper = shuffleOpeningsForTournament opening openings |> List.map (fun g -> g.Raw)
    let viaDirect = shuffleOpeningsWithSeed 0 openings |> List.map (fun g -> g.Raw)

    Assert.Equal<string list>(viaHelper, viaDirect)
