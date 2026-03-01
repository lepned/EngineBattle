module PairingTests

open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.TournamentPairing.PairingHelper
open ChessLibrary

let private mkEngine name =
    { EngineConfig.Empty with Name = name }

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

[<Fact>]
let ``gauntletSingleRound pairs challengers with all opponents`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletSingleRound false false openings.Length challengers opponents openings

    Assert.Equal(2, games.Length)
    Assert.All(games, fun g -> Assert.Equal("A", g.White.Name))
    Assert.Equal<string list>([ "1.1"; "1.2" ], games |> List.map (fun g -> g.RoundNr))
    Assert.Equal<int list>([ 1; 2 ], games |> List.map (fun g -> g.GameNr))

[<Fact>]
let ``gauntletDoubleRound includes color-reversed rematches`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletDoubleRound false false openings.Length challengers opponents openings

    Assert.Equal(4, games.Length)

    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, orderedPairs.[("A", "B")])
    Assert.Equal(1, orderedPairs.[("A", "C")])
    Assert.Equal(1, orderedPairs.[("B", "A")])
    Assert.Equal(1, orderedPairs.[("C", "A")])

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

[<Fact>]
let ``round robin single round creates one game per unordered pair`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinSingleRounds players openings

    Assert.Equal(6, games.Length)

    let unorderedPairs =
        games
        |> List.map (fun g ->
            let a, b = g.White.Name, g.Black.Name
            if a < b then (a, b) else (b, a))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, unorderedPairs.[("A", "B")])
    Assert.Equal(1, unorderedPairs.[("A", "C")])
    Assert.Equal(1, unorderedPairs.[("A", "D")])
    Assert.Equal(1, unorderedPairs.[("B", "C")])
    Assert.Equal(1, unorderedPairs.[("B", "D")])
    Assert.Equal(1, unorderedPairs.[("C", "D")])

[<Fact>]
let ``round robin double round creates both colors for each pair`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinDoubleRounds players openings

    Assert.Equal(12, games.Length)

    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> List.countBy id
        |> Map.ofList

    Assert.Equal(1, orderedPairs.[("A", "B")])
    Assert.Equal(1, orderedPairs.[("B", "A")])
    Assert.Equal(1, orderedPairs.[("A", "C")])
    Assert.Equal(1, orderedPairs.[("C", "A")])
    Assert.Equal(1, orderedPairs.[("A", "D")])
    Assert.Equal(1, orderedPairs.[("D", "A")])
    Assert.Equal(1, orderedPairs.[("B", "C")])
    Assert.Equal(1, orderedPairs.[("C", "B")])
    Assert.Equal(1, orderedPairs.[("B", "D")])
    Assert.Equal(1, orderedPairs.[("D", "B")])
    Assert.Equal(1, orderedPairs.[("C", "D")])
    Assert.Equal(1, orderedPairs.[("D", "C")])

[<Fact>]
let ``round robin keeps colors balanced across first four rounds for 8 players`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F"; mkEngine "G"; mkEngine "H" ]
    let opening = mkOpening 1
    let games = getPairingsPerOpening players opening

    let gamesPerRound = players.Length / 2
    let firstFourRounds = games |> List.take (gamesPerRound * 4)

    let addCount name delta counts =
        let current = counts |> Map.tryFind name |> Option.defaultValue 0
        counts |> Map.add name (current + delta)

    let colorCounts =
        firstFourRounds
        |> List.fold (fun acc g ->
            acc
            |> addCount g.White.Name 1
            |> addCount g.Black.Name -1) Map.empty

    for player in players do
        let diff = colorCounts.[player.Name]
        Assert.Equal(0, diff)

[<Fact>]
let ``round robin keeps colors within one for 9 players`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"; mkEngine "E"
          mkEngine "F"; mkEngine "G"; mkEngine "H"; mkEngine "I" ]
    let opening = mkOpening 1
    let games = getPairingsPerOpening players opening

    let addCount name delta counts =
        let current = counts |> Map.tryFind name |> Option.defaultValue 0
        counts |> Map.add name (current + delta)

    let colorCounts =
        games
        |> List.fold (fun acc g ->
            acc
            |> addCount g.White.Name 1
            |> addCount g.Black.Name -1) Map.empty

    for player in players do
        let diff = colorCounts.[player.Name]
        Assert.True(abs diff <= 1, $"Color imbalance too large for {player.Name}: {diff}")

[<Fact>]
let ``gauntletSingleRound with multiple challengers pairs each against all opponents`` () =
    let challengers = [ mkEngine "A"; mkEngine "B" ]
    let opponents = [ mkEngine "X"; mkEngine "Y" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletSingleRound false false openings.Length challengers opponents openings

    Assert.Equal(4, games.Length)
    let whites = games |> List.map (fun g -> g.White.Name) |> Set.ofList
    Assert.Equal<Set<string>>(Set.ofList [ "A"; "B" ], whites)
    let blacks = games |> List.map (fun g -> g.Black.Name) |> Set.ofList
    Assert.Equal<Set<string>>(Set.ofList [ "X"; "Y" ], blacks)

[<Fact>]
let ``gauntletSingleRound with multiple openings generates games per opening`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = gauntletSingleRound false false openings.Length challengers opponents openings

    // 1 challenger * 2 opponents * 2 openings = 4 games
    Assert.Equal(4, games.Length)
    let openingRaws = games |> List.map (fun g -> g.Opening.Raw) |> List.distinct
    Assert.Equal(2, openingRaws.Length)

[<Fact>]
let ``gauntletSingleRound with doNotDeviate rotates opponents per opening`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let gamesRotated = gauntletSingleRound true false openings.Length challengers opponents openings
    let gamesStatic = gauntletSingleRound false false openings.Length challengers opponents openings

    // Both produce same total count
    Assert.Equal(gamesRotated.Length, gamesStatic.Length)
    // With rotation, the opponent order for opening 2 differs from opening 1
    let opening1Opponents =
        gamesRotated
        |> List.filter (fun g -> g.Opening.Raw = "opening-1")
        |> List.map (fun g -> g.Black.Name)
    let opening2Opponents =
        gamesRotated
        |> List.filter (fun g -> g.Opening.Raw = "opening-2")
        |> List.map (fun g -> g.Black.Name)
    Assert.NotEqual<string list>(opening1Opponents, opening2Opponents)

[<Fact>]
let ``gauntletDoubleRound with multiple openings doubles game count`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = gauntletDoubleRound false false openings.Length challengers opponents openings

    // 1 challenger * 1 opponent * 2 openings * 2 colors = 4 games
    Assert.Equal(4, games.Length)
    let whites = games |> List.map (fun g -> g.White.Name) |> List.sort
    let blacks = games |> List.map (fun g -> g.Black.Name) |> List.sort
    // Each player appears as white and black equally
    Assert.Equal(2, whites |> List.filter (fun n -> n = "A") |> List.length)
    Assert.Equal(2, blacks |> List.filter (fun n -> n = "A") |> List.length)

[<Fact>]
let ``gauntletSingleRound game numbers are sequential`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = gauntletSingleRound false false openings.Length challengers opponents openings

    let gameNrs = games |> List.map (fun g -> g.GameNr)
    Assert.Equal<int list>([ 1; 2; 3; 4 ], gameNrs)

[<Fact>]
let ``round robin single round with odd players includes all players`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"; mkEngine "E" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinSingleRounds players openings

    // C(5,2) = 10 unique pairs
    Assert.Equal(10, games.Length)
    let allNames =
        games
        |> List.collect (fun g -> [ g.White.Name; g.Black.Name ])
        |> Set.ofList
    Assert.Equal(5, allNames.Count)
    // No bye player in output
    Assert.DoesNotContain("", allNames)

[<Fact>]
let ``round robin double round with odd players creates both colors for each pair`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinDoubleRounds players openings

    // C(3,2) = 3 unique pairs * 2 colors = 6 games
    Assert.Equal(6, games.Length)
    let orderedPairs =
        games
        |> List.map (fun g -> (g.White.Name, g.Black.Name))
        |> List.sort
    let expectedPairs =
        [ ("A", "B"); ("A", "C"); ("B", "A"); ("B", "C"); ("C", "A"); ("C", "B") ]
        |> List.sort
    Assert.Equal<(string * string) list>(expectedPairs, orderedPairs)

[<Fact>]
let ``round robin single round every player plays same number of games`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinSingleRounds players openings

    let gameCounts =
        games
        |> List.collect (fun g -> [ g.White.Name; g.Black.Name ])
        |> List.countBy id
        |> Map.ofList
    // Each player plays against all others: 5 games each
    for player in players do
        Assert.Equal(5, gameCounts.[player.Name])

[<Fact>]
let ``round robin with multiple openings multiplies game count`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = generateAllRoundRobinSingleRounds players openings

    // C(4,2) = 6 pairs per opening * 2 openings = 12 games
    Assert.Equal(12, games.Length)
    let perOpening =
        games
        |> List.groupBy (fun g -> g.Opening.Raw)
        |> List.map (fun (_, gs) -> gs.Length)
    Assert.All(perOpening, fun count -> Assert.Equal(6, count))

[<Fact>]
let ``round robin no player faces itself`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F"; mkEngine "G"; mkEngine "H" ]
    let openings = [ mkOpening 1 ]

    let games = generateAllRoundRobinSingleRounds players openings

    for g in games do
        Assert.NotEqual<string>(g.White.Name, g.Black.Name)

[<Fact>]
let ``shuffleOpenings produces same order for same salt across calls`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]
    let salt = "C:/output/test.pgn|Alpha,Beta"

    let result1 = shuffleOpenings salt openings
    let result2 = shuffleOpenings salt openings

    let raws1 = result1 |> List.map (fun g -> g.Raw)
    let raws2 = result2 |> List.map (fun g -> g.Raw)
    Assert.Equal<string list>(raws1, raws2)

[<Fact>]
let ``shuffleOpenings with different salts produces different order`` () =
    let openings = [ for i in 1..20 -> mkOpening i ]

    let result1 = shuffleOpenings "salt-A" openings |> List.map (fun g -> g.Raw)
    let result2 = shuffleOpenings "salt-B" openings |> List.map (fun g -> g.Raw)

    Assert.NotEqual<string list>(result1, result2)

[<Fact>]
let ``gauntletSingleRound randomOffset gives each opponent a unique opening per round`` () =
    let challengers = [ mkEngine "Hero" ]
    let opponents = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ for i in 1..9 -> mkOpening i ]

    let games = gauntletSingleRound false true 3 challengers opponents openings

    // 1 challenger * 3 opponents * 3 rounds = 9 games
    Assert.Equal(9, games.Length)
    // Group by round (3 games per round)
    let rounds = games |> List.chunkBySize 3
    for round in rounds do
        let openingRaws = round |> List.map (fun g -> g.Opening.Raw)
        // Each opponent in the round gets a different opening
        Assert.Equal(3, openingRaws |> List.distinct |> List.length)

[<Fact>]
let ``gauntletSingleRound randomOffset challengers share same opening vs same opponent`` () =
    let challengers = [ mkEngine "Hero1"; mkEngine "Hero2" ]
    let opponents = [ mkEngine "A"; mkEngine "B" ]
    let openings = [ for i in 1..4 -> mkOpening i ]

    let games = gauntletSingleRound false true 2 challengers opponents openings

    // 2 challengers * 2 opponents * 2 rounds = 8 games
    Assert.Equal(8, games.Length)
    // For each (round, opponent) pair, both challengers use the same opening
    let grouped =
        games
        |> List.groupBy (fun g -> g.Opening.Raw, g.Black.Name)
    for ((openingRaw, oppName), gs) in grouped do
        let challengerNames = gs |> List.map (fun g -> g.White.Name) |> Set.ofList
        Assert.Equal(2, challengerNames.Count) // Both challengers present

[<Fact>]
let ``gauntletSingleRound randomOffset with single opponent degenerates to current behavior`` () =
    let challengers = [ mkEngine "Hero" ]
    let opponents = [ mkEngine "Opp" ]
    let openings = [ for i in 1..5 -> mkOpening i ]

    let gamesOffset = gauntletSingleRound false true 5 challengers opponents openings
    let gamesNormal = gauntletSingleRound false false openings.Length challengers opponents openings

    Assert.Equal(gamesNormal.Length, gamesOffset.Length)
    // Same openings used in same order
    let offsetRaws = gamesOffset |> List.map (fun g -> g.Opening.Raw)
    let normalRaws = gamesNormal |> List.map (fun g -> g.Opening.Raw)
    Assert.Equal<string list>(normalRaws, offsetRaws)

[<Fact>]
let ``gauntletDoubleRound randomOffset gives each opponent a unique opening per round`` () =
    let challengers = [ mkEngine "Hero" ]
    let opponents = [ mkEngine "A"; mkEngine "B" ]
    let openings = [ for i in 1..6 -> mkOpening i ]

    let games = gauntletDoubleRound false true 3 challengers opponents openings

    // 1 challenger * 2 opponents * 3 rounds * 2 colors = 12 games
    Assert.Equal(12, games.Length)
    // Group by opening: each opening should appear exactly twice (both colors)
    let perOpening =
        games
        |> List.groupBy (fun g -> g.Opening.Raw)
        |> List.map (fun (_, gs) -> gs.Length)
    Assert.All(perOpening, fun count -> Assert.Equal(2, count))
    // 6 distinct openings used
    Assert.Equal(6, perOpening.Length)

[<Fact>]
let ``gauntletSingleRound randomOffset wraps with modulo when book is smaller than needed`` () =
    let challengers = [ mkEngine "Hero" ]
    let opponents = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    // Book has only 4 openings, but we want 5 rounds (needs 15 slots, wraps around)
    let openings = [ for i in 1..4 -> mkOpening i ]

    let games = gauntletSingleRound false true 5 challengers opponents openings

    // 1 challenger * 3 opponents * 5 rounds = 15 games
    Assert.Equal(15, games.Length)
    // All 15 games should have valid openings (no index-out-of-range)
    Assert.All(games, fun g -> Assert.NotNull(g.Opening))

[<Fact>]
let ``filterByPlayCount handles duplicate pairing keys from modulo wrapping`` () =
    let challengers = [ mkEngine "Hero" ]
    let opponents = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ for i in 1..4 -> mkOpening i ]

    let allPairings = gauntletSingleRound false true 5 challengers opponents openings
    Assert.Equal(15, allPairings.Length)

    // Simulate: first 3 games were played (round 0, all 3 opponents)
    let playedGames =
        allPairings
        |> List.take 3
        |> List.map (fun p ->
            { PgnGame.Empty p.GameNr with
                GameMetaData = { GameMetadata.Empty with
                                    OpeningHash = p.OpeningHash
                                    Fen = p.Opening.GameMetaData.Fen
                                    White = p.White.Name
                                    Black = p.Black.Name } })
        |> List.toArray

    let remaining = filterByPlayCount allPairings playedGames
    // Should skip exactly 3, play remaining 12
    Assert.Equal(12, remaining.Length)
    // First remaining game should be game #4 (round 1, opp A)
    Assert.Equal(4, remaining.Head.GameNr)
