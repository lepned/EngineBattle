module PairingTests

open Xunit
open ChessLibrary.TypesDef
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.PGNTypes
open ChessLibrary.Utilities.PairingHelper
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

    let games = gauntletSingleRound false challengers opponents openings

    Assert.Equal(2, games.Length)
    Assert.All(games, fun g -> Assert.Equal("A", g.White.Name))
    Assert.Equal<string list>([ "1.1"; "1.2" ], games |> List.map (fun g -> g.RoundNr))
    Assert.Equal<int list>([ 1; 2 ], games |> List.map (fun g -> g.GameNr))

[<Fact>]
let ``gauntletDoubleRound includes color-reversed rematches`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = gauntletDoubleRound false challengers opponents openings

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
    let hash1 = Utilities.Hash.computeOpeningHashFromGame game
    let hash2 = Utilities.Hash.computeOpeningHashFromGame game
    Assert.Equal(hash1, hash2)

[<Fact>]
let ``opening hash changes when fen or moves change`` () =
    let moves =
        [ mkPlyMove 0 1 "w" "e4" ""
          mkPlyMove 1 1 "b" "e5" "" ]
    let game = mkGameWithMoves 1 "startpos" moves
    let baseHash = Utilities.Hash.computeOpeningHashFromGame game

    let gameDiffFen = { game with GameMetaData = { game.GameMetaData with Fen = "different" } }
    let diffFenHash = Utilities.Hash.computeOpeningHashFromGame gameDiffFen
    Assert.NotEqual<string>(baseHash, diffFenHash)

    let movesDiff =
        [ mkPlyMove 0 1 "w" "d4" ""
          mkPlyMove 1 1 "b" "d5" "" ]
    let gameDiffMoves = mkGameWithMoves 1 "startpos" movesDiff
    let diffMovesHash = Utilities.Hash.computeOpeningHashFromGame gameDiffMoves
    Assert.NotEqual<string>(baseHash, diffMovesHash)

[<Fact>]
let ``opening hash falls back to game number when empty`` () =
    let hash1 = Utilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 1)
    let hash2 = Utilities.Hash.computeOpeningHashFromGame (PgnGame.Empty 2)
    Assert.NotEqual<string>(hash1, hash2)

[<Fact>]
let ``opening hash uses fen when present (epd style)`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let game = mkGameWithMoves 1 fen []
    let hash = Utilities.Hash.computeOpeningHashFromGame game
    let expected =
        let nl = System.Environment.NewLine
        Utilities.Hash.computeOpeningHash (sprintf "[Fen \"%s\"]%s%s" fen nl nl)
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

    let games = gauntletSingleRound false challengers opponents openings

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

    let games = gauntletSingleRound false challengers opponents openings

    // 1 challenger * 2 opponents * 2 openings = 4 games
    Assert.Equal(4, games.Length)
    let openingRaws = games |> List.map (fun g -> g.Opening.Raw) |> List.distinct
    Assert.Equal(2, openingRaws.Length)

[<Fact>]
let ``gauntletSingleRound with doNotDeviate rotates opponents per opening`` () =
    let challengers = [ mkEngine "A" ]
    let opponents = [ mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let gamesRotated = gauntletSingleRound true challengers opponents openings
    let gamesStatic = gauntletSingleRound false challengers opponents openings

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

    let games = gauntletDoubleRound false challengers opponents openings

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

    let games = gauntletSingleRound false challengers opponents openings

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
