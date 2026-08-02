module SchedulerTests

open Xunit
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.PGNTypes
open ChessLibrary.Scheduler

// ============================================================================
// Helpers shared by scheduler tests. Keep these tiny — each test should read
// at a glance.
// ============================================================================

let private mkEngine name =
    { EngineConfig.Empty with Name = name }

let private mkOpening gameNr =
    { PgnGame.Empty gameNr with Raw = $"opening-{gameNr}" }

/// Build a FEN-only opening (EPD-style): no book moves, just a starting
/// position. Mirrors what `EPDExtractor.mapEPDToPGN` produces.
let private mkEpdOpening gameNr (fen: string) : PgnGame =
    { PgnGame.Empty gameNr with
        GameMetaData = { GameMetadata.Empty with Fen = fen }
        Fen = fen
        Raw = $"epd-{gameNr}" }

/// Default ScheduleConfig for a 1-challenger gauntlet. Tests fluently override
/// whatever they need.
let private gauntletConfig
    (challengers: EngineConfig list)
    (opponents: EngineConfig list)
    (openings: PgnGame list)
    (rounds: int)
    : ScheduleConfig
    =
    { Mode = Gauntlet
      Challengers = challengers
      Opponents = opponents
      Openings = openings
      Rounds = rounds
      OpeningsTwice = true
      PreventDeviation = false
      Distribution = Spread }

/// Turn a PlannedGame into the equivalent PgnGame shape the runner would
/// write to disk. Used to simulate an already-played phase in resume tests.
let private playedPgnOf (p: PlannedGame) : PgnGame =
    { PgnGame.Empty 0 with
        GameMetaData =
            { GameMetadata.Empty with
                OpeningHash = p.OpeningHash
                Fen = p.Opening.GameMetaData.Fen
                White = p.White.Name
                Black = p.Black.Name } }

let private distinctOpenings (engineName: string) (games: PlannedGame list) : Set<string> =
    games
    |> List.filter (fun g -> g.White.Name = engineName || g.Black.Name = engineName)
    |> List.map (fun g -> g.Opening.Raw)
    |> Set.ofList

// ============================================================================
// Gauntlet.generate — size and invariants
// ============================================================================

[<Fact>]
let ``Spread, OpeningsTwice=true: total = rounds × numOpps × numChals × 2`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 6 -> mkOpening i ]
            3

    let games = Gauntlet.generate cfg

    Assert.Equal(12, games.Length)   // 1 × 2 × 3 × 2

[<Fact>]
let ``Spread, OpeningsTwice=false: total = rounds × numOpps × numChals`` () =
    let cfg =
        { gauntletConfig
              [ mkEngine "Hero" ]
              [ mkEngine "A"; mkEngine "B" ]
              [ for i in 1 .. 6 -> mkOpening i ]
              3
          with OpeningsTwice = false }

    let games = Gauntlet.generate cfg

    Assert.Equal(6, games.Length)

[<Fact>]
let ``Spread: each opponent plays rounds distinct openings`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 6 -> mkOpening i ]
            3

    let games = Gauntlet.generate cfg

    Assert.Equal(3, (distinctOpenings "A" games).Count)
    Assert.Equal(3, (distinctOpenings "B" games).Count)

[<Fact>]
let ``Spread: opponents' openings are disjoint`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 6 -> mkOpening i ]
            3

    let games = Gauntlet.generate cfg

    let aOps = distinctOpenings "A" games
    let bOps = distinctOpenings "B" games
    Assert.Empty(Set.intersect aOps bOps)

[<Fact>]
let ``Spread: all GameKeys in plan are unique (no wrap-duplicates)`` () =
    // Even when book is smaller than rounds × numOpps, the scheduler should
    // not produce duplicate keys within a single color direction. (With
    // OpeningsTwice, each direction still has unique keys; reversed direction
    // has its own complementary set.)
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 6 -> mkOpening i ]
            3

    let games = Gauntlet.generate cfg

    let keys = games |> List.map (fun g -> g.Key) |> Set.ofList
    Assert.Equal(games.Length, keys.Count)

[<Fact>]
let ``Shared: every opponent plays every opening`` () =
    let cfg =
        { gauntletConfig
              [ mkEngine "Hero" ]
              [ mkEngine "A"; mkEngine "B" ]
              [ for i in 1 .. 3 -> mkOpening i ]
              3
          with Distribution = Shared }

    let games = Gauntlet.generate cfg

    // 1 chal × 2 opps × 3 openings × 2 colors
    Assert.Equal(12, games.Length)
    Assert.Equal(3, (distinctOpenings "A" games).Count)
    Assert.Equal(3, (distinctOpenings "B" games).Count)
    // A and B share exactly the same opening set
    Assert.Equal<Set<string>>(distinctOpenings "A" games, distinctOpenings "B" games)

// ============================================================================
// PreventDeviation rotation
// ============================================================================

[<Fact>]
let ``PreventDeviation rotates game order per round but preserves opening-per-opponent`` () =
    let opps = [ mkEngine "A"; mkEngine "B" ]
    let openings = [ for i in 1 .. 4 -> mkOpening i ]
    let baseCfg = gauntletConfig [ mkEngine "Hero" ] opps openings 2

    let rotated = Gauntlet.generate { baseCfg with PreventDeviation = true }
    let static_ = Gauntlet.generate { baseCfg with PreventDeviation = false }

    // Same total count and same (opponent, opening) assignments — rotation
    // only changes the play order within a round, not which opening each
    // opponent plays.
    Assert.Equal(static_.Length, rotated.Length)
    Assert.Equal<Set<string>>(distinctOpenings "A" rotated, distinctOpenings "A" static_)
    Assert.Equal<Set<string>>(distinctOpenings "B" rotated, distinctOpenings "B" static_)

// ============================================================================
// Diff.diff — resume semantics
// ============================================================================

[<Fact>]
let ``Diff with empty played returns the whole plan unchanged`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A" ]
            [ for i in 1 .. 2 -> mkOpening i ]
            2

    let plan = Gauntlet.generate cfg
    let remaining = Diff.diff plan [||]

    Assert.Equal<PlannedGame list>(plan, remaining)

[<Fact>]
let ``Diff subtracts every played game by GameKey`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 4 -> mkOpening i ]
            2

    let plan = Gauntlet.generate cfg
    let half = plan |> List.take (plan.Length / 2)
    let played = half |> List.map playedPgnOf |> List.toArray

    let remaining = Diff.diff plan played

    Assert.Equal(plan.Length - half.Length, remaining.Length)

[<Fact>]
let ``Diff is idempotent: applying the remainder's played set yields the remainder`` () =
    let cfg =
        gauntletConfig
            [ mkEngine "Hero" ]
            [ mkEngine "A"; mkEngine "B" ]
            [ for i in 1 .. 4 -> mkOpening i ]
            2

    let plan = Gauntlet.generate cfg
    let someOld = plan |> List.take 3 |> List.map playedPgnOf |> List.toArray

    let once = Diff.diff plan someOld
    let twice = Diff.diff once [||]

    Assert.Equal<PlannedGame list>(once, twice)

// ============================================================================
// Resume scenarios (the real-world cases behind C3Test33 / C3Test35)
// ============================================================================

[<Fact>]
let ``Resume with added opponent: after enforceGameLimits, each opp is capped at its quota`` () =
    // Phase 1: 1 opp (A) alone. Phase 2: add B.
    // Under round-major opening assignment + quota enforcement, A stays at
    // its 4-game quota (from phase 1) and B plays its 4 new games in phase 2.
    let phase1Chal = [ mkEngine "Hero" ]
    let phase1Opps = [ mkEngine "A" ]
    let phase2Opps = [ mkEngine "A"; mkEngine "B" ]
    let bigBook = [ for i in 1 .. 4 -> mkOpening i ]

    let phase1Cfg = gauntletConfig phase1Chal phase1Opps (bigBook |> List.take 2) 2

    let phase1Plan = Gauntlet.generate phase1Cfg
    let playedAfterPhase1 = phase1Plan |> List.map playedPgnOf |> List.toArray

    let phase2Cfg =
        { phase1Cfg with Opponents = phase2Opps; Openings = bigBook }
    let phase2Plan = Gauntlet.generate phase2Cfg
    let remaining =
        Diff.diff phase2Plan playedAfterPhase1
        |> Diff.enforceGameLimits phase1Chal phase2Opps 2 true playedAfterPhase1

    // Each opponent plays exactly `rounds × factor = 4` games in total
    // across phase 1 + phase 2.
    let all = phase1Plan @ remaining
    let aGames = all |> List.filter (fun g -> g.White.Name = "A" || g.Black.Name = "A")
    let bGames = all |> List.filter (fun g -> g.White.Name = "B" || g.Black.Name = "B")
    Assert.Equal(4, aGames.Length)
    Assert.Equal(4, bGames.Length)

    // Each opponent plays exactly 2 distinct openings (rounds = 2)
    Assert.Equal(2, (distinctOpenings "A" all).Count)
    Assert.Equal(2, (distinctOpenings "B" all).Count)

    // Total games = rounds × numOpps × factor = 2 × 2 × 2 = 8
    Assert.Equal(8, all.Length)

// ============================================================================
// RoundRobin.generate — structural invariants
// ============================================================================

let private rrConfig (players: EngineConfig list) (openings: PgnGame list) : ScheduleConfig =
    { Mode = RoundRobin
      Challengers = players
      Opponents = []
      Openings = openings
      Rounds = openings.Length
      OpeningsTwice = false
      PreventDeviation = false
      Distribution = Shared }

[<Fact>]
let ``RR single: 4 players × 1 opening → 6 games, each pair appears exactly once`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    Assert.Equal(6, games.Length)   // C(4,2) = 6

    // Each unordered pair appears exactly once
    let pairs =
        games
        |> List.map (fun g ->
            if g.White.Name < g.Black.Name
            then g.White.Name, g.Black.Name
            else g.Black.Name, g.White.Name)
        |> List.countBy id
    for (_, count) in pairs do
        Assert.Equal(1, count)

[<Fact>]
let ``RR double: 4 players × 1 opening → 12 games, each ordered pair appears once`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1 ]

    let games =
        RoundRobin.generate { rrConfig players openings with OpeningsTwice = true }

    Assert.Equal(12, games.Length)   // 4 × 3

    let orderedPairs =
        games |> List.map (fun g -> g.White.Name, g.Black.Name) |> List.countBy id
    for (_, count) in orderedPairs do
        Assert.Equal(1, count)

[<Fact>]
let ``RR single: 3 players (odd) handles the bye, 3 games total per opening`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    Assert.Equal(3, games.Length)   // C(3,2) = 3
    // No game should involve the BYE sentinel
    for g in games do
        Assert.NotEqual<string>(EngineConfig.Empty.Name, g.White.Name)
        Assert.NotEqual<string>(EngineConfig.Empty.Name, g.Black.Name)

[<Fact>]
let ``RR: game count scales linearly with openings`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ for i in 1 .. 5 -> mkOpening i ]

    let single = RoundRobin.generate (rrConfig players openings)
    let double_ =
        RoundRobin.generate { rrConfig players openings with OpeningsTwice = true }

    Assert.Equal(6 * 5, single.Length)
    Assert.Equal(12 * 5, double_.Length)

[<Fact>]
let ``RR: all GameKeys are unique (no duplicates)`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ for i in 1 .. 3 -> mkOpening i ]

    let games =
        RoundRobin.generate { rrConfig players openings with OpeningsTwice = true }

    let keys = games |> List.map (fun g -> g.Key) |> Set.ofList
    Assert.Equal(games.Length, keys.Count)

// ---- RR color balance and player-coverage invariants ----------------------

[<Fact>]
let ``RR: 8 players colors balanced across first 4 rounds`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F"; mkEngine "G"; mkEngine "H" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    let gamesPerRound = players.Length / 2
    let firstFourRounds = games |> List.take (gamesPerRound * 4)

    let addCount name delta counts =
        counts |> Map.add name ((counts |> Map.tryFind name |> Option.defaultValue 0) + delta)
    let colorCounts =
        firstFourRounds
        |> List.fold
            (fun acc g ->
                acc |> addCount g.White.Name 1 |> addCount g.Black.Name -1)
            Map.empty

    for player in players do
        Assert.Equal(0, colorCounts.[player.Name])

[<Fact>]
let ``RR: 9 players (odd) colors balanced within one per player`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"; mkEngine "E"
          mkEngine "F"; mkEngine "G"; mkEngine "H"; mkEngine "I" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    let addCount name delta counts =
        counts |> Map.add name ((counts |> Map.tryFind name |> Option.defaultValue 0) + delta)
    let colorCounts =
        games
        |> List.fold
            (fun acc g ->
                acc |> addCount g.White.Name 1 |> addCount g.Black.Name -1)
            Map.empty

    for player in players do
        let diff = colorCounts.[player.Name]
        Assert.True(abs diff <= 1, $"Color imbalance too large for {player.Name}: {diff}")

[<Fact>]
let ``RR: every player plays the same number of games (N-1)`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    let gameCounts =
        games
        |> List.collect (fun g -> [ g.White.Name; g.Black.Name ])
        |> List.countBy id
        |> Map.ofList
    for player in players do
        Assert.Equal(players.Length - 1, gameCounts.[player.Name])

[<Fact>]
let ``RR: no player is paired against themselves`` () =
    let players =
        [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"
          mkEngine "E"; mkEngine "F"; mkEngine "G"; mkEngine "H" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    for g in games do
        Assert.NotEqual<string>(g.White.Name, g.Black.Name)

[<Fact>]
let ``RR: odd player count includes every player (bye sentinel not emitted)`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D"; mkEngine "E" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    // C(5,2) = 10 unique pairs
    Assert.Equal(10, games.Length)
    let allNames =
        games
        |> List.collect (fun g -> [ g.White.Name; g.Black.Name ])
        |> Set.ofList
    Assert.Equal(5, allNames.Count)
    Assert.DoesNotContain("", allNames)
    Assert.DoesNotContain(EngineConfig.Empty.Name, allNames)

[<Fact>]
let ``RR: game count scales linearly with openings (multi-opening)`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C"; mkEngine "D" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games = RoundRobin.generate (rrConfig players openings)

    // C(4,2) = 6 pairs per opening × 2 openings
    Assert.Equal(12, games.Length)
    let perOpening =
        games
        |> List.groupBy (fun g -> g.Opening.Raw)
        |> List.map (fun (_, gs) -> gs.Length)
    Assert.All(perOpening, fun count -> Assert.Equal(6, count))

[<Fact>]
let ``RR 3 engines, 1 opening, OpeningsTwice=true: all 6 games stay at opening 1`` () =
    // Regression test for "second game of pair 1 shows round=2.2": the full
    // set of single-round + reversed games for a given opening must not
    // spill into the next opening. 3 engines × OpeningsTwice → 6 games per
    // opening (3 pairs × 2 colors).
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games =
        RoundRobin.generate { rrConfig players openings with OpeningsTwice = true }

    Assert.Equal(6, games.Length)
    for g in games do
        Assert.Equal(1, g.Opening.GameNumber)

[<Fact>]
let ``RR 3 engines, 2 openings, OpeningsTwice=true: opening 1 games precede opening 2 in plan order`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1; mkOpening 2 ]

    let games =
        RoundRobin.generate { rrConfig players openings with OpeningsTwice = true }

    // 3 pairs × 2 colors × 2 openings = 12 games total
    Assert.Equal(12, games.Length)

    // First 6 games should all be at opening 1
    let firstSix = games |> List.take 6
    for g in firstSix do
        Assert.Equal(1, g.Opening.GameNumber)

    // Next 6 games should all be at opening 2
    let nextSix = games |> List.skip 6
    for g in nextSix do
        Assert.Equal(2, g.Opening.GameNumber)

[<Fact>]
let ``RR 3 engines: every unordered pair appears exactly once per opening in SingleRound`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkOpening 1 ]

    let games = RoundRobin.generate (rrConfig players openings)

    Assert.Equal(3, games.Length)   // C(3,2)

    let unordered =
        games
        |> List.map (fun g ->
            if g.White.Name < g.Black.Name
            then g.White.Name, g.Black.Name
            else g.Black.Name, g.White.Name)
        |> Set.ofList

    Assert.Equal(3, unordered.Count)
    Assert.Contains(("A", "B"), unordered)
    Assert.Contains(("A", "C"), unordered)
    Assert.Contains(("B", "C"), unordered)

// ============================================================================
// Pair-based round-label assignment
//
// The final RoundNr in PGN is `{pairIdx}.{colorIdx}`. `pairIdx` is a
// 1-based sequential counter across (engine_pair, opening) combinations in
// the plan; `colorIdx` is 1 for the first color emitted and 2 for its
// reverse (only under OpeningsTwice). On resume, pairIdx is seeded from
// priorPairs = (gamesAlreadyPlayed / gamesPerPair).
// ============================================================================

let private labelsFor (plan: PlannedGame list) = plan |> List.map (fun g -> g.RoundLabel)

[<Fact>]
let ``applyPairLabels: OpeningsTwice=true, fresh start labels pairs 1..N`` () =
    let plan =
        RoundRobin.generate { rrConfig [ mkEngine "A"; mkEngine "B"; mkEngine "C" ] [ mkOpening 1 ] with OpeningsTwice = true }
        |> Diff.applyPairLabels 0 2

    // 3 pairs × 2 colors → labels "1.1", "1.2", "2.1", "2.2", "3.1", "3.2"
    Assert.Equal<string list>(
        [ "1.1"; "1.2"; "2.1"; "2.2"; "3.1"; "3.2" ],
        labelsFor plan)

[<Fact>]
let ``applyPairLabels: OpeningsTwice=false emits one game per pair (colorIdx always 1)`` () =
    let plan =
        RoundRobin.generate (rrConfig [ mkEngine "A"; mkEngine "B"; mkEngine "C" ] [ mkOpening 1 ])
        |> Diff.applyPairLabels 0 1

    Assert.Equal<string list>([ "1.1"; "2.1"; "3.1" ], labelsFor plan)

[<Fact>]
let ``applyPairLabels: resume picks up where phase 1 left off (prior games offset)`` () =
    // Phase 1 played 2 complete pairs — 4 games. Phase 2 plan should label
    // its first pair as "3.1"/"3.2", not "1.1"/"1.2".
    let plan =
        RoundRobin.generate { rrConfig [ mkEngine "A"; mkEngine "B"; mkEngine "C" ] [ mkOpening 1 ] with OpeningsTwice = true }
        |> Diff.applyPairLabels 4 2

    Assert.Equal<string list>(
        [ "3.1"; "3.2"; "4.1"; "4.2"; "5.1"; "5.2" ],
        labelsFor plan)

[<Fact>]
let ``applyPairLabels: resume mid-pair labels the remaining game as the second colour`` () =
    // 5 games played is two complete pairs plus the white half of pair 3, so the
    // next game is "3.2" — not "3.1". Offsetting by pairs truncated that half
    // away and left every later label one game behind for the rest of the run.
    let plan =
        RoundRobin.generate { rrConfig [ mkEngine "A"; mkEngine "B"; mkEngine "C" ] [ mkOpening 1 ] with OpeningsTwice = true }
        |> Diff.applyPairLabels 5 2

    Assert.Equal<string list>(
        [ "3.2"; "4.1"; "4.2"; "5.1"; "5.2"; "6.1" ],
        labelsFor plan)

[<Fact>]
let ``Gauntlet labels: first game is always "1.1" regardless of opening shuffle`` () =
    // Even if openings arrive in shuffled order with arbitrary book numbers,
    // the first game is pair 1 / color 1 → "1.1".
    let chal = [ mkEngine "Hero" ]
    let opps = [ mkEngine "A" ]
    let shuffled = [ mkOpening 6; mkOpening 2 ]   // book positions 6 and 2
    let cfg = gauntletConfig chal opps shuffled 2

    let plan =
        Gauntlet.generate cfg
        |> Diff.applyPairLabels 0 2

    Assert.Equal("1.1", plan.Head.RoundLabel)
    Assert.Equal("1.2", plan.[1].RoundLabel)

[<Fact>]
let ``Gauntlet labels: 1 challenger, 2 opponents, 2 rounds, Spread, fresh run`` () =
    let chal = [ mkEngine "Hero" ]
    let opps = [ mkEngine "A"; mkEngine "B" ]
    let openings = [ for i in 1 .. 4 -> mkOpening i ]
    let cfg = { gauntletConfig chal opps openings 2 with Distribution = Spread }

    let plan =
        Gauntlet.generate cfg
        |> Diff.applyPairLabels 0 2

    // 4 pairs (2 opps × 2 rounds) × 2 colors = 8 games
    Assert.Equal<string list>(
        [ "1.1"; "1.2"; "2.1"; "2.2"; "3.1"; "3.2"; "4.1"; "4.2" ],
        labelsFor plan)

[<Fact>]
let ``Resume: Diff.diff + applyPairLabels continues numbering without collision`` () =
    // Phase 1: Hero vs A at openings 1, 2 — 4 games labeled "1.1", "1.2",
    // "2.1", "2.2". Phase 2 adds B. Under round-major slot + enforceGameLimits,
    // remaining games are B's 4 pair-slots. They should be labeled "3.1"
    // through "4.2" — continuing from phase 1's last pair (pair 2).
    let chal = [ mkEngine "Hero" ]
    let phase1Opps = [ mkEngine "A" ]
    let phase2Opps = [ mkEngine "A"; mkEngine "B" ]
    let bigBook = [ for i in 1 .. 4 -> mkOpening i ]

    let phase1Cfg = gauntletConfig chal phase1Opps (bigBook |> List.take 2) 2
    let phase1Plan = Gauntlet.generate phase1Cfg |> Diff.applyPairLabels 0 2
    Assert.Equal<string list>([ "1.1"; "1.2"; "2.1"; "2.2" ], labelsFor phase1Plan)

    let played = phase1Plan |> List.map playedPgnOf |> List.toArray

    let phase2Cfg = { phase1Cfg with Opponents = phase2Opps; Openings = bigBook }
    let phase2Plan = Gauntlet.generate phase2Cfg

    let priorGames = played.Length
    let remaining =
        Diff.diff phase2Plan played
        |> Diff.enforceGameLimits chal phase2Opps 2 true played
        |> Diff.applyPairLabels priorGames 2

    Assert.Equal<string list>(
        [ "3.1"; "3.2"; "4.1"; "4.2" ],
        labelsFor remaining)

[<Fact>]
let ``Spread resume with quota enforcement: phase 1 games are NOT double-counted`` () =
    // Round-major opening assignment reshuffles which opponent plays each
    // opening when a new opponent is added, which orphans some phase-1
    // games (they aren't in the phase 2 plan for the original opponent).
    // `enforceGameLimits` must compensate by capping each opp at its quota.
    let chal = [ mkEngine "Hero" ]
    let phase1Opps = [ mkEngine "A" ]
    let phase2Opps = [ mkEngine "A"; mkEngine "B" ]
    let bigBook = [ for i in 1 .. 4 -> mkOpening i ]

    let phase1Cfg = gauntletConfig chal phase1Opps (bigBook |> List.take 2) 2
    let phase2Cfg = { phase1Cfg with Opponents = phase2Opps; Openings = bigBook }

    let phase1Plan = Gauntlet.generate phase1Cfg
    let playedAfterPhase1 = phase1Plan |> List.map playedPgnOf |> List.toArray
    let phase2Plan = Gauntlet.generate phase2Cfg
    let remaining =
        Diff.diff phase2Plan playedAfterPhase1
        |> Diff.enforceGameLimits chal phase2Opps 2 true playedAfterPhase1

    // A has already hit its quota in phase 1, so no A games remain.
    let remainingAGames =
        remaining |> List.filter (fun g -> g.White.Name = "A" || g.Black.Name = "A")
    Assert.Empty(remainingAGames)

    // B is new, so all 4 of its games should be queued.
    let remainingBGames =
        remaining |> List.filter (fun g -> g.White.Name = "B" || g.Black.Name = "B")
    Assert.Equal(4, remainingBGames.Length)

// ============================================================================
// EPD books (FEN-only openings)
//
// When the opening source is an `.epd` file, each line becomes a PgnGame
// with `Fen` and `GameMetaData.Fen` populated but `Mainline` empty. The
// scheduler must produce stable opening hashes, valid pair-based labels,
// and correct resume behavior just like with PGN-source openings.
// ============================================================================

let private fen1 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
let private fen2 = "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq - 0 1"
let private fen3 = "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1"
let private fen4 = "rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq - 0 1"

[<Fact>]
let ``EPD openings: each FEN produces a distinct, stable opening hash`` () =
    let op1 = mkEpdOpening 1 fen1
    let op2 = mkEpdOpening 2 fen2
    let op1b = mkEpdOpening 99 fen1   // same FEN, different GameNumber

    let hash1 = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame op1
    let hash2 = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame op2
    let hash1b = ChessLibrary.ChessUtilities.Hash.computeOpeningHashFromGame op1b

    // Different FENs → different hashes
    Assert.NotEqual<string>(hash1, hash2)
    // Same FEN → same hash, regardless of GameNumber
    Assert.Equal(hash1, hash1b)

[<Fact>]
let ``EPD openings: Gauntlet scheduler produces correct pair-based labels`` () =
    let chal = [ mkEngine "Hero" ]
    let opps = [ mkEngine "A"; mkEngine "B" ]
    let openings =
        [ mkEpdOpening 1 fen1
          mkEpdOpening 2 fen2
          mkEpdOpening 3 fen3
          mkEpdOpening 4 fen4 ]
    let cfg = gauntletConfig chal opps openings 2

    let plan =
        Gauntlet.generate cfg
        |> Diff.applyPairLabels 0 2

    // 4 pairs × 2 colors = 8 games, pair-consecutive.
    Assert.Equal<string list>(
        [ "1.1"; "1.2"; "2.1"; "2.2"; "3.1"; "3.2"; "4.1"; "4.2" ],
        plan |> List.map (fun g -> g.RoundLabel))

[<Fact>]
let ``EPD openings: RoundRobin scheduler produces correct pair-based labels`` () =
    let players = [ mkEngine "A"; mkEngine "B"; mkEngine "C" ]
    let openings = [ mkEpdOpening 1 fen1; mkEpdOpening 2 fen2 ]
    let cfg =
        { rrConfig players openings with OpeningsTwice = true }

    let plan =
        RoundRobin.generate cfg
        |> Diff.applyPairLabels 0 2

    // 3 pairs × 2 openings × 2 colors = 12 games
    Assert.Equal(12, plan.Length)
    Assert.Equal("1.1", plan.[0].RoundLabel)
    Assert.Equal("6.2", plan.[11].RoundLabel)

[<Fact>]
let ``EPD openings: single-pairing gauntlet (1 chal + 1 opp) produces clean labels`` () =
    // Minimal case: one challenger, one opponent, one EPD opening, one
    // round with OpeningsTwice. Exactly one pair plays, both colors.
    let chal = [ mkEngine "Hero" ]
    let opps = [ mkEngine "A" ]
    let openings = [ mkEpdOpening 1 fen1 ]
    let cfg =
        { gauntletConfig chal opps openings 1 with OpeningsTwice = true }

    let plan =
        Gauntlet.generate cfg
        |> Diff.applyPairLabels 0 2

    Assert.Equal(2, plan.Length)
    Assert.Equal("1.1", plan.[0].RoundLabel)
    Assert.Equal("1.2", plan.[1].RoundLabel)

    // The pair's two games must share the EPD's FEN as the starting
    // position, and the opening hash must match on both (same opening).
    Assert.Equal(fen1, plan.[0].Opening.GameMetaData.Fen)
    Assert.Equal(fen1, plan.[1].Opening.GameMetaData.Fen)
    Assert.Equal(plan.[0].OpeningHash, plan.[1].OpeningHash)

[<Fact>]
let ``EPD openings: resume via Diff.diff identifies played games by FEN-derived hash`` () =
    // Simulate an EPD-based gauntlet where some games are already in PGN.
    // Diff.diff must recognize them by the same opening hash the scheduler
    // assigns (otherwise we'd replay or mislabel).
    let chal = [ mkEngine "Hero" ]
    let opps = [ mkEngine "A" ]
    let openings = [ mkEpdOpening 1 fen1; mkEpdOpening 2 fen2 ]
    let cfg = gauntletConfig chal opps openings 2

    let plan =
        Gauntlet.generate cfg
        |> Diff.applyPairLabels 0 2

    // Simulate pair 1 (2 games) played.
    let played = plan |> List.take 2 |> List.map playedPgnOf |> List.toArray
    let remaining =
        Diff.diff plan played
        |> Diff.applyPairLabels played.Length 2

    Assert.Equal(2, remaining.Length)
    Assert.Equal("2.1", remaining.[0].RoundLabel)
    Assert.Equal("2.2", remaining.[1].RoundLabel)
