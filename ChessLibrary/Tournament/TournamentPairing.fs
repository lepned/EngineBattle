module ChessLibrary.TournamentPairing

open System
open System.Text
open System.Collections.Generic
open Microsoft.Extensions.Logging

open PGNTypes
open TypesDef.CoreTypes
open TypesDef.Tournament
open ChessUtilities

/// Thin facade over `ChessLibrary.Scheduler.*` for callers that still work
/// with the legacy `Pairing` record shape: the Swiss and Cup runners, the
/// Tournament object (which tracks played games by `Pairing` key), a couple
/// of CLI/replay helpers, and the Scheduler's own tests use the modules
/// directly.
///
/// All actual pairing algorithms now live in `Scheduler/`. Anything here is
/// either (a) a re-export of a Scheduler function, (b) a small `Pairing`-
/// centric helper that hasn't yet earned its own module (played-set tracking,
/// schedule dumps), or (c) a thin adapter to a Scheduler function.
module PairingHelper =

    // ---- Shuffle / rotation (Scheduler.Shared) ----------------------------
    let tournamentSalt = Scheduler.Shared.tournamentSalt
    let shuffleOpenings = Scheduler.Shared.shuffleOpenings
    let shuffleOpeningsWithSeed = Scheduler.Shared.shuffleOpeningsWithSeed
    let shuffleOpeningsForTournament = Scheduler.Shared.shuffleOpeningsForTournament
    let rotateListByOne = Scheduler.Shared.rotateListByOne
    let rotateOnce = Scheduler.Shared.rotateOnce

    // ---- Cup (Scheduler.Cup) ----------------------------------------------
    type CupSeedingStrategy = Scheduler.Cup.SeedingStrategy
    let tcecSeedOrder = Scheduler.Cup.tcecSeedOrder
    let seedOrder = Scheduler.Cup.seedOrder
    let autoSeedBands = Scheduler.Cup.autoSeedBands
    let seedByBands = Scheduler.Cup.seedByBands
    let gamesPerMatchForRound = Scheduler.Cup.gamesPerMatchForRound
    let nextUnusedOpeningIndex = Scheduler.Cup.nextUnusedOpeningIndex
    let buildRemainingCupPairings = Scheduler.Cup.buildRemainingCupPairings

    // ---- Swiss (Scheduler.Swiss) ------------------------------------------
    let swissPairKey = Scheduler.Swiss.pairKey
    let swissRoundPairings = Scheduler.Swiss.pairNextRound
    let swissRoundPairingsGroupedOnly = Scheduler.Swiss.pairNextRoundGroupedOnly

    /// Swiss match-game planner in mutating form (appends to `planned` and
    /// returns the next opening index). Used by the Swiss runner as it walks
    /// through score-group pairings round by round. Alternates White-first
    /// with Black-first across each opening pick.
    ///
    /// Each preview game is labelled and chosen exactly as the runner will label and choose
    /// it when it is played: RoundNr is "<round>.<position>", the position counted across
    /// every game of the round whether played yet or not; GameNr continues the running game
    /// counter; and on a resume a pair with one game already played gets the SAME opening
    /// for its second game, colours reversed, rather than whatever the index points at. The
    /// console schedule line and the GUI pairings table show these, and they used to carry
    /// the opening's book number instead of the round - a round-one game read as "Round
    /// 43.3, game 0" while the PGN said "1.3".
    ///
    /// `startIndex` is the opening index of the first pair NOT yet started - what the runner
    /// will pick next - so pairs already played consume nothing here.
    let addPlannedPairings
        (planned: ResizeArray<Pairing>)
        (whiteFirst: EngineConfig)
        (blackFirst: EngineConfig)
        (openings: PgnGame list)
        (gamesPerMatch: int)
        (startIndex: int)
        (roundNumber: int)
        (positionBase: int)             // games of the earlier pairings in this round: pairIndex * gamesPerMatch
        (alreadyPlayed: int)            // games of THIS pairing already in the PGN, on a resume - not previewed
        (nextGameNr: int)               // the running counter as it stands when the preview is built
        (halfPairOpening: PgnGame option)  // the opening of the last played game, when alreadyPlayed is odd
        =
        if openings.IsEmpty then
            startIndex
        else
            let gamesPerPair = max 1 (gamesPerMatch / 2)
            let mutable index = startIndex
            let add (opening: PgnGame) (white: EngineConfig) (black: EngineConfig) (position: int) =
                planned.Add
                    { Opening = opening
                      White = white
                      Black = black
                      GameNr = nextGameNr + planned.Count + 1
                      RoundNr = sprintf "%d.%d" roundNumber (positionBase + position + 1)
                      OpeningHash = Hash.computeOpeningHashFromGame opening }
            let playedPairs = alreadyPlayed / 2
            let halfPair = alreadyPlayed % 2 = 1
            if halfPair then
                // The runner replays the last game's opening with colours reversed and does not
                // touch the opening index for it.
                let opening = halfPairOpening |> Option.defaultValue openings.[index % openings.Length]
                add opening blackFirst whiteFirst alreadyPlayed
            for pair in playedPairs + (if halfPair then 1 else 0) .. gamesPerPair - 1 do
                let opening = openings.[index % openings.Length]
                add opening whiteFirst blackFirst (pair * 2)
                add opening blackFirst whiteFirst (pair * 2 + 1)
                index <- index + 1
            index

    /// Legacy RR helper used by one offline PGN-replay regression test.
    /// Forwards to `Scheduler.RoundRobin.generate` and adapts the output
    /// to `Pairing list`. No other callers.
    let generateAllRoundRobinDoubleRounds (players: EngineConfig list) (openings: PgnGame list) : Pairing list =
        let cfg : Scheduler.ScheduleConfig =
            { Mode = Scheduler.RoundRobin
              Challengers = players
              Opponents = []
              Openings = openings
              Rounds = openings.Length
              OpeningsTwice = true
              PreventDeviation = false
              Distribution = Scheduler.Shared }
        Scheduler.RoundRobin.generate cfg |> Scheduler.Diff.toPairings

    // ---- Schedule dumps for the console and log ----------------------------

    /// How many pairings a startup dump prints. The UI still receives every pairing — capping
    /// the dump is not capping the schedule — but a few thousand lines in the console or the
    /// log file buries everything else, and nobody reads past the first screen.
    [<Literal>]
    let MaxPairingsLogged = 50

    /// GameNr, not a position in the list: the list is often the games *left* to play, where a
    /// position restarts at 1 while GameNr keeps counting. GameNr is also what the "G%d" line
    /// prints when the game starts, so a schedule line can be matched to a game.
    let private pairingLine (p: Pairing) =
        let openingName = PGNHelper.getOpeningInfo p.Opening
        let opName =
            if openingName.Contains "No opening name" && not (String.IsNullOrEmpty p.Opening.Fen)
            then p.Opening.Fen
            else openingName
        sprintf "Round %s, game %d: %s, %s vs %s" p.RoundNr p.GameNr opName p.White.Name p.Black.Name

    let private dump (pairings: Pairing list) =
        let sb = StringBuilder()
        pairings
        |> List.truncate MaxPairingsLogged
        |> List.iter (fun p -> sb.AppendLine(pairingLine p) |> ignore)
        let total = List.length pairings
        if total > MaxPairingsLogged then
            sb.AppendLine(sprintf "... and %d more (%d pairings in total)" (total - MaxPairingsLogged) total)
            |> ignore
        sb.ToString()

    let logOpeningPairs (logger: ILogger) (pairings: Pairing list) =
        // Silent when nothing is left to play, and no leading blank line — either one made
        // the entry read as empty.
        if not (List.isEmpty pairings) then logger.LogInformation(dump pairings)

    /// The same text for the console, with a blank line above it to separate it from the
    /// startup banner.
    let getOpeningPairs (pairings: Pairing list) : string =
        if List.isEmpty pairings then "" else "\n" + dump pairings
