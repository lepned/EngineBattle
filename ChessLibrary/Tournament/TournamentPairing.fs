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
    let addPlannedPairings
        (planned: ResizeArray<Pairing>)
        (whiteFirst: EngineConfig)
        (blackFirst: EngineConfig)
        (openings: PgnGame list)
        (gamesPerMatch: int)
        (startIndex: int)
        =
        if openings.IsEmpty then
            startIndex
        else
            let gamesPerPair = max 1 (gamesPerMatch / 2)
            let mutable index = startIndex
            for _ in 0 .. gamesPerPair - 1 do
                let opening = openings.[index % openings.Length]
                let openingHash = Hash.computeOpeningHashFromGame opening
                planned.Add
                    { Opening = opening
                      White = whiteFirst
                      Black = blackFirst
                      GameNr = 0
                      RoundNr = sprintf "%d.%d" opening.GameNumber (planned.Count + 1)
                      OpeningHash = openingHash }
                planned.Add
                    { Opening = opening
                      White = blackFirst
                      Black = whiteFirst
                      GameNr = 0
                      RoundNr = sprintf "%d.%d" opening.GameNumber (planned.Count + 1)
                      OpeningHash = openingHash }
                index <- index + 1
            index

    // ---- Pairing key / played-set (legacy `Pairing`-keyed tracking) -------
    // Used by Tournament.GetGamesLeftToPlay and one legacy PGN-replay test.
    // The Scheduler's `Diff.diff` uses the same semantics but keyed on
    // `GameKey` instead; these remain for callers that haven't migrated.

    let pairingKey (openingHash: string) (fen: string) (white: string) (black: string) =
        sprintf "%s|%s|%s|%s" openingHash fen (white.Trim()) (black.Trim())

    let playedSet (gamesAlreadyPlayed: PgnGame array) : Set<string> =
        gamesAlreadyPlayed
        |> Array.map (fun g ->
            pairingKey
                (if String.IsNullOrEmpty g.GameMetaData.OpeningHash
                 then g.GameNumber.ToString()
                 else g.GameMetaData.OpeningHash)
                g.GameMetaData.Fen
                g.GameMetaData.White
                g.GameMetaData.Black)
        |> Set.ofArray

    let hasPlayedBefore (pairing: Pairing) (playedSet: Set<string>) =
        let key =
            pairingKey
                pairing.OpeningHash
                pairing.Opening.GameMetaData.Fen
                pairing.White.Name
                pairing.Black.Name
        playedSet.Contains key

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
