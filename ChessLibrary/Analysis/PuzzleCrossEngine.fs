module ChessLibrary.PuzzleCrossEngine

open System
open System.IO
open ChessLibrary.PuzzleTypes
open ChessLibrary.Chess

type CrossEngineGroup = {
    Type: string
    /// The rating group this slice belongs to, bucketed exactly as PuzzleThemes
    /// buckets it. Leaving it out of the key merged every rating group into one
    /// group: an engine then appeared once per group in `Engines`, its solved and
    /// failed sets were overwritten by whichever group came last, and every
    /// section was written once per duplicate.
    ///
    /// The missing field was only ONE of the two ways an engine ends up in a group
    /// twice - see the distinctBy below for the other, which this key does not fix.
    RatingGroup: int
    Nodes: int
    Filter: string
}

type CrossEngineResult = {
    Group: CrossEngineGroup
    Engines: string list
    /// Full Score record per engine name, for header info (nn, rating)
    ScoresByEngine: Map<string, Score>
    /// Puzzles only engine X solved (all others failed) — keyed by engine name
    UniquelySolved: Map<string, CsvPuzzleData list>
    /// Puzzles only engine X failed (all others solved) — keyed by engine name
    UniquelyFailed: Map<string, (CsvPuzzleData * string) list>
    /// Puzzles every engine failed — with per-engine policy string
    FailedByAll: (CsvPuzzleData * Map<string, string>) list
    /// Puzzles every engine solved
    SolvedByAll: CsvPuzzleData list
}

/// Analyze cross-engine puzzle results within each (Type, RatingGroup, Nodes, Filter) group.
let analyzeCrossEngine (scores: Score seq) : CrossEngineResult list =
    scores
    |> Seq.groupBy (fun s ->
        { Type = s.Type
          RatingGroup = PuzzlePaired.ratingGroupOf s.RatingAvg
          Nodes = s.Nodes
          Filter = s.Filter })
    |> Seq.choose (fun (group, groupScores) ->
        // One Score per engine. Everything below is Map.ofList keyed on the engine
        // name, so a second row for the same engine silently overwrote the first
        // set - and `List.filter (fun e -> e <> eng)` then dropped BOTH copies,
        // leaving othersAll empty and every uniquely-solved/failed file empty. One
        // engine produces two rows in a slice whenever two tests share a Type label
        // (see PuzzlePaired.netKeyOf); the rating-group key fixed only the other
        // way this happened.
        let engines = groupScores |> Seq.distinctBy (fun e -> e.Engine) |> Seq.toList
        if engines.Length < 2 then None
        else
            let engineNames = engines |> List.map (fun e -> e.Engine)

            // Build solved/failed sets per engine (keyed by PuzzleId)
            let solvedSets =
                engines |> List.map (fun e ->
                    e.Engine,
                    e.CorrectPuzzles |> Seq.map (fun p -> p.PuzzleId) |> Set.ofSeq)
                |> Map.ofList

            let failedSets =
                engines |> List.map (fun e ->
                    e.Engine,
                    e.FailedPuzzles |> Seq.map (fun (p, _) -> p.PuzzleId) |> Set.ofSeq)
                |> Map.ofList

            // Puzzle lookup for solved puzzles (any engine's data works since MovePlayed isn't needed)
            let puzzleLookup =
                engines
                |> Seq.collect (fun e ->
                    Seq.append
                        (e.CorrectPuzzles |> Seq.map (fun p -> p.PuzzleId, p))
                        (e.FailedPuzzles |> Seq.map (fun (p, _) -> p.PuzzleId, p)))
                |> Seq.distinctBy fst
                |> Map.ofSeq

            // Per-engine failed puzzle lookup: uses each engine's own data (with MovePlayed set)
            let failedPuzzleLookup =
                engines |> List.map (fun e ->
                    e.Engine,
                    e.FailedPuzzles |> Seq.map (fun (p, m) -> p.PuzzleId, (p, m)) |> Map.ofSeq)
                |> Map.ofList

            // Uniquely solved: only engine X solved, all others failed
            let uniquelySolved =
                engineNames |> List.map (fun eng ->
                    let mySolved = solvedSets.[eng]
                    let othersAll =
                        engineNames
                        |> List.filter (fun e -> e <> eng)
                        |> List.map (fun e -> failedSets.[e])
                    // Puzzle must be in my solved AND in every other engine's failed set
                    let unique =
                        match othersAll with
                        | [] -> Set.empty
                        | sets -> sets |> List.fold Set.intersect mySolved
                    eng, unique |> Set.toList |> List.choose (fun id -> Map.tryFind id puzzleLookup))
                |> Map.ofList

            // Uniquely failed: only engine X failed, all others solved
            let uniquelyFailed =
                engineNames |> List.map (fun eng ->
                    let myFailed = failedSets.[eng]
                    let othersAll =
                        engineNames
                        |> List.filter (fun e -> e <> eng)
                        |> List.map (fun e -> solvedSets.[e])
                    let unique =
                        match othersAll with
                        | [] -> Set.empty
                        | sets -> sets |> List.fold Set.intersect myFailed
                    let puzzlesWithMoves =
                        unique |> Set.toList |> List.choose (fun id ->
                            Map.tryFind id failedPuzzleLookup.[eng])
                    eng, puzzlesWithMoves)
                |> Map.ofList

            // Failed by all: every engine failed this puzzle
            let allFailedIds =
                engineNames
                |> List.map (fun e -> failedSets.[e])
                |> function
                   | [] -> Set.empty
                   | first :: rest -> rest |> List.fold Set.intersect first
            let failedByAll =
                allFailedIds |> Set.toList |> List.choose (fun id ->
                    // Use any engine's failed puzzle data (all engines failed, so all have MovePlayed set)
                    let puzzleData =
                        engineNames |> List.tryPick (fun eng ->
                            failedPuzzleLookup.[eng] |> Map.tryFind id |> Option.map fst)
                    match puzzleData with
                    | Some p ->
                        let moves =
                            engineNames |> List.choose (fun eng ->
                                failedPuzzleLookup.[eng] |> Map.tryFind id |> Option.map (fun (_, m) -> eng, m))
                            |> Map.ofList
                        Some (p, moves)
                    | None -> None)

            // Solved by all: every engine solved this puzzle
            let allSolvedIds =
                engineNames
                |> List.map (fun e -> solvedSets.[e])
                |> function
                   | [] -> Set.empty
                   | first :: rest -> rest |> List.fold Set.intersect first
            let solvedByAll =
                allSolvedIds |> Set.toList |> List.choose (fun id -> Map.tryFind id puzzleLookup)

            let scoresByEngine =
                engines |> List.map (fun e -> e.Engine, e) |> Map.ofList

            Some {
                Group = group
                Engines = engineNames
                ScoresByEngine = scoresByEngine
                UniquelySolved = uniquelySolved
                UniquelyFailed = uniquelyFailed
                FailedByAll = failedByAll
                SolvedByAll = solvedByAll
            })
    |> Seq.toList

/// Get full EPD info for a failed puzzle command — matches the main failedLichessPuzzles format.
/// Returns (fen, bmSan, amSan, bmPolicy, amPolicy, uciCorrectMove, uciMovePlayed).
let private getFailedPuzzleEpd (puzzle: CsvPuzzleData) (policyStr: string) =
    let boardBm = Board()
    let boardAm = Board()
    puzzle.Commands |> Seq.tryPick (fun cmd ->
        if not (String.IsNullOrWhiteSpace(cmd.MovePlayed)) && cmd.MovePlayed.Length >= 4 then
            boardBm.PlayCommands(cmd.Command)
            let fen = boardBm.FEN()
            let bm = PuzzleDataUtils.sanOfMovePlayed boardBm cmd.Command cmd.CorrectMove
            let am = PuzzleDataUtils.sanOfMovePlayed boardAm cmd.Command cmd.MovePlayed
            let policies = policyStr.Split(',')
            let bmP, amP =
                if policies.Length > 1 then policies.[0].Trim(), policies.[1].Trim()
                else "", ""
            Some (fen, bm, am, bmP, amP, cmd.CorrectMove, cmd.MovePlayed)
        else
            None)

/// Get FEN and best-move SAN for a solved puzzle command.
/// Returns (fen, bmSan, uciCorrectMove). Uses the last command (decisive move).
let private getSolvedPuzzleEpd (puzzle: CsvPuzzleData) =
    let board = Board()
    let mutable result : (string * string * string) option = None
    for cmd in puzzle.Commands do
        if not (String.IsNullOrWhiteSpace(cmd.CorrectMove)) && cmd.CorrectMove.Length >= 4 then
            board.PlayCommands(cmd.Command)
            let fen = board.FEN()
            board.PlayUciMove(cmd.CorrectMove)
            let bm = board.SanMovesPlayed |> Seq.tryLast |> Option.defaultValue ""
            result <- Some (fen, bm, cmd.CorrectMove)
    result

/// Write cross-engine analysis files. Only creates files that have content. Does nothing for single-engine runs.
let writeCrossEngineFiles (outputFolder: string) (dateStr: string) (allScores: Score seq) =
    let results = analyzeCrossEngine allScores
    if results.IsEmpty then ()
    else

    let mutable totalUniqSolved = 0
    let mutable totalUniqFailed = 0
    let mutable totalFailedAll  = 0
    let mutable totalSolvedAll  = 0

    // Collect content into StringWriters first, only write files that have content
    use swUS = new StringWriter()
    use swUF = new StringWriter()
    use swFA = new StringWriter()
    use swSA = new StringWriter()

    // Helper: collect EPD lines, then write header + lines only if any lines exist
    let writeSection (sw: StringWriter) (header: string) (lines: string list) =
        if not lines.IsEmpty then
            sw.WriteLine(header)
            for line in lines do sw.WriteLine(line)
            sw.WriteLine()
        lines.Length

    for r in results do
        // Header follows the main file and adds the rating group: without it two slices
        // of the same engine produce byte-identical headers and cannot be told apart.
        let failedHeader (eng: string) =
            let score = r.ScoresByEngine.[eng]
            $"\n##Failed puzzles by {eng} (nn:{score.NeuralNet}) - overall performance: {score.PlayerRecord.Rating:F0} - Type: {r.Group.Type} - Theme: {r.Group.Filter} - Nodes: {r.Group.Nodes} - Rating group: {r.Group.RatingGroup}\n"

        let solvedHeader (eng: string) =
            let score = r.ScoresByEngine.[eng]
            $"\n##Solved puzzles by {eng} (nn:{score.NeuralNet}) - overall performance: {score.PlayerRecord.Rating:F0} - Type: {r.Group.Type} - Theme: {r.Group.Filter} - Nodes: {r.Group.Nodes} - Rating group: {r.Group.RatingGroup}\n"

        // Uniquely solved
        for eng in r.Engines do
            match Map.tryFind eng r.UniquelySolved with
            | Some puzzles when not puzzles.IsEmpty ->
                let nn = r.ScoresByEngine.[eng].NeuralNet
                let lines =
                    puzzles |> List.choose (fun p ->
                        getSolvedPuzzleEpd p |> Option.map (fun (fen, bm, uciCorrect) ->
                            $"{fen} bm {bm}; id \"Lichess id {p.PuzzleId}, engine {eng} (nn:{nn})\"; other \"{uciCorrect}\""))
                totalUniqSolved <- totalUniqSolved + writeSection swUS (solvedHeader eng) lines
            | _ -> ()

        // Uniquely failed: full format matching failedLichessPuzzles (bm, am, policy, other with UCI moves)
        for eng in r.Engines do
            match Map.tryFind eng r.UniquelyFailed with
            | Some puzzles when not puzzles.IsEmpty ->
                let nn = r.ScoresByEngine.[eng].NeuralNet
                let lines =
                    puzzles |> List.choose (fun (p, policyStr) ->
                        getFailedPuzzleEpd p policyStr |> Option.map (fun (fen, bm, am, bmP, amP, uciCorrect, uciPlayed) ->
                            $"{fen} bm {bm}; am {am}; id \"Lichess id {p.PuzzleId}, engine {eng} (nn:{nn}), policy value for bestmove {bm}={bmP} and move played {am}={amP}\"; other \"{uciCorrect},{uciPlayed}\""))
                totalUniqFailed <- totalUniqFailed + writeSection swUF (failedHeader eng) lines
            | _ -> ()

        // Failed by all: full format per engine, one section per engine so each has its own arrows/policy
        if not r.FailedByAll.IsEmpty then
            for eng in r.Engines do
                let lines =
                    r.FailedByAll |> List.choose (fun (p, moveMap) ->
                        match Map.tryFind eng moveMap with
                        | Some policyStr ->
                            getFailedPuzzleEpd p policyStr |> Option.map (fun (fen, bm, am, bmP, amP, uciCorrect, uciPlayed) ->
                                $"{fen} bm {bm}; am {am}; id \"Lichess id {p.PuzzleId}, policy value for bestmove {bm}={bmP} and move played {am}={amP}\"; other \"{uciCorrect},{uciPlayed}\"")
                        | None -> None)
                totalFailedAll <- totalFailedAll + writeSection swFA (failedHeader eng) lines

        // Solved by all: one section per engine
        if not r.SolvedByAll.IsEmpty then
            for eng in r.Engines do
                let lines =
                    r.SolvedByAll |> List.choose (fun p ->
                        getSolvedPuzzleEpd p |> Option.map (fun (fen, bm, uciCorrect) ->
                            $"{fen} bm {bm}; id \"Lichess id {p.PuzzleId}\"; other \"{uciCorrect}\""))
                totalSolvedAll <- totalSolvedAll + writeSection swSA (solvedHeader eng) lines

    // Only write files that have content
    let writeIfNotEmpty (path: string) (sw: StringWriter) =
        let content = sw.ToString()
        if not (String.IsNullOrWhiteSpace(content)) then
            File.WriteAllText(path, content)

    writeIfNotEmpty (Path.Combine(outputFolder, $"crossEngine_uniqueSolved_{dateStr}.epd")) swUS
    writeIfNotEmpty (Path.Combine(outputFolder, $"crossEngine_uniqueFailed_{dateStr}.epd")) swUF
    writeIfNotEmpty (Path.Combine(outputFolder, $"crossEngine_failedByAll_{dateStr}.epd"))  swFA
    writeIfNotEmpty (Path.Combine(outputFolder, $"crossEngine_solvedByAll_{dateStr}.epd"))  swSA

    // Console summary
    printfn "\n--- Cross-Engine Puzzle Analysis ---"
    for r in results do
        printfn "  Group: Type=%s  Rating group=%d  Nodes=%d  Theme=%s  Engines=%d"
            r.Group.Type r.Group.RatingGroup r.Group.Nodes r.Group.Filter r.Engines.Length
        for eng in r.Engines do
            let us = match Map.tryFind eng r.UniquelySolved with | Some l -> l.Length | None -> 0
            let uf = match Map.tryFind eng r.UniquelyFailed with | Some l -> l.Length | None -> 0
            if us > 0 || uf > 0 then
                printfn "    %s: uniquely solved=%d, uniquely failed=%d" eng us uf
        printfn "    Failed by all: %d,  Solved by all: %d" r.FailedByAll.Length r.SolvedByAll.Length

    printfn "  Totals: uniqueSolved=%d  uniqueFailed=%d  failedByAll=%d  solvedByAll=%d"
        totalUniqSolved totalUniqFailed totalFailedAll totalSolvedAll
    printfn "  Files written to: %s" outputFolder
