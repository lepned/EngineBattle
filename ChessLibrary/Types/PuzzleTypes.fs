namespace ChessLibrary

open System
open System.Net.Http
open System.Xml.Linq
open System.Text.Json
open System.Text.Json.Serialization
open EPDTypes

/// Puzzle-related types for Lichess puzzles, ERET, and analysis configurations.
/// Contains PuzzleConfig, EretConfig, EngineListConfig, and related types.
module PuzzleTypes =

    // Forward reference to EngineConfig - we need to reference the CoreTypes module
    // This type will be defined in CoreTypes.fs and we reference it here
    type EngineConfigRef = System.Collections.Generic.Dictionary<string, obj>

    type PuzzleEngine =
    |Engine of ConfigName: string * Nodes: int
    |EngineWithNets of ConfigName: string * Nodes: int * ListOfNetsWithPaths: string list

    type PuzzleConfig =
        {
            PuzzleFile: string
            Type: string
            MaxRating: int
            MinRating: int
            RatingGroups: string
            mutable PuzzleFilter: string
            EngineFolder: string
            Engines: ResizeArray<PuzzleEngine>
            SampleSize: int
            Nodes: string
            //add an entry for path to write to file here
            FailedPuzzlesOutputFolder: string
            Failed: int
            Solved: int
            mutable Concurrency: int
            IncludeFailedPuzzles: bool
            /// Score EVERY position of a multi-move puzzle, not just up to the first
            /// mistake. Off by default: it adds a metric, it does not change the existing
            /// one, and for the value tests it also costs engine time (positions after a
            /// miss are queried too, where today they are skipped).
            ScoreAllPositions: bool  }

    type EretConfig = {
        EngineFolder: string
        Engines: ResizeArray<PuzzleEngine>
        PuzzleFile: string
        SampleSize: int
        Nodes: int
        TimeInSeconds: int
        RunWithNodeLimit : bool
        FailedPuzzlesOutputFolder: string
      }
        with
        static member empty =
            {
              EngineFolder = ""
              Engines = ResizeArray<PuzzleEngine>()
              PuzzleFile = ""
              SampleSize = 1000
              Nodes = 1
              TimeInSeconds = 10
              RunWithNodeLimit = false
              FailedPuzzlesOutputFolder = ""
            }


    type EngineListConfig = {
        EngineFolder: string
        Engines: ResizeArray<PuzzleEngine>
        Nodes: int
        ChartLines: int
        PolicyDistributionMinMaxFilter : string
        CombineWhiteAndBlackMoves: bool
    }
      with
        static member empty =
             {
              EngineFolder = ""
              Engines = ResizeArray<PuzzleEngine>()
              Nodes = 1
              ChartLines = 4
              PolicyDistributionMinMaxFilter = ""
              CombineWhiteAndBlackMoves = true
             }

    type PuzzleEngineConverter() =
          inherit JsonConverter<PuzzleEngine>()

          override _.Write(writer: Utf8JsonWriter, value: PuzzleEngine, _options) : unit =
            writer.WriteStartObject()
            match value with
            | Engine (configName, nodes) ->
                writer.WritePropertyName("Engine")
                writer.WriteStartObject()
                writer.WriteString("ConfigName", configName)
                writer.WriteNumber("Nodes", nodes)
                writer.WriteEndObject()

            | EngineWithNets (configName, nodes, nets) ->
                writer.WritePropertyName("EngineWithNets")
                writer.WriteStartObject()
                writer.WriteString("ConfigName", configName)
                writer.WriteNumber("Nodes", nodes)
                writer.WriteStartArray("ListOfNetsWithPaths")
                for net in nets do
                  writer.WriteStringValue(net)
                writer.WriteEndArray()
                writer.WriteEndObject()

            writer.WriteEndObject()

          override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: Type, _options: JsonSerializerOptions) : PuzzleEngine =
            // Expect the outer object start
            if reader.TokenType <> JsonTokenType.StartObject then
              raise (JsonException("Expected StartObject"))

            // Move into the first property (the case name)
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.PropertyName then
              raise (JsonException("Expected a DU-case property"))

            // Grab the case name: "Engine" or "EngineWithNets"
            let caseName = reader.GetString()

            // Advance into the inner object
            reader.Read() |> ignore
            if reader.TokenType <> JsonTokenType.StartObject then
              raise (JsonException("Expected StartObject for case payload"))

            // Read the contents of the inner object
            let mutable configName = ""
            let mutable nodes = 0
            let nets = ResizeArray<string>()
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
              if reader.TokenType = JsonTokenType.PropertyName then
                let prop = reader.GetString()
                reader.Read() |> ignore
                match prop with
                | "Nodes" ->
                    nodes <- reader.GetInt32()
                | "ConfigName" ->
                    configName <- reader.GetString()
                | "ListOfNetsWithPaths" ->
                    if reader.TokenType <> JsonTokenType.StartArray then
                      raise (JsonException("Expected StartArray for ListOfNetWithPaths"))
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                      nets.Add(reader.GetString())
                | _ ->
                    reader.Skip()

            // Now we're at the inner EndObject; advance to the outer EndObject
            reader.Read() |> ignore

            // Dispatch on the case name
            match caseName with
            | "Engine" ->
                Engine (configName, nodes)
            | "EngineWithNets" ->
                EngineWithNets(configName, nodes, List.ofSeq nets)
            | other ->
                raise (JsonException($"Unknown PuzzleEngine case: {other}"))

    type ERETResults = {
        PlayerName: string
        CorrectPuzzles: ResizeArray<EPDEntry>
        FailedPuzzles: ResizeArray<EPDEntry*string>
        Accuracy: float
        Desc: string
    }

    type ERET =
        | Start of Info: string
        | Puzzle of EPD: EPDEntry * Correct: bool
        | PlayerResult of ERETResults
        | AllResults of ERETResults list
        | ResultsInConsole of string
        | EretError of string

    type BlunderRecord =
      { Date: DateTime
        FEN: string
        Source: string option
        Lc0Version: string option
        NetworkUsed: string
        Nodes: int64
        IncorrectMove: string
        CorrectMove: string
        BlunderType: string
        Comments: string option
        DiscordContact: string option }

    type PlayerRecord = { Rating: float; Deviation: float; Volatility: float }

    type Position = { Command: string; CorrectMove: string; MovePlayed: string }

    //Define the engine-actor message type
    type EngineMsg =
        | Ok      of AsyncReplyChannel<bool>
        | BestMove  of cmd:Position * AsyncReplyChannel<string*float>
        | BestMoveWithPolicy  of cmd:Position * correct:string * AsyncReplyChannel<string * string>
        | BestMoveWithAllPolicies of cmd:Position * AsyncReplyChannel<string * EngineTypes.NNValues list>
        | EvalAllMovesValue of cmd:Position * AsyncReplyChannel<(string * float) list>
        | ValueTopNEval of cmd:Position * AsyncReplyChannel<string * EngineTypes.NNValues list>
        | SolvePuzzle of command:string * AsyncReplyChannel<string * string * ResizeArray<EngineTypes.NNValues>>
        | BestMoveValueHead of cmd:Position * AsyncReplyChannel<string>
        | NewGame   of AsyncReplyChannel<unit>
        | Quit      of AsyncReplyChannel<unit>
        | Network   of AsyncReplyChannel<string>

    type CsvPuzzleData =
      { /// The puzzle's own Lichess id, verbatim from the CSV (e.g. "00sHx"). It used to be
        /// `fields.[0].GetHashCode()`, which is randomised per PROCESS in .NET, so the ids in
        /// one run's failed-puzzle CSV meant nothing in the next one and could not be looked
        /// up on lichess.org at all. It also collided: 32 bits over a 4M-row database is a few
        /// thousand colliding pairs, and a collision silently merges two puzzles inside the
        /// paired sets and the first-move theme lookup.
        PuzzleId: string
        Fen: string
        Moves: string
        Rating: float
        RatingDeviation: float
        Popularity: int
        NbPlays: int
        Themes: string
        GameUrl: string
        OpeningTags: string
        Puzzle: string
        Commands: Position seq
        Fens: string seq
        Index: int }
      with
        static member Create(puzzleId, fen, moves, rating, ratingDev, popular, nbPlays, themes, gameUrl, openingTags, puzzle, commands, fens, index) =
          { PuzzleId = puzzleId
            Fen = fen
            Moves = moves
            Rating = rating
            RatingDeviation = ratingDev
            Popularity = popular
            NbPlays = nbPlays
            Themes = themes
            GameUrl = gameUrl
            OpeningTags = openingTags
            Puzzle = puzzle
            Commands = commands
            Fens = fens
            Index = index }

    type PuzzleResult =
      { PuzzleData : CsvPuzzleData
        WasCorrect : bool
        MovePlayed : string
        FailedMove : string
        ValueHead: bool
        Policy: string
        /// Positions of this puzzle the engine got right, and how many were scored.
        /// Both 0 unless the run set ScoreAllPositions.
        PositionsCorrect: int
        PositionsScored: int
        /// 1 when the puzzle's FIRST solver move was right, 0 otherwise; Scored is 1 once
        /// the puzzle has any position at all. Always measured - unlike the position
        /// counters this costs nothing, since the first position is always queried.
        FirstMoveCorrect: int
        FirstMoveScored: int
        KLD: float
        // Engine's rank (1-indexed) of the correct move at the puzzle command that
        // produced KLD. 0 = no rank data (e.g. classical engine, no policy probed,
        // value puzzles, solve test). Used for the rank-weighted KLD aggregate
        // metric: only puzzles with EngineRank > 0 contribute to AvgRankWeightedKld.
        EngineRank: int
        MarginLoss: float
        ValueLoss: float
      }

    type Score =
      { Engine: string
        NeuralNet: string
        TotalNumber: int
        Correct: int
        Wrong: int
        RatingAvg: float
        Filter: string
        PlayerRecord: PlayerRecord
        FailedPuzzles: ResizeArray<CsvPuzzleData * string>
        CorrectPuzzles: ResizeArray<CsvPuzzleData>
        Nodes: int
        WithHistory: bool
        Type: string
        AvgKLD: float
        // Weighted avg of per-puzzle KLD using 1/engineRank as weight, over all
        // puzzles with EngineRank > 0. 0.0 for non-policy tests. See PuzzleJsonSchema.md.
        AvgRankWeightedKld: float
        // Frontier-weighted KLD: peaks at rank 2-3 (near-misses), low at rank 1
        // (already solved) and rank 6+ (too far). Targets accuracy/Elo frontier.
        AvgFrontierKld: float
        // Margin loss: -log(P_correct / (P_correct + P_best_competitor)).
        // Measures how decisively the engine prefers the correct move over alternatives.
        AvgMarginLoss: float
        // Value head loss: |Q - expected_Q| from puzzle themes. Solved puzzles only.
        AvgValueLoss: float
        // Mean of log10(1 + N_est) where N_est estimates the parent visits a PUCT
        // search needs before first exploring the correct move (fixed match-play
        // CPuct/FPU constants, see PuzzleEngineAgent). 0.0 for non-policy tests —
        // consumers use that as the "metric present" gate. Not displayed; kept as
        // the smooth aggregate for potential tuner/JSON use.
        AvgEstNodesLog10: float
        // p95/p99 of the per-puzzle N_est distribution in raw node units, over ALL
        // puzzles: "in the worst 5%/1% of positions the search needs ~X nodes before
        // it even tries the correct move". Tail statistics — robust report numbers,
        // too noisy for tuning signals at small sample sizes.
        EstNodesP95: float
        EstNodesP99: float
        // CDF of the same per-puzzle N_est distribution at a fixed 100-node budget:
        // fraction of puzzles (0..1) whose estimated nodes-to-first-visit is <= 100.
        // 100 sits between P95 and P99 for current strong nets, where the
        // distributions actually differ (1000 saturated at ~100% for all of them).
        // First visit is necessary but not sufficient for solving — continued
        // investment depends on the value head liking the child position.
        EstNodesCdf100: float
        // Top puzzles by per-puzzle N_est, descending (worst first, capped at 50).
        // Lets the cheap 1-node policy run nominate worst-case candidates for
        // targeted real-search verification. Empty for non-policy tests.
        HardestByEstNodes: ResizeArray<CsvPuzzleData * float>
        // Positions of multi-move puzzles the engine got right, and how many were
        // scored. ADDITIVE: Correct/TotalNumber above stay all-or-nothing per puzzle,
        // so every historical number keeps its meaning. Both 0 unless the run set
        // ScoreAllPositions, which is the only way to tell "not measured" from
        // "measured as zero".
        PositionsCorrect: int
        PositionsScored: int
        // The FIRST solver move only - PuzzleDataUtils builds Commands from the odd-index
        // moves, so Commands[0] is the move the puzzle exists for and the one its themes
        // describe. Later moves are usually forced follow-up and carry the puzzle's tags
        // without being about them, which is why the theme breakdown should use THIS and
        // not the all-or-nothing puzzle verdict.
        FirstMoveCorrect: int
        FirstMoveScored: int
        // PuzzleIds whose FIRST solver move was right. A puzzle that was solved outright
        // is always in here; a failed one is in here when it went wrong later. The theme
        // breakdown needs this because a puzzle's tags describe its first move, not the
        // position it happened to fail at. Empty for tests that do not track it.
        FirstMoveCorrectIds: Collections.Generic.HashSet<string> }
        with
          static member empty =
            { Engine = ""
              NeuralNet = ""
              TotalNumber = 0
              Correct = 0
              Wrong = 0
              RatingAvg = 0.0
              Filter = ""
              PlayerRecord = { Rating = 0.0; Deviation = 0.0; Volatility = 0.0 }
              FailedPuzzles = ResizeArray<CsvPuzzleData * string>()
              CorrectPuzzles = ResizeArray<CsvPuzzleData>()
              Nodes = 0
              WithHistory = false
              Type = ""
              AvgKLD = 0.0
              AvgRankWeightedKld = 0.0
              AvgFrontierKld = 0.0
              AvgMarginLoss = 0.0
              AvgValueLoss = 0.0
              AvgEstNodesLog10 = 0.0
              EstNodesP95 = 0.0
              EstNodesP99 = 0.0
              EstNodesCdf100 = 0.0
              HardestByEstNodes = ResizeArray<CsvPuzzleData * float>()
              PositionsCorrect = 0
              PositionsScored = 0
              FirstMoveCorrect = 0
              FirstMoveScored = 0
              FirstMoveCorrectIds = Collections.Generic.HashSet<string>() }

    type Lichess =
      | PuzzleResult of Score
      | Done of string
      | Progress of Processed:int * Total:int * Label:string
      | LichessError of string

    type Iteration = { Id: int; Positions: CsvPuzzleData seq; Theme: string }

    type PuzzleCategory = { Category: string; Description: string }

    let getPuzzleCategoriesAsync = async {
        try
            let url = "https://raw.githubusercontent.com/lichess-org/lila/master/translation/source/puzzleTheme.xml"
            let fetchXmlContentAsync (url: string) = async {
                use client = new HttpClient()
                let! response = client.GetAsync(url) |> Async.AwaitTask
                response.EnsureSuccessStatusCode() |> ignore
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                return content
            }
            let parseXmlToRecords (xmlContent: string) =
                let doc = XDocument.Parse(xmlContent)
                doc.Descendants(XName.Get "string")
                |> Seq.choose (fun element ->
                    let nameAttr = element.Attribute(XName.Get "name")
                    let value = element.Value
                    match nameAttr with
                    | null -> None
                    | attr when attr.Value.EndsWith "Description" ->
                        let category = attr.Value.Replace("Description", "")
                        Some { Category = category; Description = value }
                    | _ -> None)
                |> Seq.toList
            let! xmlContent = fetchXmlContentAsync url
            let puzzleCategories = parseXmlToRecords xmlContent
            return puzzleCategories
        with ex ->
            printfn "Error: %s" ex.Message
            return []
    }
    let getPuzzleCategories() = getPuzzleCategoriesAsync |> Async.RunSynchronously
    let drawPuzzleCategories n =
        let rnd = Random()
        getPuzzleCategories()
        |> Seq.sortBy (fun _ -> rnd.Next())
        |> Seq.truncate n
        |> Seq.map (fun cat -> cat.Category)
        |> String.concat ","
