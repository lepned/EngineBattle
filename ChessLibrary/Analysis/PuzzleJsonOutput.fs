module PuzzleJsonOutput

// --------------------------------------------------------------------------
// Structured JSON output for the `puzzlejson` CLI command.
//
// This module is the EngineBattle side of a stable public contract used by
// external tooling (e.g. the Python SPSA tuner) to consume puzzle-run results
// without parsing stdout. The schema is documented in
// `Console/PuzzleJsonSchema.md`.
//
// Versioning rules:
//   * `schemaVersion` is bumped only on BREAKING changes (rename or removal
//     of an existing field).
//   * New optional fields can be added freely without bumping the version;
//     consumers are expected to ignore unknown fields.
// --------------------------------------------------------------------------

open System
open System.IO
open System.Reflection
open System.Text.Json
open ChessLibrary
open ChessLibrary.PuzzleTypes

[<Literal>]
let SchemaVersion = 1

/// One row in the `scores` array — a single (engine, net, type, nodes) result.
type PuzzleScoreEntry =
    { Engine: string
      NeuralNet: string
      Type: string
      Nodes: int
      Filter: string
      TotalNumber: int
      Correct: int
      Wrong: int
      Accuracy: float
      RatingAvg: float
      PlayerRating: float
      PlayerDeviation: float
      PlayerVolatility: float
      AvgKLD: float
      // Weighted avg of per-puzzle KLD using 1/engineRank. 0.0 for non-policy tests.
      AvgRankWeightedKld: float
      // Frontier-weighted KLD: peaks at rank 2-3, low at rank 1 and 6+.
      AvgFrontierKld: float
      // Margin loss: pairwise comparison of P_correct vs P_best_competitor.
      AvgMarginLoss: float
      // Value head loss: |Q - expected_Q| from puzzle themes. Solved puzzles only.
      AvgValueLoss: float
      // Estimated PUCT nodes before a search first visits the correct move. These
      // mirror the P95/P99/Max/<=100 columns of the text summary, which was the only
      // place they existed; report tooling had to scrape that table to get them.
      // 0.0 for non-policy tests, matching AvgEstNodesLog10's "metric present" gate.
      AvgEstNodesLog10: float
      EstNodesP95: float
      EstNodesP99: float
      // Worst single puzzle in the set. HardestByEstNodes is sorted descending, so
      // this is its head; 0.0 when the list is empty.
      EstNodesMax: float
      // Fraction (0..1) of puzzles needing <= 100 nodes.
      EstNodesCdf100: float
      // Positions of multi-move puzzles scored correctly, and how many were scored.
      // Both 0 unless the run set ScoreAllPositions - that is how a consumer tells
      // "not measured" from "measured as zero". `accuracy` above is unchanged and
      // remains per-PUZZLE all-or-nothing.
      PositionsCorrect: int
      PositionsScored: int
      // PositionsCorrect / PositionsScored, or 0.0 when nothing was scored.
      PositionAccuracy: float
      // The puzzle's FIRST solver move only - the move its themes actually describe.
      // Always measured, so unlike the position fields these are meaningful in every run.
      // Use THIS, not `accuracy`, when attributing a result to a theme: a puzzle failed
      // three moves deep is counted against its themes by `accuracy` even though the
      // failing position may have nothing to do with them.
      FirstMoveCorrect: int
      FirstMoveScored: int
      FirstMoveAccuracy: float
      WithHistory: bool }

/// One row in the `paired` array — two nets compared on the puzzles they both
/// scored. Mirrors PuzzlePaired.PairedComparison; kept as its own type so the
/// wire shape does not move when the internal record does.
type PuzzlePairedEntry =
    { Type: string
      RatingGroup: int
      Nodes: int
      Filter: string
      EngineA: string
      EngineB: string
      NetA: string
      NetB: string
      N: int
      /// Solved by A, failed by B.
      OnlyA: int
      /// Solved by B, failed by A.
      OnlyB: int
      /// OnlyA + OnlyB. Under ~25 the normal approximation behind `z` is optimistic.
      Discordant: int
      AccuracyAPct: float
      AccuracyBPct: float
      /// B minus A, percentage points.
      DeltaPp: float
      /// McNemar z, signed so positive favours B.
      Z: float
      /// Two-sided p for `z`.
      P: float }

/// Top-level JSON document.
type PuzzleJsonResult =
    { SchemaVersion: int
      EngineBattleVersion: string
      PuzzleFile: string
      TotalPuzzlesLoaded: int
      SampleSize: int
      MinRating: int
      MaxRating: int
      Filter: string
      RatingGroups: string
      StartedUtc: string
      ElapsedSeconds: float
      Scores: PuzzleScoreEntry array
      /// Every net pair on every slice, measured on the puzzles both scored.
      /// Empty for single-net runs - a routine workflow, not a problem. Added after
      /// schemaVersion 1: consumers that predate it ignore the field, so no version bump.
      Paired: PuzzlePairedEntry array
      /// True when computing the paired stats threw. `paired` is then empty for a
      /// DIFFERENT reason than a single-net run, and a consumer must not read the
      /// absence of comparisons as "there was only one net".
      PairedFailed: bool }

/// Clamp NaN/Infinity to 0.0 so System.Text.Json never throws on serialization.
/// Matches the existing tuner convention (see Bayesian LLR clamping).
let private safeFinite (x: float) =
    if Double.IsNaN x || Double.IsInfinity x then 0.0 else x

/// Convert a single Score record to a serializable entry, computing accuracy
/// and clamping any non-finite floats.
let toEntry (s: Score) : PuzzleScoreEntry =
    let total = s.TotalNumber
    let correct = s.Correct
    let accuracy =
        if total > 0 then float correct / float total else 0.0
    { Engine = (if isNull s.Engine then "" else s.Engine)
      NeuralNet = (if isNull s.NeuralNet then "" else s.NeuralNet)
      Type = (if isNull s.Type then "" else s.Type)
      Nodes = s.Nodes
      Filter = (if isNull s.Filter then "" else s.Filter)
      TotalNumber = total
      Correct = correct
      Wrong = total - correct
      Accuracy = safeFinite accuracy
      RatingAvg = safeFinite s.RatingAvg
      PlayerRating = safeFinite s.PlayerRecord.Rating
      PlayerDeviation = safeFinite s.PlayerRecord.Deviation
      PlayerVolatility = safeFinite s.PlayerRecord.Volatility
      AvgKLD = safeFinite s.AvgKLD
      AvgRankWeightedKld = safeFinite s.AvgRankWeightedKld
      AvgFrontierKld = safeFinite s.AvgFrontierKld
      AvgMarginLoss = safeFinite s.AvgMarginLoss
      AvgValueLoss = safeFinite s.AvgValueLoss
      AvgEstNodesLog10 = safeFinite s.AvgEstNodesLog10
      EstNodesP95 = safeFinite s.EstNodesP95
      EstNodesP99 = safeFinite s.EstNodesP99
      EstNodesMax =
          if s.HardestByEstNodes.Count > 0 then safeFinite (snd s.HardestByEstNodes.[0]) else 0.0
      EstNodesCdf100 = safeFinite s.EstNodesCdf100
      PositionsCorrect = s.PositionsCorrect
      PositionsScored = s.PositionsScored
      PositionAccuracy =
          if s.PositionsScored > 0 then
              safeFinite (float s.PositionsCorrect / float s.PositionsScored)
          else 0.0
      FirstMoveCorrect = s.FirstMoveCorrect
      FirstMoveScored = s.FirstMoveScored
      FirstMoveAccuracy =
          if s.FirstMoveScored > 0 then
              safeFinite (float s.FirstMoveCorrect / float s.FirstMoveScored)
          else 0.0
      WithHistory = s.WithHistory }

let toPairedEntry (c: PuzzlePaired.PairedComparison) : PuzzlePairedEntry =
    { Type = c.Type
      RatingGroup = c.RatingGroup
      Nodes = c.Nodes
      Filter = c.Filter
      EngineA = c.EngineA
      EngineB = c.EngineB
      NetA = c.NetA
      NetB = c.NetB
      N = c.N
      OnlyA = c.OnlyA
      OnlyB = c.OnlyB
      Discordant = c.Discordant
      AccuracyAPct = safeFinite c.AccuracyAPct
      AccuracyBPct = safeFinite c.AccuracyBPct
      DeltaPp = safeFinite c.DeltaPp
      Z = safeFinite c.Z
      P = safeFinite (PuzzlePaired.pValueOf c.Z) }

let private getEbVersion () =
    try
        let v = Assembly.GetExecutingAssembly().GetName().Version
        if isNull v then "0.0.0" else v.ToString()
    with _ -> "0.0.0"

/// Build a PuzzleJsonResult from raw scores, metadata, and an ALREADY-COMPUTED paired
/// list. Pure function — no I/O.
///
/// The paired stats are a parameter rather than something this function derives, because
/// a run writes them to the text summary as well and, with `--json`, to two files. Deriving
/// them here meant the same O(k^2) set work ran two or three times per run and, worse, that
/// nothing guaranteed the JSON and the text table were describing the same computation.
let buildResultWithPaired
    (paired: PuzzlePaired.PairedOutcome)
    (puzzleFile: string)
    (totalPuzzlesLoaded: int)
    (sampleSize: int)
    (minRating: int)
    (maxRating: int)
    (filter: string)
    (ratingGroups: string)
    (startedUtc: DateTime)
    (elapsedSeconds: float)
    (scores: seq<Score>) : PuzzleJsonResult =
    let utc =
        if startedUtc.Kind = DateTimeKind.Utc then startedUtc
        else startedUtc.ToUniversalTime()
    // Walked twice (entries and paired stats); a lazy seq would run the source twice.
    let materialized = scores |> Seq.toList
    { SchemaVersion = SchemaVersion
      EngineBattleVersion = getEbVersion ()
      PuzzleFile = (if isNull puzzleFile then "" else puzzleFile)
      TotalPuzzlesLoaded = totalPuzzlesLoaded
      SampleSize = sampleSize
      MinRating = minRating
      MaxRating = maxRating
      Filter = (if isNull filter then "" else filter)
      RatingGroups = (if isNull ratingGroups then "" else ratingGroups)
      StartedUtc = utc.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
      ElapsedSeconds = safeFinite elapsedSeconds
      Scores = materialized |> Seq.map toEntry |> Seq.toArray
      Paired = paired.Comparisons |> List.map toPairedEntry |> List.toArray
      PairedFailed = paired.Failed }

/// Convenience wrapper: computes the paired stats itself, orienting pairs by the order
/// the scores arrive in. Every production caller passes an outcome it already has, so this
/// exists for callers that only want a document - which today means the tests.
let buildResult
    (puzzleFile: string)
    (totalPuzzlesLoaded: int)
    (sampleSize: int)
    (minRating: int)
    (maxRating: int)
    (filter: string)
    (ratingGroups: string)
    (startedUtc: DateTime)
    (elapsedSeconds: float)
    (scores: seq<Score>) : PuzzleJsonResult =
    let materialized = scores |> Seq.toList
    buildResultWithPaired
        (PuzzlePaired.outcomeOf (PuzzlePaired.compute materialized))
        puzzleFile totalPuzzlesLoaded sampleSize minRating maxRating
        filter ratingGroups startedUtc elapsedSeconds materialized

let private serializerOptions () =
    let opts = JsonSerializerOptions()
    opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    opts.WriteIndented <- true
    opts

/// Serialize a PuzzleJsonResult to a JSON string (camelCase, indented).
let serialize (result: PuzzleJsonResult) : string =
    JsonSerializer.Serialize(result, serializerOptions ())

/// Write a PuzzleJsonResult to a file path. Creates parent directories if needed.
let writeToFile (path: string) (result: PuzzleJsonResult) : unit =
    let dir = Path.GetDirectoryName path
    if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore
    File.WriteAllText(path, serialize result)
