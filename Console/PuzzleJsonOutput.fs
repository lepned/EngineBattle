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
      WithHistory: bool }

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
      Scores: PuzzleScoreEntry array }

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
      WithHistory = s.WithHistory }

let private getEbVersion () =
    try
        let v = Assembly.GetExecutingAssembly().GetName().Version
        if isNull v then "0.0.0" else v.ToString()
    with _ -> "0.0.0"

/// Build a PuzzleJsonResult from raw scores and metadata. Pure function — no I/O.
/// Caller is responsible for measuring `elapsedSeconds` and providing `startedUtc`.
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
    let utc =
        if startedUtc.Kind = DateTimeKind.Utc then startedUtc
        else startedUtc.ToUniversalTime()
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
      Scores = scores |> Seq.map toEntry |> Seq.toArray }

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
