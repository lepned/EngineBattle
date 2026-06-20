module LiveFeedRecorderTests

open System
open System.IO
open Xunit
open ChessLibrary
open ChessLibrary.MiscTypes
open ChessLibrary.EngineTypes
open ChessLibrary.TypesDef.CoreTypes
open ChessLibrary.TypesDef.Tournament
open ChessLibrary.TournamentTypes
open ChessLibrary.LiveFeedWire

let private sampleEvents: Update list =
    [ Update.StartOfTournament
        { NumberOfGames = 2
          GameDurationInSec = TimeSpan.FromSeconds 60.0
          TournamentDurationSec = TimeSpan.FromSeconds 120.0
          Tournament = Some Tournament.Empty }
      Update.GameStarted "A"
      Update.Status
          { EngineStatus.Empty with
              PlayerName = "A"
              Eval = CP 0.25
              Depth = 20
              Nodes = 1000000L }
      Update.RoundNr "1/2"
      Update.TotalNumberOfPairs 2
      Update.EndOfGame
          { Player1 = "A"; Player2 = "B"; Moves = 30; Result = "1-0"
            Reason = ResultReason.Checkmate; GameTime = 60000L; OutOfOpeningEvals = [] }
      Update.GameSummary "done"
      Update.EndOfTournament Tournament.Empty ]

[<Fact>]
let ``recorder writes one parseable wire line per event`` () =
    let path = Path.Combine(Path.GetTempPath(), $"ebfeed_{Guid.NewGuid():N}.jsonl")
    try
        (use rec1 = new LiveFeedRecorder(path)
         for u in sampleEvents do
             rec1.Record u)
        // file is flushed/closed after the recorder is disposed at end of scope above

        let lines =
            File.ReadAllLines path
            |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace l))

        Assert.Equal(sampleEvents.Length, lines.Length)

        // every recorded line parses back, and re-serializes identically to a direct serialize
        Array.iter2
            (fun (line: string) (orig: Update) ->
                match tryParseUpdate line with
                | Some parsed -> Assert.Equal(serializeUpdate orig, serializeUpdate parsed)
                | None -> Assert.True(false, sprintf "recorded line did not parse: %s" line))
            lines
            (List.toArray sampleEvents)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``recorded file round-trips event types in order`` () =
    let path = Path.Combine(Path.GetTempPath(), $"ebfeed_{Guid.NewGuid():N}.jsonl")
    try
        (use rec1 = new LiveFeedRecorder(path)
         for u in sampleEvents do
             rec1.Record u)

        let parsed =
            File.ReadAllLines path
            |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace l))
            |> Array.choose tryParseUpdate

        Assert.Equal(sampleEvents.Length, parsed.Length)
        match parsed.[0], Array.last parsed with
        | Update.StartOfTournament _, Update.EndOfTournament _ -> ()
        | _ -> Assert.True(false, "expected StartOfTournament first and EndOfTournament last")
    finally
        if File.Exists path then File.Delete path
