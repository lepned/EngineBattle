namespace ChessLibrary

open System
open System.IO
open System.Text
open ChessLibrary.TournamentTypes
open ChessLibrary.LiveFeedWire

/// Tees a tournament `Update` stream to an NDJSON file — one wire-JSON event per line. This is the
/// "record" half of the live-feed record-and-replay pipeline (LiveFeedContract.md §6): run a normal
/// internal tournament with recording on, then replay the file into JsonFeedService to validate the
/// contract and the feed view. The file is also a valid external-producer stream.
///
/// Thread-safe: the internal runner raises updates from background threads.
type LiveFeedRecorder(path: string) =
    let dir = Path.GetDirectoryName(path)
    do
        if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore
    // FileShare.ReadWrite so another process (the WebGUI grid) can tail the file live.
    let writer =
        let fs = new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
        new StreamWriter(fs, UTF8Encoding(false))
    let sync = obj ()
    do writer.AutoFlush <- true

    member val Path = path

    /// Append a pre-serialized wire line.
    member _.RecordLine(line: string) =
        lock sync (fun () -> writer.WriteLine line)

    /// Append one `Update` as a single wire-JSON line.
    member _.Record(update: Update) =
        let line = serializeUpdate update
        lock sync (fun () -> writer.WriteLine line)

    /// Append one `Update` as a wire-JSON line stamped with a gameId (for multi-game routing).
    member _.RecordWithGameId(gameId: string, update: Update) =
        let line = withGameId gameId (serializeUpdate update)
        lock sync (fun () -> writer.WriteLine line)

    member _.Dispose() =
        lock sync (fun () ->
            try
                writer.Flush()
                writer.Dispose()
            with _ ->
                ())

    interface IDisposable with
        member this.Dispose() = this.Dispose()
