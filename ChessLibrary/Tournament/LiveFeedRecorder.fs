namespace ChessLibrary

open System
open System.IO
open System.Text
open System.Threading
open ChessLibrary.TournamentTypes
open ChessLibrary.LiveFeedWire

type private RecorderMessage =
    | Line of string
    /// Flushes and closes the file; lines posted after it are dropped.
    | Close of AsyncReplyChannel<unit>

/// Tees a tournament `Update` stream to an NDJSON file — one wire-JSON event per line. This is the
/// "record" half of the live-feed record-and-replay pipeline (LiveFeedContract.md §6): run a normal
/// internal tournament with recording on, then replay the file into JsonFeedService to validate the
/// contract and the feed view. The file is also a valid external-producer stream.
///
/// An agent owns the file and writes asynchronously: callers on any thread only post, so a slow
/// disk holds up no game and no thread.
type LiveFeedRecorder(path: string) =
    let dir = Path.GetDirectoryName(path)
    do
        if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore
    // FileShare.ReadWrite so another process (the WebGUI grid) can tail the file live.
    let writer =
        let fs = new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.ReadWrite, 4096, true)
        new StreamWriter(fs, UTF8Encoding(false))
    let agent =
        MailboxProcessor<RecorderMessage>.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | Line line ->
                    try
                        // flushed line by line: a reader tailing the file never sees half a line
                        do! writer.WriteLineAsync line |> Async.AwaitTask
                        do! writer.FlushAsync() |> Async.AwaitTask
                    with _ -> ()   // recording is best-effort
                    return! loop ()
                | Close reply ->
                    // apart: a flush that fails (a full disk) must not keep the file open
                    try writer.Flush() with _ -> ()
                    try writer.Dispose() with _ -> ()
                    reply.Reply () }
            loop ())
    let mutable disposed = 0

    member val Path = path

    /// Append a pre-serialized wire line.
    member _.RecordLine(line: string) = agent.Post (Line line)

    /// Append one `Update` as a single wire-JSON line (serialized here, as the update is now).
    member _.Record(update: Update) =
        if onWire update then agent.Post (Line (serializeUpdate update))

    /// Writes out what was recorded and closes the file.
    member _.Dispose() =
        if Interlocked.Exchange(&disposed, 1) = 0 then
            agent.TryPostAndReply((fun reply -> Close reply), 10_000) |> ignore

    interface IDisposable with
        member this.Dispose() = this.Dispose()
