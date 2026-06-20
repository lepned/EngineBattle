namespace ChessLibrary

open System
open System.Net.Http
open System.Text
open System.Threading
open System.Collections.Concurrent

/// Posts live-feed wire lines to an HTTP ingest endpoint (the WebGUI's POST /api/livefeed), the
/// network counterpart of LiveFeedRecorder. Lines are buffered and flushed as NDJSON batches on a
/// timer so the high-frequency stream doesn't issue a request per event. Best-effort: network
/// errors are swallowed so they never disrupt the tournament.
///
/// `source` (optional) is sent as X-Feed-Source so the grid can label this server; if omitted, the
/// endpoint falls back to the remote IP. `token` (optional) is sent as X-Feed-Token for auth.
type LiveFeedHttpSink(url: string, source: string, token: string) =
    let client = new HttpClient()
    let queue = ConcurrentQueue<string>()
    let flushLock = obj ()
    let mutable disposed = false

    do
        if not (String.IsNullOrEmpty token) then client.DefaultRequestHeaders.Add("X-Feed-Token", token)
        if not (String.IsNullOrEmpty source) then client.DefaultRequestHeaders.Add("X-Feed-Source", source)

    let flush () =
        // Skip if a flush is already running (timer ticks can overlap a slow POST).
        if Monitor.TryEnter flushLock then
            try
                if not queue.IsEmpty then
                    let sb = StringBuilder()
                    let mutable line = ""
                    while queue.TryDequeue(&line) do
                        sb.AppendLine(line) |> ignore
                    if sb.Length > 0 then
                        try
                            use content = new StringContent(sb.ToString(), Encoding.UTF8, "application/x-ndjson")
                            client.PostAsync(url, content).GetAwaiter().GetResult() |> ignore
                        with _ -> ()  // best-effort
            finally
                Monitor.Exit flushLock

    let timer = new Timer((fun _ -> flush ()), null, 200, 200)

    member _.Send(line: string) =
        if not disposed then queue.Enqueue line

    member _.Dispose() =
        if not disposed then
            disposed <- true
            timer.Dispose()
            flush ()
            client.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
