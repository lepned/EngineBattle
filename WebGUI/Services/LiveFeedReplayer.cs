#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    /// <summary>
    /// Replays a recorded NDJSON live-feed file (see <see cref="LiveFeedRecorder"/> /
    /// LiveFeedContract.md §6) into the <see cref="JsonFeedService"/>, driving the feed view exactly
    /// as an external producer would. The "replay" half of the record-and-replay pipeline.
    ///
    /// Each non-blank line is one wire event. An optional inter-event delay paces the replay so it is
    /// watchable; with delay 0 it streams as fast as the UI can consume.
    /// </summary>
    public class LiveFeedReplayer
    {
        private readonly JsonFeedService _feed;
        private CancellationTokenSource? _cts;
        private readonly object _lock = new();

        public LiveFeedReplayer(JsonFeedService feed) => _feed = feed;

        public bool IsReplaying
        {
            get { lock (_lock) { return _cts is { IsCancellationRequested: false }; } }
        }

        /// <summary>
        /// Replay a recorded NDJSON file. Cancels any replay already in progress. Runs to completion
        /// (or cancellation); returns the number of events dispatched.
        /// </summary>
        public async Task<int> ReplayFileAsync(string path, int interEventDelayMs = 0, CancellationToken externalToken = default)
        {
            if (!File.Exists(path))
                throw new FileNotFoundException("Live-feed replay file not found.", path);

            CancellationTokenSource cts;
            lock (_lock)
            {
                _cts?.Cancel();
                _cts = CancellationTokenSource.CreateLinkedTokenSource(externalToken);
                cts = _cts;
            }

            int dispatched = 0;
            try
            {
                _feed.Reset();
                using var reader = new StreamReader(path);
                string? line;
                while ((line = await reader.ReadLineAsync(cts.Token)) != null)
                {
                    cts.Token.ThrowIfCancellationRequested();
                    if (_feed.Ingest(line))
                    {
                        dispatched++;
                        if (interEventDelayMs > 0)
                            await Task.Delay(interEventDelayMs, cts.Token);
                    }
                }
            }
            catch (OperationCanceledException) { /* stopped by caller */ }
            finally
            {
                lock (_lock)
                {
                    if (_cts == cts)
                    {
                        _cts.Dispose();
                        _cts = null;
                    }
                }
            }
            return dispatched;
        }

        /// <summary>Stop an in-progress replay, if any.</summary>
        public void Stop()
        {
            lock (_lock) { _cts?.Cancel(); }
        }
    }
}
