#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    /// <summary>
    /// Replays a recorded NDJSON live-feed file (see <see cref="LiveFeedRecorder"/> /
    /// LiveFeedContract.md §6) into the <see cref="JsonFeedService"/>, driving the feed view exactly
    /// as an external producer would. The "replay" half of the record-and-replay pipeline.
    ///
    /// Each non-blank line is one wire event. <paramref name="perMoveDelayMs"/> paces the replay by
    /// MOVE: the delay is applied after each committed move (BestMove); the rapid "thinking" events
    /// (Status/NNSeq/Time/etc.) between moves stream with only a tiny delay so the search still
    /// animates. With 0 it streams as fast as the UI can consume.
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
        public async Task<int> ReplayFileAsync(string path, int perMoveDelayMs = 0, CancellationToken externalToken = default)
        {
            // Between-move "thinking" events stream quickly so the search animates; the full delay is
            // applied only at move boundaries so perMoveDelayMs reads as "seconds per move".
            int betweenEventDelayMs = perMoveDelayMs <= 0 ? 0 : Math.Min(perMoveDelayMs, 20);

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
                        if (perMoveDelayMs > 0)
                        {
                            var isMove = line.Contains("\"type\":\"BestMove\"", StringComparison.Ordinal);
                            var delay = isMove ? perMoveDelayMs : betweenEventDelayMs;
                            if (delay > 0)
                                await Task.Delay(delay, cts.Token);
                        }
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
