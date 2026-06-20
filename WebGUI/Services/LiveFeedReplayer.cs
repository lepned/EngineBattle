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

        /// <summary>
        /// Replay several games CONCURRENTLY into the feed for the multi-game grid view. A single EB
        /// recording holds games played sequentially; this splits it into per-game segments
        /// (StartOfGame..EndOfGame) and replays the first <paramref name="maxGames"/> at once, each
        /// stamped with its own gameId, so the grid renders them side by side. The initial
        /// StartOfTournament is sent once globally. Returns the number of events dispatched.
        /// </summary>
        public async Task<int> ReplayGamesConcurrentlyAsync(string path, int maxGames, int perMoveDelayMs = 600, CancellationToken externalToken = default)
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
                var lines = await File.ReadAllLinesAsync(path, cts.Token);

                // Split into per-game segments; collect pre-game globals (StartOfTournament) separately.
                var preroll = new List<string>();
                var segments = new List<List<string>>();
                List<string>? current = null;
                foreach (var line in lines)
                {
                    if (string.IsNullOrWhiteSpace(line)) continue;
                    var t = LiveFeedWire.readType(line);
                    if (t == "StartOfGame")
                    {
                        current = new List<string> { line };
                        segments.Add(current);
                    }
                    else if (current != null)
                    {
                        current.Add(line);
                        if (t == "EndOfGame") current = null;
                    }
                    else
                    {
                        preroll.Add(line);
                    }
                }

                _feed.Reset();
                // Send the tournament header once (global, no gameId).
                foreach (var g in preroll)
                    if (LiveFeedWire.readType(g) == "StartOfTournament" && _feed.Ingest(g))
                        dispatched++;

                // Replay the first maxGames segments concurrently, each as its own gameId, lightly staggered.
                var chosen = segments.Take(Math.Max(1, maxGames)).ToList();
                var tasks = chosen
                    .Select((seg, i) => ReplaySegmentAsync(seg, $"g{i + 1}", perMoveDelayMs, i * 250, cts.Token))
                    .ToArray();
                var counts = await Task.WhenAll(tasks);
                dispatched += counts.Sum();
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

        private async Task<int> ReplaySegmentAsync(List<string> lines, string gameId, int perMoveDelayMs, int startDelayMs, CancellationToken ct)
        {
            int n = 0;
            int betweenEventDelayMs = perMoveDelayMs <= 0 ? 0 : Math.Min(perMoveDelayMs, 20);
            try
            {
                if (startDelayMs > 0)
                    await Task.Delay(startDelayMs, ct);
                foreach (var line in lines)
                {
                    ct.ThrowIfCancellationRequested();
                    var stamped = LiveFeedWire.withGameId(gameId, line);
                    if (_feed.Ingest(stamped))
                    {
                        n++;
                        if (perMoveDelayMs > 0)
                        {
                            var delay = LiveFeedWire.readType(line) == "BestMove" ? perMoveDelayMs : betweenEventDelayMs;
                            if (delay > 0)
                                await Task.Delay(delay, ct);
                        }
                    }
                }
            }
            catch (OperationCanceledException) { /* stopped */ }
            return n;
        }

        /// <summary>Stop an in-progress replay, if any.</summary>
        public void Stop()
        {
            lock (_lock) { _cts?.Cancel(); }
        }
    }
}
