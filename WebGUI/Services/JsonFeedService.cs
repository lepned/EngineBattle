#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    /// <summary>
    /// Consumes a Live Feed JSON event stream (see LiveFeedContract.md) and dispatches the parsed
    /// <see cref="TournamentTypes.Update"/> events to a subscriber — exactly like
    /// <see cref="TournamentService"/> does for the internal engine runner. This lets the live
    /// visualization render from an external (proprietary) tournament runner, or from a recorded
    /// stream replayed for testing.
    ///
    /// Parsing is delegated to <c>ChessLibrary.LiveFeedWire.tryParseUpdate</c>, the single source of
    /// truth for the wire format (shared with the F# producer side).
    /// </summary>
    public class JsonFeedService : IUpdateFeed
    {
        private Action<TournamentTypes.Update>? _subscriber;
        private readonly object _lock = new();

        public bool IsRunning { get; private set; }

        public void Subscribe(Action<TournamentTypes.Update> handler)
        {
            lock (_lock) { _subscriber = handler; }
        }

        public void Unsubscribe()
        {
            lock (_lock) { _subscriber = null; }
        }

        /// <summary>
        /// Ingest a single wire-format JSON event and dispatch it to the subscriber.
        /// Returns true if the line was parsed and dispatched; false for blank or malformed input.
        /// </summary>
        public bool Ingest(string json)
        {
            if (string.IsNullOrWhiteSpace(json))
                return false;

            // FSharpOption<T>: None is represented as null, Some as a non-null wrapper.
            var parsed = LiveFeedWire.tryParseUpdate(json);
            if (parsed == null)
                return false;

            Dispatch(parsed.Value);
            return true;
        }

        /// <summary>
        /// Ingest many wire events (e.g. NDJSON lines). Blank/malformed lines are skipped.
        /// Returns the number of events dispatched.
        /// </summary>
        public int IngestMany(IEnumerable<string> jsonEvents)
        {
            int dispatched = 0;
            foreach (var line in jsonEvents)
                if (Ingest(line))
                    dispatched++;
            return dispatched;
        }

        /// <summary>Reset running state (e.g. before replaying a new stream).</summary>
        public void Reset() => IsRunning = false;

        private void Dispatch(TournamentTypes.Update update)
        {
            switch (update)
            {
                case TournamentTypes.Update.StartOfTournament:
                    IsRunning = true;
                    break;
                case TournamentTypes.Update.EndOfTournament:
                    IsRunning = false;
                    break;
            }

            Action<TournamentTypes.Update>? handler;
            lock (_lock) { handler = _subscriber; }
            try { handler?.Invoke(update); }
            catch (Exception) { /* disposed component — ignore */ }
        }
    }
}
