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
        private Action<string, TournamentTypes.Update>? _multiSubscriber;
        private readonly object _lock = new();

        public bool IsRunning { get; private set; }

        /// <summary>True when any feed view (single- or multi-game) is subscribed (used to auto-engage the live bridge).</summary>
        public bool HasSubscriber { get { lock (_lock) { return _subscriber != null || _multiSubscriber != null; } } }

        public void Subscribe(Action<TournamentTypes.Update> handler)
        {
            lock (_lock) { _subscriber = handler; }
        }

        public void Unsubscribe()
        {
            lock (_lock) { _subscriber = null; }
        }

        /// <summary>Subscribe to the demultiplexed stream: handler receives (gameId, update) for every
        /// event, where gameId is the envelope stream key ("" for the single/global game). Used by the
        /// multi-game grid view to route per-game events to per-game tiles.</summary>
        public void SubscribeMulti(Action<string, TournamentTypes.Update> handler)
        {
            lock (_lock) { _multiSubscriber = handler; }
        }

        public void UnsubscribeMulti()
        {
            lock (_lock) { _multiSubscriber = null; }
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

            var gameId = LiveFeedWire.readGameId(json); // "" when absent (single/global game)
            Dispatch(gameId, parsed.Value);
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

        private void Dispatch(string gameId, TournamentTypes.Update update)
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

            Action<TournamentTypes.Update>? single;
            Action<string, TournamentTypes.Update>? multi;
            lock (_lock) { single = _subscriber; multi = _multiSubscriber; }
            try { single?.Invoke(update); }
            catch (Exception) { /* disposed component — ignore */ }
            try { multi?.Invoke(gameId, update); }
            catch (Exception) { /* disposed component — ignore */ }
        }
    }
}
