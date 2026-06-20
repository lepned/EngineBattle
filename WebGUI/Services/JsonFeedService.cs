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
        private readonly List<Action<string, TournamentTypes.Update>> _multiSubscribers = new();
        private readonly object _lock = new();

        public bool IsRunning { get; private set; }

        /// <summary>True when any feed view (single- or multi-game) is subscribed (used to auto-engage the live bridge).</summary>
        public bool HasSubscriber { get { lock (_lock) { return _subscriber != null || _multiSubscribers.Count > 0; } } }

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
        /// multi-game grid view and focused per-game views. Multiple subscribers are supported (one per
        /// open view) so a grid and a focused tab don't clobber each other.</summary>
        public void SubscribeMulti(Action<string, TournamentTypes.Update> handler)
        {
            lock (_lock) { if (!_multiSubscribers.Contains(handler)) _multiSubscribers.Add(handler); }
        }

        public void UnsubscribeMulti(Action<string, TournamentTypes.Update> handler)
        {
            lock (_lock) { _multiSubscribers.Remove(handler); }
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

            // Demux key namespaces the game by its source (server), so two servers using the same
            // local gameId (e.g. slot "1") don't collide. "source/gameId", or just gameId when local.
            var gameId = LiveFeedWire.readGameId(json); // "" when absent (single/global game)
            var source = LiveFeedWire.readSource(json);
            var key = string.IsNullOrEmpty(source) ? gameId : source + "/" + gameId;
            Dispatch(key, parsed.Value);
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
            Action<string, TournamentTypes.Update>[] multi;
            lock (_lock) { single = _subscriber; multi = _multiSubscribers.ToArray(); }
            try { single?.Invoke(update); }
            catch (Exception) { /* disposed component — ignore */ }
            foreach (var m in multi)
            {
                try { m(gameId, update); }
                catch (Exception) { /* disposed component — ignore */ }
            }
        }
    }
}
