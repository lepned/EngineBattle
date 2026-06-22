#nullable enable
using ChessLibrary;
using CoreTypes = ChessLibrary.TypesDef.CoreTypes;

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

        // Catch-up cache: lets a view opened mid-run reconstruct current state (LiveFeedContract.md §9.5).
        private TournamentTypes.Update? _startOfTournament;
        private TournamentTypes.Update? _endOfTournament;   // set once the tournament completes; replayed to late joiners
        private readonly List<CoreTypes.Result> _results = new();                  // all results, for standings
        private readonly List<string> _resultIds = new();                          // demux key per result (parallel to _results) — for pentanomial catch-up replay
        private readonly HashSet<string> _resultKeys = new();                      // dedup (a Ceres game arrives as both a per-thread gameEnd and a global gameResult)

        /// Stable per-game signature used to dedup result events from different streams.
        public static string ResultKey(CoreTypes.Result r) =>
            $"{r.Player1}|{r.Player2}|{r.Result}|{r.Moves}|{r.GameTime}";
        private readonly Dictionary<string, GameSnap> _snaps = new();               // current game per stream key

        private sealed class GameSnap
        {
            public TournamentTypes.Update? Start;                                    // last StartOfGame
            public readonly Dictionary<string, TournamentTypes.Update> StatusByPlayer = new();
            public TournamentTypes.Update? LastBestMove;
        }

        public bool IsRunning { get; private set; }

        /// <summary>True once an <c>EndOfTournament</c> has been seen — lets a view distinguish a
        /// completed tournament from a live (or silently dropped) one. Survives until the next
        /// <c>StartOfTournament</c> or an explicit <see cref="Reset"/>.</summary>
        public bool IsCompleted { get { lock (_lock) { return _endOfTournament != null; } } }

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
            lock (_lock)
            {
                if (!_multiSubscribers.Contains(handler)) _multiSubscribers.Add(handler);
                ReplaySnapshot(handler);   // bring a mid-run subscriber up to current state
            }
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

        /// <summary>Reset running state and catch-up cache (e.g. before replaying a new stream).</summary>
        public void Reset()
        {
            lock (_lock)
            {
                IsRunning = false;
                _startOfTournament = null;
                _endOfTournament = null;
                _results.Clear();
                _resultIds.Clear();
                _resultKeys.Clear();
                _snaps.Clear();
            }
        }

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
            lock (_lock)
            {
                UpdateCache(gameId, update);
                single = _subscriber;
                multi = _multiSubscribers.ToArray();
            }
            try { single?.Invoke(update); }
            catch (Exception) { /* disposed component — ignore */ }
            foreach (var m in multi)
            {
                try { m(gameId, update); }
                catch (Exception) { /* disposed component — ignore */ }
            }
        }

        // --- catch-up cache (called under _lock) ---

        private GameSnap Snap(string key)
        {
            if (!_snaps.TryGetValue(key, out var s)) { s = new GameSnap(); _snaps[key] = s; }
            return s;
        }

        private void UpdateCache(string key, TournamentTypes.Update update)
        {
            switch (update)
            {
                case TournamentTypes.Update.StartOfTournament:
                    // Don't globally reset — multiple servers each send StartOfTournament and must not
                    // wipe each other's cached state. Per-key snaps are replaced by their StartOfGame;
                    // a full reset is done explicitly via Reset() (e.g. before a file replay).
                    _startOfTournament = update;
                    _endOfTournament = null;   // a (re)start clears any prior completion marker
                    break;
                case TournamentTypes.Update.EndOfTournament:
                    _endOfTournament = update;   // remember completion so late joiners learn the tournament is over
                    break;
                case TournamentTypes.Update.StartOfGame:
                    _snaps[key] = new GameSnap { Start = update };   // new game on this stream resets its snap
                    break;
                case TournamentTypes.Update.Status st:
                    Snap(key).StatusByPlayer[st.Engine.PlayerName] = update;
                    break;
                case TournamentTypes.Update.BestMove:
                    Snap(key).LastBestMove = update;
                    break;
                case TournamentTypes.Update.EndOfGame e:
                    if (_resultKeys.Add(ResultKey(e.Result)))
                    {
                        _results.Add(e.Result);
                        _resultIds.Add(key);
                    }
                    break;
            }
        }

        // Replay current state to a newly-subscribed view: tournament header, accumulated results
        // (as PeriodicResults → standings), then each active game's start/status/last-move.
        private void ReplaySnapshot(Action<string, TournamentTypes.Update> handler)
        {
            try
            {
                if (_startOfTournament != null)
                    handler("", _startOfTournament);
                if (_results.Count > 0)
                    handler("", TournamentTypes.Update.NewPeriodicResults(new List<CoreTypes.Result>(_results)));
                // Replay thread-tagged results ("…/rN") as individual EndOfGame events so a late joiner
                // can rebuild pentanomial (paired by thread). Standings already covered above (dedup'd).
                for (int i = 0; i < _results.Count; i++)
                {
                    var id = _resultIds[i];
                    var board = id.Substring(id.LastIndexOf('/') + 1);
                    if (board.StartsWith("r"))
                        handler(id, TournamentTypes.Update.NewEndOfGame(_results[i]));
                }
                foreach (var (key, snap) in _snaps)
                {
                    if (snap.Start != null) handler(key, snap.Start);
                    foreach (var st in snap.StatusByPlayer.Values) handler(key, st);
                    if (snap.LastBestMove != null) handler(key, snap.LastBestMove);
                }
                // Last: if the tournament already finished, tell the new subscriber so it renders the
                // completed state instead of looking like a live-but-quiet feed.
                if (_endOfTournament != null)
                    handler("", _endOfTournament);
            }
            catch (Exception) { /* subscriber tearing down — ignore */ }
        }
    }
}
