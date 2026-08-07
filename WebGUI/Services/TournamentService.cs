#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    public class TournamentService : IUpdateFeed
    {
        private Tournament.Manager.Runner? _runner;
        private Action<TournamentTypes.Update>? _subscriber;
        private LiveFeedRecorder? _recorder;
        private readonly JsonFeedService _jsonFeed;
        private readonly object _lock = new();

        public TournamentService(JsonFeedService jsonFeed) => _jsonFeed = jsonFeed;

        public bool IsRunning { get; private set; }
        public Tournament.Manager.Runner? CurrentRunner => _runner;

        /// <summary>True while updates are being recorded to an NDJSON file.</summary>
        public bool IsRecording { get { lock (_lock) { return _recorder != null; } } }

        /// <summary>Start teeing every internal Update to an NDJSON file (the "record" half of the
        /// live-feed record-and-replay pipeline). Replaces any prior recording.</summary>
        public void StartRecording(string path)
        {
            lock (_lock)
            {
                _recorder?.Dispose();
                _recorder = new LiveFeedRecorder(path);
            }
        }

        /// <summary>Stop and flush the current recording, if any.</summary>
        public void StopRecording()
        {
            lock (_lock)
            {
                _recorder?.Dispose();
                _recorder = null;
            }
        }

        private void HandleUpdate(TournamentTypes.Update update)
        {
            if (update is TournamentTypes.Update.EndOfTournament)
                IsRunning = false;

            LiveFeedRecorder? recorder;
            Action<TournamentTypes.Update>? handler;
            lock (_lock) { recorder = _recorder; handler = _subscriber; }

            try { recorder?.Record(update); }
            catch (Exception) { /* recording is best-effort */ }

            // Live JSON bridge: when a feed view (/tournament-feed) is listening, drive it in real time
            // through the wire contract (serialize -> JsonFeedService -> parse -> dispatch). This is the
            // live end-to-end test path; it engages automatically only when a feed view is subscribed.
            if (_jsonFeed.HasSubscriber)
            {
                try { _jsonFeed.Ingest(LiveFeedWire.serializeUpdate(update)); }
                catch (Exception) { /* bridge is best-effort */ }
            }

            try { handler?.Invoke(update); }
            catch (Exception) { /* disposed component — ignore */ }
        }

        public void Subscribe(Action<TournamentTypes.Update> handler)
        {
            lock (_lock) { _subscriber = handler; }
        }

        public void Unsubscribe()
        {
            lock (_lock) { _subscriber = null; }
        }

        /// <summary>Compare-and-clear: only clears the slot if it still holds this handler.
        /// A second tab's dispose must not silence the tab that currently owns the feed.</summary>
        public void Unsubscribe(Action<TournamentTypes.Update> handler)
        {
            // Delegate value equality (same target + method), not reference equality:
            // Subscribe(Update) and Unsubscribe(Update) create distinct delegate instances.
            lock (_lock) { if (Equals(_subscriber, handler)) _subscriber = null; }
        }

        public Tournament.Manager.Runner CreateRunner(ILogger logger, ShutdownTokenProvider shutdown)
        {
            if (IsRunning)
                throw new InvalidOperationException("Tournament already running. Cancel first.");
            _runner = new Tournament.Manager.Runner(logger, HandleUpdate, true, false);
            _runner.LinkCancellation(shutdown.Token);
            return _runner;
        }

        public void MarkRunning() => IsRunning = true;

        public void Cancel()
        {
            _runner?.Cancel();
            IsRunning = false;
        }

        public Tournament.Manager.Runner GetConfigRunner(ILogger logger)
        {
            if (_runner != null)
            {
                if (!IsRunning) _runner.InvalidateTournament();
                return _runner;
            }
            _runner = new Tournament.Manager.Runner(logger, HandleUpdate, true, false);
            return _runner;
        }
    }
}
