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

        /// <summary>True when the loaded tournament will use the parallel runner with more than
        /// one concurrent game (RR/Gauntlet). Pages use this to route to the grid view and to
        /// disable single-game user adjudication.</summary>
        public bool IsParallelRun => _runner?.IsParallelRun ?? false;

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

            // Live JSON bridge: when a feed view (/tournament-feed or the grid) is listening, drive it
            // in real time through the wire contract (serialize -> JsonFeedService -> parse -> dispatch).
            // Parallel runs skip this untagged tee: their events arrive gameId-stamped through the
            // tagged sink instead, and an untagged copy would create a phantom ""-key tile in the grid.
            if (_jsonFeed.HasSubscriber && !IsParallelRun)
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
            _runner = NewRunner(logger);
            _runner.LinkCancellation(shutdown.Token);
            return _runner;
        }

        private Tournament.Manager.Runner NewRunner(ILogger logger)
        {
            var runner = new Tournament.Manager.Runner(logger, HandleUpdate, true, false);
            // In-process tagged tee for the multi-board grid: gameId-stamped wire lines go straight
            // into JsonFeedService (no HTTP loopback, no file). Only the parallel runner invokes the
            // sink; sequential runs never see it. JsonFeedService caches per-game snapshots even with
            // no subscriber, so a grid opened mid-run gets catch-up.
            runner.SetTaggedSink((gid, u) =>
            {
                try { _jsonFeed.Ingest(LiveFeedWire.withGameId(gid, LiveFeedWire.serializeUpdate(u))); }
                catch (Exception) { /* bridge is best-effort */ }
            });
            return runner;
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
            _runner = NewRunner(logger);
            return _runner;
        }
    }
}
