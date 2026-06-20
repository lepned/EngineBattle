#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    public class TournamentService : IUpdateFeed
    {
        private Tournament.Manager.Runner? _runner;
        private Action<TournamentTypes.Update>? _subscriber;
        private LiveFeedRecorder? _recorder;
        private readonly object _lock = new();

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
