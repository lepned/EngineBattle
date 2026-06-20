#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    public class TournamentService : IUpdateFeed
    {
        private Tournament.Manager.Runner? _runner;
        private Action<TournamentTypes.Update>? _subscriber;
        private readonly object _lock = new();

        public bool IsRunning { get; private set; }
        public Tournament.Manager.Runner? CurrentRunner => _runner;

        private void HandleUpdate(TournamentTypes.Update update)
        {
            if (update is TournamentTypes.Update.EndOfTournament)
                IsRunning = false;

            Action<TournamentTypes.Update>? handler;
            lock (_lock) { handler = _subscriber; }
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
