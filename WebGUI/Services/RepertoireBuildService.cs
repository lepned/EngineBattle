#nullable enable
using ChessLibrary;
using Microsoft.FSharp.Core;

namespace WebGUI.Services
{
    /// <summary>The Repertoire Trainer's Build: one repertoire build at a time, run in the background so
    /// it goes on when the page is left (as a tournament does). Pages subscribe to Changed.</summary>
    public class RepertoireBuildService
    {
        public enum BuildState { Idle, Running, Done, Stopped, Failed }

        private readonly object _lock = new();
        private readonly List<string> _log = new();
        private readonly ILogger<RepertoireBuildService> _logger;
        private CancellationTokenSource? _cts;

        public RepertoireBuildService(ILogger<RepertoireBuildService> logger) => _logger = logger;

        public BuildState State { get; private set; }
        /// Positions searched by an engine so far (the rest came from the caches).
        public int Searched { get; private set; }
        public DateTime StartedAt { get; private set; }
        public DateTime? EndedAt { get; private set; }
        public string OutPath { get; private set; } = "";
        public RepertoireBuild.Outcome? Outcome { get; private set; }
        public string? Error { get; private set; }

        public TimeSpan Elapsed => (EndedAt ?? DateTime.Now) - StartedAt;
        public bool IsRunning => State == BuildState.Running;

        public event Action? Changed;

        public IReadOnlyList<string> Log { get { lock (_lock) { return _log.ToList(); } } }

        /// <summary>Starts the build; false when one is running already.</summary>
        public bool Start(RepertoireBuild.Request request)
        {
            CancellationTokenSource cts;
            lock (_lock)
            {
                if (State == BuildState.Running) return false;
                _cts?.Dispose();
                _cts = cts = new CancellationTokenSource();
                _log.Clear();
                State = BuildState.Running;
                Searched = 0;
                StartedAt = DateTime.Now;
                EndedAt = null;
                OutPath = request.OutPath;
                Outcome = null;
                Error = null;
            }
            Raise();
            var log = FuncConvert.FromAction<string>(line =>
            {
                lock (_lock) { _log.Add(line); }
                Raise();
            });
            var onSearch = FuncConvert.FromAction<int>(n =>
            {
                Searched = n;
                Raise();
            });
            _ = Task.Run(() =>
            {
                try
                {
                    var outcome = RepertoireBuild.run(request, log, onSearch, cts.Token);
                    Finish(BuildState.Done, outcome, null);
                }
                catch (OperationCanceledException)
                {
                    Finish(BuildState.Stopped, null, null);
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "Repertoire build of {Path} failed", request.OutPath);
                    var message = ex is RepertoireBuild.NoContempt nc ? nc.Data0 : ex.Message;
                    Finish(BuildState.Failed, null, message);
                }
            });
            return true;
        }

        /// <summary>Stops the build; what was searched stays in the caches for the next one.</summary>
        public void Stop()
        {
            lock (_lock) { _cts?.Cancel(); }
        }

        private void Finish(BuildState state, RepertoireBuild.Outcome? outcome, string? error)
        {
            lock (_lock)
            {
                State = state;
                Outcome = outcome;
                Error = error;
                EndedAt = DateTime.Now;
            }
            Raise();
        }

        private void Raise()
        {
            try { Changed?.Invoke(); }
            catch (Exception ex) { _logger.LogDebug(ex, "A Changed subscriber failed"); }
        }
    }
}
