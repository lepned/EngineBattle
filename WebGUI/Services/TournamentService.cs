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

        // Idle -> Running (MarkRunning) -> Stopping (Cancel, or the run's EndOfTournament) -> Idle
        // when its Run() has returned (MarkEnded). A cancelled run still ends its game and stops
        // its engines: until then no other may start, or both play at once.
        private enum RunState { Idle, Running, Stopping }
        private RunState _state = RunState.Idle;
        private TaskCompletionSource _ended = CompletedEnd();

        private static TaskCompletionSource CompletedEnd()
        {
            var t = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
            t.SetResult();
            return t;
        }

        /// A tournament is playing (not one that is stopping).
        public bool IsRunning { get { lock (_lock) { return _state == RunState.Running; } } }

        /// A cancelled or finished run is still ending: its engines may still be up.
        public bool IsStopping { get { lock (_lock) { return _state == RunState.Stopping; } } }
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
            var next = new LiveFeedRecorder(path);
            LiveFeedRecorder? previous;
            lock (_lock)
            {
                previous = _recorder;
                _recorder = next;
            }
            // closed outside the lock: it writes out what it still holds, and updates must not wait for that
            previous?.Dispose();
        }

        /// <summary>Stop and flush the current recording, if any.</summary>
        public void StopRecording()
        {
            LiveFeedRecorder? previous;
            lock (_lock)
            {
                previous = _recorder;
                _recorder = null;
            }
            previous?.Dispose();
        }

        private void HandleUpdate(Tournament.Manager.Runner? from, TournamentTypes.Update update)
        {
            lock (_lock)
            {
                // a replaced run's late updates (a cancelled game, its standings, its end) are not this run's
                if (!ReferenceEquals(from, _runner))
                    return;
                if (update is TournamentTypes.Update.EndOfTournament && _state == RunState.Running)
                    _state = RunState.Stopping;
            }

            LiveFeedRecorder? recorder;
            Action<TournamentTypes.Update>? handler;
            lock (_lock) { recorder = _recorder; handler = _subscriber; }

            try { recorder?.Record(update); }
            catch (Exception) { /* recording is best-effort */ }

            // Live JSON bridge: when a feed view (/tournament-feed or the grid) is listening, drive it
            // in real time through the wire contract (serialize -> JsonFeedService -> parse -> dispatch).
            // Parallel runs skip this untagged tee: their events arrive gameId-stamped through the
            // tagged sink instead, and an untagged copy would create a phantom ""-key tile in the grid.
            if (_jsonFeed.HasSubscriber && !IsParallelRun && LiveFeedWire.onWire(update))
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
            lock (_lock)
            {
                if (_state == RunState.Running)
                    throw new InvalidOperationException("Tournament already running. Cancel first.");
                if (_state == RunState.Stopping)
                    throw new InvalidOperationException("The previous tournament is still stopping.");
                // the replaced runner's PGN file is closed now: its run has ended
                _runner?.Retire();
                _runner = NewRunner(logger);
                _runner.LinkCancellation(shutdown.Token);
                return _runner;
            }
        }

        private Tournament.Manager.Runner NewRunner(ILogger logger)
        {
            Tournament.Manager.Runner? self = null;
            var runner = new Tournament.Manager.Runner(logger, u => HandleUpdate(self, u), true);
            self = runner;
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

        public void MarkRunning()
        {
            lock (_lock)
            {
                _state = RunState.Running;
                _ended = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
            }
        }

        /// <summary>A run has returned, ended, failed or cancelled: the next may start.</summary>
        public void MarkEnded(Tournament.Manager.Runner runner)
        {
            TaskCompletionSource ended;
            lock (_lock)
            {
                if (!ReferenceEquals(runner, _runner))
                    return;
                _state = RunState.Idle;
                ended = _ended;
            }
            ended.TrySetResult();
        }

        public void Cancel()
        {
            Tournament.Manager.Runner? runner;
            lock (_lock)
            {
                runner = _runner;
                if (_state == RunState.Running)
                    _state = RunState.Stopping;
            }
            runner?.Cancel();
        }

        /// <summary>True once no run is ending any more; false when one is still stopping after the timeout.</summary>
        public async Task<bool> WaitUntilStoppedAsync(TimeSpan timeout)
        {
            Task ended;
            lock (_lock) { ended = _ended.Task; }
            return await Task.WhenAny(ended, Task.Delay(timeout)) == ended;
        }

        public Tournament.Manager.Runner GetConfigRunner(ILogger logger)
        {
            // one runner, though two circuits ask at once
            lock (_lock)
            {
                if (_runner != null)
                {
                    if (_state == RunState.Idle) _runner.InvalidateTournament();
                    return _runner;
                }
                _runner = NewRunner(logger);
                return _runner;
            }
        }
    }
}
