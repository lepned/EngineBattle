using ChessLibrary;
using static ChessLibrary.EngineTypes;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.TypesDef.CoreTypes;
using static ChessLibrary.GameAccuracyAnalysis;
using Microsoft.Extensions.Logging;
using Microsoft.FSharp.Core;

namespace WebGUI.Services;

public class GameReviewService : IAsyncDisposable
{
    private ChessLibrary.Engine.ChessEngineWithUCIProcessing _engine;
    private CancellationTokenSource _cts;
    private readonly ILogger<GameReviewService> _logger;

    public GameReviewService(ILogger<GameReviewService> logger)
    {
        _logger = logger;
    }

    public bool IsAnalyzing { get; private set; }

    public async Task<GameAnalysisResult> AnalyzeGameAsync(
        PgnGame game,
        EngineConfig config,
        GameReviewConfig reviewConfig,
        IProgress<(int current, int total)> progress,
        CancellationToken cancellationToken,
        Action<EngineUpdate> engineUpdateListener = null)
    {
        if (IsAnalyzing)
            throw new InvalidOperationException("Analysis already in progress");

        IsAnalyzing = true;
        _cts?.Dispose();
        _cts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        // Taken before the engine load: DisposeAsync can dispose _cts meanwhile (the tab closed),
        // and reading its Token then throws ObjectDisposedException instead of cancelling.
        var ct = _cts.Token;

        try
        {
            var dispatcher = new GameAccuracyAnalysis.EngineUpdateDispatcher();

            var engineCallback = FuncConvert.FromAction<EngineUpdate>(update =>
            {
                dispatcher.Handler(update);
                engineUpdateListener?.Invoke(update);
            });

            // The constructor waits for the network to load (Ceres ~10 s). This method starts on the
            // page's thread, so creating the engine here froze the page and its Cancel button.
            _engine = await EngineHelper.createAltEngineAsync(engineCallback, config, _logger, false);

            // Cancelled while the engine loaded: stop here; finally shuts it down.
            ct.ThrowIfCancellationRequested();
            var result = await Task.Run(() =>
            {
                var progressCallback = FuncConvert.FromAction<int, int>((current, total) =>
                {
                    progress.Report((current, total));
                });

                return analyzeGameWithEngine(_engine, dispatcher, game, reviewConfig, progressCallback, ct);
            }, ct);

            return result;
        }
        finally
        {
            IsAnalyzing = false;
            DisposeEngine();
        }
    }

    public GameAnalysisResult AnalyzeGameFromAnnotations(PgnGame game, ClassificationThresholds thresholds = null, double accuracyDecay = 0.065)
    {
        var result = analyzeGameFromAnnotations(thresholds ?? ClassificationThresholds.Default, accuracyDecay, game);
        return OptionModule.IsSome(result) ? result.Value : null;
    }

    public void Cancel()
    {
        _cts?.Cancel();
    }

    private void DisposeEngine()
    {
        try
        {
            if (_engine != null)
            {
                _engine.ShutDownEngine();
                _engine = null;
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Error disposing engine");
        }
    }

    public ValueTask DisposeAsync()
    {
        Cancel();
        DisposeEngine();
        _cts?.Dispose();
        return ValueTask.CompletedTask;
    }
}
