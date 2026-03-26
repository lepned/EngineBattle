using ChessLibrary;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.TypesDef.CoreTypes;
using static ChessLibrary.GameAccuracyAnalysis;
using Microsoft.Extensions.Logging;
using Microsoft.FSharp.Core;

namespace WebGUI.Services;

public class GameReviewService : IAsyncDisposable
{
    private ChessLibrary.Engine.ChessEngine _engine;
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
        CancellationToken cancellationToken)
    {
        if (IsAnalyzing)
            throw new InvalidOperationException("Analysis already in progress");

        IsAnalyzing = true;
        _cts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);

        try
        {
            _engine = EngineHelper.createEngine(config, null);

            var ct = _cts.Token;
            var result = await Task.Run(() =>
            {
                var callback = FuncConvert.FromAction<int, int>((current, total) =>
                {
                    progress.Report((current, total));
                });

                return analyzeGameWithEngine(_engine, game, reviewConfig, callback, ct);
            }, ct);

            return result;
        }
        finally
        {
            IsAnalyzing = false;
            DisposeEngine();
        }
    }

    public GameAnalysisResult AnalyzeGameFromAnnotations(PgnGame game)
    {
        var result = analyzeGameFromAnnotations(game);
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
                _engine.Quit();
                _engine.StopProcess();
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
