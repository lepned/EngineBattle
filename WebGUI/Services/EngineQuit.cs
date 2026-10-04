using ChessLibrary;

namespace WebGUI.Services;

/// Quitting an analysis engine waits for its process (seconds): the pages never do it on their own thread.
public static class EngineQuit
{
    public static void OffThread(AnalysisManager.SimpleEngineAnalyzer analyzer, ILogger logger)
    {
        if (analyzer != null) Run(analyzer.Quit, logger);
    }

    public static void OffThread(AnalysisEngine engine, ILogger logger)
    {
        if (engine != null) Run(engine.Quit, logger);
    }

    private static void Run(Action quit, ILogger logger) =>
        _ = Task.Run(() =>
        {
            try { quit(); }
            catch (Exception ex) { logger.LogWarning("Engine quit failed: {Msg}", ex.Message); }
        });
}
