using ChessLibrary;
using static ChessLibrary.GameAccuracyAnalysis;
using static ChessLibrary.PGNTypes;
using Microsoft.FSharp.Core;

namespace WebGUI.Services;

/// Quick Review: a game scored from the evals its PGN already carries. The engine review runs on
/// the Game Review page (ReviewModel), which owns its engine.
public class GameReviewService
{
    public GameAnalysisResult AnalyzeGameFromAnnotations(PgnGame game, ClassificationThresholds thresholds = null, double accuracyDecay = 0.065)
    {
        var result = analyzeGameFromAnnotations(thresholds ?? ClassificationThresholds.Default, accuracyDecay, game);
        return OptionModule.IsSome(result) ? result.Value : null;
    }
}
