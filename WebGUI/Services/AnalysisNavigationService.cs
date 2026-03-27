using static ChessLibrary.PGNTypes;
using static ChessLibrary.GameAccuracyAnalysis;

namespace WebGUI.Services;

public class AnalysisNavigationService
{
    // SingleAnalysis navigation
    public PgnGame Game { get; set; }
    public int SelectedPly { get; set; }

    // GameReview state preservation
    public List<PgnGame> ReviewGames { get; set; }
    public PgnGame ReviewCurrentGame { get; set; }
    public GameAnalysisResult ReviewAnalysisResult { get; set; }
    public int ReviewSelectedPly { get; set; }
    public string ReviewEngineName { get; set; }

    public void Clear()
    {
        Game = null;
        SelectedPly = 0;
    }

    public void ClearReviewState()
    {
        ReviewGames = null;
        ReviewCurrentGame = null;
        ReviewAnalysisResult = null;
        ReviewSelectedPly = 0;
        ReviewEngineName = null;
    }
}
