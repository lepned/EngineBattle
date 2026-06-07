namespace WebGUI.Services;

public class GlobalSettings
{
    // Folder paths
    public string EngineDefsFolder { get; set; } = "";
    public string OpeningsFolder { get; set; } = "";
    public string PgnOutputFolder { get; set; } = "";
    public string TournamentConfigFolder { get; set; } = "";
    public string TablebaseFolder { get; set; } = "";
    public string NeuralNetFolder { get; set; } = "";
    public string PuzzleConfigFolder { get; set; } = "";
    public string AnalysisGamesPath { get; set; } = "";

    // Browser state
    public string LastBrowsedPath { get; set; } = "";
    public string[] RecentBrowsePaths { get; set; } = [];

    // Tool paths
    public string OrdoExePath { get; set; } = "";

    // Engine defaults
    public string DefaultEnginePath { get; set; } = "";
    public string SecondaryEnginePath { get; set; } = "";

    // Analysis defaults
    public string DefaultSearchMode { get; set; } = "Nodes";  // Nodes, Time
    public double DefaultSearchTimeMs { get; set; } = 3000;
    public int DefaultSearchNodes { get; set; } = 100000;
    public string PolicyDistributionMinMaxFilter { get; set; } = "0.4,0.6";
    public bool CombineWhiteAndBlackMoves { get; set; } = true;
    public int DefaultMultiPV { get; set; } = 10;
    public double MinPolicyThreshold { get; set; } = 0.05;

    // Game Review defaults
    public string ReviewSearchMode { get; set; } = "Time";  // Time, Nodes, Depth
    public int ReviewTimePerMove { get; set; } = 1000;
    public int ReviewNodes { get; set; } = 5000;
    public int ReviewDepth { get; set; } = 18;

    // Game Review accuracy curve & weighting
    public int ReviewMultiPV { get; set; } = 5;                 // PV lines per position (more = better resolution, slower)
    public double AccuracyDecay { get; set; } = 0.085;          // exponential decay (Lichess = 0.04354, higher = harsher)
    public double MicroLossBase { get; set; } = 0.037;          // WP penalty for "best" moves in easy positions
    public double MicroLossScale { get; set; } = 0.20;          // how fast micro-loss shrinks with PV gap

    // Game Review classification thresholds (win probability loss, 0-1 scale)
    public double BrilliantPVGap { get; set; } = 0.15;
    public double BestMinPVGap { get; set; } = 0.05;
    public double GreatPVGap { get; set; } = 0.10;
    public double ExcellentMaxWPLoss { get; set; } = 0.02;
    public double GoodMaxWPLoss { get; set; } = 0.03;
    public double InaccuracyMaxWPLoss { get; set; } = 0.05;
    public double MistakeMaxWPLoss { get; set; } = 0.10;

    // Tournament defaults
    public int DelayBetweenGamesSec { get; set; } = 20;
    public int MoveOverheadMs { get; set; } = 100;

    // Puzzle display
    public bool ShowPuzzleEngineColumn { get; set; } = true;

    // App behavior
    public string StartupPage { get; set; } = "";
}
