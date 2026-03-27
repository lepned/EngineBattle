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
    public double DefaultSearchTimeMs { get; set; } = 3000;
    public int DefaultSearchNodes { get; set; } = 100000;
    public int ChartLines { get; set; } = 4;
    public string PolicyDistributionMinMaxFilter { get; set; } = "0.4,0.6";
    public bool CombineWhiteAndBlackMoves { get; set; } = true;
    public int DefaultMultiPV { get; set; } = 10;
    public double MinPolicyThreshold { get; set; } = 0.05;

    // Game Review defaults
    public string ReviewSearchMode { get; set; } = "Time";  // Time, Nodes, Depth
    public int ReviewTimePerMove { get; set; } = 1000;
    public int ReviewNodes { get; set; } = 5000;
    public int ReviewDepth { get; set; } = 18;

    // Tournament defaults
    public int DelayBetweenGamesSec { get; set; } = 20;
    public int MoveOverheadMs { get; set; } = 100;

    // App behavior
    public string StartupPage { get; set; } = "";
}
