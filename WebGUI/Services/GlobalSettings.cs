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
    public bool ShowEvalBar { get; set; } = true;             // vertical eval bar beside analysis/review boards

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

    // Board theme
    public string BoardThemePreset { get; set; } = "eb-blue";   // preset key or "custom"
    public string BoardCustomLightColor { get; set; } = "#B1D8DB";
    public string BoardCustomDarkColor { get; set; } = "#619EB3";
    public string BoardCustomHighlightColor { get; set; } = "#FAFAD2";
    public string BoardPieceSet { get; set; } = "wikipedia";
    public int BoardPieceScale { get; set; } = 100;              // piece size as % of the square (80–100)
    public bool BoardAnimateMoves { get; set; } = true;
    // Coordinates on tournament boards (streaming, PV duo, tile boards).
    public bool ShowTournamentBoardCoordinates { get; set; } = true;
    public string BoardCoordinateSize { get; set; } = "medium";  // small | medium | large | xlarge
    public string BoardCoordinateColor { get; set; } = "";       // "" = auto (opposite square color)
    public string BoardHighlightStyle { get; set; } = "replace"; // replace | tint | frame
    public string BoardMoveHighlightColor { get; set; } = "";    // "" = theme's highlight; hex = override on any preset
    public string BoardSelectionRingColor { get; set; } = "";    // "" = default green ring
    public string BoardArrowColor { get; set; } = "";            // "" = default (#9D8989)
    public string BoardArrowWidth { get; set; } = "normal";      // thin | normal | thick | xthick
    // Dual PV arrows when both engines' best moves are shown (streaming board):
    public string BoardWhiteMoveArrowColor { get; set; } = "";   // "" = default (#cfece0)
    public string BoardBlackMoveArrowColor { get; set; } = "";   // "" = default (#383231)
    public string BoardPolicyLabelColor { get; set; } = "";      // "" = default (#FFEB3B)
    public string BoardPolicyLabelStyle { get; set; } = "circle"; // circle | plain
    // Default "line": with the circle label style the line plugs into the labeled
    // badge, which is EB's signature policy-overlay look.
    public string BoardPolicyIndicator { get; set; } = "line";    // line | none
    public string BoardPolicyIndicatorColor { get; set; } = "";   // "" = default (faint black)
    public string BoardPolicyIndicatorProminence { get; set; } = "faint"; // faint | medium | strong
    public string BoardPolicyLabelBgColor { get; set; } = "";     // "" = default dark circle/pill
    public string BoardPolicyLabelSize { get; set; } = "medium";  // small | medium | large

    // App behavior
    public string StartupPage { get; set; } = "";
}
