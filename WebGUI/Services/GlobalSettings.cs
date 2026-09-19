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
    // Where puzzle runs write their results. The trend view reads the
    // LichessSummary_<stamp>.json files from here.
    public string PuzzleResultsFolder { get; set; } = "";
    public string AnalysisGamesPath { get; set; } = "";

    // Browser state
    public string LastBrowsedPath { get; set; } = "";
    /// The engine def last chosen on the Lc0 Contempt page; remembered silently, like LastBrowsedPath.
    public string ContemptEnginePath { get; set; } = "";
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
    // Rollout switch, not a permanent fork: the old InfoBanner is kept only until the new
    // header has been through enough real broadcasts, then it goes. Off by default while
    // the new one is still being built — the shipped default should be the proven page.
    public bool UseNewTournamentBanner { get; set; } = false; // true = new tournament header

    // Remembered last choice of the below-chart panel (live move stats vs PV table),
    // keyed per host page ("single-analysis", "dual-analysis"). No settings UI — written
    // when the user flips the in-panel toggle; applied only for engines with LogLiveStats.
    public Dictionary<string, bool> ShowLiveMoveStatsPanel { get; set; } = new();

    // Position-insights board overlay (pins/checks/king danger), keyed per host page.
    // Values: "pins", "danger", "pins+danger"; absent = off. No settings UI — written
    // when the user flips the checkboxes next to the board.
    public Dictionary<string, string> PositionInsightsOverlay { get; set; } = new();

    // Height in px of the iteration-log table (the scrolling part only), set with the slider
    // on the card itself. No settings UI — how many depths you want in view is a per-user
    // habit, not a configuration decision.
    public int IterationLogHeight { get; set; } = 200;

    // Iteration log: show the Nodes/T columns as per-iteration cost instead of totals.
    public bool IterationLogPerIteration { get; set; }
    public int CandidateMovesHeight { get; set; } = 320;

    // Board width in px on the analysis pages (360-900), set with the slider above the
    // board. The board holds this size while the window is resized — the side panels
    // absorb the change — and only shrinks below it when the column can no longer fit it.
    // Used as the default; pages that carry a key keep their own size below, since a dual
    // board with two engine panels wants a different size than a single one.
    public int BoardSizePx { get; set; } = 700;

    public Dictionary<string, int> BoardSizePxByPage { get; set; } = new();

    /// <summary>Board width for a page key, falling back to the shared default.</summary>
    public int BoardSizeFor(string key) =>
        !string.IsNullOrEmpty(key) && BoardSizePxByPage.TryGetValue(key, out var px) ? px : BoardSizePx;

    // Main tournament board width in px. 0 = fill its column, which is what the page did
    // before the slider existed — kept separate from BoardSizePx because the tournament
    // layout is a broadcast layout with different needs.
    public int TournamentBoardSizePx { get; set; }

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

    // Interface. "normal" means whatever the stylesheets do on their own, so a settings file
    // from before these existed renders exactly as it did.
    public string UiFontScale { get; set; } = "normal";        // small | normal | large | xlarge - root font size
    public string NavDrawerWidth { get; set; } = "normal";     // narrow | normal | wide - the menu on the left

    // Tournament text scale: the user's one nudge, multiplied into the ceiling of every
    // region that opts in (docs/FontScalingPlan.md). Keyed by screen bucket - "1920x1080@1.5" -
    // because the right nudge on a laptop panel is the wrong one on the 4K monitor it docks to,
    // and being asked to redo it on every dock is the fiddling this feature exists to remove.
    // TournamentFontScale is what a screen with no entry of its own gets.
    public double TournamentFontScale { get; set; } = 1.0;
    public Dictionary<string, double> TournamentFontScaleByScreen { get; set; } = new();

    // Per-region nudges, by group key ("standings", "crosstable", ...). A group with no entry
    // follows the global number, which is the case for everyone who never opens Advanced.
    public Dictionary<string, double> TournamentFontScaleByGroup { get; set; } = new();

    // Chart height on the tournament page, as a multiplier on the two numbers in
    // tournament.json (LayoutOption.Sizes.LiveChartHeight and MoveChartHeight). Same bargain as
    // the text nudge and kept per screen for the same reason: how many charts fit above the
    // fold is a property of the screen, not of the tournament.
    public double TournamentChartScale { get; set; } = 1.0;
    public Dictionary<string, double> TournamentChartScaleByScreen { get; set; } = new();

    /// <summary>The chart scale for a screen, falling back to the shared default.</summary>
    public double ChartScaleFor(string screenKey) =>
        ClampChartScale(!string.IsNullOrEmpty(screenKey)
                        && TournamentChartScaleByScreen.TryGetValue(screenKey, out var v)
                        ? v : TournamentChartScale);

    /// A wider range than the text nudge: a chart can be halved and still read, and doubling one
    /// is a reasonable thing to want when there is only one on screen.
    public static double ClampChartScale(double v) => v is >= 0.5 and <= 2.0 ? v : 1.0;

    // The two PV boards under the engine panel: "off", "small", "medium", "large", or "" to
    // use whatever tournament.json says. An override rather than a replacement, because the
    // file's answer is right for a two-engine broadcast and wrong the moment several boards
    // run at once and the row has nowhere to go. "off" removes the row, it does not hide it.
    public string TournamentPvBoard { get; set; } = "";

    /// <summary>The scale for a screen, falling back to the shared default.</summary>
    public double FontScaleFor(string screenKey) =>
        ClampFontScale(!string.IsNullOrEmpty(screenKey)
                       && TournamentFontScaleByScreen.TryGetValue(screenKey, out var v)
                       ? v : TournamentFontScale);

    /// <summary>The scale for one region, falling back to that screen's number.</summary>
    public double FontScaleFor(string screenKey, string group) =>
        !string.IsNullOrEmpty(group) && TournamentFontScaleByGroup.TryGetValue(group, out var g)
            ? ClampFontScale(g) : FontScaleFor(screenKey);

    /// Out-of-range, zero and NaN all mean "no nudge" rather than an unreadable page.
    public static double ClampFontScale(double v) => v is >= 0.6 and <= 1.6 ? v : 1.0;

    /// <summary>
    /// The regions of the tournament page that can be nudged, and written back, on their own.
    ///
    /// One array, three readers: Appearance builds a slider per entry, MainLayout turns the
    /// saved numbers into CSS rules, and "save sizes to tournament.json" knows which fields to
    /// write. Keeping them here is also what keeps a hand-edited settings file from reaching a
    /// stylesheet - a group name that is not in this list is simply not a group.
    ///
    /// Selector is the element that ends up CARRYING the size, and is null for the regions
    /// whose size needs no measuring: nothing clamps them, so the size on screen is exactly
    /// the ceiling times the nudge and C# can work it out without asking the browser.
    ///
    /// JsonFields may be EMPTY, which means the group can be nudged but not written back. That
    /// is the cup and ladder progress tables: what is on screen there is fed from StandingsFont,
    /// not from CupBracketFont, so writing the measured size into the three bracket fields would
    /// overwrite three separately tuned settings with the standings size.
    /// </summary>
    public sealed record FontGroup(string Key, string Label, string Selector, string[] JsonFields);

    public static readonly FontGroup[] FontGroups =
    {
        new("standings",   "Standings",       ".eb-g-standings .data-cell",  ["StandingsFont"]),
        new("crosstable",  "Crosstable",      ".eb-g-crosstable .data-cell", ["CrossTableFont"]),
        new("pairings",    "Pairings",        ".eb-g-pairings .data-cell",   ["PairingsFont"]),
        new("latest",      "Latest games",    ".eb-g-latest .data-cell",     ["LatestGamesFont"]),
        new("brackets",    "Cup and ladder",  ".eb-g-brackets .data-cell", []),
        new("movelist",    "Move list",       null, ["MoveListFont"]),
        new("enginepanel", "Engine panel",    null, ["EnginesPanelFont"]),
        new("banner",      "Header banner",   null, ["InfoBannerFont"]),
        new("description", "Description",     null, ["TournamentDescFont"]),
        new("pv",          "PV lines",        null, ["PVLabelFont"]),
    };

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
    public string BoardCoordinatePlacement { get; set; } = "inside"; // inside | outside (in the frame gutter)
    public string EvalBarPlacement { get; set; } = "left";       // left | right — side of the board (dual analysis always uses both)
    public string BoardFrameWidth { get; set; } = "medium";      // off | thin | medium | thick — frame around the board
    public string BoardFrameColor { get; set; } = "";            // "" = default dark frame
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

/// CSS for the interface-scale settings. Null means "emit nothing": the stylesheets' own
/// default, which is what every installation had before the settings existed.
public static class UiScale
{
    // Percent of the browser's default rather than px, so a user who already enlarged fonts
    // in the browser keeps that ratio. MudBlazor and the tool pages size text in rem, so this
    // scales all of it; the chessboards are sized in px on purpose and stay put.
    public static string FontSizeCss(string scale) => scale switch
    {
        "small" => "87.5%",
        "large" => "112.5%",
        "xlarge" => "125%",
        _ => null
    };

    // MudBlazor's own default is 240px.
    public static string DrawerWidthCss(string width) => width switch
    {
        "narrow" => "200px",
        "wide" => "300px",
        _ => null
    };
}
