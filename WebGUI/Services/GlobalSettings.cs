using System;
namespace WebGUI.Services;

/// <summary>
/// The regions of the tournament page whose text size is set on its own. The strings are the
/// keys the settings and the CSS groups (.eb-g-*) use; the names are what the code reads.
/// </summary>
public static class FontKey
{
    public const string Standings = "standings";
    public const string Crosstable = "crosstable";
    public const string Pairings = "pairings";
    public const string Latest = "latest";
    public const string Brackets = "brackets";
    public const string MoveList = "movelist";
    public const string EnginePanel = "enginepanel";
    public const string Banner = "banner";
    public const string Description = "description";
    public const string Pv = "pv";
}

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

    /// <summary>
    /// The tournament page's sizes for one screen: a text ceiling per region, the two chart
    /// heights and the PV board mode. Every part is optional - a region or a height with no
    /// entry falls back to tournament.json (older configs still carry these) and then to the
    /// built-in default, so a fresh installation and an old config both render as they did.
    ///
    /// Per SCREEN, keyed like the nudges ("1920x1080@1.5"), because these are decisions about
    /// how much fits on a particular monitor, not about the tournament. They are written by the
    /// corner control on the tournament page ("save" bakes what is on screen) and by the
    /// sliders in Appearance.
    /// </summary>
    public sealed class TournamentScreenLayout
    {
        /// Ceiling per region, by group key ("standings", "crosstable", ...), in px.
        public Dictionary<string, int> FontPx { get; set; } = new();
        /// 0 means not set.
        public int LiveChartHeight { get; set; }
        public int MoveChartHeight { get; set; }
        /// "off", "small", "medium", "large", or "" for not set.
        public string PvBoard { get; set; } = "";

        // Which charts and panels the page shows, and how the charts are drawn. Every one is
        // nullable: null means "whatever tournament.json says, or the built-in default" - the
        // same fallback the sizes have - so a screen that never touched a toggle follows the
        // file exactly, and one that did keeps its choice across tournaments.
        public bool? ShowEval { get; set; }
        public bool? ShowNps { get; set; }
        public bool? ShowTime { get; set; }
        public bool? ShowNodes { get; set; }
        /// Nodes per move instead of nodes per second in the standings.
        public bool? UseNpm { get; set; }
        // BestMoveWithPolicy is deliberately NOT here: the feature is not ready to ship, so it
        // stays a flag in tournament.json with no control in the GUI.
        public bool? ShowCrosstableBetweenGames { get; set; }
        /// Where the crosstable goes in the left column: "cycle", "below" or "none"; "" = not set.
        public string CrosstableWithStandings { get; set; } = "";
        /// Lines in the MCTS charts; 0 or null = not set.
        public int? NumberOfLines { get; set; }
        /// Moves further than this from the best Q are left out of the charts; 0 or null = not set.
        public double? Qdiff { get; set; }
        /// Seconds between the tables the standings box cycles through; 0 or null = not set.
        public int? AutoCycleTimeInSec { get; set; }
    }

    public Dictionary<string, TournamentScreenLayout> TournamentLayoutByScreen { get; set; } = new();

    /// The key a screen's layout is stored under. A browser that would not name its screen
    /// gets one shared bucket rather than the empty string.
    public static string ScreenBucket(string screenKey) => string.IsNullOrEmpty(screenKey) ? "*" : screenKey;

    /// <summary>This screen's layout, or null when it has none.</summary>
    public TournamentScreenLayout TournamentLayoutFor(string screenKey) =>
        TournamentLayoutByScreen.TryGetValue(ScreenBucket(screenKey), out var l) ? l : null;

    /// <summary>This screen's layout, created on first write.</summary>
    public TournamentScreenLayout TournamentLayoutForWrite(string screenKey)
    {
        var key = ScreenBucket(screenKey);
        if (!TournamentLayoutByScreen.TryGetValue(key, out var l) || l is null)
            TournamentLayoutByScreen[key] = l = new TournamentScreenLayout();
        l.FontPx ??= new();
        return l;
    }

    /// <summary>The scale for a screen, falling back to the shared default.</summary>
    public double FontScaleFor(string screenKey) =>
        ClampFontScale(!string.IsNullOrEmpty(screenKey)
                       && TournamentFontScaleByScreen.TryGetValue(screenKey, out var v)
                       ? v : TournamentFontScale);

    /// Out-of-range, zero and NaN all mean "no nudge" rather than an unreadable page.
    public static double ClampFontScale(double v) => v is >= 0.6 and <= 1.6 ? v : 1.0;

    /// <summary>
    /// The regions of the tournament page that can be nudged, and written back, on their own.
    ///
    /// One array, three readers: Appearance builds a slider per entry, the tournament page reads
    /// a ceiling per entry (FontCeiling), and "save" on that page measures each one. Keeping
    /// them here is also what keeps a hand-edited settings file from reaching a stylesheet - a
    /// group name that is not in this list is simply not a group.
    ///
    /// Selector is the element that ends up CARRYING the size, and is null for the regions
    /// whose size needs no measuring: nothing clamps them, so the size on screen is exactly
    /// the ceiling times the nudge and C# can work it out without asking the browser.
    ///
    /// </summary>
    public sealed record FontGroup(string Key, string Label, string Selector);

    /// <summary>
    /// The built-in ceiling for a region, from LayoutOption.Default in ChessLibrary - what a
    /// screen with nothing saved and a config that says nothing gets. The three bracket views
    /// (cup, swiss, ladder) are one region on screen and one number here.
    /// </summary>
    /// <summary>
    /// What a tournament.json asks for, per region, or 0 when it says nothing - the block and
    /// every field in it are optional, and a field that is left out deserialises to 0. The
    /// three bracket views (cup, swiss, ladder) are one region on screen: the largest applies.
    /// Read by the tournament page for the size it renders and by Appearance for the number it
    /// shows, so the two cannot disagree about what the file means.
    /// </summary>
    public static int FileFontPx(ChessLibrary.LayoutTypes.LayoutOption layout, string group)
    {
        var f = layout?.Fonts;
        if (f is null) return 0;
        return group switch
        {
            FontKey.Standings => f.StandingsFont,
            FontKey.Crosstable => f.CrossTableFont,
            FontKey.Pairings => f.PairingsFont,
            FontKey.Latest => f.LatestGamesFont,
            FontKey.Brackets => Math.Max(f.CupBracketFont, Math.Max(f.SwissOverviewFont, f.LadderOverviewFont)),
            FontKey.MoveList => f.MoveListFont,
            FontKey.EnginePanel => f.EnginesPanelFont,
            FontKey.Banner => f.InfoBannerFont,
            FontKey.Description => f.TournamentDescFont,
            FontKey.Pv => f.PVLabelFont,
            _ => 0
        };
    }

    public static int DefaultFontPx(string group)
    {
        var f = ChessLibrary.LayoutTypes.LayoutOption.Default.Fonts;
        return group switch
        {
            FontKey.Standings => f.StandingsFont,
            FontKey.Crosstable => f.CrossTableFont,
            FontKey.Pairings => f.PairingsFont,
            FontKey.Latest => f.LatestGamesFont,
            FontKey.Brackets => f.CupBracketFont,
            FontKey.MoveList => f.MoveListFont,
            FontKey.EnginePanel => f.EnginesPanelFont,
            FontKey.Banner => f.InfoBannerFont,
            FontKey.Description => f.TournamentDescFont,
            FontKey.Pv => f.PVLabelFont,
            _ => 14
        };
    }

    public static readonly FontGroup[] FontGroups =
    {
        new(FontKey.Standings, "Standings", ".eb-g-standings .data-cell"),
        new(FontKey.Crosstable, "Crosstable", ".eb-g-crosstable .data-cell"),
        new(FontKey.Pairings, "Pairings", ".eb-g-pairings .data-cell"),
        new(FontKey.Latest, "Latest games", ".eb-g-latest .data-cell"),
        new(FontKey.Brackets, "Cup and ladder", ".eb-g-brackets .data-cell"),
        new(FontKey.MoveList, "Move list", null),
        new(FontKey.EnginePanel, "Engine panel", null),
        new(FontKey.Banner, "Header banner", null),
        new(FontKey.Description, "Description", null),
        new(FontKey.Pv, "PV lines", null),
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
