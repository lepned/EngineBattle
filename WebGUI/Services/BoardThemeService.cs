namespace WebGUI.Services;

/// <summary>Resolved board colors, piece set and board options applied by EbChessboard.</summary>
public record BoardTheme(
    string LightSquare,
    string DarkSquare,
    string HighlightWhite,
    string HighlightBlack,
    string PieceSet,
    bool TournamentCoordinates = true,
    string CoordFontCss = "min(2cqi, 12px)",
    string CoordColor = "",
    string HighlightStyle = "replace",
    string SelRingColor = "",
    string ArrowColor = "#9D8989",
    string PolicyLabelColor = "#FFEB3B",
    double ArrowWidthScale = 1.0,
    string PolicyLabelStyle = "circle",
    string PolicyIndicator = "arrow",
    string PolicyIndicatorColor = "black",
    double PolicyIndicatorOpacity = 0.25,
    string PolicyLabelBg = "rgba(60, 60, 60, 0.8)",
    double PolicyLabelFontPx = 12,
    double PolicyLabelChipPct = 0.55,
    string WhiteMoveArrowColor = "#cfece0",
    string BlackMoveArrowColor = "#383231");

/// <summary>
/// Singleton providing the current board theme, resolved from GlobalSettings
/// (preset key or custom colors). EbChessboard subscribes to ThemeChanged so every
/// board in the app follows a settings change immediately.
/// </summary>
public class BoardThemeService
{
    // Preset colors. "eb-blue" is EngineBattle's classic look (pre-native-board defaults);
    // "lichess" matches lichess.org's brown board and last-move highlight.
    private static readonly Dictionary<string, (string Name, string Light, string Dark, string HlWhite, string HlBlack)> presets = new()
    {
        ["eb-blue"] = ("EB Classic Blue", "#B1D8DB", "#619EB3", "rgba(250, 250, 210, 0.8)", "rgba(238, 232, 170, 0.8)"),
        ["lichess"] = ("Lichess Brown", "#F0D9B5", "#B58863", "rgba(155, 199, 0, 0.41)", "rgba(155, 199, 0, 0.41)"),
        ["green"] = ("Tournament Green", "#EBECD0", "#739552", "rgba(255, 255, 51, 0.5)", "rgba(255, 255, 51, 0.5)"),
        ["grey"] = ("Slate Grey", "#DEE3E6", "#8CA2AD", "rgba(155, 199, 0, 0.41)", "rgba(155, 199, 0, 0.41)"),
        ["wood"] = ("Wood Brown", "#E8C99B", "#A5764C", "rgba(255, 215, 105, 0.55)", "rgba(255, 215, 105, 0.55)"),
        ["purple"] = ("Royal Purple", "#E8E4F0", "#8E7CB0", "rgba(255, 255, 51, 0.45)", "rgba(255, 255, 51, 0.45)"),
        ["midnight"] = ("Midnight Blue", "#8CA2C0", "#4A5A78", "rgba(160, 200, 255, 0.45)", "rgba(160, 200, 255, 0.45)"),
    };

    public static IReadOnlyList<(string Key, string Name)> Presets { get; } =
        presets.Select(kv => (kv.Key, kv.Value.Name)).Concat(new[] { ("custom", "Custom") }).ToList();

    /// <summary>Preset swatch colors for the appearance page's theme cards (excludes "custom").</summary>
    public static IReadOnlyList<(string Key, string Name, string Light, string Dark)> PresetSwatches { get; } =
        presets.Select(kv => (kv.Key, kv.Value.Name, kv.Value.Light, kv.Value.Dark)).ToList();

    private readonly GlobalSettingsService settingsService;
    private readonly string piecesRoot;

    public BoardTheme Current { get; private set; }

#nullable enable
    public event Action? ThemeChanged;
#nullable restore

    public BoardThemeService(GlobalSettingsService settingsService, IWebHostEnvironment env)
    {
        this.settingsService = settingsService;
        piecesRoot = Path.Combine(env.WebRootPath, "pieces");
        Current = Resolve(settingsService.Settings);
        settingsService.OnSettingsChanged += OnSettingsChanged;
    }

    private void OnSettingsChanged()
    {
        var resolved = Resolve(settingsService.Settings);
        if (resolved != Current)
        {
            Current = resolved;
            ThemeChanged?.Invoke();
        }
    }

    /// <summary>Opacity of the policy-move indicators (arrows/lines/dots) per prominence key.</summary>
    public static double PolicyIndicatorOpacity(string key) => key switch
    {
        "medium" => 0.45,
        "strong" => 0.65,
        _ => 0.25,
    };

    /// <summary>Policy label font size (px) per size key.</summary>
    public static double PolicyLabelFontPx(string key) => key switch
    {
        "small" => 10,
        "large" => 15,
        _ => 12,
    };

    /// <summary>Policy label chip diameter (fraction of the square) per size key.</summary>
    public static double PolicyLabelChipPct(string key) => key switch
    {
        "small" => 0.48,
        "large" => 0.63,
        _ => 0.55,
    };

    /// <summary>Policy label background: a plain #rrggbb from the color picker gets the
    /// default's 80% alpha appended; other values (hand-edited) pass through untouched.</summary>
    public static string PolicyLabelBg(string customHex) =>
        string.IsNullOrEmpty(customHex) ? "rgba(60, 60, 60, 0.8)"
        : customHex.Length == 7 && customHex[0] == '#' ? customHex + "CC"
        : customHex;

    /// <summary>Arrow stroke/head scale per setting key (multiplies every arrow's width).</summary>
    public static double ArrowWidthScale(string key) => key switch
    {
        "thin" => 0.7,
        "thick" => 1.35,
        "xthick" => 1.7,
        _ => 1.0,
    };

    /// <summary>Coordinate font size per setting key: proportional on small boards, capped on large.</summary>
    public static string CoordSizeCss(string key) => key switch
    {
        "small" => "min(1.5cqi, 9px)",
        "large" => "min(3cqi, 16px)",
        "xlarge" => "min(4cqi, 22px)",
        _ => "min(2cqi, 12px)",
    };

    public static BoardTheme Resolve(GlobalSettings s)
    {
        var pieceSet = string.IsNullOrWhiteSpace(s.BoardPieceSet) ? "wikipedia" : s.BoardPieceSet;
        var highlightStyle = s.BoardHighlightStyle is "tint" or "frame" ? s.BoardHighlightStyle : "replace";
        var selRing = s.BoardSelectionRingColor ?? "";
        var arrow = string.IsNullOrEmpty(s.BoardArrowColor) ? "#9D8989" : s.BoardArrowColor;
        var policyLabel = string.IsNullOrEmpty(s.BoardPolicyLabelColor) ? "#FFEB3B" : s.BoardPolicyLabelColor;
        var arrowWidth = ArrowWidthScale(s.BoardArrowWidth);
        var policyStyle = s.BoardPolicyLabelStyle == "plain" ? "plain" : "circle";
        var policyIndicator = s.BoardPolicyIndicator == "none" ? "none" : "line";
        var policyArrow = string.IsNullOrEmpty(s.BoardPolicyIndicatorColor) ? "black" : s.BoardPolicyIndicatorColor;
        var policyArrowOp = PolicyIndicatorOpacity(s.BoardPolicyIndicatorProminence);
        var policyBg = PolicyLabelBg(s.BoardPolicyLabelBgColor);
        var policyFont = PolicyLabelFontPx(s.BoardPolicyLabelSize);
        var policyChip = PolicyLabelChipPct(s.BoardPolicyLabelSize);
        var whiteMoveArrow = string.IsNullOrEmpty(s.BoardWhiteMoveArrowColor) ? "#cfece0" : s.BoardWhiteMoveArrowColor;
        var blackMoveArrow = string.IsNullOrEmpty(s.BoardBlackMoveArrowColor) ? "#383231" : s.BoardBlackMoveArrowColor;

        string light, dark, hlWhite, hlBlack;
        if (s.BoardThemePreset == "custom")
        {
            light = s.BoardCustomLightColor ?? "#B1D8DB";
            dark = s.BoardCustomDarkColor ?? "#619EB3";
            // The custom highlight is a hex color from a color input; append 80% alpha
            // so it composes like the preset highlights.
            hlWhite = hlBlack = (s.BoardCustomHighlightColor ?? "#FAFAD2") + "CC";
        }
        else
        {
            var key = presets.ContainsKey(s.BoardThemePreset ?? "") ? s.BoardThemePreset : "eb-blue";
            var p = presets[key];
            (light, dark, hlWhite, hlBlack) = (p.Light, p.Dark, p.HlWhite, p.HlBlack);
        }

        // Standalone move-highlight override: recolors the last-move highlight on any preset.
        if (!string.IsNullOrEmpty(s.BoardMoveHighlightColor))
        {
            var c = s.BoardMoveHighlightColor;
            hlWhite = hlBlack = c.Length == 7 && c[0] == '#' ? c + "CC" : c;
        }

        return new BoardTheme(light, dark, hlWhite, hlBlack, pieceSet, s.ShowTournamentBoardCoordinates,
            CoordSizeCss(s.BoardCoordinateSize), s.BoardCoordinateColor ?? "",
            highlightStyle, selRing, arrow, policyLabel, arrowWidth, policyStyle, policyIndicator, policyArrow,
            policyArrowOp, policyBg, policyFont, policyChip, whiteMoveArrow, blackMoveArrow);
    }

    /// <summary>Resolve a theme from arbitrary values (used by the settings-page live preview).</summary>
    public static BoardTheme Preview(string presetKey, string customLight, string customDark, string customHighlight, string pieceSet, string moveHighlight = "") =>
        Resolve(new GlobalSettings
        {
            BoardThemePreset = presetKey,
            BoardCustomLightColor = customLight,
            BoardCustomDarkColor = customDark,
            BoardCustomHighlightColor = customHighlight,
            BoardPieceSet = pieceSet,
            BoardMoveHighlightColor = moveHighlight,
        });

    /// <summary>Web path to a piece image, probing svg vs png per set (for gallery thumbnails).</summary>
    public string PieceImg(string set, string piece)
    {
        var ext = File.Exists(Path.Combine(piecesRoot, set, "wK.svg")) ? "svg" : "png";
        return $"pieces/{set}/{piece}.{ext}";
    }

    /// <summary>Piece-set folders under wwwroot/pieces (drop in a folder with wK.svg..bP.svg to add one).</summary>
    public IReadOnlyList<string> GetAvailablePieceSets()
    {
        try
        {
            if (!Directory.Exists(piecesRoot)) return new[] { "wikipedia" };
            var sets = Directory.GetDirectories(piecesRoot).Select(Path.GetFileName).OrderBy(n => n).ToArray();
            return sets.Length > 0 ? sets : new[] { "wikipedia" };
        }
        catch
        {
            return new[] { "wikipedia" };
        }
    }
}
