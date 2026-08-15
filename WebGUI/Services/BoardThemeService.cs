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
    string BlackMoveArrowColor = "#383231",
    double PieceScale = 1.0,
    string CoordPlacement = "inside",
    double FramePct = 0,
    string FrameColor = "#3B3733",
    string EvalBarPlacement = "left",
    string EvalBarDark = "#3d4148",
    string EvalBarLight = "#e8e8e4");

/// <summary>
/// Singleton providing the current board theme, resolved from GlobalSettings
/// (preset key or custom colors). EbChessboard subscribes to ThemeChanged so every
/// board in the app follows a settings change immediately.
/// </summary>
public class BoardThemeService
{
    // Preset colors. "eb-blue" is EngineBattle's classic look (pre-native-board defaults);
    // "lichess" matches lichess.org's brown board and last-move highlight. Frame is the
    // board-frame default for the preset: its dark square darkened and slightly desaturated.
    private static readonly Dictionary<string, (string Name, string Light, string Dark, string HlWhite, string HlBlack, string Frame)> presets = new()
    {
        ["eb-blue"] = ("EB Classic Blue", "#B1D8DB", "#619EB3", "rgba(250, 250, 210, 0.8)", "rgba(238, 232, 170, 0.8)", "#35505C"),
        ["lichess"] = ("Lichess Brown", "#F0D9B5", "#B58863", "rgba(155, 199, 0, 0.41)", "rgba(155, 199, 0, 0.41)", "#4A3526"),
        ["green"] = ("Tournament Green", "#EBECD0", "#739552", "rgba(255, 255, 51, 0.5)", "rgba(255, 255, 51, 0.5)", "#37502C"),
        ["grey"] = ("Slate Grey", "#DEE3E6", "#8CA2AD", "rgba(155, 199, 0, 0.41)", "rgba(155, 199, 0, 0.41)", "#3F4A50"),
        ["wood"] = ("Wood Brown", "#E8C99B", "#A5764C", "rgba(255, 215, 105, 0.55)", "rgba(255, 215, 105, 0.55)", "#54381F"),
        ["purple"] = ("Royal Purple", "#E8E4F0", "#8E7CB0", "rgba(255, 255, 51, 0.45)", "rgba(255, 255, 51, 0.45)", "#443A5C"),
        ["midnight"] = ("Midnight Blue", "#8CA2C0", "#4A5A78", "rgba(160, 200, 255, 0.45)", "rgba(160, 200, 255, 0.45)", "#252E40"),
    };

    /// <summary>The preset's default board-frame color; neutral dark for unknown keys.
    /// A custom theme has no hand-picked frame, so one is derived from its dark square the
    /// same way the presets' were — otherwise the frame and the themed eval bar would sit
    /// on a custom board in a colour taken from nowhere.</summary>
    public static string PresetFrameColor(string presetKey, string customDarkSquare = null) =>
        presets.TryGetValue(presetKey ?? "", out var p) ? p.Frame
        : Darken(customDarkSquare, 0.45) ?? "#3B3733";

    /// <summary>Mixes a #rrggbb toward black by at least `amount`, and further if needed to
    /// come down to `maxLuminance`. The ceiling mirrors Lighten's floor: a user can pick a
    /// pale custom frame colour, and the eval bar's dark half must stay dark against its
    /// light half. Null for non-hex input.</summary>
    private static string Darken(string hex, double amount, double maxLuminance = 255)
    {
        if (hex is not { Length: 7 } || hex[0] != '#'
            || !int.TryParse(hex.AsSpan(1), System.Globalization.NumberStyles.HexNumber,
                             System.Globalization.CultureInfo.InvariantCulture, out var rgb))
            return null;
        int r = (rgb >> 16) & 0xFF, g = (rgb >> 8) & 0xFF, b = rgb & 0xFF;
        // Mixing toward black scales luminance by (1 - a), so the amount that reaches the
        // ceiling is solvable directly.
        double lum = 0.299 * r + 0.587 * g + 0.114 * b;
        double needed = lum <= maxLuminance || lum <= 0 ? 0 : 1 - (maxLuminance / lum);
        double a = Math.Clamp(Math.Max(amount, needed), 0, 1);
        int Mix(int c) => (int)Math.Round(c * (1 - a));
        return $"#{Mix(r):X2}{Mix(g):X2}{Mix(b):X2}";
    }

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

    /// <summary>Frame gutter width (percent of the board's outer width) per setting key.
    /// Bumped from 1.5/2.75/4.0 after user feedback — the first round read as too slim.</summary>
    public static double FramePctOf(string key) => key switch
    {
        "thin" => 1.75,
        "medium" => 3.1,
        "thick" => 4.5,
        _ => 0,
    };

    /// <summary>Minimum gutter width (pct of board width) the outside coordinates need at
    /// a given coordinate-size setting: ~1.1x the size's cqi component, so the text keeps
    /// a margin against both the board edge and the outer frame edge instead of clamping.</summary>
    public static double MinGutterPctFor(string coordSizeKey) => coordSizeKey switch
    {
        "small" => 1.75,
        "large" => 3.3,
        "xlarge" => 4.4,
        _ => 2.2,
    };

    /// <summary>Minimum gutter width (pct of board width) an in-frame eval bar needs to
    /// stay usable — about 15px on a 700px board.</summary>
    public const double MinGutterPctForEvalBar = 2.2;

    /// <summary>Mixes a #rrggbb toward white by at least `amount`, and further if needed to
    /// reach `minLuminance`. Without the floor a custom theme with dark "light" squares
    /// would produce an eval bar whose two halves are hard to tell apart. Non-hex values
    /// pass through untouched.</summary>
    private static string Lighten(string hex, double amount, double minLuminance = 200)
    {
        if (hex is not { Length: 7 } || hex[0] != '#'
            || !int.TryParse(hex.AsSpan(1), System.Globalization.NumberStyles.HexNumber,
                             System.Globalization.CultureInfo.InvariantCulture, out var rgb))
            return hex;
        int r = (rgb >> 16) & 0xFF, g = (rgb >> 8) & 0xFF, b = rgb & 0xFF;
        // Mixing toward white maps luminance L to L + (255 - L) * a, so the amount that
        // reaches the floor is solvable directly.
        double lum = 0.299 * r + 0.587 * g + 0.114 * b;
        double needed = lum >= minLuminance ? 0 : (minLuminance - lum) / (255 - lum);
        double a = Math.Clamp(Math.Max(amount, needed), 0, 1);
        int Mix(int c) => (int)Math.Round(c + (255 - c) * a);
        return $"#{Mix(r):X2}{Mix(g):X2}{Mix(b):X2}";
    }

    /// <summary>Eval-bar colors for a theme. Deliberately NOT the raw square colors: on
    /// several presets those are two mid-tones (EB Classic Blue is #B1D8DB over #619EB3)
    /// and a bar needs contrast to be readable at a glance. The dark side reuses the
    /// frame color — already a darkened, desaturated dark square — and the light side is
    /// the light square pulled toward white with a luminance floor, so the hues stay
    /// recognisably the theme's while the contrast stays high on every preset.</summary>
    public static (string Dark, string Light) EvalBarColors(string frameColor, string lightSquare) =>
        (Darken(frameColor, 0, maxLuminance: 110) ?? "#3d4148", Lighten(lightSquare, 0.55));

    /// <summary>Resolves the frame width/color. Outside coordinates force a gutter into
    /// existence — with the frame off they yield an invisible (transparent) one — and large
    /// coordinate sizes widen any gutter to fit the text. An empty color setting falls back
    /// to the theme's default (per-preset frame color).
    /// The eval bar's own gutter floor is deliberately NOT applied here: it is per board
    /// (only boards that draw a bar need it), so ModernChessboard and StreamingChessboard
    /// apply MinGutterPctForEvalBar themselves. Anything positioned against the frame must
    /// therefore use the board's own resolved width, not this one.</summary>
    public static (double Pct, string Color) ResolveFrame(string widthKey, string colorSetting, string placement, string defaultColor, string coordSizeKey = "medium")
    {
        var pct = FramePctOf(widthKey);
        var color = string.IsNullOrEmpty(colorSetting) ? defaultColor : colorSetting;
        if (placement == "outside")
        {
            if (pct <= 0) (pct, color) = (FramePctOf("medium"), "transparent");
            pct = Math.Max(pct, MinGutterPctFor(coordSizeKey));
        }
        return (pct, color);
    }

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
        var pieceScale = (s.BoardPieceScale is >= 50 and <= 100 ? s.BoardPieceScale : 100) / 100.0;
        var coordPlacement = s.BoardCoordinatePlacement == "outside" ? "outside" : "inside";
        var evalBarPlacement = s.EvalBarPlacement == "right" ? "right" : "left";
        var (framePct, frameColor) = ResolveFrame(s.BoardFrameWidth, s.BoardFrameColor, coordPlacement,
            PresetFrameColor(s.BoardThemePreset, s.BoardCustomDarkColor), s.BoardCoordinateSize);

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

        // The themed bar follows the frame color, which is itself derived from the dark
        // square (or overridden by the user), so a custom frame color carries through.
        // The resolved color can be "transparent" when a gutter was forced into existence
        // with the frame off — fall back to the preset's color so the bar stays visible.
        var evalFrameColor = frameColor is { Length: 7 } && frameColor[0] == '#'
            ? frameColor : PresetFrameColor(s.BoardThemePreset, s.BoardCustomDarkColor);
        var (evalDark, evalLight) = EvalBarColors(evalFrameColor, light);

        return new BoardTheme(light, dark, hlWhite, hlBlack, pieceSet, s.ShowTournamentBoardCoordinates,
            CoordSizeCss(s.BoardCoordinateSize), s.BoardCoordinateColor ?? "",
            highlightStyle, selRing, arrow, policyLabel, arrowWidth, policyStyle, policyIndicator, policyArrow,
            policyArrowOp, policyBg, policyFont, policyChip, whiteMoveArrow, blackMoveArrow, pieceScale,
            coordPlacement, framePct, frameColor, evalBarPlacement, evalDark, evalLight);
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
