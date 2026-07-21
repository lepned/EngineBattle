namespace WebGUI.Services;

/// <summary>Resolved board colors, piece set and board options applied by EbChessboard.</summary>
public record BoardTheme(
    string LightSquare,
    string DarkSquare,
    string HighlightWhite,
    string HighlightBlack,
    string PieceSet,
    bool TournamentCoordinates = true,
    string CoordFontCss = "min(2cqi, 12px)");

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
    };

    public static IReadOnlyList<(string Key, string Name)> Presets { get; } =
        presets.Select(kv => (kv.Key, kv.Value.Name)).Concat(new[] { ("custom", "Custom") }).ToList();

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
        if (s.BoardThemePreset == "custom")
        {
            // The custom highlight is a hex color from a color input; append 80% alpha
            // so it composes like the preset highlights.
            var highlight = (s.BoardCustomHighlightColor ?? "#FAFAD2") + "CC";
            return new BoardTheme(
                s.BoardCustomLightColor ?? "#B1D8DB",
                s.BoardCustomDarkColor ?? "#619EB3",
                highlight, highlight, pieceSet, s.ShowTournamentBoardCoordinates, CoordSizeCss(s.BoardCoordinateSize));
        }

        var key = presets.ContainsKey(s.BoardThemePreset ?? "") ? s.BoardThemePreset : "eb-blue";
        var p = presets[key];
        return new BoardTheme(p.Light, p.Dark, p.HlWhite, p.HlBlack, pieceSet, s.ShowTournamentBoardCoordinates, CoordSizeCss(s.BoardCoordinateSize));
    }

    /// <summary>Resolve a theme from arbitrary values (used by the settings-page live preview).</summary>
    public static BoardTheme Preview(string presetKey, string customLight, string customDark, string customHighlight, string pieceSet) =>
        Resolve(new GlobalSettings
        {
            BoardThemePreset = presetKey,
            BoardCustomLightColor = customLight,
            BoardCustomDarkColor = customDark,
            BoardCustomHighlightColor = customHighlight,
            BoardPieceSet = pieceSet,
        });

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
