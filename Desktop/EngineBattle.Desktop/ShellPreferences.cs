using System.IO;
using System.Text.Json;

namespace EngineBattle.Desktop;

/// <summary>
/// The handful of settings that belong to the window rather than to EngineBattle itself, kept
/// beside the saved window placement. The app's own settings live in the server's
/// globalSettings.json and are none of the shell's business.
/// </summary>
internal static class ShellPreferences
{
    private sealed record Saved(double ZoomFactor);

    public const double MinZoom = 0.5;
    public const double MaxZoom = 3.0;

    private static string FilePath => Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
        "EngineBattle", "desktop-preferences.json");

    /// <summary>1.0 when nothing is saved, or when the file is unreadable or out of range.</summary>
    public static double LoadZoom()
    {
        try
        {
            if (!File.Exists(FilePath)) return 1.0;
            var saved = JsonSerializer.Deserialize<Saved>(File.ReadAllText(FilePath));
            if (saved is null) return 1.0;
            return saved.ZoomFactor >= MinZoom && saved.ZoomFactor <= MaxZoom ? saved.ZoomFactor : 1.0;
        }
        catch
        {
            return 1.0;   // a broken preferences file must never stop the app from starting
        }
    }

    public static void SaveZoom(double zoom)
    {
        try
        {
            Directory.CreateDirectory(Path.GetDirectoryName(FilePath)!);
            File.WriteAllText(FilePath, JsonSerializer.Serialize(new Saved(zoom)));
        }
        catch { /* not worth interrupting the user over */ }
    }
}
