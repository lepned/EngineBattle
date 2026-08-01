using System.IO;
using System.Linq;

namespace WebGUI.Services;

/// <summary>
/// Deciding whether a picked file holds games or positions. One browse dialog covers both —
/// the file browser accepts several patterns separated by ';' — so the format is worked out
/// from the file afterwards instead of making the user pick a kind before picking a file.
/// </summary>
public static class GameFileFormat
{
    /// <summary>Filter that offers PGN and EPD in the same dialog.</summary>
    public const string BrowseFilter = "*.pgn;*.epd";

    /// <summary>
    /// True for PGN. The extension decides when it is one of the two known ones. Both formats
    /// also turn up under .txt and .fen, and there the first non-empty line decides: a PGN
    /// opens with a tag pair, an EPD line opens with a board. A file that cannot be read falls
    /// to PGN, whose parser fails more gracefully.
    /// </summary>
    public static bool IsPgn(string path)
    {
        switch (Path.GetExtension(path).ToLowerInvariant())
        {
            case ".pgn": return true;
            case ".epd": return false;
        }

        try
        {
            foreach (var line in File.ReadLines(path).Take(20))
            {
                var trimmed = line.Trim();
                if (trimmed.Length == 0) continue;
                return trimmed.StartsWith('[');
            }
        }
        catch
        {
            // Unreadable here means the parser will hit the same wall with a better message.
        }

        return true;
    }
}
