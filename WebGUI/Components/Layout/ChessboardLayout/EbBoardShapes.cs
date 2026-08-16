namespace WebGUI.Components.Layout.ChessboardLayout;

/// <summary>Arrow drawn on the board overlay, e.g. "f3" -> "g5". Width is a multiplier on the
/// default stroke. Head selects the destination marker: "arrow" (default) or "line"
/// (a plain line, no marker).</summary>
public record EbArrow(string From, string To, string Color, double Opacity = 0.7, double Width = 1.0, string Head = "arrow");

/// <summary>Circle on a square. Size is the diameter as a fraction of the square (0..1).
/// StrokeWidth above zero draws a ring of that width instead of a filled disc, in the same
/// square units — a ring keeps the piece underneath readable, which is what a PGN
/// <c>[%csl]</c> highlight wants; insights stay filled so the two sources are told apart at
/// a glance.</summary>
public record EbCircle(string Square, string Color, double Opacity = 0.5, double Size = 0.8, double StrokeWidth = 0);

/// <summary>Round text chip on a square (policy percentages, eval badges).
/// SizePct is the chip diameter as a fraction of the square. FontPx pins the font to a
/// fixed pixel size (legacy look); null auto-scales with the board. BorderCss adds a chip
/// border (e.g. "2px solid yellow" for the legacy ponder badge). FitText lets the chip
/// stretch horizontally into a pill when the text is wider than the circle (so a long
/// value like 100.2K stays inside its background).</summary>
public record EbSquareLabel(string Square, string Text, string TextColor = "#ffffff", string BgColor = "#3c3c3c",
    double SizePct = 0.62, double? FontPx = null, string BorderCss = null, double Opacity = 0.9, bool FitText = false);
