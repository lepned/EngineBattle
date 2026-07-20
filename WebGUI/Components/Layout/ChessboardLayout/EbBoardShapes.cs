namespace WebGUI.Components.Layout.ChessboardLayout;

/// <summary>Arrow drawn on the board overlay, e.g. "f3" -> "g5". Width is a multiplier on the default stroke.</summary>
public record EbArrow(string From, string To, string Color, double Opacity = 0.7, double Width = 1.0);

/// <summary>Filled circle on a square. Size is the diameter as a fraction of the square (0..1).</summary>
public record EbCircle(string Square, string Color, double Opacity = 0.5, double Size = 0.8);

/// <summary>Round text chip on a square (policy percentages, eval badges).</summary>
public record EbSquareLabel(string Square, string Text, string TextColor = "#ffffff", string BgColor = "#3c3c3c");
