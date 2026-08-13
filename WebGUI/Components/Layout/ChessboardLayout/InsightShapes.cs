using ChessLibrary;

namespace WebGUI.Components.Layout.ChessboardLayout;

/// <summary>Converts <see cref="BoardUtils.PositionInsights"/> (pins, checks, king danger —
/// see BoardUtils.getPositionInsights) into EbChessboard overlay shapes. Shared by the
/// /boardtest prototype page and the SingleAnalysis insights overlay.</summary>
public static class InsightShapes
{
    public const string PinColor = "#E67E22";
    public const string CheckColor = "#D63031";
    public const string EscapeColor = "#27AE60";

    private static IEnumerable<BoardUtils.SideInsights> BothSides(BoardUtils.PositionInsights ins)
    {
        yield return ins.White;
        yield return ins.Black;
    }

    /// <summary>Pin rays (attacker→pinned solid, pinned→king thin line) and checker→king arrows.</summary>
    public static IReadOnlyList<EbArrow> Arrows(BoardUtils.PositionInsights ins, bool pinsChecks)
    {
        if (ins == null || !pinsChecks) return Array.Empty<EbArrow>();
        var list = new List<EbArrow>();
        foreach (var side in BothSides(ins))
        {
            if (string.IsNullOrEmpty(side.King)) continue;   // kingless side (test positions)
            foreach (var pin in side.Pins)
            {
                list.Add(new EbArrow(pin.Attacker, pin.Pinned, PinColor, 0.75));
                list.Add(new EbArrow(pin.Pinned, pin.King, PinColor, 0.3, 0.8, "line"));
            }
            foreach (var checker in side.Checkers)
                list.Add(new EbArrow(checker, side.King, CheckColor, 0.6));
        }
        return list;
    }

    /// <summary>Pinned/checker/checked-king discs and block dots (pinsChecks), plus red
    /// danger tint and green escape dots around each king (dangerZone).</summary>
    public static IReadOnlyList<EbCircle> Circles(BoardUtils.PositionInsights ins, bool pinsChecks, bool dangerZone)
    {
        if (ins == null || (!pinsChecks && !dangerZone)) return Array.Empty<EbCircle>();
        var list = new List<EbCircle>();
        foreach (var side in BothSides(ins))
        {
            if (string.IsNullOrEmpty(side.King)) continue;
            if (pinsChecks)
            {
                foreach (var pin in side.Pins)
                    list.Add(new EbCircle(pin.Pinned, PinColor, 0.35, 0.95));
                foreach (var checker in side.Checkers)
                    list.Add(new EbCircle(checker, CheckColor, 0.45, 0.9));
                foreach (var block in side.CheckBlockSquares)
                    list.Add(new EbCircle(block, CheckColor, 0.18, 0.45));
                if (side.InCheck)
                    list.Add(new EbCircle(side.King, CheckColor, 0.35, 0.95));
            }
            if (dangerZone)
            {
                foreach (var danger in side.KingDangerSquares)
                    list.Add(new EbCircle(danger, CheckColor, 0.25, 0.95));
                foreach (var escape in side.KingEscapeSquares)
                    list.Add(new EbCircle(escape, EscapeColor, 0.5, 0.4));
            }
        }
        return list;
    }
}
