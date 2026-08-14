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
    public const string HangingColor = "#F1C40F";
    public const string ForkColor = "#9B59B6";
    public const string SkewerColor = "#00A8CC";
    public const string OverloadColor = "#D63C8C";
    public const string DiscoveredColor = "#16A085";
    public const string RemovableColor = "#C0692B";

    private static IEnumerable<BoardUtils.SideInsights> BothSides(BoardUtils.PositionInsights ins)
    {
        yield return ins.White;
        yield return ins.Black;
    }

    /// <summary>Pin rays (attacker→pinned solid, pinned→king thin line), checker→king
    /// arrows, thin attacker→victim lines for hanging pieces, and the tactics family:
    /// fork arrows (forker→targets), skewer/relative-pin rays (attacker→front solid,
    /// front→back thin), and overload lines (defender→defended).</summary>
    public static IReadOnlyList<EbArrow> Arrows(BoardUtils.PositionInsights ins, bool pinsChecks, bool hanging = false, bool tactics = false)
    {
        if (ins == null || (!pinsChecks && !hanging && !tactics)) return Array.Empty<EbArrow>();
        var list = new List<EbArrow>();
        foreach (var side in BothSides(ins))
        {
            if (string.IsNullOrEmpty(side.King)) continue;   // kingless side (test positions)
            if (pinsChecks)
            {
                foreach (var pin in side.Pins)
                {
                    list.Add(new EbArrow(pin.Attacker, pin.Pinned, PinColor, 0.75));
                    list.Add(new EbArrow(pin.Pinned, pin.King, PinColor, 0.3, 0.8, "line"));
                }
                foreach (var checker in side.Checkers)
                    list.Add(new EbArrow(checker, side.King, CheckColor, 0.6));
            }
            if (hanging)
            {
                // The mover's own hanging pieces are warnings (they can still be saved this
                // move) — drawn muted; the opponent's are capturable right now — full strength.
                var mutedOpacity = side.IsSideToMove ? 0.28 : 0.5;
                foreach (var h in side.HangingPieces)
                    foreach (var attacker in h.Attackers)
                        list.Add(new EbArrow(attacker, h.Square, HangingColor, mutedOpacity, 0.8, "line"));
            }
            if (tactics)
            {
                foreach (var f in side.Forks)
                    foreach (var target in f.Targets)
                        list.Add(new EbArrow(f.Forker, target, ForkColor, 0.6));
                foreach (var s in side.Skewers)
                {
                    list.Add(new EbArrow(s.Attacker, s.Front, SkewerColor, 0.65));
                    list.Add(new EbArrow(s.Front, s.Back, SkewerColor, 0.3, 0.8, "line"));
                }
                foreach (var o in side.OverloadedDefenders)
                    foreach (var defended in o.Defends)
                        list.Add(new EbArrow(o.Defender, defended, OverloadColor, 0.45, 0.8, "line"));
                foreach (var d in side.DiscoveredAttacks)
                    list.Add(new EbArrow(d.Slider, d.Target, DiscoveredColor, d.IsCheck ? 0.65 : 0.4, 0.8, "line"));
                foreach (var r in side.RemovableDefenders)
                    foreach (var defended in r.Defends)
                        list.Add(new EbArrow(r.Defender, defended, RemovableColor, 0.4, 0.8, "line"));
            }
        }
        return list;
    }

    /// <summary>Pinned/checker/checked-king discs and block dots (pinsChecks), red danger
    /// tint and green escape dots around each king (dangerZone), amber discs on hanging
    /// pieces (hanging), and purple/magenta discs on forkers and overloaded defenders
    /// (tactics).</summary>
    public static IReadOnlyList<EbCircle> Circles(BoardUtils.PositionInsights ins, bool pinsChecks, bool dangerZone, bool hanging = false, bool tactics = false)
    {
        if (ins == null || (!pinsChecks && !dangerZone && !hanging && !tactics)) return Array.Empty<EbCircle>();
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
            if (hanging)
            {
                // Muted for the side to move (a warning it can still act on), full strength
                // for the opponent's pieces (capturable now) — mirrors the arrow nuance.
                var discOpacity = side.IsSideToMove ? 0.18 : 0.35;
                foreach (var h in side.HangingPieces)
                    list.Add(new EbCircle(h.Square, HangingColor, discOpacity, 0.95));
            }
            if (tactics)
            {
                foreach (var f in side.Forks)
                    list.Add(new EbCircle(f.Forker, ForkColor, 0.35, 0.95));
                foreach (var o in side.OverloadedDefenders)
                    list.Add(new EbCircle(o.Defender, OverloadColor, 0.35, 0.95));
                foreach (var d in side.DiscoveredAttacks)
                    list.Add(new EbCircle(d.Blocker, DiscoveredColor, 0.35, 0.95));
                foreach (var r in side.RemovableDefenders)
                    list.Add(new EbCircle(r.Defender, RemovableColor, 0.35, 0.95));
            }
        }
        return list;
    }
}
