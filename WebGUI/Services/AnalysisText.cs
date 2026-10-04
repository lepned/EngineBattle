using System.Globalization;
using static ChessLibrary.MiscTypes;

namespace WebGUI.Services;

/// Node counts and evals as the analysis views show them, the same in every panel.
public static class AnalysisText
{
    private static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

    /// 1.25B, 3.4M, 12.5K, 980.
    public static string Nodes(double nodes) => nodes switch
    {
        >= 1_000_000_000 => (nodes / 1_000_000_000).ToString("0.##", Inv) + "B",
        >= 1_000_000 => (nodes / 1_000_000).ToString("0.##", Inv) + "M",
        >= 1_000 => (nodes / 1_000).ToString("0.#", Inv) + "K",
        _ => nodes.ToString("0", Inv)
    };

    /// White's view: +0.25, -1.30, 0.00 (never "-0.00"), M3, -M3; "–" when there is none.
    public static string Eval(EvalType eval) => eval switch
    {
        EvalType.CP cp => cp.Info.ToString("+0.00;-0.00;0.00", Inv),
        EvalType.Mate m => m.Info > 0 ? $"M{m.Info}" : $"-M{-m.Info}",
        _ => "–"
    };
}
