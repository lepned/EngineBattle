using System.Text.RegularExpressions;
using static ChessLibrary.MiscTypes;
using static ChessLibrary.EngineTypes;

namespace WebGUI.Components.Layout.ChessboardLayout;

/// <summary>One completed search iteration (one depth) of an alpha-beta engine's main line —
/// the AB counterpart to an MCTS root-visit row. Eval is white-perspective: exactly one of
/// <see cref="Cp"/> (pawns) and <see cref="Mate"/> (moves to mate) is set. <see cref="Seconds"/>
/// is the engine's own reported search time, which is why rows are built from the raw info
/// line — EngineStatus carries everything else but has no time field. <see cref="Bounded"/>
/// marks a fail-high/fail-low line (aspiration re-search in progress).</summary>
/// <para><see cref="Hashfull"/> is the engine's hash occupancy in permille (0-1000); 0 when the
/// engine doesn't report it. It matters because a near-full table is the usual reason a deep
/// search suddenly slows down — a setup problem rather than a hard position.</para>
public sealed record IterationRow(
    int Depth,
    int SelDepth,
    double? Cp,
    int? Mate,
    string PvSan,
    string PvLan,
    long Nodes,
    double Nps,
    double Seconds,
    bool Bounded,
    int Hashfull = 0);

/// <summary>Builds <see cref="IterationRow"/>s from an engine's info stream and keeps one row
/// per depth. Shared because both hosts that show the log — the engine panel and the broadcast
/// kibitzer — need the identical reading of a line; a copy in each had already drifted.</summary>
public sealed class IterationCollector
{
    // Word boundaries matter: without them "time" would also match inside another token.
    private static readonly Regex TimeRegex = new(@"\btime\s+(\d+)", RegexOptions.Compiled);
    private static readonly Regex BoundRegex = new(@"\b(lowerbound|upperbound)\b", RegexOptions.Compiled);
    private static readonly Regex HashfullRegex = new(@"\bhashfull\s+(\d+)", RegexOptions.Compiled);

    private readonly object gate = new();
    private readonly List<IterationRow> work = new();

    /// <summary>Snapshot for rendering: replaced wholesale on every change, so a render can
    /// enumerate it on the dispatcher while the engine's reader thread keeps producing.</summary>
    public IReadOnlyList<IterationRow> Rows { get; private set; } = new List<IterationRow>();

    /// <summary>Drop everything for a new search. Left standing, the previous search's depths
    /// are all already known when the next one reports them, so nothing counts as a new depth
    /// and hosts listening for that are never told anything again.</summary>
    public void Clear()
    {
        lock (gate)
        {
            work.Clear();
            Rows = new List<IterationRow>();
        }
    }

    /// <summary>Records one info line together with the status parsed from it. Returns true
    /// when the line opened a new depth rather than refining the current one — the rate at
    /// which hosts that only care about iteration boundaries want to hear about it.</summary>
    /// <param name="fallbackSeconds">Used when the engine omits <c>time</c> from the line.</param>
    public bool Capture(EngineStatus s, string rawLine, double fallbackSeconds)
    {
        if (s.Depth <= 0) return false;

        var m = TimeRegex.Match(rawLine);
        var seconds = m.Success && long.TryParse(m.Groups[1].Value, out var ms)
            ? ms / 1000.0
            : fallbackSeconds;

        double? cp = null;
        int? mate = null;
        switch (s.Eval)
        {
            case EvalType.CP c: cp = c.Info; break;
            case EvalType.Mate mt: mate = mt.Info; break;
        }

        var hf = HashfullRegex.Match(rawLine);
        var hashfull = hf.Success && int.TryParse(hf.Groups[1].Value, out var permille) ? permille : 0;

        var row = new IterationRow(s.Depth, s.SD, cp, mate, s.PV ?? "", s.PVLongSAN ?? "",
                                   s.Nodes, s.NPS, seconds, BoundRegex.IsMatch(rawLine), hashfull);

        lock (gate)
        {
            // Aspiration re-searches repeat a depth; the last line for it wins, so the log
            // holds one row per depth like an engine's own iteration output.
            var idx = work.FindLastIndex(r => r.Depth == row.Depth);
            var isNewDepth = idx < 0;
            if (isNewDepth)
            {
                work.Add(row);
                // One row per depth bounds this by the engine's ply cap (~245 for Stockfish)
                // on its own. The limit is only a backstop against an engine reporting
                // nonsense depths — trimming for size would throw away the early iterations,
                // which are the ones that carry the information once the display merges
                // unchanged runs.
                if (work.Count > 512) work.RemoveAt(0);
            }
            else
            {
                work[idx] = row;
            }
            Rows = new List<IterationRow>(work);
            return isNewDepth;
        }
    }
}
