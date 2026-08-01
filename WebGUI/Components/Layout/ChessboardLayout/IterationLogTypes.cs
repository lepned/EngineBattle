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
