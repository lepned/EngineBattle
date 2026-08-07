#nullable enable
namespace WebGUI.Services;

/// <summary>Immutable snapshot of one PV update — read it once to get a consistent
/// PVMoves/StartFEN pair.</summary>
public sealed record PVSnapshot(
    string EngineName, string EngineId, string PVMoves, string StartFEN,
    int Depth, long Nodes, string Eval, bool IsSearching)
{
    public static readonly PVSnapshot Empty = new("", "", "", "", 0, 0L, "", false);
}

/// <summary>
/// Observable state for Principal Variation updates.
/// Used to push live PV updates from EnginePanel to PVBoardLive dialog.
/// Update() is called from engine-callback threads; state is published as a single
/// atomically-swapped snapshot so a subscriber can never pair a new PVMoves with the
/// previous StartFEN (which replayed the PV on the wrong position).
/// </summary>
public class PVState
{
    private PVSnapshot _current = PVSnapshot.Empty;

    /// <summary>Consistent view of the latest update — prefer this over the
    /// individual properties when reading more than one field.</summary>
    public PVSnapshot Current => _current;

    public string EngineName => _current.EngineName;
    public string EngineId => _current.EngineId;
    public string PVMoves => _current.PVMoves;
    public string StartFEN => _current.StartFEN;
    public int Depth => _current.Depth;
    public long Nodes => _current.Nodes;
    public string Eval => _current.Eval;
    public bool IsSearching => _current.IsSearching;

    public event Action? OnUpdate;

    public void Update(string engineName, string engineId, string pvMoves, string startFen,
                       int depth, long nodes, string eval, bool isSearching)
    {
        _current = new PVSnapshot(engineName, engineId, pvMoves, startFen, depth, nodes, eval, isSearching);
        OnUpdate?.Invoke();
    }

    public void SetSearching(bool isSearching)
    {
        _current = _current with { IsSearching = isSearching };
        OnUpdate?.Invoke();
    }

    public void Clear()
    {
        _current = _current with { PVMoves = "", Depth = 0, Nodes = 0, Eval = "", IsSearching = false };
        OnUpdate?.Invoke();
    }
}

/// <summary>
/// Container for dual engine PV states.
/// </summary>
public class DualPVState
{
    public PVState Engine1 { get; } = new();
    public PVState Engine2 { get; } = new();

    /// <summary>
    /// Finds the first move index where the two PVs deviate.
    /// Returns -1 if no deviation (identical PVs or one is empty).
    /// </summary>
    public int FindFirstDeviation()
    {
        if (string.IsNullOrEmpty(Engine1.PVMoves) || string.IsNullOrEmpty(Engine2.PVMoves))
            return -1;

        var moves1 = Engine1.PVMoves.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        var moves2 = Engine2.PVMoves.Split(' ', StringSplitOptions.RemoveEmptyEntries);

        var minLength = Math.Min(moves1.Length, moves2.Length);

        for (int i = 0; i < minLength; i++)
        {
            if (!string.Equals(moves1[i], moves2[i], StringComparison.OrdinalIgnoreCase))
                return i;
        }

        // If one PV is longer than the other, deviation is at the end of the shorter one
        if (moves1.Length != moves2.Length)
            return minLength;

        return -1; // Identical PVs
    }
}
