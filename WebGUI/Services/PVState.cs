#nullable enable
namespace WebGUI.Services;

/// <summary>
/// Observable state for Principal Variation updates.
/// Used to push live PV updates from EnginePanel to PVBoardLive dialog.
/// </summary>
public class PVState
{
    public string EngineName { get; private set; } = "";
    public string EngineId { get; private set; } = "";
    public string PVMoves { get; private set; } = "";
    public string StartFEN { get; private set; } = "";
    public int Depth { get; private set; }
    public long Nodes { get; private set; }
    public string Eval { get; private set; } = "";
    public bool IsSearching { get; private set; }

    public event Action? OnUpdate;

    public void Update(string engineName, string engineId, string pvMoves, string startFen,
                       int depth, long nodes, string eval, bool isSearching)
    {
        EngineName = engineName;
        EngineId = engineId;
        PVMoves = pvMoves;
        StartFEN = startFen;
        Depth = depth;
        Nodes = nodes;
        Eval = eval;
        IsSearching = isSearching;

        OnUpdate?.Invoke();
    }

    public void SetSearching(bool isSearching)
    {
        IsSearching = isSearching;
        OnUpdate?.Invoke();
    }

    public void Clear()
    {
        PVMoves = "";
        Depth = 0;
        Nodes = 0;
        Eval = "";
        IsSearching = false;
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
