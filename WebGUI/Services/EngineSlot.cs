namespace WebGUI.Services;

/// <summary>The two engine slots a board can draw an overlay for. The id is a key shared by
/// an engine panel (its <c>Id</c>), the overlay-settings singleton and the notifier messages,
/// and the board keeps one move list per slot. Single analysis and the broadcast kibitzer use
/// A; Dual analysis and the contempt page use both. (They were "nnInput1"/"nnInput2".)</summary>
public static class EngineSlot
{
    public const string A = "engineA";
    public const string B = "engineB";
}
