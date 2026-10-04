
using static ChessLibrary.TypesDef.CoreTypes;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.PuzzleTypes;
using static ChessLibrary.EngineTypes;

namespace WebGUI.Services
{
  //create a enum for display settings
  public enum OverlaySettings
  {
    Policy,
    SearchPolicy,
    Nodes,
    Q,
    V,
    E,
    QVDiff,
    None
  }
  // Every notify method captures the event field once before invoking: these fire from
  // engine-callback/tournament threads while components unsubscribe on the circuit
  // thread, and a second read of the field can be null after the last handler detached
  // (await null → NullReferenceException on the engine thread).
  public class NotifierService
  {
    public NotifierService()
    {
    }

    public async Task NotifyFullScreenRequested(bool isFullScreenRequested)
    {
      var handler = IsFullScreenRequested;
      if (handler != null)
        await handler.Invoke(isFullScreenRequested);
    }

    public async Task RefreshNavMenu(bool refreshNavMenu)
    {
      var handler = RefreshNavMenuRequested;
      if (handler != null)
        await handler.Invoke(refreshNavMenu);
    }
    public async Task UpdateFen(string fen)
    {
      var handler = NotifyFen;
      if (handler != null)
        await handler.Invoke(fen);
    }

    public async Task UpdateFenToBoard(string fen)
    {
      var handler = NotifyFenToBoard;
      if (handler != null)
        await handler.Invoke(fen);
    }

    public async Task MovesWithId(List<NNValues> moves, string id)
    {
      var handler = NotifyMovesWithId;
      if (handler != null)
        await handler.Invoke(moves, id);
    }

    public async Task UpdateDisplaySettings(OverlaySettings settings, string id)
    {
      var handler = NotifyDisplaySettings;
      if (handler != null)
        await handler.Invoke(settings, id);
    }

    public async Task UpdateFenMoveToBoard(MoveAndFen input)
    {
      var handler = NotifyFenAndMove;
      if (handler != null)
        await handler.Invoke(input);
    }

    public async Task OnNextTick(bool isWhite, string timeLeft, string moveTime)
    {
      var handler = NextTick;
      if (handler != null)
        await handler.Invoke(isWhite, timeLeft, moveTime);
    }

    public async Task NotifyCupUpdated()
    {
      var handler = CupUpdated;
      if (handler != null)
        await handler.Invoke();
    }

    public async Task NotifyLadderUpdated()
    {
      var handler = LadderUpdated;
      if (handler != null)
        await handler.Invoke();
    }

    public event Func<bool, string, string, Task> NextTick;
    public event Func<string, Task> NotifyFen;
    public event Func<string, Task> NotifyFenToBoard;
    public event Func<List<NNValues>, string, Task> NotifyMovesWithId;
    public event Func<MoveAndFen, Task> NotifyFenAndMove;
    public event Func<Task> CupUpdated;
    public event Func<Task> LadderUpdated;
    public event Func<bool, Task> IsFullScreenRequested;
    public event Func<bool, Task> RefreshNavMenuRequested;
    public event Func<OverlaySettings, string, Task> NotifyDisplaySettings;
  }
}
