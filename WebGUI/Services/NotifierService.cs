

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
  public enum EngineType
  {
    CeresInProcess,
    CeresInProcess1,
    CeresUCI,
    CeresUCI1,
    LC0InProcess,
    LC0InProcess1,
    Stockfish,
    UCI
  };

  public class EngineItem
  {
    public EngineType EngineType { get; set; }
    public bool Reference { get; set; }
    //public EnginePlayerDef PlayerDef { get; set; }
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
    public async Task NotifyEngineToReset(bool resetEngine)
    {
      var handler = ResetEngine;
      if (handler != null)
        await handler.Invoke(resetEngine);
    }

    public async Task NotifyFullScreenRequested(bool isFullScreenRequested)
    {
      var handler = IsFullScreenRequested;
      if (handler != null)
        await handler.Invoke(isFullScreenRequested);
    }
    public async Task NotifyUpdateRequested(int size)
    {
      var handler = UpdateRequested;
      if (handler != null)
        await handler.Invoke(size);
    }

    public async Task RefreshNavMenu(bool refreshNavMenu)
    {
      var handler = RefreshNavMenuRequested;
      if (handler != null)
        await handler.Invoke(refreshNavMenu);
    }
    public async Task NotifyDarkMode(bool isDarkMode)
    {
      var handler = IsDarkMode;
      if (handler != null)
        await handler.Invoke(isDarkMode);
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

    public async Task Moves(List<NNValues> moves)
    {
      var handler = NotifyMoves;
      if (handler != null)
        await handler.Invoke(moves);
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

    public async Task UpdatePV(string pv, string fen, string player, int depth, int id)
    {
      var handler = NotifyPV;
      if (handler != null)
        await handler.Invoke(pv, fen, player, depth, id);
    }
    public async Task UpdatePGN(PgnGame game, string id, string color, int moveNr)
    {
      var handler = NotifyPGN;
      if (handler != null)
        await handler.Invoke(game, id, color, moveNr);
    }

    public async Task UpdateOnPuzzleData(CsvPuzzleData input)
    {
      var handler = NotifyPuzzleData;
      if (handler != null)
        await handler.Invoke(input);
    }

    public async Task UpdateFenAndMove(MoveAndFen input)
    {
      var handler = NotifyFenAndMove;
      if (handler != null)
        await handler.Invoke(input);
    }

    public async Task UpdateOpeningDone(MoveAndFen input)
    {
      var handler = NotifyOpeningDone;
      if (handler != null)
        await handler.Invoke(input);
    }

    public async Task UpdateFenWithMoves(string fen)
    {
      var handler = NotifyFenWithMoves;
      if (handler != null)
        await handler.Invoke(fen);
    }

    public async Task UpdateMove(string move, bool invokeMove = true)
    {
      var handler = NotifyMove;
      if (handler != null)
        await handler.Invoke(move, invokeMove);
    }
    public async Task UndoMove(string move)
    {
      var handler = NotifyUndoMove;
      if (handler != null)
        await handler.Invoke(move);
    }

    public async Task CallNext()
    {
      var handler = NotifyCallNext;
      if (handler != null)
        await handler.Invoke();
    }

    public async Task AnnotateMove(string move)
    {
      var handler = NotifyAnnotateMove;
      if (handler != null)
        await handler.Invoke(move);
    }

    public async Task UpdateCompleteGame(string[] moves, string fen)
    {
      var handler = NotifyCompleteGame;
      if (handler != null)
        await handler.Invoke(moves, fen);
    }

    public async Task AddedEngineInTournament(List<EngineItem> engines)
    {
      var handler = NotifyAddedEngine;
      if (handler != null)
        await handler.Invoke(engines);
    }

    public async Task SettingLoaded(ChessConfigurationService config)
    {
      var handler = SettingAdded;
      if (handler != null)
        await handler.Invoke(config);
    }

    public async Task AddedDumpInfo(string text)
    {
      var handler = DumpInfoAdded;
      if (handler != null)
        await handler.Invoke(text);
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
    public event Func<List<NNValues>, Task> NotifyMoves;
    public event Func<List<NNValues>, string, Task> NotifyMovesWithId;
    public event Func<PgnGame, string, string, int, Task> NotifyPGN;
    public event Func<string, string, string, int, int, Task> NotifyPV;
    public event Func<Task> NotifyCallNext;
    public event Func<MoveAndFen, Task> NotifyFenAndMove;
    public event Func<CsvPuzzleData, Task> NotifyPuzzleData;
    public event Func<MoveAndFen, Task> NotifyOpeningDone;
    public event Func<string, Task> NotifyFenWithMoves;
    public event Func<string, bool, Task> NotifyMove;
    public event Func<string, Task> NotifyUndoMove;
    public event Func<string, Task> NotifyAnnotateMove;
    public event Func<string[], string, Task> NotifyCompleteGame;
    public event Func<List<EngineItem>, Task> NotifyAddedEngine;
    public event Func<ChessConfigurationService, Task> SettingAdded;
    public event Func<string, Task> DumpInfoAdded;
    public event Func<Task> CupUpdated;
    public event Func<Task> LadderUpdated;
    public event Func<bool, Task> IsDarkMode;
    public event Func<bool, Task> ResetEngine;
    public event Func<bool, Task> IsFullScreenRequested;
    public event Func<int, Task> UpdateRequested;
    public event Func<bool, Task> RefreshNavMenuRequested;
    public event Func<OverlaySettings, string, Task> NotifyDisplaySettings;
  }
}
