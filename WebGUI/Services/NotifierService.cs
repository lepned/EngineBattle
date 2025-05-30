


using static ChessLibrary.TypesDef.CoreTypes;
using static ChessLibrary.TypesDef.PGNTypes;
using static ChessLibrary.TypesDef.Puzzle;


namespace WebGUI.Services
{
  //create a enum for display settings
  public enum OverlaySettings
  {
    Policy,
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

  public class NotifierService
  {
    public NotifierService()
    {
    }
    public async Task NotifyEngineToReset(bool resetEngine)
    {
      if (ResetEngine != null)
        ResetEngine?.Invoke(resetEngine);
      await Task.CompletedTask;
    }

    public async Task NotifyFullScreenRequested(bool isFullScreenRequested)
    {
      if (IsFullScreenRequested != null)
        IsFullScreenRequested?.Invoke(isFullScreenRequested);
      await Task.CompletedTask;
    }
    public async Task NotifyUpdateRequested(int size)
    {
      if (UpdateRequested != null)
        UpdateRequested?.Invoke(size);
      await Task.CompletedTask;
    }

    public async Task RefreshNavMenu(bool refreshNavMenu)
    {
      if (RefreshNavMenuRequested != null)
        RefreshNavMenuRequested?.Invoke(refreshNavMenu);
      await Task.CompletedTask;
    }
    public async Task NotifyDarkMode(bool isDarkMode)
    {
      if (IsDarkMode != null)
        IsDarkMode?.Invoke(isDarkMode);
      await Task.CompletedTask;
    }
    public async Task UpdateFen(string fen)
    {
      if (NotifyFen != null)
        await NotifyFen?.Invoke(fen);
    }

    public async Task UpdateFenToBoard(string fen)
    {
      if (NotifyFenToBoard != null)
        await NotifyFenToBoard?.Invoke(fen);
    }

    public async Task Moves(List<NNValues> moves)
    {
      if (NotifyMoves != null)
        await NotifyMoves?.Invoke(moves);
    }

    public async Task MovesWithId(List<NNValues> moves, string id)
    {
      if (NotifyMovesWithId != null)
        await NotifyMovesWithId?.Invoke(moves, id);
    }

    public async Task UpdateDisplaySettings(OverlaySettings settings, string id)
    {
      if (NotifyDisplaySettings != null)
        await NotifyDisplaySettings?.Invoke(settings, id);
    }

    public async Task UpdateFenMoveToBoard(MoveAndFen input)
    {
      if (NotifyFenToBoard != null)
        await NotifyFenAndMove?.Invoke(input);
    }

    public async Task OnNextTick(bool isWhite, string timeLeft, string moveTime)
    {
      if (NextTick != null)
      {
        await NextTick.Invoke(isWhite, timeLeft, moveTime);
      }
    }
    
    public async Task UpdatePV(string pv, string fen, string player, int depth, int id)
    {
      if (NotifyPV != null)
        await NotifyPV?.Invoke(pv, fen, player, depth, id);
    }
    public async Task UpdatePGN(PgnGame game, string id, string color, int moveNr)
    {
      if (NotifyPGN != null)
        await NotifyPGN?.Invoke(game, id, color, moveNr);
    }

    public async Task UpdateOnPuzzleData(CsvPuzzleData input)
    {
      if (NotifyPuzzleData != null)
        await NotifyPuzzleData?.Invoke(input);
      //await Task.CompletedTask;
    }

    public async Task UpdateFenAndMove(MoveAndFen input)
    {
      if (NotifyFenAndMove != null)
        await NotifyFenAndMove?.Invoke(input);
    }

    public async Task UpdateOpeningDone(MoveAndFen input)
    {
      if (NotifyOpeningDone != null)
        await NotifyOpeningDone?.Invoke(input);
    }

    public async Task UpdateFenWithMoves(string fen)
    {
      if (NotifyFenWithMoves != null)
        await NotifyFenWithMoves?.Invoke(fen);
    }

    public async Task UpdateMove(string move, bool invokeMove = true)
    {
      if (NotifyMove != null)
        await NotifyMove?.Invoke(move, invokeMove);
    }
    public async Task UndoMove(string move)
    {
      if (NotifyUndoMove != null)
        await NotifyUndoMove?.Invoke(move);
    }

    public async Task CallNext()
    {
      if (NotifyCallNext != null)
        await NotifyCallNext?.Invoke();
    }

    public async Task AnnotateMove(string move)
    {
      if (NotifyAnnotateMove != null)
        await NotifyAnnotateMove?.Invoke(move);
    }

    public async Task UpdateCompleteGame(string[] moves, string fen)
    {
      if (NotifyCompleteGame != null)
        await NotifyCompleteGame?.Invoke(moves, fen);
      await Task.CompletedTask;
    }

    public async Task AddedEngineInTournament(List<EngineItem> engines)
    {
      if (NotifyAddedEngine != null)
        await NotifyAddedEngine?.Invoke(engines);
    }

    public async Task SettingLoaded(ChessConfigurationService config)
    {
      if (SettingAdded != null)
        await SettingAdded?.Invoke(config);
    }

    public async Task AddedDumpInfo(string text)
    {
      if (DumpInfoAdded != null)
        await DumpInfoAdded?.Invoke(text);
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
    public event Func<bool, Task> IsDarkMode;
    public event Func<bool, Task> ResetEngine;
    public event Func<bool, Task> IsFullScreenRequested;
    public event Func<int, Task> UpdateRequested;
    public event Func<bool, Task> RefreshNavMenuRequested;
    public event Func<OverlaySettings, string, Task> NotifyDisplaySettings;
  }
}
