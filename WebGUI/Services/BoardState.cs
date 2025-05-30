using ChessLibrary;
using Microsoft.AspNetCore.Components;
using static ChessLibrary.Chess;
using static ChessLibrary.TypesDef.CoreTypes;
using static MudBlazor.CategoryTypes;

namespace WebGUI.Services
{
  public class BoardState
  {
    public Chess.Board Board { get; set; } = new();  
    public string PlayerName { get; set; }
    public string Fen { get; set; }
    public string LastPV { get; set; }
    public int LastDepth { get; set; }
    public int Id { get; set; }
    public string LastPonderMove { get; set; }
    public double LastFraction { get; set; }
    public MoveAndFen MoveAndFenType { get; set; } = MoveAndFen.FirstEntry;

    public IEnumerable<string> GetPVMoves(string pv, int depth, bool hideFullPV)
    {
      //var movestring = "d2d3 e4d3 d1d3 b8c6 f1g2 f8e7 e1g1 f6d7 b2b3 d7c5 d3d2 e8g8 c1b2 e7f6 c3d5 f6b2 d2b2 f8e8 a1d1 a7a5 d5f4  string M = NaN";
      var arr = pv.Split(' ');
      var moves = hideFullPV ? arr.Take(depth) : arr;
      foreach (var item in moves)
      {
        if (item.Any(char.IsDigit))
          yield return item;
      }
    }

    public Tuple<char,string> GetPieceAndColorOnSquare(string toSq)
    {
      return Board.GetPieceAndColorOnSquare(toSq);
    }

    public MoveAndFen PlayPVMoves(List<string> moves, string fen)
    {
      if (moves.Count != 0)
      {
        Fen = fen;
        var moveFen = Board.PlayPVLine(moves, fen);
        MoveAndFenType = moveFen;
        return moveFen;
      }
      else
        return MoveAndFen.Init(fen);
    }

  }

  public class ThreadSafeBoardState
  {
    private readonly object _lock = new object();
    private readonly Board _board = new Board();
    public string PlayerName { get; set; }
    public string Fen { get; set; }
    public string LastPV { get; set; }
    public int LastDepth { get; set; }
    public int Id { get; set; }
    public string LastPonderMove { get; set; }
    public double LastFraction { get; set; }
    public MoveAndFen MoveAndFenType { get; set; } = MoveAndFen.FirstEntry;

    public IEnumerable<string> GetPVMoves(string pv, int depth, bool hideFullPV)
    {
      //var movestring = "d2d3 e4d3 d1d3 b8c6 f1g2 f8e7 e1g1 f6d7 b2b3 d7c5 d3d2 e8g8 c1b2 e7f6 c3d5 f6b2 d2b2 f8e8 a1d1 a7a5 d5f4  string M = NaN";
      var arr = pv.Split(' ');
      var moves = hideFullPV ? arr.Take(depth) : arr;
      foreach (var item in moves)
      {
        if (item.Any(char.IsDigit))
          yield return item;
      }
    }

    public Task<MoveAndFen> PlayPVMoves(List<string> moves, string fen)
    {
      if (moves.Count != 0)
      {
        lock (_lock)
        {
          Fen = fen;
          var moveFen = _board.PlayPVLineThreadSafe(moves, fen);
          MoveAndFenType = moveFen;
          return Task.FromResult(moveFen);
        }
      }
      else
        return Task.FromResult(MoveAndFen.Init(fen));
    }

    public Tuple<char, string> GetPieceAndColorOnSquare(string toSq)
    {
      lock (_lock)
      {
        return _board.GetPieceAndColorOnSquare(toSq);
      }        
    }

    public void LoadFen(string fen)
    {
      lock (_lock)
      {
        _board.LoadFen(fen);
      }
    }

    }
}
