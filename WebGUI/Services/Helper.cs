using static ChessLibrary.Parser;
using static ChessLibrary.TypesDef.PGNTypes;

namespace WebGUI.Services
{
  public static class Helper
  {
    public static string RemoveMoveNumbers(string input)
    {
      // Define a regular expression to match move numbers (digit followed by '.' optionally followed by a space)
      var regex = new System.Text.RegularExpressions.Regex(@"\d+\.\s?");

      // Replace all instances of move numbers with an empty string and trim the result
      return regex.Replace(input, "").Trim();
    }

    public static string ShowPGNSummary(PgnGame game)
    {
      var data = game.GameMetaData;
      return $"{data.White} vs {data.Black}";
    }
  }
}
