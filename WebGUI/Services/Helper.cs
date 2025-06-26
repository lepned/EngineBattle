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

        public static string ShowPlayersSummary(PgnGame game)
        {
            var data = game.GameMetaData;
            if (!string.IsNullOrWhiteSpace(data.White))
            {
                return $"{data.White} vs {data.Black}";
            }
            else if (data.Event.Contains("Engine Analysis"))
            {
                //we are now in analysis PGN
                var move = data.OtherTags.FirstOrDefault(e => e.Key.Contains("Move"));
                if (move != null)
                {
                    return $"{move.Value}";
                }
            }

            return string.Empty;
        }

        public static string GetResultSummary(PgnGame game)
        {
            var data = game.GameMetaData;
            if (!string.IsNullOrWhiteSpace(data.Result))
            {
                return data.Result;
            }
            else if (data.Event.Contains("Engine Analysis"))
            {
                //we are now in analysis PGN
                var eval = data.OtherTags.FirstOrDefault(e => e.Key.Contains("Eval"));
                if (eval != null)
                {
                    return $"{eval.Value}";
                }
            }

            return string.Empty;
        }
    }
}
