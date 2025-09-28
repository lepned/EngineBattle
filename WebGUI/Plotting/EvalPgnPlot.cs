using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using ChessLibrary;
using static ChessLibrary.TypesDef.PGNTypes;

namespace WebGUI.Plotting
{
    public class GamePgnChart
    {
        public string Title { get; set; }
        public string YTitle { get; set; }
        public string PlayerWhite { get; set; }
        public string PlayerBlack { get; set; }
        public double[] PlayerWhiteData { get; set; } = Array.Empty<double>();
        public double[] PlayerBlackData { get; set; } = Array.Empty<double>();
        public List<int> MoveElements { get; set; } = new List<int>();

        private string titleColor = "#c1c1c4";
        private string blackColor = "#141519";
        private string whiteColor = "rgba(245, 245, 245, 0.75)";
        private string whiteGridColor = "#484952";
        private int fontSizeTitle = 20;
        private int fontSizeTickFont = 15;

        private IJSObjectReference chessModule;
        ElementReference chartElement;

        private object margin;
        private object whiteMarker;
        private object whiteLine;
        private object blackMarker;
        private object blackLine;
        private readonly string moveIndicatorColor = "#FFD400";
        private int? currentMoveIndex;

        public GamePgnChart(IJSObjectReference chessMod, ElementReference chart, string white, string black, string title, string yTitle)
        {
            chessModule = chessMod;
            chartElement = chart;
            PlayerWhite = white;
            PlayerBlack = black;
            Title = title;
            YTitle = yTitle;

            margin = new
            {
                l = 55,
                r = 15,
                b = 30,
                t = 50,
                pad = 2,
            };

            whiteMarker = new
            {
                color = whiteColor,
                size = 6,
                symbol = "diamond",
                opacity = 0.7,
                line = new
                {
                    color = "white",
                    width = 2
                },
            };

            whiteLine = new
            {
                width = 3,
                color = whiteColor,
                opacity = 0.7
            };


            blackMarker = new
            {
                color = blackColor,
                size = 7,
                fillcolor = blackColor,
                opacity = 0.9,
                line = new
                {
                    color = "silver",
                    width = 1.5
                }
            };

            blackLine = new
            {
                width = 3,
                color = blackColor,
                opacity = 0.9
            };

        }

        private object BuildMoveIndicatorShape(int moveIndex)
        {
            return new
            {
                type = "line",
                xref = "x",
                yref = "paper",
                x0 = moveIndex,
                x1 = moveIndex,
                y0 = 0,
                y1 = 1,
                line = new
                {
                    color = moveIndicatorColor,
                    width = 2
                },
                name = "moveIndicator"
            };
        }

        public async Task UpdateMoveIndicatorAsync(int moveIndex)
        {
            currentMoveIndex = moveIndex;
            if (chessModule is not null && chartElement.Context is not null)
            {
                await chessModule.InvokeVoidAsync("updateMoveIndicator", chartElement, moveIndex, moveIndicatorColor);
            }
        }

        private double PseudoLogTransform(double x)
        {
            var cap10 = Math.Abs(x) > 10 ? 10 * Math.Sign(x) : x;
            return cap10;
        }

        public void AssignEvalsFromPGN(PgnGame game)
        {
            ClearData(PlayerWhite, PlayerBlack);
            var moveStats = Parser.PGNExtractor.extractEngineStats(game);
            PlayerWhiteData =
              moveStats.Moves
              .Where(e => e.Player == PlayerWhite)
              .Select(e => PseudoLogTransform(e.wv)).ToArray();

            PlayerBlackData =
              moveStats.Moves
              .Where(e => e.Player == PlayerBlack)
              .Select(e => PseudoLogTransform(e.wv)).ToArray();
            var maxNumberOfMoves = Math.Max(PlayerWhiteData.Length, PlayerBlackData.Length);
            MoveElements = maxNumberOfMoves == 0 ? new List<int>() : Enumerable.Range(1, maxNumberOfMoves).ToList();
        }

        public void ClearData(string white, string black)
        {
            try
            {
                PlayerWhiteData = Array.Empty<double>();
                PlayerBlackData = Array.Empty<double>();
                MoveElements.Clear();                
                PlayerWhite = white;
                PlayerBlack = black;
            }
            //catch js exception
            catch (JSException ex)
            {
                var msg = $"An error occurred in JS {nameof(ClearData)}: {ex.Message}";
                Console.WriteLine(msg);
            }

            catch (Exception ex)
            {
                Console.WriteLine($"An error occurred in ClearData method: {ex.Message}");
            }
        }

        public async Task SetEvalChartData()
        {
            if (MoveElements.Count == 0)
            {
                await Task.CompletedTask;
                return;
            }

            var xaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                showgrid = false,
                zeroline = false,
                color = whiteColor
            };

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = 8,
                tickformat = ".1f",
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
                color = whiteColor,
                range = new[] { 0, MoveElements.Count }
            };

            var layout = new
            {
                title = new
                {
                    text = Title,
                    font = new { size = fontSizeTitle, color = titleColor },
                },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = false,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = margin,
                showgrid = true,
                shapes = currentMoveIndex.HasValue
                    ? new[] { BuildMoveIndicatorShape(currentMoveIndex.Value) }
                    : Array.Empty<object>()
            };

            var trace1 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerWhiteData,
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerBlackData,
                type = "scatter",
                name = PlayerBlack,
                mode = "lines+markers",
                marker = blackMarker,
                line = blackLine,
            };

            var config = new
            {
                trace1 = trace1,
                trace2 = trace2,
            };

            if (chessModule is not null && chartElement.Context is not null)
            {
                await chessModule.InvokeVoidAsync("setLineEvalChartData", chartElement, layout, config);
            }
        }
    }
}
