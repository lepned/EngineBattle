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
        public List<int> MoveElements { get; set; } = new List<int>(Enumerable.Range(1, 500));

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
        private const double MoveIndicatorMaxY = 10d;

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
            var cap10 = Math.Abs(x) > MoveIndicatorMaxY ? MoveIndicatorMaxY * Math.Sign(x) : x;
            return cap10;
        }

        public void AssignEvalsFromPGN(PgnGame game)
        {
            ClearData(PlayerWhite, PlayerBlack);
            var moveStats = Parser.PGNExtractor.extractEngineStats(game).Moves.ToArray();
            var last = moveStats.LastOrDefault();
            if (last != null && last.wv == 0 && (last.d == 0 || last.mt == 0 || last.tl == 0))
            {
                if (moveStats.Length > 0)
                {
                    moveStats = moveStats[..^1]; // slice array excluding last element
                }                
            }
            PlayerWhiteData =
              moveStats
              .Where(e => e.Player == PlayerWhite)
              .Select(e => PseudoLogTransform(e.wv)).ToArray();
            
            PlayerBlackData =
              moveStats
              .Where(e => e.Player == PlayerBlack)
              .Select(e => PseudoLogTransform(e.wv)).ToArray();
        }

        public void ClearData(string white, string black)
        {
            try
            {
                PlayerWhiteData = Array.Empty<double>();
                PlayerBlackData = Array.Empty<double>();
                //MoveElements.Clear();                
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

        private double[] GetDynamicYRange()
        {
            var all = PlayerWhiteData.Concat(PlayerBlackData).ToArray();
            if (all.Length == 0)
            {
                return new[] { -MoveIndicatorMaxY, MoveIndicatorMaxY };
            }

            double min = all.Min();
            double max = all.Max();

            // Handle flat data
            if (Math.Abs(max - min) < 1e-9)
            {
                if (min == 0)
                {
                    min = -1;
                    max = 1;
                }
                else
                {
                    var paddingFlat = Math.Max(0.25, Math.Abs(min) * 0.1);
                    min -= paddingFlat;
                    max += paddingFlat;
                }
            }

            // Add a small padding
            const double pad = 0.5;
            min -= pad;
            max += pad;

            // Clamp to ±MoveIndicatorMaxY
            min = Math.Max(-MoveIndicatorMaxY, min);
            max = Math.Min(MoveIndicatorMaxY, max);

            // Ensure a minimal span
            if (max - min < 0.5)
            {
                var mid = (max + min) / 2.0;
                min = Math.Max(-MoveIndicatorMaxY, mid - 0.25);
                max = Math.Min(MoveIndicatorMaxY, mid + 0.25);
            }

            return new[] { min, max };
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
            var yRange = GetDynamicYRange();
            var yaxis = new
            {
                gridcolor = whiteGridColor,
                gridwidth = 1,
                //rangemode = "tozero",
                nticks = 8,
                tickformat = ".1f",
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
                color = whiteColor,
                range = yRange,
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
