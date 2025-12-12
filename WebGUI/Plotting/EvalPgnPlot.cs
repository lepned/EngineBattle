using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using ChessLibrary;
using static ChessLibrary.TypesDef.PGNTypes;

namespace WebGUI.Plotting
{
    public enum CustomChartType
    {
        Evaluation,
        Nodes,
        TimeUsage,
        NPS,
        EPS
    }

    public class GamePgnChart
    {
        public string Title { get; set; }
        public string YTitle { get; set; }
        public string PlayerWhite { get; set; }
        public string PlayerBlack { get; set; }
        public double[] PlayerWhiteData { get; set; } = Array.Empty<double>();
        public double[] PlayerBlackData { get; set; } = Array.Empty<double>();
        public List<int> MoveElements { get; set; } = new List<int>(Enumerable.Range(1, 500));
        public PgnGame Game { get; set; }

        private string titleColor = "#c1c1c4";
        private string blackColor = "#141519";
        private string whiteColor = "rgba(245, 245, 245, 0.75)";
        private string whiteGridColor = "#484952";
        private int fontSizeTitle = 20;
        private int fontSizeTickFont = 15;
        private int nticks = 6;
        private bool liveUpdateStarted = false;

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
        private int startMoveIndex = 1;
        private bool startIsWhiteToMove = true;
        private const string DefaultStartFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        private Chess.Board board = new Chess.Board();

        private int WhiteXStart => startMoveIndex + (startIsWhiteToMove ? 0 : 1);
        private int BlackXStart => startMoveIndex;

        private static int[] BuildX(int start, int len)
            => len <= 0 ? Array.Empty<int>() : Enumerable.Range(start, len).ToArray();

        private int LastXOnChart
        {
            get
            {
                var lastW = PlayerWhiteData.Length > 0 ? (WhiteXStart + PlayerWhiteData.Length - 1) : int.MinValue;
                var lastB = PlayerBlackData.Length > 0 ? (BlackXStart + PlayerBlackData.Length - 1) : int.MinValue;
                var last = Math.Max(lastW, lastB);
                return last == int.MinValue ? startMoveIndex : last;
            }
        }

        private int ClampMoveIndex(int moveIndex)
        {
            var min = startMoveIndex;
            var max = LastXOnChart;
            if (moveIndex < min) return min;
            if (moveIndex > max) return max;
            return moveIndex;
        }

        public GamePgnChart(IJSObjectReference chessMod, ElementReference chart, PgnGame game, string title, string yTitle)
        {            
            var startFen = string.IsNullOrWhiteSpace(game?.Fen) ? DefaultStartFen : game.Fen;
            board.LoadFen(startFen);
            startMoveIndex = board.MoveNumber();
            startIsWhiteToMove = board.Position.STM == 0;
            chessModule = chessMod;
            chartElement = chart;
            Game = game;
            PlayerWhite = game.GameMetaData.White;
            PlayerBlack = game.GameMetaData.Black;
            Title = title;
            YTitle = yTitle;

            margin = new
            {
                l = 60,
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
            moveIndex = ClampMoveIndex(moveIndex);
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

        // New async version that updates the chart after assigning data
        public async Task AssignTimeUsageFromPGNAsync(CustomChartType chartType = CustomChartType.TimeUsage)
        {
            var currIdx = currentMoveIndex;
            ClearData(PlayerWhite, PlayerBlack);
            currentMoveIndex = currIdx;
            var (whiteMoves, blackMoves) = Parser.PGNExtractor.extractWhiteAndBlackEngineStats(Game);
            
            PlayerWhiteData =
              whiteMoves
              .Select(e => (double)e.mt/1000).ToArray();

            PlayerBlackData =
              blackMoves
              .Select(e => (double)e.mt/1000).ToArray();
            Title = "Time Usage (sec)";
            await SetChartTimeUsageData();
        }

        public async Task AssignNodesFromPGNAsync(CustomChartType chartType = CustomChartType.Nodes)
        {
            var currIdx = currentMoveIndex;
            ClearData(PlayerWhite, PlayerBlack);
            currentMoveIndex = currIdx;
            var (whiteMoves, blackMoves) = Parser.PGNExtractor.extractWhiteAndBlackEngineStats(Game);
            
            PlayerWhiteData =
              whiteMoves
              .Select(e => (double)e.n).ToArray();

            PlayerBlackData =
              blackMoves
              .Select(e => (double)e.n).ToArray();

            Title = "Nodes";
            await SetChartNodeData();
        }

        public async Task AssignNPSFromPGNAsync(int secs = 0, CustomChartType chartType = CustomChartType.Nodes)
        {
            var minMoveTimeInMs = secs * 1000;
            var currIdx = currentMoveIndex;
            ClearData(PlayerWhite, PlayerBlack);
            currentMoveIndex = currIdx;
            var (whiteMoves, blackMoves) = Parser.PGNExtractor.extractWhiteAndBlackEngineStats(Game);
            
            PlayerWhiteData =
              whiteMoves
              .Where(e => e.mt >= minMoveTimeInMs)
              .Select(e => (double)e.s).ToArray();

            PlayerBlackData =
              blackMoves
              .Where(e => e.mt >= minMoveTimeInMs)
              .Select(e => (double)e.s).ToArray();

            Title = "NPS";
            await SetChartNPSData();
        }

        // New async version that updates the chart after assigning data
        public async Task AssignEvalsFromPGNAsync(CustomChartType chartType = CustomChartType.Evaluation)
        {
            var currIdx = currentMoveIndex;
            ClearData(PlayerWhite, PlayerBlack);
            currentMoveIndex = currIdx;            
            var (whiteMoves, blackMoves) = Parser.PGNExtractor.extractWhiteAndBlackEngineStats(Game);            
            PlayerWhiteData =
              whiteMoves
              .Select(e => PseudoLogTransform(e.wv)).ToArray();

            PlayerBlackData =
              blackMoves
              .Select(e => PseudoLogTransform(e.wv)).ToArray();
            Title = "Evaluation";
            await SetEvalChartData();
        }

        // New async method: Assign EPS from PGN (same logic as NPS but uses eps field)
        public async Task AssignEPSFromPGNAsync(int secs = 0, CustomChartType chartType = CustomChartType.EPS)
        {
            var minMoveTimeInMs = secs * 1000;
            var currIdx = currentMoveIndex;
            ClearData(PlayerWhite, PlayerBlack);
            currentMoveIndex = currIdx;
            var (whiteMoves, blackMoves) = Parser.PGNExtractor.extractWhiteAndBlackEngineStats(Game);
            PlayerWhiteData =
              whiteMoves
              .Where(e => e.mt >= minMoveTimeInMs)
              .Select(e => (double)e.eps).ToArray();

            PlayerBlackData =
              blackMoves
              .Where(e => e.mt >= minMoveTimeInMs)
              .Select(e => (double)e.eps).ToArray();

            Title = "EPS";
            // Reuse NPS rendering which supports same single/double axis logic
            await SetChartNPSData();
        }

        // Method to update chart based on chart type
        public async Task UpdateChartAsync(CustomChartType chartType, int minMoveTimeInSecs = 0)
        {
            switch (chartType)
            {
                case CustomChartType.Evaluation:
                    await AssignEvalsFromPGNAsync(CustomChartType.Evaluation);                    
                    break;
                case CustomChartType.Nodes:
                    await AssignNodesFromPGNAsync(CustomChartType.Nodes);
                    break;
                case CustomChartType.TimeUsage:
                    await AssignTimeUsageFromPGNAsync(CustomChartType.TimeUsage);                    
                    break;
                case CustomChartType.NPS:
                    await AssignNPSFromPGNAsync(minMoveTimeInSecs);
                    break;
                case CustomChartType.EPS:
                    await AssignEPSFromPGNAsync(minMoveTimeInSecs);
                    break;
            }
            await UpdateMoveIndicatorAsync(currentMoveIndex ?? startMoveIndex);
        }

        // Method to switch chart types dynamically
        public async Task SwitchChartTypeAsync(CustomChartType newChartType, int minMoveTimeinSecs = 0)
        {
            liveUpdateStarted = false; // Force full re-render
            await UpdateChartAsync(newChartType, minMoveTimeinSecs);
        }

        // Check if chart has data
        public bool HasData() => PlayerWhiteData.Length > 0 || PlayerBlackData.Length > 0;

        public void ClearData(string white, string black)
        {
            try
            {
                PlayerWhiteData = Array.Empty<double>();
                PlayerBlackData = Array.Empty<double>();                
                PlayerWhite = white;
                PlayerBlack = black;
                liveUpdateStarted = false; // Reset flag to force full re-render
                currentMoveIndex = null; // Reset move indicator
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

        // Async version that also resets the chart visuals in JS (mirrors LivePlot behavior)
        public async Task ClearDataAsync(string white, string black)
        {
            try
            {
                PlayerWhiteData = Array.Empty<double>();
                PlayerBlackData = Array.Empty<double>();
                PlayerWhite = white;
                PlayerBlack = black;
                liveUpdateStarted = false;
                currentMoveIndex = null;
                // reset traces in JS
                await Task.WhenAll(
                    SetEvalChartData(),
                    SetChartNodeData(),
                    SetChartTimeUsageData());
            }
            catch (JSException ex)
            {
                Console.WriteLine($"An error occurred in JS {nameof(ClearDataAsync)}: {ex.Message}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An error occurred in {nameof(ClearDataAsync)}: {ex.Message}");
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
                automargin = true
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
                x = BuildX(WhiteXStart, PlayerWhiteData.Length),
                y = PlayerWhiteData,
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = BuildX(BlackXStart, PlayerBlackData.Length),
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

        // --- Added chart methods copied/adapted from LivePlot.cs ---

        private bool SameYaxis()
        {
            if (PlayerBlackData.Length == 0 || PlayerWhiteData.Length == 0)
                return true;

            var avgW = PlayerWhiteData.Average();
            var avgB = PlayerBlackData.Average();

            var max = Math.Max(Math.Abs(avgW), Math.Abs(avgB));
            if (avgW == 0 || avgB == 0)
                return true;

            return max / Math.Abs(avgB) < 10 && max / Math.Abs(avgW) < 10;
        }

        public async Task SetChartNodeData()
        {
            if (SameYaxis())
                await SetSingleChartNodeData();
            else
                await SetDoubleChartNodeData();
        }

        public async Task SetChartNPSData()
        {
            if (SameYaxis())
                await SetSingleChartNodeData();
            else
                await SetDoubleChartNodeData();
        }

        public async Task SetSingleChartNodeData()
        {
            var xaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                showgrid = false,
                zeroline = false,
                color = whiteColor
            };

            var yaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                gridcolor = whiteGridColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = nticks,
                showgrid = true,
                color = whiteColor,
                automargin = true
            };

            var layout = new
            {
                title = new
                {
                    text = Title,
                    font = new
                    {
                        size = fontSizeTitle,
                        color = titleColor
                    },
                },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = false,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = margin,
            };


            var trace1 = new
            {
                x = BuildX(WhiteXStart, PlayerWhiteData.Length),
                y = PlayerWhiteData,
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = BuildX(BlackXStart, PlayerBlackData.Length),
                y = PlayerBlackData,
                type = "scatter",
                name = PlayerBlack,
                mode = "lines+markers",
                marker = blackMarker,
                line = blackLine
            };

            var config = new
            {
                trace1 = trace1,
                trace2 = trace2
            };

            if (chessModule is not null && chartElement.Context is not null)
            {
                if (liveUpdateStarted == false)
                {
                    await chessModule.InvokeVoidAsync("setSingleNodeChart", chartElement, layout, config);
                }
                else
                {
                    if (PlayerWhiteData.Length == PlayerBlackData.Length)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Length > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Length > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }

        public async Task SetDoubleChartNodeData()
        {
            var xaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                showgrid = false,
                zeroline = false,
                showline = false,
                showzeroline = false,
                color = whiteColor
            };

            var yaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                automargin = true,
                color = whiteColor,
                gridcolor = whiteGridColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = nticks,
                showgrid = true               
            };

            var yaxis2 = new
            {
                title = new { text = "Black", standoff = 10 },
                titlefont = new { color = "black", size = fontSizeTickFont },
                tickfont = new { size = fontSizeTickFont },
                automargin = true,
                color = whiteColor,
                linwidth = 2,
                linecolor = blackColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = nticks,
                side = "right",
                showgrid = false,
                zeroline = false,
                showline = false,
            };

            var layout = new
            {
                title = new
                {
                    text = Title,
                    font = new
                    {
                        size = fontSizeTitle,
                        color = titleColor
                    },
                },

                showlegend = false,
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                xaxis = xaxis,
                yaxis = yaxis,
                yaxis2 = yaxis2,
                margin = margin,
            };

            var trace1 = new
            {
                x = BuildX(WhiteXStart, PlayerWhiteData.Length),
                y = PlayerWhiteData,
                type = "scatter",
                name = PlayerWhite,
                yaxis = "y1",
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = BuildX(BlackXStart, PlayerBlackData.Length),
                y = PlayerBlackData,
                type = "scatter",
                name = PlayerBlack,
                yaxis = "y2",
                mode = "lines+markers",
                marker = blackMarker,
                line = blackLine
            };

            var config = new
            {
                trace1 = trace1,
                trace2 = trace2,
            };

            if (chessModule is not null && chartElement.Context is not null)
            {

                if (liveUpdateStarted == false)
                {
                    await chessModule.InvokeVoidAsync("setDoubleNodeChart", chartElement, layout, config);
                }

                else
                {
                    if (PlayerWhiteData.Length == PlayerBlackData.Length)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Length > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Length > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }

        public async Task SetChartTimeUsageData()
        {
            var xaxis = new
            {
                tickfont = new { size = fontSizeTickFont },
                showgrid = false,
                zeroline = false,
                color = whiteColor
            };

            var yaxis = new
            {

                tickfont = new { size = fontSizeTickFont },
                tickmode = "auto",
                gridcolor = whiteGridColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = nticks,
                tickformat = ".1f",
                showgrid = true,
                color = whiteColor,
                automargin = true
            };

            var layout = new
            {
                title = new
                {
                    text = Title,
                    font = new
                    {
                        size = fontSizeTitle,
                        color = titleColor
                    },
                },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = false,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = margin,
            };

            var trace1 = new
            {
                x = BuildX(WhiteXStart, PlayerWhiteData.Length),
                y = PlayerWhiteData,
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = BuildX(BlackXStart, PlayerBlackData.Length),
                y = PlayerBlackData,
                type = "scatter",
                name = PlayerBlack,
                mode = "lines+markers",
                marker = blackMarker,
                line = blackLine
            };

            var config = new
            {
                trace1 = trace1,
                trace2 = trace2
            };

            if (chessModule is not null && chartElement.Context is not null)
            {
                if (liveUpdateStarted == false)
                {
                    await chessModule.InvokeVoidAsync("setTimeUsageChartData", chartElement, layout, config);
                }

                else
                {
                    if (PlayerWhiteData.Length == PlayerBlackData.Length)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Length > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Length > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }
    }
}