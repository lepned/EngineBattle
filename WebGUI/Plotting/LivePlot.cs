using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;

namespace WebGUI.Plotting
{
    public class LivePlot
    {
        public string Title { get; set; }
        public string YTitle { get; set; }
        public string PlayerWhite { get; set; }
        public string PlayerBlack { get; set; }
        public List<double> PlayerWhiteData { get; set; } = new List<double>();
        public List<double> PlayerBlackData { get; set; } = new List<double>();
        public List<int> MoveElements { get; set; } = Enumerable.Range(1, maxNumberOfMoves).ToList();

        public void SetStartMove(int moveNumber)
        {
            MoveElements = Enumerable.Range(moveNumber, maxNumberOfMoves).ToList();
        }

        private object BuildXAxis(bool showLine = false, bool showZeroline = false)
        {
            var start = MoveElements.Count > 0 ? MoveElements[0] : 1;
            var hasData = PlayerWhiteData.Count > 0 || PlayerBlackData.Count > 0;

            // Compute a sensible dtick based on current move span so labels never need to auto-rotate.
            // Mirrors the logic in chessInterop.js:updateLineEvalChartData.
            var pointCount = Math.Max(PlayerWhiteData.Count, PlayerBlackData.Count);
            var span = Math.Max(2, pointCount + 2);
            var raw = span / 10.0;
            var niceTicks = new[] { 1, 2, 5, 10, 20, 50, 100 };
            var dtickValue = niceTicks.FirstOrDefault(n => n >= raw);
            if (dtickValue == 0) dtickValue = niceTicks[^1];

            if (hasData)
                return new
                {
                    tickfont = new { size = fontSizeTickFont },
                    showgrid = false,
                    zeroline = false,
                    showline = showLine,
                    showzeroline = showZeroline,
                    color = whiteColor,
                    tick0 = 0,
                    dtick = dtickValue,
                    tickangle = 0,
                    rangemode = "nonnegative",
                };
            return new
            {
                tickfont = new { size = fontSizeTickFont },
                showgrid = false,
                zeroline = false,
                showline = showLine,
                showzeroline = showZeroline,
                color = whiteColor,
                tick0 = 0,
                dtick = dtickValue,
                tickangle = 0,
                rangemode = "nonnegative",
                range = new[] { start - 1, start + 11 },
            };
        }

        private const double maxEvalValue = 20;
        private const double minEvalValue = -20;
        private const int maxNumberOfMoves = 500;
        private string titleColor = "#c1c1c4";
        private string blackColor = "#141519"; //"#351c1c"; // "#361f05"; //"#8b4513" //"#6d6964"; //"#977c5f"; //"#141519";//"rgba(50, 51, 61, 0.75)";
        private string whiteColor = "rgba(245, 245, 245, 0.75)";
        private string whiteGridColor = "#484952"; // "rgb(85,109,143)";
        private bool liveUpdateStarted = false;
        private int fontSizeTitle = 20;
        private int fontSizeTickFont = 15;

        private IJSObjectReference chessModule;
        ElementReference chartElement;

        private object margin;
        private object whiteMarker;
        private object whiteLine;
        private object blackMarker;
        private object blackLine;
        private int nticks = 6;

        public LivePlot(IJSObjectReference chessMod, ElementReference chart, string white, string black, string title, string yTitle)
        {
            chessModule = chessMod;
            chartElement = chart;
            PlayerWhite = white;
            PlayerBlack = black;
            Title = title;
            YTitle = yTitle;

            margin = new
            {
                l = 50,
                r = 30,
                b = 30,
                t = 50,
                pad = 2,
            };

            whiteMarker = new
            {
                color = whiteColor,
                size = 6,
                symbol = "diamond", //"diamond-open",
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
                    color = "silver", //"lightgrey",
                    width = 1.5//0.75
                }
            };

            blackLine = new
            {
                width = 3,
                color = blackColor,
                opacity = 0.9
            };

        }

        public async Task ClearData(string white, string black, int startMove = 1)
        {
            try
            {
                PlayerWhiteData.Clear();
                PlayerBlackData.Clear();
                MoveElements.Clear();
                liveUpdateStarted = false;
                PlayerWhite = white;
                PlayerBlack = black;
                MoveElements = Enumerable.Range(startMove, maxNumberOfMoves).ToList();
                await Task.WhenAll(
                    SetEvalChartData(),
                    SetChartNodeData(),
                    SetChartTimeUsageData());
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

        public static double ClampEval(double value)
        {
            if (value > maxEvalValue)
            {
                return maxEvalValue;
            }
            if (value < minEvalValue)
            {
                return minEvalValue;
            }

            return value;
        }

        /// <summary>
        /// when mate is found, add the max or min eval value to the list
        /// </summary>
        /// <param name="isWhite"></param>
        /// <param name="ismate"></param>
        public void AddEvalData(bool isWhite, bool ismate, double yData)
        {
            var targetList = isWhite ? PlayerWhiteData : PlayerBlackData;
            if (ismate)
            {
                double mateValue;
                if (yData > 0)
                {
                    mateValue = maxEvalValue;
                }

                else
                {
                    mateValue = minEvalValue;
                }

                targetList.Add(mateValue);
            }

            else
            {
                targetList.Add(ClampEval(yData));
            }
        }

        public void AddData(bool isWhite, double yData, bool isNPS)
        {
            var targetList = isWhite ? PlayerWhiteData : PlayerBlackData;
            if (isNPS && targetList.Any(e => e > 0))
            {
                var max = targetList.Max();
                if (yData > max * 10)
                    targetList.Add(max * 1.01);
                else
                    targetList.Add(yData);
            }
            else
            {
                targetList.Add(yData);
            }
        }

        public async Task SetEvalChartData()
        {
            var xaxis = BuildXAxis();

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                gridwidth = 1,
                rangemode = "tozero",
                nticks = nticks,
                tickformat = ".1f",
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
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
                showgrid = true,
            };

            var trace1 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerWhiteData.ToArray(),
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerBlackData.ToArray(),
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
                if (liveUpdateStarted == false)
                {
                    await chessModule.InvokeVoidAsync("setLineEvalChartData", chartElement, layout, config);
                }
                else
                {
                    if (PlayerWhiteData.Count == PlayerBlackData.Count)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                // Sign-agnostic gate: flip to live-update path as soon as both sides have any data.
                // The previous Any(e => e > 0) check never tripped in black-winning games (eval is from
                // white POV, so both traces are entirely <= 0), which kept the chart on the full re-render
                // path with a static dtick=1, eventually causing Plotly to auto-rotate x-axis labels 90°.
                if (liveUpdateStarted == false)
                {
                    if (PlayerWhiteData.Count > 0 && PlayerBlackData.Count > 0)
                        liveUpdateStarted = true;
                }
            }
        }

        private bool SameYaxis()
        {
            if (PlayerBlackData.Count == 0 || PlayerWhiteData.Count == 0)
                return true;
            var avgW = PlayerWhiteData.Average();
            var avgB = PlayerBlackData.Average();
            var max = Math.Max(avgW, avgB);
            if (avgW == 0 || avgB == 0)
                return true;
            return max / avgB < 10 && max / avgW < 10;
        }

        public async Task SetChartNodeData()
        {
            if (SameYaxis())
                await SetSingleChartNodeData();
            else
                await SetDoubleChartNodeData();
        }

        public async Task SetSingleChartNodeData()
        {
            var xaxis = BuildXAxis();

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
                x = MoveElements.ToArray(),
                y = PlayerWhiteData.ToArray(),
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerBlackData.ToArray(),
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
                    if (PlayerWhiteData.Count == PlayerBlackData.Count)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                // FIX: start live updates once both sides have any positive value
                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Count > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Count > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }

        public async Task SetDoubleChartNodeData()
        {
            var xaxis = BuildXAxis(showLine: false, showZeroline: false);

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
                color = whiteColor, //"#000000",        
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
                x = MoveElements.ToArray(),
                y = PlayerWhiteData.ToArray(),
                type = "scatter",
                name = PlayerWhite,
                yaxis = "y1",
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerBlackData.ToArray(),
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
                    if (PlayerWhiteData.Count == PlayerBlackData.Count)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                // FIX: start live updates once both sides have any positive value
                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Count > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Count > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }


        public async Task SetChartTimeUsageData()
        {
            var xaxis = BuildXAxis();

            var yaxis = new
            {
                tickformatstops = new[] // use var to infer the type and [] to create an array
                {
                    new // use new with object initializer to create an anonymous type
                    {
                        dtickrange = new object[] { null, 1000 }, // use object[] to hold null and int values
                        value = "d" // use string for value
                    },
                    new
                    {
                        dtickrange = new object[] { 1000, null },
                        value = ".2s"
                    }
                },

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
                x = MoveElements.ToArray(),
                y = PlayerWhiteData.ToArray(),
                type = "scatter",
                name = PlayerWhite,
                mode = "lines+markers",
                marker = whiteMarker,
                line = whiteLine,
            };

            var trace2 = new
            {
                x = MoveElements.ToArray(),
                y = PlayerBlackData.ToArray(),
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
                    if (PlayerWhiteData.Count == PlayerBlackData.Count)
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace2, 1);
                    }
                    else
                    {
                        await chessModule.InvokeVoidAsync("updateLineEvalChartData", chartElement, trace1, 0);
                    }
                }

                // FIX: start live updates once both sides have any positive value
                if (liveUpdateStarted == false)
                {
                    var bothPlayedAMove =
                        PlayerWhiteData.Count > 0 && PlayerWhiteData.Any(e => e > 0) &&
                        PlayerBlackData.Count > 0 && PlayerBlackData.Any(e => e > 0);

                    if (bothPlayedAMove)
                        liveUpdateStarted = true;
                }
            }
        }
    }
}
