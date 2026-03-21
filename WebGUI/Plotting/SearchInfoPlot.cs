using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using MudBlazor;
using MudBlazor.Charts;
using System.Drawing;
using WebGUI;
using static ChessLibrary.TypesDef;
using static ChessLibrary.TypesDef.CoreTypes;
using static ChessLibrary.EngineTypes;

namespace WebGUI.Plotting
{
    public class SearchInfoPlot : IDisposable
    {
        public string EngineName { get; set; }
        public string Title { get; set; }
        public string YTitle { get; set; }
        public List<double> NumberOfNodesSearched { get; set; }
        public string FEN { get; set; }
        public string NN { get; set; }
        public Dictionary<string, List<double>> QvaluesDict { get; set; }
        public Dictionary<string, List<double>> NvaluesDict { get; set; }
        public Dictionary<int,double> PolicyDistribution { get; set; } = new Dictionary<int,double>();

        private IJSObjectReference chessModule;
        ElementReference QchartElement;
        ElementReference NchartElement;
        //private object[] annotations;
        private object margin;
        private int fontSizeTickFont = 15;
        private int fontSizeTitle = 20;
        private int fontSizeLegend = 15;
        //private string axisColor = "#c1c1c4";
        private string titleColor = "#c1c1c4";
        //private string blackColor = "#141519";
        private string whiteColor = "rgba(245, 245, 245, 0.75)";
        private string whiteGridColor = "#484952";

        // Stored rank sequence for re-rendering on toggle
        private List<PolicyRankInfo> _lastRankSequence;
        private string _lastRankNeuralNet;
        private string _lastRankType;
        //add callback action field here
        private readonly Func<string, double, double, Task> _callback;
        //private string blackGridColor = "#484952";

        public SearchInfoPlot(IJSObjectReference chessMod, ElementReference Qchart, ElementReference Nchart, string yTitle, string fen, string net, Func<string, double, double, Task> callback)
        {
            _callback = callback;
            chessModule = chessMod;
            QchartElement = Qchart;
            NchartElement = Nchart;
            YTitle = yTitle;
            QvaluesDict = new Dictionary<string, List<double>>();
            NvaluesDict = new Dictionary<string, List<double>>();
            NumberOfNodesSearched = new List<double>();
            FEN = fen;
            NN = net;
            Title = fen + "<br>" + $"{EngineName}, NN: {NN}";
            margin = new
            {
                l = 20,
                r = 40,
                b = 30,
                t = 50,
                pad = 2,
            };
        }

        public SearchInfoPlot(IJSObjectReference chessMod, ElementReference Qchart, ElementReference Nchart, string yTitle, string fen, string net)
        {
            chessModule = chessMod;
            QchartElement = Qchart;
            NchartElement = Nchart;
            YTitle = yTitle;
            QvaluesDict = new Dictionary<string, List<double>>();
            NvaluesDict = new Dictionary<string, List<double>>();
            NumberOfNodesSearched = new List<double>();
            FEN = fen;
            NN = net;
            Title = fen + "<br>" + $"{EngineName}, NN: {NN}";

            margin = new
            {
                l = 20,
                r = 40,
                b = 30,
                t = 50,
                pad = 2,
            };

        }

        public SearchInfoPlot(IJSObjectReference chessMod, ElementReference Nchart, string yTitle)
        {
            chessModule = chessMod;
            NchartElement = Nchart;
            YTitle = yTitle;
            QvaluesDict = new Dictionary<string, List<double>>();
            NvaluesDict = new Dictionary<string, List<double>>();
            NumberOfNodesSearched = new List<double>();
            margin = new
            {
                l = 20,
                r = 40,
                b = 30,
                t = 50,
                pad = 2,
            };

        }

        public void AddData(IEnumerable<NNValues> list)
        {
            var nodeCount = list.Sum(e => e.Nodes);
            //if (nodeCount < 3)
            //  return false;

            NumberOfNodesSearched.Add(nodeCount);

            foreach (var item in list)
            {
                AddMoveToQDict(item);
                AddMoveToNDict(item, nodeCount);
            }
        }

        public void AddMoveToQDict(NNValues item)
        {
            if (!QvaluesDict.ContainsKey(item.SANMove))
            {
                QvaluesDict.Add(item.SANMove, [item.Q]);
            }
            else
            {
                QvaluesDict[item.SANMove].Add(item.Q);
            }
        }

        public void AddMoveToNDict(NNValues item, double total)
        {
            var v = item.Nodes;
            var fraction = v == 0 ? 0 : v / total;

            if (!NvaluesDict.ContainsKey(item.SANMove))
            {
                NvaluesDict.Add(item.SANMove, new List<double>() { fraction });
            }
            else
            {
                NvaluesDict[item.SANMove].Add(fraction);
            }
        }

        // Add helper in the class (near other helpers)
        private static double Percentile(IReadOnlyList<int> values, double p)
        {
            if (values == null || values.Count == 0) return 1;
            var sorted = values.OrderBy(v => v).ToArray();
            p = Math.Clamp(p, 0.0, 100.0);
            var rank = (p / 100.0) * (sorted.Length - 1);
            var lo = (int)Math.Floor(rank);
            var hi = (int)Math.Ceiling(rank);
            if (lo == hi) return sorted[lo];
            return sorted[lo] + (sorted[hi] - sorted[lo]) * (rank - lo);
        }

        public void ClearSearchData()
        {
            QvaluesDict.Clear();
            NvaluesDict.Clear();
            NumberOfNodesSearched.Clear();
        }


        public async Task ResetNPlot()
        {
            QvaluesDict.Clear();
            NvaluesDict.Clear();
            NumberOfNodesSearched.Clear();

            if (chessModule is not null)
            {
                Title = $"{FEN}";
                YTitle = YTitle;

                var xaxis = new
                {
                    showgrid = false,
                    zeroline = false,
                    tickfont = new { size = fontSizeTickFont },
                };

                var yaxis = new
                {
                    title = YTitle,
                    showline = true,
                    showgrid = false,
                    tickfont = new { size = fontSizeTickFont },
                };

                var layout = new
                {
                    title = Title,
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)",
                    showlegend = true,
                    xaxis = xaxis,
                    yaxis = yaxis,
                    legend = new
                    {
                        title = new
                        {
                            text = $"{EngineName}, NN: {NN}",
                        }
                    },
                };

                try
                {
                    await chessModule.InvokeVoidAsync("clearQPlot", NchartElement, layout).AsTask();
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"An error occurred in {nameof(ResetNPlot)}: {ex.Message}");
                    // Handle the exception here
                }
            }
        }

        public async Task ResetPlot(bool resetChartElement)
        {
            QvaluesDict.Clear();
            NvaluesDict.Clear();
            NumberOfNodesSearched.Clear();

            if (resetChartElement && chessModule is not null)
            {
                Title = $"{FEN}";
                YTitle = YTitle;

                var xaxis = new
                {
                    showgrid = false,
                    zeroline = false,
                    tickfont = new { size = fontSizeTickFont },
                };

                var yaxis = new
                {
                    title = YTitle,
                    showline = true,
                    showgrid = false,
                    tickfont = new { size = fontSizeTickFont },
                };

                var layout = new
                {
                    title = Title,
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)",
                    showlegend = true,
                    xaxis = xaxis,
                    yaxis = yaxis,
                    legend = new
                    {
                        title = new
                        {
                            text = $"{EngineName}, NN: {NN}",
                        }
                    },
                };

                try
                {
                    await Task.WhenAll(
                      chessModule.InvokeVoidAsync("clearQPlot", QchartElement, layout).AsTask(),
                      chessModule.InvokeVoidAsync("clearQPlot", NchartElement, layout).AsTask());
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"An error occurred in {nameof(ResetPlot)}: {ex.Message}");
                    // Handle the exception here
                }
            }
        }

        public async Task UpdateLogLiveCharts(List<NNValues> list, int numberOfLines, double qDiff)
        {
            try
            {
                await Task.WhenAll(
                  SetNChartData(list, numberOfLines, qDiff),
                  SetQChartData(list, numberOfLines, qDiff));
            }
            catch (Exception ex)
            {
                // Handle the exception here
                Console.WriteLine($"An error occurred in {nameof(UpdateLogLiveCharts)}: {ex.Message}");
            }
        }

        public object GetTrace(int n, List<NNValues> moves, Dictionary<string, List<double>> nDict, bool topPolicyNotInMoveList, int maxNumberOfLines, bool qChartSelected)
        {
            string color;
            if (topPolicyNotInMoveList && n == maxNumberOfLines - 1)
            {
                color = "#d62728"; // Red
            }
            else
            {
                color = n switch
                {
                    0 => "#1f77b4",// Blue
                    1 => "#ff7f0e",// Orange
                    2 => "#2ca02c",// Green
                    3 => "#9467bd",// Purple
                    4 => "#ffbb78",// Light Orange
                    5 => "#7f7f7f",// Gray
                    6 => "#e377c2",// Pink 
                    7 => "#17becf",// Cyan
                    8 => "#bcbd22",// Yellow-Green
                    9 => "#8c564b",// Brown
                    10 => "#aec7e8",// Light Blue
                    _ => "#7f7f7f",// Default to light gray if index is out of range
                };
            }

            return new
            {
                x = NumberOfNodesSearched.ToArray(),
                y = nDict[moves[n].SANMove].ToArray(),
                type = "scatter",
                line = new { color = color },
                name = FormatMove(moves[n], qChartSelected),
            };
        }

        public async Task SetRankSequence(
    IEnumerable<PolicyRankInfo> sequence,
    string neuralNet,
    int maxTickLabels = 12,
    bool emphasizeBest = true,
    bool highlightHardest = true,
    int minWindow = 2,
    bool showRangeSlider = false,
    bool showSpikes = true,
    int yMaxCap = 10,                 // NEW: fixed cap (<=0 disables)
    string rankType = "Policy",
    double? yMaxPercentile = null,    // NEW: percentile cap (e.g., 95.0)
    bool isVisible = true)            // Skip Plotly render if Q-plot is hidden
        {
            if (sequence == null) return;
            var list = sequence.ToList();

            // Store for re-rendering on toggle
            _lastRankSequence = list;
            _lastRankNeuralNet = neuralNet;
            _lastRankType = rankType;

            // Don't render into a hidden container — Plotly's responsive handler
            // would corrupt it on window resize. ReRenderRankSequence handles it on toggle.
            if (!isVisible) return;
            if (list.Count == 0) return;

            var xWhite = new List<int>();
            var yWhite = new List<int>();
            var tWhite = new List<string>();

            var xBlack = new List<int>();
            var yBlack = new List<int>();
            var tBlack = new List<string>();

            var tickVals = Enumerable.Range(1, list.Count).ToArray();
            var tickText = new string[list.Count];
            var diff = new double[list.Count];

            for (int i = 0; i < list.Count; i++)
            {
                var info = list[i];
                int ply = i + 1;
                var played = info.PlayedMove?.SANMove ?? "?";
                var best = info.BestMove?.SANMove ?? "?";
                var text = $"{(info.IsWhite ? "W" : "B")} {played} (top: {best})";

                int moveNumber = info.MoveNumber;
                string prefix = info.IsWhite ? $"{moveNumber}. " : $"{moveNumber}... ";
                tickText[i] = $"{prefix}{best}";

                diff[i] = Math.Max(0, (double)info.PolicyRank - 1.0);

                if (info.IsWhite)
                {
                    xWhite.Add(ply);
                    yWhite.Add(info.PolicyRank);
                    tWhite.Add(text);
                }
                else
                {
                    xBlack.Add(ply);
                    yBlack.Add(info.PolicyRank);
                    tBlack.Add(text);
                }
            }

            if (list.Count > maxTickLabels && maxTickLabels > 0)
            {
                int step = (int)Math.Ceiling(list.Count / (double)maxTickLabels);
                for (int i = 0; i < tickText.Length; i++)
                    if (i % step != 0) tickText[i] = "";
            }
            var tickAngle = list.Count > 36 ? -60 : -35;

            // Hardest window (max average rank-1)
            int bestStart = -1, bestEnd = -1;
            double bestAvg = -1.0;
            if (highlightHardest && list.Count >= minWindow)
            {
                var prefix = new double[list.Count + 1];
                for (int i = 0; i < list.Count; i++) prefix[i + 1] = prefix[i] + diff[i];

                for (int start = 0; start <= list.Count - minWindow; start++)
                {
                    for (int end = start + minWindow - 1; end < list.Count; end++)
                    {
                        double sum = prefix[end + 1] - prefix[start];
                        int len = end - start + 1;
                        double avg = sum / len;
                        if (avg > bestAvg + 1e-12 || (Math.Abs(avg - bestAvg) < 1e-12 && len > (bestEnd - bestStart + 1)))
                        {
                            bestAvg = avg;
                            bestStart = start;
                            bestEnd = end;
                        }
                    }
                }
                if (bestAvg <= 0) bestStart = bestEnd = -1;
            }
                        
            // y-axis capping (fixed or percentile)
            var ranks = list.Select(r => r.PolicyRank).ToList();
            var maxRank = Math.Max(1, ranks.Max());
            double capFromPct = yMaxPercentile.HasValue ? Percentile(ranks, yMaxPercentile.Value) : double.PositiveInfinity;
            int fixedCap = yMaxCap > 0 ? yMaxCap : int.MaxValue;
            int usedMax = (int)Math.Min(maxRank, Math.Min(fixedCap, Math.Ceiling(capFromPct)));
            int clipped = ranks.Count(r => r > usedMax);
            Title = $"{rankType} rank per move ({neuralNet})";

            int[] whiteSizes = emphasizeBest ? yWhite.Select(r => r == 1 ? 10 : 7).ToArray() : Enumerable.Repeat(8, yWhite.Count).ToArray();
            int[] blackSizes = emphasizeBest ? yBlack.Select(r => r == 1 ? 10 : 7).ToArray() : Enumerable.Repeat(8, yBlack.Count).ToArray();

            var shapes = new List<object>();
            var annotations = new List<object>();

            if (bestStart >= 0 && bestEnd >= 0)
            {
                shapes.Add(new
                {
                    type = "rect",
                    xref = "x",
                    yref = "paper",
                    x0 = bestStart + 1 - 0.5,
                    x1 = bestEnd + 1 + 0.5,
                    y0 = 0,
                    y1 = 1,
                    fillcolor = "rgba(255,193,7,0.18)",
                    line = new { width = 0 },
                    layer = "below"
                });

                shapes.Add(new
                {
                    type = "line",
                    xref = "x",
                    yref = "paper",
                    x0 = bestStart + 1 - 0.5,
                    x1 = bestStart + 1 - 0.5,
                    y0 = 0,
                    y1 = 1,
                    line = new { color = "rgba(255,193,7,0.55)", width = 1, dash = "dot" },
                    layer = "below"
                });
                shapes.Add(new
                {
                    type = "line",
                    xref = "x",
                    yref = "paper",
                    x0 = bestEnd + 1 + 0.5,
                    x1 = bestEnd + 1 + 0.5,
                    y0 = 0,
                    y1 = 1,
                    line = new { color = "rgba(255,193,7,0.55)", width = 1, dash = "dot" },
                    layer = "below"
                });

                double centerX = (bestStart + bestEnd) / 2.0 + 1.0;
                var startLbl = tickText[bestStart] ?? (bestStart + 1).ToString();
                var endLbl = tickText[bestEnd] ?? (bestEnd + 1).ToString();

                if (clipped > 0)
                {
                    // small badge in top-right to indicate clipping
                    annotations.Add(new
                    {
                        x = 1.0,
                        y = 1.0,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "right",
                        yanchor = "top",
                        text = $"Clipped {clipped} plies > {usedMax}",
                        showarrow = false,
                        bgcolor = "rgba(30,31,36,0.9)",
                        bordercolor = "#616161",
                        borderwidth = 1,
                        borderpad = 3,
                        font = new { size = fontSizeLegend - 2, color = "rgba(245,245,245,0.9)" },
                        opacity = 1.0
                    });
                }

                var xaxis = new
                {
                    showgrid = false,
                    zeroline = false,
                    autorange = true,
                    tickfont = new { size = fontSizeTickFont },
                    color = whiteColor,
                    tickmode = "array",
                    tickvals = tickVals,
                    ticktext = tickText,
                    tickangle = tickAngle,
                    rangeslider = (showRangeSlider && list.Count > 24) ? new { visible = true, thickness = 0.06, bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(0,0,0,0)" } : null,
                    showspikes = showSpikes,
                    spikemode = showSpikes ? "across" : null,
                    spikecolor = showSpikes ? "#90CAF9" : null,
                    spikethickness = showSpikes ? 1 : 0,
                };

                var yaxis = new
                {
                    showgrid = true,
                    gridcolor = whiteGridColor,
                    tickfont = new { size = fontSizeTickFont },
                    color = whiteColor,
                    range = new double[] { usedMax + 0.5, 0.5 }, // use capped max so outliers don't crush the scale
                    dtick = 2
                };

                var legend = new
                {
                    orientation = "h",
                    x = 1.0,
                    xanchor = "right",
                    y = 1.15,
                    yanchor = "top",
                    bgcolor = "rgba(0,0,0,0)",
                    borderwidth = 0,
                    font = new { size = fontSizeLegend - 2, color = titleColor }
                };

                var layout = new
                {
                    legend = legend,
                    title = new { text = Title, font = new { size = fontSizeTitle - 2, color = titleColor } },
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)",
                    showlegend = true,
                    hovermode = "x unified",
                    hoverlabel = new { bgcolor = "rgba(30,31,36,0.95)", font = new { color = whiteColor } },
                    dragmode = "pan",
                    uirevision = "rank-seq", // preserve zoom/pan on subsequent updates
                    xaxis = xaxis,
                    yaxis = yaxis,
                    margin = new { l = 20, r = 8, b = 60, t = 40, pad = 2 },
                    shapes = shapes,
                    annotations = annotations
                };

                var whiteTrace = new
                {
                    x = xWhite.ToArray(),
                    y = yWhite.ToArray(),
                    type = "scatter",
                    mode = "lines+markers",
                    name = "White",
                    marker = new { color = "#29B6F6", size = whiteSizes },
                    line = new { color = "#29B6F6" },
                    text = tWhite.ToArray(),
                    hovertemplate = "%{text}<br>Ply %{x}<br>Rank %{y}<extra></extra>"
                };

                var blackTrace = new
                {
                    x = xBlack.ToArray(),
                    y = yBlack.ToArray(),
                    type = "scatter",
                    mode = "lines+markers",
                    name = "Black",
                    marker = new { color = "#B0BEC5", size = blackSizes },
                    line = new { color = "#B0BEC5" },
                    text = tBlack.ToArray(),
                    hovertemplate = "%{text}<br>Ply %{x}<br>Rank %{y}<extra></extra>"
                };

                var traces = new List<object> { whiteTrace, blackTrace };

                if (chessModule is not null && QchartElement.Context is not null)
                    await chessModule.InvokeVoidAsync("setQdataPlot", QchartElement, layout, traces);
            }
        }

        /// Re-renders the last rank sequence chart. Call after the Q-plot container becomes visible.
        public async Task ReRenderRankSequence()
        {
            if (_lastRankSequence != null && _lastRankSequence.Count > 0)
                await SetRankSequence(_lastRankSequence, _lastRankNeuralNet, rankType: _lastRankType ?? "Policy");
        }


        public async Task SetPolicyDistributionCombinedBucketed(
    IEnumerable<Tuple<int, int>> distribution,
    string neuralNet,
    int minDistinctRanks = 4,
    int tailStart = 11,
    string rankType = "Policy")
        {

            var maxNumberOfRanks = distribution.Max(e => e.Item1);
            // Safety
            if (tailStart <= 1) tailStart = 11;
            if (minDistinctRanks < 1) minDistinctRanks = 1;

            // Build dictionary
            var dict = new Dictionary<int, int>();
            if (distribution != null)
                foreach (var (rank, count) in distribution)
                    dict[rank] = dict.TryGetValue(rank, out var v) ? v + count : count;

            var total = Math.Max(1, dict.Sum(kv => kv.Value));

            // Categories 1..(tailStart-1) plus "tailStart+"
            var singles = Enumerable.Range(1, Math.Max(1, tailStart - 1)).ToArray();
            var xCats = singles.Select(r => r.ToString())
                               .Concat(new[] { $"{tailStart}..{maxNumberOfRanks}" })
                               .ToArray();

            // Values (percent) for combined (normalized on total)
            double GetShare(int r) => dict.TryGetValue(r, out var c) ? (double)c / total : 0.0;
            var singlesY = singles.Select(GetShare).ToArray();
            var tailY = dict.Where(kv => kv.Key >= tailStart).Sum(kv => kv.Value) / (double)total;
            var yCombined = singlesY.Concat(new[] { tailY }).ToArray();

            // Headroom for outside labels
            double yMax = yCombined.Length > 0 ? yCombined.Max() : 0.0;
            double headroom = Math.Clamp(yMax * 0.18 + 0.04, 0.08, 0.25);
            double yUpperFinal = Math.Max(0.10, Math.Min(1.0, yMax + headroom));

            // Text labels: whole numbers without %
            var text = yCombined.Select(v => (v * 100.0).ToString("0", System.Globalization.CultureInfo.InvariantCulture)).ToArray();

            var xaxis = new
            {
                showgrid = false,
                zeroline = false,
                tickfont = new { size = fontSizeTickFont },
                color = whiteColor,
                type = "category",
                categoryorder = "array",
                categoryarray = xCats,
                automargin = true,
                title = new { text = "Rank", standoff = 22, font = new { size = fontSizeLegend - 2, color = titleColor } }
            };

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                automargin = true,
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".0%",
                color = whiteColor,
                rangemode = "tozero",
                range = new[] { 0.0, yUpperFinal },
                title = new { text = $"{rankType} percentage", font = new { size = fontSizeLegend - 2, color = titleColor }, standoff = 18 }
            };

            Title = $"{rankType} distribution {neuralNet}";

            var layout = new
            {
                legend = new { font = new { size = fontSizeLegend, color = titleColor } },
                title = new { text = Title, font = new { size = fontSizeTitle, color = titleColor } },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = true,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = new { l = 70, r = 40, b = 56, t = 80, pad = 2 },
                uniformtext = new { mode = "hide", minsize = 10 },
                barmode = "group",
                bargap = 0.25,
                bargroupgap = 0.1
            };

            var trace = new
            {
                x = xCats,
                y = yCombined,
                type = "bar",
                name = "All moves",
                marker = new
                {
                    color = System.Drawing.Color.GhostWhite,
                    opacity = 0.95,
                    line = new { color = "#1e1f24", width = 1 }
                },
                text = text,
                textposition = "outside",
                texttemplate = "%{text}",
                cliponaxis = false,
                constraintext = "none",
                textfont = new { color = whiteColor, size = fontSizeTickFont },
                hovertemplate = "Rank %{x}<br>%{y:.1%}<extra></extra>"
            };

            var traces = new List<object> { trace };

            if (chessModule is not null && NchartElement.Context is not null)
                await chessModule.InvokeVoidAsync("setNdataPlot", NchartElement, layout, traces);
        }

        public async Task SetPolicyDistributionBucketed(
            IEnumerable<Tuple<int, int>> distribution,
            IEnumerable<Tuple<int, int>> wDistribution,
            IEnumerable<Tuple<int, int>> bDistribution,
            string neuralNet,
            int minDistinctRanks = 6,
            int tailStart = 11,
            string rankType = "Policy")
        {
            if (tailStart <= 1) tailStart = 11;
            if (minDistinctRanks < 1) minDistinctRanks = 1;

            var maxNumberOfRanks = distribution.Max(e => e.Item1);
            // Build dicts
            var wDict = new Dictionary<int, int>();
            var bDict = new Dictionary<int, int>();
            void Acc(Dictionary<int, int> d, IEnumerable<Tuple<int, int>> src)
            {
                if (src == null) return;
                foreach (var (rank, count) in src)
                    d[rank] = d.TryGetValue(rank, out var v) ? v + count : count;
            }
            Acc(wDict, wDistribution);
            Acc(bDict, bDistribution);

            var wSum = Math.Max(1, wDict.Sum(kv => kv.Value));
            var bSum = Math.Max(1, bDict.Sum(kv => kv.Value));

            // Categories 1..(tailStart-1) plus "tailStart+"
            var singles = Enumerable.Range(1, Math.Max(1, tailStart - 1)).ToArray();
            var xCats = singles.Select(r => r.ToString())
                               .Concat(new[] { $"{tailStart}..{maxNumberOfRanks}" })
                               .ToArray();

            double WShare(int r) => wDict.TryGetValue(r, out var c) ? (double)c / wSum : 0.0;
            double BShare(int r) => bDict.TryGetValue(r, out var c) ? (double)c / bSum : 0.0;

            var yWhiteSingles = singles.Select(WShare).ToArray();
            var yBlackSingles = singles.Select(BShare).ToArray();

            var yWhiteTail = wDict.Where(kv => kv.Key >= tailStart).Sum(kv => kv.Value) / (double)wSum;
            var yBlackTail = bDict.Where(kv => kv.Key >= tailStart).Sum(kv => kv.Value) / (double)bSum;

            var yWhite = yWhiteSingles.Concat(new[] { yWhiteTail }).ToArray();
            var yBlack = yBlackSingles.Concat(new[] { yBlackTail }).ToArray();

            // Headroom
            double yMax = Math.Max(yWhite.Length > 0 ? yWhite.Max() : 0.0, yBlack.Length > 0 ? yBlack.Max() : 0.0);
            double headroom = Math.Clamp(yMax * 0.18 + 0.04, 0.08, 0.25);
            double yUpperFinal = Math.Max(0.10, Math.Min(1.0, yMax + headroom));

            // Labels (no %)
            var whiteText = yWhite.Select(v => (v * 100.0).ToString("0", System.Globalization.CultureInfo.InvariantCulture)).ToArray();
            var blackText = yBlack.Select(v => (v * 100.0).ToString("0", System.Globalization.CultureInfo.InvariantCulture)).ToArray();

            var xaxis = new
            {
                showgrid = false,
                zeroline = false,
                tickfont = new { size = fontSizeTickFont },
                color = whiteColor,
                type = "category",
                categoryorder = "array",
                categoryarray = xCats,
                automargin = true,
                title = new { text = "Rank", standoff = 22, font = new { size = fontSizeLegend - 2, color = titleColor } }
            };

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                automargin = true,
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".0%",
                color = whiteColor,
                rangemode = "tozero",
                range = new[] { 0.0, yUpperFinal },
                title = new { text = $"{rankType} percentage", font = new { size = fontSizeLegend - 2, color = titleColor }, standoff = 18 }
            };

            Title = $"{rankType} distribution {neuralNet}";

            var layout = new
            {
                legend = new { font = new { size = fontSizeLegend, color = titleColor } },
                title = new { text = Title, font = new { size = fontSizeTitle, color = titleColor } },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = true,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = new { l = 70, r = 40, b = 56, t = 80, pad = 2 },
                uniformtext = new { mode = "hide", minsize = 10 },
                barmode = "group",
                bargap = 0.25,
                bargroupgap = 0.1
            };

            var whiteTrace = new
            {
                x = xCats,
                y = yWhite,
                type = "bar",
                name = "White",
                marker = new
                {
                    color = System.Drawing.Color.GhostWhite,
                    opacity = 0.95,
                    line = new { color = "#1e1f24", width = 1 }
                },
                text = whiteText,
                textposition = "outside",
                texttemplate = "%{text}",
                cliponaxis = false,
                constraintext = "none",
                textfont = new { color = whiteColor, size = fontSizeTickFont },
                hovertemplate = "Rank %{x}<br>White %{y:.1%}<extra></extra>",
                offsetgroup = "white",
                legendgroup = "side"
            };

            var blackTrace = new
            {
                x = xCats,
                y = yBlack,
                type = "bar",
                name = "Black",
                marker = new
                {
                    opacity = 0.95,
                    line = new { color = "#1e1f24", width = 1 },
                    color = "#B0BEC5",
                    pattern = new { shape = "/", size = 6, solidity = 0.3 }
                },
                text = blackText,
                textposition = "outside",
                texttemplate = "%{text}",
                cliponaxis = false,
                constraintext = "none",
                textfont = new { color = whiteColor, size = fontSizeTickFont },
                hovertemplate = "Rank %{x}<br>Black %{y:.1%}<extra></extra>",
                offsetgroup = "black",
                legendgroup = "side"
            };

            var traces = new List<object> { whiteTrace, blackTrace };

            if (chessModule is not null && NchartElement.Context is not null)
                await chessModule.InvokeVoidAsync("setNdataPlot", NchartElement, layout, traces);
        }


        public async Task SetPolicyDistributionCombined(
    IEnumerable<Tuple<int, int>> distribution,
    string neuralNet, int nRanks, string rankType = "Policy")
        {
            // Build combined distribution dictionary
            PolicyDistribution.Clear();
            var sum = Math.Max(1, distribution?.Sum(e => e.Item2) ?? 0);
            if (distribution != null)
            {
                foreach (var (rank, count) in distribution)
                    PolicyDistribution[rank] = (double)count / sum;
            }

            Title = $"{rankType} distribution {neuralNet}";

            // Natural order: 1..nRanks (missing ranks become 0)
            var ranksToShow = Enumerable.Range(1, Math.Max(1, nRanks)).ToArray();
            var xCats = ranksToShow.Select(r => r.ToString()).ToArray();
            var yCombined = ranksToShow
                .Select(r => PolicyDistribution.TryGetValue(r, out var v) ? v : 0.0)
                .ToArray();

            // Headroom so outside labels don’t clamp
            double yMax = yCombined.Length > 0 ? yCombined.Max() : 0.0;
            double headroom = Math.Clamp(yMax * 0.18 + 0.04, 0.08, 0.25);
            double yUpper = Math.Min(1.0, yMax + headroom);
            double yUpperFinal = Math.Max(0.10, yUpper);

            // Text labels as whole numbers (no %)
            var combinedText = yCombined
                .Select(v => (v * 100.0).ToString("0", System.Globalization.CultureInfo.InvariantCulture))
                .ToArray();

            var xaxis = new
            {
                showgrid = false,
                zeroline = false,
                tickfont = new { size = fontSizeTickFont },
                color = whiteColor,
                type = "category",
                categoryorder = "array",
                categoryarray = xCats,
                automargin = true, // ensure Plotly reserves space for title/labels
                title = new
                {
                    text = "Rank",
                    standoff = 22, // add spacing between ticks and axis title
                    font = new { size = fontSizeLegend - 2, color = titleColor }
                }
            };

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                automargin = true,
                showgrid = true,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".0%",
                color = whiteColor,
                rangemode = "tozero",
                range = new[] { 0.0, yUpperFinal },
                title = new { text = $"{rankType} percentage", font = new { size = fontSizeLegend - 2, color = titleColor }, standoff = 18 }
            };

            var layout = new
            {
                legend = new { font = new { size = fontSizeLegend, color = titleColor } },
                title = new { text = Title, font = new { size = fontSizeTitle, color = titleColor } },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = true,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = new { l = 70, r = 40, b = 56, t = 80, pad = 2 }, // a bit more bottom space
                uniformtext = new { mode = "hide", minsize = 10 },
                barmode = "group",
                bargap = 0.25,
                bargroupgap = 0.1
            };

            var combinedTrace = new
            {
                x = xCats,
                y = yCombined,
                type = "bar",
                name = "All moves",
                marker = new
                {
                    color = System.Drawing.Color.GhostWhite,
                    opacity = 0.95,
                    line = new { color = "#1e1f24", width = 1 }
                },
                text = combinedText,
                textposition = "outside",
                texttemplate = "%{text}",
                cliponaxis = false,
                constraintext = "none",
                textfont = new { color = whiteColor, size = fontSizeTickFont },
                hovertemplate = "Rank %{x}<br>%{y:.1%}<extra></extra>"
            };

            var traces = new List<object> { combinedTrace };

            if (chessModule is not null && NchartElement.Context is not null)
                await chessModule.InvokeVoidAsync("setNdataPlot", NchartElement, layout, traces);

            await Task.CompletedTask;
        }
        
        // Convenience overload: build combined distribution from separate white/black counts
        public async Task SetPolicyDistributionCombinedFromWB(
            IEnumerable<Tuple<int, int>> wDistribution,
            IEnumerable<Tuple<int, int>> bDistribution,
            string neuralNet, int nRanks)
        {
            var dict = new Dictionary<int, int>();
            void Acc(IEnumerable<Tuple<int, int>> src)
            {
                if (src == null) return;
                foreach (var (rank, count) in src)
                    dict[rank] = dict.TryGetValue(rank, out var v) ? v + count : count;
            }
            Acc(wDistribution);
            Acc(bDistribution);

            var combined = dict.OrderBy(kv => kv.Key)
                               .Select(kv => Tuple.Create(kv.Key, kv.Value));

            await SetPolicyDistributionCombined(combined, neuralNet, nRanks);
        }
        
        public async Task SetNChartData(List<NNValues> list, int numberOfLines, double qDiff)
        {
            if (list == null || list.Count < 1 || numberOfLines <= 0)
            {
                //Console.WriteLine($"Returned early in SetNChartData, probably too few values in list - list size: {list.Count}");
                return;
            }

            Title = $"{FEN}";

            var xaxis = new
            {
                showgrid = false,
                zeroline = false,
                autorange = NumberOfNodesSearched.Count >= 2,
                tickfont = new { size = fontSizeTickFont },
                tickformat = NumberOfNodesSearched.Count >= 2 ? ".2s" : null, // Use scientific notation if there is data
                color = whiteColor,
                range = NumberOfNodesSearched.Count < 2 ? new double[] { 0, 1 } : null, // Set range to [0, 1] if no data
                tickvals = NumberOfNodesSearched.Count < 2 ? new double[] { 0, 1 } : null // Explicitly set tick values when no data
            };

            var yaxis = new
            {
                range = new double[] { 10.0, 20.0, 40.0, 60.0, 80.0, 100.0 },
                gridcolor = whiteGridColor,
                gridwidth = 1, // Set the gridline width        
                automargin = true,
                showgrid = true,
                nticks = 6,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".0%", // Optional: Set the tick format
                color = whiteColor,
                rangemode = "tozero" // Ensure y-axis starts at 0
            };

            bool showTitle = Title.Count() > 15;

            Title = showTitle ? Title + "<br>" + $"{EngineName}, NN: {NN}" : "Log Live Stats (LC0/Ceres)";

            var layout = new
            {
                legend = new
                {
                    font = new
                    {
                        size = fontSizeLegend, // adjust the global legend font size here
                        color = titleColor
                    }
                },

                title = new
                {
                    text = $"Top {numberOfLines} visited moves",
                    font = new
                    {
                        size = fontSizeTitle,
                        color = titleColor
                        //family = "Arial Black"
                    },
                },
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = true,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = margin,
            };

            var filterN = (NNValues item) => item.Nodes;
            var filterQ = (NNValues item) => item.Q;
            var filterP = (NNValues item) => item.P;
            var topMove = list.Max(e => e.Q);
            var topPolicy = list.MaxBy(e => e.P);
            var lowest = topMove - qDiff;

            var potentialMoves =
              list
              .Where(e => e != null && e.Q > lowest)
              .OrderByDescending(filterN)
              .Take(Math.Min(list.Count, numberOfLines))
              .ToList();


            if (!potentialMoves.Exists(e => e == topPolicy))
            {
                potentialMoves.Add(topPolicy);
            }

            var maxNumberOfLines = Math.Min(10, potentialMoves.Count);
            bool topPolicyNotInMoveList = maxNumberOfLines > numberOfLines;
            var moves = potentialMoves;

            if (NvaluesDict.Count == 0)
            {
                return;
            }

            var nDict = NvaluesDict;
            if (moves.Count > 0 && _callback is not null)
            {                
                var move = moves[0];
                var frac = nDict[move.SANMove].Last();
                await _callback?.Invoke(move.LANMove, move.P, frac);                
            }

            var traces = new List<object>();
            for (int i = 0; i < maxNumberOfLines; i++)
            {
                traces.Add(GetTrace(i, moves, nDict, topPolicyNotInMoveList, maxNumberOfLines, false));
            }

            if (chessModule is not null && NchartElement.Context is not null && moves.Count > 0)
            {
                await chessModule.InvokeVoidAsync("setNdataPlot", NchartElement, layout, traces);
            }

            await Task.CompletedTask;
        }

        public static List<T> OrderAndSortQList<T>(List<T> listN, List<T> listQ)
        {
            // Use a HashSet for fast lookups and to avoid modifying the original listQ
            var listQSet = new HashSet<T>(listQ);

            // Create a result list
            var resultList = new List<T>();

            // Add items from listN that are also in listQ (order preserved)
            foreach (var item in listN)
            {
                if (listQSet.Remove(item)) // Check and remove from the HashSet
                {
                    resultList.Add(item); // Add to resultList in the order of listN
                }
            }

            // Append remaining items from listQSet (not in listN)
            resultList.AddRange(listQSet);

            return resultList;
        }

        public async Task SetQChartData(List<NNValues> list, int numberOfLines, double qDiff)
        {
            if (list == null || list.Count < 1 || numberOfLines <= 0)
            {
                //Console.WriteLine($"Returned early in SetNChartData, probably too few values in list - list size: {list.Count}");
                return;
            }

            Title = $"{FEN}";

            var xaxis = new
            {
                zeroline = false,
                showgrid = false,
                autorange = true,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".2s",
                color = whiteColor
            };

            var yaxis = new
            {
                gridcolor = whiteGridColor,
                gridwidth = 1, // Set the gridline width
                autorange = true,
                automargin = true,
                showgrid = true,
                nticks = 6,
                tickfont = new { size = fontSizeTickFont },
                tickformat = ".2f",
                color = whiteColor
            };

            bool showTitle = Title.Count() > 15;

            Title = showTitle ? Title + "<br>" + $"{EngineName}, NN: {NN}" : "Log Live Stats (LC0/Ceres)";


            var layout = new
            {
                legend = new
                {
                    font = new
                    {
                        size = fontSizeLegend, // adjust the global legend font size here
                        color = titleColor
                    }
                },

                title = new
                {
                    text = $"Top {numberOfLines} Q-values (eval)",
                    font = new
                    {
                        size = fontSizeTitle,
                        color = titleColor
                    },
                },

                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                showlegend = true,
                xaxis = xaxis,
                yaxis = yaxis,
                margin = margin,
            };

            var filterN = (NNValues item) => item.Nodes;
            var filterQ = (NNValues item) => item.Q;
            var filterP = (NNValues item) => item.P;
            var topMove = list.Max(e => e.Q);
            var topPolicy = list.MaxBy(e => e.P);
            var lowest = topMove - qDiff;

            var qMoves =
              list
              .Where(e => e != null && e.Q > lowest)
              .OrderByDescending(filterQ)
              .Take(Math.Min(list.Count, numberOfLines))
              .ToList();

            var nMoves =
              list
              .Where(e => e != null && e.Q > lowest)
              .OrderByDescending(filterN)
              .Take(Math.Min(list.Count, numberOfLines))
              .ToList();

            //if (!qMoves.Exists(e => e == topPolicy))
            //  qMoves.Add(topPolicy);
            var potentialMoves = OrderAndSortQList(nMoves, qMoves);

            var maxNumberOfLines = Math.Min(10, potentialMoves.Count);
            bool topPolicyNotInMoveList = false; //maxNumberOfLines > numberOfLines;
            var moves = new List<NNValues>(potentialMoves.Take(maxNumberOfLines));

            if (QvaluesDict.Count == 0)
                return;

            var qDict = QvaluesDict;

            var traces = new List<object>();
            for (int i = 0; i < maxNumberOfLines; i++)
            {
                traces.Add(GetTrace(i, moves, qDict, topPolicyNotInMoveList, maxNumberOfLines, true));
            }

            if (chessModule is not null && QchartElement.Context is not null && moves.Count > 0)
            {
                await chessModule.InvokeVoidAsync("setQdataPlot", QchartElement, layout, traces);
            }

            await Task.CompletedTask;
        }

        string FormatMove(NNValues move, bool qChartSelected)
        {
            if (qChartSelected)
            {
                var fraction = NvaluesDict[move.SANMove].Last();
                var moveWithQFormatted = $"{move.SANMove}, P: {move.P / 100:p1} N: {fraction:p1}";
                return moveWithQFormatted;
            }
            else
            {
                //var eval = Utilities.EvalLogistic.logisticToCentipawn(move.Q) / 100;
                var e = move.E != 0.0 ? $"E: ± {(2 * move.E):f2}" : "";
                var moveFormatted = $"{move.SANMove}, P: {move.P / 100:p1} Q: {move.Q:f2} {e}";
                return moveFormatted;
            }
        }
        public void Dispose()
        {
            //ResetPlot();
        }
    }
}
