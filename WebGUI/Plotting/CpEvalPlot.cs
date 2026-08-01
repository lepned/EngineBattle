using System.Text.RegularExpressions;
using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using static ChessLibrary.MiscTypes;

namespace WebGUI.Plotting;

public class CpEvalPlot
{
    private const double MateEvalCap = 15.0;
    private static readonly Regex FirstMoveRegex = new(@"(?:\d+\.\.\.\.|^\d+\.)\s*([^\s]+)", RegexOptions.Compiled);

    private readonly IJSObjectReference _chessModule;
    private readonly ElementReference _chartElement;
    private readonly Dictionary<int, List<(int Depth, double Eval)>> _data = new();
    private readonly Dictionary<int, string> _moveNames = new();
    // Per PV line: how far the current unchanged-eval stretch reaches. Only materialised as
    // a point when the eval finally moves — see AddDataPoint.
    private readonly Dictionary<int, int> _runEnd = new();

    private static readonly string[] TraceColors =
    {
        "#1f77b4", // Blue
        "#ff7f0e", // Orange
        "#2ca02c", // Green
        "#9467bd", // Purple
        "#ffbb78", // Light Orange
        "#7f7f7f", // Gray
        "#e377c2", // Pink
        "#17becf", // Cyan
        "#bcbd22", // Yellow-Green
        "#8c564b", // Brown
        "#aec7e8", // Light Blue
        "#ff9896", // Light Red
        "#98df8a", // Light Green
        "#c5b0d5", // Light Purple
        "#f7b6d2", // Light Pink
        "#c49c94", // Light Brown
        "#dbdb8d", // Light Yellow-Green
        "#9edae5", // Light Cyan
        "#393b79", // Dark Blue
        "#e7969c", // Muted Rose
    };

    public CpEvalPlot(IJSObjectReference chessModule, ElementReference chartElement)
    {
        _chessModule = chessModule;
        _chartElement = chartElement;
    }

    public void AddDataPoint(int multiPV, int depth, EvalType eval, string pv)
    {
        if (eval.IsNA || depth <= 0)
            return;

        double value;
        if (eval.IsCP)
        {
            value = eval.Value;
        }
        else if (eval.IsMate)
        {
            var mateVal = eval.Value;
            value = mateVal >= 0 ? MateEvalCap : -MateEvalCap;
        }
        else
        {
            return;
        }

        var key = Math.Max(1, multiPV);

        // Track the first move of the PV as the trace label
        // Regex strips move-number prefixes like "1." (White) or "1...." (Black)
        if (!string.IsNullOrEmpty(pv))
        {
            var match = FirstMoveRegex.Match(pv);
            var firstMove = match.Success ? match.Groups[1].Value : pv.Split(' ', StringSplitOptions.RemoveEmptyEntries).FirstOrDefault();
            if (!string.IsNullOrEmpty(firstMove))
                _moveNames[key] = firstMove;
        }

        if (!_data.TryGetValue(key, out var points))
        {
            points = new List<(int, double)>();
            _data[key] = points;
        }

        // If same depth seen again for this PV line, update in place
        var existingIdx = points.FindIndex(p => p.Depth == depth);
        if (existingIdx >= 0)
        {
            points[existingIdx] = (depth, value);
            return;
        }

        if (points.Count > 0)
        {
            var last = points[^1];
            if (last.Eval == value)
            {
                // Unchanged eval: remember how far the flat stretch reaches, but do not
                // extend the series. A mate proven early repeats the same score for every
                // remaining iteration — 245 of them at the ply cap — and letting the line
                // follow would stretch the x-axis until the part that actually moved is
                // unreadable.
                _runEnd[key] = depth;
                return;
            }

            // The eval moved: close the flat stretch at its real end first, so the line
            // keeps its step shape instead of sloping across the whole plateau.
            if (_runEnd.TryGetValue(key, out var endDepth) && endDepth > last.Depth)
                points.Add((endDepth, last.Eval));
            _runEnd.Remove(key);
        }

        points.Add((depth, value));
    }

    public async Task UpdateChart(string engineName, int maxLines = 10)
    {
        if (_chessModule is null || _chartElement.Context is null)
            return;

        var traces = new List<object>();
        var visibleKeys = _data.OrderBy(k => k.Key).Take(maxLines);
        var showLegend = _data.Count > 1;

        foreach (var kvp in visibleKeys)
        {
            var pvIndex = kvp.Key;
            var points = kvp.Value.OrderBy(p => p.Depth).ToList();
            var colorIdx = Math.Min(pvIndex - 1, TraceColors.Length - 1);
            var color = TraceColors[Math.Max(0, colorIdx)];

            var traceName = _moveNames.TryGetValue(pvIndex, out var move) ? move : $"PV {pvIndex}";

            traces.Add(new
            {
                x = points.Select(p => p.Depth).ToArray(),
                y = points.Select(p => p.Eval).ToArray(),
                type = "scatter",
                mode = "lines+markers",
                line = new { color },
                marker = new { color, size = 4 },
                name = traceName,
                showlegend = showLegend,
            });
        }

        var layout = new
        {
            title = new { text = engineName, font = new { color = "#c1c1c4", size = 12 } },
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            showlegend = showLegend,
            margin = new { l = 40, r = 10, t = 30, b = 30 },
            xaxis = new
            {
                title = new { text = "Depth", font = new { color = "#c1c1c4", size = 11 } },
                showgrid = true,
                gridcolor = "#484952",
                zeroline = false,
                tickfont = new { color = "#c1c1c4", size = 10 },
                dtick = 2,
            },
            yaxis = new
            {
                title = new { text = "Eval (pawns)", font = new { color = "#c1c1c4", size = 11 } },
                showgrid = true,
                gridcolor = "#484952",
                zeroline = true,
                zerolinecolor = "#888",
                zerolinewidth = 1,
                tickfont = new { color = "#c1c1c4", size = 10 },
            },
            legend = new
            {
                font = new { color = "#c1c1c4", size = 10 },
            },
        };

        try
        {
            await _chessModule.InvokeVoidAsync("setQdataPlot", _chartElement, layout, traces);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"An error occurred in {nameof(UpdateChart)}: {ex.Message}");
        }
    }

    public void Clear()
    {
        _data.Clear();
        _moveNames.Clear();
        _runEnd.Clear();
    }

    public async Task ClearChart()
    {
        _data.Clear();
        _moveNames.Clear();
        _runEnd.Clear();
        if (_chessModule is null || _chartElement.Context is null)
            return;

        var layout = new
        {
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            showlegend = false,
            margin = new { l = 40, r = 10, t = 30, b = 30 },
            xaxis = new
            {
                showgrid = true,
                gridcolor = "#484952",
                zeroline = false,
                tickfont = new { color = "#c1c1c4", size = 10 },
            },
            yaxis = new
            {
                showgrid = true,
                gridcolor = "#484952",
                zeroline = true,
                zerolinecolor = "#888",
                tickfont = new { color = "#c1c1c4", size = 10 },
            },
        };

        try
        {
            await _chessModule.InvokeVoidAsync("clearQPlot", _chartElement, layout);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"An error occurred in {nameof(ClearChart)}: {ex.Message}");
        }
    }
}
