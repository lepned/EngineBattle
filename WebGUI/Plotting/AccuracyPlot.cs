using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using static ChessLibrary.GameAccuracyAnalysis;

namespace WebGUI.Plotting;

public class AccuracyPlot
{
    private readonly IJSObjectReference _chessModule;
    private readonly ElementReference _chartElement;
    private readonly string _moveIndicatorColor = "#FFD400";
    private int? _currentMoveIndex;

    private readonly string _titleColor = "#c1c1c4";
    private readonly string _whiteColor = "rgba(245, 245, 245, 0.75)";
    private readonly string _whiteGridColor = "#484952";

    public AccuracyPlot(IJSObjectReference chessModule, ElementReference chartElement)
    {
        _chessModule = chessModule;
        _chartElement = chartElement;
    }

    public async Task RenderAsync(GameAnalysisResult result)
    {
        var moves = result.Moves;
        if (moves.Length == 0) return;

        // Build x-axis (move numbers, half-moves displayed as "1w", "1b", "2w", etc.)
        var xValues = new int[moves.Length];
        var wpValues = new double[moves.Length]; // White's WP for area chart (0-100)
        var markerColors = new string[moves.Length];
        var hoverTexts = new string[moves.Length];

        for (int i = 0; i < moves.Length; i++)
        {
            var m = moves[i];
            xValues[i] = i;

            // WP from white's perspective for the chart y-axis
            var whiteWP = m.Color == "w" ? m.WinProbBefore : (1.0 - m.WinProbBefore);
            wpValues[i] = whiteWP * 100.0;

            markerColors[i] = GetClassificationColor(m.Classification);
            hoverTexts[i] = $"{m.MoveNumber}{(m.Color == "w" ? "." : "...")}{m.San} {m.Classification} ({m.MoveAccuracy:F0}%)";
        }

        // Build x-axis tick labels
        var tickVals = new int[moves.Length];
        var tickTexts = new string[moves.Length];
        for (int i = 0; i < moves.Length; i++)
        {
            tickVals[i] = i;
            tickTexts[i] = $"{moves[i].MoveNumber}";
        }

        // Area trace (White's win probability)
        var areaTrace = new
        {
            x = xValues,
            y = wpValues,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            fillcolor = "rgba(200, 200, 200, 0.15)",
            line = new { color = "rgba(180, 180, 180, 0.5)", width = 1 },
            hoverinfo = "skip",
            showlegend = false,
            name = "WP"
        };

        // 50% reference line
        var refLine = new
        {
            x = new[] { 0, moves.Length - 1 },
            y = new[] { 50.0, 50.0 },
            type = "scatter",
            mode = "lines",
            line = new { color = "rgba(255, 255, 255, 0.2)", width = 1, dash = "dash" },
            hoverinfo = "skip",
            showlegend = false,
            name = "50%"
        };

        // Classification scatter dots
        var scatterTrace = new
        {
            x = xValues,
            y = wpValues,
            type = "scatter",
            mode = "markers",
            marker = new
            {
                color = markerColors,
                size = 8,
                line = new { color = "rgba(0,0,0,0.3)", width = 1 }
            },
            text = hoverTexts,
            hoverinfo = "text",
            showlegend = false,
            name = "Moves"
        };

        var shapes = _currentMoveIndex.HasValue
            ? new object[] { BuildMoveIndicatorShape(_currentMoveIndex.Value) }
            : Array.Empty<object>();

        var layout = new
        {
            title = new
            {
                text = "Win Probability",
                font = new { size = 20, color = _titleColor }
            },
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            margin = new { l = 50, r = 15, b = 30, t = 50, pad = 2 },
            xaxis = new
            {
                tickfont = new { size = 12, color = _whiteColor },
                showgrid = false,
                zeroline = false,
                color = _whiteColor,
                // Show every 5th move number to avoid crowding
                tickmode = "array",
                tickvals = tickVals.Where((_, i) => i % 10 == 0 || i == moves.Length - 1).ToArray(),
                ticktext = tickTexts.Where((_, i) => i % 10 == 0 || i == moves.Length - 1).ToArray()
            },
            yaxis = new
            {
                range = new[] { 0.0, 100.0 },
                tickfont = new { size = 12, color = _whiteColor },
                gridcolor = _whiteGridColor,
                gridwidth = 1,
                showgrid = true,
                zeroline = false,
                color = _whiteColor,
                ticksuffix = "%",
                dtick = 25
            },
            shapes,
            showlegend = false
        };

        var data = new object[] { areaTrace, refLine, scatterTrace };

        await _chessModule.InvokeVoidAsync("setNdataPlot", _chartElement, layout, data);
    }

    public async Task UpdateMoveIndicatorAsync(int moveIndex)
    {
        _currentMoveIndex = moveIndex;
        if (_chessModule is not null && _chartElement.Context is not null)
        {
            await _chessModule.InvokeVoidAsync("updateMoveIndicator", _chartElement, moveIndex, _moveIndicatorColor);
        }
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
            line = new { color = _moveIndicatorColor, width = 2 },
            name = "moveIndicator"
        };
    }

    private static string GetClassificationColor(MoveClassification classification)
    {
        if (classification.IsBook || classification.IsForced) return "rgba(150, 150, 150, 0.6)";
        if (classification.IsBrilliant) return "#FFD700";    // gold
        if (classification.IsGreat) return "#00CED1";        // teal
        if (classification.IsBest) return "#4CAF50";         // green
        if (classification.IsExcellent) return "#81C784";    // light green
        if (classification.IsGood) return "rgba(200,200,200,0.5)"; // default
        if (classification.IsInaccuracy) return "#FFC107";   // amber
        if (classification.IsMistake) return "#FF9800";      // orange
        if (classification.IsBlunder) return "#F44336";      // red
        return "rgba(200,200,200,0.5)";
    }
}
