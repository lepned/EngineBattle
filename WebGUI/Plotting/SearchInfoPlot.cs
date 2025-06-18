using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using MudBlazor;
using MudBlazor.Charts;
using System.Drawing;
using WebGUI;
using static ChessLibrary.TypesDef.CoreTypes;

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
    //add callback action field here
    private Action<string, double, double> _callback;
    //private string blackGridColor = "#484952";

    public SearchInfoPlot(IJSObjectReference chessMod, ElementReference Qchart, ElementReference Nchart, string yTitle, string fen, string net, Action<string, double, double> callback)
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

    public SearchInfoPlot(IJSObjectReference chessMod, ElementReference Nchart, string yTitle, string fen, string net)
    {
      chessModule = chessMod;
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
        switch (n)
        {
          case 0:
            color = "#1f77b4"; // Blue
            break;
          case 1:
            color = "#ff7f0e"; // Orange
            break;
          case 2:
            color = "#2ca02c"; // Green
            break;
          case 3:
            color = "#9467bd"; // Purple
            break;
          case 4:
            color = "#ffbb78"; // Light Orange
            break;
          case 5:
            color = "#7f7f7f"; // Gray
            break;
          case 6:
            color = "#e377c2"; // Pink 
            break;
          case 7:
            color = "#17becf"; // Cyan
            break;
          case 8:
            color = "#bcbd22"; // Yellow-Green
            break;
          case 9:
            color = "#8c564b"; // Brown
            break;
          case 10:
            color = "#aec7e8"; // Light Blue
            break;
          default:
            color = "#7f7f7f"; // Default to light gray if index is out of range
            break;
        }
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
      if (moves.Any())
      {
        var move = moves[0];
        var frac = nDict[move.SANMove].Last();
        // var frac = move.Nodes / n;
        _callback?.Invoke(move.LANMove, move.P, frac);
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
