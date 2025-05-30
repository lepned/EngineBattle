using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using WebGUI;
using ChessLibrary;
using static ChessLibrary.TypesDef.PGNTypes;
using static MudBlazor.Colors;

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
    //private double scalingFactor = 4.0;

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

    private double PseudoLogTransform(double x)
    {
      //var abs = Math.Abs(x);
      //var log10 = (Math.Sign(x) * scalingFactor) * Math.Log10(abs + 1);
      //var log2 = Math.Sign(x) * Math.Log(1 + Math.Abs(x));
      //Console.WriteLine($"Log10 PseudoLogTransform: in: {x} out: {log10}");
      //Console.WriteLine($"Log PseudoLogTransform: in: {x} out: {log2}");
      var cap10 = Math.Abs(x) > 10 ? 10 * Math.Sign(x) : x;
      return cap10;
      //return log10;
    }

    public void AssignEvalsFromPGN(PgnGame game)
    {
      ClearData(PlayerWhite, PlayerBlack);
      var moveStats = Parser.PGNExtractor.extractEngineStats(game);
      PlayerWhiteData = 
        moveStats.Moves
        .Where(e => e.Player == PlayerWhite)
        .Reverse().SkipWhile(e => e.wv == 0.0).Reverse()
        .Select(e => PseudoLogTransform(e.wv)).ToArray();

      PlayerBlackData =
        moveStats.Moves
        .Where(e => e.Player == PlayerBlack)
        .Reverse().SkipWhile(e => e.wv == 0.0).Reverse()
        .Select(e => PseudoLogTransform(e.wv)).ToArray();
      var maxNumberOfMoves = Math.Max(PlayerWhiteData.Length, PlayerBlackData.Length);
      MoveElements = maxNumberOfMoves == 0 ? new List<int>() : Enumerable.Range(0, maxNumberOfMoves).ToList();      
    }

    public void ClearData(string white, string black)
    {
      try
      {
        PlayerWhiteData = Array.Empty<double>();
        PlayerBlackData = Array.Empty<double>();
        MoveElements.Clear();       
        liveUpdateStarted = false;
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

      var maxMoves = Math.Max(PlayerWhiteData.Length, PlayerBlackData.Length);
      //get the min and max score of both side
      var minRange = Math.Min(PlayerBlackData.Min(), PlayerWhiteData.Min());      
      var maxRange = Math.Max(PlayerBlackData.Max(), PlayerWhiteData.Max());
      var delta = Math.Abs(maxRange - minRange);
      var extraLimit = delta > 3 ? 0.5 : 0.2;
      var (min,max) = Math.Abs(maxRange) > Math.Abs(minRange) ? (minRange, maxRange + extraLimit) : (minRange - extraLimit, maxRange);
      

      MoveElements.AddRange(Enumerable.Range(0, maxMoves));
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
        //type = "log",
        nticks = 8,
        //dtick = delta < 2 ? 0.2 : 2,
        tickformat = ".1f",
        showgrid = true,
        tickfont = new { size = fontSizeTickFont },
        color = whiteColor,
        range = new[] { min, max }
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

    private bool SameYaxis()
    {
      if (PlayerBlackData.Length == 0 || PlayerWhiteData.Length == 0)
        return true;
      var maxW = PlayerWhiteData.Max();
      var maxB = PlayerBlackData.Max();
      var max = Math.Max(maxW, maxB);
      if (maxW == 0 || maxB == 0)
        return true;
      return max / maxB < 10 && max / maxW < 10;
    }

    public async Task SetChartNodeData()
    {
      var maxMoves = Math.Max(PlayerWhiteData.Length, PlayerBlackData.Length);
      //Console.WriteLine($"{nameof(SetChartNodeData)} - white move number: {PlayerWhiteData.Count}, black move number: {PlayerBlackData.Count}");
      //MoveElements.Clear();
      MoveElements.AddRange(Enumerable.Range(0, maxMoves));
      //MoveElements = Enumerable.Range(0, maxMoves).ToList();
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
        color = whiteColor
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
          var bothPlayedAMove = PlayerWhiteData.Length > 10 && PlayerWhiteData.Last() > 0 && PlayerBlackData.Length > 10 && PlayerBlackData.Last() > 0;
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
          var bothPlayedAMove = PlayerWhiteData.Length > 10 && PlayerWhiteData.Last() > 0 && PlayerBlackData.Length > 10 && PlayerBlackData.Last() > 0;
          if (bothPlayedAMove)
            liveUpdateStarted = true;
        }
      }
    }


    public async Task SetChartTimeUsageData()
    {
      var maxMoves = Math.Max(PlayerWhiteData.Length, PlayerBlackData.Length);
      var minMoves = Math.Min(maxMoves, PlayerBlackData.Length);
      //MoveElements.Clear();
      MoveElements.AddRange(Enumerable.Range(0, maxMoves));
      if (maxMoves > minMoves + 1)
      {
        var white = PlayerWhiteData.Length;
        var black = PlayerBlackData.Length;
        var msg = $"In {nameof(SetChartTimeUsageData)}: moves are not distributed equally, number of moves white {white} and black {black}";
        Console.WriteLine(msg);
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
        color = whiteColor
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
          var bothPlayedAMove = PlayerWhiteData.Length > 0 && PlayerWhiteData.Any(e => e > 0) && PlayerBlackData.Length > 0 && PlayerBlackData.Any(e => e > 0);
          if (bothPlayedAMove)
            liveUpdateStarted = true;
        }
      }
    }
  }


  public class EvalPgnPlot
  {
    public string Title { get; set; }
    public string YTitle { get; set; }
    public PgnGame Game { get; set; }
    public double[] MoveList { get; set; }
    public double[] EvalsWhite { get; set; } = Array.Empty<double>();
    public double[] EvalsBlack { get; set; } = Array.Empty<double>();

    private string wplayer;
    private string bplayer;
    private IJSObjectReference chessModule;
    private DotNetObjectReference<EvalPgnPlot> objRef;
    //private Action<string, PgnGame> evalPgnPlotAction;
    ElementReference chartElement;

    public EvalPgnPlot(IJSObjectReference chessMod, ElementReference chart, PgnGame game)
    {
      chessModule = chessMod;
      chartElement = chart;
      Title = $"Eval plot";
      objRef = DotNetObjectReference.Create(this);
      Game = game;

    }

    //[JSInvokable]
    //public async Task UpdatePlotInfo(string plotPressedInfo)
    //{
    //  evalPgnPlotAction(plotPressedInfo, Game);
    //  await Task.CompletedTask;
    //}

    public async Task UpdateChart(int moveNr)
    {
      if (EvalsBlack.Length == 0)
      {
        await Task.CompletedTask;
        return;
      }
      var maxValue = Math.Max(EvalsBlack.Max(), EvalsWhite.Max());
      if (chessModule is not null && MoveList.Length != 0)
      {
        var data = new
        {
          x = moveNr,
          y = maxValue
        };
        await chessModule.InvokeVoidAsync("changeColorInEvalPlot", chartElement, data);
      }
      await Task.CompletedTask;
    }

    private (double[], double[]) GetEvalsFromPGN(PgnGame game)
    {
      var moveStats = Parser.PGNExtractor.extractEngineStats(game);
      //var evals = moveStats.Moves.Select((e, idx) => (e.wv, idx + 1)).ToList();
      //split evals into white and black
      var whitePlayerName = game.GameMetaData.White;
      var blackPlayerName = game.GameMetaData.Black;
      wplayer = whitePlayerName;
      bplayer = blackPlayerName;
      var whiteEvals = moveStats.Moves.Where(e => e.Player == whitePlayerName).Select(e => (e.wv)).ToArray();
      var blackEvals = moveStats.Moves.Where(e => e.Player == blackPlayerName).Select(e => (e.wv)).ToArray();

      EvalsWhite = whiteEvals.ToArray();
      EvalsBlack = blackEvals.ToArray();
      return (whiteEvals, blackEvals);
    }

    public async Task SetChartData()
    {
      var xaxis = new
      {
        title = "Move #",
        showgrid = false,
        zeroline = false,
        dtick = 5
      };

      var yaxis = new
      {
        //title = "Evaluation",
        showline = true,
        showgrid = false,
        //dtick0 = 0.0,
        //range = new double[] { -3.0, 3.0 }
      };

      var layout = new
      {
        //title = $"Eval plot game: {Game.GameID}",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = true,
        showTitle = false,
        xaxis = xaxis,
        yaxis = yaxis,
        legend = new
        {
          title = new
          {
            text = "",
            //text = $"Game: {Game.GameID}",
          }
        },
        margin = new
        {
          l = 45,
          r = 5,
          b = 35,
          t = 5,
          //pad = 4
        },
      };

      (var wl, var bl) = GetEvalsFromPGN(Game);

      var trace1 = new
      {
        x = MoveList,
        y = wl,
        type = "bar",
        name = wplayer,
        marker = new
        {
          //color = "	#FFFFFF"
          color = "#00adad"
        }
      };

      var trace2 = new
      {
        x = MoveList,
        y = bl,
        type = "bar",
        name = bplayer,
        marker = new
        {
          color = "#000000"
        }
      };

      var arr = new[] { trace1, trace2 };

      if (chessModule is not null)
        await chessModule.InvokeVoidAsync("setPgnEvalPlot", objRef, chartElement, layout, arr);
    }
  }
}
