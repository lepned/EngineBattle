using ChessLibrary;
using Microsoft.AspNetCore.Components;
using static ChessLibrary.TypesDef;
using static ChessLibrary.TypesDef.CoreTypes;

namespace WebGUI.Services
{
  public class EngineConfigEventArgs
  {
    public EngineConfig EngineConfig { get; set; }
    public string OldName { get; set; }
  }

  public class InfoBannerInfo
  {
    public string Hardware { get; set; }
    public string TCText { get; set; }
    public string Game { get; set; }
    public string Round { get; set; }
    public string Move50 { get; set; }
    public string Draw { get; set; }
    public int R3 { get; set; }
    public int TB { get; set; }
    public int Dev { get; set; }
    public string ResultTxt { get; set; }
    public string TimeLeftTxt { get; set; }
    public string TournamentEndsTxt { get; set; }
    public bool UseTBAdj { get; set; }
    public bool UseDevCounter { get; set; }
    public int DevCounter { get; set; }
    public string HeadToHead { get; set; }

    public InfoBannerInfo()
    {

    }
    public InfoBannerInfo(TypesDef.Tournament.Tournament tourny)
    {
      Hardware = tourny.Hardware();
      TCText = tourny.TimeControlText();
      Game = tourny.CurrentGameNr == 0 ? "" : $"{tourny.CurrentGameNr}/{tourny.TotalGames}";
      Round = GetRound(tourny);
      UseDevCounter = tourny.PreventMoveDeviation && tourny.EngineSetup.EngineDefList.Length > 2;
      UseTBAdj = tourny.Adjudication.TBAdj.UseTBAdjudication;
      DevCounter = tourny.DeviationCounter;
      ResultTxt = "  * ";
      TimeLeftTxt = "";
      TournamentEndsTxt = "";
    }

    public static string GetFormattedTournamentEnds(DateTime dateTime)
    {
      // Formats the DateTime as "Year-Month-Day Hour:00"
      var formatted = dateTime.ToString("yyyy-MM-dd HH:mm");
      return formatted;
    }

    public static string Duration(TimeSpan dur) => string.Format("{0:00}:{1:00}:{2:00}", dur.Days, dur.Hours, dur.Minutes);

    private static string GetRound(TypesDef.Tournament.Tournament tourny)
    {
      if (tourny == null || tourny.TotalGames == 0 || tourny.CurrentGameNr == 0)
        return "";
      var tot = tourny.TotalGames;
      var cur = tourny.CurrentGameNr;
      var r = tourny.Rounds;
      var ratio = Math.Max(1, tot / r);
      //var rFract = Math.Max(cur * ratio, 1);
      var subgame = cur % ratio;
      var roundNr = cur <= ratio ? 1 : subgame == 0 ? cur / ratio : cur / ratio + 1;
      var subNr = subgame == 0 ? ratio : subgame;
      return $"{roundNr}.{subNr}";
    }

    public static string GetTournamentEnd(TimeSpan duration)
    {
      DateTime now = DateTime.UtcNow;
      TimeZoneInfo timeZone = TimeZoneInfo.Local;
      var end = TimeZoneInfo.ConvertTimeFromUtc(now, timeZone).AddTicks(duration.Ticks);
      var formattedEnd = GetFormattedTournamentEnds(end);
      return formattedEnd;
    }
  }


}
