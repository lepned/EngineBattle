using System.Text.Json.Serialization;
//using static Ceres.WebGUI.Pages.Config;

namespace WebGUI.Services
{
  public class ChessConfigurationService
  {
    //public ChessConfigurationService()
    //{
    //  var Settings = CeresUserSettingsManager.Settings;
    //  EngineFolder = Settings.DirExternalEngines;
    //  NeuralNetFolder = Settings.DirLC0Networks;
    //  NeuralNet = Settings.DefaultNetworkSpecString;
    //  RefNeuralNet = Settings.DefaultNetworkSpecString;
    //  TableBaseFolder = Settings.SyzygyPath;
    //  UrlNetworks = Settings.URLLC0Networks;
    //  EpdFolder = Settings.DirEPD;
    //  EpdFileName = "ERET.epd";
    //  CeresGPUstring = "GPU:0";
    //  NumberOfThreadsAB = 4;
    //  TableBaseSize = 1024;
    //  NumberOfGamePairs = 50;
    //  OpeningName = "sts.pgn";
    //  ExeLC0 = Settings.DirLC0Binaries;
    //  ExeSF = Path.Combine(Settings.DirExternalEngines, "Stockfish15.exe");
    //  ExeExternalCeres = @"C:\Users\lepne\source\repos\CeresCleanClone\artifacts\release\net5.0\ceres.exe";
    //  ExeEngine1 = String.Empty; //@"C:\dev\ceres\releases\v0.93\ceres.exe";
    //  ExeEngine2 = String.Empty;
    //  DropItems = MyItems;
    //  PGNFolderPlayers = @"C:\Utvikling\Chessplayers\PgnMentor";
    //  PGNFolder = Settings.DirPGN;

    //}

    public string EngineFolder { get; set; }
    public string PGNFolder { get; set; }
    public string NeuralNetFolder { get; set; }
    public string TableBaseFolder { get; set; }
    public string PGNFolderPlayers { get; set; }
    public string EpdFolder { get; set; }
    public string EpdFileName { get; set; }
    public string OpeningName { get; set; }
    public string CeresGPUstring { get; set; }
    public string NeuralNet { get; set; }
    public string RefNeuralNet { get; set; }
    public string ExeExternalCeres { get; set; }
    public string ExeLC0 { get; set; }
    public string ExeSF { get; set; }
    public string ExeEngine1 { get; set; }
    public string ExeEngine2 { get; set; }
    public int NumberOfGamePairs { get; set; }
    public string UrlNetworks { get; set; }
    public int NumberOfThreadsAB { get; set; }
    public int TableBaseSize { get; set; }
    
    [JsonIgnore]
    public List<DropItem> DropItems { get; set; }
    
    static List<DropItem> MyItems => new List<DropItem>()
    {
        new DropItem() { Name = "Ceres in process", Identifier = "In tournament" },
        new DropItem() { Name = "Ceres in process Ref net", Identifier = "In tournament" },
        new DropItem() { Name = "Stockfish", Identifier = "Engines" },
        new DropItem() { Name = "LC0 in process", Identifier = "Engines" },
        new DropItem() { Name = "LC0 in process Ref net", Identifier = "Engines" },
        new DropItem() { Name = "Ceres UCI", Identifier = "Engines" },
        new DropItem() { Name = "Ceres UCI Ref net", Identifier = "Engines" },
    };

    public void SaveSettings()
    {
      var json = System.Text.Json.JsonSerializer.Serialize(this);
      var path = Directory.GetCurrentDirectory();
      var fullpath = Path.Combine(path, "Data", "tournamentSettings.json");
      File.WriteAllText(fullpath, json);
    }

    public void LoadSettings(ref ChessConfigurationService config)
    {
      var path = Path.Combine(Directory.GetCurrentDirectory(), "Data", "tournamentSettings.json");
      var json = File.ReadAllText(path);
      var options = new System.Text.Json.JsonSerializerOptions { AllowTrailingCommas = true };
      var settings = System.Text.Json.JsonSerializer.Deserialize<ChessConfigurationService>(json, options);
      config = settings;
    }
    //public void LoadSettings()
    //{
    //  var path = Path.Combine(Directory.GetCurrentDirectory(), "Data", "tournamentSettings.json");
    //  var json = File.ReadAllText(path);
    //  var data = System.Text.Json.JsonSerializer.Deserialize<ChessConfigurationService>(json);
    //  var settings = CeresUserSettingsManager.Settings;
    //  EngineFolder = settings.DirExternalEngines;
    //  EpdFolder = data.EpdFolder;
    //  EpdFileName = data.EpdFileName;
    //  PGNFolderPlayers = data.PGNFolderPlayers;
    //  NeuralNetFolder = data.NeuralNetFolder;
    //  NeuralNet = data.NeuralNet;
    //  RefNeuralNet = data.RefNeuralNet;
    //  TableBaseFolder = data.TableBaseFolder;
    //  UrlNetworks = data.UrlNetworks;
    //  CeresGPUstring = data.CeresGPUstring;
    //  NumberOfThreadsAB = data.NumberOfThreadsAB;
    //  TableBaseSize = data.TableBaseSize;
    //  NumberOfGamePairs = data.TableBaseSize;
    //  OpeningName = data.OpeningName;
    //  ExeLC0 = settings.DirLC0Binaries;
    //  ExeSF = data.ExeSF;
    //  ExeExternalCeres = data.ExeExternalCeres;
    //  ExeEngine1 = data.ExeEngine1;
    //  ExeEngine2 = data.ExeEngine2;
    //  DropItems = data.DropItems;
    //}

    public override string ToString()
    {
      var options = new System.Text.Json.JsonSerializerOptions {WriteIndented = true, AllowTrailingCommas = true };
      return "";
      //return System.Text.Json.JsonSerializer.Serialize(CeresUserSettingsManager.Settings, options);
      //return System.Text.Json.JsonSerializer.Serialize(this,options);
    }
  }
}
